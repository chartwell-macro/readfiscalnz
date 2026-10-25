.nz_series_contract_version <- "nz-fiscal-series-1"

.nz_series_identity_fields <- function(family) {
  switch(
    family,
    hfi = c(
      "section", "category", "series", "measure", "unit", "regime_id",
      "fiscal_year_basis", "accounting_basis", "gst_treatment",
      "consolidation_scope"
    ),
    tax_outturn = c(
      "measure", "accounting_basis", "consolidation_scope", "section",
      "series", "unit", "writeoff_regime"
    ),
    monthly_financials = c(
      "table", "statement_sheet", "period_kind", "observation",
      "actual_forecast", "consolidation_scope", "accounting_basis",
      "section", "series", "note", "unit"
    ),
    efu = c(
      "release_type", "forecast_vintage", "forecast_horizon", "dataset",
      "sheet", "fiscal_year_basis", "actual_forecast",
      "accounting_basis", "consolidation_scope", "section", "series",
      "unit", "unit_multiplier", "unit_context"
    ),
    character()
  )
}

.nz_series_date_field <- function(family) {
  if (identical(family, "tax_outturn")) "date" else "period_end"
}

.nz_series_canonical_value <- function(value) {
  if (!length(value) || is.na(value[[1L]])) {
    return("<NA>")
  }
  if (inherits(value, "Date")) {
    return(as.character(value[[1L]]))
  }
  if (is.numeric(value)) {
    return(format(
      value[[1L]],
      scientific = FALSE,
      trim = TRUE,
      digits = 17,
      decimal.mark = ".",
      big.mark = ""
    ))
  }
  enc2utf8(as.character(value[[1L]]))
}

.nz_series_payload <- function(row, fields) {
  stats::setNames(
    lapply(fields, function(field) row[[field]][1L]),
    fields
  )
}

.nz_series_hash <- function(family, payload) {
  canonical <- paste(
    .nz_series_contract_version,
    family,
    paste(
      names(payload),
      vapply(payload, .nz_series_canonical_value, character(1L)),
      sep = "=",
      collapse = "\n"
    ),
    sep = "\n"
  )
  digest::digest(canonical, algo = "sha256", serialize = FALSE)
}

.nz_series_value_matches <- function(actual, expected) {
  if (!length(expected) || is.na(expected[[1L]])) {
    return(is.na(actual))
  }
  if (is.numeric(expected)) {
    return(
      !is.na(actual) &
        as.numeric(actual) == as.numeric(expected[[1L]])
    )
  }
  !is.na(actual) & as.character(actual) == as.character(expected[[1L]])
}

.nz_series_filter <- function(data, payload) {
  keep <- rep(TRUE, nrow(data))
  for (field in names(payload)) {
    keep <- keep & .nz_series_value_matches(data[[field]], payload[[field]])
  }
  data[keep %in% TRUE, , drop = FALSE]
}

#' Catalogue canonical chart-series identities in parsed fiscal data
#'
#' Builds a deterministic catalogue of the structural series in an object
#' returned by one of the `read_nz_*()` readers. A series identity represents
#' the dimensions that must remain constant for observations to be comparable;
#' point-level annotations remain on the observations and do not create a new
#' identity. Use [nz_fiscal_series_history()] with a `series_id` from the
#' result to replay one identity.
#'
#' @details
#' The identity dimensions are family-specific and are included in the exact
#' order shown below. The names and order are part of the package's
#' `nz-fiscal-series-1` contract and therefore affect the SHA-256 `series_id`.
#' The `identity_payload` list column contains the corresponding scalar values
#' from the first observation of each series, in the same order.
#' The digest input is a non-serialised UTF-8 text string containing the
#' contract version, family, and ordered `name=value` lines separated by
#' newlines. Missing values are canonicalised as `<NA>`, `Date` values as
#' ISO-8601 text, numeric values as non-scientific decimal text, and character
#' values as UTF-8. The resulting `digest::digest(..., algo = "sha256",
#' serialize = FALSE)` is the lower-case `series_id`.
#'
#' * Historical Fiscal Indicators (`hfi`): `section`, `category`, `series`,
#'   `measure`, `unit`, `regime_id`, `fiscal_year_basis`, `accounting_basis`,
#'   `gst_treatment`, `consolidation_scope`.
#' * Tax outturn (`tax_outturn`): `measure`, `accounting_basis`,
#'   `consolidation_scope`, `section`, `series`, `unit`, `writeoff_regime`.
#' * Monthly financial statements (`monthly_financials`): `table`,
#'   `statement_sheet`, `period_kind`, `observation`, `actual_forecast`,
#'   `consolidation_scope`, `accounting_basis`, `section`, `series`, `note`,
#'   `unit`.
#' * Economic and Fiscal Updates (`efu`): `release_type`, `forecast_vintage`,
#'   `forecast_horizon`, `dataset`, `sheet`, `fiscal_year_basis`,
#'   `actual_forecast`, `accounting_basis`, `consolidation_scope`, `section`,
#'   `series`, `unit`, `unit_multiplier`, `unit_context`.
#'
#' HFI `comparability_note` and `restatement_status`, and EFU
#' `source_status_label`, are observation-level metadata. They are retained by
#' [nz_fiscal_series_history()] but deliberately do not fragment a series.
#' Conversely, the monthly `observation` role is part of the identity because
#' each monthly publication is an immutable release snapshot; a supported
#' monthly release can therefore produce one-observation catalogue entries.
#' The catalogue does not stitch observations across releases. To compare
#' releases, catalogue each parsed release separately and retain its provenance.
#'
#' The output rows retain the first-occurrence order of the input identities.
#' Dates are converted to `Date` before coverage and uniqueness checks. Each
#' identity must have at least one non-missing finite numeric value, and a
#' canonical identity cannot contain missing or duplicate dates. These checks
#' fail closed because silently merging ambiguous fiscal observations would
#' change their meaning.
#' Errors are also raised when `x` has no recognised family provenance, lacks a
#' required identity/date/value field, or contains no observations. The
#' function does not infer missing dimensions or repair a changed source
#' schema.
#'
#' This function only inspects `x`: it performs no network access, does not
#' modify `x`, and does not write to a cache. A reader may already have written
#' its raw or parsed cache before `x` is supplied here.
#'
#' @param x An object returned by a `read_nz_*()` function, normally with class
#'   `c("nz_fiscal_data", "data.frame")` and an
#'   `nz_fiscal_provenance` attribute. The object must contain the family
#'   identity fields, its family date field (`date` for tax outturn or
#'   `period_end` otherwise), and numeric `value` data.
#' @return A data frame with class
#'   `c("nz_fiscal_series_catalogue", "data.frame")`, one row per canonical
#'   series. Columns are returned in this exact order: `series_id`, `family`,
#'   `series`, `unit`, `start_date`, `end_date`, `observations`,
#'   `finite_observations`, `contract_version`, `identity_fields`, and
#'   `identity_payload`. The first nine columns are atomic columns:
#'   `series_id`, `family`, `series`, `unit`, and `contract_version` are
#'   character; `start_date` and `end_date` are `Date`; and `observations` and
#'   `finite_observations` are integer. `series_id` is a lower-case 64-character
#'   SHA-256 hexadecimal identity, `contract_version` is
#'   `"nz-fiscal-series-1"`, and the date/count columns describe all source
#'   rows matching that identity. `identity_fields` is a character-vector list
#'   column (the family-specific names above); `identity_payload` is a named
#'   list-column of scalar source values with those names and order. Every
#'   identity has `observations >= 1`, `finite_observations >= 1`, and a unique
#'   `series_id`.
#' @seealso [nz_fiscal_series_history()], [nz_fiscal_provenance()],
#'   [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()], [read_nz_efu()],
#'   [list_nz_fiscal_releases()]
#' @family series-tools
#' @examples
#' hfi_file <- system.file("extdata", "hfi.csv", package = "readfiscalnz")
#' hfi <- read_nz_hfi(
#'   file = hfi_file,
#'   cache_dir = tempfile("readfiscalnz-series-cache-")
#' )
#' catalogue <- nz_fiscal_series_catalogue(hfi)
#' stopifnot(
#'   identical(class(catalogue), c("nz_fiscal_series_catalogue", "data.frame")),
#'   identical(names(catalogue), c(
#'     "series_id", "family", "series", "unit", "start_date", "end_date",
#'     "observations", "finite_observations", "contract_version",
#'     "identity_fields", "identity_payload"
#'   )),
#'   all(nchar(catalogue$series_id) == 64L),
#'   all(catalogue$contract_version == "nz-fiscal-series-1")
#' )
#' history <- nz_fiscal_series_history(hfi, catalogue$series_id[[1L]])
#' stopifnot(
#'   inherits(history, "nz_fiscal_data"),
#'   inherits(history$period_end, "Date"),
#'   is.numeric(history$value),
#'   identical(history$period_end, sort(history$period_end))
#' )
#' @export
nz_fiscal_series_catalogue <- function(x) {
  provenance <- nz_fiscal_provenance(x)
  family <- as.character(provenance$family)
  fields <- .nz_series_identity_fields(family)
  date_field <- .nz_series_date_field(family)
  required <- c(fields, date_field, "series", "unit", "value")
  missing <- setdiff(required, names(x))
  if (!length(fields) || length(missing)) {
    .abort(
      "Parsed %s data cannot satisfy the series-history contract; missing: %s.",
      family,
      paste(missing, collapse = ", ")
    )
  }
  if (!nrow(x)) {
    .abort("Parsed %s data contains no observations.", family)
  }

  keys <- do.call(
    paste,
    c(
      lapply(fields, function(field) {
        vapply(
          seq_len(nrow(x)),
          function(index) .nz_series_canonical_value(x[[field]][index]),
          character(1L)
        )
      }),
      sep = "\034"
    )
  )
  dates <- as.Date(x[[date_field]])
  if (anyNA(dates) || anyDuplicated(data.frame(key = keys, date = dates))) {
    .abort(
      paste(
        "Parsed %s data has missing or duplicate dates for a canonical",
        "series; the source schema requires review."
      ),
      family
    )
  }
  first <- !duplicated(keys)
  rows <- x[first, , drop = FALSE]
  payloads <- lapply(
    seq_len(nrow(rows)),
    function(index) .nz_series_payload(rows[index, , drop = FALSE], fields)
  )
  hashes <- vapply(
    payloads,
    function(payload) .nz_series_hash(family, payload),
    character(1L)
  )
  grouped_dates <- split(dates, keys)
  grouped_values <- split(as.numeric(x$value), keys)
  first_keys <- keys[first]
  out <- data.frame(
    series_id = unname(hashes),
    family = rep(family, length(hashes)),
    series = as.character(rows$series),
    unit = as.character(rows$unit),
    start_date = as.Date(unname(vapply(
      first_keys,
      function(key) as.numeric(min(grouped_dates[[key]])),
      numeric(1L)
    )), origin = "1970-01-01"),
    end_date = as.Date(unname(vapply(
      first_keys,
      function(key) as.numeric(max(grouped_dates[[key]])),
      numeric(1L)
    )), origin = "1970-01-01"),
    observations = as.integer(unname(vapply(
      first_keys,
      function(key) length(grouped_dates[[key]]),
      integer(1L)
    ))),
    finite_observations = as.integer(unname(vapply(
      first_keys,
      function(key) sum(is.finite(grouped_values[[key]])),
      integer(1L)
    ))),
    contract_version = rep(
      .nz_series_contract_version,
      length(hashes)
    ),
    stringsAsFactors = FALSE
  )
  if (any(out$finite_observations < 1L) || anyDuplicated(out$series_id)) {
    .abort(
      "Parsed %s data has empty or non-unique canonical series.",
      family
    )
  }
  out$identity_fields <- rep(list(fields), nrow(out))
  out$identity_payload <- payloads
  class(out) <- c("nz_fiscal_series_catalogue", class(out))
  out
}

#' Replay one canonical fiscal series
#'
#' Selects one structural identity from [nz_fiscal_series_catalogue()] and
#' returns every matching observation in date order. This is the convenient
#' hand-off from a catalogue row to a chart or other time-series consumer.
#'
#' @details
#' `series_id` must be the lower-case, exact 64-character SHA-256 hexadecimal
#' identity produced by [nz_fiscal_series_catalogue()] under the
#' `nz-fiscal-series-1` contract. The digest covers the family and the
#' family-specific identity payload, including its field names and order; it is
#' not a hash of the observations or of the raw workbook. Recomputing a series
#' ID from a different release, accounting regime, unit, forecast vintage, or
#' consolidation scope will therefore produce a different identity when those
#' dimensions are part of the family contract.
#'
#' The returned rows retain the reader's complete source schema and point-level
#' metadata. For `tax_outturn`, the canonical chart date is `date`; for HFI,
#' monthly financial statements, and EFU, it is `period_end`. The selected rows
#' are sorted ascending by that field, while all other columns and their source
#' values are preserved. The result retains the original `nz_fiscal_provenance`
#' attribute and adds an `nz_fiscal_series_identity` attribute: a named list
#' containing `series_id`, `contract_version`, `identity_fields`, and
#' `identity_payload`.
#'
#' This function only filters and orders already parsed data. It performs no
#' network access, does not modify `x`, and does not write to a cache. It does
#' not join or interpolate releases, repair missing dates, deduplicate source
#' observations, or convert values between units. Invalid IDs, absent IDs,
#' missing dates, and duplicate dates fail with an error rather than returning
#' an ambiguous history.
#'
#' @param x An object returned by a `read_nz_*()` function, normally with class
#'   `c("nz_fiscal_data", "data.frame")` and an
#'   `nz_fiscal_provenance` attribute. It must contain the fields needed by its
#'   family identity and date contract.
#' @param series_id A single lower-case 64-character hexadecimal SHA-256
#'   identity returned in the `series_id` column of
#'   [nz_fiscal_series_catalogue()]. A malformed ID or an ID not present in
#'   `x` is an error.
#' @return A data frame with class `c("nz_fiscal_data", "data.frame")` and the
#'   same source columns as `x`, including its numeric `value` column and its
#'   family date column (`date` for tax outturn or `period_end` otherwise).
#'   Rows are sorted in ascending date order and retain the source-column order.
#'   The reader-defined schemas are, respectively: HFI, `year`,
#'   `period_end`, `section`, `category`, `series`, `measure`, `value`, `unit`,
#'   `regime_id`, `fiscal_year_basis`, `accounting_basis`, `gst_treatment`,
#'   `consolidation_scope`, `comparability_note`, `restatement_status`; tax
#'   outturn, `date`, `fiscal_year`, `measure`, `accounting_basis`,
#'   `consolidation_scope`, `section`, `series`, `value`, `unit`,
#'   `writeoff_regime`; monthly financials, `table`, `statement_sheet`,
#'   `period_end`, `fiscal_year`, `period_kind`, `observation`,
#'   `actual_forecast`, `consolidation_scope`, `accounting_basis`, `section`,
#'   `series`, `note`, `value`, `unit`; and EFU, `release_type`,
#'   `forecast_vintage`, `forecast_horizon`, `dataset`, `sheet`, `year`,
#'   `period_end`, `fiscal_year_basis`, `actual_forecast`,
#'   `source_status_label`, `accounting_basis`, `consolidation_scope`, `section`,
#'   `series`, `value`, `unit`, `unit_multiplier`, `unit_context`. In these
#'   schemas, `year` is integer, `date`/`period_end` is `Date`, and `value` is
#'   numeric; EFU `unit_multiplier` is also numeric, while the remaining fields
#'   are character. Rows are not interpolated, deduplicated, or joined across
#'   releases. The
#'   `nz_fiscal_provenance` attribute is retained, and
#'   `nz_fiscal_series_identity` is a named list with `series_id`,
#'   `contract_version`, `identity_fields`, and `identity_payload`.
#' @seealso [nz_fiscal_series_catalogue()], [nz_fiscal_provenance()],
#'   [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()], [read_nz_efu()]
#' @family series-tools
#' @examples
#' hfi_file <- system.file("extdata", "hfi.csv", package = "readfiscalnz")
#' hfi <- read_nz_hfi(
#'   file = hfi_file,
#'   cache_dir = tempfile("readfiscalnz-series-cache-")
#' )
#' catalogue <- nz_fiscal_series_catalogue(hfi)
#' history <- nz_fiscal_series_history(hfi, catalogue$series_id[[1L]])
#' identity <- attr(history, "nz_fiscal_series_identity")
#' stopifnot(
#'   identical(identity$series_id, catalogue$series_id[[1L]]),
#'   identical(identity$contract_version, "nz-fiscal-series-1"),
#'   identical(names(identity), c(
#'     "series_id", "contract_version", "identity_fields", "identity_payload"
#'   )),
#'   identical(history$period_end, sort(history$period_end))
#' )
#' @export
nz_fiscal_series_history <- function(x, series_id) {
  if (!is.character(series_id) || length(series_id) != 1L ||
      is.na(series_id) || !grepl("^[a-f0-9]{64}$", series_id)) {
    .abort("series_id must be one exact SHA-256 series identity.")
  }
  catalogue <- nz_fiscal_series_catalogue(x)
  matched <- which(catalogue$series_id == series_id)
  if (length(matched) != 1L) {
    .abort("series_id is absent from the parsed fiscal release.")
  }
  payload <- catalogue$identity_payload[[matched]]
  selected <- .nz_series_filter(x, payload)
  date_field <- .nz_series_date_field(catalogue$family[[matched]])
  dates <- as.Date(selected[[date_field]])
  if (!nrow(selected) || anyNA(dates) || anyDuplicated(dates)) {
    .abort("The selected fiscal series has invalid or duplicate chart dates.")
  }
  selected <- selected[order(dates), , drop = FALSE]
  result <- .new_nz_fiscal_data(
    as.data.frame(selected, stringsAsFactors = FALSE),
    nz_fiscal_provenance(x)
  )
  attr(result, "nz_fiscal_series_identity") <- list(
    series_id = series_id,
    contract_version = catalogue$contract_version[[matched]],
    identity_fields = catalogue$identity_fields[[matched]],
    identity_payload = payload
  )
  result
}
