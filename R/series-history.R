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
      digits = 17
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

#' Catalogue continuous chart series in parsed fiscal data
#'
#' Builds canonical series identities from structural dimensions such as
#' accounting basis, consolidation, unit, regime, and actual/forecast status.
#' Observation-level annotations remain on the underlying rows and do not
#' fragment a history. In particular, HFI restatement/comparability labels and
#' EFU source-status labels are retained as point metadata rather than treated
#' as separate series.
#'
#' Monthly financial statements are immutable release snapshots. Their
#' observation role remains part of the identity, so a single supported release
#' may legitimately produce one-point catalogue entries.
#'
#' @param x An object returned by a `read_nz_*()` function.
#' @return A data frame with one row per canonical series, exact identity hash,
#'   coverage, observation count, and the semantic payload used for replay.
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

#' Replay one continuous fiscal series
#'
#' @param x An object returned by a `read_nz_*()` function.
#' @param series_id Exact SHA-256 identity returned by
#'   [nz_fiscal_series_catalogue()].
#' @return An `nz_fiscal_data` object containing the complete, date-ordered
#'   source rows for the selected structural identity. Point-level annotations
#'   and original source provenance are retained.
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
