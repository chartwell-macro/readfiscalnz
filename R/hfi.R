.hfi_regime <- function(year, section) {
  old_gaap_caveat <- year >= 1994L & year <= 1996L &
    section %in% c("Spending", "Revenue, Surplus measures")
  cut <- ifelse(
    year <= 1986, "cash_march_gst_exclusive",
    ifelse(
      year <= 1989, "cash_march_gst_inclusive",
      ifelse(
        year <= 1993, "cash_june_gst_inclusive",
        ifelse(
          old_gaap_caveat, "gaap_nzifrs_old_gaap_revenue_expense",
          ifelse(year <= 2004, "gaap_nzifrs", "gaap_pbe")
        )
      )
    )
  )
  data.frame(
    regime_id = cut,
    fiscal_year_basis = ifelse(year <= 1989, "March year", "June year"),
    accounting_basis = ifelse(
      year <= 1993,
      "cash",
      ifelse(year <= 2004, "accrual GAAP (NZ-IFRS)", "accrual GAAP (PBE Standards)")
    ),
    gst_treatment = ifelse(year >= 1987 & year <= 1993, "inclusive", "exclusive"),
    consolidation_scope = ifelse(
      year <= 1993,
      "cash-basis central government; Reserve Bank, Crown entities, and SOEs excluded",
      "series-specific core Crown or total Crown; Reserve Bank consolidated in core Crown"
    ),
    comparability_note = ifelse(
      old_gaap_caveat,
      "1994-1996 revenue and expense series are old-GAAP and were not backdated",
      "none recorded for this series/year"
    ),
    restatement_status = ifelse(
      year == 2019L,
      "restated_at_2020_for_new_pbe_standards",
      ifelse(
        year == 2020L,
        "first_reporting_year_under_new_pbe_standards",
        "as_published"
      )
    ),
    stringsAsFactors = FALSE
  )
}

.hfi_year <- function(x) {
  text <- .clean_text(x)
  valid <- grepl("^(19|20)[0-9]{2}", text, perl = TRUE)
  out <- rep(NA_integer_, length(text))
  out[valid] <- suppressWarnings(as.integer(substr(text[valid], 1L, 4L)))
  out[out < 1900L | out > 2100L] <- NA_integer_
  out
}

.hfi_block_contract <- function(label) {
  label <- tolower(.clean_text(label))
  if (grepl("^\\$[[:space:]]*millions?$", label)) {
    return(list(
      measure = "level",
      unit = "NZD million",
      multiplier = 1
    ))
  }
  if (grepl("^%[[:space:]]*gdp$", label)) {
    return(list(
      measure = "share_of_gdp",
      unit = "percent of GDP",
      multiplier = 100
    ))
  }
  NULL
}

.parse_hfi_sheet <- function(path, sheet) {
  x <- .read_sheet_text(path, sheet)
  if (ncol(x) < 3L || nrow(x) < 5L) {
    .abort("HFI sheet '%s' is smaller than the supported schema.", sheet)
  }
  block_rows <- which(vapply(
    x[[1L]],
    function(label) !is.null(.hfi_block_contract(label)),
    logical(1L)
  ))
  if (!length(block_rows)) {
    .abort(
      "HFI sheet '%s' has no supported $ millions or %% GDP block.",
      sheet
    )
  }

  output <- list()
  cursor <- 0L
  for (block_index in seq_along(block_rows)) {
    group_row <- block_rows[[block_index]]
    header_row <- group_row + 1L
    block_end <- if (block_index < length(block_rows)) {
      block_rows[[block_index + 1L]] - 1L
    } else {
      nrow(x)
    }
    years <- .hfi_year(x[[2L]])
    data_rows <- which(
      seq_len(nrow(x)) > header_row &
        seq_len(nrow(x)) <= block_end &
        !is.na(years)
    )
    if (length(data_rows) < 2L) {
      .abort(
        "HFI sheet '%s' block '%s' has no supported year rows.",
        sheet,
        .clean_text(x[[1L]][group_row])
      )
    }
    contract <- .hfi_block_contract(x[[1L]][group_row])
    category <- ""
    for (column in seq.int(3L, ncol(x))) {
      group_value <- .clean_text(x[[column]][group_row])
      if (nzchar(group_value)) {
        category <- group_value
      }
      series <- .clean_text(x[[column]][header_row])
      series_category <- category
      if (!nzchar(series) && nzchar(series_category)) {
        series <- series_category
        series_category <- ""
      }
      if (!nzchar(series)) {
        next
      }
      values <- .as_number(x[[column]][data_rows])
      keep <- !is.na(values)
      if (!any(keep)) {
        next
      }
      selected_years <- years[data_rows][keep]
      regime <- .hfi_regime(selected_years, sheet)
      cursor <- cursor + 1L
      output[[cursor]] <- data.frame(
        year = selected_years,
        period_end = as.Date(sprintf(
          "%04d-%s",
          selected_years,
          ifelse(selected_years <= 1989L, "03-31", "06-30")
        )),
        section = sheet,
        category = if (nzchar(series_category)) {
          series_category
        } else {
          NA_character_
        },
        series = series,
        measure = contract$measure,
        value = values[keep] * contract$multiplier,
        unit = contract$unit,
        regime,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(output)) {
    .abort("HFI sheet '%s' contained no numeric series under its headers.", sheet)
  }
  do.call(rbind, output)
}

.validate_hfi <- function(x) {
  required <- c(
    "year", "period_end", "section", "category", "series", "measure",
    "value", "unit", "regime_id", "fiscal_year_basis",
    "accounting_basis", "gst_treatment", "consolidation_scope",
    "comparability_note", "restatement_status"
  )
  .require_exact_columns(x, required, "HFI")
  x$year <- as.integer(x$year)
  x$period_end <- as.Date(x$period_end)
  x$value <- as.numeric(x$value)
  if (anyNA(x$year) || anyNA(x$period_end) || anyNA(x$value)) {
    .abort("HFI schema contains missing year, period_end, or value fields.")
  }
  supported_sections <- c(
    "Spending", "Revenue, Surplus measures", "Debt, Net Worth",
    "NZS Fund series", "Nominal GDP"
  )
  if (any(!x$section %in% supported_sections)) {
    .abort("HFI section drifted outside the supported workbook routes.")
  }
  measure_unit <- c(
    level = "NZD million",
    share_of_gdp = "percent of GDP"
  )
  expected_unit <- unname(measure_unit[x$measure])
  if (anyNA(expected_unit) || any(x$unit != expected_unit)) {
    .abort("HFI measure/unit pairing is unsupported.")
  }
  expected_period <- as.Date(sprintf(
    "%04d-%s",
    x$year,
    ifelse(x$year <= 1989L, "03-31", "06-30")
  ))
  if (any(x$period_end != expected_period)) {
    .abort("HFI period_end does not match its documented fiscal-year regime.")
  }
  expected_regime <- .hfi_regime(x$year, x$section)
  semantic_fields <- c(
    "regime_id", "fiscal_year_basis", "accounting_basis",
    "gst_treatment", "consolidation_scope", "comparability_note",
    "restatement_status"
  )
  drift <- vapply(semantic_fields, function(field) {
    !identical(as.character(x[[field]]), as.character(expected_regime[[field]]))
  }, logical(1L))
  if (any(drift)) {
    .abort(
      "HFI semantic drift in field(s): %s.",
      paste(semantic_fields[drift], collapse = ", ")
    )
  }
  x
}

#' Read Historical Fiscal Indicators (HFI)
#'
#' Reads and normalises a New Zealand Treasury Historical Fiscal Indicators
#' (HFI) release. HFI is a historical, long-run time series rather than a
#' single consistently defined accounting series: the result therefore keeps
#' the Treasury's fiscal-year, accounting, GST, consolidation, comparability,
#' and restatement metadata on every observation. The parser recognises the
#' separate `$ millions` and `% GDP` blocks in the official workbook, retains
#' footnote-marked year labels, and makes the historical March/June and
#' cash/accrual regime changes explicit.
#'
#' By default, the current byte-pinned HFI release in the package catalogue is
#' selected. A local XLSX workbook or canonical CSV can be supplied for an
#' offline, reproducible replay. Local artifacts are signature-checked, copied
#' to the hash-addressed raw cache, and parsed by the same validation path as a
#' downloaded artifact. Supplying `file` never performs network I/O. Without a
#' local file, retrieval is HTTPS-only from the official Treasury host and
#' requires `accept_terms = TRUE` after reviewing [nz_fiscal_terms()].
#'
#' @details
#' The supported HFI sheets are `"Spending"`, `"Revenue, Surplus measures"`,
#' `"Debt, Net Worth"`, `"NZS Fund series"`, and `"Nominal GDP"`. `NULL` for
#' `sheets` parses all five sheets in that order; otherwise, rows are emitted
#' in the requested sheet order and retain the workbook's block, series, and
#' observation order. CSV input retains its input row order. The function does
#' not sort observations.
#'
#' `period_end` is March 31 for years through 1989 and June 30 from 1990.
#' The parser assigns `regime_id` values
#' `cash_march_gst_exclusive`, `cash_march_gst_inclusive`,
#' `cash_june_gst_inclusive`, `gaap_nzifrs_old_gaap_revenue_expense`,
#' `gaap_nzifrs`, or `gaap_pbe`. The 1994--1996 old-GAAP caveat applies to
#' revenue and expense series and is recorded in `comparability_note`; it is
#' not silently backdated. Values from `% GDP` blocks are represented as
#' `measure = "share_of_gdp"` and `unit = "percent of GDP"`; `$ millions`
#' blocks use `measure = "level"` and `unit = "NZD million"`.
#'
#' Parsed results use an in-memory parsed-result cache keyed by the raw
#' SHA-256, release, revision policy, parser version, schema version, and
#' parser options. The raw cache is bounded by package options and is stored
#' below `cache_dir`. The default cache is
#' `tools::R_user_dir("readfiscalnz", "cache")/v1`; passing a temporary
#' directory is useful for isolated offline examples and tests. Reading a
#' local file can therefore create files below `cache_dir`, but does not modify
#' the input artifact.
#'
#' Inputs that are empty, exceed `max_bytes`, have an unsupported extension or
#' file signature, omit a required workbook sheet, use an unsupported sheet
#' route, do not contain numeric observations, or fail the canonical column,
#' date, unit, or regime checks are rejected with an error. A local artifact is
#' archived before parsing, so a later parser error can leave its verified raw
#' snapshot in `cache_dir`; incomplete downloads are removed. The package does
#' not infer undocumented sheets or repair accounting definitions when the
#' Treasury layout or semantics drift.
#'
#' @param release_id A character scalar identifying an exact catalogue
#'   release, normally `"hfi-1972-2025"`. If `NULL` (the default), the
#'   release selected by `revision` is used. Use [list_nz_fiscal_releases()]
#'   to inspect supported releases and their metadata.
#' @param file An optional path to one existing local `.xlsx` or canonical
#'   `.csv` artifact. The default, `NULL`, resolves the selected official
#'   Treasury artifact over HTTPS. Local files are copied into the raw cache
#'   and validated before parsing; they are not downloaded or changed.
#' @param revision A character scalar matched against
#'   `c("as_published", "latest")`; the default is `"as_published"`.
#'   With no explicit `release_id`, `"as_published"` selects the newest current
#'   catalogue row and falls back to the newest issue date when the family has
#'   no current row; `"latest"` always selects the newest issue date. The
#'   selected policy is recorded in provenance.
#' @param sheets An optional character vector of supported workbook sheet
#'   names. The default, `NULL`, parses all supported sheets. This argument is
#'   used for XLSX input; canonical CSV input is already normalised and is
#'   read as a complete result. Unsupported or absent sheet names are errors.
#' @param cache_dir A character scalar naming the raw snapshot cache root. The
#'   default is `.nz_cache_dir()`, normally
#'   `tools::R_user_dir("readfiscalnz", "cache")/v1`. The directory and its
#'   package-managed subdirectories may be created as a side effect.
#' @param accept_terms A logical scalar controlling live retrieval. The
#'   default, `FALSE`, refuses a download; set to `TRUE` only after reviewing
#'   [nz_fiscal_terms()] and the Treasury licence metadata. This argument has
#'   no effect when `file` is supplied.
#' @param timeout_sec A positive numeric scalar timeout for live artifact
#'   transport, in seconds. The default is `30`. It is ignored when `file` is
#'   supplied, apart from input validation.
#' @param max_bytes A positive numeric scalar maximum accepted artifact size
#'   in bytes. The default is `25 * 1024^2` (26,214,400 bytes). Artifacts
#'   outside the bound are rejected before parsing.
#' @return A data frame with class `c("nz_fiscal_data", "data.frame")`, one
#'   row per numeric HFI observation. Columns are returned in this exact order:
#'   `year`, `period_end`, `section`, `category`, `series`, `measure`,
#'   `value`, `unit`, `regime_id`, `fiscal_year_basis`, `accounting_basis`,
#'   `gst_treatment`, `consolidation_scope`, `comparability_note`, and
#'   `restatement_status`. `year` is integer, `period_end` is a `Date`, and
#'   `value` is numeric; all remaining columns are character. `section` is one
#'   of the five supported sheet names. `measure` is either `"level"` or
#'   `"share_of_gdp"`, with the corresponding `unit` described in Details.
#'   `period_end` agrees with `year` and the March/June rule, and the semantic
#'   regime columns are validated against the Treasury regime rules.
#'
#'   The result has an `nz_fiscal_provenance` attribute containing a named list
#'   with release and source metadata, including `provider`, `family`,
#'   `release_id`, `release_type`, `release_status`, `document_date`,
#'   `issue_date`, `fiscal_year`, `fiscal_year_basis`, `accounting_basis`,
#'   `consolidation_scope`, `actual_forecast`, `forecast_vintage`,
#'   `forecast_horizon`, `landing_url`, `original_url`, `resolved_url`,
#'   `original_file`, `source_kind`, `source_location`,
#'   `raw_snapshot_path`, `raw_size_bytes`, `raw_sha256`, `catalogue_sha256`,
#'   `retrieved_at`, `terms_statement`, `terms_license`, `terms_attribution`,
#'   `license_url`, `terms_url`, `terms_verified_at`, `revision_policy`, and
#'   `revision_note`, plus `parser_version`, `schema_version`, and
#'   `parser_options`. For HFI these version values are `"hfi-2.0.0"` and
#'   `"hfi-long-2"`; use [nz_fiscal_provenance()] to inspect the complete
#'   list. The attribute is retained by downstream
#'   [nz_fiscal_series_catalogue()] and [nz_fiscal_series_history()] calls.
#' @seealso [list_nz_fiscal_releases()], [nz_fiscal_terms()],
#'   [nz_fiscal_provenance()], [nz_fiscal_series_catalogue()],
#'   [nz_fiscal_series_history()]
#' @family fiscal-readers
#' @examples
#' hfi_file <- system.file("extdata", "hfi.csv", package = "readfiscalnz")
#' hfi <- read_nz_hfi(
#'   file = hfi_file,
#'   cache_dir = tempfile("readfiscalnz-hfi-cache-")
#' )
#' stopifnot(
#'   inherits(hfi, "nz_fiscal_data"),
#'   inherits(hfi$period_end, "Date"),
#'   is.numeric(hfi$value),
#'   identical(names(hfi), c(
#'     "year", "period_end", "section", "category", "series", "measure",
#'     "value", "unit", "regime_id", "fiscal_year_basis", "accounting_basis",
#'     "gst_treatment", "consolidation_scope", "comparability_note",
#'     "restatement_status"
#'   ))
#' )
#' head(hfi[c("year", "period_end", "series", "measure", "value")])
#' @export
read_nz_hfi <- function(
    release_id = NULL,
    file = NULL,
    revision = c("as_published", "latest"),
    sheets = NULL,
    cache_dir = .nz_cache_dir(),
    accept_terms = FALSE,
    timeout_sec = 30,
    max_bytes = 25 * 1024^2) {
  revision <- match.arg(revision)
  artifact <- .resolve_artifact(
    "hfi", release_id, file, revision, cache_dir,
    timeout_sec, max_bytes, accept_terms
  )
  supported <- c(
    "Spending", "Revenue, Surplus measures", "Debt, Net Worth",
    "NZS Fund series", "Nominal GDP"
  )
  if (is.null(sheets)) {
    sheets <- supported
  }
  unsupported <- setdiff(sheets, supported)
  if (length(unsupported)) {
    .abort(
      "Unsupported HFI sheet(s): %s. Supported: %s",
      paste(unsupported, collapse = ", "),
      paste(supported, collapse = ", ")
    )
  }
  parser_options <- list(
    sheets = sheets,
    regime_policy = "Treasury-published historical breaks"
  )
  cache_key <- .parsed_cache_key(artifact, "hfi", parser_options)
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) {
    return(.finish_result(cached, artifact, "hfi", parser_options))
  }
  if (artifact$extension == "csv") {
    data <- .read_normalized_csv(artifact$path)
  } else {
    data <- do.call(rbind, lapply(sheets, function(sheet) {
      .parse_hfi_sheet(artifact$path, sheet)
    }))
  }
  data <- .validate_hfi(data)
  result <- .finish_result(
    data, artifact, "hfi", parser_options
  )
  .cache_set(cache_key, data)
  result
}
