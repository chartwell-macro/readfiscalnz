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

#' Read Historical Fiscal Indicators
#'
#' Normalises the Treasury's historical fiscal time-series workbook while
#' explicitly labelling the March/June year, cash/accrual, GST, and accounting
#' regime breaks. The 1994-1996 old-GAAP revenue/expense caveat is retained as
#' a distinct regime. Separate `$ millions` and `% GDP` worksheet blocks are
#' parsed as distinct measures, and footnote-marked year labels retain their
#' observations.
#'
#' @param release_id Exact catalogue release. Defaults to the current HFI.
#' @param file Optional local XLSX or canonical CSV. When omitted, an official
#'   byte-pinned workbook is retrieved.
#' @param revision `as_published` or `latest`.
#' @param sheets Optional supported workbook sheets to parse.
#' @param cache_dir Hash-addressed raw snapshot cache.
#' @param accept_terms Required for live retrieval only.
#' @param timeout_sec Bounded live request timeout.
#' @param max_bytes Maximum accepted artifact size.
#' @return A long-form `nz_fiscal_data` data frame with provenance.
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
