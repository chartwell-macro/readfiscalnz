.tax_fiscal_year <- function(date) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  end <- ifelse(month >= 7L, year + 1L, year)
  sprintf("%d/%02d", end - 1L, end %% 100L)
}

.tax_consolidation_scope <- function(section, series) {
  if (!grepl("^Section III:", section %||% "", ignore.case = TRUE)) {
    return("unconsolidated")
  }
  label <- toupper(.clean_text(series))
  if (identical(label, "CORE CROWN TAX")) {
    return("core Crown consolidated")
  }
  if (identical(label, "FULLY CONSOLIDATED TAX")) {
    return("whole-of-government consolidated")
  }
  elimination_labels <- c(
    "GST ON DEPARTMENTAL EXPENSES",
    "OTHER CROWN GST",
    "INCOME TAX FROM NZSF AND GSF",
    "CORE CROWN EMPLOYER SUPERANNUATION CONTRIBUTION TAX",
    "CORE CROWN APPROVED ISSUER LEVY",
    "GAMING DUTY FROM NZLC",
    "INCOME TAX FROM SOES AND CES",
    "OTHER CROWN EMPLOYER SUPERANNUATION CONTRIBUTION TAX"
  )
  if (label %in% elimination_labels) {
    return("consolidation elimination")
  }
  .abort(
    "Unknown Section III consolidation label '%s'; schema review is required.",
    series
  )
}

.parse_tax_sheet <- function(path, sheet, measure) {
  x <- .read_sheet_text(path, sheet)
  if (ncol(x) < 3L || nrow(x) < 6L) {
    .abort("Tax sheet '%s' is smaller than the supported schema.", sheet)
  }
  scan_rows <- seq_len(min(4L, nrow(x)))
  date_counts <- vapply(scan_rows, function(row) {
    sum(!is.na(.excel_date(unlist(x[row, -1L, drop = FALSE], use.names = FALSE))))
  }, integer(1L))
  date_row <- scan_rows[which.max(date_counts)]
  dates <- .excel_date(unlist(x[date_row, -1L, drop = FALSE], use.names = FALSE))
  if (sum(!is.na(dates)) < 2L) {
    .abort("Tax sheet '%s' has no supported monthly date row.", sheet)
  }
  output <- list()
  cursor <- 0L
  section <- NA_character_
  for (row in seq.int(date_row + 1L, nrow(x))) {
    label <- .clean_text(x[[1L]][row])
    if (!nzchar(label)) {
      next
    }
    values <- .as_number(unlist(x[row, -1L, drop = FALSE], use.names = FALSE))
    if (grepl("^Section [IVX]+:", label, ignore.case = TRUE)) {
      section <- label
      next
    }
    keep <- !is.na(dates) & !is.na(values)
    if (!any(keep)) {
      next
    }
    selected_dates <- dates[keep]
    cursor <- cursor + 1L
    output[[cursor]] <- data.frame(
      date = selected_dates,
      fiscal_year = .tax_fiscal_year(selected_dates),
      measure = measure,
      accounting_basis = if (measure == "revenue") "accrual" else "cash",
      consolidation_scope = .tax_consolidation_scope(section, label),
      section = section,
      series = label,
      value = values[keep],
      unit = "NZD million",
      writeoff_regime = if (measure == "revenue") {
        ifelse(
          selected_dates >= as.Date("2007-06-01"),
          "gross_of_tax_writeoffs",
          "net_of_tax_writeoffs"
        )
      } else {
        "not_applicable_to_cash_receipts"
      },
      stringsAsFactors = FALSE
    )
  }
  if (!length(output)) {
    .abort("Tax sheet '%s' contained no labelled monthly values.", sheet)
  }
  do.call(rbind, output)
}

.validate_tax <- function(x) {
  required <- c(
    "date", "fiscal_year", "measure", "accounting_basis",
    "consolidation_scope", "section", "series", "value", "unit",
    "writeoff_regime"
  )
  .require_exact_columns(x, required, "Tax outturn")
  x$date <- as.Date(x$date)
  x$value <- as.numeric(x$value)
  if (anyNA(x$date) || anyNA(x$value)) {
    .abort("Tax outturn schema contains missing date or value fields.")
  }
  allowed <- c("revenue", "receipts")
  if (any(!x$measure %in% allowed)) {
    .abort("Tax outturn measure must be revenue or receipts.")
  }
  if (any(x$unit != "NZD million")) {
    .abort("Tax outturn unit must be exactly 'NZD million'.")
  }
  expected_basis <- ifelse(x$measure == "revenue", "accrual", "cash")
  if (any(x$accounting_basis != expected_basis)) {
    .abort("Tax outturn accounting-basis drift was detected.")
  }
  expected_scope <- vapply(seq_len(nrow(x)), function(index) {
    .tax_consolidation_scope(x$section[[index]], x$series[[index]])
  }, character(1L))
  if (any(x$consolidation_scope != expected_scope)) {
    .abort("Tax outturn consolidation drift was detected.")
  }
  expected_fiscal_year <- .tax_fiscal_year(x$date)
  if (any(x$fiscal_year != expected_fiscal_year)) {
    .abort("Tax outturn fiscal-year drift was detected.")
  }
  expected_writeoff <- ifelse(
    x$measure == "receipts",
    "not_applicable_to_cash_receipts",
    ifelse(
      x$date >= as.Date("2007-06-01"),
      "gross_of_tax_writeoffs",
      "net_of_tax_writeoffs"
    )
  )
  if (any(x$writeoff_regime != expected_writeoff)) {
    .abort("Tax outturn write-off status drift was detected.")
  }
  x
}

#' Read monthly tax outturn history
#'
#' Parses the official `AllRevenue` and `AllReceipts` sheets. Revenue remains
#' accrual and receipts remain cash. Unconsolidated source rows, Section III
#' elimination adjustments, core Crown totals, and fully consolidated totals
#' retain distinct consolidation scopes. Revenue values from June 2007 are
#' explicitly marked as gross of tax write-offs.
#'
#' @inheritParams read_nz_hfi
#' @param measure `both`, `revenue`, or `receipts`.
#' @return A long-form `nz_fiscal_data` data frame with provenance.
#' @export
read_nz_tax_outturn <- function(
    release_id = NULL,
    file = NULL,
    revision = c("as_published", "latest"),
    measure = c("both", "revenue", "receipts"),
    cache_dir = .nz_cache_dir(),
    accept_terms = FALSE,
    timeout_sec = 30,
    max_bytes = 25 * 1024^2) {
  revision <- match.arg(revision)
  measure <- match.arg(measure)
  artifact <- .resolve_artifact(
    "tax_outturn", release_id, file, revision, cache_dir,
    timeout_sec, max_bytes, accept_terms
  )
  parser_options <- list(
    measure = measure,
    consolidation = c(
      "unconsolidated",
      "consolidation elimination",
      "core Crown consolidated",
      "whole-of-government consolidated"
    ),
    revenue_writeoff_break = "2007-06-01"
  )
  cache_key <- .parsed_cache_key(
    artifact, "tax_outturn", parser_options
  )
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) {
    return(.finish_result(
      cached, artifact, "tax_outturn", parser_options
    ))
  }
  if (artifact$extension == "csv") {
    data <- .read_normalized_csv(artifact$path)
    if (measure != "both") {
      data <- data[data$measure == measure, , drop = FALSE]
    }
  } else {
    routes <- list(revenue = "AllRevenue", receipts = "AllReceipts")
    selected <- if (measure == "both") names(routes) else measure
    data <- do.call(rbind, lapply(selected, function(name) {
      .parse_tax_sheet(artifact$path, routes[[name]], name)
    }))
  }
  data <- .validate_tax(data)
  result <- .finish_result(
    data, artifact, "tax_outturn", parser_options
  )
  .cache_set(cache_key, data)
  result
}
