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

#' Read New Zealand Treasury tax outturn history
#'
#' Reads and normalises a New Zealand Treasury tax outturn release. The parser
#' maps the official `AllRevenue` and `AllReceipts` workbook sheets to a single
#' long-form table while retaining the distinction between accrual revenue and
#' cash receipts. It also keeps the source's unconsolidated rows, Section III
#' consolidation eliminations, core Crown total, and whole-of-government total
#' as separate, explicitly labelled observations. This makes it possible to
#' compare tax measures without silently mixing accounting or consolidation
#' regimes.
#'
#' The default is the current byte-pinned tax outturn release in the package
#' catalogue. An existing XLSX workbook or canonical CSV may be supplied for a
#' deterministic offline replay; supplying `file` never performs network I/O.
#' Without a local file, retrieval is HTTPS-only from the official Treasury
#' host and requires `accept_terms = TRUE` after reviewing
#' [nz_fiscal_terms()]. Downloaded bytes are checked against the catalogue
#' SHA-256 before they are parsed.
#'
#' @details
#' For XLSX input, `measure = "both"` parses `AllRevenue` first and
#' `AllReceipts` second. `measure = "revenue"` or `"receipts"` parses only the
#' corresponding sheet. Rows retain workbook row and monthly-column order;
#' the function does not sort, aggregate, interpolate, or otherwise adjust
#' values. For canonical CSV input, the file is read as an already normalised
#' result with the schema documented under `Value`; selecting one measure
#' filters rows without changing their input order.
#'
#' `date` is the monthly observation date represented by the source header.
#' `fiscal_year` is derived from that date using the Treasury July--June
#' convention: July through December belong to the fiscal year ending the next
#' calendar year, and January through June belong to the fiscal year ending in
#' the current calendar year. The resulting label is `"YYYY/YY"`. Revenue is
#' labelled `accounting_basis = "accrual"` and receipts
#' `accounting_basis = "cash"`; all values use `unit = "NZD million"`.
#'
#' Revenue rows dated before 1 June 2007 have
#' `writeoff_regime = "net_of_tax_writeoffs"`; revenue rows dated on or after
#' 1 June 2007 have `writeoff_regime = "gross_of_tax_writeoffs"`. Receipts
#' always have `writeoff_regime = "not_applicable_to_cash_receipts"`. The
#' break is represented as source metadata and does not rewrite historical
#' values.
#'
#' Consolidation scope is assigned from the source section and series label.
#' Rows outside `Section III:` are `"unconsolidated"`. Within Section III,
#' `"CORE CROWN TAX"` is `"core Crown consolidated"`,
#' `"FULLY CONSOLIDATED TAX"` is `"whole-of-government consolidated"`, and
#' the recognised elimination labels are assigned
#' `"consolidation elimination"`: `"GST ON DEPARTMENTAL EXPENSES"`,
#' `"OTHER CROWN GST"`, `"INCOME TAX FROM NZSF AND GSF"`,
#' `"CORE CROWN EMPLOYER SUPERANNUATION CONTRIBUTION TAX"`,
#' `"CORE CROWN APPROVED ISSUER LEVY"`, `"GAMING DUTY FROM NZLC"`,
#' `"INCOME TAX FROM SOES AND CES"`, and
#' `"OTHER CROWN EMPLOYER SUPERANNUATION CONTRIBUTION TAX"`. An unknown
#' Section III label is an error rather than an inferred scope, so source
#' schema drift is visible to callers.
#'
#' Parsed results use an in-memory cache keyed by the raw SHA-256, release,
#' revision policy, parser and schema versions, and parser options. Raw
#' snapshots are hash-addressed below `cache_dir` and are subject to the
#' package's raw-cache bounds. The default cache is
#' `tools::R_user_dir("readfiscalnz", "cache")/v1`; using `tempfile()` for
#' `cache_dir` is useful for isolated examples and tests. Reading a local file
#' can create package-managed files below `cache_dir`, but does not modify the
#' input file. A live request is refused when `accept_terms = FALSE`; when it
#' is enabled, transport is timeout- and size-bounded, permits only HTTPS
#' redirects, validates that the final effective URL remains on the Treasury
#' host, and uses bounded attempts for transport errors and HTTP 408, 429, 500,
#' 502, 503, and 504 responses.
#'
#' Invalid argument values, an absent or unsupported release, a missing
#' workbook sheet, an empty or malformed workbook, a non-CSV/XLSX artifact,
#' an artifact outside `max_bytes`, an invalid file signature, an unexpected
#' CSV schema, missing dates or values, semantic drift in accounting or
#' consolidation labels, and an official SHA-256 mismatch all fail with an
#' error. The reader does not attempt to repair changed Treasury layouts or
#' fill missing observations. The catalogue must be updated before a new
#' release or source schema can be consumed.
#'
#' @param release_id A character scalar identifying an exact tax outturn
#'   catalogue release, normally `"tax-outturn-2026-05"`. If `NULL` (the
#'   default), the release selected by `revision` is used. See
#'   [list_nz_fiscal_releases()] for release identifiers, dates, licensing,
#'   and revision notes.
#' @param file An optional path to one existing local `.xlsx` workbook or
#'   canonical `.csv` artifact. The default, `NULL`, resolves the selected
#'   official Treasury artifact over HTTPS. Local artifacts are checked for a
#'   supported extension, non-empty size, and file signature, copied into the
#'   hash-addressed raw cache, and parsed without changing the input or using
#'   the network.
#' @param revision A character scalar matched against
#'   `c("as_published", "latest")`; the default is `"as_published"`. With no
#'   explicit `release_id`, `"as_published"` selects the newest current
#'   catalogue row and falls back to the newest issue date when the family has
#'   no current row; `"latest"` always selects the newest issue date. The
#'   selected policy is recorded in provenance.
#' @param measure A character scalar matched against `c("both", "revenue",
#'   "receipts")`; the default, `"both"`, returns both measures. This is a
#'   single selection, not a vector of selections.
#' @param cache_dir A character scalar naming the raw snapshot cache root.
#'   The default is `.nz_cache_dir()`, normally
#'   `tools::R_user_dir("readfiscalnz", "cache")/v1`. The directory and its
#'   package-managed subdirectories may be created as a side effect. Parsed
#'   results are retained only in the current R process.
#' @param accept_terms A logical scalar controlling live retrieval. The
#'   default, `FALSE`, refuses a download; set to `TRUE` only after reviewing
#'   [nz_fiscal_terms()] and the Treasury licence metadata. This argument has
#'   no effect when `file` is supplied.
#' @param timeout_sec A positive numeric scalar timeout for live artifact
#'   transport, in seconds. The default is `30`. It is ignored when `file` is
#'   supplied, apart from input validation.
#' @param max_bytes A positive numeric scalar maximum accepted artifact size
#'   in bytes. The default is `25 * 1024^2` (26,214,400 bytes). Local and
#'   downloaded artifacts outside the bound are rejected before parsing.
#' @return A data frame with class `c("nz_fiscal_data", "data.frame")`, one
#'   row per parsed monthly tax observation. Columns are returned in this exact
#'   order: `date`, `fiscal_year`, `measure`, `accounting_basis`,
#'   `consolidation_scope`, `section`, `series`, `value`, `unit`, and
#'   `writeoff_regime`. `date` is a `Date`; `value` is numeric; all remaining
#'   columns are character. `measure` is `"revenue"` or `"receipts"`,
#'   `accounting_basis` is respectively `"accrual"` or `"cash"`, and `unit`
#'   is exactly `"NZD million"`. `fiscal_year` is a `"YYYY/YY"` July--June
#'   label. Dates and values are validated as non-missing, and the accounting,
#'   consolidation, fiscal-year, unit, and write-off fields are checked against
#'   the rules in Details. The result preserves source order and has no extra
#'   columns.
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
#'   `parser_options`. For tax outturn these version values are
#'   `"tax-1.1.0"` and `"tax-long-2"`; the parser options record the selected
#'   measure, supported consolidation scopes, and the revenue write-off break.
#'   Use [nz_fiscal_provenance()] to inspect the complete list. The attribute
#'   is retained by downstream [nz_fiscal_series_catalogue()] and
#'   [nz_fiscal_series_history()] calls.
#'
#' @seealso [list_nz_fiscal_releases()], [nz_fiscal_terms()],
#'   [nz_fiscal_provenance()], [nz_fiscal_raw_bytes()],
#'   [nz_fiscal_series_catalogue()], [nz_fiscal_series_history()],
#'   [read_nz_hfi()], [read_nz_monthly_financials()], [read_nz_efu()]
#' @family fiscal-readers
#' @examples
#' tax_file <- system.file("extdata", "tax.csv", package = "readfiscalnz")
#' tax <- read_nz_tax_outturn(
#'   file = tax_file,
#'   measure = "both",
#'   cache_dir = tempfile("readfiscalnz-tax-cache-")
#' )
#' stopifnot(
#'   inherits(tax, "nz_fiscal_data"),
#'   inherits(tax$date, "Date"),
#'   is.numeric(tax$value),
#'   identical(names(tax), c(
#'     "date", "fiscal_year", "measure", "accounting_basis",
#'     "consolidation_scope", "section", "series", "value", "unit",
#'     "writeoff_regime"
#'   )),
#'   all(tax$measure %in% c("revenue", "receipts")),
#'   all(tax$unit == "NZD million")
#' )
#' head(tax[c("date", "fiscal_year", "measure", "series", "value",
#'            "writeoff_regime")])
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
