.monthly_tables <- c(
  performance = "Fin Stmts - Perform",
  comprehensive = "Fin Stmts - Comp",
  net_worth = "Fin Stmts - Net Worth",
  cash_flows = "Fin Stmts - Cash flows",
  cash_flow_reconciliation = "Fin Stmts - CF Rec",
  position = "Fin Stmts - Position"
)

.monthly_report_date <- function(x) {
  text <- paste(.clean_text(unlist(x[seq_len(min(4L, nrow(x))), , drop = FALSE])), collapse = " ")
  date_text <- regmatches(
    text,
    regexpr("[0-9]{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}", text)
  )
  if (!length(date_text) || !nzchar(date_text)) {
    return(as.Date(NA))
  }
  .parse_english_date(date_text)
}

.parse_monthly_sheet <- function(path, sheet, table) {
  x <- .read_sheet_text(path, sheet)
  if (ncol(x) < 9L || nrow(x) < 10L) {
    .abort("Monthly financial sheet '%s' is smaller than the supported 9-column schema.", sheet)
  }
  title_text <- paste(.clean_text(unlist(x[seq_len(min(3L, nrow(x))), , drop = FALSE])), collapse = " ")
  if (!grepl("STATEMENT|RECONCILIATION", title_text, ignore.case = TRUE)) {
    .abort("Monthly financial sheet '%s' lacks a recognised statement title.", sheet)
  }
  report_date <- .monthly_report_date(x)
  if (is.na(report_date)) {
    .abort("Monthly financial sheet '%s' lacks a recognised report date.", sheet)
  }
  current_fy_end <- as.integer(format(report_date, "%Y"))
  if (as.integer(format(report_date, "%m")) > 6L) {
    current_fy_end <- current_fy_end + 1L
  }
  column_spec <- data.frame(
    column = c(2L, 5L, 6L, 7L, 8L, 9L),
    observation = c(
      "prior_year_actual", "current_year_to_date_actual",
      "current_year_to_date_forecast", "variance",
      "variance_percent", "annual_forecast"
    ),
    actual_forecast = c("actual", "actual", "forecast", "derived", "derived", "forecast"),
    period_kind = c("annual", "year_to_date", "year_to_date", "year_to_date", "year_to_date", "annual"),
    unit = c("NZD million", "NZD million", "NZD million", "NZD million", "percent", "NZD million"),
    stringsAsFactors = FALSE
  )
  output <- list()
  cursor <- 0L
  section <- NA_character_
  for (row in seq.int(9L, nrow(x))) {
    label3 <- .clean_text(x[[3L]][row])
    label1 <- .clean_text(x[[1L]][row])
    label <- if (nzchar(label3)) label3 else label1
    if (!nzchar(label)) {
      next
    }
    row_values <- .as_number(unlist(x[row, column_spec$column, drop = FALSE], use.names = FALSE))
    if (!any(!is.na(row_values))) {
      if (!grepl("^\\$|^%|^Note$", label, ignore.case = TRUE)) {
        section <- label
      }
      next
    }
    for (index in which(!is.na(row_values))) {
      spec <- column_spec[index, , drop = FALSE]
      period_end <- if (spec$observation == "prior_year_actual") {
        as.Date(sprintf("%d-06-30", current_fy_end - 1L))
      } else if (spec$observation == "annual_forecast") {
        as.Date(sprintf("%d-06-30", current_fy_end))
      } else {
        report_date
      }
      cursor <- cursor + 1L
      output[[cursor]] <- data.frame(
        table = table,
        statement_sheet = sheet,
        period_end = period_end,
        fiscal_year = sprintf("%d/%02d", current_fy_end - 1L, current_fy_end %% 100L),
        period_kind = spec$period_kind,
        observation = spec$observation,
        actual_forecast = spec$actual_forecast,
        consolidation_scope = "whole-of-government consolidated",
        accounting_basis = "accrual PBE Standards",
        section = section,
        series = label,
        note = .clean_text(x[[4L]][row]) %||% NA_character_,
        value = row_values[[index]],
        unit = spec$unit,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(output)) {
    .abort("Monthly financial sheet '%s' contained no supported observations.", sheet)
  }
  do.call(rbind, output)
}

.validate_monthly <- function(x) {
  required <- c(
    "table", "statement_sheet", "period_end", "fiscal_year",
    "period_kind", "observation", "actual_forecast", "consolidation_scope",
    "accounting_basis", "section", "series", "note", "value", "unit"
  )
  .require_exact_columns(x, required, "Monthly financials")
  x$period_end <- as.Date(x$period_end)
  x$value <- as.numeric(x$value)
  if (anyNA(x$period_end) || anyNA(x$value)) {
    .abort("Monthly financial schema contains missing period_end or value fields.")
  }
  if (any(!x$table %in% names(.monthly_tables))) {
    .abort("Monthly financial table drifted outside the supported routes.")
  }
  expected_sheet <- unname(.monthly_tables[x$table])
  if (any(x$statement_sheet != expected_sheet)) {
    .abort("Monthly financial statement-sheet drift was detected.")
  }
  observation_contract <- data.frame(
    observation = c(
      "prior_year_actual",
      "current_year_to_date_actual",
      "current_year_to_date_forecast",
      "variance",
      "variance_percent",
      "annual_forecast"
    ),
    actual_forecast = c(
      "actual", "actual", "forecast", "derived", "derived", "forecast"
    ),
    period_kind = c(
      "annual", "year_to_date", "year_to_date",
      "year_to_date", "year_to_date", "annual"
    ),
    unit = c(
      "NZD million", "NZD million", "NZD million",
      "NZD million", "percent", "NZD million"
    ),
    stringsAsFactors = FALSE
  )
  matched <- match(x$observation, observation_contract$observation)
  if (anyNA(matched)) {
    .abort("Monthly financial observation status is unsupported.")
  }
  if (any(x$actual_forecast != observation_contract$actual_forecast[matched])) {
    .abort("Monthly financial actual/forecast status drift was detected.")
  }
  if (any(x$period_kind != observation_contract$period_kind[matched])) {
    .abort("Monthly financial period status drift was detected.")
  }
  if (any(x$unit != observation_contract$unit[matched])) {
    .abort("Monthly financial unit drift was detected.")
  }
  if (any(x$consolidation_scope != "whole-of-government consolidated")) {
    .abort("Monthly financial consolidation drift was detected.")
  }
  if (any(x$accounting_basis != "accrual PBE Standards")) {
    .abort("Monthly financial accounting-basis drift was detected.")
  }
  x
}

#' Read monthly Financial Statements of the Government
#'
#' Reads an official New Zealand Treasury monthly Financial Statements of the
#' Government release and returns its supported primary-statement observations
#' in a long, analysis-ready form. The parser deliberately maps only the six
#' recognised statement sheets and fails closed when a workbook is missing a
#' required sheet or its published structure drifts. A canonical CSV can be
#' supplied for deterministic, offline replay through the same validation
#' path.
#'
#' @details
#' The `monthly_financials` release family currently maps the following
#' `table` values to workbook sheets: `performance` to `Fin Stmts - Perform`,
#' `comprehensive` to `Fin Stmts - Comp`, `net_worth` to `Fin Stmts - Net
#' Worth`, `cash_flows` to `Fin Stmts - Cash flows`,
#' `cash_flow_reconciliation` to `Fin Stmts - CF Rec`, and `position` to `Fin
#' Stmts - Position`. A workbook may contain other tabs, but they are not
#' parsed or silently treated as supported data. Use
#' [list_nz_fiscal_releases()] to inspect the byte-pinned monthly release and
#' its publication metadata.
#'
#' Each selected sheet can contribute the following observation roles:
#' `prior_year_actual` (annual actual, `NZD million`),
#' `current_year_to_date_actual` (year-to-date actual, `NZD million`),
#' `current_year_to_date_forecast` (year-to-date forecast, `NZD million`),
#' `variance` (year-to-date derived variance, `NZD million`),
#' `variance_percent` (year-to-date derived variance, `percent`), and
#' `annual_forecast` (annual forecast, `NZD million`). A source cell that is
#' absent or non-numeric does not produce an observation. `period_end` is the
#' prior or current June 30 for annual roles and the publication report date
#' for year-to-date roles. `fiscal_year` is retained as a `YYYY/YY` label. The
#' parser records the source's whole-of-government consolidated scope and
#' accrual PBE Standards accounting basis; it does not convert units, merge
#' releases, or infer additional observations.
#'
#' Rows are emitted in the requested `table` order, then in workbook row and
#' source-column order. Canonical CSV input retains its input row order. No
#' sorting, deduplication, interpolation, or revision blending is performed.
#' Monthly publications are immutable release snapshots: a later Treasury
#' publication may supersede an earlier one, but this reader preserves the
#' selected publication vintage.
#'
#' When `file` is `NULL`, the selected official Treasury artifact is fetched
#' only after `accept_terms = TRUE`; transport is HTTPS-only, bounded by
#' `timeout_sec` and `max_bytes`, retried within the package's configured
#' attempt limit, and checked against its catalogue SHA-256. Supplying a
#' local `.xlsx` or canonical `.csv` avoids network access and terms
#' acceptance, but the file is still signature-checked and copied into the
#' content-addressed raw cache. Parsing also uses a bounded in-memory cache
#' keyed by the raw hash, release, revision policy, parser version, schema
#' version, and selected tables. The default cache is
#' `tools::R_user_dir("readfiscalnz", "cache")/v1`; passing a directory below
#' `tempdir()` is useful for isolated examples and tests. Cache files are the
#' only persistent side effect; the supplied input file is not modified.
#'
#' Errors are raised for an unknown `table`, absent or unreadable input,
#' unsupported file type, empty or oversized artifacts, malformed workbook
#' signatures, missing required sheets, unrecognised statement titles or
#' dates, schema drift, missing required values, or failed official transport,
#' terms, or hash checks. There is no warning-based fallback to arbitrary
#' workbook tabs when these checks fail.
#'
#' @param release_id A character scalar identifying an exact catalogue
#'   release, normally `"monthly-financials-2026-02"`. If `NULL` (the default),
#'   the release selected by `revision` is used. Use
#'   [list_nz_fiscal_releases()] to inspect supported releases and their
#'   metadata.
#' @param file An optional path to one existing local `.xlsx` workbook or
#'   canonical `.csv` artifact. The default, `NULL`, resolves the selected
#'   official Treasury artifact over HTTPS. Local files are size- and
#'   signature-checked, copied into the raw cache, and never changed.
#' @param revision A character scalar matched against
#'   `c("as_published", "latest")`; the default is `"as_published"`. With no
#'   explicit `release_id`, `"as_published"` selects the newest current
#'   catalogue row and falls back to the newest issue date when the family has
#'   no current row; `"latest"` always selects the newest issue date. The
#'   selected policy is recorded in provenance.
#' @param table A character vector of one or more supported statement routes.
#'   The default is `"performance"`. Valid values are `"performance"`,
#'   `"comprehensive"`, `"net_worth"`, `"cash_flows"`,
#'   `"cash_flow_reconciliation"`, and `"position"`. The order supplied is
#'   retained in the returned rows.
#' @param cache_dir A character scalar naming the raw snapshot cache root.
#'   The default is `.nz_cache_dir()`, normally
#'   `tools::R_user_dir("readfiscalnz", "cache")/v1`; the directory and its
#'   package-managed subdirectories may be created as a side effect. The
#'   `readfiscalnz.cache_dir` option can override the default.
#' @param accept_terms A logical scalar controlling live retrieval. The
#'   default, `FALSE`, refuses a download; set to `TRUE` only after reviewing
#'   [nz_fiscal_terms()] and the Treasury licence metadata. This argument has
#'   no effect when `file` is supplied.
#' @param timeout_sec A positive numeric scalar timeout for live artifact
#'   transport, in seconds. The default is `30`. It is ignored when `file` is
#'   supplied, apart from input validation.
#' @param max_bytes A positive numeric scalar maximum accepted artifact size
#'   in bytes. The default is `25 * 1024^2` (26,214,400 bytes). Local and
#'   downloaded artifacts outside this bound are rejected before parsing.
#' @return A data frame with class `c("nz_fiscal_data", "data.frame")`, one
#'   row per parsed numeric observation. Columns are returned in this exact
#'   order: `table`, `statement_sheet`, `period_end`, `fiscal_year`,
#'   `period_kind`, `observation`, `actual_forecast`, `consolidation_scope`,
#'   `accounting_basis`, `section`, `series`, `note`, `value`, and `unit`.
#'   `period_end` is a `Date` and `value` is numeric; all other columns are
#'   character. `table` and `statement_sheet` obey the six route mapping in
#'   Details. `period_kind`, `observation`, `actual_forecast`, and `unit` obey
#'   the observation contracts described in Details; `consolidation_scope` is
#'   `"whole-of-government consolidated"` and `accounting_basis` is
#'   `"accrual PBE Standards"` for every row. No row-order or uniqueness
#'   guarantee is implied beyond the source-order rule in Details.
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
#'   `parser_options`. For this reader these version values are
#'   `"monthly-1.0.0"` and `"monthly-long-1"`; `parser_options` records the
#'   selected tables and supported sheet names. Use
#'   [nz_fiscal_provenance()] to inspect the complete list. The attribute is
#'   retained by downstream [nz_fiscal_series_catalogue()] and
#'   [nz_fiscal_series_history()] calls.
#' @seealso [list_nz_fiscal_releases()], [nz_fiscal_terms()],
#'   [nz_fiscal_provenance()], [nz_fiscal_series_catalogue()],
#'   [nz_fiscal_series_history()], [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_efu()]
#' @family fiscal-readers
#' @examples
#' monthly_file <- system.file("extdata", "monthly.csv", package = "readfiscalnz")
#' stopifnot(nzchar(monthly_file))
#' monthly <- read_nz_monthly_financials(
#'   file = monthly_file,
#'   table = "performance",
#'   cache_dir = tempfile("readfiscalnz-monthly-cache-")
#' )
#' stopifnot(
#'   inherits(monthly, "nz_fiscal_data"),
#'   inherits(monthly$period_end, "Date"),
#'   is.numeric(monthly$value),
#'   identical(names(monthly), c(
#'     "table", "statement_sheet", "period_end", "fiscal_year",
#'     "period_kind", "observation", "actual_forecast",
#'     "consolidation_scope", "accounting_basis", "section", "series",
#'     "note", "value", "unit"
#'   ))
#' )
#' head(monthly[c("period_end", "observation", "actual_forecast", "value")])
#' @export
read_nz_monthly_financials <- function(
    release_id = NULL,
    file = NULL,
    revision = c("as_published", "latest"),
    table = "performance",
    cache_dir = .nz_cache_dir(),
    accept_terms = FALSE,
    timeout_sec = 30,
    max_bytes = 25 * 1024^2) {
  revision <- match.arg(revision)
  unknown <- setdiff(table, names(.monthly_tables))
  if (length(unknown)) {
    .abort(
      "Unsupported monthly table(s): %s. Supported: %s",
      paste(unknown, collapse = ", "),
      paste(names(.monthly_tables), collapse = ", ")
    )
  }
  artifact <- .resolve_artifact(
    "monthly_financials", release_id, file, revision, cache_dir,
    timeout_sec, max_bytes, accept_terms
  )
  parser_options <- list(
    table = table,
    supported_sheets = unname(.monthly_tables)
  )
  cache_key <- .parsed_cache_key(
    artifact, "monthly_financials", parser_options
  )
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) {
    return(.finish_result(
      cached, artifact, "monthly_financials", parser_options
    ))
  }
  if (artifact$extension == "csv") {
    data <- .read_normalized_csv(artifact$path)
    data <- data[data$table %in% table, , drop = FALSE]
  } else {
    data <- do.call(rbind, lapply(table, function(name) {
      .parse_monthly_sheet(artifact$path, .monthly_tables[[name]], name)
    }))
  }
  data <- .validate_monthly(data)
  result <- .finish_result(
    data, artifact, "monthly_financials", parser_options
  )
  .cache_set(cache_key, data)
  result
}
