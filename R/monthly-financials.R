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
  as.Date(date_text, "%d %B %Y")
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
#' Supports six explicitly mapped primary statement sheets and fails closed on
#' other workbook tabs. Values remain whole-of-government consolidated and
#' retain actual, forecast, variance, period, and accounting-basis semantics.
#'
#' @inheritParams read_nz_hfi
#' @param table One or more of `performance`, `comprehensive`, `net_worth`,
#'   `cash_flows`, `cash_flow_reconciliation`, or `position`.
#' @return A long-form `nz_fiscal_data` data frame with provenance.
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
