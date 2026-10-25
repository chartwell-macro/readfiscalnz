.efu_sheets <- c(
  fiscal = "Fiscal Time Series",
  economic = "Economic Time Series",
  key_indicators = "Table 1"
)

.efu_sheet_name <- function(dataset, release_type) {
  if (dataset == "key_indicators" && release_type == "PREFU") {
    return("Executive Summary Table")
  }
  .efu_sheets[[dataset]]
}

.validate_efu_workbook_identity <- function(path, release) {
  index <- .read_sheet_text(path, "Index")
  values <- .clean_text(unlist(index, use.names = FALSE))
  values <- unique(values[nzchar(values)])
  text <- paste(values, collapse = " | ")
  title <- values[[1L]] %||% ""
  published_text <- regmatches(
    text,
    regexpr(
      "Published [0-9]{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}",
      text
    )
  )
  published_date <- if (length(published_text) && nzchar(published_text)) {
    as.Date(sub("^Published ", "", published_text), "%d %B %Y")
  } else {
    as.Date(NA)
  }
  expected_landing <- sub(
    "^https://www\\.",
    "https://",
    release$landing_url[[1L]]
  )
  identity_matches <-
    identical(title, release$workbook_title[[1L]]) &&
    !is.na(published_date) &&
    identical(published_date, release$issue_date[[1L]]) &&
    grepl(expected_landing, text, fixed = TRUE) &&
    grepl("Crown copyright", text, ignore.case = TRUE) &&
    grepl(
      "Creative Commons Attribution 4.0 International licence",
      text,
      fixed = TRUE
    ) &&
    grepl("creativecommons.org/licenses/by/4.0/", text, fixed = TRUE)
  if (!isTRUE(identity_matches)) {
    .abort(
      paste0(
        "EFU workbook release identity does not match '%s'; title, ",
        "publication date, landing page, and licence require review."
      ),
      release$release_id[[1L]]
    )
  }
  list(
    title = title,
    published_date = published_date,
    terms_statement = release$terms_statement[[1L]],
    license_url = release$license_url[[1L]]
  )
}

.efu_unit_spec <- function(context, strict = TRUE) {
  context <- .clean_text(context)
  lower <- tolower(context)
  unit <- multiplier <- NULL
  if (grepl("\\$[[:space:]]*billions?", lower)) {
    unit <- "NZD billion"
    multiplier <- 1e9
  } else if (grepl("\\$[[:space:]]*millions?", lower)) {
    unit <- "NZD million"
    multiplier <- 1e6
  } else if (grepl("contribution to growth", lower)) {
    unit <- "percentage points"
    multiplier <- 1
  } else if (grepl("%|percent|percentage|\\brate\\b", lower)) {
    unit <- "percent"
    multiplier <- 1
  } else if (grepl("\\btwi\\b|\\bindex\\b", lower)) {
    unit <- "index"
    multiplier <- 1
  }
  if (is.null(unit)) {
    if (strict) {
      .abort(
        "EFU unit schema mismatch: no explicit unit context in '%s'.",
        context
      )
    }
    return(NULL)
  }
  list(
    unit = unit,
    unit_multiplier = multiplier,
    unit_context = context
  )
}

.parse_efu_sheet <- function(path, sheet, dataset, release) {
  x <- .read_sheet_text(path, sheet)
  if (ncol(x) < 4L || nrow(x) < 6L) {
    .abort("EFU sheet '%s' is smaller than the supported wide schema.", sheet)
  }
  scan_rows <- seq_len(min(12L, nrow(x)))
  year_counts <- vapply(scan_rows, function(row) {
    values <- suppressWarnings(as.integer(.clean_text(unlist(x[row, , drop = FALSE], use.names = FALSE))))
    sum(!is.na(values) & values >= 1900L & values <= 2100L)
  }, integer(1L))
  header_row <- scan_rows[which.max(year_counts)]
  if (max(year_counts) < 3L) {
    .abort("EFU sheet '%s' has no supported multi-year header.", sheet)
  }
  header_values <- suppressWarnings(as.integer(.clean_text(unlist(x[header_row, , drop = FALSE], use.names = FALSE))))
  year_columns <- which(!is.na(header_values) & header_values >= 1900L & header_values <= 2100L)
  first_year_column <- min(year_columns)
  metric_column <- first_year_column - 1L
  if (metric_column < 1L) {
    .abort("EFU sheet '%s' has no metric column before the year header.", sheet)
  }
  status_row <- header_row + 1L
  statuses <- tolower(.clean_text(unlist(x[status_row, year_columns, drop = FALSE], use.names = FALSE)))
  if (!all(grepl("actual|forecast|estimate|unaudited", statuses))) {
    .abort("EFU sheet '%s' has unrecognised actual/forecast status labels.", sheet)
  }
  years <- header_values[year_columns]
  unit_context <- paste(
    unique(.clean_text(unlist(x[seq_len(header_row - 1L), seq_len(metric_column), drop = FALSE], use.names = FALSE))),
    collapse = " | "
  )
  output <- list()
  cursor <- 0L
  section <- dataset
  current_unit_context <- if (dataset == "economic") {
    .clean_text(x[[metric_column]][status_row])
  } else {
    ""
  }
  current_series_context <- ""
  for (row in seq.int(status_row + 1L, nrow(x))) {
    metric <- .clean_text(x[[metric_column]][row])
    prefix <- if (metric_column > 1L) {
      .clean_text(unlist(x[row, seq_len(metric_column - 1L), drop = FALSE], use.names = FALSE))
    } else {
      character()
    }
    values <- .as_number(unlist(x[row, year_columns, drop = FALSE], use.names = FALSE))
    contextual_subseries <- grepl(
      "^(% of gdp|as a percentage of gdp)$",
      tolower(metric)
    ) && nzchar(current_series_context)
    series <- if (contextual_subseries) {
      paste0(current_series_context, " (% of GDP)")
    } else {
      metric
    }
    row_context <- paste(
      c(
        prefix[nzchar(prefix)],
        if (contextual_subseries) current_series_context else character(),
        metric
      ),
      collapse = " / "
    )
    row_unit <- .efu_unit_spec(row_context, strict = FALSE)
    if (!any(!is.na(values))) {
      if (!is.null(row_unit)) {
        current_unit_context <- row_context
      } else if (nzchar(row_context)) {
        section <- row_context
      }
      next
    }
    if (!nzchar(metric)) {
      next
    }
    if (!contextual_subseries) {
      current_series_context <- metric
    }
    unit <- if (!is.null(row_unit)) {
      row_unit
    } else {
      .efu_unit_spec(current_unit_context)
    }
    keep <- !is.na(values)
    cursor <- cursor + 1L
    output[[cursor]] <- data.frame(
      release_type = release$release_type,
      forecast_vintage = release$forecast_vintage,
      forecast_horizon = release$forecast_horizon,
      dataset = dataset,
      sheet = sheet,
      year = years[keep],
      period_end = as.Date(sprintf("%04d-06-30", years[keep])),
      fiscal_year_basis = "June year",
      actual_forecast = ifelse(grepl("actual|unaudited", statuses[keep]), "actual", "forecast"),
      source_status_label = statuses[keep],
      accounting_basis = "accrual PBE Standards",
      consolidation_scope = "series-specific core Crown or total Crown",
      section = section,
      series = series,
      value = values[keep],
      unit = unit$unit,
      unit_multiplier = unit$unit_multiplier,
      unit_context = unit$unit_context,
      stringsAsFactors = FALSE
    )
  }
  if (!length(output)) {
    .abort("EFU sheet '%s' contained no supported time-series rows.", sheet)
  }
  do.call(rbind, output)
}

.validate_efu <- function(x, release) {
  required <- c(
    "release_type", "forecast_vintage", "forecast_horizon", "dataset",
    "sheet", "year", "period_end", "fiscal_year_basis", "actual_forecast",
    "source_status_label", "accounting_basis", "consolidation_scope",
    "section", "series", "value", "unit", "unit_multiplier", "unit_context"
  )
  .require_exact_columns(x, required, "EFU")
  x$year <- as.integer(x$year)
  x$period_end <- as.Date(x$period_end)
  x$value <- as.numeric(x$value)
  x$unit_multiplier <- as.numeric(x$unit_multiplier)
  if (anyNA(x$year) || anyNA(x$period_end) || anyNA(x$value) ||
      anyNA(x$unit_multiplier) || anyNA(x$unit) ||
      anyNA(x$unit_context) || any(!nzchar(x$unit)) ||
      any(!nzchar(x$unit_context))) {
    .abort(
      paste0(
        "EFU schema contains missing year, period_end, value, ",
        "unit, or unit-context fields."
      )
    )
  }
  if (any(!x$actual_forecast %in% c("actual", "forecast"))) {
    .abort("EFU actual_forecast must be actual or forecast.")
  }
  if (any(x$release_type != release$release_type[[1L]]) ||
      any(x$forecast_vintage != release$forecast_vintage[[1L]]) ||
      any(x$forecast_horizon != release$forecast_horizon[[1L]])) {
    .abort("EFU release metadata drift was detected in canonical data.")
  }
  if (any(!x$dataset %in% names(.efu_sheets))) {
    .abort("EFU dataset drifted outside the supported routes.")
  }
  expected_sheet <- vapply(
    x$dataset,
    .efu_sheet_name,
    character(1L),
    release_type = release$release_type[[1L]]
  )
  if (any(x$sheet != expected_sheet)) {
    .abort("EFU sheet-route drift was detected.")
  }
  expected_period <- as.Date(sprintf("%04d-06-30", x$year))
  if (any(x$period_end != expected_period) ||
      any(x$fiscal_year_basis != "June year")) {
    .abort("EFU fiscal-year status drift was detected.")
  }
  statuses <- tolower(.clean_text(x$source_status_label))
  recognised <- grepl("actual|forecast|estimate|unaudited", statuses)
  if (any(!recognised)) {
    .abort("EFU source status label drift was detected.")
  }
  expected_status <- ifelse(
    grepl("actual|unaudited", statuses),
    "actual",
    "forecast"
  )
  if (any(x$actual_forecast != expected_status)) {
    .abort("EFU actual/forecast status drift was detected.")
  }
  if (any(x$accounting_basis != "accrual PBE Standards") ||
      any(x$consolidation_scope !=
        "series-specific core Crown or total Crown")) {
    .abort("EFU accounting or consolidation drift was detected.")
  }
  unit_multiplier <- c(
    "NZD billion" = 1e9,
    "NZD million" = 1e6,
    "percentage points" = 1,
    "percent" = 1,
    "index" = 1
  )
  expected_multiplier <- unname(unit_multiplier[x$unit])
  if (anyNA(expected_multiplier) ||
      any(x$unit_multiplier != expected_multiplier)) {
    .abort("EFU unit or unit-multiplier drift was detected.")
  }
  x
}

#' Read Economic and Fiscal Update data
#'
#' Reads the stable time-series surfaces in BEFU, HYEFU, and PREFU chart-data
#' workbooks. Forecast values always retain the publication vintage and source
#' status. Generic `% of GDP` subrows inherit their parent metric so distinct
#' key indicators cannot collapse onto the same series identity. The reader
#' targets the named fiscal, economic, and key-indicator
#' time-series surfaces whose units and identities are explicit in each
#' publication.
#'
#' @inheritParams read_nz_hfi
#' @param release_id Exact BEFU, HYEFU, or PREFU release identifier.
#' @param update Optional required release type (`BEFU`, `HYEFU`, or `PREFU`).
#' @param dataset One or more of `fiscal`, `economic`, or `key_indicators`.
#' @return A long-form `nz_fiscal_data` data frame with provenance.
#' @export
read_nz_efu <- function(
    release_id = NULL,
    update = NULL,
    file = NULL,
    revision = c("as_published", "latest"),
    dataset = "fiscal",
    cache_dir = .nz_cache_dir(),
    accept_terms = FALSE,
    timeout_sec = 30,
    max_bytes = 25 * 1024^2) {
  revision <- match.arg(revision)
  if (!is.null(update)) {
    update <- toupper(update)
    if (length(update) != 1L || !update %in% c("BEFU", "HYEFU", "PREFU")) {
      .abort("update must be one of BEFU, HYEFU, or PREFU.")
    }
  }
  unknown <- setdiff(dataset, names(.efu_sheets))
  if (length(unknown)) {
    .abort(
      "Unsupported EFU dataset(s): %s. Supported: %s",
      paste(unknown, collapse = ", "),
      paste(names(.efu_sheets), collapse = ", ")
    )
  }
  artifact <- .resolve_artifact(
    "efu", release_id, file, revision, cache_dir,
    timeout_sec, max_bytes, accept_terms
  )
  if (!is.null(update) && artifact$release$release_type != update) {
    .abort(
      "release_id '%s' is %s, not %s.",
      artifact$release$release_id,
      artifact$release$release_type,
      update
    )
  }
  parser_options <- list(
    dataset = dataset,
    release_type = artifact$release$release_type,
    supported_sheets = vapply(
      dataset,
      .efu_sheet_name,
      character(1L),
      release_type = artifact$release$release_type
    )
  )
  if (artifact$extension == "xlsx") {
    workbook_identity <- .validate_efu_workbook_identity(
      artifact$path,
      artifact$release
    )
    artifact$provenance$workbook_title <- workbook_identity$title
    artifact$provenance$workbook_published_date <-
      workbook_identity$published_date
    artifact$provenance$workbook_terms_statement <-
      workbook_identity$terms_statement
    artifact$provenance$workbook_license_url <-
      workbook_identity$license_url
  }
  cache_key <- .parsed_cache_key(artifact, "efu", parser_options)
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) {
    return(.finish_result(cached, artifact, "efu", parser_options))
  }
  if (artifact$extension == "csv") {
    data <- .read_normalized_csv(artifact$path)
    data <- data[data$dataset %in% dataset, , drop = FALSE]
  } else {
    data <- do.call(rbind, lapply(dataset, function(name) {
      sheet <- .efu_sheet_name(name, artifact$release$release_type)
      .parse_efu_sheet(
        artifact$path, sheet, name, artifact$release
      )
    }))
  }
  data <- .validate_efu(data, artifact$release)
  result <- .finish_result(
    data, artifact, "efu", parser_options
  )
  .cache_set(cache_key, data)
  result
}
