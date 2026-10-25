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
    .parse_english_date(sub("^Published ", "", published_text))
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
#' Reads the stable time-series surfaces in the official Budget Economic and
#' Fiscal Update (BEFU), Half Year Economic and Fiscal Update (HYEFU), and
#' Pre-election Economic and Fiscal Update (PREFU) chart-data workbooks. This
#' reader is deliberately limited to the named fiscal, economic, and
#' key-indicator time-series surfaces; it does not attempt to extract every
#' table, figure, narrative, or supplementary workbook tab.
#'
#' Forecast values retain both their publication vintage and the source status
#' label. Generic `% of GDP` rows inherit their parent metric when constructing
#' `series`, so a level and its percentage-of-GDP counterpart remain distinct
#' series identities. Unit context is read from the source labels and is
#' retained alongside the numeric value. Supported units are `"NZD billion"`,
#' `"NZD million"`, `"percentage points"`, `"percent"`, and `"index"`.
#'
#' A local XLSX workbook or canonical CSV can be supplied for an offline,
#' reproducible replay. Local artifacts are signature-checked, copied to a
#' hash-addressed raw cache, and parsed through the same validation path as a
#' downloaded artifact; supplying `file` never performs network I/O. Without
#' a local file, retrieval is HTTPS-only from the official Treasury host and
#' requires `accept_terms = TRUE` after reviewing [nz_fiscal_terms()].
#'
#' @details
#' The catalogue currently supports the exact EFU release identifiers
#' `"befu-2026"`, `"hyefu-2025"`, and `"prefu-2023"`. Use
#' [list_nz_fiscal_releases()] with `family = "efu"` to inspect release
#' status, dates, forecast metadata, official URLs, and expected hashes. With
#' `release_id = NULL`, `revision = "as_published"` selects the newest current
#' catalogue row and falls back to the newest issue date when the family has no
#' current row; `revision = "latest"` always selects the newest issue date. An
#' explicit `release_id` always selects that release; the selected revision
#' policy is recorded in provenance.
#'
#' `dataset` routes to the following workbook sheets: `"fiscal"` to
#' `"Fiscal Time Series"`, `"economic"` to `"Economic Time Series"`, and
#' `"key_indicators"` to `"Table 1"` for BEFU and HYEFU. PREFU uses its
#' distinct `"Executive Summary Table"` sheet for `"key_indicators"`. One or
#' more routes may be requested. XLSX rows are emitted in the requested
#' dataset order and then in source-sheet order; canonical CSV input retains
#' its input row order after filtering. The function does not sort
#' observations.
#'
#' Each EFU observation represents a June fiscal year: `period_end` is June 30
#' of `year` and `fiscal_year_basis` is `"June year"`. The parser maps source
#' status labels containing `actual` or `unaudited` to `actual`, and labels
#' containing `forecast` or `estimate` to `forecast`.
#' The original `source_status_label` remains unchanged. Values remain in the
#' source scale; `unit_multiplier` records the corresponding numeric multiplier
#' (`1e9` for `"NZD billion"`, `1e6` for `"NZD million"`, and `1` for the
#' other supported units). No interpolation, currency conversion, or forecast
#' revision is performed.
#'
#' Parsed results use an in-memory parsed-result cache keyed by the raw
#' SHA-256, release, revision policy, parser version, schema version, and
#' parser options. Raw snapshots are written below `cache_dir`, whose default
#' is `.nz_cache_dir()`, normally
#' `tools::R_user_dir("readfiscalnz", "cache")/v1`; package cache limits are
#' controlled by the `readfiscalnz.*` options. Reading a local file can create
#' files below `cache_dir` and update cache timestamps, but does not modify the
#' input artifact. Live retrieval is opt-in, never occurs on package load, and
#' is HTTPS-only, size-bounded, signature-checked, SHA-256-checked, and
#' retried only within the package's bounded transport policy.
#'
#' The parser fails closed when a release, route, local artifact, workbook
#' identity, required sheet, year/status label, unit context, canonical column
#' set, or release metadata does not match the supported contract. Common
#' errors include an unknown `release_id`, an `update` value inconsistent with
#' the selected release, an unsupported `dataset`, a missing or malformed local
#' file, a terms refusal for live retrieval, and official transport, size, or
#' hash failures. No fallback workbook or guessed unit is substituted.
#'
#' @param release_id A character scalar identifying an exact catalogue release;
#'   supported values are `"befu-2026"`, `"hyefu-2025"`, and `"prefu-2023"`.
#'   The default, `NULL`, selects a release according to `revision`. Use
#'   [list_nz_fiscal_releases()] to inspect the catalogue before selecting a
#'   release.
#' @param update An optional character scalar requiring the selected release
#'   type to be `"BEFU"`, `"HYEFU"`, or `"PREFU"`. The default, `NULL`, does
#'   not add a release-type constraint. Values are matched case-insensitively
#'   after conversion to upper case; a mismatch with `release_id` is an error.
#' @param file An optional path to one existing local `.xlsx` or canonical
#'   `.csv` artifact. The default, `NULL`, resolves the selected official
#'   Treasury artifact over HTTPS. Local files are copied into the raw cache,
#'   validated, and never downloaded or changed. A local CSV must have the
#'   canonical EFU columns listed in the Value section below.
#' @param revision A character scalar matched against
#'   `c("as_published", "latest")`; the default is
#'   `c("as_published", "latest")` with the first value selected. With no
#'   explicit `release_id`, `"as_published"` selects the newest current
#'   catalogue row and falls back to the newest issue date when the family has
#'   no current row; `"latest"` always selects the newest issue date.
#' @param dataset A character vector of one or more supported routes. The
#'   default is `"fiscal"`; other values are `"economic"` and
#'   `"key_indicators"`. Unsupported route names and absent required sheets
#'   are errors.
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
#'   supplied, apart from input validation. Connection and retry bounds are
#'   additionally enforced by the transport layer.
#' @param max_bytes A positive numeric scalar maximum accepted artifact size
#'   in bytes. The default is `25 * 1024^2` (26,214,400 bytes). Local and live
#'   artifacts outside this bound are rejected before parsing.
#' @return A data frame with class `c("nz_fiscal_data", "data.frame")`, one
#'   row per numeric EFU observation. Columns are returned in this exact order:
#'   `release_type`, `forecast_vintage`, `forecast_horizon`, `dataset`,
#'   `sheet`, `year`, `period_end`, `fiscal_year_basis`, `actual_forecast`,
#'   `source_status_label`, `accounting_basis`, `consolidation_scope`,
#'   `section`, `series`, `value`, `unit`, `unit_multiplier`, and
#'   `unit_context`. `year` is integer, `period_end` is a `Date`, `value` and
#'   `unit_multiplier` are numeric, and all other columns are character.
#'   `release_type`, `forecast_vintage`, and `forecast_horizon` identify the
#'   selected publication; `dataset` and `sheet` follow the route mapping in
#'   Details; `actual_forecast` is exactly `"actual"` or `"forecast"`;
#'   `fiscal_year_basis` is exactly `"June year"`; and `period_end` equals
#'   `as.Date(sprintf("%04d-06-30", year))`. Unit labels and multipliers are
#'   internally consistent, and the source status, accounting basis,
#'   consolidation scope, unit context, and release metadata are validated.
#'
#'   The result has an `nz_fiscal_provenance` attribute containing a named list
#'   with release and source metadata: `provider`, `family`, `release_id`,
#'   `release_type`, `release_status`, `document_date`, `issue_date`,
#'   `fiscal_year`, `fiscal_year_basis`, `accounting_basis`,
#'   `consolidation_scope`, `actual_forecast`, `forecast_vintage`,
#'   `forecast_horizon`, `landing_url`, `original_url`, `resolved_url`,
#'   `original_file`, `source_kind`, `source_location`, `raw_snapshot_path`,
#'   `raw_size_bytes`, `raw_sha256`, `catalogue_sha256`, `retrieved_at`,
#'   `terms_statement`, `terms_license`, `terms_attribution`, `license_url`,
#'   `terms_url`, `terms_verified_at`, `revision_policy`, and `revision_note`,
#'   plus `parser_version`, `schema_version`, and `parser_options`. For EFU,
#'   the version values are `"efu-2.0.0"` and `"efu-long-3"`; workbook input
#'   also records validated workbook title, publication date, and terms fields
#'   in provenance. Use [nz_fiscal_provenance()] to inspect the complete list.
#'   The attribute is retained by downstream
#'   [nz_fiscal_series_catalogue()] and [nz_fiscal_series_history()] calls.
#' @seealso [list_nz_fiscal_releases()], [nz_fiscal_terms()],
#'   [nz_fiscal_provenance()], [nz_fiscal_raw_bytes()],
#'   [nz_fiscal_series_catalogue()], [nz_fiscal_series_history()],
#'   [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()]
#' @family fiscal-readers
#' @examples
#' efu_file <- system.file("extdata", "efu.csv", package = "readfiscalnz")
#' efu <- read_nz_efu(
#'   release_id = "befu-2026",
#'   file = efu_file,
#'   dataset = "fiscal",
#'   cache_dir = tempfile("readfiscalnz-efu-cache-")
#' )
#' stopifnot(
#'   inherits(efu, "nz_fiscal_data"),
#'   inherits(efu$period_end, "Date"),
#'   is.integer(efu$year),
#'   is.numeric(efu$value),
#'   identical(names(efu), c(
#'     "release_type", "forecast_vintage", "forecast_horizon", "dataset",
#'     "sheet", "year", "period_end", "fiscal_year_basis",
#'     "actual_forecast", "source_status_label", "accounting_basis",
#'     "consolidation_scope", "section", "series", "value", "unit",
#'     "unit_multiplier", "unit_context"
#'   )),
#'   identical(nz_fiscal_provenance(efu)$schema_version, "efu-long-3")
#' )
#' efu[c("period_end", "series", "actual_forecast", "value", "unit")]
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
