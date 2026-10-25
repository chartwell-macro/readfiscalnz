.nz_state <- new.env(parent = emptyenv())
.nz_state$parsed_cache <- new.env(parent = emptyenv())
.nz_state$cache_order <- character()

.nz_parser_versions <- c(
  hfi = "hfi-2.0.0",
  tax_outturn = "tax-1.1.0",
  monthly_financials = "monthly-1.0.0",
  efu = "efu-2.0.0"
)

.nz_schema_versions <- c(
  hfi = "hfi-long-2",
  tax_outturn = "tax-long-2",
  monthly_financials = "monthly-long-1",
  efu = "efu-long-3"
)

.nz_cache_dir <- function() {
  getOption(
    "readfiscalnz.cache_dir",
    file.path(tools::R_user_dir("readfiscalnz", "cache"), "v1")
  )
}

.nz_cache_max <- function() {
  value <- getOption("readfiscalnz.parsed_cache_entries", 16L)
  value <- suppressWarnings(as.integer(value))
  if (is.na(value) || value < 1L) 16L else value
}

.nz_raw_cache_limits <- function() {
  max_files <- suppressWarnings(as.integer(getOption(
    "readfiscalnz.raw_cache_files", 64L
  )))
  max_bytes <- suppressWarnings(as.numeric(getOption(
    "readfiscalnz.raw_cache_bytes", 250 * 1024^2
  )))
  max_age_days <- suppressWarnings(as.numeric(getOption(
    "readfiscalnz.raw_cache_max_age_days", 180
  )))
  if (is.na(max_files) || max_files < 1L) max_files <- 64L
  if (is.na(max_bytes) || max_bytes < 1) max_bytes <- 250 * 1024^2
  if (is.na(max_age_days) || max_age_days < 0) max_age_days <- 180
  list(
    max_files = max_files,
    max_bytes = max_bytes,
    max_age_days = max_age_days
  )
}

.nz_download_attempts <- function() {
  value <- suppressWarnings(as.integer(getOption(
    "readfiscalnz.download_attempts", 3L
  )))
  if (is.na(value) || value < 1L || value > 3L) 3L else value
}

.parsed_cache_key <- function(
    artifact,
    parser_family,
    parser_options,
    parser_version = unname(.nz_parser_versions[[parser_family]]),
    schema_version = unname(.nz_schema_versions[[parser_family]])) {
  digest::digest(
    list(
      raw_sha256 = artifact$provenance$raw_sha256,
      release_id = artifact$provenance$release_id,
      revision_policy = artifact$provenance$revision_policy,
      parser_family = parser_family,
      parser_version = parser_version,
      schema_version = schema_version,
      parser_options = parser_options
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

.cache_get <- function(key) {
  if (!exists(key, envir = .nz_state$parsed_cache, inherits = FALSE)) {
    return(NULL)
  }
  .nz_state$cache_order <- c(
    setdiff(.nz_state$cache_order, key),
    key
  )
  get(key, envir = .nz_state$parsed_cache, inherits = FALSE)
}

.cache_set <- function(key, value) {
  assign(key, value, envir = .nz_state$parsed_cache)
  .nz_state$cache_order <- c(
    setdiff(.nz_state$cache_order, key),
    key
  )
  while (length(.nz_state$cache_order) > .nz_cache_max()) {
    victim <- .nz_state$cache_order[[1L]]
    rm(list = victim, envir = .nz_state$parsed_cache)
    .nz_state$cache_order <- .nz_state$cache_order[-1L]
  }
  value
}

.reset_parsed_cache <- function() {
  keys <- ls(envir = .nz_state$parsed_cache, all.names = TRUE)
  if (length(keys)) {
    rm(list = keys, envir = .nz_state$parsed_cache)
  }
  .nz_state$cache_order <- character()
  invisible(NULL)
}

.abort <- function(..., call. = FALSE) {
  stop(sprintf(...), call. = call.)
}

.clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.as_number <- function(x) {
  x <- .clean_text(x)
  negative <- grepl("^\\(.*\\)$", x)
  x <- gsub("^\\(|\\)$", "", x)
  x <- gsub("[,$%]", "", x)
  x[x %in% c("", "-", "\u2013", "\u2014", "n/a", "N/A")] <- NA_character_
  out <- suppressWarnings(as.numeric(x))
  out[negative & !is.na(out)] <- -out[negative & !is.na(out)]
  out
}

.excel_date <- function(x) {
  x <- .clean_text(x)
  serial <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA_character_, length(x)))
  numeric_date <- !is.na(serial) & serial > 20000 & serial < 100000
  result[numeric_date] <- as.Date(serial[numeric_date], origin = "1899-12-30")
  text_date <- !numeric_date & nzchar(x)
  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%d %B %Y", "%d %b %Y")) {
    parsed <- as.Date(x[text_date], format = fmt)
    fill <- is.na(result[text_date]) & !is.na(parsed)
    idx <- which(text_date)[fill]
    result[idx] <- parsed[fill]
  }
  result
}

.require_columns <- function(x, required, family) {
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    .abort(
      "%s schema mismatch: missing column(s): %s",
      family,
      paste(missing, collapse = ", ")
    )
  }
  invisible(x)
}

.require_exact_columns <- function(x, required, family) {
  .require_columns(x, required, family)
  unexpected <- setdiff(names(x), required)
  if (length(unexpected)) {
    .abort(
      "%s schema mismatch: unexpected column(s): %s",
      family,
      paste(unexpected, collapse = ", ")
    )
  }
  if (!identical(names(x), required)) {
    .abort("%s schema mismatch: canonical column order changed.", family)
  }
  invisible(x)
}

.read_sheet_text <- function(path, sheet) {
  sheets <- readxl::excel_sheets(path)
  if (!sheet %in% sheets) {
    .abort(
      "Workbook schema mismatch: required sheet '%s' is absent. Found: %s",
      sheet,
      paste(sheets, collapse = ", ")
    )
  }
  x <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    trim_ws = TRUE,
    .name_repair = "minimal"
  ))
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

.read_normalized_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}
