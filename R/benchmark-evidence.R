.validate_benchmark_evidence <- function(
    data,
    expected_facts,
    minimum_repetitions = 3L) {
  required <- c(
    "release_id", "mode", "repetition", "elapsed_seconds",
    "rss_before_mib", "rss_after_mib", "rss_delta_mib",
    "rows", "raw_sha256", "cache_state", "process_state"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    .abort(
      "Benchmark evidence is missing column(s): %s.",
      paste(missing, collapse = ", ")
    )
  }
  expected_columns <- c("release_id", "rows", "raw_sha256")
  if (!is.data.frame(expected_facts) ||
      !identical(names(expected_facts), expected_columns) ||
      !nrow(expected_facts) ||
      anyNA(expected_facts) ||
      any(!nzchar(expected_facts$release_id)) ||
      anyDuplicated(expected_facts$release_id) ||
      any(expected_facts$rows < 1L) ||
      any(expected_facts$rows != as.integer(expected_facts$rows)) ||
      any(!grepl("^[a-f0-9]{64}$", expected_facts$raw_sha256))) {
    .abort(
      paste0(
        "expected_facts must contain unique release IDs, positive row ",
        "counts, and lowercase SHA-256 values."
      )
    )
  }
  expected_releases <- expected_facts$release_id
  minimum_repetitions <- .validate_benchmark_repetitions(
    minimum_repetitions
  )
  if (!identical(
      sort(unique(data$release_id)),
      sort(expected_releases))) {
    .abort("Benchmark evidence releases do not match the expected releases.")
  }
  if (!identical(sort(unique(data$mode)), c("cold", "warm"))) {
    .abort("Benchmark evidence must contain exactly cold and warm modes.")
  }
  key <- paste(data$release_id, data$mode, data$repetition, sep = "\r")
  if (anyDuplicated(key)) {
    .abort(
      "Benchmark evidence contains duplicate release/mode/repetition rows."
    )
  }
  expected_groups <- as.vector(outer(
    expected_releases,
    c("cold", "warm"),
    paste,
    sep = "\r"
  ))
  group <- paste(data$release_id, data$mode, sep = "\r")
  counts <- table(factor(group, levels = expected_groups))
  if (any(counts < minimum_repetitions)) {
    .abort(
      "Every benchmark release must have at least three repetitions per mode."
    )
  }
  if (any(counts > 20L)) {
    .abort(
      "Every benchmark release must have at most 20 repetitions per mode."
    )
  }
  if (!is.numeric(data$repetition) || anyNA(data$repetition) ||
      any(!is.finite(data$repetition)) ||
      any(data$repetition != as.integer(data$repetition)) ||
      any(data$repetition < 1L) || any(data$repetition > 20L)) {
    .abort("Benchmark repetition IDs must be integers from 1 to 20.")
  }
  repetitions <- split(data$repetition, group)
  if (any(vapply(repetitions, function(value) {
    !identical(sort(unique(as.integer(value))), seq_len(length(value)))
  }, logical(1)))) {
    .abort(
      paste0(
        "Benchmark repetitions must be consecutive within each release ",
        "and mode."
      )
    )
  }
  state_is_stable <- function(column) {
    states <- split(data[[column]], group)
    all(vapply(states, function(value) {
      length(unique(value)) == 1L && !is.na(value[[1L]]) &&
        nzchar(value[[1L]])
    }, logical(1)))
  }
  if (!state_is_stable("cache_state") ||
      !state_is_stable("process_state")) {
    .abort(
      paste0(
        "Cache and process state must be explicit and stable within each ",
        "release/mode."
      )
    )
  }
  numeric_columns <- c(
    "elapsed_seconds", "rss_before_mib", "rss_after_mib",
    "rss_delta_mib", "rows"
  )
  if (any(vapply(data[numeric_columns], function(value) {
    !is.numeric(value) || anyNA(value) || any(!is.finite(value))
  }, logical(1)))) {
    .abort("Benchmark measurements must be finite numeric values.")
  }
  matched <- match(data$release_id, expected_facts$release_id)
  if (!identical(
      as.integer(data$rows),
      as.integer(expected_facts$rows[matched]))) {
    .abort(
      "Each release must retain its expected row count across both modes."
    )
  }
  if (!identical(
      as.character(data$raw_sha256),
      as.character(expected_facts$raw_sha256[matched]))) {
    .abort(
      "Each release must retain its expected raw SHA-256 across both modes."
    )
  }
  invisible(data)
}

.summarise_benchmark_evidence <- function(data) {
  groups <- split(
    data,
    interaction(
      data$release_id,
      data$mode,
      drop = TRUE,
      lex.order = TRUE
    )
  )
  summary <- lapply(groups, function(group) {
    data.frame(
      release_id = group$release_id[[1L]],
      mode = group$mode[[1L]],
      repetitions = nrow(group),
      cache_state = group$cache_state[[1L]],
      process_state = group$process_state[[1L]],
      latency_min_seconds = min(group$elapsed_seconds),
      latency_median_seconds = stats::median(group$elapsed_seconds),
      latency_max_seconds = max(group$elapsed_seconds),
      rss_delta_min_mib = min(group$rss_delta_mib),
      rss_delta_median_mib = stats::median(group$rss_delta_mib),
      rss_delta_max_mib = max(group$rss_delta_mib),
      observed_peak_rss_mib = max(
        group$rss_before_mib,
        group$rss_after_mib
      ),
      rows = unique(group$rows)[[1L]],
      raw_sha256 = unique(group$raw_sha256)[[1L]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, summary)
  rownames(out) <- NULL
  out[order(out$release_id, out$mode), , drop = FALSE]
}

.validate_benchmark_repetitions <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1L || is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      numeric_value != as.integer(numeric_value) ||
      numeric_value < 3L || numeric_value > 20L) {
    .abort("Benchmark repetitions must be an integer from 3 to 20.")
  }
  as.integer(numeric_value)
}

.canonical_benchmark_output_target <- function(path, label) {
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(trimws(path))) {
    .abort("%s must be one non-empty path.", label)
  }
  expanded <- path.expand(path)
  link <- Sys.readlink(expanded)
  if (length(link) == 1L && !is.na(link) && nzchar(link)) {
    .abort("%s must not be a symbolic link.", label)
  }
  if (dir.exists(expanded)) {
    .abort("%s must identify a file, not a directory.", label)
  }
  parent <- dirname(expanded)
  if (!dir.exists(parent)) {
    .abort("%s parent directory does not exist.", label)
  }
  canonical_parent <- normalizePath(
    parent,
    winslash = "/",
    mustWork = TRUE
  )
  candidate <- file.path(canonical_parent, basename(expanded))
  if (file.exists(expanded)) {
    normalizePath(expanded, winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(candidate, winslash = "/", mustWork = FALSE)
  }
}

.validate_benchmark_output_targets <- function(
    raw_output,
    summary_output,
    input_paths) {
  raw_target <- .canonical_benchmark_output_target(
    raw_output,
    "Raw output"
  )
  summary_target <- .canonical_benchmark_output_target(
    summary_output,
    "Summary output"
  )
  compare_path <- function(path) {
    if (Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) {
      tolower(path)
    } else {
      path
    }
  }
  if (identical(compare_path(raw_target), compare_path(summary_target))) {
    .abort("Raw and summary outputs must not alias the same file.")
  }
  if (!is.character(input_paths) || !length(input_paths) ||
      anyNA(input_paths) || any(!file.exists(input_paths))) {
    .abort("Official benchmark inputs must identify existing files.")
  }
  input_targets <- normalizePath(
    input_paths,
    winslash = "/",
    mustWork = TRUE
  )
  if (any(compare_path(c(raw_target, summary_target)) %in%
          compare_path(input_targets))) {
    .abort("Benchmark outputs must not overwrite an official input file.")
  }
  if (file.exists(raw_output) || file.exists(summary_output)) {
    .abort(
      paste0(
        "Benchmark outputs must not already exist; use fresh private output ",
        "paths."
      )
    )
  }
  list(raw = raw_target, summary = summary_target)
}
