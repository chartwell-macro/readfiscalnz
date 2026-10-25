#!/usr/bin/env Rscript

# Guarded, offline-only latency/RSS harness. It is intentionally not run by
# tests or by the stage-3 evidence renderer.

truthy <- function(value) {
  tolower(trimws(as.character(value))) %in% c("1", "true", "yes", "on")
}

abort <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

if (!truthy(Sys.getenv("READFISCALNZ_RUN_QUIET_SLOT", unset = "false"))) {
  abort(
    paste0(
      "Quiet-slot benchmarking is disabled. Set ",
      "READFISCALNZ_RUN_QUIET_SLOT=true only in an approved quiet window."
    )
  )
}

required_packages <- c("readfiscalnz", "digest", "ps")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  abort(
    "Quiet-slot benchmarking requires: %s.",
    paste(missing_packages, collapse = ", ")
  )
}

input_dir <- Sys.getenv("READFISCALNZ_OFFICIAL_DIR", unset = "")
output_path <- Sys.getenv("READFISCALNZ_BENCHMARK_OUTPUT", unset = "")
if (!nzchar(input_dir) || !dir.exists(input_dir)) {
  abort("READFISCALNZ_OFFICIAL_DIR must contain the six pinned workbooks.")
}
if (!nzchar(output_path)) {
  abort("Set READFISCALNZ_BENCHMARK_OUTPUT to a private CSV path.")
}
summary_configured <- Sys.getenv(
  "READFISCALNZ_BENCHMARK_SUMMARY_OUTPUT",
  unset = ""
)
summary_path <- if (nzchar(summary_configured)) {
  summary_configured
} else {
  derived <- sub("\\.csv$", "-summary.csv", output_path, ignore.case = TRUE)
  if (identical(derived, output_path)) {
    paste0(output_path, "-summary.csv")
  } else {
    derived
  }
}
for (output_parent in unique(dirname(c(output_path, summary_path)))) {
  if (!dir.exists(output_parent) &&
      !dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)) {
    abort("Could not create the private benchmark output directory.")
  }
}
repetitions <- readfiscalnz:::.validate_benchmark_repetitions(Sys.getenv(
  "READFISCALNZ_BENCHMARK_REPETITIONS",
  unset = "5"
))

catalogue <- readfiscalnz::list_nz_fiscal_releases()
paths <- file.path(input_dir, catalogue$file_name)
hashes <- vapply(paths, function(path) {
  if (!file.exists(path)) {
    abort("Missing pinned workbook: %s.", basename(path))
  }
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}, character(1L))
if (!identical(unname(hashes), unname(catalogue$expected_sha256))) {
  abort("Pinned workbook hashes do not match the catalogue; benchmark refused.")
}
output_targets <- readfiscalnz:::.validate_benchmark_output_targets(
  output_path,
  summary_path,
  paths
)
output_path <- output_targets$raw
summary_path <- output_targets$summary
expected_rows <- c(
  `hfi-1972-2025` = 2799L,
  `tax-outturn-2026-05` = 29929L,
  `monthly-financials-2026-02` = 834L,
  `befu-2026` = 990L,
  `hyefu-2025` = 1018L,
  `prefu-2023` = 996L
)
expected_facts <- data.frame(
  release_id = catalogue$release_id,
  rows = unname(expected_rows[catalogue$release_id]),
  raw_sha256 = catalogue$expected_sha256,
  stringsAsFactors = FALSE
)
if (anyNA(expected_facts$rows)) {
  abort("Benchmark expected row counts do not cover the release catalogue.")
}

read_once <- function(release, path, cache_dir) {
  switch(
    release$family[[1L]],
    hfi = readfiscalnz::read_nz_hfi(
      release_id = release$release_id[[1L]],
      file = path,
      cache_dir = cache_dir
    ),
    tax_outturn = readfiscalnz::read_nz_tax_outturn(
      release_id = release$release_id[[1L]],
      file = path,
      cache_dir = cache_dir
    ),
    monthly_financials = readfiscalnz::read_nz_monthly_financials(
      release_id = release$release_id[[1L]],
      file = path,
      table = c(
        "performance", "comprehensive", "net_worth", "cash_flows",
        "cash_flow_reconciliation", "position"
      ),
      cache_dir = cache_dir
    ),
    efu = readfiscalnz::read_nz_efu(
      release_id = release$release_id[[1L]],
      update = release$release_type[[1L]],
      file = path,
      dataset = c("fiscal", "economic", "key_indicators"),
      cache_dir = cache_dir
    )
  )
}

process <- ps::ps_handle()
benchmark_cache_dirs <- character()
on.exit(unlink(benchmark_cache_dirs, recursive = TRUE), add = TRUE)
process_state <- paste(
  "single_r_process_package_loaded",
  "parsed_cache_reset_before_repetition",
  "measured_result_released_between_repetitions",
  "os_file_cache_uncontrolled",
  sep = ";"
)
rows <- list()
cursor <- 0L
for (index in seq_len(nrow(catalogue))) {
  release <- catalogue[index, , drop = FALSE]
  for (mode in c("cold", "warm")) {
    for (repetition in seq_len(repetitions)) {
      readfiscalnz:::.reset_parsed_cache()
      cache_dir <- tempfile("readfiscalnz-quiet-slot-")
      dir.create(cache_dir)
      benchmark_cache_dirs <- c(benchmark_cache_dirs, cache_dir)
      if (identical(mode, "warm")) {
        invisible(read_once(release, paths[[index]], cache_dir))
      }
      cache_state <- if (identical(mode, "cold")) {
        "disk_snapshot_absent;parsed_cache_cleared"
      } else {
        "disk_snapshot_preseeded;parsed_cache_primed"
      }
      gc()
      rss_before <- unname(ps::ps_memory_info(process)[["rss"]]) / 1024^2
      timing <- system.time({
        data <- read_once(release, paths[[index]], cache_dir)
      })
      rss_after <- unname(ps::ps_memory_info(process)[["rss"]]) / 1024^2
      provenance <- readfiscalnz::nz_fiscal_provenance(data)
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        release_id = release$release_id[[1L]],
        mode = mode,
        repetition = repetition,
        elapsed_seconds = unname(timing[["elapsed"]]),
        rss_before_mib = rss_before,
        rss_after_mib = rss_after,
        rss_delta_mib = rss_after - rss_before,
        rows = nrow(data),
        raw_sha256 = provenance$raw_sha256,
        cache_state = cache_state,
        process_state = process_state,
        stringsAsFactors = FALSE
      )
      data <- NULL
      gc(FALSE)
    }
  }
}
evidence <- do.call(rbind, rows)
readfiscalnz:::.validate_benchmark_evidence(
  evidence,
  expected_facts = expected_facts,
  minimum_repetitions = repetitions
)
summary <- readfiscalnz:::.summarise_benchmark_evidence(evidence)
utils::write.csv(evidence, output_path, row.names = FALSE)
utils::write.csv(summary, summary_path, row.names = FALSE)
print(summary, row.names = FALSE)
cat(sprintf(
  "Wrote %s guarded offline measurements to %s and summary to %s.\n",
  nrow(evidence),
  output_path,
  summary_path
))
