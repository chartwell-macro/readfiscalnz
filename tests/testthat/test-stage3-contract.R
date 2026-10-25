write_stage3_csv <- function(name, mutate) {
  data <- utils::read.csv(
    fixture_path("expected", name),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  data <- mutate(data)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data, path, row.names = FALSE, na = "")
  path
}

test_that("EFU sibling workbooks cannot be labelled as another release", {
  expect_error(
    read_nz_efu(
      release_id = "hyefu-2025",
      update = "HYEFU",
      file = fixture_path("xlsx", "befu.xlsx"),
      cache_dir = new_cache()
    ),
    "workbook release identity"
  )
  expect_error(
    read_nz_efu(
      release_id = "prefu-2023",
      update = "PREFU",
      file = fixture_path("xlsx", "hyefu.xlsx"),
      cache_dir = new_cache()
    ),
    "workbook release identity"
  )
})

test_that("successful redirects must remain on the official Treasury host", {
  staged <- tempfile(fileext = ".part")
  fetch <- function(url, path, handle) {
    writeBin(charToRaw("PK official-looking bytes"), path)
    list(
      status_code = 200L,
      url = "https://downloads.example.invalid/redirected.xlsx"
    )
  }
  expect_error(
    readfiscalnz:::.download_to_staged(
      "https://www.treasury.govt.nz/file.xlsx",
      staged,
      timeout_sec = 2,
      max_bytes = 1024,
      fetch_fn = fetch
    ),
    "redirected outside"
  )
  expect_false(file.exists(staged))
})

test_that("live download results preserve the verified final URL", {
  bytes <- charToRaw("PK retained official bytes")
  release <- readfiscalnz:::.nz_catalogue()[1L, , drop = FALSE]
  release$expected_sha256 <- digest::digest(
    bytes,
    algo = "sha256",
    serialize = FALSE
  )
  final_url <-
    "https://www.treasury.govt.nz/sites/default/files/final.xlsx"
  download <- function(url, staged, timeout_sec, max_bytes) {
    writeBin(bytes, staged)
    list(status_code = 200L, url = final_url)
  }
  testthat::local_mocked_bindings(
    .download_to_staged = download,
    .package = "readfiscalnz"
  )

  result <- readfiscalnz:::.download_release(
    release,
    cache_dir = new_cache(),
    timeout_sec = 2,
    max_bytes = 1024,
    accept_terms = TRUE
  )
  expect_type(result, "list")
  expect_true(file.exists(result$path))
  expect_equal(result$resolved_url, final_url)
})

test_that("saved landing-page status, date, and licence drift fail loudly", {
  html <- readLines(fixture_path("html", "hfi-release.html"), warn = FALSE)
  html <- sub("Status: Current", "Status: Superseded", html, fixed = TRUE)
  expect_error(
    list_nz_fiscal_releases(
      release_id = "hfi-1972-2025",
      page_html = html
    ),
    "release metadata drift"
  )

  html <- readLines(fixture_path("html", "tax-release.html"), warn = FALSE)
  html <- sub("CC BY 4.0", "CC BY 3.0", html, fixed = TRUE)
  html <- sub(
    "Attribution 4.0 International",
    "Attribution 3.0 New Zealand",
    html,
    fixed = TRUE
  )
  expect_error(
    list_nz_fiscal_releases(
      release_id = "tax-outturn-2026-05",
      page_html = html
    ),
    "release metadata drift"
  )
})

test_that("canonical replay rejects column and semantic drift", {
  hfi_extra <- write_stage3_csv("hfi.csv", function(data) {
    data$unexpected <- "drift"
    data
  })
  expect_error(
    read_nz_hfi(file = hfi_extra, cache_dir = new_cache()),
    "unexpected column"
  )

  hfi_unit <- write_stage3_csv("hfi.csv", function(data) {
    data$unit[[1L]] <- "NZD"
    data
  })
  expect_error(
    read_nz_hfi(file = hfi_unit, cache_dir = new_cache()),
    "unit"
  )

  tax_scope <- write_stage3_csv("tax.csv", function(data) {
    data$consolidation_scope[[1L]] <- "whole-of-government consolidated"
    data
  })
  expect_error(
    read_nz_tax_outturn(file = tax_scope, cache_dir = new_cache()),
    "consolidation"
  )

  monthly_status <- write_stage3_csv("monthly.csv", function(data) {
    data$actual_forecast[[1L]] <- "forecast"
    data
  })
  expect_error(
    read_nz_monthly_financials(
      file = monthly_status,
      table = "performance",
      cache_dir = new_cache()
    ),
    "status"
  )

  efu_status <- write_stage3_csv("efu.csv", function(data) {
    data$source_status_label[[1L]] <- "provisional"
    data
  })
  expect_error(
    read_nz_efu(
      release_id = "befu-2026",
      file = efu_status,
      cache_dir = new_cache()
    ),
    "status"
  )

  efu_release <- write_stage3_csv("efu.csv", function(data) {
    data$release_type <- "HYEFU"
    data
  })
  expect_error(
    read_nz_efu(
      release_id = "befu-2026",
      file = efu_release,
      cache_dir = new_cache()
    ),
    "release"
  )
})

test_that("raw snapshot paths are scoped to release identity", {
  file <- fixture_path("xlsx", "befu.xlsx")
  cache <- new_cache()
  befu <- readfiscalnz:::.resolve_artifact(
    "efu",
    "befu-2026",
    file,
    "as_published",
    cache,
    timeout_sec = 2,
    max_bytes = 1024^2,
    accept_terms = FALSE
  )
  hyefu <- readfiscalnz:::.resolve_artifact(
    "efu",
    "hyefu-2025",
    file,
    "as_published",
    cache,
    timeout_sec = 2,
    max_bytes = 1024^2,
    accept_terms = FALSE
  )
  expect_false(identical(befu$path, hyefu$path))
  expect_match(basename(dirname(dirname(befu$path))), "befu-2026")
  expect_match(basename(dirname(dirname(hyefu$path))), "hyefu-2025")
})

test_that("all six release cache identities are pairwise disjoint", {
  releases <- readfiscalnz:::.nz_catalogue()
  keys <- vapply(seq_len(nrow(releases)), function(index) {
    release <- releases[index, , drop = FALSE]
    artifact <- list(provenance = list(
      raw_sha256 = strrep("a", 64L),
      release_id = release$release_id,
      revision_policy = "as_published"
    ))
    readfiscalnz:::.parsed_cache_key(
      artifact,
      release$family,
      parser_options = list(route = release$release_type)
    )
  }, character(1L))
  expect_equal(anyDuplicated(keys), 0L)
})

test_that("benchmark evidence requires three cold and warm reads per release", {
  releases <- readfiscalnz:::.nz_catalogue()$release_id
  facts <- data.frame(
    release_id = releases,
    rows = seq_along(releases) + 9L,
    raw_sha256 = vapply(
      seq_along(releases),
      function(index) strrep(as.character(index), 64L),
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  evidence <- expand.grid(
    release_id = releases,
    mode = c("cold", "warm"),
    repetition = seq_len(3L),
    stringsAsFactors = FALSE
  )
  evidence$elapsed_seconds <- seq_len(nrow(evidence)) / 100
  evidence$rss_before_mib <- 100
  evidence$rss_after_mib <- 101
  evidence$rss_delta_mib <- 1
  evidence$rows <- facts$rows[match(evidence$release_id, facts$release_id)]
  evidence$raw_sha256 <- facts$raw_sha256[
    match(evidence$release_id, facts$release_id)
  ]
  evidence$cache_state <- ifelse(
    evidence$mode == "cold",
    "disk_snapshot_absent;parsed_cache_cleared",
    "disk_snapshot_preseeded;parsed_cache_primed"
  )
  evidence$process_state <- paste(
    "single_r_process_package_loaded",
    "parsed_cache_reset_before_repetition",
    "measured_result_released_between_repetitions",
    "os_file_cache_uncontrolled",
    sep = ";"
  )

  expect_invisible(readfiscalnz:::.validate_benchmark_evidence(
    evidence,
    expected_facts = facts
  ))
  summary <- readfiscalnz:::.summarise_benchmark_evidence(evidence)
  expect_equal(nrow(summary), length(releases) * 2L)
  expect_true(all(summary$repetitions == 3L))
  expect_true(all(c(
    "latency_min_seconds",
    "latency_median_seconds",
    "latency_max_seconds",
    "rss_delta_min_mib",
    "rss_delta_median_mib",
    "rss_delta_max_mib",
    "observed_peak_rss_mib"
  ) %in% names(summary)))

  missing_warm <- evidence[
    !(evidence$release_id == releases[[1L]] &
        evidence$mode == "warm" &
        evidence$repetition == 3L),
  ]
  expect_error(
    readfiscalnz:::.validate_benchmark_evidence(
      missing_warm,
      expected_facts = facts
    ),
    "at least three repetitions per mode"
  )

  duplicate <- rbind(evidence, evidence[1L, , drop = FALSE])
  expect_error(
    readfiscalnz:::.validate_benchmark_evidence(
      duplicate,
      expected_facts = facts
    ),
    "duplicate release/mode/repetition"
  )

  excessive_id <- evidence
  excessive_id$repetition[[1L]] <- 21L
  expect_error(
    readfiscalnz:::.validate_benchmark_evidence(
      excessive_id,
      expected_facts = facts
    ),
    "integers from 1 to 20"
  )

  excessive_groups <- expand.grid(
    release_id = releases,
    mode = c("cold", "warm"),
    repetition = seq_len(21L),
    stringsAsFactors = FALSE
  )
  excessive_groups <- merge(
    excessive_groups,
    evidence[
      !duplicated(evidence[c("release_id", "mode")]),
      setdiff(names(evidence), "repetition"),
      drop = FALSE
    ],
    by = c("release_id", "mode"),
    sort = FALSE
  )
  expect_error(
    readfiscalnz:::.validate_benchmark_evidence(
      excessive_groups,
      expected_facts = facts
    ),
    "at most 20 repetitions per mode"
  )

  unlike_rows <- evidence
  unlike_rows$rows[[1L]] <- 11
  expect_error(
    readfiscalnz:::.validate_benchmark_evidence(
      unlike_rows,
      expected_facts = facts
    ),
    "expected row count"
  )

  hash_drift <- evidence
  hash_drift$raw_sha256[hash_drift$release_id == releases[[1L]]] <-
    strrep("f", 64L)
  expect_error(
    readfiscalnz:::.validate_benchmark_evidence(
      hash_drift,
      expected_facts = facts
    ),
    "expected raw SHA-256"
  )
})

test_that("benchmark repetition parsing rejects lossy and non-finite values", {
  expect_identical(
    readfiscalnz:::.validate_benchmark_repetitions("3"),
    3L
  )
  expect_identical(
    readfiscalnz:::.validate_benchmark_repetitions("20"),
    20L
  )
  for (value in c("3.5", "Inf", "-Inf", "NaN", "2", "21", "not-a-number")) {
    expect_error(
      readfiscalnz:::.validate_benchmark_repetitions(value),
      "integer from 3 to 20",
      info = value
    )
  }
})

test_that("benchmark cache reset clears objects and recency state", {
  state <- readfiscalnz:::.nz_state
  assign("benchmark-sentinel", 1, envir = state$parsed_cache)
  state$cache_order <- "benchmark-sentinel"
  expect_invisible(readfiscalnz:::.reset_parsed_cache())
  expect_length(ls(envir = state$parsed_cache, all.names = TRUE), 0L)
  expect_length(state$cache_order, 0L)
})

test_that("benchmark outputs cannot alias or overwrite official inputs", {
  root <- tempfile("readfiscalnz-output-guard-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  inputs <- file.path(root, c("one.xlsx", "two.xlsx"))
  writeBin(charToRaw("one"), inputs[[1L]])
  writeBin(charToRaw("two"), inputs[[2L]])
  raw <- file.path(root, "raw.csv")
  summary <- file.path(root, "summary.csv")

  targets <- readfiscalnz:::.validate_benchmark_output_targets(
    raw,
    summary,
    inputs
  )
  expect_identical(
    targets$raw,
    file.path(normalizePath(root, winslash = "/"), "raw.csv")
  )
  expect_identical(
    targets$summary,
    file.path(normalizePath(root, winslash = "/"), "summary.csv")
  )
  expect_error(
    readfiscalnz:::.validate_benchmark_output_targets(raw, raw, inputs),
    "must not alias"
  )
  expect_error(
    readfiscalnz:::.validate_benchmark_output_targets(
      inputs[[1L]],
      summary,
      inputs
    ),
    "must not overwrite"
  )

  symlink <- file.path(root, "raw-link.csv")
  expect_true(file.symlink(summary, symlink))
  expect_error(
    readfiscalnz:::.validate_benchmark_output_targets(
      symlink,
      summary,
      inputs
    ),
    "must not be a symbolic link"
  )

  hard_link_to_input <- file.path(root, "raw-input-hard-link.csv")
  expect_true(file.link(inputs[[1L]], hard_link_to_input))
  expect_error(
    readfiscalnz:::.validate_benchmark_output_targets(
      hard_link_to_input,
      summary,
      inputs
    ),
    "must not already exist"
  )

  existing_raw <- file.path(root, "existing-raw.csv")
  writeBin(charToRaw("evidence"), existing_raw)
  hard_link_outputs <- file.path(root, "summary-output-hard-link.csv")
  expect_true(file.link(existing_raw, hard_link_outputs))
  expect_error(
    readfiscalnz:::.validate_benchmark_output_targets(
      existing_raw,
      hard_link_outputs,
      inputs
    ),
    "must not already exist"
  )
})

test_that("committed performance evidence validates and recomputes exactly", {
  review_dir <- system.file("stage3_review", package = "readfiscalnz")
  raw_path <- file.path(
    review_dir,
    "artifacts",
    "quiet_slot_performance_raw.csv"
  )
  summary_path <- file.path(
    review_dir,
    "artifacts",
    "quiet_slot_performance_summary.csv"
  )
  raw <- utils::read.csv(
    raw_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  summary <- utils::read.csv(
    summary_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  replay <- utils::read.csv(
    file.path(review_dir, "artifacts", "official_replay.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  readme <- readLines(file.path(review_dir, "README.md"), warn = FALSE)
  ledger <- utils::read.csv(
    file.path(review_dir, "stage3_gate_ledger.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expected_facts <- data.frame(
    release_id = replay$release_id,
    rows = replay$rows,
    raw_sha256 = replay$workbook_sha256,
    stringsAsFactors = FALSE
  )

  expect_identical(
    digest::digest(raw_path, algo = "sha256", file = TRUE),
    "9cdbfc2da1413a531a4112bb7c83058991220ee506198c6a6b54c20fbfd26362"
  )
  expect_identical(
    digest::digest(summary_path, algo = "sha256", file = TRUE),
    "e245997dadbbe014976e69a08d6962de9c6b3dca917c49a173ecdc7bae5b8212"
  )
  expect_equal(nrow(raw), 60L)
  expect_equal(nrow(summary), 12L)
  expect_equal(
    unname(table(raw$release_id, raw$mode)),
    matrix(5L, nrow = 6L, ncol = 2L)
  )
  expect_invisible(readfiscalnz:::.validate_benchmark_evidence(
    raw,
    expected_facts = expected_facts,
    minimum_repetitions = 5L
  ))
  recomputed <- readfiscalnz:::.summarise_benchmark_evidence(raw)
  expect_identical(summary, recomputed)
  expect_identical(
    unique(raw$cache_state[raw$mode == "cold"]),
    "disk_snapshot_absent;parsed_cache_cleared"
  )
  expect_identical(
    unique(raw$cache_state[raw$mode == "warm"]),
    "disk_snapshot_preseeded;parsed_cache_primed"
  )
  expect_identical(
    unique(raw$process_state),
    paste(
      "single_r_process_package_loaded",
      "parsed_cache_reset_before_repetition",
      "measured_result_released_between_repetitions",
      "os_file_cache_uncontrolled",
      sep = ";"
    )
  )
  expect_equal(max(raw$elapsed_seconds), 1.12)
  expect_equal(max(raw$rss_before_mib, raw$rss_after_mib), 578.125)
  tax_cold <- raw[
    raw$release_id == "tax-outturn-2026-05" &
      raw$mode == "cold",
    ,
    drop = FALSE
  ]
  expect_equal(max(tax_cold$rss_delta_mib), 222.828125)
  expect_equal(416 + max(tax_cold$rss_delta_mib), 638.828125)
  evidence_text <- c(
    readLines(raw_path, warn = FALSE),
    readLines(summary_path, warn = FALSE)
  )
  expect_false(any(grepl(
    "/private/|/Users/|readfiscalnz-benchmark-run|tmp/",
    evidence_text
  )))

  expect_true(any(grepl("PASS_WITH_MEMORY_WARNING", readme, fixed = TRUE)))
  expect_false(any(grepl("PERFORMANCE_NOT_RERUN", readme, fixed = TRUE)))
  expect_identical(
    ledger$status[ledger$gate == "latency_rss"],
    "pass_with_memory_warning"
  )
  expect_identical(
    ledger$status[ledger$gate == "stage3_overall"],
    "pass_with_memory_warning"
  )
})
