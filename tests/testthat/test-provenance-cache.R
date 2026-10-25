test_that("local input is copied to an atomic content-addressed snapshot", {
  cache <- new_cache()
  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  out <- read_nz_hfi(file = source, cache_dir = cache, sheets = "Spending")
  provenance <- nz_fiscal_provenance(out)

  expect_true(file.exists(provenance$raw_snapshot_path))
  expect_match(
    basename(provenance$raw_snapshot_path),
    paste0("^", provenance$raw_sha256, "\\.xlsx$")
  )
  expect_equal(
    digest::digest(
      nz_fiscal_raw_bytes(out),
      algo = "sha256",
      serialize = FALSE
    ),
    provenance$raw_sha256
  )
  expect_equal(
    readBin(source, "raw", n = file.info(source)$size),
    readBin(
      provenance$raw_snapshot_path,
      "raw",
      n = file.info(provenance$raw_snapshot_path)$size
    )
  )
  expect_equal(provenance$source_kind, "local_file")
  expect_equal(provenance$parser_version, "hfi-2.0.0")
  expect_equal(provenance$schema_version, "hfi-long-2")
})

test_that("raw-byte reads and artifact sizes are bounded", {
  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  out <- read_nz_hfi(
    file = source,
    cache_dir = new_cache(),
    sheets = "Spending"
  )
  expect_error(nz_fiscal_raw_bytes(out, max_bytes = 1), "max_bytes")
  expect_error(
    read_nz_hfi(
      file = source,
      cache_dir = new_cache(),
      max_bytes = 1
    ),
    "outside"
  )
})

test_that("parsed cache uses bounded LRU eviction", {
  old <- options(readfiscalnz.parsed_cache_entries = 2L)
  on.exit(options(old), add = TRUE)
  state <- readfiscalnz:::.nz_state
  if (length(ls(state$parsed_cache))) {
    rm(list = ls(state$parsed_cache), envir = state$parsed_cache)
  }
  state$cache_order <- character()

  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  cache <- new_cache()
  read_nz_hfi(file = source, cache_dir = cache, sheets = "Spending")
  read_nz_hfi(file = source, cache_dir = cache, sheets = "Nominal GDP")
  read_nz_hfi(file = source, cache_dir = cache, sheets = "Debt, Net Worth")

  expect_lte(length(ls(state$parsed_cache)), 2L)
  expect_lte(length(state$cache_order), 2L)
})

test_that("raw snapshot cache enforces file, byte, and age bounds", {
  old <- options(
    readfiscalnz.raw_cache_files = 2L,
    readfiscalnz.raw_cache_bytes = 100,
    readfiscalnz.raw_cache_max_age_days = Inf
  )
  on.exit(options(old), add = TRUE)
  cache <- new_cache()
  sources <- character()
  snapshots <- character()
  for (index in 1:3) {
    source <- tempfile(fileext = ".csv")
    writeBin(charToRaw(paste0("series,value\nx,", index, "\n")), source)
    sources <- c(sources, source)
    hash <- digest::digest(
      source, algo = "sha256", file = TRUE, serialize = FALSE
    )
    snapshots <- c(
      snapshots,
      readfiscalnz:::.archive_snapshot(source, hash, "csv", cache)
    )
  }
  expect_lte(length(readfiscalnz:::.raw_cache_files(cache)), 2L)

  options(
    readfiscalnz.raw_cache_files = 10L,
    readfiscalnz.raw_cache_bytes = 25
  )
  fourth <- tempfile(fileext = ".csv")
  writeBin(charToRaw("series,value\ny,4444\n"), fourth)
  fourth_hash <- digest::digest(
    fourth, algo = "sha256", file = TRUE, serialize = FALSE
  )
  fourth_snapshot <- readfiscalnz:::.archive_snapshot(
    fourth, fourth_hash, "csv", cache
  )
  remaining <- readfiscalnz:::.raw_cache_files(cache)
  expect_lte(sum(file.info(remaining)$size), 25)
  expect_true(file.exists(fourth_snapshot))

  Sys.setFileTime(fourth_snapshot, Sys.time() - 3 * 86400)
  result <- prune_nz_fiscal_cache(
    cache,
    max_files = 10L,
    max_bytes = 1000,
    max_age_days = 1
  )
  expect_false(file.exists(fourth_snapshot))
  expect_lte(result$files, 10L)
  expect_lte(result$bytes, 1000)
})

test_that("corrupt raw targets self-heal from verified local bytes", {
  cache <- new_cache()
  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  first <- read_nz_hfi(file = source, cache_dir = cache, sheets = "Spending")
  path <- nz_fiscal_provenance(first)$raw_snapshot_path
  writeBin(charToRaw("corrupt"), path)

  expect_error(
    nz_fiscal_raw_bytes(first),
    "no longer matches provenance"
  )
  repaired <- read_nz_hfi(
    file = source,
    cache_dir = cache,
    sheets = "Spending"
  )
  provenance <- nz_fiscal_provenance(repaired)
  expect_identical(
    digest::digest(
      provenance$raw_snapshot_path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    provenance$raw_sha256
  )
})

test_that("atomic publish verifies and replaces a corrupt race winner", {
  directory <- new_cache()
  staged <- file.path(directory, "staged.part")
  target <- file.path(directory, "target.csv")
  bytes <- charToRaw("verified bytes")
  writeBin(bytes, staged)
  expected <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  attempts <- 0L
  racing_rename <- function(from, to) {
    attempts <<- attempts + 1L
    if (attempts == 1L) {
      writeBin(charToRaw("competing corruption"), to)
      return(FALSE)
    }
    base::file.rename(from, to)
  }

  readfiscalnz:::.publish_snapshot(
    staged, target, expected, rename_fn = racing_rename
  )
  expect_equal(attempts, 2L)
  expect_identical(
    digest::digest(
      target, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    expected
  )
})

test_that("atomic publish accepts a verified second race winner", {
  directory <- new_cache()
  staged <- file.path(directory, "staged.part")
  target <- file.path(directory, "target.csv")
  bytes <- charToRaw("verified bytes")
  writeBin(bytes, staged)
  expected <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  attempts <- 0L
  racing_rename <- function(from, to) {
    attempts <<- attempts + 1L
    if (attempts == 1L) {
      writeBin(charToRaw("competing corruption"), to)
    } else {
      writeBin(bytes, to)
    }
    FALSE
  }

  readfiscalnz:::.publish_snapshot(
    staged, target, expected, rename_fn = racing_rename
  )
  expect_equal(attempts, 2L)
  expect_identical(
    digest::digest(
      target, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    expected
  )
})

test_that("parsed cache identity includes revision, parser, and schema", {
  state <- readfiscalnz:::.nz_state
  if (length(ls(state$parsed_cache))) {
    rm(list = ls(state$parsed_cache), envir = state$parsed_cache)
  }
  state$cache_order <- character()
  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  cache <- new_cache()
  first <- read_nz_hfi(
    file = source,
    revision = "as_published",
    cache_dir = cache,
    sheets = "Spending"
  )
  second <- read_nz_hfi(
    file = source,
    revision = "latest",
    cache_dir = cache,
    sheets = "Spending"
  )
  expect_equal(
    nz_fiscal_provenance(first)$revision_policy,
    "as_published"
  )
  expect_equal(nz_fiscal_provenance(second)$revision_policy, "latest")
  expect_equal(length(ls(state$parsed_cache)), 2L)

  artifact <- list(provenance = list(
    raw_sha256 = nz_fiscal_provenance(first)$raw_sha256,
    release_id = nz_fiscal_provenance(first)$release_id,
    revision_policy = "as_published"
  ))
  key_v1 <- readfiscalnz:::.parsed_cache_key(
    artifact, "hfi", list(sheets = "Spending"),
    parser_version = "hfi-1", schema_version = "schema-1"
  )
  key_parser_v2 <- readfiscalnz:::.parsed_cache_key(
    artifact, "hfi", list(sheets = "Spending"),
    parser_version = "hfi-2", schema_version = "schema-1"
  )
  key_schema_v2 <- readfiscalnz:::.parsed_cache_key(
    artifact, "hfi", list(sheets = "Spending"),
    parser_version = "hfi-1", schema_version = "schema-2"
  )
  expect_false(identical(key_v1, key_parser_v2))
  expect_false(identical(key_v1, key_schema_v2))
})

test_that("parsed cache refreshes per-call source provenance", {
  state <- readfiscalnz:::.nz_state
  if (length(ls(state$parsed_cache))) {
    rm(list = ls(state$parsed_cache), envir = state$parsed_cache)
  }
  state$cache_order <- character()
  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  copy <- tempfile(fileext = ".xlsx")
  file.copy(source, copy)
  cache <- new_cache()

  read_nz_hfi(file = source, cache_dir = cache, sheets = "Spending")
  second <- read_nz_hfi(
    file = copy, cache_dir = cache, sheets = "Spending"
  )
  expect_equal(
    nz_fiscal_provenance(second)$source_location,
    normalizePath(copy, winslash = "/", mustWork = TRUE)
  )
  expect_equal(length(ls(state$parsed_cache)), 1L)
})
