test_that("transient HTTP statuses retry within a hard attempt cap", {
  staged <- tempfile(fileext = ".part")
  attempts <- 0L
  fetch <- function(url, path, handle) {
    attempts <<- attempts + 1L
    writeBin(charToRaw(paste0("attempt-", attempts)), path)
    list(status_code = if (attempts < 3L) 503L else 200L)
  }
  readfiscalnz:::.download_to_staged(
    "https://www.treasury.govt.nz/file.xlsx",
    staged,
    timeout_sec = 2,
    max_bytes = 1024,
    max_attempts = 3L,
    fetch_fn = fetch
  )
  expect_equal(attempts, 3L)
  expect_equal(readChar(staged, nchars = file.info(staged)$size), "attempt-3")
})

test_that("non-retryable and partial failures remove staged bytes", {
  staged <- tempfile(fileext = ".part")
  attempts <- 0L
  not_found <- function(url, path, handle) {
    attempts <<- attempts + 1L
    writeBin(charToRaw("not found"), path)
    list(status_code = 404L)
  }
  expect_error(
    readfiscalnz:::.download_to_staged(
      "https://www.treasury.govt.nz/missing.xlsx",
      staged,
      timeout_sec = 2,
      max_bytes = 1024,
      max_attempts = 3L,
      fetch_fn = not_found
    ),
    "HTTP 404"
  )
  expect_equal(attempts, 1L)
  expect_false(file.exists(staged))

  partial <- function(url, path, handle) {
    writeBin(charToRaw("PKpartial"), path)
    stop("connection reset")
  }
  expect_error(
    readfiscalnz:::.download_to_staged(
      "https://www.treasury.govt.nz/truncated.xlsx",
      staged,
      timeout_sec = 2,
      max_bytes = 1024,
      max_attempts = 2L,
      fetch_fn = partial
    ),
    "after 2 attempt"
  )
  expect_false(file.exists(staged))
  expect_error(
    readfiscalnz:::.download_to_staged(
      "http://www.treasury.govt.nz/file.xlsx",
      staged,
      timeout_sec = 2,
      max_bytes = 1024,
      fetch_fn = not_found
    ),
    "must use HTTPS"
  )
})

test_that("a successful but truncated body fails the pinned hash", {
  release <- readfiscalnz:::.nz_catalogue()[1L, , drop = FALSE]
  truncated <- function(url, staged, timeout_sec, max_bytes) {
    writeBin(charToRaw("PKtruncated"), staged)
    invisible(NULL)
  }
  testthat::local_mocked_bindings(
    .download_to_staged = truncated,
    .package = "readfiscalnz"
  )
  expect_error(
    readfiscalnz:::.download_release(
      release,
      cache_dir = new_cache(),
      timeout_sec = 2,
      max_bytes = 1024,
      accept_terms = TRUE
    ),
    "SHA-256 drifted"
  )
})
