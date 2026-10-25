fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

new_cache <- function() {
  path <- tempfile("readfiscalnz-test-cache-")
  dir.create(path)
  path
}
