test_that("English source dates parse without relying on LC_TIME", {
  expect_identical(
    readfiscalnz:::.parse_english_date(c(
      "28 May 2026",
      "9 December 2025",
      "12 Sep 2023"
    )),
    as.Date(c("2026-05-28", "2025-12-09", "2023-09-12"))
  )
  expect_true(is.na(readfiscalnz:::.parse_english_date("31 February 2026")))
  expect_true(is.na(readfiscalnz:::.parse_english_date("28 mai 2026")))

  expect_identical(
    readfiscalnz:::.excel_date(c(
      "28 May 2026",
      "12 Sep 2023",
      "2026-07-02",
      "02/07/2026"
    )),
    as.Date(c("2026-05-28", "2023-09-12", "2026-07-02", "2026-07-02"))
  )

  monthly_header <- data.frame(
    one = c("STATEMENT", "For the period ended 28 February 2026"),
    two = c("", ""),
    stringsAsFactors = FALSE
  )
  expect_identical(
    readfiscalnz:::.monthly_report_date(monthly_header),
    as.Date("2026-02-28")
  )
})

test_that("series identity numbers are independent of decimal display locale", {
  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)

  expect_identical(
    readfiscalnz:::.nz_series_canonical_value(1.25),
    "1.25"
  )

  source <- fixture_path("xlsx", "hfi-regimes.xlsx")
  comma <- read_nz_hfi(
    file = source,
    cache_dir = new_cache(),
    sheets = "Spending"
  )
  comma_ids <- nz_fiscal_series_catalogue(comma)$series_id

  options(OutDec = ".")
  point <- read_nz_hfi(
    file = source,
    cache_dir = new_cache(),
    sheets = "Spending"
  )
  point_ids <- nz_fiscal_series_catalogue(point)$series_id

  expect_identical(comma_ids, point_ids)
})
