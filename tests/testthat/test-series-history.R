test_that("HFI histories retain point annotations without fragmenting series", {
  out <- read_nz_hfi(
    file = fixture_path("xlsx", "hfi-regimes.xlsx"),
    cache_dir = new_cache()
  )
  catalogue <- nz_fiscal_series_catalogue(out)
  pbe <- catalogue[
    catalogue$series == "Core Crown expenses" &
      catalogue$unit == "NZD million" &
      catalogue$start_date == as.Date("2005-06-30"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(pbe), 1L)
  expect_equal(pbe$end_date, as.Date("2020-06-30"))
  expect_equal(pbe$observations, 3L)
  history <- nz_fiscal_series_history(out, pbe$series_id)
  expect_equal(history$year, c(2005L, 2019L, 2020L))
  expect_setequal(
    history$restatement_status,
    c(
      "as_published",
      "restated_at_2020_for_new_pbe_standards",
      "first_reporting_year_under_new_pbe_standards"
    )
  )
})

test_that("EFU source labels remain point metadata in continuous status series", {
  out <- read_nz_efu(
    release_id = "prefu-2023",
    update = "PREFU",
    file = fixture_path("xlsx", "prefu.xlsx"),
    dataset = "key_indicators",
    cache_dir = new_cache()
  )
  catalogue <- nz_fiscal_series_catalogue(out)
  forecast <- catalogue[
    catalogue$series == "OBEGALx ($billions)",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(forecast), 1L)
  expect_equal(forecast$observations, 3L)
  history <- nz_fiscal_series_history(out, forecast$series_id)
  expect_equal(history$year, c(2025L, 2026L, 2027L))
  expect_setequal(history$source_status_label, c("forecast1", "forecast"))
})

test_that("snapshot and historical families retain their intended cardinality", {
  monthly <- read_nz_monthly_financials(
    file = fixture_path("xlsx", "monthly-statements.xlsx"),
    table = "performance",
    cache_dir = new_cache()
  )
  monthly_catalogue <- nz_fiscal_series_catalogue(monthly)
  expect_true(all(monthly_catalogue$observations == 1L))

  tax <- read_nz_tax_outturn(
    file = fixture_path("xlsx", "tax-writeoff-break.xlsx"),
    cache_dir = new_cache()
  )
  tax_catalogue <- nz_fiscal_series_catalogue(tax)
  receipts <- vapply(
    tax_catalogue$identity_payload,
    function(payload) identical(payload$measure, "receipts"),
    logical(1L)
  )
  expect_true(all(tax_catalogue$observations[receipts] == 3L))
  expect_setequal(
    tax_catalogue$observations[!receipts],
    c(1L, 2L)
  )
})

test_that("series replay rejects malformed identities", {
  out <- read_nz_hfi(
    file = fixture_path("xlsx", "hfi-regimes.xlsx"),
    cache_dir = new_cache()
  )
  expect_error(
    nz_fiscal_series_history(out, "not-a-series-id"),
    "SHA-256"
  )
  expect_error(
    nz_fiscal_series_history(out, strrep("f", 64L)),
    "absent"
  )

  duplicate <- rbind(out[1L, , drop = FALSE], out[1L, , drop = FALSE])
  attr(duplicate, "nz_fiscal_provenance") <- nz_fiscal_provenance(out)
  class(duplicate) <- unique(c("nz_fiscal_data", class(duplicate)))
  expect_error(
    nz_fiscal_series_catalogue(duplicate),
    "duplicate dates"
  )
})
