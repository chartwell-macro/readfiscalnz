test_that("HFI parser preserves every documented regime boundary", {
  out <- read_nz_hfi(
    file = fixture_path("xlsx", "hfi-regimes.xlsx"),
    cache_dir = new_cache()
  )

  expect_equal(nrow(out), 120L)
  expect_setequal(unique(out$measure), c("level", "share_of_gdp"))
  expect_setequal(unique(out$unit), c("NZD million", "percent of GDP"))
  expect_equal(nz_fiscal_provenance(out)$parser_version, "hfi-2.0.0")
  expect_equal(nz_fiscal_provenance(out)$schema_version, "hfi-long-2")
  expect_setequal(
    unique(out$regime_id),
    c(
      "cash_march_gst_exclusive",
      "cash_march_gst_inclusive",
      "cash_june_gst_inclusive",
      "gaap_nzifrs_old_gaap_revenue_expense",
      "gaap_nzifrs",
      "gaap_pbe"
    )
  )

  spending <- out[
    out$section == "Spending" & out$measure == "level",
    ,
    drop = FALSE
  ]
  by_year <- setNames(spending$regime_id, spending$year)
  expect_equal(by_year[["1986"]], "cash_march_gst_exclusive")
  expect_equal(by_year[["1987"]], "cash_march_gst_inclusive")
  expect_equal(by_year[["1989"]], "cash_march_gst_inclusive")
  expect_equal(by_year[["1990"]], "cash_june_gst_inclusive")
  expect_equal(by_year[["1993"]], "cash_june_gst_inclusive")
  expect_equal(by_year[["1994"]], "gaap_nzifrs_old_gaap_revenue_expense")
  expect_equal(by_year[["1996"]], "gaap_nzifrs_old_gaap_revenue_expense")
  expect_equal(by_year[["1997"]], "gaap_nzifrs")
  expect_equal(by_year[["2004"]], "gaap_nzifrs")
  expect_equal(by_year[["2005"]], "gaap_pbe")
  expect_equal(
    unique(spending$fiscal_year_basis[spending$year == 1989]),
    "March year"
  )
  expect_equal(
    unique(spending$fiscal_year_basis[spending$year == 1990]),
    "June year"
  )
  expect_equal(
    unique(spending$restatement_status[spending$year == 2019]),
    "restated_at_2020_for_new_pbe_standards"
  )
  expect_equal(
    unique(spending$restatement_status[spending$year == 2020]),
    "first_reporting_year_under_new_pbe_standards"
  )
  debt_1994 <- out[
    out$section == "Debt, Net Worth" &
      out$measure == "level" &
      out$year == 1994,
    ,
    drop = FALSE
  ]
  expect_equal(debt_1994$regime_id, "gaap_nzifrs")
  expect_equal(debt_1994$comparability_note, "none recorded for this series/year")

  spending_share <- out[
    out$section == "Spending" &
      out$measure == "share_of_gdp" &
      out$year == 1986,
    ,
    drop = FALSE
  ]
  expect_equal(spending_share$value, 10.1)
})

test_that("HFI canonical CSV is a supported replay surface", {
  out <- read_nz_hfi(
    file = fixture_path("expected", "hfi.csv"),
    cache_dir = new_cache()
  )

  expect_equal(nrow(out), 5L)
  expect_s3_class(out, "nz_fiscal_data")
  expect_s3_class(out$period_end, "Date")
  expect_equal(out$value, c(101, 102, 104, 106, 110))
})

test_that("HFI parser rejects unrecognised routes and layouts", {
  expect_error(
    read_nz_hfi(
      file = fixture_path("xlsx", "hfi-regimes.xlsx"),
      sheets = "Summary",
      cache_dir = new_cache()
    ),
    "Unsupported HFI sheet"
  )
  expect_error(
    read_nz_hfi(
      file = fixture_path("xlsx", "monthly-statements.xlsx"),
      cache_dir = new_cache()
    ),
    "required sheet"
  )
})
