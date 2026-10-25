test_that("all supported consolidated monthly statements parse explicitly", {
  tables <- c(
    "performance", "comprehensive", "net_worth", "cash_flows",
    "cash_flow_reconciliation", "position"
  )
  out <- read_nz_monthly_financials(
    file = fixture_path("xlsx", "monthly-statements.xlsx"),
    table = tables,
    cache_dir = new_cache()
  )

  expect_equal(nrow(out), 36L)
  expect_setequal(unique(out$table), tables)
  expect_equal(unique(out$consolidation_scope), "whole-of-government consolidated")
  expect_equal(unique(out$accounting_basis), "accrual PBE Standards")
  expect_setequal(
    unique(out$actual_forecast),
    c("actual", "forecast", "derived")
  )
  expect_true(all(out$period_end <= as.Date("2026-06-30")))
})

test_that("monthly canonical CSV preserves scenarios", {
  out <- read_nz_monthly_financials(
    file = fixture_path("expected", "monthly.csv"),
    cache_dir = new_cache()
  )

  expect_equal(nrow(out), 4L)
  expect_equal(
    out$observation,
    c(
      "prior_year_actual",
      "current_year_to_date_actual",
      "current_year_to_date_forecast",
      "annual_forecast"
    )
  )
})

test_that("monthly reader rejects arbitrary workbook tabs", {
  expect_error(
    read_nz_monthly_financials(
      file = fixture_path("xlsx", "monthly-statements.xlsx"),
      table = "fiscal_indicator_reconciliation",
      cache_dir = new_cache()
    ),
    "Unsupported monthly table"
  )
})
