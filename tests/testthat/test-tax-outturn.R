test_that("tax parser keeps cash, accrual, consolidation, and writeoff semantics", {
  out <- read_nz_tax_outturn(
    file = fixture_path("xlsx", "tax-writeoff-break.xlsx"),
    cache_dir = new_cache()
  )

  expect_equal(nrow(out), 30L)
  expect_equal(unique(out$accounting_basis[out$measure == "revenue"]), "accrual")
  expect_equal(unique(out$accounting_basis[out$measure == "receipts"]), "cash")
  expect_setequal(
    unique(out$consolidation_scope),
    c(
      "unconsolidated",
      "consolidation elimination",
      "core Crown consolidated",
      "whole-of-government consolidated"
    )
  )
  expect_true(all(
    out$consolidation_scope[out$series == "CORE CROWN TAX"] ==
      "core Crown consolidated"
  ))
  expect_true(all(
    out$consolidation_scope[out$series == "FULLY CONSOLIDATED TAX"] ==
      "whole-of-government consolidated"
  ))
  eliminations <- grepl("^Section III:", out$section) &
    !out$series %in% c("CORE CROWN TAX", "FULLY CONSOLIDATED TAX")
  expect_true(all(
    out$consolidation_scope[eliminations] == "consolidation elimination"
  ))

  total_revenue <- out[
    out$measure == "revenue" & out$series == "Total tax",
    ,
    drop = FALSE
  ]
  expect_equal(
    total_revenue$writeoff_regime,
    c(
      "net_of_tax_writeoffs",
      "gross_of_tax_writeoffs",
      "gross_of_tax_writeoffs"
    )
  )
  expect_equal(total_revenue$fiscal_year, c("2006/07", "2006/07", "2007/08"))
})

test_that("tax measure selection and canonical CSV are deterministic", {
  receipts <- read_nz_tax_outturn(
    file = fixture_path("xlsx", "tax-writeoff-break.xlsx"),
    measure = "receipts",
    cache_dir = new_cache()
  )
  expect_true(all(receipts$measure == "receipts"))
  expect_equal(nrow(receipts), 15L)

  replay <- read_nz_tax_outturn(
    file = fixture_path("expected", "tax.csv"),
    cache_dir = new_cache()
  )
  expect_equal(nrow(replay), 4L)
  expect_s3_class(replay$date, "Date")
})

test_that("tax parser fails closed on the wrong workbook", {
  expect_error(
    read_nz_tax_outturn(
      file = fixture_path("xlsx", "hfi-regimes.xlsx"),
      cache_dir = new_cache()
    ),
    "required sheet"
  )
  expect_error(
    readfiscalnz:::.tax_consolidation_scope(
      "Section III: Eliminations ($million)",
      "NEW CONSOLIDATED TAX TOTAL"
    ),
    "Unknown Section III consolidation label"
  )
})
