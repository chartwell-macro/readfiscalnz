test_that("static release catalogue is explicit and byte pinned", {
  releases <- list_nz_fiscal_releases()

  expect_equal(nrow(releases), 6L)
  expect_setequal(
    releases$release_type,
    c("HFI", "tax_outturn", "monthly", "BEFU", "HYEFU", "PREFU")
  )
  expect_true(all(nchar(releases$expected_sha256) == 64L))
  expect_true(all(releases$terms_license == "CC BY 4.0"))
  expect_true(all(grepl("^https://www\\.treasury\\.govt\\.nz/", releases$url)))
  expect_equal(anyDuplicated(releases$release_id), 0L)
})

test_that("saved official-page shapes verify catalogue metadata", {
  cases <- list(
    c("hfi-1972-2025", "hfi-release.html"),
    c("tax-outturn-2026-05", "tax-release.html"),
    c("monthly-financials-2026-02", "monthly-release.html"),
    c("befu-2026", "efu-release.html"),
    c("hyefu-2025", "hyefu-release.html"),
    c("prefu-2023", "prefu-release.html")
  )

  for (case in cases) {
    observed <- list_nz_fiscal_releases(
      release_id = case[[1L]],
      page_html = fixture_path("html", case[[2L]])
    )
    expect_true(observed$metadata_matches)
  }
})

test_that("catalogue filters and terms fail closed", {
  expect_equal(
    list_nz_fiscal_releases(family = "efu", status = "current")$release_id,
    "befu-2026"
  )
  expect_error(list_nz_fiscal_releases(family = "unknown"), "Unsupported")
  expect_error(list_nz_fiscal_releases(release_id = "missing"), "No release")
  expect_error(nz_fiscal_terms("missing"), "Unknown")

  terms <- nz_fiscal_terms("tax-outturn-2026-05")
  expect_equal(
    terms$terms_statement,
    "\u00a9 Crown Copyright, Attribution 4.0 International (CC BY 4.0)"
  )
  expect_equal(terms$terms_license, "CC BY 4.0")
  expect_equal(terms$terms_attribution, "The Treasury")
  expect_equal(
    terms$license_url,
    "https://creativecommons.org/licenses/by/4.0/"
  )
  expect_s3_class(terms$terms_verified_at, "Date")
})

test_that("live transport requires an explicit terms decision before network", {
  expect_error(
    read_nz_hfi(cache_dir = new_cache(), accept_terms = FALSE),
    "accept_terms = TRUE"
  )
})
