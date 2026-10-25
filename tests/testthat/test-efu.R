test_that("BEFU and HYEFU retain actual/forecast vintage semantics", {
  cases <- list(
    c("befu-2026", "BEFU", "befu.xlsx"),
    c("hyefu-2025", "HYEFU", "hyefu.xlsx")
  )

  for (case in cases) {
    out <- read_nz_efu(
      release_id = case[[1L]],
      update = case[[2L]],
      file = fixture_path("xlsx", case[[3L]]),
      dataset = c("fiscal", "economic", "key_indicators"),
      cache_dir = new_cache()
    )
    expect_setequal(
      unique(out$dataset),
      c("fiscal", "economic", "key_indicators")
    )
    expect_setequal(unique(out$actual_forecast), c("actual", "forecast"))
    expect_equal(unique(out$release_type), case[[2L]])
    expect_true(all(nzchar(out$forecast_vintage)))
    expect_true(all(nzchar(out$forecast_horizon)))
    expect_true(all(nzchar(out$unit)))
    expect_true(all(out$unit_multiplier > 0))
    expect_true(all(nzchar(out$unit_context)))
    expect_setequal(
      unique(out$unit),
      c("NZD million", "percent", "NZD billion")
    )
    expect_equal(nz_fiscal_provenance(out)$parser_version, "efu-2.0.0")
    expect_equal(nz_fiscal_provenance(out)$schema_version, "efu-long-3")
  }
})

test_that("PREFU uses its distinct executive-summary sheet contract", {
  out <- read_nz_efu(
    release_id = "prefu-2023",
    update = "PREFU",
    file = fixture_path("xlsx", "prefu.xlsx"),
    dataset = c("fiscal", "economic", "key_indicators"),
    cache_dir = new_cache()
  )

  key <- out[out$dataset == "key_indicators", , drop = FALSE]
  expect_true(all(key$sheet == "Executive Summary Table"))
  expect_true(all(key$actual_forecast == "forecast"))
  expect_true(any(key$source_status_label == "forecast1"))
  expect_true(
    "OBEGALx ($billions) (% of GDP)" %in% key$series
  )
})

test_that("EFU canonical CSV and route checks fail closed", {
  out <- read_nz_efu(
    release_id = "befu-2026",
    file = fixture_path("expected", "efu.csv"),
    cache_dir = new_cache()
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$actual_forecast, c("actual", "forecast"))
  expect_true(all(out$unit == "NZD million"))
  expect_true(all(out$unit_multiplier == 1e6))

  expect_error(
    read_nz_efu(
      release_id = "befu-2026",
      update = "PREFU",
      file = fixture_path("xlsx", "befu.xlsx"),
      cache_dir = new_cache()
    ),
    "not PREFU"
  )
  expect_error(
    read_nz_efu(
      release_id = "befu-2026",
      file = fixture_path("xlsx", "befu.xlsx"),
      dataset = "figure_1",
      cache_dir = new_cache()
    ),
    "Unsupported EFU dataset"
  )
})

test_that("EFU canonical replay rejects ambiguous unit metadata", {
  fixture <- utils::read.csv(
    fixture_path("expected", "efu.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fixture$unit <- ""
  path <- tempfile(fileext = ".csv")
  utils::write.csv(fixture, path, row.names = FALSE, na = "")

  expect_error(
    read_nz_efu(
      release_id = "befu-2026",
      file = path,
      cache_dir = new_cache()
    ),
    "unit"
  )
  expect_error(
    readfiscalnz:::.efu_unit_spec("mystery source scale"),
    "no explicit unit context"
  )
})
