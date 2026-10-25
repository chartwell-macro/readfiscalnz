# Deterministic synthetic workbooks model the supported Treasury sheet shapes.
# They contain no copied Treasury data and never access the network.

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("Install writexl to regenerate fixtures.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
package_dir <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else normalizePath(".", mustWork = TRUE)
fixture_dir <- file.path(package_dir, "tests", "testthat", "fixtures", "xlsx")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

as_sheet <- function(rows, columns = max(lengths(rows))) {
  matrix <- matrix(NA, nrow = length(rows), ncol = columns)
  for (index in seq_along(rows)) {
    matrix[index, seq_along(rows[[index]])] <- rows[[index]]
  }
  as.data.frame(matrix, stringsAsFactors = FALSE)
}

hfi_years <- c(1986, 1987, 1989, 1990, 1993, 1994, 1996, 1997, 2004, 2005, 2019, 2020)
hfi_year_labels <- ifelse(
  hfi_years == 1987,
  "1987\u2020",
  ifelse(hfi_years == 1994, "1994*", as.character(hfi_years))
)
hfi_sheet <- function(name, series, offset) {
  level_rows <- lapply(seq_along(hfi_years), function(index) {
    c(
      if (index == 1L) "Treasury regime labels omitted in fixture" else NA,
      hfi_year_labels[[index]],
      offset + index
    )
  })
  share_rows <- lapply(seq_along(hfi_years), function(index) {
    c(
      if (index == 1L) "Treasury regime labels omitted in fixture" else NA,
      hfi_year_labels[[index]],
      (offset + index) / 1000
    )
  })
  as_sheet(c(
    list(
      c(name),
      c(NA),
      c("$ millions", NA, name),
      c(NA, NA, series)
    ),
    level_rows,
    list(
      c(NA),
      c("% GDP", NA, name),
      c(NA, NA, series)
    ),
    share_rows
  ), columns = 3L)
}
hfi <- list(
  Summary = as_sheet(list(c("Synthetic fixture")), 1L),
  `Series Descriptions` = as_sheet(list(c("Synthetic fixture")), 1L),
  Sources = as_sheet(list(c("Synthetic fixture")), 1L),
  Spending = hfi_sheet("Spending", "Core Crown expenses", 100),
  `Revenue, Surplus measures` = hfi_sheet("Tax and Revenue", "Core Crown tax revenue", 200),
  `Debt, Net Worth` = hfi_sheet("Debt", "Net core Crown debt", 300),
  `NZS Fund series` = hfi_sheet("NZS Fund", "NZS Fund net worth", 400),
  `Nominal GDP` = hfi_sheet("GDP", "Nominal GDP", 500)
)
writexl::write_xlsx(
  hfi,
  file.path(fixture_dir, "hfi-regimes.xlsx"),
  col_names = FALSE
)

excel_serial <- function(date) as.numeric(as.Date(date) - as.Date("1899-12-30"))
tax_dates <- excel_serial(c("2007-05-01", "2007-06-01", "2007-07-01"))
tax_sheet <- function(measure, values) {
  as_sheet(list(
    c(NA, tax_dates),
    c(paste(measure, "Data")),
    c(NA),
    c(paste0("Section I: Actual ", tolower(measure), " ($million)")),
    c("Total tax", values),
    c("Section III: Eliminations ($million)"),
    c("GST on departmental expenses", values / 10),
    c("CORE CROWN TAX", values - 1),
    c("FULLY CONSOLIDATED TAX", values - 2),
    c("Section IV: Tax write-offs"),
    c("Tax write-offs", values / 100)
  ), columns = 4L)
}
writexl::write_xlsx(
  list(
    AllRevenue = tax_sheet("Revenue", c(10, 11, 12)),
    AllReceipts = tax_sheet("Receipts", c(9, 10, 11))
  ),
  file.path(fixture_dir, "tax-writeoff-break.xlsx"),
  col_names = FALSE
)

monthly_sheet <- function(title) {
  as_sheet(list(
    c(title),
    c("for the eight months ended 28 February 2026"),
    c(NA),
    c("Year", "8 months"),
    c("to 30 June", "to 28 Feb", NA, NA, "Current Year Actual vs Forecast"),
    c("2025", "2025"),
    c("Actual", "Actual", NA, NA, "Actual", "Forecast", "Variance", NA, "Annual Forecast"),
    c("$m", "$m", NA, NA, "$m", "$m", "$m", "%", "$m"),
    c(NA, NA, "Revenue"),
    c(NA, 100, "Taxation revenue", "2", 110, 108, 2, 1.851852, 170)
  ), columns = 9L)
}
writexl::write_xlsx(
  list(
    `Fin Stmts - Perform` = monthly_sheet("STATEMENT OF FINANCIAL PERFORMANCE"),
    `Fin Stmts - Comp` = monthly_sheet("STATEMENT OF COMPREHENSIVE REVENUE AND EXPENSE"),
    `Fin Stmts - Net Worth` = monthly_sheet("STATEMENT OF CHANGES IN NET WORTH"),
    `Fin Stmts - Cash flows` = monthly_sheet("STATEMENT OF CASH FLOWS"),
    `Fin Stmts - CF Rec` = monthly_sheet("RECONCILIATION OF CASH FLOWS"),
    `Fin Stmts - Position` = monthly_sheet("STATEMENT OF FINANCIAL POSITION")
  ),
  file.path(fixture_dir, "monthly-statements.xlsx"),
  col_names = FALSE
)

efu_sheet <- function(title, metric, statuses = c("Actual", "Forecast", "Forecast")) {
  as_sheet(list(
    c(title),
    c("Source: The Treasury"),
    c(NA),
    c("June years", 2025, 2026, 2027),
    c(NA, statuses),
    c(metric, 1, 2, 3)
  ), columns = 4L)
}
efu_fiscal <- function() {
  as_sheet(list(
    c("Time Series of Fiscal Indicators"),
    c("Source: The Treasury"),
    c(NA),
    c("June years", NA, 2025, 2026, 2027),
    c(NA, NA, "Actual", "Forecast", "Forecast"),
    c("$millions"),
    c(NA),
    c("Revenue and expenses"),
    c(NA, "Core Crown tax revenue", 100, 110, 120)
  ), columns = 5L)
}
efu_economic <- function() {
  efu_sheet(
    "Time Series of Fiscal and Economic Indicators",
    "Real production GDP (annual average % change)"
  )
}
efu_key <- function(statuses) {
  as_sheet(list(
    c("Key economic and fiscal indicators"),
    c("Source: The Treasury"),
    c(NA),
    c("June years", 2025, 2026, 2027),
    c(NA, statuses),
    c("OBEGALx ($billions)", 1, 2, 3),
    c("% of GDP", 4, 5, 6)
  ), columns = 4L)
}
efu_index <- function(title, published, landing_url) {
  as_sheet(list(
    c(title),
    c(paste("Published", published)),
    c("Published by the New Zealand Treasury at:"),
    c(landing_url),
    c("Crown copyright \u00a9"),
    c(
      paste(
        "This copyright work is licensed under the Creative Commons",
        "Attribution 4.0 International licence."
      )
    ),
    c("https://creativecommons.org/licenses/by/4.0/")
  ), 1L)
}
write_efu <- function(
    path,
    key_sheet,
    key_statuses,
    title,
    published,
    landing_url) {
  sheets <- list(
    Index = efu_index(title, published, landing_url),
    key = efu_key(key_statuses),
    `Fiscal Time Series` = efu_fiscal(),
    `Economic Time Series` = efu_economic()
  )
  names(sheets)[[2L]] <- key_sheet
  writexl::write_xlsx(sheets, path, col_names = FALSE)
}
write_efu(
  file.path(fixture_dir, "befu.xlsx"),
  "Table 1",
  c("Actual", "Forecast", "Forecast"),
  "Data and Charts - Budget Economic and Fiscal Update 2026",
  "28 May 2026",
  "https://treasury.govt.nz/publications/efu/budget-economic-and-fiscal-update-2026"
)
write_efu(
  file.path(fixture_dir, "hyefu.xlsx"),
  "Table 1",
  c("Actual", "Forecast", "Forecast"),
  "Data and Charts - Half Year Economic and Fiscal Update 2025",
  "16 December 2025",
  "https://treasury.govt.nz/publications/efu/half-year-economic-and-fiscal-update-2025"
)
write_efu(
  file.path(fixture_dir, "prefu.xlsx"),
  "Executive Summary Table",
  c("Forecast1", "Forecast", "Forecast"),
  paste(
    "Data and Charts - Executive Summary, and Chapters 1 to 3 -",
    "Pre-election Economic and Fiscal Update 2023"
  ),
  "12 September 2023",
  "https://treasury.govt.nz/publications/efu/pre-election-economic-and-fiscal-update-2023"
)
