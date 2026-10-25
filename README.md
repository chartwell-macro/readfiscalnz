# readfiscalnz

`readfiscalnz` provides reproducible access to official New Zealand Treasury
fiscal data. It reads audited workbook families, preserves exact publication
vintages and source bytes, and returns continuous fiscal series with explicit
accounting, consolidation, unit, status and forecast semantics.

## Installation

Install the private GitHub repository with an authenticated GitHub session:

```r
pak::pak("chartwell-macro/readfiscalnz")
```

Or build and install from a local clone:

```sh
R CMD build .
R CMD INSTALL readfiscalnz_0.1.0.tar.gz
```

## Data covered

| Treasury family | Package surface |
|---|---|
| Historical Fiscal Indicators | Five historical data sheets covering 1972–2025, including March/June year regimes, cash/accrual transitions, NZ-IFRS/PBE regimes and published restatements |
| Tax outturn | Accrual revenue and cash receipts through May 2026, with unconsolidated rows, elimination adjustments and government totals |
| Monthly financial statements | Six primary statement sheets from the February 2026 release, retaining actual, forecast, variance, year-to-date and annual roles |
| Economic and Fiscal Updates | BEFU 2026, HYEFU 2025 and PREFU 2023 fiscal, economic and key-indicator time series |

The checked-in catalogue contains six byte-pinned releases with verified
official URLs and SHA-256 values.

## Use

```r
library(readfiscalnz)

releases <- list_nz_fiscal_releases()

hfi <- read_nz_hfi(
  release_id = "hfi-1972-2025",
  accept_terms = TRUE
)

tax <- read_nz_tax_outturn(
  release_id = "tax-outturn-2026-05",
  measure = "both",
  accept_terms = TRUE
)

monthly <- read_nz_monthly_financials(
  release_id = "monthly-financials-2026-02",
  table = c("performance", "position"),
  accept_terms = TRUE
)

befu <- read_nz_efu(
  release_id = "befu-2026",
  dataset = c("fiscal", "economic", "key_indicators"),
  accept_terms = TRUE
)
```

Local workbook and canonical CSV replay uses the same parsers:

```r
hfi_local <- read_nz_hfi(file = "saved-hfi.xlsx")
```

## Continuous fiscal series

`nz_fiscal_series_catalogue()` identifies stable series using accounting
basis, consolidation, fiscal regime, unit and actual/forecast status.
`nz_fiscal_series_history()` then returns the ordered observations for one
identity:

```r
catalogue <- nz_fiscal_series_catalogue(hfi)
history <- nz_fiscal_series_history(hfi, catalogue$series_id[[1]])
```

Published restatement notes, source-status labels and forecast vintages remain
attached to their original observations.

## Reproducibility

Downloads are HTTPS-only, timeout- and size-bounded, signature checked,
SHA-256 verified and atomically stored in an immutable release-specific cache.
Parsed-cache identity includes the raw hash, release, revision policy, parser,
schema and parser options.

```r
provenance <- nz_fiscal_provenance(hfi)
raw_bytes <- nz_fiscal_raw_bytes(hfi)
prune_nz_fiscal_cache()
```

The default test suite is fully offline. Synthetic XLSX fixtures and canonical
CSV replays cover every supported family and fiscal-regime boundary.

Installed documentation is available through:

```r
help(package = "readfiscalnz")
```

Detailed reproducibility evidence is retained under `inst/stage3_review/`.
