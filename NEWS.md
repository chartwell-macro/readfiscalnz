# readfiscalnz (development version)

## Documentation and release engineering

- Adds a generated README, deterministic offline examples for every exported
  function, task-oriented vignettes, a grouped pkgdown site, citation and
  contribution guidance, and multi-platform continuous integration.
- Documents the exact reader schemas, classes, attributes, series identities,
  cache and network boundaries, source terms, failure modes, and the static
  six-release support boundary.
- Clarifies that the supported data comprise two rolling-history workbooks,
  one monthly accounts snapshot, and three separate EFU forecast vintages, and
  distinguishes downloaded Treasury XLSX files from installed example data.
- Adds tiny installed synthetic canonical CSV fixtures so examples and
  vignettes exercise the real public replay path without network access.
- Adds an end-to-end retrieval and chart workflow using an installed,
  attribution-documented snapshot of the official HFI Core Crown expenses
  series.
- Standardises the package author, maintainer, and copyright-holder display as
  Chartwell Macro across package and repository metadata.
- Records the explicit rights boundary: package code, documentation, and
  package-generated synthetic fixtures use the MIT licence, copyright Chartwell Macro;
  Treasury source data and official publications retain their own rights and
  attribution.

## Portability

- Makes English release-date parsing and numeric series identities independent
  of the process locale, without changing existing English-locale results.
- Makes filesystem link-hardening tests conditional only on the platform's
  actual symbolic-link and hard-link capabilities.

# readfiscalnz 0.1.0

- Adds six byte-pinned New Zealand Treasury releases covering Historical
  Fiscal Indicators, tax outturn, monthly financial statements, BEFU, HYEFU
  and PREFU.
- Reads five Historical Fiscal Indicator sheets across the published
  accounting, fiscal-year and consolidation regimes from 1972 through 2025.
- Reads accrual tax revenue and cash receipts, including elimination
  adjustments and the June 2007 write-off boundary.
- Reads six monthly financial-statement routes with actual, forecast,
  variance, year-to-date and annual observation roles.
- Reads fiscal, economic and key-indicator time series from BEFU 2026, HYEFU
  2025 and PREFU 2023.
- Adds `nz_fiscal_series_catalogue()` and
  `nz_fiscal_series_history()` for canonical, structurally consistent fiscal
  series.
- Preserves release, accounting regime, consolidation, unit, forecast vintage,
  source status, exact source bytes and SHA-256 provenance.
- Adds bounded HTTPS transport, immutable release-specific caching, parsed LRU
  caching, offline replay, cache repair and deterministic pruning.
- Adds fully offline HTML, XLSX and CSV fixtures with strict schema and release
  identity validation.
- Documents GitHub installation and the complete supported data
  surface in a conventional package README.
