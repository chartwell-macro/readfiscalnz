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
  `nz_fiscal_series_history()` for stable, continuous fiscal histories.
- Preserves release, accounting regime, consolidation, unit, forecast vintage,
  source status, exact source bytes and SHA-256 provenance.
- Adds bounded HTTPS transport, immutable release-specific caching, parsed LRU
  caching, offline replay, cache repair and deterministic pruning.
- Adds fully offline HTML, XLSX and CSV fixtures with strict schema and release
  identity validation.
- Documents private GitHub installation and the complete supported data
  surface in a conventional package README.
