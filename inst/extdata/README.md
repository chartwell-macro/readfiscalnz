# Installed replay data

Package code, documentation, and package-generated synthetic fixtures are
licensed under the MIT licence, copyright Chartwell Macro. Official source data and
publications retain their separately recorded publisher, copyright, and
licence terms and are not assigned to Chartwell Macro.

`hfi.csv`, `tax.csv`, `monthly.csv`, and `efu.csv` are tiny, synthetic,
deterministic canonical CSV fixtures generated for this package. They contain no
copied Treasury workbook bytes or Treasury product data. They support small,
reproducible, offline examples and parser tests.

`hfi-core-crown-expenses-official.csv` is different: it is an
attribution-documented canonical snapshot of the official Historical Fiscal
Indicators `Core Crown Expenses` series for fiscal years 1994--2025. It was
produced with `read_nz_hfi(..., revision = "as_published", sheets = "Spending")`
from the catalogued Treasury workbook without changing observation values.

- Source publisher and attribution: The Treasury.
- Source release: `hfi-1972-2025`, *Historical Fiscal Indicators 1972--2025*.
- Source workbook: `fiscaltimeseries1972-2025-year-end25.xlsx`.
- Source workbook SHA-256:
  `de59f9028a81a697ee66eea04861edfd8e2c3a7e472b3b8798d976951964f70f`.
- Snapshot SHA-256:
  `0b6266b64f90390eee6867e715397ec5b6bf7781d478cadddd305277981b32cf`.
- Parser and schema: `hfi-2.0.0` and `hfi-long-2`.
- Changes: selected the `Core Crown Expenses` level series from the `Spending`
  sheet and normalised it to the package's canonical long-form CSV schema;
  workbook presentation, formulas, other series, and other sheets are omitted;
  observation values are unmodified.
- Source: <https://www.treasury.govt.nz/publications/information-release/data-fiscal-time-series-historical-fiscal-indicators>.
- Licence: Crown Copyright, Attribution 4.0 International (CC BY 4.0),
  <https://creativecommons.org/licenses/by/4.0/>.

The package-level source terms and live-retrieval acknowledgement are described
in `inst/TERMS.md` and returned by `nz_fiscal_terms()`.
