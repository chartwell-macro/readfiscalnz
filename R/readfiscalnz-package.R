#' readfiscalnz: Reproducible New Zealand Treasury fiscal data
#'
#' `readfiscalnz` turns six exact New Zealand Treasury Excel publications into
#' validated, long-form R data. The supported material comprises two workbooks
#' containing long histories, one point-in-time monthly accounts release, and
#' three separate Economic and Fiscal Update forecast vintages. Fiscal regimes,
#' accounting basis, consolidation scope, units, and actual/forecast status
#' remain explicit in the returned data.
#'
#' ## Supported release families
#'
#' The package covers four Treasury families and six publication artifacts:
#'
#' * `hfi-1972-2025` is one current published snapshot of the rolling
#'   Historical Fiscal Indicators history across 1972--2025, not an archive of
#'   HFI publication vintages;
#' * `tax-outturn-2026-05` is one current published snapshot of the rolling
#'   monthly revenue and receipts history through May 2026;
#' * `monthly-financials-2026-02` is one point-in-time Financial Statements
#'   release for the eight months ended 28 February 2026, with annual,
#'   year-to-date, actual, forecast, and variance observation roles; and
#' * `befu-2026`, `hyefu-2025`, and `prefu-2023` are three separate Economic
#'   and Fiscal Update vintages with fiscal, economic, and key-indicator sheets
#'   containing both actual or unaudited rows and forecast rows.
#'
#' Each identifier maps to one original, byte-pinned Treasury `.xlsx` file.
#' With no `file` argument, the corresponding reader resolves the catalogued
#' Treasury URL after terms acknowledgement and may reuse the exact
#' hash-addressed raw cache before downloading. With `file`, it parses a
#' caller-supplied local `.xlsx` or canonical `.csv`. The six full workbooks are
#' not installed with the package; installed official data are limited to the
#' documented 32-row HFI Core Crown Expenses canonical CSV subset, not the
#' original XLSX. Other installed CSV examples are synthetic. Use
#' [list_nz_fiscal_releases()] to inspect exact filenames, release status,
#' dates, source URLs, hashes, forecast horizons, and per-release terms.
#'
#' ## Typical workflow
#'
#' 1. Discover a supported release with [list_nz_fiscal_releases()].
#' 2. Read it with the corresponding `read_nz_*()` function: [read_nz_hfi()],
#'    [read_nz_tax_outturn()], [read_nz_monthly_financials()], or
#'    [read_nz_efu()]. A saved XLSX or canonical CSV can be supplied through
#'    `file` for deterministic offline replay.
#' 3. Identify structural series with [nz_fiscal_series_catalogue()] and
#'    replay one ordered series with [nz_fiscal_series_history()].
#' 4. Audit the source with [nz_fiscal_provenance()] and, when needed, verify
#'    the exact preserved bytes with [nz_fiscal_raw_bytes()]. Inspect licensing
#'    metadata with [nz_fiscal_terms()].
#'
#' ## Output and provenance
#'
#' Every reader returns a long-form object with class
#' `c("nz_fiscal_data", "data.frame")`. Each row is an observation with a
#' family-specific set of dimensions and a numeric `value`; date fields are
#' `Date` values (`date` for tax outturn and `period_end` for the other
#' families). The dimensions retain the source's series, section or table,
#' accounting and consolidation semantics, units, fiscal regime, and
#' actual/forecast or publication-status labels. The object carries an
#' `nz_fiscal_provenance` attribute containing the selected release, source
#' route, licensing metadata, revision policy, raw SHA-256 and size, and the
#' parser and schema versions. These values are available through
#' [nz_fiscal_provenance()] rather than inferred from parsed rows.
#'
#' ## Network, cache, and terms
#'
#' Package load and the default test suite do not perform network requests.
#' Supplying `file` never performs network I/O. Otherwise, a reader resolves
#' the catalogued official artifact over HTTPS only, and refuses retrieval
#' until `accept_terms = TRUE` has been supplied after reviewing
#' [nz_fiscal_terms()]. Accepted artifacts are signature- and SHA-256-checked
#' before parsing. Raw snapshots are written below the package-managed
#' `tools::R_user_dir("readfiscalnz", "cache")/v1` cache by default; parsed
#' results are kept in a bounded in-memory cache. Set `cache_dir` to a
#' temporary or project-managed directory for isolated workflows, and use
#' [prune_nz_fiscal_cache()] to apply raw-cache limits. Local replay may write
#' to that cache but never modifies the input file.
#'
#' ## Licensing and citation
#'
#' Package code and documentation are released under the MIT licence,
#' copyright `Chartwell Macro` (see `LICENSE`).
#'
#' Source data and official publications retain their own rights. Treasury data
#' terms are recorded per release; see
#' `system.file("TERMS.md", package = "readfiscalnz")`,
#' [nz_fiscal_terms()], the [Treasury copyright and licensing page](https://www.treasury.govt.nz/copyright-and-licensing),
#' and the [CC BY 4.0 licence](https://creativecommons.org/licenses/by/4.0/).
#' Check the applicable release landing page before redistributing data, as
#' third-party material may have separate rights. The package does not transfer
#' those rights to `Chartwell Macro`. To cite the package, run
#' `citation("readfiscalnz")`. Source code is available at
#' <https://github.com/chartwell-macro/readfiscalnz>. Email support and issue
#' reports to <mailto:chartwellmacro@gmail.com>.
#'
#' @author Chartwell Macro
#'
#' @seealso
#' [list_nz_fiscal_releases()], [read_nz_hfi()],
#' [read_nz_tax_outturn()], [read_nz_monthly_financials()], [read_nz_efu()],
#' [nz_fiscal_series_catalogue()], [nz_fiscal_series_history()],
#' [nz_fiscal_provenance()], [nz_fiscal_raw_bytes()], [nz_fiscal_terms()],
#' [prune_nz_fiscal_cache()]
"_PACKAGE"
