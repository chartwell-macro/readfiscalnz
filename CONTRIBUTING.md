# Contributing to readfiscalnz

Contributions should make it easier to retrieve, validate, or replay the
official New Zealand Treasury data covered by this package. Please keep
changes reproducible, narrow, and compatible with existing reader output.

## Start with the package contract

Read the relevant sections of `README.md`, `NEWS.md`, and `inst/TERMS.md`
before changing a reader or an example. The current release catalogue is
deliberately limited to these six release IDs:

| Family | Release ID |
| --- | --- |
| Historical Fiscal Indicators | `hfi-1972-2025` |
| Tax outturn | `tax-outturn-2026-05` |
| Monthly financial statements | `monthly-financials-2026-02` |
| Economic and Fiscal Update | `befu-2026` |
| Economic and Fiscal Update | `hyefu-2025` |
| Economic and Fiscal Update | `prefu-2023` |

Use `list_nz_fiscal_releases()` to inspect the catalogue rather than copying
release metadata into new code. An issue or pull request about a new source
should say whether it extends this static support boundary or only fixes an
existing release.

The public contract includes reader arguments and defaults, exported names,
output column names and order, `nz_fiscal_data` classes and provenance
attributes, date semantics, errors and warnings, network and cache behaviour,
and the structural dimensions used by `nz_fiscal_series_catalogue()` and
`nz_fiscal_series_history()`. Preserve these whenever possible. In
particular, do not silently rename fields, change a series identity or hash,
or collapse accounting, consolidation, unit, regime, release, or
actual/forecast dimensions. A deliberate contract change needs a focused
discussion, regression coverage, and a clear `NEWS.md` entry.

## Report a reproducible issue

Use the bug or configuration/data-access issue template and include:

- the exact `readfiscalnz` and R versions, operating system, and relevant
  dependency versions;
- the release ID, family, route (`tests/testthat` fixture, local file, cache,
  or live retrieval), and the exact call that reproduces the result;
- the expected and actual values, names, classes, attributes, dates, warnings,
  or errors; and
- a short `sessionInfo()` output, with machine-specific paths shortened where
  practical.

Do not paste credentials, access tokens, cookies, signed URLs, private
absolute paths, cache contents, or private Treasury workbooks. Prefer a
checked-in fixture, a small synthetic input, or a public SHA-256 and release
ID. If a failure depends on local configuration, describe the setting using a
placeholder such as `<cache-dir>` rather than publishing the path.

## Develop and test locally

Run commands from the package root. The standard test suite is offline and
uses the six source fixtures under `tests/testthat/fixtures/` plus the small
installed replay fixtures under `inst/extdata/`.

```r
testthat::test_local(".", reporter = "summary")
```

For a release-quality check, build a source tarball and check that tarball in
a clean library. Keep the actual tarball name printed by `R CMD build` when
running the second command:

```sh
R CMD build .
R CMD check --as-cran readfiscalnz_<version>.tar.gz
```

Use temporary cache and library directories for experiments. Ordinary tests,
examples, and vignettes should remain deterministic and offline; do not add a
live-network dependency just to make a test pass.

Live validation is an explicit, optional gate rather than a prerequisite for
ordinary development. Review `inst/TERMS.md` first, then opt in only when the
current official artifacts can be reached and you accept the release terms:

```sh
READFISCALNZ_LIVE=true Rscript inst/scripts/validate_live.R
```

The live script uses the catalogue's six releases and a temporary cache. Do
not commit downloaded artifacts or live cache files, and do not put
credentials in environment values or issue/PR output.

## Documentation and generated files

Roxygen comments in `R/` are the source of truth for function documentation.
After editing them, regenerate and inspect the generated namespace and help
files:

```r
roxygen2::roxygenise()
```

If the repository contains `README.Rmd`, regenerate the committed
`README.md` from that source (for example,
`rmarkdown::render("README.Rmd", output_format = "github_document", output_file = "README.md")`).
Build vignettes through `R CMD build .` and inspect their rendered output.
When the pkgdown configuration is present, also run its site build and check
that reference groups, articles, examples, and links still agree with the
package API.

Commit intentional generated package documentation such as `NAMESPACE`,
`man/*.Rd`, and the rendered `README.md` with their source changes. Do not
commit `.Rcheck` directories, source tarballs, caches, downloaded Treasury
files, private benchmark output, or a generated `docs/` site unless the
repository explicitly adopts a convention for it.

The files under `inst/stage3_review/` are a checked-in evidence contract used
by package tests. Do not hand-edit their ledgers or summaries. If a change
requires new evidence, regenerate it with the relevant script under
`inst/scripts/`, review the resulting hashes and inputs, and run the focused
stage-3 contract tests before including the evidence in a pull request.

## Pull requests

Keep a pull request focused and explain the user-visible result, the files
changed, and any compatibility implications. Include the R/OS environment
for checks and say explicitly when a live validation was not run. Runtime
changes should include a fixture-based regression test and, where parsing or
transport performance could be affected, a before/after measurement using
the same input and cache conditions. Documentation-only changes should still
run the offline suite and documentation build/checks.
