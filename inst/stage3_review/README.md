# readfiscalnz stage-3 independent review

## Result

**PASS_WITH_MEMORY_WARNING.** The deterministic stage-3 boundary and the
independently validated quiet-slot latency/RSS run pass. The memory warning is
retained: adding the worst observed cold-read increment of `222.828125 MiB` to
the measured `416 MiB` deployed-app idle baseline projects `638.828125 MiB`.
That projection is evidence for deployment planning, not a clean memory
clearance.

- Focused and full offline package gates are successful.
- All six committed XLSX fixtures exercise their release/parser route.
- Six retained official workbooks replayed with exact catalogue SHA-256:
  HFI (2,799 rows), tax outturn (29,929), monthly statements (834), BEFU
  (990), HYEFU (1,018), and PREFU (996).
- Two charts were independently cross-checked against exact workbook cells and
  visually inspected.

This directory contains no downloaded workbook and no private absolute path.
The committed evidence records official filenames, hashes, parser/schema
versions, row counts, exact source cells, revision policies, and licence terms.

## Closed boundary gaps

- BEFU, HYEFU, and PREFU now verify their Index title, publication date,
  landing page, and embedded CC BY 4.0 terms before parsing. A sibling workbook
  cannot be assigned another release ID.
- Successful redirects must end on `treasury.govt.nz` over HTTPS. The verified
  final URL is retained in live provenance.
- Saved landing-page title, status, document date, issue date, and licence
  drift abort catalogue verification.
- Canonical CSV replay requires exact columns and validates units,
  consolidation, accounting basis, period/status semantics, release type,
  forecast vintage, and horizon.
- The HFI parser treats `$ millions` and `% GDP` as separate blocks, retains
  footnote-marked years, and exposes an explicit measure/unit pair.
- The package-owned `nz-fiscal-series-1` contract keeps structural regimes
  separate while retaining HFI restatement notes and EFU source labels as
  point metadata, so they no longer fragment continuous histories.
- Raw content-addressed paths are namespaced by release ID. Parsed identity
  continues to include raw SHA-256, release ID, revision policy, parser and
  schema versions, and parser options.
- Existing retry caps, partial-file cleanup, signature checks, pinned-hash
  rejection, and atomic corrupt-target healing remain in force.

## Independent chart checks

| Release | Independent workbook cell | Meaning | Exact value |
|---|---|---|---:|
| HFI 1972–2025 | `Nominal GDP!C58` | 2025 actual nominal GDP | `436103` NZD million |
| BEFU 2026 | `Table 1!C10` | 2026 forecast OBEGALx | `-11.937` NZD billion |

`inst/scripts/render_stage3_evidence.R` uses `readxl` directly for those cells
and separately checks the parsed package values. It also re-hashes and parses
all six official files. Reproduce after installing this package:

```sh
READFISCALNZ_OFFICIAL_DIR=/path/to/catalogue-named-workbooks \
Rscript inst/scripts/render_stage3_evidence.R
```

Expected files:

- `artifacts/official_replay.csv`
- `artifacts/chart_cross_checks.csv`
- `artifacts/hfi_nominal_gdp.png`
- `artifacts/befu_2026_obegalx.png`

## Quiet-slot latency/RSS evidence

`artifacts/quiet_slot_performance_raw.csv` and
`artifacts/quiet_slot_performance_summary.csv` contain the independently
validated replacement run. Their SHA-256 values are:

- raw: `9cdbfc2da1413a531a4112bb7c83058991220ee506198c6a6b54c20fbfd26362`;
- summary: `e245997dadbbe014976e69a08d6962de9c6b3dca917c49a173ecdc7bae5b8212`.

The raw evidence contains exactly 60 rows: six releases × cold/warm × five
repetitions. Cold rows record
`disk_snapshot_absent;parsed_cache_cleared`; warm rows record
`disk_snapshot_preseeded;parsed_cache_primed`. Every row records the
single-process/reset/release lifecycle, retains the expected parsed row count,
and matches the exact official workbook SHA-256.

Observed maxima:

- latency: `1.12` seconds;
- process RSS: `578.125 MiB`;
- RSS increment: `222.828125 MiB`, on the tax-outturn cold path; and
- projected deployed peak: `416 + 222.828125 = 638.828125 MiB`.

The committed summary is recomputed from the raw rows in the offline contract
suite. The accepted disposition is `PASS_WITH_MEMORY_WARNING`; the projection
above remains a warning rather than a deployment-memory pass.

The harness still refuses without explicit quiet-slot opt-in, exact catalogue
hashes, 3–20 repetitions, and fresh isolated output targets. It rejects
outputs that alias each other or any official workbook. The committed evidence
contains no workbook bytes and no private absolute path.
