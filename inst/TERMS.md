# Terms metadata

Package code and documentation are licensed under the MIT licence, copyright
Chartwell Macro, as recorded in the package `LICENSE`. Source data, official
publications, and third-party material retain their own copyright and licence
terms; this file records those terms separately and does not assign them to
Chartwell Macro.

The supported New Zealand Treasury release pages identified their data
artifacts with the exact statement `© Crown Copyright, Attribution 4.0
International (CC BY 4.0)` when verified on 26 July 2026. Reuse attribution is
to `The Treasury`. The licence text is available at:
https://creativecommons.org/licenses/by/4.0/

The package stores terms per release rather than assuming one site-wide rule.
Inspect `nz_fiscal_terms()` and the release landing page before retrieval.
`accept_terms = TRUE` is an explicit acknowledgement for live downloads; it is
not legal advice and does not alter the source licence.

The Treasury's licensing page notes that images or other third-party copyright
material may require separate permission. `readfiscalnz` routes catalogued
Treasury data artifacts and documented replay snapshots derived from them, and
makes no package-ownership claim for external material.

## Bundled HFI Core Crown expenses snapshot

`inst/extdata/hfi-core-crown-expenses-official.csv` contains the official
Treasury Historical Fiscal Indicators `Core Crown Expenses` observations for
fiscal years 1994--2025. It is attributed to The Treasury and distributed
under CC BY 4.0. The source workbook SHA-256 is
`de59f9028a81a697ee66eea04861edfd8e2c3a7e472b3b8798d976951964f70f`;
the canonical snapshot SHA-256 is
`0b6266b64f90390eee6867e715397ec5b6bf7781d478cadddd305277981b32cf`.
The snapshot selects the `Core Crown Expenses` level series from the workbook's
`Spending` sheet and normalises it without altering observation values. See
`inst/extdata/README.md` for the complete source, parser, schema, attribution,
licence, and change notice.

Official terms:
https://www.treasury.govt.nz/copyright-and-licensing
