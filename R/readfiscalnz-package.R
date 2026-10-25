#' readfiscalnz: replayable New Zealand Treasury fiscal data
#'
#' `readfiscalnz` supports four deliberately narrow Treasury release families:
#' Historical Fiscal Indicators, monthly tax outturn history, primary monthly
#' financial statements, and stable BEFU/HYEFU/PREFU time-series sheets.
#'
#' Every reader preserves the exact input in a content-addressed raw snapshot
#' and attaches release, licensing, revision, parser, schema, and SHA-256
#' provenance. Live retrieval is never performed on package load or during
#' default tests.
#'
#' @keywords internal
"_PACKAGE"
