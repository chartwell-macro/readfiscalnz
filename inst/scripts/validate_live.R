# Opt-in live validation against the exact official artifacts in the catalogue.

enabled <- identical(tolower(Sys.getenv("READFISCALNZ_LIVE")), "true")
if (!enabled) {
  message("Live validation disabled. Set READFISCALNZ_LIVE=true to opt in.")
  quit(status = 0L)
}

if (!requireNamespace("readfiscalnz", quietly = TRUE)) {
  stop("Install readfiscalnz before live validation.", call. = FALSE)
}

cache_dir <- Sys.getenv(
  "READFISCALNZ_CACHE_DIR",
  file.path(tempdir(), "readfiscalnz-live-cache")
)

checks <- list(
  hfi = function() readfiscalnz::read_nz_hfi(
    release_id = "hfi-1972-2025",
    sheets = "Spending",
    cache_dir = cache_dir,
    accept_terms = TRUE
  ),
  tax = function() readfiscalnz::read_nz_tax_outturn(
    release_id = "tax-outturn-2026-05",
    measure = "revenue",
    cache_dir = cache_dir,
    accept_terms = TRUE
  ),
  monthly = function() readfiscalnz::read_nz_monthly_financials(
    release_id = "monthly-financials-2026-02",
    table = "performance",
    cache_dir = cache_dir,
    accept_terms = TRUE
  ),
  befu = function() readfiscalnz::read_nz_efu(
    release_id = "befu-2026",
    update = "BEFU",
    dataset = "fiscal",
    cache_dir = cache_dir,
    accept_terms = TRUE
  ),
  hyefu = function() readfiscalnz::read_nz_efu(
    release_id = "hyefu-2025",
    update = "HYEFU",
    dataset = "fiscal",
    cache_dir = cache_dir,
    accept_terms = TRUE
  ),
  prefu = function() readfiscalnz::read_nz_efu(
    release_id = "prefu-2023",
    update = "PREFU",
    dataset = "fiscal",
    cache_dir = cache_dir,
    accept_terms = TRUE
  )
)

for (name in names(checks)) {
  result <- checks[[name]]()
  provenance <- readfiscalnz::nz_fiscal_provenance(result)
  message(sprintf(
    "%s: %s rows; SHA-256 %s",
    name,
    nrow(result),
    provenance$raw_sha256
  ))
}
