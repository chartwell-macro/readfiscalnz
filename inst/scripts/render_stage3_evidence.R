#!/usr/bin/env Rscript

required_packages <- c("readfiscalnz", "readxl", "ggplot2", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    sprintf(
      "Install the required packages before rendering evidence: %s.",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
input_dir <- Sys.getenv("READFISCALNZ_OFFICIAL_DIR", unset = "")
if (!nzchar(input_dir) && length(args) >= 1L) {
  input_dir <- args[[1L]]
}
if (!nzchar(input_dir) || !dir.exists(input_dir)) {
  stop(
    paste(
      "Set READFISCALNZ_OFFICIAL_DIR, or pass a directory containing the six",
      "catalogued official workbooks as the first argument."
    ),
    call. = FALSE
  )
}
output_dir <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path("inst", "stage3_review", "artifacts")
}
if (!dir.exists(output_dir) &&
    !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
  stop("Could not create the stage-3 evidence output directory.", call. = FALSE)
}

catalogue <- readfiscalnz::list_nz_fiscal_releases()
if (nrow(catalogue) != 6L || anyDuplicated(catalogue$release_id)) {
  stop("Expected exactly six unique catalogue releases.", call. = FALSE)
}

expected_terms <-
  "\u00a9 Crown Copyright, Attribution 4.0 International (CC BY 4.0)"
expected_attribution <- "The Treasury"
expected_license_url <- "https://creativecommons.org/licenses/by/4.0/"

read_release <- function(release, path, cache_dir) {
  switch(
    release$family[[1L]],
    hfi = readfiscalnz::read_nz_hfi(
      release_id = release$release_id[[1L]],
      file = path,
      cache_dir = cache_dir
    ),
    tax_outturn = readfiscalnz::read_nz_tax_outturn(
      release_id = release$release_id[[1L]],
      file = path,
      measure = "both",
      cache_dir = cache_dir
    ),
    monthly_financials = readfiscalnz::read_nz_monthly_financials(
      release_id = release$release_id[[1L]],
      file = path,
      table = c(
        "performance", "comprehensive", "net_worth", "cash_flows",
        "cash_flow_reconciliation", "position"
      ),
      cache_dir = cache_dir
    ),
    efu = readfiscalnz::read_nz_efu(
      release_id = release$release_id[[1L]],
      update = release$release_type[[1L]],
      file = path,
      dataset = c("fiscal", "economic", "key_indicators"),
      cache_dir = cache_dir
    ),
    stop("Unsupported catalogue family.", call. = FALSE)
  )
}

results <- vector("list", nrow(catalogue))
names(results) <- catalogue$release_id
replay_rows <- vector("list", nrow(catalogue))
for (index in seq_len(nrow(catalogue))) {
  release <- catalogue[index, , drop = FALSE]
  path <- file.path(input_dir, release$file_name[[1L]])
  if (!file.exists(path)) {
    stop(
      sprintf("Missing official workbook: %s.", release$file_name[[1L]]),
      call. = FALSE
    )
  }
  hash <- digest::digest(
    path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  if (!identical(hash, release$expected_sha256[[1L]])) {
    stop(
      sprintf(
        "Official workbook hash drift for %s: expected %s, received %s.",
        release$release_id[[1L]],
        release$expected_sha256[[1L]],
        hash
      ),
      call. = FALSE
    )
  }
  cache_dir <- tempfile("readfiscalnz-stage3-cache-")
  dir.create(cache_dir)
  data <- read_release(release, path, cache_dir)
  provenance <- readfiscalnz::nz_fiscal_provenance(data)
  required_matches <- c(
    identical(provenance$release_id, release$release_id[[1L]]),
    identical(provenance$release_status, release$status[[1L]]),
    identical(provenance$raw_sha256, hash),
    identical(provenance$catalogue_sha256, hash),
    identical(provenance$revision_policy, "as_published"),
    identical(provenance$terms_statement, expected_terms),
    identical(provenance$terms_attribution, expected_attribution),
    identical(provenance$license_url, expected_license_url),
    identical(provenance$terms_license, "CC BY 4.0")
  )
  if (!all(required_matches)) {
    stop(
      sprintf(
        "Provenance contract failed for %s.",
        release$release_id[[1L]]
      ),
      call. = FALSE
    )
  }
  results[[release$release_id[[1L]]]] <- data
  replay_rows[[index]] <- data.frame(
    release_id = release$release_id[[1L]],
    family = release$family[[1L]],
    release_type = release$release_type[[1L]],
    status = release$status[[1L]],
    workbook = release$file_name[[1L]],
    workbook_sha256 = hash,
    rows = nrow(data),
    columns = ncol(data),
    parser_version = provenance$parser_version,
    schema_version = provenance$schema_version,
    revision_policy = provenance$revision_policy,
    terms_statement = provenance$terms_statement,
    terms_attribution = provenance$terms_attribution,
    license_url = provenance$license_url,
    exact_hash_and_provenance_match = TRUE,
    stringsAsFactors = FALSE
  )
}
replay <- do.call(rbind, replay_rows)

hfi_path <- file.path(
  input_dir,
  catalogue$file_name[catalogue$release_id == "hfi-1972-2025"]
)
hfi_sheet <- suppressMessages(readxl::read_excel(
  hfi_path,
  sheet = "Nominal GDP",
  col_names = FALSE
))
hfi_cell_year <- suppressWarnings(as.integer(hfi_sheet[[2L]][58L]))
hfi_cell_value <- suppressWarnings(as.numeric(hfi_sheet[[3L]][58L]))
if (
  !identical(hfi_cell_year, 2025L) ||
    !isTRUE(all.equal(hfi_cell_value, 436103, tolerance = 1e-12))
) {
  stop("Independent HFI check changed at Nominal GDP!C58.", call. = FALSE)
}
hfi_data <- results[["hfi-1972-2025"]]
hfi_series <- hfi_data[
  hfi_data$section == "Nominal GDP" &
    hfi_data$series == "Nominal GDP",
  ,
  drop = FALSE
]
hfi_series <- hfi_series[order(hfi_series$year), , drop = FALSE]
hfi_latest <- hfi_series[hfi_series$year == 2025L, , drop = FALSE]
if (
  nrow(hfi_latest) != 1L ||
    !isTRUE(all.equal(hfi_latest$value[[1L]], hfi_cell_value, tolerance = 1e-12))
) {
  stop("HFI parser does not match independent cell C58.", call. = FALSE)
}

befu_path <- file.path(
  input_dir,
  catalogue$file_name[catalogue$release_id == "befu-2026"]
)
befu_sheet <- suppressMessages(readxl::read_excel(
  befu_path,
  sheet = "Table 1",
  col_names = FALSE
))
befu_cell_year <- suppressWarnings(as.integer(befu_sheet[[3L]][4L]))
befu_cell_status <- trimws(as.character(befu_sheet[[3L]][5L]))
befu_cell_value <- suppressWarnings(as.numeric(befu_sheet[[3L]][10L]))
if (
  !identical(befu_cell_year, 2026L) ||
    !identical(befu_cell_status, "Forecast") ||
    !isTRUE(all.equal(befu_cell_value, -11.937, tolerance = 1e-12))
) {
  stop("Independent BEFU check changed at Table 1!C10.", call. = FALSE)
}
befu_data <- results[["befu-2026"]]
befu_series <- befu_data[
  befu_data$dataset == "key_indicators" &
    befu_data$series == "OBEGALx ($billions)",
  ,
  drop = FALSE
]
befu_series <- befu_series[order(befu_series$year), , drop = FALSE]
befu_recent <- befu_series[befu_series$year == 2026L, , drop = FALSE]
if (
  nrow(befu_recent) != 1L ||
    !identical(befu_recent$actual_forecast[[1L]], "forecast") ||
    !identical(befu_recent$unit[[1L]], "NZD billion") ||
    !isTRUE(all.equal(befu_recent$value[[1L]], befu_cell_value, tolerance = 1e-12))
) {
  stop("BEFU parser does not match independent cell C10.", call. = FALSE)
}

chart_caption <- function(landing_url) {
  paste0(
    expected_terms,
    "\nAttribution: ", expected_attribution, " | ", expected_license_url,
    "\nSource: official New Zealand Treasury workbook | ", landing_url,
    "\nNo logo or emblem reproduced."
  )
}
chart_theme <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(
      size = 8,
      hjust = 0,
      lineheight = 1.05,
      margin = ggplot2::margin(t = 10)
    ),
    plot.margin = ggplot2::margin(t = 12, r = 16, b = 18, l = 12),
    panel.grid.minor = ggplot2::element_blank()
  )

hfi_chart <- ggplot2::ggplot(
  hfi_series,
  ggplot2::aes(x = year, y = value)
) +
  ggplot2::geom_line(linewidth = 0.8, colour = "#145DA0") +
  ggplot2::geom_point(
    data = hfi_latest,
    size = 2.8,
    colour = "#145DA0"
  ) +
  ggplot2::geom_text(
    data = hfi_latest,
    ggplot2::aes(label = "2025 actual: NZ$436,103m"),
    hjust = 1.05,
    vjust = -0.8,
    colour = "#145DA0",
    size = 3.4
  ) +
  ggplot2::scale_y_continuous(
    labels = function(value) format(
      value,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  ) +
  ggplot2::labs(
    title = "New Zealand nominal GDP",
    subtitle = "Historical Fiscal Indicators; 2025 actual = NZ$436,103 million",
    x = NULL,
    y = "NZD million",
    caption = chart_caption(
      catalogue$landing_url[
        catalogue$release_id == "hfi-1972-2025"
      ]
    )
  ) +
  chart_theme

befu_chart <- ggplot2::ggplot(
  befu_series,
  ggplot2::aes(
    x = year,
    y = value,
    colour = actual_forecast,
    group = 1
  )
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, colour = "#777777") +
  ggplot2::geom_line(linewidth = 0.8, colour = "#287D3C") +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::scale_colour_manual(
    values = c(actual = "#145DA0", forecast = "#B33A3A")
  ) +
  ggplot2::labs(
    title = "BEFU 2026 OBEGALx outlook",
    subtitle = "2026 forecast = -NZ$11.937 billion",
    x = NULL,
    y = "NZD billion",
    colour = NULL,
    caption = chart_caption(
      catalogue$landing_url[
        catalogue$release_id == "befu-2026"
      ]
    )
  ) +
  chart_theme

ggplot2::ggsave(
  file.path(output_dir, "hfi_nominal_gdp.png"),
  hfi_chart,
  width = 10,
  height = 7.2,
  dpi = 160,
  bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "befu_2026_obegalx.png"),
  befu_chart,
  width = 10,
  height = 7.2,
  dpi = 160,
  bg = "white"
)

cross_checks <- data.frame(
  release_id = c("hfi-1972-2025", "befu-2026"),
  workbook = c(
    catalogue$file_name[catalogue$release_id == "hfi-1972-2025"],
    catalogue$file_name[catalogue$release_id == "befu-2026"]
  ),
  workbook_sha256 = c(
    catalogue$expected_sha256[catalogue$release_id == "hfi-1972-2025"],
    catalogue$expected_sha256[catalogue$release_id == "befu-2026"]
  ),
  sheet = c("Nominal GDP", "Table 1"),
  cell = c("C58", "C10"),
  series = c("Nominal GDP", "OBEGALx ($billions)"),
  period = c("2025", "2026"),
  source_status = c("actual", "forecast"),
  raw_value = c(hfi_cell_value, befu_cell_value),
  display_value = c("NZ$436,103 million", "-NZ$11.937 billion"),
  parser_matches_independent_cell = TRUE,
  terms_statement = expected_terms,
  terms_attribution = expected_attribution,
  license_url = expected_license_url,
  stringsAsFactors = FALSE
)
utils::write.csv(
  replay,
  file.path(output_dir, "official_replay.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
utils::write.csv(
  cross_checks,
  file.path(output_dir, "chart_cross_checks.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat(
  sprintf(
    paste0(
      "Verified %s official releases and rendered 2 charts. ",
      "HFI C58 = %s; BEFU C10 = %.3f.\n"
    ),
    nrow(replay),
    format(hfi_cell_value, big.mark = ",", scientific = FALSE),
    befu_cell_value
  )
)
