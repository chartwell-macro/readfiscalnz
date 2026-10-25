.nz_catalogue <- function() {
  terms_url <- "https://www.treasury.govt.nz/copyright-and-licensing"
  data.frame(
    release_id = c(
      "hfi-1972-2025",
      "tax-outturn-2026-05",
      "monthly-financials-2026-02",
      "befu-2026",
      "hyefu-2025",
      "prefu-2023"
    ),
    family = c(
      "hfi",
      "tax_outturn",
      "monthly_financials",
      "efu",
      "efu",
      "efu"
    ),
    release_type = c("HFI", "tax_outturn", "monthly", "BEFU", "HYEFU", "PREFU"),
    release_title = c(
      "Data - Fiscal Time Series Historical Fiscal Indicators 1972-2025",
      "Tax Outturn Data - May 2026",
      "Interim Financial Statements for the eight months ended 28 February 2026",
      "Budget Economic and Fiscal Update 2026",
      "Half Year Economic and Fiscal Update 2025",
      "Pre-election Economic and Fiscal Update 2023"
    ),
    workbook_title = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "Data and Charts - Budget Economic and Fiscal Update 2026",
      "Data and Charts - Half Year Economic and Fiscal Update 2025",
      paste(
        "Data and Charts - Executive Summary, and Chapters 1 to 3 -",
        "Pre-election Economic and Fiscal Update 2023"
      )
    ),
    status = c("current", "current", "superseded", "current", "superseded", "superseded"),
    document_date = as.Date(c(
      "2026-01-09", "2026-07-02", "2026-04-02",
      "2026-05-20", "2025-12-09", "2023-09-12"
    )),
    issue_date = as.Date(c(
      "2026-01-09", "2026-07-02", "2026-04-02",
      "2026-05-28", "2025-12-16", "2023-09-12"
    )),
    fiscal_year = c(NA, "2025/26", "2025/26", "2026/27", "2025/26", "2023/24"),
    fiscal_year_basis = c(
      "March through 1989; June from 1990",
      "July-June",
      "July-June",
      "June years",
      "June years",
      "June years"
    ),
    accounting_basis = c(
      "cash through 1993; accrual GAAP from 1994",
      "cash receipts and accrual revenue",
      "accrual PBE Standards",
      "accrual PBE Standards",
      "accrual PBE Standards",
      "accrual PBE Standards"
    ),
    consolidation_scope = c(
      "varies by series and regime",
      "unconsolidated history with Section III consolidation eliminations and totals",
      "whole-of-government consolidated",
      "core Crown and total Crown by series",
      "core Crown and total Crown by series",
      "core Crown and total Crown by series"
    ),
    actual_forecast = c(
      "actual",
      "actual",
      "actual and forecast comparison",
      "actual and forecast",
      "actual and forecast",
      "actual and forecast"
    ),
    forecast_vintage = c(NA, "BEFU 2026", "in-publication", "BEFU 2026", "HYEFU 2025", "PREFU 2023"),
    forecast_horizon = c(NA, NA, "FY2025/26", "2025 actual; 2026-2030 forecast", "2025 actual; 2026-2030 forecast", "2023 mixed forecast/unaudited fiscal actual; 2024-2027 forecast"),
    artifact = c("historical_series", "monthly_history", "financial_statements", "charts_data", "charts_data", "charts_data"),
    file_name = c(
      "fiscaltimeseries1972-2025-year-end25.xlsx",
      "tax-history-may26.xlsx",
      "fsgnz-8mths-feb25.xlsx",
      "befu26-charts-data.xlsx",
      "hyefu25-charts-data.xlsx",
      "prefu23-charts-data.xlsx"
    ),
    file_format = "xlsx",
    url = c(
      "https://www.treasury.govt.nz/sites/default/files/2026-01/fiscaltimeseries1972-2025-year-end25.xlsx",
      "https://www.treasury.govt.nz/sites/default/files/2026-07/tax-history-may26.xlsx",
      "https://www.treasury.govt.nz/sites/default/files/2026-04/fsgnz-8mths-feb25.xlsx",
      "https://www.treasury.govt.nz/sites/default/files/2026-05/befu26-charts-data.xlsx",
      "https://www.treasury.govt.nz/sites/default/files/2025-12/hyefu25-charts-data.xlsx",
      "https://www.treasury.govt.nz/sites/default/files/2023-09/prefu23-charts-data.xlsx"
    ),
    landing_url = c(
      "https://www.treasury.govt.nz/publications/information-release/data-fiscal-time-series-historical-fiscal-indicators",
      "https://www.treasury.govt.nz/publications/tax-outturn-data/tax-outturn-data-may-2026",
      "https://www.treasury.govt.nz/publications/month-end/interim-financial-statements-government-new-zealand-eight-months-ended-28-february-2026",
      "https://www.treasury.govt.nz/publications/efu/budget-economic-and-fiscal-update-2026",
      "https://www.treasury.govt.nz/publications/efu/half-year-economic-and-fiscal-update-2025",
      "https://www.treasury.govt.nz/publications/efu/pre-election-economic-and-fiscal-update-2023"
    ),
    expected_sha256 = c(
      "de59f9028a81a697ee66eea04861edfd8e2c3a7e472b3b8798d976951964f70f",
      "ef670282b6fd232b1f13d2b1ee20b4d74418a2626956a20d8b05385b377aae59",
      "d4fbd66a6aab4a6c47f2a222da951e7be749db5c3dbfb653eff3c99e544538f5",
      "521e7649b3814c8de2efa2e72502c8e35c6db7a94d6f84ebc22fd3fa9f7976d4",
      "311ef4b92be1bc7174f6d8a321167f7e03f80e01abf9c71cc60cac942c7115af",
      "bb02f20612a3da546d4df1adda8b80b85f72f989c4be47a510781293decf586a"
    ),
    terms_statement =
      "\u00a9 Crown Copyright, Attribution 4.0 International (CC BY 4.0)",
    terms_license = "CC BY 4.0",
    terms_attribution = "The Treasury",
    license_url = "https://creativecommons.org/licenses/by/4.0/",
    terms_url = terms_url,
    terms_verified_at = as.Date("2026-07-26"),
    revision_note = c(
      "Treasury updates the historical workbook and does not backdate all accounting-policy changes.",
      "Monthly revenue is estimated and revised; June results are independently audited.",
      "Each monthly publication is an immutable release snapshot; later releases supersede it.",
      "Forecast vintage is preserved; later EFUs do not revise this snapshot.",
      "Forecast vintage is preserved; release is superseded by BEFU 2026.",
      "Forecast vintage is preserved; release is superseded by later EFUs."
    ),
    stringsAsFactors = FALSE
  )
}

.parse_release_html <- function(path_or_html) {
  doc <- if (length(path_or_html) == 1L && file.exists(path_or_html)) {
    xml2::read_html(path_or_html)
  } else {
    xml2::read_html(paste(path_or_html, collapse = "\n"))
  }
  text <- .clean_text(xml2::xml_text(doc))
  heading <- xml2::xml_find_first(doc, "//h1")
  title <- if (inherits(heading, "xml_missing")) NA_character_ else .clean_text(xml2::xml_text(heading))
  extract_label <- function(label) {
    pattern <- sprintf("%s[[:space:]]*:? ?([^|]+)", label)
    hit <- regexec(pattern, text, ignore.case = TRUE)
    value <- regmatches(text, hit)[[1L]]
    if (length(value) < 2L) NA_character_ else trimws(value[[2L]])
  }
  status <- if (grepl("Status[[:space:]]*:? ?Current", text, ignore.case = TRUE)) {
    "current"
  } else if (grepl("Status[[:space:]]*:? ?Superseded", text, ignore.case = TRUE)) {
    "superseded"
  } else {
    NA_character_
  }
  dates <- regmatches(
    text,
    gregexpr(
      "(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday),? [0-9]{1,2} [A-Za-z]+ [0-9]{4}",
      text,
      perl = TRUE
    )
  )[[1L]]
  data.frame(
    observed_title = title,
    observed_status = status,
    observed_document_date = if (length(dates) >= 1L) as.Date(sub("^[^,]+,? ", "", dates[[1L]]), "%d %B %Y") else as.Date(NA),
    observed_issue_date = if (length(dates) >= 2L) as.Date(sub("^[^,]+,? ", "", dates[[2L]]), "%d %B %Y") else as.Date(NA),
    observed_terms = if (grepl("CC BY 4\\.0|Attribution 4\\.0 International", text, ignore.case = TRUE)) "CC BY 4.0" else NA_character_,
    stringsAsFactors = FALSE
  )
}

#' List supported New Zealand Treasury fiscal releases
#'
#' The default catalogue is static and byte-pinned. Supplying `page_html`
#' verifies one catalogue row against a saved official release page without
#' using the network.
#'
#' @param family Optional family: `hfi`, `tax_outturn`,
#'   `monthly_financials`, or `efu`.
#' @param status One of `all`, `current`, or `superseded`.
#' @param release_id Optional exact release identifier.
#' @param page_html Optional path to a saved official HTML page (or HTML text).
#'   `release_id` must identify the row being verified.
#' @return A data frame. When `page_html` is supplied, observed metadata
#'   columns are appended and `metadata_matches` is fail-closed.
#' @export
list_nz_fiscal_releases <- function(
    family = NULL,
    status = c("all", "current", "superseded"),
    release_id = NULL,
    page_html = NULL) {
  status <- match.arg(status)
  out <- .nz_catalogue()
  if (!is.null(family)) {
    unknown <- setdiff(family, unique(out$family))
    if (length(unknown)) {
      .abort("Unsupported release family: %s", paste(unknown, collapse = ", "))
    }
    out <- out[out$family %in% family, , drop = FALSE]
  }
  if (status != "all") {
    out <- out[out$status == status, , drop = FALSE]
  }
  if (!is.null(release_id)) {
    out <- out[out$release_id %in% release_id, , drop = FALSE]
  }
  if (!nrow(out)) {
    .abort("No release matches the requested catalogue filters.")
  }
  if (!is.null(page_html)) {
    if (nrow(out) != 1L) {
      .abort("page_html verification requires exactly one release_id.")
    }
    observed <- .parse_release_html(page_html)
    out <- cbind(out, observed)
    out$metadata_matches <- with(
      out,
      !is.na(observed_title) &&
        release_title == observed_title &&
        !is.na(observed_status) &&
        status == observed_status &&
        !is.na(observed_document_date) &&
        document_date == observed_document_date &&
        !is.na(observed_issue_date) &&
        issue_date == observed_issue_date &&
        !is.na(observed_terms) &&
        terms_license == observed_terms
    )
    if (!isTRUE(out$metadata_matches[[1L]])) {
      .abort(
        paste0(
          "Official release metadata drift for '%s'; title, status, ",
          "document date, issue date, and licence require catalogue review."
        ),
        out$release_id[[1L]]
      )
    }
  }
  row.names(out) <- NULL
  out
}

#' Return terms metadata for supported releases
#'
#' @param release_id Optional exact release identifier.
#' @return A de-duplicated data frame of per-release licensing metadata.
#' @export
nz_fiscal_terms <- function(release_id = NULL) {
  out <- .nz_catalogue()
  if (!is.null(release_id)) {
    out <- out[out$release_id %in% release_id, , drop = FALSE]
  }
  if (!nrow(out)) {
    .abort("Unknown release_id.")
  }
  unique(out[c(
    "release_id", "landing_url", "terms_statement", "terms_license",
    "terms_attribution", "license_url", "terms_url", "terms_verified_at"
  )])
}
