.new_nz_fiscal_data <- function(data, provenance) {
  row.names(data) <- NULL
  class(data) <- unique(c("nz_fiscal_data", class(data)))
  attr(data, "nz_fiscal_provenance") <- provenance
  data
}

#' Return provenance attached to parsed fiscal data
#'
#' Every `read_nz_*()` reader returns an `nz_fiscal_data` data frame and
#' attaches a named provenance list to it. This function returns that list
#' without changing the data. The provenance is the audit trail for the
#' selected Treasury release and the exact bytes parsed, rather than a record
#' inferred from the parsed values. Use [nz_fiscal_raw_bytes()] when the
#' preserved source bytes themselves are required.
#'
#' The list contains the following fields:
#'
#' * `provider`, `family`, `release_id`, `release_type`, `release_status`,
#'   `document_date`, `issue_date`, and `fiscal_year` identify the Treasury
#'   publication.
#' * `fiscal_year_basis`, `accounting_basis`, `consolidation_scope`,
#'   `actual_forecast`, `forecast_vintage`, and `forecast_horizon` describe
#'   the release's fiscal and forecast semantics.
#' * `landing_url`, `original_url`, `resolved_url`, `original_file`,
#'   `source_kind`, and `source_location` identify the official landing page,
#'   requested artifact, final HTTPS URL (when retrieved), and input route.
#'   `source_kind` is `"official_url"` for a verified retrieval or
#'   `"local_file"` for a local replay.
#' * `raw_snapshot_path`, `raw_size_bytes`, and `raw_sha256` identify the
#'   content-addressed snapshot. `catalogue_sha256` records the expected
#'   SHA-256 for the catalogue's official artifact; for a local fixture or
#'   replay it may intentionally differ from `raw_sha256`. `retrieved_at`
#'   records when this artifact was resolved.
#' * `terms_statement`, `terms_license`, `terms_attribution`, `license_url`,
#'   `terms_url`, and `terms_verified_at` record the release terms and the
#'   package's terms verification metadata.
#' * `revision_policy` and `revision_note` record whether the selected
#'   publication was `"as_published"` or selected by the `"latest"` policy.
#'   `parser_version`, `schema_version`, and `parser_options` identify the
#'   parser and options used to create the returned rows.
#'
#' Dates are `Date` values where supplied by the release catalogue; unavailable
#' catalogue fields are represented by `NA`. The `raw_sha256` and
#' `catalogue_sha256` values are lowercase hexadecimal SHA-256 strings. The
#' returned object is an ordinary named `list`; it is not a data frame and it
#' does not perform a network request or write to the cache. If `x` does not
#' carry the provenance attribute (for example, an unrelated data frame), the
#' function stops with `Object has no readfiscalnz provenance.`
#'
#' @param x An `nz_fiscal_data` object returned by one of
#'   [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()], or [read_nz_efu()].
#' @return A named list containing the release identity, fiscal semantics,
#'   source and cache locations, byte hashes and size, licensing metadata,
#'   revision policy, parser and schema versions, and parser options described
#'   above. The input data frame is not modified.
#' @seealso [nz_fiscal_raw_bytes()] to verify or replay the archived bytes,
#'   [nz_fiscal_terms()] for release licensing metadata, and
#'   [prune_nz_fiscal_cache()] for bounded cache maintenance.
#' @family provenance-tools
#' @examples
#' \{
#' hfi_file <- system.file("extdata", "hfi.csv", package = "readfiscalnz")
#' if (!nzchar(hfi_file)) {
#'   stop("The packaged hfi.csv fixture is unavailable.")
#' }
#' local({
#'   cache <- tempfile("readfiscalnz-provenance-")
#'   dir.create(cache)
#'   on.exit(unlink(cache, recursive = TRUE), add = TRUE)
#'   hfi <- read_nz_hfi(file = hfi_file, cache_dir = cache)
#'   provenance <- nz_fiscal_provenance(hfi)
#'   provenance[c("family", "release_id", "raw_sha256", "parser_version", "schema_version")]
#' })
#' \}
#' @export
nz_fiscal_provenance <- function(x) {
  out <- attr(x, "nz_fiscal_provenance", exact = TRUE)
  if (is.null(out)) {
    .abort("Object has no readfiscalnz provenance.")
  }
  out
}

#' Read exact bytes from a preserved raw snapshot
#'
#' Readers copy each accepted local or official artifact into a
#' content-addressed cache and record its path and SHA-256 in provenance. This
#' function verifies that snapshot before reading it, then returns the exact
#' bytes that were available to the parser. It is useful for reproducibility
#' checks, archival replay, and comparing a parsed object with its source.
#'
#' The snapshot must still exist, must not exceed `max_bytes`, and must hash to
#' `nz_fiscal_provenance(x)$raw_sha256`. A missing, oversized, or modified
#' snapshot is rejected rather than silently read. The function never downloads
#' a replacement and never changes the cache; rerun the corresponding reader if
#' a cache entry needs to be repaired.
#' If `x` has no provenance, the same missing-provenance error as
#' [nz_fiscal_provenance()] is raised before any file is read.
#'
#' @param x An `nz_fiscal_data` object returned by one of
#'   [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()], or [read_nz_efu()].
#' @param max_bytes Numeric maximum number of bytes to read into memory. The
#'   default is 25 MiB, matching the readers' default artifact limit.
#' @return A `raw` vector of length `raw_size_bytes` from
#'   [nz_fiscal_provenance()] whose SHA-256 is exactly `raw_sha256`. No class,
#'   names, or text encoding are added to the bytes.
#' @seealso [nz_fiscal_provenance()] for the recorded path and hash, and the
#'   reader functions for rebuilding a missing or corrupt snapshot.
#' @family provenance-tools
#' @examples
#' \{
#' hfi_file <- system.file("extdata", "hfi.csv", package = "readfiscalnz")
#' if (!nzchar(hfi_file)) {
#'   stop("The packaged hfi.csv fixture is unavailable.")
#' }
#' local({
#'   cache <- tempfile("readfiscalnz-raw-")
#'   dir.create(cache)
#'   on.exit(unlink(cache, recursive = TRUE), add = TRUE)
#'   hfi <- read_nz_hfi(file = hfi_file, cache_dir = cache)
#'   bytes <- nz_fiscal_raw_bytes(hfi)
#'   provenance <- nz_fiscal_provenance(hfi)
#'   stopifnot(
#'     length(bytes) == provenance$raw_size_bytes,
#'     identical(
#'       digest::digest(bytes, algo = "sha256", serialize = FALSE),
#'       provenance$raw_sha256
#'     )
#'   )
#' })
#' \}
#' @export
nz_fiscal_raw_bytes <- function(x, max_bytes = 25 * 1024^2) {
  provenance <- nz_fiscal_provenance(x)
  path <- provenance$raw_snapshot_path
  if (is.null(path) || !file.exists(path)) {
    .abort("Raw snapshot is unavailable at '%s'.", path %||% "<missing>")
  }
  size <- file.info(path)$size
  if (is.na(size) || size > max_bytes) {
    .abort(
      "Raw snapshot is %s bytes; max_bytes is %s.",
      format(size, scientific = FALSE),
      format(max_bytes, scientific = FALSE)
    )
  }
  current_hash <- digest::digest(
    path, algo = "sha256", file = TRUE, serialize = FALSE
  )
  if (!identical(current_hash, provenance$raw_sha256)) {
    .abort(
      paste0(
        "Raw snapshot SHA-256 no longer matches provenance; ",
        "the cache entry is corrupt."
      )
    )
  }
  readBin(path, what = "raw", n = size)
}

#' Print a `nz_fiscal_data` object with source provenance
#'
#' Prints the data frame using the next `data.frame` method, followed by a
#' one-line source summary when provenance is attached. The summary includes
#' the Treasury family, exact release identifier, and the first 12 hexadecimal
#' characters of the raw snapshot SHA-256. The full hash, source URL, terms,
#' parser version, and schema version remain available from
#' [nz_fiscal_provenance()].
#'
#' Printing is observational: it does not retrieve data, update a parsed or
#' raw cache, or alter `x`. If a data frame has the class but no provenance
#' attribute, only the inherited data-frame output is printed.
#'
#' @param x An `nz_fiscal_data` data frame returned by one of the
#'   [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()], or [read_nz_efu()] readers.
#' @param ... Additional arguments passed to the inherited data-frame print
#'   method.
#' @return Invisibly returns `x` unchanged, including its
#'   `nz_fiscal_data`/`data.frame` class and `nz_fiscal_provenance` attribute.
#' @seealso [nz_fiscal_provenance()] for the complete source record.
#' @family provenance-tools
#' @examples
#' \{
#' hfi_file <- system.file("extdata", "hfi.csv", package = "readfiscalnz")
#' if (!nzchar(hfi_file)) {
#'   stop("The packaged hfi.csv fixture is unavailable.")
#' }
#' local({
#'   cache <- tempfile("readfiscalnz-print-")
#'   dir.create(cache)
#'   on.exit(unlink(cache, recursive = TRUE), add = TRUE)
#'   hfi <- read_nz_hfi(file = hfi_file, cache_dir = cache)
#'   print(hfi)
#' })
#' \}
#' @method print nz_fiscal_data
#' @export
print.nz_fiscal_data <- function(x, ...) {
  provenance <- attr(x, "nz_fiscal_provenance", exact = TRUE)
  NextMethod()
  if (!is.null(provenance)) {
    cat(
      sprintf(
        "\nSource: %s | release: %s | SHA-256: %s\n",
        provenance$family,
        provenance$release_id,
        substr(provenance$raw_sha256, 1L, 12L)
      )
    )
  }
  invisible(x)
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) y else x
}
