.new_nz_fiscal_data <- function(data, provenance) {
  row.names(data) <- NULL
  class(data) <- unique(c("nz_fiscal_data", class(data)))
  attr(data, "nz_fiscal_provenance") <- provenance
  data
}

#' Return provenance attached to parsed fiscal data
#'
#' @param x An object returned by a `read_nz_*()` function.
#' @return A named list describing the release, raw snapshot, parser, schema,
#'   terms, and revision policy.
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
#' @param x An object returned by a `read_nz_*()` function.
#' @param max_bytes Maximum snapshot size accepted into memory.
#' @return A raw vector whose SHA-256 is recorded in the object's provenance.
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
