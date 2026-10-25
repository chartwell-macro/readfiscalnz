.select_release <- function(family, release_id = NULL, revision = c("as_published", "latest")) {
  revision <- match.arg(revision)
  catalogue <- .nz_catalogue()
  catalogue <- catalogue[catalogue$family == family, , drop = FALSE]
  if (!nrow(catalogue)) {
    .abort("Unsupported release family '%s'.", family)
  }
  if (!is.null(release_id)) {
    selected <- catalogue[catalogue$release_id == release_id, , drop = FALSE]
    if (!nrow(selected)) {
      .abort("release_id '%s' is not a %s release.", release_id, family)
    }
  } else if (revision == "latest") {
    selected <- catalogue[which.max(catalogue$issue_date), , drop = FALSE]
  } else {
    current <- catalogue[catalogue$status == "current", , drop = FALSE]
    selected <- if (nrow(current)) current[which.max(current$issue_date), , drop = FALSE] else catalogue[which.max(catalogue$issue_date), , drop = FALSE]
  }
  selected[1L, , drop = FALSE]
}

.assert_file_signature <- function(path, extension) {
  size <- file.info(path)$size
  if (is.na(size) || size < 1L) {
    .abort("Retrieved artifact is empty.")
  }
  bytes <- readBin(path, what = "raw", n = min(8L, size))
  if (extension == "xlsx" && !identical(bytes[1:2], charToRaw("PK"))) {
    .abort("Retrieved artifact is not an XLSX/ZIP file (missing PK signature).")
  }
  if (extension == "csv" && any(bytes == as.raw(0L))) {
    .abort("Retrieved artifact is not a text CSV file.")
  }
  invisible(TRUE)
}

.raw_cache_files <- function(cache_dir) {
  raw_dir <- file.path(cache_dir, "raw")
  if (!dir.exists(raw_dir)) {
    return(character())
  }
  list.files(
    raw_dir,
    pattern = "^[a-f0-9]{64}\\.(xlsx|csv)$",
    full.names = TRUE,
    recursive = TRUE
  )
}

#' Prune the raw fiscal snapshot cache
#'
#' Prunes the hash-addressed raw snapshots below `file.path(cache_dir, "raw")`
#' in two passes. First, snapshots older than `max_age_days` are removed;
#' second, the oldest remaining snapshots are removed until both the file-count
#' and aggregate-byte limits are met. Only paths with a 64-character lowercase
#' SHA-256 name and an `.xlsx` or `.csv` extension are managed. Parsed results,
#' download staging files, and other files below `cache_dir` are not touched.
#' This function performs local file deletion only and never makes network
#' requests.
#'
#' The defaults are read from package options at call time. `cache_dir`
#' defaults to `getOption("readfiscalnz.cache_dir",`
#' `file.path(tools::R_user_dir("readfiscalnz", "cache"), "v1"))`.
#' `max_files`, `max_bytes`, and `max_age_days` default respectively to the
#' `readfiscalnz.raw_cache_files` (64), `readfiscalnz.raw_cache_bytes`
#' (250 * 1024^2 bytes), and `readfiscalnz.raw_cache_max_age_days` (180 days)
#' options. Invalid or missing values supplied through those options fall back
#' to these package defaults. The explicit arguments still allow a one-off
#' pruning policy, including `max_age_days = Inf` to disable age eviction.
#'
#' Age eviction and limit-based eviction both skip paths in `preserve`.
#' Preserve paths are normalised before comparison, so pass the full path to a
#' raw snapshot when a reader must continue to use it. Preserved files can make
#' the returned count or byte total exceed the requested limits; the limits are
#' therefore bounds on removable snapshots when `preserve` is non-empty. Within
#' the limit-based pass, files are considered in increasing modification-time
#' order; files with an unavailable modification time are considered last.
#'
#' @param cache_dir A single non-empty path naming the cache root used by the
#'   readers. The default is the package's managed user cache, not the current
#'   working directory. The directory need not already exist.
#' @param max_files A non-negative integer scalar giving the maximum number of
#'   managed raw snapshots to retain. `0` removes every non-preserved snapshot.
#' @param max_bytes A non-negative numeric scalar giving the maximum aggregate
#'   size, in bytes, of managed raw snapshots. `0` removes every non-preserved
#'   snapshot.
#' @param max_age_days A non-negative numeric scalar giving the maximum age,
#'   measured from each file's modification time, in days. `Inf` disables the
#'   age pass. A value of `0` treats files with an earlier modification time
#'   than the current time as expired.
#' @param preserve A character vector of raw snapshot paths to leave in place
#'   during both eviction passes. It defaults to `character()`; paths do not
#'   need to exist, but should identify snapshots below `cache_dir` when used.
#' @return Invisibly, a named list with exactly two fields: `files`, an integer
#'   count of managed snapshots remaining after pruning, and `bytes`, a numeric
#'   aggregate size in bytes. Both are zero when no matching snapshots exist.
#'   If files cannot be removed, they remain included in these totals.
#'
#' @section Validation and side effects:
#' `cache_dir` must be one non-empty path; `max_files` must be a non-negative
#' integer scalar; and `max_bytes` and `max_age_days` must be non-negative
#' numeric scalars. Invalid values cause an error. The function may unlink
#' matching snapshot files and does not provide a rollback if a later deletion
#' fails. It does not create a cache directory or alter the source artifacts
#' from which snapshots were made.
#'
#' @seealso [read_nz_hfi()], [read_nz_tax_outturn()],
#'   [read_nz_monthly_financials()], and [read_nz_efu()] for readers that
#'   populate this raw cache; [nz_fiscal_raw_bytes()] for reading a verified
#'   snapshot.
#'
#' @examples
#' cache <- tempfile("readfiscalnz-empty-cache-")
#' result <- prune_nz_fiscal_cache(cache)
#' stopifnot(identical(names(result), c("files", "bytes")))
#' stopifnot(identical(result$files, 0L), identical(result$bytes, 0))
#' @export
prune_nz_fiscal_cache <- function(
    cache_dir = .nz_cache_dir(),
    max_files = .nz_raw_cache_limits()$max_files,
    max_bytes = .nz_raw_cache_limits()$max_bytes,
    max_age_days = .nz_raw_cache_limits()$max_age_days,
    preserve = character()) {
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      is.na(cache_dir) || !nzchar(cache_dir)) {
    .abort("cache_dir must be one non-empty path.")
  }
  if (!is.numeric(max_files) || length(max_files) != 1L ||
      is.na(max_files) || max_files < 0 ||
      max_files != as.integer(max_files)) {
    .abort("max_files must be a non-negative integer.")
  }
  if (!is.numeric(max_bytes) || length(max_bytes) != 1L ||
      is.na(max_bytes) || max_bytes < 0) {
    .abort("max_bytes must be non-negative.")
  }
  if (!is.numeric(max_age_days) || length(max_age_days) != 1L ||
      is.na(max_age_days) || max_age_days < 0) {
    .abort("max_age_days must be non-negative.")
  }
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  preserve <- normalizePath(preserve, winslash = "/", mustWork = FALSE)
  files <- .raw_cache_files(cache_dir)
  if (!length(files)) {
    return(invisible(list(files = 0L, bytes = 0)))
  }
  info <- file.info(files)
  removable <- !files %in% preserve
  if (is.finite(max_age_days)) {
    cutoff <- Sys.time() - max_age_days * 86400
    expired <- removable & !is.na(info$mtime) & info$mtime < cutoff
    if (any(expired)) {
      unlink(files[expired])
    }
  }
  files <- .raw_cache_files(cache_dir)
  if (!length(files)) {
    return(invisible(list(files = 0L, bytes = 0)))
  }
  info <- file.info(files)
  total <- sum(info$size, na.rm = TRUE)
  count <- length(files)
  order_oldest <- order(info$mtime, na.last = TRUE)
  for (index in order_oldest) {
    if (count <= max_files && total <= max_bytes) {
      break
    }
    path <- files[[index]]
    if (path %in% preserve) {
      next
    }
    size <- info$size[[index]]
    if (unlink(path) == 0L) {
      count <- count - 1L
      total <- total - size
    }
  }
  invisible(list(files = count, bytes = total))
}

.publish_snapshot <- function(
    staged,
    target,
    sha256,
    rename_fn = base::file.rename) {
  published <- rename_fn(staged, target)
  if (!published) {
    if (!file.exists(target)) {
      .abort("Could not atomically publish raw snapshot '%s'.", target)
    }
    winner_hash <- digest::digest(
      target, algo = "sha256", file = TRUE, serialize = FALSE
    )
    if (!identical(winner_hash, sha256)) {
      if (unlink(target) != 0L) {
        .abort("Competing cache target was corrupt and could not be replaced.")
      }
      republished <- rename_fn(staged, target)
      if (!republished && !file.exists(target)) {
        .abort("Could not replace a corrupt competing cache target.")
      }
    }
  }
  final_hash <- digest::digest(
    target, algo = "sha256", file = TRUE, serialize = FALSE
  )
  if (!identical(final_hash, sha256)) {
    unlink(target)
    .abort("Published raw snapshot failed final SHA-256 verification.")
  }
  invisible(target)
}

.archive_snapshot <- function(
    path,
    sha256,
    extension,
    cache_dir,
    namespace = NULL,
    rename_fn = base::file.rename) {
  limits <- .nz_raw_cache_limits()
  source_size <- file.info(path)$size
  if (is.na(source_size) || source_size > limits$max_bytes) {
    .abort(
      "Artifact is %s bytes and exceeds the raw cache byte bound of %s.",
      source_size,
      limits$max_bytes
    )
  }
  if (!is.null(namespace)) {
    if (
      !is.character(namespace) ||
        length(namespace) != 1L ||
        is.na(namespace) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", namespace)
    ) {
      .abort("Raw snapshot namespace is invalid.")
    }
  }
  raw_dir <- if (is.null(namespace)) {
    file.path(cache_dir, "raw", substr(sha256, 1L, 2L))
  } else {
    file.path(cache_dir, "raw", namespace, substr(sha256, 1L, 2L))
  }
  if (!dir.exists(raw_dir) && !dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)) {
    .abort("Cannot create raw snapshot directory '%s'.", raw_dir)
  }
  target <- file.path(raw_dir, paste0(sha256, ".", extension))
  if (file.exists(target)) {
    existing_hash <- digest::digest(target, algo = "sha256", file = TRUE, serialize = FALSE)
    if (!identical(existing_hash, sha256)) {
      if (unlink(target) != 0L) {
        .abort("Corrupt cache target could not be replaced at '%s'.", target)
      }
    } else {
      Sys.setFileTime(target, Sys.time())
      prune_nz_fiscal_cache(
        cache_dir,
        limits$max_files,
        limits$max_bytes,
        limits$max_age_days,
        preserve = target
      )
      return(normalizePath(target, winslash = "/", mustWork = TRUE))
    }
  }
  staged <- tempfile(pattern = ".snapshot-", tmpdir = raw_dir, fileext = paste0(".", extension, ".part"))
  on.exit(unlink(staged), add = TRUE)
  if (!file.copy(path, staged, overwrite = FALSE, copy.mode = TRUE)) {
    .abort("Could not stage raw snapshot in '%s'.", raw_dir)
  }
  staged_hash <- digest::digest(staged, algo = "sha256", file = TRUE, serialize = FALSE)
  if (!identical(staged_hash, sha256)) {
    .abort("Raw snapshot changed during staging.")
  }
  .publish_snapshot(staged, target, sha256, rename_fn)
  Sys.setFileTime(target, Sys.time())
  prune_nz_fiscal_cache(
    cache_dir,
    limits$max_files,
    limits$max_bytes,
    limits$max_age_days,
    preserve = target
  )
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

.is_official_treasury_url <- function(url) {
  is.character(url) &&
    length(url) == 1L &&
    !is.na(url) &&
    grepl(
      "^https://(www\\.)?treasury\\.govt\\.nz(?:/|$)",
      url,
      ignore.case = TRUE,
      perl = TRUE
    )
}

.resolved_response_url <- function(response, requested_url) {
  candidates <- c(
    response$resolved_url %||% NA_character_,
    response$url %||% NA_character_,
    response$url_effective %||% NA_character_,
    requested_url
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  candidates[[1L]]
}

.download_to_staged <- function(
    url,
    staged,
    timeout_sec,
    max_bytes,
    max_attempts = .nz_download_attempts(),
    fetch_fn = curl::curl_fetch_disk) {
  if (!.is_official_treasury_url(url)) {
    .abort("Official artifact URL must use HTTPS on the Treasury host.")
  }
  if (!is.numeric(max_attempts) || length(max_attempts) != 1L ||
      is.na(max_attempts) || max_attempts < 1L ||
      max_attempts > 3L || max_attempts != as.integer(max_attempts)) {
    .abort("max_attempts must be an integer from 1 to 3.")
  }
  retry_status <- c(408L, 429L, 500L, 502L, 503L, 504L)
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    unlink(staged)
    handle <- curl::new_handle(
      timeout = timeout_sec,
      connecttimeout = min(timeout_sec, 10),
      followlocation = TRUE,
      maxredirs = 5L,
      maxfilesize_large = max_bytes,
      protocols_str = "https",
      redir_protocols_str = "https",
      useragent = "readfiscalnz/0.1.0 (+https://www.treasury.govt.nz/)"
    )
    response <- tryCatch(
      fetch_fn(url, staged, handle = handle),
      error = function(error) error
    )
    if (inherits(response, "error")) {
      last_error <- conditionMessage(response)
      if (attempt < max_attempts) {
        next
      }
      unlink(staged)
      .abort(
        "Official artifact transport failed after %s attempt(s): %s",
        max_attempts,
        last_error
      )
    }
    status <- as.integer(response$status_code %||% NA_integer_)
    if (!is.na(status) && status >= 200L && status < 300L) {
      resolved_url <- .resolved_response_url(response, url)
      if (!.is_official_treasury_url(resolved_url)) {
        unlink(staged)
        .abort(
          "Official artifact redirected outside the Treasury HTTPS host."
        )
      }
      response$resolved_url <- resolved_url
      return(invisible(response))
    }
    last_error <- sprintf("HTTP %s", status)
    retryable <- !is.na(status) && status %in% retry_status
    if (!retryable || attempt == max_attempts) {
      unlink(staged)
      .abort(
        "Official artifact transport failed after %s attempt(s): %s",
        attempt,
        last_error
      )
    }
  }
  unlink(staged)
  .abort("Official artifact transport failed: %s", last_error)
}

.download_release <- function(release, cache_dir, timeout_sec, max_bytes, accept_terms) {
  if (!isTRUE(accept_terms)) {
    .abort(
      paste0(
        "Live retrieval requires accept_terms = TRUE after reviewing ",
        "nz_fiscal_terms('%s')."
      ),
      release$release_id
    )
  }
  extension <- tolower(release$file_format)
  expected <- release$expected_sha256
  expected_path <- file.path(
    cache_dir, "raw", release$release_id, substr(expected, 1L, 2L),
    paste0(expected, ".", extension)
  )
  if (file.exists(expected_path)) {
    hash <- digest::digest(expected_path, algo = "sha256", file = TRUE, serialize = FALSE)
    if (identical(hash, expected)) {
      Sys.setFileTime(expected_path, Sys.time())
      limits <- .nz_raw_cache_limits()
      prune_nz_fiscal_cache(
        cache_dir,
        limits$max_files,
        limits$max_bytes,
        limits$max_age_days,
        preserve = expected_path
      )
      return(list(
        path = normalizePath(expected_path, winslash = "/", mustWork = TRUE),
        resolved_url = release$url
      ))
    }
    if (unlink(expected_path) != 0L) {
      .abort("Corrupt pinned cache entry could not be replaced.")
    }
  }
  download_dir <- file.path(cache_dir, "downloads")
  if (!dir.exists(download_dir) && !dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)) {
    .abort("Cannot create download directory '%s'.", download_dir)
  }
  staged <- tempfile(pattern = ".download-", tmpdir = download_dir, fileext = paste0(".", extension, ".part"))
  on.exit(unlink(staged), add = TRUE)
  response <- .download_to_staged(
    release$url, staged, timeout_sec, max_bytes
  )
  size <- file.info(staged)$size
  if (is.na(size) || size < 1L || size > max_bytes) {
    .abort(
      "Retrieved artifact size (%s) is outside 1..%s bytes.",
      size,
      max_bytes
    )
  }
  .assert_file_signature(staged, extension)
  hash <- digest::digest(staged, algo = "sha256", file = TRUE, serialize = FALSE)
  if (!identical(hash, expected)) {
    .abort(
      paste0(
        "Official artifact SHA-256 drifted for %s. Expected %s, received %s. ",
        "Catalogue review is required before parsing."
      ),
      release$release_id,
      expected,
      hash
    )
  }
  list(
    path = .archive_snapshot(
      staged,
      hash,
      extension,
      cache_dir,
      namespace = release$release_id
    ),
    resolved_url = .resolved_response_url(response, release$url)
  )
}

.resolve_artifact <- function(
    family,
    release_id,
    file,
    revision,
    cache_dir,
    timeout_sec,
    max_bytes,
    accept_terms) {
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      is.na(cache_dir) || !nzchar(cache_dir)) {
    .abort("cache_dir must be one non-empty path.")
  }
  if (!is.numeric(timeout_sec) || length(timeout_sec) != 1L ||
      is.na(timeout_sec) || timeout_sec <= 0) {
    .abort("timeout_sec must be positive.")
  }
  if (!is.numeric(max_bytes) || length(max_bytes) != 1L ||
      is.na(max_bytes) || max_bytes <= 0) {
    .abort("max_bytes must be positive.")
  }
  release <- .select_release(family, release_id, revision)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  retrieved_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  if (is.null(file)) {
    downloaded <- .download_release(
      release, cache_dir, timeout_sec, max_bytes, accept_terms
    )
    snapshot <- downloaded$path
    resolved_url <- downloaded$resolved_url
    source_kind <- "official_url"
    source_location <- release$url
  } else {
    if (length(file) != 1L || !file.exists(file)) {
      .abort("file must identify one existing XLSX or CSV artifact.")
    }
    size <- file.info(file)$size
    if (is.na(size) || size < 1L || size > max_bytes) {
      .abort(
        "Local artifact size (%s) is outside 1..%s bytes.",
        size,
        max_bytes
      )
    }
    extension <- tolower(tools::file_ext(file))
    if (!extension %in% c("xlsx", "csv")) {
      .abort("Unsupported local artifact extension '.%s'.", extension)
    }
    .assert_file_signature(file, extension)
    hash <- digest::digest(file, algo = "sha256", file = TRUE, serialize = FALSE)
    snapshot <- .archive_snapshot(
      file,
      hash,
      extension,
      cache_dir,
      namespace = release$release_id
    )
    resolved_url <- NA_character_
    source_kind <- "local_file"
    source_location <- normalizePath(file, winslash = "/", mustWork = TRUE)
  }
  extension <- tolower(tools::file_ext(snapshot))
  hash <- digest::digest(snapshot, algo = "sha256", file = TRUE, serialize = FALSE)
  list(
    release = release,
    path = snapshot,
    extension = extension,
    provenance = list(
      provider = "New Zealand Treasury",
      family = family,
      release_id = release$release_id,
      release_type = release$release_type,
      release_status = release$status,
      document_date = release$document_date,
      issue_date = release$issue_date,
      fiscal_year = release$fiscal_year,
      fiscal_year_basis = release$fiscal_year_basis,
      accounting_basis = release$accounting_basis,
      consolidation_scope = release$consolidation_scope,
      actual_forecast = release$actual_forecast,
      forecast_vintage = release$forecast_vintage,
      forecast_horizon = release$forecast_horizon,
      landing_url = release$landing_url,
      original_url = release$url,
      resolved_url = resolved_url,
      original_file = release$file_name,
      source_kind = source_kind,
      source_location = source_location,
      raw_snapshot_path = snapshot,
      raw_size_bytes = unname(file.info(snapshot)$size),
      raw_sha256 = hash,
      catalogue_sha256 = release$expected_sha256,
      retrieved_at = retrieved_at,
      terms_statement = release$terms_statement,
      terms_license = release$terms_license,
      terms_attribution = release$terms_attribution,
      license_url = release$license_url,
      terms_url = release$terms_url,
      terms_verified_at = release$terms_verified_at,
      revision_policy = revision,
      revision_note = release$revision_note
    )
  )
}

.finish_result <- function(data, artifact, parser_family, parser_options = list()) {
  provenance <- artifact$provenance
  provenance$parser_version <- unname(.nz_parser_versions[[parser_family]])
  provenance$schema_version <- unname(.nz_schema_versions[[parser_family]])
  provenance$parser_options <- parser_options
  .new_nz_fiscal_data(data, provenance)
}
