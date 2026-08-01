#' Locate the sbert Model Cache
#'
#' @return A length-one character vector containing the platform-appropriate
#'   user cache directory.
#' @export
#' @examples
#' sbert_cache_dir()
sbert_cache_dir <- function() {
  tools::R_user_dir("sbert", which = "cache")
}

#' Inspect an Installed Model
#'
#' Reports whether each required artifact exists and passes its pinned byte-size
#' and SHA-256 checks.
#'
#' @param cache_dir Cache root returned by [sbert_cache_dir()].
#' @param model Name of a pinned model listed by [sbert_models()].
#' @return A data frame with one row per required artifact.
#' @export
#' @examples
#' sbert_model_status(tempdir())
sbert_model_status <- function(
  cache_dir = sbert_cache_dir(),
  model = "all-MiniLM-L6-v2"
) {
  stopifnot(
    is.character(cache_dir),
    length(cache_dir) == 1L,
    !is.na(cache_dir),
    nzchar(cache_dir)
  )
  manifest <- resolve_sbert_manifest(model)

  paths <- sbert_artifact_paths(cache_dir, manifest)
  exists <- file.exists(paths)
  actual_bytes <- vapply(
    paths,
    function(path) {
      if (file.exists(path)) unname(file.info(path)$size) else NA_real_
    },
    numeric(1)
  )

  data.frame(
    file = manifest$artifacts$file,
    path = paths,
    exists = exists,
    valid = validate_sbert_cache(cache_dir, manifest),
    expected_bytes = manifest$artifacts$bytes,
    actual_bytes = actual_bytes,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

download_sbert_artifact <- function(
  url,
  destination,
  expected_bytes,
  expected_sha256,
  quiet
) {
  stopifnot(
    is.character(url),
    length(url) == 1L,
    is.character(destination),
    length(destination) == 1L,
    is.numeric(expected_bytes),
    length(expected_bytes) == 1L,
    is.character(expected_sha256),
    length(expected_sha256) == 1L,
    is.logical(quiet),
    length(quiet) == 1L,
    !is.na(quiet)
  )

  destination_directory <- dirname(destination)
  dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
  temporary_path <- tempfile(
    pattern = paste0(basename(destination), "-"),
    tmpdir = destination_directory
  )
  on.exit(unlink(temporary_path, force = TRUE), add = TRUE)

  tryCatch(
    utils::download.file(
      url = url,
      destfile = temporary_path,
      mode = "wb",
      quiet = quiet
    ),
    error = function(error_condition) {
      stop(
        sprintf(
          "Could not download '%s': %s",
          basename(destination),
          conditionMessage(error_condition)
        ),
        call. = FALSE
      )
    }
  )

  if (!validate_sbert_artifact(
    temporary_path,
    expected_bytes,
    expected_sha256
  )) {
    stop(
      sprintf(
        "Downloaded artifact '%s' failed size or SHA-256 validation.",
        basename(destination)
      ),
      call. = FALSE
    )
  }

  if (file.exists(destination)) {
    unlink(destination, force = TRUE)
  }
  if (!file.rename(temporary_path, destination)) {
    stop(
      sprintf("Could not move '%s' into the model cache.", basename(destination)),
      call. = FALSE
    )
  }

  invisible(destination)
}

#' Download a Pinned Sentence-BERT Model
#'
#' Downloads the official ONNX graph and tokenizer of a pinned model only
#' after this function is called. Files are locked to an immutable repository
#' revision and checked against package-controlled SHA-256 values. See
#' [sbert_models()] for the available models; the default remains
#' `all-MiniLM-L6-v2`.
#'
#' @param model Name of a pinned model listed by [sbert_models()].
#' @param cache_dir Cache root returned by [sbert_cache_dir()].
#' @param quiet Whether to suppress download progress.
#' @param timeout Minimum download timeout in seconds.
#' @return Invisibly, the model directory.
#' @export
#' @examplesIf interactive()
#' sbert_model_download()
sbert_model_download <- function(
  model = "all-MiniLM-L6-v2",
  cache_dir = sbert_cache_dir(),
  quiet = FALSE,
  timeout = 600
) {
  stopifnot(
    is.character(cache_dir),
    length(cache_dir) == 1L,
    !is.na(cache_dir),
    nzchar(cache_dir),
    is.logical(quiet),
    length(quiet) == 1L,
    !is.na(quiet),
    is.numeric(timeout),
    length(timeout) == 1L,
    is.finite(timeout),
    timeout > 0
  )
  manifest <- resolve_sbert_manifest(model)

  model_directory <- sbert_model_directory(cache_dir, manifest)
  dir.create(model_directory, recursive = TRUE, showWarnings = FALSE)

  if (!quiet) {
    message(sprintf(
      paste0(
        "Downloading %s at revision %s (Apache-2.0; %.1f MB total) ",
        "to '%s'."
      ),
      manifest$id,
      manifest$revision,
      sum(manifest$artifacts$bytes) / 1e6,
      model_directory
    ))
  }

  old_timeout <- getOption("timeout")
  options(timeout = max(as.numeric(old_timeout), timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  artifacts <- manifest$artifacts
  paths <- sbert_artifact_paths(cache_dir, manifest)
  urls <- sbert_artifact_urls(manifest)
  valid <- validate_sbert_cache(cache_dir, manifest)
  missing_indices <- which(!valid)

  invisible(lapply(
    missing_indices,
    function(index) {
      download_sbert_artifact(
        url = urls[[index]],
        destination = paths[[index]],
        expected_bytes = artifacts$bytes[[index]],
        expected_sha256 = artifacts$sha256[[index]],
        quiet = quiet
      )
    }
  ))

  if (!all(validate_sbert_cache(cache_dir, manifest))) {
    stop("The model cache is incomplete or invalid after download.", call. = FALSE)
  }

  invisible(model_directory)
}

#' Measure the sbert Cache
#'
#' @param cache_dir Cache root returned by [sbert_cache_dir()].
#' @return The total number of bytes currently used by regular files beneath
#'   the cache root.
#' @export
#' @examples
#' sbert_cache_size(tempdir())
sbert_cache_size <- function(cache_dir = sbert_cache_dir()) {
  stopifnot(
    is.character(cache_dir),
    length(cache_dir) == 1L,
    !is.na(cache_dir),
    nzchar(cache_dir)
  )

  if (!dir.exists(cache_dir)) {
    return(0)
  }

  cache_files <- list.files(
    cache_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (length(cache_files) == 0L) {
    return(0)
  }

  file_information <- file.info(cache_files)
  sum(file_information$size[!file_information$isdir], na.rm = TRUE)
}

#' Remove an Installed Model from the Cache
#'
#' @param cache_dir Cache root returned by [sbert_cache_dir()].
#' @param model Name of a pinned model listed by [sbert_models()].
#' @return Invisibly, whether the model directory no longer exists.
#' @export
#' @examples
#' cache <- file.path(tempdir(), "sbert-example-cache")
#' sbert_model_remove(cache)
sbert_model_remove <- function(
  cache_dir = sbert_cache_dir(),
  model = "all-MiniLM-L6-v2"
) {
  stopifnot(
    is.character(cache_dir),
    length(cache_dir) == 1L,
    !is.na(cache_dir),
    nzchar(cache_dir)
  )
  manifest <- resolve_sbert_manifest(model)

  model_directory <- sbert_model_directory(cache_dir, manifest)
  if (dir.exists(model_directory)) {
    unlink(model_directory, recursive = TRUE, force = TRUE)
  }

  invisible(!dir.exists(model_directory))
}
