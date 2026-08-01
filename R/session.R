# Session-level model resolution: every verb accepts a loaded model, a pinned
# model name, or nothing (the default model). Names are loaded lazily and kept
# in a per-session cache so repeated calls never rebuild the runtime. Nothing
# is ever downloaded without an explicit yes — interactively through a
# one-time prompt, non-interactively through options(sbert.download = TRUE)
# or a prior sbert_model_download() call.

.sbert_session <- new.env(parent = emptyenv())

clear_sbert_session <- function() {
  rm(list = ls(.sbert_session), envir = .sbert_session)
  invisible(NULL)
}

ensure_sbert_model_installed <- function(name, cache_dir) {
  if (all(sbert_model_status(cache_dir, model = name)$valid)) {
    return(invisible(name))
  }
  manifest <- resolve_sbert_manifest(name)
  size_mb <- sum(manifest$artifacts$bytes) / 1e6
  if (interactive()) {
    answer <- utils::askYesNo(
      sprintf(
        "Model '%s' is not installed. Download it now? (%.1f MB, %s, SHA-256 verified)",
        name,
        size_mb,
        manifest$license
      ),
      default = FALSE
    )
    if (isTRUE(answer)) {
      sbert_model_download(name, cache_dir)
      return(invisible(name))
    }
    stop(
      sprintf("Download of '%s' declined. Nothing was downloaded.", name),
      call. = FALSE
    )
  }
  if (isTRUE(getOption("sbert.download"))) {
    sbert_model_download(name, cache_dir, quiet = TRUE)
    return(invisible(name))
  }
  stop(
    sprintf(
      paste0(
        "Model '%s' is not installed. Run sbert_model_download(\"%s\") once, ",
        "or set options(sbert.download = TRUE) to allow this script to ",
        "download it."
      ),
      name,
      name
    ),
    call. = FALSE
  )
}

# Turn NULL (default model), a pinned model name, or a loaded model into a
# loaded model, reusing the session cache for names.
resolve_sbert_model <- function(model = NULL, cache_dir = sbert_cache_dir()) {
  if (inherits(model, "sbert_model")) {
    return(model)
  }
  if (is.null(model)) {
    model <- .sbert_default_model
  }
  stopifnot(is.character(model), length(model) == 1L, !is.na(model))
  name <- resolve_sbert_manifest(model)$short_name
  cached <- get0(name, envir = .sbert_session, inherits = FALSE)
  if (!is.null(cached)) {
    return(cached)
  }
  ensure_sbert_model_installed(name, cache_dir)
  loaded <- sbert_load_model(name, cache_dir = cache_dir)
  assign(name, loaded, envir = .sbert_session)
  loaded
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(sprintf(
    paste0(
      "sbert %s - sentence embeddings without Python.\n",
      "  %d pinned models: sbert_models()  |  default: %s\n",
      "  sbert_encode(text) just works; the first use of a model offers a\n",
      "  one-time SHA-256-verified download (never without asking)."
    ),
    utils::packageVersion(pkgname),
    length(.sbert_registry),
    .sbert_default_model
  ))
}
