#' Load a Pinned Sentence-BERT Model
#'
#' Loads already-downloaded model artifacts. The function never downloads
#' model files or native libraries. See [sbert_models()] for the available
#' pinned models.
#'
#' @param model Name of a pinned model listed by [sbert_models()].
#' @param cache_dir Cache root returned by [sbert_cache_dir()].
#' @param backend ONNX execution backend accepted by [onnxr::onnx_model()].
#' @param threads Positive number of inference threads.
#' @param verify Whether to verify file sizes and SHA-256 hashes before loading.
#' @return An object of class `sbert_model`.
#' @export
#' @examplesIf interactive()
#' model <- sbert_load_model()
sbert_load_model <- function(
  model = "all-MiniLM-L6-v2",
  cache_dir = sbert_cache_dir(),
  backend = "cpu",
  threads = 1L,
  verify = TRUE
) {
  stopifnot(
    is.character(cache_dir),
    length(cache_dir) == 1L,
    !is.na(cache_dir),
    nzchar(cache_dir),
    is.character(backend),
    length(backend) == 1L,
    backend %in% c("cpu", "coreml", "cuda", "xnnpack", "openvino"),
    is.numeric(threads),
    length(threads) == 1L,
    is.finite(threads),
    threads >= 1,
    threads == as.integer(threads),
    is.logical(verify),
    length(verify) == 1L,
    !is.na(verify)
  )

  manifest <- resolve_sbert_manifest(model)

  status <- sbert_model_status(cache_dir, model = manifest$short_name)
  if (!all(status$exists)) {
    stop(
      "Model files are missing. Run sbert_model_download() explicitly first.",
      call. = FALSE
    )
  }
  if (verify && !all(status$valid)) {
    stop(
      "Model files failed size or SHA-256 validation. Remove and download them again.",
      call. = FALSE
    )
  }
  if (!sbert_onnx_is_installed()) {
    stop(
      "ONNX Runtime is not installed. Run sbert_install_runtime() explicitly first.",
      call. = FALSE
    )
  }

  paths <- status$path
  names(paths) <- status$file
  tokenizer_instance <- tryCatch(
    load_sbert_tokenizer(paths[["tokenizer.json"]]),
    error = function(error_condition) {
      stop(
        sprintf("Could not load tokenizer.json: %s", conditionMessage(error_condition)),
        call. = FALSE
      )
    }
  )
  onnx_instance <- tryCatch(
    load_sbert_onnx_model(
      path = paths[["model.onnx"]],
      backend = backend,
      threads = as.integer(threads)
    ),
    error = function(error_condition) {
      stop(
        sprintf("Could not load the ONNX model: %s", conditionMessage(error_condition)),
        call. = FALSE
      )
    }
  )

  structure(
    list(
      id = manifest$id,
      short_name = manifest$short_name,
      revision = manifest$revision,
      dimension = manifest$dimension,
      max_length = manifest$max_length,
      pad_token = manifest$pad_token,
      pad_id = manifest$pad_id,
      token_type_ids = manifest$token_type_ids,
      pooling = manifest$pooling,
      prefix = manifest$prefix,
      backend = backend,
      threads = as.integer(threads),
      tokenizer = tokenizer_instance,
      onnx = onnx_instance
    ),
    class = "sbert_model"
  )
}

sbert_onnx_is_installed <- function() {
  onnxr::onnx_is_installed()
}

load_sbert_tokenizer <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  tok::tokenizer$from_file(path)
}

load_sbert_onnx_model <- function(path, backend, threads) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    file.exists(path),
    is.character(backend),
    length(backend) == 1L,
    is.numeric(threads),
    length(threads) == 1L
  )
  onnxr::onnx_model(
    path = path,
    backend = backend,
    threads = as.integer(threads)
  )
}

#' @export
print.sbert_model <- function(x, ...) {
  stopifnot(inherits(x, "sbert_model"))
  cat(sprintf(
    "<sbert_model>\n  model: %s\n  revision: %s\n  dimensions: %d\n  backend: %s\n",
    x$id,
    x$revision,
    x$dimension,
    x$backend
  ))
  invisible(x)
}
