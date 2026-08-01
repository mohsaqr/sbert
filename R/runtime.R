#' Install ONNX Runtime
#'
#' Explicitly downloads the native ONNX Runtime used by the CRAN package
#' `onnxr`. This is never called during package installation or loading.
#'
#' @param cuda Passed to [onnxr::onnx_install()]. Use `FALSE` for the portable
#'   CPU runtime, `TRUE` to request CUDA on supported systems, or `NULL` for
#'   automatic detection.
#' @return The result returned by [onnxr::onnx_install()], invisibly.
#' @export
#' @examplesIf interactive()
#' sbert_install_runtime()
sbert_install_runtime <- function(cuda = NULL) {
  stopifnot(
    is.null(cuda) ||
      (is.logical(cuda) && length(cuda) == 1L && !is.na(cuda))
  )
  invisible(install_sbert_onnx_runtime(cuda = cuda))
}

install_sbert_onnx_runtime <- function(cuda = NULL) {
  stopifnot(
    is.null(cuda) ||
      (is.logical(cuda) && length(cuda) == 1L && !is.na(cuda))
  )
  onnxr::onnx_install(cuda = cuda)
}
