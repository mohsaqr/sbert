testthat::test_that("model loading fails clearly when artifacts are missing", {
  testthat::expect_error(
    load_model(cache_dir = tempfile("missing-cache-")),
    "Run model_download"
  )
  testthat::expect_error(load_model("not-a-pinned-model"), "Unknown model")
})

testthat::test_that("model loading builds a validated sbert_model", {
  cache_dir <- tempfile("model-cache-")
  model_directory <- sbert:::sbert_model_directory(cache_dir)
  dir.create(model_directory, recursive = TRUE)
  paths <- file.path(model_directory, c("model.onnx", "tokenizer.json"))
  writeBin(charToRaw("model"), paths[[1L]])
  writeBin(charToRaw("tokenizer"), paths[[2L]])
  status <- data.frame(
    file = basename(paths),
    path = paths,
    exists = TRUE,
    valid = TRUE,
    expected_bytes = unname(file.info(paths)$size),
    actual_bytes = unname(file.info(paths)$size),
    stringsAsFactors = FALSE
  )
  fake_tokenizer_instance <- fake_tokenizer()
  fake_onnx_instance <- structure(list(path = paths[[1L]]), class = "onnx_model")
  testthat::local_mocked_bindings(
    model_status = function(cache_dir, model) status,
    sbert_onnx_is_installed = function() TRUE,
    load_sbert_tokenizer = function(path) fake_tokenizer_instance,
    load_sbert_onnx_model = function(path, backend, threads) fake_onnx_instance,
    .package = "sbert"
  )

  model <- load_model(cache_dir = cache_dir, backend = "cpu", threads = 1L)

  testthat::expect_s3_class(model, "sbert_model")
  testthat::expect_identical(model$dimension, 384L)
  testthat::expect_identical(model$tokenizer, fake_tokenizer_instance)
  testthat::expect_identical(model$onnx, fake_onnx_instance)
  testthat::expect_output(print(model), "all-MiniLM-L6-v2")
  testthat::expect_identical(model$pad_token, "[PAD]")
  testthat::expect_identical(model$token_type_ids, TRUE)
  testthat::expect_error(
    load_model(cache_dir = cache_dir, backend = "invalid")
  )
})

testthat::test_that("runtime installation remains explicit and testable", {
  testthat::local_mocked_bindings(
    install_sbert_onnx_runtime = function(cuda = NULL) {
      if (is.null(cuda)) "auto-installed" else if (cuda) "cuda-installed" else "cpu-installed"
    },
    .package = "sbert"
  )

  testthat::expect_identical(install_runtime(), "auto-installed")
  testthat::expect_identical(install_runtime(FALSE), "cpu-installed")
  testthat::expect_identical(install_runtime(TRUE), "cuda-installed")
  testthat::expect_error(install_runtime(12))
})
