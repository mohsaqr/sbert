testthat::test_that("official model matches SentenceTransformers references", {
  model_source <- Sys.getenv("SBERT_TEST_MODEL_DIR", unset = "")
  reference_path <- Sys.getenv("SBERT_PYTHON_REFERENCE", unset = "")
  testthat::skip_if(!nzchar(model_source), "official model fixture is not configured")
  testthat::skip_if_not_installed("onnxr")
  testthat::skip_if_not_installed("tok")
  testthat::skip_if(!onnxr::onnx_is_installed(), "ONNX Runtime is not installed")

  cache_dir <- tempfile("integration-cache-")
  model_directory <- sbert:::sbert_model_directory(cache_dir)
  dir.create(model_directory, recursive = TRUE)
  copied <- file.copy(
    file.path(model_source, c("model.onnx", "tokenizer.json")),
    model_directory
  )
  testthat::expect_true(all(copied))
  model <- load_model(cache_dir, threads = 1L)
  sentences <- c(
    "This is an example sentence",
    "Each sentence is converted.",
    "",
    "Café déjà vu — 東京"
  )
  expected_ids <- rbind(
    c(101L, 2023L, 2003L, 2019L, 2742L, 6251L, 102L, 0L, 0L),
    c(101L, 2169L, 6251L, 2003L, 4991L, 1012L, 102L, 0L, 0L),
    c(101L, 102L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    c(101L, 7668L, 2139L, 3900L, 24728L, 1517L, 1879L, 1755L, 102L)
  )
  inputs <- sbert:::prepare_sbert_inputs(
    sentences,
    model$tokenizer,
    model$max_length
  )
  embeddings <- encode(sentences, model, batch_size = 4L)

  testthat::expect_identical(inputs$input_ids, expected_ids)
  testthat::expect_identical(dim(embeddings), c(4L, 384L))
  testthat::expect_false(anyNA(embeddings))
  testthat::expect_true(all(is.finite(embeddings)))
  testthat::expect_equal(rowSums(embeddings^2), rep(1, 4L), tolerance = 1e-6)
  expected_prefix <- rbind(
      c(0.06765693, 0.06349593, 0.04871314, 0.07930495, 0.03744806, 0.00265273),
      c(0.07853645, 0.08856265, -0.00209270, -0.00717247, -0.01252624, 0.01881747),
      c(-0.11883835, 0.04829856, -0.00254805, -0.01101118, 0.05195070, 0.01029179),
      c(0.05083271, 0.00967685, 0.09635572, -0.00997369, -0.01987603, 0.01692343)
  )
  testthat::expect_lt(max(abs(embeddings[, 1:6] - expected_prefix)), 1e-6)

  if (nzchar(reference_path)) {
    python_embeddings <- as.matrix(utils::read.csv(
      reference_path,
      header = FALSE,
      check.names = FALSE
    ))
    testthat::expect_identical(dim(python_embeddings), dim(embeddings))
    testthat::expect_lt(max(abs(embeddings - python_embeddings)), 1e-6)
  }
})
