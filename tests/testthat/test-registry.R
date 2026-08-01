testthat::test_that("sbert_models lists every pinned model tidily", {
  models <- sbert_models(detail = TRUE)

  testthat::expect_s3_class(models, "data.frame")
  testthat::expect_identical(
    names(models),
    c(
      "model", "dimensions", "max_tokens", "languages", "pooling",
      "prefix", "size_mb", "license", "id", "revision"
    )
  )
  testthat::expect_identical(
    names(sbert_models()),
    c("model", "dimensions", "max_tokens", "languages", "size_mb")
  )
  testthat::expect_gte(nrow(models), 4L)
  testthat::expect_identical(models$model[[1L]], "all-MiniLM-L6-v2")
  testthat::expect_true(all(models$dimensions %in% c(384L, 512L, 768L, 1024L)))
  testthat::expect_true(all(models$max_tokens >= 128L))
  testthat::expect_true(all(models$size_mb > 0))
  testthat::expect_true(all(grepl("^[A-Za-z0-9-]+/", models$id)))
  testthat::expect_true(all(grepl("^[0-9a-f]{40}$", models$revision)))
  testthat::expect_false(anyDuplicated(models$model) > 0L)
})

testthat::test_that("every registry manifest is internally consistent", {
  registry <- sbert:::.sbert_registry
  for (manifest in registry) {
    testthat::expect_identical(
      manifest$artifacts$file,
      c("model.onnx", "tokenizer.json")
    )
    testthat::expect_true(all(grepl("^[0-9a-f]{64}$", manifest$artifacts$sha256)))
    testthat::expect_true(all(manifest$artifacts$bytes > 0))
    testthat::expect_type(manifest$token_type_ids, "logical")
    testthat::expect_type(manifest$pad_id, "integer")
    testthat::expect_true(nzchar(manifest$pad_token))
    testthat::expect_true(manifest$pooling %in% c("mean", "cls"))
    testthat::expect_type(manifest$prefix, "character")
    testthat::expect_true(manifest$dimension %in% c(384L, 512L, 768L, 1024L))
  }
  # the two multilingual models share the identical XLM-R tokenizer artifact
  testthat::expect_identical(
    registry[["paraphrase-multilingual-MiniLM-L12-v2"]]$artifacts$sha256[[2L]],
    registry[["paraphrase-multilingual-mpnet-base-v2"]]$artifacts$sha256[[2L]]
  )
})

testthat::test_that("manifest resolution accepts short names and full ids", {
  by_short <- sbert:::resolve_sbert_manifest("all-mpnet-base-v2")
  by_id <- sbert:::resolve_sbert_manifest("sentence-transformers/all-mpnet-base-v2")

  testthat::expect_identical(by_short, by_id)
  testthat::expect_identical(by_short$dimension, 768L)
  testthat::expect_identical(
    sbert:::resolve_sbert_manifest()$short_name,
    "all-MiniLM-L6-v2"
  )
  testthat::expect_error(
    sbert:::resolve_sbert_manifest("all-MiniLM-L99-v9"),
    "Unknown model"
  )
  testthat::expect_error(sbert:::resolve_sbert_manifest(NA_character_))
})

testthat::test_that("model-specific cache directories do not collide", {
  cache_dir <- tempfile("registry-cache-")
  directories <- vapply(
    sbert_models(detail = TRUE)$model,
    function(name) {
      sbert:::sbert_model_directory(
        cache_dir,
        sbert:::resolve_sbert_manifest(name)
      )
    },
    character(1)
  )
  testthat::expect_false(anyDuplicated(directories) > 0L)
})
