testthat::test_that("custom cache directories encode the repository id", {
  directory <- sbert:::custom_model_directory("/cache", "org/model", "abc123")
  testthat::expect_identical(
    directory,
    file.path("/cache", "custom", "org__model", "abc123")
  )
})

testthat::test_that("pooling detection reads the Sentence-Transformers config", {
  testthat::local_mocked_bindings(
    read_optional_hf_json = function(id, revision, path) {
      list(pooling_mode_cls_token = TRUE, pooling_mode_mean_tokens = FALSE)
    },
    .package = "sbert"
  )
  testthat::expect_identical(sbert:::detect_custom_pooling("a/b", "rev"), "cls")

  testthat::local_mocked_bindings(
    read_optional_hf_json = function(id, revision, path) {
      list(pooling_mode_cls_token = FALSE, pooling_mode_mean_tokens = TRUE)
    },
    .package = "sbert"
  )
  testthat::expect_identical(sbert:::detect_custom_pooling("a/b", "rev"), "mean")

  testthat::local_mocked_bindings(
    read_optional_hf_json = function(id, revision, path) NULL,
    .package = "sbert"
  )
  testthat::expect_null(sbert:::detect_custom_pooling("a/b", "rev"))
})

testthat::test_that("pad detection finds [PAD] and <pad> in tokenizer files", {
  testthat::skip_if_not_installed("jsonlite")
  tokenizer_file <- tempfile(fileext = ".json")
  writeLines(
    jsonlite::toJSON(
      list(added_tokens = list(
        list(id = 0L, content = "<s>"),
        list(id = 1L, content = "<pad>")
      )),
      auto_unbox = TRUE
    ),
    tokenizer_file
  )
  pad <- sbert:::detect_custom_pad(tokenizer_file)
  testthat::expect_identical(pad$pad_token, "<pad>")
  testthat::expect_identical(pad$pad_id, 1L)

  no_pad_file <- tempfile(fileext = ".json")
  writeLines('{"added_tokens": [{"id": 5, "content": "<mask>"}]}', no_pad_file)
  testthat::expect_null(sbert:::detect_custom_pad(no_pad_file))
})

testthat::test_that("custom manifests round-trip and verify files", {
  testthat::skip_if_not_installed("jsonlite")
  model_directory <- tempfile("custom-model-")
  dir.create(model_directory, recursive = TRUE)
  paths <- file.path(model_directory, c("model.onnx", "tokenizer.json"))
  writeBin(charToRaw("onnx bytes"), paths[[1L]])
  writeBin(charToRaw("tokenizer bytes"), paths[[2L]])
  manifest <- list(
    id = "org/model",
    revision = "abc",
    pooling = "mean",
    prefix = "",
    max_length = 128L,
    pad_token = "[PAD]",
    pad_id = 0L,
    files = list(
      file = as.list(basename(paths)),
      remote_path = as.list(basename(paths)),
      bytes = as.list(unname(file.info(paths)$size)),
      sha256 = as.list(vapply(
        paths, digest::digest, character(1), algo = "sha256", file = TRUE
      ))
    )
  )
  sbert:::write_custom_manifest(model_directory, manifest)

  recovered <- sbert:::read_custom_manifest(model_directory)
  testthat::expect_identical(recovered$id, "org/model")
  testthat::expect_identical(recovered$pooling, "mean")
  testthat::expect_true(all(sbert:::verify_custom_files(model_directory, recovered)))

  writeBin(charToRaw("tampered"), paths[[1L]])
  testthat::expect_false(all(sbert:::verify_custom_files(model_directory, recovered)))
})

testthat::test_that("load_custom validates inputs without touching the network", {
  testthat::expect_error(load_custom("no-slash"))
  testthat::expect_error(load_custom("a/b", pooling = "max"))
  testthat::expect_error(load_custom("a/b", max_length = 1L))
  testthat::expect_error(load_custom(c("a/b", "c/d")))
})
