testthat::test_that("cache paths and missing status are deterministic", {
  cache_dir <- tempfile("sbert-cache-")
  status <- model_status(cache_dir)

  testthat::expect_identical(cache_dir(), tools::R_user_dir("sbert", "cache"))
  testthat::expect_s3_class(status, "data.frame")
  testthat::expect_identical(dim(status), c(2L, 6L))
  testthat::expect_identical(status$file, c("model.onnx", "tokenizer.json"))
  testthat::expect_false(any(status$exists))
  testthat::expect_false(any(status$valid))
  testthat::expect_true(all(is.na(status$actual_bytes)))
  testthat::expect_identical(cache_size(cache_dir), 0)
})

testthat::test_that("artifact validation checks size and SHA-256", {
  artifact <- tempfile("artifact-")
  writeBin(charToRaw("verified artifact"), artifact)
  bytes <- unname(file.info(artifact)$size)
  sha256 <- digest::digest(artifact, algo = "sha256", file = TRUE)

  testthat::expect_true(sbert:::validate_sbert_artifact(artifact, bytes, sha256))
  testthat::expect_false(sbert:::validate_sbert_artifact(artifact, bytes + 1, sha256))
  testthat::expect_false(sbert:::validate_sbert_artifact(artifact, bytes, paste0("0", sha256)))
  testthat::expect_false(sbert:::validate_sbert_artifact(paste0(artifact, "-missing"), bytes, sha256))
})

testthat::test_that("artifact download is atomic and validated", {
  source_path <- tempfile("source-")
  destination <- file.path(tempfile("destination-"), "artifact.bin")
  writeBin(charToRaw("small offline fixture"), source_path)
  bytes <- unname(file.info(source_path)$size)
  sha256 <- digest::digest(source_path, algo = "sha256", file = TRUE)

  result <- sbert:::download_sbert_artifact(
    url = paste0("file://", source_path),
    destination = destination,
    expected_bytes = bytes,
    expected_sha256 = sha256,
    quiet = TRUE
  )

  testthat::expect_identical(result, destination)
  testthat::expect_true(file.exists(destination))
  testthat::expect_true(sbert:::validate_sbert_artifact(destination, bytes, sha256))
  testthat::expect_error(
    sbert:::download_sbert_artifact(
      paste0("file://", source_path),
      paste0(destination, "-bad"),
      bytes,
      paste(rep("0", 64L), collapse = ""),
      TRUE
    ),
    "failed size or SHA-256"
  )
})

testthat::test_that("the explicit model downloader is offline-testable and idempotent", {
  source_directory <- tempfile("sources-")
  dir.create(source_directory)
  source_paths <- file.path(source_directory, c("model.onnx", "tokenizer.json"))
  writeBin(charToRaw("tiny onnx fixture"), source_paths[[1L]])
  writeBin(charToRaw("tiny tokenizer fixture"), source_paths[[2L]])
  bytes <- unname(file.info(source_paths)$size)
  sha256 <- vapply(
    source_paths,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
  manifest <- list(
    id = "test/model",
    short_name = "test-model",
    revision = "immutable-revision",
    license = "Apache-2.0",
    dimension = 2L,
    max_length = 8L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    artifacts = data.frame(
      file = basename(source_paths),
      remote_path = basename(source_paths),
      bytes = bytes,
      sha256 = sha256,
      stringsAsFactors = FALSE
    )
  )
  testthat::local_mocked_bindings(
    .sbert_registry = list("test-model" = manifest),
    sbert_artifact_urls = function(manifest) paste0("file://", source_paths),
    .package = "sbert"
  )
  cache_dir <- tempfile("cache-")

  first <- model_download("test-model", cache_dir, quiet = TRUE, timeout = 10)
  second <- model_download("test-model", cache_dir, quiet = TRUE, timeout = 10)
  status <- model_status(cache_dir, model = "test-model")

  testthat::expect_identical(first, second)
  testthat::expect_true(all(status$valid))
  testthat::expect_identical(cache_size(cache_dir), sum(bytes))
  testthat::expect_true(model_remove(cache_dir, model = "test-model"))
  testthat::expect_false(dir.exists(first))
})

testthat::test_that("cache input validation rejects unsafe values", {
  testthat::expect_error(model_status(NA_character_))
  testthat::expect_error(model_download(timeout = 0))
  testthat::expect_error(model_download("not-a-pinned-model"), "Unknown model")
  testthat::expect_error(cache_size(character()))
  testthat::expect_error(model_remove(""))
})

