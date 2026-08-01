write_test_safetensors <- function(path, tensors) {
  header <- jsonlite::toJSON(
    tensors$header,
    auto_unbox = TRUE,
    digits = NA
  )
  header_raw <- charToRaw(as.character(header))
  connection <- file(path, "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(c(length(header_raw), 0L), connection, size = 4L, endian = "little")
  writeBin(header_raw, connection)
  writeBin(tensors$values, connection, size = 4L, endian = "little")
  invisible(path)
}

testthat::test_that("safetensors reader round-trips a float32 matrix", {
  testthat::skip_if_not_installed("jsonlite")
  path <- tempfile(fileext = ".safetensors")
  on.exit(unlink(path), add = TRUE)
  values <- as.numeric(1:6)
  write_test_safetensors(
    path,
    list(
      header = list(
        embeddings = list(
          dtype = "F32",
          shape = c(2L, 3L),
          data_offsets = c(0L, 24L)
        )
      ),
      values = values
    )
  )
  result <- read_safetensors_tensor(path, "embeddings")
  testthat::expect_equal(
    result,
    matrix(values, nrow = 2, ncol = 3, byrow = TRUE),
    tolerance = 1e-7
  )
})

testthat::test_that("safetensors reader rejects wrong names and dtypes", {
  testthat::skip_if_not_installed("jsonlite")
  path <- tempfile(fileext = ".safetensors")
  on.exit(unlink(path), add = TRUE)
  write_test_safetensors(
    path,
    list(
      header = list(
        weights = list(
          dtype = "F16",
          shape = c(1L, 2L),
          data_offsets = c(0L, 4L)
        )
      ),
      values = c(0, 0)
    )
  )
  testthat::expect_error(read_safetensors_tensor(path, "embeddings"), "not found")
  testthat::expect_error(read_safetensors_tensor(path, "weights"), "F32")
})

fake_static_tokenizer <- function(vocabulary) {
  tokenizer_environment <- new.env(parent = emptyenv())
  tokenizer_environment$encode_batch <- function(
    input,
    is_pretokenized,
    add_special_tokens
  ) {
    stopifnot(!is_pretokenized, !add_special_tokens)
    lapply(
      strsplit(tolower(input), " ", fixed = TRUE),
      function(tokens) {
        encoding_environment <- new.env(parent = emptyenv())
        ids <- unname(vocabulary[tokens])
        encoding_environment$ids <- as.integer(ids[!is.na(ids)])
        encoding_environment
      }
    )
  }
  tokenizer_environment
}

fake_static_model <- function() {
  structure(
    list(
      id = "test/static",
      short_name = "test-static",
      revision = "rev",
      dimension = 2L,
      max_length = 1000000L,
      pad_token = "[PAD]",
      pad_id = 0L,
      token_type_ids = FALSE,
      pooling = "mean",
      prefix = "",
      backend = "static",
      threads = 1L,
      tokenizer = fake_static_tokenizer(c(cats = 0L, mice = 1L, stocks = 2L)),
      onnx = NULL,
      embedding_matrix = rbind(
        c(1, 0),
        c(0, 1),
        c(3, 4)
      )
    ),
    class = c("sbert_static_model", "sbert_model")
  )
}

testthat::test_that("static encoding is a token-lookup mean", {
  raw <- sbert_encode(
    c("cats mice", "stocks", "cats"),
    model = fake_static_model(),
    normalize = FALSE
  )
  testthat::expect_equal(
    raw,
    rbind(c(0.5, 0.5), c(3, 4), c(1, 0)),
    tolerance = 1e-12
  )
  normalized <- sbert_encode(
    c("cats mice", "stocks"),
    model = fake_static_model()
  )
  testthat::expect_equal(
    normalized,
    rbind(c(1, 1) / sqrt(2), c(0.6, 0.8)),
    tolerance = 1e-12
  )
})

testthat::test_that("unknown-token documents embed as zero vectors", {
  result <- sbert_encode(
    c("zebra unknown", "cats"),
    model = fake_static_model()
  )
  testthat::expect_equal(result[1, ], c(0, 0), tolerance = 1e-12)
  testthat::expect_equal(result[2, ], c(1, 0), tolerance = 1e-12)
})

testthat::test_that("potion-base-8M is registered as a static model", {
  manifest <- resolve_sbert_manifest("potion-base-8M")
  testthat::expect_identical(manifest$type, "static")
  testthat::expect_identical(manifest$dimension, 256L)
  testthat::expect_identical(
    manifest$artifacts$file,
    c("model.safetensors", "tokenizer.json", "config.json")
  )
  menu <- sbert_models(detail = TRUE)
  testthat::expect_true("potion-base-8M" %in% menu$model)
  testthat::expect_identical(
    menu$engine[menu$model == "potion-base-8M"],
    "static"
  )
  testthat::expect_true(all(menu$engine[menu$model != "potion-base-8M"] == "onnx"))
})
