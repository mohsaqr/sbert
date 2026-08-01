testthat::test_that("input preparation returns aligned integer matrices", {
  inputs <- sbert:::prepare_sbert_inputs(
    c("one", "two"),
    fake_tokenizer(),
    max_length = 8L
  )

  testthat::expect_identical(
    names(inputs),
    c("input_ids", "attention_mask", "token_type_ids")
  )
  testthat::expect_identical(dim(inputs$input_ids), c(2L, 3L))
  testthat::expect_type(inputs$input_ids, "integer")
  testthat::expect_identical(inputs$attention_mask, matrix(1L, 2L, 3L))
  testthat::expect_identical(inputs$token_type_ids, matrix(0L, 2L, 3L))
  testthat::expect_false(anyNA(inputs$input_ids))
})

testthat::test_that("mask-aware pooling matches a hand calculation", {
  hidden <- array(
    c(
      1, 10, 2, 20, 100, 1000,
      4, 40, 8, 80, 200, 2000
    ),
    dim = c(2L, 3L, 2L)
  )
  mask <- matrix(c(1, 1, 0, 1, 0, 0), nrow = 2L, byrow = TRUE)
  pooled <- sbert_pool(hidden, mask, normalize = FALSE)
  normalized <- sbert_pool(hidden, mask, normalize = TRUE)

  expected <- rbind(
    c(mean(c(hidden[1, 1, 1], hidden[1, 2, 1])), mean(c(hidden[1, 1, 2], hidden[1, 2, 2]))),
    c(hidden[2, 1, 1], hidden[2, 1, 2])
  )
  testthat::expect_equal(pooled, expected, tolerance = 1e-12)
  testthat::expect_equal(rowSums(normalized^2), c(1, 1), tolerance = 1e-12)
  testthat::expect_false(anyNA(normalized))
  testthat::expect_error(sbert_pool(hidden, matrix(1, 3L, 2L)))
})

testthat::test_that("cls pooling takes the first token exactly", {
  hidden <- array(
    c(
      1, 10, 2, 20, 100, 1000,
      4, 40, 8, 80, 200, 2000
    ),
    dim = c(2L, 3L, 2L)
  )
  mask <- matrix(c(1, 1, 0, 1, 0, 0), nrow = 2L, byrow = TRUE)

  cls <- sbert_pool(hidden, mask, normalize = FALSE, method = "cls")

  testthat::expect_equal(
    cls,
    rbind(c(hidden[1, 1, 1], hidden[1, 1, 2]), c(hidden[2, 1, 1], hidden[2, 1, 2])),
    tolerance = 1e-12
  )
  testthat::expect_error(sbert_pool(hidden, mask, method = "max"))
})

testthat::test_that("the model prefix is prepended before tokenization", {
  model <- fake_sbert_model()
  model$prefix <- "query: "
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    encode_sbert_batch = function(model, text, normalize) {
      seen$text <- text
      cbind(nchar(text), seq_along(text))
    },
    .package = "sbert"
  )

  sbert_encode("hello", model)
  testthat::expect_identical(seen$text, "query: hello")
  sbert_encode("hello", model, prefix = "")
  testthat::expect_identical(seen$text, "hello")
  sbert_encode("hello", model, prefix = "passage: ")
  testthat::expect_identical(seen$text, "passage: hello")
})

testthat::test_that("one mocked ONNX batch is pooled and normalized", {
  model <- fake_sbert_model()
  testthat::local_mocked_bindings(
    run_sbert_onnx = function(onnx_model, inputs, token_type_ids = TRUE) {
      stopifnot(inherits(onnx_model, "onnx_model"), isTRUE(token_type_ids))
      list(last_hidden_state = array(1:12, dim = c(2L, 3L, 2L)))
    },
    .package = "sbert"
  )

  result <- sbert:::encode_sbert_batch(model, c("one", "two"), TRUE)

  testthat::expect_identical(dim(result), c(2L, 2L))
  testthat::expect_equal(rowSums(result^2), c(1, 1), tolerance = 1e-12)
  testthat::expect_true(all(is.finite(result)))
})

testthat::test_that("encoding preserves order, names, batching, and empty input", {
  model <- fake_sbert_model()
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  testthat::local_mocked_bindings(
    encode_sbert_batch = function(model, text, normalize) {
      calls$count <- calls$count + 1L
      cbind(nchar(text), seq_along(text))
    },
    .package = "sbert"
  )
  text <- c(a = "one", b = "three", c = "seven", d = "nine")

  result <- sbert_encode(text, model, batch_size = 2L)
  empty <- sbert_encode(character(), model)

  testthat::expect_identical(calls$count, 2L)
  testthat::expect_identical(rownames(result), names(text))
  testthat::expect_identical(unname(result[, 1L]), c(3L, 5L, 5L, 4L))
  testthat::expect_identical(dim(empty), c(0L, 2L))
  testthat::expect_error(sbert_encode(c("ok", NA_character_), model))
})



