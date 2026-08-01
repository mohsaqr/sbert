fake_encoding <- function(ids, attention_mask) {
  stopifnot(
    is.integer(ids),
    is.integer(attention_mask),
    length(ids) == length(attention_mask)
  )
  encoding_environment <- new.env(parent = emptyenv())
  encoding_environment$ids <- ids
  encoding_environment$attention_mask <- attention_mask
  encoding_environment
}

fake_tokenizer <- function() {
  tokenizer_environment <- new.env(parent = emptyenv())
  tokenizer_environment$enable_truncation <- function(max_length) {
    invisible(max_length)
  }
  tokenizer_environment$enable_padding <- function(...) {
    invisible(list(...))
  }
  tokenizer_environment$encode_batch <- function(
    input,
    is_pretokenized,
    add_special_tokens
  ) {
    stopifnot(
      is.character(input),
      !is_pretokenized,
      add_special_tokens
    )
    lapply(
      seq_along(input),
      function(index) fake_encoding(
        ids = c(101L, index + 1000L, 102L),
        attention_mask = c(1L, 1L, 1L)
      )
    )
  }
  tokenizer_environment
}

fake_sbert_model <- function() {
  structure(
    list(
      id = "test/model",
      short_name = "test-model",
      revision = "test-revision",
      dimension = 2L,
      max_length = 8L,
      pad_token = "[PAD]",
      pad_id = 0L,
      token_type_ids = TRUE,
      pooling = "mean",
      prefix = "",
      backend = "cpu",
      threads = 1L,
      tokenizer = fake_tokenizer(),
      onnx = structure(list(), class = "onnx_model")
    ),
    class = "sbert_model"
  )
}

