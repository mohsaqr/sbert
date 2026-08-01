prepare_sbert_inputs <- function(
  text,
  tokenizer_instance,
  max_length,
  pad_id = 0L,
  pad_token = "[PAD]"
) {
  stopifnot(
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    is.environment(tokenizer_instance),
    is.numeric(max_length),
    length(max_length) == 1L,
    is.finite(max_length),
    max_length >= 2,
    max_length == as.integer(max_length),
    is.numeric(pad_id),
    length(pad_id) == 1L,
    pad_id >= 0,
    pad_id == as.integer(pad_id),
    is.character(pad_token),
    length(pad_token) == 1L,
    nzchar(pad_token)
  )

  tokenizer_instance$enable_truncation(max_length = as.integer(max_length))
  tokenizer_instance$enable_padding(
    direction = "right",
    pad_id = as.integer(pad_id),
    pad_type_id = 0L,
    pad_token = pad_token
  )
  encoded <- tokenizer_instance$encode_batch(
    text,
    is_pretokenized = FALSE,
    add_special_tokens = TRUE
  )

  input_ids <- do.call(
    rbind,
    lapply(encoded, function(item) as.integer(item$ids))
  )
  attention_mask <- do.call(
    rbind,
    lapply(encoded, function(item) as.integer(item$attention_mask))
  )
  token_type_ids <- matrix(
    0L,
    nrow = nrow(input_ids),
    ncol = ncol(input_ids)
  )

  stopifnot(
    is.matrix(input_ids),
    is.matrix(attention_mask),
    identical(dim(input_ids), dim(attention_mask)),
    identical(dim(input_ids), dim(token_type_ids)),
    !anyNA(input_ids),
    !anyNA(attention_mask),
    all(attention_mask %in% c(0L, 1L))
  )

  list(
    input_ids = input_ids,
    attention_mask = attention_mask,
    token_type_ids = token_type_ids
  )
}

#' Pool Token Embeddings into Sentence Embeddings
#'
#' Applies the pooling strategy of the loaded model — attention-mask-aware
#' mean pooling (the Sentence-BERT default) or CLS-token pooling (used by the
#' BGE and mxbai families) — and optional row-wise L2 normalization.
#'
#' @param token_embeddings Numeric array shaped batch by sequence by hidden.
#' @param attention_mask Numeric matrix shaped batch by sequence, containing
#'   only zero and one.
#' @param normalize Whether to L2-normalize every sentence vector.
#' @param method `"mean"` (default) or `"cls"` (first token).
#' @return A numeric matrix shaped batch by hidden.
#' @export
#' @examples
#' hidden <- array(1:12, dim = c(2, 3, 2))
#' mask <- matrix(c(1, 1, 0, 1, 0, 0), nrow = 2, byrow = TRUE)
#' sbert_pool(hidden, mask)
#' sbert_pool(hidden, mask, method = "cls")
sbert_pool <- function(
  token_embeddings,
  attention_mask,
  normalize = TRUE,
  method = c("mean", "cls")
) {
  method <- match.arg(method)
  stopifnot(
    is.numeric(token_embeddings),
    length(dim(token_embeddings)) == 3L,
    is.matrix(attention_mask),
    is.numeric(attention_mask),
    !anyNA(token_embeddings),
    !anyNA(attention_mask),
    all(is.finite(token_embeddings)),
    all(attention_mask %in% c(0, 1)),
    identical(dim(token_embeddings)[1:2], dim(attention_mask)),
    is.logical(normalize),
    length(normalize) == 1L,
    !is.na(normalize)
  )

  batch_size <- dim(token_embeddings)[[1L]]
  hidden_size <- dim(token_embeddings)[[3L]]
  if (method == "cls") {
    pooled <- matrix(
      token_embeddings[, 1L, ],
      nrow = batch_size,
      ncol = hidden_size
    )
  } else {
    masked_embeddings <- sweep(
      token_embeddings,
      c(1L, 2L),
      attention_mask,
      `*`
    )
    pooled <- apply(masked_embeddings, c(1L, 3L), sum) |>
      matrix(nrow = batch_size, ncol = hidden_size)
    token_counts <- pmax(rowSums(attention_mask), 1e-9)
    pooled <- pooled / token_counts
  }

  if (normalize) {
    row_norms <- pmax(sqrt(rowSums(pooled^2)), 1e-12)
    pooled <- pooled / row_norms
  }

  dimnames(pooled) <- NULL
  pooled
}

encode_sbert_batch <- function(model, text, normalize) {
  stopifnot(
    inherits(model, "sbert_model"),
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    is.logical(normalize),
    length(normalize) == 1L,
    !is.na(normalize)
  )

  inputs <- prepare_sbert_inputs(
    text,
    model$tokenizer,
    model$max_length,
    pad_id = if (is.null(model$pad_id)) 0L else model$pad_id,
    pad_token = if (is.null(model$pad_token)) "[PAD]" else model$pad_token
  )
  send_token_type_ids <- is.null(model$token_type_ids) || model$token_type_ids
  model_output <- tryCatch(
    run_sbert_onnx(model$onnx, inputs, token_type_ids = send_token_type_ids),
    error = function(error_condition) {
      stop(
        sprintf("ONNX inference failed: %s", conditionMessage(error_condition)),
        call. = FALSE
      )
    }
  )

  token_embeddings <- model_output[["last_hidden_state"]]
  if (is.null(token_embeddings)) {
    stop("The ONNX model did not return 'last_hidden_state'.", call. = FALSE)
  }

  sbert_pool(
    token_embeddings,
    inputs$attention_mask,
    normalize = normalize,
    method = if (is.null(model$pooling)) "mean" else model$pooling
  )
}

run_sbert_onnx <- function(onnx_model, inputs, token_type_ids = TRUE) {
  stopifnot(
    is.list(inputs),
    identical(
      names(inputs),
      c("input_ids", "attention_mask", "token_type_ids")
    ),
    is.logical(token_type_ids),
    length(token_type_ids) == 1L,
    !is.na(token_type_ids)
  )

  if (token_type_ids) {
    onnxr::onnx_run(
      onnx_model,
      input_ids = inputs$input_ids,
      attention_mask = inputs$attention_mask,
      token_type_ids = inputs$token_type_ids
    )
  } else {
    onnxr::onnx_run(
      onnx_model,
      input_ids = inputs$input_ids,
      attention_mask = inputs$attention_mask
    )
  }
}

#' Encode Text with Sentence-BERT
#'
#' @param text Character vector of sentences. Empty strings are supported;
#'   missing values are rejected.
#' @param model A loaded [sbert_model][sbert_load_model()], the name of a
#'   pinned model from [sbert_models()], or `NULL` for the default
#'   `all-MiniLM-L6-v2`. Names are loaded lazily and kept in a session
#'   cache; the first use of a not-yet-installed model offers a one-time
#'   verified download (interactive prompt, or
#'   `options(sbert.download = TRUE)` in scripts).
#' @param batch_size Positive number of sentences processed per ONNX call.
#' @param normalize Whether to return row-wise L2-normalized embeddings.
#' @param prefix Text prepended to every input before tokenization. `NULL`
#'   (default) uses the model's pinned prefix (for example `"query: "` for
#'   E5 models, which require one); `""` disables prefixing.
#' @return A numeric matrix with one row per input and one column per
#'   embedding dimension of the loaded model.
#' @export
#' @examplesIf interactive()
#' model <- sbert_load_model()
#' embeddings <- sbert_encode(c("A short sentence.", "Another sentence."), model)
sbert_encode <- function(
  text,
  model = NULL,
  batch_size = 32L,
  normalize = TRUE,
  prefix = NULL
) {
  model <- resolve_sbert_model(model)
  stopifnot(
    is.character(text),
    !anyNA(text),
    is.numeric(batch_size),
    length(batch_size) == 1L,
    is.finite(batch_size),
    batch_size >= 1,
    batch_size == as.integer(batch_size),
    is.logical(normalize),
    length(normalize) == 1L,
    !is.na(normalize),
    is.null(prefix) ||
      (is.character(prefix) && length(prefix) == 1L && !is.na(prefix))
  )

  if (length(text) == 0L) {
    return(matrix(numeric(), nrow = 0L, ncol = model$dimension))
  }

  if (is.null(prefix)) {
    prefix <- if (is.null(model$prefix)) "" else model$prefix
  }
  input_text <- if (nzchar(prefix)) paste0(prefix, text) else unname(text)

  indices <- seq_along(text)
  batch_groups <- split(
    indices,
    ceiling(indices / as.integer(batch_size))
  )
  embedding_batches <- lapply(
    batch_groups,
    function(index) {
      encode_sbert_batch(model, input_text[index], normalize = normalize)
    }
  )
  embeddings <- do.call(rbind, embedding_batches)
  rownames(embeddings) <- names(text)
  embeddings
}
