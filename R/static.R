# Static embedding models (the Model2Vec design): a pinned vocabulary
# embedding matrix distilled from a transformer. Inference is a token lookup
# and a mean -- no ONNX session, no attention -- which makes encoding around
# two orders of magnitude faster at a modest quality cost. The matrix ships
# as a safetensors file read directly in base R.

# Minimal safetensors reader for one float32 tensor: 8-byte little-endian
# header length, JSON header {name: {dtype, shape, data_offsets}}, raw data.
read_safetensors_tensor <- function(path, name) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    file.exists(path),
    is.character(name),
    length(name) == 1L
  )
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Reading safetensors requires the 'jsonlite' package.",
      call. = FALSE
    )
  }
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  length_words <- readBin(connection, "integer", n = 2L, size = 4L, endian = "little")
  header_bytes <- length_words[1L] + length_words[2L] * 2^32
  stopifnot(header_bytes > 0, header_bytes < 1e8)
  header <- jsonlite::fromJSON(
    rawToChar(readBin(connection, "raw", n = header_bytes)),
    simplifyVector = TRUE
  )
  entry <- header[[name]]
  if (is.null(entry)) {
    stop(
      sprintf(
        "Tensor '%s' not found in %s (available: %s).",
        name,
        basename(path),
        paste(setdiff(names(header), "__metadata__"), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!identical(entry$dtype, "F32")) {
    stop(
      sprintf("Tensor '%s' has dtype %s; only F32 is supported.", name, entry$dtype),
      call. = FALSE
    )
  }
  shape <- as.integer(entry$shape)
  stopifnot(length(shape) == 2L, all(shape >= 1L))
  seek(connection, where = 8 + header_bytes + entry$data_offsets[1L])
  values <- readBin(
    connection,
    "numeric",
    n = prod(shape),
    size = 4L,
    endian = "little"
  )
  # safetensors stores row-major (C order).
  matrix(values, nrow = shape[1L], ncol = shape[2L], byrow = TRUE)
}

load_sbert_static_model <- function(manifest, paths) {
  tokenizer_instance <- load_sbert_tokenizer(paths[["tokenizer.json"]])
  embedding_matrix <- read_safetensors_tensor(
    paths[["model.safetensors"]],
    "embeddings"
  )
  if (ncol(embedding_matrix) != manifest$dimension) {
    stop(
      sprintf(
        "Static embedding matrix has %d dimensions; the manifest pins %d.",
        ncol(embedding_matrix),
        manifest$dimension
      ),
      call. = FALSE
    )
  }
  structure(
    list(
      id = manifest$id,
      short_name = manifest$short_name,
      revision = manifest$revision,
      dimension = manifest$dimension,
      max_length = manifest$max_length,
      pad_token = manifest$pad_token,
      pad_id = manifest$pad_id,
      token_type_ids = FALSE,
      pooling = "mean",
      prefix = manifest$prefix,
      backend = "static",
      threads = 1L,
      tokenizer = tokenizer_instance,
      onnx = NULL,
      embedding_matrix = embedding_matrix
    ),
    class = c("sbert_static_model", "sbert_model")
  )
}

# Token lookup + mean. Tokenization adds no special tokens (the Model2Vec
# inference contract); documents with no known tokens embed as zero vectors.
encode_static_batch <- function(model, text) {
  encodings <- model$tokenizer$encode_batch(
    text,
    is_pretokenized = FALSE,
    add_special_tokens = FALSE
  )
  vocabulary_size <- nrow(model$embedding_matrix)
  rows <- lapply(
    encodings,
    function(encoding) {
      ids <- encoding$ids
      ids <- ids[ids >= 0L & ids < vocabulary_size]
      if (length(ids) == 0L) {
        numeric(model$dimension)
      } else {
        colMeans(model$embedding_matrix[ids + 1L, , drop = FALSE])
      }
    }
  )
  do.call(rbind, rows)
}
