# Registry of pinned Sentence-BERT models. Every entry is locked to an
# immutable Hugging Face revision; artifact byte sizes and SHA-256 values were
# taken from the repository tree API (LFS object ids) or computed from the
# downloaded file at pin time. Never edit a hash without re-verifying it.
.sbert_registry <- list(
  "all-MiniLM-L6-v2" = list(
    id = "sentence-transformers/all-MiniLM-L6-v2",
    short_name = "all-MiniLM-L6-v2",
    revision = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41",
    license = "Apache-2.0",
    dimension = 384L,
    max_length = 256L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(90405214, 466247),
      sha256 = c(
        "6fd5d72fe4589f189f8ebc006442dbb529bb7ce38f8082112682524616046452",
        "be50c3628f2bf5bb5e3a7f17b1f74611b2561a3a27eeab05e5aa30f411572037"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "all-MiniLM-L12-v2" = list(
    id = "sentence-transformers/all-MiniLM-L12-v2",
    short_name = "all-MiniLM-L12-v2",
    revision = "a50ef00143b4d5391434df20ae11632588ac25be",
    license = "Apache-2.0",
    dimension = 384L,
    max_length = 128L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(133126567, 466247),
      sha256 = c(
        "84c56795d395593cbee215e2d635a8f0ad3199ae99f99299c44cf1eaecff3ad4",
        "be50c3628f2bf5bb5e3a7f17b1f74611b2561a3a27eeab05e5aa30f411572037"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "paraphrase-MiniLM-L3-v2" = list(
    id = "sentence-transformers/paraphrase-MiniLM-L3-v2",
    short_name = "paraphrase-MiniLM-L3-v2",
    revision = "4ca70771034acceecb2e72475f72050fcdde4ddc",
    license = "Apache-2.0",
    dimension = 384L,
    max_length = 128L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(69044978, 466248),
      sha256 = c(
        "84007b609c7a626b0adf825b1e36b839e705cd5995b4865b6e6696541d0a6350",
        "a9576e4dadfe7f78f071a1c00ba6902cc83ec6ed7e9b590ee974950bd4d54393"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "multi-qa-MiniLM-L6-cos-v1" = list(
    id = "sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
    short_name = "multi-qa-MiniLM-L6-cos-v1",
    revision = "b207367332321f8e44f96e224ef15bc607f4dbf0",
    license = "Apache-2.0",
    dimension = 384L,
    max_length = 512L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(90405214, 466247),
      sha256 = c(
        "826501e8460f6e1a83fa30a9b173f051100abda5559e1352efc0e3fe3136afc2",
        "7fa9272f7ef1ebd1666bb3bfd9d4707660ff0076ca9d1671cd9a9c6e18e03331"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "paraphrase-multilingual-MiniLM-L12-v2" = list(
    id = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
    short_name = "paraphrase-multilingual-MiniLM-L12-v2",
    revision = "e8f8c211226b894fcb81acc59f3b34ba3efd5f42",
    license = "Apache-2.0",
    dimension = 384L,
    max_length = 128L,
    languages = "50+ languages",
    pad_token = "<pad>",
    pad_id = 1L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(470301610, 9081518),
      sha256 = c(
        "10f7a088420252b26caf819236ca2c9d2987afd0fc06fec7553b542a5655a05a",
        "2c3387be76557bd40970cec13153b3bbf80407865484b209e655e5e4729076b8"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "all-mpnet-base-v2" = list(
    id = "sentence-transformers/all-mpnet-base-v2",
    short_name = "all-mpnet-base-v2",
    revision = "e8c3b32edf5434bc2275fc9bab85f82640a19130",
    license = "Apache-2.0",
    dimension = 768L,
    max_length = 384L,
    languages = "English",
    pad_token = "<pad>",
    pad_id = 1L,
    token_type_ids = FALSE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(435826548, 466021),
      sha256 = c(
        "74187b16d9c946fea252e120cfd7a12c5779d8b8b86838a2e4c56573c47941bd",
        "b8be2c30ba5dd723a6d5ee26d013da103d5408d92ddcb23747622f9e48f1d842"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "paraphrase-multilingual-mpnet-base-v2" = list(
    id = "sentence-transformers/paraphrase-multilingual-mpnet-base-v2",
    short_name = "paraphrase-multilingual-mpnet-base-v2",
    revision = "4328cf26390c98c5e3c738b4460a05b95f4911f5",
    license = "Apache-2.0",
    dimension = 768L,
    max_length = 128L,
    languages = "50+ languages",
    pad_token = "<pad>",
    pad_id = 1L,
    token_type_ids = FALSE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(1110068629, 9081518),
      sha256 = c(
        "253e00bb467fcdcac714a7b2443330c28ffbecb6d1f791c92caaf2af468bfbaa",
        "2c3387be76557bd40970cec13153b3bbf80407865484b209e655e5e4729076b8"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "bge-small-en-v1.5" = list(
    id = "BAAI/bge-small-en-v1.5",
    short_name = "bge-small-en-v1.5",
    revision = "5c38ec7c405ec4b44b94cc5a9bb96e735b38267a",
    license = "MIT",
    dimension = 384L,
    max_length = 512L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "cls",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(133093490, 711396),
      sha256 = c(
        "828e1496d7fabb79cfa4dcd84fa38625c0d3d21da474a00f08db0f559940cf35",
        "d241a60d5e8f04cc1b2b3e9ef7a4921b27bf526d9f6050ab90f9267a1f9e5c66"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "bge-base-en-v1.5" = list(
    id = "BAAI/bge-base-en-v1.5",
    short_name = "bge-base-en-v1.5",
    revision = "a5beb1e3e68b9ab74eb54cfd186867f64f240e1a",
    license = "MIT",
    dimension = 768L,
    max_length = 512L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "cls",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(435811539, 711396),
      sha256 = c(
        "9bc579acdba21c253c62a9bf866891355a63ffa3442b52c8a37d75b2ccb91848",
        "d241a60d5e8f04cc1b2b3e9ef7a4921b27bf526d9f6050ab90f9267a1f9e5c66"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "multilingual-e5-small" = list(
    id = "intfloat/multilingual-e5-small",
    short_name = "multilingual-e5-small",
    revision = "614241f622f53c4eeff9890bdc4f31cfecc418b3",
    license = "MIT",
    dimension = 384L,
    max_length = 512L,
    languages = "100+ languages",
    pad_token = "<pad>",
    pad_id = 1L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "query: ",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(470268510, 17082730),
      sha256 = c(
        "ca456c06b3a9505ddfd9131408916dd79290368331e7d76bb621f1cba6bc8665",
        "0b44a9d7b51c3c62626640cda0e2c2f70fdacdc25bbbd68038369d14ebdf4c39"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "nomic-embed-text-v1.5" = list(
    id = "nomic-ai/nomic-embed-text-v1.5",
    short_name = "nomic-embed-text-v1.5",
    revision = "e9b6763023c676ca8431644204f50c2b100d9aab",
    license = "Apache-2.0",
    dimension = 768L,
    max_length = 8192L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "search_document: ",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(547310275, 711396),
      sha256 = c(
        "147d5aa88c2101237358e17796cf3a227cead1ec304ec34b465bb08e9d952965",
        "d241a60d5e8f04cc1b2b3e9ef7a4921b27bf526d9f6050ab90f9267a1f9e5c66"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "jina-embeddings-v2-small-en" = list(
    id = "jinaai/jina-embeddings-v2-small-en",
    short_name = "jina-embeddings-v2-small-en",
    revision = "44e7d1d6caec8c883c2d4b207588504d519788d0",
    license = "Apache-2.0",
    dimension = 512L,
    max_length = 8192L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("model.onnx", "tokenizer.json"),
      bytes = c(129809014, 711573),
      sha256 = c(
        "974fdefe71fc9889258f569132b35acae6278874c8d09dbdf7806d23ad0b4497",
        "e9f999ac74497843ed9f4303246a8f43d9f100ee8aab8e133667903f447ceb48"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "mxbai-embed-large-v1" = list(
    id = "mixedbread-ai/mxbai-embed-large-v1",
    short_name = "mxbai-embed-large-v1",
    revision = "b33106f585b9ce46904ad7443a3b52b7a63e231c",
    license = "Apache-2.0",
    dimension = 1024L,
    max_length = 512L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = TRUE,
    pooling = "cls",
    prefix = "",
    artifacts = data.frame(
      file = c("model.onnx", "tokenizer.json"),
      remote_path = c("onnx/model.onnx", "tokenizer.json"),
      bytes = c(1336854282, 711396),
      sha256 = c(
        "adb53ed475faa339bfad3bd2bdb7e6a30b4f47280ade9811f81bef7953f9ab77",
        "d241a60d5e8f04cc1b2b3e9ef7a4921b27bf526d9f6050ab90f9267a1f9e5c66"
      ),
      stringsAsFactors = FALSE
    )
  ),
  "potion-base-8M" = list(
    id = "minishlab/potion-base-8M",
    short_name = "potion-base-8M",
    revision = "bf8b056651a2c21b8d2565580b8569da283cab23",
    license = "MIT",
    type = "static",
    dimension = 256L,
    # Static lookup has no attention window; the config's nominal limit.
    max_length = 1000000L,
    languages = "English",
    pad_token = "[PAD]",
    pad_id = 0L,
    token_type_ids = FALSE,
    pooling = "mean",
    prefix = "",
    artifacts = data.frame(
      file = c("model.safetensors", "tokenizer.json", "config.json"),
      remote_path = c("model.safetensors", "tokenizer.json", "config.json"),
      bytes = c(30236760, 683666, 202),
      sha256 = c(
        "f65d0f325faadc1e121c319e2faa41170d3fa07d8c89abd48ca5358d9a223de2",
        "e67e803f624fb4d67dea1c730d06e1067e1b14d830e2c2202569e3ef0f70bb50",
        "2a6ac0e9aaa356a68a5688070db78fc3a464fefe85d2f06a1905ce3718687553"
      ),
      stringsAsFactors = FALSE
    )
  )
)

.sbert_default_model <- "all-MiniLM-L6-v2"

# Resolve a user-supplied model name (short name or full repository id) to its
# pinned manifest.
resolve_sbert_manifest <- function(model = .sbert_default_model) {
  stopifnot(
    is.character(model),
    length(model) == 1L,
    !is.na(model),
    nzchar(model)
  )
  short_names <- names(.sbert_registry)
  full_ids <- vapply(.sbert_registry, `[[`, character(1), "id")
  index <- match(model, short_names)
  if (is.na(index)) {
    index <- match(model, full_ids)
  }
  if (is.na(index)) {
    stop(
      sprintf(
        "Unknown model '%s'. Available models: %s. See models().",
        model,
        paste(short_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  .sbert_registry[[index]]
}

#' List the Available Pinned Sentence-BERT Models
#'
#' Every supported model is locked to an immutable Hugging Face revision and
#' verified by byte size and SHA-256 before use. Pass the `model` column value
#' to any verb that takes a model, for example
#' `encode(text, model = "bge-small-en-v1.5")`.
#'
#' @param detail Whether to include the technical columns. The default
#'   `FALSE` returns the menu (`model`, `dimensions`, `max_tokens`,
#'   `languages`, `size_mb`); `TRUE` adds `pooling` (`"mean"` or `"cls"`),
#'   `prefix` (text automatically prepended by [encode()]), `license`,
#'   `id`, and `revision`.
#' @return A data frame with one row per model.
#' @export
#' @examples
#' models()
#' models(detail = TRUE)
models <- function(detail = FALSE) {
  stopifnot(is.logical(detail), length(detail) == 1L, !is.na(detail))
  full <- data.frame(
    model = vapply(.sbert_registry, `[[`, character(1), "short_name"),
    dimensions = vapply(.sbert_registry, `[[`, integer(1), "dimension"),
    max_tokens = vapply(.sbert_registry, `[[`, integer(1), "max_length"),
    languages = vapply(.sbert_registry, `[[`, character(1), "languages"),
    pooling = vapply(.sbert_registry, `[[`, character(1), "pooling"),
    prefix = vapply(.sbert_registry, `[[`, character(1), "prefix"),
    engine = vapply(
      .sbert_registry,
      function(manifest) {
        if (is.null(manifest$type)) "onnx" else manifest$type
      },
      character(1)
    ),
    size_mb = round(
      vapply(
        .sbert_registry,
        function(manifest) sum(manifest$artifacts$bytes),
        numeric(1)
      ) / 1e6,
      1
    ),
    license = vapply(.sbert_registry, `[[`, character(1), "license"),
    id = vapply(.sbert_registry, `[[`, character(1), "id"),
    revision = vapply(.sbert_registry, `[[`, character(1), "revision"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  if (detail) {
    return(full)
  }
  full[, c("model", "dimensions", "max_tokens", "languages", "size_mb")]
}

sbert_model_directory <- function(cache_dir, manifest = resolve_sbert_manifest()) {
  stopifnot(
    is.character(cache_dir),
    length(cache_dir) == 1L,
    !is.na(cache_dir),
    nzchar(cache_dir),
    is.list(manifest)
  )

  file.path(cache_dir, manifest$short_name, manifest$revision)
}

sbert_artifact_paths <- function(cache_dir, manifest = resolve_sbert_manifest()) {
  model_directory <- sbert_model_directory(cache_dir, manifest)
  file.path(model_directory, manifest$artifacts$file)
}

sbert_artifact_urls <- function(manifest = resolve_sbert_manifest()) {
  sprintf(
    "https://huggingface.co/%s/resolve/%s/%s",
    manifest$id,
    manifest$revision,
    manifest$artifacts$remote_path
  )
}

validate_sbert_artifact <- function(path, expected_bytes, expected_sha256) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    is.numeric(expected_bytes),
    length(expected_bytes) == 1L,
    is.character(expected_sha256),
    length(expected_sha256) == 1L
  )

  if (!file.exists(path)) {
    return(FALSE)
  }

  actual_bytes <- unname(file.info(path)$size)
  if (is.na(actual_bytes) || actual_bytes != expected_bytes) {
    return(FALSE)
  }

  identical(
    digest::digest(path, algo = "sha256", file = TRUE),
    expected_sha256
  )
}

validate_sbert_cache <- function(cache_dir, manifest = resolve_sbert_manifest()) {
  paths <- sbert_artifact_paths(cache_dir, manifest)
  artifacts <- manifest$artifacts

  vapply(
    seq_len(nrow(artifacts)),
    function(index) {
      validate_sbert_artifact(
        path = paths[[index]],
        expected_bytes = artifacts$bytes[[index]],
        expected_sha256 = artifacts$sha256[[index]]
      )
    },
    logical(1)
  )
}
