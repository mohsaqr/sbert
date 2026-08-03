# Generic Hugging Face embedding-model loader with trust-on-first-use pinning.
# Unlike the curated registry (models()), configuration here is
# auto-detected where possible and recorded to a local manifest on first
# download; later loads verify files against that recorded manifest. The
# caller vouches for the pooling configuration; no parity certificate is
# implied.

custom_model_directory <- function(cache_dir, id, revision) {
  file.path(cache_dir, "custom", gsub("/", "__", id, fixed = TRUE), revision)
}

read_hf_json <- function(url) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "load_custom() requires the 'jsonlite' package. Install it first.",
      call. = FALSE
    )
  }
  tryCatch(
    jsonlite::fromJSON(url, simplifyVector = TRUE),
    error = function(error_condition) {
      stop(
        sprintf("Could not read '%s': %s", url, conditionMessage(error_condition)),
        call. = FALSE
      )
    }
  )
}

resolve_hf_revision <- function(id) {
  info <- read_hf_json(sprintf("https://huggingface.co/api/models/%s", utils::URLencode(id)))
  revision <- info$sha
  if (is.null(revision) || !nzchar(revision)) {
    stop(sprintf("Could not resolve the current revision of '%s'.", id), call. = FALSE)
  }
  revision
}

find_hf_onnx_path <- function(id, revision) {
  tree <- read_hf_json(sprintf(
    "https://huggingface.co/api/models/%s/tree/%s?recursive=true",
    utils::URLencode(id),
    revision
  ))
  candidates <- intersect(c("onnx/model.onnx", "model.onnx"), tree$path)
  if (length(candidates) == 0L) {
    onnx_files <- grep("\\.onnx$", tree$path, value = TRUE)
    stop(
      sprintf(
        "No 'onnx/model.onnx' or 'model.onnx' in '%s' at revision %s.%s",
        id,
        revision,
        if (length(onnx_files) > 0L) {
          sprintf(
            " Other .onnx files present (%s); pass onnx_path explicitly.",
            paste(utils::head(onnx_files, 5L), collapse = ", ")
          )
        } else {
          " The repository ships no ONNX export."
        }
      ),
      call. = FALSE
    )
  }
  candidates[[1L]]
}

# Read optional Sentence-Transformers configuration files; each returns NULL
# when the file is absent so explicit arguments and probes can take over.
read_optional_hf_json <- function(id, revision, path) {
  url <- sprintf("https://huggingface.co/%s/resolve/%s/%s", id, revision, path)
  tryCatch(read_hf_json(url), error = function(error_condition) NULL)
}

detect_custom_pooling <- function(id, revision) {
  pooling_config <- read_optional_hf_json(id, revision, "1_Pooling/config.json")
  if (is.null(pooling_config)) {
    return(NULL)
  }
  if (isTRUE(pooling_config$pooling_mode_cls_token)) {
    return("cls")
  }
  if (isTRUE(pooling_config$pooling_mode_mean_tokens)) {
    return("mean")
  }
  NULL
}

detect_custom_pad <- function(tokenizer_json_path) {
  tokenizer_config <- tryCatch(
    jsonlite::fromJSON(tokenizer_json_path, simplifyVector = FALSE),
    error = function(error_condition) NULL
  )
  added <- tokenizer_config$added_tokens
  if (is.null(added)) {
    return(NULL)
  }
  contents <- vapply(added, function(token) as.character(token$content), character(1))
  ids <- vapply(added, function(token) as.integer(token$id), integer(1))
  index <- match(TRUE, tolower(contents) %in% c("[pad]", "<pad>"))
  if (is.na(index)) {
    return(NULL)
  }
  list(pad_token = contents[[index]], pad_id = ids[[index]])
}

download_custom_artifact <- function(url, destination, quiet) {
  destination_directory <- dirname(destination)
  dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
  temporary_path <- tempfile(
    pattern = paste0(basename(destination), "-"),
    tmpdir = destination_directory
  )
  on.exit(unlink(temporary_path, force = TRUE), add = TRUE)
  tryCatch(
    utils::download.file(url, temporary_path, mode = "wb", quiet = quiet),
    error = function(error_condition) {
      stop(
        sprintf(
          "Could not download '%s': %s",
          basename(destination),
          conditionMessage(error_condition)
        ),
        call. = FALSE
      )
    }
  )
  if (!file.rename(temporary_path, destination)) {
    stop(
      sprintf("Could not move '%s' into the cache.", basename(destination)),
      call. = FALSE
    )
  }
  invisible(destination)
}

custom_manifest_path <- function(model_directory) {
  file.path(model_directory, "sbert-manifest.json")
}

write_custom_manifest <- function(model_directory, manifest) {
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
    custom_manifest_path(model_directory)
  )
}

read_custom_manifest <- function(model_directory) {
  path <- custom_manifest_path(model_directory)
  if (!file.exists(path)) {
    return(NULL)
  }
  read_hf_json(path)
}

verify_custom_files <- function(model_directory, manifest) {
  files <- manifest$files
  vapply(
    seq_along(files$file),
    function(index) {
      validate_sbert_artifact(
        file.path(model_directory, files$file[[index]]),
        files$bytes[[index]],
        files$sha256[[index]]
      )
    },
    logical(1)
  )
}

#' Load an Arbitrary Hugging Face Embedding Model
#'
#' Loads any public Hugging Face repository that ships an ONNX encoder export
#' and a `tokenizer.json`, returning the same `sbert_model` object as
#' [load_model()] so that [encode()], [topics()], and every
#' downstream verb work unchanged.
#'
#' This is the escape hatch below the curated registry. On first use the
#' current revision is resolved (unless `revision` is given), the artifacts
#' are downloaded, and their byte sizes and SHA-256 values are recorded in a
#' local manifest ("trust on first use"); later loads verify the files
#' against that manifest and never touch the network. Pooling, prefix, and
#' maximum length are auto-detected from the repository's
#' Sentence-Transformers configuration when present, and must be supplied
#' explicitly otherwise. The ONNX input signature and embedding dimension are
#' read from the graph itself. Unlike registry models, the package makes no
#' numerical-parity claim: you vouch for the configuration.
#'
#' @param id Hugging Face repository id, for example `"thenlper/gte-small"`.
#' @param revision Optional commit hash to pin. `NULL` resolves the current
#'   revision on first download (recorded in the manifest thereafter).
#' @param onnx_path Repository path of the ONNX graph. `NULL` auto-detects
#'   `onnx/model.onnx` or `model.onnx`.
#' @param tokenizer_path Repository path of the tokenizer. Default
#'   `"tokenizer.json"`.
#' @param pooling `"mean"` or `"cls"`. `NULL` auto-detects from
#'   `1_Pooling/config.json` and fails with a clear message when absent.
#' @param prefix Text prepended to every input by [encode()]. `NULL`
#'   (default) keeps the recorded value (`""` on first use); pass `""` to
#'   clear a recorded prefix.
#' @param max_length Maximum word pieces per input. `NULL` auto-detects from
#'   `sentence_bert_config.json`, falling back to 512.
#' @param cache_dir Cache root returned by [cache_dir()].
#' @param backend ONNX execution backend accepted by [onnxr::onnx_model()].
#' @param threads Positive number of inference threads.
#' @param verify Whether to verify recorded byte sizes and SHA-256 hashes
#'   when loading from an existing cache.
#' @param quiet Whether to suppress download progress.
#' @return An object of class `sbert_model`.
#' @export
#' @examplesIf interactive()
#' model <- load_custom("thenlper/gte-small")
#' encode(c("one sentence", "another sentence"), model)
load_custom <- function(
  id,
  revision = NULL,
  onnx_path = NULL,
  tokenizer_path = "tokenizer.json",
  pooling = NULL,
  prefix = NULL,
  max_length = NULL,
  cache_dir = default_cache_dir(),
  backend = "cpu",
  threads = 1L,
  verify = TRUE,
  quiet = FALSE
) {
  stopifnot(
    is.character(id),
    length(id) == 1L,
    !is.na(id),
    grepl("^[^/]+/[^/]+$", id),
    is.null(revision) ||
      (is.character(revision) && length(revision) == 1L && nzchar(revision)),
    is.null(onnx_path) ||
      (is.character(onnx_path) && length(onnx_path) == 1L && nzchar(onnx_path)),
    is.character(tokenizer_path),
    length(tokenizer_path) == 1L,
    nzchar(tokenizer_path),
    is.null(pooling) || pooling %in% c("mean", "cls"),
    is.null(prefix) ||
      (is.character(prefix) && length(prefix) == 1L && !is.na(prefix)),
    is.null(max_length) ||
      (is.numeric(max_length) && length(max_length) == 1L && max_length >= 2 &&
        max_length == as.integer(max_length)),
    is.character(cache_dir),
    length(cache_dir) == 1L,
    nzchar(cache_dir),
    is.logical(verify),
    length(verify) == 1L,
    !is.na(verify),
    is.logical(quiet),
    length(quiet) == 1L,
    !is.na(quiet)
  )
  if (!sbert_onnx_is_installed()) {
    stop(
      "ONNX Runtime is not installed. Run install_runtime() explicitly first.",
      call. = FALSE
    )
  }

  # Reuse an already-pinned local copy when the revision is known.
  if (!is.null(revision)) {
    model_directory <- custom_model_directory(cache_dir, id, revision)
    manifest <- read_custom_manifest(model_directory)
  } else {
    manifest <- NULL
  }

  if (is.null(manifest)) {
    if (is.null(revision)) {
      revision <- resolve_hf_revision(id)
      model_directory <- custom_model_directory(cache_dir, id, revision)
      manifest <- read_custom_manifest(model_directory)
    }
  }

  if (is.null(manifest)) {
    if (is.null(onnx_path)) {
      onnx_path <- find_hf_onnx_path(id, revision)
    }
    if (is.null(pooling)) {
      pooling <- detect_custom_pooling(id, revision)
      if (is.null(pooling)) {
        stop(
          paste0(
            "Could not auto-detect the pooling mode (no Sentence-Transformers ",
            "1_Pooling config). Pass pooling = \"mean\" or \"cls\" explicitly."
          ),
          call. = FALSE
        )
      }
    }
    if (is.null(max_length)) {
      sbc <- read_optional_hf_json(id, revision, "sentence_bert_config.json")
      max_length <- if (!is.null(sbc$max_seq_length)) {
        as.integer(sbc$max_seq_length)
      } else {
        512L
      }
    }
    if (!quiet) {
      message(sprintf(
        "Downloading %s at revision %s to '%s'.",
        id,
        revision,
        model_directory
      ))
    }
    base_url <- sprintf("https://huggingface.co/%s/resolve/%s", id, revision)
    local_files <- c("model.onnx", "tokenizer.json")
    remote_files <- c(onnx_path, tokenizer_path)
    invisible(lapply(
      seq_along(local_files),
      function(index) {
        destination <- file.path(model_directory, local_files[[index]])
        if (!file.exists(destination)) {
          download_custom_artifact(
            paste(base_url, remote_files[[index]], sep = "/"),
            destination,
            quiet = quiet
          )
        }
      }
    ))
    paths <- file.path(model_directory, local_files)
    pad <- detect_custom_pad(paths[[2L]])
    if (is.null(pad)) {
      pad <- list(pad_token = "[PAD]", pad_id = 0L)
    }
    manifest <- list(
      id = id,
      revision = revision,
      pooling = pooling,
      prefix = if (is.null(prefix)) "" else prefix,
      max_length = as.integer(max_length),
      pad_token = pad$pad_token,
      pad_id = as.integer(pad$pad_id),
      files = list(
        file = as.list(local_files),
        remote_path = as.list(remote_files),
        bytes = as.list(unname(file.info(paths)$size)),
        sha256 = as.list(vapply(
          paths,
          digest::digest,
          character(1),
          algo = "sha256",
          file = TRUE
        ))
      )
    )
    write_custom_manifest(model_directory, manifest)
  } else {
    if (verify && !all(verify_custom_files(model_directory, manifest))) {
      stop(
        paste0(
          "Cached files no longer match the recorded manifest. Delete '",
          model_directory,
          "' and load again."
        ),
        call. = FALSE
      )
    }
    # Explicit arguments override what was recorded on first use.
    if (!is.null(pooling)) {
      manifest$pooling <- pooling
    }
    if (!is.null(prefix)) {
      manifest$prefix <- prefix
    }
    if (!is.null(max_length)) {
      manifest$max_length <- as.integer(max_length)
    }
  }

  paths <- file.path(model_directory, c("model.onnx", "tokenizer.json"))
  tokenizer_instance <- load_sbert_tokenizer(paths[[2L]])
  onnx_instance <- load_sbert_onnx_model(
    paths[[1L]],
    backend = backend,
    threads = as.integer(threads)
  )
  input_names <- onnx_instance$input_names
  output_names <- onnx_instance$output_names
  if (!"last_hidden_state" %in% output_names) {
    stop(
      sprintf(
        paste0(
          "The ONNX graph outputs (%s) do not include 'last_hidden_state'. ",
          "Only encoder feature-extraction exports are supported."
        ),
        paste(output_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unsupported <- setdiff(
    input_names,
    c("input_ids", "attention_mask", "token_type_ids")
  )
  if (length(unsupported) > 0L) {
    stop(
      sprintf(
        "The ONNX graph requires unsupported inputs: %s.",
        paste(unsupported, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  hidden <- onnx_instance$output_shapes[["last_hidden_state"]]
  dimension <- as.integer(hidden[[length(hidden)]])
  model <- structure(
    list(
      id = manifest$id,
      short_name = paste0("custom:", manifest$id),
      revision = manifest$revision,
      dimension = dimension,
      max_length = as.integer(manifest$max_length),
      pad_token = manifest$pad_token,
      pad_id = as.integer(manifest$pad_id),
      token_type_ids = "token_type_ids" %in% input_names,
      pooling = manifest$pooling,
      prefix = manifest$prefix,
      backend = backend,
      threads = as.integer(threads),
      tokenizer = tokenizer_instance,
      onnx = onnx_instance
    ),
    class = "sbert_model"
  )
  if (is.na(dimension) || dimension <= 0L) {
    # Dynamic output shape: probe the graph once to learn the dimension.
    probe <- encode_sbert_batch(model, "a", normalize = FALSE)
    model$dimension <- ncol(probe)
  }
  model
}
