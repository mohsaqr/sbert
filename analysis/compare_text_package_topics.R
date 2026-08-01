text_library <- "/private/tmp/text-r-lib"
python_executable <- "/private/tmp/text-compare-venv/bin/python"
sentence_transformer_path <- "/private/tmp/text-all-MiniLM-L6-v2"
input_model_path <- file.path(
  "outputs",
  "feedback_translation_topics",
  "feedback_translation_topic_model.rds"
)
embedding_path <- file.path(
  "outputs",
  "feedback_translation_topics",
  "feedback_translation_embeddings.rds"
)
output_directory <- file.path(
  "outputs",
  "feedback_translation_topics",
  "text_package_comparison"
)
native_output_directory <- file.path(output_directory, "native_text_topics")
comparison_path <- file.path(output_directory, "text_topic_comparison.rds")
native_seed_directory <- file.path(native_output_directory, "seed_8")
reuse_native_cache <- identical(
  tolower(Sys.getenv("TEXT_REUSE_NATIVE_CACHE", unset = "false")),
  "true"
)

stopifnot(
  dir.exists(text_library),
  file.exists(python_executable),
  dir.exists(sentence_transformer_path),
  file.exists(input_model_path),
  file.exists(embedding_path)
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(RETICULATE_PYTHON = python_executable)
.libPaths(c(text_library, .libPaths()))

devtools::load_all(quiet = TRUE)
library(text)

sbert_result <- readRDS(input_model_path)
embedding_result <- readRDS(embedding_path)
assignments <- sbert_result$assignments
included_assignments <- assignments[assignments$included, , drop = FALSE]
analysis_embeddings <- embedding_result$unique_embeddings[
  embedding_result$analysis_data$embedding_id,
  ,
  drop = FALSE
]
text_data <- included_assignments[
  ,
  c("row_id", "translation")
]

str(text_data)
print(class(text_data))
print(dim(text_data))
print(names(text_data))
print(head(text_data))
print(summary(text_data))
print(vapply(text_data, class, character(1)))
str(analysis_embeddings)
print(class(analysis_embeddings))
print(dim(analysis_embeddings))
print(summary(as.numeric(analysis_embeddings)))

stopifnot(
  nrow(text_data) == 8976L,
  identical(text_data$row_id, included_assignments$row_id),
  identical(text_data$translation, included_assignments$translation),
  !anyNA(text_data),
  all(nzchar(trimws(text_data$translation))),
  identical(nrow(analysis_embeddings), nrow(text_data)),
  identical(ncol(analysis_embeddings), 384L)
)

# Verify that the Python SentenceTransformer runtime used by text and the
# package's native ONNX runtime produce numerically equivalent MiniLM vectors.
parity_index <- seq_len(32L)
sentence_transformers <- reticulate::import("sentence_transformers")
numpy <- reticulate::import("numpy")
python_embedding_model <- sentence_transformers$SentenceTransformer(
  sentence_transformer_path
)
python_embeddings <- reticulate::py_to_r(python_embedding_model$encode(
  as.list(text_data$translation[parity_index]),
  show_progress_bar = FALSE,
  normalize_embeddings = TRUE
))
r_embeddings <- analysis_embeddings[parity_index, , drop = FALSE]
embedding_max_absolute_difference <- max(abs(python_embeddings - r_embeddings))
embedding_allclose <- reticulate::py_to_r(numpy$allclose(
  python_embeddings,
  r_embeddings,
  rtol = 1e-5,
  atol = 1e-6
))
print(embedding_max_absolute_difference)
print(embedding_allclose)
stopifnot(
  identical(dim(python_embeddings), dim(r_embeddings)),
  isTRUE(embedding_allclose),
  embedding_max_absolute_difference < 1e-5
)

native_cache_files <- file.path(
  native_seed_directory,
  c("data.csv", "doc_info.csv", "topic_info.csv")
)
native_model_directory <- file.path(native_seed_directory, "my_model")
if (
  reuse_native_cache &&
    all(file.exists(native_cache_files)) &&
    dir.exists(native_model_directory)
) {
  bertopic_module <- reticulate::import("bertopic")
  text_result <- list(
    train_data = read.csv(
      native_cache_files[[1L]],
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    ),
    preds = NULL,
    doc_info = read.csv(
      native_cache_files[[2L]],
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    ),
    topic_info = read.csv(
      native_cache_files[[3L]],
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    ),
    model = list(
      model = bertopic_module$BERTopic$load(
        native_model_directory,
        embedding_model = python_embedding_model
      )
    ),
    model_type = "bert_topic",
    seed = 8L,
    save_dir = native_output_directory
  )
  text_fit_seconds <- NA_real_
} else {
  text_fit_time <- system.time(
    text_result <- textTopics(
      data = text_data,
      variable_name = "translation",
      embedding_model = "miniLM",
      representation_model = "mmr",
      umap_n_neighbors = 15L,
      umap_n_components = 5L,
      umap_min_dist = 0,
      umap_metric = "cosine",
      hdbscan_min_cluster_size = 5L,
      hdbscan_min_samples = NULL,
      hdbscan_metric = "euclidean",
      hdbscan_cluster_selection_method = "eom",
      hdbscan_prediction_data = TRUE,
      num_top_words = 10L,
      n_gram_range = c(1L, 3L),
      stopwords = "english",
      min_df = 3L,
      max_df = nrow(text_data),
      bm25_weighting = FALSE,
      reduce_frequent_words = TRUE,
      set_seed = 8L,
      save_dir = native_output_directory
    )
  )
  text_fit_seconds <- unname(text_fit_time[["elapsed"]])
}

str(text_result, max.level = 2L)
print(class(text_result))
print(names(text_result))
print(if (is.null(text_result$preds)) NULL else dim(text_result$preds))
print(head(text_result$doc_info))
print(text_result$topic_info)

native_document_info <- as.data.frame(text_result$doc_info)
native_topic_info <- as.data.frame(text_result$topic_info)
stopifnot(
  nrow(native_document_info) == nrow(text_data),
  "Topic" %in% names(native_document_info),
  !anyNA(native_document_info$Topic),
  identical(native_document_info$Document, text_result$train_data$translation)
)

reduced_document_path <- file.path(output_directory, "reduced_document_info.csv")
reduced_topic_path <- file.path(output_directory, "reduced_topic_info.csv")
text_documents <- as.list(text_result$train_data$translation)
reduction_time <- system.time(
  reduced_model <- text_result$model$model$reduce_topics(
    text_documents,
    nr_topics = as.integer(7L)
  )
)
text_result$model$model$get_document_info(text_documents)$to_csv(
  reduced_document_path,
  index = FALSE
)
text_result$model$model$get_topic_info()$to_csv(
  reduced_topic_path,
  index = FALSE
)
reduced_document_info <- read.csv(
  reduced_document_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  check.names = FALSE
)
reduced_topic_info <- read.csv(
  reduced_topic_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  check.names = FALSE
)

str(reduced_document_info)
print(class(reduced_document_info))
print(dim(reduced_document_info))
print(names(reduced_document_info))
print(head(reduced_document_info))
print(reduced_topic_info)

stopifnot(
  nrow(reduced_document_info) == nrow(text_data),
  !anyNA(reduced_document_info$Topic),
  identical(reduced_document_info$Document, text_result$train_data$translation)
)

comparison_assignments <- data.frame(
  row_id = text_data$row_id,
  translation = text_data$translation,
  translation_review_flag =
    included_assignments$translation_review_flag,
  translation_duplicate_count =
    included_assignments$translation_duplicate_count,
  sbert_topic = included_assignments$topic,
  text_native_topic = as.integer(native_document_info$Topic),
  text_reduced_topic = as.integer(reduced_document_info$Topic),
  stringsAsFactors = FALSE
)

unique_non_outlier_native <- sort(unique(
  comparison_assignments$text_native_topic[
    comparison_assignments$text_native_topic >= 0L
  ]
))
unique_non_outlier_reduced <- sort(unique(
  comparison_assignments$text_reduced_topic[
    comparison_assignments$text_reduced_topic >= 0L
  ]
))
print(unique_non_outlier_native)
print(unique_non_outlier_reduced)

partition_labels <- list(
  sbert_6 = comparison_assignments$sbert_topic,
  text_native = comparison_assignments$text_native_topic,
  text_reduced_6 = comparison_assignments$text_reduced_topic
)
subset_indices <- list(
  all_rows = seq_len(nrow(comparison_assignments)),
  unique_translations = which(!duplicated(comparison_assignments$translation)),
  clean_translation = which(!comparison_assignments$translation_review_flag),
  native_non_outliers = which(
    comparison_assignments$text_native_topic >= 0L
  )
)
partition_pairs <- list(
  c("sbert_6", "text_native"),
  c("sbert_6", "text_reduced_6"),
  c("text_native", "text_reduced_6")
)
agreement_rows <- unlist(
  lapply(
    names(subset_indices),
    function(subset_name) {
      row_index <- subset_indices[[subset_name]]
      lapply(
        partition_pairs,
        function(pair) {
          label_a <- partition_labels[[pair[[1L]]]][row_index]
          label_b <- partition_labels[[pair[[2L]]]][row_index]
          data.frame(
            subset = subset_name,
            method_a = pair[[1L]],
            method_b = pair[[2L]],
            n_rows = length(row_index),
            adjusted_rand = aricode::ARI(label_a, label_b),
            normalized_mutual_information = aricode::NMI(label_a, label_b),
            normalized_variation_information = aricode::NVI(label_a, label_b),
            stringsAsFactors = FALSE
          )
        }
      )
    }
  ),
  recursive = FALSE
)
agreement_metrics <- do.call(rbind, agreement_rows)
rownames(agreement_metrics) <- NULL

# Evaluate all partitions in the same original 384-dimensional MiniLM space.
partition_quality <- lapply(
  names(partition_labels),
  function(partition_name) {
    raw_label <- partition_labels[[partition_name]]
    modeled <- raw_label >= 0L
    modeled_label <- raw_label[modeled]
    modeled_embeddings <- analysis_embeddings[modeled, , drop = FALSE]
    topic_ids <- sort(unique(modeled_label))
    center_matrix <- do.call(
      rbind,
      lapply(
        topic_ids,
        function(topic_id) {
          center <- colMeans(
            modeled_embeddings[modeled_label == topic_id, , drop = FALSE]
          )
          center / sqrt(sum(center^2))
        }
      )
    )
    topic_center_row <- match(modeled_label, topic_ids)
    assigned_similarity <- rowSums(
      modeled_embeddings * center_matrix[topic_center_row, , drop = FALSE]
    )
    cosine_distance <- pmax(0, 1 - assigned_similarity)
    topic_size <- as.integer(table(factor(
      modeled_label,
      levels = topic_ids
    )))
    topic_proportion <- topic_size / sum(topic_size)
    sample_count <- min(1200L, nrow(modeled_embeddings))
    sample_index <- unique(as.integer(round(seq(
      1,
      nrow(modeled_embeddings),
      length.out = sample_count
    ))))
    sample_label <- as.integer(factor(modeled_label[sample_index]))
    silhouette_mean <- if (length(unique(sample_label)) >= 2L) {
      mean(cluster::silhouette(
        sample_label,
        stats::dist(modeled_embeddings[sample_index, , drop = FALSE])
      )[, "sil_width"])
    } else {
      NA_real_
    }
    data.frame(
      method = partition_name,
      topic_count = length(topic_ids),
      modeled_rows = sum(modeled),
      outlier_rows = sum(!modeled),
      outlier_share = mean(!modeled),
      largest_topic_share = max(topic_proportion),
      normalized_size_entropy = -sum(
        topic_proportion * log(topic_proportion)
      ) / log(length(topic_ids)),
      mean_cosine_distance = mean(cosine_distance),
      median_cosine_distance = stats::median(cosine_distance),
      p90_cosine_distance = unname(stats::quantile(
        cosine_distance,
        probs = 0.9,
        names = FALSE
      )),
      sampled_mean_silhouette = silhouette_mean,
      stringsAsFactors = FALSE
    )
  }
)
quality_metrics <- do.call(rbind, partition_quality)
rownames(quality_metrics) <- NULL

partition_summaries <- lapply(
  names(partition_labels),
  function(partition_name) {
    raw_label <- partition_labels[[partition_name]]
    modeled <- raw_label >= 0L
    modeled_label <- raw_label[modeled]
    modeled_text <- comparison_assignments$translation[modeled]
    modeled_embeddings <- analysis_embeddings[modeled, , drop = FALSE]
    raw_topic_ids <- names(sort(table(modeled_label), decreasing = TRUE))
    topic_id <- match(as.character(modeled_label), raw_topic_ids)
    topic_count <- length(raw_topic_ids)
    term_result <- sbert:::topic_term_scores(
      text = modeled_text,
      topic = as.integer(topic_id),
      n_topics = as.integer(topic_count),
      n_terms = 10L,
      stopwords = sbert_stopwords(),
      min_term_frequency = 5L,
      min_token_length = 2L
    )
    topic_rows <- lapply(
      seq_len(topic_count),
      function(current_topic) {
        topic_index <- which(topic_id == current_topic)
        center <- colMeans(modeled_embeddings[topic_index, , drop = FALSE])
        center <- center / sqrt(sum(center^2))
        distance <- pmax(
          0,
          1 - as.numeric(
            modeled_embeddings[topic_index, , drop = FALSE] %*% center
          )
        )
        rank_index <- order(distance, topic_index)
        distinct_rank_index <- rank_index[
          !duplicated(modeled_text[topic_index][rank_index])
        ]
        representative_index <- utils::head(distinct_rank_index, 3L)
        topic_terms <- term_result$terms$term[
          term_result$terms$topic == current_topic &
            term_result$terms$rank <= 10L
        ]
        representative_text <- modeled_text[topic_index][representative_index]
        representative_text <- c(
          representative_text,
          rep.int("", max(0L, 3L - length(representative_text)))
        )
        data.frame(
          method = partition_name,
          topic = current_topic,
          original_topic_id = as.integer(raw_topic_ids[[current_topic]]),
          n_rows = length(topic_index),
          proportion_of_modeled = length(topic_index) / sum(modeled),
          n_unique_translations = length(unique(modeled_text[topic_index])),
          median_cosine_distance = stats::median(distance),
          top_terms = paste(topic_terms, collapse = ", "),
          representative_1 = representative_text[[1L]],
          representative_2 = representative_text[[2L]],
          representative_3 = representative_text[[3L]],
          stringsAsFactors = FALSE
        )
      }
    )
    do.call(rbind, topic_rows)
  }
)
topic_summaries <- do.call(rbind, partition_summaries)
rownames(topic_summaries) <- NULL

duplicate_groups <- split(
  seq_len(nrow(comparison_assignments)),
  comparison_assignments$translation
)
repeated_group_indices <- duplicate_groups[
  vapply(duplicate_groups, length, integer(1)) > 1L
]
duplicate_consistency <- do.call(
  rbind,
  lapply(
    names(partition_labels),
    function(partition_name) {
      label <- partition_labels[[partition_name]]
      inconsistent <- vapply(
        repeated_group_indices,
        function(row_index) length(unique(label[row_index])) > 1L,
        logical(1)
      )
      data.frame(
        method = partition_name,
        repeated_translation_groups = length(repeated_group_indices),
        groups_split_across_topics = sum(inconsistent),
        split_group_share = mean(inconsistent),
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(duplicate_consistency) <- NULL

method_details <- data.frame(
  item = c(
    "text_r_package_version",
    "python_executable",
    "python_version",
    "sentence_transformers_version",
    "bertopic_version",
    "embedding_model",
    "embedding_revision",
    "text_native_algorithm",
    "text_reduced_algorithm",
    "sbert_algorithm",
    "seed",
    "text_fit_seconds",
    "text_reduction_seconds",
    "embedding_parity_max_abs_difference",
    "embedding_parity_numpy_allclose",
    "text_runtime_workarounds"
  ),
  value = c(
    as.character(utils::packageVersion("text")),
    python_executable,
    reticulate::py_config()$version_string,
    as.character(sentence_transformers$`__version__`),
    as.character(reticulate::import("bertopic")$`__version__`),
    "sentence-transformers/all-MiniLM-L6-v2",
    embedding_result$model_revision,
    "SentenceTransformer -> UMAP -> HDBSCAN -> BERTopic c-TF-IDF/MMR",
    paste0(
      "Native BERTopic topics merged post hoc with reduce_topics(nr_topics=7): ",
      "six non-outlier topics plus the persistent outlier group"
    ),
    sbert_result$topic_model$diagnostics$algorithm,
    "8",
    if (is.na(text_fit_seconds)) "reused verified native cache" else {
      sprintf("%.3f", text_fit_seconds)
    },
    sprintf("%.3f", unname(reduction_time[["elapsed"]])),
    sprintf("%.9g", embedding_max_absolute_difference),
    as.character(embedding_allclose),
    paste0(
      "Used max_df=8976 to avoid text 1.9 integer coercion bug; ",
      "loaded only the selected local MiniLM model instead of four eager models; ",
      "converted pandas Series to a string list for sentence-transformers 5.6 compatibility."
    )
  ),
  stringsAsFactors = FALSE
)

str(comparison_assignments)
print(head(comparison_assignments))
str(agreement_metrics)
print(agreement_metrics)
str(quality_metrics)
print(quality_metrics)
str(topic_summaries)
print(topic_summaries)
str(duplicate_consistency)
print(duplicate_consistency)

stopifnot(
  nrow(comparison_assignments) == 8976L,
  anyDuplicated(comparison_assignments$row_id) == 0L,
  length(unique_non_outlier_reduced) == 6L,
  sum(quality_metrics$modeled_rows + quality_metrics$outlier_rows) ==
    3L * nrow(comparison_assignments),
  !anyNA(agreement_metrics),
  all(is.finite(as.matrix(agreement_metrics[
    ,
    c(
      "adjusted_rand",
      "normalized_mutual_information",
      "normalized_variation_information"
    )
  ]))),
  all(agreement_metrics$adjusted_rand >= -1 &
    agreement_metrics$adjusted_rand <= 1),
  all(agreement_metrics$normalized_mutual_information >= 0 &
    agreement_metrics$normalized_mutual_information <= 1),
  all(quality_metrics$outlier_share >= 0 & quality_metrics$outlier_share <= 1),
  sum(comparison_assignments$translation_review_flag) == 179L,
  sum(!comparison_assignments$translation_review_flag) == 8797L,
  all(duplicate_consistency$repeated_translation_groups == 592L)
)

saveRDS(
  list(
    comparison_assignments = comparison_assignments,
    native_document_info = native_document_info,
    native_topic_info = native_topic_info,
    reduced_document_info = reduced_document_info,
    reduced_topic_info = reduced_topic_info,
    agreement_metrics = agreement_metrics,
    quality_metrics = quality_metrics,
    topic_summaries = topic_summaries,
    duplicate_consistency = duplicate_consistency,
    method_details = method_details
  ),
  comparison_path,
  compress = "xz"
)
write.csv(
  comparison_assignments,
  file.path(output_directory, "comparison_assignments.csv"),
  row.names = FALSE
)
write.csv(
  agreement_metrics,
  file.path(output_directory, "agreement_metrics.csv"),
  row.names = FALSE
)
write.csv(
  quality_metrics,
  file.path(output_directory, "quality_metrics.csv"),
  row.names = FALSE
)
write.csv(
  topic_summaries,
  file.path(output_directory, "topic_summaries.csv"),
  row.names = FALSE
)
write.csv(
  duplicate_consistency,
  file.path(output_directory, "duplicate_consistency.csv"),
  row.names = FALSE
)
write.csv(
  native_topic_info,
  file.path(output_directory, "native_topic_info.csv"),
  row.names = FALSE
)
write.csv(
  reduced_topic_info,
  file.path(output_directory, "reduced_topic_info.csv"),
  row.names = FALSE
)
write.csv(
  method_details,
  file.path(output_directory, "method_details.csv"),
  row.names = FALSE
)
