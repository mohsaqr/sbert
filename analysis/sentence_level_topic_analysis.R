source(file.path("analysis", "sentence_level_topic_functions.R"))
devtools::load_all(quiet = TRUE)

embedding_path <- file.path(
  "outputs",
  "feedback_translation_topics",
  "feedback_translation_embeddings.rds"
)
output_directory <- file.path(
  "outputs",
  "feedback_translation_topics",
  "sentence_level_topics"
)
output_model_path <- file.path(output_directory, "sentence_level_topic_model.rds")

stopifnot(file.exists(embedding_path))
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

embedding_result <- readRDS(embedding_path)
feedback_data <- embedding_result$feedback_data
analysis_data <- embedding_result$analysis_data
unique_text <- embedding_result$unique_translation
unique_embeddings <- embedding_result$unique_embeddings

str(feedback_data)
print(class(feedback_data))
print(dim(feedback_data))
print(names(feedback_data))
print(head(feedback_data))
print(summary(feedback_data))
print(vapply(feedback_data, class, character(1)))
str(unique_embeddings)
print(class(unique_embeddings))
print(dim(unique_embeddings))
print(summary(as.numeric(unique_embeddings)))

stopifnot(
  nrow(feedback_data) == 8987L,
  nrow(analysis_data) == 8976L,
  length(unique_text) == 8144L,
  identical(dim(unique_embeddings), c(8144L, 384L)),
  !anyNA(unique_embeddings),
  all(is.finite(unique_embeddings)),
  identical(sort(unique(analysis_data$embedding_id)), seq_len(8144L))
)

normalized_embeddings <- normalize_sentence_embeddings(unique_embeddings)
embedding_norms <- sqrt(rowSums(normalized_embeddings^2))
stopifnot(max(abs(embedding_norms - 1)) < 1e-10)

language_review_by_row <- analysis_data$translation_has_cyrillic |
  analysis_data$translation_has_non_ascii_latin
language_review_counts <- rowsum(
  as.integer(language_review_by_row),
  group = analysis_data$embedding_id,
  reorder = FALSE
)
unique_language_review <- as.logical(language_review_counts[, 1L] > 0L)
eligible_for_fit <- !unique_language_review

print(table(unique_language_review))
print(summary(embedding_norms))
stopifnot(
  length(unique_language_review) == 8144L,
  sum(unique_language_review) == 63L,
  sum(eligible_for_fit) == 8081L
)

# Fit exact six-topic spherical k-means to distinct, language-screened full
# sentences. Exact duplicate strings therefore cannot move topic boundaries.
fit_time <- system.time(
  spherical_fit <- fit_spherical_sentence_topics(
    normalized_embeddings[eligible_for_fit, , drop = FALSE],
    n_topics = 6L,
    seeds = 8:57,
    iter_max = 100L
  )
)
selected_fit <- spherical_fit$best_fit
raw_centers <- selected_fit$centers

# Assign every distinct sentence, including held-out translation-review rows,
# to its nearest fitted sentence centroid.
all_similarities <- normalized_embeddings %*% t(raw_centers)
raw_unique_topic <- max.col(all_similarities, ties.method = "first")
raw_chosen_similarity <- all_similarities[cbind(
  seq_len(nrow(all_similarities)),
  raw_unique_topic
)]
alternative_similarities <- all_similarities
alternative_similarities[cbind(
  seq_len(nrow(alternative_similarities)),
  raw_unique_topic
)] <- -Inf
raw_second_similarity <- apply(alternative_similarities, 1L, max)
raw_unique_distance <- pmax(0, 1 - raw_chosen_similarity)
raw_unique_margin <- raw_chosen_similarity - raw_second_similarity

# Number topics by descending observed row prevalence, using the smallest
# eligible embedding ID as a deterministic tie-breaker.
raw_row_topic <- raw_unique_topic[analysis_data$embedding_id]
raw_topic_sizes <- tabulate(raw_row_topic, nbins = 6L)
raw_tie_break <- vapply(
  seq_len(6L),
  function(topic_id) {
    min(which(raw_unique_topic == topic_id & eligible_for_fit))
  },
  integer(1)
)
topic_order <- order(-raw_topic_sizes, raw_tie_break)
topic_map <- integer(6L)
topic_map[topic_order] <- seq_len(6L)
unique_topic <- as.integer(topic_map[raw_unique_topic])
topic_centers <- raw_centers[topic_order, , drop = FALSE]
unique_distance <- raw_unique_distance
unique_margin <- raw_unique_margin

row_topic <- unique_topic[analysis_data$embedding_id]
row_distance <- unique_distance[analysis_data$embedding_id]
row_margin <- unique_margin[analysis_data$embedding_id]

print(spherical_fit$restart_diagnostics)
print(table(row_topic))
print(summary(row_distance))
print(summary(row_margin))

stopifnot(
  identical(sort(unique(row_topic)), 1:6),
  all(tabulate(row_topic, nbins = 6L) > 0L),
  all(row_distance >= 0),
  all(row_margin >= 0),
  sum(spherical_fit$restart_diagnostics$selected) == 1L,
  selected_fit$objective == max(spherical_fit$restart_diagnostics$objective)
)

# Select five diverse representative sentences per topic entirely in embedding
# space: the canonical centroid-nearest sentence plus four MMR representatives
# from the most central quarter of eligible distinct sentences.
representative_tables <- lapply(
  seq_len(6L),
  function(topic_id) {
    candidate_ids <- which(unique_topic == topic_id & eligible_for_fit)
    centroid_similarity <- as.numeric(
      normalized_embeddings[candidate_ids, , drop = FALSE] %*%
        topic_centers[topic_id, ]
    )
    central_count <- max(5L, ceiling(length(candidate_ids) * 0.25))
    central_order <- order(-centroid_similarity, candidate_ids)
    central_ids <- candidate_ids[utils::head(central_order, central_count)]
    central_similarity <- centroid_similarity[utils::head(
      central_order,
      central_count
    )]
    selected_ids <- Reduce(
      function(selected, unused_rank) {
        remaining <- central_ids[!central_ids %in% selected]
        relevance <- central_similarity[match(remaining, central_ids)]
        redundancy <- apply(
          normalized_embeddings[remaining, , drop = FALSE] %*%
            t(normalized_embeddings[selected, , drop = FALSE]),
          1L,
          max
        )
        mmr_score <- 0.70 * relevance - 0.30 * redundancy
        c(selected, remaining[order(-mmr_score, remaining)[[1L]]])
      },
      seq_len(4L),
      init = central_ids[[1L]]
    )
    data.frame(
      topic = rep.int(topic_id, length(selected_ids)),
      rank = seq_along(selected_ids),
      embedding_id = selected_ids,
      representative_sentence = unique_text[selected_ids],
      cosine_distance_to_centroid = unique_distance[selected_ids],
      assignment_margin = unique_margin[selected_ids],
      stringsAsFactors = FALSE
    )
  }
)
representative_sentences <- do.call(rbind, representative_tables)
rownames(representative_sentences) <- NULL

boundary_tables <- lapply(
  seq_len(6L),
  function(topic_id) {
    candidate_ids <- which(unique_topic == topic_id & eligible_for_fit)
    selected_ids <- candidate_ids[utils::head(
      order(unique_margin[candidate_ids], candidate_ids),
      3L
    )]
    data.frame(
      topic = rep.int(topic_id, length(selected_ids)),
      rank = seq_along(selected_ids),
      embedding_id = selected_ids,
      boundary_sentence = unique_text[selected_ids],
      cosine_distance_to_centroid = unique_distance[selected_ids],
      assignment_margin = unique_margin[selected_ids],
      stringsAsFactors = FALSE
    )
  }
)
boundary_sentences <- do.call(rbind, boundary_tables)
rownames(boundary_sentences) <- NULL

canonical_sentences <- representative_sentences[
  representative_sentences$rank == 1L,
  c("topic", "representative_sentence")
]
names(canonical_sentences)[[2L]] <- "canonical_sentence"
canonical_sentences <- canonical_sentences[order(canonical_sentences$topic), ]

# These concise descriptions are analyst interpretations of the complete
# representative sentences. The exact canonical corpus sentence remains the
# primary automatic topic label.
intent_titles <- c(
  "Selecting pictures between objects",
  "Clarifying what ‘between’ means",
  "Naming objects located between",
  "Ordinal and within-box picture relations",
  "Classifying people or animals by attributes",
  "Comparing quantities across colors"
)
sentence_topic_labels <- sprintf(
  "Topic %d — %s",
  seq_len(6L),
  canonical_sentences$canonical_sentence
)
representative_sentences$intent_title <- intent_titles[
  representative_sentences$topic
]
representative_sentences$sentence_topic_label <- sentence_topic_labels[
  representative_sentences$topic
]
boundary_sentences$intent_title <- intent_titles[boundary_sentences$topic]
boundary_sentences$sentence_topic_label <- sentence_topic_labels[
  boundary_sentences$topic
]

ambiguity_threshold <- unname(stats::quantile(
  row_margin[!language_review_by_row],
  probs = 0.1,
  names = FALSE
))
row_status <- ifelse(
  language_review_by_row,
  "translation_review",
  ifelse(row_margin <= ambiguity_threshold, "ambiguous", "assigned")
)
distance_percentile <- ave(
  row_distance,
  row_topic,
  FUN = function(topic_distance) {
    rank(topic_distance, ties.method = "max") / length(topic_distance)
  }
)

included_assignments <- data.frame(
  row_id = analysis_data$row_id,
  feedback = analysis_data$feedback,
  translation = analysis_data$translation,
  included = TRUE,
  exclusion_reason = "",
  translation_duplicate_count = analysis_data$translation_duplicate_count,
  translation_has_cyrillic = analysis_data$translation_has_cyrillic,
  translation_has_non_ascii_latin =
    analysis_data$translation_has_non_ascii_latin,
  source_equals_translation = analysis_data$source_equals_translation,
  translation_review_flag = analysis_data$translation_review_flag,
  embedding_id = analysis_data$embedding_id,
  topic = row_topic,
  intent_title = intent_titles[row_topic],
  sentence_topic_label = sentence_topic_labels[row_topic],
  canonical_sentence = canonical_sentences$canonical_sentence[row_topic],
  cosine_distance_to_centroid = row_distance,
  assignment_margin = row_margin,
  distance_percentile_within_topic = distance_percentile,
  assignment_status = row_status,
  stringsAsFactors = FALSE
)

excluded_rows <- feedback_data[!feedback_data$included, , drop = FALSE]
excluded_assignments <- data.frame(
  row_id = excluded_rows$row_id,
  feedback = excluded_rows$feedback,
  translation = excluded_rows$translation,
  included = FALSE,
  exclusion_reason = "blank translation",
  translation_duplicate_count = excluded_rows$translation_duplicate_count,
  translation_has_cyrillic = excluded_rows$translation_has_cyrillic,
  translation_has_non_ascii_latin =
    excluded_rows$translation_has_non_ascii_latin,
  source_equals_translation = excluded_rows$source_equals_translation,
  translation_review_flag = excluded_rows$translation_review_flag,
  embedding_id = NA_integer_,
  topic = NA_integer_,
  intent_title = NA_character_,
  sentence_topic_label = NA_character_,
  canonical_sentence = NA_character_,
  cosine_distance_to_centroid = NA_real_,
  assignment_margin = NA_real_,
  distance_percentile_within_topic = NA_real_,
  assignment_status = "excluded_blank",
  stringsAsFactors = FALSE
)
sentence_assignments <- rbind(included_assignments, excluded_assignments)
sentence_assignments <- sentence_assignments[
  order(sentence_assignments$row_id),
  ,
  drop = FALSE
]

topic_rows <- tabulate(row_topic, nbins = 6L)
topic_unique_sentences <- tabulate(unique_topic, nbins = 6L)
topic_mean_distance <- vapply(
  seq_len(6L),
  function(topic_id) mean(row_distance[row_topic == topic_id]),
  numeric(1)
)
topic_median_distance <- vapply(
  seq_len(6L),
  function(topic_id) stats::median(row_distance[row_topic == topic_id]),
  numeric(1)
)
topic_p90_distance <- vapply(
  seq_len(6L),
  function(topic_id) unname(stats::quantile(
    row_distance[row_topic == topic_id],
    probs = 0.9,
    names = FALSE
  )),
  numeric(1)
)
topic_status_counts <- as.data.frame.matrix(table(
  factor(row_topic, levels = seq_len(6L)),
  factor(row_status, levels = c("assigned", "ambiguous", "translation_review"))
))

topic_summary <- data.frame(
  topic = seq_len(6L),
  intent_title = intent_titles,
  sentence_topic_label = sentence_topic_labels,
  canonical_sentence = canonical_sentences$canonical_sentence,
  n_rows = topic_rows,
  proportion = topic_rows / nrow(analysis_data),
  n_unique_sentences = topic_unique_sentences,
  mean_cosine_distance = topic_mean_distance,
  median_cosine_distance = topic_median_distance,
  p90_cosine_distance = topic_p90_distance,
  assigned_rows = topic_status_counts$assigned,
  ambiguous_rows = topic_status_counts$ambiguous,
  translation_review_rows = topic_status_counts$translation_review,
  stringsAsFactors = FALSE
)

ngram_grid <- expand.grid(
  topic = seq_len(6L),
  n = seq_len(3L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
frequent_ngrams <- do.call(
  rbind,
  lapply(
    seq_len(nrow(ngram_grid)),
    function(grid_row) {
      topic_id <- ngram_grid$topic[[grid_row]]
      ngram_length <- ngram_grid$n[[grid_row]]
      frequency_table <- frequent_sentence_ngrams(
        analysis_data$translation[row_topic == topic_id],
        n = ngram_length,
        top_n = 10L,
        stopwords = stop_words()
      )
      data.frame(
        topic = rep.int(topic_id, nrow(frequency_table)),
        intent_title = rep.int(intent_titles[[topic_id]], nrow(frequency_table)),
        n = rep.int(ngram_length, nrow(frequency_table)),
        frequency_table,
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(frequent_ngrams) <- NULL

unique_sentence_assignments <- data.frame(
  embedding_id = seq_len(8144L),
  sentence = unique_text,
  held_out_translation_review = unique_language_review,
  topic = unique_topic,
  intent_title = intent_titles[unique_topic],
  sentence_topic_label = sentence_topic_labels[unique_topic],
  cosine_distance_to_centroid = unique_distance,
  assignment_margin = unique_margin,
  stringsAsFactors = FALSE
)

method <- data.frame(
  item = c(
    "source_file", "text_field", "embedding_model", "embedding_revision",
    "assignment_method", "fit_population", "translation_holdout",
    "topic_definition", "automatic_topic_label", "intent_title_method",
    "ngram_role", "requested_topic_count", "restarts", "selected_seed",
    "selected_objective", "fit_seconds", "ambiguity_rule"
  ),
  value = c(
    embedding_result$source_path,
    "translation",
    embedding_result$model_id,
    embedding_result$model_revision,
    "Cosine-aware multi-start spherical k-means on full-sentence embeddings",
    "8,081 distinct language-screened sentences; duplicate occurrences mapped back",
    "63 distinct Cyrillic/accented translations excluded from centroid fitting and assigned provisionally afterward",
    "Five diverse full-sentence representatives selected in embedding space",
    "Exact centroid-nearest eligible corpus sentence",
    "Provisional analyst interpretation of representative full sentences",
    "Secondary raw frequency appendix only; never used for assignment or titles",
    "6", "50", as.character(selected_fit$seed),
    sprintf("%.8f", selected_fit$objective),
    sprintf("%.3f", unname(fit_time[["elapsed"]])),
    sprintf("Lowest 10%% of non-language-review margins (<= %.6f)", ambiguity_threshold)
  ),
  stringsAsFactors = FALSE
)

result <- list(
  sentence_assignments = sentence_assignments,
  unique_sentence_assignments = unique_sentence_assignments,
  topic_summary = topic_summary,
  representative_sentences = representative_sentences,
  boundary_sentences = boundary_sentences,
  frequent_ngrams = frequent_ngrams,
  review_queue = sentence_assignments[
    sentence_assignments$assignment_status != "assigned",
    ,
    drop = FALSE
  ],
  restart_diagnostics = spherical_fit$restart_diagnostics,
  model = list(
    centers = topic_centers,
    selected_seed = selected_fit$seed,
    objective = selected_fit$objective,
    ambiguity_threshold = ambiguity_threshold
  ),
  method = method
)

str(result, max.level = 2L)
print(result$topic_summary)
print(result$representative_sentences)
print(result$boundary_sentences)
print(table(result$sentence_assignments$assignment_status, useNA = "ifany"))

stopifnot(
  nrow(result$sentence_assignments) == 8987L,
  identical(result$sentence_assignments$row_id, seq_len(8987L)),
  sum(result$sentence_assignments$included) == 8976L,
  sum(!result$sentence_assignments$included) == 11L,
  nrow(result$unique_sentence_assignments) == 8144L,
  sum(result$topic_summary$n_rows) == 8976L,
  nrow(result$topic_summary) == 6L,
  nrow(result$representative_sentences) == 30L,
  nrow(result$boundary_sentences) == 18L,
  nrow(result$frequent_ngrams) == 180L,
  all(result$representative_sentences$representative_sentence %in% unique_text),
  !any(result$representative_sentences$embedding_id %in%
    which(unique_language_review)),
  !any(grepl("c_tfidf|c-TF-IDF", names(result), ignore.case = TRUE))
)

saveRDS(result, output_model_path)
export_tables <- list(
  sentence_assignments = result$sentence_assignments,
  unique_sentence_assignments = result$unique_sentence_assignments,
  topic_summary = result$topic_summary,
  representative_sentences = result$representative_sentences,
  boundary_sentences = result$boundary_sentences,
  frequent_ngrams_secondary = result$frequent_ngrams,
  review_queue = result$review_queue,
  restart_diagnostics = result$restart_diagnostics,
  method = result$method
)
invisible(lapply(
  names(export_tables),
  function(export_name) {
    utils::write.csv(
      export_tables[[export_name]],
      file.path(output_directory, paste0(export_name, ".csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
))
