input_path <- Sys.getenv(
  "SBERT_FEEDBACK_CSV",
  unset = "/Users/mohammedsaqr/Downloads/Bee2/feedback_translations (2).csv"
)
output_directory <- file.path("outputs", "feedback_translation_topics")
embedding_path <- file.path(output_directory, "feedback_translation_embeddings.rds")
diagnostic_path <- file.path(output_directory, "topic_count_diagnostics.csv")
model_path <- file.path(output_directory, "feedback_translation_topic_model.rds")
assignment_path <- file.path(output_directory, "document_assignments.csv")
topic_summary_path <- file.path(output_directory, "topic_summary.csv")
term_path <- file.path(output_directory, "topic_terms.csv")
representative_path <- file.path(output_directory, "topic_representatives.csv")
data_quality_path <- file.path(output_directory, "data_quality.csv")
duplicate_path <- file.path(output_directory, "repeated_translations.csv")
method_path <- file.path(output_directory, "method.csv")
model_cache <- Sys.getenv(
  "SBERT_MODEL_CACHE",
  unset = "/private/tmp/sbert-package-download-test"
)
selected_topic_count <- 6L
reuse_analysis_cache <- identical(
  tolower(Sys.getenv("SBERT_REUSE_ANALYSIS_CACHE", unset = "false")),
  "true"
)

stopifnot(
  file.exists(input_path),
  dir.exists(model_cache)
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

feedback_data <- tryCatch(
  read.csv(
    input_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  ),
  error = function(error_condition) {
    stop(sprintf("CSV import failed: %s", conditionMessage(error_condition)))
  }
)

str(feedback_data)
print(class(feedback_data))
print(dim(feedback_data))
print(names(feedback_data))
print(head(feedback_data))
print(summary(feedback_data))
print(vapply(feedback_data, class, character(1)))

stopifnot(
  identical(names(feedback_data), c("feedback", "translation")),
  is.character(feedback_data$feedback),
  is.character(feedback_data$translation),
  !anyNA(feedback_data$translation)
)

feedback_data$row_id <- seq_len(nrow(feedback_data))
feedback_data$translation_clean <- trimws(feedback_data$translation)
feedback_data$included <- nzchar(feedback_data$translation_clean)
feedback_data$translation_has_cyrillic <- grepl(
  "[\u0400-\u04FF]",
  feedback_data$translation,
  perl = TRUE
)
feedback_data$translation_has_non_ascii_latin <- grepl(
  "[\u00C0-\u024F]",
  feedback_data$translation,
  perl = TRUE
)
feedback_data$source_equals_translation <-
  trimws(feedback_data$feedback) == feedback_data$translation_clean &
  feedback_data$included
feedback_data$translation_review_flag <-
  feedback_data$translation_has_cyrillic |
  feedback_data$translation_has_non_ascii_latin |
  feedback_data$source_equals_translation

analysis_data <- subset(
  feedback_data,
  included,
  select = c(
    "row_id",
    "feedback",
    "translation",
    "translation_clean",
    "translation_has_cyrillic",
    "translation_has_non_ascii_latin",
    "source_equals_translation",
    "translation_review_flag"
  )
)
unique_translation <- unique(analysis_data$translation_clean)
analysis_data$embedding_id <- match(
  analysis_data$translation_clean,
  unique_translation
)
translation_frequency <- table(analysis_data$translation_clean)
analysis_data$translation_duplicate_count <- as.integer(
  translation_frequency[analysis_data$translation_clean]
)
feedback_data$translation_duplicate_count <- 0L
feedback_data$translation_duplicate_count[feedback_data$included] <-
  analysis_data$translation_duplicate_count

stopifnot(
  nrow(analysis_data) == sum(feedback_data$included),
  !anyNA(analysis_data),
  anyDuplicated(analysis_data$row_id) == 0L,
  anyDuplicated(unique_translation) == 0L,
  all(analysis_data$embedding_id %in% seq_along(unique_translation)),
  all(analysis_data$translation_duplicate_count >= 1L)
)

model <- load_model(model_cache, threads = 2L)
if (reuse_analysis_cache && file.exists(embedding_path)) {
  embedding_cache <- readRDS(embedding_path)
  str(embedding_cache, max.level = 1L)
  print(names(embedding_cache))
  stopifnot(
    identical(embedding_cache$source_path, input_path),
    identical(embedding_cache$unique_translation, unique_translation),
    is.matrix(embedding_cache$unique_embeddings)
  )
  unique_embeddings <- embedding_cache$unique_embeddings
  embedding_seconds <- embedding_cache$embedding_seconds
} else {
  embedding_time <- system.time(
    unique_embeddings <- encode(
      unique_translation,
      model,
      batch_size = 64L,
      normalize = TRUE
    )
  )
  embedding_seconds <- unname(embedding_time[["elapsed"]])
}
analysis_embeddings <- unique_embeddings[
  analysis_data$embedding_id,
  ,
  drop = FALSE
]

str(unique_embeddings)
print(class(unique_embeddings))
print(dim(unique_embeddings))
print(summary(as.numeric(unique_embeddings)))
print(embedding_seconds)
print(summary(rowSums(unique_embeddings^2)))

stopifnot(
  identical(dim(unique_embeddings), c(length(unique_translation), 384L)),
  identical(nrow(analysis_embeddings), nrow(analysis_data)),
  !anyNA(unique_embeddings),
  all(is.finite(unique_embeddings)),
  isTRUE(all.equal(
    unname(rowSums(unique_embeddings^2)),
    rep(1, nrow(unique_embeddings)),
    tolerance = 1e-5
  ))
)

saveRDS(
  list(
    source_path = input_path,
    model_id = model$id,
    model_revision = model$revision,
    created_at = Sys.time(),
    embedding_seconds = embedding_seconds,
    feedback_data = feedback_data,
    analysis_data = analysis_data,
    unique_translation = unique_translation,
    unique_embeddings = unique_embeddings
  ),
  embedding_path,
  compress = "xz"
)

candidate_topics <- 4:16
if (reuse_analysis_cache && file.exists(diagnostic_path)) {
  topic_count_diagnostics <- read.csv(
    diagnostic_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
} else {
  candidate_results <- lapply(
    candidate_topics,
    function(n_topics) {
      elapsed <- system.time(
        clustering <- sbert:::fit_embedding_topics(
          analysis_embeddings,
          as.integer(n_topics),
          iter_max = 100L
        )
      )
      topic_proportion <- clustering$size / nrow(analysis_embeddings)
      calinski_harabasz <-
        (clustering$diagnostics$betweenss / (n_topics - 1)) /
        (clustering$diagnostics$tot_withinss /
          (nrow(analysis_embeddings) - n_topics))
      normalized_entropy <- -sum(
        topic_proportion * log(topic_proportion)
      ) / log(n_topics)

      data.frame(
        n_topics = as.integer(n_topics),
        calinski_harabasz = calinski_harabasz,
        between_total_ratio = clustering$diagnostics$betweenss /
          clustering$diagnostics$totss,
        normalized_entropy = normalized_entropy,
        min_topic_size = min(clustering$size),
        median_topic_size = stats::median(clustering$size),
        max_topic_size = max(clustering$size),
        min_topic_proportion = min(topic_proportion),
        max_min_size_ratio = max(clustering$size) / min(clustering$size),
        iterations = clustering$diagnostics$iterations,
        elapsed_seconds = unname(elapsed[["elapsed"]]),
        stringsAsFactors = FALSE
      )
    }
  )
  topic_count_diagnostics <- do.call(rbind, candidate_results)
  rownames(topic_count_diagnostics) <- NULL
}
topic_count_diagnostics$selected <-
  topic_count_diagnostics$n_topics == selected_topic_count
topic_count_diagnostics$selection_note <- ifelse(
  topic_count_diagnostics$selected,
  "Six-topic solution requested by the user.",
  ""
)

str(topic_count_diagnostics)
print(topic_count_diagnostics)
print(summary(topic_count_diagnostics))
stopifnot(
  identical(nrow(topic_count_diagnostics), length(candidate_topics)),
  !anyNA(topic_count_diagnostics),
  all(topic_count_diagnostics$min_topic_size > 0),
  anyDuplicated(topic_count_diagnostics$n_topics) == 0L,
  sum(topic_count_diagnostics$selected) == 1L
)

write.csv(topic_count_diagnostics, diagnostic_path, row.names = FALSE)

topic_model_time <- system.time(
  topic_model <- topics(
    analysis_data$translation_clean,
    n_topics = selected_topic_count,
    embeddings = analysis_embeddings,
    iter_max = 100L,
    n_terms = 15L,
    n_representatives = 5L,
    min_term_frequency = 5L,
    min_token_length = 2L,
    keep_embeddings = FALSE
  )
)

str(topic_model, max.level = 2L)
print(class(topic_model))
print(names(topic_model))
print(dim(topic_model$documents))
print(head(topic_model$documents))
print(topic_model$topics)
print(summary(topic_model$documents$distance))

topic_label <- setNames(topic_model$topics$label, topic_model$topics$topic)
modeled_assignments <- data.frame(
  document_id = topic_model$documents$document_id,
  row_id = analysis_data$row_id,
  feedback = analysis_data$feedback,
  translation = analysis_data$translation,
  translation_clean = analysis_data$translation_clean,
  translation_duplicate_count = analysis_data$translation_duplicate_count,
  topic = topic_model$documents$topic,
  topic_label = unname(topic_label[as.character(topic_model$documents$topic)]),
  cosine_distance_to_centroid = topic_model$documents$distance,
  stringsAsFactors = FALSE
)

# Pick the closest distinct translations so repeated rows do not crowd out
# the representative examples shown to readers.
representative_data <- do.call(
  rbind,
  lapply(
    seq_len(selected_topic_count),
    function(topic_id) {
      topic_rows <- modeled_assignments[
        modeled_assignments$topic == topic_id,
        ,
        drop = FALSE
      ]
      ranking <- order(
        topic_rows$cosine_distance_to_centroid,
        topic_rows$row_id
      )
      ranked_rows <- topic_rows[ranking, , drop = FALSE]
      unique_rows <- ranked_rows[
        !duplicated(ranked_rows$translation_clean),
        ,
        drop = FALSE
      ]
      selected_rows <- utils::head(unique_rows, 5L)
      data.frame(
        topic = selected_rows$topic,
        topic_label = selected_rows$topic_label,
        rank = seq_len(nrow(selected_rows)),
        document_id = selected_rows$document_id,
        row_id = selected_rows$row_id,
        feedback = selected_rows$feedback,
        translation = selected_rows$translation,
        cosine_distance_to_centroid =
          selected_rows$cosine_distance_to_centroid,
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(representative_data) <- NULL

representative_rank <- setNames(
  representative_data$rank,
  representative_data$row_id
)
assignment_match <- match(feedback_data$row_id, modeled_assignments$row_id)
document_assignments <- data.frame(
  row_id = feedback_data$row_id,
  feedback = feedback_data$feedback,
  translation = feedback_data$translation,
  included = feedback_data$included,
  exclusion_reason = ifelse(
    feedback_data$included,
    "",
    "blank translation"
  ),
  translation_duplicate_count = feedback_data$translation_duplicate_count,
  translation_has_cyrillic = feedback_data$translation_has_cyrillic,
  translation_has_non_ascii_latin =
    feedback_data$translation_has_non_ascii_latin,
  source_equals_translation = feedback_data$source_equals_translation,
  translation_review_flag = feedback_data$translation_review_flag,
  document_id = modeled_assignments$document_id[assignment_match],
  topic = modeled_assignments$topic[assignment_match],
  topic_label = modeled_assignments$topic_label[assignment_match],
  cosine_distance_to_centroid =
    modeled_assignments$cosine_distance_to_centroid[assignment_match],
  is_representative = feedback_data$row_id %in% representative_data$row_id,
  representative_rank = as.integer(
    representative_rank[as.character(feedback_data$row_id)]
  ),
  stringsAsFactors = FALSE
)

topic_distance_statistics <- do.call(
  rbind,
  lapply(
    seq_len(selected_topic_count),
    function(topic_id) {
      topic_rows <- modeled_assignments[
        modeled_assignments$topic == topic_id,
        ,
        drop = FALSE
      ]
      data.frame(
        topic = topic_id,
        n_unique_translations = length(unique(topic_rows$translation_clean)),
        repeated_group_rows = sum(
          topic_rows$translation_duplicate_count > 1L
        ),
        mean_cosine_distance = mean(
          topic_rows$cosine_distance_to_centroid
        ),
        median_cosine_distance = stats::median(
          topic_rows$cosine_distance_to_centroid
        ),
        p90_cosine_distance = unname(stats::quantile(
          topic_rows$cosine_distance_to_centroid,
          probs = 0.9,
          names = FALSE
        )),
        stringsAsFactors = FALSE
      )
    }
  )
)
top_terms <- vapply(
  seq_len(selected_topic_count),
  function(topic_id) {
    paste(
      topic_model$terms$term[
        topic_model$terms$topic == topic_id &
          topic_model$terms$rank <= 10L
      ],
      collapse = ", "
    )
  },
  character(1)
)
representative_columns <- do.call(
  cbind,
  lapply(
    seq_len(3L),
    function(representative_id) {
      vapply(
        seq_len(selected_topic_count),
        function(topic_id) {
          values <- representative_data$translation[
            representative_data$topic == topic_id &
              representative_data$rank == representative_id
          ]
          if (length(values) == 0L) "" else values[[1L]]
        },
        character(1)
      )
    }
  )
)
colnames(representative_columns) <- paste0("representative_", seq_len(3L))
topic_summary <- merge(
  topic_model$topics,
  topic_distance_statistics,
  by = "topic",
  sort = TRUE
)
names(topic_summary)[names(topic_summary) == "label"] <- "topic_label"
names(topic_summary)[names(topic_summary) == "n_documents"] <- "n_rows"
topic_summary$repeated_group_share <-
  topic_summary$repeated_group_rows / topic_summary$n_rows
topic_summary$top_terms <- top_terms[topic_summary$topic]
topic_summary <- cbind(topic_summary, representative_columns[topic_summary$topic, ])

topic_terms <- topic_model$terms
topic_terms$topic_label <- unname(
  topic_label[as.character(topic_terms$topic)]
)
topic_terms <- topic_terms[
  ,
  c("topic", "topic_label", "rank", "term", "score", "frequency")
]
names(topic_terms)[names(topic_terms) == "score"] <- "c_tfidf_score"
names(topic_terms)[names(topic_terms) == "frequency"] <-
  "within_topic_frequency"

repeated_translations <- data.frame(
  translation = names(translation_frequency)[translation_frequency > 1L],
  row_count = as.integer(translation_frequency[translation_frequency > 1L]),
  stringsAsFactors = FALSE
)
repeated_translations <- repeated_translations[
  order(-repeated_translations$row_count, repeated_translations$translation),
  ,
  drop = FALSE
]
rownames(repeated_translations) <- NULL

data_quality <- data.frame(
  metric = c(
    "source_rows",
    "included_rows",
    "excluded_blank_rows",
    "unique_nonblank_translations",
    "duplicate_occurrences_beyond_first",
    "rows_in_repeated_translation_groups",
    "maximum_translation_repeat_count",
    "missing_translation_values",
    "translations_with_cyrillic",
    "translations_with_non_ascii_latin",
    "nonblank_source_equal_translations",
    "rows_flagged_for_translation_review",
    "selected_topics"
  ),
  value = c(
    nrow(feedback_data),
    nrow(analysis_data),
    sum(!feedback_data$included),
    length(unique_translation),
    nrow(analysis_data) - length(unique_translation),
    sum(analysis_data$translation_duplicate_count > 1L),
    max(analysis_data$translation_duplicate_count),
    sum(is.na(feedback_data$translation)),
    sum(feedback_data$translation_has_cyrillic),
    sum(feedback_data$translation_has_non_ascii_latin),
    sum(feedback_data$source_equals_translation),
    sum(feedback_data$translation_review_flag),
    selected_topic_count
  ),
  stringsAsFactors = FALSE
)
method <- data.frame(
  item = c(
    "source_file",
    "text_field",
    "embedding_model",
    "model_revision",
    "runtime",
    "clustering",
    "topic_terms",
    "selected_topic_count",
    "selection_basis",
    "weighting",
    "translation_quality",
    "distance_interpretation",
    "label_interpretation",
    "embedding_seconds",
    "topic_model_seconds",
    "analysis_timestamp",
    "r_version",
    "platform"
  ),
  value = c(
    input_path,
    "translation",
    model$id,
    model$revision,
    "Python-free R pipeline using tok and onnxr",
    topic_model$diagnostics$algorithm,
    "BERTopic-style class-based TF-IDF",
    as.character(selected_topic_count),
    "Six-topic solution requested by the user; diagnostics for k=4 through k=16 are retained.",
    paste0(
      "All nonblank rows were clustered; repeated translations retain their ",
      "frequency weight. Embeddings were computed once per distinct text."
    ),
    paste0(
      "The supplied translation field was used as-is. Review flags identify ",
      "Cyrillic, accented non-ASCII Latin, or nonblank source-equal text; flags ",
      "are screening indicators and do not prove a translation error."
    ),
    "Cosine distance to the assigned centroid; lower is closer, not a probability.",
    "Topic IDs and automatic term labels are exploratory and require substantive review.",
    sprintf("%.3f", embedding_seconds),
    sprintf("%.3f", unname(topic_model_time[["elapsed"]])),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    R.version.string,
    R.version$platform
  ),
  stringsAsFactors = FALSE
)

str(document_assignments)
print(dim(document_assignments))
print(head(document_assignments))
print(summary(document_assignments$cosine_distance_to_centroid))
str(topic_summary)
print(topic_summary)
str(topic_terms)
print(head(topic_terms))
str(representative_data)
print(head(representative_data))

included_assignment <- document_assignments[
  document_assignments$included,
  ,
  drop = FALSE
]
excluded_assignment <- document_assignments[
  !document_assignments$included,
  ,
  drop = FALSE
]
stopifnot(
  nrow(document_assignments) == nrow(feedback_data),
  nrow(included_assignment) == nrow(analysis_data),
  nrow(excluded_assignment) == 11L,
  anyDuplicated(document_assignments$row_id) == 0L,
  all(is.finite(included_assignment$cosine_distance_to_centroid)),
  all(included_assignment$cosine_distance_to_centroid >= 0),
  all(included_assignment$cosine_distance_to_centroid <= 2),
  all(is.na(excluded_assignment$topic)),
  all(is.na(excluded_assignment$cosine_distance_to_centroid)),
  identical(sort(unique(included_assignment$topic)), seq_len(selected_topic_count)),
  sum(topic_summary$n_rows) == nrow(analysis_data),
  isTRUE(all.equal(sum(topic_summary$proportion), 1, tolerance = 1e-12)),
  all(topic_summary$n_rows > 0L),
  all(vapply(
    split(topic_terms$rank, topic_terms$topic),
    function(ranks) identical(ranks, seq_along(ranks)),
    logical(1)
  )),
  all(representative_data$topic == included_assignment$topic[
    match(representative_data$row_id, included_assignment$row_id)
  ]),
  !anyNA(included_assignment$translation),
  all(nzchar(trimws(included_assignment$translation))),
  sum(document_assignments$translation_has_cyrillic) == 49L,
  sum(document_assignments$translation_has_non_ascii_latin) == 14L,
  sum(document_assignments$source_equals_translation) == 167L,
  sum(document_assignments$translation_review_flag) == 179L
)

saveRDS(
  list(
    topic_model = topic_model,
    assignments = document_assignments,
    topic_summary = topic_summary,
    topic_terms = topic_terms,
    representatives = representative_data,
    topic_count_diagnostics = topic_count_diagnostics,
    data_quality = data_quality,
    repeated_translations = repeated_translations,
    method = method
  ),
  model_path,
  compress = "xz"
)
write.csv(document_assignments, assignment_path, row.names = FALSE)
write.csv(topic_summary, topic_summary_path, row.names = FALSE)
write.csv(topic_terms, term_path, row.names = FALSE)
write.csv(representative_data, representative_path, row.names = FALSE)
write.csv(data_quality, data_quality_path, row.names = FALSE)
write.csv(repeated_translations, duplicate_path, row.names = FALSE)
write.csv(method, method_path, row.names = FALSE)
