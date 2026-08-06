source(file.path("analysis", "sentence_level_topic_functions.R"))
devtools::load_all(quiet = TRUE)

input_path <- Sys.getenv(
  "MCSE_RDS_PATH",
  unset = "/Users/mohammedsaqr/Documents/Documents X/MyData/AKA/STM/MCSE_gold.RDS"
)
output_directory <- file.path("outputs", "mcse_abstract_30_topics")
embedding_path <- file.path(output_directory, "mcse_abstract_embeddings.rds")
result_path <- file.path(output_directory, "mcse_abstract_30_topics.rds")
model_cache <- Sys.getenv(
  "SBERT_MODEL_CACHE",
  unset = "/private/tmp/sbert-package-download-test"
)
reuse_embeddings <- identical(
  tolower(Sys.getenv("MCSE_REUSE_EMBEDDINGS", unset = "true")),
  "true"
)

stopifnot(file.exists(input_path), dir.exists(model_cache))
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

mcse_data <- tryCatch(
  readRDS(input_path),
  error = function(error_condition) {
    stop(sprintf("RDS import failed: %s", conditionMessage(error_condition)))
  }
)

str(mcse_data)
print(class(mcse_data))
print(dim(mcse_data))
print(names(mcse_data))
print(head(mcse_data[, c("UT", "TI", "PY", "AB")]))
print(vapply(mcse_data, class, character(1)))

stopifnot(
  is.data.frame(mcse_data),
  identical(dim(mcse_data), c(16453L, 54L)),
  all(c("AB", "UT", "TI", "PY", "AU", "SO", "DI", "LA", "TC") %in%
    names(mcse_data)),
  is.character(mcse_data$AB),
  is.character(mcse_data$UT),
  nrow(mcse_data) == length(mcse_data$UT)
)

raw_abstract <- mcse_data$AB
trimmed_abstract <- trimws(raw_abstract)
missing_abstract <- is.na(raw_abstract) | !nzchar(trimmed_abstract)
placeholder_abstract <- !missing_abstract &
  toupper(trimmed_abstract) == "[NO ABSTRACT AVAILABLE]"
content_abstract <- !missing_abstract & !placeholder_abstract
cleaned_abstract <- rep(NA_character_, nrow(mcse_data))
cleaned_abstract[content_abstract] <- clean_bibliographic_abstract(
  raw_abstract[content_abstract]
)

stopifnot(
  sum(missing_abstract) == 152L,
  sum(placeholder_abstract) == 641L,
  sum(content_abstract) == 15660L,
  sum(missing_abstract) + sum(placeholder_abstract) +
    sum(content_abstract) == nrow(mcse_data),
  all(nzchar(cleaned_abstract[content_abstract])),
  all(nzchar(mcse_data$UT[content_abstract])),
  anyDuplicated(mcse_data$UT[content_abstract]) == 0L
)

unique_abstract <- unique(cleaned_abstract[content_abstract])
abstract_id_by_row <- rep(NA_integer_, nrow(mcse_data))
abstract_id_by_row[content_abstract] <- match(
  cleaned_abstract[content_abstract],
  unique_abstract
)
source_frequency <- tabulate(
  abstract_id_by_row[content_abstract],
  nbins = length(unique_abstract)
)

print(c(
  source_rows = nrow(mcse_data),
  missing_abstracts = sum(missing_abstract),
  placeholder_abstracts = sum(placeholder_abstract),
  modeled_source_rows = sum(content_abstract),
  distinct_clean_abstracts = length(unique_abstract),
  duplicate_occurrences = sum(content_abstract) - length(unique_abstract)
))
print(summary(nchar(unique_abstract, type = "chars")))
print(summary(source_frequency))

model <- load_model("all-MiniLM-L6-v2", threads = 2L)
model_id <- model$id
model_revision <- model$revision
model_dimension <- model$dimension
model_max_length <- model$max_length
model$tokenizer$no_truncation()
token_count <- function(text) {
  stopifnot(is.character(text), length(text) == 1L, !is.na(text))
  encoding <- model$tokenizer$encode(text)
  as.integer(sum(encoding$attention_mask))
}

if (reuse_embeddings && file.exists(embedding_path)) {
  embedding_cache <- readRDS(embedding_path)
  str(embedding_cache, max.level = 1L)
  stopifnot(
    identical(embedding_cache$source_path, input_path),
    identical(embedding_cache$unique_abstract, unique_abstract),
    identical(embedding_cache$model_revision, model_revision),
    is.matrix(embedding_cache$abstract_embeddings)
  )
  abstract_embeddings <- embedding_cache$abstract_embeddings
  chunk_table <- embedding_cache$chunk_table
  embedding_seconds <- embedding_cache$embedding_seconds
} else {
  chunk_lists <- lapply(
    unique_abstract,
    pack_abstract_chunks,
    token_count = token_count,
    max_tokens = model_max_length
  )
  chunk_table <- data.frame(
    abstract_id = rep.int(seq_along(chunk_lists), lengths(chunk_lists)),
    chunk_index = unlist(
      lapply(lengths(chunk_lists), seq_len),
      use.names = FALSE
    ),
    chunk_text = unlist(chunk_lists, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  chunk_table$token_count <- vapply(
    chunk_table$chunk_text,
    token_count,
    integer(1)
  )
  stopifnot(
    all(chunk_table$token_count <= model_max_length),
    all(tabulate(chunk_table$abstract_id, nbins = length(unique_abstract)) >= 1L)
  )

  rm(token_count, model)
  invisible(gc())
  chunk_manifest_path <- file.path(output_directory, "mcse_chunk_manifest.rds")
  batch_directory <- file.path(output_directory, "embedding_batches_400")
  dir.create(batch_directory, recursive = TRUE, showWarnings = FALSE)
  saveRDS(chunk_table, chunk_manifest_path)
  embedding_batch_size <- 400L
  batch_start <- seq.int(1L, nrow(chunk_table), by = embedding_batch_size)
  batch_end <- pmin(
    batch_start + embedding_batch_size - 1L,
    nrow(chunk_table)
  )
  batch_paths <- file.path(
    batch_directory,
    sprintf("batch_%04d.rds", seq_along(batch_start))
  )
  batch_logs <- file.path(
    batch_directory,
    sprintf("batch_%04d.log", seq_along(batch_start))
  )
  embedding_time <- system.time(
    invisible(lapply(
      seq_along(batch_paths),
      function(batch_index) {
        if (!file.exists(batch_paths[[batch_index]])) {
          exit_status <- system2(
            "Rscript",
            args = c(
              "--vanilla",
              file.path("analysis", "mcse_embed_batch.R"),
              chunk_manifest_path,
              batch_paths[[batch_index]],
              as.character(batch_start[[batch_index]]),
              as.character(batch_end[[batch_index]]),
              model_cache
            ),
            stdout = batch_logs[[batch_index]],
            stderr = batch_logs[[batch_index]]
          )
          if (!identical(exit_status, 0L)) {
            stop(
              sprintf(
                "Embedding batch %d failed; inspect %s.",
                batch_index,
                batch_logs[[batch_index]]
              ),
              call. = FALSE
            )
          }
        }
        stopifnot(file.exists(batch_paths[[batch_index]]))
        invisible(TRUE)
      }
    ))
  )
  embedding_parts <- lapply(batch_paths, readRDS)
  stopifnot(
    identical(
      vapply(embedding_parts, `[[`, integer(1), "start_row"),
      batch_start
    ),
    identical(
      vapply(embedding_parts, `[[`, integer(1), "end_row"),
      batch_end
    )
  )
  chunk_embeddings <- do.call(
    rbind,
    lapply(embedding_parts, `[[`, "embeddings")
  )
  stopifnot(nrow(chunk_embeddings) == nrow(chunk_table))
  content_token_weight <- pmax(1L, chunk_table$token_count - 2L)
  weighted_chunks <- chunk_embeddings * content_token_weight
  abstract_sums <- rowsum(
    weighted_chunks,
    group = factor(
      chunk_table$abstract_id,
      levels = seq_along(unique_abstract)
    ),
    reorder = FALSE
  )
  abstract_embeddings <- normalize_sentence_embeddings(abstract_sums)
  embedding_seconds <- unname(embedding_time[["elapsed"]])

  saveRDS(
    list(
      source_path = input_path,
      model_id = model_id,
      model_revision = model_revision,
      model_max_length = model_max_length,
      unique_abstract = unique_abstract,
      chunk_table = chunk_table,
      abstract_embeddings = abstract_embeddings,
      embedding_seconds = embedding_seconds,
      created_at = Sys.time()
    ),
    embedding_path
  )
}

str(chunk_table)
print(dim(chunk_table))
print(head(chunk_table))
print(summary(chunk_table$token_count))
print(dim(abstract_embeddings))
print(summary(as.numeric(abstract_embeddings)))
print(summary(sqrt(rowSums(abstract_embeddings^2))))

stopifnot(
  identical(dim(abstract_embeddings), c(length(unique_abstract), 384L)),
  nrow(chunk_table) >= length(unique_abstract),
  all(chunk_table$token_count <= 256L),
  all(is.finite(abstract_embeddings)),
  !anyNA(abstract_embeddings),
  max(abs(sqrt(rowSums(abstract_embeddings^2)) - 1)) < 1e-6
)

fit_time <- system.time(
  spherical_fit <- fit_spherical_sentence_topics(
    abstract_embeddings,
    n_topics = 30L,
    seeds = 8:57,
    iter_max = 100L
  )
)
restart_quality <- do.call(
  rbind,
  lapply(
    seq_along(spherical_fit$all_fits),
    function(fit_index) {
      fit <- spherical_fit$all_fits[[fit_index]]
      topic_size <- tabulate(fit$topic, nbins = 30L)
      topic_proportion <- topic_size / sum(topic_size)
      data.frame(
        fit_index = fit_index,
        min_topic_size = min(topic_size),
        max_topic_size = max(topic_size),
        normalized_size_entropy = -sum(
          topic_proportion * log(topic_proportion)
        ) / log(30),
        stringsAsFactors = FALSE
      )
    }
  )
)
restart_diagnostics <- cbind(
  spherical_fit$restart_diagnostics[, c(
    "seed", "objective", "iterations", "converged"
  )],
  restart_quality
)
near_optimal <- restart_diagnostics$objective >=
  0.995 * max(restart_diagnostics$objective)
size_eligible <- restart_diagnostics$min_topic_size >= 30L
candidate_index <- which(
  near_optimal & size_eligible & restart_diagnostics$converged
)
if (length(candidate_index) == 0L) {
  stop(
    paste0(
      "No converged restart within 0.5% of the best objective has a ",
      "minimum topic size of 30."
    ),
    call. = FALSE
  )
}
selected_index <- candidate_index[order(
  -restart_diagnostics$normalized_size_entropy[candidate_index],
  -restart_diagnostics$objective[candidate_index],
  restart_diagnostics$seed[candidate_index]
)[[1L]]]
restart_diagnostics$selected <- seq_len(nrow(restart_diagnostics)) ==
  selected_index
selected_fit <- spherical_fit$all_fits[[selected_index]]
restart_diagnostics$ari_to_selected <- vapply(
  spherical_fit$all_fits,
  function(fit) adjusted_rand_index(selected_fit$topic, fit$topic),
  numeric(1)
)
restart_diagnostics$objective_ratio_to_best <-
  restart_diagnostics$objective / max(restart_diagnostics$objective)
raw_topic <- selected_fit$topic
raw_row_topic <- raw_topic[abstract_id_by_row[content_abstract]]
raw_row_sizes <- tabulate(raw_row_topic, nbins = 30L)
raw_tie_break <- vapply(
  seq_len(30L),
  function(topic_id) min(which(raw_topic == topic_id)),
  integer(1)
)
topic_order <- order(-raw_row_sizes, raw_tie_break)
topic_map <- integer(30L)
topic_map[topic_order] <- seq_len(30L)
abstract_topic <- as.integer(topic_map[raw_topic])
topic_centers <- selected_fit$centers[topic_order, , drop = FALSE]

all_similarities <- abstract_embeddings %*% t(topic_centers)
chosen_similarity <- all_similarities[cbind(
  seq_len(nrow(all_similarities)),
  abstract_topic
)]
alternative_similarities <- all_similarities
alternative_similarities[cbind(
  seq_len(nrow(alternative_similarities)),
  abstract_topic
)] <- -Inf
second_similarity <- apply(alternative_similarities, 1L, max)
alternative_topic <- max.col(alternative_similarities, ties.method = "first")
abstract_distance <- pmax(0, 1 - chosen_similarity)
abstract_margin <- chosen_similarity - second_similarity

stopifnot(
  identical(sort(unique(abstract_topic)), seq_len(30L)),
  all(tabulate(abstract_topic, nbins = 30L) > 0L),
  all(abstract_distance >= 0),
  all(abstract_margin >= 0),
  isTRUE(selected_fit$converged),
  selected_fit$objective >= 0.995 * max(restart_diagnostics$objective),
  sum(restart_diagnostics$selected) == 1L
)

canonical_source_row <- match(seq_along(unique_abstract), abstract_id_by_row)
canonical_metadata <- data.frame(
  abstract_id = seq_along(unique_abstract),
  source_row_id = canonical_source_row,
  source_record_id = mcse_data$UT[canonical_source_row],
  title = mcse_data$TI[canonical_source_row],
  authors = mcse_data$AU[canonical_source_row],
  year = mcse_data$PY[canonical_source_row],
  source = mcse_data$SO[canonical_source_row],
  doi = mcse_data$DI[canonical_source_row],
  language = mcse_data$LA[canonical_source_row],
  citations = mcse_data$TC[canonical_source_row],
  raw_abstract = raw_abstract[canonical_source_row],
  cleaned_abstract = unique_abstract,
  source_record_count = source_frequency,
  chunk_count = tabulate(chunk_table$abstract_id, nbins = length(unique_abstract)),
  topic = abstract_topic,
  cosine_distance_to_centroid = abstract_distance,
  assignment_margin = abstract_margin,
  alternative_topic = alternative_topic,
  alternative_similarity = second_similarity,
  stringsAsFactors = FALSE
)

# Select four diverse canonical abstracts per topic. MMR uses embeddings only;
# full abstracts and bibliographic metadata remain attached for interpretation.
representative_tables <- lapply(
  seq_len(30L),
  function(topic_id) {
    candidate_ids <- which(abstract_topic == topic_id)
    relevance <- as.numeric(
      abstract_embeddings[candidate_ids, , drop = FALSE] %*%
        topic_centers[topic_id, ]
    )
    central_count <- max(4L, ceiling(length(candidate_ids) * 0.25))
    central_order <- order(-relevance, candidate_ids)
    central_ids <- candidate_ids[utils::head(central_order, central_count)]
    central_relevance <- relevance[utils::head(central_order, central_count)]
    selected_ids <- Reduce(
      function(selected, unused_rank) {
        remaining <- central_ids[!central_ids %in% selected]
        candidate_relevance <- central_relevance[match(remaining, central_ids)]
        redundancy <- apply(
          abstract_embeddings[remaining, , drop = FALSE] %*%
            t(abstract_embeddings[selected, , drop = FALSE]),
          1L,
          max
        )
        mmr_score <- 0.70 * candidate_relevance - 0.30 * redundancy
        c(selected, remaining[order(-mmr_score, remaining)[[1L]]])
      },
      seq_len(3L),
      init = central_ids[[1L]]
    )
    data.frame(
      topic = rep.int(topic_id, length(selected_ids)),
      rank = seq_along(selected_ids),
      canonical_metadata[
        selected_ids,
        setdiff(names(canonical_metadata), "topic"),
        drop = FALSE
      ],
      stringsAsFactors = FALSE
    )
  }
)
representatives <- do.call(rbind, representative_tables)
rownames(representatives) <- NULL

boundary_tables <- lapply(
  seq_len(30L),
  function(topic_id) {
    candidate_ids <- which(abstract_topic == topic_id)
    selected_ids <- candidate_ids[utils::head(
      order(abstract_margin[candidate_ids], candidate_ids),
      2L
    )]
    data.frame(
      topic = rep.int(topic_id, length(selected_ids)),
      rank = seq_along(selected_ids),
      canonical_metadata[
        selected_ids,
        setdiff(names(canonical_metadata), "topic"),
        drop = FALSE
      ],
      stringsAsFactors = FALSE
    )
  }
)
boundary_abstracts <- do.call(rbind, boundary_tables)
rownames(boundary_abstracts) <- NULL

canonical_rows <- representatives[representatives$rank == 1L, , drop = FALSE]
canonical_rows <- canonical_rows[order(canonical_rows$topic), , drop = FALSE]
canonical_lead_sentence <- first_complete_sentence(
  canonical_rows$cleaned_abstract
)
topic_label <- sprintf(
  "Topic %d — %s",
  seq_len(30L),
  canonical_lead_sentence
)
representatives$topic_label <- topic_label[representatives$topic]
boundary_abstracts$topic_label <- topic_label[boundary_abstracts$topic]
canonical_metadata$topic_label <- topic_label[canonical_metadata$topic]

ambiguity_threshold <- unname(stats::quantile(
  abstract_margin,
  probs = 0.1,
  names = FALSE
))
canonical_metadata$assignment_status <- ifelse(
  canonical_metadata$assignment_margin <= ambiguity_threshold,
  "ambiguous",
  "assigned"
)

row_assignment_match <- abstract_id_by_row
source_assignments <- data.frame(
  source_row_id = seq_len(nrow(mcse_data)),
  source_record_id = mcse_data$UT,
  included = content_abstract,
  exclusion_reason = ifelse(
    missing_abstract,
    "missing abstract",
    ifelse(placeholder_abstract, "placeholder abstract", "")
  ),
  abstract_id = row_assignment_match,
  title = mcse_data$TI,
  authors = mcse_data$AU,
  year = mcse_data$PY,
  source = mcse_data$SO,
  doi = mcse_data$DI,
  raw_abstract = raw_abstract,
  cleaned_abstract = cleaned_abstract,
  topic = abstract_topic[row_assignment_match],
  topic_label = topic_label[abstract_topic[row_assignment_match]],
  cosine_distance_to_centroid = abstract_distance[row_assignment_match],
  assignment_margin = abstract_margin[row_assignment_match],
  alternative_topic = alternative_topic[row_assignment_match],
  alternative_similarity = second_similarity[row_assignment_match],
  assignment_status = ifelse(
    !content_abstract,
    "excluded",
    ifelse(
      abstract_margin[row_assignment_match] <= ambiguity_threshold,
      "ambiguous",
      "assigned"
    )
  ),
  stringsAsFactors = FALSE
)

topic_rows <- tabulate(
  source_assignments$topic[source_assignments$included],
  nbins = 30L
)
topic_distinct <- tabulate(abstract_topic, nbins = 30L)
topic_mean_distance <- vapply(
  seq_len(30L),
  function(topic_id) mean(abstract_distance[abstract_topic == topic_id]),
  numeric(1)
)
topic_median_distance <- vapply(
  seq_len(30L),
  function(topic_id) stats::median(abstract_distance[abstract_topic == topic_id]),
  numeric(1)
)
topic_p90_distance <- vapply(
  seq_len(30L),
  function(topic_id) unname(stats::quantile(
    abstract_distance[abstract_topic == topic_id],
    probs = 0.9,
    names = FALSE
  )),
  numeric(1)
)
topic_year_min <- vapply(
  seq_len(30L),
  function(topic_id) min(canonical_metadata$year[
    canonical_metadata$topic == topic_id
  ], na.rm = TRUE),
  numeric(1)
)
topic_year_max <- vapply(
  seq_len(30L),
  function(topic_id) max(canonical_metadata$year[
    canonical_metadata$topic == topic_id
  ], na.rm = TRUE),
  numeric(1)
)

topic_summary <- data.frame(
  topic = seq_len(30L),
  topic_label = topic_label,
  canonical_lead_sentence = canonical_lead_sentence,
  canonical_paper_title = canonical_rows$title,
  n_source_records = topic_rows,
  source_record_share = topic_rows / sum(topic_rows),
  n_distinct_abstracts = topic_distinct,
  mean_cosine_distance = topic_mean_distance,
  median_cosine_distance = topic_median_distance,
  p90_cosine_distance = topic_p90_distance,
  year_min = topic_year_min,
  year_max = topic_year_max,
  stringsAsFactors = FALSE
)

ngram_grid <- expand.grid(
  topic = seq_len(30L),
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
        unique_abstract[abstract_topic == topic_id],
        n = ngram_length,
        top_n = 10L,
        stopwords = stop_words()
      )
      data.frame(
        topic = rep.int(topic_id, nrow(frequency_table)),
        n = rep.int(ngram_length, nrow(frequency_table)),
        frequency_table,
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(frequent_ngrams) <- NULL

data_audit <- data.frame(
  metric = c(
    "source_rows", "missing_abstracts", "placeholder_abstracts",
    "modeled_source_rows", "distinct_clean_abstracts", "duplicate_occurrences",
    "embedded_chunks", "abstracts_with_multiple_chunks", "model_max_tokens"
  ),
  value = c(
    nrow(mcse_data), sum(missing_abstract), sum(placeholder_abstract),
    sum(content_abstract), length(unique_abstract),
    sum(content_abstract) - length(unique_abstract), nrow(chunk_table),
    sum(tabulate(chunk_table$abstract_id) > 1L), model_max_length
  ),
  stringsAsFactors = FALSE
)

method <- data.frame(
  item = c(
    "source_file", "source_field", "semantic_unit", "embedding_model",
    "embedding_revision", "embedding_dimension", "model_max_tokens",
    "long_abstract_handling", "duplicate_handling", "clustering",
    "topic_count", "restarts", "restart_selection", "selected_seed", "selected_objective",
    "embedding_seconds", "fit_seconds", "automatic_label",
    "ngram_role", "ambiguity_rule"
  ),
  value = c(
    input_path, "AB", "complete cleaned abstract",
    model_id, model_revision, as.character(model_dimension),
    as.character(model_max_length),
    "Consecutive sentence-aware chunks; content-token-weighted mean; L2 normalization",
    "Fit distinct cleaned abstracts equally, then map to all source records",
    "Cosine-aware multi-start spherical k-means", "30", "50",
    "Highest size entropy among fits within 0.5% of the best objective and minimum topic size >= 30",
    as.character(selected_fit$seed), sprintf("%.8f", selected_fit$objective),
    sprintf("%.3f", embedding_seconds),
    sprintf("%.3f", unname(fit_time[["elapsed"]])),
    "Exact lead sentence from the centroid-nearest corpus abstract",
    "Secondary raw frequency appendix only; not used for assignments or labels",
    sprintf("Lowest 10%% of distinct-abstract margins (<= %.6f)", ambiguity_threshold)
  ),
  stringsAsFactors = FALSE
)

result <- list(
  source_assignments = source_assignments,
  canonical_abstracts = canonical_metadata,
  topic_summary = topic_summary,
  representatives = representatives,
  boundary_abstracts = boundary_abstracts,
  frequent_ngrams = frequent_ngrams,
  review_queue = source_assignments[
    source_assignments$assignment_status != "assigned",
    ,
    drop = FALSE
  ],
  restart_diagnostics = restart_diagnostics,
  data_audit = data_audit,
  model = list(
    centers = topic_centers,
    selected_seed = selected_fit$seed,
    selected_fit_index = selected_index,
    selected_iterations = selected_fit$iterations,
    selected_converged = selected_fit$converged,
    objective = selected_fit$objective,
    ambiguity_threshold = ambiguity_threshold
  ),
  settings = list(
    source_path = input_path,
    source_field = "AB",
    embedding_cache_path = embedding_path,
    model_id = model_id,
    model_revision = model_revision,
    embedding_dimension = model_dimension,
    model_max_length = model_max_length,
    n_topics = 30L,
    restart_seeds = 8:57,
    minimum_topic_size = 30L,
    near_optimal_ratio = 0.995
  ),
  method = method
)

str(result, max.level = 2L)
print(result$data_audit)
print(result$topic_summary)
print(result$representatives[, c(
  "topic", "rank", "title", "cosine_distance_to_centroid"
)])
print(table(result$source_assignments$assignment_status, useNA = "ifany"))

stopifnot(
  nrow(result$source_assignments) == 16453L,
  sum(result$source_assignments$included) == 15660L,
  nrow(result$canonical_abstracts) == length(unique_abstract),
  nrow(result$topic_summary) == 30L,
  sum(result$topic_summary$n_source_records) == 15660L,
  sum(result$topic_summary$n_distinct_abstracts) == length(unique_abstract),
  nrow(result$representatives) == 120L,
  nrow(result$boundary_abstracts) == 60L,
  nrow(result$frequent_ngrams) == 900L,
  anyDuplicated(names(result$representatives)) == 0L,
  anyDuplicated(names(result$boundary_abstracts)) == 0L,
  isTRUE(result$model$selected_converged),
  all(result$representatives$cleaned_abstract %in% unique_abstract),
  !any(result$representatives$cleaned_abstract == "[NO ABSTRACT AVAILABLE]"),
  !any(grepl("c_tfidf|c-TF-IDF", names(result), ignore.case = TRUE))
)

saveRDS(result, result_path)
export_tables <- list(
  source_assignments = result$source_assignments,
  canonical_abstracts = result$canonical_abstracts,
  topic_summary = result$topic_summary,
  representatives = result$representatives,
  boundary_abstracts = result$boundary_abstracts,
  frequent_ngrams_secondary = result$frequent_ngrams,
  review_queue = result$review_queue,
  restart_diagnostics = result$restart_diagnostics,
  data_audit = result$data_audit,
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
