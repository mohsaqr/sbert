candidate_roots <- unique(c(
  Sys.getenv("SBERT_PROJECT_ROOT", unset = ""),
  getwd(),
  file.path(getwd(), "..", "..")
))
candidate_roots <- candidate_roots[nzchar(candidate_roots)]
helper_exists <- vapply(
  candidate_roots,
  function(candidate_root) {
    file.exists(file.path(
      candidate_root,
      "analysis",
      "sentence_level_topic_functions.R"
    ))
  },
  logical(1)
)
stopifnot(any(helper_exists))
project_root <- normalizePath(candidate_roots[which(helper_exists)[[1L]]])
source(file.path(
  project_root,
  "analysis",
  "sentence_level_topic_functions.R"
))
source(file.path(project_root, "R", "topics.R"))

source_path <- Sys.getenv(
  "MCSE_RDS_PATH",
  unset = "/Users/mohammedsaqr/Documents/Documents X/MyData/AKA/STM/MCSE_gold.RDS"
)
output_directory <- file.path(
  project_root,
  "outputs",
  "mcse_abstract_30_topics"
)
embedding_path <- file.path(output_directory, "mcse_abstract_embeddings.rds")
result_path <- file.path(output_directory, "mcse_abstract_30_topics.rds")

mcse_expected_clean_abstracts <- function(source_data) {
  stopifnot(
    is.data.frame(source_data),
    "AB" %in% names(source_data),
    is.character(source_data$AB)
  )
  trimmed_abstract <- trimws(source_data$AB)
  missing_abstract <- is.na(source_data$AB) | !nzchar(trimmed_abstract)
  placeholder_abstract <- !missing_abstract &
    toupper(trimmed_abstract) == "[NO ABSTRACT AVAILABLE]"
  content_abstract <- !missing_abstract & !placeholder_abstract
  cleaned_abstract <- rep(NA_character_, nrow(source_data))
  cleaned_abstract[content_abstract] <- clean_bibliographic_abstract(
    source_data$AB[content_abstract]
  )
  list(
    missing = missing_abstract,
    placeholder = placeholder_abstract,
    content = content_abstract,
    cleaned = cleaned_abstract,
    unique = unique(cleaned_abstract[content_abstract])
  )
}

testthat::test_that("bibliographic cleaning follows observed edge-case behavior", {
  synthetic_abstract <- c(
    " Finding. Copyright 2020 ACM. ",
    "Result. © 2019 IEEE.",
    "Finding without a separating space.©2018 ACM.",
    " A   spaced\nabstract. ",
    "COPYRIGHT LAW IS STUDIED.",
    "[NO ABSTRACT AVAILABLE]",
    NA_character_
  )
  cleaned <- clean_bibliographic_abstract(synthetic_abstract)

  str(cleaned)
  print(cleaned)

  testthat::expect_identical(
    cleaned,
    c(
      "Finding.",
      "Result.",
      "Finding without a separating space.",
      "A spaced abstract.",
      "COPYRIGHT LAW IS STUDIED.",
      "[NO ABSTRACT AVAILABLE]",
      NA_character_
    )
  )
  testthat::expect_identical(
    clean_bibliographic_abstract(cleaned),
    cleaned
  )
})

testthat::test_that("sentence-aware chunking preserves text and token limits", {
  synthetic_token_count <- function(text) {
    stopifnot(is.character(text), length(text) == 1L, !is.na(text))
    as.integer(length(strsplit(
      trimws(text),
      "[[:space:]]+",
      perl = TRUE
    )[[1L]]) + 2L)
  }
  synthetic_abstract <- c(
    "One two. Three four. Five six.",
    "one two three four five six seven",
    "Short. This sentence is much longer now. End."
  )
  chunks <- lapply(
    synthetic_abstract,
    pack_abstract_chunks,
    token_count = synthetic_token_count,
    max_tokens = 6L
  )

  str(chunks)
  print(chunks)

  testthat::expect_identical(
    chunks,
    list(
      c("One two. Three four.", "Five six."),
      c("one two three four", "five six seven"),
      c("Short.", "This sentence is much", "longer now. End.")
    )
  )
  reconstructed <- vapply(chunks, paste, collapse = " ", character(1))
  testthat::expect_identical(reconstructed, synthetic_abstract)
  chunk_token_counts <- unlist(
    lapply(
      chunks,
      function(abstract_chunks) {
        vapply(abstract_chunks, synthetic_token_count, integer(1))
      }
    ),
    use.names = FALSE
  )
  testthat::expect_true(all(chunk_token_counts <= 6L))
  testthat::expect_identical(
    first_complete_sentence(c(
      "First sentence. Second sentence.",
      "Question? Next!",
      "No punctuation"
    )),
    c("First sentence.", "Question?", "No punctuation")
  )
})

testthat::test_that("adjusted Rand agreement is invariant to topic labels", {
  first_partition <- c(1L, 1L, 2L, 2L, 3L, 3L)
  permuted_partition <- c(3L, 3L, 1L, 1L, 2L, 2L)
  crossed_partition <- c(1L, 2L, 1L, 2L, 1L, 2L)
  agreement <- c(
    permuted = adjusted_rand_index(first_partition, permuted_partition),
    crossed = adjusted_rand_index(first_partition, crossed_partition)
  )

  str(agreement)
  print(agreement)

  testthat::expect_identical(unname(agreement[["permuted"]]), 1)
  testthat::expect_lt(agreement[["crossed"]], 1)
})

testthat::test_that("MCSE plot stop words cover requested generic terms", {
  plot_stopwords <- mcse_common_word_stopwords()

  str(plot_stopwords)
  print(plot_stopwords)

  testthat::expect_type(plot_stopwords, "character")
  testthat::expect_identical(plot_stopwords, sort(unique(plot_stopwords)))
  testthat::expect_true(all(c(
    "computer", "student", "students", "coding", "programming",
    "computing", "education", "paper", "study", "course", "results",
    "used", "using"
  ) %in% plot_stopwords))
})

testthat::test_that("MCSE source audit reproduces the analysis population", {
  testthat::skip_if(!file.exists(source_path), "MCSE source RDS is unavailable")
  source_data <- readRDS(source_path)
  abstract_data <- mcse_expected_clean_abstracts(source_data)

  str(source_data$AB)
  print(dim(source_data))
  print(c(
    missing = sum(abstract_data$missing),
    placeholder = sum(abstract_data$placeholder),
    content = sum(abstract_data$content),
    distinct_clean = length(abstract_data$unique)
  ))

  testthat::expect_s3_class(source_data, "data.frame")
  testthat::expect_identical(dim(source_data), c(16453L, 54L))
  testthat::expect_type(source_data$AB, "character")
  testthat::expect_equal(sum(abstract_data$missing), 152L)
  testthat::expect_equal(sum(abstract_data$placeholder), 641L)
  testthat::expect_equal(sum(abstract_data$content), 15660L)
  testthat::expect_equal(length(abstract_data$unique), 15308L)
  testthat::expect_equal(
    sum(abstract_data$content) - length(abstract_data$unique),
    352L
  )
  testthat::expect_false(anyNA(abstract_data$unique))
  testthat::expect_true(all(nzchar(abstract_data$unique)))
  testthat::expect_equal(anyDuplicated(abstract_data$unique), 0L)
  testthat::expect_false(any(
    toupper(abstract_data$unique) == "[NO ABSTRACT AVAILABLE]"
  ))
  testthat::expect_equal(
    anyDuplicated(source_data$UT[abstract_data$content]),
    0L
  )
})

testthat::test_that("MCSE embedding cache is complete and reconstructable", {
  testthat::skip_if(!file.exists(embedding_path), "MCSE embedding cache is unavailable")
  embedding_cache <- readRDS(embedding_path)
  chunk_table <- embedding_cache$chunk_table
  embeddings <- embedding_cache$abstract_embeddings
  unique_abstract <- embedding_cache$unique_abstract

  str(embedding_cache, max.level = 2L)
  print(dim(embeddings))
  print(dim(chunk_table))
  print(summary(chunk_table$token_count))
  print(summary(sqrt(rowSums(embeddings^2))))

  testthat::expect_identical(
    names(embedding_cache),
    c(
      "source_path", "model_id", "model_revision", "model_max_length",
      "unique_abstract", "chunk_table", "abstract_embeddings",
      "embedding_seconds", "created_at"
    )
  )
  testthat::expect_identical(embedding_cache$source_path, source_path)
  testthat::expect_identical(
    embedding_cache$model_id,
    "sentence-transformers/all-MiniLM-L6-v2"
  )
  testthat::expect_identical(embedding_cache$model_max_length, 256L)
  testthat::expect_equal(length(unique_abstract), 15308L)
  testthat::expect_equal(anyDuplicated(unique_abstract), 0L)
  testthat::expect_false(any(unique_abstract == "[NO ABSTRACT AVAILABLE]"))
  testthat::expect_identical(dim(embeddings), c(15308L, 384L))
  testthat::expect_true(all(is.finite(embeddings)))
  testthat::expect_false(anyNA(embeddings))
  testthat::expect_equal(
    unname(sqrt(rowSums(embeddings^2))),
    rep(1, nrow(embeddings)),
    tolerance = 1e-6
  )
  testthat::expect_s3_class(chunk_table, "data.frame")
  testthat::expect_identical(
    names(chunk_table),
    c("abstract_id", "chunk_index", "chunk_text", "token_count")
  )
  testthat::expect_equal(nrow(chunk_table), 17564L)
  testthat::expect_true(all(chunk_table$abstract_id %in% seq_len(15308L)))
  testthat::expect_true(all(nzchar(chunk_table$chunk_text)))
  testthat::expect_false(anyNA(chunk_table))
  testthat::expect_true(all(chunk_table$token_count >= 1L))
  testthat::expect_true(all(chunk_table$token_count <= 256L))

  chunk_rows <- split(
    seq_len(nrow(chunk_table)),
    factor(chunk_table$abstract_id, levels = seq_len(length(unique_abstract)))
  )
  testthat::expect_true(all(lengths(chunk_rows) >= 1L))
  testthat::expect_true(all(vapply(
    chunk_rows,
    function(row_index) {
      identical(
        chunk_table$chunk_index[row_index],
        seq_along(row_index)
      )
    },
    logical(1)
  )))
  reconstructed_abstract <- vapply(
    chunk_rows,
    function(row_index) {
      paste(chunk_table$chunk_text[row_index], collapse = " ")
    },
    character(1)
  )
  testthat::expect_identical(unname(reconstructed_abstract), unique_abstract)

  if (file.exists(source_path)) {
    source_data <- readRDS(source_path)
    expected <- mcse_expected_clean_abstracts(source_data)
    testthat::expect_identical(unique_abstract, expected$unique)
  }
})

testthat::test_that("MCSE result has exactly 30 internally consistent topics", {
  testthat::skip_if(!file.exists(result_path), "MCSE topic result is unavailable")
  result <- readRDS(result_path)
  assignments <- result$source_assignments
  canonical <- result$canonical_abstracts
  topics <- result$topic_summary
  representatives <- result$representatives
  boundaries <- result$boundary_abstracts
  restarts <- result$restart_diagnostics
  centers <- result$model$centers

  str(result, max.level = 2L)
  print(topics)
  print(restarts[restarts$selected, , drop = FALSE])

  testthat::expect_identical(
    names(result),
    c(
      "source_assignments", "canonical_abstracts", "topic_summary",
      "representatives", "boundary_abstracts", "frequent_ngrams",
      "review_queue", "restart_diagnostics", "data_audit", "model",
      "settings", "method"
    )
  )
  testthat::expect_equal(nrow(assignments), 16453L)
  testthat::expect_identical(assignments$source_row_id, seq_len(16453L))
  testthat::expect_equal(sum(assignments$included), 15660L)
  testthat::expect_equal(sum(!assignments$included), 793L)
  testthat::expect_equal(
    as.integer(table(assignments$exclusion_reason)),
    c(15660L, 152L, 641L)
  )
  testthat::expect_true(all(is.na(assignments$topic[!assignments$included])))
  testthat::expect_false(anyNA(assignments$topic[assignments$included]))
  testthat::expect_identical(
    sort(unique(assignments$topic[assignments$included])),
    seq_len(30L)
  )
  testthat::expect_equal(nrow(canonical), 15308L)
  testthat::expect_equal(anyDuplicated(canonical$cleaned_abstract), 0L)
  testthat::expect_identical(sort(unique(canonical$topic)), seq_len(30L))
  testthat::expect_true(all(canonical$cosine_distance_to_centroid >= 0))
  testthat::expect_true(all(canonical$cosine_distance_to_centroid <= 2))
  testthat::expect_true(all(canonical$assignment_margin >= 0))
  testthat::expect_true(all(canonical$alternative_topic %in% seq_len(30L)))
  testthat::expect_true(all(canonical$alternative_topic != canonical$topic))
  testthat::expect_true(all(is.finite(canonical$alternative_similarity)))
  testthat::expect_identical(topics$topic, seq_len(30L))
  testthat::expect_equal(sum(topics$n_source_records), 15660L)
  testthat::expect_equal(sum(topics$n_distinct_abstracts), 15308L)
  testthat::expect_equal(sum(topics$source_record_share), 1, tolerance = 1e-12)
  testthat::expect_identical(
    topics$n_source_records,
    tabulate(assignments$topic[assignments$included], nbins = 30L)
  )
  testthat::expect_identical(
    topics$n_distinct_abstracts,
    tabulate(canonical$topic, nbins = 30L)
  )
  testthat::expect_identical(dim(centers), c(30L, 384L))
  testthat::expect_true(all(is.finite(centers)))
  testthat::expect_equal(
    unname(sqrt(rowSums(centers^2))),
    rep(1, 30L),
    tolerance = 1e-10
  )

  duplicate_groups <- split(
    assignments[assignments$included, , drop = FALSE],
    assignments$abstract_id[assignments$included]
  )
  testthat::expect_true(all(vapply(
    duplicate_groups,
    function(group_rows) {
      length(unique(group_rows$topic)) == 1L &&
        length(unique(group_rows$cosine_distance_to_centroid)) == 1L &&
        length(unique(group_rows$assignment_margin)) == 1L
    },
    logical(1)
  )))

  testthat::expect_equal(nrow(representatives), 120L)
  testthat::expect_equal(anyDuplicated(names(representatives)), 0L)
  testthat::expect_identical(
    as.integer(table(representatives$topic)),
    rep.int(4L, 30L)
  )
  testthat::expect_true(all(vapply(
    split(representatives$rank, representatives$topic),
    function(rank) identical(rank, seq_len(4L)),
    logical(1)
  )))
  testthat::expect_true(all(representatives$cleaned_abstract %in%
    canonical$cleaned_abstract))
  testthat::expect_false(any(
    representatives$cleaned_abstract == "[NO ABSTRACT AVAILABLE]"
  ))
  rank_one <- representatives[representatives$rank == 1L, , drop = FALSE]
  rank_one <- rank_one[order(rank_one$topic), , drop = FALSE]
  testthat::expect_identical(
    topics$canonical_lead_sentence,
    first_complete_sentence(rank_one$cleaned_abstract)
  )
  testthat::expect_identical(topics$canonical_paper_title, rank_one$title)

  testthat::expect_equal(nrow(boundaries), 60L)
  testthat::expect_equal(anyDuplicated(names(boundaries)), 0L)
  testthat::expect_identical(
    as.integer(table(boundaries$topic)),
    rep.int(2L, 30L)
  )
  expected_boundary_ids <- unlist(
    lapply(
      seq_len(30L),
      function(topic_id) {
        topic_rows <- canonical[canonical$topic == topic_id, , drop = FALSE]
        topic_rows$abstract_id[utils::head(order(
          topic_rows$assignment_margin,
          topic_rows$abstract_id
        ), 2L)]
      }
    ),
    use.names = FALSE
  )
  testthat::expect_identical(boundaries$abstract_id, expected_boundary_ids)

  testthat::expect_equal(nrow(restarts), 50L)
  testthat::expect_equal(sum(restarts$selected), 1L)
  testthat::expect_true(all(is.finite(restarts$objective)))
  testthat::expect_true(all(is.finite(restarts$ari_to_selected)))
  testthat::expect_true(all(restarts$ari_to_selected >= -1))
  testthat::expect_true(all(restarts$ari_to_selected <= 1))
  testthat::expect_equal(restarts$ari_to_selected[restarts$selected], 1)
  best_objective <- max(restarts$objective)
  near_optimal <- restarts$objective >= 0.995 * best_objective
  candidate_index <- which(
    near_optimal & restarts$min_topic_size >= 30L & restarts$converged
  )
  expected_selected <- candidate_index[order(
    -restarts$normalized_size_entropy[candidate_index],
    -restarts$objective[candidate_index],
    restarts$seed[candidate_index]
  )[[1L]]]
  testthat::expect_identical(which(restarts$selected), expected_selected)
  testthat::expect_identical(
    result$model$selected_seed,
    restarts$seed[expected_selected]
  )
  testthat::expect_equal(
    result$model$objective,
    restarts$objective[expected_selected],
    tolerance = 1e-10
  )
  testthat::expect_gte(result$model$objective, 0.995 * best_objective)
  testthat::expect_true(result$model$selected_converged)
  testthat::expect_identical(result$model$selected_fit_index, expected_selected)
  testthat::expect_identical(result$settings$n_topics, 30L)
  testthat::expect_identical(result$settings$source_field, "AB")
  testthat::expect_identical(
    result$settings$model_id,
    "sentence-transformers/all-MiniLM-L6-v2"
  )

  if (file.exists(embedding_path)) {
    embedding_cache <- readRDS(embedding_path)
    similarities <- embedding_cache$abstract_embeddings %*% t(centers)
    recomputed_topic <- max.col(similarities, ties.method = "first")
    recomputed_objective <- sum(similarities[cbind(
      seq_len(nrow(similarities)),
      recomputed_topic
    )])
    testthat::expect_identical(recomputed_topic, canonical$topic)
    testthat::expect_equal(
      recomputed_objective,
      result$model$objective,
      tolerance = 1e-8
    )
  }

  testthat::expect_equal(nrow(result$frequent_ngrams), 900L)
  testthat::expect_identical(
    sort(unique(result$frequent_ngrams$topic)),
    seq_len(30L)
  )
  testthat::expect_identical(
    sort(unique(result$frequent_ngrams$n)),
    seq_len(3L)
  )
  common_word_stopwords <- unique(c(
    sbert_stopwords(),
    mcse_common_word_stopwords()
  ))
  common_word_tables <- lapply(
    seq_len(30L),
    function(topic_id) {
      frequent_sentence_ngrams(
        canonical$cleaned_abstract[canonical$topic == topic_id],
        n = 1L,
        top_n = 8L,
        stopwords = common_word_stopwords
      )
    }
  )
  common_word_data <- do.call(
    rbind,
    lapply(
      seq_along(common_word_tables),
      function(topic_id) {
        data.frame(
          topic = rep.int(topic_id, nrow(common_word_tables[[topic_id]])),
          common_word_tables[[topic_id]],
          stringsAsFactors = FALSE
        )
      }
    )
  )
  common_word_data <- common_word_data[
    order(common_word_data$topic, common_word_data$rank),
    ,
    drop = FALSE
  ]
  str(common_word_data)
  print(head(common_word_data, 16L))
  testthat::expect_equal(nrow(common_word_data), 240L)
  testthat::expect_identical(
    as.integer(table(common_word_data$topic)),
    rep.int(8L, 30L)
  )
  testthat::expect_true(all(common_word_data$frequency > 0L))
  testthat::expect_false(any(
    common_word_data$ngram %in% mcse_common_word_stopwords()
  ))
  testthat::expect_true(all(vapply(
    split(common_word_data$rank, common_word_data$topic),
    identical,
    logical(1),
    seq_len(8L)
  )))
  testthat::expect_true(all(vapply(
    split(common_word_data$frequency, common_word_data$topic),
    function(frequency) all(diff(frequency) <= 0L),
    logical(1)
  )))
  testthat::expect_identical(
    result$review_queue$source_row_id,
    assignments$source_row_id[assignments$assignment_status != "assigned"]
  )
  testthat::expect_false(any(grepl(
    "c_tfidf|c-tf-idf",
    names(result),
    ignore.case = TRUE
  )))
})

testthat::test_that("MCSE serialized result agrees with CSV exports", {
  testthat::skip_if(!file.exists(result_path), "MCSE topic result is unavailable")
  result <- readRDS(result_path)
  export_names <- c(
    "source_assignments", "canonical_abstracts", "topic_summary",
    "representatives", "boundary_abstracts", "frequent_ngrams_secondary",
    "review_queue", "restart_diagnostics", "data_audit", "method"
  )
  export_paths <- file.path(output_directory, paste0(export_names, ".csv"))
  testthat::expect_true(all(file.exists(export_paths)))
  exported <- lapply(
    export_paths,
    read.csv,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  result_tables <- list(
    result$source_assignments,
    result$canonical_abstracts,
    result$topic_summary,
    result$representatives,
    result$boundary_abstracts,
    result$frequent_ngrams,
    result$review_queue,
    result$restart_diagnostics,
    result$data_audit,
    result$method
  )

  testthat::expect_identical(
    vapply(exported, nrow, integer(1)),
    vapply(result_tables, nrow, integer(1))
  )
  testthat::expect_equal(
    exported[[1L]]$topic,
    result$source_assignments$topic
  )
  testthat::expect_equal(
    exported[[2L]]$cleaned_abstract,
    result$canonical_abstracts$cleaned_abstract
  )
  testthat::expect_equal(
    exported[[3L]]$n_source_records,
    result$topic_summary$n_source_records
  )
  testthat::expect_equal(
    exported[[8L]]$selected,
    result$restart_diagnostics$selected
  )
})
