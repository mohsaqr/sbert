project_root <- Sys.getenv("SBERT_PROJECT_ROOT", unset = ".")
source(file.path(project_root, "analysis", "sentence_level_topic_functions.R"))

output_directory <- file.path(
  project_root,
  "outputs",
  "feedback_translation_topics",
  "sentence_level_topics"
)
result_path <- file.path(output_directory, "sentence_level_topic_model.rds")

testthat::test_that("frequent n-grams are raw, deterministic counts", {
  synthetic_text <- c(
    "Choose the red box",
    "Choose the blue box",
    "Choose the red box"
  )
  unigrams <- frequent_sentence_ngrams(
    synthetic_text,
    n = 1L,
    top_n = 4L,
    stopwords = "the"
  )
  bigrams <- frequent_sentence_ngrams(
    synthetic_text,
    n = 2L,
    top_n = 4L,
    stopwords = "the"
  )

  testthat::expect_identical(
    unigrams$ngram,
    c("box", "choose", "red", "blue")
  )
  testthat::expect_identical(unigrams$frequency, c(3L, 3L, 2L, 1L))
  testthat::expect_identical(bigrams$ngram[[1L]], "choose the")
  testthat::expect_identical(bigrams$frequency[[1L]], 3L)
  testthat::expect_false(any(grepl("tfidf|tf-idf", names(unigrams))))
})

testthat::test_that("representatives are complete distinct sentences", {
  synthetic_assignments <- data.frame(
    row_id = 1:5,
    feedback = letters[1:5],
    translation = c("alpha sentence", "alpha sentence", "beta", "gamma", "delta"),
    topic = c(1L, 1L, 1L, 2L, 2L),
    cosine_distance_to_centroid = c(0.2, 0.1, 0.3, 0.4, 0.2),
    stringsAsFactors = FALSE
  )
  representatives <- rank_topic_sentences(
    synthetic_assignments,
    n_topics = 2L,
    n_representatives = 2L
  )

  testthat::expect_identical(
    representatives$representative_sentence,
    c("alpha sentence", "beta", "delta", "gamma")
  )
  testthat::expect_identical(representatives$row_id, c(2L, 3L, 5L, 4L))
  testthat::expect_identical(representatives$rank, c(1L, 2L, 1L, 2L))
})

testthat::test_that("spherical clustering recovers synthetic sentence groups", {
  synthetic_embeddings <- rbind(
    c(1, 0, 0),
    c(0.99, 0.01, 0),
    c(0, 1, 0),
    c(0.01, 0.99, 0),
    c(0, 0, 1),
    c(0, 0.01, 0.99)
  )
  expected_group <- rep(seq_len(3L), each = 2L)
  fit_one <- fit_spherical_sentence_topics(
    synthetic_embeddings,
    n_topics = 3L,
    seeds = 1:5
  )
  fit_two <- fit_spherical_sentence_topics(
    synthetic_embeddings,
    n_topics = 3L,
    seeds = 1:5
  )
  predicted_same <- outer(
    fit_one$best_fit$topic,
    fit_one$best_fit$topic,
    `==`
  )

  testthat::expect_true(all(predicted_same == outer(
    expected_group,
    expected_group,
    `==`
  )))
  testthat::expect_identical(fit_one, fit_two)
  testthat::expect_equal(
    unname(sqrt(rowSums(fit_one$best_fit$centers^2))),
    rep(1, 3L),
    tolerance = 1e-12
  )
  testthat::expect_true(all(fit_one$best_fit$assignment_margin >= 0))
  testthat::expect_equal(
    fit_one$best_fit$objective,
    max(fit_one$restart_diagnostics$objective)
  )
})

testthat::test_that("spherical restarts preserve the caller random state", {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  synthetic_embeddings <- matrix(
    c(1, 0, 0.9, 0.1, 0, 1, 0.1, 0.9),
    nrow = 4L,
    byrow = TRUE
  )
  fit_spherical_sentence_topics(
    synthetic_embeddings,
    n_topics = 2L,
    seeds = 1:2
  )

  testthat::expect_false(exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
})

testthat::test_that("sentence-level output retains all source rows", {
  testthat::expect_true(file.exists(result_path))
  result <- readRDS(result_path)
  assignments <- result$sentence_assignments

  testthat::expect_identical(nrow(assignments), 8987L)
  testthat::expect_identical(assignments$row_id, seq_len(8987L))
  testthat::expect_equal(sum(assignments$included), 8976L)
  testthat::expect_equal(sum(!assignments$included), 11L)
  testthat::expect_equal(sum(result$topic_summary$n_rows), 8976L)
  testthat::expect_identical(
    as.integer(table(assignments$topic)),
    c(2517L, 2350L, 2009L, 1076L, 529L, 495L)
  )
})

testthat::test_that("topics are defined by representative full sentences", {
  result <- readRDS(result_path)

  testthat::expect_equal(nrow(result$topic_summary), 6L)
  testthat::expect_equal(nrow(result$representative_sentences), 30L)
  testthat::expect_equal(nrow(result$boundary_sentences), 18L)
  testthat::expect_identical(
    as.integer(table(result$representative_sentences$topic)),
    rep.int(5L, 6L)
  )
  testthat::expect_true(all(nzchar(
    result$topic_summary$canonical_sentence
  )))
  testthat::expect_true(all(
    result$topic_summary$canonical_sentence %in%
      result$sentence_assignments$translation
  ))
  testthat::expect_true(all(
    result$topic_summary$canonical_sentence %in%
      result$representative_sentences$representative_sentence
  ))
  testthat::expect_false(any(
    result$representative_sentences$embedding_id %in%
      result$unique_sentence_assignments$embedding_id[
        result$unique_sentence_assignments$held_out_translation_review
      ]
  ))
  testthat::expect_false("topic_terms" %in% names(result))
  testthat::expect_false(any(grepl(
    "c_tfidf|c-tf-idf",
    names(result),
    ignore.case = TRUE
  )))
})

testthat::test_that("frequency n-grams are supplementary and complete", {
  result <- readRDS(result_path)
  ngrams <- result$frequent_ngrams

  testthat::expect_equal(nrow(ngrams), 180L)
  testthat::expect_identical(sort(unique(ngrams$topic)), 1:6)
  testthat::expect_identical(sort(unique(ngrams$n)), 1:3)
  testthat::expect_true(all(ngrams$frequency >= 1L))
  testthat::expect_identical(
    as.integer(table(interaction(ngrams$topic, ngrams$n))),
    rep.int(10L, 18L)
  )
  testthat::expect_match(
    result$method$value[result$method$item == "ngram_role"],
    "never used for assignment or titles"
  )
})

testthat::test_that("review queue captures distant and translation-QA rows", {
  result <- readRDS(result_path)
  review_queue <- result$review_queue

  testthat::expect_equal(nrow(review_queue), 966L)
  testthat::expect_true(all(
    review_queue$assignment_status %in%
      c("ambiguous", "translation_review", "excluded_blank")
  ))
  testthat::expect_equal(
    as.integer(table(factor(
      result$sentence_assignments$assignment_status,
      levels = c(
        "ambiguous", "assigned", "excluded_blank", "translation_review"
      )
    ))),
    c(892L, 8021L, 11L, 63L)
  )
  testthat::expect_equal(
    sum(result$sentence_assignments$translation_review_flag),
    179L
  )
})

testthat::test_that("serialized and CSV sentence assignments agree", {
  result <- readRDS(result_path)
  csv_path <- file.path(output_directory, "sentence_assignments.csv")
  csv_assignments <- read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  testthat::expect_true(file.exists(csv_path))
  testthat::expect_equal(nrow(csv_assignments), 8987L)
  testthat::expect_equal(
    csv_assignments$sentence_topic_label,
    result$sentence_assignments$sentence_topic_label
  )
  testthat::expect_equal(
    csv_assignments$canonical_sentence,
    result$sentence_assignments$canonical_sentence
  )
})

testthat::test_that("unique-sentence fit and restart diagnostics are valid", {
  result <- readRDS(result_path)
  unique_assignments <- result$unique_sentence_assignments
  centers <- result$model$centers

  testthat::expect_equal(nrow(unique_assignments), 8144L)
  testthat::expect_equal(
    sum(unique_assignments$held_out_translation_review),
    63L
  )
  testthat::expect_identical(sort(unique(unique_assignments$topic)), 1:6)
  testthat::expect_equal(
    unname(sqrt(rowSums(centers^2))),
    rep(1, 6L),
    tolerance = 1e-10
  )
  testthat::expect_equal(nrow(result$restart_diagnostics), 50L)
  testthat::expect_equal(sum(result$restart_diagnostics$selected), 1L)
  testthat::expect_equal(
    result$model$objective,
    max(result$restart_diagnostics$objective)
  )
  testthat::expect_identical(result$model$selected_seed, 40L)
})

testthat::test_that("duplicate sentences always receive identical results", {
  result <- readRDS(result_path)
  included <- result$sentence_assignments[result$sentence_assignments$included, ]
  duplicate_groups <- split(
    included,
    included$translation
  )
  consistent <- vapply(
    duplicate_groups,
    function(group_rows) {
      length(unique(group_rows$topic)) == 1L &&
        length(unique(group_rows$cosine_distance_to_centroid)) == 1L &&
        length(unique(group_rows$assignment_margin)) == 1L
    },
    logical(1)
  )

  testthat::expect_true(all(consistent))
})
