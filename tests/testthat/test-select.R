select_test_corpus <- function() {
  c(
    "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
    "Stocks and bonds trade", "Markets price shares", "Banks report profit"
  )
}

select_test_embeddings <- function() {
  rbind(
    c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
    c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
  )
}

testthat::test_that("topic-count comparison returns one tidy row per candidate", {
  result <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(
    names(result),
    c("n_topics", "coherence", "topic_diversity", "explained")
  )
  testthat::expect_identical(result$n_topics, 2:3)
  testthat::expect_true(all(is.finite(result$coherence)))
  testthat::expect_true(all(result$topic_diversity > 0 & result$topic_diversity <= 1))
  testthat::expect_true(all(result$explained >= 0 & result$explained <= 1))
  testthat::expect_identical(attr(result, "measure"), "npmi")
})

testthat::test_that("candidates are sorted and deterministic", {
  shuffled <- select_topics(
    select_test_corpus(),
    n_topics = c(3L, 2L),
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  ordered <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(shuffled, ordered)
})

testthat::test_that("umass measure is recorded", {
  result <- select_topics(
    select_test_corpus(),
    n_topics = 2L,
    embeddings = select_test_embeddings(),
    measure = "umass",
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(attr(result, "measure"), "umass")
})

testthat::test_that("invalid candidate sets are rejected", {
  corpus <- select_test_corpus()
  embeddings <- select_test_embeddings()
  testthat::expect_error(
    select_topics(corpus, n_topics = 1:2, embeddings = embeddings)
  )
  testthat::expect_error(
    select_topics(corpus, n_topics = c(2L, 6L), embeddings = embeddings)
  )
  testthat::expect_error(
    select_topics(corpus, n_topics = c(2L, 2L), embeddings = embeddings)
  )
  testthat::expect_error(
    select_topics(corpus, n_topics = 2.5, embeddings = embeddings)
  )
})

testthat::test_that("the sweep retains one fitted model per candidate", {
  sweep <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_s3_class(sweep, "sbert_topic_sweep")
  testthat::expect_s3_class(sweep, "data.frame")

  two <- fitted(sweep, n_topics = 2)
  three <- fitted(sweep, n_topics = 3)
  testthat::expect_s3_class(two, "sbert_topic_model")
  testthat::expect_identical(nrow(two$topics), 2L)
  testthat::expect_identical(nrow(three$topics), 3L)
})

testthat::test_that("retained models equal a direct refit", {
  sweep <- select_topics(
    select_test_corpus(),
    n_topics = 2L,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  direct <- topics(
    select_test_corpus(),
    n_topics = 2L,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    n_representatives = 1L,
    min_term_frequency = 1L
  )
  testthat::expect_equal(fitted(sweep, n_topics = 2), direct)
})

testthat::test_that("the comparison table matches the retained models", {
  sweep <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  recomputed <- vapply(
    sweep$n_topics,
    function(k) {
      topic_diversity(fitted(sweep, n_topics = k), n_terms = 3)
    },
    numeric(1)
  )
  testthat::expect_equal(recomputed, sweep$topic_diversity)
})

testthat::test_that("keep_models = FALSE returns the plain tidy table", {
  result <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    keep_models = FALSE,
    min_term_frequency = 1L
  )
  testthat::expect_s3_class(result, "sbert_topic_sweep")
  testthat::expect_null(attr(result, "models"))
  testthat::expect_error(
    fitted(result, n_topics = 2),
    "kept no models"
  )
})

testthat::test_that("unknown or malformed candidates are rejected", {
  sweep <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_error(fitted(sweep, n_topics = 5), "no model for")
  testthat::expect_error(fitted(sweep, n_topics = 2.5))
  testthat::expect_error(fitted(sweep, n_topics = c(2, 3)))
})

testthat::test_that("as.data.frame drops the models and keeps the columns", {
  sweep <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  plain <- as.data.frame(sweep)
  testthat::expect_identical(class(plain), "data.frame")
  testthat::expect_null(attr(plain, "models"))
  testthat::expect_identical(
    names(plain),
    c("n_topics", "coherence", "topic_diversity", "explained")
  )
})

testthat::test_that("the sweep plots without error", {
  sweep <- select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 1200, height = 420)
  testthat::expect_silent(plot(sweep))
  grDevices::dev.off()
  testthat::expect_true(file.exists(path))
})
