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
  result <- sbert_select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(
    names(result),
    c("n_topics", "coherence", "diversity", "explained")
  )
  testthat::expect_identical(result$n_topics, 2:3)
  testthat::expect_true(all(is.finite(result$coherence)))
  testthat::expect_true(all(result$diversity > 0 & result$diversity <= 1))
  testthat::expect_true(all(result$explained >= 0 & result$explained <= 1))
  testthat::expect_identical(attr(result, "measure"), "npmi")
})

testthat::test_that("candidates are sorted and deterministic", {
  shuffled <- sbert_select_topics(
    select_test_corpus(),
    n_topics = c(3L, 2L),
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  ordered <- sbert_select_topics(
    select_test_corpus(),
    n_topics = 2:3,
    embeddings = select_test_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(shuffled, ordered)
})

testthat::test_that("umass measure is recorded", {
  result <- sbert_select_topics(
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
    sbert_select_topics(corpus, n_topics = 1:2, embeddings = embeddings)
  )
  testthat::expect_error(
    sbert_select_topics(corpus, n_topics = c(2L, 6L), embeddings = embeddings)
  )
  testthat::expect_error(
    sbert_select_topics(corpus, n_topics = c(2L, 2L), embeddings = embeddings)
  )
  testthat::expect_error(
    sbert_select_topics(corpus, n_topics = 2.5, embeddings = embeddings)
  )
})
