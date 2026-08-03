testthat::test_that("c-TF-IDF uses a real-valued average topic length", {
  # Topic lengths 3 and 2 give A = mean(c(3, 2)) = 2.5, a non-integer that the
  # earlier integer-truncated implementation would have rounded to 2.
  result <- sbert:::topic_term_scores(
    text = c("apple apple apple", "banana carrot"),
    topic = as.integer(c(1, 2)),
    n_topics = 2L,
    n_terms = 3L,
    stop_words = character(),
    min_term_frequency = 1L,
    min_token_length = 1L
  )

  testthat::expect_equal(result$average_topic_length, 2.5)
  # apple: tf = 3/3 = 1, f_x = 3, idf = log(1 + 2.5 / 3).
  testthat::expect_equal(
    unname(result$scores[1L, "apple"]),
    1 * log1p(2.5 / 3),
    tolerance = 1e-12
  )
})

testthat::test_that("BM25 weighting matches the published formula", {
  result <- sbert:::topic_term_scores(
    text = c("apple apple apple", "banana carrot"),
    topic = as.integer(c(1, 2)),
    n_topics = 2L,
    n_terms = 3L,
    stop_words = character(),
    min_term_frequency = 1L,
    min_token_length = 1L,
    weighting = "bm25"
  )

  # idf_bm25 = log(1 + (A - f_x + 0.5) / (f_x + 0.5)), A = 2.5, f_apple = 3.
  expected_idf <- log1p((2.5 - 3 + 0.5) / (3 + 0.5))
  testthat::expect_equal(
    unname(result$scores[1L, "apple"]),
    1 * expected_idf,
    tolerance = 1e-12
  )
})

testthat::test_that("reduce_frequent_words square-roots the term frequency", {
  base_result <- sbert:::topic_term_scores(
    text = c("apple apple banana", "carrot"),
    topic = as.integer(c(1, 2)),
    n_topics = 2L,
    n_terms = 3L,
    stop_words = character(),
    min_term_frequency = 1L,
    min_token_length = 1L
  )
  reduced_result <- sbert:::topic_term_scores(
    text = c("apple apple banana", "carrot"),
    topic = as.integer(c(1, 2)),
    n_topics = 2L,
    n_terms = 3L,
    stop_words = character(),
    min_term_frequency = 1L,
    min_token_length = 1L,
    reduce_frequent_words = TRUE
  )

  # tf(apple in topic 1) = 2/3; reduced score uses sqrt(2/3) in place of 2/3.
  testthat::expect_equal(
    reduced_result$scores[1L, "apple"],
    base_result$scores[1L, "apple"] * (sqrt(2 / 3) / (2 / 3)),
    tolerance = 1e-12
  )
})

testthat::test_that("topics threads weighting options into settings", {
  text <- c("cats mice", "dogs balls", "stocks bonds", "market shares")
  embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))

  default_model <- topics(text, 2L, embeddings = embeddings)
  bm25_model <- topics(
    text,
    2L,
    embeddings = embeddings,
    weighting = "bm25",
    reduce_frequent_words = TRUE
  )

  testthat::expect_identical(default_model$settings$weighting, "ctfidf")
  testthat::expect_false(default_model$settings$reduce_frequent_words)
  testthat::expect_identical(bm25_model$settings$weighting, "bm25")
  testthat::expect_true(bm25_model$settings$reduce_frequent_words)
  testthat::expect_error(
    topics(text, 2L, embeddings = embeddings, weighting = "tfidf")
  )
})
