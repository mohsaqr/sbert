testthat::test_that("sbert_dedupe counts and preserves first-appearance order", {
  result <- sbert_dedupe(c("b", "a", "b", NA, "  ", "b", "a", "c"))

  testthat::expect_identical(names(result), c("text", "n"))
  testthat::expect_identical(result$text, c("b", "a", "c"))
  testthat::expect_identical(result$n, c(3L, 2L, 1L))
  testthat::expect_error(sbert_dedupe(c(NA_character_, " ")), "non-blank")
  testthat::expect_error(sbert_dedupe(1:3))
})

testthat::test_that("sbert_topic_sizes reports both scales exactly", {
  fitted <- sbert_topics(
    c("apple apple banana", "banana apple fruit", "stocks trade daily", "markets trade stocks"),
    2L,
    embeddings = rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9)),
    stopwords = character()
  )

  plain <- sbert_topic_sizes(fitted)
  testthat::expect_identical(
    names(plain),
    c("topic", "label", "n_documents", "proportion")
  )
  testthat::expect_identical(plain$n_documents, fitted$topics$n_documents)

  weighted <- sbert_topic_sizes(fitted, weights = c(10, 1, 1, 1))
  testthat::expect_identical(
    names(weighted),
    c("topic", "label", "n_documents", "proportion", "n_weighted", "weighted_share")
  )
  fruit_topic <- fitted$documents$topic[[1L]]
  fruit_row <- subset(weighted, topic == fruit_topic)
  testthat::expect_identical(fruit_row$n_weighted, 11)
  testthat::expect_equal(fruit_row$weighted_share, 11 / 13, tolerance = 1e-12)
  testthat::expect_equal(sum(weighted$weighted_share), 1, tolerance = 1e-12)

  testthat::expect_error(sbert_topic_sizes(fitted, weights = c(1, 2)))
  testthat::expect_error(sbert_topic_sizes(fitted, weights = c(-1, 1, 1, 1)))
})

testthat::test_that("dedupe and sizes compose into the weighted workflow", {
  corpus <- sbert_dedupe(c("aa bb", "cc dd", "aa bb", "aa bb", "ee ff", "gg hh"))
  fitted <- sbert_topics(
    corpus$text,
    2L,
    embeddings = rbind(c(1, 0), c(0.95, 0.05), c(0, 1), c(0.05, 0.95)),
    stopwords = character()
  )
  sizes <- sbert_topic_sizes(fitted, weights = corpus$n)
  testthat::expect_identical(sum(sizes$n_weighted), 6)
  testthat::expect_identical(sum(sizes$n_documents), 4L)
})
