terms_fixture <- function(...) {
  topics(
    c(
      "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
      "Stocks and bonds trade", "Markets price shares", "Banks report profit"
    ),
    n_topics = 2L,
    embeddings = rbind(
      c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
      c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
    ),
    min_term_frequency = 1L,
    ...
  )
}

testthat::test_that("terms() dispatches to the model, not stats::terms.default", {
  fitted <- terms_fixture()
  result <- terms(fitted, n = 2)
  # terms.default would return the stored $terms table, which has no beta.
  testthat::expect_true("beta" %in% names(result))
  testthat::expect_identical(
    names(result),
    c("topic", "label", "term", "rank", "score", "frequency", "beta")
  )
  testthat::expect_identical(nrow(result), 4L)
})

testthat::test_that("n limits the terms returned per topic", {
  fitted <- terms_fixture()
  for (requested in 1:3) {
    counts <- table(terms(fitted, n = requested)$topic)
    testthat::expect_true(all(counts <= requested))
  }
})

testthat::test_that("beta is a distribution over each topic's vocabulary", {
  fitted <- terms_fixture()
  full <- terms(fitted, n = NULL)
  totals <- tapply(full$beta, full$topic, sum)
  testthat::expect_equal(as.numeric(totals), c(1, 1), tolerance = 1e-12)
  testthat::expect_true(all(full$beta > 0))
})

testthat::test_that("settings default to the fitted model", {
  fitted <- terms_fixture(n_terms = 2L, weighting = "bm25")
  inherited <- terms(fitted)
  restated <- terms(fitted, n = 2L, weighting = "bm25")
  testthat::expect_equal(inherited, restated)
})

testthat::test_that("retuning changes terms without touching the model", {
  fitted <- terms_fixture()
  before <- fitted$terms
  bm25 <- terms(fitted, n = 3, weighting = "bm25")
  testthat::expect_identical(fitted$terms, before)
  testthat::expect_s3_class(bm25, "data.frame")
})

testthat::test_that("smoothing moves mass onto terms the topic never used", {
  fitted <- terms_fixture()
  unsmoothed <- terms(fitted, n = NULL)
  smoothed <- terms(fitted, n = NULL, smoothing = 0.1)
  # Only observed terms are returned, so once some mass is reserved for the
  # zero-count vocabulary the returned rows sum to less than one.
  totals <- as.numeric(tapply(smoothed$beta, smoothed$topic, sum))
  testthat::expect_true(all(totals < 1))
  testthat::expect_true(all(totals > 0.8))
  testthat::expect_equal(
    as.numeric(tapply(unsmoothed$beta, unsmoothed$topic, sum)),
    c(1, 1),
    tolerance = 1e-12
  )
})

testthat::test_that("label matches the fitted topic labels", {
  fitted <- terms_fixture()
  result <- terms(fitted, n = 2)
  testthat::expect_identical(result$label, fitted$topics$label[result$topic])
})


testthat::test_that("sort_by chooses between distinctive and most-used terms", {
  # "data" is frequent in BOTH topics: high beta, low class-based score.
  shared <- topics(
    c(
      "data data data cats mice", "data data cats purr", "data cats nap",
      "data data data stocks bonds", "data data stocks shares", "data stocks profit"
    ),
    n_topics = 2L,
    embeddings = rbind(
      c(1, 0), c(0.98, 0.02), c(0.96, 0.04),
      c(0, 1), c(0.02, 0.98), c(0.04, 0.96)
    ),
    min_term_frequency = 1L
  )
  by_score <- subset(terms(shared, n = 3, sort_by = "score"), topic == 1)
  by_beta <- subset(terms(shared, n = 3, sort_by = "beta"), topic == 1)
  testthat::expect_false(identical(by_score$term, by_beta$term))
  testthat::expect_identical(by_beta$term[1], "data")
  # beta ordering must be non-increasing within a topic
  testthat::expect_false(is.unsorted(rev(by_beta$beta)))
  # score ordering likewise
  testthat::expect_false(is.unsorted(rev(by_score$score)))
})

testthat::test_that("n is applied after ordering, not before", {
  shared <- topics(
    c(
      "data data data cats mice", "data data cats purr", "data cats nap",
      "data data data stocks bonds", "data data stocks shares", "data stocks profit"
    ),
    n_topics = 2L,
    embeddings = rbind(
      c(1, 0), c(0.98, 0.02), c(0.96, 0.04),
      c(0, 1), c(0.02, 0.98), c(0.04, 0.96)
    ),
    min_term_frequency = 1L
  )
  full <- subset(terms(shared, n = NULL, sort_by = "beta"), topic == 1)
  cut <- subset(terms(shared, n = 2, sort_by = "beta"), topic == 1)
  testthat::expect_identical(cut$term, head(full$term, 2L))
  testthat::expect_identical(cut$rank, 1:2)
})
