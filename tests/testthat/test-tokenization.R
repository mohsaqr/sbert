testthat::test_that("curly and straight apostrophes tokenize identically", {
  tokens <- sbert:::tokenize_topic_documents(
    c("It’s fine", "It's fine"),
    stop_words = character(),
    min_token_length = 2L
  )
  testthat::expect_identical(tokens[[1L]], tokens[[2L]])
  testthat::expect_identical(tokens[[1L]], c("it's", "fine"))
})

testthat::test_that("stemming collapses inflections to a shared display form", {
  testthat::skip_if_not_installed("SnowballC")
  tokens <- sbert:::tokenize_topic_documents(
    c("animals animal animals", "meaning means mean"),
    stop_words = character(),
    min_token_length = 2L,
    stem = TRUE
  )
  # "animals" is the most frequent surface for its stem, so it is the display
  # form; "mean" is most frequent for the mean/means/meaning stem.
  testthat::expect_identical(unique(tokens[[1L]]), "animals")
  testthat::expect_true(all(tokens[[2L]] == tokens[[2L]][[1L]]))
  testthat::expect_length(unique(unlist(tokens)), 2L)
})

testthat::test_that("stemming keeps topic terms distinct and deduplicated", {
  testthat::skip_if_not_installed("SnowballC")
  text <- c(
    "animals animal run", "animal animals move",
    "colour colours bright", "colours colour vivid"
  )
  embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
  model <- topics(
    text, 2L,
    embeddings = embeddings,
    n_terms = 3L,
    stem = TRUE
  )

  testthat::expect_true(model$settings$stem)
  testthat::expect_false(any(c("animal", "colour") %in%
    model$terms$term[model$terms$term %in% c("animals", "colours") == FALSE &
      duplicated(model$terms$term)]))
  # No inflection pair (animal/animals, colour/colours) survives together.
  terms <- model$terms$term
  testthat::expect_false(all(c("animal", "animals") %in% terms))
  testthat::expect_false(all(c("colour", "colours") %in% terms))
})
