hierarchy_test_corpus <- function() {
  c(
    "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
    "Stocks and bonds trade", "Markets price shares", "Banks report profit"
  )
}

hierarchy_test_embeddings <- function() {
  rbind(
    c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
    c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
  )
}

hierarchy_test_model <- function(keep_embeddings = TRUE) {
  topics(
    hierarchy_test_corpus(),
    n_topics = 3,
    embeddings = hierarchy_test_embeddings(),
    n_terms = 3,
    n_representatives = 2,
    min_term_frequency = 1L,
    keep_embeddings = keep_embeddings
  )
}

testthat::test_that("topic_hierarchy returns a labeled merge table", {
  topic_hierarchy <- topic_hierarchy(hierarchy_test_model())
  testthat::expect_s3_class(topic_hierarchy, "sbert_topic_hierarchy")
  testthat::expect_identical(
    names(topic_hierarchy$merges),
    c("step", "height", "left", "right")
  )
  testthat::expect_identical(nrow(topic_hierarchy$merges), 2L)
  testthat::expect_true(all(diff(topic_hierarchy$merges$height) >= 0))
  testthat::expect_s3_class(topic_hierarchy$tree, "hclust")
  testthat::expect_identical(length(topic_hierarchy$tree$labels), 3L)
  # The two animal-side topics must merge before either joins finance.
  testthat::expect_true(topic_hierarchy$merges$height[1] < topic_hierarchy$merges$height[2])
})

testthat::test_that("topic_hierarchy print and plot run", {
  topic_hierarchy <- topic_hierarchy(hierarchy_test_model())
  testthat::expect_output(print(topic_hierarchy), "sbert_topic_hierarchy")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::expect_invisible(plot(topic_hierarchy))
})

testthat::test_that("reduce_topics merges to a full working model", {
  reduced <- reduce_topics(hierarchy_test_model(), 2)
  testthat::expect_s3_class(reduced, "sbert_topic_model")
  testthat::expect_identical(nrow(reduced$topics), 2L)
  testthat::expect_identical(sum(reduced$topics$n_documents), 6L)
  # Largest topic first, package convention.
  testthat::expect_true(all(diff(reduced$topics$n_documents) <= 0))
  # Animal documents together, finance documents together.
  assignments <- reduced$documents$topic
  testthat::expect_identical(length(unique(head(assignments, 3))), 1L)
  testthat::expect_identical(length(unique(tail(assignments, 3))), 1L)
  testthat::expect_false(assignments[1] == assignments[4])
  # Downstream verbs keep working.
  testthat::expect_s3_class(
    coherence(reduced, measure = "npmi"),
    "data.frame"
  )
  testthat::expect_true(topic_diversity(reduced, n_terms = 3) > 0)
  prediction <- predict(
    reduced,
    hierarchy_test_corpus(),
    embeddings = hierarchy_test_embeddings()
  )
  testthat::expect_identical(prediction$topic, assignments)
})

testthat::test_that("reduced centers are unit-normalized member means", {
  reduced <- reduce_topics(hierarchy_test_model(), 2)
  norms <- sqrt(rowSums(reduced$centers^2))
  testthat::expect_equal(norms, rep(1, 2), tolerance = 1e-12)
  testthat::expect_true(all(reduced$documents$distance >= 0))
  testthat::expect_true(
    reduced$diagnostics$betweenss <= reduced$diagnostics$totss
  )
})

testthat::test_that("reduce_topics without stored embeddings needs them supplied", {
  slim <- hierarchy_test_model(keep_embeddings = FALSE)
  testthat::expect_error(reduce_topics(slim, 2), "keep_embeddings")
  recovered <- reduce_topics(
    slim,
    2,
    embeddings = hierarchy_test_embeddings()
  )
  testthat::expect_identical(nrow(recovered$topics), 2L)
})

testthat::test_that("reduction is deterministic", {
  first <- reduce_topics(hierarchy_test_model(), 2)
  second <- reduce_topics(hierarchy_test_model(), 2)
  testthat::expect_identical(first$documents, second$documents)
  testthat::expect_identical(first$topics, second$topics)
})

testthat::test_that("invalid reduction targets are rejected", {
  model <- hierarchy_test_model()
  testthat::expect_error(reduce_topics(model, 3), "below the current")
  testthat::expect_error(reduce_topics(model, 5), "below the current")
  testthat::expect_error(reduce_topics(model, 1))
  testthat::expect_error(topic_hierarchy("not a model"))
})
