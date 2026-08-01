evaluation_test_corpus <- function() {
  c(
    animals_1 = "Cats chase mice and sleep.",
    animals_2 = "Dogs chase balls and sleep.",
    learning_1 = "Neural networks learn representations.",
    learning_2 = "Machine learning models learn patterns.",
    vehicles_1 = "Bicycles have wheels and pedals.",
    vehicles_2 = "Cars have wheels and engines."
  )
}

evaluation_test_embeddings <- function() {
  rbind(
    c(1, 0, 0),
    c(0.95, 0.05, 0),
    c(0, 1, 0),
    c(0.05, 0.95, 0),
    c(0, 0, 1),
    c(0.05, 0, 0.95)
  )
}

evaluation_test_model <- function(...) {
  sbert_topics(
    evaluation_test_corpus(),
    3L,
    embeddings = evaluation_test_embeddings(),
    n_terms = 4L,
    keep_embeddings = TRUE,
    ...
  )
}

testthat::test_that("UMass and NPMI match a hand calculation", {
  incidence <- rbind(
    c(1, 1, 0),
    c(1, 1, 0),
    c(1, 0, 1),
    c(0, 0, 1)
  )
  colnames(incidence) <- c("w1", "w2", "w3")
  document_frequency <- colSums(incidence)
  co_document_frequency <- crossprod(incidence)

  umass <- sbert:::score_topic_coherence(
    term_indices = c(1L, 2L, 3L),
    document_frequency = document_frequency,
    co_document_frequency = co_document_frequency,
    n_documents = 4L,
    measure = "umass",
    smoothing = 1
  )
  npmi <- sbert:::score_topic_coherence(
    term_indices = c(1L, 2L, 3L),
    document_frequency = document_frequency,
    co_document_frequency = co_document_frequency,
    n_documents = 4L,
    measure = "npmi",
    smoothing = 1
  )

  # UMass: mean(log(3/3), log(2/3), log(1/2)).
  expected_umass <- mean(c(log(1), log(2 / 3), log(0.5)))
  # NPMI: interior pairs use the standard formula; the zero co-occurrence pair
  # is defined as exactly -1.
  npmi_pair <- function(pi, pj, pij) {
    log(pij / (pi * pj)) / -log(pij)
  }
  expected_npmi <- mean(c(
    npmi_pair(0.75, 0.5, 0.5),
    npmi_pair(0.75, 0.5, 0.25),
    -1
  ))

  testthat::expect_equal(umass, expected_umass, tolerance = 1e-12)
  testthat::expect_equal(npmi, expected_npmi, tolerance = 1e-12)
})

testthat::test_that("coherence is tidy, deterministic, and topic-aligned", {
  model <- evaluation_test_model()
  coherence_one <- sbert_coherence(model, measure = "npmi", n_terms = 4L)
  coherence_two <- sbert_coherence(model, measure = "npmi", n_terms = 4L)

  testthat::expect_identical(coherence_one, coherence_two)
  testthat::expect_identical(
    names(coherence_one),
    c("topic", "label", "measure", "n_terms", "coherence")
  )
  testthat::expect_identical(coherence_one$topic, model$topics$topic)
  testthat::expect_identical(coherence_one$label, model$topics$label)
  testthat::expect_true(all(coherence_one$coherence >= -1 & coherence_one$coherence <= 1))
  testthat::expect_equal(
    attr(coherence_one, "mean_coherence"),
    mean(coherence_one$coherence)
  )
  testthat::expect_true(all(sbert_coherence(model, "umass")$measure == "umass"))
})

testthat::test_that("NPMI stays finite and bounded at the co-occurrence limits", {
  # Two terms that co-occur in every document must score exactly 1, not Inf.
  always <- rbind(c(1, 1), c(1, 1), c(1, 1))
  colnames(always) <- c("w1", "w2")
  score_always <- sbert:::score_topic_coherence(
    term_indices = c(1L, 2L),
    document_frequency = colSums(always),
    co_document_frequency = crossprod(always),
    n_documents = 3L,
    measure = "npmi",
    smoothing = 1
  )
  # Two terms that never co-occur must score exactly -1.
  never <- rbind(c(1, 0), c(1, 0), c(0, 1))
  colnames(never) <- c("w1", "w2")
  score_never <- sbert:::score_topic_coherence(
    term_indices = c(1L, 2L),
    document_frequency = colSums(never),
    co_document_frequency = crossprod(never),
    n_documents = 3L,
    measure = "npmi",
    smoothing = 1
  )

  testthat::expect_equal(score_always, 1)
  testthat::expect_equal(score_never, -1)
  testthat::expect_true(is.finite(score_always))
})

testthat::test_that("single-term topics yield NA coherence, not an error", {
  na_score <- sbert:::score_topic_coherence(
    term_indices = 1L,
    document_frequency = c(w1 = 2),
    co_document_frequency = matrix(2, 1, 1, dimnames = list("w1", "w1")),
    n_documents = 4L,
    measure = "npmi",
    smoothing = 1
  )
  testthat::expect_true(is.na(na_score))
})

testthat::test_that("diversity is the unique-term proportion", {
  model <- evaluation_test_model()
  pooled <- unlist(
    lapply(
      model$topics$topic,
      function(topic_id) {
        topic_terms <- model$terms[model$terms$topic == topic_id, , drop = FALSE]
        utils::head(topic_terms$term[order(topic_terms$rank)], 4L)
      }
    ),
    use.names = FALSE
  )
  expected <- length(unique(pooled)) / length(pooled)

  testthat::expect_equal(sbert_diversity(model, 4L), expected)
  testthat::expect_true(sbert_diversity(model, 4L) > 0 && sbert_diversity(model, 4L) <= 1)
})

testthat::test_that("summary prints a report and returns a tidy quality table", {
  model <- evaluation_test_model()
  quality <- NULL
  output <- utils::capture.output(quality <- summary(model, measure = "npmi", n_terms = 4L))

  testthat::expect_true(any(grepl("Semantic topic model summary", output)))
  testthat::expect_true(any(grepl("topic diversity", output)))
  testthat::expect_identical(
    names(quality),
    c("topic", "label", "n_documents", "proportion", "coherence")
  )
  testthat::expect_identical(quality$topic, model$topics$topic)
  testthat::expect_identical(nrow(quality), nrow(model$topics))
})

testthat::test_that("evaluation rejects malformed inputs", {
  model <- evaluation_test_model()
  testthat::expect_error(sbert_coherence(model, measure = "cosine"))
  testthat::expect_error(sbert_coherence(model, n_terms = 0))
  testthat::expect_error(sbert_coherence(model, smoothing = -1))
  testthat::expect_error(sbert_coherence(list(), "npmi"))
  testthat::expect_error(sbert_diversity(model, n_terms = 1.5))
})
