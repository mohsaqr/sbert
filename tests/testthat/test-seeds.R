seed_test_corpus <- function() {
  c(
    "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
    "Dogs chase balls",
    "Stocks and bonds trade", "Markets price shares"
  )
}

# Four animal documents, two finance documents: without seeds, size
# reordering would make the animal cluster topic 1.
seed_test_embeddings <- function() {
  rbind(
    c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04), c(0.94, 0.03, 0.03),
    c(0, 1, 0), c(0.02, 0.98, 0)
  )
}

finance_first_seeds <- function() {
  c(finance = "stocks markets trading", animals = "cats and dogs")
}

finance_first_seed_embeddings <- function() {
  rbind(c(0, 1, 0), c(1, 0, 0))
}

testthat::test_that("seeded topics keep seed order and take seed names", {
  model <- sbert_topics(
    seed_test_corpus(),
    n_topics = 2,
    embeddings = seed_test_embeddings(),
    seeds = finance_first_seeds(),
    seed_embeddings = finance_first_seed_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  # Topic 1 is the finance seed although the animal cluster is larger.
  testthat::expect_identical(model$topics$label, c("finance", "animals"))
  testthat::expect_identical(model$topics$n_documents, c(2L, 4L))
  testthat::expect_identical(model$documents$topic, c(2L, 2L, 2L, 2L, 1L, 1L))
  testthat::expect_identical(model$settings$seeds, unname(finance_first_seeds()))
  testthat::expect_false(model$settings$fixed_seeds)
})

testthat::test_that("free topics join the seeded ones", {
  model <- sbert_topics(
    seed_test_corpus(),
    n_topics = 2,
    embeddings = seed_test_embeddings(),
    seeds = c(finance = "stocks markets trading"),
    seed_embeddings = rbind(c(0, 1, 0)),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(model$topics$label[1], "finance")
  testthat::expect_identical(model$topics$n_documents, c(2L, 4L))
  testthat::expect_identical(model$documents$topic, c(2L, 2L, 2L, 2L, 1L, 1L))
})

testthat::test_that("fixed seeds freeze the centroids", {
  seeds <- finance_first_seeds()
  model <- sbert_topics(
    seed_test_corpus(),
    n_topics = 2,
    embeddings = seed_test_embeddings(),
    seeds = seeds,
    seed_embeddings = finance_first_seed_embeddings(),
    fixed_seeds = TRUE,
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_equal(
    model$centers,
    finance_first_seed_embeddings(),
    tolerance = 1e-12
  )
  testthat::expect_match(model$diagnostics$algorithm, "fixed seed")
  testthat::expect_true(model$settings$fixed_seeds)
})

testthat::test_that("fixed seeds allow empty topics", {
  model <- sbert_topics(
    seed_test_corpus(),
    n_topics = 3,
    embeddings = seed_test_embeddings(),
    seeds = c(
      finance = "stocks markets",
      animals = "cats dogs",
      chemistry = "acids and bases"
    ),
    seed_embeddings = rbind(c(0, 1, 0), c(1, 0, 0), c(0, 0, 1)),
    fixed_seeds = TRUE,
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(model$topics$n_documents, c(2L, 4L, 0L))
  testthat::expect_identical(model$topics$label[3], "chemistry")
})

testthat::test_that("seed lists collapse to phrases and runs are deterministic", {
  as_list <- sbert_topics(
    seed_test_corpus(),
    n_topics = 2,
    embeddings = seed_test_embeddings(),
    seeds = list(finance = c("stocks", "markets", "trading"), animals = "cats"),
    seed_embeddings = finance_first_seed_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(
    as_list$settings$seeds,
    c("stocks markets trading", "cats")
  )
  again <- sbert_topics(
    seed_test_corpus(),
    n_topics = 2,
    embeddings = seed_test_embeddings(),
    seeds = list(finance = c("stocks", "markets", "trading"), animals = "cats"),
    seed_embeddings = finance_first_seed_embeddings(),
    n_terms = 3,
    min_term_frequency = 1L
  )
  testthat::expect_identical(as_list$documents, again$documents)
})

testthat::test_that("invalid seed configurations are rejected", {
  corpus <- seed_test_corpus()
  embeddings <- seed_test_embeddings()
  testthat::expect_error(
    sbert_topics(
      corpus, 2,
      embeddings = embeddings,
      seeds = c("a", "b", "c"),
      seed_embeddings = diag(3)
    ),
    "More seeds than topics"
  )
  testthat::expect_error(
    sbert_topics(
      corpus, 3,
      embeddings = embeddings,
      seeds = c("a", "b"),
      seed_embeddings = finance_first_seed_embeddings(),
      fixed_seeds = TRUE
    ),
    "must equal"
  )
  testthat::expect_error(
    sbert_topics(
      corpus, 2,
      embeddings = embeddings,
      seeds = c("finance seed")
    ),
    "seed_embeddings"
  )
  testthat::expect_error(
    sbert_topics(
      corpus, 2,
      embeddings = embeddings,
      seeds = c(""),
      seed_embeddings = rbind(c(0, 1, 0))
    )
  )
  testthat::expect_error(
    sbert_topics(
      corpus, 2,
      embeddings = embeddings,
      seeds = c("finance"),
      seed_embeddings = rbind(c(0, 1))
    )
  )
})
