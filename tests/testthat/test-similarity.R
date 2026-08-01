testthat::test_that("cosine similarity handles matrices and vectors", {
  embeddings <- rbind(
    first = c(1, 0),
    second = c(0, 1),
    diagonal = c(1, 1)
  )
  pairwise <- sbert_similarity(embeddings)
  cross <- sbert_similarity(c(1, 0), embeddings)

  testthat::expect_identical(dim(pairwise), c(3L, 3L))
  testthat::expect_equal(pairwise, t(pairwise), tolerance = 1e-12)
  testthat::expect_equal(unname(diag(pairwise)), rep(1, 3L), tolerance = 1e-12)
  testthat::expect_equal(
    unname(cross),
    matrix(c(1, 0, 1 / sqrt(2)), 1L),
    tolerance = 1e-12
  )
  testthat::expect_error(sbert_similarity(matrix(0, 1L, 2L)), "non-zero")
  testthat::expect_error(
    sbert_similarity(matrix(1, 1L, 2L), matrix(1, 1L, 3L)),
    "same number of columns"
  )
})
