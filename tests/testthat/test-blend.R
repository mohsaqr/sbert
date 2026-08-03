two_segment_frame <- function(document_ids = c(1L, 1L)) {
  data.frame(
    document_id = document_ids,
    document_name = "",
    segment = seq_along(document_ids),
    text = paste("segment", seq_along(document_ids)),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("orthogonal segment blends to the hand-computed midpoint", {
  # u = (1, 0) orthogonal to d = (0, 1): residual is u itself, so the blend is
  # normalize(0.5 u + 0.5 d) = (1, 1) / sqrt(2).
  result <- blend(
    two_segment_frame(c(1L, 1L)),
    alpha = 0.5,
    embeddings = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE),
    document_embeddings = matrix(c(0, 1), nrow = 1)
  )
  expected <- matrix(
    c(1 / sqrt(2), 1 / sqrt(2), 0, 1),
    nrow = 2,
    byrow = TRUE
  )
  testthat::expect_equal(result, expected, tolerance = 1e-12)
})

testthat::test_that("collinear segment falls back to the document direction", {
  # u = d: the residual vanishes; whatever alpha < 1, the blend is d itself,
  # and at alpha = 1 the degenerate guard also returns d.
  segments <- two_segment_frame(c(1L, 1L))
  unit_vectors <- matrix(c(0, 1, 0, 1), nrow = 2, byrow = TRUE)
  document_vectors <- matrix(c(0, 1), nrow = 1)
  for (alpha in c(0, 0.5, 1)) {
    result <- blend(
      segments,
      alpha = alpha,
      embeddings = unit_vectors,
      document_embeddings = document_vectors
    )
    testthat::expect_equal(result, unit_vectors, tolerance = 1e-12)
  }
})

testthat::test_that("alpha endpoints recover document and pure residual", {
  segments <- two_segment_frame(c(1L, 1L))
  unit_vectors <- matrix(c(1, 0, 0.6, 0.8), nrow = 2, byrow = TRUE)
  document_vectors <- matrix(c(0, 1), nrow = 1)

  at_zero <- blend(
    segments,
    alpha = 0,
    embeddings = unit_vectors,
    document_embeddings = document_vectors
  )
  testthat::expect_equal(
    at_zero,
    matrix(c(0, 1, 0, 1), nrow = 2, byrow = TRUE),
    tolerance = 1e-12
  )

  # alpha = 1 keeps only the context-orthogonal component: for u = (0.6, 0.8)
  # against d = (0, 1) the residual is (0.6, 0), normalized to (1, 0).
  at_one <- blend(
    segments,
    alpha = 1,
    embeddings = unit_vectors,
    document_embeddings = document_vectors
  )
  testthat::expect_equal(
    at_one,
    matrix(c(1, 0, 1, 0), nrow = 2, byrow = TRUE),
    tolerance = 1e-12
  )
})

testthat::test_that("document_id maps each segment to its own parent", {
  # Two documents with opposite directions: identical segment vectors must
  # blend differently depending on their parent.
  segments <- two_segment_frame(c(1L, 2L))
  unit_vectors <- matrix(c(1, 0, 1, 0), nrow = 2, byrow = TRUE)
  document_vectors <- matrix(c(0, 1, 0, -1), nrow = 2, byrow = TRUE)
  result <- blend(
    segments,
    alpha = 0.5,
    embeddings = unit_vectors,
    document_embeddings = document_vectors
  )
  expected <- matrix(
    c(1 / sqrt(2), 1 / sqrt(2), 1 / sqrt(2), -1 / sqrt(2)),
    nrow = 2,
    byrow = TRUE
  )
  testthat::expect_equal(result, expected, tolerance = 1e-12)
  testthat::expect_false(isTRUE(all.equal(result[1, ], result[2, ])))
})

testthat::test_that("unnormalized inputs are normalized before blending", {
  # Scaling u or d must not change the result: the blend works on directions.
  segments <- two_segment_frame(c(1L, 1L))
  scaled <- blend(
    segments,
    alpha = 0.5,
    embeddings = matrix(c(10, 0, 0, 3), nrow = 2, byrow = TRUE),
    document_embeddings = matrix(c(0, 7), nrow = 1)
  )
  plain <- blend(
    segments,
    alpha = 0.5,
    embeddings = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE),
    document_embeddings = matrix(c(0, 1), nrow = 1)
  )
  testthat::expect_equal(scaled, plain, tolerance = 1e-12)
})

testthat::test_that("every output row has unit norm", {
  set.seed(7)
  segments <- data.frame(
    document_id = rep(1:3, each = 4),
    document_name = "",
    segment = rep(1:4, times = 3),
    text = paste("segment", 1:12),
    stringsAsFactors = FALSE
  )
  result <- blend(
    segments,
    alpha = 0.3,
    embeddings = matrix(rnorm(12 * 5), nrow = 12),
    document_embeddings = matrix(rnorm(3 * 5), nrow = 3)
  )
  testthat::expect_equal(
    sqrt(rowSums(result^2)),
    rep(1, 12),
    tolerance = 1e-12
  )
})

testthat::test_that("invalid inputs are rejected", {
  segments <- two_segment_frame(c(1L, 1L))
  unit_vectors <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  document_vectors <- matrix(c(0, 1), nrow = 1)

  testthat::expect_error(
    blend(segments, alpha = 0.5, embeddings = unit_vectors),
    "both"
  )
  testthat::expect_error(
    blend(segments, alpha = 0.5, document_embeddings = document_vectors),
    "both"
  )
  testthat::expect_error(
    blend(
      segments,
      alpha = 1.5,
      embeddings = unit_vectors,
      document_embeddings = document_vectors
    )
  )
  testthat::expect_error(
    blend(
      segments,
      alpha = 0.5,
      embeddings = unit_vectors[1, , drop = FALSE],
      document_embeddings = document_vectors
    )
  )
  testthat::expect_error(
    blend(
      two_segment_frame(c(1L, 5L)),
      alpha = 0.5,
      embeddings = unit_vectors,
      document_embeddings = document_vectors
    )
  )
  testthat::expect_error(
    blend(data.frame(text = "x"), alpha = 0.5)
  )
  testthat::expect_error(blend(segments))
})
