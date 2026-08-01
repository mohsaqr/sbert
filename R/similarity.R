normalize_embedding_rows <- function(embeddings) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    !anyNA(embeddings),
    all(is.finite(embeddings))
  )

  norms <- sqrt(rowSums(embeddings^2))
  if (any(norms <= 0)) {
    stop("Embedding rows must have non-zero norms.", call. = FALSE)
  }
  embeddings / norms
}

#' Compute Cosine Similarity
#'
#' @param x Numeric embedding matrix, or one numeric vector.
#' @param y Optional second embedding matrix or vector. When omitted, computes
#'   all pairwise similarities among rows of `x`.
#' @return A numeric similarity matrix.
#' @export
#' @examples
#' x <- rbind(c(1, 0), c(0, 1), c(1, 1))
#' sbert_similarity(x)
sbert_similarity <- function(x, y = NULL) {
  stopifnot(is.numeric(x), is.null(y) || is.numeric(y))

  if (is.null(dim(x))) {
    x <- matrix(x, nrow = 1L)
  }
  if (!is.null(y) && is.null(dim(y))) {
    y <- matrix(y, nrow = 1L)
  }
  stopifnot(is.matrix(x), is.null(y) || is.matrix(y))

  if (is.null(y)) {
    y <- x
  }
  if (ncol(x) != ncol(y)) {
    stop("x and y must have the same number of columns.", call. = FALSE)
  }

  normalize_embedding_rows(x) %*% t(normalize_embedding_rows(y))
}

