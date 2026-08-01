# Context-blended segment embeddings: each unit keeps what it says beyond its
# parent document (the context-orthogonal residual) and inherits the rest from
# the document vector. Blending the residual rather than the raw unit vector
# avoids near-collinearity: a segment and its parent are already highly
# aligned, so a plain convex combination mostly re-adds shared direction and
# lets the document term wash out unit-level nuance.

#' Blend Segment Embeddings with Their Document Context
#'
#' Computes context-aware embeddings for text segments: each segment vector is
#' decomposed against its parent document embedding, and the blend keeps
#' `alpha` of the segment's context-orthogonal residual plus `1 - alpha` of
#' the document direction, renormalized to unit length:
#' \deqn{m = normalize(\alpha (u - (u \cdot d) d) + (1 - \alpha) d)}
#' with `u` the unit-normalized segment embedding and `d` the unit-normalized
#' parent document embedding.
#'
#' This is the principled version of "mixing two embeddings": granularities
#' are mixed per unit, never by pooling units of different levels into one
#' training set (which lets clusters form along a length axis — see
#' length-induced embedding collapse, arXiv:2410.24200). A segment that is
#' ambiguous in isolation inherits its document's context; identical segment
#' texts from different documents get different blended vectors, which keeps
#' repeated boilerplate from collapsing into a single artificial topic.
#'
#' `alpha = 0` returns the parent document embedding; `alpha = 1` keeps only
#' what the segment says beyond its document; the default `0.5` balances the
#' two. A segment exactly collinear with its document falls back to the
#' document vector.
#'
#' @param segments A data frame from [sbert_segment()] (or any data frame
#'   with integer `document_id` and character `text` columns), one row per
#'   segment.
#' @param documents The character vector of documents that was segmented;
#'   `segments$document_id` indexes into it. Required unless both embedding
#'   matrices are supplied.
#' @param model A loaded sbert model, a pinned model name, or `NULL` for the
#'   session default (see [sbert_load_model()]). Ignored when embeddings are
#'   supplied.
#' @param alpha Blend weight in `[0, 1]` for the segment's
#'   context-orthogonal residual. Default `0.5`.
#' @param embeddings Optional precomputed segment embeddings (one row per
#'   row of `segments`). Supply together with `document_embeddings` to skip
#'   encoding.
#' @param document_embeddings Optional precomputed document embeddings (one
#'   row per document; `segments$document_id` indexes its rows).
#' @param batch_size Number of texts encoded per model call when encoding is
#'   needed. Default `32`.
#' @return A numeric matrix with one L2-normalized row per segment, suitable
#'   for the `embeddings` argument of [sbert_topics()],
#'   [sbert_representatives()], and [predict.sbert_topic_model()].
#' @export
#' @examples
#' segments <- sbert_segment("The model works. It is fast.", level = "sentence")
#' unit_vectors <- matrix(c(1, 0, 0.6, 0.8), nrow = 2, byrow = TRUE)
#' document_vectors <- matrix(c(0, 1), nrow = 1)
#' sbert_blend(
#'   segments,
#'   alpha = 0.5,
#'   embeddings = unit_vectors,
#'   document_embeddings = document_vectors
#' )
sbert_blend <- function(
  segments,
  documents = NULL,
  model = NULL,
  alpha = 0.5,
  embeddings = NULL,
  document_embeddings = NULL,
  batch_size = 32L
) {
  stopifnot(
    is.data.frame(segments),
    all(c("document_id", "text") %in% names(segments)),
    nrow(segments) >= 1L,
    is.numeric(segments$document_id),
    !anyNA(segments$document_id),
    is.numeric(alpha),
    length(alpha) == 1L,
    is.finite(alpha),
    alpha >= 0,
    alpha <= 1
  )
  if (is.null(embeddings) != is.null(document_embeddings)) {
    stop(
      "sbert_blend: supply both 'embeddings' and 'document_embeddings', ",
      "or neither."
    )
  }
  if (is.null(embeddings)) {
    if (is.null(documents)) {
      stop(
        "sbert_blend: supply 'documents' (with an optional 'model') or ",
        "both 'embeddings' and 'document_embeddings'."
      )
    }
    stopifnot(is.character(documents), !anyNA(documents))
    model <- resolve_sbert_model(model)
    embeddings <- sbert_encode(
      segments$text,
      model = model,
      batch_size = batch_size
    )
    document_embeddings <- sbert_encode(
      documents,
      model = model,
      batch_size = batch_size
    )
  }
  stopifnot(
    is.matrix(embeddings),
    is.matrix(document_embeddings),
    is.numeric(embeddings),
    is.numeric(document_embeddings),
    nrow(embeddings) == nrow(segments),
    ncol(embeddings) == ncol(document_embeddings),
    max(segments$document_id) <= nrow(document_embeddings),
    min(segments$document_id) >= 1
  )

  unit_matrix <- normalize_rows(embeddings)
  parent_matrix <- normalize_rows(
    document_embeddings[segments$document_id, , drop = FALSE]
  )
  # Elementwise vector-matrix products recycle column-major, i.e. per row.
  alignment <- rowSums(unit_matrix * parent_matrix)
  residual <- unit_matrix - alignment * parent_matrix
  blended <- alpha * residual + (1 - alpha) * parent_matrix
  blended_norms <- sqrt(rowSums(blended^2))
  degenerate <- blended_norms < 1e-8
  blended[degenerate, ] <- parent_matrix[degenerate, , drop = FALSE]
  blended_norms[degenerate] <- 1
  blended / blended_norms
}

normalize_rows <- function(m) {
  norms <- sqrt(rowSums(m^2))
  norms[norms < 1e-12] <- 1
  m / norms
}
