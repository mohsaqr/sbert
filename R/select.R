# Topic-count selection: fit the deterministic topic model across candidate
# counts and report coherence, diversity, and cluster separation side by side,
# so the chosen granularity is justified by numbers rather than habit.

#' Compare Topic Counts Before Committing to One
#'
#' Fits [sbert_topics()] once per candidate topic count — deterministically,
#' on one shared set of embeddings — and returns the quality metrics that
#' justify a granularity choice: mean topic coherence, topic diversity, and
#' the share of embedding variance separated between topics.
#'
#' There is no single correct topic count; this verb replaces the habit of
#' picking one by feel with a table you can defend. Coherence typically falls
#' as counts grow while separation rises, so look for the count after which
#' coherence stops improving (or starts degrading) rather than a global
#' maximum.
#'
#' @param text A character vector of documents.
#' @param n_topics Integer vector of candidate topic counts, each at least 2
#'   and below the number of documents. Default `c(5, 10, 15, 20, 25, 30)`.
#' @param model A loaded sbert model, a pinned model name, or `NULL` for the
#'   session default. Ignored when `embeddings` is supplied.
#' @param embeddings Optional precomputed document embedding matrix (one row
#'   per document); supply it to avoid re-encoding.
#' @param measure Coherence measure, `"npmi"` (default) or `"umass"`.
#' @param n_terms Top terms per topic used for coherence and diversity.
#'   Default `10`.
#' @param batch_size Number of texts encoded per model call when encoding is
#'   needed. Default `32`.
#' @param ... Further arguments passed to [sbert_topics()] (for example
#'   `stopwords` or `weighting`).
#' @return A base data frame with one row per candidate and columns
#'   `n_topics`, `coherence` (corpus mean), `diversity`, and `explained`
#'   (between-topic share of total variance). The coherence measure is
#'   recorded in the `measure` attribute.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls", "Kittens nap in sunshine",
#'   "Stocks and bonds trade", "Markets price shares", "Banks report profit"
#' )
#' embeddings <- rbind(
#'   c(1, 0), c(0.95, 0.05), c(0.9, 0.1),
#'   c(0, 1), c(0.05, 0.95), c(0.1, 0.9)
#' )
#' sbert_select_topics(text, n_topics = 2:3, embeddings = embeddings)
sbert_select_topics <- function(
  text,
  n_topics = c(5L, 10L, 15L, 20L, 25L, 30L),
  model = NULL,
  embeddings = NULL,
  measure = c("npmi", "umass"),
  n_terms = 10L,
  batch_size = 32L,
  ...
) {
  measure <- match.arg(measure)
  stopifnot(
    is.character(text),
    length(text) >= 3L,
    !anyNA(text),
    is.numeric(n_topics),
    length(n_topics) >= 1L,
    all(is.finite(n_topics)),
    all(n_topics == as.integer(n_topics)),
    all(n_topics >= 2L),
    all(n_topics < length(text)),
    !anyDuplicated(n_topics)
  )
  if (is.null(embeddings)) {
    resolved <- resolve_sbert_model(model)
    embeddings <- encode_topic_documents(
      unname(text),
      model = resolved,
      batch_size = batch_size
    )
  }

  candidates <- sort(as.integer(n_topics))
  rows <- lapply(
    candidates,
    function(candidate) {
      fitted <- sbert_topics(
        text,
        n_topics = candidate,
        embeddings = embeddings,
        n_terms = n_terms,
        n_representatives = 1L,
        ...
      )
      coherence <- sbert_coherence(fitted, measure = measure, n_terms = n_terms)
      explained <- if (fitted$diagnostics$totss > 0) {
        fitted$diagnostics$betweenss / fitted$diagnostics$totss
      } else {
        0
      }
      data.frame(
        n_topics = candidate,
        coherence = mean(coherence$coherence),
        diversity = sbert_diversity(fitted, n_terms = n_terms),
        explained = explained,
        stringsAsFactors = FALSE
      )
    }
  )
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "measure") <- measure
  result
}
