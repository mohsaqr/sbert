#' Deduplicate a Text Corpus with Frequencies
#'
#' Collapses a character vector to its distinct non-blank texts, keeping the
#' order of first appearance and counting how often each text occurred.
#' Embedding and clustering operate on the distinct texts (so repeated
#' templates cannot drag the cluster geometry), while the returned counts
#' carry the original frequencies back into reporting, for example through
#' [sbert_topic_sizes()].
#'
#' @param text A character vector. `NA` and blank (whitespace-only) elements
#'   are dropped before deduplication.
#' @return A base data frame with one row per distinct text and columns
#'   `text` (in order of first appearance) and `n` (number of occurrences).
#' @export
#' @examples
#' sbert_dedupe(c("Very good!", "Try again.", "Very good!", NA, "  "))
sbert_dedupe <- function(text) {
  stopifnot(is.character(text), length(text) >= 1L)

  kept <- text[!is.na(text) & nzchar(trimws(text))]
  if (length(kept) == 0L) {
    stop("text contains no non-blank elements.", call. = FALSE)
  }
  counts <- table(factor(kept, levels = unique(kept)))
  data.frame(
    text = names(counts),
    n = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Topic Sizes on the Distinct and Weighted Scales
#'
#' Returns the size of every topic as fitted (distinct documents) and,
#' when `weights` are supplied, on the weighted scale — for example the
#' original row frequencies from [sbert_dedupe()]. The gap between
#' `proportion` and `weighted_share` measures how template-driven a topic
#' is: a topic whose weighted share far exceeds its distinct share is a
#' small repertoire of heavily repeated texts.
#'
#' @param object A fitted [sbert_topics()] model.
#' @param weights Optional numeric vector with one non-negative weight per
#'   fitted document (in document order), typically the `n` column of
#'   [sbert_dedupe()].
#' @return A base data frame with one row per topic and columns `topic`,
#'   `label`, `n_documents`, and `proportion`, plus `n_weighted` and
#'   `weighted_share` when `weights` are supplied.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' fitted <- sbert_topics(text, 2, embeddings = embeddings)
#' sbert_topic_sizes(fitted, weights = c(10, 1, 1, 1))
sbert_topic_sizes <- function(object, weights = NULL) {
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.null(weights) ||
      (
        is.numeric(weights) &&
          length(weights) == nrow(object$documents) &&
          !anyNA(weights) &&
          all(is.finite(weights)) &&
          all(weights >= 0)
      )
  )

  sizes <- data.frame(
    topic = object$topics$topic,
    label = object$topics$label,
    n_documents = object$topics$n_documents,
    proportion = object$topics$proportion,
    stringsAsFactors = FALSE
  )
  if (is.null(weights)) {
    return(sizes)
  }

  weighted_totals <- vapply(
    sizes$topic,
    function(topic_id) sum(weights[object$documents$topic == topic_id]),
    numeric(1)
  )
  sizes$n_weighted <- weighted_totals
  sizes$weighted_share <- weighted_totals / sum(weighted_totals)
  sizes
}
