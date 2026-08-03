# Topic terms are a pure function of the fitted assignments and the document
# text, so retuning them never needs the clustering — let alone the encoding —
# to run again. This is the verb that used to be reachable only as
# sbert:::topic_term_scores().

#' Topic Terms, Retuned Without Refitting
#'
#' Recomputes the ranked terms of a fitted topic model under different term
#' settings. Term extraction depends only on the fitted topic assignments and
#' the document text, so every argument here can be changed without repeating
#' the clustering or the encoding.
#'
#' @param x A fitted model from [topics()].
#' @param n Terms returned per topic, or `NULL` for the whole vocabulary
#'   (useful together with `smoothing` to obtain a full \eqn{p(term | topic)}
#'   distribution). Defaults to the model's fitted `n_terms`.
#' @param stop_words Character vector excluded from terms. Defaults to the
#'   model's fitted list; see [stop_words()].
#' @param min_term_frequency Minimum corpus-wide token frequency.
#' @param min_token_length Minimum Unicode character length for a token.
#' @param weighting Class-based term weighting, `"ctfidf"` or `"bm25"`.
#' @param reduce_frequent_words Square-root the within-topic frequency before
#'   weighting, damping very frequent words.
#' @param stem Collapse inflected forms onto a shared Porter stem, displaying
#'   the most frequent surface form. Requires `SnowballC`.
#' @param smoothing Additive (Dirichlet) smoothing for the `beta` column. Only
#'   terms a topic actually used are returned, so with `smoothing = 0`
#'   (default) each topic's `beta` sums to one, while any positive value
#'   reserves mass for the zero-count vocabulary and the returned rows sum to
#'   less than one.
#' @param sort_by Order terms within each topic by `"score"` (default, the
#'   class-based weight — distinctive words) or `"beta"` (raw \eqn{p(term | topic)}
#'   — the words the topic uses most). `n` is applied after ordering.
#' @param ... Unused, present for generic consistency.
#' @return A base data frame with one row per topic and term: `topic`,
#'   `label`, `term`, `rank`, `score` (the class-based weight), `frequency`
#'   (within-topic count), and `beta`, the multinomial \eqn{p(term | topic)}.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
#'   "Stocks and bonds trade", "Markets price shares", "Banks report profit"
#' )
#' embeddings <- rbind(
#'   c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
#'   c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
#' )
#' fitted <- topics(
#'   text, n_topics = 2, embeddings = embeddings, min_term_frequency = 1
#' )
#' terms(fitted, n = 3)
#' terms(fitted, n = 3, weighting = "bm25")
terms.sbert_topic_model <- function(
  x,
  n = NULL,
  stop_words = NULL,
  min_term_frequency = NULL,
  min_token_length = NULL,
  weighting = NULL,
  reduce_frequent_words = NULL,
  stem = NULL,
  smoothing = 0,
  sort_by = c("score", "beta"),
  ...
) {
  sort_by <- match.arg(sort_by)
  stopifnot(inherits(x, "sbert_topic_model"))
  settings <- x$settings
  # Anything not restated keeps the value the model was fitted with.
  n <- if (is.null(n)) settings$n_terms else n
  stop_words <- if (is.null(stop_words)) settings$stop_words else stop_words
  min_term_frequency <- if (is.null(min_term_frequency)) {
    settings$min_term_frequency
  } else {
    min_term_frequency
  }
  min_token_length <- if (is.null(min_token_length)) {
    settings$min_token_length
  } else {
    min_token_length
  }
  weighting <- if (is.null(weighting)) settings$weighting else weighting
  reduce_frequent_words <- if (is.null(reduce_frequent_words)) {
    settings$reduce_frequent_words
  } else {
    reduce_frequent_words
  }
  stem <- if (is.null(stem)) settings$stem else stem
  stopifnot(
    is.numeric(smoothing),
    length(smoothing) == 1L,
    is.finite(smoothing),
    smoothing >= 0
  )

  n_topics <- settings$n_topics
  vocabulary_size <- length(unique(unlist(
    tokenize_topic_documents(
      x$documents$text,
      stop_words = stop_words,
      min_token_length = min_token_length,
      stem = stem
    ),
    use.names = FALSE
  )))
  # The whole vocabulary is scored first so that `n` can cut the list after
  # it has been ordered by the requested key. Taking the top n by score and
  # then re-sorting those by beta would answer a different question.
  scored <- topic_term_scores(
    text = x$documents$text,
    topic = x$documents$topic,
    n_topics = n_topics,
    n_terms = max(vocabulary_size, 1L),
    stop_words = stop_words,
    min_term_frequency = min_term_frequency,
    min_token_length = min_token_length,
    weighting = weighting,
    reduce_frequent_words = reduce_frequent_words,
    stem = stem
  )

  # p(term | topic) from the same counts the scores were built on, so `beta`
  # and `score` can never disagree about the vocabulary.
  counts <- scored$counts
  topic_totals <- rowSums(counts)
  beta_matrix <- (counts + smoothing) /
    (topic_totals + smoothing * ncol(counts))

  result <- insert_topic_label(scored$terms, x$topics$label)
  result$beta <- beta_matrix[
    cbind(result$topic, match(result$term, colnames(counts)))
  ]

  # Order within each topic by the requested key, renumber rank to match, then
  # keep the first n of each topic.
  key <- if (identical(sort_by, "beta")) result$beta else result$score
  result <- result[order(result$topic, -key, result$term), , drop = FALSE]
  result$rank <- unlist(
    lapply(rle(result$topic)$lengths, seq_len),
    use.names = FALSE
  )
  if (!is.null(n)) {
    result <- result[result$rank <= as.integer(n), , drop = FALSE]
  }
  rownames(result) <- NULL
  result
}
