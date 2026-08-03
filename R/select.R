# Topic-count selection: fit the deterministic topic model across candidate
# counts and report coherence, topic_diversity, and cluster separation side by side,
# so the chosen granularity is justified by numbers rather than habit.

#' Compare Topic Counts Before Committing to One
#'
#' Fits [topics()] once per candidate topic count — deterministically,
#' on one shared set of embeddings — and returns the quality metrics that
#' justify a granularity choice: mean topic coherence, topic topic_diversity, and
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
#' @param n_terms Top terms per topic used for coherence and topic_diversity.
#'   Default `10`.
#' @param n_representatives Representative documents kept per topic in each
#'   retained model. Default `1`; raise it when the stored models are meant
#'   to be inspected rather than only compared.
#' @param keep_models Whether to retain every fitted model so the chosen
#'   granularity needs no refitting. Default `TRUE`; set `FALSE` to return
#'   only the comparison table when memory matters.
#' @param batch_size Number of texts encoded per model call when encoding is
#'   needed. Default `32`.
#' @param ... Further arguments passed to [topics()] (for example
#'   `stop_words` or `weighting`).
#' @return A base data frame with one row per candidate and columns
#'   `n_topics`, `coherence` (corpus mean), `topic_diversity`, and `explained`
#'   (between-topic share of total variance). The coherence measure is
#'   recorded in the `measure` attribute. When `keep_models = TRUE` the
#'   result also carries class `sbert_topic_sweep` and the fitted models,
#'   which [fitted()] extracts and [plot.sbert_topic_sweep()] draws.
#' @seealso [fitted()] to pull one fitted model out of the sweep.
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
#' sweep <- select_topics(text, n_topics = 2:3, embeddings = embeddings)
#' sweep
#' fitted(sweep, n_topics = 2)
select_topics <- function(
  text,
  n_topics = c(5L, 10L, 15L, 20L, 25L, 30L),
  model = NULL,
  embeddings = NULL,
  measure = c("npmi", "umass"),
  n_terms = 10L,
  n_representatives = 1L,
  keep_models = TRUE,
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
  models <- lapply(
    candidates,
    function(candidate) {
      topics(
        text,
        n_topics = candidate,
        embeddings = embeddings,
        n_terms = n_terms,
        n_representatives = n_representatives,
        ...
      )
    }
  )
  names(models) <- as.character(candidates)

  rows <- Map(
    function(candidate, fitted) {
      coherence <- coherence(fitted, measure = measure, n_terms = n_terms)
      explained <- if (fitted$diagnostics$totss > 0) {
        fitted$diagnostics$betweenss / fitted$diagnostics$totss
      } else {
        0
      }
      data.frame(
        n_topics = candidate,
        coherence = mean(coherence$coherence),
        topic_diversity = topic_diversity(fitted, n_terms = n_terms),
        explained = explained,
        stringsAsFactors = FALSE
      )
    },
    candidates,
    models
  )
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "measure") <- measure
  # The class is always applied so fitted() keeps dispatching here and can
  # explain itself; keep_models only controls whether the models are carried.
  if (keep_models) {
    attr(result, "models") <- models
  }
  class(result) <- c("sbert_topic_sweep", class(result))
  result
}

#' Extract One Fitted Model from a Topic-Count Sweep
#'
#' Returns the model [select_topics()] already fitted for a given topic
#' count, so choosing a granularity from the comparison table costs no
#' refitting.
#'
#' @param object A sweep returned by [select_topics()] with
#'   `keep_models = TRUE` (the default).
#' @param ... Unused, present for generic consistency.
#' @param n_topics The topic count to extract. Must be one of the candidates
#'   in the sweep.
#' @return The `sbert_topic_model` fitted for that candidate.
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
#' sweep <- select_topics(
#'   text, n_topics = 2:3, embeddings = embeddings, n_terms = 3
#' )
#' fitted(sweep, n_topics = 2)
fitted.sbert_topic_sweep <- function(object, n_topics, ...) {
  sweep <- object
  models <- attr(sweep, "models")
  if (is.null(models)) {
    stop(
      "fitted: this sweep kept no models. Refit with ",
      "select_topics(..., keep_models = TRUE).",
      call. = FALSE
    )
  }
  stopifnot(
    is.numeric(n_topics),
    length(n_topics) == 1L,
    is.finite(n_topics),
    n_topics == as.integer(n_topics)
  )
  key <- as.character(as.integer(n_topics))
  if (!key %in% names(models)) {
    stop(
      sprintf(
        "fitted: no model for n_topics = %s. Available: %s.",
        key,
        paste(names(models), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  models[[key]]
}

#' @export
print.sbert_topic_sweep <- function(x, ...) {
  measure <- attr(x, "measure")
  models <- attr(x, "models")
  cat(sprintf(
    "<sbert_topic_sweep> %d candidates, coherence measure: %s\n",
    nrow(x),
    if (is.null(measure)) "unknown" else measure
  ))
  print(as.data.frame(x), row.names = FALSE)
  if (!is.null(models)) {
    cat(sprintf(
      "\nFitted models retained: fitted(x, n_topics = %s)\n",
      names(models)[which.max(x$coherence)]
    ))
  }
  invisible(x)
}

#' @export
as.data.frame.sbert_topic_sweep <- function(x, ...) {
  plain <- x
  attr(plain, "models") <- NULL
  class(plain) <- "data.frame"
  plain
}

#' Plot a Topic-Count Sweep
#'
#' Draws coherence, topic_diversity, and between-topic variance against the
#' candidate topic counts, marking the count with the highest coherence.
#' There is no single correct topic count; the useful signal is the count
#' after which coherence stops improving, not a global maximum.
#'
#' @param x A sweep returned by [select_topics()].
#' @param main Overall title.
#' @param ... Passed to the underlying plotting calls.
#' @return `x`, invisibly.
#' @export
plot.sbert_topic_sweep <- function(x, main = "Topic-count comparison", ...) {
  panels <- list(
    list(column = "coherence", ylab = sprintf(
      "Coherence (%s)",
      if (is.null(attr(x, "measure"))) "npmi" else attr(x, "measure")
    )),
    list(column = "topic_diversity", ylab = "Topic topic_diversity"),
    list(column = "explained", ylab = "Variance between topics")
  )
  colors <- topic_palette(3)
  previous <- graphics::par(
    mfrow = c(1, 3),
    mar = c(4.2, 4.4, 3.2, 1.0),
    oma = c(0, 0, 2.4, 0),
    mgp = c(2.5, 0.7, 0)
  )
  on.exit(graphics::par(previous), add = TRUE)

  best <- x$n_topics[which.max(x$coherence)]
  for (index in seq_along(panels)) {
    values <- x[[panels[[index]]$column]]
    plot(
      x$n_topics,
      values,
      type = "b",
      pch = 19,
      lwd = 2,
      col = colors[index],
      xlab = "Number of topics",
      ylab = panels[[index]]$ylab,
      main = panels[[index]]$column,
      ...
    )
    graphics::abline(v = best, lty = 3, col = "grey40")
    graphics::grid(col = "grey90", lty = 1)
  }
  graphics::mtext(
    sprintf("%s  (highest coherence at %d topics)", main, best),
    outer = TRUE,
    cex = 0.95,
    font = 2
  )
  invisible(x)
}
