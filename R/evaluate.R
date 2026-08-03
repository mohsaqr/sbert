# Intrinsic evaluation of embedding-based topic models: term coherence and
# term topic_diversity. Both are computed on the input corpus itself (no external
# reference corpus is required), matching the standard protocol used to assess
# Sentence-BERT topic models (Mimno et al. 2011; Reimers and Gurevych 2019;
# Mendonca and Figueira 2025).

# Build a document-by-term presence matrix for a fixed set of reference terms.
# Rows are documents, columns are terms, entries are TRUE when the (unique)
# tokenization of a document contains the term. Document frequencies and
# co-document frequencies are then simple matrix reductions.
topic_document_incidence <- function(
  text,
  terms,
  min_token_length,
  stem = FALSE,
  stop_words = character()
) {
  stopifnot(
    is.character(text),
    !anyNA(text),
    is.character(terms),
    !anyNA(terms),
    length(terms) >= 1L,
    is.numeric(min_token_length),
    length(min_token_length) == 1L,
    min_token_length >= 1,
    is.character(stop_words),
    !anyNA(stop_words)
  )

  reference_terms <- unique(terms)
  # Reuse the fit-time stop_words so that, under stemming, each stem's chosen
  # display surface is identical to the one in the topic term table.
  token_lists <- tokenize_topic_documents(
    text,
    stop_words = stop_words,
    min_token_length = as.integer(min_token_length),
    stem = stem
  )
  presence <- vapply(
    token_lists,
    function(document_tokens) {
      reference_terms %in% document_tokens
    },
    logical(length(reference_terms))
  )
  presence <- matrix(
    presence,
    nrow = length(reference_terms),
    ncol = length(text),
    dimnames = list(reference_terms, NULL)
  )
  incidence <- t(presence)
  storage.mode(incidence) <- "double"
  incidence
}

# Coherence of a single ordered set of top terms (most probable first).
score_topic_coherence <- function(
  term_indices,
  document_frequency,
  co_document_frequency,
  n_documents,
  measure,
  smoothing
) {
  n_words <- length(term_indices)
  if (n_words < 2L) {
    return(NA_real_)
  }

  pairs <- utils::combn(n_words, 2L)
  earlier <- term_indices[pairs[1L, ]]
  later <- term_indices[pairs[2L, ]]
  co_counts <- co_document_frequency[cbind(earlier, later)]

  pair_scores <- if (measure == "umass") {
    # UMass coherence (Mimno et al. 2011): the more-probable word of each pair
    # supplies the denominator document frequency.
    log((co_counts + smoothing) / document_frequency[earlier])
  } else {
    # Normalized pointwise mutual information, bounded in [-1, 1]. The two
    # limiting cases are handled exactly: terms that never co-occur score -1,
    # terms that always co-occur score 1.
    probability_earlier <- document_frequency[earlier] / n_documents
    probability_later <- document_frequency[later] / n_documents
    joint_probability <- co_counts / n_documents
    interior <- joint_probability > 0 & joint_probability < 1
    npmi <- ifelse(joint_probability <= 0, -1, 1)
    npmi[interior] <- log(
      joint_probability[interior] /
        (probability_earlier[interior] * probability_later[interior])
    ) / -log(joint_probability[interior])
    npmi
  }
  mean(pair_scores)
}

#' Score Topic Coherence
#'
#' Computes intrinsic topic coherence from the top terms of a fitted topic
#' model and the co-occurrence structure of the input documents. Coherence
#' rewards topics whose top terms tend to appear together in the same
#' documents, and is the standard quantitative measure for embedding-based
#' topic models (Mimno et al. 2011).
#'
#' @param object An `sbert_topic_model` returned by [topics()].
#' @param measure Either `"umass"` (Mimno et al. 2011; larger, i.e. closer to
#'   zero, is more coherent) or `"npmi"` (normalized pointwise mutual
#'   information, bounded in `[-1, 1]`; larger is more coherent).
#' @param n_terms Number of top terms per topic to score. Capped at the number
#'   of terms actually available for each topic.
#' @param smoothing Additive smoothing for the UMass co-occurrence count.
#' @return A data frame with one row per topic and columns `topic`, `label`,
#'   `measure`, `n_terms`, and `coherence`. The corpus-level mean is attached as
#'   the `"mean_coherence"` attribute. Topics with fewer than two scorable terms
#'   yield `NA`.
#' @references Mimno, D., Wallach, H. M., Talley, E., Leenders, M., and
#'   McCallum, A. (2011). Optimizing semantic coherence in topic models. EMNLP.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' topics <- topics(text, 2, embeddings = embeddings, n_terms = 3)
#' coherence(topics)
coherence <- function(
  object,
  measure = c("npmi", "umass"),
  n_terms = 10L,
  smoothing = 1
) {
  measure <- match.arg(measure)
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.numeric(n_terms),
    length(n_terms) == 1L,
    is.finite(n_terms),
    n_terms >= 1,
    n_terms == as.integer(n_terms),
    is.numeric(smoothing),
    length(smoothing) == 1L,
    is.finite(smoothing),
    smoothing > 0
  )

  topic_ids <- object$topics$topic
  ranked_terms <- object$terms
  reference_terms <- unique(ranked_terms$term)
  incidence <- topic_document_incidence(
    object$documents$text,
    reference_terms,
    object$settings$min_token_length,
    stem = isTRUE(object$settings$stem),
    stop_words = if (is.null(object$settings$stop_words)) {
      character()
    } else {
      object$settings$stop_words
    }
  )
  document_frequency <- colSums(incidence)
  co_document_frequency <- crossprod(incidence)
  n_documents <- nrow(incidence)
  term_position <- seq_len(ncol(incidence))
  names(term_position) <- colnames(incidence)

  scores <- vapply(
    topic_ids,
    function(topic_id) {
      topic_terms <- ranked_terms[ranked_terms$topic == topic_id, , drop = FALSE]
      topic_terms <- topic_terms[order(topic_terms$rank), , drop = FALSE]
      selected <- utils::head(topic_terms$term, as.integer(n_terms))
      term_indices <- term_position[selected]
      score_topic_coherence(
        term_indices = unname(term_indices),
        document_frequency = document_frequency,
        co_document_frequency = co_document_frequency,
        n_documents = n_documents,
        measure = measure,
        smoothing = smoothing
      )
    },
    numeric(1)
  )
  terms_used <- vapply(
    topic_ids,
    function(topic_id) {
      min(
        as.integer(n_terms),
        sum(ranked_terms$topic == topic_id)
      )
    },
    integer(1)
  )

  result <- data.frame(
    topic = topic_ids,
    label = object$topics$label,
    measure = measure,
    n_terms = terms_used,
    coherence = scores,
    stringsAsFactors = FALSE
  )
  attr(result, "mean_coherence") <- mean(scores, na.rm = TRUE)
  result
}

#' Measure Topic Diversity
#'
#' Reports the proportion of distinct terms among the pooled top terms of every
#' topic. A value near one indicates topics described by largely different
#' vocabulary; a low value indicates redundant topics that repeat the same
#' words (Dieng et al. 2020).
#'
#' @param object An `sbert_topic_model` returned by [topics()].
#' @param n_terms Number of top terms per topic to pool. Capped at the number
#'   of terms available for each topic.
#' @return A single proportion in `(0, 1]`.
#' @references Dieng, A. B., Ruiz, F. J. R., and Blei, D. M. (2020). Topic
#'   modeling in embedding spaces. TACL.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' topics <- topics(text, 2, embeddings = embeddings, n_terms = 3)
#' topic_diversity(topics)
topic_diversity <- function(object, n_terms = 10L) {
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.numeric(n_terms),
    length(n_terms) == 1L,
    is.finite(n_terms),
    n_terms >= 1,
    n_terms == as.integer(n_terms)
  )

  ranked_terms <- object$terms
  pooled_terms <- unlist(
    lapply(
      object$topics$topic,
      function(topic_id) {
        topic_terms <- ranked_terms[ranked_terms$topic == topic_id, , drop = FALSE]
        topic_terms <- topic_terms[order(topic_terms$rank), , drop = FALSE]
        utils::head(topic_terms$term, as.integer(n_terms))
      }
    ),
    use.names = FALSE
  )
  if (length(pooled_terms) == 0L) {
    stop("The topic model has no terms to evaluate.", call. = FALSE)
  }
  length(unique(pooled_terms)) / length(pooled_terms)
}

#' Summarize a Semantic Topic Model
#'
#' Prints a compact scientific report -- corpus size, cluster separation,
#' coherence, and topic_diversity -- and returns a tidy per-topic quality table.
#'
#' @param object An `sbert_topic_model` returned by [topics()].
#' @param measure Coherence measure passed to [coherence()].
#' @param n_terms Number of top terms per topic used for coherence and
#'   topic_diversity.
#' @param ... Unused; present for S3 compatibility.
#' @return Invisibly, a data frame with one row per topic containing `topic`,
#'   `label`, `n_documents`, `proportion`, and `coherence`.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' topics <- topics(text, 2, embeddings = embeddings, n_terms = 3)
#' summary(topics)
summary.sbert_topic_model <- function(
  object,
  measure = c("npmi", "umass"),
  n_terms = 10L,
  ...
) {
  measure <- match.arg(measure)
  coherence <- coherence(object, measure = measure, n_terms = n_terms)
  topic_diversity <- topic_diversity(object, n_terms = n_terms)
  explained <- if (object$diagnostics$totss > 0) {
    object$diagnostics$betweenss / object$diagnostics$totss
  } else {
    0
  }

  cat(sprintf(
    paste0(
      "Semantic topic model summary\n",
      "  documents:            %d\n",
      "  topics:               %d\n",
      "  model:                %s\n",
      "  between/total SS:      %.1f%%\n",
      "  mean %-5s coherence:  %.4f\n",
      "  topic topic_diversity:      %.3f (top %d terms)\n\n"
    ),
    nrow(object$documents),
    nrow(object$topics),
    object$model$id,
    100 * explained,
    measure,
    attr(coherence, "mean_coherence"),
    topic_diversity,
    as.integer(n_terms)
  ))

  quality <- data.frame(
    topic = object$topics$topic,
    label = object$topics$label,
    n_documents = object$topics$n_documents,
    proportion = object$topics$proportion,
    coherence = coherence$coherence,
    stringsAsFactors = FALSE
  )
  print(quality, row.names = FALSE, digits = 4)
  invisible(quality)
}
