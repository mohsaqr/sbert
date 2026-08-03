# Embedding-based keyword extraction (the KeyBERT design): candidate words
# and phrases come from the document itself, are embedded with the same model
# as the document, and are ranked by cosine topic_similarity to the document vector.
# Maximal marginal relevance trades relevance against redundancy so the
# selected keywords do not all say the same thing.

# Internal encoding seam so tests can substitute a deterministic embedder.
encode_keyword_texts <- function(texts, model, batch_size) {
  model <- resolve_sbert_model(model)
  encode(texts, model = model, batch_size = batch_size)
}

# Consecutive-token n-grams of length 1..max_length over one token sequence.
token_ngrams <- function(tokens, max_length) {
  if (length(tokens) == 0L) {
    return(character(0))
  }
  lengths <- seq_len(min(max_length, length(tokens)))
  unique(unlist(
    lapply(
      lengths,
      function(size) {
        starts <- seq_len(length(tokens) - size + 1L)
        vapply(
          starts,
          function(start) {
            paste(tokens[seq.int(start, start + size - 1L)], collapse = " ")
          },
          character(1)
        )
      }
    ),
    use.names = FALSE
  ))
}

# Maximal marginal relevance: iteratively pick the candidate maximizing
# (1 - topic_diversity) * relevance - topic_diversity * max-topic_similarity-to-already-chosen.
# Candidates arrive alphabetically sorted, so which.max ties are deterministic.
mmr_select <- function(relevance, candidate_similarity, n, topic_diversity) {
  n_candidates <- length(relevance)
  picks <- min(n, n_candidates)
  state <- Reduce(
    function(current, step) {
      available <- current$available
      scores <- if (length(current$chosen) == 0L) {
        relevance[available]
      } else {
        redundancy <- apply(
          candidate_similarity[
            available,
            current$chosen,
            drop = FALSE
          ],
          1L,
          max
        )
        (1 - topic_diversity) * relevance[available] - topic_diversity * redundancy
      }
      pick <- available[which.max(scores)]
      list(
        chosen = c(current$chosen, pick),
        available = setdiff(available, pick)
      )
    },
    seq_len(picks),
    list(chosen = integer(0), available = seq_len(n_candidates))
  )
  state$chosen
}

#' Extract Keywords from Documents by Embedding Similarity
#'
#' Ranks each document's own words and phrases by cosine topic_similarity between
#' their embeddings and the document embedding, computed with the same model,
#' and selects the top `n` by maximal marginal relevance so the keywords are
#' relevant without being redundant (the KeyBERT design).
#'
#' Candidates are consecutive-token n-grams (lengths `1` to `ngrams`) drawn
#' from the document after stop-word removal, so a candidate phrase never
#' crosses a removed stop word silently: "analysis of networks" yields the
#' candidates "analysis", "networks", and "analysis networks".
#'
#' @param text A character vector of documents. Names, when present, are
#'   carried into the `document_name` column.
#' @param model A loaded sbert model, a pinned model name, or `NULL` for the
#'   session default.
#' @param n Maximum keywords returned per document. Default `10`.
#' @param ngrams Maximum phrase length in tokens. Default `2` (unigrams and
#'   bigrams).
#' @param topic_diversity Maximal-marginal-relevance trade-off in `[0, 1)`: `0`
#'   ranks purely by topic_similarity, larger values penalize keywords similar to
#'   ones already selected. Default `0.3`.
#' @param stop_words Words excluded from candidates. Defaults to
#'   [stop_words()].
#' @param min_token_length Minimum character length of a candidate token.
#'   Default `3`.
#' @param batch_size Number of texts encoded per model call. Default `32`.
#' @return A base data frame with one row per keyword and columns
#'   `document_id`, `document_name`, `rank`, `keyword`, and `topic_similarity`
#'   (cosine topic_similarity between the keyword and its document).
#' @export
#' @examples
#' \dontrun{
#' keywords(
#'   "Transition network analysis models learning event sequences.",
#'   n = 5
#' )
#' }
keywords <- function(
  text,
  model = NULL,
  n = 10L,
  ngrams = 2L,
  topic_diversity = 0.3,
  stop_words = default_stop_words(),
  min_token_length = 3L,
  batch_size = 32L
) {
  stopifnot(
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    all(nzchar(trimws(text))),
    is.numeric(n),
    length(n) == 1L,
    is.finite(n),
    n >= 1,
    n == as.integer(n),
    is.numeric(ngrams),
    length(ngrams) == 1L,
    is.finite(ngrams),
    ngrams >= 1,
    ngrams == as.integer(ngrams),
    is.numeric(topic_diversity),
    length(topic_diversity) == 1L,
    is.finite(topic_diversity),
    topic_diversity >= 0,
    topic_diversity < 1,
    is.character(stop_words),
    !anyNA(stop_words)
  )

  document_names <- names(text)
  if (is.null(document_names)) {
    document_names <- rep.int("", length(text))
  }
  token_lists <- tokenize_topic_documents(
    unname(text),
    stop_words = stop_words,
    min_token_length = min_token_length
  )
  candidate_lists <- lapply(token_lists, token_ngrams, max_length = ngrams)
  all_candidates <- sort(unique(unlist(candidate_lists, use.names = FALSE)))
  if (length(all_candidates) == 0L) {
    stop(
      "No keyword candidates survive tokenization; ",
      "lower min_token_length or supply fewer stop_words.",
      call. = FALSE
    )
  }

  document_embeddings <- normalize_rows(
    encode_keyword_texts(unname(text), model, batch_size)
  )
  candidate_embeddings <- normalize_rows(
    encode_keyword_texts(all_candidates, model, batch_size)
  )

  per_document <- lapply(
    seq_along(text),
    function(document_index) {
      candidates <- candidate_lists[[document_index]]
      if (length(candidates) == 0L) {
        return(NULL)
      }
      candidates <- sort(candidates)
      rows <- match(candidates, all_candidates)
      vectors <- candidate_embeddings[rows, , drop = FALSE]
      relevance <- as.numeric(
        vectors %*% document_embeddings[document_index, ]
      )
      chosen <- mmr_select(
        relevance = relevance,
        candidate_similarity = tcrossprod(vectors),
        n = n,
        topic_diversity = topic_diversity
      )
      data.frame(
        document_id = rep.int(document_index, length(chosen)),
        document_name = rep.int(
          document_names[[document_index]],
          length(chosen)
        ),
        rank = seq_along(chosen),
        keyword = candidates[chosen],
        topic_similarity = relevance[chosen],
        stringsAsFactors = FALSE
      )
    }
  )
  result <- do.call(rbind, Filter(Negate(is.null), per_document))
  rownames(result) <- NULL
  result
}
