.sbert_english_stopwords <- c(
  "a", "about", "after", "again", "against", "all", "am", "an", "and",
  "any", "are", "as", "at", "be", "because", "been", "before", "being",
  "below", "between", "both", "but", "by", "can", "could", "did", "do",
  "does", "doing", "down", "during", "each", "few", "for", "from",
  "further", "had", "has", "have", "having", "he", "her", "here", "hers",
  "herself", "him", "himself", "his", "how", "i", "if", "in", "into",
  "is", "it", "its", "itself", "just", "me", "more", "most", "my",
  "myself", "no", "nor", "not", "now", "of", "off", "on", "once",
  "only", "or", "other", "our", "ours", "ourselves", "out", "over",
  "own", "same", "she", "should", "so", "some", "such", "than", "that",
  "the", "their", "theirs", "them", "themselves", "then", "there",
  "these", "they", "this", "those", "through", "to", "too", "under",
  "until", "up", "very", "was", "we", "were", "what", "when", "where",
  "which", "while", "who", "whom", "why", "will", "with", "would", "you",
  "your", "yours", "yourself", "yourselves"
)

#' Obtain Built-in Topic Stop Words
#'
#' @param language Currently only `"en"` is supported.
#' @return A sorted character vector of lowercase stop words.
#' @export
#' @examples
#' head(sbert_stopwords())
sbert_stopwords <- function(language = "en") {
  stopifnot(
    is.character(language),
    length(language) == 1L,
    !is.na(language),
    nzchar(language)
  )
  if (!identical(language, "en")) {
    stop("Only the built-in English stop-word list is currently available.", call. = FALSE)
  }
  sort(unique(.sbert_english_stopwords))
}

tokenize_topic_documents <- function(text, stopwords, min_token_length, stem = FALSE) {
  stopifnot(
    is.character(text),
    !anyNA(text),
    is.character(stopwords),
    !anyNA(stopwords),
    is.numeric(min_token_length),
    length(min_token_length) == 1L,
    is.finite(min_token_length),
    min_token_length >= 1,
    min_token_length == as.integer(min_token_length),
    is.logical(stem),
    length(stem) == 1L,
    !is.na(stem)
  )

  # Normalize the curly apostrophe (U+2019) to the straight one so that
  # contractions like "it's" and "it’s" are the same token.
  normalized_text <- gsub("\u2019", "'", tolower(enc2utf8(text)), fixed = TRUE)
  token_pattern <- "(*UTF)(*UCP)[[:alnum:]]+(?:['][[:alnum:]]+)*"
  token_matches <- gregexpr(token_pattern, normalized_text, perl = TRUE)
  token_lists <- regmatches(normalized_text, token_matches)
  normalized_stopwords <- unique(
    gsub("\u2019", "'", tolower(enc2utf8(stopwords)), fixed = TRUE)
  )

  filtered <- lapply(
    token_lists,
    function(tokens) {
      keep <- nchar(tokens, type = "chars") >= min_token_length &
        !tokens %in% normalized_stopwords
      unname(tokens[keep])
    }
  )
  if (stem) {
    filtered <- collapse_inflections(filtered)
  }
  filtered
}

# Collapse inflected forms (mice/mouse aside, e.g. animal/animals, mean/means)
# onto a shared Porter stem, but display the most frequent surface form for each
# stem so labels stay readable ("picture", not "pictur"). Deterministic.
collapse_inflections <- function(token_lists) {
  if (!requireNamespace("SnowballC", quietly = TRUE)) {
    stop(
      "Stemming requires the 'SnowballC' package. Install it or use stem = FALSE.",
      call. = FALSE
    )
  }
  all_tokens <- unlist(token_lists, use.names = FALSE)
  if (length(all_tokens) == 0L) {
    return(token_lists)
  }

  unique_tokens <- unique(all_tokens)
  token_stems <- SnowballC::wordStem(unique_tokens, language = "english")
  surface_frequency <- table(all_tokens)
  # Rank surfaces by descending corpus frequency, breaking ties alphabetically;
  # the first surface seen for each stem becomes that stem's display form.
  surface_order <- order(
    -as.integer(surface_frequency[unique_tokens]),
    unique_tokens
  )
  ordered_tokens <- unique_tokens[surface_order]
  ordered_stems <- token_stems[surface_order]
  display_form <- ordered_tokens[!duplicated(ordered_stems)]
  names(display_form) <- ordered_stems[!duplicated(ordered_stems)]
  canonical <- display_form[token_stems]
  names(canonical) <- unique_tokens

  lapply(
    token_lists,
    function(tokens) {
      if (length(tokens) == 0L) tokens else unname(canonical[tokens])
    }
  )
}

topic_term_scores <- function(
  text,
  topic,
  n_topics,
  n_terms,
  stopwords,
  min_term_frequency,
  min_token_length,
  weighting = c("ctfidf", "bm25"),
  reduce_frequent_words = FALSE,
  stem = FALSE
) {
  weighting <- match.arg(weighting)
  stopifnot(
    is.character(text),
    !anyNA(text),
    is.integer(topic),
    length(topic) == length(text),
    all(topic %in% seq_len(n_topics)),
    is.numeric(n_topics),
    length(n_topics) == 1L,
    n_topics == as.integer(n_topics),
    is.numeric(n_terms),
    length(n_terms) == 1L,
    n_terms >= 1,
    n_terms == as.integer(n_terms),
    is.numeric(min_term_frequency),
    length(min_term_frequency) == 1L,
    min_term_frequency >= 1,
    min_term_frequency == as.integer(min_term_frequency),
    is.logical(reduce_frequent_words),
    length(reduce_frequent_words) == 1L,
    !is.na(reduce_frequent_words)
  )

  token_lists <- tokenize_topic_documents(text, stopwords, min_token_length, stem = stem)
  all_tokens <- unlist(token_lists, use.names = FALSE)
  if (length(all_tokens) == 0L) {
    stop("No topic terms remain after tokenization and filtering.", call. = FALSE)
  }

  corpus_counts <- table(all_tokens)
  vocabulary <- sort(names(corpus_counts[corpus_counts >= min_term_frequency]))
  if (length(vocabulary) == 0L) {
    stop("No topic terms meet min_term_frequency.", call. = FALSE)
  }

  count_matrix <- do.call(
    rbind,
    lapply(
      seq_len(n_topics),
      function(topic_id) {
        topic_tokens <- unlist(token_lists[topic == topic_id], use.names = FALSE)
        tabulate(match(topic_tokens, vocabulary), nbins = length(vocabulary))
      }
    )
  )
  storage.mode(count_matrix) <- "double"
  colnames(count_matrix) <- vocabulary
  row_totals <- rowSums(count_matrix)
  # A is the average number of words per class (kept as a float to match the
  # published c-TF-IDF formula, Mendonca & Figueira 2025, Eq. 1); f_x is the
  # frequency of a term across all classes.
  average_topic_length <- mean(row_totals)
  if (average_topic_length < 1) {
    stop(
      "The average topic length is zero after tokenization and filtering.",
      call. = FALSE
    )
  }
  global_frequency <- pmax(colSums(count_matrix), 1)
  inverse_document_frequency <- if (weighting == "bm25") {
    # BM25 weighting (Eq. 2): down-weights terms common across classes.
    log1p((average_topic_length - global_frequency + 0.5) /
      (global_frequency + 0.5))
  } else {
    log1p(average_topic_length / global_frequency)
  }
  term_frequency <- count_matrix / pmax(row_totals, 1)
  if (reduce_frequent_words) {
    # reduce_frequent_words (Eq. 3): square-root damping of within-class tf.
    term_frequency <- sqrt(term_frequency)
  }
  score_matrix <- sweep(
    term_frequency,
    2L,
    inverse_document_frequency,
    `*`
  )

  topic_tables <- lapply(
    seq_len(n_topics),
    function(topic_id) {
      positive <- which(count_matrix[topic_id, ] > 0)
      if (length(positive) == 0L) {
        return(data.frame(
          topic = integer(),
          term = character(),
          rank = integer(),
          score = numeric(),
          frequency = integer(),
          stringsAsFactors = FALSE
        ))
      }
      ranking <- order(
        -score_matrix[topic_id, positive],
        -count_matrix[topic_id, positive],
        vocabulary[positive]
      )
      selected <- positive[utils::head(ranking, n_terms)]
      data.frame(
        topic = rep.int(as.integer(topic_id), length(selected)),
        term = vocabulary[selected],
        rank = seq_along(selected),
        score = unname(score_matrix[topic_id, selected]),
        frequency = as.integer(count_matrix[topic_id, selected]),
        stringsAsFactors = FALSE
      )
    }
  )

  terms <- do.call(rbind, topic_tables)
  rownames(terms) <- NULL
  list(
    terms = terms,
    counts = count_matrix,
    scores = score_matrix,
    average_topic_length = average_topic_length
  )
}

deterministic_topic_centers <- function(embeddings, n_topics) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    nrow(embeddings) >= 2L,
    ncol(embeddings) >= 1L,
    !anyNA(embeddings),
    all(is.finite(embeddings)),
    is.numeric(n_topics),
    length(n_topics) == 1L,
    n_topics >= 2,
    n_topics == as.integer(n_topics),
    n_topics <= nrow(embeddings)
  )

  distinct_rows <- !duplicated(as.data.frame(embeddings))
  if (sum(distinct_rows) < n_topics) {
    stop(
      "n_topics cannot exceed the number of distinct document embeddings.",
      call. = FALSE
    )
  }

  global_center <- matrix(
    colMeans(embeddings),
    nrow = nrow(embeddings),
    ncol = ncol(embeddings),
    byrow = TRUE
  )
  first_center <- which.max(rowSums((embeddings - global_center)^2))
  selected <- Reduce(
    function(selected_rows, unused_iteration) {
      distance_matrix <- vapply(
        selected_rows,
        function(center_row) {
          center <- matrix(
            embeddings[center_row, ],
            nrow = nrow(embeddings),
            ncol = ncol(embeddings),
            byrow = TRUE
          )
          rowSums((embeddings - center)^2)
        },
        numeric(nrow(embeddings))
      )
      minimum_distances <- apply(distance_matrix, 1L, min)
      minimum_distances[selected_rows] <- -Inf
      c(selected_rows, which.max(minimum_distances))
    },
    seq_len(n_topics - 1L),
    init = first_center
  )

  embeddings[selected, , drop = FALSE]
}

fit_embedding_topics <- function(embeddings, n_topics, iter_max) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    !anyNA(embeddings),
    all(is.finite(embeddings)),
    is.numeric(iter_max),
    length(iter_max) == 1L,
    iter_max >= 1,
    iter_max == as.integer(iter_max)
  )

  normalized_embeddings <- normalize_embedding_rows(embeddings)
  initial_centers <- deterministic_topic_centers(
    normalized_embeddings,
    n_topics
  )
  fit <- tryCatch(
    stats::kmeans(
      normalized_embeddings,
      centers = initial_centers,
      iter.max = as.integer(iter_max),
      algorithm = "Lloyd"
    ),
    error = function(error_condition) {
      stop(
        sprintf("Topic clustering failed: %s", conditionMessage(error_condition)),
        call. = FALSE
      )
    }
  )
  if (length(fit$size) != n_topics || any(fit$size <= 0L)) {
    stop("Topic clustering produced an empty topic.", call. = FALSE)
  }

  topic_order <- order(-fit$size, seq_len(n_topics))
  new_topic_id <- integer(n_topics)
  new_topic_id[topic_order] <- seq_len(n_topics)
  topic <- as.integer(new_topic_id[fit$cluster])
  centers <- unname(fit$centers[topic_order, , drop = FALSE])
  normalized_centers <- normalize_embedding_rows(centers)
  cosine_similarity <- vapply(
    seq_len(nrow(normalized_embeddings)),
    function(document_id) {
      sum(
        normalized_embeddings[document_id, ] *
          normalized_centers[topic[[document_id]], ]
      )
    },
    numeric(1)
  )

  list(
    topic = topic,
    distance = pmax(0, 1 - cosine_similarity),
    centers = centers,
    normalized_embeddings = normalized_embeddings,
    size = as.integer(fit$size[topic_order]),
    withinss = unname(fit$withinss[topic_order]),
    diagnostics = list(
      totss = unname(fit$totss),
      tot_withinss = unname(fit$tot.withinss),
      betweenss = unname(fit$betweenss),
      iterations = as.integer(fit$iter),
      algorithm = "deterministic k-means (Lloyd)"
    )
  )
}

topic_representatives <- function(documents, n_topics, n_representatives) {
  stopifnot(
    is.data.frame(documents),
    all(c("document_id", "document_name", "text", "topic", "distance") %in%
      names(documents)),
    is.numeric(n_representatives),
    length(n_representatives) == 1L,
    n_representatives >= 1,
    n_representatives == as.integer(n_representatives)
  )

  representatives <- lapply(
    seq_len(n_topics),
    function(topic_id) {
      topic_documents <- documents[documents$topic == topic_id, , drop = FALSE]
      ranking <- order(topic_documents$distance, topic_documents$document_id)
      selected <- utils::head(ranking, n_representatives)
      data.frame(
        topic = rep.int(as.integer(topic_id), length(selected)),
        rank = seq_along(selected),
        document_id = topic_documents$document_id[selected],
        document_name = topic_documents$document_name[selected],
        text = topic_documents$text[selected],
        distance = topic_documents$distance[selected],
        stringsAsFactors = FALSE
      )
    }
  )
  result <- do.call(rbind, representatives)
  rownames(result) <- NULL
  result
}

encode_topic_documents <- function(text, model, batch_size) {
  stopifnot(
    is.character(text),
    !anyNA(text),
    inherits(model, "sbert_model"),
    is.numeric(batch_size),
    length(batch_size) == 1L,
    batch_size >= 1,
    batch_size == as.integer(batch_size)
  )
  sbert_encode(
    text,
    model,
    batch_size = as.integer(batch_size),
    normalize = TRUE
  )
}

#' Discover Semantic Topics in Documents
#'
#' Performs deterministic document-level topic clustering with Sentence-BERT
#' embeddings. Supply either a loaded `model` or a precomputed embedding matrix.
#' Topics are summarized with representative documents and class-based TF-IDF
#' terms. This is embedding-based topic discovery, not a probabilistic LDA model.
#'
#' @details Topic labels are arbitrary cluster identifiers. Interpret them with
#'   the ranked `terms` and `representatives` tables. Term scores use the
#'   class-based TF-IDF weighting `tf * log(1 + A / f_x)`, where `tf` is the
#'   frequency of a term normalized within its topic, `f_x` is the term's
#'   frequency across all topics, and `A` is the mean topic length (a real
#'   number, matching Mendonca and Figueira (2025), Eq. 1). Set
#'   `weighting = "bm25"` and/or `reduce_frequent_words = TRUE` for the BM25 and
#'   square-root variants (their Eq. 2 to 4). The built-in tokenizer preserves
#'   Unicode alphanumeric tokens and internal apostrophes, but does not perform
#'   language-specific word segmentation for unspaced CJK text.
#'
#' @param text Character vector containing one document per element.
#' @param n_topics Number of semantic topics. Must be at least two.
#' @param model A loaded [sbert_model][sbert_load_model()], a pinned model
#'   name from [sbert_models()], or `NULL` for the default model. Ignored
#'   when `embeddings` are supplied.
#' @param embeddings Optional numeric matrix with one row per document;
#'   when supplied, no model is loaded or used.
#' @param batch_size Batch size passed to [sbert_encode()] when `model` is used.
#' @param iter_max Maximum deterministic k-means iterations.
#' @param n_terms Maximum class-based TF-IDF terms returned per topic.
#' @param n_representatives Maximum representative documents per topic.
#' @param stopwords Character vector excluded from topic terms. Use
#'   `character()` to disable stop-word filtering.
#' @param min_term_frequency Minimum corpus-wide token frequency.
#' @param min_token_length Minimum Unicode character length for a topic token.
#' @param weighting Class-based term-weighting scheme. `"ctfidf"` (default) uses
#'   `tf * log(1 + A / f_x)`; `"bm25"` uses the BM25 inverse-frequency variant
#'   `tf * log(1 + (A - f_x + 0.5) / (f_x + 0.5))`, which more aggressively
#'   down-weights terms shared across topics (Mendonca and Figueira 2025).
#' @param reduce_frequent_words Whether to square-root the within-topic term
#'   frequency before weighting, damping very frequent words.
#' @param stem Whether to collapse inflected forms (for example `animals` and
#'   `animal`) onto a shared Porter stem before scoring, displaying the most
#'   frequent surface form of each stem. Requires the `SnowballC` package.
#' @param keep_embeddings Whether to retain normalized document embeddings in
#'   the returned object.
#' @return An object of class `sbert_topic_model` containing document
#'   assignments, topic summaries, ranked terms, representatives, centers, and
#'   clustering diagnostics.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' topics <- sbert_topics(text, 2, embeddings = embeddings)
#' topics$topics
sbert_topics <- function(
  text,
  n_topics,
  model = NULL,
  embeddings = NULL,
  batch_size = 32L,
  iter_max = 100L,
  n_terms = 10L,
  n_representatives = 3L,
  stopwords = sbert_stopwords(),
  min_term_frequency = 1L,
  min_token_length = 2L,
  weighting = c("ctfidf", "bm25"),
  reduce_frequent_words = FALSE,
  stem = FALSE,
  keep_embeddings = FALSE
) {
  weighting <- match.arg(weighting)
  stopifnot(
    is.character(text),
    !anyNA(text),
    length(text) >= 2L,
    is.numeric(n_topics),
    length(n_topics) == 1L,
    is.finite(n_topics),
    n_topics >= 2,
    n_topics == as.integer(n_topics),
    n_topics <= length(text),
    is.numeric(batch_size),
    length(batch_size) == 1L,
    is.finite(batch_size),
    batch_size >= 1,
    batch_size == as.integer(batch_size),
    is.numeric(iter_max),
    length(iter_max) == 1L,
    is.finite(iter_max),
    iter_max >= 1,
    iter_max == as.integer(iter_max),
    is.numeric(n_terms),
    length(n_terms) == 1L,
    is.finite(n_terms),
    n_terms >= 1,
    n_terms == as.integer(n_terms),
    is.numeric(n_representatives),
    length(n_representatives) == 1L,
    is.finite(n_representatives),
    n_representatives >= 1,
    n_representatives == as.integer(n_representatives),
    is.character(stopwords),
    !anyNA(stopwords),
    is.numeric(min_term_frequency),
    length(min_term_frequency) == 1L,
    is.finite(min_term_frequency),
    min_term_frequency >= 1,
    min_term_frequency == as.integer(min_term_frequency),
    is.numeric(min_token_length),
    length(min_token_length) == 1L,
    is.finite(min_token_length),
    min_token_length >= 1,
    min_token_length == as.integer(min_token_length),
    is.logical(reduce_frequent_words),
    length(reduce_frequent_words) == 1L,
    !is.na(reduce_frequent_words),
    is.logical(stem),
    length(stem) == 1L,
    !is.na(stem),
    is.logical(keep_embeddings),
    length(keep_embeddings) == 1L,
    !is.na(keep_embeddings)
  )
  if (stem && !requireNamespace("SnowballC", quietly = TRUE)) {
    stop(
      "stem = TRUE requires the 'SnowballC' package. Install it or use stem = FALSE.",
      call. = FALSE
    )
  }
  if (any(!nzchar(trimws(text)))) {
    stop("text cannot contain blank documents.", call. = FALSE)
  }
  if (!is.null(model) && !is.null(embeddings)) {
    stop("Supply model or embeddings, not both.", call. = FALSE)
  }

  if (is.null(embeddings)) {
    model <- resolve_sbert_model(model)
    embedding_matrix <- encode_topic_documents(
      text,
      model,
      batch_size = as.integer(batch_size)
    )
    model_information <- list(
      id = model$id,
      revision = model$revision,
      dimension = model$dimension
    )
  } else {
    embedding_matrix <- embeddings
    model_information <- list(
      id = "precomputed embeddings",
      revision = NA_character_,
      dimension = if (is.matrix(embeddings)) ncol(embeddings) else NA_integer_
    )
  }
  if (
    !is.matrix(embedding_matrix) ||
      !is.numeric(embedding_matrix) ||
      nrow(embedding_matrix) != length(text) ||
      ncol(embedding_matrix) < 1L ||
      anyNA(embedding_matrix) ||
      any(!is.finite(embedding_matrix))
  ) {
    stop(
      "embeddings must be a finite numeric matrix with one row per document.",
      call. = FALSE
    )
  }

  clustering <- fit_embedding_topics(
    embedding_matrix,
    as.integer(n_topics),
    as.integer(iter_max)
  )
  document_names <- names(text)
  if (is.null(document_names)) {
    document_names <- rep.int("", length(text))
  }
  documents <- data.frame(
    document_id = seq_along(text),
    document_name = unname(document_names),
    text = unname(text),
    topic = clustering$topic,
    distance = clustering$distance,
    stringsAsFactors = FALSE
  )
  term_results <- topic_term_scores(
    text = unname(text),
    topic = clustering$topic,
    n_topics = as.integer(n_topics),
    n_terms = as.integer(n_terms),
    stopwords = stopwords,
    min_term_frequency = as.integer(min_term_frequency),
    min_token_length = as.integer(min_token_length),
    weighting = weighting,
    reduce_frequent_words = reduce_frequent_words,
    stem = stem
  )
  topic_labels <- vapply(
    seq_len(n_topics),
    function(topic_id) {
      label_terms <- term_results$terms$term[
        term_results$terms$topic == topic_id & term_results$terms$rank <= 3L
      ]
      if (length(label_terms) == 0L) {
        sprintf("topic_%d", topic_id)
      } else {
        paste(label_terms, collapse = " / ")
      }
    },
    character(1)
  )
  topics <- data.frame(
    topic = seq_len(n_topics),
    label = topic_labels,
    n_documents = clustering$size,
    proportion = clustering$size / length(text),
    withinss = clustering$withinss,
    stringsAsFactors = FALSE
  )
  representatives <- topic_representatives(
    documents,
    as.integer(n_topics),
    as.integer(n_representatives)
  )

  structure(
    list(
      documents = documents,
      topics = topics,
      terms = term_results$terms,
      representatives = representatives,
      centers = clustering$centers,
      embeddings = if (keep_embeddings) {
        clustering$normalized_embeddings
      } else {
        NULL
      },
      diagnostics = clustering$diagnostics,
      model = model_information,
      settings = list(
        n_topics = as.integer(n_topics),
        n_terms = as.integer(n_terms),
        n_representatives = as.integer(n_representatives),
        min_term_frequency = as.integer(min_term_frequency),
        min_token_length = as.integer(min_token_length),
        weighting = weighting,
        reduce_frequent_words = reduce_frequent_words,
        stem = stem,
        stopwords = stopwords
      )
    ),
    class = "sbert_topic_model"
  )
}

#' @export
print.sbert_topic_model <- function(x, ...) {
  stopifnot(inherits(x, "sbert_topic_model"))
  explained <- if (x$diagnostics$totss > 0) {
    x$diagnostics$betweenss / x$diagnostics$totss
  } else {
    0
  }
  cat(sprintf(
    paste0(
      "<sbert_topic_model>\n",
      "  documents: %d\n",
      "  topics: %d\n",
      "  model: %s\n",
      "  algorithm: %s\n",
      "  topic sizes: %s\n",
      "  between/total SS: %.1f%%\n"
    ),
    nrow(x$documents),
    nrow(x$topics),
    x$model$id,
    x$diagnostics$algorithm,
    paste(x$topics$n_documents, collapse = ", "),
    100 * explained
  ))
  invisible(x)
}
