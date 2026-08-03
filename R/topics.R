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

#' Obtain and Adjust the Topic Stop-Word List
#'
#' Returns the stop words excluded from term extraction and keyword
#' candidates. `add` extends the list — the standard way to remove a
#' corpus-wide vocabulary from topic labels (words every document shares
#' carry no discriminative information). `remove` un-lists built-in entries
#' whose surface form is meaningful in a specific corpus.
#'
#' @param language Currently only `"en"` is supported.
#' @param add Character vector of extra words to exclude (matched
#'   case-insensitively).
#' @param remove Character vector of words to drop from the list.
#' @return A sorted character vector of lowercase stop words.
#' @export
#' @examples
#' head(stop_words())
#' stop_words(add = c("students", "learning"), remove = "against")
stop_words <- function(language = "en", add = NULL, remove = NULL) {
  stopifnot(
    is.character(language),
    length(language) == 1L,
    !is.na(language),
    nzchar(language),
    is.null(add) || (is.character(add) && !anyNA(add)),
    is.null(remove) || (is.character(remove) && !anyNA(remove))
  )
  if (!identical(language, "en")) {
    stop("Only the built-in English stop-word list is currently available.", call. = FALSE)
  }
  words <- unique(c(.sbert_english_stopwords, tolower(add)))
  sort(setdiff(words, tolower(remove)))
}

tokenize_topic_documents <- function(text, stop_words, min_token_length, stem = FALSE) {
  stopifnot(
    is.character(text),
    !anyNA(text),
    is.character(stop_words),
    !anyNA(stop_words),
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
    gsub("\u2019", "'", tolower(enc2utf8(stop_words)), fixed = TRUE)
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
  stop_words,
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

  token_lists <- tokenize_topic_documents(text, stop_words, min_token_length, stem = stem)
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

deterministic_topic_centers <- function(embeddings, n_topics, existing_centers = NULL) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    nrow(embeddings) >= 2L,
    ncol(embeddings) >= 1L,
    !anyNA(embeddings),
    all(is.finite(embeddings)),
    is.numeric(n_topics),
    length(n_topics) == 1L,
    n_topics >= 1,
    n_topics == as.integer(n_topics),
    n_topics <= nrow(embeddings),
    is.null(existing_centers) ||
      (is.matrix(existing_centers) && ncol(existing_centers) == ncol(embeddings))
  )

  distinct_rows <- !duplicated(as.data.frame(embeddings))
  if (sum(distinct_rows) < n_topics) {
    stop(
      "n_topics cannot exceed the number of distinct document embeddings.",
      call. = FALSE
    )
  }

  # Squared distance from every embedding row to its nearest given center.
  minimum_distance_to <- function(centers_matrix) {
    distance_matrix <- vapply(
      seq_len(nrow(centers_matrix)),
      function(center_row) {
        center <- matrix(
          centers_matrix[center_row, ],
          nrow = nrow(embeddings),
          ncol = ncol(embeddings),
          byrow = TRUE
        )
        rowSums((embeddings - center)^2)
      },
      numeric(nrow(embeddings))
    )
    apply(distance_matrix, 1L, min)
  }

  first_center <- if (is.null(existing_centers) || nrow(existing_centers) == 0L) {
    global_center <- matrix(
      colMeans(embeddings),
      nrow = nrow(embeddings),
      ncol = ncol(embeddings),
      byrow = TRUE
    )
    which.max(rowSums((embeddings - global_center)^2))
  } else {
    # Seeded: the first free center is the point least covered by the seeds.
    which.max(minimum_distance_to(existing_centers))
  }
  selected <- Reduce(
    function(selected_rows, unused_iteration) {
      reference_centers <- rbind(
        existing_centers,
        embeddings[selected_rows, , drop = FALSE]
      )
      minimum_distances <- minimum_distance_to(reference_centers)
      minimum_distances[selected_rows] <- -Inf
      c(selected_rows, which.max(minimum_distances))
    },
    seq_len(n_topics - 1L),
    init = first_center
  )

  embeddings[selected, , drop = FALSE]
}

fit_embedding_topics <- function(
  embeddings,
  n_topics,
  iter_max,
  initial_centers = NULL,
  reorder = TRUE,
  fixed = FALSE
) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    !anyNA(embeddings),
    all(is.finite(embeddings)),
    is.numeric(iter_max),
    length(iter_max) == 1L,
    iter_max >= 1,
    iter_max == as.integer(iter_max),
    is.null(initial_centers) ||
      (is.matrix(initial_centers) && nrow(initial_centers) == n_topics),
    is.logical(reorder),
    length(reorder) == 1L,
    is.logical(fixed),
    length(fixed) == 1L,
    !fixed || !is.null(initial_centers)
  )

  normalized_embeddings <- normalize_embedding_rows(embeddings)
  starting_centers <- if (is.null(initial_centers)) {
    deterministic_topic_centers(normalized_embeddings, n_topics)
  } else {
    normalize_embedding_rows(initial_centers)
  }

  if (fixed) {
    # Zero-shot assignment: centroids stay exactly at the supplied centers and
    # every document joins its nearest one. Topics may legitimately be empty.
    assignment <- max.col(
      normalized_embeddings %*% t(starting_centers),
      ties.method = "first"
    )
    sizes <- tabulate(assignment, nbins = n_topics)
    fixed_withinss <- vapply(
      seq_len(n_topics),
      function(topic_id) {
        member_rows <- normalized_embeddings[
          assignment == topic_id, ,
          drop = FALSE
        ]
        if (nrow(member_rows) == 0L) {
          return(0)
        }
        center <- matrix(
          starting_centers[topic_id, ],
          nrow = nrow(member_rows),
          ncol = ncol(starting_centers),
          byrow = TRUE
        )
        sum((member_rows - center)^2)
      },
      numeric(1)
    )
    global_center <- matrix(
      colMeans(normalized_embeddings),
      nrow = nrow(normalized_embeddings),
      ncol = ncol(normalized_embeddings),
      byrow = TRUE
    )
    totss <- sum((normalized_embeddings - global_center)^2)
    fit <- list(
      cluster = as.integer(assignment),
      centers = unname(starting_centers),
      size = as.integer(sizes),
      withinss = fixed_withinss,
      totss = totss,
      tot.withinss = sum(fixed_withinss),
      betweenss = totss - sum(fixed_withinss),
      iter = 0L
    )
  } else {
    fit <- tryCatch(
      stats::kmeans(
        normalized_embeddings,
        centers = starting_centers,
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
  }

  topic_order <- if (reorder) {
    order(-fit$size, seq_len(n_topics))
  } else {
    # Seeded models keep centroid identity: topic i stays seed i.
    seq_len(n_topics)
  }
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
      algorithm = if (fixed) {
        "fixed seed centroids (nearest-centroid assignment)"
      } else {
        "deterministic k-means (Lloyd)"
      }
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
  encode(
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
#' @param column When `text` is a data frame, the name of the column
#'   holding the documents to model. Every other column is carried into
#'   `$documents`, and rows whose text is missing, blank, or a
#'   bibliographic placeholder are dropped. Leave `NULL` for a character
#'   vector.
#' @param model A loaded [sbert_model][load_model()], a pinned model
#'   name from [models()], or `NULL` for the default model. Ignored
#'   when `embeddings` are supplied.
#' @param embeddings Optional numeric matrix with one row per document;
#'   when supplied, no model is loaded or used.
#' @param batch_size Batch size passed to [encode()] when `model` is used.
#' @param iter_max Maximum deterministic k-means iterations.
#' @param n_terms Maximum class-based TF-IDF terms returned per topic.
#' @param n_representatives Maximum representative documents per topic.
#' @param stop_words Character vector excluded from topic terms. Use
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
#' @param seeds Optional guided topics: a character vector (or list of
#'   character vectors, collapsed with spaces) of seed words or topic
#'   descriptions — one element per seeded topic. Seeds are embedded and
#'   become the first `length(seeds)` cluster centroids; remaining topics
#'   (up to `n_topics`) are initialized away from the seeds. Seeded topics
#'   keep their position — topic i is seed i, never reordered by size — and
#'   a named `seeds` vector names those topics' labels. With precomputed
#'   `embeddings`, supply `seed_embeddings` as well.
#' @param seed_embeddings Optional matrix of seed embeddings (one row per
#'   seed, same dimension as the document embeddings); required when
#'   `seeds` is used together with precomputed `embeddings`.
#' @param fixed_seeds When `TRUE`, seed centroids are frozen and documents
#'   are simply assigned to the nearest seed (zero-shot classification into
#'   the seeded topics; `n_topics` must equal the number of seeds, and
#'   topics may be empty). When `FALSE` (default), seeds only initialize
#'   the clustering and the data can move the centroids.
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
#' topics <- topics(text, 2, embeddings = embeddings)
#' topics$topics
topics <- function(
  text,
  n_topics,
  column = NULL,
  model = NULL,
  embeddings = NULL,
  batch_size = 32L,
  iter_max = 100L,
  n_terms = 10L,
  n_representatives = 3L,
  stop_words = default_stop_words(),
  min_term_frequency = 1L,
  min_token_length = 2L,
  weighting = c("ctfidf", "bm25"),
  reduce_frequent_words = FALSE,
  stem = FALSE,
  keep_embeddings = TRUE,
  seeds = NULL,
  seed_embeddings = NULL,
  fixed_seeds = FALSE
) {
  weighting <- match.arg(weighting)

  # A data frame goes in whole: `column` names the text to model, every other
  # column rides along into `$documents`, and unusable rows are dropped here
  # rather than by every caller in turn. A character vector keeps the old path.
  prepared <- prepare_topic_input(text, column)
  text <- prepared$text
  metadata <- prepared$metadata
  if (!is.null(embeddings) && length(prepared$kept) < prepared$n_supplied) {
    embeddings <- embeddings[prepared$kept, , drop = FALSE]
  }
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
    is.character(stop_words),
    !anyNA(stop_words),
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

  seed_labels <- NULL
  initial_centers <- NULL
  if (!is.null(seeds)) {
    seed_texts <- if (is.list(seeds)) {
      vapply(seeds, function(seed) paste(seed, collapse = " "), character(1))
    } else {
      seeds
    }
    stopifnot(
      is.character(seed_texts),
      length(seed_texts) >= 1L,
      !anyNA(seed_texts),
      all(nzchar(trimws(seed_texts))),
      is.logical(fixed_seeds),
      length(fixed_seeds) == 1L,
      !is.na(fixed_seeds)
    )
    n_seeds <- length(seed_texts)
    if (fixed_seeds && n_seeds != n_topics) {
      stop(
        "With fixed_seeds = TRUE, n_topics must equal the number of seeds.",
        call. = FALSE
      )
    }
    if (n_seeds > n_topics) {
      stop("More seeds than topics; raise n_topics.", call. = FALSE)
    }
    if (is.null(seed_embeddings)) {
      if (is.null(model)) {
        stop(
          "Seeds need the encoding model; with precomputed embeddings, ",
          "supply seed_embeddings as well.",
          call. = FALSE
        )
      }
      seed_embeddings <- encode_topic_documents(
        unname(seed_texts),
        model,
        batch_size = as.integer(batch_size)
      )
    }
    stopifnot(
      is.matrix(seed_embeddings),
      nrow(seed_embeddings) == n_seeds,
      ncol(seed_embeddings) == ncol(embedding_matrix),
      !anyNA(seed_embeddings),
      all(is.finite(seed_embeddings))
    )
    seed_centers <- normalize_embedding_rows(seed_embeddings)
    free_topics <- as.integer(n_topics) - n_seeds
    initial_centers <- if (free_topics > 0L) {
      rbind(
        seed_centers,
        deterministic_topic_centers(
          normalize_embedding_rows(embedding_matrix),
          free_topics,
          existing_centers = seed_centers
        )
      )
    } else {
      seed_centers
    }
    seed_labels <- names(seed_texts)
  }

  clustering <- fit_embedding_topics(
    embedding_matrix,
    as.integer(n_topics),
    as.integer(iter_max),
    initial_centers = initial_centers,
    reorder = is.null(seeds),
    fixed = !is.null(seeds) && isTRUE(fixed_seeds)
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
  if (!is.null(metadata) && ncol(metadata) > 0L) {
    documents <- cbind(documents, metadata)
  }
  term_results <- topic_term_scores(
    text = unname(text),
    topic = clustering$topic,
    n_topics = as.integer(n_topics),
    n_terms = as.integer(n_terms),
    stop_words = stop_words,
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
  if (!is.null(seed_labels) && any(nzchar(seed_labels))) {
    named_seeds <- which(nzchar(seed_labels))
    topic_labels[named_seeds] <- seed_labels[named_seeds]
  }
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

  # The topic label belongs on every table that names a topic, so joining it
  # back by hand is never necessary.
  documents <- insert_topic_label(documents, topic_labels)
  term_results$terms <- insert_topic_label(term_results$terms, topic_labels)
  representatives <- insert_topic_label(representatives, topic_labels)

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
        stop_words = stop_words,
        seeds = if (is.null(seeds)) NULL else unname(
          if (is.list(seeds)) {
            vapply(seeds, function(seed) paste(seed, collapse = " "), character(1))
          } else {
            seeds
          }
        ),
        fixed_seeds = !is.null(seeds) && isTRUE(fixed_seeds)
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
