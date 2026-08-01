#' Count Frequent N-grams Without Topic Weighting
#'
#' @param text Character vector of sentences.
#' @param n Integer n-gram length.
#' @param top_n Maximum number of n-grams to return.
#' @param stopwords Character vector used to remove n-grams made entirely of
#'   stop words. Single-word stop words are therefore excluded, while natural
#'   bigrams and trigrams containing content words are retained.
#' @return A data frame with `ngram`, `frequency`, and `rank` columns.
#' @examples
#' frequent_sentence_ngrams(
#'   c("choose the red box", "choose the blue box"),
#'   n = 2L,
#'   top_n = 3L
#' )
frequent_sentence_ngrams <- function(
  text,
  n,
  top_n = 10L,
  stopwords = character()
) {
  stopifnot(
    is.character(text),
    !anyNA(text),
    is.numeric(n),
    length(n) == 1L,
    is.finite(n),
    n >= 1,
    n == as.integer(n),
    is.numeric(top_n),
    length(top_n) == 1L,
    is.finite(top_n),
    top_n >= 1,
    top_n == as.integer(top_n),
    is.character(stopwords),
    !anyNA(stopwords)
  )

  normalized_text <- tolower(enc2utf8(text))
  normalized_text <- gsub("[’`]", "'", normalized_text, perl = TRUE)
  normalized_text <- gsub(
    "(*UTF)(*UCP)[^[:alnum:]']+",
    " ",
    normalized_text,
    perl = TRUE
  )
  token_lists <- strsplit(trimws(normalized_text), "[[:space:]]+", perl = TRUE)
  token_lists <- lapply(
    token_lists,
    function(tokens) tokens[nzchar(tokens)]
  )
  ngram_lists <- lapply(
    token_lists,
    function(tokens) {
      if (length(tokens) < n) {
        return(character())
      }
      vapply(
        seq_len(length(tokens) - n + 1L),
        function(start_index) {
          paste(tokens[start_index:(start_index + n - 1L)], collapse = " ")
        },
        character(1)
      )
    }
  )
  ngrams <- unlist(ngram_lists, use.names = FALSE)
  if (length(ngrams) == 0L) {
    return(data.frame(
      ngram = character(),
      frequency = integer(),
      rank = integer(),
      stringsAsFactors = FALSE
    ))
  }

  normalized_stopwords <- unique(tolower(enc2utf8(stopwords)))
  all_stopwords <- vapply(
    strsplit(ngrams, " ", fixed = TRUE),
    function(tokens) {
      length(normalized_stopwords) > 0L && all(tokens %in% normalized_stopwords)
    },
    logical(1)
  )
  frequencies <- table(ngrams[!all_stopwords])
  ranking <- order(-as.integer(frequencies), names(frequencies))
  selected <- utils::head(ranking, as.integer(top_n))

  data.frame(
    ngram = names(frequencies)[selected],
    frequency = as.integer(frequencies[selected]),
    rank = seq_along(selected),
    stringsAsFactors = FALSE
  )
}

#' Obtain Generic MCSE Words for Descriptive Topic Plots
#'
#' @return A sorted character vector of domain-wide and research-boilerplate
#'   words excluded from raw-frequency topic plots. These words are not removed
#'   from embeddings or topic assignments.
#' @examples
#' mcse_common_word_stopwords()
mcse_common_word_stopwords <- function() {
  sort(unique(c(
    "also", "based", "code", "coding", "computer", "computers",
    "computing", "course", "courses", "education", "educational",
    "learning", "paper", "papers", "program", "programming", "programs",
    "results", "science", "sciences", "student", "students", "students'",
    "studies", "study", "teaching", "use", "used", "using"
  )))
}

#' Select Full-Sentence Topic Representatives
#'
#' @param assignments Data frame containing topic assignments, full translated
#'   sentences, row identifiers, and cosine distances to topic centroids.
#' @param n_topics Number of topics.
#' @param n_representatives Maximum number of distinct sentences per topic.
#' @return A data frame of distinct centroid-nearest sentences ranked by topic.
#' @examples
#' rank_topic_sentences(
#'   data.frame(
#'     row_id = 1:3,
#'     feedback = letters[1:3],
#'     translation = c("alpha", "alpha", "beta"),
#'     topic = c(1L, 1L, 2L),
#'     cosine_distance_to_centroid = c(0.2, 0.1, 0.3)
#'   ),
#'   n_topics = 2L,
#'   n_representatives = 1L
#' )
rank_topic_sentences <- function(
  assignments,
  n_topics,
  n_representatives = 10L
) {
  required_columns <- c(
    "row_id", "feedback", "translation", "topic",
    "cosine_distance_to_centroid"
  )
  stopifnot(
    is.data.frame(assignments),
    all(required_columns %in% names(assignments)),
    is.integer(assignments$row_id),
    is.character(assignments$feedback),
    is.character(assignments$translation),
    is.numeric(assignments$topic),
    is.numeric(assignments$cosine_distance_to_centroid),
    !anyNA(assignments[, required_columns]),
    is.numeric(n_topics),
    length(n_topics) == 1L,
    n_topics >= 1,
    n_topics == as.integer(n_topics),
    all(assignments$topic %in% seq_len(n_topics)),
    is.numeric(n_representatives),
    length(n_representatives) == 1L,
    n_representatives >= 1,
    n_representatives == as.integer(n_representatives)
  )

  representative_tables <- lapply(
    seq_len(n_topics),
    function(topic_id) {
      topic_rows <- assignments[assignments$topic == topic_id, , drop = FALSE]
      ordered_rows <- topic_rows[
        order(topic_rows$cosine_distance_to_centroid, topic_rows$row_id),
        ,
        drop = FALSE
      ]
      distinct_rows <- ordered_rows[!duplicated(ordered_rows$translation), , drop = FALSE]
      selected <- utils::head(distinct_rows, as.integer(n_representatives))
      data.frame(
        topic = rep.int(as.integer(topic_id), nrow(selected)),
        rank = seq_len(nrow(selected)),
        row_id = selected$row_id,
        feedback = selected$feedback,
        representative_sentence = selected$translation,
        cosine_distance_to_centroid = selected$cosine_distance_to_centroid,
        stringsAsFactors = FALSE
      )
    }
  )
  representatives <- do.call(rbind, representative_tables)
  rownames(representatives) <- NULL
  representatives
}

#' Normalize Sentence Embedding Rows
#'
#' @param embeddings Numeric embedding matrix.
#' @return A numeric matrix with unit-length rows.
#' @examples
#' normalize_sentence_embeddings(matrix(c(3, 4, 0, 2), nrow = 2L, byrow = TRUE))
normalize_sentence_embeddings <- function(embeddings) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    nrow(embeddings) >= 1L,
    ncol(embeddings) >= 1L,
    !anyNA(embeddings),
    all(is.finite(embeddings))
  )
  row_norms <- sqrt(rowSums(embeddings^2))
  if (any(row_norms <= 0)) {
    stop("Every embedding row must have a positive norm.", call. = FALSE)
  }
  embeddings / row_norms
}

#' Adjusted Rand Agreement Between Two Partitions
#'
#' @param first_partition Integer or character cluster labels.
#' @param second_partition Integer or character cluster labels of the same
#'   observations.
#' @return A numeric scalar. One indicates identical partitions up to label
#'   permutation; values near zero indicate chance-level agreement.
#' @examples
#' adjusted_rand_index(c(1L, 1L, 2L, 2L), c(2L, 2L, 1L, 1L))
adjusted_rand_index <- function(first_partition, second_partition) {
  stopifnot(
    is.atomic(first_partition),
    is.atomic(second_partition),
    length(first_partition) == length(second_partition),
    length(first_partition) >= 2L,
    !anyNA(first_partition),
    !anyNA(second_partition)
  )
  contingency <- table(first_partition, second_partition)
  choose_two <- function(value) value * (value - 1) / 2
  pair_total <- choose_two(length(first_partition))
  cell_pairs <- sum(choose_two(contingency))
  row_pairs <- sum(choose_two(rowSums(contingency)))
  column_pairs <- sum(choose_two(colSums(contingency)))
  expected_pairs <- row_pairs * column_pairs / pair_total
  maximum_pairs <- (row_pairs + column_pairs) / 2
  denominator <- maximum_pairs - expected_pairs
  if (abs(denominator) < .Machine$double.eps) {
    return(if (cell_pairs == maximum_pairs) 1 else 0)
  }
  as.numeric((cell_pairs - expected_pairs) / denominator)
}

#' Extract the First Complete Sentence
#'
#' @param text Character vector containing one or more sentences per element.
#' @return A character vector containing the first nonblank sentence from each
#'   input element.
#' @examples
#' first_complete_sentence(c("First sentence. Second sentence.", "Only one"))
first_complete_sentence <- function(text) {
  stopifnot(is.character(text), !anyNA(text))
  sentence_lists <- strsplit(
    trimws(text),
    "(?<=[.!?])\\s+",
    perl = TRUE
  )
  vapply(
    sentence_lists,
    function(sentences) {
      nonblank <- sentences[nzchar(trimws(sentences))]
      if (length(nonblank) == 0L) "" else trimws(nonblank[[1L]])
    },
    character(1)
  )
}

#' Clean Bibliographic Abstract Text
#'
#' @param text Character vector of raw abstracts.
#' @return Whitespace-normalized abstracts with terminal publisher copyright
#'   statements removed. Missing values are preserved.
#' @examples
#' clean_bibliographic_abstract("A finding. Copyright 2020 ACM.")
clean_bibliographic_abstract <- function(text) {
  stopifnot(is.character(text))
  cleaned <- trimws(text)
  cleaned <- sub(
    paste0(
      "[[:space:]]*(?:(?:COPYRIGHT[[:space:]]*(?:©[[:space:]]*)?)|",
      "(?:©[[:space:]]*))[0-9]{4}.*$"
    ),
    "",
    cleaned,
    perl = TRUE,
    ignore.case = TRUE
  )
  gsub("[[:space:]]+", " ", cleaned, perl = TRUE)
}

#' Pack an Abstract into Model-sized Sentence-aware Chunks
#'
#' @param text One nonblank abstract.
#' @param token_count Function returning the number of model tokens for one
#'   character string, including special tokens.
#' @param max_tokens Maximum tokens allowed per chunk.
#' @return A character vector of consecutive chunks covering the abstract.
#' @examples
#' word_counter <- function(x) length(strsplit(x, " ", fixed = TRUE)[[1L]]) + 2L
#' pack_abstract_chunks(
#'   "One two three. Four five six.",
#'   token_count = word_counter,
#'   max_tokens = 5L
#' )
pack_abstract_chunks <- function(text, token_count, max_tokens = 256L) {
  stopifnot(
    is.character(text),
    length(text) == 1L,
    !is.na(text),
    nzchar(trimws(text)),
    is.function(token_count),
    is.numeric(max_tokens),
    length(max_tokens) == 1L,
    max_tokens >= 4,
    max_tokens == as.integer(max_tokens)
  )
  sentences <- strsplit(
    trimws(text),
    "(?<=[.!?])[[:space:]]+",
    perl = TRUE
  )[[1L]]
  sentences <- trimws(sentences[nzchar(trimws(sentences))])

  fitted_segments <- unlist(
    lapply(
      sentences,
      function(sentence) {
        if (token_count(sentence) <= max_tokens) {
          return(sentence)
        }
        words <- strsplit(sentence, "[[:space:]]+", perl = TRUE)[[1L]]
        split_state <- Reduce(
          function(state, word) {
            candidate <- trimws(paste(state$current, word))
            if (!nzchar(state$current) || token_count(candidate) <= max_tokens) {
              state$current <- candidate
            } else {
              state$chunks <- c(state$chunks, state$current)
              state$current <- word
            }
            state
          },
          words,
          init = list(chunks = character(), current = "")
        )
        c(split_state$chunks, split_state$current)
      }
    ),
    use.names = FALSE
  )

  packed_state <- Reduce(
    function(state, segment) {
      candidate <- trimws(paste(state$current, segment))
      if (!nzchar(state$current) || token_count(candidate) <= max_tokens) {
        state$current <- candidate
      } else {
        state$chunks <- c(state$chunks, state$current)
        state$current <- segment
      }
      state
    },
    fitted_segments,
    init = list(chunks = character(), current = "")
  )
  chunks <- c(packed_state$chunks, packed_state$current)
  chunks[nzchar(chunks)]
}

cosine_kmeans_plus_plus <- function(embeddings, n_topics, seed) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    nrow(embeddings) >= n_topics,
    is.numeric(n_topics),
    length(n_topics) == 1L,
    n_topics >= 2,
    n_topics == as.integer(n_topics),
    is.numeric(seed),
    length(seed) == 1L,
    is.finite(seed),
    seed == as.integer(seed)
  )

  seed_existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_existed) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit(
    if (seed_existed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    },
    add = TRUE
  )
  set.seed(as.integer(seed))

  first_center <- sample.int(nrow(embeddings), 1L)
  selected <- Reduce(
    function(selected_rows, unused_iteration) {
      similarities <- embeddings %*%
        t(embeddings[selected_rows, , drop = FALSE])
      nearest_similarity <- apply(similarities, 1L, max)
      sampling_weight <- pmax(0, 1 - nearest_similarity)
      sampling_weight[selected_rows] <- 0
      next_center <- if (sum(sampling_weight) > 0) {
        sample.int(nrow(embeddings), 1L, prob = sampling_weight)
      } else {
        which(!seq_len(nrow(embeddings)) %in% selected_rows)[[1L]]
      }
      c(selected_rows, next_center)
    },
    seq_len(n_topics - 1L),
    init = first_center
  )
  embeddings[selected, , drop = FALSE]
}

fit_spherical_once <- function(
  embeddings,
  n_topics,
  seed,
  iter_max = 100L
) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    nrow(embeddings) >= n_topics,
    is.numeric(n_topics),
    n_topics == as.integer(n_topics),
    n_topics >= 2,
    is.numeric(iter_max),
    iter_max == as.integer(iter_max),
    iter_max >= 1
  )
  normalized_embeddings <- normalize_sentence_embeddings(embeddings)
  initial_centers <- cosine_kmeans_plus_plus(
    normalized_embeddings,
    as.integer(n_topics),
    as.integer(seed)
  )
  initial_state <- list(
    centers = initial_centers,
    assignment = integer(),
    converged = FALSE,
    iterations = 0L
  )
  final_state <- Reduce(
    function(state, iteration) {
      if (state$converged) {
        return(state)
      }
      similarities <- normalized_embeddings %*% t(state$centers)
      assignment <- max.col(similarities, ties.method = "first")
      empty_topics <- which(tabulate(assignment, nbins = n_topics) == 0L)
      if (length(empty_topics) > 0L) {
        chosen_similarity <- similarities[cbind(
          seq_len(nrow(similarities)),
          assignment
        )]
        assignment <- Reduce(
          function(current_assignment, empty_topic) {
            current_size <- tabulate(current_assignment, nbins = n_topics)
            candidates <- order(chosen_similarity, seq_along(chosen_similarity))
            candidates <- candidates[current_size[current_assignment[candidates]] > 1L]
            current_assignment[candidates[[1L]]] <- empty_topic
            current_assignment
          },
          empty_topics,
          init = assignment
        )
      }
      center_sums <- rowsum(
        normalized_embeddings,
        group = factor(assignment, levels = seq_len(n_topics)),
        reorder = FALSE
      )
      updated_centers <- normalize_sentence_embeddings(center_sums)
      list(
        centers = updated_centers,
        assignment = assignment,
        converged = length(state$assignment) > 0L &&
          identical(assignment, state$assignment),
        iterations = as.integer(iteration)
      )
    },
    seq_len(iter_max),
    init = initial_state
  )

  final_similarities <- normalized_embeddings %*% t(final_state$centers)
  final_assignment <- max.col(final_similarities, ties.method = "first")
  chosen_similarity <- final_similarities[cbind(
    seq_len(nrow(final_similarities)),
    final_assignment
  )]
  alternative_similarities <- final_similarities
  alternative_similarities[cbind(
    seq_len(nrow(alternative_similarities)),
    final_assignment
  )] <- -Inf
  second_similarity <- apply(alternative_similarities, 1L, max)

  list(
    topic = as.integer(final_assignment),
    cosine_distance = pmax(0, 1 - chosen_similarity),
    assignment_margin = chosen_similarity - second_similarity,
    centers = final_state$centers,
    objective = sum(chosen_similarity),
    iterations = final_state$iterations,
    converged = final_state$converged,
    seed = as.integer(seed)
  )
}

#' Fit Multi-start Spherical Sentence Topics
#'
#' @param embeddings Numeric sentence embedding matrix.
#' @param n_topics Exact number of nonempty topics.
#' @param seeds Integer seeds defining independent cosine-aware restarts.
#' @param iter_max Maximum spherical k-means iterations per restart.
#' @return A list containing the best fit and restart diagnostics.
#' @examples
#' example_embeddings <- rbind(
#'   matrix(c(1, 0), 2L, 2L, byrow = TRUE),
#'   matrix(c(0, 1), 2L, 2L, byrow = TRUE)
#' )
#' fit_spherical_sentence_topics(example_embeddings, 2L, seeds = 1:2)
fit_spherical_sentence_topics <- function(
  embeddings,
  n_topics,
  seeds = 8:27,
  iter_max = 100L
) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    !anyNA(embeddings),
    all(is.finite(embeddings)),
    is.numeric(n_topics),
    n_topics == as.integer(n_topics),
    n_topics >= 2,
    is.numeric(seeds),
    length(seeds) >= 1L,
    !anyNA(seeds),
    all(is.finite(seeds)),
    all(seeds == as.integer(seeds)),
    length(unique(seeds)) == length(seeds)
  )
  fits <- lapply(
    as.integer(seeds),
    function(seed) {
      tryCatch(
        fit_spherical_once(
          embeddings,
          n_topics = as.integer(n_topics),
          seed = seed,
          iter_max = as.integer(iter_max)
        ),
        error = function(error_condition) {
          list(
            objective = -Inf,
            seed = seed,
            iterations = NA_integer_,
            converged = FALSE,
            error = conditionMessage(error_condition)
          )
        }
      )
    }
  )
  objectives <- vapply(fits, `[[`, numeric(1), "objective")
  if (!any(is.finite(objectives))) {
    stop("Every spherical k-means restart failed.", call. = FALSE)
  }
  best_index <- order(-objectives, as.integer(seeds))[[1L]]
  restart_diagnostics <- data.frame(
    seed = as.integer(seeds),
    objective = objectives,
    iterations = vapply(fits, `[[`, integer(1), "iterations"),
    converged = vapply(fits, `[[`, logical(1), "converged"),
    selected = seq_along(fits) == best_index,
    stringsAsFactors = FALSE
  )
  list(
    best_fit = fits[[best_index]],
    restart_diagnostics = restart_diagnostics,
    all_fits = fits
  )
}
