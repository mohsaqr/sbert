# Topic hierarchy and reduction: agglomerative clustering of topic centroids
# (cosine distance, deterministic) exposes which topics are near-duplicates
# and which are genuinely distinct, and cutting the tree merges a fitted
# model down to fewer topics without re-running k-means.

topic_labels_from_terms <- function(terms, n_topics) {
  vapply(
    seq_len(n_topics),
    function(topic_id) {
      label_terms <- terms$term[terms$topic == topic_id & terms$rank <= 3L]
      if (length(label_terms) == 0L) {
        sprintf("topic_%d", topic_id)
      } else {
        paste(label_terms, collapse = " / ")
      }
    },
    character(1)
  )
}

hierarchy_tree <- function(object, method) {
  centers <- normalize_rows(object$centers)
  cosine_distance <- 1 - tcrossprod(centers)
  cosine_distance[cosine_distance < 0] <- 0
  tree <- stats::hclust(stats::as.dist(cosine_distance), method = method)
  tree$labels <- object$topics$label
  tree
}

#' Build the Topic Hierarchy of a Fitted Model
#'
#' Agglomeratively clusters the topic centroids by cosine distance and
#' returns the merge tree: which topics are semantic neighbors, in what
#' order they would fuse, and how far apart they are. Use it to judge
#' whether a topic count is too fine (early merges at small heights are
#' near-duplicate topics) and to choose a target count for
#' [sbert_reduce()]. Deterministic — no sampling, no seed.
#'
#' @param object An `sbert_topic_model` returned by [sbert_topics()].
#' @param method Agglomeration method passed to [stats::hclust()]. Default
#'   `"average"`.
#' @return An `sbert_topic_hierarchy`: a list with `merges` (a tidy data
#'   frame with one row per merge — `step`, `height`, and the human-readable
#'   `left` and `right` branch descriptions) and `tree` (the underlying
#'   `hclust` object, labeled with topic labels). `print()` shows the merge
#'   table; `plot()` draws the labeled dendrogram.
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
#' topics <- sbert_topics(text, 3, embeddings = embeddings, n_terms = 3)
#' sbert_hierarchy(topics)
sbert_hierarchy <- function(object, method = "average") {
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.character(method),
    length(method) == 1L
  )
  if (nrow(object$topics) < 2L) {
    stop("A hierarchy needs at least two topics.", call. = FALSE)
  }
  tree <- hierarchy_tree(object, method)
  branch_name <- function(node) {
    if (node < 0L) {
      sprintf("topic %d (%s)", -node, object$topics$label[-node])
    } else {
      sprintf("merge %d", node)
    }
  }
  merges <- data.frame(
    step = seq_len(nrow(tree$merge)),
    height = tree$height,
    left = vapply(tree$merge[, 1L], branch_name, character(1)),
    right = vapply(tree$merge[, 2L], branch_name, character(1)),
    stringsAsFactors = FALSE
  )
  structure(
    list(merges = merges, tree = tree, n_topics = nrow(object$topics)),
    class = "sbert_topic_hierarchy"
  )
}

#' @export
print.sbert_topic_hierarchy <- function(x, ...) {
  cat(sprintf(
    "<sbert_topic_hierarchy> %d topics, %d merges (cosine distance)\n\n",
    x$n_topics,
    nrow(x$merges)
  ))
  print(x$merges, row.names = FALSE)
  invisible(x)
}

#' @export
plot.sbert_topic_hierarchy <- function(
  x,
  main = "Topic hierarchy",
  cex = 0.8,
  ...
) {
  previous <- par(mar = c(2.2, 4.2, 2.6, 0.6), mgp = c(2.4, 0.6, 0))
  on.exit(par(previous), add = TRUE)
  plot(
    x$tree,
    hang = -1,
    main = main,
    sub = "",
    xlab = "",
    ylab = "Cosine distance",
    cex = cex,
    font.main = 2,
    adj = 0.5,
    ...
  )
  invisible(x)
}

#' Reduce a Fitted Topic Model to Fewer Topics
#'
#' Cuts the topic hierarchy (see [sbert_hierarchy()]) at the requested
#' count and rebuilds the model: documents keep their cluster memberships
#' (merged, never re-clustered), centroids are recomputed from the member
#' documents, and terms, labels, sizes, and representatives are derived
#' afresh for the merged topics. The result is a full `sbert_topic_model`
#' — every downstream verb (`summary()`, `sbert_coherence()`, `predict()`,
#' [sbert_representatives()], plots) works unchanged.
#'
#' @param object An `sbert_topic_model` returned by [sbert_topics()].
#' @param n_topics Target number of topics, at least 2 and below the
#'   current count.
#' @param embeddings The document embedding matrix used to fit `object`.
#'   Only needed when the model was fitted with `keep_embeddings = FALSE`;
#'   models fitted with `keep_embeddings = TRUE` carry their embeddings.
#' @param method Agglomeration method passed to [stats::hclust()]. Default
#'   `"average"`.
#' @return An `sbert_topic_model` with `n_topics` topics.
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
#' topics <- sbert_topics(
#'   text, 3,
#'   embeddings = embeddings, n_terms = 3, keep_embeddings = TRUE
#' )
#' sbert_reduce(topics, 2)
sbert_reduce <- function(object, n_topics, embeddings = NULL, method = "average") {
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.numeric(n_topics),
    length(n_topics) == 1L,
    is.finite(n_topics),
    n_topics == as.integer(n_topics),
    n_topics >= 2L
  )
  n_topics <- as.integer(n_topics)
  if (n_topics >= nrow(object$topics)) {
    stop(
      "n_topics must be below the current topic count; ",
      "use the model as is for the same count.",
      call. = FALSE
    )
  }
  if (is.null(embeddings)) {
    embeddings <- object$embeddings
  }
  if (is.null(embeddings)) {
    stop(
      "The model carries no embeddings; refit with keep_embeddings = TRUE ",
      "or supply the original embedding matrix through 'embeddings'.",
      call. = FALSE
    )
  }
  stopifnot(
    is.matrix(embeddings),
    nrow(embeddings) == nrow(object$documents)
  )

  membership <- stats::cutree(hierarchy_tree(object, method), k = n_topics)
  normalized <- normalize_rows(embeddings)
  merged_topic <- as.integer(membership[object$documents$topic])
  # Package convention: topic 1 is the largest (ties broken by first
  # appearance), matching sbert_topics().
  first_sizes <- tabulate(merged_topic, nbins = n_topics)
  topic_order <- order(-first_sizes, seq_len(n_topics))
  renumbered <- integer(n_topics)
  renumbered[topic_order] <- seq_len(n_topics)
  merged_topic <- as.integer(renumbered[merged_topic])

  centers <- do.call(rbind, lapply(
    seq_len(n_topics),
    function(topic_id) {
      member_rows <- normalized[merged_topic == topic_id, , drop = FALSE]
      colMeans(member_rows)
    }
  ))
  centers <- normalize_rows(centers)

  assigned_centers <- centers[merged_topic, , drop = FALSE]
  distance <- 1 - rowSums(normalized * assigned_centers)
  distance[distance < 0] <- 0

  documents <- data.frame(
    document_id = object$documents$document_id,
    document_name = object$documents$document_name,
    text = object$documents$text,
    topic = merged_topic,
    distance = distance,
    stringsAsFactors = FALSE
  )

  settings <- object$settings
  term_results <- topic_term_scores(
    text = documents$text,
    topic = documents$topic,
    n_topics = n_topics,
    n_terms = settings$n_terms,
    stopwords = settings$stopwords,
    min_term_frequency = settings$min_term_frequency,
    min_token_length = settings$min_token_length,
    weighting = settings$weighting,
    reduce_frequent_words = settings$reduce_frequent_words,
    stem = settings$stem
  )
  labels <- topic_labels_from_terms(term_results$terms, n_topics)

  sizes <- tabulate(documents$topic, nbins = n_topics)
  withinss <- vapply(
    seq_len(n_topics),
    function(topic_id) {
      member_rows <- normalized[documents$topic == topic_id, , drop = FALSE]
      center_matrix <- matrix(
        centers[topic_id, ],
        nrow = nrow(member_rows),
        ncol = ncol(centers),
        byrow = TRUE
      )
      sum((member_rows - center_matrix)^2)
    },
    numeric(1)
  )
  global_center <- matrix(
    colMeans(normalized),
    nrow = nrow(normalized),
    ncol = ncol(normalized),
    byrow = TRUE
  )
  totss <- sum((normalized - global_center)^2)

  topics <- data.frame(
    topic = seq_len(n_topics),
    label = labels,
    n_documents = sizes,
    proportion = sizes / nrow(documents),
    withinss = withinss,
    stringsAsFactors = FALSE
  )

  settings$n_topics <- n_topics
  structure(
    list(
      documents = documents,
      topics = topics,
      terms = term_results$terms,
      representatives = topic_representatives(
        documents,
        n_topics,
        settings$n_representatives
      ),
      centers = centers,
      embeddings = if (is.null(object$embeddings)) NULL else normalized,
      diagnostics = list(
        totss = totss,
        tot_withinss = sum(withinss),
        betweenss = totss - sum(withinss),
        iterations = object$diagnostics$iterations,
        algorithm = "hierarchical merge of deterministic k-means topics"
      ),
      model = object$model,
      settings = settings
    ),
    class = "sbert_topic_model"
  )
}
