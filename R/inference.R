# Inferential layer for fitted topic models: assignment of new documents,
# soft topic topic_membership, generative word probabilities, and document-topic
# distributions. Everything operates on the deterministic k-means centroids
# stored in the fitted model; nothing here refits or perturbs the clustering.

# Assign embedding rows to the nearest stored centroid by cosine distance.
assign_topic_embeddings <- function(embeddings, centers) {
  stopifnot(
    is.matrix(embeddings),
    is.numeric(embeddings),
    nrow(embeddings) >= 1L,
    !anyNA(embeddings),
    all(is.finite(embeddings)),
    is.matrix(centers),
    is.numeric(centers)
  )
  if (ncol(embeddings) != ncol(centers)) {
    stop(
      sprintf(
        "Embeddings have %d dimensions but the topic model was fitted with %d.",
        ncol(embeddings),
        ncol(centers)
      ),
      call. = FALSE
    )
  }

  normalized <- normalize_embedding_rows(embeddings)
  normalized_centers <- normalize_embedding_rows(centers)
  topic_similarity <- normalized %*% t(normalized_centers)
  topic <- max.col(topic_similarity, ties.method = "first")
  distance <- pmax(
    0,
    1 - topic_similarity[cbind(seq_len(nrow(topic_similarity)), topic)]
  )
  list(
    topic = as.integer(topic),
    distance = unname(distance),
    topic_similarity = unname(topic_similarity)
  )
}

# Resolve the embedding matrix for new text from exactly one of model /
# embeddings, mirroring the topics() contract.
resolve_new_embeddings <- function(text, model, embeddings, batch_size, n_expected) {
  if (!is.null(model) && !is.null(embeddings)) {
    stop("Supply model or embeddings, not both.", call. = FALSE)
  }
  if (is.null(embeddings)) {
    embeddings <- encode(
      text,
      resolve_sbert_model(model),
      batch_size = as.integer(batch_size),
      normalize = TRUE
    )
  }
  if (
    !is.matrix(embeddings) ||
      !is.numeric(embeddings) ||
      nrow(embeddings) != n_expected ||
      anyNA(embeddings) ||
      any(!is.finite(embeddings))
  ) {
    stop(
      sprintf(
        "embeddings must be a finite numeric matrix with %d rows (one per unit).",
        n_expected
      ),
      call. = FALSE
    )
  }
  embeddings
}

#' Assign New Documents to Fitted Topics
#'
#' Predicts the topic of unseen documents by embedding them and assigning each
#' to the nearest stored topic centroid under cosine distance. The fitted
#' model is not modified. Supply either a loaded `model` (to embed `text`) or
#' a precomputed embedding matrix whose rows align with `text`.
#'
#' @param object A fitted [topics()] model.
#' @param text Character vector of new documents.
#' @param model A loaded [sbert_model][load_model()], a pinned model
#'   name, or `NULL` for the default model; ignored when `embeddings` are
#'   supplied. The embedding dimension must match the fitted model.
#' @param embeddings Optional numeric matrix with one row per document.
#' @param batch_size Batch size passed to [encode()] when `model` is
#'   used.
#' @param ... Unused; included for S3 compatibility.
#' @return A base data frame with one row per document and columns
#'   `document_id`, `document_name`, `text`, `topic`, `label` (the fitted
#'   topic label), and `distance` (cosine distance to the assigned centroid).
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' fitted <- topics(text, 2, embeddings = embeddings)
#' predict(fitted, "Bulls and bears move markets", embeddings = rbind(c(0.2, 0.8)))
predict.sbert_topic_model <- function(
  object,
  text,
  model = NULL,
  embeddings = NULL,
  batch_size = 32L,
  ...
) {
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    is.numeric(batch_size),
    length(batch_size) == 1L,
    is.finite(batch_size),
    batch_size >= 1,
    batch_size == as.integer(batch_size)
  )

  embedding_matrix <- resolve_new_embeddings(
    text,
    model,
    embeddings,
    batch_size,
    n_expected = length(text)
  )
  assignment <- assign_topic_embeddings(embedding_matrix, object$centers)
  document_names <- names(text)
  if (is.null(document_names)) {
    document_names <- rep.int("", length(text))
  }

  data.frame(
    document_id = seq_along(text),
    document_name = unname(document_names),
    text = unname(text),
    topic = assignment$topic,
    label = object$topics$label[assignment$topic],
    distance = assignment$distance,
    stringsAsFactors = FALSE
  )
}

#' Soft Topic Membership Probabilities
#'
#' Computes fuzzy-c-means-style topic_membership of every document in every topic
#' from cosine distances to the stored centroids, without changing the
#' deterministic hard clustering. In high-dimensional embedding spaces
#' distances concentrate, so the textbook fuzzifier `sharpness = 2` collapses
#' memberships toward the uniform `1/k`; the default `sharpness = 1.15`
#' preserves contrast. The topic_membership ranking of topics within a document is
#' invariant to `sharpness`; the probability magnitudes are not, and should be
#' interpreted relative to the chosen value.
#'
#' @param object A fitted [topics()] model.
#' @param embeddings Optional numeric matrix of document embeddings. When
#'   omitted, the embeddings stored by `topics(keep_embeddings = TRUE)`
#'   are used.
#' @param sharpness Fuzzy-c-means fuzzifier `m`. Must be greater than 1;
#'   values close to 1 give crisper memberships.
#' @return A base data frame with one row per document-topic pair and columns
#'   `document_id`, `topic`, `probability`, and `rank` (1 is the strongest
#'   topic of the document). Probabilities sum to 1 within each document.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' fitted <- topics(text, 2, embeddings = embeddings, keep_embeddings = TRUE)
#' topic_membership(fitted)
topic_membership <- function(object, embeddings = NULL, sharpness = 1.15) {
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.numeric(sharpness),
    length(sharpness) == 1L,
    is.finite(sharpness),
    sharpness > 1
  )
  if (is.null(embeddings)) {
    embeddings <- object$embeddings
    if (is.null(embeddings)) {
      stop(
        paste0(
          "The model stores no embeddings. Refit with keep_embeddings = TRUE ",
          "or supply embeddings."
        ),
        call. = FALSE
      )
    }
  }

  assignment <- assign_topic_embeddings(embeddings, object$centers)
  distance <- pmax(1 - assignment$topic_similarity, 1e-12)
  # Fuzzy-c-means topic_membership computed in log space for numerical stability:
  # u_ij = d_ij^(-2/(m-1)) / sum_l d_il^(-2/(m-1)).
  weight <- -(2 / (sharpness - 1)) * log(distance)
  weight <- weight - apply(weight, 1L, max)
  unnormalized <- exp(weight)
  probability <- unnormalized / rowSums(unnormalized)

  n_documents <- nrow(probability)
  n_topics <- ncol(probability)
  long <- data.frame(
    document_id = rep.int(seq_len(n_documents), rep.int(n_topics, n_documents)),
    topic = rep.int(seq_len(n_topics), n_documents),
    probability = as.numeric(t(probability)),
    stringsAsFactors = FALSE
  )
  ordering <- order(long$document_id, -long$probability, long$topic)
  long <- long[ordering, , drop = FALSE]
  long$rank <- rep.int(seq_len(n_topics), n_documents)
  rownames(long) <- NULL
  long
}


#' Document-Topic Distributions from Segment Assignments
#'
#' Computes `gamma`, the distribution of every document over the fitted
#' topics, by segmenting each document with [segment()], assigning each
#' segment to its nearest topic centroid, and normalizing the per-document
#' segment counts. This yields parameter-free mixed topic_membership: a document
#' that discusses two topics in different sentences receives weight on both,
#' which the single-embedding hard assignment cannot express.
#'
#' @param object A fitted [topics()] model.
#' @param text Character vector of documents.
#' @param model A loaded [sbert_model][load_model()], a pinned model
#'   name, or `NULL` for the default model, used to embed the segments;
#'   ignored when `embeddings` are supplied.
#' @param embeddings Optional precomputed numeric matrix of segment
#'   embeddings whose rows align with `segment(text, level = level)`.
#' @param level Segmentation granularity passed to [segment()].
#' @param batch_size Batch size passed to [encode()] when `model` is
#'   used.
#' @return A base data frame with one row per document-topic pair and columns
#'   `document_id`, `topic`, `gamma`, and `n_segments`. `gamma` sums to 1
#'   within each document; documents with no segments contribute no rows.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' fitted <- topics(text, 2, embeddings = embeddings)
#' mixed <- "Cats chase mice. Stocks and bonds trade."
#' segment_embeddings <- rbind(c(1, 0), c(0, 1))
#' topic_gamma(fitted, mixed, embeddings = segment_embeddings)
topic_gamma <- function(
  object,
  text,
  model = NULL,
  embeddings = NULL,
  level = c("clause", "sentence", "phrase"),
  batch_size = 32L
) {
  level <- match.arg(level)
  stopifnot(
    inherits(object, "sbert_topic_model"),
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    is.numeric(batch_size),
    length(batch_size) == 1L,
    is.finite(batch_size),
    batch_size >= 1,
    batch_size == as.integer(batch_size)
  )

  segments <- segment(text, level = level)
  if (nrow(segments) == 0L) {
    stop("No document produced any segment.", call. = FALSE)
  }
  embedding_matrix <- resolve_new_embeddings(
    segments$text,
    model,
    embeddings,
    batch_size,
    n_expected = nrow(segments)
  )
  assignment <- assign_topic_embeddings(embedding_matrix, object$centers)

  n_topics <- nrow(object$topics)
  segment_counts <- table(
    factor(segments$document_id, levels = sort(unique(segments$document_id))),
    factor(assignment$topic, levels = seq_len(n_topics))
  )
  count_matrix <- matrix(
    as.numeric(segment_counts),
    nrow = nrow(segment_counts)
  )
  totals <- rowSums(count_matrix)
  gamma_matrix <- count_matrix / totals
  documents_present <- as.integer(rownames(segment_counts))

  data.frame(
    document_id = rep.int(documents_present, rep.int(n_topics, length(documents_present))),
    topic = rep.int(seq_len(n_topics), length(documents_present)),
    gamma = as.numeric(t(gamma_matrix)),
    n_segments = rep.int(as.integer(totals), rep.int(n_topics, length(documents_present))),
    stringsAsFactors = FALSE
  )
}

#' Representative Text Units for Every Topic
#'
#' Selects, for each fitted topic, the text units (documents, sentences, or
#' clause segments) that best evidence it. The default ranking is by
#' *distinctiveness margin* — the unit's cosine distance to its second-best
#' centroid minus the distance to its own — rather than by raw closeness.
#' Raw closeness systematically favors long units, because topic centroids
#' are averages of whole-document embeddings and long units resemble whole
#' documents; the margin instead rewards units that are unambiguously about
#' one topic and no other, which are naturally short and pointed. Ties break
#' toward the shorter unit, then alphabetically, so the selection is fully
#' deterministic.
#'
#' @param object A fitted [topics()] model.
#' @param text Character vector of candidate units, for example the `text`
#'   column of [segment()].
#' @param model A loaded [sbert_model][load_model()], a pinned model
#'   name, or `NULL` for the default model; ignored when `embeddings` are
#'   supplied.
#' @param embeddings Optional numeric matrix with one row per unit.
#' @param n Number of units returned per topic.
#' @param rank `"margin"` (default) or `"distance"` (raw closeness to the
#'   topic centroid).
#' @param batch_size Batch size passed to [encode()] when `model` is
#'   used.
#' @return A base data frame with columns `topic`, `rank`, `text`,
#'   `distance` (to the unit's own centroid), and `margin`, ordered by topic
#'   and rank. Topics to which no unit is assigned contribute no rows.
#' @export
#' @examples
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' fitted <- topics(text, 2, embeddings = embeddings)
#' representatives(
#'   fitted,
#'   c("kittens pounce", "bond yields", "pets and prices"),
#'   embeddings = rbind(c(1, 0.1), c(0.1, 1), c(0.7, 0.7)),
#'   n = 1
#' )
representatives <- function(
  object,
  text = NULL,
  model = NULL,
  embeddings = NULL,
  n = 3L,
  rank = c("margin", "distance"),
  batch_size = 32L
) {
  rank <- match.arg(rank)
  stopifnot(inherits(object, "sbert_topic_model"))
  # With no text supplied, rank the documents the model was fitted on. Their
  # embeddings are already stored, so nothing is re-encoded.
  if (is.null(text)) {
    text <- object$documents$text
    if (is.null(embeddings)) {
      embeddings <- object$embeddings
      if (is.null(embeddings)) {
        stop(
          "representatives: this model kept no embeddings. Refit with ",
          "topics(..., keep_embeddings = TRUE), or supply 'text'.",
          call. = FALSE
        )
      }
    }
  }
  stopifnot(
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    is.numeric(n),
    length(n) == 1L,
    is.finite(n),
    n >= 1,
    n == as.integer(n),
    is.numeric(batch_size),
    length(batch_size) == 1L,
    is.finite(batch_size),
    batch_size >= 1,
    batch_size == as.integer(batch_size)
  )

  embedding_matrix <- resolve_new_embeddings(
    text,
    model,
    embeddings,
    batch_size,
    n_expected = length(text)
  )
  assignment <- assign_topic_embeddings(embedding_matrix, object$centers)
  # pmax() would strip the dim attribute; clamp by index assignment instead.
  distance_matrix <- 1 - assignment$topic_similarity
  distance_matrix[distance_matrix < 0] <- 0
  own_distance <- assignment$distance
  second_distance <- vapply(
    seq_len(nrow(distance_matrix)),
    function(unit) {
      others <- distance_matrix[unit, -assignment$topic[[unit]]]
      min(others)
    },
    numeric(1)
  )
  margin <- second_distance - own_distance

  candidates <- data.frame(
    topic = assignment$topic,
    text = unname(text),
    distance = own_distance,
    margin = margin,
    stringsAsFactors = FALSE
  )
  selected <- lapply(
    sort(unique(candidates$topic)),
    function(topic_id) {
      topic_rows <- candidates[candidates$topic == topic_id, , drop = FALSE]
      ordering <- if (rank == "margin") {
        order(-topic_rows$margin, nchar(topic_rows$text), topic_rows$text)
      } else {
        order(topic_rows$distance, nchar(topic_rows$text), topic_rows$text)
      }
      chosen <- utils::head(topic_rows[ordering, , drop = FALSE], as.integer(n))
      chosen$rank <- seq_len(nrow(chosen))
      chosen
    }
  )
  result <- do.call(rbind, selected)
  rownames(result) <- NULL
  result[, c("topic", "rank", "text", "distance", "margin")]
}
