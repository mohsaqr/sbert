# Base-graphics visualizations for semantic topic models. Everything here is
# deterministic and dependency-light: the two-dimensional document map uses
# classical multidimensional scaling (stats::cmdscale) on cosine distances
# rather than a stochastic projection such as UMAP or t-SNE, so the same model
# always yields the same picture.

#' Qualitative Colour Palette for Topics
#'
#' A colour-blind-friendly qualitative palette used by the package plots.
#'
#' @param n Number of colours to return.
#' @return A character vector of `n` hex colours.
#' @export
#' @examples
#' sbert_palette(4)
sbert_palette <- function(n) {
  stopifnot(
    is.numeric(n),
    length(n) == 1L,
    is.finite(n),
    n >= 1,
    n == as.integer(n)
  )
  grDevices::hcl.colors(as.integer(n), palette = "Dark 3")
}

plot_topic_sizes <- function(x, colors) {
  sizes <- x$topics$n_documents
  labels <- x$topics$label
  order_index <- rev(seq_along(sizes))
  old_par <- graphics::par(mar = c(4.5, 12, 3, 2))
  on.exit(graphics::par(old_par), add = TRUE)
  midpoints <- graphics::barplot(
    sizes[order_index],
    names.arg = labels[order_index],
    horiz = TRUE,
    las = 1,
    col = colors[order_index],
    border = NA,
    xlab = "Documents",
    main = "Topic sizes",
    cex.names = 0.85
  )
  graphics::text(
    x = sizes[order_index],
    y = midpoints,
    labels = sizes[order_index],
    pos = 4,
    xpd = NA,
    cex = 0.8
  )
  invisible(x)
}

plot_topic_terms <- function(x, colors, n_terms) {
  topic_ids <- x$topics$topic
  panels <- length(topic_ids)
  grid_columns <- ceiling(sqrt(panels))
  grid_rows <- ceiling(panels / grid_columns)
  old_par <- graphics::par(
    mfrow = c(grid_rows, grid_columns),
    mar = c(3, 7.5, 2.5, 1),
    oma = c(0, 0, 2, 0)
  )
  on.exit(graphics::par(old_par), add = TRUE)

  invisible(lapply(
    seq_along(topic_ids),
    function(index) {
      topic_id <- topic_ids[[index]]
      topic_terms <- x$terms[x$terms$topic == topic_id, , drop = FALSE]
      topic_terms <- topic_terms[order(topic_terms$rank), , drop = FALSE]
      topic_terms <- utils::head(topic_terms, n_terms)
      display_order <- order(topic_terms$score)
      graphics::barplot(
        topic_terms$score[display_order],
        names.arg = topic_terms$term[display_order],
        horiz = TRUE,
        las = 1,
        col = colors[index],
        border = NA,
        main = x$topics$label[[index]],
        cex.names = 0.8,
        cex.main = 0.9
      )
    }
  ))
  graphics::mtext(
    "Top terms by class-based TF-IDF score",
    outer = TRUE,
    cex = 1.05,
    font = 2
  )
  invisible(x)
}

# Deterministic stratified thinning of document indices so the O(n^2) MDS map
# stays feasible on large corpora. Each topic keeps an evenly spaced subsample
# proportional to its size; no random number generator is used.
thin_map_documents <- function(topic, topic_ids, max_points) {
  n_documents <- length(topic)
  if (n_documents <= max_points) {
    return(seq_len(n_documents))
  }
  kept <- lapply(
    topic_ids,
    function(topic_id) {
      indices <- which(topic == topic_id)
      quota <- max(2L, round(max_points * length(indices) / n_documents))
      quota <- min(quota, length(indices))
      indices[unique(round(seq(1, length(indices), length.out = quota)))]
    }
  )
  sort(unlist(kept, use.names = FALSE))
}

plot_topic_map <- function(x, colors, max_points) {
  embeddings <- x$embeddings
  if (is.null(embeddings)) {
    stop(
      paste(
        "The document map needs stored embeddings.",
        "Refit with sbert_topics(..., keep_embeddings = TRUE)."
      ),
      call. = FALSE
    )
  }
  if (nrow(embeddings) < 3L) {
    stop("The document map needs at least three documents.", call. = FALSE)
  }

  topic <- x$documents$topic
  keep <- thin_map_documents(topic, x$topics$topic, max_points)
  if (length(keep) < nrow(embeddings)) {
    message(sprintf(
      "Document map: showing %d of %d documents (deterministic stratified sample).",
      length(keep),
      nrow(embeddings)
    ))
    embeddings <- embeddings[keep, , drop = FALSE]
    topic <- topic[keep]
  }

  # Cosine distance on unit-norm embeddings, projected to 2D with classical MDS.
  cosine_similarity <- embeddings %*% t(embeddings)
  distance_matrix <- 1 - cosine_similarity
  distance_matrix[distance_matrix < 0] <- 0
  coordinates <- stats::cmdscale(stats::as.dist(distance_matrix), k = 2L)

  old_par <- graphics::par(mar = c(4, 4, 3, 11))
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::plot(
    coordinates,
    col = colors[topic],
    pch = 19,
    xlab = "MDS dimension 1",
    ylab = "MDS dimension 2",
    main = "Document map (classical MDS on cosine distance)"
  )
  centroids <- t(vapply(
    x$topics$topic,
    function(topic_id) colMeans(coordinates[topic == topic_id, , drop = FALSE]),
    numeric(2)
  ))
  graphics::text(
    centroids,
    labels = x$topics$topic,
    font = 2,
    cex = 1.1
  )
  legend_labels <- sprintf(
    "%d. %s",
    x$topics$topic,
    ifelse(
      nchar(x$topics$label) > 22L,
      paste0(strtrim(x$topics$label, 19L), "..."),
      x$topics$label
    )
  )
  graphics::legend(
    "topright",
    legend = legend_labels,
    col = colors,
    pch = 19,
    bty = "n",
    cex = 0.75,
    xpd = NA,
    inset = c(-0.45, 0)
  )
  invisible(x)
}

#' Plot a Semantic Topic Model
#'
#' Draws one of three deterministic base-graphics views of a fitted topic
#' model.
#'
#' @param x An `sbert_topic_model` returned by [sbert_topics()].
#' @param type One of `"sizes"` (document count per topic), `"terms"` (top
#'   class-based TF-IDF terms per topic), or `"map"` (a two-dimensional
#'   classical-MDS projection of the document embeddings, coloured by topic).
#'   The `"map"` view requires a model fitted with `keep_embeddings = TRUE`.
#' @param n_terms Number of top terms shown per panel when `type = "terms"`.
#' @param colors Optional vector of topic colours; defaults to [sbert_palette()].
#' @param max_points Maximum documents drawn when `type = "map"`. Larger corpora
#'   are thinned to a deterministic stratified subsample so the classical-MDS
#'   projection stays tractable.
#' @param ... Unused; present for S3 compatibility.
#' @return Invisibly, `x`.
#' @export
#' @examplesIf interactive()
#' text <- c(
#'   "Cats chase mice", "Dogs chase balls",
#'   "Stocks and bonds trade", "Markets price shares"
#' )
#' embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
#' topics <- sbert_topics(text, 2, embeddings = embeddings, keep_embeddings = TRUE)
#' plot(topics, type = "sizes")
#' plot(topics, type = "terms")
#' plot(topics, type = "map")
plot.sbert_topic_model <- function(
  x,
  type = c("sizes", "terms", "map"),
  n_terms = 8L,
  colors = sbert_palette(nrow(x$topics)),
  max_points = 1500L,
  ...
) {
  type <- match.arg(type)
  stopifnot(
    inherits(x, "sbert_topic_model"),
    is.numeric(n_terms),
    length(n_terms) == 1L,
    is.finite(n_terms),
    n_terms >= 1,
    n_terms == as.integer(n_terms),
    is.character(colors),
    length(colors) >= nrow(x$topics),
    is.numeric(max_points),
    length(max_points) == 1L,
    is.finite(max_points),
    max_points >= nrow(x$topics)
  )

  switch(
    type,
    sizes = plot_topic_sizes(x, colors),
    terms = plot_topic_terms(x, colors, as.integer(n_terms)),
    map = plot_topic_map(x, colors, as.integer(max_points))
  )
}
