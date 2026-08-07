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
#' topic_palette(4)
topic_palette <- function(n) {
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

# One horizontal bar panel in the shared topic-plot style, drawn after the
# package's Levebee tutorial: axis-free bars that grow rightward, each annotated
# with its own value, under a left-aligned bold topic label.
draw_topic_bar_panel <- function(
  values,
  labels,
  title,
  color,
  value_format,
  cex_names = 0.75
) {
  headroom <- if (length(values) > 0 && max(values) > 0) max(values) * 1.18 else 1
  positions <- graphics::barplot(
    values,
    names.arg = labels,
    horiz = TRUE,
    las = 1,
    col = color,
    border = NA,
    xlim = c(0, headroom),
    cex.names = cex_names,
    axes = FALSE
  )
  graphics::text(
    x = values,
    y = positions,
    labels = sprintf(value_format, values),
    pos = 4,
    cex = 0.62,
    col = "grey30"
  )
  graphics::title(
    main = title,
    adj = 0,
    cex.main = 0.9,
    font.main = 2,
    line = 0.6
  )
  invisible(positions)
}

# Term-ranking metrics. `frequency` and `beta` share a ranking (beta is the
# within-topic count normalised), so both are read from the beta-sorted term
# table; only the plotted value and its number format differ.
.sbert_term_metrics <- list(
  score = list(
    sort_by = "score", value = "score", format = "%.3f",
    short = "TF-IDF", long = "class-based TF-IDF score"
  ),
  beta = list(
    sort_by = "beta", value = "beta", format = "%.3f",
    short = "beta", long = "generative probability (beta)"
  ),
  frequency = list(
    sort_by = "beta", value = "frequency", format = "%d",
    short = "count", long = "within-topic count"
  )
)

# Draw one topic's terms for one metric into the current panel.
draw_topic_term_panel <- function(rows, metric, title, color) {
  rows <- rows[order(rows$rank), , drop = FALSE]
  values <- rows[[metric$value]]
  display_order <- order(values)
  draw_topic_bar_panel(
    values[display_order],
    rows$term[display_order],
    title,
    color,
    metric$format
  )
}

plot_topic_terms <- function(x, colors, n_terms, by, topic_ids) {
  metrics <- .sbert_term_metrics[by]
  color_index <- match(topic_ids, x$topics$topic)
  # Precompute the term tables for each sort order we need (score and/or beta).
  sorts <- unique(vapply(metrics, `[[`, character(1), "sort_by"))
  sorted <- stats::setNames(
    lapply(sorts, function(s) terms(x, n = n_terms, sort_by = s)),
    sorts
  )
  rows_for <- function(metric, topic_id) {
    tbl <- sorted[[metric$sort_by]]
    tbl[tbl$topic == topic_id, , drop = FALSE]
  }

  if (length(by) == 1L) {
    # One panel per topic, arranged in a near-square grid.
    panels <- length(topic_ids)
    grid_columns <- ceiling(sqrt(panels))
    grid_rows <- ceiling(panels / grid_columns)
    old_par <- graphics::par(
      mfrow = c(grid_rows, grid_columns),
      mar = c(3, 7.5, 2.5, 1),
      oma = c(0, 0, 2, 0)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    metric <- metrics[[1L]]
    invisible(lapply(seq_along(topic_ids), function(i) {
      draw_topic_term_panel(
        rows_for(metric, topic_ids[[i]]),
        metric,
        x$topics$label[[color_index[i]]],
        colors[color_index[i]]
      )
    }))
    graphics::mtext(
      sprintf("Top terms by %s", metric$long),
      outer = TRUE, cex = 1.05, font = 2
    )
  } else {
    # One row per topic, one column per requested metric.
    old_par <- graphics::par(
      mfrow = c(length(topic_ids), length(by)),
      mar = c(3, 7.5, 2.5, 1),
      oma = c(0, 0, 2, 0)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    invisible(lapply(seq_along(topic_ids), function(i) {
      label <- x$topics$label[[color_index[i]]]
      lapply(metrics, function(metric) {
        draw_topic_term_panel(
          rows_for(metric, topic_ids[[i]]),
          metric,
          paste0(label, " - ", metric$short),
          colors[color_index[i]]
        )
      })
    }))
    graphics::mtext(
      sprintf(
        "Terms per topic. Columns: %s",
        paste(vapply(metrics, `[[`, character(1), "short"), collapse = ", ")
      ),
      outer = TRUE, cex = 1.05, font = 2
    )
  }
  invisible(x)
}

# Collapse internal whitespace and clip long representative documents so they
# fit as horizontal bar labels.
truncate_topic_text <- function(text, max_chars) {
  text <- gsub("[[:space:]]+", " ", trimws(text))
  ifelse(
    nchar(text) > max_chars,
    paste0(substr(text, 1L, max_chars - 3L), "..."),
    text
  )
}

# One topic's representatives as a ranked text list in the current panel:
# rank 1 (centroid-nearest) at the top, each line the document with its cosine
# similarity to the centroid in parentheses. `reps` is already ordered and
# capped by the caller.
draw_topic_rep_panel <- function(reps, title, color, max_chars) {
  proximity <- 1 - reps$distance
  graphics::plot.new()
  graphics::title(
    main = title,
    adj = 0,
    col.main = color,
    cex.main = 1,
    font.main = 2,
    line = 0.4
  )
  n <- nrow(reps)
  positions <- seq(0.92, 0.08, length.out = max(n, 1L))
  for (j in seq_len(n)) {
    graphics::text(
      0.02,
      positions[[j]],
      sprintf(
        '%d. "%s"  (%.2f)',
        j,
        truncate_topic_text(reps$text[[j]], max_chars),
        proximity[[j]]
      ),
      adj = c(0, 0.5),
      cex = 0.82
    )
  }
}

# Representatives to plot. The model stores only the number requested at fit
# time, so when a plot asks for more, recompute from the retained embeddings
# (identical ordering, just deeper). Falls back to the stored set -- with a
# warning -- when embeddings were not kept.
plot_representatives_table <- function(x, n) {
  stored <- x$representatives
  per_topic <- table(factor(stored$topic, levels = x$topics$topic))
  if (n <= min(per_topic)) {
    return(stored)
  }
  if (is.null(x$embeddings)) {
    warning(
      sprintf(
        paste(
          "Only %d representatives per topic were stored; refit with",
          "topics(..., n_representatives = %d, keep_embeddings = TRUE) to",
          "plot more."
        ),
        min(per_topic),
        n
      ),
      call. = FALSE
    )
    return(stored)
  }
  representatives(x, n = n, rank = "distance")
}

# Representative documents are sentences, not magnitudes, so each topic is
# drawn as a ranked text list rather than as bars.
plot_topic_representatives <- function(x, colors, n_representatives, topic_ids) {
  color_index <- match(topic_ids, x$topics$topic)
  panels <- length(topic_ids)
  grid_columns <- if (panels == 1L) 1L else 2L
  grid_rows <- ceiling(panels / grid_columns)
  max_chars <- as.integer(96 / grid_columns)
  rep_table <- plot_representatives_table(x, n_representatives)
  old_par <- graphics::par(
    mfrow = c(grid_rows, grid_columns),
    mar = c(0.5, 0.5, 2.4, 0.5),
    oma = c(0, 0, 2.4, 0)
  )
  on.exit(graphics::par(old_par), add = TRUE)

  invisible(lapply(
    seq_along(topic_ids),
    function(i) {
      topic_id <- topic_ids[[i]]
      reps <- rep_table[rep_table$topic == topic_id, , drop = FALSE]
      reps <- reps[order(reps$rank), , drop = FALSE]
      reps <- utils::head(reps, n_representatives)
      draw_topic_rep_panel(
        reps,
        x$topics$label[[color_index[i]]],
        colors[color_index[i]],
        max_chars
      )
    }
  ))
  graphics::mtext(
    "Representative documents, ranked by cosine similarity to the topic centroid",
    outer = TRUE,
    cex = 1.05,
    font = 2
  )
  invisible(x)
}

# A per-topic fit report: one row per topic with the three term views
# (within-topic count, TF-IDF, beta) followed by the representative documents.
plot_topic_fit <- function(x, colors, n_terms, n_representatives, topic_ids) {
  color_index <- match(topic_ids, x$topics$topic)
  metrics <- .sbert_term_metrics[c("frequency", "score", "beta")]
  n <- length(topic_ids)
  # Four cells per topic row; the document column is drawn wider.
  layout_matrix <- matrix(seq_len(n * 4L), nrow = n, ncol = 4L, byrow = TRUE)
  old_par <- graphics::par(oma = c(0, 0, 2.6, 0))
  on.exit(
    {
      graphics::layout(1L)
      graphics::par(old_par)
    },
    add = TRUE
  )
  graphics::layout(layout_matrix, widths = c(1, 1, 1, 1.9))

  sorted <- list(
    score = terms(x, n = n_terms, sort_by = "score"),
    beta = terms(x, n = n_terms, sort_by = "beta")
  )
  rep_table <- plot_representatives_table(x, n_representatives)

  invisible(lapply(
    seq_along(topic_ids),
    function(i) {
      topic_id <- topic_ids[[i]]
      color <- colors[color_index[i]]
      for (metric in metrics) {
        tbl <- sorted[[metric$sort_by]]
        rows <- tbl[tbl$topic == topic_id, , drop = FALSE]
        graphics::par(mar = c(2.6, 6.5, 2.4, 0.8))
        draw_topic_term_panel(rows, metric, metric$short, color)
      }
      reps <- rep_table[rep_table$topic == topic_id, , drop = FALSE]
      reps <- reps[order(reps$rank), , drop = FALSE]
      reps <- utils::head(reps, n_representatives)
      graphics::par(mar = c(2.6, 0.5, 2.4, 0.5))
      draw_topic_rep_panel(
        reps,
        x$topics$label[[color_index[i]]],
        color,
        48L
      )
    }
  ))
  graphics::mtext(
    paste(
      "Model fit per topic (rows). Term columns: count, TF-IDF, beta.",
      "Last column: representative documents."
    ),
    outer = TRUE,
    cex = 1.0,
    font = 2
  )
  invisible(x)
}

# One topic on its own figure: the three term views across the top, the
# representative documents spanning the full width beneath them. Used for
# type = "fit" with per_topic = TRUE.
plot_topic_fit_single <- function(x, colors, n_terms, n_representatives, topic_id) {
  color_index <- match(topic_id, x$topics$topic)
  color <- colors[color_index]
  label <- x$topics$label[[color_index]]
  metrics <- .sbert_term_metrics[c("frequency", "score", "beta")]
  # Top row: three term panels; bottom row: documents spanning all columns.
  layout_matrix <- rbind(c(1L, 2L, 3L), c(4L, 4L, 4L))
  old_par <- graphics::par(oma = c(0, 0, 2.6, 0))
  on.exit(
    {
      graphics::layout(1L)
      graphics::par(old_par)
    },
    add = TRUE
  )
  graphics::layout(layout_matrix, heights = c(1, 1))

  for (metric in metrics) {
    tbl <- terms(x, n = n_terms, sort_by = metric$sort_by)
    rows <- tbl[tbl$topic == topic_id, , drop = FALSE]
    graphics::par(mar = c(2.6, 6.5, 2.4, 0.8))
    draw_topic_term_panel(rows, metric, metric$short, color)
  }

  rep_table <- plot_representatives_table(x, n_representatives)
  reps <- rep_table[rep_table$topic == topic_id, , drop = FALSE]
  reps <- reps[order(reps$rank), , drop = FALSE]
  reps <- utils::head(reps, n_representatives)
  graphics::par(mar = c(0.5, 0.5, 2.4, 0.5))
  draw_topic_rep_panel(reps, "representative documents", color, 120L)

  graphics::mtext(label, outer = TRUE, cex = 1.1, font = 2, col = color)
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
        "Refit with topics(..., keep_embeddings = TRUE)."
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
#' Draws one of five deterministic base-graphics views of a fitted topic
#' model.
#'
#' @param x An `sbert_topic_model` returned by [topics()].
#' @param type One of `"sizes"` (document count per topic), `"terms"` (top
#'   terms per topic), `"representatives"` (a ranked text list of the
#'   centroid-nearest documents per topic), `"fit"` (a per-topic report with all
#'   three term views -- count, TF-IDF, beta -- followed by the representative
#'   documents, one row per topic), or `"map"` (a two-dimensional classical-MDS
#'   projection of the document embeddings, coloured by topic). The `"map"` view
#'   requires a model fitted with `keep_embeddings = TRUE`.
#' @param by For `type = "terms"`, one or more of `"score"` (class-based
#'   TF-IDF, the default and most distinctive terms), `"beta"` (the generative
#'   within-topic word probability), and `"frequency"` (the raw within-topic
#'   count). A single value draws one panel per topic; several values draw one
#'   row per topic with a column for each metric.
#' @param topics For `type` `"terms"`, `"representatives"`, or `"fit"`, an
#'   optional vector of topic numbers to restrict the plot to. Defaults to all
#'   topics.
#' @param per_topic For `type` `"terms"`, `"representatives"`, or `"fit"`, draw
#'   a separate figure for each topic instead of arranging all topics into one
#'   gridded figure. Most useful with a device or `knitr` chunk that keeps every
#'   figure (each topic then becomes its own image).
#' @param n_terms Number of top terms shown per panel when `type` is `"terms"`
#'   or `"fit"`.
#' @param n_representatives Number of representative documents shown per panel
#'   when `type` is `"representatives"` or `"fit"`. When more are requested than
#'   the model stored at fit time, they are recomputed from the retained
#'   embeddings; if the model was fitted with `keep_embeddings = FALSE`, the
#'   stored set is used and a warning is issued.
#' @param colors Optional vector of topic colours; defaults to [topic_palette()].
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
#' topics <- topics(text, 2, embeddings = embeddings, keep_embeddings = TRUE)
#' plot(topics, type = "sizes")
#' plot(topics, type = "terms")
#' plot(topics, type = "terms", by = "frequency")
#' plot(topics, type = "terms", by = c("frequency", "score", "beta"), topics = 1)
#' plot(topics, type = "representatives")
#' plot(topics, type = "fit")
#' plot(topics, type = "fit", per_topic = TRUE)
#' plot(topics, type = "map")
plot.sbert_topic_model <- function(
  x,
  type = c("sizes", "terms", "representatives", "fit", "map"),
  by = "score",
  topics = NULL,
  per_topic = FALSE,
  n_terms = 8L,
  n_representatives = 5L,
  colors = topic_palette(nrow(x$topics)),
  max_points = 1500L,
  ...
) {
  type <- match.arg(type)
  by <- match.arg(by, c("score", "beta", "frequency"), several.ok = TRUE)
  by <- unique(by)
  stopifnot(
    inherits(x, "sbert_topic_model"),
    is.null(topics) ||
      (is.numeric(topics) && length(topics) >= 1L && !anyNA(topics) &&
        all(topics %in% x$topics$topic)),
    is.logical(per_topic),
    length(per_topic) == 1L,
    !is.na(per_topic),
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
    is.character(colors),
    length(colors) >= nrow(x$topics),
    is.numeric(max_points),
    length(max_points) == 1L,
    is.finite(max_points),
    max_points >= nrow(x$topics)
  )

  # Selected topics, kept in the model's own topic order.
  topic_ids <- if (is.null(topics)) {
    x$topics$topic
  } else {
    x$topics$topic[x$topics$topic %in% topics]
  }

  draw <- function(ids) {
    switch(
      type,
      sizes = plot_topic_sizes(x, colors),
      terms = plot_topic_terms(x, colors, as.integer(n_terms), by, ids),
      representatives = plot_topic_representatives(
        x, colors, as.integer(n_representatives), ids
      ),
      fit = plot_topic_fit(
        x, colors, as.integer(n_terms), as.integer(n_representatives), ids
      ),
      map = plot_topic_map(x, colors, as.integer(max_points))
    )
  }

  if (per_topic && type %in% c("terms", "representatives", "fit")) {
    # One standalone figure per topic. For "fit", stack the documents beneath
    # the three term panels instead of using a fourth column.
    for (topic_id in topic_ids) {
      if (type == "fit") {
        plot_topic_fit_single(
          x, colors, as.integer(n_terms), as.integer(n_representatives), topic_id
        )
      } else {
        draw(topic_id)
      }
    }
  } else {
    draw(topic_ids)
  }
  invisible(x)
}
