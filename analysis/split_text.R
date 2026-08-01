# =============================================================================
# split_text() -- one dispatcher over the full splitting design space.
#
#   Job B (linguistic units, for representative extraction):
#     "sentence" | "clause" | "phrase"  -> validated segment_text() (F1 0.999)
#   Job A (chunks for embedding / retrieval):
#     "token_window" -> fixed word window with overlap
#     "recursive"    -> hierarchical separator splitting + greedy packing
#     "semantic"     -> cut at embedding-similarity troughs (needs an encoder)
#
# See analysis/SEGMENTATION_PLAN.md for the literature and benchmarks.
# =============================================================================
if (!exists("segment_text", mode = "function")) {
  .candidates <- c("segment_text.R", file.path("analysis", "segment_text.R"))
  .hit <- .candidates[file.exists(.candidates)]
  source(if (length(.hit) > 0L) .hit[[1L]] else "segment_text.R")
}

# ---- Job A: fixed token (word) window with overlap -------------------------
split_token_window <- function(text, size = 128L, overlap = 24L) {
  stopifnot(size > overlap, overlap >= 0L)
  words <- strsplit(trimws(text), "\\s+")[[1L]]
  n <- length(words)
  if (n == 0L) return(character(0))
  starts <- seq.int(1L, n, by = size - overlap)
  vapply(starts, function(s) {
    paste(words[s:min(s + size - 1L, n)], collapse = " ")
  }, character(1L))
}

# ---- Job A: recursive hierarchical separator splitting ---------------------
recursive_pieces <- function(text, separators, chunk_size) {
  if (nchar(text) <= chunk_size || length(separators) == 0L) return(text)
  sep <- separators[[1L]]
  parts <- if (nzchar(sep)) {
    strsplit(text, sep, fixed = TRUE)[[1L]]
  } else {
    strsplit(text, "")[[1L]]
  }
  parts <- parts[nzchar(parts)]
  unlist(lapply(parts, function(p) {
    if (nchar(p) > chunk_size) recursive_pieces(p, separators[-1L], chunk_size) else p
  }), use.names = FALSE)
}

split_recursive <- function(text, chunk_size = 800L, overlap = 100L,
                            separators = c("\n\n", "\n", ". ", " ")) {
  pieces <- recursive_pieces(trimws(text), separators, chunk_size)
  # greedily pack pieces into chunks of <= chunk_size, carrying a char overlap
  packed <- Reduce(function(acc, piece) {
    if (length(acc) == 0L) return(piece)
    last <- acc[[length(acc)]]
    if (nchar(last) + nchar(piece) + 1L <= chunk_size) {
      acc[[length(acc)]] <- paste(last, piece)
      acc
    } else {
      tail_overlap <- substr(last, max(1L, nchar(last) - overlap + 1L), nchar(last))
      c(acc, trimws(paste(tail_overlap, piece)))
    }
  }, pieces, character(0))
  packed[nzchar(packed)]
}

# ---- Job A/B: semantic splitting (embedding-similarity troughs) ------------
# `encode` must map a character vector -> a normalized embedding matrix.
split_semantic <- function(text, encode, level = "sentence", drop_quantile = 0.15) {
  units <- segment_text(text, level = level)
  if (length(units) < 3L) return(units)
  embeddings <- encode(units)
  adjacent <- vapply(seq_len(nrow(embeddings) - 1L), function(i) {
    sum(embeddings[i, ] * embeddings[i + 1L, ])
  }, numeric(1L))
  cut_after <- which(adjacent <= stats::quantile(adjacent, drop_quantile, names = FALSE))
  # a cut after unit i starts a new segment at unit i+1
  segment_id <- cumsum(c(1L, as.integer(seq_len(length(units) - 1L) %in% cut_after)))
  unname(vapply(split(units, segment_id), paste, character(1L), collapse = " "))
}

#' Split text by any method in the design space.
#' @param text length-one character.
#' @param method one of the six methods above.
#' @param ... method-specific parameters.
split_text <- function(text,
                       method = c("clause", "sentence", "phrase",
                                  "token_window", "recursive", "semantic"),
                       ...) {
  method <- match.arg(method)
  stopifnot(is.character(text), length(text) == 1L, !is.na(text))
  switch(method,
    sentence = segment_text(text, level = "sentence", ...),
    clause = segment_text(text, level = "clause", ...),
    phrase = segment_text(text, level = "phrase", ...),
    token_window = split_token_window(text, ...),
    recursive = split_recursive(text, ...),
    semantic = split_semantic(text, ...)
  )
}
