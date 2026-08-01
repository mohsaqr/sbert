# =============================================================================
# segment_text() -- deterministic multi-level text segmentation (base R only)
#
# Purpose: split all-caps academic text (Scopus abstracts have no case cue) into
# units at a selectable granularity, for representative-unit selection.
#
# Method: the standard rule-based sentence-boundary-detection (SBD) pipeline
# (abbreviation gazetteer + numeric/parenthetical guards) plus shallow clause
# chunking at discourse connectives -- the same family of methods used by
# Punkt / Stanford / spaCy sentencizers, adapted to the no-case setting.
#
# Levels (each finer level adds separators):
#   sentence : . ? !                              (faithful sentences)
#   clause   : + ; : spaced-dash + subordinating  (crisp, on-point clauses)
#              hinges (WHICH, WHERE, IN TERMS OF, ...); enumerations stay whole
#   phrase   : + commas                           (maximal granularity)
#
# Segmentation is PURE. Optional `merge_below` re-joins sub-threshold fragments
# (an enumeration/short-shard guard) and is off by default so the core stays
# testable in isolation.
# =============================================================================

# Word-boundary-anchored so "ST." (Saint) never matches the end of "COST.".
.seg_abbreviations <- c(
  "E.G.", "I.E.", "ET AL.", "ETC.", "VS.", "CF.", "FIG.", "FIGS.", "EQ.",
  "EQS.", "NO.", "NOS.", "VOL.", "PP.", "DR.", "PROF.", "ST.", "REF.", "REFS.",
  "U.S.", "U.K.", "PH.D.", "M.SC.", "B.SC.", "APPROX."
)

# Subordinating hinges that reliably begin a new clause and are NOT list items.
# Bare "AND"/"OR" are intentionally excluded: they mostly join enumerations,
# which we keep intact at clause level. Multi-word hinges are matched first.
.seg_hinges <- c(
  "IN TERMS OF", "IN ORDER TO", "SUCH THAT", "AS WELL AS", "RATHER THAN",
  "WHEREAS", "WHEREBY", "WHICH", "WHERE", "WHILE", "BECAUSE", "ALTHOUGH",
  "THOUGH"
)

# ASCII sentinels that cannot occur in the (upper-cased) source text.
.seg_boundary <- "@@B@@"
.seg_period <- "@@P@@"
.seg_comma <- "@@C@@"

seg_normalize <- function(text) {
  text <- toupper(enc2utf8(text))
  text <- gsub("[‘’‛′]", "'", text, perl = TRUE)
  text <- gsub("[“”]", '"', text, perl = TRUE)
  text <- gsub("[–—]", " - ", text, perl = TRUE)
  trimws(gsub("\\s+", " ", text, perl = TRUE))
}

# Mask periods/commas inside parentheses so "(E.G., PYTHON)" never splits.
seg_protect_parentheticals <- function(text) {
  matches <- gregexpr("\\([^()]*\\)", text, perl = TRUE)
  regmatches(text, matches) <- lapply(
    regmatches(text, matches),
    function(spans) {
      spans <- gsub(".", .seg_period, spans, fixed = TRUE)
      gsub(",", .seg_comma, spans, fixed = TRUE)
    }
  )
  text
}

# Mask periods inside abbreviations and decimals so they are not boundaries.
seg_protect_periods <- function(text, abbreviations = .seg_abbreviations) {
  for (abbreviation in abbreviations) {
    pattern <- paste0("\\b", gsub(".", "\\.", abbreviation, fixed = TRUE))
    replacement <- gsub(".", .seg_period, abbreviation, fixed = TRUE)
    text <- gsub(pattern, replacement, text, perl = TRUE)
  }
  gsub("([0-9])\\.([0-9])", paste0("\\1", .seg_period, "\\2"), text, perl = TRUE)
}

# Insert a boundary sentinel at every separator permitted by `level`.
seg_mark <- function(text, level) {
  text <- gsub("([.?!])\\s+", paste0("\\1", .seg_boundary), text, perl = TRUE)
  if (level %in% c("clause", "phrase")) {
    text <- gsub("([;:])\\s+", paste0("\\1", .seg_boundary), text, perl = TRUE)
    text <- gsub("\\s-\\s", paste0(" ", .seg_boundary), text, perl = TRUE)
    for (hinge in .seg_hinges) {
      text <- gsub(
        paste0("\\s+(", hinge, ")\\b"),
        paste0(" ", .seg_boundary, "\\1"),
        text,
        perl = TRUE
      )
    }
  }
  if (level == "phrase") {
    text <- gsub(",\\s+", paste0(",", .seg_boundary), text, perl = TRUE)
  }
  text
}

seg_restore <- function(parts) {
  parts <- gsub(.seg_period, ".", parts, fixed = TRUE)
  trimws(gsub(.seg_comma, ",", parts, fixed = TRUE))
}

# Re-join any segment with fewer than `min_words` words into the previous one
# (or the next, for a short leader). This is the enumeration / short-shard guard.
seg_merge_short <- function(segments, min_words) {
  if (min_words <= 1L || length(segments) <= 1L) {
    return(segments)
  }
  word_count <- function(segment) length(strsplit(segment, "\\s+")[[1L]])
  merged <- Reduce(
    function(accumulated, segment) {
      if (length(accumulated) > 0L && word_count(segment) < min_words) {
        accumulated[length(accumulated)] <-
          trimws(paste(accumulated[length(accumulated)], segment))
        accumulated
      } else {
        c(accumulated, segment)
      }
    },
    segments,
    character(0)
  )
  if (length(merged) > 1L && word_count(merged[[1L]]) < min_words) {
    merged <- c(trimws(paste(merged[[1L]], merged[[2L]])), merged[-(1:2)])
  }
  merged
}

#' Segment text at a chosen granularity.
#'
#' @param text A length-one character string.
#' @param level "sentence", "clause" (default), or "phrase".
#' @param merge_below Re-join segments shorter than this many words. 0 disables
#'   merging and returns the pure segmentation.
#' @return A character vector of segments (possibly empty for blank input).
segment_text <- function(text, level = c("clause", "sentence", "phrase"),
                         merge_below = 0L, abbreviations = .seg_abbreviations) {
  level <- match.arg(level)
  stopifnot(
    is.character(text), length(text) == 1L, !is.na(text),
    is.numeric(merge_below), length(merge_below) == 1L, merge_below >= 0,
    is.character(abbreviations)
  )
  normalized <- seg_normalize(text)
  if (!nzchar(normalized)) {
    return(character(0))
  }
  guarded <- seg_protect_periods(
    seg_protect_parentheticals(normalized), abbreviations
  )
  parts <- strsplit(seg_mark(guarded, level), .seg_boundary, fixed = TRUE)[[1L]]
  parts <- seg_restore(parts)
  parts <- parts[nzchar(parts) & grepl("[[:alnum:]]", parts)]
  seg_merge_short(parts, as.integer(merge_below))
}
