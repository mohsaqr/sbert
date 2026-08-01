# Deterministic rule-based text segmentation: an abbreviation gazetteer plus
# numeric and parenthetical guards for sentence-boundary detection, and shallow
# clause chunking at discourse connectives. Matching is case-insensitive so the
# same rules work on cased prose and on all-caps abstract text.

# Word-boundary-anchored so "ST." (Saint) never matches the end of "COST.".
# "ETC." is deliberately absent: boundaries only fire at "period space word",
# and mid-sentence "etc." is written "ETC.," or "ETC.)" (never bare), so
# protecting it can only glue true sentence ends ("..., ETC. HOWEVER, ...").
.sbert_segment_abbreviations <- c(
  "E.G.", "I.E.", "ET AL.", "VS.", "CF.", "FIG.", "FIGS.", "EQ.",
  "EQS.", "NO.", "NOS.", "VOL.", "PP.", "DR.", "PROF.", "ST.", "REF.", "REFS.",
  "U.S.", "U.K.", "PH.D.", "M.SC.", "B.SC.", "APPROX."
)

# Subordinating hinges that reliably begin a new clause and are NOT list items.
# Bare "AND"/"OR" are intentionally excluded: they mostly join enumerations,
# which stay intact at clause level. Multi-word hinges are matched first.
.sbert_segment_hinges <- c(
  "IN TERMS OF", "IN ORDER TO", "SUCH THAT", "AS WELL AS", "RATHER THAN",
  "WHEREAS", "WHEREBY", "WHICH", "WHERE", "WHILE", "BECAUSE", "ALTHOUGH",
  "THOUGH"
)

# ASCII sentinels that cannot occur in the normalized source text.
.sbert_segment_boundary <- "@@B@@"
.sbert_segment_period <- "@@P@@"
.sbert_segment_comma <- "@@C@@"

#' Obtain the Built-in Abbreviation Gazetteer
#'
#' Returns the abbreviations whose periods are never treated as sentence
#' boundaries by [sbert_segment()]. Matching is case-insensitive and anchored
#' at a word boundary, so `"ST."` protects `"St. Petersburg"` but never the
#' end of `"cost."`.
#'
#' @return A sorted character vector of uppercase abbreviations, each ending
#'   in a period.
#' @export
#' @examples
#' head(sbert_abbreviations())
sbert_abbreviations <- function() {
  sort(unique(.sbert_segment_abbreviations))
}

segment_normalize <- function(text) {
  text <- enc2utf8(text)
  text <- gsub(
    "(*UTF)[\\x{2018}\\x{2019}\\x{201B}\\x{2032}]", "'", text,
    perl = TRUE
  )
  text <- gsub("(*UTF)[\\x{201C}\\x{201D}]", "\"", text, perl = TRUE)
  text <- gsub("(*UTF)[\\x{2013}\\x{2014}]", " - ", text, perl = TRUE)
  trimws(gsub("\\s+", " ", text, perl = TRUE))
}

# Mask periods/commas inside parentheses so "(e.g., Python)" never splits.
segment_protect_parentheticals <- function(text) {
  matches <- gregexpr("\\([^()]*\\)", text, perl = TRUE)
  regmatches(text, matches) <- lapply(
    regmatches(text, matches),
    function(spans) {
      spans <- gsub(".", .sbert_segment_period, spans, fixed = TRUE)
      gsub(",", .sbert_segment_comma, spans, fixed = TRUE)
    }
  )
  text
}

# Mask periods inside abbreviations and decimals so they are not boundaries.
# Masking edits the matched span in place, preserving the original case.
segment_protect_periods <- function(text, abbreviations) {
  patterns <- paste0(
    "(?i)\\b",
    gsub(".", "\\.", toupper(abbreviations), fixed = TRUE)
  )
  text <- Reduce(
    function(current, pattern) {
      matches <- gregexpr(pattern, current, perl = TRUE)
      regmatches(current, matches) <- lapply(
        regmatches(current, matches),
        function(spans) gsub(".", .sbert_segment_period, spans, fixed = TRUE)
      )
      current
    },
    patterns,
    text
  )
  gsub(
    "([0-9])\\.([0-9])",
    paste0("\\1", .sbert_segment_period, "\\2"),
    text,
    perl = TRUE
  )
}

# Insert a boundary sentinel at every separator permitted by `level`.
segment_mark <- function(text, level) {
  text <- gsub(
    "([.?!])\\s+",
    paste0("\\1", .sbert_segment_boundary),
    text,
    perl = TRUE
  )
  if (level %in% c("clause", "phrase")) {
    text <- gsub(
      "([;:])\\s+",
      paste0("\\1", .sbert_segment_boundary),
      text,
      perl = TRUE
    )
    text <- gsub(
      "\\s-\\s",
      paste0(" ", .sbert_segment_boundary),
      text,
      perl = TRUE
    )
    text <- Reduce(
      function(current, hinge) {
        gsub(
          paste0("(?i)\\s+(", hinge, ")\\b"),
          paste0(" ", .sbert_segment_boundary, "\\1"),
          current,
          perl = TRUE
        )
      },
      .sbert_segment_hinges,
      text
    )
  }
  if (level == "phrase") {
    text <- gsub(
      ",\\s+",
      paste0(",", .sbert_segment_boundary),
      text,
      perl = TRUE
    )
  }
  text
}

segment_restore <- function(parts) {
  parts <- gsub(.sbert_segment_period, ".", parts, fixed = TRUE)
  trimws(gsub(.sbert_segment_comma, ",", parts, fixed = TRUE))
}

# Re-join any segment with fewer than `min_words` words into the previous one
# (or the next, for a short leader): the enumeration / short-shard guard.
segment_merge_short <- function(segments, min_words) {
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

segment_document <- function(text, level, merge_below, abbreviations) {
  normalized <- segment_normalize(text)
  if (!nzchar(normalized)) {
    return(character(0))
  }
  guarded <- segment_protect_periods(
    segment_protect_parentheticals(normalized),
    abbreviations
  )
  parts <- strsplit(
    segment_mark(guarded, level),
    .sbert_segment_boundary,
    fixed = TRUE
  )[[1L]]
  parts <- segment_restore(parts)
  parts <- parts[nzchar(parts) & grepl("[[:alnum:]]", parts)]
  segment_merge_short(parts, as.integer(merge_below))
}

#' Segment Text into Sentences, Clauses, or Phrases
#'
#' Splits each document into units at a selectable granularity using
#' deterministic sentence-boundary rules: a word-boundary-anchored,
#' case-insensitive abbreviation gazetteer (see [sbert_abbreviations()]),
#' a decimal-number guard, and a parenthetical guard, plus shallow clause
#' chunking at discourse connectives. No model call is involved, so
#' segmentation works offline and is fully reproducible.
#'
#' Each finer level adds separators:
#' \describe{
#'   \item{`"sentence"`}{splits at `.`, `?`, and `!` only.}
#'   \item{`"clause"`}{additionally splits at `;`, `:`, spaced dashes, and
#'     subordinating hinges (for example "which", "where", "in terms of").
#'     Comma-separated enumerations stay whole. This is the default.}
#'   \item{`"phrase"`}{additionally splits at commas, for maximal granularity.}
#' }
#'
#' Before segmentation, each document is normalized: curly quotes become
#' straight quotes, en and em dashes become spaced hyphens, and runs of
#' whitespace collapse to single spaces. Letter case is preserved.
#'
#' @param text A character vector of documents. Names, when present, are
#'   carried into the `document_name` column.
#' @param level `"clause"` (default), `"sentence"`, or `"phrase"`.
#' @param merge_below Re-join segments shorter than this many words into their
#'   neighbor. `0` (default) disables merging and returns the pure
#'   segmentation.
#' @param abbreviations Character vector of abbreviations (each ending in a
#'   period) whose periods never end a sentence. Defaults to the built-in
#'   gazetteer from [sbert_abbreviations()]; matching is case-insensitive.
#' @return A base data frame with one row per segment and columns
#'   `document_id` (integer position in `text`), `document_name` (name of the
#'   input element, or `""`), `segment` (integer position within the
#'   document), and `text` (the segment). Blank documents contribute no rows.
#' @export
#' @examples
#' sbert_segment(
#'   "We propose a simulator which runs alongside the processor."
#' )
#'
#' sbert_segment(
#'   c(intro = "See Fig. 3 for details. The next part follows."),
#'   level = "sentence"
#' )
sbert_segment <- function(
  text,
  level = c("clause", "sentence", "phrase"),
  merge_below = 0L,
  abbreviations = sbert_abbreviations()
) {
  level <- match.arg(level)
  stopifnot(
    is.character(text),
    length(text) >= 1L,
    !anyNA(text),
    is.numeric(merge_below),
    length(merge_below) == 1L,
    is.finite(merge_below),
    merge_below >= 0,
    merge_below == as.integer(merge_below),
    is.character(abbreviations),
    !anyNA(abbreviations)
  )

  document_names <- names(text)
  if (is.null(document_names)) {
    document_names <- rep.int("", length(text))
  }
  segment_lists <- lapply(
    unname(text),
    segment_document,
    level = level,
    merge_below = merge_below,
    abbreviations = abbreviations
  )
  counts <- lengths(segment_lists)
  data.frame(
    document_id = rep.int(seq_along(text), counts),
    document_name = rep.int(unname(document_names), counts),
    segment = sequence(counts),
    text = as.character(unlist(segment_lists, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}
