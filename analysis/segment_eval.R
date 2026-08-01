# =============================================================================
# Rigorous, data-driven evaluation of sentence-boundary methods on real MCSE
# abstracts. Compares the rule-based segment_text() splitter against reference
# tokenizers (ICU via {tokenizers}, NLTK Punkt) and baselines/ablations.
#
# Test construction (known ground truth, no hand annotation needed):
#   1. Mine "clean single sentences" from MCSE abstracts: end in ".", no internal
#      terminal punctuation, no digits/parens/internal abbreviations -> each has
#      exactly ONE, unambiguous boundary (the final period).
#   2. Concatenate K singles per document -> the K-1 join points are the ONLY
#      true boundaries (known by construction).
#   3. Inject abbreviations/decimals (FIG. 3, E.G., U.S., 3.14, ET AL., NO. 5)
#      at interior positions of some singles -> known NON-boundaries that stress
#      each method's precision.
# Metric: boundary Precision / Recall / F1 (the NUPunkt protocol) + a breakdown
# into join-recall and abbreviation false-split rate, plus throughput.
# =============================================================================
source(file.path("analysis", "segment_text.R"))
suppressMessages(library(tokenizers))
set.seed(42)

count_tokens <- function(text) {
  vapply(text, function(x) {
    x <- seg_normalize(x)
    if (!nzchar(x)) 0L else length(strsplit(x, " ", fixed = TRUE)[[1L]])
  }, integer(1L), USE.NAMES = FALSE)
}
naive_split <- function(text) {
  trimws(unlist(strsplit(text, "(?<=[.?!])\\s+", perl = TRUE)))
}

# ---- 1. Clean-single pool from real abstracts ------------------------------
cache <- readRDS("outputs/mcse_abstract_30_topics/mcse_abstract_embeddings.rds")
candidates <- toupper(trimws(unlist(lapply(
  cache$unique_abstract[1:5000], naive_split
))))
body <- substr(candidates, 1L, nchar(candidates) - 1L)
word_count <- lengths(strsplit(candidates, "\\s+"))
clean <- candidates[
  grepl("\\.$", candidates) &                 # ends in a period
    !grepl("[.?!]\\s", body) &                 # no internal terminal punctuation
    !grepl("[0-9]", candidates) &              # no digits (avoids decimals)
    !grepl("[()\\[\\]]", candidates) &         # no brackets
    !grepl("[A-Z]\\.", body) &                 # no internal abbreviation period
    word_count >= 6 & word_count <= 30
]
clean <- unique(clean)
cat(sprintf("clean single-sentence pool: %d\n", length(clean)))

# ---- 1b. Corpus abbreviation mining (Kiss & Strunk / Punkt collocation) -----
# A short token whose stem almost always carries a trailing period (rarely
# appears bare) is an abbreviation. Learned from the corpus, no labels needed.
mine_abbreviations <- function(texts, min_with_period = 8L, max_bare_ratio = 0.3,
                               max_stem_len = 5L) {
  big <- paste(toupper(texts), collapse = " ")
  count_of <- function(pattern) {
    hits <- gregexpr(pattern, big, perl = TRUE)[[1L]]
    if (length(hits) == 1L && hits[1L] == -1L) 0L else length(hits)
  }
  with_dot <- unlist(regmatches(big, gregexpr("\\b[A-Z]{2,5}\\.", big, perl = TRUE)))
  stems <- unique(gsub(".", "", with_dot, fixed = TRUE))
  stems <- stems[nchar(stems) <= max_stem_len]
  keep <- vapply(stems, function(stem) {
    with_period <- count_of(paste0("\\b", stem, "\\."))
    bare_total <- count_of(paste0("\\b", stem, "\\b"))
    bare_only <- max(bare_total - with_period, 0L)
    with_period >= min_with_period && (bare_only / max(with_period, 1L)) <= max_bare_ratio
  }, logical(1L))
  paste0(stems[keep], ".")
}
mined_abbreviations <- mine_abbreviations(cache$unique_abstract[1:6000])
cat(sprintf("corpus-mined abbreviations (%d): %s\n", length(mined_abbreviations),
            paste(utils::head(sort(mined_abbreviations), 40L), collapse = " ")))

# ---- 2. Build evaluation documents with injected non-boundaries ------------
# Three conditions: no injection (sanity), abbreviations IN the gazetteer, and
# UNSEEN abbreviations (not in the gazetteer) to measure generalization.
n_docs <- 200L
per_doc <- 5L
pool <- sample(clean, n_docs * per_doc)
inject_in_gazetteer <- c("FIG. 3", "E.G., PYTHON", "U.S.", "NO. 5", "ET AL.", "I.E., THE MODEL")
inject_unseen <- c("PROC. ACM", "DEPT. OF SCIENCE", "SECT. TWO", "PARA. THREE",
                   "CHAP. FOUR", "TABL. ONE", "COL. SMITH", "ILL. USA")

build_docs <- function(injections, inject_prob = 0.6) {
  lapply(seq_len(n_docs), function(d) {
    idx <- ((d - 1L) * per_doc + 1L):(d * per_doc)
    pieces <- vapply(pool[idx], function(sentence) {
      if (length(injections) == 0L || stats::runif(1L) >= inject_prob) return(sentence)
      words <- strsplit(sentence, " ", fixed = TRUE)[[1L]]
      if (length(words) < 5L) return(sentence)
      position <- sample(2:(length(words) - 1L), 1L)
      paste(c(words[1:position], sample(injections, 1L),
              words[(position + 1L):length(words)]), collapse = " ")
    }, character(1L))
    pieces_norm <- vapply(pieces, seg_normalize, character(1L), USE.NAMES = FALSE)
    list(
      text = paste(pieces_norm, collapse = " "),
      n_tokens = sum(count_tokens(pieces_norm)),
      gold = utils::head(cumsum(count_tokens(pieces_norm)), -1L)
    )
  })
}
cat(sprintf("documents per condition: %d   true boundaries: %d\n", n_docs, n_docs * (per_doc - 1L)))

# ---- 3. Methods under test -------------------------------------------------
run_nltk <- function(texts) {
  infile <- tempfile(fileext = ".txt"); outfile <- tempfile(fileext = ".txt")
  writeLines(texts, infile)
  py <- sprintf('
import sys
from nltk.tokenize import sent_tokenize
with open("%s") as f, open("%s","w") as o:
    for line in f:
        line=line.rstrip("\\n")
        segs=sent_tokenize(line)
        o.write("\\u0001".join(segs)+"\\n")
', infile, outfile)
  status <- system2("python3", c("-c", shQuote(py)), stdout = FALSE, stderr = FALSE)
  if (status != 0L) return(NULL)
  lapply(strsplit(readLines(outfile), "", fixed = TRUE), identity)
}

boundaries_of <- function(segments, n_tokens) {
  segs <- segments[nzchar(trimws(segments))]
  tc <- count_tokens(segs)
  if (sum(tc) != n_tokens) return(NULL)          # token stream mismatch -> invalid
  utils::head(cumsum(tc), -1L)
}

score_method <- function(splitter, docs, nltk_out = NULL) {
  tp <- 0L; fp <- 0L; fn <- 0L; invalid <- 0L
  join_tp <- 0L; join_fn <- 0L; abbrev_fp <- 0L
  for (i in seq_along(docs)) {
    segs <- if (is.null(nltk_out)) splitter(docs[[i]]$text) else nltk_out[[i]]
    pred <- boundaries_of(segs, docs[[i]]$n_tokens)
    gold <- docs[[i]]$gold
    if (is.null(pred)) { invalid <- invalid + 1L; fn <- fn + length(gold); next }
    tp <- tp + sum(pred %in% gold)
    fp <- fp + sum(!pred %in% gold)
    fn <- fn + sum(!gold %in% pred)
    join_tp <- join_tp + sum(gold %in% pred)
    join_fn <- join_fn + sum(!gold %in% pred)
    abbrev_fp <- abbrev_fp + sum(!pred %in% gold)   # every false split is an over-split
  }
  precision <- tp / max(tp + fp, 1L)
  recall <- tp / max(tp + fn, 1L)
  data.frame(
    tp = tp, fp = fp, fn = fn, invalid = invalid,
    precision = round(precision, 4),
    recall = round(recall, 4),
    f1 = round(2 * precision * recall / max(precision + recall, 1e-9), 4),
    join_recall = round(join_tp / max(join_tp + join_fn, 1L), 4),
    over_splits = fp,
    stringsAsFactors = FALSE
  )
}

methods <- list(
  `naive (.?! )`      = function(t) naive_split(t),
  `icu (tokenizers)`  = function(t) unlist(tokenize_sentences(t)),
  `sbd no-gazetteer`  = function(t) segment_text(t, "sentence", abbreviations = character()),
  `sbd full (ours)`   = function(t) segment_text(t, "sentence"),
  `sbd + mined`       = function(t) segment_text(
    t, "sentence", abbreviations = union(.seg_abbreviations, mined_abbreviations)
  )
)

conditions <- list(
  clean = character(0),
  `in-gazetteer` = inject_in_gazetteer,
  `unseen-abbrev` = inject_unseen
)

evaluate_condition <- function(cond_name, injections) {
  set.seed(42)                                   # same clean pieces across methods
  docs <- build_docs(injections)
  all_text <- vapply(docs, function(x) x$text, character(1L))
  chars <- sum(nchar(all_text))
  rows <- do.call(rbind, lapply(names(methods), function(nm) {
    timing <- system.time(scored <- score_method(methods[[nm]], docs))
    cbind(condition = cond_name, method = nm, scored,
          chars_per_sec = round(chars / timing[["elapsed"]]))
  }))
  nltk_out <- run_nltk(all_text)
  if (!is.null(nltk_out)) {
    scored <- score_method(NULL, docs, nltk_out = nltk_out)
    rows <- rbind(rows, cbind(condition = cond_name, method = "nltk punkt (ref)",
                              scored, chars_per_sec = NA))
  }
  rows
}

results <- do.call(rbind, lapply(names(conditions), function(cn) {
  cat(sprintf("evaluating condition: %s\n", cn))
  evaluate_condition(cn, conditions[[cn]])
}))
rownames(results) <- NULL

cat("\n============ BOUNDARY DETECTION ON MCSE ABSTRACTS (P/R/F1) ============\n")
for (cn in names(conditions)) {
  cat(sprintf("\n--- condition: %s ---\n", cn))
  sub <- results[results$condition == cn, ]
  print(sub[, c("method", "precision", "recall", "f1", "over_splits", "chars_per_sec")],
        row.names = FALSE)
}

dir.create("outputs/segmentation_eval", showWarnings = FALSE, recursive = TRUE)
write.csv(results, "outputs/segmentation_eval/boundary_metrics.csv", row.names = FALSE)
cat("\nwrote outputs/segmentation_eval/boundary_metrics.csv\n")
