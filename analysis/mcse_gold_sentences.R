# Rebuilds the MCSE review cards with representative *sentences* chosen the
# Sentence-BERT way: split each topic's nearest abstracts into sentences, embed
# every sentence, and keep the ones whose embedding is closest to the topic
# centroid. Reuses the already-rendered term charts.
devtools::load_all(quiet = TRUE)

model <- readRDS("outputs/mcse_gold_topics/mcse_gold_topic_model.rds")
quality <- read.csv("outputs/mcse_gold_topics/topic_quality.csv", stringsAsFactors = FALSE)
review_directory <- file.path("tmp", "mcse_review")
final_k <- nrow(model$topics)
stopifnot(is.matrix(model$centers), nrow(model$centers) == final_k)

sbert_model <- load_model("/private/tmp/sbert-package-download-test", threads = 2L)

# Validated segmentation algorithm (analysis/segment_text.R): rule-based,
# gazetteer-protected, benchmarked at F1 0.999 on realistic MCSE abbreviations
# (see analysis/segment_eval.R). SEG_LEVEL picks the granularity.
source(file.path("analysis", "segment_text.R"))
segment_level <- Sys.getenv("SEG_LEVEL", unset = "clause")
stopifnot(segment_level %in% c("sentence", "clause", "phrase"))
min_words <- c(sentence = 5L, clause = 4L, phrase = 3L)[[segment_level]]

split_sentences <- function(text) {
  segments <- segment_text(text, level = segment_level)
  segments[lengths(strsplit(segments, "\\s+")) >= min_words]
}
prettify_sentence <- function(sentence) {
  # drop a trailing clause separator (";", ":", ",", dash) for display; keep
  # sentence-terminal punctuation
  sentence <- sub("[;:,—–[:space:]]+$", "", sentence)
  lowered <- tolower(sentence)
  paste0(toupper(substr(lowered, 1, 1)), substr(lowered, 2, nchar(lowered)))
}
escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}

# For a topic: clauses from its 200 nearest abstracts (a wide pool so the ranker
# can find short, sharp units), ranked by how DISTINCTIVE they are -- similarity
# to this topic's centroid minus the best similarity to any other centroid, with
# a gentle penalty for length so a crisp clause beats a long one of equal
# relevance. The sentence analogue of c-TF-IDF.
representative_sentences <- function(topic_id) {
  rows <- model$documents[model$documents$topic == topic_id, , drop = FALSE]
  rows <- rows[order(rows$distance), , drop = FALSE]
  rows <- rows[!duplicated(rows$text), , drop = FALSE]
  sentences <- unique(unlist(lapply(
    utils::head(rows$text, 200L), split_sentences
  ), use.names = FALSE))
  if (length(sentences) < 1L) {
    return(character(0))
  }
  embeddings <- encode(sentences, sbert_model, batch_size = 64L, normalize = TRUE)
  similarity <- topic_similarity(embeddings, model$centers)
  own <- similarity[, topic_id]
  other <- apply(similarity[, -topic_id, drop = FALSE], 1L, max)
  word_count <- lengths(strsplit(sentences, "\\s+"))
  # distinctiveness, minus a gentle length penalty (~0.004 per word beyond 12)
  score <- (own - other) - 0.004 * pmax(word_count - 12L, 0L)
  # Require the clause still belong to this topic before rewarding distinctiveness.
  eligible <- own >= stats::median(own)
  ranked <- order(-ifelse(eligible, score, -Inf))
  vapply(sentences[utils::head(ranked, 5L)], prettify_sentence, character(1))
}

cards <- vapply(seq_len(final_k), function(topic_id) {
  sentences <- representative_sentences(topic_id)
  items <- paste(sprintf("<li>%s</li>", escape_html(sentences)), collapse = "\n")
  sprintf(
    '<section class="card"><h2>Topic %d &mdash; %s <span class="n">(%d docs, %.1f%%, NPMI %.2f)</span></h2>
<div class="charts"><img src="raw_%02d.png"><img src="tfidf_%02d.png"></div>
<h4>Most distinctive %s-level segments (this topic&rsquo;s centroid vs. the nearest other topic)</h4><ol>%s</ol></section>',
    topic_id, escape_html(model$topics$label[topic_id]),
    model$topics$n_documents[topic_id], 100 * model$topics$proportion[topic_id],
    quality$npmi[topic_id], topic_id, topic_id, segment_level, items
  )
}, character(1))

html <- sprintf('<!doctype html><html><head><meta charset="utf-8">
<title>MCSE_gold topics (keywords removed)</title><style>
body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;max-width:1040px;margin:2rem auto;padding:0 1rem;color:#1a1a1a}
h1{font-size:1.5rem} .lead{color:#555;font-size:.92rem}
.card{border:1px solid #eee;border-radius:10px;padding:1rem 1.2rem;margin:1.3rem 0;background:#fafafa}
.card h2{font-size:1.1rem;margin:.2rem 0 .6rem} .card .n{color:#888;font-weight:400;font-size:.82rem}
.charts{display:flex;gap:.5rem;flex-wrap:wrap} .charts img{flex:1 1 360px;max-width:100%%;border:1px solid #eee;border-radius:6px;background:#fff}
h4{margin:.9rem 0 .3rem;font-size:.9rem;color:#333} ol{margin:.2rem 0 .2rem 1.1rem} li{margin:.3rem 0;font-size:.86rem}
img.overview{max-width:100%%;border:1px solid #eee;border-radius:8px;margin:.5rem 0}
</style></head><body>
<h1>MCSE_gold &mdash; %d abstracts, %d topics (search keywords removed)</h1>
<p class="lead">Sentence-BERT embeddings clustered into %d topics. Search terms removed from the
term charts. Representative sentences are chosen to be distinctive: each sentence of the nearest abstracts is
embedded and ranked by its similarity to this topic&rsquo;s centroid minus its similarity to the
nearest other centroid (the sentence analogue of c-TF-IDF). Segmentation is punctuation-based.</p>
<h2>Topic sizes</h2><img class="overview" src="sizes.png">
<h2>Document map (MDS, stratified sample)</h2><img class="overview" src="map.png">
<h2>Per-topic detail</h2>
%s
</body></html>',
  nrow(model$documents), final_k, final_k, paste(cards, collapse = "\n"))
writeLines(html, file.path(review_directory, "index.html"))
cat("Wrote", file.path(review_directory, "index.html"), "\n")
