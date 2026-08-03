# Per-topic breakdown for the feedback translations: for each of the six
# semantic clusters, show (1) the actual most frequent words, (2) the most
# distinctive words by class-based TF-IDF, and (3) the real sentences closest
# to the cluster centre. Reuses the cached embeddings; deterministic.
devtools::load_all(quiet = TRUE)

source_csv <- "/Users/mohammedsaqr/Downloads/Bee2/feedback_translations (2).csv"
cache <- readRDS(file.path(
  "outputs", "feedback_translation_topics",
  "feedback_translation_embeddings.rds"
))
stopifnot(identical(cache$source_path, source_csv))
review_directory <- file.path("tmp", "feedback_terms")
dir.create(review_directory, recursive = TRUE, showWarnings = FALSE)

n_topics <- 6L
n_show <- 10L
n_sentences <- 6L

ad <- cache$analysis_data
emb <- cache$unique_embeddings[ad$embedding_id, , drop = FALSE]

model <- topics(
  ad$translation_clean,
  n_topics = n_topics,
  embeddings = emb,
  n_terms = 15L,
  min_term_frequency = 5L,
  min_token_length = 2L,
  weighting = "bm25",
  reduce_frequent_words = TRUE,
  stem = TRUE,
  keep_embeddings = FALSE
)

# Full per-topic count and c-TF-IDF matrices from the identical tokenization.
term_scores <- sbert:::topic_term_scores(
  text = model$documents$text,
  topic = model$documents$topic,
  n_topics = n_topics,
  n_terms = n_show,
  stopwords = stop_words(),
  min_term_frequency = 5L,
  min_token_length = 2L,
  weighting = "bm25",
  reduce_frequent_words = TRUE,
  stem = TRUE
)
counts <- term_scores$counts
scores <- term_scores$scores
vocabulary <- colnames(counts)
colours <- topic_palette(n_topics)

top_terms <- function(values, weights, k) {
  present <- which(weights > 0)
  ranked <- present[order(-values[present], vocabulary[present])]
  utils::head(ranked, k)
}

draw_bars <- function(labels, values, colour, title, xlab) {
  old_par <- graphics::par(mar = c(4, 8.5, 2.5, 2))
  on.exit(graphics::par(old_par), add = TRUE)
  order_index <- order(values)
  graphics::barplot(
    values[order_index],
    names.arg = labels[order_index],
    horiz = TRUE, las = 1, col = colour, border = NA,
    main = title, xlab = xlab, cex.names = 0.9, cex.main = 1
  )
}

# Distinct sentences closest to each centroid = the real cluster exemplars.
representative_sentences <- function(topic_id) {
  rows <- model$documents[model$documents$topic == topic_id, , drop = FALSE]
  rows <- rows[order(rows$distance), , drop = FALSE]
  rows <- rows[!duplicated(rows$text), , drop = FALSE]
  utils::head(rows$text, n_sentences)
}

escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}

cards <- vapply(
  seq_len(n_topics),
  function(topic_id) {
    raw_index <- top_terms(counts[topic_id, ], counts[topic_id, ], n_show)
    tfidf_index <- top_terms(scores[topic_id, ], counts[topic_id, ], n_show)

    raw_png <- sprintf("raw_%d.png", topic_id)
    tfidf_png <- sprintf("tfidf_%d.png", topic_id)
    grDevices::png(file.path(review_directory, raw_png), width = 470, height = 340, res = 96)
    draw_bars(vocabulary[raw_index], counts[topic_id, raw_index],
      colours[topic_id], "Actual word count", "documents-weighted count")
    grDevices::dev.off()
    grDevices::png(file.path(review_directory, tfidf_png), width = 470, height = 340, res = 96)
    draw_bars(vocabulary[tfidf_index], scores[topic_id, tfidf_index],
      colours[topic_id], "Distinctive (c-TF-IDF)", "class-based TF-IDF score")
    grDevices::dev.off()

    sentences <- representative_sentences(topic_id)
    sentence_items <- paste(
      sprintf("<li>%s</li>", escape_html(sentences)),
      collapse = "\n"
    )
    sprintf(
      '<section class="card">
  <h2>Topic %d &mdash; %s <span class="n">(%d documents, %.1f%%)</span></h2>
  <div class="charts">
    <img src="%s"><img src="%s">
  </div>
  <h4>Real sentences used for classification (closest to the centre)</h4>
  <ol>%s</ol>
</section>',
      topic_id,
      escape_html(model$topics$label[topic_id]),
      model$topics$n_documents[topic_id],
      100 * model$topics$proportion[topic_id],
      raw_png, tfidf_png, sentence_items
    )
  },
  character(1)
)

html <- sprintf('<!doctype html>
<html><head><meta charset="utf-8"><title>Feedback topics: words and sentences</title>
<style>
body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;max-width:1000px;margin:2rem auto;padding:0 1rem;color:#1a1a1a}
h1{font-size:1.5rem} .lead{color:#555;font-size:.92rem}
.card{border:1px solid #eee;border-radius:10px;padding:1rem 1.2rem;margin:1.4rem 0;background:#fafafa}
.card h2{font-size:1.15rem;margin:.2rem 0 .6rem} .card .n{color:#888;font-weight:400;font-size:.85rem}
.charts{display:flex;gap:.5rem;flex-wrap:wrap} .charts img{flex:1 1 380px;max-width:100%%;border:1px solid #eee;border-radius:6px;background:#fff}
h4{margin:1rem 0 .3rem;font-size:.92rem;color:#333}
ol{margin:.2rem 0 .2rem 1.1rem;padding:0} li{margin:.25rem 0;font-size:.9rem}
</style></head><body>
<h1>Feedback translations &mdash; words vs. distinctiveness vs. real sentences</h1>
<p class="lead">%d sentences classified into %d semantic clusters (Sentence-BERT + k-means).
For each cluster: the <b>actual most frequent words</b>, the <b>distinctive words</b> (class-based
TF-IDF), and the <b>real sentences</b> nearest the cluster centre. The clusters come from the
embeddings; the two word charts only describe them.</p>
%s
</body></html>',
  nrow(ad), n_topics, paste(cards, collapse = "\n")
)
writeLines(html, file.path(review_directory, "index.html"))

# Also a tidy side-by-side term table.
term_table <- do.call(rbind, lapply(seq_len(n_topics), function(topic_id) {
  raw_index <- top_terms(counts[topic_id, ], counts[topic_id, ], n_show)
  tfidf_index <- top_terms(scores[topic_id, ], counts[topic_id, ], n_show)
  data.frame(
    topic = topic_id,
    label = model$topics$label[topic_id],
    rank = seq_len(n_show),
    top_by_count = vocabulary[raw_index],
    count = counts[topic_id, raw_index],
    top_by_tfidf = vocabulary[tfidf_index],
    tfidf = round(scores[topic_id, tfidf_index], 4),
    stringsAsFactors = FALSE
  )
}))
write.csv(
  term_table,
  file.path("outputs", "feedback_translation_topics", "topic_terms_count_vs_tfidf.csv"),
  row.names = FALSE
)
cat("Wrote", file.path(review_directory, "index.html"), "\n")
