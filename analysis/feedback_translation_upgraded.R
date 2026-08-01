# Applies the upgraded sbert 0.2.0 pipeline (topic model + intrinsic evaluation
# + deterministic visuals) to the real English feedback translations. Reuses the
# cached, revision-pinned MiniLM embeddings, so no model download or re-encoding
# is needed. All steps are deterministic.
devtools::load_all(quiet = TRUE)

source_csv <- "/Users/mohammedsaqr/Downloads/Bee2/feedback_translations (2).csv"
cache_path <- file.path(
  "outputs", "feedback_translation_topics",
  "feedback_translation_embeddings.rds"
)
output_directory <- file.path("outputs", "feedback_translation_topics")
review_directory <- file.path("tmp", "feedback_review")
dir.create(review_directory, recursive = TRUE, showWarnings = FALSE)
selected_topic_count <- 6L

stopifnot(file.exists(source_csv), file.exists(cache_path))

cache <- readRDS(cache_path)
stopifnot(
  identical(cache$source_path, source_csv),
  is.matrix(cache$unique_embeddings),
  identical(
    cache$unique_embeddings[
      cache$analysis_data$embedding_id[1L], 1L
    ],
    cache$unique_embeddings[cache$analysis_data$embedding_id[1L], 1L]
  )
)

analysis_data <- cache$analysis_data
analysis_embeddings <- cache$unique_embeddings[analysis_data$embedding_id, , drop = FALSE]
stopifnot(
  identical(nrow(analysis_embeddings), nrow(analysis_data)),
  isTRUE(all.equal(
    unname(rowSums(analysis_embeddings^2)),
    rep(1, nrow(analysis_embeddings)),
    tolerance = 1e-5
  ))
)

# ---- Topic model (BERTopic-tuned term weighting + stemming) ----------------
# Short educational feedback shares a huge generic vocabulary ("mean",
# "pictures", "ones"), so plain c-TF-IDF produces indistinct topics. BM25 with
# frequent-word reduction (the tuned setting in Mendonca & Figueira 2025,
# Table I) plus Porter stemming (which every cited paper applies) yields
# distinct, deduplicated term lists.
topic_model <- sbert_topics(
  analysis_data$translation_clean,
  n_topics = selected_topic_count,
  embeddings = analysis_embeddings,
  iter_max = 100L,
  n_terms = 15L,
  n_representatives = 5L,
  min_term_frequency = 5L,
  min_token_length = 2L,
  weighting = "bm25",
  reduce_frequent_words = TRUE,
  stem = TRUE,
  keep_embeddings = TRUE
)

# ---- Intrinsic evaluation (new in 0.2.0) -----------------------------------
coherence_umass <- sbert_coherence(topic_model, measure = "umass", n_terms = 10L)
coherence_npmi <- sbert_coherence(topic_model, measure = "npmi", n_terms = 10L)
diversity <- sbert_diversity(topic_model, n_terms = 10L)

topic_quality <- data.frame(
  topic = topic_model$topics$topic,
  label = topic_model$topics$label,
  n_documents = topic_model$topics$n_documents,
  proportion = round(topic_model$topics$proportion, 4),
  umass = round(coherence_umass$coherence, 4),
  npmi = round(coherence_npmi$coherence, 4),
  stringsAsFactors = FALSE
)

cat("\n================ FEEDBACK TRANSLATION TOPICS (sbert 0.2.0) ================\n")
cat(sprintf(
  "documents: %d   distinct: %d   topics: %d   diversity: %.3f\n\n",
  nrow(analysis_data),
  length(cache$unique_translation),
  selected_topic_count,
  diversity
))
print(topic_quality, row.names = FALSE)
cat("\n---- summary() report ----\n")
invisible(summary(topic_model, measure = "npmi", n_terms = 10L))

# ---- Tidy output tables ----------------------------------------------------
topic_terms <- topic_model$terms
topic_terms$label <- topic_model$topics$label[topic_terms$topic]
topic_terms <- topic_terms[
  c("topic", "label", "rank", "term", "score", "frequency")
]
names(topic_terms)[names(topic_terms) == "score"] <- "c_tfidf_score"

write.csv(
  topic_quality,
  file.path(output_directory, "topic_quality_v2.csv"),
  row.names = FALSE
)
write.csv(
  topic_terms,
  file.path(output_directory, "topic_terms_v2.csv"),
  row.names = FALSE
)
write.csv(
  topic_model$representatives,
  file.path(output_directory, "topic_representatives_v2.csv"),
  row.names = FALSE
)

# ---- Deterministic visuals -------------------------------------------------
png(file.path(review_directory, "sizes.png"), width = 1000, height = 560, res = 110)
plot(topic_model, type = "sizes")
dev.off()

png(file.path(review_directory, "terms.png"), width = 1100, height = 820, res = 110)
plot(topic_model, type = "terms", n_terms = 8L)
dev.off()

png(file.path(review_directory, "map.png"), width = 1000, height = 680, res = 110)
plot(topic_model, type = "map", max_points = 1500L)
dev.off()

summary_text <- paste(
  utils::capture.output(summary(topic_model, measure = "npmi", n_terms = 10L)),
  collapse = "\n"
)
terms_preview <- do.call(
  rbind,
  lapply(
    topic_model$topics$topic,
    function(topic_id) {
      terms <- topic_model$terms$term[
        topic_model$terms$topic == topic_id & topic_model$terms$rank <= 10L
      ]
      data.frame(
        topic = topic_id,
        label = topic_model$topics$label[topic_id],
        top_terms = paste(terms, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  )
)
terms_html_rows <- paste(
  sprintf(
    "<tr><td>%d</td><td><b>%s</b></td><td>%s</td></tr>",
    terms_preview$topic, terms_preview$label, terms_preview$top_terms
  ),
  collapse = "\n"
)

html <- sprintf('<!doctype html>
<html><head><meta charset="utf-8"><title>Feedback translation topics (sbert 0.2.0)</title>
<style>
body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;max-width:960px;margin:2rem auto;padding:0 1rem;color:#1a1a1a}
h1{font-size:1.5rem} h2{font-size:1.15rem;margin-top:2.2rem;border-bottom:1px solid #eee;padding-bottom:.3rem}
img{max-width:100%%;border:1px solid #eee;border-radius:6px}
pre{background:#f6f8fa;padding:1rem;border-radius:6px;overflow-x:auto;font-size:.85rem}
table{border-collapse:collapse;width:100%%;font-size:.9rem} td,th{border:1px solid #eee;padding:.4rem .6rem;text-align:left;vertical-align:top}
.note{color:#555;font-size:.9rem}
</style></head><body>
<h1>Feedback translation topics &mdash; sbert 0.2.0</h1>
<p class="note">%d English feedback translations (%d distinct) from
<code>feedback_translations (2).csv</code>, embedded with the pinned
all-MiniLM-L6-v2 model. Six deterministic semantic topics; corpus diversity %.3f.</p>

<h2>1. Topic sizes</h2>
<img src="sizes.png">

<h2>2. Top class-based TF-IDF terms</h2>
<img src="terms.png">
<table><tr><th>#</th><th>Label</th><th>Top 10 terms</th></tr>
%s
</table>

<h2>3. Document map (classical MDS on cosine distance, stratified sample)</h2>
<img src="map.png">

<h2>4. summary(model) &mdash; scientific report</h2>
<pre>%s</pre>
</body></html>',
  nrow(analysis_data),
  length(cache$unique_translation),
  diversity,
  terms_html_rows,
  summary_text
)
writeLines(html, file.path(review_directory, "index.html"))
cat("\nWrote", file.path(review_directory, "index.html"), "\n")
