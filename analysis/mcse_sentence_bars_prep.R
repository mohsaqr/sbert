# Prepares per-topic bar figures (actual word count = beta, and c-TF-IDF =
# distinctive) plus representative sentences for the sentence-level CSE model,
# so the knitted report just embeds them. Same layout as the earlier reviews.
devtools::load_all(quiet = TRUE)

model <- readRDS("outputs/mcse_sentence_topics/sentence_topic_model.rds")
labels <- model$topics$label
k_topics <- length(labels)

search_keywords <- c("computing","computer","computers","science","sciences",
 "education","educational","learn","learning","learner","learners","learned",
 "learns","learnt","teach","teaching","teacher","teachers","teaches","taught",
 "course","courses","coursework","curriculum","curricula","curricular",
 "curriculums","introductory","informatics","informatic")
academic_filler <- c("paper","papers","article","articles","study","studies",
 "research","present","presents","presented","result","results","work","works",
 "use","used","uses","using","based","provide","provides","provided","propose",
 "proposed","proposes","describe","describes","described","discuss","discusses",
 "discussed","show","shows","shown","include","includes","included","including",
 "concept","concepts","method","methods","methodology","also","however",
 "different","various")
domain_ubiquitous <- c("students","student","programming","program","programs")
stop_words <- unique(c(sbert_stopwords(), search_keywords, academic_filler, domain_ubiquitous))

ts <- sbert:::topic_term_scores(
  text = model$documents$text, topic = model$documents$topic, n_topics = k_topics,
  n_terms = 12L, stopwords = stop_words, min_term_frequency = 20L,
  min_token_length = 3L, weighting = "bm25", reduce_frequent_words = TRUE, stem = TRUE
)
counts <- ts$counts; scores <- ts$scores; vocab <- colnames(counts)
coherence <- sbert_coherence(model, "npmi", n_terms = 10L)
colors <- sbert_palette(k_topics)

fig_dir <- normalizePath(file.path("tmp", "mcse_sentence_bars"), mustWork = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

draw_bars <- function(term_labels, values, colour, title, xlab, file) {
  grDevices::png(file, width = 470, height = 330, res = 94)
  old <- graphics::par(mar = c(4, 9, 2.5, 1)); on.exit(graphics::par(old), add = TRUE)
  o <- order(values)
  graphics::barplot(values[o], names.arg = term_labels[o], horiz = TRUE, las = 1,
    col = colour, border = NA, main = title, xlab = xlab, cex.names = 0.85, cex.main = 0.95)
  grDevices::dev.off()
}
top_by <- function(values, weights, n = 10L) {
  present <- which(weights > 0)
  utils::head(present[order(-values[present], vocab[present])], n)
}
pretty <- function(s) { s <- tolower(s); paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s))) }

order_by_size <- order(-model$topics$n_documents)
report_data <- lapply(order_by_size, function(t) {
  raw_index <- top_by(counts[t, ], counts[t, ])
  tfidf_index <- top_by(scores[t, ], counts[t, ])
  raw_png <- file.path(fig_dir, sprintf("raw_%02d.png", t))
  tfidf_png <- file.path(fig_dir, sprintf("tfidf_%02d.png", t))
  draw_bars(vocab[raw_index], counts[t, raw_index], colors[t], "Actual word count", "documents-weighted count", raw_png)
  draw_bars(vocab[tfidf_index], scores[t, tfidf_index], colors[t], "Distinctive (c-TF-IDF)", "class-based TF-IDF score", tfidf_png)
  reps <- vapply(utils::head(model$representatives$text[model$representatives$topic == t], 4L), pretty, character(1L))
  list(topic = t, label = labels[t], n = model$topics$n_documents[t],
       proportion = model$topics$proportion[t], npmi = round(coherence$coherence[t], 3),
       raw = raw_png, tfidf = tfidf_png, reps = unname(reps))
})
saveRDS(report_data, "outputs/mcse_sentence_topics/bars_report_data.rds")
cat("wrote", length(report_data), "topic figure sets +",
    "outputs/mcse_sentence_topics/bars_report_data.rds\n")
