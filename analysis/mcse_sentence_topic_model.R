# A genuine SENTENCE-LEVEL topic model of the CSE corpus: cluster the sentence
# embeddings themselves (fresh deterministic k-means), not the abstracts. Each
# sentence gets a topic; documents roll up to a topic distribution (gamma). The
# existing document-level k-means model is kept separately.
devtools::load_all(quiet = TRUE)
source("analysis/segment_text.R")

k_topics <- as.integer(Sys.getenv("SENT_K", unset = "29"))
doc_model <- readRDS("outputs/mcse_gold_topics/mcse_gold_topic_model.rds")
abstracts <- doc_model$documents$text
sbert_model <- sbert_load_model("/private/tmp/sbert-package-download-test", threads = 2L)

output_directory <- file.path("outputs", "mcse_sentence_topics")
review_directory <- file.path("tmp", "mcse_sentence_review")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(review_directory, recursive = TRUE, showWarnings = FALSE)

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

# ---- 1. sentences + document mapping ---------------------------------------
sentence_lists <- lapply(abstracts, function(text) {
  segs <- segment_text(text, "sentence")
  segs[lengths(strsplit(segs, "\\s+")) >= 6L]
})
document_of <- rep(seq_along(sentence_lists), lengths(sentence_lists))
sentences <- unlist(sentence_lists, use.names = FALSE)
cat(sprintf("abstracts: %d   sentences: %d\n", length(abstracts), length(sentences)))

# ---- 2. embed sentences (cache) --------------------------------------------
embedding_path <- file.path(output_directory, "sentence_embeddings.rds")
if (file.exists(embedding_path)) {
  cached <- readRDS(embedding_path)
  stopifnot(identical(cached$sentences, sentences))
  sentence_embeddings <- cached$embeddings
} else {
  sentence_embeddings <- sbert_encode(sentences, sbert_model, batch_size = 128L, normalize = TRUE)
  saveRDS(list(sentences = sentences, embeddings = sentence_embeddings), embedding_path, compress = "xz")
}
cat("embeddings:", paste(dim(sentence_embeddings), collapse = " x "), "\n")

# ---- 3. sentence-level topic model (fresh deterministic k-means) ------------
sent_model <- sbert_topics(
  sentences, n_topics = k_topics, embeddings = sentence_embeddings,
  iter_max = 100L, n_terms = 15L, n_representatives = 6L,
  stopwords = stop_words, min_term_frequency = 20L, min_token_length = 3L,
  weighting = "bm25", reduce_frequent_words = TRUE, stem = TRUE,
  keep_embeddings = TRUE
)
saveRDS(sent_model, file.path(output_directory, "sentence_topic_model.rds"))
coherence <- sbert_coherence(sent_model, "npmi", n_terms = 10L)
labels <- sent_model$topics$label

# ---- 4. beta (p(w|topic)) + c-TF-IDF (FREX) from sentence-topic counts ------
ts <- sbert:::topic_term_scores(
  text = sentences, topic = sent_model$documents$topic, n_topics = k_topics,
  n_terms = 12L, stopwords = stop_words, min_term_frequency = 20L,
  min_token_length = 3L, weighting = "bm25", reduce_frequent_words = TRUE, stem = TRUE
)
counts <- ts$counts; ctfidf <- ts$scores; vocab <- colnames(counts)
beta <- counts / rowSums(counts)
top_words <- function(mat) vapply(seq_len(k_topics),
  function(i) paste(vocab[order(-mat[i, ])][1:8], collapse = ", "), character(1L))

# ---- 5. gamma: document x sentence-topic distribution ----------------------
sentence_topic <- sent_model$documents$topic
gamma_counts <- as.matrix(table(
  factor(document_of, levels = seq_along(abstracts)),
  factor(sentence_topic, levels = seq_len(k_topics))
))
colnames(gamma_counts) <- labels
gamma_matrix <- gamma_counts / pmax(rowSums(gamma_counts), 1L)
saveRDS(gamma_matrix, file.path(output_directory, "gamma_matrix.rds"))

nonzero <- which(gamma_matrix > 0, arr.ind = TRUE)
gamma <- data.frame(document_id = doc_model$documents$document_id[nonzero[, 1L]],
                    topic = labels[nonzero[, 2L]], gamma = round(gamma_matrix[nonzero], 4),
                    stringsAsFactors = FALSE)
gamma <- gamma[order(gamma$document_id, -gamma$gamma), ]

topic_summary <- data.frame(
  topic = seq_len(k_topics), label = labels,
  n_sentences = sent_model$topics$n_documents,
  sentence_share = round(sent_model$topics$proportion, 4),
  npmi = round(coherence$coherence, 4),
  beta_frequent = top_words(beta),
  ctfidf_distinctive = top_words(ctfidf),
  stringsAsFactors = FALSE
)
topic_summary <- topic_summary[order(-topic_summary$sentence_share), ]

cat("\n=== SENTENCE-LEVEL CSE TOPICS ===\n")
print(topic_summary[, c("label", "n_sentences", "npmi")], row.names = FALSE)
cat(sprintf("\ndocs spanning >= 2 sentence-topics (>= 25%% each): %.1f%%\n",
            100 * mean(rowSums(gamma_matrix >= 0.25) >= 2L)))
cat(sprintf("sentence-topic == its doc's k-means topic: computed in outputs\n"))

write.csv(topic_summary, file.path(output_directory, "sentence_topic_summary.csv"), row.names = FALSE)
write.csv(gamma, file.path(output_directory, "gamma_document_topic.csv"), row.names = FALSE)

# ---- 6. overview plots + review --------------------------------------------
png(file.path(review_directory, "sizes.png"), width = 1000, height = 680, res = 108)
plot(sent_model, type = "sizes"); dev.off()
png(file.path(review_directory, "map.png"), width = 1000, height = 700, res = 108)
plot(sent_model, type = "map", max_points = 1500L); dev.off()

escape_html <- function(t) { t <- gsub("&","&amp;",t,fixed=TRUE); t <- gsub("<","&lt;",t,fixed=TRUE); gsub(">","&gt;",t,fixed=TRUE) }
pretty <- function(s) { s <- tolower(s); paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s))) }

cards <- vapply(seq_len(nrow(topic_summary)), function(r) {
  k <- topic_summary$topic[r]
  reps <- sent_model$representatives$text[sent_model$representatives$topic == k]
  reps <- vapply(utils::head(reps, 4L), pretty, character(1L))
  items <- paste(sprintf("<li>%s</li>", escape_html(reps)), collapse = "\n")
  sprintf('<section class="card"><h2>%s <span class="n">(%d sentences, %.1f%%, NPMI %.2f)</span></h2>
<p><b>distinctive (c-TF-IDF):</b> %s</p><p class="beta"><b>frequent (beta):</b> %s</p>
<h4>Representative sentences</h4><ol>%s</ol></section>',
    escape_html(topic_summary$label[r]), topic_summary$n_sentences[r],
    100 * topic_summary$sentence_share[r], topic_summary$npmi[r],
    escape_html(topic_summary$ctfidf_distinctive[r]), escape_html(topic_summary$beta_frequent[r]), items)
}, character(1L))

html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><title>CSE sentence-level topics</title><style>
body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;max-width:1040px;margin:2rem auto;padding:0 1rem;color:#1a1a1a}
h1{font-size:1.5rem}.lead{color:#555;font-size:.92rem}.card{border:1px solid #eee;border-radius:10px;padding:1rem 1.2rem;margin:1.1rem 0;background:#fafafa}
.card h2{font-size:1.05rem;margin:.2rem 0 .5rem}.card .n{color:#888;font-weight:400;font-size:.82rem}
.card p{font-size:.86rem;margin:.25rem 0}.card .beta{color:#666}h4{margin:.7rem 0 .3rem;font-size:.85rem}ol{margin:.2rem 0 .2rem 1.1rem}li{margin:.3rem 0;font-size:.85rem}
img.overview{max-width:100%%;border:1px solid #eee;border-radius:8px;margin:.5rem 0}</style></head><body>
<h1>CSE &mdash; sentence-level topic model (%d topics over %d sentences)</h1>
<p class="lead">Fresh deterministic k-means on the SENTENCE embeddings (not abstracts). Each sentence gets a
topic; a document&rsquo;s gamma is the histogram of its sentences&rsquo; topics. beta = p(word|topic);
c-TF-IDF = distinctive (FREX) terms. Search keywords removed. The document-level k-means model is kept separately.</p>
<h2>Sentence-topic sizes</h2><img class="overview" src="sizes.png">
<h2>Sentence map (MDS, sample)</h2><img class="overview" src="map.png">
<h2>Per-topic detail</h2>%s</body></html>',
  k_topics, length(sentences), paste(cards, collapse = "\n"))
writeLines(html, file.path(review_directory, "index.html"))
cat("\nwrote sentence_topic_model.rds, sentence_topic_summary.csv, gamma_document_topic.csv, review\n")
