# Topic model of the MCSE_gold corpus with the Scopus search keywords removed
# from the topic-term vocabulary. Reuses the cached, chunk-aware MiniLM
# abstract embeddings; the keyword removal affects only the class-based TF-IDF
# description, not the embeddings or clustering. Deterministic throughout.
devtools::load_all(quiet = TRUE)

embedding_cache <- readRDS(
  "outputs/mcse_abstract_30_topics/mcse_abstract_embeddings.rds"
)
stopifnot(grepl("MCSE_gold\\.RDS$", embedding_cache$source_path))
abstracts <- embedding_cache$unique_abstract
embeddings <- embedding_cache$abstract_embeddings
stopifnot(
  is.character(abstracts),
  is.matrix(embeddings),
  nrow(embeddings) == length(abstracts)
)

# Map each abstract to its source paper title (compact representative labels).
source_records <- readRDS(embedding_cache$source_path)
title_by_index <- source_records$TI[match(
  substr(toupper(trimws(abstracts)), 1, 90),
  substr(toupper(trimws(source_records$AB)), 1, 90)
)]
stopifnot(length(title_by_index) == length(abstracts))

output_directory <- file.path("outputs", "mcse_gold_topics")
review_directory <- file.path("tmp", "mcse_review")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(review_directory, recursive = TRUE, showWarnings = FALSE)

# ---- Search keywords removed from topic terms ------------------------------
# Derived from the Scopus query that defined the corpus (COMPUTING /
# COMPUTER SCIENCE / INFORMATICS x EDUCATION / LEARN* / TEACH* / COURSE* /
# CURRICUL* / INTRODUCTORY). Surface forms are listed because stop-word
# filtering runs before stemming.
search_keywords <- c(
  "computing", "computer", "computers",
  "science", "sciences",
  "education", "educational",
  "learn", "learning", "learner", "learners", "learned", "learns", "learnt",
  "teach", "teaching", "teacher", "teachers", "teaches", "taught",
  "course", "courses", "coursework",
  "curriculum", "curricula", "curricular", "curriculums",
  "introductory", "informatics", "informatic"
)
# ---- Non-specific academic/domain words removed from topic terms -----------
# Empirically identified as appearing across most of the 15 topics. The user
# chose to KEEP "approach"/"approaches", the "develop" family, and "experience".
academic_filler <- c(
  "paper", "papers", "article", "articles", "study", "studies", "research",
  "present", "presents", "presented", "result", "results",
  "work", "works", "use", "used", "uses", "using",
  "based", "provide", "provides", "provided", "propose", "proposed", "proposes",
  "describe", "describes", "described", "discuss", "discusses", "discussed",
  "show", "shows", "shown", "include", "includes", "included", "including",
  "concept", "concepts", "method", "methods", "methodology",
  "also", "however", "different", "various"
)
domain_ubiquitous <- c("students", "student", "programming", "program", "programs")
stop_words <- unique(c(
  sbert_stopwords(), search_keywords, academic_filler, domain_ubiquitous
))

fit <- function(k) {
  sbert_topics(
    abstracts,
    n_topics = k,
    embeddings = embeddings,
    iter_max = 100L,
    n_terms = 15L,
    n_representatives = 6L,
    stopwords = stop_words,
    min_term_frequency = 10L,
    min_token_length = 3L,
    weighting = "bm25",
    reduce_frequent_words = TRUE,
    stem = TRUE,
    keep_embeddings = TRUE
  )
}

# ---- Topic-count sweep (coherence + diversity) -----------------------------
run_sweep <- identical(Sys.getenv("MCSE_RUN_SWEEP", unset = "false"), "true")
if (run_sweep) {
  candidate_k <- c(10L, 12L, 15L, 20L, 25L)
  sweep <- do.call(rbind, lapply(candidate_k, function(k) {
    model <- fit(k)
    npmi <- sbert_coherence(model, "npmi", n_terms = 10L)
    data.frame(
      k = k,
      mean_npmi = round(attr(npmi, "mean_coherence"), 4),
      diversity = round(sbert_diversity(model, 10L), 4),
      min_size = min(model$topics$n_documents),
      max_size = max(model$topics$n_documents),
      stringsAsFactors = FALSE
    )
  }))
  cat("=== topic-count sweep (keywords removed) ===\n")
  print(sweep, row.names = FALSE)
  write.csv(sweep, file.path(output_directory, "topic_count_sweep.csv"), row.names = FALSE)
}

# Balance coherence against granularity: prefer the largest k whose diversity
# stays high and whose coherence is within 15% of the best observed.
final_k <- as.integer(Sys.getenv("MCSE_K", unset = "29"))
cat("\nselected k:", final_k, "\n")
model <- fit(final_k)
saveRDS(model, file.path(output_directory, "mcse_gold_topic_model.rds"))

coherence <- sbert_coherence(model, "npmi", n_terms = 10L)
topic_quality <- data.frame(
  topic = model$topics$topic,
  label = model$topics$label,
  n_documents = model$topics$n_documents,
  proportion = round(model$topics$proportion, 4),
  npmi = round(coherence$coherence, 4),
  stringsAsFactors = FALSE
)
cat("\n=== topics (k =", final_k, ") ===\n")
print(topic_quality, row.names = FALSE)
write.csv(topic_quality, file.path(output_directory, "topic_quality.csv"), row.names = FALSE)

# ---- Actual-count vs c-TF-IDF terms ----------------------------------------
term_scores <- sbert:::topic_term_scores(
  text = model$documents$text,
  topic = model$documents$topic,
  n_topics = final_k,
  n_terms = 12L,
  stopwords = stop_words,
  min_term_frequency = 10L,
  min_token_length = 3L,
  weighting = "bm25",
  reduce_frequent_words = TRUE,
  stem = TRUE
)
counts <- term_scores$counts
scores <- term_scores$scores
vocabulary <- colnames(counts)
colours <- sbert_palette(final_k)
n_show <- 10L

top_by <- function(values, present_weights, k) {
  present <- which(present_weights > 0)
  utils::head(present[order(-values[present], vocabulary[present])], k)
}

term_table <- do.call(rbind, lapply(seq_len(final_k), function(topic_id) {
  raw_index <- top_by(counts[topic_id, ], counts[topic_id, ], n_show)
  tfidf_index <- top_by(scores[topic_id, ], counts[topic_id, ], n_show)
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
write.csv(term_table, file.path(output_directory, "topic_terms_count_vs_tfidf.csv"), row.names = FALSE)

# ---- Visuals + per-topic breakdown -----------------------------------------
draw_bars <- function(labels, values, colour, title, xlab) {
  old_par <- graphics::par(mar = c(4, 9, 2.5, 2))
  on.exit(graphics::par(old_par), add = TRUE)
  order_index <- order(values)
  graphics::barplot(values[order_index], names.arg = labels[order_index],
    horiz = TRUE, las = 1, col = colour, border = NA,
    main = title, xlab = xlab, cex.names = 0.85, cex.main = 0.95)
}
escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}
prettify_title <- function(title) {
  if (is.na(title) || !nzchar(title)) return("(title unavailable)")
  tools::toTitleCase(tolower(title))
}
representative_titles <- function(topic_id) {
  rows <- model$documents[model$documents$topic == topic_id, , drop = FALSE]
  rows <- rows[order(rows$distance), , drop = FALSE]
  rows <- rows[!duplicated(rows$text), , drop = FALSE]
  ids <- utils::head(rows$document_id, 5L)
  vapply(title_by_index[ids], prettify_title, character(1))
}

png(file.path(review_directory, "sizes.png"), width = 1000, height = 620, res = 108)
plot(model, type = "sizes")
dev.off()
png(file.path(review_directory, "map.png"), width = 1000, height = 700, res = 108)
plot(model, type = "map", max_points = 1500L)
dev.off()

cards <- vapply(seq_len(final_k), function(topic_id) {
  raw_index <- top_by(counts[topic_id, ], counts[topic_id, ], n_show)
  tfidf_index <- top_by(scores[topic_id, ], counts[topic_id, ], n_show)
  raw_png <- sprintf("raw_%02d.png", topic_id)
  tfidf_png <- sprintf("tfidf_%02d.png", topic_id)
  grDevices::png(file.path(review_directory, raw_png), width = 460, height = 330, res = 94)
  draw_bars(vocabulary[raw_index], counts[topic_id, raw_index], colours[topic_id],
    "Actual word count", "documents-weighted count")
  grDevices::dev.off()
  grDevices::png(file.path(review_directory, tfidf_png), width = 460, height = 330, res = 94)
  draw_bars(vocabulary[tfidf_index], scores[topic_id, tfidf_index], colours[topic_id],
    "Distinctive (c-TF-IDF)", "class-based TF-IDF score")
  grDevices::dev.off()
  titles <- representative_titles(topic_id)
  items <- paste(sprintf("<li>%s</li>", escape_html(titles)), collapse = "\n")
  sprintf('<section class="card"><h2>Topic %d &mdash; %s <span class="n">(%d docs, %.1f%%, NPMI %.2f)</span></h2>
<div class="charts"><img src="%s"><img src="%s"></div>
<h4>Representative papers (closest to the cluster centre)</h4><ol>%s</ol></section>',
    topic_id, escape_html(model$topics$label[topic_id]), model$topics$n_documents[topic_id],
    100 * model$topics$proportion[topic_id], coherence$coherence[topic_id],
    raw_png, tfidf_png, items)
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
<p class="lead">Sentence-BERT embeddings (chunk-aware, all-MiniLM-L6-v2) clustered into %d semantic
topics. The Scopus search terms (computing / computer science / informatics x education / learn* /
teach* / course* / curricul* / introductory) are removed from the topic vocabulary, so the words
shown are what distinguishes each cluster beyond the shared search domain. Clusters come from the
embeddings; the word charts only describe them.</p>
<h2>Topic sizes</h2><img class="overview" src="sizes.png">
<h2>Document map (MDS, stratified sample)</h2><img class="overview" src="map.png">
<h2>Per-topic detail</h2>
%s
</body></html>',
  length(abstracts), final_k, final_k, paste(cards, collapse = "\n"))
writeLines(html, file.path(review_directory, "index.html"))
cat("\nWrote", file.path(review_directory, "index.html"), "\n")
