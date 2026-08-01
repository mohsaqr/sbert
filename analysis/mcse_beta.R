# Proper topic-term matrices for the k-means model, with honest naming:
#   beta   = p(word | topic)  -- empirical multinomial (STM's "Highest-Prob"), a
#            real probability that sums to 1 within each topic.
#   ctfidf = class-based TF-IDF DISTINCTIVE weight (STM's "FREX" analogue) -- a
#            discriminative score, NOT a probability (can be negative under BM25).
# c-TF-IDF is for LABELS; beta is the generative word distribution.
devtools::load_all(quiet = TRUE)

model <- readRDS("outputs/mcse_gold_topics/mcse_gold_topic_model.rds")
labels <- model$topics$label
n_topics <- length(labels)

# same stop-word set the topic model used (search keywords + academic filler)
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
  text = model$documents$text, topic = model$documents$topic,
  n_topics = n_topics, n_terms = 12L, stopwords = stop_words,
  min_term_frequency = 10L, min_token_length = 3L,
  weighting = "bm25", reduce_frequent_words = TRUE, stem = TRUE
)
counts <- ts$counts                    # topic x vocab (raw within-topic counts)
ctfidf <- ts$scores                    # topic x vocab (c-TF-IDF / FREX weight)
vocab <- colnames(counts)
beta <- counts / rowSums(counts)       # p(word | topic): real multinomial

stopifnot(all(abs(rowSums(beta) - 1) < 1e-9))

# tidy beta (top 15 highest-probability words per topic) + tidy c-TF-IDF (FREX)
tidy_top <- function(mat, value_name, k_top = 15L) {
  do.call(rbind, lapply(seq_len(n_topics), function(k) {
    ord <- order(-mat[k, ])[seq_len(k_top)]
    out <- data.frame(topic = labels[k], term = vocab[ord],
                      round(mat[k, ord], 6), stringsAsFactors = FALSE)
    names(out)[3] <- value_name
    out
  }))
}
beta_tidy <- tidy_top(beta, "beta")
frex_tidy <- tidy_top(ctfidf, "ctfidf")

cat("=== beta (p(word|topic), HIGHEST-PROB) vs c-TF-IDF (FREX/distinctive) ===\n")
for (k in c(20L, 6L, 29L)) {  # gender, assessment, accessibility
  b <- vocab[order(-beta[k, ])][1:8]
  f <- vocab[order(-ctfidf[k, ])][1:8]
  cat(sprintf("\nTopic %-34s\n  beta   (frequent): %s\n  cTFIDF (distinct): %s\n",
              labels[k], paste(b, collapse = ", "), paste(f, collapse = ", ")))
}

dir.create("outputs/mcse_gold_topics", showWarnings = FALSE, recursive = TRUE)
write.csv(beta_tidy, "outputs/mcse_gold_topics/beta_topic_term.csv", row.names = FALSE)
write.csv(frex_tidy, "outputs/mcse_gold_topics/ctfidf_topic_term.csv", row.names = FALSE)
cat("\nwrote beta_topic_term.csv (real p(w|topic)) and ctfidf_topic_term.csv (FREX/distinctive)\n")
