# Incremental ADD-ON: a sentence-level topic-profile layer over the existing
# deterministic k-means model (k-means is NOT replaced). Every abstract is split
# into sentences (validated segment_text), each content sentence is assigned to
# the nearest of the 29 k-means centroids, and each document's topic profile is
# the histogram of its sentences' topics -- a parameter-free mixed membership.
#
# Boilerplate handling: sentences whose best centroid cosine is in the bottom
# 20% (generic / template sentences that align with no topic) are dropped, so
# they do not pollute the profile or the incoherent topics.
devtools::load_all(quiet = TRUE)
source("analysis/segment_text.R")

model <- readRDS("outputs/mcse_gold_topics/mcse_gold_topic_model.rds")
sbert_model <- load_model("/private/tmp/sbert-package-download-test", threads = 2L)
labels <- model$topics$label
n_topics <- length(labels)
centroids <- model$centers / sqrt(rowSums(model$centers^2))
abstracts <- model$documents$text

# ---- split every abstract into content sentences ---------------------------
sentence_lists <- lapply(abstracts, function(text) {
  segs <- segment_text(text, "sentence")
  segs[lengths(strsplit(segs, "\\s+")) >= 6L]
})
document_of <- rep(seq_along(sentence_lists), lengths(sentence_lists))
sentences <- unlist(sentence_lists, use.names = FALSE)
cat(sprintf("abstracts: %d   content sentences: %d   mean per abstract: %.1f\n",
            length(abstracts), length(sentences), length(sentences) / length(abstracts)))

# ---- embed sentences and assign to nearest k-means centroid ----------------
embeddings <- encode(sentences, sbert_model, batch_size = 128L, normalize = TRUE)
similarity <- embeddings %*% t(centroids)        # sentences x 29
sentence_topic <- max.col(similarity, ties.method = "first")
best_cosine <- similarity[cbind(seq_len(nrow(similarity)), sentence_topic)]

# drop boilerplate: sentences that align with no topic (bottom 20% by best cosine)
threshold <- stats::quantile(best_cosine, 0.20, names = FALSE)
keep <- best_cosine >= threshold
cat(sprintf("kept %d of %d sentences after boilerplate filter (cosine >= %.3f)\n",
            sum(keep), length(keep), threshold))

# ---- aggregate to per-document topic profiles ------------------------------
profile_counts <- as.matrix(table(
  factor(document_of[keep], levels = seq_along(abstracts)),
  factor(sentence_topic[keep], levels = seq_len(n_topics))
))
colnames(profile_counts) <- labels
sentences_per_doc <- rowSums(profile_counts)
profile <- profile_counts / pmax(sentences_per_doc, 1L)

# document-level summary: hard topic vs dominant sentence-topic + top-3
top3 <- t(apply(profile, 1L, function(row) order(-row)[1:3]))
dominant <- labels[top3[, 1L]]
entropy <- apply(profile, 1L, function(p) {
  p <- p[p > 0]; if (length(p) == 0L) 0 else -sum(p * log(p)) / log(n_topics)
})
document_profiles <- data.frame(
  document_id = model$documents$document_id,
  hard_topic = labels[model$documents$topic],
  n_sentences = sentences_per_doc,
  dominant = dominant,
  dominant_share = round(profile[cbind(seq_len(nrow(profile)), top3[, 1L])], 3),
  second = labels[top3[, 2L]],
  second_share = round(profile[cbind(seq_len(nrow(profile)), top3[, 2L])], 3),
  third = labels[top3[, 3L]],
  third_share = round(profile[cbind(seq_len(nrow(profile)), top3[, 3L])], 3),
  topic_entropy = round(entropy, 3),
  stringsAsFactors = FALSE
)

scored <- sentences_per_doc > 0L
cat(sprintf("\ndominant sentence-topic == k-means hard label: %.1f%% of abstracts\n",
            100 * mean(dominant[scored] == labels[model$documents$topic][scored])))
cat(sprintf("abstracts spanning >= 2 topics with >= 25%% each: %.1f%%\n",
            100 * mean(rowSums(profile >= 0.25) >= 2L)))
cat(sprintf("median document topic-entropy (0 = single topic, 1 = uniform): %.2f\n",
            median(entropy[scored])))

# topic sizes: by sentences (soft) vs by documents (hard)
topic_sentence_share <- colSums(profile_counts) / sum(profile_counts)
topic_sizes <- data.frame(
  topic = labels,
  hard_doc_share = round(model$topics$proportion, 4),
  sentence_share = round(topic_sentence_share, 4),
  n_sentences = colSums(profile_counts),
  stringsAsFactors = FALSE
)
topic_sizes <- topic_sizes[order(-topic_sizes$sentence_share), ]
cat("\n=== topic share: by documents (k-means) vs by sentences (top 10) ===\n")
print(utils::head(topic_sizes, 10L), row.names = FALSE)

# ---- STM/LDA-style tidy matrices -------------------------------------------
# gamma (per-document topic distribution): the sentence-topic histogram, tidy
# and non-zero only (each row: one document's share of one topic; sums to 1/doc).
nonzero <- which(profile > 0, arr.ind = TRUE)
gamma <- data.frame(
  document_id = model$documents$document_id[nonzero[, 1L]],
  topic = labels[nonzero[, 2L]],
  gamma = round(profile[nonzero], 4),
  stringsAsFactors = FALSE
)
gamma <- gamma[order(gamma$document_id, -gamma$gamma), ]

# NOTE: beta (p(word|topic)) is produced by analysis/mcse_beta.R as a real
# empirical multinomial -- NOT normalized c-TF-IDF, which is a discriminative
# FREX-style weight, not a probability.

# per-sentence assignment with its top-3 nearest topics (several topics/sentence)
sentence_top3 <- t(apply(similarity, 1L, function(row) order(-row)[1:3]))
sentence_assignments <- data.frame(
  document_id = model$documents$document_id[document_of],
  sentence = sentences,
  topic_1 = labels[sentence_top3[, 1L]],
  topic_2 = labels[sentence_top3[, 2L]],
  topic_3 = labels[sentence_top3[, 3L]],
  cosine_1 = round(similarity[cbind(seq_len(nrow(similarity)), sentence_top3[, 1L])], 3),
  kept = keep,
  stringsAsFactors = FALSE
)

dir.create("outputs/mcse_gold_topics", showWarnings = FALSE, recursive = TRUE)
write.csv(document_profiles, "outputs/mcse_gold_topics/sentence_topic_profiles.csv", row.names = FALSE)
write.csv(topic_sizes, "outputs/mcse_gold_topics/sentence_topic_sizes.csv", row.names = FALSE)
write.csv(gamma, "outputs/mcse_gold_topics/gamma_document_topic.csv", row.names = FALSE)
write.csv(sentence_assignments, "outputs/mcse_gold_topics/sentence_assignments.csv", row.names = FALSE)
saveRDS(profile, "outputs/mcse_gold_topics/sentence_profile_matrix.rds")
cat(sprintf("\ngamma rows: %d   beta rows: %d   sentence rows: %d\n",
            nrow(gamma), nrow(beta), nrow(sentence_assignments)))
cat("wrote gamma_document_topic.csv, beta_topic_term.csv, sentence_assignments.csv,\n")
cat("      sentence_topic_profiles.csv, sentence_topic_sizes.csv, sentence_profile_matrix.rds\n")
