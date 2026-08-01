# Incremental ADD-ON to the deterministic k-means topic model (does NOT replace
# it). Computes fuzzy-c-means soft memberships from the EXISTING k-means
# centroids, giving each abstract a topic distribution (the mixed-membership
# view) alongside its unchanged hard assignment. Temperature-free: uses the
# standard fuzzy-c-means formula with fuzzifier m = 2.
devtools::load_all(quiet = TRUE)

model <- readRDS("outputs/mcse_gold_topics/mcse_gold_topic_model.rds")
stopifnot(!is.null(model$embeddings))
labels <- model$topics$label
embeddings <- model$embeddings                     # 15308 x 384 (normalized)
centers <- model$centers                           # 29 x 384 (k-means centroids)

# Fuzzy-c-means membership from fixed centroids:
#   u_ik = 1 / sum_j (d_ik / d_jk)^(1/(m-1)),  d = squared Euclidean.
# NOTE: in 384-dim space pairwise distances concentrate, so the textbook m = 2
# collapses memberships to ~uniform (1/29). A sharpness parameter is therefore
# unavoidable in embedding space; the fuzzifier m -> 1 recovers hard k-means and
# m = 2 is degenerate. m = 1.15 gives interpretable proportions while the top-k
# RANKING (the nearest centroids) is identical for any m.
fuzzifier <- as.numeric(Sys.getenv("FCM_M", unset = "1.15"))
squared_distance <- outer(rowSums(embeddings^2), rowSums(centers^2), `+`) -
  2 * (embeddings %*% t(centers))
squared_distance[squared_distance < 1e-9] <- 1e-9
weight <- squared_distance^(-1 / (fuzzifier - 1))
membership <- weight / rowSums(weight)             # rows sum to 1
colnames(membership) <- labels
stopifnot(all(abs(rowSums(membership) - 1) < 1e-9))

# hard assignment is unchanged; verify the k-means topic is (almost always) the
# soft argmax too -- a sanity check that the add-on is consistent with k-means.
soft_argmax <- max.col(membership)
cat(sprintf("soft argmax agrees with k-means hard label: %.1f%% of abstracts\n",
            100 * mean(soft_argmax == model$documents$topic)))

top1 <- apply(membership, 1L, max)
cat(sprintf("median membership on the primary topic: %.2f\n", median(top1)))
cat(sprintf("abstracts that are genuinely multi-topic (>= 2 topics above 20%%): %.1f%%\n",
            100 * mean(rowSums(membership >= 0.20) >= 2L)))
cat(sprintf("abstracts strongly single-topic (primary >= 60%%): %.1f%%\n\n",
            100 * mean(top1 >= 0.60)))

# document-level output: hard topic + soft top-3 with proportions
top3 <- t(apply(membership, 1L, function(row) order(-row)[1:3]))
document_memberships <- data.frame(
  document_id = model$documents$document_id,
  hard_topic = labels[model$documents$topic],
  primary = labels[top3[, 1L]],
  primary_share = round(membership[cbind(seq_len(nrow(membership)), top3[, 1L])], 3),
  second = labels[top3[, 2L]],
  second_share = round(membership[cbind(seq_len(nrow(membership)), top3[, 2L])], 3),
  third = labels[top3[, 3L]],
  third_share = round(membership[cbind(seq_len(nrow(membership)), top3[, 3L])], 3),
  stringsAsFactors = FALSE
)

# topic-level: mean membership across the whole corpus (a soft topic "size"
# that, unlike hard counts, credits partial membership)
soft_size <- colMeans(membership)
topic_soft <- data.frame(
  topic = labels,
  hard_docs = model$topics$n_documents,
  hard_share = round(model$topics$proportion, 4),
  soft_share = round(soft_size, 4),
  stringsAsFactors = FALSE
)
topic_soft <- topic_soft[order(-topic_soft$soft_share), ]

cat("=== hard vs soft topic share (top 8 by soft share) ===\n")
print(utils::head(topic_soft, 8L), row.names = FALSE)

cat("\n=== example multi-topic abstracts (primary and second both substantial) ===\n")
gap <- top1 - apply(membership, 1L, function(row) sort(row, decreasing = TRUE)[2L])
mixed <- which(top1 < 0.55 & gap < 0.15)
set.seed(11)
for (i in utils::head(sample(mixed), 3L)) {
  ord <- order(-membership[i, ])[1:3]
  cat(sprintf("  hard: %s\n", labels[model$documents$topic[i]]))
  cat("  soft:", paste(sprintf("%s %.0f%%", labels[ord], 100 * membership[i, ord]), collapse = " | "), "\n")
  cat("  ", substr(tolower(model$documents$text[i]), 1L, 150L), "...\n\n", sep = "")
}

dir.create("outputs/mcse_gold_topics", showWarnings = FALSE, recursive = TRUE)
write.csv(document_memberships, "outputs/mcse_gold_topics/document_memberships.csv", row.names = FALSE)
write.csv(topic_soft, "outputs/mcse_gold_topics/topic_hard_vs_soft.csv", row.names = FALSE)
saveRDS(membership, "outputs/mcse_gold_topics/soft_membership_matrix.rds")
cat("\nwrote document_memberships.csv, topic_hard_vs_soft.csv, soft_membership_matrix.rds\n")
