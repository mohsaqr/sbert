# sbert roadmap

Gaps against the 2025–2026 state of the art (BERTopic feature set, current
embedding tooling), filtered by the package's principles: deterministic,
base-R, Python-free, pinned models, tidy one-verb APIs. Newest assessment
2026-08-02.

## Done (this round)

- [x] `keywords()` — embedding-ranked keyword/phrase extraction with
      MMR diversification (KeyBERT design)
- [x] `stop_words(add = , remove = )` — corpus-vocabulary exclusion
- [x] `select_topics()` — coherence/diversity/separation sweep over
      candidate topic counts
- [x] `topic_hierarchy()` + `reduce_topics()` — topic dendrogram (cosine,
      deterministic) and tree-cut merging to fewer topics
- [x] `blend()` — context-blended segment embeddings (residual
      alpha-blend)
- [x] `representatives()` — margin-ranked evidence selection
- [x] **Static embeddings (Model2Vec)** — `potion-base-8M` pinned; token
      lookup + mean in base R, ~10k sentences/s, parity 3.3e-08 vs Python
- [x] **Seeded / guided topics** — `topics(seeds = , fixed_seeds = )`;
      seed-initialized or frozen centroids, seed-named labels

## Topic-model layer

- [ ] **Dynamic topics** — prevalence and per-period c-TF-IDF over a time
      variable, holding assignments fixed. Natural fit for the temporal
      research line.
- [ ] **Outlier awareness** — k-means forces every document into a topic;
      add a distance-threshold outlier class (deterministic alternative to
      HDBSCAN noise).
- [ ] **MMR term diversification + bigrams for topic terms** — reuse
      `mmr_select()` on term embeddings; extend `topic_term_scores()` to
      n-grams.
- [ ] **Embedding-based coherence** — mean pairwise term-embedding
      similarity as a third `coherence()` measure.
- [ ] **Topic stability** — label-invariant agreement (ARI) across
      perturbed refits (bootstrap resampling of documents).

## Embedding layer

- [ ] **More static models** — potion-base-32M (higher quality) and
      potion-multilingual-128M (101 languages) once demand shows; the
      loader already supports them.
- [ ] **Matryoshka truncation** — `dimensions =` on `encode()`
      (truncate + renormalize); `nomic-embed-text-v1.5` is MRL-trained.
- [ ] **Late chunking** — successor to `blend()`: pool unit token
      spans from the contextualized document pass. Requires exact
      char-to-token offset alignment, special-token exclusion, and
      truncation-safe windowing (NOT a splice of a single truncated pass).
- [ ] **Qwen3-Embedding-0.6B** — current top open model; needs last-token
      pooling mode. EmbeddingGemma stays excluded while gated.

## Housekeeping

- [x] R CMD check --as-cran clean (done at 0.5.1)
- [x] Fold the analysis tutorials into the package vignette
      (`topic-modeling` vignette ships)
- [ ] Decide whether `topics()`'s built-in representatives should use
      margin ranking (currently raw distance; `representatives()` is
      the margin path)

## Deliberately out of scope

- LLM-in-the-loop topic naming (TopicGPT-style) — breaks determinism and
  the no-API principle
- Sparse/hybrid retrieval, cross-encoder reranking — retrieval-stack
  features, not topic modeling
