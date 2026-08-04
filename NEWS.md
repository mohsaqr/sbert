# sbert 0.5.2

- README updated for the 0.5 API: data-frame input to `topics()`, the
  retained-model sweep with `fitted()`, and `terms()` retuning without a
  refit. Adds a pointer to the tutorial vignette.
- Repaired README prose damaged by the 0.5.0 rename, where the English words
  "membership" and "diversity" had been rewritten as function names.
- Corrected the model count in the README from thirteen to fourteen.
- Named the `n_topics` argument in the README's `reduce_topics()` example, and
  stopped shadowing `topic_hierarchy()` with a variable of the same name.

# sbert 0.5.1

- `R CMD check --as-cran` is clean: 0 errors, 0 warnings, 0 notes.
- Documented the `column` argument of `topics()`.
- Repaired a malformed `\eqn` macro in the `terms()` documentation.
- Qualified `graphics::par()` in the topic-hierarchy plot method.
- Declared `withr` in Suggests; the session tests use it.
- Excluded `.DS_Store`, `TODO.md` and knitr caches from the package build.

# sbert 0.5.0

## Breaking: the `sbert_` prefix is gone

Every exported function lost its prefix. The package has never been released,
so the old names were removed outright rather than deprecated:

`sbert_topics()` is now `topics()`, `sbert_encode()` `encode()`,
`sbert_segment()` `segment()`, `sbert_blend()` `blend()`,
`sbert_keywords()` `keywords()`, `sbert_dedupe()` `dedupe()`,
`sbert_coherence()` `coherence()`, `sbert_select_topics()`
`select_topics()`, `sbert_representatives()` `representatives()`,
`sbert_load_model()` `load_model()`, `sbert_models()` `models()`, and so on
for the cache and runtime verbs.

Six names are qualified because the bare form collides with a package this
audience is likely to have attached — `igraph`, `quanteda`, `purrr`:

- `sbert_membership()` -> `topic_membership()`
- `sbert_diversity()` -> `topic_diversity()`
- `sbert_hierarchy()` -> `topic_hierarchy()`
- `sbert_similarity()` -> `topic_similarity()`
- `sbert_reduce()` -> `reduce_topics()`
- `sbert_stopwords()` -> `stop_words()`

Two are qualified because the bare form would mask base R:
`sbert_gamma()` -> `topic_gamma()`, `sbert_palette()` -> `topic_palette()`.

Class names keep the prefix (`sbert_topic_model`, `sbert_model`), so S3
methods read `terms.sbert_topic_model()`.

## New: `terms()` retunes without refitting

Topic terms depend only on the fitted assignments and the document text, so
every term setting can now be changed without repeating the clustering or the
encoding:

```r
terms(fit, n = 12, weighting = "bm25", stem = TRUE)
terms(fit, n = 8, sort_by = "beta")     # words a topic uses most
terms(fit, n = 8)                        # words that distinguish it
```

`sort_by` chooses between the class-based weight (distinctive terms, the
default) and raw `p(term | topic)`. `n` is applied after ordering. The
returned frame carries `beta`, which absorbs the removed `sbert_beta()`.

## Verbs take your data frame and keep what they compute

- `topics()` accepts a data frame plus `column =` naming the text column.
  Remaining columns ride along into `$documents`. Rows whose text is missing,
  blank, or a `[NO ABSTRACT AVAILABLE]` placeholder are dropped once, and any
  supplied `embeddings` are subset to match.
- `keep_embeddings` now defaults to `TRUE`. `topic_membership()`,
  `reduce_topics()` and `plot(type = "map")` previously failed on a freshly
  fitted model and told the user to refit — which meant re-encoding.
- `label` is carried into `$documents`, `$terms` and `$representatives`, so
  joining it back by hand is never necessary.
- `representatives(fit)` defaults to the fitted corpus and its stored
  embeddings.

## `select_topics()` keeps every model it fits

The sweep no longer discards the models it built. `fitted(sweep, n_topics =)`
returns one without refitting, and the result gains `print`, `plot` and
`as.data.frame` methods while remaining a plain data frame with the same
columns.

## Other

- New tutorial vignette, "Topic Modeling: A Tutorial", running end to end on
  the bundled `feedback_translations` data with precomputed embeddings, so it
  builds without any download.
- Removed `sbert_beta()` (now the `beta` column of `terms()`).
- Fixed two documentation blocks that could not run: an example passed a cache
  directory where a model name belongs, and the README referenced an
  unassigned `model` object.

# sbert 0.4.0

## Static embeddings (instant-speed tier)

- New pinned model `potion-base-8M` (Model2Vec static embeddings, MIT):
  encoding is a token-lookup and mean in pure base R — no ONNX session —
  which makes it roughly two orders of magnitude faster than transformer
  models (10,000 sentences in under a second) at a modest quality cost.
  Loaded through the same verbs (`sbert_model_download()`,
  `sbert_load_model("potion-base-8M")`, `sbert_encode()`); the safetensors
  weight matrix is read with a minimal base-R reader and verified by
  SHA-256 like every other artifact. `sbert_models(detail = TRUE)` gains
  an `engine` column ("onnx" or "static"). Numerical parity with Python
  `model2vec`: max abs diff 3.3e-08.

## Seeded (guided) topics

- `sbert_topics()` gains `seeds`, `seed_embeddings`, and `fixed_seeds`:
  supply seed words or topic descriptions and they become the first
  cluster centroids. Seeded topics keep their position (topic i is seed i,
  never reordered by size) and take the seed's name as their label when
  `seeds` is named. Remaining topics up to `n_topics` are initialized
  deterministically away from the seeds. With `fixed_seeds = TRUE` the
  centroids never move — documents are zero-shot assigned to the nearest
  seed, and seeded topics may legitimately be empty.

## Analysis utilities

- New `sbert_keywords()`: embedding-based keyword and phrase extraction
  (the KeyBERT design) — each document's own unigrams and n-grams are
  embedded with the same model, ranked by cosine similarity to the
  document, and selected by maximal marginal relevance (`diversity`
  argument) so keywords are relevant without being redundant.
- `sbert_stopwords()` gains `add` and `remove`: exclude a corpus-wide
  vocabulary from topic terms and keyword candidates, or un-list built-in
  entries.
- New `sbert_select_topics()`: fits the model across candidate topic
  counts on one shared embedding matrix and returns coherence, diversity,
  and between-topic variance share per count — granularity chosen by
  numbers, not habit.
- New `sbert_hierarchy()`: deterministic agglomerative merge tree of the
  topic centroids (cosine distance) with a tidy merge table, labeled
  dendrogram plot, and readable print.
- New `sbert_reduce()`: cuts the hierarchy at a target count and rebuilds
  a full `sbert_topic_model` (merged assignments, recomputed centroids,
  terms, labels, sizes, representatives) — every downstream verb works on
  the reduced model unchanged.

## Context-blended segment embeddings

- New `sbert_blend()`: context-aware embeddings for segments — each segment
  keeps `alpha` of its context-orthogonal residual and inherits the rest of
  its direction from the parent document embedding, renormalized. Blending
  the residual (rather than the raw segment vector) avoids the
  near-collinearity of a segment with its own document. Accepts a
  `sbert_segment()` data frame plus the original documents (encoding both),
  or precomputed embedding matrices. Output drops into `sbert_topics()`,
  `sbert_representatives()`, and `predict()` unchanged. This is the
  package's supported way to "mix" granularities: per unit, never by pooling
  units of different levels into one training set.

## Representative selection and segmentation

- New `sbert_representatives()`: evidence selection by distinctiveness
  margin (distance to the second-best centroid minus distance to the own
  centroid) instead of raw closeness, which systematically favors long
  units. `rank = "distance"` restores the old behavior.
- `"ETC."` removed from the default abbreviation gazetteer: sentence
  boundaries only fire at period-space-word, and mid-sentence "etc." always
  carries trailing punctuation, so protecting it could only glue true
  sentence ends together.

## Multi-model registry

- Added six modern embedding models to the registry: `bge-small-en-v1.5` and
  `bge-base-en-v1.5` (BAAI, CLS pooling), `multilingual-e5-small` (100+
  languages, automatic `"query: "` prefix), `nomic-embed-text-v1.5` and
  `jina-embeddings-v2-small-en` (8,192-token context), and
  `mxbai-embed-large-v1` (1,024 dimensions). The encoder now supports CLS
  pooling (`sbert_pool(method = "cls")`) and per-model input prefixes
  (`sbert_encode(prefix = ...)` overrides the pinned default). Parity vs
  Python: at or below 2.9e-7 for the BGE/E5/nomic models, 8e-8 cross-runtime
  for jina; mxbai agrees to cosine 0.9996 with its float16-distributed
  reference (the pinned ONNX is the full-precision float32 artifact).

- Added a registry of pinned, SHA-256-verified models. Besides the default
  `all-MiniLM-L6-v2`, the package now supports six further models:
  `all-MiniLM-L12-v2` (deeper English MiniLM), `paraphrase-MiniLM-L3-v2`
  (the smallest and fastest, 69 MB), `multi-qa-MiniLM-L6-cos-v1` (semantic
  search, 512-token inputs), `paraphrase-multilingual-MiniLM-L12-v2`
  (384 dimensions, 50+ languages), `all-mpnet-base-v2` (768 dimensions,
  higher-quality English), and `paraphrase-multilingual-mpnet-base-v2`
  (768 dimensions, 50+ languages). `sbert_models()` lists them tidily. Every
  model remains locked to an immutable revision with byte-size and SHA-256
  verification, and every MiniLM addition was verified numerically identical
  to Python `SentenceTransformers` output (max difference below 3e-7).
- `sbert_model_download()` and `sbert_load_model()` now take the model name
  as their first argument (`sbert_load_model("all-mpnet-base-v2")`);
  `sbert_model_status()` and `sbert_model_remove()` gained a trailing `model`
  argument. Calls that passed `cache_dir` positionally to
  `sbert_model_download()`/`sbert_load_model()` must now name it.
- The encoder adapts to each model's input signature: MPNet- and
  XLM-RoBERTa-based graphs receive two ONNX inputs (no `token_type_ids`) and
  are padded with their own pad token; every model uses its published
  maximum sequence length.

## Generic loader

- Added `sbert_load_custom()`: loads any public Hugging Face repository that
  ships an ONNX encoder export and a `tokenizer.json`, with
  trust-on-first-use pinning (revision, byte sizes, and SHA-256 recorded in
  a local manifest on first download and verified on every later load).
  Pooling, maximum length, and the pad token are auto-detected from the
  repository's Sentence-Transformers configuration and tokenizer; the input
  signature and embedding dimension are read from the ONNX graph itself.
  Returns a regular `sbert_model`, so every downstream verb works
  unchanged. Requires the `jsonlite` package (Suggests). Unlike registry
  models, no parity certificate is implied.

## One-verb usage

- Every verb that takes a model now accepts a pinned model name or nothing
  at all: `sbert_encode(text)` uses the default `all-MiniLM-L6-v2`,
  `sbert_encode(text, model = "bge-small-en-v1.5")` switches by name, and
  `sbert_topics()`, `predict()`, and `sbert_gamma()` resolve models the
  same way. Loaded models are kept in a session cache and reused.
- Missing models are never downloaded silently: interactive sessions get a
  one-time yes/no prompt (size and license shown); scripts must opt in via
  `sbert_model_download()` or `options(sbert.download = TRUE)`.
- Attaching the package now prints a one-line orientation message with the
  model count, the default model, and the `sbert_models()` menu pointer.

## Corpus verbs

- Added `sbert_dedupe()`: collapses a corpus to its distinct non-blank texts
  (first-appearance order) with occurrence counts, so repeated templates
  cannot dominate the clustering geometry while frequencies remain available
  as weights.
- Added `sbert_topic_sizes()`: topic sizes on the distinct and weighted
  scales in one call; the gap between `proportion` and `weighted_share`
  measures how template-driven each topic is.

## Inferential layer

- Added `predict()` for `sbert_topic_model`: assigns unseen documents to the
  nearest fitted centroid under cosine distance, returning a tidy data frame
  with topics, labels, and distances.
- Added `sbert_membership()`: fuzzy-c-means soft topic probabilities on the
  stored centroids with a `sharpness` fuzzifier (default 1.15; the textbook
  value 2 degenerates toward uniform membership in high-dimensional
  embedding spaces). The within-document topic ranking is
  sharpness-invariant.
- Added `sbert_beta()`: the generative multinomial `p(term | topic)` from
  within-topic token counts with optional Laplace smoothing — the frequent-
  words view that complements (and must not be confused with) the
  discriminative class-based TF-IDF terms.
- Added `sbert_gamma()`: per-document topic distributions obtained by
  segmenting each document with `sbert_segment()` and assigning each segment
  to its nearest centroid — parameter-free mixed membership.

# sbert 0.3.0

- Added the `feedback_translations` dataset: 8,757 multilingual AI-generated
  mathematics feedback messages from the Levebee educational application
  paired with their English translations (8,005 distinct translations, template
  repetition preserved), giving every example and
  vignette a realistic offline corpus for the embedding, segmentation, and
  topic-modeling workflow.
- Added `sbert_segment()`, deterministic rule-based text segmentation at
  sentence, clause (default), or phrase granularity. Sentence boundaries are
  guarded by a word-boundary-anchored, case-insensitive abbreviation gazetteer
  (`sbert_abbreviations()`), a decimal-number guard, and a parenthetical
  guard; clause level adds splits at semicolons, colons, spaced dashes, and
  subordinating hinges while keeping comma enumerations whole. Returns a tidy
  one-row-per-segment data frame and preserves letter case. Promoted from the
  benchmarked analysis module (F1 0.999 on realistic abbreviation-rich
  abstracts versus 0.80-0.82 for ICU and NLTK Punkt references).

# sbert 0.2.0

- Fixed topic tokenization to normalize the curly apostrophe (U+2019) to the
  straight apostrophe, so contractions such as `it's` and `it’s` are one token.
- Added optional Porter stemming to `sbert_topics()` (`stem = TRUE`, using
  `SnowballC`) that collapses inflected forms while displaying the most
  frequent surface form of each stem, giving cleaner, deduplicated topic terms.
- Made NPMI coherence exact at the co-occurrence limits (terms that always or
  never co-occur now score 1 and -1 instead of a non-finite value).
- Added intrinsic topic-model evaluation: `sbert_coherence()` (UMass and NPMI)
  and `sbert_diversity()`, with a `summary()` method that reports cluster
  separation, coherence, and diversity alongside a tidy per-topic table.
- Added deterministic base-graphics visualizations via a `plot()` method for
  topic models (`type = "sizes"`, `"terms"`, `"map"`) and the `sbert_palette()`
  colour helper. The document map uses classical MDS on cosine distance.
- Added `weighting = "bm25"` and `reduce_frequent_words` options to
  `sbert_topics()`, implementing the BM25 and square-root class-based TF-IDF
  variants of Mendonca and Figueira (2025).
- Fixed the class-based TF-IDF average topic length to be real-valued rather
  than integer-truncated, matching the published formula (Eq. 1).

# sbert 0.1.0

- Added Python-free `all-MiniLM-L6-v2` inference through `tok` and `onnxr`.
- Added revision-pinned, SHA-256-verified model download and cache management.
- Added mask-aware mean pooling, L2 normalization, and cosine similarity.
- Added deterministic semantic topic modeling with representative documents,
  Unicode tokenization, and BERTopic-style class TF-IDF term summaries.
- Added offline unit tests and optional official-model numerical parity tests.
