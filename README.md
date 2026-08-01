# sbert

`sbert` computes genuine Sentence-BERT embeddings in R without Python.
Thirteen models are supported, each pinned to an immutable revision and
verified by SHA-256 (`sbert_models()` lists them): the classic
`sentence-transformers` family (`all-MiniLM-L6-v2` — the default —
`all-MiniLM-L12-v2`, `paraphrase-MiniLM-L3-v2`, `multi-qa-MiniLM-L6-cos-v1`,
`all-mpnet-base-v2`, and the multilingual MiniLM/mpnet pair) plus modern
embedders: `bge-small-en-v1.5` and `bge-base-en-v1.5` (CLS pooling),
`multilingual-e5-small` (100+ languages), `nomic-embed-text-v1.5` and
`jina-embeddings-v2-small-en` (8,192-token context), and
`mxbai-embed-large-v1` (1,024 dimensions). Hugging Face-compatible tokenization
is provided by [`tok`](https://cran.r-project.org/package=tok), and native
model inference by [`onnxr`](https://cran.r-project.org/package=onnxr).
Embeddings are numerically identical to Python `SentenceTransformers` output
(verified to ~1e-7).

The package never downloads a model or native runtime during installation,
loading, examples, tests, or vignette building. Both downloads are explicit:

```r
install.packages("sbert")

library(sbert)
sbert_install_runtime()

sentences <- c(
  "A student is reading a research paper.",
  "A learner studies an academic article.",
  "The bicycle is parked beside the building."
)
embeddings <- sbert_encode(sentences)
sbert_similarity(embeddings)
```

`sbert_encode()` uses the default `all-MiniLM-L6-v2` and asks once before
its first download; pick any other pinned model by name
(`sbert_encode(sentences, model = "bge-small-en-v1.5")` — see
`sbert_models()` for the menu). Loaded models are reused for the whole
session. Explicit `sbert_model_download()` / `sbert_load_model()` remain
available for scripted installs and backend/thread control.

Semantic topic modeling uses the same native embeddings, deterministic
k-means, representative documents, and class-based TF-IDF terms:

```r
topic_model <- sbert_topics(
  sentences,
  n_topics = 2,
  model = model,
  n_terms = 8
)

topic_model$topics
topic_model$terms
topic_model$representatives
```

Fitted topic models support a full inferential layer — assignment of new
documents, soft membership, generative word probabilities, and mixed-topic
document distributions:

```r
predict(topic_model, new_sentences, model = model)  # nearest-centroid topics
sbert_membership(topic_model)                       # fuzzy topic probabilities
sbert_beta(topic_model)                             # p(term | topic) multinomial
sbert_gamma(topic_model, sentences, model = model)  # per-document topic mixture
```

Beyond the curated registry, `sbert_load_custom()` loads any public Hugging
Face repository with an ONNX encoder export, auto-detecting its
configuration and pinning it locally on first use ("trust on first use"):

```r
gte <- sbert_load_custom("thenlper/gte-small")
sbert_encode(sentences, gte)
```

Multilingual corpora use the same API with a multilingual model:

```r
sbert_model_download("paraphrase-multilingual-MiniLM-L12-v2")
multilingual <- sbert_load_model("paraphrase-multilingual-MiniLM-L12-v2")
embeddings <- sbert_encode(head(feedback_translations$feedback, 100), multilingual)
```

`n_topics` is deliberately explicit: the package does not silently guess a
topic count. You may also pass a precomputed embedding matrix through the
`embeddings` argument for reproducible offline analysis.

Every topic solution can be evaluated and visualized without any further model
call:

```r
sbert_coherence(topic_model, measure = "npmi")  # per-topic UMass / NPMI coherence
sbert_diversity(topic_model)                     # distinct-vocabulary proportion
summary(topic_model)                             # scientific report + quality table

plot(topic_model, type = "sizes")                # documents per topic
plot(topic_model, type = "terms")                # top class-based TF-IDF terms
plot(topic_model, type = "map")                  # classical-MDS document map
```

Term weighting supports the class-based TF-IDF, BM25 (`weighting = "bm25"`), and
square-root (`reduce_frequent_words = TRUE`) schemes of Mendonca and Figueira
(2025).

Documents can be split into sentences, clauses, or phrases before embedding,
entirely offline and deterministically:

```r
sbert_segment(
  "We had two goals: speed and clarity; both were met. See Fig. 3.",
  level = "clause"
)
```

`sbert_segment()` returns one row per segment with the source document index,
guarding abbreviations (`sbert_abbreviations()`), decimals, and parentheticals
so they never end a sentence. The clause level (the default) also splits at
subordinating hinges while keeping comma enumerations whole.

Segments can carry their document's context into the embedding:
`sbert_blend()` keeps `alpha` of each segment's context-orthogonal residual
and inherits the rest from the parent document vector, so a sentence that is
ambiguous in isolation embeds near its document's subject while keeping what
it alone says. The result drops into `sbert_topics()` and
`sbert_representatives()` unchanged:

```r
sentences <- sbert_segment(abstracts, level = "sentence")
embeddings <- sbert_blend(sentences, abstracts, alpha = 0.5)
```

The bundled `feedback_translations` dataset (8,987 multilingual AI-generated
mathematics feedback messages from the Levebee educational application, with
English translations) provides a realistic corpus for trying the full
workflow offline:

```r
head(feedback_translations)
```

Model downloads range from 69 MB (`paraphrase-MiniLM-L3-v2`) to 1.1 GB
(`paraphrase-multilingual-mpnet-base-v2`) and are stored under the
platform-specific path returned by `sbert_cache_dir()`. Every artifact is
verified by byte size and SHA-256 before it is used.

## Supported scope

- Models: thirteen pinned models (see `sbert_models()`), classic and modern,
  English and multilingual, 384 to 1,024 dimensions, 128 to 8,192 tokens,
  69 MB to 1.3 GB
- Pooling: attention-mask-aware mean pooling or CLS pooling, per model
- Prefixes: model-pinned input prefixes applied automatically (E5, Nomic)
- Custom models: `sbert_load_custom()` for any HF ONNX encoder repository,
  with trust-on-first-use local pinning
- Tokenization: each model's official `tokenizer.json`, truncated to the
  model's published maximum sequence length
- Pooling: attention-mask-aware mean pooling
- Normalization: row-wise L2 normalization by default
- Segmentation: deterministic sentence, clause, and phrase splitting with an
  abbreviation gazetteer and decimal/parenthetical guards
- Topic discovery: deterministic k-means with farthest-point initialization
- Topic descriptions: representative documents and BERTopic-style c-TF-IDF
- Topic inference: `predict()` for new documents, fuzzy soft membership,
  generative `beta`, and segment-based document-topic `gamma`
- Topic evaluation: intrinsic UMass and NPMI coherence, and topic diversity
- Topic visualization: deterministic base-graphics size, term, and MDS map views
- Backends: those exposed by `onnxr`, with CPU as the portable default

Arbitrary unpinned Hugging Face models and training/fine-tuning are
intentionally out of scope.
