# sbert

`sbert` computes genuine Sentence-BERT embeddings in R without Python.
Thirteen models are supported, each pinned to an immutable revision and
verified by SHA-256 (`models()` lists them): the classic
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
install_runtime()

sentences <- c(
  "A student is reading a research paper.",
  "A learner studies an academic article.",
  "The bicycle is parked beside the building."
)
embeddings <- encode(sentences)
topic_similarity(embeddings)
```

`encode()` uses the default `all-MiniLM-L6-v2` and asks once before
its first download; pick any other pinned model by name
(`encode(sentences, model = "bge-small-en-v1.5")` — see
`models()` for the menu). Loaded models are reused for the whole
session. Explicit `model_download()` / `load_model()` remain
available for scripted installs and backend/thread control.

Semantic topic modeling uses the same native embeddings, deterministic
k-means, representative documents, and class-based TF-IDF terms:

```r
topic_model <- topics(
  sentences,
  n_topics = 2,
  n_terms = 8
)

topic_model$topics
topic_model$terms
topic_model$representatives
```

Fitted topic models support a full inferential layer — assignment of new
documents, soft topic_membership, generative word probabilities, and mixed-topic
document distributions:

```r
predict(topic_model, new_sentences)   # nearest-centroid topics
topic_membership(topic_model)         # fuzzy topic probabilities
terms(topic_model)                    # ranked terms + p(term | topic)
topic_gamma(topic_model, sentences)   # per-document topic mixture
```

Beyond the curated registry, `load_custom()` loads any public Hugging
Face repository with an ONNX encoder export, auto-detecting its
configuration and pinning it locally on first use ("trust on first use"):

```r
gte <- load_custom("thenlper/gte-small")
encode(sentences, gte)
```

Multilingual corpora use the same API with a multilingual model:

```r
model_download("paraphrase-multilingual-MiniLM-L12-v2")
multilingual <- load_model("paraphrase-multilingual-MiniLM-L12-v2")
embeddings <- encode(head(feedback_translations$feedback, 100), multilingual)
```

`n_topics` is deliberately explicit: the package does not silently guess a
topic count. You may also pass a precomputed embedding matrix through the
`embeddings` argument for reproducible offline analysis.

Every topic solution can be evaluated and visualized without any further model
call:

```r
coherence(topic_model, measure = "npmi")  # per-topic UMass / NPMI coherence
topic_diversity(topic_model)                     # distinct-vocabulary proportion
summary(topic_model)                             # scientific report + quality table

plot(topic_model, type = "sizes")                # documents per topic
plot(topic_model, type = "terms")                # top class-based TF-IDF terms
plot(topic_model, type = "map")                  # classical-MDS document map
```

Term weighting supports the class-based TF-IDF, BM25 (`weighting = "bm25"`), and
square-root (`reduce_frequent_words = TRUE`) schemes of Mendonca and Figueira
(2025).

For very large corpora there is an instant-speed tier: `potion-base-8M`
is a static (Model2Vec) model whose encoding is a token lookup and mean in
pure base R — around 10,000 sentences per second, no ONNX involved, with
the same pinning, verification, and verbs as every other model:

```r
model_download("potion-base-8M")   # 30 MB
fast <- load_model("potion-base-8M")
embeddings <- encode(text, model = fast)
```

Topic models can be guided: name your topics and seed them with words or
descriptions, and the seeds become the first centroids — initialization
only by default, frozen with `fixed_seeds = TRUE` (zero-shot assignment):

```r
topics(
  text,
  n_topics = 10,
  seeds = c(
    motivation = "student motivation and engagement",
    assessment = "grading feedback and assessment"
  )
)
```

Around the topic model, four utilities cover the everyday analysis moves:

```r
keywords(text, n = 5)                        # embedding-ranked keywords (MMR)
stop_words(add = c("students", "learning"))   # exclude corpus vocabulary
select_topics(text, n_topics = c(10, 20, 30), embeddings = embeddings)
topic_hierarchy <- topic_hierarchy(topic_model)          # which topics are neighbors?
plot(topic_hierarchy)                                    # labeled dendrogram
smaller <- reduce_topics(topic_model, 12)           # merge down, keep all verbs
```

Documents can be split into sentences, clauses, or phrases before embedding,
entirely offline and deterministically:

```r
segment(
  "We had two goals: speed and clarity; both were met. See Fig. 3.",
  level = "clause"
)
```

`segment()` returns one row per segment with the source document index,
guarding abbreviations (`abbreviations()`), decimals, and parentheticals
so they never end a sentence. The clause level (the default) also splits at
subordinating hinges while keeping comma enumerations whole.

Segments can carry their document's context into the embedding:
`blend()` keeps `alpha` of each segment's context-orthogonal residual
and inherits the rest from the parent document vector, so a sentence that is
ambiguous in isolation embeds near its document's subject while keeping what
it alone says. The result drops into `topics()` and
`representatives()` unchanged:

```r
sentences <- segment(abstracts, level = "sentence")
embeddings <- blend(sentences, abstracts, alpha = 0.5)
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
platform-specific path returned by `cache_dir()`. Every artifact is
verified by byte size and SHA-256 before it is used.

## Supported scope

- Models: thirteen pinned models (see `models()`), classic and modern,
  English and multilingual, 384 to 1,024 dimensions, 128 to 8,192 tokens,
  69 MB to 1.3 GB
- Pooling: attention-mask-aware mean pooling or CLS pooling, per model
- Prefixes: model-pinned input prefixes applied automatically (E5, Nomic)
- Custom models: `load_custom()` for any HF ONNX encoder repository,
  with trust-on-first-use local pinning
- Tokenization: each model's official `tokenizer.json`, truncated to the
  model's published maximum sequence length
- Pooling: attention-mask-aware mean pooling
- Normalization: row-wise L2 normalization by default
- Segmentation: deterministic sentence, clause, and phrase splitting with an
  abbreviation gazetteer and decimal/parenthetical guards
- Topic discovery: deterministic k-means with farthest-point initialization
- Topic descriptions: representative documents and BERTopic-style c-TF-IDF
- Topic inference: `predict()` for new documents, fuzzy soft topic_membership,
  generative `beta`, and segment-based document-topic `gamma`
- Topic evaluation: intrinsic UMass and NPMI coherence, and topic topic_diversity
- Topic visualization: deterministic base-graphics size, term, and MDS map views
- Backends: those exposed by `onnxr`, with CPU as the portable default

Arbitrary unpinned Hugging Face models and training/fine-tuning are
intentionally out of scope.
