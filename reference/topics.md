# Discover Semantic Topics in Documents

Performs deterministic document-level topic clustering with
Sentence-BERT embeddings. Supply either a loaded \`model\` or a
precomputed embedding matrix. Topics are summarized with representative
documents and class-based TF-IDF terms. This is embedding-based topic
discovery, not a probabilistic LDA model.

## Usage

``` r
topics(
  text,
  n_topics,
  column = NULL,
  model = NULL,
  embeddings = NULL,
  batch_size = 32L,
  iter_max = 100L,
  n_terms = 10L,
  n_representatives = 5L,
  stop_words = default_stop_words(),
  min_term_frequency = 1L,
  min_token_length = 2L,
  weighting = c("ctfidf", "bm25"),
  reduce_frequent_words = FALSE,
  stem = FALSE,
  keep_embeddings = TRUE,
  seeds = NULL,
  seed_embeddings = NULL,
  fixed_seeds = FALSE
)
```

## Arguments

- text:

  Character vector containing one document per element.

- n_topics:

  Number of semantic topics. Must be at least two.

- column:

  When \`text\` is a data frame, the name of the column holding the
  documents to model. Every other column is carried into
  \`\$documents\`, and rows whose text is missing, blank, or a
  bibliographic placeholder are dropped. Leave \`NULL\` for a character
  vector.

- model:

  A loaded \[sbert_model\]\[load_model()\], a pinned model name from
  \[models()\], or \`NULL\` for the default model. Ignored when
  \`embeddings\` are supplied.

- embeddings:

  Optional numeric matrix with one row per document; when supplied, no
  model is loaded or used.

- batch_size:

  Batch size passed to \[encode()\] when \`model\` is used.

- iter_max:

  Maximum deterministic k-means iterations.

- n_terms:

  Maximum class-based TF-IDF terms returned per topic.

- n_representatives:

  Maximum representative documents per topic.

- stop_words:

  Character vector excluded from topic terms. Use \`character()\` to
  disable stop-word filtering.

- min_term_frequency:

  Minimum corpus-wide token frequency.

- min_token_length:

  Minimum Unicode character length for a topic token.

- weighting:

  Class-based term-weighting scheme. \`"ctfidf"\` (default) uses \`tf \*
  log(1 + A / f_x)\`; \`"bm25"\` uses the BM25 inverse-frequency variant
  \`tf \* log(1 + (A - f_x + 0.5) / (f_x + 0.5))\`, which more
  aggressively down-weights terms shared across topics (Mendonca and
  Figueira 2025).

- reduce_frequent_words:

  Whether to square-root the within-topic term frequency before
  weighting, damping very frequent words.

- stem:

  Whether to collapse inflected forms (for example \`animals\` and
  \`animal\`) onto a shared Porter stem before scoring, displaying the
  most frequent surface form of each stem. Requires the \`SnowballC\`
  package.

- keep_embeddings:

  Whether to retain normalized document embeddings in the returned
  object.

- seeds:

  Optional guided topics: a character vector (or list of character
  vectors, collapsed with spaces) of seed words or topic descriptions —
  one element per seeded topic. Seeds are embedded and become the first
  \`length(seeds)\` cluster centroids; remaining topics (up to
  \`n_topics\`) are initialized away from the seeds. Seeded topics keep
  their position — topic i is seed i, never reordered by size — and a
  named \`seeds\` vector names those topics' labels. With precomputed
  \`embeddings\`, supply \`seed_embeddings\` as well.

- seed_embeddings:

  Optional matrix of seed embeddings (one row per seed, same dimension
  as the document embeddings); required when \`seeds\` is used together
  with precomputed \`embeddings\`.

- fixed_seeds:

  When \`TRUE\`, seed centroids are frozen and documents are simply
  assigned to the nearest seed (zero-shot classification into the seeded
  topics; \`n_topics\` must equal the number of seeds, and topics may be
  empty). When \`FALSE\` (default), seeds only initialize the clustering
  and the data can move the centroids.

## Value

An object of class \`sbert_topic_model\` containing document
assignments, topic summaries, ranked terms, representatives, centers,
and clustering diagnostics.

## Details

Topic labels are arbitrary cluster identifiers. Interpret them with the
ranked \`terms\` and \`representatives\` tables. Term scores use the
class-based TF-IDF weighting \`tf \* log(1 + A / f_x)\`, where \`tf\` is
the frequency of a term normalized within its topic, \`f_x\` is the
term's frequency across all topics, and \`A\` is the mean topic length
(a real number, matching Mendonca and Figueira (2025), Eq. 1). Set
\`weighting = "bm25"\` and/or \`reduce_frequent_words = TRUE\` for the
BM25 and square-root variants (their Eq. 2 to 4). The built-in tokenizer
preserves Unicode alphanumeric tokens and internal apostrophes, but does
not perform language-specific word segmentation for unspaced CJK text.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls",
  "Stocks and bonds trade", "Markets price shares"
)
embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
topics <- topics(text, 2, embeddings = embeddings)
topics$topics
#>   topic                   label n_documents proportion    withinss
#> 1     1    chase / balls / cats           2        0.5 0.006116265
#> 2     2 bonds / markets / price           2        0.5 0.006116265
```
