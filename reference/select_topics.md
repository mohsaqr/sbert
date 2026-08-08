# Compare Topic Counts Before Committing to One

Fits \[topics()\] once per candidate topic count — deterministically, on
one shared set of embeddings — and returns the quality metrics that
justify a granularity choice: mean topic coherence, topic
topic_diversity, and the share of embedding variance separated between
topics.

## Usage

``` r
select_topics(
  text,
  n_topics = c(5L, 10L, 15L, 20L, 25L, 30L),
  model = NULL,
  embeddings = NULL,
  measure = c("npmi", "umass"),
  n_terms = 10L,
  n_representatives = 1L,
  keep_models = TRUE,
  batch_size = 32L,
  ...
)
```

## Arguments

- text:

  A character vector of documents.

- n_topics:

  Integer vector of candidate topic counts, each at least 2 and below
  the number of documents. Default \`c(5, 10, 15, 20, 25, 30)\`.

- model:

  A loaded sbert model, a pinned model name, or \`NULL\` for the session
  default. Ignored when \`embeddings\` is supplied.

- embeddings:

  Optional precomputed document embedding matrix (one row per document);
  supply it to avoid re-encoding.

- measure:

  Coherence measure, \`"npmi"\` (default) or \`"umass"\`.

- n_terms:

  Top terms per topic used for coherence and topic_diversity. Default
  \`10\`.

- n_representatives:

  Representative documents kept per topic in each retained model.
  Default \`1\`; raise it when the stored models are meant to be
  inspected rather than only compared.

- keep_models:

  Whether to retain every fitted model so the chosen granularity needs
  no refitting. Default \`TRUE\`; set \`FALSE\` to return only the
  comparison table when memory matters.

- batch_size:

  Number of texts encoded per model call when encoding is needed.
  Default \`32\`.

- ...:

  Further arguments passed to \[topics()\] (for example \`stop_words\`
  or \`weighting\`).

## Value

A base data frame with one row per candidate and columns \`n_topics\`,
\`coherence\` (corpus mean), \`topic_diversity\`, and \`explained\`
(between-topic share of total variance). The coherence measure is
recorded in the \`measure\` attribute. When \`keep_models = TRUE\` the
result also carries class \`sbert_topic_sweep\` and the fitted models,
which \[fitted()\] extracts and \[plot.sbert_topic_sweep()\] draws.

## Details

There is no single correct topic count; this verb replaces the habit of
picking one by feel with a table you can defend. Coherence typically
falls as counts grow while separation rises, so look for the count after
which coherence stops improving (or starts degrading) rather than a
global maximum.

## See also

\[fitted()\] to pull one fitted model out of the sweep.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls", "Kittens nap in sunshine",
  "Stocks and bonds trade", "Markets price shares", "Banks report profit"
)
embeddings <- rbind(
  c(1, 0), c(0.95, 0.05), c(0.9, 0.1),
  c(0, 1), c(0.05, 0.95), c(0.1, 0.9)
)
sweep <- select_topics(text, n_topics = 2:3, embeddings = embeddings)
sweep
#> <sbert_topic_sweep> 2 candidates, coherence measure: npmi
#>  n_topics  coherence topic_diversity explained
#>         2 -0.4562038               1 0.9954323
#>         3  0.1817530               1 0.9972006
#> 
#> Fitted models retained: fitted(x, n_topics = 3)
fitted(sweep, n_topics = 2)
#> <sbert_topic_model>
#>   documents: 6
#>   topics: 2
#>   model: precomputed embeddings
#>   algorithm: deterministic k-means (Lloyd)
#>   topic sizes: 3, 3
#>   between/total SS: 99.5%
```
