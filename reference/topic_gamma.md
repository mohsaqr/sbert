# Document-Topic Distributions from Segment Assignments

Computes \`gamma\`, the distribution of every document over the fitted
topics, by segmenting each document with \[segment()\], assigning each
segment to its nearest topic centroid, and normalizing the per-document
segment counts. This yields parameter-free mixed topic_membership: a
document that discusses two topics in different sentences receives
weight on both, which the single-embedding hard assignment cannot
express.

## Usage

``` r
topic_gamma(
  object,
  text,
  model = NULL,
  embeddings = NULL,
  level = c("clause", "sentence", "phrase"),
  batch_size = 32L
)
```

## Arguments

- object:

  A fitted \[topics()\] model.

- text:

  Character vector of documents.

- model:

  A loaded \[sbert_model\]\[load_model()\], a pinned model name, or
  \`NULL\` for the default model, used to embed the segments; ignored
  when \`embeddings\` are supplied.

- embeddings:

  Optional precomputed numeric matrix of segment embeddings whose rows
  align with \`segment(text, level = level)\`.

- level:

  Segmentation granularity passed to \[segment()\].

- batch_size:

  Batch size passed to \[encode()\] when \`model\` is used.

## Value

A base data frame with one row per document-topic pair and columns
\`document_id\`, \`topic\`, \`gamma\`, and \`n_segments\`. \`gamma\`
sums to 1 within each document; documents with no segments contribute no
rows.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls",
  "Stocks and bonds trade", "Markets price shares"
)
embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
fitted <- topics(text, 2, embeddings = embeddings)
mixed <- "Cats chase mice. Stocks and bonds trade."
segment_embeddings <- rbind(c(1, 0), c(0, 1))
topic_gamma(fitted, mixed, embeddings = segment_embeddings)
#>   document_id topic gamma n_segments
#> 1           1     1   0.5          2
#> 2           1     2   0.5          2
```
