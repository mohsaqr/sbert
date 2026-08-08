# Blend Segment Embeddings with Their Document Context

Computes context-aware embeddings for text segments: each segment vector
is decomposed against its parent document embedding, and the blend keeps
\`alpha\` of the segment's context-orthogonal residual plus \`1 -
alpha\` of the document direction, renormalized to unit length: \$\$m =
normalize(\alpha (u - (u \cdot d) d) + (1 - \alpha) d)\$\$ with \`u\`
the unit-normalized segment embedding and \`d\` the unit-normalized
parent document embedding.

## Usage

``` r
blend(
  segments,
  documents = NULL,
  model = NULL,
  alpha = 0.5,
  embeddings = NULL,
  document_embeddings = NULL,
  batch_size = 32L
)
```

## Arguments

- segments:

  A data frame from \[segment()\] (or any data frame with integer
  \`document_id\` and character \`text\` columns), one row per segment.

- documents:

  The character vector of documents that was segmented;
  \`segments\$document_id\` indexes into it. Required unless both
  embedding matrices are supplied.

- model:

  A loaded sbert model, a pinned model name, or \`NULL\` for the session
  default (see \[load_model()\]). Ignored when embeddings are supplied.

- alpha:

  Blend weight in \`\[0, 1\]\` for the segment's context-orthogonal
  residual. Default \`0.5\`.

- embeddings:

  Optional precomputed segment embeddings (one row per row of
  \`segments\`). Supply together with \`document_embeddings\` to skip
  encoding.

- document_embeddings:

  Optional precomputed document embeddings (one row per document;
  \`segments\$document_id\` indexes its rows).

- batch_size:

  Number of texts encoded per model call when encoding is needed.
  Default \`32\`.

## Value

A numeric matrix with one L2-normalized row per segment, suitable for
the \`embeddings\` argument of \[topics()\], \[representatives()\], and
\[predict.sbert_topic_model()\].

## Details

This is the principled version of "mixing two embeddings": granularities
are mixed per unit, never by pooling units of different levels into one
training set (which lets clusters form along a length axis — see
length-induced embedding collapse, arXiv:2410.24200). A segment that is
ambiguous in isolation inherits its document's context; identical
segment texts from different documents get different blended vectors,
which keeps repeated boilerplate from collapsing into a single
artificial topic.

\`alpha = 0\` returns the parent document embedding; \`alpha = 1\` keeps
only what the segment says beyond its document; the default \`0.5\`
balances the two. A segment exactly collinear with its document falls
back to the document vector.

## Examples

``` r
segments <- segment("The model works. It is fast.", level = "sentence")
unit_vectors <- matrix(c(1, 0, 0.6, 0.8), nrow = 2, byrow = TRUE)
document_vectors <- matrix(c(0, 1), nrow = 1)
blend(
  segments,
  alpha = 0.5,
  embeddings = unit_vectors,
  document_embeddings = document_vectors
)
#>           [,1]      [,2]
#> [1,] 0.7071068 0.7071068
#> [2,] 0.5144958 0.8574929
```
