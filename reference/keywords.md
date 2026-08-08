# Extract Keywords from Documents by Embedding Similarity

Ranks each document's own words and phrases by cosine topic_similarity
between their embeddings and the document embedding, computed with the
same model, and selects the top \`n\` by maximal marginal relevance so
the keywords are relevant without being redundant (the KeyBERT design).

## Usage

``` r
keywords(
  text,
  model = NULL,
  n = 10L,
  ngrams = 2L,
  topic_diversity = 0.3,
  stop_words = default_stop_words(),
  min_token_length = 3L,
  batch_size = 32L
)
```

## Arguments

- text:

  A character vector of documents. Names, when present, are carried into
  the \`document_name\` column.

- model:

  A loaded sbert model, a pinned model name, or \`NULL\` for the session
  default.

- n:

  Maximum keywords returned per document. Default \`10\`.

- ngrams:

  Maximum phrase length in tokens. Default \`2\` (unigrams and bigrams).

- topic_diversity:

  Maximal-marginal-relevance trade-off in \`\[0, 1)\`: \`0\` ranks
  purely by topic_similarity, larger values penalize keywords similar to
  ones already selected. Default \`0.3\`.

- stop_words:

  Words excluded from candidates. Defaults to \[stop_words()\].

- min_token_length:

  Minimum character length of a candidate token. Default \`3\`.

- batch_size:

  Number of texts encoded per model call. Default \`32\`.

## Value

A base data frame with one row per keyword and columns \`document_id\`,
\`document_name\`, \`rank\`, \`keyword\`, and \`topic_similarity\`
(cosine topic_similarity between the keyword and its document).

## Details

Candidates are consecutive-token n-grams (lengths \`1\` to \`ngrams\`)
drawn from the document after stop-word removal, so a candidate phrase
never crosses a removed stop word silently: "analysis of networks"
yields the candidates "analysis", "networks", and "analysis networks".

## Examples

``` r
if (FALSE) { # \dontrun{
keywords(
  "Transition network analysis models learning event sequences.",
  n = 5
)
} # }
```
