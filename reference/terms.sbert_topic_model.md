# Topic Terms, Retuned Without Refitting

Recomputes the ranked terms of a fitted topic model under different term
settings. Term extraction depends only on the fitted topic assignments
and the document text, so every argument here can be changed without
repeating the clustering or the encoding.

## Usage

``` r
# S3 method for class 'sbert_topic_model'
terms(
  x,
  n = NULL,
  stop_words = NULL,
  min_term_frequency = NULL,
  min_token_length = NULL,
  weighting = NULL,
  reduce_frequent_words = NULL,
  stem = NULL,
  smoothing = 0,
  sort_by = c("score", "beta"),
  ...
)
```

## Arguments

- x:

  A fitted model from \[topics()\].

- n:

  Terms returned per topic, or \`NULL\` for the whole vocabulary (useful
  together with \`smoothing\` to obtain a full \\p(term \| topic)\\
  distribution). Defaults to the model's fitted \`n_terms\`.

- stop_words:

  Character vector excluded from terms. Defaults to the model's fitted
  list; see \[stop_words()\].

- min_term_frequency:

  Minimum corpus-wide token frequency.

- min_token_length:

  Minimum Unicode character length for a token.

- weighting:

  Class-based term weighting, \`"ctfidf"\` or \`"bm25"\`.

- reduce_frequent_words:

  Square-root the within-topic frequency before weighting, damping very
  frequent words.

- stem:

  Collapse inflected forms onto a shared Porter stem, displaying the
  most frequent surface form. Requires \`SnowballC\`.

- smoothing:

  Additive (Dirichlet) smoothing for the \`beta\` column. Only terms a
  topic actually used are returned, so with \`smoothing = 0\` (default)
  each topic's \`beta\` sums to one, while any positive value reserves
  mass for the zero-count vocabulary and the returned rows sum to less
  than one.

- sort_by:

  Order terms within each topic by \`"score"\` (default, the class-based
  weight — distinctive words) or \`"beta"\` (raw \\p(term \| topic)\\ —
  the words the topic uses most). \`n\` is applied after ordering.

- ...:

  Unused, present for generic consistency.

## Value

A base data frame with one row per topic and term: \`topic\`, \`label\`,
\`term\`, \`rank\`, \`score\` (the class-based weight), \`frequency\`
(within-topic count), and \`beta\`, the multinomial \\p(term \|
topic)\\.

## Examples

``` r
text <- c(
  "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
  "Stocks and bonds trade", "Markets price shares", "Banks report profit"
)
embeddings <- rbind(
  c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
  c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
)
fitted <- topics(
  text, n_topics = 2, embeddings = embeddings, min_term_frequency = 1
)
terms(fitted, n = 3)
#>   topic                   label    term rank     score frequency      beta
#> 1     1     cats / chase / mice    cats    1 0.3788329         2 0.2222222
#> 2     1     cats / chase / mice   chase    2 0.3788329         2 0.2222222
#> 3     1     cats / chase / mice    mice    3 0.3788329         2 0.2222222
#> 4     2 banks / bonds / markets   banks    1 0.2558428         1 0.1111111
#> 5     2 banks / bonds / markets   bonds    2 0.2558428         1 0.1111111
#> 6     2 banks / bonds / markets markets    3 0.2558428         1 0.1111111
terms(fitted, n = 3, weighting = "bm25")
#>   topic                   label    term rank     score frequency      beta
#> 1     1     cats / chase / mice    cats    1 0.3080654         2 0.2222222
#> 2     1     cats / chase / mice   chase    2 0.3080654         2 0.2222222
#> 3     1     cats / chase / mice    mice    3 0.3080654         2 0.2222222
#> 4     2 banks / bonds / markets   banks    1 0.2107911         1 0.1111111
#> 5     2 banks / bonds / markets   bonds    2 0.2107911         1 0.1111111
#> 6     2 banks / bonds / markets markets    3 0.2107911         1 0.1111111
```
