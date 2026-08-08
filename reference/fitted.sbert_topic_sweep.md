# Extract One Fitted Model from a Topic-Count Sweep

Returns the model \[select_topics()\] already fitted for a given topic
count, so choosing a granularity from the comparison table costs no
refitting.

## Usage

``` r
# S3 method for class 'sbert_topic_sweep'
fitted(object, n_topics, ...)
```

## Arguments

- object:

  A sweep returned by \[select_topics()\] with \`keep_models = TRUE\`
  (the default).

- n_topics:

  The topic count to extract. Must be one of the candidates in the
  sweep.

- ...:

  Unused, present for generic consistency.

## Value

The \`sbert_topic_model\` fitted for that candidate.

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
sweep <- select_topics(
  text, n_topics = 2:3, embeddings = embeddings, n_terms = 3
)
fitted(sweep, n_topics = 2)
#> <sbert_topic_model>
#>   documents: 6
#>   topics: 2
#>   model: precomputed embeddings
#>   algorithm: deterministic k-means (Lloyd)
#>   topic sizes: 3, 3
#>   between/total SS: 99.9%
```
