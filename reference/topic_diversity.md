# Measure Topic Diversity

Reports the proportion of distinct terms among the pooled top terms of
every topic. A value near one indicates topics described by largely
different vocabulary; a low value indicates redundant topics that repeat
the same words (Dieng et al. 2020).

## Usage

``` r
topic_diversity(object, n_terms = 10L)
```

## Arguments

- object:

  An \`sbert_topic_model\` returned by \[topics()\].

- n_terms:

  Number of top terms per topic to pool. Capped at the number of terms
  available for each topic.

## Value

A single proportion in \`(0, 1\]\`.

## References

Dieng, A. B., Ruiz, F. J. R., and Blei, D. M. (2020). Topic modeling in
embedding spaces. TACL.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls",
  "Stocks and bonds trade", "Markets price shares"
)
embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
topics <- topics(text, 2, embeddings = embeddings, n_terms = 3)
topic_diversity(topics)
#> [1] 1
```
