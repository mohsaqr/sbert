# Summarize a Semantic Topic Model

Prints a compact scientific report – corpus size, cluster separation,
coherence, and topic_diversity – and returns a tidy per-topic quality
table.

## Usage

``` r
# S3 method for class 'sbert_topic_model'
summary(object, measure = c("npmi", "umass"), n_terms = 10L, ...)
```

## Arguments

- object:

  An \`sbert_topic_model\` returned by \[topics()\].

- measure:

  Coherence measure passed to \[coherence()\].

- n_terms:

  Number of top terms per topic used for coherence and topic_diversity.

- ...:

  Unused; present for S3 compatibility.

## Value

Invisibly, a data frame with one row per topic containing \`topic\`,
\`label\`, \`n_documents\`, \`proportion\`, and \`coherence\`.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls",
  "Stocks and bonds trade", "Markets price shares"
)
embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
topics <- topics(text, 2, embeddings = embeddings, n_terms = 3)
summary(topics)
#> Semantic topic model summary
#>   documents:            4
#>   topics:               2
#>   model:                precomputed embeddings
#>   between/total SS:      99.3%
#>   mean npmi  coherence:  -0.1667
#>   topic topic_diversity:      1.000 (top 10 terms)
#> 
#>  topic                   label n_documents proportion coherence
#>      1    chase / balls / cats           2        0.5    0.0000
#>      2 bonds / markets / price           2        0.5   -0.3333
```
