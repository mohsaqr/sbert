# Score Topic Coherence

Computes intrinsic topic coherence from the top terms of a fitted topic
model and the co-occurrence structure of the input documents. Coherence
rewards topics whose top terms tend to appear together in the same
documents, and is the standard quantitative measure for embedding-based
topic models (Mimno et al. 2011).

## Usage

``` r
coherence(object, measure = c("npmi", "umass"), n_terms = 10L, smoothing = 1)
```

## Arguments

- object:

  An \`sbert_topic_model\` returned by \[topics()\].

- measure:

  Either \`"umass"\` (Mimno et al. 2011; larger, i.e. closer to zero, is
  more coherent) or \`"npmi"\` (normalized pointwise mutual information,
  bounded in \`\[-1, 1\]\`; larger is more coherent).

- n_terms:

  Number of top terms per topic to score. Capped at the number of terms
  actually available for each topic.

- smoothing:

  Additive smoothing for the UMass co-occurrence count.

## Value

A data frame with one row per topic and columns \`topic\`, \`label\`,
\`measure\`, \`n_terms\`, and \`coherence\`. The corpus-level mean is
attached as the \`"mean_coherence"\` attribute. Topics with fewer than
two scorable terms yield \`NA\`.

## References

Mimno, D., Wallach, H. M., Talley, E., Leenders, M., and McCallum, A.
(2011). Optimizing semantic coherence in topic models. EMNLP.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls",
  "Stocks and bonds trade", "Markets price shares"
)
embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
topics <- topics(text, 2, embeddings = embeddings, n_terms = 3)
coherence(topics)
#>   topic                   label measure n_terms  coherence
#> 1     1    chase / balls / cats    npmi       3  0.0000000
#> 2     2 bonds / markets / price    npmi       3 -0.3333333
```
