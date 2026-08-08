# Representative Text Units for Every Topic

Selects, for each fitted topic, the text units (documents, sentences, or
clause segments) that best evidence it. The default ranking is by
\*distinctiveness margin\* — the unit's cosine distance to its
second-best centroid minus the distance to its own — rather than by raw
closeness. Raw closeness systematically favors long units, because topic
centroids are averages of whole-document embeddings and long units
resemble whole documents; the margin instead rewards units that are
unambiguously about one topic and no other, which are naturally short
and pointed. Ties break toward the shorter unit, then alphabetically, so
the selection is fully deterministic.

## Usage

``` r
representatives(
  object,
  text = NULL,
  model = NULL,
  embeddings = NULL,
  n = 3L,
  rank = c("margin", "distance"),
  batch_size = 32L
)
```

## Arguments

- object:

  A fitted \[topics()\] model.

- text:

  Character vector of candidate units, for example the \`text\` column
  of \[segment()\].

- model:

  A loaded \[sbert_model\]\[load_model()\], a pinned model name, or
  \`NULL\` for the default model; ignored when \`embeddings\` are
  supplied.

- embeddings:

  Optional numeric matrix with one row per unit.

- n:

  Number of units returned per topic.

- rank:

  \`"margin"\` (default) or \`"distance"\` (raw closeness to the topic
  centroid).

- batch_size:

  Batch size passed to \[encode()\] when \`model\` is used.

## Value

A base data frame with columns \`topic\`, \`rank\`, \`text\`,
\`distance\` (to the unit's own centroid), and \`margin\`, ordered by
topic and rank. Topics to which no unit is assigned contribute no rows.

## Examples

``` r
text <- c(
  "Cats chase mice", "Dogs chase balls",
  "Stocks and bonds trade", "Markets price shares"
)
embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
fitted <- topics(text, 2, embeddings = embeddings)
representatives(
  fitted,
  c("kittens pounce", "bond yields", "pets and prices"),
  embeddings = rbind(c(1, 0.1), c(0.1, 1), c(0.7, 0.7)),
  n = 1
)
#>   topic rank           text     distance    margin
#> 1     1    1 kittens pounce 0.0009828586 0.8446397
#> 2     2    1    bond yields 0.0009828586 0.8446397
```
