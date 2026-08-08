# Deduplicate a Text Corpus with Frequencies

Collapses a character vector to its distinct non-blank texts, keeping
the order of first appearance and counting how often each text occurred.
Embedding and clustering operate on the distinct texts (so repeated
templates cannot drag the cluster geometry), while the returned counts
carry the original frequencies back into reporting, for example through
\[topic_sizes()\].

## Usage

``` r
dedupe(text)
```

## Arguments

- text:

  A character vector. \`NA\` and blank (whitespace-only) elements are
  dropped before deduplication.

## Value

A base data frame with one row per distinct text and columns \`text\`
(in order of first appearance) and \`n\` (number of occurrences).

## Examples

``` r
dedupe(c("Very good!", "Try again.", "Very good!", NA, "  "))
#>         text n
#> 1 Very good! 2
#> 2 Try again. 1
```
