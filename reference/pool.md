# Pool Token Embeddings into Sentence Embeddings

Applies the pooling strategy of the loaded model — attention-mask-aware
mean pooling (the Sentence-BERT default) or CLS-token pooling (used by
the BGE and mxbai families) — and optional row-wise L2 normalization.

## Usage

``` r
pool(
  token_embeddings,
  attention_mask,
  normalize = TRUE,
  method = c("mean", "cls")
)
```

## Arguments

- token_embeddings:

  Numeric array shaped batch by sequence by hidden.

- attention_mask:

  Numeric matrix shaped batch by sequence, containing only zero and one.

- normalize:

  Whether to L2-normalize every sentence vector.

- method:

  \`"mean"\` (default) or \`"cls"\` (first token).

## Value

A numeric matrix shaped batch by hidden.

## Examples

``` r
hidden <- array(1:12, dim = c(2, 3, 2))
mask <- matrix(c(1, 1, 0, 1, 0, 0), nrow = 2, byrow = TRUE)
pool(hidden, mask)
#>           [,1]      [,2]
#> [1,] 0.2425356 0.9701425
#> [2,] 0.2425356 0.9701425
pool(hidden, mask, method = "cls")
#>           [,1]      [,2]
#> [1,] 0.1414214 0.9899495
#> [2,] 0.2425356 0.9701425
```
