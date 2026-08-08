# Compute Cosine Similarity

Compute Cosine Similarity

## Usage

``` r
topic_similarity(x, y = NULL)
```

## Arguments

- x:

  Numeric embedding matrix, or one numeric vector.

- y:

  Optional second embedding matrix or vector. When omitted, computes all
  pairwise similarities among rows of \`x\`.

## Value

A numeric topic_similarity matrix.

## Examples

``` r
x <- rbind(c(1, 0), c(0, 1), c(1, 1))
topic_similarity(x)
#>           [,1]      [,2]      [,3]
#> [1,] 1.0000000 0.0000000 0.7071068
#> [2,] 0.0000000 1.0000000 0.7071068
#> [3,] 0.7071068 0.7071068 1.0000000
```
