# Measure the sbert Cache

Measure the sbert Cache

## Usage

``` r
cache_size(cache_dir = cache_dir())
```

## Arguments

- cache_dir:

  Cache root returned by \[cache_dir()\].

## Value

The total number of bytes currently used by regular files beneath the
cache root.

## Examples

``` r
cache_size(tempdir())
#> [1] 701232
```
