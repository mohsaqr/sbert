# Remove an Installed Model from the Cache

Remove an Installed Model from the Cache

## Usage

``` r
model_remove(cache_dir = default_cache_dir(), model = "all-MiniLM-L6-v2")
```

## Arguments

- cache_dir:

  Cache root returned by \[cache_dir()\].

- model:

  Name of a pinned model listed by \[models()\].

## Value

Invisibly, whether the model directory no longer exists.

## Examples

``` r
cache <- file.path(tempdir(), "sbert-example-cache")
model_remove(cache)
```
