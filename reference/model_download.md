# Download a Pinned Sentence-BERT Model

Downloads the official ONNX graph and tokenizer of a pinned model only
after this function is called. Files are locked to an immutable
repository revision and checked against package-controlled SHA-256
values. See \[models()\] for the available models; the default remains
\`all-MiniLM-L6-v2\`.

## Usage

``` r
model_download(
  model = "all-MiniLM-L6-v2",
  cache_dir = default_cache_dir(),
  quiet = FALSE,
  timeout = 600
)
```

## Arguments

- model:

  Name of a pinned model listed by \[models()\].

- cache_dir:

  Cache root returned by \[cache_dir()\].

- quiet:

  Whether to suppress download progress.

- timeout:

  Minimum download timeout in seconds.

## Value

Invisibly, the model directory.

## Examples

``` r
if (FALSE) { # interactive()
model_download()
}
```
