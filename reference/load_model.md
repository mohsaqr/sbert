# Load a Pinned Sentence-BERT Model

Loads already-downloaded model artifacts. The function never downloads
model files or native libraries. See \[models()\] for the available
pinned models.

## Usage

``` r
load_model(
  model = "all-MiniLM-L6-v2",
  cache_dir = default_cache_dir(),
  backend = "cpu",
  threads = 1L,
  verify = TRUE
)
```

## Arguments

- model:

  Name of a pinned model listed by \[models()\].

- cache_dir:

  Cache root returned by \[cache_dir()\].

- backend:

  ONNX execution backend accepted by \[onnxr::onnx_model()\].

- threads:

  Positive number of inference threads.

- verify:

  Whether to verify file sizes and SHA-256 hashes before loading.

## Value

An object of class \`sbert_model\`.

## Examples

``` r
if (FALSE) { # interactive()
model <- load_model()
}
```
