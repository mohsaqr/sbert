# Encode Text with Sentence-BERT

Encode Text with Sentence-BERT

## Usage

``` r
encode(text, model = NULL, batch_size = 32L, normalize = TRUE, prefix = NULL)
```

## Arguments

- text:

  Character vector of sentences. Empty strings are supported; missing
  values are rejected.

- model:

  A loaded \[sbert_model\]\[load_model()\], the name of a pinned model
  from \[models()\], or \`NULL\` for the default \`all-MiniLM-L6-v2\`.
  Names are loaded lazily and kept in a session cache; the first use of
  a not-yet-installed model offers a one-time verified download
  (interactive prompt, or \`options(sbert.download = TRUE)\` in
  scripts).

- batch_size:

  Positive number of sentences processed per ONNX call.

- normalize:

  Whether to return row-wise L2-normalized embeddings.

- prefix:

  Text prepended to every input before tokenization. \`NULL\` (default)
  uses the model's pinned prefix (for example \`"query: "\` for E5
  models, which require one); \`""\` disables prefixing.

## Value

A numeric matrix with one row per input and one column per embedding
dimension of the loaded model.

## Examples

``` r
if (FALSE) { # interactive()
model <- load_model()
embeddings <- encode(c("A short sentence.", "Another sentence."), model)
}
```
