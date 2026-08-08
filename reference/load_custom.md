# Load an Arbitrary Hugging Face Embedding Model

Loads any public Hugging Face repository that ships an ONNX encoder
export and a \`tokenizer.json\`, returning the same \`sbert_model\`
object as \[load_model()\] so that \[encode()\], \[topics()\], and every
downstream verb work unchanged.

## Usage

``` r
load_custom(
  id,
  revision = NULL,
  onnx_path = NULL,
  tokenizer_path = "tokenizer.json",
  pooling = NULL,
  prefix = NULL,
  max_length = NULL,
  cache_dir = default_cache_dir(),
  backend = "cpu",
  threads = 1L,
  verify = TRUE,
  quiet = FALSE
)
```

## Arguments

- id:

  Hugging Face repository id, for example \`"thenlper/gte-small"\`.

- revision:

  Optional commit hash to pin. \`NULL\` resolves the current revision on
  first download (recorded in the manifest thereafter).

- onnx_path:

  Repository path of the ONNX graph. \`NULL\` auto-detects
  \`onnx/model.onnx\` or \`model.onnx\`.

- tokenizer_path:

  Repository path of the tokenizer. Default \`"tokenizer.json"\`.

- pooling:

  \`"mean"\` or \`"cls"\`. \`NULL\` auto-detects from
  \`1_Pooling/config.json\` and fails with a clear message when absent.

- prefix:

  Text prepended to every input by \[encode()\]. \`NULL\` (default)
  keeps the recorded value (\`""\` on first use); pass \`""\` to clear a
  recorded prefix.

- max_length:

  Maximum word pieces per input. \`NULL\` auto-detects from
  \`sentence_bert_config.json\`, falling back to 512.

- cache_dir:

  Cache root returned by \[cache_dir()\].

- backend:

  ONNX execution backend accepted by \[onnxr::onnx_model()\].

- threads:

  Positive number of inference threads.

- verify:

  Whether to verify recorded byte sizes and SHA-256 hashes when loading
  from an existing cache.

- quiet:

  Whether to suppress download progress.

## Value

An object of class \`sbert_model\`.

## Details

This is the escape hatch below the curated registry. On first use the
current revision is resolved (unless \`revision\` is given), the
artifacts are downloaded, and their byte sizes and SHA-256 values are
recorded in a local manifest ("trust on first use"); later loads verify
the files against that manifest and never touch the network. Pooling,
prefix, and maximum length are auto-detected from the repository's
Sentence-Transformers configuration when present, and must be supplied
explicitly otherwise. The ONNX input signature and embedding dimension
are read from the graph itself. Unlike registry models, the package
makes no numerical-parity claim: you vouch for the configuration.

## Examples

``` r
if (FALSE) { # interactive()
model <- load_custom("thenlper/gte-small")
encode(c("one sentence", "another sentence"), model)
}
```
