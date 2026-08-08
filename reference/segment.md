# Segment Text into Sentences, Clauses, or Phrases

Splits each document into units at a selectable granularity using
deterministic sentence-boundary rules: a word-boundary-anchored,
case-insensitive abbreviation gazetteer (see \[abbreviations()\]), a
decimal-number guard, and a parenthetical guard, plus shallow clause
chunking at discourse connectives. No model call is involved, so
segmentation works offline and is fully reproducible.

## Usage

``` r
segment(
  text,
  level = c("clause", "sentence", "phrase"),
  merge_below = 0L,
  abbreviations = default_abbreviations()
)
```

## Arguments

- text:

  A character vector of documents. Names, when present, are carried into
  the \`document_name\` column.

- level:

  \`"clause"\` (default), \`"sentence"\`, or \`"phrase"\`.

- merge_below:

  Re-join segments shorter than this many words into their neighbor.
  \`0\` (default) disables merging and returns the pure segmentation.

- abbreviations:

  Character vector of abbreviations (each ending in a period) whose
  periods never end a sentence. Defaults to the built-in gazetteer from
  \[abbreviations()\]; matching is case-insensitive.

## Value

A base data frame with one row per segment and columns \`document_id\`
(integer position in \`text\`), \`document_name\` (name of the input
element, or \`""\`), \`segment\` (integer position within the document),
and \`text\` (the segment). Blank documents contribute no rows.

## Details

Each finer level adds separators:

- \`"sentence"\`:

  splits at \`.\`, \`?\`, and \`!\` only.

- \`"clause"\`:

  additionally splits at \`;\`, \`:\`, spaced dashes, and subordinating
  hinges (for example "which", "where", "in terms of"). Comma-separated
  enumerations stay whole. This is the default.

- \`"phrase"\`:

  additionally splits at commas, for maximal granularity.

Before segmentation, each document is normalized: curly quotes become
straight quotes, en and em dashes become spaced hyphens, and runs of
whitespace collapse to single spaces. Letter case is preserved.

## Examples

``` r
segment(
  "We propose a simulator which runs alongside the processor."
)
#>   document_id document_name segment                                text
#> 1           1                     1              We propose a simulator
#> 2           1                     2 which runs alongside the processor.

segment(
  c(intro = "See Fig. 3 for details. The next part follows."),
  level = "sentence"
)
#>   document_id document_name segment                    text
#> 1           1         intro       1 See Fig. 3 for details.
#> 2           1         intro       2  The next part follows.
```
