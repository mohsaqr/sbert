# Obtain the Built-in Abbreviation Gazetteer

Returns the abbreviations whose periods are never treated as sentence
boundaries by \[segment()\]. Matching is case-insensitive and anchored
at a word boundary, so \`"ST."\` protects \`"St. Petersburg"\` but never
the end of \`"cost."\`.

## Usage

``` r
abbreviations()
```

## Value

A sorted character vector of uppercase abbreviations, each ending in a
period.

## Examples

``` r
head(abbreviations())
#> [1] "APPROX." "B.SC."   "CF."     "DR."     "E.G."    "EQ."    
```
