# Obtain and Adjust the Topic Stop-Word List

Returns the stop words excluded from term extraction and keyword
candidates. \`add\` extends the list — the standard way to remove a
corpus-wide vocabulary from topic labels (words every document shares
carry no discriminative information). \`remove\` un-lists built-in
entries whose surface form is meaningful in a specific corpus.

## Usage

``` r
stop_words(language = "en", add = NULL, remove = NULL)
```

## Arguments

- language:

  Currently only \`"en"\` is supported.

- add:

  Character vector of extra words to exclude (matched
  case-insensitively).

- remove:

  Character vector of words to drop from the list.

## Value

A sorted character vector of lowercase stop words.

## Examples

``` r
head(stop_words())
#> [1] "a"       "about"   "after"   "again"   "against" "all"    
stop_words(add = c("students", "learning"), remove = "against")
#>   [1] "a"          "about"      "after"      "again"      "all"       
#>   [6] "am"         "an"         "and"        "any"        "are"       
#>  [11] "as"         "at"         "be"         "because"    "been"      
#>  [16] "before"     "being"      "below"      "between"    "both"      
#>  [21] "but"        "by"         "can"        "could"      "did"       
#>  [26] "do"         "does"       "doing"      "down"       "during"    
#>  [31] "each"       "few"        "for"        "from"       "further"   
#>  [36] "had"        "has"        "have"       "having"     "he"        
#>  [41] "her"        "here"       "hers"       "herself"    "him"       
#>  [46] "himself"    "his"        "how"        "i"          "if"        
#>  [51] "in"         "into"       "is"         "it"         "its"       
#>  [56] "itself"     "just"       "learning"   "me"         "more"      
#>  [61] "most"       "my"         "myself"     "no"         "nor"       
#>  [66] "not"        "now"        "of"         "off"        "on"        
#>  [71] "once"       "only"       "or"         "other"      "our"       
#>  [76] "ours"       "ourselves"  "out"        "over"       "own"       
#>  [81] "same"       "she"        "should"     "so"         "some"      
#>  [86] "students"   "such"       "than"       "that"       "the"       
#>  [91] "their"      "theirs"     "them"       "themselves" "then"      
#>  [96] "there"      "these"      "they"       "this"       "those"     
#> [101] "through"    "to"         "too"        "under"      "until"     
#> [106] "up"         "very"       "was"        "we"         "were"      
#> [111] "what"       "when"       "where"      "which"      "while"     
#> [116] "who"        "whom"       "why"        "will"       "with"      
#> [121] "would"      "you"        "your"       "yours"      "yourself"  
#> [126] "yourselves"
```
