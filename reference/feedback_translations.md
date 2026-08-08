# Levebee AI Mathematics Feedback with English Translations

AI-generated feedback messages shown to young learners solving
mathematics exercises in the Levebee educational application, paired
with their English translations. The messages are short instructional
hints and encouragements ("Choose the picture...", "Try listening to the
instruction again.") whose source languages include Czech, Slovak,
Polish, German, Hungarian, Romanian, Ukrainian, Russian, Mongolian, and
Vietnamese. The corpus is a realistic, quirk-preserving benchmark for
the package's embedding, topic_similarity, segmentation, and
topic-modeling workflow: translations repeat (templates such as "Try
again." recur), some source messages remain in their original language,
and a few are identical to their translation.

## Usage

``` r
feedback_translations
```

## Format

A data frame with 8,757 rows and 2 character columns:

- feedback:

  The original feedback message in the source language.

- translation:

  The English translation of the message.

There are 8,005 distinct translations. Messages are at most 193
characters long.

## Source

Anonymized export of the AI mathematics feedback of the Levebee
educational application (\<https://www.levebee.com/\>), December 2025.
The messages address learners generically and contain no personal names
or identifiers.

## Examples

``` r
head(feedback_translations)
#>                                                       feedback
#> 1                                Víš, co znamená o jeden více?
#> 2                   V červeném má být o dva více než v modrém.
#> 3                     Když to není člověk, tak co to může být?
#> 4                                Co znamená všechny za prvním?
#> 5 V šedém rámečku je pět obrázků. Kde je o jeden obrázek méně?
#> 6                              Podle čeho se obrázky střídají?
#>                                                                 translation
#> 1                                        Do you know what “one more” means?
#> 2                     There should be two more in the red than in the blue.
#> 3                                   If it’s not a person, what could it be?
#> 4                            What does “everyone after the first one” mean?
#> 5 There are five pictures in the gray box. Where is there one picture less?
#> 6                                            How do the pictures alternate?

segment(
  head(feedback_translations$translation, 5),
  level = "sentence"
)
#>   document_id document_name segment
#> 1           1                     1
#> 2           2                     1
#> 3           3                     1
#> 4           4                     1
#> 5           5                     1
#> 6           5                     2
#>                                                    text
#> 1                    Do you know what "one more" means?
#> 2 There should be two more in the red than in the blue.
#> 3               If it's not a person, what could it be?
#> 4        What does "everyone after the first one" mean?
#> 5              There are five pictures in the gray box.
#> 6                      Where is there one picture less?
```
