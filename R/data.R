#' Levebee AI Mathematics Feedback with English Translations
#'
#' AI-generated feedback messages shown to young learners solving mathematics
#' exercises in the Levebee educational application, paired with their English
#' translations. The messages are short instructional hints and encouragements
#' ("Choose the picture...", "Try listening to the instruction again.") whose
#' source languages include Czech, Slovak, Polish, German, Hungarian,
#' Romanian, Ukrainian, Russian, Mongolian, and Vietnamese. The corpus is a
#' realistic, quirk-preserving benchmark for the package's embedding,
#' topic_similarity, segmentation, and topic-modeling workflow: translations repeat
#' (templates such as "Very good!" appear many times), a small number of rows
#' were never translated (the translation still contains Cyrillic text), and a
#' few source messages are identical to their translation.
#'
#' @format A data frame with 8,987 rows and 2 character columns:
#' \describe{
#'   \item{feedback}{The original feedback message in the source language.}
#'   \item{translation}{The English translation of the message.}
#' }
#' Eleven rows are missing both fields (`NA`); the export represented them as
#' blank. Among the non-missing rows there are 8,144 distinct translations.
#' Messages are at most 193 characters long.
#'
#' @source Anonymized export of the AI mathematics feedback of the Levebee
#'   educational application (<https://www.levebee.com/>), December 2025. The
#'   messages address learners generically and contain no personal names or
#'   identifiers.
#' @examples
#' head(feedback_translations)
#'
#' segment(
#'   head(feedback_translations$translation, 5),
#'   level = "sentence"
#' )
"feedback_translations"
