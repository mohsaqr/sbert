# Build data/feedback_translations.rda from the raw export.
#
# Source: feedback_translations.csv — the AI-generated mathematics feedback of
# the Levebee educational application (levebee.com), copied from the Bee2
# project export "feedback_translations (2).csv", December 2025.
# Run from the package root:
#   Rscript data-raw/feedback_translations.R
# The raw CSV and this script are .Rbuildignore'd; only the .rda ships.

raw <- read.csv(
  file.path("data-raw", "feedback_translations.csv"),
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)
stopifnot(
  identical(names(raw), c("feedback", "translation")),
  nrow(raw) == 8987L
)

# Faithful copy of the export; the only transformation is representing the
# 11 fully blank rows as NA instead of empty strings.
feedback_translations <- data.frame(
  feedback = ifelse(nzchar(trimws(raw$feedback)), raw$feedback, NA_character_),
  translation = ifelse(
    nzchar(trimws(raw$translation)), raw$translation, NA_character_
  ),
  stringsAsFactors = FALSE
)
stopifnot(
  sum(is.na(feedback_translations$feedback)) == 11L,
  sum(is.na(feedback_translations$translation)) == 11L,
  all(validEnc(feedback_translations$feedback[
    !is.na(feedback_translations$feedback)
  ])),
  all(validEnc(feedback_translations$translation[
    !is.na(feedback_translations$translation)
  ]))
)

save(
  feedback_translations,
  file = file.path("data", "feedback_translations.rda"),
  compress = "xz"
)
cat(
  "Wrote data/feedback_translations.rda:",
  nrow(feedback_translations), "rows,",
  file.size(file.path("data", "feedback_translations.rda")), "bytes\n"
)
