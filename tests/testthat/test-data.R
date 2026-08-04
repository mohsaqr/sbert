testthat::test_that("feedback_translations has the documented shape", {
  testthat::expect_s3_class(feedback_translations, "data.frame")
  testthat::expect_identical(dim(feedback_translations), c(8757L, 2L))
  testthat::expect_identical(
    names(feedback_translations),
    c("feedback", "translation")
  )
  testthat::expect_type(feedback_translations$feedback, "character")
  testthat::expect_type(feedback_translations$translation, "character")
})

testthat::test_that("feedback_translations matches the documented quirks", {
  # No missing or blank fields, and no near-empty rows.
  testthat::expect_identical(sum(is.na(feedback_translations$feedback)), 0L)
  testthat::expect_identical(sum(is.na(feedback_translations$translation)), 0L)
  testthat::expect_true(all(nzchar(feedback_translations$translation)))
  testthat::expect_identical(
    length(unique(feedback_translations$translation)),
    8005L
  )
  testthat::expect_lte(
    max(nchar(feedback_translations$translation)),
    193L
  )
  testthat::expect_true(all(validEnc(feedback_translations$translation)))
  # Every translation is rendered into English: none is left in Cyrillic.
  # The source `feedback` may still be in its original language.
  testthat::expect_identical(
    sum(grepl("[Ѐ-ӿ]", feedback_translations$translation)),
    0L
  )
  testthat::expect_gt(
    sum(grepl("[Ѐ-ӿ]", feedback_translations$feedback)),
    0L
  )
})

testthat::test_that("dataset rows feed segment directly", {
  segments <- segment(
    utils::head(feedback_translations$translation, 5L),
    level = "sentence"
  )
  testthat::expect_true(nrow(segments) >= 5L)
  testthat::expect_identical(unique(segments$document_id), 1:5)
})
