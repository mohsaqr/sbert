testthat::test_that("feedback_translations has the documented shape", {
  testthat::expect_s3_class(feedback_translations, "data.frame")
  testthat::expect_identical(dim(feedback_translations), c(8987L, 2L))
  testthat::expect_identical(
    names(feedback_translations),
    c("feedback", "translation")
  )
  testthat::expect_type(feedback_translations$feedback, "character")
  testthat::expect_type(feedback_translations$translation, "character")
})

testthat::test_that("feedback_translations matches the documented quirks", {
  testthat::expect_identical(sum(is.na(feedback_translations$feedback)), 11L)
  testthat::expect_identical(sum(is.na(feedback_translations$translation)), 11L)
  complete <- stats::complete.cases(feedback_translations)
  testthat::expect_identical(
    length(unique(feedback_translations$translation[complete])),
    8144L
  )
  testthat::expect_true(
    all(nzchar(feedback_translations$translation[complete]))
  )
  testthat::expect_lte(
    max(nchar(feedback_translations$translation[complete])),
    193L
  )
  testthat::expect_true(
    all(validEnc(feedback_translations$translation[complete]))
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
