project_root <- Sys.getenv(
  "SBERT_PROJECT_ROOT",
  unset = normalizePath(file.path(getwd(), "..", ".."), mustWork = TRUE)
)
output_directory <- file.path(
  project_root,
  "outputs",
  "feedback_translation_topics"
)
source_path <- "/Users/mohammedsaqr/Downloads/Bee2/feedback_translations (2).csv"

testthat::test_that("feedback topic outputs preserve and classify every source row", {
  required_files <- file.path(
    output_directory,
    c(
      "document_assignments.csv",
      "topic_summary.csv",
      "topic_terms.csv",
      "topic_representatives.csv",
      "topic_count_diagnostics.csv",
      "data_quality.csv",
      "method.csv"
    )
  )
  testthat::expect_true(all(file.exists(required_files)))

  source_data <- read.csv(
    source_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  assignments <- read.csv(
    required_files[[1L]],
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )

  str(assignments)
  print(dim(assignments))
  print(head(assignments))
  print(summary(assignments$cosine_distance_to_centroid))

  testthat::expect_equal(nrow(assignments), nrow(source_data))
  testthat::expect_identical(assignments$row_id, seq_len(nrow(source_data)))
  testthat::expect_identical(assignments$feedback, source_data$feedback)
  testthat::expect_identical(assignments$translation, source_data$translation)
  testthat::expect_equal(anyDuplicated(assignments$row_id), 0L)
  testthat::expect_equal(sum(assignments$included), 8976L)
  testthat::expect_equal(sum(!assignments$included), 11L)
  testthat::expect_false(anyNA(assignments$topic[assignments$included]))
  testthat::expect_true(all(is.na(assignments$topic[!assignments$included])))
  testthat::expect_true(all(
    is.finite(assignments$cosine_distance_to_centroid[assignments$included])
  ))
  testthat::expect_true(all(
    assignments$cosine_distance_to_centroid[assignments$included] >= 0 &
      assignments$cosine_distance_to_centroid[assignments$included] <= 2
  ))
  testthat::expect_equal(sum(assignments$translation_has_cyrillic), 49L)
  testthat::expect_equal(
    sum(assignments$translation_has_non_ascii_latin),
    14L
  )
  testthat::expect_equal(sum(assignments$source_equals_translation), 167L)
  testthat::expect_equal(sum(assignments$translation_review_flag), 179L)
})

testthat::test_that("topic summaries, terms, and representatives are consistent", {
  topics <- read.csv(
    file.path(output_directory, "topic_summary.csv"),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  terms <- read.csv(
    file.path(output_directory, "topic_terms.csv"),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  representatives <- read.csv(
    file.path(output_directory, "topic_representatives.csv"),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  diagnostics <- read.csv(
    file.path(output_directory, "topic_count_diagnostics.csv"),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )

  str(topics)
  print(topics)
  str(terms)
  print(head(terms))
  str(representatives)
  print(head(representatives))

  testthat::expect_identical(topics$topic, seq_len(6L))
  testthat::expect_equal(sum(topics$n_rows), 8976L)
  testthat::expect_equal(sum(topics$proportion), 1, tolerance = 1e-12)
  testthat::expect_true(all(topics$n_rows > 0L))
  testthat::expect_true(all(vapply(
    split(terms$rank, terms$topic),
    function(ranks) identical(ranks, seq_along(ranks)),
    logical(1)
  )))
  testthat::expect_equal(nrow(representatives), 30L)
  testthat::expect_true(all(vapply(
    split(representatives$rank, representatives$topic),
    function(ranks) identical(ranks, seq_len(5L)),
    logical(1)
  )))
  testthat::expect_true(all(vapply(
    split(representatives$translation, representatives$topic),
    function(text) anyDuplicated(text) == 0L,
    logical(1)
  )))
  testthat::expect_equal(sum(diagnostics$selected), 1L)
  testthat::expect_identical(
    diagnostics$n_topics[diagnostics$selected],
    6L
  )
})
