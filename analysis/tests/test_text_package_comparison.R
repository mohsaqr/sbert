project_root <- Sys.getenv("SBERT_PROJECT_ROOT", unset = ".")
comparison_directory <- file.path(
  project_root,
  "outputs",
  "feedback_translation_topics",
  "text_package_comparison"
)
comparison_path <- file.path(
  comparison_directory,
  "text_topic_comparison.rds"
)

testthat::test_that("comparison artifacts retain every modeled translation", {
  testthat::expect_true(file.exists(comparison_path))
  comparison <- readRDS(comparison_path)
  assignments <- comparison$comparison_assignments

  testthat::expect_s3_class(assignments, "data.frame")
  testthat::expect_equal(nrow(assignments), 8976L)
  testthat::expect_true(all(diff(assignments$row_id) > 0L))
  testthat::expect_equal(range(assignments$row_id), c(1L, 8987L))
  testthat::expect_equal(length(setdiff(seq_len(8987L), assignments$row_id)), 11L)
  testthat::expect_false(anyNA(assignments$translation))
  testthat::expect_true(all(nzchar(trimws(assignments$translation))))
  testthat::expect_equal(sum(assignments$translation_review_flag), 179L)
})

testthat::test_that("native and reduced text partitions have verified sizes", {
  comparison <- readRDS(comparison_path)
  assignments <- comparison$comparison_assignments
  native_topics <- sort(unique(assignments$text_native_topic))
  reduced_topics <- sort(unique(assignments$text_reduced_topic))

  testthat::expect_identical(native_topics, c(-1L, 0:404))
  testthat::expect_identical(reduced_topics, c(-1L, 0:5))
  testthat::expect_equal(sum(assignments$text_native_topic == -1L), 1602L)
  testthat::expect_equal(sum(assignments$text_reduced_topic == -1L), 1602L)
  testthat::expect_equal(
    as.integer(table(assignments$text_reduced_topic)),
    c(1602L, 6572L, 503L, 131L, 78L, 48L, 42L)
  )
})

testthat::test_that("agreement and common-space quality metrics are valid", {
  comparison <- readRDS(comparison_path)
  agreement <- comparison$agreement_metrics
  quality <- comparison$quality_metrics
  all_rows <- subset(
    agreement,
    subset == "all_rows" & method_a == "sbert_6" &
      method_b == "text_reduced_6"
  )

  testthat::expect_equal(nrow(all_rows), 1L)
  testthat::expect_equal(all_rows$adjusted_rand, 0.138574, tolerance = 1e-6)
  testthat::expect_equal(
    all_rows$normalized_mutual_information,
    0.2127723,
    tolerance = 1e-6
  )
  testthat::expect_true(all(is.finite(quality$mean_cosine_distance)))
  testthat::expect_true(all(quality$sampled_mean_silhouette >= -1))
  testthat::expect_true(all(quality$sampled_mean_silhouette <= 1))
  testthat::expect_equal(quality$topic_count, c(6L, 405L, 6L))
  testthat::expect_equal(quality$outlier_rows, c(0L, 1602L, 1602L))
})

testthat::test_that("agreement metrics are label invariant on synthetic data", {
  synthetic_a <- c(1L, 1L, 2L, 2L, 3L, 3L)
  synthetic_b <- c(9L, 9L, 4L, 4L, 7L, 7L)

  testthat::expect_equal(aricode::ARI(synthetic_a, synthetic_b), 1)
  testthat::expect_equal(aricode::NMI(synthetic_a, synthetic_b), 1)
})

testthat::test_that("comparison CSV exports agree with the serialized result", {
  comparison <- readRDS(comparison_path)
  export_names <- c(
    "agreement_metrics.csv",
    "quality_metrics.csv",
    "topic_summaries.csv",
    "duplicate_consistency.csv",
    "comparison_assignments.csv"
  )
  export_paths <- file.path(comparison_directory, export_names)
  exported <- lapply(
    export_paths,
    read.csv,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  testthat::expect_true(all(file.exists(export_paths)))
  testthat::expect_equal(nrow(exported[[1L]]), nrow(comparison$agreement_metrics))
  testthat::expect_equal(nrow(exported[[2L]]), nrow(comparison$quality_metrics))
  testthat::expect_equal(nrow(exported[[3L]]), nrow(comparison$topic_summaries))
  testthat::expect_equal(
    nrow(exported[[4L]]),
    nrow(comparison$duplicate_consistency)
  )
  testthat::expect_equal(
    exported[[5L]]$text_reduced_topic,
    comparison$comparison_assignments$text_reduced_topic
  )
})
