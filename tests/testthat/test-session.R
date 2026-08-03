testthat::test_that("a loaded model passes through untouched", {
  model <- fake_sbert_model()
  testthat::expect_identical(sbert:::resolve_sbert_model(model), model)
})

testthat::test_that("names load lazily once and stay in the session cache", {
  sbert:::clear_sbert_session()
  loads <- new.env(parent = emptyenv())
  loads$count <- 0L
  testthat::local_mocked_bindings(
    model_status = function(cache_dir, model) data.frame(valid = TRUE),
    load_model = function(model, cache_dir) {
      loads$count <- loads$count + 1L
      fake_sbert_model()
    },
    .package = "sbert"
  )

  first <- sbert:::resolve_sbert_model("all-MiniLM-L6-v2")
  second <- sbert:::resolve_sbert_model("all-MiniLM-L6-v2")
  by_default <- sbert:::resolve_sbert_model(NULL)
  by_full_id <- sbert:::resolve_sbert_model("sentence-transformers/all-MiniLM-L6-v2")

  testthat::expect_identical(loads$count, 1L)
  testthat::expect_identical(first, second)
  testthat::expect_identical(first, by_default)
  testthat::expect_identical(first, by_full_id)
  sbert:::clear_sbert_session()
})

testthat::test_that("missing models never download without an explicit yes", {
  sbert:::clear_sbert_session()
  downloads <- new.env(parent = emptyenv())
  downloads$count <- 0L
  testthat::local_mocked_bindings(
    model_status = function(cache_dir, model) {
      data.frame(valid = downloads$count > 0L)
    },
    model_download = function(model, cache_dir, quiet = TRUE) {
      downloads$count <- downloads$count + 1L
      invisible(cache_dir)
    },
    load_model = function(model, cache_dir) fake_sbert_model(),
    .package = "sbert"
  )

  # non-interactive without opting in: clear refusal, nothing downloaded
  withr::local_options(sbert.download = NULL)
  testthat::expect_error(
    sbert:::resolve_sbert_model("all-MiniLM-L6-v2"),
    "model_download"
  )
  testthat::expect_identical(downloads$count, 0L)

  # opting in downloads exactly once
  withr::local_options(sbert.download = TRUE)
  resolved <- sbert:::resolve_sbert_model("all-MiniLM-L6-v2")
  testthat::expect_s3_class(resolved, "sbert_model")
  testthat::expect_identical(downloads$count, 1L)
  sbert:::clear_sbert_session()
})

testthat::test_that("unknown names fail before any install logic", {
  testthat::expect_error(
    sbert:::resolve_sbert_model("no-such-model"),
    "Unknown model"
  )
})
