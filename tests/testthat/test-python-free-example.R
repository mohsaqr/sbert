testthat::test_that("the genuine Python-free SBERT HTML example renders", {
  model_source <- Sys.getenv("SBERT_TEST_MODEL_DIR", unset = "")
  testthat::skip_if(!nzchar(model_source), "official model fixture is not configured")

  project_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  input_path <- file.path(project_root, "examples", "python_free_sbert.Rmd")
  testthat::skip_if_not(
    file.exists(input_path),
    "the repository-only live-model example is not included in the package"
  )
  output_directory <- file.path(project_root, "tmp")
  output_filename <- "python_free_sbert.html"
  render_environment <- new.env(parent = globalenv())

  output_path <- rmarkdown::render(
    input = input_path,
    output_file = output_filename,
    output_dir = output_directory,
    envir = render_environment,
    quiet = TRUE,
    clean = TRUE
  )

  testthat::expect_true(file.exists(output_path))
  testthat::expect_gt(unname(file.info(output_path)$size), 0)
  testthat::expect_identical(
    dim(render_environment$embedding_matrix),
    c(4L, 384L)
  )
  testthat::expect_false(anyNA(render_environment$embedding_matrix))
  testthat::expect_true(all(is.finite(render_environment$embedding_matrix)))
  testthat::expect_equal(
    unname(rowSums(render_environment$embedding_matrix^2)),
    rep(1, 4L),
    tolerance = 1e-6
  )
  testthat::expect_gt(
    render_environment$within_topic_similarity,
    render_environment$between_topic_similarity
  )

  html_text <- paste(readLines(output_path, warn = FALSE), collapse = "\n")
  testthat::expect_match(html_text, "Genuine Sentence-BERT", fixed = TRUE)
  testthat::expect_match(html_text, "Python is not installed", fixed = TRUE)
  testthat::expect_match(html_text, "384-dimensional", fixed = TRUE)
})
