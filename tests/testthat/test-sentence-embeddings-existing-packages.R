testthat::test_that("the existing-package sentence embedding example renders", {
  project_root <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = TRUE
  )
  input_path <- file.path(
    project_root,
    "examples",
    "sentence_embeddings_existing_packages.Rmd"
  )
  testthat::skip_if_not(
    file.exists(input_path),
    "the repository-only historical example is not included in the package"
  )
  # The example compares against other packages; skip when any is absent
  # (mirrors the required_packages guard inside the example itself).
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("quanteda")
  testthat::skip_if_not_installed("reticulate")
  testthat::skip_if_not_installed("RSpectra")
  output_directory <- file.path(tempdir(), "sbert-existing-package-example")
  dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
  output_filename <- "sentence_embeddings_existing_packages.html"
  render_environment <- new.env(parent = globalenv())

  output_path <- tryCatch(
    rmarkdown::render(
      input = input_path,
      output_file = output_filename,
      output_dir = output_directory,
      envir = render_environment,
      quiet = TRUE,
      clean = TRUE
    ),
    error = function(error_condition) {
      stop(sprintf(
        "Rendering the sentence embedding example failed: %s",
        conditionMessage(error_condition)
      ))
    }
  )

  testthat::expect_true(file.exists(output_path))
  testthat::expect_gt(unname(file.info(output_path)$size), 0)

  testthat::expect_type(render_environment$sbert_available, "logical")
  testthat::expect_length(render_environment$sbert_available, 1L)

  embedding_matrix <- render_environment$embedding_matrix
  similarity_matrix <- render_environment$similarity_matrix

  testthat::expect_true(is.matrix(embedding_matrix))
  testthat::expect_identical(dim(embedding_matrix), c(6L, 2L))
  testthat::expect_false(anyNA(embedding_matrix))
  testthat::expect_true(all(is.finite(embedding_matrix)))

  testthat::expect_true(is.matrix(similarity_matrix))
  testthat::expect_identical(dim(similarity_matrix), c(6L, 6L))
  testthat::expect_false(anyNA(similarity_matrix))
  testthat::expect_true(all(is.finite(similarity_matrix)))
  testthat::expect_equal(
    similarity_matrix,
    t(similarity_matrix),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    unname(diag(similarity_matrix)),
    rep(1, 6L),
    tolerance = 1e-12
  )
  testthat::expect_gt(
    render_environment$within_topic_similarity,
    render_environment$between_topic_similarity
  )

  html_lines <- readLines(output_path, warn = FALSE)
  html_text <- paste(html_lines, collapse = "\n")

  testthat::expect_match(html_text, "Sentence-BERT", fixed = TRUE)
  testthat::expect_match(html_text, "not Sentence-BERT", fixed = TRUE)
  testthat::expect_true(grepl(
    "Latent[[:space:]]+Semantic[[:space:]]+Analysis",
    html_text
  ))
})
