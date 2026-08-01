testthat::test_that("the live Sentence-BERT topic-modeling HTML renders", {
  model_source <- Sys.getenv("SBERT_TEST_MODEL_DIR", unset = "")
  testthat::skip_if(!nzchar(model_source), "official model fixture is not configured")

  project_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  input_path <- file.path(project_root, "examples", "topic_modeling.Rmd")
  testthat::skip_if_not(
    file.exists(input_path),
    "the repository-only live topic example is not included in the package"
  )
  output_directory <- file.path(project_root, "tmp")
  output_filename <- "topic_modeling.html"
  render_environment <- new.env(parent = globalenv())

  output_path <- rmarkdown::render(
    input = input_path,
    output_file = output_filename,
    output_dir = output_directory,
    envir = render_environment,
    quiet = TRUE,
    clean = TRUE
  )

  topic_model <- render_environment$topic_model
  testthat::expect_true(file.exists(output_path))
  testthat::expect_gt(unname(file.info(output_path)$size), 0)
  testthat::expect_s3_class(topic_model, "sbert_topic_model")
  testthat::expect_identical(dim(topic_model$embeddings), c(15L, 384L))
  testthat::expect_false(anyNA(topic_model$embeddings))
  testthat::expect_true(all(is.finite(topic_model$embeddings)))
  testthat::expect_identical(sort(topic_model$topics$n_documents), c(5L, 5L, 5L))
  testthat::expect_equal(render_environment$rand_agreement, 1)
  testthat::expect_identical(nrow(topic_model$terms), 18L)
  testthat::expect_identical(nrow(topic_model$representatives), 6L)

  html_text <- paste(readLines(output_path, warn = FALSE), collapse = "\n")
  testthat::expect_match(html_text, "Python-Free Sentence-BERT Topic Modeling", fixed = TRUE)
  testthat::expect_match(html_text, "Pairwise Rand agreement", fixed = TRUE)
  testthat::expect_match(html_text, "class TF-IDF", fixed = TRUE)
})

