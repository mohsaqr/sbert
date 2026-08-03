plots_test_model <- function(keep_embeddings = TRUE) {
  text <- c(
    animals_1 = "Cats chase mice and sleep.",
    animals_2 = "Dogs chase balls and sleep.",
    learning_1 = "Neural networks learn representations.",
    learning_2 = "Machine learning models learn patterns.",
    vehicles_1 = "Bicycles have wheels and pedals.",
    vehicles_2 = "Cars have wheels and engines."
  )
  embeddings <- rbind(
    c(1, 0, 0), c(0.95, 0.05, 0),
    c(0, 1, 0), c(0.05, 0.95, 0),
    c(0, 0, 1), c(0.05, 0, 0.95)
  )
  topics(
    text,
    3L,
    embeddings = embeddings,
    n_terms = 4L,
    keep_embeddings = keep_embeddings
  )
}

testthat::test_that("the palette returns the requested number of colours", {
  colours <- topic_palette(3L)
  testthat::expect_length(colours, 3L)
  testthat::expect_true(all(grepl("^#", colours)))
  testthat::expect_error(topic_palette(0))
})

testthat::test_that("every plot type draws without error and returns the model", {
  model <- plots_test_model()
  temporary_device <- tempfile(fileext = ".pdf")
  grDevices::pdf(temporary_device)
  on.exit(
    {
      grDevices::dev.off()
      unlink(temporary_device)
    },
    add = TRUE
  )

  testthat::expect_identical(plot(model, type = "sizes"), model)
  testthat::expect_identical(plot(model, type = "terms", n_terms = 3L), model)
  testthat::expect_identical(plot(model, type = "map"), model)
})

testthat::test_that("the map requires stored embeddings", {
  model <- plots_test_model(keep_embeddings = FALSE)
  temporary_device <- tempfile(fileext = ".pdf")
  grDevices::pdf(temporary_device)
  on.exit(
    {
      grDevices::dev.off()
      unlink(temporary_device)
    },
    add = TRUE
  )

  testthat::expect_error(plot(model, type = "map"), "keep_embeddings")
  testthat::expect_error(plot(model, type = "orbit"))
})
