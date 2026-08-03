testthat::test_that("official Sentence-BERT discovers three semantic topics", {
  model_source <- Sys.getenv("SBERT_TEST_MODEL_DIR", unset = "")
  testthat::skip_if(!nzchar(model_source), "official model fixture is not configured")
  testthat::skip_if(!onnxr::onnx_is_installed(), "ONNX Runtime is not installed")

  cache_dir <- tempfile("topic-integration-cache-")
  model_directory <- sbert:::sbert_model_directory(cache_dir)
  dir.create(model_directory, recursive = TRUE)
  copied <- file.copy(
    file.path(model_source, c("model.onnx", "tokenizer.json")),
    model_directory
  )
  testthat::expect_true(all(copied))
  model <- load_model(cache_dir, threads = 1L)
  expected_topic <- rep(c("astronomy", "cooking", "finance"), each = 5L)
  text <- c(
    "A telescope captured a distant galaxy filled with newborn stars.",
    "Astronomers measured the orbit of a planet around a nearby star.",
    "A spacecraft photographed craters across the surface of the moon.",
    "The observatory detected light from a supernova in deep space.",
    "A satellite mapped clouds of dust throughout the Milky Way galaxy.",
    "The chef sauteed garlic and onions in olive oil before adding tomatoes.",
    "Bread dough rises with yeast before it is baked in the oven.",
    "The recipe says to whisk eggs and sugar into the cake batter.",
    "Vegetables and spices simmered in broth to make a hearty soup.",
    "The cook grilled salmon seasoned with fresh lemon and herbs.",
    "The central bank raised interest rates to slow inflation.",
    "Investors bought government bonds when the stock market declined.",
    "Strong quarterly earnings sent the company share price higher.",
    "The borrower compared mortgage loans and their monthly payments.",
    "The portfolio spreads risk among equities, bonds, and cash."
  )

  result <- topics(
    text,
    3L,
    model = model,
    batch_size = 8L,
    n_terms = 6L,
    n_representatives = 2L,
    keep_embeddings = TRUE
  )
  true_same <- outer(expected_topic, expected_topic, `==`)
  predicted_same <- outer(result$documents$topic, result$documents$topic, `==`)
  upper <- upper.tri(true_same)
  rand_agreement <- mean(true_same[upper] == predicted_same[upper])

  testthat::expect_identical(dim(result$embeddings), c(15L, 384L))
  testthat::expect_false(anyNA(result$documents))
  testthat::expect_false(anyNA(result$embeddings))
  testthat::expect_true(all(is.finite(result$embeddings)))
  testthat::expect_equal(rowSums(result$embeddings^2), rep(1, 15L), tolerance = 1e-6)
  testthat::expect_identical(sort(result$topics$n_documents), c(5L, 5L, 5L))
  testthat::expect_equal(rand_agreement, 1)
  testthat::expect_true(all(result$terms$score > 0))
  testthat::expect_identical(nrow(result$representatives), 6L)
})
