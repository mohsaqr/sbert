topic_test_corpus <- function() {
  c(
    animals_1 = "Cats chase mice and sleep.",
    animals_2 = "Dogs chase balls and sleep.",
    learning_1 = "Neural networks learn representations.",
    learning_2 = "Machine learning models learn patterns.",
    vehicles_1 = "Bicycles have wheels and pedals.",
    vehicles_2 = "Cars have wheels and engines."
  )
}

topic_test_embeddings <- function() {
  rbind(
    c(1, 0, 0),
    c(0.95, 0.05, 0),
    c(0, 1, 0),
    c(0.05, 0.95, 0),
    c(0, 0, 1),
    c(0.05, 0, 0.95)
  )
}

testthat::test_that("built-in stop words are stable and validated", {
  words <- sbert_stopwords()

  testthat::expect_type(words, "character")
  testthat::expect_true(all(c("a", "and", "the", "with") %in% words))
  testthat::expect_identical(words, sort(unique(words)))
  testthat::expect_false(anyNA(words))
  testthat::expect_identical(anyDuplicated(words), 0L)
  testthat::expect_error(sbert_stopwords("fi"), "Only the built-in English")
})

testthat::test_that("topic tokenization handles Unicode and filtering", {
  tokens <- sbert:::tokenize_topic_documents(
    c("Café déjà vu — 東京", "Rock-n-roll isn\u2019t dead and buried"),
    stopwords = c("and", "buried"),
    min_token_length = 2L
  )

  testthat::expect_identical(tokens[[1L]], c("café", "déjà", "vu", "東京"))
  testthat::expect_identical(tokens[[2L]], c("rock", "roll", "isn't", "dead"))
  testthat::expect_false(anyNA(unlist(tokens, use.names = FALSE)))
  testthat::expect_error(
    sbert:::tokenize_topic_documents("text", NA_character_, 2L)
  )
})

testthat::test_that("class TF-IDF matches the audited numeric fixture", {
  result <- sbert:::topic_term_scores(
    text = c("Apple apple apple", "banana", "banana carrot"),
    topic = as.integer(c(1, 1, 2)),
    n_topics = 2L,
    n_terms = 3L,
    stopwords = character(),
    min_term_frequency = 1L,
    min_token_length = 1L
  )

  expected_counts <- rbind(c(3, 1, 0), c(0, 1, 1))
  colnames(expected_counts) <- c("apple", "banana", "carrot")
  expected_scores <- rbind(
    c(0.519860385419959, 0.229072682968539, 0),
    c(0, 0.458145365937078, 0.693147180559945)
  )
  colnames(expected_scores) <- colnames(expected_counts)

  testthat::expect_equal(result$average_topic_length, 3)
  testthat::expect_equal(result$counts, expected_counts, tolerance = 1e-12)
  testthat::expect_equal(result$scores, expected_scores, tolerance = 1e-12)
  testthat::expect_identical(
    result$terms$term,
    c("apple", "banana", "carrot", "banana")
  )
  testthat::expect_false(anyNA(result$terms))
  testthat::expect_identical(anyDuplicated(result$terms[c("topic", "term")]), 0L)
  testthat::expect_error(
    sbert:::topic_term_scores(
      c("a", "b"),
      as.integer(c(1, 2)),
      2L,
      2L,
      character(),
      1L,
      2L
    ),
    "No topic terms remain"
  )
})

testthat::test_that("deterministic centers are distinct and do not use RNG", {
  embeddings <- topic_test_embeddings()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    },
    add = TRUE
  )
  if (had_seed) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  centers_one <- sbert:::deterministic_topic_centers(embeddings, 3L)
  centers_two <- sbert:::deterministic_topic_centers(embeddings, 3L)

  testthat::expect_identical(centers_one, centers_two)
  testthat::expect_identical(dim(centers_one), c(3L, 3L))
  testthat::expect_identical(nrow(unique(as.data.frame(centers_one))), 3L)
  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  testthat::expect_error(
    sbert:::deterministic_topic_centers(matrix(1, 4L, 2L), 2L),
    "distinct document embeddings"
  )
})

testthat::test_that("embedding clustering recovers separated groups", {
  fit_one <- sbert:::fit_embedding_topics(topic_test_embeddings(), 3L, 100L)
  fit_two <- sbert:::fit_embedding_topics(topic_test_embeddings(), 3L, 100L)
  expected_group <- rep(seq_len(3L), each = 2L)
  true_same <- outer(expected_group, expected_group, `==`)
  predicted_same <- outer(fit_one$topic, fit_one$topic, `==`)

  testthat::expect_identical(fit_one, fit_two)
  testthat::expect_true(all(true_same == predicted_same))
  testthat::expect_identical(sort(fit_one$size), c(2L, 2L, 2L))
  testthat::expect_identical(dim(fit_one$centers), c(3L, 3L))
  testthat::expect_true(all(is.finite(fit_one$distance)))
  testthat::expect_true(all(fit_one$distance >= 0))
})

testthat::test_that("representatives are ranked and capped per topic", {
  documents <- data.frame(
    document_id = 1:4,
    document_name = letters[1:4],
    text = paste("document", 1:4),
    topic = c(1L, 1L, 1L, 2L),
    distance = c(0.3, 0.1, 0.2, 0.4),
    stringsAsFactors = FALSE
  )
  representatives <- sbert:::topic_representatives(documents, 2L, 2L)

  testthat::expect_identical(representatives$document_id, c(2L, 3L, 4L))
  testthat::expect_identical(representatives$rank, c(1L, 2L, 1L))
  testthat::expect_identical(representatives$topic, c(1L, 1L, 2L))
})

testthat::test_that("topic model output is complete and deterministic", {
  result_one <- sbert_topics(
    topic_test_corpus(),
    3L,
    embeddings = topic_test_embeddings(),
    n_terms = 4L,
    n_representatives = 2L,
    keep_embeddings = TRUE
  )
  result_two <- sbert_topics(
    topic_test_corpus(),
    3L,
    embeddings = topic_test_embeddings(),
    n_terms = 4L,
    n_representatives = 2L,
    keep_embeddings = TRUE
  )

  testthat::expect_s3_class(result_one, "sbert_topic_model")
  testthat::expect_identical(result_one, result_two)
  testthat::expect_identical(
    names(result_one),
    c(
      "documents", "topics", "terms", "representatives", "centers",
      "embeddings", "diagnostics", "model", "settings"
    )
  )
  testthat::expect_identical(result_one$documents$document_name, names(topic_test_corpus()))
  testthat::expect_identical(result_one$documents$text, unname(topic_test_corpus()))
  testthat::expect_false(anyNA(result_one$documents))
  testthat::expect_identical(sum(result_one$topics$n_documents), 6L)
  testthat::expect_identical(sort(result_one$topics$n_documents), c(2L, 2L, 2L))
  testthat::expect_identical(dim(result_one$embeddings), c(6L, 3L))
  testthat::expect_equal(rowSums(result_one$embeddings^2), rep(1, 6L), tolerance = 1e-12)
  testthat::expect_true(all(nzchar(result_one$topics$label)))
  testthat::expect_true(all(result_one$terms$rank <= 4L))
  testthat::expect_output(print(result_one), "deterministic k-means")
  returned <- NULL
  invisible(utils::capture.output(returned <- print(result_one)))
  testthat::expect_identical(returned, result_one)
})

testthat::test_that("model-backed topics forward encoding arguments", {
  model <- fake_sbert_model()
  observed <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    encode_topic_documents = function(text, model, batch_size) {
      observed$text <- text
      observed$model <- model
      observed$batch_size <- batch_size
      rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))
    },
    .package = "sbert"
  )
  text <- c("cats mice", "dogs balls", "stocks bonds", "market shares")

  result <- sbert_topics(
    text,
    2L,
    model = model,
    batch_size = 2L,
    keep_embeddings = FALSE
  )

  testthat::expect_identical(observed$text, text)
  testthat::expect_identical(observed$model, model)
  testthat::expect_identical(observed$batch_size, 2L)
  testthat::expect_null(result$embeddings)
  testthat::expect_identical(result$model$id, model$id)
})

testthat::test_that("topic modeling rejects malformed inputs", {
  embeddings <- topic_test_embeddings()

  testthat::expect_error(sbert_topics(character(), 2L, embeddings = embeddings))
  testthat::expect_error(sbert_topics(c("ok", NA_character_), 2L, embeddings = embeddings[1:2, ]))
  testthat::expect_error(sbert_topics(c("ok", " "), 2L, embeddings = embeddings[1:2, ]), "blank")
  testthat::expect_error(sbert_topics(topic_test_corpus(), 1L, embeddings = embeddings))
  testthat::expect_error(sbert_topics(topic_test_corpus(), 7L, embeddings = embeddings))
  testthat::expect_error(
    sbert_topics(topic_test_corpus(), 3L, model = fake_sbert_model(), embeddings = embeddings),
    "not both"
  )
  testthat::expect_error(
    sbert_topics(topic_test_corpus(), 3L, embeddings = embeddings[1:5, ]),
    "one row per document"
  )
  non_finite <- embeddings
  non_finite[1L, 1L] <- Inf
  testthat::expect_error(
    sbert_topics(topic_test_corpus(), 3L, embeddings = non_finite),
    "finite numeric matrix"
  )
})
