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
  words <- stop_words()

  testthat::expect_type(words, "character")
  testthat::expect_true(all(c("a", "and", "the", "with") %in% words))
  testthat::expect_identical(words, sort(unique(words)))
  testthat::expect_false(anyNA(words))
  testthat::expect_identical(anyDuplicated(words), 0L)
  testthat::expect_error(stop_words("fi"), "Only the built-in English")
})

testthat::test_that("topic tokenization handles Unicode and filtering", {
  tokens <- sbert:::tokenize_topic_documents(
    c("Café déjà vu — 東京", "Rock-n-roll isn\u2019t dead and buried"),
    stop_words = c("and", "buried"),
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
    stop_words = character(),
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
  result_one <- topics(
    topic_test_corpus(),
    3L,
    embeddings = topic_test_embeddings(),
    n_terms = 4L,
    n_representatives = 2L,
    keep_embeddings = TRUE
  )
  result_two <- topics(
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

  result <- topics(
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

  testthat::expect_error(topics(character(), 2L, embeddings = embeddings))
  testthat::expect_error(topics(c("ok", NA_character_), 2L, embeddings = embeddings[1:2, ]))
  testthat::expect_error(topics(c("ok", " "), 2L, embeddings = embeddings[1:2, ]), "blank")
  testthat::expect_error(topics(topic_test_corpus(), 1L, embeddings = embeddings))
  testthat::expect_error(topics(topic_test_corpus(), 7L, embeddings = embeddings))
  testthat::expect_error(
    topics(topic_test_corpus(), 3L, model = fake_sbert_model(), embeddings = embeddings),
    "not both"
  )
  testthat::expect_error(
    topics(topic_test_corpus(), 3L, embeddings = embeddings[1:5, ]),
    "one row per document"
  )
  non_finite <- embeddings
  non_finite[1L, 1L] <- Inf
  testthat::expect_error(
    topics(topic_test_corpus(), 3L, embeddings = non_finite),
    "finite numeric matrix"
  )
})

topic_frame_fixture <- function() {
  data.frame(
    abstract = c(
      "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
      NA, "[NO ABSTRACT AVAILABLE]", "   ",
      "Stocks and bonds trade", "Markets price shares", "Banks report profit"
    ),
    year = 2001:2009,
    journal = letters[1:9],
    stringsAsFactors = FALSE
  )
}

topic_frame_embeddings <- function() {
  rbind(
    c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
    c(0.5, 0.5, 0), c(0.4, 0.4, 0.2), c(0.3, 0.3, 0.3),
    c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
  )
}

fit_topic_frame <- function(...) {
  topics(
    topic_frame_fixture(),
    n_topics = 2L,
    column = "abstract",
    embeddings = topic_frame_embeddings(),
    n_terms = 3L,
    min_term_frequency = 1L,
    ...
  )
}

testthat::test_that("a data frame drops unusable rows and keeps its metadata", {
  fitted <- fit_topic_frame()
  testthat::expect_identical(nrow(fitted$documents), 6L)
  testthat::expect_true(all(c("year", "journal") %in% names(fitted$documents)))
  testthat::expect_identical(
    fitted$documents$year,
    c(2001L, 2002L, 2003L, 2007L, 2008L, 2009L)
  )
})

testthat::test_that("supplied embeddings are subset to the surviving rows", {
  fitted <- fit_topic_frame()
  kept <- topic_frame_embeddings()[c(1:3, 7:9), , drop = FALSE]
  testthat::expect_identical(nrow(fitted$embeddings), 6L)
  # Rows are stored L2-normalized; direction must match the supplied rows.
  cosines <- rowSums(fitted$embeddings * kept) /
    sqrt(rowSums(kept^2))
  testthat::expect_equal(cosines, rep(1, 6L), tolerance = 1e-12)
})

testthat::test_that("topic label rides on every table keyed by topic", {
  fitted <- fit_topic_frame()
  for (table in list(fitted$documents, fitted$terms, fitted$representatives)) {
    testthat::expect_true("label" %in% names(table))
    testthat::expect_identical(
      table$label,
      fitted$topics$label[table$topic]
    )
  }
})

testthat::test_that("label sits immediately after topic", {
  fitted <- fit_topic_frame()
  testthat::expect_identical(
    names(fitted$terms)[1:2],
    c("topic", "label")
  )
})

testthat::test_that("embeddings are retained by default", {
  fitted <- fit_topic_frame()
  testthat::expect_false(is.null(fitted$embeddings))
  dropped <- fit_topic_frame(keep_embeddings = FALSE)
  testthat::expect_null(dropped$embeddings)
})

testthat::test_that("a character vector still takes the original path", {
  fitted <- topics(
    c(
      "Cats chase mice", "Kittens chase mice too", "Cats nap daily",
      "Stocks and bonds trade", "Markets price shares", "Banks report profit"
    ),
    n_topics = 2L,
    embeddings = rbind(
      c(1, 0, 0), c(0.98, 0.02, 0), c(0.96, 0, 0.04),
      c(0, 1, 0), c(0.02, 0.98, 0), c(0, 0.96, 0.04)
    ),
    n_terms = 3L,
    min_term_frequency = 1L
  )
  testthat::expect_identical(nrow(fitted$documents), 6L)
  testthat::expect_false("year" %in% names(fitted$documents))
})

testthat::test_that("malformed data-frame input is rejected clearly", {
  testthat::expect_error(
    topics(topic_frame_fixture(), n_topics = 2L),
    "supply 'column'"
  )
  testthat::expect_error(
    topics(topic_frame_fixture(), n_topics = 2L, column = "missing"),
    "not found"
  )
  testthat::expect_error(
    topics(letters[1:6], n_topics = 2L, column = "abstract"),
    "only when the first argument is a data frame"
  )
})

testthat::test_that("representatives default to the fitted corpus", {
  fitted <- fit_topic_frame()
  from_fit <- representatives(fitted, n = 2L)
  explicit <- representatives(
    fitted,
    fitted$documents$text,
    embeddings = fitted$embeddings,
    n = 2L
  )
  testthat::expect_equal(from_fit, explicit)
})

testthat::test_that("representatives explain a model that kept no embeddings", {
  dropped <- fit_topic_frame(keep_embeddings = FALSE)
  testthat::expect_error(
    representatives(dropped, n = 2L),
    "kept no embeddings"
  )
})
