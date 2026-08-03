fit_toy_model <- function(keep_embeddings = FALSE) {
  topics(
    c(
      "apple apple banana", "banana apple fruit",
      "stocks trade daily", "markets trade stocks"
    ),
    2L,
    embeddings = rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9)),
    stop_words = character(),
    keep_embeddings = keep_embeddings
  )
}

testthat::test_that("predict reproduces the fitted assignments on training data", {
  fitted <- fit_toy_model()
  training_embeddings <- rbind(c(1, 0), c(0.9, 0.1), c(0, 1), c(0.1, 0.9))

  predicted <- predict(
    fitted,
    fitted$documents$text,
    embeddings = training_embeddings
  )

  testthat::expect_s3_class(predicted, "data.frame")
  testthat::expect_identical(
    names(predicted),
    c("document_id", "document_name", "text", "topic", "label", "distance")
  )
  testthat::expect_identical(predicted$topic, fitted$documents$topic)
  testthat::expect_equal(
    predicted$distance,
    fitted$documents$distance,
    tolerance = 1e-12
  )
  testthat::expect_identical(
    predicted$label,
    fitted$topics$label[predicted$topic]
  )
})

testthat::test_that("predict assigns new documents to the nearest centroid", {
  fitted <- fit_toy_model()
  fruit_topic <- fitted$documents$topic[[1L]]
  finance_topic <- fitted$documents$topic[[3L]]

  predicted <- predict(
    fitted,
    c(new_fruit = "fresh apples", new_finance = "bond markets"),
    embeddings = rbind(c(0.95, 0.05), c(0.05, 0.95))
  )

  testthat::expect_identical(predicted$topic, c(fruit_topic, finance_topic))
  testthat::expect_identical(
    predicted$document_name,
    c("new_fruit", "new_finance")
  )
  testthat::expect_true(all(predicted$distance >= 0))
})

testthat::test_that("predict validates its inputs", {
  fitted <- fit_toy_model()
  testthat::expect_error(
    predict(fitted, "text", embeddings = rbind(c(1, 0, 0))),
    "dimensions"
  )
  testthat::expect_error(
    predict(fitted, "text", model = "not-a-model", embeddings = rbind(c(1, 0))),
    "not both"
  )
  testthat::expect_error(
    predict(fitted, c("a", "b"), embeddings = rbind(c(1, 0)))
  )
  testthat::expect_error(predict(fitted, NA_character_, embeddings = rbind(c(1, 0))))
})

testthat::test_that("topic_membership matches the hand-computed fuzzy-c-means values", {
  fitted <- fit_toy_model()
  fruit_topic <- fitted$documents$topic[[1L]]
  # Unit embedding with cosine topic_similarity 0.8 / 0.6 to the two centroids has
  # distances 0.2 / 0.4; with sharpness m = 2 the memberships are exactly
  # (1/0.2^2) / (1/0.2^2 + 1/0.4^2) = 0.8 and 0.2.
  centroid_like <- rbind(c(0.8, 0.6))
  pure_centers <- topics(
    c("aa bb", "cc dd", "ee ff", "gg hh"),
    2L,
    embeddings = rbind(c(1, 0), c(1, 0), c(0, 1), c(0, 1)),
    stop_words = character()
  )

  topic_membership <- topic_membership(
    pure_centers,
    embeddings = centroid_like,
    sharpness = 2
  )

  testthat::expect_identical(
    names(topic_membership),
    c("document_id", "topic", "probability", "rank")
  )
  testthat::expect_equal(sum(topic_membership$probability), 1, tolerance = 1e-12)
  testthat::expect_equal(
    topic_membership$probability[topic_membership$rank == 1L],
    0.8,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    topic_membership$probability[topic_membership$rank == 2L],
    0.2,
    tolerance = 1e-9
  )
  testthat::expect_length(unique(topic_membership$document_id), 1L)
  testthat::expect_identical(sort(topic_membership$topic), c(1L, 2L))
  # equidistant embedding gets exactly uniform topic_membership
  uniform <- topic_membership(
    pure_centers,
    embeddings = rbind(c(1, 1) / sqrt(2)),
    sharpness = 2
  )
  testthat::expect_equal(uniform$probability, c(0.5, 0.5), tolerance = 1e-9)
  testthat::expect_identical(fruit_topic %in% c(1L, 2L), TRUE)
})

testthat::test_that("topic_membership ranking is sharpness-invariant and uses stored embeddings", {
  fitted <- fit_toy_model(keep_embeddings = TRUE)

  crisp <- topic_membership(fitted, sharpness = 1.15)
  soft <- topic_membership(fitted, sharpness = 3)

  testthat::expect_identical(nrow(crisp), 8L)
  testthat::expect_identical(
    crisp$topic[crisp$rank == 1L],
    soft$topic[soft$rank == 1L]
  )
  testthat::expect_identical(
    crisp$topic[crisp$rank == 1L],
    fitted$documents$topic
  )
  per_document <- vapply(
    split(crisp$probability, crisp$document_id),
    sum,
    numeric(1)
  )
  testthat::expect_equal(unname(per_document), rep.int(1, 4L), tolerance = 1e-12)
  # crisper sharpness concentrates more mass on the top topic
  testthat::expect_true(
    all(
      crisp$probability[crisp$rank == 1L] >= soft$probability[soft$rank == 1L]
    )
  )
})

testthat::test_that("topic_membership fails clearly without embeddings", {
  fitted <- fit_toy_model(keep_embeddings = FALSE)
  testthat::expect_error(topic_membership(fitted), "keep_embeddings")
  testthat::expect_error(
    topic_membership(fitted, embeddings = rbind(c(1, 0)), sharpness = 1),
    "sharpness"
  )
})



testthat::test_that("gamma recovers mixed topic_membership from segments", {
  fitted <- fit_toy_model()
  fruit_topic <- fitted$documents$topic[[1L]]
  finance_topic <- fitted$documents$topic[[3L]]
  text <- c(
    mixed = "Fresh apples taste sweet. Stocks trade daily.",
    pure = "Ripe bananas everywhere."
  )
  segments <- segment(text, level = "sentence")
  testthat::expect_identical(nrow(segments), 3L)
  segment_embeddings <- rbind(c(1, 0), c(0, 1), c(0.9, 0.1))

  gamma <- topic_gamma(
    fitted,
    text,
    embeddings = segment_embeddings,
    level = "sentence"
  )

  testthat::expect_identical(
    names(gamma),
    c("document_id", "topic", "gamma", "n_segments")
  )
  testthat::expect_identical(nrow(gamma), 4L)
  mixed_rows <- gamma[gamma$document_id == 1L, , drop = FALSE]
  testthat::expect_equal(mixed_rows$gamma, c(0.5, 0.5), tolerance = 1e-12)
  testthat::expect_identical(mixed_rows$n_segments, c(2L, 2L))
  pure_rows <- gamma[gamma$document_id == 2L, , drop = FALSE]
  testthat::expect_equal(
    pure_rows$gamma[pure_rows$topic == fruit_topic],
    1,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    pure_rows$gamma[pure_rows$topic == finance_topic],
    0,
    tolerance = 1e-12
  )
  per_document <- vapply(split(gamma$gamma, gamma$document_id), sum, numeric(1))
  testthat::expect_equal(unname(per_document), c(1, 1), tolerance = 1e-12)
})

testthat::test_that("gamma validates segment embedding alignment", {
  fitted <- fit_toy_model()
  testthat::expect_error(
    topic_gamma(
      fitted,
      "One sentence. Two sentences.",
      embeddings = rbind(c(1, 0)),
      level = "sentence"
    ),
    "one per unit"
  )
  testthat::expect_error(
    topic_gamma(
      fitted,
      "Some text.",
      model = "x",
      embeddings = rbind(c(1, 0)),
      level = "sentence"
    ),
    "not both"
  )
})

testthat::test_that("representatives rank by margin and prefer unambiguous units", {
  fitted <- fit_toy_model()
  fruit_topic <- fitted$documents$topic[[1L]]
  # unit 1: unambiguous fruit; unit 3: between both topics (small margin);
  # both belong to the fruit topic, but margin ranking must put unit 1 first
  # even though the ambiguous unit could sit closer to the centroid.
  units <- c("pure fruit words", "pure finance words", "fruit and finance mixed")
  unit_embeddings <- rbind(c(1, 0.05), c(0.05, 1), c(0.75, 0.66))

  result <- representatives(
    fitted,
    units,
    embeddings = unit_embeddings,
    n = 2
  )

  testthat::expect_identical(
    names(result),
    c("topic", "rank", "text", "distance", "margin")
  )
  fruit_rows <- subset(result, topic == fruit_topic)
  testthat::expect_identical(fruit_rows$text[[1L]], "pure fruit words")
  testthat::expect_true(all(fruit_rows$margin[[1L]] > fruit_rows$margin[[2L]]))
  testthat::expect_true(all(result$margin >= -1e-12))

  by_distance <- representatives(
    fitted,
    units,
    embeddings = unit_embeddings,
    n = 1,
    rank = "distance"
  )
  testthat::expect_identical(nrow(by_distance), 2L)
  testthat::expect_true(all(by_distance$rank == 1L))
})

testthat::test_that("representative ties break toward the shorter unit", {
  fitted <- fit_toy_model()
  fruit_topic <- fitted$documents$topic[[1L]]
  units <- c("a much longer fruit statement here", "short fruit")
  unit_embeddings <- rbind(c(1, 0), c(1, 0))

  result <- representatives(fitted, units, embeddings = unit_embeddings, n = 2)
  fruit_rows <- subset(result, topic == fruit_topic)
  testthat::expect_identical(fruit_rows$text[[1L]], "short fruit")
})
