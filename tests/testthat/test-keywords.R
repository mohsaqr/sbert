# A deterministic toy embedder: known words (and their bigrams) map onto a
# 2-dimensional animal/finance plane, so relevance rankings are hand-checkable.
keyword_lookup_embedder <- function(texts) {
  animal_words <- c("cats", "chase", "mice", "kittens")
  finance_words <- c("stocks", "bonds", "trade", "markets")
  do.call(rbind, lapply(
    strsplit(tolower(texts), " ", fixed = TRUE),
    function(tokens) {
      animal_hits <- sum(tokens %in% animal_words)
      finance_hits <- sum(tokens %in% finance_words)
      if (animal_hits + finance_hits == 0L) {
        c(1, 1) / sqrt(2)
      } else {
        vector_sum <- c(animal_hits, finance_hits)
        vector_sum / sqrt(sum(vector_sum^2))
      }
    }
  ))
}

with_keyword_mock <- function(expression) {
  testthat::local_mocked_bindings(
    encode_keyword_texts = function(texts, model, batch_size) {
      keyword_lookup_embedder(texts)
    },
    .package = "sbert",
    .env = parent.frame()
  )
  expression
}

testthat::test_that("keywords are ranked by topic_similarity to the document", {
  result <- with_keyword_mock(
    keywords(
      c(pets = "Cats chase mice daily", money = "Stocks and bonds trade"),
      n = 3,
      ngrams = 1,
      topic_diversity = 0
    )
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(
    names(result),
    c("document_id", "document_name", "rank", "keyword", "topic_similarity")
  )
  pets <- subset(result, document_name == "pets")
  money <- subset(result, document_name == "money")
  # Pure animal words dominate the animal document; "daily" (neutral) ranks
  # below them.
  testthat::expect_true(all(c("cats", "chase", "mice") %in% pets$keyword))
  testthat::expect_true(all(c("stocks", "bonds", "trade") %in% money$keyword))
  testthat::expect_identical(pets$rank, 1:3)
  testthat::expect_true(all(diff(pets$topic_similarity) <= 0))
})

testthat::test_that("bigram candidates are generated and rankable", {
  result <- with_keyword_mock(
    keywords(
      "Cats chase mice",
      n = 10,
      ngrams = 2,
      topic_diversity = 0
    )
  )
  testthat::expect_true("cats chase" %in% result$keyword)
  testthat::expect_true("chase mice" %in% result$keyword)
  # A two-hit bigram is closer to the all-animal document than any single
  # word mixed with direction (all are pure animal here, so bigrams tie with
  # unigrams at topic_similarity 1 and alphabetical order breaks the tie).
  testthat::expect_equal(max(result$topic_similarity), 1, tolerance = 1e-12)
})

testthat::test_that("topic_diversity penalizes redundant keywords", {
  plain <- with_keyword_mock(
    keywords("Cats chase mice with stocks", n = 2, ngrams = 1, topic_diversity = 0)
  )
  diverse <- with_keyword_mock(
    keywords("Cats chase mice with stocks", n = 2, ngrams = 1, topic_diversity = 0.9)
  )
  # Without topic_diversity both picks are animal words; with strong topic_diversity the
  # second pick flips to the finance word.
  testthat::expect_false("stocks" %in% plain$keyword)
  testthat::expect_true("stocks" %in% diverse$keyword)
})

testthat::test_that("extraction is deterministic", {
  first <- with_keyword_mock(
    keywords("Cats chase mice daily", n = 4, topic_diversity = 0.3)
  )
  second <- with_keyword_mock(
    keywords("Cats chase mice daily", n = 4, topic_diversity = 0.3)
  )
  testthat::expect_identical(first, second)
})

testthat::test_that("stop_words and short tokens are excluded from candidates", {
  result <- with_keyword_mock(
    keywords(
      "The cats of the market do chase mice",
      n = 10,
      ngrams = 1,
      topic_diversity = 0,
      stop_words = stop_words(add = "market")
    )
  )
  testthat::expect_false("the" %in% result$keyword)
  testthat::expect_false("of" %in% result$keyword)
  testthat::expect_false("do" %in% result$keyword)
  testthat::expect_false("market" %in% result$keyword)
})

testthat::test_that("invalid inputs are rejected", {
  testthat::expect_error(keywords(character(0)))
  testthat::expect_error(keywords(NA_character_))
  testthat::expect_error(keywords("  "))
  testthat::expect_error(keywords("fine text", n = 0))
  testthat::expect_error(keywords("fine text", topic_diversity = 1))
  testthat::expect_error(keywords("fine text", ngrams = 0))
  testthat::expect_error(
    with_keyword_mock(
      keywords("of the and", ngrams = 1)
    ),
    "candidates"
  )
})
