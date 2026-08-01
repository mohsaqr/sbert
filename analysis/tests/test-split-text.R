library(testthat)
source(file.path("..", "segment_text.R"))
source(file.path("..", "split_text.R"))

sample_text <- paste(
  "In this paper, we explore online learning.",
  "It has two goals: speed and clarity; both were met.",
  "Our approach improves retrieval accuracy over the baseline."
)

test_that("dispatch routes linguistic levels to segment_text", {
  expect_identical(split_text(sample_text, "sentence"),
                   segment_text(sample_text, "sentence"))
  expect_identical(split_text(sample_text, "clause"),
                   segment_text(sample_text, "clause"))
})

test_that("token_window respects size and overlap", {
  words <- strsplit(trimws(sample_text), "\\s+")[[1L]]
  windows <- split_text(sample_text, "token_window", size = 10L, overlap = 3L)
  # every window has at most `size` words; consecutive windows share `overlap`
  expect_true(all(lengths(strsplit(windows, "\\s+")) <= 10L))
  first_tail <- utils::tail(strsplit(windows[[1L]], " ")[[1L]], 3L)
  second_head <- utils::head(strsplit(windows[[2L]], " ")[[1L]], 3L)
  expect_identical(first_tail, second_head)
  # union of windows covers all words in order (ignoring overlap)
  expect_true(all(words %in% unlist(strsplit(windows, "\\s+"))))
})

test_that("recursive keeps chunks within the size budget", {
  chunks <- split_text(sample_text, "recursive", chunk_size = 60L, overlap = 10L)
  expect_true(all(nchar(chunks) <= 60L + 15L))  # size + one overlap tail
  expect_true(length(chunks) >= 2L)
})

test_that("semantic groups similar adjacent units and splits at troughs", {
  encoder <- function(x) {
    m <- t(vapply(x, function(u) {
      c(grepl("GOAL|SPEED|CLARITY|MET", u),
        grepl("RETRIEVAL|ACCURACY|BASELINE", u),
        grepl("ONLINE|LEARNING|PAPER", u)) + 0.01
    }, numeric(3L)))
    m / sqrt(rowSums(m^2))
  }
  segments <- split_text(sample_text, "semantic", encode = encoder)
  expect_true(length(segments) >= 2L && length(segments) <= 3L)
  expect_true(any(grepl("SPEED AND CLARITY", segments)))
})

test_that("invalid method and input are rejected", {
  expect_error(split_text(sample_text, "regex"))
  expect_error(split_text(c("a", "b"), "clause"))
})
