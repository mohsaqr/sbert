library(testthat)
source(file.path("..", "segment_text.R"), chdir = TRUE)

# ---- Gold cases: input, level, expected segments ---------------------------
gold <- list(
  # abbreviation vs. word-ending ("ST." must not glue COST. to the next sentence)
  list(
    "SAVING TIME, SPACE AND COST. MORE AND MORE UNIVERSITIES ARE HERE.",
    "sentence",
    c("SAVING TIME, SPACE AND COST.", "MORE AND MORE UNIVERSITIES ARE HERE.")
  ),
  list(
    "THE TEST. THE MOST. THE LIST. EACH ONE IS SEPARATE.",
    "sentence",
    c("THE TEST.", "THE MOST.", "THE LIST.", "EACH ONE IS SEPARATE.")
  ),
  # real abbreviation + trailing digit must stay in one sentence
  list(
    "SEE FIG. 3 FOR DETAILS. THE NEXT PART FOLLOWS.",
    "sentence",
    c("SEE FIG. 3 FOR DETAILS.", "THE NEXT PART FOLLOWS.")
  ),
  # "NO." (number) protected; "CASINO." is not falsely protected
  list(
    "REFERENCE NO. 5 IS CITED. THE CASINO. IT ALL WORKS.",
    "sentence",
    c("REFERENCE NO. 5 IS CITED.", "THE CASINO.", "IT ALL WORKS.")
  ),
  # single-letter end "I/O." must still split
  list(
    "IT SUPPORTS ASYNCHRONOUS I/O. THE HARDWARE IS REPORTED.",
    "sentence",
    c("IT SUPPORTS ASYNCHRONOUS I/O.", "THE HARDWARE IS REPORTED.")
  ),
  # decimals never split
  list(
    "ACCURACY REACHED 3.14 PERCENT OVERALL. IT THEN ROSE.",
    "sentence",
    c("ACCURACY REACHED 3.14 PERCENT OVERALL.", "IT THEN ROSE.")
  ),
  # the flagged sentence: clause splits at the hinge, list stays whole
  list(
    paste(
      "IN THIS PAPER, WE EXPLORE WHAT IS TERMED DISTANCE LEARNING,",
      "ASYNCHRONOUS, ONLINE, AND WEB-SUPPORTED LEARNING IN TERMS OF",
      "HOW IT CAN IMPROVE EDUCATION."
    ),
    "clause",
    c(
      paste(
        "IN THIS PAPER, WE EXPLORE WHAT IS TERMED DISTANCE LEARNING,",
        "ASYNCHRONOUS, ONLINE, AND WEB-SUPPORTED LEARNING"
      ),
      "IN TERMS OF HOW IT CAN IMPROVE EDUCATION."
    )
  ),
  # same sentence at SENTENCE level is one unit (no hinge split)
  list(
    paste(
      "IN THIS PAPER, WE EXPLORE WHAT IS TERMED DISTANCE LEARNING,",
      "ASYNCHRONOUS, ONLINE, AND WEB-SUPPORTED LEARNING IN TERMS OF",
      "HOW IT CAN IMPROVE EDUCATION."
    ),
    "sentence",
    paste(
      "IN THIS PAPER, WE EXPLORE WHAT IS TERMED DISTANCE LEARNING,",
      "ASYNCHRONOUS, ONLINE, AND WEB-SUPPORTED LEARNING IN TERMS OF",
      "HOW IT CAN IMPROVE EDUCATION."
    )
  ),
  # clause split on a relative "WHICH"
  list(
    "WE PROPOSE A SIMULATOR WHICH RUNS ALONGSIDE THE PROCESSOR.",
    "clause",
    c("WE PROPOSE A SIMULATOR", "WHICH RUNS ALONGSIDE THE PROCESSOR.")
  ),
  # clause split on semicolon and colon
  list(
    "WE HAD TWO GOALS: SPEED AND CLARITY; BOTH WERE MET.",
    "clause",
    c("WE HAD TWO GOALS:", "SPEED AND CLARITY;", "BOTH WERE MET.")
  ),
  # clause keeps an enumeration whole (no comma split at clause level)
  list(
    "THE TOOL COVERS TESTS, GRADING, AND FEEDBACK.",
    "clause",
    "THE TOOL COVERS TESTS, GRADING, AND FEEDBACK."
  ),
  # phrase splits the same enumeration
  list(
    "THE TOOL COVERS TESTS, GRADING, AND FEEDBACK.",
    "phrase",
    c("THE TOOL COVERS TESTS,", "GRADING,", "AND FEEDBACK.")
  ),
  # parenthetical comma/period never split (even at phrase level)
  list(
    "WE USE A LANGUAGE (E.G., PYTHON, RUBY) HERE, THEN STOP.",
    "phrase",
    c("WE USE A LANGUAGE (E.G., PYTHON, RUBY) HERE,", "THEN STOP.")
  )
)

test_that("gold segmentations are exact", {
  for (case in gold) {
    expect_identical(
      segment_text(case[[1L]], level = case[[2L]]),
      case[[3L]],
      info = case[[1L]]
    )
  }
})

test_that("merge_below re-joins sub-threshold shards at phrase level", {
  # Phrase level is deliberately aggressive; merge_below re-joins short shards
  # ("B,") but cannot perfectly reconstruct a list whose final item runs into a
  # longer clause -- the list-comma / clause-comma ambiguity is unsolvable
  # without parsing. Enumeration integrity is instead guaranteed at CLAUSE level
  # (which never splits commas -- see the enumeration gold case above).
  expect_identical(
    segment_text("IT COVERS A, B, AND C IN ONE TOOL.", "phrase", merge_below = 3L),
    c("IT COVERS A, B,", "AND C IN ONE TOOL.")
  )
  # a purely short list collapses fully
  expect_identical(
    segment_text("TESTS, GRADING, FEEDBACK, EXAMS.", "phrase", merge_below = 4L),
    "TESTS, GRADING, FEEDBACK, EXAMS."
  )
})

test_that("edge cases are handled", {
  expect_identical(segment_text("", "sentence"), character(0))
  expect_identical(segment_text("   ", "clause"), character(0))
  expect_identical(segment_text("ONEWORD", "sentence"), "ONEWORD")
  expect_identical(
    segment_text("NO TERMINAL PUNCTUATION HERE", "sentence"),
    "NO TERMINAL PUNCTUATION HERE"
  )
})

test_that("segmentation is deterministic and content-preserving", {
  sample <- paste(
    "WE BUILT A WEB-BASED TOOL; IT SUPPORTS ONLINE AND OFFLINE USE.",
    "IN TERMS OF SPEED, IT IS 2.5 TIMES FASTER (SEE FIG. 4)."
  )
  for (lvl in c("sentence", "clause", "phrase")) {
    first <- segment_text(sample, lvl)
    expect_identical(first, segment_text(sample, lvl))                 # deterministic
    expect_false(any(grepl("@@", first, fixed = TRUE)))               # sentinels gone
    expect_true(all(nzchar(first)))                                    # no empties
    tokens <- function(x) sort(unlist(regmatches(x, gregexpr("[[:alnum:]]+", x))))
    expect_identical(tokens(first), tokens(seg_normalize(sample)))     # no lost words
  }
})
