segment_units <- function(text, level, ...) {
  sbert_segment(text, level = level, ...)$text
}

# ---- Gold cases: input, level, expected segments ---------------------------
segmentation_gold_cases <- list(
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
  # clause splits at the hinge, list stays whole
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

testthat::test_that("gold segmentations are exact", {
  for (case in segmentation_gold_cases) {
    testthat::expect_identical(
      segment_units(case[[1L]], case[[2L]]),
      case[[3L]],
      info = case[[1L]]
    )
  }
})

testthat::test_that("mixed-case text segments identically with case preserved", {
  testthat::expect_identical(
    segment_units(
      "Saving time, space and cost. More and more universities are here.",
      "sentence"
    ),
    c("Saving time, space and cost.", "More and more universities are here.")
  )
  testthat::expect_identical(
    segment_units("See Fig. 3 for details. The next part follows.", "sentence"),
    c("See Fig. 3 for details.", "The next part follows.")
  )
  testthat::expect_identical(
    segment_units(
      "We propose a simulator which runs alongside the processor.",
      "clause"
    ),
    c("We propose a simulator", "which runs alongside the processor.")
  )
  testthat::expect_identical(
    segment_units(
      "We use a language (e.g., Python, Ruby) here, then stop.",
      "phrase"
    ),
    c("We use a language (e.g., Python, Ruby) here,", "then stop.")
  )
})

testthat::test_that("normalization straightens curly quotes and spaces dashes", {
  testthat::expect_identical(
    segment_units("It’s fast—it works. The end.", "sentence"),
    c("It's fast - it works.", "The end.")
  )
})

testthat::test_that("result is a tidy one-row-per-segment data frame", {
  result <- sbert_segment(
    c(a = "One sentence here. Another sentence there.", b = "A single unit."),
    level = "sentence"
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(
    names(result),
    c("document_id", "document_name", "segment", "text")
  )
  testthat::expect_identical(result$document_id, c(1L, 1L, 2L))
  testthat::expect_identical(result$document_name, c("a", "a", "b"))
  testthat::expect_identical(result$segment, c(1L, 2L, 1L))
  testthat::expect_identical(
    result$text,
    c("One sentence here.", "Another sentence there.", "A single unit.")
  )
})

testthat::test_that("unnamed input yields empty document names", {
  result <- sbert_segment("Only one unit.")
  testthat::expect_identical(result$document_name, "")
  testthat::expect_identical(result$document_id, 1L)
})

testthat::test_that("blank documents contribute no rows but keep numbering", {
  result <- sbert_segment(c("", "   ", "A real unit."), level = "clause")
  testthat::expect_identical(nrow(result), 1L)
  testthat::expect_identical(result$document_id, 3L)

  empty <- sbert_segment("", level = "sentence")
  testthat::expect_identical(nrow(empty), 0L)
  testthat::expect_identical(
    names(empty),
    c("document_id", "document_name", "segment", "text")
  )
  testthat::expect_type(empty$text, "character")
})

testthat::test_that("merge_below re-joins sub-threshold shards at phrase level", {
  # Phrase level is deliberately aggressive; merge_below re-joins short shards
  # but cannot perfectly reconstruct a list whose final item runs into a longer
  # clause -- the list-comma / clause-comma ambiguity is unsolvable without
  # parsing. Enumeration integrity is instead guaranteed at CLAUSE level.
  testthat::expect_identical(
    segment_units(
      "IT COVERS A, B, AND C IN ONE TOOL.",
      "phrase",
      merge_below = 3L
    ),
    c("IT COVERS A, B,", "AND C IN ONE TOOL.")
  )
  # a purely short list collapses fully
  testthat::expect_identical(
    segment_units(
      "TESTS, GRADING, FEEDBACK, EXAMS.",
      "phrase",
      merge_below = 4L
    ),
    "TESTS, GRADING, FEEDBACK, EXAMS."
  )
})

testthat::test_that("edge cases are handled", {
  testthat::expect_identical(segment_units("ONEWORD", "sentence"), "ONEWORD")
  testthat::expect_identical(
    segment_units("NO TERMINAL PUNCTUATION HERE", "sentence"),
    "NO TERMINAL PUNCTUATION HERE"
  )
})

testthat::test_that("segmentation is deterministic and content-preserving", {
  sample_text <- paste(
    "WE BUILT A WEB-BASED TOOL; IT SUPPORTS ONLINE AND OFFLINE USE.",
    "IN TERMS OF SPEED, IT IS 2.5 TIMES FASTER (SEE FIG. 4)."
  )
  tokens <- function(x) {
    sort(unlist(regmatches(x, gregexpr("[[:alnum:]]+", x))))
  }
  for (level in c("sentence", "clause", "phrase")) {
    first <- segment_units(sample_text, level)
    testthat::expect_identical(first, segment_units(sample_text, level))
    testthat::expect_false(any(grepl("@@", first, fixed = TRUE)))
    testthat::expect_true(all(nzchar(first)))
    testthat::expect_identical(tokens(first), tokens(sample_text))
  }
})

testthat::test_that("ETC. ends sentences and never glues them", {
  # Boundaries only fire at "period space word"; mid-sentence etc. carries a
  # comma, so the list case is untouched and the sentence end must split.
  testthat::expect_identical(
    segment_units(
      "SUBJECTS SUCH AS SE, HCI, DATABASE, ETC. HOWEVER, STUDIES SHOW GAPS.",
      "sentence"
    ),
    c(
      "SUBJECTS SUCH AS SE, HCI, DATABASE, ETC.",
      "HOWEVER, STUDIES SHOW GAPS."
    )
  )
  testthat::expect_identical(
    segment_units("TESTS, GRADING, ETC., ARE ALL COVERED HERE.", "sentence"),
    "TESTS, GRADING, ETC., ARE ALL COVERED HERE."
  )
  testthat::expect_false("ETC." %in% sbert_abbreviations())
})

testthat::test_that("custom abbreviations extend the built-in guard", {
  text <- "THE CIRCA. 1900 SAMPLE IS OLD. IT STILL WORKS."
  testthat::expect_identical(
    segment_units(text, "sentence"),
    c("THE CIRCA.", "1900 SAMPLE IS OLD.", "IT STILL WORKS.")
  )
  testthat::expect_identical(
    segment_units(
      text,
      "sentence",
      abbreviations = c(sbert_abbreviations(), "CIRCA.")
    ),
    c("THE CIRCA. 1900 SAMPLE IS OLD.", "IT STILL WORKS.")
  )
})

testthat::test_that("sbert_abbreviations returns the sorted gazetteer", {
  gazetteer <- sbert_abbreviations()
  testthat::expect_type(gazetteer, "character")
  testthat::expect_identical(gazetteer, sort(unique(gazetteer)))
  testthat::expect_true(all(endsWith(gazetteer, ".")))
  testthat::expect_true(all(c("E.G.", "FIG.", "NO.", "ST.") %in% gazetteer))
})

testthat::test_that("invalid inputs are rejected", {
  testthat::expect_error(sbert_segment(character(0)))
  testthat::expect_error(sbert_segment(NA_character_))
  testthat::expect_error(sbert_segment(42))
  testthat::expect_error(sbert_segment("Fine text.", level = "paragraph"))
  testthat::expect_error(sbert_segment("Fine text.", merge_below = -1))
  testthat::expect_error(sbert_segment("Fine text.", merge_below = 1.5))
  testthat::expect_error(sbert_segment("Fine text.", abbreviations = 1L))
})
