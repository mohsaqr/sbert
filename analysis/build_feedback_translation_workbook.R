stopifnot(requireNamespace("openxlsx", quietly = TRUE))

output_directory <- file.path("outputs", "feedback_translation_topics")
model_path <- file.path(
  output_directory,
  "feedback_translation_topic_model.rds"
)
workbook_path <- file.path(
  output_directory,
  "feedback_translation_topic_model.xlsx"
)
chart_path <- file.path(output_directory, "topic_size_chart.png")

stopifnot(file.exists(model_path))
analysis_result <- readRDS(model_path)
assignments <- analysis_result$assignments
topics <- analysis_result$topic_summary
terms <- analysis_result$topic_terms
representatives <- analysis_result$representatives
diagnostics <- analysis_result$topic_count_diagnostics
data_quality <- analysis_result$data_quality
repeated_translations <- analysis_result$repeated_translations
method <- analysis_result$method

str(assignments)
print(dim(assignments))
print(head(assignments))
str(topics)
print(topics)

grDevices::png(chart_path, width = 1800, height = 1000, res = 180)
graphics::par(mar = c(5, 12, 3, 1), family = "sans")
topic_chart_color <- ifelse(
  topics$topic %in% c(12L, 15L),
  "#D99032",
  "#2D6A9F"
)
graphics::barplot(
  rev(topics$n_rows),
  names.arg = rev(sprintf("T%d  %s", topics$topic, topics$topic_label)),
  horiz = TRUE,
  las = 1,
  col = rev(topic_chart_color),
  border = NA,
  xlab = "Feedback rows",
  main = "Topic prevalence"
)
grDevices::dev.off()

workbook <- openxlsx::createWorkbook(
  creator = "Mohammed Saqr",
  title = "Feedback Translation Topic Model",
  subject = "Python-free Sentence-BERT topic modeling"
)
sheet_names <- c(
  "Dashboard",
  "Topic Summary",
  "Documents",
  "Topic Terms",
  "Representatives",
  "Model Selection",
  "Data Quality",
  "Repeated Text",
  "Method"
)
invisible(lapply(
  sheet_names,
  function(sheet_name) {
    openxlsx::addWorksheet(
      workbook,
      sheet_name,
      gridLines = FALSE,
      tabColour = if (identical(sheet_name, "Dashboard")) {
        "#17365D"
      } else {
        "#5B9BD5"
      }
    )
  }
))

title_style <- openxlsx::createStyle(
  fontName = "Aptos Display",
  fontSize = 20,
  fontColour = "#FFFFFF",
  textDecoration = "bold",
  halign = "left",
  valign = "center",
  fgFill = "#17365D"
)
subtitle_style <- openxlsx::createStyle(
  fontName = "Aptos",
  fontSize = 10,
  fontColour = "#44546A",
  valign = "center"
)
section_style <- openxlsx::createStyle(
  fontName = "Aptos Display",
  fontSize = 12,
  fontColour = "#FFFFFF",
  textDecoration = "bold",
  fgFill = "#2D6A9F",
  valign = "center"
)
kpi_label_style <- openxlsx::createStyle(
  fontName = "Aptos",
  fontSize = 9,
  fontColour = "#5B6B7C",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fgFill = "#F3F7FB",
  border = "TopBottomLeftRight",
  borderColour = "#D9E2EC"
)
kpi_value_style <- openxlsx::createStyle(
  fontName = "Aptos Display",
  fontSize = 20,
  fontColour = "#17365D",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fgFill = "#F3F7FB",
  border = "TopBottomLeftRight",
  borderColour = "#D9E2EC"
)
note_style <- openxlsx::createStyle(
  fontName = "Aptos",
  fontSize = 10,
  fontColour = "#5B4A00",
  fgFill = "#FFF7DF",
  wrapText = TRUE,
  valign = "top",
  border = "TopBottomLeftRight",
  borderColour = "#E3A008"
)
percentage_style <- openxlsx::createStyle(numFmt = "0.0%")
distance_style <- openxlsx::createStyle(numFmt = "0.000")
score_style <- openxlsx::createStyle(numFmt = "0.000000")
wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
flag_style <- openxlsx::createStyle(
  fgFill = "#FFF2CC",
  fontColour = "#7F6000",
  textDecoration = "bold"
)
selected_style <- openxlsx::createStyle(
  fgFill = "#E2F0D9",
  fontColour = "#375623",
  textDecoration = "bold"
)

openxlsx::mergeCells(workbook, "Dashboard", cols = 1:16, rows = 1:2)
openxlsx::writeData(
  workbook,
  "Dashboard",
  "Feedback Translation Topic Model",
  startCol = 1,
  startRow = 1
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  title_style,
  rows = 1:2,
  cols = 1:16,
  gridExpand = TRUE
)
openxlsx::mergeCells(workbook, "Dashboard", cols = 1:16, rows = 3)
openxlsx::writeData(
  workbook,
  "Dashboard",
  paste0(
    "Exploratory Python-free Sentence-BERT analysis of the supplied translation field · ",
    format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  ),
  startCol = 1,
  startRow = 3
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  subtitle_style,
  rows = 3,
  cols = 1:16,
  gridExpand = TRUE
)

kpi_labels <- c(
  "SOURCE ROWS",
  "MODELED ROWS",
  "DISTINCT TRANSLATIONS",
  "EXPLORATORY TOPICS"
)
kpi_values <- c(
  nrow(assignments),
  sum(assignments$included),
  length(unique(assignments$translation[assignments$included])),
  nrow(topics)
)
kpi_start_columns <- c(1L, 5L, 9L, 13L)
invisible(lapply(
  seq_along(kpi_start_columns),
  function(kpi_id) {
    start_column <- kpi_start_columns[[kpi_id]]
    openxlsx::mergeCells(
      workbook,
      "Dashboard",
      cols = start_column:(start_column + 2L),
      rows = 5
    )
    openxlsx::mergeCells(
      workbook,
      "Dashboard",
      cols = start_column:(start_column + 2L),
      rows = 6:7
    )
    openxlsx::writeData(
      workbook,
      "Dashboard",
      kpi_labels[[kpi_id]],
      startCol = start_column,
      startRow = 5
    )
    openxlsx::writeData(
      workbook,
      "Dashboard",
      kpi_values[[kpi_id]],
      startCol = start_column,
      startRow = 6
    )
    openxlsx::addStyle(
      workbook,
      "Dashboard",
      kpi_label_style,
      rows = 5,
      cols = start_column:(start_column + 2L),
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      workbook,
      "Dashboard",
      kpi_value_style,
      rows = 6:7,
      cols = start_column:(start_column + 2L),
      gridExpand = TRUE
    )
  }
))

openxlsx::mergeCells(workbook, "Dashboard", cols = 1:16, rows = 9)
openxlsx::writeData(
  workbook,
  "Dashboard",
  "Topic overview",
  startCol = 1,
  startRow = 9
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  section_style,
  rows = 9,
  cols = 1:16,
  gridExpand = TRUE
)
dashboard_topics <- topics[
  ,
  c(
    "topic",
    "topic_label",
    "n_rows",
    "proportion",
    "median_cosine_distance",
    "top_terms"
  )
]
names(dashboard_topics) <- c(
  "Topic",
  "Automatic label",
  "Rows",
  "Share",
  "Median distance",
  "Top terms"
)
openxlsx::writeDataTable(
  workbook,
  "Dashboard",
  dashboard_topics,
  startCol = 1,
  startRow = 11,
  tableName = "DashboardTopics",
  tableStyle = "TableStyleMedium2"
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  percentage_style,
  rows = 12:(11L + nrow(dashboard_topics)),
  cols = 4,
  gridExpand = TRUE
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  distance_style,
  rows = 12:(11L + nrow(dashboard_topics)),
  cols = 5,
  gridExpand = TRUE
)
dashboard_bars <- data.frame(
  Topic = sprintf("T%d: %s", topics$topic, topics$topic_label),
  Rows = topics$n_rows,
  Share = topics$proportion,
  Prevalence = vapply(
    topics$n_rows,
    function(topic_size) {
      paste(rep.int("█", max(1L, round(28 * topic_size / max(topics$n_rows)))), collapse = "")
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)
openxlsx::writeDataTable(
  workbook,
  "Dashboard",
  dashboard_bars,
  startCol = 8,
  startRow = 11,
  tableName = "DashboardTopicBars",
  tableStyle = "TableStyleMedium2"
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  percentage_style,
  rows = 12:(11L + nrow(dashboard_bars)),
  cols = 10,
  gridExpand = TRUE
)
openxlsx::setColWidths(workbook, "Dashboard", cols = 8, widths = 28)
openxlsx::setColWidths(workbook, "Dashboard", cols = 9:10, widths = 11)
openxlsx::setColWidths(workbook, "Dashboard", cols = 11, widths = 34)
openxlsx::mergeCells(workbook, "Dashboard", cols = 1:16, rows = 29:32)
openxlsx::writeData(
  workbook,
  "Dashboard",
  paste0(
    "Interpretation note: topic IDs are arbitrary and automatic labels require substantive review. ",
    "Cosine distance is proximity to the assigned centroid, not a probability. The supplied ",
    "translation field was used as-is; 179 rows carry a conservative translation-review flag, ",
    "including 49 with Cyrillic and 14 with accented non-ASCII Latin characters."
  ),
  startCol = 1,
  startRow = 29
)
openxlsx::addStyle(
  workbook,
  "Dashboard",
  note_style,
  rows = 29:32,
  cols = 1:16,
  gridExpand = TRUE
)
openxlsx::setColWidths(workbook, "Dashboard", cols = 1, widths = 8)
openxlsx::setColWidths(workbook, "Dashboard", cols = 2, widths = 25)
openxlsx::setColWidths(workbook, "Dashboard", cols = 3:5, widths = 12)
openxlsx::setColWidths(workbook, "Dashboard", cols = 6, widths = 38)
openxlsx::setColWidths(workbook, "Dashboard", cols = 7:16, widths = 11)
openxlsx::setRowHeights(workbook, "Dashboard", rows = 1:2, heights = 25)
openxlsx::setRowHeights(workbook, "Dashboard", rows = 6:7, heights = 23)
openxlsx::setRowHeights(workbook, "Dashboard", rows = 29:32, heights = 21)
openxlsx::freezePane(workbook, "Dashboard", firstActiveRow = 11)

sheet_data <- list(
  "Topic Summary" = topics,
  "Documents" = assignments,
  "Topic Terms" = terms,
  "Representatives" = representatives,
  "Model Selection" = diagnostics,
  "Repeated Text" = repeated_translations,
  "Method" = method
)
sheet_table_names <- c(
  "TopicSummaryTable",
  "DocumentAssignmentsTable",
  "TopicTermsTable",
  "RepresentativeTable",
  "ModelSelectionTable",
  "RepeatedTextTable",
  "MethodTable"
)
invisible(lapply(
  seq_along(sheet_data),
  function(sheet_id) {
    sheet_name <- names(sheet_data)[[sheet_id]]
    data <- sheet_data[[sheet_id]]
    openxlsx::mergeCells(
      workbook,
      sheet_name,
      cols = seq_len(max(2L, ncol(data))),
      rows = 1
    )
    openxlsx::writeData(
      workbook,
      sheet_name,
      sheet_name,
      startCol = 1,
      startRow = 1
    )
    openxlsx::addStyle(
      workbook,
      sheet_name,
      title_style,
      rows = 1,
      cols = seq_len(max(2L, ncol(data))),
      gridExpand = TRUE
    )
    openxlsx::writeDataTable(
      workbook,
      sheet_name,
      data,
      startCol = 1,
      startRow = 3,
      tableName = sheet_table_names[[sheet_id]],
      tableStyle = "TableStyleMedium2"
    )
    openxlsx::freezePane(
      workbook,
      sheet_name,
      firstActiveRow = 4,
      firstActiveCol = 2
    )
    openxlsx::setRowHeights(workbook, sheet_name, rows = 1, heights = 28)
  }
))

openxlsx::setColWidths(
  workbook,
  "Topic Summary",
  cols = 1:15,
  widths = c(8, 27, 11, 11, 12, 14, 14, 14, 14, 14, 14, 38, 44, 44, 44)
)
openxlsx::addStyle(
  workbook,
  "Topic Summary",
  percentage_style,
  rows = 4:(3L + nrow(topics)),
  cols = c(4, 11),
  gridExpand = TRUE
)
openxlsx::addStyle(
  workbook,
  "Topic Summary",
  distance_style,
  rows = 4:(3L + nrow(topics)),
  cols = 8:10,
  gridExpand = TRUE
)
openxlsx::addStyle(
  workbook,
  "Topic Summary",
  wrap_style,
  rows = 4:(3L + nrow(topics)),
  cols = 12:15,
  gridExpand = TRUE,
  stack = TRUE
)

openxlsx::setColWidths(
  workbook,
  "Documents",
  cols = 1:16,
  widths = c(9, 42, 42, 10, 18, 12, 12, 14, 14, 14, 11, 8, 28, 14, 13, 13)
)
openxlsx::addStyle(
  workbook,
  "Documents",
  distance_style,
  rows = 4:(3L + nrow(assignments)),
  cols = 14,
  gridExpand = TRUE
)
openxlsx::conditionalFormatting(
  workbook,
  "Documents",
  cols = 10,
  rows = 4:(3L + nrow(assignments)),
  rule = "==TRUE",
  style = flag_style,
  type = "expression"
)

openxlsx::setColWidths(
  workbook,
  "Topic Terms",
  cols = 1:6,
  widths = c(8, 28, 8, 22, 17, 21)
)
openxlsx::addStyle(
  workbook,
  "Topic Terms",
  score_style,
  rows = 4:(3L + nrow(terms)),
  cols = 5,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  workbook,
  "Representatives",
  cols = 1:8,
  widths = c(8, 28, 8, 12, 10, 44, 52, 18)
)
openxlsx::addStyle(
  workbook,
  "Representatives",
  distance_style,
  rows = 4:(3L + nrow(representatives)),
  cols = 8,
  gridExpand = TRUE
)
openxlsx::addStyle(
  workbook,
  "Representatives",
  wrap_style,
  rows = 4:(3L + nrow(representatives)),
  cols = 6:7,
  gridExpand = TRUE,
  stack = TRUE
)

openxlsx::setColWidths(
  workbook,
  "Model Selection",
  cols = 1:13,
  widths = c(10, 18, 18, 18, 14, 16, 14, 18, 18, 11, 15, 10, 70)
)
openxlsx::conditionalFormatting(
  workbook,
  "Model Selection",
  cols = 12,
  rows = 4:(3L + nrow(diagnostics)),
  rule = "==TRUE",
  style = selected_style,
  type = "expression"
)

openxlsx::setColWidths(
  workbook,
  "Repeated Text",
  cols = 1:2,
  widths = c(70, 12)
)
openxlsx::setColWidths(
  workbook,
  "Method",
  cols = 1:2,
  widths = c(28, 90)
)
openxlsx::addStyle(
  workbook,
  "Method",
  wrap_style,
  rows = 4:(3L + nrow(method)),
  cols = 2,
  gridExpand = TRUE,
  stack = TRUE
)

openxlsx::mergeCells(workbook, "Data Quality", cols = 1:6, rows = 1)
openxlsx::writeData(
  workbook,
  "Data Quality",
  "Data Quality",
  startCol = 1,
  startRow = 1
)
openxlsx::addStyle(
  workbook,
  "Data Quality",
  title_style,
  rows = 1,
  cols = 1:6,
  gridExpand = TRUE
)
openxlsx::writeDataTable(
  workbook,
  "Data Quality",
  data_quality,
  startCol = 1,
  startRow = 3,
  tableName = "DataQualityTable",
  tableStyle = "TableStyleMedium2"
)
excluded_rows <- assignments[
  !assignments$included,
  c("row_id", "feedback", "translation", "exclusion_reason"),
  drop = FALSE
]
flagged_rows <- assignments[
  assignments$translation_review_flag,
  c(
    "row_id",
    "feedback",
    "translation",
    "translation_has_cyrillic",
    "translation_has_non_ascii_latin",
    "source_equals_translation",
    "topic"
  ),
  drop = FALSE
]
excluded_start <- 6L + nrow(data_quality)
flagged_start <- excluded_start + nrow(excluded_rows) + 5L
openxlsx::writeData(
  workbook,
  "Data Quality",
  "Excluded blank rows",
  startCol = 1,
  startRow = excluded_start
)
openxlsx::addStyle(
  workbook,
  "Data Quality",
  section_style,
  rows = excluded_start,
  cols = 1:6,
  gridExpand = TRUE
)
openxlsx::writeDataTable(
  workbook,
  "Data Quality",
  excluded_rows,
  startCol = 1,
  startRow = excluded_start + 2L,
  tableName = "ExcludedRowsTable",
  tableStyle = "TableStyleMedium2"
)
openxlsx::writeData(
  workbook,
  "Data Quality",
  "Rows flagged for translation review",
  startCol = 1,
  startRow = flagged_start
)
openxlsx::addStyle(
  workbook,
  "Data Quality",
  section_style,
  rows = flagged_start,
  cols = 1:7,
  gridExpand = TRUE
)
openxlsx::writeDataTable(
  workbook,
  "Data Quality",
  flagged_rows,
  startCol = 1,
  startRow = flagged_start + 2L,
  tableName = "TranslationReviewTable",
  tableStyle = "TableStyleMedium2"
)
openxlsx::setColWidths(
  workbook,
  "Data Quality",
  cols = 1:7,
  widths = c(10, 46, 52, 15, 18, 15, 10)
)
openxlsx::freezePane(workbook, "Data Quality", firstActiveRow = 4)

openxlsx::saveWorkbook(workbook, workbook_path, overwrite = TRUE)

stopifnot(file.exists(workbook_path), file.info(workbook_path)$size > 0)
sheet_names_written <- openxlsx::getSheetNames(workbook_path)
print(sheet_names_written)
stopifnot(identical(sheet_names_written, sheet_names))
workbook_tables <- lapply(
  sheet_names,
  function(sheet_name) {
    tryCatch(
      openxlsx::read.xlsx(
        workbook_path,
        sheet = sheet_name,
        startRow = if (identical(sheet_name, "Dashboard")) 1L else 3L,
        check.names = FALSE
      ),
      error = function(error_condition) {
        stop(sprintf(
          "Workbook verification failed for %s: %s",
          sheet_name,
          conditionMessage(error_condition)
        ))
      }
    )
  }
)
names(workbook_tables) <- sheet_names
print(vapply(workbook_tables, nrow, integer(1)))
print(vapply(workbook_tables, ncol, integer(1)))
formula_error_pattern <- "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A"
formula_error_count <- sum(vapply(
  workbook_tables,
  function(sheet_data) {
    sum(grepl(
      formula_error_pattern,
      as.character(unlist(sheet_data, use.names = FALSE)),
      perl = TRUE
    ))
  },
  integer(1)
))
print(formula_error_count)
stopifnot(
  formula_error_count == 0L,
  nrow(workbook_tables$Documents) == nrow(assignments),
  nrow(workbook_tables$`Topic Summary`) == nrow(topics),
  nrow(workbook_tables$`Topic Terms`) == nrow(terms),
  nrow(workbook_tables$Representatives) == nrow(representatives),
  nrow(workbook_tables$`Model Selection`) == nrow(diagnostics),
  nrow(workbook_tables$`Repeated Text`) == nrow(repeated_translations),
  nrow(workbook_tables$Method) == nrow(method)
)
