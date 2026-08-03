arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 5L)

manifest_path <- arguments[[1L]]
output_path <- arguments[[2L]]
start_row <- as.integer(arguments[[3L]])
end_row <- as.integer(arguments[[4L]])
model_cache <- arguments[[5L]]

stopifnot(
  file.exists(manifest_path),
  dir.exists(model_cache),
  is.finite(start_row),
  is.finite(end_row),
  start_row >= 1L,
  end_row >= start_row
)

devtools::load_all(quiet = TRUE)
chunk_table <- readRDS(manifest_path)
stopifnot(
  is.data.frame(chunk_table),
  "chunk_text" %in% names(chunk_table),
  end_row <= nrow(chunk_table)
)
batch_text <- chunk_table$chunk_text[start_row:end_row]
str(batch_text)
print(length(batch_text))
print(summary(nchar(batch_text)))

model <- load_model(model_cache, threads = 2L)
batch_embeddings <- encode(
  batch_text,
  model,
  batch_size = 64L,
  normalize = TRUE
)
stopifnot(
  identical(dim(batch_embeddings), c(length(batch_text), 384L)),
  !anyNA(batch_embeddings),
  all(is.finite(batch_embeddings)),
  max(abs(sqrt(rowSums(batch_embeddings^2)) - 1)) < 1e-5
)
saveRDS(
  list(
    start_row = start_row,
    end_row = end_row,
    embeddings = batch_embeddings
  ),
  output_path
)
