#!/usr/bin/env Rscript

# Run one deterministic shard of the package regression suite.  Launch shards
# in separate R processes to avoid shared mutable state while reducing wall
# time.  Each shard loads the current source tree into a real package namespace
# so getFromNamespace() and source-file manifest tests inspect the same code.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "Usage: run_testthat_regression_shard.R <shard_id> <shard_n> <output_dir>",
    call. = FALSE
  )
}

shard_id <- suppressWarnings(as.integer(args[[1L]]))
shard_n <- suppressWarnings(as.integer(args[[2L]]))
output_dir <- normalizePath(args[[3L]], mustWork = FALSE)
if (!is.finite(shard_id) || !is.finite(shard_n) || shard_n < 1L ||
    shard_id < 1L || shard_id > shard_n) {
  stop("Invalid shard_id/shard_n.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
test_dir <- file.path(getwd(), "tests", "testthat")
files <- sort(list.files(
  test_dir, pattern = "^test.*[.]R$", full.names = FALSE
))
# A local, untracked Finder duplicate is intentionally outside the official
# suite; the canonical file without the " 2" suffix remains included.
files <- files[!grepl(" 2[.]R$", files)]
selected <- files[((seq_along(files) - 1L) %% shard_n) + 1L == shard_id]
if (!length(selected)) stop("This shard has no test files.", call. = FALSE)

started <- proc.time()[["elapsed"]]
suppressPackageStartupMessages(devtools::load_all(getwd(), quiet = TRUE))
result <- lapply(selected, function(file) {
  testthat::test_file(
    file.path(test_dir, file),
    reporter = "summary",
    package = "SpikeTrainPatternDetector",
    load_package = "none",
    stop_on_failure = FALSE,
    stop_on_warning = FALSE
  )
})
elapsed <- proc.time()[["elapsed"]] - started
table <- do.call(rbind, lapply(result, as.data.frame))
utils::write.csv(
  table[, setdiff(names(table), "result"), drop = FALSE],
  file.path(output_dir, sprintf("testthat_shard_%02d.csv", shard_id)),
  row.names = FALSE, na = ""
)

failed <- sum(as.integer(table$failed), na.rm = TRUE)
errors <- sum(as.logical(table$error), na.rm = TRUE)
warnings <- sum(as.integer(table$warning), na.rm = TRUE)
skipped <- sum(as.logical(table$skipped), na.rm = TRUE)
summary <- data.frame(
  shard_id = shard_id,
  shard_n = shard_n,
  file_n = length(selected),
  test_n = nrow(table),
  failed = failed,
  errors = errors,
  warnings = warnings,
  skipped = skipped,
  elapsed_sec = elapsed,
  stringsAsFactors = FALSE
)
utils::write.csv(
  summary,
  file.path(output_dir, sprintf("testthat_shard_%02d_summary.csv", shard_id)),
  row.names = FALSE
)
cat(sprintf(
  "REGRESSION_SHARD shard=%d/%d files=%d tests=%d failed=%d errors=%d warnings=%d skipped=%d elapsed_sec=%.3f\n",
  shard_id, shard_n, length(selected), nrow(table), failed, errors,
  warnings, skipped, elapsed
))
quit(status = if (failed + errors + warnings > 0L) 1L else 0L)
