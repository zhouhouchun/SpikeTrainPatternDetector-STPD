#!/usr/bin/env Rscript

# Consolidate the frozen three-region detector runtimes into a publication table.
# Author: Zhou Houchun

options(stringsAsFactors = FALSE)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "evaluation", "performance", "summarize_three_region_runtime.R")
}
repo <- normalizePath(file.path(
  dirname(normalizePath(script_path, mustWork = TRUE)), "..", ".."
), mustWork = TRUE)
source_root <- file.path(repo, "test-results", "multi_region_three_regime_20260831")
output_dir <- Sys.getenv(
  "STPD_PERFORMANCE_SUMMARY_OUT",
  unset = file.path(repo, "test-results", "performance_benchmark_20260831")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

region_map <- c(GPE = "GPe", STN = "STN", GPI = "GPi")
regimes <- c("automatic", "full_params")
rows <- list()

for (region_dir in names(region_map)) {
  for (regime in regimes) {
    path <- file.path(source_root, region_dir, regime)
    protocol <- utils::read.csv(
      file.path(path, "protocol.csv"), stringsAsFactors = FALSE
    )
    runtime <- utils::read.csv(
      file.path(path, "runtime.csv"), stringsAsFactors = FALSE
    )
    if (nrow(protocol) != 1L || nrow(runtime) != 1L) {
      stop("Expected one frozen full-dataset run in ", path, call. = FALSE)
    }
    elapsed <- as.numeric(runtime$elapsed_seconds[[1]])
    isi_n <- as.integer(protocol$isi_n[[1]])
    train_n <- as.integer(protocol$train_n[[1]])
    rows[[length(rows) + 1L]] <- data.frame(
      region = unname(region_map[[region_dir]]),
      dataset = as.character(protocol$dataset[[1]]),
      regime = regime,
      repeat_n = 1L,
      train_n = train_n,
      isi_n = isi_n,
      elapsed_seconds = elapsed,
      elapsed_minutes = elapsed / 60,
      seconds_per_train = elapsed / train_n,
      seconds_per_1000_isi = elapsed / isi_n * 1000,
      isi_per_second = isi_n / elapsed,
      label_blind = as.logical(runtime$label_blind[[1]]),
      manual_example_n = as.integer(runtime$manual_example_n[[1]]),
      threshold_source_mode = as.character(runtime$threshold_source_mode[[1]]),
      workbook_sha256 = as.character(protocol$workbook_sha256[[1]]),
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, rows)
out <- out[order(out$regime, match(out$region, unname(region_map))), ]
rownames(out) <- NULL
utils::write.csv(
  out,
  file.path(output_dir, "real_three_region_runtime.csv"),
  row.names = FALSE
)

cat("Three-region runtime summary written to:\n")
cat(normalizePath(output_dir, mustWork = TRUE), "\n")
print(out, row.names = FALSE)
