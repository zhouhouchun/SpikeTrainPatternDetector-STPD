#!/usr/bin/env Rscript

# Build a deterministic SHA-256 manifest for the prepared upload directory.
# Author: Zhou Houchun

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.", call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "tools", "build_upload_manifest.R")
}
repo <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
manifest_path <- file.path(repo, "UPLOAD_MANIFEST_SHA256.csv")
summary_path <- file.path(repo, "UPLOAD_MANIFEST_SUMMARY.txt")

all_paths <- list.files(repo, recursive = TRUE, all.files = TRUE,
                        full.names = TRUE, include.dirs = FALSE, no.. = TRUE)
relative <- substring(all_paths, nchar(repo) + 2L)
excluded <- grepl("(^|/)\\.git(/|$)", relative) |
  relative %in% c("UPLOAD_MANIFEST_SHA256.csv", "UPLOAD_MANIFEST_SUMMARY.txt") |
  grepl("(^|/)\\.DS_Store$|(^|/)~\\$|[.]Rhistory$|[.]RData$", relative)
paths <- all_paths[!excluded]
relative <- relative[!excluded]
ord <- order(relative, method = "radix")
paths <- paths[ord]
relative <- relative[ord]

manifest <- data.frame(
  path = relative,
  bytes = unname(file.info(paths)$size),
  sha256 = vapply(paths, digest::digest, character(1),
                  algo = "sha256", file = TRUE),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE, quote = TRUE)
manifest_hash <- digest::digest(manifest_path, algo = "sha256", file = TRUE)
summary <- c(
  "STPD prepared-upload manifest",
  paste0("generated_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("file_n=", nrow(manifest)),
  paste0("total_bytes=", sum(manifest$bytes)),
  paste0("manifest_sha256=", manifest_hash),
  "excluded=.git/, UPLOAD_MANIFEST_SHA256.csv, UPLOAD_MANIFEST_SUMMARY.txt, transient OS/R files"
)
writeLines(summary, summary_path, useBytes = TRUE)
cat(paste(summary, collapse = "\n"), "\n")
