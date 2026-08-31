#!/usr/bin/env Rscript

# Create a content-addressed freeze record for the scientific detector source.
# Run only after the final regression suite has completed.

root <- normalizePath(getwd(), mustWork = TRUE)
description <- read.dcf(file.path(root, "DESCRIPTION"))
package_version <- unname(description[1L, "Version"])
algorithm_roots <- c("R", "src", file.path("inst", "config"), file.path("inst", "schema"))
files <- c("DESCRIPTION", "NAMESPACE")
for (dir in algorithm_roots) {
  if (dir.exists(file.path(root, dir))) {
    relative <- list.files(
      file.path(root, dir), recursive = TRUE, full.names = FALSE,
      all.files = FALSE
    )
    files <- c(files, file.path(dir, relative))
  }
}
files <- sort(unique(files[file.exists(file.path(root, files))]))
files <- files[!grepl("[.](o|so|dll|dylib)$|/symbols[.]rds$", files)]

manifest <- data.frame(
  path = files,
  bytes = as.numeric(file.info(file.path(root, files))$size),
  sha256 = vapply(files, function(path) {
    digest::digest(file = file.path(root, path), algo = "sha256")
  }, character(1)),
  stringsAsFactors = FALSE
)
freeze_sha256 <- digest::digest(
  paste(manifest$path, manifest$sha256, sep = "\t", collapse = "\n"),
  algo = "sha256", serialize = FALSE
)

suppressPackageStartupMessages(devtools::load_all(root, quiet = TRUE))
params_hash <- stpd_params_hash(default_params())
git_commit <- tryCatch(
  trimws(system2("git", c("-C", root, "rev-parse", "HEAD"), stdout = TRUE)),
  error = function(...) NA_character_
)
if (!length(git_commit)) git_commit <- NA_character_

regression_dir <- file.path(root, "test-results", "release_freeze_final", "regression_final")
summary_files <- sort(list.files(
  regression_dir, pattern = "testthat_shard_[0-9]+_summary[.]csv$",
  full.names = TRUE
))
regression <- if (length(summary_files)) {
  do.call(rbind, lapply(summary_files, utils::read.csv, stringsAsFactors = FALSE))
} else {
  data.frame()
}
regression_status <- if (
  nrow(regression) &&
  sum(regression$failed + regression$errors + regression$warnings) == 0L
) "PASS" else "NOT_VERIFIED"

out_csv <- file.path(root, "publication", "FINAL_ALGORITHM_FREEZE.csv")
out_md <- file.path(root, "publication", "FINAL_ALGORITHM_FREEZE.md")
utils::write.csv(manifest, out_csv, row.names = FALSE, na = "")

regression_lines <- if (nrow(regression)) {
  c(
    sprintf("- Regression status: `%s`", regression_status),
    sprintf("- Official test files: `%d`", sum(regression$file_n)),
    sprintf("- Test blocks: `%d`", sum(regression$test_n)),
    sprintf("- Failures/errors/warnings: `%d/%d/%d`",
            sum(regression$failed), sum(regression$errors), sum(regression$warnings)),
    sprintf("- Skipped test blocks: `%d`", sum(regression$skipped)),
    sprintf("- Aggregate detector test time: `%.3f s`", sum(regression$elapsed_sec)),
    sprintf("- Parallel wall time: `%.3f s`", max(regression$elapsed_sec))
  )
} else {
  "- Regression status: `NOT_VERIFIED` (shard summaries were not found)"
}

writeLines(c(
  "# Final algorithm freeze",
  "",
  "Author: Zhou Houchun",
  "",
  "Freeze date: 2026-08-31",
  "",
  sprintf("- Package version (`DESCRIPTION`): `%s`", package_version),
  sprintf("- Generation-time repository HEAD (metadata only): `%s`", git_commit[[1L]]),
  sprintf("- Frozen algorithm manifest SHA-256: `%s`", freeze_sha256),
  sprintf("- Default effective-parameter hash: `%s`", params_hash),
  sprintf("- Frozen source files: `%d`", nrow(manifest)),
  "",
  "## Release identity",
  "",
  "The per-file manifest and its aggregate SHA-256 are the authoritative identity of the frozen algorithm bytes. The generation-time repository HEAD records repository context only; it is not asserted to reconstruct working-tree bytes that were uncommitted when the manifest was generated. Record a public reconstruction commit only after independently verifying every manifest path and SHA-256 against that commit.",
  "",
  "## Regression evidence",
  "",
  regression_lines,
  "",
  "The official suite excludes the local untracked Finder duplicate `test_gate_b_v3_phase1_contract 2.R`; the canonical test file remains included. Any subsequent change to a frozen source file invalidates this record and requires regeneration plus regression.",
  "",
  "The per-file SHA-256 manifest is stored in `FINAL_ALGORITHM_FREEZE.csv`."
), out_md, useBytes = TRUE)

cat(sprintf(
  "ALGORITHM_FREEZE status=%s files=%d manifest_sha256=%s params_hash=%s\n",
  regression_status, nrow(manifest), freeze_sha256, params_hash
))
