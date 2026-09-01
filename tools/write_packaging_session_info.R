#!/usr/bin/env Rscript

# Record the environment used for final upload-package QA.
# This does not replace the environment records of the frozen validation runs.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "tools", "write_packaging_session_info.R")
}
repo <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
out_dir <- file.path(repo, "results", "reproducibility")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_dir, "PACKAGING_QA_SESSION_INFO.txt")
con <- file(out, open = "wt", encoding = "UTF-8")
sink(con)
cat("STPD upload-package QA environment\n")
cat("Generated UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n\n")
print(sessionInfo())
sink()
close(con)
message("Wrote: ", out)
