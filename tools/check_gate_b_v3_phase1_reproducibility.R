#!/usr/bin/env Rscript

root <- normalizePath(".", mustWork = TRUE)
artifacts <- c(
  "inst/config/gate_b_v3_phase1_schema_bundle.json",
  "inst/config/gate_b_v3_phase1_registry.json",
  "inst/config/gate_b_v3_phase1_fixture_spec.json",
  "inst/config/gate_b_v3_phase1_fixture_inputs.json"
)
if (any(!file.exists(file.path(root, artifacts)))) {
  stop("Committed Gate B v3 phase-1 artifacts are missing.", call. = FALSE)
}

copy_clean_source <- function(destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  entries <- list.files(root, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  excluded <- c(".git", ".Rproj.user", "Simulator data", "check",
                "test-results", "docs")
  keep <- !basename(entries) %in% excluded
  copied <- file.copy(entries[keep], destination, recursive = TRUE,
                      copy.mode = TRUE, copy.date = TRUE)
  if (!all(copied)) stop("Could not construct a clean source copy.", call. = FALSE)
  source_docs <- file.path(root,"docs")
  target_docs <- file.path(destination,"docs")
  dir.create(target_docs,recursive=TRUE,showWarnings=FALSE)
  docs_entries <- list.files(source_docs,all.files=TRUE,no..=TRUE,
                             full.names=TRUE)
  docs_entries <- docs_entries[basename(docs_entries)!="reviews"]
  docs_copied <- file.copy(docs_entries,target_docs,recursive=TRUE,
                           copy.mode=TRUE,copy.date=TRUE)
  if (!all(docs_copied)) {
    stop("Could not copy clean normative documentation.",call.=FALSE)
  }
}

run_generation <- function(source) {
  commands <- c("tools/generate_gate_b_v3_phase1_bundle.R",
                "tools/generate_gate_b_v3_phase1_specs.R")
  for (command in commands) {
    output <- system2(file.path(R.home("bin"), "Rscript"), command,
                      stdout = TRUE, stderr = TRUE, env = character(),
                      wait = TRUE)
    status <- attr(output, "status") %||% 0L
    if (!identical(as.integer(status), 0L)) {
      stop("Clean Gate B v3 generation failed: ", paste(output, collapse = "\n"),
           call. = FALSE)
    }
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
hash_files <- function(source) setNames(vapply(
  file.path(source, artifacts), digest::digest, character(1),
  algo = "sha256", file = TRUE
), artifacts)

scratch <- tempfile("stpd-gb3-repro-")
dir.create(scratch)
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
copies <- file.path(scratch, c("clean-a", "clean-b"))
for (copy in copies) copy_clean_source(copy)
for (copy in copies) {
  old <- setwd(copy)
  run_generation(copy)
  setwd(old)
}

committed <- hash_files(root)
first <- hash_files(copies[[1L]])
second <- hash_files(copies[[2L]])
if (!identical(first, second) || !identical(first, committed)) {
  mismatch <- names(first)[first != second | first != committed]
  stop("Gate B v3 clean regeneration is not byte-identical: ",
       paste(mismatch, collapse = ", "), call. = FALSE)
}
cat("GATE_B_V3_PHASE1_CLEAN_REPRODUCIBILITY_OK\n")
