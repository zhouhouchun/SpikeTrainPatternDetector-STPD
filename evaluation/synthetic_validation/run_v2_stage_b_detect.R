#!/usr/bin/env Rscript

stpd_v2_stage_b_main <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else NA_character_
  repo_default <- if (is.character(script_path) && !is.na(script_path)) {
    file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
  } else getwd()
  repo <- normalizePath(
    Sys.getenv("STPD_REPO", unset = repo_default),
    mustWork = TRUE
  )
  pipeline_candidates <- c(
    file.path(repo, "evaluation", "synthetic_validation"),
    file.path(repo, "test-results", "clean_synthetic_validation")
  )
  pipeline_dir <- pipeline_candidates[dir.exists(pipeline_candidates)][1L]
  stage_a <- normalizePath(
    Sys.getenv(
      "STPD_V2_STAGE_A",
      unset = "/private/tmp/stpd_clean_benchmark_v2_stage_a_20260827"
    ), mustWork = TRUE
  )
  out_dir <- Sys.getenv(
    "STPD_V2_STAGE_B_OUT",
    unset = "/private/tmp/stpd_clean_benchmark_v2_stage_b_20260827"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  source(file.path(
    pipeline_dir, "v2_pipeline_common.R"
  ), local = FALSE)
  required <- c("digest", "readxl", "pkgload")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stpd_v2_stop("Missing packages: ", paste(missing, collapse = ", "))
  pkgload::load_all(repo, quiet = TRUE)
  source(file.path(
    repo, "evaluation", "publication_validation",
    "publication_validation_helpers.R"
  ), local = FALSE)

  stage_a_manifest <- utils::read.csv(
    file.path(stage_a, "stage_a_manifest.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  expected_columns <- c(
    "Batch_ID", "Params_File", "Params_SHA256", "Holdout_Input_File",
    "Holdout_Input_SHA256", "Holdout_Sample_N"
  )
  if (!identical(names(stage_a_manifest), expected_columns) ||
      anyDuplicated(stage_a_manifest$Batch_ID)) {
    stpd_v2_stop("Invalid Stage-A manifest.")
  }

  selected_batches <- stpd_v2_batch_selection()
  for (batch_id in selected_batches) {
    row <- stage_a_manifest[
      as.character(stage_a_manifest$Batch_ID) == batch_id, , drop = FALSE
    ]
    if (nrow(row) != 1L) stpd_v2_stop("No unique Stage-A artifact for ", batch_id)
    params_path <- file.path(stage_a, row$Params_File)
    stpd_v2_assert_sha256(params_path, row$Params_SHA256)
    params <- readRDS(params_path)
    stpd_v2_assert_blind_artifact(params)

    input_path <- file.path(stage_a, row$Holdout_Input_File)
    stpd_v2_assert_sha256(input_path, row$Holdout_Input_SHA256)
    if (as.integer(row$Holdout_Sample_N) != 15L) {
      stpd_v2_stop("Stage-A holdout count is not 15 for ", batch_id, ".")
    }
    batch <- stpd_v2_read_blinded_holdout(
      input_path, expected_batch = batch_id
    )
    spikes <- stpd_v2_spike_lists(batch)
    sample_ids <- names(spikes)
    if (length(sample_ids) != 15L) {
      stpd_v2_stop("Stage B must detect exactly 15 holdout samples per batch.")
    }
    ds <- SpikeTrainPatternDetector:::make_dataset(
      name = paste0("clean_v2_blinded_", batch_id),
      source = "blinded_xlsx_stage_b", trains = stpd_pub_make_trains(
        spikes, sample_ids
      ), unit_in = "s"
    )
    started <- Sys.time()
    detected <- stpd_detect(
      ds, params = params, selected_trains = sample_ids,
      lock_manual = FALSE, collect_diagnostics = FALSE, label_blind = TRUE
    )
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (!isTRUE(detected$results$label_blind)) {
      stpd_v2_stop("Detector did not preserve label-blind mode.")
    }
    prediction <- stpd_pub_extract_predictions(detected, sample_ids)
    prediction$interval$Batch_ID <- rep(batch_id, nrow(prediction$interval))
    prediction$events$Batch_ID <- rep(batch_id, nrow(prediction$events))
    auto <- stpd_multitrack_auto(detected)

    gaps <- auto$gaps
    gaps <- gaps[
      as.character(gaps$gap_class) == "pause" &
        as.character(gaps$candidate_decision) == "accepted", , drop = FALSE
    ]
    if (nrow(gaps)) {
      if (!"gap_semantics" %in% names(gaps)) {
        stpd_v2_stop("Accepted Pause output lacks gap_semantics in ", batch_id, ".")
      }
      semantics <- as.character(gaps$gap_semantics)
      allowed_semantics <- c(
        "canonical_pause", "contextual_interburst_pause"
      )
      if (anyNA(semantics) || any(!semantics %in% allowed_semantics)) {
        stpd_v2_stop(
          "Accepted Pause lacks canonical gap_semantics in ", batch_id, "."
        )
      }
      gaps$predicted_pause_target <- unname(c(
        canonical_pause = "canonical_pause",
        contextual_interburst_pause = "contextual_separator"
      )[semantics])
    } else {
      gaps$predicted_pause_target <- character()
    }
    gaps$Batch_ID <- rep(batch_id, nrow(gaps))
    states <- auto$states
    states$Batch_ID <- rep(batch_id, nrow(states))

    runtime <- data.frame(
      Batch_ID = batch_id, Sample_N = length(sample_ids),
      Elapsed_Seconds = elapsed, Label_Blind = TRUE,
      Params_SHA256 = as.character(row$Params_SHA256),
      Candidate_Product_SHA256 = as.character(
        prediction$candidate_metadata$product_sha256[1L]
      ), stringsAsFactors = FALSE
    )
    runtime$Scientific_Content_SHA256 <- stpd_v2_scientific_content_sha256(
      prediction$interval, prediction$events, gaps, states
    )
    paths <- c(
      detector = file.path(out_dir, paste0("detector_result_", batch_id, ".rds")),
      intervals = file.path(out_dir, paste0("predicted_intervals_", batch_id, ".csv")),
      episodes = file.path(out_dir, paste0("predicted_episodes_", batch_id, ".csv")),
      gaps = file.path(out_dir, paste0("predicted_gaps_", batch_id, ".csv")),
      states = file.path(out_dir, paste0("predicted_states_", batch_id, ".csv")),
      runtime = file.path(out_dir, paste0("runtime_", batch_id, ".csv"))
    )
    saveRDS(detected, paths[["detector"]], version = 3)
    stpd_v2_write_csv(prediction$interval, paths[["intervals"]])
    stpd_v2_write_csv(prediction$events, paths[["episodes"]])
    stpd_v2_write_csv(gaps, paths[["gaps"]])
    stpd_v2_write_csv(states, paths[["states"]])
    stpd_v2_write_csv(runtime, paths[["runtime"]])
    manifest <- stpd_v2_file_manifest(
      batch_id, unname(paths), names(paths)
    )
    manifest$Stage_A_Params_SHA256 <- as.character(row$Params_SHA256)
    stpd_v2_write_csv(
      manifest,
      file.path(out_dir, paste0("stage_b_manifest_", batch_id, ".csv"))
    )
    cat("Stage B complete for", batch_id, "in", sprintf("%.2f", elapsed),
        "seconds\n")
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) stpd_v2_stage_b_main()
