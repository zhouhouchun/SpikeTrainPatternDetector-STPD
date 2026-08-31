#!/usr/bin/env Rscript

stpd_v2_stage_a_main <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else NA_character_
  repo_default <- if (is.character(script_path) && !is.na(script_path)) {
    file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
  } else getwd()
  repo <- normalizePath(
    Sys.getenv("STPD_REPO", unset = repo_default),
    mustWork = TRUE
  )
  benchmark_candidates <- c(
    file.path(repo, "data", "synthetic", "v2.3.0"),
    file.path(repo, "Simulator data", "clean_synthetic_mechanism_benchmark_v2_3_0")
  )
  benchmark_default <- benchmark_candidates[file.exists(benchmark_candidates)][1L]
  pipeline_candidates <- c(
    file.path(repo, "evaluation", "synthetic_validation"),
    file.path(repo, "test-results", "clean_synthetic_validation")
  )
  pipeline_dir <- pipeline_candidates[dir.exists(pipeline_candidates)][1L]
  benchmark <- normalizePath(
    Sys.getenv(
      "STPD_CLEAN_BENCHMARK",
      unset = benchmark_default
    ), mustWork = TRUE
  )
  out_dir <- Sys.getenv(
    "STPD_V2_STAGE_A_OUT",
    unset = "/private/tmp/stpd_clean_benchmark_v2_stage_a_20260827"
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

  workbook_path <- file.path(
    benchmark, "detector_inputs", "spike_timestamps_blinded.xlsx"
  )
  calibration_candidates <- file.path(
    benchmark, "calibration", c(
      "calibration_same_episode_ids_all_scales.csv",
      "calibration_10_episodes_per_mode_per_scale.csv"
    )
  )
  calibration_path <- calibration_candidates[file.exists(calibration_candidates)][1L]
  if (!length(calibration_path) || is.na(calibration_path)) {
    stpd_v2_stop("No supported calibration artifact was found.")
  }
  key_path <- file.path(
    benchmark, "truth", "sample_template_scale_key.csv"
  )
  workbook <- stpd_v2_read_workbook(workbook_path)
  calibration <- utils::read.csv(
    calibration_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  key <- utils::read.csv(key_path, check.names = FALSE, stringsAsFactors = FALSE)
  required_calibration <- c(
    "Calibration_Mode", "Episode_ID", "Sample_ID", "Template_ID",
    "Scale_Factor", "Start_s", "End_s"
  )
  if (!all(required_calibration %in% names(calibration))) {
    stpd_v2_stop("Calibration artifact is missing required columns.")
  }
  matched <- merge(
    unique(calibration[c("Sample_ID", "Template_ID", "Scale_Factor")]),
    key[c("Sample_ID", "Template_ID", "Scale_Factor", "Split")],
    by = c("Sample_ID", "Template_ID", "Scale_Factor"), all.x = TRUE
  )
  if (anyNA(matched$Split) || !all(matched$Split == "development") ||
      any(key$Sample_ID[key$Split == "holdout"] %in% calibration$Sample_ID)) {
    stpd_v2_stop("Calibration is not development-only.")
  }
  rm(matched)

  manifests <- list()
  for (batch_id in names(workbook)) {
    batch_key <- key[key$Workbook_Batch == batch_id, , drop = FALSE]
    development_ids <- as.character(
      batch_key$Sample_ID[batch_key$Split == "development"]
    )
    holdout_ids <- as.character(
      batch_key$Sample_ID[batch_key$Split == "holdout"]
    )
    if (length(development_ids) != 5L || length(holdout_ids) != 15L ||
        length(intersect(development_ids, holdout_ids))) {
      stpd_v2_stop("Invalid protected split for ", batch_id, ".")
    }
    inputs <- stpd_v2_calibration_inputs(calibration, workbook[[batch_id]])
    if (!setequal(unique(inputs$selected$Train_ID), development_ids) ||
        any(inputs$intervals$Train_ID %in% holdout_ids)) {
      stpd_v2_stop("Calibration rows do not equal the five development samples.")
    }
    inputs$spikes <- inputs$spikes[development_ids]
    learned <- stpd_pub_learn_params(
      spikes = inputs$spikes,
      calibration_trains = development_ids,
      selected = inputs$selected, intervals = inputs$intervals,
      dataset_name = paste0("clean_v2_calibration_", batch_id),
      min_examples = 10L, bounded_borrowing = TRUE
    )
    params <- learned$params
    stpd_v2_assert_blind_artifact(params)
    params_path <- file.path(out_dir, paste0("frozen_params_", batch_id, ".rds"))
    saveRDS(params, params_path, version = 3)

    holdout_input <- workbook[[batch_id]][
      workbook[[batch_id]]$Sample_ID %in% holdout_ids,
      c("Sample_ID", "Spike_Index", "Time_s"), drop = FALSE
    ]
    if (!setequal(unique(holdout_input$Sample_ID), holdout_ids)) {
      stpd_v2_stop("Sanitized holdout input is incomplete for ", batch_id, ".")
    }
    stpd_v2_assert_blind_artifact(holdout_input)
    holdout_path <- file.path(
      out_dir, paste0("blinded_holdout_input_", batch_id, ".csv")
    )
    stpd_v2_write_csv(holdout_input, holdout_path)
    stpd_v2_read_blinded_holdout(holdout_path, expected_batch = batch_id)
    manifests[[batch_id]] <- data.frame(
      Batch_ID = batch_id, Params_File = basename(params_path),
      Params_SHA256 = stpd_v2_sha256_file(params_path),
      Holdout_Input_File = basename(holdout_path),
      Holdout_Input_SHA256 = stpd_v2_sha256_file(holdout_path),
      Holdout_Sample_N = 15L,
      stringsAsFactors = FALSE
    )
  }
  manifest <- do.call(rbind, manifests)
  rownames(manifest) <- NULL
  if (!identical(manifest$Batch_ID, c("Batch_A", "Batch_B", "Batch_C"))) {
    stpd_v2_stop("Stage-A batch manifest is incomplete.")
  }
  stpd_v2_write_csv(manifest, file.path(out_dir, "stage_a_manifest.csv"))
  cat("Stage A complete:", normalizePath(out_dir), "\n")
  invisible(manifest)
}

if (sys.nframe() == 0L) stpd_v2_stage_a_main()
