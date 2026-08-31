test_that("input fingerprints and detector run identities form one provenance chain", {
  input_path <- tempfile(fileext = ".csv")
  on.exit(unlink(input_path), add = TRUE)
  utils::write.csv(
    data.frame(train_1 = seq(0, 1.2, by = 0.01)),
    input_path,
    row.names = FALSE
  )

  ds <- build_spike_dataset(input_path, mode = "raw", unit_in = "s")
  expected_input_hash <- digest::digest(input_path, algo = "sha256", file = TRUE)
  expect_identical(ds$meta$input_file_name, basename(input_path))
  expect_identical(ds$meta$input_sha256, expected_input_hash)
  expect_equal(ds$meta$input_size_bytes, unname(file.info(input_path)$size))
  expect_identical(ds$meta$input_parser_mode, "raw")
  expect_identical(ds$meta$input_unit, "s")
  expect_true(ds$meta$input_header)
  expect_identical(ds$meta$input_duplicate_policy, "error_keep")

  out <- stpd_detect(
    ds,
    default_params(),
    selected_trains = "train_1",
    collect_diagnostics = TRUE,
    label_blind = TRUE
  )
  metadata <- out$results$run_metadata_public

  expect_equal(nrow(metadata), 1L)
  expect_match(metadata$run_id, "^stpd_run_[0-9]{8}_[0-9]{12}_p[0-9]+_n[0-9]{2}$")
  expect_match(metadata$params_hash, "^[0-9a-f]{64}$")
  expect_identical(metadata$params_hash_algorithm, "SHA-256")
  expect_identical(metadata$input_file_name, basename(input_path))
  expect_identical(metadata$input_sha256, expected_input_hash)
  expect_equal(metadata$input_size_bytes, unname(file.info(input_path)$size))
  expect_identical(metadata$input_parser_mode, "raw")
  expect_identical(metadata$input_unit, "s")
  expect_true(metadata$input_header)
  expect_identical(metadata$input_duplicate_policy, "error_keep")
  expect_identical(metadata$detection_mode, "label_blind")
  expect_true(metadata$label_blind)
  expect_false(metadata$lock_manual_effective)
  expect_true(metadata$detector_deterministic)
  expect_true(is.na(metadata$random_seed))
  expect_equal(metadata$selected_train_n, 1L)
  expect_identical(metadata$selected_trains, "train_1")
  expect_true(nzchar(metadata$package_version))
  expect_true(metadata$code_revision_source %in% c(
    "STPD_GIT_COMMIT", "package_description_remote_sha",
    "STPD_SOURCE_DIR_git", "working_directory_git", "unavailable"
  ))
  if (!is.na(metadata$code_revision)) expect_match(metadata$code_revision, "^[0-9a-fA-F]{40}$")
  expect_true(nzchar(metadata$r_version))
  expect_true(nzchar(metadata$dependency_versions))

  internal_metadata <- out$results$run_metadata
  expect_identical(as.character(internal_metadata$run_id), metadata$run_id)
  expect_identical(as.character(internal_metadata$params_hash), metadata$params_hash)
  expect_true(is.list(out$params_effective))
  expect_identical(stpd_params_hash(out$params_effective), metadata$params_hash)

  events <- as.data.frame(out$results$events)
  expect_gt(nrow(events), 0L)
  expect_true(all(as.character(events$run_id) == metadata$run_id))
  expect_true(all(as.character(events$params_hash) == metadata$params_hash))

  candidates <- as.data.frame(out$results$candidate_ledger)
  expect_gt(nrow(candidates), 0L)
  expect_true(all(as.character(candidates$run_id) == metadata$run_id))
  expect_true(all(as.character(candidates$params_hash) == metadata$params_hash))
})

test_that("run metadata records manual-label exposure even in label-blind mode", {
  ds <- SpikeTrainPatternDetector:::stpd_golden_test_dataset("stable_high_frequency")
  n <- nrow(ds$trains$train_1)
  ds$trains$train_1$pattern_manual <- rep("", n)
  ds$trains$train_1$pattern_manual_negative <- rep("", n)
  ds$trains$train_1$pattern_manual[5:7] <- "burst"
  ds$trains$train_1$pattern_manual_negative[10:11] <- "not_burst"

  out <- stpd_detect(ds, default_params(), label_blind = TRUE, collect_diagnostics = FALSE)
  metadata <- out$results$run_metadata_public

  expect_equal(metadata$manual_positive_interval_n_at_input, 3L)
  expect_equal(metadata$manual_negative_interval_n_at_input, 2L)
  expect_identical(out$trains$train_1$pattern_manual, ds$trains$train_1$pattern_manual)
  expect_identical(
    out$trains$train_1$pattern_manual_negative,
    ds$trains$train_1$pattern_manual_negative
  )
})

test_that("run identifiers remain unique for runs sharing a timestamp", {
  at <- as.POSIXct("2026-08-10 12:00:00.123456", tz = "UTC")
  first <- SpikeTrainPatternDetector:::stpd_new_run_id(at)
  second <- SpikeTrainPatternDetector:::stpd_new_run_id(at)

  expect_false(identical(first, second))
  expect_match(first, "_n01$")
  expect_match(second, "_n02$")
})

test_that("label-blind export keeps reference labels separate from detector decisions", {
  ds <- SpikeTrainPatternDetector:::stpd_golden_test_dataset("stable_high_frequency")
  n <- nrow(ds$trains$train_1)
  ds$trains$train_1$pattern_manual <- rep("", n)
  ds$trains$train_1$pattern_manual_negative <- rep("", n)
  ds$trains$train_1$pattern_manual[5:9] <- "burst"

  out <- stpd_detect(ds, default_params(), label_blind = TRUE, collect_diagnostics = FALSE)
  export_dir <- tempfile("stpd_label_blind_export_")
  on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)
  stpd_export_results(out, default_params(), export_dir)

  labels <- utils::read.csv(file.path(export_dir, "ISI_labels_final.csv"), na.strings = character())
  labels$manual_label[is.na(labels$manual_label)] <- ""
  labels$auto_label[is.na(labels$auto_label)] <- ""
  labels$final_label[is.na(labels$final_label)] <- ""
  reference_rows <- labels$manual_label == "burst"

  expect_true(any(reference_rows))
  expect_identical(labels$final_label, labels$auto_label)
  expect_true(all(labels$manual_label[reference_rows] == "burst"))
  expect_true(all(labels$final_label_source == "automatic_label_blind"))
  expect_true(file.exists(file.path(export_dir, "Detector_threshold_table.csv")))
  expect_true(file.exists(file.path(export_dir, "Detector_effective_params.rds")))
  effective_params <- readRDS(file.path(export_dir, "Detector_effective_params.rds"))
  detector_metadata <- utils::read.csv(file.path(export_dir, "Detector_run_metadata.csv"))
  expect_identical(stpd_params_hash(effective_params), detector_metadata$params_hash)

  export_metadata <- utils::read.csv(file.path(export_dir, "Export_run_metadata.csv"))
  events <- utils::read.csv(file.path(export_dir, "Events_final.csv"))
  expect_match(export_metadata$export_id, "^stpd_export_[0-9]{8}_[0-9]{12}_p[0-9]+_n[0-9]{2}$")
  expect_identical(export_metadata$parent_detector_run_id, detector_metadata$run_id)
  expect_identical(export_metadata$parent_params_hash, detector_metadata$params_hash)
  expect_identical(export_metadata$final_label_source, "automatic_label_blind")
  expect_equal(export_metadata$final_event_count, nrow(events))
  expect_equal(export_metadata$final_isi_label_count, nrow(labels))
  expect_match(export_metadata$review_state_sha256, "^[0-9a-f]{64}$")
  expect_true(export_metadata$effective_params_hash_matches_parent)
})

test_that("dataset-frozen thresholds contribute to the effective parameter hash", {
  make_ds <- function(step) {
    times <- seq(0, by = step, length.out = 180L)
    dat <- data.frame(
      idx = seq_along(times), timestamp_sec = times,
      ISI_sec = c(NA_real_, diff(times)),
      pattern_manual = rep("", length(times)),
      pattern_manual_negative = rep("", length(times)),
      pattern_auto = rep("", length(times)),
      stringsAsFactors = FALSE
    )
    make_dataset("hash_threshold_test", "synthetic", list(train_1 = dat), unit_in = "s")
  }

  fast <- stpd_detect(make_ds(0.006), default_params(), label_blind = TRUE, collect_diagnostics = FALSE)
  slow <- stpd_detect(make_ds(0.045), default_params(), label_blind = TRUE, collect_diagnostics = FALSE)

  expect_false(identical(
    fast$results$run_metadata_public$params_hash,
    slow$results$run_metadata_public$params_hash
  ))
  expect_true(nrow(fast$results$threshold_table) > 0)
  expect_true(nrow(slow$results$threshold_table) > 0)
  expect_identical(
    fast$results$run_metadata_public$params_hash,
    fast$results$run_metadata$params_hash
  )
})

test_that("all exported detector entry points preserve label-blind provenance", {
  ds <- SpikeTrainPatternDetector:::stpd_golden_test_dataset("stable_high_frequency")
  via_run_detector <- run_detector(ds, default_params(), label_blind = TRUE, collect_diagnostics = FALSE)
  via_dataset_core <- stpd_detect_dataset_core(ds, default_params(), label_blind = TRUE, collect_diagnostics = FALSE)

  expect_true(via_run_detector$results$run_metadata_public$label_blind)
  expect_true(via_dataset_core$results$run_metadata_public$label_blind)
  expect_identical(via_run_detector$results$detection_mode, "label_blind")
  expect_identical(via_dataset_core$results$detection_mode, "label_blind")
})
