preview_integration_params <- function(enabled) {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- isTRUE(enabled)
  params
}

preview_integration_dataset <- function() {
  stpd_golden_test_dataset("middle_burst")
}

preview_export_filenames <- function() {
  c(
    "Multitrack_preview_metadata.csv",
    "Multitrack_preview_intervals.csv",
    "Multitrack_preview_relationships.csv",
    "Multitrack_preview_per_isi.csv",
    "Multitrack_preview_hfs_dominance_evidence.csv",
    "Multitrack_preview_invariants.csv",
    "Multitrack_preview_manifest.csv",
    "Multitrack_preview.rds"
  )
}

preview_stable_tables <- function(result) {
  preview <- result$results$multitrack_preview
  table_names <- c(
    "intervals", "relationships", "per_isi",
    "hfs_dominance_evidence", "invariants"
  )
  lapply(stats::setNames(table_names, table_names), function(name) {
    table <- preview[[name]]
    volatile <- intersect(c("run_id", "invariant_id"), names(table))
    table[volatile] <- NULL
    rownames(table) <- NULL
    table
  })
}

test_that("the public-preview contract is explicit, fixed, and disabled by default", {
  params <- default_params()
  preview <- params$spiketrainpattern$multitrack_preview
  contract <- stpd_parameter_contract()

  expect_identical(params$spiketrainpattern$schema_version, "spiketrainpattern-2")
  expect_identical(preview$enabled, FALSE)
  expect_identical(
    preview$schema_version,
    "stpd_multitrack_public_preview_v1"
  )
  expect_true(all(c(
    "spiketrainpattern.multitrack_preview.enabled",
    "spiketrainpattern.multitrack_preview.schema_version"
  ) %in% contract$path))

  hash <- getFromNamespace(
    "stpd_multitrack_preview_hash", "SpikeTrainPatternDetector"
  )(params)
  expect_match(hash, "^[0-9a-f]{64}$")
  enabled <- preview_integration_params(TRUE)
  expect_false(identical(
    hash,
    getFromNamespace(
      "stpd_multitrack_preview_hash", "SpikeTrainPatternDetector"
    )(enabled)
  ))

  public_report <- stpd_public_parameter_table(params)
  expect_true(any(
    public_report$section == "Multi-track public preview" &
      public_report$parameter == "preview_policy_hash" &
      public_report$value == hash
  ))
  flat_report <- getFromNamespace(
    "stpd_parameter_report_flat_with_policy_hash",
    "SpikeTrainPatternDetector"
  )(params)
  expect_true(any(
    flat_report$path ==
      "spiketrainpattern.multitrack_preview.preview_policy_hash" &
      flat_report$current_value == hash
  ))
})

test_that("default OFF leaves results, metadata, trains, and exports preview-free", {
  params_off <- preview_integration_params(FALSE)
  result_off <- stpd_detect(
    preview_integration_dataset(), params_off,
    selected_trains = "train_1", collect_diagnostics = TRUE
  )

  expect_false("multitrack_preview" %in% names(result_off$results))
  expect_false(any(grepl(
    "^multitrack_preview_", names(result_off$results$run_metadata_public)
  )))
  expect_false(any(grepl(
    "^pattern_auto_(event|state|gap|review)_preview($|_)",
    names(result_off$trains$train_1)
  )))

  out_dir <- tempfile("stpd_preview_off_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  # Export parameters cannot manufacture products that were absent at run time.
  stpd_export_results(
    result_off, params = preview_integration_params(TRUE), out_dir = out_dir
  )
  expect_identical(
    list.files(out_dir, pattern = "^Multitrack_preview"),
    character()
  )
})

test_that("ON wiring persists one canonical preview and export follows the result", {
  params_on <- preview_integration_params(TRUE)
  result_on <- stpd_detect(
    preview_integration_dataset(), params_on,
    selected_trains = "train_1", collect_diagnostics = TRUE
  )
  preview <- result_on$results$multitrack_preview

  expect_s3_class(preview, "stpd_multitrack_public_preview")
  expect_identical(preview$metadata$materialization_status, "materialized")
  expect_identical(preview$metadata$failure_code, "")
  expect_identical(nrow(preview$intervals), 3L)
  expect_identical(nrow(preview$per_isi), 8L)
  expect_identical(sum(preview$intervals$active_in_preview), 2L)
  expect_false(preview$metadata$label_blind)
  expect_identical(
    preview$metadata$detection_mode,
    "manual_aware"
  )
  expect_identical(
    preview$metadata$schema_version,
    "stpd_multitrack_public_preview_v1"
  )
  expect_identical(preview$metadata$intervals_n, as.integer(nrow(preview$intervals)))
  expect_identical(preview$metadata$per_isi_n, as.integer(nrow(preview$per_isi)))
  expect_true(all(c(
    "pattern_auto_event_preview", "pattern_auto_state_preview",
    "pattern_auto_gap_preview", "pattern_auto_review_preview"
  ) %in% names(preview$per_isi)))
  # Phase 2A keeps the legacy train data frame unchanged; the additive per-ISI
  # projection is carried only by results$multitrack_preview$per_isi.
  expect_false(any(grepl(
    "^pattern_auto_(event|state|gap|review)_preview($|_)",
    names(result_on$trains$train_1)
  )))

  # The opt-in product stays nested and must not reshape the stable legacy
  # detector metadata table. Its own metadata and manifest remain complete.
  result_shape_off <- stpd_detect(
    preview_integration_dataset(), preview_integration_params(FALSE),
    selected_trains = "train_1", collect_diagnostics = TRUE
  )
  expect_identical(
    names(result_on$results$run_metadata_public),
    names(result_shape_off$results$run_metadata_public)
  )
  expect_false(any(grepl(
    "^multitrack_preview_", names(result_on$results$run_metadata_public)
  )))
  stable_metadata <- setdiff(
    names(result_on$results$run_metadata_public),
    c(
      "run_id", "run_started_at_utc", "params_hash",
      "multitrack_auto_product_sha256"
    )
  )
  expect_equal(
    result_on$results$run_metadata_public[stable_metadata],
    result_shape_off$results$run_metadata_public[stable_metadata],
    check.attributes = FALSE
  )
  expect_true(all(c(
    "materialization_status", "schema_version", "policy_hash",
    "params_hash", "intervals_n", "per_isi_n"
  ) %in% names(preview$metadata)))
  expect_true(all(c(
    "table_name", "file_name", "table_sha256"
  ) %in% names(preview$table_manifest)))
  phase1a <- attr(result_on$trains$train_1, "multitrack_shadow", exact = TRUE)
  phase1b <- attr(
    result_on$trains$train_1,
    "multitrack_compatibility_shadow",
    exact = TRUE
  )
  expected_hash <- stpd_params_hash(result_on$params_effective)
  expect_identical(preview$metadata$params_hash, expected_hash)
  expect_identical(
    result_on$results$run_metadata$params_hash,
    expected_hash
  )
  expect_identical(
    result_on$results$run_metadata_public$params_hash,
    expected_hash
  )
  expect_identical(phase1a$source_params_hash, expected_hash)
  expect_identical(phase1b$source_params_hash, expected_hash)

  canonical <- stpd_canonicalize_result_names(result_on)
  expect_identical(canonical$results$multitrack_preview, preview)
  expect_s3_class(
    canonical$results$multitrack_preview,
    "stpd_multitrack_public_preview"
  )

  out_dir <- tempfile("stpd_preview_on_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  # Delayed export is controlled by the persisted run result, not by a new
  # disabled parameter object supplied by the caller.
  stpd_export_results(
    result_on, params = preview_integration_params(FALSE), out_dir = out_dir
  )
  expect_setequal(
    list.files(out_dir, pattern = "^Multitrack_preview"),
    preview_export_filenames()
  )
  off_out_dir <- tempfile("stpd_preview_shape_off_")
  dir.create(off_out_dir)
  on.exit(unlink(off_out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  stpd_export_results(
    result_shape_off, params = preview_integration_params(FALSE),
    out_dir = off_out_dir
  )
  on_export_metadata <- utils::read.csv(
    file.path(out_dir, "Detector_run_metadata.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  off_export_metadata <- utils::read.csv(
    file.path(off_out_dir, "Detector_run_metadata.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(names(on_export_metadata), names(off_export_metadata))

  # A later OFF rerun strips the previous preview before detection and cannot
  # leak stale result or metadata fields.
  rerun_off <- stpd_detect(
    result_on, preview_integration_params(FALSE),
    selected_trains = "train_1", collect_diagnostics = TRUE
  )
  expect_false("multitrack_preview" %in% names(rerun_off$results))
  expect_false(any(grepl(
    "^multitrack_preview_", names(rerun_off$results$run_metadata_public)
  )))
})

test_that("all dataset detector entry points produce the same preview tables", {
  params <- preview_integration_params(TRUE)
  make_ds <- preview_integration_dataset
  results <- list(
    stpd_detect = stpd_detect(
      make_ds(), params, selected_trains = "train_1",
      collect_diagnostics = TRUE
    ),
    stpd_run_detector = stpd_run_detector(
      make_ds(), params, selected_trains = "train_1",
      collect_diagnostics = TRUE
    ),
    run_detector_dataset = run_detector_dataset(
      make_ds(), params, selected_trains = "train_1",
      collect_diagnostics = TRUE
    ),
    stpd_detect_dataset_core = stpd_detect_dataset_core(
      make_ds(), params, selected_trains = "train_1",
      collect_diagnostics = TRUE
    )
  )

  expect_true(all(vapply(results, function(x) {
    inherits(x$results$multitrack_preview, "stpd_multitrack_public_preview") &&
      identical(
        x$results$multitrack_preview$metadata$materialization_status,
        "materialized"
      )
  }, logical(1))))
  reference <- preview_stable_tables(results[[1]])
  for (name in names(results)[-1]) {
    expect_equal(preview_stable_tables(results[[name]]), reference, info = name)
  }
})

test_that("label-blind public preview is invariant to manual labels", {
  params <- preview_integration_params(TRUE)
  unlabeled <- preview_integration_dataset()
  annotated <- preview_integration_dataset()
  dat <- annotated$trains$train_1
  dat$pattern_manual <- rep("", nrow(dat))
  dat$pattern_manual_negative <- rep("", nrow(dat))
  if (nrow(dat) >= 8L) {
    dat$pattern_manual[3:6] <- "burst"
    dat$pattern_manual_negative[7:8] <- "not_burst"
  }
  annotated$trains$train_1 <- dat

  reference <- stpd_detect(
    unlabeled, params, selected_trains = "train_1",
    collect_diagnostics = TRUE, label_blind = TRUE
  )
  challenged <- stpd_detect(
    annotated, params, selected_trains = "train_1",
    collect_diagnostics = TRUE, label_blind = TRUE
  )

  expect_equal(
    preview_stable_tables(challenged),
    preview_stable_tables(reference)
  )
  expect_identical(
    challenged$trains$train_1$pattern_auto,
    reference$trains$train_1$pattern_auto
  )
  expect_identical(
    challenged$trains$train_1$pattern_manual,
    annotated$trains$train_1$pattern_manual
  )
  expect_identical(
    challenged$trains$train_1$pattern_manual_negative,
    annotated$trains$train_1$pattern_manual_negative
  )
  expect_true(challenged$results$multitrack_preview$metadata$label_blind)
  expect_identical(
    challenged$results$multitrack_preview$metadata$detection_mode,
    "label_blind"
  )
})

test_that("a corrupted persisted Preview cannot abort or overwrite legacy export", {
  params <- preview_integration_params(TRUE)
  result <- stpd_detect(
    preview_integration_dataset(), params,
    selected_trains = "train_1", collect_diagnostics = TRUE
  )
  result$results$multitrack_preview$per_isi <- NULL
  out_dir <- tempfile("stpd_corrupt_preview_export_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_warning(
    exported <- stpd_export_results(result, params = params, out_dir = out_dir),
    "multitrack_preview_export_skipped"
  )
  expect_identical(exported, out_dir)
  expect_true(all(file.exists(file.path(out_dir, c(
    "Events_final.csv", "ISI_labels_final.csv", "Detector_run_metadata.csv",
    "Export_run_metadata.csv", "Detector_effective_params.rds",
    "Detector_params.txt", "README_results.txt",
    "Methodological_warnings.txt"
  )))))
  expect_identical(
    list.files(out_dir, pattern = "^Multitrack_preview"), character()
  )
  warning_lines <- readLines(
    file.path(out_dir, "Methodological_warnings.txt"), warn = FALSE
  )
  expect_true(any(grepl(
    "component=multitrack_preview; status=skipped_invalid_persisted_preview",
    warning_lines, fixed = TRUE
  )))
})

test_that("writer re-materializes from the parent before publishing", {
  params <- preview_integration_params(TRUE)
  result <- stpd_detect(
    preview_integration_dataset(), params,
    selected_trains = "train_1", collect_diagnostics = TRUE
  )
  preview <- result$results$multitrack_preview
  preview$intervals$candidate_source[1] <- "self_consistent_but_not_parent_owned"
  manifest_row <- match("intervals", preview$table_manifest$table_name)
  preview$table_manifest$table_sha256[manifest_row] <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_table_hash(
      preview$intervals
    )
  result$results$multitrack_preview <- preview

  out_dir <- tempfile("stpd_parent_rematerialization_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  expect_warning(
    paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
      result, out_dir
    ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(paths, "multitrack_preview_export_code"),
    "preview_parent_rematerialization_mismatch"
  )
  expect_identical(
    list.files(out_dir, pattern = "^Multitrack_preview"), character()
  )
})

test_that("all public-preview artifacts and track schemas are rejected as raw CSV", {
  hard_name <- getFromNamespace(
    "stpd_event_grammar_hard_derived_csv_filename",
    "SpikeTrainPatternDetector"
  )
  expect_true(all(vapply(
    preview_export_filenames(), hard_name, logical(1)
  )))

  strong_schema <- getFromNamespace(
    "stpd_event_grammar_strong_derived_csv_schema",
    "SpikeTrainPatternDetector"
  )
  expect_true(strong_schema(data.frame(
    semantic_track = "event",
    interval_id = "mtpi_example",
    pattern_auto_event_preview = "burst",
    stringsAsFactors = FALSE
  )))

  input_dir <- tempfile("stpd_preview_guard_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE, force = TRUE), add = TRUE)
  derived <- file.path(input_dir, "Multitrack_preview_per_isi.csv")
  utils::write.csv(data.frame(timestamp_sec = c(0, 0.1)), derived,
                   row.names = FALSE)
  expect_error(
    build_trains_from_raw(derived, unit_in = "s"),
    "high-confidence derived output table"
  )
})
