phase2b_export_candidate <- function(id, label, start, end, score, priority) {
  data.frame(
    candidate_id = id,
    candidate_layer = "phase2b_export_fixture",
    candidate_source = "synthetic_unit_fixture",
    final_label = label,
    start_isi = as.integer(start),
    end_isi = as.integer(end),
    n_isi = as.integer(end - start + 1L),
    score = as.numeric(score),
    priority = as.numeric(priority),
    stringsAsFactors = FALSE
  )
}

phase2b_export_fixture <- function(
    include_candidate_source = TRUE, exact_burst = FALSE,
    two_trains = FALSE) {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  pool <- dplyr::bind_rows(
    phase2b_export_candidate(
      "review_source", "possible_burst", 20L, 23L, 12, 300
    ),
    phase2b_export_candidate(
      "hfs_state", "high_frequency_spiking", 10L, 45L, 20, 900
    )
  )
  if (!isTRUE(include_candidate_source)) pool$candidate_source <- NULL
  if (isTRUE(exact_burst)) {
    exact <- phase2b_export_candidate(
      "existing_burst", "burst", 20L, 23L, 30, 1200
    )
    if (!isTRUE(include_candidate_source)) exact$candidate_source <- NULL
    pool <- dplyr::bind_rows(pool, exact)
  }
  n <- 60L
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = c(0, cumsum(rep(0.04, n - 1L))),
    ISI_sec = c(NA_real_, rep(0.04, n - 1L)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
  phase1a <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool,
    patterns = c("burst", "possible_burst", "high_frequency_spiking"),
    params = params
  )
  phase1b <- SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a, dat = dat, params = params, variable_params = NULL,
    min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  params_hash <- stpd_params_hash(params)
  trains <- list(train_1 = dat)
  if (isTRUE(two_trains)) trains$train_2 <- dat
  ds <- list(
    trains = trains,
    params_effective = params,
    results = list(run_metadata = data.frame(
      run_id = "phase2b_export_test_run",
      params_hash = params_hash,
      stringsAsFactors = FALSE
    )),
    meta = list(display_name = "phase2b_export_fixture", unit_in = "s")
  )
  ds$results$multitrack_preview <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      ds, params, run_id = "phase2b_export_test_run",
      params_hash = params_hash
    )
  review <- ds$results$multitrack_preview$intervals
  review <- review[
    review$semantic_track == "review" & review$label == "possible_burst" &
      review$active_in_preview,
    , drop = FALSE
  ]
  stopifnot(nrow(review) == if (isTRUE(two_trains)) 2L else 1L)
  request <- data.frame(
    train = review$train,
    source_review_interval_id = review$interval_id,
    stringsAsFactors = FALSE
  )
  preflight <- stpd_multitrack_review_preflight(ds, request, "confirm")
  confirmed <- stpd_multitrack_review_confirm(
    ds, request, preflight$precondition_sha256,
    reviewer = "export_reviewer", reason = "export validation fixture",
    operation_id = "phase2b_export_fixture_confirm"
  )
  list(ds = confirmed$dataset, product = confirmed$product, request = request)
}

test_that("production-shaped candidates without candidate_source confirm and export", {
  fixture <- phase2b_export_fixture(include_candidate_source = FALSE)
  history <- stpd_multitrack_review_history(fixture$ds)
  expect_identical(history$source_candidate_source, "")

  out_dir <- tempfile("phase2b_review_export_optional_source_")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    fixture$ds, out_dir
  )
  expect_identical(
    attr(paths, "multitrack_review_export_status"), "written"
  )
  expect_length(paths, 10L)
  expect_true(all(file.exists(paths)))
  product <- readRDS(paths[["rds"]])
  expect_identical(product$transition_history$source_candidate_source, "")
  expect_identical(
    stpd_multitrack_review_product(fixture$ds), fixture$product
  )
})

test_that("an exact-span Burst link survives strict export without duplicate Event", {
  fixture <- phase2b_export_fixture(exact_burst = TRUE)
  expect_equal(nrow(fixture$product$manual_intervals), 1L)
  expect_false(fixture$product$manual_intervals$creates_new_event)
  expect_equal(nrow(fixture$product$final_relationships), 1L)
  expect_equal(sum(
    fixture$product$final_intervals$semantic_track == "event" &
      fixture$product$final_intervals$label == "burst"
  ), 1L)

  out_dir <- tempfile("phase2b_review_export_link_")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    fixture$ds, out_dir
  )
  expect_identical(
    attr(paths, "multitrack_review_export_status"), "written"
  )
  exported <- readRDS(paths[["rds"]])
  expect_equal(nrow(exported$final_relationships), 1L)
  expect_identical(
    exported$final_relationships$target_final_interval_id,
    exported$manual_intervals$linked_event_interval_id
  )
  expect_true(
    exported$final_relationships$target_final_interval_id %in%
      exported$final_intervals$final_interval_id
  )
  expect_equal(sum(
    exported$final_intervals$semantic_track == "event" &
      exported$final_intervals$label == "burst"
  ), 1L)
})

phase2b_export_refresh_hashes <- function(product) {
  product$metadata$transition_history_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_table_hash(
      product$transition_history
    )
  product$metadata$transition_n <- as.integer(nrow(product$transition_history))
  product$metadata$confirmed_n <- as.integer(sum(
    product$current_decisions$current_status == "confirmed"
  ))
  product$metadata$manual_interval_n <- as.integer(nrow(product$manual_intervals))
  product$metadata$final_interval_n <- as.integer(nrow(product$final_intervals))
  product$metadata$final_relationship_n <-
    as.integer(nrow(product$final_relationships))
  product$metadata$final_per_isi_n <- as.integer(nrow(product$final_per_isi))
  payload_names <- c(
    "transition_history", "current_decisions", "manual_intervals",
    "final_intervals", "final_relationships", "final_per_isi", "invariants"
  )
  payload <- lapply(payload_names, function(name) product[[name]])
  names(payload) <- payload_names
  product$metadata$product_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_sha256(payload)
  table_names <- names(
    SpikeTrainPatternDetector:::stpd_multitrack_review_manifest_filenames()
  )
  for (name in table_names) {
    row <- match(name, product$table_manifest$table_name)
    table <- product[[name]]
    product$table_manifest$row_count[row] <- as.integer(nrow(table))
    product$table_manifest$column_count[row] <- as.integer(ncol(table))
    product$table_manifest$column_types[row] <-
      SpikeTrainPatternDetector:::stpd_multitrack_review_column_types(table)
    product$table_manifest$table_sha256[row] <-
      SpikeTrainPatternDetector:::stpd_multitrack_review_table_hash(table)
  }
  product
}

phase2b_export_fixed_files <- function() {
  c(
    "Multitrack_review_metadata.csv",
    "Multitrack_review_transition_history.csv",
    "Multitrack_review_current_decisions.csv",
    "Multitrack_review_manual_intervals.csv",
    "Multitrack_review_final_intervals.csv",
    "Multitrack_review_final_relationships.csv",
    "Multitrack_review_final_per_isi.csv",
    "Multitrack_review_invariants.csv",
    "Multitrack_review_manifest.csv",
    "Multitrack_review.rds"
  )
}

test_that("Phase 2B export writes eight fixed tables, manifest, and canonical RDS", {
  fixture <- phase2b_export_fixture()
  dataset_before <- serialize(fixture$ds, NULL)
  preview_before <- serialize(fixture$ds$results$multitrack_preview, NULL)
  out_dir <- tempfile("phase2b_review_export_")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    fixture$ds, out_dir
  )
  expect_length(paths, 10L)
  expect_true(all(file.exists(paths)))
  expect_equal(sum(grepl("\\.csv$", paths, ignore.case = TRUE)), 9L)
  expect_identical(
    attr(paths, "multitrack_review_export_status"), "written"
  )
  expect_identical(
    sort(basename(paths), method = "radix"),
    sort(phase2b_export_fixed_files(), method = "radix")
  )
  expect_identical(readRDS(paths[["rds"]]), fixture$product)
  expect_identical(serialize(fixture$ds, NULL), dataset_before)
  expect_identical(
    serialize(fixture$ds$results$multitrack_preview, NULL), preview_before
  )

  manifest <- utils::read.csv(
    paths[["manifest"]], stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_equal(nrow(manifest), 8L)
  expect_identical(
    as.character(manifest$file_name),
    phase2b_export_fixed_files()[seq_len(8L)]
  )
  for (i in seq_len(nrow(manifest))) {
    table <- utils::read.csv(
      file.path(out_dir, manifest$file_name[i]),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    expect_identical(nrow(table), as.integer(manifest$row_count[i]))
    expect_identical(ncol(table), as.integer(manifest$column_count[i]))
  }

  exported_metadata <- utils::read.csv(
    file.path(out_dir, "Multitrack_review_metadata.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(exported_metadata$run_id, fixture$product$metadata$run_id)
  expect_identical(
    exported_metadata$transition_history_sha256,
    fixture$product$metadata$transition_history_sha256
  )
  exported_history <- utils::read.csv(
    file.path(out_dir, "Multitrack_review_transition_history.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(
    exported_history$transition_action,
    fixture$product$transition_history$transition_action
  )
  expect_identical(
    exported_history$target_event_interval_id,
    fixture$product$transition_history$target_event_interval_id
  )
})

test_that("Phase 2B export keeps train-qualified identities distinct", {
  fixture <- phase2b_export_fixture(two_trains = TRUE)
  expect_identical(fixture$product$transition_history$transition_sequence, 1:2)
  expect_equal(length(unique(
    fixture$product$transition_history$operation_id
  )), 1L)
  expect_identical(
    fixture$product$transition_history$previous_transition_id[2],
    fixture$product$transition_history$transition_id[1]
  )
  expect_identical(
    sort(unique(fixture$product$final_per_isi$train), method = "radix"),
    c("train_1", "train_2")
  )
  promoted <- fixture$product$final_intervals[
    fixture$product$final_intervals$final_source ==
      "phase2b_review_confirmation",
    , drop = FALSE
  ]
  expect_equal(nrow(promoted), 2L)
  expect_equal(length(unique(promoted$final_interval_id)), 2L)

  out_dir <- tempfile("phase2b_review_export_two_train_")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    fixture$ds, out_dir
  )
  expect_identical(
    attr(paths, "multitrack_review_export_status"), "written"
  )
  per_isi <- utils::read.csv(
    file.path(out_dir, "Multitrack_review_final_per_isi.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_equal(nrow(per_isi), 120L)
  expect_identical(
    sort(unique(per_isi$train), method = "radix"),
    c("train_1", "train_2")
  )
  events <- utils::read.csv(
    file.path(out_dir, "Multitrack_review_final_intervals.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  manual_events <- events[
    events$final_source == "phase2b_review_confirmation", , drop = FALSE
  ]
  expect_equal(nrow(manual_events), 2L)
  expect_equal(length(unique(manual_events$final_interval_id)), 2L)
})

test_that("Phase 2B corruption fails soft, cannot choose paths, and preserves other exports", {
  fixture <- phase2b_export_fixture()
  out_dir <- tempfile("phase2b_review_corruption_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  legacy_sentinel <- file.path(out_dir, "Legacy_results.csv")
  preview_sentinel <- file.path(out_dir, "Multitrack_preview.rds")
  warnings_file <- file.path(out_dir, "Methodological_warnings.txt")
  writeLines("legacy intact", legacy_sentinel)
  writeLines("preview intact", preview_sentinel)
  writeLines("existing warnings", warnings_file)

  initial <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    fixture$ds, out_dir
  )
  expect_identical(attr(initial, "multitrack_review_export_status"), "written")

  path_tamper <- fixture$ds
  outside <- file.path(dirname(out_dir), "phase2b_escape_sentinel.csv")
  on.exit(unlink(outside, force = TRUE), add = TRUE)
  writeLines("outside intact", outside)
  path_tamper$results$multitrack_review$table_manifest$file_name[1] <-
    paste0("../", basename(outside))
  expect_warning(
    skipped_path <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
      path_tamper, out_dir
    ),
    "multitrack_review_export_skipped"
  )
  expect_identical(
    attr(skipped_path, "multitrack_review_export_code"),
    "review_manifest_filename_invalid"
  )
  expect_identical(readLines(outside), "outside intact")
  expect_identical(readLines(legacy_sentinel), "legacy intact")
  expect_identical(readLines(preview_sentinel), "preview intact")
  expect_false(any(file.exists(file.path(out_dir, phase2b_export_fixed_files()))))
  expect_true(any(grepl(
    "component=multitrack_review", readLines(warnings_file), fixed = TRUE
  )))

  payload_tamper <- fixture$ds
  event <- which(
    payload_tamper$results$multitrack_review$final_intervals$
      final_source == "phase2b_review_confirmation"
  )
  expect_length(event, 1L)
  payload_tamper$results$multitrack_review$final_intervals$label[event] <-
    "long_burst"
  expect_warning(
    skipped_stale_hash <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_review(
        payload_tamper, out_dir
      ),
    "multitrack_review_export_skipped"
  )
  expect_identical(
    attr(skipped_stale_hash, "multitrack_review_export_code"),
    "review_manifest_payload_mismatch"
  )
  payload_tamper$results$multitrack_review <- phase2b_export_refresh_hashes(
    payload_tamper$results$multitrack_review
  )
  expect_warning(
    skipped_payload <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_review(
        payload_tamper, out_dir
      ),
    "multitrack_review_export_skipped"
  )
  expect_identical(
    attr(skipped_payload, "multitrack_review_export_code"),
    "review_manual_foreign_key_invalid"
  )

  chain_tamper <- fixture$ds
  chain_tamper$results$multitrack_review$transition_history$reviewer[1] <-
    "tampered_reviewer"
  chain_tamper$results$multitrack_review <- phase2b_export_refresh_hashes(
    chain_tamper$results$multitrack_review
  )
  expect_warning(
    skipped_chain <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
      chain_tamper, out_dir
    ),
    "multitrack_review_export_skipped"
  )
  expect_identical(
    attr(skipped_chain, "multitrack_review_export_code"),
    "review_history_hash_chain_invalid"
  )

  wrong_parent <- fixture$ds
  wrong_parent$results$run_metadata$run_id <- "foreign_parent_run"
  expect_warning(
    skipped_parent <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
      wrong_parent, out_dir
    ),
    "multitrack_review_export_skipped"
  )
  expect_identical(
    attr(skipped_parent, "multitrack_review_export_code"),
    "review_parent_preview_invalid"
  )
  expect_identical(readLines(legacy_sentinel), "legacy intact")
  expect_identical(readLines(preview_sentinel), "preview intact")

  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = 2)
  warn_as_error_safe <-
    SpikeTrainPatternDetector:::stpd_write_multitrack_review(
      path_tamper, out_dir
    )
  expect_identical(
    attr(warn_as_error_safe, "multitrack_review_export_status"),
    "skipped_invalid_persisted_review"
  )

  unlink(warnings_file, force = TRUE)
  dir.create(warnings_file)
  logging_failure_safe <-
    SpikeTrainPatternDetector:::stpd_write_multitrack_review(
      path_tamper, out_dir
    )
  expect_identical(
    attr(logging_failure_safe, "multitrack_review_export_status"),
    "skipped_invalid_persisted_review"
  )
  expect_identical(readLines(legacy_sentinel), "legacy intact")
  expect_identical(readLines(preview_sentinel), "preview intact")
})

test_that("stale Phase 2B export is history-only and no-state cleanup is scoped", {
  fixture <- phase2b_export_fixture()
  out_dir <- tempfile("phase2b_review_stale_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  legacy_sentinel <- file.path(out_dir, "Legacy_results.csv")
  preview_sentinel <- file.path(out_dir, "Multitrack_preview_manifest.csv")
  writeLines("legacy intact", legacy_sentinel)
  writeLines("preview intact", preview_sentinel)

  stale <- SpikeTrainPatternDetector:::stpd_multitrack_review_strip_for_rerun(
    fixture$ds
  )
  stale$results$multitrack_preview <- NULL
  stale_paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    stale, out_dir
  )
  expect_length(stale_paths, 10L)
  expect_identical(
    attr(stale_paths, "multitrack_review_export_status"),
    "written_stale_history_only"
  )
  stale_rds <- readRDS(stale_paths[["rds"]])
  expect_identical(
    stale_rds$metadata$lifecycle_status, "stale_after_detector_rerun"
  )
  expect_equal(nrow(stale_rds$transition_history), 1L)
  expect_equal(nrow(stale_rds$current_decisions), 0L)
  expect_equal(nrow(stale_rds$final_intervals), 0L)
  expect_equal(nrow(stale_rds$final_per_isi), 0L)

  stale_with_final <- stale
  stale_with_final$results$multitrack_review$final_intervals <-
    fixture$product$final_intervals
  stale_with_final$results$multitrack_review <- phase2b_export_refresh_hashes(
    stale_with_final$results$multitrack_review
  )
  expect_warning(
    rejected_stale <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
      stale_with_final, out_dir
    ),
    "multitrack_review_export_skipped"
  )
  expect_identical(
    attr(rejected_stale, "multitrack_review_export_code"),
    "review_stale_contains_active_final"
  )
  expect_false(any(file.exists(file.path(out_dir, phase2b_export_fixed_files()))))

  SpikeTrainPatternDetector:::stpd_write_multitrack_review(fixture$ds, out_dir)
  without_state <- fixture$ds
  without_state$results$multitrack_review <- NULL
  absent <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    without_state, out_dir
  )
  expect_length(absent, 0L)
  expect_identical(
    attr(absent, "multitrack_review_export_status"), "not_present"
  )
  expect_false(any(file.exists(file.path(out_dir, phase2b_export_fixed_files()))))
  expect_identical(readLines(legacy_sentinel), "legacy intact")
  expect_identical(readLines(preview_sentinel), "preview intact")
})

test_that("append-only history supports explicit reconfirmation after a new detector run", {
  fixture <- phase2b_export_fixture()
  rerun <- fixture$ds
  rerun$results$multitrack_review <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
      fixture$product, "detector_rerun_test"
    )
  new_run_id <- "phase2b_export_test_run_2"
  params_hash <- stpd_params_hash(rerun$params_effective)
  rerun$results$run_metadata$run_id <- new_run_id
  rerun$results$run_metadata$params_hash <- params_hash
  rerun$results$multitrack_preview <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      rerun, rerun$params_effective,
      run_id = new_run_id, params_hash = params_hash
    )
  review <- rerun$results$multitrack_preview$intervals
  review <- review[
    review$semantic_track == "review" & review$label == "possible_burst" &
      review$active_in_preview,
    , drop = FALSE
  ]
  request <- data.frame(
    train = review$train,
    source_review_interval_id = review$interval_id,
    stringsAsFactors = FALSE
  )
  # The scientific Review identity is stable, but the immutable parent run is
  # new; this is an explicit reconfirmation rather than a duplicate confirm.
  expect_identical(
    request$source_review_interval_id,
    fixture$request$source_review_interval_id
  )
  preflight <- stpd_multitrack_review_preflight(rerun, request, "confirm")
  expect_true(preflight$eligible)
  confirmed <- stpd_multitrack_review_confirm(
    rerun, request, preflight$precondition_sha256,
    reviewer = "export_reviewer_2", reason = "reconfirmed after rerun",
    operation_id = "phase2b_export_fixture_confirm_run_2"
  )
  expect_equal(nrow(confirmed$product$transition_history), 2L)
  expect_identical(
    confirmed$product$transition_history$parent_preview_run_id,
    c("phase2b_export_test_run", new_run_id)
  )

  out_dir <- tempfile("phase2b_review_multirun_")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  active_paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    confirmed$dataset, out_dir
  )
  expect_identical(
    attr(active_paths, "multitrack_review_export_status"), "written"
  )
  expect_length(active_paths, 10L)

  stale_again <- confirmed$dataset
  stale_again$results$multitrack_review <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
      confirmed$product, "second_detector_rerun_test"
    )
  stale_paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_review(
    stale_again, out_dir
  )
  expect_identical(
    attr(stale_paths, "multitrack_review_export_status"),
    "written_stale_history_only"
  )
  expect_length(stale_paths, 10L)
})
