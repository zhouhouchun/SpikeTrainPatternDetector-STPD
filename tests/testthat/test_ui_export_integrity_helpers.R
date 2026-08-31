export_integrity_make_dataset <- function(id = "dataset_a", run_id = "run_a") {
  params <- default_params()
  train <- data.frame(
    idx = 1:5,
    timestamp_sec = c(0, 0.01, 0.03, 0.07, 0.12),
    ISI_sec = c(NA_real_, 0.01, 0.02, 0.04, 0.05),
    pattern_manual = rep("", 5),
    pattern_manual_negative = rep("", 5),
    pattern_auto = c("", "burst", "burst", "", ""),
    auto_score = c(NA_real_, 0.9, 0.8, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  phash <- stpd_params_hash(params)
  result_row <- data.frame(
    train = "train_1", start_isi = 2L, end_isi = 3L,
    score = 0.8, run_id = run_id, params_hash = phash,
    stringsAsFactors = FALSE
  )
  ds <- list(
    trains = list(train_1 = train),
    task_events = data.frame(),
    train_settings = list(),
    params_effective = params,
    meta = list(
      display_name = id,
      input_sha256 = paste(rep(substr(id, 1L, 1L), 64L), collapse = "")
    ),
    results = list(
      events = result_row,
      candidate_ledger = result_row,
      event_ledger = result_row,
      candidate_features = result_row,
      final_classification_audit = result_row,
      run_metadata_public = data.frame(
        run_id = run_id,
        params_hash = phash,
        dataset_name = id,
        input_sha256 = paste(rep(substr(id, 1L, 1L), 64L), collapse = ""),
        total_train_n = 1L,
        selected_train_n = 1L,
        selected_trains = "train_1",
        stringsAsFactors = FALSE
      )
    )
  )
  list(dataset = ds, params = params)
}

test_that("detector-output identity is fixed-schema, deterministic, and tamper-sensitive", {
  fixture <- export_integrity_make_dataset()
  identity <- stpd_ui_detector_output_identity(fixture$dataset)

  expect_s3_class(identity, "stpd_ui_detector_output_identity")
  expect_identical(identity$schema_version, "stpd_ui_detector_output_v2")
  expect_true(identity$verifiable)
  expect_match(identity$combined_sha256, "^[0-9a-f]{64}$")
  expect_identical(identity$selected_trains, "train_1")
  expect_true(all(c(
    "train_auto_output", "events", "candidate_ledger", "event_ledger",
    "candidate_features", "final_classification_audit",
    "run_metadata_public"
  ) %in% identity$artifact_names))
  expect_identical(identity$artifact_names, identity$artifacts$artifact)
  expect_length(identity$missing_required, 0L)

  reordered <- fixture$dataset
  reordered$results <- reordered$results[rev(names(reordered$results))]
  reordered$results$candidate_ledger <-
    reordered$results$candidate_ledger[, rev(names(reordered$results$candidate_ledger))]
  expect_identical(
    stpd_ui_detector_output_identity(reordered)$combined_sha256,
    identity$combined_sha256
  )

  tampered <- fixture$dataset
  tampered$results$candidate_ledger$score <- 0.1
  state <- stpd_ui_detector_output_state(tampered, identity)
  expect_false(state$current)
  expect_identical(state$code, "detector_output_changed")
  expect_identical(state$changed_artifacts, "candidate_ledger")
  expect_false(identical(state$current_sha256, state$expected_sha256))

  missing <- fixture$dataset
  missing$results$candidate_ledger <- NULL
  missing_identity <- stpd_ui_detector_output_identity(missing)
  expect_false(missing_identity$verifiable)
  expect_true("candidate_ledger" %in% missing_identity$missing_required)
  expect_identical(
    stpd_ui_detector_output_state(missing, identity)$code,
    "detector_output_identity_unverifiable"
  )
})

test_that("detector-output identity binds the explicit train scope", {
  fixture <- export_integrity_make_dataset()
  empty <- stpd_ui_detector_output_identity(
    fixture$dataset, selected_trains = character()
  )
  expect_false(empty$verifiable)
  expect_true("empty_scope" %in% empty$reason_codes)

  identity <- stpd_ui_detector_output_identity(fixture$dataset)
  wrong_scope <- identity
  wrong_scope$selected_trains <- "other_train"
  wrong_scope$selected_train_n <- 1L
  state <- stpd_ui_detector_output_state(fixture$dataset, wrong_scope)
  expect_false(state$current)
  expect_identical(state$code, "detector_output_scope_invalid")
})

test_that("validation artifacts are included only when their identity is current", {
  fixture <- export_integrity_make_dataset()
  ds <- fixture$dataset
  params <- fixture$params
  dataset_identity <- stpd_ui_dataset_identity(ds, "dataset_a")
  run_identity <- stpd_ui_run_identity(
    ds, params = params, dataset_id = "dataset_a",
    dataset_identity = dataset_identity
  )
  validation <- list(metadata = data.frame(
    schema_version = "validation_fixture_v1", stringsAsFactors = FALSE
  ))
  validation_identity <- stpd_ui_validation_identity(
    validation,
    dataset_identity = dataset_identity,
    run_identity = run_identity,
    selected_trains = "train_1",
    truth_dependencies = character(),
    source_kind = "run_bound",
    source_params_sha256 = stpd_params_hash(params),
    dataset = ds
  )

  current <- stpd_ui_validation_export_decision(
    ds = ds, validation = validation, params = params,
    dataset_id = "dataset_a", run_identity = run_identity,
    validation_identity = validation_identity,
    dataset_identity = dataset_identity
  )
  expect_identical(current$action, "include")
  expect_true(current$include)
  expect_true(current$current)
  expect_identical(current$code, "validation_export_current")
  expect_true(nzchar(current$reason))

  tampered_validation <- validation
  tampered_validation$metrics <- data.frame(
    metric = "fabricated", value = 999, stringsAsFactors = FALSE
  )
  tampered <- stpd_ui_validation_export_decision(
    ds = ds, validation = tampered_validation, params = params,
    dataset_id = "dataset_a", run_identity = run_identity,
    validation_identity = validation_identity,
    dataset_identity = dataset_identity,
    stale_policy = "omit"
  )
  expect_identical(tampered$action, "omit")
  expect_false(tampered$include)
  expect_identical(tampered$code, "validation_export_report_changed")

  legacy_identity <- validation_identity
  legacy_identity$report_sha256 <- NULL
  unverifiable_report <- stpd_ui_validation_export_decision(
    ds = ds, validation = validation, params = params,
    dataset_id = "dataset_a", run_identity = run_identity,
    validation_identity = legacy_identity,
    dataset_identity = dataset_identity,
    stale_policy = "block"
  )
  expect_identical(unverifiable_report$action, "block")
  expect_identical(
    unverifiable_report$code,
    "validation_export_report_identity_unverifiable"
  )

  stale_identity <- validation_identity
  stale_identity$source_run_id <- "old_run"
  omitted <- stpd_ui_validation_export_decision(
    ds = ds, validation = validation, params = params,
    dataset_id = "dataset_a", run_identity = run_identity,
    validation_identity = stale_identity,
    dataset_identity = dataset_identity,
    stale_policy = "omit"
  )
  expect_identical(omitted$action, "omit")
  expect_false(omitted$include)
  expect_false(omitted$current)
  expect_match(omitted$code, "validation_export_omitted_")
  expect_match(omitted$reason, "validation_run_changed", fixed = TRUE)

  blocked <- stpd_ui_validation_export_decision(
    ds = ds, validation = validation, params = params,
    dataset_id = "dataset_a", run_identity = run_identity,
    validation_identity = stale_identity,
    dataset_identity = dataset_identity,
    stale_policy = "block"
  )
  expect_identical(blocked$action, "block")
  expect_false(blocked$include)
  expect_match(blocked$code, "validation_export_blocked_")

  absent <- stpd_ui_validation_export_decision(
    ds = ds, validation = NULL, params = params,
    dataset_id = "dataset_a", run_identity = run_identity,
    dataset_identity = dataset_identity
  )
  expect_identical(absent$action, "omit")
  expect_identical(absent$code, "validation_export_absent")
})

test_that("batch export plan fails closed per dataset and never shares parameters", {
  a <- export_integrity_make_dataset("dataset_a", "run_a")
  b <- export_integrity_make_dataset("dataset_b", "run_b")
  datasets <- list(dataset_a = a$dataset, dataset_b = b$dataset)
  params <- list(dataset_a = a$params, dataset_b = b$params)
  runs <- lapply(names(datasets), function(id) {
    stpd_ui_run_identity(datasets[[id]], params[[id]], dataset_id = id)
  })
  names(runs) <- names(datasets)
  outputs <- lapply(datasets, stpd_ui_detector_output_identity)

  plan <- stpd_ui_batch_export_plan(
    datasets = datasets,
    params_by_dataset = params,
    run_identities = runs,
    expected_detector_outputs = outputs,
    output_root = "/tmp/batch-fixture"
  )
  expect_true(plan$eligible)
  expect_identical(plan$code, "batch_export_ready")
  expect_identical(nrow(plan$failures), 0L)
  expect_length(plan$eligible_entries, 2L)
  expect_false(identical(
    plan$entries$dataset_a$directory_name,
    plan$entries$dataset_b$directory_name
  ))
  expect_identical(
    plan$entries$dataset_a$params_sha256,
    stpd_params_hash(a$params)
  )

  missing_run <- runs
  missing_run$dataset_b <- NULL
  blocked <- stpd_ui_batch_export_plan(
    datasets = datasets,
    params_by_dataset = params,
    run_identities = missing_run,
    expected_detector_outputs = outputs
  )
  expect_false(blocked$eligible)
  expect_identical(blocked$code, "batch_export_blocked")
  expect_true("dataset_b" %in% blocked$failures$dataset_id)
  expect_false("dataset_b" %in% names(blocked$eligible_entries))

  changed <- a$params
  changed$spiketrainpattern$qc$artifact_min_valid_isi_sec <-
    changed$spiketrainpattern$qc$artifact_min_valid_isi_sec + 0.0001
  stale_params <- params
  stale_params$dataset_b <- changed
  blocked_params <- stpd_ui_batch_export_plan(
    datasets = datasets,
    params_by_dataset = stale_params,
    run_identities = runs,
    expected_detector_outputs = outputs
  )
  expect_false(blocked_params$eligible)
  expect_true("dataset_b" %in% blocked_params$failures$dataset_id)

  tampered <- datasets
  tampered$dataset_b$results$candidate_features$score <- 0.2
  blocked_output <- stpd_ui_batch_export_plan(
    datasets = tampered,
    params_by_dataset = params,
    run_identities = runs,
    expected_detector_outputs = outputs
  )
  expect_false(blocked_output$eligible)
  expect_true("dataset_b" %in% blocked_output$failures$dataset_id)
  expect_identical(
    blocked_output$entries$dataset_b$detector_output_state$code,
    "detector_output_changed"
  )
})

test_that("safe directory and temporary workspace helpers prevent collisions", {
  ids <- c("a/b", "a?b", "..", "a/b")
  safe <- stpd_ui_export_safe_directory_names(ids)
  expect_length(safe, length(ids))
  expect_false(anyDuplicated(unname(safe)) > 0L)
  expect_false(any(grepl("[/\\\\]", safe)))
  expect_false(any(safe %in% c("", ".", "..")))
  expect_true(all(nzchar(safe)))

  root <- tempfile("integrity_tmp_root_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  one <- stpd_ui_export_temp_path(
    prefix = "batch export", tmpdir = root, create = "directory"
  )
  two <- stpd_ui_export_temp_path(
    prefix = "batch export", tmpdir = root, create = "directory"
  )
  expect_true(dir.exists(one))
  expect_true(dir.exists(two))
  expect_false(identical(one, two))
  expect_identical(dirname(one), normalizePath(root, winslash = "/"))
})

test_that("ZIP validation rejects impostors and verifies required sentinels", {
  fake <- tempfile(fileext = ".zip")
  on.exit(unlink(fake), add = TRUE)
  writeLines("not a zip", fake)
  invalid <- stpd_ui_validate_zip_archive(
    fake, required_files = "Export_run_metadata.csv",
    sentinel_files = "Export_run_metadata.csv"
  )
  expect_false(invalid$valid)
  expect_true(invalid$code %in% c("zip_invalid_archive", "zip_unreadable"))

  source_dir <- tempfile("zip_integrity_source_")
  dir.create(source_dir)
  on.exit(unlink(source_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("run_id,params_hash\nr1,abc", file.path(source_dir, "Export_run_metadata.csv"))
  writeLines("train,event\nt1,burst", file.path(source_dir, "Events.csv"))
  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)
  old <- setwd(source_dir)
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(utils::zip(
    zipfile = zip_file,
    files = c("Export_run_metadata.csv", "Events.csv")
  ))
  setwd(old)

  valid <- stpd_ui_validate_zip_archive(
    zip_file,
    required_files = c("Export_run_metadata.csv", "Events.csv"),
    sentinel_files = "Export_run_metadata.csv"
  )
  expect_true(valid$valid)
  expect_identical(valid$code, "zip_valid")
  expect_identical(valid$missing_files, character())
  expect_identical(valid$empty_sentinels, character())
  expect_true(valid$sentinel_verified)

  missing <- stpd_ui_validate_zip_archive(
    zip_file,
    required_files = c("Export_run_metadata.csv", "Missing.csv"),
    sentinel_files = "Export_run_metadata.csv"
  )
  expect_false(missing$valid)
  expect_identical(missing$code, "zip_required_file_missing")
  expect_identical(missing$missing_files, "Missing.csv")
})
