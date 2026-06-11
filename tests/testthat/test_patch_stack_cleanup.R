test_that("candidate audit rows are filtered before public ledger and decisions", {
  ds <- stpd_golden_test_dataset("middle_burst")
  p <- default_params()
  dat <- ds$trains$train_1
  dat$pattern_auto[4:6] <- "burst"
  ds$trains$train_1 <- dat
  ds$results <- list(
    candidate_diagnostic_audit = data.frame(
      candidate_id = c("selected_burst", "rejected_burst", "profile_row"),
      train = "train_1",
      start_isi = c(4L, 2L, 6L),
      end_isi = c(6L, 3L, 6L),
      candidate_layer = c("event_grammar", "event_grammar", "profile"),
      candidate_class = c("burst", "burst", "profile"),
      final_label = c("burst", "burst", "profile"),
      selected_for_auto = c(TRUE, TRUE, FALSE),
      written_to_auto = c(TRUE, FALSE, FALSE),
      action = c("accept", "reject", "audit_only"),
      rejection_reason = c("", "failed_gate", ""),
      stringsAsFactors = FALSE
    )
  )

  ledger <- build_candidate_ledger(ds, p, selected_trains = "train_1", run_id = "r", params_hash = "h")
  expect_equal(nrow(ledger), 1L)
  expect_equal(ledger$candidate_id, "selected_burst")
  expect_true(all(ledger$written_to_auto))
  expect_false(any(grepl("reject|profile", paste(ledger$action, ledger$rejection_reason, ledger$final_candidate_class), ignore.case = TRUE)))

  features <- compute_candidate_feature_table(ds, candidates = ledger, params = p, selected_trains = "train_1")
  decisions <- final_classify_candidates(features, p)
  expect_equal(nrow(features), 1L)
  expect_equal(nrow(decisions), 1L)
  expect_false(any(grepl("reject|profile", paste(decisions$final_candidate_class, decisions$decision_reason), ignore.case = TRUE)))
})

test_that("public final classification rejects diagnostic-looking rows", {
  p <- default_params()
  bad <- data.frame(
    candidate_id = "bad",
    train = "train_1",
    start_isi = 2L,
    end_isi = 4L,
    final_candidate_class = "burst",
    written_to_auto = TRUE,
    selected_for_auto = TRUE,
    action = "accept",
    rejection_reason = "failed_gate",
    stringsAsFactors = FALSE
  )
  one <- final_classify_candidate(bad, p)
  many <- final_classify_candidates(bad, p)
  expect_equal(nrow(one), 0L)
  expect_equal(nrow(many), 0L)
})

test_that("product hardening still attaches QC and frozen threshold outputs", {
  ds <- stpd_golden_test_dataset("middle_burst")
  out <- stpd_detect(ds, default_params(), selected_trains = "train_1")
  expect_true("pre_detection_quality" %in% names(out$results))
  expect_true("threshold_table" %in% names(out$results))
  expect_true(is.data.frame(out$results$pre_detection_quality))
  expect_true(is.data.frame(out$results$threshold_table))
  expect_false(any(grepl("(^v[0-9]|_v[0-9])", names(out$results), ignore.case = TRUE)))
})

test_that("event grammar and dataset-ISI fallbacks use semantic entry points", {
  ds <- stpd_golden_test_dataset("middle_burst")
  p <- default_params()
  p$event_grammar$enabled <- FALSE
  dat <- run_detector_train(ds$trains$train_1, p, train = "train_1")
  expect_true("pattern_auto" %in% names(dat))
  expect_true(length(dat$pattern_auto) == nrow(ds$trains$train_1))

  p2 <- default_params()
  p2$event_core$dataset_seed_band_enabled <- FALSE
  th <- stpd_seed_bridge_thresholds(ds$trains$train_1, p2, min_isi_sec = p2$detector$min_valid_isi_sec)
  expect_true(is.list(th))
  expect_true(all(c("core_thr", "bridge_thr") %in% names(th)))
})

test_that("namespace does not expose patch-stack implementation names", {
  ns_names <- ls(asNamespace("SpikeTrainPatternDetector"), all.names = TRUE)
  patch_stack <- paste0(
    "_", "pre", "_(audit|public|product|internal|derived|event|arbitration|dataset)|",
    "legacy", "_", "entrypoint|",
    "pre", "_public|",
    "pre", "_internal"
  )
  expect_false(any(grepl(patch_stack, ns_names)))
})

test_that("split server module installers share the live server environment", {
  expect_true(is.function(stpd_server_install_parameters_module))
  expect_true(is.function(stpd_server_install_data_io_module))
  expect_true(is.function(stpd_server_install_visualization_module))
  expect_true(is.function(stpd_server_install_detection_module))
  expect_true(is.function(stpd_server_install_ml_module))
  expect_true(is.function(stpd_server_install_export_module))
  shiny::testServer(server, {
    expect_true(exists("rv"))
    expect_true(is.function(get_dataset))
    expect_true(is.function(aligned_data))
    expect_true(is.function(run_detector_from_ui))
    expect_true(is.function(ml_feature_table_current))
    expect_true(is.function(export_event_csv))
  })
})
