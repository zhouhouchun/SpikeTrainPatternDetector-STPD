test_that("product parameters mirror into active detector fields", {
  d <- default_params()
  expect_equal(d$spiketrainpattern$pause$max_isi_sec, 0.150)
  expect_equal(d$pause$T_strong, 0.150)
  expect_true(all(c("event_core", "event_grammar", "arbitration") %in% names(d)))
  expect_false(any(grepl("^v[[:digit:]]+[[:alpha:]]?$", names(d))))
  expect_false(any(grepl(paste0("^", "v", "1[123]_"), names(d))))
  expect_false(any(grepl("^use_v[[:digit:]]+[[:alpha:]]?_arbitration$", names(d$detector))))

  p <- default_params()
  p$spiketrainpattern$pause$min_isi_sec <- 0.123
  p$spiketrainpattern$pause$max_isi_sec <- 0.234
  p$spiketrainpattern$high_frequency_tonic$max_isi_sec <- 0.041
  p$spiketrainpattern$high_frequency_spiking$short_isi_upper_sec <- 0.018
  p$spiketrainpattern$burst$allow_one_sided_as_canonical <- TRUE

  pp <- stpd_productize_params(p)
  expect_equal(pp$pause$T_seed, 0.123)
  expect_equal(pp$pause$T_strong, 0.234)
  expect_equal(pp$highfreq$T_high_max, 0.041)
  expect_equal(pp$highfreq$ISI_abs_max, 0.041)
  expect_equal(pp$highfreq$spiking_max_ISI_abs, 0.018)
  expect_true(pp$spiketrainpattern$burst$allow_one_sided_as_canonical)
  expect_equal(pp$event_core$seed_band_upper_sec, pp$spiketrainpattern$burst$seed_upper_sec)
  expect_equal(pp$event_grammar$allow_one_sided_burst_as_canonical, TRUE)
  expect_true(pp$arbitration$enabled)

  dat <- stpd_golden_test_dataset("middle_burst")$trains$train_1
  vp <- stpd_event_core_params(dat, pp, min_isi_sec = pp$detector$min_valid_isi_sec)
  expect_equal(vp$hf_spiking_short_upper, 0.018)
})

test_that("refractory suspect action survives product/public round-trip", {
  p <- default_params()
  p$detector$refractory_suspect_action <- "warn_only"
  p$burst$refractory_suspect_action <- "warn_only"

  pp <- stpd_productize_params(p, prefer = "legacy")
  expect_equal(pp$spiketrainpattern$qc$refractory_suspect_action, "warn_only")
  expect_equal(pp$detector$refractory_suspect_action, "warn_only")
  expect_equal(pp$burst$refractory_suspect_action, "warn_only")

  public <- stpd_public_params_only(pp)
  expect_equal(public$spiketrainpattern$qc$refractory_suspect_action, "warn_only")

  restored <- effective_params_for_detector(public)
  expect_equal(restored$detector$refractory_suspect_action, "warn_only")
  expect_equal(restored$burst$refractory_suspect_action, "warn_only")

  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"),
    pp,
    selected_trains = "train_1",
    collect_diagnostics = FALSE
  )
  expect_equal(out$params_last$spiketrainpattern$qc$refractory_suspect_action, "warn_only")
  expect_equal(effective_params_for_detector(out$params_last)$detector$refractory_suspect_action, "warn_only")
})

test_that("manual-locked candidates stay diagnostic and do not enter public auto tables", {
  ds <- stpd_golden_test_dataset("middle_burst")
  ds$trains$train_1$pattern_manual[4:6] <- "burst"

  out <- stpd_detect(
    ds,
    default_params(),
    selected_trains = "train_1",
    lock_manual = TRUE,
    collect_diagnostics = TRUE
  )

  expect_equal(as.character(out$trains$train_1$pattern_auto[4:6]), rep("", 3L))

  audit <- as.data.frame(out$results$candidate_diagnostic_audit)
  blocked <- as.logical(audit$auto_write_blocked_by_manual_lock)
  blocked[is.na(blocked)] <- FALSE
  expect_true(any(blocked))
  expect_true(any(as.character(audit$selection_status) == "blocked_by_manual_label", na.rm = TRUE))

  public_tables <- list(
    candidate_ledger = out$results$candidate_ledger,
    candidate_features = out$results$candidate_features,
    final_decisions = out$results$final_decisions,
    eventness_audit = out$results$eventness_audit
  )
  for (nm in names(public_tables)) {
    tab <- as.data.frame(public_tables[[nm]])
    if (nrow(tab) == 0) next
    txt <- apply(tab, 1L, paste, collapse = " ")
    expect_false(any(grepl("manual_lock|blocked_by_manual", txt, ignore.case = TRUE)), info = nm)
    if (all(c("start_isi", "end_isi") %in% names(tab))) {
      overlaps_manual <- suppressWarnings(as.integer(tab$start_isi)) <= 6L &
        suppressWarnings(as.integer(tab$end_isi)) >= 4L
      cls <- as.character(tab$final_candidate_class %||% tab$final_class %||% "")
      expect_false(any(overlaps_manual & cls %in% c("burst", "long_burst", "possible_burst"), na.rm = TRUE), info = nm)
    }
  }
})

test_that("legacy near-miss threshold edits survive product resolution", {
  p <- default_params()
  p$pause$T_seed <- 0.111
  p$pause$T_strong <- 0.222
  p$tonic$T_min <- 0.033
  p$tonic$T_max <- 0.077

  mirrored <- stpd_productize_params(p, prefer = "legacy")
  eff <- effective_params_for_detector(mirrored)

  expect_equal(eff$pause$T_seed, 0.111)
  expect_equal(eff$pause$T_strong, 0.222)
  expect_equal(eff$spiketrainpattern$pause$min_isi_sec, 0.111)
  expect_equal(eff$spiketrainpattern$pause$max_isi_sec, 0.222)
  expect_equal(eff$tonic$T_min, 0.033)
  expect_equal(eff$tonic$T_max, 0.077)
  expect_equal(eff$spiketrainpattern$tonic$min_isi_sec, 0.033)
  expect_equal(eff$spiketrainpattern$tonic$max_isi_sec, 0.077)
})

test_that("pause near-miss companion gates cover all required detector gates", {
  make_train <- function(isi) {
    ts <- c(0, cumsum(isi))
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }

  pause_idx <- 11L
  dat <- make_train(c(rep(0.040, 9L), 0.087, rep(0.040, 10L)))
  p <- default_params()
  p$detector$patterns_to_run <- "pause"
  p$pause$adaptive_use_train_ranges <- FALSE
  p$pause$anti_tonic_veto <- FALSE

  nm <- mine_pause_near_miss_train(dat, p$pause, min_isi_sec = 0.001, train = "pause_near", max_relax = 0.25)
  rows <- nm[as.character(nm$candidate_ref) == paste0("pause:", pause_idx), , drop = FALSE]

  expect_setequal(
    as.character(rows$parameter),
    c("pause_alpha", "pause_T_seed", "pause_global_median_factor")
  )

  path_for <- function(parameter) {
    switch(as.character(parameter),
           "pause_alpha" = "pause.alpha",
           "pause_T_seed" = "pause.T_seed",
           "pause_global_median_factor" = "pause.global_median_factor",
           stop("Unexpected parameter: ", parameter, call. = FALSE))
  }

  for (ii in seq_len(nrow(rows))) {
    p <- stpd_set_param(p, path_for(rows$parameter[ii]), as.numeric(rows$required_value[ii]))
  }
  p <- stpd_productize_params(p, prefer = "legacy")

  out <- run_detector_one_train(dat, p, min_isi_sec = 0.001, train = "pause_near")
  expect_equal(as.character(out$pattern_auto[pause_idx]), "pause")
})

test_that("seed-bridge diagnostics are available for active event-grammar runs", {
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"),
    default_params(),
    selected_trains = "train_1",
    collect_diagnostics = TRUE
  )

  expect_gt(nrow(out$results$seed_candidates), 0)
  expect_true(is.data.frame(out$results$bridge_candidates))
  expect_true(is.data.frame(out$results$burst_candidates))

  rebuilt <- build_near_miss_table(
    out,
    effective_params_for_detector(out$params_last),
    min_isi_sec = effective_params_for_detector(out$params_last)$detector$min_valid_isi_sec,
    target_trains = "train_1"
  )
  expect_true(is.data.frame(rebuilt))
  expect_true(all(c("pattern", "category", "parameter") %in% names(rebuilt)))

  make_train <- function(isi) {
    ts <- c(0, cumsum(isi))
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  bridge_ds <- make_dataset(
    "bridge_synthetic",
    "synthetic",
    list(train_1 = make_train(c(0.100, 0.005, 0.006, 0.020, 0.005, 0.006, 0.100))),
    unit_in = "s"
  )
  bridge_p <- default_params()
  bridge_p$burst$use_structure_candidates <- FALSE
  bridge_p$burst$T_seed <- 0.008
  bridge_p$burst$T_bridge <- 0.025
  bridge_p$burst$allow_bridge <- TRUE
  bridge_p$burst$connector_max_n <- 2L

  bridge_out <- stpd_detect(bridge_ds, bridge_p, selected_trains = "train_1", collect_diagnostics = TRUE)
  expect_gt(nrow(bridge_out$results$bridge_candidates), 0)
})

test_that("burst-associated regular packets are recorded as burst sublabels, not primary labels", {
  make_train <- function(isi, burst_rows) {
    ts <- c(0, cumsum(isi))
    dat <- data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
    dat$pattern_auto[burst_rows] <- "burst"
    dat
  }

  p <- effective_params_for_detector(default_params())$burst
  mine <- getFromNamespace("mine_structure_candidates", "SpikeTrainPatternDetector")

  before_dat <- make_train(
    c(0.110, 0.120, 0.105, 0.034, 0.035, 0.036, 0.034, 0.035, 0.036,
      0.005, 0.006, 0.0055, 0.006, 0.130, 0.120),
    burst_rows = 11:14
  )
  before_struct <- mine(before_dat, p, min_isi_sec = 0.001, train = "before")
  before_motif <- before_struct[before_struct$burst_sublabel == "interesting_structure", , drop = FALSE]
  expect_gt(nrow(before_motif), 0)
  expect_true(any(before_motif$burst_motif_type == "regular_before_burst"))
  expect_false(any(before_motif$structure_class %in% c("structure_seed", "possible_structure")))

  after_dat <- make_train(
    c(0.120, 0.110, 0.100, 0.005, 0.006, 0.0055, 0.006,
      0.034, 0.035, 0.036, 0.034, 0.035, 0.036, 0.120, 0.110),
    burst_rows = 5:8
  )
  after_struct <- mine(after_dat, p, min_isi_sec = 0.001, train = "after")
  after_motif <- after_struct[after_struct$burst_sublabel == "interesting_structure", , drop = FALSE]
  expect_gt(nrow(after_motif), 0)
  expect_true(any(after_motif$burst_motif_type == "regular_after_burst"))

  possible_only <- before_dat
  possible_only$pattern_auto[possible_only$pattern_auto == "burst"] <- "possible_burst"
  possible_struct <- mine(possible_only, p, min_isi_sec = 0.001, train = "possible")
  expect_false(any(possible_struct$burst_sublabel == "interesting_structure"))

  gapped_dat <- before_dat
  gapped_dat$ISI_sec[10] <- 0.120
  gapped_struct <- mine(gapped_dat, p, min_isi_sec = 0.001, train = "gapped")
  expect_false(any(gapped_struct$burst_sublabel == "interesting_structure"))
})

test_that("thresholds are resolved once at dataset scope before detection", {
  make_train <- function(isi) {
    ts <- c(0, cumsum(isi))
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "threshold_scope",
    source = "synthetic",
    unit_in = "s",
    trains = list(
      fast = make_train(rep(c(0.004, 0.006, 0.050, 0.070), 20)),
      slow = make_train(rep(c(0.020, 0.030, 0.090, 0.120), 20))
    )
  )

  out <- stpd_detect(ds, default_params(), collect_diagnostics = FALSE)
  expect_true("threshold_table" %in% names(out$results))

  tab <- out$results$threshold_table
  fast_tab <- attr(out$trains$fast, "event_grammar_params")$threshold_table
  slow_tab <- attr(out$trains$slow, "event_grammar_params")$threshold_table
  expect_equal(fast_tab$effective_sec, tab$effective_sec)
  expect_equal(slow_tab$effective_sec, tab$effective_sec)
})

test_that("manual lock prevents AUTO labels on manually labeled ISIs", {
  p <- default_params()
  ds_probe <- stpd_golden_test_dataset("middle_burst")
  probe <- stpd_detect(ds_probe, p, selected_trains = "train_1", lock_manual = FALSE)
  auto_idx <- which(as.character(probe$trains$train_1$pattern_auto) != "")
  expect_true(length(auto_idx) > 0)

  ds <- stpd_golden_test_dataset("middle_burst")
  locked_idx <- auto_idx[1]
  ds$trains$train_1$pattern_manual[locked_idx] <- "burst"
  out <- stpd_detect(ds, p, selected_trains = "train_1", lock_manual = TRUE)

  expect_equal(as.character(out$trains$train_1$pattern_auto[locked_idx]), "")
  final <- compute_final_pattern(
    out$trains$train_1$pattern_manual,
    out$trains$train_1$pattern_auto,
    out$trains$train_1$ISI_sec,
    min_isi_sec = out$params_last$spiketrainpattern$qc$artifact_min_valid_isi_sec
  )
  expect_equal(as.character(final[locked_idx]), "burst")
})

test_that("pre-detection QC can stop on data-integrity errors", {
  dat <- data.frame(
    idx = 1:4,
    timestamp_sec = c(0, 0.01, 0.01, 0.02),
    ISI_sec = c(NA_real_, 0.01, 0, 0.01),
    pattern_manual = rep("", 4),
    pattern_auto = rep("", 4),
    stringsAsFactors = FALSE
  )
  ds <- make_dataset("duplicate_timestamp", "synthetic", list(train_1 = dat), unit_in = "s")

  expect_error(
    stpd_detect(ds, default_params(), selected_trains = "train_1"),
    "Pre-detection QC"
  )

  p <- default_params()
  p$spiketrainpattern$engine$stop_on_qc_error <- FALSE
  out <- stpd_detect(ds, p, selected_trains = "train_1")
  expect_true("pre_detection_quality" %in% names(out$results))
  expect_true(any(out$results$pre_detection_quality$warning_level == "error"))
})

test_that("pre-detection QC stops when ISI_sec contains zero even if timestamps increase", {
  dat <- data.frame(
    idx = 1:4,
    timestamp_sec = c(0, 0.01, 0.02, 0.03),
    ISI_sec = c(NA_real_, 0.01, 0, 0.01),
    pattern_manual = rep("", 4),
    pattern_auto = rep("", 4),
    stringsAsFactors = FALSE
  )
  ds <- make_dataset("zero_isi_column", "synthetic", list(train_1 = dat), unit_in = "s")

  expect_error(
    stpd_detect(ds, default_params(), selected_trains = "train_1"),
    "zero_or_negative_ISI=1"
  )
})
