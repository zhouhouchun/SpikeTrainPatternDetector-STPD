make_structure_first_train <- function(isi_sec) {
  timestamp_sec <- c(0, cumsum(isi_sec))
  n <- length(timestamp_sec)
  data.frame(
    idx = seq_len(n),
    timestamp_sec = timestamp_sec,
    ISI_sec = c(NA_real_, isi_sec),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
}

structure_first_candidates <- function(dat, params = default_params_sec(),
                                       min_isi_sec = 0.0009, vp = NULL) {
  if (is.null(vp)) {
    vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(
      dat, params, min_isi_sec = min_isi_sec
    )
  }
  SpikeTrainPatternDetector:::stpd_event_core_structure_first_burst_candidates(
    dat, params, vp, min_isi_sec = min_isi_sec, train = "test_train"
  )
}

test_that("structure-first recovers a locally separated burst outside fixed seed and bridge bands", {
  dat <- make_structure_first_train(c(
    0.100, 0.100,
    0.029, 0.030, 0.028,
    0.100, 0.110, 0.100, 0.090, 0.100
  ))
  params <- default_params_sec()
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(dat, params, 0.0009)
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015

  structure <- structure_first_candidates(dat, params, vp = vp)
  target <- structure[structure$start_isi == 4L & structure$end_isi == 6L, , drop = FALSE]
  expect_equal(nrow(target), 1L)
  expect_identical(target$final_label, "burst")
  expect_true(target$structure_first_no_seed_band_gate)
  expect_gt(target$intra_q90_sec, vp$bridge_high)
  expect_gte(target$pre_ratio_q90, 3)
  expect_gte(target$post_ratio_q90, 3)
  expect_equal(target$structure_first_compact_upper_sec, 0.035, tolerance = 1e-12)
  expect_true(is.na(target$q90_bridge_pass))
  expect_match(target$decision_path, "no_default_burst_seed_band_used=true", fixed = TRUE)
  expect_match(target$threshold_source_summary, "seed_band_not_used", fixed = TRUE)

  # With the new first stage disabled, the same train has neither a seed-band
  # candidate nor a dense-episode rescue candidate under these fixed bands.
  disabled <- params
  disabled$spiketrainpattern$burst$structure_first_enabled <- FALSE
  fallback_only <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_dispatch(
    dat, disabled, vp, min_isi_sec = 0.0009, train = "test_train"
  )
  expect_equal(nrow(fallback_only), 0L)

  enabled <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, min_isi_sec = 0.0009, train = "test_train"
  )
  expect_true(any(enabled$candidate_layer == "structure_first_burst_screen"))
})

test_that("the four-spike contrast floor is not a universal Burst definition", {
  dat <- make_structure_first_train(c(
    0.080, 0.080,
    0.008, 0.009,
    0.080, 0.080
  ))
  params <- default_params_sec()
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(
    dat, params, min_isi_sec = 0.0009
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.012
  vp$bridge_high <- 0.012
  vp$min_spikes <- 3L
  vp$classic_max_spikes <- 10L
  vp$S <- 3
  vp$S_possible <- 1.5

  # Two ISIs (three spikes) cannot enter through the initial contrast-only
  # structure-first generator.
  structure <- structure_first_candidates(dat, params, vp = vp)
  expect_equal(nrow(structure), 0L)

  # The independent seed/threshold event-grammar route retains its own
  # evidence contract and may still accept the same three-spike Burst.  Thus
  # the screening floor does not become a universal final-classification veto.
  all_routes <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_final_impl(
      dat, params, vp, min_isi_sec = 0.0009, train = "scope_boundary"
    )
  target <- all_routes[
    all_routes$start_isi == 4L & all_routes$end_isi == 5L,
    , drop = FALSE
  ]
  expect_equal(nrow(target), 1L)
  expect_identical(target$n_spikes, 3L)
  expect_identical(target$final_label, "burst")
  expect_identical(target$action, "accept")
  expect_identical(target$candidate_layer, "event_grammar_burst_event")
})

test_that("train-adaptive compactness prevents uniform tonic packets from passing on contrast alone", {
  uniform_tonic <- make_structure_first_train(rep(0.030, 20L))
  expect_equal(nrow(structure_first_candidates(uniform_tonic)), 0L)

  # Long flanks around a still-tonic-scale packet are insufficient when the
  # candidate is not compact relative to the train's own Q80 reference.
  tonic_packet <- make_structure_first_train(c(
    rep(0.030, 8L), 0.100,
    0.030, 0.030, 0.030,
    0.100, rep(0.030, 8L)
  ))
  expect_equal(nrow(structure_first_candidates(tonic_packet)), 0L)
})

test_that("Mac compactness boundary semantics are preserved", {
  # When Q80 is exactly the artifact floor, the train has no usable adaptive
  # background scale. The Mac screen leaves the compactness gate unavailable;
  # it must not clamp candidates to the artifact floor.
  floor_context <- make_structure_first_train(c(
    rep(0.0009, 20L), 0.010,
    rep(0.0010, 3L),
    0.010, rep(0.0009, 20L)
  ))
  floor_candidates <- structure_first_candidates(floor_context, min_isi_sec = 0.0009)
  floor_target <- floor_candidates[
    floor_candidates$start_isi == 23L & floor_candidates$end_isi == 25L,
    , drop = FALSE
  ]
  expect_equal(nrow(floor_target), 1L)
  expect_equal(floor_target$structure_first_train_quantile_sec, 0.0009, tolerance = 1e-12)
  expect_false(floor_target$structure_first_compactness_gate_active)
  expect_true(is.na(floor_target$structure_first_compact_upper_sec))

  # Mirror the Mac numeric tolerance at the compactness upper: a value only a
  # few floating-point ulps above U is still on the boundary, not a rejection.
  near_upper <- make_structure_first_train(c(
    rep(0.100, 10L), 0.110,
    rep(0.03500002, 3L),
    0.110, rep(0.100, 10L)
  ))
  near_candidates <- structure_first_candidates(near_upper)
  near_target <- near_candidates[
    near_candidates$start_isi == 13L & near_candidates$end_isi == 15L,
    , drop = FALSE
  ]
  expect_equal(nrow(near_target), 1L)
  expect_true(near_target$structure_first_q90_compact_pass)
  expect_true(near_target$structure_first_max_intra_strict_pass)
})

test_that("tolerated internal tails require a two-sided packet and stay within 1.25 U", {
  tolerated <- make_structure_first_train(c(
    rep(0.100, 10L), 0.130,
    rep(0.020, 7L), 0.040,
    0.130, rep(0.100, 10L)
  ))
  tolerated_candidates <- structure_first_candidates(tolerated)
  target <- tolerated_candidates[
    tolerated_candidates$start_isi == 13L & tolerated_candidates$end_isi == 20L,
    , drop = FALSE
  ]
  expect_equal(nrow(target), 1L)
  expect_true(target$structure_first_tolerated_tail_pass)
  expect_false(target$structure_first_max_intra_strict_pass)
  expect_lte(target$structure_first_internal_tail_ratio, 1.25)
  expect_identical(target$structure_first_anchor_band_source, "structure")
  expect_equal(target$structure_first_anchor_band_lower_sec, 0.0009)
  expect_equal(target$structure_first_anchor_band_upper_sec, target$intra_q90_sec)
  expect_identical(target$structure_first_min_train_valid_isi, 8L)

  excessive <- make_structure_first_train(c(
    rep(0.100, 10L), 0.140,
    rep(0.020, 7L), 0.045,
    0.140, rep(0.100, 10L)
  ))
  excessive_candidates <- structure_first_candidates(excessive)
  expect_false(any(
    excessive_candidates$start_isi == 13L & excessive_candidates$end_isi == 20L
  ))

  # A true recording endpoint has only one observable flank and therefore
  # cannot use the two-sided tolerated-tail rescue.
  endpoint_tail <- make_structure_first_train(c(
    rep(0.020, 7L), 0.040,
    0.130, rep(0.100, 10L)
  ))
  endpoint_candidates <- structure_first_candidates(endpoint_tail)
  expect_false(any(
    endpoint_candidates$start_isi == 2L & endpoint_candidates$end_isi == 9L
  ))
})

test_that("structure-first geometry is independent of soft threshold values and rejects weak flanks", {
  dat <- make_structure_first_train(c(
    0.100, 0.100, 0.029, 0.030, 0.028, 0.100, 0.110, 0.100, 0.090, 0.100
  ))
  params <- default_params_sec()
  low <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(dat, params, 0.0009)
  high <- low
  low$seed_low <- 0.001; low$seed_high <- 0.005; low$bridge_high <- 0.008
  high$seed_low <- 0.001; high$seed_high <- 0.020; high$bridge_high <- 0.025
  a <- structure_first_candidates(dat, params, vp = low)
  b <- structure_first_candidates(dat, params, vp = high)
  cols <- c("start_isi", "end_isi", "final_label", "score", "pre_ratio_q90", "post_ratio_q90")
  expect_equal(a[, cols, drop = FALSE], b[, cols, drop = FALSE])

  weak <- make_structure_first_train(c(
    0.100, 0.100, 0.029, 0.030, 0.028, 0.085, 0.110, 0.100, 0.090, 0.100
  ))
  weak_candidates <- structure_first_candidates(weak, params)
  expect_false(any(weak_candidates$start_isi == 4L & weak_candidates$end_isi == 6L))
})

test_that("artifact ISIs split structure windows while refractory suspects are handled afterward", {
  artifact_dat <- make_structure_first_train(c(
    0.100, 0.100, 0.002, 0.0008, 0.002, 0.100, 0.110, 0.100, 0.090, 0.100
  ))
  artifact_candidates <- structure_first_candidates(artifact_dat)
  if (nrow(artifact_candidates) > 0L) {
    expect_false(any(artifact_candidates$start_isi <= 5L & artifact_candidates$end_isi >= 5L))
  }

  suspect_dat <- make_structure_first_train(c(
    0.100, 0.100, 0.0020, 0.00095, 0.0020, 0.100, 0.110, 0.100, 0.090, 0.100
  ))
  params <- default_params_sec()
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(suspect_dat, params, 0.0009)
  candidates <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_dispatch(
    suspect_dat, params, vp, min_isi_sec = 0.0009, train = "suspect"
  )
  structure <- candidates[candidates$candidate_layer == "structure_first_burst_screen", , drop = FALSE]
  expect_true(any(structure$refractory_suspect_n > 0L))
  expect_true(any(structure$final_label == "possible_burst"))
  expect_true(any(structure$refractory_suspect_policy_applied))
  expect_true(any(structure$refractory_suspect_action == "demoted_to_possible_burst"))
})

test_that("structure-first decisions are scale invariant when the artifact floor is scaled", {
  isi <- c(0.100, 0.100, 0.029, 0.030, 0.028, 0.100, 0.110, 0.100, 0.090, 0.100)
  params <- default_params_sec()
  original <- structure_first_candidates(
    make_structure_first_train(isi), params, min_isi_sec = 0.0009
  )
  scaled <- structure_first_candidates(
    make_structure_first_train(10 * isi), params, min_isi_sec = 0.009
  )

  cols <- c("start_isi", "end_isi", "final_label", "boundary_type")
  expect_equal(original[, cols, drop = FALSE], scaled[, cols, drop = FALSE])
  expect_equal(original$pre_ratio_q90, scaled$pre_ratio_q90, tolerance = 1e-12)
  expect_equal(original$post_ratio_q90, scaled$post_ratio_q90, tolerance = 1e-12)
  expect_equal(
    10 * original$structure_first_compact_upper_sec,
    scaled$structure_first_compact_upper_sec,
    tolerance = 1e-12
  )
})

test_that("endpoint candidates require the sole observable flank and remain configurable", {
  dat <- make_structure_first_train(c(
    0.029, 0.030, 0.028,
    0.100, 0.110, 0.100, 0.090, 0.100, 0.100
  ))
  params <- default_params_sec()
  endpoint <- structure_first_candidates(dat, params)
  target <- endpoint[endpoint$start_isi == 2L & endpoint$end_isi == 4L, , drop = FALSE]
  expect_equal(nrow(target), 1L)
  expect_identical(target$boundary_type, "train_start_endpoint")
  expect_true(target$one_sided_boundary_pass)
  expect_false(target$strict_boundary_pass)
  expect_identical(target$final_label, "possible_burst")

  no_endpoint <- params
  no_endpoint$spiketrainpattern$burst$structure_first_allow_endpoint <- FALSE
  disabled <- structure_first_candidates(dat, no_endpoint)
  expect_false(any(disabled$start_isi == 2L & disabled$end_isi == 4L))
})

test_that("label-blind structure-first candidates ignore manual positive and negative fields", {
  dat <- make_structure_first_train(c(
    0.100, 0.100, 0.029, 0.030, 0.028, 0.100, 0.110, 0.100, 0.090, 0.100
  ))
  unlabelled <- SpikeTrainPatternDetector:::make_dataset(
    "structure_first_unlabelled", "synthetic", list(train_1 = dat), unit_in = "s"
  )
  labelled <- unlabelled
  labelled$trains$train_1$pattern_manual[4:6] <- "burst"
  labelled$trains$train_1$pattern_manual_negative[4:6] <- "not_burst"

  out_unlabelled <- stpd_detect(
    unlabelled, default_params_sec(), lock_manual = FALSE,
    collect_diagnostics = TRUE, label_blind = TRUE
  )
  out_labelled <- stpd_detect(
    labelled, default_params_sec(), lock_manual = FALSE,
    collect_diagnostics = TRUE, label_blind = TRUE
  )

  select_structure <- function(out) {
    audit <- as.data.frame(out$results$candidate_diagnostic_audit)
    audit <- audit[audit$candidate_layer == "structure_first_burst_screen", , drop = FALSE]
    audit <- audit[order(audit$train, audit$start_isi, audit$end_isi), , drop = FALSE]
    audit[, c("train", "start_isi", "end_isi", "final_label", "score",
              "pre_ratio_q90", "post_ratio_q90"), drop = FALSE]
  }
  expect_equal(select_structure(out_labelled), select_structure(out_unlabelled))
  labelled_audit <- as.data.frame(out_labelled$results$candidate_diagnostic_audit)
  expect_true(any(
    labelled_audit$candidate_layer == "structure_first_burst_screen" &
      as.logical(labelled_audit$selected_for_auto),
    na.rm = TRUE
  ))
  expect_identical(
    as.character(out_labelled$trains$train_1$pattern_auto),
    as.character(out_unlabelled$trains$train_1$pattern_auto)
  )
})

test_that("structure-first settings are reported and contribute to the parameter hash", {
  params <- default_params_sec()
  report <- stpd_parameter_report(params)
  expect_true(any(report$section == "Burst structure-first" & report$parameter == "flank_contrast_min"))

  changed <- params
  changed$spiketrainpattern$burst$structure_first_contrast_min <- 4.0
  expect_false(identical(stpd_params_hash(params), stpd_params_hash(changed)))
  expect_false(any(stpd_validate_params(changed)$severity == "error"))

  min_changed <- params
  min_changed$spiketrainpattern$burst$classic_min_spikes <- 5L
  effective <- effective_params_for_detector(min_changed)
  expect_identical(effective$event_core$min_spikes, 5L)
})

test_that("strict structure evidence remains auditable when a strong HFS state wins AUTO", {
  isi <- rep(0.006, 300L)
  isi[149:151] <- c(0.0018, 0.0018, 0.0018)
  dat <- make_structure_first_train(isi)
  out <- SpikeTrainPatternDetector:::stpd_detect_train_hf_protected_impl(
    dat, default_params_sec(), min_isi_sec = 0.0009,
    train = "hfs_with_structure", lock_manual = FALSE
  )
  audit <- attr(out, "candidate_diagnostic_audit")
  structure <- audit[
    audit$candidate_layer == "structure_first_burst_screen" &
      as.logical(audit$strict_boundary_pass),
    , drop = FALSE
  ]
  expect_gt(nrow(structure), 0L)
  expect_true(all(as.logical(structure$suppressed_by_hf_spiking_state)))
  expect_true(all(structure$suppressed_original_label %in% c("burst", "long_burst")))
  expect_true(all(structure$final_label == "reject"))
  expect_true(all(grepl(
    "structure_first_evidence_preserved_in_audit",
    structure$decision_path,
    fixed = TRUE
  )))
  expect_true(any(audit$final_label == "high_frequency_spiking" & audit$selected_for_auto))
})

test_that("disabled structure-first mode is reported consistently in profile and run metadata", {
  dat <- make_structure_first_train(rep(0.030, 20L))
  params <- default_params_sec()
  params$spiketrainpattern$burst$structure_first_enabled <- FALSE
  effective <- effective_params_for_detector(params)
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(dat, effective, 0.0009)
  profile <- SpikeTrainPatternDetector:::stpd_event_core_train_profile_row(
    dat, effective, vp, min_isi_sec = 0.0009, train = "disabled"
  )
  expect_identical(profile$initial_burst_screen, "seed_band_threshold_centred")
  expect_match(profile$decision_path, "structure_first_disabled", fixed = TRUE)
  expect_false(grepl("precedes_seed_band", profile$decision_path, fixed = TRUE))

  ds <- SpikeTrainPatternDetector:::make_dataset(
    "structure_first_disabled", "synthetic", list(train_1 = dat), unit_in = "s"
  )
  out <- stpd_detect(ds, params, collect_diagnostics = TRUE, label_blind = TRUE)
  meta <- out$results$run_metadata_public
  expect_false(meta$structure_first_configured)
  expect_false(meta$structure_first_executed)
  expect_false(meta$structure_first_enabled)
  expect_identical(meta$initial_burst_screen, "seed_band_threshold_centred")
  expect_match(meta$burst_candidate_stage_order, "threshold_centred", fixed = TRUE)
})

test_that("the combined burst-stage candidate budget retains strict structure anchors", {
  ds <- SpikeTrainPatternDetector:::stpd_golden_test_dataset("middle_burst")
  dat <- ds$trains$train_1
  params <- default_params_sec()
  min_isi <- params$detector$min_valid_isi_sec
  vp <- SpikeTrainPatternDetector:::stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = min_isi, train = "train_1"
  )
  vp$max_candidates <- 1L

  dispatched <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, min_isi_sec = min_isi, train = "train_1", pipeline = "final"
  )
  expect_equal(nrow(dispatched), 1L)
  expect_identical(as.character(dispatched$candidate_layer), "structure_first_burst_screen")
  expect_true(as.logical(dispatched$strict_boundary_pass))

  direct <- SpikeTrainPatternDetector:::stpd_event_core_detect_burst_events(
    dat, params, vp, min_isi_sec = min_isi, train = "train_1"
  )
  expect_equal(nrow(direct), 1L)
  expect_identical(as.character(direct$candidate_layer), "structure_first_burst_screen")
  expect_true(as.logical(direct$strict_boundary_pass))

  repeated <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, min_isi_sec = min_isi, train = "train_1", pipeline = "final"
  )
  expect_equal(
    dispatched[, c("candidate_id", "candidate_layer", "start_isi", "end_isi"), drop = FALSE],
    repeated[, c("candidate_id", "candidate_layer", "start_isi", "end_isi"), drop = FALSE]
  )
})

test_that("run metadata distinguishes configured from executed structure-first screening", {
  ds <- SpikeTrainPatternDetector:::stpd_golden_test_dataset("middle_burst")

  legacy_params <- default_params_sec()
  legacy_params$detector$train_pipeline <- "near_miss_augmented"
  legacy <- stpd_detect(
    ds, legacy_params, collect_diagnostics = TRUE, label_blind = TRUE
  )
  legacy_meta <- legacy$results$run_metadata_public
  expect_true(legacy_meta$structure_first_configured)
  expect_false(legacy_meta$structure_first_executed)
  expect_false(legacy_meta$structure_first_enabled)
  expect_identical(
    legacy_meta$initial_burst_screen,
    "pipeline_specific__near_miss_augmented"
  )
  legacy_audit <- as.data.frame(legacy$results$candidate_diagnostic_audit)
  expect_false(any(
    as.character(legacy_audit$candidate_layer) == "structure_first_burst_screen",
    na.rm = TRUE
  ))

  no_burst_params <- default_params_sec()
  no_burst_params$detector$patterns_to_run <- c("tonic", "pause")
  no_burst <- stpd_detect(
    ds, no_burst_params, collect_diagnostics = TRUE, label_blind = TRUE
  )
  no_burst_meta <- no_burst$results$run_metadata_public
  expect_true(no_burst_meta$structure_first_configured)
  expect_false(no_burst_meta$structure_first_executed)
  expect_false(no_burst_meta$structure_first_enabled)
  expect_false(no_burst_meta$burst_family_requested)
  expect_identical(
    no_burst_meta$initial_burst_screen,
    "not_run_burst_family_disabled"
  )
})
