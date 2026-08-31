phase0b_multitrack_pool <- function(rows) {
  dplyr::bind_rows(lapply(rows, function(row) {
    data.frame(
      candidate_id = as.character(row$candidate_id),
      candidate_layer = "phase0b_fixture",
      candidate_source = "phase0b_fixture",
      final_label = as.character(row$final_label),
      start_isi = as.integer(row$start_isi),
      end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$end_isi - row$start_isi + 1L),
      score = 10,
      priority = 1000,
      CV = as.numeric(row$CV %||% NA_real_),
      LV = as.numeric(row$LV %||% NA_real_),
      MM = as.numeric(row$MM %||% NA_real_),
      hf_spiking_large_fraction = as.numeric(
        row$hf_spiking_large_fraction %||% NA_real_
      ),
      stringsAsFactors = FALSE
    )
  }))
}

test_that("multi-track policy defaults are complete, reportable, and hash-visible", {
  params <- default_params()
  policy <- params$spiketrainpattern$multitrack_shadow
  contract <- stpd_parameter_contract()
  expected_paths <- paste0(
    "spiketrainpattern.multitrack_shadow.", names(policy)
  )

  expect_length(policy, 49L)
  expect_true(all(expected_paths %in% contract$path))
  expect_identical(policy$phase1a_policy_version, "phase1a_observation_only_v3")
  expect_identical(
    policy$phase1a_track_ontology_version,
    "stpd_multitrack_semantic_tracks_v2"
  )
  expect_identical(policy$possible_burst_review_target_track, "event")
  expect_identical(policy$possible_burst_review_target_label, "burst")
  expect_true(policy$possible_burst_review_promotion_required)
  expect_identical(
    policy$possible_burst_review_policy_status,
    paste0(
      "review_only__not_accepted_event__manual_confirmation_required__",
      "primary_event_metrics_ineligible__event_state_coexistence_non_destructive__",
      "hfs_dominance_ineligible"
    )
  )
  expect_identical(policy$phase1b_policy_version, "phase1b_compatibility_shadow_v1")
  expect_equal(policy$hfs_dominance_group_floor, 6L)
  expect_equal(policy$hfs_dominance_group_fraction, 0.055)
  expect_equal(policy$hfs_dominance_distributed_coverage_min, 0.25)
  expect_equal(policy$hfs_dominance_majority_coverage_min, 0.50)
  expect_equal(policy$variable_hfs_cv_min, 0.65)
  expect_equal(policy$variable_hfs_lv_min, 0.45)
  expect_equal(policy$variable_hfs_large_fraction_min, 0.08)
  expect_equal(policy$variable_hfs_mm_audit_min, 3.0)
  expect_equal(policy$packet_like_multi_group_coverage_min, 0.08)
  expect_equal(policy$packet_like_single_group_coverage_min, 0.18)
  expect_equal(policy$packet_neighbor_max_gap_isi, 3L)

  effective <- effective_params_for_detector(params)
  expect_identical(effective$spiketrainpattern$multitrack_shadow, policy)

  report <- getFromNamespace(
    "stpd_public_parameter_table", "SpikeTrainPatternDetector"
  )(effective)
  reported_policy <- report[report$section == "Multi-track shadow", , drop = FALSE]
  expect_true(all(c(names(policy), "multitrack_policy_hash") %in%
    reported_policy$parameter))
  policy_hash <- reported_policy$value[
    reported_policy$parameter == "multitrack_policy_hash"
  ][1]
  expect_match(policy_hash, "^[0-9a-f]{64}$")
  expect_identical(
    policy_hash,
    getFromNamespace("stpd_multitrack_policy_hash", "SpikeTrainPatternDetector")(
      effective
    )
  )

  changed <- params
  changed$spiketrainpattern$multitrack_shadow$hfs_dominance_group_floor <- 7L
  expect_false(identical(stpd_params_hash(changed), stpd_params_hash(params)))
  expect_false(identical(
    getFromNamespace("stpd_multitrack_policy_hash", "SpikeTrainPatternDetector")(changed),
    getFromNamespace("stpd_multitrack_policy_hash", "SpikeTrainPatternDetector")(params)
  ))
  expect_equal(
    effective_params_for_detector(changed)$spiketrainpattern$multitrack_shadow$hfs_dominance_group_floor,
    7L
  )

  identity_changed <- params
  identity_changed$spiketrainpattern$multitrack_shadow[[
    "possible_burst_review_policy_status"
  ]] <- "accepted_event"
  expect_false(identical(
    stpd_params_hash(identity_changed), stpd_params_hash(params)
  ))
  expect_false(identical(
    getFromNamespace("stpd_multitrack_policy_hash", "SpikeTrainPatternDetector")(
      identity_changed
    ),
    getFromNamespace("stpd_multitrack_policy_hash", "SpikeTrainPatternDetector")(
      params
    )
  ))
})

test_that("fixed policy identifiers cannot contradict their implemented behavior", {
  fixed <- c(
    phase1a_policy_version = "wrong_phase1a",
    phase1a_track_ontology_version = "wrong_ontology_version",
    phase1a_track_ontology = "event=everything",
    possible_burst_review_target_track = "state",
    possible_burst_review_target_label = "possible_burst",
    possible_burst_review_policy_status = "accepted_event",
    phase1b_policy_version = "wrong_phase1b",
    tonic_hft_burst_rule = "overlay_only",
    hfs_burst_rule = "split_hfs",
    packet_rule = "destructive",
    variable_hfs_mm_rule = "boolean_gate"
  )
  phase1a_fields <- c(
    "phase1a_policy_version", "phase1a_track_ontology_version",
    "phase1a_track_ontology", "possible_burst_review_target_track",
    "possible_burst_review_target_label", "possible_burst_review_policy_status"
  )
  for (field in names(fixed)) {
    changed <- default_params()
    changed$spiketrainpattern$multitrack_shadow[[field]] <- unname(fixed[[field]])
    if (field %in% phase1a_fields) {
      expect_error(
        getFromNamespace("stpd_multitrack_shadow_policy", "SpikeTrainPatternDetector")(changed),
        "Unsupported fixed multi-track policy value"
      )
    } else {
      expect_error(
        getFromNamespace(
          "stpd_multitrack_compatibility_policy", "SpikeTrainPatternDetector"
        )(changed),
        "Unsupported fixed multi-track policy value"
      )
    }
  }

  changed <- default_params()
  changed$spiketrainpattern$multitrack_shadow[[
    "possible_burst_review_promotion_required"
  ]] <- FALSE
  expect_error(
    getFromNamespace("stpd_multitrack_shadow_policy", "SpikeTrainPatternDetector")(changed),
    "Unsupported fixed multi-track policy value"
  )
})

test_that("Phase 1A ontology and Phase 1B Boolean thresholds use the public policy", {
  starts <- c(15L, 30L, 45L, 60L, 75L, 90L)
  pool <- phase0b_multitrack_pool(c(
    list(list(
      candidate_id = "hfs", final_label = "high_frequency_spiking",
      start_isi = 10L, end_isi = 109L
    )),
    lapply(seq_along(starts), function(i) {
      list(
        candidate_id = paste0("burst_", i), final_label = "burst",
        start_isi = starts[i], end_isi = starts[i] + 4L
      )
    })
  ))
  select <- getFromNamespace(
    "stpd_multitrack_shadow_select", "SpikeTrainPatternDetector"
  )
  resolve <- getFromNamespace(
    "stpd_multitrack_compatibility_shadow", "SpikeTrainPatternDetector"
  )

  params <- default_params()
  phase1a <- select(pool, patterns = c("burst", "high_frequency_spiking"), params = params)
  phase1b <- resolve(phase1a, params = params)
  expect_identical(phase1a$policy_version, "phase1a_observation_only_v3")
  expect_identical(
    phase1a$track_ontology_version, "stpd_multitrack_semantic_tracks_v2"
  )
  expect_true(all(phase1a$selected_candidates$semantic_track %in% c("event", "state")))
  expect_true(phase1b$hfs_dominance$distributed_dominance_flag)
  expect_identical(phase1a$multitrack_policy_hash, phase1b$multitrack_policy_hash)
  expect_identical(phase1b$source_params_hash, stpd_params_hash(params))
  expect_match(phase1b$multitrack_policy_hash, "^[0-9a-f]{64}$")

  changed <- params
  changed$spiketrainpattern$multitrack_shadow$hfs_dominance_group_floor <- 7L
  changed_phase1a <- select(
    pool, patterns = c("burst", "high_frequency_spiking"), params = changed
  )
  changed_phase1b <- resolve(changed_phase1a, params = changed)
  expect_false(changed_phase1b$hfs_dominance$distributed_dominance_flag)
  expect_false(changed_phase1b$hfs_dominance$threshold_dominance_flag)
  expect_false(identical(
    phase1b$multitrack_policy_hash, changed_phase1b$multitrack_policy_hash
  ))
})

test_that("partial re-gate inputs fail closed with a typed not-evaluated result", {
  dat <- data.frame(
    timestamp_sec = seq(0, by = 0.05, length.out = 30L),
    ISI_sec = c(NA_real_, rep(0.05, 29L)),
    stringsAsFactors = FALSE
  )
  regate <- getFromNamespace(
    "stpd_multitrack_compatibility_fragment_regate",
    "SpikeTrainPatternDetector"
  )
  result <- regate(
    "tonic", 5L, 20L, dat = dat, params = default_params(),
    variable_params = list(tonic_min_spikes = 3L), min_isi_sec = 0.001
  )

  expect_false(result$gate_evaluated)
  expect_true(is.na(result$provisional_gate_pass))
  expect_identical(
    result$provisional_gate_status,
    "not_evaluated__missing_required_variable_params"
  )
  expect_match(result$failed_checks, "missing_variable_params:")
})

test_that("Pause-created HFS children inherit accepted parent episode evidence", {
  pool <- phase0b_multitrack_pool(list(
    list(
      candidate_id = "hfs", final_label = "high_frequency_spiking",
      start_isi = 10L, end_isi = 109L
    ),
    list(candidate_id = "pause", final_label = "pause", start_isi = 60L, end_isi = 60L),
    list(
      candidate_id = "left_majority", final_label = "long_burst",
      start_isi = 15L, end_isi = 50L
    )
  ))
  dat <- data.frame(
    timestamp_sec = seq(0, by = 0.20, length.out = 120L),
    ISI_sec = c(NA_real_, rep(0.20, 119L)),
    pattern_manual = "",
    pattern_manual_negative = "",
    stringsAsFactors = FALSE
  )
  params <- effective_params_for_detector(default_params())
  variable_params <- getFromNamespace(
    "stpd_event_grammar_params_impl", "SpikeTrainPatternDetector"
  )(dat, params, min_isi_sec = 0.001, train = "phase0b_failed_child")
  phase1a <- getFromNamespace(
    "stpd_multitrack_shadow_select", "SpikeTrainPatternDetector"
  )(
    pool,
    patterns = c("long_burst", "high_frequency_spiking", "pause"),
    params = params
  )
  phase1b <- getFromNamespace(
    "stpd_multitrack_compatibility_shadow", "SpikeTrainPatternDetector"
  )(
    phase1a, dat = dat, params = params, variable_params = variable_params,
    min_isi_sec = 0.001
  )
  left <- phase1b$hfs_dominance[
    phase1b$hfs_dominance$start_isi == 10L &
      phase1b$hfs_dominance$end_isi == 59L,
    , drop = FALSE
  ]

  expect_true(left$threshold_dominance_flag)
  expect_true(left$fragment_gate_evaluated)
  expect_true(left$fragment_provisional_gate_pass)
  expect_true(left$dominance_evidence_applicable)
  expect_true(left$provisional_dominance_flag)
  expect_identical(
    left$provisional_state_status,
    "retain_hfs__burst_rich_diagnostic_only"
  )
  expect_true(left$state_selected_within_track_preserved)
  expect_false(left$destructive_action_applied)
})

test_that("run metadata and both shadow phases share one policy provenance chain", {
  ds <- getFromNamespace(
    "stpd_golden_test_dataset", "SpikeTrainPatternDetector"
  )("stable_high_frequency")
  out <- stpd_detect(
    ds, default_params(), selected_trains = "train_1",
    collect_diagnostics = FALSE, label_blind = TRUE
  )
  metadata <- out$results$run_metadata_public
  phase1a <- attr(out$trains$train_1, "multitrack_shadow")
  phase1b <- attr(out$trains$train_1, "multitrack_compatibility_shadow")

  expect_false(metadata$multitrack_shadow_authoritative)
  expect_identical(
    metadata$multitrack_phase1a_policy_version,
    phase1a$policy_version
  )
  expect_identical(
    metadata$multitrack_track_ontology_version,
    phase1a$track_ontology_version
  )
  expect_identical(
    metadata$multitrack_phase1b_policy_version,
    phase1b$policy_version
  )
  expect_identical(metadata$multitrack_policy_hash, phase1a$multitrack_policy_hash)
  expect_identical(metadata$multitrack_policy_hash, phase1b$multitrack_policy_hash)
  expect_identical(metadata$params_hash, phase1a$source_params_hash)
  expect_identical(metadata$params_hash, phase1b$source_params_hash)
  expect_identical(
    stpd_params_hash(out$params_effective), metadata$params_hash
  )
  parameter_report <- out$results$parameter_report
  policy_hash_row <- parameter_report[
    parameter_report$path ==
      "spiketrainpattern.multitrack_shadow.multitrack_policy_hash",
    , drop = FALSE
  ]
  expect_equal(nrow(policy_hash_row), 1L)
  expect_identical(
    policy_hash_row$current_value,
    metadata$multitrack_policy_hash
  )

  changed_params <- default_params()
  changed_params$spiketrainpattern$multitrack_shadow$hfs_dominance_group_floor <- 7L
  changed <- stpd_detect(
    ds, changed_params, selected_trains = "train_1",
    collect_diagnostics = FALSE, label_blind = TRUE
  )
  expect_identical(
    changed$trains$train_1$pattern_auto,
    out$trains$train_1$pattern_auto
  )
  expect_equal(
    changed$trains$train_1$auto_score,
    out$trains$train_1$auto_score
  )
  expect_false(identical(
    changed$results$run_metadata_public$params_hash,
    metadata$params_hash
  ))
  expect_false(identical(
    changed$results$run_metadata_public$multitrack_policy_hash,
    metadata$multitrack_policy_hash
  ))
})
