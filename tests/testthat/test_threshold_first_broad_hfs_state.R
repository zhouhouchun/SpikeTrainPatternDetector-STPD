broad_hfs_observer_dat <- function(gap_sec = 0.060) {
  isi <- c(rep(0.010, 11L), gap_sec, rep(0.010, 11L))
  timestamp <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, diff(timestamp)), pattern_manual = "",
    pattern_manual_negative = "", stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

broad_hfs_observer_params <- function() {
  params <- default_params()
  params$detector$patterns_to_run <- c("high_frequency_spiking", "pause")
  # This fixture tests the observer's support-role accounting, not automatic
  # HFS identifiability.  Use an explicit threshold contract so a homogeneous
  # HFS parent is scientifically authorized without a slower local flank.
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.040,
      bridge_upper_sec = 0.075,
      seed_lower_sec_source = "user",
      seed_upper_sec_source = "user",
      bridge_upper_sec_source = "user"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0, max_sec = 0.040
  )
  params$highfreq$spiking_min_spikes <- 20L
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.075
  params
}

broad_hfs_observer_active <- function(run_id = "broad_hfs_observer") {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("c", 64L), collapse = ""),
    "broad_hfs_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  out <- stpd_detect_train_hf_protected_impl(
    broad_hfs_observer_dat(), broad_hfs_observer_params(), 0.001,
    "train_1", lock_manual = FALSE, candidate_lineage_collector = shard
  )
  list(
    out = out, collector = collector, shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

test_that("Broad-HFS observer separates direct support and one connector", {
  run <- broad_hfs_observer_active()
  baseline <- stpd_detect_train_hf_protected_impl(
    broad_hfs_observer_dat(), broad_hfs_observer_params(), 0.001,
    "train_1", lock_manual = FALSE
  )
  expect_identical(run$out, baseline)
  observations <- run$snapshot$observations
  expect_true(all(
    names(stpd_candidate_lineage_broad_hfs_hooks()) %in% names(observations)
  ))
  receipt <- observations$hfs_state_receipt_v1
  expect_identical(receipt$candidate_n, 1L)
  expect_identical(receipt$selected_parent_n, 1L)
  expect_identical(receipt$direct_support_isi_n, 22L)
  expect_identical(receipt$tolerated_connector_isi_n, 1L)
  expect_identical(receipt$canonical_pause_intrusion_isi_n, 0L)
  expect_identical(receipt$connector_identity_status, "validated_exact")
  expect_identical(
    receipt$support_envelope_closure_status, "validated_separate"
  )
  expect_identical(receipt$parent_signature_status, "validated_unchanged")
  expect_identical(receipt$burst_overlay_policy, "orthogonal_event_non_veto")
  expect_identical(receipt$pause_direct_support_policy,
                   "pause_excluded_from_direct_support_preserved_in_episode_envelope")
  expect_false(receipt$publication_authority)
  expect_identical(receipt$scientific_result_influence, "none_observer_only")

  roles <- observations$hfs_state_support_roles_v1
  connector <- roles$connector_kind == "supra_direct_support"
  expect_identical(sum(connector), 1L)
  expect_equal(roles$isi_sec[connector], 0.060, tolerance = 1e-12)
  expect_false(roles$active_state_support[connector])
  expect_true(roles$episode_membership[connector])
  expect_true(all(roles$active_state_support[!connector]))
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    run$shard, "hfs_state_receipt_v1", receipt
  ))
})

test_that("Broad-HFS observer preserves a post-ownership Pause in the parent envelope", {
  dat <- broad_hfs_observer_dat()
  params <- broad_hfs_observer_params()
  params$event_grammar$effective_bands$pause <- list(
    seed_lower_sec = 0.050,
    seed_upper_sec = 0.050,
    bridge_upper_sec = 0.050,
    seed_lower_sec_source = "user",
    seed_upper_sec_source = "user",
    bridge_upper_sec_source = "user"
  )
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, "broad_hfs_pause_envelope",
    paste(rep("e", 64L), collapse = ""),
    "broad_hfs_pause_envelope_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(
    collector, "train_1"
  )
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = shard
  )
  observations <- stpd_candidate_lineage_collector_observation_snapshot(
    shard
  )$observations
  receipt <- observations$hfs_state_receipt_v1
  expect_identical(receipt$candidate_n, 1L)
  expect_identical(receipt$direct_support_isi_n, 22L)
  expect_identical(receipt$tolerated_connector_isi_n, 0L)
  expect_identical(receipt$canonical_pause_intrusion_isi_n, 1L)
  expect_identical(
    receipt$support_envelope_closure_status, "validated_separate"
  )
  expect_identical(
    receipt$pause_direct_support_policy,
    "pause_excluded_from_direct_support_preserved_in_episode_envelope"
  )
  roles <- observations$hfs_state_support_roles_v1
  pause <- roles$canonical_pause_boundary
  expect_identical(sum(pause), 1L)
  expect_true(roles$episode_membership[pause])
  expect_false(roles$active_state_support[pause])
  expect_identical(
    roles$support_role[pause], "excluded_pause_direct_support"
  )
})

test_that("Broad-HFS subtype requests use the same parent generator", {
  dat <- broad_hfs_observer_dat()
  params <- broad_hfs_observer_params()
  params$detector$patterns_to_run <- "high_frequency_tonic"
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, "broad_hfs_hft", paste(rep("d", 64L), collapse = ""),
    "broad_hfs_hft_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = shard
  )
  observations <- stpd_candidate_lineage_collector_observation_snapshot(
    shard)$observations
  expect_true(observations$hfs_state_entry_v1$broad_hfs_requested)
  expect_identical(
    observations$hfs_state_entry_v1$unique_parent_generator,
    "stpd_event_core_detect_hf_spiking"
  )
  expect_true(all(
    observations$hfs_state_candidates_v1$parent_state_class ==
      "high_frequency_spiking"
  ))
  expect_true(all(
    observations$hfs_state_candidates_v1$subtype_status ==
      "deferred_same_frozen_direct_support"
  ))
})

test_that("Broad-HFS hook payloads fail closed on mutation", {
  run <- broad_hfs_observer_active("broad_hfs_tamper")
  before <- run$snapshot
  mutations <- list(
    hfs_state_entry_v1 = function(x) {
      x$burst_to_state_veto_allowed <- TRUE; x
    },
    hfs_state_candidates_v1 = function(x) {
      x$direct_support_isi_n[[1L]] <- 0L; x
    },
    hfs_state_support_roles_v1 = function(x) {
      x$active_state_support[[1L]] <- !x$active_state_support[[1L]]; x
    },
    hfs_state_parent_selection_v1 = function(x) {
      x$burst_veto_applied[[1L]] <- TRUE; x
    },
    hfs_state_receipt_v1 = function(x) {
      x$publication_authority <- TRUE; x
    }
  )
  for (hook in names(mutations)) {
    expect_error(
      stpd_candidate_lineage_validate_hook_payload(
        run$shard, hook,
        mutations[[hook]](before$observations[[hook]])
      ),
      class = "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
    )
  }
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("collector-off path never enters the Broad-HFS observer", {
  dat <- broad_hfs_observer_dat()
  params <- broad_hfs_observer_params()
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_broad_hfs_state = function(...) {
      stop("Broad-HFS begin entered with collector off")
    },
    stpd_candidate_lineage_capture_broad_hfs_state = function(...) {
      stop("Broad-HFS capture entered with collector off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  expect_identical(observed, baseline)
})
