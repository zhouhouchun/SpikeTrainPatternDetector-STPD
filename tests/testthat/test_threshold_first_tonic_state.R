tonic_state_observer_dat <- function(isi = rep(0.050, 20L)) {
  timestamp <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi), pattern_manual = "",
    pattern_manual_negative = "", stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

tonic_state_observer_params <- function() {
  params <- default_params()
  params$detector$patterns_to_run <- c("tonic", "pause")
  params
}

tonic_state_observer_active <- function(dat = tonic_state_observer_dat(),
                                        run_id = "tonic_state_observer") {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("e", 64L), collapse = ""),
    "tonic_state_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  out <- stpd_detect_train_hf_protected_impl(
    dat, tonic_state_observer_params(), 0.001, "train_1",
    lock_manual = FALSE, candidate_lineage_collector = shard
  )
  list(
    out = out, collector = collector, shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

test_that("Tonic-State observer separates frequency and regularity evidence", {
  run <- tonic_state_observer_active()
  baseline <- stpd_detect_train_hf_protected_impl(
    tonic_state_observer_dat(), tonic_state_observer_params(), 0.001,
    "train_1", lock_manual = FALSE
  )
  expect_identical(run$out, baseline)
  observations <- run$snapshot$observations
  expect_true(all(
    names(stpd_candidate_lineage_tonic_state_hooks()) %in% names(observations)
  ))
  entry <- observations$tonic_state_entry_v1
  evidence <- observations$tonic_state_frequency_regularity_v1
  receipt <- observations$tonic_state_receipt_v1
  expect_true(entry$tonic_requested)
  expect_false(entry$hft_requested)
  expect_identical(entry$frequency_axis, "candidate_local_isi_distribution")
  expect_identical(entry$regularity_axes, "CV|LV|MM")
  expect_false(entry$burst_to_state_veto_allowed)
  expect_identical(receipt$tonic_candidate_n, 1L)
  expect_equal(evidence$median_isi_sec, 0.050, tolerance = 1e-12)
  expect_equal(evidence$median_frequency_hz, 20, tolerance = 1e-12)
  expect_equal(evidence$CV, 0, tolerance = 1e-12)
  expect_equal(evidence$LV, 0, tolerance = 1e-12)
  expect_equal(evidence$MM, 1, tolerance = 1e-12)
  expect_identical(
    receipt$burst_overlay_status,
    "validated_non_destructive_event_overlay"
  )
  expect_false(receipt$publication_authority)
  expect_identical(receipt$scientific_result_influence, "none_observer_only")
})

test_that("sparse Tonic evidence produces an explicit abstention", {
  run <- tonic_state_observer_active(
    tonic_state_observer_dat(rep(0.050, 3L)), "tonic_state_sparse"
  )
  receipt <- run$snapshot$observations$tonic_state_receipt_v1
  expect_true(receipt$sparse_case_applicable)
  expect_identical(
    receipt$sparse_evidence_abstention_status, "validated_abstained"
  )
  expect_identical(receipt$tonic_candidate_n, 0L)
})

test_that("Pause-created Tonic children run the complete generator again", {
  dat <- tonic_state_observer_dat()
  params <- effective_params_for_detector(tonic_state_observer_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )
  tonic <- stpd_event_core_detect_tonic(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )
  expect_gt(nrow(tonic), 0L)
  tonic <- tonic[1L, , drop = FALSE]
  boundaries <- data.frame(
    start_isi = 11L, end_isi = 11L, boundary_kind = "canonical_pause",
    stringsAsFactors = FALSE
  )
  burst <- data.frame(
    candidate_id = "embedded_burst", start_isi = 6L, end_isi = 8L,
    final_label = "burst", stringsAsFactors = FALSE
  )
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, "tonic_fragment", paste(rep("f", 64L), collapse = ""),
    "tonic_fragment_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_begin_tonic_state(
    shard, "train_1", dat, params, vp, 0.001, c("tonic", "pause"),
    boundaries
  )
  stpd_candidate_lineage_capture_tonic_state(
    shard, "train_1", tonic, data.frame(), burst
  )
  observations <- stpd_candidate_lineage_collector_observation_snapshot(
    shard)$observations
  candidates <- observations$tonic_state_candidates_v1
  fragments <- observations$tonic_state_fragment_redetection_v1
  receipt <- observations$tonic_state_receipt_v1
  expect_identical(candidates$burst_overlap_n, 1L)
  expect_false(candidates$burst_veto_applied)
  expect_equal(nrow(fragments), 2L)
  expect_true(all(fragments$created_by_canonical_pause))
  expect_true(all(fragments$redetection_performed))
  expect_true(all(fragments$fragment_accepted))
  expect_true(all(fragments$regenerated_candidate_n > 0L))
  expect_identical(
    receipt$fragment_redetection_status,
    "validated_full_generator_redetection"
  )
  expect_identical(receipt$burst_overlapping_candidate_n, 1L)
})

test_that("Tonic-State hook payloads fail closed on mutation", {
  run <- tonic_state_observer_active(run_id = "tonic_state_tamper")
  before <- run$snapshot
  mutations <- list(
    tonic_state_entry_v1 = function(x) {
      x$burst_to_state_veto_allowed <- TRUE; x
    },
    tonic_state_candidates_v1 = function(x) {
      x$burst_veto_applied[[1L]] <- TRUE; x
    },
    tonic_state_frequency_regularity_v1 = function(x) {
      x$CV[[1L]] <- 99; x
    },
    tonic_state_fragment_redetection_v1 = function(x) {
      x$redetection_performed[[1L]] <- FALSE; x
    },
    tonic_state_receipt_v1 = function(x) {
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

test_that("collector-off path never enters the Tonic-State observer", {
  dat <- tonic_state_observer_dat()
  params <- tonic_state_observer_params()
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_tonic_state = function(...) {
      stop("Tonic-State begin entered with collector off")
    },
    stpd_candidate_lineage_capture_tonic_state = function(...) {
      stop("Tonic-State capture entered with collector off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  expect_identical(observed, baseline)
})
