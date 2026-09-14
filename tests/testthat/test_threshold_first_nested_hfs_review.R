nested_hfs_review_fixture <- function(with_acceleration = TRUE) {
  isi <- rep(0.006, 40L)
  if (isTRUE(with_acceleration)) isi[19:22] <- c(0.0018, 0.0018, 0.0020, 0.0018)
  timestamp <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi), pattern_manual = "",
    pattern_manual_negative = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.0009, train = "train_1"
  )
  parent <- data.frame(
    candidate_id = "frozen_hfs_parent", start_isi = 2L,
    end_isi = nrow(dat), final_label = "high_frequency_spiking",
    stringsAsFactors = FALSE
  )
  raw_all <- stpd_event_core_detect_nested_hfs_bursts(
    dat, parent, settings = stpd_nested_hfs_detector_settings(params, vp),
    min_isi_sec = 0.0009
  )
  standardized_all <- stpd_nested_hfs_standardize_candidates(
    dat, raw_all, params, vp, min_isi_sec = 0.0009, train = "train_1"
  )
  raw <- raw_all[!raw_all$canonical_eligible, , drop = FALSE]
  standardized <- if (nrow(standardized_all)) {
    standardized_all[!standardized_all$canonical_eligible, , drop = FALSE]
  } else data.frame()
  shadow <- stpd_multitrack_shadow_select(
    standardized, patterns = params$detector$patterns_to_run, params = params
  )
  list(dat = dat, params = params, vp = vp, parent = parent, raw = raw,
       standardized = standardized, shadow = shadow,
       automatic = if (nrow(standardized_all)) {
         standardized_all[standardized_all$canonical_eligible, , drop = FALSE]
       } else data.frame())
}

nested_hfs_review_observe <- function(fixture, run_id = "nested_hfs_review") {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("7", 64L), collapse = ""),
    "nested_hfs_review_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_begin_nested_hfs_review(
    shard, "train_1", fixture$dat, fixture$params, fixture$vp, 0.0009,
    c("burst", "high_frequency_spiking"), fixture$parent, data.frame()
  )
  signature <- stpd_candidate_lineage_nested_hfs_review_hash(
    fixture$parent, "fixture-parent-signature"
  )
  stpd_candidate_lineage_capture_nested_hfs_review(
    shard, "train_1", fixture$raw, fixture$standardized, fixture$shadow,
    signature, signature
  )
  list(
    collector = collector, shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

test_that("accepted nested HFS Bursts do not leak into the Review side channel", {
  fixture <- nested_hfs_review_fixture()
  expect_identical(nrow(fixture$automatic), 1L)
  expect_identical(fixture$automatic$final_label, "burst")
  expect_identical(nrow(fixture$raw), 0L)
  run <- nested_hfs_review_observe(fixture)
  observations <- run$snapshot$observations
  candidates <- observations$nested_hfs_review_candidates_v1
  contrast <- observations$nested_hfs_review_local_contrast_v1
  invariance <- observations$nested_hfs_review_parent_invariance_v1
  receipt <- observations$nested_hfs_review_receipt_v1
  expect_true(all(
    names(stpd_candidate_lineage_nested_hfs_review_hooks()) %in%
      names(observations)
  ))
  expect_identical(nrow(candidates), 0L)
  expect_identical(nrow(contrast), 0L)
  expect_true(invariance$parent_signature_unchanged)
  expect_false(invariance$review_can_veto_state)
  expect_false(invariance$review_can_reshape_state)
  expect_false(invariance$review_can_create_state)
  expect_identical(receipt$review_identity_status,
                   "validated_review_not_event")
  expect_identical(receipt$local_contrast_status,
                   "validated_local_hfs_background")
  expect_identical(receipt$parent_invariance_status,
                   "validated_no_state_effect")
  expect_identical(receipt$promotion_authority_status,
                   "separate_review_transition_required")
  expect_false(receipt$publication_authority)
})

test_that("homogeneous HFS closes with no invented Review event", {
  fixture <- nested_hfs_review_fixture(with_acceleration = FALSE)
  expect_identical(nrow(fixture$raw), 0L)
  run <- nested_hfs_review_observe(fixture, "nested_hfs_empty")
  receipt <- run$snapshot$observations$nested_hfs_review_receipt_v1
  expect_identical(receipt$raw_candidate_n, 0L)
  expect_identical(receipt$standardized_candidate_n, 0L)
  expect_identical(receipt$review_track_candidate_n, 0L)
  expect_identical(receipt$review_identity_status,
                   "validated_review_not_event")
})

test_that("empty nested HFS Review payloads fail closed on mutation", {
  run <- nested_hfs_review_observe(
    nested_hfs_review_fixture(), "nested_hfs_tamper"
  )
  before <- run$snapshot
  mutations <- list(
    nested_hfs_review_entry_v1 = function(x) {
      x$automatic_event_promotion_allowed <- TRUE; x
    },
    nested_hfs_review_parent_invariance_v1 = function(x) {
      x$review_can_veto_state <- TRUE; x
    },
    nested_hfs_review_receipt_v1 = function(x) {
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

test_that("collector-off path never enters nested HFS Review observer", {
  fixture <- nested_hfs_review_fixture()
  params <- fixture$params
  params$detector$patterns_to_run <- c("burst", "high_frequency_spiking")
  baseline <- stpd_detect_train_hf_protected_impl(
    fixture$dat, params, 0.0009, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_nested_hfs_review = function(...) {
      stop("nested-HFS begin entered with collector off")
    },
    stpd_candidate_lineage_capture_nested_hfs_review = function(...) {
      stop("nested-HFS capture entered with collector off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    fixture$dat, params, 0.0009, "train_1", lock_manual = FALSE
  )
  expect_identical(observed, baseline)
})
