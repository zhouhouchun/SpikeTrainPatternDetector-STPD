phase1a_shadow_candidate_pool <- function() {
  data.frame(
    candidate_id = c("hfs_1", "burst_1", "burst_2", "pause_1", "review_1", "reject_1", "profile_1"),
    candidate_layer = c("state", "event", "event", "gap", "review", "diagnostic", "profile"),
    final_label = c(
      "high_frequency_spiking", "burst", "burst", "pause",
      "possible_burst", "reject", "profile"
    ),
    start_isi = c(10L, 30L, 70L, 120L, 32L, 5L, NA_integer_),
    end_isi = c(110L, 34L, 74L, 120L, 36L, 7L, NA_integer_),
    n_isi = c(101L, 5L, 5L, 1L, 5L, 3L, NA_integer_),
    score = c(45, 9, 8, 5, 6, 0, NA_real_),
    priority = c(1040, 1250, 1250, 320, 520, 0, 0),
    stringsAsFactors = FALSE
  )
}

test_that("Phase 1A shadow selects an HFS state and its embedded bursts", {
  select_shadow <- getFromNamespace("stpd_multitrack_shadow_select", "SpikeTrainPatternDetector")
  pool <- phase1a_shadow_candidate_pool()
  before <- pool

  shadow <- select_shadow(
    pool,
    patterns = c("burst", "long_burst", "high_frequency_spiking", "pause")
  )

  expect_identical(pool, before)
  expect_false(isTRUE(shadow$authoritative))
  expect_identical(shadow$policy_version, "phase1a_observation_only_v3")
  expect_identical(
    shadow$track_ontology_version, "stpd_multitrack_semantic_tracks_v2"
  )
  expect_identical(shadow$candidate_source, "pre_hf_spiking_protection")
  expect_identical(shadow$cross_track_resolution, "none")
  expect_false("pre_protection_candidates" %in% names(shadow))
  expect_equal(sort(shadow$candidates$source_candidate_index), seq_len(nrow(pool)))

  selected <- shadow$selected_candidates
  expect_true(any(selected$semantic_track == "state" & selected$final_label == "high_frequency_spiking"))
  expect_true(any(selected$semantic_track == "event" & selected$final_label == "burst"))
  expect_true(all(c("event", "state", "gap", "review", "diagnostic", "profile") %in%
    shadow$track_summary$semantic_track))
  expect_true("n_selected_within_track" %in% names(shadow$track_summary))
  expect_equal(
    as.character(shadow$candidates$semantic_track[match(pool$candidate_id, shadow$candidates$candidate_id)]),
    c("state", "event", "event", "gap", "review", "diagnostic", "profile")
  )

  review_row <- shadow$candidates[
    shadow$candidates$candidate_id == "review_1", , drop = FALSE
  ]
  selected_review_row <- shadow$selected_candidates[
    shadow$selected_candidates$candidate_id == "review_1", , drop = FALSE
  ]
  expect_equal(nrow(selected_review_row), 1L)
  expect_identical(review_row$semantic_track, "review")
  expect_true(review_row$selected_within_track)
  expect_identical(review_row$review_target_track, "event")
  expect_identical(review_row$review_target_label, "burst")
  expect_true(review_row$review_promotion_required)
  expect_identical(
    review_row$review_policy_status,
    paste0(
      "review_only__not_accepted_event__manual_confirmation_required__",
      "primary_event_metrics_ineligible__event_state_coexistence_non_destructive__",
      "hfs_dominance_ineligible"
    )
  )
  expect_identical(
    as.list(selected_review_row[1, c(
      "review_target_track", "review_target_label",
      "review_promotion_required", "review_policy_status"
    ), drop = FALSE]),
    as.list(review_row[1, c(
      "review_target_track", "review_target_label",
      "review_promotion_required", "review_policy_status"
    ), drop = FALSE])
  )
  non_possible <- shadow$candidates$final_label != "possible_burst"
  expect_true(all(shadow$candidates$review_target_track[non_possible] == ""))
  expect_true(all(shadow$candidates$review_target_label[non_possible] == ""))
  expect_false(any(shadow$candidates$review_promotion_required[non_possible]))
  expect_true(all(shadow$candidates$review_policy_status[non_possible] == ""))
  expect_false(any(
    shadow$candidates$semantic_track == "event" &
      shadow$candidates$final_label == "possible_burst"
  ))
})

test_that("all normalized possible-burst variants retain Review targets even when not selected", {
  select_shadow <- getFromNamespace(
    "stpd_multitrack_shadow_select", "SpikeTrainPatternDetector"
  )
  pool <- data.frame(
    candidate_id = c("possible_space", "possible_hyphen"),
    candidate_layer = "review_fixture",
    final_label = c("Possible Burst", "possible-burst"),
    start_isi = c(20L, 20L),
    end_isi = c(24L, 24L),
    n_isi = c(5L, 5L),
    score = c(9, 8),
    priority = c(520, 520),
    stringsAsFactors = FALSE
  )

  shadow <- select_shadow(pool, patterns = "possible_burst")
  rows <- shadow$candidates[
    match(pool$candidate_id, shadow$candidates$candidate_id), , drop = FALSE
  ]

  expect_true(all(rows$semantic_track == "review"))
  expect_true(all(rows$review_target_track == "event"))
  expect_true(all(rows$review_target_label == "burst"))
  expect_true(all(rows$review_promotion_required))
  expect_true(all(nzchar(rows$review_policy_status)))
  expect_equal(sum(rows$selected_within_track), 1L)
  expect_equal(sum(!rows$selected_within_track), 1L)
  expect_true(any(grepl(
    "selected_within_review_weighted_interval_grammar",
    rows$track_selection_status
  )))
  expect_true(any(grepl("within_review_track", rows$track_selection_status)))
})

test_that("Phase 1A within-track selection is deterministic under candidate permutation", {
  select_shadow <- getFromNamespace("stpd_multitrack_shadow_select", "SpikeTrainPatternDetector")
  pool <- phase1a_shadow_candidate_pool()
  tied <- pool[pool$candidate_id == "burst_1", , drop = FALSE]
  tied$candidate_id <- "burst_0"
  pool <- rbind(pool, tied)

  patterns <- c("burst", "long_burst", "high_frequency_spiking", "pause")
  reference <- select_shadow(pool, patterns = patterns)
  permuted <- select_shadow(pool[c(8L, 3L, 6L, 1L, 7L, 4L, 2L, 5L), , drop = FALSE], patterns = patterns)

  stable_fields <- c(
    "candidate_id", "semantic_track", "selected_within_track",
    "track_selection_status", "review_target_track", "review_target_label",
    "review_promotion_required", "review_policy_status"
  )
  reference_stable <- reference$candidates[, stable_fields, drop = FALSE]
  permuted_stable <- permuted$candidates[, stable_fields, drop = FALSE]
  expect_identical(reference_stable, permuted_stable)
  expect_identical(
    setdiff(reference$selected_candidates$candidate_id, ""),
    setdiff(permuted$selected_candidates$candidate_id, "")
  )
  expect_true("burst_0" %in% reference$selected_candidates$candidate_id)
  expect_false("burst_1" %in% reference$selected_candidates$candidate_id)
})

test_that("Phase 1A integration leaves legacy output and candidate audit schemas unchanged", {
  detect <- getFromNamespace("stpd_detect_train_hf_protected_impl", "SpikeTrainPatternDetector")
  protect <- getFromNamespace("stpd_event_grammar_protect_hf_spiking_states", "SpikeTrainPatternDetector")
  select_legacy <- getFromNamespace("stpd_event_core_weighted_select", "SpikeTrainPatternDetector")

  isi <- c(rep(0.45, 6L), rep(0.04, 6L), rep(0.45, 8L), 1.20, rep(0.45, 7L))
  timestamp <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    stringsAsFactors = FALSE
  )
  params <- default_params()
  out <- detect(dat, params, min_isi_sec = 0.001, train = "phase1a_legacy_guard", lock_manual = FALSE)

  shadow <- attr(out, "multitrack_shadow")
  legacy_audit <- attr(out, "candidate_diagnostic_audit")
  expect_s3_class(shadow, "stpd_multitrack_shadow")
  expect_false(any(c(
    "semantic_track", "selected_within_track", "track_selection_status",
    "review_target_track", "review_target_label",
    "review_promotion_required", "review_policy_status"
  ) %in% names(legacy_audit)))
  expect_false(any(c(
    "pattern_auto_event", "pattern_auto_state", "pattern_auto_gap", "pattern_auto_review"
  ) %in% names(out)))

  expected_audit <- shadow$candidates[
    order(shadow$candidates$source_candidate_index),
    ,
    drop = FALSE
  ]
  shadow_columns <- c(
    "source_candidate_index", "semantic_track", "selected_within_track",
    "track_selection_status", "review_target_track", "review_target_label",
    "review_promotion_required", "review_policy_status"
  )
  expected_audit <- expected_audit[, setdiff(names(expected_audit), shadow_columns), drop = FALSE]
  rownames(expected_audit) <- NULL
  expected_audit <- protect(expected_audit)
  expected_audit <- select_legacy(
    expected_audit,
    locked = NULL,
    patterns = params$detector$patterns_to_run
  )
  expect_identical(legacy_audit, expected_audit)

  # The established single-label projection remains active and internally
  # consistent while the new result is confined to a non-authoritative attr.
  legacy_labels <- as.character(out$pattern_auto)
  expect_gte(sum(legacy_labels == "tonic", na.rm = TRUE), 14L)
  expect_gte(sum(legacy_labels == "burst", na.rm = TRUE), 6L)
  expect_equal(sum(legacy_labels == "pause", na.rm = TRUE), 1L)
})

test_that("Phase 1A preserves the exact Burst evidence that legacy HFS protection rejects", {
  detect <- getFromNamespace(
    "stpd_detect_train_hf_protected_impl", "SpikeTrainPatternDetector"
  )

  isi <- rep(0.006, 300L)
  isi[149:151] <- rep(0.0018, 3L)
  timestamp <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    stringsAsFactors = FALSE
  )

  out <- detect(
    dat, default_params(), min_isi_sec = 0.0009,
    train = "phase1a_hfs_suppression_guard", lock_manual = FALSE
  )
  shadow <- attr(out, "multitrack_shadow")
  legacy <- attr(out, "candidate_diagnostic_audit")

  legacy_suppressed <- legacy[
    legacy$candidate_layer == "structure_first_burst_screen" &
      legacy$final_label == "reject" &
      legacy$suppressed_original_label %in% c("burst", "long_burst"),
    ,
    drop = FALSE
  ]
  expect_gt(nrow(legacy_suppressed), 0L)

  shadow_match <- shadow$candidates[
    shadow$candidates$candidate_id %in% legacy_suppressed$candidate_id &
      shadow$candidates$candidate_layer == "structure_first_burst_screen",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(shadow_match), nrow(legacy_suppressed))
  expect_true(all(shadow_match$final_label %in% c("burst", "long_burst")))
  expect_true(all(shadow_match$semantic_track == "event"))
  expect_true(any(shadow_match$selected_within_track))
  expect_true(any(
    shadow$candidates$semantic_track == "state" &
      shadow$candidates$final_label == "high_frequency_spiking" &
      shadow$candidates$selected_within_track
  ))

  rematch <- match(
    shadow_match$candidate_id,
    legacy_suppressed$candidate_id
  )
  expect_true(all(!is.na(rematch)))
  expect_identical(
    as.character(legacy_suppressed$suppressed_original_label[rematch]),
    as.character(shadow_match$final_label)
  )
  expect_true(all(legacy_suppressed$final_label[rematch] == "reject"))
})

test_that("exported label-blind detection retains shadow attrs and restores manual review fields", {
  isi <- rep(0.010, 60L)
  timestamp <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    stringsAsFactors = FALSE
  )
  dat$pattern_manual[25:28] <- "burst"
  dat$pattern_manual_negative[40:41] <- "not_burst"
  expected_manual <- dat[, c("pattern_manual", "pattern_manual_negative"), drop = FALSE]
  ds <- SpikeTrainPatternDetector:::make_dataset(
    "phase1a_exported_contract", "synthetic", list(train_1 = dat), unit_in = "s"
  )

  out <- stpd_detect(
    ds, default_params(), lock_manual = FALSE,
    collect_diagnostics = TRUE, label_blind = TRUE
  )
  train <- out$trains$train_1

  expect_s3_class(attr(train, "multitrack_shadow"), "stpd_multitrack_shadow")
  expect_s3_class(
    attr(train, "multitrack_compatibility_shadow"),
    "stpd_multitrack_compatibility_shadow"
  )
  expect_identical(
    train[, c("pattern_manual", "pattern_manual_negative"), drop = FALSE],
    expected_manual
  )
  expect_identical(
    out$results$multitrack_auto$metadata$materialization_status,
    "materialized"
  )
  expect_true(out$results$multitrack_auto$metadata$label_blind_execution)
  expect_false(
    out$results$multitrack_auto$metadata$detector_performance_eligible
  )
  expect_false("multitrack_preview" %in% names(out$results))
  expect_false(any(c(
    "pattern_auto_event", "pattern_auto_state", "pattern_auto_gap",
    "pattern_auto_review"
  ) %in% names(train)))
})
