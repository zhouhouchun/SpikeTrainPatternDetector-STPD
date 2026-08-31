phase1b_candidate_pool <- function(rows) {
  defaults <- list(
    candidate_layer = "phase1b_fixture",
    score = 10,
    priority = 1000,
    CV = NA_real_,
    LV = NA_real_,
    MM = NA_real_,
    hf_spiking_large_fraction = NA_real_
  )
  out <- lapply(rows, function(row) {
    row <- utils::modifyList(defaults, row)
    data.frame(
      candidate_id = as.character(row$candidate_id),
      candidate_layer = as.character(row$candidate_layer),
      final_label = as.character(row$final_label),
      start_isi = as.integer(row$start_isi),
      end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$end_isi - row$start_isi + 1L),
      score = as.numeric(row$score),
      priority = as.numeric(row$priority),
      CV = as.numeric(row$CV),
      LV = as.numeric(row$LV),
      MM = as.numeric(row$MM),
      hf_spiking_large_fraction = as.numeric(row$hf_spiking_large_fraction),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(out)
}

phase1b_resolve <- function(pool, dat = NULL, params = NULL, variable_params = NULL) {
  select_shadow <- getFromNamespace(
    "stpd_multitrack_shadow_select", "SpikeTrainPatternDetector"
  )
  resolve <- getFromNamespace(
    "stpd_multitrack_compatibility_shadow", "SpikeTrainPatternDetector"
  )
  phase1a <- select_shadow(
    pool,
    patterns = c(
      "burst", "long_burst", "possible_burst", "high_frequency_spiking",
      "high_frequency_tonic", "tonic", "pause"
    )
  )
  list(
    phase1a = phase1a,
    phase1b = resolve(
      phase1a, dat = dat, params = params,
      variable_params = variable_params, min_isi_sec = 0.001
    )
  )
}

test_that("Phase 1B is idempotent, order invariant, and keeps its ledger lightweight", {
  pool <- phase1b_candidate_pool(list(
    list(candidate_id = "hfs", final_label = "high_frequency_spiking", start_isi = 10, end_isi = 109),
    list(candidate_id = "b1", final_label = "burst", start_isi = 20, end_isi = 23),
    list(candidate_id = "b2", final_label = "burst", start_isi = 50, end_isi = 53),
    list(candidate_id = "b3", final_label = "long_burst", start_isi = 80, end_isi = 83)
  ))
  reference <- phase1b_resolve(pool)
  permuted <- phase1b_resolve(pool[c(4L, 2L, 1L, 3L), , drop = FALSE])
  resolve <- getFromNamespace(
    "stpd_multitrack_compatibility_shadow", "SpikeTrainPatternDetector"
  )

  expect_identical(reference$phase1b, resolve(reference$phase1b))
  expect_identical(reference$phase1b, permuted$phase1b)
  expect_s3_class(reference$phase1b, "stpd_multitrack_compatibility_shadow")
  expect_false(reference$phase1b$authoritative)
  expect_false(reference$phase1b$legacy_projection_changed)
  expect_false(reference$phase1b$legacy_candidate_audit_changed)
  expect_false(any(c("candidates", "selected_candidates") %in% names(reference$phase1b)))
})

test_that("HFS and selected canonical Bursts coexist as non-destructive overlays", {
  pool <- phase1b_candidate_pool(list(
    list(candidate_id = "hfs", final_label = "high_frequency_spiking", start_isi = 10, end_isi = 109),
    list(candidate_id = "b1", final_label = "burst", start_isi = 20, end_isi = 23),
    list(candidate_id = "b2", final_label = "burst", start_isi = 50, end_isi = 53),
    list(candidate_id = "b3", final_label = "burst", start_isi = 80, end_isi = 83)
  ))
  out <- phase1b_resolve(pool)
  selected <- out$phase1a$selected_candidates
  dominance <- out$phase1b$hfs_dominance

  expect_true(any(selected$candidate_id == "hfs" & selected$semantic_track == "state"))
  expect_equal(sum(selected$semantic_track == "event"), 3L)
  expect_equal(out$phase1b$state_parents$split_kind, "none")
  expect_equal(nrow(out$phase1b$state_fragments), 1L)
  expect_equal(
    sum(out$phase1b$overlays$compatibility_rule == "hfs_burst_overlay"),
    3L
  )
  expect_equal(dominance$selected_event_group_count, 3L)
  expect_false(dominance$provisional_dominance_flag)
  expect_true(dominance$state_selected_within_track_preserved)
  expect_false(dominance$destructive_action_applied)
})

test_that("normalized canonical Event labels remain active in Phase 1B", {
  pool <- phase1b_candidate_pool(list(
    list(
      candidate_id = "tonic", final_label = "tonic",
      start_isi = 10, end_isi = 60
    ),
    list(
      candidate_id = "long_hyphen", final_label = "long-burst",
      start_isi = 25, end_isi = 30
    ),
    list(
      candidate_id = "hfs", final_label = "high_frequency_spiking",
      start_isi = 80, end_isi = 140
    ),
    list(
      candidate_id = "long_space", final_label = "long burst",
      start_isi = 100, end_isi = 104
    )
  ))
  out <- phase1b_resolve(pool)$phase1b

  expect_identical(
    out$state_parents$split_kind[out$state_parents$state_candidate_id == "tonic"],
    "none"
  )
  expect_true(any(
    out$overlays$state_candidate_id == "tonic" &
      out$overlays$event_candidate_id == "long_hyphen" &
      out$overlays$compatibility_rule == "burst_state_non_destructive_overlay"
  ))
  expect_identical(
    out$hfs_dominance$contributor_candidate_ids[
      out$hfs_dominance$root_hfs_candidate_id == "hfs"
    ],
    "long_space"
  )
  expect_true(any(
    out$overlays$event_candidate_id == "long_space" &
      out$overlays$compatibility_rule == "hfs_burst_overlay"
  ))
})

test_that("normalized canonical Broad-HFS State labels retain Phase 1B compatibility semantics", {
  pool <- phase1b_candidate_pool(list(
    list(
      candidate_id = "hfs_hyphen", final_label = "high-frequency-spiking",
      start_isi = 10, end_isi = 70
    ),
    list(
      candidate_id = "burst_in_hfs", final_label = "burst",
      start_isi = 30, end_isi = 34
    ),
    list(
      candidate_id = "broad_hfs_space", final_label = "high frequency spiking",
      start_isi = 80, end_isi = 130
    ),
    # HFT/HF-irregular are subtypes computed on the frozen Broad-HFS support;
    # an old HFT-labelled candidate must not create a second State parent.
    list(
      candidate_id = "legacy_hft_subtype", final_label = "high frequency tonic",
      start_isi = 80, end_isi = 130
    ),
    list(
      candidate_id = "burst_in_broad_hfs", final_label = "long_burst",
      start_isi = 100, end_isi = 104
    )
  ))
  out <- phase1b_resolve(pool)$phase1b

  expect_false(any(
    out$state_parents$state_candidate_id == "legacy_hft_subtype"
  ))
  expect_true(any(
    out$overlays$state_candidate_id == "hfs_hyphen" &
      out$overlays$event_candidate_id == "burst_in_hfs" &
      out$overlays$compatibility_rule == "hfs_burst_overlay"
  ))
  expect_identical(
    out$state_parents$split_kind[
      out$state_parents$state_candidate_id == "broad_hfs_space"
    ],
    "none"
  )
  expect_true(any(
    out$overlays$state_candidate_id == "broad_hfs_space" &
      out$overlays$event_candidate_id == "burst_in_broad_hfs" &
      out$overlays$compatibility_rule == "hfs_burst_overlay"
  ))
})

test_that("selected Pause splits HFS before child-level evidence is calculated", {
  pool <- phase1b_candidate_pool(list(
    list(candidate_id = "hfs", final_label = "high_frequency_spiking", start_isi = 10, end_isi = 109),
    list(candidate_id = "pause", final_label = "pause", start_isi = 60, end_isi = 60),
    list(candidate_id = "left_burst", final_label = "burst", start_isi = 20, end_isi = 24),
    list(candidate_id = "right_burst", final_label = "burst", start_isi = 80, end_isi = 84)
  ))
  out <- phase1b_resolve(pool)$phase1b
  fragments <- out$state_fragments

  expect_identical(fragments$start_isi, c(10L, 61L))
  expect_identical(fragments$end_isi, c(59L, 109L))
  expect_true(all(fragments$root_candidate_id == "hfs"))
  expect_true(out$state_parents$parent_consumed_in_provisional_policy)
  expect_identical(out$state_parents$split_kind, "pause_boundary")
  expect_equal(nrow(out$hfs_dominance), 2L)
  expect_equal(out$hfs_dominance$selected_event_group_count, c(1L, 1L))
  expect_true(any(
    out$relationships$compatibility_rule == "state_pause_direct_support_split"
  ))
})

test_that("Pause-created HFS children inherit parent acceptance and keep Burst evidence descriptive", {
  pool <- phase1b_candidate_pool(list(
    list(
      candidate_id = "hfs", final_label = "high_frequency_spiking",
      start_isi = 10, end_isi = 109
    ),
    list(candidate_id = "pause", final_label = "pause", start_isi = 60, end_isi = 60),
    list(
      candidate_id = "left_majority", final_label = "long_burst",
      start_isi = 15, end_isi = 50
    )
  ))

  # Deliberately omit raw train data/effective gates.  The left Pause-bounded
  # support child inherits the already-accepted parent episode. Burst coverage
  # remains an audit annotation and cannot delete the State.
  dom <- phase1b_resolve(pool)$phase1b$hfs_dominance
  left <- dom[dom$start_isi == 10L & dom$end_isi == 59L, , drop = FALSE]

  expect_true(left$majority_dominance_flag)
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

test_that("tonic and Broad HFS retain continuous State support with Burst overlays", {
  pool <- phase1b_candidate_pool(list(
    list(candidate_id = "tonic_parent", final_label = "tonic", start_isi = 10, end_isi = 40),
    list(candidate_id = "tonic_burst", final_label = "burst", start_isi = 20, end_isi = 23),
    list(candidate_id = "broad_hfs_parent", final_label = "high_frequency_spiking", start_isi = 50, end_isi = 90),
    list(candidate_id = "broad_hfs_burst", final_label = "long_burst", start_isi = 60, end_isi = 64)
  ))

  isi <- rep(0.08, 100L)
  isi[10:40] <- 0.050
  isi[20:23] <- 0.005
  isi[50:90] <- 0.020
  isi[60:64] <- 0.005
  dat <- data.frame(
    timestamp_sec = c(0, cumsum(isi[-1L])),
    ISI_sec = c(NA_real_, isi[-1L]),
    pattern_manual = "",
    pattern_manual_negative = "",
    stringsAsFactors = FALSE
  )
  params <- default_params()
  resolve_vp <- getFromNamespace(
    "stpd_event_grammar_params_impl", "SpikeTrainPatternDetector"
  )
  vp <- resolve_vp(dat, params, min_isi_sec = 0.001, train = "phase1b_regate")
  out <- phase1b_resolve(pool, dat = dat, params = params, variable_params = vp)$phase1b
  fragments <- out$state_fragments

  tonic <- fragments[fragments$root_candidate_id == "tonic_parent", , drop = FALSE]
  broad_hfs <- fragments[
    fragments$root_candidate_id == "broad_hfs_parent", , drop = FALSE
  ]
  expect_identical(tonic$start_isi, 10L)
  expect_identical(tonic$end_isi, 40L)
  expect_identical(broad_hfs$start_isi, 50L)
  expect_identical(broad_hfs$end_isi, 90L)
  expect_true(all(fragments$root_candidate_id == fragments$parent_candidate_id))
  expect_true(all(nzchar(fragments$fragment_candidate_id)))
  expect_equal(
    nrow(out$overlays[
      out$overlays$compatibility_rule == "burst_state_non_destructive_overlay",
    ]),
    1L
  )
  expect_equal(
    nrow(out$overlays[
      out$overlays$compatibility_rule == "hfs_burst_overlay",
    ]),
    1L
  )
  expect_true(all(out$state_parents$overlay_alternative_recorded))
  expect_true(all(out$state_parents$overlay_alternative_computed))
})

test_that("possible_burst remains Review-only before manual Event promotion", {
  pool <- phase1b_candidate_pool(list(
    list(
      candidate_id = "tonic_parent", final_label = "tonic",
      start_isi = 10, end_isi = 40
    ),
    list(
      candidate_id = "possible_tonic", final_label = "possible_burst",
      start_isi = 20, end_isi = 23
    ),
    list(
      candidate_id = "broad_hfs_parent", final_label = "high_frequency_spiking",
      start_isi = 50, end_isi = 80
    ),
    list(
      candidate_id = "possible_broad_hfs", final_label = "possible_burst",
      start_isi = 60, end_isi = 63
    ),
    list(
      candidate_id = "hfs_parent", final_label = "high_frequency_spiking",
      start_isi = 90, end_isi = 140
    ),
    list(
      candidate_id = "possible_hfs", final_label = "possible_burst",
      start_isi = 100, end_isi = 104
    ),
    list(
      candidate_id = "canonical_hfs", final_label = "burst",
      start_isi = 120, end_isi = 124
    )
  ))
  out <- phase1b_resolve(pool)
  phase1a <- out$phase1a
  phase1b <- out$phase1b
  possible_ids <- c(
    "possible_tonic", "possible_broad_hfs", "possible_hfs"
  )
  review <- phase1a$selected_candidates[
    phase1a$selected_candidates$candidate_id %in% possible_ids,
    , drop = FALSE
  ]

  expect_setequal(review$candidate_id, possible_ids)
  expect_true(all(review$semantic_track == "review"))
  expect_true(all(review$review_target_track == "event"))
  expect_true(all(review$review_target_label == "burst"))
  expect_true(all(review$review_promotion_required))
  expect_true(all(grepl(
    "not_accepted_event.*primary_event_metrics_ineligible.*",
    review$review_policy_status
  )))

  observed_states <- phase1b$state_parents[
    phase1b$state_parents$state_candidate_id %in%
      c("tonic_parent", "broad_hfs_parent"),
    , drop = FALSE
  ]
  expect_equal(nrow(observed_states), 2L)
  expect_true(all(observed_states$split_kind == "none"))
  expect_false(any(observed_states$parent_consumed_in_provisional_policy))

  dominance <- phase1b$hfs_dominance[
    phase1b$hfs_dominance$root_hfs_candidate_id == "hfs_parent",
    , drop = FALSE
  ]
  expect_equal(dominance$selected_event_candidate_count, 1L)
  expect_equal(dominance$selected_event_group_count, 1L)
  expect_equal(dominance$selected_event_covered_isi_n, 5L)
  expect_equal(dominance$selected_event_coverage, 5 / 51)
  expect_identical(dominance$contributor_candidate_ids, "canonical_hfs")
  expect_false(any(grepl("possible_", dominance$contributor_candidate_ids)))

  review_relationships <- phase1b$relationships[
    phase1b$relationships$source_candidate_id %in% possible_ids,
    , drop = FALSE
  ]
  expect_equal(nrow(review_relationships), 3L)
  expect_true(all(review_relationships$relationship_type == "review_overlay"))
  expect_true(all(
    review_relationships$compatibility_rule == "review_never_splits_state"
  ))
  expect_false(any(phase1b$overlays$event_candidate_id %in% possible_ids))
  expect_identical(
    phase1b$overlays$event_candidate_id[
      phase1b$overlays$compatibility_rule == "hfs_burst_overlay"
    ],
    "canonical_hfs"
  )
})

test_that("distributed and majority HFS dominance are evidence flags only", {
  distributed_events <- lapply(seq_along(c(15L, 30L, 45L, 60L, 75L, 90L)), function(i) {
    start <- c(15L, 30L, 45L, 60L, 75L, 90L)[i]
    list(
      candidate_id = paste0("distributed_", i), final_label = "burst",
      start_isi = start, end_isi = start + 4L
    )
  })
  pool <- phase1b_candidate_pool(c(
    list(list(
      candidate_id = "hfs_distributed", final_label = "high_frequency_spiking",
      start_isi = 10, end_isi = 109
    )),
    distributed_events,
    list(
      list(
        candidate_id = "hfs_majority", final_label = "high_frequency_spiking",
        start_isi = 130, end_isi = 229
      ),
      list(
        candidate_id = "majority_event", final_label = "long_burst",
        start_isi = 140, end_isi = 219
      )
    )
  ))
  out <- phase1b_resolve(pool)
  dom <- out$phase1b$hfs_dominance
  distributed <- dom[dom$root_hfs_candidate_id == "hfs_distributed", , drop = FALSE]
  majority <- dom[dom$root_hfs_candidate_id == "hfs_majority", , drop = FALSE]

  expect_equal(distributed$selected_event_group_count, 6L)
  expect_equal(distributed$selected_event_coverage, 0.30)
  expect_true(distributed$distributed_dominance_flag)
  expect_false(distributed$majority_dominance_flag)
  expect_true(majority$majority_dominance_flag)
  expect_true(all(dom$provisional_dominance_flag))
  expect_true(all(dom$state_selected_within_track_preserved))
  expect_false(any(dom$destructive_action_applied))
  expect_true(all(c(6L, 0.055, 0.25, 0.50) %in% c(
    distributed$group_floor_threshold,
    distributed$group_fraction_threshold,
    distributed$distributed_coverage_threshold,
    distributed$majority_coverage_threshold
  )))
  expect_true(all(c("hfs_distributed", "hfs_majority") %in%
    out$phase1a$selected_candidates$candidate_id))
})

test_that("adjacent selected event spans merge into one HFS evidence group", {
  pool <- phase1b_candidate_pool(list(
    list(candidate_id = "hfs", final_label = "high_frequency_spiking", start_isi = 10, end_isi = 109),
    list(candidate_id = "adjacent_a", final_label = "burst", start_isi = 20, end_isi = 24),
    list(candidate_id = "adjacent_b", final_label = "burst", start_isi = 25, end_isi = 29)
  ))
  dom <- phase1b_resolve(pool)$phase1b$hfs_dominance
  expect_equal(dom$selected_event_candidate_count, 2L)
  expect_equal(dom$selected_event_group_count, 1L)
  expect_identical(dom$merged_event_group_spans, "20-29")
})

test_that("MM is retained in audit but excluded narrowly from variable-HFS boolean gating", {
  pool <- phase1b_candidate_pool(list(
    list(
      candidate_id = "hfs_mm_only", final_label = "high_frequency_spiking",
      start_isi = 10, end_isi = 109, CV = 0.10, LV = 0.10, MM = 4.0,
      hf_spiking_large_fraction = 0.01
    ),
    list(candidate_id = "embedded", final_label = "burst", start_isi = 20, end_isi = 39)
  ))
  dom <- phase1b_resolve(pool)$phase1b$hfs_dominance

  expect_equal(dom$MM, 4.0)
  expect_true(dom$variable_by_mm_audit_only)
  expect_true(dom$legacy_variable_hfs_gate_with_mm)
  expect_false(dom$variable_hfs_gate_without_mm)
  expect_false(dom$mm_used_in_boolean_gate)
  expect_false(dom$packet_like_annotation)
  expect_false(dom$packet_annotations_destructive)
})

test_that("packet-like and packet-neighbor remain annotations without state deletion", {
  pool <- phase1b_candidate_pool(list(
    list(
      candidate_id = "hfs_packet", final_label = "high_frequency_spiking",
      start_isi = 10, end_isi = 49, CV = 0.70, LV = 0.10, MM = 1.5,
      hf_spiking_large_fraction = 0.01
    ),
    list(candidate_id = "packet_event", final_label = "burst", start_isi = 15, end_isi = 22),
    list(
      candidate_id = "hfs_neighbor", final_label = "high_frequency_spiking",
      start_isi = 53, end_isi = 90, CV = 0.70, LV = 0.10, MM = 1.5,
      hf_spiking_large_fraction = 0.01
    )
  ))
  out <- phase1b_resolve(pool)
  dom <- out$phase1b$hfs_dominance
  packet <- dom[dom$root_hfs_candidate_id == "hfs_packet", , drop = FALSE]
  neighbor <- dom[dom$root_hfs_candidate_id == "hfs_neighbor", , drop = FALSE]

  expect_true(packet$packet_like_annotation)
  expect_true(neighbor$packet_neighbor_annotation)
  expect_false(any(dom$packet_annotations_destructive))
  expect_false(any(dom$destructive_action_applied))
  expect_true(all(c("hfs_packet", "hfs_neighbor") %in%
    out$phase1a$selected_candidates$candidate_id))
})

test_that("Phase 1B integration is an independent attr and leaves legacy schemas untouched", {
  detect <- getFromNamespace(
    "stpd_detect_train_hf_protected_impl", "SpikeTrainPatternDetector"
  )
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
  out <- detect(
    dat, default_params(), min_isi_sec = 0.001,
    train = "phase1b_integration", lock_manual = FALSE
  )
  compatibility <- attr(out, "multitrack_compatibility_shadow")
  legacy_audit <- attr(out, "candidate_diagnostic_audit")

  expect_s3_class(compatibility, "stpd_multitrack_compatibility_shadow")
  expect_false(any(c(
    "semantic_track", "relationship_type", "provisional_dominance_flag",
    "selected_within_track"
  ) %in% names(legacy_audit)))
  expect_false(any(c(
    "pattern_auto_event", "pattern_auto_state", "pattern_auto_gap", "pattern_auto_review"
  ) %in% names(out)))
  expect_false(any(c("candidates", "selected_candidates") %in% names(compatibility)))
})

test_that("short-train early return carries typed empty shadow contracts", {
  detect <- getFromNamespace(
    "stpd_detect_train_hf_protected_impl", "SpikeTrainPatternDetector"
  )
  dat <- data.frame(
    timestamp_sec = 0,
    ISI_sec = NA_real_,
    pattern_manual = "",
    pattern_manual_negative = "",
    stringsAsFactors = FALSE
  )
  out <- detect(
    dat, default_params(), min_isi_sec = 0.001,
    train = "phase1b_empty_contract", lock_manual = FALSE
  )

  phase1a <- attr(out, "multitrack_shadow")
  phase1b <- attr(out, "multitrack_compatibility_shadow")
  expect_s3_class(phase1a, "stpd_multitrack_shadow")
  expect_s3_class(phase1b, "stpd_multitrack_compatibility_shadow")
  expect_equal(nrow(phase1a$candidates), 0L)
  expect_type(phase1a$candidates$track_selection_status, "character")
  expect_type(phase1a$candidates$review_target_track, "character")
  expect_type(phase1a$candidates$review_target_label, "character")
  expect_type(phase1a$candidates$review_promotion_required, "logical")
  expect_type(phase1a$candidates$review_policy_status, "character")
  expect_true(all(c(
    "candidate_id", "candidate_layer", "candidate_source", "final_label",
    "start_isi", "end_isi", "n_isi", "score", "priority",
    "source_candidate_index", "semantic_track", "selected_within_track",
    "track_selection_status", "review_target_track", "review_target_label",
    "review_promotion_required", "review_policy_status"
  ) %in% names(phase1a$selected_candidates)))
  expect_type(phase1a$selected_candidates$review_target_track, "character")
  expect_type(phase1a$selected_candidates$review_target_label, "character")
  expect_type(phase1a$selected_candidates$review_promotion_required, "logical")
  expect_type(phase1a$selected_candidates$review_policy_status, "character")
  expect_equal(nrow(phase1b$state_fragments), 0L)
  expect_false(phase1b$authoritative)
  expect_identical(out$pattern_auto, "")

  empty_out <- detect(
    dat[FALSE, , drop = FALSE], default_params(), min_isi_sec = 0.001,
    train = "phase1b_zero_row_contract", lock_manual = FALSE
  )
  expect_length(empty_out$pattern_auto, 0L)
  expect_s3_class(attr(empty_out, "multitrack_shadow"), "stpd_multitrack_shadow")
  expect_s3_class(
    attr(empty_out, "multitrack_compatibility_shadow"),
    "stpd_multitrack_compatibility_shadow"
  )

  reference <- phase1b_resolve(phase1b_candidate_pool(list(
    list(
      candidate_id = "schema_hfs", final_label = "high_frequency_spiking",
      start_isi = 10, end_isi = 109
    ),
    list(
      candidate_id = "schema_burst", final_label = "burst",
      start_isi = 20, end_isi = 24
    )
  )))$phase1b
  for (table in c(
    "state_parents", "state_fragments", "relationships", "overlays",
    "hfs_dominance", "invariants"
  )) {
    expect_identical(names(phase1b[[table]]), names(reference[[table]]), info = table)
  }
})
