nested_hfs_test_train <- function(isi_sec) {
  isi_sec <- as.numeric(isi_sec)
  timestamp <- c(0, cumsum(isi_sec))
  data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi_sec),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    stringsAsFactors = FALSE
  )
}

nested_hfs_test_parent <- function(dat, id = "hfs_parent") {
  data.frame(
    candidate_id = id,
    start_isi = 2L,
    end_isi = nrow(dat),
    final_label = "high_frequency_spiking",
    stringsAsFactors = FALSE
  )
}

nested_hfs_detect <- function(
    isi_sec, hard_boundaries = NULL, min_isi_sec = 0.001,
    settings = list()) {
  dat <- nested_hfs_test_train(isi_sec)
  hfs <- nested_hfs_test_parent(dat)
  list(
    dat = dat,
    hfs = hfs,
    candidates = stpd_event_core_detect_nested_hfs_bursts(
      dat, hfs, hard_boundaries = hard_boundaries,
      settings = settings, min_isi_sec = min_isi_sec
    )
  )
}

test_that("the four-spike floor also scopes the HFS-local contrast proposal", {
  settings <- stpd_nested_hfs_detector_settings(
    default_params_sec(),
    list(min_spikes = 3L, long_max_spikes = 15L,
         hf_spiking_min_spikes = 20L)
  )

  expect_identical(settings$min_spikes, 4L)
  expect_identical(settings$local_min_radius, 3L)
})

test_that("homogeneous HFS has a mathematical minimum pair but no confirmed Burst", {
  fixture <- nested_hfs_detect(rep(0.020, 80L))

  expect_equal(nrow(fixture$candidates), 0L)
  expect_identical(fixture$candidates, stpd_nested_hfs_empty_candidates())
})

test_that("a true local rate acceleration becomes review evidence without changing HFS geometry", {
  isi <- rep(0.020, 80L)
  event_rows <- 32:35
  isi[event_rows - 1L] <- c(0.004, 0.004, 0.005, 0.004)
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 1L)
  candidate <- fixture$candidates[1L, , drop = FALSE]
  expect_identical(candidate$start_isi, 32L)
  expect_identical(candidate$end_isi, 35L)
  expect_identical(candidate$n_spikes, 5L)
  expect_identical(candidate$final_label, "possible_burst")
  expect_identical(candidate$action, "demote_to_possible")
  expect_true(candidate$review_only)
  expect_false(candidate$canonical_eligible)
  expect_true(candidate$nested_in_hfs)
  expect_false(candidate$absolute_pattern_threshold_used)
  expect_gte(min(candidate$final_left_ratio, candidate$final_right_ratio), 1.5)
  expect_identical(
    unname(as.integer(fixture$hfs[c("start_isi", "end_isi")])),
    c(2L, nrow(fixture$dat))
  )
})

test_that("immediate edge evidence is audited but is not a hard review gate", {
  isi <- rep(0.020, 80L)
  isi[31:36] <- c(0.004, 0.004, 0.004, 0.004, 0.010, 0.010)
  isi[37] <- 0.0139
  fixture <- nested_hfs_detect(
    isi,
    settings = list(
      bridge_max_count = 2L,
      max_consecutive_borrowed = 2L,
      native_fraction_min = 0.55,
      edge_side_ratio_min = 1.50,
      edge_geom_ratio_min = 1.80
    )
  )

  expect_equal(nrow(fixture$candidates), 1L)
  expect_true(fixture$candidates$robust_edge_pass)
  expect_false(fixture$candidates$immediate_edge_pass)
  expect_identical(
    fixture$candidates$review_evidence_strength,
    "robust_two_sided_local_background"
  )
  expect_identical(fixture$candidates$final_label, "possible_burst")
})

test_that("one relative bridge joins two local cores", {
  isi <- rep(0.020, 80L)
  event_rows <- 32:36
  isi[event_rows - 1L] <- c(0.004, 0.004, 0.012, 0.004, 0.004)
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 1L)
  candidate <- fixture$candidates[1L, , drop = FALSE]
  expect_identical(candidate$start_isi, 32L)
  expect_identical(candidate$end_isi, 36L)
  expect_identical(candidate$bridge_isi_count, 1L)
  expect_lte(candidate$bridge_max_to_core_median_ratio, 3.5)
})

test_that("sustained borrowed HFS support rolls back to intrusion onset", {
  isi <- rep(0.020, 80L)
  isi[31:36] <- c(0.004, 0.004, 0.004, 0.010, 0.010, 0.010)
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 1L)
  candidate <- fixture$candidates[1L, , drop = FALSE]
  expect_identical(candidate$start_isi, 32L)
  expect_identical(candidate$end_isi, 34L)
  expect_identical(candidate$n_spikes, 4L)
  expect_true(candidate$rollback_right_to_intrusion_onset)
  expect_identical(candidate$bridge_isi_count, 0L)
})

test_that("a two-ISI Burst3 seed is below the HFS-local contrast floor", {
  isi <- rep(0.020, 80L)
  isi[21:30] <- 0.016
  isi[31:32] <- 0.012
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 0L)
})

test_that("data outside frozen Broad-HFS support cannot change a local proposal", {
  local <- rep(0.020, 80L)
  local[31:34] <- c(0.004, 0.004, 0.005, 0.004)
  outside_a <- c(rep(0.200, 20L), local, rep(0.200, 20L))
  outside_b <- c(rep(0.002, 20L), local, rep(0.080, 20L))

  detect_on_frozen_parent <- function(isi) {
    dat <- nested_hfs_test_train(isi)
    parent <- data.frame(
      candidate_id = "frozen_hfs_parent",
      start_isi = 22L,
      end_isi = 101L,
      final_label = "high_frequency_spiking",
      stringsAsFactors = FALSE
    )
    parent_before <- parent
    candidates <- stpd_event_core_detect_nested_hfs_bursts(
      dat, parent, min_isi_sec = 0.001
    )
    expect_identical(parent, parent_before)
    candidates
  }

  a <- detect_on_frozen_parent(outside_a)
  b <- detect_on_frozen_parent(outside_b)
  geometry <- c(
    "start_isi", "end_isi", "seed_start_isi", "seed_end_isi",
    "n_isi", "n_spikes"
  )
  expect_equal(nrow(a), 1L)
  expect_equal(nrow(b), 1L)
  expect_identical(a[, geometry, drop = FALSE], b[, geometry, drop = FALSE])
  expect_true(all(a$start_isi >= 22L & a$end_isi <= 101L))
  expect_true(all(b$start_isi >= 22L & b$end_isi <= 101L))
})

test_that("a spike-count ceiling never publishes a truncated low-ISI prefix", {
  isi <- rep(0.020, 80L)
  isi[29:43] <- 0.004
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 0L)
})

test_that("multiplicative time scaling preserves nested-Burst ISI geometry", {
  base <- rep(0.020, 80L)
  base[31:34] <- c(0.004, 0.004, 0.005, 0.004)
  factors <- c(1, 4, 10)
  results <- lapply(factors, function(scale) {
    nested_hfs_detect(base * scale)$candidates
  })

  geometry <- lapply(results, function(x) {
    x[, c("start_isi", "end_isi", "n_isi", "n_spikes"), drop = FALSE]
  })
  expect_true(all(vapply(results, nrow, integer(1)) == 1L))
  expect_identical(geometry[[1L]], geometry[[2L]])
  expect_identical(geometry[[1L]], geometry[[3L]])
  expect_equal(
    results[[1L]]$seed_left_ratio,
    results[[3L]]$seed_left_ratio,
    tolerance = 1e-12
  )
  expect_true(all(vapply(
    results, function(x) !any(x$absolute_pattern_threshold_used), logical(1)
  )))
})

test_that("canonical Pause and artifact ISIs are hard Event boundaries", {
  isi <- rep(0.020, 80L)
  isi[31:34] <- c(0.004, 0.004, 0.005, 0.004)
  pause_boundary <- data.frame(
    start_isi = 34L, end_isi = 34L,
    boundary_kind = "pause", hard_for_event = TRUE,
    stringsAsFactors = FALSE
  )
  pause_fixture <- nested_hfs_detect(
    isi, hard_boundaries = pause_boundary
  )
  expect_equal(nrow(pause_fixture$candidates), 0L)

  artifact_isi <- isi
  artifact_isi[33L] <- 0.0001
  artifact_fixture <- nested_hfs_detect(
    artifact_isi, min_isi_sec = 0.001
  )
  expect_equal(nrow(artifact_fixture$candidates), 0L)

  soft_boundary <- pause_boundary
  soft_boundary$hard_for_event <- FALSE
  soft_fixture <- nested_hfs_detect(
    isi, hard_boundaries = soft_boundary
  )
  expect_equal(nrow(soft_fixture$candidates), 1L)
  expect_identical(soft_fixture$candidates$start_isi, 32L)
  expect_identical(soft_fixture$candidates$end_isi, 35L)
})

test_that("pipeline exposes nested HFS proposals only on the Review track", {
  isi <- rep(0.006, 300L)
  isi[149:152] <- c(0.0018, 0.0018, 0.0020, 0.0018)
  dat <- nested_hfs_test_train(isi)
  params <- default_params()
  # This fixture tests the nested-Burst review layer, not automatic HFS
  # identifiability. Authorize its homogeneous parent HFS with the frozen
  # default threshold contract.
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"

  out <- stpd_detect_train_hf_protected_impl(
    dat, params, min_isi_sec = 0.0009,
    train = "nested_hfs_review_isolation", lock_manual = FALSE
  )
  review <- attr(out, "nested_hfs_burst_review_candidates", exact = TRUE)
  audit <- attr(out, "candidate_diagnostic_audit", exact = TRUE)
  shadow <- attr(out, "multitrack_shadow", exact = TRUE)

  expect_gt(nrow(review), 0L)
  expect_true(all(review$final_label == "possible_burst"))
  expect_true(all(review$canonical_eligible == FALSE))
  expect_false(any(audit$candidate_layer == "nested_hfs_local_rate_contrast"))

  nested_shadow <- shadow$candidates[
    shadow$candidates$candidate_layer == "nested_hfs_local_rate_contrast",
    , drop = FALSE
  ]
  expect_equal(nrow(nested_shadow), nrow(review))
  expect_true(all(nested_shadow$semantic_track == "review"))
  expect_true(all(nested_shadow$review_promotion_required))
  expect_false(any(nested_shadow$semantic_track == "event"))

  canonical_input <- shadow$candidates[
    shadow$candidates$candidate_layer != "nested_hfs_local_rate_contrast",
    , drop = FALSE
  ]
  shadow_columns <- c(
    "source_candidate_index", "semantic_track", "selected_within_track",
    "track_selection_status", "review_target_track", "review_target_label",
    "review_promotion_required", "review_policy_status"
  )
  canonical_input <- canonical_input[
    , setdiff(names(canonical_input), shadow_columns), drop = FALSE
  ]
  canonical_shadow <- stpd_multitrack_shadow_select(
    canonical_input,
    patterns = params$detector$patterns_to_run,
    params = params
  )
  stable <- function(x, track) {
    rows <- x$candidates[
      x$candidates$semantic_track == track &
        x$candidates$selected_within_track,
      c("candidate_id", "final_label", "start_isi", "end_isi"),
      drop = FALSE
    ]
    rows <- rows[order(rows$candidate_id, method = "radix"), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  expect_identical(stable(shadow, "event"), stable(canonical_shadow, "event"))
  expect_identical(stable(shadow, "state"), stable(canonical_shadow, "state"))
  expect_identical(stable(shadow, "gap"), stable(canonical_shadow, "gap"))
})
