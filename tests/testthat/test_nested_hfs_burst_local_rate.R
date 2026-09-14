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

test_that("the HFS-local route permits the classical three-spike minimum", {
  settings <- stpd_nested_hfs_detector_settings(
    default_params_sec(),
    list(min_spikes = 3L, long_max_spikes = 15L,
         hf_spiking_min_spikes = 20L)
  )

  expect_identical(settings$min_spikes, 3L)
  expect_identical(settings$local_min_radius, 3L)
})

test_that("homogeneous HFS has a mathematical minimum pair but no confirmed Burst", {
  fixture <- nested_hfs_detect(rep(0.020, 80L))

  expect_equal(nrow(fixture$candidates), 0L)
  expect_identical(fixture$candidates, stpd_nested_hfs_empty_candidates())
})

test_that("a true local rate acceleration becomes a Burst Event without changing HFS geometry", {
  isi <- rep(0.020, 80L)
  event_rows <- 32:35
  isi[event_rows - 1L] <- c(0.004, 0.004, 0.005, 0.004)
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 1L)
  candidate <- fixture$candidates[1L, , drop = FALSE]
  expect_identical(candidate$start_isi, 32L)
  expect_identical(candidate$end_isi, 35L)
  expect_identical(candidate$n_spikes, 5L)
  expect_identical(candidate$final_label, "burst")
  expect_identical(candidate$action, "accept")
  expect_false(candidate$review_only)
  expect_true(candidate$canonical_eligible)
  expect_true(candidate$nested_in_hfs)
  expect_false(candidate$absolute_pattern_threshold_used)
  expect_identical(candidate$detection_route,
                   "local_rate_and_boundary_contrast")
  expect_gte(min(candidate$final_left_ratio, candidate$final_right_ratio), 1.5)
  expect_identical(
    unname(as.integer(fixture$hfs[c("start_isi", "end_isi")])),
    c(2L, nrow(fixture$dat))
  )
})

test_that("immediate edge evidence is audited but is not a hard Event gate", {
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
  expect_identical(fixture$candidates$final_label, "burst")
  expect_true(fixture$candidates$canonical_eligible)
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

test_that("a two-ISI Burst3 passes through two-sided boundary contrast", {
  isi <- rep(0.020, 80L)
  isi[21:30] <- 0.016
  isi[31:32] <- 0.012
  fixture <- nested_hfs_detect(isi)

  expect_equal(nrow(fixture$candidates), 1L)
  candidate <- fixture$candidates[1L, , drop = FALSE]
  expect_identical(candidate$start_isi, 32L)
  expect_identical(candidate$end_isi, 33L)
  expect_identical(candidate$n_spikes, 3L)
  expect_identical(candidate$final_label, "burst")
  expect_true(candidate$detection_route %in% c(
    "two_sided_boundary_contrast", "local_rate_and_boundary_contrast"
  ))
  expect_true(candidate$canonical_eligible)
  expect_false(candidate$review_only)
})

test_that("either HFS-local evidence route is sufficient but neither means no Burst", {
  # A flat HFS has neither a local rate increase nor two-sided boundary
  # contrast and must remain free of nested Burst calls.
  flat <- nested_hfs_detect(rep(0.012, 80L))
  expect_equal(nrow(flat$candidates), 0L)

  # Put the short packet just inside the accepted HFS boundary. Both immediate
  # flanks exist, but there are too few robust background observations on the
  # left; this specifically exercises the boundary-only route.
  isi <- rep(0.012, 80L)
  isi[1L] <- 0.008
  isi[2:3] <- c(0.004, 0.004)
  isi[4L] <- 0.008
  boundary <- nested_hfs_detect(isi)
  expect_equal(nrow(boundary$candidates), 1L)
  expect_identical(boundary$candidates$detection_route,
                   "two_sided_boundary_contrast")
  expect_identical(boundary$candidates$n_spikes, 3L)
})

test_that("HFS variability raises contrast evidence without an absolute ISI gate", {
  candidate_for <- function(noisy) {
    isi <- if (noisy) rep(c(0.006, 0.018), 20L) else rep(0.012, 40L)
    isi[19L] <- 0.012
    isi[20:21] <- c(0.008, 0.008)
    isi[22L] <- 0.012
    dat <- nested_hfs_test_train(isi)
    values <- dat$ISI_sec
    valid <- is.finite(values) & values >= 0.001
    valid[1L] <- FALSE
    stpd_nested_hfs_candidate_from_seed(
      dat, values, valid, 2L, nrow(dat), 21L, "hfs_parent",
      stpd_nested_hfs_settings()
    )
  }

  stable <- candidate_for(FALSE)
  expect_equal(nrow(stable), 1L)
  expect_identical(stable$dynamic_side_ratio_min, 1.2)
  expect_false(stable$absolute_pattern_threshold_used)

  # The same 8-ms core and 12-ms immediate flanks are insufficient in a much
  # more variable HFS background; the comparison is relative, not absolute.
  expect_null(candidate_for(TRUE))
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

test_that("the nested-HFS detector uses an inclusive 1-ms validity floor", {
  isi <- rep(0.020, 80L)
  isi[31:34] <- c(0.001, 0.001, 0.001, 0.001)
  inclusive <- nested_hfs_detect(isi, min_isi_sec = 0.0009)
  expect_equal(nrow(inclusive$candidates), 1L)
  expect_identical(inclusive$candidates$final_label, "burst")

  below <- isi
  below[32L] <- 0.00099
  rejected <- nested_hfs_detect(below, min_isi_sec = 0.0009)
  expect_equal(nrow(rejected$candidates), 0L)
})

test_that("pipeline exposes nested HFS Bursts on the Event track", {
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
  automatic <- attr(out, "nested_hfs_burst_auto_candidates", exact = TRUE)
  audit <- attr(out, "candidate_diagnostic_audit", exact = TRUE)
  shadow <- attr(out, "multitrack_shadow", exact = TRUE)

  expect_equal(nrow(review), 0L)
  expect_gt(nrow(automatic), 0L)
  expect_true(all(automatic$final_label == "burst"))
  expect_true(all(automatic$canonical_eligible))
  expect_false(any(audit$candidate_layer == "nested_hfs_local_rate_contrast"))

  nested_shadow <- shadow$candidates[
    shadow$candidates$candidate_layer == "nested_hfs_local_rate_contrast",
    , drop = FALSE
  ]
  expect_equal(nrow(nested_shadow), nrow(automatic))
  expect_true(all(nested_shadow$semantic_track == "event"))
  expect_false(any(nested_shadow$review_promotion_required))

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
  expect_gte(nrow(stable(shadow, "event")), nrow(stable(canonical_shadow, "event")))
  expect_identical(stable(shadow, "state"), stable(canonical_shadow, "state"))
  expect_identical(stable(shadow, "gap"), stable(canonical_shadow, "gap"))
})
