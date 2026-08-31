make_delta_preview_train <- function(isi) {
  t <- cumsum(c(0, isi))
  data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = rep("", length(t)),
    pattern_auto = rep("", length(t)),
    stringsAsFactors = FALSE
  )
}

test_that("parameter delta preview detects burst gained when seed upper is relaxed", {
  ds <- make_dataset(
    "delta_burst",
    "synthetic",
    list(train_1 = make_delta_preview_train(c(0.10, 0.20, 0.008, 0.012, 0.008, 0.20, 0.10))),
    unit_in = "s"
  )
  baseline <- default_params()
  baseline$event_core$seed_band_upper_sec <- 0.006
  baseline$spiketrainpattern$burst$seed_upper_sec <- 0.006
  baseline$event_grammar$threshold_source_mode <- "default"
  baseline$event_grammar$burst_detector_pipeline <- "threshold_resolved_base"
  baseline$spiketrainpattern$engine$threshold_source_mode <- "default"
  # This test isolates the legacy seed-band parameter. Structure-first is tested
  # separately and would intentionally recover the packet in both runs.
  baseline$spiketrainpattern$burst$structure_first_enabled <- FALSE

  current <- baseline
  current$event_core$seed_band_upper_sec <- 0.013
  current$spiketrainpattern$burst$seed_upper_sec <- 0.013

  preview <- stpd_parameter_delta_preview(ds, current, baseline, selected_trains = "train_1", max_trains = 1, iou_min = 0.25)
  expect_true(any(preview$counts$pattern == "burst" & preview$counts$delta_n > 0))
  expect_true(any(preview$event_diff$status == "added_event" & preview$event_diff$current_pattern == "burst"))
  expect_true(any(preview$parameter_changes$path == "event_core.seed_band_upper_sec"))

  overlay <- stpd_parameter_delta_overlay_rows(preview, ds$trains, selected_trains = "train_1")
  expect_true(any(overlay$status == "added_event"))
  expect_true(all(is.finite(overlay$start_align_sec)))
  expect_true(all(overlay$end_align_sec >= overlay$start_align_sec))

  out_dir <- tempfile("delta_preview_export_")
  stpd_parameter_delta_export(preview, out_dir)
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_summary.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_counts.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_events.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_state_counts.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_state_episodes.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_state_direct_support.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_overlap_resolution.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_parameter_changes.csv")))
})

test_that("parameter delta preview detects pause removed when pause threshold is tightened", {
  ds <- make_dataset(
    "delta_pause",
    "synthetic",
    list(train_1 = make_delta_preview_train(c(0.05, 0.05, 0.05, 0.18, 0.05, 0.05, 0.30, 0.05))),
    unit_in = "s"
  )
  baseline <- default_params()
  baseline$detector$patterns_to_run <- "pause"
  baseline$spiketrainpattern$engine$threshold_source_mode <- "default"
  baseline$event_grammar$threshold_source_mode <- "default"
  baseline$pause$T_seed <- 0.10
  baseline$pause$T_strong <- 0.10
  baseline$spiketrainpattern$pause$min_isi_sec <- 0.10
  baseline$spiketrainpattern$pause$max_isi_sec <- 0.10

  current <- baseline
  current$pause$T_seed <- 0.35
  current$pause$T_strong <- 0.35
  current$spiketrainpattern$pause$min_isi_sec <- 0.35
  current$spiketrainpattern$pause$max_isi_sec <- 0.35

  preview <- stpd_parameter_delta_preview(ds, current, baseline, selected_trains = "train_1", max_trains = 1, iou_min = 0.25)
  expect_true(any(preview$counts$pattern == "pause" & preview$counts$delta_n < 0))
  expect_true(any(preview$event_diff$status == "removed_event" & preview$event_diff$baseline_pattern == "pause"))
})

test_that("parameter delta preview is a dry-run and does not pollute formal dataset results", {
  ds <- stpd_golden_test_dataset("middle_burst")
  ds_before <- ds
  current <- default_params()
  current$event_core$seed_band_upper_sec <- 0.012
  current$spiketrainpattern$burst$seed_upper_sec <- 0.012

  preview <- stpd_parameter_delta_preview(ds, current, default_params(), selected_trains = "train_1", max_trains = 1)
  expect_true(is.list(preview))
  expect_identical(ds$trains, ds_before$trains)
  expect_identical(ds$results, ds_before$results)
  expect_identical(ds$params_last, ds_before$params_last)
})

test_that("real-data dry-run preview works on a small loaded subset", {
  path <- system.file("extdata", "STN_2017_subset.csv", package = "SpikeTrainPatternDetector")
  skip_if(!file.exists(path))
  ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
  ds_before <- ds
  target <- head(names(ds$trains), 1)
  current <- default_params()
  current$event_core$seed_band_upper_sec <- 0.012
  current$spiketrainpattern$burst$seed_upper_sec <- 0.012
  preview <- stpd_parameter_delta_preview(ds, current, default_params(), selected_trains = target, max_trains = 1)
  expect_true(is.data.frame(preview$summary))
  expect_equal(preview$selected_trains, target)
  expect_identical(ds$results, ds_before$results)
})

make_delta_state_product <- function(current = FALSE) {
  tonic_end <- if (isTRUE(current)) 7L else 6L
  hfs_start <- if (isTRUE(current)) 11L else 10L
  hfs_subtype <- if (isTRUE(current)) {
    "high_frequency_irregular"
  } else {
    "high_frequency_tonic"
  }
  states <- data.frame(
    train = rep("train_1", 3L),
    state_id = c("tonic_1", "hfs_fragment_1", "hfs_fragment_2"),
    state_class = c(
      "tonic", "high_frequency_spiking", "high_frequency_spiking"
    ),
    state_subtype = c("tonic", hfs_subtype, hfs_subtype),
    state_episode_id = c("", "hfs_episode_1", "hfs_episode_1"),
    episode_start_isi = c(NA_integer_, hfs_start, hfs_start),
    episode_end_isi = c(NA_integer_, 20L, 20L),
    start_isi = c(2L, hfs_start, 16L),
    end_isi = c(tonic_end, 14L, 20L),
    stringsAsFactors = FALSE
  )
  per_isi_rows <- function(index, state_class, state_subtype, state_id,
                           episode_id, support_role) {
    data.frame(
      train = "train_1", isi_index = as.integer(index),
      state_class = state_class, state_subtype = state_subtype,
      state_id = state_id, state_episode_id = episode_id,
      state_support_role = support_role, stringsAsFactors = FALSE
    )
  }
  per_isi <- rbind(
    per_isi_rows(
      2L:tonic_end, "tonic", "tonic", "tonic_1", "", ""
    ),
    per_isi_rows(
      hfs_start:14L, "high_frequency_spiking", hfs_subtype,
      "hfs_fragment_1", "hfs_episode_1", "direct_support"
    ),
    per_isi_rows(
      15L, "high_frequency_spiking", hfs_subtype, "",
      "hfs_episode_1", "canonical_pause_gap"
    ),
    per_isi_rows(
      16L:20L, "high_frequency_spiking", hfs_subtype,
      "hfs_fragment_2", "hfs_episode_1", "direct_support"
    )
  )
  list(states = states, per_isi = per_isi)
}

test_that("State episode and direct-support helpers keep envelope and support separate", {
  baseline <- make_delta_state_product(FALSE)
  current <- make_delta_state_product(TRUE)
  baseline_episodes <- stpd_delta_state_episode_table(baseline)
  current_episodes <- stpd_delta_state_episode_table(current)

  expect_equal(nrow(baseline_episodes), 2L)
  baseline_hfs <- baseline_episodes[
    baseline_episodes$state_class == "high_frequency_spiking", , drop = FALSE
  ]
  expect_equal(baseline_hfs$start_isi, 10L)
  expect_equal(baseline_hfs$end_isi, 20L)
  expect_equal(baseline_hfs$n_isi, 11L)
  expect_equal(baseline_hfs$direct_support_isi_n, 10L)
  expect_equal(baseline_hfs$support_fragment_n, 2L)

  episode_diff <- stpd_parameter_delta_state_episode_diff(
    baseline_episodes, current_episodes, iou_min = 0.25
  )
  expect_equal(sum(episode_diff$status == "state_boundary_changed"), 2L)

  baseline_replaced <- baseline_episodes[
    baseline_episodes$state_class == "tonic", , drop = FALSE
  ]
  current_replaced <- baseline_replaced
  current_replaced$state_class <- "high_frequency_spiking"
  current_replaced$state_subtype <- "high_frequency_tonic"
  current_replaced$episode_key <- "replacement_hfs"
  replacement_diff <- stpd_parameter_delta_state_episode_diff(
    baseline_replaced, current_replaced, iou_min = 0.25
  )
  expect_setequal(
    replacement_diff$status,
    c("removed_state_episode", "added_state_episode")
  )
  expect_false(any(replacement_diff$status == "state_class_changed"))

  baseline_support <- stpd_delta_state_support_table(baseline)
  current_support <- stpd_delta_state_support_table(current)
  expect_false(15L %in% baseline_support$isi_index)
  expect_equal(
    sum(baseline_support$state_class == "high_frequency_spiking"), 10L
  )
  support_diff <- stpd_parameter_delta_state_support_diff(
    baseline_support, current_support
  )
  expect_true(any(
    support_diff$status == "removed_direct_support" &
      support_diff$isi_index == 10L
  ))
  expect_true(any(
    support_diff$status == "added_direct_support" &
      support_diff$isi_index == 7L
  ))
  expect_equal(
    sum(support_diff$status == "direct_support_subtype_changed"), 9L
  )
})

make_delta_overlap_audit <- function(resolution, post_start, residual_pass) {
  data.frame(
    train = c("train_1", "train_1"),
    candidate_id = c("hfs_candidate", "tonic_candidate"),
    final_label = c("high_frequency_spiking", "tonic"),
    suppressed_original_label = c("", ""),
    state_overlap_resolution = c(resolution, resolution),
    state_overlap_geometry = c("left_edge", "left_edge"),
    state_overlap_peer_candidate_id = c("tonic_candidate", "hfs_candidate"),
    state_overlap_n_isi = c(10L, 10L),
    pre_resolution_start_isi = c(10L, 10L),
    pre_resolution_end_isi = c(40L, 40L),
    post_resolution_start_isi = c(post_start, 10L),
    post_resolution_end_isi = c(40L, 19L),
    start_isi = c(post_start, 10L),
    end_isi = c(40L, 19L),
    residual_gate_pass = c(residual_pass, NA),
    residual_gate_status = c(
      if (residual_pass) "passed" else "failed", "peer_receipt"
    ),
    residual_failed_checks = c(
      if (residual_pass) "" else "min_spikes", ""
    ),
    resolution_reason = c("fixture", "fixture"),
    root_hfs_candidate_id = c("hfs_candidate", "hfs_candidate"),
    stringsAsFactors = FALSE
  )
}

test_that("overlap-resolution delta is HFS-rooted and does not double-count peers", {
  baseline <- stpd_delta_overlap_resolution_table(
    make_delta_overlap_audit(
      "resolved_edge_tonic_hfs_transition", 20L, TRUE
    )
  )
  current <- stpd_delta_overlap_resolution_table(
    make_delta_overlap_audit(
      "edge_candidate_residual_failed_keep_parent", 10L, FALSE
    )
  )
  expect_equal(nrow(baseline), 1L)
  expect_equal(nrow(current), 1L)
  expect_equal(baseline$root_hfs_candidate_id, "hfs_candidate")
  delta <- stpd_parameter_delta_overlap_resolution_diff(
    baseline, current, iou_min = 0.25
  )
  expect_equal(nrow(delta), 1L)
  expect_equal(delta$status, "overlap_resolution_changed")
  expect_equal(delta$baseline_root_hfs_candidate_id, "hfs_candidate")
  expect_equal(delta$current_root_hfs_candidate_id, "hfs_candidate")
  expect_true(delta$baseline_residual_gate_pass)
  expect_false(delta$current_residual_gate_pass)
  expect_equal(delta$current_residual_failed_checks, "min_spikes")
})

test_that("real detector fixture reports removed Tonic State without changing legacy API", {
  ds <- make_dataset(
    "delta_state", "synthetic",
    # A single unambiguous regular mid-ISI State keeps this contract focused on
    # preview accounting rather than Tonic/HFS boundary arbitration.
    list(train_1 = make_delta_preview_train(rep(0.05, 20L))),
    unit_in = "s"
  )
  ds_before <- ds
  baseline <- default_params()
  baseline$detector$patterns_to_run <- c(
    "tonic", "high_frequency_spiking"
  )
  current <- baseline
  current$detector$patterns_to_run <- "high_frequency_spiking"

  preview <- stpd_parameter_delta_preview(
    ds, current, baseline, selected_trains = "train_1", max_trains = 1L,
    label_blind = TRUE
  )
  expect_true(all(c(
    "summary", "counts", "event_diff", "event_diff_all",
    "parameter_changes", "baseline_events", "current_events",
    "selected_trains", "iou_min", "source"
  ) %in% names(preview)))
  expect_true(all(c(
    "state_counts", "state_episode_diff", "state_direct_support_diff",
    "overlap_resolution_diff"
  ) %in% names(preview)))
  expect_true(any(
    preview$state_episode_diff$status == "removed_state_episode" &
      preview$state_episode_diff$baseline_state_class == "tonic"
  ))
  expect_true(any(
    preview$state_direct_support_diff$status == "removed_direct_support" &
      preview$state_direct_support_diff$baseline_state_class == "tonic"
  ))
  tonic_delta <- preview$state_counts[
    preview$state_counts$state_class == "tonic", , drop = FALSE
  ]
  expect_equal(tonic_delta$delta_n, -1L)
  summary_value <- function(metric) {
    preview$summary$value[match(metric, preview$summary$metric)]
  }
  expect_equal(summary_value("baseline_tonic_state_episode_n"), "1")
  expect_equal(summary_value("current_tonic_state_episode_n"), "0")
  expect_identical(ds$trains, ds_before$trains)
  expect_identical(ds$results, ds_before$results)
  expect_identical(ds$params_last, ds_before$params_last)
})
