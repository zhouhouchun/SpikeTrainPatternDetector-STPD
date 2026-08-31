test_that("manual Burst structure learning is conservative with insufficient evidence", {
  defaults <- default_params_sec()$spiketrainpattern$burst
  events <- data.frame(
    n_spikes = c(4, 6, 8, 10),
    duration_sec = c(0.03, 0.05, 0.08, 0.10),
    contrast_min_q = c(1.3, 1.4, 1.5, 1.6),
    contrast_geom_q = c(1.4, 1.5, 1.6, 1.7)
  )

  learned <- getFromNamespace(
    "stpd_manual_burst_structure_estimates",
    "SpikeTrainPatternDetector"
  )(events, defaults = defaults, min_events = 5L)

  expect_false(learned$applied)
  expect_identical(learned$evidence_n, 0L)
  expect_equal(learned$min_isi_count, defaults$structure_first_min_isi_count)
  expect_equal(learned$max_isi_count, defaults$structure_first_max_isi_count)
  expect_equal(learned$contrast_min, defaults$structure_first_contrast_min)
  expect_equal(learned$geom_contrast_min, defaults$structure_first_geom_contrast_min)
})

test_that("manual Burst structure learning preserves the four-spike contrast floor", {
  defaults <- default_params_sec()$spiketrainpattern$burst
  events <- data.frame(
    n_spikes = c(3, 4, 6, 9, 12, 16),
    duration_sec = c(0.025, 0.04, 0.07, 0.12, 0.18, 0.25),
    contrast_min_q = c(1.20, 1.25, 1.35, 1.45, 1.55, 1.65),
    contrast_geom_q = c(1.30, 1.35, 1.45, 1.55, 1.65, 1.75)
  )

  learned <- getFromNamespace(
    "stpd_manual_burst_structure_estimates",
    "SpikeTrainPatternDetector"
  )(events, defaults = defaults)

  expect_true(learned$applied)
  expect_identical(learned$evidence_n, 6L)
  expect_identical(learned$min_isi_count, 3L)
  expect_equal(learned$min_isi_count, defaults$structure_first_min_isi_count)
  expect_gt(learned$max_isi_count, defaults$structure_first_max_isi_count)
  expect_lt(learned$contrast_min, defaults$structure_first_contrast_min)
  expect_lt(learned$geom_contrast_min, defaults$structure_first_geom_contrast_min)
  expect_lte(learned$possible_contrast_min, learned$contrast_min)
  expect_identical(learned$min_spikes, as.integer(defaults$classic_min_spikes))
  expect_lte(learned$max_isi_count, defaults$classic_max_spikes - 1L)
  expect_gt(learned$min_duration_sec, 0)
})

test_that("manual pool estimates reach the active structure-first detector namespace", {
  make_train <- function(i) {
    core <- c(0.010, 0.012, 0.011, 0.009, 0.013 + i * 0.0001)
    isi <- c(NA_real_, 0.020 + i * 0.0002, core, 0.022 + i * 0.0002)
    timestamp <- cumsum(replace(isi, 1L, 0))
    data.frame(
      idx = seq_along(timestamp),
      timestamp_sec = timestamp,
      ISI_sec = isi,
      pattern_manual = c("", "", rep("burst", length(core)), ""),
      pattern_manual_negative = "",
      pattern_auto = "",
      stringsAsFactors = FALSE
    )
  }
  pool <- stats::setNames(lapply(seq_len(6L), make_train), paste0("train_", seq_len(6L)))
  dataset_map <- stats::setNames(rep("calibration", length(pool)), names(pool))

  estimated <- estimate_params_from_manual_pool(
    pool,
    dataset_map = dataset_map,
    min_isi_sec = 0.001
  )
  effective <- effective_params_for_detector(estimated)
  learned <- estimated$metadata$manual_parameter_learning$burst_structure
  active <- getFromNamespace(
    "stpd_event_core_structure_first_settings",
    "SpikeTrainPatternDetector"
  )(effective, list(min_spikes = effective$event_core$min_spikes))

  expect_true(learned$applied)
  expect_identical(learned$evidence_n, 6L)
  expect_equal(effective$event_core$burst_contrast_min, learned$contrast_min)
  expect_equal(effective$event_core$possible_burst_contrast_min, learned$possible_contrast_min)
  expect_equal(active$min_isi_n, learned$min_isi_count)
  expect_equal(active$max_isi_n, learned$max_isi_count)
  expect_equal(active$contrast_min, learned$contrast_min)
  expect_equal(active$geom_contrast_min, learned$geom_contrast_min)
  expect_equal(effective$burst$D_min, learned$min_duration_sec)
  expect_true(isTRUE(estimated$stats$burst_structure_learning_applied))
  expect_identical(estimated$stats$burst_structure_evidence_n, 6L)
})

test_that("manual Burst boundary gaps do not inflate the internal bridge band", {
  make_train <- function(i) {
    core <- c(0.009, 0.010, 0.011, 0.012, 0.013 + i * 0.0001)
    isi <- c(NA_real_, 0.055, core, 0.060)
    timestamp <- cumsum(replace(isi, 1L, 0))
    data.frame(
      idx = seq_along(timestamp),
      timestamp_sec = timestamp,
      ISI_sec = isi,
      pattern_manual = c("", "", rep("burst", length(core)), ""),
      pattern_manual_negative = "",
      pattern_auto = "",
      stringsAsFactors = FALSE
    )
  }
  pool <- stats::setNames(lapply(seq_len(6L), make_train),
                          paste0("train_", seq_len(6L)))
  dataset_map <- stats::setNames(rep("calibration", length(pool)), names(pool))

  estimated <- estimate_params_from_manual_pool(
    pool, dataset_map = dataset_map, min_isi_sec = 0.001
  )

  expect_lt(estimated$burst$T_bridge, 0.020)
  expect_gt(estimated$burst$T_edge_pre, 0.050)
  expect_gt(estimated$burst$T_edge_post, 0.050)
})
