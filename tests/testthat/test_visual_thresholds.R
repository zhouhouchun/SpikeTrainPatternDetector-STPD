test_that("visual spike tick visual semantics are uniform black", {
  st <- pattern_style("burst", source = "manual", mode = "source_priority")
  expect_equal(st$color, "#000000")
  expect_equal(st$dash, "solid")
  expect_equal(st$width, 1.0)
  st2 <- pattern_style("burst", source = "auto", mode = "source_priority")
  expect_equal(st2$color, "#000000")
  expect_equal(st2$dash, "solid")
  expect_equal(st2$width, 1.0)
  st3 <- pattern_style("possible_burst", source = "review", mode = "source_priority")
  expect_equal(st3$color, "#000000")
  expect_equal(st3$dash, "solid")
  expect_equal(st3$width, 1.0)
})

test_that("train-specific ISI range metrics are finite for valid ISIs", {
  x <- c(NA, 0.01, 0.02, 0.04, 0.08)
  m <- compute_isi_range_metrics_vector(x, min_isi_sec = 0.001)
  expect_equal(nrow(m), length(x))
  expect_true(all(is.finite(m$ISI_range_pct_linear[2:5])))
  expect_true(all(m$ISI_range_pct_linear[2:5] >= 0 & m$ISI_range_pct_linear[2:5] <= 100))
})

test_that("train-specific thresholds merge into adaptive ranges", {
  p <- default_params_sec()
  thr <- list(train_a = list(burst_max_sec = 0.02, pause_min_sec = 0.3, tonic_min_sec = 0.05, tonic_max_sec = 0.12))
  p2 <- merge_train_isi_thresholds_into_params(p, thr)
  expect_true(isTRUE(p2$burst$adaptive_use_train_ranges))
  expect_equal(p2$burst$adaptive_train_ranges$train_a$high_sec, 0.02)
  expect_equal(p2$pause$adaptive_train_ranges$train_a$low_sec, 0.3)
  expect_equal(p2$tonic$adaptive_train_ranges$train_a$low_sec, 0.05)
  expect_equal(p2$tonic$adaptive_train_ranges$train_a$high_sec, 0.12)
  expect_equal(p2$burst$adaptive_train_ranges$train_a$threshold_mode, "soft_anchor")
  expect_false(isTRUE(p2$burst$adaptive_train_ranges$train_a$hard_threshold))
  expect_true(stpd_range_is_manual_anchor(p2$burst$adaptive_train_ranges$train_a))

  tab <- train_isi_threshold_dataframe(thr)
  expect_true(all(c("threshold_mode", "hard_threshold") %in% names(tab)))
  expect_equal(tab$threshold_mode[[1]], "soft_anchor")
  expect_false(isTRUE(tab$hard_threshold[[1]]))

  hard_thr <- list(train_a = list(
    burst_max_sec = 0.02,
    pause_min_sec = 0.3,
    tonic_min_sec = 0.05,
    tonic_max_sec = 0.12,
    threshold_mode = "hard_threshold",
    hard_threshold = TRUE
  ))
  p3 <- merge_train_isi_thresholds_into_params(p, hard_thr)
  expect_equal(p3$burst$adaptive_train_ranges$train_a$threshold_mode, "hard_threshold")
  expect_true(isTRUE(p3$burst$adaptive_train_ranges$train_a$hard_threshold))
  expect_false(stpd_range_is_manual_anchor(p3$burst$adaptive_train_ranges$train_a))
})

test_that("ISI profile threshold lines are applied to event grammar parameters", {
  dat <- data.frame(
    idx = seq_len(13),
    timestamp_sec = cumsum(c(0, rep(0.08, 12))),
    ISI_sec = c(NA_real_, rep(0.08, 12)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  p <- default_params_sec()
  thr <- list(train_a = list(
    burst_max_sec = 0.03,
    pause_min_sec = 0.4,
    tonic_min_sec = 0.025,
    tonic_max_sec = 0.12,
    threshold_mode = "soft_anchor"
  ))
  p2 <- merge_train_isi_thresholds_into_params(p, thr)
  p2 <- stpd_attach_thresholds_to_params(p2, list(trains = list(train_a = dat)), min_isi_sec = 0.001)
  vp <- stpd_event_grammar_params(dat, p2, min_isi_sec = 0.001, train = "train_a")
  expect_gte(vp$seed_high, 0.03)
  expect_gte(vp$bridge_high, 0.03)
  expect_equal(vp$pause_thr, 0.4)
  expect_gte(vp$tonic_min, 0.03 * 1.15)
  expect_equal(vp$tonic_max, 0.12)

  hard_thr <- list(train_a = list(
    burst_max_sec = 0.02,
    threshold_mode = "hard_threshold",
    hard_threshold = TRUE
  ))
  p3 <- merge_train_isi_thresholds_into_params(default_params_sec(), hard_thr)
  p3 <- stpd_attach_thresholds_to_params(p3, list(trains = list(train_a = dat)), min_isi_sec = 0.001)
  vp_hard <- stpd_event_grammar_params(dat, p3, min_isi_sec = 0.001, train = "train_a")
  expect_equal(vp_hard$seed_high, 0.02)
})

test_that("hard ISI threshold lines create direct auditable burst candidates", {
  isi <- c(0.40, 0.40, 0.050, 0.081, 0.079, 0.085, 0.046, 0.084, 0.120, 0.20, 0.40)
  ts <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "",
    pattern_manual_negative = "",
    stringsAsFactors = FALSE
  )
  hard_thr <- list(train_a = list(
    burst_max_sec = 0.07,
    pause_min_sec = 0.30,
    tonic_min_sec = 0.10,
    tonic_max_sec = 0.20,
    threshold_mode = "hard_threshold",
    hard_threshold = TRUE,
    source = "ui_isi_profile_threshold_line"
  ))
  p <- merge_train_isi_thresholds_into_params(default_params_sec(), hard_thr)
  p <- stpd_attach_thresholds_to_params(p, list(trains = list(train_a = dat)), min_isi_sec = 0.001)
  out <- run_detector_one_train(dat, p, min_isi_sec = 0.001, train = "train_a", lock_manual = FALSE)

  expect_true(all(out$pattern_auto[4:9] == "burst"))
  audit <- attr(out, "candidate_diagnostic_audit")
  hard <- audit[
    audit$candidate_layer == "isi_profile_hard_threshold_burst" &
      audit$selected_for_auto == TRUE,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(hard), 0L)
  expect_true(any(hard$start_isi <= 4L & hard$end_isi >= 9L))
  expect_true(all(hard$hard_threshold_pattern == "burst"))
  expect_true(all(hard$threshold_mode == "hard_threshold"))
  expect_false(any(audit$candidate_layer == "isi_profile_hard_threshold_pause", na.rm = TRUE))
  expect_false(any(audit$candidate_layer == "isi_profile_hard_threshold_tonic", na.rm = TRUE))

  soft_thr <- list(train_a = list(
    burst_max_sec = 0.07,
    threshold_mode = "soft_anchor",
    hard_threshold = FALSE,
    source = "isi_profile_threshold_line_soft_anchor"
  ))
  p_soft <- merge_train_isi_thresholds_into_params(default_params_sec(), soft_thr)
  p_soft <- stpd_attach_thresholds_to_params(p_soft, list(trains = list(train_a = dat)), min_isi_sec = 0.001)
  out_soft <- run_detector_one_train(dat, p_soft, min_isi_sec = 0.001, train = "train_a", lock_manual = FALSE)
  audit_soft <- attr(out_soft, "candidate_diagnostic_audit")
  expect_false(any(audit_soft$candidate_layer == "isi_profile_hard_threshold_burst", na.rm = TRUE))
})

test_that("hard burst thresholds do not override valid HF-spiking states", {
  isi <- rep(0.012, 50L)
  ts <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "",
    pattern_manual_negative = "",
    stringsAsFactors = FALSE
  )
  hard_thr <- list(train_a = list(
    burst_max_sec = 0.02,
    threshold_mode = "hard_threshold",
    hard_threshold = TRUE,
    source = "ui_isi_profile_threshold_line"
  ))
  p <- merge_train_isi_thresholds_into_params(default_params_sec(), hard_thr)
  p <- stpd_attach_thresholds_to_params(p, list(trains = list(train_a = dat)), min_isi_sec = 0.001)
  out <- run_detector_one_train(dat, p, min_isi_sec = 0.001, train = "train_a", lock_manual = FALSE)

  expect_true(all(out$pattern_auto[2:nrow(out)] == "high_frequency_spiking"))
  audit <- attr(out, "candidate_diagnostic_audit")
  hard_long <- audit[
    audit$candidate_layer == "isi_profile_hard_threshold_burst" &
      audit$final_label == "long_burst",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(hard_long), 0L)
  expect_false(any(hard_long$selected_for_auto, na.rm = TRUE))
  hfs <- audit[
    audit$final_label == "high_frequency_spiking" &
      audit$selected_for_auto == TRUE,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(hfs), 0L)
})
