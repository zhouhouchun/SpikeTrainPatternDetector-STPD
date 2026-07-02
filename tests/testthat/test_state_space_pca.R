test_that("ISI state-space features support label-free PCA trajectories", {
  ds <- stpd_golden_test_dataset("stable_high_frequency")
  dat <- ds$trains$train_1
  dat$pattern_auto[2:8] <- "high_frequency_spiking"
  dat$pattern_auto[13] <- "pause"

  feats <- stpd_make_isi_state_space_features(
    dat,
    train = "train_1",
    label_source = "final",
    k = 2,
    min_isi_sec = 0.0009,
    auto_others = FALSE
  )

  expect_gt(nrow(feats), 5)
  expect_true(all(c("log_isi_feature", "lag_0", "local_cv2", "local_lv", "label") %in% names(feats)))
  expect_false(any(vapply(feats[c("train", "label", "label_source")], is.list, logical(1))))
  expect_true("high_frequency_spiking" %in% feats$label)

  res <- stpd_run_isi_state_pca(feats, scaling = "robust")
  expect_true(all(c("PC1", "PC2", "PC3") %in% names(res$scores)))
  expect_equal(nrow(res$scores), nrow(feats))
  expect_true(all(c("feature", "PC1", "PC2", "PC3") %in% names(res$loadings)))
  expect_gt(nrow(res$loadings), 1)
  expect_true(is.numeric(res$variance$variance))
})

test_that("logISI phase portrait builds adjacent-ISI state pairs", {
  ds <- stpd_golden_test_dataset("middle_burst")
  dat <- ds$trains$train_1
  dat$pattern_manual[3:5] <- "burst"
  dat$pattern_manual[7] <- "pause"

  ph <- stpd_make_logisi_phase_portrait(
    dat,
    train = "train_1",
    label_source = "manual",
    min_isi_sec = 0.0009,
    lag = 1
  )

  expect_gt(nrow(ph), 2)
  expect_true(all(c("logISI_i", "logISI_next", "transition", "label", "next_label") %in% names(ph)))
  expect_true(any(ph$label == "burst"))
  expect_true(all(is.finite(ph$logISI_i)))
  expect_true(all(is.finite(ph$logISI_next)))
})

test_that("ISI state-space Isomap returns a bounded nonlinear embedding", {
  isi <- c(
    rep(c(0.032, 0.034, 0.030, 0.036), 12),
    rep(c(0.006, 0.007, 0.008, 0.040), 18),
    rep(c(0.018, 0.019, 0.021, 0.020), 10),
    c(0.140, 0.155, 0.170),
    rep(c(0.028, 0.060, 0.030, 0.055), 10)
  )
  spike_times <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(spike_times),
    timestamp_sec = spike_times,
    ISI_sec = c(NA_real_, diff(spike_times)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[dat$ISI_sec < 0.010] <- "burst"
  dat$pattern_auto[dat$ISI_sec > 0.120] <- "pause"

  feats <- stpd_make_isi_state_space_features(
    dat,
    train = "synthetic_train",
    label_source = "auto",
    k = 3,
    min_isi_sec = 0.0009,
    auto_others = FALSE
  )

  iso <- stpd_run_isi_state_isomap(
    feats,
    n_neighbors = 8,
    max_points = 80,
    scaling = "robust",
    ndim = 3
  )

	  expect_true(all(c("Isomap1", "Isomap2", "Isomap3") %in% names(iso$scores)))
	  expect_true(all(c("time_mid_sec", "ISI_sec", "log_isi", "local_rate_hz",
	                    "local_cv2", "local_lv", "prepost_ratio",
	                    "delta_logisi", "next_delta_logisi") %in% names(iso$scores)))
	  expect_gt(nrow(iso$scores), 20)
  expect_lte(nrow(iso$scores), 80)
  expect_equal(nrow(iso$diagnostics), 1)
  expect_true(all(is.finite(iso$scores$Isomap1)))
  expect_true(all(is.finite(iso$scores$Isomap2)))
  expect_true(all(is.finite(iso$scores$Isomap3)))
  expect_true(iso$diagnostics$n_neighbors >= 2)
  expect_true(is.finite(iso$diagnostics$stress) || is.na(iso$diagnostics$stress))
})

test_that("state-space labels keep AUTO, MANUAL, and final choices explicit", {
  ds <- stpd_golden_test_dataset("middle_burst")
  dat <- ds$trains$train_1
  dat$pattern_auto[3] <- "possible_burst"
  dat$pattern_manual[3] <- "burst"

  expect_equal(stpd_state_space_pattern_labels(dat, "auto", min_isi_sec = 0.0009)[3], "possible_burst")
  expect_equal(stpd_state_space_pattern_labels(dat, "manual", min_isi_sec = 0.0009)[3], "burst")
  expect_equal(stpd_state_space_pattern_labels(dat, "final", min_isi_sec = 0.0009)[3], "burst")
})
