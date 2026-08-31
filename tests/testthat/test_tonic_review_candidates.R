make_tonic_review_train <- function(isi) {
  timestamp <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi), pattern_manual = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
}

make_tonic_review_vp <- function(
    lower, upper, dmin, lv_max, min_spikes = 3L,
    source_mode = "user") {
  list(
    tonic_min = lower, tonic_max = upper,
    tonic_threshold_source_mode = source_mode,
    tonic_bridge_upper = upper, tonic_connector_max_n = 0L,
    tonic_min_spikes = min_spikes, tonic_min_duration = dmin,
    tonic_lv_max = lv_max, tonic_mm_min = 0.85, tonic_mm_max = 1.25,
    tonic_burst_overlap_ref = NA_real_, tonic_burst_overlap_guard = FALSE,
    tonic_burst_overlap_guard_factor = 1.15,
    tonic_burst_overlap_lower_quantile = 0.10,
    tonic_burst_overlap_low_fraction_max = 0.05,
    tonic_burst_overlap_reference_quantile = 0.95,
    tonic_short_regular_route_enabled = FALSE,
    seed_low = 0, seed_high = lower
  )
}

make_tonic_review_params <- function(lower, upper, dmin, lv_max) {
  params <- SpikeTrainPatternDetector::default_params()
  params$detector$patterns_to_run <- "tonic"
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$user$tonic <- list(
    enable = TRUE, seed_lower_sec = lower, seed_upper_sec = upper,
    bridge_upper_sec = upper, contrast_S = NA_real_
  )
  params$spiketrainpattern$engine$threshold_source_mode <- "user"
  params$spiketrainpattern$tonic$min_isi_sec <- lower
  params$spiketrainpattern$tonic$max_isi_sec <- upper
  params$spiketrainpattern$tonic$bridge_upper_sec <- upper
  params$spiketrainpattern$tonic$min_spikes <- 3L
  params$spiketrainpattern$tonic$min_duration_sec <- dmin
  params$spiketrainpattern$tonic$lv_max <- lv_max
  params$spiketrainpattern$tonic$mm_min <- 0.85
  params$spiketrainpattern$tonic$mm_max <- 1.25
  params
}

test_that("one sub-band ISI yields review-only Tonic evidence", {
  review_fun <- getFromNamespace(
    "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
  )
  detect_tonic <- getFromNamespace(
    "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
  )
  # Includes other low excursions in the same <=upper parent run.  The target
  # is the fold-7 mechanism: ISI 8 is only 1.19% below the frozen lower bound.
  dat <- make_tonic_review_train(c(
    0.080, 0.040631, 0.027115, 0.007888,
    0.021364, 0.024362, 0.020090, 0.022185, 0.028429,
    0.009162, 0.020665, 0.039933, 0.011462, 0.028964, 0.080
  ))
  params <- SpikeTrainPatternDetector::default_params()
  params$detector$patterns_to_run <- "tonic"
  vp <- make_tonic_review_vp(
    0.0203323246, 0.0474788274, 0.05283305, 0.0412484649351685
  )
  canonical <- detect_tonic(dat, params, vp, train = "fold7")
  expect_equal(nrow(canonical), 0L)

  out <- review_fun(
    dat, params, vp, train = "fold7", canonical_tonic = canonical
  )
  expect_gt(nrow(out), 0L)
  target <- out[out$start_isi <= 6L & out$end_isi >= 10L, , drop = FALSE]
  expect_equal(nrow(target), 1L)
  expect_identical(target$candidate_source, "near_lower_single_isi")
  expect_equal(target$below_lower_isi_n, 1L)
  expect_equal(target$native_band_fraction, 0.8)
  expect_equal(
    target$below_lower_max_deviation_fraction,
    (0.0203323246 - 0.020090) / 0.0203323246,
    tolerance = 1e-12
  )
  expect_identical(
    target$mm_semantics, "current_canonical_max_over_mean_v1"
  )
  expect_true(all(out$review_only))
  expect_false(any(out$canonical_eligible))
  expect_false(any(out$pause_or_qc_crossed))
  expect_true(all(out$above_upper_isi_n == 0L))
})

test_that("failed maximal band run yields short local regular Tonic proposal", {
  review_fun <- getFromNamespace(
    "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
  )
  detect_tonic <- getFromNamespace(
    "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
  )
  # The nine-ISI parent reproduces the fold-8 mechanism.  Its LV/MM fail, while
  # the final five-ISI local window is regular and calibration-supported.
  dat <- make_tonic_review_train(c(
    0.080, 0.022021, 0.031552, 0.026252, 0.053737,
    0.032333, 0.040056, 0.037961, 0.042192, 0.029252, 0.080
  ))
  params <- SpikeTrainPatternDetector::default_params()
  params$detector$patterns_to_run <- "tonic"
  vp <- make_tonic_review_vp(
    0.0160569276, 0.0544407456, 0.0457712, 0.082493017038585
  )
  canonical <- detect_tonic(dat, params, vp, train = "fold8")
  expect_equal(nrow(canonical), 0L)
  out <- review_fun(
    dat, params, vp, train = "fold8", canonical_tonic = canonical
  )
  expect_true(any(
    out$candidate_source == "local_regular_subwindow" &
      out$start_isi == 7L & out$end_isi == 11L
  ))
  expect_true(all(out$native_band_fraction == 1))
  expect_true(all(out$lv <= out$tonic_lv_max))
  expect_true(all(out$mm <= out$tonic_mm_effective_max))
  if (nrow(out) > 1L) {
    for (i in seq_len(nrow(out) - 1L)) {
      expect_false(any(
        out$start_isi[-seq_len(i)] <= out$end_isi[i] &
          out$end_isi[-seq_len(i)] >= out$start_isi[i]
      ))
    }
  }
})

test_that("review proposals stop at frozen upper, Pause, QC, and canonical Tonic", {
  review_fun <- getFromNamespace(
    "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
  )
  dat <- make_tonic_review_train(c(
    0.080, 0.021364, 0.024362, 0.020090, 0.022185, 0.028429, 0.080
  ))
  params <- SpikeTrainPatternDetector::default_params()
  params$detector$patterns_to_run <- "tonic"
  vp <- make_tonic_review_vp(
    0.0203323246, 0.0474788274, 0.05283305, 0.0412484649351685
  )
  ordinary <- review_fun(dat, params, vp, train = "limits")
  expect_gt(nrow(ordinary), 0L)

  canonical <- data.frame(start_isi = 3L, end_isi = 7L)
  expect_equal(nrow(review_fun(
    dat, params, vp, train = "limits", canonical_tonic = canonical
  )), 0L)
  pause <- data.frame(start_isi = 5L, end_isi = 5L)
  paused <- review_fun(
    dat, params, vp, train = "limits", pause_boundaries = pause
  )
  expect_false(any(
    paused$start_isi <= 5L & paused$end_isi >= 5L
  ))

  over <- dat
  over$ISI_sec[5L] <- vp$tonic_max * 1.01
  over$timestamp_sec <- c(0, cumsum(over$ISI_sec[-1L]))
  upper_stopped <- review_fun(over, params, vp, train = "limits")
  expect_false(any(
    upper_stopped$start_isi <= 5L & upper_stopped$end_isi >= 5L
  ))
  qc <- dat
  qc$ISI_sec[5L] <- 0.0005
  qc$timestamp_sec <- c(0, cumsum(qc$ISI_sec[-1L]))
  qc_stopped <- review_fun(qc, params, vp, min_isi_sec = 0.001, train = "limits")
  expect_false(any(qc_stopped$start_isi <= 5L & qc_stopped$end_isi >= 5L))
})

test_that("review-only side channel cannot contaminate canonical products", {
  detector <- getFromNamespace(
    "stpd_detect_train_hf_protected_impl", "SpikeTrainPatternDetector"
  )
  review_fun <- getFromNamespace(
    "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
  )
  lower <- 0.0203323246
  upper <- 0.0474788274
  dmin <- 0.05283305
  lv_max <- 0.0412484649351685
  dat <- make_tonic_review_train(c(
    0.080, 0.021364, 0.024362, 0.020090, 0.022185, 0.028429, 0.080
  ))
  params <- make_tonic_review_params(lower, upper, dmin, lv_max)
  out <- detector(dat, params, train = "integration", lock_manual = FALSE)
  review <- attr(out, "tonic_review_candidates", exact = TRUE)
  expect_s3_class(review, "data.frame")
  expect_gt(nrow(review), 0L)
  audit <- attr(out, "candidate_diagnostic_audit", exact = TRUE)
  expect_false(any(as.character(audit$final_label %||% "") == "possible_tonic"))
  expect_false(any(grepl("tonic_review", as.character(
    audit$candidate_source %||% ""
  ), fixed = TRUE)))
  shadow <- attr(out, "multitrack_shadow", exact = TRUE)
  shadow_candidates <- as.data.frame(shadow$candidates %||% data.frame())
  expect_false(any(as.character(
    shadow_candidates$final_label %||% ""
  ) == "possible_tonic"))
  expect_false(any(out$pattern_auto == "possible_tonic"))

  canonical_payload <- function(x) {
    y <- x
    attr(y, "tonic_review_candidates") <- NULL
    serialize(list(
      train = y,
      audit = attr(y, "candidate_diagnostic_audit", exact = TRUE),
      shadow = attr(y, "multitrack_shadow", exact = TRUE),
      compatibility = attr(y, "multitrack_compatibility_shadow", exact = TRUE)
    ), NULL, version = 3)
  }
  before <- canonical_payload(out)
  vp <- attr(out, "event_grammar_params", exact = TRUE)
  input_bytes <- serialize(out, NULL, version = 3)
  invisible(review_fun(out, params, vp, train = "integration"))
  expect_identical(serialize(out, NULL, version = 3), input_bytes)
  attr(out, "tonic_review_candidates") <- review[0, , drop = FALSE]
  expect_identical(canonical_payload(out), before)
})

test_that("pure HFS and random oscillation do not create canonical Tonic", {
  review_fun <- getFromNamespace(
    "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$detector$patterns_to_run <- "tonic"
  vp <- make_tonic_review_vp(0.020, 0.050, 0.040, 0.050)
  pure_hfs <- review_fun(
    make_tonic_review_train(rep(0.008, 20L)), params, vp, train = "hfs"
  )
  oscillating <- review_fun(
    make_tonic_review_train(rep(c(0.0205, 0.0495), 10L)),
    params, vp, train = "random"
  )
  stable_full_run <- review_fun(
    make_tonic_review_train(rep(0.030, 20L)), params, vp, train = "stable"
  )
  expect_equal(nrow(pure_hfs), 0L)
  expect_equal(nrow(oscillating), 0L)
  # A full regular band run belongs to the canonical path and is intentionally
  # not duplicated by the local review proposer.
  expect_equal(nrow(stable_full_run), 0L)

  auto_vp <- vp
  auto_vp$tonic_threshold_source_mode <- "auto"
  expect_equal(nrow(review_fun(
    make_tonic_review_train(rep(0.030, 20L)), params, auto_vp
  )), 0L)
})

test_that("Tonic review proposals are multiplicatively time-scale invariant", {
  review_fun <- getFromNamespace(
    "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
  )
  isi <- c(
    0.080, 0.040631, 0.027115, 0.007888,
    0.021364, 0.024362, 0.020090, 0.022185, 0.028429,
    0.009162, 0.020665, 0.039933, 0.011462, 0.028964, 0.080
  )
  dat <- make_tonic_review_train(isi)
  params <- SpikeTrainPatternDetector::default_params()
  params$detector$patterns_to_run <- "tonic"
  vp <- make_tonic_review_vp(
    0.0203323246, 0.0474788274, 0.05283305, 0.0412484649351685
  )
  base <- review_fun(dat, params, vp, min_isi_sec = 0.001, train = "scale")
  factor <- 1000
  scaled_dat <- dat
  scaled_dat$timestamp_sec <- scaled_dat$timestamp_sec * factor
  scaled_dat$ISI_sec <- scaled_dat$ISI_sec * factor
  scaled_vp <- vp
  for (field in c("tonic_min", "tonic_max", "tonic_min_duration")) {
    scaled_vp[[field]] <- scaled_vp[[field]] * factor
  }
  scaled <- review_fun(
    scaled_dat, params, scaled_vp,
    min_isi_sec = 0.001 * factor, train = "scale"
  )
  expect_equal(
    base[, c("start_isi", "end_isi", "n_isi", "candidate_source")],
    scaled[, c("start_isi", "end_isi", "n_isi", "candidate_source")]
  )
  expect_equal(
    base[, c("native_band_fraction", "below_lower_max_deviation_fraction",
             "cv", "lv", "mm")],
    scaled[, c("native_band_fraction", "below_lower_max_deviation_fraction",
               "cv", "lv", "mm")],
    tolerance = 1e-12
  )
  expect_equal(scaled$duration_sec, base$duration_sec * factor, tolerance = 1e-9)
  expect_equal(
    scaled$below_lower_max_deviation_sec,
    base$below_lower_max_deviation_sec * factor,
    tolerance = 1e-9
  )
})
