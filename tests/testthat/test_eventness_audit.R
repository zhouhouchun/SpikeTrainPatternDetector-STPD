test_that("eventness feature columns are added", {
  ds <- stpd_golden_test_dataset("middle_burst")
  p <- default_params()
  ds <- run_detector(ds, p)
  feats <- ds$results$candidate_features
  expect_true(all(c("eventness_score", "regularity_score", "context_contrast", "return_to_baseline_score", "internal_outlier_count") %in% names(feats)))
})

test_that("eventness recommendation prioritizes burst-family when eventness is high", {
  p <- default_params()
  feat <- data.frame(
    final_candidate_class = "high_frequency_tonic",
    candidate_source = "synthetic_test",
    n_spikes = 5L,
    n_isi = 4L,
    q90_ISI_sec = 0.006,
    max_ISI_pct = 10,
    edge_contrast_min = 3.5,
    context_contrast = 4.0,
    eventness_score = 0.9,
    regularity_score = 0.95,
    short_ISI_fraction_35pct = 1.0,
    internal_outlier_count = 0,
    internal_outlier_max_ratio = 1.2,
    stringsAsFactors = FALSE
  )
  rec <- stpd_recommend_family_subtype(feat, p)
  expect_equal(rec$recommended_family[1], "burst_event")
  expect_true(rec$recommended_subtype[1] %in% c("high_frequency_burst", "classic_burst"))
})

test_that("state recommendation uses low eventness and high regularity", {
  p <- default_params()
  feat <- data.frame(
    final_candidate_class = "high_frequency_tonic",
    candidate_source = "synthetic_state",
    n_spikes = 9L,
    n_isi = 8L,
    q90_ISI_sec = 0.012,
    edge_contrast_min = 1.1,
    context_contrast = 1.2,
    eventness_score = 0.2,
    regularity_score = 0.9,
    short_ISI_fraction_35pct = 0.85,
    stringsAsFactors = FALSE
  )
  rec <- stpd_recommend_family_subtype(feat, p)
  expect_equal(rec$recommended_family[1], "state_epoch")
  expect_equal(rec$recommended_subtype[1], "high_frequency_tonic")
})
