make_distribution_test_train <- function(isi_sec) {
  ts <- c(0, cumsum(isi_sec))
  data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, isi_sec),
    pattern_manual = rep("", length(ts)),
    pattern_auto = rep("", length(ts)),
    stringsAsFactors = FALSE
  )
}

test_that("logISI moments and spike-count PMF are finite for usable trains", {
  isi <- c(rep(0.35, 8L), rep(0.03, 5L), 1.20, rep(0.35, 8L))
  moments <- stpd_logisi_moments(isi, min_isi_sec = 0.001)
  expect_equal(moments$n_valid_isi, length(isi))
  expect_true(is.finite(moments$logISI_mean))
  expect_true(is.finite(moments$logISI_robust_skew_q90_q10))

  dat <- make_distribution_test_train(isi)
  pmf <- stpd_spike_count_pmf(list(train_1 = dat), windows_sec = c(0.05, 0.10))
  expect_equal(nrow(pmf), 2L)
  expect_true(all(c("P_N0", "P_Nge3", "fano", "poisson_deviation_L1") %in% names(pmf)))
  expect_true(any(pmf$P_Nge3 > 0, na.rm = TRUE))
})

test_that("train distribution features provide phenotype scores without changing labels", {
  burst_dat <- make_distribution_test_train(c(rep(0.40, 6L), rep(0.025, 6L), rep(0.40, 6L)))
  tonic_dat <- make_distribution_test_train(rep(0.30, 24L))
  ds <- list(trains = list(burst_train = burst_dat, tonic_train = tonic_dat), results = list())
  before_auto <- ds$trains$burst_train$pattern_auto

  feats <- stpd_train_distribution_features(ds, min_isi_sec = 0.001)

  expect_equal(nrow(feats), 2L)
  expect_true(all(c("CV2", "LV", "LvR", "dominant_phenotype", "phenotype_confidence") %in% names(feats)))
  expect_true(any(feats$dominant_phenotype %in% c("burst_like", "tonic_like", "mixed_burst_pause_like")))
  expect_identical(ds$trains$burst_train$pattern_auto, before_auto)
})

test_that("event distribution evidence supports compact burst candidates and records audit-final labels", {
  dat <- make_distribution_test_train(c(rep(0.40, 6L), rep(0.025, 5L), rep(0.40, 6L)))
  dat$pattern_auto[8:12] <- "possible_burst"
  ds <- list(
    trains = list(train_1 = dat),
    results = list(),
    meta = list(display_name = "distribution_test")
  )
  cand <- data.frame(
    candidate_id = "cand_burst_1",
    train = "train_1",
    start_isi = 8L,
    end_isi = 12L,
    start_time_sec = dat$timestamp_sec[7],
    end_time_sec = dat$timestamp_sec[12],
    final_candidate_class = "possible_burst",
    stringsAsFactors = FALSE
  )
  ds$results$candidate_features <- cand
  ds <- stpd_apply_final_audit(ds, promote_possible = TRUE, min_isi_sec = 0.001,
                               audit_id = "distribution_audit_test")$dataset

  ev <- stpd_event_distribution_evidence(ds, candidates = cand, min_isi_sec = 0.001)

  expect_equal(nrow(ev), 1L)
  expect_equal(ev$candidate_id, "cand_burst_1")
  expect_equal(ev$audit_final_label, "burst")
  expect_true(ev$distribution_support %in% c("strong", "moderate"))
  expect_gt(ev$short_ISI_enrichment_vs_global, 0)
})

test_that("distributional result attachment stores all public tables without rewriting detector labels", {
  dat <- make_distribution_test_train(c(rep(0.40, 6L), rep(0.025, 5L), 1.20, rep(0.40, 6L)))
  dat$pattern_auto[8:12] <- "possible_burst"
  dat$pattern_auto[13] <- "pause"
  ds <- list(
    trains = list(train_1 = dat),
    results = list(candidate_features = data.frame(
      candidate_id = c("cand_burst_1", "cand_pause_1"),
      train = c("train_1", "train_1"),
      start_isi = c(8L, 13L),
      end_isi = c(12L, 13L),
      final_candidate_class = c("possible_burst", "pause"),
      stringsAsFactors = FALSE
    )),
    meta = list(display_name = "distribution_attach_test")
  )
  before <- ds$trains$train_1$pattern_auto
  out <- stpd_add_distributional_results(ds, params = default_params_sec(), selected_trains = "train_1")

  expect_true(all(c("event_distribution_evidence", "train_distribution_features", "spike_count_pmf") %in% names(out$results)))
  expect_gt(nrow(out$results$event_distribution_evidence), 0L)
  expect_gt(nrow(out$results$train_distribution_features), 0L)
  expect_gt(nrow(out$results$spike_count_pmf), 0L)
  expect_identical(out$trains$train_1$pattern_auto, before)
})
