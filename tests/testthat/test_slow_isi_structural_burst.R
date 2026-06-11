test_that("slow trains use structural low-tail burst thresholds when all ISIs exceed classical burst bands", {
  make_train <- function(isi) {
    ts <- c(0, cumsum(isi))
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }

  isi <- rep(c(0.18, 0.22, 0.030, 0.030, 0.030, 0.24, 0.16, 0.20), 6)
  dat <- make_train(isi)
  ds <- SpikeTrainPatternDetector:::make_dataset(
    name = "slow_structural_burst",
    source = "synthetic",
    unit_in = "s",
    trains = list(train_1 = dat)
  )
  params <- default_params()
  min_isi <- params$detector$min_valid_isi_sec

  resolved <- stpd_resolve_thresholds_for_dataset(
    ds$trains,
    params,
    min_isi_sec = min_isi,
    bin_width_sec = params$event_grammar$histogram_bin_width_sec
  )
  burst_seed_hi <- resolved$threshold_table$effective_sec[
    resolved$threshold_table$pattern == "burst" &
      resolved$threshold_table$field == "seed_upper_sec"
  ][1]
  burst_bridge_hi <- resolved$threshold_table$effective_sec[
    resolved$threshold_table$pattern == "burst" &
      resolved$threshold_table$field == "bridge_upper_sec"
  ][1]

  expect_gte(burst_seed_hi, 0.030)
  expect_gte(burst_bridge_hi, burst_seed_hi)

  out <- stpd_detect(ds, params, selected_trains = "train_1", collect_diagnostics = TRUE)
  labels <- as.character(out$trains$train_1$pattern_auto)
  expect_true(any(labels == "burst"))

  audit <- attr(out$trains$train_1, "candidate_diagnostic_audit")
  expect_true(is.data.frame(audit))
  expect_true(any(as.character(audit$final_label) %in% c("burst", "long_burst", "possible_burst")))
})
