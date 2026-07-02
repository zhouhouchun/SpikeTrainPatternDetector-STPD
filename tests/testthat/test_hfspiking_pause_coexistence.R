make_hfspiking_pause_train <- function(left_n = 90L, right_n = 90L, fast_isi = 0.010, pause_isi = 0.110) {
  isi <- c(rep(fast_isi, left_n), pause_isi, rep(fast_isi, right_n))
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

test_that("pause embedded in an HF-spiking state is detected as a pause", {
  left_n <- 90L
  pause_idx <- left_n + 2L
  dat <- make_hfspiking_pause_train(left_n = left_n)

  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$spiketrainpattern$pause$min_isi_sec <- 0.100
  params$pause$T_seed <- 0.100
  params$spiketrainpattern$high_frequency_spiking$tolerated_gap_isi_sec <- 0.120
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.120

  out <- run_detector_one_train(dat, params, min_isi_sec = 0.001, train = "hf_pause")
  auto <- as.character(out$pattern_auto)
  expect_equal(auto[pause_idx], "pause")
  expect_true(any(auto[seq_len(pause_idx - 1L)] == "high_frequency_spiking"))
  expect_true(any(auto[seq.int(pause_idx + 1L, length(auto))] == "high_frequency_spiking"))

  audit <- attr(out, "candidate_diagnostic_audit")
  expect_true(is.data.frame(audit))
  selected <- audit[as.logical(audit$selected_for_auto %||% FALSE), , drop = FALSE]
  selected_hfs <- selected[as.character(selected$final_label) == "high_frequency_spiking", , drop = FALSE]
  expect_false(any(
    as.integer(selected_hfs$start_isi) <= pause_idx &
      as.integer(selected_hfs$end_isi) >= pause_idx,
    na.rm = TRUE
  ))
})

test_that("HF-spiking pattern Max_ISI is a hard label ceiling", {
  left_n <- 90L
  gap_idx <- left_n + 2L
  dat <- make_hfspiking_pause_train(left_n = left_n, fast_isi = 0.010, pause_isi = 0.0478)

  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(min_sec = 0, max_sec = 0.040)
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.075

  out <- run_detector_one_train(dat, params, min_isi_sec = 0.001, train = "hf_gap")
  auto <- as.character(out$pattern_auto)
  expect_false(auto[gap_idx] == "high_frequency_spiking")
  expect_true(any(auto[seq_len(gap_idx - 1L)] == "high_frequency_spiking"))
  expect_true(any(auto[seq.int(gap_idx + 1L, length(auto))] == "high_frequency_spiking"))
})

test_that("HF-spiking pattern Max_ISI rejects over-limit candidate spans", {
  params <- default_params()
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(min_sec = 0, max_sec = 0.040)
  vals <- c(rep(0.010, 40L), 0.047985, rep(0.010, 40L))

  gate <- getFromNamespace("stpd_pattern_isi_gate_pass", "SpikeTrainPatternDetector")(
    vals,
    "high_frequency_spiking",
    params,
    min_isi_sec = 0.001
  )

  expect_false(gate$pass)
  expect_match(gate$reason, "hf_spiking_above_pattern_Max_ISI")
})
