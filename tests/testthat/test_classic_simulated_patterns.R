make_classic_sim_train <- function(isi_sec) {
  ts <- c(0, cumsum(isi_sec))
  data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, isi_sec),
    pattern_manual = rep("", length(ts)),
    pattern_manual_negative = rep("", length(ts)),
    stringsAsFactors = FALSE
  )
}

test_that("default AUTO detects classic slow tonic, burst, and relative pause structure", {
  isi <- c(
    rep(0.45, 6L),
    rep(0.04, 6L),
    rep(0.45, 8L),
    1.20,
    rep(0.45, 7L),
    rep(0.04, 6L),
    rep(0.45, 7L)
  )
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "classic_scale"
  )

  auto <- as.character(out$pattern_auto)
  expect_gte(sum(auto == "tonic", na.rm = TRUE), 20L)
  expect_gte(sum(auto == "burst", na.rm = TRUE), 10L)
  expect_equal(sum(auto == "pause", na.rm = TRUE), 1L)

  audit <- attr(out, "candidate_diagnostic_audit")
  expect_true(any(audit$final_label == "tonic" & audit$gate_status == "event_core_tonic_pass"))
  expect_true(any(audit$final_label == "pause" & audit$decision_path == "relative_long_isi_gap_layer"))
})

test_that("compact burst cores can be rescued when local flank contrast is imperfect", {
  isi <- c(rep(0.45, 5L), 0.07, 0.04, 0.04, 0.04, 0.07, rep(0.45, 8L))
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "structural_rescue"
  )
  audit <- attr(out, "candidate_diagnostic_audit")
  rescued <- audit[audit$gate_status == "event_grammar_structural_burst_rescue_pass", , drop = FALSE]

  expect_gt(nrow(rescued), 0L)
  expect_true(any(rescued$final_label == "burst"))
  expect_true(any(as.numeric(rescued$structural_compression_ratio) >= 3))
})

test_that("AUTO rescues classic low-ISI burst packets with weak seed-band entry", {
  isi <- c(rep(0.45, 6L), 0.073, 0.086, 0.045, 0.076, rep(0.45, 6L))
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "low_isi_packet"
  )
  audit <- attr(out, "candidate_diagnostic_audit")
  selected_burst <- audit[
    as.logical(audit$selected_for_auto %||% FALSE) &
      audit$final_label == "burst",
    ,
    drop = FALSE
  ]

  expect_true(any(selected_burst$start_isi <= 8L & selected_burst$end_isi >= 11L))
  expect_true(all(as.character(out$pattern_auto[8:11]) == "burst"))
})

test_that("AUTO keeps tonic tail ISIs out of pause when they match a regular tonic band", {
  tonic_block <- c(0.45, 0.48, 0.50, 0.47, 0.46, 0.49, 0.45, 0.48)
  isi <- c(tonic_block, 1.20, tonic_block)
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "tonic_tail_not_pause"
  )
  auto <- as.character(out$pattern_auto)

  expect_equal(sum(auto == "pause", na.rm = TRUE), 1L)
  expect_true(all(auto[c(4, 5, 7, 8, 13, 14, 16, 17)] == "tonic"))
  audit <- attr(out, "candidate_diagnostic_audit")
  pause <- audit[audit$final_label == "pause" & as.logical(audit$selected_for_auto %||% FALSE), , drop = FALSE]
  expect_true(all(as.numeric(pause$pause_effective_threshold_sec) > 0.55))
})

test_that("burst episode edge trimming does not block following tonic states", {
  isi <- c(rep(0.45, 5L), 0.050, 0.080, 0.060, 0.130, rep(0.45, 8L))
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "burst_edge_trim"
  )
  auto <- as.character(out$pattern_auto)

  expect_true(all(auto[7:9] == "burst"))
  expect_true(all(auto[11:18] == "tonic"))
  audit <- attr(out, "candidate_diagnostic_audit")
  selected_tonic <- audit[
    as.logical(audit$selected_for_auto %||% FALSE) &
      audit$candidate_layer == "event_core_tonic_state",
    ,
    drop = FALSE
  ]
  expect_true(any(selected_tonic$start_isi <= 11L & selected_tonic$end_isi >= 18L))
})

test_that("AUTO preserves stable tonic cores before short transition ISIs", {
  isi <- c(
    rep(0.45, 5L),
    rep(0.04, 3L),
    rep(0.45, 7L),
    0.12, 0.11, 0.09,
    rep(0.04, 3L)
  )
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "tonic_core_transition_trim"
  )
  auto <- as.character(out$pattern_auto)

  expect_true(all(auto[10:16] == "tonic"))
  expect_false(any(auto[17:19] == "tonic"))
  audit <- attr(out, "candidate_diagnostic_audit")
  selected_tonic <- audit[
    as.logical(audit$selected_for_auto %||% FALSE) &
      audit$candidate_layer == "event_core_tonic_state",
    ,
    drop = FALSE
  ]
  expect_true(any(
    selected_tonic$gate_status == "event_core_tonic_core_trim_pass" &
      selected_tonic$start_isi == 10L &
      selected_tonic$end_isi == 16L
  ))
})

test_that("dense burst episodes beat fragmented burst kernels in AUTO selection", {
  isi <- c(
    0.035, 0.036, 0.080, 0.047, 0.043, 0.060, 0.050, 0.036,
    0.090, 0.094, 0.092, 0.128, 0.104, 0.069, 0.064, 0.038,
    1.20, 0.80, 0.75, 0.45, 0.44, 0.42, 0.48, 0.46
  )
  out <- run_detector_one_train(
    make_classic_sim_train(isi),
    default_params(),
    min_isi_sec = 0.001,
    train = "dense_episode"
  )
  auto <- as.character(out$pattern_auto)
  audit <- attr(out, "candidate_diagnostic_audit")
  selected_episode <- audit[
    as.logical(audit$selected_for_auto %||% FALSE) &
      audit$candidate_layer == "event_grammar_burst_episode",
    ,
    drop = FALSE
  ]

  expect_true(any(selected_episode$final_label == "burst"))
  expect_gte(max(as.integer(selected_episode$n_isi), na.rm = TRUE), 10L)
  expect_gte(sum(auto[2:17] == "burst", na.rm = TRUE), 10L)
})
