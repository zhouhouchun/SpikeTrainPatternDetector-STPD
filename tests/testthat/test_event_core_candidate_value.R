test_that("event core candidate value has one formal entry and named historical implementations", {
  registry <- getFromNamespace("stpd_event_core_candidate_value_registry", "SpikeTrainPatternDetector")()
  expected <- c(
    "label_base",
    "priority_aware",
    "possible_burst_dynamic",
    "explicit_priority",
    "hf_protected"
  )

  expect_equal(names(registry), expected)
  expect_true(all(vapply(registry, is.function, logical(1))))
})

test_that("event core candidate value preserves HF-protected priority semantics", {
  value <- getFromNamespace("stpd_event_core_candidate_value", "SpikeTrainPatternDetector")
  row <- function(label, priority = NA_real_, score = 0, n_isi = 3) {
    data.frame(
      final_label = label,
      priority = priority,
      score = score,
      n_isi = n_isi,
      stringsAsFactors = FALSE
    )
  }

  possible_explicit <- value(row("possible_burst", priority = 760, score = 37.83, n_isi = 3))
  burst_explicit <- value(row("burst", priority = 1250, score = 20.83, n_isi = 3))
  hf_spiking <- value(row("high_frequency_spiking", score = 11, n_isi = 20))
  tonic <- value(row("tonic", score = 5, n_isi = 3))
  pause <- value(row("pause", score = 5, n_isi = 1))

  expect_equal(possible_explicit, 5203786)
  expect_equal(burst_explicit, 12502086)
  expect_equal(hf_spiking, 11301980)
  expect_gt(burst_explicit, hf_spiking)
  expect_gt(hf_spiking, possible_explicit)
  expect_gt(possible_explicit, tonic)
  expect_gt(possible_explicit, pause)
})

test_that("weighted interval selection uses the HF-protected candidate value", {
  select <- getFromNamespace("stpd_event_core_weighted_select", "SpikeTrainPatternDetector")

  overlapping_state <- data.frame(
    final_label = c("possible_burst", "high_frequency_spiking"),
    start_isi = c(2L, 2L),
    end_isi = c(8L, 8L),
    score = c(37.83, 11),
    priority = c(760, NA_real_),
    n_isi = c(3L, 20L),
    stringsAsFactors = FALSE
  )
  selected_state <- select(overlapping_state)
  expect_equal(as.character(selected_state$final_label[selected_state$selected_for_auto]), "high_frequency_spiking")

  overlapping_burst <- data.frame(
    final_label = c("possible_burst", "high_frequency_spiking", "burst"),
    start_isi = c(2L, 2L, 3L),
    end_isi = c(8L, 8L, 5L),
    score = c(37.83, 11, 20.83),
    priority = c(760, NA_real_, 1250),
    n_isi = c(3L, 20L, 3L),
    stringsAsFactors = FALSE
  )
  selected_burst <- select(overlapping_burst)
  expect_equal(as.character(selected_burst$final_label[selected_burst$selected_for_auto]), "burst")
})

test_that("compact burst kernels inside strong long HF spiking states are subordinate", {
  protect <- getFromNamespace("stpd_event_grammar_protect_hf_spiking_states", "SpikeTrainPatternDetector")
  select <- getFromNamespace("stpd_event_core_weighted_select", "SpikeTrainPatternDetector")

  audit <- data.frame(
    final_label = c("high_frequency_spiking", "burst", "burst", "pause"),
    class = c("high_frequency_spiking", "burst", "burst", "pause"),
    start_isi = c(10L, 40L, 160L, 95L),
    end_isi = c(280L, 42L, 164L, 96L),
    n_isi = c(271L, 3L, 5L, 2L),
    score = c(52, 4.8, 5.1, 4),
    priority = c(1040, 1250, 1250, 320),
    duration_sec = c(3.2, 0.012, 0.025, 0.09),
    hf_spiking_short_fraction = c(0.92, NA, NA, NA),
    hf_spiking_bridge_fraction = c(0.98, NA, NA, NA),
    hf_spiking_q90_sec = c(0.023, NA, NA, NA),
    hf_spiking_q90_max_sec = c(0.045, NA, NA, NA),
    decision_path = "",
    selection_status = "",
    action = "accept",
    stringsAsFactors = FALSE
  )

  protected <- protect(audit)
  expect_equal(as.character(protected$final_label[2:4]), c("reject", "reject", "reject"))
  expect_true(all(protected$suppressed_by_hf_spiking_state[2:4]))
  expect_match(
    protected$decision_path[2],
    "compact_burst_kernel_suppressed_inside_long_hf_spiking_state",
    fixed = TRUE
  )

  selected <- select(protected)
  expect_equal(
    as.character(selected$final_label[selected$selected_for_auto]),
    "high_frequency_spiking"
  )
})

test_that("burst-dominated HF spiking candidates do not suppress canonical bursts", {
  protect <- getFromNamespace("stpd_event_grammar_protect_hf_spiking_states", "SpikeTrainPatternDetector")
  select <- getFromNamespace("stpd_event_core_weighted_select", "SpikeTrainPatternDetector")

  burst_starts <- c(18L, 28L, 38L, 48L, 58L, 68L, 78L, 88L)
  burst_ends <- burst_starts + 3L
  audit <- data.frame(
    final_label = c("high_frequency_spiking", rep("burst", length(burst_starts)), "possible_burst"),
    class = c("high_frequency_spiking", rep("burst", length(burst_starts)), "possible_burst"),
    start_isi = c(10L, burst_starts, 42L),
    end_isi = c(100L, burst_ends, 44L),
    n_isi = c(91L, rep(4L, length(burst_starts)), 3L),
    score = c(55, rep(8, length(burst_starts)), 6),
    priority = c(1040, rep(1250, length(burst_starts)), 700),
    duration_sec = c(1.8, rep(0.020, length(burst_starts)), 0.015),
    hf_spiking_short_fraction = c(0.90, rep(NA, length(burst_starts) + 1L)),
    hf_spiking_bridge_fraction = c(1.0, rep(NA, length(burst_starts) + 1L)),
    hf_spiking_q90_sec = c(0.040, rep(NA, length(burst_starts) + 1L)),
    hf_spiking_q90_max_sec = c(0.056, rep(NA, length(burst_starts) + 1L)),
    decision_path = "",
    selection_status = "",
    action = "accept",
    stringsAsFactors = FALSE
  )

  protected <- protect(audit)
  expect_equal(as.character(protected$final_label[1]), "reject")
  expect_true(isTRUE(protected$hf_spiking_burst_dominated[1]))
  expect_gte(protected$hf_spiking_embedded_burst_group_count[1], 6)
  expect_match(
    protected$decision_path[1],
    "reject_burst_dominated_hf_spiking_state",
    fixed = TRUE
  )
  expect_equal(as.character(protected$final_label[2:9]), rep("burst", 8))
  expect_false(any(protected$suppressed_by_hf_spiking_state[2:9]))

  selected <- select(protected)
  selected_labels <- as.character(selected$final_label[selected$selected_for_auto])
  expect_false("high_frequency_spiking" %in% selected_labels)
  expect_true(any(selected_labels == "burst"))
})

test_that("burst-packet-like HF spiking candidates are rejected before they suppress bursts", {
  protect <- getFromNamespace("stpd_event_grammar_protect_hf_spiking_states", "SpikeTrainPatternDetector")

  audit <- data.frame(
    final_label = c("high_frequency_spiking", "long_burst", "long_burst"),
    class = c("high_frequency_spiking", "long_burst", "long_burst"),
    start_isi = c(10L, 32L, 72L),
    end_isi = c(120L, 44L, 86L),
    n_isi = c(111L, 13L, 15L),
    score = c(54, 9.2, 9.5),
    priority = c(1040, 1160, 1160),
    duration_sec = c(1.5, 0.11, 0.12),
    CV = c(0.90, 0.30, 0.32),
    LV = c(0.55, 0.20, 0.22),
    MM = c(5.8, 1.7, 1.8),
    hf_spiking_short_fraction = c(0.86, NA, NA),
    hf_spiking_bridge_fraction = c(0.94, NA, NA),
    hf_spiking_large_fraction = c(0.11, NA, NA),
    hf_spiking_q90_sec = c(0.038, NA, NA),
    hf_spiking_q90_max_sec = c(0.045, NA, NA),
    decision_path = "",
    selection_status = "",
    action = "accept",
    stringsAsFactors = FALSE
  )

  protected <- protect(audit)
  expect_equal(as.character(protected$final_label[1]), "reject")
  expect_true(isTRUE(protected$hf_spiking_burst_packet_like[1]))
  expect_match(
    protected$decision_path[1],
    "reject_burst_packet_like_hf_spiking_state",
    fixed = TRUE
  )
  expect_equal(as.character(protected$final_label[2:3]), c("long_burst", "long_burst"))
})

test_that("compact pure HF spiking states suppress only tiny embedded burst kernels", {
  protect <- getFromNamespace("stpd_event_grammar_protect_hf_spiking_states", "SpikeTrainPatternDetector")
  select <- getFromNamespace("stpd_event_core_weighted_select", "SpikeTrainPatternDetector")

  audit <- data.frame(
    final_label = c("high_frequency_spiking", "burst", "possible_burst"),
    class = c("high_frequency_spiking", "burst", "possible_burst"),
    start_isi = c(33L, 35L, 50L),
    end_isi = c(69L, 36L, 69L),
    n_isi = c(37L, 2L, 20L),
    score = c(32, 8, 6),
    priority = c(1040, 1250, 120),
    duration_sec = c(0.30, 0.008, 0.14),
    hf_spiking_short_fraction = c(0.94, NA, NA),
    hf_spiking_bridge_fraction = c(1.0, NA, NA),
    hf_spiking_q90_sec = c(0.025, NA, NA),
    hf_spiking_q90_max_sec = c(0.034, NA, NA),
    decision_path = "",
    selection_status = "",
    action = "accept",
    stringsAsFactors = FALSE
  )

  protected <- protect(audit)
  expect_equal(as.character(protected$final_label[1]), "high_frequency_spiking")
  expect_equal(as.character(protected$final_label[2:3]), c("reject", "reject"))
  selected <- select(protected)
  expect_equal(
    as.character(selected$final_label[selected$selected_for_auto]),
    "high_frequency_spiking"
  )
})

test_that("manual burst-family labels split HF spiking states instead of bridging through them", {
  detect <- getFromNamespace("stpd_detect_train_product_hardened", "SpikeTrainPatternDetector")
  params <- SpikeTrainPatternDetector::default_params()

  isi <- rep(0.010, 120)
  ts <- cumsum(c(0, isi))
  n <- length(ts)
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, diff(ts)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
  dat$pattern_manual[55:66] <- "burst"

  out <- detect(dat, params, min_isi_sec = 0.0009, train = "manual_split", lock_manual = TRUE)
  audit <- attr(out, "candidate_diagnostic_audit")
  expect_true(all(as.character(out$pattern_auto[55:66]) == ""))

  hfs <- audit[as.character(audit$final_label) == "high_frequency_spiking", , drop = FALSE]
  expect_gt(nrow(hfs), 0)
  starts <- suppressWarnings(as.integer(hfs$start_isi))
  ends <- suppressWarnings(as.integer(hfs$end_isi))
  expect_false(any(starts < 55L & ends > 66L, na.rm = TRUE))
})

test_that("HF spiking support can bridge a transparent sub-threshold artifact gap", {
  detect <- getFromNamespace("stpd_detect_train_product_hardened", "SpikeTrainPatternDetector")
  params <- SpikeTrainPatternDetector::default_params()

  isi <- c(rep(0.008, 16), 0.0008, rep(0.008, 32))
  ts <- cumsum(c(0, isi))
  n <- length(ts)
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, diff(ts)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )

  out <- detect(dat, params, min_isi_sec = 0.001, train = "artifact_bridge", lock_manual = TRUE)
  audit <- attr(out, "candidate_diagnostic_audit")
  hfs <- audit[as.character(audit$final_label) == "high_frequency_spiking", , drop = FALSE]
  selected <- as.logical(hfs$selected_for_auto)
  selected[is.na(selected)] <- FALSE
  hfs <- hfs[selected, , drop = FALSE]
  expect_gt(nrow(hfs), 0)
  expect_lte(min(as.integer(hfs$start_isi), na.rm = TRUE), 2L)
  expect_gte(max(as.integer(hfs$end_isi), na.rm = TRUE), n - 1L)
  expect_true(any(as.character(out$pattern_auto[2:17]) == "high_frequency_spiking"))
  expect_true(any(as.character(out$pattern_auto[19:n]) == "high_frequency_spiking"))
})
