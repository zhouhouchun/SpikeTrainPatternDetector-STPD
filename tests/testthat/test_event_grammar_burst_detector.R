test_that("event grammar burst detector registry exposes semantic implementations", {
  registry <- getFromNamespace("stpd_event_grammar_burst_detector_registry", "SpikeTrainPatternDetector")()
  default_pipeline <- getFromNamespace("stpd_event_grammar_burst_detector_default", "SpikeTrainPatternDetector")

  expected <- c(
    "threshold_resolved_base",
    "threshold_resolved_optimized",
    "consistency_optimized",
    "final"
  )

  expect_equal(names(registry), expected)
  expect_true(all(vapply(registry, is.function, logical(1))))
  expect_equal(default_pipeline(default_params()), "final")

  params <- default_params()
  params$event_grammar$burst_detector_pipeline <- "threshold_resolved_base"
  expect_equal(default_pipeline(params), "threshold_resolved_base")

  params$event_grammar$burst_detector_pipeline <- "not_a_burst_pipeline"
  expect_equal(default_pipeline(params), "final")
})

test_that("event grammar burst detector adds structure-first evidence while preserving the golden threshold candidate", {
  detector <- getFromNamespace("stpd_event_grammar_detect_burst_events", "SpikeTrainPatternDetector")
  dispatch <- getFromNamespace("stpd_event_grammar_detect_burst_events_dispatch", "SpikeTrainPatternDetector")
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  dat <- ds$trains$train_1
  min_isi <- params$detector$min_valid_isi_sec
  vp <- stpd_event_grammar_params(dat, params, min_isi_sec = min_isi)

  burst <- detector(dat, params, vp, min_isi_sec = min_isi, train = "train_1")
  explicit <- dispatch(dat, params, vp, min_isi_sec = min_isi, train = "train_1", pipeline = "final")

  structure <- burst[burst$candidate_layer == "structure_first_burst_screen", , drop = FALSE]
  legacy <- burst[burst$candidate_id == "event_grammar_burst_opt2_1", , drop = FALSE]
  expect_equal(nrow(structure), 1L)
  expect_equal(nrow(legacy), 1L)
  expect_equal(as.character(structure$final_label), "burst")
  expect_equal(as.integer(structure$start_isi), 4L)
  expect_equal(as.integer(structure$end_isi), 6L)
  expect_equal(as.character(structure$boundary_type), "two_sided")
  expect_true(structure$structure_first_no_seed_band_gate)
  expect_match(structure$decision_path, "source=local_flank_contrast", fixed = TRUE)
  expect_equal(as.character(legacy$final_label), "burst")
  expect_equal(as.integer(legacy$start_isi), 4L)
  expect_equal(as.integer(legacy$end_isi), 6L)
  expect_equal(as.character(legacy$boundary_type), "two_sided")
  expect_equal(as.numeric(legacy$priority), 1250)
  expect_equal(as.numeric(legacy$score), 20.83, tolerance = 1e-8)
  expect_equal(
    burst[, intersect(names(burst), c("candidate_id", "final_label", "start_isi", "end_isi", "score", "priority"))],
    explicit[, intersect(names(explicit), c("candidate_id", "final_label", "start_isi", "end_isi", "score", "priority"))]
  )

  out <- run_detector_one_train(dat, params, min_isi_sec = min_isi, train = "train_1")
  expect_equal(
    as.character(out$pattern_auto),
    c("", "", "", "burst", "burst", "burst", "pause", "")
  )
})

test_that("short edge-limited candidates remain Review-only in both routes", {
  detector <- getFromNamespace("stpd_event_grammar_detect_burst_events", "SpikeTrainPatternDetector")
  ds <- stpd_golden_test_dataset("boundary_start")
  params <- default_params()
  dat <- ds$trains$train_1
  min_isi <- params$detector$min_valid_isi_sec
  vp <- stpd_event_grammar_params(dat, params, min_isi_sec = min_isi)

  burst <- detector(dat, params, vp, min_isi_sec = min_isi, train = "train_1")
  structure <- burst[burst$candidate_layer == "structure_first_burst_screen", , drop = FALSE]
  legacy <- burst[burst$candidate_layer == "event_grammar_burst_event", , drop = FALSE]
  expect_equal(nrow(structure), 1L)
  expect_equal(nrow(legacy), 1L)
  expect_equal(as.character(structure$final_label), "possible_burst")
  expect_equal(as.integer(structure$start_isi), 2L)
  expect_equal(as.integer(structure$end_isi), 4L)
  expect_equal(as.character(structure$boundary_type), "train_start_endpoint")
  expect_equal(as.character(legacy$final_label), "possible_burst")
  expect_equal(as.integer(legacy$start_isi), 2L)
  expect_equal(as.integer(legacy$end_isi), 4L)
  expect_equal(as.character(legacy$boundary_type), "clean_one_sided_or_edge_limited")
  expect_equal(as.numeric(legacy$priority), 760)
  expect_equal(
    as.character(legacy$decision_path),
    "clean_one_sided_flank_contrast_pass_core_compact"
  )
  expect_false(legacy$threshold_elastic_acceptance)
  expect_false(legacy$threshold_elastic_support_pass)
  expect_identical(legacy$contrast_evidence_status, "clean_edge_limited")

  out <- run_detector_one_train(dat, params, min_isi_sec = min_isi, train = "train_1")
  expect_equal(
    as.character(out$pattern_auto),
    c("", "possible_burst", "possible_burst", "possible_burst", "pause", "", "")
  )
})

test_that("event grammar burst detector dispatch rejects unknown names", {
  dispatch <- getFromNamespace("stpd_event_grammar_detect_burst_events_dispatch", "SpikeTrainPatternDetector")
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  dat <- ds$trains$train_1
  min_isi <- params$detector$min_valid_isi_sec
  vp <- stpd_event_grammar_params(dat, params, min_isi_sec = min_isi)

  expect_error(
    dispatch(dat, params, vp, min_isi_sec = min_isi, train = "train_1", pipeline = "not_a_burst_pipeline"),
    "Unknown event grammar burst detector pipeline"
  )
})
