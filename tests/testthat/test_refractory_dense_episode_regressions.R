make_event_grammar_regression_train <- function(isi) {
  timestamps <- cumsum(c(0, isi))
  data.frame(
    idx = seq_along(timestamps),
    timestamp_sec = timestamps,
    ISI_sec = c(NA_real_, diff(timestamps)),
    pattern_manual = rep("", length(timestamps)),
    pattern_manual_negative = rep("", length(timestamps)),
    pattern_auto = rep("", length(timestamps)),
    stringsAsFactors = FALSE
  )
}

test_that("event-grammar spans count refractory-suspect ISIs and demote burst output", {
  params <- default_params()
  dat <- make_event_grammar_regression_train(c(0.1, 0.1, 0.005, 0.005, 0.00095, 0.1, 0.15))
  vp <- getFromNamespace("stpd_event_core_params_impl", "SpikeTrainPatternDetector")(
    dat,
    params,
    min_isi_sec = 0.0009
  )
  metrics <- getFromNamespace("stpd_event_core_span_metrics", "SpikeTrainPatternDetector")(
    dat,
    4L,
    6L,
    params,
    vp,
    min_isi_sec = 0.0009,
    train = "suspect_burst"
  )

  expect_equal(metrics$refractory_suspect_threshold_sec, 0.001)
  expect_equal(metrics$refractory_suspect_n, 1L)
  expect_equal(metrics$refractory_suspect_fraction, 1 / 3)
  expect_equal(metrics$refractory_suspect_min_ISI_sec, 0.00095, tolerance = 1e-12)

  make_dataset <- getFromNamespace("make_dataset", "SpikeTrainPatternDetector")
  ds <- make_dataset(
    name = "suspect_burst",
    source = "regression_test",
    trains = list(train_1 = dat),
    unit_in = "s"
  )
  out <- stpd_detect(ds, params, selected_trains = "train_1")

  expect_equal(as.character(out$trains$train_1$pattern_auto[4:6]), rep("possible_burst", 3L))

  ledger <- as.data.frame(out$results$candidate_ledger)
  suspect_rows <- ledger[
    is.finite(suppressWarnings(as.numeric(ledger$refractory_suspect_n))) &
      suppressWarnings(as.numeric(ledger$refractory_suspect_n)) > 0,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(suspect_rows), 0L)
  expect_true(all(suspect_rows$final_candidate_class == "possible_burst"))
  expect_true(all(suspect_rows$refractory_suspect_n == 1L))

  final <- as.data.frame(out$results$final_decisions)
  final_suspect <- final[
    is.finite(suppressWarnings(as.numeric(final$refractory_suspect_n))) &
      suppressWarnings(as.numeric(final$refractory_suspect_n)) > 0,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(final_suspect), 0L)
  expect_true(all(final_suspect$final_class == "possible_burst"))
  expect_false(any(is.na(final_suspect$refractory_suspect_n)))
})

test_that("dense burst episodes use the public spike-count subtype boundaries", {
  params <- default_params()
  detect_dense <- getFromNamespace(
    "stpd_event_grammar_detect_burst_events_final_impl",
    "SpikeTrainPatternDetector"
  )

  detected_label <- function(n_spikes) {
    dat <- make_event_grammar_regression_train(c(
      rep(0.1, 20L),
      rep(0.012, n_spikes - 1L),
      rep(0.1, 20L)
    ))
    vp <- getFromNamespace("stpd_event_core_params_impl", "SpikeTrainPatternDetector")(
      dat,
      params,
      min_isi_sec = 0.0009
    )
    candidates <- detect_dense(
      dat,
      params,
      vp,
      min_isi_sec = 0.0009,
      train = paste0("dense_", n_spikes)
    )
    dense <- candidates[
      as.character(candidates$candidate_layer) == "event_grammar_burst_episode" &
        suppressWarnings(as.integer(candidates$n_spikes)) == n_spikes,
      ,
      drop = FALSE
    ]
    expect_equal(nrow(dense), 1L)
    as.character(dense$final_label[1])
  }

  expect_equal(detected_label(10L), "burst")
  expect_equal(detected_label(11L), "long_burst")
  expect_equal(detected_label(15L), "long_burst")
  expect_equal(detected_label(16L), "possible_burst")
  expect_equal(detected_label(20L), "possible_burst")
})

test_that("accepted dense burst subtypes retain equal arbitration evidence", {
  value_fun <- getFromNamespace(
    "stpd_event_core_candidate_value",
    "SpikeTrainPatternDetector"
  )
  candidate <- data.frame(
    final_label = "burst",
    candidate_layer = "event_grammar_burst_episode",
    priority = 1035,
    score = 6,
    n_isi = 11,
    stringsAsFactors = FALSE
  )
  burst_value <- value_fun(candidate)
  candidate$final_label <- "long_burst"
  long_value <- value_fun(candidate)

  expect_equal(long_value, burst_value)
})

test_that("stable HF-tonic state evidence outranks overlapping dense rescue", {
  value_fun <- getFromNamespace(
    "stpd_event_core_candidate_value",
    "SpikeTrainPatternDetector"
  )
  dense <- data.frame(
    final_label = "long_burst", candidate_layer = "event_grammar_burst_episode",
    priority = 1035, score = 6, n_isi = 11,
    stringsAsFactors = FALSE
  )
  hft <- data.frame(
    final_label = "high_frequency_tonic", candidate_layer = "event_core_hf_tonic_state",
    priority = 500, score = 6, n_isi = 11,
    stringsAsFactors = FALSE
  )

  expect_gt(value_fun(hft), value_fun(dense))
})

test_that("hard burst thresholds obey the same prolonged-review boundary", {
  params <- default_params()
  dat <- make_event_grammar_regression_train(c(rep(0.1, 4L), rep(0.012, 16L), rep(0.1, 4L)))
  hard <- list(train_a = list(
    burst_max_sec = 0.02,
    threshold_mode = "hard_threshold",
    hard_threshold = TRUE,
    source = "reviewer_regression_test"
  ))
  params <- merge_train_isi_thresholds_into_params(params, hard)
  params <- stpd_attach_thresholds_to_params(
    params, list(trains = list(train_a = dat)), min_isi_sec = 0.0009
  )
  vp <- stpd_event_grammar_params(dat, params, min_isi_sec = 0.0009, train = "train_a")
  candidates <- getFromNamespace(
    "stpd_event_core_detect_hard_isi_thresholds",
    "SpikeTrainPatternDetector"
  )(dat, params, vp, min_isi_sec = 0.0009, train = "train_a")
  prolonged <- candidates[
    candidates$candidate_layer == "isi_profile_hard_threshold_burst" &
      suppressWarnings(as.integer(candidates$n_spikes)) >= 16L,
    , drop = FALSE
  ]

  expect_gt(nrow(prolonged), 0L)
  expect_true(all(prolonged$final_label == "possible_burst"))
  expect_true(all(prolonged$action == "demote_to_possible"))
  expect_true(all(prolonged$priority == 120))
})

test_that("all advertised refractory-suspect actions have distinct auditable semantics", {
  dat <- make_event_grammar_regression_train(c(
    0.1, 0.1, 0.005, 0.005, 0.00095, 0.005, 0.005, 0.1
  ))
  base_params <- default_params()
  vp <- getFromNamespace("stpd_event_core_params_impl", "SpikeTrainPatternDetector")(
    dat, base_params, min_isi_sec = 0.0009
  )
  metrics <- getFromNamespace("stpd_event_core_span_metrics", "SpikeTrainPatternDetector")(
    dat, 4L, 8L, base_params, vp,
    min_isi_sec = 0.0009, train = "refractory_actions"
  )
  candidate <- metrics
  candidate$candidate_id <- "refractory_parent"
  candidate$candidate_layer <- "event_core_burst_event"
  candidate$candidate_diagnostic_class <- "burst__strict"
  candidate$class <- "burst"
  candidate$final_label <- "burst"
  candidate$gate_status <- "strict_pass"
  candidate$decision_path <- "strict_pass"
  candidate$action <- "accept"
  candidate$score <- 5
  candidate$priority <- 1000
  candidate$selected_for_auto <- FALSE
  candidate$selection_status <- "not_selected"
  apply_policy <- getFromNamespace(
    "stpd_event_core_apply_refractory_suspect_policy",
    "SpikeTrainPatternDetector"
  )
  run_action <- function(action) {
    params <- base_params
    params$detector$refractory_suspect_action <- action
    params$burst$refractory_suspect_action <- action
    apply_policy(
      candidate, params, dat = dat, vp = vp, min_isi_sec = 0.0009
    )
  }

  warned <- run_action("warn_only")
  demoted <- run_action("demote_to_possible")
  marked <- run_action("mark_multiunit_contamination")
  excluded <- run_action("exclude_candidate")
  split <- run_action("split_at_suspect")
  reevaluated <- run_action("exclude_suspect_isi_and_reevaluate")

  expect_identical(warned$final_label, "burst")
  expect_identical(warned$refractory_suspect_action, "warn_only")
  expect_identical(demoted$final_label, "possible_burst")
  expect_identical(demoted$refractory_suspect_action, "demoted_to_possible_burst")
  expect_identical(marked$final_label, "possible_burst")
  expect_identical(marked$refractory_suspect_action, "marked_possible_multiunit_contamination")
  expect_identical(excluded$final_label, "reject")
  expect_identical(excluded$action, "reject")
  expect_identical(excluded$refractory_suspect_action, "excluded_entire_candidate")

  for (fragmented in list(split, reevaluated)) {
    expect_true(any(fragmented$final_label == "reject"))
    fragments <- fragmented[fragmented$final_label == "possible_burst", , drop = FALSE]
    expect_equal(nrow(fragments), 2L)
    expect_true(all(fragments$refractory_suspect_n == 0L))
    expect_true(all(fragments$refractory_suspect_policy_applied))
  }

  # Reapplying a policy must not rewrite the action audit or split a parent twice.
  params <- base_params
  params$detector$refractory_suspect_action <- "split_at_suspect"
  reapplied <- apply_policy(split, params, dat = dat, vp = vp, min_isi_sec = 0.0009)
  expect_identical(
    reapplied[, c("start_isi", "end_isi", "final_label", "refractory_suspect_action")],
    split[, c("start_isi", "end_isi", "final_label", "refractory_suspect_action")]
  )

  # Compatibility/legacy pipelines must delegate to the same auditable policy.
  legacy_policy <- getFromNamespace(
    "apply_refractory_suspect_policy_burst_candidates",
    "SpikeTrainPatternDetector"
  )
  legacy_params <- base_params$burst
  legacy_params$refractory_suspect_sec <- 0.0010
  legacy_params$refractory_suspect_action <- "exclude_candidate"
  legacy_excluded <- legacy_policy(candidate, dat, legacy_params, min_isi_sec = 0.0009)
  expect_equal(nrow(legacy_excluded), 1L)
  expect_identical(legacy_excluded$final_label, "reject")
  expect_identical(legacy_excluded$refractory_suspect_action, "excluded_entire_candidate")
  expect_true(legacy_excluded$refractory_suspect_policy_applied)

  legacy_params$refractory_suspect_action <- "split_at_suspect"
  legacy_split <- legacy_policy(candidate, dat, legacy_params, min_isi_sec = 0.0009)
  expect_true(any(legacy_split$final_label == "reject"))
  expect_equal(sum(legacy_split$final_label == "possible_burst"), 2L)
  legacy_split_again <- legacy_policy(legacy_split, dat, legacy_params, min_isi_sec = 0.0009)
  expect_identical(
    legacy_split_again[, c("start_isi", "end_isi", "final_label", "refractory_suspect_action")],
    legacy_split[, c("start_isi", "end_isi", "final_label", "refractory_suspect_action")]
  )
})
