publication_helper_env <- new.env(parent = globalenv())
publication_helper_path <- file.path(
  "..", "..", "evaluation", "publication_validation",
  "publication_validation_helpers.R"
)
if (!file.exists(publication_helper_path)) {
  publication_helper_path <- file.path(
    getwd(), "evaluation", "publication_validation",
    "publication_validation_helpers.R"
  )
}
publication_helper_available <- file.exists(publication_helper_path)

if (!publication_helper_available) {
  test_that("repository-only publication helper is not packaged", {
    skip("Publication validation helpers are intentionally repository-only.")
  })
} else {
  sys.source(
    publication_helper_path,
    envir = publication_helper_env
  )

test_that("canonical publication validation helper is available", {
  expect_true(file.exists(publication_helper_path))
  expect_true(exists(
    "stpd_pub_balanced_sample",
    envir = publication_helper_env,
    inherits = FALSE
  ))
})

test_that("balanced calibration sampling handles singleton groups by position", {
  patterns <- c(
    "burst", "high_frequency_spiking", "tonic", "pause", "other"
  )
  episodes <- do.call(rbind, lapply(patterns, function(pattern) {
    data.frame(
      Pattern = rep(pattern, 10L),
      Group_ID = c(rep(paste0(pattern, "_multi"), 9L),
                   paste0(pattern, "_singleton")),
      Episode = paste0(pattern, "_", seq_len(10L)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(episodes) <- NULL

  for (seed in seq_len(25L)) {
    sampled <- publication_helper_env$stpd_pub_balanced_sample(
      episodes, patterns = patterns, n_each = 10L, seed = seed
    )
    expect_equal(nrow(sampled), 50L, info = paste("seed", seed))
    expect_true(all(table(sampled$Pattern) == 10L),
                info = paste("seed", seed))
    expect_equal(length(unique(sampled$Episode)), 50L,
                 info = paste("seed", seed))
    expect_true(setequal(sampled$Episode, episodes$Episode),
                info = paste("seed", seed))
  }
})

test_that("review-only Tonic does not perturb other calibration samples", {
  patterns <- c(
    "burst", "pause", "tonic", "high_frequency_spiking", "other"
  )
  episodes <- do.call(rbind, lapply(patterns, function(pattern) {
    data.frame(
      Train_ID = paste0(pattern, "_train_", seq_len(12L)),
      Group_ID = paste0(pattern, "_group_", rep(seq_len(4L), each = 3L)),
      Axis = if (pattern %in% c("tonic", "high_frequency_spiking")) {
        "state"
      } else if (pattern == "other") {
        "other"
      } else {
        "event"
      },
      Pattern = pattern,
      Episode = paste0(pattern, "_", seq_len(12L)),
      stringsAsFactors = FALSE
    )
  }))
  formal <- publication_helper_env$stpd_pub_balanced_sample(
    episodes, patterns = patterns, n_each = 10L, seed = 991L
  )
  review_only <- publication_helper_env$stpd_pub_balanced_sample(
    episodes, patterns = setdiff(patterns, "tonic"), n_each = 10L,
    seed = 991L
  )

  for (pattern in setdiff(patterns, "tonic")) {
    expect_identical(
      formal$Episode[formal$Pattern == pattern],
      review_only$Episode[review_only$Pattern == pattern],
      info = pattern
    )
  }
})

test_that("real Tonic references partition into a descriptive review axis", {
  intervals <- data.frame(
    Train_ID = rep("train_1", 7L), Group_ID = rep("group_1", 7L),
    Right_Spike_Index = c(2:6, 4L, 7L),
    ISI_s = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.04, 0.08),
    Axis = c(rep("state", 5L), "event", "other"),
    Pattern = c("tonic", "tonic", "high_frequency_spiking", "tonic",
                "tonic", "burst", "other"),
    stringsAsFactors = FALSE
  )

  partition <- publication_helper_env$stpd_pub_partition_tonic_reference(
    intervals, tonic_reference_role = "tonic_like_review"
  )
  expect_false(any(
    partition$formal_intervals$Axis == "state" &
      partition$formal_intervals$Pattern == "tonic"
  ))
  expect_identical(
    partition$review_intervals$Right_Spike_Index, c(2L, 3L, 5L, 6L)
  )
  expect_true(all(partition$review_intervals$Axis == "tonic_like_review"))
  expect_true(all(partition$review_intervals$Pattern == "tonic_like_review"))
  expect_true(all(partition$review_intervals$Source_Pattern == "tonic"))

  audit <- publication_helper_env$stpd_pub_tonic_like_review_audit(
    partition$review_intervals
  )
  expect_identical(audit$episodes$n_isi, c(2L, 2L))
  expect_identical(audit$episodes$n_spikes, c(3L, 3L))
  expect_equal(audit$episodes$duration_sec, c(0.05, 0.11))
  expect_false(any(c("tp", "fp", "fn", "precision", "recall", "F1") %in%
                     names(audit$episodes)))

  truth <- publication_helper_env$stpd_pub_truth_axes(
    partition$formal_intervals, "train_1",
    state_review_exclusions = partition$review_intervals
  )
  expect_false(any(truth$Truth == "tonic"))
  expect_false(any(
    truth$Axis %in% c("state_strict", "state_hf_family", "coexistence") &
      truth$Right_Spike_Index %in% c(2L, 3L, 5L, 6L)
  ))
  tonic_like_event <- truth[
    truth$Axis == "event" &
      truth$Right_Spike_Index %in% c(2L, 3L, 5L, 6L),
    , drop = FALSE
  ]
  expect_identical(
    sort(tonic_like_event$Right_Spike_Index), c(2L, 3L, 5L, 6L)
  )
  expect_true(all(tonic_like_event$Truth == "other"))
  expect_true(any(
    truth$Axis == "event" & truth$Right_Spike_Index == 4L &
      truth$Truth == "burst"
  ))
})

test_that("formal Tonic references remain State endpoints", {
  intervals <- data.frame(
    Train_ID = rep("train_1", 4L), Group_ID = rep("group_1", 4L),
    Right_Spike_Index = 2:5, ISI_s = rep(0.04, 4L),
    Axis = c("state", "state", "event", "other"),
    Pattern = c("tonic", "tonic", "burst", "other"),
    stringsAsFactors = FALSE
  )

  partition <- publication_helper_env$stpd_pub_partition_tonic_reference(
    intervals, tonic_reference_role = "formal_state"
  )
  expect_equal(nrow(partition$review_intervals), 0L)
  expect_equal(sum(
    partition$formal_intervals$Axis == "state" &
      partition$formal_intervals$Pattern == "tonic"
  ), 2L)

  truth <- publication_helper_env$stpd_pub_truth_axes(
    partition$formal_intervals, "train_1",
    state_review_exclusions = partition$review_intervals
  )
  expect_equal(sum(truth$Axis == "state_strict" & truth$Truth == "tonic"), 2L)
})

test_that("Pause calibration separates canonical entry from contextual lower tail", {
  pause_values <- c(0.012, 0.020, 0.045, 0.050, 0.060, 0.070, 0.080, 0.090)
  out <- publication_helper_env$stpd_pub_resolve_hfs_pause_bands(
    hfs_values = c(0.010, 0.012, 0.014, 0.016, 0.018, 0.020),
    pause_values = pause_values,
    min_valid_isi_sec = 0.001
  )

  expect_equal(
    unname(out$pause_band[["lower"]]),
    unname(stats::quantile(pause_values, 0.25, type = 7))
  )
  expect_lt(
    out$contextual_pause_observed_lower,
    out$pause_band[["lower"]]
  )
})

test_that("review-only Tonic calibration is fail-closed and provenance-explicit", {
  learn_body <- paste(
    deparse(body(publication_helper_env$stpd_pub_learn_params)),
    collapse = "\n"
  )
  split_body <- paste(
    deparse(body(publication_helper_env$stpd_pub_run_split)),
    collapse = "\n"
  )
  expect_true("tonic_reference_role" %in%
                names(formals(publication_helper_env$stpd_pub_learn_params)))
  expect_true("tonic_reference_role" %in%
                names(formals(publication_helper_env$stpd_pub_run_split)))
  expect_match(learn_body, "predeclared_default_no_real_tonic_reference",
               fixed = TRUE)
  expect_match(split_body, "tonic_like_review", fixed = TRUE)
  expect_match(split_body, 'label != "tonic"', fixed = TRUE)
})

make_frozen_real_result_fixture <- function() {
  path <- tempfile("stpd_real_bundle_")
  dir.create(path, recursive = TRUE)
  write_table <- function(file, value) utils::write.csv(
    value, file.path(path, file), row.names = FALSE
  )
  write_table("pooled_observed_metrics.csv", data.frame(
    level = "event",
    axis = c("event", "event", "state_hf_family"),
    label = c("burst", "pause", "high_frequency_spiking"),
    F1 = c(0.8, 0.7, 0.6)
  ))
  write_table("group_cluster_bootstrap_95ci.csv", data.frame(
    level = "event", axis = "event", label = "burst", metric = "F1",
    observed = 0.8, ci_low = 0.7, ci_high = 0.9
  ))
  saveRDS(data.frame(label = "burst"),
          file.path(path, "group_cluster_bootstrap_draws.rds"))
  write_table("runtime_and_candidate_status.csv", data.frame(
    repeat_id = 1L, label_blind = TRUE,
    tonic_reference_role = "tonic_like_review"
  ))
  write_table("sampled_10_per_pattern_by_fold.csv", data.frame(
    Pattern = c("burst", "pause", "high_frequency_spiking", "other"),
    fold_id = 1L
  ))
  write_table("learned_parameters_by_fold.csv", data.frame(
    Parameter = "tonic.min_isi_sec", Value = 0.01,
    Source = "predeclared_default_no_real_tonic_reference", fold_id = 1L
  ))
  confirmatory <- data.frame(
    pattern = c("broad_hfs", "burst", "pause"), truth_n = c(2L, 3L, 4L)
  )
  write_table("confirmatory_event_iou_metrics.csv", confirmatory)
  write_table("confirmatory_isi_support_metrics.csv", data.frame(
    pattern = c("broad_hfs", "burst", "pause"), truth_isi_n = 1:3
  ))
  write_table("explicit_manual_positive_recall_sensitivity.csv", data.frame(
    axis = "event", label = "burst", explicit_positive_n = 1L,
    detected_n = 1L, recall = 1
  ))
  write_table("reference_eligibility_by_train.csv", data.frame(
    train_id = "train_1", reference_eligible = TRUE
  ))
  write_table("real_data_scope.csv", data.frame(
    Patient_ID = "patient_1", Hemisphere = "left_STN",
    Group_ID = "group_1", train_id = "train_1"
  ))
  quality_keys <- c(
    "patient_n", "hemisphere_n", "recording_group_n", "train_n", "isi_n",
    "manual_labeled_n", "blank_as_other_n", "multi_axis_interval_n",
    "group_overlap_max", "state_reference_eligible_train_n", "bootstrap_n"
  )
  write_table("data_quality_and_protocol_checks.csv", data.frame(
    check = quality_keys,
    value = c(1, 2, 1, 1, 100, 80, 20, 3, 0, 1, 2000)
  ))
  write_table("tonic_like_review_intervals.csv", data.frame(
    Train_ID = "train_1", Pattern = "tonic_like_review"
  ))
  write_table("tonic_like_review_episodes.csv", data.frame(
    Train_ID = "train_1", n_isi = 3L, duration_sec = 0.09
  ))
  write_table("tonic_like_review_summary.csv", data.frame(
    review_label = "tonic_like_review", interval_n = 3L, episode_n = 1L,
    train_n = 1L, median_episode_isi_n = 3,
    median_episode_duration_sec = 0.09,
    reporting_role = "descriptive_review_only_not_state_endpoint"
  ))
  saveRDS(list(status = "fixture"),
          file.path(path, "publication_validation_result.rds"))
  path
}

test_that("frozen result paths and real Tonic semantics fail closed", {
  expect_error(
    publication_helper_env$stpd_pub_resolve_frozen_result_dir(
      "STPD_REAL_VALIDATION_DIR", configured_path = ""
    ),
    "fallback is disabled", fixed = TRUE
  )
  path <- make_frozen_real_result_fixture()
  bundle <- publication_helper_env$stpd_pub_validate_real_result_bundle(path)
  expect_identical(bundle$path, normalizePath(path, mustWork = TRUE))
  expect_false(any(bundle$selected$Pattern == "tonic"))
  expect_setequal(
    bundle$confirmatory_event$pattern, c("burst", "pause", "broad_hfs")
  )

  runtime_file <- file.path(path, "runtime_and_candidate_status.csv")
  runtime <- read.csv(runtime_file, stringsAsFactors = FALSE)
  runtime$tonic_reference_role <- "formal_state"
  write.csv(runtime, runtime_file, row.names = FALSE)
  expect_error(
    publication_helper_env$stpd_pub_validate_real_result_bundle(path),
    "not a tonic_like_review run", fixed = TRUE
  )
  runtime$tonic_reference_role <- "tonic_like_review"
  write.csv(runtime, runtime_file, row.names = FALSE)

  selected_file <- file.path(path, "sampled_10_per_pattern_by_fold.csv")
  selected <- read.csv(selected_file, stringsAsFactors = FALSE)
  selected <- rbind(selected, data.frame(Pattern = "tonic", fold_id = 1L))
  write.csv(selected, selected_file, row.names = FALSE)
  expect_error(
    publication_helper_env$stpd_pub_validate_real_result_bundle(path),
    "Tonic is review-only", fixed = TRUE
  )
  selected <- selected[selected$Pattern != "tonic", , drop = FALSE]
  write.csv(selected, selected_file, row.names = FALSE)

  learned_file <- file.path(path, "learned_parameters_by_fold.csv")
  learned <- read.csv(learned_file, stringsAsFactors = FALSE)
  learned$Source[1L] <- "balanced_manual_examples"
  write.csv(learned, learned_file, row.names = FALSE)
  expect_error(
    publication_helper_env$stpd_pub_validate_real_result_bundle(path),
    "no real Tonic calibration evidence", fixed = TRUE
  )
  learned$Source[1L] <- "predeclared_default_no_real_tonic_reference"
  write.csv(learned, learned_file, row.names = FALSE)

  confirmatory_file <- file.path(path, "confirmatory_event_iou_metrics.csv")
  confirmatory <- read.csv(confirmatory_file, stringsAsFactors = FALSE)
  confirmatory$pattern[1L] <- "tonic"
  write.csv(confirmatory, confirmatory_file, row.names = FALSE)
  expect_error(
    publication_helper_env$stpd_pub_validate_real_result_bundle(path),
    "Burst, Pause, and Broad HFS only", fixed = TRUE
  )
})

test_that("report and manifest require explicit frozen result directories", {
  repo_root <- dirname(dirname(dirname(publication_helper_path)))
  report_text <- paste(readLines(file.path(
    repo_root, "evaluation", "publication_validation",
    "build_publication_validation_report.R"
  ), warn = FALSE), collapse = "\n")
  manifest_text <- paste(readLines(file.path(
    repo_root, "evaluation", "publication_validation",
    "write_reproducibility_manifest.R"
  ), warn = FALSE), collapse = "\n")
  required_env <- c(
    "STPD_SIM_SHORT_VALIDATION_DIR", "STPD_SIM_MEDIUM_VALIDATION_DIR",
    "STPD_SIM_LONG_VALIDATION_DIR", "STPD_REAL_VALIDATION_DIR"
  )
  expect_true(all(vapply(required_env, function(x) {
    grepl(x, report_text, fixed = TRUE) && grepl(x, manifest_text, fixed = TRUE)
  }, logical(1))))
  expect_match(report_text, "stpd_pub_validate_real_result_bundle", fixed = TRUE)
  expect_match(
    report_text,
    "确认性Tonic仅允许使用Stage-C `phenotype / eligible_primary`估计量",
    fixed = TRUE
  )
  expect_false(grepl("include_real_tonic", report_text, fixed = TRUE))
  expect_match(manifest_text, "stpd_pub_validate_real_result_bundle", fixed = TRUE)
  expect_match(manifest_text, "detector_r_files", fixed = TRUE)
  expect_match(manifest_text, "parameter_config_files", fixed = TRUE)
  expect_false(grepl(
    'file.path(root, "real_PD_STN_reference_eligible")',
    report_text, fixed = TRUE
  ))
})

test_that("review-only Tonic leaves Burst Pause and HFS calibration unchanged", {
  trains <- paste0("train_", seq_len(10L))
  spikes <- stats::setNames(vector("list", length(trains)), trains)
  interval_rows <- list()
  selected_rows <- list()
  for (ii in seq_along(trains)) {
    isi <- rep(0.030, 90L)
    isi[1:5] <- c(0.008, 0.009, 0.010, 0.009, 0.008)
    isi[10:12] <- c(0.090, 0.100, 0.110)
    isi[20:25] <- c(0.035, 0.037, 0.036, 0.038, 0.035, 0.037)
    isi[30:59] <- 0.012
    spikes[[ii]] <- c(0, cumsum(isi))
    specifications <- list(
      burst = list(axis = "event", index = 2:6),
      pause = list(axis = "event", index = 11:13),
      tonic = list(axis = "state", index = 21:26),
      high_frequency_spiking = list(axis = "state", index = 31:60),
      other = list(axis = "other", index = 70:74)
    )
    for (pattern in names(specifications)) {
      specification <- specifications[[pattern]]
      episode <- paste0(pattern, "_", ii)
      interval_rows[[length(interval_rows) + 1L]] <- data.frame(
        Train_ID = trains[ii], Group_ID = paste0("group_", ii),
        Right_Spike_Index = specification$index,
        ISI_s = isi[specification$index - 1L],
        Axis = specification$axis, Pattern = pattern, Episode = episode,
        stringsAsFactors = FALSE
      )
      selected_rows[[length(selected_rows) + 1L]] <- data.frame(
        Train_ID = trains[ii], Group_ID = paste0("group_", ii),
        Axis = specification$axis, Episode = episode, Pattern = pattern,
        stringsAsFactors = FALSE
      )
    }
  }
  intervals <- do.call(rbind, interval_rows)
  selected <- do.call(rbind, selected_rows)
  formal <- publication_helper_env$stpd_pub_learn_params(
    spikes, trains, selected, intervals, dataset_name = "formal_fixture",
    tonic_reference_role = "formal_state"
  )
  review <- publication_helper_env$stpd_pub_learn_params(
    spikes, trains, selected, intervals, dataset_name = "review_fixture",
    tonic_reference_role = "tonic_like_review"
  )

  invariant <- grepl("^(qc[.]|burst[.]|pause[.]|hfs[.])",
                     formal$manifest$Parameter)
  expect_identical(
    formal$manifest$Parameter[invariant], review$manifest$Parameter[invariant]
  )
  expect_equal(
    formal$manifest$Value[invariant], review$manifest$Value[invariant],
    tolerance = 0
  )
  tonic_manifest <- review$manifest[
    grepl("^tonic[.]", review$manifest$Parameter), , drop = FALSE
  ]
  expect_true(nrow(tonic_manifest) > 0L)
  expect_true(all(
    tonic_manifest$Source == "predeclared_default_no_real_tonic_reference"
  ))
  expect_identical(
    tonic_manifest$Value[
      tonic_manifest$Parameter == "tonic.short_regular_route_enabled"
    ],
    0
  )
  expect_identical(
    tonic_manifest$Value[
      tonic_manifest$Parameter == "tonic.mm_relaxation_evidence_n"
    ],
    0
  )
})

test_that("real publication workbook path and SHA are fail-closed", {
  fixture_repo <- file.path(tempdir(), paste0("stpd_reference_", Sys.getpid()))
  canonical <- file.path(
    fixture_repo, "data", "real", "STN",
    "PD_STN_manual_isi_labels.xlsx"
  )
  legacy <- file.path(
    fixture_repo,
    "PD_STN_manual_isi_labels.xlsx"
  )
  dir.create(dirname(canonical), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("authoritative fixture"), canonical)
  writeBin(charToRaw("legacy fixture"), legacy)
  expected_sha <- digest::digest(canonical, algo = "sha256", file = TRUE)

  resolved <- publication_helper_env$stpd_pub_resolve_authoritative_workbook(
    fixture_repo, expected_sha256 = expected_sha
  )
  expect_identical(resolved$path, normalizePath(canonical, mustWork = TRUE))
  expect_identical(resolved$sha256, expected_sha)
  expect_error(
    publication_helper_env$stpd_pub_resolve_authoritative_workbook(
      fixture_repo, configured_path = legacy,
      expected_sha256 = digest::digest(legacy, algo = "sha256", file = TRUE)
    ),
    "canonical PD_STN path"
  )
  expect_error(
    publication_helper_env$stpd_pub_resolve_authoritative_workbook(
      fixture_repo, expected_sha256 = strrep("0", 64L)
    ),
    "SHA-256"
  )
})

test_that("real publication entry wires review-only Tonic and workbook contract", {
  repo_root <- dirname(dirname(dirname(publication_helper_path)))
  entry_paths <- c(
    file.path(
      repo_root, "evaluation", "publication_validation",
      "run_real_leave_group_out_validation.R"
    ),
    file.path(
      repo_root, "evaluation", "publication_validation", "diagnostics",
      "real_tonic_miss_trace", "run_trace.R"
    ),
    file.path(
      repo_root, "evaluation", "publication_validation",
      "write_reproducibility_manifest.R"
    )
  )
  expect_true(all(file.exists(entry_paths)))
  entry_text <- vapply(entry_paths, function(path) paste(
    readLines(path, warn = FALSE), collapse = "\n"
  ), character(1))
  expect_true(all(grepl(
    "stpd_pub_resolve_authoritative_workbook", entry_text, fixed = TRUE
  )))
  expect_match(entry_text[[1L]], 'tonic_reference_role = "tonic_like_review"',
               fixed = TRUE)
  expect_match(entry_text[[2L]], 'tonic_reference_role = "tonic_like_review"',
               fixed = TRUE)
  expect_false(any(grepl("xlsx_candidates", entry_text, fixed = TRUE)))
})

test_that("Broad-HFS truth excludes canonical Pause from direct support", {
  interval <- rbind(
    data.frame(
      Train_ID = "train_1", Group_ID = "group_1",
      Right_Spike_Index = 2:6, ISI_s = c(0.02, 0.02, 0.11, 0.004, 0.02),
      Axis = "state", Pattern = "high_frequency_spiking"
    ),
    data.frame(
      Train_ID = "train_1", Group_ID = "group_1",
      Right_Spike_Index = c(4L, 5L), ISI_s = c(0.11, 0.004),
      Axis = "event", Pattern = c("pause", "burst")
    )
  )
  truth <- publication_helper_env$stpd_pub_truth_axes(interval, "train_1")
  broad <- truth[truth$Axis == "state_hf_family", , drop = FALSE]
  joint <- truth[truth$Axis == "coexistence", , drop = FALSE]

  expect_identical(
    broad$Truth[match(2:6, broad$Right_Spike_Index)],
    c(
      "high_frequency_spiking", "high_frequency_spiking", "other",
      "high_frequency_spiking", "high_frequency_spiking"
    )
  )
  expect_identical(
    joint$Truth[match(c(4L, 5L), joint$Right_Spike_Index)],
    c("high_frequency_spiking+pause", "high_frequency_spiking+burst")
  )

  events <- publication_helper_env$stpd_pub_truth_events(interval, "train_1")
  broad_events <- events[
    events$Axis == "state_hf_family" &
      events$Pattern == "high_frequency_spiking",
    , drop = FALSE
  ]
  expect_identical(broad_events$start_isi, 2L)
  expect_identical(broad_events$end_isi, 6L)
})

test_that("Broad-HFS mapping recognizes the parent episode class", {
  expect_identical(
    publication_helper_env$stpd_pub_broad_hf_pattern(c(
      "broad_high_frequency_state", "high_frequency_irregular_state",
      "high_frequency_tonic", "hf_unresolved", "tonic"
    )),
    c(
      "high_frequency_spiking", "high_frequency_spiking",
      "high_frequency_spiking", "high_frequency_spiking", "tonic"
    )
  )
})

test_that("truth event construction handles a held-out train with no State rows", {
  interval <- data.frame(
    Train_ID = "train_no_state", Group_ID = "group_1",
    Right_Spike_Index = 2:4, ISI_s = c(0.1, 0.2, 0.1),
    Axis = c("event", "other", "event"),
    Pattern = c("burst", "other", "pause"),
    stringsAsFactors = FALSE
  )

  events <- publication_helper_env$stpd_pub_truth_events(
    interval, "train_no_state"
  )

  expect_s3_class(events, "data.frame")
  expect_false(any(events$Axis == "state_hf_family"))
  expect_true(all(events$Pattern %in% c("burst", "pause")))
})

test_that("prediction adapter keeps strict support but uses the HF parent envelope", {
  per_isi <- data.frame(
    train = "train_1", isi_index = 1:6,
    state_class = c(
      "", "high_frequency_irregular_state", "hf_unresolved",
      "high_frequency_tonic", "high_frequency_tonic", "high_frequency_tonic"
    ),
    state_episode_id = c("", rep("hfs_episode_1", 5L)),
    state_episode_class = c("", rep("broad_high_frequency_state", 5L)),
    event_family = c("", "", "", "", "burst", ""),
    gap_class = c("", "", "", "pause", "", ""),
    stringsAsFactors = FALSE
  )
  product <- list(
    metadata = data.frame(
      materialization_status = "materialized",
      schema_version = "stpd_multitrack_auto_v3",
      policy_hash = strrep("a", 64L),
      product_sha256 = strrep("b", 64L),
      stringsAsFactors = FALSE
    ),
    per_isi = per_isi
  )
  detected <- structure(list(results = list(multitrack_auto = "raw-slot-poison")),
                        class = "publication_accessor_fixture")
  accessor_called <- 0L
  old_accessor <- get0(
    "stpd_multitrack_auto", envir = publication_helper_env,
    inherits = FALSE
  )
  assign("stpd_multitrack_auto", function(x) {
    accessor_called <<- accessor_called + 1L
    expect_s3_class(x, "publication_accessor_fixture")
    product
  }, envir = publication_helper_env)
  on.exit({
    if (is.null(old_accessor)) {
      rm("stpd_multitrack_auto", envir = publication_helper_env)
    } else {
      assign("stpd_multitrack_auto", old_accessor,
             envir = publication_helper_env)
    }
  }, add = TRUE)
  result <- publication_helper_env$stpd_pub_extract_predictions(
    detected, "train_1"
  )
  expect_identical(accessor_called, 1L)
  broad <- result$interval[result$interval$Axis == "state_hf_family", ]
  joint <- result$interval[result$interval$Axis == "coexistence", ]
  expect_identical(
    broad$Prediction[match(2:6, broad$Right_Spike_Index)],
    c(
      "high_frequency_spiking", "high_frequency_spiking", "other",
      "high_frequency_spiking", "high_frequency_spiking"
    )
  )
  expect_identical(
    joint$Prediction[match(c(4L, 5L), joint$Right_Spike_Index)],
    c("high_frequency_spiking+pause", "high_frequency_spiking+burst")
  )
  broad_events <- result$events[
    result$events$Axis == "state_hf_family" &
      result$events$Pattern == "high_frequency_spiking",
    , drop = FALSE
  ]
  expect_identical(broad_events$start_isi, 2L)
  expect_identical(broad_events$end_isi, 6L)
  expect_identical(
    as.character(result$candidate_metadata$schema_version),
    "stpd_multitrack_auto_v3"
  )
  expect_identical(
    as.character(result$candidate_metadata$policy_hash),
    strrep("a", 64L)
  )
  expect_identical(
    as.character(result$candidate_metadata$product_sha256),
    strrep("b", 64L)
  )
  provenance <- publication_helper_env$stpd_pub_candidate_provenance(
    result$candidate_metadata
  )
  expect_identical(
    names(provenance),
    c(
      "multitrack_schema_version", "multitrack_policy_hash",
      "multitrack_product_sha256"
    )
  )
  expect_identical(
    unname(as.character(provenance[1, ])),
    c("stpd_multitrack_auto_v3", strrep("a", 64L), strrep("b", 64L))
  )

  fold_runtimes <- do.call(rbind, list(
    cbind(data.frame(repeat_id = 1L), provenance),
    cbind(data.frame(repeat_id = 2L), provenance)
  ))
  expect_identical(nrow(fold_runtimes), 2L)
  expect_true(all(c(
    "multitrack_schema_version", "multitrack_policy_hash",
    "multitrack_product_sha256"
  ) %in% names(fold_runtimes)))
})

test_that("publication F1 is zero when errors exist and no true positive exists", {
  counts <- data.frame(
    level = "event", axis = "state_hf_family",
    label = "high_frequency_spiking", tp = 0L, fp = 2L, fn = 3L,
    support = 3L, correct = NA_integer_, total = NA_integer_
  )
  metric <- publication_helper_env$stpd_pub_metric_from_counts(counts)
  expect_identical(metric$F1, 0)
})

test_that("confirmatory evaluation includes Burst, Pause, and Broad HFS only", {
  events <- data.frame(
    Train_ID = rep("train_1", 5L),
    Axis = c("event", "event", "state_hf_family", "state_strict", "event"),
    Pattern = c(
      "burst", "pause", "high_frequency_spiking", "tonic", "other"
    ),
    start_isi = c(2L, 10L, 20L, 30L, 40L),
    end_isi = c(4L, 10L, 28L, 35L, 40L),
    stringsAsFactors = FALSE
  )
  out <- publication_helper_env$stpd_pub_confirmatory_evaluation_events(events)
  expect_identical(out$pattern, c("broad_hfs", "burst", "pause"))
  expect_false(any(out$pattern %in% c("tonic", "other")))
  result <- stpd_evaluate_detection_results(out, out)
  expect_true(all(result$isi_metrics$truth_isi_recall == 1))
  expect_true(all(result$fragmentation_metrics$false_split_rate_all_truth == 0))
})

test_that("publication split exposes the prespecified borrowing ablation", {
  learn_formals <- formals(publication_helper_env$stpd_pub_learn_params)
  split_formals <- formals(publication_helper_env$stpd_pub_run_split)

  expect_true("bounded_borrowing" %in% names(learn_formals))
  expect_true("bounded_borrowing" %in% names(split_formals))
  expect_identical(eval(learn_formals$bounded_borrowing), TRUE)
  expect_identical(eval(split_formals$bounded_borrowing), TRUE)

  split_body <- paste(deparse(body(publication_helper_env$stpd_pub_run_split)),
                      collapse = "\n")
  expect_match(split_body, "disabled_ablation", fixed = TRUE)
  expect_match(split_body, "bounded_borrowing_calibration_input_sha256",
               fixed = TRUE)
  expect_match(split_body, "bounded_borrowing_applied_train_n", fixed = TRUE)
})

test_that("short contextual Pause labels do not collapse Broad-HFS support", {
  resolved <- publication_helper_env$stpd_pub_resolve_hfs_pause_bands(
    hfs_values = c(rep(0.010, 20L), rep(0.020, 20L), rep(0.040, 10L)),
    pause_values = c(rep(0.007, 8L), rep(0.080, 8L), rep(0.120, 8L)),
    min_valid_isi_sec = 0.001
  )

  expect_gt(resolved$hfs_band[["upper"]], 0.007)
  expect_gt(resolved$hfs_bridge, resolved$hfs_band[["upper"]])
  expect_lt(
    resolved$contextual_pause_observed_lower,
    resolved$hfs_band[["upper"]]
  )
  expect_lt(resolved$pause_band[["lower"]], resolved$hfs_bridge)
  expect_gte(
    resolved$pause_band[["upper"]], resolved$pause_band[["lower"]]
  )
  expect_lte(
    resolved$hfs_bridge, resolved$hfs_band[["upper"]] * 1.50
  )
})

test_that("reference eligibility is applied independently by semantic axis", {
  x <- data.frame(
    Train_ID = rep(c("event_only", "state_ready"), each = 4L),
    Axis = rep(c("event", "state_strict", "state_hf_family", "coexistence"), 2L),
    value = seq_len(8L), stringsAsFactors = FALSE
  )
  eligibility <- data.frame(
    Train_ID = c("event_only", "state_ready"),
    event_reference_eligible = c(TRUE, TRUE),
    state_reference_eligible = c(FALSE, TRUE),
    coexistence_reference_eligible = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  kept <- publication_helper_env$stpd_pub_filter_axis_reference(
    x, eligibility
  )

  expect_identical(
    kept$Axis[kept$Train_ID == "event_only"], "event"
  )
  expect_identical(
    kept$Axis[kept$Train_ID == "state_ready"],
    c("event", "state_strict", "state_hf_family", "coexistence")
  )
})
}
