#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else NA_character_
repo_default <- if (is.character(script_path) && !is.na(script_path)) {
  file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
} else getwd()
repo <- normalizePath(Sys.getenv("STPD_REPO", unset = repo_default), mustWork = TRUE)
pipeline_candidates <- c(
  file.path(repo, "evaluation", "synthetic_validation"),
  file.path(repo, "test-results", "clean_synthetic_validation")
)
pipeline <- pipeline_candidates[dir.exists(pipeline_candidates)][1L]
benchmark_candidates <- c(
  file.path(repo, "data", "synthetic", "v2.3.0"),
  file.path(repo, "Simulator data", "clean_synthetic_mechanism_benchmark_v2_3_0")
)
benchmark <- benchmark_candidates[file.exists(benchmark_candidates)][1L]
files <- file.path(pipeline, c(
  "v2_pipeline_common.R", "run_v2_stage_a_calibrate.R",
  "run_v2_stage_b_detect.R", "run_v2_stage_c_score.R"
))
for (path in files) {
  parse(file = path)
  source(path, local = FALSE)
}

stopifnot(
  identical(stpd_v2_batch_selection(""), c("Batch_A", "Batch_B", "Batch_C")),
  identical(stpd_v2_batch_selection("Batch_B"), "Batch_B")
)

stage_b_text <- paste(readLines(files[3L], warn = FALSE), collapse = "\n")
forbidden_stage_b <- c(
  "sample_template_scale_key", "interval_truth", "event_episodes",
  "state_envelopes", "observable_phenotype", "generator_parameters",
  "STPD_CLEAN_BENCHMARK", "spike_timestamps_blinded", "Template_ID",
  "Scale_Factor", "candidate_reason_code)"
)
hits <- forbidden_stage_b[vapply(
  forbidden_stage_b, grepl, logical(1), x = stage_b_text, fixed = TRUE
)]
if (length(hits)) stop(
  "Stage B contains a protected-source token: ", paste(hits, collapse = ", "),
  call. = FALSE
)
stopifnot(
  grepl("gap_semantics", stage_b_text, fixed = TRUE),
  grepl(
    "STPD_V2_BATCH",
    paste(readLines(files[1L], warn = FALSE), collapse = "\n"), fixed = TRUE
  ),
  grepl("exactly 15 holdout samples", stage_b_text, fixed = TRUE)
)

workbook_path <- file.path(
  benchmark, "detector_inputs", "spike_timestamps_blinded.xlsx"
)
book <- stpd_v2_read_workbook(workbook_path)
stopifnot(
  identical(names(book), c("Batch_A", "Batch_B", "Batch_C")),
  all(vapply(book, function(z) length(unique(z$Sample_ID)), integer(1)) == 20L)
)
key <- utils::read.csv(
  file.path(benchmark, "truth", "sample_template_scale_key.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
calibration <- utils::read.csv(
  file.path(
    benchmark, "calibration", "calibration_same_episode_ids_all_scales.csv"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)
for (batch_id in names(book)) {
  development <- key$Sample_ID[
    key$Workbook_Batch == batch_id & key$Split == "development"
  ]
  holdout <- key$Sample_ID[
    key$Workbook_Batch == batch_id & key$Split == "holdout"
  ]
  stopifnot(
    length(development) == 5L, length(holdout) == 15L,
    !any(holdout %in% calibration$Sample_ID)
  )
  inputs <- stpd_v2_calibration_inputs(calibration, book[[batch_id]])
  stopifnot(
    setequal(unique(inputs$selected$Train_ID), development),
    !any(inputs$intervals$Train_ID %in% holdout)
  )
}

holdout_a <- key$Sample_ID[
  key$Workbook_Batch == "Batch_A" & key$Split == "holdout"
]
sanitized <- book$Batch_A[
  book$Batch_A$Sample_ID %in% holdout_a,
  c("Sample_ID", "Spike_Index", "Time_s"), drop = FALSE
]
temporary_input <- tempfile(fileext = ".csv")
on.exit(unlink(temporary_input), add = TRUE)
stpd_v2_write_csv(sanitized, temporary_input)
roundtrip <- stpd_v2_read_blinded_holdout(
  temporary_input, expected_batch = "Batch_A"
)
stopifnot(
  identical(names(roundtrip), c("Sample_ID", "Spike_Index", "Time_s")),
  length(unique(roundtrip$Sample_ID)) == 15L
)

sample_key <- key[
  key$Split == "holdout",
  c("Sample_ID", "Template_ID", "Scale_Factor", "Workbook_Batch")
]
names(sample_key)[4L] <- "Batch_ID"
signature <- data.frame(
  Estimand = "mechanism", Variant = "strict_raw", Level = "isi_support",
  IoU = "support", Target = "burst", stringsAsFactors = FALSE
)
one <- cbind(signature, sample_key[1L, , drop = FALSE], tp = 1, fp = 0, fn = 0)
complete <- stpd_v2_complete_counts(one, sample_key, signature)
stopifnot(
  nrow(complete) == 45L, sum(complete$tp) == 1, !anyNA(complete),
  all(table(complete$Scale_Factor) == 15L)
)
bootstrap <- stpd_v2_bootstrap(complete, n_bootstrap = 3L, seed = 1L)
stopifnot(
  nrow(bootstrap$observed) == 3L,
  nrow(bootstrap$summary) == 9L,
  nrow(bootstrap$draws) == 9L
)
wrong <- one
wrong$Template_ID <- "TPL_WRONG"
wrong_error <- try(stpd_v2_complete_counts(wrong, sample_key, signature), silent = TRUE)
stopifnot(inherits(wrong_error, "try-error"))

semantic_a <- stpd_v2_scientific_content_sha256(
  data.frame(x = 1:2), data.frame(y = 2:1),
  data.frame(run_id = "run-a", gap_id = c("ga", "gb"), start_isi = 1:2),
  data.frame(
    run_id = "run-a", state_id = "sa", state_episode_id = "ea",
    start_isi = 1L
  )
)
semantic_b <- stpd_v2_scientific_content_sha256(
  data.frame(x = 2:1), data.frame(y = 1:2),
  data.frame(run_id = "run-b", gap_id = c("gb2", "ga2"), start_isi = 2:1),
  data.frame(
    run_id = "run-b", state_id = "sb", state_episode_id = "eb",
    start_isi = 1L
  )
)
stopifnot(identical(semantic_a, semantic_b))

stage_c_text <- paste(readLines(files[4L], warn = FALSE), collapse = "\n")
stopifnot(
  grepl("stpd_v2_tonic_phenotype_contract", stage_c_text, fixed = TRUE),
  grepl("stpd_v2_masked_episode_tables", stage_c_text, fixed = TRUE),
  grepl('Variant = "eligible_primary"', stage_c_text, fixed = TRUE),
  grepl('Target = "tonic"', stage_c_text, fixed = TRUE)
)

tonic_fixture <- data.frame(
  Sample_ID = rep("S1", 8L), Right_Spike_Index = seq_len(8L),
  State_Label = c(rep("Tonic", 4L), rep("none", 4L)),
  State_Envelope_ID = c("E1", "E1", "E2", "E3", rep("none", 4L)),
  Tonic_Phenotype = c(
    "eligible", "eligible", "ambiguous", "no_evidence",
    rep("not_applicable", 4L)
  ),
  stringsAsFactors = FALSE
)
tonic_contract <- stpd_v2_tonic_phenotype_contract(tonic_fixture)
stopifnot(
  identical(
    tonic_contract$score_keep,
    c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  ),
  identical(
    tonic_contract$truth_positive,
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
)
tonic_tables <- stpd_v2_masked_episode_tables(
  tonic_fixture, sample_ids = "S1",
  score_keep = tonic_contract$score_keep,
  truth_positive = tonic_contract$truth_positive,
  prediction_positive = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  target = "tonic"
)
stopifnot(
  nrow(tonic_tables$truth) == 1L,
  identical(as.integer(tonic_tables$truth$start_isi), 1L),
  identical(as.integer(tonic_tables$truth$end_isi), 2L),
  nrow(tonic_tables$pred) == 2L,
  identical(as.integer(tonic_tables$pred$start_isi), c(1L, 5L)),
  identical(as.integer(tonic_tables$pred$end_isi), c(2L, 6L))
)

missing_phenotype <- tonic_fixture[names(tonic_fixture) != "Tonic_Phenotype"]
unexpected_phenotype <- tonic_fixture
unexpected_phenotype$Tonic_Phenotype[3L] <- "review"
non_tonic_eligible <- tonic_fixture
non_tonic_eligible$Tonic_Phenotype[5L] <- "eligible"
mixed_envelope <- tonic_fixture
mixed_envelope$Tonic_Phenotype[2L] <- "ambiguous"
stopifnot(
  inherits(try(
    stpd_v2_tonic_phenotype_contract(missing_phenotype), silent = TRUE
  ), "try-error"),
  inherits(try(
    stpd_v2_tonic_phenotype_contract(unexpected_phenotype), silent = TRUE
  ), "try-error"),
  inherits(try(
    stpd_v2_tonic_phenotype_contract(non_tonic_eligible), silent = TRUE
  ), "try-error"),
  inherits(try(
    stpd_v2_tonic_phenotype_contract(mixed_envelope), silent = TRUE
  ), "try-error")
)

for (version in c("v2_2_0", "v2_3_0")) {
  version_root <- file.path(
    repo, "Simulator data",
    paste0("clean_synthetic_mechanism_benchmark_", version)
  )
  version_key <- utils::read.csv(
    file.path(version_root, "truth", "sample_template_scale_key.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  version_truth <- utils::read.csv(
    file.path(version_root, "truth", "interval_truth_multitrack.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  version_holdout <- as.character(
    version_key$Sample_ID[version_key$Split == "holdout"]
  )
  version_truth <- version_truth[
    version_truth$Sample_ID %in% version_holdout, , drop = FALSE
  ]
  version_contract <- stpd_v2_tonic_phenotype_contract(version_truth)
  version_is_tonic <- version_truth$State_Label == "Tonic"
  stopifnot(
    setequal(unique(version_truth$Sample_ID), version_holdout),
    identical(
      version_contract$truth_positive,
      version_is_tonic & version_truth$Tonic_Phenotype == "eligible"
    ),
    identical(
      version_contract$score_keep,
      !(version_is_tonic & version_truth$Tonic_Phenotype %in%
          c("ambiguous", "no_evidence"))
    ),
    all(version_contract$score_keep[!version_is_tonic]),
    any(version_contract$truth_positive)
  )
}

interval_truth <- utils::read.csv(
  file.path(benchmark, "truth", "interval_truth_multitrack.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
stopifnot(all(
  as.integer(interval_truth$Interval_Index) + 1L ==
    as.integer(interval_truth$Right_Spike_Index)
))

cat("v2 pipeline lightweight contract: PASS\n")
