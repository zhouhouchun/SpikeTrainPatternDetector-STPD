#!/usr/bin/env Rscript

stpd_v2_tonic_phenotype_contract <- function(interval_truth) {
  interval_truth <- as.data.frame(interval_truth, stringsAsFactors = FALSE)
  required <- c(
    "Sample_ID", "Right_Spike_Index", "State_Label",
    "State_Envelope_ID", "Tonic_Phenotype"
  )
  missing <- setdiff(required, names(interval_truth))
  if (length(missing)) {
    stpd_v2_stop(
      "Tonic phenotype scoring is missing required column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }
  if (!nrow(interval_truth)) {
    stpd_v2_stop("Tonic phenotype scoring received no interval truth rows.")
  }

  sample_id <- as.character(interval_truth$Sample_ID)
  state_label <- as.character(interval_truth$State_Label)
  phenotype <- as.character(interval_truth$Tonic_Phenotype)
  if (anyNA(sample_id) || any(!nzchar(sample_id)) ||
      anyNA(state_label) || any(!nzchar(state_label)) ||
      anyNA(phenotype) || any(!nzchar(phenotype))) {
    stpd_v2_stop("Tonic phenotype scoring fields contain missing or blank values.")
  }
  allowed <- c("eligible", "ambiguous", "no_evidence", "not_applicable")
  unexpected <- setdiff(unique(phenotype), allowed)
  if (length(unexpected)) {
    stpd_v2_stop(
      "Unexpected Tonic_Phenotype value(s): ",
      paste(sort(unexpected, method = "radix"), collapse = ", "), "."
    )
  }
  unexpected_state <- setdiff(
    unique(state_label), c("none", "Tonic", "Broad_HFS")
  )
  if (length(unexpected_state)) {
    stpd_v2_stop(
      "Unexpected State_Label value(s) in Tonic phenotype scoring: ",
      paste(sort(unexpected_state, method = "radix"), collapse = ", "), "."
    )
  }

  is_tonic <- state_label == "Tonic"
  if (any(is_tonic & !phenotype %in% c(
    "eligible", "ambiguous", "no_evidence"
  )) || any(!is_tonic & phenotype != "not_applicable")) {
    stpd_v2_stop(
      "Tonic_Phenotype must classify every Tonic row and be not_applicable ",
      "for every non-Tonic row."
    )
  }
  tonic_envelope <- as.character(interval_truth$State_Envelope_ID)
  if (any(is_tonic & (
    is.na(tonic_envelope) | !nzchar(tonic_envelope) |
      tonic_envelope == "none"
  ))) {
    stpd_v2_stop("A Tonic phenotype row lacks a State_Envelope_ID.")
  }
  if (any(is_tonic)) {
    envelope_key <- paste(sample_id[is_tonic], tonic_envelope[is_tonic], sep = "\r")
    phenotype_n <- vapply(
      split(phenotype[is_tonic], envelope_key),
      function(value) length(unique(value)), integer(1)
    )
    if (any(phenotype_n != 1L)) {
      stpd_v2_stop("A Tonic State envelope has inconsistent phenotype labels.")
    }
  }

  truth_positive <- is_tonic & phenotype == "eligible"
  if (!any(truth_positive)) {
    stpd_v2_stop("No phenotype-eligible Tonic support is present for scoring.")
  }
  excluded <- is_tonic & phenotype %in% c("ambiguous", "no_evidence")
  list(
    score_keep = !excluded,
    truth_positive = truth_positive,
    excluded = excluded
  )
}

stpd_v2_masked_episode_tables <- function(interval_truth, sample_ids,
                                          score_keep, truth_positive,
                                          prediction_positive, target) {
  interval_truth <- as.data.frame(interval_truth, stringsAsFactors = FALSE)
  required <- c("Sample_ID", "Right_Spike_Index")
  missing <- setdiff(required, names(interval_truth))
  if (length(missing)) {
    stpd_v2_stop(
      "Masked episode scoring is missing required column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }
  n <- nrow(interval_truth)
  flags <- list(
    score_keep = score_keep,
    truth_positive = truth_positive,
    prediction_positive = prediction_positive
  )
  if (any(vapply(flags, length, integer(1)) != n) ||
      any(!vapply(flags, is.logical, logical(1))) ||
      any(vapply(flags, anyNA, logical(1)))) {
    stpd_v2_stop(
      "Masked episode scoring requires complete logical vectors for every row."
    )
  }
  sample <- as.character(interval_truth$Sample_ID)
  sample_ids <- as.character(sample_ids)
  target <- as.character(target)[1L]
  right_index <- suppressWarnings(as.numeric(interval_truth$Right_Spike_Index))
  if (!length(sample_ids) || anyNA(sample_ids) || any(!nzchar(sample_ids)) ||
      anyDuplicated(sample_ids) || anyNA(sample) || any(!nzchar(sample)) ||
      !setequal(unique(sample), sample_ids) ||
      any(!is.finite(right_index)) || any(right_index != floor(right_index)) ||
      anyDuplicated(data.frame(Sample_ID = sample, Right_Spike_Index = right_index)) ||
      is.na(target) || !nzchar(target)) {
    stpd_v2_stop("Masked episode scoring received an invalid sample/index contract.")
  }

  empty <- data.frame(
    train = character(), target = character(), start_isi = integer(),
    end_isi = integer(), stringsAsFactors = FALSE
  )
  prediction_rows <- list()
  truth_rows <- list()
  for (id in sample_ids) {
    index <- which(sample == id & score_keep)
    index <- index[order(right_index[index], method = "radix")]
    if (!length(index)) next
    predicted <- stpd_v2_runs(right_index[index], prediction_positive[index])
    truth <- stpd_v2_runs(right_index[index], truth_positive[index])
    if (nrow(predicted)) {
      prediction_rows[[length(prediction_rows) + 1L]] <- data.frame(
        train = rep(id, nrow(predicted)), target = rep(target, nrow(predicted)),
        predicted, stringsAsFactors = FALSE
      )
    }
    if (nrow(truth)) {
      truth_rows[[length(truth_rows) + 1L]] <- data.frame(
        train = rep(id, nrow(truth)), target = rep(target, nrow(truth)),
        truth, stringsAsFactors = FALSE
      )
    }
  }
  list(
    pred = if (length(prediction_rows)) do.call(rbind, prediction_rows) else empty,
    truth = if (length(truth_rows)) do.call(rbind, truth_rows) else empty
  )
}

stpd_v2_stage_c_main <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else NA_character_
  repo_default <- if (is.character(script_path) && !is.na(script_path)) {
    file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
  } else getwd()
  repo <- normalizePath(
    Sys.getenv("STPD_REPO", unset = repo_default),
    mustWork = TRUE
  )
  benchmark_candidates <- c(
    file.path(repo, "data", "synthetic", "v2.3.0"),
    file.path(repo, "Simulator data", "clean_synthetic_mechanism_benchmark_v2_3_0")
  )
  benchmark_default <- benchmark_candidates[file.exists(benchmark_candidates)][1L]
  pipeline_candidates <- c(
    file.path(repo, "evaluation", "synthetic_validation"),
    file.path(repo, "test-results", "clean_synthetic_validation")
  )
  pipeline_dir <- pipeline_candidates[dir.exists(pipeline_candidates)][1L]
  benchmark <- normalizePath(
    Sys.getenv(
      "STPD_CLEAN_BENCHMARK",
      unset = benchmark_default
    ), mustWork = TRUE
  )
  stage_b <- normalizePath(
    Sys.getenv(
      "STPD_V2_STAGE_B",
      unset = "/private/tmp/stpd_clean_benchmark_v2_stage_b_20260827"
    ), mustWork = TRUE
  )
  out_dir <- Sys.getenv(
    "STPD_V2_STAGE_C_OUT",
    unset = "/private/tmp/stpd_clean_benchmark_v2_stage_c_20260827"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  source(file.path(
    pipeline_dir, "v2_pipeline_common.R"
  ), local = FALSE)
  required <- c("digest", "pkgload")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stpd_v2_stop("Missing packages: ", paste(missing, collapse = ", "))
  pkgload::load_all(repo, quiet = TRUE)

  batches <- c("Batch_A", "Batch_B", "Batch_C")
  read_stage_b <- function(batch_id, role) {
    manifest_path <- file.path(
      stage_b, paste0("stage_b_manifest_", batch_id, ".csv")
    )
    manifest <- utils::read.csv(
      manifest_path, check.names = FALSE, stringsAsFactors = FALSE
    )
    row <- manifest[manifest$File_Role == role, , drop = FALSE]
    if (nrow(row) != 1L || !identical(as.character(row$Batch_ID), batch_id)) {
      stpd_v2_stop("Invalid Stage-B manifest role ", role, " for ", batch_id)
    }
    path <- file.path(stage_b, row$File_Name)
    stpd_v2_assert_sha256(path, row$SHA256)
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }
  predicted_intervals <- do.call(rbind, lapply(
    batches, read_stage_b, role = "intervals"
  ))
  predicted_episodes_raw <- do.call(rbind, lapply(
    batches, read_stage_b, role = "episodes"
  ))
  predicted_gaps <- do.call(rbind, lapply(
    batches, read_stage_b, role = "gaps"
  ))
  predicted_states <- do.call(rbind, lapply(
    batches, read_stage_b, role = "states"
  ))

  key <- utils::read.csv(
    file.path(benchmark, "truth", "sample_template_scale_key.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  interval_truth_candidates <- file.path(
    benchmark, "truth", c(
      "interval_truth_multitrack.csv", "interval_truth_dual_track.csv"
    )
  )
  interval_truth_path <- interval_truth_candidates[
    file.exists(interval_truth_candidates)
  ][1L]
  if (!length(interval_truth_path) || is.na(interval_truth_path)) {
    stpd_v2_stop("No supported interval-truth artifact was found.")
  }
  interval_truth <- utils::read.csv(
    interval_truth_path,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  state_truth <- utils::read.csv(
    file.path(benchmark, "truth", "state_envelopes.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  holdout_key <- key[key$Split == "holdout", , drop = FALSE]
  if (nrow(holdout_key) != 45L || length(unique(holdout_key$Template_ID)) != 15L ||
      any(table(holdout_key$Scale_Factor) != 15L)) {
    stpd_v2_stop("The protected holdout key is not 15 templates x 3 scales.")
  }
  holdout_ids <- as.character(holdout_key$Sample_ID)
  interval_truth <- interval_truth[interval_truth$Sample_ID %in% holdout_ids, , drop = FALSE]
  state_truth <- state_truth[state_truth$Sample_ID %in% holdout_ids, , drop = FALSE]

  sample_key <- holdout_key[c(
    "Sample_ID", "Template_ID", "Scale_Factor", "Workbook_Batch"
  )]
  names(sample_key)[names(sample_key) == "Workbook_Batch"] <- "Batch_ID"
  if (anyDuplicated(sample_key$Sample_ID) ||
      any(table(sample_key$Template_ID) != 3L) ||
      any(table(sample_key$Scale_Factor) != 15L)) {
    stpd_v2_stop("Holdout scoring key is not a paired 15-template x 3-scale panel.")
  }
  predicted_intervals <- predicted_intervals[
    predicted_intervals$Train_ID %in% holdout_ids, , drop = FALSE
  ]
  predicted_sample_batch <- unique(
    predicted_intervals[c("Train_ID", "Batch_ID")]
  )
  expected_batch <- sample_key$Batch_ID[
    match(predicted_sample_batch$Train_ID, sample_key$Sample_ID)
  ]
  if (nrow(predicted_sample_batch) != 45L ||
      !setequal(predicted_sample_batch$Train_ID, holdout_ids) ||
      anyNA(expected_batch) ||
      any(as.character(predicted_sample_batch$Batch_ID) != expected_batch)) {
    stpd_v2_stop("Stage-B predictions do not match the 45 protected holdout samples.")
  }
  base_key <- paste(interval_truth$Sample_ID,
                    interval_truth$Right_Spike_Index, sep = "\r")
  if (anyDuplicated(base_key)) stpd_v2_stop("Duplicated truth interval key.")
  lookup_prediction <- function(axis) {
    z <- predicted_intervals[predicted_intervals$Axis == axis, , drop = FALSE]
    key_z <- paste(z$Train_ID, z$Right_Spike_Index, sep = "\r")
    if (anyDuplicated(key_z)) stpd_v2_stop("Duplicated prediction interval key.")
    if (length(key_z) != length(base_key) || !setequal(key_z, base_key) ||
        anyNA(z$Prediction) || any(!nzchar(as.character(z$Prediction)))) {
      stpd_v2_stop(
        "Prediction axis ", axis,
        " is not an exact export of every held-out right-spike interval."
      )
    }
    stats::setNames(as.character(z$Prediction), key_z)
  }
  event_map <- lookup_prediction("event")
  tonic_map <- lookup_prediction("state_strict")
  hfs_map <- lookup_prediction("state_hf_family")
  interval_truth$Pred_Event <- event_map[base_key]
  interval_truth$Pred_Tonic <- tonic_map[base_key]
  interval_truth$Pred_HFS <- hfs_map[base_key]
  for (column in c("Pred_Event", "Pred_Tonic", "Pred_HFS")) {
    if (anyNA(interval_truth[[column]])) {
      stpd_v2_stop("Prediction key alignment introduced missing values.")
    }
  }

  pause_map <- character()
  if (nrow(predicted_gaps)) {
    pause_rows <- lapply(seq_len(nrow(predicted_gaps)), function(i) {
      index <- seq.int(
        as.integer(predicted_gaps$start_isi[i]),
        as.integer(predicted_gaps$end_isi[i])
      )
      data.frame(
        key = paste(predicted_gaps$train[i], index, sep = "\r"),
        target = as.character(predicted_gaps$predicted_pause_target[i]),
        stringsAsFactors = FALSE
      )
    })
    pause_rows <- do.call(rbind, pause_rows)
    if (anyDuplicated(pause_rows$key)) stpd_v2_stop("Overlapping predicted gaps.")
    pause_map <- stats::setNames(pause_rows$target, pause_rows$key)
  }
  interval_truth$Pred_Pause_Target <- pause_map[base_key]
  interval_truth$Pred_Pause_Target[is.na(interval_truth$Pred_Pause_Target)] <- "other"

  sample_match <- match(interval_truth$Sample_ID, sample_key$Sample_ID)
  if (anyNA(sample_match)) stpd_v2_stop("An interval has no protected sample key.")
  if ("Template_ID" %in% names(interval_truth) &&
      any(as.character(interval_truth$Template_ID) !=
          as.character(sample_key$Template_ID[sample_match]))) {
    stpd_v2_stop("Interval truth and protected Template_ID disagree.")
  }
  if ("Scale_Factor" %in% names(interval_truth) &&
      any(as.character(interval_truth$Scale_Factor) !=
          as.character(sample_key$Scale_Factor[sample_match]))) {
    stpd_v2_stop("Interval truth and protected Scale_Factor disagree.")
  }
  if (!all(
    as.integer(interval_truth$Interval_Index) + 1L ==
      as.integer(interval_truth$Right_Spike_Index)
  )) {
    stpd_v2_stop("Truth support is not inclusive right-spike-index support.")
  }
  interval_truth$Batch_ID <- sample_key$Batch_ID[sample_match]
  if (anyNA(interval_truth$Batch_ID)) stpd_v2_stop("Failed to unblind a held-out sample.")
  tonic_phenotype_contract <- stpd_v2_tonic_phenotype_contract(interval_truth)

  count_binary <- function(z, truth_positive, prediction_positive,
                           estimand, variant, target, level, iou = "support") {
    rows <- lapply(split(seq_len(nrow(z)), z$Sample_ID), function(index) {
      q_truth <- as.logical(truth_positive[index])
      q_pred <- as.logical(prediction_positive[index])
      data.frame(
        Estimand = estimand, Variant = variant,
        Batch_ID = z$Batch_ID[index[1L]],
        Scale_Factor = z$Scale_Factor[index[1L]], Level = level,
        IoU = iou, Target = target,
        Template_ID = z$Template_ID[index[1L]], Sample_ID = z$Sample_ID[index[1L]],
        tp = sum(q_truth & q_pred), fp = sum(!q_truth & q_pred),
        fn = sum(q_truth & !q_pred), stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
  support_counts <- list()
  has_multitrack_contextual <- all(c(
    "Contextual_Separator_Label", "Contextual_Separator_ID"
  ) %in% names(interval_truth))
  contextual_truth <- if (has_multitrack_contextual) {
    interval_truth$Contextual_Separator_Label == "Contextual_Separator"
  } else {
    interval_truth$Event_Label == "Pause" &
      interval_truth$Pause_Subtype == "contextual_interburst_gap_separator"
  }
  target_contract <- list(
    burst = list(
      truth = interval_truth$Event_Label == "Burst",
      pred = interval_truth$Pred_Event == "burst"
    ),
    pause = list(
      truth = interval_truth$Event_Label == "Pause",
      pred = interval_truth$Pred_Pause_Target == "canonical_pause"
    ),
    contextual_separator = list(
      truth = contextual_truth,
      pred = interval_truth$Pred_Pause_Target == "contextual_separator"
    ),
    tonic = list(
      truth = interval_truth$State_Label == "Tonic" &
        interval_truth$Event_Label != "Pause",
      pred = interval_truth$Pred_Tonic == "tonic"
    ),
    broad_hfs = list(
      truth = interval_truth$State_Label == "Broad_HFS" &
        interval_truth$Event_Label != "Pause",
      pred = interval_truth$Pred_HFS == "high_frequency_spiking"
    )
  )
  for (target in names(target_contract)) {
    q <- target_contract[[target]]
    support_counts[[length(support_counts) + 1L]] <- count_binary(
      interval_truth, q$truth, q$pred, "mechanism", "strict_raw", target,
      "isi_support"
    )
  }
  tonic_phenotype_support <- interval_truth[
    tonic_phenotype_contract$score_keep, , drop = FALSE
  ]
  support_counts[[length(support_counts) + 1L]] <- count_binary(
    tonic_phenotype_support,
    tonic_phenotype_contract$truth_positive[
      tonic_phenotype_contract$score_keep
    ],
    tonic_phenotype_support$Pred_Tonic == "tonic",
    "phenotype", "eligible_primary", "tonic", "isi_support"
  )
  independent_null <-
    interval_truth$Phenotype_Candidate_Origin ==
      "null_model_burst_like_excursion" &
    interval_truth$Event_Label != "Burst"
  protocol_rows <- !independent_null
  support_counts[[length(support_counts) + 1L]] <- count_binary(
    interval_truth[protocol_rows, , drop = FALSE],
    interval_truth$Event_Label[protocol_rows] == "Burst",
    interval_truth$Pred_Event[protocol_rows] == "burst",
    "mechanism", "protocol_attributable", "burst", "isi_support"
  )

  phenotype_variants <- list(
    primary = list(
      keep = interval_truth$Phenotype_Label != "ambiguous_burst_like",
      positive = interval_truth$Phenotype_Label == "clear_burst_like"
    ),
    ambiguous_negative = list(
      keep = rep(TRUE, nrow(interval_truth)),
      positive = interval_truth$Phenotype_Label == "clear_burst_like"
    ),
    ambiguous_positive = list(
      keep = rep(TRUE, nrow(interval_truth)),
      positive = interval_truth$Phenotype_Label %in%
        c("clear_burst_like", "ambiguous_burst_like")
    )
  )
  for (variant in names(phenotype_variants)) {
    q <- phenotype_variants[[variant]]
    z <- interval_truth[q$keep, , drop = FALSE]
    support_counts[[length(support_counts) + 1L]] <- count_binary(
      z, q$positive[q$keep], z$Pred_Event == "burst",
      "phenotype", variant, "burst", "isi_support"
    )
  }
  support_counts <- do.call(rbind, support_counts)

  aggregate_episode <- function(z, id_column, label, target) {
    identifier <- as.character(z[[id_column]])
    keep <- !is.na(identifier) & identifier != "none" & nzchar(identifier) &
      !is.na(label) & as.logical(label)
    z <- z[keep, , drop = FALSE]
    if (!nrow(z)) return(data.frame())
    groups <- split(seq_len(nrow(z)), paste(z$Sample_ID, z[[id_column]], sep = "\r"))
    do.call(rbind, lapply(groups, function(index) data.frame(
      train = z$Sample_ID[index[1L]], target = target,
      start_isi = min(z$Right_Spike_Index[index]),
      end_isi = max(z$Right_Spike_Index[index]),
      stringsAsFactors = FALSE
    )))
  }
  truth_episodes <- rbind(
    aggregate_episode(
      interval_truth, "Event_ID", interval_truth$Event_Label == "Burst", "burst"
    ),
    aggregate_episode(
      interval_truth, "Event_ID",
      interval_truth$Event_Label == "Pause", "pause"
    ),
    aggregate_episode(
      interval_truth, "State_Envelope_ID",
      interval_truth$State_Label == "Tonic", "tonic"
    ),
    aggregate_episode(
      interval_truth, "State_Envelope_ID",
      interval_truth$State_Label == "Broad_HFS", "broad_hfs"
    )
  )
  contextual_episode_id <- if (has_multitrack_contextual) {
    "Contextual_Separator_ID"
  } else {
    "Event_ID"
  }
  truth_episodes <- rbind(
    truth_episodes,
    aggregate_episode(
      interval_truth, contextual_episode_id, contextual_truth,
      "contextual_separator"
    )
  )
  predicted_episodes_raw <- predicted_episodes_raw[
    predicted_episodes_raw$Train_ID %in% holdout_ids, , drop = FALSE
  ]
  predicted_episodes <- predicted_episodes_raw[
    (predicted_episodes_raw$Axis == "event" &
       predicted_episodes_raw$Pattern == "burst") |
      (predicted_episodes_raw$Axis == "state_strict" &
         predicted_episodes_raw$Pattern == "tonic") |
      (predicted_episodes_raw$Axis == "state_hf_family" &
         predicted_episodes_raw$Pattern == "high_frequency_spiking"),
    , drop = FALSE
  ]
  predicted_episodes$target <- ifelse(
    predicted_episodes$Pattern == "burst", "burst",
    ifelse(predicted_episodes$Pattern == "tonic", "tonic", "broad_hfs")
  )
  predicted_episodes$train <- predicted_episodes$Train_ID
  gap_episode <- predicted_gaps[predicted_gaps$train %in% holdout_ids, , drop = FALSE]
  if (nrow(gap_episode)) {
    gap_episode$target <- ifelse(
      gap_episode$predicted_pause_target == "canonical_pause",
      "pause", gap_episode$predicted_pause_target
    )
    gap_episode <- gap_episode[c("train", "target", "start_isi", "end_isi")]
    predicted_episodes <- rbind(
      predicted_episodes[c("train", "target", "start_isi", "end_isi")],
      gap_episode
    )
  } else {
    predicted_episodes <- predicted_episodes[c("train", "target", "start_isi", "end_isi")]
  }

  attach_episode_key <- function(z) {
    meta <- sample_key
    names(meta)[names(meta) == "Sample_ID"] <- "train"
    out <- merge(z, meta, by = "train", all.x = TRUE, sort = FALSE)
    if (nrow(out) && anyNA(out$Template_ID)) {
      stpd_v2_stop("An episode has no protected sample metadata.")
    }
    out
  }
  truth_episodes <- attach_episode_key(truth_episodes)
  predicted_episodes <- attach_episode_key(predicted_episodes)

  episode_counts_one <- function(pred, truth, estimand, variant, iou_min,
                                 targets) {
    rows <- list(); matches_out <- list()
    for (id in as.character(sample_key$Sample_ID)) for (target in targets) {
      p <- pred[pred$train == id & pred$target == target, , drop = FALSE]
      t <- truth[truth$train == id & truth$target == target, , drop = FALSE]
      matches <- if (nrow(p) && nrow(t)) stpd_match_events_optimal(
        p, t, class_col = "target", iou_min = iou_min
      ) else data.frame()
      tp <- nrow(matches)
      key_row <- sample_key[sample_key$Sample_ID == id, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        Estimand = estimand, Variant = variant,
        Batch_ID = key_row$Batch_ID, Scale_Factor = key_row$Scale_Factor,
        Level = "episode", IoU = sprintf("%.2f", iou_min), Target = target,
        Template_ID = key_row$Template_ID, Sample_ID = id,
        tp = tp, fp = nrow(p) - tp, fn = nrow(t) - tp,
        stringsAsFactors = FALSE
      )
      if (nrow(matches)) {
        q <- data.frame(
          Estimand = estimand, Variant = variant, Batch_ID = key_row$Batch_ID,
          Scale_Factor = key_row$Scale_Factor, Template_ID = key_row$Template_ID,
          Sample_ID = id, Target = target, IoU_Threshold = iou_min,
          IoU = matches$iou,
          Predicted_Start_ISI = p$start_isi[matches$pred_index],
          Predicted_End_ISI = p$end_isi[matches$pred_index],
          Truth_Start_ISI = t$start_isi[matches$truth_index],
          Truth_End_ISI = t$end_isi[matches$truth_index],
          stringsAsFactors = FALSE
        )
        q$Start_Boundary_Error_ISI <- q$Predicted_Start_ISI - q$Truth_Start_ISI
        q$End_Boundary_Error_ISI <- q$Predicted_End_ISI - q$Truth_End_ISI
        matches_out[[length(matches_out) + 1L]] <- q
      }
    }
    list(
      counts = do.call(rbind, rows),
      matches = if (length(matches_out)) do.call(rbind, matches_out) else data.frame()
    )
  }
  episode_counts <- list(); all_matches <- list()
  for (threshold in c(.25, .50, .75)) {
    q <- episode_counts_one(
      predicted_episodes, truth_episodes, "mechanism", "strict_raw", threshold,
      targets = names(target_contract)
    )
    episode_counts[[length(episode_counts) + 1L]] <- q$counts
    if (nrow(q$matches)) all_matches[[length(all_matches) + 1L]] <- q$matches
  }

  burst_episode_tables <- function(keep, truth_positive) {
    rows_pred <- list(); rows_truth <- list()
    for (id in holdout_ids) {
      index <- which(interval_truth$Sample_ID == id & keep)
      z <- interval_truth[index, , drop = FALSE]
      p <- stpd_v2_runs(z$Right_Spike_Index, z$Pred_Event == "burst")
      t <- stpd_v2_runs(z$Right_Spike_Index, truth_positive[index])
      meta <- sample_key[sample_key$Sample_ID == id, , drop = FALSE]
      if (nrow(p)) rows_pred[[length(rows_pred) + 1L]] <- data.frame(
        train = id, target = "burst", p, meta[rep(1L, nrow(p)), -1L, drop = FALSE]
      )
      if (nrow(t)) rows_truth[[length(rows_truth) + 1L]] <- data.frame(
        train = id, target = "burst", t, meta[rep(1L, nrow(t)), -1L, drop = FALSE]
      )
    }
    empty <- data.frame(
      train = character(), target = character(), start_isi = integer(),
      end_isi = integer(), Template_ID = character(),
      Scale_Factor = numeric(), Batch_ID = character(),
      stringsAsFactors = FALSE
    )
    list(
      pred = if (length(rows_pred)) do.call(rbind, rows_pred) else empty,
      truth = if (length(rows_truth)) do.call(rbind, rows_truth) else empty
    )
  }
  protocol_episode_tables <- burst_episode_tables(
    keep = !independent_null,
    truth_positive = interval_truth$Event_Label == "Burst"
  )
  for (threshold in c(.25, .50, .75)) {
    q <- episode_counts_one(
      protocol_episode_tables$pred, protocol_episode_tables$truth,
      "mechanism", "protocol_attributable", threshold, targets = "burst"
    )
    episode_counts[[length(episode_counts) + 1L]] <- q$counts
    if (nrow(q$matches)) all_matches[[length(all_matches) + 1L]] <- q$matches
  }
  for (variant in names(phenotype_variants)) {
    contract <- phenotype_variants[[variant]]
    tables <- burst_episode_tables(contract$keep, contract$positive)
    for (threshold in c(.25, .50, .75)) {
      q <- episode_counts_one(
        tables$pred, tables$truth, "phenotype", variant, threshold,
        targets = "burst"
      )
      episode_counts[[length(episode_counts) + 1L]] <- q$counts
      if (nrow(q$matches)) all_matches[[length(all_matches) + 1L]] <- q$matches
    }
  }
  tonic_phenotype_episode_tables <- stpd_v2_masked_episode_tables(
    interval_truth = interval_truth,
    sample_ids = holdout_ids,
    score_keep = tonic_phenotype_contract$score_keep,
    truth_positive = tonic_phenotype_contract$truth_positive,
    prediction_positive = interval_truth$Pred_Tonic == "tonic",
    target = "tonic"
  )
  for (threshold in c(.25, .50, .75)) {
    q <- episode_counts_one(
      tonic_phenotype_episode_tables$pred,
      tonic_phenotype_episode_tables$truth,
      "phenotype", "eligible_primary", threshold, targets = "tonic"
    )
    episode_counts[[length(episode_counts) + 1L]] <- q$counts
    if (nrow(q$matches)) all_matches[[length(all_matches) + 1L]] <- q$matches
  }
  episode_counts <- do.call(rbind, episode_counts)
  all_counts <- rbind(support_counts, episode_counts)
  expected_signatures <- rbind(
    expand.grid(
      Estimand = "mechanism", Variant = "strict_raw",
      Level = "isi_support", IoU = "support", Target = names(target_contract),
      stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "mechanism", Variant = "strict_raw", Level = "episode",
      IoU = sprintf("%.2f", c(.25, .50, .75)), Target = names(target_contract),
      stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "mechanism", Variant = "protocol_attributable",
      Level = c("isi_support"), IoU = "support", Target = "burst",
      stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "mechanism", Variant = "protocol_attributable",
      Level = "episode", IoU = sprintf("%.2f", c(.25, .50, .75)),
      Target = "burst", stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "phenotype", Variant = names(phenotype_variants),
      Level = "isi_support", IoU = "support", Target = "burst",
      stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "phenotype", Variant = names(phenotype_variants),
      Level = "episode", IoU = sprintf("%.2f", c(.25, .50, .75)),
      Target = "burst", stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "phenotype", Variant = "eligible_primary",
      Level = "isi_support", IoU = "support", Target = "tonic",
      stringsAsFactors = FALSE
    ),
    expand.grid(
      Estimand = "phenotype", Variant = "eligible_primary",
      Level = "episode", IoU = sprintf("%.2f", c(.25, .50, .75)),
      Target = "tonic", stringsAsFactors = FALSE
    )
  )
  all_counts <- stpd_v2_complete_counts(
    all_counts, sample_key, signatures = expected_signatures
  )
  if (anyNA(all_counts) || any(table(
    interaction(
      all_counts$Estimand, all_counts$Variant, all_counts$Level,
      all_counts$IoU, all_counts$Target, all_counts$Scale_Factor,
      drop = TRUE
    )
  ) != 15L)) {
    stpd_v2_stop("Completed metric counts do not have 15 templates per stratum.")
  }
  bootstrap_n <- as.integer(Sys.getenv("STPD_V2_BOOTSTRAP_N", unset = "2000"))
  bootstrap <- stpd_v2_bootstrap(all_counts, n_bootstrap = bootstrap_n)

  matches <- if (length(all_matches)) do.call(rbind, all_matches) else data.frame()
  boundary <- matches[matches$IoU_Threshold == .50, , drop = FALSE]
  overlap_diagnostics <- function(pred, truth, estimand, variant) {
    truth_rows <- lapply(seq_len(nrow(truth)), function(i) {
      p <- pred[pred$train == truth$train[i] & pred$target == truth$target[i], ]
      n <- if (nrow(p)) sum(stpd_v2_iou(
        p$start_isi, p$end_isi, truth$start_isi[i], truth$end_isi[i]
      ) > 0) else 0L
      data.frame(
        Estimand = estimand, Variant = variant, Kind = "truth_fragmentation",
        Batch_ID = truth$Batch_ID[i], Scale_Factor = truth$Scale_Factor[i],
        Template_ID = truth$Template_ID[i], Sample_ID = truth$train[i],
        Target = truth$target[i], Overlap_N = n, Excess_N = max(0L, n - 1L),
        Error = n > 1L, stringsAsFactors = FALSE
      )
    })
    pred_rows <- lapply(seq_len(nrow(pred)), function(i) {
      t <- truth[truth$train == pred$train[i] & truth$target == pred$target[i], ]
      n <- if (nrow(t)) sum(stpd_v2_iou(
        pred$start_isi[i], pred$end_isi[i], t$start_isi, t$end_isi
      ) > 0) else 0L
      data.frame(
        Estimand = estimand, Variant = variant, Kind = "prediction_merge",
        Batch_ID = pred$Batch_ID[i], Scale_Factor = pred$Scale_Factor[i],
        Template_ID = pred$Template_ID[i], Sample_ID = pred$train[i],
        Target = pred$target[i], Overlap_N = n, Excess_N = max(0L, n - 1L),
        Error = n > 1L, stringsAsFactors = FALSE
      )
    })
    do.call(rbind, c(truth_rows, pred_rows))
  }
  fragmentation <- overlap_diagnostics(
    predicted_episodes, truth_episodes, "mechanism", "strict_raw"
  )
  fragmentation <- rbind(
    fragmentation,
    overlap_diagnostics(
      protocol_episode_tables$pred, protocol_episode_tables$truth,
      "mechanism", "protocol_attributable"
    )
  )
  fragmentation_summary <- aggregate(
    cbind(Error = as.numeric(fragmentation$Error), Excess_N = fragmentation$Excess_N),
    fragmentation[c("Estimand", "Variant", "Kind", "Batch_ID", "Scale_Factor", "Target")],
    mean
  )

  state_description <- predicted_states[
    predicted_states$train %in% holdout_ids &
      predicted_states$state_family == "broad_high_frequency_state", , drop = FALSE
  ]
  if (nrow(state_description)) {
    subtype_key <- sample_key[c("Sample_ID", "Template_ID", "Scale_Factor")]
    state_description <- merge(
      state_description, subtype_key, by.x = "train", by.y = "Sample_ID",
      all.x = TRUE, sort = FALSE
    )
    predicted_subtypes <- aggregate(
      cbind(Episode_N = rep(1, nrow(state_description)), ISI_N = state_description$n_isi),
      state_description[c(
        "Batch_ID", "Scale_Factor", "state_frequency_class",
        "state_regularity_class", "state_subtype"
      )], sum
    )
    predicted_subtypes$Source <- "predicted_descriptive_only"
  } else predicted_subtypes <- data.frame()
  state_truth <- state_truth[state_truth$State_Label == "Broad_HFS", , drop = FALSE]
  state_truth$Batch_ID <- sample_key$Batch_ID[
    match(state_truth$Sample_ID, sample_key$Sample_ID)
  ]
  truth_subtypes <- aggregate(
    cbind(Episode_N = rep(1, nrow(state_truth)), ISI_N = state_truth$N_ISIs),
    state_truth[c("Batch_ID", "Scale_Factor", "Regularity_Regime")], sum
  )
  truth_subtypes$Source <- "generator_regime_descriptive_only"

  null_interval <- interval_truth[
    independent_null & !is.na(interval_truth$Phenotype_Evidence_ID) &
      interval_truth$Phenotype_Evidence_ID != "none" &
      nzchar(as.character(interval_truth$Phenotype_Evidence_ID)), , drop = FALSE
  ]
  null_groups <- split(
    seq_len(nrow(null_interval)),
    paste(null_interval$Sample_ID, null_interval$Phenotype_Evidence_ID, sep = "\r")
  )
  null_excursions <- if (length(null_groups)) do.call(rbind, lapply(
    null_groups, function(index) data.frame(
      Phenotype_Evidence_ID = null_interval$Phenotype_Evidence_ID[index[1L]],
      Sample_ID = null_interval$Sample_ID[index[1L]],
      Template_ID = null_interval$Template_ID[index[1L]],
      Scale_Factor = null_interval$Scale_Factor[index[1L]],
      Batch_ID = null_interval$Batch_ID[index[1L]],
      Start_Right_Spike_Index = min(null_interval$Right_Spike_Index[index]),
      End_Right_Spike_Index = max(null_interval$Right_Spike_Index[index]),
      Support_ISI_N = length(index), stringsAsFactors = FALSE
    )
  )) else data.frame()
  if (nrow(null_excursions)) {
    null_excursions$Predicted_Burst_Overlap <- vapply(seq_len(nrow(null_excursions)), function(i) {
      p <- predicted_episodes[
        predicted_episodes$train == null_excursions$Sample_ID[i] &
          predicted_episodes$target == "burst", , drop = FALSE
      ]
      nrow(p) > 0L && any(stpd_v2_iou(
        p$start_isi, p$end_isi,
        null_excursions$Start_Right_Spike_Index[i],
        null_excursions$End_Right_Spike_Index[i]
      ) > 0)
    }, logical(1))
  }
  protocol_diversion <- do.call(rbind, lapply(holdout_ids, function(id) {
    q <- interval_truth$Sample_ID == id
    null_index <- as.integer(interval_truth$Right_Spike_Index[q & independent_null])
    p <- predicted_episodes[
      predicted_episodes$train == id & predicted_episodes$target == "burst",
      , drop = FALSE
    ]
    touching <- if (nrow(p) && length(null_index)) vapply(
      seq_len(nrow(p)), function(i) any(
        null_index >= p$start_isi[i] & null_index <= p$end_isi[i]
      ), logical(1)
    ) else rep(FALSE, nrow(p))
    fully <- if (nrow(p) && length(null_index)) vapply(
      seq_len(nrow(p)), function(i) all(
        seq.int(p$start_isi[i], p$end_isi[i]) %in% null_index
      ), logical(1)
    ) else rep(FALSE, nrow(p))
    meta <- sample_key[sample_key$Sample_ID == id, , drop = FALSE]
    data.frame(
      meta, Diverted_Null_Support_ISI_N = length(null_index),
      Diverted_Predicted_Burst_Support_ISI_N = sum(
        q & independent_null & interval_truth$Pred_Event == "burst"
      ),
      Diverted_Null_Episode_N = length(unique(
        interval_truth$Phenotype_Evidence_ID[q & independent_null]
      )),
      Raw_Predicted_Burst_Episodes_Touching_Diverted_Support_N = sum(touching),
      Fully_Diverted_Predicted_Burst_Episode_N = sum(fully),
      stringsAsFactors = FALSE
    )
  }))

  # Canonical and complex Pause are one primary detector class.  Subtype
  # diagnostics therefore report recall/fragmentation, not artificial
  # subtype precision that the detector was never asked to predict.
  pause_truth <- interval_truth[
    interval_truth$Event_Label == "Pause" &
      interval_truth$Pause_Subtype %in% c("canonical", "complex_multi_gap"),
    , drop = FALSE
  ]
  pause_groups <- split(
    seq_len(nrow(pause_truth)),
    paste(pause_truth$Sample_ID, pause_truth$Event_ID, sep = "\r")
  )
  pause_subtype_episode <- do.call(rbind, lapply(pause_groups, function(index) {
    value <- pause_truth[index, , drop = FALSE]
    predicted <- value$Pred_Pause_Target == "canonical_pause"
    runs <- stpd_v2_runs(value$Right_Spike_Index, predicted)
    data.frame(
      Batch_ID = value$Batch_ID[[1L]],
      Scale_Factor = value$Scale_Factor[[1L]],
      Template_ID = value$Template_ID[[1L]],
      Sample_ID = value$Sample_ID[[1L]],
      Event_ID = value$Event_ID[[1L]],
      Pause_Subtype = value$Pause_Subtype[[1L]],
      Truth_ISI_N = nrow(value),
      Detected_ISI_N = sum(predicted),
      Support_Recall = mean(predicted),
      Any_Overlap = any(predicted),
      Predicted_Run_N = nrow(runs),
      Fragmented = nrow(runs) > 1L,
      stringsAsFactors = FALSE
    )
  }))
  pause_summary_one <- function(value) {
    data.frame(
      Truth_ISI_N = sum(value$Truth_ISI_N),
      Detected_ISI_N = sum(value$Detected_ISI_N),
      Support_Recall = sum(value$Detected_ISI_N) / sum(value$Truth_ISI_N),
      Truth_Episode_N = nrow(value),
      Overlapped_Episode_N = sum(value$Any_Overlap),
      Episode_Any_Overlap_Recall = mean(value$Any_Overlap),
      Fragmented_Episode_N = sum(value$Fragmented),
      Fragmentation_Rate_Among_Overlapped = if (any(value$Any_Overlap)) {
        mean(value$Fragmented[value$Any_Overlap])
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  pause_subtype_summary <- do.call(rbind, lapply(
    split(
      pause_subtype_episode,
      paste(
        pause_subtype_episode$Scale_Factor,
        pause_subtype_episode$Pause_Subtype,
        sep = "\r"
      )
    ),
    function(value) cbind(
      value[1L, c("Scale_Factor", "Pause_Subtype"), drop = FALSE],
      pause_summary_one(value)
    )
  ))
  pause_subtype_summary <- pause_subtype_summary[order(
    pause_subtype_summary$Scale_Factor,
    pause_subtype_summary$Pause_Subtype,
    method = "radix"
  ), , drop = FALSE]
  set.seed(20260828L)
  pause_clusters <- sort(unique(pause_subtype_episode$Template_ID), method = "radix")
  pause_bootstrap_rows <- vector("list", bootstrap_n)
  for (bootstrap_id in seq_len(bootstrap_n)) {
    sampled_clusters <- sample(
      pause_clusters, length(pause_clusters), replace = TRUE
    )
    sampled <- do.call(rbind, lapply(seq_along(sampled_clusters), function(i) {
      value <- pause_subtype_episode[
        pause_subtype_episode$Template_ID == sampled_clusters[[i]],
        , drop = FALSE
      ]
      value$Template_ID <- paste0(value$Template_ID, "__draw", i)
      value
    }))
    pause_bootstrap_rows[[bootstrap_id]] <- do.call(rbind, lapply(
      split(
        sampled,
        paste(sampled$Scale_Factor, sampled$Pause_Subtype, sep = "\r")
      ),
      function(value) cbind(
        value[1L, c("Scale_Factor", "Pause_Subtype"), drop = FALSE],
        pause_summary_one(value),
        Bootstrap_ID = bootstrap_id
      )
    ))
  }
  pause_bootstrap_draws <- do.call(rbind, pause_bootstrap_rows)
  pause_subtype_bootstrap <- do.call(rbind, lapply(
    seq_len(nrow(pause_subtype_summary)), function(i) {
      observed <- pause_subtype_summary[i, , drop = FALSE]
      draw <- pause_bootstrap_draws[
        pause_bootstrap_draws$Scale_Factor == observed$Scale_Factor &
          pause_bootstrap_draws$Pause_Subtype == observed$Pause_Subtype,
        , drop = FALSE
      ]
      metrics <- c(
        "Support_Recall", "Episode_Any_Overlap_Recall",
        "Fragmentation_Rate_Among_Overlapped"
      )
      do.call(rbind, lapply(metrics, function(metric) {
        value <- as.numeric(draw[[metric]])
        value <- value[is.finite(value)]
        data.frame(
          Scale_Factor = observed$Scale_Factor,
          Pause_Subtype = observed$Pause_Subtype,
          Metric = metric,
          Observed = as.numeric(observed[[metric]]),
          Bootstrap_N = length(value),
          CI_Low = if (length(value)) unname(stats::quantile(
            value, 0.025, type = 7
          )) else NA_real_,
          CI_High = if (length(value)) unname(stats::quantile(
            value, 0.975, type = 7
          )) else NA_real_,
          stringsAsFactors = FALSE
        )
      }))
    }
  ))

  outputs <- c(
    metrics = file.path(out_dir, "primary_observed_metrics.csv"),
    counts = file.path(out_dir, "template_cluster_counts.csv"),
    bootstrap = file.path(out_dir, "template_cluster_bootstrap_95ci.csv"),
    matches = file.path(out_dir, "episode_matches_iou_grid.csv"),
    boundary = file.path(out_dir, "boundary_errors_iou_050.csv"),
    fragmentation = file.path(out_dir, "fragmentation_merge_by_episode.csv"),
    fragmentation_summary = file.path(out_dir, "fragmentation_merge_summary.csv"),
    predicted_subtypes = file.path(out_dir, "hfs_predicted_subtypes_descriptive.csv"),
    truth_subtypes = file.path(out_dir, "hfs_generator_regimes_descriptive.csv"),
    null_excursions = file.path(out_dir, "hfs_null_excursion_audit.csv"),
    protocol_diversion = file.path(out_dir, "protocol_attribution_diversion.csv"),
    pause_subtype_episode = file.path(
      out_dir, "pause_subtype_episode_diagnostics.csv"
    ),
    pause_subtype_summary = file.path(
      out_dir, "pause_subtype_summary.csv"
    ),
    pause_subtype_bootstrap = file.path(
      out_dir, "pause_subtype_template_bootstrap_95ci.csv"
    )
  )
  stpd_v2_write_csv(bootstrap$observed, outputs[["metrics"]])
  stpd_v2_write_csv(all_counts, outputs[["counts"]])
  stpd_v2_write_csv(bootstrap$summary, outputs[["bootstrap"]])
  stpd_v2_write_csv(matches, outputs[["matches"]])
  stpd_v2_write_csv(boundary, outputs[["boundary"]])
  stpd_v2_write_csv(fragmentation, outputs[["fragmentation"]])
  stpd_v2_write_csv(fragmentation_summary, outputs[["fragmentation_summary"]])
  stpd_v2_write_csv(predicted_subtypes, outputs[["predicted_subtypes"]])
  stpd_v2_write_csv(truth_subtypes, outputs[["truth_subtypes"]])
  stpd_v2_write_csv(null_excursions, outputs[["null_excursions"]])
  stpd_v2_write_csv(protocol_diversion, outputs[["protocol_diversion"]])
  stpd_v2_write_csv(
    pause_subtype_episode, outputs[["pause_subtype_episode"]]
  )
  stpd_v2_write_csv(
    pause_subtype_summary, outputs[["pause_subtype_summary"]]
  )
  stpd_v2_write_csv(
    pause_subtype_bootstrap, outputs[["pause_subtype_bootstrap"]]
  )
  manifest <- stpd_v2_file_manifest("all_batches", unname(outputs), names(outputs))
  stpd_v2_write_csv(manifest, file.path(out_dir, "stage_c_manifest.csv"))
  cat("Stage C complete:", normalizePath(out_dir), "\n")
  invisible(list(counts = all_counts, metrics = bootstrap$observed))
}

if (sys.nframe() == 0L) stpd_v2_stage_c_main()
