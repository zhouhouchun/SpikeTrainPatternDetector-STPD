# Event-level validation and Basic-parameter sensitivity scanning.
# This module connects the manual-vs-detector event metrics with parameter
# dry-run comparisons. It is intentionally read-only for caller datasets.

stpd_event_level_empty_match_table <- function() {
  data.frame(
    match_status = character(),
    error_type = character(),
    train = character(),
    truth_pattern = character(),
    predicted_pattern = character(),
    truth_event_id = integer(),
    predicted_event_id = integer(),
    truth_start_isi = integer(),
    truth_end_isi = integer(),
    predicted_start_isi = integer(),
    predicted_end_isi = integer(),
    iou = numeric(),
    nearest_iou = numeric(),
    start_boundary_error_isi = integer(),
    end_boundary_error_isi = integer(),
    boundary_abs_error_isi = numeric(),
    stringsAsFactors = FALSE
  )
}

stpd_event_level_empty_validation_metrics <- function() {
  data.frame(
    split = character(), metric_mode = character(), pattern = character(),
    truth_n = integer(), predicted_n = integer(), true_positive_n = integer(),
    false_positive_n = integer(), false_negative_n = integer(),
    precision = numeric(), recall = numeric(), F1 = numeric(),
    precision_ci_low = numeric(), precision_ci_high = numeric(),
    recall_ci_low = numeric(), recall_ci_high = numeric(),
    F1_ci_low = numeric(), F1_ci_high = numeric(),
    ci_method = character(), ci_conf_level = numeric(),
    stringsAsFactors = FALSE
  )
}

stpd_event_level_nearest_any_label <- function(row, candidates) {
  if (is.null(row) || nrow(row) == 0 || is.null(candidates) || nrow(candidates) == 0) return(NULL)
  tr <- as.character(row$train[1] %||% "")
  cand <- candidates[as.character(candidates$train %||% "") == tr, , drop = FALSE]
  if (nrow(cand) == 0) return(NULL)
  iou <- stpd_event_iou(row$start_isi[1], row$end_isi[1], cand$start_isi, cand$end_isi)
  ok <- which(is.finite(iou))
  if (length(ok) == 0) return(NULL)
  best <- ok[which.max(iou[ok])]
  out <- cand[best, , drop = FALSE]
  out$.nearest_iou <- as.numeric(iou[best])
  out
}

stpd_event_level_match_table <- function(pred, truth, iou_min = 0.25) {
  empty <- stpd_event_level_empty_match_table()
  pred <- as.data.frame(pred %||% empty_events_tbl(), stringsAsFactors = FALSE)
  truth <- as.data.frame(truth %||% empty_events_tbl(), stringsAsFactors = FALSE)
  if (nrow(pred) > 0 && !("event_row_id" %in% names(pred))) pred$event_row_id <- seq_len(nrow(pred))
  if (nrow(truth) > 0 && !("event_row_id" %in% names(truth))) truth$event_row_id <- seq_len(nrow(truth))
  if (nrow(pred) == 0 && nrow(truth) == 0) return(empty)

  matches <- stpd_match_events_greedy(pred, truth, class_col = "pattern", iou_min = iou_min)
  used_pred <- integer()
  used_truth <- integer()
  rows <- list()

  add_row <- function(match_status, error_type, tr, truth_pattern, predicted_pattern,
                      truth_id, pred_id, ts, te, ps, pe, iou, nearest_iou) {
    truth_id <- suppressWarnings(as.integer(truth_id %||% NA_integer_))[1]
    pred_id <- suppressWarnings(as.integer(pred_id %||% NA_integer_))[1]
    ts <- suppressWarnings(as.integer(ts %||% NA_integer_))[1]
    te <- suppressWarnings(as.integer(te %||% NA_integer_))[1]
    ps <- suppressWarnings(as.integer(ps %||% NA_integer_))[1]
    pe <- suppressWarnings(as.integer(pe %||% NA_integer_))[1]
    iou <- suppressWarnings(as.numeric(iou %||% NA_real_))[1]
    nearest_iou <- suppressWarnings(as.numeric(nearest_iou %||% NA_real_))[1]
    start_err <- if (is.finite(ts) && is.finite(ps)) as.integer(ps - ts) else NA_integer_
    end_err <- if (is.finite(te) && is.finite(pe)) as.integer(pe - te) else NA_integer_
    boundary_abs <- if (is.finite(start_err) || is.finite(end_err)) {
      mean(abs(c(start_err, end_err)[is.finite(c(start_err, end_err))]))
    } else {
      NA_real_
    }
    data.frame(
      match_status = match_status,
      error_type = error_type,
      train = tr,
      truth_pattern = truth_pattern,
      predicted_pattern = predicted_pattern,
      truth_event_id = truth_id,
      predicted_event_id = pred_id,
      truth_start_isi = ts,
      truth_end_isi = te,
      predicted_start_isi = ps,
      predicted_end_isi = pe,
      iou = iou,
      nearest_iou = nearest_iou,
      start_boundary_error_isi = start_err,
      end_boundary_error_isi = end_err,
      boundary_abs_error_isi = boundary_abs,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(matches) && nrow(matches) > 0) {
    for (ii in seq_len(nrow(matches))) {
      pi <- as.integer(matches$pred_index[ii])
      ti <- as.integer(matches$truth_index[ii])
      if (!pi %in% seq_len(nrow(pred)) || !ti %in% seq_len(nrow(truth))) next
      used_pred <- c(used_pred, pi)
      used_truth <- c(used_truth, ti)
      rows[[length(rows) + 1L]] <- add_row(
        "true_positive", "matched_same_label",
        as.character(pred$train[pi] %||% truth$train[ti] %||% ""),
        as.character(truth$pattern[ti] %||% ""),
        as.character(pred$pattern[pi] %||% ""),
        truth$event_id[ti] %||% ti,
        pred$event_id[pi] %||% pi,
        truth$start_isi[ti], truth$end_isi[ti],
        pred$start_isi[pi], pred$end_isi[pi],
        matches$iou[ii], matches$iou[ii]
      )
    }
  }

  if (nrow(pred) > 0) {
    for (pi in setdiff(seq_len(nrow(pred)), used_pred)) {
      near <- stpd_event_level_nearest_any_label(pred[pi, , drop = FALSE], truth)
      has_near <- !is.null(near) && is.finite(near$.nearest_iou[1])
      near_iou <- if (has_near) near$.nearest_iou[1] else NA_real_
      err <- if (has_near && near_iou >= iou_min && !identical(as.character(near$pattern[1]), as.character(pred$pattern[pi]))) {
        "label_confusion"
      } else {
        "extra_detector_event"
      }
      use_near <- has_near && near_iou >= iou_min
      rows[[length(rows) + 1L]] <- add_row(
        "false_positive", err,
        as.character(pred$train[pi] %||% ""),
        if (use_near) as.character(near$pattern[1] %||% "") else "",
        as.character(pred$pattern[pi] %||% ""),
        if (use_near) near$event_id[1] %||% NA_integer_ else NA_integer_,
        pred$event_id[pi] %||% pi,
        if (use_near) near$start_isi[1] else NA_integer_,
        if (use_near) near$end_isi[1] else NA_integer_,
        pred$start_isi[pi], pred$end_isi[pi],
        NA_real_, near_iou
      )
    }
  }

  if (nrow(truth) > 0) {
    for (ti in setdiff(seq_len(nrow(truth)), used_truth)) {
      near <- stpd_event_level_nearest_any_label(truth[ti, , drop = FALSE], pred)
      has_near <- !is.null(near) && is.finite(near$.nearest_iou[1])
      near_iou <- if (has_near) near$.nearest_iou[1] else NA_real_
      err <- if (has_near && near_iou >= iou_min && !identical(as.character(near$pattern[1]), as.character(truth$pattern[ti]))) {
        "label_confusion"
      } else {
        "missed_manual_event"
      }
      use_near <- has_near && near_iou >= iou_min
      rows[[length(rows) + 1L]] <- add_row(
        "false_negative", err,
        as.character(truth$train[ti] %||% ""),
        as.character(truth$pattern[ti] %||% ""),
        if (use_near) as.character(near$pattern[1] %||% "") else "",
        truth$event_id[ti] %||% ti,
        if (use_near) near$event_id[1] %||% NA_integer_ else NA_integer_,
        truth$start_isi[ti], truth$end_isi[ti],
        if (use_near) near$start_isi[1] else NA_integer_,
        if (use_near) near$end_isi[1] else NA_integer_,
        NA_real_, near_iou
      )
    }
  }

  if (length(rows) == 0) return(empty)
  out <- dplyr::bind_rows(rows)
  out[order(out$train, out$truth_start_isi, out$predicted_start_isi, out$match_status), , drop = FALSE]
}

stpd_event_level_confusion_table <- function(matches, iou_min = 0.25) {
  if (is.null(matches) || nrow(matches) == 0) {
    return(data.frame(truth_pattern = character(), predicted_pattern = character(), n = integer(), stringsAsFactors = FALSE))
  }
  truth <- as.character(matches$truth_pattern %||% "")
  pred <- as.character(matches$predicted_pattern %||% "")
  truth[!nzchar(truth) | is.na(truth)] <- "none"
  pred[!nzchar(pred) | is.na(pred)] <- "none"
  tab <- as.data.frame(table(truth_pattern = truth, predicted_pattern = pred), stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 0, , drop = FALSE]
  names(tab)[names(tab) == "Freq"] <- "n"
  tab[order(tab$truth_pattern, tab$predicted_pattern), , drop = FALSE]
}

stpd_event_level_validation_not_estimable_result <- function(meta, split_table) {
  list(
    meta = meta,
    metrics = stpd_event_level_empty_validation_metrics(),
    confusion = data.frame(
      truth_pattern = character(), predicted_pattern = character(),
      n = integer(), stringsAsFactors = FALSE
    ),
    matches = stpd_event_level_empty_match_table(),
    truth_events = empty_events_tbl(),
    predicted_events = empty_events_tbl(),
    split = split_table,
    bootstrap_ci = data.frame(),
    bootstrap_replicates = data.frame(),
    score_calibration = data.frame(),
    score_calibration_summary = data.frame(),
    frozen_score_calibration = data.frame(),
    frozen_score_calibration_summary = data.frame(),
    frozen_score_calibrated_predictions = data.frame(),
    manual_uncertainty_boundary_sensitivity = data.frame(),
    manual_uncertainty_inter_rater = data.frame(),
    manual_uncertainty_meta = data.frame()
  )
}

stpd_event_level_validation_report <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                               iou_min = 0.25,
                                               metric_mode = c("strict_high_confidence", "candidate_family"),
                                               use_learned_ranges = FALSE,
                                               split_table = NULL,
                                               collect_diagnostics = FALSE,
                                               threshold_freeze = c("none", "calibration"),
                                               conf_level = 0.95,
                                               bootstrap_ci = FALSE,
                                               n_bootstrap = 200L,
                                               bootstrap_seed = NULL,
                                               score_calibration_bins = 10L,
                                               score_calibrator = c("platt", "isotonic", "none"),
                                               ambiguous_manual_labels = stpd_ambiguous_manual_labels(),
                                               exclude_ambiguous_manual = TRUE,
                                               iou_sensitivity_grid = c(0.10, 0.25, 0.50)) {
  metric_mode <- match.arg(metric_mode)
  threshold_freeze <- match.arg(threshold_freeze)
  score_calibrator <- match.arg(score_calibrator)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_event_level_validation_report(): ds must be a dataset with trains.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) stop("No target trains found for event-level validation.", call. = FALSE)
  params_eval <- if (isTRUE(use_learned_ranges)) params else strip_learned_ranges_for_eval(params)
  ds_eval <- if (isTRUE(use_learned_ranges)) ds else stpd_strip_learned_dataset_settings_for_eval(ds)
  iou_min <- suppressWarnings(as.numeric(iou_min %||% 0.25))
  if (!is.finite(iou_min)) iou_min <- 0.25
  iou_min <- max(0.01, min(1, iou_min))

  if (is.null(split_table)) {
    split_table <- data.frame(train = target, split = "all", stringsAsFactors = FALSE)
  } else {
    split_table <- as.data.frame(split_table, stringsAsFactors = FALSE)
    split_table <- split_table[as.character(split_table$train) %in% target, , drop = FALSE]
    if (nrow(split_table) == 0) split_table <- data.frame(train = target, split = "all", stringsAsFactors = FALSE)
  }
  split_table$split[is.na(split_table$split) | !nzchar(as.character(split_table$split))] <- "all"

  calibration_trains <- unique(as.character(
    split_table$train[as.character(split_table$split) == "calibration"]
  ))
  validation_trains <- unique(as.character(
    split_table$train[as.character(split_table$split) == "validation"]
  ))
  split_overlap <- intersect(calibration_trains, validation_trains)
  split_groups <- if ("validation_group" %in% names(split_table)) {
    as.character(split_table$validation_group)
  } else {
    paste0("train::", as.character(split_table$train))
  }
  missing_group <- is.na(split_groups) | !nzchar(trimws(split_groups))
  split_groups[missing_group] <- paste0("train::", as.character(split_table$train[missing_group]))
  calibration_groups <- unique(split_groups[as.character(split_table$split) == "calibration"])
  validation_groups <- unique(split_groups[as.character(split_table$split) == "validation"])
  split_group_overlap <- intersect(calibration_groups, validation_groups)
  validation_reason <- if (!identical(threshold_freeze, "calibration")) {
    "independent_split_not_required"
  } else if (length(calibration_trains) == 0L && length(validation_trains) == 0L) {
    "calibration_and_validation_splits_missing"
  } else if (length(calibration_trains) == 0L) {
    "calibration_split_missing"
  } else if (length(validation_trains) == 0L) {
    "validation_split_missing"
  } else if (length(split_overlap) > 0L) {
    "calibration_validation_train_overlap"
  } else if (length(split_group_overlap) > 0L) {
    "calibration_validation_group_overlap"
  } else {
    "independent_calibration_validation_split_available"
  }
  validation_status <- if (identical(threshold_freeze, "calibration") &&
                           !identical(validation_reason, "independent_calibration_validation_split_available")) {
    "not_estimable"
  } else {
    "estimable"
  }

  threshold_training_trains <- character()
  threshold_freeze_status <- if (identical(validation_status, "not_estimable")) {
    "skipped_not_estimable"
  } else {
    "not_requested"
  }
  if (identical(threshold_freeze, "calibration") && identical(validation_status, "estimable")) {
    threshold_training_trains <- calibration_trains
    params_eval <- stpd_freeze_thresholds_for_trains(
      ds_eval, params_eval,
      calibration_trains = threshold_training_trains,
      freeze_scope = "event_level_validation_calibration",
      label_blind = TRUE
    )
    threshold_freeze_status <- "frozen"
  }
  threshold_training_scope <- if (identical(validation_status, "not_estimable")) {
    "not_applicable_not_estimable"
  } else if (identical(threshold_freeze_status, "frozen")) {
    "calibration_only"
  } else if (isTRUE((effective_params_for_detector(params_eval)$detector %||% list())$freeze_dataset_thresholds %||% TRUE)) {
    "all_selected_transductive"
  } else {
    "pre_frozen_or_fixed"
  }

  truth_raw <- stpd_extract_events_by_source(ds, params_eval, source = "manual", selected_trains = target, metric_mode = metric_mode)
  truth_filter <- stpd_filter_ambiguous_events(truth_raw, ambiguous_labels = ambiguous_manual_labels)
  truth_all <- if (isTRUE(exclude_ambiguous_manual)) truth_filter$events else truth_raw
  meta <- data.frame(
    validation_run_id = paste0("event_validation_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    metric_mode = metric_mode,
    iou_min = iou_min,
    matching_rule = stpd_event_matching_rule(),
    selected_train_n = length(target),
    selected_trains = paste(target, collapse = ";"),
    validation_status = validation_status,
    validation_reason = validation_reason,
    calibration_train_n = length(calibration_trains),
    validation_train_n = length(validation_trains),
    calibration_validation_overlap_n = length(split_overlap),
    calibration_group_n = length(calibration_groups),
    validation_group_n = length(validation_groups),
    calibration_validation_group_overlap_n = length(split_group_overlap),
    detection_executed = identical(validation_status, "estimable"),
    label_blind_detection = TRUE,
    label_blind_threshold_estimation = TRUE,
    manual_event_n = nrow(truth_all %||% data.frame()),
    predicted_event_n = NA_integer_,
    learned_ranges_used = isTRUE(use_learned_ranges),
    threshold_freeze = threshold_freeze,
    threshold_freeze_status = threshold_freeze_status,
    threshold_training_scope = threshold_training_scope,
    threshold_training_train_n = length(threshold_training_trains),
    threshold_training_trains = paste(threshold_training_trains, collapse = ";"),
    ci_conf_level = suppressWarnings(as.numeric(conf_level %||% 0.95)),
    bootstrap_ci = isTRUE(bootstrap_ci),
    n_bootstrap = suppressWarnings(as.integer(n_bootstrap %||% 200L)),
    score_calibration_bins = suppressWarnings(as.integer(score_calibration_bins %||% 10L)),
    score_calibrator = score_calibrator,
    exclude_ambiguous_manual = isTRUE(exclude_ambiguous_manual),
    manual_event_n_total = nrow(truth_raw %||% data.frame()),
    manual_ambiguous_excluded_n = truth_filter$excluded_n,
    iou_sensitivity_grid = paste(iou_sensitivity_grid, collapse = ";"),
    interpretation = if (identical(validation_status, "not_estimable")) {
      paste0(
        "Performance is not estimable: ", validation_reason,
        ". Independent calibration and validation groups/trains are required when threshold_freeze='calibration'."
      )
    } else if (nrow(truth_all %||% data.frame()) == 0) {
      "No manual events available; event-level validation cannot estimate performance."
    } else {
      "Event-level IoU validation against manual labels; use false positives/negatives and boundary errors for parameter choice."
    },
    stringsAsFactors = FALSE
  )
  if (identical(validation_status, "not_estimable")) {
    return(stpd_event_level_validation_not_estimable_result(meta, split_table))
  }
  if (nrow(truth_all %||% data.frame()) == 0) {
    meta$predicted_event_n <- 0L
    meta$detection_executed <- FALSE
    empty_metrics <- data.frame(
      split = unique(as.character(split_table$split))[1] %||% "all",
      metric_mode = metric_mode,
      pattern = NA_character_,
      truth_n = 0L,
      predicted_n = NA_integer_,
      true_positive_n = NA_integer_,
      false_positive_n = NA_integer_,
      false_negative_n = NA_integer_,
      precision = NA_real_,
      recall = NA_real_,
      F1 = NA_real_,
      note = if (truth_filter$excluded_n > 0 && isTRUE(exclude_ambiguous_manual)) "Only ambiguous/manual_uncertain labels were present; primary validation metrics were not computed." else "No MANUAL labels available; event-level validation not computed.",
      stringsAsFactors = FALSE
    )
    return(list(
      meta = meta,
      metrics = empty_metrics,
      confusion = data.frame(),
      matches = stpd_event_level_empty_match_table(),
      truth_events = truth_all,
      predicted_events = data.frame(),
      split = split_table,
      bootstrap_ci = data.frame(),
      bootstrap_replicates = data.frame(),
      score_calibration = data.frame(),
      score_calibration_summary = data.frame(),
      frozen_score_calibration = data.frame(),
      frozen_score_calibration_summary = data.frame(),
      frozen_score_calibrated_predictions = data.frame(),
      manual_uncertainty_boundary_sensitivity = data.frame(),
      manual_uncertainty_inter_rater = data.frame(),
      manual_uncertainty_meta = data.frame()
    ))
  }

  pred_ds <- stpd_detect(
    ds_eval, params_eval, selected_trains = target, lock_manual = FALSE,
    collect_diagnostics = collect_diagnostics, label_blind = TRUE
  )
  pred_all <- stpd_extract_events_by_source(pred_ds, params_eval, source = "auto", selected_trains = target, metric_mode = metric_mode)
  meta$predicted_event_n <- nrow(pred_all %||% data.frame())
  splits <- unique(as.character(split_table$split))
  metric_rows <- list()
  match_rows <- list()
  bootstrap_summary_rows <- list()
  bootstrap_replicate_rows <- list()
  for (sp in splits) {
    tr <- as.character(split_table$train[as.character(split_table$split) == sp])
    truth <- truth_all[truth_all$train %in% tr, , drop = FALSE]
    pred <- pred_all[pred_all$train %in% tr, , drop = FALSE]
    mm <- stpd_event_level_metrics(pred, truth, class_col = "pattern", iou_min = iou_min)
    if (is.null(mm) || nrow(mm) == 0) {
      mm <- data.frame(pattern = NA_character_, truth_n = nrow(truth), predicted_n = nrow(pred),
                       true_positive_n = 0L, false_positive_n = nrow(pred), false_negative_n = nrow(truth),
                       precision = NA_real_, recall = NA_real_, F1 = NA_real_, stringsAsFactors = FALSE)
    }
    mm <- stpd_event_level_metrics_ci(mm, conf_level = conf_level)
    mm$split <- sp
    mm$metric_mode <- metric_mode
    if (isTRUE(bootstrap_ci) && nrow(mm) > 0) {
      seed_i <- if (is.null(bootstrap_seed)) NULL else suppressWarnings(as.integer(bootstrap_seed) + length(metric_rows))
      boot <- stpd_event_level_cluster_bootstrap(pred, truth, class_col = "pattern", iou_min = iou_min,
                                                n_bootstrap = n_bootstrap, seed = seed_i, conf_level = conf_level)
      bs <- boot$summary %||% data.frame()
      if (nrow(bs) > 0) {
        bs$split <- sp
        bs$metric_mode <- metric_mode
        bootstrap_summary_rows[[length(bootstrap_summary_rows) + 1L]] <- bs
        mm <- stpd_event_level_merge_bootstrap_ci(mm, bs)
      }
      br <- boot$bootstrap %||% data.frame()
      if (nrow(br) > 0) {
        br$split <- sp
        br$metric_mode <- metric_mode
        bootstrap_replicate_rows[[length(bootstrap_replicate_rows) + 1L]] <- br
      }
    }
    metric_rows[[length(metric_rows) + 1L]] <- mm
    mt <- stpd_event_level_match_table(pred, truth, iou_min = iou_min)
    if (nrow(mt) > 0) {
      mt$split <- sp
      mt$metric_mode <- metric_mode
      match_rows[[length(match_rows) + 1L]] <- mt
    }
  }
  metrics <- dplyr::bind_rows(metric_rows)
  metric_base_cols <- c("split", "metric_mode", "pattern", "truth_n", "predicted_n", "true_positive_n",
                        "false_positive_n", "false_negative_n", "precision", "recall", "F1",
                        "precision_ci_low", "precision_ci_high", "recall_ci_low", "recall_ci_high",
                        "F1_ci_low", "F1_ci_high", "precision_cluster_boot_ci_low",
                        "precision_cluster_boot_ci_high", "recall_cluster_boot_ci_low",
                        "recall_cluster_boot_ci_high", "F1_cluster_boot_ci_low",
                        "F1_cluster_boot_ci_high", "ci_method", "ci_conf_level")
  metrics <- metrics[, c(intersect(metric_base_cols, names(metrics)), setdiff(names(metrics), metric_base_cols)), drop = FALSE]
  matches <- if (length(match_rows) > 0) dplyr::bind_rows(match_rows) else stpd_event_level_empty_match_table()
  confusion <- stpd_event_level_confusion_table(matches, iou_min = iou_min)
  score_cal <- stpd_score_calibration(pred_all, truth_all, iou_min = iou_min, n_bins = score_calibration_bins, conf_level = conf_level)
  cal_tr <- as.character(split_table$train[as.character(split_table$split) == "calibration"])
  val_tr <- as.character(split_table$train[as.character(split_table$split) == "validation"])
  if (!identical(score_calibrator, "none") && length(cal_tr) > 0 && length(val_tr) > 0) {
    frozen_score_cal <- stpd_score_calibration_frozen(
      pred_all[pred_all$train %in% cal_tr, , drop = FALSE],
      truth_all[truth_all$train %in% cal_tr, , drop = FALSE],
      pred_all[pred_all$train %in% val_tr, , drop = FALSE],
      truth_all[truth_all$train %in% val_tr, , drop = FALSE],
      iou_min = iou_min,
      method = score_calibrator,
      n_bins = score_calibration_bins,
      conf_level = conf_level
    )
  } else {
    frozen_score_cal <- list(reliability = data.frame(), summary = data.frame(), calibrated_predictions = data.frame())
  }
  manual_unc <- stpd_manual_label_uncertainty_report(
    pred = pred_all,
    truth = truth_raw,
    iou_grid = iou_sensitivity_grid,
    iou_min = iou_min,
    ambiguous_labels = ambiguous_manual_labels,
    exclude_ambiguous = exclude_ambiguous_manual,
    conf_level = conf_level
  )
  list(meta = meta, metrics = metrics, confusion = confusion, matches = matches,
       truth_events = truth_all, predicted_events = pred_all, split = split_table,
       bootstrap_ci = if (length(bootstrap_summary_rows) > 0) dplyr::bind_rows(bootstrap_summary_rows) else data.frame(),
       bootstrap_replicates = if (length(bootstrap_replicate_rows) > 0) dplyr::bind_rows(bootstrap_replicate_rows) else data.frame(),
       score_calibration = score_cal$calibration,
       score_calibration_summary = score_cal$summary,
       frozen_score_calibration = frozen_score_cal$reliability,
       frozen_score_calibration_summary = frozen_score_cal$summary,
       frozen_score_calibrated_predictions = frozen_score_cal$calibrated_predictions,
       manual_uncertainty_boundary_sensitivity = manual_unc$boundary_sensitivity,
       manual_uncertainty_inter_rater = manual_unc$inter_rater,
       manual_uncertainty_meta = manual_unc$meta)
}

stpd_event_level_validation_export <- function(report, out_dir) {
  if (is.null(report) || !is.list(report)) stop("No event-level validation report is available to export.", call. = FALSE)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_safe(report$meta %||% data.frame(), file.path(out_dir, "Event_level_validation_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$metrics %||% data.frame(), file.path(out_dir, "Event_level_validation_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$matches %||% data.frame(), file.path(out_dir, "Manual_detector_event_matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$bootstrap_ci %||% data.frame(), file.path(out_dir, "Event_level_validation_cluster_bootstrap_ci.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$score_calibration %||% data.frame(), file.path(out_dir, "Event_level_validation_score_calibration.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$frozen_score_calibration %||% data.frame(), file.path(out_dir, "Event_level_validation_frozen_score_calibration.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$frozen_score_calibration_summary %||% data.frame(), file.path(out_dir, "Event_level_validation_frozen_score_calibration_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(report$manual_uncertainty_boundary_sensitivity %||% data.frame(), file.path(out_dir, "Event_level_validation_manual_boundary_sensitivity.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}

stpd_basic_sensitivity_paths <- function(max_params = 8L) {
  schema <- tryCatch(stpd_contract_ui_schema(ui_level = "basic"), error = function(e) data.frame())
  if (is.null(schema) || nrow(schema) == 0) return(character())
  legacy_diagnostic <- if (exists("stpd_contract_ui_legacy_diagnostic_paths", mode = "function")) {
    stpd_contract_ui_legacy_diagnostic_paths()
  } else {
    c("burst.T_seed", "burst.T_bridge", "highfreq.T_high_max", "highfreq.spiking_max_ISI_abs")
  }
  schema <- schema[!(as.character(schema$path) %in% legacy_diagnostic), , drop = FALSE]
  schema <- schema[as.character(schema$type) %in% c("numeric", "integer"), , drop = FALSE]
  # The automatic threshold resolver normally supplies seed/bridge/contrast,
  # tonic-band, and pause-band values from USER/MANUAL/histogram sources. Those
  # schema inputs remain valid default fallbacks, but varying them in a default
  # sensitivity run would often be a scientific no-op. Prefer controls that are
  # consumed directly by the active event grammar regardless of source mode.
  preferred <- c(
    "event_core.boundary_floor_sec",
    "event_core.min_seed_isi_count",
    "event_core.max_bridge_isi_count",
    "event_core.max_bridge_isi_fraction",
    "event_core.max_expansion_isi_each_side",
    "event_core.possible_burst_contrast_min",
    "burst.G_min",
    "tonic.G_min"
  )
  paths <- unique(c(preferred[preferred %in% schema$path], as.character(schema$path)))
  max_params <- suppressWarnings(as.integer(max_params %||% length(paths)))
  if (!is.finite(max_params) || max_params < 1L) max_params <- length(paths)
  head(paths, max_params)
}

# Advanced/Expert parameters are intentionally unavailable to the generic
# sensitivity scan.  Tonic regularity is the one narrow exception because the
# product exposes a threshold-preview workflow for CV/LV/MM tuning.  Keep this
# exact allowlist frozen so selecting this scope cannot silently become an
# unrestricted Expert-parameter sweep when the YAML contract grows.
stpd_tonic_sensitivity_paths <- function(max_params = 6L) {
  allowed <- c(
    "tonic.LV_core",
    "tonic.tonic_mm_min",
    "tonic.tonic_mm_max",
    "tonic.tonic_mm_relax_lv_max",
    "tonic.tonic_mm_relax_cv_max",
    "tonic.tonic_mm_relaxed_max"
  )
  schema <- tryCatch(
    stpd_contract_ui_schema(
      groups = "tonic", ui_level = c("advanced", "expert")
    ),
    error = function(e) data.frame()
  )
  if (is.null(schema) || nrow(schema) == 0L) return(character())
  paths <- allowed[allowed %in% as.character(schema$path)]
  max_params <- suppressWarnings(as.integer(max_params %||% length(paths)))
  if (!is.finite(max_params) || max_params < 1L) max_params <- length(paths)
  head(paths, max_params)
}

stpd_parameter_sensitivity_schema <- function(
    path_scope = c("basic", "tonic_regularization")) {
  path_scope <- match.arg(path_scope)
  schema <- if (identical(path_scope, "tonic_regularization")) {
    tryCatch(
      stpd_contract_ui_schema(
        groups = "tonic", ui_level = c("advanced", "expert")
      ),
      error = function(e) data.frame()
    )
  } else {
    tryCatch(
      stpd_contract_ui_schema(ui_level = "basic"),
      error = function(e) data.frame()
    )
  }
  if (is.null(schema) || nrow(schema) == 0L) return(data.frame())
  schema <- schema[
    as.character(schema$type) %in% c("numeric", "integer", "logical"),
    , drop = FALSE
  ]
  if (identical(path_scope, "tonic_regularization")) {
    allowed <- stpd_tonic_sensitivity_paths(max_params = 6L)
    schema <- schema[as.character(schema$path) %in% allowed, , drop = FALSE]
    schema <- schema[match(allowed, as.character(schema$path)), , drop = FALSE]
    schema <- schema[!is.na(schema$path), , drop = FALSE]
  }
  schema
}

stpd_parameter_sensitivity_values <- function(params, schema_row, relative_step = 0.25) {
  path <- as.character(schema_row$path[1] %||% "")
  typ <- as.character(schema_row$type[1] %||% "")
  cur <- stpd_get_param(params, path, stpd_schema_value(schema_row))
  rel <- suppressWarnings(as.numeric(relative_step %||% 0.25))
  if (!is.finite(rel) || rel <= 0) rel <- 0.25
  rel <- min(rel, 0.95)
  minv <- suppressWarnings(as.numeric(schema_row$min[1]))
  maxv <- suppressWarnings(as.numeric(schema_row$max[1]))
  step <- suppressWarnings(as.numeric(schema_row$step[1]))
  if (typ %in% c("numeric", "integer")) {
    x <- suppressWarnings(as.numeric(cur[1]))
    if (!is.finite(x)) x <- suppressWarnings(as.numeric(schema_row$default[1]))
    if (!is.finite(x)) x <- 0
    delta <- if (abs(x) > 0) abs(x) * rel else if (is.finite(step) && step > 0) step else rel
    vals <- c(x - delta, x + delta)
    if (is.finite(minv)) vals <- pmax(vals, minv)
    if (is.finite(maxv)) vals <- pmin(vals, maxv)
    if (typ == "integer") vals <- round(vals)
    vals <- unique(vals[is.finite(vals) & vals != x])
    if (length(vals) == 0 && is.finite(step) && step > 0) {
      vals <- unique(c(x + step, x - step))
      if (is.finite(minv)) vals <- pmax(vals, minv)
      if (is.finite(maxv)) vals <- pmin(vals, maxv)
      if (typ == "integer") vals <- round(vals)
      vals <- unique(vals[is.finite(vals) & vals != x])
    }
    return(vals)
  }
  if (typ == "logical") return(!isTRUE(cur))
  NULL
}

stpd_parameter_sensitivity_scientific_direction <- function(
    path, baseline_value, variant_value, direction = "") {
  path <- as.character(path %||% "")[1L]
  base <- suppressWarnings(as.numeric(baseline_value))[1L]
  value <- suppressWarnings(as.numeric(variant_value))[1L]
  if (!is.finite(base) || !is.finite(value) || identical(base, value)) {
    return(as.character(direction %||% "unchanged")[1L])
  }
  upper_ceiling <- c(
    "tonic.LV_core",
    "tonic.tonic_mm_max",
    "tonic.tonic_mm_relax_lv_max",
    "tonic.tonic_mm_relax_cv_max",
    "tonic.tonic_mm_relaxed_max"
  )
  lower_floor <- "tonic.tonic_mm_min"
  if (path %in% upper_ceiling) {
    return(if (value > base) "relaxed" else "tightened")
  }
  if (path %in% lower_floor) {
    return(if (value < base) "relaxed" else "tightened")
  }
  as.character(direction %||% if (value > base) "increase" else "decrease")[1L]
}

stpd_parameter_sensitivity_context_hash <- function(
    ds, params, selected_trains, use_learned_ranges = FALSE) {
  target <- intersect(
    as.character(selected_trains %||% names(ds$trains %||% list())),
    names(ds$trains %||% list())
  )
  effective <- tryCatch(
    effective_params_for_detector(params),
    error = function(e) params
  )
  payload <- list(
    dataset_name = as.character((ds$meta %||% list())$display_name %||%
                                  (ds$meta %||% list())$name %||% ""),
    selected_trains = target,
    trains = (ds$trains %||% list())[target],
    configured_params = params,
    effective_params = effective,
    use_learned_ranges = isTRUE(use_learned_ranges),
    learned_ranges = if (isTRUE(use_learned_ranges)) ds$params_est %||% NULL else NULL
  )
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

stpd_metric_macro_summary <- function(metrics) {
  if (is.null(metrics) || nrow(metrics) == 0) {
    return(list(macro_precision = NA_real_, macro_recall = NA_real_, macro_F1 = NA_real_))
  }
  out <- list(
    macro_precision = mean(suppressWarnings(as.numeric(metrics$precision)), na.rm = TRUE),
    macro_recall = mean(suppressWarnings(as.numeric(metrics$recall)), na.rm = TRUE),
    macro_F1 = mean(suppressWarnings(as.numeric(metrics$F1)), na.rm = TRUE)
  )
  for (nm in names(out)) if (is.nan(out[[nm]])) out[[nm]] <- NA_real_
  out
}

stpd_validation_nonempty_character <- function(x) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(trimws(out))] <- NA_character_
  out
}

stpd_validation_metadata_frame <- function(metadata, target) {
  if (is.null(metadata)) return(data.frame())
  out <- tryCatch(as.data.frame(metadata, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(out) == 0L) return(data.frame())
  if (!("train" %in% names(out)) && !is.null(rownames(out))) out$train <- rownames(out)
  if (!("train" %in% names(out))) return(data.frame())
  out$train <- as.character(out$train)
  out <- out[out$train %in% target & !duplicated(out$train), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_validation_overlay_metadata <- function(out, metadata) {
  if (is.null(metadata) || nrow(metadata) == 0L || !("train" %in% names(metadata))) return(out)
  position <- match(as.character(out$train), as.character(metadata$train))
  for (column in setdiff(names(metadata), "train")) {
    incoming <- metadata[[column]][position]
    if (!(column %in% names(out))) {
      out[[column]] <- incoming
      next
    }
    usable <- !is.na(incoming)
    if (is.character(incoming)) usable <- usable & nzchar(trimws(incoming))
    out[[column]][usable] <- incoming[usable]
  }
  out
}

stpd_validation_find_column <- function(x, candidates) {
  if (is.null(x) || length(names(x)) == 0L) return(NA_character_)
  lower <- tolower(names(x))
  for (candidate in candidates) {
    hit <- which(lower == tolower(candidate))
    if (length(hit) > 0L) return(names(x)[hit[1L]])
  }
  NA_character_
}

stpd_validation_group_values_from_table <- function(metadata, target, candidates) {
  empty <- list(values = rep(NA_character_, length(target)), column = NA_character_)
  if (is.null(metadata) || nrow(metadata) == 0L) return(empty)
  column <- stpd_validation_find_column(metadata, candidates)
  if (is.na(column)) return(empty)
  position <- match(target, as.character(metadata$train))
  list(
    values = stpd_validation_nonempty_character(metadata[[column]][position]),
    column = column
  )
}

stpd_validation_group_values_from_trains <- function(ds, target, candidates) {
  values <- rep(NA_character_, length(target))
  columns <- rep(NA_character_, length(target))
  for (i in seq_along(target)) {
    dat <- ds$trains[[target[i]]]
    if (!is.data.frame(dat) || nrow(dat) == 0L) next
    column <- stpd_validation_find_column(dat, candidates)
    if (is.na(column)) next
    observed <- unique(stpd_validation_nonempty_character(dat[[column]]))
    observed <- observed[!is.na(observed)]
    if (length(observed) > 1L) {
      stop(
        paste0("Grouping metadata column '", column, "' is not constant within train '", target[i], "'."),
        call. = FALSE
      )
    }
    if (length(observed) == 1L) {
      values[i] <- observed
      columns[i] <- column
    }
  }
  list(values = values, column = columns)
}

stpd_validation_group_values_from_dataset <- function(ds, target, candidates) {
  values <- rep(NA_character_, length(target))
  column <- stpd_validation_find_column(ds$meta %||% list(), candidates)
  if (is.na(column)) return(list(values = values, column = NA_character_))
  value <- stpd_validation_nonempty_character((ds$meta %||% list())[[column]])
  value <- value[!is.na(value)]
  if (length(value) > 0L) values[] <- value[1L]
  list(values = values, column = column)
}

stpd_validation_resolve_groups <- function(ds, target, metadata = NULL, group_col = NULL) {
  explicit_metadata <- stpd_validation_metadata_frame(metadata, target)
  dataset_metadata <- stpd_validation_metadata_frame((ds$meta %||% list())$train_metadata %||% NULL, target)
  if (!is.null(group_col)) {
    if (length(group_col) != 1L || is.na(group_col) || !nzchar(trimws(as.character(group_col)))) {
      stop("group_col must be NULL or one non-empty column name.", call. = FALSE)
    }
    requested <- as.character(group_col)
    candidates <- unique(c(requested, paste0("meta_", requested)))
    choices <- list(list(label = requested, candidates = candidates))
  } else {
    choices <- list(
      list(label = "subject", candidates = c("subject", "subject_id", "meta_subject")),
      list(label = "animal", candidates = c("animal", "animal_id", "meta_animal")),
      list(label = "session", candidates = c("session", "session_id", "recording_session", "meta_session"))
    )
  }

  resolved <- NULL
  for (choice in choices) {
    values <- rep(NA_character_, length(target))
    sources <- rep(NA_character_, length(target))
    source_columns <- rep(NA_character_, length(target))
    table_sources <- list(
      explicit_metadata = explicit_metadata,
      dataset_train_metadata = dataset_metadata
    )
    for (source_name in names(table_sources)) {
      incoming <- stpd_validation_group_values_from_table(
        table_sources[[source_name]], target, choice$candidates
      )
      fill <- is.na(values) & !is.na(incoming$values)
      values[fill] <- incoming$values[fill]
      sources[fill] <- source_name
      source_columns[fill] <- incoming$column
    }
    incoming_train <- stpd_validation_group_values_from_trains(ds, target, choice$candidates)
    fill <- is.na(values) & !is.na(incoming_train$values)
    values[fill] <- incoming_train$values[fill]
    sources[fill] <- "train_column"
    source_columns[fill] <- incoming_train$column[fill]
    incoming_dataset <- stpd_validation_group_values_from_dataset(ds, target, choice$candidates)
    fill <- is.na(values) & !is.na(incoming_dataset$values)
    values[fill] <- incoming_dataset$values[fill]
    sources[fill] <- "dataset_meta"
    source_columns[fill] <- incoming_dataset$column
    if (any(!is.na(values))) {
      resolved <- list(
        label = choice$label, values = values,
        sources = sources, source_columns = source_columns
      )
      break
    }
  }

  if (is.null(resolved)) {
    if (!is.null(group_col)) {
      stop(paste0("group_col '", as.character(group_col), "' was not found in dataset/train metadata."), call. = FALSE)
    }
    resolved <- list(
      label = "train", values = rep(NA_character_, length(target)),
      sources = rep(NA_character_, length(target)),
      source_columns = rep(NA_character_, length(target))
    )
  }
  missing <- is.na(resolved$values)
  resolved$values[missing] <- target[missing]
  resolved$sources[missing] <- "fallback_train"
  resolved$source_columns[missing] <- "train"
  prefix <- ifelse(resolved$sources == "fallback_train", "train", resolved$label)
  data.frame(
    train = target,
    validation_group = paste0(prefix, "::", resolved$values),
    validation_group_value = resolved$values,
    validation_group_col = rep(resolved$label, length(target)),
    validation_group_source = resolved$sources,
    validation_group_source_col = resolved$source_columns,
    stringsAsFactors = FALSE
  )
}

stpd_train_validation_strata <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                         metadata = NULL, strata_cols = NULL,
                                         metric_mode = c("strict_high_confidence", "candidate_family"),
                                         group_col = NULL) {
  metric_mode <- match.arg(metric_mode)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_train_validation_strata(): ds must be a dataset with trains.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) return(data.frame())
  ev <- stpd_extract_events_by_source(ds, params, source = "manual", selected_trains = target, metric_mode = metric_mode)
  rows <- lapply(target, function(tr) {
    dat <- ds$trains[[tr]]
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec %||% NA_real_))
    duration <- if (length(ts) > 1 && any(is.finite(ts))) max(ts, na.rm = TRUE) - min(ts, na.rm = TRUE) else NA_real_
    ev_tr <- ev[as.character(ev$train) == tr, , drop = FALSE]
    pat <- sort(unique(as.character(ev_tr$pattern %||% character())))
    pat <- pat[nzchar(pat) & !is.na(pat)]
    data.frame(
      train = tr,
      spike_n = nrow(dat),
      duration_sec = duration,
      manual_event_n = nrow(ev_tr),
      pattern_signature = if (length(pat) > 0) paste(pat, collapse = "+") else "none",
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  dur <- suppressWarnings(as.numeric(out$duration_sec))
  if (sum(is.finite(dur)) >= 3 && length(unique(dur[is.finite(dur)])) >= 3) {
    qs <- unique(as.numeric(stats::quantile(dur, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, type = 7)))
    labs <- c("short", "medium", "long")[seq_len(max(1, length(qs) - 1L))]
    out$length_bin <- as.character(cut(dur, breaks = qs, include.lowest = TRUE, labels = labs))
    out$length_bin[is.na(out$length_bin)] <- "unknown_length"
  } else {
    out$length_bin <- "all_lengths"
  }
  dataset_metadata <- stpd_validation_metadata_frame((ds$meta %||% list())$train_metadata %||% NULL, target)
  explicit_metadata <- stpd_validation_metadata_frame(metadata, target)
  out <- stpd_validation_overlay_metadata(out, dataset_metadata)
  out <- stpd_validation_overlay_metadata(out, explicit_metadata)
  groups <- stpd_validation_resolve_groups(ds, target, metadata = metadata, group_col = group_col)
  out <- stpd_validation_overlay_metadata(out, groups)
  if (is.null(strata_cols)) {
    strata_cols <- c(intersect(c("nucleus", "condition"), names(out)),
                     "pattern_signature", "length_bin")
  }
  strata_cols <- intersect(as.character(strata_cols), names(out))
  if (length(strata_cols) == 0) strata_cols <- "pattern_signature"
  out$stratum <- apply(out[, strata_cols, drop = FALSE], 1, function(x) {
    x <- as.character(x); x[is.na(x) | !nzchar(x)] <- "NA"
    paste(x, collapse = "|")
  })
  out$strata_cols <- paste(strata_cols, collapse = ";")
  out[match(target, out$train), , drop = FALSE]
}

stpd_validation_groups_from_strata <- function(strata_table, group_col = NULL) {
  st <- as.data.frame(strata_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(st) == 0L || !("train" %in% names(st))) return(st)
  st$train <- as.character(st$train)

  requested <- NULL
  if (!is.null(group_col)) {
    if (length(group_col) != 1L || is.na(group_col) || !nzchar(trimws(as.character(group_col)))) {
      stop("group_col must be NULL or one non-empty column name.", call. = FALSE)
    }
    requested <- stpd_validation_find_column(st, as.character(group_col))
    if (is.na(requested)) {
      stop(paste0("group_col '", as.character(group_col), "' was not found in strata_table."), call. = FALSE)
    }
  } else if ("validation_group" %in% names(st)) {
    requested <- "validation_group"
  } else {
    automatic_candidates <- list(
      c("subject", "subject_id", "meta_subject"),
      c("animal", "animal_id", "meta_animal"),
      c("session", "session_id", "recording_session", "meta_session")
    )
    for (candidates in automatic_candidates) {
      candidate <- stpd_validation_find_column(st, candidates)
      if (!is.na(candidate) && any(!is.na(stpd_validation_nonempty_character(st[[candidate]])))) {
        requested <- candidate
        break
      }
    }
  }

  if (is.null(requested) || is.na(requested)) {
    values <- st$train
    identifiers <- paste0("train::", values)
    resolved_col <- "train"
    source <- rep("fallback_train", nrow(st))
  } else {
    values <- stpd_validation_nonempty_character(st[[requested]])
    missing <- is.na(values)
    values[missing] <- st$train[missing]
    if (identical(requested, "validation_group")) {
      identifiers <- values
      fallback_identifier <- paste0("train::", st$train)
      identifiers[missing] <- fallback_identifier[missing]
      resolved_col <- as.character(st$validation_group_col %||% "validation_group")
      if (length(resolved_col) == 1L) resolved_col <- rep(resolved_col, nrow(st))
      source <- as.character(st$validation_group_source %||% "strata_table")
      if (length(source) == 1L) source <- rep(source, nrow(st))
      source[missing] <- "fallback_train"
    } else {
      identifiers <- paste0(requested, "::", values)
      identifiers[missing] <- paste0("train::", st$train[missing])
      resolved_col <- rep(requested, nrow(st))
      source <- rep("strata_table", nrow(st))
      source[missing] <- "fallback_train"
    }
  }

  st$validation_group <- identifiers
  if (!("validation_group_value" %in% names(st)) || !identical(requested, "validation_group")) {
    st$validation_group_value <- values
  }
  if (!("validation_group_col" %in% names(st)) || !identical(requested, "validation_group")) {
    st$validation_group_col <- resolved_col
  }
  if (!("validation_group_source" %in% names(st)) || !identical(requested, "validation_group")) {
    st$validation_group_source <- source
  }
  if (!("validation_group_source_col" %in% names(st))) {
    st$validation_group_source_col <- ifelse(source == "fallback_train", "train", resolved_col)
  }
  st
}

stpd_stratified_train_splits <- function(strata_table, validation_fraction = 0.25,
                                         n_repeats = 50L, seed = 1L,
                                         min_validation_trains = 1L,
                                         group_col = NULL) {
  st <- as.data.frame(strata_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(st) == 0 || !("train" %in% names(st))) return(data.frame())
  if (!("manual_event_n" %in% names(st))) st$manual_event_n <- 1L
  if (!("stratum" %in% names(st))) st$stratum <- "all"
  st <- stpd_validation_groups_from_strata(st, group_col = group_col)
  st$stratum <- stpd_validation_nonempty_character(st$stratum)
  st$stratum[is.na(st$stratum)] <- "NA"
  validation_fraction <- suppressWarnings(as.numeric(validation_fraction %||% 0.25))
  if (!is.finite(validation_fraction) || validation_fraction <= 0 || validation_fraction >= 1) validation_fraction <- 0.25
  n_repeats <- suppressWarnings(as.integer(n_repeats %||% 50L))
  if (!is.finite(n_repeats) || n_repeats < 1L) n_repeats <- 50L
  min_validation_trains <- suppressWarnings(as.integer(min_validation_trains %||% 1L))
  if (!is.finite(min_validation_trains) || min_validation_trains < 1L) min_validation_trains <- 1L
  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }
  eligible_row <- suppressWarnings(as.numeric(st$manual_event_n)) > 0
  eligible_row[is.na(eligible_row)] <- FALSE
  eligible_groups <- sort(unique(as.character(st$validation_group[eligible_row])))
  group_stratum <- vapply(eligible_groups, function(group) {
    strata <- sort(unique(as.character(st$stratum[eligible_row & st$validation_group == group])))
    paste(strata, collapse = "||")
  }, character(1))
  group_table <- data.frame(
    validation_group = eligible_groups,
    group_stratum = unname(group_stratum),
    stringsAsFactors = FALSE
  )
  group_stratum_by_row <- group_table$group_stratum[match(st$validation_group, group_table$validation_group)]
  group_stratum_by_row[is.na(group_stratum_by_row)] <- "unlabeled"
  st$group_stratum <- group_stratum_by_row

  eligibility_reason <- if (length(eligible_groups) == 0L) {
    "no_labeled_groups"
  } else if (length(eligible_groups) < 2L) {
    "insufficient_independent_groups"
  } else {
    "independent_group_holdout_available"
  }
  rows <- vector("list", n_repeats)
  for (rr in seq_len(n_repeats)) {
    val_groups <- character()
    if (identical(eligibility_reason, "independent_group_holdout_available")) {
      for (ss in sort(unique(group_table$group_stratum))) {
        members <- sort(as.character(group_table$validation_group[group_table$group_stratum == ss]))
        if (length(members) >= 2L) {
          n_val <- max(1L, floor(length(members) * validation_fraction))
          n_val <- min(n_val, length(members) - 1L)
          val_groups <- c(val_groups, sample(members, n_val))
        }
      }
      needed <- min(
        max(min_validation_trains, floor(length(eligible_groups) * validation_fraction)),
        length(eligible_groups) - 1L
      )
      val_groups <- unique(val_groups)
      if (length(val_groups) < needed) {
        pool <- sort(setdiff(eligible_groups, val_groups))
        if (length(pool) > 0L) {
          val_groups <- c(val_groups, sample(pool, min(needed - length(val_groups), length(pool))))
        }
      }
      if (length(val_groups) == 0L) val_groups <- sample(eligible_groups, 1L)
      if (length(val_groups) >= length(eligible_groups)) val_groups <- val_groups[-1L]
    }

    sp <- st
    sp$repeat_id <- rr
    if (!identical(eligibility_reason, "independent_group_holdout_available")) {
      sp$split <- ifelse(eligible_row, "not_estimable", "unlabeled")
      split_status <- "not_estimable"
      split_reason <- eligibility_reason
    } else {
      sp$split <- ifelse(
        !eligible_row,
        "unlabeled",
        ifelse(sp$validation_group %in% val_groups, "validation", "calibration")
      )
      cal_groups <- unique(sp$validation_group[sp$split == "calibration"])
      observed_val_groups <- unique(sp$validation_group[sp$split == "validation"])
      invalid <- length(cal_groups) == 0L || length(observed_val_groups) == 0L ||
        length(intersect(cal_groups, observed_val_groups)) > 0L
      if (invalid) {
        sp$split[eligible_row] <- "not_estimable"
        split_status <- "not_estimable"
        split_reason <- "independent_group_split_failed"
      } else {
        split_status <- "estimable"
        split_reason <- eligibility_reason
      }
    }
    cal_groups <- unique(sp$validation_group[sp$split == "calibration"])
    observed_val_groups <- unique(sp$validation_group[sp$split == "validation"])
    sp$split_status <- split_status
    sp$split_reason <- split_reason
    sp$eligible_group_n <- length(eligible_groups)
    sp$calibration_group_n <- length(cal_groups)
    sp$validation_group_n <- length(observed_val_groups)
    fallback_only <- all(as.character(sp$validation_group_source[eligible_row]) == "fallback_train")
    sp$split_note <- if (!identical(split_status, "estimable")) {
      paste0("not_estimable:", split_reason)
    } else if (isTRUE(fallback_only)) {
      "stratified_train_holdout"
    } else {
      "stratified_group_holdout"
    }
    rows[[rr]] <- sp
  }
  out <- dplyr::bind_rows(rows)
  out[order(out$repeat_id, out$split, out$group_stratum, out$stratum, out$train), , drop = FALSE]
}

stpd_train_macro_metrics_from_report <- function(report, iou_min = 0.25) {
  pred_all <- as.data.frame(report$predicted_events %||% data.frame(), stringsAsFactors = FALSE)
  truth_all <- as.data.frame(report$truth_events %||% data.frame(), stringsAsFactors = FALSE)
  trains <- sort(unique(c(as.character(pred_all$train %||% character()), as.character(truth_all$train %||% character()))))
  trains <- trains[nzchar(trains) & !is.na(trains)]
  if (length(trains) == 0) return(data.frame(train = character(), macro_precision = numeric(), macro_recall = numeric(), macro_F1 = numeric(), stringsAsFactors = FALSE))
  dplyr::bind_rows(lapply(trains, function(tr) {
    mm <- stpd_event_level_metrics(pred_all[pred_all$train == tr, , drop = FALSE],
                                   truth_all[truth_all$train == tr, , drop = FALSE],
                                   iou_min = iou_min)
    macro <- stpd_metric_macro_summary(mm)
    data.frame(train = tr, macro_precision = macro$macro_precision,
               macro_recall = macro$macro_recall, macro_F1 = macro$macro_F1,
               stringsAsFactors = FALSE)
  }))
}

stpd_signflip_pvalue <- function(diff, n_permutations = 999L, seed = NULL) {
  diff <- suppressWarnings(as.numeric(diff))
  diff <- diff[is.finite(diff)]
  if (length(diff) < 2L) return(NA_real_)
  obs <- abs(mean(diff))
  if (!is.finite(obs)) return(NA_real_)
  if (all(abs(diff) < .Machine$double.eps)) return(1)
  n_permutations <- suppressWarnings(as.integer(n_permutations %||% 999L))
  if (!is.finite(n_permutations) || n_permutations < 1L) n_permutations <- 999L
  n <- length(diff)
  if (!is.null(seed)) set.seed(seed)
  if (n <= 12L && 2^n <= n_permutations + 1L) {
    signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), n)))
  } else {
    signs <- matrix(sample(c(-1, 1), n * n_permutations, replace = TRUE), ncol = n)
  }
  stats <- abs(rowMeans(sweep(signs, 2, diff, `*`)))
  (1 + sum(stats >= obs, na.rm = TRUE)) / (length(stats) + 1)
}

stpd_parameter_sensitivity_adjustment <- function(summary, train_metrics,
                                                  baseline_variant_id = "baseline_current",
                                                  alpha = 0.05,
                                                  effect_threshold = 0.02,
                                                  p_adjust_method = "BH",
                                                  n_permutations = 999L,
                                                  seed = 1L) {
  sm <- as.data.frame(summary %||% data.frame(), stringsAsFactors = FALSE)
  tm <- as.data.frame(train_metrics %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(sm) == 0 || nrow(tm) == 0 || !all(c("variant_id", "train", "macro_F1") %in% names(tm))) {
    sm$sensitivity_raw_p_value <- NA_real_
    sm$sensitivity_q_value <- NA_real_
    sm$robust_parameter_flag <- ifelse(as.character(sm$variant_id %||% "") == baseline_variant_id, "baseline", "not_tested")
    return(list(summary = sm, tests = data.frame()))
  }
  base <- tm[as.character(tm$variant_id) == baseline_variant_id, c("train", "macro_F1"), drop = FALSE]
  names(base)[names(base) == "macro_F1"] <- "baseline_macro_F1"
  tests <- lapply(seq_len(nrow(sm)), function(ii) {
    vid <- as.character(sm$variant_id[ii])
    if (identical(vid, baseline_variant_id)) {
      return(data.frame(variant_id = vid, paired_train_n = nrow(base), mean_train_delta_macro_F1 = 0,
                        sensitivity_raw_p_value = NA_real_, stringsAsFactors = FALSE))
    }
    vv <- tm[as.character(tm$variant_id) == vid, c("train", "macro_F1"), drop = FALSE]
    names(vv)[names(vv) == "macro_F1"] <- "variant_macro_F1"
    m <- merge(base, vv, by = "train", all = FALSE)
    diff <- suppressWarnings(as.numeric(m$variant_macro_F1 - m$baseline_macro_F1))
    data.frame(variant_id = vid, paired_train_n = length(diff),
               mean_train_delta_macro_F1 = if (length(diff) > 0) mean(diff, na.rm = TRUE) else NA_real_,
               sensitivity_raw_p_value = stpd_signflip_pvalue(diff, n_permutations = n_permutations, seed = seed + ii),
               stringsAsFactors = FALSE)
  })
  tests <- dplyr::bind_rows(tests)
  is_test <- is.finite(tests$sensitivity_raw_p_value)
  tests$sensitivity_q_value <- NA_real_
  tests$sensitivity_q_value[is_test] <- stats::p.adjust(tests$sensitivity_raw_p_value[is_test], method = p_adjust_method)
  sm <- merge(sm, tests, by = "variant_id", all.x = TRUE, sort = FALSE)
  delta <- suppressWarnings(as.numeric(sm$delta_macro_F1_vs_baseline %||% sm$mean_train_delta_macro_F1))
  qv <- suppressWarnings(as.numeric(sm$sensitivity_q_value))
  sm$robust_parameter_flag <- ifelse(as.character(sm$variant_id) == baseline_variant_id, "baseline",
    ifelse(!is.finite(qv), "insufficient_train_pairs",
      ifelse(qv <= alpha & abs(delta) >= effect_threshold, "sensitive_after_BH_FDR",
        ifelse(abs(delta) < effect_threshold, "stable_within_effect_threshold", "not_significant_after_BH_FDR"))))
  sm$multiple_correction_method <- p_adjust_method
  sm$multiple_correction_alpha <- alpha
  sm$effect_threshold_macro_F1 <- effect_threshold
  list(summary = sm, tests = tests)
}

stpd_parameter_variant_grid <- function(params, paths = NULL, max_params = 4L,
                                        relative_step = 0.25,
                                        path_scope = c("basic", "tonic_regularization")) {
  path_scope <- match.arg(path_scope)
  schema <- stpd_parameter_sensitivity_schema(path_scope)
  if (is.null(paths) || length(paths) == 0) {
    paths <- if (identical(path_scope, "tonic_regularization")) {
      stpd_tonic_sensitivity_paths(max_params = max_params)
    } else {
      stpd_basic_sensitivity_paths(max_params = max_params)
    }
  }
  if (identical(path_scope, "tonic_regularization")) {
    invalid <- setdiff(as.character(paths), as.character(schema$path))
    if (length(invalid)) {
      stop(
        "Tonic regularization sensitivity paths are not allowlisted: ",
        paste(invalid, collapse = ", "), call. = FALSE
      )
    }
  }
  paths <- head(intersect(as.character(paths), as.character(schema$path)), max_params)
  variants <- list(list(variant_id = "baseline_current", parameter_path = "", parameter_label = "Current UI parameters",
                        baseline_value = "", variant_value = "", direction = "baseline", params = params))
  for (path in paths) {
    sr <- schema[as.character(schema$path) == path, , drop = FALSE]
    if (nrow(sr) == 0) next
    base_val <- stpd_get_param(params, path, stpd_schema_value(sr[1, , drop = FALSE]))
    vals <- stpd_parameter_sensitivity_values(params, sr[1, , drop = FALSE], relative_step = relative_step)
    for (vv in vals) {
      pp <- stpd_set_param(params, path, if (identical(as.character(sr$type[1]), "integer")) as.integer(vv) else vv)
      # Contract UI paths are compatibility aliases for the canonical
      # spiketrainpattern namespace. Mirror the edited alias exactly as the
      # main UI does; otherwise the later canonical productizer silently
      # restores the old value and the sensitivity variant is a no-op.
      if (exists("stpd_productize_params", mode = "function")) {
        pp <- stpd_productize_params(pp, prefer = "legacy")
      }
      variants[[length(variants) + 1L]] <- list(
        variant_id = paste0(gsub("[^A-Za-z0-9_]+", "_", path), "_", length(variants)),
        parameter_path = path,
        parameter_label = as.character(sr$label[1] %||% path),
        baseline_value = as.character(base_val),
        variant_value = as.character(vv),
        direction = if (is.logical(vv)) if (isTRUE(vv)) "toggle_on" else "toggle_off" else if (suppressWarnings(as.numeric(vv)) > suppressWarnings(as.numeric(base_val))) "increase" else "decrease",
        params = pp
      )
    }
  }
  variants
}

stpd_nested_tuning_not_estimable <- function(params, reason, inner_split = data.frame(),
                                              selection_metric = "macro_F1") {
  tuning_table <- data.frame(
    variant_id = "baseline_current",
    parameter_path = "",
    parameter_label = "Current UI parameters",
    baseline_value = "",
    variant_value = "",
    direction = "baseline",
    macro_precision = NA_real_,
    macro_recall = NA_real_,
    macro_F1 = NA_real_,
    selected = TRUE,
    selection_metric = selection_metric,
    validation_status = "not_estimable",
    validation_reason = reason,
    note = paste0("Inner tuning was not estimable: ", reason, ". Baseline parameters were retained."),
    stringsAsFactors = FALSE
  )
  list(
    params = params,
    selected_variant_id = "baseline_current",
    tuning_table = tuning_table,
    inner_split = inner_split,
    validation_status = "not_estimable",
    validation_reason = reason
  )
}

stpd_nested_tune_params <- function(ds, params, calibration_trains,
                                    paths = NULL, max_params = 4L, relative_step = 0.25,
                                    inner_validation_fraction = 0.25, seed = 1L,
                                    iou_min = 0.25,
                                    metric_mode = c("strict_high_confidence", "candidate_family"),
                                    use_learned_ranges = FALSE,
                                    collect_diagnostics = FALSE,
                                    selection_metric = c("macro_F1", "macro_recall", "macro_precision"),
                                    metadata = NULL,
                                    strata_cols = NULL,
                                    group_col = NULL) {
  metric_mode <- match.arg(metric_mode)
  selection_metric <- match.arg(selection_metric)
  calibration_trains <- intersect(as.character(calibration_trains), names(ds$trains))
  if (length(calibration_trains) == 0L) {
    return(stpd_nested_tuning_not_estimable(
      params, "no_calibration_trains", selection_metric = selection_metric
    ))
  }
  strata <- stpd_train_validation_strata(
    ds, params, selected_trains = calibration_trains,
    metadata = metadata, strata_cols = strata_cols,
    metric_mode = metric_mode, group_col = group_col
  )
  split <- stpd_stratified_train_splits(
    strata, validation_fraction = inner_validation_fraction,
    n_repeats = 1L, seed = seed, group_col = "validation_group"
  )
  split_status <- unique(as.character(split$split_status %||% character()))
  split_reason <- unique(as.character(split$split_reason %||% character()))
  calibration_groups <- unique(as.character(split$validation_group[split$split == "calibration"] %||% character()))
  validation_groups <- unique(as.character(split$validation_group[split$split == "validation"] %||% character()))
  invalid <- nrow(split) == 0L || !identical(split_status, "estimable") ||
    length(calibration_groups) == 0L || length(validation_groups) == 0L ||
    length(intersect(calibration_groups, validation_groups)) > 0L
  if (isTRUE(invalid)) {
    reason <- split_reason[nzchar(split_reason) & !is.na(split_reason)][1]
    if (is.na(reason) || length(reason) == 0L) reason <- "inner_independent_group_split_unavailable"
    return(stpd_nested_tuning_not_estimable(
      params, reason, inner_split = split, selection_metric = selection_metric
    ))
  }
  variants <- stpd_parameter_variant_grid(params, paths = paths, max_params = max_params, relative_step = relative_step)
  report_split <- split[, intersect(c("train", "split", "validation_group"), names(split)), drop = FALSE]
  rows <- lapply(variants, function(vv) {
    rep <- stpd_event_level_validation_report(
      ds, vv$params, selected_trains = calibration_trains, split_table = report_split,
      iou_min = iou_min, metric_mode = metric_mode, use_learned_ranges = use_learned_ranges,
      threshold_freeze = "calibration", collect_diagnostics = collect_diagnostics,
      score_calibrator = "none"
    )
    mm <- as.data.frame(rep$metrics %||% data.frame(), stringsAsFactors = FALSE)
    if (nrow(mm) > 0L && "split" %in% names(mm)) {
      mm <- mm[as.character(mm$split) == "validation", , drop = FALSE]
    } else {
      mm <- data.frame()
    }
    macro <- stpd_metric_macro_summary(mm)
    data.frame(variant_id = vv$variant_id, parameter_path = vv$parameter_path,
               parameter_label = vv$parameter_label, baseline_value = vv$baseline_value,
               variant_value = vv$variant_value, direction = vv$direction,
               macro_precision = macro$macro_precision, macro_recall = macro$macro_recall,
               macro_F1 = macro$macro_F1, stringsAsFactors = FALSE)
  })
  tab <- dplyr::bind_rows(rows)
  score <- suppressWarnings(as.numeric(tab[[selection_metric]]))
  score[!is.finite(score)] <- -Inf
  best <- if (all(!is.finite(score))) 1L else which.max(score)
  tab$selected <- seq_len(nrow(tab)) == best
  tab$selection_metric <- selection_metric
  tab$validation_status <- "estimable"
  tab$validation_reason <- "independent_group_holdout_available"
  tab$note <- ifelse(tab$selected, "Selected using the independent inner validation groups.", "")
  list(params = variants[[best]]$params, selected_variant_id = variants[[best]]$variant_id,
       tuning_table = tab, inner_split = split,
       validation_status = "estimable",
       validation_reason = "independent_group_holdout_available")
}

stpd_repeated_holdout_empty_metrics <- function() {
  data.frame(
    split = character(), metric_mode = character(), pattern = character(),
    truth_n = integer(), predicted_n = integer(), true_positive_n = integer(),
    false_positive_n = integer(), false_negative_n = integer(),
    precision = numeric(), recall = numeric(), F1 = numeric(),
    repeat_id = integer(), selected_variant_id = character(),
    stringsAsFactors = FALSE
  )
}

stpd_repeated_holdout_empty_repeat_summary <- function() {
  data.frame(
    repeat_id = integer(), validation_status = character(), validation_reason = character(),
    calibration_train_n = integer(), validation_train_n = integer(),
    calibration_group_n = integer(), validation_group_n = integer(),
    calibration_validation_group_overlap_n = integer(),
    selected_variant_id = character(), nested_tuning_status = character(),
    nested_tuning_reason = character(), macro_precision = numeric(),
    macro_recall = numeric(), macro_F1 = numeric(), split_note = character(),
    stringsAsFactors = FALSE
  )
}

stpd_repeated_holdout_empty_summary <- function() {
  data.frame(
    pattern = character(), repeat_n = integer(), mean_precision = numeric(),
    mean_recall = numeric(), mean_F1 = numeric(), sd_F1 = numeric(),
    q05_F1 = numeric(), q50_F1 = numeric(), q95_F1 = numeric(),
    stringsAsFactors = FALSE
  )
}

stpd_repeated_train_holdout_validation <- function(ds, params = default_params_sec(),
                                                   selected_trains = NULL,
                                                   metadata = NULL,
                                                   strata_cols = NULL,
                                                   n_repeats = 50L,
                                                   validation_fraction = 0.25,
                                                   seed = 1L,
                                                   nested_tuning = FALSE,
                                                   tuning_paths = NULL,
                                                   max_tuning_params = 4L,
                                                   relative_step = 0.25,
                                                   iou_min = 0.25,
                                                   metric_mode = c("strict_high_confidence", "candidate_family"),
                                                   use_learned_ranges = FALSE,
                                                   threshold_freeze = c("calibration", "none"),
                                                   collect_diagnostics = FALSE,
                                                   group_col = NULL) {
  metric_mode <- match.arg(metric_mode)
  threshold_freeze <- match.arg(threshold_freeze)
  strata <- stpd_train_validation_strata(ds, params, selected_trains = selected_trains,
                                         metadata = metadata, strata_cols = strata_cols,
                                         metric_mode = metric_mode, group_col = group_col)
  splits <- stpd_stratified_train_splits(strata, validation_fraction = validation_fraction,
                                         n_repeats = n_repeats, seed = seed,
                                         group_col = "validation_group")
  metric_rows <- list(); repeat_rows <- list(); tuning_rows <- list()
  repeat_ids <- if ("repeat_id" %in% names(splits)) sort(unique(splits$repeat_id)) else integer()
  for (rr in repeat_ids) {
    sp <- splits[splits$repeat_id == rr, , drop = FALSE]
    target <- unique(as.character(sp$train[sp$split %in% c("calibration", "validation")]))
    cal <- unique(as.character(sp$train[sp$split == "calibration"]))
    val <- unique(as.character(sp$train[sp$split == "validation"]))
    cal_groups <- unique(as.character(sp$validation_group[sp$split == "calibration"]))
    val_groups <- unique(as.character(sp$validation_group[sp$split == "validation"]))
    group_overlap <- intersect(cal_groups, val_groups)
    split_status <- unique(as.character(sp$split_status %||% character()))
    split_status <- split_status[nzchar(split_status) & !is.na(split_status)]
    split_reason <- unique(as.character(sp$split_reason %||% character()))
    split_reason <- split_reason[nzchar(split_reason) & !is.na(split_reason)]
    invalid_split <- length(split_status) != 1L || !identical(split_status, "estimable") ||
      length(cal) == 0L || length(val) == 0L ||
      length(cal_groups) == 0L || length(val_groups) == 0L || length(group_overlap) > 0L
    if (isTRUE(invalid_split)) {
      reason <- split_reason[1]
      if (length(reason) == 0L || is.na(reason)) reason <- "independent_group_split_unavailable"
      repeat_rows[[length(repeat_rows) + 1L]] <- data.frame(
        repeat_id = as.integer(rr), validation_status = "not_estimable",
        validation_reason = reason,
        calibration_train_n = length(cal), validation_train_n = length(val),
        calibration_group_n = length(cal_groups), validation_group_n = length(val_groups),
        calibration_validation_group_overlap_n = length(group_overlap),
        selected_variant_id = "baseline_current",
        nested_tuning_status = if (isTRUE(nested_tuning)) "not_run" else "not_requested",
        nested_tuning_reason = if (isTRUE(nested_tuning)) "outer_split_not_estimable" else "not_requested",
        macro_precision = NA_real_, macro_recall = NA_real_, macro_F1 = NA_real_,
        split_note = paste(unique(as.character(sp$split_note %||% "")), collapse = ";"),
        stringsAsFactors = FALSE
      )
      next
    }
    tuned <- if (isTRUE(nested_tuning)) {
      stpd_nested_tune_params(ds, params, cal, paths = tuning_paths, max_params = max_tuning_params,
                              relative_step = relative_step, seed = seed + rr,
                              iou_min = iou_min, metric_mode = metric_mode,
                              use_learned_ranges = use_learned_ranges,
                              collect_diagnostics = collect_diagnostics,
                              metadata = metadata, strata_cols = strata_cols,
                              group_col = group_col)
    } else {
      list(params = params, selected_variant_id = "baseline_current", tuning_table = data.frame(),
           validation_status = "not_requested", validation_reason = "not_requested")
    }
    split_table <- sp[, intersect(c("train", "split", "validation_group"), names(sp)), drop = FALSE]
    rep <- stpd_event_level_validation_report(
      ds, tuned$params, selected_trains = target, split_table = split_table,
      iou_min = iou_min, metric_mode = metric_mode,
      use_learned_ranges = use_learned_ranges,
      threshold_freeze = threshold_freeze,
      collect_diagnostics = collect_diagnostics,
      score_calibrator = "platt"
    )
    report_status <- as.character(rep$meta$validation_status[1] %||% "estimable")
    report_reason <- as.character(rep$meta$validation_reason[1] %||% "independent_group_holdout_available")
    val_metrics <- as.data.frame(rep$metrics %||% data.frame(), stringsAsFactors = FALSE)
    if (nrow(val_metrics) > 0L && "split" %in% names(val_metrics)) {
      val_metrics <- val_metrics[as.character(val_metrics$split) == "validation", , drop = FALSE]
    } else {
      val_metrics <- data.frame()
    }
    if (nrow(val_metrics) > 0) {
      val_metrics$repeat_id <- rr
      val_metrics$selected_variant_id <- tuned$selected_variant_id
      metric_rows[[length(metric_rows) + 1L]] <- val_metrics
    }
    macro <- stpd_metric_macro_summary(val_metrics)
    repeat_rows[[length(repeat_rows) + 1L]] <- data.frame(
      repeat_id = rr,
      validation_status = report_status,
      validation_reason = report_reason,
      calibration_train_n = length(cal),
      validation_train_n = length(val),
      calibration_group_n = length(cal_groups),
      validation_group_n = length(val_groups),
      calibration_validation_group_overlap_n = length(group_overlap),
      selected_variant_id = tuned$selected_variant_id,
      nested_tuning_status = as.character(tuned$validation_status %||% "not_requested"),
      nested_tuning_reason = as.character(tuned$validation_reason %||% "not_requested"),
      macro_precision = macro$macro_precision,
      macro_recall = macro$macro_recall,
      macro_F1 = macro$macro_F1,
      split_note = paste(unique(sp$split_note), collapse = ";"),
      stringsAsFactors = FALSE
    )
    tt <- tuned$tuning_table %||% data.frame()
    if (nrow(tt) > 0) {
      tt$repeat_id <- rr
      tuning_rows[[length(tuning_rows) + 1L]] <- tt
    }
  }
  metrics <- if (length(metric_rows) > 0) dplyr::bind_rows(metric_rows) else stpd_repeated_holdout_empty_metrics()
  repeats <- if (length(repeat_rows) > 0) dplyr::bind_rows(repeat_rows) else stpd_repeated_holdout_empty_repeat_summary()
  summary <- if (nrow(metrics) > 0) {
    dplyr::bind_rows(lapply(split(metrics, as.character(metrics$pattern)), function(mm) {
      f1 <- suppressWarnings(as.numeric(mm$F1))
      data.frame(pattern = as.character(mm$pattern[1]), repeat_n = length(unique(mm$repeat_id)),
                 mean_precision = mean(suppressWarnings(as.numeric(mm$precision)), na.rm = TRUE),
                 mean_recall = mean(suppressWarnings(as.numeric(mm$recall)), na.rm = TRUE),
                 mean_F1 = mean(f1, na.rm = TRUE),
                 sd_F1 = stats::sd(f1, na.rm = TRUE),
                 q05_F1 = stats::quantile(f1, 0.05, na.rm = TRUE, names = FALSE, type = 6),
                 q50_F1 = stats::quantile(f1, 0.50, na.rm = TRUE, names = FALSE, type = 6),
                 q95_F1 = stats::quantile(f1, 0.95, na.rm = TRUE, names = FALSE, type = 6),
                 stringsAsFactors = FALSE)
    }))
  } else stpd_repeated_holdout_empty_summary()
  estimable_repeat_n <- if (nrow(repeats) > 0L) sum(as.character(repeats$validation_status) == "estimable") else 0L
  validation_status <- if (estimable_repeat_n > 0L) "estimable" else "not_estimable"
  validation_reason <- if (identical(validation_status, "estimable")) {
    "independent_group_holdout_available"
  } else {
    reasons <- unique(as.character(repeats$validation_reason %||% character()))
    reasons <- reasons[nzchar(reasons) & !is.na(reasons)]
    if (length(reasons) > 0L) paste(reasons, collapse = ";") else "independent_group_split_unavailable"
  }
  eligible_groups <- if (nrow(strata) > 0L && all(c("validation_group", "manual_event_n") %in% names(strata))) {
    unique(as.character(strata$validation_group[suppressWarnings(as.numeric(strata$manual_event_n)) > 0]))
  } else {
    character()
  }
  resolved_group_col <- unique(as.character(strata$validation_group_col %||% character()))
  resolved_group_col <- resolved_group_col[nzchar(resolved_group_col) & !is.na(resolved_group_col)]
  group_sources <- unique(as.character(strata$validation_group_source %||% character()))
  group_sources <- group_sources[nzchar(group_sources) & !is.na(group_sources)]
  list(
    meta = data.frame(validation_run_id = paste0("repeated_holdout_", format(Sys.time(), "%Y%m%d_%H%M%S")),
                      n_repeats = n_repeats, validation_fraction = validation_fraction,
                      nested_tuning = isTRUE(nested_tuning), iou_min = iou_min,
                      metric_mode = metric_mode,
                      matching_rule = stpd_event_matching_rule(),
                      validation_status = validation_status,
                      validation_reason = validation_reason,
                      estimable_repeat_n = estimable_repeat_n,
                      eligible_group_n = length(eligible_groups),
                      requested_group_col = as.character(group_col %||% "auto"),
                      resolved_group_col = paste(resolved_group_col, collapse = ";"),
                      validation_group_sources = paste(group_sources, collapse = ";"),
                      stringsAsFactors = FALSE),
    strata = strata,
    splits = splits,
    repeat_metrics = metrics,
    repeat_summary = repeats,
    summary = summary,
    tuning = if (length(tuning_rows) > 0) dplyr::bind_rows(tuning_rows) else data.frame()
  )
}

stpd_parameter_sensitivity_scan <- function(ds, params_current = default_params_sec(),
                                            selected_trains = NULL,
                                            paths = NULL,
                                            path_scope = c("basic", "tonic_regularization"),
                                            max_params = 6L,
                                            max_trains = 3L,
                                            relative_step = 0.25,
                                            iou_min = 0.25,
                                            metric_mode = c("strict_high_confidence", "candidate_family"),
                                            use_learned_ranges = FALSE,
                                            collect_diagnostics = FALSE,
                                            multiple_correction = c("BH", "none"),
                                            fdr_alpha = 0.05,
                                            robust_delta_F1 = 0.02,
                                            permutation_n = 999L,
                                            permutation_seed = 1L) {
  metric_mode <- match.arg(metric_mode)
  path_scope <- match.arg(path_scope)
  multiple_correction <- match.arg(multiple_correction)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_parameter_sensitivity_scan(): ds must be a dataset with trains.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  max_trains <- suppressWarnings(as.integer(max_trains %||% 3L))
  if (!is.finite(max_trains) || max_trains < 1L) max_trains <- 3L
  target <- head(target, max_trains)
  if (length(target) == 0) stop("No target trains found for parameter sensitivity scan.", call. = FALSE)
  schema <- stpd_parameter_sensitivity_schema(path_scope)
  if (is.null(paths) || length(paths) == 0) {
    paths <- if (identical(path_scope, "tonic_regularization")) {
      stpd_tonic_sensitivity_paths(max_params = max_params)
    } else {
      stpd_basic_sensitivity_paths(max_params = max_params)
    }
  }
  if (identical(path_scope, "tonic_regularization")) {
    invalid <- setdiff(as.character(paths), as.character(schema$path))
    if (length(invalid)) {
      stop(
        "Tonic regularization sensitivity paths are not allowlisted: ",
        paste(invalid, collapse = ", "), call. = FALSE
      )
    }
  }
  paths <- intersect(as.character(paths), as.character(schema$path))
  max_params <- suppressWarnings(as.integer(max_params %||% length(paths)))
  if (!is.finite(max_params) || max_params < 1L) max_params <- length(paths)
  paths <- head(paths, max_params)
  if (length(paths) == 0) {
    stop("No numeric/logical parameter paths are available for the selected sensitivity scope.", call. = FALSE)
  }

  baseline_issues <- tryCatch(
    stpd_validate_params(params_current),
    error = function(e) data.frame(
      severity = "error", path = "params_current",
      issue = conditionMessage(e), stringsAsFactors = FALSE
    )
  )
  baseline_errors <- baseline_issues[
    as.character(baseline_issues$severity %||% "") == "error",
    , drop = FALSE
  ]
  if (nrow(baseline_errors) > 0L) {
    stop(
      "The current parameter set is not valid for a sensitivity preview: ",
      paste(unique(as.character(baseline_errors$issue)), collapse = "; "),
      call. = FALSE
    )
  }

  baseline_params_hash <- tryCatch(
    compute_params_hash(effective_params_for_detector(params_current)),
    error = function(e) digest::digest(params_current, algo = "sha256", serialize = TRUE)
  )
  context_hash <- stpd_parameter_sensitivity_context_hash(
    ds, params_current, target, use_learned_ranges = use_learned_ranges
  )

  baseline_validation <- stpd_event_level_validation_report(
    ds, params_current, selected_trains = target, iou_min = iou_min,
    metric_mode = metric_mode, use_learned_ranges = use_learned_ranges,
    collect_diagnostics = collect_diagnostics
  )
  base_macro <- stpd_metric_macro_summary(baseline_validation$metrics)
  rows <- list()
  metric_rows <- list()
  match_rows <- list()
  train_metric_rows <- list()
  state_episode_rows <- list()
  state_support_rows <- list()
  overlap_resolution_rows <- list()
  scan_id <- paste0("parameter_sensitivity_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  add_variant_tables <- function(variant_id, path, label, ui_level, section, baseline_value, variant_value,
                                 direction, report, delta_preview,
                                 variant_status = "valid_contract",
                                 variant_issue_codes = "",
                                 scientific_direction = direction,
                                 preview_applied = identical(variant_status, "valid_contract"),
                                 effective_variant_value = variant_value,
                                 runtime_contract_status = "not_applicable") {
    metrics <- report$metrics %||% data.frame()
    matches <- report$matches %||% data.frame()
    if (nrow(metrics) > 0) {
      metrics$scan_id <- scan_id
      metrics$variant_id <- variant_id
      metrics$parameter_path <- path
      metrics$parameter_label <- label
      metrics$baseline_value <- as.character(baseline_value)
      metrics$variant_value <- as.character(variant_value)
      metric_rows[[length(metric_rows) + 1L]] <<- metrics
    }
    if (nrow(matches) > 0) {
      matches$scan_id <- scan_id
      matches$variant_id <- variant_id
      matches$parameter_path <- path
      matches$parameter_label <- label
      matches$baseline_value <- as.character(baseline_value)
      matches$variant_value <- as.character(variant_value)
      match_rows[[length(match_rows) + 1L]] <<- matches
    }
    macro <- stpd_metric_macro_summary(metrics)
    train_macro <- stpd_train_macro_metrics_from_report(report, iou_min = iou_min)
    if (nrow(train_macro) > 0) {
      train_macro$scan_id <- scan_id
      train_macro$variant_id <- variant_id
      train_macro$parameter_path <- path
      train_macro$parameter_label <- label
      train_metric_rows[[length(train_metric_rows) + 1L]] <<- train_macro
    }
    delta_summary <- delta_preview$summary %||% data.frame(metric = character(), value = character(), stringsAsFactors = FALSE)
    get_delta <- function(metric) {
      val <- delta_summary$value[as.character(delta_summary$metric) == metric][1]
      suppressWarnings(as.numeric(val %||% NA_real_))
    }
    add_detail <- function(table, destination) {
      table <- as.data.frame(table %||% data.frame(), stringsAsFactors = FALSE)
      if (nrow(table) == 0L) return(invisible(NULL))
      table$scan_id <- scan_id
      table$variant_id <- variant_id
      table$parameter_path <- path
      table$parameter_label <- label
      table$baseline_value <- as.character(baseline_value)
      table$variant_value <- as.character(variant_value)
      table$effective_variant_value <- as.character(effective_variant_value)
      table$scientific_direction <- scientific_direction
      table$variant_status <- variant_status
      if (identical(destination, "state_episode")) {
        state_episode_rows[[length(state_episode_rows) + 1L]] <<- table
      } else if (identical(destination, "state_support")) {
        state_support_rows[[length(state_support_rows) + 1L]] <<- table
      } else if (identical(destination, "overlap_resolution")) {
        overlap_resolution_rows[[length(overlap_resolution_rows) + 1L]] <<- table
      }
      invisible(NULL)
    }
    add_detail(delta_preview$state_episode_diff, "state_episode")
    add_detail(delta_preview$state_direct_support_diff, "state_support")
    add_detail(delta_preview$overlap_resolution_diff, "overlap_resolution")
    rows[[length(rows) + 1L]] <<- data.frame(
      scan_id = scan_id,
      variant_id = variant_id,
      parameter_path = path,
      parameter_label = label,
      ui_level = ui_level,
      section = section,
      baseline_value = as.character(baseline_value),
      variant_value = as.character(variant_value),
      direction = direction,
      scientific_direction = scientific_direction,
      variant_status = variant_status,
      variant_issue_codes = variant_issue_codes,
      preview_authority = "dry_run_non_authoritative",
      preview_applied = isTRUE(preview_applied),
      effective_variant_value = as.character(effective_variant_value),
      runtime_contract_status = runtime_contract_status,
      baseline_params_hash = baseline_params_hash,
      context_sha256 = context_hash,
      selected_train_n = length(target),
      selected_trains = paste(target, collapse = ";"),
      manual_event_n = report$meta$manual_event_n[1] %||% NA_integer_,
      predicted_event_n = report$meta$predicted_event_n[1] %||% NA_integer_,
      changed_event_n = get_delta("changed_event_n"),
      added_event_n = get_delta("added_event_n"),
      removed_event_n = get_delta("removed_event_n"),
      label_changed_n = get_delta("label_changed_n"),
      boundary_changed_n = get_delta("boundary_changed_n"),
      baseline_tonic_state_episode_n = get_delta("baseline_tonic_state_episode_n"),
      current_tonic_state_episode_n = get_delta("current_tonic_state_episode_n"),
      delta_tonic_state_episode_n = get_delta("delta_tonic_state_episode_n"),
      baseline_broad_hfs_state_episode_n = get_delta("baseline_broad_hfs_state_episode_n"),
      current_broad_hfs_state_episode_n = get_delta("current_broad_hfs_state_episode_n"),
      delta_broad_hfs_state_episode_n = get_delta("delta_broad_hfs_state_episode_n"),
      changed_state_episode_n = get_delta("changed_state_episode_n"),
      baseline_tonic_direct_support_isi_n = get_delta("baseline_tonic_direct_support_isi_n"),
      current_tonic_direct_support_isi_n = get_delta("current_tonic_direct_support_isi_n"),
      delta_tonic_direct_support_isi_n =
        get_delta("current_tonic_direct_support_isi_n") -
        get_delta("baseline_tonic_direct_support_isi_n"),
      baseline_broad_hfs_direct_support_isi_n = get_delta("baseline_broad_hfs_direct_support_isi_n"),
      current_broad_hfs_direct_support_isi_n = get_delta("current_broad_hfs_direct_support_isi_n"),
      delta_broad_hfs_direct_support_isi_n =
        get_delta("current_broad_hfs_direct_support_isi_n") -
        get_delta("baseline_broad_hfs_direct_support_isi_n"),
      changed_state_direct_support_isi_n = get_delta("changed_state_direct_support_isi_n"),
      changed_overlap_resolution_n = get_delta("changed_overlap_resolution_n"),
      macro_precision = macro$macro_precision,
      macro_recall = macro$macro_recall,
      macro_F1 = macro$macro_F1,
      delta_macro_F1_vs_baseline = macro$macro_F1 - base_macro$macro_F1,
      iou_min = suppressWarnings(as.numeric(iou_min)),
      metric_mode = metric_mode,
      stringsAsFactors = FALSE
    )
  }

  base_meta <- baseline_validation$meta
  base_meta$predicted_event_n <- nrow(baseline_validation$predicted_events %||% data.frame())
  baseline_validation$meta <- base_meta
  add_variant_tables("baseline_current", "", "Current UI parameters", "", "", "", "", "baseline", baseline_validation,
                     stpd_parameter_delta_preview(ds, params_current, params_current, selected_trains = target, max_trains = length(target), iou_min = iou_min, source = "auto", lock_manual = FALSE, collect_diagnostics = collect_diagnostics, label_blind = TRUE))

  for (path in paths) {
    sr <- schema[as.character(schema$path) == path, , drop = FALSE]
    if (nrow(sr) == 0) next
    base_val <- stpd_get_param(params_current, path, stpd_schema_value(sr[1, , drop = FALSE]))
    vals <- stpd_parameter_sensitivity_values(params_current, sr[1, , drop = FALSE], relative_step = relative_step)
    if (length(vals) == 0) next
    for (vv in vals) {
      params_variant <- stpd_set_param(params_current, path, if (identical(as.character(sr$type[1]), "integer")) as.integer(vv) else vv)
      if (exists("stpd_productize_params", mode = "function")) {
        params_variant <- stpd_productize_params(
          params_variant,
          prefer = if (startsWith(path, "spiketrainpattern.")) "canonical" else "legacy"
        )
      }
      variant_id <- paste0(gsub("[^A-Za-z0-9_]+", "_", path), "_", length(rows))
      direction <- if (is.logical(vv)) {
        if (isTRUE(vv)) "toggle_on" else "toggle_off"
      } else if (suppressWarnings(as.numeric(vv)) > suppressWarnings(as.numeric(base_val))) {
        "increase"
      } else {
        "decrease"
      }
      scientific_direction <- stpd_parameter_sensitivity_scientific_direction(
        path, base_val, vv, direction
      )
      parameter_issues <- tryCatch(
        stpd_validate_params(params_variant),
        error = function(e) data.frame(
          severity = "error", path = path,
          issue = conditionMessage(e), stringsAsFactors = FALSE
        )
      )
      parameter_errors <- parameter_issues[
        as.character(parameter_issues$severity %||% "") == "error",
        , drop = FALSE
      ]
      if (nrow(parameter_errors) > 0L) {
        issue_path <- as.character(parameter_errors$path %||% rep(path, nrow(parameter_errors)))
        issue_text <- as.character(parameter_errors$issue %||% rep("invalid parameter contract", nrow(parameter_errors)))
        issue_code <- paste0(issue_path, "::", issue_text)
        is_mm_relaxed <- identical(path, "tonic.tonic_mm_relaxed_max") &&
          is.finite(suppressWarnings(as.numeric(vv))) &&
          suppressWarnings(as.numeric(vv)) > 1.40
        fallback_value <- if (is_mm_relaxed) "1.40" else NA_character_
        contract_status <- if (is_mm_relaxed) {
          "invalid_calibration_contract__detector_would_fallback_to_1.40"
        } else {
          "invalid_parameter_contract__variant_not_run"
        }
        empty_report <- list(meta = data.frame(manual_event_n = NA_integer_),
                             metrics = data.frame(), matches = data.frame())
        add_variant_tables(
          variant_id, path,
          as.character(sr$label[1] %||% path),
          as.character(sr$ui_level[1] %||% ""),
          as.character(sr$section[1] %||% ""),
          base_val, vv, direction, empty_report,
          list(summary = data.frame(metric = character(), value = character())),
          variant_status = "invalid_contract",
          variant_issue_codes = paste(
            unique(issue_code),
            collapse = ";"
          ),
          scientific_direction = scientific_direction,
          preview_applied = FALSE,
          effective_variant_value = fallback_value,
          runtime_contract_status = contract_status
        )
        next
      }
      report <- stpd_event_level_validation_report(
        ds, params_variant, selected_trains = target, iou_min = iou_min,
        metric_mode = metric_mode, use_learned_ranges = use_learned_ranges,
        collect_diagnostics = collect_diagnostics
      )
      meta <- report$meta
      meta$predicted_event_n <- nrow(report$predicted_events %||% data.frame())
      report$meta <- meta
      delta_preview <- stpd_parameter_delta_preview(
        ds, params_variant, params_current, selected_trains = target, max_trains = length(target),
        iou_min = iou_min, source = "auto", lock_manual = FALSE,
        collect_diagnostics = collect_diagnostics, label_blind = TRUE
      )
      add_variant_tables(
        variant_id, path,
        as.character(sr$label[1] %||% path),
        as.character(sr$ui_level[1] %||% ""),
        as.character(sr$section[1] %||% ""),
        base_val, vv, direction, report, delta_preview,
        scientific_direction = scientific_direction,
        preview_applied = TRUE,
        effective_variant_value = vv,
        runtime_contract_status = "valid_parameter_contract"
      )
    }
  }

  summary <- if (length(rows) > 0) dplyr::bind_rows(rows) else data.frame()
  metrics <- if (length(metric_rows) > 0) dplyr::bind_rows(metric_rows) else data.frame()
  matches <- if (length(match_rows) > 0) dplyr::bind_rows(match_rows) else data.frame()
  train_metrics <- if (length(train_metric_rows) > 0) dplyr::bind_rows(train_metric_rows) else data.frame()
  state_episode_differences <- if (length(state_episode_rows) > 0) {
    dplyr::bind_rows(state_episode_rows)
  } else data.frame()
  state_direct_support_differences <- if (length(state_support_rows) > 0) {
    dplyr::bind_rows(state_support_rows)
  } else data.frame()
  overlap_resolution_differences <- if (length(overlap_resolution_rows) > 0) {
    dplyr::bind_rows(overlap_resolution_rows)
  } else data.frame()
  adjustment <- list(summary = summary, tests = data.frame())
  if (identical(multiple_correction, "BH")) {
    adjustment <- stpd_parameter_sensitivity_adjustment(
      summary, train_metrics,
      alpha = fdr_alpha,
      effect_threshold = robust_delta_F1,
      p_adjust_method = "BH",
      n_permutations = permutation_n,
      seed = permutation_seed
    )
    summary <- adjustment$summary
  }
  list(
    meta = data.frame(
      scan_id = scan_id,
      preview_authority = "dry_run_non_authoritative",
      preview_applies_parameters = FALSE,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      dataset_name = as.character((ds$meta %||% list())$display_name %||%
                                   (ds$meta %||% list())$name %||% ""),
      baseline_params_hash = baseline_params_hash,
      context_sha256 = context_hash,
      selected_trains = paste(target, collapse = ";"),
      use_learned_ranges = isTRUE(use_learned_ranges),
      stringsAsFactors = FALSE
    ),
    summary = summary,
    metrics = metrics,
    matches = matches,
    train_metrics = train_metrics,
    state_episode_differences = state_episode_differences,
    state_direct_support_differences = state_direct_support_differences,
    overlap_resolution_differences = overlap_resolution_differences,
    multiple_comparison_tests = adjustment$tests,
    selected_trains = target,
    paths = paths,
    path_scope = path_scope,
    iou_min = iou_min,
    metric_mode = metric_mode,
    relative_step = relative_step
  )
}

stpd_parameter_sensitivity_export <- function(scan, out_dir) {
  if (is.null(scan) || !is.list(scan)) stop("No parameter sensitivity scan is available to export.", call. = FALSE)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_safe(scan$meta %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_meta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$summary %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$metrics %||% data.frame(), file.path(out_dir, "Event_level_validation_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$matches %||% data.frame(), file.path(out_dir, "Manual_detector_event_matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$train_metrics %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_train_paired_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$multiple_comparison_tests %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_multiple_comparison_tests.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$state_episode_differences %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_state_episode_differences.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$state_direct_support_differences %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_state_direct_support_differences.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$overlap_resolution_differences %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_overlap_resolution_differences.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}
