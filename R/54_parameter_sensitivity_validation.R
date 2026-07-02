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

stpd_event_level_validation_report <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                               iou_min = 0.25,
                                               metric_mode = c("strict_high_confidence", "candidate_family"),
                                               use_learned_ranges = TRUE,
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

  threshold_training_trains <- character()
  threshold_freeze_status <- "not_requested"
  if (identical(threshold_freeze, "calibration")) {
    threshold_training_trains <- as.character(split_table$train[as.character(split_table$split) == "calibration"])
    if (length(threshold_training_trains) > 0) {
      params_eval <- stpd_freeze_thresholds_for_trains(
        ds, params_eval,
        calibration_trains = threshold_training_trains,
        freeze_scope = "event_level_validation_calibration"
      )
      threshold_freeze_status <- "frozen"
    } else {
      threshold_freeze_status <- "skipped_no_calibration_split"
    }
  }

  truth_raw <- stpd_extract_events_by_source(ds, params_eval, source = "manual", selected_trains = target, metric_mode = metric_mode)
  truth_filter <- stpd_filter_ambiguous_events(truth_raw, ambiguous_labels = ambiguous_manual_labels)
  truth_all <- if (isTRUE(exclude_ambiguous_manual)) truth_filter$events else truth_raw
  meta <- data.frame(
    validation_run_id = paste0("event_validation_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    metric_mode = metric_mode,
    iou_min = iou_min,
    selected_train_n = length(target),
    selected_trains = paste(target, collapse = ";"),
    manual_event_n = nrow(truth_all %||% data.frame()),
    predicted_event_n = NA_integer_,
    learned_ranges_used = isTRUE(use_learned_ranges),
    threshold_freeze = threshold_freeze,
    threshold_freeze_status = threshold_freeze_status,
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
    interpretation = if (nrow(truth_all %||% data.frame()) == 0) "No manual events available; event-level validation cannot estimate performance." else "Event-level IoU validation against manual labels; use false positives/negatives and boundary errors for parameter choice.",
    stringsAsFactors = FALSE
  )
  if (nrow(truth_all %||% data.frame()) == 0) {
    meta$predicted_event_n <- 0L
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
      manual_uncertainty_boundary_sensitivity = data.frame(),
      manual_uncertainty_inter_rater = data.frame()
    ))
  }

  pred_ds <- stpd_detect(ds, params_eval, selected_trains = target, lock_manual = FALSE, collect_diagnostics = collect_diagnostics)
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
  schema <- schema[as.character(schema$type) %in% c("numeric", "integer"), , drop = FALSE]
  preferred <- c(
    "event_core.seed_band_upper_sec",
    "event_core.bridge_band_upper_sec",
    "event_core.burst_contrast_min",
    "event_core.possible_burst_contrast_min",
    "highfreq.T_high_max",
    "highfreq.spiking_max_ISI_abs",
    "tonic.T_min",
    "tonic.T_max",
    "pause.T_seed",
    "pause.T_strong"
  )
  paths <- unique(c(preferred[preferred %in% schema$path], as.character(schema$path)))
  max_params <- suppressWarnings(as.integer(max_params %||% length(paths)))
  if (!is.finite(max_params) || max_params < 1L) max_params <- length(paths)
  head(paths, max_params)
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

stpd_train_validation_strata <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                         metadata = NULL, strata_cols = NULL,
                                         metric_mode = c("strict_high_confidence", "candidate_family")) {
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
  if (!is.null(metadata)) {
    md <- as.data.frame(metadata, stringsAsFactors = FALSE)
    if (!("train" %in% names(md)) && !is.null(rownames(md))) md$train <- rownames(md)
    if ("train" %in% names(md)) out <- merge(out, md, by = "train", all.x = TRUE, sort = FALSE)
  }
  if (is.null(strata_cols)) {
    strata_cols <- c(intersect(c("nucleus", "condition", "subject", "group"), names(out)),
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

stpd_stratified_train_splits <- function(strata_table, validation_fraction = 0.25,
                                         n_repeats = 50L, seed = 1L,
                                         min_validation_trains = 1L) {
  st <- as.data.frame(strata_table %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(st) == 0 || !("train" %in% names(st))) return(data.frame())
  if (!("manual_event_n" %in% names(st))) st$manual_event_n <- 1L
  if (!("stratum" %in% names(st))) st$stratum <- "all"
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
  eligible <- as.character(st$train[suppressWarnings(as.numeric(st$manual_event_n)) > 0])
  rows <- vector("list", n_repeats)
  for (rr in seq_len(n_repeats)) {
    val <- character()
    if (length(eligible) > 0) {
      for (ss in unique(as.character(st$stratum[st$train %in% eligible]))) {
        members <- as.character(st$train[st$train %in% eligible & as.character(st$stratum) == ss])
        if (length(members) >= 2L) {
          n_val <- max(1L, floor(length(members) * validation_fraction))
          n_val <- min(n_val, length(members) - 1L)
          val <- c(val, sample(members, n_val))
        }
      }
      needed <- if (length(eligible) > 1L) {
        min(max(min_validation_trains, floor(length(eligible) * validation_fraction)), length(eligible) - 1L)
      } else {
        1L
      }
      if (length(val) < needed) {
        pool <- setdiff(eligible, val)
        if (length(pool) > 0) val <- c(val, sample(pool, min(needed - length(val), length(pool))))
      }
      if (length(val) == 0 && length(eligible) > 0) val <- sample(eligible, 1L)
      if (length(val) >= length(eligible) && length(eligible) > 1L) val <- val[-1L]
    }
    sp <- st
    sp$repeat_id <- rr
    sp$split <- ifelse(sp$train %in% val, "validation", ifelse(sp$train %in% eligible, "calibration", "unlabeled"))
    sp$split_note <- if (sum(sp$split == "calibration") == 0 || sum(sp$split == "validation") == 0) {
      "insufficient_labeled_trains_for_independent_holdout"
    } else {
      "stratified_train_holdout"
    }
    rows[[rr]] <- sp
  }
  out <- dplyr::bind_rows(rows)
  out[order(out$repeat_id, out$split, out$stratum, out$train), , drop = FALSE]
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

stpd_parameter_variant_grid <- function(params, paths = NULL, max_params = 4L, relative_step = 0.25) {
  schema <- tryCatch(stpd_contract_ui_schema(ui_level = "basic"), error = function(e) data.frame())
  schema <- schema[as.character(schema$type) %in% c("numeric", "integer", "logical"), , drop = FALSE]
  if (is.null(paths) || length(paths) == 0) paths <- stpd_basic_sensitivity_paths(max_params = max_params)
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

stpd_nested_tune_params <- function(ds, params, calibration_trains,
                                    paths = NULL, max_params = 4L, relative_step = 0.25,
                                    inner_validation_fraction = 0.25, seed = 1L,
                                    iou_min = 0.25,
                                    metric_mode = c("strict_high_confidence", "candidate_family"),
                                    use_learned_ranges = TRUE,
                                    collect_diagnostics = FALSE,
                                    selection_metric = c("macro_F1", "macro_recall", "macro_precision")) {
  metric_mode <- match.arg(metric_mode)
  selection_metric <- match.arg(selection_metric)
  calibration_trains <- intersect(as.character(calibration_trains), names(ds$trains))
  variants <- stpd_parameter_variant_grid(params, paths = paths, max_params = max_params, relative_step = relative_step)
  if (length(calibration_trains) < 2L) {
    return(list(params = params, selected_variant_id = "baseline_current",
                tuning_table = data.frame(variant_id = "baseline_current", selected = TRUE,
                                          note = "Not enough calibration trains for inner validation.", stringsAsFactors = FALSE)))
  }
  strata <- stpd_train_validation_strata(ds, params, selected_trains = calibration_trains, metric_mode = metric_mode)
  split <- stpd_stratified_train_splits(strata, validation_fraction = inner_validation_fraction, n_repeats = 1L, seed = seed)
  split <- split[, c("train", "split"), drop = FALSE]
  if (!any(split$split == "calibration") || !any(split$split == "validation")) {
    return(list(params = params, selected_variant_id = "baseline_current",
                tuning_table = data.frame(variant_id = "baseline_current", selected = TRUE,
                                          note = "Inner split did not contain both calibration and validation trains.", stringsAsFactors = FALSE)))
  }
  rows <- lapply(variants, function(vv) {
    rep <- stpd_event_level_validation_report(
      ds, vv$params, selected_trains = calibration_trains, split_table = split,
      iou_min = iou_min, metric_mode = metric_mode, use_learned_ranges = use_learned_ranges,
      threshold_freeze = "calibration", collect_diagnostics = collect_diagnostics,
      score_calibrator = "none"
    )
    mm <- rep$metrics[as.character(rep$metrics$split) == "validation", , drop = FALSE]
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
  list(params = variants[[best]]$params, selected_variant_id = variants[[best]]$variant_id,
       tuning_table = tab, inner_split = split)
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
                                                   use_learned_ranges = TRUE,
                                                   threshold_freeze = c("calibration", "none"),
                                                   collect_diagnostics = FALSE) {
  metric_mode <- match.arg(metric_mode)
  threshold_freeze <- match.arg(threshold_freeze)
  strata <- stpd_train_validation_strata(ds, params, selected_trains = selected_trains,
                                         metadata = metadata, strata_cols = strata_cols,
                                         metric_mode = metric_mode)
  splits <- stpd_stratified_train_splits(strata, validation_fraction = validation_fraction,
                                         n_repeats = n_repeats, seed = seed)
  metric_rows <- list(); repeat_rows <- list(); tuning_rows <- list()
  for (rr in sort(unique(splits$repeat_id))) {
    sp <- splits[splits$repeat_id == rr, , drop = FALSE]
    target <- as.character(sp$train[sp$split %in% c("calibration", "validation")])
    cal <- as.character(sp$train[sp$split == "calibration"])
    if (length(target) == 0 || !any(sp$split == "validation")) next
    tuned <- if (isTRUE(nested_tuning)) {
      stpd_nested_tune_params(ds, params, cal, paths = tuning_paths, max_params = max_tuning_params,
                              relative_step = relative_step, seed = seed + rr,
                              iou_min = iou_min, metric_mode = metric_mode,
                              use_learned_ranges = use_learned_ranges,
                              collect_diagnostics = collect_diagnostics)
    } else {
      list(params = params, selected_variant_id = "baseline_current", tuning_table = data.frame())
    }
    split_table <- sp[, c("train", "split"), drop = FALSE]
    rep <- stpd_event_level_validation_report(
      ds, tuned$params, selected_trains = target, split_table = split_table,
      iou_min = iou_min, metric_mode = metric_mode,
      use_learned_ranges = use_learned_ranges,
      threshold_freeze = threshold_freeze,
      collect_diagnostics = collect_diagnostics,
      score_calibrator = "platt"
    )
    val_metrics <- rep$metrics[as.character(rep$metrics$split) == "validation", , drop = FALSE]
    if (nrow(val_metrics) > 0) {
      val_metrics$repeat_id <- rr
      val_metrics$selected_variant_id <- tuned$selected_variant_id
      metric_rows[[length(metric_rows) + 1L]] <- val_metrics
    }
    macro <- stpd_metric_macro_summary(val_metrics)
    repeat_rows[[length(repeat_rows) + 1L]] <- data.frame(
      repeat_id = rr,
      calibration_train_n = sum(sp$split == "calibration"),
      validation_train_n = sum(sp$split == "validation"),
      selected_variant_id = tuned$selected_variant_id,
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
  metrics <- if (length(metric_rows) > 0) dplyr::bind_rows(metric_rows) else data.frame()
  repeats <- if (length(repeat_rows) > 0) dplyr::bind_rows(repeat_rows) else data.frame()
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
  } else data.frame()
  list(
    meta = data.frame(validation_run_id = paste0("repeated_holdout_", format(Sys.time(), "%Y%m%d_%H%M%S")),
                      n_repeats = n_repeats, validation_fraction = validation_fraction,
                      nested_tuning = isTRUE(nested_tuning), iou_min = iou_min,
                      metric_mode = metric_mode, stringsAsFactors = FALSE),
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
                                            max_params = 6L,
                                            max_trains = 3L,
                                            relative_step = 0.25,
                                            iou_min = 0.25,
                                            metric_mode = c("strict_high_confidence", "candidate_family"),
                                            use_learned_ranges = TRUE,
                                            collect_diagnostics = FALSE,
                                            multiple_correction = c("BH", "none"),
                                            fdr_alpha = 0.05,
                                            robust_delta_F1 = 0.02,
                                            permutation_n = 999L,
                                            permutation_seed = 1L) {
  metric_mode <- match.arg(metric_mode)
  multiple_correction <- match.arg(multiple_correction)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_parameter_sensitivity_scan(): ds must be a dataset with trains.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  max_trains <- suppressWarnings(as.integer(max_trains %||% 3L))
  if (!is.finite(max_trains) || max_trains < 1L) max_trains <- 3L
  target <- head(target, max_trains)
  if (length(target) == 0) stop("No target trains found for parameter sensitivity scan.", call. = FALSE)
  schema <- tryCatch(stpd_contract_ui_schema(ui_level = "basic"), error = function(e) data.frame())
  schema <- schema[as.character(schema$type) %in% c("numeric", "integer", "logical"), , drop = FALSE]
  if (is.null(paths) || length(paths) == 0) paths <- stpd_basic_sensitivity_paths(max_params = max_params)
  paths <- intersect(as.character(paths), as.character(schema$path))
  max_params <- suppressWarnings(as.integer(max_params %||% length(paths)))
  if (!is.finite(max_params) || max_params < 1L) max_params <- length(paths)
  paths <- head(paths, max_params)
  if (length(paths) == 0) stop("No Basic numeric/logical parameter paths are available for sensitivity scan.", call. = FALSE)

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
  scan_id <- paste0("parameter_sensitivity_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  add_variant_tables <- function(variant_id, path, label, ui_level, section, baseline_value, variant_value,
                                 direction, report, delta_preview) {
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
      selected_train_n = length(target),
      selected_trains = paste(target, collapse = ";"),
      manual_event_n = report$meta$manual_event_n[1] %||% NA_integer_,
      predicted_event_n = report$meta$predicted_event_n[1] %||% NA_integer_,
      changed_event_n = get_delta("changed_event_n"),
      added_event_n = get_delta("added_event_n"),
      removed_event_n = get_delta("removed_event_n"),
      label_changed_n = get_delta("label_changed_n"),
      boundary_changed_n = get_delta("boundary_changed_n"),
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
                     stpd_parameter_delta_preview(ds, params_current, params_current, selected_trains = target, max_trains = length(target), iou_min = iou_min, source = "auto", lock_manual = FALSE, collect_diagnostics = collect_diagnostics))

  for (path in paths) {
    sr <- schema[as.character(schema$path) == path, , drop = FALSE]
    if (nrow(sr) == 0) next
    base_val <- stpd_get_param(params_current, path, stpd_schema_value(sr[1, , drop = FALSE]))
    vals <- stpd_parameter_sensitivity_values(params_current, sr[1, , drop = FALSE], relative_step = relative_step)
    if (length(vals) == 0) next
    for (vv in vals) {
      params_variant <- stpd_set_param(params_current, path, if (identical(as.character(sr$type[1]), "integer")) as.integer(vv) else vv)
      variant_id <- paste0(gsub("[^A-Za-z0-9_]+", "_", path), "_", length(rows))
      direction <- if (is.logical(vv)) {
        if (isTRUE(vv)) "toggle_on" else "toggle_off"
      } else if (suppressWarnings(as.numeric(vv)) > suppressWarnings(as.numeric(base_val))) {
        "increase"
      } else {
        "decrease"
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
        iou_min = iou_min, source = "auto", lock_manual = FALSE, collect_diagnostics = collect_diagnostics
      )
      add_variant_tables(
        variant_id, path,
        as.character(sr$label[1] %||% path),
        as.character(sr$ui_level[1] %||% ""),
        as.character(sr$section[1] %||% ""),
        base_val, vv, direction, report, delta_preview
      )
    }
  }

  summary <- if (length(rows) > 0) dplyr::bind_rows(rows) else data.frame()
  metrics <- if (length(metric_rows) > 0) dplyr::bind_rows(metric_rows) else data.frame()
  matches <- if (length(match_rows) > 0) dplyr::bind_rows(match_rows) else data.frame()
  train_metrics <- if (length(train_metric_rows) > 0) dplyr::bind_rows(train_metric_rows) else data.frame()
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
    summary = summary,
    metrics = metrics,
    matches = matches,
    train_metrics = train_metrics,
    multiple_comparison_tests = adjustment$tests,
    selected_trains = target,
    paths = paths,
    iou_min = iou_min,
    metric_mode = metric_mode,
    relative_step = relative_step
  )
}

stpd_parameter_sensitivity_export <- function(scan, out_dir) {
  if (is.null(scan) || !is.list(scan)) stop("No parameter sensitivity scan is available to export.", call. = FALSE)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_safe(scan$summary %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$metrics %||% data.frame(), file.path(out_dir, "Event_level_validation_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$matches %||% data.frame(), file.path(out_dir, "Manual_detector_event_matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$train_metrics %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_train_paired_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(scan$multiple_comparison_tests %||% data.frame(), file.path(out_dir, "Parameter_sensitivity_multiple_comparison_tests.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}
