#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
finite_mean <- function(x) { x <- x[is.finite(x)]; if (length(x) > 0) mean(x) else NA_real_ }
prediction_path <- if (length(args) >= 1) args[[1]] else 'detector_predictions_observed_time_overlap.csv'
truth_path <- if (length(args) >= 2) args[[2]] else list.files(pattern = '_interval_table.csv$', full.names = TRUE, recursive = TRUE)[1]
parse_iou_thresholds <- function(x) { vals <- suppressWarnings(as.numeric(strsplit(paste(x, collapse = ','), ',', fixed = TRUE)[[1]])); vals <- vals[is.finite(vals) & vals >= 0 & vals <= 1]; unique(vals) }
iou_thresholds <- if (length(args) >= 3) parse_iou_thresholds(args[[3]]) else c(0.10, 0.30, 0.50)
if (length(iou_thresholds) == 0) stop('IoU thresholds must be finite values between 0 and 1, e.g. 0.10,0.30,0.50.')
matching <- if (length(args) >= 4) tolower(args[[4]]) else 'greedy'
if (!matching %in% c('greedy', 'optimal')) stop("matching must be 'greedy' or 'optimal'.")
exact_fallback_limit <- if (length(args) >= 5) suppressWarnings(as.integer(args[[5]])) else 10L
if (!is.finite(exact_fallback_limit) || exact_fallback_limit < 1L) exact_fallback_limit <- 10L
clue_available <- requireNamespace('clue', quietly = TRUE)
clue_version <- if (clue_available) as.character(utils::packageVersion('clue')) else NA_character_
matching_method_used <- if (identical(matching, 'greedy')) 'greedy_descending_iou_by_label_and_train' else if (clue_available) 'optimal_lsap_clue_by_label_and_train' else 'optimal_exact_base_r_by_label_and_train'
if (!file.exists(prediction_path)) stop('Missing detector_predictions_observed_time_overlap.csv. Required columns: Pred_Start_s, Pred_End_s, Pred_Label; Train is optional.')
if (is.na(truth_path) || !file.exists(truth_path)) stop('Missing interval ground truth table.')
truth <- read.csv(truth_path, stringsAsFactors = FALSE)
pred <- read.csv(prediction_path, stringsAsFactors = FALSE)
required_truth <- c('Start_Time_s', 'End_Time_s', 'ISI_Label')
if (!all(required_truth %in% names(truth))) stop('Truth table must contain Start_Time_s, End_Time_s, and ISI_Label.')
if (!all(c('Pred_Start_s', 'Pred_End_s', 'Pred_Label') %in% names(pred))) stop('Prediction file must contain Pred_Start_s, Pred_End_s, and Pred_Label.')
if (!'Train' %in% names(truth)) truth$Train <- 1L
if (!'Train' %in% names(pred)) pred$Train <- 1L
labels <- c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')
n_invalid_pred_labels <- sum(!is.na(pred$Pred_Label) & !as.character(pred$Pred_Label) %in% labels)
truth <- truth[truth$ISI_Label %in% labels & is.finite(truth$Start_Time_s) & is.finite(truth$End_Time_s) & truth$End_Time_s > truth$Start_Time_s, , drop = FALSE]
pred <- pred[pred$Pred_Label %in% labels & is.finite(pred$Pred_Start_s) & is.finite(pred$Pred_End_s) & pred$Pred_End_s > pred$Pred_Start_s, , drop = FALSE]
iou_pair <- function(a0, a1, b0, b1) {
  inter <- max(0, min(a1, b1) - max(a0, b0))
  uni <- max(a1, b1) - min(a0, b0)
  if (!is.finite(uni) || uni <= 0) return(0)
  inter / uni
}
empty_match <- function() data.frame(Truth_Row = integer(0), Pred_Row = integer(0), IoU = numeric(0))
greedy_match <- function(candidates, n_truth, n_pred) {
  if (nrow(candidates) == 0) return(empty_match())
  candidates <- candidates[order(-candidates$IoU), , drop = FALSE]
  matched_truth <- integer(0)
  matched_pred <- integer(0)
  rows <- empty_match()
  for (ci in seq_len(nrow(candidates))) {
    ti <- as.integer(candidates$Truth_Row[ci])
    pi <- as.integer(candidates$Pred_Row[ci])
    if (ti %in% matched_truth || pi %in% matched_pred) next
    matched_truth <- c(matched_truth, ti)
    matched_pred <- c(matched_pred, pi)
    rows <- rbind(rows, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = candidates$IoU[ci]))
  }
  rows
}
optimal_lsap_match <- function(candidates, n_truth, n_pred) {
  if (nrow(candidates) == 0) return(empty_match())
  side <- max(n_truth, n_pred, 1L)
  weight <- matrix(0, nrow = side, ncol = side)
  iou_lookup <- matrix(NA_real_, nrow = side, ncol = side)
  for (ci in seq_len(nrow(candidates))) {
    ti <- as.integer(candidates$Truth_Row[ci])
    pi <- as.integer(candidates$Pred_Row[ci])
    score <- as.numeric(candidates$IoU[ci])
    if (!is.finite(score) || ti < 1L || pi < 1L || ti > n_truth || pi > n_pred) next
    w <- 1e6 + score
    if (w > weight[ti, pi]) {
      weight[ti, pi] <- w
      iou_lookup[ti, pi] <- score
    }
  }
  assignment <- clue::solve_LSAP(weight, maximum = TRUE)
  rows <- empty_match()
  for (ti in seq_len(n_truth)) {
    pi <- as.integer(assignment[ti])
    if (pi <= n_pred && weight[ti, pi] > 0) rows <- rbind(rows, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = iou_lookup[ti, pi]))
  }
  rows
}
exact_small_match <- function(candidates, n_truth, n_pred, limit = 10L) {
  if (nrow(candidates) == 0) return(empty_match())
  if (n_truth > limit || n_pred > limit) {
    stop(paste0("matching='optimal' needs package 'clue' for this candidate size. Install clue, rerun with matching='greedy', or use exact fallback only for <= ", limit, " truth and prediction intervals per label."))
  }
  weight <- matrix(0, nrow = n_truth, ncol = n_pred)
  iou_lookup <- matrix(NA_real_, nrow = n_truth, ncol = n_pred)
  for (ci in seq_len(nrow(candidates))) {
    ti <- as.integer(candidates$Truth_Row[ci])
    pi <- as.integer(candidates$Pred_Row[ci])
    score <- as.numeric(candidates$IoU[ci])
    if (!is.finite(score) || ti < 1L || pi < 1L || ti > n_truth || pi > n_pred) next
    w <- 1e6 + score
    if (w > weight[ti, pi]) {
      weight[ti, pi] <- w
      iou_lookup[ti, pi] <- score
    }
  }
  memo <- new.env(parent = emptyenv())
  choice <- new.env(parent = emptyenv())
  solve_state <- function(ti, mask) {
    if (ti > n_truth) return(0)
    key <- paste(ti, mask, sep = ':')
    if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))
    best <- solve_state(ti + 1L, mask)
    best_pred <- 0L
    available <- which(weight[ti, ] > 0)
    if (length(available) > 0) {
      for (pi in available) {
        bit <- bitwShiftL(1L, pi - 1L)
        if (bitwAnd(mask, bit) != 0L) next
        val <- weight[ti, pi] + solve_state(ti + 1L, bitwOr(mask, bit))
        if (val > best) {
          best <- val
          best_pred <- as.integer(pi)
        }
      }
    }
    assign(key, best, envir = memo)
    assign(key, best_pred, envir = choice)
    best
  }
  invisible(solve_state(1L, 0L))
  rows <- empty_match()
  mask <- 0L
  for (ti in seq_len(n_truth)) {
    key <- paste(ti, mask, sep = ':')
    pi <- if (exists(key, envir = choice, inherits = FALSE)) get(key, envir = choice, inherits = FALSE) else 0L
    if (pi > 0L) {
      rows <- rbind(rows, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = iou_lookup[ti, pi]))
      mask <- bitwOr(mask, bitwShiftL(1L, pi - 1L))
    }
  }
  rows
}
select_matches <- function(candidates, n_truth, n_pred) {
  if (identical(matching, 'greedy')) return(greedy_match(candidates, n_truth, n_pred))
  if (clue_available) return(optimal_lsap_match(candidates, n_truth, n_pred))
  exact_small_match(candidates, n_truth, n_pred, exact_fallback_limit)
}
score_one_threshold <- function(min_iou) {
match_one_label <- function(label) {
  t <- truth[truth$ISI_Label == label, , drop = FALSE]
  p <- pred[pred$Pred_Label == label, , drop = FALSE]
  candidates <- data.frame()
  if (nrow(t) > 0 && nrow(p) > 0) {
    for (ti in seq_len(nrow(t))) {
      same_train <- which(as.integer(p$Train) == as.integer(t$Train[ti]))
      if (length(same_train) == 0) next
      for (pi in same_train) {
        score <- iou_pair(t$Start_Time_s[ti], t$End_Time_s[ti], p$Pred_Start_s[pi], p$Pred_End_s[pi])
        if (is.finite(score) && score >= min_iou) {
          candidates <- rbind(candidates, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = score))
        }
      }
    }
  }
  matched <- select_matches(candidates, nrow(t), nrow(p))
  matched_truth <- as.integer(matched$Truth_Row)
  matched_pred <- as.integer(matched$Pred_Row)
  matched_iou <- as.numeric(matched$IoU)
  onset_err <- if (nrow(matched) > 0) p$Pred_Start_s[matched_pred] - t$Start_Time_s[matched_truth] else numeric(0)
  offset_err <- if (nrow(matched) > 0) p$Pred_End_s[matched_pred] - t$End_Time_s[matched_truth] else numeric(0)
  tp <- nrow(matched)
  fp <- max(0L, nrow(p) - tp)
  fn <- max(0L, nrow(t) - tp)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  data.frame(Min_IoU = min_iou, Requested_Matching = matching, Matching_Method = matching_method_used, Clue_Available = clue_available, Clue_Version = clue_version, Exact_Fallback_Limit = exact_fallback_limit, Label = label, Truth_Intervals = nrow(t), Predicted_Intervals = nrow(p), TP = tp, FP = fp, FN = fn, Precision = precision, Recall = recall, F1 = f1, Mean_IoU = if (length(matched_iou) > 0) mean(matched_iou) else NA_real_, Mean_Onset_Error_s = if (length(onset_err) > 0) mean(onset_err) else NA_real_, Mean_Offset_Error_s = if (length(offset_err) > 0) mean(offset_err) else NA_real_)
}
per_class <- do.call(rbind, lapply(labels, match_one_label))
tp <- sum(per_class$TP)
fp <- sum(per_class$FP)
fn <- sum(per_class$FN)
micro_precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
micro_recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
micro_f1 <- if ((micro_precision + micro_recall) > 0) 2 * micro_precision * micro_recall / (micro_precision + micro_recall) else 0
macro_mask <- (per_class$TP + per_class$FP + per_class$FN) > 0
summary <- data.frame(Min_IoU = min_iou, Requested_Matching = matching, Matching_Method = matching_method_used, Clue_Available = clue_available, Clue_Version = clue_version, Exact_Fallback_Limit = exact_fallback_limit, Truth_Intervals = nrow(truth), Predicted_Intervals = nrow(pred), N_Invalid_Pred_Labels = n_invalid_pred_labels, Invalid_Prediction_Audit_Pass = n_invalid_pred_labels == 0, TP = tp, FP = fp, FN = fn, Micro_Precision = micro_precision, Micro_Recall = micro_recall, Micro_F1 = micro_f1, Macro_F1 = if (any(macro_mask)) mean(per_class$F1[macro_mask]) else NA_real_, Macro_F1_Labels = paste(per_class$Label[macro_mask], collapse = ';'), Mean_Matched_IoU = finite_mean(per_class$Mean_IoU), Primary_Metric = min_iou == 0.30)
list(per_class = per_class, summary = summary)
}
scored <- lapply(iou_thresholds, score_one_threshold)
per_class <- do.call(rbind, lapply(scored, `[[`, 'per_class'))
summary <- do.call(rbind, lapply(scored, `[[`, 'summary'))
write.csv(per_class, 'score_observed_time_overlap_per_class.csv', row.names = FALSE)
write.csv(summary, 'score_observed_time_overlap_summary.csv', row.names = FALSE)
print(summary)
