#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
finite_mean <- function(x) { x <- x[is.finite(x)]; if (length(x) > 0) mean(x) else NA_real_ }
prediction_path <- if (length(args) >= 1) args[[1]] else 'detector_predictions_latent_interval.csv'
truth_path <- if (length(args) >= 2) args[[2]] else list.files(pattern = '_interval_table.csv$', full.names = TRUE, recursive = TRUE)[1]
if (!file.exists(prediction_path)) stop('Missing detector_predictions_latent_interval.csv. Required columns: Train, Interval_ID, Pred_Label.')
if (is.na(truth_path) || !file.exists(truth_path)) stop('Missing interval ground truth table.')
truth <- read.csv(truth_path, stringsAsFactors = FALSE)
pred <- read.csv(prediction_path, stringsAsFactors = FALSE)
if (!'Train' %in% names(truth)) truth$Train <- 1L
if (!all(c('Train', 'Interval_ID', 'Pred_Label') %in% names(pred))) stop('Prediction file must contain Train, Interval_ID, and Pred_Label.')
truth$Train <- as.integer(truth$Train)
truth$Interval_ID <- as.integer(truth$Interval_ID)
pred$Train <- suppressWarnings(as.integer(pred$Train))
pred$Interval_ID <- suppressWarnings(as.integer(pred$Interval_ID))
pred$Pred_Label <- as.character(pred$Pred_Label)
valid_labels <- c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')
n_invalid_pred_labels <- sum(!is.na(pred$Pred_Label) & !pred$Pred_Label %in% valid_labels)
pred$Pred_Label[!pred$Pred_Label %in% valid_labels] <- 'Invalid_Label'
truth_key <- paste(truth$Train, truth$Interval_ID, sep = '::')
pred_key_valid <- is.finite(pred$Train) & is.finite(pred$Interval_ID)
pred_key <- rep(NA_character_, nrow(pred))
pred_key[pred_key_valid] <- paste(pred$Train[pred_key_valid], pred$Interval_ID[pred_key_valid], sep = '::')
n_invalid_train_interval_keys <- sum(!pred_key_valid)
n_duplicate_prediction_keys <- sum(duplicated(pred_key[pred_key_valid]))
valid_unique_prediction_keys <- unique(pred_key[pred_key_valid])
n_extra_prediction_keys <- sum(!valid_unique_prediction_keys %in% unique(truth_key))
pred_for_scoring <- pred[pred_key_valid & !duplicated(pred_key), c('Train', 'Interval_ID', 'Pred_Label'), drop = FALSE]
dat <- merge(truth, pred_for_scoring, by = c('Train', 'Interval_ID'), all.x = TRUE)
dat$Pred_Label[is.na(dat$Pred_Label)] <- 'Unclassified'
scorable <- dat$ISI_Label %in% c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')
dat <- dat[scorable, , drop = FALSE]
labels <- c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')
rows <- lapply(labels, function(label) {
  tp <- sum(dat$ISI_Label == label & dat$Pred_Label == label)
  fp <- sum(dat$ISI_Label != label & dat$Pred_Label == label)
  fn <- sum(dat$ISI_Label == label & dat$Pred_Label != label)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  data.frame(Label = label, TP = tp, FP = fp, FN = fn, Precision = precision, Recall = recall, F1 = f1)
})
per_class <- do.call(rbind, rows)
macro_mask <- (per_class$TP + per_class$FP + per_class$FN) > 0
summary <- data.frame(Interval_Accuracy = if (nrow(dat) > 0) mean(dat$ISI_Label == dat$Pred_Label) else NA_real_, Macro_F1 = if (any(macro_mask)) mean(per_class$F1[macro_mask]) else NA_real_, Macro_F1_Labels = paste(per_class$Label[macro_mask], collapse = ';'), N_Scorable = nrow(dat), N_Prediction_Rows = nrow(pred), N_Prediction_Rows_Used = nrow(pred_for_scoring), N_Extra_Prediction_Keys = n_extra_prediction_keys, N_Invalid_Train_Interval_Keys = n_invalid_train_interval_keys, N_Duplicate_Prediction_Keys = n_duplicate_prediction_keys, N_Invalid_Pred_Labels = n_invalid_pred_labels, Prediction_Key_Audit_Pass = n_extra_prediction_keys == 0 && n_invalid_train_interval_keys == 0 && n_duplicate_prediction_keys == 0, Invalid_Prediction_Audit_Pass = n_invalid_pred_labels == 0)
write.csv(per_class, 'score_latent_interval_per_class.csv', row.names = FALSE)
write.csv(summary, 'score_latent_interval_summary.csv', row.names = FALSE)
print(summary)
