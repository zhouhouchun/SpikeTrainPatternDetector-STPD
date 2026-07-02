# Event-level validation helpers. These functions do not alter the detector;
# they provide clean, explicit evaluation primitives for held-out workflows.

stpd_event_iou <- function(a_start, a_end, b_start, b_end) {
  a_start <- as.integer(a_start); a_end <- as.integer(a_end); b_start <- as.integer(b_start); b_end <- as.integer(b_end)
  lo <- pmax(a_start, b_start); hi <- pmin(a_end, b_end)
  ov <- pmax(0L, hi - lo + 1L)
  union <- pmax(a_end, b_end) - pmin(a_start, b_start) + 1L
  ifelse(union > 0, ov / union, 0)
}

stpd_match_events <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  if (is.null(pred) || is.null(truth) || nrow(pred) == 0 || nrow(truth) == 0) {
    return(tibble::tibble(pred_index = integer(), truth_index = integer(), iou = numeric(), pattern = character()))
  }
  rows <- list()
  for (ii in seq_len(nrow(pred))) {
    tr <- pred$train[ii]
    cls <- pred[[class_col]][ii]
    cand <- truth[truth$train == tr & truth[[class_col]] == cls, , drop = FALSE]
    if (nrow(cand) == 0) next
    iou <- stpd_event_iou(pred$start_isi[ii], pred$end_isi[ii], cand$start_isi, cand$end_isi)
    jj <- which.max(iou)
    if (length(jj) && is.finite(iou[jj]) && iou[jj] >= iou_min) {
      rows[[length(rows) + 1L]] <- data.frame(pred_index = ii, truth_index = as.integer(rownames(cand)[jj] %||% jj), iou = iou[jj], pattern = cls, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) tibble::tibble(pred_index = integer(), truth_index = integer(), iou = numeric(), pattern = character()) else dplyr::bind_rows(rows)
}

stpd_event_level_metrics <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  classes <- sort(unique(c(pred[[class_col]] %||% character(), truth[[class_col]] %||% character())))
  if (length(classes) == 0) return(tibble::tibble())
  match <- stpd_match_events(pred, truth, class_col = class_col, iou_min = iou_min)
  rows <- lapply(classes, function(cls) {
    pidx <- which(pred[[class_col]] == cls)
    tidx <- which(truth[[class_col]] == cls)
    midx <- match$pattern == cls
    tp <- sum(midx)
    fp <- max(0L, length(pidx) - tp)
    fn <- max(0L, length(tidx) - tp)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1 <- if (is.finite(precision) && is.finite(recall) && (precision + recall) > 0) 2 * precision * recall / (precision + recall) else NA_real_
    data.frame(pattern = cls, tp = tp, fp = fp, fn = fn, precision = precision, recall = recall, f1 = f1, stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

stpd_holdout_split_by_train <- function(train_names, fraction = 0.25, seed = 1L) {
  train_names <- unique(as.character(train_names))
  set.seed(seed)
  n_val <- max(1L, floor(length(train_names) * fraction))
  validation <- sample(train_names, n_val)
  data.frame(train = train_names, split = ifelse(train_names %in% validation, "validation", "calibration"), stringsAsFactors = FALSE)
}

stpd_overfit_report <- function(calibration_metrics, validation_metrics) {
  if (is.null(calibration_metrics) || is.null(validation_metrics)) return(tibble::tibble())
  m <- merge(calibration_metrics, validation_metrics, by = "pattern", suffixes = c("_calibration", "_validation"), all = TRUE)
  m$recall_gap <- m$recall_calibration - m$recall_validation
  m$precision_gap <- m$precision_calibration - m$precision_validation
  m$interpretation <- ifelse(is.finite(m$recall_gap) & m$recall_gap > 0.2, "possible overfit: calibration recall much higher than validation", "no large recall gap detected")
  m
}
