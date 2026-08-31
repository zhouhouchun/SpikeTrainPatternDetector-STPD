options(stringsAsFactors = FALSE, width = 220)

args <- commandArgs(trailingOnly = TRUE)
validation_arg <- grep("^--validation-dir=", args, value = TRUE)
root <- if (length(validation_arg) > 0L) {
  sub("^--validation-dir=", "", validation_arg[[1L]])
} else {
  Sys.getenv("STPD_REAL_VALIDATION_DIR", unset = "")
}
if (!nzchar(root) || !dir.exists(root)) {
  stop(
    "Set STPD_REAL_VALIDATION_DIR or pass --validation-dir=<directory>.",
    call. = FALSE
  )
}
root <- normalizePath(path.expand(root), mustWork = TRUE)
pred <- read.csv(file.path(root, "heldout_interval_predictions.csv"), check.names = FALSE)
truth_events <- read.csv(file.path(root, "heldout_truth_events.csv"), check.names = FALSE)
learned <- read.csv(file.path(root, "learned_parameters_by_fold.csv"), check.names = FALSE)
mm_min_semantics <- if ("--current-canonical-mm" %in% args) "current_canonical_max_over_mean" else "intended_min_over_mean"
output_arg <- grep("^--output=", args, value = TRUE)
sink_open <- length(output_arg) > 0L
if (sink_open) sink(sub("^--output=", "", output_arg[1L]))

# Exploratory, label-blind candidate scan only. This deliberately does not call
# the detector and must not be interpreted as an independent validation.
state <- pred[pred$Axis == "state_hf_family", ]
state <- state[order(state$Train_ID, state$Right_Spike_Index), ]
truth_tonic <- truth_events[truth_events$Axis == "state_hf_family" & truth_events$Pattern == "tonic", ]

lv <- function(x) {
  if (length(x) < 2L) return(NA_real_)
  a <- head(x, -1L); b <- tail(x, -1L); den <- a + b
  3 * mean(((a - b) / den)^2)
}
getp <- function(fold, parameter) {
  x <- learned$Value[learned$fold_id == fold & learned$Parameter == parameter]
  if (!length(x)) NA_real_ else as.numeric(x[1L])
}

scan_train <- function(z, window_n = 4L, mm_max = 1.25, mm_min = 0.85) {
  z <- z[order(z$Right_Spike_Index), ]
  fold <- unique(z$fold_id)
  stopifnot(length(fold) == 1L)
  lower <- getp(fold, "tonic.min_isi_sec")
  upper <- getp(fold, "tonic.max_isi_sec")
  lv_max <- getp(fold, "tonic.lv_max")
  d_min <- getp(fold, "tonic.min_duration_sec")
  starts <- if (nrow(z) >= window_n) seq_len(nrow(z) - window_n + 1L) else integer()
  pass <- vapply(starts, function(j) {
    zz <- z[j:(j + window_n - 1L), ]
    x <- zz$ISI_s
    contiguous <- all(diff(zz$Right_Spike_Index) == 1L)
    mu <- mean(x)
    mm_lower_pass <- if (identical(mm_min_semantics, "current_canonical_max_over_mean")) {
      # This reproduces current stpd_event_core_mm semantics. With positive ISIs
      # and mm_min=0.85 it is mathematically non-binding because max/mean >= 1.
      max(x) / mu >= mm_min
    } else {
      # This reproduces legacy near-miss semantics and the intended lower-tail
      # regularity interpretation.
      min(x) / mu >= mm_min
    }
    contiguous && all(is.finite(x)) && sum(x < lower) <= 1L &&
      all(x <= upper) && sum(x) >= d_min && lv(x) <= lv_max &&
      max(x) / mu <= mm_max && mm_lower_pass
  }, logical(1))
  starts <- starts[pass]
  if (!length(starts)) return(data.frame())
  raw <- data.frame(start_pos = starts, end_pos = starts + window_n - 1L)
  group <- cumsum(c(TRUE, tail(raw$start_pos, -1L) > head(raw$end_pos, -1L)))
  do.call(rbind, lapply(split(raw, group), function(rr) {
    p1 <- min(rr$start_pos); p2 <- max(rr$end_pos)
    zz <- z[p1:p2, ]
    data.frame(
      Train_ID = zz$Train_ID[1L], fold_id = fold,
      start_isi = min(zz$Right_Spike_Index), end_isi = max(zz$Right_Spike_Index),
      n_isi = nrow(zz), duration_ms = sum(zz$ISI_s) * 1000,
      truth_tonic_n = sum(zz$Truth == "tonic"),
      truth_hfs_n = sum(zz$Truth == "high_frequency_spiking"),
      truth_blank_other_n = sum(zz$Truth == "other"),
      passing_core_n = nrow(rr)
    )
  }))
}

candidates <- do.call(rbind, lapply(split(state, state$Train_ID), scan_train))
if (is.null(candidates)) candidates <- data.frame()

cat("EXPLORATORY LOCAL-REGULARITY CORE BURDEN CHECK\n")
cat("MM-min semantics:", mm_min_semantics, "\n")
cat("Definition: exact 4-ISI cores; <=1 ISI below fold-frozen lower band; none above upper; sum duration >= frozen Dmin; LV <= frozen LVmax; max/mean <=1.25; lower MM gate is ",
    if (identical(mm_min_semantics, "current_canonical_max_over_mean")) "max/mean >=0.85 (current canonical; non-binding)" else "min/mean >=0.85 (legacy/intended lower-tail interpretation)",
    "; overlapping cores merged.\n\n", sep = "")

cat("State intervals:", nrow(state), "; truth tonic:", sum(state$Truth == "tonic"),
    "; blank-as-other:", sum(state$Truth == "other"),
    "; HFS:", sum(state$Truth == "high_frequency_spiking"), "\n")
cat("Candidates:", nrow(candidates), "; candidate ISI support:", sum(candidates$n_isi),
    sprintf(" (%.1f candidates / 1000 state ISI)\n", 1000 * nrow(candidates) / nrow(state)))

if (nrow(candidates)) {
  candidates$category <- ifelse(candidates$truth_tonic_n > 0, "overlaps_tonic",
    ifelse(candidates$truth_hfs_n > 0 & candidates$truth_blank_other_n == 0, "HFS_only",
      ifelse(candidates$truth_blank_other_n > 0 & candidates$truth_hfs_n == 0, "blank_other_only", "mixed_HFS_blank")))
  cat("\nCandidate burden by current reference category:\n")
  print(aggregate(cbind(candidates = rep(1L, nrow(candidates)), candidate_isi = candidates$n_isi) ~ category,
                  data = candidates, FUN = sum), row.names = FALSE)
  blank_only <- candidates[candidates$category == "blank_other_only", ]
  hfs_only <- candidates[candidates$category == "HFS_only", ]
  cat(sprintf("blank-as-other-only burden: %d candidates, %.1f / 1000 blank-as-other ISI; %d candidate ISI\n",
              nrow(blank_only), 1000 * nrow(blank_only) / sum(state$Truth == "other"), sum(blank_only$n_isi)))
  cat(sprintf("HFS-only burden: %d candidates, %.1f / 1000 HFS ISI; %d candidate ISI\n",
              nrow(hfs_only), 1000 * nrow(hfs_only) / sum(state$Truth == "high_frequency_spiking"), sum(hfs_only$n_isi)))
}

# Candidate-support confusion, explicitly treating blank-as-other as negative.
pred_flag <- rep(FALSE, nrow(state))
for (i in seq_len(nrow(candidates))) {
  pred_flag <- pred_flag | (state$Train_ID == candidates$Train_ID[i] &
    state$Right_Spike_Index >= candidates$start_isi[i] &
    state$Right_Spike_Index <= candidates$end_isi[i])
}
truth_flag <- state$Truth == "tonic"
tp <- sum(pred_flag & truth_flag); fp <- sum(pred_flag & !truth_flag); fn <- sum(!pred_flag & truth_flag)
cat(sprintf("\nSupport (exploratory; blank-as-other negative): TP/FP/FN=%d/%d/%d, P/R/F1=%.3f/%.3f/%.3f\n",
            tp, fp, fn, tp/(tp+fp), tp/(tp+fn), 2*tp/(2*tp+fp+fn)))

# Manual episode recovery: any overlap, >=50% truth coverage, and best IoU.
episode_rows <- list()
for (i in seq_len(nrow(truth_tonic))) {
  e <- truth_tonic[i, ]
  cc <- candidates[candidates$Train_ID == e$Train_ID & candidates$end_isi >= e$start_isi & candidates$start_isi <= e$end_isi, ]
  n_truth <- e$end_isi - e$start_isi + 1L
  if (!nrow(cc)) {
    best_cov <- 0; best_iou <- 0; n_hit <- 0L
  } else {
    ov <- pmax(0L, pmin(cc$end_isi, e$end_isi) - pmax(cc$start_isi, e$start_isi) + 1L)
    cov <- ov / n_truth
    iou <- ov / (n_truth + cc$n_isi - ov)
    best_cov <- max(cov); best_iou <- max(iou); n_hit <- nrow(cc)
  }
  episode_rows[[i]] <- data.frame(Train_ID=e$Train_ID, fold_id=e$fold_id,
    start_isi=e$start_isi, end_isi=e$end_isi, n_truth_isi=n_truth,
    candidate_hits=n_hit, best_coverage=best_cov, best_IoU=best_iou)
}
er <- do.call(rbind, episode_rows)
cat(sprintf("Episode recall any overlap=%d/%d (%.3f); >=50%% coverage=%d/%d (%.3f); IoU>=0.30=%d/%d (%.3f)\n",
            sum(er$best_coverage > 0), nrow(er), mean(er$best_coverage > 0),
            sum(er$best_coverage >= .5), nrow(er), mean(er$best_coverage >= .5),
            sum(er$best_IoU >= .3), nrow(er), mean(er$best_IoU >= .3)))
cat(sprintf("For reference episodes with >=4 ISI: >=50%% coverage=%d/%d; IoU>=0.30=%d/%d\n",
            sum(er$best_coverage[er$n_truth_isi >= 4] >= .5), sum(er$n_truth_isi >= 4),
            sum(er$best_IoU[er$n_truth_isi >= 4] >= .3), sum(er$n_truth_isi >= 4)))
cat("\nPer-episode results:\n")
print(er, row.names = FALSE)
if (sink_open) sink()
