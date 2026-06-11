# Scientific validation layer.
# This module keeps detector semantics stable and adds explicit validation,
# split accounting, and exportable reports.

stpd_metric_mode_normalize <- function(x, mode = c("strict_high_confidence", "candidate_family")) {
  mode <- match.arg(mode)
  x <- as.character(x %||% "")
  x <- trimws(tolower(x))
  x[x %in% c("", "na", "none", "unlabeled")] <- "unlabeled"
  x[x %in% c("possible-burst", "possible burst", "possibleburst")] <- "possible_burst"
  x[x %in% c("long-burst", "long burst", "longburst")] <- "long_burst"
  x[x %in% c("hf tonic", "high-frequency tonic", "high frequency tonic", "hftonic")] <- "high_frequency_tonic"
  x[x %in% c("hf spiking", "high-frequency spiking", "high frequency spiking", "hfspiking")] <- "high_frequency_spiking"
  if (identical(mode, "candidate_family")) {
    x[x %in% c("burst", "long_burst", "possible_burst")] <- "burst_family"
  }
  x
}

stpd_events_apply_metric_mode <- function(events, mode = c("strict_high_confidence", "candidate_family")) {
  mode <- match.arg(mode)
  if (is.null(events) || nrow(events) == 0) return(events %||% tibble::tibble())
  out <- events
  out$pattern_original <- as.character(out$pattern %||% "")
  out$pattern <- stpd_metric_mode_normalize(out$pattern_original, mode = mode)
  out <- out[out$pattern != "unlabeled" & nzchar(out$pattern), , drop = FALSE]
  out
}

stpd_extract_events_by_source <- function(ds, params = default_params_sec(), source = c("manual", "auto", "final", "audit_final"),
                                          selected_trains = NULL, metric_mode = c("strict_high_confidence", "candidate_family")) {
  source <- match.arg(source)
  metric_mode <- match.arg(metric_mode)
  if (is.null(ds) || is.null(ds$trains)) return(tibble::tibble())
  params <- effective_params_for_detector(params)
  trains <- ds$trains
  target <- selected_trains %||% names(trains)
  target <- intersect(target, names(trains))
  if (length(target) == 0) return(tibble::tibble())
  trains <- trains[target]
  dataset_name <- ds$meta$display_name %||% "dataset"
  ev <- derive_interval_tables(
    trains,
    source = source,
    auto_others = FALSE,
    dataset_map = setNames(rep(dataset_name, length(trains)), names(trains)),
    min_isi_sec = params$detector$min_valid_isi_sec %||% 0.0009,
    contrast_q = params$burst$contrast_q %||% 0.90,
    context_k = params$burst$context_k %||% 5L
  )$events
  stpd_events_apply_metric_mode(ev, mode = metric_mode)
}

stpd_has_manual_labels <- function(ds, selected_trains = NULL) {
  if (is.null(ds) || is.null(ds$trains)) return(FALSE)
  target <- selected_trains %||% names(ds$trains)
  target <- intersect(target, names(ds$trains))
  any(vapply(ds$trains[target], function(dat) {
    if (is.null(dat$pattern_manual)) return(FALSE)
    any(nzchar(as.character(dat$pattern_manual %||% "")), na.rm = TRUE)
  }, logical(1)))
}

stpd_split_trains_by_manual_events <- function(ds, params = default_params_sec(), validation_fraction = 0.25,
                                               seed = 1L, metric_mode = c("strict_high_confidence", "candidate_family")) {
  metric_mode <- match.arg(metric_mode)
  train_names <- names(ds$trains %||% list())
  if (length(train_names) == 0) return(data.frame(train = character(), split = character(), manual_event_n = integer(), stringsAsFactors = FALSE))
  ev <- stpd_extract_events_by_source(ds, params, source = "manual", selected_trains = train_names, metric_mode = metric_mode)
  counts <- if (nrow(ev) > 0) as.data.frame(table(ev$train), stringsAsFactors = FALSE) else data.frame(Var1 = character(), Freq = integer())
  names(counts) <- c("train", "manual_event_n")
  out <- data.frame(train = train_names, manual_event_n = 0L, stringsAsFactors = FALSE)
  idx <- match(counts$train, out$train)
  out$manual_event_n[idx[!is.na(idx)]] <- counts$manual_event_n[!is.na(idx)]
  eligible <- out$train[out$manual_event_n > 0]
  set.seed(seed)
  if (length(eligible) == 0) {
    out$split <- "unlabeled"
  } else {
    n_val <- max(1L, floor(length(eligible) * validation_fraction))
    n_val <- min(n_val, length(eligible))
    validation <- sample(eligible, n_val)
    out$split <- ifelse(out$train %in% validation, "validation", ifelse(out$train %in% eligible, "calibration", "unlabeled"))
  }
  out[order(out$split, out$train), , drop = FALSE]
}

stpd_match_events_greedy <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  if (is.null(pred) || is.null(truth) || nrow(pred) == 0 || nrow(truth) == 0) {
    return(tibble::tibble(pred_index = integer(), truth_index = integer(), train = character(), pattern = character(), iou = numeric()))
  }
  pred$.pred_index <- seq_len(nrow(pred)); truth$.truth_index <- seq_len(nrow(truth))
  pairs <- list()
  for (tr in intersect(unique(pred$train), unique(truth$train))) {
    p_tr <- pred[pred$train == tr, , drop = FALSE]
    t_tr <- truth[truth$train == tr, , drop = FALSE]
    for (cls in intersect(unique(p_tr[[class_col]]), unique(t_tr[[class_col]]))) {
      pp <- p_tr[p_tr[[class_col]] == cls, , drop = FALSE]
      tt <- t_tr[t_tr[[class_col]] == cls, , drop = FALSE]
      if (nrow(pp) == 0 || nrow(tt) == 0) next
      for (ii in seq_len(nrow(pp))) {
        iou <- stpd_event_iou(pp$start_isi[ii], pp$end_isi[ii], tt$start_isi, tt$end_isi)
        ok <- which(is.finite(iou) & iou >= iou_min)
        if (length(ok) == 0) next
        for (jj in ok) {
          pairs[[length(pairs) + 1L]] <- data.frame(
            pred_index = pp$.pred_index[ii], truth_index = tt$.truth_index[jj], train = tr, pattern = cls, iou = iou[jj],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(pairs) == 0) return(tibble::tibble(pred_index = integer(), truth_index = integer(), train = character(), pattern = character(), iou = numeric()))
  cand <- dplyr::bind_rows(pairs)
  cand <- cand[order(-cand$iou), , drop = FALSE]
  used_p <- integer(); used_t <- integer(); rows <- list()
  for (ii in seq_len(nrow(cand))) {
    if (cand$pred_index[ii] %in% used_p || cand$truth_index[ii] %in% used_t) next
    rows[[length(rows) + 1L]] <- cand[ii, , drop = FALSE]
    used_p <- c(used_p, cand$pred_index[ii]); used_t <- c(used_t, cand$truth_index[ii])
  }
  if (length(rows) == 0) tibble::tibble(pred_index = integer(), truth_index = integer(), train = character(), pattern = character(), iou = numeric()) else dplyr::bind_rows(rows)
}

stpd_event_level_metrics <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  pred <- pred %||% tibble::tibble(); truth <- truth %||% tibble::tibble()
  if (!(class_col %in% names(pred))) pred[[class_col]] <- character(nrow(pred))
  if (!(class_col %in% names(truth))) truth[[class_col]] <- character(nrow(truth))
  classes <- sort(unique(c(as.character(pred[[class_col]]), as.character(truth[[class_col]]))))
  classes <- classes[nzchar(classes) & !is.na(classes)]
  if (length(classes) == 0) return(tibble::tibble())
  matches <- stpd_match_events_greedy(pred, truth, class_col = class_col, iou_min = iou_min)
  rows <- lapply(classes, function(cls) {
    pidx <- which(as.character(pred[[class_col]]) == cls)
    tidx <- which(as.character(truth[[class_col]]) == cls)
    tp <- sum(matches$pattern == cls, na.rm = TRUE)
    fp <- max(0L, length(pidx) - tp)
    fn <- max(0L, length(tidx) - tp)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1 <- if (is.finite(precision) && is.finite(recall) && (precision + recall) > 0) 2 * precision * recall / (precision + recall) else NA_real_
    data.frame(pattern = cls, truth_n = length(tidx), predicted_n = length(pidx), true_positive_n = tp,
               false_positive_n = fp, false_negative_n = fn, precision = precision, recall = recall, F1 = f1,
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

stpd_binom_wilson_ci <- function(x, n, conf_level = 0.95) {
  x <- suppressWarnings(as.numeric(x))
  n <- suppressWarnings(as.numeric(n))
  conf_level <- suppressWarnings(as.numeric(conf_level %||% 0.95))
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- 0.95
  if (!is.finite(x) || !is.finite(n) || n <= 0) return(c(low = NA_real_, high = NA_real_))
  x <- max(0, min(x, n))
  p <- x / n
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(low = max(0, center - half), high = min(1, center + half))
}

stpd_f1_from_pr <- function(precision, recall) {
  precision <- suppressWarnings(as.numeric(precision))
  recall <- suppressWarnings(as.numeric(recall))
  out <- rep(NA_real_, max(length(precision), length(recall)))
  ok <- is.finite(precision) & is.finite(recall) & (precision + recall) > 0
  out[ok] <- 2 * precision[ok] * recall[ok] / (precision[ok] + recall[ok])
  out
}

stpd_event_level_metrics_ci <- function(metrics, conf_level = 0.95) {
  if (is.null(metrics) || nrow(metrics) == 0) return(metrics %||% tibble::tibble())
  conf_level <- suppressWarnings(as.numeric(conf_level %||% 0.95))
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- 0.95
  out <- as.data.frame(metrics, stringsAsFactors = FALSE)
  need <- c("true_positive_n", "false_positive_n", "false_negative_n", "predicted_n", "truth_n")
  for (nm in need) if (!(nm %in% names(out))) out[[nm]] <- NA_real_
  tp <- suppressWarnings(as.numeric(out$true_positive_n))
  fp <- suppressWarnings(as.numeric(out$false_positive_n))
  fn <- suppressWarnings(as.numeric(out$false_negative_n))
  p_den <- tp + fp
  r_den <- tp + fn
  p_ci <- t(vapply(seq_len(nrow(out)), function(i) stpd_binom_wilson_ci(tp[i], p_den[i], conf_level = conf_level), numeric(2)))
  r_ci <- t(vapply(seq_len(nrow(out)), function(i) stpd_binom_wilson_ci(tp[i], r_den[i], conf_level = conf_level), numeric(2)))
  out$precision_ci_low <- p_ci[, "low"]
  out$precision_ci_high <- p_ci[, "high"]
  out$recall_ci_low <- r_ci[, "low"]
  out$recall_ci_high <- r_ci[, "high"]
  out$F1_ci_low <- stpd_f1_from_pr(out$precision_ci_low, out$recall_ci_low)
  out$F1_ci_high <- stpd_f1_from_pr(out$precision_ci_high, out$recall_ci_high)
  out$ci_method <- "Wilson score; F1 bounds are plug-in bounds from precision/recall intervals"
  out$ci_conf_level <- conf_level
  out
}

stpd_event_level_complete_metrics <- function(metrics, classes) {
  classes <- sort(unique(as.character(classes %||% character())))
  classes <- classes[nzchar(classes) & !is.na(classes)]
  if (length(classes) == 0) return(metrics %||% tibble::tibble())
  out <- as.data.frame(metrics %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(out) == 0) {
    out <- data.frame(pattern = character(), truth_n = integer(), predicted_n = integer(), true_positive_n = integer(),
                      false_positive_n = integer(), false_negative_n = integer(), precision = numeric(), recall = numeric(),
                      F1 = numeric(), stringsAsFactors = FALSE)
  }
  if (!("pattern" %in% names(out))) out$pattern <- character(nrow(out))
  miss <- setdiff(classes, as.character(out$pattern))
  if (length(miss) > 0) {
    add <- data.frame(pattern = miss, truth_n = 0L, predicted_n = 0L, true_positive_n = 0L,
                      false_positive_n = 0L, false_negative_n = 0L, precision = NA_real_, recall = NA_real_,
                      F1 = NA_real_, stringsAsFactors = FALSE)
    out <- dplyr::bind_rows(out, add)
  }
  out[match(classes, as.character(out$pattern)), , drop = FALSE]
}

stpd_event_level_cluster_bootstrap <- function(pred, truth, class_col = "pattern", iou_min = 0.25,
                                               cluster_col = "train", n_bootstrap = 200L,
                                               seed = NULL, conf_level = 0.95) {
  pred <- as.data.frame(pred %||% data.frame(), stringsAsFactors = FALSE)
  truth <- as.data.frame(truth %||% data.frame(), stringsAsFactors = FALSE)
  if (!(class_col %in% names(pred))) pred[[class_col]] <- character(nrow(pred))
  if (!(class_col %in% names(truth))) truth[[class_col]] <- character(nrow(truth))
  if (!(cluster_col %in% names(pred))) pred[[cluster_col]] <- as.character(pred$train %||% character(nrow(pred)))
  if (!(cluster_col %in% names(truth))) truth[[cluster_col]] <- as.character(truth$train %||% character(nrow(truth)))
  if (!("train" %in% names(pred))) pred$train <- as.character(pred[[cluster_col]])
  if (!("train" %in% names(truth))) truth$train <- as.character(truth[[cluster_col]])

  classes <- sort(unique(c(as.character(pred[[class_col]]), as.character(truth[[class_col]]))))
  classes <- classes[nzchar(classes) & !is.na(classes)]
  clusters <- sort(unique(c(as.character(pred[[cluster_col]]), as.character(truth[[cluster_col]]))))
  clusters <- clusters[nzchar(clusters) & !is.na(clusters)]
  n_bootstrap <- suppressWarnings(as.integer(n_bootstrap %||% 200L))
  if (!is.finite(n_bootstrap) || n_bootstrap < 1L) n_bootstrap <- 200L
  conf_level <- suppressWarnings(as.numeric(conf_level %||% 0.95))
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- 0.95

  observed <- stpd_event_level_complete_metrics(
    stpd_event_level_metrics(pred, truth, class_col = class_col, iou_min = iou_min),
    classes
  )
  if (length(classes) == 0 || length(clusters) == 0) {
    return(list(
      observed = observed,
      bootstrap = data.frame(),
      summary = data.frame(pattern = character(), metric = character(), observed = numeric(),
                           bootstrap_n = integer(), bootstrap_mean = numeric(), bootstrap_ci_low = numeric(),
                           bootstrap_ci_high = numeric(), stringsAsFactors = FALSE)
    ))
  }

  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  clone_cluster <- function(tbl, cluster_value, replicate_index) {
    rows <- tbl[as.character(tbl[[cluster_col]]) == cluster_value, , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    rows[[cluster_col]] <- paste0(as.character(rows[[cluster_col]]), "__boot", replicate_index)
    rows$train <- paste0(as.character(rows$train), "__boot", replicate_index)
    rows
  }

  boot_rows <- vector("list", n_bootstrap)
  for (bb in seq_len(n_bootstrap)) {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    pred_b <- dplyr::bind_rows(lapply(seq_along(sampled), function(ii) clone_cluster(pred, sampled[ii], ii)))
    truth_b <- dplyr::bind_rows(lapply(seq_along(sampled), function(ii) clone_cluster(truth, sampled[ii], ii)))
    mm <- stpd_event_level_complete_metrics(
      stpd_event_level_metrics(pred_b, truth_b, class_col = class_col, iou_min = iou_min),
      classes
    )
    mm$bootstrap_replicate <- bb
    boot_rows[[bb]] <- mm
  }
  boot <- dplyr::bind_rows(boot_rows)
  metric_names <- c("precision", "recall", "F1", "true_positive_n", "false_positive_n", "false_negative_n", "predicted_n", "truth_n")
  alpha <- (1 - conf_level) / 2
  summary_rows <- list()
  for (cls in classes) {
    obs_row <- observed[as.character(observed$pattern) == cls, , drop = FALSE]
    for (metric in metric_names) {
      vals <- suppressWarnings(as.numeric(boot[[metric]][as.character(boot$pattern) == cls]))
      vals <- vals[is.finite(vals)]
      ci <- if (length(vals) > 0) stats::quantile(vals, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE, type = 6) else c(NA_real_, NA_real_)
      observed_val <- if (nrow(obs_row) > 0 && metric %in% names(obs_row)) suppressWarnings(as.numeric(obs_row[[metric]][1])) else NA_real_
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        pattern = cls,
        metric = metric,
        observed = observed_val,
        bootstrap_n = length(vals),
        bootstrap_mean = if (length(vals) > 0) mean(vals) else NA_real_,
        bootstrap_ci_low = ci[1],
        bootstrap_ci_high = ci[2],
        cluster_col = cluster_col,
        cluster_n = length(clusters),
        n_bootstrap = n_bootstrap,
        ci_conf_level = conf_level,
        stringsAsFactors = FALSE
      )
    }
  }
  list(observed = observed, bootstrap = boot, summary = dplyr::bind_rows(summary_rows))
}

stpd_event_level_merge_bootstrap_ci <- function(metrics, bootstrap_summary) {
  if (is.null(metrics) || nrow(metrics) == 0 || is.null(bootstrap_summary) || nrow(bootstrap_summary) == 0) {
    return(metrics %||% tibble::tibble())
  }
  out <- as.data.frame(metrics, stringsAsFactors = FALSE)
  bs <- as.data.frame(bootstrap_summary, stringsAsFactors = FALSE)
  for (metric in c("precision", "recall", "F1")) {
    low_col <- paste0(metric, "_cluster_boot_ci_low")
    high_col <- paste0(metric, "_cluster_boot_ci_high")
    out[[low_col]] <- NA_real_
    out[[high_col]] <- NA_real_
    bs_metric <- bs[as.character(bs$metric) == metric, , drop = FALSE]
    if (nrow(bs_metric) == 0) next
    for (ii in seq_len(nrow(out))) {
      hit <- as.character(bs_metric$pattern) == as.character(out$pattern[ii])
      if ("split" %in% names(out) && "split" %in% names(bs_metric)) hit <- hit & as.character(bs_metric$split) == as.character(out$split[ii])
      jj <- which(hit)[1]
      if (length(jj) && !is.na(jj)) {
        out[[low_col]][ii] <- suppressWarnings(as.numeric(bs_metric$bootstrap_ci_low[jj]))
        out[[high_col]][ii] <- suppressWarnings(as.numeric(bs_metric$bootstrap_ci_high[jj]))
      }
    }
  }
  out
}

stpd_freeze_thresholds_for_trains <- function(ds, params = default_params_sec(), calibration_trains = NULL,
                                              min_isi_sec = NULL, bin_width_sec = NULL,
                                              freeze_scope = "calibration") {
  params_eval <- effective_params_for_detector(params)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_freeze_thresholds_for_trains(): ds must be a dataset with trains.", call. = FALSE)
  target <- intersect(as.character(calibration_trains %||% names(ds$trains)), names(ds$trains))
  if (is.null(params_eval$event_grammar)) params_eval$event_grammar <- list()
  if (is.null(params_eval$detector)) params_eval$detector <- list()
  if (length(target) == 0) {
    params_eval$event_grammar$threshold_resolution_scope <- paste0(freeze_scope, "_no_training_trains")
    params_eval$event_grammar$threshold_training_trains <- ""
    params_eval$event_grammar$threshold_training_train_n <- 0L
    params_eval$detector$freeze_dataset_thresholds <- FALSE
    if (!is.null(params_eval$spiketrainpattern$engine)) params_eval$spiketrainpattern$engine$freeze_dataset_thresholds <- FALSE
    return(params_eval)
  }
  if (exists("stpd_event_grammar_clear_threshold_resolution", mode = "function")) {
    params_eval <- stpd_event_grammar_clear_threshold_resolution(params_eval)
  } else {
    params_eval$event_grammar$threshold_table <- NULL
    params_eval$event_grammar$effective_bands <- NULL
  }
  scoped_ds <- ds
  scoped_ds$trains <- scoped_ds$trains[target]
  min_isi_sec <- min_isi_sec %||% params_eval$detector$min_valid_isi_sec %||% 0.0009
  bin_width_sec <- bin_width_sec %||% params_eval$event_grammar$histogram_bin_width_sec %||% params_eval$event_core$histogram_bin_width_sec %||% 0.005
  params_eval <- stpd_attach_thresholds_to_params_impl(params_eval, ds = scoped_ds, min_isi_sec = min_isi_sec, bin_width_sec = bin_width_sec)
  params_eval$event_grammar$threshold_resolution_scope <- freeze_scope
  params_eval$event_grammar$threshold_training_trains <- paste(target, collapse = ";")
  params_eval$event_grammar$threshold_training_train_n <- length(target)
  params_eval$detector$freeze_dataset_thresholds <- FALSE
  if (!is.null(params_eval$spiketrainpattern$engine)) params_eval$spiketrainpattern$engine$freeze_dataset_thresholds <- FALSE
  params_eval
}

stpd_score_calibration <- function(pred, truth, score_col = "auto_score", class_col = "pattern",
                                   iou_min = 0.25, n_bins = 10L, conf_level = 0.95) {
  pred <- as.data.frame(pred %||% data.frame(), stringsAsFactors = FALSE)
  truth <- as.data.frame(truth %||% data.frame(), stringsAsFactors = FALSE)
  empty_cal <- data.frame(pattern = character(), bin = integer(), n = integer(), score_min = numeric(),
                          score_max = numeric(), mean_score = numeric(), empirical_precision = numeric(),
                          precision_ci_low = numeric(), precision_ci_high = numeric(), stringsAsFactors = FALSE)
  if (nrow(pred) == 0 || !(score_col %in% names(pred))) {
    return(list(calibration = empty_cal, prediction_scores = data.frame(), summary = data.frame(
      score_col = score_col, predicted_event_n = nrow(pred), scored_event_n = 0L,
      brier_score = NA_real_, note = "No detector score column available for calibration.",
      stringsAsFactors = FALSE
    )))
  }
  if (!(class_col %in% names(pred))) pred[[class_col]] <- character(nrow(pred))
  if (!(class_col %in% names(truth))) truth[[class_col]] <- character(nrow(truth))
  pred$.pred_index <- seq_len(nrow(pred))
  matches <- stpd_match_events_greedy(pred, truth, class_col = class_col, iou_min = iou_min)
  pred$true_positive <- pred$.pred_index %in% matches$pred_index
  pred$score <- suppressWarnings(as.numeric(pred[[score_col]]))
  scored <- pred[is.finite(pred$score), , drop = FALSE]
  if (nrow(scored) == 0) {
    return(list(calibration = empty_cal, prediction_scores = pred, summary = data.frame(
      score_col = score_col, predicted_event_n = nrow(pred), scored_event_n = 0L,
      brier_score = NA_real_, note = "Detector events exist, but all scores are missing or non-finite.",
      stringsAsFactors = FALSE
    )))
  }
  n_bins <- suppressWarnings(as.integer(n_bins %||% 10L))
  if (!is.finite(n_bins) || n_bins < 2L) n_bins <- 10L
  conf_level <- suppressWarnings(as.numeric(conf_level %||% 0.95))
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- 0.95
  score_is_probability <- all(scored$score >= 0 & scored$score <= 1, na.rm = TRUE)
  classes <- sort(unique(as.character(scored[[class_col]])))
  classes <- classes[nzchar(classes) & !is.na(classes)]

  build_one <- function(df, cls) {
    if (nrow(df) == 0) return(empty_cal)
    qn <- min(n_bins, nrow(df))
    probs <- seq(0, 1, length.out = qn + 1L)
    breaks <- unique(as.numeric(stats::quantile(df$score, probs = probs, na.rm = TRUE, type = 7)))
    if (length(breaks) < 2L) breaks <- c(min(df$score, na.rm = TRUE) - 1e-12, max(df$score, na.rm = TRUE) + 1e-12)
    df$bin <- as.integer(cut(df$score, breaks = breaks, include.lowest = TRUE, labels = FALSE))
    df <- df[is.finite(df$bin), , drop = FALSE]
    bins <- sort(unique(df$bin))
    dplyr::bind_rows(lapply(bins, function(bb) {
      sub <- df[df$bin == bb, , drop = FALSE]
      tp <- sum(sub$true_positive, na.rm = TRUE)
      ci <- stpd_binom_wilson_ci(tp, nrow(sub), conf_level = conf_level)
      data.frame(pattern = cls, bin = bb, n = nrow(sub), score_min = min(sub$score, na.rm = TRUE),
                 score_max = max(sub$score, na.rm = TRUE), mean_score = mean(sub$score, na.rm = TRUE),
                 empirical_precision = tp / nrow(sub), precision_ci_low = ci["low"], precision_ci_high = ci["high"],
                 score_col = score_col, score_scale = if (score_is_probability) "probability" else "raw_detector_score",
                 stringsAsFactors = FALSE)
    }))
  }

  rows <- list(build_one(scored, "all"))
  for (cls in classes) {
    rows[[length(rows) + 1L]] <- build_one(scored[as.character(scored[[class_col]]) == cls, , drop = FALSE], cls)
  }
  brier <- if (score_is_probability) mean((scored$score - as.numeric(scored$true_positive))^2, na.rm = TRUE) else NA_real_
  list(
    calibration = dplyr::bind_rows(rows),
    prediction_scores = scored,
    summary = data.frame(
      score_col = score_col,
      predicted_event_n = nrow(pred),
      scored_event_n = nrow(scored),
      score_is_probability = score_is_probability,
      brier_score = brier,
      note = if (score_is_probability) "Brier score is reported because scores are bounded in [0, 1]." else "Scores are treated as raw detector ranks; empirical precision by bin is reported without probability-scale Brier scoring.",
      stringsAsFactors = FALSE
    )
  )
}

stpd_ambiguous_manual_labels <- function() {
  c("ambiguous", "manual_uncertain", "uncertain", "needs_review", "review",
    "unknown", "possible_uncertain", "boundary_uncertain")
}

stpd_filter_ambiguous_events <- function(events, ambiguous_labels = stpd_ambiguous_manual_labels(),
                                         class_col = "pattern") {
  events <- as.data.frame(events %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(events) == 0 || !(class_col %in% names(events))) {
    return(list(events = events, excluded = events[FALSE, , drop = FALSE], excluded_n = 0L))
  }
  amb <- tolower(trimws(as.character(ambiguous_labels %||% character())))
  lab <- tolower(trimws(as.character(events[[class_col]])))
  excluded <- nzchar(lab) & lab %in% amb
  list(
    events = events[!excluded, , drop = FALSE],
    excluded = events[excluded, , drop = FALSE],
    excluded_n = sum(excluded, na.rm = TRUE)
  )
}

stpd_boundary_tolerance_sensitivity <- function(pred, truth, iou_grid = c(0.10, 0.25, 0.50),
                                                class_col = "pattern",
                                                ambiguous_labels = stpd_ambiguous_manual_labels(),
                                                exclude_ambiguous = TRUE,
                                                conf_level = 0.95) {
  pred <- as.data.frame(pred %||% data.frame(), stringsAsFactors = FALSE)
  truth <- as.data.frame(truth %||% data.frame(), stringsAsFactors = FALSE)
  filtered <- stpd_filter_ambiguous_events(truth, ambiguous_labels = ambiguous_labels, class_col = class_col)
  truth_eval <- if (isTRUE(exclude_ambiguous)) filtered$events else truth
  iou_grid <- suppressWarnings(as.numeric(iou_grid %||% c(0.10, 0.25, 0.50)))
  iou_grid <- sort(unique(iou_grid[is.finite(iou_grid) & iou_grid > 0 & iou_grid <= 1]))
  if (length(iou_grid) == 0) iou_grid <- c(0.10, 0.25, 0.50)
  rows <- lapply(iou_grid, function(ii) {
    mm <- stpd_event_level_metrics(pred, truth_eval, class_col = class_col, iou_min = ii)
    mm <- stpd_event_level_metrics_ci(mm, conf_level = conf_level)
    if (nrow(mm) == 0) {
      mm <- data.frame(pattern = NA_character_, truth_n = nrow(truth_eval), predicted_n = nrow(pred),
                       true_positive_n = 0L, false_positive_n = nrow(pred), false_negative_n = nrow(truth_eval),
                       precision = NA_real_, recall = NA_real_, F1 = NA_real_, stringsAsFactors = FALSE)
      mm <- stpd_event_level_metrics_ci(mm, conf_level = conf_level)
    }
    mm$iou_min <- ii
    mm$ambiguous_excluded_n <- filtered$excluded_n
    mm$ambiguous_policy <- if (isTRUE(exclude_ambiguous)) "excluded_from_primary_metrics" else "included"
    mm
  })
  out <- dplyr::bind_rows(rows)
  front <- c("iou_min", "ambiguous_policy", "ambiguous_excluded_n")
  out[, c(front, setdiff(names(out), front)), drop = FALSE]
}

stpd_match_events_any_label_greedy <- function(a, b, iou_min = 0.25) {
  if (is.null(a) || is.null(b) || nrow(a) == 0 || nrow(b) == 0) {
    return(tibble::tibble(a_index = integer(), b_index = integer(), train = character(), iou = numeric()))
  }
  a$.a_index <- seq_len(nrow(a)); b$.b_index <- seq_len(nrow(b))
  pairs <- list()
  for (tr in intersect(unique(a$train), unique(b$train))) {
    aa <- a[a$train == tr, , drop = FALSE]
    bb <- b[b$train == tr, , drop = FALSE]
    if (nrow(aa) == 0 || nrow(bb) == 0) next
    for (ii in seq_len(nrow(aa))) {
      iou <- stpd_event_iou(aa$start_isi[ii], aa$end_isi[ii], bb$start_isi, bb$end_isi)
      ok <- which(is.finite(iou) & iou >= iou_min)
      if (length(ok) == 0) next
      for (jj in ok) {
        pairs[[length(pairs) + 1L]] <- data.frame(
          a_index = aa$.a_index[ii], b_index = bb$.b_index[jj], train = tr, iou = iou[jj],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(pairs) == 0) return(tibble::tibble(a_index = integer(), b_index = integer(), train = character(), iou = numeric()))
  cand <- dplyr::bind_rows(pairs)
  cand <- cand[order(-cand$iou), , drop = FALSE]
  used_a <- integer(); used_b <- integer(); rows <- list()
  for (ii in seq_len(nrow(cand))) {
    if (cand$a_index[ii] %in% used_a || cand$b_index[ii] %in% used_b) next
    rows[[length(rows) + 1L]] <- cand[ii, , drop = FALSE]
    used_a <- c(used_a, cand$a_index[ii]); used_b <- c(used_b, cand$b_index[ii])
  }
  if (length(rows) == 0) tibble::tibble(a_index = integer(), b_index = integer(), train = character(), iou = numeric()) else dplyr::bind_rows(rows)
}

stpd_cohen_kappa_from_labels <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) == 0) return(NA_real_)
  labs <- sort(unique(c(a, b)))
  tab <- table(factor(a, levels = labs), factor(b, levels = labs))
  n <- sum(tab)
  if (n <= 0) return(NA_real_)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / (n * n)
  if (!is.finite(pe) || abs(1 - pe) < .Machine$double.eps) return(NA_real_)
  (po - pe) / (1 - pe)
}

stpd_inter_rater_reliability <- function(rater_events, rater_col = "rater", class_col = "pattern",
                                         iou_min = 0.25,
                                         ambiguous_labels = stpd_ambiguous_manual_labels(),
                                         exclude_ambiguous = TRUE) {
  if (is.list(rater_events) && !is.data.frame(rater_events)) {
    nm <- names(rater_events) %||% paste0("rater_", seq_along(rater_events))
    rater_events <- dplyr::bind_rows(lapply(seq_along(rater_events), function(ii) {
      x <- as.data.frame(rater_events[[ii]], stringsAsFactors = FALSE)
      x[[rater_col]] <- nm[ii]
      x
    }))
  } else {
    rater_events <- as.data.frame(rater_events %||% data.frame(), stringsAsFactors = FALSE)
  }
  empty <- data.frame(rater_a = character(), rater_b = character(), event_union_n = integer(),
                      matched_event_n = integer(), same_label_match_n = integer(), label_kappa = numeric(),
                      event_f1_same_label = numeric(), mean_iou_matched = numeric(),
                      mean_boundary_abs_error_isi = numeric(), ambiguous_excluded_n = integer(),
                      stringsAsFactors = FALSE)
  if (nrow(rater_events) == 0 || !(rater_col %in% names(rater_events)) || !(class_col %in% names(rater_events))) return(empty)
  if (isTRUE(exclude_ambiguous)) {
    before <- nrow(rater_events)
    rater_events <- stpd_filter_ambiguous_events(rater_events, ambiguous_labels = ambiguous_labels, class_col = class_col)$events
    excluded_n <- before - nrow(rater_events)
  } else {
    excluded_n <- 0L
  }
  raters <- sort(unique(as.character(rater_events[[rater_col]])))
  if (length(raters) < 2) return(empty)
  rows <- list()
  for (ii in seq_len(length(raters) - 1L)) {
    for (jj in (ii + 1L):length(raters)) {
      ra <- raters[ii]; rb <- raters[jj]
      a <- rater_events[as.character(rater_events[[rater_col]]) == ra, , drop = FALSE]
      b <- rater_events[as.character(rater_events[[rater_col]]) == rb, , drop = FALSE]
      mt <- stpd_match_events_any_label_greedy(a, b, iou_min = iou_min)
      used_a <- mt$a_index %||% integer()
      used_b <- mt$b_index %||% integer()
      lab_a <- character(); lab_b <- character()
      same_label <- logical()
      boundary_abs <- numeric()
      if (nrow(mt) > 0) {
        for (kk in seq_len(nrow(mt))) {
          ai <- as.integer(mt$a_index[kk]); bi <- as.integer(mt$b_index[kk])
          la <- as.character(a[[class_col]][ai] %||% "")
          lb <- as.character(b[[class_col]][bi] %||% "")
          lab_a <- c(lab_a, la); lab_b <- c(lab_b, lb)
          same_label <- c(same_label, identical(la, lb))
          se <- abs(suppressWarnings(as.integer(a$start_isi[ai])) - suppressWarnings(as.integer(b$start_isi[bi])))
          ee <- abs(suppressWarnings(as.integer(a$end_isi[ai])) - suppressWarnings(as.integer(b$end_isi[bi])))
          boundary_abs <- c(boundary_abs, mean(c(se, ee), na.rm = TRUE))
        }
      }
      if (nrow(a) > 0) {
        ua <- setdiff(seq_len(nrow(a)), used_a)
        lab_a <- c(lab_a, as.character(a[[class_col]][ua] %||% character()))
        lab_b <- c(lab_b, rep("none", length(ua)))
      }
      if (nrow(b) > 0) {
        ub <- setdiff(seq_len(nrow(b)), used_b)
        lab_a <- c(lab_a, rep("none", length(ub)))
        lab_b <- c(lab_b, as.character(b[[class_col]][ub] %||% character()))
      }
      union_n <- length(lab_a)
      rows[[length(rows) + 1L]] <- data.frame(
        rater_a = ra,
        rater_b = rb,
        event_union_n = union_n,
        matched_event_n = nrow(mt),
        same_label_match_n = sum(same_label, na.rm = TRUE),
        label_kappa = stpd_cohen_kappa_from_labels(lab_a, lab_b),
        event_f1_same_label = if (union_n > 0) 2 * sum(same_label, na.rm = TRUE) / (nrow(a) + nrow(b)) else NA_real_,
        mean_iou_matched = if (nrow(mt) > 0) mean(mt$iou, na.rm = TRUE) else NA_real_,
        mean_boundary_abs_error_isi = if (length(boundary_abs) > 0) mean(boundary_abs, na.rm = TRUE) else NA_real_,
        ambiguous_excluded_n = excluded_n,
        iou_min = iou_min,
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

stpd_manual_label_uncertainty_report <- function(pred = NULL, truth = NULL, rater_events = NULL,
                                                 iou_grid = c(0.10, 0.25, 0.50),
                                                 iou_min = 0.25,
                                                 ambiguous_labels = stpd_ambiguous_manual_labels(),
                                                 exclude_ambiguous = TRUE,
                                                 conf_level = 0.95) {
  boundary <- if (!is.null(pred) && !is.null(truth)) {
    stpd_boundary_tolerance_sensitivity(pred, truth, iou_grid = iou_grid,
                                        ambiguous_labels = ambiguous_labels,
                                        exclude_ambiguous = exclude_ambiguous,
                                        conf_level = conf_level)
  } else {
    data.frame()
  }
  inter <- if (!is.null(rater_events)) {
    stpd_inter_rater_reliability(rater_events, iou_min = iou_min,
                                 ambiguous_labels = ambiguous_labels,
                                 exclude_ambiguous = exclude_ambiguous)
  } else {
    data.frame()
  }
  list(
    boundary_sensitivity = boundary,
    inter_rater = inter,
    meta = data.frame(
      iou_grid = paste(iou_grid, collapse = ";"),
      iou_min = iou_min,
      ambiguous_labels = paste(ambiguous_labels, collapse = ";"),
      exclude_ambiguous = isTRUE(exclude_ambiguous),
      stringsAsFactors = FALSE
    )
  )
}

stpd_score_calibration_labels <- function(pred, truth, score_col = "auto_score", class_col = "pattern",
                                          iou_min = 0.25) {
  pred <- as.data.frame(pred %||% data.frame(), stringsAsFactors = FALSE)
  truth <- as.data.frame(truth %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(pred) == 0 || !(score_col %in% names(pred))) {
    return(data.frame(score = numeric(), true_positive = integer(), stringsAsFactors = FALSE))
  }
  if (!(class_col %in% names(pred))) pred[[class_col]] <- character(nrow(pred))
  if (!(class_col %in% names(truth))) truth[[class_col]] <- character(nrow(truth))
  pred$.pred_index <- seq_len(nrow(pred))
  matches <- stpd_match_events_greedy(pred, truth, class_col = class_col, iou_min = iou_min)
  pred$score <- suppressWarnings(as.numeric(pred[[score_col]]))
  pred$true_positive <- as.integer(pred$.pred_index %in% matches$pred_index)
  pred[is.finite(pred$score), , drop = FALSE]
}

stpd_fit_score_calibrator <- function(pred, truth, score_col = "auto_score", class_col = "pattern",
                                      iou_min = 0.25,
                                      method = c("platt", "isotonic")) {
  method <- match.arg(method)
  dat <- stpd_score_calibration_labels(pred, truth, score_col = score_col, class_col = class_col, iou_min = iou_min)
  empty_summary <- data.frame(method = method, method_used = "unfit", score_col = score_col,
                              training_event_n = nrow(dat), positive_n = sum(dat$true_positive %||% 0),
                              note = "No finite training scores.", stringsAsFactors = FALSE)
  if (nrow(dat) == 0) {
    return(structure(list(method = "constant", requested_method = method, score_col = score_col,
                          probability = NA_real_, training_summary = empty_summary),
                     class = "stpd_score_calibrator"))
  }
  y <- as.integer(dat$true_positive)
  score <- suppressWarnings(as.numeric(dat$score))
  pos <- sum(y == 1, na.rm = TRUE)
  neg <- sum(y == 0, na.rm = TRUE)
  if (pos == 0 || neg == 0 || length(unique(score)) < 2L) {
    p <- mean(y, na.rm = TRUE)
    return(structure(list(method = "constant", requested_method = method, score_col = score_col,
                          probability = p,
                          training_summary = data.frame(method = method, method_used = "constant",
                                                        score_col = score_col, training_event_n = nrow(dat),
                                                        positive_n = pos, negative_n = neg,
                                                        note = "Constant calibrator used because calibration labels or scores lack variation.",
                                                        stringsAsFactors = FALSE)),
                     class = "stpd_score_calibrator"))
  }
  if (identical(method, "platt")) {
    fit <- tryCatch(suppressWarnings(stats::glm(y ~ score, family = stats::binomial())),
                    error = function(e) NULL)
    cf <- if (!is.null(fit)) stats::coef(fit) else c(NA_real_, NA_real_)
    if (is.null(fit) || any(!is.finite(cf))) {
      p <- mean(y, na.rm = TRUE)
      return(structure(list(method = "constant", requested_method = method, score_col = score_col,
                            probability = p,
                            training_summary = data.frame(method = method, method_used = "constant",
                                                          score_col = score_col, training_event_n = nrow(dat),
                                                          positive_n = pos, negative_n = neg,
                                                          note = "Constant calibrator used because Platt fit was unstable.",
                                                          stringsAsFactors = FALSE)),
                       class = "stpd_score_calibrator"))
    }
    return(structure(list(method = "platt", requested_method = method, score_col = score_col, fit = fit,
                          training_summary = data.frame(method = method, method_used = "platt",
                                                        score_col = score_col, training_event_n = nrow(dat),
                                                        positive_n = pos, negative_n = neg,
                                                        note = "Platt logistic calibrator fit on calibration split only.",
                                                        stringsAsFactors = FALSE)),
                     class = "stpd_score_calibrator"))
  }
  ord <- order(score)
  iso <- stats::isoreg(score[ord], y[ord])
  tab <- stats::aggregate(probability ~ score, data = data.frame(score = iso$x, probability = pmin(1, pmax(0, iso$yf))), FUN = mean)
  structure(list(method = "isotonic", requested_method = method, score_col = score_col,
                 isotonic_table = tab,
                 training_summary = data.frame(method = method, method_used = "isotonic",
                                               score_col = score_col, training_event_n = nrow(dat),
                                               positive_n = pos, negative_n = neg,
                                               note = "Isotonic calibrator fit on calibration split only.",
                                               stringsAsFactors = FALSE)),
            class = "stpd_score_calibrator")
}

stpd_predict_score_calibrator <- function(calibrator, pred) {
  pred <- as.data.frame(pred %||% data.frame(), stringsAsFactors = FALSE)
  if (is.null(calibrator) || nrow(pred) == 0) return(rep(NA_real_, nrow(pred)))
  score_col <- calibrator$score_col %||% "auto_score"
  score <- if (score_col %in% names(pred)) suppressWarnings(as.numeric(pred[[score_col]])) else rep(NA_real_, nrow(pred))
  out <- rep(NA_real_, length(score))
  ok <- is.finite(score)
  if (!any(ok)) return(out)
  if (identical(calibrator$method, "constant")) {
    out[ok] <- calibrator$probability %||% NA_real_
  } else if (identical(calibrator$method, "platt")) {
    out[ok] <- tryCatch(as.numeric(stats::predict(calibrator$fit, newdata = data.frame(score = score[ok]), type = "response")),
                        error = function(e) rep(NA_real_, sum(ok)))
  } else if (identical(calibrator$method, "isotonic")) {
    tab <- calibrator$isotonic_table
    out[ok] <- stats::approx(tab$score, tab$probability, xout = score[ok], rule = 2, ties = mean)$y
  }
  pmin(1, pmax(0, out))
}

stpd_score_reliability <- function(probability, observed, n_bins = 10L, conf_level = 0.95) {
  probability <- suppressWarnings(as.numeric(probability))
  observed <- suppressWarnings(as.integer(observed))
  ok <- is.finite(probability) & !is.na(observed)
  probability <- probability[ok]; observed <- observed[ok]
  if (length(probability) == 0) {
    return(list(table = data.frame(), summary = data.frame(n = 0L, ECE = NA_real_, MCE = NA_real_, Brier = NA_real_, stringsAsFactors = FALSE)))
  }
  n_bins <- suppressWarnings(as.integer(n_bins %||% 10L))
  if (!is.finite(n_bins) || n_bins < 2L) n_bins <- 10L
  qn <- min(n_bins, length(probability))
  breaks <- unique(as.numeric(stats::quantile(probability, probs = seq(0, 1, length.out = qn + 1L), na.rm = TRUE, type = 7)))
  if (length(breaks) < 2L) breaks <- c(min(probability) - 1e-12, max(probability) + 1e-12)
  bin <- as.integer(cut(probability, breaks = breaks, include.lowest = TRUE, labels = FALSE))
  rows <- lapply(sort(unique(bin[is.finite(bin)])), function(bb) {
    idx <- which(bin == bb)
    obs_rate <- mean(observed[idx], na.rm = TRUE)
    ci <- stpd_binom_wilson_ci(sum(observed[idx], na.rm = TRUE), length(idx), conf_level = conf_level)
    data.frame(bin = bb, n = length(idx), mean_probability = mean(probability[idx], na.rm = TRUE),
               observed_rate = obs_rate, observed_ci_low = ci["low"], observed_ci_high = ci["high"],
               abs_calibration_error = abs(obs_rate - mean(probability[idx], na.rm = TRUE)),
               stringsAsFactors = FALSE)
  })
  tab <- dplyr::bind_rows(rows)
  ece <- sum(tab$n / sum(tab$n) * tab$abs_calibration_error, na.rm = TRUE)
  mce <- max(tab$abs_calibration_error, na.rm = TRUE)
  brier <- mean((probability - observed)^2, na.rm = TRUE)
  list(
    table = tab,
    summary = data.frame(n = length(probability), ECE = ece, MCE = mce, Brier = brier,
                         n_bins = n_bins, stringsAsFactors = FALSE)
  )
}

stpd_score_calibration_frozen <- function(pred_cal, truth_cal, pred_val, truth_val = NULL,
                                          score_col = "auto_score", class_col = "pattern",
                                          iou_min = 0.25,
                                          method = c("platt", "isotonic"),
                                          n_bins = 10L,
                                          conf_level = 0.95) {
  method <- match.arg(method)
  calibrator <- stpd_fit_score_calibrator(pred_cal, truth_cal, score_col = score_col,
                                          class_col = class_col, iou_min = iou_min, method = method)
  pred_val <- as.data.frame(pred_val %||% data.frame(), stringsAsFactors = FALSE)
  scored <- pred_val
  scored$calibrated_probability <- stpd_predict_score_calibrator(calibrator, scored)
  if (!is.null(truth_val)) {
    lab <- stpd_score_calibration_labels(scored, truth_val, score_col = score_col, class_col = class_col, iou_min = iou_min)
    if (nrow(lab) > 0) {
      lab$calibrated_probability <- stpd_predict_score_calibrator(calibrator, lab)
      rel <- stpd_score_reliability(lab$calibrated_probability, lab$true_positive, n_bins = n_bins, conf_level = conf_level)
    } else {
      rel <- stpd_score_reliability(numeric(), integer(), n_bins = n_bins, conf_level = conf_level)
    }
  } else {
    rel <- stpd_score_reliability(numeric(), integer(), n_bins = n_bins, conf_level = conf_level)
  }
  summary <- calibrator$training_summary
  for (nm in names(rel$summary)) summary[[paste0("validation_", nm)]] <- rel$summary[[nm]][1]
  summary$freeze_policy <- "fit_on_calibration_apply_to_validation"
  list(calibrator = calibrator, calibrated_predictions = scored,
       reliability = rel$table, summary = summary)
}

stpd_surrogate_clear_labels <- function(dat) {
  out <- dat
  for (nm in c("pattern_manual", "pattern_manual_negative", "pattern_auto", "pattern_final", "manual_label", "manual_negative")) {
    if (nm %in% names(out)) out[[nm]] <- rep("", nrow(out))
  }
  if ("auto_score" %in% names(out)) out$auto_score <- rep(NA_real_, nrow(out))
  out
}

stpd_surrogate_train <- function(dat, method = c("isi_permutation", "renewal", "block_isi_shuffle"),
                                 block_length = 10L) {
  method <- match.arg(method)
  out <- stpd_surrogate_clear_labels(dat)
  if (is.null(out) || nrow(out) <= 2 || !("timestamp_sec" %in% names(out))) return(out)
  ts <- suppressWarnings(as.numeric(out$timestamp_sec))
  if (any(!is.finite(ts)) || any(diff(ts) <= 0, na.rm = TRUE)) return(out)
  isi <- diff(ts)
  if (length(isi) == 0 || any(!is.finite(isi)) || any(isi <= 0)) return(out)
  block_length <- suppressWarnings(as.integer(block_length %||% 10L))
  if (!is.finite(block_length) || block_length < 1L) block_length <- 10L
  sur_isi <- switch(
    method,
    isi_permutation = sample(isi, length(isi), replace = FALSE),
    renewal = sample(isi, length(isi), replace = TRUE),
    block_isi_shuffle = {
      block_id <- ceiling(seq_along(isi) / block_length)
      blocks <- split(isi, block_id)
      unlist(blocks[sample(seq_along(blocks), length(blocks), replace = FALSE)], use.names = FALSE)
    }
  )
  sur_ts <- c(ts[1], ts[1] + cumsum(sur_isi))
  out$timestamp_sec <- sur_ts
  if ("ISI_sec" %in% names(out)) out$ISI_sec <- c(NA_real_, diff(sur_ts))
  out
}

stpd_surrogate_dataset <- function(ds, selected_trains = NULL,
                                   method = c("isi_permutation", "renewal", "block_isi_shuffle"),
                                   block_length = 10L) {
  method <- match.arg(method)
  out <- ds
  target <- intersect(as.character(selected_trains %||% names(out$trains)), names(out$trains))
  for (tr in target) out$trains[[tr]] <- stpd_surrogate_train(out$trains[[tr]], method = method, block_length = block_length)
  out
}

stpd_count_events_by_pattern <- function(events) {
  events <- as.data.frame(events %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(events) == 0 || !("pattern" %in% names(events))) {
    return(data.frame(pattern = character(), event_n = integer(), stringsAsFactors = FALSE))
  }
  tab <- as.data.frame(table(pattern = as.character(events$pattern)), stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 0, , drop = FALSE]
  names(tab)[names(tab) == "Freq"] <- "event_n"
  tab[order(tab$pattern), , drop = FALSE]
}

stpd_detector_surrogate_false_alarm <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                                n_surrogates = 99L,
                                                methods = c("isi_permutation", "renewal"),
                                                metric_mode = c("strict_high_confidence", "candidate_family"),
                                                seed = NULL, block_length = 10L,
                                                collect_diagnostics = FALSE) {
  metric_mode <- match.arg(metric_mode)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_detector_surrogate_false_alarm(): ds must be a dataset with trains.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) stop("No target trains found for surrogate false-alarm analysis.", call. = FALSE)
  n_surrogates <- suppressWarnings(as.integer(n_surrogates %||% 99L))
  if (!is.finite(n_surrogates) || n_surrogates < 1L) n_surrogates <- 99L
  methods <- intersect(as.character(methods %||% "isi_permutation"), c("isi_permutation", "renewal", "block_isi_shuffle"))
  if (length(methods) == 0) methods <- "isi_permutation"

  params_eval <- stpd_freeze_thresholds_for_trains(ds, params, calibration_trains = target, freeze_scope = "surrogate_observed_threshold_freeze")
  observed_ds <- stpd_detect(ds, params_eval, selected_trains = target, lock_manual = FALSE, collect_diagnostics = collect_diagnostics)
  observed_events <- stpd_extract_events_by_source(observed_ds, params_eval, source = "auto", selected_trains = target, metric_mode = metric_mode)
  observed_counts <- stpd_count_events_by_pattern(observed_events)
  observed_counts <- dplyr::bind_rows(observed_counts, data.frame(pattern = "all", event_n = nrow(observed_events), stringsAsFactors = FALSE))

  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  count_rows <- list()
  for (method in methods) {
    for (ss in seq_len(n_surrogates)) {
      sur_ds <- stpd_surrogate_dataset(ds, selected_trains = target, method = method, block_length = block_length)
      sur_out <- stpd_detect(sur_ds, params_eval, selected_trains = target, lock_manual = FALSE, collect_diagnostics = collect_diagnostics)
      sur_events <- stpd_extract_events_by_source(sur_out, params_eval, source = "auto", selected_trains = target, metric_mode = metric_mode)
      cc <- stpd_count_events_by_pattern(sur_events)
      cc <- dplyr::bind_rows(cc, data.frame(pattern = "all", event_n = nrow(sur_events), stringsAsFactors = FALSE))
      cc$surrogate_method <- method
      cc$surrogate_id <- ss
      count_rows[[length(count_rows) + 1L]] <- cc
    }
  }
  surrogate_counts <- if (length(count_rows) > 0) dplyr::bind_rows(count_rows) else data.frame()
  patterns <- sort(unique(c(as.character(observed_counts$pattern), as.character(surrogate_counts$pattern))))
  summary_rows <- list()
  for (method in methods) {
    for (pat in patterns) {
      obs <- suppressWarnings(as.numeric(observed_counts$event_n[as.character(observed_counts$pattern) == pat][1] %||% 0))
      vals <- surrogate_counts$event_n[as.character(surrogate_counts$surrogate_method) == method & as.character(surrogate_counts$pattern) == pat]
      vals <- suppressWarnings(as.numeric(vals))
      if (length(vals) < n_surrogates) vals <- c(vals, rep(0, n_surrogates - length(vals)))
      p_high <- (1 + sum(vals >= obs, na.rm = TRUE)) / (length(vals) + 1)
      null_mean <- mean(vals, na.rm = TRUE)
      fdr_estimate <- if (is.finite(obs) && obs > 0) min(1, null_mean / obs) else NA_real_
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        surrogate_method = method,
        pattern = pat,
        observed_event_n = obs,
        surrogate_mean_event_n = null_mean,
        surrogate_q95_event_n = stats::quantile(vals, probs = 0.95, na.rm = TRUE, names = FALSE, type = 6),
        empirical_p_count_ge_observed = p_high,
        detector_level_fdr_estimate = fdr_estimate,
        fdr_interpretation = if (is.finite(obs) && obs > 0) "estimated expected surrogate events divided by observed detector events, capped at 1" else "not estimable when observed detector event count is zero",
        n_surrogates = length(vals),
        threshold_scope = params_eval$event_grammar$threshold_resolution_scope %||% "",
        stringsAsFactors = FALSE
      )
    }
  }
  summary <- if (length(summary_rows) > 0) dplyr::bind_rows(summary_rows) else data.frame()
  if (nrow(summary) > 0) summary$empirical_q_BH <- stats::p.adjust(summary$empirical_p_count_ge_observed, method = "BH")
  list(
    observed_counts = observed_counts,
    surrogate_counts = surrogate_counts,
    summary = summary,
    observed_events = observed_events,
    selected_trains = target,
    metric_mode = metric_mode
  )
}

stpd_scientific_validation_report <- function(ds, params = default_params_sec(), validation_fraction = 0.25,
                                              seed = 1L, iou_min = 0.25,
                                              metric_mode = c("strict_high_confidence", "candidate_family"),
                                              use_learned_ranges = TRUE,
                                              threshold_freeze = c("calibration", "all", "none"),
                                              conf_level = 0.95,
                                              bootstrap_ci = TRUE,
                                              n_bootstrap = 200L,
                                              bootstrap_seed = NULL,
                                              score_calibration_bins = 10L,
                                              score_calibrator = c("platt", "isotonic", "none"),
                                              ambiguous_manual_labels = stpd_ambiguous_manual_labels(),
                                              exclude_ambiguous_manual = TRUE,
                                              iou_sensitivity_grid = c(0.10, 0.25, 0.50),
                                              surrogate_false_alarm = FALSE,
                                              n_surrogates = 99L,
                                              surrogate_methods = c("isi_permutation", "renewal")) {
  metric_mode <- match.arg(metric_mode)
  threshold_freeze <- match.arg(threshold_freeze)
  score_calibrator <- match.arg(score_calibrator)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_scientific_validation_report(): ds must be a dataset with trains.", call. = FALSE)
  params_eval <- if (isTRUE(use_learned_ranges)) params else strip_learned_ranges_for_eval(params)
  split <- stpd_split_trains_by_manual_events(ds, params_eval, validation_fraction = validation_fraction, seed = seed, metric_mode = metric_mode)
  manual_event_n <- sum(split$manual_event_n, na.rm = TRUE)
  target_detect <- split$train[split$split %in% c("calibration", "validation")]
  threshold_training_split <- threshold_freeze
  threshold_training_trains <- character()
  threshold_freeze_status <- "not_requested"
  if (!identical(threshold_freeze, "none") && length(target_detect) > 0) {
    threshold_training_trains <- if (identical(threshold_freeze, "calibration")) split$train[split$split == "calibration"] else target_detect
    if (length(threshold_training_trains) == 0 && identical(threshold_freeze, "calibration")) {
      threshold_training_trains <- target_detect
      threshold_freeze_status <- "fallback_no_calibration_train"
      threshold_training_split <- "all_labeled_fallback"
    } else {
      threshold_freeze_status <- "frozen"
    }
    params_eval <- stpd_freeze_thresholds_for_trains(
      ds, params_eval,
      calibration_trains = threshold_training_trains,
      freeze_scope = paste0("scientific_validation_", threshold_training_split)
    )
  }
  meta <- data.frame(
    validation_run_id = paste0("validation_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    metric_mode = metric_mode,
    iou_min = iou_min,
    validation_fraction = validation_fraction,
    seed = seed,
    learned_ranges_used = isTRUE(use_learned_ranges),
    threshold_freeze = threshold_freeze,
    threshold_freeze_status = threshold_freeze_status,
    threshold_training_split = threshold_training_split,
    threshold_training_train_n = length(threshold_training_trains),
    threshold_training_trains = paste(threshold_training_trains, collapse = ";"),
    ci_conf_level = suppressWarnings(as.numeric(conf_level %||% 0.95)),
    bootstrap_ci = isTRUE(bootstrap_ci),
    n_bootstrap = suppressWarnings(as.integer(n_bootstrap %||% 200L)),
    score_calibration_bins = suppressWarnings(as.integer(score_calibration_bins %||% 10L)),
    score_calibrator = score_calibrator,
    exclude_ambiguous_manual = isTRUE(exclude_ambiguous_manual),
    iou_sensitivity_grid = paste(iou_sensitivity_grid, collapse = ";"),
    surrogate_false_alarm = isTRUE(surrogate_false_alarm),
    n_surrogates = suppressWarnings(as.integer(n_surrogates %||% 99L)),
    manual_event_n = manual_event_n,
    interpretation = if (manual_event_n == 0) "No manual events available; validation cannot estimate performance." else "Calibration/validation report based on manual labels. Use validation split for methods reporting; calibration split is for tuning feedback.",
    stringsAsFactors = FALSE
  )
  if (manual_event_n == 0) {
    return(list(meta = meta, split = split, calibration_metrics = data.frame(), validation_metrics = data.frame(), overfit_report = data.frame(), matches_calibration = data.frame(), matches_validation = data.frame(), truth_events = data.frame(), predicted_events = data.frame(),
                bootstrap_ci_calibration = data.frame(), bootstrap_ci_validation = data.frame(), score_calibration_validation = data.frame(), score_calibration_summary = data.frame(), surrogate_false_alarm_summary = data.frame(), surrogate_false_alarm_counts = data.frame()))
  }
  # Shadow detector: do not lock manual labels, so AUTO predictions can be generated on manual intervals without modifying the caller's ds.
  pred_ds <- stpd_detect(ds, params_eval, selected_trains = target_detect, lock_manual = FALSE, collect_diagnostics = TRUE)
  truth_raw <- stpd_extract_events_by_source(ds, params_eval, source = "manual", selected_trains = split$train, metric_mode = metric_mode)
  pred <- stpd_extract_events_by_source(pred_ds, params_eval, source = "auto", selected_trains = split$train, metric_mode = metric_mode)
  add_split <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(x)
    x$split <- split$split[match(x$train, split$train)]
    x
  }
  truth_raw <- add_split(truth_raw); pred <- add_split(pred)
  truth_filter <- stpd_filter_ambiguous_events(truth_raw, ambiguous_labels = ambiguous_manual_labels)
  truth <- if (isTRUE(exclude_ambiguous_manual)) truth_filter$events else truth_raw
  meta$manual_event_n_included <- nrow(truth %||% data.frame())
  meta$manual_ambiguous_excluded_n <- truth_filter$excluded_n
  truth_cal <- truth[truth$split == "calibration", , drop = FALSE]
  pred_cal <- pred[pred$split == "calibration", , drop = FALSE]
  truth_val <- truth[truth$split == "validation", , drop = FALSE]
  pred_val <- pred[pred$split == "validation", , drop = FALSE]
  truth_val_raw <- truth_raw[truth_raw$split == "validation", , drop = FALSE]
  m_cal <- stpd_match_events_greedy(pred_cal, truth_cal, iou_min = iou_min)
  m_val <- stpd_match_events_greedy(pred_val, truth_val, iou_min = iou_min)
  metrics_cal <- stpd_event_level_metrics_ci(stpd_event_level_metrics(pred_cal, truth_cal, iou_min = iou_min), conf_level = conf_level)
  metrics_val <- stpd_event_level_metrics_ci(stpd_event_level_metrics(pred_val, truth_val, iou_min = iou_min), conf_level = conf_level)
  boot_cal <- list(summary = data.frame(), bootstrap = data.frame())
  boot_val <- list(summary = data.frame(), bootstrap = data.frame())
  if (isTRUE(bootstrap_ci)) {
    bs_seed <- bootstrap_seed %||% (seed + 1000L)
    if (nrow(metrics_cal %||% data.frame()) > 0) {
      boot_cal <- stpd_event_level_cluster_bootstrap(pred_cal, truth_cal, iou_min = iou_min, n_bootstrap = n_bootstrap, seed = bs_seed, conf_level = conf_level)
      metrics_cal <- stpd_event_level_merge_bootstrap_ci(metrics_cal, boot_cal$summary)
    }
    if (nrow(metrics_val %||% data.frame()) > 0) {
      boot_val <- stpd_event_level_cluster_bootstrap(pred_val, truth_val, iou_min = iou_min, n_bootstrap = n_bootstrap, seed = bs_seed + 1L, conf_level = conf_level)
      metrics_val <- stpd_event_level_merge_bootstrap_ci(metrics_val, boot_val$summary)
    }
  }
  score_cal <- stpd_score_calibration(pred_val, truth_val, iou_min = iou_min, n_bins = score_calibration_bins, conf_level = conf_level)
  frozen_score_cal <- if (!identical(score_calibrator, "none")) {
    stpd_score_calibration_frozen(pred_cal, truth_cal, pred_val, truth_val,
                                  iou_min = iou_min, method = score_calibrator,
                                  n_bins = score_calibration_bins, conf_level = conf_level)
  } else {
    list(reliability = data.frame(), summary = data.frame(), calibrated_predictions = data.frame())
  }
  manual_unc <- stpd_manual_label_uncertainty_report(
    pred = pred_val,
    truth = truth_val_raw,
    iou_grid = iou_sensitivity_grid,
    iou_min = iou_min,
    ambiguous_labels = ambiguous_manual_labels,
    exclude_ambiguous = exclude_ambiguous_manual,
    conf_level = conf_level
  )
  surr <- NULL
  if (isTRUE(surrogate_false_alarm)) {
    surr <- stpd_detector_surrogate_false_alarm(
      ds, params_eval, selected_trains = target_detect, n_surrogates = n_surrogates,
      methods = surrogate_methods, metric_mode = metric_mode, seed = seed + 2000L,
      collect_diagnostics = FALSE
    )
  }
  overfit <- stpd_overfit_report(metrics_cal, metrics_val)
  list(meta = meta, split = split,
       calibration_metrics = metrics_cal, validation_metrics = metrics_val, overfit_report = overfit,
       matches_calibration = m_cal, matches_validation = m_val,
       truth_events = truth, predicted_events = pred,
       bootstrap_ci_calibration = boot_cal$summary,
       bootstrap_ci_validation = boot_val$summary,
       bootstrap_replicates_calibration = boot_cal$bootstrap,
       bootstrap_replicates_validation = boot_val$bootstrap,
       score_calibration_validation = score_cal$calibration,
       score_calibration_predictions = score_cal$prediction_scores,
       score_calibration_summary = score_cal$summary,
       frozen_score_calibration_validation = frozen_score_cal$reliability,
       frozen_score_calibration_summary = frozen_score_cal$summary,
       frozen_score_calibrated_predictions = frozen_score_cal$calibrated_predictions,
       manual_uncertainty_boundary_sensitivity = manual_unc$boundary_sensitivity,
       manual_uncertainty_inter_rater = manual_unc$inter_rater,
       manual_uncertainty_meta = manual_unc$meta,
       surrogate_false_alarm_summary = if (!is.null(surr)) surr$summary else data.frame(),
       surrogate_false_alarm_counts = if (!is.null(surr)) surr$surrogate_counts else data.frame(),
       surrogate_false_alarm_observed_counts = if (!is.null(surr)) surr$observed_counts else data.frame())
}

stpd_write_scientific_validation_exports <- function(report, out_dir) {
  if (is.null(report) || !is.list(report)) return(invisible(FALSE))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_safe(report$meta %||% data.frame(), file.path(out_dir, "Scientific_validation_meta.csv"))
  write_csv_safe(report$split %||% data.frame(), file.path(out_dir, "Scientific_validation_split.csv"))
  write_csv_safe(report$calibration_metrics %||% data.frame(), file.path(out_dir, "Scientific_validation_calibration_metrics.csv"))
  write_csv_safe(report$validation_metrics %||% data.frame(), file.path(out_dir, "Scientific_validation_validation_metrics.csv"))
  write_csv_safe(report$overfit_report %||% data.frame(), file.path(out_dir, "Scientific_validation_overfit_report.csv"))
  write_csv_safe(report$matches_calibration %||% data.frame(), file.path(out_dir, "Scientific_validation_matches_calibration.csv"))
  write_csv_safe(report$matches_validation %||% data.frame(), file.path(out_dir, "Scientific_validation_matches_validation.csv"))
  write_csv_safe(report$bootstrap_ci_calibration %||% data.frame(), file.path(out_dir, "Scientific_validation_bootstrap_ci_calibration.csv"))
  write_csv_safe(report$bootstrap_ci_validation %||% data.frame(), file.path(out_dir, "Scientific_validation_bootstrap_ci_validation.csv"))
  write_csv_safe(report$score_calibration_validation %||% data.frame(), file.path(out_dir, "Scientific_validation_score_calibration_validation.csv"))
  write_csv_safe(report$score_calibration_summary %||% data.frame(), file.path(out_dir, "Scientific_validation_score_calibration_summary.csv"))
  write_csv_safe(report$frozen_score_calibration_validation %||% data.frame(), file.path(out_dir, "Scientific_validation_frozen_score_calibration_validation.csv"))
  write_csv_safe(report$frozen_score_calibration_summary %||% data.frame(), file.path(out_dir, "Scientific_validation_frozen_score_calibration_summary.csv"))
  write_csv_safe(report$manual_uncertainty_boundary_sensitivity %||% data.frame(), file.path(out_dir, "Scientific_validation_manual_boundary_sensitivity.csv"))
  write_csv_safe(report$manual_uncertainty_inter_rater %||% data.frame(), file.path(out_dir, "Scientific_validation_inter_rater_reliability.csv"))
  write_csv_safe(report$surrogate_false_alarm_summary %||% data.frame(), file.path(out_dir, "Scientific_validation_surrogate_false_alarm_summary.csv"))
  write_csv_safe(report$surrogate_false_alarm_counts %||% data.frame(), file.path(out_dir, "Scientific_validation_surrogate_false_alarm_counts.csv"))
  invisible(TRUE)
}

# Lightweight validation wrappers used by the canonical engine.
# These avoid recursive detector calls inside stpd_detect(). Full hold-out
# calibration/validation remains available through stpd_scientific_validation_report().

stpd_manual_events <- function(ds, params = default_params_sec(), selected_trains = NULL,
                               metric_mode = c("strict_high_confidence", "candidate_family", "review_assisted")) {
  metric_mode <- match.arg(metric_mode)
  if (metric_mode == "review_assisted") metric_mode <- "strict_high_confidence"
  stpd_extract_events_by_source(ds, params, source = "manual", selected_trains = selected_trains, metric_mode = metric_mode)
}

stpd_predicted_events <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                  metric_mode = c("strict_high_confidence", "candidate_family", "review_assisted"),
                                  prediction_source = c("auto", "final")) {
  metric_mode <- match.arg(metric_mode)
  prediction_source <- match.arg(prediction_source)
  if (metric_mode == "review_assisted") metric_mode <- "strict_high_confidence"
  if (is.null(ds) || is.null(ds$trains)) return(tibble::tibble())
  if (identical(prediction_source, "auto")) {
    return(stpd_extract_events_by_source(ds, params, source = "auto", selected_trains = selected_trains, metric_mode = metric_mode))
  }
  ev <- ds$results$events %||% NULL
  if (is.null(ev) || nrow(ev) == 0) {
    return(stpd_extract_events_by_source(ds, params, source = "final", selected_trains = selected_trains, metric_mode = metric_mode))
  }
  if (!is.null(selected_trains)) ev <- ev[ev$train %in% selected_trains, , drop = FALSE]
  stpd_events_apply_metric_mode(ev, mode = metric_mode)
}

stpd_event_level_validation <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                        split_table = NULL, iou_min = 0.25,
                                        metric_mode = c("strict_high_confidence", "candidate_family", "review_assisted"),
                                        prediction_source = c("auto", "final")) {
  metric_mode <- match.arg(metric_mode)
  prediction_source <- match.arg(prediction_source)
  if (!stpd_has_manual_labels(ds, selected_trains = selected_trains)) {
    return(tibble::tibble(
      split = "all", metric_mode = metric_mode, pattern = NA_character_,
      truth_n = NA_integer_, predicted_n = NA_integer_, true_positive_n = NA_integer_,
      false_positive_n = NA_integer_, false_negative_n = NA_integer_,
      precision = NA_real_, recall = NA_real_, F1 = NA_real_,
      note = "No MANUAL labels available; event-level validation not computed."
    ))
  }
  target <- selected_trains %||% names(ds$trains)
  if (is.null(split_table)) split_table <- data.frame(train = target, split = "all", stringsAsFactors = FALSE)
  pred_all <- stpd_predicted_events(ds, params = params, selected_trains = target, metric_mode = metric_mode,
                                    prediction_source = prediction_source)
  truth_all <- stpd_manual_events(ds, params = params, selected_trains = target, metric_mode = metric_mode)
  splits <- unique(as.character(split_table$split))
  rows <- lapply(splits, function(sp) {
    tr <- split_table$train[split_table$split == sp]
    pred <- pred_all[pred_all$train %in% tr, , drop = FALSE]
    truth <- truth_all[truth_all$train %in% tr, , drop = FALSE]
    out <- stpd_event_level_metrics(pred, truth, class_col = "pattern", iou_min = iou_min)
    if (is.null(out) || nrow(out) == 0) {
      out <- tibble::tibble(pattern = NA_character_, truth_n = nrow(truth), predicted_n = nrow(pred), true_positive_n = 0L,
                            false_positive_n = nrow(pred), false_negative_n = nrow(truth), precision = NA_real_, recall = NA_real_, F1 = NA_real_)
    }
    out$split <- sp
    out$metric_mode <- metric_mode
    out$note <- if (identical(prediction_source, "final")) {
      "final-source agreement audit: predictions include MANUAL-first final labels and must not be interpreted as unbiased detector performance."
    } else if (metric_mode == "candidate_family") {
      "auto-source candidate-family: burst/long_burst/possible_burst are merged into burst_family; use as candidate sensitivity, not high-confidence burst accuracy."
    } else {
      "auto-source strict: possible_burst remains a separate review class."
    }
    out
  })
  ans <- dplyr::bind_rows(rows)
  ans[, c("split", "metric_mode", "pattern", "truth_n", "predicted_n", "true_positive_n", "false_positive_n", "false_negative_n", "precision", "recall", "F1", "note"), drop = FALSE]
}

stpd_result_consistency_check_core <- function(ds) {
  issues <- list()
  add <- function(severity, component, issue, detail = "") {
    issues[[length(issues) + 1L]] <<- data.frame(severity = severity, component = component, issue = issue, detail = detail, stringsAsFactors = FALSE)
  }
  if (is.null(ds) || is.null(ds$results)) {
    add("error", "dataset", "results missing", "Run stpd_detect() before auditing results.")
    return(dplyr::bind_rows(issues))
  }
  res <- ds$results
  cand <- res$candidate_ledger %||% data.frame()
  ev <- res$events %||% data.frame()
  evt <- res$event_audit %||% data.frame()
  feat <- res$candidate_features %||% data.frame()
  dec <- res$final_decisions %||% res$final_classification_audit %||% data.frame()
  if (nrow(ev) > 0 && nrow(evt) == 0) add("warning", "event_audit", "event audit missing", "Final events exist but event_audit is empty.")
  if (nrow(cand) > 0 && nrow(feat) == 0) add("warning", "candidate_features", "candidate feature table missing", "Candidates exist but candidate_features is empty.")
  if (nrow(feat) > 0 && nrow(dec) == 0) add("warning", "final_decisions", "final classification audit missing", "Candidate features exist but final_decisions is empty.")
  if (nrow(cand) > 0) {
    if ("candidate_source" %in% names(cand) && any(grepl("final_event", as.character(cand$candidate_source)), na.rm = TRUE)) {
      add("error", "candidate_ledger", "candidate ledger contains final-event rows", "Candidate ledger should contain detector candidates only; use event_audit for final events.")
    }
    if (!("run_id" %in% names(cand))) add("warning", "candidate_ledger", "run_id missing", "Candidate rows should be traceable to a detector run.")
    if (!("params_hash" %in% names(cand))) add("warning", "candidate_ledger", "params_hash missing", "Candidate rows should include parameter hash.")
  }
  if (nrow(ev) > 0 && "pattern" %in% names(ev) && any(ev$pattern == "possible_burst", na.rm = TRUE)) {
    add("info", "events", "review candidates present", "possible_burst is a review class and should not be silently merged into high-confidence burst metrics.")
  }
  if (length(issues) == 0) return(data.frame(severity = "ok", component = "all", issue = "no consistency issues detected", detail = "", stringsAsFactors = FALSE))
  dplyr::bind_rows(issues)
}

stpd_result_consistency_check <- stpd_result_consistency_check_core

stpd_scientific_validation_summary <- function(ds, params = default_params_sec()) {
  res <- ds$results %||% list()
  qc <- ds$quality %||% data.frame()
  has_manual <- stpd_has_manual_labels(ds)
  ev <- res$events %||% data.frame()
  cand <- res$candidate_ledger %||% data.frame()
  stationarity_warn <- if (nrow(qc) > 0 && "stationarity_status" %in% names(qc)) sum(qc$stationarity_status %in% c("warning", "nonstationary", "high_drift", "error"), na.rm = TRUE) else NA_integer_
  data.frame(
    item = c("tool_role", "manual_labels_available", "candidate_count", "event_count", "review_candidate_count", "stationarity_warning_trains", "recommended_validation"),
    value = c(
      "candidate event generator + semi-supervised review platform; not an unbiased final truth classifier",
      as.character(has_manual),
      as.character(nrow(cand)),
      as.character(nrow(ev)),
      as.character(if (nrow(ev) > 0 && "pattern" %in% names(ev)) sum(ev$pattern == "possible_burst", na.rm = TRUE) else 0L),
      as.character(stationarity_warn),
      "Use strict high-confidence metrics and held-out train/dataset validation for publication-level analysis. Candidate-family metrics estimate candidate sensitivity only."
    ),
    stringsAsFactors = FALSE
  )
}

stpd_scientific_validation_method_readme <- function() {
  c(
    "Spike Train Pattern Detector result package",
    "",
    "Role:",
    "  This software is a candidate-event generator and semi-supervised review platform. It is not an unbiased final truth classifier.",
    "",
    "Key result layers:",
    "  Candidate_ledger.csv: detector candidates before final event interpretation.",
    "  Event_audit.csv: final event-layer audit rows.",
    "  Events_high_confidence.csv: high-confidence events excluding review-only possible_burst.",
    "  Events_review_candidates.csv: possible_burst and other review candidates.",
    "  Events_burst_family_candidates.csv: burst/long_burst/possible_burst family candidates for candidate sensitivity analysis.",
    "",
    "Validation:",
    "  Strict metrics keep possible_burst as a separate review class.",
    "  Candidate-family metrics merge burst/long_burst/possible_burst and should not be reported as high-confidence burst accuracy.",
    "  If train-specific thresholds or learned ranges were tuned using manual labels, validation on the same labels is calibration feedback, not unbiased performance.",
    "",
    "Recommended publication workflow:",
    "  1. Report artifact/refractory thresholds and duplicate timestamp policy.",
    "  2. Report whether train-specific thresholds or learned ranges were used.",
    "  3. Report high-confidence and review-candidate results separately.",
    "  4. Use held-out train/dataset evaluation where possible."
  )
}
