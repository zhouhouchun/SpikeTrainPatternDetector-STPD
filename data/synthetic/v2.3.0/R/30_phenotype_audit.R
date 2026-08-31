score_burst_window <- function(isis, start_idx, end_idx,
                               flank_n = parameters$phenotype_audit$flank_isis_each_side,
                               allowed_flank_indices = NULL) {
  n <- length(isis)
  internal <- isis[start_idx:end_idx]
  left_idx <- if (start_idx > 1L) max(1L, start_idx - flank_n):(start_idx - 1L) else integer()
  right_idx <- if (end_idx < n) (end_idx + 1L):min(n, end_idx + flank_n) else integer()
  if (!is.null(allowed_flank_indices)) {
    left_idx <- intersect(left_idx, allowed_flank_indices)
    right_idx <- intersect(right_idx, allowed_flank_indices)
  }
  left <- isis[left_idx]
  right <- isis[right_idx]
  flank <- c(left, right)
  if (length(flank) < 2L || length(internal) < 2L) return(NULL)
  internal_median <- stats::median(internal)
  flank_median <- stats::median(flank)
  internal_mean <- mean(internal)
  flank_mean <- mean(flank)
  contrast <- flank_median / internal_median
  mean_rate_ratio <- flank_mean / internal_mean
  internal_cv <- if (length(internal) > 1L) stats::sd(internal) / internal_mean else 0
  compactness <- stats::quantile(internal, 0.75, names = FALSE) / flank_median
  duration <- sum(internal)
  score <- log(pmax(contrast, 1e-12)) * sqrt(length(internal)) +
    0.25 * log(pmax(mean_rate_ratio, 1e-12)) - 0.10 * internal_cv
  data.frame(
    Local_Start_ISI = start_idx, Local_End_ISI = end_idx,
    Left_Flank_N_Used = length(left), Right_Flank_N_Used = length(right),
    N_ISIs = length(internal), N_Spikes = length(internal) + 1L,
    Duration_u = duration, Internal_Median_ISI_u = internal_median,
    Flank_Median_ISI_u = flank_median, Median_Contrast = contrast,
    Mean_Rate_Ratio = mean_rate_ratio, Internal_CV = internal_cv,
    Compactness = compactness, Evidence_Score = score, stringsAsFactors = FALSE
  )
}

scan_burst_windows <- function(isis) {
  min_w <- parameters$phenotype_audit$minimum_window_isis
  max_w <- min(parameters$phenotype_audit$maximum_window_isis, length(isis) - 1L)
  if (max_w < min_w) return(data.frame())
  out <- list()
  for (w in seq.int(min_w, max_w)) {
    for (s in seq_len(length(isis) - w + 1L)) {
      candidate <- score_burst_window(isis, s, s + w - 1L)
      if (!is.null(candidate)) out[[length(out) + 1L]] <- candidate
    }
  }
  if (!length(out)) data.frame() else do.call(rbind, out)
}

best_burst_window <- function(isis) {
  candidates <- scan_burst_windows(isis)
  if (!nrow(candidates)) return(NULL)
  candidates[which.max(candidates$Evidence_Score), , drop = FALSE]
}

burst_flank_pattern <- function(left_n, right_n) {
  if (left_n > 0L && right_n > 0L) return("both_sides")
  if (left_n > 0L) return("left_only")
  if (right_n > 0L) return("right_only")
  "none"
}

fit_phenotype_thresholds <- function(states, runs, events) {
  dev_states <- states[states$Template_ID %in% parameters$development_templates &
                         states$State_Label == "Broad_HFS", , drop = FALSE]
  if (!nrow(dev_states)) stop("No development HFS states are available for null calibration.", call. = FALSE)
  quantile_rows <- function(scores, purpose, context, flank_pattern, window_n, null_model, set_id) data.frame(
    Threshold_Set_ID = set_id, Threshold_Purpose = purpose,
    Phenotype_Null_Context = context, Phenotype_Null_Flank_Pattern = flank_pattern,
    Phenotype_Null_Window_N_ISIs = window_n,
    Threshold_Name = c("ambiguous_lower", "clear_lower"),
    Quantile = c(parameters$phenotype_audit$ambiguous_quantile, parameters$phenotype_audit$clear_quantile),
    Evidence_Score = as.numeric(stats::quantile(
      scores, probs = c(parameters$phenotype_audit$ambiguous_quantile, parameters$phenotype_audit$clear_quantile),
      names = FALSE, type = 8)),
    Null_Model = null_model, Calibration_Split = "development_only",
    N_Null_Surrogates = length(scores), Detector_Output_Used = FALSE, stringsAsFactors = FALSE)

  threshold_rows <- list()
  dev_events <- events[events$Template_ID %in% parameters$development_templates &
                         events$Event_Label == "Burst", , drop = FALSE]
  dev_events$Flank_Pattern <- mapply(burst_flank_pattern,
                                     dev_events$Observed_Left_Context_ISIs,
                                     dev_events$Observed_Right_Context_ISIs)
  window_sizes <- seq.int(parameters$phenotype_audit$minimum_window_isis,
                          parameters$phenotype_audit$maximum_window_isis - 1L)
  groups <- unique(dev_events[, c("Event_Context", "Flank_Pattern")])
  groups <- groups[order(groups$Event_Context, groups$Flank_Pattern), , drop = FALSE]
  for (group_index in seq_len(nrow(groups))) {
    context <- groups$Event_Context[group_index]
    flank_pattern <- groups$Flank_Pattern[group_index]
    context_events <- dev_events[dev_events$Event_Context == context &
                                   dev_events$Flank_Pattern == flank_pattern, , drop = FALSE]
    parameter_rows <- runs[match(context_events$Run_ID, runs$Run_ID), , drop = FALSE]
    parameter_rows <- parameter_rows[is.finite(parameter_rows$Gamma_Shape), , drop = FALSE]
    if (!nrow(parameter_rows)) stop("A Burst context lacks development null parameters.", call. = FALSE)
    for (window_n in window_sizes) {
      set.seed(parameters$phenotype_null_seed + 1000L * group_index + window_n)
      scores <- numeric(parameters$phenotype_audit$null_surrogates)
      flank_n <- parameters$phenotype_audit$flank_isis_each_side
      for (i in seq_along(scores)) {
        p <- parameter_rows[sample(seq_len(nrow(parameter_rows)), 1L), ]
        synthetic <- generate_fixed_count_run(2L * flank_n + window_n + 1L,
                                              p$Mean_ISI_Parameter_u, p$Gamma_Shape)
        allowed <- if (flank_pattern == "left_only") seq_len(flank_n) else
          if (flank_pattern == "right_only") (flank_n + window_n + 1L):(2L * flank_n + window_n) else NULL
        scored <- score_burst_window(synthetic$isis, flank_n + 1L, flank_n + window_n,
                                     allowed_flank_indices = allowed)
        scores[i] <- scored$Evidence_Score
      }
      threshold_rows[[length(threshold_rows) + 1L]] <- quantile_rows(
        scores, "injected_support", context, flank_pattern, window_n,
        "context_flank_pattern_and_window_matched_shifted_gamma_fixed_support",
        sprintf("INJECTED_%s_%s_N%02d", toupper(context), toupper(flank_pattern), window_n))
    }
  }

  set.seed(parameters$phenotype_null_seed)
  scores <- numeric(parameters$phenotype_audit$null_surrogates)
  parameter_rows <- runs[match(dev_states$Run_ID, runs$Run_ID), , drop = FALSE]
  for (i in seq_along(scores)) {
    p <- parameter_rows[sample(seq_len(nrow(parameter_rows)), 1L), ]
    n_spikes <- sample(seq.int(parameters$broad_hfs$boundary_spikes[1], parameters$broad_hfs$boundary_spikes[2]), 1)
    synthetic <- generate_fixed_count_run(n_spikes, p$Mean_ISI_Parameter_u, p$Gamma_Shape)
    best <- best_burst_window(synthetic$isis)
    scores[i] <- if (is.null(best)) -Inf else best$Evidence_Score
  }
  threshold_rows[[length(threshold_rows) + 1L]] <- quantile_rows(
    scores, "null_scan", "broad_hfs", "scan_both_sides", NA_integer_, "shifted_gamma_HFS_max_window_score",
    "NULL_SCAN_BROAD_HFS")
  do.call(rbind, threshold_rows)
}

classify_score <- function(score, thresholds) {
  ambiguous <- thresholds$Evidence_Score[thresholds$Threshold_Name == "ambiguous_lower"]
  clear <- thresholds$Evidence_Score[thresholds$Threshold_Name == "clear_lower"]
  if (is.na(score) || score < ambiguous) return("no_burst_evidence")
  if (score < clear) return("ambiguous_burst_like")
  "clear_burst_like"
}

audit_phenotypes <- function(intervals, events, states, runs, thresholds) {
  evidence <- list()
  evidence_counter <- 0L

  for (i in seq_len(nrow(events))) {
    event <- events[i, ]
    if (event$Event_Label != "Burst") next
    template_rows <- intervals[intervals$Template_ID == event$Template_ID, , drop = FALSE]
    event_positions <- which(template_rows$Event_ID == event$Event_ID)
    if (!length(event_positions)) next
    allowed_flanks <- which(template_rows$Run_ID == event$Run_ID & template_rows$Event_ID != event$Event_ID)
    flank_pattern <- burst_flank_pattern(event$Observed_Left_Context_ISIs,
                                         event$Observed_Right_Context_ISIs)
    scored <- score_burst_window(template_rows$ISI_u, min(event_positions), max(event_positions),
                                 allowed_flank_indices = allowed_flanks)
    threshold_subset <- thresholds[
      thresholds$Threshold_Purpose == "injected_support" &
        thresholds$Phenotype_Null_Context == event$Event_Context &
        thresholds$Phenotype_Null_Flank_Pattern == flank_pattern &
        thresholds$Phenotype_Null_Window_N_ISIs == length(event_positions), , drop = FALSE]
    if (nrow(threshold_subset) != 2L) stop("Injected Burst threshold set is missing.", call. = FALSE)
    evidence_counter <- evidence_counter + 1L
    label <- if (is.null(scored)) "no_burst_evidence" else classify_score(scored$Evidence_Score, threshold_subset)
    evidence[[length(evidence) + 1L]] <- data.frame(
      Phenotype_Evidence_ID = sprintf("PHENO_%04d", evidence_counter),
      Template_ID = event$Template_ID, Candidate_Origin = "injected_rate_pulse",
      Mechanism_Event_ID = event$Event_ID, State_Envelope_ID = event$State_Envelope_ID,
      Start_Interval_Index = template_rows$Interval_Index[min(event_positions)],
      End_Interval_Index = template_rows$Interval_Index[max(event_positions)],
      Start_u = template_rows$Start_u[min(event_positions)],
      End_u = template_rows$End_u[max(event_positions)],
      N_ISIs = length(event_positions), N_Spikes = length(event_positions) + 1L,
      Duration_u = sum(template_rows$ISI_u[event_positions]),
      Internal_Median_ISI_u = if (is.null(scored)) NA_real_ else scored$Internal_Median_ISI_u,
      Flank_Median_ISI_u = if (is.null(scored)) NA_real_ else scored$Flank_Median_ISI_u,
      Median_Contrast = if (is.null(scored)) NA_real_ else scored$Median_Contrast,
      Mean_Rate_Ratio = if (is.null(scored)) NA_real_ else scored$Mean_Rate_Ratio,
      Internal_CV = if (is.null(scored)) NA_real_ else scored$Internal_CV,
      Compactness = if (is.null(scored)) NA_real_ else scored$Compactness,
      Evidence_Score = if (is.null(scored)) NA_real_ else scored$Evidence_Score,
      Left_Flank_N_Used = if (is.null(scored)) 0L else scored$Left_Flank_N_Used,
      Right_Flank_N_Used = if (is.null(scored)) 0L else scored$Right_Flank_N_Used,
      Flank_Pattern = flank_pattern,
      Flank_Source_Contract = "same_generator_run_non_event_intervals_only",
      Phenotype_Label = label,
      Phenotype_Threshold_Set_ID = threshold_subset$Threshold_Set_ID[1],
      Phenotype_Threshold_Purpose = "injected_support",
      Phenotype_Null_Context = event$Event_Context,
      Phenotype_Null_Flank_Pattern = flank_pattern,
      Phenotype_Null_Window_N_ISIs = length(event_positions),
      Ambiguity_Reason = if (label == "ambiguous_burst_like") "score_between_pre_frozen_gamma_null_guard_bands" else "none",
      Mechanism_Estimand_Role = "injected_burst",
      Phenotype_Estimand_Eligibility = if (label == "ambiguous_burst_like") "exclude_primary_include_sensitivity" else "eligible",
      stringsAsFactors = FALSE
    )
  }

  null_states <- states[states$State_Label == "Broad_HFS" & !states$Contains_Injected_Burst, , drop = FALSE]
  null_thresholds <- thresholds[thresholds$Threshold_Purpose == "null_scan", , drop = FALSE]
  for (i in seq_len(nrow(null_states))) {
    state <- null_states[i, ]
    state_rows <- intervals[intervals$State_Envelope_ID == state$State_Envelope_ID, , drop = FALSE]
    best <- best_burst_window(state_rows$ISI_u)
    if (is.null(best)) next
    label <- classify_score(best$Evidence_Score, null_thresholds)
    s <- best$Local_Start_ISI
    e <- best$Local_End_ISI
    evidence_counter <- evidence_counter + 1L
    evidence[[length(evidence) + 1L]] <- data.frame(
      Phenotype_Evidence_ID = sprintf("PHENO_%04d", evidence_counter),
      Template_ID = state$Template_ID, Candidate_Origin = "null_model_burst_like_excursion",
      Mechanism_Event_ID = "none", State_Envelope_ID = state$State_Envelope_ID,
      Start_Interval_Index = state_rows$Interval_Index[s], End_Interval_Index = state_rows$Interval_Index[e],
      Start_u = state_rows$Start_u[s], End_u = state_rows$End_u[e],
      N_ISIs = best$N_ISIs, N_Spikes = best$N_Spikes, Duration_u = best$Duration_u,
      Internal_Median_ISI_u = best$Internal_Median_ISI_u,
      Flank_Median_ISI_u = best$Flank_Median_ISI_u, Median_Contrast = best$Median_Contrast,
      Mean_Rate_Ratio = best$Mean_Rate_Ratio, Internal_CV = best$Internal_CV,
      Compactness = best$Compactness, Evidence_Score = best$Evidence_Score,
      Left_Flank_N_Used = best$Left_Flank_N_Used,
      Right_Flank_N_Used = best$Right_Flank_N_Used,
      Flank_Pattern = burst_flank_pattern(best$Left_Flank_N_Used, best$Right_Flank_N_Used),
      Flank_Source_Contract = "within_HFS_state_scan_window",
      Phenotype_Label = label,
      Phenotype_Threshold_Set_ID = null_thresholds$Threshold_Set_ID[1],
      Phenotype_Threshold_Purpose = "null_scan",
      Phenotype_Null_Context = "broad_hfs",
      Phenotype_Null_Flank_Pattern = "scan_both_sides",
      Phenotype_Null_Window_N_ISIs = best$N_ISIs,
      Ambiguity_Reason = if (label == "ambiguous_burst_like") "gamma_null_excursion_between_guard_bands" else "none",
      Mechanism_Estimand_Role = "mechanism_negative_null_excursion_audit",
      Phenotype_Estimand_Eligibility = if (label == "ambiguous_burst_like") "exclude_primary_include_sensitivity" else "eligible",
      stringsAsFactors = FALSE
    )
  }
  evidence <- do.call(rbind, evidence)
  rownames(evidence) <- NULL

  intervals$Phenotype_Evidence_ID <- "none"
  intervals$Phenotype_Label <- "no_burst_evidence"
  intervals$Phenotype_Evidence_Score <- NA_real_
  intervals$Phenotype_Candidate_Origin <- "none"
  intervals$Phenotype_Evaluation_Eligibility <- "eligible"
  intervals$Ambiguity_Reason <- "none"
  priority <- c(no_burst_evidence = 1L, ambiguous_burst_like = 2L, clear_burst_like = 3L)
  for (i in seq_len(nrow(evidence))) {
    idx <- which(intervals$Template_ID == evidence$Template_ID[i] &
                   intervals$Interval_Index >= evidence$Start_Interval_Index[i] &
                   intervals$Interval_Index <= evidence$End_Interval_Index[i])
    if (!length(idx)) next
    existing <- priority[intervals$Phenotype_Label[idx]]
    replace <- idx[priority[evidence$Phenotype_Label[i]] >= existing]
    intervals$Phenotype_Evidence_ID[replace] <- evidence$Phenotype_Evidence_ID[i]
    intervals$Phenotype_Label[replace] <- evidence$Phenotype_Label[i]
    intervals$Phenotype_Evidence_Score[replace] <- evidence$Evidence_Score[i]
    intervals$Phenotype_Candidate_Origin[replace] <- evidence$Candidate_Origin[i]
    intervals$Phenotype_Evaluation_Eligibility[replace] <- evidence$Phenotype_Estimand_Eligibility[i]
    intervals$Ambiguity_Reason[replace] <- evidence$Ambiguity_Reason[i]
  }
  list(intervals = intervals, evidence = evidence)
}

clamp01 <- function(x) pmax(0, pmin(1, x))

audit_tonic_phenotypes <- function(intervals, states) {
  tonic <- states[states$State_Label == "Tonic", , drop = FALSE]
  if (!nrow(tonic)) return(tonic)
  weights <- parameters$tonic$phenotype_score_weights
  low <- parameters$tonic$phenotype_score_thresholds[["no_evidence_upper"]]
  high <- parameters$tonic$phenotype_score_thresholds[["eligible_lower"]]
  out <- vector("list", nrow(tonic))
  for (i in seq_len(nrow(tonic))) {
    s <- tonic[i, ]
    tr <- intervals[intervals$Template_ID == s$Template_ID, , drop = FALSE]
    pos <- which(tr$State_Envelope_ID == s$State_Envelope_ID)
    k <- parameters$tonic$flank_isis_each_side
    left <- if (min(pos) > 1L) tr$ISI_u[max(1L, min(pos) - k):(min(pos) - 1L)] else numeric()
    right <- if (max(pos) < nrow(tr)) tr$ISI_u[(max(pos) + 1L):min(nrow(tr), max(pos) + k)] else numeric()
    med <- stats::median(tr$ISI_u[pos])
    left_med <- if (length(left)) stats::median(left) else NA_real_
    right_med <- if (length(right)) stats::median(right) else NA_real_
    contrasts <- abs(log(c(left_med, right_med) / med))
    contrasts <- contrasts[is.finite(contrasts)]
    local_separation <- if (length(contrasts)) mean(clamp01(contrasts / log(2.5))) else 0
    stability <- mean(c(clamp01(1 - s$CV / 0.55), clamp01(1 - s$CV2 / 0.65), clamp01(1 - s$LV / 0.45)))
    duration_score <- clamp01((s$N_ISIs - 6) / 10)
    target_range <- parameters$tonic$rate_overlap_mean_isi_B[[s$Tonic_Subtype]][[s$Tonic_Rate_Overlap_Stratum]]
    target <- mean(target_range)
    subtype_band <- clamp01(1 - abs(log(med / target)) / log(1.8))
    score <- weights[["stability"]] * stability + weights[["duration"]] * duration_score +
      weights[["local_separation"]] * local_separation + weights[["subtype_band"]] * subtype_band
    phenotype <- if (score >= high) "eligible" else if (score >= low) "ambiguous" else "no_evidence"
    reason <- if (phenotype == "eligible") "pre_frozen_timestamp_evidence_contract_met" else paste(
      c(if (stability < 0.55) "low_regularity_evidence" else NULL,
        if (duration_score < 0.40) "short_state" else NULL,
        if (local_separation < 0.20) "weak_local_boundary_contrast" else NULL,
        if (subtype_band < 0.35) "outside_subtype_rate_band" else NULL), collapse = ";")
    if (!nzchar(reason)) reason <- "borderline_continuous_evidence_score"
    out[[i]] <- data.frame(
      Template_ID = s$Template_ID, State_Envelope_ID = s$State_Envelope_ID,
      Tonic_Subtype = s$Tonic_Subtype, Tonic_Intended_Stratum = s$Tonic_Intended_Stratum,
      Tonic_Rate_Overlap_Stratum = s$Tonic_Rate_Overlap_Stratum,
      Tonic_Phenotype_Evidence_Score = score, Tonic_Phenotype = phenotype,
      Eligibility_Reason = reason, N_ISIs = s$N_ISIs, Duration_u = s$Duration_u,
      Median_ISI_u = med, Local_Left_Median_ISI_u = left_med, Local_Right_Median_ISI_u = right_med,
      Left_Log_Rate_Contrast = ifelse(is.finite(left_med), abs(log(left_med / med)), NA_real_),
      Right_Log_Rate_Contrast = ifelse(is.finite(right_med), abs(log(right_med / med)), NA_real_),
      CV = s$CV, CV2 = s$CV2, LV = s$LV, Stability_Subscore = stability,
      Duration_Subscore = duration_score, Local_Separation_Subscore = local_separation,
      Subtype_Band_Subscore = subtype_band,
      Detector_Output_Used = FALSE, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

audit_hfs_boundaries <- function(intervals, states) {
  cfg <- parameters$hfs_boundary_audit
  classify <- function(x, hard_cut = FALSE) {
    if (hard_cut) return("hard_cut_canonical_pause")
    if (!is.finite(x)) return("recording_edge_unobservable")
    if (x >= cfg$clear_contrast) "clear" else if (x >= cfg$ambiguous_contrast) "ambiguous" else "no_evidence"
  }
  non_hfs <- states$State_Label != "Broad_HFS"
  states$Boundary_Observability[non_hfs] <- "not_applicable"
  states$Start_Boundary_Observability[non_hfs] <- "not_applicable"
  states$End_Boundary_Observability[non_hfs] <- "not_applicable"
  for (i in which(states$State_Label == "Broad_HFS")) {
    s <- states[i, ]; tr <- intervals[intervals$Template_ID == s$Template_ID, , drop = FALSE]
    pos <- which(tr$State_Envelope_ID == s$State_Envelope_ID); k <- cfg$flank_isis_each_side
    inside_start <- stats::median(tr$ISI_u[head(pos, min(k, length(pos)))])
    inside_end <- stats::median(tr$ISI_u[tail(pos, min(k, length(pos)))])
    left <- if (min(pos) > 1L) tr$ISI_u[max(1L, min(pos) - k):(min(pos) - 1L)] else numeric()
    right <- if (max(pos) < nrow(tr)) tr$ISI_u[(max(pos) + 1L):min(nrow(tr), max(pos) + k)] else numeric()
    start_contrast <- if (length(left)) stats::median(left) / inside_start else NA_real_
    end_contrast <- if (length(right)) stats::median(right) / inside_end else NA_real_
    start_label <- classify(start_contrast, s$Start_Hard_Cut_Reason == "canonical_pause")
    end_label <- classify(end_contrast, s$End_Hard_Cut_Reason == "canonical_pause")
    states$Start_Boundary_Contrast[i] <- start_contrast
    states$End_Boundary_Contrast[i] <- end_contrast
    states$Start_Boundary_Observability[i] <- start_label
    states$End_Boundary_Observability[i] <- end_label
    states$Boundary_Observability[i] <- if (all(c(start_label, end_label) %in% c("clear", "hard_cut_canonical_pause"))) "clear_or_hard_cut" else
      if (any(c(start_label, end_label) == "no_evidence")) "no_evidence_at_one_or_more_boundaries" else "ambiguous_or_edge"
  }
  states
}

single_isi_threshold_metrics <- function(x, threshold) {
  lower <- x[x$Class_Role == "lower", , drop = FALSE]
  higher <- x[x$Class_Role == "higher", , drop = FALSE]
  sensitivity <- if (nrow(lower)) mean(lower$ISI_u <= threshold) else NA_real_
  specificity <- if (nrow(higher)) mean(higher$ISI_u > threshold) else NA_real_
  lower_by_template <- if (nrow(lower)) tapply(lower$ISI_u <= threshold, lower$Template_ID, mean) else NA_real_
  higher_by_template <- if (nrow(higher)) tapply(higher$ISI_u > threshold, higher$Template_ID, mean) else NA_real_
  c(Sensitivity = sensitivity, Specificity = specificity,
    Balanced_Accuracy = mean(c(sensitivity, specificity)),
    Template_Equal_Sensitivity = mean(lower_by_template, na.rm = TRUE),
    Template_Equal_Specificity = mean(higher_by_template, na.rm = TRUE),
    Template_Equal_Balanced_Accuracy = mean(c(mean(lower_by_template, na.rm = TRUE),
                                               mean(higher_by_template, na.rm = TRUE))))
}

fit_single_isi_threshold <- function(x) {
  values <- sort(unique(x$ISI_u))
  candidates <- if (length(values) > 1L) c(values[1] - .Machine$double.eps,
                                            (values[-length(values)] + values[-1L]) / 2,
                                            tail(values, 1) + .Machine$double.eps) else values
  scores <- vapply(candidates, function(threshold) {
    single_isi_threshold_metrics(x, threshold)[["Template_Equal_Balanced_Accuracy"]]
  }, numeric(1))
  stats::median(candidates[which(abs(scores - max(scores, na.rm = TRUE)) < 1e-14)])
}

bootstrap_single_isi_metric <- function(x, threshold, replicates, seed) {
  ids <- sort(unique(x$Template_ID))
  if (length(ids) < 2L) return(c(Lower = NA_real_, Upper = NA_real_))
  set.seed(seed)
  estimates <- numeric(replicates)
  for (b in seq_len(replicates)) {
    sampled <- sample(ids, length(ids), replace = TRUE)
    boot <- do.call(rbind, lapply(seq_along(sampled), function(j) {
      z <- x[x$Template_ID == sampled[j], , drop = FALSE]
      z$Template_ID <- paste0(sampled[j], "__", j)
      z
    }))
    estimates[b] <- single_isi_threshold_metrics(boot, threshold)[["Template_Equal_Balanced_Accuracy"]]
  }
  stats::quantile(estimates, c(.025, .975), names = FALSE, type = 8, na.rm = TRUE)
}

make_dev_holdout_single_isi_baseline <- function(intervals) {
  make_class <- function(mask, label, role) data.frame(
    Template_ID = intervals$Template_ID[mask], ISI_u = intervals$ISI_u[mask],
    Class_Label = label, Class_Role = role, stringsAsFactors = FALSE)
  classes <- list(
    Burst = make_class(intervals$Event_Label == "Burst", "Burst", "lower"),
    HFS_direct = make_class(intervals$Direct_HFS_Support, "Broad_HFS_direct", "higher"),
    Tonic_strict = make_class(intervals$State_Label == "Tonic", "Tonic_strict", "higher"),
    Tonic_observable = make_class(intervals$State_Label == "Tonic" & intervals$Tonic_Phenotype == "eligible",
                                  "Tonic_observable", "higher"),
    Primary_Pause = make_class(intervals$Event_Label == "Pause", "Primary_Pause", "higher"),
    Canonical_Pause = make_class(intervals$Event_Label == "Pause" & intervals$Pause_Subtype == "canonical",
                                 "Canonical_Pause", "higher"),
    Complex_Pause = make_class(intervals$Event_Label == "Pause" & intervals$Pause_Subtype == "complex_multi_gap",
                               "Complex_Pause_internal_ISI", "higher"),
    Borderline_Gap = make_class(intervals$Borderline_Gap_Label == parameters$borderline_gap$secondary_label,
                                "Borderline_Gap_secondary", "higher")
  )
  comparisons <- list(
    c("Burst", "HFS_direct"),
    c("HFS_direct", "Tonic_strict"),
    c("HFS_direct", "Tonic_observable"),
    c("Tonic_strict", "Primary_Pause"),
    c("Tonic_strict", "Canonical_Pause"),
    c("Tonic_strict", "Complex_Pause"),
    c("Tonic_strict", "Borderline_Gap"),
    c("Tonic_observable", "Borderline_Gap")
  )
  out <- lapply(seq_along(comparisons), function(i) {
    pair <- comparisons[[i]]
    lower <- classes[[pair[1]]]; higher <- classes[[pair[2]]]
    lower$Class_Role <- "lower"; higher$Class_Role <- "higher"
    x <- rbind(lower, higher)
    development <- x[x$Template_ID %in% parameters$development_templates, , drop = FALSE]
    holdout <- x[x$Template_ID %in% parameters$holdout_templates, , drop = FALSE]
    if (!all(c("lower", "higher") %in% unique(development$Class_Role)) ||
        !all(c("lower", "higher") %in% unique(holdout$Class_Role)))
      stop("A predeclared single-ISI baseline comparison lacks one class in a split.", call. = FALSE)
    threshold <- fit_single_isi_threshold(development)
    dev_metrics <- single_isi_threshold_metrics(development, threshold)
    holdout_metrics <- single_isi_threshold_metrics(holdout, threshold)
    ci <- bootstrap_single_isi_metric(holdout, threshold,
                                      parameters$dev_holdout_single_isi_baseline$bootstrap_replicates,
                                      parameters$dev_holdout_single_isi_baseline$bootstrap_seed + i)
    data.frame(
      Comparison = paste(lower$Class_Label[1], "vs", higher$Class_Label[1]),
      Lower_ISI_Class = lower$Class_Label[1], Higher_ISI_Class = higher$Class_Label[1],
      Decision_Rule = "lower class if ISI_u <= frozen development threshold",
      Threshold_u = threshold, Threshold_Fit_Split = "development_templates_1_to_5",
      Evaluation_Split = "holdout_templates_6_to_20",
      Development_Pooled_Balanced_Accuracy = dev_metrics[["Balanced_Accuracy"]],
      Development_Template_Equal_Balanced_Accuracy = dev_metrics[["Template_Equal_Balanced_Accuracy"]],
      Holdout_Pooled_Sensitivity = holdout_metrics[["Sensitivity"]],
      Holdout_Pooled_Specificity = holdout_metrics[["Specificity"]],
      Holdout_Pooled_Balanced_Accuracy = holdout_metrics[["Balanced_Accuracy"]],
      Holdout_Template_Equal_Balanced_Accuracy = holdout_metrics[["Template_Equal_Balanced_Accuracy"]],
      Holdout_Template_Cluster_Bootstrap_CI_Lower = ci[1],
      Holdout_Template_Cluster_Bootstrap_CI_Upper = ci[2],
      Development_Lower_N = sum(development$Class_Role == "lower"),
      Development_Higher_N = sum(development$Class_Role == "higher"),
      Holdout_Lower_N = sum(holdout$Class_Role == "lower"),
      Holdout_Higher_N = sum(holdout$Class_Role == "higher"),
      Scale_Use = "dimensionless_mother_templates_only",
      Acceptance_Target = "none_descriptive_audit_only",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}
