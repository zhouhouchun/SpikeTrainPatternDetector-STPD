# eventness audit eventness audit refinement layer.
# This module adds event-like vs state-like feature metrics and recommended
# family/subtype decisions. It is intentionally audit-first: it does not
# overwrite AUTO labels by default.

stpd_eventness_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || !is.finite(x[1])) return(default)
  x[1]
}

stpd_eventness_chr <- function(x, default = "") {
  x <- as.character(x)
  if (length(x) == 0 || is.na(x[1])) return(default)
  x[1]
}

stpd_eventness_logical <- function(x, default = FALSE) {
  if (length(x) == 0 || is.na(x[1])) return(default)
  isTRUE(x[1]) || identical(tolower(as.character(x[1])), "true")
}

stpd_eventness_clamp01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  pmax(0, pmin(1, x))
}

stpd_eventness_valid_seq <- function(a, b) {
  a <- as.integer(a); b <- as.integer(b)
  if (!is.finite(a) || !is.finite(b) || a > b) return(integer())
  seq.int(a, b)
}

stpd_eventness_context_indices <- function(n, s_isi, e_isi, gap = 2L, window = 12L) {
  n <- as.integer(n); s_isi <- as.integer(s_isi); e_isi <- as.integer(e_isi)
  gap <- max(0L, as.integer(gap)); window <- max(1L, as.integer(window))
  left <- stpd_eventness_valid_seq(max(2L, s_isi - gap - window), s_isi - gap - 1L)
  right <- stpd_eventness_valid_seq(e_isi + gap + 1L, min(n, e_isi + gap + window))
  unique(c(left, right))
}

stpd_regularity_score <- function(cv, lv, q90_q10, params) {
  cv_ref <- stpd_eventness_num(params$classification$regularity_cv_ref %||% 0.50, 0.50)
  lv_ref <- stpd_eventness_num(params$classification$regularity_lv_ref %||% 0.50, 0.50)
  qr_ref <- stpd_eventness_num(params$classification$regularity_q90_q10_ref %||% 2.00, 2.00)
  parts <- c(
    1 - stpd_eventness_clamp01(cv / max(cv_ref, .Machine$double.eps)),
    1 - stpd_eventness_clamp01(lv / max(lv_ref, .Machine$double.eps)),
    1 - stpd_eventness_clamp01((q90_q10 - 1) / max(qr_ref - 1, .Machine$double.eps))
  )
  if (all(!is.finite(parts))) return(NA_real_)
  mean(parts[is.finite(parts)], na.rm = TRUE)
}

stpd_eventness_score <- function(edge_min, context_contrast, return_score, params) {
  edge_ref <- stpd_eventness_num(params$classification$eventness_edge_ref %||% 3.0, 3.0)
  context_ref <- stpd_eventness_num(params$classification$eventness_context_ref %||% 3.0, 3.0)
  edge_score <- if (is.finite(edge_min)) stpd_eventness_clamp01(edge_min / max(edge_ref, .Machine$double.eps)) else NA_real_
  context_score <- if (is.finite(context_contrast)) stpd_eventness_clamp01(context_contrast / max(context_ref, .Machine$double.eps)) else NA_real_
  boundary_score <- suppressWarnings(min(c(edge_score, context_score), na.rm = TRUE))
  if (!is.finite(boundary_score)) boundary_score <- if (is.finite(edge_score)) edge_score else context_score
  if (!is.finite(boundary_score)) return(NA_real_)
  if (is.finite(return_score)) return(mean(c(boundary_score, return_score), na.rm = TRUE))
  boundary_score
}


stpd_eventness_zone <- function(eventness, params) {
  eventness <- stpd_eventness_num(eventness, NA_real_)
  if (!is.finite(eventness)) return("unknown")
  event_thr <- stpd_eventness_num(params$classification$eventness_threshold %||% 0.60, 0.60)
  state_thr <- stpd_eventness_num(params$classification$state_eventness_threshold %||% 0.45, 0.45)
  if (eventness >= event_thr) return("event_like")
  if (eventness <= state_thr) return("state_like")
  "medium_eventness_review"
}

stpd_long_burst_eventness_status <- function(n_isi, edge_min, context_contrast, short_frac,
                                        duration_sec, internal_count, params) {
  long_min_isi <- stpd_eventness_num(params$burst$long_burst_min_isi_count %||% 10L, 10)
  long_edge <- stpd_eventness_num(params$burst$long_burst_edge_min %||% params$burst$long_burst_edge_contrast_min %||% 3.5, 3.5)
  context_min <- stpd_eventness_num(params$burst$long_burst_context_contrast_min %||% 3.0, 3.0)
  short_min <- stpd_eventness_num(params$burst$long_burst_short_isi_fraction_min %||% params$burst$long_burst_short_fraction_min %||% 0.70, 0.70)
  max_dur <- stpd_eventness_num(params$burst$long_burst_max_duration_sec %||% params$burst$long_burst_max_duration %||% 0, 0)
  int_max <- stpd_eventness_num(params$burst$long_burst_internal_outlier_count_max %||% params$burst$internal_outlier_count_max %||% 3L, 3)

  n_isi <- stpd_eventness_num(n_isi, NA_real_)
  edge_min <- stpd_eventness_num(edge_min, NA_real_)
  context_contrast <- stpd_eventness_num(context_contrast, NA_real_)
  short_frac <- stpd_eventness_num(short_frac, NA_real_)
  duration_sec <- stpd_eventness_num(duration_sec, NA_real_)
  internal_count <- stpd_eventness_num(internal_count, 0)

  structural <- is.finite(n_isi) && n_isi >= long_min_isi && is.finite(edge_min) && edge_min >= long_edge
  if (!structural) {
    return(list(
      structural = FALSE,
      status = "not_structural_long_burst_candidate",
      context_pass = NA,
      short_fraction_pass = NA,
      duration_pass = NA,
      internal_outlier_pass = NA,
      failures = character()
    ))
  }

  failures <- character()
  context_pass <- is.finite(context_contrast) && context_contrast >= context_min
  if (!is.finite(context_contrast)) failures <- c(failures, "context_unresolved") else if (!context_pass) failures <- c(failures, "context_weak")
  short_pass <- is.finite(short_frac) && short_frac >= short_min
  if (!is.finite(short_frac)) failures <- c(failures, "short_fraction_unresolved") else if (!short_pass) failures <- c(failures, "short_fraction_weak")
  duration_pass <- TRUE
  if (is.finite(max_dur) && max_dur > 0) {
    duration_pass <- is.finite(duration_sec) && duration_sec <= max_dur
    if (!is.finite(duration_sec)) failures <- c(failures, "duration_unresolved") else if (!duration_pass) failures <- c(failures, "duration_too_long")
  }
  internal_pass <- !is.finite(internal_count) || internal_count <= int_max
  if (!internal_pass) failures <- c(failures, "internal_outlier_excess")
  status <- if (length(failures) == 0) "strict_pass" else paste(failures, collapse = ";")
  list(
    structural = TRUE,
    status = status,
    context_pass = context_pass,
    short_fraction_pass = short_pass,
    duration_pass = duration_pass,
    internal_outlier_pass = internal_pass,
    failures = failures
  )
}

stpd_enhance_candidate_features_eventness_core <- function(ds, features, params = default_params_sec(), selected_trains = NULL) {
  if (is.null(features) || nrow(features) == 0 || is.null(ds) || is.null(ds$trains)) return(features %||% tibble::tibble())
  params <- apply_schema_defaults(params)
  min_isi <- params$detector$min_valid_isi_sec %||% 0.0009
  context_window <- as.integer(params$classification$context_window_isi_n %||% 12L)
  context_gap <- as.integer(params$classification$context_gap_isi_n %||% 2L)
  return_tol <- stpd_eventness_num(params$classification$return_to_baseline_tolerance %||% 1.5, 1.5)
  internal_ratio_thr <- stpd_eventness_num(params$burst$internal_outlier_ratio_max %||% 3.5, 3.5)
  target <- selected_trains %||% names(ds$trains)
  out <- features
  new_cols <- c(
    "q10_ISI_sec", "q50_ISI_sec", "q90_q10_ratio", "distant_context_median_sec",
    "context_contrast", "return_to_baseline_score", "eventness_score",
    "eventness_edge_component", "eventness_context_component", "regularity_score",
    "internal_outlier_count", "internal_outlier_fraction", "internal_outlier_max_ratio",
    "eventness_zone", "medium_eventness_review",
    "long_burst_context_pass", "long_burst_short_fraction_pass",
    "long_burst_duration_pass", "long_burst_internal_outlier_pass",
    "long_burst_definition_status", "eventness_audit_note"
  )
  for (nm in new_cols) if (!(nm %in% names(out))) out[[nm]] <- NA_real_
  out$eventness_zone <- as.character(out$eventness_zone)
  out$long_burst_definition_status <- as.character(out$long_burst_definition_status)
  out$eventness_audit_note <- as.character(out$eventness_audit_note)
  out$medium_eventness_review <- as.logical(out$medium_eventness_review)
  out$long_burst_context_pass <- as.logical(out$long_burst_context_pass)
  out$long_burst_short_fraction_pass <- as.logical(out$long_burst_short_fraction_pass)
  out$long_burst_duration_pass <- as.logical(out$long_burst_duration_pass)
  out$long_burst_internal_outlier_pass <- as.logical(out$long_burst_internal_outlier_pass)

  for (ii in seq_len(nrow(out))) {
    tr <- stpd_eventness_chr(out$train[ii], "")
    if (!tr %in% target || !tr %in% names(ds$trains)) next
    dat <- ds$trains[[tr]]
    s0 <- suppressWarnings(as.integer(out$start_isi[ii])); e0 <- suppressWarnings(as.integer(out$end_isi[ii]))
    if (!is.finite(s0) || !is.finite(e0) || s0 < 2L || e0 > nrow(dat) || e0 < s0) next
    vals <- valid_isi_values(dat$ISI_sec[s0:e0], min_isi)
    if (length(vals) == 0) {
      out$eventness_audit_note[ii] <- "no_valid_candidate_ISI"
      next
    }
    q10 <- as.numeric(stats::quantile(vals, 0.10, na.rm = TRUE, names = FALSE))
    q50 <- safe_median(vals)
    q90 <- as.numeric(stats::quantile(vals, 0.90, na.rm = TRUE, names = FALSE))
    qratio <- if (is.finite(q10) && q10 > 0 && is.finite(q90)) q90 / q10 else NA_real_
    out$q10_ISI_sec[ii] <- q10
    out$q50_ISI_sec[ii] <- q50
    out$q90_q10_ratio[ii] <- qratio

    internal_ratios <- if (is.finite(q50) && q50 > 0) vals / q50 else rep(NA_real_, length(vals))
    outlier <- is.finite(internal_ratios) & internal_ratios > internal_ratio_thr
    out$internal_outlier_count[ii] <- sum(outlier, na.rm = TRUE)
    out$internal_outlier_fraction[ii] <- if (length(vals) > 0) mean(outlier, na.rm = TRUE) else NA_real_
    out$internal_outlier_max_ratio[ii] <- if (any(is.finite(internal_ratios))) max(internal_ratios, na.rm = TRUE) else NA_real_

    cidx <- stpd_eventness_context_indices(nrow(dat), s0, e0, gap = context_gap, window = context_window)
    ctx <- if (length(cidx) > 0) valid_isi_values(dat$ISI_sec[cidx], min_isi) else numeric()
    ctx_med <- if (length(ctx) > 0) safe_median(ctx) else NA_real_
    out$distant_context_median_sec[ii] <- ctx_med
    q90_core <- stpd_eventness_num(out$q90_ISI_sec[ii], q90)
    out$context_contrast[ii] <- if (is.finite(ctx_med) && is.finite(q90_core) && q90_core > 0) ctx_med / q90_core else NA_real_

    pre <- stpd_eventness_num(out$pre_ISI_sec[ii], NA_real_)
    post <- stpd_eventness_num(out$post_ISI_sec[ii], NA_real_)
    prepost <- c(pre, post); prepost <- prepost[is.finite(prepost)]
    if (length(prepost) > 0 && is.finite(ctx_med) && ctx_med > 0) {
      ratio <- mean(prepost, na.rm = TRUE) / ctx_med
      denom <- if (is.finite(return_tol) && return_tol > 1) log(return_tol) else 1
      out$return_to_baseline_score[ii] <- stpd_eventness_clamp01(exp(-abs(log(ratio)) / denom))
    } else {
      out$return_to_baseline_score[ii] <- NA_real_
    }

    edge_min <- stpd_eventness_num(out$edge_contrast_min[ii], NA_real_)
    edge_ref <- stpd_eventness_num(params$classification$eventness_edge_ref %||% 3.0, 3.0)
    context_ref <- stpd_eventness_num(params$classification$eventness_context_ref %||% 3.0, 3.0)
    out$eventness_edge_component[ii] <- if (is.finite(edge_min)) stpd_eventness_clamp01(edge_min / max(edge_ref, .Machine$double.eps)) else NA_real_
    out$eventness_context_component[ii] <- if (is.finite(out$context_contrast[ii])) stpd_eventness_clamp01(out$context_contrast[ii] / max(context_ref, .Machine$double.eps)) else NA_real_
    out$eventness_score[ii] <- stpd_eventness_score(edge_min, out$context_contrast[ii], out$return_to_baseline_score[ii], params)
    out$regularity_score[ii] <- stpd_regularity_score(stpd_eventness_num(out$CV[ii], NA_real_), stpd_eventness_num(out$LV[ii], NA_real_), qratio, params)
    zone <- stpd_eventness_zone(out$eventness_score[ii], params)
    out$eventness_zone[ii] <- zone
    out$medium_eventness_review[ii] <- identical(zone, "medium_eventness_review")

    n_sp <- stpd_eventness_num(out$n_spikes[ii], NA_real_)
    n_isi <- stpd_eventness_num(out$n_isi[ii], if (is.finite(n_sp)) max(0, n_sp - 1) else NA_real_)
    short_frac <- stpd_eventness_num(out$short_ISI_fraction_35pct[ii], stpd_eventness_num(out$short_ISI_fraction[ii], NA_real_))
    duration <- stpd_eventness_num(out$duration_sec[ii], NA_real_)
    long_status <- stpd_long_burst_eventness_status(n_isi, edge_min, out$context_contrast[ii], short_frac, duration, out$internal_outlier_count[ii], params)
    out$long_burst_context_pass[ii] <- long_status$context_pass
    out$long_burst_short_fraction_pass[ii] <- long_status$short_fraction_pass
    out$long_burst_duration_pass[ii] <- long_status$duration_pass
    out$long_burst_internal_outlier_pass[ii] <- long_status$internal_outlier_pass
    out$long_burst_definition_status[ii] <- long_status$status

    out$eventness_audit_note[ii] <- paste0(
      "eventness=", ifelse(is.finite(out$eventness_score[ii]), sprintf("%.3f", out$eventness_score[ii]), "NA"),
      "; zone=", zone,
      "; regularity=", ifelse(is.finite(out$regularity_score[ii]), sprintf("%.3f", out$regularity_score[ii]), "NA"),
      "; long_burst_status=", long_status$status
    )
  }
  out
}

stpd_enhance_candidate_features_eventness <- stpd_enhance_candidate_features_eventness_core

stpd_recommend_family_subtype <- function(feature, params = default_params_sec()) {
  params <- apply_schema_defaults(params)
  row <- as.data.frame(feature)[1, , drop = FALSE]
  getc <- function(nm, default = "") stpd_eventness_chr(if (nm %in% names(row)) row[[nm]][1] else default, default)
  getn <- function(nm, default = NA_real_) stpd_eventness_num(if (nm %in% names(row)) row[[nm]][1] else default, default)
  cls <- getc("final_candidate_class", getc("final_class", getc("raw_candidate_class", "")))
  src <- getc("candidate_source", getc("source", ""))
  n_spikes <- getn("n_spikes", NA_real_)
  n_isi <- getn("n_isi", if (is.finite(n_spikes)) max(0, n_spikes - 1) else NA_real_)
  eventness <- getn("eventness_score", NA_real_)
  regularity <- getn("regularity_score", NA_real_)
  edge_min <- getn("edge_contrast_min", NA_real_)
  context <- getn("context_contrast", NA_real_)
  q90 <- getn("q90_ISI_sec", NA_real_)
  max_pct <- getn("max_ISI_pct", NA_real_)
  short_frac <- getn("short_ISI_fraction_35pct", getn("short_ISI_fraction", NA_real_))
  internal_count <- getn("internal_outlier_count", 0)
  internal_max <- getn("internal_outlier_max_ratio", NA_real_)
  median_isi <- getn("median_ISI_sec", NA_real_)
  duration <- getn("duration_sec", NA_real_)

  event_thr <- stpd_eventness_num(params$classification$eventness_threshold %||% 0.60, 0.60)
  state_thr <- stpd_eventness_num(params$classification$state_eventness_threshold %||% 0.45, 0.45)
  eventness_zone <- getc("eventness_zone", stpd_eventness_zone(eventness, params))

  fam <- "other_or_ambiguous"
  subtype <- "other"
  rec_final <- cls
  reason <- "default_other"
  review <- FALSE
  uncertainty <- ""
  confidence <- "audit_contextual"
  lb_status <- getc("long_burst_definition_status", "")

  is_pause <- cls == "pause" || grepl("pause", src, ignore.case = TRUE)
  is_burst_family <- cls %in% c("burst", "long_burst", "possible_burst", "burst_family") || grepl("burst|structure|seed|bridge|local_compression|boundary", src, ignore.case = TRUE)

  if (is_pause) {
    fam <- "pause_event"
    subtype <- if (cls == "pause") "pause" else "possible_pause"
    rec_final <- if (cls == "pause") "pause" else "possible_pause"
    reason <- "pause_candidate_path"
    confidence <- if (cls == "pause") "audit_high_confidence" else "audit_review"
    review <- cls != "pause"
  } else if ((is.finite(eventness) && eventness >= event_thr) || (is_burst_family && !is.finite(eventness))) {
    fam <- "burst_event"
    confidence <- "audit_burst_event_candidate"
    long_min_isi <- params$burst$long_burst_min_isi_count %||% 10L
    long_edge <- params$burst$long_burst_edge_min %||% params$burst$long_burst_edge_contrast_min %||% 3.5
    classic_max <- params$burst$classic_burst_max_spikes %||% 10L
    classic_min <- params$burst$classic_burst_min_spikes %||% 3L
    classic_edge <- params$burst$classic_burst_edge_min %||% 3.5
    hf_max <- params$burst$hf_burst_max_spikes %||% 15L
    hf_min <- params$burst$hf_burst_min_spikes %||% 3L
    hf_edge <- params$burst$hf_burst_edge_min %||% 3.0
    hf_context <- params$burst$hf_burst_context_contrast_min %||% 3.0
    hf_q90 <- params$burst$hf_burst_core_q90_max %||% 0.015
    hf_pct <- params$burst$hf_burst_core_pct_max %||% 25
    outlier_count_max <- params$burst$classic_burst_internal_outlier_count_max %||% params$burst$internal_outlier_count_max %||% 1L
    outlier_ratio_max <- params$burst$classic_burst_internal_outlier_ratio_max %||% params$burst$internal_outlier_ratio_max %||% 3.5

    long_status <- stpd_long_burst_eventness_status(n_isi, edge_min, context, short_frac, duration, internal_count, params)
    lb_status <- long_status$status
    if (isTRUE(long_status$structural)) {
      subtype <- "long_burst"
      rec_final <- "long_burst"
      if (identical(lb_status, "strict_pass")) {
        reason <- "eventness_high__long_burst_strict_pass"
        confidence <- "audit_high_confidence_structural"
      } else {
        reason <- paste0("eventness_high__long_burst_structural_review__", lb_status)
        review <- TRUE
        uncertainty <- lb_status
        confidence <- "audit_review_structural_long_burst"
      }
    } else if (is.finite(n_spikes) && n_spikes >= hf_min && n_spikes <= hf_max &&
               is.finite(edge_min) && edge_min >= hf_edge &&
               (is.finite(context) && context >= hf_context) &&
               ((is.finite(q90) && q90 <= hf_q90) || (is.finite(max_pct) && max_pct <= hf_pct))) {
      subtype <- "high_frequency_burst"
      rec_final <- "burst"
      reason <- "eventness_high__core_fast__limited_extent"
      confidence <- "audit_high_confidence_event_like"
    } else if (is.finite(n_spikes) && n_spikes >= classic_min && n_spikes <= classic_max &&
               is.finite(edge_min) && edge_min >= classic_edge &&
               (!is.finite(internal_count) || internal_count <= outlier_count_max) &&
               (!is.finite(internal_max) || internal_max <= outlier_ratio_max)) {
      subtype <- "classic_burst"
      rec_final <- "burst"
      reason <- "eventness_high__classic_size__internal_outliers_limited"
      confidence <- "audit_high_confidence_event_like"
    } else {
      subtype <- "possible_burst"
      rec_final <- "possible_burst"
      reason <- "eventness_high_but_subtype_criteria_incomplete"
      review <- TRUE
      uncertainty <- "burst_family_subtype_unresolved"
      confidence <- "audit_review"
    }
  } else {
    fam <- "state_epoch"
    confidence <- "audit_state_candidate"
    classic_min_isi <- params$state$classic_tonic_min_isi_count %||% 3L
    classic_reg <- params$state$classic_tonic_regularity_min %||% 0.60
    hf_tonic_min <- params$state$hf_tonic_min_isi_count %||% 6L
    hf_tonic_reg <- params$state$hf_tonic_regularity_min %||% 0.70
    hf_tonic_evmax <- params$state$hf_tonic_eventness_max %||% 0.50
    hf_spiking_min <- params$state$hf_spiking_min_isi_count %||% 14L
    hf_spiking_frac <- params$state$hf_spiking_short_isi_fraction_min %||% 0.70
    hf_spiking_evmax <- params$state$hf_spiking_eventness_max %||% 0.65
    medium_eventness <- identical(eventness_zone, "medium_eventness_review")

    if (is.finite(n_isi) && n_isi >= hf_tonic_min && is.finite(regularity) && regularity >= hf_tonic_reg && (!is.finite(eventness) || eventness <= hf_tonic_evmax)) {
      subtype <- "high_frequency_tonic"
      rec_final <- "high_frequency_tonic"
      reason <- "eventness_low__regular_high_frequency_state"
      confidence <- if (medium_eventness) "audit_review_medium_eventness_state" else "audit_high_confidence_state"
      review <- medium_eventness
      if (medium_eventness) uncertainty <- "medium_eventness_regular_high_frequency_state"
    } else if (is.finite(n_isi) && n_isi >= hf_spiking_min && is.finite(short_frac) && short_frac >= hf_spiking_frac && (!is.finite(eventness) || eventness <= hf_spiking_evmax)) {
      subtype <- "high_frequency_spiking"
      rec_final <- "high_frequency_spiking"
      reason <- if (medium_eventness) "medium_eventness__many_short_ISIs__review" else "eventness_low__many_short_ISIs"
      confidence <- if (medium_eventness) "audit_review_medium_eventness_state" else "audit_high_confidence_state"
      review <- medium_eventness
      if (medium_eventness) uncertainty <- "medium_eventness_high_frequency_epoch"
    } else if (is.finite(n_isi) && n_isi >= classic_min_isi && is.finite(regularity) && regularity >= classic_reg) {
      subtype <- "classic_tonic"
      rec_final <- "tonic"
      reason <- if (medium_eventness) "medium_eventness__regular_tonic_state__review" else "eventness_low__regular_tonic_state"
      confidence <- if (medium_eventness) "audit_review_medium_eventness_state" else "audit_high_confidence_state"
      review <- medium_eventness
      if (medium_eventness) uncertainty <- "medium_eventness_regular_state"
    } else {
      fam <- "other_or_ambiguous"
      subtype <- "other_or_ambiguous"
      rec_final <- if (nzchar(cls)) cls else "others"
      reason <- if (medium_eventness) "medium_eventness_insufficient_state_or_event_evidence" else "insufficient_eventness_or_state_evidence"
      review <- medium_eventness
      uncertainty <- if (medium_eventness) "medium_eventness_ambiguous" else "insufficient_evidence"
      confidence <- if (medium_eventness) "audit_review" else "audit_contextual"
    }
  }
  data.frame(
    recommended_family = fam,
    recommended_subtype = subtype,
    recommended_final_class = rec_final,
    decision_path = reason,
    eventness_zone = eventness_zone,
    recommended_review_required = review,
    recommendation_confidence_tier = confidence,
    recommended_uncertainty_reason = uncertainty,
    long_burst_definition_status = lb_status,
    stringsAsFactors = FALSE
  )
}

stpd_eventness_method_readme <- function() {
  c(
    "Spike Train Pattern Detector results README",
    "",
    "Eventness audit recommendations do not overwrite AUTO labels by default.",
    "The detector first generates candidate events; candidate features then quantify rate level, eventness, regularity, extent and long-burst strictness.",
    "Eventness audit fields include eventness_score, eventness_zone, context_contrast, return_to_baseline_score, regularity_score, internal_outlier_count, q90_q10_ratio and long_burst_definition_status.",
    "Final_classification_audit.csv and Eventness_audit.csv include recommended_family, recommended_subtype, recommended_final_class, decision_path and review/uncertainty fields.",
    "Interpretation rule: eventness high -> burst-family is preferred even if internal regularity is high; this prevents high-frequency burst candidates from being collapsed into high-frequency tonic solely by CV/LV/MM.",
    "Medium-eventness state recommendations are flagged for review rather than treated as high-confidence state labels.",
    "Long_burst recommendations now carry strictness status: strict_pass versus context_weak, short_fraction_weak, duration_too_long, internal_outlier_excess or unresolved variants.",
    "Recommended subtypes are audit recommendations, not authoritative AUTO labels unless a future explicit option enables them.",
    "Strict high-confidence and candidate-family summaries remain separate. Do not merge possible_burst into burst silently in publication-level metrics."
  )
}
