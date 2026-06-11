# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Audit / preset / candidate-feature helpers
# ============================================================

stpd_methodological_warning <- function(as_vector = FALSE) {
  lines <- c(
    "Spike Train Pattern Detector is a candidate-event generator and semi-supervised review platform, not an unbiased final ground-truth classifier. Results should be reported as high-confidence events, review candidates, and family-level summaries with validation where available.",
    "High-confidence events, review candidates (possible_burst), and burst-family summaries should be reported separately.",
    "long_burst is a structural/event-like label based on spike count, duration, short-ISI fraction, and flank contrast; it should not be interpreted automatically as a distinct biological mechanism.",
    "high_frequency_tonic and high_frequency_spiking are state/epoch-style labels. Their biological interpretation depends on cell type, preparation, and spike-sorting quality.",
    "Interactive tuning with manual labels can overfit. For publication-grade analysis, use held-out trains/datasets, event-level metrics, and report the preset/parameter hash.",
    "Refractory-suspect ISIs indicate possible spike-sorting/multi-unit/timestamp issues; default handling is conservative review/demotion rather than silent acceptance."
  )
  if (isTRUE(as_vector)) lines else paste(lines, collapse = "\n")
}

preset_catalog <- function() {
  data.frame(
    preset_name = c("conservative_single_unit", "balanced_single_unit", "sensitive_exploratory", "fast_spiking_interneuron", "mea_multiunit"),
    label = c("Conservative single-unit", "Balanced single-unit", "Sensitive exploratory", "Fast-spiking interneuron", "MEA / multi-unit"),
    interpretation = c(
      "Strict artifact/refractory handling; review-first possible_burst policy.",
      "Default balanced candidate-generation mode.",
      "Higher recall; more review candidates; not recommended for final publication without held-out validation.",
      "Allows stable high-rate tonic interpretation; conservative burst promotion.",
      "Warn-focused refractory policy; suitable when very short ISIs may reflect population activity."
    ),
    stringsAsFactors = FALSE
  )
}

apply_preset_to_params <- function(params, preset_name = "balanced_single_unit") {
  p <- params %||% default_params_sec()
  if (is.null(p$detector)) p$detector <- list()
  if (is.null(p$burst)) p$burst <- default_params_sec()$burst
  if (is.null(p$pause)) p$pause <- default_params_sec()$pause
  if (is.null(p$highfreq)) p$highfreq <- default_params_sec()$highfreq
  preset_name <- as.character(preset_name %||% "balanced_single_unit")
  p$detector$preset_name <- preset_name
  p$detector$analysis_role <- "candidate_event_generator_plus_review"
  p$detector$require_human_or_model_review_for_publication <- TRUE

  if (preset_name == "conservative_single_unit") {
    p$detector$min_valid_isi_sec <- 0.0009
    p$detector$refractory_suspect_sec <- 0.0020
    p$detector$refractory_suspect_action <- "demote_to_possible"
    p$burst$label_possible_burst <- FALSE
    p$burst$label_boundary_possible_burst <- TRUE
    p$burst$local_compression_candidate_class <- "possible_burst"
    p$burst$local_compression_burst_label <- "possible_burst"
    p$burst$final_tonic_like_action <- "demote_to_possible"
    p$burst$long_burst_edge_contrast_min <- max(1.60, p$burst$long_burst_edge_contrast_min %||% 1.45)
    p$burst$long_burst_edge_contrast_geom <- max(1.70, p$burst$long_burst_edge_contrast_geom %||% 1.50)
    p$pause$global_median_guard <- TRUE
    p$pause$global_median_factor <- max(3.0, p$pause$global_median_factor %||% 2.5)
  } else if (preset_name == "sensitive_exploratory") {
    p$detector$refractory_suspect_sec <- 0.0015
    p$detector$refractory_suspect_action <- "warn_only"
    p$burst$label_possible_burst <- TRUE
    p$burst$label_boundary_possible_burst <- TRUE
    p$burst$local_compression_candidate_class <- "possible_burst"
    p$burst$local_compression_burst_label <- "possible_burst"
    p$burst$structure_edge_min <- min(1.15, p$burst$structure_edge_min %||% 1.25)
    p$burst$structure_edge_geom_min <- min(1.25, p$burst$structure_edge_geom_min %||% 1.35)
    p$burst$final_tonic_like_action <- "annotate_only"
    p$pause$global_median_guard <- TRUE
    p$pause$global_median_factor <- min(2.0, p$pause$global_median_factor %||% 2.5)
  } else if (preset_name == "fast_spiking_interneuron") {
    p$detector$refractory_suspect_sec <- 0.0015
    p$detector$refractory_suspect_action <- "demote_to_possible"
    p$burst$final_tonic_like_action <- "demote_to_possible"
    p$burst$local_compression_candidate_class <- "possible_burst"
    p$burst$local_compression_burst_label <- "possible_burst"
    p$highfreq$stable_CV_max <- min(0.30, p$highfreq$stable_CV_max %||% 0.30)
    p$highfreq$spiking_min_spikes <- max(30L, p$highfreq$spiking_min_spikes %||% 30L)
    p$pause$global_median_guard <- TRUE
  } else if (preset_name == "mea_multiunit") {
    p$detector$refractory_suspect_sec <- 0.0015
    p$detector$refractory_suspect_action <- "warn_only"
    p$burst$label_possible_burst <- TRUE
    p$burst$local_compression_candidate_class <- "possible_burst"
    p$burst$local_compression_burst_label <- "possible_burst"
    p$burst$final_tonic_like_action <- "annotate_only"
    p$pause$global_median_guard <- FALSE
  } else {
    # balanced_single_unit defaults
    p$detector$refractory_suspect_sec <- p$detector$refractory_suspect_sec %||% 0.0010
    p$detector$refractory_suspect_action <- p$detector$refractory_suspect_action %||% "demote_to_possible"
    p$burst$final_tonic_like_action <- p$burst$final_tonic_like_action %||% "demote_to_possible"
    p$burst$local_compression_candidate_class <- p$burst$local_compression_candidate_class %||% "possible_burst"
    p$burst$local_compression_burst_label <- p$burst$local_compression_burst_label %||% "possible_burst"
    p$pause$global_median_guard <- p$pause$global_median_guard %||% TRUE
  }
  p
}

candidate_biological_warning <- function(final_class, uncertainty_reason = "", source = "") {
  cls <- as.character(final_class %||% "")
  reason <- as.character(uncertainty_reason %||% "")
  src <- as.character(source %||% "")
  if (cls == "long_burst") return("Structural long_burst candidate: event-like criteria only; distinguish from sustained high-rate epoch by context and validation.")
  if (cls == "possible_burst") return(paste0("Review candidate: ", ifelse(nzchar(reason), reason, "insufficient high-confidence evidence")))
  if (cls %in% c("high_frequency_tonic", "high_frequency_spiking")) return("High-frequency epoch label: interpretation depends on cell type and spike sorting; do not merge with burst without review.")
  if (grepl("refractory|multiunit", reason, ignore.case = TRUE) || grepl("refractory|multiunit", src, ignore.case = TRUE)) return("Contains refractory-suspect evidence; review spike sorting / multi-unit contamination.")
  ""
}

compute_candidate_features_core <- function(ds, ledger = NULL, params = NULL, selected_trains = NULL) {
  if (is.null(ds) || is.null(ds$trains)) return(tibble())
  ledger <- ledger %||% ds$results$candidate_ledger %||% data.frame()
  if (is.null(ledger) || nrow(ledger) == 0) return(tibble())
  min_isi <- params$detector$min_valid_isi_sec %||% 0.0009
  trains <- selected_trains %||% names(ds$trains)
  out <- list()
  for (ii in seq_len(nrow(ledger))) {
    r <- ledger[ii, , drop = FALSE]
    tr <- as.character(r$train %||% "")
    if (!tr %in% trains || !tr %in% names(ds$trains)) next
    dat <- ds$trains[[tr]]
    s0 <- suppressWarnings(as.integer(r$start_isi %||% NA_integer_)); e0 <- suppressWarnings(as.integer(r$end_isi %||% NA_integer_))
    if (!is.finite(s0) || !is.finite(e0) || s0 < 2 || e0 > nrow(dat) || e0 < s0) next
    vals <- valid_isi_values(dat$ISI_sec[s0:e0], min_isi)
    pct <- if ("ISI_pct" %in% names(dat)) suppressWarnings(as.numeric(dat$ISI_pct[s0:e0])) else rep(NA_real_, e0 - s0 + 1L)
    pre <- if (s0 > 2) suppressWarnings(as.numeric(dat$ISI_sec[s0 - 1L])) else NA_real_
    post <- if (e0 < nrow(dat)) suppressWarnings(as.numeric(dat$ISI_sec[e0 + 1L])) else NA_real_
    core_med <- safe_median(vals)
    core_q90 <- if (length(vals) > 0) as.numeric(stats::quantile(vals, probs = 0.90, na.rm = TRUE, names = FALSE)) else NA_real_
    ratios <- c(if (is.finite(pre) && is.finite(core_q90) && core_q90 > 0) pre / core_q90 else NA_real_,
                if (is.finite(post) && is.finite(core_q90) && core_q90 > 0) post / core_q90 else NA_real_)
    mm <- if (length(vals) > 0 && is.finite(mean(vals, na.rm = TRUE)) && mean(vals, na.rm = TRUE) > 0) max(vals, na.rm = TRUE) / mean(vals, na.rm = TRUE) else NA_real_
    cv <- if (length(vals) >= 2 && is.finite(mean(vals, na.rm = TRUE)) && mean(vals, na.rm = TRUE) > 0) stats::sd(vals, na.rm = TRUE) / mean(vals, na.rm = TRUE) else NA_real_
    lv <- calc_LV(vals)
    refr_n <- suppressWarnings(as.numeric(r$refractory_suspect_n %||% NA_real_))
    final_cls <- as.character(r$final_candidate_class %||% r$final_label_majority %||% "")
    ur <- as.character(r$uncertainty_reason %||% "")
    src <- as.character(r$candidate_source %||% "")
    out[[length(out) + 1L]] <- tibble(
      candidate_id = as.character(r$candidate_id %||% paste0("candidate_", ii)),
      run_id = as.character(r$run_id %||% ""),
      params_hash = as.character(r$params_hash %||% ""),
      train = tr,
      start_isi = s0,
      end_isi = e0,
      n_isi = e0 - s0 + 1L,
      n_spikes = e0 - s0 + 2L,
      start_time_sec = suppressWarnings(as.numeric(r$start_time_sec %||% dat$timestamp_sec[s0 - 1L])),
      end_time_sec = suppressWarnings(as.numeric(r$end_time_sec %||% dat$timestamp_sec[e0])),
      duration_sec = dat$timestamp_sec[e0] - dat$timestamp_sec[s0 - 1L],
      candidate_source = src,
      raw_candidate_class = as.character(r$raw_candidate_class %||% ""),
      final_candidate_class = final_cls,
      written_to_auto = as.logical(r$written_to_auto %||% FALSE),
      review_required = final_cls %in% c("possible_burst") || nzchar(ur),
      uncertainty_reason = ur,
      biological_warning = candidate_biological_warning(final_cls, ur, src),
      mean_ISI_sec = if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_,
      median_ISI_sec = core_med,
      q90_ISI_sec = core_q90,
      min_ISI_sec = if (length(vals) > 0) min(vals, na.rm = TRUE) else NA_real_,
      max_ISI_sec = if (length(vals) > 0) max(vals, na.rm = TRUE) else NA_real_,
      mean_ISI_pct = if (any(is.finite(pct))) mean(pct, na.rm = TRUE) else NA_real_,
      max_ISI_pct = if (any(is.finite(pct))) max(pct, na.rm = TRUE) else NA_real_,
      short_ISI_fraction_35pct = if (any(is.finite(pct))) mean(pct <= 35, na.rm = TRUE) else NA_real_,
      pre_ISI_sec = pre,
      post_ISI_sec = post,
      pre_core_ratio = ratios[1],
      post_core_ratio = ratios[2],
      edge_contrast_min = if (any(is.finite(ratios))) min(ratios, na.rm = TRUE) else NA_real_,
      edge_contrast_geom = if (all(is.finite(ratios))) sqrt(ratios[1] * ratios[2]) else NA_real_,
      LV = lv,
      CV = cv,
      MM = mm,
      refractory_suspect_n = refr_n,
      policy_action = as.character(r$policy_action %||% ""),
      rejection_reason = as.character(r$rejection_reason %||% ""),
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) tibble() else bind_rows(out)
}

semantic_consistency_report <- function(ds, params = NULL) {
  if (is.null(ds) || is.null(ds$results)) return(tibble())
  events <- ds$results$events %||% data.frame()
  ledger <- ds$results$candidate_ledger %||% data.frame()
  feats <- ds$results$candidate_features %||% data.frame()
  layer_high <- ds$results$events_high_confidence %||% data.frame()
  layer_review <- ds$results$events_review_candidates %||% data.frame()
  layer_family <- ds$results$events_burst_family %||% data.frame()
  event_ledger <- ds$results$event_ledger %||% data.frame()
  rows <- list()
  add <- function(check, value, status, note) {
    rows[[length(rows) + 1L]] <<- tibble(check = check, value = as.character(value), status = status, note = note)
  }
  ev_n <- if (!is.null(events)) nrow(events) else 0L
  led_n <- if (!is.null(ledger)) nrow(ledger) else 0L
  feat_n <- if (!is.null(feats)) nrow(feats) else 0L
  add("events_count", ev_n, "info", "Number of final events currently stored.")
  add("candidate_ledger_count", led_n, "info", "Candidate ledger contains candidate-stage records only; it can be empty for pure tonic/high-frequency outputs.")
  add("event_ledger_count", if (!is.null(event_ledger)) nrow(event_ledger) else 0L, if (ev_n > 0 && (is.null(event_ledger) || nrow(event_ledger) == 0)) "warn" else "ok", "Event ledger stores final extracted events separately from candidates.")
  add("candidate_feature_count", feat_n, if (led_n > 0 && feat_n == 0) "warn" else "ok", "Feature table should be derived from ledger for audit/export.")
  if (!is.null(events) && nrow(events) > 0) {
    pats <- table(as.character(events$pattern))
    for (nm in names(pats)) add(paste0("events_pattern_", nm), pats[[nm]], "info", "Final event count by pattern.")
  }
  if (!is.null(ledger) && nrow(ledger) > 0) {
    if ("final_candidate_class" %in% names(ledger)) {
      tt <- table(as.character(ledger$final_candidate_class))
      for (nm in names(tt)) add(paste0("ledger_final_class_", nm), tt[[nm]], "info", "Candidate ledger final class count.")
    }
    if ("written_to_auto" %in% names(ledger)) {
      not_written <- sum(!as.logical(ledger$written_to_auto %||% FALSE), na.rm = TRUE)
      add("ledger_not_written_to_auto", not_written, if (not_written > 0) "info" else "ok", "Candidates retained for audit but not written to AUTO labels.")
    }
  }
  add("events_high_confidence_count", if (!is.null(layer_high)) nrow(layer_high) else 0L, "info", "High-confidence layer excludes possible_burst.")
  add("events_review_candidates_count", if (!is.null(layer_review)) nrow(layer_review) else 0L, "info", "Review layer contains possible_burst events.")
  add("events_burst_family_count", if (!is.null(layer_family)) nrow(layer_family) else 0L, "info", "Burst-family layer contains only burst/long_burst/possible_burst events for candidate-recall interpretation.")
  if (length(rows) == 0) tibble() else bind_rows(rows)
}



# Compatibility wrappers used by unified API/export. They keep the
# governance/audit tables as derived outputs instead of changing AUTO labels.
candidate_features_from_results <- function(ds, params = NULL) {
  compute_candidate_features_internal(ds, ds$results$candidate_ledger %||% data.frame(), params %||% (ds$params_last %||% default_params_sec()))
}

final_classify_candidates_internal <- function(features, params = NULL) {
  # Compatibility wrapper. Final decision semantics are centralized in
  # final_classify_candidates(); the internal historical name is kept only so older
  # UI/export paths call the centralized classifier.
  final_classify_candidates(features, params %||% default_params_sec())
}

validation_guidance <- function(ds = NULL, params = NULL) {
  data.frame(
    priority = c("required", "required", "recommended", "recommended", "recommended"),
    validation_step = c(
      "Separate high-confidence events from possible/review candidates in reports.",
      "Use held-out trains or datasets for unbiased performance estimates.",
      "Report params_hash, preset_name, refractory policy, and tonic-like policy.",
      "Use event-level overlap / IoU in addition to per-ISI confusion matrices.",
      "For nonstationary recordings, segment by behavioral/experimental state before interpreting pause thresholds."
    ),
    rationale = c(
      "possible_burst is intentionally reviewable and can inflate burst performance if merged silently.",
      "Manual-learned ranges and threshold tuning can overfit the calibration subset.",
      "Parameter traceability is required for reproducibility.",
      "Per-ISI metrics can overestimate agreement for long events and understate boundary errors.",
      "Global median guards assume a meaningful baseline distribution; nonstationary data violate this assumption."
    ),
    stringsAsFactors = FALSE
  )
}

consistency_audit <- function(ds, params = NULL) {
  semantic_consistency_report(ds, params %||% (ds$params_last %||% default_params_sec()))
}

params_governance_summary <- function(params = default_params_sec()) {
  pp <- effective_params_for_detector(params)
  data.frame(
    field = c("analysis_role", "preset_name", "refractory_suspect_sec", "refractory_suspect_action", "tonic_like_policy", "label_possible_burst", "local_compression_label", "pause_global_median_guard", "pause_global_median_factor"),
    value = c(
      pp$detector$analysis_role %||% "candidate_event_generator_plus_review",
      pp$detector$preset_name %||% "balanced_single_unit",
      as.character(pp$detector$refractory_suspect_sec %||% ""),
      pp$detector$refractory_suspect_action %||% "",
      pp$burst$final_tonic_like_action %||% "",
      as.character(isTRUE(pp$burst$label_possible_burst)),
      pp$burst$local_compression_candidate_class %||% pp$burst$local_compression_burst_label %||% "",
      as.character(isTRUE(pp$pause$global_median_guard %||% TRUE)),
      as.character(pp$pause$global_median_factor %||% "")
    ),
    stringsAsFactors = FALSE
  )
}


