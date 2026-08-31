# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Audit / preset / candidate-feature helpers
# ============================================================

stpd_methodological_warning <- function(as_vector = FALSE) {
  lines <- c(
    "Spike Train Pattern Detector \u662F\u5019\u9009\u4E8B\u4EF6\u751F\u6210\u5668\u548C\u534A\u76D1\u7763\u590D\u6838\u5E73\u53F0\uFF0C\u4E0D\u662F\u65E0\u504F\u7684\u6700\u7EC8\u771F\u503C\u5206\u7C7B\u5668\u3002\u62A5\u544A\u7ED3\u679C\u65F6\uFF0C\u5E94\u533A\u5206\u9AD8\u7F6E\u4FE1\u5EA6\u4E8B\u4EF6\u3001\u590D\u6838\u5019\u9009\u548C\u6A21\u5F0F\u5BB6\u65CF\u7EA7\u6458\u8981\uFF0C\u5E76\u5728\u53EF\u884C\u65F6\u63D0\u4F9B\u9A8C\u8BC1\u7ED3\u679C\u3002",
    "\u5E94\u5206\u522B\u62A5\u544A\u9AD8\u7F6E\u4FE1\u5EA6\u4E8B\u4EF6\u3001\u590D\u6838\u5019\u9009\uFF08possible_burst\uFF09\u548C burst \u5BB6\u65CF\u6458\u8981\u3002",
    "long_burst \u662F\u4F9D\u636E spike \u6570\u3001\u6301\u7EED\u65F6\u95F4\u3001\u77ED ISI \u6BD4\u4F8B\u548C\u4E24\u4FA7\u5BF9\u6BD4\u5EA6\u5B9A\u4E49\u7684\u7ED3\u6784/\u4E8B\u4EF6\u578B\u6807\u7B7E\uFF1B\u4E0D\u80FD\u81EA\u52A8\u5C06\u5176\u89E3\u91CA\u4E3A\u72EC\u7ACB\u7684\u751F\u7269\u5B66\u673A\u5236\u3002",
    "high_frequency_tonic \u548C high_frequency_spiking \u662F\u72B6\u6001/\u65F6\u671F\u578B\u6807\u7B7E\uFF1B\u5176\u751F\u7269\u5B66\u89E3\u91CA\u53D6\u51B3\u4E8E\u7EC6\u80DE\u7C7B\u578B\u3001\u5B9E\u9A8C\u5236\u5907\u548C spike sorting \u8D28\u91CF\u3002",
    "\u4F7F\u7528 MANUAL \u6807\u7B7E\u8FDB\u884C\u4EA4\u4E92\u5F0F\u8C03\u53C2\u53EF\u80FD\u5BFC\u81F4\u8FC7\u62DF\u5408\u3002\u7528\u4E8E\u53D1\u8868\u7EA7\u5206\u6790\u65F6\uFF0C\u5E94\u91C7\u7528\u7559\u51FA\u7684 train/\u6570\u636E\u96C6\u3001\u4E8B\u4EF6\u7EA7\u6307\u6807\uFF0C\u5E76\u62A5\u544A\u9884\u8BBE\u540D\u79F0\u548C params_hash\u3002",
    "\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u53EF\u80FD\u63D0\u793A spike sorting\u3001multi-unit \u6DF7\u6742\u6216 timestamp \u95EE\u9898\uFF1B\u9ED8\u8BA4\u7B56\u7565\u662F\u4FDD\u5B88\u590D\u6838/\u964D\u7EA7\uFF0C\u800C\u4E0D\u662F\u9759\u9ED8\u63A5\u53D7\u3002"
  )
  if (isTRUE(as_vector)) lines else paste(lines, collapse = "\n")
}

preset_catalog <- function() {
  data.frame(
    preset_name = c("conservative_single_unit", "balanced_single_unit", "sensitive_exploratory", "fast_spiking_interneuron", "mea_multiunit"),
    label = c("\u4FDD\u5B88\u578B single-unit", "\u5747\u8861\u578B single-unit", "\u7075\u654F\u63A2\u7D22\u578B", "\u5FEB\u901F\u653E\u7535\u4E2D\u95F4\u795E\u7ECF\u5143", "MEA / multi-unit"),
    interpretation = c(
      "\u4E25\u683C\u5904\u7406\u4F2A\u8FF9/\u7591\u4F3C\u4E0D\u5E94\u671F ISI\uFF1B\u91C7\u7528\u4F18\u5148\u590D\u6838\u7684 possible_burst \u7B56\u7565\u3002",
      "\u9ED8\u8BA4\u7684\u5747\u8861\u578B\u5019\u9009\u4E8B\u4EF6\u751F\u6210\u6A21\u5F0F\u3002",
      "\u53EC\u56DE\u7387\u66F4\u9AD8\uFF0C\u590D\u6838\u5019\u9009\u66F4\u591A\uFF1B\u82E5\u65E0\u7559\u51FA\u9A8C\u8BC1\uFF0C\u4E0D\u5EFA\u8BAE\u76F4\u63A5\u7528\u4E8E\u6700\u7EC8\u53D1\u8868\u3002",
      "\u5141\u8BB8\u5C06\u7A33\u5B9A\u9AD8\u9891\u653E\u7535\u89E3\u91CA\u4E3A tonic\uFF1B\u5BF9 burst \u664B\u7EA7\u4FDD\u6301\u4FDD\u5B88\u3002",
      "\u4EE5\u8B66\u544A\u4E3A\u4E3B\u7684\u4E0D\u5E94\u671F\u7B56\u7565\uFF1B\u9002\u7528\u4E8E\u6781\u77ED ISI \u53EF\u80FD\u53CD\u6620\u7FA4\u4F53\u6D3B\u52A8\u7684\u60C5\u51B5\u3002"
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
  if (cls == "long_burst") return("\u7ED3\u6784\u578B long_burst \u5019\u9009\uFF1A\u4EC5\u6EE1\u8DB3\u4E8B\u4EF6\u578B\u5224\u636E\uFF1B\u5E94\u7ED3\u5408\u4E0A\u4E0B\u6587\u548C\u9A8C\u8BC1\uFF0C\u4E0E\u6301\u7EED\u9AD8\u653E\u7535\u7387\u65F6\u671F\u533A\u5206\u3002")
  if (cls == "possible_burst") return(paste0("\u590D\u6838\u5019\u9009\uFF1A", ifelse(nzchar(reason), reason, "\u9AD8\u7F6E\u4FE1\u5EA6\u8BC1\u636E\u4E0D\u8DB3")))
  if (cls %in% c("high_frequency_tonic", "high_frequency_spiking")) return("\u9AD8\u9891\u65F6\u671F\u6807\u7B7E\uFF1A\u89E3\u91CA\u53D6\u51B3\u4E8E\u7EC6\u80DE\u7C7B\u578B\u548C spike sorting\uFF1B\u672A\u7ECF\u590D\u6838\u4E0D\u8981\u4E0E burst \u5408\u5E76\u3002")
  if (grepl("refractory|multiunit", reason, ignore.case = TRUE) || grepl("refractory|multiunit", src, ignore.case = TRUE)) return("\u5305\u542B\u7591\u4F3C\u4E0D\u5E94\u671F\u8BC1\u636E\uFF1B\u8BF7\u590D\u6838 spike sorting / multi-unit \u6DF7\u6742\u3002")
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
  add("events_count", ev_n, "info", "\u5F53\u524D\u5B58\u50A8\u7684\u6700\u7EC8\u4E8B\u4EF6\u6570\u3002")
  add("candidate_ledger_count", led_n, "info", "Candidate ledger \u4EC5\u5305\u542B\u5019\u9009\u9636\u6BB5\u8BB0\u5F55\uFF1B\u7EAF tonic/\u9AD8\u9891\u8F93\u51FA\u65F6\u53EF\u4EE5\u4E3A\u7A7A\u3002")
  add("event_ledger_count", if (!is.null(event_ledger)) nrow(event_ledger) else 0L, if (ev_n > 0 && (is.null(event_ledger) || nrow(event_ledger) == 0)) "warn" else "ok", "Event ledger \u5C06\u6700\u7EC8\u63D0\u53D6\u4E8B\u4EF6\u4E0E\u5019\u9009\u8BB0\u5F55\u5206\u5F00\u5B58\u50A8\u3002")
  add("candidate_feature_count", feat_n, if (led_n > 0 && feat_n == 0) "warn" else "ok", "\u7528\u4E8E\u5BA1\u8BA1/\u5BFC\u51FA\u7684\u7279\u5F81\u8868\u5E94\u7531 ledger \u6D3E\u751F\u3002")
  if (!is.null(events) && nrow(events) > 0) {
    pats <- table(as.character(events$pattern))
    for (nm in names(pats)) add(paste0("events_pattern_", nm), pats[[nm]], "info", "\u6309\u6A21\u5F0F\u7EDF\u8BA1\u7684\u6700\u7EC8\u4E8B\u4EF6\u6570\u3002")
  }
  if (!is.null(ledger) && nrow(ledger) > 0) {
    if ("final_candidate_class" %in% names(ledger)) {
      tt <- table(as.character(ledger$final_candidate_class))
      for (nm in names(tt)) add(paste0("ledger_final_class_", nm), tt[[nm]], "info", "Candidate ledger \u6700\u7EC8\u7C7B\u522B\u8BA1\u6570\u3002")
    }
    if ("written_to_auto" %in% names(ledger)) {
      not_written <- sum(!as.logical(ledger$written_to_auto %||% FALSE), na.rm = TRUE)
      add("ledger_not_written_to_auto", not_written, if (not_written > 0) "info" else "ok", "\u5019\u9009\u4FDD\u7559\u7528\u4E8E\u5BA1\u8BA1\uFF0C\u4F46\u672A\u5199\u5165 AUTO \u6807\u7B7E\u3002")
    }
  }
  add("events_high_confidence_count", if (!is.null(layer_high)) nrow(layer_high) else 0L, "info", "\u9AD8\u7F6E\u4FE1\u5EA6\u5C42\u4E0D\u5305\u542B possible_burst\u3002")
  add("events_review_candidates_count", if (!is.null(layer_review)) nrow(layer_review) else 0L, "info", "Review \u5C42\u5305\u542B possible_burst \u4E8B\u4EF6\u3002")
  add("events_burst_family_count", if (!is.null(layer_family)) nrow(layer_family) else 0L, "info", "Burst-family \u5C42\u4EC5\u5305\u542B burst/long_burst/possible_burst \u4E8B\u4EF6\uFF0C\u7528\u4E8E\u89E3\u91CA\u5019\u9009\u53EC\u56DE\u3002")
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
      "\u5728\u62A5\u544A\u4E2D\u5C06\u9AD8\u7F6E\u4FE1\u5EA6\u4E8B\u4EF6\u4E0E possible/\u590D\u6838\u5019\u9009\u5206\u5F00\u3002",
      "\u4F7F\u7528\u7559\u51FA\u7684 train \u6216\u6570\u636E\u96C6\u83B7\u5F97\u5C3D\u91CF\u65E0\u504F\u7684\u6027\u80FD\u4F30\u8BA1\u3002",
      "\u62A5\u544A params_hash\u3001preset_name\u3001\u4E0D\u5E94\u671F\u7B56\u7565\u548C tonic-like \u7B56\u7565\u3002",
      "\u9664\u9010 ISI \u6DF7\u6DC6\u77E9\u9635\u5916\uFF0C\u8FD8\u5E94\u4F7F\u7528\u4E8B\u4EF6\u7EA7\u91CD\u53E0 / IoU\u3002",
      "\u5BF9\u4E8E\u975E\u5E73\u7A33\u8BB0\u5F55\uFF0C\u5E94\u5148\u6309\u884C\u4E3A/\u5B9E\u9A8C\u72B6\u6001\u5206\u6BB5\uFF0C\u518D\u89E3\u91CA pause \u9608\u503C\u3002"
    ),
    rationale = c(
      "possible_burst \u88AB\u6709\u610F\u8BBE\u8BA1\u4E3A\u53EF\u590D\u6838\u7C7B\u522B\uFF1B\u82E5\u9759\u9ED8\u5E76\u5165 burst\uFF0C\u4F1A\u5938\u5927 burst \u6027\u80FD\u3002",
      "\u7531 MANUAL \u6807\u7B7E\u5B66\u4E60\u7684\u8303\u56F4\u548C\u9608\u503C\u8C03\u8282\u53EF\u80FD\u5BF9\u6821\u51C6\u5B50\u96C6\u8FC7\u62DF\u5408\u3002",
      "\u53EF\u590D\u73B0\u6027\u8981\u6C42\u53C2\u6570\u53EF\u8FFD\u6EAF\u3002",
      "\u9010 ISI \u6307\u6807\u53EF\u80FD\u9AD8\u4F30\u957F\u4E8B\u4EF6\u7684\u4E00\u81F4\u6027\uFF0C\u5E76\u4F4E\u4F30\u8FB9\u754C\u8BEF\u5DEE\u3002",
      "\u5168\u5C40\u4E2D\u4F4D\u6570\u4FDD\u62A4\u5047\u8BBE\u5B58\u5728\u6709\u610F\u4E49\u7684\u57FA\u7EBF\u5206\u5E03\uFF1B\u975E\u5E73\u7A33\u6570\u636E\u4F1A\u8FDD\u53CD\u8BE5\u5047\u8BBE\u3002"
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
