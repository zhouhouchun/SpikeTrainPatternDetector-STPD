# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# refined dataset APIs and detector-vs-manual evaluation
# ============================================================

pattern_eval_normalize <- function(x, metric_mode = c("strict_high_confidence", "candidate_family", "review_assisted")) {
  metric_mode <- match.arg(metric_mode)
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x[x %in% c("possible burst", "possible-burst")] <- "possible_burst"
  x[x %in% c("long burst", "long-burst", "long_burst", "longburst")] <- "long_burst"
  x[x %in% c("high-frequency tonic", "high frequency tonic", "high_frequency_tonic", "hf tonic", "hf_tonic", "hftonic")] <- "high_frequency_tonic"
  x[x %in% c("high-frequency spiking", "high frequency spiking", "high_frequency_spiking", "hf spiking", "hf_spiking", "hfspiking")] <- "high_frequency_spiking"
  x[x %in% c("other", "unclassified")] <- "others"
  x[x == ""] <- "unlabeled"
  if (metric_mode == "candidate_family") {
    x[x %in% c("burst", "long_burst", "possible_burst")] <- "burst_family"
  }
  # review_assisted currently keeps possible_burst separate; reviewed labels should
  # be exported/evaluated from user-corrected FINAL labels rather than inferred here.
  x
}

result_metric_classes <- function(metric_mode = "strict_high_confidence") {
  metric_mode <- metric_mode %||% "strict_high_confidence"
  if (metric_mode == "candidate_family") return(c("burst_family", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others", "unlabeled"))
  c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others", "unlabeled")
}

manual_event_overlap <- function(truth, pred, train = "", metric_mode = "strict_high_confidence") {
  out <- list()
  labs <- setdiff(result_metric_classes(metric_mode), "unlabeled")
  for (lab in labs) {
    seg <- find_segments(ifelse(truth == lab, lab, ""), lab)
    if (nrow(seg) == 0) next
    for (ii in seq_len(nrow(seg))) {
      idx <- seg$start_isi[ii]:seg$end_isi[ii]
      same <- sum(pred[idx] == lab, na.rm = TRUE)
      out[[length(out) + 1L]] <- tibble(train = train, pattern = lab, start_isi = seg$start_isi[ii], end_isi = seg$end_isi[ii],
                                        n_isi = length(idx), overlap_same_n = same, overlap_same_frac = same / max(1L, length(idx)),
                                        detected_50pct = same / max(1L, length(idx)) >= 0.50)
    }
  }
  if (length(out) == 0) tibble(train = character(), pattern = character(), start_isi = integer(), end_isi = integer(), n_isi = integer(), overlap_same_n = integer(), overlap_same_frac = numeric(), detected_50pct = logical()) else bind_rows(out)
}

strip_learned_ranges_for_eval <- function(params) {
  pp <- params
  family_fields <- list(
    burst = c("adaptive_train_ranges", "train_burst_ranges"),
    tonic = c("adaptive_train_ranges", "train_tonic_ranges"),
    pause = c("adaptive_train_ranges", "train_pause_ranges"),
    highfreq = c("adaptive_train_ranges", "train_highfreq_ranges", "train_isi_ranges")
  )
  for (family in names(family_fields)) {
    if (is.null(pp[[family]]) || !is.list(pp[[family]])) next
    for (field in family_fields[[family]]) pp[[family]][[field]] <- list()
  }
  if (!is.null(pp$detector) && is.list(pp$detector)) pp$detector$train_isi_thresholds <- list()
  if (!is.null(pp$event_grammar) && is.list(pp$event_grammar)) {
    pp$event_grammar$threshold_table <- NULL
    pp$event_grammar$effective_bands <- NULL
    pp$event_grammar$manual_event_table <- NULL
    pp$event_grammar$manual_suggest <- NULL
    pp$event_grammar$histogram_suggest <- NULL
  }
  pp
}

stpd_strip_learned_dataset_settings_for_eval <- function(ds) {
  out <- ds
  if (is.null(out$train_settings) || !is.list(out$train_settings)) return(out)
  for (field in c(
    "burst_isi_ranges", "tonic_isi_ranges", "pause_isi_ranges",
    "highfreq_isi_ranges", "isi_thresholds"
  )) {
    if (field %in% names(out$train_settings)) out$train_settings[[field]] <- list()
  }
  out
}

compute_params_hash <- function(params) {
  tryCatch({
    if (exists("stpd_params_hash", mode = "function")) {
      return(stpd_params_hash(params))
    }
    canonical <- if (exists("stpd_productize_params", mode = "function")) {
      stpd_productize_params(params, prefer = "canonical")
    } else {
      params
    }
    if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(canonical, algo = "sha256", serialize = TRUE)
    } else {
      as.character(stats::runif(1))
    }
  }, error = function(e) paste0("hash_unavailable_", format(Sys.time(), "%Y%m%d%H%M%S")))
}

