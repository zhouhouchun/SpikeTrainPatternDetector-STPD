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
  if (!is.null(pp$burst)) {
    pp$burst$adaptive_train_ranges <- list()
    pp$burst$train_burst_ranges <- list()
  }
  if (!is.null(pp$tonic)) {
    pp$tonic$adaptive_train_ranges <- list()
    pp$tonic$train_tonic_ranges <- list()
  }
  if (!is.null(pp$pause)) {
    pp$pause$adaptive_train_ranges <- list()
    pp$pause$train_pause_ranges <- list()
  }
  pp
}

compute_params_hash <- function(params) {
  tryCatch({
    if (requireNamespace("digest", quietly = TRUE)) digest::digest(params) else as.character(stats::runif(1))
  }, error = function(e) paste0("hash_unavailable_", format(Sys.time(), "%Y%m%d%H%M%S")))
}



