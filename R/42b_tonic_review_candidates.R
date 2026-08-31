# Review-only local Tonic regularity proposals
#
# This module deliberately sits outside the canonical Event/State grammar.  It
# inspects a calibration-frozen Tonic band only after canonical detection has
# finished and returns a separate attribute payload.  Rows emitted here are
# hypotheses for human review (`possible_tonic`), never canonical candidates.
# In particular, this module must not be appended to the candidate audit,
# multitrack shadow, AUTO projection, or any Burst/HFS/Pause gate.

stpd_tonic_review_empty_candidates <- function() {
  data.frame(
    candidate_id = character(), review_label = character(),
    candidate_source = character(), train = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(),
    n_spikes = integer(), start_time_sec = double(), end_time_sec = double(),
    duration_sec = double(), parent_band_run_start_isi = integer(),
    parent_band_run_end_isi = integer(), parent_band_run_n_isi = integer(),
    tonic_lower_sec = double(), tonic_upper_sec = double(),
    tonic_core_lower_sec = double(), tonic_min_duration_sec = double(),
    tonic_min_spikes = integer(), tonic_local_max_isi_count = integer(),
    tonic_lv_max = double(), tonic_mm_min = double(), tonic_mm_max = double(),
    tonic_mm_effective_max = double(), mm_semantics = character(),
    tonic_mm_relaxation_applied = logical(),
    tonic_mm_relaxation_contract_required = logical(),
    tonic_mm_relaxation_contract_valid = logical(),
    tonic_mm_relaxation_contract_reason = character(),
    tonic_mm_relaxation_fallback_applied = logical(),
    tonic_mm_relaxation_mode = character(),
    tonic_mm_relaxation_status = character(),
    native_band_isi_n = integer(),
    native_band_fraction = double(), below_lower_isi_n = integer(),
    below_lower_max_deviation_sec = double(),
    below_lower_max_deviation_fraction = double(),
    above_upper_isi_n = integer(), cv = double(), lv = double(), mm = double(),
    support_count_pass = logical(), duration_pass = logical(),
    lv_pass = logical(), mm_pass = logical(), pause_or_qc_crossed = logical(),
    overlaps_canonical_tonic = logical(), threshold_source_mode = character(),
    review_only = logical(), canonical_eligible = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_tonic_review_overlap <- function(start, end, intervals) {
  if (is.null(intervals) || !is.data.frame(intervals) || nrow(intervals) == 0L ||
      !all(c("start_isi", "end_isi") %in% names(intervals))) {
    return(FALSE)
  }
  left <- suppressWarnings(as.integer(intervals$start_isi))
  right <- suppressWarnings(as.integer(intervals$end_isi))
  valid <- is.finite(left) & is.finite(right) & right >= left
  any(valid & left <= end & right >= start)
}

stpd_tonic_review_hard_mask <- function(n, pause_boundaries = NULL) {
  mask <- rep(FALSE, n)
  if (is.null(pause_boundaries) || !is.data.frame(pause_boundaries) ||
      nrow(pause_boundaries) == 0L ||
      !all(c("start_isi", "end_isi") %in% names(pause_boundaries))) {
    return(mask)
  }
  for (i in seq_len(nrow(pause_boundaries))) {
    start <- suppressWarnings(as.integer(pause_boundaries$start_isi[i]))
    end <- suppressWarnings(as.integer(pause_boundaries$end_isi[i]))
    if (!is.finite(start) || !is.finite(end) || end < start) next
    start <- max(1L, start)
    end <- min(n, end)
    if (end >= start) mask[seq.int(start, end)] <- TRUE
  }
  mask
}

stpd_tonic_review_window_stats <- function(
    dat, isi, valid, hard_mask, start, end, lower, upper, vp) {
  index <- if (is.finite(start) && is.finite(end) && end >= start) {
    seq.int(as.integer(start), as.integer(end))
  } else {
    integer()
  }
  values <- isi[index]
  native <- is.finite(values) & values >= lower & values <= upper
  below <- is.finite(values) & values < lower
  above <- !is.finite(values) | values > upper
  n_isi <- length(index)
  n_spikes <- n_isi + 1L
  min_spikes <- max(3L, stpd_event_core_int(vp$tonic_min_spikes, 3L))
  duration <- if (length(index) > 0L && "timestamp_sec" %in% names(dat) &&
                  start > 1L) {
    suppressWarnings(
      as.numeric(dat$timestamp_sec[end]) - as.numeric(dat$timestamp_sec[start - 1L])
    )
  } else {
    sum(values, na.rm = TRUE)
  }
  if (!is.finite(duration)) duration <- sum(values, na.rm = TRUE)
  min_duration <- max(0, stpd_event_core_num(vp$tonic_min_duration, 0))
  cv <- stpd_event_core_cv(values)
  lv <- stpd_event_core_lv(values)
  mm <- stpd_event_core_mm(values)
  lv_max <- stpd_event_core_num(vp$tonic_lv_max, NA_real_)
  mm_min <- stpd_event_core_num(vp$tonic_mm_min, NA_real_)
  mm_max <- stpd_event_core_num(vp$tonic_mm_max, NA_real_)
  mm_gate <- stpd_tonic_mm_gate(vp, cv = cv, lv = lv)
  mm_effective_max <- mm_gate$effective_max
  support_count_pass <- n_spikes >= min_spikes
  duration_pass <- min_duration <= 0 ||
    (is.finite(duration) && duration >= min_duration)
  lv_pass <- is.finite(lv) && (!is.finite(lv_max) || lv <= lv_max)
  mm_pass <- is.finite(mm) &&
    (!is.finite(mm_min) || mm >= mm_min) &&
    (!is.finite(mm_effective_max) || mm <= mm_effective_max)
  boundary_crossed <- length(index) == 0L ||
    any(!valid[index] | hard_mask[index])
  deviation <- if (any(below)) lower - values[below] else numeric()
  list(
    index = index, values = values, n_isi = n_isi, n_spikes = n_spikes,
    duration = duration, cv = cv, lv = lv, mm = mm,
    mm_effective_max = mm_effective_max,
    mm_relaxation_applied = mm_gate$applied,
    mm_relaxation_contract_required = mm_gate$contract_required,
    mm_relaxation_contract_valid = mm_gate$contract_valid,
    mm_relaxation_contract_reason = mm_gate$contract_reason,
    mm_relaxation_fallback_applied = mm_gate$fallback_applied,
    mm_relaxation_mode = mm_gate$mode,
    mm_relaxation_status = mm_gate$status,
    native_n = sum(native), native_fraction = if (n_isi > 0L) mean(native) else 0,
    below_n = sum(below),
    below_max_deviation = if (length(deviation)) max(deviation) else 0,
    below_max_deviation_fraction = if (length(deviation) && is.finite(lower) && lower > 0) {
      max(deviation) / lower
    } else 0,
    above_n = sum(above), support_count_pass = support_count_pass,
    duration_pass = duration_pass, lv_pass = lv_pass, mm_pass = mm_pass,
    boundary_crossed = boundary_crossed,
    regular_pass = support_count_pass && duration_pass && lv_pass && mm_pass &&
      !boundary_crossed
  )
}

stpd_tonic_review_candidates <- function(
    dat, params, vp, min_isi_sec = 0.001, train = "",
    canonical_tonic = NULL, pause_boundaries = NULL) {
  empty <- stpd_tonic_review_empty_candidates()
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) <= 2L ||
      !("ISI_sec" %in% names(dat))) return(empty)

  patterns <- (params$detector %||% list())$patterns_to_run %||%
    stpd_default_patterns_to_run()
  if (!("tonic" %in% patterns)) return(empty)

  n <- nrow(dat)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  artifact <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !artifact
  valid[1L] <- FALSE
  bounds <- stpd_event_core_tonic_adaptive_bounds(isi, valid, vp, min_isi_sec)
  source_mode <- tolower(as.character(
    bounds$tonic_threshold_source_mode %||% vp$tonic_threshold_source_mode %||% "auto"
  )[1L])
  # Review proposals are meaningful only when the scientific band was frozen
  # before seeing this train.  AUTO/histogram modes already adapt on the train
  # and are deliberately left unchanged.
  if (!(source_mode %in% c("user", "manual", "default"))) return(empty)

  lower <- stpd_event_core_num(bounds$lower, NA_real_)
  upper <- stpd_event_core_num(bounds$upper, NA_real_)
  core_lower <- stpd_event_core_num(bounds$core_lower, lower)
  if (!is.finite(lower) || !is.finite(upper) || lower <= 0 || upper <= lower) {
    return(empty)
  }
  hard_mask <- stpd_tonic_review_hard_mask(n, pause_boundaries)
  native <- valid & !hard_mask & isi >= lower & isi <= upper
  below_or_native <- valid & !hard_mask & isi <= upper
  native[1L] <- FALSE
  below_or_native[1L] <- FALSE

  min_spikes <- max(3L, stpd_event_core_int(vp$tonic_min_spikes, 3L))
  min_isi_count <- max(2L, min_spikes - 1L)
  # A local proposal is deliberately short: at most two minimum-size State
  # supports sharing one boundary spike.  This cap is derived solely from the
  # existing structural count, not from an absolute time or ISI threshold.
  local_max_isi_count <- max(min_isi_count, 2L * min_spikes - 1L)
  proposals <- list()

  add_proposal <- function(start, end, source, parent_start, parent_end) {
    if (stpd_tonic_review_overlap(start, end, canonical_tonic)) return(invisible(NULL))
    stat <- stpd_tonic_review_window_stats(
      dat, isi, valid, hard_mask, start, end, lower, upper, vp
    )
    if (!isTRUE(stat$regular_pass) || stat$above_n > 0L || stat$below_n > 1L) {
      return(invisible(NULL))
    }
    if (identical(source, "near_lower_single_isi") && stat$below_n != 1L) {
      return(invisible(NULL))
    }
    if (identical(source, "local_regular_subwindow") && stat$below_n != 0L) {
      return(invisible(NULL))
    }
    start_time <- if (start > 1L && "timestamp_sec" %in% names(dat)) {
      suppressWarnings(as.numeric(dat$timestamp_sec[start - 1L]))
    } else NA_real_
    end_time <- if ("timestamp_sec" %in% names(dat)) {
      suppressWarnings(as.numeric(dat$timestamp_sec[end]))
    } else NA_real_
    proposals[[length(proposals) + 1L]] <<- data.frame(
      candidate_id = "", review_label = "possible_tonic",
      candidate_source = source, train = as.character(train)[1L],
      start_isi = as.integer(start), end_isi = as.integer(end),
      n_isi = as.integer(stat$n_isi), n_spikes = as.integer(stat$n_spikes),
      start_time_sec = start_time, end_time_sec = end_time,
      duration_sec = stat$duration,
      parent_band_run_start_isi = as.integer(parent_start),
      parent_band_run_end_isi = as.integer(parent_end),
      parent_band_run_n_isi = as.integer(parent_end - parent_start + 1L),
      tonic_lower_sec = lower, tonic_upper_sec = upper,
      tonic_core_lower_sec = core_lower,
      tonic_min_duration_sec = max(0, stpd_event_core_num(vp$tonic_min_duration, 0)),
      tonic_min_spikes = min_spikes,
      tonic_local_max_isi_count = local_max_isi_count,
      tonic_lv_max = stpd_event_core_num(vp$tonic_lv_max, NA_real_),
      tonic_mm_min = stpd_event_core_num(vp$tonic_mm_min, NA_real_),
      tonic_mm_max = stpd_event_core_num(vp$tonic_mm_max, NA_real_),
      tonic_mm_effective_max = stat$mm_effective_max,
      mm_semantics = "current_canonical_max_over_mean_v1",
      tonic_mm_relaxation_applied = stat$mm_relaxation_applied,
      tonic_mm_relaxation_contract_required =
        stat$mm_relaxation_contract_required,
      tonic_mm_relaxation_contract_valid =
        stat$mm_relaxation_contract_valid,
      tonic_mm_relaxation_contract_reason =
        stat$mm_relaxation_contract_reason,
      tonic_mm_relaxation_fallback_applied =
        stat$mm_relaxation_fallback_applied,
      tonic_mm_relaxation_mode = stat$mm_relaxation_mode,
      tonic_mm_relaxation_status = stat$mm_relaxation_status,
      native_band_isi_n = as.integer(stat$native_n),
      native_band_fraction = stat$native_fraction,
      below_lower_isi_n = as.integer(stat$below_n),
      below_lower_max_deviation_sec = stat$below_max_deviation,
      below_lower_max_deviation_fraction = stat$below_max_deviation_fraction,
      above_upper_isi_n = as.integer(stat$above_n),
      cv = stat$cv, lv = stat$lv, mm = stat$mm,
      support_count_pass = stat$support_count_pass,
      duration_pass = stat$duration_pass, lv_pass = stat$lv_pass,
      mm_pass = stat$mm_pass,
      pause_or_qc_crossed = stat$boundary_crossed,
      overlaps_canonical_tonic = FALSE,
      threshold_source_mode = source_mode,
      review_only = TRUE, canonical_eligible = FALSE,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  # Mechanism 1: one below-band ISI split an otherwise locally regular frozen-
  # band support.  Its magnitude is not rescued by a new absolute threshold;
  # the existing LV/MM gates must carry the evidence and only one such ISI is
  # ever allowed in a proposed window.
  relaxed_runs <- stpd_event_core_bool_runs(below_or_native)
  if (nrow(relaxed_runs) > 0L) {
    for (rr in seq_len(nrow(relaxed_runs))) {
      run_start <- as.integer(relaxed_runs$start_isi[rr])
      run_end <- as.integer(relaxed_runs$end_isi[rr])
      low_index <- which(seq_len(n) >= run_start & seq_len(n) <= run_end &
                           valid & !hard_mask & isi < lower)
      if (length(low_index) == 0L) next
      # A long <=upper parent may contain several low excursions.  Each short
      # window is adjudicated independently and must contain exactly one; the
      # parent itself is not required to have only one.  This preserves the
      # fold-7 mechanism without allowing a proposal to bridge multiple low
      # excursions.
      for (low in low_index) {
        first_start <- max(run_start, low - local_max_isi_count + 1L)
        last_end <- min(run_end, low + local_max_isi_count - 1L)
        for (start in seq.int(first_start, low)) {
          min_end <- max(low, start + min_isi_count - 1L)
          max_end <- min(last_end, start + local_max_isi_count - 1L)
          if (max_end < min_end) next
          for (end in seq.int(min_end, max_end)) {
            add_proposal(start, end, "near_lower_single_isi", run_start, run_end)
          }
        }
      }
    }
  }

  # Mechanism 2: a maximal native-band run has enough count/duration support
  # but fails the frozen LV/MM regularity gate because a transition is attached.
  # Search only strict, short subwindows of that failed maximal run.
  native_runs <- stpd_event_core_bool_runs(native)
  if (nrow(native_runs) > 0L) {
    for (rr in seq_len(nrow(native_runs))) {
      run_start <- as.integer(native_runs$start_isi[rr])
      run_end <- as.integer(native_runs$end_isi[rr])
      run_n <- run_end - run_start + 1L
      if (run_n <= min_isi_count) next
      whole <- stpd_tonic_review_window_stats(
        dat, isi, valid, hard_mask, run_start, run_end, lower, upper, vp
      )
      parent_support_pass <- whole$support_count_pass && whole$duration_pass &&
        !whole$boundary_crossed && whole$above_n == 0L
      parent_regularity_failed <- !whole$lv_pass || !whole$mm_pass
      if (!parent_support_pass || !parent_regularity_failed) next
      max_width <- min(local_max_isi_count, run_n - 1L)
      if (max_width < min_isi_count) next
      for (start in seq.int(run_start, run_end - min_isi_count + 1L)) {
        max_end <- min(run_end, start + max_width - 1L)
        min_end <- start + min_isi_count - 1L
        if (max_end < min_end) next
        for (end in seq.int(min_end, max_end)) {
          add_proposal(start, end, "local_regular_subwindow", run_start, run_end)
        }
      }
    }
  }

  if (length(proposals) == 0L) return(empty)
  out <- dplyr::bind_rows(proposals)
  out <- unique(out[, names(empty), drop = FALSE])
  # Deterministic, non-overlapping review burden: longer support first, then
  # higher native-band fraction, then lower LV, then stable geometry order.
  lv_order <- suppressWarnings(as.numeric(out$lv))
  lv_order[!is.finite(lv_order)] <- Inf
  ord <- order(-out$n_isi, -out$native_band_fraction, lv_order,
               out$start_isi, out$end_isi, out$candidate_source,
               method = "radix")
  out <- out[ord, , drop = FALSE]
  keep <- logical(nrow(out))
  occupied <- rep(FALSE, n)
  for (i in seq_len(nrow(out))) {
    index <- seq.int(out$start_isi[i], out$end_isi[i])
    if (any(occupied[index])) next
    keep[i] <- TRUE
    occupied[index] <- TRUE
  }
  out <- out[keep, , drop = FALSE]
  if (nrow(out) == 0L) return(empty)
  out$candidate_id <- sprintf("possible_tonic_review_%03d", seq_len(nrow(out)))
  rownames(out) <- NULL
  out[, names(empty), drop = FALSE]
}
