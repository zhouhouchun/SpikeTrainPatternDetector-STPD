# Poisson-surprise Burst support module
#
# Implements the Legéndy-Salcman / pCLAMP-style algorithm:
#   1. Estimate one baseline firing rate for the complete analysis epoch.
#   2. Find a seed containing at least `min_spikes` consecutive spikes whose
#      internal ISIs are all shorter than the seed threshold (default: 0.5 x
#      the train-wide mean ISI).
#   3. Extend the seed to the right while searching for a higher surprise.
#      Stop at a long ISI (default: 2 x mean ISI) or after 10 consecutive
#      added spikes fail to improve the surprise.
#   4. Remove leading spikes one at a time while doing so increases surprise.
#   5. Retain the optimized, non-overlapping event when S >= threshold.
#
# Count convention: for m spikes spanning [t_start, t_end], the Poisson count
# is k = m - 1 (the first event is included and the last event is excluded),
# matching the classic event-rate convention used by pCLAMP and basal-ganglia
# implementations of Poisson surprise.
#
# This file uses only base R.

stpd_ps_empty_bursts <- function() {
  data.frame(
    method = character(),
    burst_id = integer(),
    start_isi = integer(),
    end_isi = integer(),
    start_spike = integer(),
    end_spike = integer(),
    start_spike_index = integer(),
    end_spike_index = integer(),
    start_time_sec = numeric(),
    end_time_sec = numeric(),
    n_isi = integer(),
    n_spikes = integer(),
    counted_events = integer(),
    duration_sec = numeric(),
    surprise = numeric(),
    surprise_log_base = character(),
    log_tail_probability_natural = numeric(),
    poisson_tail_probability = numeric(),
    expected_count = numeric(),
    baseline_rate_hz = numeric(),
    intraburst_rate_hz = numeric(),
    firing_rate_Hz = numeric(),
    mean_isi_sec = numeric(),
    median_isi_sec = numeric(),
    min_isi_sec = numeric(),
    max_isi_sec = numeric(),
    q90_isi_sec = numeric(),
    q95_isi_sec = numeric(),
    mean_ISI_sec = numeric(),
    median_ISI_sec = numeric(),
    min_ISI_sec = numeric(),
    max_ISI_sec = numeric(),
    q90_ISI_sec = numeric(),
    q95_ISI_sec = numeric(),
    preburst_isi_sec = numeric(),
    postburst_isi_sec = numeric(),
    pre_ISI_sec = numeric(),
    post_ISI_sec = numeric(),
    pre_core_ratio = numeric(),
    post_core_ratio = numeric(),
    edge_min = numeric(),
    edge_geom = numeric(),
    interburst_interval_sec = numeric(),
    previous_burst_id = integer(),
    inter_burst_gap_sec = numeric(),
    inter_burst_onset_interval_sec = numeric(),
    seed_start_spike_index = integer(),
    seed_end_spike_index = integer(),
    right_extension_spikes = integer(),
    front_trim_spikes = integer(),
    extension_stop_reason = character(),
    stringsAsFactors = FALSE
  )
}

stpd_ps_empty_candidates <- function() {
  data.frame(
    candidate_id = integer(),
    accepted = logical(),
    rejection_reason = character(),
    seed_start_spike_index = integer(),
    seed_end_spike_index = integer(),
    optimized_start_spike_index = integer(),
    optimized_end_spike_index = integer(),
    n_spikes = integer(),
    duration_sec = numeric(),
    surprise = numeric(),
    right_extension_spikes = integer(),
    front_trim_spikes = integer(),
    extension_stop_reason = character(),
    stringsAsFactors = FALSE
  )
}

stpd_ps_validate_spike_times <- function(
    spike_times,
    time_unit = c("s", "ms"),
    sort_policy = c("error", "sort"),
    duplicate_policy = c("error", "drop")) {
  time_unit <- match.arg(time_unit)
  sort_policy <- match.arg(sort_policy)
  duplicate_policy <- match.arg(duplicate_policy)

  if (is.factor(spike_times)) {
    spike_times <- as.character(spike_times)
  }

  raw_text <- trimws(as.character(spike_times))
  nonempty <- !is.na(raw_text) & nzchar(raw_text)
  converted <- suppressWarnings(as.numeric(raw_text))

  if (any(nonempty & !is.finite(converted))) {
    bad <- unique(raw_text[nonempty & !is.finite(converted)])
    stop(
      "spike_times contains non-numeric or non-finite values: ",
      paste(utils::head(bad, 5L), collapse = ", "),
      call. = FALSE
    )
  }

  x <- converted[is.finite(converted)]
  if (time_unit == "ms") {
    x <- x / 1000
  }

  if (length(x) < 2L) {
    stop("At least two finite spike timestamps are required.", call. = FALSE)
  }

  was_unsorted <- any(diff(x) < 0)
  if (was_unsorted) {
    if (sort_policy == "error") {
      stop(
        "spike_times must be sorted in strictly increasing order. ",
        "Use sort_policy = 'sort' only when reordering is scientifically justified.",
        call. = FALSE
      )
    }
    x <- sort(x, method = "radix")
  }

  duplicate_n <- sum(duplicated(x))
  if (duplicate_n > 0L) {
    if (duplicate_policy == "error") {
      stop(
        "Duplicate spike timestamps detected (n = ", duplicate_n, "). ",
        "Resolve spike-sorting/export duplicates before burst analysis.",
        call. = FALSE
      )
    }
    x <- x[!duplicated(x)]
  }

  if (length(x) < 2L || any(!is.finite(x)) || any(diff(x) <= 0)) {
    stop("Validated spike timestamps must be finite and strictly increasing.", call. = FALSE)
  }

  list(
    spike_times_sec = x,
    qc = data.frame(
      input_value_n = length(spike_times),
      retained_spike_n = length(x),
      removed_empty_or_na_n = sum(!nonempty | is.na(raw_text)),
      input_was_unsorted = was_unsorted,
      duplicate_timestamp_n = duplicate_n,
      time_unit_in = time_unit,
      sort_policy = sort_policy,
      duplicate_policy = duplicate_policy,
      stringsAsFactors = FALSE
    )
  )
}

stpd_ps_baseline_rate <- function(
    spike_times_sec,
    rate_method = c("mean_isi", "recording_window"),
    recording_start_sec = NULL,
    recording_end_sec = NULL) {
  rate_method <- match.arg(rate_method)
  x <- as.numeric(spike_times_sec)
  n <- length(x)

  if (n < 2L || any(!is.finite(x)) || any(diff(x) <= 0)) {
    stop("spike_times_sec must contain at least two increasing timestamps.", call. = FALSE)
  }

  train_mean_isi_sec <- mean(diff(x))
  if (!is.finite(train_mean_isi_sec) || train_mean_isi_sec <= 0) {
    stop("The train-wide mean ISI is invalid.", call. = FALSE)
  }

  if (rate_method == "mean_isi") {
    observation_start_sec <- x[1L]
    observation_end_sec <- x[n]
    observation_duration_sec <- observation_end_sec - observation_start_sec
    rate_hz <- 1 / train_mean_isi_sec
  } else {
    if (is.null(recording_start_sec) || is.null(recording_end_sec)) {
      stop(
        "recording_start_sec and recording_end_sec are required when ",
        "rate_method = 'recording_window'.",
        call. = FALSE
      )
    }
    observation_start_sec <- suppressWarnings(as.numeric(recording_start_sec)[1L])
    observation_end_sec <- suppressWarnings(as.numeric(recording_end_sec)[1L])
    if (!is.finite(observation_start_sec) ||
        !is.finite(observation_end_sec) ||
        observation_end_sec <= observation_start_sec) {
      stop("The recording window must be finite and have positive duration.", call. = FALSE)
    }
    tolerance <- max(1e-12, abs(observation_end_sec - observation_start_sec) * 1e-12)
    if (x[1L] < observation_start_sec - tolerance ||
        x[n] > observation_end_sec + tolerance) {
      stop("The recording window does not contain all spike timestamps.", call. = FALSE)
    }
    observation_duration_sec <- observation_end_sec - observation_start_sec
    rate_hz <- n / observation_duration_sec
  }

  if (!is.finite(rate_hz) || rate_hz <= 0) {
    stop("The estimated baseline firing rate is invalid.", call. = FALSE)
  }

  list(
    rate_hz = rate_hz,
    train_mean_isi_sec = train_mean_isi_sec,
    observation_start_sec = observation_start_sec,
    observation_end_sec = observation_end_sec,
    observation_duration_sec = observation_duration_sec,
    rate_method = rate_method
  )
}

# Stable log P[X >= k] for X ~ Poisson(mu).
stpd_ps_poisson_upper_log_probability <- function(k, mu) {
  k <- suppressWarnings(as.integer(k)[1L])
  mu <- suppressWarnings(as.numeric(mu)[1L])

  if (is.na(k) || k < 0L) {
    stop("k must be a non-negative integer.", call. = FALSE)
  }
  if (!is.finite(mu) || mu < 0) {
    stop("mu must be a finite, non-negative number.", call. = FALSE)
  }
  if (k == 0L) {
    return(0) # log(1)
  }
  if (mu == 0) {
    return(-Inf)
  }

  log_p <- stats::ppois(
    q = k - 1L,
    lambda = mu,
    lower.tail = FALSE,
    log.p = TRUE
  )
  if (is.finite(log_p)) {
    return(log_p)
  }

  # Defensive fallback for extreme upper tails.  For k > mu,
  # P(X >= k) = P(X = k) * [1 + mu/(k+1) + ...].
  if (k <= mu) {
    stop("Failed to evaluate a non-extreme Poisson upper tail.", call. = FALSE)
  }

  log_pmf_k <- -mu + k * log(mu) - lgamma(k + 1)
  relative_term <- 1
  relative_sum <- 1
  j <- k

  for (iteration in seq_len(100000L)) {
    j <- j + 1L
    relative_term <- relative_term * mu / j
    relative_sum <- relative_sum + relative_term
    if (!is.finite(relative_sum)) {
      stop("Poisson upper-tail fallback overflowed.", call. = FALSE)
    }
    if (relative_term <= relative_sum * .Machine$double.eps * 8) {
      break
    }
    if (iteration == 100000L) {
      stop("Poisson upper-tail fallback did not converge.", call. = FALSE)
    }
  }

  log_pmf_k + log(relative_sum)
}

stpd_ps_surprise_details <- function(
    n_spikes,
    duration_sec,
    baseline_rate_hz,
    log_base = c("e", "10")) {
  log_base <- match.arg(log_base)
  n_spikes <- suppressWarnings(as.integer(n_spikes)[1L])
  duration_sec <- suppressWarnings(as.numeric(duration_sec)[1L])
  baseline_rate_hz <- suppressWarnings(as.numeric(baseline_rate_hz)[1L])

  if (is.na(n_spikes) || n_spikes < 2L) {
    stop("n_spikes must be an integer >= 2.", call. = FALSE)
  }
  if (!is.finite(duration_sec) || duration_sec <= 0) {
    stop("duration_sec must be finite and > 0.", call. = FALSE)
  }
  if (!is.finite(baseline_rate_hz) || baseline_rate_hz <= 0) {
    stop("baseline_rate_hz must be finite and > 0.", call. = FALSE)
  }

  counted_events <- n_spikes - 1L
  expected_count <- baseline_rate_hz * duration_sec
  log_tail <- stpd_ps_poisson_upper_log_probability(counted_events, expected_count)
  surprise_natural <- -log_tail
  surprise <- if (log_base == "10") surprise_natural / log(10) else surprise_natural

  list(
    surprise = surprise,
    counted_events = counted_events,
    expected_count = expected_count,
    log_tail_probability_natural = log_tail,
    poisson_tail_probability = exp(log_tail),
    log_base = log_base
  )
}

stpd_ps_surprise_interval <- function(
    spike_times_sec,
    start_spike_index,
    end_spike_index,
    baseline_rate_hz,
    log_base = c("e", "10")) {
  log_base <- match.arg(log_base)
  x <- spike_times_sec
  start_spike_index <- suppressWarnings(as.integer(start_spike_index)[1L])
  end_spike_index <- suppressWarnings(as.integer(end_spike_index)[1L])

  if (is.na(start_spike_index) || is.na(end_spike_index) ||
      start_spike_index < 1L || end_spike_index > length(x) ||
      end_spike_index <= start_spike_index) {
    stop("Invalid inclusive spike-index interval.", call. = FALSE)
  }

  stpd_ps_surprise_details(
    n_spikes = end_spike_index - start_spike_index + 1L,
    duration_sec = x[end_spike_index] - x[start_spike_index],
    baseline_rate_hz = baseline_rate_hz,
    log_base = log_base
  )
}

stpd_ps_is_improvement <- function(new_value, old_value, tolerance) {
  is.finite(new_value) &&
    is.finite(old_value) &&
    new_value > old_value + tolerance * max(1, abs(old_value))
}

stpd_ps_optimize_seed <- function(
    spike_times_sec,
    seed_start_spike_index,
    min_spikes,
    baseline_rate_hz,
    rejection_isi_sec,
    max_failed_additions,
    log_base,
    improvement_tolerance) {
  x <- spike_times_sec
  n <- length(x)
  seed_start <- as.integer(seed_start_spike_index)
  seed_end <- seed_start + min_spikes - 1L

  initial <- stpd_ps_surprise_interval(
    x, seed_start, seed_end, baseline_rate_hz, log_base
  )

  best_start <- seed_start
  best_end <- seed_end
  best_surprise <- initial$surprise
  probe_end <- best_end
  failed_additions <- 0L
  extension_stop_reason <- "end_of_train"

  while (probe_end < n) {
    next_end <- probe_end + 1L
    next_isi <- x[next_end] - x[next_end - 1L]

    if (next_isi > rejection_isi_sec) {
      extension_stop_reason <- "rejection_isi_exceeded"
      break
    }

    probe_end <- next_end
    trial <- stpd_ps_surprise_interval(
      x, best_start, probe_end, baseline_rate_hz, log_base
    )

    if (stpd_ps_is_improvement(
      trial$surprise, best_surprise, improvement_tolerance
    )) {
      best_end <- probe_end
      best_surprise <- trial$surprise
      failed_additions <- 0L
    } else {
      failed_additions <- failed_additions + 1L
      if (failed_additions >= max_failed_additions) {
        extension_stop_reason <- "max_failed_additions_reached"
        break
      }
    }
  }

  front_trim_spikes <- 0L
  while ((best_end - best_start + 1L) > min_spikes) {
    trial <- stpd_ps_surprise_interval(
      x, best_start + 1L, best_end, baseline_rate_hz, log_base
    )
    if (!stpd_ps_is_improvement(
      trial$surprise, best_surprise, improvement_tolerance
    )) {
      break
    }
    best_start <- best_start + 1L
    best_surprise <- trial$surprise
    front_trim_spikes <- front_trim_spikes + 1L
  }

  final <- stpd_ps_surprise_interval(
    x, best_start, best_end, baseline_rate_hz, log_base
  )

  list(
    seed_start_spike_index = seed_start,
    seed_end_spike_index = seed_end,
    start_spike_index = best_start,
    end_spike_index = best_end,
    n_spikes = best_end - best_start + 1L,
    duration_sec = x[best_end] - x[best_start],
    surprise = final$surprise,
    counted_events = final$counted_events,
    expected_count = final$expected_count,
    log_tail_probability_natural = final$log_tail_probability_natural,
    poisson_tail_probability = final$poisson_tail_probability,
    right_extension_spikes = best_end - seed_end,
    front_trim_spikes = front_trim_spikes,
    extension_stop_reason = extension_stop_reason
  )
}

stpd_ps_make_burst_row <- function(
    optimized,
    spike_times_sec,
    burst_id,
    baseline_rate_hz,
    log_base) {
  x <- spike_times_sec
  s <- optimized$start_spike_index
  e <- optimized$end_spike_index
  event_spikes <- x[seq.int(s, e)]
  event_isis <- diff(event_spikes)
  duration_sec <- event_spikes[length(event_spikes)] - event_spikes[1L]
  q90 <- as.numeric(stats::quantile(
    event_isis, 0.90, na.rm = TRUE, names = FALSE, type = 7
  ))
  q95 <- as.numeric(stats::quantile(
    event_isis, 0.95, na.rm = TRUE, names = FALSE, type = 7
  ))
  mean_isi <- mean(event_isis)
  median_isi <- stats::median(event_isis)
  min_isi <- min(event_isis)
  max_isi <- max(event_isis)
  pre_isi <- if (s > 1L) x[s] - x[s - 1L] else NA_real_
  post_isi <- if (e < length(x)) x[e + 1L] - x[e] else NA_real_

  data.frame(
    method = "poisson_surprise",
    burst_id = as.integer(burst_id),
    start_isi = as.integer(s),
    end_isi = as.integer(e - 1L),
    start_spike = as.integer(s),
    end_spike = as.integer(e),
    start_spike_index = as.integer(s),
    end_spike_index = as.integer(e),
    start_time_sec = event_spikes[1L],
    end_time_sec = event_spikes[length(event_spikes)],
    n_isi = length(event_isis),
    n_spikes = length(event_spikes),
    counted_events = optimized$counted_events,
    duration_sec = duration_sec,
    surprise = optimized$surprise,
    surprise_log_base = log_base,
    log_tail_probability_natural = optimized$log_tail_probability_natural,
    poisson_tail_probability = optimized$poisson_tail_probability,
    expected_count = optimized$expected_count,
    baseline_rate_hz = baseline_rate_hz,
    intraburst_rate_hz = (length(event_spikes) - 1L) / duration_sec,
    firing_rate_Hz = (length(event_spikes) - 1L) / duration_sec,
    mean_isi_sec = mean_isi,
    median_isi_sec = median_isi,
    min_isi_sec = min_isi,
    max_isi_sec = max_isi,
    q90_isi_sec = q90,
    q95_isi_sec = q95,
    mean_ISI_sec = mean_isi,
    median_ISI_sec = median_isi,
    min_ISI_sec = min_isi,
    max_ISI_sec = max_isi,
    q90_ISI_sec = q90,
    q95_ISI_sec = q95,
    preburst_isi_sec = pre_isi,
    postburst_isi_sec = post_isi,
    pre_ISI_sec = pre_isi,
    post_ISI_sec = post_isi,
    pre_core_ratio = if (is.finite(pre_isi) && q90 > 0) pre_isi / q90 else NA_real_,
    post_core_ratio = if (is.finite(post_isi) && q90 > 0) post_isi / q90 else NA_real_,
    edge_min = if (is.finite(pre_isi) && is.finite(post_isi) && q90 > 0) {
      min(pre_isi / q90, post_isi / q90)
    } else {
      NA_real_
    },
    edge_geom = if (is.finite(pre_isi) && is.finite(post_isi) && q90 > 0) {
      sqrt((pre_isi / q90) * (post_isi / q90))
    } else {
      NA_real_
    },
    interburst_interval_sec = NA_real_,
    previous_burst_id = NA_integer_,
    inter_burst_gap_sec = NA_real_,
    inter_burst_onset_interval_sec = NA_real_,
    seed_start_spike_index = optimized$seed_start_spike_index,
    seed_end_spike_index = optimized$seed_end_spike_index,
    right_extension_spikes = optimized$right_extension_spikes,
    front_trim_spikes = optimized$front_trim_spikes,
    extension_stop_reason = optimized$extension_stop_reason,
    stringsAsFactors = FALSE
  )
}

stpd_ps_make_membership <- function(spike_times_sec, bursts) {
  n <- length(spike_times_sec)
  spike_burst_id <- rep(NA_integer_, n)

  if (nrow(bursts) > 0L) {
    for (row in seq_len(nrow(bursts))) {
      idx <- seq.int(
        bursts$start_spike_index[row],
        bursts$end_spike_index[row]
      )
      spike_burst_id[idx] <- bursts$burst_id[row]
    }
  }

  spikes <- data.frame(
    spike_index = seq_len(n),
    timestamp_sec = spike_times_sec,
    burst_id = spike_burst_id,
    in_burst = !is.na(spike_burst_id),
    stringsAsFactors = FALSE
  )

  if (n >= 2L) {
    isi_burst_id <- rep(NA_integer_, n - 1L)
    if (nrow(bursts) > 0L) {
      for (row in seq_len(nrow(bursts))) {
        left <- bursts$start_spike_index[row]
        right <- bursts$end_spike_index[row] - 1L
        if (right >= left) {
          isi_burst_id[seq.int(left, right)] <- bursts$burst_id[row]
        }
      }
    }
    isis <- data.frame(
      isi_index = seq_len(n - 1L),
      left_spike_index = seq_len(n - 1L),
      right_spike_index = seq.int(2L, n),
      start_time_sec = spike_times_sec[-n],
      end_time_sec = spike_times_sec[-1L],
      isi_sec = diff(spike_times_sec),
      burst_id = isi_burst_id,
      in_burst = !is.na(isi_burst_id),
      stringsAsFactors = FALSE
    )
  } else {
    isis <- data.frame()
  }

  list(spikes = spikes, isis = isis)
}

stpd_ps_make_summary <- function(
    spike_times_sec,
    bursts,
    baseline,
    surprise_threshold,
    log_base) {
  n_spikes <- length(spike_times_sec)
  n_bursts <- nrow(bursts)
  spikes_in_bursts <- if (n_bursts > 0L) sum(bursts$n_spikes) else 0L
  total_burst_duration <- if (n_bursts > 0L) sum(bursts$duration_sec) else 0
  observation_duration <- baseline$observation_duration_sec

  data.frame(
    spike_n = n_spikes,
    observation_duration_sec = observation_duration,
    baseline_rate_hz = baseline$rate_hz,
    train_mean_isi_sec = baseline$train_mean_isi_sec,
    rate_method = baseline$rate_method,
    surprise_threshold = surprise_threshold,
    surprise_log_base = log_base,
    burst_n = n_bursts,
    bursts_per_min = if (observation_duration > 0) {
      60 * n_bursts / observation_duration
    } else {
      NA_real_
    },
    bursts_per_1000_spikes = if (n_spikes > 0L) {
      1000 * n_bursts / n_spikes
    } else {
      NA_real_
    },
    spikes_in_bursts = spikes_in_bursts,
    burst_spike_fraction = if (n_spikes > 0L) {
      spikes_in_bursts / n_spikes
    } else {
      NA_real_
    },
    burst_time_fraction = if (observation_duration > 0) {
      total_burst_duration / observation_duration
    } else {
      NA_real_
    },
    total_burst_duration_sec = total_burst_duration,
    mean_burst_duration_sec = if (n_bursts > 0L) {
      mean(bursts$duration_sec)
    } else {
      NA_real_
    },
    median_burst_duration_sec = if (n_bursts > 0L) {
      stats::median(bursts$duration_sec)
    } else {
      NA_real_
    },
    mean_spikes_per_burst = if (n_bursts > 0L) {
      mean(bursts$n_spikes)
    } else {
      NA_real_
    },
    mean_intraburst_rate_hz = if (n_bursts > 0L) {
      mean(bursts$intraburst_rate_hz)
    } else {
      NA_real_
    },
    mean_inter_burst_gap_sec = if (n_bursts > 1L) {
      mean(bursts$inter_burst_gap_sec[-1L])
    } else {
      NA_real_
    },
    median_inter_burst_gap_sec = if (n_bursts > 1L) {
      stats::median(bursts$inter_burst_gap_sec[-1L])
    } else {
      NA_real_
    },
    mean_inter_burst_onset_interval_sec = if (n_bursts > 1L) {
      mean(bursts$inter_burst_onset_interval_sec[-1L])
    } else {
      NA_real_
    },
    median_inter_burst_onset_interval_sec = if (n_bursts > 1L) {
      stats::median(bursts$inter_burst_onset_interval_sec[-1L])
    } else {
      NA_real_
    },
    mean_surprise = if (n_bursts > 0L) {
      mean(bursts$surprise)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

#' Detect bursts using the classical Poisson-surprise algorithm
#'
#' @param spike_times Numeric spike timestamps from one stationary analysis
#'   epoch. Timestamps may contain empty/NA cells but must otherwise be numeric.
#' @param surprise_threshold Minimum optimized surprise. The original method
#'   commonly used S >= 10 with natural logarithms. Studies using -log10(P)
#'   often use thresholds such as 3; the log base and threshold must be reported
#'   together.
#' @param log_base "e" for S = -ln(P), or "10" for S = -log10(P).
#' @param min_spikes Minimum number of spikes in the seed and final burst.
#'   The classical minimum is 3. Some basal-ganglia protocols require 4 spikes
#'   (three ISIs); set min_spikes = 4 explicitly for that protocol.
#' @param seed_isi_factor Automatic seed threshold as a multiple of the
#'   train-wide mean ISI. Classical default: 0.5.
#' @param seed_isi_sec Optional absolute seed ISI threshold in seconds. When
#'   supplied, it overrides seed_isi_factor.
#' @param rejection_isi_factor Automatic long-ISI stopping threshold as a
#'   multiple of the train-wide mean ISI. Classic software default: 2.
#' @param rejection_isi_sec Optional absolute long-ISI stopping threshold in
#'   seconds. When supplied, it overrides rejection_isi_factor.
#' @param max_failed_additions Number of consecutive right-side additions that
#'   may fail to improve surprise before extension stops. Classic default: 10.
#' @param rate_method "mean_isi" uses 1/mean(ISI), matching common classic
#'   implementations when only event timestamps are available.
#'   "recording_window" uses all spikes divided by the known acquisition
#'   duration and requires recording_start_sec and recording_end_sec.
#' @param recording_start_sec,recording_end_sec Known acquisition boundaries,
#'   in seconds, for rate_method = "recording_window".
#' @param time_unit Unit of spike_times: "s" or "ms".
#' @param sort_policy "error" (recommended) or "sort".
#' @param duplicate_policy "error" (recommended) or "drop".
#' @param return_candidates Whether to retain the audit table of all optimized
#'   seed candidates, including candidates below the surprise threshold.
#' @param improvement_tolerance Relative numerical tolerance for declaring a
#'   strict increase in surprise.
#'
#' @return A list of class "poisson_surprise_result" with bursts, candidates,
#'   spike/ISI membership, summary, parameters, QC, and validated timestamps.
stpd_detect_poisson_surprise <- function(
    spike_times,
    surprise_threshold = 10,
    log_base = c("e", "10"),
    min_spikes = 3L,
    seed_isi_factor = 0.5,
    seed_isi_sec = NULL,
    rejection_isi_factor = 2,
    rejection_isi_sec = NULL,
    max_failed_additions = 10L,
    rate_method = c("mean_isi", "recording_window"),
    recording_start_sec = NULL,
    recording_end_sec = NULL,
    time_unit = c("s", "ms"),
    sort_policy = c("error", "sort"),
    duplicate_policy = c("error", "drop"),
    return_candidates = TRUE,
    improvement_tolerance = sqrt(.Machine$double.eps)) {
  log_base <- match.arg(log_base)
  rate_method <- match.arg(rate_method)
  time_unit <- match.arg(time_unit)
  sort_policy <- match.arg(sort_policy)
  duplicate_policy <- match.arg(duplicate_policy)

  surprise_threshold <- suppressWarnings(as.numeric(surprise_threshold)[1L])
  min_spikes <- suppressWarnings(as.integer(min_spikes)[1L])
  max_failed_additions <- suppressWarnings(as.integer(max_failed_additions)[1L])
  seed_isi_factor <- suppressWarnings(as.numeric(seed_isi_factor)[1L])
  rejection_isi_factor <- suppressWarnings(as.numeric(rejection_isi_factor)[1L])
  improvement_tolerance <- suppressWarnings(as.numeric(improvement_tolerance)[1L])

  if (!is.finite(surprise_threshold) || surprise_threshold < 0) {
    stop("surprise_threshold must be finite and >= 0.", call. = FALSE)
  }
  if (is.na(min_spikes) || min_spikes < 3L) {
    stop("min_spikes must be an integer >= 3.", call. = FALSE)
  }
  if (is.na(max_failed_additions) || max_failed_additions < 1L) {
    stop("max_failed_additions must be an integer >= 1.", call. = FALSE)
  }
  if (!is.finite(improvement_tolerance) || improvement_tolerance < 0) {
    stop("improvement_tolerance must be finite and >= 0.", call. = FALSE)
  }

  validated <- stpd_ps_validate_spike_times(
    spike_times,
    time_unit = time_unit,
    sort_policy = sort_policy,
    duplicate_policy = duplicate_policy
  )
  x <- validated$spike_times_sec

  baseline <- stpd_ps_baseline_rate(
    x,
    rate_method = rate_method,
    recording_start_sec = recording_start_sec,
    recording_end_sec = recording_end_sec
  )
  mean_isi_sec <- baseline$train_mean_isi_sec

  if (is.null(seed_isi_sec)) {
    if (!is.finite(seed_isi_factor) || seed_isi_factor <= 0) {
      stop("seed_isi_factor must be finite and > 0.", call. = FALSE)
    }
    effective_seed_isi_sec <- seed_isi_factor * mean_isi_sec
    seed_source <- "factor_x_train_mean_isi"
  } else {
    effective_seed_isi_sec <- suppressWarnings(as.numeric(seed_isi_sec)[1L])
    seed_source <- "absolute_seconds"
  }

  if (is.null(rejection_isi_sec)) {
    if (!is.finite(rejection_isi_factor) || rejection_isi_factor <= 0) {
      stop("rejection_isi_factor must be finite and > 0.", call. = FALSE)
    }
    effective_rejection_isi_sec <- rejection_isi_factor * mean_isi_sec
    rejection_source <- "factor_x_train_mean_isi"
  } else {
    effective_rejection_isi_sec <- suppressWarnings(as.numeric(rejection_isi_sec)[1L])
    rejection_source <- "absolute_seconds"
  }

  if (!is.finite(effective_seed_isi_sec) || effective_seed_isi_sec <= 0) {
    stop("The effective seed ISI threshold must be finite and > 0.", call. = FALSE)
  }
  if (!is.finite(effective_rejection_isi_sec) ||
      effective_rejection_isi_sec <= 0) {
    stop("The effective rejection ISI threshold must be finite and > 0.", call. = FALSE)
  }
  if (effective_rejection_isi_sec < effective_seed_isi_sec) {
    stop(
      "rejection_isi_sec must be greater than or equal to seed_isi_sec.",
      call. = FALSE
    )
  }

  burst_rows <- list()
  candidate_rows <- list()
  burst_id <- 0L
  candidate_id <- 0L
  i <- 1L
  last_seed_start <- length(x) - min_spikes + 1L

  if (last_seed_start >= 1L) {
    while (i <= last_seed_start) {
      seed_indices <- seq.int(i, i + min_spikes - 1L)
      seed_isis <- diff(x[seed_indices])
      is_seed <- length(seed_isis) == (min_spikes - 1L) &&
        all(seed_isis < effective_seed_isi_sec)

      if (!is_seed) {
        i <- i + 1L
        next
      }

      candidate_id <- candidate_id + 1L
      optimized <- stpd_ps_optimize_seed(
        spike_times_sec = x,
        seed_start_spike_index = i,
        min_spikes = min_spikes,
        baseline_rate_hz = baseline$rate_hz,
        rejection_isi_sec = effective_rejection_isi_sec,
        max_failed_additions = max_failed_additions,
        log_base = log_base,
        improvement_tolerance = improvement_tolerance
      )

      accepted <- is.finite(optimized$surprise) &&
        optimized$surprise >= surprise_threshold
      rejection_reason <- if (accepted) "" else "surprise_below_threshold"

      if (isTRUE(return_candidates)) {
        candidate_rows[[length(candidate_rows) + 1L]] <- data.frame(
          candidate_id = candidate_id,
          accepted = accepted,
          rejection_reason = rejection_reason,
          seed_start_spike_index = optimized$seed_start_spike_index,
          seed_end_spike_index = optimized$seed_end_spike_index,
          optimized_start_spike_index = optimized$start_spike_index,
          optimized_end_spike_index = optimized$end_spike_index,
          n_spikes = optimized$n_spikes,
          duration_sec = optimized$duration_sec,
          surprise = optimized$surprise,
          right_extension_spikes = optimized$right_extension_spikes,
          front_trim_spikes = optimized$front_trim_spikes,
          extension_stop_reason = optimized$extension_stop_reason,
          stringsAsFactors = FALSE
        )
      }

      if (accepted) {
        burst_id <- burst_id + 1L
        burst_rows[[length(burst_rows) + 1L]] <- stpd_ps_make_burst_row(
          optimized,
          spike_times_sec = x,
          burst_id = burst_id,
          baseline_rate_hz = baseline$rate_hz,
          log_base = log_base
        )
        # Classic non-overlap policy: resume at the first spike after the
        # optimized burst.
        i <- optimized$end_spike_index + 1L
      } else {
        i <- i + 1L
      }
    }
  }

  bursts <- if (length(burst_rows) > 0L) {
    do.call(rbind, burst_rows)
  } else {
    stpd_ps_empty_bursts()
  }
  rownames(bursts) <- NULL

  if (nrow(bursts) > 1L) {
    current <- 2L:nrow(bursts)
    previous <- 1L:(nrow(bursts) - 1L)
    bursts$previous_burst_id[current] <- bursts$burst_id[previous]
    bursts$inter_burst_gap_sec[current] <-
      bursts$start_time_sec[current] - bursts$end_time_sec[previous]
    bursts$inter_burst_onset_interval_sec[current] <-
      bursts$start_time_sec[current] - bursts$start_time_sec[previous]
    # Backward-compatible alias. Its explicit definition is end-to-start gap.
    bursts$interburst_interval_sec <- bursts$inter_burst_gap_sec
  }

  candidates <- if (isTRUE(return_candidates) && length(candidate_rows) > 0L) {
    do.call(rbind, candidate_rows)
  } else {
    stpd_ps_empty_candidates()
  }
  rownames(candidates) <- NULL

  membership <- stpd_ps_make_membership(x, bursts)
  summary <- stpd_ps_make_summary(
    x,
    bursts,
    baseline,
    surprise_threshold = surprise_threshold,
    log_base = log_base
  )

  parameters <- data.frame(
    surprise_threshold = surprise_threshold,
    surprise_log_base = log_base,
    count_convention = "first_event_included_last_event_excluded",
    min_spikes = min_spikes,
    train_mean_isi_sec = mean_isi_sec,
    seed_isi_sec = effective_seed_isi_sec,
    seed_isi_source = seed_source,
    seed_isi_factor = if (seed_source == "factor_x_train_mean_isi") {
      seed_isi_factor
    } else {
      NA_real_
    },
    rejection_isi_sec = effective_rejection_isi_sec,
    rejection_isi_source = rejection_source,
    rejection_isi_factor = if (rejection_source == "factor_x_train_mean_isi") {
      rejection_isi_factor
    } else {
      NA_real_
    },
    max_failed_additions = max_failed_additions,
    rate_method = baseline$rate_method,
    baseline_rate_hz = baseline$rate_hz,
    recording_start_sec = baseline$observation_start_sec,
    recording_end_sec = baseline$observation_end_sec,
    improvement_tolerance = improvement_tolerance,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      bursts = bursts,
      candidates = candidates,
      spike_membership = membership$spikes,
      isi_membership = membership$isis,
      summary = summary,
      parameters = parameters,
      qc = validated$qc,
      spike_times_sec = x
    ),
    class = c("poisson_surprise_result", "list")
  )
}

print.poisson_surprise_result <- function(x, ...) {
  cat("Classical Poisson-surprise burst detection\n")
  cat("  spikes:        ", x$summary$spike_n, "\n", sep = "")
  cat("  duration (s):  ", signif(x$summary$observation_duration_sec, 7), "\n", sep = "")
  cat("  baseline (Hz): ", signif(x$summary$baseline_rate_hz, 7), "\n", sep = "")
  cat("  bursts:        ", x$summary$burst_n, "\n", sep = "")
  cat(
    "  criterion:      S >= ", x$parameters$surprise_threshold,
    " with log base ", x$parameters$surprise_log_base, "\n", sep = ""
  )
  invisible(x)
}

summary.poisson_surprise_result <- function(object, ...) {
  object$summary
}

# Batch wrapper for a wide data frame: one spike train per column.
stpd_detect_poisson_surprise_dataframe <- function(data, ...) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame with one spike train per column.", call. = FALSE)
  }
  if (ncol(data) == 0L) {
    stop("data has no columns.", call. = FALSE)
  }

  train_names <- names(data)
  results <- vector("list", length(train_names))
  names(results) <- train_names

  burst_tables <- list()
  candidate_tables <- list()
  summary_tables <- list()
  qc_tables <- list()

  for (j in seq_along(train_names)) {
    train <- train_names[j]
    result <- stpd_detect_poisson_surprise(data[[j]], ...)
    results[[j]] <- result

    b <- result$bursts
    b$train <- rep(train, nrow(b))
    if (nrow(b) > 0L) {
      b <- b[, c("train", setdiff(names(b), "train")), drop = FALSE]
    }
    burst_tables[[j]] <- b

    cnd <- result$candidates
    cnd$train <- rep(train, nrow(cnd))
    if (nrow(cnd) > 0L) {
      cnd <- cnd[, c("train", setdiff(names(cnd), "train")), drop = FALSE]
    }
    candidate_tables[[j]] <- cnd

    sm <- result$summary
    sm$train <- train
    sm <- sm[, c("train", setdiff(names(sm), "train")), drop = FALSE]
    summary_tables[[j]] <- sm

    q <- result$qc
    q$train <- train
    q <- q[, c("train", setdiff(names(q), "train")), drop = FALSE]
    qc_tables[[j]] <- q
  }

  bind_nonempty <- function(tables, empty = data.frame()) {
    keep <- vapply(tables, function(z) is.data.frame(z) && nrow(z) > 0L, logical(1))
    if (!any(keep)) return(empty)
    out <- do.call(rbind, tables[keep])
    rownames(out) <- NULL
    out
  }

  structure(
    list(
      by_train = results,
      bursts = bind_nonempty(burst_tables),
      candidates = bind_nonempty(candidate_tables),
      summary = bind_nonempty(summary_tables),
      qc = bind_nonempty(qc_tables)
    ),
    class = c("poisson_surprise_batch_result", "list")
  )
}

stpd_poisson_surprise_support_dataset <- function(
    ds,
    selected_trains = NULL,
    surprise_threshold = 10,
    log_base = c("e", "10"),
    min_spikes = 3L,
    seed_isi_factor = 0.5,
    seed_isi_sec = NULL,
    rejection_isi_factor = 2,
    rejection_isi_sec = NULL,
    max_failed_additions = 10L,
    rate_method = "mean_isi",
    recording_start_sec = NULL,
    recording_end_sec = NULL,
    improvement_tolerance = sqrt(.Machine$double.eps)) {
  if (is.null(ds) || is.null(ds$trains) || !length(ds$trains)) {
    stop("Dataset has no trains.", call. = FALSE)
  }
  log_base <- match.arg(log_base)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(as.character(trains), names(ds$trains))
  if (!length(trains)) stop("No selected trains exist in the dataset.", call. = FALSE)

  bind_rows <- function(x) {
    keep <- vapply(x, function(z) is.data.frame(z) && nrow(z), logical(1))
    if (!any(keep)) return(data.frame())
    out <- do.call(rbind, x[keep])
    rownames(out) <- NULL
    out
  }
  train_value <- function(value, train) {
    if (is.null(value)) return(NULL)
    if (length(value) == 1L && is.null(names(value))) return(value)
    if (!is.null(names(value)) && train %in% names(value)) return(value[[train]])
    stop("Recording-window values must be scalar or named by train.", call. = FALSE)
  }

  by_train <- setNames(vector("list", length(trains)), trains)
  bursts <- candidates <- spike_support <- isi_support <- summaries <- thresholds <- qc <- list()
  for (train in trains) {
    dat <- ds$trains[[train]]
    if (!is.data.frame(dat) || !("timestamp_sec" %in% names(dat))) {
      stop("Each train must contain timestamp_sec.", call. = FALSE)
    }
    result <- stpd_detect_poisson_surprise(
      spike_times = dat$timestamp_sec,
      surprise_threshold = surprise_threshold,
      log_base = log_base,
      min_spikes = min_spikes,
      seed_isi_factor = seed_isi_factor,
      seed_isi_sec = seed_isi_sec,
      rejection_isi_factor = rejection_isi_factor,
      rejection_isi_sec = rejection_isi_sec,
      max_failed_additions = max_failed_additions,
      rate_method = rate_method,
      recording_start_sec = train_value(recording_start_sec, train),
      recording_end_sec = train_value(recording_end_sec, train),
      time_unit = "s",
      sort_policy = "error",
      duplicate_policy = "error",
      return_candidates = TRUE,
      improvement_tolerance = improvement_tolerance
    )
    by_train[[train]] <- result

    add_train <- function(table) {
      table$train <- rep(train, nrow(table))
      table[, c("train", setdiff(names(table), "train")), drop = FALSE]
    }
    bursts[[train]] <- add_train(result$bursts)
    candidates[[train]] <- add_train(result$candidates)
    spike_support[[train]] <- add_train(result$spike_membership)
    isi_support[[train]] <- add_train(result$isi_membership)
    summaries[[train]] <- add_train(result$summary)
    qc[[train]] <- add_train(result$qc)

    p <- result$parameters[1L, , drop = FALSE]
    thresholds[[train]] <- data.frame(
      train = train,
      threshold_name = c(
        "surprise_threshold", "seed_isi_sec", "rejection_isi_sec",
        "baseline_rate_hz"
      ),
      threshold_value = c(
        p$surprise_threshold, p$seed_isi_sec, p$rejection_isi_sec,
        p$baseline_rate_hz
      ),
      threshold_unit = c("dimensionless", "sec", "sec", "hz"),
      threshold_source = c(
        "declared_parameter", p$seed_isi_source,
        p$rejection_isi_source, p$rate_method
      ),
      comparison_operator = c("ge", "lt", "le", "reported"),
      support_role = "auxiliary_burst_evidence_only",
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      method = "poisson_surprise",
      authority_scope = "support_evidence_only",
      by_train = by_train,
      bursts = bind_rows(bursts),
      candidates = bind_rows(candidates),
      spike_support = bind_rows(spike_support),
      isi_support = bind_rows(isi_support),
      summary = bind_rows(summaries),
      thresholds = bind_rows(thresholds),
      qc = bind_rows(qc)
    ),
    class = c("stpd_poisson_surprise_support", "list")
  )
}

stpd_poisson_surprise_support_export <- function(support, out_dir) {
  if (!is.list(support) ||
      !identical(support$authority_scope, "support_evidence_only")) {
    stop("support must be a Poisson-surprise auxiliary support result.", call. = FALSE)
  }
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  tables <- list(
    PS_thresholds.csv = support$thresholds,
    PS_burst_features.csv = support$bursts,
    PS_all_candidates.csv = support$candidates,
    PS_spike_support.csv = support$spike_support,
    PS_ISI_support.csv = support$isi_support,
    PS_support_summary.csv = support$summary,
    PS_QC.csv = support$qc
  )
  for (filename in names(tables)) {
    table <- tables[[filename]]
    if (is.data.frame(table) && nrow(table) > 0L) {
      write_csv_safe(
        table,
        file.path(out_dir, filename),
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  }
  invisible(normalizePath(out_dir, winslash = "/", mustWork = TRUE))
}

# Lightweight internal checks. Run manually with stpd_ps_self_test().
stpd_ps_self_test <- function(verbose = TRUE) {
  # 1. Score definition: 4 spikes span 30 ms; count is 3, mu is 3.
  details <- stpd_ps_surprise_details(
    n_spikes = 4L,
    duration_sec = 0.03,
    baseline_rate_hz = 100,
    log_base = "e"
  )
  expected <- -stats::ppois(
    q = 2L,
    lambda = 3,
    lower.tail = FALSE,
    log.p = TRUE
  )
  stopifnot(isTRUE(all.equal(details$surprise, expected, tolerance = 1e-12)))

  # 2. A perfectly regular 100-Hz train has no seed under the classical
  #    half-mean-ISI rule, so it must not become one giant burst.
  regular <- seq(0, 2, by = 0.01)
  regular_result <- stpd_detect_poisson_surprise(
    regular,
    surprise_threshold = 3,
    log_base = "10"
  )
  stopifnot(nrow(regular_result$bursts) == 0L)

  # 3. A compact packet embedded in a slow baseline should be detected.
  synthetic_isi <- c(rep(0.10, 50L), rep(0.005, 7L), rep(0.10, 50L))
  synthetic <- c(0, cumsum(synthetic_isi))
  synthetic_result <- stpd_detect_poisson_surprise(
    synthetic,
    surprise_threshold = 5,
    log_base = "e"
  )
  stopifnot(nrow(synthetic_result$bursts) >= 1L)
  stopifnot(any(
    synthetic_result$bursts$start_spike_index <= 52L &
      synthetic_result$bursts$end_spike_index >= 58L
  ))

  # 4. Seconds and milliseconds must give identical spike-index geometry.
  milliseconds_result <- stpd_detect_poisson_surprise(
    synthetic * 1000,
    surprise_threshold = 5,
    log_base = "e",
    time_unit = "ms"
  )
  stopifnot(identical(
    synthetic_result$bursts$start_spike_index,
    milliseconds_result$bursts$start_spike_index
  ))
  stopifnot(identical(
    synthetic_result$bursts$end_spike_index,
    milliseconds_result$bursts$end_spike_index
  ))

  if (isTRUE(verbose)) {
    message("All Poisson-surprise self-tests passed.")
  }
  invisible(TRUE)
}
