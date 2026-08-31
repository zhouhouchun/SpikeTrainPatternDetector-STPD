# HFS-local nested Burst review-candidate generator
#
# This module mines short, locally accelerated proposals only inside an already
# accepted Broad-HFS support interval.  The calibration evidence available at
# implementation time does not separate true nested Bursts from random low-tail
# HFS fluctuations well enough for automatic promotion.  Every emitted row is
# therefore `possible_burst` review evidence and is never a canonical Event.
# All scientific gates are dimensionless ratios, ranks, or counts;
# `min_isi_sec` is used only by the pre-existing artifact/QC policy.

stpd_nested_hfs_num <- function(x, default) {
  value <- suppressWarnings(as.numeric(x))[1L]
  if (!is.finite(value)) default else value
}

stpd_nested_hfs_int <- function(x, default) {
  value <- suppressWarnings(as.integer(round(as.numeric(x))))[1L]
  if (!is.finite(value)) as.integer(default) else as.integer(value)
}

stpd_nested_hfs_settings <- function(settings = list()) {
  settings <- settings %||% list()
  out <- list(
    context_isi_n = max(4L, stpd_nested_hfs_int(
      settings$context_isi_n %||% 10L, 10L
    )),
    guard_isi_n = max(0L, stpd_nested_hfs_int(
      settings$guard_isi_n %||% 1L, 1L
    )),
    min_background_isi_n = max(3L, stpd_nested_hfs_int(
      settings$min_background_isi_n %||% 4L, 4L
    )),
    local_min_radius = max(1L, stpd_nested_hfs_int(
      settings$local_min_radius %||% 3L, 3L
    )),
    seed_pair_rank_max = min(0.5, max(0, stpd_nested_hfs_num(
      settings$seed_pair_rank_max %||% 0.20, 0.20
    ))),
    seed_side_ratio_min = max(1, stpd_nested_hfs_num(
      settings$seed_side_ratio_min %||% 1.20, 1.20
    )),
    seed_geom_ratio_min = max(1, stpd_nested_hfs_num(
      settings$seed_geom_ratio_min %||% 1.30, 1.30
    )),
    direct_expand_factor = max(1, stpd_nested_hfs_num(
      settings$direct_expand_factor %||% 1.50, 1.50
    )),
    edge_side_ratio_min = max(1, stpd_nested_hfs_num(
      settings$edge_side_ratio_min %||% 1.50, 1.50
    )),
    edge_geom_ratio_min = max(1, stpd_nested_hfs_num(
      settings$edge_geom_ratio_min %||% 1.80, 1.80
    )),
    bridge_ratio_max = max(1, stpd_nested_hfs_num(
      settings$bridge_ratio_max %||% 3.50, 3.50
    )),
    bridge_max_count = max(0L, stpd_nested_hfs_int(
      settings$bridge_max_count %||% 1L, 1L
    )),
    max_consecutive_borrowed = max(0L, stpd_nested_hfs_int(
      settings$max_consecutive_borrowed %||% 1L, 1L
    )),
    native_fraction_min = min(1, max(0.5, stpd_nested_hfs_num(
      settings$native_fraction_min %||% 0.55, 0.55
    ))),
    min_spikes = max(4L, stpd_nested_hfs_int(
      settings$min_spikes %||% 4L, 4L
    )),
    max_spikes = max(4L, stpd_nested_hfs_int(
      settings$max_spikes %||% 10L, 10L
    )),
    max_expand_each_side = max(1L, stpd_nested_hfs_int(
      settings$max_expand_each_side %||% 10L, 10L
    )),
    max_candidates = max(1L, stpd_nested_hfs_int(
      settings$max_candidates %||% 300L, 300L
    ))
  )
  if (out$max_spikes < out$min_spikes) out$max_spikes <- out$min_spikes
  out
}

stpd_nested_hfs_empty_candidates <- function() {
  data.frame(
    candidate_id = character(), candidate_layer = character(),
    candidate_source = character(), final_label = character(),
    action = character(), decision_path = character(),
    parent_hfs_candidate_id = character(), start_isi = integer(),
    end_isi = integer(), n_isi = integer(), n_spikes = integer(),
    score = double(), priority = double(), seed_start_isi = integer(),
    seed_end_isi = integer(), seed_pair_max_sec = double(),
    seed_pair_local_rank = double(), seed_left_background_sec = double(),
    seed_right_background_sec = double(), seed_left_ratio = double(),
    seed_right_ratio = double(), seed_geom_ratio = double(),
    direct_upper_sec = double(), candidate_upper_sec = double(),
    final_q90_sec = double(), final_left_background_sec = double(),
    final_right_background_sec = double(), final_left_ratio = double(),
    final_right_ratio = double(), final_geom_ratio = double(),
    immediate_left_ratio = double(), immediate_right_ratio = double(),
    immediate_geom_ratio = double(), robust_edge_pass = logical(),
    immediate_edge_pass = logical(), review_evidence_strength = character(),
    bridge_isi_count = integer(), bridge_max_to_core_median_ratio = double(),
    native_isi_fraction = double(), rollback_left_to_intrusion_onset = logical(),
    rollback_right_to_intrusion_onset = logical(),
    nested_in_hfs = logical(), absolute_pattern_threshold_used = logical(),
    canonical_eligible = logical(), review_only = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_nested_hfs_is_artifact <- function(x, min_isi_sec) {
  if (exists("is_artifact_isi", mode = "function", inherits = TRUE)) {
    return(is_artifact_isi(x, min_isi_sec))
  }
  !is.finite(x) | x <= 0 | x < min_isi_sec
}

stpd_nested_hfs_bool_runs <- function(flag) {
  flag <- as.logical(flag)
  flag[is.na(flag)] <- FALSE
  index <- which(flag)
  if (!length(index)) {
    return(data.frame(start_isi = integer(), end_isi = integer()))
  }
  group <- cumsum(c(TRUE, diff(index) != 1L))
  parts <- split(index, group)
  data.frame(
    start_isi = as.integer(vapply(parts, min, integer(1))),
    end_isi = as.integer(vapply(parts, max, integer(1))),
    stringsAsFactors = FALSE
  )
}

stpd_nested_hfs_hard_mask <- function(n, hard_boundaries = NULL) {
  mask <- rep(FALSE, n)
  if (is.null(hard_boundaries) || !is.data.frame(hard_boundaries) ||
      !nrow(hard_boundaries) ||
      !all(c("start_isi", "end_isi") %in% names(hard_boundaries))) {
    return(mask)
  }
  boundaries <- hard_boundaries
  if ("hard_for_event" %in% names(boundaries)) {
    keep <- as.logical(boundaries$hard_for_event)
    # An unknown hard-boundary flag fails closed; only an explicit FALSE makes
    # the row transparent to Event generation.
    keep[is.na(keep)] <- TRUE
    boundaries <- boundaries[keep, , drop = FALSE]
  }
  for (i in seq_len(nrow(boundaries))) {
    start <- suppressWarnings(as.integer(boundaries$start_isi[i]))
    end <- suppressWarnings(as.integer(boundaries$end_isi[i]))
    if (!is.finite(start) || !is.finite(end) || end < start) next
    clipped_start <- max(1L, start)
    clipped_end <- min(n, end)
    if (clipped_end < clipped_start) next
    index <- seq.int(clipped_start, clipped_end)
    if (length(index)) mask[index] <- TRUE
  }
  mask
}

stpd_nested_hfs_side_background <- function(
    isi, valid, segment_start, segment_end, start, end, settings) {
  guard <- settings$guard_isi_n
  width <- settings$context_isi_n
  left_end <- start - guard - 1L
  left_start <- max(segment_start, left_end - width + 1L)
  right_start <- end + guard + 1L
  right_end <- min(segment_end, right_start + width - 1L)
  left_index <- if (left_end >= left_start) seq.int(left_start, left_end) else integer()
  right_index <- if (right_end >= right_start) seq.int(right_start, right_end) else integer()
  left_values <- isi[left_index][valid[left_index]]
  right_values <- isi[right_index][valid[right_index]]
  left_values <- left_values[is.finite(left_values) & left_values > 0]
  right_values <- right_values[is.finite(right_values) & right_values > 0]
  list(
    left = if (length(left_values) >= settings$min_background_isi_n) {
      stats::median(left_values)
    } else NA_real_,
    right = if (length(right_values) >= settings$min_background_isi_n) {
      stats::median(right_values)
    } else NA_real_,
    left_n = length(left_values), right_n = length(right_values),
    left_index = left_index, right_index = right_index
  )
}

stpd_nested_hfs_pair_rank <- function(
    isi, valid, segment_start, segment_end, pair_start, settings) {
  context_start <- max(segment_start, pair_start - settings$context_isi_n)
  context_end <- min(segment_end - 1L,
                     pair_start + 1L + settings$context_isi_n)
  starts <- if (context_end >= context_start) {
    seq.int(context_start, context_end)
  } else integer()
  starts <- starts[
    starts >= segment_start & starts + 1L <= segment_end &
      valid[starts] & valid[starts + 1L]
  ]
  if (!length(starts)) return(NA_real_)
  scores <- vapply(starts, function(i) max(isi[i], isi[i + 1L]), numeric(1))
  observed <- max(isi[pair_start], isi[pair_start + 1L])
  # Mid-rank avoids declaring every pair in a homogeneous HFS to be the lower
  # tail merely because all pair scores are tied.
  (sum(scores < observed) + 0.5 * sum(scores == observed)) / length(scores)
}

stpd_nested_hfs_pair_is_local_minimum <- function(
    isi, valid, segment_start, segment_end, pair_start, radius) {
  low <- max(segment_start, pair_start - radius)
  high <- min(segment_end - 1L, pair_start + radius)
  starts <- if (high >= low) seq.int(low, high) else integer()
  starts <- starts[valid[starts] & valid[starts + 1L]]
  if (!length(starts)) return(FALSE)
  scores <- vapply(starts, function(i) max(isi[i], isi[i + 1L]), numeric(1))
  observed <- max(isi[pair_start], isi[pair_start + 1L])
  observed <= min(scores) && any(scores > observed)
}

stpd_nested_hfs_expand_direction <- function(
    isi, valid, segment_start, segment_end, seed_start, seed_end,
    direct_upper, candidate_upper, direction, settings) {
  stopifnot(direction %in% c(-1L, 1L))
  current <- if (direction < 0L) seed_start - 1L else seed_end + 1L
  lower <- seed_start
  upper <- seed_end
  steps <- 0L
  borrowed_run_n <- 0L
  borrowed_run_start <- NA_integer_
  borrowed_total <- 0L
  rollback <- FALSE
  hit_size_ceiling <- FALSE

  in_segment <- function(i) i >= segment_start && i <= segment_end
  while (in_segment(current) && steps < settings$max_expand_each_side) {
    if (!isTRUE(valid[current]) || !is.finite(isi[current]) ||
        isi[current] > candidate_upper) break

    borrowed <- isi[current] > direct_upper
    if (borrowed) {
      if (borrowed_run_n == 0L) borrowed_run_start <- current
      borrowed_run_n <- borrowed_run_n + 1L
      borrowed_total <- borrowed_total + 1L
      if (borrowed_run_n > settings$max_consecutive_borrowed ||
          borrowed_total > settings$bridge_max_count) {
        rollback <- TRUE
        # Exclude the entire borrowed run, including its first ISI.
        if (direction < 0L) lower <- borrowed_run_start + 1L
        else upper <- borrowed_run_start - 1L
        break
      }
    } else {
      borrowed_run_n <- 0L
      borrowed_run_start <- NA_integer_
    }

    proposed_lower <- if (direction < 0L) current else lower
    proposed_upper <- if (direction > 0L) current else upper
    proposed_spikes <- proposed_upper - proposed_lower + 2L
    if (proposed_spikes > settings$max_spikes) {
      hit_size_ceiling <- TRUE
      break
    }
    lower <- proposed_lower
    upper <- proposed_upper
    current <- current + direction
    steps <- steps + 1L
  }
  hit_expand_ceiling <- steps >= settings$max_expand_each_side &&
    in_segment(current) && isTRUE(valid[current]) &&
    is.finite(isi[current]) && isi[current] <= candidate_upper
  list(
    start = lower, end = upper, rollback = rollback,
    borrowed_n = borrowed_total, hit_size_ceiling = hit_size_ceiling,
    hit_expand_ceiling = hit_expand_ceiling
  )
}

stpd_nested_hfs_candidate_from_seed <- function(
    dat, isi, valid, segment_start, segment_end, seed_start,
    parent_id, settings) {
  seed_end <- seed_start + 1L
  seed_values <- isi[seed_start:seed_end]
  seed_max <- max(seed_values)
  background <- stpd_nested_hfs_side_background(
    isi, valid, segment_start, segment_end, seed_start, seed_end, settings
  )
  if (!all(is.finite(c(background$left, background$right)))) return(NULL)

  left_ratio <- background$left / seed_max
  right_ratio <- background$right / seed_max
  geom_ratio <- sqrt(max(0, left_ratio) * max(0, right_ratio))
  pair_rank <- stpd_nested_hfs_pair_rank(
    isi, valid, segment_start, segment_end, seed_start, settings
  )
  local_min <- stpd_nested_hfs_pair_is_local_minimum(
    isi, valid, segment_start, segment_end, seed_start,
    settings$local_min_radius
  )
  seed_pass <- local_min && is.finite(pair_rank) &&
    pair_rank <= settings$seed_pair_rank_max &&
    min(left_ratio, right_ratio) >= settings$seed_side_ratio_min &&
    geom_ratio >= settings$seed_geom_ratio_min
  if (!seed_pass) return(NULL)

  background_floor <- min(background$left, background$right)
  # The stricter final-flank gate is confirmatory evidence, not an automatic
  # acceptance condition.  A proposal-level seed that passes the frozen weak
  # contrast rule remains reviewable even when its weaker flank cannot support
  # expansion beyond the original two-ISI seed.
  direct_upper <- max(seed_max, min(
    seed_max * settings$direct_expand_factor,
    background_floor / settings$edge_side_ratio_min
  ))
  candidate_upper <- max(direct_upper, min(
    seed_max * settings$bridge_ratio_max,
    background_floor / settings$edge_side_ratio_min
  ))
  if (!all(is.finite(c(direct_upper, candidate_upper)))) return(NULL)

  left <- stpd_nested_hfs_expand_direction(
    isi, valid, segment_start, segment_end, seed_start, seed_end,
    direct_upper, candidate_upper, -1L, settings
  )
  right <- stpd_nested_hfs_expand_direction(
    isi, valid, segment_start, segment_end, left$start, seed_end,
    direct_upper, candidate_upper, 1L, settings
  )
  start <- left$start
  end <- right$end
  # A resource or spike-count ceiling is not a biological boundary.  Never
  # publish the truncated prefix as a Burst; fail closed until the complete
  # local event geometry can be adjudicated.
  if (isTRUE(left$hit_size_ceiling) || isTRUE(right$hit_size_ceiling) ||
      isTRUE(left$hit_expand_ceiling) || isTRUE(right$hit_expand_ceiling)) {
    return(NULL)
  }
  values <- isi[start:end]
  n_isi <- end - start + 1L
  n_spikes <- n_isi + 1L
  if (n_spikes < settings$min_spikes || n_spikes > settings$max_spikes) {
    return(NULL)
  }

  bridge <- values > direct_upper
  bridge_n <- sum(bridge)
  native_fraction <- mean(!bridge)
  core_values <- values[!bridge]
  if (!length(core_values)) return(NULL)
  core_median <- stats::median(core_values)
  bridge_ratio <- if (bridge_n) max(values[bridge]) / core_median else NA_real_
  bridge_pass <- bridge_n <= settings$bridge_max_count &&
    native_fraction >= settings$native_fraction_min &&
    (!bridge_n || (is.finite(bridge_ratio) &&
      bridge_ratio <= settings$bridge_ratio_max))
  if (!bridge_pass) return(NULL)

  final_background <- stpd_nested_hfs_side_background(
    isi, valid, segment_start, segment_end, start, end, settings
  )
  if (!all(is.finite(c(final_background$left, final_background$right)))) {
    return(NULL)
  }
  q90 <- as.numeric(stats::quantile(
    values, 0.90, names = FALSE, type = 7
  ))
  pre <- if (start > segment_start && isTRUE(valid[start - 1L])) {
    isi[start - 1L]
  } else NA_real_
  post <- if (end < segment_end && isTRUE(valid[end + 1L])) {
    isi[end + 1L]
  } else NA_real_
  if (!all(is.finite(c(q90, pre, post))) || q90 <= 0) return(NULL)

  robust_left_ratio <- final_background$left / q90
  robust_right_ratio <- final_background$right / q90
  robust_geom_ratio <- sqrt(
    max(0, robust_left_ratio) * max(0, robust_right_ratio)
  )
  immediate_left_ratio <- pre / q90
  immediate_right_ratio <- post / q90
  immediate_geom_ratio <- sqrt(
    max(0, immediate_left_ratio) * max(0, immediate_right_ratio)
  )
  robust_edge_pass <- min(robust_left_ratio, robust_right_ratio) >=
    settings$edge_side_ratio_min &&
    robust_geom_ratio >= settings$edge_geom_ratio_min
  immediate_edge_pass <- min(immediate_left_ratio, immediate_right_ratio) >=
    settings$edge_side_ratio_min &&
    immediate_geom_ratio >= settings$edge_geom_ratio_min
  evidence_strength <- if (robust_edge_pass) {
    "robust_two_sided_local_background"
  } else {
    "proposal_seed_only"
  }

  score <- log(max(min(robust_left_ratio, robust_right_ratio), 1)) +
    log(max(1 / max(pair_rank, .Machine$double.eps), 1)) -
    0.25 * bridge_n
  data.frame(
    candidate_id = paste(
      "nested_hfs_burst", parent_id, start, end, seed_start, sep = "_"
    ),
    candidate_layer = "nested_hfs_local_rate_contrast",
    candidate_source = "accepted_broad_hfs_local_context",
    final_label = "possible_burst", action = "demote_to_possible",
    decision_path = paste(
      "nested_hfs_two_isi_seed", evidence_strength,
      "frozen_relative_expansion", "rollback_to_intrusion_onset",
      "review_only_no_automatic_promotion",
      sep = ";"
    ),
    parent_hfs_candidate_id = as.character(parent_id),
    start_isi = as.integer(start), end_isi = as.integer(end),
    n_isi = as.integer(n_isi), n_spikes = as.integer(n_spikes),
    score = as.numeric(score), priority = 360,
    seed_start_isi = as.integer(seed_start),
    seed_end_isi = as.integer(seed_end),
    seed_pair_max_sec = seed_max, seed_pair_local_rank = pair_rank,
    seed_left_background_sec = background$left,
    seed_right_background_sec = background$right,
    seed_left_ratio = left_ratio, seed_right_ratio = right_ratio,
    seed_geom_ratio = geom_ratio, direct_upper_sec = direct_upper,
    candidate_upper_sec = candidate_upper, final_q90_sec = q90,
    final_left_background_sec = final_background$left,
    final_right_background_sec = final_background$right,
    final_left_ratio = robust_left_ratio,
    final_right_ratio = robust_right_ratio,
    final_geom_ratio = robust_geom_ratio,
    immediate_left_ratio = immediate_left_ratio,
    immediate_right_ratio = immediate_right_ratio,
    immediate_geom_ratio = immediate_geom_ratio,
    robust_edge_pass = robust_edge_pass,
    immediate_edge_pass = immediate_edge_pass,
    review_evidence_strength = evidence_strength,
    bridge_isi_count = as.integer(bridge_n),
    bridge_max_to_core_median_ratio = bridge_ratio,
    native_isi_fraction = native_fraction,
    rollback_left_to_intrusion_onset = isTRUE(left$rollback),
    rollback_right_to_intrusion_onset = isTRUE(right$rollback),
    nested_in_hfs = TRUE, absolute_pattern_threshold_used = FALSE,
    canonical_eligible = FALSE, review_only = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Detect locally accelerated Burst Events within accepted Broad-HFS support
#'
#' @param dat Spike-row data with an `ISI_sec` column.  ISI indices are row
#'   indices; row one normally has an undefined ISI.
#' @param hfs_candidates Accepted Broad-HFS intervals with inclusive
#'   `start_isi` and `end_isi` columns.
#' @param hard_boundaries Optional Pause/QC boundary intervals.  Rows with a
#'   false `hard_for_event` value are ignored.
#' @param settings Dimensionless/count-only nested-Burst settings.
#' @param min_isi_sec Existing artifact/QC floor; never used as a Burst gate.
#' @return A deterministic data frame of non-overlapping `possible_burst`
#'   review candidates.  No returned row is eligible for automatic promotion.
stpd_event_core_detect_nested_hfs_bursts <- function(
    dat, hfs_candidates, hard_boundaries = NULL, settings = list(),
    min_isi_sec = 0.001) {
  empty <- stpd_nested_hfs_empty_candidates()
  if (is.null(dat) || !is.data.frame(dat) ||
      !("ISI_sec" %in% names(dat)) || nrow(dat) < 4L ||
      is.null(hfs_candidates) || !is.data.frame(hfs_candidates) ||
      !nrow(hfs_candidates) ||
      !all(c("start_isi", "end_isi") %in% names(hfs_candidates))) {
    return(empty)
  }
  settings <- stpd_nested_hfs_settings(settings)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  n <- length(isi)
  artifact <- stpd_nested_hfs_is_artifact(isi, min_isi_sec)
  valid <- is.finite(isi) & isi > 0 & !artifact
  if (length(valid)) valid[1L] <- FALSE
  hard <- stpd_nested_hfs_hard_mask(n, hard_boundaries)
  valid <- valid & !hard

  hfs <- as.data.frame(hfs_candidates, stringsAsFactors = FALSE)
  hfs$.__source_order <- seq_len(nrow(hfs))
  hfs <- hfs[order(
    suppressWarnings(as.integer(hfs$start_isi)),
    suppressWarnings(as.integer(hfs$end_isi)), hfs$.__source_order,
    method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rows <- list()
  for (i in seq_len(nrow(hfs))) {
    parent_start <- suppressWarnings(as.integer(hfs$start_isi[i]))
    parent_end <- suppressWarnings(as.integer(hfs$end_isi[i]))
    if (!is.finite(parent_start) || !is.finite(parent_end) ||
        parent_start < 2L || parent_end > n || parent_end <= parent_start) next
    parent_id <- if ("candidate_id" %in% names(hfs) &&
        !is.na(hfs$candidate_id[i]) && nzchar(as.character(hfs$candidate_id[i]))) {
      as.character(hfs$candidate_id[i])
    } else {
      paste0("hfs_", parent_start, "_", parent_end)
    }
    parent_valid <- rep(FALSE, n)
    parent_valid[parent_start:parent_end] <- valid[parent_start:parent_end]
    segments <- stpd_nested_hfs_bool_runs(parent_valid)
    if (!nrow(segments)) next
    for (segment_i in seq_len(nrow(segments))) {
      segment_start <- segments$start_isi[segment_i]
      segment_end <- segments$end_isi[segment_i]
      minimum_span <- 2L + 2L * (
        settings$guard_isi_n + settings$min_background_isi_n
      )
      if (segment_end - segment_start + 1L < minimum_span) next
      for (seed_start in seq.int(segment_start, segment_end - 1L)) {
        if (!valid[seed_start] || !valid[seed_start + 1L]) next
        candidate <- stpd_nested_hfs_candidate_from_seed(
          dat, isi, valid, segment_start, segment_end, seed_start,
          parent_id, settings
        )
        if (!is.null(candidate)) rows[[length(rows) + 1L]] <- candidate
        if (length(rows) >= settings$max_candidates) break
      }
      if (length(rows) >= settings$max_candidates) break
    }
    if (length(rows) >= settings$max_candidates) break
  }
  if (!length(rows)) return(empty)
  out <- dplyr::bind_rows(rows)

  # Multiple overlapping two-ISI seeds commonly reconstruct the same physical
  # event.  Select one geometry deterministically without changing State input.
  order_index <- order(
    -out$score, out$start_isi, out$end_isi, out$seed_start_isi,
    out$parent_hfs_candidate_id, method = "radix", na.last = TRUE
  )
  out <- out[order_index, , drop = FALSE]
  keep <- logical(nrow(out))
  selected <- list()
  for (i in seq_len(nrow(out))) {
    overlap <- FALSE
    if (length(selected)) {
      overlap <- any(vapply(selected, function(interval) {
        out$start_isi[i] <= interval[2L] && out$end_isi[i] >= interval[1L]
      }, logical(1)))
    }
    if (!overlap) {
      keep[i] <- TRUE
      selected[[length(selected) + 1L]] <- c(out$start_isi[i], out$end_isi[i])
    }
  }
  out <- out[keep, , drop = FALSE]
  out <- out[order(out$start_isi, out$end_isi, out$candidate_id,
                   method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}
