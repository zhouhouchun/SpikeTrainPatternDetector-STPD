# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Parameter estimation from manual labels
# ============================================================

# Learn the active structure-first Burst gates from reviewed events.  The
# estimator is deliberately recall-preserving above the structural floor: a
# small calibration sample may relax a fixed contrast gate or enlarge the
# supported window, but a contrast-only anchor always requires four boundary
# spikes (three ISIs).  This prevents doublet/triplet fluctuations from being
# promoted solely by local flank separation.
stpd_manual_burst_structure_estimates <- function(burst_events,
                                                  defaults = list(),
                                                  min_events = 5L) {
  num1 <- function(x, fallback) {
    z <- suppressWarnings(as.numeric(x))[1]
    if (length(z) && is.finite(z)) z else fallback
  }
  int1 <- function(x, fallback) {
    z <- suppressWarnings(as.integer(x))[1]
    if (length(z) && is.finite(z)) z else as.integer(fallback)
  }
  q1 <- function(x, probability, fallback = NA_real_) {
    z <- suppressWarnings(as.numeric(x))
    z <- z[is.finite(z)]
    if (!length(z)) return(fallback)
    as.numeric(stats::quantile(z, probability, na.rm = TRUE, names = FALSE, type = 7))
  }

  default_min_isi <- max(3L, int1(defaults$structure_first_min_isi_count, 3L))
  default_max_isi <- max(default_min_isi, int1(defaults$structure_first_max_isi_count, 8L))
  default_contrast <- max(1, num1(defaults$structure_first_contrast_min, 3.0))
  default_geom <- max(1, num1(defaults$structure_first_geom_contrast_min, 3.0))
  default_possible <- max(1, num1(defaults$possible_contrast_min, 2.0))
  default_min_spikes <- max(2L, int1(defaults$classic_min_spikes, 3L))
  classic_max_spikes <- max(default_min_spikes, int1(defaults$classic_max_spikes, 10L))
  active_classic_isi_cap <- max(default_min_isi, classic_max_spikes - 1L)

  out <- list(
    applied = FALSE,
    evidence_n = 0L,
    min_isi_count = default_min_isi,
    max_isi_count = default_max_isi,
    contrast_min = default_contrast,
    geom_contrast_min = default_geom,
    possible_contrast_min = min(default_possible, default_contrast),
    min_spikes = default_min_spikes,
    min_duration_sec = 0,
    observed_n_isi_q05 = NA_real_,
    observed_n_isi_q95 = NA_real_,
    observed_contrast_q10 = NA_real_,
    observed_geom_contrast_q10 = NA_real_
  )
  if (!is.data.frame(burst_events) || nrow(burst_events) < as.integer(min_events)) return(out)

  n_spikes <- suppressWarnings(as.numeric(burst_events$n_spikes))
  n_spikes <- n_spikes[is.finite(n_spikes) & n_spikes >= 2]
  n_isi <- pmax(1, n_spikes - 1)
  edge_min <- suppressWarnings(as.numeric(burst_events$contrast_min_q))
  edge_geom <- suppressWarnings(as.numeric(burst_events$contrast_geom_q))
  duration <- suppressWarnings(as.numeric(burst_events$duration_sec))
  duration <- duration[is.finite(duration) & duration >= 0]

  support_q05 <- q1(n_isi, 0.05, default_min_isi)
  support_q95 <- q1(n_isi, 0.95, default_max_isi)
  contrast_q10 <- q1(edge_min, 0.10, default_contrast)
  geom_q10 <- q1(edge_geom, 0.10, default_geom)

  # Never make a small calibration sample stricter than the product defaults,
  # but do not learn below the four-spike structural floor.
  # The upper support window may expand to cover reviewed long events.
  learned_min_isi <- max(
    3L,
    min(default_min_isi, max(1L, as.integer(floor(support_q05))))
  )
  learned_max_isi <- max(default_max_isi, as.integer(ceiling(support_q95)))
  learned_max_isi <- min(100L, active_classic_isi_cap, max(learned_min_isi, learned_max_isi))
  learned_contrast <- min(default_contrast, max(1.05, contrast_q10))
  learned_geom <- min(default_geom, max(1.10, geom_q10))
  learned_possible <- min(
    default_possible,
    learned_contrast,
    max(1.05, learned_contrast * 0.85)
  )

  out$applied <- TRUE
  out$evidence_n <- as.integer(nrow(burst_events))
  out$min_isi_count <- learned_min_isi
  out$max_isi_count <- learned_max_isi
  out$contrast_min <- learned_contrast
  out$geom_contrast_min <- learned_geom
  out$possible_contrast_min <- learned_possible
  # Preserve the established biological minimum (normally three spikes).  The
  # sampled support distribution is used for the active structure window, not
  # to admit two-spike pseudo-bursts.
  out$min_spikes <- default_min_spikes
  out$min_duration_sec <- max(0, q1(duration, 0.02, 0))
  out$observed_n_isi_q05 <- support_q05
  out$observed_n_isi_q95 <- support_q95
  out$observed_contrast_q10 <- contrast_q10
  out$observed_geom_contrast_q10 <- geom_q10
  out
}

# Learn whether calibration contains a reproducible short-but-regular Tonic
# support class. Duration remains useful evidence against chance fragments, but
# a hard duration floor alone is not scale robust: a held-out Tonic episode can
# contain the calibrated number of regular ISIs while falling just below the
# smallest sampled duration. The alternative route is enabled only when the
# calibration episodes themselves support an informative ISI count. Candidate
# regularity is still decided by the existing frozen LV/MM gates downstream;
# this route does not learn a second set of regularity thresholds. No held-out
# labels or absolute time constants are used.
stpd_manual_tonic_short_route_estimates <- function(
    tonic_events, group = NULL, min_events = 10L, min_groups = 4L,
    structural_floor_spikes = 3L) {
  # These minima define the versioned calibration contract and cannot be
  # weakened by callers.
  requested_min_events <- suppressWarnings(as.integer(min_events))[1L]
  requested_min_groups <- suppressWarnings(as.integer(min_groups))[1L]
  if (!is.finite(requested_min_events)) requested_min_events <- 10L
  if (!is.finite(requested_min_groups)) requested_min_groups <- 4L
  min_events <- max(10L, requested_min_events)
  min_groups <- max(4L, requested_min_groups)
  q1 <- function(x, probability, fallback = NA_real_) {
    z <- suppressWarnings(as.numeric(x))
    z <- z[is.finite(z)]
    if (!length(z)) return(fallback)
    as.numeric(stats::quantile(
      z, probability, na.rm = TRUE, names = FALSE, type = 7
    ))
  }
  out <- list(
    enabled = FALSE,
    evidence_n = 0L,
    group_n = 0L,
    min_isi_count = NA_integer_,
    count_q10_full = NA_real_,
    logo_q10_min = NA_real_,
    logo_q10_max = NA_real_,
    structural_floor_spikes = as.integer(structural_floor_spikes),
    mode = "calibration_group_logo_q10_v1",
    status = "insufficient_calibration_evidence"
  )
  if (!is.data.frame(tonic_events) || nrow(tonic_events) == 0L) return(out)

  n_spikes <- suppressWarnings(as.numeric(tonic_events$n_spikes))
  if (is.null(group)) {
    group <- if ("train" %in% names(tonic_events)) {
      as.character(tonic_events$train)
    } else {
      as.character(seq_len(nrow(tonic_events)))
    }
  }
  group <- as.character(group)
  if (length(group) != length(n_spikes)) {
    stop("Tonic short-support groups must align with calibration episodes.",
         call. = FALSE)
  }
  keep <- is.finite(n_spikes) & n_spikes >= 2L & !is.na(group) & nzchar(group)
  n_isi <- n_spikes[keep] - 1
  group <- group[keep]
  out$evidence_n <- as.integer(length(n_isi))
  out$group_n <- as.integer(length(unique(group)))
  if (length(n_isi) < as.integer(min_events) ||
      out$group_n < as.integer(min_groups)) return(out)

  count_q10 <- q1(n_isi, 0.10)
  group_q10 <- vapply(sort(unique(group), method = "radix"), function(g) {
    q1(n_isi[group != g], 0.10)
  }, numeric(1))
  group_q10 <- group_q10[is.finite(group_q10)]
  logo_q10_min <- if (length(group_q10)) min(group_q10) else NA_real_
  logo_q10_max <- if (length(group_q10)) max(group_q10) else NA_real_
  min_isi_count <- if (is.finite(logo_q10_min)) {
    as.integer(floor(logo_q10_min))
  } else {
    NA_integer_
  }

  # The route is informative only when its conservative leave-one-group-out
  # support exceeds the already established three-spike structural floor. This
  # is a count/topology condition, not an absolute ISI-duration threshold.
  informative_count <- is.finite(min_isi_count) &&
    min_isi_count > (as.integer(structural_floor_spikes) - 1L)
  enabled <- informative_count

  out$enabled <- isTRUE(enabled)
  out$min_isi_count <- min_isi_count
  out$count_q10_full <- count_q10
  out$logo_q10_min <- logo_q10_min
  out$logo_q10_max <- logo_q10_max
  out$status <- if (isTRUE(enabled)) {
    "enabled__calibration_frozen_short_regular_support"
  } else if (!informative_count) {
    "disabled__calibration_support_not_above_structural_floor"
  } else {
    "disabled__insufficient_group_evidence"
  }
  out
}

# Learn only the conditional Tonic MM ceiling from calibration episodes.
# Evidence is summarized per episode (never by pooling ISIs), restricted by
# prespecified strong-regularity CV/LV guards, and checked with leave-one-group-
# out q95 estimates. Held-out labels are not an input to this function.
stpd_manual_tonic_mm_relaxation_estimates <- function(
    tonic_events, group = NULL, min_events = 10L, min_groups = 4L,
    relax_cv_max = 0.30, relax_lv_max = 0.15,
    default_relaxed_max = 1.40, maximum_relaxed_max = 1.50,
    safety_multiplier = 1.02) {
  q1 <- function(x, probability, fallback = NA_real_) {
    z <- suppressWarnings(as.numeric(x))
    z <- z[is.finite(z)]
    if (!length(z)) return(fallback)
    as.numeric(stats::quantile(
      z, probability, na.rm = TRUE, names = FALSE, type = 7
    ))
  }
  out <- list(
    enabled = FALSE,
    relaxed_max = as.numeric(default_relaxed_max),
    evidence_n = 0L,
    group_n = 0L,
    q95_full = NA_real_,
    logo_q95_min = NA_real_,
    logo_q95_median = NA_real_,
    logo_q95_max = NA_real_,
    relax_cv_max = as.numeric(relax_cv_max),
    relax_lv_max = as.numeric(relax_lv_max),
    default_relaxed_max = as.numeric(default_relaxed_max),
    maximum_relaxed_max = as.numeric(maximum_relaxed_max),
    safety_multiplier = as.numeric(safety_multiplier),
    mode = "calibration_group_logo_q95_v1",
    status = "disabled__insufficient_calibration_evidence"
  )
  if (!is.data.frame(tonic_events) || nrow(tonic_events) == 0L ||
      !all(c("CV", "LV", "MM") %in% names(tonic_events))) {
    return(out)
  }
  if (is.null(group)) {
    group <- if ("group" %in% names(tonic_events)) {
      tonic_events$group
    } else if ("train" %in% names(tonic_events)) {
      tonic_events$train
    } else {
      seq_len(nrow(tonic_events))
    }
  }
  group <- as.character(group)
  if (length(group) != nrow(tonic_events)) {
    stop("Tonic MM-relaxation groups must align with calibration episodes.",
         call. = FALSE)
  }
  cv <- suppressWarnings(as.numeric(tonic_events$CV))
  lv <- suppressWarnings(as.numeric(tonic_events$LV))
  mm <- suppressWarnings(as.numeric(tonic_events$MM))
  keep <- is.finite(cv) & cv <= relax_cv_max &
    is.finite(lv) & lv <= relax_lv_max &
    is.finite(mm) & mm > 0 &
    !is.na(group) & nzchar(group)
  mm <- mm[keep]
  group <- group[keep]
  out$evidence_n <- as.integer(length(mm))
  out$group_n <- as.integer(length(unique(group)))
  if (length(mm) < max(10L, as.integer(min_events)) ||
      out$group_n < max(4L, as.integer(min_groups))) {
    return(out)
  }

  q95_full <- q1(mm, 0.95)
  logo <- vapply(sort(unique(group), method = "radix"), function(g) {
    q1(mm[group != g], 0.95)
  }, numeric(1))
  logo <- logo[is.finite(logo)]
  if (!is.finite(q95_full) || length(logo) < out$group_n) {
    out$status <- "disabled__invalid_logo_evidence"
    return(out)
  }
  logo_median <- stats::median(logo)
  raw <- max(
    default_relaxed_max,
    safety_multiplier * q95_full,
    safety_multiplier * logo_median,
    na.rm = TRUE
  )
  frozen <- min(
    maximum_relaxed_max,
    ceiling((raw - 1e-12) * 100) / 100
  )
  frozen <- max(default_relaxed_max, frozen)

  out$enabled <- TRUE
  out$relaxed_max <- as.numeric(frozen)
  out$q95_full <- q95_full
  out$logo_q95_min <- min(logo)
  out$logo_q95_median <- logo_median
  out$logo_q95_max <- max(logo)
  out$status <- "enabled__calibration_frozen_group_logo_q95"
  out
}

# Fail-closed contract for the conditional Tonic MM ceiling.  The historical
# 1.40 ceiling is a prespecified guardrail and needs no calibration payload.
# Any higher ceiling is accepted only when the complete episode-level,
# group-LOGO calibration evidence reproduces the frozen value exactly.  This
# keeps a stale YAML value (or an isolated expert edit) from silently turning a
# calibration candidate into a global detector default.
stpd_tonic_mm_relaxation_contract <- function(x) {
  x <- x %||% list()
  canonical <- ((x$spiketrainpattern %||% list())$tonic %||% list())
  legacy <- x$tonic %||% list()
  direct <- if (length(canonical) || length(legacy)) list() else x
  contract <- stpd_first_nonnull(
    canonical$mm_relaxation_contract,
    legacy$mm_relaxation_contract,
    direct$tonic_mm_relaxation_contract,
    direct$mm_relaxation_contract,
    list()
  )
  if (!is.list(contract)) contract <- list()
  pick_active <- function(canonical_name, legacy_name, fallback = NULL) {
    stpd_first_nonnull(
      canonical[[canonical_name]], legacy[[legacy_name]],
      direct[[paste0("tonic_", legacy_name)]], direct[[legacy_name]],
      fallback
    )
  }
  num1 <- function(value, fallback = NA_real_) {
    z <- suppressWarnings(as.numeric(value))[1L]
    if (length(z) && is.finite(z)) z else fallback
  }
  int1 <- function(value, fallback = NA_integer_) {
    z <- suppressWarnings(as.integer(value))[1L]
    if (length(z) && is.finite(z)) z else fallback
  }
  same_num <- function(a, b, tolerance = 1e-8) {
    is.finite(a) && is.finite(b) && abs(a - b) <= tolerance
  }

  default_relaxed_max <- 1.40
  maximum_relaxed_max <- 1.50
  maximum_relax_cv <- 0.30
  maximum_relax_lv <- 0.15
  prespecified_multiplier <- 1.02
  configured_relaxed_max <- num1(
    pick_active("mm_relaxed_max", "tonic_mm_relaxed_max",
                default_relaxed_max),
    default_relaxed_max
  )
  configured_relax_cv <- num1(
    pick_active("mm_relax_cv_max", "tonic_mm_relax_cv_max",
                maximum_relax_cv),
    maximum_relax_cv
  )
  configured_relax_lv <- num1(
    pick_active("mm_relax_lv_max", "tonic_mm_relax_lv_max",
                maximum_relax_lv),
    maximum_relax_lv
  )
  required <- configured_relaxed_max >
    default_relaxed_max + sqrt(.Machine$double.eps)

  enabled <- isTRUE(contract$enabled)
  evidence_n <- int1(contract$evidence_n, 0L)
  group_n <- int1(contract$group_n, 0L)
  q95_full <- num1(contract$q95_full)
  logo_min <- num1(contract$logo_q95_min)
  logo_median <- num1(contract$logo_q95_median)
  logo_max <- num1(contract$logo_q95_max)
  contract_relaxed_max <- num1(contract$relaxed_max)
  contract_relax_cv <- num1(contract$relax_cv_max)
  contract_relax_lv <- num1(contract$relax_lv_max)
  contract_default <- num1(contract$default_relaxed_max)
  contract_maximum <- num1(contract$maximum_relaxed_max)
  multiplier <- num1(contract$safety_multiplier)
  mode <- as.character(
    contract$mode %||% direct$tonic_mm_relaxation_mode %||%
      "prespecified_default_v1"
  )[1L]
  status <- as.character(
    contract$status %||% direct$tonic_mm_relaxation_status %||%
      "default_guardrail"
  )[1L]

  reasons <- character()
  recomputed_relaxed_max <- default_relaxed_max
  if (required) {
    if (!enabled) reasons <- c(reasons, "calibration_contract_not_enabled")
    if (!identical(mode, "calibration_group_logo_q95_v1")) {
      reasons <- c(reasons, "invalid_calibration_mode")
    }
    if (!identical(
      status, "enabled__calibration_frozen_group_logo_q95"
    )) {
      reasons <- c(reasons, "invalid_calibration_status")
    }
    if (!is.finite(evidence_n) || evidence_n < 10L) {
      reasons <- c(reasons, "fewer_than_10_calibration_episodes")
    }
    if (!is.finite(group_n) || group_n < 4L) {
      reasons <- c(reasons, "fewer_than_4_calibration_groups")
    }
    if (is.finite(group_n) && is.finite(evidence_n) && group_n > evidence_n) {
      reasons <- c(reasons, "calibration_groups_exceed_episode_count")
    }
    if (!all(is.finite(c(q95_full, logo_min, logo_median, logo_max)))) {
      reasons <- c(reasons, "incomplete_q95_calibration_evidence")
    } else if (logo_min > logo_median + 1e-8 ||
               logo_median > logo_max + 1e-8) {
      reasons <- c(reasons, "invalid_logo_q95_order")
    }
    if (!same_num(contract_default, default_relaxed_max)) {
      reasons <- c(reasons, "invalid_default_relaxed_max")
    }
    if (!same_num(contract_maximum, maximum_relaxed_max)) {
      reasons <- c(reasons, "invalid_maximum_relaxed_max")
    }
    if (!same_num(multiplier, prespecified_multiplier)) {
      reasons <- c(reasons, "invalid_safety_multiplier")
    }
    if (!same_num(contract_relax_cv, configured_relax_cv) ||
        configured_relax_cv > maximum_relax_cv + 1e-8) {
      reasons <- c(reasons, "invalid_or_mismatched_cv_guard")
    }
    if (!same_num(contract_relax_lv, configured_relax_lv) ||
        configured_relax_lv > maximum_relax_lv + 1e-8) {
      reasons <- c(reasons, "invalid_or_mismatched_lv_guard")
    }
    if (all(is.finite(c(q95_full, logo_median, contract_default,
                        contract_maximum, multiplier)))) {
      raw <- max(
        contract_default,
        multiplier * q95_full,
        multiplier * logo_median,
        na.rm = TRUE
      )
      recomputed_relaxed_max <- min(
        contract_maximum,
        ceiling((raw - 1e-12) * 100) / 100
      )
      recomputed_relaxed_max <- max(contract_default,
                                    recomputed_relaxed_max)
      if (!same_num(contract_relaxed_max, recomputed_relaxed_max)) {
        reasons <- c(reasons, "contract_value_differs_from_recomputed_value")
      }
      if (!same_num(configured_relaxed_max, recomputed_relaxed_max)) {
        reasons <- c(reasons, "active_value_differs_from_calibration_contract")
      }
    } else {
      reasons <- c(reasons, "cannot_recompute_frozen_value")
    }
    if (configured_relaxed_max > maximum_relaxed_max + 1e-8) {
      reasons <- c(reasons, "active_value_above_prespecified_cap")
    }
  }

  valid <- !required || length(reasons) == 0L
  list(
    requested = required,
    required = required,
    valid = valid,
    reason = if (!required) {
      "prespecified_default_guardrail"
    } else if (length(reasons)) {
      paste(unique(reasons), collapse = ";")
    } else {
      "valid_calibration_frozen_contract"
    },
    configured_relaxed_max = configured_relaxed_max,
    runtime_relaxed_max = if (valid) {
      min(configured_relaxed_max, maximum_relaxed_max)
    } else {
      default_relaxed_max
    },
    fallback_applied = required && !valid,
    evidence_n = evidence_n,
    group_n = group_n,
    q95_full = q95_full,
    logo_q95_min = logo_min,
    logo_q95_median = logo_median,
    logo_q95_max = logo_max,
    recomputed_relaxed_max = recomputed_relaxed_max,
    relax_cv_max = min(configured_relax_cv, maximum_relax_cv),
    relax_lv_max = min(configured_relax_lv, maximum_relax_lv),
    mode = mode,
    status = if (required && !valid) {
      "fallback__invalid_mm_relaxation_contract"
    } else {
      status
    }
  )
}

# Fail-closed contract for the calibration-frozen short Tonic route.  The
# function accepts either a complete parameter object, a legacy `tonic` list,
# a canonical `spiketrainpattern$tonic` list, or the flattened runtime view
# produced by `stpd_event_grammar_params()`.  Enabling a Boolean flag alone is
# never sufficient: the group-level calibration evidence and its conservative
# LOGO support bound must be internally consistent.
stpd_tonic_short_route_contract <- function(x) {
  x <- x %||% list()
  canonical <- ((x$spiketrainpattern %||% list())$tonic %||% list())
  legacy <- x$tonic %||% list()
  direct <- if (length(canonical) || length(legacy)) list() else x
  pick <- function(name, fallback = NULL) {
    stpd_first_nonnull(
      canonical[[name]], legacy[[name]], direct[[name]],
      direct[[paste0("tonic_", name)]], fallback
    )
  }
  num1 <- function(value, fallback = NA_real_) {
    z <- suppressWarnings(as.numeric(value))[1L]
    if (length(z) && is.finite(z)) z else fallback
  }
  int1 <- function(value, fallback = NA_integer_) {
    z <- suppressWarnings(as.integer(value))[1L]
    if (length(z) && is.finite(z)) z else fallback
  }

  requested <- isTRUE(pick("short_regular_route_enabled", FALSE))
  evidence_n <- int1(pick("short_regular_evidence_n", 0L), 0L)
  group_n <- int1(pick("short_regular_group_n", 0L), 0L)
  min_count <- int1(pick("short_regular_min_isi_count", NA_integer_))
  count_q10 <- num1(pick("short_regular_count_q10_full", NA_real_))
  logo_min <- num1(pick("short_regular_logo_q10_min", NA_real_))
  logo_max <- num1(pick("short_regular_logo_q10_max", NA_real_))
  floor_spikes <- int1(
    pick("short_regular_structural_floor_spikes", 3L), 3L
  )
  mode <- as.character(
    pick("short_regular_mode", "not_calibrated")
  )[1L]
  status <- as.character(
    pick("short_regular_status", "not_calibrated")
  )[1L]

  reasons <- character()
  if (requested) {
    if (!identical(mode, "calibration_group_logo_q10_v1")) {
      reasons <- c(reasons, "invalid_calibration_mode")
    }
    if (!identical(
      status, "enabled__calibration_frozen_short_regular_support"
    )) {
      reasons <- c(reasons, "invalid_calibration_status")
    }
    if (!is.finite(evidence_n) || evidence_n < 10L) {
      reasons <- c(reasons, "fewer_than_10_calibration_episodes")
    }
    if (!is.finite(group_n) || group_n < 4L) {
      reasons <- c(reasons, "fewer_than_4_calibration_groups")
    }
    if (is.finite(group_n) && is.finite(evidence_n) &&
        group_n > evidence_n) {
      reasons <- c(reasons, "calibration_groups_exceed_episode_count")
    }
    if (!is.finite(floor_spikes) || floor_spikes < 3L) {
      reasons <- c(reasons, "invalid_structural_floor")
    }
    if (!is.finite(min_count) ||
        min_count <= (floor_spikes - 1L)) {
      reasons <- c(reasons, "support_not_above_structural_floor")
    }
    if (!is.finite(count_q10) || !is.finite(min_count) ||
        count_q10 + sqrt(.Machine$double.eps) < min_count) {
      reasons <- c(reasons, "full_q10_below_frozen_support")
    }
    if (!is.finite(logo_min) || !is.finite(min_count) ||
        floor(logo_min + sqrt(.Machine$double.eps)) != min_count) {
      reasons <- c(reasons, "frozen_support_differs_from_logo_q10")
    }
    if (!is.finite(logo_max) || !is.finite(logo_min) ||
        logo_max + sqrt(.Machine$double.eps) < logo_min) {
      reasons <- c(reasons, "invalid_logo_q10_range")
    }
  }

  list(
    requested = requested,
    valid = requested && length(reasons) == 0L,
    reason = if (!requested) "route_disabled" else if (length(reasons)) {
      paste(unique(reasons), collapse = ";")
    } else {
      "valid_calibration_frozen_contract"
    },
    evidence_n = evidence_n,
    group_n = group_n,
    min_isi_count = min_count,
    count_q10_full = count_q10,
    logo_q10_min = logo_min,
    logo_q10_max = logo_max,
    structural_floor_spikes = floor_spikes,
    mode = mode,
    status = status
  )
}

estimate_params_from_manual_pool <- function(trains_pool,
                                             dataset_map,
                                             min_isi_sec = 0.001,
                                             logisi_mcv_sec = 0.1) {
  base <- default_params_sec()
  dd <- derive_interval_tables(
    trains_pool,
    source = "manual",
    auto_others = FALSE,
    dataset_map = dataset_map,
    min_isi_sec = min_isi_sec,
    contrast_q = base$burst$contrast_q,
    context_k = base$burst$context_k
  )
  ev <- dd$events
  ints <- dd$intervals
  
  burst_intra <- ints$intra_burst$value_sec %||% numeric(0)
  tonic_intra <- ints$intra_tonic$value_sec %||% numeric(0)
  pause_isi <- ints$pause_isi$value_sec %||% numeric(0)
  burst_pre <- ints$pre_burst$value_sec %||% numeric(0)
  burst_post <- ints$after_burst$value_sec %||% numeric(0)
  tonic_pre <- ints$pre_tonic$value_sec %||% numeric(0)
  tonic_post <- ints$after_tonic$value_sec %||% numeric(0)
  
  burst_ev <- ev %>% filter(pattern == "burst")
  tonic_ev <- ev %>% filter(pattern == "tonic")
  pause_ev <- ev %>% filter(pattern == "pause")
  
  ML_r <- vapply(trains_pool, function(dat) {
    estimate_mean_isi_threshold_train(dat$ISI_sec, min_isi_sec = min_isi_sec)
  }, numeric(1))
  ML_r <- ML_r[is.finite(ML_r)]
  
  LOG_details <- lapply(trains_pool, function(dat) {
    estimate_logisi_threshold_train_result(dat$ISI_sec, min_isi_sec = min_isi_sec, mcv_sec = logisi_mcv_sec)
  })
  LOG_all <- vapply(LOG_details, function(x) as.numeric(x$threshold_sec %||% NA_real_), numeric(1))
  LOG_unresolved_n <- sum(!vapply(LOG_details, function(x) isTRUE(x$accepted), logical(1)), na.rm = TRUE)
  LOG_r <- LOG_all[is.finite(LOG_all)]
  
  T_B_manual <- safe_q(burst_intra, 0.95, default = NA_real_)
  T_B_bridge0 <- safe_q(burst_intra, 0.98, default = NA_real_)
  # A Burst bridge is internal support.  Pre/post-Burst ISIs describe the
  # boundary contrast and must not inflate the internal bridge band.  Mixing
  # the two previously allowed a genuine inter-event gap to become the active
  # bridge threshold (for example 52 ms in C1_T05 although the calibrated
  # Burst-left support ended near 18 ms).
  T_B_bridge <- T_B_bridge0
  if (!is.finite(T_B_bridge)) T_B_bridge <- T_B_manual
  edge_fallback <- if (is.finite(T_B_bridge)) T_B_bridge else if (is.finite(T_B_manual)) T_B_manual else 0.025
  T_B_edge_pre <- safe_q(burst_pre, 0.10, default = edge_fallback)
  T_B_edge_post <- safe_q(burst_post, 0.10, default = edge_fallback)
  
  T_B_MI <- if (length(ML_r) > 0) median(ML_r, na.rm = TRUE) else NA_real_
  T_B_log <- if (length(LOG_r) > 0) median(LOG_r, na.rm = TRUE) else NA_real_
  seed_candidates <- finite_num(c(T_B_manual, T_B_MI, T_B_log))
  T_B_seed <- if (length(seed_candidates) > 0) median(seed_candidates) else 0.020
  
  # High-recall protections: manual examples are often too typical.
  # Keep seed reasonably permissive, and use context score to recover precision.
  if (is.finite(T_B_seed) && is.finite(T_B_manual)) {
    T_B_seed <- max(T_B_seed, safe_q(burst_intra, 0.75, default = T_B_seed))
  }
  
  burst_ctx_min <- finite_num(burst_ev$contrast_min_ctx_q)
  burst_ctx_geom <- finite_num(burst_ev$contrast_geom_ctx_q)
  
  c_possible <- safe_q(burst_ctx_min, 0.05, default = base$burst$contrast_min_possible)
  c_high <- safe_q(burst_ctx_min, 0.10, default = base$burst$contrast_min_high)
  g_possible <- safe_q(burst_ctx_geom, 0.05, default = base$burst$contrast_geom_possible)
  g_high <- safe_q(burst_ctx_geom, 0.10, default = base$burst$contrast_geom_high)
  
  c_possible <- clamp(c_possible, 1.20, 1.60)
  c_high <- clamp(c_high, 1.45, 1.90)
  g_possible <- clamp(g_possible, 1.25, 1.75)
  g_high <- clamp(g_high, 1.45, 2.10)
  
  local_comp <- safe_q(burst_ctx_min, 0.20, default = base$burst$local_compression_min)
  local_comp <- clamp(local_comp, 1.25, 1.60)
  
  G_B_min <- 3L
  D_B_min <- safe_q(burst_ev$duration_sec, 0.02, default = 0)
  
  pooled_valid_isi <- unlist(lapply(trains_pool, function(dat) {
    x <- suppressWarnings(as.numeric(dat$ISI_sec %||% numeric()))
    x[is.finite(x) & x >= min_isi_sec]
  }), use.names = FALSE)
  pause_fallback <- max(c(
    T_B_bridge, T_B_seed,
    safe_q(pooled_valid_isi, 0.95, default = NA_real_)
  ), na.rm = TRUE)
  if (!is.finite(pause_fallback)) pause_fallback <- min_isi_sec * 2
  T_P_strong <- safe_q(pause_isi, 0.50, default = pause_fallback)
  T_P_seed <- safe_q(pause_isi, 0.10, default = pause_fallback)
  D_P_min <- safe_q(pause_ev$duration_sec, 0.05, default = 0)
  G_P_min <- if (nrow(pause_ev) > 0) max(2L, floor(as.numeric(stats::quantile(pause_ev$n_spikes, 0.05, na.rm = TRUE)))) else 2L
  
  T_T_min <- safe_q(tonic_intra, 0.05, default = 0.020)
  T_T_max <- safe_q(tonic_intra, 0.95, default = 0.060)
  T_LV_core <- safe_q(tonic_ev$LV, 0.95, default = 0.5)
  T_LV_pre <- safe_q(tonic_ev$Pre_LV, 0.95, default = T_LV_core)
  T_LV_post <- safe_q(tonic_ev$After_LV, 0.95, default = T_LV_core)
  G_T_min <- if (nrow(tonic_ev) > 0) max(3L, floor(as.numeric(stats::quantile(tonic_ev$n_spikes, 0.05, na.rm = TRUE)))) else 5L
  D_T_min <- safe_q(tonic_ev$duration_sec, 0.05, default = 0)
  tonic_short_route <- stpd_manual_tonic_short_route_estimates(tonic_ev)
  finite_or_zero <- function(x) {
    z <- suppressWarnings(as.numeric(x))[1L]
    if (length(z) && is.finite(z)) z else 0
  }
  tonic_short_serialized <- list(
    min_isi_count = as.integer(finite_or_zero(tonic_short_route$min_isi_count)),
    evidence_n = as.integer(finite_or_zero(tonic_short_route$evidence_n)),
    group_n = as.integer(finite_or_zero(tonic_short_route$group_n)),
    count_q10_full = finite_or_zero(tonic_short_route$count_q10_full),
    logo_q10_min = finite_or_zero(tonic_short_route$logo_q10_min),
    logo_q10_max = finite_or_zero(tonic_short_route$logo_q10_max),
    structural_floor_spikes = as.integer(finite_or_zero(
      tonic_short_route$structural_floor_spikes
    ))
  )
  
  base$burst$T_manual <- T_B_manual
  base$burst$T_MI <- T_B_MI
  base$burst$T_log <- T_B_log
  base$burst$T_log_method <- "pasquale_logisi"
  base$burst$T_log_status <- if (is.finite(T_B_log)) "resolved" else "threshold_unresolved"
  base$burst$T_log_resolved_n <- as.integer(length(LOG_r))
  base$burst$T_log_unresolved_n <- as.integer(LOG_unresolved_n)
  base$burst$T_seed <- T_B_seed
  base$burst$T_bridge <- T_B_bridge
  base$burst$T_edge_pre <- T_B_edge_pre
  base$burst$T_edge_post <- T_B_edge_post
  base$burst$G_min <- as.integer(G_B_min)
  base$burst$D_min <- D_B_min
  base$burst$local_compression_min <- local_comp
  base$burst$contrast_min_possible <- c_possible
  base$burst$contrast_min_high <- c_high
  base$burst$contrast_geom_possible <- g_possible
  base$burst$contrast_geom_high <- g_high
  
  # seed-bridge seed-bridge estimates. Use immediate edge contrast, not fixed-k context median,
  # as the primary burst boundary reference. Raw intra-burst ISI remains only a weak
  # seed reference so low-frequency bursts do not corrupt high-frequency seed logic.
  burst_edge_min <- finite_num(burst_ev$contrast_min_q)
  burst_edge_geom <- finite_num(burst_ev$contrast_geom_q)
  burst_core_q <- finite_num(burst_ev$core_q_ISI_sec)
  burst_core_q_pct <- numeric(0)
  if (nrow(burst_ev) > 0) {
    burst_core_q_pct <- suppressWarnings(vapply(seq_len(nrow(burst_ev)), function(ii) {
      tr <- as.character(burst_ev$train[ii])
      if (!(tr %in% names(trains_pool))) return(NA_real_)
      isi_percentile_scalar(burst_ev$core_q_ISI_sec[ii], trains_pool[[tr]]$ISI_sec, min_isi_sec = min_isi_sec)
    }, numeric(1)))
    burst_core_q_pct <- finite_num(burst_core_q_pct)
  }
  burst_mm <- finite_num(burst_ev$MM)
  base$burst$use_seed_bridge_model <- TRUE
  base$burst$use_structure_candidates <- TRUE
  seed_q_est <- safe_q(burst_core_q, 0.90, default = if (is.finite(T_B_seed)) T_B_seed else 0.035)
  base$burst$seed_q_max <- max(min_isi_sec, seed_q_est)
  base$burst$structure_core_q_max <- max(
    base$burst$seed_q_max, seed_q_est * 1.35
  )
  base$burst$structure_core_q_loosen <- 1.25
  base$burst$adaptive_apply_core_pct_to_structure <- FALSE
  base$burst$adaptive_core_pct_seed_max <- clamp(safe_q(burst_core_q_pct, 0.90, default = base$burst$adaptive_core_pct_seed_max %||% 25), 5, 45)
  base$burst$adaptive_core_pct_possible_max <- clamp(safe_q(burst_core_q_pct, 0.98, default = max(base$burst$adaptive_core_pct_seed_max + 10, 35)), base$burst$adaptive_core_pct_seed_max, 60)
  base$burst$structure_edge_min <- clamp(safe_q(burst_edge_min, 0.05, default = 1.25), 1.05, 1.70)
  base$burst$structure_edge_geom_min <- clamp(safe_q(burst_edge_geom, 0.05, default = 1.35), 1.10, 1.90)
  base$burst$seed_q_loosen <- 1.35
  base$burst$seed_internal_bridge_split_ratio <- clamp(safe_q(burst_mm, 0.50, default = 1.80), 1.35, 2.50)
  base$burst$seed_edge_contrast_min <- clamp(safe_q(burst_edge_min, 0.02, default = 1.05), 1.00, 1.35)
  base$burst$bridge_ratio_max <- clamp(safe_q(burst_mm, 0.85, default = 3.50) + 0.50, 2.00, 6.00)
  base$burst$bridge_ratio_possible_max <- max(base$burst$bridge_ratio_max + 1.00, 4.00)
  bridge_raw_est <- safe_q(
    burst_intra, 0.99, default = base$burst$T_bridge
  )
  if (!is.finite(bridge_raw_est)) bridge_raw_est <- base$burst$seed_q_max
  base$burst$bridge_raw_max <- max(
    base$burst$seed_q_max, bridge_raw_est * 1.50
  )
  base$burst$final_edge_contrast_min <- clamp(safe_q(burst_edge_min, 0.10, default = 1.45), 1.20, 1.90)
  base$burst$final_edge_contrast_geom_min <- clamp(safe_q(burst_edge_geom, 0.10, default = 1.50), 1.25, 2.20)
  base$burst$bridge_merged_edge_min <- clamp(base$burst$final_edge_contrast_min * 0.85, 1.10, 1.60)
  base$burst$bridge_merged_edge_geom_min <- clamp(base$burst$final_edge_contrast_geom_min * 0.85, 1.15, 1.80)
  base$burst$seed_min_isi_n <- 2L
  burst_structure <- stpd_manual_burst_structure_estimates(
    burst_ev,
    defaults = (base$spiketrainpattern %||% list())$burst %||% list()
  )
  base$burst$G_min <- as.integer(burst_structure$min_spikes)
  base$burst$D_min <- as.numeric(burst_structure$min_duration_sec)

  # Materialize the estimates into both namespaces consumed by the active
  # detector.  The historical estimator previously populated only legacy
  # diagnostic fields, leaving structure-first detection at fixed defaults.
  base$event_core <- base$event_core %||% list()
  base$event_core$burst_contrast_min <- as.numeric(burst_structure$contrast_min)
  base$event_core$possible_burst_contrast_min <- as.numeric(burst_structure$possible_contrast_min)
  base$event_core$min_seed_isi_count <- as.integer(burst_structure$min_isi_count)
  base$spiketrainpattern <- base$spiketrainpattern %||% list()
  base$spiketrainpattern$burst <- base$spiketrainpattern$burst %||% list()
  base$spiketrainpattern$burst$contrast_min <- as.numeric(burst_structure$contrast_min)
  base$spiketrainpattern$burst$possible_contrast_min <- as.numeric(burst_structure$possible_contrast_min)
  base$spiketrainpattern$burst$min_seed_isi_count <- as.integer(burst_structure$min_isi_count)
  base$spiketrainpattern$burst$classic_min_spikes <- as.integer(burst_structure$min_spikes)
  base$spiketrainpattern$burst$structure_first_min_isi_count <- as.integer(burst_structure$min_isi_count)
  base$spiketrainpattern$burst$structure_first_max_isi_count <- as.integer(burst_structure$max_isi_count)
  base$spiketrainpattern$burst$structure_first_contrast_min <- as.numeric(burst_structure$contrast_min)
  base$spiketrainpattern$burst$structure_first_geom_contrast_min <- as.numeric(burst_structure$geom_contrast_min)
  base$metadata <- base$metadata %||% list()
  base$metadata$manual_parameter_learning <- base$metadata$manual_parameter_learning %||% list()
  base$metadata$manual_parameter_learning$burst_structure <- burst_structure
  
  base$tonic$T_min <- T_T_min
  base$tonic$T_max <- T_T_max
  base$tonic$LV_core <- T_LV_core
  base$tonic$LV_pre <- T_LV_pre
  base$tonic$LV_post <- T_LV_post
  base$tonic$G_min <- as.integer(G_T_min)
  base$tonic$D_min <- D_T_min
  base$tonic$short_regular_route_enabled <-
    isTRUE(tonic_short_route$enabled)
  base$tonic$short_regular_min_isi_count <-
    tonic_short_serialized$min_isi_count
  base$tonic$short_regular_evidence_n <-
    tonic_short_serialized$evidence_n
  base$tonic$short_regular_group_n <- tonic_short_serialized$group_n
  base$tonic$short_regular_count_q10_full <-
    tonic_short_serialized$count_q10_full
  base$tonic$short_regular_logo_q10_min <-
    tonic_short_serialized$logo_q10_min
  base$tonic$short_regular_logo_q10_max <-
    tonic_short_serialized$logo_q10_max
  base$tonic$short_regular_structural_floor_spikes <-
    tonic_short_serialized$structural_floor_spikes
  base$tonic$short_regular_mode <- as.character(tonic_short_route$mode)
  base$tonic$short_regular_status <- as.character(tonic_short_route$status)
  base$spiketrainpattern <- base$spiketrainpattern %||% list()
  base$spiketrainpattern$tonic <-
    base$spiketrainpattern$tonic %||% list()
  base$spiketrainpattern$tonic$min_duration_sec <- D_T_min
  base$spiketrainpattern$tonic$short_regular_route_enabled <-
    isTRUE(tonic_short_route$enabled)
  base$spiketrainpattern$tonic$short_regular_min_isi_count <-
    tonic_short_serialized$min_isi_count
  base$spiketrainpattern$tonic$short_regular_evidence_n <-
    tonic_short_serialized$evidence_n
  base$spiketrainpattern$tonic$short_regular_group_n <-
    tonic_short_serialized$group_n
  base$spiketrainpattern$tonic$short_regular_count_q10_full <-
    tonic_short_serialized$count_q10_full
  base$spiketrainpattern$tonic$short_regular_logo_q10_min <-
    tonic_short_serialized$logo_q10_min
  base$spiketrainpattern$tonic$short_regular_logo_q10_max <-
    tonic_short_serialized$logo_q10_max
  base$spiketrainpattern$tonic$short_regular_structural_floor_spikes <-
    tonic_short_serialized$structural_floor_spikes
  base$spiketrainpattern$tonic$short_regular_mode <-
    as.character(tonic_short_route$mode)
  base$spiketrainpattern$tonic$short_regular_status <-
    as.character(tonic_short_route$status)
  
  base$pause$T_strong <- T_P_strong
  base$pause$T_seed <- T_P_seed
  base$pause$D_min <- D_P_min
  base$pause$G_min <- as.integer(G_P_min)
  
  base$detector$min_valid_isi_sec <- min_isi_sec
  base$detector$logisi_mcv_sec <- logisi_mcv_sec
  base$stats <- list(
    n_manual_burst_events = nrow(burst_ev),
    n_manual_tonic_events = nrow(tonic_ev),
    n_manual_pause_events = nrow(pause_ev),
    n_ML = length(ML_r),
    n_LOG = length(LOG_r),
    n_LOG_unresolved = LOG_unresolved_n,
    n_burst_context_values = length(burst_ctx_min),
    n_burst_edge_values = length(burst_edge_min),
    n_burst_core_pct_values = length(burst_core_q_pct),
    burst_structure_learning_applied = isTRUE(burst_structure$applied),
    burst_structure_evidence_n = as.integer(burst_structure$evidence_n),
    burst_structure_min_isi_count = as.integer(burst_structure$min_isi_count),
    burst_structure_max_isi_count = as.integer(burst_structure$max_isi_count),
    tonic_short_regular_route_enabled = isTRUE(tonic_short_route$enabled),
    tonic_short_regular_evidence_n = as.integer(tonic_short_route$evidence_n),
    tonic_short_regular_group_n = as.integer(tonic_short_route$group_n),
    tonic_short_regular_min_isi_count =
      as.integer(tonic_short_route$min_isi_count),
    tonic_short_regular_logo_q10_min =
      as.numeric(tonic_short_route$logo_q10_min),
    tonic_short_regular_logo_q10_max =
      as.numeric(tonic_short_route$logo_q10_max),
    tonic_short_regular_status = as.character(tonic_short_route$status),
    burst_structure_contrast_min = as.numeric(burst_structure$contrast_min),
    burst_structure_geom_contrast_min = as.numeric(burst_structure$geom_contrast_min),
    burst_min_duration_sec = as.numeric(burst_structure$min_duration_sec)
  )
  base
}
