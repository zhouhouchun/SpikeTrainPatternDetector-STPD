# RGS_core.R
# -----------------------------------------------------------------------------
# Robust Gaussian Surprise (RGS) burst and pause support module.
#
# Scientific scope
# - Implements a fully specified Ko-style RGS workflow:
#   1) local moving normalization of log10(ISI),
#   2) pooled reference distribution within a declared reference group,
#   3) burst/pause seeds at the 0.5% / 99.5% Gaussian limits,
#   4) iterative add/drop extension to minimize the Gaussian tail probability,
#   5) deterministic removal of overlapping candidates,
#   6) the Bonferroni approximation described by Ko et al. (2012).
# - Supports calibration-only fitting and held-out prediction through rgs_fit()
#   and rgs_predict(). rgs_analyze() is the same-data convenience wrapper.
# - Does not silently sort timestamps, remove duplicate spikes, or repair invalid
#   ISIs. Such records are stopped by QC.
#
# Dependencies for the Shiny application: shiny, DT, ggplot2, gridExtra.
# The RGS computation itself uses base R only.
# -----------------------------------------------------------------------------

RGS_IMPLEMENTATION_VERSION <- "1.1.0"

rgs_stop <- function(..., call. = FALSE) {
  stop(..., call. = call.)
}

rgs_as_scalar_numeric <- function(x, name, lower = -Inf, upper = Inf,
                                  lower_open = FALSE, upper_open = FALSE) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || !is.finite(value)) {
    rgs_stop(name, " must be one finite number.")
  }
  lower_bad <- if (lower_open) value <= lower else value < lower
  upper_bad <- if (upper_open) value >= upper else value > upper
  if (lower_bad || upper_bad) {
    left <- if (lower_open) "(" else "["
    right <- if (upper_open) ")" else "]"
    rgs_stop(name, " must be in ", left, lower, ", ", upper, right, ".")
  }
  value
}

rgs_as_scalar_integer <- function(x, name, lower = 1L, upper = .Machine$integer.max) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || !is.finite(value) || value != floor(value)) {
    rgs_stop(name, " must be one finite integer.")
  }
  value <- as.integer(value)
  if (value < lower || value > upper) {
    rgs_stop(name, " must be between ", lower, " and ", upper, ".")
  }
  value
}

rgs_raw_mad <- function(x, center = stats::median(x, na.rm = TRUE)) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x) || !is.finite(center)) return(NA_real_)
  stats::median(abs(x - center), na.rm = TRUE)
}

rgs_robust_scale <- function(x,
                             mode = c("normal_consistent", "raw_mad"),
                             center = NULL) {
  mode <- match.arg(mode)
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  center <- center %||% stats::median(x)
  raw <- rgs_raw_mad(x, center = center)
  value <- if (identical(mode, "normal_consistent")) {
    1.482602218505602 * raw
  } else {
    raw
  }
  if (!is.finite(value) || value <= .Machine$double.eps) {
    value <- stats::IQR(x, type = 8, na.rm = TRUE) / 1.34897950039216
  }
  if (!is.finite(value) || value <= .Machine$double.eps) {
    value <- stats::sd(x, na.rm = TRUE)
  }
  if (!is.finite(value) || value <= .Machine$double.eps) NA_real_ else value
}

rgs_log_transform <- function(x, log_base = c("10", "e")) {
  log_base <- match.arg(log_base)
  if (identical(log_base, "10")) log10(x) else log(x)
}

rgs_validate_spike_train <- function(x, train = "train", time_unit = c("s", "ms"),
                                     min_input_spikes = 5L) {
  time_unit <- match.arg(time_unit)
  min_input_spikes <- rgs_as_scalar_integer(min_input_spikes, "min_input_spikes", 3L)

  raw <- x
  raw_chr <- trimws(as.character(raw))
  numeric_value <- suppressWarnings(as.numeric(raw_chr))
  nonempty <- !is.na(raw) & nzchar(raw_chr)
  invalid_text <- nonempty & !is.finite(numeric_value)
  if (any(invalid_text)) {
    bad <- unique(raw_chr[invalid_text])
    rgs_stop("Train '", train, "' contains non-numeric values: ",
             paste(utils::head(bad, 5L), collapse = ", "), ".")
  }

  timestamp <- numeric_value[is.finite(numeric_value)]
  if (identical(time_unit, "ms")) timestamp <- timestamp / 1000
  if (length(timestamp) < min_input_spikes) {
    rgs_stop("Train '", train, "' has ", length(timestamp),
             " finite spikes; at least ", min_input_spikes, " are required.")
  }

  step <- diff(timestamp)
  duplicate_n <- sum(step == 0, na.rm = TRUE)
  decreasing_n <- sum(step < 0, na.rm = TRUE)
  nonpositive_n <- sum(step <= 0, na.rm = TRUE)
  if (nonpositive_n > 0L) {
    rgs_stop(
      "Train '", train, "' is not strictly increasing: ",
      duplicate_n, " duplicate step(s), ", decreasing_n,
      " decreasing step(s). Correct the source data before RGS analysis."
    )
  }
  if (any(!is.finite(step))) {
    rgs_stop("Train '", train, "' contains a non-finite ISI.")
  }

  duration <- timestamp[length(timestamp)] - timestamp[1L]
  if (!is.finite(duration) || duration <= 0) {
    rgs_stop("Train '", train, "' has an invalid recording duration.")
  }

  list(
    timestamps_sec = timestamp,
    isi_sec = step,
    qc = data.frame(
      train = train,
      n_input_rows = length(raw),
      n_spikes = length(timestamp),
      n_isi = length(step),
      start_time_sec = timestamp[1L],
      end_time_sec = timestamp[length(timestamp)],
      duration_sec = duration,
      interval_rate_hz = length(step) / duration,
      spike_count_rate_hz = length(timestamp) / duration,
      min_isi_sec = min(step),
      median_isi_sec = stats::median(step),
      max_isi_sec = max(step),
      duplicate_timestamp_n = duplicate_n,
      decreasing_timestamp_n = decreasing_n,
      warning = if (length(step) < 41L) {
        "Fewer than 41 ISIs: the moving-center window is necessarily shortened."
      } else {
        ""
      },
      stringsAsFactors = FALSE
    )
  )
}

rgs_default_parameters <- function() {
  list(
    p = 0.05,
    candidate_count_p = 0.05,
    familywise_alpha = 0.05,
    seed_z = abs(stats::qnorm(0.005)),
    central_z = abs(stats::qnorm(0.005)),
    center_set_z = stats::qnorm(0.95),
    window_fraction = 0.20,
    min_half_window = 20L,
    central_refinements = 2L,
    log_base = "10",
    mad_scale = "normal_consistent",
    min_burst_spikes = 3L,
    min_pause_spikes = 2L,
    min_burst_duration_sec = 0,
    min_pause_duration_sec = 0,
    extension_tolerance = 1e-12,
    max_extension_iterations = 100000L,
    min_input_spikes = 5L,
    min_central_nlisi = 10L,
    min_qq_correlation = 0.97,
    max_abs_disjoint_adjacent_pair_correlation = 0.20,
    scientific_mode = "exploratory"
  )
}

rgs_validate_parameters <- function(params = rgs_default_parameters()) {
  defaults <- rgs_default_parameters()
  if (!is.null(params$alpha) && is.null(params$familywise_alpha)) {
    params$familywise_alpha <- params$alpha
  }
  params$alpha <- NULL
  params <- utils::modifyList(defaults, params %||% list())

  params$p <- rgs_as_scalar_numeric(params$p, "p", 0, 0.5,
                                    lower_open = TRUE, upper_open = TRUE)
  params$candidate_count_p <- rgs_as_scalar_numeric(
    params$candidate_count_p, "candidate_count_p", 0, 1,
    lower_open = TRUE, upper_open = TRUE
  )
  params$familywise_alpha <- rgs_as_scalar_numeric(
    params$familywise_alpha, "familywise_alpha", 0, 1,
    lower_open = TRUE, upper_open = TRUE
  )
  params$seed_z <- rgs_as_scalar_numeric(params$seed_z, "seed_z", 0, Inf,
                                         lower_open = TRUE)
  params$central_z <- rgs_as_scalar_numeric(params$central_z, "central_z", 0, Inf,
                                            lower_open = TRUE)
  params$center_set_z <- rgs_as_scalar_numeric(params$center_set_z, "center_set_z", 0, Inf,
                                               lower_open = TRUE)
  params$window_fraction <- rgs_as_scalar_numeric(
    params$window_fraction, "window_fraction", 0, 1,
    lower_open = TRUE, upper_open = TRUE
  )
  params$min_half_window <- rgs_as_scalar_integer(
    params$min_half_window, "min_half_window", 1L
  )
  params$central_refinements <- rgs_as_scalar_integer(
    params$central_refinements, "central_refinements", 1L, 10L
  )
  params$log_base <- match.arg(as.character(params$log_base), c("10", "e"))
  params$mad_scale <- match.arg(
    as.character(params$mad_scale), c("normal_consistent", "raw_mad")
  )
  params$min_burst_spikes <- rgs_as_scalar_integer(
    params$min_burst_spikes, "min_burst_spikes", 2L
  )
  params$min_pause_spikes <- rgs_as_scalar_integer(
    params$min_pause_spikes, "min_pause_spikes", 2L
  )
  params$min_burst_duration_sec <- rgs_as_scalar_numeric(
    params$min_burst_duration_sec, "min_burst_duration_sec", 0, Inf
  )
  params$min_pause_duration_sec <- rgs_as_scalar_numeric(
    params$min_pause_duration_sec, "min_pause_duration_sec", 0, Inf
  )
  params$extension_tolerance <- rgs_as_scalar_numeric(
    params$extension_tolerance, "extension_tolerance", 0, Inf
  )
  params$max_extension_iterations <- rgs_as_scalar_integer(
    params$max_extension_iterations, "max_extension_iterations", 1L
  )
  params$min_input_spikes <- rgs_as_scalar_integer(
    params$min_input_spikes, "min_input_spikes", 3L
  )
  params$min_central_nlisi <- rgs_as_scalar_integer(
    params$min_central_nlisi, "min_central_nlisi", 3L
  )
  params$min_qq_correlation <- rgs_as_scalar_numeric(
    params$min_qq_correlation, "min_qq_correlation", 0, 1
  )
  params$max_abs_disjoint_adjacent_pair_correlation <- rgs_as_scalar_numeric(
    params$max_abs_disjoint_adjacent_pair_correlation,
    "max_abs_disjoint_adjacent_pair_correlation", 0, 1
  )
  params$scientific_mode <- match.arg(
    as.character(params$scientific_mode), c("exploratory", "publication")
  )
  params
}

rgs_central_location <- function(log_isi, p = 0.05,
                                 center_set_z = stats::qnorm(0.95),
                                 mad_scale = c("normal_consistent", "raw_mad"),
                                 refinements = 2L) {
  mad_scale <- match.arg(mad_scale)
  x <- suppressWarnings(as.numeric(log_isi))
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  if (length(x) < 3L) return(stats::median(x))

  quant <- stats::quantile(x, probs = c(p, 1 - p), names = FALSE,
                           type = 8, na.rm = TRUE)
  center <- mean(quant)
  scale <- rgs_robust_scale(x, mode = mad_scale)
  if (!is.finite(scale) || scale <= 0) return(stats::median(x))

  refinements <- max(1L, as.integer(refinements))
  for (iteration in seq_len(refinements)) {
    central <- x[abs(x - center) <= center_set_z * scale]
    if (length(central) < 3L) break
    next_center <- stats::median(central)
    next_scale <- rgs_robust_scale(central, mode = mad_scale,
                                   center = next_center)
    center <- next_center
    if (is.finite(next_scale) && next_scale > 0) scale <- next_scale
  }
  center
}

rgs_effective_half_window <- function(n_isi, min_half_window = 20L,
                                      window_fraction = 0.20) {
  if (n_isi < 3L) return(0L)
  requested <- max(as.integer(min_half_window), floor(window_fraction * n_isi))
  as.integer(max(1L, min(requested, floor((n_isi - 1L) / 2L))))
}

rgs_normalize_isi <- function(isi_sec, params = rgs_default_parameters()) {
  params <- rgs_validate_parameters(params)
  isi <- suppressWarnings(as.numeric(isi_sec))
  if (length(isi) < 2L || any(!is.finite(isi)) || any(isi <= 0)) {
    rgs_stop("rgs_normalize_isi() requires at least two finite, positive ISIs.")
  }

  log_isi <- rgs_log_transform(isi, params$log_base)
  n <- length(log_isi)
  q <- rgs_effective_half_window(
    n,
    min_half_window = params$min_half_window,
    window_fraction = params$window_fraction
  )
  center <- rep(NA_real_, n)

  if (q == 0L || (2L * q + 1L) >= n) {
    global_center <- rgs_central_location(
      log_isi,
      p = params$p,
      center_set_z = params$center_set_z,
      mad_scale = params$mad_scale,
      refinements = params$central_refinements
    )
    center[] <- global_center
  } else {
    first_index <- seq_len(2L * q + 1L)
    last_index <- seq.int(n - 2L * q, n)
    first_center <- rgs_central_location(
      log_isi[first_index], params$p, params$center_set_z,
      params$mad_scale, params$central_refinements
    )
    last_center <- rgs_central_location(
      log_isi[last_index], params$p, params$center_set_z,
      params$mad_scale, params$central_refinements
    )
    center[seq_len(q)] <- first_center
    center[seq.int(n - q + 1L, n)] <- last_center
    middle <- seq.int(q + 1L, n - q)
    for (i in middle) {
      window <- seq.int(i - q, i + q)
      center[i] <- rgs_central_location(
        log_isi[window], params$p, params$center_set_z,
        params$mad_scale, params$central_refinements
      )
    }
  }

  if (any(!is.finite(center))) {
    fallback <- rgs_central_location(
      log_isi,
      p = params$p,
      center_set_z = params$center_set_z,
      mad_scale = params$mad_scale,
      refinements = params$central_refinements
    )
    center[!is.finite(center)] <- fallback
  }

  data.frame(
    isi_index = seq_along(isi),
    isi_sec = isi,
    log_isi = log_isi,
    moving_center = center,
    nlisi = log_isi - center,
    effective_half_window = q,
    effective_window_length = min(n, 2L * q + 1L),
    stringsAsFactors = FALSE
  )
}

rgs_resolve_groups <- function(train_names, groups = NULL) {
  train_names <- trimws(as.character(train_names))
  if (any(!nzchar(train_names)) || anyDuplicated(train_names)) {
    rgs_stop("train_names must be unique and non-empty.")
  }
  if (is.null(groups)) {
    return(stats::setNames(rep("all", length(train_names)), train_names))
  }
  if (is.data.frame(groups)) {
    required <- c("Spike_train", "Reference_group")
    missing <- setdiff(required, names(groups))
    if (length(missing)) {
      rgs_stop("Group table must contain columns: Spike_train, Reference_group.")
    }
    spike_train <- trimws(as.character(groups$Spike_train))
    reference_group <- trimws(as.character(groups$Reference_group))
    if (any(!nzchar(spike_train))) {
      rgs_stop("Group table contains an empty Spike_train value.")
    }
    duplicated_train <- unique(spike_train[duplicated(spike_train)])
    if (length(duplicated_train)) {
      rgs_stop(
        "Group table contains duplicate Spike_train row(s): ",
        paste(duplicated_train, collapse = ", "), "."
      )
    }
    vector <- reference_group
    names(vector) <- spike_train
    groups <- vector
  }
  group_names <- names(groups)
  groups <- as.character(groups)
  names(groups) <- group_names
  if (is.null(names(groups))) {
    if (length(groups) != length(train_names)) {
      rgs_stop("Unnamed groups must have one value per train.")
    }
    names(groups) <- train_names
  } else {
    names(groups) <- trimws(as.character(names(groups)))
    if (any(!nzchar(names(groups)))) {
      rgs_stop("Named groups contain an empty train name.")
    }
    duplicated_name <- unique(names(groups)[duplicated(names(groups))])
    if (length(duplicated_name)) {
      rgs_stop(
        "Named groups contain duplicate train name(s): ",
        paste(duplicated_name, collapse = ", "), "."
      )
    }
  }
  out <- groups[train_names]
  missing <- is.na(out) | !nzchar(trimws(out))
  if (any(missing)) {
    rgs_stop("Missing reference group for train(s): ",
             paste(train_names[missing], collapse = ", "), ".")
  }
  stats::setNames(as.character(out), train_names)
}

rgs_reference_diagnostics <- function(values, group, params, train_n) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  preliminary_mu <- stats::median(values)
  preliminary_sigma <- rgs_robust_scale(
    values, params$mad_scale, center = preliminary_mu
  )
  if (!is.finite(preliminary_sigma) || preliminary_sigma <= 0) {
    rgs_stop("Reference group '", group, "' has zero or undefined NLISI scale.")
  }
  central_lower <- preliminary_mu - params$central_z * preliminary_sigma
  central_upper <- preliminary_mu + params$central_z * preliminary_sigma
  central_flag <- values >= central_lower & values <= central_upper
  central <- values[central_flag]
  if (length(central) < params$min_central_nlisi) {
    rgs_stop("Reference group '", group,
             "' has fewer than ", params$min_central_nlisi,
             " central NLISIs after trimming; RGS is not estimable.")
  }
  mu <- stats::median(central)
  sigma <- rgs_robust_scale(central, params$mad_scale, center = mu)
  if (!is.finite(sigma) || sigma <= 0) {
    rgs_stop("Reference group '", group, "' has an invalid central scale.")
  }

  lower_probability <- stats::pnorm(central_lower, mean = mu, sd = sigma)
  upper_probability <- stats::pnorm(central_upper, mean = mu, sd = sigma)
  probability <- lower_probability + stats::ppoints(length(central)) *
    (upper_probability - lower_probability)
  theoretical <- stats::qnorm(probability, mean = mu, sd = sigma)
  observed <- sort(central)
  qq_cor <- if (length(observed) >= 3L) {
    suppressWarnings(stats::cor(observed, theoretical))
  } else {
    NA_real_
  }

  qq_not_estimable <- !is.finite(qq_cor) ||
    qq_cor < params$min_qq_correlation
  scientific_status <- if (qq_not_estimable) "not_estimable" else "estimable"
  scientific_reason <- if (!is.finite(qq_cor)) {
    "qq_correlation_not_finite"
  } else if (qq_cor < params$min_qq_correlation) {
    "qq_correlation_below_declared_minimum"
  } else {
    ""
  }

  data.frame(
    reference_group = group,
    train_n = as.integer(train_n),
    pooled_nlisi_n = length(values),
    central_nlisi_n = length(central),
    central_fraction = length(central) / length(values),
    preliminary_median = preliminary_mu,
    preliminary_scale = preliminary_sigma,
    central_lower = central_lower,
    central_upper = central_upper,
    central_mu = mu,
    central_sigma = sigma,
    burst_seed_threshold = preliminary_mu - params$seed_z * preliminary_sigma,
    pause_seed_threshold = preliminary_mu + params$seed_z * preliminary_sigma,
    seed_z = params$seed_z,
    central_z = params$central_z,
    qq_correlation = qq_cor,
    scientific_status = scientific_status,
    scientific_reasons = scientific_reason,
    warning = if (qq_not_estimable) {
      "Central NLISI distribution departs visibly from a Gaussian reference; inspect the diagnostic plot."
    } else {
      ""
    },
    stringsAsFactors = FALSE
  )
}

rgs_prepare_trains <- function(spike_trains, params, time_unit = c("s", "ms")) {
  time_unit <- match.arg(time_unit)
  if (!is.list(spike_trains) || !length(spike_trains)) {
    rgs_stop("spike_trains must be a non-empty named list.")
  }
  train_names <- names(spike_trains)
  if (is.null(train_names) || any(!nzchar(train_names)) || anyDuplicated(train_names)) {
    rgs_stop("spike_trains must have unique, non-empty names.")
  }

  validated <- vector("list", length(spike_trains))
  normalized <- vector("list", length(spike_trains))
  names(validated) <- names(normalized) <- train_names
  qc <- vector("list", length(spike_trains))

  for (i in seq_along(spike_trains)) {
    train <- train_names[i]
    checked <- rgs_validate_spike_train(
      spike_trains[[i]], train = train, time_unit = time_unit,
      min_input_spikes = params$min_input_spikes
    )
    validated[[i]] <- checked$timestamps_sec
    normalized[[i]] <- rgs_normalize_isi(checked$isi_sec, params)
    normalized[[i]]$train <- train
    qc[[i]] <- checked$qc
  }

  list(
    timestamps = validated,
    normalized = normalized,
    qc = do.call(rbind, qc)
  )
}

rgs_fit <- function(spike_trains, groups = NULL,
                    params = rgs_default_parameters(),
                    time_unit = c("s", "ms")) {
  params <- rgs_validate_parameters(params)
  time_unit <- match.arg(time_unit)
  prepared <- rgs_prepare_trains(spike_trains, params, time_unit)
  group_map <- rgs_resolve_groups(names(prepared$timestamps), groups)

  references <- list()
  diagnostic_rows <- list()
  pooled_values <- list()
  unique_groups <- sort(unique(unname(group_map)), method = "radix")
  for (group in unique_groups) {
    trains <- names(group_map)[group_map == group]
    pool <- unlist(lapply(prepared$normalized[trains], function(x) x$nlisi),
                   use.names = FALSE)
    diag <- rgs_reference_diagnostics(
      pool, group = group, params = params, train_n = length(trains)
    )
    pair_rows <- lapply(prepared$normalized[trains], function(tbl) {
      x <- suppressWarnings(as.numeric(tbl$nlisi))
      if (length(x) < 2L) return(NULL)
      index <- seq.int(1L, length(x) - 1L, by = 2L)
      left <- x[index]
      right <- x[index + 1L]
      central <- is.finite(left) & is.finite(right) &
        left >= diag$burst_seed_threshold[1] &
        left <= diag$pause_seed_threshold[1] &
        right >= diag$burst_seed_threshold[1] &
        right <= diag$pause_seed_threshold[1]
      if (!any(central)) return(NULL)
      data.frame(left = left[central], right = right[central])
    })
    pair_rows <- Filter(Negate(is.null), pair_rows)
    central_pairs <- if (length(pair_rows)) do.call(rbind, pair_rows) else data.frame()
    lag1 <- if (nrow(central_pairs) >= 3L) {
      suppressWarnings(stats::cor(central_pairs$left, central_pairs$right))
    } else {
      NA_real_
    }
    diag$central_disjoint_adjacent_pair_n <- nrow(central_pairs)
    diag$central_disjoint_adjacent_pair_correlation <- lag1
    dependence_warning <- is.finite(lag1) &&
      abs(lag1) > params$max_abs_disjoint_adjacent_pair_correlation
    if (dependence_warning) {
      note <- paste0(
        "Central NLISI disjoint-adjacent-pair dependence exceeds the declared limit; ",
        "Gaussian-sum independence is questionable."
      )
      diag$warning <- if (nzchar(diag$warning)) paste(diag$warning, note) else note
      if (!identical(diag$scientific_status, "not_estimable")) {
        diag$scientific_status <- "estimable_with_warning"
      }
      diag$scientific_reasons <- paste(
        c(
          diag$scientific_reasons[nzchar(diag$scientific_reasons)],
          "disjoint_adjacent_pair_correlation_above_declared_maximum"
        ),
        collapse = ";"
      )
    }
    references[[group]] <- list(
      reference_group = group,
      mu = diag$central_mu[1],
      sigma = diag$central_sigma[1],
      burst_seed_threshold = diag$burst_seed_threshold[1],
      pause_seed_threshold = diag$pause_seed_threshold[1],
      training_trains = trains,
      pooled_nlisi_n = length(pool),
      scientific_status = diag$scientific_status[1],
      scientific_reasons = diag$scientific_reasons[1]
    )
    diagnostic_rows[[group]] <- diag
    pooled_values[[group]] <- pool
  }

  structure(
    list(
      implementation = "Ko-style Robust Gaussian Surprise",
      implementation_version = RGS_IMPLEMENTATION_VERSION,
      parameters = params,
      time_unit_input = time_unit,
      group_map = group_map,
      references = references,
      reference_diagnostics = do.call(rbind, diagnostic_rows),
      pooled_nlisi = pooled_values,
      training_qc = prepared$qc,
      training_normalized = prepared$normalized,
      training_timestamps_sec = prepared$timestamps,
      scientific_status = if (any(
        vapply(diagnostic_rows, function(x) identical(x$scientific_status[1], "not_estimable"), logical(1))
      )) {
        "not_estimable"
      } else if (any(
        vapply(diagnostic_rows, function(x) identical(x$scientific_status[1], "estimable_with_warning"), logical(1))
      )) {
        "estimable_with_warning"
      } else {
        "estimable"
      },
      scientific_reasons = setdiff(unique(unlist(lapply(
        diagnostic_rows,
        function(x) strsplit(x$scientific_reasons[1], ";", fixed = TRUE)[[1L]]
      ), use.names = FALSE)), "")
    ),
    class = c("rgs_fit", "list")
  )
}

rgs_interval_log_p <- function(nlisi, start_isi, end_isi, mu, sigma,
                               event_type = c("burst", "pause")) {
  event_type <- match.arg(event_type)
  start_isi <- as.integer(start_isi)
  end_isi <- as.integer(end_isi)
  if (!is.finite(start_isi) || !is.finite(end_isi) ||
      start_isi < 1L || end_isi > length(nlisi) || end_isi < start_isi) {
    return(NA_real_)
  }
  q <- end_isi - start_isi + 1L
  observed_sum <- sum(nlisi[start_isi:end_isi])
  mean_sum <- q * mu
  sd_sum <- sqrt(q) * sigma
  if (!is.finite(sd_sum) || sd_sum <= 0) return(NA_real_)
  stats::pnorm(
    observed_sum,
    mean = mean_sum,
    sd = sd_sum,
    lower.tail = identical(event_type, "burst"),
    log.p = TRUE
  )
}

rgs_expand_seed <- function(nlisi, seed_isi, mu, sigma,
                            event_type = c("burst", "pause"),
                            tolerance = 1e-12,
                            max_iterations = 100000L) {
  event_type <- match.arg(event_type)
  n <- length(nlisi)
  start <- end <- as.integer(seed_isi)
  current <- rgs_interval_log_p(nlisi, start, end, mu, sigma, event_type)
  if (!is.finite(current)) return(NULL)
  left_added <- 0L
  right_added <- 0L
  iteration <- 0L

  repeat {
    iteration <- iteration + 1L
    if (iteration > max_iterations) {
      rgs_stop("RGS extension exceeded max_iterations at seed ", seed_isi, ".")
    }

    choices <- data.frame(
      side = character(),
      start = integer(),
      end = integer(),
      log_p = numeric(),
      stringsAsFactors = FALSE
    )
    if (start > 1L) {
      lp <- rgs_interval_log_p(nlisi, start - 1L, end, mu, sigma, event_type)
      choices <- rbind(choices, data.frame(
        side = "left", start = start - 1L, end = end, log_p = lp,
        stringsAsFactors = FALSE
      ))
    }
    if (end < n) {
      rp <- rgs_interval_log_p(nlisi, start, end + 1L, mu, sigma, event_type)
      choices <- rbind(choices, data.frame(
        side = "right", start = start, end = end + 1L, log_p = rp,
        stringsAsFactors = FALSE
      ))
    }
    choices <- choices[is.finite(choices$log_p), , drop = FALSE]
    if (!nrow(choices)) break
    choices <- choices[order(choices$log_p, choices$side, method = "radix"), , drop = FALSE]
    best <- choices[1L, , drop = FALSE]
    scale <- max(1, abs(current), abs(best$log_p))
    improved <- best$log_p < current - tolerance * scale
    if (!isTRUE(improved)) break

    start <- best$start
    end <- best$end
    current <- best$log_p
    if (best$side == "left") left_added <- left_added + 1L
    if (best$side == "right") right_added <- right_added + 1L
  }

  data.frame(
    seed_isi = as.integer(seed_isi),
    start_isi = start,
    end_isi = end,
    n_isi = end - start + 1L,
    log_p = current,
    surprise = -current,
    left_added = left_added,
    right_added = right_added,
    extension_iterations = iteration,
    stringsAsFactors = FALSE
  )
}

rgs_generate_candidates <- function(nlisi, seed_indices, mu, sigma,
                                    event_type, params) {
  if (!length(seed_indices)) {
    return(data.frame(
      seed_isi = integer(), start_isi = integer(), end_isi = integer(),
      n_isi = integer(), log_p = numeric(), surprise = numeric(),
      left_added = integer(), right_added = integer(),
      extension_iterations = integer(), event_type = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seed_indices, function(seed) {
    rgs_expand_seed(
      nlisi, seed, mu, sigma, event_type,
      tolerance = params$extension_tolerance,
      max_iterations = params$max_extension_iterations
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out$event_type <- event_type
  out
}

rgs_deduplicate_candidates <- function(candidates) {
  if (is.null(candidates) || !nrow(candidates)) return(candidates)
  probability_column <- if ("log_p" %in% names(candidates)) {
    "log_p"
  } else if ("raw_log_p" %in% names(candidates)) {
    "raw_log_p"
  } else {
    rgs_stop(
      "Candidate table must contain log_p or raw_log_p before deduplication."
    )
  }
  probability <- candidates[[probability_column]]
  candidates <- candidates[order(
    probability,
    -candidates$n_isi,
    candidates$start_isi,
    candidates$end_isi,
    candidates$seed_isi,
    method = "radix"
  ), , drop = FALSE]
  geometry <- paste(candidates$start_isi, candidates$end_isi, sep = ":")
  candidates <- candidates[!duplicated(geometry), , drop = FALSE]
  rownames(candidates) <- NULL
  candidates
}

rgs_resolve_overlaps <- function(candidates) {
  candidates <- rgs_deduplicate_candidates(candidates)
  if (is.null(candidates) || !nrow(candidates)) return(candidates)
  probability_column <- if ("log_p" %in% names(candidates)) {
    "log_p"
  } else if ("raw_log_p" %in% names(candidates)) {
    "raw_log_p"
  } else {
    rgs_stop(
      "Candidate table must contain log_p or raw_log_p before overlap resolution."
    )
  }
  candidates <- candidates[order(
    candidates[[probability_column]],
    -candidates$n_isi,
    candidates$start_isi,
    candidates$end_isi,
    method = "radix"
  ), , drop = FALSE]
  accepted <- integer()
  for (i in seq_len(nrow(candidates))) {
    if (!length(accepted)) {
      accepted <- i
      next
    }
    overlap <- candidates$start_isi[i] <= candidates$end_isi[accepted] &
      candidates$end_isi[i] >= candidates$start_isi[accepted]
    if (!any(overlap)) accepted <- c(accepted, i)
  }
  out <- candidates[accepted, , drop = FALSE]
  out <- out[order(out$start_isi, out$end_isi, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

rgs_attach_candidate_metrics <- function(candidates, timestamps_sec, nlisi,
                                         train, reference_group) {
  if (is.null(candidates) || !nrow(candidates)) return(candidates)
  isi <- diff(timestamps_sec)
  rows <- lapply(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, , drop = FALSE]
    start_isi <- as.integer(row$start_isi)
    end_isi <- as.integer(row$end_isi)
    start_spike <- start_isi
    end_spike <- end_isi + 1L
    event_isi <- isi[start_isi:end_isi]
    n_isi <- length(event_isi)
    n_spikes <- n_isi + 1L
    duration <- timestamps_sec[end_spike] - timestamps_sec[start_spike]
    raw_p <- exp(row$log_p)
    if (!is.finite(raw_p)) raw_p <- if (row$log_p == -Inf) 0 else NA_real_
    data.frame(
      train = train,
      reference_group = reference_group,
      event_type = as.character(row$event_type),
      seed_isi = as.integer(row$seed_isi),
      start_isi = start_isi,
      end_isi = end_isi,
      start_spike_index = start_spike,
      end_spike_index = end_spike,
      start_time_sec = timestamps_sec[start_spike],
      end_time_sec = timestamps_sec[end_spike],
      duration_sec = duration,
      n_isi = n_isi,
      n_spikes = n_spikes,
      n_internal_spikes = max(0L, n_spikes - 2L),
      interval_rate_hz = if (duration > 0) n_isi / duration else NA_real_,
      internal_spike_rate_hz = if (duration > 0) max(0L, n_spikes - 2L) / duration else NA_real_,
      mean_isi_sec = mean(event_isi),
      median_isi_sec = stats::median(event_isi),
      min_isi_sec = min(event_isi),
      max_isi_sec = max(event_isi),
      pre_event_isi_sec = if (start_isi > 1L) isi[start_isi - 1L] else NA_real_,
      post_event_isi_sec = if (end_isi < length(isi)) isi[end_isi + 1L] else NA_real_,
      nlisi_sum = sum(nlisi[start_isi:end_isi]),
      raw_log_p = row$log_p,
      raw_p = raw_p,
      surprise = -row$log_p,
      left_added = as.integer(row$left_added),
      right_added = as.integer(row$right_added),
      extension_iterations = as.integer(row$extension_iterations),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

rgs_finalize_candidates <- function(candidates, params, event_type) {
  if (is.null(candidates) || !nrow(candidates)) {
    candidates$overlap_selected <- logical(nrow(candidates))
    candidates$bonferroni_k <- integer(nrow(candidates))
    candidates$adjusted_p <- numeric(nrow(candidates))
    candidates$adjusted_surprise <- numeric(nrow(candidates))
    candidates$accepted <- logical(nrow(candidates))
    candidates$rejection_reason <- character(nrow(candidates))
    return(candidates)
  }
  min_spikes <- if (identical(event_type, "burst")) {
    params$min_burst_spikes
  } else {
    params$min_pause_spikes
  }
  min_duration <- if (identical(event_type, "burst")) {
    params$min_burst_duration_sec
  } else {
    params$min_pause_duration_sec
  }

  candidates <- rgs_deduplicate_candidates(candidates)
  size_pass <- candidates$n_spikes >= min_spikes
  duration_pass <- candidates$duration_sec >= min_duration
  finite_pass <- is.finite(candidates$raw_p)
  pre_overlap_eligible <- size_pass & duration_pass & finite_pass

  overlap_selected <- rep(FALSE, nrow(candidates))
  if (any(pre_overlap_eligible)) {
    eligible_rows <- which(pre_overlap_eligible)
    selected_table <- rgs_resolve_overlaps(
      candidates[eligible_rows, , drop = FALSE]
    )
    selected_geometry <- paste(
      selected_table$start_isi, selected_table$end_isi, sep = ":"
    )
    all_geometry <- paste(candidates$start_isi, candidates$end_isi, sep = ":")
    overlap_selected <- pre_overlap_eligible & all_geometry %in% selected_geometry
  }

  k <- sum(
    overlap_selected & candidates$raw_p < params$candidate_count_p,
    na.rm = TRUE
  )
  adjusted <- rep(1, nrow(candidates))
  if (k > 0L) {
    adjusted[overlap_selected] <- pmin(
      1, candidates$raw_p[overlap_selected] * k
    )
  }
  accepted <- overlap_selected & adjusted < params$familywise_alpha
  reason <- rep("", nrow(candidates))
  reason[!size_pass] <- "below_minimum_spike_count"
  reason[size_pass & !duration_pass] <- "below_minimum_duration"
  reason[size_pass & duration_pass & !finite_pass] <- "non_finite_probability"
  reason[pre_overlap_eligible & !overlap_selected] <-
    "overlap_with_more_significant_candidate"
  reason[overlap_selected & !accepted] <-
    "bonferroni_adjusted_p_not_below_alpha"

  candidates$overlap_selected <- overlap_selected
  candidates$bonferroni_k <- as.integer(k)
  candidates$adjusted_p <- adjusted
  candidates$adjusted_surprise <- ifelse(adjusted > 0, -log(adjusted), Inf)
  candidates$accepted <- accepted
  candidates$rejection_reason <- reason
  candidates <- candidates[order(
    candidates$start_isi, candidates$end_isi, candidates$raw_log_p,
    method = "radix"
  ), , drop = FALSE]
  rownames(candidates) <- NULL
  candidates
}

rgs_assign_event_ids <- function(events, prefix) {
  if (is.null(events) || !nrow(events)) {
    events$event_id <- character(nrow(events))
    return(events)
  }
  events <- events[order(events$train, events$start_isi, events$end_isi,
                         method = "radix"), , drop = FALSE]
  index <- ave(seq_len(nrow(events)), events$train, FUN = seq_along)
  events$event_id <- paste0(prefix, "_", events$train, "_", sprintf("%04d", index))
  rownames(events) <- NULL
  events
}

rgs_detect_train <- function(timestamps_sec, normalized, reference, train,
                             reference_group, params) {
  nlisi <- normalized$nlisi
  burst_seeds <- which(nlisi < reference$burst_seed_threshold)
  pause_seeds <- which(nlisi > reference$pause_seed_threshold)

  burst_candidates <- rgs_generate_candidates(
    nlisi, burst_seeds, reference$mu, reference$sigma, "burst", params
  )
  pause_candidates <- rgs_generate_candidates(
    nlisi, pause_seeds, reference$mu, reference$sigma, "pause", params
  )
  burst_candidates <- rgs_deduplicate_candidates(burst_candidates)
  pause_candidates <- rgs_deduplicate_candidates(pause_candidates)

  burst_candidates <- rgs_attach_candidate_metrics(
    burst_candidates, timestamps_sec, nlisi, train, reference_group
  )
  pause_candidates <- rgs_attach_candidate_metrics(
    pause_candidates, timestamps_sec, nlisi, train, reference_group
  )
  burst_candidates <- rgs_finalize_candidates(burst_candidates, params, "burst")
  pause_candidates <- rgs_finalize_candidates(pause_candidates, params, "pause")

  bursts <- burst_candidates[burst_candidates$accepted %in% TRUE, , drop = FALSE]
  pauses <- pause_candidates[pause_candidates$accepted %in% TRUE, , drop = FALSE]
  bursts <- rgs_assign_event_ids(bursts, "rgs_burst")
  pauses <- rgs_assign_event_ids(pauses, "rgs_pause")

  normalized$timestamp_left_sec <- timestamps_sec[normalized$isi_index]
  normalized$timestamp_right_sec <- timestamps_sec[normalized$isi_index + 1L]
  normalized$train <- train
  normalized$reference_group <- reference_group
  normalized$burst_seed <- normalized$nlisi < reference$burst_seed_threshold
  normalized$pause_seed <- normalized$nlisi > reference$pause_seed_threshold
  normalized$burst_event_id <- ""
  normalized$pause_event_id <- ""
  if (nrow(bursts)) {
    for (i in seq_len(nrow(bursts))) {
      idx <- seq.int(bursts$start_isi[i], bursts$end_isi[i])
      normalized$burst_event_id[idx] <- bursts$event_id[i]
    }
  }
  if (nrow(pauses)) {
    for (i in seq_len(nrow(pauses))) {
      idx <- seq.int(pauses$start_isi[i], pauses$end_isi[i])
      normalized$pause_event_id[idx] <- pauses$event_id[i]
    }
  }

  list(
    bursts = bursts,
    pauses = pauses,
    burst_candidates = burst_candidates,
    pause_candidates = pause_candidates,
    per_isi = normalized,
    seed_counts = data.frame(
      train = train,
      reference_group = reference_group,
      burst_seed_n = length(burst_seeds),
      pause_seed_n = length(pause_seeds),
      burst_candidate_n = nrow(burst_candidates),
      pause_candidate_n = nrow(pause_candidates),
      burst_event_n = nrow(bursts),
      pause_event_n = nrow(pauses),
      stringsAsFactors = FALSE
    )
  )
}

rgs_event_union_spikes <- function(events) {
  if (is.null(events) || !nrow(events)) return(integer())
  sort(unique(unlist(Map(
    function(a, b) seq.int(a, b),
    events$start_spike_index,
    events$end_spike_index
  ), use.names = FALSE)))
}

rgs_summarize_train <- function(train, timestamps_sec, bursts, pauses,
                                reference_group, per_isi) {
  duration <- timestamps_sec[length(timestamps_sec)] - timestamps_sec[1L]
  burst_spikes <- rgs_event_union_spikes(bursts)
  pause_time <- if (nrow(pauses)) sum(pauses$duration_sec) else 0
  burst_time <- if (nrow(bursts)) sum(bursts$duration_sec) else 0
  data.frame(
    train = train,
    reference_group = reference_group,
    n_spikes = length(timestamps_sec),
    n_isi = length(timestamps_sec) - 1L,
    start_time_sec = timestamps_sec[1L],
    end_time_sec = timestamps_sec[length(timestamps_sec)],
    duration_sec = duration,
    interval_rate_hz = (length(timestamps_sec) - 1L) / duration,
    spike_count_rate_hz = length(timestamps_sec) / duration,
    burst_n = nrow(bursts),
    burst_rate_per_min = if (duration > 0) 60 * nrow(bursts) / duration else NA_real_,
    burst_spike_n = length(burst_spikes),
    burst_spike_fraction = length(burst_spikes) / length(timestamps_sec),
    burst_time_sec = burst_time,
    burst_time_fraction = burst_time / duration,
    median_burst_duration_sec = if (nrow(bursts)) stats::median(bursts$duration_sec) else NA_real_,
    median_spikes_per_burst = if (nrow(bursts)) stats::median(bursts$n_spikes) else NA_real_,
    pause_n = nrow(pauses),
    pause_rate_per_min = if (duration > 0) 60 * nrow(pauses) / duration else NA_real_,
    pause_time_sec = pause_time,
    pause_time_fraction = pause_time / duration,
    median_pause_duration_sec = if (nrow(pauses)) stats::median(pauses$duration_sec) else NA_real_,
    median_pause_internal_spikes = if (nrow(pauses)) stats::median(pauses$n_internal_spikes) else NA_real_,
    nlisi_median = stats::median(per_isi$nlisi),
    nlisi_scale = rgs_robust_scale(per_isi$nlisi, "normal_consistent"),
    nlisi_lag1_correlation = if (nrow(per_isi) >= 3L) {
      suppressWarnings(stats::cor(per_isi$nlisi[-nrow(per_isi)],
                                  per_isi$nlisi[-1L]))
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

rgs_predict <- function(fit, spike_trains, groups = NULL,
                        time_unit = c("s", "ms")) {
  if (!inherits(fit, "rgs_fit")) rgs_stop("fit must be an object returned by rgs_fit().")
  time_unit <- match.arg(time_unit)
  params <- rgs_validate_parameters(fit$parameters)
  prepared <- rgs_prepare_trains(spike_trains, params, time_unit)
  group_map <- rgs_resolve_groups(names(prepared$timestamps), groups)
  missing_groups <- setdiff(unique(unname(group_map)), names(fit$references))
  if (length(missing_groups)) {
    rgs_stop("No fitted RGS reference for group(s): ",
             paste(missing_groups, collapse = ", "), ".")
  }
  used_groups <- unique(unname(group_map))
  not_estimable_groups <- used_groups[vapply(
    used_groups,
    function(group) identical(fit$references[[group]]$scientific_status, "not_estimable"),
    logical(1)
  )]
  if (identical(params$scientific_mode, "publication") &&
      length(not_estimable_groups)) {
    rgs_stop(
      "RGS is not estimable for reference group(s): ",
      paste(not_estimable_groups, collapse = ", "),
      ". Use exploratory mode only for explicitly flagged diagnostic output."
    )
  }

  result_rows <- lapply(names(prepared$timestamps), function(train) {
    group <- unname(group_map[train])
    rgs_detect_train(
      timestamps_sec = prepared$timestamps[[train]],
      normalized = prepared$normalized[[train]],
      reference = fit$references[[group]],
      train = train,
      reference_group = group,
      params = params
    )
  })
  names(result_rows) <- names(prepared$timestamps)

  bind_component <- function(name) {
    all_rows <- lapply(result_rows, `[[`, name)
    rows <- all_rows[vapply(
      all_rows,
      function(x) is.data.frame(x) && nrow(x) > 0L,
      logical(1)
    )]
    if (length(rows)) return(do.call(rbind, rows))
    prototype <- all_rows[vapply(all_rows, is.data.frame, logical(1))]
    if (!length(prototype)) return(data.frame())
    prototype[[1L]][0, , drop = FALSE]
  }
  bursts <- bind_component("bursts")
  pauses <- bind_component("pauses")
  burst_candidates <- bind_component("burst_candidates")
  pause_candidates <- bind_component("pause_candidates")
  per_isi <- bind_component("per_isi")
  seed_counts <- bind_component("seed_counts")

  summaries <- lapply(names(result_rows), function(train) {
    item <- result_rows[[train]]
    rgs_summarize_train(
      train,
      prepared$timestamps[[train]],
      item$bursts,
      item$pauses,
      unname(group_map[train]),
      item$per_isi
    )
  })
  summary <- do.call(rbind, summaries)

  structure(
    list(
      implementation = fit$implementation,
      implementation_version = fit$implementation_version,
      scientific_status = fit$scientific_status,
      scientific_reasons = fit$scientific_reasons,
      fit = fit,
      parameters = params,
      target_group_map = group_map,
      timestamps_sec = prepared$timestamps,
      qc = prepared$qc,
      summary = summary,
      bursts = bursts,
      pauses = pauses,
      burst_candidates = burst_candidates,
      pause_candidates = pause_candidates,
      per_isi = per_isi,
      seed_counts = seed_counts
    ),
    class = c("rgs_analysis", "list")
  )
}

rgs_analyze <- function(spike_trains, groups = NULL,
                        params = rgs_default_parameters(),
                        time_unit = c("s", "ms")) {
  time_unit <- match.arg(time_unit)
  fit <- rgs_fit(spike_trains, groups = groups, params = params,
                 time_unit = time_unit)
  rgs_predict(fit, spike_trains, groups = groups, time_unit = time_unit)
}

rgs_dataframe_to_trains <- function(data, include_regex = NULL) {
  if (!is.data.frame(data) || !ncol(data)) {
    rgs_stop("Input CSV contains no columns.")
  }
  nonempty_column <- vapply(data, function(x) {
    text <- trimws(as.character(x))
    any(!is.na(x) & nzchar(text))
  }, logical(1))
  data <- data[, nonempty_column, drop = FALSE]
  columns <- names(data)
  if (!length(columns)) {
    rgs_stop("Input CSV contains only empty columns.")
  }
  if (!is.null(include_regex) && nzchar(trimws(include_regex))) {
    keep <- grepl(include_regex, columns, perl = TRUE)
    columns <- columns[keep]
  }
  if (!length(columns)) {
    rgs_stop("No spike-train columns matched the requested regular expression.")
  }
  out <- lapply(columns, function(column) data[[column]])
  names(out) <- columns
  out
}

rgs_read_group_map <- function(path, train_names) {
  if (is.null(path) || !nzchar(path)) {
    return(stats::setNames(rep("all", length(train_names)), train_names))
  }
  map <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  rgs_resolve_groups(train_names, map)
}

rgs_compact_analysis <- function(analysis) {
  fit <- analysis$fit
  compact_references <- lapply(fit$references, function(reference) {
    reference[c(
      "reference_group", "mu", "sigma", "burst_seed_threshold",
      "pause_seed_threshold", "training_trains", "pooled_nlisi_n",
      "scientific_status", "scientific_reasons"
    )]
  })
  list(
    implementation = analysis$implementation,
    implementation_version = analysis$implementation_version,
    scientific_status = analysis$scientific_status,
    scientific_reasons = analysis$scientific_reasons,
    parameters = analysis$parameters,
    target_group_map = analysis$target_group_map,
    fit = list(
      implementation = fit$implementation,
      implementation_version = fit$implementation_version,
      parameters = fit$parameters,
      group_map = fit$group_map,
      references = compact_references,
      reference_diagnostics = fit$reference_diagnostics,
      training_qc = fit$training_qc
    ),
    summary = analysis$summary,
    seed_counts = analysis$seed_counts,
    qc = analysis$qc
  )
}

rgs_write_exports <- function(analysis, out_dir,
                              include_full_analysis_rds = FALSE) {
  if (!inherits(analysis, "rgs_analysis")) {
    rgs_stop("analysis must be returned by rgs_analyze() or rgs_predict().")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(analysis$summary, file.path(out_dir, "RGS_summary.csv"), row.names = FALSE)
  utils::write.csv(analysis$bursts, file.path(out_dir, "RGS_bursts.csv"), row.names = FALSE)
  utils::write.csv(analysis$pauses, file.path(out_dir, "RGS_pauses.csv"), row.names = FALSE)
  utils::write.csv(analysis$burst_candidates, file.path(out_dir, "RGS_burst_candidates_audit.csv"), row.names = FALSE)
  utils::write.csv(analysis$pause_candidates, file.path(out_dir, "RGS_pause_candidates_audit.csv"), row.names = FALSE)
  utils::write.csv(analysis$per_isi, file.path(out_dir, "RGS_per_ISI.csv"), row.names = FALSE)
  utils::write.csv(analysis$qc, file.path(out_dir, "RGS_input_QC.csv"), row.names = FALSE)
  utils::write.csv(analysis$fit$reference_diagnostics, file.path(out_dir, "RGS_reference_diagnostics.csv"), row.names = FALSE)
  utils::write.csv(analysis$seed_counts, file.path(out_dir, "RGS_seed_candidate_counts.csv"), row.names = FALSE)
  saveRDS(rgs_compact_analysis(analysis), file.path(out_dir, "RGS_analysis.rds"))
  if (isTRUE(include_full_analysis_rds)) {
    saveRDS(analysis, file.path(out_dir, "RGS_analysis_full.rds"))
  }
  invisible(out_dir)
}

print.rgs_fit <- function(x, ...) {
  cat("Ko-style Robust Gaussian Surprise fit\n")
  cat("Version:", x$implementation_version, "\n")
  cat("Reference groups:", paste(names(x$references), collapse = ", "), "\n")
  print(x$reference_diagnostics, row.names = FALSE)
  invisible(x)
}

print.rgs_analysis <- function(x, ...) {
  cat("Ko-style Robust Gaussian Surprise analysis\n")
  cat("Version:", x$implementation_version, "\n")
  cat("Trains:", nrow(x$summary), "\n")
  cat("Accepted bursts:", nrow(x$bursts), "\n")
  cat("Accepted pauses:", nrow(x$pauses), "\n")
  invisible(x)
}


rgs_self_test <- function(verbose = TRUE) {
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(12092026)

  make_train <- function(shift = 0) {
    isi <- stats::rlnorm(420, meanlog = log(0.10), sdlog = 0.08)
    isi[100:105] <- c(0.003, 0.004, 0.0035, 0.004, 0.003, 0.004)
    isi[250:251] <- c(1.5, 0.9)
    c(shift, shift + cumsum(isi))
  }
  trains_s <- list(
    train_a = make_train(0),
    train_b = make_train(0.2),
    train_c = make_train(0.4)
  )
  groups <- stats::setNames(rep("test", length(trains_s)), names(trains_s))
  params <- rgs_default_parameters()
  params$min_burst_spikes <- 3L
  params$min_pause_spikes <- 2L
  result_s <- rgs_analyze(trains_s, groups, params, time_unit = "s")
  if (nrow(result_s$bursts) < 1L) {
    rgs_stop("Self-test failed: the injected short-ISI packet was not detected.")
  }
  if (nrow(result_s$pauses) < 1L) {
    rgs_stop("Self-test failed: the injected long-ISI pause was not detected.")
  }
  injected_detected <- function(events, start_isi, end_isi) {
    all(vapply(names(trains_s), function(train) {
      z <- events[events$train == train, , drop = FALSE]
      any(z$start_isi <= end_isi & z$end_isi >= start_isi)
    }, logical(1)))
  }
  if (!injected_detected(result_s$bursts, 100L, 105L)) {
    rgs_stop("Self-test failed: an injected burst was missed in at least one train.")
  }
  if (!injected_detected(result_s$pauses, 250L, 251L)) {
    rgs_stop("Self-test failed: an injected pause was missed in at least one train.")
  }
  if (any(result_s$bursts$n_spikes < params$min_burst_spikes)) {
    rgs_stop("Self-test failed: an accepted burst violates the size rule.")
  }
  if (any(result_s$pauses$n_spikes < params$min_pause_spikes)) {
    rgs_stop("Self-test failed: an accepted pause violates the size rule.")
  }

  trains_ms <- lapply(trains_s, function(x) 1000 * x)
  result_ms <- rgs_analyze(trains_ms, groups, params, time_unit = "ms")
  geometry <- function(x) {
    if (!nrow(x)) return(character())
    sort(paste(x$train, x$event_type, x$start_isi, x$end_isi, sep = ":"))
  }
  if (!identical(geometry(result_s$bursts), geometry(result_ms$bursts)) ||
      !identical(geometry(result_s$pauses), geometry(result_ms$pauses))) {
    rgs_stop("Self-test failed: second and millisecond inputs gave different event geometry.")
  }

  no_overlap <- function(x) {
    if (nrow(x) < 2L) return(TRUE)
    by_train <- split(x, x$train)
    all(vapply(by_train, function(z) {
      z <- z[order(z$start_isi, z$end_isi), , drop = FALSE]
      if (nrow(z) < 2L) return(TRUE)
      all(z$start_isi[-1L] > z$end_isi[-nrow(z)])
    }, logical(1)))
  }
  if (!no_overlap(result_s$bursts) || !no_overlap(result_s$pauses)) {
    rgs_stop("Self-test failed: accepted same-type events overlap.")
  }

  duplicate_map <- data.frame(
    Spike_train = c("train_a", "train_a", "train_b", "train_c"),
    Reference_group = rep("test", 4L),
    stringsAsFactors = FALSE
  )
  duplicate_rejected <- inherits(
    try(rgs_resolve_groups(names(trains_s), duplicate_map), silent = TRUE),
    "try-error"
  )
  if (!duplicate_rejected) {
    rgs_stop("Self-test failed: duplicate group-map rows were not rejected.")
  }
  duplicate_vector <- c(train_a = "test", train_a = "test",
                        train_b = "test", train_c = "test")
  duplicate_vector_rejected <- inherits(
    try(rgs_resolve_groups(names(trains_s), duplicate_vector), silent = TRUE),
    "try-error"
  )
  if (!duplicate_vector_rejected) {
    rgs_stop("Self-test failed: duplicate named-group entries were not rejected.")
  }

  if (isTRUE(verbose)) {
    message(
      "RGS self-test passed: ", nrow(result_s$bursts), " burst(s), ",
      nrow(result_s$pauses), " pause(s), with unit-invariant geometry."
    )
  }
  invisible(TRUE)
}


# STPD auxiliary-provider adapter ---------------------------------------------
# RGS remains an evidence provider.  These wrappers do not alter AUTO labels,
# detector thresholds, or candidate arbitration in the main STPD engine.

stpd_rgs_dataset_trains <- function(ds, selected_trains = NULL) {
  if (is.null(ds) || is.null(ds$trains) || !length(ds$trains)) {
    stop("Dataset has no trains.", call. = FALSE)
  }
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(as.character(trains), names(ds$trains))
  if (!length(trains)) stop("No selected trains exist in the dataset.", call. = FALSE)
  out <- lapply(trains, function(train) {
    dat <- ds$trains[[train]]
    if (!is.data.frame(dat) || !("timestamp_sec" %in% names(dat))) {
      stop("Each train must contain timestamp_sec.", call. = FALSE)
    }
    dat$timestamp_sec
  })
  names(out) <- trains
  out
}

stpd_rgs_default_parameters <- function() rgs_default_parameters()

stpd_rgs_subset_groups <- function(groups, trains) {
  if (is.null(groups)) return(stats::setNames(rep("all", length(trains)), trains))
  if (is.data.frame(groups)) {
    train_col <- intersect(c("Spike_train", "spike_train", "train"), names(groups))
    group_col <- intersect(c("Reference_group", "reference_group", "group"), names(groups))
    if (!length(train_col) || !length(group_col)) {
      stop("RGS group data must contain Spike_train and Reference_group columns.", call. = FALSE)
    }
    groups <- groups[groups[[train_col[1L]]] %in% trains, , drop = FALSE]
    return(rgs_resolve_groups(trains, groups))
  }
  if (is.null(names(groups))) {
    if (length(groups) == 1L) return(stats::setNames(rep(as.character(groups), length(trains)), trains))
    if (length(groups) == length(trains)) return(stats::setNames(as.character(groups), trains))
    stop("Unnamed RGS groups must be scalar or match the selected train count.", call. = FALSE)
  }
  rgs_resolve_groups(trains, groups[names(groups) %in% trains])
}

stpd_rgs_fit <- function(ds, selected_trains = NULL, groups = NULL,
                         params = stpd_rgs_default_parameters()) {
  spike_trains <- stpd_rgs_dataset_trains(ds, selected_trains)
  group_map <- stpd_rgs_subset_groups(groups, names(spike_trains))
  rgs_fit(spike_trains, groups = group_map, params = params, time_unit = "s")
}

stpd_rgs_add_event_intervals <- function(events) {
  if (!is.data.frame(events) || !nrow(events)) return(events)
  events <- events[order(events$train, events$start_time_sec, events$end_time_sec,
                         method = "radix"), , drop = FALSE]
  events$previous_event_id <- NA_character_
  events$inter_event_gap_sec <- NA_real_
  events$inter_event_onset_interval_sec <- NA_real_
  for (train in unique(events$train)) {
    idx <- which(events$train == train)
    if (length(idx) < 2L) next
    current <- idx[-1L]
    previous <- idx[-length(idx)]
    events$previous_event_id[current] <- events$event_id[previous]
    events$inter_event_gap_sec[current] <-
      events$start_time_sec[current] - events$end_time_sec[previous]
    events$inter_event_onset_interval_sec[current] <-
      events$start_time_sec[current] - events$start_time_sec[previous]
  }
  rownames(events) <- NULL
  events
}

stpd_rgs_standardize_events <- function(events) {
  events <- stpd_rgs_add_event_intervals(events)
  if (!is.data.frame(events) || !nrow(events)) return(events)
  events$method <- "robust_gaussian_surprise"
  events$start_spike <- events$start_spike_index
  events$end_spike <- events$end_spike_index
  events$mean_ISI_sec <- events$mean_isi_sec
  events$median_ISI_sec <- events$median_isi_sec
  events$min_ISI_sec <- events$min_isi_sec
  events$max_ISI_sec <- events$max_isi_sec
  events$pre_ISI_sec <- events$pre_event_isi_sec
  events$post_ISI_sec <- events$post_event_isi_sec
  events
}

stpd_rgs_threshold_table <- function(analysis) {
  refs <- analysis$fit$references
  if (!length(refs)) return(data.frame())
  rows <- lapply(names(refs), function(group) {
    ref <- refs[[group]]
    data.frame(
      reference_group = group,
      threshold_name = c(
        "central_mu", "central_sigma", "burst_seed_threshold",
        "pause_seed_threshold", "candidate_count_p", "familywise_alpha"
      ),
      threshold_value = c(ref$mu, ref$sigma, ref$burst_seed_threshold,
                          ref$pause_seed_threshold,
                          analysis$parameters$candidate_count_p,
                          analysis$parameters$familywise_alpha),
      threshold_unit = "normalized_log_ISI",
      threshold_source = c(rep("fitted_reference", 4L), rep("declared_parameter", 2L)),
      comparison_operator = c(
        "reported", "reported", "lt", "gt", "raw_p_lt_for_K", "adjusted_p_lt"
      ),
      support_role = "auxiliary_burst_pause_evidence_only",
      calibration_train_n = length(ref$training_trains),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

stpd_rgs_support_dataset <- function(
    ds,
    selected_trains = NULL,
    groups = NULL,
    params = stpd_rgs_default_parameters(),
    fit = NULL) {
  spike_trains <- stpd_rgs_dataset_trains(ds, selected_trains)
  group_map <- stpd_rgs_subset_groups(groups, names(spike_trains))
  calibration_mode <- if (is.null(fit)) "same_data_reference" else "frozen_reference_prediction"
  if (is.null(fit)) {
    analysis <- rgs_analyze(spike_trains, groups = group_map, params = params,
                            time_unit = "s")
  } else {
    analysis <- rgs_predict(fit, spike_trains, groups = group_map, time_unit = "s")
  }
  bursts <- stpd_rgs_standardize_events(analysis$bursts)
  pauses <- stpd_rgs_standardize_events(analysis$pauses)
  structure(
    list(
      method = "robust_gaussian_surprise",
      authority_scope = "support_evidence_only",
      calibration_mode = calibration_mode,
      scientific_status = analysis$scientific_status,
      scientific_reasons = analysis$scientific_reasons,
      analysis = analysis,
      bursts = bursts,
      pauses = pauses,
      burst_candidates = analysis$burst_candidates,
      pause_candidates = analysis$pause_candidates,
      isi_support = analysis$per_isi,
      summary = analysis$summary,
      thresholds = stpd_rgs_threshold_table(analysis),
      reference_diagnostics = analysis$fit$reference_diagnostics,
      seed_counts = analysis$seed_counts,
      qc = analysis$qc
    ),
    class = c("stpd_rgs_support", "list")
  )
}

stpd_rgs_support_export <- function(support, out_dir,
                                    include_full_analysis_rds = FALSE) {
  if (!is.list(support) || !identical(support$authority_scope, "support_evidence_only")) {
    stop("support must be an RGS auxiliary support result.", call. = FALSE)
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(
    RGS_thresholds.csv = support$thresholds,
    RGS_burst_features.csv = support$bursts,
    RGS_pause_features.csv = support$pauses,
    RGS_burst_candidates_audit.csv = support$burst_candidates,
    RGS_pause_candidates_audit.csv = support$pause_candidates,
    RGS_ISI_support.csv = support$isi_support,
    RGS_support_summary.csv = support$summary,
    RGS_reference_diagnostics.csv = support$reference_diagnostics,
    RGS_seed_candidate_counts.csv = support$seed_counts,
    RGS_QC.csv = support$qc,
    RGS_run_status.csv = data.frame(
      status = if (identical(support$scientific_status, "not_estimable")) {
        "not_estimable"
      } else if (nrow(support$bursts) == 0L && nrow(support$pauses) == 0L) {
        "zero_events"
      } else {
        "success"
      },
      scientific_status = support$scientific_status,
      scientific_reasons = paste(support$scientific_reasons, collapse = ";"),
      authority_scope = support$authority_scope,
      calibration_mode = support$calibration_mode,
      stringsAsFactors = FALSE
    )
  )
  for (filename in names(tables)) {
    table <- tables[[filename]]
    if (is.data.frame(table)) {
      write_csv_safe(table, file.path(out_dir, filename), row.names = FALSE,
                     fileEncoding = "UTF-8")
    }
  }
  saveRDS(rgs_compact_analysis(support$analysis), file.path(out_dir, "RGS_analysis.rds"))
  if (isTRUE(include_full_analysis_rds)) {
    saveRDS(support$analysis, file.path(out_dir, "RGS_analysis_full.rds"))
  }
  invisible(normalizePath(out_dir, winslash = "/", mustWork = TRUE))
}

# End of core RGS engine and STPD support adapter.
