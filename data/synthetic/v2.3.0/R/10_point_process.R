regularity_metrics <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n == 0L) return(c(mean = NA_real_, cv = NA_real_, cv2 = NA_real_, lv = NA_real_))
  mean_x <- mean(x)
  cv <- if (n > 1L && mean_x > 0) stats::sd(x) / mean_x else 0
  if (n > 1L) {
    denom <- x[-n] + x[-1L]
    delta <- x[-1L] - x[-n]
    cv2 <- mean(2 * abs(delta) / denom)
    lv <- mean(3 * (delta / denom)^2)
  } else {
    cv2 <- 0
    lv <- 0
  }
  c(mean = mean_x, cv = cv, cv2 = cv2, lv = lv)
}

rng_seed_from_key <- function(fields) {
  fields <- as.character(fields)
  if (!length(fields) || anyNA(fields) || any(!nzchar(fields)))
    stop("An RNG seed key contains a missing or empty field.", call. = FALSE)
  key <- paste(fields, collapse = "|")
  raw <- as.double(digest::digest2int(key, seed = 0L))
  modulus <- as.double(parameters$rng_contract$seed_modulus)
  if (!is.finite(modulus) || modulus < 1 || modulus > .Machine$integer.max)
    stop("The RNG seed modulus is outside R's integer seed range.", call. = FALSE)
  as.integer((abs(raw) %% modulus) + 1)
}

derive_stream_attempt_seed <- function(base_stream_seed, stream, attempt) {
  if (length(base_stream_seed) != 1L || !is.finite(base_stream_seed) ||
      base_stream_seed < 1 || base_stream_seed > parameters$rng_contract$seed_modulus)
    stop("The base RNG stream seed is invalid.", call. = FALSE)
  if (length(stream) != 1L || is.na(stream) || !stream %in% parameters$rng_contract$streams)
    stop("The RNG stream is not declared by the frozen contract.", call. = FALSE)
  if (length(attempt) != 1L || is.na(attempt) || attempt < 1 || attempt != as.integer(attempt))
    stop("The RNG attempt index must be a positive integer.", call. = FALSE)
  if (attempt == 1L && isTRUE(parameters$rng_contract$attempt_one_uses_base_stream_seed))
    return(as.integer(base_stream_seed))
  rng_seed_from_key(c(parameters$rng_contract$namespace, "attempt", as.integer(base_stream_seed),
                      stream, as.integer(attempt)))
}

shifted_gamma_isis <- function(n, mean_isi, shape, refractory = parameters$shared_refractory_B) {
  stopifnot(n >= 1L, mean_isi > refractory, shape > 0)
  refractory + stats::rgamma(n, shape = shape, scale = (mean_isi - refractory) / shape)
}

hazard_advance <- function(start_time, hazard_budget, base_rate, refractory,
                           pulse_start = Inf, pulse_end = Inf, multiplier = 1) {
  t <- start_time + refractory
  remaining <- hazard_budget
  boundaries <- sort(unique(c(t, pulse_start, pulse_end, Inf)))
  while (is.finite(remaining) && remaining > 0) {
    inside <- t >= pulse_start && t < pulse_end
    rate <- base_rate * if (inside) multiplier else 1
    future <- boundaries[boundaries > t + 1e-14]
    next_boundary <- if (length(future)) future[1] else Inf
    capacity <- rate * (next_boundary - t)
    if (!is.finite(capacity) || remaining <= capacity) return(t + remaining / rate)
    remaining <- remaining - capacity
    t <- next_boundary
  }
  t
}

simulate_piecewise_shifted_gamma <- function(duration, mean_isi, shape,
                                              pulse_start = Inf, pulse_end = Inf,
                                              multiplier = 1,
                                              refractory = parameters$shared_refractory_B) {
  stopifnot(duration > 0, mean_isi > refractory, shape > 0, multiplier >= 1)
  base_rate <- 1 / (mean_isi - refractory)
  times <- 0
  pulse_exposed <- FALSE
  while (tail(times, 1) < duration) {
    q <- stats::rgamma(1, shape = shape, scale = 1 / shape)
    next_time <- hazard_advance(tail(times, 1), q, base_rate, refractory,
                                pulse_start, pulse_end, multiplier)
    if (next_time > duration + 1e-12) break
    times <- c(times, next_time)
    pulse_exposed <- c(pulse_exposed, next_time >= pulse_start && next_time <= pulse_end)
  }
  list(times = times, isis = diff(times), pulse_exposed = pulse_exposed)
}

generate_fixed_count_run <- function(n_spikes, mean_isi, shape,
                                     refractory = parameters$shared_refractory_B) {
  stopifnot(n_spikes >= 2L)
  isis <- shifted_gamma_isis(n_spikes - 1L, mean_isi, shape, refractory)
  list(times = c(0, cumsum(isis)), isis = isis, pulse_exposed = rep(FALSE, n_spikes))
}

generate_pulse_run <- function(context = c("standalone", "hfs"), regime = NA_character_,
                               strength_stratum, target_spikes,
                               hfs_rate_overlap_stratum = "not_applicable", rng_seeds = NULL) {
  context <- match.arg(context)
  expected_streams <- parameters$rng_contract$pulse_streams[[context]]
  if (is.null(expected_streams) || anyDuplicated(expected_streams) ||
      !all(expected_streams %in% parameters$rng_contract$streams))
    stop("The pulse RNG stream contract is invalid.", call. = FALSE)
  if (!is.null(rng_seeds) && !setequal(names(rng_seeds), expected_streams))
    stop("Registered pulse RNG streams do not match the streams used by this context.", call. = FALSE)
  cfg <- parameters$burst_pulse
  contrast_range <- cfg$pulse_contrast_strata[[strength_stratum]]
  if (is.null(contrast_range)) stop("Unknown frozen pulse-strength stratum.", call. = FALSE)
  if (!target_spikes %in% c(cfg$boundary_realized_spikes, cfg$standard_realized_spikes)) {
    stop("Target Burst spike count is outside the frozen contract.", call. = FALSE)
  }
  for (attempt in seq_len(parameters$acceptance_contract$maximum_attempts_per_run)) {
    seeded <- function(stream, expr) {
      if (is.null(rng_seeds) || is.null(rng_seeds[[stream]])) return(force(expr))
      with_component_rng(derive_stream_attempt_seed(rng_seeds[[stream]], stream, attempt), force(expr))
    }
    from_unit <- function(u, range) range[1] + u * diff(range)
    # Draw each component's parameter vector once per attempt.  This preserves
    # independence between component streams without resetting a stream for
    # every scalar (which would create artificial within-component correlation).
    hfs_u <- seeded("hfs", stats::runif(6))
    nested_u <- seeded("nested_burst", stats::runif(4))
    target_contrast <- from_unit(nested_u[1], contrast_range)
    pre <- from_unit(hfs_u[1], cfg$pre_buffer_B)
    post <- from_unit(hfs_u[2], cfg$post_buffer_B)

    if (context == "hfs") {
      mean_range <- parameters$broad_hfs$rate_overlap_strata[[hfs_rate_overlap_stratum]]
      if (is.null(mean_range)) stop("Unknown frozen HFS rate-overlap stratum.", call. = FALSE)
      mean_isi <- from_unit(hfs_u[3], mean_range)
      shape_range <- parameters$broad_hfs$regularity_regimes[[regime]]
      shape <- from_unit(hfs_u[4], shape_range)
    } else {
      mean_isi <- from_unit(hfs_u[3], cfg$standalone_baseline_mean_isi_B)
      shape <- from_unit(hfs_u[4], cfg$standalone_baseline_shape)
    }
    pulse_target_mean_isi <- mean_isi / target_contrast
    denominator <- pulse_target_mean_isi - parameters$shared_refractory_B
    if (denominator <= 0) next
    multiplier <- (mean_isi - parameters$shared_refractory_B) / denominator
    pulse_mean_isi <- parameters$shared_refractory_B +
      (mean_isi - parameters$shared_refractory_B) / multiplier
    pulse_duration <- max(cfg$pulse_duration_B[1], min(cfg$pulse_duration_B[2],
      target_spikes * pulse_mean_isi * from_unit(nested_u[2], c(0.82, 1.18))))
    pulse_start <- pre
    pulse_end <- pre + pulse_duration
    duration <- pulse_end + post

    if (context == "hfs") {
      target_total <- seeded("hfs_total", sample(seq.int(parameters$broad_hfs$boundary_spikes[1], parameters$broad_hfs$boundary_spikes[2]), 1))
      # Compensate the observation window for the extra expected events created by
      # the pulse. This keeps the HFS envelope inside its frozen 20--35 spike
      # contract without altering the sampled pulse or inspecting detector output.
      pulse_compensation <- pulse_duration * (multiplier - 1)
      duration <- max((target_total - 1) * mean_isi + pulse_compensation,
                      parameters$broad_hfs$minimum_duration_B)
      pulse_start <- from_unit(nested_u[3], c(0.20 * duration, 0.58 * duration))
      pulse_end <- min(pulse_start + pulse_duration, 0.82 * duration)
      if (pulse_end <= pulse_start + 0.08) next
    }

    run <- seeded("point_process", simulate_piecewise_shifted_gamma(duration, mean_isi, shape,
                                                                      pulse_start, pulse_end, multiplier))
    exposed_idx <- which(run$pulse_exposed)
    if (length(exposed_idx) != target_spikes) next
    shoulder_n <- parameters$burst_pulse$minimum_observed_context_shoulders_each_side
    if ((min(exposed_idx) - 1L) < shoulder_n || (length(run$times) - max(exposed_idx)) < shoulder_n) next
    if (context == "hfs") {
      if (length(run$times) < parameters$broad_hfs$boundary_spikes[1] ||
          length(run$times) > parameters$broad_hfs$boundary_spikes[2]) next
      if (tail(run$times, 1) < parameters$broad_hfs$minimum_duration_B) next
      non_direct <- run$isis > parameters$broad_hfs$direct_support_max_isi_B
      if (any(run$isis > parameters$broad_hfs$tolerated_interruption_max_isi_B)) next
      if (mean(non_direct) > parameters$broad_hfs$maximum_tolerated_interruption_fraction + 1e-12) next
      if (any(rle(non_direct)$values & rle(non_direct)$lengths >
              parameters$broad_hfs$maximum_consecutive_tolerated_interruptions)) next
    }
    if (any(diff(run$times) < parameters$shared_refractory_B - 1e-12)) next
    return(c(run, list(
      duration = duration,
      mean_isi = mean_isi,
      shape = shape,
      regime = if (context == "hfs") regime else "standalone_background",
      pulse_start = pulse_start,
      pulse_end = pulse_end,
      multiplier = multiplier,
      target_mean_isi_contrast = target_contrast,
      pulse_refractory_limited = FALSE,
      latent_window_truncated = FALSE,
      pulse_strength_stratum = strength_stratum,
      hfs_rate_overlap_stratum = if (context == "hfs") hfs_rate_overlap_stratum else "not_applicable",
      frozen_target_spikes = target_spikes,
      exposed_indices = exposed_idx,
      attempts = attempt
    )))
  }
  stop(sprintf("Unable to generate pulse run: context=%s regime=%s overlap=%s strength=%s target_spikes=%s.",
               context, regime, hfs_rate_overlap_stratum, strength_stratum, target_spikes), call. = FALSE)
}

generate_hfs_run <- function(regime, rate_overlap_stratum) {
  cfg <- parameters$broad_hfs
  mean_range <- cfg$rate_overlap_strata[[rate_overlap_stratum]]
  if (is.null(mean_range)) stop("Unknown frozen HFS rate-overlap stratum.", call. = FALSE)
  for (attempt in seq_len(parameters$acceptance_contract$maximum_attempts_per_run)) {
    n_spikes <- sample(seq.int(cfg$boundary_spikes[1], cfg$boundary_spikes[2]), 1)
    mean_isi <- stats::runif(1, mean_range[1], mean_range[2])
    shape_range <- cfg$regularity_regimes[[regime]]
    shape <- stats::runif(1, shape_range[1], shape_range[2])
    run <- generate_fixed_count_run(n_spikes, mean_isi, shape)
    if (tail(run$times, 1) < cfg$minimum_duration_B) next
    non_direct <- run$isis > cfg$direct_support_max_isi_B
    if (any(run$isis > cfg$tolerated_interruption_max_isi_B)) next
    if (mean(non_direct) > cfg$maximum_tolerated_interruption_fraction + 1e-12) next
    rr <- rle(non_direct)
    if (any(rr$values & rr$lengths > cfg$maximum_consecutive_tolerated_interruptions)) next
    return(c(run, list(duration = tail(run$times, 1), mean_isi = mean_isi,
                       shape = shape, regime = regime,
                       hfs_rate_overlap_stratum = rate_overlap_stratum, attempts = attempt)))
  }
  stop("Unable to generate an HFS state under the structural contract.", call. = FALSE)
}

generate_tonic_run <- function(subtype = c("generic_stress", "stn_like_empirical"),
                               intended_stratum = c("eligible", "ambiguous", "no_evidence"),
                               rate_overlap_stratum = c("core", "boundary", "deep")) {
  subtype <- match.arg(subtype)
  intended_stratum <- match.arg(intended_stratum)
  rate_overlap_stratum <- match.arg(rate_overlap_stratum)
  regime_cfg <- parameters$tonic$regimes[[intended_stratum]]
  cfg <- regime_cfg[[subtype]]
  mean_range <- parameters$tonic$rate_overlap_mean_isi_B[[subtype]][[rate_overlap_stratum]]
  if (is.null(mean_range)) stop("Unknown frozen Tonic rate-overlap stratum.", call. = FALSE)
  for (attempt in seq_len(parameters$acceptance_contract$maximum_attempts_per_run)) {
    n_spikes <- sample(seq.int(regime_cfg$boundary_spikes[1], regime_cfg$boundary_spikes[2]), 1)
    mean_isi <- stats::runif(1, mean_range[1], mean_range[2])
    shape <- stats::runif(1, cfg$gamma_shape[1], cfg$gamma_shape[2])
    run <- generate_fixed_count_run(n_spikes, mean_isi, shape)
    metrics <- regularity_metrics(run$isis)
    if (metrics["cv"] > cfg$cv_qc_max || metrics["cv2"] > cfg$cv2_qc_max || metrics["lv"] > cfg$lv_qc_max) next
    return(c(run, list(duration = tail(run$times, 1), mean_isi = mean_isi,
                       shape = shape, regime = subtype, tonic_subtype = subtype,
                       tonic_intended_stratum = intended_stratum,
                       tonic_rate_overlap_stratum = rate_overlap_stratum,
                       metrics = metrics, attempts = attempt)))
  }
  stop("Unable to generate a Tonic state under the subtype contract.", call. = FALSE)
}

generate_background_run <- function() {
  cfg <- parameters$background
  n_spikes <- sample(seq.int(cfg$boundary_spikes[1], cfg$boundary_spikes[2]), 1)
  mean_isi <- stats::runif(1, cfg$mean_isi_B[1], cfg$mean_isi_B[2])
  shape <- stats::runif(1, cfg$gamma_shape[1], cfg$gamma_shape[2])
  run <- generate_fixed_count_run(n_spikes, mean_isi, shape)
  c(run, list(duration = tail(run$times, 1), mean_isi = mean_isi,
              shape = shape, regime = "background", attempts = 1L))
}

generate_complex_pause_run <- function() {
  cfg <- parameters$complex_pause
  for (attempt in seq_len(parameters$acceptance_contract$maximum_attempts_per_run)) {
    n_isi <- sample(seq.int(cfg$isi_count[1], cfg$isi_count[2]), 1)
    isis <- stats::runif(n_isi, cfg$component_gap_B[1], cfg$component_gap_B[2])
    duration <- sum(isis)
    if (duration < cfg$minimum_total_duration_B || duration > cfg$maximum_total_duration_B) next
    return(list(times = c(0, cumsum(isis)), isis = isis,
                pulse_exposed = rep(FALSE, n_isi + 1L), mean_isi = mean(isis),
                shape = NA_real_, regime = "complex_multi_gap", duration = duration,
                attempts = attempt))
  }
  stop("Unable to generate a complex Pause under the frozen contract.", call. = FALSE)
}
