# Event-aligned spike-train activity analysis.
#
# This module is deliberately upstream of neural manifold analysis: it answers
# the first-order neurophysiology questions about event-locked spiking, rate
# modulation, neuron-wise responses, and count synchrony before low-dimensional
# geometry is interpreted.

stpd_event_aligned_empty_result <- function(message = "No event-aligned activity can be computed.") {
  list(
    status = "empty",
    message = as.character(message %||% ""),
    raster = data.frame(),
    bins = data.frame(),
    psth = data.frame(),
    population = data.frame(),
    heatmap = data.frame(),
    correlation = data.frame(),
    correlogram = data.frame(),
    summary = data.frame(message = as.character(message %||% ""), stringsAsFactors = FALSE)
  )
}

stpd_event_aligned_bin_table <- function(pre_sec = 1, post_sec = 2, bin_sec = 0.05) {
  pre_sec <- suppressWarnings(as.numeric(pre_sec %||% 1))[1]
  post_sec <- suppressWarnings(as.numeric(post_sec %||% 2))[1]
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% 0.05))[1]
  if (!is.finite(pre_sec) || pre_sec < 0) pre_sec <- 1
  if (!is.finite(post_sec) || post_sec <= 0) post_sec <- 2
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
  span <- pre_sec + post_sec
  n_bins <- max(1L, ceiling(span / bin_sec))
  starts <- -pre_sec + (seq_len(n_bins) - 1L) * bin_sec
  ends <- pmin(starts + bin_sec, post_sec)
  data.frame(
    bin_id = seq_len(n_bins),
    rel_start_sec = starts,
    rel_end_sec = ends,
    rel_time_sec = (starts + ends) / 2,
    bin_width_sec = ends - starts,
    stringsAsFactors = FALSE
  )
}

stpd_event_aligned_sem <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  stats::sd(x) / sqrt(length(x))
}

stpd_event_aligned_smooth_matrix <- function(mat, sigma_bins = 0) {
  sigma_bins <- suppressWarnings(as.numeric(sigma_bins %||% 0))[1]
  if (!is.finite(sigma_bins) || sigma_bins <= 0 || ncol(mat) < 3L) return(mat)
  out <- mat
  for (ii in seq_len(nrow(mat))) {
    out[ii, ] <- stpd_state_trajectory_gaussian_smooth(mat[ii, ], sigma_bins)
  }
  out
}

stpd_event_aligned_bin_index <- function(rel, pre_sec, post_sec, bin_sec, n_bins) {
  rel <- suppressWarnings(as.numeric(rel))
  idx <- floor((rel + pre_sec) / bin_sec) + 1L
  idx[is.finite(rel) & abs(rel - post_sec) <= max(.Machine$double.eps * 128, 1e-12)] <- n_bins
  idx[!is.finite(rel) | rel < -pre_sec | rel > post_sec | idx < 1L | idx > n_bins] <- NA_integer_
  as.integer(idx)
}

stpd_event_aligned_activity <- function(trains,
                                        task_events,
                                        selected_trains = NULL,
                                        event_names = NULL,
                                        pre_sec = 1,
                                        post_sec = 2,
                                        bin_sec = 0.05,
                                        smoothing_sigma_bins = 1,
                                        baseline_start_sec = -1,
                                        baseline_end_sec = -0.2,
                                        label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                        min_isi_sec = 0.001,
                                        auto_others = FALSE,
                                        correlogram_lag_sec = 0.25,
                                        correlogram_bin_sec = NULL,
                                        max_correlogram_pairs = 30L) {
  label_source <- match.arg(label_source)
  if (is.null(trains) || length(trains) == 0L) {
    return(stpd_event_aligned_empty_result("No spike trains are loaded."))
  }
  selected_trains <- as.character(selected_trains %||% names(trains))
  selected_trains <- intersect(selected_trains, names(trains))
  if (length(selected_trains) == 0L) {
    return(stpd_event_aligned_empty_result("Select at least one spike train."))
  }
  events <- stpd_normalize_task_events(task_events %||% data.frame())
  if (nrow(events) == 0L) {
    return(stpd_event_aligned_empty_result("No task-event columns were found. Load a dataset with Event / Event_* columns."))
  }
  event_names_all <- sort(unique(as.character(events$event_name)))
  event_names <- as.character(event_names %||% event_names_all)
  event_names <- intersect(event_names, event_names_all)
  if (length(event_names) == 0L) event_names <- event_names_all
  events <- events[as.character(events$event_name) %in% event_names, , drop = FALSE]
  events <- events[order(events$event_time_sec, events$event_name), , drop = FALSE]
  if (nrow(events) == 0L) return(stpd_event_aligned_empty_result("No selected task events are available."))

  pre_sec <- suppressWarnings(as.numeric(pre_sec %||% 1))[1]
  post_sec <- suppressWarnings(as.numeric(post_sec %||% 2))[1]
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% 0.05))[1]
  smoothing_sigma_bins <- suppressWarnings(as.numeric(smoothing_sigma_bins %||% 0))[1]
  if (!is.finite(pre_sec) || pre_sec < 0) pre_sec <- 1
  if (!is.finite(post_sec) || post_sec <= 0) post_sec <- 2
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
  if (!is.finite(smoothing_sigma_bins) || smoothing_sigma_bins < 0) smoothing_sigma_bins <- 0

  bins <- stpd_event_aligned_bin_table(pre_sec, post_sec, bin_sec)
  n_events <- nrow(events)
  n_trains <- length(selected_trains)
  n_bins <- nrow(bins)
  counts <- array(0, dim = c(n_events, n_trains, n_bins),
                  dimnames = list(events$event_id, selected_trains, as.character(bins$bin_id)))
  raster_rows <- list()
  prepared <- vector("list", length(selected_trains))
  names(prepared) <- selected_trains

  for (tt in seq_along(selected_trains)) {
    tr <- selected_trains[tt]
    dat <- trains[[tr]]
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L || !("timestamp_sec" %in% names(dat))) next
    dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ok <- is.finite(ts)
    if (!any(ok)) next
    labels <- tryCatch(
      stpd_state_space_pattern_labels(dat, label_source = label_source, min_isi_sec = min_isi_sec, auto_others = auto_others),
      error = function(e) rep("unlabeled", nrow(dat))
    )
    labels <- rep_len(as.character(labels), length(ts))
    labels[is.na(labels) | !nzchar(labels)] <- "unlabeled"
    prepared[[tr]] <- list(dat = dat, ts = ts, labels = labels)

    for (ee in seq_len(n_events)) {
      ev_t <- suppressWarnings(as.numeric(events$event_time_sec[ee]))
      if (!is.finite(ev_t)) next
      rel <- ts - ev_t
      keep <- ok & rel >= -pre_sec & rel <= post_sec
      if (!any(keep)) next
      rel_keep <- rel[keep]
      idx_keep <- seq_along(ts)[keep]
      bin_idx <- stpd_event_aligned_bin_index(rel_keep, pre_sec, post_sec, bin_sec, n_bins)
      bin_idx <- bin_idx[is.finite(bin_idx)]
      if (length(bin_idx) > 0L) counts[ee, tt, ] <- counts[ee, tt, ] + tabulate(bin_idx, nbins = n_bins)
      row_id <- length(raster_rows) + 1L
      raster_rows[[row_id]] <- data.frame(
        event_index = ee,
        event_id = as.character(events$event_id[ee]),
        event_name = as.character(events$event_name[ee]),
        trial_id = as.character(events$trial_id[ee]),
        event_time_sec = ev_t,
        train = tr,
        train_index = tt,
        raster_row = (tt - 1L) * (n_events + 1L) + ee,
        rel_time_sec = rel_keep,
        timestamp_sec = ts[keep],
        idx = dat$idx[idx_keep] %||% idx_keep,
        pattern_label = labels[idx_keep],
        stringsAsFactors = FALSE
      )
    }
  }

  raster <- if (length(raster_rows) > 0L) do.call(rbind, raster_rows) else data.frame()
  rates <- counts
  for (bb in seq_len(n_bins)) rates[, , bb] <- counts[, , bb] / max(bins$bin_width_sec[bb], .Machine$double.eps)
  if (smoothing_sigma_bins > 0 && n_bins >= 3L) {
    for (tt in seq_len(n_trains)) {
      mat <- matrix(rates[, tt, ], nrow = n_events, ncol = n_bins)
      rates[, tt, ] <- stpd_event_aligned_smooth_matrix(mat, smoothing_sigma_bins)
    }
  }

  psth_rows <- list()
  for (tt in seq_along(selected_trains)) {
    tr <- selected_trains[tt]
    mat <- matrix(rates[, tt, ], nrow = n_events, ncol = n_bins)
    for (bb in seq_len(n_bins)) {
      vals <- mat[, bb]
      psth_rows[[length(psth_rows) + 1L]] <- data.frame(
        train = tr,
        train_index = tt,
        bin_id = bins$bin_id[bb],
        rel_time_sec = bins$rel_time_sec[bb],
        rel_start_sec = bins$rel_start_sec[bb],
        rel_end_sec = bins$rel_end_sec[bb],
        mean_rate_hz = mean(vals, na.rm = TRUE),
        sem_rate_hz = stpd_event_aligned_sem(vals),
        n_events = sum(is.finite(vals)),
        stringsAsFactors = FALSE
      )
    }
  }
  psth <- if (length(psth_rows) > 0L) do.call(rbind, psth_rows) else data.frame()

  pop_event_rate <- matrix(NA_real_, nrow = n_events, ncol = n_bins)
  for (ee in seq_len(n_events)) {
    for (bb in seq_len(n_bins)) pop_event_rate[ee, bb] <- mean(rates[ee, , bb], na.rm = TRUE)
  }
  population <- data.frame(
    bin_id = bins$bin_id,
    rel_time_sec = bins$rel_time_sec,
    rel_start_sec = bins$rel_start_sec,
    rel_end_sec = bins$rel_end_sec,
    mean_rate_hz = apply(pop_event_rate, 2, mean, na.rm = TRUE),
    sem_rate_hz = apply(pop_event_rate, 2, stpd_event_aligned_sem),
    n_events = apply(pop_event_rate, 2, function(x) sum(is.finite(x))),
    stringsAsFactors = FALSE
  )

  baseline_start_sec <- suppressWarnings(as.numeric(baseline_start_sec %||% -pre_sec))[1]
  baseline_end_sec <- suppressWarnings(as.numeric(baseline_end_sec %||% -0.2))[1]
  if (!is.finite(baseline_start_sec)) baseline_start_sec <- -pre_sec
  if (!is.finite(baseline_end_sec)) baseline_end_sec <- min(-bin_sec, 0)
  if (baseline_end_sec <= baseline_start_sec) {
    baseline_start_sec <- -pre_sec
    baseline_end_sec <- min(-bin_sec, 0)
  }
  base_bins <- which(bins$rel_time_sec >= baseline_start_sec & bins$rel_time_sec <= baseline_end_sec)
  if (length(base_bins) == 0L) base_bins <- which(bins$rel_time_sec < 0)
  if (length(base_bins) == 0L) base_bins <- seq_len(n_bins)

  heat_rows <- list()
  for (tt in seq_along(selected_trains)) {
    tr <- selected_trains[tt]
    mat <- matrix(rates[, tt, ], nrow = n_events, ncol = n_bins)
    base_vals <- as.numeric(mat[, base_bins, drop = FALSE])
    base_vals <- base_vals[is.finite(base_vals)]
    base_mu <- if (length(base_vals) > 0L) mean(base_vals, na.rm = TRUE) else 0
    base_sd <- if (length(base_vals) > 1L) stats::sd(base_vals, na.rm = TRUE) else NA_real_
    if (!is.finite(base_sd) || base_sd <= .Machine$double.eps) {
      all_vals <- as.numeric(mat)
      all_vals <- all_vals[is.finite(all_vals)]
      base_sd <- if (length(all_vals) > 1L) stats::sd(all_vals, na.rm = TRUE) else 1
    }
    if (!is.finite(base_sd) || base_sd <= .Machine$double.eps) base_sd <- 1
    mean_rate <- apply(mat, 2, mean, na.rm = TRUE)
    for (bb in seq_len(n_bins)) {
      heat_rows[[length(heat_rows) + 1L]] <- data.frame(
        train = tr,
        train_index = tt,
        bin_id = bins$bin_id[bb],
        rel_time_sec = bins$rel_time_sec[bb],
        mean_rate_hz = mean_rate[bb],
        baseline_mean_hz = base_mu,
        baseline_sd_hz = base_sd,
        z_rate = (mean_rate[bb] - base_mu) / base_sd,
        stringsAsFactors = FALSE
      )
    }
  }
  heatmap <- if (length(heat_rows) > 0L) do.call(rbind, heat_rows) else data.frame()

  obs <- matrix(NA_real_, nrow = n_events * n_bins, ncol = n_trains, dimnames = list(NULL, selected_trains))
  rr <- 1L
  for (ee in seq_len(n_events)) {
    for (bb in seq_len(n_bins)) {
      obs[rr, ] <- counts[ee, , bb]
      rr <- rr + 1L
    }
  }
  correlation <- data.frame()
  if (n_trains >= 2L && nrow(obs) >= 2L) {
    cm <- suppressWarnings(stats::cor(obs, use = "pairwise.complete.obs"))
    correlation <- do.call(rbind, lapply(seq_len(n_trains), function(ii) {
      data.frame(
        train_x = selected_trains[ii],
        train_y = selected_trains,
        correlation = as.numeric(cm[ii, ]),
        stringsAsFactors = FALSE
      )
    }))
  }

  correlogram <- stpd_event_aligned_correlogram_from_prepared(
    prepared = prepared,
    selected_trains = selected_trains,
    events = events,
    pre_sec = pre_sec,
    post_sec = post_sec,
    lag_sec = correlogram_lag_sec,
    lag_bin_sec = correlogram_bin_sec %||% bin_sec,
    max_pairs = max_correlogram_pairs
  )

  peak_row <- if (nrow(population) > 0L) population[which.max(population$mean_rate_hz), , drop = FALSE] else data.frame()
  summary <- data.frame(
    metric = c(
      "status", "event_names", "n_events", "n_trains", "bin_sec", "pre_sec", "post_sec",
      "baseline_start_sec", "baseline_end_sec", "smoothing_sigma_bins",
      "peak_population_rate_hz", "peak_population_rate_time_sec",
      "raster_spikes", "correlogram_pairs"
    ),
    value = c(
      "ok", paste(event_names, collapse = "; "), n_events, n_trains, bin_sec, pre_sec, post_sec,
      baseline_start_sec, baseline_end_sec, smoothing_sigma_bins,
      if (nrow(peak_row) > 0L) signif(peak_row$mean_rate_hz[1], 6) else NA,
      if (nrow(peak_row) > 0L) signif(peak_row$rel_time_sec[1], 6) else NA,
      nrow(raster), length(unique(correlogram$pair %||% character(0)))
    ),
    stringsAsFactors = FALSE
  )

  list(
    status = "ok",
    message = "",
    raster = raster,
    bins = bins,
    psth = psth,
    population = population,
    heatmap = heatmap,
    correlation = correlation,
    correlogram = correlogram,
    summary = summary,
    counts = counts,
    rates = rates,
    event_names = event_names,
    selected_trains = selected_trains,
    events = events,
    baseline_window = c(baseline_start_sec, baseline_end_sec),
    bin_sec = bin_sec,
    pre_sec = pre_sec,
    post_sec = post_sec,
    smoothing_sigma_bins = smoothing_sigma_bins,
    label_source = label_source
  )
}

stpd_event_aligned_correlogram_from_prepared <- function(prepared,
                                                        selected_trains,
                                                        events,
                                                        pre_sec,
                                                        post_sec,
                                                        lag_sec = 0.25,
                                                        lag_bin_sec = 0.05,
                                                        max_pairs = 30L) {
  if (length(selected_trains) < 2L || nrow(events) == 0L) return(data.frame())
  lag_sec <- suppressWarnings(as.numeric(lag_sec %||% 0.25))[1]
  lag_bin_sec <- suppressWarnings(as.numeric(lag_bin_sec %||% 0.05))[1]
  max_pairs <- suppressWarnings(as.integer(max_pairs %||% 30L))[1]
  if (!is.finite(lag_sec) || lag_sec <= 0) lag_sec <- 0.25
  if (!is.finite(lag_bin_sec) || lag_bin_sec <= 0) lag_bin_sec <- min(0.05, lag_sec)
  if (!is.finite(max_pairs) || max_pairs <= 0L) max_pairs <- 30L
  starts <- seq(-lag_sec, lag_sec, by = lag_bin_sec)
  if (tail(starts, 1) < lag_sec) starts <- c(starts, lag_sec)
  if (length(starts) < 2L) starts <- c(-lag_sec, lag_sec)
  lag_starts <- head(starts, -1L)
  lag_ends <- tail(starts, -1L)
  lag_mid <- (lag_starts + lag_ends) / 2
  pairs <- utils::combn(selected_trains, 2, simplify = FALSE)
  if (length(pairs) > max_pairs) pairs <- pairs[seq_len(max_pairs)]
  rows <- list()
  for (pp in pairs) {
    tr_a <- pp[1]
    tr_b <- pp[2]
    item_a <- prepared[[tr_a]]
    item_b <- prepared[[tr_b]]
    if (is.null(item_a) || is.null(item_b)) next
    counts <- numeric(length(lag_mid))
    for (ee in seq_len(nrow(events))) {
      ev_t <- suppressWarnings(as.numeric(events$event_time_sec[ee]))
      if (!is.finite(ev_t)) next
      a <- sort(item_a$ts[is.finite(item_a$ts) & (item_a$ts - ev_t) >= -pre_sec & (item_a$ts - ev_t) <= post_sec] - ev_t)
      b <- sort(item_b$ts[is.finite(item_b$ts) & (item_b$ts - ev_t) >= -pre_sec & (item_b$ts - ev_t) <= post_sec] - ev_t)
      if (length(a) == 0L || length(b) == 0L) next
      for (aa in a) {
        hit <- b[b >= aa - lag_sec & b <= aa + lag_sec] - aa
        if (length(hit) == 0L) next
        idx <- findInterval(hit, lag_starts, rightmost.closed = TRUE)
        idx <- idx[idx >= 1L & idx <= length(lag_mid)]
        if (length(idx) > 0L) counts <- counts + tabulate(idx, nbins = length(lag_mid))
      }
    }
    rows[[length(rows) + 1L]] <- data.frame(
      pair = paste(tr_a, tr_b, sep = " -> "),
      train_ref = tr_a,
      train_target = tr_b,
      lag_start_sec = lag_starts,
      lag_end_sec = lag_ends,
      lag_sec = lag_mid,
      count = counts,
      count_per_event = counts / max(1L, nrow(events)),
      rate_per_event_hz = counts / max(1L, nrow(events)) / pmax(lag_ends - lag_starts, .Machine$double.eps),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) data.frame() else do.call(rbind, rows)
}

stpd_event_aligned_empty_plot <- function(message) {
  plotly::layout(
    plotly::plot_ly(),
    annotations = list(list(
      x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      text = as.character(message %||% "No data available."),
      showarrow = FALSE
    )),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )
}

stpd_event_aligned_add_onset_shape <- function(p) {
  plotly::layout(
    p,
    shapes = list(list(
      type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, xref = "x", yref = "paper",
      line = list(color = "rgba(31,36,48,0.65)", width = 1, dash = "dash")
    ))
  )
}

stpd_event_aligned_raster_plot <- function(res, max_spikes = 5000L) {
  dat <- as.data.frame(res$raster %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) return(stpd_event_aligned_empty_plot(res$message %||% "No event-aligned spikes are available."))
  max_spikes <- suppressWarnings(as.integer(max_spikes %||% 5000L))[1]
  if (!is.finite(max_spikes) || max_spikes <= 0L) max_spikes <- 5000L
  if (nrow(dat) > max_spikes) {
    set.seed(1)
    dat <- dat[sort(sample(seq_len(nrow(dat)), max_spikes)), , drop = FALSE]
  }
  dat$y0 <- dat$raster_row - 0.35
  dat$y1 <- dat$raster_row + 0.35
  dat$hover <- paste0(
    "event: ", dat$event_name,
    "<br>trial: ", dat$trial_id,
    "<br>train: ", dat$train,
    "<br>t-event: ", signif(dat$rel_time_sec, 5), " s",
    "<br>label: ", dat$pattern_label
  )
  train_levels <- unique(dat[, c("train", "train_index"), drop = FALSE])
  train_levels <- train_levels[order(train_levels$train_index), , drop = FALSE]
  n_events <- length(unique(dat$event_id))
  tickvals <- (train_levels$train_index - 1L) * (n_events + 1L) + (n_events + 1L) / 2
  p <- plotly::plot_ly(source = "event_aligned_raster")
  p <- plotly::add_segments(
    p,
    data = dat,
    x = ~rel_time_sec, xend = ~rel_time_sec,
    y = ~y0, yend = ~y1,
    color = ~train,
    colors = "Set2",
    line = list(width = 1),
    text = ~hover,
    hoverinfo = "text",
    showlegend = FALSE
  )
  p <- stpd_event_aligned_add_onset_shape(p)
  plotly::layout(
    p,
    title = list(text = "Event-aligned raster", x = 0.02),
    xaxis = list(title = "Time from event onset (s)", zeroline = FALSE),
    yaxis = list(title = "Spike train / event trials", tickvals = tickvals, ticktext = train_levels$train, autorange = "reversed"),
    margin = list(l = 90, r = 20, t = 45, b = 55)
  )
}

stpd_event_aligned_psth_plot <- function(res) {
  dat <- as.data.frame(res$psth %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) return(stpd_event_aligned_empty_plot(res$message %||% "No PSTH can be computed."))
  dat$lower <- pmax(0, dat$mean_rate_hz - ifelse(is.finite(dat$sem_rate_hz), dat$sem_rate_hz, 0))
  dat$upper <- dat$mean_rate_hz + ifelse(is.finite(dat$sem_rate_hz), dat$sem_rate_hz, 0)
  trains <- unique(as.character(dat$train))
  p <- plotly::plot_ly(source = "event_aligned_psth")
  cols <- grDevices::hcl.colors(max(3L, length(trains)), "Dark 3")
  for (ii in seq_along(trains)) {
    sub <- dat[dat$train == trains[ii], , drop = FALSE]
    col <- cols[ii]
    p <- plotly::add_ribbons(
      p, data = sub, x = ~rel_time_sec, ymin = ~lower, ymax = ~upper,
      fillcolor = grDevices::adjustcolor(col, alpha.f = 0.12),
      line = list(color = "rgba(0,0,0,0)"),
      name = paste0(trains[ii], " SEM"),
      hoverinfo = "skip",
      showlegend = FALSE
    )
    p <- plotly::add_trace(
      p, data = sub, x = ~rel_time_sec, y = ~mean_rate_hz,
      type = "scatter", mode = "lines",
      line = list(color = col, width = 1.5),
      name = trains[ii],
      hovertemplate = paste0(trains[ii], "<br>t=%{x:.3f}s<br>rate=%{y:.3f} Hz<extra></extra>")
    )
  }
  p <- stpd_event_aligned_add_onset_shape(p)
  plotly::layout(
    p,
    title = list(text = "Peri-event firing rate / PSTH by neuron", x = 0.02),
    xaxis = list(title = "Time from event onset (s)", zeroline = FALSE),
    yaxis = list(title = "Firing rate (Hz)"),
    legend = list(orientation = "h", x = 0, y = 1.08),
    margin = list(l = 70, r = 20, t = 60, b = 55)
  )
}

stpd_event_aligned_population_plot <- function(res) {
  dat <- as.data.frame(res$population %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) return(stpd_event_aligned_empty_plot(res$message %||% "No population-rate summary can be computed."))
  dat$lower <- pmax(0, dat$mean_rate_hz - ifelse(is.finite(dat$sem_rate_hz), dat$sem_rate_hz, 0))
  dat$upper <- dat$mean_rate_hz + ifelse(is.finite(dat$sem_rate_hz), dat$sem_rate_hz, 0)
  p <- plotly::plot_ly(source = "event_aligned_population")
  p <- plotly::add_ribbons(
    p, data = dat, x = ~rel_time_sec, ymin = ~lower, ymax = ~upper,
    fillcolor = "rgba(113,180,54,0.16)",
    line = list(color = "rgba(0,0,0,0)"),
    hoverinfo = "skip",
    showlegend = FALSE
  )
  p <- plotly::add_trace(
    p, data = dat, x = ~rel_time_sec, y = ~mean_rate_hz,
    type = "scatter", mode = "lines+markers",
    line = list(color = "#71B436", width = 2),
    marker = list(size = 4, color = "#71B436"),
    name = "Population mean",
    hovertemplate = "t=%{x:.3f}s<br>mean rate=%{y:.3f} Hz/neuron<extra></extra>"
  )
  p <- stpd_event_aligned_add_onset_shape(p)
  plotly::layout(
    p,
    title = list(text = "Population mean firing rate +/- SEM", x = 0.02),
    xaxis = list(title = "Time from event onset (s)", zeroline = FALSE),
    yaxis = list(title = "Mean rate (Hz/neuron)"),
    margin = list(l = 70, r = 20, t = 45, b = 55)
  )
}

stpd_event_aligned_heatmap_plot <- function(res) {
  dat <- as.data.frame(res$heatmap %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) return(stpd_event_aligned_empty_plot(res$message %||% "No neuron heatmap can be computed."))
  trains <- unique(dat$train[order(dat$train_index)])
  times <- sort(unique(dat$rel_time_sec))
  z <- matrix(NA_real_, nrow = length(trains), ncol = length(times), dimnames = list(trains, times))
  for (ii in seq_len(nrow(dat))) {
    z[as.character(dat$train[ii]), as.character(dat$rel_time_sec[ii])] <- dat$z_rate[ii]
  }
  z_lim <- max(1, suppressWarnings(stats::quantile(abs(z), 0.98, na.rm = TRUE, names = FALSE)))
  p <- plotly::plot_ly(
    x = times,
    y = trains,
    z = z,
    type = "heatmap",
    zmin = -z_lim,
    zmax = z_lim,
    colorscale = list(c(0, "#2E4780"), c(0.5, "#FFFFFF"), c(1, "#CC6F47")),
    colorbar = list(title = "baseline z"),
    hovertemplate = "train=%{y}<br>t=%{x:.3f}s<br>z=%{z:.3f}<extra></extra>",
    source = "event_aligned_heatmap"
  )
  p <- stpd_event_aligned_add_onset_shape(p)
  plotly::layout(
    p,
    title = list(text = "Neuron-wise z-scored firing-rate heatmap", x = 0.02),
    xaxis = list(title = "Time from event onset (s)", zeroline = FALSE),
    yaxis = list(title = "Spike train / neuron"),
    margin = list(l = 110, r = 20, t = 45, b = 55)
  )
}

stpd_event_aligned_correlation_plot <- function(res) {
  dat <- as.data.frame(res$correlation %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) return(stpd_event_aligned_empty_plot("Select at least two trains to compute spike-count correlation."))
  trains <- unique(c(as.character(dat$train_x), as.character(dat$train_y)))
  z <- matrix(NA_real_, nrow = length(trains), ncol = length(trains), dimnames = list(trains, trains))
  for (ii in seq_len(nrow(dat))) z[dat$train_x[ii], dat$train_y[ii]] <- dat$correlation[ii]
  plotly::plot_ly(
    x = trains, y = trains, z = z,
    type = "heatmap",
    zmin = -1, zmax = 1,
    colorscale = list(c(0, "#2E4780"), c(0.5, "#FFFFFF"), c(1, "#BD569B")),
    colorbar = list(title = "r"),
    hovertemplate = "%{y} vs %{x}<br>r=%{z:.3f}<extra></extra>",
    source = "event_aligned_correlation"
  ) %>%
    plotly::layout(
      title = list(text = "Spike-count correlation across event bins", x = 0.02),
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      margin = list(l = 110, r = 20, t = 45, b = 80)
    )
}

stpd_event_aligned_correlogram_plot <- function(res) {
  dat <- as.data.frame(res$correlogram %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) return(stpd_event_aligned_empty_plot("Select at least two trains to compute cross-correlograms."))
  pairs <- unique(as.character(dat$pair))
  lags <- sort(unique(dat$lag_sec))
  z <- matrix(0, nrow = length(pairs), ncol = length(lags), dimnames = list(pairs, lags))
  for (ii in seq_len(nrow(dat))) z[as.character(dat$pair[ii]), as.character(dat$lag_sec[ii])] <- dat$count_per_event[ii]
  plotly::plot_ly(
    x = lags, y = pairs, z = z,
    type = "heatmap",
    colorscale = "Viridis",
    colorbar = list(title = "count/event"),
    hovertemplate = "pair=%{y}<br>lag=%{x:.4f}s<br>count/event=%{z:.3f}<extra></extra>",
    source = "event_aligned_correlogram"
  ) %>%
    plotly::layout(
      title = list(text = "Cross-correlogram around task events", x = 0.02),
      xaxis = list(title = "Lag target - reference (s)", zeroline = TRUE),
      yaxis = list(title = "Train pair"),
      margin = list(l = 160, r = 20, t = 45, b = 55)
    )
}
