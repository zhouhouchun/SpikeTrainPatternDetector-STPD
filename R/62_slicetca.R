# Optional sliceTCA backend for trialized neural tensors.
#
# The R package builds auditable trial x neuron x time tensors and calls the
# official Python slicetca package only when the user's Python environment has
# numpy, torch and slicetca available.

stpd_slicetca_rank_parse <- function(x, default = c(2L, 0L, 2L)) {
  vals <- suppressWarnings(as.integer(strsplit(gsub("[;\\s]+", ",", as.character(x %||% "")), ",")[[1]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) < 3L) vals <- default
  vals <- vals[seq_len(3L)]
  vals[!is.finite(vals) | vals < 0L] <- 0L
  as.integer(vals)
}

stpd_slicetca_backend_packages <- function() {
  c("numpy", "torch", "slicetca")
}

stpd_slicetca_backend_status <- function(initialize = FALSE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(data.frame(
      backend = "python_slicetca",
      status = "missing_reticulate",
      detail = "Install the R package 'reticulate' to enable the optional Python backend.",
      stringsAsFactors = FALSE
    ))
  }
  if (!isTRUE(initialize) && !isTRUE(reticulate::py_available(initialize = FALSE))) {
    return(data.frame(
      backend = "python_slicetca",
      status = "not_initialized",
      detail = "Python has not been initialized; call stpd_install_slicetca_backend() or run the sliceTCA backend to check modules.",
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(stpd_slicetca_backend_packages(), function(pkg) {
    ok <- tryCatch(reticulate::py_module_available(pkg), error = function(e) FALSE)
    data.frame(
      backend = "python_slicetca",
      status = if (isTRUE(ok)) "available" else "missing_python_module",
      detail = pkg,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

stpd_install_slicetca_backend <- function(envname = "stpd-slicetca",
                                          packages = stpd_slicetca_backend_packages(),
                                          method = c("virtualenv", "conda"),
                                          conda = "auto") {
  method <- match.arg(method)
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Install the R package 'reticulate' first.", call. = FALSE)
  }
  packages <- unique(as.character(packages %||% stpd_slicetca_backend_packages()))
  packages <- packages[nzchar(packages)]
  if (identical(method, "virtualenv")) {
    if (!(envname %in% reticulate::virtualenv_list())) {
      reticulate::virtualenv_create(envname)
    }
    reticulate::virtualenv_install(envname, packages = packages, ignore_installed = TRUE)
    reticulate::use_virtualenv(envname, required = TRUE)
  } else {
    if (!(envname %in% reticulate::conda_list()$name)) {
      reticulate::conda_create(envname, packages = "python", conda = conda)
    }
    reticulate::conda_install(envname, packages = packages, pip = TRUE, conda = conda)
    reticulate::use_condaenv(envname, required = TRUE)
  }
  stpd_slicetca_backend_status(initialize = TRUE)
}

stpd_slicetca_empty_result <- function(message = "No sliceTCA tensor is available.") {
  list(
    status = "empty",
    message = message,
    tensor = NULL,
    reconstruction = NULL,
    diagnostics = data.frame(message = message, stringsAsFactors = FALSE),
    tensor_summary = data.frame(message = message, stringsAsFactors = FALSE),
    reconstruction_metrics = data.frame(message = message, stringsAsFactors = FALSE),
    trial_embedding = data.frame(),
    reconstructed_embedding = data.frame(),
    event_annotation = data.frame()
  )
}

stpd_slicetca_transform_counts <- function(counts,
                                           bin_sec,
                                           transform = c("sqrt_count", "log1p_rate", "rate", "count")) {
  transform <- match.arg(transform)
  rates <- counts / pmax(bin_sec, .Machine$double.eps)
  switch(
    transform,
    count = counts,
    rate = rates,
    log1p_rate = log1p(pmax(rates, 0)),
    sqrt_count = sqrt(pmax(counts, 0) + 0.375)
  )
}

stpd_slicetca_scale_tensor <- function(X, scaling = c("zscore", "robust", "none")) {
  scaling <- match.arg(scaling)
  if (identical(scaling, "none")) return(X)
  dims <- dim(X)
  if (length(dims) != 3L || any(dims <= 0L)) return(X)
  for (jj in seq_len(dims[2])) {
    x <- as.numeric(X[, jj, ])
    ok <- is.finite(x)
    if (!any(ok)) {
      X[, jj, ] <- 0
      next
    }
    if (identical(scaling, "robust")) {
      center <- stats::median(x[ok], na.rm = TRUE)
      scale <- stats::mad(x[ok], center = center, constant = 1.4826, na.rm = TRUE)
    } else {
      center <- mean(x[ok], na.rm = TRUE)
      scale <- stats::sd(x[ok], na.rm = TRUE)
    }
    if (!is.finite(scale) || scale <= 0) scale <- 1
    x[!ok] <- center
    X[, jj, ] <- (x - center) / scale
  }
  X
}

stpd_slicetca_prepare_trains <- function(trains, selected_trains, time_origin = c("raw", "aligned")) {
  time_origin <- match.arg(time_origin)
  selected_trains <- as.character(selected_trains %||% names(trains))
  selected_trains <- intersect(selected_trains, names(trains))
  lapply(selected_trains, function(tr) {
    dat <- trains[[tr]]
    if (is.null(dat) || !is.data.frame(dat) || !("timestamp_sec" %in% names(dat))) return(NULL)
    dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ok <- is.finite(ts)
    if (sum(ok) < 2L) return(NULL)
    if (identical(time_origin, "aligned")) ts <- ts - min(ts[ok], na.rm = TRUE)
    list(train = tr, dat = dat, ts = ts)
  })
}

stpd_slicetca_trial_events <- function(events,
                                       event_time_col,
                                       trial_id_col = NULL,
                                       condition_col = NULL) {
  if (is.null(events) || !is.data.frame(events) || nrow(events) == 0L) return(data.frame())
  event_time_col <- as.character(event_time_col %||% "")[1]
  trial_id_col <- as.character(trial_id_col %||% "")[1]
  condition_col <- as.character(condition_col %||% "")[1]
  if (!(event_time_col %in% names(events))) return(data.frame())
  tt <- suppressWarnings(as.numeric(events[[event_time_col]]))
  ok <- is.finite(tt)
  if (!any(ok)) return(data.frame())
  events <- events[ok, , drop = FALSE]
  tt <- tt[ok]
  ord <- order(tt)
  events <- events[ord, , drop = FALSE]
  tt <- tt[ord]
  trial_id <- if (nzchar(trial_id_col) && trial_id_col %in% names(events)) as.character(events[[trial_id_col]]) else paste0("trial_", seq_along(tt))
  trial_id[is.na(trial_id) | !nzchar(trial_id)] <- paste0("trial_", which(is.na(trial_id) | !nzchar(trial_id)))
  condition <- if (nzchar(condition_col) && condition_col %in% names(events)) as.character(events[[condition_col]]) else "trial"
  condition[is.na(condition) | !nzchar(condition)] <- "trial"
  data.frame(
    trial_index = seq_along(tt),
    trial_id = make.unique(trial_id, sep = "_"),
    event_time_sec = tt,
    condition = condition,
    stringsAsFactors = FALSE
  )
}

stpd_slicetca_event_annotation <- function(prepared,
                                           trial_events,
                                           rel_bins,
                                           label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                           min_isi_sec = 0.001,
                                           auto_others = FALSE) {
  label_source <- match.arg(label_source)
  states <- stpd_state_trajectory_state_levels()
  groups <- stpd_state_trajectory_pattern_groups()
  n_trials <- nrow(trial_events)
  n_bins <- nrow(rel_bins)
  if (n_trials == 0L || n_bins == 0L || length(prepared) == 0L) return(data.frame())
  frac <- array(0, dim = c(n_trials, n_bins, length(states)), dimnames = list(NULL, NULL, states))
  rate <- array(0, dim = c(n_trials, n_bins, length(states)), dimnames = list(NULL, NULL, states))
  n_trains <- 0L
  for (item in prepared) {
    if (is.null(item)) next
    n_trains <- n_trains + 1L
    dat <- item$dat
    ts <- item$ts
    labels <- stpd_state_space_pattern_labels(
      dat,
      label_source = label_source,
      min_isi_sec = min_isi_sec,
      auto_others = auto_others
    )
    labels <- rep_len(as.character(labels), length(ts))
    labels[is.na(labels) | !nzchar(labels)] <- "unlabeled"
    for (rr in seq_len(n_trials)) {
      t0 <- trial_events$event_time_sec[rr]
      for (bb in seq_len(n_bins)) {
        b0 <- t0 + rel_bins$rel_start_sec[bb]
        b1 <- t0 + rel_bins$rel_end_sec[bb]
        bw <- rel_bins$bin_width_sec[bb]
        spike_idx <- which(is.finite(ts) & ts >= b0 & ts < b1)
        if (bb == n_bins) spike_idx <- which(is.finite(ts) & ts >= b0 & ts <= b1)
        if (length(spike_idx) > 0L) {
          lab_sp <- labels[spike_idx]
          for (st in states) rate[rr, bb, st] <- rate[rr, bb, st] + sum(lab_sp %in% groups[[st]], na.rm = TRUE) / pmax(bw, .Machine$double.eps)
        }
        if (length(ts) >= 2L) {
          for (ii in seq.int(2L, length(ts))) {
            if (!is.finite(ts[ii - 1L]) || !is.finite(ts[ii]) || ts[ii] <= ts[ii - 1L]) next
            ov <- stpd_state_trajectory_interval_overlap(ts[ii - 1L], ts[ii], b0, b1)
            if (!is.finite(ov) || ov <= 0) next
            lab <- labels[ii]
            for (st in states) if (lab %in% groups[[st]]) frac[rr, bb, st] <- frac[rr, bb, st] + ov / pmax(bw, .Machine$double.eps)
          }
        }
      }
    }
  }
  denom <- max(1L, n_trains)
  frac <- frac / denom
  frac[!is.finite(frac)] <- 0
  frac[frac < 0] <- 0
  frac[frac > 1] <- 1
  rate <- rate / denom
  rate[!is.finite(rate)] <- 0
  rows <- vector("list", n_trials * n_bins)
  kk <- 0L
  for (rr in seq_len(n_trials)) {
    for (bb in seq_len(n_bins)) {
      kk <- kk + 1L
      score <- frac[rr, bb, ]
      score[!is.finite(score)] <- 0
      dom <- states[which.max(score)]
      if (sum(score, na.rm = TRUE) <= 0) dom <- "unlabeled"
      row <- data.frame(
        trial_index = rr,
        trial_id = trial_events$trial_id[rr],
        condition = trial_events$condition[rr],
        rel_bin = bb,
        rel_time_sec = rel_bins$rel_mid_sec[bb],
        event_state = dom,
        stringsAsFactors = FALSE
      )
      for (st in states) {
        row[[paste0(st, "_fraction")]] <- frac[rr, bb, st]
        row[[paste0(st, "_rate_hz")]] <- rate[rr, bb, st]
      }
      rows[[kk]] <- row
    }
  }
  do.call(rbind, rows)
}

stpd_make_slicetca_trial_tensor <- function(trains,
                                            selected_trains = NULL,
                                            trial_events,
                                            event_time_col,
                                            trial_id_col = NULL,
                                            condition_col = NULL,
                                            pre_sec = 0.5,
                                            post_sec = 1.0,
                                            bin_sec = 0.02,
                                            time_origin = c("raw", "aligned"),
                                            transform = c("sqrt_count", "log1p_rate", "rate", "count"),
                                            scaling = c("zscore", "robust", "none"),
                                            smoothing_sigma_bins = 0,
                                            label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                            min_isi_sec = 0.001,
                                            auto_others = FALSE) {
  time_origin <- match.arg(time_origin)
  transform <- match.arg(transform)
  scaling <- match.arg(scaling)
  label_source <- match.arg(label_source)
  if (is.null(trains) || length(trains) == 0L) return(stpd_slicetca_empty_result("No spike trains are loaded."))
  trial_events <- stpd_slicetca_trial_events(trial_events, event_time_col, trial_id_col, condition_col)
  if (nrow(trial_events) < 2L) return(stpd_slicetca_empty_result("Need at least two trial/event times for sliceTCA."))
  selected_trains <- as.character(selected_trains %||% names(trains))
  selected_trains <- intersect(selected_trains, names(trains))
  if (length(selected_trains) < 2L) return(stpd_slicetca_empty_result("Need at least two selected neurons for sliceTCA."))
  pre_sec <- suppressWarnings(as.numeric(pre_sec %||% 0.5))[1]
  post_sec <- suppressWarnings(as.numeric(post_sec %||% 1.0))[1]
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% 0.02))[1]
  if (!is.finite(pre_sec) || pre_sec < 0) pre_sec <- 0.5
  if (!is.finite(post_sec) || post_sec <= 0) post_sec <- 1.0
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.02
  n_bins <- max(2L, ceiling((pre_sec + post_sec) / bin_sec))
  starts <- -pre_sec + (seq_len(n_bins) - 1L) * bin_sec
  ends <- pmin(starts + bin_sec, post_sec)
  rel_bins <- data.frame(
    rel_bin = seq_len(n_bins),
    rel_start_sec = starts,
    rel_end_sec = ends,
    rel_mid_sec = (starts + ends) / 2,
    bin_width_sec = ends - starts,
    stringsAsFactors = FALSE
  )
  prepared <- stpd_slicetca_prepare_trains(trains, selected_trains, time_origin = time_origin)
  prepared <- prepared[!vapply(prepared, is.null, logical(1))]
  if (length(prepared) < 2L) return(stpd_slicetca_empty_result("Need at least two valid spike trains with timestamps."))
  train_names <- vapply(prepared, function(x) x$train, character(1))
  n_trials <- nrow(trial_events)
  counts <- array(0, dim = c(n_trials, length(prepared), n_bins),
                  dimnames = list(trial_events$trial_id, train_names, paste0("bin_", seq_len(n_bins))))
  for (jj in seq_along(prepared)) {
    ts <- prepared[[jj]]$ts
    for (rr in seq_len(n_trials)) {
      t0 <- trial_events$event_time_sec[rr]
      for (bb in seq_len(n_bins)) {
        b0 <- t0 + rel_bins$rel_start_sec[bb]
        b1 <- t0 + rel_bins$rel_end_sec[bb]
        if (bb == n_bins) {
          counts[rr, jj, bb] <- sum(is.finite(ts) & ts >= b0 & ts <= b1)
        } else {
          counts[rr, jj, bb] <- sum(is.finite(ts) & ts >= b0 & ts < b1)
        }
      }
    }
  }
  X <- stpd_slicetca_transform_counts(counts, bin_sec = bin_sec, transform = transform)
  smoothing_sigma_bins <- suppressWarnings(as.numeric(smoothing_sigma_bins %||% 0))[1]
  if (is.finite(smoothing_sigma_bins) && smoothing_sigma_bins > 0 && n_bins >= 3L) {
    for (rr in seq_len(n_trials)) for (jj in seq_along(prepared)) {
      X[rr, jj, ] <- stpd_state_trajectory_gaussian_smooth(X[rr, jj, ], smoothing_sigma_bins)
    }
  }
  X_scaled <- stpd_slicetca_scale_tensor(X, scaling = scaling)
  event_annotation <- stpd_slicetca_event_annotation(
    prepared,
    trial_events,
    rel_bins,
    label_source = label_source,
    min_isi_sec = min_isi_sec,
    auto_others = auto_others
  )
  tensor_summary <- data.frame(
    n_trials = n_trials,
    n_neurons = length(prepared),
    n_time_bins = n_bins,
    bin_sec = bin_sec,
    pre_sec = pre_sec,
    post_sec = post_sec,
    transform = transform,
    scaling = scaling,
    smoothing_sigma_bins = smoothing_sigma_bins,
    time_origin = time_origin,
    label_source = label_source,
    tensor_shape = paste(n_trials, length(prepared), n_bins, sep = " x "),
    stringsAsFactors = FALSE
  )
  list(
    status = "ready",
    tensor = X_scaled,
    tensor_raw = X,
    counts = counts,
    trial_events = trial_events,
    rel_bins = rel_bins,
    train_names = train_names,
    event_annotation = event_annotation,
    tensor_summary = tensor_summary
  )
}

stpd_slicetca_tensor_embedding <- function(tensor_res, reconstruction = NULL, prefix = "TensorPC") {
  X <- reconstruction %||% tensor_res$tensor
  if (is.null(X) || length(dim(X)) != 3L) return(data.frame())
  dims <- dim(X)
  if (dims[1] < 1L || dims[2] < 2L || dims[3] < 2L) return(data.frame())
  obs <- matrix(NA_real_, nrow = dims[1] * dims[3], ncol = dims[2])
  meta <- vector("list", dims[1] * dims[3])
  kk <- 0L
  for (rr in seq_len(dims[1])) {
    for (bb in seq_len(dims[3])) {
      kk <- kk + 1L
      obs[kk, ] <- X[rr, , bb]
      meta[[kk]] <- data.frame(
        trial_index = rr,
        trial_id = tensor_res$trial_events$trial_id[rr],
        condition = tensor_res$trial_events$condition[rr],
        rel_bin = bb,
        rel_time_sec = tensor_res$rel_bins$rel_mid_sec[bb],
        stringsAsFactors = FALSE
      )
    }
  }
  obs <- stpd_neural_scale_matrix(obs, scaling = "zscore")
  fit <- tryCatch(stats::prcomp(obs, center = FALSE, scale. = FALSE), error = function(e) NULL)
  if (is.null(fit)) return(data.frame())
  coords <- stpd_neural_take3(fit$x, prefix)
  names(coords) <- c("TC1", "TC2", "TC3")
  out <- cbind(do.call(rbind, meta), coords)
  ann <- tensor_res$event_annotation %||% data.frame()
  if (is.data.frame(ann) && nrow(ann) > 0L) {
    keep <- intersect(c("trial_index", "rel_bin", "event_state", "burst_fraction", "pause_fraction", "tonic_fraction"), names(ann))
    out <- merge(out, ann[, keep, drop = FALSE], by = c("trial_index", "rel_bin"), all.x = TRUE, sort = FALSE)
  }
  out[order(out$trial_index, out$rel_bin), , drop = FALSE]
}

stpd_slicetca_reconstruction_metrics <- function(tensor_res, reconstruction = NULL) {
  X <- tensor_res$tensor
  if (is.null(X) || is.null(reconstruction)) {
    return(data.frame(metric = "slicetca_reconstruction", value = NA_real_, status = "not_run",
                      note = "Python sliceTCA backend has not produced a reconstruction.", stringsAsFactors = FALSE))
  }
  if (!identical(dim(X), dim(reconstruction))) {
    return(data.frame(metric = "slicetca_reconstruction", value = NA_real_, status = "failed",
                      note = "Reconstruction shape does not match tensor shape.", stringsAsFactors = FALSE))
  }
  err <- as.numeric(X - reconstruction)
  obs <- as.numeric(X)
  mse <- mean(err^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  r2 <- 1 - sum(err^2, na.rm = TRUE) / pmax(sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE), .Machine$double.eps)
  data.frame(
    metric = c("slicetca_reconstruction_mse", "slicetca_reconstruction_rmse", "slicetca_reconstruction_r2"),
    value = c(mse, rmse, r2),
    status = "ok",
    note = "Reconstruction quality on the fitted tensor; use held-out block CV for publication-grade model selection.",
    stringsAsFactors = FALSE
  )
}

stpd_run_slicetca_backend <- function(tensor_res,
                                      ranks = c(2L, 0L, 2L),
                                      run_python = FALSE,
                                      seed = 1L,
                                      max_iter = 1000L,
                                      learning_rate = 0.005,
                                      positive = FALSE,
                                      apply_invariance = TRUE) {
  if (is.null(tensor_res) || !identical(tensor_res$status, "ready")) return(stpd_slicetca_empty_result("sliceTCA tensor is not ready."))
  ranks <- as.integer(ranks %||% c(2L, 0L, 2L))[seq_len(3L)]
  ranks[!is.finite(ranks) | ranks < 0L] <- 0L
  seed <- safe_int(seed, 1L)
  max_iter <- max(1L, safe_int(max_iter, 1000L))
  learning_rate <- suppressWarnings(as.numeric(learning_rate %||% 0.005))[1]
  if (!is.finite(learning_rate) || learning_rate <= 0) learning_rate <- 0.005
  raw_embedding <- stpd_slicetca_tensor_embedding(tensor_res)
  diagnostics <- data.frame(
    backend = "python_slicetca",
    metric = c("requested_ranks_trial_neuron_time", "run_python"),
    value = c(paste(ranks, collapse = ","), as.character(isTRUE(run_python))),
    status = "reported",
    note = c("Ranks are ordered as trial-slicing, neuron-slicing, time-slicing components.", ""),
    stringsAsFactors = FALSE
  )
  reconstruction <- NULL
  components <- NULL
  model <- NULL
  if (!isTRUE(run_python)) {
    diagnostics <- rbind(diagnostics, data.frame(
      backend = "python_slicetca", metric = "status", value = "not_run", status = "not_run",
      note = "Enable 'Run Python sliceTCA backend' after installing numpy, torch and slicetca.",
      stringsAsFactors = FALSE
    ))
  } else {
    py_result <- tryCatch({
      if (!requireNamespace("reticulate", quietly = TRUE)) stop("R package 'reticulate' is not installed.", call. = FALSE)
      missing <- stpd_slicetca_backend_status(initialize = TRUE)
      miss <- missing[missing$status != "available", , drop = FALSE]
      if (nrow(miss) > 0L) stop(paste0("Missing Python modules: ", paste(miss$detail, collapse = ", ")), call. = FALSE)
      slicetca <- reticulate::import("slicetca", delay_load = FALSE)
      torch <- reticulate::import("torch", delay_load = FALSE)
      np <- reticulate::import("numpy", delay_load = FALSE)
      device <- if (isTRUE(torch$cuda$is_available())) "cuda" else "cpu"
      data_py <- torch$tensor(np$array(tensor_res$tensor, dtype = "float32"), dtype = torch$float32, device = device)
      res <- slicetca$decompose(
        data_py,
        do.call(reticulate::tuple, as.list(as.integer(ranks))),
        positive = isTRUE(positive),
        learning_rate = learning_rate,
        max_iter = as.integer(max_iter),
        seed = as.integer(seed),
        progress_bar = FALSE,
        verbose = FALSE
      )
      comps <- res[[1]]
      mdl <- res[[2]]
      if (isTRUE(apply_invariance)) {
        mdl <- tryCatch(slicetca$invariance(mdl, progress_bar = FALSE), error = function(e) mdl)
      }
      recon <- reticulate::py_to_r(mdl$construct()$detach()$cpu()$numpy())
      list(reconstruction = recon, components = comps, model = mdl, device = device)
    }, error = function(e) e)
    if (inherits(py_result, "error")) {
      diagnostics <- rbind(diagnostics, data.frame(
        backend = "python_slicetca", metric = "status", value = "failed", status = "failed",
        note = py_result$message,
        stringsAsFactors = FALSE
      ))
    } else {
      reconstruction <- py_result$reconstruction
      components <- py_result$components
      model <- py_result$model
      diagnostics <- rbind(diagnostics, data.frame(
        backend = "python_slicetca",
        metric = c("status", "device", "max_iter", "learning_rate"),
        value = c("ok", py_result$device, max_iter, signif(learning_rate, 6)),
        status = "ok",
        note = c("Official Python slicetca backend completed.", "", "", ""),
        stringsAsFactors = FALSE
      ))
    }
  }
  recon_embedding <- if (!is.null(reconstruction)) stpd_slicetca_tensor_embedding(tensor_res, reconstruction = reconstruction) else data.frame()
  list(
    status = if (!is.null(reconstruction)) "ok" else if (isTRUE(run_python)) "failed" else "not_run",
    tensor = tensor_res$tensor,
    tensor_raw = tensor_res$tensor_raw,
    reconstruction = reconstruction,
    components = components,
    model = model,
    ranks = ranks,
    tensor_summary = tensor_res$tensor_summary,
    trial_events = tensor_res$trial_events,
    rel_bins = tensor_res$rel_bins,
    train_names = tensor_res$train_names,
    event_annotation = tensor_res$event_annotation,
    diagnostics = diagnostics,
    reconstruction_metrics = stpd_slicetca_reconstruction_metrics(tensor_res, reconstruction),
    trial_embedding = raw_embedding,
    reconstructed_embedding = recon_embedding
  )
}

stpd_slicetca_plot <- function(res, use_reconstruction = TRUE) {
  dat <- if (isTRUE(use_reconstruction) && is.data.frame(res$reconstructed_embedding) && nrow(res$reconstructed_embedding) > 0L) {
    res$reconstructed_embedding
  } else {
    res$trial_embedding
  }
  if (is.null(dat) || nrow(dat) == 0L || !all(c("TC1", "TC2", "TC3") %in% names(dat))) {
    return(layout(plot_ly(), annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                                     text = "No sliceTCA tensor manifold is available.", showarrow = FALSE))))
  }
  dat$hover <- paste0(
    "trial: ", dat$trial_id,
    "<br>condition: ", dat$condition,
    "<br>relative time: ", signif(dat$rel_time_sec, 5), " s",
    if ("event_state" %in% names(dat)) paste0("<br>event state: ", dat$event_state) else ""
  )
  p <- plot_ly(source = "slicetca_tensor_plot")
  conds <- unique(as.character(dat$condition))
  pal <- grDevices::hcl.colors(max(3L, length(conds)), palette = "Dark 3")
  names(pal) <- conds
  for (tr in unique(dat$trial_id)) {
    sub <- dat[as.character(dat$trial_id) == tr, , drop = FALSE]
    if (nrow(sub) < 2L) next
    col <- pal[as.character(sub$condition[1]) %||% conds[1]]
    p <- add_trace(
      p,
      data = sub,
      x = ~TC1, y = ~TC2, z = ~TC3,
      type = "scatter3d",
      mode = "lines+markers",
      line = list(color = col, width = 3),
      marker = list(size = 2.6, color = col),
      text = ~hover,
      hoverinfo = "text",
      name = as.character(sub$condition[1]),
      showlegend = FALSE
    )
  }
  p %>%
    layout(
      title = list(text = if (isTRUE(use_reconstruction) && nrow(res$reconstructed_embedding %||% data.frame()) > 0L) "sliceTCA reconstructed trial manifold" else "Raw trial tensor manifold", x = 0.02, font = list(size = 15)),
      scene = list(
        xaxis = list(title = "TC1", backgroundcolor = "#ffffff", gridcolor = "#e5e7eb"),
        yaxis = list(title = "TC2", backgroundcolor = "#ffffff", gridcolor = "#e5e7eb"),
        zaxis = list(title = "TC3", backgroundcolor = "#ffffff", gridcolor = "#e5e7eb")
      ),
      margin = list(l = 0, r = 0, t = 60, b = 10),
      paper_bgcolor = "#ffffff"
    ) %>%
    config(displaylogo = FALSE, scrollZoom = TRUE)
}
