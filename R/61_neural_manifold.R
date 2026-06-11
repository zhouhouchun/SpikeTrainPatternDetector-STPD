# Neural manifold helpers built from simultaneous population spike-count/rate
# vectors. Detector-derived event labels are intentionally not used as input
# coordinates; labels and behavior are annotation/validation layers.

stpd_neural_manifold_method_choices <- function() {
  c(
    "PCA: transparent linear baseline" = "pca",
    "FA: shared variability / private noise" = "fa",
    "GPFA-style: smoothed FA trajectory" = "gpfa",
    "Isomap: geodesic manifold" = "isomap",
    "PHATE: progression / branch view" = "phate",
    "UMAP: fuzzy topological embedding" = "umap",
    "t-SNE: local-neighborhood view" = "tsne",
    "CEBRA-style supervised behavior axis" = "cebra"
  )
}

stpd_neural_manifold_transform_choices <- function() {
  c(
    "sqrt(count + 3/8)" = "sqrt_count",
    "log1p firing rate" = "log1p_rate",
    "firing rate (Hz)" = "rate",
    "raw spike count" = "count"
  )
}

stpd_neural_empty_result <- function(message = "No neural manifold can be built from the selected spike trains.") {
  list(
    bins = data.frame(),
    features = data.frame(),
    X = matrix(numeric(0), nrow = 0),
    X_raw = matrix(numeric(0), nrow = 0),
    feature_cols = character(0),
    train_windows = data.frame(),
    window_summary = data.frame(message = message, stringsAsFactors = FALSE),
    diagnostics = data.frame(),
    validation = data.frame(),
    loadings = data.frame(),
    method = NA_character_,
    method_label = NA_character_
  )
}

stpd_neural_fill_matrix <- function(X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) == 0L || ncol(X) == 0L) return(X)
  for (jj in seq_len(ncol(X))) {
    x <- X[, jj]
    ok <- is.finite(x)
    fill <- if (any(ok)) stats::median(x[ok], na.rm = TRUE) else 0
    x[!ok] <- fill
    X[, jj] <- x
  }
  X
}

stpd_neural_scale_matrix <- function(X, scaling = c("zscore", "robust", "none")) {
  scaling <- match.arg(scaling)
  X <- stpd_neural_fill_matrix(X)
  if (nrow(X) == 0L || ncol(X) == 0L || identical(scaling, "none")) return(X)
  df <- as.data.frame(X, stringsAsFactors = FALSE)
  out <- stpd_scale_matrix_for_pca(df, scaling = if (identical(scaling, "robust")) "robust" else "zscore")
  out[!is.finite(out)] <- 0
  colnames(out) <- colnames(X)
  out
}

stpd_neural_attach_behavior <- function(features,
                                        behavior = NULL,
                                        behavior_time_col = NULL,
                                        behavior_value_col = NULL) {
  features$behavior_value <- NA
  features$behavior_numeric <- NA_real_
  features$behavior_type <- "none"
  if (is.null(behavior) || !is.data.frame(behavior) || nrow(behavior) == 0L) return(features)
  behavior_time_col <- as.character(behavior_time_col %||% "")[1]
  behavior_value_col <- as.character(behavior_value_col %||% "")[1]
  if (!(behavior_time_col %in% names(behavior)) || !(behavior_value_col %in% names(behavior))) return(features)
  tt <- suppressWarnings(as.numeric(behavior[[behavior_time_col]]))
  vv <- behavior[[behavior_value_col]]
  ok <- is.finite(tt) & !is.na(vv)
  if (!any(ok)) return(features)
  tt <- tt[ok]
  vv <- vv[ok]
  ord <- order(tt)
  tt <- tt[ord]
  vv <- vv[ord]
  mids <- suppressWarnings(as.numeric(features$time_mid_sec))
  vv_num <- suppressWarnings(as.numeric(vv))
  if (sum(is.finite(vv_num)) >= max(3L, length(vv_num) * 0.8)) {
    features$behavior_numeric <- stats::approx(tt, vv_num, xout = mids, rule = 2, ties = "ordered")$y
    features$behavior_value <- features$behavior_numeric
    features$behavior_type <- "numeric"
  } else {
    idx <- findInterval(mids, tt)
    idx[idx < 1L] <- 1L
    idx[idx >= length(tt)] <- length(tt) - 1L
    idx2 <- idx + 1L
    choose_next <- abs(tt[idx2] - mids) < abs(tt[idx] - mids)
    nearest <- idx
    nearest[choose_next] <- idx2[choose_next]
    nearest[!is.finite(mids)] <- NA_integer_
    lab <- as.character(vv[nearest])
    lab[is.na(nearest)] <- NA_character_
    features$behavior_value <- lab
    features$behavior_type <- "categorical"
  }
  features
}

stpd_neural_attach_task_events <- function(features,
                                          task_events = NULL,
                                          event_names = NULL,
                                          pre_sec = 1,
                                          post_sec = 2) {
  features$task_event_name <- NA_character_
  features$task_event_trial_id <- NA_character_
  features$task_event_id <- NA_character_
  features$task_event_time_sec <- NA_real_
  features$task_event_rel_time_sec <- NA_real_
  features$task_event_epoch <- "none"
  features$task_event_in_window <- FALSE
  events <- stpd_normalize_task_events(task_events)
  if (nrow(features) == 0L || nrow(events) == 0L || !("time_mid_sec" %in% names(features))) return(features)
  if (!is.null(event_names) && length(event_names) > 0L) {
    events <- events[as.character(events$event_name) %in% as.character(event_names), , drop = FALSE]
  }
  if (nrow(events) == 0L) return(features)
  pre_sec <- suppressWarnings(as.numeric(pre_sec %||% 1)[1])
  post_sec <- suppressWarnings(as.numeric(post_sec %||% 2)[1])
  if (!is.finite(pre_sec) || pre_sec < 0) pre_sec <- 1
  if (!is.finite(post_sec) || post_sec <= 0) post_sec <- 2
  mids <- suppressWarnings(as.numeric(features$time_mid_sec))
  ev_t <- suppressWarnings(as.numeric(events$event_time_sec))
  for (ii in seq_along(mids)) {
    if (!is.finite(mids[ii])) next
    rel <- mids[ii] - ev_t
    in_window <- is.finite(rel) & rel >= -pre_sec & rel <= post_sec
    if (!any(in_window)) next
    cand <- which(in_window)
    pick <- cand[which.min(abs(rel[cand]))]
    features$task_event_name[ii] <- as.character(events$event_name[pick])
    features$task_event_trial_id[ii] <- as.character(events$trial_id[pick])
    features$task_event_id[ii] <- as.character(events$event_id[pick])
    features$task_event_time_sec[ii] <- ev_t[pick]
    features$task_event_rel_time_sec[ii] <- rel[pick]
    features$task_event_in_window[ii] <- TRUE
    features$task_event_epoch[ii] <- if (rel[pick] < 0) "pre_event" else if (abs(rel[pick]) <= max(.Machine$double.eps, suppressWarnings(as.numeric(features$bin_width_sec[ii] %||% 0)) / 2)) "event_onset" else "post_event"
  }
  features
}

stpd_make_neural_population_matrix <- function(trains,
                                               selected_trains = NULL,
                                               bin_sec = 0.05,
                                               start_sec = NULL,
                                               end_sec = NULL,
                                               time_origin = c("raw", "aligned"),
                                               transform = c("sqrt_count", "log1p_rate", "rate", "count"),
                                               smoothing_sigma_bins = 1,
                                               scaling = c("zscore", "robust", "none"),
                                               behavior = NULL,
                                               behavior_time_col = NULL,
                                               behavior_value_col = NULL,
                                               task_events = NULL,
                                               task_event_names = NULL,
                                               task_event_pre_sec = 1,
                                               task_event_post_sec = 2) {
  time_origin <- match.arg(time_origin)
  transform <- match.arg(transform)
  scaling <- match.arg(scaling)
  if (is.null(trains) || length(trains) == 0L) return(stpd_neural_empty_result("No spike trains are loaded."))
  selected_trains <- as.character(selected_trains %||% names(trains))
  selected_trains <- intersect(selected_trains, names(trains))
  if (length(selected_trains) < 2L) return(stpd_neural_empty_result("Select at least two spike trains for a population manifold."))
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% 0.05))[1]
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
  smoothing_sigma_bins <- suppressWarnings(as.numeric(smoothing_sigma_bins %||% 0))[1]
  if (!is.finite(smoothing_sigma_bins) || smoothing_sigma_bins < 0) smoothing_sigma_bins <- 0

  prepared <- lapply(selected_trains, function(tr) {
    dat <- trains[[tr]]
    if (is.null(dat) || !is.data.frame(dat) || !("timestamp_sec" %in% names(dat))) return(NULL)
    dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ok <- is.finite(ts)
    if (sum(ok) < 2L) return(NULL)
    raw_ts <- ts
    if (identical(time_origin, "aligned")) ts <- ts - min(ts[ok], na.rm = TRUE)
    list(train = tr, raw_ts = raw_ts, ts = ts)
  })
  prepared <- prepared[!vapply(prepared, is.null, logical(1))]
  if (length(prepared) < 2L) return(stpd_neural_empty_result("Need at least two valid spike trains with timestamps."))

  all_ts <- unlist(lapply(prepared, function(x) x$ts[is.finite(x$ts)]), use.names = FALSE)
  auto_start <- min(all_ts, na.rm = TRUE)
  auto_end <- max(all_ts, na.rm = TRUE)
  start_use <- suppressWarnings(as.numeric(start_sec %||% auto_start))[1]
  end_use <- suppressWarnings(as.numeric(end_sec %||% auto_end))[1]
  if (!is.finite(start_use)) start_use <- auto_start
  if (!is.finite(end_use) || end_use <= start_use) end_use <- auto_end
  if (!is.finite(end_use) || end_use <= start_use) end_use <- start_use + bin_sec
  bins <- stpd_state_trajectory_make_bin_table(start_use, end_use, bin_sec)

  safe <- make.unique(stpd_state_trajectory_clean_feature_name(vapply(prepared, function(x) x$train, character(1))), sep = "_")
  train_names <- vapply(prepared, function(x) x$train, character(1))
  names(safe) <- train_names
  counts <- matrix(0, nrow = nrow(bins), ncol = length(prepared), dimnames = list(NULL, train_names))
  for (jj in seq_along(prepared)) {
    ts <- prepared[[jj]]$ts
    for (bb in seq_len(nrow(bins))) {
      b0 <- bins$bin_start_sec[bb]
      b1 <- bins$bin_end_sec[bb]
      if (bb == nrow(bins)) {
        counts[bb, jj] <- sum(is.finite(ts) & ts >= b0 & ts <= b1)
      } else {
        counts[bb, jj] <- sum(is.finite(ts) & ts >= b0 & ts < b1)
      }
    }
  }
  rates <- sweep(counts, 1, pmax(bins$bin_width_sec, .Machine$double.eps), "/")
  signal <- switch(
    transform,
    count = counts,
    rate = rates,
    log1p_rate = log1p(pmax(rates, 0)),
    sqrt_count = sqrt(pmax(counts, 0) + 0.375)
  )
  if (smoothing_sigma_bins > 0 && nrow(signal) >= 3L) {
    for (jj in seq_len(ncol(signal))) signal[, jj] <- stpd_state_trajectory_gaussian_smooth(signal[, jj], smoothing_sigma_bins)
  }

  feature_cols <- paste0("neuron__", safe)
  colnames(signal) <- feature_cols
  X_scaled <- stpd_neural_scale_matrix(signal, scaling = scaling)
  features <- bins
  features$total_spike_count <- rowSums(counts, na.rm = TRUE)
  features$population_rate_hz <- rowSums(rates, na.rm = TRUE) / max(1L, ncol(rates))
  for (jj in seq_along(train_names)) {
    features[[paste0("count__", safe[jj])]] <- counts[, jj]
    features[[paste0("rate_hz__", safe[jj])]] <- rates[, jj]
    features[[feature_cols[jj]]] <- signal[, jj]
  }
  features <- stpd_neural_attach_behavior(features, behavior, behavior_time_col, behavior_value_col)
  if (identical(time_origin, "raw")) {
    features <- stpd_neural_attach_task_events(
      features,
      task_events = task_events,
      event_names = task_event_names,
      pre_sec = task_event_pre_sec,
      post_sec = task_event_post_sec
    )
  } else {
    features <- stpd_neural_attach_task_events(features, task_events = NULL)
  }
  train_windows <- do.call(rbind, lapply(prepared, function(x) {
    raw_ok <- is.finite(x$raw_ts)
    analysis_ok <- is.finite(x$ts)
    data.frame(
      train = x$train,
      raw_start_sec = min(x$raw_ts[raw_ok], na.rm = TRUE),
      raw_end_sec = max(x$raw_ts[raw_ok], na.rm = TRUE),
      analysis_start_sec = min(x$ts[analysis_ok], na.rm = TRUE),
      analysis_end_sec = max(x$ts[analysis_ok], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  train_windows$raw_duration_sec <- train_windows$raw_end_sec - train_windows$raw_start_sec
  train_windows$analysis_duration_sec <- train_windows$analysis_end_sec - train_windows$analysis_start_sec
  window_summary <- data.frame(
    n_trains = length(prepared),
    n_bins = nrow(bins),
    bin_sec = bin_sec,
    transform = transform,
    scaling = scaling,
    smoothing_sigma_bins = smoothing_sigma_bins,
    time_origin = time_origin,
    window_start_sec = start_use,
    window_end_sec = end_use,
    window_duration_sec = end_use - start_use,
    behavior_attached = any(!is.na(features$behavior_value)),
    task_events_attached = any(isTRUE(features$task_event_in_window) | features$task_event_in_window, na.rm = TRUE),
    n_task_event_bins = sum(isTRUE(features$task_event_in_window) | features$task_event_in_window, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  list(
    bins = bins,
    features = features,
    X = X_scaled,
    X_raw = signal,
    counts = counts,
    rates = rates,
    feature_cols = feature_cols,
    train_names = train_names,
    train_windows = train_windows,
    window_summary = window_summary,
    bin_sec = bin_sec,
    time_origin = time_origin,
    transform = transform,
    scaling = scaling,
    smoothing_sigma_bins = smoothing_sigma_bins
  )
}

stpd_neural_take3 <- function(M, prefix) {
  M <- as.data.frame(M, stringsAsFactors = FALSE)
  names(M) <- paste0(prefix, seq_len(ncol(M)))
  for (nm in paste0(prefix, 1:3)) if (!(nm %in% names(M))) M[[nm]] <- 0
  M[, paste0(prefix, 1:3), drop = FALSE]
}

stpd_neural_generic_isomap <- function(X, n_neighbors = 15L, ndim = 3L) {
  X <- stpd_neural_fill_matrix(X)
  n <- nrow(X)
  if (n < 5L) stop("Need at least five time bins for Isomap.", call. = FALSE)
  dmat <- as.matrix(stats::dist(X))
  graph <- stpd_isomap_knn_graph(dmat, n_neighbors = n_neighbors)
  comp <- stpd_isomap_components(graph$neighbors)
  keep <- seq_len(n)
  if (length(comp$sizes) > 1L) {
    keep <- which(comp$component == which.max(comp$sizes))
    sub <- stpd_isomap_subset_graph(graph$neighbors, graph$weights, keep)
    graph$neighbors <- sub$neighbors
    graph$weights <- sub$weights
  }
  geo <- stpd_isomap_all_pairs_shortest_paths(graph$neighbors, graph$weights)
  finite_geo <- geo[upper.tri(geo) & is.finite(geo)]
  if (length(finite_geo) == 0L) stop("Isomap graph has no finite geodesic distances.", call. = FALSE)
  geo[!is.finite(geo)] <- max(finite_geo, na.rm = TRUE) * 2
  ndim <- max(2L, min(3L, ndim, nrow(geo) - 1L))
  mds <- stats::cmdscale(stats::as.dist(geo), k = ndim, eig = TRUE, add = FALSE)
  pts <- as.data.frame(mds$points, stringsAsFactors = FALSE)
  pts <- stpd_neural_take3(pts, "Isomap")
  full <- data.frame(Isomap1 = rep(NA_real_, n), Isomap2 = rep(NA_real_, n), Isomap3 = rep(NA_real_, n))
  full[keep, ] <- pts
  d_emb <- as.matrix(stats::dist(as.matrix(pts)))
  ok <- upper.tri(geo) & is.finite(geo) & is.finite(d_emb)
  cor_val <- suppressWarnings(stats::cor(geo[ok], d_emb[ok]))
  list(
    coords = full,
    diagnostics = data.frame(
      method = "Isomap",
      metric = c("n_neighbors", "embedded_points", "component_count", "residual_variance"),
      value = c(graph$n_neighbors, length(keep), length(comp$sizes), if (is.finite(cor_val)) 1 - cor_val^2 else NA_real_),
      note = c(
        "Euclidean kNN graph.",
        "Largest connected component is embedded when the kNN graph is disconnected.",
        "Number of connected components in the kNN graph.",
        "1 - cor(geodesic distance, embedding distance)^2."
      ),
      stringsAsFactors = FALSE
    )
  )
}

stpd_neural_generic_phate <- function(X, n_neighbors = 15L, diffusion_time = 3L, ndim = 3L, use_phateR = TRUE) {
  X <- stpd_neural_fill_matrix(X)
  if (nrow(X) < 5L) stop("Need at least five time bins for PHATE.", call. = FALSE)
  phate_error <- NULL
  if (isTRUE(use_phateR) && requireNamespace("phateR", quietly = TRUE)) {
    ph <- tryCatch({
      ph_tmp <- NULL
      invisible(utils::capture.output(
        ph_tmp <- suppressMessages(suppressWarnings(phateR::phate(X, ndim = ndim, knn = n_neighbors, t = diffusion_time))),
        type = "message"
      ))
      ph_tmp
    }, error = function(e) e)
    if (!inherits(ph, "error") && !is.null(ph$embedding)) {
      coords <- stpd_neural_take3(ph$embedding, "PHATE")
      return(list(
        coords = coords,
        diagnostics = data.frame(method = "PHATE", metric = "backend", value = "phateR",
                                 note = "Canonical phateR backend.", stringsAsFactors = FALSE)
      ))
    }
    if (inherits(ph, "error")) phate_error <- ph$message
  }
  prob <- stpd_diffusion_probability(X, kernel_scale = "local", n_neighbors = n_neighbors, alpha = 1)
  P <- prob$P
  t_use <- max(1L, safe_int(diffusion_time, 3L))
  Pt <- P
  if (t_use > 1L) for (ii in seq_len(t_use - 1L)) Pt <- Pt %*% P
  Pt <- pmax(Pt, .Machine$double.eps)
  potential <- -log(Pt)
  dpot <- stats::dist(potential)
  mds <- stats::cmdscale(dpot, k = max(2L, min(3L, ndim, nrow(X) - 1L)), eig = TRUE, add = TRUE)
  coords <- stpd_neural_take3(mds$points, "PHATE")
  note <- "PHATE-like fallback: diffusion potential plus metric MDS; install/use phateR for canonical PHATE."
  if (!is.null(phate_error)) note <- paste(note, "phateR error:", phate_error)
  list(
    coords = coords,
    diagnostics = data.frame(method = "PHATE", metric = "backend", value = "diffusion_potential_mds",
                             note = note, stringsAsFactors = FALSE)
  )
}

stpd_run_neural_manifold_embedding <- function(pop,
                                               method = c("pca", "fa", "gpfa", "isomap", "phate", "umap", "tsne", "cebra"),
                                               n_neighbors = 15L,
                                               tsne_perplexity = 30,
                                               umap_min_dist = 0.1,
                                               diffusion_time = 3L,
                                               seed = 1L,
                                               max_points = 1200L) {
  method <- match.arg(method)
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  X <- as.matrix(pop$X %||% matrix(numeric(0), nrow = 0))
  if (nrow(X) < 3L || ncol(X) < 2L) return(stpd_neural_empty_result("Need at least three bins and two varying neurons."))
  n <- nrow(X)
  max_points <- max(20L, safe_int(max_points, 1200L))
  idx <- stpd_state_trajectory_sample_index(n, max_points)
  X0 <- X[idx, , drop = FALSE]
  seed <- safe_int(seed, 1L)
  diagnostics <- data.frame()
  loadings <- data.frame()
  method_label <- method

  coords <- NULL
  if (identical(method, "pca")) {
    fit <- tryCatch(stats::prcomp(X, center = FALSE, scale. = FALSE), error = function(e) e)
    if (inherits(fit, "error")) stop(fit$message, call. = FALSE)
    coords <- stpd_neural_take3(fit$x, "PC")
    sdev2 <- fit$sdev^2
    total <- sum(sdev2)
    diagnostics <- data.frame(
      method = "PCA",
      metric = paste0("PC", seq_len(min(3L, length(sdev2))), "_variance_explained"),
      value = signif(if (total > 0) sdev2[seq_len(min(3L, length(sdev2)))] / total else NA_real_, 6),
      note = "SVD of the scaled population activity matrix.",
      stringsAsFactors = FALSE
    )
    loadings <- as.data.frame(fit$rotation[, seq_len(min(3L, ncol(fit$rotation))), drop = FALSE], stringsAsFactors = FALSE)
    for (nm in paste0("PC", 1:3)) if (!(nm %in% names(loadings))) loadings[[nm]] <- NA_real_
    names(loadings)[seq_len(min(3L, ncol(fit$rotation)))] <- paste0("PC", seq_len(min(3L, ncol(fit$rotation))))
    loadings$feature <- rownames(fit$rotation)
    rownames(loadings) <- NULL
    loadings <- loadings[, c("feature", "PC1", "PC2", "PC3"), drop = FALSE]
    method_label <- "PCA"
  } else if (identical(method, "fa") || identical(method, "gpfa")) {
    X_fa <- X
    if (identical(method, "gpfa") && nrow(X_fa) >= 3L) {
      sigma <- max(1, suppressWarnings(as.numeric(pop$smoothing_sigma_bins %||% 1)[1]))
      for (jj in seq_len(ncol(X_fa))) X_fa[, jj] <- stpd_state_trajectory_gaussian_smooth(X_fa[, jj], sigma)
    }
    qr_fa <- qr(X_fa)
    keep_cols <- seq_len(ncol(X_fa))
    if (qr_fa$rank < ncol(X_fa)) keep_cols <- sort(qr_fa$pivot[seq_len(qr_fa$rank)])
    X_fa <- X_fa[, keep_cols, drop = FALSE]
    n_fac <- stpd_state_trajectory_max_fa_factors(ncol(X_fa), max_factors = 3L)
    if (n_fac < 1L || nrow(X_fa) < n_fac + 3L) stop("Not enough bins/features for maximum-likelihood factor analysis.", call. = FALSE)
    fit <- NULL
    err <- NULL
    for (nf in rev(seq_len(n_fac))) {
      tmp <- tryCatch(stats::factanal(X_fa, factors = nf, scores = "regression", rotation = "none", control = list(nstart = 20L)),
                      error = function(e) e)
      if (!inherits(tmp, "error")) {
        fit <- tmp
        break
      }
      err <- tmp$message
    }
    if (is.null(fit)) stop(err %||% "Unable to fit factor-analysis model.", call. = FALSE)
    coords <- stpd_neural_take3(fit$scores, if (identical(method, "gpfa")) "GPFA" else "FA")
    lam <- as.data.frame(unclass(fit$loadings), stringsAsFactors = FALSE)
    names(lam) <- paste0(if (identical(method, "gpfa")) "GPFA" else "FA", seq_len(ncol(lam)))
    for (nm in paste0(if (identical(method, "gpfa")) "GPFA" else "FA", 1:3)) if (!(nm %in% names(lam))) lam[[nm]] <- NA_real_
    lam$feature <- colnames(X)[keep_cols]
    lam$uniqueness <- suppressWarnings(as.numeric(fit$uniquenesses[seq_along(keep_cols)]))
    rownames(lam) <- NULL
    loadings <- lam
    diagnostics <- data.frame(
      method = if (identical(method, "gpfa")) "GPFA-style" else "FA",
      metric = c("n_factors", "mean_uniqueness"),
      value = c(ncol(fit$loadings), signif(mean(fit$uniquenesses, na.rm = TRUE), 6)),
      note = c(
        if (identical(method, "gpfa")) "Gaussian-smoothed factor-analysis trajectory; not a full GPFA EM/Kalman implementation." else "Linear Gaussian factor analysis.",
        "Feature-specific private variance; lower values imply more shared population structure."
      ),
      stringsAsFactors = FALSE
    )
    method_label <- if (identical(method, "gpfa")) "GPFA-style smooth FA" else "FA"
  } else if (identical(method, "isomap")) {
    iso <- stpd_neural_generic_isomap(X0, n_neighbors = n_neighbors, ndim = 3L)
    coords <- data.frame(PC1 = rep(NA_real_, n), PC2 = rep(NA_real_, n), PC3 = rep(NA_real_, n))
    names(coords) <- c("Isomap1", "Isomap2", "Isomap3")
    coords[idx, ] <- iso$coords
    diagnostics <- iso$diagnostics
    method_label <- "Isomap"
  } else if (identical(method, "phate")) {
    ph <- stpd_neural_generic_phate(X0, n_neighbors = n_neighbors, diffusion_time = diffusion_time)
    coords <- data.frame(PHATE1 = rep(NA_real_, n), PHATE2 = rep(NA_real_, n), PHATE3 = rep(NA_real_, n))
    coords[idx, ] <- ph$coords
    diagnostics <- ph$diagnostics
    method_label <- "PHATE"
  } else if (identical(method, "umap")) {
    if (!requireNamespace("uwot", quietly = TRUE)) stop("Package 'uwot' is required for UMAP.", call. = FALSE)
    k <- max(2L, min(safe_int(n_neighbors, 15L), nrow(X0) - 1L))
    set.seed(seed)
    emb <- uwot::umap(X0, n_components = 3L, n_neighbors = k, min_dist = umap_min_dist,
                      metric = "euclidean", n_threads = 1L, ret_model = FALSE, verbose = FALSE)
    coords <- data.frame(UMAP1 = rep(NA_real_, n), UMAP2 = rep(NA_real_, n), UMAP3 = rep(NA_real_, n))
    coords[idx, ] <- stpd_neural_take3(emb, "UMAP")
    diagnostics <- data.frame(method = "UMAP", metric = c("n_neighbors", "min_dist", "embedded_points"),
                              value = c(k, signif(umap_min_dist, 6), nrow(X0)),
                              note = c("uwot implementation.", "", ""), stringsAsFactors = FALSE)
    method_label <- "UMAP"
  } else if (identical(method, "tsne")) {
    if (!requireNamespace("Rtsne", quietly = TRUE)) stop("Package 'Rtsne' is required for t-SNE.", call. = FALSE)
    perp <- suppressWarnings(as.numeric(tsne_perplexity %||% 30))[1]
    perp <- min(perp, (nrow(X0) - 1) / 3 - 1e-6)
    if (!is.finite(perp) || perp < 1) stop("Not enough sampled bins for a valid t-SNE perplexity.", call. = FALSE)
    set.seed(seed)
    emb <- Rtsne::Rtsne(X0, dims = 3L, perplexity = perp, pca = FALSE, check_duplicates = FALSE,
                        verbose = FALSE, theta = 0.5, max_iter = 1000L)
    coords <- data.frame(tSNE1 = rep(NA_real_, n), tSNE2 = rep(NA_real_, n), tSNE3 = rep(NA_real_, n))
    coords[idx, ] <- stpd_neural_take3(emb$Y, "tSNE")
    diagnostics <- data.frame(method = "t-SNE", metric = c("perplexity", "embedded_points"),
                              value = c(signif(perp, 6), nrow(X0)),
                              note = c("Rtsne implementation.", ""), stringsAsFactors = FALSE)
    method_label <- "t-SNE"
  } else if (identical(method, "cebra")) {
    y <- suppressWarnings(as.numeric(features$behavior_numeric))
    if (sum(is.finite(y)) < 5L) stop("CEBRA-style supervised mode needs a numeric behavior variable.", call. = FALSE)
    ok <- is.finite(y) & stats::complete.cases(X)
    X_ok <- X[ok, , drop = FALSE]
    y_ok <- y[ok]
    lambda <- 1
    beta <- tryCatch(solve(crossprod(X_ok) + diag(lambda, ncol(X_ok)), crossprod(X_ok, y_ok)), error = function(e) NULL)
    if (is.null(beta)) beta <- qr.solve(crossprod(X_ok) + diag(lambda, ncol(X_ok)), crossprod(X_ok, y_ok))
    axis1 <- as.numeric(X %*% beta)
    resid <- X - outer(axis1, as.numeric(crossprod(axis1, X) / pmax(sum(axis1^2), .Machine$double.eps)))
    pc <- stats::prcomp(resid, center = FALSE, scale. = FALSE)
    coords <- data.frame(CEBRA1 = axis1, CEBRA2 = pc$x[, 1], CEBRA3 = if (ncol(pc$x) >= 2L) pc$x[, 2] else 0)
    diagnostics <- data.frame(
      method = "CEBRA-style",
      metric = c("backend", "behavior_axis_cor"),
      value = c("ridge_behavior_axis", signif(suppressWarnings(stats::cor(axis1[ok], y[ok])), 6)),
      note = c("Behavior-guided linear proxy; use external CEBRA for contrastive neural embedding in final analyses.",
               "Correlation between supervised neural axis and behavior in available bins."),
      stringsAsFactors = FALSE
    )
    method_label <- "CEBRA-style supervised behavior axis"
  }

  coords3 <- as.data.frame(coords, stringsAsFactors = FALSE)
  names(coords3)[seq_len(3L)] <- c("NM1", "NM2", "NM3")
  features$NM1 <- coords3$NM1
  features$NM2 <- coords3$NM2
  features$NM3 <- coords3$NM3
  diagnostics <- rbind(
    data.frame(method = method_label, metric = "input_matrix", value = paste0(nrow(X), " bins x ", ncol(X), " neurons"),
               note = "Embedding is computed from binned population activity, not detector-derived event labels.",
               stringsAsFactors = FALSE),
    diagnostics
  )
  out <- pop
  out$features <- features
  out$diagnostics <- diagnostics
  out$loadings <- loadings
  out$method <- method
  out$method_label <- method_label
  out$sample_index <- idx
  out
}

stpd_neural_trustworthiness <- function(X_high, X_low, k = 10L) {
  X_high <- stpd_neural_fill_matrix(X_high)
  X_low <- stpd_neural_fill_matrix(X_low)
  n <- min(nrow(X_high), nrow(X_low))
  if (n < 4L) return(NA_real_)
  k <- max(1L, min(safe_int(k, 10L), floor((n - 1L) / 2L)))
  Dh <- as.matrix(stats::dist(X_high[seq_len(n), , drop = FALSE]))
  Dl <- as.matrix(stats::dist(X_low[seq_len(n), , drop = FALSE]))
  penalty <- 0
  for (ii in seq_len(n)) {
    high_order <- order(Dh[ii, ], na.last = NA)
    high_order <- high_order[high_order != ii]
    low_order <- order(Dl[ii, ], na.last = NA)
    low_order <- low_order[low_order != ii]
    high_rank <- seq_along(high_order)
    names(high_rank) <- high_order
    low_k <- head(low_order, k)
    intruders <- setdiff(low_k, head(high_order, k))
    if (length(intruders) > 0L) penalty <- penalty + sum(as.numeric(high_rank[as.character(intruders)]) - k, na.rm = TRUE)
  }
  denom <- n * k * (2 * n - 3 * k - 1)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  1 - 2 * penalty / denom
}

stpd_neural_behavior_decoding <- function(features) {
  y_num <- suppressWarnings(as.numeric(features$behavior_numeric))
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  ok <- is.finite(y_num) & stats::complete.cases(Y)
  if (sum(ok) < 8L) {
    return(data.frame(metric = "behavior_decoding", value = NA_real_, status = "missing",
                      note = "No numeric behavior variable with enough bins.", stringsAsFactors = FALSE))
  }
  idx <- which(ok)
  n_train <- max(4L, floor(length(idx) * 0.7))
  train <- idx[seq_len(n_train)]
  test <- idx[(n_train + 1L):length(idx)]
  if (length(test) < 3L) test <- tail(idx, min(3L, length(idx)))
  df_train <- data.frame(y = y_num[train], Y[train, , drop = FALSE])
  df_test <- data.frame(Y[test, , drop = FALSE])
  fit <- tryCatch(stats::lm(y ~ ., data = df_train), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(metric = "behavior_decoding", value = NA_real_, status = "failed",
                      note = "Linear behavior decoder failed.", stringsAsFactors = FALSE))
  }
  pred <- suppressWarnings(stats::predict(fit, newdata = df_test))
  obs <- y_num[test]
  cor_val <- suppressWarnings(stats::cor(pred, obs, use = "complete.obs"))
  rmse <- sqrt(mean((pred - obs)^2, na.rm = TRUE))
  r2 <- 1 - sum((pred - obs)^2, na.rm = TRUE) / pmax(sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE), .Machine$double.eps)
  data.frame(
    metric = c("behavior_decoding_cor", "behavior_decoding_rmse", "behavior_decoding_r2"),
    value = c(cor_val, rmse, r2),
    status = "ok",
    note = "Blocked 70/30 time split; linear decoder from 3D manifold coordinates to numeric behavior.",
    stringsAsFactors = FALSE
  )
}

stpd_neural_heldout_neuron_prediction <- function(pop, max_neurons = 10L) {
  X <- as.matrix(pop$X)
  if (nrow(X) < 10L || ncol(X) < 3L) {
    return(data.frame(metric = "heldout_neuron_prediction", value = NA_real_, status = "skipped",
                      note = "Need at least ten bins and three neurons.", stringsAsFactors = FALSE))
  }
  vars <- apply(X, 2, stats::var, na.rm = TRUE)
  cols <- order(vars, decreasing = TRUE)
  cols <- cols[seq_len(min(length(cols), max(1L, safe_int(max_neurons, 10L))))]
  n <- nrow(X)
  train_idx <- seq_len(max(4L, floor(n * 0.7)))
  test_idx <- setdiff(seq_len(n), train_idx)
  cors <- rmses <- numeric(0)
  for (jj in cols) {
    X_rem <- X[, setdiff(seq_len(ncol(X)), jj), drop = FALSE]
    if (ncol(X_rem) < 2L) next
    pc <- tryCatch(stats::prcomp(X_rem, center = FALSE, scale. = FALSE), error = function(e) NULL)
    if (is.null(pc)) next
    Z <- pc$x[, seq_len(min(3L, ncol(pc$x))), drop = FALSE]
    Z <- as.data.frame(stpd_neural_take3(Z, "Z"), stringsAsFactors = FALSE)
    y <- X[, jj]
    fit <- tryCatch(stats::lm(y ~ ., data = data.frame(y = y[train_idx], Z[train_idx, , drop = FALSE])),
                    error = function(e) NULL)
    if (is.null(fit) || length(test_idx) < 2L) next
    pred <- suppressWarnings(stats::predict(fit, newdata = Z[test_idx, , drop = FALSE]))
    obs <- y[test_idx]
    cors <- c(cors, suppressWarnings(stats::cor(pred, obs, use = "complete.obs")))
    rmses <- c(rmses, sqrt(mean((pred - obs)^2, na.rm = TRUE)))
  }
  data.frame(
    metric = c("heldout_neuron_prediction_cor", "heldout_neuron_prediction_rmse"),
    value = c(mean(cors, na.rm = TRUE), mean(rmses, na.rm = TRUE)),
    status = if (length(cors) > 0L) "ok" else "failed",
    note = "Each sampled neuron is held out from PCA coordinates, then predicted on a later time split.",
    stringsAsFactors = FALSE
  )
}

stpd_neural_shuffle_controls <- function(pop, seed = 1L) {
  X <- as.matrix(pop$X)
  if (nrow(X) < 5L || ncol(X) < 2L) {
    return(data.frame(metric = "shuffle_controls", value = NA_real_, status = "skipped",
                      note = "Need enough bins and neurons.", stringsAsFactors = FALSE))
  }
  pca_var3 <- function(M) {
    fit <- tryCatch(stats::prcomp(M, center = FALSE, scale. = FALSE), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    s <- fit$sdev^2
    sum(head(s, 3), na.rm = TRUE) / pmax(sum(s, na.rm = TRUE), .Machine$double.eps)
  }
  obs <- pca_var3(X)
  set.seed(safe_int(seed, 1L))
  X_time <- X
  for (jj in seq_len(ncol(X_time))) X_time[, jj] <- sample(X_time[, jj])
  X_neuron <- X
  for (ii in seq_len(nrow(X_neuron))) X_neuron[ii, ] <- sample(X_neuron[ii, ])
  data.frame(
    metric = c("pca3_variance_observed", "time_shuffle_pca3_variance_ratio", "neuron_shuffle_pca3_variance_ratio"),
    value = c(obs, pca_var3(X_time) / obs, pca_var3(X_neuron) / obs),
    status = "ok",
    note = c(
      "Fraction of scaled population variance captured by first three PCs.",
      "Independent within-neuron time shuffling destroys temporal co-activation.",
      "Within-bin neuron-value shuffling preserves population magnitude but disrupts neuron identity."
    ),
    stringsAsFactors = FALSE
  )
}

stpd_neural_add_latent_dynamics <- function(features, bin_sec = NULL) {
  features <- as.data.frame(features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !all(c("NM1", "NM2", "NM3") %in% names(features))) return(features)
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  storage.mode(Y) <- "double"
  n <- nrow(Y)
  speed <- rep(NA_real_, n)
  curvature <- rep(NA_real_, n)
  tt <- suppressWarnings(as.numeric(features$time_mid_sec %||% rep(NA_real_, n)))
  dt <- c(NA_real_, diff(tt))
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% NA_real_))[1]
  dt[!is.finite(dt) | dt <= 0] <- bin_sec
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- stats::median(dt[is.finite(dt) & dt > 0], na.rm = TRUE)
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 1
  dt[!is.finite(dt) | dt <= 0] <- bin_sec
  if (n >= 2L) {
    step <- sqrt(rowSums((Y[-1, , drop = FALSE] - Y[-n, , drop = FALSE])^2))
    speed[-1] <- step / pmax(dt[-1], .Machine$double.eps)
  }
  if (n >= 3L) {
    for (ii in 2:(n - 1L)) {
      a <- Y[ii, ] - Y[ii - 1L, ]
      b <- Y[ii + 1L, ] - Y[ii, ]
      na <- sqrt(sum(a^2))
      nb <- sqrt(sum(b^2))
      if (!is.finite(na) || !is.finite(nb) || na <= 0 || nb <= 0) next
      cosang <- sum(a * b) / (na * nb)
      cosang <- max(-1, min(1, cosang))
      angle <- acos(cosang)
      curvature[ii] <- angle / pmax((na + nb) / 2, .Machine$double.eps)
    }
  }
  features$latent_speed <- speed
  features$latent_curvature <- curvature
  features
}

stpd_neural_attach_event_states <- function(pop, state_res, label_source = "audit_final") {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  state_features <- as.data.frame(state_res$features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || nrow(state_features) == 0L || !("bin_id" %in% names(features)) || !("bin_id" %in% names(state_features))) {
    features$event_state <- "unlabeled"
    pop$features <- stpd_neural_add_latent_dynamics(features, pop$bin_sec %||% NA_real_)
    pop$event_state_result <- state_res
    pop$event_label_source <- label_source
    return(pop)
  }
  idx <- match(features$bin_id, state_features$bin_id)
  state <- as.character(state_features$dominant_state[idx] %||% rep("unlabeled", nrow(features)))
  state[is.na(state) | !nzchar(state)] <- "unlabeled"
  features$event_state <- state
  states <- stpd_state_trajectory_state_levels()
  copy_cols <- unique(c(
    "burst_activity", "pause_activity", "tonic_activity", "hf_spiking_activity",
    paste0(states, "_spike_count"),
    paste0(states, "_rate_hz"),
    paste0(states, "_fraction")
  ))
  copy_cols <- intersect(copy_cols, names(state_features))
  for (nm in copy_cols) {
    features[[paste0("event_", nm)]] <- state_features[[nm]][idx]
  }
  pop$features <- stpd_neural_add_latent_dynamics(features, pop$bin_sec %||% NA_real_)
  pop$event_state_result <- state_res
  pop$event_label_source <- label_source
  pop
}

stpd_neural_fast_event_state_result <- function(pop,
                                                trains,
                                                selected_trains = NULL,
                                                label_source = "audit_final",
                                                min_isi_sec = 0.001,
                                                auto_others = FALSE) {
  base_features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  required <- c("bin_id", "bin_start_sec", "bin_end_sec", "time_mid_sec", "bin_width_sec")
  if (nrow(base_features) == 0L || !all(required %in% names(base_features))) {
    return(list(features = data.frame(), bins = data.frame()))
  }
  selected_trains <- as.character(selected_trains %||% pop$train_names %||% names(trains))
  selected_trains <- intersect(selected_trains, names(trains))
  if (length(selected_trains) == 0L) return(list(features = data.frame(), bins = data.frame()))

  bins <- base_features[, required, drop = FALSE]
  n_bins <- nrow(bins)
  starts <- suppressWarnings(as.numeric(bins$bin_start_sec))
  ends <- suppressWarnings(as.numeric(bins$bin_end_sec))
  widths <- suppressWarnings(as.numeric(bins$bin_width_sec))
  if (n_bins == 0L || any(!is.finite(starts)) || any(!is.finite(ends))) {
    return(list(features = data.frame(), bins = data.frame()))
  }
  widths[!is.finite(widths) | widths <= 0] <- suppressWarnings(as.numeric(pop$bin_sec %||% NA_real_))[1]
  widths[!is.finite(widths) | widths <= 0] <- stats::median(diff(starts), na.rm = TRUE)
  widths[!is.finite(widths) | widths <= 0] <- 1

  groups <- stpd_state_trajectory_pattern_groups()
  group_names <- names(groups)
  states <- stpd_state_trajectory_state_levels()
  features <- bins
  features$n_trains <- length(selected_trains)
  features$total_spike_count <- 0
  features$firing_rate_hz <- 0
  for (g in group_names) {
    features[[paste0(g, "_spike_count")]] <- 0
    features[[paste0(g, "_rate_hz")]] <- 0
    features[[paste0(g, "_fraction")]] <- 0
  }

  prepared_names <- character(0)
  win_start <- min(starts, na.rm = TRUE)
  win_end <- max(ends, na.rm = TRUE)
  tol <- max(.Machine$double.eps * 128, 1e-12)

  for (tr in selected_trains) {
    dat <- trains[[tr]]
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat) < 1L || !("timestamp_sec" %in% names(dat))) next
    dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ok_ts <- is.finite(ts)
    if (sum(ok_ts) < 1L) next
    if (identical(pop$time_origin %||% "raw", "aligned")) {
      ts <- ts - min(ts[ok_ts], na.rm = TRUE)
    }
    labels <- stpd_state_space_pattern_labels(
      dat,
      label_source = label_source,
      min_isi_sec = min_isi_sec,
      auto_others = auto_others
    )
    labels <- rep_len(as.character(labels), length(ts))
    labels[is.na(labels) | !nzchar(labels)] <- "unlabeled"

    prepared_names <- c(prepared_names, tr)
    tr_safe <- stpd_state_trajectory_clean_feature_name(tr)
    train_count <- numeric(n_bins)
    train_group_count <- stats::setNames(vector("list", length(group_names)), group_names)
    train_group_overlap <- stats::setNames(vector("list", length(group_names)), group_names)
    for (g in group_names) {
      train_group_count[[g]] <- numeric(n_bins)
      train_group_overlap[[g]] <- numeric(n_bins)
    }

    in_window <- is.finite(ts) & ts >= win_start & ts <= win_end
    if (any(in_window)) {
      spike_bins <- findInterval(ts[in_window], starts, rightmost.closed = TRUE)
      spike_bins <- spike_bins[spike_bins >= 1L & spike_bins <= n_bins]
      if (length(spike_bins) > 0L) {
        train_count <- train_count + tabulate(spike_bins, nbins = n_bins)
      }
      lab_sp <- labels[in_window]
      spike_bins_all <- findInterval(ts[in_window], starts, rightmost.closed = TRUE)
      keep_sp <- spike_bins_all >= 1L & spike_bins_all <= n_bins
      if (any(keep_sp)) {
        spike_bins_all <- spike_bins_all[keep_sp]
        lab_sp <- lab_sp[keep_sp]
        for (g in group_names) {
          hit <- lab_sp %in% groups[[g]]
          if (any(hit)) train_group_count[[g]] <- train_group_count[[g]] + tabulate(spike_bins_all[hit], nbins = n_bins)
        }
      }
    }

    if (length(ts) >= 2L) {
      for (ii in seq.int(2L, length(ts))) {
        a0 <- ts[ii - 1L]
        a1 <- ts[ii]
        if (!is.finite(a0) || !is.finite(a1) || a1 <= a0) next
        if (a1 <= win_start || a0 >= win_end) next
        lab <- labels[ii]
        g_hit <- group_names[vapply(groups, function(vals) lab %in% vals, logical(1))]
        if (length(g_hit) == 0L) next
        a0c <- max(a0, win_start)
        a1c <- min(a1, win_end)
        if (!is.finite(a0c) || !is.finite(a1c) || a1c <= a0c) next
        s_idx <- findInterval(a0c, starts, rightmost.closed = TRUE)
        e_idx <- findInterval(a1c, starts, rightmost.closed = TRUE)
        if (e_idx > 1L && abs(a1c - starts[e_idx]) <= tol) e_idx <- e_idx - 1L
        s_idx <- max(1L, min(n_bins, s_idx))
        e_idx <- max(1L, min(n_bins, e_idx))
        if (e_idx < s_idx) e_idx <- s_idx
        idx <- seq.int(s_idx, e_idx)
        ov <- pmax(0, pmin(a1c, ends[idx]) - pmax(a0c, starts[idx]))
        if (!any(is.finite(ov) & ov > 0)) next
        for (g in g_hit) train_group_overlap[[g]][idx] <- train_group_overlap[[g]][idx] + ov
      }
    }

    features$total_spike_count <- features$total_spike_count + train_count
    features[[paste0(tr_safe, "__firing_rate_hz")]] <- train_count / widths
    for (g in group_names) {
      features[[paste0(g, "_spike_count")]] <- features[[paste0(g, "_spike_count")]] + train_group_count[[g]]
      features[[paste0(g, "_fraction")]] <- features[[paste0(g, "_fraction")]] + train_group_overlap[[g]] / widths
      features[[paste0(tr_safe, "__", g, "_rate_hz")]] <- train_group_count[[g]] / widths
      features[[paste0(tr_safe, "__", g, "_fraction")]] <- pmax(0, pmin(1, train_group_overlap[[g]] / widths))
    }
  }

  denom <- max(1L, length(prepared_names))
  features$n_trains <- denom
  features$firing_rate_hz <- features$total_spike_count / widths / denom
  for (g in group_names) {
    features[[paste0(g, "_rate_hz")]] <- features[[paste0(g, "_spike_count")]] / widths / denom
    features[[paste0(g, "_fraction")]] <- pmax(0, pmin(1, features[[paste0(g, "_fraction")]] / denom))
  }
  features$burst_activity <- features$burst_rate_hz
  features$pause_activity <- features$pause_fraction
  features$tonic_activity <- features$tonic_rate_hz
  features$hf_spiking_activity <- features$hf_spiking_rate_hz
  features$hf_activity <- features$hf_spiking_activity

  state_score_cols <- paste0(states, "_fraction")
  for (nm in state_score_cols) if (!(nm %in% names(features))) features[[nm]] <- 0
  score <- as.matrix(features[, state_score_cols, drop = FALSE])
  score[!is.finite(score)] <- 0
  max_idx <- max.col(score, ties.method = "first")
  zero_rows <- rowSums(score, na.rm = TRUE) <= 0
  features$dominant_state <- states[max_idx]
  features$dominant_state[zero_rows] <- "unlabeled"

  list(
    bins = features,
    features = features,
    variance = data.frame(),
    loadings = data.frame(),
    fa_loadings = data.frame(),
    embedding_diagnostics = data.frame(),
    pca = NULL,
    feature_cols = names(features)[grepl("__", names(features), fixed = TRUE)],
    selected_trains = prepared_names,
    per_train_states = stpd_state_trajectory_per_train_states(features, prepared_names),
    train_windows = data.frame(),
    window_summary = data.frame(
      n_trains = denom,
      bin_sec = suppressWarnings(as.numeric(pop$bin_sec %||% NA_real_))[1],
      n_bins = n_bins,
      window_start_sec = win_start,
      window_end_sec = win_end,
      window_duration_sec = win_end - win_start,
      stringsAsFactors = FALSE
    ),
    bin_sec = suppressWarnings(as.numeric(pop$bin_sec %||% NA_real_))[1],
    time_origin = pop$time_origin %||% "raw",
    label_source = label_source,
    smoothing_sigma_bins = 0,
    computation = "fast_interval_scan"
  )
}

stpd_neural_add_event_state_layer <- function(pop,
                                              trains,
                                              selected_trains = NULL,
                                              label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                              min_isi_sec = 0.001,
                                              auto_others = FALSE) {
  label_source <- match.arg(label_source)
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L) return(pop)
  start_sec <- suppressWarnings(as.numeric(pop$window_summary$window_start_sec[1] %||% min(features$bin_start_sec, na.rm = TRUE)))
  end_sec <- suppressWarnings(as.numeric(pop$window_summary$window_end_sec[1] %||% max(features$bin_end_sec, na.rm = TRUE)))
  bin_sec <- suppressWarnings(as.numeric(pop$bin_sec %||% pop$window_summary$bin_sec[1] %||% NA_real_))[1]
  if (!is.finite(bin_sec) || bin_sec <= 0) {
    bw <- suppressWarnings(as.numeric(features$bin_width_sec))
    bin_sec <- stats::median(bw[is.finite(bw) & bw > 0], na.rm = TRUE)
  }
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
  state_res <- tryCatch(
    stpd_neural_fast_event_state_result(
      pop = pop,
      trains = trains,
      selected_trains = selected_trains %||% pop$train_names %||% names(trains),
      label_source = label_source,
      min_isi_sec = min_isi_sec,
      auto_others = auto_others
    ),
    error = function(e) {
      stpd_make_state_trajectory(
        trains,
        selected_trains = selected_trains %||% pop$train_names %||% names(trains),
        bin_sec = bin_sec,
        start_sec = start_sec,
        end_sec = end_sec,
        time_origin = pop$time_origin %||% "raw",
        label_source = label_source,
        min_isi_sec = min_isi_sec,
        auto_others = auto_others,
        smoothing_sigma_bins = 0,
        embedding_methods = character(0)
      )
    }
  )
  stpd_neural_attach_event_states(pop, state_res, label_source = label_source)
}

stpd_neural_event_geometry <- function(pop, min_bins = 1L) {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !all(c("NM1", "NM2", "NM3", "event_state") %in% names(features))) {
    return(data.frame(message = "No event-state annotations are available.", stringsAsFactors = FALSE))
  }
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  lab <- as.character(features$event_state)
  ok <- stats::complete.cases(Y) & !is.na(lab) & nzchar(lab)
  if (sum(ok) == 0L) return(data.frame(message = "No embedded bins have event-state labels.", stringsAsFactors = FALSE))
  states <- unique(c(stpd_state_trajectory_state_levels(), sort(unique(lab[ok]))))
  states <- states[states %in% unique(lab[ok])]
  min_bins <- max(1L, safe_int(min_bins, 1L))
  rows <- lapply(states, function(st) {
    ii <- which(ok & lab == st)
    if (length(ii) < min_bins) return(NULL)
    centroid <- colMeans(Y[ii, , drop = FALSE], na.rm = TRUE)
    d <- sqrt(rowSums(sweep(Y[ii, , drop = FALSE], 2, centroid, "-")^2))
    data.frame(
      event_state = st,
      n_bins = length(ii),
      occupancy_fraction = length(ii) / sum(ok),
      centroid_NM1 = centroid[1],
      centroid_NM2 = centroid[2],
      centroid_NM3 = centroid[3],
      dispersion_mean = mean(d, na.rm = TRUE),
      dispersion_median = stats::median(d, na.rm = TRUE),
      dispersion_sd = stats::sd(d, na.rm = TRUE),
      radius95 = suppressWarnings(as.numeric(stats::quantile(d, 0.95, na.rm = TRUE, names = FALSE))),
      mean_latent_speed = if ("latent_speed" %in% names(features)) mean(features$latent_speed[ii], na.rm = TRUE) else NA_real_,
      mean_latent_curvature = if ("latent_curvature" %in% names(features)) mean(features$latent_curvature[ii], na.rm = TRUE) else NA_real_,
      mean_behavior_numeric = if ("behavior_numeric" %in% names(features)) mean(features$behavior_numeric[ii], na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out) || nrow(out) == 0L) return(data.frame(message = "No event state has enough embedded bins.", stringsAsFactors = FALSE))
  out[order(out$n_bins, decreasing = TRUE), , drop = FALSE]
}

stpd_neural_centroid_distance <- function(Y, lab, state_a, state_b, min_bins = 2L) {
  ia <- which(lab == state_a & stats::complete.cases(Y))
  ib <- which(lab == state_b & stats::complete.cases(Y))
  if (length(ia) < min_bins || length(ib) < min_bins) return(NA_real_)
  ca <- colMeans(Y[ia, , drop = FALSE], na.rm = TRUE)
  cb <- colMeans(Y[ib, , drop = FALSE], na.rm = TRUE)
  sqrt(sum((ca - cb)^2))
}

stpd_neural_event_distance_tests <- function(pop,
                                             states = c("burst", "pause", "tonic", "hf_spiking"),
                                             n_perm = 199L,
                                             seed = 1L,
                                             min_bins = 2L) {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !all(c("NM1", "NM2", "NM3", "event_state") %in% names(features))) {
    return(data.frame(message = "No event-state manifold geometry is available.", stringsAsFactors = FALSE))
  }
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  lab <- as.character(features$event_state)
  ok <- stats::complete.cases(Y) & !is.na(lab) & nzchar(lab)
  Y <- Y[ok, , drop = FALSE]
  lab <- lab[ok]
  keep_states <- unique(c(states, sort(unique(lab))))
  keep_states <- keep_states[keep_states %in% unique(lab)]
  keep_states <- keep_states[vapply(keep_states, function(st) sum(lab == st) >= min_bins, logical(1))]
  if (length(keep_states) < 2L) {
    return(data.frame(message = "Need at least two event states with enough bins.", stringsAsFactors = FALSE))
  }
  pairs <- utils::combn(keep_states, 2L, simplify = FALSE)
  n_perm <- max(0L, safe_int(n_perm, 199L))
  seed <- safe_int(seed, 1L)
  set.seed(seed)
  rows <- lapply(pairs, function(pr) {
    a <- pr[1]
    b <- pr[2]
    obs <- stpd_neural_centroid_distance(Y, lab, a, b, min_bins = min_bins)
    if (!is.finite(obs)) return(NULL)
    null_perm <- null_shift <- numeric(0)
    if (n_perm > 0L) {
      for (ii in seq_len(n_perm)) {
        lab_perm <- sample(lab)
        val <- stpd_neural_centroid_distance(Y, lab_perm, a, b, min_bins = min_bins)
        if (is.finite(val)) null_perm <- c(null_perm, val)
        offset <- sample.int(max(1L, length(lab) - 1L), 1L)
        lab_shift <- c(tail(lab, offset), head(lab, length(lab) - offset))
        val2 <- stpd_neural_centroid_distance(Y, lab_shift, a, b, min_bins = min_bins)
        if (is.finite(val2)) null_shift <- c(null_shift, val2)
      }
    }
    p_perm <- if (length(null_perm) > 0L) (1 + sum(null_perm >= obs, na.rm = TRUE)) / (length(null_perm) + 1) else NA_real_
    p_shift <- if (length(null_shift) > 0L) (1 + sum(null_shift >= obs, na.rm = TRUE)) / (length(null_shift) + 1) else NA_real_
    data.frame(
      state_a = a,
      state_b = b,
      n_a = sum(lab == a),
      n_b = sum(lab == b),
      centroid_distance = obs,
      event_label_shuffle_p = p_perm,
      circular_time_shift_p = p_shift,
      n_permutations = n_perm,
      note = "Large centroid distances are tested against event-label permutation and circular time-shift controls.",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out) || nrow(out) == 0L) return(data.frame(message = "No valid event-state distance tests.", stringsAsFactors = FALSE))
  bp <- out$state_a == "burst" & out$state_b == "pause"
  out <- out[order(!bp, out$event_label_shuffle_p, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_neural_nearest_centroid_predict <- function(Y_train, y_train, Y_test) {
  cls <- sort(unique(as.character(y_train)))
  cls <- cls[nzchar(cls)]
  if (length(cls) < 2L) return(rep(NA_character_, nrow(Y_test)))
  centers <- do.call(rbind, lapply(cls, function(cl) colMeans(Y_train[y_train == cl, , drop = FALSE], na.rm = TRUE)))
  rownames(centers) <- cls
  pred <- apply(Y_test, 1L, function(row) {
    d <- rowSums(sweep(centers, 2, row, "-")^2)
    names(which.min(d))[1]
  })
  as.character(pred)
}

stpd_neural_balanced_accuracy <- function(pred, truth) {
  pred <- as.character(pred)
  truth <- as.character(truth)
  cls <- sort(unique(truth[nzchar(truth)]))
  if (length(cls) == 0L) return(NA_real_)
  acc <- vapply(cls, function(cl) {
    ii <- truth == cl
    if (!any(ii)) return(NA_real_)
    mean(pred[ii] == truth[ii], na.rm = TRUE)
  }, numeric(1))
  mean(acc, na.rm = TRUE)
}

stpd_neural_event_decoding_rows <- function(value = c(NA_real_, NA_real_, NA_real_),
                                            status = "skipped",
                                            note = "Event-label decoding was skipped.") {
  data.frame(
    metric = c("event_label_decoding_accuracy", "event_label_decoding_balanced_accuracy", "event_label_shuffle_accuracy_p"),
    value = rep_len(as.numeric(value), 3L),
    status = as.character(status),
    note = rep_len(as.character(note), 3L),
    stringsAsFactors = FALSE
  )
}

stpd_neural_event_label_decoding <- function(features, seed = 1L, n_perm = 199L) {
  features <- as.data.frame(features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !all(c("NM1", "NM2", "NM3", "event_state") %in% names(features))) {
    return(stpd_neural_event_decoding_rows(note = "No event-state labels are available."))
  }
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  lab <- as.character(features$event_state)
  ok <- stats::complete.cases(Y) & !is.na(lab) & nzchar(lab) & lab != "unlabeled"
  Y <- Y[ok, , drop = FALSE]
  lab <- lab[ok]
  cls <- sort(unique(lab))
  if (nrow(Y) < 10L || length(cls) < 2L) {
    return(stpd_neural_event_decoding_rows(note = "Need at least ten labeled bins and two event classes excluding unlabeled."))
  }
  n_train <- max(4L, floor(nrow(Y) * 0.7))
  train <- seq_len(n_train)
  test <- seq.int(n_train + 1L, nrow(Y))
  if (length(test) < 3L || length(unique(lab[train])) < 2L || length(unique(lab[test])) < 2L) {
    return(stpd_neural_event_decoding_rows(note = "Blocked time split does not contain enough event classes in train/test."))
  }
  pred <- stpd_neural_nearest_centroid_predict(Y[train, , drop = FALSE], lab[train], Y[test, , drop = FALSE])
  acc <- mean(pred == lab[test], na.rm = TRUE)
  bal <- stpd_neural_balanced_accuracy(pred, lab[test])
  n_perm <- max(0L, safe_int(n_perm, 199L))
  seed <- safe_int(seed, 1L)
  null_acc <- numeric(0)
  if (n_perm > 0L) {
    set.seed(seed)
    for (ii in seq_len(n_perm)) {
      lab_perm <- sample(lab)
      if (length(unique(lab_perm[train])) < 2L || length(unique(lab_perm[test])) < 2L) next
      pp <- stpd_neural_nearest_centroid_predict(Y[train, , drop = FALSE], lab_perm[train], Y[test, , drop = FALSE])
      null_acc <- c(null_acc, mean(pp == lab_perm[test], na.rm = TRUE))
    }
  }
  p_acc <- if (length(null_acc) > 0L) (1 + sum(null_acc >= acc, na.rm = TRUE)) / (length(null_acc) + 1) else NA_real_
  stpd_neural_event_decoding_rows(
    value = c(acc, bal, p_acc),
    status = "ok",
    note = c(
      "Nearest-centroid classifier from 3D manifold coordinates to event state; blocked 70/30 time split.",
      "Mean per-class recall for event-state decoding.",
      "Permutation p-value against shuffled event labels; small values mean event labels align with manifold geometry beyond class imbalance."
    )
  )
}

stpd_neural_behavior_decoding_with_events <- function(features) {
  features <- as.data.frame(features %||% data.frame(), stringsAsFactors = FALSE)
  y_num <- suppressWarnings(as.numeric(features$behavior_numeric))
  if (!all(c("NM1", "NM2", "NM3") %in% names(features))) {
    return(data.frame(metric = "behavior_decoding_with_events", value = NA_real_, status = "skipped",
                      note = "No 3D manifold coordinates.", stringsAsFactors = FALSE))
  }
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  ok <- is.finite(y_num) & stats::complete.cases(Y)
  if (sum(ok) < 10L) {
    return(data.frame(metric = "behavior_decoding_with_events", value = NA_real_, status = "missing",
                      note = "No numeric behavior variable with enough bins.", stringsAsFactors = FALSE))
  }
  event_cols <- grep("^event_.*_(fraction|activity)$", names(features), value = TRUE, perl = TRUE)
  event_cols <- event_cols[vapply(features[event_cols], is.numeric, logical(1))]
  if (length(event_cols) == 0L && "event_state" %in% names(features)) {
    mm <- stats::model.matrix(~ event_state - 1, data = features)
    event_mat <- mm
  } else {
    event_mat <- as.matrix(features[, event_cols, drop = FALSE])
  }
  event_mat[!is.finite(event_mat)] <- 0
  ok <- ok & stats::complete.cases(event_mat)
  idx <- which(ok)
  if (length(idx) < 10L || ncol(event_mat) == 0L) {
    return(data.frame(metric = "behavior_decoding_with_events", value = NA_real_, status = "skipped",
                      note = "Event-state regressors are unavailable.", stringsAsFactors = FALSE))
  }
  n_train <- max(5L, floor(length(idx) * 0.7))
  train <- idx[seq_len(n_train)]
  test <- idx[(n_train + 1L):length(idx)]
  if (length(test) < 3L) {
    return(data.frame(metric = "behavior_decoding_with_events", value = NA_real_, status = "skipped",
                      note = "Blocked time split leaves too few behavior test bins.", stringsAsFactors = FALSE))
  }
  score_model <- function(X_train, X_test) {
    df_train <- data.frame(y = y_num[train], X_train, check.names = FALSE)
    df_test <- data.frame(X_test, check.names = FALSE)
    fit <- tryCatch(stats::lm(y ~ ., data = df_train), error = function(e) NULL)
    if (is.null(fit)) return(c(r2 = NA_real_, rmse = NA_real_))
    pred <- suppressWarnings(stats::predict(fit, newdata = df_test))
    obs <- y_num[test]
    r2 <- 1 - sum((pred - obs)^2, na.rm = TRUE) / pmax(sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE), .Machine$double.eps)
    rmse <- sqrt(mean((pred - obs)^2, na.rm = TRUE))
    c(r2 = r2, rmse = rmse)
  }
  base <- score_model(Y[train, , drop = FALSE], Y[test, , drop = FALSE])
  plus <- score_model(cbind(Y[train, , drop = FALSE], event_mat[train, , drop = FALSE]),
                      cbind(Y[test, , drop = FALSE], event_mat[test, , drop = FALSE]))
  data.frame(
    metric = c("behavior_r2_manifold_only", "behavior_r2_manifold_plus_event", "behavior_delta_r2_event",
               "behavior_rmse_manifold_only", "behavior_rmse_manifold_plus_event"),
    value = c(base["r2"], plus["r2"], plus["r2"] - base["r2"], base["rmse"], plus["rmse"]),
    status = "ok",
    note = "Blocked 70/30 behavior decoding comparison: 3D manifold alone versus 3D manifold plus event-state regressors.",
    stringsAsFactors = FALSE
  )
}

stpd_neural_event_onsets <- function(features, states = c("burst", "pause")) {
  features <- as.data.frame(features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !("event_state" %in% names(features))) return(data.frame())
  lab <- as.character(features$event_state)
  lab[is.na(lab) | !nzchar(lab)] <- "unlabeled"
  prev <- c("unlabeled", head(lab, -1L))
  states <- intersect(as.character(states), unique(lab))
  rows <- lapply(states, function(st) {
    idx <- which(lab == st & prev != st)
    if (length(idx) == 0L) return(NULL)
    data.frame(event_state = st, onset_bin_index = idx, onset_bin_id = features$bin_id[idx],
               onset_time_sec = features$time_mid_sec[idx], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)) data.frame() else out
}

stpd_neural_event_triggered_trajectory <- function(pop,
                                                   states = c("burst", "pause"),
                                                   window_bins = 5L) {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !all(c("NM1", "NM2", "NM3", "event_state") %in% names(features))) {
    return(data.frame(message = "No event-triggered manifold trajectory is available.", stringsAsFactors = FALSE))
  }
  window_bins <- max(1L, safe_int(window_bins, 5L))
  onsets <- stpd_neural_event_onsets(features, states = states)
  if (nrow(onsets) == 0L) return(data.frame(message = "No burst/pause onsets are available in the current bins.", stringsAsFactors = FALSE))
  rows <- list()
  for (rr in seq_len(nrow(onsets))) {
    center <- onsets$onset_bin_index[rr]
    st <- onsets$event_state[rr]
    rel <- seq.int(-window_bins, window_bins)
    idx <- center + rel
    keep <- idx >= 1L & idx <= nrow(features)
    if (!any(keep)) next
    sub <- features[idx[keep], , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      event_state = st,
      onset_time_sec = onsets$onset_time_sec[rr],
      rel_bin = rel[keep],
      rel_time_sec = rel[keep] * (pop$bin_sec %||% stats::median(features$bin_width_sec, na.rm = TRUE)),
      NM1 = sub$NM1,
      NM2 = sub$NM2,
      NM3 = sub$NM3,
      latent_speed = sub$latent_speed %||% NA_real_,
      latent_curvature = sub$latent_curvature %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
  long <- do.call(rbind, rows)
  if (is.null(long) || nrow(long) == 0L) return(data.frame(message = "No event-triggered manifold trajectory rows.", stringsAsFactors = FALSE))
  parts <- split(long, list(long$event_state, long$rel_bin), drop = TRUE)
  out <- do.call(rbind, lapply(parts, function(dat) {
    data.frame(
      event_state = dat$event_state[1],
      rel_bin = dat$rel_bin[1],
      rel_time_sec = dat$rel_time_sec[1],
      n_points = sum(stats::complete.cases(dat[, c("NM1", "NM2", "NM3"), drop = FALSE])),
      n_events = length(unique(dat$onset_time_sec)),
      mean_NM1 = mean(dat$NM1, na.rm = TRUE),
      mean_NM2 = mean(dat$NM2, na.rm = TRUE),
      mean_NM3 = mean(dat$NM3, na.rm = TRUE),
      sem_NM1 = stats::sd(dat$NM1, na.rm = TRUE) / sqrt(max(1L, sum(is.finite(dat$NM1)))),
      sem_NM2 = stats::sd(dat$NM2, na.rm = TRUE) / sqrt(max(1L, sum(is.finite(dat$NM2)))),
      sem_NM3 = stats::sd(dat$NM3, na.rm = TRUE) / sqrt(max(1L, sum(is.finite(dat$NM3)))),
      mean_latent_speed = mean(dat$latent_speed, na.rm = TRUE),
      mean_latent_curvature = mean(dat$latent_curvature, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out <- out[order(out$event_state, out$rel_bin), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_neural_task_event_triggered_trajectory <- function(pop) {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  needed <- c("NM1", "NM2", "NM3", "task_event_in_window", "task_event_name", "task_event_rel_time_sec")
  if (nrow(features) == 0L || !all(needed %in% names(features))) {
    return(data.frame(message = "No task-event-triggered manifold trajectory is available.", stringsAsFactors = FALSE))
  }
  keep <- isTRUE(features$task_event_in_window) | features$task_event_in_window
  keep <- keep & stats::complete.cases(features[, c("NM1", "NM2", "NM3"), drop = FALSE]) &
    !is.na(features$task_event_name) & nzchar(as.character(features$task_event_name)) &
    is.finite(suppressWarnings(as.numeric(features$task_event_rel_time_sec)))
  if (!any(keep)) return(data.frame(message = "No manifold bins fall inside the selected task-event peri-event windows.", stringsAsFactors = FALSE))
  dat <- features[keep, , drop = FALSE]
  bin_sec <- suppressWarnings(as.numeric(pop$bin_sec %||% stats::median(features$bin_width_sec, na.rm = TRUE))[1])
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
  dat$rel_bin <- as.integer(round(suppressWarnings(as.numeric(dat$task_event_rel_time_sec)) / bin_sec))
  dat$task_event_name <- as.character(dat$task_event_name)
  dat$task_event_trial_id <- as.character(dat$task_event_trial_id %||% "")
  split_key <- interaction(dat$task_event_name, dat$rel_bin, drop = TRUE)
  parts <- split(dat, split_key, drop = TRUE)
  out <- do.call(rbind, lapply(parts, function(x) {
    data.frame(
      task_event_name = x$task_event_name[1],
      rel_bin = x$rel_bin[1],
      rel_time_sec = mean(suppressWarnings(as.numeric(x$task_event_rel_time_sec)), na.rm = TRUE),
      n_bins = nrow(x),
      n_trials = length(unique(x$task_event_trial_id)),
      mean_NM1 = mean(x$NM1, na.rm = TRUE),
      mean_NM2 = mean(x$NM2, na.rm = TRUE),
      mean_NM3 = mean(x$NM3, na.rm = TRUE),
      mean_latent_speed = mean(suppressWarnings(as.numeric(x$latent_speed %||% NA_real_)), na.rm = TRUE),
      mean_latent_curvature = mean(suppressWarnings(as.numeric(x$latent_curvature %||% NA_real_)), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(out) || nrow(out) == 0L) return(data.frame(message = "No task-event trajectory rows.", stringsAsFactors = FALSE))
  out <- out[order(out$task_event_name, out$rel_bin), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_neural_event_dynamics_summary <- function(pop,
                                               states = c("burst", "pause"),
                                               window_bins = 5L) {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(features) == 0L || !all(c("event_state", "latent_speed", "latent_curvature") %in% names(features))) {
    return(data.frame(message = "No latent speed/curvature event summary is available.", stringsAsFactors = FALSE))
  }
  window_bins <- max(1L, safe_int(window_bins, 5L))
  onsets <- stpd_neural_event_onsets(features, states = states)
  if (nrow(onsets) == 0L) return(data.frame(message = "No event onsets are available.", stringsAsFactors = FALSE))
  rows <- lapply(split(onsets, onsets$event_state), function(oo) {
    pre_speed <- post_speed <- pre_curv <- post_curv <- numeric(0)
    for (center in oo$onset_bin_index) {
      pre <- seq.int(max(1L, center - window_bins), max(1L, center - 1L))
      post <- seq.int(center, min(nrow(features), center + window_bins))
      pre <- pre[pre < center]
      if (length(pre) > 0L) {
        pre_speed <- c(pre_speed, mean(features$latent_speed[pre], na.rm = TRUE))
        pre_curv <- c(pre_curv, mean(features$latent_curvature[pre], na.rm = TRUE))
      }
      if (length(post) > 0L) {
        post_speed <- c(post_speed, mean(features$latent_speed[post], na.rm = TRUE))
        post_curv <- c(post_curv, mean(features$latent_curvature[post], na.rm = TRUE))
      }
    }
    data.frame(
      event_state = oo$event_state[1],
      n_onsets = nrow(oo),
      pre_speed_mean = mean(pre_speed, na.rm = TRUE),
      post_speed_mean = mean(post_speed, na.rm = TRUE),
      delta_speed_post_minus_pre = mean(post_speed, na.rm = TRUE) - mean(pre_speed, na.rm = TRUE),
      pre_curvature_mean = mean(pre_curv, na.rm = TRUE),
      post_curvature_mean = mean(post_curv, na.rm = TRUE),
      delta_curvature_post_minus_pre = mean(post_curv, na.rm = TRUE) - mean(pre_curv, na.rm = TRUE),
      note = "Latent speed/curvature around event onset; compare against time-shift controls before biological interpretation.",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stpd_neural_event_validation_rows <- function(pop, seed = 1L, n_perm = 199L) {
  features <- as.data.frame(pop$features %||% data.frame(), stringsAsFactors = FALSE)
  dec <- stpd_neural_event_label_decoding(features, seed = seed, n_perm = n_perm)
  dist <- stpd_neural_event_distance_tests(pop, states = c("burst", "pause"), n_perm = n_perm, seed = seed)
  if (is.data.frame(dist) && all(c("state_a", "state_b", "centroid_distance") %in% names(dist))) {
    bp <- dist[dist$state_a == "burst" & dist$state_b == "pause", , drop = FALSE]
    if (nrow(bp) > 0L) {
      bp_rows <- data.frame(
        metric = c("burst_pause_centroid_distance", "burst_pause_event_label_shuffle_p", "burst_pause_circular_time_shift_p"),
        value = c(bp$centroid_distance[1], bp$event_label_shuffle_p[1], bp$circular_time_shift_p[1]),
        status = "ok",
        note = c(
          "Distance between burst and pause centroids in the 3D manifold.",
          "Permutation p-value for burst-pause centroid distance under shuffled event labels.",
          "Circular time-shift p-value preserving label run structure while breaking exact alignment."
        ),
        stringsAsFactors = FALSE
      )
      dec <- rbind(dec, bp_rows)
    }
  }
  rbind(dec, stpd_neural_behavior_decoding_with_events(features))
}

stpd_neural_manifold_validation <- function(pop,
                                            seed = 1L,
                                            n_neighbors = 10L,
                                            event_permutations = 199L) {
  features <- pop$features %||% data.frame()
  if (nrow(features) == 0L || !all(c("NM1", "NM2", "NM3") %in% names(features))) {
    return(data.frame(metric = "validation", value = NA_real_, status = "skipped",
                      note = "No 3D manifold coordinates are available.", stringsAsFactors = FALSE))
  }
  X <- as.matrix(pop$X)
  Y <- as.matrix(features[, c("NM1", "NM2", "NM3"), drop = FALSE])
  ok <- stats::complete.cases(Y)
  metric_rows <- data.frame(
    metric = c("trustworthiness", "continuity"),
    value = c(stpd_neural_trustworthiness(X[ok, , drop = FALSE], Y[ok, , drop = FALSE], k = n_neighbors),
              stpd_neural_trustworthiness(Y[ok, , drop = FALSE], X[ok, , drop = FALSE], k = n_neighbors)),
    status = "ok",
    note = c("Neighborhood preservation from population space to manifold.", "Neighborhood preservation from manifold back to population space."),
    stringsAsFactors = FALSE
  )
  rbind(
    metric_rows,
    stpd_neural_heldout_neuron_prediction(pop),
    stpd_neural_behavior_decoding(features),
    stpd_neural_shuffle_controls(pop, seed = seed),
    stpd_neural_event_validation_rows(pop, seed = seed, n_perm = event_permutations),
    data.frame(metric = c("bin_width_sec", "smoothing_sigma_bins", "embedding_seed"),
               value = c(pop$bin_sec %||% NA_real_, pop$smoothing_sigma_bins %||% NA_real_, seed),
               status = "reported",
               note = c("Run sensitivity by changing this control and comparing validation metrics.",
                        "Run sensitivity by changing this control and comparing validation metrics.",
                        "For UMAP/t-SNE, repeat with different seeds and compare trustworthiness/behavior decoding."),
               stringsAsFactors = FALSE)
  )
}

stpd_neural_manifold_plot <- function(pop) {
  dat <- pop$features %||% data.frame()
  if (is.null(dat) || nrow(dat) == 0L || !all(c("NM1", "NM2", "NM3") %in% names(dat))) {
    return(layout(plot_ly(), scene = list(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), zaxis = list(visible = FALSE)),
                  annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                          text = "No neural manifold coordinates available.", showarrow = FALSE))))
  }
  ok <- is.finite(dat$NM1) & is.finite(dat$NM2) & is.finite(dat$NM3)
  dat <- dat[ok, , drop = FALSE]
  if (nrow(dat) < 2L) {
    return(layout(plot_ly(), annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                                     text = "Need at least two embedded bins.", showarrow = FALSE))))
  }
  color_var <- if ("behavior_numeric" %in% names(dat) && sum(is.finite(dat$behavior_numeric)) >= 3L) dat$behavior_numeric else dat$time_mid_sec
  color_title <- if ("behavior_numeric" %in% names(dat) && sum(is.finite(dat$behavior_numeric)) >= 3L) "behavior" else "time (s)"
  dat$hover <- paste0(
    "bin ", dat$bin_id,
    "<br>time: ", signif(dat$bin_start_sec, 5), "-", signif(dat$bin_end_sec, 5), " s",
    "<br>population rate: ", signif(dat$population_rate_hz, 4), " Hz/neuron",
    if ("behavior_value" %in% names(dat)) paste0("<br>behavior: ", dat$behavior_value) else "",
    if ("task_event_in_window" %in% names(dat)) ifelse(
      isTRUE(dat$task_event_in_window) | dat$task_event_in_window,
      paste0(
        "<br>task event: ", dat$task_event_name,
        "<br>event-relative time: ", signif(dat$task_event_rel_time_sec, 5), " s",
        "<br>event epoch: ", dat$task_event_epoch
      ),
      ""
    ) else ""
  )
  plot_ly(dat, x = ~NM1, y = ~NM2, z = ~NM3, type = "scatter3d", mode = "lines+markers",
          line = list(color = "rgba(71,85,105,0.45)", width = 4),
          marker = list(size = 4.2, color = color_var, colorscale = "Viridis",
                        colorbar = list(title = color_title), line = list(color = "#ffffff", width = 0.4)),
          text = ~hover, hoverinfo = "text", source = "neural_manifold_plot") %>%
    layout(
      title = list(text = paste0("Neural manifold: ", pop$method_label %||% pop$method %||% ""), x = 0.02, font = list(size = 15)),
      scene = list(
        xaxis = list(title = "NM1", backgroundcolor = "#ffffff", gridcolor = "#e5e7eb"),
        yaxis = list(title = "NM2", backgroundcolor = "#ffffff", gridcolor = "#e5e7eb"),
        zaxis = list(title = "NM3", backgroundcolor = "#ffffff", gridcolor = "#e5e7eb")
      ),
      margin = list(l = 0, r = 0, t = 60, b = 10),
      paper_bgcolor = "#ffffff"
    ) %>%
    config(displaylogo = FALSE, scrollZoom = TRUE)
}

stpd_neural_manifold_method_notes <- function() {
  data.frame(
    layer = c("Basic", "Basic", "Temporal", "Nonlinear", "Nonlinear", "Nonlinear", "Nonlinear", "Supervised", "Validation"),
    method = c("PCA", "FA", "GPFA", "UMAP / PHATE / Isomap / t-SNE", "PHATE", "t-SNE", "UMAP", "CEBRA / supervised manifold", "Controls"),
    recommendation = c(
      "Transparent linear baseline for population activity.",
      "Better neural population baseline when shared variability and private noise matter.",
      "Use full GPFA for publication-grade single-trial trajectories; this panel provides a GPFA-style smoothed FA preview.",
      "Use as exploratory views, not standalone proof of a biological manifold.",
      "Useful for continuous progressions and branch-like structure.",
      "Local-neighborhood visualization; global geometry is fragile.",
      "Common manifold-learning view; check seed and neighbor sensitivity.",
      "Use behavior or movement labels to ask whether neural geometry encodes task variables; this panel provides a behavior-guided proxy and recommends external CEBRA for final contrastive embedding.",
      "Held-out neuron prediction, behavior decoding, event-state centroid/dispersion, event-triggered trajectories, shuffles, and parameter sensitivity matter more than pretty 3D plots."
    ),
    source = c(
      "Cunningham & Yu 2014",
      "Cunningham & Yu 2014",
      "Yu et al. 2009",
      "Manifold-learning visualization literature",
      "Moon et al. 2019",
      "van der Maaten & Hinton 2008",
      "McInnes et al. 2018",
      "Schneider et al. 2023",
      "Internal validation contract"
    ),
    stringsAsFactors = FALSE
  )
}
