# ISI state-space analysis for single-train trajectory diagnostics.

stpd_state_space_pattern_labels <- function(dat,
                                            label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                            min_isi_sec = 0.001,
                                            auto_others = FALSE) {
  label_source <- match.arg(label_source)
  n <- if (is.null(dat)) 0L else nrow(dat)
  if (n == 0) return(character(0))
  manual <- dat$pattern_manual %||% rep("", n)
  auto <- dat$pattern_auto %||% rep("", n)
  isi <- dat$ISI_sec %||% rep(NA_real_, n)
  manual <- rep_len(as.character(manual), n)
  auto <- rep_len(as.character(auto), n)
  isi <- rep_len(suppressWarnings(as.numeric(isi)), n)

  out <- switch(
    label_source,
    audit_final = stpd_audit_final_labels(dat, min_isi_sec = min_isi_sec,
                                          auto_others = auto_others,
                                          prefer_stored = TRUE),
    final = compute_final_pattern(manual, auto, isi, auto_others = auto_others, min_isi_sec = min_isi_sec),
    manual_priority = compute_final_pattern(manual, auto, isi, auto_others = auto_others, min_isi_sec = min_isi_sec),
    auto = {
      x <- auto
      if (isTRUE(auto_others)) x <- fill_unlabeled_others_for_display(x, isi, min_isi_sec = min_isi_sec)
      x
    },
    manual = manual
  )
  out <- as.character(out)
  out[is.na(out) | !nzchar(out)] <- "unlabeled"
  out
}

stpd_state_space_cv2 <- function(x, min_isi_sec = 0.001) {
  x <- valid_isi_values(x, min_isi_sec = min_isi_sec)
  if (length(x) < 2) return(NA_real_)
  a <- head(x, -1)
  b <- tail(x, -1)
  denom <- a + b
  ok <- is.finite(denom) & denom > 0
  if (!any(ok)) return(NA_real_)
  mean(2 * abs(a[ok] - b[ok]) / denom[ok], na.rm = TRUE)
}

stpd_winsorize_numeric <- function(x, probs = c(0.01, 0.99)) {
  x <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x)
  if (sum(ok) < 4) return(x)
  qs <- suppressWarnings(stats::quantile(x[ok], probs = probs, na.rm = TRUE, names = FALSE))
  if (length(qs) != 2 || any(!is.finite(qs)) || qs[2] <= qs[1]) return(x)
  x[ok] <- pmin(pmax(x[ok], qs[1]), qs[2])
  x
}

stpd_make_isi_state_space_features <- function(dat,
                                               train = "",
                                               label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                               k = 3L,
                                               min_isi_sec = 0.001,
                                               auto_others = FALSE,
                                               winsorize = TRUE,
                                               winsor_probs = c(0.01, 0.99)) {
  label_source <- match.arg(label_source)
  if (is.null(dat) || nrow(dat) < 3) return(data.frame())
  if (!("timestamp_sec" %in% names(dat))) return(data.frame())
  dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
  n <- nrow(dat)
  k <- max(1L, safe_int(k, 3L))
  idx_values <- dat$idx %||% seq_len(n)
  idx_values <- rep_len(suppressWarnings(as.integer(idx_values)), n)
  ts <- rep_len(suppressWarnings(as.numeric(dat$timestamp_sec)), n)
  isi <- rep_len(suppressWarnings(as.numeric(dat$ISI_sec %||% c(NA_real_, diff(ts)))), n)
  labels <- stpd_state_space_pattern_labels(dat, label_source = label_source,
                                            min_isi_sec = min_isi_sec,
                                            auto_others = auto_others)
  log_isi <- rep(NA_real_, n)
  valid <- is.finite(isi) & isi >= min_isi_sec
  log_isi[valid] <- log10(isi[valid])
  log_feature <- if (isTRUE(winsorize)) stpd_winsorize_numeric(log_isi, winsor_probs) else log_isi
  rows <- which(valid & seq_len(n) >= 2L & is.finite(ts) & is.finite(c(NA_real_, head(ts, -1))))
  if (length(rows) == 0) return(data.frame())

  lag_offsets <- seq.int(-k, k)
  lag_names <- ifelse(lag_offsets < 0,
                      paste0("lag_m", abs(lag_offsets)),
                      ifelse(lag_offsets > 0, paste0("lag_p", lag_offsets), "lag_0"))

  out <- vector("list", length(rows))
  for (rr in seq_along(rows)) {
    i <- rows[rr]
    left_time <- ts[i - 1L]
    right_time <- ts[i]
    window_idx <- seq.int(max(1L, i - k), min(n, i + k))
    local_isi <- valid_isi_values(isi[window_idx], min_isi_sec = min_isi_sec)
    local_log <- log_feature[window_idx]
    local_log <- local_log[is.finite(local_log)]
    lag_vals <- rep(NA_real_, length(lag_offsets))
    for (jj in seq_along(lag_offsets)) {
      pos <- i + lag_offsets[jj]
      if (pos >= 1L && pos <= n) lag_vals[jj] <- log_feature[pos]
    }
    names(lag_vals) <- lag_names
    prev_isi <- if (i > 1L) isi[i - 1L] else NA_real_
    next_isi <- if (i < n) isi[i + 1L] else NA_real_
    flank_vals <- valid_isi_values(c(prev_isi, next_isi), min_isi_sec = min_isi_sec)
    current_isi <- isi[i]
    local_mean_isi <- if (length(local_isi) > 0) mean(local_isi, na.rm = TRUE) else NA_real_
    local_median_isi <- if (length(local_isi) > 0) stats::median(local_isi, na.rm = TRUE) else NA_real_
    out[[rr]] <- c(
      list(
        train = as.character(train %||% ""),
        row_number = i,
        idx = idx_values[i],
        left_idx = idx_values[i - 1L],
        right_idx = idx_values[i],
        left_time_sec = left_time,
        right_time_sec = right_time,
        time_mid_sec = (left_time + right_time) / 2,
        ISI_sec = current_isi,
        log_isi = log_isi[i],
        log_isi_feature = log_feature[i],
        label = labels[i],
        label_source = label_source
      ),
      as.list(lag_vals),
      list(
        local_median_logisi = if (length(local_log) > 0) stats::median(local_log, na.rm = TRUE) else NA_real_,
        local_mean_logisi = if (length(local_log) > 0) mean(local_log, na.rm = TRUE) else NA_real_,
        local_sd_logisi = if (length(local_log) >= 2) stats::sd(local_log, na.rm = TRUE) else NA_real_,
        local_q10_logisi = if (length(local_log) > 0) safe_q(local_log, 0.10)[1] else NA_real_,
        local_q90_logisi = if (length(local_log) > 0) safe_q(local_log, 0.90)[1] else NA_real_,
        local_iqr_logisi = if (length(local_log) > 0) diff(safe_q(local_log, c(0.25, 0.75))) else NA_real_,
        local_mean_isi_sec = local_mean_isi,
        local_median_isi_sec = local_median_isi,
        local_rate_hz = if (is.finite(local_median_isi) && local_median_isi > 0) 1 / local_median_isi else NA_real_,
        local_cv = if (length(local_isi) >= 2) calc_CV(local_isi) else NA_real_,
        local_lv = if (length(local_isi) >= 2) calc_LV(local_isi) else NA_real_,
        local_cv2 = stpd_state_space_cv2(local_isi, min_isi_sec = min_isi_sec),
        delta_logisi = if (i > 2L && is.finite(log_feature[i - 1L])) log_feature[i] - log_feature[i - 1L] else NA_real_,
        next_delta_logisi = if (i < n && is.finite(log_feature[i + 1L])) log_feature[i + 1L] - log_feature[i] else NA_real_,
        prepost_ratio = if (length(flank_vals) > 0 && is.finite(current_isi) && current_isi > 0) stats::median(flank_vals) / current_isi else NA_real_
      )
    )
  }
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  character_cols <- intersect(c("train", "label", "label_source"), names(out))
  for (nm in character_cols) out[[nm]] <- as.character(unlist(out[[nm]], use.names = FALSE))
  numeric_cols <- setdiff(names(out), c("train", "label", "label_source"))
  for (nm in numeric_cols) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  out
}

stpd_isi_pca_feature_columns <- function(features) {
  if (is.null(features) || nrow(features) == 0) return(character(0))
  prefixes <- c("^lag_", "^local_", "^delta_logisi$", "^next_delta_logisi$",
                "^prepost_ratio$", "^log_isi_feature$")
  cols <- names(features)[vapply(names(features), function(nm) {
    any(grepl(paste(prefixes, collapse = "|"), nm))
  }, logical(1))]
  cols[vapply(features[cols], is.numeric, logical(1))]
}

stpd_scale_matrix_for_pca <- function(X, scaling = c("robust", "zscore")) {
  scaling <- match.arg(scaling)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  for (j in seq_len(ncol(X))) {
    col <- X[, j]
    med <- stats::median(col[is.finite(col)], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    col[!is.finite(col)] <- med
    X[, j] <- col
  }
  if (identical(scaling, "robust")) {
    center <- apply(X, 2, stats::median, na.rm = TRUE)
    scale <- apply(X, 2, stats::mad, na.rm = TRUE, constant = 1.4826)
    fallback <- apply(X, 2, stats::sd, na.rm = TRUE)
    scale[!is.finite(scale) | scale <= .Machine$double.eps] <- fallback[!is.finite(scale) | scale <= .Machine$double.eps]
  } else {
    center <- colMeans(X, na.rm = TRUE)
    scale <- apply(X, 2, stats::sd, na.rm = TRUE)
  }
  scale[!is.finite(scale) | scale <= .Machine$double.eps] <- 1
  sweep(sweep(X, 2, center, "-"), 2, scale, "/")
}

stpd_run_isi_state_pca <- function(features,
                                   feature_cols = NULL,
                                   scaling = c("robust", "zscore")) {
  scaling <- match.arg(scaling)
  if (is.null(features) || nrow(features) < 2) {
    stop("Need at least two ISI feature rows for PCA.", call. = FALSE)
  }
  feature_cols <- feature_cols %||% stpd_isi_pca_feature_columns(features)
  feature_cols <- intersect(feature_cols, names(features))
  feature_cols <- feature_cols[vapply(features[feature_cols], is.numeric, logical(1))]
  varying <- vapply(feature_cols, function(nm) {
    x <- suppressWarnings(as.numeric(features[[nm]]))
    x <- x[is.finite(x)]
    length(x) >= 2 && length(unique(signif(x, 12))) >= 2
  }, logical(1))
  feature_cols <- feature_cols[varying]
  if (length(feature_cols) < 2) {
    stop("Need at least two varying numeric ISI features for PCA.", call. = FALSE)
  }
  X_scaled <- stpd_scale_matrix_for_pca(features[, feature_cols, drop = FALSE], scaling = scaling)
  pca <- stats::prcomp(X_scaled, center = FALSE, scale. = FALSE)
  n_score_pc <- min(3L, ncol(pca$x))
  scores <- as.data.frame(pca$x[, seq_len(n_score_pc), drop = FALSE], stringsAsFactors = FALSE)
  for (pc in paste0("PC", 1:3)) {
    if (!(pc %in% names(scores))) scores[[pc]] <- NA_real_
  }
  scores <- cbind(features, scores[, paste0("PC", 1:3), drop = FALSE])
  sdev2 <- pca$sdev^2
  total <- sum(sdev2)
  variance <- data.frame(
    PC = paste0("PC", seq_along(sdev2)),
    variance = if (is.finite(total) && total > 0) sdev2 / total else NA_real_,
    cumulative = if (is.finite(total) && total > 0) cumsum(sdev2 / total) else NA_real_,
    stringsAsFactors = FALSE
  )
  n_loading_pc <- min(3L, ncol(pca$rotation))
  loadings <- as.data.frame(pca$rotation[, seq_len(n_loading_pc), drop = FALSE],
                            stringsAsFactors = FALSE)
  for (pc in paste0("PC", 1:3)) {
    if (!(pc %in% names(loadings))) loadings[[pc]] <- NA_real_
  }
  loadings$feature <- rownames(loadings)
  rownames(loadings) <- NULL
  loadings <- loadings[, c("feature", "PC1", "PC2", "PC3"), drop = FALSE]
  list(
    scores = scores,
    loadings = loadings,
    variance = variance,
    pca = pca,
    feature_cols = feature_cols,
    scaling = scaling
  )
}

stpd_isomap_knn_graph <- function(dmat, n_neighbors = 15L) {
  dmat <- as.matrix(dmat)
  n <- nrow(dmat)
  if (!is.finite(n) || n < 3) stop("Need at least three points for Isomap.", call. = FALSE)
  k <- max(2L, min(n - 1L, safe_int(n_neighbors, 15L)))
  adj <- matrix(Inf, nrow = n, ncol = n)
  diag(adj) <- 0
  for (i in seq_len(n)) {
    row <- dmat[i, ]
    row[i] <- Inf
    nn <- order(row, na.last = NA)
    nn <- nn[is.finite(row[nn])]
    if (length(nn) == 0) next
    nn <- head(nn, k)
    adj[i, nn] <- row[nn]
  }
  adj <- pmin(adj, t(adj), na.rm = TRUE)
  neighbors <- vector("list", n)
  weights <- vector("list", n)
  all_idx <- seq_len(n)
  for (i in seq_len(n)) {
    idx <- all_idx[is.finite(adj[i, ]) & all_idx != i]
    neighbors[[i]] <- as.integer(idx)
    weights[[i]] <- as.numeric(adj[i, idx])
  }
  list(neighbors = neighbors, weights = weights, n_neighbors = k)
}

stpd_isomap_components <- function(neighbors) {
  n <- length(neighbors)
  comp <- integer(n)
  comp_id <- 0L
  sizes <- integer(0)
  for (i in seq_len(n)) {
    if (comp[i] != 0L) next
    comp_id <- comp_id + 1L
    stack <- i
    comp[i] <- comp_id
    size <- 0L
    while (length(stack) > 0) {
      v <- stack[length(stack)]
      stack <- stack[-length(stack)]
      size <- size + 1L
      nb <- neighbors[[v]]
      if (length(nb) == 0) next
      nb <- nb[comp[nb] == 0L]
      if (length(nb) > 0) {
        comp[nb] <- comp_id
        stack <- c(stack, nb)
      }
    }
    sizes[comp_id] <- size
  }
  list(component = comp, sizes = sizes)
}

stpd_isomap_subset_graph <- function(neighbors, weights, keep) {
  keep <- as.integer(keep)
  map <- integer(length(neighbors))
  map[keep] <- seq_along(keep)
  new_neighbors <- vector("list", length(keep))
  new_weights <- vector("list", length(keep))
  for (ii in seq_along(keep)) {
    old <- keep[ii]
    nb <- neighbors[[old]]
    wt <- weights[[old]]
    if (length(nb) == 0) {
      new_neighbors[[ii]] <- integer(0)
      new_weights[[ii]] <- numeric(0)
      next
    }
    ok <- map[nb] > 0L
    new_neighbors[[ii]] <- as.integer(map[nb[ok]])
    new_weights[[ii]] <- as.numeric(wt[ok])
  }
  list(neighbors = new_neighbors, weights = new_weights)
}

stpd_isomap_dijkstra <- function(neighbors, weights, source) {
  n <- length(neighbors)
  dist <- rep(Inf, n)
  visited <- rep(FALSE, n)
  dist[source] <- 0

  heap_nodes <- integer(max(16L, 4L * n))
  heap_dists <- numeric(length(heap_nodes))
  heap_size <- 0L

  push <- function(node, value) {
    if (heap_size >= length(heap_nodes)) {
      heap_nodes <<- c(heap_nodes, integer(length(heap_nodes)))
      heap_dists <<- c(heap_dists, numeric(length(heap_dists)))
    }
    heap_size <<- heap_size + 1L
    pos <- heap_size
    heap_nodes[pos] <<- as.integer(node)
    heap_dists[pos] <<- as.numeric(value)
    while (pos > 1L) {
      parent <- pos %/% 2L
      if (heap_dists[parent] <= heap_dists[pos]) break
      tmp_n <- heap_nodes[parent]; tmp_d <- heap_dists[parent]
      heap_nodes[parent] <<- heap_nodes[pos]; heap_dists[parent] <<- heap_dists[pos]
      heap_nodes[pos] <<- tmp_n; heap_dists[pos] <<- tmp_d
      pos <- parent
    }
  }

  pop <- function() {
    if (heap_size <= 0L) return(NULL)
    out_node <- heap_nodes[1L]
    out_dist <- heap_dists[1L]
    heap_nodes[1L] <<- heap_nodes[heap_size]
    heap_dists[1L] <<- heap_dists[heap_size]
    heap_size <<- heap_size - 1L
    pos <- 1L
    repeat {
      left <- pos * 2L
      right <- left + 1L
      if (left > heap_size) break
      small <- left
      if (right <= heap_size && heap_dists[right] < heap_dists[left]) small <- right
      if (heap_dists[pos] <= heap_dists[small]) break
      tmp_n <- heap_nodes[pos]; tmp_d <- heap_dists[pos]
      heap_nodes[pos] <<- heap_nodes[small]; heap_dists[pos] <<- heap_dists[small]
      heap_nodes[small] <<- tmp_n; heap_dists[small] <<- tmp_d
      pos <- small
    }
    list(node = out_node, dist = out_dist)
  }

  push(source, 0)
  while (heap_size > 0L) {
    item <- pop()
    v <- item$node
    if (visited[v]) next
    visited[v] <- TRUE
    nb <- neighbors[[v]]
    wt <- weights[[v]]
    if (length(nb) == 0) next
    for (jj in seq_along(nb)) {
      u <- nb[jj]
      if (visited[u]) next
      alt <- item$dist + wt[jj]
      if (alt < dist[u]) {
        dist[u] <- alt
        push(u, alt)
      }
    }
  }
  dist
}

stpd_isomap_all_pairs_shortest_paths <- function(neighbors, weights) {
  n <- length(neighbors)
  geo <- matrix(Inf, nrow = n, ncol = n)
  for (i in seq_len(n)) geo[i, ] <- stpd_isomap_dijkstra(neighbors, weights, i)
  diag(geo) <- 0
  geo
}

stpd_run_isi_state_isomap <- function(features,
                                      n_neighbors = 15L,
                                      ndim = 3L,
                                      max_points = 900L,
                                      feature_cols = NULL,
                                      scaling = c("robust", "zscore"),
                                      component = c("largest", "error")) {
  scaling <- match.arg(scaling)
  component <- match.arg(component)
  if (is.null(features) || nrow(features) < 5) {
    stop("Need at least five ISI feature rows for Isomap.", call. = FALSE)
  }
  max_points <- max(20L, safe_int(max_points, 900L))
  sample_index <- seq_len(nrow(features))
  sampled <- FALSE
  if (nrow(features) > max_points) {
    sample_index <- sort(unique(round(seq(1, nrow(features), length.out = max_points))))
    sampled <- TRUE
  }
  features0 <- features[sample_index, , drop = FALSE]
  feature_cols <- feature_cols %||% stpd_isi_pca_feature_columns(features0)
  feature_cols <- intersect(feature_cols, names(features0))
  feature_cols <- feature_cols[vapply(features0[feature_cols], is.numeric, logical(1))]
  varying <- vapply(feature_cols, function(nm) {
    x <- suppressWarnings(as.numeric(features0[[nm]]))
    x <- x[is.finite(x)]
    length(x) >= 2 && length(unique(signif(x, 12))) >= 2
  }, logical(1))
  feature_cols <- feature_cols[varying]
  if (length(feature_cols) < 2) {
    stop("Need at least two varying numeric ISI features for Isomap.", call. = FALSE)
  }

  X_scaled <- stpd_scale_matrix_for_pca(features0[, feature_cols, drop = FALSE], scaling = scaling)
  n <- nrow(X_scaled)
  k <- max(2L, min(n - 1L, safe_int(n_neighbors, 15L)))
  dmat <- as.matrix(stats::dist(X_scaled))
  graph <- stpd_isomap_knn_graph(dmat, n_neighbors = k)
  comps <- stpd_isomap_components(graph$neighbors)
  component_n <- length(comps$sizes)
  largest_id <- if (length(comps$sizes) > 0) which.max(comps$sizes) else 1L
  keep <- which(comps$component == largest_id)
  if (component_n > 1L) {
    if (identical(component, "error")) {
      stop("Isomap kNN graph is disconnected; increase n_neighbors.", call. = FALSE)
    }
    sub_graph <- stpd_isomap_subset_graph(graph$neighbors, graph$weights, keep)
    graph$neighbors <- sub_graph$neighbors
    graph$weights <- sub_graph$weights
    features0 <- features0[keep, , drop = FALSE]
  } else {
    keep <- seq_len(nrow(features0))
  }
  if (nrow(features0) < 5) {
    stop("Largest Isomap component has too few points; increase n_neighbors.", call. = FALSE)
  }

  geo <- stpd_isomap_all_pairs_shortest_paths(graph$neighbors, graph$weights)
  if (any(!is.finite(geo))) {
    stop("Isomap geodesic graph remains disconnected; increase n_neighbors.", call. = FALSE)
  }
  ndim <- max(2L, min(3L, safe_int(ndim, 3L), nrow(features0) - 1L))
  mds <- stats::cmdscale(stats::as.dist(geo), k = ndim, eig = TRUE, add = FALSE)
  points <- as.data.frame(mds$points, stringsAsFactors = FALSE)
  names(points) <- paste0("Isomap", seq_len(ncol(points)))
  for (nm in paste0("Isomap", 1:3)) {
    if (!(nm %in% names(points))) points[[nm]] <- NA_real_
  }

  emb_dist <- as.matrix(stats::dist(points[, paste0("Isomap", seq_len(ndim)), drop = FALSE]))
  geo_vec <- geo[upper.tri(geo)]
  emb_vec <- emb_dist[upper.tri(emb_dist)]
  corr <- suppressWarnings(stats::cor(geo_vec, emb_vec, use = "complete.obs"))
  residual_variance <- if (is.finite(corr)) 1 - corr^2 else NA_real_
  stress <- if (sum(geo_vec^2, na.rm = TRUE) > 0) {
    sqrt(sum((geo_vec - emb_vec)^2, na.rm = TRUE) / sum(geo_vec^2, na.rm = TRUE))
  } else NA_real_

  scores <- cbind(features0, points[, paste0("Isomap", 1:3), drop = FALSE])
  diagnostics <- data.frame(
    n_input = nrow(features),
    n_sampled = length(sample_index),
    n_embedded = nrow(scores),
    sampled = sampled,
    n_neighbors = graph$n_neighbors,
    component_n = component_n,
    largest_component_n = length(keep),
    residual_variance = residual_variance,
    stress = stress,
    scaling = scaling,
    feature_n = length(feature_cols),
    stringsAsFactors = FALSE
  )
  list(
    scores = scores,
    diagnostics = diagnostics,
    feature_cols = feature_cols,
    sample_index = sample_index,
    kept_sample_index = sample_index[keep],
    geodesic_distance = geo,
    scaling = scaling
  )
}

stpd_make_logisi_phase_portrait <- function(dat,
                                            train = "",
                                            label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                            min_isi_sec = 0.001,
                                            auto_others = FALSE,
                                            lag = 1L,
                                            winsorize = FALSE,
                                            winsor_probs = c(0.01, 0.99)) {
  label_source <- match.arg(label_source)
  if (is.null(dat) || nrow(dat) < 4) return(data.frame())
  if (!("timestamp_sec" %in% names(dat))) return(data.frame())
  dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
  n <- nrow(dat)
  lag <- max(1L, safe_int(lag, 1L))
  idx_values <- dat$idx %||% seq_len(n)
  idx_values <- rep_len(suppressWarnings(as.integer(idx_values)), n)
  ts <- rep_len(suppressWarnings(as.numeric(dat$timestamp_sec)), n)
  isi <- rep_len(suppressWarnings(as.numeric(dat$ISI_sec %||% c(NA_real_, diff(ts)))), n)
  labels <- stpd_state_space_pattern_labels(dat, label_source = label_source,
                                            min_isi_sec = min_isi_sec,
                                            auto_others = auto_others)
  valid <- is.finite(isi) & isi >= min_isi_sec
  log_isi <- rep(NA_real_, n)
  log_isi[valid] <- log10(isi[valid])
  if (isTRUE(winsorize)) log_isi <- stpd_winsorize_numeric(log_isi, winsor_probs)
  rows <- which(seq_len(n) >= 2L & (seq_len(n) + lag) <= n & valid & valid[seq_len(n) + lag])
  rows <- rows[is.finite(ts[rows]) & is.finite(ts[rows - 1L])]
  if (length(rows) == 0) return(data.frame())
  nxt <- rows + lag
  data.frame(
    train = as.character(train %||% ""),
    row_number = rows,
    idx = idx_values[rows],
    next_idx = idx_values[nxt],
    left_time_sec = ts[rows - 1L],
    right_time_sec = ts[rows],
    time_mid_sec = (ts[rows - 1L] + ts[rows]) / 2,
    ISI_sec = isi[rows],
    next_ISI_sec = isi[nxt],
    logISI_i = log_isi[rows],
    logISI_next = log_isi[nxt],
    label = labels[rows],
    next_label = labels[nxt],
    transition = paste0(labels[rows], " -> ", labels[nxt]),
    label_source = label_source,
    stringsAsFactors = FALSE
  )
}
