# State dynamics evidence, exploration, and model helpers.

stpd_state_sequence <- function(dat = NULL,
                                labels = NULL,
                                train = "",
                                label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                min_isi_sec = 0.001,
                                auto_others = FALSE,
                                drop_unlabeled = FALSE) {
  label_source <- match.arg(label_source)
  if (is.null(labels)) {
    if (is.null(dat) || nrow(dat) < 3) return(data.frame())
    labels <- stpd_state_space_pattern_labels(
      dat,
      label_source = label_source,
      min_isi_sec = min_isi_sec,
      auto_others = auto_others
    )
  }
  labels <- as.character(labels)
  labels[is.na(labels) | !nzchar(labels)] <- "unlabeled"

  if (is.null(dat)) {
    rows <- seq_along(labels)
    out <- data.frame(
      train = as.character(train %||% ""),
      position = rows,
      row_number = rows,
      idx = rows,
      time_mid_sec = NA_real_,
      duration_isi_sec = NA_real_,
      label = labels,
      label_source = label_source,
      stringsAsFactors = FALSE
    )
  } else {
    dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
    n <- min(nrow(dat), length(labels))
    ts <- rep_len(suppressWarnings(as.numeric(dat$timestamp_sec %||% NA_real_)), nrow(dat))
    isi <- rep_len(suppressWarnings(as.numeric(dat$ISI_sec %||% c(NA_real_, diff(ts)))), nrow(dat))
    idx_values <- rep_len(suppressWarnings(as.integer(dat$idx %||% seq_len(nrow(dat)))), nrow(dat))
    valid <- is.finite(isi) & isi >= min_isi_sec
    rows <- which(seq_len(n) >= 2L & valid[seq_len(n)])
    if (length(rows) == 0) return(data.frame())
    out <- data.frame(
      train = as.character(train %||% ""),
      position = seq_along(rows),
      row_number = rows,
      idx = idx_values[rows],
      time_mid_sec = ifelse(is.finite(ts[rows]) & is.finite(ts[rows - 1L]),
                            (ts[rows] + ts[rows - 1L]) / 2, NA_real_),
      duration_isi_sec = isi[rows],
      label = labels[rows],
      label_source = label_source,
      stringsAsFactors = FALSE
    )
  }

  if (isTRUE(drop_unlabeled)) {
    out <- out[!(out$label %in% c("unlabeled", "others", "artifact", "")), , drop = FALSE]
    out$position <- seq_len(nrow(out))
  }
  rownames(out) <- NULL
  out
}

stpd_state_transition_matrix <- function(x,
                                         states = NULL,
                                         smoothing = 0,
                                         normalize = c("row", "joint", "none"),
                                         drop_self = FALSE) {
  normalize <- match.arg(normalize)
  labels <- if (is.data.frame(x)) as.character(x$label) else as.character(x)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) < 2) {
    states <- states %||% sort(unique(labels))
    mat <- matrix(0, nrow = length(states), ncol = length(states),
                  dimnames = list(from = states, to = states))
    return(list(matrix = mat, counts = mat, table = data.frame(), states = states,
                normalize = normalize, smoothing = smoothing, drop_self = drop_self))
  }
  states <- states %||% sort(unique(labels))
  states <- as.character(states)
  smoothing <- suppressWarnings(as.numeric(smoothing %||% 0))
  if (!is.finite(smoothing) || smoothing < 0) smoothing <- 0
  counts <- matrix(smoothing, nrow = length(states), ncol = length(states),
                   dimnames = list(from = states, to = states))
  from <- head(labels, -1)
  to <- tail(labels, -1)
  keep <- from %in% states & to %in% states
  if (isTRUE(drop_self)) keep <- keep & from != to
  from <- from[keep]
  to <- to[keep]
  if (length(from) > 0) {
    tab <- table(factor(from, levels = states), factor(to, levels = states))
    counts <- counts + matrix(as.numeric(tab), nrow = length(states), ncol = length(states),
                              dimnames = dimnames(counts))
  }
  if (isTRUE(drop_self) && nrow(counts) == ncol(counts)) {
    diag(counts) <- 0
  }

  mat <- counts
  if (identical(normalize, "row")) {
    rs <- rowSums(mat)
    ok <- is.finite(rs) & rs > 0
    mat[ok, ] <- mat[ok, , drop = FALSE] / rs[ok]
    mat[!ok, ] <- NA_real_
  } else if (identical(normalize, "joint")) {
    total <- sum(mat, na.rm = TRUE)
    mat <- if (is.finite(total) && total > 0) mat / total else mat * NA_real_
  }

  table_out <- expand.grid(from = states, to = states, stringsAsFactors = FALSE)
  table_out$n <- as.numeric(counts[cbind(table_out$from, table_out$to)])
  table_out$prob <- as.numeric(mat[cbind(table_out$from, table_out$to)])
  table_out <- table_out[order(table_out$from, -table_out$n, table_out$to), , drop = FALSE]
  rownames(table_out) <- NULL

  list(
    matrix = mat,
    counts = counts,
    table = table_out,
    states = states,
    normalize = normalize,
    smoothing = smoothing,
    drop_self = drop_self
  )
}

stpd_state_transition_table <- function(dat = NULL,
                                        labels = NULL,
                                        train = "",
                                        label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                        min_isi_sec = 0.001,
                                        auto_others = FALSE,
                                        drop_unlabeled = FALSE,
                                        states = NULL,
                                        smoothing = 0,
                                        normalize = c("row", "joint", "none")) {
  seq <- stpd_state_sequence(
    dat = dat,
    labels = labels,
    train = train,
    label_source = label_source,
    min_isi_sec = min_isi_sec,
    auto_others = auto_others,
    drop_unlabeled = drop_unlabeled
  )
  tm <- stpd_state_transition_matrix(seq, states = states, smoothing = smoothing, normalize = normalize)
  out <- tm$table
  out$train <- as.character(train %||% "")
  out <- out[, c("train", "from", "to", "n", "prob"), drop = FALSE]
  out
}

stpd_state_dwell_times <- function(x,
                                   min_duration_isi = 1L,
                                   drop_unlabeled = FALSE) {
  seq <- if (is.data.frame(x)) x else stpd_state_sequence(labels = x)
  if (is.null(seq) || nrow(seq) == 0) return(data.frame())
  if (isTRUE(drop_unlabeled)) {
    seq <- seq[!(seq$label %in% c("unlabeled", "others", "artifact", "")), , drop = FALSE]
  }
  if (nrow(seq) == 0) return(data.frame())
  lab <- as.character(seq$label)
  r <- rle(lab)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  min_duration_isi <- max(1L, safe_int(min_duration_isi, 1L))
  rows <- vector("list", length(r$lengths))
  for (ii in seq_along(r$lengths)) {
    idx <- starts[ii]:ends[ii]
    dur <- suppressWarnings(as.numeric(seq$duration_isi_sec[idx]))
    rows[[ii]] <- data.frame(
      train = as.character(seq$train[idx[1]] %||% ""),
      label = r$values[ii],
      segment_id = ii,
      start_position = starts[ii],
      end_position = ends[ii],
      start_row = seq$row_number[idx[1]],
      end_row = seq$row_number[idx[length(idx)]],
      n_isi = r$lengths[ii],
      duration_sec = if (any(is.finite(dur))) sum(dur, na.rm = TRUE) else NA_real_,
      start_time_mid_sec = seq$time_mid_sec[idx[1]] %||% NA_real_,
      end_time_mid_sec = seq$time_mid_sec[idx[length(idx)]] %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out <- out[out$n_isi >= min_duration_isi, , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_transition_entropy <- function(x,
                                    base = 2,
                                    smoothing = 0,
                                    drop_self = FALSE) {
  tm <- if (is.list(x) && !is.null(x$counts)) x else {
    stpd_state_transition_matrix(x, smoothing = smoothing, normalize = "none", drop_self = drop_self)
  }
  counts <- as.matrix(tm$counts)
  states <- rownames(counts)
  drop_self_effective <- isTRUE(tm$drop_self %||% drop_self)
  if (isTRUE(drop_self_effective) && nrow(counts) == ncol(counts)) {
    diag(counts) <- 0
  }
  rs <- rowSums(counts)
  total <- sum(rs)
  probs <- counts
  ok <- rs > 0
  probs[ok, ] <- probs[ok, , drop = FALSE] / rs[ok]
  probs[!ok, ] <- NA_real_
  logfun <- function(z) log(z) / log(base)
  by_state <- rep(NA_real_, nrow(probs))
  for (i in seq_len(nrow(probs))) {
    p <- probs[i, ]
    p <- p[is.finite(p) & p > 0]
    if (length(p) > 0) by_state[i] <- -sum(p * logfun(p))
  }
  weights <- if (is.finite(total) && total > 0) rs / total else rep(NA_real_, length(rs))
  entropy_rate <- sum(weights * by_state, na.rm = TRUE)
  max_support <- if (isTRUE(drop_self_effective)) max(0L, length(states) - 1L) else length(states)
  max_entropy <- if (max_support > 1) logfun(max_support) else 0
  data.frame(
    state = c(states, "weighted_rate"),
    transition_n = c(as.numeric(rs), as.numeric(total)),
    entropy = c(by_state, entropy_rate),
    normalized_entropy = c(
      if (max_entropy > 0) by_state / max_entropy else rep(NA_real_, length(by_state)),
      if (max_entropy > 0) entropy_rate / max_entropy else NA_real_
    ),
    base = base,
    stringsAsFactors = FALSE
  )
}

stpd_motif_frequency <- function(x,
                                 motif_length = 3L,
                                 collapse_repeats = FALSE,
                                 sep = " -> ") {
  labels <- if (is.data.frame(x)) as.character(x$label) else as.character(x)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (isTRUE(collapse_repeats) && length(labels) > 1) {
    labels <- labels[c(TRUE, tail(labels, -1) != head(labels, -1))]
  }
  motif_length <- max(2L, safe_int(motif_length, 3L))
  if (length(labels) < motif_length) {
    return(data.frame(motif = character(), n = integer(), rate = numeric(),
                      motif_length = integer(), stringsAsFactors = FALSE))
  }
  motifs <- vapply(seq_len(length(labels) - motif_length + 1L), function(i) {
    paste(labels[i:(i + motif_length - 1L)], collapse = sep)
  }, character(1))
  tab <- sort(table(motifs), decreasing = TRUE)
  data.frame(
    motif = names(tab),
    n = as.integer(tab),
    rate = as.numeric(tab) / length(motifs),
    motif_length = motif_length,
    stringsAsFactors = FALSE
  )
}

stpd_state_sequence_metrics <- function(x, motif_length = 3L) {
  labels <- if (is.data.frame(x)) as.character(x$label) else as.character(x)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) < 2) {
    return(c(
      transition_entropy = NA_real_,
      max_transition_prob = NA_real_,
      top_motif_rate = NA_real_,
      mean_dwell_isi = NA_real_,
      dwell_cv = NA_real_,
      state_n = length(unique(labels))
    ))
  }
  tm <- stpd_state_transition_matrix(labels, smoothing = 0, normalize = "row")
  ent <- stpd_transition_entropy(stpd_state_transition_matrix(labels, normalize = "none"))
  entropy_rate <- ent$entropy[ent$state == "weighted_rate"][1]
  probs <- as.numeric(tm$matrix)
  maxp <- if (any(is.finite(probs))) max(probs, na.rm = TRUE) else NA_real_
  mf <- stpd_motif_frequency(labels, motif_length = motif_length)
  dwell <- stpd_state_dwell_times(labels)
  dwell_n <- suppressWarnings(as.numeric(dwell$n_isi))
  dwell_cv <- if (length(dwell_n) >= 2 && mean(dwell_n) > 0) stats::sd(dwell_n) / mean(dwell_n) else NA_real_
  c(
    transition_entropy = entropy_rate,
    max_transition_prob = maxp,
    top_motif_rate = if (nrow(mf) > 0) mf$rate[1] else NA_real_,
    mean_dwell_isi = if (length(dwell_n) > 0) mean(dwell_n) else NA_real_,
    dwell_cv = dwell_cv,
    state_n = length(unique(labels))
  )
}

stpd_sample_state <- function(prob, states) {
  prob <- suppressWarnings(as.numeric(prob))
  prob[!is.finite(prob) | prob < 0] <- 0
  if (sum(prob) <= 0) prob <- rep(1, length(states))
  sample(states, size = 1L, prob = prob)
}

stpd_make_state_surrogate <- function(x,
                                      method = c("label_permutation", "block_shuffle", "run_shuffle", "markov", "renewal"),
                                      block_length = 10L,
                                      smoothing = 1) {
  method <- match.arg(method)
  labels <- if (is.data.frame(x)) as.character(x$label) else as.character(x)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  n <- length(labels)
  if (n <= 1) return(labels)
  states <- sort(unique(labels))
  block_length <- max(1L, safe_int(block_length, 10L))

  if (identical(method, "label_permutation")) {
    return(sample(labels, length(labels), replace = FALSE))
  }
  if (identical(method, "block_shuffle")) {
    block_id <- ceiling(seq_along(labels) / block_length)
    blocks <- split(labels, block_id)
    return(unlist(blocks[sample(seq_along(blocks))], use.names = FALSE)[seq_len(n)])
  }
  if (identical(method, "run_shuffle")) {
    rr <- rle(labels)
    ord <- sample(seq_along(rr$values))
    return(rep(rr$values[ord], rr$lengths[ord])[seq_len(n)])
  }
  if (identical(method, "markov")) {
    tm <- stpd_state_transition_matrix(labels, states = states, smoothing = smoothing, normalize = "row")
    init <- tabulate(match(labels, states), nbins = length(states))
    out <- character(n)
    out[1] <- stpd_sample_state(init, states)
    for (i in 2:n) {
      out[i] <- stpd_sample_state(tm$matrix[out[i - 1L], ], states)
    }
    return(out)
  }

  rr <- rle(labels)
  state_prob <- tabulate(match(rr$values, states), nbins = length(states))
  run_lengths <- split(rr$lengths, rr$values)
  out <- character(0)
  last <- ""
  while (length(out) < n) {
    p <- state_prob
    if (nzchar(last)) p[states == last] <- 0
    state <- stpd_sample_state(p, states)
    lens <- run_lengths[[state]] %||% rr$lengths
    len <- sample(lens, size = 1L)
    out <- c(out, rep(state, len))
    last <- state
  }
  out[seq_len(n)]
}

stpd_state_surrogate_controls <- function(x,
                                          n_surrogates = 199L,
                                          methods = c("label_permutation", "block_shuffle", "run_shuffle", "markov", "renewal"),
                                          block_length = 10L,
                                          motif_length = 3L,
                                          seed = NULL) {
  labels <- if (is.data.frame(x)) as.character(x$label) else as.character(x)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  n_surrogates <- max(1L, safe_int(n_surrogates, 199L))
  observed <- stpd_state_sequence_metrics(labels, motif_length = motif_length)
  if (!is.null(seed)) set.seed(safe_int(seed, 1L))
  rows <- list()
  for (method in methods) {
    method <- match.arg(method, c("label_permutation", "block_shuffle", "run_shuffle", "markov", "renewal"))
    for (bb in seq_len(n_surrogates)) {
      s <- stpd_make_state_surrogate(labels, method = method, block_length = block_length)
      m <- stpd_state_sequence_metrics(s, motif_length = motif_length)
      rows[[length(rows) + 1L]] <- data.frame(
        method = method,
        surrogate_id = bb,
        metric = names(m),
        value = as.numeric(m),
        stringsAsFactors = FALSE
      )
    }
  }
  surrogate_metrics <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  summary_rows <- list()
  for (method in unique(surrogate_metrics$method)) {
    md <- surrogate_metrics[surrogate_metrics$method == method, , drop = FALSE]
    for (metric in names(observed)) {
      vals <- md$value[md$metric == metric]
      vals <- vals[is.finite(vals)]
      obs <- observed[[metric]]
      if (!is.finite(obs) || length(vals) == 0) {
        p_high <- p_low <- p_two <- NA_real_
      } else {
        p_high <- (1 + sum(vals >= obs)) / (length(vals) + 1)
        p_low <- (1 + sum(vals <= obs)) / (length(vals) + 1)
        p_two <- min(1, 2 * min(p_high, p_low))
      }
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        method = method,
        metric = metric,
        observed = obs,
        surrogate_mean = if (length(vals) > 0) mean(vals) else NA_real_,
        surrogate_sd = if (length(vals) >= 2) stats::sd(vals) else NA_real_,
        p_high = p_high,
        p_low = p_low,
        p_two_sided = p_two,
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    observed = data.frame(metric = names(observed), value = as.numeric(observed), stringsAsFactors = FALSE),
    surrogate_metrics = surrogate_metrics,
    summary = if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
  )
}

stpd_prepare_state_feature_matrix <- function(features,
                                              feature_cols = NULL,
                                              scaling = c("robust", "zscore"),
                                              max_points = 900L) {
  scaling <- match.arg(scaling)
  if (is.null(features) || nrow(features) < 3) {
    stop("Need at least three ISI feature rows.", call. = FALSE)
  }
  max_points <- max(10L, safe_int(max_points, 900L))
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
    stop("Need at least two varying numeric ISI features.", call. = FALSE)
  }
  X <- stpd_scale_matrix_for_pca(features0[, feature_cols, drop = FALSE], scaling = scaling)
  list(X = X, features = features0, feature_cols = feature_cols,
       sample_index = sample_index, sampled = sampled, scaling = scaling)
}

stpd_diffusion_probability <- function(X,
                                       kernel_scale = c("local", "median"),
                                       n_neighbors = 15L,
                                       alpha = 0.5) {
  kernel_scale <- match.arg(kernel_scale)
  X <- as.matrix(X)
  n <- nrow(X)
  d2 <- as.matrix(stats::dist(X))^2
  if (identical(kernel_scale, "local")) {
    k <- max(2L, min(n - 1L, safe_int(n_neighbors, 15L)))
    kth <- apply(d2 + diag(Inf, n), 1, function(z) sort(z, partial = k)[k])
    epsilon <- stats::median(kth[is.finite(kth) & kth > 0], na.rm = TRUE)
  } else {
    vals <- d2[upper.tri(d2)]
    epsilon <- stats::median(vals[is.finite(vals) & vals > 0], na.rm = TRUE)
  }
  if (!is.finite(epsilon) || epsilon <= 0) epsilon <- 1
  K <- exp(-d2 / epsilon)
  q <- rowSums(K)
  q[!is.finite(q) | q <= 0] <- 1
  alpha <- min(max(suppressWarnings(as.numeric(alpha %||% 0.5)), 0), 1)
  K <- K / outer(q^alpha, q^alpha)
  d <- rowSums(K)
  d[!is.finite(d) | d <= 0] <- 1
  P <- K / d
  S <- K / sqrt(outer(d, d))
  list(P = P, S = S, epsilon = epsilon, density = d)
}

stpd_run_isi_state_diffusion_map <- function(features,
                                             ndim = 3L,
                                             diffusion_time = 1,
                                             kernel_scale = c("local", "median"),
                                             n_neighbors = 15L,
                                             alpha = 0.5,
                                             max_points = 900L,
                                             feature_cols = NULL,
                                             scaling = c("robust", "zscore")) {
  prep <- stpd_prepare_state_feature_matrix(
    features,
    feature_cols = feature_cols,
    scaling = scaling,
    max_points = max_points
  )
  kernel_scale <- match.arg(kernel_scale)
  prob <- stpd_diffusion_probability(prep$X, kernel_scale = kernel_scale,
                                     n_neighbors = n_neighbors, alpha = alpha)
  eig <- eigen(prob$S, symmetric = TRUE)
  vals <- Re(eig$values)
  vecs <- Re(eig$vectors)
  ord <- order(vals, decreasing = TRUE)
  vals <- vals[ord]
  vecs <- vecs[, ord, drop = FALSE]
  d <- prob$density
  psi <- vecs / sqrt(d)
  ndim <- max(2L, min(3L, safe_int(ndim, 3L), nrow(prep$X) - 1L, ncol(psi) - 1L))
  use <- seq.int(2L, ndim + 1L)
  tpow <- suppressWarnings(as.numeric(diffusion_time %||% 1))
  if (!is.finite(tpow) || tpow < 0) tpow <- 1
  coords <- sweep(psi[, use, drop = FALSE], 2, vals[use]^tpow, "*")
  coords <- as.data.frame(coords, stringsAsFactors = FALSE)
  names(coords) <- paste0("Diffusion", seq_len(ncol(coords)))
  for (nm in paste0("Diffusion", 1:3)) if (!(nm %in% names(coords))) coords[[nm]] <- NA_real_
  scores <- cbind(prep$features, coords[, paste0("Diffusion", 1:3), drop = FALSE])
  diagnostics <- data.frame(
    n_input = nrow(features),
    n_embedded = nrow(scores),
    sampled = prep$sampled,
    ndim = ndim,
    diffusion_time = tpow,
    kernel_scale = kernel_scale,
    epsilon = prob$epsilon,
    alpha = alpha,
    feature_n = length(prep$feature_cols),
    stringsAsFactors = FALSE
  )
  list(scores = scores, diagnostics = diagnostics, eigenvalues = vals,
       feature_cols = prep$feature_cols, sample_index = prep$sample_index,
       scaling = prep$scaling)
}

stpd_run_isi_state_phate <- function(features,
                                     ndim = 3L,
                                     diffusion_time = 5L,
                                     kernel_scale = c("local", "median"),
                                     n_neighbors = 15L,
                                     alpha = 0.5,
                                     max_points = 700L,
                                     feature_cols = NULL,
                                     scaling = c("robust", "zscore"),
                                     use_phateR = TRUE) {
  prep <- stpd_prepare_state_feature_matrix(
    features,
    feature_cols = feature_cols,
    scaling = scaling,
    max_points = max_points
  )
  ndim <- max(2L, min(3L, safe_int(ndim, 3L), nrow(prep$X) - 1L))
  ph <- NULL
  phate_error <- NULL
  if (isTRUE(use_phateR) && requireNamespace("phateR", quietly = TRUE)) {
    ph <- tryCatch(
      {
        utils::capture.output(
          utils::capture.output(
            ph_fit <- phateR::phate(prep$X, ndim = ndim, knn = n_neighbors, t = diffusion_time),
            type = "message"
          ),
          type = "output"
        )
        ph_fit
      },
      error = function(e) {
        phate_error <<- conditionMessage(e)
        NULL
      }
    )
  }
  if (!is.null(ph) && !is.null(ph$embedding)) {
    emb <- as.data.frame(ph$embedding, stringsAsFactors = FALSE)
    names(emb) <- paste0("PHATE", seq_len(ncol(emb)))
    method <- "phateR"
    note <- ""
  } else {
    kernel_scale <- match.arg(kernel_scale)
    prob <- stpd_diffusion_probability(prep$X, kernel_scale = kernel_scale,
                                       n_neighbors = n_neighbors, alpha = alpha)
    tsteps <- max(1L, safe_int(diffusion_time, 5L))
    Pt <- prob$P
    if (tsteps > 1L) {
      for (ii in seq_len(tsteps - 1L)) Pt <- Pt %*% prob$P
    }
    potential <- -log(pmax(Pt, .Machine$double.eps))
    pot_dist <- stats::dist(potential)
    mds <- stats::cmdscale(pot_dist, k = ndim, eig = TRUE, add = TRUE)
    emb <- as.data.frame(mds$points, stringsAsFactors = FALSE)
    names(emb) <- paste0("PHATE", seq_len(ncol(emb)))
    method <- "diffusion_potential_mds"
    note <- "PHATE-like fallback: diffusion potential plus metric MDS; install phateR for canonical PHATE."
    if (!is.null(phate_error)) {
      note <- paste(note, "phateR was unavailable at runtime:", phate_error)
    }
  }
  for (nm in paste0("PHATE", 1:3)) if (!(nm %in% names(emb))) emb[[nm]] <- NA_real_
  scores <- cbind(prep$features, emb[, paste0("PHATE", 1:3), drop = FALSE])
  diagnostics <- data.frame(
    n_input = nrow(features),
    n_embedded = nrow(scores),
    sampled = prep$sampled,
    ndim = ndim,
    diffusion_time = max(1L, safe_int(diffusion_time, 5L)),
    method = method,
    note = note,
    feature_n = length(prep$feature_cols),
    stringsAsFactors = FALSE
  )
  list(scores = scores, diagnostics = diagnostics, feature_cols = prep$feature_cols,
       sample_index = prep$sample_index, scaling = prep$scaling)
}

stpd_run_isi_state_isomap_sweep <- function(features,
                                            neighbor_grid = c(5L, 8L, 10L, 15L, 20L, 30L),
                                            ndim = 3L,
                                            max_points = 900L,
                                            feature_cols = NULL,
                                            scaling = c("robust", "zscore")) {
  scaling <- match.arg(scaling)
  embeddings <- list()
  diag_rows <- list()
  for (kk in unique(as.integer(neighbor_grid))) {
    res <- tryCatch(
      stpd_run_isi_state_isomap(
        features,
        n_neighbors = kk,
        ndim = ndim,
        max_points = max_points,
        feature_cols = feature_cols,
        scaling = scaling,
        component = "largest"
      ),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      diag_rows[[length(diag_rows) + 1L]] <- data.frame(
        n_neighbors_requested = kk,
        ok = FALSE,
        message = conditionMessage(res),
        stringsAsFactors = FALSE
      )
    } else {
      embeddings[[paste0("k_", kk)]] <- res
      d <- res$diagnostics
      d$n_neighbors_requested <- kk
      d$ok <- TRUE
      d$message <- ""
      diag_rows[[length(diag_rows) + 1L]] <- d
    }
  }
  list(
    embeddings = embeddings,
    diagnostics = if (length(diag_rows) > 0) dplyr::bind_rows(diag_rows) else data.frame()
  )
}

stpd_recurrence_line_lengths <- function(x, min_length = 2L, direction = c("diagonal", "vertical")) {
  direction <- match.arg(direction)
  x <- as.matrix(x)
  n <- nrow(x)
  min_length <- max(1L, safe_int(min_length, 2L))
  lens <- integer(0)
  add_runs <- function(v) {
    rr <- rle(as.logical(v))
    rr$lengths[rr$values & rr$lengths >= min_length]
  }
  if (identical(direction, "vertical")) {
    for (j in seq_len(ncol(x))) lens <- c(lens, add_runs(x[, j]))
  } else {
    offsets <- seq.int(-(n - 1L), n - 1L)
    offsets <- offsets[offsets != 0L]
    for (off in offsets) {
      if (off >= 0L) {
        idx <- cbind(seq_len(n - off), seq.int(1L + off, n))
      } else {
        idx <- cbind(seq.int(1L - off, n), seq_len(n + off))
      }
      lens <- c(lens, add_runs(x[idx]))
    }
  }
  as.integer(lens)
}

stpd_rqa_metrics <- function(recurrence_matrix,
                             min_diagonal = 2L,
                             min_vertical = 2L,
                             base = 2) {
  R <- as.matrix(recurrence_matrix)
  if (nrow(R) != ncol(R)) stop("Recurrence matrix must be square.", call. = FALSE)
  diag(R) <- FALSE
  rec_points <- sum(R, na.rm = TRUE)
  total_points <- length(R) - nrow(R)
  diag_lens <- stpd_recurrence_line_lengths(R, min_length = min_diagonal, direction = "diagonal")
  vert_lens <- stpd_recurrence_line_lengths(R, min_length = min_vertical, direction = "vertical")
  det_points <- sum(diag_lens)
  lam_points <- sum(vert_lens)
  ent <- NA_real_
  if (length(diag_lens) > 0) {
    p <- as.numeric(table(diag_lens))
    p <- p / sum(p)
    ent <- -sum(p * (log(p) / log(base)))
  }
  data.frame(
    recurrence_rate = if (total_points > 0) rec_points / total_points else NA_real_,
    determinism = if (rec_points > 0) det_points / rec_points else NA_real_,
    laminarity = if (rec_points > 0) lam_points / rec_points else NA_real_,
    trapping_time = if (length(vert_lens) > 0) mean(vert_lens) else NA_real_,
    longest_diagonal = if (length(diag_lens) > 0) max(diag_lens) else 0L,
    diagonal_entropy = ent,
    recurrent_points = rec_points,
    stringsAsFactors = FALSE
  )
}

stpd_make_recurrence_plot <- function(features,
                                      radius = NULL,
                                      recurrence_rate = 0.05,
                                      max_points = 500L,
                                      feature_cols = NULL,
                                      scaling = c("robust", "zscore")) {
  prep <- stpd_prepare_state_feature_matrix(
    features,
    feature_cols = feature_cols,
    scaling = scaling,
    max_points = max_points
  )
  d <- as.matrix(stats::dist(prep$X))
  vals <- d[upper.tri(d)]
  vals <- vals[is.finite(vals) & vals > 0]
  if (is.null(radius) || !is.finite(suppressWarnings(as.numeric(radius)))) {
    rr <- min(max(suppressWarnings(as.numeric(recurrence_rate %||% 0.05)), 0.001), 0.5)
    radius <- as.numeric(stats::quantile(vals, probs = rr, na.rm = TRUE, names = FALSE))
  } else {
    radius <- suppressWarnings(as.numeric(radius))
  }
  if (!is.finite(radius) || radius <= 0) radius <- stats::median(vals, na.rm = TRUE)
  R <- d <= radius
  diag(R) <- FALSE
  diagnostics <- data.frame(
    n_input = nrow(features),
    n_embedded = nrow(prep$features),
    sampled = prep$sampled,
    radius = radius,
    target_recurrence_rate = recurrence_rate,
    feature_n = length(prep$feature_cols),
    stringsAsFactors = FALSE
  )
  list(matrix = R, distance = d, diagnostics = diagnostics,
       metrics = stpd_rqa_metrics(R), sample_index = prep$sample_index,
       feature_cols = prep$feature_cols, scaling = prep$scaling)
}

stpd_run_isi_state_umap <- function(features,
                                    ndim = 2L,
                                    n_neighbors = 15L,
                                    min_dist = 0.1,
                                    max_points = 2000L,
                                    feature_cols = NULL,
                                    scaling = c("robust", "zscore"),
                                    allow_missing = TRUE) {
  if (!requireNamespace("uwot", quietly = TRUE)) {
    if (isTRUE(allow_missing)) {
      return(list(scores = data.frame(), diagnostics = data.frame(
        ok = FALSE, method = "uwot", message = "Package 'uwot' is not installed.",
        stringsAsFactors = FALSE
      )))
    }
    stop("Package 'uwot' is required for UMAP.", call. = FALSE)
  }
  prep <- stpd_prepare_state_feature_matrix(features, feature_cols = feature_cols,
                                            scaling = scaling, max_points = max_points)
  ndim <- max(2L, min(3L, safe_int(ndim, 2L)))
  emb <- uwot::umap(prep$X, n_components = ndim, n_neighbors = n_neighbors, min_dist = min_dist)
  emb <- as.data.frame(emb, stringsAsFactors = FALSE)
  names(emb) <- paste0("UMAP", seq_len(ncol(emb)))
  for (nm in paste0("UMAP", 1:3)) if (!(nm %in% names(emb))) emb[[nm]] <- NA_real_
  list(
    scores = cbind(prep$features, emb[, paste0("UMAP", 1:3), drop = FALSE]),
    diagnostics = data.frame(ok = TRUE, method = "uwot", n_embedded = nrow(emb),
                             feature_n = length(prep$feature_cols), stringsAsFactors = FALSE),
    feature_cols = prep$feature_cols,
    sample_index = prep$sample_index
  )
}

stpd_run_isi_state_tsne <- function(features,
                                    ndim = 2L,
                                    perplexity = 30,
                                    max_points = 2000L,
                                    feature_cols = NULL,
                                    scaling = c("robust", "zscore"),
                                    allow_missing = TRUE) {
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    if (isTRUE(allow_missing)) {
      return(list(scores = data.frame(), diagnostics = data.frame(
        ok = FALSE, method = "Rtsne", message = "Package 'Rtsne' is not installed.",
        stringsAsFactors = FALSE
      )))
    }
    stop("Package 'Rtsne' is required for t-SNE.", call. = FALSE)
  }
  prep <- stpd_prepare_state_feature_matrix(features, feature_cols = feature_cols,
                                            scaling = scaling, max_points = max_points)
  ndim <- max(2L, min(3L, safe_int(ndim, 2L)))
  perplexity <- min(suppressWarnings(as.numeric(perplexity)), max(2, floor((nrow(prep$X) - 1) / 3)))
  emb0 <- Rtsne::Rtsne(prep$X, dims = ndim, perplexity = perplexity,
                       pca = FALSE, check_duplicates = FALSE)
  emb <- as.data.frame(emb0$Y, stringsAsFactors = FALSE)
  names(emb) <- paste0("tSNE", seq_len(ncol(emb)))
  for (nm in paste0("tSNE", 1:3)) if (!(nm %in% names(emb))) emb[[nm]] <- NA_real_
  list(
    scores = cbind(prep$features, emb[, paste0("tSNE", 1:3), drop = FALSE]),
    diagnostics = data.frame(ok = TRUE, method = "Rtsne", n_embedded = nrow(emb),
                             perplexity = perplexity, feature_n = length(prep$feature_cols),
                             stringsAsFactors = FALSE),
    feature_cols = prep$feature_cols,
    sample_index = prep$sample_index
  )
}

stpd_gmm_logsumexp <- function(x) {
  m <- max(x)
  if (!is.finite(m)) return(m)
  m + log(sum(exp(x - m)))
}

stpd_gmm_fit_diag <- function(X, k, max_iter = 100L, tol = 1e-6) {
  X <- as.matrix(X)
  n <- nrow(X)
  d <- ncol(X)
  k <- max(1L, min(safe_int(k, 2L), n))
  km <- tryCatch(stats::kmeans(X, centers = k, iter.max = 25), error = function(e) NULL)
  if (!is.null(km)) {
    z <- km$cluster
    means <- km$centers
  } else {
    z <- rep(seq_len(k), length.out = n)
    means <- X[round(seq(1, n, length.out = k)), , drop = FALSE]
  }
  vars <- matrix(1, nrow = k, ncol = d)
  weights <- tabulate(z, nbins = k) / n
  for (j in seq_len(k)) {
    idx <- z == j
    if (sum(idx) >= 2) vars[j, ] <- apply(X[idx, , drop = FALSE], 2, stats::var)
  }
  floor_var <- pmax(apply(X, 2, stats::var, na.rm = TRUE) * 1e-4, 1e-6)
  vars <- sweep(vars, 2, floor_var, pmax)
  ll_old <- -Inf
  resp <- matrix(1 / k, nrow = n, ncol = k)
  max_iter <- max(5L, safe_int(max_iter, 100L))
  for (iter in seq_len(max_iter)) {
    logp <- matrix(NA_real_, nrow = n, ncol = k)
    for (j in seq_len(k)) {
      v <- pmax(vars[j, ], floor_var)
      xc <- sweep(X, 2, means[j, ], "-")
      logp[, j] <- log(max(weights[j], .Machine$double.eps)) -
        0.5 * (d * log(2 * pi) + sum(log(v)) + rowSums(sweep(xc^2, 2, v, "/")))
    }
    lse <- apply(logp, 1, stpd_gmm_logsumexp)
    ll <- sum(lse)
    resp <- exp(logp - lse)
    nk <- colSums(resp)
    weights <- nk / n
    for (j in seq_len(k)) {
      if (nk[j] <= .Machine$double.eps) next
      means[j, ] <- colSums(resp[, j] * X) / nk[j]
      xc <- sweep(X, 2, means[j, ], "-")
      vars[j, ] <- colSums(resp[, j] * xc^2) / nk[j]
    }
    vars <- sweep(vars, 2, floor_var, pmax)
    if (is.finite(ll_old) && abs(ll - ll_old) < tol * (abs(ll_old) + 1)) break
    ll_old <- ll
  }
  param_n <- (k - 1L) + k * d + k * d
  list(k = k, weights = weights, means = means, vars = vars, posterior = resp,
       logLik = ll_old, BIC = -2 * ll_old + param_n * log(n), iter = iter)
}

stpd_candidate_states_gmm <- function(features,
                                      n_states = 2:5,
                                      feature_cols = NULL,
                                      scaling = c("robust", "zscore"),
                                      seed = NULL,
                                      max_iter = 100L) {
  scaling <- match.arg(scaling)
  if (!is.null(seed)) set.seed(safe_int(seed, 1L))
  preferred <- c("log_isi_feature", "local_cv2", "local_lv", "delta_logisi",
                 "next_delta_logisi", "prepost_ratio", "local_rate_hz")
  feature_cols <- feature_cols %||% intersect(preferred, names(features))
  prep <- stpd_prepare_state_feature_matrix(features, feature_cols = feature_cols,
                                            scaling = scaling, max_points = nrow(features))
  grid <- sort(unique(as.integer(n_states)))
  grid <- grid[is.finite(grid) & grid >= 1L & grid <= nrow(prep$X)]
  if (length(grid) == 0) grid <- min(3L, nrow(prep$X))
  fits <- lapply(grid, function(k) stpd_gmm_fit_diag(prep$X, k = k, max_iter = max_iter))
  bic <- vapply(fits, function(z) z$BIC, numeric(1))
  best <- fits[[which.min(bic)]]
  z <- max.col(best$posterior, ties.method = "first")
  state_names <- paste0("gmm_state_", seq_len(best$k))
  gmm_state <- state_names[z]
  logisi <- suppressWarnings(as.numeric(prep$features$log_isi_feature %||% prep$features$log_isi))
  cv2 <- suppressWarnings(as.numeric(prep$features$local_cv2 %||% NA_real_))
  state_stats <- data.frame(
    gmm_state = state_names,
    median_logisi = NA_real_,
    median_cv2 = NA_real_,
    n = integer(best$k),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(best$k)) {
    idx <- z == j
    state_stats$n[j] <- sum(idx)
    state_stats$median_logisi[j] <- stats::median(logisi[idx], na.rm = TRUE)
    state_stats$median_cv2[j] <- stats::median(cv2[idx], na.rm = TRUE)
  }
  ord <- order(state_stats$median_logisi, na.last = TRUE)
  role <- rep("candidate_irregular_or_transition", best$k)
  if (best$k >= 1) role[ord[1]] <- "candidate_hf_or_burst"
  if (best$k >= 2) role[ord[length(ord)]] <- "candidate_pause"
  if (best$k >= 3) {
    middle <- ord[2:(length(ord) - 1L)]
    for (j in middle) {
      role[j] <- if (is.finite(state_stats$median_cv2[j]) && state_stats$median_cv2[j] <= 0.5) {
        "candidate_tonic_or_hf_tonic"
      } else {
        "candidate_irregular_or_transition"
      }
    }
  }
  state_stats$candidate_state <- role
  out <- prep$features
  out$gmm_state <- gmm_state
  out$candidate_state <- role[z]
  out$gmm_confidence <- apply(best$posterior, 1, max)
  post <- as.data.frame(best$posterior, stringsAsFactors = FALSE)
  names(post) <- paste0("posterior_", state_names)
  out <- cbind(out, post)
  diagnostics <- data.frame(
    k = grid,
    BIC = bic,
    selected = grid == best$k,
    logLik = vapply(fits, function(z) z$logLik, numeric(1)),
    stringsAsFactors = FALSE
  )
  list(scores = out, model = best, state_stats = state_stats,
       diagnostics = diagnostics, feature_cols = prep$feature_cols,
       scaling = prep$scaling)
}

stpd_candidate_states_rule_based <- function(features,
                                            short_q = 0.20,
                                            hf_q = 0.35,
                                            pause_q = 0.95,
                                            regular_cv2_max = 0.45,
                                            burst_prepost_ratio_min = 1.5) {
  if (is.null(features) || nrow(features) == 0) return(data.frame())
  logisi <- suppressWarnings(as.numeric(features$log_isi_feature %||% features$log_isi))
  cv2 <- suppressWarnings(as.numeric(features$local_cv2 %||% NA_real_))
  prepost <- suppressWarnings(as.numeric(features$prepost_ratio %||% NA_real_))
  q_short <- safe_q(logisi, short_q)[1]
  q_hf <- safe_q(logisi, hf_q)[1]
  q_pause <- safe_q(logisi, pause_q)[1]
  state <- rep("candidate_tonic", length(logisi))
  state[is.finite(logisi) & is.finite(q_pause) & logisi >= q_pause] <- "candidate_pause"
  burst_like <- is.finite(logisi) & is.finite(q_short) & logisi <= q_short &
    is.finite(prepost) & prepost >= burst_prepost_ratio_min
  state[burst_like] <- "candidate_burst"
  hf_like <- is.finite(logisi) & is.finite(q_hf) & logisi <= q_hf & !burst_like
  state[hf_like & is.finite(cv2) & cv2 <= regular_cv2_max] <- "candidate_hf_tonic"
  state[hf_like & (!is.finite(cv2) | cv2 > regular_cv2_max)] <- "candidate_hf_irregular"
  state[is.finite(cv2) & cv2 > 1.2 & state == "candidate_tonic"] <- "candidate_irregular_or_transition"
  out <- features
  out$candidate_state <- state
  out$rule_confidence <- ifelse(state %in% c("candidate_pause", "candidate_burst"), 0.85, 0.65)
  out
}

stpd_hsmm_fit_priors <- function(labels,
                                 states = NULL,
                                 max_duration = 50L,
                                 transition_smoothing = 1,
                                 duration_smoothing = 1) {
  labels <- as.character(labels)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  states <- states %||% sort(unique(labels))
  states <- as.character(states)
  k <- length(states)
  max_duration <- max(1L, safe_int(max_duration, 50L))
  if (length(labels) == 0 || k == 0) stop("Need labels to fit HSMM priors.", call. = FALSE)
  rr <- rle(labels)
  init <- tabulate(match(rr$values[1], states), nbins = k) + duration_smoothing
  init <- init / sum(init)
  names(init) <- states
  trans <- matrix(transition_smoothing, nrow = k, ncol = k, dimnames = list(from = states, to = states))
  if (length(rr$values) >= 2) {
    for (i in seq_len(length(rr$values) - 1L)) {
      trans[rr$values[i], rr$values[i + 1L]] <- trans[rr$values[i], rr$values[i + 1L]] + 1
    }
  }
  diag(trans) <- transition_smoothing
  trans <- trans / rowSums(trans)
  dur <- matrix(duration_smoothing, nrow = k, ncol = max_duration,
                dimnames = list(state = states, duration = seq_len(max_duration)))
  for (i in seq_along(rr$values)) {
    s <- rr$values[i]
    d <- min(max_duration, rr$lengths[i])
    dur[s, d] <- dur[s, d] + 1
  }
  dur <- dur / rowSums(dur)
  list(states = states, init = init, transition = trans, duration = dur,
       max_duration = max_duration)
}

stpd_hsmm_emissions_from_labels <- function(labels,
                                            states = NULL,
                                            confidence = 0.96) {
  labels <- as.character(labels)
  states <- states %||% sort(unique(labels[!is.na(labels) & nzchar(labels)]))
  states <- as.character(states)
  k <- length(states)
  n <- length(labels)
  confidence <- min(max(suppressWarnings(as.numeric(confidence %||% 0.96)), 0.5), 0.999)
  eps <- (1 - confidence) / max(1L, k - 1L)
  E <- matrix(eps, nrow = n, ncol = k, dimnames = list(NULL, states))
  for (i in seq_len(n)) {
    if (labels[i] %in% states) E[i, labels[i]] <- confidence
  }
  E / rowSums(E)
}

stpd_decode_hsmm <- function(labels = NULL,
                             emissions = NULL,
                             states = NULL,
                             max_duration = 50L,
                             transition_smoothing = 1,
                             duration_smoothing = 1,
                             emission_confidence = 0.96) {
  if (is.null(emissions)) {
    if (is.null(labels)) stop("Need labels or an emission probability matrix.", call. = FALSE)
    states <- states %||% sort(unique(as.character(labels)))
    emissions <- stpd_hsmm_emissions_from_labels(labels, states = states, confidence = emission_confidence)
  } else {
    emissions <- as.matrix(emissions)
    states <- states %||% colnames(emissions) %||% paste0("state_", seq_len(ncol(emissions)))
    colnames(emissions) <- states
  }
  states <- as.character(states)
  n <- nrow(emissions)
  k <- length(states)
  if (n < 1 || k < 1) stop("Empty HSMM emission matrix.", call. = FALSE)
  emissions <- pmax(emissions[, states, drop = FALSE], .Machine$double.eps)
  emissions <- emissions / rowSums(emissions)
  labels_for_priors <- labels %||% states[max.col(emissions, ties.method = "first")]
  priors <- stpd_hsmm_fit_priors(labels_for_priors, states = states,
                                 max_duration = max_duration,
                                 transition_smoothing = transition_smoothing,
                                 duration_smoothing = duration_smoothing)
  if (k == 1L) {
    decoded <- rep(states, n)
    return(list(
      decoded = decoded,
      scores = data.frame(position = seq_len(n), hsmm_state = decoded, stringsAsFactors = FALSE),
      segments = stpd_state_dwell_times(decoded),
      logLik = sum(log(emissions[, 1])),
      priors = priors
    ))
  }
  logE <- log(emissions)
  cumE <- rbind(rep(0, k), apply(logE, 2, cumsum))
  log_init <- log(pmax(priors$init, .Machine$double.eps))
  log_trans <- log(pmax(priors$transition, .Machine$double.eps))
  log_dur <- log(pmax(priors$duration, .Machine$double.eps))
  maxD <- priors$max_duration

  dp <- matrix(-Inf, nrow = n, ncol = k)
  back_dur <- matrix(NA_integer_, nrow = n, ncol = k)
  back_state <- matrix(NA_integer_, nrow = n, ncol = k)
  for (t in seq_len(n)) {
    dmax <- min(maxD, t)
    for (s in seq_len(k)) {
      best_val <- -Inf
      best_d <- NA_integer_
      best_prev <- NA_integer_
      for (d in seq_len(dmax)) {
        start <- t - d + 1L
        seg_emit <- cumE[t + 1L, s] - cumE[start, s]
        if (start == 1L) {
          prev_val <- log_init[s]
          prev_state <- NA_integer_
        } else {
          prev_scores <- dp[start - 1L, ] + log_trans[, s]
          prev_scores[s] <- -Inf
          prev_state <- which.max(prev_scores)
          prev_val <- prev_scores[prev_state]
        }
        val <- prev_val + log_dur[s, d] + seg_emit
        if (is.finite(val) && val > best_val) {
          best_val <- val
          best_d <- d
          best_prev <- prev_state
        }
      }
      dp[t, s] <- best_val
      back_dur[t, s] <- best_d
      back_state[t, s] <- best_prev
    }
  }
  end_state <- which.max(dp[n, ])
  logLik <- dp[n, end_state]
  decoded <- character(n)
  t <- n
  s <- end_state
  while (t >= 1L && is.finite(s) && !is.na(s)) {
    d <- back_dur[t, s]
    if (!is.finite(d) || is.na(d) || d < 1L) d <- 1L
    start <- max(1L, t - d + 1L)
    decoded[start:t] <- states[s]
    ps <- back_state[t, s]
    t <- start - 1L
    s <- ps
  }
  decoded[!nzchar(decoded)] <- states[max.col(emissions, ties.method = "first")][!nzchar(decoded)]
  list(
    decoded = decoded,
    scores = data.frame(position = seq_len(n), hsmm_state = decoded,
                        emission_state = states[max.col(emissions, ties.method = "first")],
                        stringsAsFactors = FALSE),
    segments = stpd_state_dwell_times(decoded),
    logLik = logLik,
    priors = priors,
    dynamic_program = dp
  )
}

stpd_hsmm_sequence_loglik <- function(labels, priors) {
  labels <- as.character(labels)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0) return(NA_real_)
  rr <- rle(labels)
  states <- priors$states
  ll <- log(pmax(priors$init[rr$values[1]], .Machine$double.eps))
  for (i in seq_along(rr$values)) {
    d <- min(priors$max_duration, rr$lengths[i])
    ll <- ll + log(pmax(priors$duration[rr$values[i], d], .Machine$double.eps))
    if (i >= 2L) {
      ll <- ll + log(pmax(priors$transition[rr$values[i - 1L], rr$values[i]], .Machine$double.eps))
    }
  }
  as.numeric(ll)
}

stpd_hsmm_heldout_likelihood <- function(labels,
                                         train_fraction = 0.7,
                                         max_duration = 50L,
                                         transition_smoothing = 1,
                                         duration_smoothing = 1) {
  labels <- as.character(labels)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  n <- length(labels)
  if (n < 6) stop("Need at least six labels for held-out scoring.", call. = FALSE)
  cut <- max(3L, min(n - 3L, floor(n * train_fraction)))
  states <- sort(unique(labels))
  priors <- stpd_hsmm_fit_priors(labels[seq_len(cut)], states = states,
                                 max_duration = max_duration,
                                 transition_smoothing = transition_smoothing,
                                 duration_smoothing = duration_smoothing)
  ll <- stpd_hsmm_sequence_loglik(labels[(cut + 1L):n], priors)
  data.frame(
    n_train = cut,
    n_test = n - cut,
    logLik = ll,
    mean_logLik_per_isi = ll / (n - cut),
    stringsAsFactors = FALSE
  )
}

stpd_label_agreement <- function(predicted,
                                 truth,
                                 labels = NULL) {
  predicted <- as.character(predicted)
  truth <- as.character(truth)
  n <- min(length(predicted), length(truth))
  predicted <- predicted[seq_len(n)]
  truth <- truth[seq_len(n)]
  ok <- !is.na(predicted) & nzchar(predicted) & !is.na(truth) & nzchar(truth)
  predicted <- predicted[ok]
  truth <- truth[ok]
  labels <- labels %||% sort(unique(c(predicted, truth)))
  tab <- table(factor(truth, levels = labels), factor(predicted, levels = labels))
  total <- sum(tab)
  acc <- if (total > 0) sum(diag(tab)) / total else NA_real_
  row_tot <- rowSums(tab)
  col_tot <- colSums(tab)
  pe <- if (total > 0) sum(row_tot * col_tot) / total^2 else NA_real_
  kappa <- if (is.finite(pe) && pe < 1) (acc - pe) / (1 - pe) else NA_real_
  per <- data.frame(
    label = labels,
    support = as.numeric(row_tot),
    precision = ifelse(col_tot > 0, diag(tab) / col_tot, NA_real_),
    recall = ifelse(row_tot > 0, diag(tab) / row_tot, NA_real_),
    stringsAsFactors = FALSE
  )
  per$f1 <- ifelse(is.finite(per$precision + per$recall) & (per$precision + per$recall) > 0,
                   2 * per$precision * per$recall / (per$precision + per$recall), NA_real_)
  list(
    confusion = as.data.frame.matrix(tab),
    per_label = per,
    summary = data.frame(
      n = total,
      accuracy = acc,
      balanced_accuracy = mean(per$recall, na.rm = TRUE),
      kappa = kappa,
      stringsAsFactors = FALSE
    )
  )
}

stpd_state_bootstrap_metrics <- function(x,
                                         n_bootstrap = 200L,
                                         block_length = 10L,
                                         motif_length = 3L,
                                         seed = NULL) {
  labels <- if (is.data.frame(x)) as.character(x$label) else as.character(x)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  n <- length(labels)
  if (n < 2) stop("Need at least two labels for bootstrap metrics.", call. = FALSE)
  n_bootstrap <- max(1L, safe_int(n_bootstrap, 200L))
  block_length <- max(1L, safe_int(block_length, 10L))
  if (!is.null(seed)) set.seed(safe_int(seed, 1L))
  observed <- stpd_state_sequence_metrics(labels, motif_length = motif_length)
  blocks <- split(labels, ceiling(seq_along(labels) / block_length))
  rows <- list()
  for (bb in seq_len(n_bootstrap)) {
    boot <- unlist(blocks[sample(seq_along(blocks), size = length(blocks), replace = TRUE)], use.names = FALSE)
    boot <- boot[seq_len(min(length(boot), n))]
    if (length(boot) < n) boot <- c(boot, sample(labels, n - length(boot), replace = TRUE))
    m <- stpd_state_sequence_metrics(boot, motif_length = motif_length)
    rows[[bb]] <- data.frame(bootstrap_id = bb, metric = names(m), value = as.numeric(m),
                             stringsAsFactors = FALSE)
  }
  boot <- do.call(rbind, rows)
  summary_rows <- lapply(names(observed), function(metric) {
    vals <- boot$value[boot$metric == metric]
    vals <- vals[is.finite(vals)]
    data.frame(
      metric = metric,
      observed = observed[[metric]],
      boot_mean = if (length(vals) > 0) mean(vals) else NA_real_,
      ci_low = if (length(vals) > 0) as.numeric(stats::quantile(vals, 0.025, na.rm = TRUE, names = FALSE)) else NA_real_,
      ci_high = if (length(vals) > 0) as.numeric(stats::quantile(vals, 0.975, na.rm = TRUE, names = FALSE)) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  list(observed = data.frame(metric = names(observed), value = as.numeric(observed), stringsAsFactors = FALSE),
       bootstrap = boot, summary = do.call(rbind, summary_rows))
}

stpd_build_transition_model_data <- function(trains,
                                             metadata = NULL,
                                             selected_trains = NULL,
                                             label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                             min_isi_sec = 0.001,
                                             auto_others = FALSE,
                                             drop_unlabeled = TRUE) {
  label_source <- match.arg(label_source)
  selected_trains <- selected_trains %||% names(trains)
  selected_trains <- intersect(as.character(selected_trains), names(trains))
  rows <- list()
  for (tr in selected_trains) {
    seq <- stpd_state_sequence(trains[[tr]], train = tr, label_source = label_source,
                               min_isi_sec = min_isi_sec, auto_others = auto_others,
                               drop_unlabeled = drop_unlabeled)
    if (nrow(seq) < 2) next
    labs <- seq$label
    rows[[length(rows) + 1L]] <- data.frame(
      train = tr,
      from = head(labs, -1),
      to = tail(labs, -1),
      n = 1,
      stringsAsFactors = FALSE
    )
  }
  out <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  if (nrow(out) == 0) return(out)
  agg <- stats::aggregate(n ~ train + from + to, data = out, FUN = sum)
  if (!is.null(metadata) && nrow(metadata) > 0 && "train" %in% names(metadata)) {
    agg <- merge(agg, metadata, by = "train", all.x = TRUE, sort = FALSE)
  }
  agg
}

stpd_fit_transition_statistical_model <- function(transition_data,
                                                  fixed_effects = c("from", "nucleus"),
                                                  random_effects = c("subject"),
                                                  method = c("auto", "multinom", "one_vs_rest_glm", "mixed_one_vs_rest"),
                                                  target_states = NULL) {
  method <- match.arg(method)
  df <- as.data.frame(transition_data)
  if (is.null(df) || nrow(df) == 0) stop("Need non-empty transition data.", call. = FALSE)
  if (!all(c("from", "to") %in% names(df))) stop("transition_data must contain from and to columns.", call. = FALSE)
  if (!("n" %in% names(df))) df$n <- 1
  df$n <- suppressWarnings(as.numeric(df$n))
  df$n[!is.finite(df$n) | df$n <= 0] <- 1
  fixed_effects <- intersect(fixed_effects, names(df))
  if (length(fixed_effects) == 0) fixed_effects <- "from"
  fixed_rhs <- paste(fixed_effects, collapse = " + ")
  method_used <- method

  if (identical(method, "auto")) {
    method_used <- if (requireNamespace("nnet", quietly = TRUE) && length(unique(df$to)) > 2) {
      "multinom"
    } else {
      "one_vs_rest_glm"
    }
  }
  if (identical(method_used, "multinom")) {
    if (!requireNamespace("nnet", quietly = TRUE)) {
      stop("Package 'nnet' is required for multinomial transition modeling.", call. = FALSE)
    }
    form <- stats::as.formula(paste("to ~", fixed_rhs))
    fit_warnings <- character()
    fit <- withCallingHandlers(
      nnet::multinom(form, data = df, weights = df$n, trace = FALSE),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    return(list(method = "multinom", fit = fit, formula = form, data = df,
                warnings = unique(fit_warnings)))
  }

  target_states <- target_states %||% sort(unique(as.character(df$to)))
  fits <- list()
  fit_warnings <- list()
  for (target in target_states) {
    df$target_hit <- as.integer(as.character(df$to) == target)
    target_warnings <- character()
    if (identical(method_used, "mixed_one_vs_rest")) {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        warning("Package 'lme4' is not installed; falling back to one-vs-rest glm.", call. = FALSE)
        method_used <- "one_vs_rest_glm"
      } else {
        random_effects <- intersect(random_effects, names(df))
        rand <- if (length(random_effects) > 0) {
          paste(paste0("(1|", random_effects, ")"), collapse = " + ")
        } else ""
        rhs <- paste(c(fixed_rhs, rand[nzchar(rand)]), collapse = " + ")
        form <- stats::as.formula(paste("target_hit ~", rhs))
        fits[[target]] <- withCallingHandlers(
          lme4::glmer(form, data = df, weights = df$n, family = stats::binomial()),
          warning = function(w) {
            target_warnings <<- c(target_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        if (length(target_warnings) > 0) fit_warnings[[target]] <- unique(target_warnings)
        next
      }
    }
    form <- stats::as.formula(paste("target_hit ~", fixed_rhs))
    fits[[target]] <- withCallingHandlers(
      stats::glm(form, data = df, weights = df$n, family = stats::binomial()),
      warning = function(w) {
        target_warnings <<- c(target_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (length(target_warnings) > 0) fit_warnings[[target]] <- unique(target_warnings)
  }
  list(method = method_used, fits = fits, targets = target_states, data = df,
       fixed_effects = fixed_effects, random_effects = random_effects,
       warnings = fit_warnings)
}

stpd_analyze_state_dynamics <- function(dat,
                                        train = "",
                                        label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                        min_isi_sec = 0.001,
                                        auto_others = FALSE,
                                        k = 3L,
                                        n_surrogates = 99L,
                                        run_exploration = FALSE,
                                        run_models = FALSE,
                                        seed = NULL) {
  label_source <- match.arg(label_source)
  features <- stpd_make_isi_state_space_features(
    dat,
    train = train,
    label_source = label_source,
    k = k,
    min_isi_sec = min_isi_sec,
    auto_others = auto_others
  )
  seq <- stpd_state_sequence(dat, train = train, label_source = label_source,
                             min_isi_sec = min_isi_sec, auto_others = auto_others)
  labels <- as.character(seq$label)
  core <- list(
    features = features,
    phase_portrait = stpd_make_logisi_phase_portrait(dat, train = train, label_source = label_source,
                                                     min_isi_sec = min_isi_sec, auto_others = auto_others),
    pca = if (nrow(features) >= 3) tryCatch(stpd_run_isi_state_pca(features), error = function(e) e) else NULL,
    sequence = seq,
    transition = stpd_state_transition_matrix(seq, normalize = "row"),
    dwell_times = stpd_state_dwell_times(seq),
    transition_entropy = stpd_transition_entropy(labels),
    motif_frequency = stpd_motif_frequency(labels, motif_length = 3L),
    surrogate_controls = if (length(labels) >= 6 && n_surrogates > 0) {
      stpd_state_surrogate_controls(labels, n_surrogates = n_surrogates, seed = seed)
    } else NULL
  )

  exploration <- NULL
  if (isTRUE(run_exploration) && nrow(features) >= 6) {
    exploration <- list(
      diffusion_map = tryCatch(stpd_run_isi_state_diffusion_map(features), error = function(e) e),
      phate = tryCatch(stpd_run_isi_state_phate(features), error = function(e) e),
      isomap_sweep = tryCatch(stpd_run_isi_state_isomap_sweep(features), error = function(e) e),
      recurrence = tryCatch(stpd_make_recurrence_plot(features), error = function(e) e),
      umap = tryCatch(stpd_run_isi_state_umap(features), error = function(e) e),
      tsne = tryCatch(stpd_run_isi_state_tsne(features), error = function(e) e)
    )
  }

  models <- NULL
  if (isTRUE(run_models) && nrow(features) >= 6) {
    rule <- stpd_candidate_states_rule_based(features)
    gmm <- tryCatch(stpd_candidate_states_gmm(features, seed = seed), error = function(e) e)
    candidate_labels <- if (!inherits(gmm, "error")) gmm$scores$candidate_state else rule$candidate_state
    hsmm <- tryCatch(stpd_decode_hsmm(candidate_labels), error = function(e) e)
    models <- list(
      rule_states = rule,
      gmm_states = gmm,
      hsmm = hsmm,
      hsmm_label_agreement = if (!inherits(hsmm, "error")) stpd_label_agreement(hsmm$decoded, candidate_labels) else NULL,
      bootstrap = tryCatch(stpd_state_bootstrap_metrics(candidate_labels, seed = seed), error = function(e) e),
      heldout = tryCatch(stpd_hsmm_heldout_likelihood(candidate_labels), error = function(e) e)
    )
  }

  list(core = core, exploration = exploration, models = models)
}
