# Multi-train pattern-state trajectory helpers.
#
# This module builds a time-binned state vector from several spike trains.
# It is intentionally separate from the single-train ISI state-space module:
# these trajectories describe a selected train set, not one ISI sequence.

stpd_state_trajectory_pattern_groups <- function() {
  list(
    burst = c("burst", "long_burst", "possible_burst"),
    pause = "pause",
    tonic = c("tonic", "high_frequency_tonic"),
    hf_spiking = "high_frequency_spiking",
    others = "others",
    unlabeled = "unlabeled"
  )
}

stpd_state_trajectory_state_colors <- function(states) {
  states <- as.character(states)
  cols <- c(
    burst = "#c84bd8",
    pause = "#2563eb",
    tonic = "#78d957",
    hf_spiking = "#f21b38",
    others = "#e6d84a",
    unlabeled = "#94a3b8"
  )
  out <- cols[states]
  out[is.na(out)] <- "#64748b"
  out
}

stpd_state_trajectory_state_levels <- function() {
  c("burst", "pause", "tonic", "hf_spiking", "others", "unlabeled")
}

stpd_state_trajectory_axis_choices <- function() {
  c(
    "Burst-family rate (Hz/train)" = "burst_activity",
    "Pause occupancy fraction" = "pause_activity",
    "Tonic-family rate (Hz/train)" = "tonic_activity",
    "HF spiking rate (Hz/train)" = "hf_spiking_activity",
    "HF spiking occupancy fraction" = "hf_spiking_fraction",
    "Overall firing rate (Hz/train)" = "firing_rate_hz",
    "Burst-family occupancy fraction" = "burst_fraction",
    "Tonic-family occupancy fraction" = "tonic_fraction",
    "Others occupancy fraction" = "others_fraction",
    "Unlabeled occupancy fraction" = "unlabeled_fraction"
  )
}

stpd_state_trajectory_embedding_choices <- function() {
  c(
    "Direct pattern axes" = "pattern_axes",
    "PCA: linear orthogonal variance axes" = "pca",
    "Factor analysis: linear Gaussian latent factors" = "fa",
    "Isomap: geodesic manifold embedding" = "isomap",
    "t-SNE: local-neighborhood embedding" = "tsne",
    "UMAP: fuzzy topological embedding" = "umap"
  )
}

stpd_state_trajectory_axis_titles <- function(cols) {
  choices <- stpd_state_trajectory_axis_choices()
  titles <- names(choices)[match(cols, unname(choices))]
  titles[is.na(titles) | !nzchar(titles)] <- cols[is.na(titles) | !nzchar(titles)]
  titles
}

stpd_state_trajectory_per_train_states <- function(features, selected_trains = NULL) {
  if (is.null(features) || !is.data.frame(features) || nrow(features) == 0L) return(data.frame())
  selected_trains <- as.character(selected_trains %||% character(0))
  selected_trains <- selected_trains[nzchar(selected_trains)]
  if (length(selected_trains) == 0L) return(data.frame())
  states <- stpd_state_trajectory_state_levels()
  base_cols <- intersect(c("bin_id", "bin_start_sec", "bin_end_sec", "time_mid_sec", "bin_width_sec"), names(features))
  out <- lapply(selected_trains, function(tr) {
    safe <- stpd_state_trajectory_clean_feature_name(tr)
    frac_cols <- paste0(safe, "__", states, "_fraction")
    rate_cols <- paste0(safe, "__", states, "_rate_hz")
    frac <- matrix(0, nrow = nrow(features), ncol = length(states), dimnames = list(NULL, states))
    rate <- matrix(0, nrow = nrow(features), ncol = length(states), dimnames = list(NULL, states))
    for (ii in seq_along(states)) {
      if (frac_cols[ii] %in% names(features)) frac[, ii] <- suppressWarnings(as.numeric(features[[frac_cols[ii]]]))
      if (rate_cols[ii] %in% names(features)) rate[, ii] <- suppressWarnings(as.numeric(features[[rate_cols[ii]]]))
    }
    frac[!is.finite(frac)] <- 0
    rate[!is.finite(rate)] <- 0
    max_idx <- max.col(frac, ties.method = "first")
    zero_rows <- rowSums(frac, na.rm = TRUE) <= 0
    state <- states[max_idx]
    state[zero_rows] <- "unlabeled"
    idx <- cbind(seq_len(nrow(frac)), match(state, states))
    dat <- features[, base_cols, drop = FALSE]
    dat$train <- tr
    dat$state <- state
    dat$state_fraction <- frac[idx]
    dat$state_rate_hz <- rate[idx]
    for (ii in seq_along(states)) {
      dat[[paste0(states[ii], "_fraction")]] <- frac[, ii]
      dat[[paste0(states[ii], "_rate_hz")]] <- rate[, ii]
    }
    dat
  })
  do.call(rbind, out)
}

stpd_make_state_pair_analysis <- function(res,
                                          train_x = NULL,
                                          train_y = NULL,
                                          trains = NULL,
                                          lag_bins = 0,
                                          states = stpd_state_trajectory_state_levels(),
                                          max_joint_combinations = 5000L) {
  selected <- as.character(res$selected_trains %||% character(0))
  if (length(selected) < 2L) {
    return(list(pair_bins = data.frame(), matrix = data.frame(), transitions = data.frame(),
                states = states, trains = character(0), train_count = 0L))
  }
  if (is.null(trains)) {
    train_x <- as.character(train_x %||% selected[1])[1]
    train_y <- as.character(train_y %||% selected[2])[1]
    if (!(train_x %in% selected)) train_x <- selected[1]
    if (!(train_y %in% selected)) train_y <- selected[selected != train_x][1] %||% selected[1]
    if (identical(train_x, train_y)) {
      alt <- selected[selected != train_x]
      if (length(alt) > 0L) train_y <- alt[1]
    }
    trains <- c(train_x, train_y)
  } else {
    trains <- unique(as.character(trains))
    trains <- trains[nzchar(trains) & trains %in% selected]
    if (length(trains) < 2L) {
      trains <- unique(c(trains, selected))
      trains <- trains[seq_len(min(2L, length(trains)))]
    }
  }
  trains <- unique(trains)
  trains <- trains[trains %in% selected]
  if (length(trains) < 2L) {
    return(list(pair_bins = data.frame(), matrix = data.frame(), transitions = data.frame(),
                states = states, trains = trains, train_count = length(trains)))
  }
  train_x <- trains[1]
  train_y <- trains[2]
  lag_bins <- suppressWarnings(as.integer(round(as.numeric(lag_bins %||% 0)[1])))
  if (!is.finite(lag_bins)) lag_bins <- 0L
  max_joint_combinations <- suppressWarnings(as.integer(max_joint_combinations[1]))
  if (!is.finite(max_joint_combinations) || max_joint_combinations < 1L) max_joint_combinations <- 5000L

  per <- res$per_train_states %||% data.frame()
  if (is.null(per) || nrow(per) == 0L) {
    per <- stpd_state_trajectory_per_train_states(res$features %||% res$bins, selected)
  }
  if (!is.data.frame(per) || nrow(per) == 0L) {
    return(list(pair_bins = data.frame(), matrix = data.frame(), transitions = data.frame(),
                states = states, trains = trains, train_count = length(trains),
                train_x = train_x, train_y = train_y, lag_bins = lag_bins))
  }

  safe <- make.unique(stpd_state_trajectory_clean_feature_name(trains), sep = "_")
  names(safe) <- trains
  state_cols <- stats::setNames(paste0("state__", safe), trains)
  fraction_cols <- stats::setNames(paste0("state_fraction__", safe), trains)
  rate_cols <- stats::setNames(paste0("state_rate_hz__", safe), trains)
  bin_id_cols <- stats::setNames(paste0("bin_id__", safe), trains)
  time_cols <- stats::setNames(paste0("time_mid_sec__", safe), trains)

  parts <- lapply(seq_along(trains), function(ii) {
    tr <- trains[ii]
    dat <- per[as.character(per$train) == tr, , drop = FALSE]
    if (nrow(dat) == 0L) return(data.frame())
    dat$pair_bin_id <- if (ii == 1L) dat$bin_id else dat$bin_id - lag_bins
    keep <- intersect(c("pair_bin_id", "bin_id", "bin_start_sec", "bin_end_sec", "time_mid_sec",
                        "state", "state_fraction", "state_rate_hz"), names(dat))
    dat <- dat[, keep, drop = FALSE]
    suffix <- safe[ii]
    rename <- c(
      bin_id = paste0("bin_id__", suffix),
      bin_start_sec = paste0("bin_start_sec__", suffix),
      bin_end_sec = paste0("bin_end_sec__", suffix),
      time_mid_sec = paste0("time_mid_sec__", suffix),
      state = paste0("state__", suffix),
      state_fraction = paste0("state_fraction__", suffix),
      state_rate_hz = paste0("state_rate_hz__", suffix)
    )
    for (nm in intersect(names(rename), names(dat))) {
      names(dat)[names(dat) == nm] <- rename[nm]
    }
    dat
  })
  if (any(vapply(parts, nrow, integer(1)) == 0L)) {
    return(list(pair_bins = data.frame(), matrix = data.frame(), transitions = data.frame(),
                states = states, trains = trains, train_count = length(trains),
                train_x = train_x, train_y = train_y, lag_bins = lag_bins))
  }
  pairs <- Reduce(function(a, b) merge(a, b, by = "pair_bin_id", all = FALSE), parts)
  if (nrow(pairs) == 0L) {
    return(list(pair_bins = data.frame(), matrix = data.frame(), transitions = data.frame(),
                states = states, trains = trains, train_count = length(trains),
                train_x = train_x, train_y = train_y, lag_bins = lag_bins))
  }
  pairs <- pairs[order(pairs$pair_bin_id), , drop = FALSE]
  ref_safe <- safe[trains[1]]
  pairs$bin_id <- pairs[[paste0("bin_id__", ref_safe)]]
  if (paste0("bin_start_sec__", ref_safe) %in% names(pairs)) pairs$bin_start_sec <- pairs[[paste0("bin_start_sec__", ref_safe)]]
  if (paste0("bin_end_sec__", ref_safe) %in% names(pairs)) pairs$bin_end_sec <- pairs[[paste0("bin_end_sec__", ref_safe)]]
  pairs$time_mid_sec <- pairs[[paste0("time_mid_sec__", ref_safe)]]
  pairs$train_x <- train_x
  pairs$train_y <- train_y
  pairs$lag_bins <- lag_bins
  pairs$train_count <- length(trains)

  x_safe <- safe[train_x]
  y_safe <- safe[train_y]
  pairs$bin_id_y <- pairs[[paste0("bin_id__", y_safe)]]
  pairs$time_mid_sec_y <- pairs[[paste0("time_mid_sec__", y_safe)]]
  pairs$state_x <- pairs[[paste0("state__", x_safe)]]
  pairs$state_y <- pairs[[paste0("state__", y_safe)]]
  pairs$state_x_fraction <- pairs[[paste0("state_fraction__", x_safe)]]
  pairs$state_y_fraction <- pairs[[paste0("state_fraction__", y_safe)]]
  pairs$state_x_rate_hz <- pairs[[paste0("state_rate_hz__", x_safe)]]
  pairs$state_y_rate_hz <- pairs[[paste0("state_rate_hz__", y_safe)]]

  state_frame <- pairs[, unname(state_cols), drop = FALSE]
  state_frame[] <- lapply(state_frame, as.character)
  pair_key <- do.call(paste, c(state_frame, sep = "||"))
  pairs$joint_state_labeled <- vapply(seq_len(nrow(state_frame)), function(ii) {
    paste(paste0(trains, "=", unlist(state_frame[ii, , drop = TRUE])), collapse = " | ")
  }, character(1))
  pairs$joint_state <- if (length(trains) == 2L) {
    paste0(pairs$state_x, " x ", pairs$state_y)
  } else {
    pairs$joint_state_labeled
  }

  total <- nrow(pairs)
  combo_count <- length(states) ^ length(trains)
  complete_grid <- combo_count <= max_joint_combinations
  if (complete_grid) {
    combo_list <- rep(list(states), length(trains))
    combos <- expand.grid(combo_list, stringsAsFactors = FALSE)
    names(combos) <- unname(state_cols)
  } else {
    combos <- unique(state_frame)
  }
  combo_key <- do.call(paste, c(combos, sep = "||"))
  obs_counts <- as.numeric(table(pair_key)[combo_key])
  obs_counts[!is.finite(obs_counts)] <- 0
  marginal_probs <- lapply(unname(state_cols), function(col) {
    tab <- table(factor(pairs[[col]], levels = states))
    as.numeric(tab) / max(1L, total)
  })
  names(marginal_probs) <- unname(state_cols)
  for (col in names(marginal_probs)) names(marginal_probs[[col]]) <- states
  expected_prob <- rep(1, nrow(combos))
  for (col in unname(state_cols)) {
    expected_prob <- expected_prob * marginal_probs[[col]][as.character(combos[[col]])]
  }
  expected_count <- total * expected_prob

  mat <- combos
  mat$joint_state_labeled <- vapply(seq_len(nrow(combos)), function(ii) {
    paste(paste0(trains, "=", unlist(combos[ii, , drop = TRUE])), collapse = " | ")
  }, character(1))
  if (length(trains) == 2L) {
    mat$state_x <- as.character(combos[[unname(state_cols[1])]])
    mat$state_y <- as.character(combos[[unname(state_cols[2])]])
    mat$joint_state <- paste0(mat$state_x, " x ", mat$state_y)
  } else {
    mat$joint_state <- mat$joint_state_labeled
  }
  mat$observed_count <- obs_counts
  mat$expected_count <- expected_count
  mat$observed_prob <- if (total > 0) mat$observed_count / total else NA_real_
  mat$expected_prob <- expected_prob
  eps <- 0.5 / max(1, total)
  mat$observed_expected_ratio <- (mat$observed_prob + eps) / (mat$expected_prob + eps)
  mat$log2_enrichment <- log2(mat$observed_expected_ratio)
  mat$standardized_residual <- (mat$observed_count - mat$expected_count) / sqrt(pmax(mat$expected_count, 1e-9))

  obs <- expected <- NULL
  if (length(trains) == 2L) {
    sx <- factor(pairs$state_x, levels = states)
    sy <- factor(pairs$state_y, levels = states)
    obs <- as.matrix(table(sx, sy))
    total2 <- sum(obs)
    expected <- if (total2 > 0) outer(rowSums(obs), colSums(obs)) / total2 else obs * NA_real_
    fisher <- lapply(seq_len(nrow(mat)), function(ii) {
      a <- mat$observed_count[ii]
      row_total <- sum(obs[mat$state_x[ii], , drop = TRUE])
      col_total <- sum(obs[, mat$state_y[ii], drop = TRUE])
      b <- row_total - a
      c <- col_total - a
      d <- total2 - a - b - c
      if (!is.finite(a + b + c + d) || min(a, b, c, d) < 0 || total2 <= 0) return(c(odds_ratio = NA_real_, p_value = NA_real_))
      ft <- tryCatch(stats::fisher.test(matrix(c(a, b, c, d), nrow = 2)), error = function(e) NULL)
      if (is.null(ft)) c(odds_ratio = NA_real_, p_value = NA_real_) else c(odds_ratio = unname(ft$estimate), p_value = ft$p.value)
    })
    fisher <- do.call(rbind, fisher)
    mat$odds_ratio <- fisher[, "odds_ratio"]
    mat$p_value <- fisher[, "p_value"]
  } else {
    mat$odds_ratio <- NA_real_
    mat$p_value <- vapply(seq_len(nrow(mat)), function(ii) {
      p0 <- suppressWarnings(as.numeric(mat$expected_prob[ii]))
      if (!is.finite(p0) || p0 < 0 || p0 > 1 || total <= 0) return(NA_real_)
      bt <- tryCatch(stats::binom.test(as.integer(mat$observed_count[ii]), total, p = p0), error = function(e) NULL)
      if (is.null(bt)) NA_real_ else bt$p.value
    }, numeric(1))
  }
  mat$p_fdr <- stats::p.adjust(mat$p_value, method = "BH")
  mat$association <- ifelse(!is.finite(mat$log2_enrichment), "insufficient",
                            ifelse(mat$log2_enrichment >= log2(1.5), "enriched",
                                   ifelse(mat$log2_enrichment <= -log2(1.5), "depleted", "near_expected")))
  mat$complete_joint_grid <- complete_grid
  mat <- mat[order(abs(mat$log2_enrichment), mat$observed_count, decreasing = TRUE), , drop = FALSE]

  transitions <- data.frame()
  if (nrow(pairs) >= 2L) {
    from <- head(pairs$joint_state, -1L)
    to <- tail(pairs$joint_state, -1L)
    transitions <- as.data.frame(table(from = from, to = to), stringsAsFactors = FALSE)
    transitions <- transitions[transitions$Freq > 0, , drop = FALSE]
    names(transitions)[names(transitions) == "Freq"] <- "n"
    denom <- ave(transitions$n, transitions$from, FUN = sum)
    transitions$prob <- transitions$n / pmax(1, denom)
    transitions <- transitions[order(transitions$n, transitions$prob, decreasing = TRUE), , drop = FALSE]
  }

  notes <- character(0)
  if (!complete_grid) {
    notes <- c(notes, paste0("Joint-state space has ", combo_count,
                             " combinations; matrix is restricted to observed combinations."))
  }
  if (length(trains) > 2L) {
    notes <- c(notes, "Multi-train p-values use a binomial test against the product-of-marginals independence expectation.")
  }

  list(
    pair_bins = pairs,
    matrix = mat,
    transitions = transitions,
    observed_matrix = obs,
    expected_matrix = expected,
    states = states,
    trains = trains,
    train_count = length(trains),
    train_state_columns = state_cols,
    train_fraction_columns = fraction_cols,
    train_rate_columns = rate_cols,
    train_bin_id_columns = bin_id_cols,
    train_time_columns = time_cols,
    train_x = train_x,
    train_y = train_y,
    lag_bins = lag_bins,
    bin_sec = res$bin_sec %||% NA_real_,
    complete_joint_grid = complete_grid,
    max_joint_combinations = max_joint_combinations,
    notes = notes
  )
}

stpd_state_pair_heatmap <- function(pair_res,
                                    value = c("log2_enrichment", "observed_count", "observed_prob", "standardized_residual", "observed_expected_ratio")) {
  value <- match.arg(value)
  mat <- pair_res$matrix %||% data.frame()
  states <- pair_res$states %||% stpd_state_trajectory_state_levels()
  if (is.null(mat) || nrow(mat) == 0L || !(value %in% names(mat))) {
    return(layout(plot_ly(), annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                                     text = "No state-pair matrix available.", showarrow = FALSE))))
  }
  train_count <- pair_res$train_count %||% length(pair_res$trains %||% character(0))
  if (!is.finite(train_count) || train_count <= 0L) train_count <- 2L
  if (train_count > 2L || !all(c("state_x", "state_y") %in% names(mat))) {
    if (value %in% c("log2_enrichment", "standardized_residual")) {
      ord <- order(abs(mat[[value]]), mat$observed_count %||% 0, decreasing = TRUE)
      colors <- c("#2563eb", "#f8fafc", "#f21b38")
    } else {
      ord <- order(mat[[value]], mat$observed_count %||% 0, decreasing = TRUE)
      colors <- c("#f8fafc", "#2563eb")
    }
    mat_top <- mat[ord[seq_len(min(40L, length(ord)))], , drop = FALSE]
    label_col <- if ("joint_state_labeled" %in% names(mat_top)) "joint_state_labeled" else "joint_state"
    y <- rev(as.character(mat_top[[label_col]]))
    z <- matrix(rev(suppressWarnings(as.numeric(mat_top[[value]]))), ncol = 1,
                dimnames = list(y, value))
    text <- matrix(rev(paste0(
      as.character(mat_top[[label_col]]),
      "<br>observed: ", mat_top$observed_count,
      "<br>expected: ", signif(mat_top$expected_count, 4),
      "<br>observed/expected: ", signif(mat_top$observed_expected_ratio, 4),
      "<br>log2 enrichment: ", signif(mat_top$log2_enrichment, 4),
      "<br>FDR p: ", signif(mat_top$p_fdr, 4)
    )), ncol = 1, dimnames = list(y, value))
    subtitle <- if (nrow(mat) > nrow(mat_top)) paste0("top ", nrow(mat_top), " of ", nrow(mat), " combinations") else paste0(nrow(mat_top), " combinations")
    return(plot_ly(
      x = value,
      y = y,
      z = z,
      type = "heatmap",
      colors = colors,
      text = text,
      hoverinfo = "text",
      source = "state_pair_heatmap"
    ) %>%
      layout(
        title = list(text = paste0("Joint-state combinations: ", value, " (", subtitle, ")"), x = 0.02, font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 220, r = 20, t = 60, b = 50),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#ffffff"
      ) %>%
      config(displaylogo = FALSE))
  }
  z <- matrix(NA_real_, nrow = length(states), ncol = length(states), dimnames = list(states, states))
  text <- matrix("", nrow = length(states), ncol = length(states), dimnames = list(states, states))
  for (ii in seq_len(nrow(mat))) {
    z[mat$state_x[ii], mat$state_y[ii]] <- mat[[value]][ii]
    text[mat$state_x[ii], mat$state_y[ii]] <- paste0(
      pair_res$train_x, ": ", mat$state_x[ii],
      "<br>", pair_res$train_y, if (pair_res$lag_bins == 0) ": " else paste0(" lag ", pair_res$lag_bins, " bins: "), mat$state_y[ii],
      "<br>observed: ", mat$observed_count[ii],
      "<br>expected: ", signif(mat$expected_count[ii], 4),
      "<br>log2 enrichment: ", signif(mat$log2_enrichment[ii], 4),
      "<br>odds ratio: ", signif(mat$odds_ratio[ii], 4),
      "<br>FDR p: ", signif(mat$p_fdr[ii], 4)
    )
  }
  plot_ly(
    x = states,
    y = states,
    z = z,
    type = "heatmap",
    colors = c("#2563eb", "#f8fafc", "#f21b38"),
    text = text,
    hoverinfo = "text",
    source = "state_pair_heatmap"
  ) %>%
    layout(
      title = list(text = paste0("State-pair matrix: ", value), x = 0.02, font = list(size = 14)),
      xaxis = list(title = pair_res$train_y),
      yaxis = list(title = pair_res$train_x, autorange = "reversed"),
      margin = list(l = 80, r = 20, t = 60, b = 70),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    ) %>%
    config(displaylogo = FALSE)
}

stpd_state_pair_timeline_plot <- function(pair_res) {
  pairs <- pair_res$pair_bins %||% data.frame()
  states <- pair_res$states %||% stpd_state_trajectory_state_levels()
  if (is.null(pairs) || nrow(pairs) == 0L) {
    return(layout(plot_ly(), annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                                     text = "No state-pair timeline available.", showarrow = FALSE))))
  }
  trains <- as.character(pair_res$trains %||% c(pair_res$train_x, pair_res$train_y))
  trains <- trains[nzchar(trains)]
  state_cols <- pair_res$train_state_columns %||% stats::setNames(c("state_x", "state_y"), trains[seq_len(min(2L, length(trains)))])
  time_cols <- pair_res$train_time_columns %||% stats::setNames(c("time_mid_sec", "time_mid_sec_y"), trains[seq_len(min(2L, length(trains)))])
  trains <- intersect(trains, names(state_cols))
  state_cols <- state_cols[trains]
  time_cols <- time_cols[trains]
  state_cols <- state_cols[unname(state_cols) %in% names(pairs)]
  trains <- names(state_cols)
  if (length(trains) == 0L) {
    return(layout(plot_ly(), annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                                     text = "No state-pair timeline available.", showarrow = FALSE))))
  }
  state_code <- stats::setNames(seq_along(states), states)
  z <- do.call(rbind, lapply(unname(state_cols), function(col) {
    unname(state_code[as.character(pairs[[col]])])
  }))
  rownames(z) <- trains
  cols <- stpd_state_trajectory_state_colors(states)
  n <- length(states)
  colorscale <- unlist(lapply(seq_along(states), function(ii) {
    lo <- (ii - 1) / n
    hi <- ii / n
    list(list(lo, unname(cols[ii])), list(hi, unname(cols[ii])))
  }), recursive = FALSE)
  text <- do.call(rbind, lapply(seq_along(trains), function(ii) {
    tr <- trains[ii]
    col <- unname(state_cols[ii])
    tcol <- unname(time_cols[tr])
    if (is.na(tcol) || !nzchar(tcol)) tcol <- "time_mid_sec"
    tt <- if (tcol %in% names(pairs)) pairs[[tcol]] else pairs$time_mid_sec
    paste0(tr, "<br>time: ", signif(tt, 5), " s<br>state: ", pairs[[col]])
  }))
  plot_ly(
    x = pairs$time_mid_sec,
    y = trains,
    z = z,
    type = "heatmap",
    colorscale = colorscale,
    zmin = 1,
    zmax = n,
    showscale = FALSE,
    text = text,
    hoverinfo = "text",
    source = "state_pair_timeline"
  ) %>%
    layout(
      title = list(text = if (length(trains) > 2L) "Joint-state timeline" else "State-pair timeline", x = 0.02, font = list(size = 14)),
      xaxis = list(title = "time (s)"),
      yaxis = list(title = "", autorange = "reversed"),
      margin = list(l = 120, r = 20, t = 55, b = 55),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    ) %>%
    config(displaylogo = FALSE, scrollZoom = TRUE)
}

stpd_state_pair_transition_heatmap <- function(pair_res,
                                               value = c("prob", "n")) {
  value <- match.arg(value)
  trans <- pair_res$transitions %||% data.frame()
  states <- pair_res$states %||% stpd_state_trajectory_state_levels()
  if (is.null(trans) || nrow(trans) == 0L || !(value %in% names(trans))) {
    return(layout(plot_ly(), annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                                     text = "No joint-state transitions available.", showarrow = FALSE))))
  }
  train_count <- pair_res$train_count %||% length(pair_res$trains %||% character(0))
  if (isTRUE(train_count == 2L)) {
    joint_levels <- as.vector(outer(states, states, paste, sep = " x "))
    observed_levels <- intersect(joint_levels, unique(c(as.character(trans$from), as.character(trans$to))))
    if (length(observed_levels) == 0L) observed_levels <- unique(c(as.character(trans$from), as.character(trans$to)))
  } else {
    observed_levels <- unique(c(as.character(trans$from), as.character(trans$to)))
  }
  if (length(observed_levels) > 60L) {
    level_counts <- rowsum(c(trans$n, trans$n), c(as.character(trans$from), as.character(trans$to)), reorder = FALSE)
    level_weight <- sort(as.numeric(level_counts[, 1]), decreasing = TRUE, index.return = TRUE)
    all_levels <- rownames(level_counts)
    observed_levels <- all_levels[level_weight$ix[seq_len(min(60L, length(level_weight$ix)))]]
  }
  z <- matrix(0, nrow = length(observed_levels), ncol = length(observed_levels),
              dimnames = list(observed_levels, observed_levels))
  text <- matrix("", nrow = length(observed_levels), ncol = length(observed_levels),
                 dimnames = list(observed_levels, observed_levels))
  for (ii in seq_len(nrow(trans))) {
    from <- as.character(trans$from[ii])
    to <- as.character(trans$to[ii])
    if (!(from %in% observed_levels) || !(to %in% observed_levels)) next
    z[from, to] <- suppressWarnings(as.numeric(trans[[value]][ii]))
    text[from, to] <- paste0(
      "from: ", from,
      "<br>to: ", to,
      "<br>count: ", trans$n[ii],
      "<br>probability: ", signif(trans$prob[ii], 4)
    )
  }
  plot_ly(
    x = observed_levels,
    y = observed_levels,
    z = z,
    type = "heatmap",
    colors = c("#f8fafc", "#2563eb"),
    text = text,
    hoverinfo = "text",
    source = "state_pair_transition_heatmap"
  ) %>%
    layout(
      title = list(text = paste0("Joint-state transition matrix: ", value), x = 0.02, font = list(size = 14)),
      xaxis = list(title = "next joint state"),
      yaxis = list(title = "current joint state", autorange = "reversed"),
      margin = list(l = 150, r = 20, t = 60, b = 130),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    ) %>%
    config(displaylogo = FALSE, scrollZoom = TRUE)
}

stpd_state_trajectory_clean_feature_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]+", "_", as.character(x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nzchar(x), x, "train")
}

stpd_state_trajectory_gaussian_smooth <- function(x, sigma_bins = 0) {
  x <- suppressWarnings(as.numeric(x))
  sigma_bins <- suppressWarnings(as.numeric(sigma_bins %||% 0))[1]
  if (!is.finite(sigma_bins) || sigma_bins <= 0 || length(x) < 3L) return(x)
  radius <- max(1L, ceiling(3 * sigma_bins))
  out <- x
  for (ii in seq_along(x)) {
    jj <- seq.int(max(1L, ii - radius), min(length(x), ii + radius))
    w <- exp(-0.5 * ((jj - ii) / sigma_bins)^2)
    ok <- is.finite(x[jj])
    if (!any(ok)) {
      out[ii] <- NA_real_
    } else {
      out[ii] <- sum(x[jj][ok] * w[ok]) / sum(w[ok])
    }
  }
  out
}

stpd_state_trajectory_make_bin_table <- function(start_sec, end_sec, bin_sec) {
  start_sec <- suppressWarnings(as.numeric(start_sec %||% 0))[1]
  end_sec <- suppressWarnings(as.numeric(end_sec %||% NA_real_))[1]
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% 0.1))[1]
  if (!is.finite(start_sec)) start_sec <- 0
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.1
  if (!is.finite(end_sec) || end_sec <= start_sec) end_sec <- start_sec + bin_sec
  n_bins <- max(1L, ceiling((end_sec - start_sec) / bin_sec))
  starts <- start_sec + (seq_len(n_bins) - 1L) * bin_sec
  ends <- pmin(starts + bin_sec, end_sec)
  data.frame(
    bin_id = seq_len(n_bins),
    bin_start_sec = starts,
    bin_end_sec = ends,
    time_mid_sec = (starts + ends) / 2,
    bin_width_sec = ends - starts,
    stringsAsFactors = FALSE
  )
}

stpd_state_trajectory_interval_overlap <- function(a0, a1, b0, b1) {
  pmax(0, pmin(a1, b1) - pmax(a0, b0))
}

stpd_state_trajectory_embedding_diag <- function(method, metric, value, component = "", note = "") {
  data.frame(
    method = as.character(method),
    component = as.character(component %||% ""),
    metric = as.character(metric),
    value = as.character(value),
    note = as.character(note %||% ""),
    stringsAsFactors = FALSE
  )
}

stpd_state_trajectory_sample_index <- function(n, max_points = 900L) {
  n <- safe_int(n, 0L)
  if (n <= 0L) return(integer(0))
  max_points <- max(20L, safe_int(max_points, 900L))
  if (n <= max_points) return(seq_len(n))
  sort(unique(round(seq(1, n, length.out = max_points))))
}

stpd_state_trajectory_max_fa_factors <- function(p, max_factors = 3L) {
  p <- safe_int(p, 0L)
  max_factors <- max(1L, safe_int(max_factors, 3L))
  cand <- seq_len(min(max_factors, max(0L, p - 1L)))
  if (length(cand) == 0L) return(0L)
  # For maximum-likelihood factor analysis, df = ((p - m)^2 - p - m) / 2.
  ok <- ((p - cand)^2 - p - cand) >= 0
  if (!any(ok)) return(0L)
  max(cand[ok])
}

stpd_state_trajectory_compute_embeddings <- function(features,
                                                     feature_cols,
                                                     methods = c("pca"),
                                                     n_neighbors = 15L,
                                                     tsne_perplexity = 30,
                                                     umap_min_dist = 0.1,
                                                     seed = 1L,
                                                     max_points = 900L) {
  methods <- unique(as.character(methods %||% "pca"))
  methods <- intersect(methods, c("pca", "fa", "isomap", "tsne", "umap"))
  features <- as.data.frame(features, stringsAsFactors = FALSE)
  feature_cols <- intersect(as.character(feature_cols %||% character(0)), names(features))
  feature_cols <- feature_cols[vapply(features[feature_cols], is.numeric, logical(1))]

  embedding_cols <- c(
    paste0("PC", 1:3),
    paste0("FA", 1:3),
    paste0("Isomap", 1:3),
    paste0("tSNE", 1:3),
    paste0("UMAP", 1:3)
  )
  for (nm in embedding_cols) if (!(nm %in% names(features))) features[[nm]] <- NA_real_

  diagnostics <- data.frame()
  variance <- data.frame()
  loadings <- data.frame()
  fa_loadings <- data.frame()
  pca <- NULL

  if (nrow(features) < 3L || length(feature_cols) < 2L) {
    diagnostics <- rbind(
      diagnostics,
      stpd_state_trajectory_embedding_diag(
        "embedding", "status", "skipped", "",
        "At least three time bins and two varying numeric train-pattern features are required."
      )
    )
    return(list(
      features = features,
      variance = variance,
      loadings = loadings,
      fa_loadings = fa_loadings,
      diagnostics = diagnostics,
      pca = pca
    ))
  }

  X <- stpd_scale_matrix_for_pca(features[, feature_cols, drop = FALSE], scaling = "zscore")
  X[!is.finite(X)] <- 0
  colnames(X) <- feature_cols
  n <- nrow(X)
  p <- ncol(X)
  n_neighbors <- max(2L, min(n - 1L, safe_int(n_neighbors, 15L)))
  max_points <- max(20L, safe_int(max_points, 900L))
  seed <- safe_int(seed, 1L)

  if ("pca" %in% methods) {
    pca <- tryCatch(stats::prcomp(X, center = FALSE, scale. = FALSE), error = function(e) e)
    if (inherits(pca, "error")) {
      diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("PCA", "status", "failed", "", pca$message))
      pca <- NULL
    } else {
      n_pc <- min(3L, ncol(pca$x))
      for (ii in seq_len(n_pc)) features[[paste0("PC", ii)]] <- pca$x[, ii]
      if (n_pc < 3L) for (ii in seq.int(n_pc + 1L, 3L)) features[[paste0("PC", ii)]] <- 0
      sdev2 <- pca$sdev^2
      total <- sum(sdev2)
      variance <- data.frame(
        PC = paste0("PC", seq_along(sdev2)),
        variance = if (is.finite(total) && total > 0) sdev2 / total else NA_real_,
        cumulative = if (is.finite(total) && total > 0) cumsum(sdev2 / total) else NA_real_,
        stringsAsFactors = FALSE
      )
      n_loading <- min(3L, ncol(pca$rotation))
      loadings <- as.data.frame(pca$rotation[, seq_len(n_loading), drop = FALSE], stringsAsFactors = FALSE)
      for (pc in paste0("PC", seq_len(3L))) if (!(pc %in% names(loadings))) loadings[[pc]] <- NA_real_
      loadings$feature <- rownames(loadings)
      rownames(loadings) <- NULL
      loadings <- loadings[, c("feature", "PC1", "PC2", "PC3"), drop = FALSE]
      for (ii in seq_len(min(3L, nrow(variance)))) {
        diagnostics <- rbind(
          diagnostics,
          stpd_state_trajectory_embedding_diag("PCA", "variance_explained", signif(variance$variance[ii], 6), variance$PC[ii],
                                               "SVD of the centered/scaled feature matrix; components are orthogonal.")
        )
      }
    }
  }

  if ("fa" %in% methods) {
    X_fa <- X
    fa_feature_cols <- feature_cols
    qr_fa <- qr(X_fa)
    if (qr_fa$rank < ncol(X_fa)) {
      keep_cols <- sort(qr_fa$pivot[seq_len(qr_fa$rank)])
      X_fa <- X_fa[, keep_cols, drop = FALSE]
      fa_feature_cols <- colnames(X_fa)
      diagnostics <- rbind(
        diagnostics,
        stpd_state_trajectory_embedding_diag(
          "FA", "rank_filter_dropped_features", p - ncol(X_fa), "",
          "Linearly dependent train-pattern features were removed before ML factor analysis."
        )
      )
    }
    n_fac <- stpd_state_trajectory_max_fa_factors(ncol(X_fa), max_factors = 3L)
    if (n_fac < 1L || nrow(X_fa) < (n_fac + 3L)) {
      diagnostics <- rbind(
        diagnostics,
        stpd_state_trajectory_embedding_diag(
          "FA", "status", "skipped", "",
          "Not enough variables or time bins for maximum-likelihood factor analysis with non-negative degrees of freedom."
        )
      )
    } else {
      fa <- NULL
      fa_error <- NULL
      used_n_fac <- NA_integer_
      for (nf in rev(seq_len(n_fac))) {
        fit <- tryCatch(
          stats::factanal(X_fa, factors = nf, scores = "regression", rotation = "none", control = list(nstart = 20L)),
          error = function(e) e
        )
        if (!inherits(fit, "error")) {
          fa <- fit
          used_n_fac <- nf
          break
        }
        fa_error <- fit$message
      }
      if (is.null(fa)) {
        diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("FA", "status", "failed", "", fa_error %||% "Unable to fit factor-analysis model."))
      } else {
        scores <- as.data.frame(fa$scores, stringsAsFactors = FALSE)
        names(scores) <- paste0("FA", seq_len(ncol(scores)))
        for (nm in names(scores)) features[[nm]] <- scores[[nm]]
        for (nm in setdiff(paste0("FA", 1:3), names(scores))) features[[nm]] <- 0
        lam <- as.data.frame(unclass(fa$loadings), stringsAsFactors = FALSE)
        names(lam) <- paste0("FA", seq_len(ncol(lam)))
        for (nm in paste0("FA", 1:3)) if (!(nm %in% names(lam))) lam[[nm]] <- NA_real_
        lam$feature <- fa_feature_cols
        lam$uniqueness <- suppressWarnings(as.numeric(fa$uniquenesses[lam$feature]))
        rownames(lam) <- NULL
        fa_loadings <- lam[, c("feature", "FA1", "FA2", "FA3", "uniqueness"), drop = FALSE]
        diagnostics <- rbind(
          diagnostics,
          stpd_state_trajectory_embedding_diag("FA", "n_factors", used_n_fac, "",
                                               "Linear Gaussian model X = Lambda f + epsilon; Cov(X) = Lambda Lambda' + Psi."),
          stpd_state_trajectory_embedding_diag("FA", "mean_uniqueness", signif(mean(fa$uniquenesses, na.rm = TRUE), 6), "",
                                               "Uniqueness is the feature-specific residual variance Psi.")
        )
      }
    }
  }

  if ("isomap" %in% methods) {
    if (n < 5L) {
      diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("Isomap", "status", "skipped", "", "Need at least five time bins."))
    } else {
      iso <- tryCatch(
        stpd_run_isi_state_isomap(
          features,
          n_neighbors = n_neighbors,
          ndim = 3L,
          max_points = max_points,
          feature_cols = feature_cols,
          scaling = "zscore",
          component = "largest"
        ),
        error = function(e) e
      )
      if (inherits(iso, "error")) {
        diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("Isomap", "status", "failed", "", iso$message))
      } else {
        idx <- iso$kept_sample_index
        coords <- iso$scores[, paste0("Isomap", 1:3), drop = FALSE]
        for (nm in names(coords)) {
          x <- suppressWarnings(as.numeric(coords[[nm]]))
          if (all(!is.finite(x))) x <- rep(0, length(idx))
          features[[nm]][idx] <- x
        }
        dg <- iso$diagnostics
        diagnostics <- rbind(
          diagnostics,
          stpd_state_trajectory_embedding_diag("Isomap", "n_neighbors", dg$n_neighbors[1], "",
                                               "Euclidean kNN graph, shortest-path geodesic distances, then classical MDS."),
          stpd_state_trajectory_embedding_diag("Isomap", "embedded_points", dg$n_embedded[1], "", ""),
          stpd_state_trajectory_embedding_diag("Isomap", "residual_variance", signif(dg$residual_variance[1], 6), "",
                                               "1 - cor(geodesic distance, embedding distance)^2."),
          stpd_state_trajectory_embedding_diag("Isomap", "stress", signif(dg$stress[1], 6), "", "")
        )
      }
    }
  }

  if ("tsne" %in% methods) {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("t-SNE", "status", "missing_package", "", "Install the Rtsne package to enable t-SNE."))
    } else {
      idx <- stpd_state_trajectory_sample_index(n, max_points)
      X0 <- X[idx, , drop = FALSE]
      perp <- suppressWarnings(as.numeric(tsne_perplexity %||% 30))[1]
      if (!is.finite(perp) || perp <= 0) perp <- 30
      perp <- min(perp, (nrow(X0) - 1) / 3 - 1e-6)
      if (nrow(X0) < 5L || !is.finite(perp) || perp < 1) {
        diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("t-SNE", "status", "skipped", "", "Need enough sampled bins for a valid perplexity."))
      } else {
        tsne <- tryCatch({
          set.seed(seed)
          Rtsne::Rtsne(
            X0,
            dims = 3L,
            perplexity = perp,
            pca = FALSE,
            check_duplicates = FALSE,
            verbose = FALSE,
            theta = 0.5,
            max_iter = 1000L
          )
        }, error = function(e) e)
        if (inherits(tsne, "error")) {
          diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("t-SNE", "status", "failed", "", tsne$message))
        } else {
          coords <- as.data.frame(tsne$Y, stringsAsFactors = FALSE)
          names(coords) <- paste0("tSNE", seq_len(ncol(coords)))
          for (nm in names(coords)) features[[nm]][idx] <- coords[[nm]]
          for (nm in setdiff(paste0("tSNE", 1:3), names(coords))) features[[nm]][idx] <- 0
          final_cost <- if (!is.null(tsne$itercosts) && length(tsne$itercosts)) tail(tsne$itercosts, 1) else NA_real_
          diagnostics <- rbind(
            diagnostics,
            stpd_state_trajectory_embedding_diag("t-SNE", "perplexity", signif(perp, 6), "",
                                                 "Minimizes KL divergence between high-dimensional Gaussian affinities and low-dimensional Student-t affinities."),
            stpd_state_trajectory_embedding_diag("t-SNE", "embedded_points", nrow(X0), "", ""),
            stpd_state_trajectory_embedding_diag("t-SNE", "final_cost", signif(final_cost, 6), "", "")
          )
        }
      }
    }
  }

  if ("umap" %in% methods) {
    if (!requireNamespace("uwot", quietly = TRUE)) {
      diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("UMAP", "status", "missing_package", "", "Install the uwot package to enable UMAP."))
    } else {
      idx <- stpd_state_trajectory_sample_index(n, max_points)
      X0 <- X[idx, , drop = FALSE]
      k <- max(2L, min(n_neighbors, nrow(X0) - 1L))
      min_dist <- suppressWarnings(as.numeric(umap_min_dist %||% 0.1))[1]
      if (!is.finite(min_dist) || min_dist < 0) min_dist <- 0.1
      if (nrow(X0) < 5L || k < 2L) {
        diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("UMAP", "status", "skipped", "", "Need at least five sampled bins."))
      } else {
        umap <- tryCatch({
          set.seed(seed)
          uwot::umap(
            X0,
            n_components = 3L,
            n_neighbors = k,
            min_dist = min_dist,
            metric = "euclidean",
            n_threads = 1L,
            ret_model = FALSE,
            verbose = FALSE
          )
        }, error = function(e) e)
        if (inherits(umap, "error")) {
          diagnostics <- rbind(diagnostics, stpd_state_trajectory_embedding_diag("UMAP", "status", "failed", "", umap$message))
        } else {
          coords <- as.data.frame(umap, stringsAsFactors = FALSE)
          names(coords) <- paste0("UMAP", seq_len(ncol(coords)))
          for (nm in names(coords)) features[[nm]][idx] <- coords[[nm]]
          for (nm in setdiff(paste0("UMAP", 1:3), names(coords))) features[[nm]][idx] <- 0
          diagnostics <- rbind(
            diagnostics,
            stpd_state_trajectory_embedding_diag("UMAP", "n_neighbors", k, "",
                                                 "Builds a fuzzy simplicial set and optimizes a low-dimensional cross-entropy objective."),
            stpd_state_trajectory_embedding_diag("UMAP", "min_dist", signif(min_dist, 6), "", ""),
            stpd_state_trajectory_embedding_diag("UMAP", "embedded_points", nrow(X0), "", "")
          )
        }
      }
    }
  }

  list(
    features = features,
    variance = variance,
    loadings = loadings,
    fa_loadings = fa_loadings,
    diagnostics = diagnostics,
    pca = pca
  )
}

stpd_make_state_trajectory <- function(trains,
                                       selected_trains = NULL,
                                       bin_sec = 0.1,
                                       start_sec = NULL,
                                       end_sec = NULL,
                                       time_origin = c("aligned", "raw"),
                                       label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
                                       min_isi_sec = 0.001,
                                       auto_others = FALSE,
                                       smoothing_sigma_bins = 1,
                                       embedding_methods = c("pca"),
                                       embedding_n_neighbors = 15L,
                                       embedding_tsne_perplexity = 30,
                                       embedding_umap_min_dist = 0.1,
                                       embedding_seed = 1L,
                                       embedding_max_points = 900L) {
  time_origin <- match.arg(time_origin)
  label_source <- match.arg(label_source)
  if (is.null(trains) || length(trains) == 0L) {
    return(list(bins = data.frame(), features = data.frame(), variance = data.frame(), loadings = data.frame()))
  }
  selected_trains <- as.character(selected_trains %||% names(trains))
  selected_trains <- intersect(selected_trains, names(trains))
  if (length(selected_trains) == 0L) {
    return(list(bins = data.frame(), features = data.frame(), variance = data.frame(), loadings = data.frame()))
  }
  bin_sec <- suppressWarnings(as.numeric(bin_sec %||% 0.1))[1]
  if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.1
  smoothing_sigma_bins <- suppressWarnings(as.numeric(smoothing_sigma_bins %||% 0))[1]
  if (!is.finite(smoothing_sigma_bins) || smoothing_sigma_bins < 0) smoothing_sigma_bins <- 0

  prepared <- lapply(selected_trains, function(tr) {
    dat <- trains[[tr]]
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat) < 2L || !("timestamp_sec" %in% names(dat))) {
      return(NULL)
    }
    dat <- dat[order(dat$idx %||% seq_len(nrow(dat))), , drop = FALSE]
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ok_ts <- is.finite(ts)
    if (sum(ok_ts) < 2L) return(NULL)
    raw_ts <- ts
    if (identical(time_origin, "aligned")) {
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
    list(train = tr, dat = dat, raw_ts = raw_ts, ts = ts, labels = labels)
  })
  prepared <- prepared[!vapply(prepared, is.null, logical(1))]
  if (length(prepared) == 0L) {
    return(list(bins = data.frame(), features = data.frame(), variance = data.frame(), loadings = data.frame()))
  }

  all_ts <- unlist(lapply(prepared, function(x) x$ts[is.finite(x$ts)]), use.names = FALSE)
  auto_start <- min(all_ts, na.rm = TRUE)
  auto_end <- max(all_ts, na.rm = TRUE)
  start_use <- suppressWarnings(as.numeric(start_sec %||% auto_start))[1]
  end_use <- suppressWarnings(as.numeric(end_sec %||% auto_end))[1]
  if (!is.finite(start_use)) start_use <- auto_start
  if (!is.finite(end_use) || end_use <= start_use) end_use <- auto_end
  if (!is.finite(end_use) || end_use <= start_use) end_use <- start_use + bin_sec
  bins <- stpd_state_trajectory_make_bin_table(start_use, end_use, bin_sec)
  train_windows <- do.call(rbind, lapply(prepared, function(x) {
    raw_ok <- is.finite(x$raw_ts)
    analysis_ok <- is.finite(x$ts)
    raw_start <- if (any(raw_ok)) min(x$raw_ts[raw_ok], na.rm = TRUE) else NA_real_
    raw_end <- if (any(raw_ok)) max(x$raw_ts[raw_ok], na.rm = TRUE) else NA_real_
    analysis_start <- if (any(analysis_ok)) min(x$ts[analysis_ok], na.rm = TRUE) else NA_real_
    analysis_end <- if (any(analysis_ok)) max(x$ts[analysis_ok], na.rm = TRUE) else NA_real_
    data.frame(
      train = x$train,
      raw_start_sec = raw_start,
      raw_end_sec = raw_end,
      raw_duration_sec = raw_end - raw_start,
      analysis_start_sec = analysis_start,
      analysis_end_sec = analysis_end,
      analysis_duration_sec = analysis_end - analysis_start,
      stringsAsFactors = FALSE
    )
  }))
  finite_dur <- train_windows$raw_duration_sec[is.finite(train_windows$raw_duration_sec)]
  window_summary <- data.frame(
    n_trains = length(prepared),
    bin_sec = bin_sec,
    n_bins = nrow(bins),
    window_start_sec = start_use,
    window_end_sec = end_use,
    window_duration_sec = end_use - start_use,
    train_duration_min_sec = if (length(finite_dur)) min(finite_dur, na.rm = TRUE) else NA_real_,
    train_duration_median_sec = if (length(finite_dur)) stats::median(finite_dur, na.rm = TRUE) else NA_real_,
    train_duration_max_sec = if (length(finite_dur)) max(finite_dur, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
  groups <- stpd_state_trajectory_pattern_groups()
  group_names <- names(groups)

  features <- bins
  features$n_trains <- length(prepared)
  features$total_spike_count <- 0
  features$firing_rate_hz <- 0
  for (g in group_names) {
    features[[paste0(g, "_spike_count")]] <- 0
    features[[paste0(g, "_rate_hz")]] <- 0
    features[[paste0(g, "_fraction")]] <- 0
  }

  for (item in prepared) {
    tr <- item$train
    ts <- item$ts
    labels <- item$labels
    tr_safe <- stpd_state_trajectory_clean_feature_name(tr)
    train_count <- numeric(nrow(bins))
    train_group_count <- stats::setNames(vector("list", length(group_names)), group_names)
    train_group_overlap <- stats::setNames(vector("list", length(group_names)), group_names)
    for (g in group_names) {
      train_group_count[[g]] <- numeric(nrow(bins))
      train_group_overlap[[g]] <- numeric(nrow(bins))
    }

    for (bb in seq_len(nrow(bins))) {
      b0 <- bins$bin_start_sec[bb]
      b1 <- bins$bin_end_sec[bb]
      bw <- bins$bin_width_sec[bb]
      spike_idx <- which(is.finite(ts) & ts >= b0 & ts < b1)
      if (bb == nrow(bins)) spike_idx <- which(is.finite(ts) & ts >= b0 & ts <= b1)
      train_count[bb] <- length(spike_idx)
      if (length(spike_idx) > 0L) {
        lab_sp <- labels[spike_idx]
        for (g in group_names) {
          train_group_count[[g]][bb] <- sum(lab_sp %in% groups[[g]], na.rm = TRUE)
        }
      }
      if (length(ts) >= 2L) {
        for (ii in seq.int(2L, length(ts))) {
          if (!is.finite(ts[ii - 1L]) || !is.finite(ts[ii]) || ts[ii] <= ts[ii - 1L]) next
          ov <- stpd_state_trajectory_interval_overlap(ts[ii - 1L], ts[ii], b0, b1)
          if (!is.finite(ov) || ov <= 0) next
          lab <- labels[ii]
          for (g in group_names) {
            if (lab %in% groups[[g]]) train_group_overlap[[g]][bb] <- train_group_overlap[[g]][bb] + ov
          }
        }
      }
      if (!is.finite(bw) || bw <= 0) bw <- bin_sec
    }

    features$total_spike_count <- features$total_spike_count + train_count
    features[[paste0(tr_safe, "__firing_rate_hz")]] <- train_count / bins$bin_width_sec
    for (g in group_names) {
      features[[paste0(g, "_spike_count")]] <- features[[paste0(g, "_spike_count")]] + train_group_count[[g]]
      features[[paste0(g, "_fraction")]] <- features[[paste0(g, "_fraction")]] + train_group_overlap[[g]] / bins$bin_width_sec
      features[[paste0(tr_safe, "__", g, "_rate_hz")]] <- train_group_count[[g]] / bins$bin_width_sec
      features[[paste0(tr_safe, "__", g, "_fraction")]] <- train_group_overlap[[g]] / bins$bin_width_sec
    }
  }

  denom <- pmax(1, features$n_trains)
  features$firing_rate_hz <- features$total_spike_count / bins$bin_width_sec / denom
  for (g in group_names) {
    features[[paste0(g, "_rate_hz")]] <- features[[paste0(g, "_spike_count")]] / bins$bin_width_sec / denom
    features[[paste0(g, "_fraction")]] <- pmax(0, pmin(1, features[[paste0(g, "_fraction")]] / denom))
  }
  features$burst_activity <- features$burst_rate_hz
  features$pause_activity <- features$pause_fraction
  features$tonic_activity <- features$tonic_rate_hz
  features$hf_spiking_activity <- features$hf_spiking_rate_hz
  features$hf_activity <- features$hf_spiking_activity
  state_score_cols <- c(
    "burst_fraction", "pause_fraction", "tonic_fraction",
    "hf_spiking_fraction", "others_fraction", "unlabeled_fraction"
  )
  state_names <- c("burst", "pause", "tonic", "hf_spiking", "others", "unlabeled")
  score <- as.matrix(features[, state_score_cols, drop = FALSE])
  score[!is.finite(score)] <- 0
  max_idx <- max.col(score, ties.method = "first")
  zero_rows <- rowSums(score, na.rm = TRUE) <= 0
  features$dominant_state <- state_names[max_idx]
  features$dominant_state[zero_rows] <- "unlabeled"
  per_train_states <- stpd_state_trajectory_per_train_states(features, vapply(prepared, function(x) x$train, character(1)))

  numeric_cols <- names(features)[vapply(features, is.numeric, logical(1))]
  smooth_exclude <- c("bin_id", "bin_start_sec", "bin_end_sec", "time_mid_sec", "bin_width_sec",
                      "n_trains", "total_spike_count")
  smooth_cols <- setdiff(numeric_cols, smooth_exclude)
  if (smoothing_sigma_bins > 0 && nrow(features) >= 3L) {
    for (nm in smooth_cols) features[[nm]] <- stpd_state_trajectory_gaussian_smooth(features[[nm]], smoothing_sigma_bins)
  }

  feature_cols <- names(features)[grepl("__", names(features), fixed = TRUE)]
  feature_cols <- feature_cols[vapply(features[feature_cols], is.numeric, logical(1))]
  varying <- vapply(feature_cols, function(nm) {
    x <- suppressWarnings(as.numeric(features[[nm]]))
    x <- x[is.finite(x)]
    length(x) >= 2L && length(unique(signif(x, 12))) >= 2L
  }, logical(1))
  feature_cols <- feature_cols[varying]
  emb <- stpd_state_trajectory_compute_embeddings(
    features = features,
    feature_cols = feature_cols,
    methods = embedding_methods,
    n_neighbors = embedding_n_neighbors,
    tsne_perplexity = embedding_tsne_perplexity,
    umap_min_dist = embedding_umap_min_dist,
    seed = embedding_seed,
    max_points = embedding_max_points
  )
  features <- emb$features
  variance <- emb$variance
  loadings <- emb$loadings
  fa_loadings <- emb$fa_loadings
  embedding_diagnostics <- emb$diagnostics
  pca <- emb$pca

  list(
    bins = features,
    features = features,
    variance = variance,
    loadings = loadings,
    fa_loadings = fa_loadings,
    embedding_diagnostics = embedding_diagnostics,
    pca = pca,
    feature_cols = feature_cols,
    selected_trains = vapply(prepared, function(x) x$train, character(1)),
    per_train_states = per_train_states,
    train_windows = train_windows,
    window_summary = window_summary,
    bin_sec = bin_sec,
    time_origin = time_origin,
    label_source = label_source,
    smoothing_sigma_bins = smoothing_sigma_bins
  )
}

stpd_state_trajectory_plot <- function(res,
                                       coordinate_mode = c("pattern_axes", "pca", "fa", "isomap", "tsne", "umap"),
                                       axis_cols = NULL,
                                       title = NULL) {
  coordinate_mode <- match.arg(coordinate_mode)
  dat <- res$features %||% res$bins %||% data.frame()
  if (is.null(dat) || nrow(dat) == 0L) {
    return(layout(
      plot_ly(),
      scene = list(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), zaxis = list(visible = FALSE)),
      annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                              text = "No state trajectory can be built from the selected trains.",
                              showarrow = FALSE))
    ))
  }
  if (identical(coordinate_mode, "pca")) {
    cols <- c("PC1", "PC2", "PC3")
    axis_titles <- cols
    plot_title <- title %||% "PCA state trajectory"
  } else if (identical(coordinate_mode, "fa")) {
    cols <- c("FA1", "FA2", "FA3")
    axis_titles <- cols
    plot_title <- title %||% "Factor-analysis state trajectory"
  } else if (identical(coordinate_mode, "isomap")) {
    cols <- c("Isomap1", "Isomap2", "Isomap3")
    axis_titles <- cols
    plot_title <- title %||% "Isomap state trajectory"
  } else if (identical(coordinate_mode, "tsne")) {
    cols <- c("tSNE1", "tSNE2", "tSNE3")
    axis_titles <- cols
    plot_title <- title %||% "t-SNE state trajectory"
  } else if (identical(coordinate_mode, "umap")) {
    cols <- c("UMAP1", "UMAP2", "UMAP3")
    axis_titles <- cols
    plot_title <- title %||% "UMAP state trajectory"
  } else {
    default_cols <- c("burst_activity", "pause_activity", "tonic_activity")
    allowed_cols <- unname(stpd_state_trajectory_axis_choices())
    cols <- as.character(axis_cols %||% default_cols)
    cols <- unique(cols[nzchar(cols) & cols %in% allowed_cols])
    for (default_col in default_cols) {
      if (length(cols) >= 3L) break
      if (!(default_col %in% cols)) cols <- c(cols, default_col)
    }
    cols <- cols[seq_len(3L)]
    axis_titles <- stpd_state_trajectory_axis_titles(cols)
    plot_title <- title %||% "Custom pattern-state trajectory"
  }
  missing_cols <- setdiff(cols, names(dat))
  if (length(missing_cols) > 0L) {
    dat[missing_cols] <- NA_real_
  }
  ok <- is.finite(dat[[cols[1]]]) & is.finite(dat[[cols[2]]]) & is.finite(dat[[cols[3]]])
  dat <- dat[ok, , drop = FALSE]
  if (nrow(dat) < 2L) {
    return(layout(
      plot_ly(),
      scene = list(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), zaxis = list(visible = FALSE)),
      annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                              text = "Need at least two valid time bins for a trajectory.",
                              showarrow = FALSE))
    ))
  }
  dat$state_color <- stpd_state_trajectory_state_colors(dat$dominant_state)
  dat$hover <- paste0(
    "bin ", dat$bin_id,
    "<br>time: ", signif(dat$bin_start_sec, 5), "-", signif(dat$bin_end_sec, 5), " s",
    "<br>state: ", dat$dominant_state,
    "<br>firing rate: ", signif(dat$firing_rate_hz, 4), " Hz/train",
    "<br>burst-family rate: ", signif(dat$burst_activity, 4),
    "<br>pause occupancy: ", signif(dat$pause_activity, 4),
    "<br>tonic-family rate: ", signif(dat$tonic_activity, 4),
    "<br>HF spiking rate: ", signif(dat$hf_spiking_rate_hz, 4),
    "<br>HF spiking occupancy: ", signif(dat$hf_spiking_fraction, 4)
  )
  p <- plot_ly(source = "state_trajectory")
  p <- add_trace(
    p,
    data = dat,
    x = as.formula(paste0("~", cols[1])),
    y = as.formula(paste0("~", cols[2])),
    z = as.formula(paste0("~", cols[3])),
    type = "scatter3d",
    mode = "lines",
    line = list(color = "rgba(51,65,85,0.36)", width = 5),
    showlegend = FALSE,
    hoverinfo = "skip",
    inherit = FALSE
  )
  for (state in unique(as.character(dat$dominant_state))) {
    sub <- dat[as.character(dat$dominant_state) == state, , drop = FALSE]
    p <- add_trace(
      p,
      data = sub,
      x = as.formula(paste0("~", cols[1])),
      y = as.formula(paste0("~", cols[2])),
      z = as.formula(paste0("~", cols[3])),
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 4.8, color = stpd_state_trajectory_state_colors(state),
                    line = list(color = "#ffffff", width = 0.7)),
      name = state,
      text = ~hover,
      hoverinfo = "text",
      inherit = FALSE
    )
  }
  layout(
    p,
    title = list(text = plot_title, x = 0.02, font = list(size = 15, color = "#111827")),
    scene = list(
      xaxis = list(title = axis_titles[1], backgroundcolor = "#ffffff", gridcolor = "#e5e7eb", zerolinecolor = "#cbd5e1"),
      yaxis = list(title = axis_titles[2], backgroundcolor = "#ffffff", gridcolor = "#e5e7eb", zerolinecolor = "#cbd5e1"),
      zaxis = list(title = axis_titles[3], backgroundcolor = "#ffffff", gridcolor = "#e5e7eb", zerolinecolor = "#cbd5e1")
    ),
    legend = list(orientation = "h", x = 0, y = 1.05, font = list(size = 10)),
    margin = list(l = 0, r = 0, t = 70, b = 18),
    paper_bgcolor = "#ffffff",
    font = list(color = "#1f2937")
  ) %>%
    config(displaylogo = FALSE, scrollZoom = TRUE)
}
