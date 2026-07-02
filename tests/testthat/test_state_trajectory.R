make_state_trajectory_train <- function(spike_times, labels) {
  n <- length(spike_times)
  data.frame(
    idx = seq_len(n),
    timestamp_sec = spike_times,
    ISI_sec = c(NA_real_, diff(spike_times)),
    pattern_manual = rep("", n),
    pattern_auto = rep_len(labels, n),
    stringsAsFactors = FALSE
  )
}

test_that("multi-train state trajectory builds pattern-axis and PCA features", {
  tr1 <- make_state_trajectory_train(
    c(0.00, 0.05, 0.10, 0.30, 0.45, 0.65, 0.78),
    c("", "burst", "burst", "pause", "tonic", "tonic", "pause")
  )
  tr2 <- make_state_trajectory_train(
    c(0.00, 0.08, 0.18, 0.28, 0.50, 0.70, 0.82),
    c("", "tonic", "burst", "burst", "pause", "tonic", "tonic")
  )

  res <- stpd_make_state_trajectory(
    list(train_A = tr1, train_B = tr2),
    selected_trains = c("train_A", "train_B"),
    bin_sec = 0.2,
    start_sec = 0,
    end_sec = 0.8,
    time_origin = "aligned",
    label_source = "auto",
    smoothing_sigma_bins = 0
  )

  expect_equal(res$selected_trains, c("train_A", "train_B"))
  expect_gt(nrow(res$features), 2)
  expect_true(all(c("burst_activity", "pause_activity", "tonic_activity",
                    "dominant_state", "PC1", "PC2", "PC3") %in% names(res$features)))
  expect_true("hf_spiking_activity" %in% names(res$features))
  expect_true(any(res$features$burst_activity > 0))
  expect_true(any(res$features$pause_activity > 0))
  expect_true(any(res$features$tonic_activity > 0))
  expect_true(length(res$feature_cols) >= 2)
  expect_true(all(c("window_duration_sec", "train_duration_median_sec") %in% names(res$window_summary)))
  expect_equal(res$window_summary$n_bins, nrow(res$features))
  expect_true(all(c("feature", "PC1", "PC2", "PC3") %in% names(res$loadings)))
  expect_true("hf_spiking_activity" %in% unname(stpd_state_trajectory_axis_choices()))
  expect_s3_class(stpd_state_trajectory_plot(res, coordinate_mode = "pattern_axes"), "plotly")
  expect_s3_class(
    stpd_state_trajectory_plot(
      res,
      coordinate_mode = "pattern_axes",
      axis_cols = c("burst_activity", "pause_activity", "hf_spiking_activity")
    ),
    "plotly"
  )
  expect_s3_class(stpd_state_trajectory_plot(res, coordinate_mode = "pca"), "plotly")
})

test_that("state trajectory supports linear and nonlinear embeddings", {
  skip_if_not_installed("Rtsne")
  skip_if_not_installed("uwot")

  make_dense_train <- function(seed) {
    set.seed(seed)
    ts <- sort(runif(260, 0, 5))
    labels <- rep("unlabeled", length(ts))
    labels[ts > 0.5 & ts <= 1.2] <- "burst"
    labels[ts > 2.0 & ts <= 2.7] <- "pause"
    labels[ts > 3.3 & ts <= 4.2] <- "high_frequency_spiking"
    make_state_trajectory_train(ts, labels)
  }

  trains <- list(
    dense_A = make_dense_train(11),
    dense_B = make_dense_train(12),
    dense_C = make_dense_train(13)
  )
  res <- stpd_make_state_trajectory(
    trains,
    selected_trains = names(trains),
    bin_sec = 0.05,
    start_sec = 0,
    end_sec = 5,
    label_source = "auto",
    smoothing_sigma_bins = 1,
    embedding_methods = c("pca", "fa", "isomap", "tsne", "umap"),
    embedding_n_neighbors = 8,
    embedding_tsne_perplexity = 8,
    embedding_umap_min_dist = 0.1,
    embedding_seed = 123,
    embedding_max_points = 150
  )

  for (cols in list(paste0("PC", 1:3), paste0("FA", 1:3), paste0("Isomap", 1:3),
                    paste0("tSNE", 1:3), paste0("UMAP", 1:3))) {
    expect_true(all(cols %in% names(res$features)))
    expect_gt(sum(stats::complete.cases(res$features[, cols, drop = FALSE])), 5)
  }
  expect_true(all(c("method", "metric", "value", "note") %in% names(res$embedding_diagnostics)))
  expect_true(any(res$embedding_diagnostics$method == "FA" & res$embedding_diagnostics$metric == "n_factors"))
  expect_s3_class(stpd_state_trajectory_plot(res, coordinate_mode = "fa"), "plotly")
  expect_s3_class(stpd_state_trajectory_plot(res, coordinate_mode = "isomap"), "plotly")
  expect_s3_class(stpd_state_trajectory_plot(res, coordinate_mode = "tsne"), "plotly")
  expect_s3_class(stpd_state_trajectory_plot(res, coordinate_mode = "umap"), "plotly")
})

test_that("state trajectory respects selected trains and empty selections", {
  tr1 <- make_state_trajectory_train(
    c(0.00, 0.05, 0.10, 0.30, 0.45),
    c("", "burst", "burst", "pause", "tonic")
  )
  tr2 <- make_state_trajectory_train(
    c(0.00, 0.20, 0.40, 0.60, 0.80),
    c("", "tonic", "tonic", "pause", "pause")
  )

  res <- stpd_make_state_trajectory(
    list(train_A = tr1, train_B = tr2),
    selected_trains = "train_A",
    bin_sec = 0.2,
    start_sec = 0,
    end_sec = 0.8,
    label_source = "auto",
    smoothing_sigma_bins = 0
  )

  expect_equal(res$selected_trains, "train_A")
  expect_equal(unique(res$features$n_trains), 1)

  empty <- stpd_make_state_trajectory(
    list(train_A = tr1, train_B = tr2),
    selected_trains = "missing_train",
    bin_sec = 0.2
  )
  expect_equal(nrow(empty$features), 0)
})

test_that("raw timestamp trajectories auto-start at selected train timestamps", {
  tr1 <- make_state_trajectory_train(
    30 + c(0.0, 0.2, 1.0, 5.0, 9.8),
    c("", "burst", "tonic", "pause", "tonic")
  )
  tr2 <- make_state_trajectory_train(
    31 + c(0.0, 0.3, 1.5, 5.2, 10.0),
    c("", "burst", "burst", "pause", "tonic")
  )

  res <- stpd_make_state_trajectory(
    list(raw_A = tr1, raw_B = tr2),
    selected_trains = c("raw_A", "raw_B"),
    bin_sec = 1,
    start_sec = NULL,
    end_sec = NULL,
    time_origin = "raw",
    label_source = "auto",
    smoothing_sigma_bins = 0
  )

  expect_equal(res$window_summary$window_start_sec, 30)
  expect_equal(res$window_summary$window_end_sec, 41)
  expect_equal(res$train_windows$raw_duration_sec, c(9.8, 10.0), tolerance = 1e-12)
  expect_lte(res$window_summary$window_duration_sec, 11)
})

test_that("dominant state uses occupancy, not incompatible firing-rate units", {
  pause_train <- make_state_trajectory_train(
    c(0, 1.0, 1.2),
    c("", "pause", "tonic")
  )
  hf_train <- make_state_trajectory_train(
    seq(0, 1.0, by = 0.02),
    c("", rep("high_frequency_spiking", 50))
  )

  res <- stpd_make_state_trajectory(
    list(pause_train = pause_train, hf_train = hf_train),
    selected_trains = c("pause_train", "hf_train"),
    bin_sec = 0.2,
    start_sec = 0,
    end_sec = 1,
    time_origin = "aligned",
    label_source = "auto",
    smoothing_sigma_bins = 1
  )

  expect_true("pause" %in% res$features$dominant_state)
  expect_true(any(res$features$pause_fraction >= 0.49))
  expect_true(any(res$features$hf_spiking_rate_hz > 10))
})

test_that("HF tonic is grouped into tonic-family while HF spiking remains independent", {
  tonic_train <- make_state_trajectory_train(
    c(0, 0.1, 0.2, 0.3, 0.4),
    c("", "high_frequency_tonic", "high_frequency_tonic", "high_frequency_tonic", "high_frequency_tonic")
  )
  hf_spiking_train <- make_state_trajectory_train(
    c(0, 0.1, 0.2, 0.3, 0.4),
    c("", "high_frequency_spiking", "high_frequency_spiking", "high_frequency_spiking", "high_frequency_spiking")
  )

  tonic_res <- stpd_make_state_trajectory(
    list(tonic_train = tonic_train),
    selected_trains = "tonic_train",
    bin_sec = 0.2,
    start_sec = 0,
    end_sec = 0.4,
    label_source = "auto",
    smoothing_sigma_bins = 0
  )
  hf_res <- stpd_make_state_trajectory(
    list(hf_spiking_train = hf_spiking_train),
    selected_trains = "hf_spiking_train",
    bin_sec = 0.2,
    start_sec = 0,
    end_sec = 0.4,
    label_source = "auto",
    smoothing_sigma_bins = 0
  )

  expect_true(any(tonic_res$features$dominant_state == "tonic"))
  expect_false("hf_tonic" %in% tonic_res$features$dominant_state)
  expect_true(any(hf_res$features$dominant_state == "hf_spiking"))
})

test_that("state-pair analysis reports joint states, enrichment, and transitions", {
  tr_a <- make_state_trajectory_train(
    c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    c("", "pause", "pause", "burst", "burst", "tonic")
  )
  tr_b <- make_state_trajectory_train(
    c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    c("", "high_frequency_spiking", "high_frequency_spiking", "tonic", "tonic", "burst")
  )
  tr_c <- make_state_trajectory_train(
    c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    c("", "burst", "burst", "pause", "pause", "tonic")
  )
  res <- stpd_make_state_trajectory(
    list(unit_a = tr_a, unit_b = tr_b, unit_c = tr_c),
    selected_trains = c("unit_a", "unit_b", "unit_c"),
    bin_sec = 0.2,
    start_sec = 0,
    end_sec = 1.0,
    label_source = "auto",
    smoothing_sigma_bins = 0
  )
  pair <- stpd_make_state_pair_analysis(res, train_x = "unit_a", train_y = "unit_b", lag_bins = 0)

  expect_gt(nrow(pair$pair_bins), 2)
  expect_true(all(c("state_x", "state_y", "observed_count", "expected_count",
                    "log2_enrichment", "p_fdr", "association") %in% names(pair$matrix)))
  pause_hf <- pair$matrix[pair$matrix$state_x == "pause" & pair$matrix$state_y == "hf_spiking", , drop = FALSE]
  expect_equal(nrow(pause_hf), 1)
  expect_gt(pause_hf$observed_count, 0)
  expect_gt(nrow(pair$transitions), 0)
  expect_s3_class(stpd_state_pair_heatmap(pair, value = "log2_enrichment"), "plotly")
  timeline <- stpd_state_pair_timeline_plot(pair)
  expect_s3_class(timeline, "plotly")
  timeline_layout <- timeline$x$layoutAttrs[[length(timeline$x$layoutAttrs)]]
  expect_identical(timeline_layout$yaxis$autorange, "reversed")
  expect_s3_class(stpd_state_pair_transition_heatmap(pair, value = "prob"), "plotly")

  lagged <- stpd_make_state_pair_analysis(res, train_x = "unit_a", train_y = "unit_b", lag_bins = 1)
  expect_equal(lagged$lag_bins, 1)
  expect_lt(nrow(lagged$pair_bins), nrow(pair$pair_bins))

  multi <- stpd_make_state_pair_analysis(res, trains = c("unit_a", "unit_b", "unit_c"), lag_bins = 0)
  expect_equal(multi$train_count, 3)
  expect_equal(multi$trains, c("unit_a", "unit_b", "unit_c"))
  expect_true(all(c("state__unit_a", "state__unit_b", "state__unit_c") %in% names(multi$pair_bins)))
  expect_true(all(c("state__unit_a", "state__unit_b", "state__unit_c",
                    "joint_state_labeled", "observed_expected_ratio") %in% names(multi$matrix)))
  expect_true(any(grepl("unit_c=", multi$pair_bins$joint_state_labeled)))
  expect_true(any(multi$matrix$observed_count > 0))
  expect_gt(nrow(multi$transitions), 0)
  expect_true(length(multi$notes) > 0)
  expect_s3_class(stpd_state_pair_heatmap(multi, value = "log2_enrichment"), "plotly")
  expect_s3_class(stpd_state_pair_timeline_plot(multi), "plotly")
  expect_s3_class(stpd_state_pair_transition_heatmap(multi, value = "prob"), "plotly")
})
