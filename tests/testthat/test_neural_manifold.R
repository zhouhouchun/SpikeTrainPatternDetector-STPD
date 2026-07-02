make_neural_manifold_train <- function(times) {
  times <- sort(as.numeric(times))
  data.frame(
    idx = seq_along(times),
    timestamp_sec = times,
    ISI_sec = c(NA_real_, diff(times)),
    pattern_manual = rep("", length(times)),
    pattern_auto = rep("", length(times)),
    stringsAsFactors = FALSE
  )
}

mark_neural_manifold_events <- function(dat) {
  dat$pattern_auto[dat$timestamp_sec >= 0.35 & dat$timestamp_sec <= 0.85] <- "burst"
  dat$pattern_auto[dat$timestamp_sec >= 1.10 & dat$timestamp_sec <= 1.55] <- "pause"
  dat
}

test_that("neural manifold builds population matrix, embeddings, and validation", {
  set.seed(42)
  behavior <- data.frame(
    time = seq(0, 2, by = 0.02),
    movement = sin(seq(0, 2, by = 0.02) * 2 * pi),
    stringsAsFactors = FALSE
  )
  trains <- list(
    unit_a = make_neural_manifold_train(c(seq(0.05, 1.95, by = 0.08), runif(8, 0.4, 0.8))),
    unit_b = make_neural_manifold_train(c(seq(0.03, 1.9, by = 0.11), runif(10, 1.0, 1.5))),
    unit_c = make_neural_manifold_train(c(seq(0.02, 1.8, by = 0.13), runif(10, 0.2, 1.8))),
    unit_d = make_neural_manifold_train(c(seq(0.04, 1.7, by = 0.17), runif(12, 0.6, 1.4)))
  )
  trains <- lapply(trains, mark_neural_manifold_events)
  pop <- stpd_make_neural_population_matrix(
    trains,
    selected_trains = names(trains),
    bin_sec = 0.05,
    start_sec = 0,
    end_sec = 2,
    time_origin = "raw",
    transform = "sqrt_count",
    smoothing_sigma_bins = 1,
    scaling = "zscore",
    behavior = behavior,
    behavior_time_col = "time",
    behavior_value_col = "movement"
  )
  expect_equal(ncol(pop$X), 4)
  expect_gt(nrow(pop$features), 20)
  expect_true(any(is.finite(pop$features$behavior_numeric)))

  pca <- stpd_run_neural_manifold_embedding(pop, method = "pca")
  pca <- stpd_neural_add_event_state_layer(pca, trains, selected_trains = names(trains), label_source = "auto")
  expect_true(all(c("NM1", "NM2", "NM3") %in% names(pca$features)))
  expect_true("event_state" %in% names(pca$features))
  expect_true(any(pca$features$event_state %in% c("burst", "pause")))
  expect_gt(nrow(pca$diagnostics), 1)
  pca$event_geometry <- stpd_neural_event_geometry(pca)
  pca$event_distances <- stpd_neural_event_distance_tests(pca, n_perm = 9, seed = 1)
  pca$event_triggered <- stpd_neural_event_triggered_trajectory(pca, window_bins = 3)
  pca$event_dynamics <- stpd_neural_event_dynamics_summary(pca, window_bins = 3)
  expect_true("event_state" %in% names(pca$event_geometry))
  expect_true(all(c("state_a", "state_b", "centroid_distance") %in% names(pca$event_distances)))
  expect_true(any(pca$event_triggered$event_state %in% c("burst", "pause")))
  expect_true(any(pca$event_dynamics$event_state %in% c("burst", "pause")))
  pca$validation <- stpd_neural_manifold_validation(pca, seed = 1, n_neighbors = 5, event_permutations = 9)
  expect_true(all(c("metric", "value", "status", "note") %in% names(pca$validation)))
  expect_true(any(pca$validation$metric == "trustworthiness"))
  expect_true(any(pca$validation$metric == "event_label_decoding_accuracy"))
  expect_true(any(pca$validation$metric == "behavior_delta_r2_event"))
  expect_s3_class(stpd_neural_manifold_plot(pca), "plotly")

  fa <- stpd_run_neural_manifold_embedding(pop, method = "fa")
  expect_true(all(c("NM1", "NM2", "NM3") %in% names(fa$features)))
  expect_true("uniqueness" %in% names(fa$loadings))

  supervised <- stpd_run_neural_manifold_embedding(pop, method = "cebra")
  expect_true(all(c("NM1", "NM2", "NM3") %in% names(supervised$features)))
  expect_true(any(grepl("external CEBRA", supervised$diagnostics$note)))

  phate <- stpd_run_neural_manifold_embedding(pop, method = "phate", n_neighbors = 5, diffusion_time = 2)
  expect_true(all(c("NM1", "NM2", "NM3") %in% names(phate$features)))
  expect_true(any(phate$diagnostics$method == "PHATE"))
})
