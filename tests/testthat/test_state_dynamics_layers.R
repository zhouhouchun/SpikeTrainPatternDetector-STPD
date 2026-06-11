make_state_dynamics_train <- function(reps = 5) {
  isi <- rep(c(
    0.030, 0.032, 0.031, 0.029,
    0.006, 0.007, 0.008,
    0.130,
    0.026, 0.028, 0.027,
    0.012, 0.013, 0.014, 0.016
  ), reps)
  spike_times <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(spike_times),
    timestamp_sec = spike_times,
    ISI_sec = c(NA_real_, diff(spike_times)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[dat$ISI_sec <= 0.009] <- "burst"
  dat$pattern_auto[dat$ISI_sec >= 0.100] <- "pause"
  dat$pattern_auto[dat$ISI_sec > 0.009 & dat$ISI_sec <= 0.018] <- "high_frequency_tonic"
  dat$pattern_auto[dat$ISI_sec > 0.018 & dat$ISI_sec < 0.100] <- "tonic"
  dat
}

test_that("state transition grammar returns dwell, entropy, motifs, and surrogates", {
  labels <- c("tonic", "tonic", "burst", "burst", "pause", "tonic", "burst", "pause")

  tm <- stpd_state_transition_matrix(labels, normalize = "row")
  expect_true(all(c("tonic", "burst", "pause") %in% tm$states))
  expect_true(is.finite(tm$matrix["tonic", "burst"]))

  dwell <- stpd_state_dwell_times(labels)
  expect_true(all(c("label", "n_isi", "duration_sec") %in% names(dwell)))
  expect_true(any(dwell$label == "burst" & dwell$n_isi == 2))

  ent <- stpd_transition_entropy(labels)
  expect_true("weighted_rate" %in% ent$state)

  tm_no_self <- stpd_state_transition_matrix(
    labels,
    states = c("burst", "pause", "tonic"),
    smoothing = 1,
    normalize = "none",
    drop_self = TRUE
  )
  expect_equal(unname(diag(tm_no_self$counts)), rep(0, 3))

  balanced_no_self <- c("A", "B", "A", "C", "A", "B", "A", "C")
  ent_no_self <- stpd_transition_entropy(balanced_no_self, drop_self = TRUE, base = 2)
  a_entropy <- ent_no_self[ent_no_self$state == "A", , drop = FALSE]
  expect_equal(a_entropy$entropy, 1, tolerance = 1e-12)
  expect_equal(a_entropy$normalized_entropy, 1, tolerance = 1e-12)

  motifs <- stpd_motif_frequency(labels, motif_length = 3)
  expect_gt(nrow(motifs), 0)
  expect_true(all(c("motif", "n", "rate") %in% names(motifs)))

  ctrl <- stpd_state_surrogate_controls(
    labels,
    n_surrogates = 5,
    methods = c("label_permutation", "block_shuffle"),
    seed = 11
  )
  expect_true(all(c("method", "metric", "p_two_sided") %in% names(ctrl$summary)))
  expect_true("transition_entropy" %in% ctrl$summary$metric)
})

test_that("exploration layer computes diffusion, PHATE-like, Isomap sweep, and RQA", {
  dat <- make_state_dynamics_train(reps = 6)
  feats <- stpd_make_isi_state_space_features(dat, train = "synthetic", label_source = "auto",
                                              k = 3, min_isi_sec = 0.0009)

  diff <- stpd_run_isi_state_diffusion_map(feats, ndim = 3, max_points = 120)
  expect_true(all(c("Diffusion1", "Diffusion2", "Diffusion3") %in% names(diff$scores)))
  expect_true(is.finite(diff$diagnostics$epsilon))

  ph <- stpd_run_isi_state_phate(feats, ndim = 2, diffusion_time = 3, max_points = 120,
                                 use_phateR = FALSE)
  expect_true(all(c("PHATE1", "PHATE2", "PHATE3") %in% names(ph$scores)))
  expect_equal(ph$diagnostics$method, "diffusion_potential_mds")

  iso <- stpd_run_isi_state_isomap_sweep(feats, neighbor_grid = c(5, 8), max_points = 80)
  expect_true("ok" %in% names(iso$diagnostics))
  expect_gte(nrow(iso$diagnostics), 2)

  rec <- stpd_make_recurrence_plot(feats, recurrence_rate = 0.08, max_points = 100)
  expect_true(is.matrix(rec$matrix))
  expect_true(all(c("recurrence_rate", "determinism", "laminarity") %in% names(rec$metrics)))
})

test_that("model layer builds candidate states, HSMM-style decoding, and validation summaries", {
  dat <- make_state_dynamics_train(reps = 7)
  feats <- stpd_make_isi_state_space_features(dat, train = "synthetic", label_source = "auto",
                                              k = 3, min_isi_sec = 0.0009)

  rule <- stpd_candidate_states_rule_based(feats)
  expect_equal(nrow(rule), nrow(feats))
  expect_true("candidate_state" %in% names(rule))

  gmm <- stpd_candidate_states_gmm(feats, n_states = 2:4, seed = 7)
  expect_equal(nrow(gmm$scores), nrow(feats))
  expect_true(any(gmm$diagnostics$selected))
  expect_true(all(c("gmm_state", "candidate_state", "gmm_confidence") %in% names(gmm$scores)))

  hsmm <- stpd_decode_hsmm(rule$candidate_state, max_duration = 20)
  expect_equal(length(hsmm$decoded), nrow(feats))
  expect_true(is.finite(hsmm$logLik))

  agree <- stpd_label_agreement(hsmm$decoded, rule$candidate_state)
  expect_true(all(c("accuracy", "kappa") %in% names(agree$summary)))

  held <- stpd_hsmm_heldout_likelihood(rule$candidate_state, max_duration = 20)
  expect_true(is.finite(held$mean_logLik_per_isi))

  boot <- stpd_state_bootstrap_metrics(rule$candidate_state, n_bootstrap = 5, seed = 3)
  expect_true(all(c("metric", "observed", "ci_low", "ci_high") %in% names(boot$summary)))
})

test_that("train-level transition model data supports nucleus-level comparisons", {
  tr1 <- make_state_dynamics_train(reps = 4)
  tr2 <- make_state_dynamics_train(reps = 4)
  tr2$pattern_auto[tr2$pattern_auto == "pause"] <- "tonic"
  trains <- list(STN_1 = tr1, GPe_1 = tr2)
  metadata <- data.frame(
    train = c("STN_1", "GPe_1"),
    subject = c("S1", "S2"),
    nucleus = c("STN", "GPe"),
    stringsAsFactors = FALSE
  )

  td <- stpd_build_transition_model_data(trains, metadata = metadata,
                                         label_source = "auto",
                                         min_isi_sec = 0.0009)
  expect_true(all(c("train", "from", "to", "n", "nucleus") %in% names(td)))
  expect_gt(nrow(td), 0)

  fit <- expect_warning(
    stpd_fit_transition_statistical_model(td, fixed_effects = c("from", "nucleus"),
                                          method = "one_vs_rest_glm"),
    NA
  )
  expect_equal(fit$method, "one_vs_rest_glm")
  expect_gt(length(fit$fits), 0)
  expect_true("warnings" %in% names(fit))
})

test_that("orchestrator returns core, exploration, and model bundles", {
  dat <- make_state_dynamics_train(reps = 4)
  res <- stpd_analyze_state_dynamics(dat, train = "synthetic", label_source = "auto",
                                     min_isi_sec = 0.0009, n_surrogates = 3,
                                     run_exploration = TRUE, run_models = TRUE,
                                     seed = 5)
  expect_true(all(c("features", "phase_portrait", "pca", "transition",
                    "dwell_times", "transition_entropy", "motif_frequency") %in% names(res$core)))
  expect_true(is.list(res$exploration))
  expect_false(inherits(res$exploration$phate, "error"))
  expect_true(res$exploration$phate$diagnostics$method %in% c("phateR", "diffusion_potential_mds"))
  expect_true(is.list(res$models))
})
