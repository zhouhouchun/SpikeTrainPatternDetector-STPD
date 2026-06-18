
test_that("native interval overlap wrapper works", {
  res <- interval_best_overlap(c(1, 10), c(5, 12), c(3, 20), c(6, 25))
  expect_equal(nrow(res), 2)
  expect_equal(res$best_index[1], 1)
  expect_true(res$overlap[1] > 0)
})

test_that("structure scan wrapper returns a data frame", {
  isi <- c(NA, .10, .01, .012, .011, .10, .20)
  pct <- compute_isi_percentiles(isi, min_isi_sec=.001)
  res <- scan_structure_candidates(isi, pct, min_core_isi_n=2, max_core_isi_n=3, core_q90_max_sec=.02, edge_min=2, edge_geom=2)
  expect_true(is.data.frame(res))
})

test_that("native wrappers bound oversized local and structure windows", {
  isi <- c(NA, rep(c(.10, .01, .012, .011), 5), .20)
  local <- compute_local_median_cache(isi, window = 1e9)
  expect_equal(length(local), length(isi))

  pct <- compute_isi_percentiles(isi, min_isi_sec = .001)
  short_res <- scan_structure_candidates(
    isi,
    pct,
    min_core_isi_n = 2,
    max_core_isi_n = 1e9,
    core_q90_max_sec = .02,
    edge_min = 1,
    edge_geom = 1
  )
  expect_true(is.data.frame(short_res))

  long_isi <- rep(.01, 10005)
  expect_error(
    scan_structure_candidates(long_isi, min_core_isi_n = 2, max_core_isi_n = 10003),
    "too large"
  )
})

test_that("structure scan uses R type-7 q90 and includes the penultimate valid core", {
  isi <- c(NA, .10, .010, .011, .012, .013, .10)
  pct <- compute_isi_percentiles(isi, min_isi_sec = .001)
  res <- scan_structure_candidates(
    isi,
    pct,
    min_core_isi_n = 4,
    max_core_isi_n = 4,
    core_q90_max_sec = .02,
    core_pct_max = Inf,
    edge_min = 1,
    edge_geom = 1
  )
  row <- res[res$start_isi == 3L & res$end_isi == 6L, , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(
    row$core_q90_ISI_sec,
    as.numeric(stats::quantile(isi[3:6], 0.90, na.rm = TRUE, names = FALSE, type = 7)),
    tolerance = 1e-12
  )
})

test_that("native prefilters treat Inf thresholds as open upper bounds", {
  isi <- c(NA, .10, .010, .011, .012, .013, .10)
  pct <- compute_isi_percentiles(isi, min_isi_sec = .001)

  structure_res <- scan_structure_candidates(
    isi,
    pct,
    min_core_isi_n = 4,
    max_core_isi_n = 4,
    core_q90_max_sec = .005,
    core_pct_max = Inf,
    edge_min = 1,
    edge_geom = 1
  )
  expect_equal(nrow(structure_res[structure_res$start_isi == 3L & structure_res$end_isi == 6L, , drop = FALSE]), 1L)

  run_res <- scan_short_isi_runs(c(.010, .020, .500), min_run_isi_n = 1)
  expect_equal(nrow(run_res), 1L)
  expect_equal(run_res$start_isi, 1L)
  expect_equal(run_res$end_isi, 3L)
})

test_that("native wrappers reject mismatched paired vector lengths", {
  isi <- c(NA, .10, .01, .012, .011, .10, .20)
  expect_error(
    scan_structure_candidates(isi, isi_pct = c(10, 20), min_core_isi_n = 2, max_core_isi_n = 3),
    "same length"
  )
  expect_error(
    scan_short_isi_runs(isi, isi_pct = c(10, 20), max_abs_sec = .02),
    "same length"
  )
  expect_error(
    interval_best_overlap(c(1L, 10L), c(5L), c(3L), c(6L)),
    "query_start and query_end"
  )
  expect_error(
    interval_best_overlap(c(1L), c(5L), c(3L, 20L), c(6L)),
    "target_start and target_end"
  )
})

test_that("native local-median cache matches an independent R reference", {
  # Regression for the native stpd_local_median_cache_c segfault/divergence:
  # the C result must equal a from-scratch R median (excluding the first ISI and
  # the focal index, finite and >= min_isi). A crash aborts the process; a wrong
  # value fails the comparison.
  ref_local_median <- function(isi, window, min_isi = 0.001) {
    if (window %% 2 == 0) window <- window + 1
    n <- length(isi); half <- window %/% 2; out <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      idx <- setdiff(max(2L, i - half):min(n, i + half), i)
      v <- isi[idx]; v <- v[is.finite(v) & v >= min_isi]
      if (length(v)) out[i] <- stats::median(v)
    }
    out
  }
  inputs <- list(
    c(NA, rep(c(.01, .02, .03, .2, .005, .006, .15, .02), 4)),
    c(NA, rep(c(.002, .2), 30)),
    c(NA, rep(.005, 40))
  )
  path <- system.file("extdata", "Grechishnikova_STN_2017_subset.csv", package = "SpikeTrainPatternDetector")
  if (file.exists(path)) {
    ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
    inputs <- c(inputs, lapply(ds$trains, function(t) as.numeric(t$ISI_sec)))
  }
  for (isi in inputs) {
    for (w in c(3L, 5L, 11L)) {  # all <= length, so the wrapper does not shrink them
      got <- compute_local_median_cache(isi, window = w, min_isi_sec = 0.001)
      expect_equal(length(got), length(isi))
      expect_equal(got, ref_local_median(isi, w), tolerance = 1e-9)
    }
  }
})

test_that("native local-median cache handles degenerate sizes without crashing", {
  for (isi in list(numeric(0), NA_real_, c(NA, .01), c(NA, .01, .02))) {
    res <- compute_local_median_cache(as.numeric(isi), window = 11L)
    expect_equal(length(res), length(isi))
  }
})
