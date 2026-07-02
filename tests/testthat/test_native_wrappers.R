
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
