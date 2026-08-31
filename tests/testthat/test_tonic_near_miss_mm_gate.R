make_tonic_mm_near_miss_train <- function(values) {
  timestamps <- cumsum(c(0, values))
  data.frame(
    idx = seq_along(timestamps),
    timestamp_sec = timestamps,
    ISI_sec = c(NA_real_, values),
    pattern_manual = rep("", length(timestamps)),
    pattern_auto = rep("", length(timestamps)),
    stringsAsFactors = FALSE
  )
}

tonic_mm_near_miss_params <- function() {
  list(
    G_min = 6L,
    T_min = 0.001,
    T_max = 1,
    LV_core = 0.50,
    seed_ratio = 10,
    tonic_mm_min = 0.50,
    tonic_mm_max = 1.25,
    tonic_mm_relax_lv_max = 0.15,
    tonic_mm_relax_cv_max = 0.30,
    tonic_mm_relaxed_max = 1.40,
    mm_relaxation_contract = list(
      mode = "test_frozen_contract",
      status = "test_only"
    )
  )
}

test_that("Tonic near-miss uses the same conditional MM gate as detection", {
  dat <- make_tonic_mm_near_miss_train(c(0.04, 0.04, 0.04, 0.04, 0.06))
  params <- tonic_mm_near_miss_params()

  values <- dat$ISI_sec[-1L]
  gate <- getFromNamespace(
    "stpd_tonic_near_miss_mm_gate",
    "SpikeTrainPatternDetector"
  )(params, cv = calc_CV(values), lv = calc_LV(values))
  expect_true(gate$applied)
  expect_gt(max(values) / mean(values), gate$base_max)
  expect_lte(max(values) / mean(values), gate$effective_max)

  out <- getFromNamespace(
    "mine_tonic_near_miss_train",
    "SpikeTrainPatternDetector"
  )(dat, params, train = "train_1", max_relax = 1)
  expect_equal(nrow(out), 0L)
})

test_that("Tonic near-miss preserves a valid frozen MM contract", {
  params <- tonic_mm_near_miss_params()
  params$tonic_mm_relaxed_max <- 1.50
  params$mm_relaxation_contract <- list(
    enabled = TRUE,
    relaxed_max = 1.50,
    evidence_n = 10L,
    group_n = 5L,
    q95_full = 1.47,
    logo_q95_min = 1.45,
    logo_q95_median = 1.46,
    logo_q95_max = 1.47,
    relax_cv_max = 0.30,
    relax_lv_max = 0.15,
    default_relaxed_max = 1.40,
    maximum_relaxed_max = 1.50,
    safety_multiplier = 1.02,
    mode = "calibration_group_logo_q95_v1",
    status = "enabled__calibration_frozen_group_logo_q95"
  )
  gate <- getFromNamespace(
    "stpd_tonic_near_miss_mm_gate", "SpikeTrainPatternDetector"
  )(params, cv = 0.20, lv = 0.10)
  expect_true(gate$contract_required)
  expect_true(gate$contract_valid)
  expect_false(gate$fallback_applied)
  expect_equal(gate$effective_max, 1.50)
  expect_identical(
    gate$mode, "calibration_group_logo_q95_v1"
  )
  expect_identical(
    gate$status, "enabled__calibration_frozen_group_logo_q95"
  )
})

test_that("Tonic near-miss identifies the CV guard that blocks MM relaxation", {
  dat <- make_tonic_mm_near_miss_train(c(0.025, 0.030, 0.040, 0.050, 0.060))
  params <- tonic_mm_near_miss_params()
  params$tonic_mm_relaxed_max <- 1.50

  out <- getFromNamespace(
    "mine_tonic_near_miss_train",
    "SpikeTrainPatternDetector"
  )(dat, params, train = "train_cv", max_relax = 1)

  expect_true(any(out$parameter == "tonic_mm_relax_cv_max"))
  expect_false(any(out$parameter == "tonic_mm_max"))
  cv_row <- out[out$parameter == "tonic_mm_relax_cv_max", , drop = FALSE]
  expect_true(all(cv_row$required_value > cv_row$current_value))
  expect_true(all(grepl("MM base/effective/relaxed", cv_row$details, fixed = TRUE)))
})

test_that("Tonic near-miss identifies the LV guard independently", {
  dat <- make_tonic_mm_near_miss_train(c(0.030, 0.060, 0.030, 0.060, 0.061))
  params <- tonic_mm_near_miss_params()
  params$tonic_mm_relax_lv_max <- 0.10
  params$tonic_mm_relax_cv_max <- 0.40
  params$tonic_mm_relaxed_max <- 1.50

  out <- getFromNamespace(
    "mine_tonic_near_miss_train",
    "SpikeTrainPatternDetector"
  )(dat, params, train = "train_lv", max_relax = 2)

  expect_true(any(out$parameter == "tonic_mm_relax_lv_max"))
  expect_false(any(out$parameter == "tonic_mm_max"))
  lv_row <- out[out$parameter == "tonic_mm_relax_lv_max", , drop = FALSE]
  expect_true(all(lv_row$required_value > lv_row$current_value))
})

test_that("strong-regular Tonic exposes the conditional relaxed MM ceiling", {
  dat <- make_tonic_mm_near_miss_train(c(0.04, 0.04, 0.04, 0.04, 0.065))
  params <- tonic_mm_near_miss_params()

  out <- getFromNamespace(
    "mine_tonic_near_miss_train",
    "SpikeTrainPatternDetector"
  )(dat, params, train = "train_relaxed_ceiling", max_relax = 1)

  expect_true(any(out$parameter == "tonic_mm_relaxed_max"))
  expect_false(any(out$parameter == "tonic_mm_max"))
  relaxed <- out[out$parameter == "tonic_mm_relaxed_max", , drop = FALSE]
  expect_true(all(relaxed$required_value > relaxed$current_value))
  expect_true(all(relaxed$required_value <= 1.50))
})

test_that("Tonic near-miss never proposes a relaxed MM ceiling above 1.50", {
  dat <- make_tonic_mm_near_miss_train(c(0.04, 0.04, 0.04, 0.04, 0.08))
  params <- tonic_mm_near_miss_params()
  params$tonic_mm_relax_lv_max <- 1
  params$tonic_mm_relax_cv_max <- 1
  params$tonic_mm_relaxed_max <- 1.50

  out <- getFromNamespace(
    "mine_tonic_near_miss_train",
    "SpikeTrainPatternDetector"
  )(dat, params, train = "train_cap", max_relax = 2)
  relaxed <- out[out$parameter == "tonic_mm_relaxed_max", , drop = FALSE]
  expect_true(nrow(relaxed) == 0L || all(relaxed$required_value <= 1.50))
})

test_that("server maps all conditional Tonic MM near-miss parameters", {
  server_text <- paste(
    deparse(body(getFromNamespace("server", "SpikeTrainPatternDetector")),
            width.cutoff = 500L),
    collapse = "\n"
  )
  expect_match(
    server_text,
    'tonic_mm_relax_lv_max = "tonic.tonic_mm_relax_lv_max"',
    fixed = TRUE
  )
  expect_match(
    server_text,
    'tonic_mm_relax_cv_max = "tonic.tonic_mm_relax_cv_max"',
    fixed = TRUE
  )
  expect_match(
    server_text,
    'tonic_mm_relaxed_max = "tonic.tonic_mm_relaxed_max"',
    fixed = TRUE
  )
})
