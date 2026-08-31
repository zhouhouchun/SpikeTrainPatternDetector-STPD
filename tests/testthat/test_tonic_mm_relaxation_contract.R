make_valid_tonic_mm_relaxation_contract <- function(
    relaxed_max = 1.50, relax_cv_max = 0.30, relax_lv_max = 0.15) {
  list(
    enabled = TRUE,
    relaxed_max = relaxed_max,
    evidence_n = 10L,
    group_n = 5L,
    q95_full = 1.47,
    logo_q95_min = 1.45,
    logo_q95_median = 1.46,
    logo_q95_max = 1.47,
    relax_cv_max = relax_cv_max,
    relax_lv_max = relax_lv_max,
    default_relaxed_max = 1.40,
    maximum_relaxed_max = 1.50,
    safety_multiplier = 1.02,
    mode = "calibration_group_logo_q95_v1",
    status = "enabled__calibration_frozen_group_logo_q95"
  )
}

test_that("Tonic MM relaxation is conditional and bounded", {
  gate <- getFromNamespace("stpd_tonic_mm_gate", "SpikeTrainPatternDetector")
  vp <- list(
    tonic_mm_max = 1.25,
    tonic_mm_relax_lv_max = 0.15,
    tonic_mm_relax_cv_max = 0.30,
    tonic_mm_relaxed_max = 1.50,
    tonic_lv_max = 0.50,
    tonic_mm_relaxation_contract =
      make_valid_tonic_mm_relaxation_contract(),
    tonic_mm_relaxation_mode = "calibration_group_logo_q95_v1",
    tonic_mm_relaxation_status =
      "enabled__calibration_frozen_group_logo_q95"
  )

  strong <- gate(vp, cv = 0.24, lv = 0.10)
  expect_true(strong$applied)
  expect_equal(strong$base_max, 1.25)
  expect_equal(strong$effective_max, 1.50)
  expect_true(strong$contract_required)
  expect_true(strong$contract_valid)
  expect_false(strong$fallback_applied)

  high_cv <- gate(vp, cv = 0.31, lv = 0.10)
  high_lv <- gate(vp, cv = 0.24, lv = 0.16)
  expect_false(high_cv$applied)
  expect_false(high_lv$applied)
  expect_equal(high_cv$effective_max, 1.25)
  expect_equal(high_lv$effective_max, 1.25)
})

test_that("default MM relaxation preserves the historical 1.40 behavior", {
  params <- stpd_productize_params(default_params(), prefer = "canonical")
  expect_equal(params$spiketrainpattern$tonic$mm_max, 1.25)
  expect_equal(params$spiketrainpattern$tonic$mm_relaxed_max, 1.40)
  expect_equal(params$tonic$tonic_mm_relaxed_max, 1.40)
  expect_equal(
    params$spiketrainpattern$multitrack_shadow$
      tonic_fragment_mm_relaxed_max,
    1.40
  )
  vp <- stpd_event_core_params(data.frame(), params)
  gate <- getFromNamespace(
    "stpd_tonic_mm_gate", "SpikeTrainPatternDetector"
  )(vp, cv = 0.20, lv = 0.10)
  expect_false(gate$contract_required)
  expect_true(gate$contract_valid)
  expect_equal(gate$effective_max, 1.40)
})

test_that("calibration episode and group evidence can freeze 1.50", {
  estimate <- getFromNamespace(
    "stpd_manual_tonic_mm_relaxation_estimates",
    "SpikeTrainPatternDetector"
  )
  fixture <- data.frame(
    CV = rep(0.20, 10L),
    LV = rep(0.10, 10L),
    MM = c(1.36, 1.39, 1.41, 1.43, 1.45,
           1.46, 1.47, 1.48, 1.49, 1.496),
    group = rep(paste0("G", 1:5), each = 2L),
    stringsAsFactors = FALSE
  )
  out <- estimate(fixture, group = fixture$group)
  expect_true(out$enabled)
  expect_equal(out$evidence_n, 10L)
  expect_equal(out$group_n, 5L)
  expect_equal(out$relaxed_max, 1.50)
  expect_lte(out$relaxed_max, out$maximum_relaxed_max)
  validated <- getFromNamespace(
    "stpd_tonic_mm_relaxation_contract",
    "SpikeTrainPatternDetector"
  )(list(spiketrainpattern = list(tonic = list(
    mm_relax_lv_max = out$relax_lv_max,
    mm_relax_cv_max = out$relax_cv_max,
    mm_relaxed_max = out$relaxed_max,
    mm_relaxation_contract = out
  ))))
  expect_true(validated$required)
  expect_true(validated$valid)
  expect_equal(validated$recomputed_relaxed_max, out$relaxed_max)

  too_few <- estimate(fixture[1:9, ], group = fixture$group[1:9])
  too_few_groups <- estimate(
    transform(fixture, group = rep(c("A", "B", "C"), length.out = 10L)),
    group = rep(c("A", "B", "C"), length.out = 10L)
  )
  expect_false(too_few$enabled)
  expect_false(too_few_groups$enabled)
  expect_equal(too_few$relaxed_max, 1.40)
  expect_equal(too_few_groups$relaxed_max, 1.40)
})

test_that("canonical MM relaxation values own legacy and shadow mirrors", {
  canonical_contract <- make_valid_tonic_mm_relaxation_contract()
  canonical <- list(
    spiketrainpattern = list(
      tonic = list(
        mm_relax_lv_max = 0.14,
        mm_relax_cv_max = 0.28,
        mm_relaxed_max = 1.50,
        mm_relaxation_contract = within(
          canonical_contract,
          {
            relax_lv_max <- 0.14
            relax_cv_max <- 0.28
          }
        )
      ),
      multitrack_shadow = list(
        tonic_fragment_mm_relax_lv_max = 0.15,
        tonic_fragment_mm_relax_cv_max = 0.30,
        tonic_fragment_mm_relaxed_max = 1.40
      )
    ),
    tonic = list(
      tonic_mm_relax_lv_max = 0.13,
      tonic_mm_relax_cv_max = 0.27,
      tonic_mm_relaxed_max = 1.45
    )
  )
  resolved <- stpd_productize_params(canonical, prefer = "canonical")
  expect_equal(resolved$tonic$tonic_mm_relax_lv_max, 0.14)
  expect_equal(resolved$tonic$tonic_mm_relax_cv_max, 0.28)
  expect_equal(resolved$tonic$tonic_mm_relaxed_max, 1.50)
  expect_identical(
    resolved$tonic$mm_relaxation_contract,
    resolved$spiketrainpattern$tonic$mm_relaxation_contract
  )
  vp <- stpd_event_core_params(data.frame(), resolved)
  expect_identical(
    vp$tonic_mm_relaxation_mode,
    "calibration_group_logo_q95_v1"
  )
  expect_identical(
    vp$tonic_mm_relaxation_status,
    "enabled__calibration_frozen_group_logo_q95"
  )
  expect_equal(
    resolved$spiketrainpattern$multitrack_shadow$
      tonic_fragment_mm_relaxed_max,
    1.50
  )

  shadow_only <- list(spiketrainpattern = list(multitrack_shadow = list(
    tonic_fragment_mm_relax_lv_max = 0.12,
    tonic_fragment_mm_relax_cv_max = 0.25,
    tonic_fragment_mm_relaxed_max = 1.48
  )))
  migrated <- stpd_productize_params(shadow_only, prefer = "canonical")
  expect_equal(migrated$spiketrainpattern$tonic$mm_relax_lv_max, 0.12)
  expect_equal(migrated$spiketrainpattern$tonic$mm_relax_cv_max, 0.25)
  expect_equal(migrated$spiketrainpattern$tonic$mm_relaxed_max, 1.48)
})

test_that("Tonic MM relation contract rejects incoherent ceilings", {
  params <- default_params_sec()
  params$spiketrainpattern$tonic$mm_min <- 1.30
  params$spiketrainpattern$tonic$mm_max <- 1.25
  params$spiketrainpattern$tonic$mm_relaxed_max <- 1.20

  issues <- stpd_validate_param_relations(params)
  expect_true(all(c(
    "tonic_mm_min_above_max",
    "tonic_mm_max_above_relaxed_max"
  ) %in% issues$issue_code))
})

test_that("MM relaxation above 1.40 fails closed without valid evidence", {
  gate_fun <- getFromNamespace(
    "stpd_tonic_mm_gate", "SpikeTrainPatternDetector"
  )
  vp <- list(
    tonic_mm_max = 1.25,
    tonic_lv_max = 0.50,
    tonic_mm_relax_lv_max = 0.15,
    tonic_mm_relax_cv_max = 0.30,
    tonic_mm_relaxed_max = 1.50
  )
  missing <- gate_fun(vp, cv = 0.20, lv = 0.10)
  expect_true(missing$contract_required)
  expect_false(missing$contract_valid)
  expect_true(missing$fallback_applied)
  expect_equal(missing$configured_relaxed_max, 1.50)
  expect_equal(missing$relaxed_max, 1.40)
  expect_equal(missing$effective_max, 1.40)
  expect_match(missing$contract_reason, "calibration_contract_not_enabled")

  malformed <- vp
  malformed$tonic_mm_relaxation_contract <-
    make_valid_tonic_mm_relaxation_contract()
  malformed$tonic_mm_relaxation_contract$logo_q95_median <- 1.10
  malformed_gate <- gate_fun(malformed, cv = 0.20, lv = 0.10)
  expect_false(malformed_gate$contract_valid)
  expect_true(malformed_gate$fallback_applied)
  expect_equal(malformed_gate$effective_max, 1.40)
  expect_match(
    malformed_gate$contract_reason,
    "invalid_logo_q95_order|contract_value_differs_from_recomputed_value"
  )

  params <- default_params_sec()
  params$spiketrainpattern$tonic$mm_relaxed_max <- 1.50
  params$spiketrainpattern$tonic$mm_relaxation_contract <- NULL
  params$tonic$mm_relaxation_contract <- NULL
  issues <- stpd_validate_params(params)
  expect_true(any(
    issues$severity == "error" &
      issues$path == "spiketrainpattern.tonic.mm_relaxed_max" &
      grepl("invalid calibration-frozen contract", issues$issue)
  ))
})

test_that("MM strong-regularity guards cannot be loosened at runtime", {
  gate_fun <- getFromNamespace(
    "stpd_tonic_mm_gate", "SpikeTrainPatternDetector"
  )
  vp <- list(
    tonic_mm_max = 1.25,
    tonic_lv_max = 0.50,
    tonic_mm_relax_lv_max = 0.50,
    tonic_mm_relax_cv_max = 0.50,
    tonic_mm_relaxed_max = 1.40
  )
  gate <- gate_fun(vp, cv = 0.45, lv = 0.45)
  expect_equal(gate$lv_limit, 0.15)
  expect_equal(gate$cv_limit, 0.30)
  expect_false(gate$applied)
  expect_equal(gate$effective_max, 1.25)

  params <- stpd_productize_params(default_params_sec(), prefer = "canonical")
  params$spiketrainpattern$tonic$mm_relax_lv_max <- 0.50
  params$spiketrainpattern$tonic$mm_relax_cv_max <- 0.50
  params <- stpd_productize_params(params, prefer = "canonical")
  issues <- stpd_validate_params(params)
  expect_true(any(
    issues$severity == "error" &
      grepl("mm_relax_lv_max", issues$path, fixed = TRUE)
  ))
  expect_true(any(
    issues$severity == "error" &
      grepl("mm_relax_cv_max", issues$path, fixed = TRUE)
  ))
})

test_that("MM and LV parameter units remain dimensionless ratios", {
  contract <- stpd_parameter_contract()
  expected <- c(
    "tonic.tonic_mm_max",
    "spiketrainpattern.tonic.mm_max",
    "spiketrainpattern.tonic.lv_max"
  )
  rows <- contract[match(expected, contract$path), , drop = FALSE]
  expect_false(anyNA(rows$path))
  expect_true(all(rows$unit == "ratio"))
})
