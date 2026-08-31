test_that("public event-family parameters mirror into the active runtime", {
  p <- default_params()

  p$spiketrainpattern$high_frequency_spiking$min_spikes <- 47L
  p$spiketrainpattern$high_frequency_spiking$min_duration_sec <- 0.42
  p$spiketrainpattern$high_frequency_spiking$tolerated_gap_isi_sec <- 0.091
  p$spiketrainpattern$high_frequency_spiking$allowed_large_isi_fraction <- 0.37
  p$spiketrainpattern$high_frequency_spiking$max_consecutive_large_isi <- 5L

  p$spiketrainpattern$high_frequency_tonic$min_spikes <- 9L
  p$spiketrainpattern$high_frequency_tonic$min_isi_floor_sec <- 0.012
  p$spiketrainpattern$high_frequency_tonic$low_tail_fraction_max <- 0.13
  p$spiketrainpattern$high_frequency_tonic$veto_burst_core_run <- FALSE

  p$spiketrainpattern$tonic$min_isi_sec <- 0.024
  p$spiketrainpattern$tonic$max_isi_sec <- 0.057
  p$spiketrainpattern$tonic$bridge_upper_sec <- 0.081
  p$spiketrainpattern$tonic$min_spikes <- 8L
  p$spiketrainpattern$tonic$lv_max <- 0.44
  p$spiketrainpattern$tonic$mm_max <- 1.18

  eff <- stpd_productize_params(p, prefer = "canonical")

  expect_equal(eff$highfreq$spiking_min_spikes, 47L)
  expect_equal(eff$highfreq$spiking_min_duration, 0.42)
  expect_equal(eff$highfreq$spiking_tolerated_gap_ISI_sec, 0.091)
  expect_equal(eff$highfreq$spiking_allowed_large_isi_fraction, 0.37)
  expect_equal(eff$highfreq$spiking_max_consecutive_large_isi, 5L)
  expect_equal(eff$highfreq$G_min, 9L)
  expect_equal(eff$highfreq$tonic_min_ISI_floor_sec, 0.012)
  expect_equal(eff$highfreq$tonic_low_tail_fraction_max, 0.13)
  expect_false(eff$highfreq$tonic_burst_core_veto)
  expect_equal(eff$tonic$T_min, 0.024)
  expect_equal(eff$tonic$T_max, 0.057)
  expect_equal(eff$tonic$bridge_upper_sec, 0.081)
  expect_equal(eff$tonic$G_min, 8L)
  expect_equal(eff$tonic$LV_core, 0.44)
  expect_equal(eff$tonic$tonic_mm_max, 1.18)

  dat <- stpd_golden_test_dataset("stable_high_frequency")$trains$train_1
  runtime <- stpd_event_core_params(
    dat,
    eff,
    min_isi_sec = eff$detector$min_valid_isi_sec
  )
  expect_equal(runtime$hf_spiking_min_spikes, 47L)
  expect_equal(runtime$hf_spiking_min_duration, 0.42)
  expect_equal(runtime$hf_spiking_allowed_large_frac, 0.37)
  expect_equal(runtime$hf_spiking_max_consec_large, 5L)
  expect_equal(runtime$hf_tonic_min_spikes, 9L)
  expect_equal(runtime$hf_tonic_floor, 0.012)
  expect_equal(runtime$hf_tonic_low_tail_max, 0.13)
  expect_false(runtime$hf_tonic_burst_core_veto)
  expect_equal(runtime$tonic_min, 0.024)
  expect_equal(runtime$tonic_max, 0.057)
  expect_equal(runtime$tonic_min_spikes, 8L)
  expect_equal(runtime$tonic_lv_max, 0.44)
  expect_equal(runtime$tonic_mm_max, 1.18)

  suggested <- SpikeTrainPatternDetector:::stpd_event_grammar_default_suggest(eff)
  expect_equal(suggested$tonic$bridge_upper_sec, 0.081)

  eff$highfreq$tonic_bridge_upper_sec <- 0.043
  eff$pause$bridge_upper_sec <- 0.177
  suggested <- SpikeTrainPatternDetector:::stpd_event_grammar_default_suggest(eff)
  expect_equal(suggested$high_frequency_tonic$bridge_upper_sec, 0.043)
  expect_equal(suggested$pause$bridge_upper_sec, 0.177)

  resolved <- eff
  resolved$event_grammar$effective_bands <- list(
    high_frequency_tonic = list(
      seed_lower_sec = 0.012, seed_upper_sec = 0.027,
      bridge_upper_sec = 0.043, contrast_S = NA_real_
    ),
    tonic = list(
      seed_lower_sec = 0.024, seed_upper_sec = 0.057,
      bridge_upper_sec = 0.081, contrast_S = NA_real_
    )
  )
  resolved_runtime <- stpd_event_grammar_params(
    dat, resolved, min_isi_sec = eff$detector$min_valid_isi_sec
  )
  expect_equal(resolved_runtime$hf_tonic_bridge_upper, 0.043)
  expect_equal(resolved_runtime$tonic_bridge_upper, 0.081)
})

test_that("tonic bridge upper changes state-run assembly", {
  isi <- c(0.1, rep(0.05, 5L), 0.07, rep(0.05, 5L), 0.1)
  times <- cumsum(c(0, isi))
  dat <- data.frame(
    idx = seq_along(times), timestamp_sec = times,
    ISI_sec = c(NA_real_, diff(times)),
    pattern_manual = rep("", length(times)),
    pattern_manual_negative = rep("", length(times)),
    pattern_auto = rep("", length(times)),
    stringsAsFactors = FALSE
  )
  params <- effective_params_for_detector(default_params())
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(dat, params, min_isi_sec = 0.0009)
  vp$tonic_min <- 0.04
  vp$tonic_max <- 0.06
  vp$tonic_min_spikes <- 5L
  vp$tonic_connector_max_n <- 1L
  detect_tonic <- SpikeTrainPatternDetector:::stpd_event_core_detect_tonic

  vp$tonic_bridge_upper <- 0.065
  without_bridge <- detect_tonic(dat, params, vp, min_isi_sec = 0.0009, train = "tonic_bridge")
  vp$tonic_bridge_upper <- 0.075
  with_bridge <- detect_tonic(dat, params, vp, min_isi_sec = 0.0009, train = "tonic_bridge")

  expect_gt(nrow(without_bridge), 0L)
  expect_gt(nrow(with_bridge), 0L)
  expect_false(any(without_bridge$tonic_bridge_isi_n > 0L))
  expect_true(any(with_bridge$tonic_bridge_isi_n == 1L))
  expect_true(any(with_bridge$tonic_candidate_source == "bridge_merged"))
  expect_gt(
    max(with_bridge$end_isi - with_bridge$start_isi),
    max(without_bridge$end_isi - without_bridge$start_isi)
  )
})

test_that("the public parameter report agrees with mirrored runtime values", {
  p <- default_params()
  p$spiketrainpattern$high_frequency_spiking$min_spikes <- 53L
  p$spiketrainpattern$high_frequency_spiking$min_duration_sec <- 0.31
  p$spiketrainpattern$high_frequency_spiking$allowed_large_isi_fraction <- 0.29
  p$spiketrainpattern$high_frequency_tonic$low_tail_fraction_max <- 0.11
  p$spiketrainpattern$high_frequency_tonic$veto_burst_core_run <- FALSE
  p$spiketrainpattern$tonic$min_spikes <- 7L
  p$spiketrainpattern$tonic$lv_max <- 0.41
  p$spiketrainpattern$tonic$mm_max <- 1.16

  eff <- stpd_productize_params(p, prefer = "canonical")
  report <- stpd_public_parameter_table(eff)
  reported <- function(section, parameter) {
    report$value[report$section == section & report$parameter == parameter][1]
  }

  expect_equal(as.integer(reported("HF spiking", "min_spikes")), eff$highfreq$spiking_min_spikes)
  expect_equal(as.numeric(reported("HF spiking", "min_duration")), eff$highfreq$spiking_min_duration)
  expect_equal(as.numeric(reported("HF spiking", "allowed_large_isi_fraction")), eff$highfreq$spiking_allowed_large_isi_fraction)
  expect_equal(as.numeric(reported("HF tonic", "low_tail_fraction_max")), eff$highfreq$tonic_low_tail_fraction_max)
  expect_identical(as.logical(reported("HF tonic", "veto_burst_core_run")), eff$highfreq$tonic_burst_core_veto)
  expect_equal(as.integer(reported("Tonic", "min_spikes")), eff$tonic$G_min)
  expect_equal(as.numeric(reported("Tonic", "lv_max")), eff$tonic$LV_core)
  expect_equal(as.numeric(reported("Tonic", "mm_max")), eff$tonic$tonic_mm_max)
})

test_that("the parameter hash covers normalized full effective parameters", {
  p <- default_params()
  base_hash <- stpd_params_hash(p)

  expect_match(base_hash, "^[0-9a-f]{64}$")

  structural <- p
  structural$event_grammar$structural_burst_rescue_compression_min <- 999
  expect_false(identical(stpd_params_hash(structural), base_hash))

  public_change <- p
  public_change$spiketrainpattern$high_frequency_spiking$min_spikes <- 999L
  expect_false(identical(stpd_params_hash(public_change), base_hash))

  prepared <- p
  prepared$meta <- list(params_hash = "stale-recursive-value", run_id = "runtime-only")
  expect_identical(stpd_params_hash(prepared), base_hash)

  reordered <- p[rev(names(p))]
  expect_identical(stpd_params_hash(reordered), base_hash)

  timed_a <- p
  timed_b <- p
  timed_a$event_grammar$effective_bands <- list(updated_at = "2026-08-10T12:00:00Z")
  timed_b$event_grammar$effective_bands <- list(updated_at = "2026-08-10T13:00:00Z")
  expect_identical(stpd_params_hash(timed_a), stpd_params_hash(timed_b))
})

test_that("validation range stripping covers every detector family and dataset setting", {
  p <- default_params()
  p$burst$adaptive_train_ranges <- list(train_1 = list(low = 1))
  p$tonic$adaptive_train_ranges <- list(train_1 = list(low = 1))
  p$pause$adaptive_train_ranges <- list(train_1 = list(low = 1))
  p$highfreq$adaptive_train_ranges <- list(train_1 = list(low = 1))
  p$detector$train_isi_thresholds <- list(train_1 = list(burst_max_sec = 0.02))
  p$event_grammar$effective_bands <- list(burst = list(seed_upper_sec = 0.01))
  stripped <- SpikeTrainPatternDetector:::strip_learned_ranges_for_eval(p)

  expect_length(stripped$burst$adaptive_train_ranges, 0L)
  expect_length(stripped$tonic$adaptive_train_ranges, 0L)
  expect_length(stripped$pause$adaptive_train_ranges, 0L)
  expect_length(stripped$highfreq$adaptive_train_ranges, 0L)
  expect_length(stripped$detector$train_isi_thresholds, 0L)
  expect_null(stripped$event_grammar$effective_bands)

  ds <- SpikeTrainPatternDetector:::stpd_golden_test_dataset("middle_burst")
  ds$train_settings <- list(
    burst_isi_ranges = list(train_1 = list(low = 1)),
    tonic_isi_ranges = list(train_1 = list(low = 1)),
    pause_isi_ranges = list(train_1 = list(low = 1)),
    highfreq_isi_ranges = list(train_1 = list(low = 1)),
    isi_thresholds = list(train_1 = list(burst_max_sec = 0.02))
  )
  ds_stripped <- SpikeTrainPatternDetector:::stpd_strip_learned_dataset_settings_for_eval(ds)
  expect_true(all(vapply(ds_stripped$train_settings, length, integer(1)) == 0L))
})
