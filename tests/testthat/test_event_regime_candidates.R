event_regime_fixture <- function(
    scale = 1, event_spans = list(), gap_specs = list(),
    state_spans = list(), n = 70L) {
  base <- 0.01 * scale
  events <- if (length(event_spans)) {
    dplyr::bind_rows(lapply(seq_along(event_spans), function(i) {
      span <- as.integer(event_spans[[i]])
      data.frame(
        train = "train_1", event_id = paste0("event_", i),
        event_family = "burst", start_isi = span[1], end_isi = span[2],
        start_time_sec = span[1] * base, end_time_sec = span[2] * base,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      train = character(), event_id = character(), event_family = character(),
      start_isi = integer(), end_isi = integer(), start_time_sec = double(),
      end_time_sec = double(), stringsAsFactors = FALSE
    )
  }
  gaps <- if (length(gap_specs)) {
    dplyr::bind_rows(lapply(seq_along(gap_specs), function(i) {
      spec <- gap_specs[[i]]
      span <- as.integer(spec$span)
      is_long <- isTRUE(spec$long)
      data.frame(
        train = "train_1", gap_id = paste0("gap_", i), gap_class = "pause",
        gap_semantics = if (is_long) {
          "canonical_pause"
        } else {
          "contextual_interburst_pause"
        },
        candidate_decision = spec$decision %||% "accepted",
        candidate_reason_code = if (is_long) {
          "high_specificity_absolute_and_relative_long_isi_anchor"
        } else {
          "two_independent_burst_cores_with_non_bridge_gap"
        },
        start_isi = span[1], end_isi = span[2],
        start_time_sec = span[1] * base, end_time_sec = span[2] * base,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      train = character(), gap_id = character(), gap_class = character(),
      gap_semantics = character(), candidate_decision = character(),
      candidate_reason_code = character(), start_isi = integer(),
      end_isi = integer(), start_time_sec = double(), end_time_sec = double(),
      stringsAsFactors = FALSE
    )
  }
  states <- if (length(state_spans)) {
    dplyr::bind_rows(lapply(seq_along(state_spans), function(i) {
      spec <- state_spans[[i]]
      span <- as.integer(spec$span)
      broad <- identical(spec$class, "high_frequency_spiking")
      data.frame(
        train = "train_1", state_id = paste0("state_", i),
        state_class = spec$class,
        state_family = if (broad) "broad_high_frequency_state" else "tonic",
        state_subtype = if (broad) "high_frequency_tonic" else "tonic",
        start_isi = span[1], end_isi = span[2],
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      train = character(), state_id = character(), state_class = character(),
      state_family = character(), state_subtype = character(),
      start_isi = integer(), end_isi = integer(), stringsAsFactors = FALSE
    )
  }
  list(
    metadata = data.frame(
      run_id = "event_regime_fixture",
      params_hash = paste(rep("1", 64L), collapse = ""),
      product_sha256 = paste(rep("2", 64L), collapse = ""),
      stringsAsFactors = FALSE
    ),
    events = events, gaps = gaps, states = states,
    per_isi = data.frame(
      train = "train_1", isi_index = seq_len(n),
      timestamp_sec = seq_len(n) * base,
      ISI_sec = c(NA_real_, rep(base, n - 1L)),
      stringsAsFactors = FALSE
    )
  )
}

event_regime_materialize <- function(fixture, hard_boundaries = NULL) {
  SpikeTrainPatternDetector:::stpd_event_regime_materialize(
    fixture, hard_boundaries
  )
}

test_that("three canonical Bursts create one non-destructive parent envelope", {
  fixture <- event_regime_fixture(
    event_spans = list(c(10, 13), c(20, 23), c(30, 33)),
    gap_specs = list(
      list(span = c(15, 15), long = FALSE),
      list(span = c(25, 25), long = FALSE)
    ),
    state_spans = list(
      list(span = c(8, 35), class = "high_frequency_spiking"),
      list(span = c(14, 29), class = "tonic")
    )
  )
  source_hash <- digest::digest(
    list(fixture$events, fixture$gaps, fixture$states),
    algo = "sha256", serialize = TRUE
  )
  out <- event_regime_materialize(fixture)
  regime <- out$regimes[out$regimes$regime_class == "recurrent_bursting", ]

  expect_equal(nrow(regime), 1L)
  expect_identical(regime$semantic_domain, "state")
  expect_identical(regime$parent_state_class, "recurrent_bursting_state")
  expect_identical(regime$candidate_status, "descriptive_state_candidate")
  expect_identical(regime$trigger_route, "three_canonical_burst_events")
  expect_identical(regime$start_isi, 10L)
  expect_identical(regime$end_isi, 33L)
  expect_identical(regime$envelope_n_isi, 24L)
  expect_identical(regime$direct_support_isi_n, 12L)
  expect_identical(regime$interruption_isi_n, 12L)
  expect_false(regime$absolute_isi_threshold_used)
  expect_true(regime$one_pass_nonrecursive)
  expect_equal(sum(out$memberships$regime_id == regime$regime_id), 3L)
  expect_true(all(out$memberships$child_domain == "event"))
  expect_equal(nrow(out$carrier_relationships), 2L)
  expect_identical(regime$carrier_context, "mixed_broad_hfs_and_tonic")
  expect_identical(
    digest::digest(
      list(fixture$events, fixture$gaps, fixture$states),
      algo = "sha256", serialize = TRUE
    ),
    source_hash
  )
})

test_that("Pause is interruption but QC/acquisition boundary blocks recurrence", {
  fixture <- event_regime_fixture(
    event_spans = list(c(10, 13), c(20, 23), c(30, 33)),
    gap_specs = list(
      list(span = c(15, 15), long = FALSE),
      list(span = c(25, 25), long = TRUE)
    )
  )
  without_boundary <- event_regime_materialize(fixture)
  expect_equal(sum(
    without_boundary$regimes$regime_class == "recurrent_bursting"
  ), 1L)

  with_boundary <- event_regime_materialize(
    fixture,
    data.frame(
      train = "train_1", boundary_isi = 25L,
      boundary_class = "acquisition_hard", stringsAsFactors = FALSE
    )
  )
  expect_equal(sum(
    with_boundary$regimes$regime_class == "recurrent_bursting"
  ), 0L)
})

test_that("Pause regimes use typed 3-long or 5-ordinary accepted triggers", {
  long_fixture <- event_regime_fixture(gap_specs = list(
    list(span = c(10, 10), long = TRUE),
    list(span = c(20, 20), long = TRUE),
    list(span = c(30, 30), long = TRUE)
  ))
  long_out <- event_regime_materialize(long_fixture)
  expect_equal(nrow(long_out$regimes), 1L)
  expect_identical(
    long_out$regimes$trigger_route,
    "three_high_specificity_long_pauses"
  )
  expect_identical(long_out$regimes$long_pause_child_n, 3L)
  expect_identical(long_out$regimes$semantic_domain, "state")
  expect_identical(
    long_out$regimes$parent_state_class, "recurrent_pause_state")

  regular_fixture <- event_regime_fixture(gap_specs = lapply(
    c(10L, 20L, 30L, 40L, 50L),
    function(index) list(span = c(index, index), long = FALSE)
  ))
  regular_out <- event_regime_materialize(regular_fixture)
  expect_equal(nrow(regular_out$regimes), 1L)
  expect_identical(
    regular_out$regimes$trigger_route,
    "five_ordinary_accepted_pause_events"
  )
  expect_identical(regular_out$regimes$accepted_pause_child_n, 5L)
  expect_identical(
    regular_out$regimes$candidate_layer,
    "post_final_recurrent_parent_state"
  )
  expect_identical(
    regular_out$regimes$candidate_source, "canonical_final_child_entities"
  )
  expect_true(all(
    regular_out$memberships$parent_state_class == "recurrent_pause_state"))

  rejected_fixture <- regular_fixture
  rejected_fixture$gaps$candidate_decision[3] <- "rejected"
  expect_equal(nrow(event_regime_materialize(rejected_fixture)$regimes), 0L)

  # Both routes must be budgeted independently.  Here the five ordinary Pause
  # route passes, whereas the first-to-third long-Pause subset has excessive
  # interruption fraction and must not mask the valid OR route. Long Pauses
  # remain members of the envelope but do not contribute to the ordinary five.
  fallback_fixture <- event_regime_fixture(gap_specs = list(
    list(span = c(7, 7), long = FALSE),
    list(span = c(8, 8), long = FALSE),
    list(span = c(9, 9), long = FALSE),
    list(span = c(10, 10), long = TRUE),
    list(span = c(25, 25), long = TRUE),
    list(span = c(41, 41), long = TRUE),
    list(span = c(42, 42), long = FALSE),
    list(span = c(43, 43), long = FALSE)
  ))
  fallback_out <- event_regime_materialize(fallback_fixture)
  expect_equal(nrow(fallback_out$regimes), 1L)
  expect_identical(
    fallback_out$regimes$trigger_route,
    "five_ordinary_accepted_pause_events"
  )
  expect_identical(fallback_out$regimes$trigger_child_n, 5L)
  expect_identical(fallback_out$regimes$long_pause_child_n, 3L)
  expect_true(fallback_out$regimes$temporal_budget_pass)

  mixed_fixture <- event_regime_fixture(gap_specs = list(
    list(span = c(10, 10), long = TRUE),
    list(span = c(20, 20), long = TRUE),
    list(span = c(30, 30), long = FALSE),
    list(span = c(40, 40), long = FALSE),
    list(span = c(50, 50), long = FALSE)
  ))
  expect_equal(nrow(event_regime_materialize(mixed_fixture)$regimes), 0L)

  one_entity <- event_regime_fixture(gap_specs = list(
    list(span = c(10, 12), long = TRUE)
  ))
  expect_equal(nrow(event_regime_materialize(one_entity)$regimes), 0L)
})

test_that("selection uses one frozen anchor cadence without recursive budget growth", {
  fixture <- event_regime_fixture(event_spans = list(
    c(10, 13), c(20, 23), c(30, 33), c(40, 43), c(50, 53)
  ))
  out <- event_regime_materialize(fixture)
  expect_equal(nrow(out$regimes), 1L)
  expect_identical(out$regimes$start_isi, 10L)
  expect_identical(out$regimes$end_isi, 53L)
  expect_identical(out$regimes$trigger_child_n, 3L)
  expect_identical(out$regimes$child_n, 5L)
  expect_equal(nrow(out$memberships), 5L)

  shuffled <- fixture
  shuffled$events <- shuffled$events[c(5, 2, 4, 1, 3), , drop = FALSE]
  reordered <- event_regime_materialize(shuffled)
  expect_identical(out$regimes, reordered$regimes)
  expect_identical(out$memberships, reordered$memberships)

  sparse <- event_regime_fixture(
    event_spans = list(c(10, 13), c(200, 203), c(400, 403)), n = 450L
  )
  expect_equal(nrow(event_regime_materialize(sparse)$regimes), 0L)
})

test_that("multiplicative time scaling preserves decisions without absolute thresholds", {
  make <- function(scale) event_regime_materialize(event_regime_fixture(
    scale = scale,
    event_spans = list(c(10, 13), c(20, 23), c(30, 33)),
    gap_specs = list(
      list(span = c(15, 15), long = FALSE),
      list(span = c(25, 25), long = FALSE)
    )
  ))
  one <- make(1)
  four <- make(4)
  ten <- make(10)
  invariant <- c(
    "train", "regime_class", "trigger_route", "start_isi", "end_isi",
    "envelope_n_isi", "direct_support_isi_n", "interruption_isi_n",
    "interruption_fraction", "child_n", "absolute_isi_threshold_used"
  )
  expect_identical(one$regimes[invariant], four$regimes[invariant])
  expect_identical(one$regimes[invariant], ten$regimes[invariant])
  expect_equal(four$regimes$duration_sec, one$regimes$duration_sec * 4)
  expect_equal(ten$regimes$duration_sec, one$regimes$duration_sec * 10)
  expect_equal(
    four$regimes$adjacent_child_gap_median_sec,
    one$regimes$adjacent_child_gap_median_sec * 4
  )
  expect_equal(
    ten$regimes$adjacent_child_gap_max_sec,
    one$regimes$adjacent_child_gap_max_sec * 10
  )
})

test_that("public detection attaches a parent-hash-bound descriptive product", {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- FALSE
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
  product <- stpd_event_regimes(out)
  expect_s3_class(product, "stpd_event_regime_product")
  expect_false(product$metadata$authoritative)
  expect_false(product$metadata$biological_ground_truth)
  expect_identical(
    product$metadata$parent_final_product_sha256,
    out$results$multitrack_final$metadata$product_sha256
  )
  expect_false(product$metadata$absolute_isi_threshold_used)
  expect_true(product$metadata$one_pass_nonrecursive)
  expect_true("post_final_pause_aggregation_only" %in%
                product$invariants$check_name)
  expect_true(all(!product$regimes$regime_class %in% c(
    "tonic", "high_frequency_spiking"
  )))
  expect_true(all(product$regimes$semantic_domain == "state"))
  expect_true(all(product$regimes$parent_state_class %in% c(
    "recurrent_bursting_state", "recurrent_pause_state"
  )))

  tampered <- product
  tampered$invariants$run_id[1] <- "CORRUPT"
  expect_error(
    SpikeTrainPatternDetector:::stpd_event_regime_validate(tampered),
    class = "stpd_event_regime_error"
  )

  if (nrow(product$regimes)) {
    tampered_domain <- product
    tampered_domain$regimes$semantic_domain[1] <- "event"
    expect_error(
      SpikeTrainPatternDetector:::stpd_event_regime_validate(tampered_domain),
      class = "stpd_event_regime_error"
    )
  }
})

test_that("QC support contamination and boundary ontology fail closed", {
  fixture <- event_regime_fixture(
    event_spans = list(c(10, 13), c(20, 23), c(30, 33))
  )
  fixture$per_isi$timestamp_sec[10] <- NA_real_
  contaminated <- event_regime_materialize(fixture)
  expect_equal(nrow(contaminated$regimes), 0L)

  expect_error(
    event_regime_materialize(
      event_regime_fixture(
        event_spans = list(c(10, 13), c(20, 23), c(30, 33))
      ),
      data.frame(
        train = "train_1", boundary_isi = 25L,
        boundary_class = "canonical_pause", stringsAsFactors = FALSE
      )
    ),
    class = "stpd_event_regime_error"
  )

  for (bad_boundary in list(
    data.frame(
      train = "train_typo", boundary_isi = 25L,
      boundary_class = "qc_hard", stringsAsFactors = FALSE
    ),
    data.frame(
      train = "train_1", boundary_isi = 71L,
      boundary_class = "qc_hard", stringsAsFactors = FALSE
    )
  )) {
    expect_error(
      event_regime_materialize(fixture, bad_boundary),
      class = "stpd_event_regime_error"
    )
  }
})
