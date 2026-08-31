state_boundary_semantics_pool <- function(rows) {
  defaults <- list(
    candidate_layer = "state_boundary_semantics_fixture",
    candidate_source = "pre_hf_spiking_protection",
    score = 10,
    priority = 1000,
    CV = NA_real_,
    LV = NA_real_,
    MM = NA_real_,
    hf_spiking_large_fraction = NA_real_
  )
  dplyr::bind_rows(lapply(rows, function(row) {
    row <- utils::modifyList(defaults, row)
    data.frame(
      candidate_id = as.character(row$candidate_id),
      candidate_layer = as.character(row$candidate_layer),
      candidate_source = as.character(row$candidate_source),
      final_label = as.character(row$final_label),
      start_isi = as.integer(row$start_isi),
      end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$end_isi - row$start_isi + 1L),
      score = as.numeric(row$score),
      priority = as.numeric(row$priority),
      CV = as.numeric(row$CV),
      LV = as.numeric(row$LV),
      MM = as.numeric(row$MM),
      hf_spiking_large_fraction = as.numeric(row$hf_spiking_large_fraction),
      stringsAsFactors = FALSE
    )
  }))
}

state_boundary_semantics_resolve <- function(rows) {
  pool <- state_boundary_semantics_pool(rows)
  phase1a <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool,
    patterns = c(
      "burst", "long_burst", "high_frequency_spiking",
      "high_frequency_tonic", "tonic", "pause"
    )
  )
  SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a, min_isi_sec = 0.001
  )
}

test_that("one or several Bursts overlay every State without changing geometry", {
  state_labels <- c("tonic", "high_frequency_spiking")
  for (state_label in state_labels) {
    for (burst_n in c(1L, 2L)) {
      bursts <- list(
        list(
          candidate_id = "burst_1", final_label = "burst",
          start_isi = 30L, end_isi = 34L
        ),
        list(
          candidate_id = "burst_2", final_label = "long_burst",
          start_isi = 60L, end_isi = 64L
        )
      )[seq_len(burst_n)]
      resolved <- state_boundary_semantics_resolve(c(list(list(
        candidate_id = "state_parent", final_label = state_label,
        start_isi = 10L, end_isi = 90L
      )), bursts))

      parent <- resolved$state_parents[
        resolved$state_parents$state_candidate_id == "state_parent",
        , drop = FALSE
      ]
      fragment <- resolved$state_fragments[
        resolved$state_fragments$root_candidate_id == "state_parent",
        , drop = FALSE
      ]
      overlays <- resolved$overlays[
        resolved$overlays$state_candidate_id == "state_parent",
        , drop = FALSE
      ]
      relations <- resolved$relationships[
        resolved$relationships$relationship_type ==
          "event_overlay_within_state",
        , drop = FALSE
      ]

      expect_identical(
        unname(as.integer(parent[c("start_isi", "end_isi")])),
        c(10L, 90L),
        info = paste(state_label, burst_n, "Burst parent geometry")
      )
      expect_identical(parent$split_kind, "none")
      expect_false(parent$parent_consumed_in_provisional_policy)
      expect_identical(fragment$start_isi, 10L)
      expect_identical(fragment$end_isi, 90L)
      expect_equal(nrow(overlays), burst_n)
      expect_equal(nrow(relations), burst_n)
      expect_true(all(relations$non_destructive))
      expect_false(any(grepl(
        "burst.*split|split.*burst",
        resolved$relationships$compatibility_rule,
        ignore.case = TRUE
      )))
    }
  }
})

test_that("pre-existing independent State boundaries survive a crossing Burst", {
  resolved <- state_boundary_semantics_resolve(list(
    list(
      candidate_id = "left_state", final_label = "high_frequency_spiking",
      start_isi = 10L, end_isi = 40L
    ),
    list(
      candidate_id = "right_state", final_label = "tonic",
      start_isi = 50L, end_isi = 80L
    ),
    list(
      candidate_id = "crossing_burst", final_label = "burst",
      start_isi = 35L, end_isi = 55L
    )
  ))
  parents <- resolved$state_parents[
    order(resolved$state_parents$start_isi), , drop = FALSE
  ]
  relations <- resolved$relationships[
    resolved$relationships$relationship_type == "event_overlay_within_state",
    , drop = FALSE
  ]

  expect_identical(parents$state_candidate_id, c("left_state", "right_state"))
  expect_identical(parents$start_isi, c(10L, 50L))
  expect_identical(parents$end_isi, c(40L, 80L))
  expect_true(all(parents$split_kind == "none"))
  expect_equal(nrow(relations), 2L)
  expect_true(all(relations$non_destructive))
})

test_that("canonical Pause cuts direct support for every State class", {
  for (state_label in c("tonic", "high_frequency_spiking")) {
    resolved <- state_boundary_semantics_resolve(list(
      list(
        candidate_id = "state_parent", final_label = state_label,
        start_isi = 10L, end_isi = 30L
      ),
      list(
        candidate_id = "canonical_pause", final_label = "pause",
        start_isi = 20L, end_isi = 20L
      )
    ))
    parent <- resolved$state_parents[
      resolved$state_parents$state_candidate_id == "state_parent",
      , drop = FALSE
    ]
    fragments <- resolved$state_fragments[
      resolved$state_fragments$root_candidate_id == "state_parent",
      , drop = FALSE
    ]
    fragments <- fragments[order(fragments$start_isi), , drop = FALSE]
    boundary <- resolved$relationships[
      resolved$relationships$compatibility_rule ==
        "state_pause_direct_support_split" &
        resolved$relationships$relationship_type == "gap_boundary",
      , drop = FALSE
    ]

    expect_identical(parent$split_kind, "pause_boundary")
    expect_true(parent$parent_consumed_in_provisional_policy)
    expect_identical(fragments$start_isi, c(10L, 21L))
    expect_identical(fragments$end_isi, c(19L, 30L))
    expect_equal(nrow(boundary), 1L)
    expect_true(boundary$non_destructive)
    expect_true(all(fragments$provisional_gate_pass))
    expect_true(all(grepl(
      "parent_episode_acceptance_inherited",
      fragments$provisional_gate_status,
      fixed = TRUE
    )))
  }
})

test_that("the automatic boundary registry admits only Pause and QC-invalid support", {
  dat <- data.frame(
    timestamp_sec = seq(0, 0.29, length.out = 30L),
    ISI_sec = c(NA_real_, rep(0.01, 29L)),
    stringsAsFactors = FALSE
  )
  dat$ISI_sec[20L] <- NA_real_
  gaps <- data.frame(
    start_isi = 15L, end_isi = 15L,
    stringsAsFactors = FALSE
  )
  boundaries <- SpikeTrainPatternDetector:::stpd_multitrack_auto_boundary_rows(
    dat, gaps, min_isi_sec = 0.001
  )
  fragments <-
    SpikeTrainPatternDetector:::stpd_multitrack_compatibility_subtract_intervals(
      10L, 30L, boundaries
    )

  expect_setequal(boundaries$boundary_kind, c("pause", "invalid_support"))
  expect_identical(boundaries$start_isi, c(15L, 20L))
  expect_identical(boundaries$end_isi, c(15L, 20L))
  expect_identical(fragments$start_isi, c(10L, 16L, 21L))
  expect_identical(fragments$end_isi, c(14L, 19L, 30L))
  expect_false(any(grepl("burst", boundaries$boundary_kind, fixed = TRUE)))
})

test_that("AUTO Event-State relationships are complete and non-destructive", {
  events <- data.frame(
    train = rep("train_1", 4L),
    event_id = paste0("event_", seq_len(4L)),
    start_isi = c(15L, 30L, 55L, 70L),
    end_isi = c(18L, 34L, 59L, 74L),
    n_isi = c(4L, 5L, 5L, 5L),
    stringsAsFactors = FALSE
  )
  states <- data.frame(
    train = rep("train_1", 3L),
    state_id = c("tonic_state_left", "hfs_state", "tonic_state_right"),
    state_class = c("tonic", "high_frequency_spiking", "tonic"),
    start_isi = c(10L, 40L, 65L),
    end_isi = c(35L, 60L, 90L),
    n_isi = c(26L, 21L, 26L),
    stringsAsFactors = FALSE
  )
  original_states <- states
  relationships <- SpikeTrainPatternDetector:::stpd_multitrack_auto_relationships(
    events, states, policy_hash = strrep("a", 64L),
    run_id = "state_event_boundary_semantics",
    params_hash = strrep("b", 64L)
  )

  expect_identical(states, original_states)
  expect_equal(nrow(relationships), 4L)
  expect_true(all(relationships$non_destructive))
  expect_setequal(relationships$state_id, states$state_id)
  expect_setequal(
    relationships$compatibility_rule,
    c(
      "burst_state_non_destructive_coexistence",
      "hfs_burst_non_destructive_coexistence"
    )
  )
})
