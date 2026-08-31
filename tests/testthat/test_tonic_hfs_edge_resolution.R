tonic_hfs_candidate <- function(id, label, start, end, priority = 100) {
  data.frame(
    candidate_id = id,
    candidate_layer = paste0("fixture_", label),
    candidate_source = "unit_test",
    final_label = label,
    class = label,
    start_isi = as.integer(start),
    end_isi = as.integer(end),
    n_isi = as.integer(end - start + 1L),
    n_spikes = as.integer(end - start + 2L),
    score = 10,
    priority = priority,
    stringsAsFactors = FALSE
  )
}

tonic_hfs_stub_detector <- function(expected) {
  force(expected)
  function(dat, params, vp, min_isi_sec = 0.001, train = "",
           hard_boundaries = NULL) {
    expected
  }
}

tonic_hfs_resolve <- function(hfs, tonic, residual) {
  resolve <- getFromNamespace(
    "stpd_event_grammar_resolve_edge_tonic_hfs_states",
    "SpikeTrainPatternDetector"
  )
  resolve(
    dat = data.frame(ISI_sec = c(NA_real_, rep(0.02, 200L))),
    hfs = hfs, tonic = tonic, params = list(), vp = list(),
    hfs_detector = tonic_hfs_stub_detector(residual)
  )
}

test_that("edge Tonic transitions require a fully re-passed HFS residual", {
  cases <- list(
    boundary_touch = list(
      hfs = c(81L, 113L), tonic = c(113L, 127L),
      residual = c(81L, 112L)
    ),
    left_prefix = list(
      hfs = c(130L, 172L), tonic = c(130L, 140L),
      residual = c(141L, 172L)
    ),
    recording_edge = list(
      hfs = c(2L, 37L), tonic = c(2L, 12L),
      residual = c(13L, 37L)
    )
  )
  for (case in cases) {
    hfs <- tonic_hfs_candidate(
      "hfs_parent", "high_frequency_spiking",
      case$hfs[1], case$hfs[2]
    )
    tonic <- tonic_hfs_candidate(
      "tonic_peer", "tonic", case$tonic[1], case$tonic[2]
    )
    residual <- tonic_hfs_candidate(
      "hfs_redetected", "high_frequency_spiking",
      case$residual[1], case$residual[2]
    )
    out <- tonic_hfs_resolve(hfs, tonic, residual)

    expect_equal(c(out$hfs$start_isi, out$hfs$end_isi), case$residual)
    expect_identical(
      out$hfs$state_overlap_resolution,
      "resolved_edge_tonic_hfs_transition"
    )
    expect_true(out$hfs$residual_gate_pass)
    expect_identical(
      out$tonic$state_overlap_resolution,
      "resolved_edge_tonic_hfs_transition"
    )
    expect_true(out$tonic$residual_gate_pass)
  }
})

test_that("an interior Tonic proposal cannot split Broad HFS", {
  hfs <- tonic_hfs_candidate(
    "hfs_parent", "high_frequency_spiking", 2L, 106L
  )
  tonic <- tonic_hfs_candidate("tonic_peer", "tonic", 86L, 96L)
  residual <- dplyr::bind_rows(
    tonic_hfs_candidate("left", "high_frequency_spiking", 2L, 85L),
    tonic_hfs_candidate("right", "high_frequency_spiking", 97L, 106L)
  )
  out <- tonic_hfs_resolve(hfs, tonic, residual)

  expect_equal(c(out$hfs$start_isi, out$hfs$end_isi), c(2L, 106L))
  expect_identical(
    out$hfs$state_overlap_resolution,
    "unresolved_nonedge_state_conflict"
  )
  expect_identical(out$hfs$state_overlap_geometry, "tonic_inside_hfs")
  expect_false(isTRUE(out$hfs$residual_gate_pass))
})

test_that("a failed HFS residual restores the historical HFS veto", {
  hfs <- tonic_hfs_candidate(
    "hfs_parent", "high_frequency_spiking", 30L, 70L
  )
  tonic <- tonic_hfs_candidate("tonic_peer", "tonic", 30L, 55L)
  out <- tonic_hfs_resolve(hfs, tonic, data.frame())

  expect_equal(c(out$hfs$start_isi, out$hfs$end_isi), c(30L, 70L))
  expect_identical(
    out$hfs$state_overlap_resolution,
    "edge_candidate_residual_failed_keep_parent"
  )
  expect_false(out$hfs$residual_gate_pass)
  expect_identical(
    out$hfs$residual_failed_checks,
    "no_exact_residual_candidate"
  )
})

test_that("edge resolution is invariant to candidate row order", {
  hfs <- dplyr::bind_rows(
    tonic_hfs_candidate(
      "hfs_b", "high_frequency_spiking", 100L, 150L
    ),
    tonic_hfs_candidate("hfs_a", "high_frequency_spiking", 10L, 60L)
  )
  tonic <- dplyr::bind_rows(
    tonic_hfs_candidate("tonic_b", "tonic", 140L, 165L),
    tonic_hfs_candidate("tonic_a", "tonic", 10L, 20L)
  )
  residual <- dplyr::bind_rows(
    tonic_hfs_candidate(
      "residual_a", "high_frequency_spiking", 21L, 60L
    ),
    tonic_hfs_candidate(
      "residual_b", "high_frequency_spiking", 100L, 139L
    )
  )
  forward <- tonic_hfs_resolve(hfs, tonic, residual)
  reverse <- tonic_hfs_resolve(
    hfs[2:1, ], tonic[2:1, ], residual[2:1, ]
  )

  canonical <- function(x) {
    x <- x[order(x$root_hfs_candidate_id), c(
      "start_isi", "end_isi", "root_hfs_candidate_id",
      "state_overlap_resolution"
    )]
    rownames(x) <- NULL
    x
  }
  expect_equal(canonical(forward$hfs), canonical(reverse$hfs))
})
