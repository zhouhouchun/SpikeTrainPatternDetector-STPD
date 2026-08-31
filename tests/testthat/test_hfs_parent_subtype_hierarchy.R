hfs_hierarchy_dataset <- function(isi) {
  time <- c(0, cumsum(as.numeric(isi)))
  dat <- data.frame(
    idx = seq_along(time), timestamp_sec = time,
    ISI_sec = c(NA_real_, as.numeric(isi)), pattern_manual = "",
    pattern_auto = "", stringsAsFactors = FALSE
  )
  SpikeTrainPatternDetector:::make_dataset(
    name = "hfs_hierarchy", source = "synthetic",
    trains = list(train_1 = dat), unit_in = "s"
  )
}

hfs_hierarchy_detect <- function(isi) {
  params <- default_params()
  # State/subtype hierarchy is orthogonal to zero-shot identifiability. Use the
  # explicit default HFS contract so a homogeneous parent is authorized; the
  # histogram-homogeneous abstention has separate negative tests.
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  stpd_detect(
    hfs_hierarchy_dataset(isi), params,
    selected_trains = "train_1", collect_diagnostics = TRUE,
    label_blind = TRUE
  )
}

test_that("regular and irregular HF share one Broad-HFS parent", {
  cases <- list(
    regular = list(
      isi = rep(0.010, 60L), regularity = "regular",
      subtype = "high_frequency_tonic"
    ),
    irregular = list(
      isi = rep(c(0.004, 0.018, 0.006, 0.016), 15L),
      regularity = "irregular",
      subtype = "high_frequency_irregular_state"
    )
  )
  for (case in cases) {
    product <- stpd_multitrack_auto(hfs_hierarchy_detect(case$isi))
    hfs <- product$states[
      product$states$state_class == "high_frequency_spiking", , drop = FALSE
    ]
    expect_equal(nrow(hfs), 1L)
    expect_identical(hfs$state_family, "broad_high_frequency_state")
    expect_identical(hfs$state_frequency_class, "high")
    expect_identical(hfs$state_regularity_class, case$regularity)
    expect_identical(hfs$state_subtype, case$subtype)
    expect_identical(hfs$subtype_status, "classified")

    direct <- product$per_isi$state_id == hfs$state_id
    expect_equal(sum(direct), hfs$n_isi)
    expect_true(all(product$per_isi$state_family[direct] ==
      "broad_high_frequency_state"))
    expect_true(all(product$per_isi$state_subtype[direct] == case$subtype))
    expect_false(any(product$states$state_class == "tonic"))
  }
})

test_that("regularity gray zone preserves Broad HFS and abstains on subtype", {
  product <- stpd_multitrack_auto(
    hfs_hierarchy_detect(rep(c(0.010, 0.020), 30L))
  )
  hfs <- product$states[
    product$states$state_class == "high_frequency_spiking", , drop = FALSE
  ]
  expect_equal(nrow(hfs), 1L)
  expect_identical(hfs$state_frequency_class, "high")
  expect_identical(hfs$state_regularity_class, "unresolved")
  expect_identical(hfs$state_subtype, "hf_unresolved")
  expect_identical(hfs$subtype_status, "regularity_gray_zone")
  expect_equal(hfs$subtype_median_cv2, 2 / 3, tolerance = 1e-12)
})

test_that("legacy HFT evidence cannot create or enlarge Broad-HFS support", {
  pool <- data.frame(
    candidate_id = c("hfs_parent", "hft_evidence", "tonic_peer", "burst"),
    candidate_layer = "hfs_hierarchy_fixture",
    candidate_source = "pre_hf_spiking_protection",
    final_label = c(
      "high_frequency_spiking", "high_frequency_tonic", "tonic", "burst"
    ),
    start_isi = c(10L, 5L, 30L, 40L),
    end_isi = c(80L, 90L, 70L, 44L),
    n_isi = c(71L, 86L, 41L, 5L),
    score = c(10, 1000, 1000, 10), priority = c(10, 1000, 1000, 10),
    stringsAsFactors = FALSE
  )
  shadow <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool, patterns = unique(pool$final_label), params = default_params()
  )
  rows <- shadow$candidates
  selected <- rows$final_label[rows$selected_within_track]
  expect_setequal(selected, c("high_frequency_spiking", "burst"))
  expect_false(rows$selected_within_track[
    rows$final_label == "high_frequency_tonic"
  ])
  expect_match(
    rows$track_selection_status[rows$final_label == "high_frequency_tonic"],
    "subtype_evidence_for_selected_broad_hf_parent"
  )
  expect_match(
    rows$track_selection_status[rows$final_label == "tonic"],
    "blocked_by_selected_broad_hf_parent"
  )

  hft_only <- pool[pool$final_label == "high_frequency_tonic", , drop = FALSE]
  hft_shadow <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    hft_only, patterns = "high_frequency_tonic", params = default_params()
  )
  expect_false(any(hft_shadow$candidates$selected_within_track))
  expect_match(
    hft_shadow$candidates$track_selection_status,
    "subtype_evidence_without_selected_broad_hf_parent"
  )
})
