test_that("MANUAL lock removes undersized AUTO fragments created by trimming", {
  params <- default_params()
  ds <- stpd_golden_test_dataset("middle_burst")

  probe <- stpd_detect(
    ds,
    params,
    selected_trains = "train_1",
    lock_manual = FALSE,
    collect_diagnostics = TRUE
  )
  expect_equal(which(as.character(probe$trains$train_1$pattern_auto) == "burst"), 4:6)

  ds$trains$train_1$pattern_manual[5] <- "tonic"
  out <- stpd_detect(
    ds,
    params,
    selected_trains = "train_1",
    lock_manual = TRUE,
    collect_diagnostics = TRUE
  )
  train <- out$trains$train_1

  expect_equal(as.character(train$pattern_auto[5]), "")
  expect_false(any(as.character(train$pattern_auto[c(4, 6)]) == "burst"))
  expect_equal(as.character(train$pattern_auto[7]), "pause")

  auto_events <- stpd_predicted_events(
    out,
    params,
    selected_trains = "train_1"
  )
  expect_false(any(
    as.character(auto_events$pattern) == "burst" &
      suppressWarnings(as.numeric(auto_events$n_spikes)) < params$burst$G_min,
    na.rm = TRUE
  ))

  fragment_audit <- attr(train, "posthoc_fragment_audit")
  expect_true(is.data.frame(fragment_audit))
  removed <- fragment_audit[
    as.character(fragment_audit$pattern) == "burst" &
      as.character(fragment_audit$action) == "removed_auto_fragment_after_overlap_resolution",
    ,
    drop = FALSE
  ]
  expect_equal(sort(as.integer(removed$start_isi)), c(4L, 6L))
  expect_equal(sort(as.integer(removed$end_isi)), c(4L, 6L))
  expect_true(all(
    as.integer(removed$n_spikes_final) < as.integer(removed$required_min_spikes)
  ))

  result_audit <- as.data.frame(out$results$posthoc_fragment_audit)
  result_removed <- result_audit[
    as.character(result_audit$pattern) == "burst" &
      as.character(result_audit$action) == "removed_auto_fragment_after_overlap_resolution",
    ,
    drop = FALSE
  ]
  expect_equal(sort(as.integer(result_removed$start_isi)), c(4L, 6L))
})

test_that("posthoc fragment audit merge preserves earlier and MANUAL-lock rows", {
  earlier <- data.frame(
    train = "train_1",
    pattern = "high_frequency_spiking",
    start_isi = 2L,
    end_isi = 2L,
    action = "removed_auto_event_by_pattern_isi_gate",
    stringsAsFactors = FALSE
  )
  after_manual_lock <- data.frame(
    train = "train_1",
    pattern = "burst",
    start_isi = 4L,
    end_isi = 4L,
    action = "removed_auto_fragment_after_overlap_resolution",
    stringsAsFactors = FALSE
  )

  merged <- stpd_product_merge_posthoc_fragment_audits(
    earlier,
    NULL,
    data.frame(),
    after_manual_lock
  )

  expect_equal(nrow(merged), 2L)
  expect_equal(
    as.character(merged$action),
    c(
      "removed_auto_event_by_pattern_isi_gate",
      "removed_auto_fragment_after_overlap_resolution"
    )
  )
})
