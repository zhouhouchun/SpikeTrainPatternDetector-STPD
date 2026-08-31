gate_b_consumer_dataset <- function() {
  stpd_detect(
    stpd_golden_test_dataset("middle_burst"), default_params(),
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
}

gate_b_truth_from_auto <- function(auto) {
  rows <- list()
  if (nrow(auto$events) > 0L) rows[[length(rows) + 1L]] <- data.frame(
    truth_interval_id = paste0("truth_", auto$events$event_id),
    train = auto$events$train, semantic_track = "event",
    label = auto$events$event_family,
    start_isi = as.integer(auto$events$start_isi),
    end_isi = as.integer(auto$events$end_isi), stringsAsFactors = FALSE
  )
  if (nrow(auto$states) > 0L) rows[[length(rows) + 1L]] <- data.frame(
    truth_interval_id = paste0("truth_", auto$states$state_id),
    train = auto$states$train, semantic_track = "state",
    label = auto$states$state_class,
    start_isi = as.integer(auto$states$start_isi),
    end_isi = as.integer(auto$states$end_isi), stringsAsFactors = FALSE
  )
  if (nrow(auto$gaps) > 0L) rows[[length(rows) + 1L]] <- data.frame(
    truth_interval_id = paste0("truth_", auto$gaps$gap_id),
    train = auto$gaps$train, semantic_track = "gap",
    label = auto$gaps$gap_class,
    start_isi = as.integer(auto$gaps$start_isi),
    end_isi = as.integer(auto$gaps$end_isi), stringsAsFactors = FALSE
  )
  dplyr::bind_rows(rows)
}

test_that("Gate B authoritative accessor refuses pending local products", {
  ds <- gate_b_consumer_dataset()
  expect_error(
    stpd_multitrack_authoritative(ds, "automatic"),
    "pending_release_attestation"
  )
  expect_error(
    stpd_multitrack_authoritative_per_isi(ds, "preferred"),
    "pending_release_attestation"
  )
})

test_that("Gate B official writer omits pending products", {
  ds <- gate_b_consumer_dataset()
  out <- tempfile("gate_b_official_")
  dir.create(out)
  expect_error(
    SpikeTrainPatternDetector:::stpd_write_multitrack_gate_b(ds, out),
    "pending_release_attestation"
  )
  status <- SpikeTrainPatternDetector:::stpd_write_multitrack_gate_b_fail_soft(ds, out)
  files <- list.files(out)

  expect_identical(status$automatic_status, "omit")
  expect_identical(status$reviewed_final_status, "omit")
  expect_identical(files, "Multitrack_Gate_B_status.csv")
})

test_that("Gate C pending validation is exploratory technical agreement", {
  ds <- gate_b_consumer_dataset()
  auto <- stpd_multitrack_auto(ds)
  truth <- gate_b_truth_from_auto(auto)
  expect_gt(nrow(truth), 0L)
  report <- stpd_multitrack_validation(
    ds, truth, prediction_source = "automatic_v2",
    truth_role = "independent_reference", selected_trains = "train_1",
    iou_threshold = 0.5
  )
  expect_identical(report$metadata$prediction_source, "automatic_v2")
  expect_identical(report$metadata$estimand,
                   "exploratory_technical_agreement")
  expect_false(report$metadata$source_authoritative)
  expect_true(report$metadata$source_label_blind)
  expect_equal(report$metadata$matched_interval_n,
               report$metadata$truth_interval_n)

  reviewed <- stpd_multitrack_validation(
    ds, truth, prediction_source = "reviewed_v2",
    truth_role = "adjudicated_reference", selected_trains = "train_1"
  )
  expect_identical(reviewed$metadata$estimand, "adjudicated_agreement")
  expect_false(reviewed$metadata$source_authoritative)
})
