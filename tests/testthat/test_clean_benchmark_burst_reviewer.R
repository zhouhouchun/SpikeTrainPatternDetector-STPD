make_clean_burst_reviewer_fixture <- function() {
  right <- 2:18
  intervals <- data.frame(
    Sample_ID = "S_TEST", Template_ID = "TPL_TEST", Scale_Factor = 1L,
    Right_Spike_Index = right, B_s = 0.1,
    Start_s = (right - 2) * 0.01, End_s = (right - 1) * 0.01,
    ISI_s = 0.01, event_truth = "other", event_prediction = "other",
    hfs_truth = "other", hfs_prediction = "other",
    stringsAsFactors = FALSE
  )
  predicted <- data.frame(
    Sample_ID = "S_TEST", Template_ID = "TPL_TEST", Scale_Factor = 1L,
    target = "burst", start_isi = c(3L, 8L, 14L),
    end_isi = c(5L, 9L, 16L), stringsAsFactors = FALSE
  )
  truth <- data.frame(
    Sample_ID = "S_TEST", Template_ID = "TPL_TEST", Scale_Factor = 1L,
    target = "burst", start_isi = c(3L, 10L, 14L),
    end_isi = c(5L, 11L, 17L),
    subtype = c("standalone_burst", "burst_in_hfs", "standalone_burst"),
    stringsAsFactors = FALSE
  )
  matches <- data.frame(
    train = c("S_TEST", "S_TEST"), pattern = "burst",
    iou = c(1, 0.75), truth_start_isi = c(3L, 14L),
    truth_end_isi = c(5L, 17L), predicted_start_isi = c(3L, 14L),
    predicted_end_isi = c(5L, 16L), Scale_Factor = 1L,
    Template_ID = "TPL_TEST", target = "burst",
    start_boundary_error_isi = c(0L, 0L),
    end_boundary_error_isi = c(0L, -1L), stringsAsFactors = FALSE
  )
  list(
    intervals = intervals, predicted = predicted,
    truth = truth, matches = matches
  )
}

test_that("clean Burst reviewer separates FP, FN, and boundary cases", {
  fixture <- make_clean_burst_reviewer_fixture()
  catalog <- stpd_clean_burst_catalog(
    fixture$intervals, fixture$predicted,
    fixture$truth, fixture$matches
  )

  counts <- table(catalog$error_type)
  expect_equal(as.integer(counts[c("boundary", "fn", "fp")]), c(1L, 1L, 1L))
  expect_equal(anyDuplicated(catalog$case_id), 0L)
  expect_true(all(catalog$Scale_Factor == 1L))

  false_positive <- catalog[catalog$error_type == "fp", , drop = FALSE]
  expect_equal(false_positive$predicted_start_isi, 8L)
  expect_equal(false_positive$predicted_end_isi, 9L)
  expect_equal(false_positive$iou, 0)
  expect_true(is.na(false_positive$truth_start_isi))

  false_negative <- catalog[catalog$error_type == "fn", , drop = FALSE]
  expect_equal(false_negative$truth_start_isi, 10L)
  expect_equal(false_negative$truth_end_isi, 11L)
  expect_equal(false_negative$truth_subtype, "burst_in_hfs")

  boundary <- catalog[catalog$error_type == "boundary", , drop = FALSE]
  expect_equal(boundary$iou, 0.75)
  expect_equal(boundary$truth_end_isi, 17L)
  expect_equal(boundary$predicted_end_isi, 16L)
})

test_that("clean Burst reviewer reports exact integer-support IoU", {
  expect_equal(stpd_clean_burst_iou(10L, 14L, 10L, 14L), 1)
  expect_equal(stpd_clean_burst_iou(10L, 14L, 12L, 16L), 3 / 7)
  expect_equal(stpd_clean_burst_iou(10L, 11L, 12L, 13L), 0)
  expect_true(is.na(stpd_clean_burst_iou(NA, 11L, 12L, 13L)))
})
