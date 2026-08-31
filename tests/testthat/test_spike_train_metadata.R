test_that("STN-style column names remain first-class recording metadata", {
  meta <- parse_spike_train_column_metadata(
    c(
      "LT1D00.83_fon1_1_nw_minus_7_08_minus_1_1",
      "LT1D01.96_fon_nw_minus_7_08_minus_1_1",
      "RT2D03.41_fon_nw_minus_7_08_minus_1_3"
    ),
    dataset_name = "STN_2017.csv"
  )

  expect_true(all(meta$parse_ok))
  expect_equal(as.character(meta$structure), rep("STN", 3))
  expect_equal(as.character(meta$side), c("L", "L", "R"))
  expect_equal(as.character(meta$trajectory), c("T1", "T1", "T2"))
  expect_equal(meta$recording_depth, c(0.83, 1.96, 3.41))
  expect_equal(as.character(meta$channel_type), rep("fon", 3))
})
