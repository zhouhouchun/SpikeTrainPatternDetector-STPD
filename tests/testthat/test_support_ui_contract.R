test_that("support run identity becomes stale after data or dataset changes", {
  ds <- stpd_golden_test_dataset("middle_burst")
  trains <- names(ds$trains)[1]
  identity <- stpd_support_run_identity(
    ds, "dataset-a", trains, "poisson_surprise", list(alpha = 1)
  )
  record <- stpd_support_run_record("success", list(ok = TRUE), identity)
  current <- stpd_support_run_state(record, ds, "dataset-a")
  expect_true(current$current)
  expect_identical(current$status, "success")

  expect_identical(
    stpd_support_run_state(record, ds, "dataset-b")$reason,
    "foreign_dataset"
  )
  expect_identical(
    stpd_support_run_state(record, ds, "dataset-a", parameters = list(alpha = 2))$reason,
    "support_parameters_changed"
  )
  expect_identical(
    stpd_support_run_state(record, ds, "dataset-a", selected_trains = "another-train")$reason,
    "train_scope_changed"
  )
  expect_identical(
    stpd_support_run_state(record, ds, "dataset-a", reference_definition = list(group = "B"))$reason,
    "reference_definition_changed"
  )
  ds$trains[[trains]]$timestamp_sec[1] <- ds$trains[[trains]]$timestamp_sec[1] + 1e-5
  expect_identical(
    stpd_support_run_state(record, ds, "dataset-a")$reason,
    "spike_data_changed"
  )
})

test_that("RGS group tables require exact non-duplicated train coverage", {
  trains <- c("t1", "t2")
  good <- data.frame(train = trains, reference_group = c("A", "B"))
  expect_identical(
    unname(stpd_support_validate_group_table(good, trains)), c("A", "B")
  )
  expect_error(
    stpd_support_validate_group_table(good[c(1, 1), ], trains),
    "unique"
  )
  expect_error(
    stpd_support_validate_group_table(good[1, ], trains),
    "scope mismatch"
  )
})

test_that("support ZIP uses a unique workspace and validates required files", {
  exporter <- function(support, out_dir) {
    utils::write.csv(data.frame(value = support), file.path(out_dir, "result.csv"), row.names = FALSE)
  }
  files <- replicate(2L, tempfile(fileext = ".zip"))
  checked <- lapply(seq_along(files), function(index) {
    stpd_support_write_validated_zip(
      file = files[index],
      support = index,
      exporter = exporter,
      prefix = "support-test",
      readme_name = "README.txt",
      readme_lines = "support test",
      required_files = "result.csv"
    )
  })
  expect_true(all(vapply(checked, `[[`, logical(1), "valid")))
  expect_true(all(vapply(
    checked, function(item) all(c("result.csv", "README.txt") %in% item$members), logical(1)
  )))
  expect_true(all(file.exists(files)))
})

test_that("metadata grouping requires one stable value per train", {
  ds <- stpd_golden_test_dataset("middle_burst")
  trains <- names(ds$trains)
  for (train in trains) ds$trains[[train]]$recording_group <- "group-a"
  groups <- stpd_support_groups_from_metadata(ds, trains, "recording_group")
  expect_identical(unname(groups), rep("group-a", length(trains)))

  ds$trains[[trains[1]]]$recording_group[1] <- "group-b"
  expect_error(
    stpd_support_groups_from_metadata(ds, trains, "recording_group"),
    "exactly one group value"
  )
})
