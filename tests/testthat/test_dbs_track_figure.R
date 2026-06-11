test_that("DBS track preparation keeps only side-depth trains and windows spikes", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_GPi",
    source = "raw",
    trains = list(
      "LT1D4.885_SPK 01a" = make_train(c(0, 0.10, 0.20, 0.60)),
      "RT1D2.646_SPK 01a" = make_train(c(0, 0.05, 0.55, 0.70)),
      "SPK 01a" = make_train(c(0, 0.20, 0.40))
    ),
    unit_in = "s"
  )

  prep <- stpd_dbs_track_prepare(ds, start_sec = 0, window_sec = 0.5)

  expect_equal(nrow(prep$rows), 2L)
  expect_setequal(as.character(prep$rows$side), c("L", "R"))
  expect_false("SPK 01a" %in% as.character(prep$rows$train))
  expect_equal(sum(prep$rows$n_spikes_window), 5L)
  expect_true(all(prep$spikes$spike_time_rel >= 0 & prep$spikes$spike_time_rel <= 1))
})

test_that("DBS track preparation tolerates trains with no spikes in the displayed window", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_GPe",
    source = "raw",
    trains = list(
      "LT1D7.051_SPK 01b" = make_train(c(0, 1.05, 1.20)),
      "RT1D6.017_SPK 01a" = make_train(c(0, 0.10, 0.20))
    ),
    unit_in = "s"
  )

  prep <- stpd_dbs_track_prepare(ds, structures = "GPe", start_sec = 1, window_sec = 0.5)

  expect_equal(nrow(prep$rows), 2L)
  expect_equal(nrow(prep$spikes), 2L)
  expect_setequal(as.character(prep$spikes$train), "LT1D7.051_SPK 01b")
  expect_error(
    plotly::plotly_build(stpd_dbs_track_plotly_dot_model(prep, show_anatomical_context = TRUE)),
    NA
  )
})

test_that("DBS track preparation can filter side and structure", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list(
      "LT1D1.000_SPK 01a" = make_train(c(0, 0.10, 0.20)),
      "RT1D2.000_SPK 01a" = make_train(c(0, 0.10, 0.20))
    ),
    unit_in = "s"
  )

  prep <- stpd_dbs_track_prepare(ds, structures = "STN", sides = "R", start_sec = 0, window_sec = 0.2)

  expect_equal(nrow(prep$rows), 1L)
  expect_equal(as.character(prep$rows$side), "R")
  expect_equal(as.character(prep$rows$structure), "STN")
})

test_that("DBS track view can combine multiple loaded target datasets without train-name collisions", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  train_name <- "RT1D01.00_fon_nw_minus_7_08_minus_1_1"
  gpe_ds <- make_dataset(
    name = "Patient01_GPe",
    source = "raw",
    trains = stats::setNames(list(make_train(c(0, 0.05, 0.11))), train_name),
    unit_in = "s"
  )
  gpi_ds <- make_dataset(
    name = "Patient01_GPi",
    source = "raw",
    trains = stats::setNames(list(make_train(c(0, 0.07, 0.15))), train_name),
    unit_in = "s"
  )
  stn_ds <- make_dataset(
    name = "Patient01_STN",
    source = "raw",
    trains = list("LT1D00.50_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.06, 0.13))),
    unit_in = "s"
  )

  combined <- stpd_dbs_track_combine_datasets(
    list(gpe = gpe_ds, gpi = gpi_ds, stn = stn_ds),
    ids = c("gpe", "gpi", "stn")
  )
  prep <- stpd_dbs_track_prepare(
    combined,
    structures = c("GPe", "GPi", "STN"),
    sides = c("L", "R"),
    start_sec = 0,
    window_sec = 0.2
  )

  expect_equal(length(combined$trains), 3L)
  expect_equal(length(unique(names(combined$trains))), 3L)
  expect_equal(nrow(prep$rows), 3L)
  expect_setequal(as.character(prep$rows$structure), c("GPe", "GPi", "STN"))
  expect_true(all(c("dataset", "source_train") %in% names(prep$rows)))
  expect_true(all(grepl("Patient01_", as.character(prep$rows$dataset), fixed = TRUE)))
  expect_true(any(duplicated(as.character(prep$rows$source_train))))
  expect_false(any(duplicated(as.character(prep$rows$train))))
})

test_that("DBS dot-node layout maps each structure's depth range to its own nucleus", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  make_depth_trains <- function(depths) {
    stats::setNames(
      lapply(depths, function(dd) make_train(c(0, 0.05, 0.12, 0.18))),
      sprintf("LT1D%05.2f_fon_nw_minus_7_08_minus_1_1", depths)
    )
  }
  gpe_ds <- make_dataset(
    name = "Patient01_GPe",
    source = "raw",
    trains = make_depth_trains(c(1, 2, 3)),
    unit_in = "s"
  )
  gpi_ds <- make_dataset(
    name = "Patient01_GPi",
    source = "raw",
    trains = make_depth_trains(c(8, 9, 10)),
    unit_in = "s"
  )
  combined <- stpd_dbs_track_combine_datasets(
    list(gpe = gpe_ds, gpi = gpi_ds),
    ids = c("gpe", "gpi")
  )
  prep <- stpd_dbs_track_prepare(
    combined,
    structures = c("GPe", "GPi"),
    sides = "L",
    start_sec = 0,
    window_sec = 0.2
  )
  layout_data <- stpd_dbs_track_static_layout_data(prep, show_anatomical_context = TRUE)

  for (structure_name in c("GPe", "GPi")) {
    rows <- layout_data$rows[layout_data$rows$side == "L" & layout_data$rows$structure == structure_name, , drop = FALSE]
    shape <- layout_data$shape_df[layout_data$shape_df$side == "L" & layout_data$shape_df$structure == structure_name, , drop = FALSE]
    expect_equal(nrow(rows), 3L)
    expect_gt(nrow(shape), 20L)
    expect_equal(max(rows$depth_y, na.rm = TRUE), max(shape$y, na.rm = TRUE), tolerance = 1e-6)
    expect_equal(min(rows$depth_y, na.rm = TRUE), min(shape$y, na.rm = TRUE), tolerance = 1e-6)
    expect_lte(max(rows$lane_y, na.rm = TRUE), max(shape$y, na.rm = TRUE) + 1e-6)
    expect_gte(min(rows$lane_y, na.rm = TRUE), min(shape$y, na.rm = TRUE) - 1e-6)
  }

  gpe_bottom <- min(layout_data$rows$depth_y[layout_data$rows$structure == "GPe"], na.rm = TRUE)
  gpi_top <- max(layout_data$rows$depth_y[layout_data$rows$structure == "GPi"], na.rm = TRUE)
  expect_lt(abs(gpe_bottom - gpi_top), diff(range(layout_data$depth_bottom, layout_data$depth_top)) * 0.55)
  expect_setequal(as.character(layout_data$tick_df$structure), c("GPe", "GPi"))
})

test_that("DBS track preparation shows all trains by default and can cap per side", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  trains <- list()
  for (ii in seq_len(3)) trains[[paste0("LT1D0", ii, ".00_fon_nw_minus_7_08_minus_1_", ii)]] <- make_train(c(0, 0.1))
  for (ii in seq_len(4)) trains[[paste0("RT1D0", ii, ".00_fon_nw_minus_7_08_minus_1_", ii)]] <- make_train(c(0, 0.1))
  ds <- make_dataset(name = "Example_STN", source = "raw", trains = trains, unit_in = "s")

  prep_all <- stpd_dbs_track_prepare(ds, structures = "STN")
  prep_cap <- stpd_dbs_track_prepare(ds, structures = "STN", max_trains_per_side = 2)

  expect_equal(nrow(prep_all$rows), 7L)
  expect_equal(nrow(prep_cap$rows), 4L)
})

test_that("DBS track preparation builds pattern segments for overlays", {
  dat <- data.frame(
    idx = 1:4,
    timestamp_sec = c(0, 0.05, 0.10, 0.25),
    ISI_sec = c(NA_real_, 0.05, 0.05, 0.15),
    pattern_manual = c("", "", "tonic", ""),
    pattern_auto = c("", "burst", "burst", ""),
    stringsAsFactors = FALSE
  )
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list("LT1D01.00_fon_nw_minus_7_08_minus_1_1" = dat),
    unit_in = "s"
  )

  prep <- stpd_dbs_track_prepare(ds, structures = "STN", window_sec = 0.3, pattern_mode = "final")

  expect_equal(sort(unique(as.character(prep$pattern_segments$pattern))), c("burst", "tonic"))
  expect_true(all(prep$pattern_segments$start_rel >= 0 & prep$pattern_segments$end_rel <= 1))
  expect_true("manual" %in% as.character(prep$pattern_segments$source_kind))
  expect_true("auto" %in% as.character(prep$pattern_segments$source_kind))

  pb <- plotly::plotly_build(stpd_dbs_track_plotly(prep))
  trace_names <- unlist(lapply(pb$x$data, function(tr) tr$name %||% character(0)), use.names = FALSE)
  expect_true(any(grepl("AUTO burst", trace_names, fixed = TRUE)))
  expect_true(any(grepl("MANUAL tonic", trace_names, fixed = TRUE)))
})

test_that("DBS track separates same-depth units into nearby display lanes", {
  display_lanes <- stpd_dbs_track_display_lanes(
    side = c("R", "R", "R"),
    depth_y = c(0.20, 0.20, 0.05)
  )
  expect_equal(length(unique(display_lanes)), 3L)
  expect_true(all(diff(sort(display_lanes)) >= 0.23))

  make_train <- function(pattern) {
    data.frame(
      idx = 1:4,
      timestamp_sec = c(0, 0.05, 0.10, 0.20),
      ISI_sec = c(NA_real_, 0.05, 0.05, 0.10),
      pattern_manual = rep("", 4),
      pattern_auto = c("", pattern, "", ""),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Grechishnikova_STN_2017.csv",
    source = "raw",
    trains = list(
      "RT1D-0.15_fon1_1_nw_minus_7_08_minus_1_1" = make_train("pause"),
      "RT1D-0.15_fon1_2_nw_minus_7_08_minus_1_2" = make_train("high_frequency_spiking")
    ),
    unit_in = "s"
  )

  prep <- stpd_dbs_track_prepare(ds, structures = "STN", window_sec = 0.2, pattern_mode = "auto")
  expect_equal(nrow(prep$rows), 2L)

  pb <- plotly::plotly_build(stpd_dbs_track_plotly(prep))
  get_trace_y <- function(trace_name) {
    ys <- lapply(pb$x$data, function(tr) {
      nm <- tr$name %||% ""
      if (!identical(nm, trace_name)) return(numeric(0))
      unique(as.numeric(unlist(tr$y)))
    })
    unlist(ys, use.names = FALSE)
  }
  pause_y <- get_trace_y("AUTO pause")
  hfs_y <- get_trace_y("AUTO high_frequency_spiking")

  expect_length(pause_y, 1L)
  expect_length(hfs_y, 1L)
  expect_false(isTRUE(all.equal(pause_y, hfs_y)))
  expect_gt(abs(pause_y - hfs_y), 0.23)
})

test_that("DBS track raster lanes are extended 1.5x while keeping near nucleus anchors", {
  bounds <- stpd_dbs_track_raster_bounds(c("L", "R"), length_multiplier = 1.5)

  expect_equal(bounds$x1 - bounds$x0, rep(3.3, 2), tolerance = 1e-8)
  expect_equal(bounds$x1[1], -2.05, tolerance = 1e-8)
  expect_equal(bounds$x0[2], 2.05, tolerance = 1e-8)
})

test_that("DBS final overlay uses the same min-ISI bridge rule as the main raster", {
  dat <- data.frame(
    idx = 1:5,
    timestamp_sec = c(0, 0.05, 0.0504, 0.10, 0.20),
    ISI_sec = c(NA_real_, 0.05, 0.0004, 0.0496, 0.10),
    pattern_manual = rep("", 5),
    pattern_auto = c("", "burst", "", "burst", ""),
    stringsAsFactors = FALSE
  )
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list("LT1D01.00_fon_nw_minus_7_08_minus_1_1" = dat),
    unit_in = "s"
  )

  prep_bridge <- stpd_dbs_track_prepare(ds, structures = "STN", window_sec = 0.2, pattern_mode = "final", min_isi_sec = 0.001)
  prep_no_bridge <- stpd_dbs_track_prepare(ds, structures = "STN", window_sec = 0.2, pattern_mode = "final", min_isi_sec = 0)

  expect_true(any(prep_bridge$pattern_segments$idx == 3L & prep_bridge$pattern_segments$pattern == "burst"))
  expect_false(any(prep_no_bridge$pattern_segments$idx == 3L & prep_no_bridge$pattern_segments$pattern == "burst"))
})

test_that("Grechishnikova STN-style column names are parsed as first-class metadata", {
  meta <- parse_spike_train_column_metadata(
    c(
      "LT1D00.83_fon1_1_nw_minus_7_08_minus_1_1",
      "LT1D01.96_fon_nw_minus_7_08_minus_1_1",
      "RT2D03.41_fon_nw_minus_7_08_minus_1_3"
    ),
    dataset_name = "Grechishnikova_STN_2017.csv"
  )

  expect_true(all(meta$parse_ok))
  expect_equal(as.character(meta$structure), rep("STN", 3))
  expect_equal(as.character(meta$side), c("L", "L", "R"))
  expect_equal(as.character(meta$trajectory), c("T1", "T1", "T2"))
  expect_equal(meta$recording_depth, c(0.83, 1.96, 3.41))
  expect_equal(as.character(meta$channel_type), rep("fon", 3))
})

test_that("DBS track depth scale uses the full selected dataset reference", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list(
      "LT1D00.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.10)),
      "LT1D05.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.10)),
      "LT1D10.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.10))
    ),
    unit_in = "s"
  )

  prep <- stpd_dbs_track_prepare(
    ds,
    structures = "STN",
    sides = "L",
    selected_trains = "LT1D05.00_fon_nw_minus_7_08_minus_1_1"
  )

  expect_equal(nrow(prep$rows), 1L)
  expect_equal(sort(unique(prep$depth_reference$recording_depth)), c(0, 5, 10))
  expect_equal(
    stpd_dbs_track_rescale_depth(5, reference_depth = prep$depth_reference$recording_depth),
    1.15 - 0.5 * 2.45
  )
})

test_that("DBS target nucleus view maps depth endpoints to separate enlarged nucleus bounds", {
  bounds <- stpd_dbs_track_focus_bounds()
  y <- stpd_dbs_track_rescale_depth(
    c(0, 10),
    reference_depth = c(0, 10),
    y_top = bounds[["top"]],
    y_bottom = bounds[["bottom"]]
  )
  expect_equal(unname(y), unname(c(bounds[["top"]], bounds[["bottom"]])))

  left <- stpd_dbs_track_focus_nucleus_shape("STN", side_x = stpd_dbs_track_focus_side_x("L"))
  right <- stpd_dbs_track_focus_nucleus_shape("STN", side_x = stpd_dbs_track_focus_side_x("R"))

  expect_equal(range(left$points$y), unname(c(bounds[["bottom"]], bounds[["top"]])), tolerance = 1e-8)
  expect_equal(range(right$points$y), unname(c(bounds[["bottom"]], bounds[["top"]])), tolerance = 1e-8)
  expect_lt(max(left$points$x), min(right$points$x))
})

test_that("DBS electrode axes and sampled depth sites stay inside every target nucleus envelope", {
  point_in_or_on_polygon <- function(x, y, polygon, tol = 1e-8) {
    px <- as.numeric(polygon$x)
    py <- as.numeric(polygon$y)
    if (!identical(px[1], px[length(px)]) || !identical(py[1], py[length(py)])) {
      px <- c(px, px[1])
      py <- c(py, py[1])
    }
    inside <- FALSE
    for (ii in seq_len(length(px) - 1L)) {
      x1 <- px[ii]; x2 <- px[ii + 1L]
      y1 <- py[ii]; y2 <- py[ii + 1L]
      cross <- (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)
      if (abs(cross) <= tol &&
          x >= min(x1, x2) - tol && x <= max(x1, x2) + tol &&
          y >= min(y1, y2) - tol && y <= max(y1, y2) + tol) {
        return(TRUE)
      }
      intersects <- ((y1 > y) != (y2 > y)) &&
        (x < (x2 - x1) * (y - y1) / ((y2 - y1) + .Machine$double.eps) + x1)
      if (isTRUE(intersects)) inside <- !inside
    }
    inside
  }

  bounds <- stpd_dbs_track_focus_bounds()
  y_sites <- seq(bounds[["bottom"]], bounds[["top"]], length.out = 11)
  trajectory_offsets <- stpd_dbs_track_trajectory_offsets(
    c("T1", "T2"),
    levels = c("T1", "T2"),
    span = 0.18
  )
  for (target in c("GPe", "GPi", "STN")) {
    for (side in c("L", "R")) {
      side_x <- stpd_dbs_track_focus_side_x(side)
      electrode_x <- side_x + trajectory_offsets
      shapes <- list(
        focus = stpd_dbs_track_focus_nucleus_shape(target, side_x = side_x),
        context_target = stpd_dbs_track_context_model_shape(
          target,
          target_structure = target,
          side_x = side_x,
          y_top = bounds[["top"]],
          y_bottom = bounds[["bottom"]]
        )
      )
      for (shape in shapes) {
        for (xx in electrode_x) {
          expect_true(all(vapply(y_sites, function(yy) point_in_or_on_polygon(xx, yy, shape$points), logical(1))))
        }
      }
    }
  }
})

test_that("DBS anatomical context keeps left and right STN references separated", {
  bounds <- stpd_dbs_track_focus_bounds()

  for (target in c("GPe", "GPi", "STN")) {
    left <- stpd_dbs_track_context_model_shape(
      "STN",
      target_structure = target,
      side_x = stpd_dbs_track_focus_side_x("L"),
      y_top = bounds[["top"]],
      y_bottom = bounds[["bottom"]]
    )$points
    right <- stpd_dbs_track_context_model_shape(
      "STN",
      target_structure = target,
      side_x = stpd_dbs_track_focus_side_x("R"),
      y_top = bounds[["top"]],
      y_bottom = bounds[["bottom"]]
    )$points

    expect_lt(max(left$x, na.rm = TRUE), min(right$x, na.rm = TRUE))
    expect_lt(mean(left$x, na.rm = TRUE), 0)
    expect_gt(mean(right$x, na.rm = TRUE), 0)
  }
})

test_that("DBS trajectory offsets stay stable when one trajectory is displayed", {
  offsets <- stpd_dbs_track_trajectory_offsets("T2", levels = c("T1", "T2"))
  expect_gt(offsets, 0)
  expect_equal(stpd_dbs_track_trajectory_offsets("T2", levels = "T2"), 0)
})

test_that("DBS track plot draws only represented nuclei unless context is requested", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list(
      "LT1D1.000_SPK 01a" = make_train(c(0, 0.10, 0.20)),
      "RT1D2.000_SPK 01a" = make_train(c(0, 0.10, 0.20))
    ),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "STN", start_sec = 0, window_sec = 0.2)

  plot_default <- plotly::plotly_build(stpd_dbs_track_plotly(prep, show_anatomical_context = FALSE))
  default_text <- unlist(lapply(plot_default$x$data, function(tr) tr$text %||% character(0)), use.names = FALSE)

  expect_true("STN" %in% default_text)
  expect_false("GPe" %in% default_text)
  expect_false("GPi" %in% default_text)

  plot_context <- plotly::plotly_build(stpd_dbs_track_plotly(prep, show_anatomical_context = TRUE))
  context_text <- unlist(lapply(plot_context$x$data, function(tr) tr$text %||% character(0)), use.names = FALSE)

  expect_true(all(c("GPe", "GPi", "STN") %in% context_text))
})

test_that("DBS nucleus shapes preserve basal-ganglia relative anatomy", {
  polygon_area <- function(points) {
    points <- rbind(points, points[1, , drop = FALSE])
    x <- points$x
    y <- points$y
    abs(sum(x[-1] * y[-length(y)] - x[-length(x)] * y[-1])) / 2
  }
  gpe <- stpd_dbs_track_nucleus_shape("GPe", side_x = 0.72)$points
  gpi <- stpd_dbs_track_nucleus_shape("GPi", side_x = 0.72)$points
  stn <- stpd_dbs_track_nucleus_shape("STN", side_x = 0.72)$points
  left_gpe <- stpd_dbs_track_nucleus_shape("GPe", side_x = -0.72)$points
  bounds <- stpd_dbs_track_focus_bounds()
  context_gpe <- stpd_dbs_track_context_nucleus_shape(
    "GPe",
    side_x = stpd_dbs_track_focus_side_x("R"),
    y_top = bounds[["top"]],
    y_bottom = bounds[["bottom"]]
  )$points
  context_gpi <- stpd_dbs_track_context_nucleus_shape(
    "GPi",
    side_x = stpd_dbs_track_focus_side_x("R"),
    y_top = bounds[["top"]],
    y_bottom = bounds[["bottom"]]
  )$points
  focus_gpe <- stpd_dbs_track_focus_nucleus_shape("GPe", side_x = stpd_dbs_track_focus_side_x("R"))$points
  focus_gpi <- stpd_dbs_track_focus_nucleus_shape("GPi", side_x = stpd_dbs_track_focus_side_x("R"))$points
  context_ratio <- polygon_area(context_gpe) / polygon_area(context_gpi)

  expect_gt(nrow(gpe), 40L)
  expect_gt(diff(range(gpe$x)), diff(range(gpi$x)))
  expect_gt(diff(range(gpi$x)), diff(range(stn$x)))
  expect_lt(mean(gpi$x), mean(gpe$x))
  expect_lt(mean(stn$y), mean(gpi$y))
  expect_lt(mean(left_gpe$x), 0)
  expect_equal(abs(mean(left_gpe$x)), mean(gpe$x), tolerance = 0.03)
  expect_gt(context_ratio, 1.25)
  expect_lt(context_ratio, 2.10)
  expect_gt(mean(context_gpe$y), mean(context_gpi$y))
  expect_gt(mean(context_gpe$x), mean(context_gpi$x))
  expect_gt(diff(range(focus_gpe$x)), diff(range(focus_gpi$x)) * 1.15)
})

test_that("DBS nucleus palette separates GPe warm color from GPi cool color", {
  gpe_rgb <- grDevices::col2rgb(stpd_dbs_track_dot_base_color("GPe"))[, 1]
  gpi_rgb <- grDevices::col2rgb(stpd_dbs_track_dot_base_color("GPi"))[, 1]

  expect_gt(unname(gpe_rgb["red"]), unname(gpe_rgb["blue"]))
  expect_gt(unname(gpe_rgb["red"]), unname(gpe_rgb["green"]))
  expect_gt(unname(gpi_rgb["blue"]), unname(gpi_rgb["red"]))
  expect_match(stpd_dbs_track_3d_color("GPe", 0.5), "rgba\\(240,90,40,0.500\\)")
  expect_equal(stpd_dbs_track_structure_label_color("GPe"), "#c2410c")
  expect_equal(stpd_dbs_track_structure_label_color("GPi"), "#2563eb")
  expect_equal(stpd_dbs_track_structure_label_color("STN"), "#16a34a")
})

test_that("DBS atlas context scales the standard GPe-GPi-STN model to the target nucleus", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_GPi",
    source = "raw",
    trains = list("RT1D02.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.08, 0.16))),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "GPi", sides = "R", start_sec = 0, window_sec = 0.2)
  layout_data <- stpd_dbs_track_static_layout_data(prep, show_anatomical_context = TRUE)
  shape_df <- layout_data$shape_df
  bounds <- stpd_dbs_track_focus_bounds()
  gpe <- shape_df[shape_df$side == "R" & shape_df$structure == "GPe", , drop = FALSE]
  gpi <- shape_df[shape_df$side == "R" & shape_df$structure == "GPi", , drop = FALSE]
  stn <- shape_df[shape_df$side == "R" & shape_df$structure == "STN", , drop = FALSE]
  depth_span <- diff(range(unname(bounds)))

  expect_gt(nrow(gpe), 20L)
  expect_gt(nrow(gpi), 20L)
  expect_gt(nrow(stn), 20L)
  expect_true(all(unique(gpi$active)))
  expect_false(any(unique(gpe$active)))
  expect_false(any(unique(stn$active)))
  expect_gte(max(gpe$fill_alpha), 0.20)
  expect_gte(max(stn$fill_alpha), 0.14)
  expect_lt(max(gpe$fill_alpha), max(gpi$fill_alpha))
  expect_equal(tail(stpd_dbs_track_draw_groups(shape_df), 1), "R_GPi")
  expect_equal(diff(range(gpi$y)), depth_span, tolerance = 1e-6)
  expect_lt(diff(range(stn$y)), diff(range(gpi$y)))
  expect_lt(max(stn$fill_alpha), max(gpe$fill_alpha))
  expect_gt(mean(gpe$x), mean(gpi$x))
  expect_gt(mean(gpe$y), mean(gpi$y))
  expect_lt(mean(stn$y), mean(gpi$y))
})

test_that("DBS dot-node atlas labels each structure once at the shared model center", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_GPi",
    source = "raw",
    trains = list(
      "LT1D02.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.08, 0.16)),
      "RT1D02.00_fon_nw_minus_7_08_minus_1_2" = make_train(c(0, 0.08, 0.16))
    ),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "GPi", start_sec = 0, window_sec = 0.2)
  layout_data <- stpd_dbs_track_static_layout_data(prep, show_anatomical_context = TRUE)
  labels <- layout_data$nucleus_label_df

  expect_setequal(as.character(labels$label), c("GPe", "GPi", "STN"))
  expect_equal(anyDuplicated(as.character(labels$label)), 0L)
  expect_true(all(as.character(labels$side) == "center"))
  expect_true(labels$active[match("GPi", labels$label)])
  expect_true(abs(labels$x[match("GPi", labels$label)]) < 0.05)
})

test_that("DBS animated particles make larger random moves but remain inside nucleus outline", {
  bounds <- stpd_dbs_track_focus_bounds()
  shape <- stpd_dbs_track_context_model_shape(
    "STN",
    target_structure = "STN",
    side_x = stpd_dbs_track_focus_side_x("R"),
    y_top = bounds[["top"]],
    y_bottom = bounds[["bottom"]],
    n = 180
  )
  dots <- stpd_dbs_track_nucleus_dot_cloud(
    shape$points,
    structure = "STN",
    side = "R",
    active = TRUE
  )
  dots <- dots[seq_len(min(120L, nrow(dots))), , drop = FALSE]
  flow <- stpd_dbs_track_dot_flow_frames(dots, shape$points, n_frames = 24L, amplitude = 0.12)
  base <- dots[match(flow$seed_id, dots$seed_id), , drop = FALSE]
  displacement <- sqrt((flow$x - base$x)^2 + (flow$y - base$y)^2)
  centroid_x <- mean(range(shape$points$x, na.rm = TRUE))
  centroid_y <- mean(range(shape$points$y, na.rm = TRUE))
  radial_norm <- pmax(sqrt((centroid_x - base$x)^2 + (centroid_y - base$y)^2), 1e-8)
  radial_shift <- ((flow$x - base$x) * (centroid_x - base$x) +
                     (flow$y - base$y) * (centroid_y - base$y)) / radial_norm

  expect_equal(nrow(flow), nrow(dots) * 24L)
  expect_true(all(stpd_dbs_track_point_in_polygon(flow$x, flow$y, shape$points)))
  expect_true(all(stpd_dbs_track_point_in_polygon(flow$dot_shadow_x, flow$dot_shadow_y, shape$points)))
  expect_true(all(stpd_dbs_track_point_in_polygon(flow$dot_side_x, flow$dot_side_y, shape$points)))
  expect_true(all(stpd_dbs_track_point_in_polygon(flow$dot_inner_x, flow$dot_inner_y, shape$points)))
  expect_gt(max(displacement, na.rm = TRUE), 0.10)
  expect_lt(abs(mean(radial_shift, na.rm = TRUE)), 0.025)
  expect_gt(length(unique(flow$flow_frame)), 20L)
})

test_that("DBS standard atlas model fits each parsed target nucleus to the depth axis", {
  bounds <- stpd_dbs_track_focus_bounds()
  depth_span <- diff(range(unname(bounds)))
  side_x <- stpd_dbs_track_focus_side_x("R")

  for (target in c("GPe", "GPi", "STN")) {
    target_shape <- stpd_dbs_track_context_model_shape(
      target,
      target_structure = target,
      side_x = side_x,
      y_top = bounds[["top"]],
      y_bottom = bounds[["bottom"]],
      n = 180
    )
    expect_equal(diff(range(target_shape$points$y)), depth_span, tolerance = 1e-6)
    expect_lt(min(target_shape$points$x, na.rm = TRUE), side_x)
    expect_gt(max(target_shape$points$x, na.rm = TRUE), side_x)
  }
})

test_that("DBS dot-node shell model is 2D pseudo-3D rather than true 3D", {
  make_train <- function(ts, pattern = "") {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = c("", rep(pattern, max(0L, length(ts) - 1L))),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list("RT1D01.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.05, 0.12), "burst")),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "STN", sides = "R", start_sec = 0, window_sec = 0.2)
  pb <- plotly::plotly_build(stpd_dbs_track_plotly_dot_model(prep, show_anatomical_context = FALSE))
  trace_types <- unlist(lapply(pb$x$data, function(tr) tr$type %||% character(0)), use.names = FALSE)
  trace_modes <- unlist(lapply(pb$x$data, function(tr) tr$mode %||% character(0)), use.names = FALSE)
  trace_names <- unlist(lapply(pb$x$data, function(tr) tr$name %||% character(0)), use.names = FALSE)
  trace_text <- unlist(lapply(pb$x$data, function(tr) tr$text %||% character(0)), use.names = FALSE)

  expect_false(any(trace_types %in% c("mesh3d", "scatter3d")))
  expect_true(any(trace_types == "scatter" & trace_modes == "markers"))
  expect_true(any(grepl("RT STN shell", trace_names, fixed = TRUE)))
  expect_true(any(grepl("2.5D translucent dot-node shell", trace_text, fixed = TRUE)))
  expect_true(any(grepl("<b>STN</b>", trace_text, fixed = TRUE)))
  expect_equal(pb$x$layout$dragmode, "pan")
  expect_false(isTRUE(pb$x$layout$xaxis$fixedrange))
  expect_false(isTRUE(pb$x$layout$yaxis$fixedrange))
  expect_null(pb$x$layout$yaxis$scaleanchor)
  expect_length(pb$x$layout$xaxis$range, 2L)
  expect_length(pb$x$layout$yaxis$range, 2L)
})

test_that("DBS 3D model renders target nucleus, electrode, spikes and optional context", {
  make_train <- function(ts, pattern = "") {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = c("", rep(pattern, max(0L, length(ts) - 1L))),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list("RT1D01.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.05, 0.12), "burst")),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "STN", sides = "R", start_sec = 0, window_sec = 0.2)

  target_plot <- plotly::plotly_build(stpd_dbs_track_plotly_3d(prep, show_anatomical_context = FALSE))
  target_types <- unlist(lapply(target_plot$x$data, function(tr) tr$type %||% character(0)), use.names = FALSE)
  target_text <- unlist(lapply(target_plot$x$data, function(tr) tr$text %||% character(0)), use.names = FALSE)
  expect_true("mesh3d" %in% target_types)
  expect_true("scatter3d" %in% target_types)
  expect_true(any(grepl("RT STN schematic", target_text, fixed = TRUE)))
  expect_false(any(grepl("RT GPe schematic", target_text, fixed = TRUE)))
  expect_false(any(grepl("RT GPi schematic", target_text, fixed = TRUE)))

  context_plot <- plotly::plotly_build(stpd_dbs_track_plotly_3d(prep, show_anatomical_context = TRUE))
  context_text <- unlist(lapply(context_plot$x$data, function(tr) tr$text %||% character(0)), use.names = FALSE)
  expect_true(any(grepl("RT GPe schematic", context_text, fixed = TRUE)))
  expect_true(any(grepl("RT GPi schematic", context_text, fixed = TRUE)))
})

test_that("DBS 3D model maps non-STN datasets to their parsed target nucleus", {
  make_train <- function(ts) {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = rep("", length(ts)),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_GPi",
    source = "raw",
    trains = list("RT1D02.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.08, 0.16))),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "GPi", sides = "R", start_sec = 0, window_sec = 0.2)
  pb <- plotly::plotly_build(stpd_dbs_track_plotly_3d(prep, show_anatomical_context = FALSE))
  trace_text <- unlist(lapply(pb$x$data, function(tr) tr$text %||% character(0)), use.names = FALSE)

  expect_true(any(grepl("RT GPi schematic", trace_text, fixed = TRUE)))
  expect_true(any(grepl("RT T1 GPi target electrode segment", trace_text, fixed = TRUE)))
  expect_false(any(grepl("RT STN schematic", trace_text, fixed = TRUE)))
})

test_that("DBS static paper figure exports through ggplot2", {
  testthat::skip_if_not_installed("ggplot2")
  make_train <- function(ts, pattern = "") {
    data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = rep("", length(ts)),
      pattern_auto = c("", rep(pattern, max(0L, length(ts) - 1L))),
      stringsAsFactors = FALSE
    )
  }
  ds <- make_dataset(
    name = "Example_STN",
    source = "raw",
    trains = list(
      "LT1D01.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.05, 0.11, 0.18), "burst"),
      "RT1D02.00_fon_nw_minus_7_08_minus_1_1" = make_train(c(0, 0.08, 0.16, 0.24), "pause")
    ),
    unit_in = "s"
  )
  prep <- stpd_dbs_track_prepare(ds, structures = "STN", start_sec = 0, window_sec = 0.25, pattern_mode = "auto")
  fig <- stpd_dbs_track_ggplot_static(prep, show_anatomical_context = FALSE)
  expect_s3_class(fig, "ggplot")

  out <- tempfile(fileext = ".png")
  ggplot2::ggsave(out, plot = fig, width = 6, height = 3.8, units = "in", dpi = 150, bg = "white")
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
})
