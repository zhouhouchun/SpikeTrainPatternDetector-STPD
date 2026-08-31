test_that("global raster overview depends directly on the time-window slider", {
  src <- paste(deparse(SpikeTrainPatternDetector:::stpd_server_install_visualization_module), collapse = "\n")
  expect_match(src, "output\\$raster_overview_plot <- renderPlotly")
  expect_match(src, "slider_window <- raster_slider_window_source\\(\\)")
  expect_match(src, "win <- slider_window %\\|\\|% raster_window_for_plot")
  expect_match(src, "output\\$raster_raw_overview_plot <- renderPlotly")
  expect_match(src, "raw_slider_window <- input\\$raw_xrange")
  expect_match(src, "dat\\$timestamp_sec")
})
