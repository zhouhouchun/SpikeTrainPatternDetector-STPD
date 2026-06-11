
#' Launch the Spike Train Pattern Detector Shiny app
#'
#' @param browser Whether to launch a browser window.
#' @param host Host for the local Shiny server.
#' @param port Optional port. Use NULL to let Shiny choose.
#' @export
launch_spike_detector <- function(browser = TRUE, host = "127.0.0.1", port = NULL) {
  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app, host = host, port = port, launch.browser = browser)
}

#' Return package architecture notes
#' @export
stpd_architecture_notes <- function() {
  c(
    "modular package: Shiny frontend separated from detector modules.",
    "Native C kernels accelerate ISI percentile and local-median cache calculation.",
    "Core detector behavior is preserved as reference behavior; future changes should be made through tests and git rather than user-facing version labels.",
    "Candidate generation, candidate features, final classification, ledger, evaluation, and export are separated into source modules."
  )
}
