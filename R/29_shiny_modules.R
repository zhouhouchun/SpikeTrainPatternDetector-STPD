# Shiny module skeletons. These modules are deliberately small wrappers.
# They define the future boundary between UI and detector engine without forcing
# a risky one-step rewrite of the current app.

mod_import_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(
      ns("raw_file"), "\u4E0A\u4F20\u539F\u59CB timestamp CSV",
      buttonLabel = "\u6D4F\u89C8\u2026", placeholder = "\u672A\u9009\u62E9\u6587\u4EF6"
    ),
    shiny::radioButtons(ns("unit"), "\u8F93\u5165\u5355\u4F4D", choices = c("s", "ms"), selected = "s", inline = TRUE),
    shiny::selectInput(ns("duplicate_policy"), "\u91CD\u590D timestamp \u5904\u7406\u7B56\u7565", choices = c("error_keep", "warn_keep", "collapse_exact"), selected = "error_keep"),
    DT::DTOutput(ns("qc_table"))
  )
}

mod_import_qc_server <- function(id, params_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ds <- shiny::reactive({
      req(input$raw_file)
      build_spike_dataset(input$raw_file$datapath, mode = "raw", unit_in = input$unit, duplicate_policy = input$duplicate_policy)
    })
    qc <- shiny::reactive({
      p <- if (is.null(params_reactive)) default_params() else params_reactive()
      run_qc(ds(), p)
    })
    output$qc_table <- DT::renderDT(qc(), options = list(pageLength = 10, scrollX = TRUE))
    list(dataset = ds, qc = qc)
  })
}

mod_detector_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("run"), "\u8FD0\u884C\u68C0\u6D4B\u5668\u5F15\u64CE"),
    shiny::verbatimTextOutput(ns("run_summary"))
  )
}

mod_detector_server <- function(id, dataset_reactive, params_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::eventReactive(input$run, {
      p <- if (is.null(params_reactive)) default_params() else params_reactive()
      tryCatch(
        stpd_detect(dataset_reactive(), p),
        error = function(e) {
          msg <- if (exists("stpd_shiny_detector_error_message", mode = "function")) {
            stpd_shiny_detector_error_message(e)
          } else {
            paste0("\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25\u3002\u6280\u672F\u8BE6\u60C5\uFF1A", conditionMessage(e))
          }
          shiny::showNotification(msg, type = "error", duration = 15)
          shiny::validate(shiny::need(FALSE, msg))
        }
      )
    })
    output$run_summary <- shiny::renderPrint({
      req(result())
      result()$results$run_metadata_public
    })
    result
  })
}
