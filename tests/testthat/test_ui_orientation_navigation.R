stpd_orientation_ui <- function() {
  SpikeTrainPatternDetector:::ui
}

stpd_orientation_query <- function(selector) {
  htmltools::tagQuery(stpd_orientation_ui())$find(selector)$selectedTags()
}

stpd_orientation_attr <- function(tags, name) {
  unname(vapply(tags, function(tag) {
    as.character(tag$attribs[[name]] %||% "")[[1]]
  }, character(1)))
}

stpd_orientation_tag_text <- function(tag) {
  text <- gsub("<[^>]*>", " ", paste(as.character(tag), collapse = ""))
  trimws(gsub("[[:space:]]+", " ", text))
}

stpd_orientation_server_source <- function() {
  namespace <- asNamespace("SpikeTrainPatternDetector")
  function_names <- c(
    "server",
    grep(
      "^stpd_server_install_",
      ls(namespace, all.names = TRUE),
      value = TRUE
    )
  )
  function_names <- unique(function_names[
    vapply(function_names, exists, logical(1), envir = namespace, inherits = FALSE)
  ])
  paste(
    unlist(lapply(function_names, function(name) {
      deparse(get(name, envir = namespace, inherits = FALSE), width.cutoff = 500L)
    })),
    collapse = "\n"
  )
}

stpd_orientation_script_source <- function(marker) {
  scripts <- stpd_orientation_query("script")
  sources <- vapply(scripts, function(tag) {
    paste(as.character(tag), collapse = "\n")
  }, character(1))
  paste(sources[grepl(marker, sources, fixed = TRUE)], collapse = "\n")
}

test_that("Orientation navigation represents every legacy main tab exactly once", {
  expected_tabs <- c(
    "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE",
    "\u539F\u59CB\u65F6\u95F4\u6233\u56FE",
    "ISI \u65F6\u95F4\u5256\u9762",
    "ISI \u72B6\u6001\u7A7A\u95F4",
    "\u72B6\u6001\u8F68\u8FF9",
    "\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8",
    "\u795E\u7ECF\u6D41\u5F62",
    "\u533A\u95F4\u76F4\u65B9\u56FE",
    "\u6570\u636E\u96C6 ISI \u76F4\u65B9\u56FE",
    "\u7ED3\u6784\u5019\u9009",
    "\u79CD\u5B50 / \u6865\u63A5\u8BCA\u65AD",
    "\u9608\u503C\u9884\u89C8",
    "\u591A\u7B97\u6CD5\u7EA0\u5BDF\u4E0E\u4EBA\u5DE5\u6807\u6CE8",
    "\u652F\u6301\u65B9\u6CD5",
    "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A",
    "\u79D1\u5B66\u9A8C\u8BC1",
    "\u6279\u5904\u7406 / API",
    "\u65B9\u6CD5 / \u5BA1\u8BA1\u8BF4\u660E",
    "\u68C0\u6D4B\u5668 / \u53C2\u6570",
    "\u81EA\u9002\u5E94 train \u8C03\u53C2",
    "\u6570\u636E QC",
    "\u4E8B\u4EF6 / \u8F93\u51FA"
  )

  nav <- stpd_orientation_query("#stpd_orientation_nav")
  expect_length(nav, 1L)
  expect_match(nav[[1]]$attribs$class %||% "", "stpd-orientation-nav", fixed = TRUE)

  nav_buttons <- htmltools::tagQuery(nav[[1]])$find("button")$selectedTags()
  nav_targets <- stpd_orientation_attr(nav_buttons, "data-stpd-tab-target")
  nav_targets <- nav_targets[nzchar(nav_targets)]

  expect_length(nav_targets, length(expected_tabs))
  expect_identical(anyDuplicated(nav_targets), 0L)
  expect_setequal(nav_targets, expected_tabs)

  main_tabs <- stpd_orientation_query("#main_tabs")
  expect_length(main_tabs, 1L)
  main_tabs_html <- paste(as.character(main_tabs[[1]]), collapse = "\n")
  expect_true(
    grepl(
      paste0(
        '<li class="active">[[:space:]]*<a[^>]+data-value="',
        "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE", '"'
      ),
      main_tabs_html,
      perl = TRUE
    )
  )
})

test_that("Orientation exposes eight coherent navigation groups", {
  labels <- stpd_orientation_query(".stpd-orientation-group-label")
  expect_length(labels, 8L)

  english <- unname(vapply(labels, function(tag) {
    SpikeTrainPatternDetector:::stpd_i18n_translate_text(
      stpd_orientation_tag_text(tag), "en"
    )
  }, character(1)))

  expect_identical(
    english,
    c(
      "Data", "Detect", "Explore", "Review", "Validate", "Export",
      "Analysis", "Expert / Diagnostics"
    )
  )
})

test_that("workflow cards navigate only and cannot trigger a detector run", {
  steps <- stpd_orientation_query(".workflow-step")
  expect_length(steps, 5L)
  expect_true(all(vapply(steps, function(tag) identical(tag$name, "button"), logical(1))))

  expected_ids <- c(
    "orientation_step_data", "orientation_step_params", "orientation_step_run",
    "orientation_step_validate", "orientation_step_export"
  )
  expected_targets <- c(
    "\u6570\u636E QC", "\u68C0\u6D4B\u5668 / \u53C2\u6570",
    "\u68C0\u6D4B\u5668 / \u53C2\u6570",
    "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A",
    "\u4E8B\u4EF6 / \u8F93\u51FA"
  )

  expect_identical(stpd_orientation_attr(steps, "id"), expected_ids)
  expect_identical(
    stpd_orientation_attr(steps, "data-stpd-tab-target"),
    expected_targets
  )
  expect_true(all(stpd_orientation_attr(steps, "type") == "button"))
  expect_true(all(stpd_orientation_attr(steps, "onclick") == ""))
  expect_true(all(stpd_orientation_attr(steps, "data-stpd-action") == ""))

  action_labels <- stpd_orientation_query(".workflow-step > .action-label")
  indices <- stpd_orientation_query(".workflow-index")
  expect_length(action_labels, 5L)
  expect_length(indices, 5L)
  expect_identical(
    unname(vapply(indices, stpd_orientation_tag_text, character(1))),
    as.character(seq_len(5L))
  )
  expect_true(all(stpd_orientation_attr(indices, "aria-hidden") == "true"))
  expect_identical(
    stpd_orientation_attr(steps, "aria-label"),
    stpd_ui_orientation_workflow()$label
  )
  short_labels <- stpd_orientation_query(".workflow-short")
  short_english <- unname(vapply(short_labels, function(tag) {
    SpikeTrainPatternDetector:::stpd_i18n_translate_text(
      stpd_orientation_tag_text(tag), "en"
    )
  }, character(1)))
  expect_identical(
    short_english,
    c("Data", "Params", "Detect", "Review", "Export")
  )

  navigation_script <- stpd_orientation_script_source("data-stpd-tab-target")
  expect_true(nzchar(navigation_script))
  expect_match(navigation_script, "setActive(target)", fixed = TRUE)
  expect_no_match(navigation_script, "run_detector", fixed = TRUE)
  expect_no_match(navigation_script, "Shiny.setInputValue", fixed = TRUE)
})

test_that("the Detector page exposes one primary pattern-detection action", {
  run_card <- stpd_orientation_query("#stpd_run_controls")
  run_buttons <- stpd_orientation_query("#stpd_run_controls #run_detector")
  scope_controls <- stpd_orientation_query(
    "#stpd_run_controls #detector_selected_only"
  )
  status_outputs <- stpd_orientation_query(
    "#stpd_run_controls #ui_run_state_status"
  )

  expect_length(run_card, 1L)
  expect_length(run_buttons, 1L)
  expect_length(scope_controls, 1L)
  expect_length(status_outputs, 1L)
  expect_length(stpd_orientation_query("#run_detector"), 1L)
  expect_length(stpd_orientation_query(".sidebar-fixed #run_detector"), 0L)
  expect_length(
    stpd_orientation_query("#stpd_detect_controls #estimate_apply_manual_params"),
    1L
  )
  expect_match(
    paste(as.character(run_buttons[[1]]), collapse = "\n"),
    "btn-primary", fixed = TRUE
  )
  expect_identical(
    stpd_orientation_attr(run_buttons, "aria-label"),
    "\u5F00\u59CB\u6A21\u5F0F\u68C0\u6D4B"
  )

  workflow <- SpikeTrainPatternDetector:::stpd_ui_orientation_workflow()
  expect_identical(
    workflow$control_anchor[workflow$step_id == "orientation_step_params"],
    "stpd_detect_controls"
  )
  expect_identical(
    workflow$control_anchor[workflow$step_id == "orientation_step_run"],
    "stpd_run_controls"
  )

  focus_script <- stpd_orientation_script_source("stpd-orientation-focus")
  expect_match(
    focus_script, "requestedAnchor.closest('.sidebar-fixed')", fixed = TRUE
  )

  server_source <- stpd_orientation_server_source()
  expect_match(server_source, "observeEvent(input$run_detector", fixed = TRUE)
  expect_match(server_source, "ignoreInit = TRUE", fixed = TRUE)
})

test_that("persistent context distinguishes viewing scope from detector run scope", {
  context <- stpd_orientation_query("#orientation_context_bar")
  expect_length(context, 1L)

  server_source <- stpd_orientation_server_source()
  expect_true(grepl("orientation_context_bar", server_source, fixed = TRUE))
  expect_true(grepl("data-viewing-scope", server_source, fixed = TRUE))
  expect_true(grepl("data-detected-scope", server_source, fixed = TRUE))
  expect_true(grepl("\u6B63\u5728\u67E5\u770B", server_source, fixed = TRUE))
  expect_true(grepl("\u4E0A\u6B21\u68C0\u6D4B", server_source, fixed = TRUE))

  viewing <- SpikeTrainPatternDetector:::stpd_i18n_translate_text(
    "\u6B63\u5728\u67E5\u770B\uFF1A3/10 trains", "en"
  )
  detected <- SpikeTrainPatternDetector:::stpd_i18n_translate_text(
    "\u4E0A\u6B21\u68C0\u6D4B\uFF1A8/10 trains", "en"
  )
  expect_false(SpikeTrainPatternDetector:::stpd_i18n_contains_cjk(viewing))
  expect_false(SpikeTrainPatternDetector:::stpd_i18n_contains_cjk(detected))
  expect_false(identical(viewing, detected))
})

test_that("notification history is bounded and copies notification text safely", {
  expected_ids <- c(
    "stpd_notification_history_toggle", "stpd_notification_history_count",
    "stpd_notification_history_panel", "stpd_notification_history_list",
    "stpd_notification_history_clear", "stpd_notification_history_close"
  )
  for (id in expected_ids) {
    expect_length(stpd_orientation_query(paste0("#", id)), 1L)
  }

  notification_script <- stpd_orientation_script_source(
    "stpd_notification_history_panel"
  )
  expect_true(nzchar(notification_script))
  expect_match(notification_script, "MutationObserver", fixed = TRUE)
  expect_match(notification_script, "textContent", fixed = TRUE)
  expect_match(notification_script, "maxHistory = 50", fixed = TRUE)
  expect_match(notification_script, "maxMessageLength = 4000", fixed = TRUE)
  expect_match(notification_script, "history.length > maxHistory", fixed = TRUE)
  expect_match(
    notification_script,
    "unread = Math.min(maxHistory, unread + 1)", fixed = TRUE
  )
  expect_match(notification_script, "seen.delete(item.id)", fixed = TRUE)
  expect_match(notification_script, "seen.clear()", fixed = TRUE)
  expect_match(notification_script, "input.name !== 'ui_language'", fixed = TRUE)
  expect_match(
    notification_script,
    "Clear prior-language prose instead of showing stale English", fixed = TRUE
  )
  expect_match(notification_script, "shiny-progress", fixed = TRUE)
  expect_match(
    notification_script,
    "stpd-notification-history-type-label", fixed = TRUE
  )
  seen_checks <- gregexpr("seen.has(id)", notification_script, fixed = TRUE)[[1]]
  expect_gte(sum(seen_checks > 0L), 2L)
  expect_match(notification_script, "data-stpd-i18n-source-text", fixed = TRUE)
  expect_no_match(notification_script, "innerHTML", fixed = TRUE)

  sidebar_script <- stpd_orientation_script_source("mobileQuery")
  expect_match(
    sidebar_script, "mobileQuery.addEventListener('change'", fixed = TRUE
  )
  expect_match(
    sidebar_script, "stpdSetSidebarCollapsedTransient", fixed = TRUE
  )
})

test_that("Orientation CSS has desktop, compact, and mobile layout contracts", {
  styles <- stpd_orientation_query("style")
  css <- paste(vapply(styles, as.character, character(1)), collapse = "\n")

  expect_match(css, ".stpd-orientation-nav", fixed = TRUE)
  expect_match(css, ".stpd-orientation-nav-group", fixed = TRUE)
  expect_match(css, ".stpd-orientation-context", fixed = TRUE)
  expect_match(css, ".stpd-notification-history", fixed = TRUE)
  expect_match(css, ".stpd-workbench > .row > .col-sm-3", fixed = TRUE)
  expect_match(css, ".workflow-short", fixed = TRUE)
  expect_match(css, ".workflow-step > .action-label", fixed = TRUE)
  expect_match(css, "display: inline-flex", fixed = TRUE)
  expect_match(css, "align-items: center", fixed = TRUE)
  expect_match(css, "justify-content: center", fixed = TRUE)
  expect_match(css, "width: clamp(32px, 2vw, 38px)", fixed = TRUE)
  expect_match(css, "height: clamp(32px, 2vw, 38px)", fixed = TRUE)
  expect_match(css, "background: #2563eb", fixed = TRUE)
  expect_match(css, "color: #ffffff", fixed = TRUE)
  expect_match(css, "border-radius: 50%", fixed = TRUE)
  expect_match(css, "white-space: normal", fixed = TRUE)
  expect_match(css, ".detector-run-card", fixed = TRUE)
  expect_match(css, ".detector-run-actions .btn-primary", fixed = TRUE)
  expect_match(css, "min-height: 48px", fixed = TRUE)
  expect_no_match(
    css, ".workflow-step .workflow-index { display: none; }", fixed = TRUE
  )
  expect_no_match(
    css, ".workflow-step .workflow-title { font-size: 11px; }", fixed = TRUE
  )
  expect_match(css, "@media (max-width: 1100px)", fixed = TRUE)
  expect_match(css, "@media (max-width: 900px)", fixed = TRUE)

  mobile_marker <- regexpr("@media (max-width: 900px)", css, fixed = TRUE)
  narrow_marker <- regexpr("@media (max-width: 520px)", css, fixed = TRUE)
  expect_gt(mobile_marker, 0L)
  expect_gt(narrow_marker, mobile_marker)
  expect_match(
    substr(css, mobile_marker, min(nchar(css), mobile_marker + 6000L)),
    "stpd-orientation-nav",
    fixed = TRUE
  )
  expect_match(
    substr(css, mobile_marker, min(nchar(css), mobile_marker + 6000L)),
    "workflow-strip",
    fixed = TRUE
  )
  expect_match(
    substr(css, narrow_marker, min(nchar(css), narrow_marker + 3000L)),
    "stpd-notification-history-panel",
    fixed = TRUE
  )
})

test_that("all new visible Orientation copy has a complete English translation", {
  selectors <- c(
    ".stpd-orientation-group-label", ".workflow-title", ".workflow-short", ".workflow-note",
    "#stpd_notification_history_title", "#stpd_notification_history_clear"
  )
  tags <- unlist(lapply(selectors, stpd_orientation_query), recursive = FALSE)
  toggle <- stpd_orientation_query("#stpd_notification_history_toggle")[[1]]
  close <- stpd_orientation_query("#stpd_notification_history_close")[[1]]
  history_head <- stpd_orientation_query(".stpd-notification-history-head")[[1]]
  history_note <- htmltools::tagQuery(history_head)$find(".small-note")$selectedTags()
  toggle_spans <- htmltools::tagQuery(toggle)$find("span")$selectedTags()

  source_text <- c(
    vapply(tags, stpd_orientation_tag_text, character(1)),
    stpd_orientation_tag_text(toggle_spans[[1]]),
    stpd_orientation_attr(list(toggle), "title"),
    stpd_orientation_attr(list(close), "aria-label"),
    vapply(history_note, stpd_orientation_tag_text, character(1)),
    "\u672C session \u5C1A\u65E0\u754C\u9762\u901A\u77E5\u3002"
  )
  source_text <- unique(source_text)
  source_text <- source_text[nzchar(source_text)]
  chinese <- source_text[
    vapply(source_text, SpikeTrainPatternDetector:::stpd_i18n_contains_cjk, logical(1))
  ]

  expect_gt(length(chinese), 0L)
  english <- vapply(
    chinese,
    SpikeTrainPatternDetector:::stpd_i18n_translate_text,
    character(1),
    lang = "en"
  )
  expect_false(any(vapply(
    english,
    SpikeTrainPatternDetector:::stpd_i18n_contains_cjk,
    logical(1)
  )))

  expect_identical(
    SpikeTrainPatternDetector:::stpd_i18n_translate_text(
      "\u6A21\u5F0F\uFF1A\u672A\u8FD0\u884C", "en"
    ),
    "Mode: Not run"
  )
  expect_identical(
    SpikeTrainPatternDetector:::stpd_i18n_translate_text(
      "\u68C0\u6D4B\u5668\u8FD0\u884C\u672A\u5B8C\u6210\uFF1A\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002",
      "en"
    ),
    "Detection did not run: upload at least one dataset."
  )
})
