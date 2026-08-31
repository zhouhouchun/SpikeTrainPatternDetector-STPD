ui_language_contract_placeholders <- function(x) {
  x <- as.character(x %||% "")[1]
  hits <- regmatches(
    x,
    gregexpr("\\{[A-Za-z0-9_]+\\}", x, perl = TRUE)
  )[[1]]
  if (length(hits) == 1L && identical(hits, "")) return(character())
  substring(hits, 2L, nchar(hits) - 1L)
}

ui_language_contract_fixed_count <- function(needle, haystack) {
  positions <- gregexpr(needle, haystack, fixed = TRUE)[[1]]
  if (length(positions) == 1L && identical(positions, -1L)) 0L else length(positions)
}

ui_language_contract_technical_allowlist <- c(
  # Product/algorithm names.
  "Spike", "Train", "Pattern", "Detector", "English",
  "AUTO", "MANUAL", "Event", "State", "Gap", "Review", "Legacy",
  "ISI", "Mean", "LogISI", "Pasquale", "newBD",
  "PCA", "PHATE", "UMAP", "t", "SNE", "Isomap", "GMM", "HSMM", "RQA",
  "FA", "GPFA", "CEBRA", "sliceTCA", "SVD", "EM", "Kalman", "kNN",
  "phateR", "uwot", "Rtsne", "numpy", "torch", "slicetca",
  # File formats, runtimes, and units.
  "CSV", "RDS", "ZIP", "YAML", "JSON", "API", "UI", "QC", "UTF",
  "R", "Python", "Shiny", "s", "ms", "Hz", "mm", "Z",
  # Scientific labels, field names, and metric names. These are exact tokens;
  # ordinary English prose is intentionally absent.
  "burst", "long_burst", "possible_burst", "tonic", "pause", "others",
  "hf_spiking", "hf_tonic", "spike", "spikes", "spiking", "train",
  "neuron", "bin", "trial", "condition", "event", "timestamp", "source",
  "dataset", "pattern", "category", "parameter", "score", "percentile",
  "q", "q90", "CV", "LV", "MM", "ratio", "relative_change", "scope",
  "phase", "status", "code", "ID", "hash", "params_hash", "ledger",
  "candidate", "fragment", "edge", "bridge", "seed", "method", "artifact",
  "overlap", "overlay", "raster", "support", "runner", "session", "core",
  "numeric", "integer", "logical", "character", "sorting", "multi", "unit",
  "family", "final", "all", "specific", "phenotype", "frequency", "high",
  "contrast", "contract", "near", "miss", "logISI", "behavior", "decoding",
  "trustworthiness", "shuffle", "proxy", "backend", "rank", "NM1", "NM2",
  "NM3", "TC1", "TC2", "TC3", "Event_"
)

ui_language_contract_forbidden_prose_tokens <- c(
  "no", "failed", "failure", "failures", "select", "selected", "available",
  "current", "raw", "generated", "generate", "loading", "loaded", "writing",
  "validating", "preparing", "please", "the", "is", "are", "was", "were",
  "has", "have", "from", "to", "for", "with", "without", "and", "or"
)

ui_language_contract_latin_tokens <- function(x) {
  x <- as.character(x %||% "")[1]
  # Placeholder names and DataTables control tokens are separately checked
  # for exact byte preservation; they are not user-facing prose.
  x <- gsub("\\{[A-Za-z0-9_]+\\}", " ", x, perl = TRUE)
  x <- gsub("_(START|END|TOTAL|MAX|MENU)_", " ", x, perl = TRUE)
  x <- gsub("<[^>]*>", " ", x, perl = TRUE)
  hits <- regmatches(
    x,
    gregexpr("[A-Za-z][A-Za-z0-9_]*", x, perl = TRUE)
  )[[1]]
  if (length(hits) == 1L && identical(hits, "")) character() else hits
}

ui_language_contract_unapproved_english <- function(x) {
  tokens <- ui_language_contract_latin_tokens(x)
  if (length(tokens) == 0L) return(character())
  token_lower <- tolower(tokens)
  approved <- token_lower %in% tolower(ui_language_contract_technical_allowlist)
  # Snake-case identifiers are field names or runtime label values, not prose.
  approved <- approved | grepl(
    "^[A-Za-z][A-Za-z0-9]*(?:_[A-Za-z0-9]+)+$",
    tokens,
    perl = TRUE
  )
  forbidden <- token_lower %in% ui_language_contract_forbidden_prose_tokens
  unique(tokens[!approved | forbidden])
}

ui_language_contract_is_acceptable_zh <- function(x) {
  x <- trimws(as.character(x %||% ""))
  vapply(
    x,
    function(value) length(ui_language_contract_unapproved_english(value)) == 0L,
    logical(1)
  )
}

ui_language_contract_progress_text <- function(events) {
  events <- Filter(
    function(x) identical(x$type, "stpdDataLoadProgress"),
    events
  )
  unlist(lapply(events, function(x) {
    payload <- x$payload
    c(
      as.character(payload$message %||% ""),
      as.character(payload$detail %||% "")
    )
  }), use.names = FALSE)
}

ui_language_contract_import_probe <- function(lang) {
  good_path <- tempfile(pattern = paste0("stpd_language_", lang, "_ok_"), fileext = ".csv")
  warning_path <- tempfile(pattern = paste0("stpd_language_", lang, "_warning_"), fileext = ".csv")
  on.exit(unlink(c(good_path, warning_path)), add = TRUE)

  timestamps <- seq(0, 0.59, by = 0.01)
  warning_timestamps <- timestamps
  warning_timestamps[30L] <- warning_timestamps[29L]
  utils::write.csv(
    data.frame(train_1 = timestamps),
    good_path,
    row.names = FALSE,
    quote = FALSE
  )
  utils::write.csv(
    data.frame(train_1 = warning_timestamps),
    warning_path,
    row.names = FALSE,
    quote = FALSE
  )

  uploads <- data.frame(
    name = c("ui_language_contract_ok.csv", "ui_language_contract_qc_warning.csv"),
    size = unname(file.info(c(good_path, warning_path))$size),
    type = rep("text/csv", 2L),
    datapath = c(good_path, warning_path),
    stringsAsFactors = FALSE
  )

  notifications <- list()
  custom_messages <- list()
  dataset_ids <- character()
  quality_levels <- list()

  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      notification_env <- environment(data_load_progress)
      had_local_notification <- exists(
        "showNotification", envir = notification_env, inherits = FALSE
      )
      old_notification <- if (had_local_notification) {
        get("showNotification", envir = notification_env, inherits = FALSE)
      } else {
        NULL
      }
      assign(
        "showNotification",
        function(ui, type = NULL, duration = NULL, ...) {
          notifications[[length(notifications) + 1L]] <<- list(
            text = paste(as.character(ui), collapse = ""),
            type = as.character(type %||% "")[1],
            duration = duration
          )
          invisible(NULL)
        },
        envir = notification_env
      )
      session$sendCustomMessage <- function(type, message) {
        custom_messages[[length(custom_messages) + 1L]] <<- list(
          type = as.character(type)[1], payload = message
        )
        invisible(NULL)
      }

      session$setInputs(
        ui_language = lang,
        unit_in_raw = "s",
        header_raw = TRUE,
        duplicate_timestamp_policy = "warn_keep",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        refractory_suspect_ms = 1,
        time_unit = "s",
        file_raw = uploads
      )
      session$flushReact()

      dataset_ids <<- names(rv$datasets)
      quality_levels <<- lapply(
        rv$datasets,
        function(ds) unique(as.character(ds$quality$warning_level))
      )

      if (had_local_notification) {
        assign("showNotification", old_notification, envir = notification_env)
      } else if (exists("showNotification", envir = notification_env, inherits = FALSE)) {
        rm(list = "showNotification", envir = notification_env)
      }
    })
  ))

  list(
    dataset_ids = dataset_ids,
    quality_levels = quality_levels,
    notifications = notifications,
    progress = custom_messages
  )
}

ui_language_contract_detector_dataset <- function() {
  timestamps <- c(0, 0.01, 0.025, 0.05, 0.09)
  train <- data.frame(
    idx = seq_along(timestamps),
    timestamp_sec = timestamps,
    ISI_sec = c(NA_real_, diff(timestamps)),
    pattern_manual = rep("", length(timestamps)),
    pattern_manual_negative = rep("", length(timestamps)),
    pattern_auto = rep("", length(timestamps)),
    stringsAsFactors = FALSE
  )
  make_dataset(
    name = "ui-language-detector-fixture",
    source = "synthetic",
    trains = list(train_1 = train),
    unit_in = "s"
  )
}

test_that("Chinese prose scanner rejects English sentences but preserves technical bytes", {
  forbidden_examples <- c(
    "\u4E2D\u6587\u63D0\u793A\uFF1ANo data available.",
    "\u5F53\u524D\u72B6\u6001\uFF1AFailed.",
    "\u8BF7\u9009\u62E9\u9879\u76EE\uFF1ASelect item.",
    "\u539F\u59CB\u65F6\u95F4\uFF1ARaw timestamp.",
    "\u5BFC\u51FA\u72B6\u6001\uFF1AGenerated.",
    "\u5BFC\u5165\u8FDB\u5EA6\uFF1ALoading CSV."
  )
  expect_false(any(ui_language_contract_is_acceptable_zh(forbidden_examples)))
  expect_setequal(
    ui_language_contract_unapproved_english(forbidden_examples[[1L]]),
    c("No", "data", "available")
  )

  technical_values <- c(
    "\u65B9\u6CD5\uFF1APCA | \u6807\u7B7E\uFF1Aburst | \u5B57\u6BB5\uFF1Aparams_sha256 | \u683C\u5F0F\uFF1AUTF-8 CSV | \u6570\u503C\uFF1A0.1 ms | 40 Hz",
    "trial_1 / condition_A / q90=0.95"
  )
  technical_bytes <- serialize(technical_values, NULL, version = 3L)
  expect_true(all(ui_language_contract_is_acceptable_zh(technical_values)))
  expect_identical(
    serialize(technical_values, NULL, version = 3L),
    technical_bytes
  )
})

test_that("UI-copy catalog is complete and language-safe", {
  catalog <- stpd_ui_copy_catalog()
  expect_type(catalog, "list")
  expect_gt(length(catalog), 0L)
  expect_true(all(nzchar(names(catalog))))
  expect_false(anyDuplicated(names(catalog)) > 0L)

  valid_language_entries <- vapply(catalog, function(entry) {
    is.character(entry) &&
      setequal(names(entry), c("zh", "en")) &&
      length(entry[["zh"]]) == 1L &&
      length(entry[["en"]]) == 1L &&
      !is.na(entry[["zh"]]) && nzchar(entry[["zh"]]) &&
      !is.na(entry[["en"]]) && nzchar(entry[["en"]])
  }, logical(1))
  expect_true(
    all(valid_language_entries),
    info = paste("invalid keys:", paste(names(catalog)[!valid_language_entries], collapse = ", "))
  )

  placeholder_parity <- vapply(catalog, function(entry) {
    identical(
      sort(ui_language_contract_placeholders(entry[["zh"]])),
      sort(ui_language_contract_placeholders(entry[["en"]]))
    )
  }, logical(1))
  expect_true(
    all(placeholder_parity),
    info = paste("placeholder mismatch:", paste(names(catalog)[!placeholder_parity], collapse = ", "))
  )

  zh_safe <- vapply(
    catalog,
    function(entry) ui_language_contract_is_acceptable_zh(entry[["zh"]]),
    logical(1)
  )
  expect_true(
    all(zh_safe),
    info = paste("unapproved English in Chinese copy:", paste(names(catalog)[!zh_safe], collapse = ", "))
  )

  en_has_cjk <- vapply(
    catalog,
    function(entry) stpd_i18n_contains_cjk(entry[["en"]]),
    logical(1)
  )
  expect_false(
    any(en_has_cjk),
    info = paste("Chinese fallback in English copy:", paste(names(catalog)[en_has_cjk], collapse = ", "))
  )
})

test_that("UI-copy interpolation preserves every supplied value verbatim", {
  catalog_before <- stpd_ui_copy_catalog()
  placeholder_keys <- names(Filter(
    function(entry) length(ui_language_contract_placeholders(entry[["zh"]])) > 0L,
    catalog_before
  ))
  expect_gt(length(placeholder_keys), 0L)

  checks <- unlist(lapply(placeholder_keys, function(key) {
    required <- unique(ui_language_contract_placeholders(catalog_before[[key]][["zh"]]))
    values <- setNames(
      lapply(required, function(name) paste0("SENTINEL|", name, "|A/B_007")),
      required
    )
    unlist(lapply(c("zh", "en"), function(lang) {
      template <- catalog_before[[key]][[lang]]
      rendered <- do.call(
        stpd_ui_copy,
        c(list(key = key, lang = lang), values)
      )
      occurrence_checks <- vapply(required, function(name) {
        expected_count <- sum(ui_language_contract_placeholders(template) == name)
        ui_language_contract_fixed_count(values[[name]], rendered) == expected_count
      }, logical(1))
      c(
        no_unresolved_placeholder =
          length(ui_language_contract_placeholders(rendered)) == 0L,
        occurrence_checks
      )
    }), use.names = TRUE)
  }), use.names = TRUE)

  expect_true(
    all(checks),
    info = paste("failed interpolation checks:", paste(names(checks)[!checks], collapse = ", "))
  )
  expect_identical(stpd_ui_copy_catalog(), catalog_before)

  missing_key <- placeholder_keys[[1L]]
  expect_error(
    stpd_ui_copy(missing_key, lang = "zh"),
    "Missing UI-copy interpolation value",
    fixed = TRUE
  )
})

test_that("DataTables language contract keeps tokens and avoids fallback", {
  zh <- stpd_ui_dt_language("zh")
  en <- stpd_ui_dt_language("en")

  expect_identical(en, list())
  expect_setequal(
    names(zh),
    c(
      "emptyTable", "info", "infoEmpty", "infoFiltered", "lengthMenu",
      "loadingRecords", "processing", "search", "zeroRecords", "paginate", "aria"
    )
  )
  expect_setequal(names(zh$paginate), c("first", "previous", "next", "last"))
  expect_setequal(names(zh$aria), c("sortAscending", "sortDescending"))

  zh_text <- unname(unlist(zh, use.names = FALSE))
  expect_true(all(vapply(zh_text, stpd_i18n_contains_cjk, logical(1))))
  expect_true(all(ui_language_contract_is_acceptable_zh(zh_text)))
  expect_match(zh$info, "_START_", fixed = TRUE)
  expect_match(zh$info, "_END_", fixed = TRUE)
  expect_match(zh$info, "_TOTAL_", fixed = TRUE)
  expect_match(zh$infoFiltered, "_MAX_", fixed = TRUE)
  expect_match(zh$lengthMenu, "_MENU_", fixed = TRUE)

  source <- data.frame(train = c("train_1", "train_2"), value = 1:2)
  zh_widget <- stpd_ui_localize_dt_filter_html(
    DT::datatable(source, filter = "top"),
    lang = "zh"
  )
  en_widget <- stpd_ui_localize_dt_filter_html(
    DT::datatable(source, filter = "top"),
    lang = "en"
  )
  expect_true(any(grepl("placeholder=['\"]\u5168\u90E8['\"]", zh_widget$x$filterHTML)))
  expect_false(any(grepl("placeholder=['\"]All['\"]", zh_widget$x$filterHTML)))
  expect_true(any(grepl("placeholder=['\"]All['\"]", en_widget$x$filterHTML)))
})

test_that("table copy localizes known neural notes without touching technical values", {
  neural_note <- paste0(
    "Embedding is computed from binned population activity, ",
    "not detector-derived event labels."
  )
  slicetca_note <- "Python sliceTCA backend has not produced a reconstruction."
  unknown_technical_value <- "CEBRA::latent_axis_vNext / sliceTCA.rank[trial]=7"
  source <- data.frame(
    method = c("neural_manifold", "sliceTCA", "future_backend"),
    note = c(neural_note, slicetca_note, unknown_technical_value),
    score = c(0.25, NA_real_, 7),
    stringsAsFactors = FALSE
  )
  source_bytes <- serialize(source, NULL, version = 3L)

  zh <- stpd_ui_localize_table_copy(source, lang = "zh")
  en <- stpd_ui_localize_table_copy(source, lang = "en")

  expect_identical(
    zh$note[[1L]],
    paste0(
      "\u5D4C\u5165\u7531\u5206\u7BB1\u540E\u7684\u7FA4\u4F53\u6D3B\u52A8\u8BA1\u7B97\uFF0C",
      "\u800C\u4E0D\u662F\u7531\u68C0\u6D4B\u5668\u751F\u6210\u7684\u4E8B\u4EF6\u6807\u7B7E\u8BA1\u7B97\u3002"
    )
  )
  expect_identical(
    zh$note[[2L]],
    "Python sliceTCA \u540E\u7AEF\u5C1A\u672A\u751F\u6210\u91CD\u5EFA\u7ED3\u679C\u3002"
  )
  expect_identical(zh$note[[3L]], unknown_technical_value)
  expect_identical(zh$method, source$method)
  expect_identical(zh$score, source$score)
  expect_true(all(ui_language_contract_is_acceptable_zh(zh$note)))
  expect_identical(serialize(en, NULL, version = 3L), source_bytes)
  expect_identical(serialize(source, NULL, version = 3L), source_bytes)
})

test_that("governance and validation table prose localizes in both directions", {
  source_tables <- list(
    preset = preset_catalog(),
    guidance = validation_guidance(NULL, default_params_sec()),
    roadmap = development_roadmap()
  )

  for (name in names(source_tables)) {
    source <- source_tables[[name]]
    english <- stpd_ui_localize_table_copy(source, lang = "en")
    english_text <- unlist(
      english[vapply(english, function(x) is.character(x) || is.factor(x), logical(1))],
      use.names = FALSE
    )
    expect_false(
      any(vapply(english_text, stpd_i18n_contains_cjk, logical(1))),
      info = name
    )
  }

  roadmap_zh <- stpd_ui_localize_table_copy(development_roadmap(), lang = "zh")
  expect_match(roadmap_zh$recommendation[[1L]], "\u53C2\u8003\u539F\u578B", fixed = TRUE)
  expect_match(roadmap_zh$scientific_rationale[[1L]], "\u884C\u4E3A\u53C2\u8003", fixed = TRUE)

  overfit_source <- data.frame(
    item = c("recommended_interpretation", "publication_warning"),
    value = c(
      "Use current metrics as calibration feedback unless held-out train/dataset evaluation is performed.",
      "Report strict high-confidence, review-candidate, and burst-family metrics separately; do not merge possible_burst into burst silently."
    ),
    stringsAsFactors = FALSE
  )
  overfit_zh <- stpd_ui_localize_table_copy(overfit_source, lang = "zh")
  expect_match(overfit_zh$value[[1L]], "\u6821\u51C6\u53CD\u9988", fixed = TRUE)
  expect_match(overfit_zh$value[[2L]], "\u4E0D\u5F97\u5C06 possible_burst", fixed = TRUE)
})

test_that("server-local Plotly config follows the active UI language", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  locales <- list()
  dependencies <- list()
  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      session$setInputs(ui_language = "zh")
      session$flushReact()
      plot_zh <- config(
        plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers"),
        displaylogo = FALSE
      )
      locales$zh <<- plot_zh$x$config$locale
      dependencies$zh <<- vapply(
        plot_zh$dependencies, function(dep) dep$name, character(1)
      )

      session$setInputs(ui_language = "en")
      session$flushReact()
      plot_en <- config(
        plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers"),
        displaylogo = FALSE
      )
      locales$en <<- plot_en$x$config$locale
      dependencies$en <<- vapply(
        plot_en$dependencies, function(dep) dep$name, character(1)
      )
    })
  ))

  expect_identical(locales$zh, "zh-CN")
  expect_identical(locales$en, "en")
  expect_true("plotly-locale-zh-CN" %in% dependencies$zh)
  expect_false(any(grepl("^plotly-locale-", dependencies$en)))
})

test_that("neural-manifold and sliceTCA plots localize empty and ready states", {
  skip_if_not_installed("plotly")

  neural_empty_zh <- suppressMessages(suppressWarnings(plotly::plotly_build(
    stpd_neural_manifold_plot(list(features = data.frame()), lang = "zh")
  )))
  neural_empty_en <- suppressMessages(suppressWarnings(plotly::plotly_build(
    stpd_neural_manifold_plot(list(features = data.frame()), lang = "en")
  )))
  neural_empty_zh_text <- neural_empty_zh$x$layout$annotations[[1L]]$text
  neural_empty_en_text <- neural_empty_en$x$layout$annotations[[1L]]$text
  expect_identical(
    neural_empty_zh_text,
    "\u6682\u65E0\u795E\u7ECF\u6D41\u5F62\u5750\u6807\u3002"
  )
  expect_true(ui_language_contract_is_acceptable_zh(neural_empty_zh_text))
  expect_identical(
    neural_empty_en_text,
    "No neural manifold coordinates available."
  )
  expect_false(stpd_i18n_contains_cjk(neural_empty_en_text))

  slicetca_empty <- list(
    reconstructed_embedding = data.frame(),
    trial_embedding = data.frame()
  )
  slicetca_empty_zh <- suppressMessages(suppressWarnings(plotly::plotly_build(
    stpd_slicetca_plot(slicetca_empty, lang = "zh")
  )))
  slicetca_empty_en <- suppressMessages(suppressWarnings(plotly::plotly_build(
    stpd_slicetca_plot(slicetca_empty, lang = "en")
  )))
  slicetca_empty_zh_text <- slicetca_empty_zh$x$layout$annotations[[1L]]$text
  slicetca_empty_en_text <- slicetca_empty_en$x$layout$annotations[[1L]]$text
  expect_identical(
    slicetca_empty_zh_text,
    "\u6682\u65E0\u53EF\u7528\u7684 sliceTCA \u5F20\u91CF\u6D41\u5F62\u3002"
  )
  expect_true(ui_language_contract_is_acceptable_zh(slicetca_empty_zh_text))
  expect_identical(
    slicetca_empty_en_text,
    "No sliceTCA tensor manifold is available."
  )
  expect_false(stpd_i18n_contains_cjk(slicetca_empty_en_text))

  neural_features <- data.frame(
    NM1 = c(0, 1), NM2 = c(0.5, 1.5), NM3 = c(1, 2),
    bin_id = 1:2,
    bin_start_sec = c(0, 0.1), bin_end_sec = c(0.1, 0.2),
    time_mid_sec = c(0.05, 0.15), population_rate_hz = c(4, 6),
    stringsAsFactors = FALSE
  )
  neural_ready_zh <- plotly::plotly_build(stpd_neural_manifold_plot(
    list(features = neural_features, method_label = "PCA"), lang = "zh"
  ))
  neural_ready_en <- plotly::plotly_build(stpd_neural_manifold_plot(
    list(features = neural_features, method_label = "PCA"), lang = "en"
  ))
  neural_ready_zh_hover <- unlist(
    lapply(neural_ready_zh$x$data, `[[`, "text"), use.names = FALSE
  )
  neural_ready_en_hover <- unlist(
    lapply(neural_ready_en$x$data, `[[`, "text"), use.names = FALSE
  )
  expect_identical(neural_ready_zh$x$layout$title$text, "\u795E\u7ECF\u6D41\u5F62\uFF1APCA")
  expect_true(ui_language_contract_is_acceptable_zh(neural_ready_zh$x$layout$title$text))
  expect_true(all(ui_language_contract_is_acceptable_zh(neural_ready_zh_hover)))
  expect_true(any(grepl("0-0.1 s", neural_ready_zh_hover, fixed = TRUE)))
  expect_true(any(grepl("4 Hz/neuron", neural_ready_zh_hover, fixed = TRUE)))
  expect_identical(neural_ready_en$x$layout$title$text, "Neural manifold: PCA")
  expect_false(any(vapply(
    neural_ready_en_hover,
    stpd_i18n_contains_cjk,
    logical(1)
  )))
  expect_identical(neural_ready_zh$x$config$locale, "zh-CN")
  expect_identical(neural_ready_en$x$config$locale, "en")
  expect_true("plotly-locale-zh-CN" %in% vapply(
    neural_ready_zh$dependencies, function(dep) dep$name, character(1)
  ))

  slicetca_embedding <- data.frame(
    TC1 = c(0, 1), TC2 = c(0.5, 1.5), TC3 = c(1, 2),
    trial_id = c("trial_1", "trial_1"),
    condition = c("condition_A", "condition_A"),
    rel_time_sec = c(-0.1, 0.1),
    event_state = c("burst", "pause"),
    stringsAsFactors = FALSE
  )
  slicetca_ready <- list(
    reconstructed_embedding = data.frame(),
    trial_embedding = slicetca_embedding
  )
  slicetca_ready_zh <- plotly::plotly_build(stpd_slicetca_plot(
    slicetca_ready, use_reconstruction = FALSE, lang = "zh"
  ))
  slicetca_ready_en <- plotly::plotly_build(stpd_slicetca_plot(
    slicetca_ready, use_reconstruction = FALSE, lang = "en"
  ))
  slicetca_ready_zh_hover <- unlist(
    lapply(slicetca_ready_zh$x$data, `[[`, "text"), use.names = FALSE
  )
  slicetca_ready_en_hover <- unlist(
    lapply(slicetca_ready_en$x$data, `[[`, "text"), use.names = FALSE
  )
  expect_identical(
    slicetca_ready_zh$x$layout$title$text,
    "\u539F\u59CB\u8BD5\u6B21\u5F20\u91CF\u6D41\u5F62"
  )
  expect_true(ui_language_contract_is_acceptable_zh(slicetca_ready_zh$x$layout$title$text))
  expect_true(all(ui_language_contract_is_acceptable_zh(slicetca_ready_zh_hover)))
  expect_true(any(grepl("trial_1", slicetca_ready_zh_hover, fixed = TRUE)))
  expect_true(any(grepl("condition_A", slicetca_ready_zh_hover, fixed = TRUE)))
  expect_true(any(grepl("\u4E8B\u4EF6\u72B6\u6001\uFF1Aburst", slicetca_ready_zh_hover, fixed = TRUE)))
  expect_identical(
    slicetca_ready_en$x$layout$title$text,
    "Raw trial tensor manifold"
  )
  expect_false(any(vapply(
    slicetca_ready_en_hover,
    stpd_i18n_contains_cjk,
    logical(1)
  )))
  expect_identical(slicetca_ready_zh$x$config$locale, "zh-CN")
  expect_identical(slicetca_ready_en$x$config$locale, "en")
  expect_true("plotly-locale-zh-CN" %in% vapply(
    slicetca_ready_zh$dependencies, function(dep) dep$name, character(1)
  ))
})

test_that("detector summary switches language without rerunning or mutating science", {
  skip_if_not_installed("shiny")

  ds <- ui_language_contract_detector_dataset()
  expected_en <- stpd_ui_copy("no_selected_trains", lang = "en")
  expected_zh <- stpd_ui_copy("no_selected_trains", lang = "zh")
  pipeline_calls <- 0L

  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      rv$datasets <- list(ui_language_detector_fixture = ds)
      rv$current_id <- "ui_language_detector_fixture"
      session$setInputs(
        ui_language = "en",
        train_display_mode = "selected_only",
        trains = character(0),
        use_train_metadata_filter = FALSE,
        detector_selected_only = TRUE,
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        refractory_suspect_ms = 1
      )
      session$flushReact()
      expect_identical(displayed_train_names(), character(0))

      detector_env <- environment(run_detector_from_ui)
      had_local_pre_qc <- exists(
        "stpd_product_pre_detection_qc", envir = detector_env, inherits = FALSE
      )
      old_pre_qc <- if (had_local_pre_qc) {
        get("stpd_product_pre_detection_qc", envir = detector_env, inherits = FALSE)
      } else {
        NULL
      }
      assign(
        "stpd_product_pre_detection_qc",
        function(...) {
          pipeline_calls <<- pipeline_calls + 1L
          stop("detector pipeline must not start for an empty selected scope")
        },
        envir = detector_env
      )

      science_before <- serialize(rv$datasets, NULL, version = 3L)
      result <- run_detector_from_ui(
        params_override = default_params(),
        switch_to_plot = FALSE,
        notify = FALSE
      )
      session$flushReact()

      expect_null(result)
      expect_identical(pipeline_calls, 0L)
      expect_identical(output$detector_before_after_summary, expected_en)
      expect_false(stpd_i18n_contains_cjk(output$detector_before_after_summary))
      expect_s3_class(rv$last_detector_summary, "stpd_ui_bilingual")
      expect_setequal(names(rv$last_detector_summary), c("zh", "en"))
      expect_identical(unname(rv$last_detector_summary[["en"]]), expected_en)
      expect_identical(unname(rv$last_detector_summary[["zh"]]), expected_zh)
      expect_identical(serialize(rv$datasets, NULL, version = 3L), science_before)

      bilingual_before_switch <- serialize(
        rv$last_detector_summary, NULL, version = 3L
      )
      session$setInputs(ui_language = "zh")
      session$flushReact()

      expect_identical(output$detector_before_after_summary, expected_zh)
      expect_true(stpd_i18n_contains_cjk(output$detector_before_after_summary))
      expect_identical(pipeline_calls, 0L)
      expect_identical(
        serialize(rv$last_detector_summary, NULL, version = 3L),
        bilingual_before_switch
      )
      expect_identical(serialize(rv$datasets, NULL, version = 3L), science_before)

      if (had_local_pre_qc) {
        assign("stpd_product_pre_detection_qc", old_pre_qc, envir = detector_env)
      } else if (exists(
        "stpd_product_pre_detection_qc", envir = detector_env, inherits = FALSE
      )) {
        rm(list = "stpd_product_pre_detection_qc", envir = detector_env)
      }
    })
  ))
})

test_that("real raw imports localize QC notifications and progress in both languages", {
  skip_if_not_installed("shiny")

  zh <- ui_language_contract_import_probe("zh")
  en <- ui_language_contract_import_probe("en")

  expect_length(zh$dataset_ids, 2L)
  expect_length(en$dataset_ids, 2L)
  expect_true(all(vapply(zh$quality_levels, length, integer(1)) > 0L))
  expect_true(all(vapply(en$quality_levels, length, integer(1)) > 0L))

  zh_notifications <- vapply(zh$notifications, `[[`, character(1), "text")
  en_notifications <- vapply(en$notifications, `[[`, character(1), "text")
  expect_length(zh_notifications, 2L)
  expect_length(en_notifications, 2L)
  expect_true(stpd_ui_copy("qc_passed", "zh") %in% zh_notifications)
  expect_true(stpd_ui_copy("qc_warning_summary", "zh", n = 1L) %in% zh_notifications)
  expect_true(stpd_ui_copy("qc_passed", "en") %in% en_notifications)
  expect_true(stpd_ui_copy("qc_warning_summary", "en", n = 1L) %in% en_notifications)
  expect_true(all(ui_language_contract_is_acceptable_zh(zh_notifications)))
  expect_false(any(vapply(en_notifications, stpd_i18n_contains_cjk, logical(1))))

  zh_progress <- ui_language_contract_progress_text(zh$progress)
  en_progress <- ui_language_contract_progress_text(en$progress)
  expect_gt(length(zh_progress), 0L)
  expect_gt(length(en_progress), 0L)
  expect_true(any(grepl("ui_language_contract_ok.csv", zh_progress, fixed = TRUE)))
  expect_true(any(grepl("ui_language_contract_qc_warning.csv", en_progress, fixed = TRUE)))
  expect_true(all(ui_language_contract_is_acceptable_zh(zh_progress)))
  expect_false(
    any(vapply(en_progress, stpd_i18n_contains_cjk, logical(1))),
    info = paste(
      "Chinese progress leaked into English mode:",
      paste(unique(en_progress[vapply(en_progress, stpd_i18n_contains_cjk, logical(1))]), collapse = " | ")
    )
  )
})

test_that("parameter-contract issues are readable in Chinese without altering technical values", {
  raw <- c(
    "type mismatch: expected numeric, got character",
    "value outside choices: manual,auto",
    "value below contract minimum 0.001",
    "value above contract maximum 25",
    "invalid YAML logical value"
  )
  zh <- stpd_ui_localize_parameter_issue_text(raw, lang = "zh")
  en <- stpd_ui_localize_parameter_issue_text(raw, lang = "en")

  expect_identical(en, raw)
  expect_true(all(ui_language_contract_is_acceptable_zh(zh)))
  expect_match(zh[1], "numeric")
  expect_match(zh[1], "character")
  expect_match(zh[2], "manual,auto", fixed = TRUE)
  expect_match(zh[3], "0.001", fixed = TRUE)
  expect_match(zh[4], "25", fixed = TRUE)

  issues <- data.frame(
    severity = "error", path = "burst.T_seed", issue = raw[1],
    stringsAsFactors = FALSE
  )
  localized <- stpd_ui_localize_parameter_issues(issues, lang = "zh")
  expect_identical(localized$path, issues$path)
  expect_identical(localized$severity, issues$severity)
  expect_identical(localized$issue, zh[1])
})

test_that("scientific diagnostics and dynamic stationarity prose localize without changing English bytes", {
  source <- data.frame(
    method = c("stationarity", "stationarity", "FA", "PHATE", "sliceTCA"),
    status = c("warning", "warning", "skipped", "reported", "failed"),
    note = c(
      "<25 valid ISIs; stationarity check weak",
      "sliding median ISI drift ratio=2.4; pause/global thresholds may be state-dependent",
      "Not enough variables or time bins for maximum-likelihood factor analysis with non-negative degrees of freedom.",
      "PHATE-like fallback: diffusion potential plus metric MDS; install/use phateR for canonical PHATE.",
      "SYNTHETIC PYTHON BACKEND FAILURE"
    ),
    stringsAsFactors = FALSE
  )
  source_bytes <- serialize(source, NULL, version = 3L)
  en <- stpd_ui_localize_table_copy(source, lang = "en")
  zh <- stpd_ui_localize_table_copy(source, lang = "zh")

  expect_identical(serialize(en, NULL, version = 3L), source_bytes)
  expect_match(zh$note[1], "25")
  expect_match(zh$note[1], "\u5E73\u7A33\u6027")
  expect_match(zh$note[2], "2.4", fixed = TRUE)
  expect_match(zh$note[2], "\u6F02\u79FB")
  expect_match(zh$note[3], "\u6700\u5927\u4F3C\u7136\u56E0\u5B50\u5206\u6790")
  expect_match(zh$note[4], "PHATE")
  expect_match(zh$note[4], "\u56DE\u9000\u65B9\u6848")
  expect_identical(zh$note[5], "\u6280\u672F\u8BE6\u60C5\uFF1ASYNTHETIC PYTHON BACKEND FAILURE")
})

test_that("core detector scientific notes are Chinese-first with an exact English rendering", {
  schema <- stpd_parameter_schema(scope = "all")
  rows <- schema[
    as.character(schema$path) == "state.hf_spiking_min_isi_count",
    , drop = FALSE
  ]

  expect_gt(nrow(rows), 0L)
  notes <- unique(c(
    as.character(rows$scientific_note),
    as.character(rows$help_text)
  ))
  notes <- notes[!is.na(notes) & nzchar(notes)]
  expect_length(notes, 1L)
  expect_true(ui_language_contract_is_acceptable_zh(notes))
  expect_match(notes, "ISI")
  expect_match(notes, "spike")

  en <- stpd_i18n_translate_text(notes, lang = "en")
  expect_false(stpd_i18n_contains_cjk(en))
  expect_identical(
    en,
    paste(
      "Audit/recommendation threshold. For a contiguous high-frequency epoch,",
      "ISI count is usually spike count minus 1; artifact ISIs are not valid support."
    )
  )
})

test_that("schema-generated parameter panels rerender cleanly in either language", {
  core_zh <- htmltools::renderTags(schema_ui_controls(
    prefix = "schema_contract_zh_",
    group_by = TRUE,
    group_field = "group",
    open_groups = TRUE,
    show_notes = TRUE,
    lang = "zh"
  ))$html
  core_en <- htmltools::renderTags(schema_ui_controls(
    prefix = "schema_contract_en_",
    group_by = TRUE,
    group_field = "group",
    open_groups = TRUE,
    show_notes = TRUE,
    lang = "en"
  ))$html

  expect_true(stpd_i18n_contains_cjk(core_zh))
  expect_match(core_zh, "\u4E8B\u4EF6\u6027\u5206\u7C7B")
  expect_false(stpd_i18n_contains_cjk(core_en))
  expect_match(core_en, "Eventness audit")

  for (level in c("basic", "advanced", "expert", "all")) {
    html <- htmltools::renderTags(stpd_contract_ui_controls(
      prefix = paste0("parameter_contract_", level, "_"),
      ui_level = level,
      lang = "en"
    ))$html
    expect_false(
      stpd_i18n_contains_cjk(html),
      info = paste("Chinese copy remained in the English", level, "parameter panel")
    )
  }

  schema <- stpd_parameter_schema(scope = "all")
  row <- schema[
    as.character(schema$path) == "state.hf_spiking_min_isi_count",
    , drop = FALSE
  ][1, , drop = FALSE]
  input_id <- paste0("schema_param_", as.character(row$input_id))
  preserved <- stats::setNames(list(77L), input_id)
  preserved_html <- htmltools::renderTags(stpd_schema_ui_control(
    row,
    prefix = "schema_param_",
    show_notes = TRUE,
    lang = "en",
    current_values = preserved
  ))$html
  expect_match(preserved_html, 'value="77"', fixed = TRUE)
})

test_that("multi-line methodological guidance switches language line by line", {
  zh <- stpd_methodological_warning(as_vector = TRUE)
  en <- vapply(zh, stpd_i18n_translate_text, character(1), lang = "en")

  expect_length(zh, 6L)
  expect_true(all(ui_language_contract_is_acceptable_zh(zh)))
  expect_false(any(vapply(en, stpd_i18n_contains_cjk, logical(1))))
  expect_true(all(nzchar(trimws(en))))
})
