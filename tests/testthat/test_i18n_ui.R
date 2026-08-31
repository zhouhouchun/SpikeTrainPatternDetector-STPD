test_that("UI language switch assets are generated", {
  expect_equal(stpd_i18n_exact_dictionary()[["\u4E2D\u6587"]], "Chinese")
  expect_equal(stpd_i18n_exact_dictionary()[["\u539F\u59CB\u6587\u4EF6\u65F6\u95F4\u5355\u4F4D"]], "raw file time unit")
  expect_equal(stpd_i18n_exact_dictionary()[["burst \u68C0\u6D4B\u4E2D\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u7684\u5904\u7406\u65B9\u5F0F"]], "handling strategy for suspected refractory-period ISIs during burst detection")
  expect_equal(stpd_i18n_exact_dictionary()[["Seed / Bridge \u8BCA\u65AD"]], "Seed / Bridge diagnostics")
  expect_equal(stpd_i18n_exact_dictionary()[["\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002"]], "Please upload at least one dataset.")
  expect_equal(stpd_i18n_exact_dictionary()[["\u795E\u7ECF\u6D41\u5F62"]], "Neural Manifold")
  expect_equal(
    stpd_i18n_exact_dictionary()[["\u5F00\u59CB\u6A21\u5F0F\u68C0\u6D4B"]],
    "Start pattern detection"
  )
  expect_false(stpd_i18n_contains_cjk(stpd_i18n_translate_text(
    "\u5FEB\u901F\u9884\u89C8\uFF1A\u4EC5\u68C0\u6D4B\u5F53\u524D\u53EF\u89C1 trains\uFF08\u6B63\u5F0F\u8FD0\u884C\u9ED8\u8BA4\u5168\u90E8 trains\uFF09",
    "en"
  )))
  expect_equal(stpd_i18n_exact_dictionary()[["Isomap 3D X \u8F74"]], "Isomap 3D X axis")
  expect_equal(stpd_i18n_exact_dictionary()[["Isomap 3D Z \u8F74"]], "Isomap 3D Z axis")
  expect_equal(stpd_i18n_phrase_dictionary()[["\u5BFC\u5165\u6570\u636E"]], "import data")

  js <- paste(as.character(stpd_i18n_assets()), collapse = "\n")
  expect_match(js, "stpd_ui_language", fixed = TRUE)
  expect_match(js, "ui_language", fixed = TRUE)
  expect_match(js, "MutationObserver", fixed = TRUE)
  expect_match(js, "replacePhraseWithContext", fixed = TRUE)
  expect_match(js, "if (cjkPattern.test(out)) return source;", fixed = TRUE)
  expect_match(js, "translatablePreIds", fixed = TRUE)
  expect_match(js, "stpd-i18n-status", fixed = TRUE)
  ui_txt <- paste(as.character(ui), collapse = "\n")
  expect_match(ui_txt, "\u795E\u7ECF\u6D41\u5F62", fixed = TRUE)
  expect_no_match(ui_txt, "Neural Manifold\uFF08\u795E\u7ECF\u6D41\u5F62\uFF09", fixed = TRUE)
})

test_that("unknown or incomplete Chinese copy fails closed without information loss", {
  unknown <- "\u53C2\u6570\u4E2D\u7684\u5168\u65B0\u672A\u77E5\u79D1\u5B66\u8BF4\u660E\uFF1A\u4E0D\u5F97\u5220\u9664\u3002"
  partially_known <- "\u8FD0\u884C\u68C0\u6D4B\u540E\u8BF7\u68C0\u67E5\u672A\u77E5\u751F\u7269\u5B66\u8FB9\u754C\u3002"

  expect_identical(stpd_i18n_translate_text(unknown, "en"), unknown)
  expect_identical(stpd_i18n_translate_text(partially_known, "en"), partially_known)
  expect_identical(stpd_i18n_translate_text(unknown, "zh"), unknown)
  expect_true(nzchar(stpd_i18n_translate_text(unknown, "en")))

  phrases <- stpd_i18n_phrase_dictionary()
  expect_false(any(nchar(names(phrases), type = "chars") == 1L))
  expect_false(any(c("\u4E0E", "\u4E2D", "\u5904", "\u7684", "\u4E3A", "\u5C06") %in% names(phrases)))

  js <- paste(as.character(stpd_i18n_assets()), collapse = "\n")
  expect_no_match(js, ".replace(/[\\u3400-\\u9FFF\\uF900-\\uFAFF]+/g, ' ')", fixed = TRUE)
})

test_that("critical scientific guidance uses complete-sentence translations", {
  scientific_zh <- paste0(
    "\u79D1\u5B66\u9A8C\u8BC1\u4EE5\u624B\u52A8\u6807\u7B7E\u4F5C\u4E3A\u771F\u503C\uFF0C\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A\u624B\u52A8\u533A\u95F4\u7684 shadow \u68C0\u6D4B\uFF0C",
    "\u5C06\u5DF2\u6807\u8BB0 train \u5206\u6210\u6821\u51C6/\u9A8C\u8BC1\u96C6\uFF0C\u5E76\u62A5\u544A\u4E8B\u4EF6\u7EA7\u6307\u6807\u3002"
  )
  scientific_en <- paste0(
    "Scientific validation treats manual labels as truth, runs shadow detection without locking manual intervals, ",
    "splits annotated trains into calibration and validation sets, and reports event-level metrics."
  )
  review_boundary_zh <- paste0(
    "\u4E25\u683C\u6A21\u5F0F\u5C06 possible_burst \u4F5C\u4E3A\u590D\u6838\u7C7B\u3002",
    "\u5019\u9009\u5BB6\u65CF\u6A21\u5F0F\u5408\u5E76 burst + long_burst + possible_burst\uFF0C",
    "\u7528\u4E8E\u8BC4\u4F30\u5019\u9009\u53EC\u56DE\uFF0C\u800C\u975E\u9AD8\u7F6E\u4FE1 burst \u51C6\u786E\u7387\u3002"
  )

  expect_identical(stpd_i18n_translate_text(scientific_zh, "en"), scientific_en)
  expect_identical(
    stpd_i18n_translate_text(review_boundary_zh, "en"),
    paste0(
      "Strict mode reports possible_burst as a Review category. Candidate-family mode combines ",
      "burst + long_burst + possible_burst to evaluate candidate recall, not high-confidence burst precision."
    )
  )
  expect_false(stpd_i18n_contains_cjk(stpd_i18n_translate_text(scientific_zh, "en")))
})

test_that("dynamic human-readable statuses translate as anchored whole messages", {
  preview_zh <- "\u9884\u89C8\u5B8C\u6210\uFF1A3 \u6761 train\uFF1B\u53EF\u5347\u7EA7 4 \u4E2A possible_burst event / 9 \u4E2A ISI\u3002"
  expect_identical(
    stpd_i18n_translate_text(preview_zh, "en"),
    "Preview complete: 3 train(s); 4 possible_burst event(s) / 9 ISI(s) are eligible for promotion."
  )
  expect_identical(
    stpd_i18n_translate_text("\u6B63\u5728\u68C0\u6D4B train 4/10: Train_04", "en"),
    "Detecting train 4/10: Train_04"
  )
  expect_identical(
    stpd_i18n_translate_text("\u5C1A\u672A\u8FD0\u884C\u6279\u5904\u7406\u3002", "en"),
    "Batch processing has not been run yet."
  )

  js <- paste(as.character(stpd_i18n_assets()), collapse = "\n")
  expect_match(js, "'possible_burst_promotion_status'", fixed = TRUE)
  expect_match(js, "'parameter_sensitivity_status'", fixed = TRUE)
  expect_no_match(js, "'parameter_validation_summary'", fixed = TRUE)
  expect_match(js, "if (pre && !translatablePreIds.has(pre.id)", fixed = TRUE)
})

test_that("detector-output integrity failures remain actionable in English", {
  message_zh <- paste0(
    "\u6700\u7EC8\u5BA1\u8BA1\u91CD\u5EFA\u5931\u8D25\uFF1A",
    "\u68C0\u6D4B\u7ED3\u679C\u8EAB\u4EFD\u5DF2\u6539\u53D8\u6216\u65E0\u6CD5\u9A8C\u8BC1\uFF1B",
    "\u8BF7\u5148\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\uFF0C\u518D\u4FEE\u6539 Legacy \u5BA1\u8BA1\u5C42\u3002"
  )
  translated <- stpd_i18n_translate_text(message_zh, "en")
  expect_identical(
    translated,
    paste0(
      "Final-audit rebuild failed: The detector-output identity changed or ",
      "cannot be verified. Rerun detection before changing the Legacy audit layer."
    )
  )
  expect_false(stpd_i18n_contains_cjk(translated))
})

test_that("Phase 0 state cards and raster statuses remain distinguishable in English", {
  statuses <- c(
    "\u672A\u5206\u6790" = "Not analyzed",
    "\u5DF2\u5206\u6790" = "Analyzed",
    "\u7ED3\u679C\u8FC7\u671F" = "Results stale",
    "\u72B6\u6001\u672A\u77E5" = "Status unknown"
  )
  translated <- vapply(
    names(statuses), stpd_i18n_translate_text, character(1), lang = "en"
  )
  expect_identical(unname(translated), unname(statuses))
  expect_identical(
    stpd_i18n_translate_text("Train_04 [\u5DF2\u5206\u6790]", "en"),
    "Train_04 [analyzed]"
  )
  expect_identical(
    stpd_i18n_translate_text("Train_08 [\u7ED3\u679C\u8FC7\u671F]", "en"),
    "Train_08 [results stale]"
  )
  expect_identical(
    stpd_i18n_translate_text(
      "\u5F53\u524D\u7ED3\u679C\u8986\u76D6 7/12 \u6761 train\uFF1B\u672A\u8FD0\u884C\u7684 train \u4E0D\u5E94\u663E\u793A\u4E3A\u9634\u6027\u3002",
      "en"
    ),
    "Current results cover 7/12 train(s); trains that were not run must not be shown as negative."
  )
  expect_identical(
    stpd_i18n_translate_text(
      "\u6570\u636E\u3001\u8FD0\u884C\u3001\u771F\u503C\u548C\u9A8C\u8BC1\u8303\u56F4\u4E00\u81F4\uFF0812/12 \u6761 train\uFF09\u3002",
      "en"
    ),
    "Data, run, truth, and validation scope match (12/12 train(s))."
  )
})

test_that("confirmation and undo copy is translated without changing typed tokens", {
  expect_identical(stpd_i18n_translate_text("\u786E\u8BA4\u6267\u884C", "en"), "Confirm action")
  expect_identical(stpd_i18n_translate_text("\u53D6\u6D88", "en"), "Cancel")
  expect_identical(stpd_i18n_translate_text("\u64A4\u9500\u4E0A\u4E00\u6B21\u53D8\u66F4", "en"), "Undo last change")
  expect_identical(
    stpd_i18n_translate_text("\u6CA1\u6709\u53EF\u64A4\u9500\u7684\u4E0A\u4E00\u6B21\u53D8\u66F4\u3002", "en"),
    "There is no previous change to undo."
  )

  prompt <- stpd_i18n_translate_text("\u4E3A\u907F\u514D\u8BEF\u64CD\u4F5C\uFF0C\u8BF7\u8F93\u5165\uFF1A\u6E05\u7A7A", "en")
  expect_identical(prompt, "To prevent accidental changes, type this token unchanged: \u6E05\u7A7A")
  expect_match(prompt, "\u6E05\u7A7A", fixed = TRUE)
  expect_false("\u6E05\u7A7A" %in% names(stpd_i18n_exact_dictionary()))
  expect_identical(
    stpd_i18n_translate_text("\u8BF7\u8F93\u5165\u201C\u6E05\u7A7A\u201D\u540E\u518D\u786E\u8BA4\u3002", "en"),
    "Type \"\u6E05\u7A7A\" before confirming."
  )
})

test_that("Phase 2B disables Legacy promotion with complete English guidance", {
  warning_zh <- paste0(
    "\u8BE5\u5DE5\u5177\u53EA\u4FEE\u6539 Legacy MANUAL \u5355\u6807\u7B7E\u5C42\uFF0C\u4E0D\u4F1A\u521B\u5EFA Phase 2B Review\u2192Event \u8F6C\u6362\u3002",
    "\u6807\u51C6\u754C\u9762\u4E0D\u5141\u8BB8\u8986\u76D6\u5DF2\u6709 MANUAL / NOT-burst \u8BC1\u636E\u3002"
  )
  warning_en <- paste0(
    "This tool modifies only the Legacy MANUAL single-label layer; it does not create a Phase 2B Review\u2192Event transition. ",
    "The standard interface does not allow existing MANUAL / NOT-burst evidence to be overwritten."
  )
  disabled_zh <- paste0(
    "Legacy possible\u2192real \u5347\u7EA7\u5DF2\u7981\u7528\u3002",
    "\u5019\u9009\u7EA7 Review\u2192Event Confirm/Revoke \u5F53\u524D\u4EC5\u901A\u8FC7 API \u63D0\u4F9B\uFF0C",
    "\u672C\u9636\u6BB5 UI \u4E0D\u6267\u884C\u8BE5\u8F6C\u6362\u3002"
  )

  expect_identical(stpd_i18n_translate_text(warning_zh, "en"), warning_en)
  expect_identical(
    stpd_i18n_translate_text(disabled_zh, "en"),
    "Legacy possible\u2192real promotion is disabled. Candidate-level Review\u2192Event Confirm/Revoke is currently API-only; this UI phase does not execute that transition."
  )
  expect_false(stpd_i18n_contains_cjk(stpd_i18n_translate_text(warning_zh, "en")))
  expect_false(stpd_i18n_contains_cjk(stpd_i18n_translate_text(disabled_zh, "en")))

  confirmation_zh <- paste0(
    "\u5C06\u6309\u5DF2\u9884\u89C8\u7684\u56FA\u5B9A\u8303\u56F4\uFF0C\u628A 3 \u4E2A possible_burst event / 14 \u4E2A ISI \u5199\u5165 Legacy MANUAL burst\u3002",
    "\u4E0D\u4F1A\u8986\u76D6\u5DF2\u6709 MANUAL/NOT-burst\uFF0C\u4E5F\u4E0D\u4F1A\u521B\u5EFA Phase 2B transition\u3002"
  )
  expect_identical(
    stpd_i18n_translate_text(confirmation_zh, "en"),
    paste0(
      "Using the fixed previewed scope, this will write 3 possible_burst event(s) / 14 ISI(s) to Legacy MANUAL burst. ",
      "It will not overwrite existing MANUAL/NOT-burst evidence or create a Phase 2B transition."
    )
  )
})

test_that("validation estimands and biological limits survive English mode", {
  manual_zh <- paste0(
    "\u672C\u9875\u6BD4\u8F83\u4E0D\u9501\u5B9A MANUAL \u533A\u95F4\u7684 shadow AUTO \u4E0E\u5F53\u524D MANUAL \u53C2\u8003\u6807\u8BB0\uFF1B",
    "\u5B83\u4E0D\u662F\u5916\u90E8\u72EC\u7ACB\u9A8C\u8BC1\uFF0C\u4E5F\u4E0D\u80FD\u5355\u72EC\u8BC1\u660E\u751F\u7269\u5B66\u6709\u6548\u6027\u3002"
  )
  scientific_zh <- paste0(
    "\u672C\u9875\u5728\u5DF2\u6807\u8BB0 trains \u5185\u8FDB\u884C calibration/validation \u62C6\u5206\u5E76\u8FD0\u884C shadow \u68C0\u6D4B\uFF1B",
    "\u8FD9\u4E0D\u7B49\u4E8E\u5916\u90E8\u6CDB\u5316\u8BC1\u636E\uFF0C\u4E5F\u4E0D\u80FD\u5355\u72EC\u8BC1\u660E\u751F\u7269\u5B66\u6709\u6548\u6027\u3002"
  )

  expect_identical(
    stpd_i18n_translate_text("\u4F30\u8BA1\u76EE\u6807\uFF1Adetector_performance\u3002", "en"),
    "Estimand: detector_performance."
  )
  expect_match(stpd_i18n_translate_text(manual_zh, "en"),
               "does not by itself establish biological validity", fixed = TRUE)
  expect_match(stpd_i18n_translate_text(scientific_zh, "en"),
               "not evidence of external generalization", fixed = TRUE)
  ui_txt <- paste(as.character(ui), collapse = "\n")
  expect_match(ui_txt, "stpd-estimand-notice", fixed = TRUE)
  expect_match(ui_txt, "detector_performance", fixed = TRUE)
})

test_that("language round trips retain the original source text", {
  original <- "\u5C1A\u672A\u9884\u89C8 possible_burst \u6279\u91CF\u5347\u7EA7\u3002"
  english <- stpd_i18n_translate_text(original, "en")

  expect_identical(english, "No possible_burst bulk-promotion preview has been run yet.")
  # The client renders both directions from its retained source (WeakMap for
  # text nodes and data attributes for element attributes), never from the
  # previously translated string.
  expect_identical(stpd_i18n_translate_text(original, "zh"), original)

  js <- paste(as.character(stpd_i18n_assets()), collapse = "\n")
  expect_match(js, "const textOriginals = new WeakMap();", fixed = TRUE)
  expect_match(js, "data-stpd-i18n-original-", fixed = TRUE)
  expect_match(js, "translateText(original) : original", fixed = TRUE)
  expect_match(
    js,
    "window.Shiny.setInputValue('ui_language', currentLang", fixed = TRUE
  )
  expect_match(js, "document.addEventListener('shiny:connected'", fixed = TRUE)
})

test_that("workbench copy and layout are domain-neutral and compact", {
  subtitle_zh <- "\u8109\u51B2\u5E8F\u5217\uFF08spike train\uFF09\u5DE5\u4F5C\u53F0\uFF1A\u5BFC\u5165\u6570\u636E\u3001\u8BBE\u7F6E\u5173\u952E\u53C2\u6570\u3001\u8FD0\u884C\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\uFF0C\u7136\u540E\u590D\u6838\u5DEE\u5F02\u3001\u9A8C\u8BC1\u548C\u5BFC\u51FA\u3002"
  old_subtitle_zh <- "\u57FA\u5E95\u795E\u7ECF\u8282 spike train \u5DE5\u4F5C\u53F0\uFF1A\u5BFC\u5165\u6570\u636E\u3001\u8BBE\u7F6E\u5173\u952E\u53C2\u6570\u3001\u8FD0\u884C\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\uFF0C\u7136\u540E\u590D\u6838\u5DEE\u5F02\u3001\u9A8C\u8BC1\u548C\u5BFC\u51FA\u3002"
  dictionary <- stpd_i18n_exact_dictionary()

  expect_identical(
    unname(dictionary[[subtitle_zh]]),
    "Spike-train workbench: import data, set key parameters, run event-grammar detection, then review differences, validate, and export."
  )
  expect_false(old_subtitle_zh %in% names(dictionary))

  ui_txt <- paste(as.character(ui), collapse = "\n")
  expect_match(ui_txt, subtitle_zh, fixed = TRUE)
  expect_no_match(ui_txt, "\u57FA\u5E95\u795E\u7ECF\u8282", fixed = TRUE)
  expect_match(ui_txt, "col-sm-3", fixed = TRUE)
  expect_match(ui_txt, "col-sm-9", fixed = TRUE)
})

test_that("target-nucleus depth view is absent from the package and UI", {
  ui_txt <- paste(as.character(ui), collapse = "\n")

  expect_no_match(ui_txt, "\u76EE\u6807\u6838\u56E2\u6DF1\u5EA6\u56FE", fixed = TRUE)
  expect_no_match(ui_txt, "dbs_track_", fixed = TRUE)
  expect_false(any(grepl(
    "^stpd_dbs_track_",
    ls(asNamespace("SpikeTrainPatternDetector")),
    perl = TRUE
  )))
})

test_that("neural-network classifier workflow is absent while neural manifold remains", {
  ui_txt <- paste(as.character(ui), collapse = "\n")
  retired_ids <- c(
    "train_nn_model", "nn_model_in", "apply_nn_model",
    "evaluate_nn_model", "download_nn_model", "download_nn_model_tab",
    "download_ml_features_csv", "ml_label_source", "ml_hidden",
    "ml_decay", "ml_maxit", "ml_confidence", "ml_event_smoothing",
    "ml_nn_event_guardrails", "ml_apply_others", "nn_model_summary",
    "nn_eval_table", "nn_prediction_table"
  )

  expect_no_match(ui_txt, "\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B", fixed = TRUE)
  for (id in retired_ids) expect_no_match(ui_txt, id, fixed = TRUE)
  expect_match(ui_txt, "\u795E\u7ECF\u6D41\u5F62", fixed = TRUE)

  ns_names <- ls(asNamespace("SpikeTrainPatternDetector"), all.names = TRUE)
  retired_functions <- paste0(
    "^(train_nnet|predict_nnet|stpd_nn_|validate_nn|postprocess_nn|",
    "stpd_server_install_ml_module$|extract_ml_feature_table$|classification_eval$)"
  )
  expect_false(any(grepl(retired_functions, ns_names, perl = TRUE)))
})

test_that("i18n JSON encoding escapes generated script values", {
  encoded <- stpd_i18n_json_object(c("a\"b" = "c\\d"))
  expect_match(encoded, "\\\"a\\\\\\\"b\\\"", perl = TRUE)
  expect_match(encoded, "\\\"c\\\\\\\\d\\\"", perl = TRUE)

  hostile <- stpd_i18n_json_object(c("</script>" = "<img src=x onerror=alert(1)>&"))
  expect_no_match(hostile, "</script>", fixed = TRUE)
  expect_no_match(hostile, "<img", fixed = TRUE)
  expect_match(hostile, "\\u003C/script\\u003E", fixed = TRUE)
  expect_match(hostile, "\\u003Cimg src=x onerror=alert(1)\\u003E\\u0026", fixed = TRUE)
})
