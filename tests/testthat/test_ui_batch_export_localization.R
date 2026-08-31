test_that("batch export failures localize stable codes before raw technical details", {
  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      server_env <- environment(run_detector_from_ui)
      plan_message <- get(
        "batch_export_plan_failure_message", envir = server_env,
        inherits = FALSE
      )

      plan <- list(
        eligible = FALSE,
        code = "batch_export_blocked",
        detail = "1 of 2 datasets failed the batch export gate.",
        failures = data.frame(
          dataset_id = "dataset_a",
          code = "batch_export_params_effective_mismatch",
          detail = paste0(
            "The supplied dataset parameters do not match the frozen ",
            "params_effective snapshot."
          ),
          stringsAsFactors = FALSE
        )
      )

      zh <- plan_message(plan, lang = "zh")
      zh_lines <- strsplit(zh, "\n", fixed = TRUE)[[1L]]
      expect_identical(
        zh_lines[[1L]],
        "批量正式导出已停止；未写出任何批量结果。"
      )
      expect_match(
        zh_lines[[2L]],
        "当前参数与检测时冻结的有效参数不一致",
        fixed = TRUE
      )
      expect_match(
        zh_lines[[2L]],
        "状态码：batch_export_params_effective_mismatch",
        fixed = TRUE
      )
      expect_false(grepl("datasets failed", zh_lines[[1L]], fixed = TRUE))
      expect_false(grepl("supplied dataset", zh_lines[[2L]], fixed = TRUE))
      expect_true(startsWith(zh_lines[[3L]], "技术详情："))
      expect_match(zh_lines[[3L]], plan$detail, fixed = TRUE)
      expect_match(zh_lines[[3L]], plan$failures$detail, fixed = TRUE)

      en <- plan_message(plan, lang = "en")
      en_lines <- strsplit(en, "\n", fixed = TRUE)[[1L]]
      expect_identical(
        en_lines[[1L]],
        "Formal batch export was stopped; no batch results were written."
      )
      expect_match(
        en_lines[[2L]],
        "current parameters do not match the effective parameters",
        fixed = TRUE
      )
      expect_match(
        en_lines[[2L]],
        "Code: batch_export_params_effective_mismatch",
        fixed = TRUE
      )
      expect_true(startsWith(en_lines[[3L]], "Technical details: "))
      expect_false(grepl("[\u3400-\u9FFF]", en, perl = TRUE))

      session$setInputs(ui_language = "zh")
      expect_true(startsWith(plan_message(plan), "批量正式导出已停止"))
      session$setInputs(ui_language = "en")
      expect_true(startsWith(plan_message(plan), "Formal batch export was stopped"))
    })
  ))
})

test_that("batch ZIP verifier reasons are secondary technical details", {
  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      server_env <- environment(run_detector_from_ui)
      zip_message <- get(
        "batch_zip_failure_message", envir = server_env,
        inherits = FALSE
      )

      checked <- list(
        valid = FALSE,
        code = "zip_required_file_missing",
        reason = "One or more required archive members are missing."
      )
      zh <- zip_message(checked, lang = "zh")
      zh_lines <- strsplit(zh, "\n", fixed = TRUE)[[1L]]
      expect_identical(
        zh_lines[[1L]],
        "批量 ZIP 未通过生成后的完整性校验，因此不会提供下载。"
      )
      expect_match(zh_lines[[2L]], "缺少一个或多个必需文件", fixed = TRUE)
      expect_match(
        zh_lines[[2L]], "状态码：zip_required_file_missing", fixed = TRUE
      )
      expect_false(grepl(checked$reason, zh_lines[[2L]], fixed = TRUE))
      expect_identical(zh_lines[[3L]], paste0("技术详情：", checked$reason))

      checked$reason <- "底层校验器无法读取归档目录。"
      en <- zip_message(checked, lang = "en")
      en_lines <- strsplit(en, "\n", fixed = TRUE)[[1L]]
      expect_identical(
        en_lines[[1L]],
        paste0(
          "The batch ZIP failed post-generation integrity verification ",
          "and will not be offered for download."
        )
      )
      expect_match(en_lines[[2L]], "missing one or more required files", fixed = TRUE)
      expect_match(en_lines[[2L]], "Code: zip_required_file_missing", fixed = TRUE)
      expect_false(grepl("[\u3400-\u9FFF]", en_lines[[2L]], perl = TRUE))
      expect_identical(
        en_lines[[3L]],
        paste0("Technical details: ", checked$reason)
      )

      checked$reason <- "first validator line\nsecond validator line"
      normalized <- zip_message(checked, lang = "en")
      normalized_lines <- strsplit(normalized, "\n", fixed = TRUE)[[1L]]
      expect_length(normalized_lines, 3L)
      expect_identical(
        normalized_lines[[3L]],
        "Technical details: first validator line second validator line"
      )

      unknown <- zip_message(
        list(code = "zip_future_code", reason = "future validator detail"),
        lang = "zh"
      )
      expect_match(unknown, "无法根据此状态确认导出完整性", fixed = TRUE)
      expect_match(unknown, "技术详情：future validator detail", fixed = TRUE)
    })
  ))
})
