
test_that("CSV safe conversion handles list columns", {
  df <- data.frame(a = 1:2)
  df$b <- list(c(1,2,3), c(4,5))
  out <- csv_safe_df(df)
  expect_true(is.character(out$b))
  expect_equal(out$b[1], "1;2;3")
})

test_that("CSV safe conversion escapes spreadsheet formulas in text columns", {
  df <- data.frame(
    label = c("=cmd", "+SUM(A1:A2)", " ok", "@hidden"),
    numeric_value = c(-1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  df$list_value <- list(c("=nested", "plain"), "safe")
  out <- csv_safe_df(df)
  expect_equal(out$label[1], "'=cmd")
  expect_equal(out$label[2], "'+SUM(A1:A2)")
  expect_equal(out$label[3], " ok")
  expect_equal(out$label[4], "'@hidden")
  expect_equal(out$list_value[1], "'=nested;plain")
  expect_equal(out$numeric_value[1], -1)
})

test_that("wide labeled CSV export can drop columns with no data below the header", {
  df <- data.frame(
    timestamp = c(0, 1, 2),
    blank_label = c("", " ", NA_character_),
    all_na_score = c(NA_real_, NA_real_, NA_real_),
    zero_is_data = c(0, 0, 0),
    false_is_data = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  df$list_empty <- list(NULL, character(0), NA_character_)
  df$pattern_auto <- c("", "tonic", "")

  out <- stpd_drop_empty_columns(df)
  expect_true(all(c("timestamp", "zero_is_data", "false_is_data", "pattern_auto") %in% names(out)))
  expect_false(any(c("blank_label", "all_na_score", "list_empty") %in% names(out)))
})

test_that("HTML escaping protects Plotly hover text fields", {
  expect_equal(
    stpd_html_escape("train <A> & \"B\""),
    "train &lt;A&gt; &amp; &quot;B&quot;"
  )
})
