test_that("scientific validation defaults to range-blind evaluation", {
  html <- htmltools::renderTags(ui)$html
  input_tag <- regmatches(
    html,
    regexpr(
      "<input[^>]*id=\\\"sci_val_use_learned_ranges\\\"[^>]*>",
      html,
      perl = TRUE
    )
  )

  expect_length(input_tag, 1L)
  expect_match(input_tag, "type=\\\"checkbox\\\"")
  expect_false(grepl("\\bchecked(?:=|\\s|>)", input_tag, perl = TRUE))
})
