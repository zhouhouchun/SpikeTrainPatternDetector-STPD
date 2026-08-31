stpd_static_ui_contract_decode_html <- function(x) {
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

stpd_static_ui_contract_sources <- function() {
  html <- htmltools::renderTags(ui)$html
  html <- gsub(
    "(?is)<(script|style|noscript)\\b[^>]*>.*?</\\1>",
    "",
    html,
    perl = TRUE
  )
  text <- unlist(
    strsplit(
      gsub("(?s)<[^>]+>", "\n", html, perl = TRUE),
      "\n",
      fixed = TRUE
    ),
    use.names = FALSE
  )
  attribute_matches <- regmatches(
    html,
    gregexpr(
      "(?:placeholder|title|aria-label|data-original-title|value)=\"[^\"]*\"",
      html,
      perl = TRUE
    )
  )[[1L]]
  attributes <- sub("^[^=]+=\"", "", attribute_matches)
  attributes <- sub("\"$", "", attributes)
  source <- unique(trimws(stpd_static_ui_contract_decode_html(c(text, attributes))))
  source[
    nzchar(source) & vapply(source, stpd_i18n_contains_cjk, logical(1))
  ]
}

test_that("the complete static UI has exact, lossless English coverage", {
  source <- stpd_static_ui_contract_sources()
  expect_length(source, 829L)

  english <- vapply(
    source,
    stpd_i18n_translate_text,
    character(1),
    lang = "en"
  )
  residual <- source[
    vapply(english, stpd_i18n_contains_cjk, logical(1))
  ]
  expect_true(
    length(residual) == 0L,
    info = paste(utils::head(residual, 10L), collapse = " | ")
  )
  expect_true(all(nzchar(trimws(english))))
})

test_that("the exact dictionary has unique keys and preserves technical tokens", {
  exact <- stpd_i18n_exact_dictionary()
  additions <- stpd_i18n_static_ui_exact_dictionary()

  expect_gte(length(exact), 813L)
  expect_identical(anyDuplicated(names(exact)), 0L)
  expect_false(any(is.na(names(exact))) || any(!nzchar(trimws(names(exact)))))
  expect_false(any(is.na(exact)) || any(!nzchar(trimws(unname(exact)))))

  expect_length(additions, 235L)
  expect_identical(anyDuplicated(names(additions)), 0L)
  expect_true(all(vapply(names(additions), stpd_i18n_contains_cjk, logical(1))))
  expect_false(any(vapply(unname(additions), stpd_i18n_contains_cjk, logical(1))))

  technical_tokens <- c(
    "AUTO", "MANUAL", "FINAL", "ISI", "train", "burst", "tonic",
    "pause", "HF", "IoU", "UI", "ZIP", "YAML", "API",
    "possible_burst", "burst-family", "logISIH", "newBD", "timestamp",
    "seed", "bridge", "stpd_detect()", "run_detector()"
  )
  for (token in technical_tokens) {
    uses_token <- grepl(token, names(additions), fixed = TRUE)
    if (!any(uses_token)) next
    expect_true(
      all(grepl(
        tolower(token),
        tolower(unname(additions[uses_token])),
        fixed = TRUE
      )),
      info = token
    )
  }
})
