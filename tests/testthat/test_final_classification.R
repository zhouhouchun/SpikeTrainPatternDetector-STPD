
test_that("final classifier preserves strict possible_burst semantics", {
  p <- default_params()
  f <- data.frame(candidate_id="c1", candidate_source="boundary_burst", final_candidate_class="burst",
                  one_flank_ratio=3, refractory_suspect_n=0, stringsAsFactors=FALSE)
  out <- final_classify_candidate(f, p)
  expect_equal(out$final_class, "possible_burst")
  expect_true(out$review_required)
})

test_that("refractory suspect demotes high confidence burst by default", {
  p <- default_params()
  p$detector$refractory_suspect_action <- "demote_to_possible"
  f <- data.frame(candidate_id="c2", candidate_source="structure_seed_bridge", final_candidate_class="burst",
                  refractory_suspect_n=1, stringsAsFactors=FALSE)
  out <- final_classify_candidate(f, p)
  expect_equal(out$final_class, "possible_burst")
})
