test_that("publication-readiness framework freezes 12 serial non-authoritative stages", {
  framework <- stpd_publication_readiness_framework()
  expect_identical(framework$schema,
                   "stpd_publication_readiness_framework_v1")
  expect_identical(as.integer(framework$stage_count), 12L)
  expect_identical(
    vapply(framework$stages, `[[`, character(1), "stage_id"),
    stpd_publication_readiness_required_stage_ids()
  )
  expect_identical(framework$current_stage_id, "FRAMEWORK_COMPLETE")
  expect_true(all(vapply(framework$stages, function(stage) {
    identical(stage$framework_status, "reviewed_framework") &&
      identical(stage$review_status, "passed")
  }, logical(1))))
  expect_true(all(vapply(framework$stages, function(stage) {
    identical(stage$authority_scope,
              "none_framework_only_no_completion_claim") &&
      !identical(stage$scientific_completion_status, "complete")
  }, logical(1))))
})

test_that("all 12 minimum stage packets are ordered and non-authoritative", {
  packets <- stpd_publication_readiness_stage_packets()
  expect_identical(
    vapply(packets$packets, `[[`, character(1), "stage_id"),
    stpd_publication_readiness_required_stage_ids()
  )
  expect_true(all(vapply(packets$packets, function(packet) {
    identical(packet$authority_granted, FALSE) &&
      length(packet$artifact_contracts) >= 3L &&
      length(packet$review_evidence) >= 3L &&
      length(packet$deferred_work) >= 2L
  }, logical(1))))

  attack <- unserialize(serialize(packets, NULL))
  attack$packets[[2L]]$authority_granted <- TRUE
  expect_error(
    stpd_publication_readiness_validate_packets(attack),
    "incomplete or grants authority"
  )

  meaningless <- unserialize(serialize(packets, NULL))
  meaningless$packets[[3L]]$artifact_contracts <- rep(
    "meaningless_placeholder", 4L)
  expect_error(
    stpd_publication_readiness_validate_packets(meaningless),
    "too short or duplicated|does not exactly bind"
  )

  mismatched <- unserialize(serialize(packets, NULL))
  mismatched$packets[[7L]]$review_evidence[[1L]] <- "unverified"
  expect_error(
    stpd_publication_readiness_validate_packets(mismatched),
    "does not exactly bind"
  )

  extra_packet_field <- unserialize(serialize(packets, NULL))
  extra_packet_field$unreviewed_override <- TRUE
  expect_error(
    stpd_publication_readiness_validate_packets(extra_packet_field),
    "invalid identity or scope"
  )
})

test_that("FRAMEWORK_COMPLETE is hash-bound to all 12 review receipts", {
  framework <- stpd_publication_readiness_framework()
  packets <- stpd_publication_readiness_stage_packets()
  receipts <- stpd_publication_readiness_review_receipts(
    verify_source_reviews = "always")
  expect_identical(framework$current_stage_id, "FRAMEWORK_COMPLETE")
  expect_identical(
    vapply(receipts$receipts, `[[`, character(1), "stage_id"),
    stpd_publication_readiness_required_stage_ids()
  )
  expect_true(all(vapply(receipts$receipts, function(receipt) {
    identical(receipt$review_status, "bounded_framework_review_passed") &&
      identical(receipt$authority_granted, FALSE)
  }, logical(1))))

  tampered_hash <- unserialize(serialize(receipts, NULL))
  tampered_hash$framework_sha256 <- paste0(
    "0", substring(tampered_hash$framework_sha256, 2L))
  expect_error(
    stpd_publication_readiness_validate_receipts(
      tampered_hash, framework, packets,
      stpd_publication_readiness_framework_path(),
      stpd_publication_readiness_packets_path(), "always"),
    "not receipt-bound"
  )

  tampered_binding <- unserialize(serialize(receipts, NULL))
  tampered_binding$receipts[[8L]]$passed_stop_check_ids[[1L]] <-
    "unverified"
  expect_error(
    stpd_publication_readiness_validate_receipts(
      tampered_binding, framework, packets,
      stpd_publication_readiness_framework_path(),
      stpd_publication_readiness_packets_path(), "always"),
    "does not exactly close"
  )

  missing_authority_boundary <- unserialize(serialize(receipts, NULL))
  missing_authority_boundary$receipts[[12L]]$authority_granted <- TRUE
  expect_error(
    stpd_publication_readiness_validate_receipts(
      missing_authority_boundary, framework, packets,
      stpd_publication_readiness_framework_path(),
      stpd_publication_readiness_packets_path(), "always"),
    "incomplete or unsafe"
  )
})

test_that("Gate 1B root closure registry is ordered and cannot overclaim", {
  framework <- stpd_publication_readiness_framework()
  roots <- framework$gate1b_remaining_roots
  expect_identical(
    vapply(roots, `[[`, character(1), "root_id"),
    stpd_publication_readiness_required_gate1b_roots()
  )
  expected_dependencies <- c(
    "post_ownership_pause_proposal_root",
    stpd_publication_readiness_required_gate1b_roots()[-6L]
  )
  expect_identical(
    vapply(roots, `[[`, character(1), "depends_on"),
    expected_dependencies
  )
  expect_true(all(vapply(roots, function(root) {
    identical(root$framework_status,
              "implementation_reviewed") &&
      identical(
        root$scientific_closure_status,
        "bounded_observer_root_closed_no_performance_claim"
      ) &&
      identical(root$publication_authority, FALSE) &&
      grepl("^[0-9a-f]{64}$", root$closure_record_sha256) &&
      length(root$minimum_review_checks) >= 4L
  }, logical(1))))
  expect_true(stpd_publication_readiness_validate_gate1b_closure_records(
    framework, stpd_publication_readiness_framework_path(), "always"
  ))
})

test_that("framework validator rejects skipped review and premature authority", {
  framework <- stpd_publication_readiness_framework()

  skipped <- unserialize(serialize(framework, NULL))
  skipped$current_stage_id <- "PR-12"
  skipped$stages[[11L]]$framework_status <- "framework_pending"
  skipped$stages[[11L]]$review_status <- "not_started"
  skipped$stages[[12L]]$framework_status <- "ready_for_review"
  skipped$stages[[12L]]$review_status <- "pending"
  expect_error(
    stpd_publication_readiness_validate(skipped),
    "earlier publication-readiness framework lacks passed review"
  )

  incomplete <- unserialize(serialize(framework, NULL))
  incomplete$stages[[12L]]$framework_status <- "ready_for_review"
  incomplete$stages[[12L]]$review_status <- "pending"
  expect_error(
    stpd_publication_readiness_validate(incomplete),
    "Completed publication-readiness framework has an unreviewed stage"
  )

  authority <- unserialize(serialize(framework, NULL))
  authority$stages[[1L]]$authority_scope <- "publication_authoritative"
  expect_error(
    stpd_publication_readiness_validate(authority),
    "status or authority is invalid"
  )

  root_authority <- unserialize(serialize(framework, NULL))
  root_authority$gate1b_remaining_roots[[1L]]$publication_authority <- TRUE
  expect_error(
    stpd_publication_readiness_validate(root_authority),
    "root closure, dependency, or authority is invalid"
  )

  root_hash <- unserialize(serialize(framework, NULL))
  root_hash$gate1b_remaining_roots[[1L]]$closure_record_sha256 <-
    paste0("0", substring(
      root_hash$gate1b_remaining_roots[[1L]]$closure_record_sha256, 2L
    ))
  expect_error(
    stpd_publication_readiness_validate_gate1b_closure_records(
      root_hash, stpd_publication_readiness_framework_path(), "always"
    ),
    "closure record hash differs"
  )

  hidden_override <- unserialize(serialize(framework, NULL))
  hidden_override$unreviewed_override <- TRUE
  expect_error(
    stpd_publication_readiness_validate(hidden_override),
    "structurally incomplete"
  )
})

test_that("framework loading is detector-free and RNG/options neutral", {
  set.seed(48121)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()

  first <- stpd_publication_readiness_framework()
  second <- stpd_publication_readiness_framework()

  expect_identical(
    serialize(first, NULL, version = 3L),
    serialize(second, NULL, version = 3L)
  )
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
})
