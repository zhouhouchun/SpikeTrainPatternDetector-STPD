gate_b_science_fixture <- local({
  fixtures <- new.env(parent=emptyenv())
  function(with_overlap=TRUE) {
    key <- if (isTRUE(with_overlap)) "overlap" else "empty"
    if (!exists(key,envir=fixtures,inherits=FALSE)) {
      assign(key,stpd_gate_b_v3_phase1_minimal_product(with_overlap),
             envir=fixtures)
    }
    unserialize(serialize(get(key,envir=fixtures,inherits=FALSE),NULL))
  }
})

test_that("Gate B v3 scientific closure accepts the frozen complete prototypes", {
  expect_true(stpd_gate_b_v3_validate_product_prototype(
    gate_b_science_fixture(FALSE)))
  expect_true(stpd_gate_b_v3_validate_product_prototype(
    gate_b_science_fixture(TRUE)))
  hft <- stpd_gate_b_v3_phase1_state_only_product("high_regular")
  expect_true(stpd_gate_b_v3_validate_product_prototype(hft))
  expect_identical(hft$state_episodes$state_class,
                   "high_frequency_tonic")
  tonic <- stpd_gate_b_v3_phase1_state_only_product("non_high_regular")
  expect_true(stpd_gate_b_v3_validate_product_prototype(tonic))
  expect_identical(tonic$state_episodes$state_class,"tonic")
})

test_that("Gate B v3.2 overlays Burst on HFT and Tonic non-destructively", {
  for (profile in c("high_regular", "non_high_regular")) {
    product <- stpd_gate_b_v3_phase1_minimal_product(
      with_overlap = TRUE, state_profile = profile
    )
    expect_true(stpd_gate_b_v3_2_validate_product_prototype(product))
    expect_equal(nrow(product$event_state_relationships), 1L)
    expect_equal(nrow(product$event_interrupted_state_relationships), 0L)
    expect_true(product$state_episodes$state_class %in%
      c("high_frequency_tonic", "tonic"))
  }
  expect_identical(
    stpd_gate_b_v3_2_compatibility_action(
      "event", "burst_event", "state", "high_frequency_tonic"
    )$action,
    "coexist_non_destructive"
  )
  expect_identical(
    stpd_gate_b_v3_2_compatibility_action(
      "event", "burst_event", "state", "tonic"
    )$action,
    "coexist_non_destructive"
  )
  expect_identical(
    stpd_gate_b_v3_2_schema_version(),
    "stpd_multitrack_v3_2_semantic_overlay"
  )
})

test_that("State-axis statistics are recomputed from the per-ISI spine", {
  product <- gate_b_science_fixture(TRUE)
  product$state_axis_evidence$frequency_stat <- 1
  product <- stpd_gate_b_v3_phase1_reseal_product(product)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(product),
    "State-axis statistics differ"
  )
})

test_that("State-axis gates are bound to effective threshold instances", {
  product <- gate_b_science_fixture(TRUE)
  product$state_axis_evidence$frequency_pass_bound <- 49
  product <- stpd_gate_b_v3_phase1_reseal_product(product)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(product),
    "State-axis statistics differ"
  )

  payload_attack <- gate_b_science_fixture(TRUE)
  payload_attack$state_axis_evidence$frequency_gray_margin <- 4
  payload_attack <- stpd_gate_b_v3_phase1_reseal_product(payload_attack)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(payload_attack),
    "State-axis statistics differ"
  )
})

test_that("Event modifier statistics and evidence identity are recomputed", {
  product <- gate_b_science_fixture(TRUE)
  product$event_modifier_evidence$statistic_value <- c(999, -999)
  product$event_modifier_evidence$pass_bound <- c(998, -998)
  product <- stpd_gate_b_v3_phase1_reseal_product(product)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(product),
    "Event modifier statistics or content identity"
  )
})

test_that("Event, Gap, and relationship geometry cannot be coordinated away", {
  product <- gate_b_science_fixture(TRUE)

  missing_projection <- product
  missing_projection$per_isi$event_id[] <- NA_character_
  missing_projection <- stpd_gate_b_v3_phase1_reseal_product(missing_projection)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(missing_projection),
    "per-ISI Event/Gap/boundary projection"
  )

  missing_accepted_candidate <- product
  missing_accepted_candidate$event_candidates$candidate_decision <-
    "rejected"
  missing_accepted_candidate <- stpd_gate_b_v3_phase1_reseal_product(
    missing_accepted_candidate)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(missing_accepted_candidate),
    "accepted Event-candidate expected set"
  )

  wrong_overlap <- product
  wrong_overlap$event_state_relationships$episode_overlap_n_isi <- 1L
  wrong_overlap$event_state_relationships$direct_support_overlap_n_isi <- 1L
  wrong_overlap$event_state_relationships$episode_overlap_sec <- 0.005
  wrong_overlap$event_state_relationships$direct_support_overlap_sec <- 0.005
  wrong_overlap$event_state_relationships$event_covered_by_episode_time_fraction <- 0.25
  wrong_overlap$event_state_relationships$event_covered_by_direct_support_time_fraction <- 0.25
  wrong_overlap$event_state_relationships$state_envelope_covered_by_event_time_fraction <- 0.25
  wrong_overlap$event_state_relationships$state_direct_support_covered_by_event_time_fraction <- 0.25
  wrong_overlap$event_state_relationships$event_covered_by_episode_isi_fraction <- 0.5
  wrong_overlap$event_state_relationships$event_covered_by_direct_support_isi_fraction <- 0.5
  wrong_overlap$event_state_relationships$state_envelope_covered_by_event_isi_fraction <- 0.5
  wrong_overlap$event_state_relationships$state_direct_support_covered_by_event_isi_fraction <- 0.5
  wrong_overlap <- stpd_gate_b_v3_phase1_reseal_product(wrong_overlap)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(wrong_overlap),
    "relationship geometry differs"
  )
})

test_that("Gap candidate family and accepted Pause projection are closed", {
  product <- gate_b_science_fixture(TRUE)
  expect_gt(nrow(product$gap_candidates), 0L)

  deleted_negative <- product
  deleted_gap_candidate_id <- deleted_negative$gap_candidates$gap_candidate_id[[1L]]
  deleted_negative$gap_candidates <-
    deleted_negative$gap_candidates[-1L, , drop = FALSE]
  # Coordinate the obvious FK-dependent deletion so this attack reaches the
  # independent candidate-universe reconstruction rather than failing earlier
  # at the polymorphic binding FK.
  deleted_negative$threshold_instance_bindings <-
    deleted_negative$threshold_instance_bindings[
      deleted_negative$threshold_instance_bindings$consumer_id !=
        deleted_gap_candidate_id,,drop=FALSE]
  deleted_negative <- stpd_gate_b_v3_phase1_reseal_product(deleted_negative)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(deleted_negative),
    "Gap-candidate universe"
  )

  impossible_bh <- product
  impossible_bh$gap_candidates$adjusted_q_value[[1L]] <- 0.001
  impossible_bh <- stpd_gate_b_v3_phase1_reseal_product(impossible_bh)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(impossible_bh),
    "BH values differ"
  )

  # Coordinated attack: split one complete train/policy/epoch family into
  # singleton families, recompute each q value, and reseal the whole product.
  # The validator must reconstruct the family partition from parent fields,
  # rather than accepting the submitted family ids.
  split_family <- product
  for (i in seq_len(nrow(split_family$gap_candidates))) {
    row <- split_family$gap_candidates[i,,drop=FALSE]
    split_family$gap_candidates$multiple_testing_family_id[[i]] <-
      stpd_gate_b_v3_entity_id("multiple_testing_family",list(
        detection_root_id=row$detection_root_id[[1L]],
        train=row$train[[1L]],
        gap_policy_registry_id=row$gap_policy_registry_id[[1L]],
        scientific_context_epoch_id=if (
          is.na(row$scientific_context_epoch_id[[1L]])) NULL else
            row$scientific_context_epoch_id[[1L]],
        sorted_gap_candidate_ids=list(row$gap_candidate_id[[1L]])),
        stpd_gate_b_v3_phase1_bundle())
    split_family$gap_candidates$adjusted_q_value[[i]] <-
      split_family$gap_candidates$raw_p_value[[i]]
  }
  split_family <- stpd_gate_b_v3_phase1_reseal_product(split_family)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(split_family),
    "family identity is incomplete"
  )

  wrong_reference <- product
  wrong_reference$gap_candidates$local_reference_sec[[1L]] <- 0.01
  wrong_reference <- stpd_gate_b_v3_phase1_reseal_product(wrong_reference)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(wrong_reference),
    "local evidence differs"
  )
})

test_that("structure-first contrast Burst generation is frozen and executable", {
  registry <- stpd_gate_b_v3_phase1_registry(FALSE)
  rule <- registry[
    registry$registry_domain == "event_candidate_rule" &
      registry$code == "structure_first_local_flank_contrast_v1",,
    drop=FALSE]
  expect_equal(nrow(rule),1L)
  definition <- jsonlite::fromJSON(rule$semantic_definition_json[[1L]],
                                   simplifyVector=FALSE)
  expected_paths <- c(
    "event_core.min_spikes",
    "event_core.classic_max_spikes",
    "detector.artifact_min_valid_isi_sec",
    "spiketrainpattern.burst.structure_first_enabled",
    "spiketrainpattern.burst.structure_first_min_isi_count",
    "spiketrainpattern.burst.structure_first_max_isi_count",
    "spiketrainpattern.burst.structure_first_contrast_min",
    "spiketrainpattern.burst.structure_first_geom_contrast_min",
    "spiketrainpattern.burst.structure_first_compactness_quantile",
    "spiketrainpattern.burst.structure_first_background_fraction",
    "spiketrainpattern.burst.structure_first_min_train_valid_isi",
    "spiketrainpattern.burst.structure_first_max_internal_tail_ratio",
    "spiketrainpattern.burst.structure_first_allow_endpoint",
    "spiketrainpattern.burst.allow_one_sided_as_canonical")
  expect_setequal(unlist(definition$parameter_paths,use.names=FALSE),
                  expected_paths)
  expect_identical(definition$formula_or_state_machine$seed_or_bridge_gate_used,
                   FALSE)
  expect_identical(definition$formula_or_state_machine$label_access,"none")

  make_train <- function(isi) data.frame(
    idx=seq_len(length(isi)+1L),timestamp_sec=c(0,cumsum(isi)),
    ISI_sec=c(NA_real_,isi),pattern_manual="",pattern_manual_negative="",
    pattern_auto="",stringsAsFactors=FALSE)
  params <- default_params_sec()
  dat <- make_train(c(.100,.100,.029,.030,.028,.100,.110,.100,.090,.100))
  vp <- stpd_event_core_params_impl(dat,params,min_isi_sec=.0009)
  vp$seed_low <- .001; vp$seed_high <- .010; vp$bridge_high <- .015
  generated <- stpd_event_core_structure_first_burst_candidates(
    dat,params,vp,min_isi_sec=.0009,train="test_train")
  target <- generated[generated$start_isi==4L & generated$end_isi==6L,,drop=FALSE]
  expect_equal(nrow(target),1L)
  expect_identical(target$final_label,"burst")
  expect_true(target$structure_first_no_seed_band_gate)
  expect_gte(target$pre_ratio_q90,3)
  expect_gte(target$post_ratio_q90,3)

  three_spikes <- make_train(c(.100,.030,.028,.100,.100,.100,.100,.100))
  three_spikes_vp <- stpd_event_core_params_impl(
    three_spikes,params,min_isi_sec=.0009)
  expect_equal(nrow(stpd_event_core_structure_first_burst_candidates(
    three_spikes,params,three_spikes_vp,min_isi_sec=.0009,
    train="test_train")),0L)

  homogeneous <- make_train(rep(.030,20L))
  homogeneous_vp <- stpd_event_core_params_impl(
    homogeneous,params,min_isi_sec=.0009)
  expect_equal(nrow(stpd_event_core_structure_first_burst_candidates(
    homogeneous,params,homogeneous_vp,min_isi_sec=.0009,
    train="test_train")),0L)

  product <- gate_b_science_fixture(TRUE)
  candidate <- product$event_candidates$event_candidate_id[[1L]]
  candidate_domain <- stpd_gate_b_v3_domain_id(product,"event_candidate")
  bound <- product$threshold_instance_bindings[
    product$threshold_instance_bindings$consumer_domain_id==candidate_domain &
      product$threshold_instance_bindings$consumer_id==candidate,,drop=FALSE]
  expect_setequal(bound$path,expected_paths)

  expected <- stpd_gate_b_v3_structure_first_expected_candidates(product)
  expect_identical(
    expected$event_candidate_id,
    product$event_candidates$event_candidate_id)
  expect_silent(stpd_gate_b_v3_validate_product_prototype(product))

  # Parent replay, not submitted candidate IDs, defines the complete universe.
  # Coordinated resealing after deleting a real candidate must not make the
  # scientific closure accept the altered set.
  deleted <- product
  deleted$event_candidates <- deleted$event_candidates[0,,drop=FALSE]
  deleted$threshold_instance_bindings <-
    deleted$threshold_instance_bindings[
      deleted$threshold_instance_bindings$consumer_id != candidate,,drop=FALSE]
  deleted <- stpd_gate_b_v3_phase1_reseal_product(deleted)
  expect_error(
    stpd_gate_b_v3_validate_structure_first_event_contract(deleted),
    "candidate universe differs from parent replay"
  )

  # The inverse attack inserts a content-addressed but generator-impossible
  # endpoint candidate and reseals all table hashes.  Replay must still reject.
  inserted <- product
  fake <- inserted$event_candidates[1,,drop=FALSE]
  fake$start_isi <- 3L
  fake$end_isi <- 3L
  fake$n_isi <- 1L
  fake$n_spikes <- 2L
  fake$start_time_sec <- inserted$per_isi$isi_start_time_sec[
    inserted$per_isi$isi_index==3L]
  fake$end_time_sec <- inserted$per_isi$isi_end_time_sec[
    inserted$per_isi$isi_index==3L]
  fake$duration_sec <- inserted$per_isi$isi_sec[
    inserted$per_isi$isi_index==3L]
  fake$source_candidate_key_hash <- stpd_gate_b_v3_hash_raw(
    "stpd-structure-first-source-candidate-key-v1",
    stpd_gate_b_v3_canonical_json(list(
      train=fake$train[[1L]],start_isi=3L,end_isi=3L,
      candidate_generation_rule_registry_id=
        fake$candidate_generation_rule_registry_id[[1L]])))
  fake$event_candidate_id <- stpd_gate_b_v3_entity_id(
    "event_candidate",list(
      detection_root_id=fake$detection_root_id[[1L]],
      train=fake$train[[1L]],start_isi=3L,end_isi=3L,
      candidate_generation_rule_registry_id=
        fake$candidate_generation_rule_registry_id[[1L]],
      source_candidate_key_hash=fake$source_candidate_key_hash[[1L]]),
    stpd_gate_b_v3_phase1_bundle())
  inserted$event_candidates <- rbind(inserted$event_candidates,fake)
  inserted <- stpd_gate_b_v3_phase1_reseal_product(inserted)
  expect_error(
    stpd_gate_b_v3_validate_structure_first_event_contract(inserted),
    "candidate universe differs from parent replay"
  )
})

test_that("accepted scientific consumers require domain-correct threshold bindings", {
  product <- gate_b_science_fixture(TRUE)
  product$threshold_instance_bindings <-
    product$threshold_instance_bindings[0, , drop = FALSE]
  product <- stpd_gate_b_v3_phase1_reseal_product(product)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(product),
    "threshold binding"
  )
})

test_that("held-out partitions are patient-exclusive with session fallback", {
  source_a <- strrep("1", 64L)
  source_b <- strrep("2", 64L)
  shared_session <- strrep("b", 64L)
  memberships <- data.frame(
    group_role = c("calibration", "validation"),
    patient_group_id_hash = c(NA_character_, NA_character_),
    session_group_id_hash = rep(shared_session, 2L),
    source_input_hash = c(source_a, source_b),
    stringsAsFactors = FALSE
  )
  manifest <- data.frame(
    normalized_timestamp_bytes_sha256 = c(source_a, source_b),
    patient_group_id_hash = c(NA_character_, NA_character_),
    session_group_id_hash = rep(shared_session, 2L),
    stringsAsFactors = FALSE)
  expect_error(
    stpd_gate_b_v3_validate_patient_holdout(
      list(partition_memberships = memberships,
           normalized_input_manifest = manifest)),
    "shares a patient or session"
  )

  memberships$session_group_id_hash[[2L]] <- strrep("c", 64L)
  expect_error(
    stpd_gate_b_v3_validate_patient_holdout(
      list(partition_memberships = memberships,
           normalized_input_manifest = manifest)),
    "not bound to its normalized patient/session source row"
  )
})
