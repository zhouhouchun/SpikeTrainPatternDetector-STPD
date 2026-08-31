gb3_hash64 <- function(letter = "a") strrep(letter, 64L)

gb3_train_id <- function(letter = "a") paste0("tr_", gb3_hash64(letter))

gb3_field_value <- function(field, spec, bundle) {
  if (!is.null(field$constant) && length(field$constant) == 1L &&
      !is.na(field$constant)) return(field$constant)
  if (!identical(field$enum, "none")) {
    return(unlist(bundle$enums[[field$enum]], use.names = FALSE)[[1L]])
  }
  if (identical(field$type, "int")) return(1L)
  if (identical(field$type, "dbl")) return(1)
  if (identical(field$type, "lgl")) return(FALSE)
  if (identical(field$type, "json")) return("{}")
  if (identical(field$type, "lst")) return(list(character()))
  if (identical(field$name, "train")) return(gb3_train_id())
  if (identical(field$name, "not_after_utc"))
    return("2027-08-23T12:34:56.123456Z")
  if (grepl("_utc$", field$name)) return("2026-08-23T12:34:56.123456Z")

  resolution <- spec$id_resolution[[field$name]]
  if (!is.null(resolution)) {
    kind <- resolution$resolution
    if (identical(kind, "external_audit_identifier")) {
      if (identical(field$name, "execution_id")) {
        return("00000000-0000-4000-8000-000000000000")
      }
      if (field$name %in% c("detection_root_id", "target_detection_root_id")) {
        return(paste0("dr_", gb3_hash64()))
      }
      if (identical(field$name, "contract_id")) return("GB3-TEST-001")
      if (identical(field$name, "fixture_id")) return("fixture_x")
      if (field$name %in% c("key_id", "native_build_id")) return("x")
      return(gb3_hash64())
    }
    if (identical(kind, "signed_toolchain_identifier")) return("x")
    if (kind %in% c("generated_entity", "generated_special_contract_id")) {
      return(paste0(resolution$prefix, gb3_hash64()))
    }
    if (identical(kind, "registry_foreign_key")) {
      return(paste0("reg_", gb3_hash64()))
    }
    return(paste0("x_", gb3_hash64()))
  }
  if (grepl("(_sha256|_hash)$", field$name)) return(gb3_hash64())
  "x"
}

gb3_example_row <- function(table_name,
                            bundle = stpd_gate_b_v3_phase1_bundle()) {
  if (identical(table_name, "label_blind_contract_manifest")) {
    train <- gb3_train_id("a")
    return(stpd_gate_b_v3_label_blind_input(list(trains=setNames(
      list(data.frame(timestamp=c(0,0.1),stringsAsFactors=FALSE)),train)
    ))$label_blind_contract_manifest)
  }
  spec <- bundle$tables[[table_name]]
  prototype <- stpd_gate_b_v3_empty_table(table_name, bundle)
  values <- lapply(spec$columns, gb3_field_value,
                   spec = spec, bundle = bundle)
  columns <- lapply(seq_along(values), function(i) {
    template <- prototype[[i]]
    value <- values[[i]]
    if (is.list(template)) return(I(list(value[[1L]])))
    if (is.integer(template)) return(as.integer(value))
    if (is.double(template)) return(as.double(value))
    if (is.logical(template)) return(as.logical(value))
    as.character(value)
  })
  names(columns) <- names(prototype)
  out <- as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)

  if (identical(table_name, "product_status_envelope")) {
    out$product_kind <- "auto"
    out$product_status <- "candidate_pending"
    out$gate_b_status <- "pending"
    out$gate_c_status <- "pending"
    out$authority_scope <- "none_candidate"
    out$record_authoritative <- FALSE
    out$biological_ground_truth <- FALSE
    out$reference_standard_status <- "none"
    out$detector_performance_eligible <- FALSE
    out$integrity_level <- "detached_internal_integrity"
    out$failure_stage <- NA_character_
    out$failure_code_registry_id <- NA_character_
    out$requires_redetection_reason_registry_id <- NA_character_
    out$release_attestation_hash <- NA_character_
    out$product_attestation_hash <- NA_character_
    out$reference_manifest_hash <- NA_character_
  }
  if (identical(table_name, "threshold_policies")) {
    out$threshold_source_class <- "fixed_preregistered"
    out$adaptation_unit <- "none"
    out$label_access <- "none"
    out$partition_id <- NA_character_
  }
  if (identical(table_name, "threshold_instances")) {
    out$application_scope <- "global"
    out$train <- NA_character_
    out$session_group_id_hash <- NA_character_
    out$dataset_batch_id_hash <- NA_character_
    out$value_type <- "double"
    out$value_num <- 1
    out$value_chr <- NA_character_
  }
  if ("geometry_count_duration_closure_v1" %in%
      unlist(spec$row_constraints, use.names = FALSE)) {
    out$start_isi <- 1L
    out$end_isi <- 1L
    out$n_isi <- 1L
    out$n_spikes <- 2L
    out$start_time_sec <- 0
    out$end_time_sec <- 0.1
    out$duration_sec <- 0.1
  }
  if (identical(table_name, "state_segments")) {
    out$segment_role <- "direct_support"
    out$source_state_candidate_id <- paste0("sc_", gb3_hash64("b"))
    out$state_axis_evidence_id <- paste0("sx_", gb3_hash64("c"))
    out$connector_decision_id <- NA_character_
  }
  if (identical(table_name, "state_episodes")) {
    out$state_frequency_class <- "high"
    out$state_regularity_class <- "irregular"
    out$state_class <- "high_frequency_irregular_state"
    out$frequency_evidence_status <- "pass"
    out$regularity_evidence_status <- "pass"
  }
  if (identical(table_name, "per_isi")) {
    out$state_episode_membership <- FALSE
    out$state_episode_id <- NA_character_
    out$episode_state_class <- NA_character_
    out$state_segment_id <- NA_character_
    out$state_segment_role <- NA_character_
    out$active_state_segment_id <- NA_character_
    out$active_state_class <- NA_character_
    out$state_direct_support <- FALSE
  }
  if (identical(table_name, "gaps")) {
    out$gap_evidence_status <- "pass"
    out$raw_p_value <- 0.05
    out$adjusted_q_value <- 0.05
  }
  if (identical(table_name, "events")) {
    if (identical(out$extent_modifier, "unresolved"))
      out$extent_evidence_status <- "insufficient"
    if (identical(out$frequency_modifier, "unresolved"))
      out$frequency_evidence_status <- "insufficient"
  }
  if (identical(table_name, "event_modifier_evidence")) {
    out$modifier_domain <- "extent"
    out$modifier_value <- "classic"
  }
  if (identical(table_name, "event_state_relationships")) {
    out$episode_overlap_n_isi <- 1L
    out$direct_support_overlap_n_isi <- 1L
    out$connector_overlap_n_isi <- 0L
    out$episode_overlap_sec <- 0.1
    out$direct_support_overlap_sec <- 0.1
    out$connector_overlap_sec <- 0
    fraction_names <- names(out)[grepl("_fraction$", names(out))]
    out[fraction_names] <- 1
  }
  if (identical(table_name, "reference_annotations")) {
    out$annotation_pass <- 1L
    out$annotation_domain <- "state_direct_support"
    blind <- c("blinded_to_auto", "blinded_to_final", "blinded_to_thresholds",
               "blinded_to_other_raters", "blinded_to_clinical_group")
    out[blind] <- TRUE
  }
  if (identical(table_name, "partition_memberships")) {
    canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                  "SpikeTrainPatternDetector")
    hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                                 "SpikeTrainPatternDetector")
    membership_payload <- list(
      group_id_hash = out$group_id_hash,
      group_role = out$group_role,
      patient_group_id_hash = if (is.na(out$patient_group_id_hash)) NULL else
        out$patient_group_id_hash,
      session_group_id_hash = out$session_group_id_hash,
      source_input_hash = out$source_input_hash
    )
    out$membership_row_hash <- hash_raw(
      "stpd-partition-membership-row-v1", canonical(membership_payload)
    )
    row_payload <- list(
      schema_version = out$schema_version,
      partition_version = out$partition_version,
      group_id_hash = out$group_id_hash,
      group_role = out$group_role,
      patient_group_id_hash = if (is.na(out$patient_group_id_hash)) NULL else
        out$patient_group_id_hash,
      session_group_id_hash = out$session_group_id_hash,
      source_input_hash = out$source_input_hash,
      membership_row_hash = out$membership_row_hash
    )
    out$partition_id <- getFromNamespace(
      "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
    )("partition", list(
      partition_version = out$partition_version,
      sorted_membership_rows_without_partition_id = list(row_payload)
    ), bundle)
  }
  if (identical(table_name, "lineage_records")) {
    canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                  "SpikeTrainPatternDetector")
    hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                                 "SpikeTrainPatternDetector")
    parent <- list(
      parent_domain_id = out$parent_domain_id,
      parent_id = out$parent_id,
      parent_product_hash = if (is.na(out$parent_product_hash)) NULL else
        out$parent_product_hash,
      lineage_action_registry_id = out$lineage_action_registry_id,
      transition_id = if (is.na(out$transition_id)) NULL else out$transition_id,
      evidence_id = out$evidence_id
    )
    out$edge_index <- 1L
    out$parent_edge_set_hash <- hash_raw(
      "stpd-lineage-parent-edge-set-v1", canonical(list(
        child_domain_id = out$child_domain_id,
        child_id = out$child_id,
        child_product_hash = if (is.na(out$child_product_hash)) NULL else
          out$child_product_hash,
        sorted_parent_edge_payloads = list(parent)
      ))
    )
    out$lineage_id <- getFromNamespace(
      "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
    )("lineage", list(
      detection_root_id = out$detection_root_id,
      child_domain_id = out$child_domain_id,
      child_id = out$child_id,
      parent_edge_set_hash = out$parent_edge_set_hash
    ), bundle)
  }

  entity_id <- getFromNamespace("stpd_gate_b_v3_entity_id",
                                "SpikeTrainPatternDetector")
  for (field in names(spec$id_resolution)) {
    resolution <- spec$id_resolution[[field]]
    if (identical(resolution$resolution, "generated_entity") &&
        field %in% unlist(spec$primary_key, use.names = FALSE)) {
      payload_fields <- unlist(
        bundle$id_payload_registry[[resolution$domain]]$fields,
        use.names = FALSE
      )
      if (all(payload_fields %in% names(out))) {
        payload <- lapply(payload_fields, function(name) out[[name]][[1L]])
        names(payload) <- payload_fields
        out[[field]] <- entity_id(resolution$domain, payload, bundle)
      }
    }
  }
  reference_specs <- list(
    reference_annotations = list(
      id = "annotation_id", prefix = "ra_",
      domain = "stpd-reference-annotation-v1",
      exclude = c("annotation_id", "created_utc")
    ),
    reference_adjudications = list(
      id = "adjudication_id", prefix = "rj_",
      domain = "stpd-reference-adjudication-v1",
      exclude = c("adjudication_id", "created_utc")
    ),
    reference_display_protocols = list(
      id = "display_protocol_id", prefix = "rd_",
      domain = "stpd-reference-display-id-v1",
      exclude = c("display_protocol_id", "protocol_payload_sha256"),
      payload_hash = "protocol_payload_sha256",
      payload_domain = "stpd-reference-display-payload-v1"
    ),
    reference_project_manifest = list(
      id = "annotation_project_id", prefix = "rp_",
      domain = "stpd-reference-project-v1",
      exclude = c("annotation_project_id", "project_payload_sha256"),
      payload_hash = "project_payload_sha256",
      payload_domain = "stpd-reference-project-payload-v1"
    )
  )
  reference_spec <- reference_specs[[table_name]]
  if (!is.null(reference_spec)) {
    canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                  "SpikeTrainPatternDetector")
    hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                                 "SpikeTrainPatternDetector")
    payload_names <- setdiff(names(out), reference_spec$exclude)
    payload <- lapply(payload_names, function(name) {
      value <- out[[name]][[1L]]
      if (length(value) == 1L && is.na(value)) NULL else value
    })
    names(payload) <- payload_names
    if (!is.null(reference_spec$payload_hash)) {
      out[[reference_spec$payload_hash]] <- hash_raw(
        reference_spec$payload_domain, canonical(payload)
      )
    }
    out[[reference_spec$id]] <- paste0(
      reference_spec$prefix,
      hash_raw(reference_spec$domain, canonical(payload))
    )
  }
  if (identical(table_name, "contract_registries")) {
    canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                  "SpikeTrainPatternDetector")
    hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                                 "SpikeTrainPatternDetector")
    out$semantic_definition_sha256 <- hash_raw(
      "stpd-registry-semantic-v1", out$semantic_definition_json
    )
    out$registry_entry_id <- paste0("reg_", hash_raw(
      "stpd-registry-entry-v1", canonical(list(
        registry_domain = out$registry_domain,
        code = out$code,
        code_version = out$code_version,
        semantic_definition_sha256 = out$semantic_definition_sha256
      ))
    ))
  }
  out
}

test_that("Gate B v3 phase-1 bundle is self-hashed and structurally complete", {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  expect_identical(bundle$plan_sha256,
                   "eb944b45becdb80633cf56508463f49a0bd4bb437502b291e55c73ee80536aa5")
  expect_identical(bundle$normative_contract_sha256,
                   "81aa7f48411e774b0b1faa45e19b45f591891bb368d72bfd06401555ee01e8f4")
  expect_length(bundle$tables, 59L)
  expect_length(bundle$product_inventory, 32L)
  expect_length(bundle$id_payload_registry, 29L)
  expect_length(bundle$status_matrix, 9L)
  expect_true(all(vapply(bundle$status_matrix, function(row) {
    identical(row$gate_c, "pending") &&
      identical(row$detector_performance_eligible, FALSE) &&
      identical(row$biological_ground_truth, FALSE)
  }, logical(1))))
  expect_true(stpd_gate_b_v3_phase1_schema_lint(bundle))

  release_nodes <- c(
    "package_build_manifest_sha256", "installed_manifest_sha256",
    "release_attestation_core_hash", "embedded_release_signature",
    "release_attestation_hash", "tarball_sha256",
    "distribution_payload_sha256", "distribution_signature"
  )
  expect_true(all(release_nodes %in% unlist(bundle$hash_dag$nodes)))
  edge_key <- vapply(bundle$hash_dag$edges, paste, collapse = "|",
                     FUN.VALUE = character(1))
  expect_true(all(c(
    "package_build_manifest_sha256|installed_manifest_sha256",
    "release_attestation_hash|tarball_sha256",
    "tarball_sha256|distribution_payload_sha256",
    "distribution_payload_sha256|distribution_signature"
  ) %in% edge_key))
  expect_false("product_anchor_hash|distribution_signature" %in% edge_key)

  reseal_dag <- function(candidate) {
    candidate$hash_dag$contract_sha256 <- NULL
    candidate$hash_dag$contract_sha256 <- digest::digest(
      jsonlite::toJSON(candidate$hash_dag, auto_unbox = TRUE,
                       null = "null", digits = NA, pretty = FALSE),
      algo = "sha256", serialize = FALSE
    )
    candidate
  }
  missing_tarball_edge <- bundle
  keys <- vapply(missing_tarball_edge$hash_dag$edges, paste,
                 collapse = "|", FUN.VALUE = character(1))
  missing_tarball_edge$hash_dag$edges <-
    missing_tarball_edge$hash_dag$edges[
      keys != "tarball_sha256|distribution_payload_sha256"
    ]
  missing_tarball_edge <- reseal_dag(missing_tarball_edge)
  expect_error(getFromNamespace("stpd_gate_b_v3_phase1_hash_dag_lint",
                                "SpikeTrainPatternDetector")(
    missing_tarball_edge
  ), "exact required edge set")

  obsolete_direct_edge <- bundle
  obsolete_direct_edge$hash_dag$edges <- c(
    obsolete_direct_edge$hash_dag$edges,
    list(c("product_anchor_hash", "distribution_signature"))
  )
  obsolete_direct_edge <- reseal_dag(obsolete_direct_edge)
  expect_error(getFromNamespace("stpd_gate_b_v3_phase1_hash_dag_lint",
                                "SpikeTrainPatternDetector")(
    obsolete_direct_edge
  ), "exact required edge set")

  wrong_dag_schema <- bundle
  wrong_dag_schema$hash_dag$dag_schema <- "attacker_resealed_schema"
  wrong_dag_schema <- reseal_dag(wrong_dag_schema)
  expect_error(getFromNamespace("stpd_gate_b_v3_phase1_hash_dag_lint",
                                "SpikeTrainPatternDetector")(
    wrong_dag_schema
  ), "frozen exact schema")

  missing_forbidden_policy <- bundle
  missing_forbidden_policy$hash_dag$forbidden_back_edges <- list()
  missing_forbidden_policy <- reseal_dag(missing_forbidden_policy)
  expect_error(getFromNamespace("stpd_gate_b_v3_phase1_hash_dag_lint",
                                "SpikeTrainPatternDetector")(
    missing_forbidden_policy
  ), "forbidden hash-edge set is not exact")

  missing_edge <- bundle
  missing_edge$hash_dag$edges <- missing_edge$hash_dag$edges[-1L]
  expect_error(getFromNamespace("stpd_gate_b_v3_phase1_hash_dag_lint",
                                "SpikeTrainPatternDetector")(missing_edge),
               "contract digest")
  added_edge <- bundle
  added_edge$hash_dag$edges <- c(
    added_edge$hash_dag$edges,
    list(c("normalized_input_rows", "distribution_signature"))
  )
  expect_error(getFromNamespace("stpd_gate_b_v3_phase1_hash_dag_lint",
                                "SpikeTrainPatternDetector")(added_edge),
               "contract digest")

  claimed <- bundle$schema_contract_sha256
  unhashed <- bundle
  unhashed$schema_contract_sha256 <- NULL
  hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                              "SpikeTrainPatternDetector")
  canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                "SpikeTrainPatternDetector")
  expect_identical(
    claimed,
    hash_raw("stpd-gate-b-v3-schema-contract-v1", canonical(unhashed))
  )

  altered <- bundle
  altered$schema_constants$schema_version <- "tampered"
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(altered, path, auto_unbox = TRUE, null = "null")
  expect_error(stpd_gate_b_v3_phase1_bundle(path, verify_frozen = FALSE),
               "schema constants|content hash")
})

test_that("all 59 table prototypes and populated examples obey exact schemas", {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  for (table_name in names(bundle$tables)) {
    empty <- stpd_gate_b_v3_empty_table(table_name, bundle)
    expect_true(stpd_gate_b_v3_validate_table_prototype(
      empty, table_name, bundle
    ))
    expect_type(stpd_gate_b_v3_canonical_table_bytes(
      empty, table_name, bundle
    ), "raw")

    populated <- gb3_example_row(table_name, bundle)
    expect_true(stpd_gate_b_v3_validate_table_prototype(
      populated, table_name, bundle
    ))
  }
})

test_that("complete products enforce coordinated candidate and relationship closure", {
  minimal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_minimal_product",
    "SpikeTrainPatternDetector"
  )
  reseal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_reseal_product",
    "SpikeTrainPatternDetector"
  )
  product <- minimal(with_overlap = TRUE)
  expect_true(stpd_gate_b_v3_validate_product_prototype(product))

  missing_relation <- product
  missing_relation$event_state_relationships <-
    missing_relation$event_state_relationships[0, , drop = FALSE]
  missing_relation <- reseal(missing_relation)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(missing_relation),
    "Event-State expected-set closure"
  )

  missing_candidate <- product
  candidate_id <- missing_candidate$event_candidates$event_candidate_id[[1L]]
  missing_candidate$event_candidates <-
    missing_candidate$event_candidates[0, , drop = FALSE]
  missing_candidate$lineage_records <- missing_candidate$lineage_records[
    missing_candidate$lineage_records$child_id != candidate_id, , drop = FALSE
  ]
  missing_candidate <- reseal(missing_candidate)
  expect_error(
    stpd_gate_b_v3_validate_product_prototype(missing_candidate),
    "accepted Event-candidate expected set|polymorphic entity ID is absent"
  )
})

test_that("provenance lineage and manifest semantics fail closed", {
  minimal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_minimal_product",
    "SpikeTrainPatternDetector"
  )
  reseal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_reseal_product",
    "SpikeTrainPatternDetector"
  )
  structural <- getFromNamespace(
    ".stpd_gate_b_v3_validate_product_prototype_structural",
    "SpikeTrainPatternDetector"
  )
  canonical <- getFromNamespace(
    "stpd_gate_b_v3_canonical_json", "SpikeTrainPatternDetector"
  )
  product <- minimal(with_overlap = TRUE)
  expect_true(structural(product))

  domain_id <- function(domain) {
    hit <- product$entity_domain_registry$entity_domain == domain &
      product$entity_domain_registry$active
    expect_identical(sum(hit), 1L)
    product$entity_domain_registry$entity_domain_id[hit]
  }
  role <- product$contract_registries$registry_entry_id[
    product$contract_registries$registry_domain == "evidence_role" &
      product$contract_registries$active
  ][[1L]]
  source_domain <- product$entity_domain_registry[
    product$entity_domain_registry$entity_domain == "event_candidate" &
      product$entity_domain_registry$active, , drop = FALSE
  ]
  source_key <- list(
    event_candidate_id = product$event_candidates$event_candidate_id[[1L]]
  )
  source_hash <- getFromNamespace(
    "stpd_gate_b_v3_source_row_hash", "SpikeTrainPatternDetector"
  )(
    source_domain = source_domain,
    source_table_name = "event_candidates",
    source_row = product$event_candidates[1L, , drop = FALSE],
    source_row_key = source_key,
    source_product_hash = NA_character_,
    bundle = stpd_gate_b_v3_phase1_bundle()
  )
  edge <- data.frame(
    schema_version = "stpd_multitrack_v3_1",
    detection_root_id =
      product$materialized_product_identity$detection_root_id[[1L]],
    entity_domain_id = domain_id("event"),
    entity_id = product$events$event_id[[1L]],
    entity_product_hash = NA_character_, edge_index = 1L,
    evidence_role_registry_id = role,
    source_domain_id = domain_id("event_candidate"),
    source_row_key_json = canonical(source_key),
    source_product_hash = NA_character_,
    source_row_hash = source_hash,
    evidence_id = product$evidence_records$evidence_id[[1L]],
    stringsAsFactors = FALSE
  )
  nonempty_provenance <- product
  nonempty_provenance$entity_evidence_edges <- edge
  nonempty_provenance <- reseal(nonempty_provenance)
  expect_true(structural(nonempty_provenance))

  wrong_source_hash <- nonempty_provenance
  wrong_source_hash$entity_evidence_edges$source_row_hash <- gb3_hash64("e")
  wrong_source_hash <- reseal(wrong_source_hash)
  expect_error(structural(wrong_source_hash), "source-row hash is invalid")

  fabricated_source <- product
  edge$source_domain_id <- paste0("dm_", gb3_hash64("f"))
  fabricated_source$entity_evidence_edges <- edge
  fabricated_source <- reseal(fabricated_source)
  expect_error(structural(fabricated_source),
               "source domain is unknown or inactive")

  cross_entity_lineage <- product
  state_lineage <- cross_entity_lineage$lineage_records$lineage_id[
    cross_entity_lineage$lineage_records$child_id ==
      cross_entity_lineage$state_candidates$state_candidate_id[[1L]]
  ][[1L]]
  cross_entity_lineage$events$lineage_id <- state_lineage
  cross_entity_lineage <- reseal(cross_entity_lineage)
  expect_error(structural(cross_entity_lineage),
               "lineage parent-edge set is not referenced by its child")

  orphan_lineage <- product
  event_lineage_index <- which(
    orphan_lineage$lineage_records$child_id ==
      orphan_lineage$events$event_id[[1L]]
  )[[1L]]
  alternate_parent_index <- which(
    orphan_lineage$lineage_records$parent_id !=
      orphan_lineage$lineage_records$parent_id[[event_lineage_index]]
  )[[1L]]
  extra_lineage <- orphan_lineage$lineage_records[
    event_lineage_index, , drop = FALSE
  ]
  extra_lineage$parent_id <-
    orphan_lineage$lineage_records$parent_id[[alternate_parent_index]]
  extra_lineage$evidence_id <-
    orphan_lineage$lineage_records$evidence_id[[alternate_parent_index]]
  parent_payload <- list(
    parent_domain_id = extra_lineage$parent_domain_id[[1L]],
    parent_id = extra_lineage$parent_id[[1L]],
    parent_product_hash = NULL,
    lineage_action_registry_id =
      extra_lineage$lineage_action_registry_id[[1L]],
    transition_id = NULL,
    evidence_id = extra_lineage$evidence_id[[1L]]
  )
  hash_raw <- getFromNamespace(
    "stpd_gate_b_v3_hash_raw", "SpikeTrainPatternDetector"
  )
  extra_lineage$parent_edge_set_hash <- hash_raw(
    "stpd-lineage-parent-edge-set-v1", canonical(list(
      child_domain_id = extra_lineage$child_domain_id[[1L]],
      child_id = extra_lineage$child_id[[1L]], child_product_hash = NULL,
      sorted_parent_edge_payloads = list(parent_payload)
    ))
  )
  entity_id <- getFromNamespace(
    "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
  )
  extra_lineage$lineage_id <- entity_id("lineage", list(
    detection_root_id = extra_lineage$detection_root_id[[1L]],
    child_domain_id = extra_lineage$child_domain_id[[1L]],
    child_id = extra_lineage$child_id[[1L]],
    parent_edge_set_hash = extra_lineage$parent_edge_set_hash[[1L]]
  ), stpd_gate_b_v3_phase1_bundle())
  orphan_lineage$lineage_records <- rbind(
    orphan_lineage$lineage_records, extra_lineage
  )
  orphan_lineage$lineage_records <- orphan_lineage$lineage_records[
    order(orphan_lineage$lineage_records$lineage_id,
          orphan_lineage$lineage_records$edge_index, method = "radix"),
    , drop = FALSE
  ]
  orphan_lineage <- reseal(orphan_lineage)
  expect_error(structural(orphan_lineage),
               "more than one parent-edge set")

  wrong_sort <- product
  boundary_policy <- wrong_sort$contract_registries$registry_entry_id[
    wrong_sort$contract_registries$registry_domain == "operational_policy" &
      wrong_sort$contract_registries$code == "boundary_precedence_v1"
  ][[1L]]
  event_manifest <- wrong_sort$canonical_manifest$table_name == "events"
  wrong_sort$canonical_manifest$sort_contract_registry_id[event_manifest] <-
    boundary_policy
  identity_check <- getFromNamespace(
    "stpd_gate_b_v3_validate_identity_hash_closure",
    "SpikeTrainPatternDetector"
  )
  expect_error(identity_check(
    wrong_sort, stpd_gate_b_v3_phase1_bundle()
  ), "canonical manifest row differs")
})

test_that("schema validators reject PK, enum, type, ID, sort and FK attacks", {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  registry <- stpd_gate_b_v3_phase1_registry()

  duplicate <- rbind(registry[1L, , drop = FALSE],
                     registry[1L, , drop = FALSE])
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    duplicate, "contract_registries", bundle
  ), "duplicated")

  unknown_enum <- registry[1L, , drop = FALSE]
  unknown_enum$registry_domain <- "unknown_domain"
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    unknown_enum, "contract_registries", bundle
  ), "unknown enum")

  wrong_type <- registry
  wrong_type$active <- as.integer(wrong_type$active)
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    wrong_type, "contract_registries", bundle
  ), "type mismatch")

  wrong_registry_id <- registry[1L, , drop = FALSE]
  wrong_registry_id$registry_entry_id <- paste0("reg_", gb3_hash64("f"))
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    wrong_registry_id, "contract_registries", bundle
  ), "content-addressed identity")

  wrong_id <- gb3_example_row("state_candidates", bundle)
  wrong_id$state_candidate_id <- paste0("sc_", gb3_hash64("f"))
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    wrong_id, "state_candidates", bundle
  ), "exact payload")

  expect_error(stpd_gate_b_v3_validate_table_prototype(
    registry[rev(seq_len(nrow(registry))), ], "contract_registries", bundle
  ), "canonical sort")

  policy <- gb3_example_row("detector_policy_manifest", bundle)
  wrong_registry_row <- registry[registry$registry_domain !=
                                   "operational_policy", ][1L, , drop = FALSE]
  policy$governing_registry_entry_id <- wrong_registry_row$registry_entry_id
  tables <- lapply(names(bundle$tables), stpd_gate_b_v3_empty_table,
                   bundle = bundle)
  names(tables) <- names(bundle$tables)
  tables$contract_registries <- wrong_registry_row
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    policy, "detector_policy_manifest", bundle, tables = tables
  ), "wrong domain")

  tables$contract_registries <- registry[0, , drop = FALSE]
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    policy, "detector_policy_manifest", bundle, tables = tables
  ), "orphaned")

  partition <- gb3_example_row("partition_memberships", bundle)
  partition$group_role <- if (partition$group_role == "calibration")
    "validation" else "calibration"
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    partition, "partition_memberships", bundle
  ), "membership row hash")

  lineage <- gb3_example_row("lineage_records", bundle)
  lineage$parent_id <- paste0("x_", gb3_hash64("f"))
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    lineage, "lineage_records", bundle
  ), "Lineage ID/edge-set")
})

test_that("RFC 8785 serialization is deterministic across Unicode and process", {
  canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                "SpikeTrainPatternDetector")
  expect_identical(canonical(list("\ue000" = 2, "😀" = 1)),
                   "{\"😀\":1,\"\":2}")
  expect_identical(
    canonical(c(333333333.33333325, 1e-7, 1e20, 1e21, 1e-6, 0)),
    "[333333333.33333325,1e-7,100000000000000000000,1e+21,0.000001,0]"
  )
  expect_error(canonical(-0), "Negative zero")
  expect_identical(canonical(list(b = "e\u0301", a = 1)),
                   "{\"a\":1,\"b\":\"é\"}")
  duplicate_names <- list(1, 2)
  names(duplicate_names) <- c("é", "e\u0301")
  expect_error(canonical(duplicate_names), "unique")

  expected <- digest::digest(
    stpd_gate_b_v3_canonical_table_bytes(
      stpd_gate_b_v3_phase1_registry(), "contract_registries"
    ), algo = "sha256", serialize = FALSE
  )
  root_candidates <- normalizePath(c(".", file.path("..", "..")),
                                   mustWork = FALSE)
  source_roots <- root_candidates[vapply(root_candidates, function(path) {
    file.exists(file.path(path,"DESCRIPTION")) &&
      dir.exists(file.path(path,"R")) &&
      file.exists(file.path(path,"tools",
                            "generate_gate_b_v3_phase1_bundle.R")) &&
      file.exists(file.path(path,"docs",
        "gate-b-v3-normative-data-identity-release-contract.md"))
  }, logical(1))]
  source_root <- length(source_roots) > 0L
  root <- if (source_root) source_roots[[1L]] else ""
  expression <- paste0(
    "root<-Sys.getenv('STPD_TEST_ROOT');",
    "if(nzchar(root)&&file.exists(file.path(root,'DESCRIPTION'))&&",
    "dir.exists(file.path(root,'R'))){pkgload::load_all(root,quiet=TRUE)}else{",
    "library(SpikeTrainPatternDetector)};",
    "b<-stpd_gate_b_v3_canonical_table_bytes(",
    "stpd_gate_b_v3_phase1_registry(),'contract_registries');",
    "cat(digest::digest(b,algo='sha256',serialize=FALSE))"
  )
  actual <- system2(file.path(R.home("bin"), "Rscript"),
                    c("-e", shQuote(expression)),
                    env = c(
                      paste0("STPD_TEST_ROOT=", shQuote(
                        if (source_root) root else ""
                      )),
                      paste0("R_LIBS_USER=", shQuote(dirname(
                        system.file(package = "SpikeTrainPatternDetector")
                      )))
                    ),
                    stdout = TRUE, stderr = TRUE)
  expect_identical(tail(actual, 1L), expected)
})

test_that("canonical table bytes match an independently frozen literal digest", {
  bytes <- stpd_gate_b_v3_canonical_table_bytes(
    stpd_gate_b_v3_empty_table("events"), "events"
  )
  expect_identical(
    digest::digest(bytes, algo = "sha256", serialize = FALSE),
    "98b500f600452956619354260f5281645c7fdd008367d83af7b484f500235d5f"
  )
  parsed <- jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
  expect_identical(parsed$schema_id, "stpd_canonical_table_v1")
  expect_identical(parsed$rows, list())
})

test_that("State, Pause and cross-track scientific rules are explicit", {
  expected <- c(
    `high|regular` = "high_frequency_tonic",
    `high|irregular` = "high_frequency_irregular_state",
    `high|unresolved` = NA_character_,
    `non_high|regular` = "tonic",
    `non_high|irregular` = NA_character_,
    `non_high|unresolved` = NA_character_,
    `unresolved|regular` = NA_character_,
    `unresolved|irregular` = NA_character_,
    `unresolved|unresolved` = NA_character_
  )
  for (key in names(expected)) {
    axes <- strsplit(key, "|", fixed = TRUE)[[1L]]
    result <- stpd_gate_b_v3_state_axis_decision(axes[[1L]], axes[[2L]])
    expect_identical(result$state_class, unname(expected[[key]]))
    expect_identical(result$decision,
                     if (is.na(expected[[key]])) "abstain" else "accepted")
  }

  short <- stpd_gate_b_v3_pause_attainability(10L, 5L, 0.05)
  long <- stpd_gate_b_v3_pause_attainability(999L, 5L, 0.05)
  expect_false(short$algebraically_attainable)
  expect_true(long$algebraically_attainable)
  expect_false(short$fdr_control_claim)
  expect_false(long$fdr_control_claim)
  expect_equal(short$isolated_rank1_q_lower_bound,
               min(1, short$discrete_empirical_p_lower_bound * 5))

  compatibility <- stpd_gate_b_v3_compatibility_matrix()
  expect_equal(nrow(compatibility), 20L)
  expect_false(anyDuplicated(compatibility[1:4]) > 0L)
  expect_identical(stpd_gate_b_v3_compatibility_action(
    "event", "burst_event", "state", "high_frequency_irregular_state"
  )$action, "coexist_non_destructive")
  expect_identical(stpd_gate_b_v3_compatibility_action(
    "event", "burst_event", "gap", "canonical_pause"
  )$action, "reject_event_preserve_gap")
  expect_error(stpd_gate_b_v3_compatibility_action(
    "event", "burst_event", "gap", "unknown"
  ), "No frozen")
})

test_that("label-blind reconstruction excludes labels and validates provenance", {
  train <- gb3_train_id()
  raw <- data.frame(timestamp = c(0.1, 0.2, 0.4))
  clean <- list(trains = setNames(list(raw), train))
  polluted <- raw
  polluted$pattern_manual <- c("burst", NA, NA)
  polluted$pattern_manual_negative <- c(NA, "not_burst", NA)
  polluted$pattern_final <- c("burst", "tonic", "pause")
  attr(polluted, "learned_ranges") <- list(burst = c(0.001, 0.02))
  dirty <- list(
    trains = setNames(list(polluted), train),
    results = list(multitrack_auto = list(secret = "manual-aware")),
    cache = list(manual_threshold = 0.02)
  )
  a <- stpd_gate_b_v3_label_blind_input(clean)
  b <- stpd_gate_b_v3_label_blind_input(dirty)
  expect_identical(a, b)
  expect_identical(names(a$trains[[train]]), "timestamp")
  expect_true(all(grepl("^tr_[0-9a-f]{64}$",
                        a$normalized_input_manifest$train)))

  context <- setNames(list(list(
    timestamp_unit = "seconds",
    waveform_qc_available = FALSE,
    patient_group_id_hash = gb3_hash64("b")
  )), train)
  with_context <- stpd_gate_b_v3_label_blind_input(clean, context)
  expect_identical(with_context$normalized_input_manifest$patient_group_id_hash,
                   gb3_hash64("b"))
  forbidden_context <- context
  forbidden_context[[train]]$forbidden_label <- "burst"
  expect_error(stpd_gate_b_v3_label_blind_input(clean, forbidden_context),
               "Unknown acquisition/QC input source")
  bad_context <- context
  bad_context[[train]]$patient_group_id_hash <- "patient-1"
  expect_error(stpd_gate_b_v3_label_blind_input(clean, bad_context),
               "lowercase SHA-256")
  expect_error(stpd_gate_b_v3_label_blind_input(
    list(trains = list(label_bearing_name = raw))
  ), "pseudonyms")

  root <- getFromNamespace("stpd_gate_b_v3_dataset_snapshot_identity",
                           "SpikeTrainPatternDetector")
  changed <- clean
  changed$trains[[train]]$timestamp[[3L]] <- 0.5
  expect_false(identical(root(a), root(stpd_gate_b_v3_label_blind_input(changed))))
  expect_false(identical(root(a), root(with_context)))
})

test_that("active registry rows are complete content-addressed semantics", {
  registry <- stpd_gate_b_v3_phase1_registry()
  bundle <- stpd_gate_b_v3_phase1_bundle()
  expect_equal(nrow(registry), 154L)
  expect_setequal(unique(registry$registry_domain),
                  unlist(bundle$enums$registry_domain, use.names = FALSE))
  expect_false(anyDuplicated(registry$registry_entry_id) > 0L)
  expect_false(anyDuplicated(registry[c("registry_domain", "code",
                                        "code_version")]) > 0L)
  expect_true(all(registry$active))

  canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                "SpikeTrainPatternDetector")
  hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                              "SpikeTrainPatternDetector")
  required_fields <- c(
    "contract_clause_id", "definition_schema", "edge_cases",
    "formula_or_state_machine", "input_schema_id", "output_schema_id",
    "parameter_paths"
  )
  for (i in seq_len(nrow(registry))) {
    semantic <- jsonlite::fromJSON(registry$semantic_definition_json[[i]],
                                   simplifyVector = FALSE)
    expect_setequal(names(semantic), required_fields)
    semantic_hash <- hash_raw("stpd-registry-semantic-v1",
                              registry$semantic_definition_json[[i]])
    expect_identical(semantic_hash,
                     registry$semantic_definition_sha256[[i]])
    payload <- canonical(list(
      registry_domain = registry$registry_domain[[i]],
      code = registry$code[[i]],
      code_version = registry$code_version[[i]],
      semantic_definition_sha256 = semantic_hash
    ))
    expect_identical(
      registry$registry_entry_id[[i]],
      paste0("reg_", hash_raw("stpd-registry-entry-v1", payload))
    )
  }
  expect_false(any(grepl("TOST", registry$semantic_definition_json,
                         ignore.case = TRUE)))
})

test_that("Gate B v3 phase-1 executable fixture matrix", {
  fixture <- stpd_gate_b_v3_phase1_fixture_spec()
  bundle <- stpd_gate_b_v3_phase1_bundle()
  expect_equal(nrow(fixture), 58L)
  expect_setequal(unique(fixture$contract_id),
                  unlist(bundle$required_contract_ids, use.names = FALSE))
  expect_equal(sum(grepl("^acceptance_gb3_", fixture$fixture_id)), 31L)
  expect_false(anyDuplicated(fixture[c("contract_id", "fixture_id")]) > 0L)

  canonical <- getFromNamespace("stpd_gate_b_v3_canonical_json",
                                "SpikeTrainPatternDetector")
  hash_raw <- getFromNamespace("stpd_gate_b_v3_hash_raw",
                              "SpikeTrainPatternDetector")
  for (i in seq_len(nrow(fixture))) {
    actual <- canonical(stpd_gate_b_v3_run_phase1_fixture(
      fixture$fixture_id[[i]]
    ))
    expect_identical(actual, fixture$expected_result_json[[i]])
    expect_identical(
      fixture$expected_result_hash[[i]],
      hash_raw(
        "stpd-fixture-expected-result-v1",
        canonical(list(
          expected_result_schema = fixture$expected_result_schema[[i]],
          expected_result_json = fixture$expected_result_json[[i]]
        ))
      )
    )
    parsed <- jsonlite::fromJSON(actual, simplifyVector = FALSE)
    expect_false(parsed$authority_granted)
    expect_false(parsed$release_evidence_created)
  }
})

test_that("resource budgets fail closed without truncation", {
  contract <- stpd_gate_b_v3_complexity_contract()
  expect_equal(nrow(contract), 9L)
  expect_false(anyDuplicated(contract$budget_metric) > 0L)
  expect_true(all(contract$cancellation_point))
  expect_true(all(contract$cleanup_state ==
                    "zero_published_tables_and_remove_staging"))

  limits <- setNames(contract$limit, contract$budget_metric)
  expect_error(
    stpd_gate_b_v3_resource_budget_check(limits),
    "Direct Gate B v3 resource claims are disabled"
  )
  compare <- getFromNamespace(
    "stpd_gate_b_v3_resource_budget_compare", "SpikeTrainPatternDetector"
  )
  ok <- compare(limits)
  expect_identical(ok$product_status, "within_budget")
  expect_false(ok$truncated)
  for (metric in contract$budget_metric) {
    overflow <- limits
    overflow[[metric]] <- overflow[[metric]] + 1
    failed <- compare(overflow)
    expect_identical(failed$product_status, "failed_closed")
    expect_identical(failed$failure_code, "resource_budget_exceeded")
    expect_identical(failed$canonical_tables_published, 0L)
    expect_false(failed$truncated)
  }
  cancelled <- compare(limits, cancelled = TRUE)
  expect_identical(cancelled$product_status, "failed_closed")
  expect_identical(cancelled$failure_stage, "cancelled")
  expect_identical(cancelled$canonical_tables_published, 0L)
  expect_error(
    compare(limits[-1L]), "missing"
  )
  forged <- new.env(parent=emptyenv())
  forged$counters <- limits
  class(forged) <- "stpd_gate_b_v3_resource_observation"
  expect_error(stpd_gate_b_v3_resource_budget_check(forged),
               "Direct Gate B v3 resource claims are disabled")
})

test_that("table-specific enums and status permissions are exact", {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  expect_setequal(unlist(bundle$enums$record_status),
                  c("predicted", "review_confirmed", "review_rejected"))
  expect_setequal(unlist(bundle$enums$state_candidate_decision),
                  c("accepted_direct_support", "rejected", "abstained",
                    "interrupted_parent"))
  expect_setequal(unlist(bundle$enums$event_candidate_decision),
                  c("accepted_event", "rejected", "review_candidate"))
  expect_setequal(unlist(bundle$enums$label_access),
                  c("none", "development_reference_only"))
  expect_setequal(unlist(bundle$enums$threshold_value_type),
                  c("double", "string"))
  expect_true(all(vapply(names(bundle$tables), function(name) {
    spec <- bundle$tables[[name]]
    identical(spec$table_schema,
              paste0("stpd_multitrack_v3_", name, "_1")) &&
      identical(spec$canonical_envelope_schema, "stpd_canonical_table_v1")
  }, logical(1))))

  event_candidate <- gb3_example_row("event_candidates", bundle)
  event_candidate$candidate_decision <- "accepted_direct_support"
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    event_candidate, "event_candidates", bundle
  ), "unknown enum")

  event <- gb3_example_row("events", bundle)
  event$record_status <- "predicted"
  expect_true(stpd_gate_b_v3_validate_table_prototype(event, "events", bundle))
  event$record_status <- "auto_accepted"
  expect_error(stpd_gate_b_v3_validate_table_prototype(event, "events", bundle),
               "unknown enum")

  policy <- gb3_example_row("threshold_policies", bundle)
  policy$threshold_source_class <- "development_trained"
  policy$adaptation_unit <- "none"
  policy$label_access <- "development_reference_only"
  policy$partition_id <- paste0("pt_", gb3_hash64("c"))
  rekey_policy <- function(row) {
    fields <- unlist(bundle$id_payload_registry$threshold_policy$fields,
                     use.names = FALSE)
    payload <- lapply(fields, function(name) row[[name]][[1L]])
    names(payload) <- fields
    row$threshold_policy_id <- getFromNamespace(
      "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
    )("threshold_policy", payload, bundle)
    row
  }
  policy <- rekey_policy(policy)
  expect_true(stpd_gate_b_v3_validate_table_prototype(
    policy, "threshold_policies", bundle
  ))
  policy$label_access <- "none"
  policy <- rekey_policy(policy)
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    policy, "threshold_policies", bundle
  ), "permission matrix")

  status <- gb3_example_row("product_status_envelope", bundle)
  status$product_status <- "failed_closed"
  status$gate_b_status <- "failed"
  status$integrity_level <- "none"
  status$detection_root_id <- NA_character_
  status$product_hash <- NA_character_
  status$failure_stage <- "materialization"
  status$failure_code_registry_id <- NA_character_
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    status, "product_status_envelope", bundle
  ), "failure/re-detection reason presence")

  event_geometry <- gb3_example_row("events", bundle)
  event_geometry$n_spikes <- event_geometry$n_spikes + 1L
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    event_geometry, "events", bundle
  ), "geometry/count/duration")

  connector <- gb3_example_row("state_segments", bundle)
  connector$segment_role <- "tolerated_connector"
  fields <- unlist(bundle$id_payload_registry$state_segment$fields,
                   use.names = FALSE)
  payload <- lapply(fields, function(name) connector[[name]][[1L]])
  names(payload) <- fields
  connector$state_segment_id <- getFromNamespace(
    "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
  )("state_segment", payload, bundle)
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    connector, "state_segments", bundle
  ), "role/FK conditional")

  relationship <- gb3_example_row("event_state_relationships", bundle)
  relationship$connector_overlap_n_isi <- 1L
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    relationship, "event_state_relationships", bundle
  ), "overlap decomposition")

  reference <- gb3_example_row("reference_annotations", bundle)
  reference$blinded_to_thresholds <- FALSE
  payload_names <- setdiff(names(reference), c("annotation_id", "created_utc"))
  payload <- lapply(payload_names, function(name) {
    value <- reference[[name]][[1L]]
    if (length(value) == 1L && is.na(value)) NULL else value
  })
  names(payload) <- payload_names
  reference$annotation_id <- paste0(
    "ra_",
    getFromNamespace("stpd_gate_b_v3_hash_raw",
                     "SpikeTrainPatternDetector")(
      "stpd-reference-annotation-v1",
      getFromNamespace("stpd_gate_b_v3_canonical_json",
                       "SpikeTrainPatternDetector")(payload)
    )
  )
  # A non-blinded row remains representable for workflow-agreement use; it is
  # excluded from detector-performance eligibility by the reference manifest.
  expect_true(stpd_gate_b_v3_validate_table_prototype(
    reference, "reference_annotations", bundle
  ))
})

test_that("threshold policy instances enforce scope and held-out separation", {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  policy <- gb3_example_row("threshold_policies", bundle)
  instance <- gb3_example_row("threshold_instances", bundle)
  instance$threshold_policy_id <- policy$threshold_policy_id
  instance$path <- policy$path
  instance$units <- policy$units

  instance_fields <- unlist(
    bundle$id_payload_registry$threshold_instance$fields,
    use.names = FALSE
  )
  instance_payload <- lapply(
    instance_fields,
    function(name) instance[[name]][[1L]]
  )
  names(instance_payload) <- instance_fields
  instance$threshold_instance_id <- getFromNamespace(
    "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
  )("threshold_instance", instance_payload, bundle)

  no_memberships <- stpd_gate_b_v3_empty_table(
    "partition_memberships", bundle
  )
  expect_true(stpd_gate_b_v3_validate_threshold_permissions(
    policy, instance, no_memberships
  ))

  wrong_scope <- instance
  wrong_scope$application_scope <- "train"
  wrong_scope$train <- gb3_train_id("b")
  wrong_scope$application_scope_key_hash <- gb3_hash64("b")
  wrong_scope_payload <- lapply(
    instance_fields,
    function(name) wrong_scope[[name]][[1L]]
  )
  names(wrong_scope_payload) <- instance_fields
  wrong_scope$threshold_instance_id <- getFromNamespace(
    "stpd_gate_b_v3_entity_id", "SpikeTrainPatternDetector"
  )("threshold_instance", wrong_scope_payload, bundle)
  expect_error(stpd_gate_b_v3_validate_threshold_permissions(
    policy, wrong_scope, no_memberships
  ), "policy/instance|scope")

  calibration <- gb3_example_row("partition_memberships", bundle)
  validation <- calibration
  validation$group_role <- "validation"
  validation$membership_row_hash <- gb3_hash64("e")
  expect_error(stpd_gate_b_v3_validate_threshold_permissions(
    policy, instance, rbind(calibration, validation)
  ), "overlap|partition|unique")
})

test_that("canonical table sorting is UTF-8 and rejects hidden attributes", {
  sort_index <- getFromNamespace("stpd_gate_b_v3_sort_index",
                                 "SpikeTrainPatternDetector")
  inverse_pair <- data.frame(key = c("\ue000", "😀"), stringsAsFactors = FALSE)
  expect_identical(sort_index(inverse_pair, "key"), 1:2)

  invalid <- rawToChar(as.raw(c(0xc3, 0x28)))
  Encoding(invalid) <- "UTF-8"
  expect_error(getFromNamespace("stpd_gate_b_v3_strict_nfc",
                                "SpikeTrainPatternDetector")(invalid),
               "Invalid UTF-8")

  event <- gb3_example_row("events")
  attr(event$record_status, "hidden_manual_label") <- "burst"
  expect_error(stpd_gate_b_v3_validate_table_prototype(event, "events"),
               "attributes/classes")

  context_row <- gb3_example_row("scientific_context")
  contexts <- rbind(context_row,context_row)
  contexts$detection_root_id <- paste0(
    "dr_", c(strrep("1",64L),strrep("2",64L))
  )
  expect_true(stpd_gate_b_v3_validate_table_prototype(
    contexts,"scientific_context"
  ))
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    contexts[2:1,,drop=FALSE],"scientific_context"
  ), "canonical sort key")
})

test_that("label-blind reconstruction strips allowlisted scalar attributes", {
  train <- gb3_train_id("d")
  unit <- structure("seconds", hidden_manual_label = "burst")
  resolution <- structure(1e-6, hidden_review_label = "pause")
  clean <- list(trains = setNames(list(data.frame(
    timestamp = c(0, .1, .2), stringsAsFactors = FALSE
  )), train))
  context <- setNames(list(list(timestamp_unit = unit,
                                timestamp_resolution_sec = resolution)), train)
  rebuilt <- stpd_gate_b_v3_label_blind_input(clean, context)
  expect_identical(rebuilt$acquisition_context[[train]]$timestamp_unit,
                   "seconds")
  expect_null(attributes(rebuilt$acquisition_context[[train]]$timestamp_unit))
  expect_identical(rebuilt$acquisition_context[[train]]$timestamp_resolution_sec,
                   1e-6)
  expect_null(attributes(
    rebuilt$acquisition_context[[train]]$timestamp_resolution_sec
  ))
})

test_that("acceptance fixtures contain concrete immutable inputs and exact oracles", {
  inputs <- getFromNamespace(
    "stpd_gate_b_v3_phase1_fixture_inputs", "SpikeTrainPatternDetector"
  )()
  cases <- inputs[vapply(inputs, function(x) {
    identical(x$case_kind, "acceptance_scenario")
  }, logical(1))]
  expect_length(cases, 31L)
  expect_true(all(vapply(cases, function(x) {
    length(x$case_input$input) > 0L
  }, logical(1))))
  expect_true(all(vapply(cases[1:15], function(x) {
    any(c("roles", "frequency", "expected_relations", "state_profile") %in%
          names(x$case_input$input))
  }, logical(1))))
  run_scenario <- getFromNamespace(
    "stpd_gate_b_v3_phase1_scenario", "SpikeTrainPatternDetector"
  )
  expected_scenario <- getFromNamespace(
    "stpd_gate_b_v3_phase1_expected_scenario", "SpikeTrainPatternDetector"
  )
  canonical <- getFromNamespace(
    "stpd_gate_b_v3_canonical_json", "SpikeTrainPatternDetector"
  )
  expect_true(all(vapply(cases, function(x) {
    actual <- run_scenario(
      x$case_input$scenario_id, x$case_input$input
    )
    expected <- expected_scenario(
      x$case_input$scenario_id
    )
    identical(canonical(actual), canonical(expected))
  }, logical(1))))
})

test_that("FINAL transition payloads use exact keys and compare-and-swap hashes", {
  payload <- list(
    review_candidate_id = paste0("rv_", gb3_hash64("a")),
    expected_review_candidate_hash = gb3_hash64("b"),
    source_event_candidate_id = paste0("ec_", gb3_hash64("c")),
    expected_source_candidate_hash = gb3_hash64("d"),
    suggested_class_registry_id = paste0("reg_", gb3_hash64("e")),
    reason_registry_id = paste0("reg_", gb3_hash64("f"))
  )
  validated <- stpd_gate_b_v3_validate_transition_payload(
    "event_candidate_accept", payload
  )
  expect_match(validated$payload_json_hash, "^[0-9a-f]{64}$")
  expect_setequal(names(jsonlite::fromJSON(validated$payload_json,
                                            simplifyVector = FALSE)),
                  names(payload))
  extra <- payload
  extra$reference_record_id <- "forbidden"
  expect_error(stpd_gate_b_v3_validate_transition_payload(
    "event_candidate_accept", extra
  ), "exact schema")
  missing <- payload[-1L]
  expect_error(stpd_gate_b_v3_validate_transition_payload(
    "event_candidate_accept", missing
  ), "exact schema")
  bad_hash <- payload
  bad_hash$expected_source_candidate_hash <- "not-a-hash"
  expect_error(stpd_gate_b_v3_validate_transition_payload(
    "event_candidate_accept", bad_hash
  ), "lowercase SHA-256")
})

test_that("resource staging verifies cleanup on overflow and cancellation", {
  root <- tempfile("gb3-resource-root-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  candidate_limit <- stpd_gate_b_v3_complexity_contract()$limit[
    stpd_gate_b_v3_complexity_contract()$budget_metric == "candidate_rows"
  ][[1L]]
  ran <- FALSE
  failed <- stpd_gate_b_v3_resource_staging_probe(
    root, list(list(op = "reserve_candidate_rows",
                    n_rows = candidate_limit + 1))
  )
  expect_false(ran)
  expect_identical(failed$failure_code, "resource_budget_exceeded")
  expect_true(failed$staging_path_absent)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  cancelled <- stpd_gate_b_v3_resource_staging_probe(
    root, cancelled = TRUE
  )
  expect_identical(cancelled$failure_code, "cancelled")
  expect_true(cancelled$staging_path_absent)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  measured <- stpd_gate_b_v3_resource_staging_probe(
    root, list(list(op = "write_artifact_raw", relative_path = "one.bin",
                    bytes = as.raw(1L)))
  )
  expect_identical(measured$observed_counters[["artifact_bytes_total"]], 1)
  expect_gt(measured$observed_counters[["peak_memory_bytes"]], 0)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  peak_rss <- getFromNamespace(
    "stpd_gate_b_v3_peak_rss_bytes", "SpikeTrainPatternDetector"
  )
  peak_before <- peak_rss()
  peak_probe_allocation <- raw(1024L * 1024L)
  peak_probe_allocation[[length(peak_probe_allocation)]] <- as.raw(1L)
  peak_after <- peak_rss()
  expect_true(is.finite(peak_before) && peak_before > 0)
  expect_gte(peak_after, peak_before)

  local({
    testthat::local_mocked_bindings(
      stpd_gate_b_v3_peak_rss_native = function() NA_real_,
      .package = "SpikeTrainPatternDetector"
    )
    expect_error(
      stpd_gate_b_v3_resource_staging_probe(root),
      "measurement backend is unavailable"
    )
    expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
  })

  expect_error(stpd_gate_b_v3_resource_staging_probe(
    root, setNames(rep(0, 9),
                   stpd_gate_b_v3_complexity_contract()$budget_metric)
  ), "declarative list")
  expect_error(stpd_gate_b_v3_resource_staging_probe(
    root, probe = function(stage) {
      ran <<- TRUE
      file.create(file.path(dirname(root), "gb3-forbidden-leak"))
    }
  ), "unused argument")
  expect_false(ran)

  escaped <- file.path(dirname(root), "gb3-forbidden-escape.bin")
  on.exit(unlink(escaped, force = TRUE), add = TRUE)
  expect_error(stpd_gate_b_v3_resource_staging_probe(
    root, list(list(op = "write_artifact_raw",
                    relative_path = "../gb3-forbidden-escape.bin",
                    bytes = as.raw(1L)))
  ), "safe ASCII relative path")
  expect_false(file.exists(escaped))

  expect_error(
    stpd_gate_b_v3_resource_staging_probe(
      root, list(list(op = "reserve_candidate_rows", n_rows = 1L))
    ),
    "reserved/materialized row counters do not close"
  )
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  candidate_row <- gb3_example_row(
    "state_candidates", stpd_gate_b_v3_phase1_bundle()
  )
  expect_error(
    stpd_gate_b_v3_resource_staging_probe(root, list(list(
      op = "write_canonical_table", relative_path = "candidate.json",
      table_name = "state_candidates", value = candidate_row
    ))),
    "reserved/materialized row counters do not close"
  )
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  matched_rows <- stpd_gate_b_v3_resource_staging_probe(root, list(
    list(op = "reserve_candidate_rows", n_rows = 1L),
    list(op = "write_canonical_table", relative_path = "candidate.json",
         table_name = "state_candidates", value = candidate_row)
  ))
  expect_identical(matched_rows$product_status, "within_budget")
  expect_identical(matched_rows$observed_counters[["candidate_rows"]], 1)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  json_limit <- stpd_gate_b_v3_complexity_contract()$limit[
    stpd_gate_b_v3_complexity_contract()$budget_metric == "json_bytes_max"
  ][[1L]]
  actual_overflow <- stpd_gate_b_v3_resource_staging_probe(
    root, list(list(
      op = "write_json", relative_path = "oversize.json",
      value = list(payload = strrep("x", json_limit + 1))
    ))
  )
  expect_identical(actual_overflow$failure_code,"resource_budget_exceeded")
  expect_true("json_bytes_max" %in% actual_overflow$exceeded_counters)
  expect_gt(actual_overflow$observed_counters[["json_bytes_max"]],json_limit)
  expect_true(actual_overflow$staging_path_absent)

  current_root <- tempfile("gb3-resource-current-")
  dir.create(current_root)
  on.exit(unlink(current_root,recursive=TRUE,force=TRUE),add=TRUE)
  current <- file.path(current_root,"CURRENT")
  writeLines("generation=old",current,useBytes=TRUE)
  current_before <- digest::digest(current,algo="sha256",file=TRUE)
  current_failure <- stpd_gate_b_v3_resource_staging_probe(
    current_root,list(list(op="reserve_edge_rows",n_rows=
      stpd_gate_b_v3_complexity_contract()$limit[
        stpd_gate_b_v3_complexity_contract()$budget_metric=="edge_rows"
      ][[1L]]+1))
  )
  expect_identical(current_failure$failure_code,"resource_budget_exceeded")
  expect_identical(digest::digest(current,algo="sha256",file=TRUE),
                   current_before)
})

test_that("privacy redaction rejects nested identifiers and absolute paths", {
  expect_true(stpd_gate_b_v3_validate_redacted_artifacts(list(
    diagnostic_code = "not_asserted_v1",
    stage = "detection", completed_trains = 1, total_trains = 2,
    counters = list(input_isi = 10),
    detail = list(metric = "input_isi", value = 10)
  )))
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    detail = list(patient_id = "P7")
  )), "Unknown key")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    detail = "file:///Users/name/raw.csv"
  )), "detail schema")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    diagnostic_code = "not_asserted_v1",
    detail = list(reason = "private-workstation.example.local")
  )), "Uncontrolled value")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    diagnostic_code = "not_asserted_v1", detail = list(code = "operator-private")
  )), "Uncontrolled value")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    diagnostic_code = "not_asserted_v1", detail = list(reason = "张三")
  )), "Uncontrolled value")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    completed_trains = 3, total_trains = 2
  )), "exceeds total")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    completed_trains = structure(
      1, operator = "operator-private", path = "/Users/example/private"
    ),
    total_trains = 2
  )), "not an exact non-negative integer")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
    detail = list(metric = "input_isi", value = structure(
      10, hostname = "private-workstation.example.local"
    ))
  )), "finite numeric scalar")
  expect_error(stpd_gate_b_v3_validate_redacted_artifacts(structure(
    list(diagnostic_code = "not_asserted_v1"), operator = "operator-private"
  )), "nonempty named object")
  local({
    testthat::local_mocked_bindings(
      stpd_gate_b_v3_phase1_registry = function(verify_materialized = TRUE) {
        expect_true(verify_materialized)
        stop("simulated materialized registry drift", call. = FALSE)
      },
      .package = "SpikeTrainPatternDetector"
    )
    expect_error(stpd_gate_b_v3_validate_redacted_artifacts(list(
      diagnostic_code = "not_asserted_v1"
    )), "simulated materialized registry drift")
  })
})

test_that("phase-1 artifacts regenerate byte-identically in two clean trees", {
  candidates <- c(
    file.path("tools", "check_gate_b_v3_phase1_reproducibility.R"),
    file.path("..", "..", "tools", "check_gate_b_v3_phase1_reproducibility.R")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)==0L) {
    # Installed-package contract: the four embedded artifacts and their
    # internal source/hash closure remain mandatory; source-tree regeneration
    # is a separate required CI job and is never reported as an installed skip.
    bundle <- stpd_gate_b_v3_phase1_bundle(verify_frozen=TRUE)
    expect_true(stpd_gate_b_v3_phase1_schema_lint(bundle))
    expect_true(nrow(stpd_gate_b_v3_phase1_registry())>0L)
    expect_true(nrow(stpd_gate_b_v3_phase1_fixture_spec())>0L)
    succeed()
    return(invisible(NULL))
  }
  script <- existing[[1L]]
  repo <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
  command <- sprintf("cd %s && %s %s",
                     shQuote(repo), shQuote(file.path(R.home("bin"), "Rscript")),
                     shQuote(file.path("tools", basename(script))))
  output <- system(command, intern = TRUE, ignore.stderr = FALSE)
  expect_identical(attr(output, "status") %||% 0L, 0L)
  expect_true(any(output == "GATE_B_V3_PHASE1_CLEAN_REPRODUCIBILITY_OK"))
})

test_that("phase-1 cannot promote, export or claim detector performance", {
  promotion_functions <- c(
    "stpd_multitrack_gate_b_promote_auto_product",
    "stpd_multitrack_gate_b_promote_final_tables",
    "stpd_multitrack_gate_b_promote_final_product"
  )
  for (name in promotion_functions) {
    fun <- getFromNamespace(name, "SpikeTrainPatternDetector")
    expect_error(fun(list()), "disabled")
  }
  expect_error(stpd_multitrack_authoritative(list()), "No Gate B|unavailable")
  official_writer <- getFromNamespace(
    "stpd_write_multitrack_gate_b", "SpikeTrainPatternDetector"
  )
  expect_error(official_writer(
    list(), tempfile("gate-b-v3-official-")
  ), "No Gate B|Official Gate B export is unavailable")

  report <- stpd_gate_b_v3_phase1_halt_report()
  expect_true(all(report$passed))
  expect_false(any(report$authority_granted))
  expect_false(any(report$release_evidence_created))
  expect_true(all(report$phase1_status ==
                    "implementation_candidate_pending_independent_review"))
})
