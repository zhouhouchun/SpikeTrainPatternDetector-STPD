lineage_error <- function(expr) {
  tryCatch(force(expr), error = function(error) error)
}

lineage_sha <- function(letter) paste(rep(letter, 64L), collapse = "")

lineage_reason_precedence <- function(reason) {
  taxonomy <- stpd_candidate_lineage_reason_taxonomy()
  taxonomy$reason_precedence[match(reason, taxonomy$reason_code)]
}

lineage_bind <- function(rows, table_name) {
  if (!length(rows)) return(stpd_candidate_lineage_empty_table(table_name))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

lineage_rehash_event <- function(row) {
  stpd_candidate_lineage_stage_event_id(
    row$run_id, row$params_hash, row$stage_scope_id,
    row$candidate_node_id, row$stage, row$decision, row$reason_code,
    row$source_decision_code, row$current_start_isi, row$current_end_isi,
    row$overlap_winner_candidate_node_id, row$final_product_type,
    row$final_product_id, row$candidate_count, row$candidate_cap,
    row$cap_scope_member, row$cap_rank, row$cap_policy_id,
    row$universe_status
  )
}

lineage_node <- function(
    dataset_id, train_id, source_candidate_id, family_id = "burst",
    semantic_track = "event", node_role = "candidate",
    candidate_class = family_id,
    node_origin = if (node_role == "final_product") "final_product" else if (
      created_stage == "raw_threshold_support_proposed"
    ) "root" else "derived",
    created_stage = "raw_threshold_support_proposed", created_sequence = 1L,
    rank = 1L, start_isi = 1L, end_isi = 4L,
    candidate_count = 1L, provider_id = "train_adaptive",
    raw_candidate_layer = "candidate",
    raw_candidate_class = candidate_class,
    raw_final_label = candidate_class,
    raw_candidate_source = "candidate_diagnostic_audit",
    source_candidate_class = candidate_class,
    source_final_label = candidate_class,
    direct_support_isi = seq.int(start_isi, end_isi),
    final_product_type = if (node_role == "final_product") switch(
      semantic_track, event = "Event", gap = "Gap", state = "State",
      review = "Review", diagnostic = "Diagnostic"
    ) else NA_character_,
    final_product_id = if (node_role == "final_product") {
      paste0(tolower(final_product_type), "_", source_candidate_id)
    } else NA_character_) {
  run_id <- "run_gate1"
  params_hash <- lineage_sha("a")
  block <- paste0(dataset_id, "_block")
  payload_hash <- stpd_threshold_first_hash_domain(
    "gate1-test-source-payload-v1",
    list(dataset_id = dataset_id, train_id = train_id,
         source_candidate_id = source_candidate_id)
  )
  final <- node_role == "final_product"
  product_id <- final_product_id
  product_payload_hash <- if (final) stpd_threshold_first_hash_domain(
    "gate1-test-product-payload-v1",
    list(product_id = product_id, start_isi = start_isi, end_isi = end_isi,
         direct_support_isi = as.integer(direct_support_isi))
  ) else NA_character_
  support_indices <- if (final) stpd_candidate_lineage_encode_direct_support(
    as.integer(direct_support_isi)
  ) else NA_character_
  support_hash <- if (final) stpd_candidate_lineage_direct_support_sha256(
    as.integer(direct_support_isi)
  ) else NA_character_
  id <- stpd_candidate_lineage_node_id(
    run_id, params_hash, dataset_id, train_id,
    paste0(dataset_id, "_group"), "fold_1", block,
    source_candidate_id, payload_hash, family_id, semantic_track,
    node_role, candidate_class, node_origin,
    source_candidate_class, source_final_label,
    raw_candidate_layer, raw_candidate_class, raw_final_label,
    raw_candidate_source, provider_id, "threshold_first",
    start_isi, end_isi, start_isi, end_isi, created_stage,
    NA_real_, 0.02, "stable", "adequate", FALSE,
    end_isi - start_isi + 1L, 0L, 0L, 1, 0, 0,
    final_product_type, product_id,
    product_payload_hash, support_hash,
    support_indices,
    if (final) length(direct_support_isi) else NA_integer_
  )
  data.frame(
    schema_version = STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION,
    candidate_node_id = id, run_id = run_id, params_hash = params_hash,
    dataset_id = dataset_id, train_id = train_id,
    group_id = paste0(dataset_id, "_group"), fold_id = "fold_1",
    analysis_block_id = block, family_id = family_id,
    semantic_track = semantic_track, node_role = node_role,
    candidate_class = candidate_class, node_origin = node_origin,
    source_candidate_class = source_candidate_class,
    source_final_label = source_final_label,
    raw_candidate_layer = raw_candidate_layer,
    raw_candidate_class = raw_candidate_class,
    raw_final_label = raw_final_label,
    raw_candidate_source = raw_candidate_source,
    source_candidate_id = source_candidate_id,
    source_payload_sha256 = payload_hash,
    provider_id = provider_id, source_mode = "threshold_first",
    created_stage = created_stage,
    created_stage_order = as.integer(match(
      created_stage, STPD_CANDIDATE_LINEAGE_STAGES
    )),
    created_sequence = as.integer(created_sequence), rank = as.integer(rank),
    original_start_isi = as.integer(start_isi),
    original_end_isi = as.integer(end_isi),
    current_start_isi = as.integer(start_isi),
    current_end_isi = as.integer(end_isi),
    threshold_lower_sec = NA_real_, threshold_upper_sec = 0.02,
    stability_status = "stable", model_adequacy_status = "adequate",
    gray_zone = FALSE,
    direct_isi_count = as.integer(end_isi - start_isi + 1L),
    bridge_isi_count = 0L, borrowed_isi_count = 0L,
    direct_fraction = 1, bridge_fraction = 0, borrowed_fraction = 0,
    terminal_decision = NA_character_,
    node_first_failure_stage = NA_character_,
    node_first_failure_reason = NA_character_,
    final_product_type = final_product_type,
    final_product_id = product_id,
    product_payload_sha256 = product_payload_hash,
    direct_support_sha256 = support_hash,
    direct_support_isi_indices = support_indices,
    direct_support_isi_count = if (final) {
      as.integer(length(direct_support_isi))
    } else NA_integer_,
    runtime_ms = 0, candidate_count = as.integer(candidate_count),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

lineage_rehash_node <- function(row) {
  stpd_candidate_lineage_node_id(
    row$run_id, row$params_hash, row$dataset_id, row$train_id,
    row$group_id, row$fold_id, row$analysis_block_id,
    row$source_candidate_id, row$source_payload_sha256, row$family_id,
    row$semantic_track, row$node_role, row$candidate_class,
    row$node_origin, row$source_candidate_class, row$source_final_label,
    row$raw_candidate_layer, row$raw_candidate_class, row$raw_final_label,
    row$raw_candidate_source, row$provider_id, row$source_mode,
    row$original_start_isi, row$original_end_isi,
    row$current_start_isi, row$current_end_isi, row$created_stage,
    row$threshold_lower_sec, row$threshold_upper_sec,
    row$stability_status, row$model_adequacy_status, row$gray_zone,
    row$direct_isi_count, row$bridge_isi_count, row$borrowed_isi_count,
    row$direct_fraction, row$bridge_fraction, row$borrowed_fraction,
    row$final_product_type, row$final_product_id,
    row$product_payload_sha256, row$direct_support_sha256,
    row$direct_support_isi_indices,
    row$direct_support_isi_count
  )
}

lineage_edge <- function(
    parent, child, relation, operation_id, operation_sequence,
    reason_code) {
  stage <- child$created_stage
  operation_id <- stpd_candidate_lineage_operation_id(
    parent$run_id, parent$params_hash, parent$dataset_id, parent$train_id,
    parent$group_id, parent$fold_id, parent$analysis_block_id, stage,
    relation, parent$candidate_node_id, child$candidate_node_id,
    reason_code
  )
  id <- stpd_candidate_lineage_edge_id(
    parent$run_id, parent$params_hash, parent$dataset_id, parent$train_id,
    parent$group_id, parent$fold_id, parent$analysis_block_id,
    operation_id, parent$candidate_node_id, child$candidate_node_id,
    relation, stage,
    reason_code
  )
  data.frame(
    schema_version = STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION,
    lineage_edge_id = id, run_id = parent$run_id,
    params_hash = parent$params_hash,
    dataset_id = parent$dataset_id, train_id = parent$train_id,
    group_id = parent$group_id, fold_id = parent$fold_id,
    analysis_block_id = parent$analysis_block_id,
    operation_id = operation_id,
    operation_sequence = as.integer(operation_sequence),
    parent_candidate_node_id = parent$candidate_node_id,
    child_candidate_node_id = child$candidate_node_id,
    relation = relation, stage = stage,
    stage_order = child$created_stage_order, reason_code = reason_code,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

lineage_operation <- function(
    parents, children, relation, operation_sequence, reason_code) {
  parent_ids <- parents$candidate_node_id
  child_ids <- children$candidate_node_id
  stage <- children$created_stage[1L]
  operation_id <- stpd_candidate_lineage_operation_id(
    parents$run_id[1L], parents$params_hash[1L], parents$dataset_id[1L],
    parents$train_id[1L], parents$group_id[1L], parents$fold_id[1L],
    parents$analysis_block_id[1L], stage, relation, parent_ids, child_ids,
    reason_code
  )
  rows <- list()
  for (parent_id in parent_ids) for (child_id in child_ids) {
    parent <- parents[parents$candidate_node_id == parent_id, , drop = FALSE]
    child <- children[children$candidate_node_id == child_id, , drop = FALSE]
    row <- lineage_edge(
      parent, child, relation, operation_id, operation_sequence, reason_code
    )
    row$operation_id <- operation_id
    row$lineage_edge_id <- stpd_candidate_lineage_edge_id(
      row$run_id, row$params_hash, row$dataset_id, row$train_id,
      row$group_id, row$fold_id, row$analysis_block_id, operation_id,
      row$parent_candidate_node_id, row$child_candidate_node_id,
      row$relation, row$stage, row$reason_code
    )
    rows[[length(rows) + 1L]] <- row
  }
  lineage_bind(rows, "candidate_lineage_edges")
}

lineage_stage_reason <- function(stage) {
  switch(
    stage,
    raw_threshold_support_proposed = "threshold_support",
    seed_run_formed = "seed_formed",
    structure_candidate_proposed = "structure_supported",
    bridge_or_boundary_expanded = "boundary_expanded",
    burst_pause_ownership_resolved = "ownership_resolved",
    candidate_gate_adjudicated = "gate_passed",
    within_track_selected = "within_track_winner",
    post_size_isi_validated = "post_size_isi_passed",
    multitrack_materialized = "continues"
  )
}

lineage_events <- function(
    node, terminal_stage, terminal_decision, terminal_reason,
    final_product_id = NA_character_, cap_stage = NA_character_,
    candidate_count = 0L, candidate_cap = 0L,
    cap_rank = node$rank,
    cap_policy_id = "test_top_k_v1") {
  terminal_order <- match(terminal_stage, STPD_CANDIDATE_LINEAGE_STAGES)
  rows <- lapply(seq_along(STPD_CANDIDATE_LINEAGE_STAGES), function(i) {
    stage <- STPD_CANDIDATE_LINEAGE_STAGES[i]
    if (i < node$created_stage_order) {
      decision <- "not_applicable"
      reason <- "not_yet_created"
    } else if (i == terminal_order) {
      decision <- terminal_decision
      reason <- terminal_reason
    } else if (i > terminal_order) {
      decision <- "not_applicable"
      reason <- "terminal_previously_reached"
    } else {
      decision <- if (i == node$created_stage_order) {
        "proposed"
      } else if (stage == "within_track_selected") {
        "selected"
      } else {
        "retained"
      }
      reason <- lineage_stage_reason(stage)
    }
    cap_member <- !is.na(cap_stage) && identical(stage, cap_stage)
    source_code <- paste0("source_", decision)
    stage_scope <- if (cap_member) {
      stpd_candidate_lineage_cap_scope_id(
        node$run_id, node$params_hash, node$dataset_id, node$train_id,
        node$group_id, node$fold_id, node$analysis_block_id,
        node$family_id, node$semantic_track, stage, cap_policy_id
      )
    } else paste(
      node$run_id, node$dataset_id, node$train_id,
      node$analysis_block_id, stage, sep = "::"
    )
    materialized <- decision == "materialized"
    data.frame(
      schema_version = STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION,
      stage_event_id = stpd_candidate_lineage_stage_event_id(
        node$run_id, node$params_hash, stage_scope, node$candidate_node_id,
        stage, decision, reason, source_code,
        node$current_start_isi, node$current_end_isi,
        NA_character_,
        if (materialized) node$final_product_type else NA_character_,
        if (materialized) final_product_id else NA_character_,
        if (cap_member) as.integer(candidate_count) else 0L,
        if (cap_member) as.integer(candidate_cap) else 0L,
        cap_member, if (cap_member) as.integer(cap_rank) else 0L,
        if (cap_member) cap_policy_id else "",
        if (cap_member) "complete" else "not_applicable"
      ),
      run_id = node$run_id, params_hash = node$params_hash,
      dataset_id = node$dataset_id,
      train_id = node$train_id,
      group_id = node$group_id, fold_id = node$fold_id,
      analysis_block_id = node$analysis_block_id,
      stage_scope_id = stage_scope,
      candidate_node_id = node$candidate_node_id,
      stage = stage, stage_order = as.integer(i), decision = decision,
      reason_code = reason,
      reason_precedence = as.integer(lineage_reason_precedence(reason)),
      source_decision_code = source_code,
      current_start_isi = node$current_start_isi,
      current_end_isi = node$current_end_isi,
      overlap_winner_candidate_node_id = NA_character_,
      final_product_type = if (materialized) {
        node$final_product_type
      } else NA_character_,
      final_product_id = if (materialized) final_product_id else NA_character_,
      runtime_ms = 0,
      candidate_count = if (cap_member) as.integer(candidate_count) else 0L,
      candidate_cap = if (cap_member) as.integer(candidate_cap) else 0L,
      cap_scope_member = cap_member,
      cap_rank = if (cap_member) as.integer(cap_rank) else 0L,
      cap_policy_id = if (cap_member) cap_policy_id else "",
      universe_status = if (cap_member) "complete" else "not_applicable",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  lineage_bind(rows, "candidate_stage_events")
}

lineage_cap_policy <- function(
    node, stage, candidate_cap, policy_id = "test_top_k_v1",
    policy_status = "applied_complete") {
  data.frame(
    stage_scope_id = stpd_candidate_lineage_cap_scope_id(
      node$run_id, node$params_hash, node$dataset_id, node$train_id,
      node$group_id, node$fold_id, node$analysis_block_id,
      node$family_id, node$semantic_track, stage, policy_id
    ),
    run_id = node$run_id, params_hash = node$params_hash,
    dataset_id = node$dataset_id, train_id = node$train_id,
    group_id = node$group_id, fold_id = node$fold_id,
    analysis_block_id = node$analysis_block_id, family_id = node$family_id,
    semantic_track = node$semantic_track, stage = stage,
    stage_order = as.integer(match(stage, STPD_CANDIDATE_LINEAGE_STAGES)),
    candidate_cap = as.integer(candidate_cap), policy_id = policy_id,
    policy_status = policy_status,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

lineage_fixture <- function() {
  merge_p1 <- lineage_node(
    "merge_ds", "merge_train", "merge_p1", created_sequence = 1L,
    start_isi = 1L, end_isi = 4L
  )
  merge_p2 <- lineage_node(
    "merge_ds", "merge_train", "merge_p2", created_sequence = 2L,
    start_isi = 6L, end_isi = 9L
  )
  merge_child <- lineage_node(
    "merge_ds", "merge_train", "merge_child",
    created_stage = "bridge_or_boundary_expanded", created_sequence = 3L,
    start_isi = 1L, end_isi = 9L
  )
  final <- lineage_node(
    "merge_ds", "merge_train", "merge_final",
    node_role = "final_product",
    created_stage = "multitrack_materialized", created_sequence = 4L,
    start_isi = 1L, end_isi = 9L, final_product_id = "event_merge"
  )

  split_parent <- lineage_node(
    "split_ds", "split_train", "split_parent", created_sequence = 1L,
    start_isi = 20L, end_isi = 30L
  )
  split_c1 <- lineage_node(
    "split_ds", "split_train", "split_c1",
    created_stage = "burst_pause_ownership_resolved", created_sequence = 2L,
    start_isi = 20L, end_isi = 24L
  )
  split_c2 <- lineage_node(
    "split_ds", "split_train", "split_c2",
    created_stage = "burst_pause_ownership_resolved", created_sequence = 3L,
    start_isi = 26L, end_isi = 30L
  )

  cap_1 <- lineage_node(
    "cap_ds", "cap_train", "cap_1", created_sequence = 1L,
    candidate_count = 3L
  )
  cap_2 <- lineage_node(
    "cap_ds", "cap_train", "cap_2", created_sequence = 2L,
    rank = 2L, start_isi = 10L, end_isi = 13L, candidate_count = 3L
  )
  cap_3 <- lineage_node(
    "cap_ds", "cap_train", "cap_3", created_sequence = 3L,
    rank = 3L, start_isi = 20L, end_isi = 23L, candidate_count = 3L
  )

  nodes <- lineage_bind(list(
    merge_p1, merge_p2, merge_child, final,
    split_parent, split_c1, split_c2, cap_1, cap_2, cap_3
  ), "candidate_nodes")
  edges <- lineage_bind(list(
    lineage_operation(
      rbind(merge_p1, merge_p2), merge_child, "merge", 1L,
      "merged_into_child"
    ),
    lineage_edge(
      merge_child, final, "supersede", "final_op", 2L,
      "superseded_by_child"
    ),
    lineage_operation(
      split_parent, rbind(split_c1, split_c2), "split", 1L,
      "split_at_pause"
    )
  ), "candidate_lineage_edges")
  cap_stage <- "bridge_or_boundary_expanded"
  events <- lineage_bind(list(
    lineage_events(
      merge_p1, "bridge_or_boundary_expanded", "superseded",
      "merged_into_child"
    ),
    lineage_events(
      merge_p2, "bridge_or_boundary_expanded", "superseded",
      "merged_into_child"
    ),
    lineage_events(
      merge_child, "multitrack_materialized", "superseded",
      "superseded_by_child"
    ),
    lineage_events(
      final, "multitrack_materialized", "materialized", "finalized",
      final_product_id = "event_merge"
    ),
    lineage_events(
      split_parent, "burst_pause_ownership_resolved", "superseded",
      "split_at_pause"
    ),
    lineage_events(
      split_c1, "post_size_isi_validated", "rejected",
      "post_size_isi_failed"
    ),
    lineage_events(
      split_c2, "post_size_isi_validated", "rejected",
      "post_size_isi_failed"
    ),
    lineage_events(
      cap_1, "post_size_isi_validated", "rejected",
      "post_size_isi_failed", cap_stage = cap_stage,
      candidate_count = 3L, candidate_cap = 2L
    ),
    lineage_events(
      cap_2, "post_size_isi_validated", "rejected",
      "post_size_isi_failed", cap_stage = cap_stage,
      candidate_count = 3L, candidate_cap = 2L
    ),
    lineage_events(
      cap_3, cap_stage, "truncated_by_cap", "candidate_cap_rank",
      cap_stage = cap_stage, candidate_count = 3L, candidate_cap = 2L
    )
  ), "candidate_stage_events")
  cap_policies <- lineage_cap_policy(cap_1, cap_stage, 2L)
  list(
    nodes = nodes, edges = edges, events = events,
    cap_policies = cap_policies
  )
}

lineage_build <- function(
    fixture, audit_level = "full",
    validation_mode = "publication_authoritative") {
  stpd_candidate_lineage_build(
    fixture$nodes, fixture$edges, fixture$events, audit_level,
    fixture$cap_policies, validation_mode
  )
}

lineage_authoritative_products <- function(bundle) {
  final <- bundle$candidate_nodes[
    bundle$candidate_nodes$node_role == "final_product", , drop = FALSE
  ]
  data.frame(
    run_id = final$run_id, params_hash = final$params_hash,
    dataset_id = final$dataset_id, train_id = final$train_id,
    group_id = final$group_id, fold_id = final$fold_id,
    analysis_block_id = final$analysis_block_id,
    final_product_type = final$final_product_type,
    final_product_id = final$final_product_id,
    semantic_track = final$semantic_track,
    candidate_class = final$candidate_class,
    start_isi = final$current_start_isi, end_isi = final$current_end_isi,
    product_payload_sha256 = final$product_payload_sha256,
    direct_support_sha256 = final$direct_support_sha256,
    direct_support_isi_indices = final$direct_support_isi_indices,
    direct_support_isi_count = final$direct_support_isi_count,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

lineage_state_fixture <- function() {
  root <- lineage_node(
    "state_ds", "state_train", "tonic_root", family_id = "tonic",
    semantic_track = "state", candidate_class = "tonic",
    start_isi = 1L, end_isi = 4L
  )
  final <- lineage_node(
    "state_ds", "state_train", "tonic_final", family_id = "tonic",
    semantic_track = "state", candidate_class = "tonic",
    node_role = "final_product", node_origin = "final_product",
    created_stage = "multitrack_materialized",
    start_isi = 1L, end_isi = 4L, direct_support_isi = c(1L, 2L, 4L),
    final_product_type = "State", final_product_id = "state_tonic"
  )
  nodes <- lineage_bind(list(root, final), "candidate_nodes")
  edges <- lineage_edge(
    root, final, "supersede", "ignored", 1L, "superseded_by_child"
  )
  events <- lineage_bind(list(
    lineage_events(
      root, "multitrack_materialized", "superseded",
      "superseded_by_child"
    ),
    lineage_events(
      final, "multitrack_materialized", "materialized", "finalized",
      final_product_id = "state_tonic"
    )
  ), "candidate_stage_events")
  list(
    nodes = nodes, edges = edges, events = events,
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
}

lineage_transform_fixture <- function() {
  trim_parent <- lineage_node(
    "trim_ds", "trim_train", "trim_parent",
    node_origin = "root", created_stage = "bridge_or_boundary_expanded",
    start_isi = 1L, end_isi = 10L
  )
  trim_child <- lineage_node(
    "trim_ds", "trim_train", "trim_child",
    node_origin = "derived", created_stage = "bridge_or_boundary_expanded",
    start_isi = 2L, end_isi = 9L
  )
  retype_parent <- lineage_node(
    "retype_ds", "retype_train", "retype_parent",
    node_origin = "root",
    created_stage = "burst_pause_ownership_resolved",
    start_isi = 20L, end_isi = 25L
  )
  retype_child <- lineage_node(
    "retype_ds", "retype_train", "retype_child",
    family_id = "tonic", semantic_track = "state",
    candidate_class = "tonic", node_origin = "derived",
    created_stage = "burst_pause_ownership_resolved",
    start_isi = 20L, end_isi = 25L
  )
  nodes <- lineage_bind(list(
    trim_parent, trim_child, retype_parent, retype_child
  ), "candidate_nodes")
  edges <- lineage_bind(list(
    lineage_edge(
      trim_parent, trim_child, "trim", "ignored", 1L,
      "trimmed_into_child"
    ),
    lineage_edge(
      retype_parent, retype_child, "retype", "ignored", 1L,
      "retyped_into_child"
    )
  ), "candidate_lineage_edges")
  events <- lineage_bind(list(
    lineage_events(
      trim_parent, "bridge_or_boundary_expanded", "superseded",
      "trimmed_into_child"
    ),
    lineage_events(
      trim_child, "post_size_isi_validated", "rejected",
      "post_size_isi_failed"
    ),
    lineage_events(
      retype_parent, "burst_pause_ownership_resolved", "superseded",
      "retyped_into_child"
    ),
    lineage_events(
      retype_child, "post_size_isi_validated", "rejected",
      "post_size_isi_failed"
    )
  ), "candidate_stage_events")
  list(
    nodes = nodes, edges = edges, events = events,
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
}

test_that("Gate 1 freezes scoped typed schemas, taxonomy, stages, and relations", {
  schema <- stpd_candidate_lineage_schema()
  expect_identical(names(schema), c(
    "candidate_nodes", "candidate_lineage_edges", "candidate_stage_events"
  ))
  expect_length(STPD_CANDIDATE_LINEAGE_STAGES, 9L)
  expect_identical(STPD_CANDIDATE_LINEAGE_RELATIONS, c(
    "derive", "merge", "split", "trim", "retype", "supersede"
  ))
  expect_identical(
    STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS, c("off", "summary", "full")
  )
  taxonomy <- stpd_candidate_lineage_reason_taxonomy()
  expect_identical(anyDuplicated(taxonomy$reason_code), 0L)
  expect_identical(
    taxonomy$reason_precedence, as.integer(seq_len(nrow(taxonomy)))
  )
  for (name in names(schema)) {
    empty <- stpd_candidate_lineage_empty_table(name)
    expect_identical(names(empty), names(schema[[name]]))
    expect_equal(nrow(empty), 0L)
  }
})

test_that("merge, split, arbitrary-stage cap, and final-product DAG close", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  expect_true(stpd_candidate_lineage_validate_bundle(
    full, "full", fixture$cap_policies
  ))
  cap <- full$candidate_nodes[
    full$candidate_nodes$source_candidate_id == "cap_3", , drop = FALSE
  ]
  expect_identical(cap$terminal_decision, "truncated_by_cap")
  expect_identical(
    cap$node_first_failure_stage, "bridge_or_boundary_expanded"
  )
  expect_identical(cap$node_first_failure_reason, "candidate_cap_rank")
  final <- full$candidate_nodes[
    full$candidate_nodes$node_role == "final_product", , drop = FALSE
  ]
  expect_identical(final$final_product_id, "event_merge")
  expect_equal(sum(full$candidate_lineage_edges$relation == "merge"), 2L)
  expect_equal(sum(full$candidate_lineage_edges$relation == "split"), 2L)
})

test_that("pre-creation N/A is required and node failure cannot be caller-authored", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  child_id <- full$candidate_nodes$candidate_node_id[
    full$candidate_nodes$source_candidate_id == "merge_child"
  ]
  before <- full$candidate_stage_events[
    full$candidate_stage_events$candidate_node_id == child_id &
      full$candidate_stage_events$stage_order < 4L, , drop = FALSE
  ]
  expect_true(all(before$decision == "not_applicable"))
  expect_true(all(before$reason_code == "not_yet_created"))

  tampered <- full
  row <- match("cap_3", tampered$candidate_nodes$source_candidate_id)
  tampered$candidate_nodes$node_first_failure_reason[row] <- "overlap_lost"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      tampered, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_derived_field_mismatch"
  )

  active_before_creation <- full
  row <- which(
    active_before_creation$candidate_stage_events$candidate_node_id == child_id &
      active_before_creation$candidate_stage_events$stage_order == 1L
  )
  active_before_creation$candidate_stage_events$decision[row] <- "retained"
  active_before_creation$candidate_stage_events$reason_code[row] <-
    "threshold_support"
  active_before_creation$candidate_stage_events$reason_precedence[row] <-
    lineage_reason_precedence("threshold_support")
  active_before_creation$candidate_stage_events$stage_event_id[row] <-
    lineage_rehash_event(
      active_before_creation$candidate_stage_events[row, , drop = FALSE]
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      active_before_creation, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_stage_decision_reason_invalid"
  )
})

test_that("scope, DAG time, taxonomy, cap universe, and post-terminal activity fail closed", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  scope_bad <- full
  scope_bad$candidate_lineage_edges$dataset_id[1L] <- "other_dataset"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      scope_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_scope_mismatch"
  )

  taxonomy_bad <- full
  taxonomy_bad$candidate_stage_events$reason_precedence[1L] <- 999L
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      taxonomy_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_reason_taxonomy_invalid"
  )

  incomplete <- full
  cap_row <- which(incomplete$candidate_stage_events$cap_scope_member)[1L]
  incomplete$candidate_stage_events$universe_status[cap_row] <-
    "incomplete_early_stop"
  incomplete$candidate_stage_events$stage_event_id[cap_row] <-
    lineage_rehash_event(
      incomplete$candidate_stage_events[cap_row, , drop = FALSE]
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      incomplete, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_universe_incomplete"
  )

  cap_bad <- full
  cap_bad$candidate_stage_events$candidate_count[cap_row] <- 4L
  cap_bad$candidate_stage_events$stage_event_id[cap_row] <-
    lineage_rehash_event(
      cap_bad$candidate_stage_events[cap_row, , drop = FALSE]
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      cap_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_cap_log_incomplete"
  )

  terminal_bad <- full
  cap_id <- terminal_bad$candidate_nodes$candidate_node_id[
    terminal_bad$candidate_nodes$source_candidate_id == "cap_3"
  ]
  row <- which(
    terminal_bad$candidate_stage_events$candidate_node_id == cap_id &
      terminal_bad$candidate_stage_events$stage_order == 5L
  )
  terminal_bad$candidate_stage_events$decision[row] <- "retained"
  terminal_bad$candidate_stage_events$reason_code[row] <- "continues"
  terminal_bad$candidate_stage_events$reason_precedence[row] <-
    lineage_reason_precedence("continues")
  terminal_bad$candidate_stage_events$stage_event_id[row] <-
    lineage_rehash_event(
      terminal_bad$candidate_stage_events[row, , drop = FALSE]
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      terminal_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_stage_decision_reason_invalid"
  )
})

test_that("builder and canonical IDs are invariant to row permutation", {
  fixture <- lineage_fixture()
  first <- lineage_build(fixture)
  second <- stpd_candidate_lineage_build(
    fixture$nodes[rev(seq_len(nrow(fixture$nodes))), , drop = FALSE],
    fixture$edges[rev(seq_len(nrow(fixture$edges))), , drop = FALSE],
    fixture$events[rev(seq_len(nrow(fixture$events))), , drop = FALSE],
    "full", fixture$cap_policies
  )
  expect_identical(first, second)
  expect_true(all(grepl(
    "^candidate_[0-9a-f]{64}$", first$candidate_nodes$candidate_node_id
  )))
})

test_that("truth-level first failure is independent from node terminal failure", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  truth <- data.frame(
    truth_id = c("truth_merge", "truth_split", "truth_missing"),
    run_id = rep("run_gate1", 3L),
    params_hash = rep(lineage_sha("a"), 3L),
    dataset_id = c("merge_ds", "split_ds", "merge_ds"),
    train_id = c("merge_train", "split_train", "merge_train"),
    group_id = c("merge_ds_group", "split_ds_group", "merge_ds_group"),
    fold_id = rep("fold_1", 3L),
    analysis_block_id = c(
      "merge_ds_block", "split_ds_block", "merge_ds_block"
    ),
    family_id = c("burst", "burst", "pause"),
    truth_start_isi = c(1L, 20L, 40L),
    truth_end_isi = c(9L, 30L, 45L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  evaluated <- stpd_candidate_lineage_truth_node_first_failure(
    truth, full, fixture$cap_policies
  )
  summary <- evaluated$truth_first_failure
  merge <- summary[summary$truth_id == "truth_merge", , drop = FALSE]
  split <- summary[summary$truth_id == "truth_split", , drop = FALSE]
  missing <- summary[summary$truth_id == "truth_missing", , drop = FALSE]
  expect_true(is.na(merge$truth_first_failure_stage))
  expect_identical(split$truth_first_failure_stage, "post_size_isi_validated")
  expect_identical(split$truth_first_failure_reason, "post_size_isi_failed")
  expect_identical(
    missing$truth_first_failure_reason, "no_candidate_proposed"
  )
  expect_gt(nrow(evaluated$truth_node_evidence), nrow(truth))
})

test_that("stage matrix rejects a stage-1 selected/within-track decision", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  row <- which(
    full$candidate_stage_events$stage_order == 1L &
      full$candidate_stage_events$decision == "proposed"
  )[1L]
  full$candidate_stage_events$decision[row] <- "selected"
  full$candidate_stage_events$reason_code[row] <- "within_track_winner"
  full$candidate_stage_events$reason_precedence[row] <-
    lineage_reason_precedence("within_track_winner")
  full$candidate_stage_events$stage_event_id[row] <- lineage_rehash_event(
    full$candidate_stage_events[row, , drop = FALSE]
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      full, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_stage_decision_reason_invalid"
  )
})

test_that("operation geometry and live causal ancestry are cross-validated", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  child_id <- full$candidate_nodes$candidate_node_id[
    full$candidate_nodes$source_candidate_id == "merge_child"
  ]
  geometry_bad <- full
  row <- which(
    geometry_bad$candidate_stage_events$candidate_node_id == child_id &
      geometry_bad$candidate_stage_events$stage ==
        "bridge_or_boundary_expanded"
  )
  geometry_bad$candidate_stage_events$current_start_isi[row] <- 100L
  geometry_bad$candidate_stage_events$current_end_isi[row] <- 108L
  geometry_bad$candidate_stage_events$stage_event_id[row] <-
    lineage_rehash_event(
      geometry_bad$candidate_stage_events[row, , drop = FALSE]
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      geometry_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_silent_geometry_mutation"
  )

  ancestry_bad <- full
  dead_parent <- ancestry_bad$candidate_nodes[
    ancestry_bad$candidate_nodes$source_candidate_id == "merge_p1", ,
    drop = FALSE
  ]
  final <- ancestry_bad$candidate_nodes[
    ancestry_bad$candidate_nodes$node_role == "final_product", , drop = FALSE
  ]
  ancestry_bad$candidate_lineage_edges <- rbind(
    ancestry_bad$candidate_lineage_edges,
    lineage_edge(dead_parent, final, "derive", "dead_derive", 3L, "continues")
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      ancestry_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_operation_event_invalid"
  )
})

test_that("product ontology and authoritative product set fail closed", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  authoritative <- lineage_authoritative_products(full)
  expect_true(stpd_candidate_lineage_validate_authoritative_products(
    full, authoritative, fixture$cap_policies
  ))
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_authoritative_products(
      full, authoritative[0, , drop = FALSE], fixture$cap_policies
    )),
    "stpd_candidate_lineage_authoritative_product_mismatch"
  )
  expect_silent(stpd_candidate_lineage_attach(
    list(scientific = TRUE), full, authoritative, fixture$cap_policies
  ))
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_attach(
      list(scientific = TRUE), full,
      authoritative[0, , drop = FALSE], fixture$cap_policies
    )),
    "stpd_candidate_lineage_authoritative_product_mismatch"
  )

  ontology_bad <- full
  final_row <- which(ontology_bad$candidate_nodes$node_role == "final_product")
  final_id <- ontology_bad$candidate_nodes$candidate_node_id[final_row]
  event_row <- which(
    ontology_bad$candidate_stage_events$candidate_node_id == final_id &
      ontology_bad$candidate_stage_events$decision == "materialized"
  )
  ontology_bad$candidate_nodes$final_product_type[final_row] <- "State"
  ontology_bad$candidate_stage_events$final_product_type[event_row] <- "State"
  ontology_bad$candidate_stage_events$stage_event_id[event_row] <-
    lineage_rehash_event(
      ontology_bad$candidate_stage_events[event_row, , drop = FALSE]
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      ontology_bad, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_product_ontology_invalid"
  )
})

test_that("external cap policy distinguishes complete from unavailable lineage", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      full, "full", stpd_candidate_lineage_empty_cap_policies()
    )),
    "stpd_candidate_lineage_cap_policy_log_mismatch"
  )

  unavailable <- full
  rows <- which(unavailable$candidate_stage_events$cap_scope_member)
  unavailable$candidate_stage_events$universe_status[rows] <-
    "incomplete_early_stop"
  unavailable$candidate_stage_events$stage_event_id[rows] <- vapply(
    rows, function(i) lineage_rehash_event(
      unavailable$candidate_stage_events[i, , drop = FALSE]
    ), character(1)
  )
  incomplete_policy <- fixture$cap_policies
  incomplete_policy$policy_status <- "incomplete_early_stop"
  expect_true(stpd_candidate_lineage_validate_bundle(
    unavailable, "full", incomplete_policy, "diagnostic_unavailable"
  ))
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      unavailable, "full", incomplete_policy, "publication_authoritative"
    )),
    "stpd_candidate_lineage_universe_incomplete"
  )

  incomplete_attached <- stpd_candidate_lineage_attach(
    list(scientific = TRUE), unavailable,
    expected_cap_scopes = incomplete_policy,
    validation_mode = "diagnostic_unavailable"
  )
  expect_identical(
    incomplete_attached$candidate_lineage_audit$metadata$unavailable_reason,
    "incomplete_early_stop"
  )
  expect_false(
    incomplete_attached$candidate_lineage_audit$metadata$universe_complete
  )
  expect_true(stpd_candidate_lineage_validate_envelope(
    incomplete_attached$candidate_lineage_audit
  ))

  expect_s3_class(
    lineage_error(stpd_candidate_lineage_attach(
      list(scientific = TRUE), full,
      expected_cap_scopes = fixture$cap_policies,
      validation_mode = "diagnostic_unavailable"
    )),
    "stpd_candidate_lineage_unavailable_reason_required"
  )
  complete_diagnostic <- stpd_candidate_lineage_attach(
    list(scientific = TRUE), full,
    expected_cap_scopes = fixture$cap_policies,
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = "pipeline_not_instrumented"
  )
  expect_identical(
    complete_diagnostic$candidate_lineage_audit$metadata$unavailable_reason,
    "pipeline_not_instrumented"
  )
  expect_false(
    complete_diagnostic$candidate_lineage_audit$metadata$universe_complete
  )
  tampered_reason <- complete_diagnostic$candidate_lineage_audit
  tampered_reason$metadata$unavailable_reason <- "diagnostic_only"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_envelope(tampered_reason)),
    "stpd_candidate_lineage_audit_manifest_hash_mismatch"
  )

})

test_that("truth evaluator rejects silent within-node geometry mutation", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  parent_id <- full$candidate_nodes$candidate_node_id[
    full$candidate_nodes$source_candidate_id == "split_parent"
  ]
  moved <- full
  row <- which(
    moved$candidate_stage_events$candidate_node_id == parent_id &
      moved$candidate_stage_events$stage_order == 2L
  )
  moved$candidate_stage_events$current_start_isi[row] <- 100L
  moved$candidate_stage_events$current_end_isi[row] <- 110L
  moved$candidate_stage_events$stage_event_id[row] <- lineage_rehash_event(
    moved$candidate_stage_events[row, , drop = FALSE]
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      moved, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_silent_geometry_mutation"
  )
})

test_that("node identity binds full scope and scientific projection removes only top-level audit", {
  node <- lineage_node("id_ds", "id_train", "id_source")
  changed <- node
  changed$group_id <- "changed_group"
  changed$candidate_node_id <- lineage_rehash_node(changed)
  expect_false(identical(node$candidate_node_id, changed$candidate_node_id))

  scientific <- list(
    candidate_nodes = "legacy_scientific_field",
    nested = list(candidate_nodes = "must_remain"),
    candidate_lineage_audit = list(candidate_nodes = "audit_only")
  )
  projected <- stpd_candidate_lineage_scientific_projection(scientific)
  expect_identical(projected$candidate_nodes, "legacy_scientific_field")
  expect_identical(projected$nested$candidate_nodes, "must_remain")
  expect_false("candidate_lineage_audit" %in% names(projected))
})

test_that("audit materialization preserves RNG and independent scientific projection hash", {
  fixture <- lineage_fixture()
  scientific <- list(
    run_metadata = list(run_id = "run_gate1", params_hash = lineage_sha("a")),
    results = list(
      detector_rows = data.frame(
        idx = 1:3, pattern_auto = c("burst", "", "pause"),
        stringsAsFactors = FALSE
      ),
      events = data.frame(
        event_id = c("E1", "G1"), type = c("burst", "pause"),
        stringsAsFactors = FALSE
      )
    )
  )
  set.seed(73019)
  seed_before <- .Random.seed
  full <- lineage_build(fixture, "full")
  summary <- lineage_build(
    fixture, "summary", "diagnostic_unavailable"
  )
  off <- lineage_build(fixture, "off", "diagnostic_unavailable")
  attached <- lapply(
    seq_along(list(off, summary, full)),
    function(i) {
      audit <- list(off, summary, full)[[i]]
      is_full <- nrow(audit$candidate_stage_events) > 0L
      stpd_candidate_lineage_attach(
        scientific, audit,
        authoritative_products = if (is_full) {
          lineage_authoritative_products(audit)
        } else stpd_candidate_lineage_empty_authoritative_products(),
        expected_cap_scopes = if (is_full) {
          fixture$cap_policies
        } else stpd_candidate_lineage_empty_cap_policies(),
        validation_mode = if (is_full) {
          "publication_authoritative"
        } else "diagnostic_unavailable",
        unavailable_reason = if (is_full) NULL else
          "pipeline_not_instrumented"
      )
    }
  )
  hashes <- c(
    stpd_candidate_lineage_scientific_projection_hash(scientific),
    vapply(
      attached, stpd_candidate_lineage_scientific_projection_hash,
      character(1)
    )
  )
  expect_identical(.Random.seed, seed_before)
  expect_length(unique(hashes), 1L)
  expect_identical(
    stpd_candidate_lineage_scientific_projection(attached[[3L]]), scientific
  )
  expect_equal(nrow(off$candidate_nodes), 0L)
  expect_equal(nrow(summary$candidate_stage_events), 0L)
  expect_gt(nrow(full$candidate_stage_events), 0L)
  expect_true(stpd_candidate_lineage_validate_envelope(
    attached[[3L]]$candidate_lineage_audit
  ))
})

test_that("v3 publication APIs require explicit manifests and preserve zero-row envelopes", {
  fixture <- lineage_fixture()
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      lineage_build(fixture), "full"
    )),
    "stpd_candidate_lineage_cap_policy_manifest_required"
  )
  full <- lineage_build(fixture)
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_attach(
      list(scientific = TRUE), full,
      expected_cap_scopes = fixture$cap_policies
    )),
    "stpd_candidate_lineage_authoritative_products_required"
  )
  off <- lineage_build(fixture, "off", "diagnostic_unavailable")
  summary <- lineage_build(
    fixture, "summary", "diagnostic_unavailable"
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      off, "off", stpd_candidate_lineage_empty_cap_policies()
    )),
    "stpd_candidate_lineage_publication_authority_requires_full_audit"
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      summary, "summary", stpd_candidate_lineage_empty_cap_policies()
    )),
    "stpd_candidate_lineage_publication_authority_requires_full_audit"
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_attach(
      list(scientific = TRUE), off,
      stpd_candidate_lineage_empty_authoritative_products(),
      stpd_candidate_lineage_empty_cap_policies()
    )),
    "stpd_candidate_lineage_publication_authority_requires_full_audit"
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_attach(
      list(scientific = TRUE), summary,
      stpd_candidate_lineage_empty_authoritative_products(),
      stpd_candidate_lineage_empty_cap_policies()
    )),
    "stpd_candidate_lineage_publication_authority_requires_full_audit"
  )
  attached <- stpd_candidate_lineage_attach(
    list(scientific = TRUE), off,
    stpd_candidate_lineage_empty_authoritative_products(),
    stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = "pipeline_not_instrumented"
  )
  expect_true(stpd_candidate_lineage_validate_envelope(
    attached$candidate_lineage_audit
  ))
  expect_identical(
    attached$candidate_lineage_audit$metadata$audit_level, "off"
  )
  expect_identical(
    attached$candidate_lineage_audit$metadata$evidence_authority,
    "diagnostic_unavailable"
  )
})

test_that("raw source registry is explicit, multi-track, and never defaults to other", {
  review <- lineage_node(
    "registry_ds", "review_train", "possible_burst",
    semantic_track = "review", candidate_class = "burst",
    raw_candidate_layer = "review", raw_candidate_class = "possible_burst",
    raw_final_label = "burst"
  )
  review$terminal_decision <- "rejected"
  diagnostic <- lineage_node(
    "registry_ds", "diagnostic_train", "tonic_diagnostic",
    family_id = "tonic", semantic_track = "diagnostic",
    candidate_class = "tonic", raw_candidate_layer = "diagnostic",
    raw_candidate_class = "tonic", raw_final_label = "tonic"
  )
  diagnostic$terminal_decision <- "rejected"
  expect_true(stpd_candidate_lineage_validate_nodes(review))
  expect_true(stpd_candidate_lineage_validate_nodes(diagnostic))

  native_reject <- lineage_node(
    "registry_ds", "native_reject_train", "native_reject",
    semantic_track = "diagnostic", candidate_class = "burst",
    provider_id = "native_stpd",
    raw_candidate_layer = "event_grammar_burst_event",
    raw_candidate_class = "event_grammar_seed_centered_burst",
    raw_final_label = "reject", raw_candidate_source = "",
    source_candidate_class = "burst", source_final_label = "reject"
  )
  native_reject$terminal_decision <- "rejected"
  native_hft <- lineage_node(
    "registry_ds", "native_hft_train", "native_hft",
    family_id = "broad_hfs", semantic_track = "state",
    candidate_class = "hft", provider_id = "native_stpd",
    raw_candidate_layer = "event_core_hf_tonic_state",
    raw_candidate_class = "event_core_hf_tonic",
    raw_final_label = "high_frequency_tonic", raw_candidate_source = "",
    source_candidate_class = "hft",
    source_final_label = "high_frequency_tonic"
  )
  native_hft$terminal_decision <- "rejected"
  expect_true(stpd_candidate_lineage_validate_nodes(native_reject))
  expect_true(stpd_candidate_lineage_validate_nodes(native_hft))
  expect_identical(native_hft$family_id, "broad_hfs")

  profile <- lineage_node(
    "registry_ds", "profile_train", "train_profile",
    family_id = "tonic", semantic_track = "diagnostic",
    candidate_class = "tonic", raw_candidate_layer = "profile",
    raw_candidate_class = "train_profile", raw_final_label = "tonic"
  )
  profile$terminal_decision <- "rejected"
  profile$current_start_isi <- profile$current_end_isi <- NA_integer_
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(profile)),
    "stpd_candidate_lineage_profile_node_forbidden"
  )

  other <- lineage_node(
    "registry_ds", "other_train", "raw_other",
    raw_candidate_class = "other", raw_final_label = "other"
  )
  other$terminal_decision <- "rejected"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(other)),
    "stpd_candidate_lineage_other_candidate_forbidden"
  )

  adapter <- data.frame(
    registry_version = "stpd_source_registry_v1",
    registry_source = "external", provider_id = "external_detector",
    raw_candidate_layer = "candidate",
    raw_candidate_class = "spike_cluster",
    raw_final_label = "candidate_burst",
    raw_candidate_source = "external_v1",
    source_candidate_class = "burst", source_final_label = "burst",
    candidate_class = "burst", semantic_track = "event", family_id = "burst",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  external <- lineage_node(
    "registry_ds", "external_train", "external_1",
    provider_id = "external_detector", raw_candidate_layer = "candidate",
    raw_candidate_class = "spike_cluster",
    raw_final_label = "candidate_burst",
    raw_candidate_source = "external_v1"
  )
  external$terminal_decision <- "rejected"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(external)),
    "stpd_candidate_lineage_adapter_mapping_missing"
  )
  expect_true(stpd_candidate_lineage_validate_nodes(external, adapter))
  expect_identical(external$raw_candidate_class, "spike_cluster")

  profile_adapter <- adapter
  profile_adapter$raw_candidate_layer <- "profile"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(
      profile_adapter
    )),
    "stpd_candidate_lineage_source_adapter_forbidden_route"
  )
  composite_adapter <- adapter
  composite_adapter$raw_candidate_class <- "composite"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(
      composite_adapter
    )),
    "stpd_candidate_lineage_source_adapter_forbidden_route"
  )
  other_source_adapter <- adapter
  other_source_adapter$raw_candidate_source <- "others"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(
      other_source_adapter
    )),
    "stpd_candidate_lineage_source_adapter_invalid"
  )

  external_fixture <- list(
    nodes = external,
    edges = stpd_candidate_lineage_empty_table("candidate_lineage_edges"),
    events = lineage_events(
      external, "post_size_isi_validated", "rejected",
      "post_size_isi_failed"
    ),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  external_bundle <- stpd_candidate_lineage_build(
    external_fixture$nodes, external_fixture$edges,
    external_fixture$events, "full", external_fixture$cap_policies,
    source_adapters = adapter
  )
  attached <- stpd_candidate_lineage_attach(
    list(scientific = TRUE), external_bundle,
    stpd_candidate_lineage_empty_authoritative_products(),
    external_fixture$cap_policies, source_adapters = adapter
  )
  expect_identical(
    attached$candidate_lineage_audit$source_adapter_registry, adapter
  )
  expect_true(stpd_candidate_lineage_validate_envelope(
    attached$candidate_lineage_audit
  ))
  tampered <- attached$candidate_lineage_audit
  tampered$metadata$source_registry_sha256 <- lineage_sha("f")
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_envelope(tampered)),
    "stpd_candidate_lineage_audit_manifest_hash_mismatch"
  )
})

test_that("native later roots and same-stage trim/retype have causal order", {
  native <- lineage_node(
    "native_ds", "native_train", "native_stage3", node_origin = "root",
    created_stage = "structure_candidate_proposed",
    start_isi = 5L, end_isi = 8L
  )
  native_fixture <- list(
    nodes = native,
    edges = stpd_candidate_lineage_empty_table("candidate_lineage_edges"),
    events = lineage_events(
      native, "post_size_isi_validated", "rejected",
      "post_size_isi_failed"
    ),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  expect_silent(lineage_build(native_fixture))

  transformed <- lineage_build(lineage_transform_fixture())
  expect_true(stpd_candidate_lineage_validate_bundle(
    transformed, "full", stpd_candidate_lineage_empty_cap_policies()
  ))
  for (relation in c("trim", "retype")) {
    edge <- transformed$candidate_lineage_edges[
      transformed$candidate_lineage_edges$relation == relation, , drop = FALSE
    ]
    parent <- transformed$candidate_nodes[
      transformed$candidate_nodes$candidate_node_id ==
        edge$parent_candidate_node_id, , drop = FALSE
    ]
    child <- transformed$candidate_nodes[
      transformed$candidate_nodes$candidate_node_id ==
        edge$child_candidate_node_id, , drop = FALSE
    ]
    expect_identical(parent$created_stage, child$created_stage)
    expect_lt(parent$created_sequence, child$created_sequence)
  }
})

test_that("natural node and operation identities exclude caller sequence and locale", {
  first_node <- lineage_node(
    "natural_ds", "natural_train", "same_payload", created_sequence = 1L
  )
  second_node <- lineage_node(
    "natural_ds", "natural_train", "same_payload", created_sequence = 999L
  )
  ranked_node <- lineage_node(
    "natural_ds", "natural_train", "same_payload", rank = 999L
  )
  expect_identical(first_node$candidate_node_id,
                   second_node$candidate_node_id)
  expect_identical(first_node$candidate_node_id,
                   ranked_node$candidate_node_id)

  fixture <- lineage_fixture()
  current_locale <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", current_locale)),
          add = TRUE)
  baseline <- lineage_build(fixture)
  suppressWarnings(Sys.setlocale("LC_COLLATE", "C"))
  permuted <- stpd_candidate_lineage_build(
    fixture$nodes[rev(seq_len(nrow(fixture$nodes))), , drop = FALSE],
    fixture$edges[rev(seq_len(nrow(fixture$edges))), , drop = FALSE],
    fixture$events[rev(seq_len(nrow(fixture$events))), , drop = FALSE],
    "full", fixture$cap_policies
  )
  expect_identical(baseline, permuted)
  expect_true(all(grepl(
    "^operation_[0-9a-f]{64}$",
    baseline$candidate_lineage_edges$operation_id
  )))

  wrong_creation_order <- baseline
  rows <- which(
    wrong_creation_order$candidate_nodes$dataset_id == "merge_ds" &
      wrong_creation_order$candidate_nodes$created_stage_order == 1L
  )
  wrong_creation_order$candidate_nodes$created_sequence[rows] <-
    rev(wrong_creation_order$candidate_nodes$created_sequence[rows])
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      wrong_creation_order, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_creation_sequence_noncanonical"
  )

  wrong_operation_order <- baseline
  rows <- which(wrong_operation_order$candidate_lineage_edges$dataset_id ==
                  "merge_ds" &
                  wrong_operation_order$candidate_lineage_edges$stage ==
                    "bridge_or_boundary_expanded")
  wrong_operation_order$candidate_lineage_edges$operation_sequence[rows] <- 2L
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      wrong_operation_order, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_operation_sequence_noncanonical"
  )

  wrong_operation_id <- baseline
  wrong_operation_id$candidate_lineage_edges$operation_id[rows] <-
    paste0("operation_", lineage_sha("f"))
  for (row in rows) {
    edge <- wrong_operation_id$candidate_lineage_edges[row, , drop = FALSE]
    wrong_operation_id$candidate_lineage_edges$lineage_edge_id[row] <-
      stpd_candidate_lineage_edge_id(
        edge$run_id, edge$params_hash, edge$dataset_id, edge$train_id,
        edge$group_id, edge$fold_id, edge$analysis_block_id,
        edge$operation_id, edge$parent_candidate_node_id,
        edge$child_candidate_node_id, edge$relation, edge$stage,
        edge$reason_code
      )
  }
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      wrong_operation_id, "full", fixture$cap_policies
    )),
    "stpd_candidate_lineage_operation_identity_mismatch"
  )
})

test_that("cap rank is stage-local and may reorder the same candidate", {
  fixture <- lineage_fixture()
  cap_ids <- c("cap_1", "cap_2", "cap_3")
  stage3_ranks <- c(3L, 1L, 2L)
  policy_id <- "stage3_top_k_v1"
  for (j in seq_along(cap_ids)) {
    node <- fixture$nodes[
      fixture$nodes$source_candidate_id == cap_ids[j], , drop = FALSE
    ]
    row <- which(
      fixture$events$candidate_node_id == node$candidate_node_id &
        fixture$events$stage == "structure_candidate_proposed"
    )
    fixture$events$stage_scope_id[row] <-
      stpd_candidate_lineage_cap_scope_id(
        node$run_id, node$params_hash, node$dataset_id, node$train_id,
        node$group_id, node$fold_id, node$analysis_block_id,
        node$family_id, node$semantic_track,
        "structure_candidate_proposed", policy_id
      )
    fixture$events$candidate_count[row] <- 3L
    fixture$events$candidate_cap[row] <- 3L
    fixture$events$cap_scope_member[row] <- TRUE
    fixture$events$cap_rank[row] <- stage3_ranks[j]
    fixture$events$cap_policy_id[row] <- policy_id
    fixture$events$universe_status[row] <- "complete"
    fixture$events$stage_event_id[row] <- lineage_rehash_event(
      fixture$events[row, , drop = FALSE]
    )
  }
  node <- fixture$nodes[fixture$nodes$source_candidate_id == "cap_1", ,
                        drop = FALSE]
  fixture$cap_policies <- rbind(
    fixture$cap_policies,
    lineage_cap_policy(
      node, "structure_candidate_proposed", 3L, policy_id
    )
  )
  full <- lineage_build(fixture)
  cap1 <- full$candidate_nodes$candidate_node_id[
    full$candidate_nodes$source_candidate_id == "cap_1"
  ]
  ranks <- full$candidate_stage_events$cap_rank[
    full$candidate_stage_events$candidate_node_id == cap1 &
      full$candidate_stage_events$cap_scope_member
  ]
  expect_identical(ranks, c(3L, 1L))
})

test_that("truth entry is dynamic and failures exclude unrelated branches", {
  immediate <- lineage_node(
    "truth_immediate_ds", "truth_immediate_train", "immediate",
    start_isi = 1L, end_isi = 4L
  )
  immediate_fixture <- list(
    nodes = immediate,
    edges = stpd_candidate_lineage_empty_table("candidate_lineage_edges"),
    events = lineage_events(
      immediate, "raw_threshold_support_proposed", "rejected",
      "threshold_failed"
    ),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  immediate_bundle <- lineage_build(immediate_fixture)

  relevant <- lineage_node(
    "truth_branch_ds", "truth_branch_train", "relevant",
    start_isi = 10L, end_isi = 13L
  )
  unrelated <- lineage_node(
    "truth_branch_ds", "truth_branch_train", "unrelated",
    start_isi = 30L, end_isi = 33L
  )
  branch_fixture <- list(
    nodes = lineage_bind(list(relevant, unrelated), "candidate_nodes"),
    edges = stpd_candidate_lineage_empty_table("candidate_lineage_edges"),
    events = lineage_bind(list(
      lineage_events(
        relevant, "post_size_isi_validated", "rejected",
        "post_size_isi_failed"
      ),
      lineage_events(
        unrelated, "raw_threshold_support_proposed", "rejected",
        "threshold_failed"
      )
    ), "candidate_stage_events"),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  branch_bundle <- lineage_build(branch_fixture)

  make_truth <- function(node, truth_id, start_isi, end_isi) data.frame(
    truth_id = truth_id, run_id = node$run_id,
    params_hash = node$params_hash, dataset_id = node$dataset_id,
    train_id = node$train_id, group_id = node$group_id,
    fold_id = node$fold_id, analysis_block_id = node$analysis_block_id,
    family_id = node$family_id, truth_start_isi = as.integer(start_isi),
    truth_end_isi = as.integer(end_isi), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  immediate_result <- stpd_candidate_lineage_truth_node_first_failure(
    make_truth(immediate, "truth_immediate", 1L, 4L), immediate_bundle,
    stpd_candidate_lineage_empty_cap_policies()
  )$truth_first_failure
  expect_identical(
    immediate_result$truth_first_failure_stage,
    "raw_threshold_support_proposed"
  )
  expect_identical(immediate_result$truth_first_failure_reason,
                   "threshold_failed")

  branch_result <- stpd_candidate_lineage_truth_node_first_failure(
    make_truth(relevant, "truth_branch", 10L, 13L), branch_bundle,
    stpd_candidate_lineage_empty_cap_policies()
  )$truth_first_failure
  expect_identical(branch_result$truth_first_failure_stage,
                   "post_size_isi_validated")
  expect_identical(branch_result$truth_first_failure_reason,
                   "post_size_isi_failed")

  fixture <- lineage_fixture()
  merged <- lineage_build(fixture)
  merge_node <- fixture$nodes[
    fixture$nodes$source_candidate_id == "merge_p1", , drop = FALSE
  ]
  merge_result <- stpd_candidate_lineage_truth_node_first_failure(
    make_truth(merge_node, "truth_later_merge", 5L, 5L), merged,
    fixture$cap_policies
  )$truth_first_failure
  expect_identical(merge_result$entry_stage,
                   "bridge_or_boundary_expanded")
  expect_true(is.na(merge_result$truth_first_failure_stage))
})

test_that("publication product binding detects State support holes and round-trips", {
  fixture <- lineage_state_fixture()
  full <- lineage_build(fixture)
  authoritative <- lineage_authoritative_products(full)
  expect_true(stpd_candidate_lineage_validate_authoritative_products(
    full, authoritative, fixture$cap_policies
  ))
  changed_holes <- authoritative
  changed_holes$direct_support_isi_indices <-
    stpd_candidate_lineage_encode_direct_support(c(1L, 3L, 4L))
  changed_holes$direct_support_sha256 <-
    stpd_candidate_lineage_direct_support_sha256(c(1L, 3L, 4L))
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_authoritative_products(
      full, changed_holes, fixture$cap_policies
    )),
    "stpd_candidate_lineage_authoritative_product_mismatch"
  )
  changed_payload <- authoritative
  changed_payload$product_payload_sha256 <- lineage_sha("f")
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_authoritative_products(
      full, changed_payload, fixture$cap_policies
    )),
    "stpd_candidate_lineage_authoritative_product_mismatch"
  )

  attached <- stpd_candidate_lineage_attach(
    list(scientific = TRUE), full, authoritative, fixture$cap_policies
  )
  roundtrip <- unserialize(serialize(
    attached$candidate_lineage_audit, NULL, version = 3L
  ))
  expect_true(stpd_candidate_lineage_validate_envelope(roundtrip))
  roundtrip$authoritative_products$direct_support_sha256 <-
    changed_holes$direct_support_sha256
  roundtrip$authoritative_products$direct_support_isi_indices <-
    changed_holes$direct_support_isi_indices
  roundtrip$metadata$product_manifest_sha256 <-
    stpd_threshold_first_hash_domain(
      "stpd-candidate-lineage-product-manifest-v1",
      stpd_candidate_lineage_canonical_product_manifest(
        roundtrip$authoritative_products
      )
    )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_envelope(roundtrip)),
    "stpd_candidate_lineage_authoritative_product_mismatch"
  )
})

test_that("normalized raw routes and frozen provider tuples fail closed", {
  adapter <- data.frame(
    registry_version = "stpd_source_registry_v1",
    registry_source = "external", provider_id = "route_detector",
    raw_candidate_layer = "candidate", raw_candidate_class = "cluster",
    raw_final_label = "burst", raw_candidate_source = "route_v1",
    source_candidate_class = "burst", source_final_label = "burst",
    candidate_class = "burst", semantic_track = "event",
    family_id = "burst", stringsAsFactors = FALSE, check.names = FALSE
  )
  raw_fields <- c(
    "raw_candidate_layer", "raw_candidate_class", "raw_final_label",
    "raw_candidate_source"
  )
  forbidden_routes <- c(
    "profile_v2", "train.profile", "profile ", "custom_profile",
    "profile_candidate", "composite.state", "composite/state"
  )
  route_results <- unlist(lapply(raw_fields, function(field) {
    vapply(forbidden_routes, function(value) {
      attack <- adapter
      attack[[field]] <- value
      inherits(
        lineage_error(stpd_candidate_lineage_validate_source_adapters(attack)),
        "stpd_candidate_lineage_source_adapter_forbidden_route"
      )
    }, logical(1))
  }))
  expect_true(all(route_results))

  other_results <- unlist(lapply(raw_fields, function(field) {
    vapply(c("others_v2", "other "), function(value) {
      attack <- adapter
      attack[[field]] <- value
      inherits(
        lineage_error(stpd_candidate_lineage_validate_source_adapters(attack)),
        "stpd_candidate_lineage_source_adapter_invalid"
      )
    }, logical(1))
  }))
  expect_true(all(other_results))

  profile_node <- lineage_node(
    "route_ds", "route_train", "profile_attack",
    raw_candidate_class = "profile_v2"
  )
  profile_node$terminal_decision <- "rejected"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(profile_node)),
    "stpd_candidate_lineage_profile_node_forbidden"
  )

  registry <- stpd_candidate_lineage_builtin_source_registry()
  provider_rows <- registry[
    registry$provider_id %in% c("mean_isi", "logisi_newbd") &
      registry$raw_candidate_layer == "candidate" &
      registry$raw_candidate_class == "burst" &
      registry$raw_final_label == "burst", , drop = FALSE
  ]
  provider_rows <- provider_rows[order(
    provider_rows$provider_id, method = "radix"
  ), , drop = FALSE]
  expect_identical(
    provider_rows$provider_id,
    sort(c("mean_isi", "logisi_newbd"), method = "radix")
  )
  expect_identical(provider_rows$raw_candidate_source,
                   provider_rows$provider_id)
  expect_false(any(registry$provider_id == "log_isi"))
  expect_false(any(registry$provider_id == "manual"))

  manual_route_results <- unlist(lapply(raw_fields, function(field) {
    vapply(c("manual_examples", "manual_calibration"), function(value) {
      attack <- adapter
      attack[[field]] <- value
      inherits(
        lineage_error(stpd_candidate_lineage_validate_source_adapters(attack)),
        "stpd_candidate_lineage_source_adapter_forbidden_route"
      )
    }, logical(1))
  }))
  expect_true(all(manual_route_results))
  manual_provider <- adapter
  manual_provider$provider_id <- "manual"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(
      manual_provider
    )),
    "stpd_candidate_lineage_source_adapter_forbidden_route"
  )
  manual_node <- lineage_node(
    "manual_ds", "manual_train", "manual_metadata",
    raw_candidate_source = "manual.calibration.v2"
  )
  manual_node$terminal_decision <- "rejected"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(manual_node)),
    "stpd_candidate_lineage_profile_node_forbidden"
  )
  native_review <- registry[
    registry$provider_id == "native_stpd" &
      registry$raw_candidate_layer == "review_adjudication" &
      registry$raw_candidate_class == "possible_burst" &
      registry$raw_final_label == "burst" &
      registry$raw_candidate_source == "legacy_phase2b_exact_migration", ,
    drop = FALSE
  ]
  expect_equal(nrow(native_review), 1L)
  expect_identical(native_review$semantic_track, "event")

  direct_adapter <- data.frame(
    registry_version = "stpd_source_registry_v1",
    registry_source = "external", provider_id = "external_data_import",
    raw_candidate_layer = "imported_interval",
    raw_candidate_class = "burst_interval", raw_final_label = "burst",
    raw_candidate_source = "import_batch_1",
    source_candidate_class = "burst", source_final_label = "burst",
    candidate_class = "burst", semantic_track = "event",
    family_id = "burst", stringsAsFactors = FALSE, check.names = FALSE
  )
  direct_node <- lineage_node(
    "import_ds", "import_train", "imported_burst",
    provider_id = "external_data_import",
    raw_candidate_layer = "imported_interval",
    raw_candidate_class = "burst_interval", raw_final_label = "burst",
    raw_candidate_source = "import_batch_1"
  )
  direct_node$terminal_decision <- "rejected"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(direct_node)),
    "stpd_candidate_lineage_adapter_mapping_missing"
  )
  expect_true(stpd_candidate_lineage_validate_nodes(
    direct_node, direct_adapter
  ))
})

test_that("source labels and external adapters cannot silently cross family", {
  adapter <- data.frame(
    registry_version = "stpd_source_registry_v1",
    registry_source = "external", provider_id = "family_detector",
    raw_candidate_layer = "candidate", raw_candidate_class = "hf",
    raw_final_label = "hf", raw_candidate_source = "family_v1",
    source_candidate_class = "hft",
    source_final_label = "high_frequency_tonic",
    candidate_class = "tonic", semantic_track = "state",
    family_id = "tonic", stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(adapter)),
    "stpd_candidate_lineage_source_adapter_cross_family"
  )
  irregular <- adapter
  irregular$source_candidate_class <- "hf_irregular"
  irregular$source_final_label <- "high_frequency_irregular_state"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(irregular)),
    "stpd_candidate_lineage_source_adapter_cross_family"
  )
  reject_event <- adapter
  reject_event$source_candidate_class <- "burst"
  reject_event$source_final_label <- "reject"
  reject_event$candidate_class <- "burst"
  reject_event$semantic_track <- "event"
  reject_event$family_id <- "burst"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(
      reject_event
    )),
    "stpd_candidate_lineage_source_adapter_label_track_invalid"
  )
  possible_event <- reject_event
  possible_event$source_final_label <- "possible_burst"
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_source_adapters(
      possible_event
    )),
    "stpd_candidate_lineage_source_adapter_label_track_invalid"
  )
})

test_that("node geometry, support audit, and product ownership are closed", {
  base <- lineage_node("audit_ds", "audit_train", "audit_node")
  base$terminal_decision <- "rejected"

  disjoint <- base
  disjoint$original_start_isi <- 50L
  disjoint$original_end_isi <- 60L
  disjoint$candidate_node_id <- lineage_rehash_node(disjoint)
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(disjoint)),
    "stpd_candidate_lineage_geometry_invalid"
  )

  over_span <- base
  over_span$direct_isi_count <- 5L
  over_span$direct_fraction <- 1.25
  over_span$candidate_node_id <- lineage_rehash_node(over_span)
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(over_span)),
    "stpd_candidate_lineage_support_audit_invalid"
  )

  fraction_mismatch <- base
  fraction_mismatch$direct_isi_count <- 3L
  fraction_mismatch$candidate_node_id <- lineage_rehash_node(
    fraction_mismatch
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(
      fraction_mismatch
    )),
    "stpd_candidate_lineage_support_audit_invalid"
  )

  ghost_id <- base
  ghost_id$final_product_id <- "ghost_event"
  ghost_id$candidate_node_id <- lineage_rehash_node(ghost_id)
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(ghost_id)),
    "stpd_candidate_lineage_final_product_invalid"
  )
  ghost_type <- base
  ghost_type$final_product_type <- "Event"
  ghost_type$candidate_node_id <- lineage_rehash_node(ghost_type)
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_nodes(ghost_type)),
    "stpd_candidate_lineage_final_product_invalid"
  )
})

test_that("scientific attachment is named, non-destructive, and single-use", {
  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  authoritative <- lineage_authoritative_products(full)
  invalid_results <- list(
    list(TRUE), setNames(list(TRUE), ""),
    setNames(list(TRUE, FALSE), c("duplicate", "duplicate")),
    data.frame(scientific = TRUE)
  )
  expect_true(all(vapply(invalid_results, function(scientific) inherits(
    lineage_error(stpd_candidate_lineage_attach(
      scientific, full, authoritative, fixture$cap_policies
    )), "stpd_candidate_lineage_scientific_result_invalid"
  ), logical(1))))

  already <- list(scientific = TRUE, candidate_lineage_audit = list())
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_attach(
      already, full, authoritative, fixture$cap_policies
    )),
    "stpd_candidate_lineage_audit_already_attached"
  )
  scientific <- list(
    scientific = TRUE,
    nested = list(values = data.frame(x = 1:2, y = c("a", "b")))
  )
  attached <- stpd_candidate_lineage_attach(
    scientific, full, authoritative, fixture$cap_policies
  )
  expect_identical(
    stpd_candidate_lineage_scientific_projection(attached), scientific
  )
})

test_that("transforming terminals and cap manifests bind causal scope", {
  fixtures <- list(
    base = list(bundle = lineage_build(lineage_fixture()),
                policies = lineage_fixture()$cap_policies,
                relations = c("merge", "split", "supersede")),
    transform = list(
      bundle = lineage_build(lineage_transform_fixture()),
      policies = stpd_candidate_lineage_empty_cap_policies(),
      relations = c("trim", "retype")
    )
  )
  closure_results <- unlist(lapply(fixtures, function(case) {
    vapply(case$relations, function(relation) {
      attack <- case$bundle
      rows <- attack$candidate_lineage_edges$relation == relation
      attack$candidate_lineage_edges$relation[rows] <- "derive"
      attack$candidate_lineage_edges$reason_code[rows] <- "continues"
      inherits(
        lineage_error(stpd_candidate_lineage_validate_bundle(
          attack, "full", case$policies
        )), "stpd_candidate_lineage_terminal_edge_missing"
      )
    }, logical(1))
  }))
  expect_true(all(closure_results))

  fixture <- lineage_fixture()
  full <- lineage_build(fixture)
  foreign_policy <- fixture$cap_policies
  foreign_policy$train_id <- "foreign_train"
  foreign_policy$stage_scope_id <- stpd_candidate_lineage_cap_scope_id(
    foreign_policy$run_id, foreign_policy$params_hash,
    foreign_policy$dataset_id, foreign_policy$train_id,
    foreign_policy$group_id, foreign_policy$fold_id,
    foreign_policy$analysis_block_id, foreign_policy$family_id,
    foreign_policy$semantic_track, foreign_policy$stage,
    foreign_policy$policy_id
  )
  attack <- full
  cap_rows <- which(attack$candidate_stage_events$cap_scope_member)
  attack$candidate_stage_events$stage_scope_id[cap_rows] <-
    foreign_policy$stage_scope_id
  attack$candidate_stage_events$stage_event_id[cap_rows] <- vapply(
    cap_rows, function(row) lineage_rehash_event(
      attack$candidate_stage_events[row, , drop = FALSE]
    ), character(1)
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_bundle(
      attack, "full", foreign_policy
    )),
    "stpd_candidate_lineage_cap_policy_log_mismatch"
  )
})

test_that("cross-family retype establishes target entry and State holes fail", {
  make_truth <- function(node, truth_id, start_isi, end_isi) data.frame(
    truth_id = truth_id, run_id = node$run_id,
    params_hash = node$params_hash, dataset_id = node$dataset_id,
    train_id = node$train_id, group_id = node$group_id,
    fold_id = node$fold_id, analysis_block_id = node$analysis_block_id,
    family_id = node$family_id, truth_start_isi = as.integer(start_isi),
    truth_end_isi = as.integer(end_isi), stringsAsFactors = FALSE,
    check.names = FALSE
  )

  terminated_fixture <- lineage_transform_fixture()
  terminated <- lineage_build(terminated_fixture)
  tonic_child <- terminated$candidate_nodes[
    terminated$candidate_nodes$source_candidate_id == "retype_child", ,
    drop = FALSE
  ]
  terminated_truth <- make_truth(
    tonic_child, "truth_retype_terminated", 20L, 25L
  )
  terminated_result <- stpd_candidate_lineage_truth_node_first_failure(
    terminated_truth, terminated,
    stpd_candidate_lineage_empty_cap_policies()
  )
  expect_identical(
    terminated_result$truth_first_failure$entry_stage,
    "burst_pause_ownership_resolved"
  )
  expect_identical(
    terminated_result$truth_first_failure$truth_first_failure_stage,
    "post_size_isi_validated"
  )
  burst_parent_id <- terminated$candidate_nodes$candidate_node_id[
    terminated$candidate_nodes$source_candidate_id == "retype_parent"
  ]
  expect_false(burst_parent_id %in%
                 terminated_result$truth_node_evidence$candidate_node_id)

  parent <- lineage_node(
    "retype_success_ds", "retype_success_train", "burst_parent",
    start_isi = 40L, end_isi = 45L
  )
  child <- lineage_node(
    "retype_success_ds", "retype_success_train", "tonic_child",
    family_id = "tonic", semantic_track = "state",
    candidate_class = "tonic", node_origin = "derived",
    created_stage = "burst_pause_ownership_resolved",
    start_isi = 40L, end_isi = 45L
  )
  final <- lineage_node(
    "retype_success_ds", "retype_success_train", "tonic_final",
    family_id = "tonic", semantic_track = "state",
    candidate_class = "tonic", node_role = "final_product",
    created_stage = "multitrack_materialized",
    start_isi = 40L, end_isi = 45L,
    final_product_type = "State", final_product_id = "state_retyped"
  )
  success_fixture <- list(
    nodes = lineage_bind(list(parent, child, final), "candidate_nodes"),
    edges = lineage_bind(list(
      lineage_edge(
        parent, child, "retype", "ignored", 1L, "retyped_into_child"
      ),
      lineage_edge(
        child, final, "supersede", "ignored", 1L,
        "superseded_by_child"
      )
    ), "candidate_lineage_edges"),
    events = lineage_bind(list(
      lineage_events(
        parent, "burst_pause_ownership_resolved", "superseded",
        "retyped_into_child"
      ),
      lineage_events(
        child, "multitrack_materialized", "superseded",
        "superseded_by_child"
      ),
      lineage_events(
        final, "multitrack_materialized", "materialized", "finalized",
        final_product_id = "state_retyped"
      )
    ), "candidate_stage_events"),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  success <- lineage_build(success_fixture)
  success_truth <- make_truth(final, "truth_retype_success", 40L, 45L)
  success_result <- stpd_candidate_lineage_truth_node_first_failure(
    success_truth, success, stpd_candidate_lineage_empty_cap_policies()
  )$truth_first_failure
  expect_identical(success_result$entry_stage,
                   "burst_pause_ownership_resolved")
  expect_true(is.na(success_result$truth_first_failure_stage))

  duplicate_truth <- rbind(
    terminated_truth,
    transform(success_truth, truth_id = "truth_retype_terminated")
  )
  expect_s3_class(
    lineage_error(stpd_candidate_lineage_validate_truth(duplicate_truth)),
    "stpd_candidate_lineage_truth_value_invalid"
  )

  state_fixture <- lineage_state_fixture()
  state <- lineage_build(state_fixture)
  state_final <- state$candidate_nodes[
    state$candidate_nodes$node_role == "final_product", , drop = FALSE
  ]
  hole_truth <- make_truth(state_final, "truth_state_hole", 3L, 3L)
  hole_result <- stpd_candidate_lineage_truth_node_first_failure(
    hole_truth, state, stpd_candidate_lineage_empty_cap_policies()
  )$truth_first_failure
  expect_identical(hole_result$truth_first_failure_stage,
                   "multitrack_materialized")
  expect_identical(hole_result$truth_first_failure_reason,
                   "geometry_support_lost")
})

test_that("later native truth entry can recover an early weak-root failure", {
  make_truth <- function(node, truth_id) data.frame(
    truth_id = truth_id, run_id = node$run_id,
    params_hash = node$params_hash, dataset_id = node$dataset_id,
    train_id = node$train_id, group_id = node$group_id,
    fold_id = node$fold_id, analysis_block_id = node$analysis_block_id,
    family_id = node$family_id,
    truth_start_isi = node$current_start_isi,
    truth_end_isi = node$current_end_isi,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  weak <- lineage_node(
    "recovery_ds", "recovery_train", "weak_stage1",
    start_isi = 1L, end_isi = 4L
  )
  later <- lineage_node(
    "recovery_ds", "recovery_train", "later_native",
    node_origin = "root", created_stage = "structure_candidate_proposed",
    start_isi = 1L, end_isi = 4L
  )
  final <- lineage_node(
    "recovery_ds", "recovery_train", "later_final",
    node_role = "final_product",
    created_stage = "multitrack_materialized",
    start_isi = 1L, end_isi = 4L,
    final_product_type = "Event", final_product_id = "event_recovered"
  )
  success_fixture <- list(
    nodes = lineage_bind(list(weak, later, final), "candidate_nodes"),
    edges = lineage_edge(
      later, final, "supersede", "ignored", 1L,
      "superseded_by_child"
    ),
    events = lineage_bind(list(
      lineage_events(
        weak, "raw_threshold_support_proposed", "rejected",
        "threshold_failed"
      ),
      lineage_events(
        later, "multitrack_materialized", "superseded",
        "superseded_by_child"
      ),
      lineage_events(
        final, "multitrack_materialized", "materialized", "finalized",
        final_product_id = "event_recovered"
      )
    ), "candidate_stage_events"),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  success <- lineage_build(success_fixture)
  success_result <- stpd_candidate_lineage_truth_node_first_failure(
    make_truth(weak, "truth_recovered"), success,
    stpd_candidate_lineage_empty_cap_policies()
  )
  expect_identical(success_result$truth_first_failure$entry_stage,
                   "raw_threshold_support_proposed")
  expect_true(is.na(
    success_result$truth_first_failure$truth_first_failure_stage
  ))
  expected_evidence <- success$candidate_nodes$candidate_node_id[
    success$candidate_nodes$source_candidate_id %in%
      c("weak_stage1", "later_native", "later_final")
  ]
  expect_setequal(
    success_result$truth_node_evidence$candidate_node_id,
    expected_evidence
  )

  weak_failed <- lineage_node(
    "recovery_fail_ds", "recovery_fail_train", "weak_stage1",
    start_isi = 10L, end_isi = 13L
  )
  later_failed <- lineage_node(
    "recovery_fail_ds", "recovery_fail_train", "later_native",
    node_origin = "root", created_stage = "structure_candidate_proposed",
    start_isi = 10L, end_isi = 13L
  )
  failed_fixture <- list(
    nodes = lineage_bind(
      list(weak_failed, later_failed), "candidate_nodes"
    ),
    edges = stpd_candidate_lineage_empty_table("candidate_lineage_edges"),
    events = lineage_bind(list(
      lineage_events(
        weak_failed, "raw_threshold_support_proposed", "rejected",
        "threshold_failed"
      ),
      lineage_events(
        later_failed, "post_size_isi_validated", "rejected",
        "post_size_isi_failed"
      )
    ), "candidate_stage_events"),
    cap_policies = stpd_candidate_lineage_empty_cap_policies()
  )
  failed <- lineage_build(failed_fixture)
  failed_result <- stpd_candidate_lineage_truth_node_first_failure(
    make_truth(weak_failed, "truth_recovery_failed"), failed,
    stpd_candidate_lineage_empty_cap_policies()
  )$truth_first_failure
  expect_identical(failed_result$entry_stage,
                   "raw_threshold_support_proposed")
  expect_identical(failed_result$truth_first_failure_stage,
                   "post_size_isi_validated")
  expect_identical(failed_result$truth_first_failure_reason,
                   "post_size_isi_failed")
})

test_that("manifest hashes are invariant to equivalent row ordering", {
  node_a <- lineage_node("hash_a", "hash_train_a", "hash_a")
  node_b <- lineage_node("hash_b", "hash_train_b", "hash_b")
  policies <- rbind(
    lineage_cap_policy(
      node_a, "structure_candidate_proposed", 2L, "hash_policy_a"
    ),
    lineage_cap_policy(
      node_b, "bridge_or_boundary_expanded", 3L, "hash_policy_b"
    )
  )
  cap_hash <- function(x) stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-cap-manifest-v1",
    stpd_candidate_lineage_canonical_cap_manifest(x)
  )
  expect_identical(cap_hash(policies), cap_hash(policies[2:1, , drop = FALSE]))

  first <- lineage_authoritative_products(lineage_build(lineage_fixture()))
  second <- lineage_authoritative_products(
    lineage_build(lineage_state_fixture())
  )
  products <- rbind(first, second)
  product_hash <- function(x) stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-product-manifest-v1",
    stpd_candidate_lineage_canonical_product_manifest(x)
  )
  expect_identical(
    product_hash(products), product_hash(products[2:1, , drop = FALSE])
  )

  adapter <- data.frame(
    registry_version = rep("stpd_source_registry_v1", 2L),
    registry_source = rep("external", 2L),
    provider_id = c("hash_adapter_a", "hash_adapter_b"),
    raw_candidate_layer = rep("candidate", 2L),
    raw_candidate_class = c("cluster_a", "cluster_b"),
    raw_final_label = rep("burst", 2L),
    raw_candidate_source = c("source_a", "source_b"),
    source_candidate_class = rep("burst", 2L),
    source_final_label = rep("burst", 2L),
    candidate_class = rep("burst", 2L),
    semantic_track = rep("event", 2L), family_id = rep("burst", 2L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  source_hash <- function(x) stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-source-registry-v1",
    stpd_candidate_lineage_source_registry(x)
  )
  expect_identical(
    source_hash(adapter), source_hash(adapter[2:1, , drop = FALSE])
  )
})
