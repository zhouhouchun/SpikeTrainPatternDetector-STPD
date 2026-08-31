# Gate B v3.2 semantic overlay
#
# This file deliberately leaves the frozen v3.1 schema, registries, fixtures,
# hashes, and validators unchanged.  It supplies the revised scientific
# compatibility semantics for products whose structural representation is
# still carried by the v3.1 canonical tables.

#' Return the Gate B v3.2 semantic-overlay identifier
#' @return A scalar character identifier.
stpd_gate_b_v3_2_schema_version <- function() {
  "stpd_multitrack_v3_2_semantic_overlay"
}

#' Return the Gate B v3.2 cross-track compatibility matrix
#'
#' Burst is an Event and HFI/HFT/Tonic are States.  Their supports may overlap
#' without either track deleting or re-detecting the other.  Canonical Pause is
#' excluded from State direct support, while the surrounding State episode may
#' retain a typed pause-interrupted relationship.
#' @return A deterministic compatibility data frame.
stpd_gate_b_v3_2_compatibility_matrix <- function() {
  out <- stpd_gate_b_v3_compatibility_matrix()

  event_state <- out$candidate_domain == "event" &
    out$candidate_class == "burst_event" &
    out$overlap_domain == "state" &
    out$overlap_class %in% c(
      "high_frequency_irregular_state", "high_frequency_tonic", "tonic"
    )
  out$action[event_state] <- "coexist_non_destructive"
  out$lineage_required[event_state] <- FALSE
  out$hard_for_event[event_state] <- FALSE
  out$hard_for_state[event_state] <- FALSE

  state_pause <- out$candidate_domain == "state" &
    out$overlap_domain == "gap" &
    out$overlap_class == "canonical_pause"
  out$action[state_pause] <-
    "exclude_gap_from_direct_support_preserve_episode"
  out$lineage_required[state_pause] <- TRUE
  out$hard_for_event[state_pause] <- TRUE
  out$hard_for_state[state_pause] <- TRUE

  state_boundary <- out$candidate_domain == "state" &
    out$overlap_domain == "boundary"
  out$action[state_boundary] <- "split_state_support_without_redetection"
  out$lineage_required[state_boundary] <- TRUE

  out <- out[do.call(order, c(out[1:4], list(method = "radix"))), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Resolve one conflict under the Gate B v3.2 semantic overlay
#' @return A unique action and lineage requirement.
stpd_gate_b_v3_2_compatibility_action <- function(
    candidate_domain, candidate_class, overlap_domain, overlap_class) {
  key <- paste(candidate_domain, candidate_class, overlap_domain,
               overlap_class, sep = "|")
  matrix <- stpd_gate_b_v3_2_compatibility_matrix()
  matrix_key <- do.call(paste, c(matrix[c(
    "candidate_domain", "candidate_class", "overlap_domain", "overlap_class"
  )], sep = "|"))
  hit <- which(matrix_key == key)
  if (length(hit) != 1L) {
    stop("No Gate B v3.2 compatibility action exists for this tuple.",
         call. = FALSE)
  }
  list(
    action = matrix$action[[hit]],
    lineage_required = matrix$lineage_required[[hit]],
    row_order_independent = TRUE,
    semantic_overlay = stpd_gate_b_v3_2_schema_version()
  )
}

stpd_gate_b_v3_2_validate_relationship_science <- function(tables) {
  observed <- tables$event_state_relationships
  expected <- stpd_gate_b_v3_expected_event_state_relationships(tables)
  expected_key <- if (is.null(expected)) character() else paste(
    expected$event_id, expected$state_episode_id, sep = "\u001f")
  observed_key <- if (nrow(observed) == 0L) character() else paste(
    observed$event_id, observed$state_episode_id, sep = "\u001f")
  if (!setequal(expected_key, observed_key) || anyDuplicated(observed_key)) {
    stop("Gate B v3.2 Event-State relationship expected set is incomplete.",
         call. = FALSE)
  }
  if (!is.null(expected)) {
    numeric_fields <- setdiff(names(expected), c("event_id", "state_episode_id"))
    expected <- expected[match(observed_key, expected_key), , drop = FALSE]
    for (field in numeric_fields) {
      if (!identical(unname(observed[[field]]), unname(expected[[field]]))) {
        stop(
          "Gate B v3.2 Event-State geometry differs from the canonical spine: ",
          field, call. = FALSE
        )
      }
    }
  }

  if (nrow(observed) > 0L) {
    event_class <- tables$events$event_class[
      match(observed$event_id, tables$events$event_id)]
    state_class <- tables$state_episodes$state_class[
      match(observed$state_episode_id, tables$state_episodes$state_episode_id)]
    supported <- event_class == "burst_event" & state_class %in% c(
      "high_frequency_irregular_state", "high_frequency_tonic", "tonic"
    )
    if (any(!supported)) {
      stop("Gate B v3.2 contains an unsupported Event-State relationship.",
           call. = FALSE)
    }
  }

  # Non-destructive coexistence is a product invariant.  A downstream stage
  # must not use Burst to split/re-detect an accepted State candidate.
  if (nrow(tables$event_interrupted_state_relationships) != 0L) {
    stop("Gate B v3.2 forbids Burst-triggered State interruption or re-detection.",
         call. = FALSE)
  }

  links <- tables$state_episode_links
  if (nrow(links) > 0L) {
    pre_class <- tables$state_episodes$state_class[
      match(links$pre_state_episode_id, tables$state_episodes$state_episode_id)]
    post_class <- tables$state_episodes$state_class[
      match(links$post_state_episode_id, tables$state_episodes$state_episode_id)]
    broad_hfs <- c("high_frequency_irregular_state", "high_frequency_tonic")
    if (any(!pre_class %in% broad_hfs | !post_class %in% broad_hfs)) {
      stop(
        "Gate B v3.2 pause-interrupted Broad HFS links require HFI/HFT on both sides.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Validate a v3.1-structured product using Gate B v3.2 science semantics
#'
#' The structural and provenance contract remains frozen at v3.1.  Only the
#' explicitly versioned cross-track scientific semantics are replaced.
#' @return Invisible TRUE; otherwise fails closed.
stpd_gate_b_v3_2_validate_product_prototype <- function(
    tables, bundle = stpd_gate_b_v3_phase1_bundle()) {
  .stpd_gate_b_v3_validate_product_prototype_structural(tables, bundle)
  stpd_gate_b_v3_validate_state_axis_science(tables)
  stpd_gate_b_v3_validate_projection_science(tables)
  stpd_gate_b_v3_2_validate_relationship_science(tables)
  stpd_gate_b_v3_validate_gap_science(tables)
  stpd_gate_b_v3_validate_patient_holdout(tables)
  stpd_gate_b_v3_validate_threshold_evidence_science(tables)
  stpd_gate_b_v3_validate_structure_first_event_contract(tables)
  stpd_gate_b_v3_validate_threshold_binding_completeness(tables)
  stpd_gate_b_v3_validate_entity_evidence_science(tables)
  invisible(TRUE)
}
