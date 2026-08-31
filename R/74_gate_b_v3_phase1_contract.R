# Gate B v3 phase-1 executable contract prototypes.
#
# This file intentionally does not materialize detector output and cannot grant
# authority.  It turns the frozen normative Markdown contract into executable
# schema, registry, fixture and safety prototypes for phase-1 review.

stpd_gate_b_v3_phase1_bundle_path <- function() {
  installed <- system.file(
    "config", "gate_b_v3_phase1_schema_bundle.json",
    package = "SpikeTrainPatternDetector"
  )
  if (nzchar(installed)) return(installed)
  file.path("inst", "config", "gate_b_v3_phase1_schema_bundle.json")
}

stpd_gate_b_v3_phase1_runtime_cache <- new.env(parent = emptyenv())

#' Load the executable Gate B v3 phase-1 schema bundle
#' @param path Optional explicit bundle path.
#' @param verify_frozen Whether to verify the frozen plan and contract hashes
#'   when their source files are available.
#' @return A non-authoritative schema-bundle list.
#' @export
stpd_gate_b_v3_phase1_bundle <- function(
    path = NULL, verify_frozen = TRUE) {
  path <- path %||% stpd_gate_b_v3_phase1_bundle_path()
  if (!file.exists(path)) {
    stop("Gate B v3 phase-1 schema bundle is missing.", call. = FALSE)
  }
  bundle <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(bundle$bundle_schema,
                 "stpd_gate_b_v3_phase1_schema_bundle_1")) {
    stop("Gate B v3 phase-1 schema bundle has an unknown schema.", call. = FALSE)
  }
  embedded_plan_hash <- if (is.character(bundle$frozen_sources$plan_utf8) &&
      length(bundle$frozen_sources$plan_utf8) == 1L) {
    digest::digest(bundle$frozen_sources$plan_utf8, algo = "sha256",
                   serialize = FALSE)
  } else NA_character_
  embedded_contract_hash <- if (
      is.character(bundle$frozen_sources$normative_contract_utf8) &&
      length(bundle$frozen_sources$normative_contract_utf8) == 1L) {
    digest::digest(bundle$frozen_sources$normative_contract_utf8,
                   algo = "sha256", serialize = FALSE)
  } else NA_character_
  if (!identical(embedded_plan_hash, bundle$plan_sha256) ||
      !identical(embedded_contract_hash, bundle$normative_contract_sha256)) {
    stop("Embedded frozen Gate B v3 sources do not match their hashes.",
         call. = FALSE)
  }
  if (!identical(length(bundle$required_contract_ids), 27L) ||
      anyDuplicated(unlist(bundle$required_contract_ids, use.names = FALSE))) {
    stop("Gate B v3 required contract-ID set is incomplete or duplicated.",
         call. = FALSE)
  }
  constants <- c(
    schema_version = "stpd_multitrack_v3_1",
    status_schema = "stpd_multitrack_v3_status_1",
    identity_schema = "stpd_multitrack_v3_identity_1",
    manifest_schema = "stpd_multitrack_v3_manifest_1",
    artifact_manifest_schema = "stpd_multitrack_v3_artifact_manifest_1",
    response_schema = "stpd_multitrack_v3_response_1",
    evidence_record_schema = "stpd_gate_b_v3_evidence_record_1",
    reference_schema = "stpd_multitrack_v3_reference_1",
    migration_schema = "stpd_multitrack_v3_migration_1",
    approval_schema = "stpd_gate_b_v3_approval_1",
    signature_schema = "stpd_gate_b_v3_signature_1",
    test_bundle_schema = "stpd_gate_b_v3_test_bundle_1",
    release_attestation_schema = "stpd_gate_b_v3_release_attestation_1"
  )
  actual_constants <- unlist(bundle$schema_constants[names(constants)],
                             use.names = TRUE)
  if (!identical(actual_constants, constants)) {
    stop("Gate B v3 fixed schema constants do not match the normative contract.",
         call. = FALSE)
  }
  inventory <- names(bundle$product_inventory)
  missing <- setdiff(inventory, names(bundle$tables))
  if (length(missing) > 0L) {
    stop("Schema bundle is missing inventory tables: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  incomplete <- names(bundle$tables)[!vapply(bundle$tables, function(spec) {
    length(spec$columns) > 0L && length(spec$primary_key) > 0L &&
      length(spec$unique_keys) > 0L && length(spec$sort_key) > 0L &&
      !is.null(spec$table_schema) && !is.null(spec$row_constraints) &&
      length(spec$id_resolution) >= 0L &&
      all(vapply(spec$columns, function(field) {
        all(c("name", "type", "nullable", "enum", "default_policy",
              "constant") %in% names(field))
      }, logical(1)))
  }, logical(1))]
  if (length(incomplete) > 0L) {
    stop("Inventory table constraints are incomplete: ",
         paste(incomplete, collapse = ", "), call. = FALSE)
  }
  generating <- identical(Sys.getenv("STPD_GATE_B_PHASE1_GENERATING"), "1")
  if (length(bundle$id_payload_registry) < 25L ||
      length(bundle$status_matrix) != 9L ||
      length(bundle$hash_dag$nodes) < 20L ||
      length(bundle$hash_dag$edges) < 20L ||
      (!generating && length(bundle$registry_semantics) < 1L)) {
    stop("Gate B v3 schema bundle lacks ID, status, or hash-DAG closure.",
         call. = FALSE)
  }
  if (!generating) {
    claimed_schema_hash <- bundle$schema_contract_sha256
    bundle_without_hash <- bundle
    bundle_without_hash$schema_contract_sha256 <- NULL
    actual_schema_hash <- stpd_gate_b_v3_hash_raw(
      "stpd-gate-b-v3-schema-contract-v1",
      stpd_gate_b_v3_canonical_json(bundle_without_hash)
    )
    if (!identical(claimed_schema_hash, actual_schema_hash)) {
      stop("Gate B v3 schema-contract content hash is invalid.", call.=FALSE)
    }
  }
  if (isTRUE(verify_frozen)) {
    roots <- c(".", normalizePath(file.path(dirname(path), "..", "..", ".."),
                                   mustWork = FALSE))
    plan_rel <- file.path("docs", "gate-b-v3-implementation-and-acceptance-plan.md")
    contract_rel <- file.path(
      "docs", "gate-b-v3-normative-data-identity-release-contract.md"
    )
    for (root in unique(roots)) {
      plan <- file.path(root, plan_rel)
      contract <- file.path(root, contract_rel)
      if (file.exists(plan) && file.exists(contract)) {
        actual_plan <- digest::digest(plan, algo = "sha256", file = TRUE)
        actual_contract <- digest::digest(contract, algo = "sha256", file = TRUE)
        if (!identical(actual_plan, bundle$plan_sha256) ||
            !identical(actual_contract, bundle$normative_contract_sha256)) {
          stop("Frozen Gate B v3 plan/contract bytes do not match the bundle.",
               call. = FALSE)
        }
        break
      }
    }
  }
  bundle
}

#' Lint the complete Gate B v3 phase-1 schema bundle
#' @param bundle Optional preloaded bundle.
#' @return An invisible `TRUE`; otherwise fails closed.
#' @export
stpd_gate_b_v3_phase1_schema_lint <- function(
    bundle = stpd_gate_b_v3_phase1_bundle()) {
  table_names <- names(bundle$tables)
  if (length(table_names) != 59L || anyDuplicated(table_names)) {
    stop("Gate B v3 table inventory is not exact.", call. = FALSE)
  }
  known_constraints <- c(
    "state_candidate_decision_domain_v1","event_candidate_decision_domain_v1",
    "geometry_count_duration_closure_v1","segment_role_conditional_fk_v1",
    "episode_partition_v1","per_isi_role_closure_v1",
    "event_modifier_evidence_cardinality_v1",
    "canonical_pause_hard_boundary_v1","threshold_permission_matrix_v1",
    "threshold_value_xor_v1","threshold_scope_key_v1",
    "threshold_policy_instance_match_v1","product_status_matrix_v1",
    "failure_reason_presence_v1","attestation_presence_v1",
    "reference_blinding_v1","event_state_expected_set_closure_v1",
    "episode_link_expected_set_closure_v1",
    "detection_root_source_context_v1",
    "state_axis_tuple_v1","event_candidate_class_v1",
    "event_modifier_domain_value_v1","episode_link_truth_v1",
    "label_blind_manifest_action_v1"
  )
  for (table_name in table_names) {
    spec <- bundle$tables[[table_name]]
    expected_table_schema <- paste0("stpd_multitrack_v3_",table_name,"_1")
    if (!identical(spec$table_schema,expected_table_schema) ||
        !identical(spec$canonical_envelope_schema,"stpd_canonical_table_v1")) {
      stop("Gate B v3 per-table schema/envelope identity is invalid: ",
           table_name,call.=FALSE)
    }
    if (is.null(spec$row_constraints) ||
        any(!unlist(spec$row_constraints,use.names=FALSE) %in% known_constraints)) {
      stop("Gate B v3 row-constraint registry is incomplete: ",table_name,
           call.=FALSE)
    }
    fields <- vapply(spec$columns, `[[`, character(1), "name")
    if (anyDuplicated(fields) || any(!nzchar(fields))) {
      stop("Gate B v3 table has duplicate/empty field names: ", table_name,
           call. = FALSE)
    }
    keys <- c(unlist(spec$primary_key, use.names = FALSE),
              unlist(spec$sort_key, use.names = FALSE),
              unlist(spec$unique_keys, recursive = TRUE, use.names = FALSE))
    if (any(!keys %in% fields)) {
      stop("Gate B v3 key references an unknown field: ", table_name,
           call. = FALSE)
    }
    for (field in spec$columns) {
      if (!identical(field$enum, "none") &&
          is.null(bundle$enums[[field$enum]])) {
        stop("Gate B v3 field references an unknown enum: ", field$name,
             call. = FALSE)
      }
      expected_default <- if (isTRUE(field$nullable)) {
        paste0("typed_na_",field$type)
      } else "no_implicit_default"
      if (!identical(field$default_policy,expected_default)) {
        stop("Gate B v3 field default policy is not exact: ",
             table_name,".",field$name,call.=FALSE)
      }
    }
    for (foreign_key in spec$foreign_keys) {
      target <- bundle$tables[[foreign_key$target_table]]
      if (is.null(target)) stop("Gate B v3 FK target table is unknown.",
                                call. = FALSE)
      target_fields <- vapply(target$columns, `[[`, character(1), "name")
      if (any(!unlist(foreign_key$columns) %in% fields) ||
          any(!unlist(foreign_key$target_columns) %in% target_fields)) {
        stop("Gate B v3 FK columns are unresolved.", call. = FALSE)
      }
    }
    id_fields <- fields[grepl("_id$", fields)]
    if (!setequal(id_fields, names(spec$id_resolution))) {
      stop("Gate B v3 ID/FK domain resolution is incomplete: ", table_name,
           call. = FALSE)
    }
  }
  prefixes <- vapply(bundle$id_payload_registry, `[[`, character(1), "prefix")
  if (anyDuplicated(prefixes) || any(!grepl("^[a-z]{2}_$", prefixes))) {
    stop("Gate B v3 entity-ID prefixes are ambiguous.", call. = FALSE)
  }
  matrix_key <- vapply(bundle$status_matrix, function(row) paste(
    row$product_kind, row$product_status, row$deployment_context, sep = "|"
  ), character(1))
  if (length(matrix_key) != 9L || anyDuplicated(matrix_key)) {
    stop("Gate B v3 product-status matrix is incomplete or ambiguous.",
         call. = FALSE)
  }
  stpd_gate_b_v3_phase1_hash_dag_lint(bundle)
  invisible(TRUE)
}

stpd_gate_b_v3_empty_column <- function(type) {
  switch(
    type,
    chr = character(), int = integer(), dbl = double(), lgl = logical(),
    json = character(), lst = I(vector("list", 0L)),
    stop("Unknown Gate B v3 field type: ", type, call. = FALSE)
  )
}

#' Construct an exact empty Gate B v3 table prototype
#' @param table_name A table in the executable schema bundle.
#' @param bundle Optional preloaded schema bundle.
#' @return A zero-row data frame with exact columns and storage types.
#' @export
stpd_gate_b_v3_empty_table <- function(
    table_name, bundle = stpd_gate_b_v3_phase1_bundle()) {
  if (length(table_name) != 1L || is.na(table_name) ||
      !table_name %in% names(bundle$tables)) {
    stop("Unknown Gate B v3 table name.", call. = FALSE)
  }
  fields <- bundle$tables[[table_name]]$columns
  columns <- lapply(fields, function(field) {
    stpd_gate_b_v3_empty_column(field$type)
  })
  names(columns) <- vapply(fields, `[[`, character(1), "name")
  out <- as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  attr(out, "stpd_gate_b_v3_table_name") <- table_name
  out
}

stpd_gate_b_v3_rows_from_json <- function(rows, prototype) {
  if (length(rows) == 0L) return(prototype)
  expected <- names(prototype)
  if (!all(vapply(rows, function(row) identical(names(row), expected),
                  logical(1)))) {
    stop("Materialized Gate B v3 rows do not match the exact column order.",
         call. = FALSE)
  }
  columns <- lapply(seq_along(expected), function(j) {
    values <- lapply(rows, `[[`, j)
    template <- prototype[[j]]
    if (is.list(template)) return(I(values))
    if (is.integer(template)) return(as.integer(unlist(values)))
    if (is.double(template)) return(as.double(unlist(values)))
    if (is.logical(template)) return(as.logical(unlist(values)))
    as.character(unlist(values))
  })
  names(columns) <- expected
  out <- as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

stpd_gate_b_v3_column_type_ok <- function(x, type) {
  switch(
    type,
    chr = is.character(x), int = is.integer(x),
    dbl = is.double(x), lgl = is.logical(x),
    json = is.character(x), lst = is.list(x), FALSE
  )
}

stpd_gate_b_v3_utf16_sort_key <- function(x) {
  x <- stpd_gate_b_v3_strict_nfc(x)
  vapply(x, function(value) {
    bytes <- iconv(value, from = "UTF-8", to = "UTF-16BE", toRaw = TRUE)[[1L]]
    paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

stpd_gate_b_v3_strict_nfc <- function(x) {
  if (!is.character(x)) stop("UTF-8/NFC input must be character.", call.=FALSE)
  valid <- iconv(x, from="UTF-8", to="UTF-8", sub=NA_character_)
  if (anyNA(valid) && any(!is.na(x))) {
    stop("Invalid UTF-8 is forbidden in Gate B v3 canonical data.", call.=FALSE)
  }
  normalized <- stringi::stri_trans_nfc(valid)
  if (any(!is.na(x) & x != normalized)) {
    stop("Gate B v3 strings must already be NFC-normalized.", call.=FALSE)
  }
  normalized
}

stpd_gate_b_v3_normalize_nfc <- function(x) {
  if (!is.character(x)) stop("UTF-8 input must be character.",call.=FALSE)
  valid <- iconv(x,from="UTF-8",to="UTF-8",sub=NA_character_)
  if (anyNA(valid) && any(!is.na(x))) {
    stop("Invalid UTF-8 is forbidden in Gate B v3 canonical data.",call.=FALSE)
  }
  stringi::stri_trans_nfc(valid)
}

stpd_gate_b_v3_is_negative_zero <- function(x) {
  is.double(x) & !is.na(x) & x == 0 & is.infinite(1 / x) & (1 / x) < 0
}

stpd_gate_b_v3_is_exact_utc <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(FALSE)
  }
  match <- regexec(
    "^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})[.]([0-9]{6})Z$",
    value, perl = TRUE
  )
  fields <- regmatches(value, match)[[1L]]
  if (length(fields) != 8L) return(FALSE)
  part <- suppressWarnings(as.integer(fields[2:8]))
  if (anyNA(part)) return(FALSE)
  year <- part[[1L]]
  month <- part[[2L]]
  day <- part[[3L]]
  hour <- part[[4L]]
  minute <- part[[5L]]
  second <- part[[6L]]
  if (year < 1L || month < 1L || month > 12L || hour > 23L || minute > 59L ||
      second > 59L) return(FALSE)
  leap <- (year %% 4L == 0L) &&
    ((year %% 100L != 0L) || (year %% 400L == 0L))
  month_days <- c(31L, if (leap) 29L else 28L, 31L, 30L, 31L, 30L,
                  31L, 31L, 30L, 31L, 30L, 31L)
  day >= 1L && day <= month_days[[month]]
}

stpd_gate_b_v3_utf8_sort_key <- function(x) {
  x <- stpd_gate_b_v3_strict_nfc(x)
  vapply(x, function(value) {
    paste(sprintf("%02x", as.integer(charToRaw(value))), collapse="")
  }, character(1), USE.NAMES=FALSE)
}

stpd_gate_b_v3_sort_index <- function(x, sort_key) {
  if (nrow(x) < 2L) return(seq_len(nrow(x)))
  keys <- lapply(x[sort_key], function(value) {
    if (is.character(value)) {
      out <- stpd_gate_b_v3_utf8_sort_key(ifelse(is.na(value), "", value))
      paste0(ifelse(is.na(value), "0:", "1:"), out)
    } else if (is.logical(value)) {
      ifelse(is.na(value), -1L, as.integer(value))
    } else if (is.numeric(value)) {
      ifelse(is.na(value), -Inf, as.double(value))
    } else {
      vapply(value, stpd_gate_b_v3_canonical_json, character(1))
    }
  })
  do.call(order, c(keys, list(na.last = FALSE, method = "radix")))
}

#' Validate one table against the Gate B v3 phase-1 prototype
#' @param x A data frame.
#' @param table_name The exact table schema to apply.
#' @param bundle Optional preloaded schema bundle.
#' @return `TRUE`; otherwise fails closed with a typed message.
#' @export
stpd_gate_b_v3_validate_table_prototype <- function(
    x, table_name, bundle = stpd_gate_b_v3_phase1_bundle(), tables = NULL,
    require_sorted = TRUE) {
  if (!is.data.frame(x)) stop("Gate B v3 table must be a data frame.", call. = FALSE)
  spec <- bundle$tables[[table_name]]
  if (is.null(spec)) stop("Unknown Gate B v3 table name.", call. = FALSE)
  expected <- vapply(spec$columns, `[[`, character(1), "name")
  if (!identical(names(x), expected)) {
    stop("Gate B v3 table columns or order do not match the frozen prototype.",
         call. = FALSE)
  }
  for (i in seq_along(spec$columns)) {
    field <- spec$columns[[i]]
    value <- x[[i]]
    list_column_wrapper <- identical(field$type, "lst") &&
      identical(attributes(value), list(class = "AsIs"))
    if (length(attributes(value)) > 0L && !list_column_wrapper) {
      stop("Gate B v3 canonical columns may not carry R attributes/classes: ",
           field$name, call.=FALSE)
    }
    if (!stpd_gate_b_v3_column_type_ok(value, field$type)) {
      stop("Gate B v3 field type mismatch: ", field$name, call. = FALSE)
    }
    if (!isTRUE(field$nullable)) {
      if (is.list(value)) {
        if (any(vapply(value, is.null, logical(1)))) {
          stop("Non-nullable Gate B v3 list field contains NULL: ", field$name,
               call. = FALSE)
        }
      } else if (anyNA(value)) {
        stop("Non-nullable Gate B v3 field contains NA: ", field$name,
             call. = FALSE)
      }
    }
    if (is.double(value) && any(!is.finite(value[!is.na(value)]))) {
      stop("Gate B v3 double fields must be finite.", call. = FALSE)
    }
    if (is.double(value) && any(stpd_gate_b_v3_is_negative_zero(value))) {
      stop("Gate B v3 canonical double fields forbid negative zero: ",
           field$name, call. = FALSE)
    }
    if (is.character(value) && length(value) > 0L) {
      stpd_gate_b_v3_strict_nfc(value[!is.na(value)])
      observed_text <- value[!is.na(value)]
      if ((grepl("(_sha256|_hash)$", field$name) ||
           field$name %in% c("product_hash", "parent_auto_product_hash",
                             "source_product_hash", "child_product_hash",
                             "parent_product_hash")) &&
          any(!grepl("^[0-9a-f]{64}$", observed_text))) {
        stop("Gate B v3 SHA-256/hash field is not lowercase 64-hex: ",
             field$name, call.=FALSE)
      }
      if (grepl("_utc$", field$name) &&
          any(!vapply(observed_text, stpd_gate_b_v3_is_exact_utc,
                      logical(1)))) {
        stop("Gate B v3 UTC field is not exact RFC3339 microsecond UTC: ",
             field$name, call.=FALSE)
      }
      if (field$name %in% c("repository_relative_path", "artifact_path") &&
          any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)",
                    observed_text, perl=TRUE))) {
        stop("Gate B v3 paths must be root-relative POSIX paths: ",
             field$name, call.=FALSE)
      }
    }
    enum_name <- field$enum %||% "none"
    if (!identical(enum_name, "none")) {
      allowed <- unlist(bundle$enums[[enum_name]], use.names = FALSE)
      observed <- unique(value[!is.na(value)])
      if (length(allowed) == 0L || any(!observed %in% allowed)) {
        stop("Gate B v3 field has an unknown enum value: ", field$name,
             call. = FALSE)
      }
    }
    constant <- field$constant
    if (!is.null(constant) && length(constant) == 1L && !is.na(constant) &&
        any(value[!is.na(value)] != constant)) {
      stop("Gate B v3 fixed schema constant mismatch: ", field$name,
           call. = FALSE)
    }
    if (identical(field$type, "json") && length(value) > 0L) {
      for (json in value[!is.na(value)]) {
        parsed <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE),
                           error = function(e) e)
        if (inherits(parsed, "error") ||
            !identical(stpd_gate_b_v3_canonical_json(parsed), json)) {
          stop("Gate B v3 json fields must contain canonical JCS text: ",
               field$name, call. = FALSE)
        }
      }
    }
  }
  if (identical(table_name, "signature_envelope") && nrow(x) > 0L &&
      any(x$not_before_utc >= x$not_after_utc)) {
    stop("Gate B v3 signature validity requires not_before_utc < not_after_utc.",
         call. = FALSE)
  }
  for (key in spec$unique_keys) {
    key <- unlist(key, use.names = FALSE)
    if (!all(key %in% names(x))) {
      stop("Gate B v3 unique key references a missing column.", call. = FALSE)
    }
    canonical_key <- if (nrow(x) == 0L) character() else vapply(
      seq_len(nrow(x)), function(row_index) stpd_gate_b_v3_canonical_json(
        lapply(key, function(name) {
          value <- x[[name]][row_index]
          if (length(value) == 1L && is.na(value)) NULL else value
        })
      ), character(1)
    )
    if (anyDuplicated(canonical_key)) {
      stop("Gate B v3 PK/unique constraint is duplicated.", call. = FALSE)
    }
  }
  primary <- unlist(spec$primary_key, use.names = FALSE)
  primary_nullable <- setNames(vapply(spec$columns, function(field) {
    isTRUE(field$nullable)
  }, logical(1)), vapply(spec$columns, `[[`, character(1), "name"))
  forbidden_null <- primary[!primary_nullable[primary]]
  if (nrow(x) > 0L && length(forbidden_null) > 0L &&
      anyNA(x[forbidden_null])) {
    stop("Gate B v3 primary key has a forbidden null value.", call. = FALSE)
  }
  if ("train" %in% names(x) && any(!is.na(x$train) &
      !grepl("^tr_[0-9a-f]{64}$", x$train))) {
    stop("Gate B v3 train values must be stable tr_ pseudonyms.", call.=FALSE)
  }
  for (field in names(spec$id_resolution)) {
    resolution <- spec$id_resolution[[field]]
    values <- x[[field]]
    observed <- values[!is.na(values)]
    if (length(observed) == 0L) next
    if (resolution$resolution %in% c("generated_entity",
                                     "generated_special_contract_id")) {
      prefix <- resolution$prefix %||% ""
      if (any(!grepl(paste0("^", prefix, "[0-9a-f]{64}$"), observed))) {
        stop("Gate B v3 generated ID has the wrong prefix/length: ", field,
             call.=FALSE)
      }
    }
    if (identical(resolution$resolution,"external_audit_identifier") &&
        any(!grepl(resolution$pattern,observed))) {
      stop("Gate B v3 external identifier violates its frozen format: ", field,
           call.=FALSE)
    }
    if (identical(resolution$resolution,"signed_toolchain_identifier") &&
        any(!grepl(resolution$pattern,observed))) {
      stop("Gate B v3 toolchain identifier violates its signed format.",
           call.=FALSE)
    }
    if (identical(resolution$resolution,"generated_entity") &&
        field %in% primary && !is.null(bundle$id_payload_registry[[resolution$domain]])) {
      payload_fields <- unlist(bundle$id_payload_registry[[resolution$domain]]$fields,
                               use.names=FALSE)
      if (all(payload_fields %in% names(x))) {
        expected_ids <- vapply(seq_len(nrow(x)), function(i) {
          payload <- lapply(payload_fields, function(name) x[[name]][i])
          names(payload) <- payload_fields
          stpd_gate_b_v3_entity_id(resolution$domain,payload,bundle)
        }, character(1))
        if (!identical(as.character(values),expected_ids)) {
          stop("Gate B v3 generated entity ID does not match its exact payload: ",
               field, call.=FALSE)
        }
      }
    }
  }
  reference_id_specs <- list(
    reference_annotations=list(id="annotation_id",prefix="ra_",
      domain="stpd-reference-annotation-v1",exclude=c("annotation_id","created_utc")),
    reference_adjudications=list(id="adjudication_id",prefix="rj_",
      domain="stpd-reference-adjudication-v1",exclude=c("adjudication_id","created_utc")),
    reference_display_protocols=list(id="display_protocol_id",prefix="rd_",
      domain="stpd-reference-display-id-v1",
      exclude=c("display_protocol_id","protocol_payload_sha256")),
    reference_project_manifest=list(id="annotation_project_id",prefix="rp_",
      domain="stpd-reference-project-v1",
      exclude=c("annotation_project_id","project_payload_sha256"))
  )
  reference_spec <- reference_id_specs[[table_name]]
  if (!is.null(reference_spec) && nrow(x)>0L) {
    expected_ids <- vapply(seq_len(nrow(x)),function(i) {
      payload_names <- setdiff(names(x),reference_spec$exclude)
      payload <- lapply(payload_names,function(name) {
        value <- if (is.list(x[[name]])) x[[name]][[i]] else x[[name]][[i]]
        if (length(value)==1L && is.na(value)) NULL else value
      })
      names(payload) <- payload_names
      paste0(reference_spec$prefix,stpd_gate_b_v3_hash_raw(
        reference_spec$domain,stpd_gate_b_v3_canonical_json(payload)))
    },character(1))
    if (!identical(unname(x[[reference_spec$id]]),unname(expected_ids))) {
      stop("Gate B v3 reference content-addressed ID is invalid.",call.=FALSE)
    }
    if (identical(table_name,"reference_display_protocols")) {
      expected_payload_hash <- sub("^rd_","",expected_ids)
      expected_payload_hash <- vapply(seq_len(nrow(x)),function(i) {
        payload_names <- setdiff(names(x),reference_spec$exclude)
        payload <- lapply(payload_names,function(name) x[[name]][[i]])
        names(payload)<-payload_names
        stpd_gate_b_v3_hash_raw("stpd-reference-display-payload-v1",
          stpd_gate_b_v3_canonical_json(payload))
      },character(1))
      if (!identical(unname(x$protocol_payload_sha256),
                     unname(expected_payload_hash))) {
        stop("Gate B v3 reference display payload hash is invalid.",call.=FALSE)
      }
    }
    if (identical(table_name,"reference_project_manifest")) {
      expected_payload_hash <- vapply(seq_len(nrow(x)),function(i) {
        payload_names <- setdiff(names(x),reference_spec$exclude)
        payload <- lapply(payload_names,function(name) x[[name]][[i]])
        names(payload)<-payload_names
        stpd_gate_b_v3_hash_raw("stpd-reference-project-payload-v1",
          stpd_gate_b_v3_canonical_json(payload))
      },character(1))
      if (!identical(unname(x$project_payload_sha256),
                     unname(expected_payload_hash))) {
        stop("Gate B v3 reference project payload hash is invalid.",call.=FALSE)
      }
    }
  }
  if (identical(table_name,"reference_annotations") && nrow(x)>0L) {
    domain_by_pass <- list(
      `0`=c("qc_interval"),
      `1`=c("state_direct_support","reference_pause","burst_event"),
      `2`=c("connector_edge","pause_interrupted_compound_link"))
    valid <- vapply(seq_len(nrow(x)),function(i)
      x$annotation_domain[[i]] %in%
        (domain_by_pass[[as.character(x$annotation_pass[[i]])]] %||% character()),
      logical(1))
    if (!all(valid)) stop("Reference annotation pass/domain is illegal.",call.=FALSE)
  }
  if (identical(table_name,"reference_display_protocols") && nrow(x)>0L &&
      any(x$threshold_overlay_visible | x$algorithm_overlay_visible)) {
    stop("Validation display protocols must hide algorithm/threshold overlays.",
         call.=FALSE)
  }
  if (identical(table_name,"partition_memberships") && nrow(x)>0L) {
    partitions <- split(seq_len(nrow(x)),x$partition_id)
    for (indices in partitions) {
      versions <- unique(x$partition_version[indices])
      if (length(versions)!=1L) stop("Partition version is inconsistent.",call.=FALSE)
      row_hash <- vapply(indices,function(i) {
        payload <- list(group_id_hash=x$group_id_hash[i],group_role=x$group_role[i],
          patient_group_id_hash=if(is.na(x$patient_group_id_hash[i]))NULL else x$patient_group_id_hash[i],
          session_group_id_hash=x$session_group_id_hash[i],
          source_input_hash=x$source_input_hash[i])
        stpd_gate_b_v3_hash_raw("stpd-partition-membership-row-v1",
                                stpd_gate_b_v3_canonical_json(payload))
      },character(1))
      if (!identical(unname(x$membership_row_hash[indices]),unname(row_hash))) {
        stop("Partition membership row hash is invalid.",call.=FALSE)
      }
      order_index <- order(stpd_gate_b_v3_utf8_sort_key(x$group_id_hash[indices]),
                           method="radix")
      rows <- lapply(indices[order_index],function(i) list(
        schema_version=x$schema_version[i],partition_version=x$partition_version[i],
        group_id_hash=x$group_id_hash[i],group_role=x$group_role[i],
        patient_group_id_hash=if(is.na(x$patient_group_id_hash[i]))NULL else x$patient_group_id_hash[i],
        session_group_id_hash=x$session_group_id_hash[i],
        source_input_hash=x$source_input_hash[i],membership_row_hash=x$membership_row_hash[i]))
      expected <- stpd_gate_b_v3_entity_id("partition",list(
        partition_version=versions[[1L]],
        sorted_membership_rows_without_partition_id=rows),bundle)
      if (any(x$partition_id[indices]!=expected)) {
        stop("Partition ID does not match its exact sorted membership payload.",
             call.=FALSE)
      }
    }
  }
  if (identical(table_name,"lineage_records") && nrow(x)>0L) {
    groups <- split(seq_len(nrow(x)),x$lineage_id)
    for (indices in groups) {
      invariant <- c("detection_root_id","child_domain_id","child_id",
                     "child_product_hash","parent_edge_set_hash")
      if (any(vapply(invariant,function(name)
          length(unique(x[[name]][indices]))!=1L,logical(1)))) {
        stop("Lineage group invariants are inconsistent.",call.=FALSE)
      }
      payloads <- lapply(indices,function(i) list(
        parent_domain_id=x$parent_domain_id[i],parent_id=x$parent_id[i],
        parent_product_hash=if(is.na(x$parent_product_hash[i]))NULL else x$parent_product_hash[i],
        lineage_action_registry_id=x$lineage_action_registry_id[i],
        transition_id=if(is.na(x$transition_id[i]))NULL else x$transition_id[i],
        evidence_id=x$evidence_id[i]))
      payload_json <- vapply(payloads,stpd_gate_b_v3_canonical_json,character(1))
      if (anyDuplicated(payload_json)) stop("Duplicate lineage parent edge.",call.=FALSE)
      order_index <- order(stpd_gate_b_v3_utf8_sort_key(payload_json),method="radix")
      payloads <- payloads[order_index]
      expected_set_hash <- stpd_gate_b_v3_hash_raw(
        "stpd-lineage-parent-edge-set-v1",stpd_gate_b_v3_canonical_json(list(
          child_domain_id=x$child_domain_id[indices[1L]],
          child_id=x$child_id[indices[1L]],
          child_product_hash=if(is.na(x$child_product_hash[indices[1L]]))NULL else
            x$child_product_hash[indices[1L]],sorted_parent_edge_payloads=payloads)))
      expected_id <- stpd_gate_b_v3_entity_id("lineage",list(
        detection_root_id=x$detection_root_id[indices[1L]],
        child_domain_id=x$child_domain_id[indices[1L]],
        child_id=x$child_id[indices[1L]],
        parent_edge_set_hash=expected_set_hash),bundle)
      if (any(x$parent_edge_set_hash[indices]!=expected_set_hash) ||
          any(x$lineage_id[indices]!=expected_id) ||
          !identical(sort(x$edge_index[indices]),seq_along(indices))) {
        stop("Lineage ID/edge-set/edge-index closure is invalid.",call.=FALSE)
      }
    }
  }
  if (identical(table_name, "contract_registries") && nrow(x) > 0L) {
    expected_semantic_hash <- vapply(
      x$semantic_definition_json,
      function(json) stpd_gate_b_v3_hash_raw("stpd-registry-semantic-v1", json),
      character(1)
    )
    expected_registry_id <- vapply(seq_len(nrow(x)), function(i) {
      payload <- stpd_gate_b_v3_canonical_json(list(
        registry_domain = x$registry_domain[[i]],
        code = x$code[[i]],
        code_version = x$code_version[[i]],
        semantic_definition_sha256 = expected_semantic_hash[[i]]
      ))
      paste0("reg_", stpd_gate_b_v3_hash_raw(
        "stpd-registry-entry-v1", payload
      ))
    }, character(1))
    if (!identical(unname(x$semantic_definition_sha256),
                   unname(expected_semantic_hash)) ||
        !identical(unname(x$registry_entry_id),
                   unname(expected_registry_id))) {
      stop("Gate B v3 registry content-addressed identity is invalid.",
           call. = FALSE)
    }
  }
  if (identical(table_name,"evidence_records") && nrow(x)>0L &&
      !is.null(tables$contract_registries)) {
    registry <- tables$contract_registries
    schema_index <- match(x$evidence_schema_registry_id,
                          registry$registry_entry_id)
    creator_index <- match(x$created_by_registry_entry_id,
                           registry$registry_entry_id)
    if (anyNA(schema_index) || anyNA(creator_index)) {
      stop("Gate B v3 evidence registry references are unresolved.",call.=FALSE)
    }
    for (i in seq_len(nrow(x))) {
      semantic <- jsonlite::fromJSON(
        registry$semantic_definition_json[[schema_index[[i]]]],
        simplifyVector=FALSE)
      canonical_payload <- tryCatch(jsonlite::fromJSON(
        x$evidence_json[[i]],simplifyVector=FALSE),error=function(e) NULL)
      if (is.null(canonical_payload) || is.null(names(canonical_payload)) ||
          !identical(stpd_gate_b_v3_canonical_json(canonical_payload),
                     x$evidence_json[[i]])) {
        stop("Gate B v3 evidence JSON is not one canonical named object.",
             call.=FALSE)
      }
      field_spec <- unlist(
        semantic$formula_or_state_machine$exact_ordered_fields,use.names=FALSE)
      field_name <- sub(":.*$","",field_spec)
      field_type <- sub("^[^:]+:","",field_spec)
      if (!setequal(names(canonical_payload),field_name) ||
          length(canonical_payload)!=length(field_name)) {
        stop("Gate B v3 evidence payload keys do not match its exact schema.",
             call.=FALSE)
      }
      for (j in seq_along(field_name)) {
        value <- canonical_payload[[field_name[[j]]]]
        nullable <- grepl("\\?$",field_type[[j]])
        type <- sub("[!?]$","",field_type[[j]])
        if (is.null(value)) {
          if (!nullable) stop("Non-null evidence field is null.",call.=FALSE)
          next
        }
        ok <- switch(type,chr=is.character(value)&&length(value)==1L,
          dbl=is.numeric(value)&&length(value)==1L&&is.finite(value),
          int=is.integer(value)&&length(value)==1L,
          lgl=is.logical(value)&&length(value)==1L,
          lst=is.list(value)&&is.null(names(value)),
          json=is.list(value),FALSE)
        if (!isTRUE(ok)) stop("Gate B v3 evidence payload field type is invalid.",
                              call.=FALSE)
      }
      expected_json_hash <- stpd_gate_b_v3_hash_raw(
        "stpd-evidence-json-v1",stpd_gate_b_v3_canonical_json(list(
          evidence_schema_registry_id=x$evidence_schema_registry_id[[i]],
          evidence_json=canonical_payload)))
      if (x$evidence_json_hash[[i]]!=expected_json_hash) {
        stop("Gate B v3 evidence JSON hash is invalid.",call.=FALSE)
      }
      allowed <- unlist(
        semantic$formula_or_state_machine$compatible_creator_registry_domains,
        use.names=FALSE)
      if (length(allowed)==0L ||
          !registry$registry_domain[[creator_index[[i]]]] %in% allowed) {
        stop("Gate B v3 evidence creator is incompatible with its evidence schema.",
             call.=FALSE)
      }
    }
  }
  sort_key <- unlist(spec$sort_key, use.names = FALSE)
  primary_key <- unlist(spec$primary_key,use.names=FALSE)
  # The registry contract freezes the scientific sort key and an exact-PK
  # tie-break.  The tie-break is observable for standalone/malformed tables
  # containing more than one detection root, even where a valid product makes
  # the scientific key unique by construction.
  canonical_sort_key <- unique(c(sort_key,primary_key))
  if (isTRUE(require_sorted) && nrow(x) > 1L &&
      !identical(stpd_gate_b_v3_sort_index(x, canonical_sort_key),
                 seq_len(nrow(x)))) {
    stop("Gate B v3 table rows do not follow the canonical sort key.",
         call. = FALSE)
  }
  if (!is.null(tables)) {
    for (foreign_key in spec$foreign_keys) {
      columns <- unlist(foreign_key$columns, use.names = FALSE)
      target_columns <- unlist(foreign_key$target_columns, use.names = FALSE)
      target <- tables[[foreign_key$target_table]]
      if (is.null(target)) {
        stop("Gate B v3 FK target table is unavailable.", call. = FALSE)
      }
      source_values <- x[columns]
      nonnull_n <- if (nrow(x) == 0L) integer() else
        Reduce(`+`, lapply(source_values, function(z) as.integer(!is.na(z))))
      if (any(nonnull_n > 0L & nonnull_n < length(columns))) {
        stop("Gate B v3 composite foreign key is only partially populated.",
             call.=FALSE)
      }
      present <- nonnull_n == length(columns)
      if (any(present)) {
        source_key <- do.call(paste, c(source_values[present, , drop = FALSE], sep = "\u001f"))
        target_key <- do.call(paste, c(target[target_columns], sep = "\u001f"))
        if (any(!source_key %in% target_key)) {
          stop("Gate B v3 foreign key is orphaned.", call. = FALSE)
        }
        expected_domain <- foreign_key$expected_registry_domain %||% "none"
        if (identical(foreign_key$target_table, "contract_registries") &&
            !identical(expected_domain, "none")) {
          matched <- match(source_values[[1L]][present], target$registry_entry_id)
          domain_ok <- if (identical(expected_domain,"creator_domain_allowlist")) {
            target$registry_domain[matched] %in%
              unlist(foreign_key$allowed_registry_domains,use.names=FALSE)
          } else target$registry_domain[matched] == expected_domain
          if (any(!domain_ok)) {
            stop("Gate B v3 registry FK resolves to the wrong domain.",
                 call. = FALSE)
          }
          positive_tables <- c(
            "states", "state_segments", "events", "event_modifier_evidence",
            "gaps", "event_state_relationships", "state_episode_links",
            "threshold_instances", "threshold_instance_bindings"
          )
          positive_candidate <-
            identical(table_name, "state_candidates") &&
              any(x$candidate_decision[present] == "accepted_direct_support") ||
            identical(table_name, "event_candidates") &&
              any(x$candidate_decision[present] == "accepted_event")
          if ((table_name %in% positive_tables || positive_candidate) &&
              any(target$code[matched] == "not_asserted_v1")) {
            stop("A positive Gate B v3 scientific row may not reference a typed unknown registry sentinel.",
                 call. = FALSE)
          }
        }
      }
    }
    domain_pairs <- list(
      c("entity_domain_id","entity_id"), c("consumer_domain_id","consumer_id"),
      c("target_domain_id","target_id"), c("child_domain_id","child_id"),
      c("parent_domain_id","parent_id"),
      c("source_entity_domain_id","source_entity_id"),
      c("target_entity_domain_id","target_entity_id")
    )
    registry_table <- tables$entity_domain_registry
    for (pair in domain_pairs) {
      if (!all(pair %in% names(x))) next
      domain_value <- x[[pair[[1L]]]]
      entity_value <- x[[pair[[2L]]]]
      if (any(xor(is.na(domain_value),is.na(entity_value)))) {
        stop("Gate B v3 polymorphic domain/entity IDs must be null as a pair.",
             call.=FALSE)
      }
      present_pair <- !is.na(domain_value)
      if (any(present_pair)) {
        if (is.null(registry_table)) {
          stop("Gate B v3 entity-domain registry is unavailable.",call.=FALSE)
        }
        matched_domain <- match(domain_value[present_pair],
                                registry_table$entity_domain_id)
        if (anyNA(matched_domain) ||
            any(!registry_table$active[matched_domain])) {
          stop("Gate B v3 polymorphic entity domain is unknown/inactive.",
               call.=FALSE)
        }
        source_rows <- which(present_pair)
        for (j in seq_along(source_rows)) {
          source_row <- source_rows[[j]]
          domain_row <- matched_domain[[j]]
          target_name <- registry_table$target_table[[domain_row]]
          target <- tables[[target_name]]
          if (is.null(target)) {
            stop("Gate B v3 polymorphic target table is unavailable.",
                 call.=FALSE)
          }
          pk <- tryCatch(jsonlite::fromJSON(
            registry_table$primary_key_columns_json[[domain_row]],
            simplifyVector=TRUE),error=function(e) character())
          pk <- as.character(unlist(pk,use.names=FALSE))
          if (length(pk)!=1L || !pk %in% names(target) ||
              !entity_value[[source_row]] %in% target[[pk]]) {
            stop("Gate B v3 polymorphic entity ID is absent from its resolved target.",
                 call.=FALSE)
          }
          train_column <- registry_table$train_column[[domain_row]]
          if (!is.na(train_column) && nzchar(train_column) &&
              "train" %in% names(x) && train_column %in% names(target)) {
            hit <- which(target[[pk]]==entity_value[[source_row]])
            if (length(hit)!=1L ||
                !identical(x$train[[source_row]],target[[train_column]][[hit]])) {
              stop("Gate B v3 polymorphic entity resolves across train scope.",
                   call.=FALSE)
            }
          }
        }
      }
    }
  }
  if (identical(table_name,"threshold_policies") && nrow(x)>0L) {
    legal <- list(
      c("fixed_preregistered","none","none","partition_na"),
      c("development_trained","none","development_reference_only","partition_required"),
      c("development_trained","session","development_reference_only","partition_required"),
      c("per_train_unsupervised","train","none","partition_na"),
      c("batch_transductive","dataset_batch","none","partition_na")
    )
    for (i in seq_len(nrow(x))) {
      partition_state <- if (is.na(x$partition_id[i])) "partition_na" else
        "partition_required"
      tuple <- c(x$threshold_source_class[i],x$adaptation_unit[i],
                 x$label_access[i],partition_state)
      if (!any(vapply(legal,identical,logical(1),tuple))) {
        stop("Gate B v3 threshold policy violates the exact permission matrix.",
             call.=FALSE)
      }
    }
  }
  if (identical(table_name,"label_blind_contract_manifest") && nrow(x)>0L) {
    registry <- if (!is.null(tables) && !is.null(tables$contract_registries)) {
      tables$contract_registries
    } else stpd_gate_b_v3_phase1_registry(FALSE)
    expected <- data.frame(
      rule=c("fresh_reconstruction_v1","strip_label_review_sources_v1",
             "strip_derived_state_sources_v1","forbid_unknown_input_sources_v1"),
      allowlist=c("timestamp_acquisition_qc_allowlist_v1",
        "prohibited_label_review_sources_v1",
        "prohibited_derived_state_sources_v1","unknown_source_rejection_v1"),
      source=c("normalized_timestamp_and_acquisition_qc_v1",
        "label_and_review_sources_v1","derived_state_sources_v1",
        "unknown_input_source_v1"),
      action=c("allow","strip","strip","forbid"),stringsAsFactors=FALSE)
    resolve <- function(domain,code) {
      hit <- which(registry$registry_domain==domain & registry$code==code &
                     registry$active)
      if (length(hit)!=1L) stop("Gate B v3 label-blind registry closure is incomplete.",
                                call.=FALSE)
      hit
    }
    expected_rows <- lapply(seq_len(nrow(expected)),function(i) {
      rule <- resolve("label_blind_rule",expected$rule[[i]])
      allowlist <- resolve("input_allowlist_schema",expected$allowlist[[i]])
      source <- resolve("input_source_domain",expected$source[[i]])
      c(contract_rule_registry_id=registry$registry_entry_id[[rule]],
        allowlist_schema_registry_id=registry$registry_entry_id[[allowlist]],
        source_domain_registry_id=registry$registry_entry_id[[source]],
        action=expected$action[[i]],
        semantic_definition_sha256=registry$semantic_definition_sha256[[rule]])
    })
    observed <- apply(x,1L,function(row) paste(row,collapse="\u001f"))
    required_rows <- vapply(expected_rows,function(row)
      paste(row,collapse="\u001f"),character(1))
    if (!setequal(observed,required_rows) || nrow(x)!=length(required_rows)) {
      stop("Gate B v3 label-blind manifest expected-set/action closure is invalid.",
           call.=FALSE)
    }
  }
  constraints <- unlist(spec$row_constraints,use.names=FALSE)
  if ("detection_root_source_context_v1" %in% constraints && nrow(x)>0L) {
    expected_root <- paste0("dr_",x$source_context_hash)
    if (!identical(unname(x$detection_root_id),unname(expected_root))) {
      stop("Gate B v3 detection root is not bound to source context.",
           call.=FALSE)
    }
  }
  if ("geometry_count_duration_closure_v1" %in% constraints && nrow(x)>0L) {
    expected_duration <- x$end_time_sec-x$start_time_sec
    if (any(x$end_isi < x$start_isi) ||
        any(x$n_isi != x$end_isi-x$start_isi+1L) ||
        any(x$n_spikes != x$n_isi+1L) ||
        any(x$end_time_sec < x$start_time_sec) ||
        !identical(unname(x$duration_sec),unname(expected_duration))) {
      stop("Gate B v3 interval geometry/count/duration closure is invalid.",
           call.=FALSE)
    }
  }
  if (identical(table_name,"state_episodes") && nrow(x)>0L) {
    mapped <- mapply(function(frequency,regularity) {
      decision <- stpd_gate_b_v3_state_axis_decision(frequency,regularity)
      if (!identical(decision$decision,"accepted")) NA_character_ else
        decision$state_class
    },x$state_frequency_class,x$state_regularity_class,
    USE.NAMES=FALSE)
    if (anyNA(mapped) || !identical(unname(x$state_class),unname(mapped)) ||
        any(x$frequency_evidence_status!="pass") ||
        any(x$regularity_evidence_status!="pass")) {
      stop("Gate B v3 accepted State tuple is not a legal 3x3 axis mapping.",
           call.=FALSE)
    }
  }
  if (identical(table_name,"event_candidates") && nrow(x)>0L &&
      any(x$candidate_class!="burst_candidate")) {
    stop("Gate B v3 Event candidate class is not burst_candidate.",call.=FALSE)
  }
  if (identical(table_name,"state_segments") && nrow(x)>0L) {
    direct <- x$segment_role=="direct_support"
    connector <- x$segment_role=="tolerated_connector"
    if (any(direct & (is.na(x$source_state_candidate_id) |
                      is.na(x$state_axis_evidence_id) |
                      !is.na(x$connector_decision_id))) ||
        any(connector & (!is.na(x$source_state_candidate_id) |
                         !is.na(x$state_axis_evidence_id) |
                         is.na(x$connector_decision_id)))) {
      stop("Gate B v3 State segment role/FK conditional closure is invalid.",
           call.=FALSE)
    }
  }
  if (identical(table_name,"per_isi") && nrow(x)>0L) {
    connector <- x$state_segment_role=="tolerated_connector" &
      !is.na(x$state_segment_role)
    direct <- x$state_segment_role=="direct_support" &
      !is.na(x$state_segment_role)
    outside <- !x$state_episode_membership
    if (any(connector & (!is.na(x$active_state_segment_id) |
                         !is.na(x$active_state_class) |
                         x$state_direct_support)) ||
        any(direct & (is.na(x$active_state_segment_id) |
                      is.na(x$state_segment_id) |
                      x$active_state_segment_id != x$state_segment_id |
                      is.na(x$active_state_class) |
                      !x$state_direct_support)) ||
        any(outside & (!is.na(x$state_episode_id) |
                       !is.na(x$state_segment_id) |
                       !is.na(x$active_state_segment_id) |
                       x$state_direct_support))) {
      stop("Gate B v3 per-ISI episode/segment/active-role closure is invalid.",
           call.=FALSE)
    }
  }
  if (identical(table_name,"gaps") && nrow(x)>0L) {
    if (any(x$start_isi!=x$end_isi | x$n_isi!=1L | x$n_spikes!=2L |
            x$gap_evidence_status!="pass" | x$raw_p_value<0 |
            x$raw_p_value>1 | x$adjusted_q_value<0 | x$adjusted_q_value>1)) {
      stop("Gate B v3 canonical Pause geometry/evidence closure is invalid.",
           call.=FALSE)
    }
  }
  if (identical(table_name,"events") && nrow(x)>0L) {
    unresolved_extent <- x$extent_modifier=="unresolved"
    unresolved_frequency <- x$frequency_modifier=="unresolved"
    allowed_unresolved <- c("insufficient","gray_zone")
    if (any(unresolved_extent & !x$extent_evidence_status %in% allowed_unresolved) ||
        any(unresolved_frequency &
              !x$frequency_evidence_status %in% allowed_unresolved)) {
      stop("Gate B v3 Event modifier/evidence status closure is invalid.",
           call.=FALSE)
    }
  }
  if (identical(table_name,"event_modifier_evidence") && nrow(x)>0L) {
    extent <- x$modifier_domain=="extent"
    frequency <- x$modifier_domain=="frequency"
    if (any(extent & !x$modifier_value %in%
              c("classic","long","prolonged","unresolved")) ||
        any(frequency & !x$modifier_value %in%
              c("high","non_high","unresolved"))) {
      stop("Gate B v3 Event modifier value is outside its domain.",call.=FALSE)
    }
  }
  if (identical(table_name,"state_episode_links") && nrow(x)>0L &&
      any(x$biological_ground_truth)) {
    stop("Gate B v3 episode links are prediction records, not biological truth.",
         call.=FALSE)
  }
  if (identical(table_name,"event_state_relationships") && nrow(x)>0L) {
    fractions <- x[grepl("_fraction$",names(x))]
    if (any(vapply(fractions,function(v) any(v<0 | v>1),logical(1))) ||
        any(x$direct_support_overlap_n_isi+x$connector_overlap_n_isi !=
              x$episode_overlap_n_isi) ||
        !identical(unname(x$direct_support_overlap_sec+
                            x$connector_overlap_sec),
                   unname(x$episode_overlap_sec))) {
      stop("Gate B v3 Event-State overlap decomposition is invalid.",
           call.=FALSE)
    }
  }
  # Blinding flags are stored facts.  A row with a FALSE flag remains a legal
  # workflow annotation, but a separate reference-eligibility validator must
  # exclude it from an independent performance reference.
  if (identical(table_name,"threshold_instances") && nrow(x)>0L) {
    if (any((x$value_type=="double") !=
            (!is.na(x$value_num) & is.na(x$value_chr))) ||
        any((x$value_type=="string") !=
            (is.na(x$value_num) & !is.na(x$value_chr)))) {
      stop("Gate B v3 threshold value representation is inconsistent.",
           call.=FALSE)
    }
    scope_ok <- vapply(seq_len(nrow(x)),function(i) {
      scope <- x$application_scope[i]
      fields <- c(train=!is.na(x$train[i]),session=!is.na(x$session_group_id_hash[i]),
                  dataset_batch=!is.na(x$dataset_batch_id_hash[i]))
      identical(fields, switch(scope,
        global=c(train=FALSE,session=FALSE,dataset_batch=FALSE),
        train=c(train=TRUE,session=FALSE,dataset_batch=FALSE),
        session=c(train=FALSE,session=TRUE,dataset_batch=FALSE),
        dataset_batch=c(train=FALSE,session=FALSE,dataset_batch=TRUE),
        c(train=TRUE,session=TRUE,dataset_batch=TRUE)))
    },logical(1))
    if (!all(scope_ok)) stop("Gate B v3 threshold scope keys are inconsistent.",
                             call.=FALSE)
    if (!is.null(tables$threshold_policies)) {
      policy_index <- match(x$threshold_policy_id,
                            tables$threshold_policies$threshold_policy_id)
      if (anyNA(policy_index) ||
          any(x$path != tables$threshold_policies$path[policy_index]) ||
          any(x$units != tables$threshold_policies$units[policy_index])) {
        stop("Gate B v3 threshold instance does not match its policy.",
             call.=FALSE)
      }
      source <- tables$threshold_policies$threshold_source_class[policy_index]
      adaptation <- tables$threshold_policies$adaptation_unit[policy_index]
      legal_scope <- (source=="fixed_preregistered" & x$application_scope=="global") |
        (source=="development_trained" & adaptation=="none" & x$application_scope=="global") |
        (source=="development_trained" & adaptation=="session" & x$application_scope=="session") |
        (source=="per_train_unsupervised" & x$application_scope=="train") |
        (source=="batch_transductive" & x$application_scope=="dataset_batch")
      if (!all(legal_scope)) stop("Gate B v3 threshold application scope is illegal.",
                                  call.=FALSE)
    }
  }
  if (identical(table_name,"product_status_envelope") && nrow(x) > 0L) {
    for (i in seq_len(nrow(x))) {
      deployment <- if (identical(x$product_status[i],"gate_b_authoritative")) {
        if (identical(x$integrity_level[i],"externally_anchored_product"))
          "export" else "live"
      } else "any"
      candidates <- vapply(bundle$status_matrix,function(row) {
          (identical(row$product_kind,"any") ||
             identical(row$product_kind,x$product_kind[i])) &&
          identical(row$product_status,x$product_status[i]) &&
          identical(row$deployment_context,deployment) &&
          identical(row$gate_b,x$gate_b_status[i]) &&
          identical(row$gate_c,x$gate_c_status[i]) &&
          identical(row$detector_performance_eligible,
                    isTRUE(x$detector_performance_eligible[i])) &&
          identical(row$biological_ground_truth,
                    isTRUE(x$biological_ground_truth[i])) &&
          identical(row$authoritative,isTRUE(x$record_authoritative[i])) &&
          identical(row$authority,x$authority_scope[i]) &&
          (identical(row$integrity,x$integrity_level[i]) ||
             identical(row$integrity,"detached_or_parent_bound") &&
               x$integrity_level[i] %in% c("detached_internal_integrity",
                                           "parent_bound_recomputed"))
      },logical(1))
      if (sum(candidates) != 1L) {
        stop("Gate B v3 product-status combination is illegal.",call.=FALSE)
      }
      root_required <- bundle$status_matrix[[which(candidates)]]$root
      hash_required <- bundle$status_matrix[[which(candidates)]]$product_hash
      execution_required <- bundle$status_matrix[[which(candidates)]]$execution
      release_required <- bundle$status_matrix[[which(candidates)]]$release_attestation
      product_attestation_required <- bundle$status_matrix[[which(candidates)]]$product_attestation
      if (identical(root_required,"required") && is.na(x$detection_root_id[i]) ||
          identical(root_required,"na") && !is.na(x$detection_root_id[i]) ||
          identical(hash_required,"required") && is.na(x$product_hash[i]) ||
          identical(hash_required,"na") && !is.na(x$product_hash[i]) ||
          identical(execution_required,"required") && is.na(x$execution_id[i]) ||
          identical(execution_required,"na") && !is.na(x$execution_id[i]) ||
          identical(release_required,"required") && is.na(x$release_attestation_hash[i]) ||
          identical(release_required,"na") && !is.na(x$release_attestation_hash[i]) ||
          identical(product_attestation_required,"required") && is.na(x$product_attestation_hash[i]) ||
          identical(product_attestation_required,"na") && !is.na(x$product_attestation_hash[i])) {
        stop("Gate B v3 product-status identity presence is illegal.",call.=FALSE)
      }
      failed <- identical(x$product_status[i],"failed_closed")
      redetect <- identical(x$product_status[i],"requires_redetection")
      failure_pair <- !is.na(x$failure_stage[i]) &&
        !is.na(x$failure_code_registry_id[i])
      failure_half <- xor(is.na(x$failure_stage[i]),
                          is.na(x$failure_code_registry_id[i]))
      if (failure_half || failed != failure_pair ||
          redetect != !is.na(x$requires_redetection_reason_registry_id[i])) {
        stop("Gate B v3 failure/re-detection reason presence is illegal.",
             call.=FALSE)
      }
      reference_none <- identical(x$reference_standard_status[i], "none")
      if (reference_none != is.na(x$reference_manifest_hash[i])) {
        stop("Gate B v3 reference status/hash presence is illegal.",call.=FALSE)
      }
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_sum_binary64 <- function(x) {
  x <- as.double(x)
  if (any(!is.finite(x))) stop("Non-finite geometry source.",call.=FALSE)
  total <- 0
  for (value in x) total <- total + value
  total
}

stpd_gate_b_v3_status_fixture_row <- function(matrix_row,
    bundle=stpd_gate_b_v3_phase1_bundle()) {
  value <- function(rule,required_value,optional_value=NA_character_) {
    if (identical(rule,"required")) required_value else
      if (identical(rule,"optional")) optional_value else NA_character_
  }
  kind <- if (identical(matrix_row$product_kind,"any")) "auto" else
    matrix_row$product_kind
  failed <- identical(matrix_row$product_status,"failed_closed")
  redetect <- identical(matrix_row$product_status,"requires_redetection")
  data.frame(
    status_schema="stpd_multitrack_v3_status_1",product_kind=kind,
    product_status=matrix_row$product_status,
    execution_id=value(matrix_row$execution,
      "00000000-0000-4000-8000-000000000000"),
    detection_root_id=value(matrix_row$root,paste0("dr_",strrep("a",64L))),
    product_hash=value(matrix_row$product_hash,strrep("b",64L)),
    gate_b_status=matrix_row$gate_b,gate_c_status=matrix_row$gate_c,
    authority_scope=matrix_row$authority,
    record_authoritative=isTRUE(matrix_row$authoritative),
    biological_ground_truth=isTRUE(matrix_row$biological_ground_truth),
    reference_standard_status="none",
    detector_performance_eligible=isTRUE(matrix_row$detector_performance_eligible),
    integrity_level=if (identical(matrix_row$integrity,"detached_or_parent_bound"))
      "detached_internal_integrity" else matrix_row$integrity,
    failure_stage=if (failed) "materialization" else NA_character_,
    failure_code_registry_id=if (failed) paste0("reg_",strrep("c",64L)) else
      NA_character_,
    requires_redetection_reason_registry_id=if (redetect)
      paste0("reg_",strrep("d",64L)) else NA_character_,
    release_attestation_hash=value(matrix_row$release_attestation,
      strrep("e",64L)),
    product_attestation_hash=value(matrix_row$product_attestation,
      strrep("f",64L)),reference_manifest_hash=NA_character_,
    stringsAsFactors=FALSE)
}

stpd_gate_b_v3_status_matrix_probe <- function(invalid_scope) {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  rows <- lapply(bundle$status_matrix,stpd_gate_b_v3_status_fixture_row,
                 bundle=bundle)
  valid <- vapply(rows,function(row) tryCatch({
    stpd_gate_b_v3_validate_table_prototype(
      row,"product_status_envelope",bundle);TRUE
  },error=function(e) FALSE),logical(1))
  candidate <- which(vapply(bundle$status_matrix,function(row)
    identical(row$product_kind,"auto") &&
      identical(row$product_status,"gate_b_authoritative") &&
      identical(row$deployment_context,"live"),logical(1)))[[1L]]
  invalid <- rows[[candidate]]
  invalid$authority_scope <- invalid_scope
  invalid_rejected <- inherits(tryCatch({
    stpd_gate_b_v3_validate_table_prototype(
      invalid,"product_status_envelope",bundle);NULL
  },error=function(e)e),"error")
  list(valid_matrix_rows=as.integer(sum(valid)),
       invalid_scope_rejected=invalid_rejected)
}

stpd_gate_b_v3_interval_spine <- function(spine,train,start_isi,end_isi) {
  rows <- spine$train==train & spine$isi_index>=start_isi &
    spine$isi_index<=end_isi
  out <- spine[rows,,drop=FALSE]
  if (!identical(out$isi_index,seq.int(start_isi,end_isi))) {
    stop("Gate B v3 interval is not closed over the canonical per-ISI spine.",
         call.=FALSE)
  }
  out
}

stpd_gate_b_v3_resolve_domain_row <- function(
    tables, domain_id, entity_id, context) {
  domains <- tables$entity_domain_registry
  hit_domain <- which(domains$entity_domain_id == domain_id & domains$active)
  if (length(hit_domain) != 1L) {
    stop("Gate B v3 ", context, " domain is unknown or inactive.",
         call. = FALSE)
  }
  domain <- domains[hit_domain, , drop = FALSE]
  target_name <- domain$target_table[[1L]]
  target <- tables[[target_name]]
  key_columns <- tryCatch(
    as.character(unlist(jsonlite::fromJSON(
      domain$primary_key_columns_json[[1L]], simplifyVector = TRUE
    ), use.names = FALSE)),
    error = function(e) character()
  )
  if (is.null(target) || length(key_columns) != 1L ||
      !key_columns %in% names(target)) {
    stop("Gate B v3 ", context, " domain has no executable row resolver.",
         call. = FALSE)
  }
  hit_row <- which(target[[key_columns]] == entity_id)
  if (length(hit_row) != 1L) {
    stop("Gate B v3 ", context, " row is absent or ambiguous.",
         call. = FALSE)
  }
  list(
    domain = domain,
    table_name = target_name,
    key_columns = key_columns,
    row = target[hit_row, , drop = FALSE]
  )
}

stpd_gate_b_v3_source_row_hash <- function(
    source_domain, source_table_name, source_row, source_row_key,
    source_product_hash = NA_character_,
    bundle = stpd_gate_b_v3_phase1_bundle()) {
  if (!is.data.frame(source_domain) || nrow(source_domain) != 1L ||
      !is.data.frame(source_row) || nrow(source_row) != 1L ||
      !is.list(source_row_key) || is.null(names(source_row_key))) {
    stop("Gate B v3 source-row hash inputs are not exact.", call. = FALSE)
  }
  key_columns <- tryCatch(
    as.character(unlist(jsonlite::fromJSON(
      source_domain$primary_key_columns_json[[1L]], simplifyVector = TRUE
    ), use.names = FALSE)),
    error = function(e) character()
  )
  if (!identical(source_domain$target_table[[1L]], source_table_name) ||
      length(key_columns) == 0L || !setequal(names(source_row_key), key_columns) ||
      length(source_product_hash) != 1L ||
      (!is.na(source_product_hash) &&
       !grepl("^[0-9a-f]{64}$", source_product_hash))) {
    stop("Gate B v3 source-row hash domain/key/product binding is invalid.",
         call. = FALSE)
  }
  source_bytes <- stpd_gate_b_v3_canonical_table_bytes(
    source_row, source_table_name, bundle
  )
  envelope <- list(
    source_row_hash_schema =
      "stpd_multitrack_v3_entity_evidence_source_row_hash_1",
    source_domain_id = source_domain$entity_domain_id[[1L]],
    source_table_name = source_table_name,
    primary_key_columns = as.list(key_columns),
    source_row_key = source_row_key,
    source_binding = if (is.na(source_product_hash))
      "current_product" else "strict_ancestor_product",
    source_product_hash = if (is.na(source_product_hash))
      NULL else source_product_hash,
    canonical_source_row_sha256 = digest::digest(
      source_bytes, algo = "sha256", serialize = FALSE
    )
  )
  stpd_gate_b_v3_hash_raw(
    "stpd-entity-evidence-source-row-v1",
    stpd_gate_b_v3_canonical_json(envelope)
  )
}

stpd_gate_b_v3_expected_entity_edge_indices <- function(edges) {
  if (!is.data.frame(edges) || nrow(edges) == 0L) return(integer())
  target_key <- paste(edges$entity_domain_id, edges$entity_id, sep = "\u001f")
  expected <- integer(nrow(edges))
  for (indices in split(seq_len(nrow(edges)), target_key)) {
    payload <- vapply(indices, function(i) stpd_gate_b_v3_canonical_json(list(
      evidence_role_registry_id = edges$evidence_role_registry_id[[i]],
      source_domain_id = edges$source_domain_id[[i]],
      source_row_key_json = edges$source_row_key_json[[i]],
      source_product_hash = if (is.na(edges$source_product_hash[[i]]))
        NULL else edges$source_product_hash[[i]],
      source_row_hash = edges$source_row_hash[[i]],
      evidence_id = edges$evidence_id[[i]]
    )), character(1))
    if (anyDuplicated(payload)) {
      stop("Gate B v3 entity-evidence pre-index payload is duplicated.",
           call. = FALSE)
    }
    payload_key <- stpd_gate_b_v3_utf8_sort_key(payload)
    order_index <- order(payload_key, method = "radix")
    expected[indices[order_index]] <- seq_along(indices)
  }
  expected
}

stpd_gate_b_v3_validate_entity_provenance_lineage_closure <- function(
    tables, bundle) {
  edges <- tables$entity_evidence_edges
  if (nrow(edges) > 0L) {
    if (any(!is.na(edges$entity_product_hash)) ||
        any(!is.na(edges$source_product_hash))) {
      stop(paste(
        "Gate B v3 phase-1 detached validation cannot authorize",
        "cross-product provenance hashes without a verified ancestor inventory."
      ), call. = FALSE)
    }
    for (i in seq_len(nrow(edges))) {
      target <- stpd_gate_b_v3_resolve_domain_row(
        tables, edges$entity_domain_id[[i]], edges$entity_id[[i]],
        "provenance target"
      )
      source_domain <- tables$entity_domain_registry[
        tables$entity_domain_registry$entity_domain_id ==
          edges$source_domain_id[[i]] &
          tables$entity_domain_registry$active,
        , drop = FALSE
      ]
      if (nrow(source_domain) != 1L) {
        stop("Gate B v3 provenance source domain is unknown or inactive.",
             call. = FALSE)
      }
      source_table_name <- source_domain$target_table[[1L]]
      source_table <- tables[[source_table_name]]
      key_columns <- tryCatch(
        as.character(unlist(jsonlite::fromJSON(
          source_domain$primary_key_columns_json[[1L]],
          simplifyVector = TRUE
        ), use.names = FALSE)),
        error = function(e) character()
      )
      key_object <- tryCatch(jsonlite::fromJSON(
        edges$source_row_key_json[[i]], simplifyVector = FALSE
      ), error = function(e) NULL)
      if (is.null(source_table) || length(key_columns) == 0L ||
          any(!key_columns %in% names(source_table)) ||
          is.null(key_object) || is.null(names(key_object)) ||
          anyDuplicated(names(key_object)) ||
          !setequal(names(key_object), key_columns) ||
          !identical(stpd_gate_b_v3_canonical_json(key_object),
                     edges$source_row_key_json[[i]])) {
        stop("Gate B v3 provenance source-row key is not canonical/executable.",
             call. = FALSE)
      }
      key_match <- rep(TRUE, nrow(source_table))
      for (column in key_columns) {
        value <- key_object[[column]]
        if (is.null(value) || length(value) != 1L) {
          stop("Gate B v3 provenance source-row key contains a null/non-scalar value.",
               call. = FALSE)
        }
        expected_type <- bundle$tables[[source_table_name]]$columns[[
          match(column, vapply(bundle$tables[[source_table_name]]$columns,
                               `[[`, character(1), "name"))
        ]]$type
        typed_value <- switch(expected_type,
          chr = if (is.character(value)) value else NA_character_,
          int = if (is.integer(value) ||
                    (is.numeric(value) && value == floor(value)))
            as.integer(value) else NA_integer_,
          dbl = if (is.numeric(value)) as.double(value) else NA_real_,
          lgl = if (is.logical(value)) value else NA,
          NA
        )
        if (length(typed_value) != 1L || is.na(typed_value)) {
          stop("Gate B v3 provenance source-row key has the wrong type.",
               call. = FALSE)
        }
        key_match <- key_match & !is.na(source_table[[column]]) &
          source_table[[column]] == typed_value
      }
      hit <- which(key_match)
      if (length(hit) != 1L) {
        stop("Gate B v3 provenance source-row key is absent or ambiguous.",
             call. = FALSE)
      }
      source <- source_table[hit, , drop = FALSE]
      target_train_column <- target$domain$train_column[[1L]]
      source_train_column <- source_domain$train_column[[1L]]
      if (!is.na(target_train_column) && nzchar(target_train_column) &&
          !is.na(source_train_column) && nzchar(source_train_column) &&
          !identical(target$row[[target_train_column]][[1L]],
                     source[[source_train_column]][[1L]])) {
        stop("Gate B v3 provenance source and target resolve across train scope.",
             call. = FALSE)
      }
      expected_source_row_hash <- stpd_gate_b_v3_source_row_hash(
        source_domain = source_domain,
        source_table_name = source_table_name,
        source_row = source,
        source_row_key = key_object,
        source_product_hash = edges$source_product_hash[[i]],
        bundle = bundle
      )
      if (!identical(edges$source_row_hash[[i]], expected_source_row_hash)) {
        stop("Gate B v3 provenance source-row hash is invalid.", call. = FALSE)
      }
    }
    expected_edge_index <- stpd_gate_b_v3_expected_entity_edge_indices(edges)
    if (!identical(unname(edges$edge_index), unname(expected_edge_index))) {
      stop("Gate B v3 entity-evidence edge-index expected set is invalid.",
           call. = FALSE)
    }
  }

  lineage <- tables$lineage_records
  if (nrow(lineage) > 0L) {
    if (any(!is.na(lineage$child_product_hash)) ||
        any(!is.na(lineage$parent_product_hash))) {
      stop(paste(
        "Gate B v3 phase-1 detached validation cannot authorize",
        "cross-product lineage without a verified ancestor inventory."
      ), call. = FALSE)
    }
    for (i in seq_len(nrow(lineage))) {
      child <- stpd_gate_b_v3_resolve_domain_row(
        tables, lineage$child_domain_id[[i]], lineage$child_id[[i]],
        "lineage child"
      )
      parent <- stpd_gate_b_v3_resolve_domain_row(
        tables, lineage$parent_domain_id[[i]], lineage$parent_id[[i]],
        "lineage parent"
      )
      if (identical(lineage$child_domain_id[[i]],
                    lineage$parent_domain_id[[i]]) &&
          identical(lineage$child_id[[i]], lineage$parent_id[[i]])) {
        stop("Gate B v3 lineage contains a direct self-cycle.", call. = FALSE)
      }
      child_train_column <- child$domain$train_column[[1L]]
      parent_train_column <- parent$domain$train_column[[1L]]
      if (!is.na(child_train_column) && nzchar(child_train_column) &&
          !is.na(parent_train_column) && nzchar(parent_train_column) &&
          !identical(child$row[[child_train_column]][[1L]],
                     parent$row[[parent_train_column]][[1L]])) {
        stop("Gate B v3 lineage parent and child resolve across train scope.",
             call. = FALSE)
      }
    }
    node <- function(domain, id) paste(domain, id, sep = "\u001f")
    child_nodes <- node(lineage$child_domain_id, lineage$child_id)
    parent_nodes <- node(lineage$parent_domain_id, lineage$parent_id)
    all_nodes <- unique(c(child_nodes, parent_nodes))
    indegree <- setNames(integer(length(all_nodes)), all_nodes)
    adjacency <- split(parent_nodes, child_nodes)
    for (parent_node in parent_nodes[parent_nodes %in% all_nodes]) {
      indegree[[parent_node]] <- indegree[[parent_node]] + 1L
    }
    queue <- names(indegree)[indegree == 0L]
    visited <- character()
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      visited <- c(visited, current)
      for (parent_node in adjacency[[current]] %||% character()) {
        indegree[[parent_node]] <- indegree[[parent_node]] - 1L
        if (indegree[[parent_node]] == 0L) {
          queue <- unique(c(queue, parent_node))
        }
      }
    }
    if (length(visited) != length(all_nodes)) {
      stop("Gate B v3 current-product lineage graph contains a cycle.",
           call. = FALSE)
    }
    child_groups <- split(seq_len(nrow(lineage)),child_nodes)
    for (indices in child_groups) {
      lineage_ids <- unique(lineage$lineage_id[indices])
      if (length(lineage_ids)!=1L) {
        stop("Gate B v3 lineage child has more than one parent-edge set.",
             call.=FALSE)
      }
      child <- stpd_gate_b_v3_resolve_domain_row(
        tables,lineage$child_domain_id[[indices[[1L]]]],
        lineage$child_id[[indices[[1L]]]],"lineage child reverse closure"
      )
      if ("lineage_id" %in% names(child$row) &&
          (is.na(child$row$lineage_id[[1L]]) ||
           !identical(child$row$lineage_id[[1L]],lineage_ids[[1L]]))) {
        stop("Gate B v3 lineage parent-edge set is not referenced by its child.",
             call.=FALSE)
      }
    }
  }

  lineage_tables <- names(bundle$tables)[vapply(bundle$tables, function(spec) {
    "lineage_id" %in% vapply(spec$columns, `[[`, character(1), "name")
  }, logical(1))]
  lineage_tables <- setdiff(lineage_tables, "lineage_records")
  for (table_name in lineage_tables) {
    entity <- tables[[table_name]]
    if (nrow(entity) == 0L) next
    domain <- tables$entity_domain_registry[
      tables$entity_domain_registry$target_table == table_name &
        tables$entity_domain_registry$active,
      , drop = FALSE
    ]
    id_column <- unlist(bundle$tables[[table_name]]$primary_key,
                        use.names = FALSE)
    if (nrow(domain) != 1L || length(id_column) != 1L) {
      stop("Gate B v3 lineage-bearing entity domain is not executable.",
           call. = FALSE)
    }
    for (i in which(!is.na(entity$lineage_id))) {
      linked <- lineage[lineage$lineage_id == entity$lineage_id[[i]],
                        , drop = FALSE]
      if (nrow(linked) == 0L ||
          any(linked$child_domain_id != domain$entity_domain_id[[1L]]) ||
          any(linked$child_id != entity[[id_column]][[i]])) {
        stop("Gate B v3 entity lineage ID resolves to a different child.",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_transition_row_hash <- function(row) {
  if (!is.data.frame(row) || nrow(row) != 1L) {
    stop("Gate B v3 transition hash requires exactly one row.", call. = FALSE)
  }
  payload_names <- setdiff(names(row), c("transition_id", "transition_hash"))
  payload <- lapply(payload_names, function(name) {
    value <- if (is.list(row[[name]])) row[[name]][[1L]] else row[[name]][[1L]]
    if (length(value) == 1L && is.na(value)) NULL else value
  })
  names(payload) <- payload_names
  stpd_gate_b_v3_hash_raw(
    "stpd-transition-v1", stpd_gate_b_v3_canonical_json(payload)
  )
}

stpd_gate_b_v3_history_head_hash <- function(parent_auto_product_hash,
                                             transitions) {
  if (length(parent_auto_product_hash) != 1L ||
      !grepl("^[0-9a-f]{64}$", parent_auto_product_hash) ||
      !is.data.frame(transitions)) {
    stop("Gate B v3 history-head inputs are invalid.", call. = FALSE)
  }
  transition_n <- nrow(transitions)
  last_transition_hash <- if (transition_n == 0L) NULL else
    transitions$transition_hash[[transition_n]]
  stpd_gate_b_v3_hash_raw(
    "stpd-history-head-v1",
    stpd_gate_b_v3_canonical_json(list(
      parent_auto_product_hash = parent_auto_product_hash,
      transition_n = as.integer(transition_n),
      last_transition_hash = last_transition_hash
    ))
  )
}

stpd_gate_b_v3_validate_product_history_matrix <- function(tables) {
  identity <- tables$materialized_product_identity
  transitions <- tables$adjudication_transitions
  kind <- identity$product_kind[[1L]]
  parent_missing <- is.na(identity$parent_auto_product_hash[[1L]])
  history_missing <- is.na(identity$history_head_hash[[1L]])
  if (identical(kind, "auto")) {
    if (!parent_missing || !history_missing || nrow(transitions) != 0L) {
      stop(paste(
        "Gate B v3 AUTO identity requires typed-NA parent/history",
        "and an empty transition history."
      ), call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!identical(kind, "final") || parent_missing || history_missing) {
    stop(paste(
      "Gate B v3 FINAL identity requires nonmissing parent AUTO",
      "and history-head hashes."
    ), call. = FALSE)
  }
  parent <- identity$parent_auto_product_hash[[1L]]
  if (nrow(transitions) > 0L) {
    if (!identical(unname(transitions$sequence_no), seq_len(nrow(transitions))) ||
        any(transitions$parent_auto_product_hash != parent)) {
      stop("Gate B v3 FINAL transition sequence/parent expected set is invalid.",
           call. = FALSE)
    }
    genesis <- stpd_gate_b_v3_hash_raw(
      "stpd-history-genesis-v1", parent
    )
    expected_previous <- c(genesis, transitions$transition_hash[-nrow(transitions)])
    if (!identical(unname(transitions$previous_transition_hash),
                   unname(expected_previous))) {
      stop("Gate B v3 FINAL transition hash chain is invalid.", call. = FALSE)
    }
    expected_transition_hash <- vapply(seq_len(nrow(transitions)), function(i) {
      stpd_gate_b_v3_transition_row_hash(transitions[i, , drop = FALSE])
    }, character(1))
    if (!identical(unname(transitions$transition_hash),
                   unname(expected_transition_hash))) {
      stop("Gate B v3 FINAL transition row hash is invalid.", call. = FALSE)
    }
    first_parent_final <- stpd_gate_b_v3_hash_raw(
      "stpd-initial-final-v1", parent
    )
    if (!identical(transitions$expected_parent_final_hash[[1L]],
                   first_parent_final)) {
      stop("Gate B v3 first FINAL compare-and-swap parent hash is invalid.",
           call. = FALSE)
    }
  }
  expected_history <- stpd_gate_b_v3_history_head_hash(parent, transitions)
  if (!identical(identity$history_head_hash[[1L]], expected_history)) {
    stop("Gate B v3 FINAL history-head hash is invalid.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a complete Gate B v3 canonical product prototype
#' @param tables Named list containing every canonical inventory table.
#' @param bundle Optional frozen phase-1 schema bundle.
#' @return Invisible TRUE; otherwise fails closed.
#' @export
stpd_gate_b_v3_validate_product_prototype <- function(
    tables,bundle=stpd_gate_b_v3_phase1_bundle()) {
  if (!is.list(tables) || is.null(names(tables)) || anyDuplicated(names(tables))) {
    stop("Gate B v3 canonical product must be a uniquely named table list.",
         call.=FALSE)
  }
  required_inventory <- names(bundle$product_inventory)
  required <- names(bundle$tables)
  if (!setequal(names(tables),required)) {
    stop("Gate B v3 complete product prototype is missing canonical or signed support tables, or has extras.",
         call.=FALSE)
  }
  for (name in required) {
    stpd_gate_b_v3_validate_table_prototype(
      tables[[name]],name,bundle,tables=tables,require_sorted=TRUE)
  }
  identity <- tables$materialized_product_identity
  if (nrow(identity)!=1L) {
    stop("Gate B v3 materialized product must have exactly one identity row.",
         call.=FALSE)
  }
  stpd_gate_b_v3_validate_product_history_matrix(tables)
  nonempty <- c("threshold_policies","threshold_instances",
                "contract_registries","entity_domain_registry")
  empty_required <- nonempty[vapply(nonempty,function(name)
    nrow(tables[[name]])==0L,logical(1))]
  if (length(empty_required)>0L) {
    stop("Gate B v3 required nonempty canonical tables are empty: ",
         paste(empty_required,collapse=", "),call.=FALSE)
  }
  if (nrow(tables$per_isi)>0L && nrow(tables$scientific_context)==0L) {
    stop("Gate B v3 nonempty input requires scientific context rows.",
         call.=FALSE)
  }
  stpd_gate_b_v3_validate_threshold_permissions(
    tables$threshold_policies,tables$threshold_instances,
    tables$partition_memberships
  )
  stpd_gate_b_v3_validate_threshold_source_closure(tables)
  stpd_gate_b_v3_validate_entity_provenance_lineage_closure(tables,bundle)
  stpd_gate_b_v3_validate_identity_hash_closure(tables,bundle)
  root <- identity$detection_root_id[[1L]]
  source_context <- identity$source_context_hash[[1L]]
  for (name in required) {
    tab <- tables[[name]]
    if ("detection_root_id" %in% names(tab) && nrow(tab)>0L &&
        any(tab$detection_root_id!=root)) {
      stop("Gate B v3 canonical product mixes detection roots.",call.=FALSE)
    }
    if ("source_context_hash" %in% names(tab) && nrow(tab)>0L &&
        any(tab$source_context_hash!=source_context)) {
      stop("Gate B v3 canonical product mixes source contexts.",call.=FALSE)
    }
  }

  spine <- tables$per_isi
  for (train in unique(spine$train)) {
    rows <- spine[spine$train==train,,drop=FALSE]
    if (!identical(rows$isi_index,seq_len(nrow(rows))) ||
        !identical(rows$left_spike_index,rows$isi_index) ||
        !identical(rows$right_spike_index,rows$isi_index+1L) ||
        !identical(unname(rows$isi_sec),
                   unname(rows$isi_end_time_sec-rows$isi_start_time_sec)) ||
        any(rows$isi_sec<=0) ||
        (nrow(rows)>1L &&
         !identical(rows$isi_end_time_sec[-nrow(rows)],
                    rows$isi_start_time_sec[-1L]))) {
      stop("Gate B v3 per-ISI coordinate spine is not exact.",call.=FALSE)
    }
  }

  interval_tables <- c("state_candidates","state_episodes","state_segments",
    "event_candidates","events","gaps")
  duration_field <- c(state_candidates="duration_sec",
    state_episodes="envelope_duration_sec",state_segments="duration_sec",
    event_candidates="duration_sec",events="duration_sec",gaps="duration_sec")
  for (name in interval_tables) {
    tab <- tables[[name]]
    if (nrow(tab)==0L) next
    for (i in seq_len(nrow(tab))) {
      rows <- stpd_gate_b_v3_interval_spine(
        spine,tab$train[[i]],tab$start_isi[[i]],tab$end_isi[[i]])
      exact_duration <- stpd_gate_b_v3_sum_binary64(rows$isi_sec)
      if (!identical(tab$start_time_sec[[i]],rows$isi_start_time_sec[[1L]]) ||
          !identical(tab$end_time_sec[[i]],rows$isi_end_time_sec[[nrow(rows)]]) ||
          !identical(tab[[duration_field[[name]]]][[i]],exact_duration)) {
        stop("Gate B v3 interval geometry differs from the canonical spine: ",
             name,call.=FALSE)
      }
    }
  }

  episodes <- tables$state_episodes
  segments <- tables$state_segments
  candidates <- tables$state_candidates
  axis <- tables$state_axis_evidence
  gaps <- tables$gaps
  boundaries <- tables$boundary_evidence
  for (i in seq_len(nrow(episodes))) {
    episode <- episodes[i,,drop=FALSE]
    parts <- segments[segments$state_episode_id==episode$state_episode_id,,drop=FALSE]
    if (nrow(parts)==0L ||
        !identical(parts$segment_index,seq_len(nrow(parts))) ||
        parts$start_isi[[1L]]!=episode$start_isi ||
        parts$end_isi[[nrow(parts)]]!=episode$end_isi ||
        (nrow(parts)>1L && any(parts$start_isi[-1L]!=
          parts$end_isi[-nrow(parts)]+1L)) ||
        any(parts$train!=episode$train) ||
        any(parts$state_class!=episode$state_class) ||
        any(parts$state_frequency_class!=episode$state_frequency_class) ||
        any(parts$state_regularity_class!=episode$state_regularity_class) ||
        (nrow(parts)>1L && any(parts$segment_role[-1L]==
          parts$segment_role[-nrow(parts)]))) {
      stop("Gate B v3 State episode/segment partition is incomplete or nonmaximal.",
           call.=FALSE)
    }
    direct <- parts$segment_role=="direct_support"
    connector <- !direct
    direct_n <- sum(parts$n_isi[direct])
    connector_n <- sum(parts$n_isi[connector])
    direct_sec <- stpd_gate_b_v3_sum_binary64(parts$duration_sec[direct])
    connector_sec <- stpd_gate_b_v3_sum_binary64(parts$duration_sec[connector])
    envelope_sec <- direct_sec+connector_sec
    if (!identical(episode$direct_support_n_isi[[1L]],as.integer(direct_n)) ||
        !identical(episode$connector_n_isi[[1L]],as.integer(connector_n)) ||
        !identical(episode$direct_support_duration_sec[[1L]],direct_sec) ||
        !identical(episode$connector_duration_sec[[1L]],connector_sec) ||
        !identical(episode$envelope_duration_sec[[1L]],envelope_sec) ||
        !identical(episode$support_fraction[[1L]],direct_sec/envelope_sec)) {
      stop("Gate B v3 State episode support/count/duration closure is invalid.",
           call.=FALSE)
    }
    for (j in which(direct)) {
      candidate <- candidates[candidates$state_candidate_id==
        parts$source_state_candidate_id[[j]],,drop=FALSE]
      evidence <- axis[axis$state_axis_evidence_id==
        parts$state_axis_evidence_id[[j]],,drop=FALSE]
      if (nrow(candidate)!=1L || nrow(evidence)!=1L ||
          candidate$candidate_decision!="accepted_direct_support" ||
          candidate$start_isi!=parts$start_isi[[j]] ||
          candidate$end_isi!=parts$end_isi[[j]] ||
          evidence$state_candidate_id!=candidate$state_candidate_id ||
          evidence$frequency_evidence_status!="pass" ||
          evidence$regularity_evidence_status!="pass") {
        stop("Gate B v3 direct fragment did not independently pass before linking.",
             call.=FALSE)
      }
    }
    owned <- seq.int(episode$start_isi,episode$end_isi)
    if (nrow(gaps)>0L && any(gaps$train==episode$train &
        gaps$start_isi %in% owned) || nrow(boundaries)>0L && any(
          boundaries$train==episode$train & boundaries$hard_for_state &
          vapply(seq_len(nrow(boundaries)),function(k) any(seq.int(
            boundaries$start_isi[[k]],boundaries$end_isi[[k]]) %in% owned),
            logical(1)))) {
      stop("Gate B v3 Pause/hard boundary occurs inside a State episode.",
           call.=FALSE)
    }
  }
  if (nrow(episodes)>1L) {
    for (train in unique(episodes$train)) {
      z <- episodes[episodes$train==train,,drop=FALSE]
      if (nrow(z)>1L && any(z$start_isi[-1L]<=z$end_isi[-nrow(z)])) {
        stop("Gate B v3 accepted State episodes overlap.",call.=FALSE)
      }
    }
  }

  # Per-ISI ownership is reconstructed, never trusted as a lossy projection.
  expected_episode <- expected_segment <- expected_role <- expected_active <-
    expected_class <- rep(NA_character_,nrow(spine))
  expected_membership <- expected_direct <- rep(FALSE,nrow(spine))
  for (i in seq_len(nrow(segments))) {
    hit <- spine$train==segments$train[[i]] &
      spine$isi_index>=segments$start_isi[[i]] &
      spine$isi_index<=segments$end_isi[[i]]
    if (any(expected_membership[hit])) stop("State segment ownership overlaps.",call.=FALSE)
    expected_membership[hit] <- TRUE
    expected_episode[hit] <- segments$state_episode_id[[i]]
    expected_segment[hit] <- segments$state_segment_id[[i]]
    expected_role[hit] <- segments$segment_role[[i]]
    if (segments$segment_role[[i]]=="direct_support") {
      expected_direct[hit] <- TRUE
      expected_active[hit] <- segments$state_segment_id[[i]]
      expected_class[hit] <- segments$state_class[[i]]
    }
  }
  same_na <- function(a,b) identical(is.na(a),is.na(b)) &&
    identical(unname(a[!is.na(a)]),unname(b[!is.na(b)]))
  if (!same_na(spine$state_episode_id,expected_episode) ||
      !same_na(spine$state_segment_id,expected_segment) ||
      !same_na(spine$state_segment_role,expected_role) ||
      !same_na(spine$active_state_segment_id,expected_active) ||
      !same_na(spine$active_state_class,expected_class) ||
      !identical(spine$state_episode_membership,expected_membership) ||
      !identical(spine$state_direct_support,expected_direct)) {
    stop("Gate B v3 per-ISI State projection is not the exact segment partition.",
         call.=FALSE)
  }

  events <- tables$events
  modifiers <- tables$event_modifier_evidence
  for (i in seq_len(nrow(events))) {
    rows <- modifiers[modifiers$event_id==events$event_id[[i]],,drop=FALSE]
    if (nrow(rows)!=2L || !setequal(rows$modifier_domain,c("extent","frequency")) ||
        rows$modifier_value[match("extent",rows$modifier_domain)]!=
          events$extent_modifier[[i]] ||
        rows$modifier_value[match("frequency",rows$modifier_domain)]!=
          events$frequency_modifier[[i]] ||
        rows$modifier_evidence_id[match("extent",rows$modifier_domain)]!=
          events$extent_modifier_evidence_id[[i]] ||
        rows$modifier_evidence_id[match("frequency",rows$modifier_domain)]!=
          events$frequency_modifier_evidence_id[[i]]) {
      stop("Gate B v3 Event modifier evidence is not exactly one row per axis.",
           call.=FALSE)
    }
  }
  if (any(!modifiers$event_id %in% events$event_id)) {
    stop("Gate B v3 Event modifier evidence is orphaned.",call.=FALSE)
  }

  accepted_state <- candidates$state_candidate_id[
    candidates$candidate_decision=="accepted_direct_support"]
  consumed_state <- segments$source_state_candidate_id[
    segments$segment_role=="direct_support"]
  if (!setequal(accepted_state,consumed_state) ||
      anyDuplicated(consumed_state)) {
    stop("Gate B v3 accepted State-candidate expected set is invalid.",
         call.=FALSE)
  }
  event_candidates <- tables$event_candidates
  accepted_event <- event_candidates[
    event_candidates$candidate_decision=="accepted_event",,drop=FALSE]
  event_key <- function(x,candidate=FALSE) {
    if (nrow(x)==0L) return(character())
    event_class <- if (candidate) {
      ifelse(x$candidate_class=="burst_candidate","burst_event",NA_character_)
    } else x$event_class
    paste(x$train,event_class,x$start_isi,x$end_isi,
          x$base_event_evidence_id,sep="\u001f")
  }
  accepted_keys <- event_key(accepted_event,TRUE)
  event_keys <- event_key(events,FALSE)
  if (anyNA(accepted_keys) || !setequal(accepted_keys,event_keys) ||
      anyDuplicated(accepted_keys) || anyDuplicated(event_keys)) {
    stop("Gate B v3 accepted Event-candidate expected set is invalid.",
         call.=FALSE)
  }

  expected_rel <- character()
  if (nrow(events)>0L && nrow(episodes)>0L) {
    for (i in seq_len(nrow(events))) for (j in seq_len(nrow(episodes))) {
      if (events$train[[i]]!=episodes$train[[j]]) next
      lo <- max(events$start_isi[[i]],episodes$start_isi[[j]])
      hi <- min(events$end_isi[[i]],episodes$end_isi[[j]])
      if (lo<=hi) expected_rel <- c(expected_rel,paste(
        events$event_id[[i]],episodes$state_episode_id[[j]],sep="\u001f"))
    }
  }
  observed_rel <- if (nrow(tables$event_state_relationships)==0L) character() else
    paste(tables$event_state_relationships$event_id,
          tables$event_state_relationships$state_episode_id,sep="\u001f")
  if (!setequal(expected_rel,observed_rel) || anyDuplicated(observed_rel)) {
    stop("Gate B v3 Event-State expected-set closure is invalid.",call.=FALSE)
  }

  expected_links <- character()
  hf <- c("high_frequency_irregular_state","high_frequency_tonic")
  if (nrow(gaps)>0L && nrow(episodes)>1L) {
    for (i in seq_len(nrow(gaps))) {
      pre <- episodes[episodes$train==gaps$train[[i]] &
        episodes$end_isi==gaps$start_isi[[i]]-1L &
        episodes$state_class %in% hf,,drop=FALSE]
      post <- episodes[episodes$train==gaps$train[[i]] &
        episodes$start_isi==gaps$end_isi[[i]]+1L &
        episodes$state_class %in% hf,,drop=FALSE]
      if (nrow(pre)==1L && nrow(post)==1L) expected_links <- c(expected_links,
        paste(pre$state_episode_id,gaps$gap_id[[i]],post$state_episode_id,
              sep="\u001f"))
    }
  }
  links <- tables$state_episode_links
  observed_links <- if (nrow(links)==0L) character() else paste(
    links$pre_state_episode_id,links$gap_id,links$post_state_episode_id,
    sep="\u001f")
  if (!setequal(expected_links,observed_links) || anyDuplicated(observed_links)) {
    stop("Gate B v3 pause-interrupted episode-link expected set is invalid.",
         call.=FALSE)
  }
  invisible(TRUE)
}

stpd_gate_b_v3_entity_domain_registry <- function(
    bundle=stpd_gate_b_v3_phase1_bundle()) {
  rows <- list()
  seen <- character()
  for (table_name in names(bundle$tables)) {
    spec <- bundle$tables[[table_name]]
    primary <- unlist(spec$primary_key,use.names=FALSE)
    if (length(primary)!=1L) next
    resolution <- spec$id_resolution[[primary[[1L]]]]
    if (is.null(resolution) || !identical(resolution$resolution,
                                          "generated_entity")) next
    domain <- resolution$domain
    if (domain %in% seen || identical(domain,"entity_domain")) next
    seen <- c(seen,domain)
    fields <- vapply(spec$columns,`[[`,character(1),"name")
    train_column <- if ("train" %in% fields) "train" else NA_character_
    pk_json <- stpd_gate_b_v3_canonical_json(as.list(primary))
    payload <- list(entity_domain=domain,target_table=table_name,
      primary_key_columns_json=pk_json,train_column=if(is.na(train_column))NULL
        else train_column,product_scope="auto_final_shared")
    rows[[length(rows)+1L]] <- data.frame(
      schema_version="stpd_multitrack_v3_1",
      entity_domain_id=stpd_gate_b_v3_entity_id("entity_domain",payload,bundle),
      entity_domain=domain,target_table=table_name,
      primary_key_columns_json=pk_json,train_column=train_column,
      product_scope="auto_final_shared",active=TRUE,stringsAsFactors=FALSE)
  }
  out <- do.call(rbind,rows)
  stpd_gate_b_v3_sort_table(out,unlist(
    bundle$tables$entity_domain_registry$sort_key,use.names=FALSE))
}

stpd_gate_b_v3_phase1_minimal_product <- function(
    with_overlap = FALSE,
    state_profile = c("high_irregular","high_regular","non_high_regular"),
    structure_first_fixture = TRUE) {
  state_profile <- match.arg(state_profile)
  bundle <- stpd_gate_b_v3_phase1_bundle()
  cache_key <- paste(isTRUE(with_overlap),state_profile,
    isTRUE(structure_first_fixture),
    bundle$normative_contract_sha256,bundle$schema_contract_sha256,sep="|")
  cached_products <- stpd_gate_b_v3_phase1_runtime_cache$minimal_products
  if (is.list(cached_products) && !is.null(cached_products[[cache_key]])) {
    return(unserialize(cached_products[[cache_key]]))
  }
  tables <- lapply(names(bundle$tables),stpd_gate_b_v3_empty_table,
                   bundle=bundle)
  names(tables) <- names(bundle$tables)
  registry <- stpd_gate_b_v3_phase1_registry(FALSE)
  tables$contract_registries <- registry
  tables$entity_domain_registry <- stpd_gate_b_v3_entity_domain_registry(bundle)
  rid <- function(domain,code=NULL) {
    hit <- registry$registry_domain==domain & registry$active
    if (!is.null(code)) hit <- hit & registry$code==code
    index <- which(hit)
    if (length(index)<1L) stop("Minimal fixture registry entry missing.",call.=FALSE)
    registry$registry_entry_id[[index[[1L]]]]
  }
  semantic_hash <- function(id) registry$semantic_definition_sha256[
    match(id,registry$registry_entry_id)]
  train <- paste0("tr_",strrep("a",64L))
  # The overlap fixture is intentionally high-rate and irregular: two ISIs
  # of 5 ms and 15 ms yield 100 Hz and median CV2 approximately 1.  The
  # stored binary64 value is computed by the same frozen estimator used by
  # the validator rather than rounded to a display value.  This makes
  # the HF-irregular State claim independently recomputable from the spine.
  timestamps <- if (!isTRUE(with_overlap)) 0 else if (
      isTRUE(structure_first_fixture)) switch(state_profile,
        high_irregular=c(0,0.004,0.016,0.020,0.120),
        high_regular=c(0,0.005,0.01,0.015,0.115),
        non_high_regular=c(0,0.025,0.05,0.075,0.175)) else switch(state_profile,
          high_irregular=c(0,0.005,0.02,0.035),
          high_regular=c(0,0.005,0.01,0.015),
          non_high_regular=c(0,0.025,0.05,0.075))
  blind <- stpd_gate_b_v3_label_blind_input(list(trains=setNames(
    list(data.frame(timestamp=timestamps)),train)))
  tables$normalized_input_manifest <- blind$normalized_input_manifest
  tables$label_blind_contract_manifest <- blind$label_blind_contract_manifest
  canonical <- stpd_gate_b_v3_canonical_json
  hash <- stpd_gate_b_v3_hash_raw
  threshold_specs <- data.frame(
    path=c(
      "event.selector.minimum_score",
      "event.extent.long_min_spikes",
      "event.extent.prolonged_min_spikes",
      "event.frequency.high_enter_hz",
      "event.frequency.high_exit_hz",
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
      "spiketrainpattern.burst.allow_one_sided_as_canonical",
      "pause.scan_seed_sec",
      "pause.absolute_threshold_sec",
      "pause.local_ratio_min",
      "pause.local_window_isi",
      "pause.guard_band_isi",
      "pause.local_min_valid_isi",
      "pause.score_alpha",
      "state.frequency.high_enter_hz",
      "state.frequency.high_exit_hz",
      "state.regularity.regular_upper_cv2",
      "state.regularity.irregular_lower_cv2",
      "state.state_min_valid_isi",
      "state.state_min_duration_sec"),
    value=c(0.5,5,8,150,100,3,10,0.0009,1,3,8,3,3,0.8,0.35,8,1.25,1,1,
            0.004,0.2,3,2,0,1,0.05,
            50,45,0.5,0.8,2,0.001),
    units=c("score","spikes","spikes","Hz","Hz","spikes","spikes","s",
            "boolean","ISI","ISI","ratio","ratio","probability",
            "fraction","ISI","ratio","boolean","boolean",
            "s","s","ratio",
            "ISI","ISI","ISI","probability","Hz","Hz","ratio",
            "ratio","ISI","s"),
    stringsAsFactors=FALSE)
  threshold_specs$requested_value_hash <- vapply(
    threshold_specs$value,function(value) hash(
      "stpd-requested-param-value-v1",canonical(list(
        value_type="double",value=value))),character(1))
  tables$requested_params_manifest <- data.frame(
    path=threshold_specs$path,value_type="double",
    canonical_value_hash=threshold_specs$requested_value_hash,
    stringsAsFactors=FALSE)
  tables$requested_params_manifest <- stpd_gate_b_v3_sort_table(
    tables$requested_params_manifest,unlist(
      bundle$tables$requested_params_manifest$sort_key,use.names=FALSE))
  tables$source_manifest <- data.frame(
    repository_relative_path="R/74_gate_b_v3_phase1_contract.R",
    file_type="R",size_bytes=1,
    content_sha256=digest::digest("phase1-fixture-source",algo="sha256",
                                  serialize=FALSE),stringsAsFactors=FALSE)
  tables$native_manifest <- data.frame(
    repository_relative_path="src/stpd_jcs.cpp",file_type="cpp",size_bytes=1,
    content_sha256=digest::digest("phase1-fixture-native",algo="sha256",
                                  serialize=FALSE),native_build_id="fixture-native",
    stringsAsFactors=FALSE)
  toolchain_core <- list(os="fixture-os",arch="fixture-arch",
    R_version=as.character(getRversion()),compiler_id="fixture-compiler",
    compiler_version="1",BLAS_id="fixture-blas",LAPACK_id="fixture-lapack",
    native_abi="fixture-abi",numeric_equivalence_class="toolchain_bound")
  toolchain_id <- hash("stpd-toolchain-id-v1",canonical(toolchain_core))
  tables$toolchain_manifest <- data.frame(toolchain_id=toolchain_id,
    os=toolchain_core$os,arch=toolchain_core$arch,
    R_version=toolchain_core$R_version,compiler_id=toolchain_core$compiler_id,
    compiler_version=toolchain_core$compiler_version,
    BLAS_id=toolchain_core$BLAS_id,LAPACK_id=toolchain_core$LAPACK_id,
    native_abi=toolchain_core$native_abi,
    numeric_equivalence_class=toolchain_core$numeric_equivalence_class,
    stringsAsFactors=FALSE)
  policy_registry_id <- rid("operational_policy","boundary_precedence_v1")
  tables$detector_policy_manifest <- data.frame(
    policy_path="detector.boundary_precedence",value_type="string",
    canonical_value_hash=hash("stpd-requested-param-value-v1",canonical(list(
      value_type="string",value="boundary_precedence_v1"))),
    governing_registry_entry_id=policy_registry_id,stringsAsFactors=FALSE)
  tables$qc_input_manifest <- data.frame(train=train,
    qc_input_schema="stpd-qc-input-v1",
    canonical_qc_input_hash=hash("stpd-qc-input-row-v1",canonical(list(
      train=train,qc_status="not_available"))),stringsAsFactors=FALSE)
  algorithm_id <- rid("threshold_algorithm","fixed_preregistered_v1")
  algorithm_sha <- semantic_hash(algorithm_id)
  policy_parameter_hash <- hash("stpd-threshold-policy-parameters-v1",
                                canonical(list()))
  policy_source <- data.frame(path=threshold_specs$path,
    units=threshold_specs$units,
    threshold_source_class="fixed_preregistered",adaptation_unit="none",
    label_access="none",threshold_algorithm_semantic_sha256=algorithm_sha,
    policy_parameter_hash=policy_parameter_hash,
    requested_value_hash=threshold_specs$requested_value_hash,
    frozen_before_validation=TRUE,partition_id=NA_character_,
    policy_source_row_hash=NA_character_,stringsAsFactors=FALSE)
  policy_source$policy_source_row_hash <- vapply(seq_len(nrow(policy_source)),
    function(i) {
      payload <- as.list(policy_source[i,setdiff(names(policy_source),
        "policy_source_row_hash"),drop=FALSE])
      hash("stpd-threshold-policy-source-row-v1",canonical(payload))
    },character(1))
  policy_source <- stpd_gate_b_v3_sort_table(policy_source,unlist(
    bundle$tables$threshold_policy_source_manifest$sort_key,use.names=FALSE))
  tables$threshold_policy_source_manifest <- policy_source
  app_rows <- lapply(seq_len(nrow(tables$normalized_input_manifest)),function(i) {
    row <- as.list(tables$normalized_input_manifest[i,,drop=FALSE])
    lapply(row,function(value) if(length(value)==1L&&is.na(value))NULL else value)
  })
  application_hash <- hash("stpd-threshold-application-input-v1",
                           canonical(app_rows))
  derivation_hash <- hash("stpd-threshold-derivation-input-v1",canonical(list(
    threshold_source_class="fixed_preregistered",
    authorized_derivation_rows=list())))
  source_scope_key <- hash("stpd-global-source-scope-v1",canonical(list(
    application_input_manifest_hash=application_hash)))
  source_specs <- threshold_specs[match(policy_source$path,
                                        threshold_specs$path),,drop=FALSE]
  effective_hashes <- vapply(seq_len(nrow(source_specs)),function(i) hash(
    "stpd-effective-threshold-value-v1",canonical(list(
      value_type="double",value_num=source_specs$value[[i]],value_chr=NULL,
      units=source_specs$units[[i]]))),character(1))
  evidence_source_hashes <- vapply(seq_len(nrow(source_specs)),function(i) hash(
    "stpd-threshold-instance-evidence-source-v1",canonical(list(
      threshold_algorithm_semantic_sha256=algorithm_sha,
      policy_source_row_hash=policy_source$policy_source_row_hash[[i]],
      path=source_specs$path[[i]],application_scope="global",
      source_scope_key_hash=source_scope_key,
      derivation_input_manifest_hash=derivation_hash,
      application_input_manifest_hash=application_hash,
      derivation_statistics_schema_registry_id=NULL,
      derivation_statistics_json=NULL,
      application_statistics_schema_registry_id=NULL,
      application_statistics_json=NULL,
      effective_value_hash=effective_hashes[[i]]))),character(1))
  tables$threshold_instance_source_manifest <- data.frame(
    policy_source_row_hash=policy_source$policy_source_row_hash,
    path=source_specs$path,application_scope="global",
    source_scope_key_hash=source_scope_key,train=NA_character_,
    session_group_id_hash=NA_character_,dataset_batch_id_hash=NA_character_,
    derivation_input_manifest_hash=derivation_hash,
    application_input_manifest_hash=application_hash,
    derivation_statistics_schema_registry_id=NA_character_,
    derivation_statistics_json=NA_character_,
    application_statistics_schema_registry_id=NA_character_,
    application_statistics_json=NA_character_,value_type="double",
    value_num=source_specs$value,value_chr=NA_character_,units=source_specs$units,
    effective_value_hash=effective_hashes,
    instance_evidence_source_hash=evidence_source_hashes,
    stringsAsFactors=FALSE)
  tables$threshold_instance_source_manifest <- stpd_gate_b_v3_sort_table(
    tables$threshold_instance_source_manifest,unlist(
      bundle$tables$threshold_instance_source_manifest$sort_key,
      use.names=FALSE))
  tables$partition_membership_source_manifest <-
    stpd_gate_b_v3_empty_table("partition_membership_source_manifest",bundle)
  # Root-free source leaves are now complete; derive the sole detection root.
  leaf <- function(name,domain) hash(domain,rawToChar(
    stpd_gate_b_v3_canonical_table_bytes(tables[[name]],name,bundle)))
  source_hash <- leaf("source_manifest","stpd-source-manifest-v1")
  native_hash <- leaf("native_manifest","stpd-native-manifest-v1")
  toolchain_hash <- leaf("toolchain_manifest","stpd-toolchain-manifest-v1")
  package_version <- tryCatch(as.character(utils::packageVersion(
    "SpikeTrainPatternDetector")),error=function(e) as.character(
      read.dcf("DESCRIPTION",fields="Version")[[1L]]))
  code_identity_hash <- hash("stpd-code-identity-v1",canonical(list(
    source_manifest_hash=source_hash,native_manifest_hash=native_hash,
    package_version=package_version,toolchain_manifest_hash=toolchain_hash)))
  source_context_hash <- hash("stpd-detection-root-v1",canonical(list(
    normalized_input_manifest_hash=leaf("normalized_input_manifest",
      "stpd-normalized-input-manifest-v1"),
    requested_params_hash=leaf("requested_params_manifest",
      "stpd-requested-params-v1"),
    threshold_policy_source_manifest_hash=leaf(
      "threshold_policy_source_manifest","stpd-threshold-policy-source-v1"),
    threshold_instance_source_manifest_hash=leaf(
      "threshold_instance_source_manifest","stpd-threshold-instance-source-v1"),
    partition_membership_manifest_hash=leaf(
      "partition_membership_source_manifest","stpd-partition-membership-v1"),
    qc_input_manifest_hash=leaf("qc_input_manifest","stpd-qc-input-v1"),
    code_identity_hash=code_identity_hash,
    detector_policy_hash=leaf("detector_policy_manifest",
      "stpd-detector-policy-manifest-v1"),
    label_blind_contract_hash=leaf("label_blind_contract_manifest",
      "stpd-label-blind-contract-manifest-v1"))))
  root <- paste0("dr_",source_context_hash)
  policy_ids <- vapply(seq_len(nrow(source_specs)),function(i)
    stpd_gate_b_v3_entity_id("threshold_policy",list(
      detection_root_id=root,path=source_specs$path[[i]],
      threshold_source_class="fixed_preregistered",adaptation_unit="none",
      label_access="none",policy_parameter_hash=policy_parameter_hash,
      partition_id=NULL),bundle),character(1))
  tables$threshold_policies <- data.frame(schema_version="stpd_multitrack_v3_1",
    detection_root_id=root,threshold_policy_id=policy_ids,
    path=source_specs$path,units=source_specs$units,
    threshold_source_class="fixed_preregistered",adaptation_unit="none",
    label_access="none",threshold_algorithm_registry_id=algorithm_id,
    policy_parameter_hash=policy_parameter_hash,
    requested_value_hash=source_specs$requested_value_hash,
    frozen_before_validation=TRUE,partition_id=NA_character_,
    stringsAsFactors=FALSE)
  tables$threshold_policies <- stpd_gate_b_v3_sort_table(
    tables$threshold_policies,unlist(
      bundle$tables$threshold_policies$sort_key,use.names=FALSE))
  scope_key <- hash("stpd-materialized-threshold-scope-v1",canonical(list(
    detection_root_id=root,application_scope="global",
    source_scope_key_hash=source_scope_key)))
  evidence_schema_id <- rid("evidence_schema","application_statistics_v1")
  evidence_type_id <- rid("evidence_type")
  evidence_payload <- list(statistics_schema="threshold_instance_source_v1",
                           statistics=list())
  evidence_json <- canonical(evidence_payload)
  evidence_json_hash <- hash("stpd-evidence-json-v1",canonical(list(
    evidence_schema_registry_id=evidence_schema_id,
    evidence_json=evidence_payload)))
  evidence_ids <- vapply(seq_len(nrow(source_specs)),function(i)
    stpd_gate_b_v3_entity_id("evidence",list(
      detection_root_id=root,evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=evidence_schema_id,
      source_bytes_sha256=evidence_source_hashes[[i]],
      evidence_json_hash=evidence_json_hash),bundle),character(1))
  policy_ids_by_path <- setNames(
    tables$threshold_policies$threshold_policy_id,
    tables$threshold_policies$path)
  instance_ids <- vapply(seq_len(nrow(source_specs)),function(i)
    stpd_gate_b_v3_entity_id("threshold_instance",list(
      detection_root_id=root,
      threshold_policy_id=policy_ids_by_path[[source_specs$path[[i]]]],
      path=source_specs$path[[i]],application_scope="global",
      application_scope_key_hash=scope_key,
      effective_value_hash=effective_hashes[[i]]),bundle),character(1))
  tables$threshold_instances <- data.frame(
    schema_version="stpd_multitrack_v3_1",detection_root_id=root,
    threshold_instance_id=instance_ids,
    threshold_policy_id=unname(policy_ids_by_path[source_specs$path]),
    path=source_specs$path,application_scope="global",
    source_scope_key_hash=source_scope_key,application_scope_key_hash=scope_key,
    train=NA_character_,session_group_id_hash=NA_character_,
    dataset_batch_id_hash=NA_character_,
    derivation_input_manifest_hash=derivation_hash,
    application_input_manifest_hash=application_hash,value_type="double",
    value_num=source_specs$value,value_chr=NA_character_,units=source_specs$units,
    effective_value_hash=effective_hashes,instance_evidence_id=evidence_ids,
    stringsAsFactors=FALSE)
  tables$threshold_instances <- stpd_gate_b_v3_sort_table(
    tables$threshold_instances,unlist(
      bundle$tables$threshold_instances$sort_key,use.names=FALSE))
  tables$evidence_records <- data.frame(schema_version="stpd_multitrack_v3_1",
    detection_root_id=root,evidence_id=evidence_ids,
    evidence_type_registry_id=evidence_type_id,
    evidence_schema_registry_id=evidence_schema_id,evidence_json=evidence_json,
    evidence_json_hash=evidence_json_hash,
    source_bytes_sha256=evidence_source_hashes,
    label_access="none",created_by_registry_entry_id=algorithm_id,
    stringsAsFactors=FALSE)
  tables$evidence_records <- stpd_gate_b_v3_sort_table(
    tables$evidence_records,unlist(
      bundle$tables$evidence_records$sort_key,use.names=FALSE))
  instance_ids_by_path <- setNames(
    tables$threshold_instances$threshold_instance_id,
    tables$threshold_instances$path)
  evidence_ids_by_path <- setNames(
    tables$threshold_instances$instance_evidence_id,
    tables$threshold_instances$path)
  state_threshold_evidence_id <-
    evidence_ids_by_path[["state.frequency.high_enter_hz"]]
  event_threshold_evidence_id <-
    evidence_ids_by_path[["event.selector.minimum_score"]]
  pause_threshold_evidence_id <-
    evidence_ids_by_path[["pause.absolute_threshold_sec"]]
  event_instance_id <- instance_ids_by_path[["event.selector.minimum_score"]]
  context_id <- function(domain) rid(domain)
  context_core <- list(train=train,patient_group_id_hash=NULL,
    session_group_id_hash=NULL,unit_id_hash=strrep("1",64L),
    species_registry_id=context_id("species"),
    brain_region_registry_id=context_id("brain_region"),
    brain_subregion_registry_id=NULL,
    acquisition_condition_registry_id=context_id("acquisition_condition"),
    anesthesia_status_registry_id=context_id("anesthesia_status"),
    medication_status_registry_id=context_id("medication_status"),
    task_status_registry_id=context_id("task_status"),
    recording_duration_sec=if (isTRUE(with_overlap)) max(timestamps) else 0,
    timestamp_resolution_sec=1e-6,
    timebase_provenance_registry_id=context_id("timebase_provenance"),
    waveform_qc_available=FALSE,raw_waveform_available=FALSE,
    spike_sorting_method_registry_id=context_id("spike_sorting_method"),
    spike_sorting_version="not_available",
    sorting_qc_status_registry_id=context_id("sorting_qc_status"),
    isolation_metric_name_registry_id=NULL,isolation_metric_value=NULL,
    isolation_metric_units_registry_id=NULL,
    nonstationarity_status_registry_id=context_id("nonstationarity_status"),
    condition_boundary_available=FALSE,
    context_completeness="minimal_timestamp_only")
  context_hash <- hash("stpd-scientific-context-row-v1",canonical(context_core))
  tables$scientific_context <- data.frame(schema_version="stpd_multitrack_v3_1",
    detection_root_id=root,train=train,patient_group_id_hash=NA_character_,
    session_group_id_hash=NA_character_,unit_id_hash=strrep("1",64L),
    species_registry_id=context_core$species_registry_id,
    brain_region_registry_id=context_core$brain_region_registry_id,
    brain_subregion_registry_id=NA_character_,
    acquisition_condition_registry_id=context_core$acquisition_condition_registry_id,
    anesthesia_status_registry_id=context_core$anesthesia_status_registry_id,
    medication_status_registry_id=context_core$medication_status_registry_id,
    task_status_registry_id=context_core$task_status_registry_id,
    recording_duration_sec=context_core$recording_duration_sec,
    timestamp_resolution_sec=1e-6,
    timebase_provenance_registry_id=context_core$timebase_provenance_registry_id,
    waveform_qc_available=FALSE,raw_waveform_available=FALSE,
    spike_sorting_method_registry_id=context_core$spike_sorting_method_registry_id,
    spike_sorting_version="not_available",
    sorting_qc_status_registry_id=context_core$sorting_qc_status_registry_id,
    isolation_metric_name_registry_id=NA_character_,isolation_metric_value=NA_real_,
    isolation_metric_units_registry_id=NA_character_,
    nonstationarity_status_registry_id=context_core$nonstationarity_status_registry_id,
    condition_boundary_available=FALSE,
    context_completeness="minimal_timestamp_only",
    source_context_row_hash=context_hash,stringsAsFactors=FALSE)
  entity_domain_id <- function(domain) {
    hit <- tables$entity_domain_registry$entity_domain==domain
    if (sum(hit)!=1L) stop("Phase-1 fixture entity domain is missing.",
                           call.=FALSE)
    tables$entity_domain_registry$entity_domain_id[hit]
  }
  if (isTRUE(with_overlap)) {
    state_rule <- rid("state_candidate_rule")
    state_classification <- rid("classification_rule")
    segment_rule <- rid("segment_rule")
    event_rule <- rid("event_candidate_rule",
                      "structure_first_local_flank_contrast_v1")
    frequency_estimator <- rid("estimator","state_frequency_rate_v1")
    regularity_estimator <- rid("estimator","state_regularity_median_cv2_v1")
    event_extent_estimator <- rid("estimator","event_extent_n_spikes_v1")
    event_frequency_estimator <- rid("estimator","event_frequency_rate_hz_v1")
    episode_rule <- rid("episode_aggregation")
    reason_accepted <- rid("reason_code","accepted_evidence_pass_v1")
    source_candidate_hash <- hash("stpd-phase1-overlap-source-v1",train)
    fixture_isi <- diff(timestamps)
    state_isi <- fixture_isi[1:3]
    fixture_duration <- stpd_gate_b_v3_sum_binary64(state_isi)
    fixture_frequency <- length(state_isi)/fixture_duration
    fixture_regularity <- stats::median(
      2*abs(diff(state_isi))/(state_isi[-length(state_isi)]+
        state_isi[-1L]))
    fixture_frequency_class <- if (fixture_frequency>=50) "high" else
      if (fixture_frequency<=45) "non_high" else "unresolved"
    fixture_regularity_class <- if (fixture_regularity<=0.5) "regular" else
      if (fixture_regularity>=0.8) "irregular" else "unresolved"
    fixture_state_class <- switch(paste(fixture_frequency_class,
                                        fixture_regularity_class,sep="\u001f"),
      "high\u001firregular"="high_frequency_irregular_state",
      "high\u001fregular"="high_frequency_tonic",
      "non_high\u001fregular"="tonic",
      stop("Minimal Gate B v3 fixture profile is unresolved.",call.=FALSE))
    fixture_end <- timestamps[[4L]]
    state_candidate_id <- stpd_gate_b_v3_entity_id("state_candidate",list(
      detection_root_id=root,train=train,start_isi=1L,end_isi=3L,
      candidate_generation_rule_registry_id=state_rule,
      source_candidate_key_hash=source_candidate_hash),bundle)
    axis_payload_hash <- strrep("0",64L)
    axis_id <- stpd_gate_b_v3_entity_id("state_axis_evidence",list(
      detection_root_id=root,state_candidate_id=state_candidate_id,
      start_isi=1L,end_isi=3L,evidence_payload_hash=axis_payload_hash),bundle)
    tables$state_axis_evidence <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      state_axis_evidence_id=axis_id,train=train,
      state_candidate_id=state_candidate_id,start_isi=1L,end_isi=3L,
      n_valid_isi=3L,effective_duration_sec=fixture_duration,
      frequency_estimator_registry_id=frequency_estimator,
      frequency_stat=fixture_frequency,
      frequency_pass_bound=if(fixture_frequency_class=="non_high")45 else 50,
      frequency_gray_margin=5,
      frequency_evaluable=TRUE,frequency_evidence_status="pass",
      regularity_estimator_registry_id=regularity_estimator,
      regularity_stat=fixture_regularity,
      regularity_pass_bound=if(fixture_regularity_class=="regular")0.5 else 0.8,
      regularity_gray_margin=0.8-0.5,
      regularity_evaluable=TRUE,regularity_evidence_status="pass",
      episode_aggregation_registry_id=episode_rule,
      insufficient_reason_registry_id=NA_character_,
      evidence_payload_hash=axis_payload_hash,stringsAsFactors=FALSE)
    axis_payload_hash <- hash("stpd-state-axis-evidence-payload-v1",canonical(
      stpd_gate_b_v3_scalar_row_payload(tables$state_axis_evidence,c(
        "schema_version","detection_root_id","state_axis_evidence_id",
        "state_candidate_id","evidence_payload_hash"))))
    tables$state_axis_evidence$evidence_payload_hash <- axis_payload_hash
    axis_id <- stpd_gate_b_v3_entity_id("state_axis_evidence",list(
      detection_root_id=root,state_candidate_id=state_candidate_id,
      start_isi=1L,end_isi=3L,evidence_payload_hash=axis_payload_hash),bundle)
    tables$state_axis_evidence$state_axis_evidence_id <- axis_id
    state_axis_schema_id <- rid("evidence_schema","state_axis_v1")
    state_paths <- c(
      "state.frequency.high_enter_hz",
      "state.frequency.high_exit_hz",
      "state.regularity.regular_upper_cv2",
      "state.regularity.irregular_lower_cv2",
      "state.state_min_valid_isi",
      "state.state_min_duration_sec")
    state_threshold_instance_ids <- sort(
      unname(instance_ids_by_path[state_paths]),method="radix")
    state_axis_record_payload <- list(
      frequency_stat=fixture_frequency,frequency_status="pass",
      regularity_stat=fixture_regularity,regularity_status="pass",
      threshold_instance_ids=as.list(state_threshold_instance_ids))
    state_axis_record_json <- canonical(state_axis_record_payload)
    state_axis_record_json_hash <- hash("stpd-evidence-json-v1",canonical(list(
      evidence_schema_registry_id=state_axis_schema_id,
      evidence_json=state_axis_record_payload)))
    state_axis_record_source_hash <- hash("stpd-state-axis-record-source-v1",
      canonical(list(state_candidate_id=state_candidate_id,
        state_axis_evidence_id=axis_id,
        evidence_payload_hash=axis_payload_hash)))
    state_axis_record_id <- stpd_gate_b_v3_entity_id("evidence",list(
      detection_root_id=root,evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=state_axis_schema_id,
      source_bytes_sha256=state_axis_record_source_hash,
      evidence_json_hash=state_axis_record_json_hash),bundle)
    tables$evidence_records <- rbind(tables$evidence_records,data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      evidence_id=state_axis_record_id,
      evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=state_axis_schema_id,
      evidence_json=state_axis_record_json,
      evidence_json_hash=state_axis_record_json_hash,
      source_bytes_sha256=state_axis_record_source_hash,label_access="none",
      created_by_registry_entry_id=state_classification,
      stringsAsFactors=FALSE))
    tables$state_candidates <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      source_context_hash=source_context_hash,train=train,
      state_candidate_id=state_candidate_id,
      candidate_generation_rule_registry_id=state_rule,
      source_candidate_key_hash=source_candidate_hash,start_isi=1L,end_isi=3L,
      n_isi=3L,n_spikes=4L,start_time_sec=0,end_time_sec=fixture_end,
      duration_sec=fixture_end,candidate_decision="accepted_direct_support",
      state_axis_evidence_id=axis_id,
      decision_reason_registry_id=reason_accepted,
      lineage_id=NA_character_,stringsAsFactors=FALSE)
    state_episode_id <- stpd_gate_b_v3_entity_id("state_episode",list(
      detection_root_id=root,train=train,
      state_class=fixture_state_class,start_isi=1L,end_isi=3L,
      classification_evidence_id=state_axis_record_id),bundle)
    tables$state_episodes <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      source_context_hash=source_context_hash,train=train,
      state_episode_id=state_episode_id,state_class=fixture_state_class,
      state_frequency_class=fixture_frequency_class,
      state_regularity_class=fixture_regularity_class,
      frequency_evidence_status="pass",regularity_evidence_status="pass",
      classification_rule_registry_id=state_classification,start_isi=1L,
      end_isi=3L,n_isi=3L,n_spikes=4L,start_time_sec=0,
      end_time_sec=fixture_end,envelope_duration_sec=fixture_end,
      direct_support_n_isi=3L,connector_n_isi=0L,
      direct_support_duration_sec=fixture_end,connector_duration_sec=0,
      support_fraction=1,classification_evidence_id=state_axis_record_id,
      record_status="predicted",stringsAsFactors=FALSE)
    state_segment_id <- stpd_gate_b_v3_entity_id("state_segment",list(
      detection_root_id=root,state_episode_id=state_episode_id,segment_index=1L,
      segment_role="direct_support",start_isi=1L,end_isi=3L,
      source_state_candidate_id=state_candidate_id,state_axis_evidence_id=axis_id,
      connector_decision_id=NULL),bundle)
    tables$state_segments <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      source_context_hash=source_context_hash,train=train,
      state_segment_id=state_segment_id,state_episode_id=state_episode_id,
      segment_index=1L,segment_role="direct_support",
      state_class=fixture_state_class,
      state_frequency_class=fixture_frequency_class,
      state_regularity_class=fixture_regularity_class,
      start_isi=1L,end_isi=3L,n_isi=3L,n_spikes=4L,start_time_sec=0,
      end_time_sec=fixture_end,duration_sec=fixture_end,
      segment_rule_registry_id=segment_rule,
      source_state_candidate_id=state_candidate_id,state_axis_evidence_id=axis_id,
      connector_decision_id=NA_character_,lineage_id=NA_character_,
      stringsAsFactors=FALSE)
    event_source_candidate_hash <- hash(
      "stpd-structure-first-source-candidate-key-v1",canonical(list(
        train=train,start_isi=1L,end_isi=3L,
        candidate_generation_rule_registry_id=event_rule)))
    event_candidate_id <- stpd_gate_b_v3_entity_id("event_candidate",list(
      detection_root_id=root,train=train,start_isi=1L,end_isi=3L,
      candidate_generation_rule_registry_id=event_rule,
      source_candidate_key_hash=event_source_candidate_hash),bundle)
    event_support_schema_id <- rid("evidence_schema","entity_support_v1")
    event_candidate_domain_id <- entity_domain_id("event_candidate")
    event_support_payload <- list(
      entity_domain_id=event_candidate_domain_id,
      entity_id=event_candidate_id,
      support_rows=as.list(c(1L,2L,3L)))
    event_support_json <- canonical(event_support_payload)
    event_support_json_hash <- hash("stpd-evidence-json-v1",canonical(list(
      evidence_schema_registry_id=event_support_schema_id,
      evidence_json=event_support_payload)))
    event_support_source_hash <- hash("stpd-event-support-source-v1",canonical(
      list(event_candidate_id=event_candidate_id,train=train,
        start_isi=1L,end_isi=3L)))
    event_support_evidence_id <- stpd_gate_b_v3_entity_id("evidence",list(
      detection_root_id=root,evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=event_support_schema_id,
      source_bytes_sha256=event_support_source_hash,
      evidence_json_hash=event_support_json_hash),bundle)
    tables$evidence_records <- rbind(tables$evidence_records,data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      evidence_id=event_support_evidence_id,
      evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=event_support_schema_id,
      evidence_json=event_support_json,
      evidence_json_hash=event_support_json_hash,
      source_bytes_sha256=event_support_source_hash,label_access="none",
      created_by_registry_entry_id=event_rule,stringsAsFactors=FALSE))
    tables$event_candidates <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      source_context_hash=source_context_hash,train=train,
      event_candidate_id=event_candidate_id,
      candidate_generation_rule_registry_id=event_rule,
      source_candidate_key_hash=event_source_candidate_hash,candidate_class="burst_candidate",
      start_isi=1L,end_isi=3L,n_isi=3L,n_spikes=4L,start_time_sec=0,
      end_time_sec=fixture_end,duration_sec=fixture_end,
      candidate_decision="accepted_event",
      decision_reason_registry_id=reason_accepted,
      base_event_evidence_id=event_support_evidence_id,
      lineage_id=NA_character_,stringsAsFactors=FALSE)
    event_id <- stpd_gate_b_v3_entity_id("event",list(
      detection_root_id=root,train=train,event_class="burst_event",
      start_isi=1L,end_isi=3L,
      base_event_evidence_id=event_support_evidence_id),bundle)
    modifier_schema_id <- rid("evidence_schema","event_modifier_v1")
    modifier_bundle <- function(domain,value,estimator_id,statistic_value,
                                pass_bound,gray_margin,threshold_path,
                                bound_paths) {
      threshold_instance_id <- instance_ids_by_path[[threshold_path]]
      modifier_payload <- list(event_id=event_id,modifier_domain=domain,
        modifier_value=value,evidence_status="pass",
        estimator_registry_id=estimator_id,
        statistic_value=as.double(statistic_value),
        pass_bound=as.double(pass_bound),gray_margin=as.double(gray_margin),
        threshold_instance_id=threshold_instance_id)
      modifier_json <- canonical(modifier_payload)
      modifier_json_hash <- hash("stpd-evidence-json-v1",canonical(list(
        evidence_schema_registry_id=modifier_schema_id,
        evidence_json=modifier_payload)))
      modifier_source_hash <- hash("stpd-event-modifier-source-v1",
        canonical(modifier_payload))
      modifier_record_id <- stpd_gate_b_v3_entity_id("evidence",list(
        detection_root_id=root,evidence_type_registry_id=evidence_type_id,
        evidence_schema_registry_id=modifier_schema_id,
        source_bytes_sha256=modifier_source_hash,
        evidence_json_hash=modifier_json_hash),bundle)
      modifier_id <- stpd_gate_b_v3_entity_id("event_modifier_evidence",list(
        detection_root_id=root,event_id=event_id,modifier_domain=domain,
        evidence_id=modifier_record_id),bundle)
      list(record=data.frame(schema_version="stpd_multitrack_v3_1",
        detection_root_id=root,evidence_id=modifier_record_id,
        evidence_type_registry_id=evidence_type_id,
        evidence_schema_registry_id=modifier_schema_id,
        evidence_json=modifier_json,evidence_json_hash=modifier_json_hash,
        source_bytes_sha256=modifier_source_hash,label_access="none",
        created_by_registry_entry_id=estimator_id,stringsAsFactors=FALSE),
        modifier=data.frame(schema_version="stpd_multitrack_v3_1",
          detection_root_id=root,modifier_evidence_id=modifier_id,
          event_id=event_id,modifier_domain=domain,modifier_value=value,
          evidence_status="pass",estimator_registry_id=estimator_id,
          statistic_value=as.double(statistic_value),
          pass_bound=as.double(pass_bound),gray_margin=as.double(gray_margin),
          threshold_instance_id=threshold_instance_id,
          evidence_id=modifier_record_id,stringsAsFactors=FALSE),
        bound_paths=bound_paths)
    }
    event_frequency_value <- if (fixture_frequency>=150) "high" else
      if (fixture_frequency<=100) "non_high" else "unresolved"
    event_frequency_bound <- if (event_frequency_value=="non_high") 100 else 150
    event_frequency_threshold <- if (event_frequency_value=="non_high")
      "event.frequency.high_exit_hz" else "event.frequency.high_enter_hz"
    event_frequency_status <- if (event_frequency_value=="unresolved")
      "gray_zone" else "pass"
    modifier_bundles <- list(
      modifier_bundle("extent","classic",event_extent_estimator,4,5,0,
        "event.extent.long_min_spikes",
        c("event.extent.long_min_spikes",
          "event.extent.prolonged_min_spikes")),
      modifier_bundle("frequency",event_frequency_value,
        event_frequency_estimator,fixture_frequency,
        event_frequency_bound,50,event_frequency_threshold,
        c("event.frequency.high_enter_hz",
          "event.frequency.high_exit_hz")))
    tables$evidence_records <- rbind(tables$evidence_records,
      do.call(rbind,lapply(modifier_bundles,`[[`,"record")))
    tables$event_modifier_evidence <- do.call(rbind,
      lapply(modifier_bundles,`[[`,"modifier"))
    extent_id <- tables$event_modifier_evidence$modifier_evidence_id[
      tables$event_modifier_evidence$modifier_domain=="extent"]
    frequency_id <- tables$event_modifier_evidence$modifier_evidence_id[
      tables$event_modifier_evidence$modifier_domain=="frequency"]
    tables$events <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      source_context_hash=source_context_hash,train=train,event_id=event_id,
      event_class="burst_event",extent_modifier="classic",
      frequency_modifier=event_frequency_value,
      base_event_evidence_id=event_support_evidence_id,
      extent_evidence_status="pass",extent_modifier_evidence_id=extent_id,
      frequency_evidence_status=event_frequency_status,
      frequency_modifier_evidence_id=frequency_id,
      start_isi=1L,end_isi=3L,n_isi=3L,n_spikes=4L,start_time_sec=0,
      end_time_sec=fixture_end,duration_sec=fixture_end,
      record_status="predicted",
      lineage_id=NA_character_,stringsAsFactors=FALSE)
    relationship_id <- stpd_gate_b_v3_entity_id("event_state_relationship",list(
      detection_root_id=root,event_id=event_id,
      state_episode_id=state_episode_id),bundle)
    tables$event_state_relationships <- data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      source_context_hash=source_context_hash,train=train,
      relationship_id=relationship_id,event_id=event_id,
      state_episode_id=state_episode_id,
      relation_type="event_overlaps_state_episode",
      episode_overlap_n_isi=3L,direct_support_overlap_n_isi=3L,
      connector_overlap_n_isi=0L,episode_overlap_sec=fixture_end,
      direct_support_overlap_sec=fixture_end,connector_overlap_sec=0,
      event_covered_by_episode_time_fraction=1,
      event_covered_by_direct_support_time_fraction=1,
      state_envelope_covered_by_event_time_fraction=1,
      state_direct_support_covered_by_event_time_fraction=1,
      event_covered_by_episode_isi_fraction=1,
      event_covered_by_direct_support_isi_fraction=1,
      state_envelope_covered_by_event_isi_fraction=1,
      state_direct_support_covered_by_event_isi_fraction=1,
      stringsAsFactors=FALSE)
    spine_n <- length(fixture_isi)
    tables$per_isi <- data.frame(
      schema_version=rep("stpd_multitrack_v3_1",spine_n),
      detection_root_id=rep(root,spine_n),
      source_context_hash=rep(source_context_hash,spine_n),
      train=rep(train,spine_n),isi_index=seq_len(spine_n),
      left_spike_index=seq_len(spine_n),
      right_spike_index=seq.int(2L,spine_n+1L),
      isi_start_time_sec=timestamps[seq_len(spine_n)],
      isi_end_time_sec=timestamps[seq.int(2L,spine_n+1L)],isi_sec=fixture_isi,
      state_episode_id=c(rep(state_episode_id,3L),rep(NA_character_,spine_n-3L)),
      episode_state_class=c(rep(fixture_state_class,3L),rep(NA_character_,spine_n-3L)),
      state_episode_membership=c(rep(TRUE,3L),rep(FALSE,spine_n-3L)),
      state_segment_id=c(rep(state_segment_id,3L),rep(NA_character_,spine_n-3L)),
      state_segment_role=c(rep("direct_support",3L),rep(NA_character_,spine_n-3L)),
      active_state_segment_id=c(rep(state_segment_id,3L),rep(NA_character_,spine_n-3L)),
      active_state_class=c(rep(fixture_state_class,3L),rep(NA_character_,spine_n-3L)),
      state_direct_support=c(rep(TRUE,3L),rep(FALSE,spine_n-3L)),
      event_id=c(rep(event_id,3L),rep(NA_character_,spine_n-3L)),
      gap_id=rep(NA_character_,spine_n),
      controlling_boundary_id=rep(NA_character_,spine_n),
      stringsAsFactors=FALSE)
    # The Gap candidate table is a complete, label-blind, single-ISI universe.
    # No candidate is a canonical Pause in this compact fixture, but all rows
    # remain materialized so deletion cannot turn a negative example into an
    # apparently complete product.
    gap_policy_id <- rid("gap_policy","pause_local_empirical_bh_v1")
    gap_estimator_id <- rid("estimator","pause_local_window_median_v1")
    gap_method_id <- rid("multiple_testing_method","benjamini_hochberg_v1")
    gap_reject_reason_id <- rid("reason_code","rejected_pause_threshold_v1")
    gap_ids <- vapply(seq_len(spine_n),function(index) stpd_gate_b_v3_entity_id(
      "gap_candidate",list(detection_root_id=root,train=train,
        start_isi=as.integer(index),end_isi=as.integer(index),
        gap_policy_registry_id=gap_policy_id),bundle),character(1))
    gap_family_id <- stpd_gate_b_v3_entity_id("multiple_testing_family",list(
      detection_root_id=root,train=train,
      gap_policy_registry_id=gap_policy_id,
      scientific_context_epoch_id=NULL,
      sorted_gap_candidate_ids=as.list(sort(gap_ids,method="radix"))),bundle)
    gap_durations <- fixture_isi
    reference_sets <- lapply(seq_len(spine_n),function(index) {
      local <- seq.int(max(1L,index-2L),min(spine_n,index+2L))
      setdiff(local,index)
    })
    gap_references <- vapply(reference_sets,function(index)
      stats::median(gap_durations[index]),numeric(1))
    gap_ratios <- gap_durations/gap_references
    gap_raw_p <- vapply(seq_len(spine_n),function(index) {
      reference <- gap_durations[reference_sets[[index]]]
      (1+sum(reference>=gap_durations[[index]]))/(length(reference)+1)
    },numeric(1))
    gap_adjusted_q <- stats::p.adjust(gap_raw_p,method="BH")
    gap_evidence_order <- order(gap_ids,method="radix")
    gap_schema_id <- rid("evidence_schema","gap_empirical_family_v1")
    gap_qc_schema_id <- rid("evidence_schema","qc_eligibility_v1")
    gap_qc_bundles <- lapply(seq_len(spine_n),function(index) {
      payload <- list(hard_boundary_clear=TRUE,isi_index=as.integer(index),
        qc_eligibility="timestamp_only_uncertain",train=train)
      evidence_json <- canonical(payload)
      evidence_json_hash <- hash("stpd-evidence-json-v1",canonical(list(
        evidence_schema_registry_id=gap_qc_schema_id,
        evidence_json=payload)))
      source_hash <- hash("stpd-qc-eligibility-source-v1",evidence_json)
      evidence_id <- stpd_gate_b_v3_entity_id("evidence",list(
        detection_root_id=root,evidence_type_registry_id=evidence_type_id,
        evidence_schema_registry_id=gap_qc_schema_id,
        source_bytes_sha256=source_hash,
        evidence_json_hash=evidence_json_hash),bundle)
      list(id=evidence_id,record=data.frame(
        schema_version="stpd_multitrack_v3_1",detection_root_id=root,
        evidence_id=evidence_id,evidence_type_registry_id=evidence_type_id,
        evidence_schema_registry_id=gap_qc_schema_id,
        evidence_json=evidence_json,evidence_json_hash=evidence_json_hash,
        source_bytes_sha256=source_hash,label_access="none",
        created_by_registry_entry_id=gap_policy_id,stringsAsFactors=FALSE))
    })
    gap_payload <- list(candidate_ids=as.list(gap_ids[gap_evidence_order]),
      raw_p_values=as.list(gap_raw_p[gap_evidence_order]),
      adjusted_q_values=as.list(gap_adjusted_q[gap_evidence_order]),
      method_registry_id=gap_method_id)
    gap_evidence_json <- canonical(gap_payload)
    gap_evidence_json_hash <- hash("stpd-evidence-json-v1",canonical(list(
      evidence_schema_registry_id=gap_schema_id,evidence_json=gap_payload)))
    gap_source_hash <- hash("stpd-gap-empirical-family-source-v1",
      gap_evidence_json)
    gap_evidence_id <- stpd_gate_b_v3_entity_id("evidence",list(
      detection_root_id=root,evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=gap_schema_id,
      source_bytes_sha256=gap_source_hash,
      evidence_json_hash=gap_evidence_json_hash),bundle)
    tables$evidence_records <- rbind(tables$evidence_records,data.frame(
      schema_version="stpd_multitrack_v3_1",detection_root_id=root,
      evidence_id=gap_evidence_id,evidence_type_registry_id=evidence_type_id,
      evidence_schema_registry_id=gap_schema_id,
      evidence_json=gap_evidence_json,evidence_json_hash=gap_evidence_json_hash,
      source_bytes_sha256=gap_source_hash,label_access="none",
      created_by_registry_entry_id=gap_estimator_id,stringsAsFactors=FALSE),
      do.call(rbind,lapply(gap_qc_bundles,`[[`,"record")))
    tables$evidence_records <- stpd_gate_b_v3_sort_table(
      tables$evidence_records,
      unlist(bundle$tables$evidence_records$sort_key,use.names=FALSE))
    tables$gap_candidates <- data.frame(
      schema_version=rep("stpd_multitrack_v3_1",spine_n),
      detection_root_id=rep(root,spine_n),gap_candidate_id=gap_ids,
      train=rep(train,spine_n),start_isi=seq_len(spine_n),
      end_isi=seq_len(spine_n),n_isi=rep(1L,spine_n),
      start_time_sec=timestamps[seq_len(spine_n)],
      end_time_sec=timestamps[seq.int(2L,spine_n+1L)],
      duration_sec=gap_durations,
      reference_window_start_isi=vapply(reference_sets,min,integer(1)),
      reference_window_end_isi=vapply(reference_sets,max,integer(1)),
      guard_band_n_isi=rep(0L,spine_n),
      local_estimator_registry_id=rep(gap_estimator_id,spine_n),
      valid_local_n_isi=vapply(reference_sets,length,integer(1)),
      local_reference_sec=gap_references,
      local_ratio=gap_ratios,raw_p_value=gap_raw_p,
      adjusted_q_value=gap_adjusted_q,
      multiple_testing_family_id=rep(gap_family_id,spine_n),
      multiple_testing_method_registry_id=rep(gap_method_id,spine_n),
      boundary_censored=rep(FALSE,spine_n),
      scientific_context_epoch_id=rep(NA_character_,spine_n),
      qc_evidence_id=vapply(gap_qc_bundles,`[[`,character(1),"id"),
      decision=rep("rejected",spine_n),
      decision_reason_registry_id=rep(gap_reject_reason_id,spine_n),
      gap_policy_registry_id=rep(gap_policy_id,spine_n),
      evidence_id=rep(gap_evidence_id,spine_n),stringsAsFactors=FALSE)
    tables$gap_candidates <- stpd_gate_b_v3_sort_table(
      tables$gap_candidates,
      unlist(bundle$tables$gap_candidates$sort_key,use.names=FALSE))

    binding_role_id <- rid("binding_role","classification_gate_v1")
    decision_binding_role_id <- rid("binding_role","decision_gate_v1")
    binding_rows <- function(domain,ids,paths,role_id) {
      if (length(ids)==0L || length(paths)==0L) {
        return(stpd_gate_b_v3_empty_table("threshold_instance_bindings",bundle))
      }
      rows <- do.call(rbind,lapply(ids,function(consumer_id) do.call(rbind,
        lapply(paths,function(path) {
          consumer_domain_id <- entity_domain_id(domain)
          threshold_instance_id <- instance_ids_by_path[[path]]
          binding_id <- stpd_gate_b_v3_entity_id("threshold_binding",list(
            detection_root_id=root,consumer_domain_id=consumer_domain_id,
            consumer_id=consumer_id,path=path,
            threshold_instance_id=threshold_instance_id,
            binding_role_registry_id=role_id),bundle)
          data.frame(schema_version="stpd_multitrack_v3_1",
            detection_root_id=root,binding_id=binding_id,
            consumer_domain_id=consumer_domain_id,consumer_id=consumer_id,
            path=path,threshold_instance_id=threshold_instance_id,
            binding_role_registry_id=role_id,stringsAsFactors=FALSE)
        }))))
      rows
    }
    event_candidate_paths <- c(
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
    event_paths <- "event.selector.minimum_score"
    pause_paths <- c("pause.scan_seed_sec","pause.absolute_threshold_sec",
      "pause.local_ratio_min","pause.local_window_isi",
      "pause.guard_band_isi","pause.local_min_valid_isi",
      "pause.score_alpha")
    tables$threshold_instance_bindings <- do.call(rbind,list(
      binding_rows("state_candidate",state_candidate_id,state_paths,
        binding_role_id),
      binding_rows("state_axis_evidence",axis_id,state_paths,binding_role_id),
      binding_rows("state_episode",state_episode_id,state_paths,binding_role_id),
      binding_rows("event_candidate",event_candidate_id,event_candidate_paths,
        binding_role_id),
      binding_rows("event",event_id,event_paths,binding_role_id),
      binding_rows("event_modifier_evidence",extent_id,
        c("event.extent.long_min_spikes",
          "event.extent.prolonged_min_spikes"),binding_role_id),
      binding_rows("event_modifier_evidence",frequency_id,
        c("event.frequency.high_enter_hz",
          "event.frequency.high_exit_hz"),binding_role_id),
      binding_rows("gap_candidate",gap_ids,pause_paths,
        decision_binding_role_id)))
    tables$threshold_instance_bindings <- stpd_gate_b_v3_sort_table(
      tables$threshold_instance_bindings,
      unlist(bundle$tables$threshold_instance_bindings$sort_key,
        use.names=FALSE))

    lineage_action_id <- rid("lineage_action","generation_before_parent_v1")
    evidence_domain_id <- entity_domain_id("evidence")
    lineage_for <- function(child_domain,child_id,parent_evidence_id) {
      parent_payload <- list(parent_domain_id=evidence_domain_id,
        parent_id=parent_evidence_id,parent_product_hash=NULL,
        lineage_action_registry_id=lineage_action_id,transition_id=NULL,
        evidence_id=parent_evidence_id)
      edge_hash <- hash("stpd-lineage-parent-edge-set-v1",canonical(list(
        child_domain_id=entity_domain_id(child_domain),child_id=child_id,
        child_product_hash=NULL,sorted_parent_edge_payloads=list(parent_payload))))
      lineage_id <- stpd_gate_b_v3_entity_id("lineage",list(
        detection_root_id=root,child_domain_id=entity_domain_id(child_domain),
        child_id=child_id,parent_edge_set_hash=edge_hash),bundle)
      list(id=lineage_id,row=data.frame(
        schema_version="stpd_multitrack_v3_1",detection_root_id=root,
        lineage_id=lineage_id,child_domain_id=entity_domain_id(child_domain),
        child_id=child_id,child_product_hash=NA_character_,
        parent_domain_id=evidence_domain_id,parent_id=parent_evidence_id,
        parent_product_hash=NA_character_,
        lineage_action_registry_id=lineage_action_id,edge_index=1L,
        transition_id=NA_character_,evidence_id=parent_evidence_id,
        parent_edge_set_hash=edge_hash,stringsAsFactors=FALSE))
    }
    lineage_specs <- list(
      state_candidate=lineage_for("state_candidate",state_candidate_id,
        state_axis_record_id),
      state_segment=lineage_for("state_segment",state_segment_id,
        state_axis_record_id),
      event_candidate=lineage_for("event_candidate",event_candidate_id,
        event_support_evidence_id),
      event=lineage_for("event",event_id,event_support_evidence_id))
    tables$lineage_records <- do.call(rbind,lapply(lineage_specs,`[[`,"row"))
    tables$lineage_records <- stpd_gate_b_v3_sort_table(
      tables$lineage_records,
      unlist(bundle$tables$lineage_records$sort_key,use.names=FALSE))
    tables$state_candidates$lineage_id <- lineage_specs$state_candidate$id
    tables$state_segments$lineage_id <- lineage_specs$state_segment$id
    tables$event_candidates$lineage_id <- lineage_specs$event_candidate$id
    tables$events$lineage_id <- lineage_specs$event$id
  }
  inventory <- names(bundle$product_inventory)
  manifest <- do.call(rbind,lapply(inventory,function(name) data.frame(
    manifest_schema="stpd_multitrack_v3_manifest_1",product_kind="auto",
    table_name=name,table_schema=bundle$tables[[name]]$table_schema,
    required=TRUE,row_count=as.integer(nrow(tables[[name]])),
    canonical_table_sha256=digest::digest(
      stpd_gate_b_v3_canonical_table_bytes(tables[[name]],name,bundle),
      algo="sha256",serialize=FALSE),
    sort_contract_registry_id=rid("operational_policy",
      paste0("sort_contract_",name,"_v1")),stringsAsFactors=FALSE)))
  tables$canonical_manifest <- stpd_gate_b_v3_sort_table(manifest,
    unlist(bundle$tables$canonical_manifest$sort_key,use.names=FALSE))
  manifest_core_hash <- hash("stpd-canonical-manifest-core-v1",rawToChar(
    stpd_gate_b_v3_canonical_table_bytes(tables$canonical_manifest,
                                         "canonical_manifest",bundle)))
  product_context_hash <- hash("stpd-product-context-v1",canonical(list(
    product_kind="auto",detection_root_id=root,parent_auto_product_hash=NULL,
    history_head_hash=NULL,canonical_manifest_core_hash=manifest_core_hash,
    contract_sha256=bundle$normative_contract_sha256,
    schema_contract_sha256=bundle$schema_contract_sha256)))
  product_hash <- hash("stpd-product-v3",canonical(list(
    identity_schema="stpd_multitrack_v3_identity_1",product_kind="auto",
    detection_root_id=root,source_context_hash=source_context_hash,
    product_context_hash=product_context_hash,parent_auto_product_hash=NULL,
    history_head_hash=NULL,canonical_manifest_core_hash=manifest_core_hash,
    contract_sha256=bundle$normative_contract_sha256,
    schema_contract_sha256=bundle$schema_contract_sha256)))
  tables$materialized_product_identity <- data.frame(
    identity_schema="stpd_multitrack_v3_identity_1",product_kind="auto",
    detection_root_id=root,source_context_hash=source_context_hash,
    product_context_hash=product_context_hash,parent_auto_product_hash=NA_character_,
    history_head_hash=NA_character_,canonical_manifest_core_hash=manifest_core_hash,
    product_hash=product_hash,contract_sha256=bundle$normative_contract_sha256,
    schema_contract_sha256=bundle$schema_contract_sha256,
    stringsAsFactors=FALSE)
  cached_products <- stpd_gate_b_v3_phase1_runtime_cache$minimal_products
  if (!is.list(cached_products)) cached_products <- list()
  cached_products[[cache_key]] <- serialize(tables,NULL,version=2)
  stpd_gate_b_v3_phase1_runtime_cache$minimal_products <- cached_products
  unserialize(cached_products[[cache_key]])
}

stpd_gate_b_v3_phase1_state_only_product <- function(state_profile) {
  cache_key <- paste0("state_only|",state_profile)
  cached <- stpd_gate_b_v3_phase1_runtime_cache$derived_products
  if (is.list(cached) && !is.null(cached[[cache_key]])) {
    return(unserialize(cached[[cache_key]]))
  }
  tables <- stpd_gate_b_v3_phase1_minimal_product(
    TRUE,state_profile,structure_first_fixture=FALSE)
  event_candidate_ids <- tables$event_candidates$event_candidate_id
  event_ids <- tables$events$event_id
  modifier_ids <- tables$event_modifier_evidence$modifier_evidence_id
  event_evidence_ids <- unique(c(
    tables$event_candidates$base_event_evidence_id,
    tables$events$base_event_evidence_id,
    tables$event_modifier_evidence$evidence_id))
  consumer_ids <- unique(c(event_candidate_ids,event_ids,modifier_ids))
  tables$threshold_instance_bindings <- tables$threshold_instance_bindings[
    !tables$threshold_instance_bindings$consumer_id %in% consumer_ids,,
    drop=FALSE]
  tables$lineage_records <- tables$lineage_records[
    !tables$lineage_records$child_id %in% c(event_candidate_ids,event_ids),,
    drop=FALSE]
  tables$evidence_records <- tables$evidence_records[
    !tables$evidence_records$evidence_id %in% event_evidence_ids,,drop=FALSE]
  tables$event_state_relationships <-
    tables$event_state_relationships[0,,drop=FALSE]
  tables$event_modifier_evidence <-
    tables$event_modifier_evidence[0,,drop=FALSE]
  tables$event_candidates <- tables$event_candidates[0,,drop=FALSE]
  tables$events <- tables$events[0,,drop=FALSE]
  tables$per_isi$event_id[] <- NA_character_
  tables <- stpd_gate_b_v3_phase1_reseal_product(tables)
  cached <- stpd_gate_b_v3_phase1_runtime_cache$derived_products
  if (!is.list(cached)) cached <- list()
  cached[[cache_key]] <- serialize(tables,NULL,version=2)
  stpd_gate_b_v3_phase1_runtime_cache$derived_products <- cached
  unserialize(cached[[cache_key]])
}

stpd_gate_b_v3_phase1_provenance_product <- function() {
  cache_key <- "provenance|auto"
  cached <- stpd_gate_b_v3_phase1_runtime_cache$derived_products
  if (is.list(cached) && !is.null(cached[[cache_key]])) {
    return(unserialize(cached[[cache_key]]))
  }
  tables <- stpd_gate_b_v3_phase1_minimal_product(TRUE)
  bundle <- stpd_gate_b_v3_phase1_bundle()
  domain <- function(name) tables$entity_domain_registry[
    tables$entity_domain_registry$entity_domain==name &
      tables$entity_domain_registry$active,,drop=FALSE]
  event_domain <- domain("event")
  candidate_domain <- domain("event_candidate")
  if (nrow(event_domain)!=1L || nrow(candidate_domain)!=1L) {
    stop("Gate B v3 provenance fixture domain is ambiguous.",call.=FALSE)
  }
  source_key <- list(
    event_candidate_id=tables$event_candidates$event_candidate_id[[1L]])
  role <- tables$contract_registries[
    tables$contract_registries$registry_domain=="evidence_role" &
      tables$contract_registries$active,,drop=FALSE]
  if (nrow(role)<1L) stop("Gate B v3 provenance fixture role is missing.",
                          call.=FALSE)
  tables$entity_evidence_edges <- data.frame(
    schema_version="stpd_multitrack_v3_1",
    detection_root_id=
      tables$materialized_product_identity$detection_root_id[[1L]],
    entity_domain_id=event_domain$entity_domain_id[[1L]],
    entity_id=tables$events$event_id[[1L]],
    entity_product_hash=NA_character_,edge_index=1L,
    evidence_role_registry_id=role$registry_entry_id[[1L]],
    source_domain_id=candidate_domain$entity_domain_id[[1L]],
    source_row_key_json=stpd_gate_b_v3_canonical_json(source_key),
    source_product_hash=NA_character_,
    source_row_hash=stpd_gate_b_v3_source_row_hash(
      candidate_domain,"event_candidates",
      tables$event_candidates[1L,,drop=FALSE],source_key,NA_character_,bundle),
    evidence_id=tables$events$base_event_evidence_id[[1L]],
    stringsAsFactors=FALSE)
  tables <- stpd_gate_b_v3_phase1_reseal_product(tables,bundle)
  cached <- stpd_gate_b_v3_phase1_runtime_cache$derived_products
  if (!is.list(cached)) cached <- list()
  cached[[cache_key]] <- serialize(tables,NULL,version=2)
  stpd_gate_b_v3_phase1_runtime_cache$derived_products <- cached
  unserialize(cached[[cache_key]])
}

stpd_gate_b_v3_phase1_reseal_product <- function(
    tables,bundle=stpd_gate_b_v3_phase1_bundle()) {
  if (!is.list(tables) || is.null(tables$materialized_product_identity)) {
    stop("Gate B v3 fixture resealing requires a materialized product.",
         call.=FALSE)
  }
  registry <- tables$contract_registries
  rid <- function(code) {
    hit <- registry$registry_domain=="operational_policy" &
      registry$code==code & registry$active
    if (sum(hit)!=1L) stop("Gate B v3 sort contract is missing.",call.=FALSE)
    registry$registry_entry_id[hit]
  }
  inventory <- names(bundle$product_inventory)
  identity <- tables$materialized_product_identity[1L,,drop=FALSE]
  manifest <- do.call(rbind,lapply(inventory,function(name) data.frame(
    manifest_schema="stpd_multitrack_v3_manifest_1",
    product_kind=identity$product_kind[[1L]],table_name=name,
    table_schema=bundle$tables[[name]]$table_schema,required=TRUE,
    row_count=as.integer(nrow(tables[[name]])),
    canonical_table_sha256=digest::digest(
      stpd_gate_b_v3_canonical_table_bytes(tables[[name]],name,bundle),
      algo="sha256",serialize=FALSE),
    sort_contract_registry_id=rid(paste0("sort_contract_",name,"_v1")),
    stringsAsFactors=FALSE)))
  tables$canonical_manifest <- stpd_gate_b_v3_sort_table(manifest,
    unlist(bundle$tables$canonical_manifest$sort_key,use.names=FALSE))
  canonical <- stpd_gate_b_v3_canonical_json
  hash <- stpd_gate_b_v3_hash_raw
  manifest_core_hash <- hash("stpd-canonical-manifest-core-v1",rawToChar(
    stpd_gate_b_v3_canonical_table_bytes(tables$canonical_manifest,
                                         "canonical_manifest",bundle)))
  parent <- if (is.na(identity$parent_auto_product_hash[[1L]])) NULL else
    identity$parent_auto_product_hash[[1L]]
  history <- if (is.na(identity$history_head_hash[[1L]])) NULL else
    identity$history_head_hash[[1L]]
  context_hash <- hash("stpd-product-context-v1",canonical(list(
    product_kind=identity$product_kind[[1L]],
    detection_root_id=identity$detection_root_id[[1L]],
    parent_auto_product_hash=parent,history_head_hash=history,
    canonical_manifest_core_hash=manifest_core_hash,
    contract_sha256=bundle$normative_contract_sha256,
    schema_contract_sha256=bundle$schema_contract_sha256)))
  product_hash <- hash("stpd-product-v3",canonical(list(
    identity_schema="stpd_multitrack_v3_identity_1",
    product_kind=identity$product_kind[[1L]],
    detection_root_id=identity$detection_root_id[[1L]],
    source_context_hash=identity$source_context_hash[[1L]],
    product_context_hash=context_hash,parent_auto_product_hash=parent,
    history_head_hash=history,canonical_manifest_core_hash=manifest_core_hash,
    contract_sha256=bundle$normative_contract_sha256,
    schema_contract_sha256=bundle$schema_contract_sha256)))
  identity$product_context_hash <- context_hash
  identity$canonical_manifest_core_hash <- manifest_core_hash
  identity$product_hash <- product_hash
  identity$contract_sha256 <- bundle$normative_contract_sha256
  identity$schema_contract_sha256 <- bundle$schema_contract_sha256
  tables$materialized_product_identity <- identity
  tables
}

stpd_gate_b_v3_jcs_number <- function(x) {
  if (!is.double(x) || length(x) != 1L || !is.finite(x) ||
      stpd_gate_b_v3_is_negative_zero(x)) {
    stop("JCS numbers must be finite binary64 scalars.", call. = FALSE)
  }
  .Call("stpd_jcs_number_c", x, PACKAGE = "SpikeTrainPatternDetector")[[1L]]
}

stpd_gate_b_v3_entity_id <- function(domain, payload,
                                     bundle = stpd_gate_b_v3_phase1_bundle()) {
  spec <- bundle$id_payload_registry[[domain]]
  if (is.null(spec)) stop("Unknown Gate B v3 entity-ID domain.", call.=FALSE)
  fields <- unlist(spec$fields, use.names=FALSE)
  if (!is.list(payload) || !identical(sort(names(payload),method="radix"),
                                      sort(fields,method="radix"))) {
    stop("Gate B v3 entity-ID payload fields are not exact.", call.=FALSE)
  }
  paste0(spec$prefix, stpd_gate_b_v3_hash_raw(
    paste0("stpd-entity-id-v1:",domain),
    stpd_gate_b_v3_canonical_json(payload[fields])
  ))
}

stpd_gate_b_v3_canonical_json <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.atomic(x) && is.na(x))) return("null")
  if (is.factor(x)) stop("Factors are forbidden in canonical JSON.", call. = FALSE)
  if (is.character(x)) {
    x <- stpd_gate_b_v3_normalize_nfc(x)
    if (length(x) == 1L) {
      return(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null")))
    }
    return(paste0("[", paste(vapply(x, stpd_gate_b_v3_canonical_json,
                                    character(1)), collapse = ","), "]"))
  }
  if (is.logical(x)) {
    if (length(x) != 1L) return(paste0(
      "[", paste(vapply(as.list(x), stpd_gate_b_v3_canonical_json,
                         character(1)), collapse = ","), "]"
    ))
    return(if (isTRUE(x)) "true" else "false")
  }
  if (is.integer(x)) {
    if (length(x) != 1L) return(paste0(
      "[", paste(vapply(as.list(x), stpd_gate_b_v3_canonical_json,
                         character(1)), collapse = ","), "]"
    ))
    return(as.character(x))
  }
  if (is.double(x)) {
    if (any(!is.finite(x))) stop("Non-finite canonical number.", call. = FALSE)
    if (any(stpd_gate_b_v3_is_negative_zero(x))) {
      stop("Negative zero is forbidden in canonical JSON.", call. = FALSE)
    }
    if (length(x) != 1L) return(paste0(
      "[", paste(vapply(as.list(x), stpd_gate_b_v3_canonical_json,
                         character(1)), collapse = ","), "]"
    ))
    return(stpd_gate_b_v3_jcs_number(x))
  }
  if (is.list(x)) {
    if (is.null(names(x))) return(paste0(
      "[", paste(vapply(x, stpd_gate_b_v3_canonical_json, character(1)),
                   collapse = ","), "]"
    ))
    normalized_names <- stpd_gate_b_v3_normalize_nfc(names(x))
    if (anyNA(normalized_names) || any(!nzchar(normalized_names)) ||
        anyDuplicated(normalized_names)) {
      stop("Canonical object names must be unique and non-empty.", call. = FALSE)
    }
    order_index <- order(stpd_gate_b_v3_utf16_sort_key(normalized_names),
                         method = "radix")
    x <- x[order_index]
    normalized_names <- normalized_names[order_index]
    members <- vapply(seq_along(x), function(i) paste0(
      stpd_gate_b_v3_canonical_json(normalized_names[[i]]), ":",
      stpd_gate_b_v3_canonical_json(x[[i]])
    ), character(1))
    return(paste0("{", paste(members, collapse = ","), "}"))
  }
  stop("Unsupported canonical JSON type.", call. = FALSE)
}

stpd_gate_b_v3_sort_table <- function(x, sort_key) {
  if (nrow(x) < 2L || length(sort_key) == 0L) return(x)
  if (!all(sort_key %in% names(x))) {
    stop("Gate B v3 sort key references a missing column.", call. = FALSE)
  }
  out <- x[stpd_gate_b_v3_sort_index(x, sort_key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Serialize a Gate B v3 table to deterministic canonical JSON bytes
#' @param x A table matching `table_name`.
#' @param table_name Frozen table name.
#' @param bundle Optional schema bundle.
#' @return A UTF-8 raw vector.
#' @export
stpd_gate_b_v3_canonical_table_bytes <- function(
    x, table_name, bundle = stpd_gate_b_v3_phase1_bundle()) {
  stpd_gate_b_v3_validate_table_prototype(x, table_name, bundle)
  spec <- bundle$tables[[table_name]]
  columns <- lapply(spec$columns, function(field) list(
    name = field$name, type = field$type,
    nullable = isTRUE(field$nullable),
    enum = if (identical(field$enum, "none")) NULL else
      unlist(bundle$enums[[field$enum]], use.names = FALSE)
  ))
  rows <- lapply(seq_len(nrow(x)), function(i) {
    lapply(seq_along(x), function(j) {
      if (is.list(x[[j]])) x[[j]][[i]] else x[[j]][i]
    })
  })
  payload <- list(
    schema_id = spec$canonical_envelope_schema,
    columns = columns, rows = rows
  )
  charToRaw(enc2utf8(stpd_gate_b_v3_canonical_json(payload)))
}

stpd_gate_b_v3_hash_raw <- function(domain, canonical_text) {
  if (length(domain) != 1L || is.na(domain) || !nzchar(domain) ||
      grepl("[^ -~]", domain)) {
    stop("Hash domain must be one non-empty printable ASCII string.",
         call. = FALSE)
  }
  digest::digest(
    c(charToRaw(domain), as.raw(0L), charToRaw(enc2utf8(canonical_text))),
    algo = "sha256", serialize = FALSE
  )
}

#' Reconstruct the v3 label-blind input from an explicit allowlist
#' @param ds A dataset containing `trains`.
#' @param acquisition_context Optional root-free acquisition/QC rows by train.
#' @return A fresh list containing only normalized timestamps and allowlisted
#'   acquisition context. Source attributes and caches are not copied.
#' @export
stpd_gate_b_v3_label_blind_input <- function(ds, acquisition_context = NULL) {
  raw_named_list <- function(value, context) {
    if (!identical(typeof(value), "list")) {
      stop(context, " must be a list container.", call. = FALSE)
    }
    value_names <- attr(value, "names", exact = TRUE)
    if (is.null(value_names) || anyNA(value_names) ||
        any(!nzchar(value_names)) ||
        anyDuplicated(stringi::stri_trans_nfc(value_names))) {
      stop(context, " names must be unique and non-empty.", call. = FALSE)
    }
    # Remove every caller class/attribute before any `$`, `[[` or `[` access.
    # `.subset2` below then operates on a fresh base list and cannot dispatch
    # caller-defined S3 methods or observe hidden container attributes.
    out <- unclass(value)
    attributes(out) <- list(names = as.character(value_names))
    out
  }
  ds_raw <- raw_named_list(ds, "Gate B v3 dataset")
  ds_names <- attr(ds_raw, "names", exact = TRUE)
  trains_index <- match("trains", ds_names)
  if (is.na(trains_index)) {
    stop("A uniquely named train list is required for v3 label-blind reconstruction.",
         call. = FALSE)
  }
  trains_source <- raw_named_list(
    .subset2(ds_raw, trains_index), "Gate B v3 train container"
  )
  source_train_names <- attr(trains_source, "names", exact = TRUE)
  if (anyDuplicated(stringi::stri_trans_nfc(source_train_names))) {
    stop("A uniquely named train list is required for v3 label-blind reconstruction.",
         call. = FALSE)
  }
  canonical_train <- "^tr_[0-9a-f]{64}$"
  if (any(!grepl(canonical_train, source_train_names))) {
    stop("Gate B v3 train names must be stable tr_ pseudonyms.", call. = FALSE)
  }
  context_source <- NULL
  context_names <- character()
  if (!is.null(acquisition_context)) {
    context_source <- raw_named_list(
      acquisition_context, "Gate B v3 acquisition context"
    )
    context_names <- attr(context_source, "names", exact = TRUE)
    if (any(!context_names %in% source_train_names)) {
      stop("Acquisition context must be a uniquely named subset of trains.",
           call. = FALSE)
    }
  }
  train_names <- sort(source_train_names, method = "radix")
  trains <- lapply(train_names, function(train) {
    dat <- .subset2(trains_source, match(train, source_train_names))
    dat_class <- attr(dat, "class", exact = TRUE)
    dat_names <- attr(dat, "names", exact = TRUE)
    if (!identical(typeof(dat), "list") ||
        is.null(dat_class) || !"data.frame" %in% dat_class ||
        is.null(dat_names) || anyNA(dat_names) || anyDuplicated(dat_names)) {
      stop("Each train must be a data frame with unique columns.", call. = FALSE)
    }
    timestamp_index <- match("timestamp", dat_names)
    if (is.na(timestamp_index)) {
      stop("Label-blind timestamps are missing.", call. = FALSE)
    }
    dat_raw <- unclass(dat)
    attributes(dat_raw) <- list(names = as.character(dat_names))
    timestamp <- .subset2(dat_raw, timestamp_index)
    timestamp_type <- typeof(timestamp)
    attributes(timestamp) <- NULL
    if (!timestamp_type %in% c("integer", "double")) {
      stop("Label-blind timestamps must use a base numeric vector.",
           call. = FALSE)
    }
    timestamp <- as.double(timestamp)
    if (any(!is.finite(timestamp)) ||
        any(stpd_gate_b_v3_is_negative_zero(timestamp)) ||
        (length(timestamp) > 1L && any(diff(timestamp) <= 0))) {
      stop("Label-blind timestamps must be finite and strictly increasing.",
           call. = FALSE)
    }
    data.frame(timestamp = as.double(timestamp), stringsAsFactors = FALSE)
  })
  names(trains) <- train_names
  context <- lapply(train_names, function(train) {
    context_index <- match(train, context_names)
    row <- if (!is.null(context_source) && !is.na(context_index))
      .subset2(context_source, context_index) else NULL
    allowed <- c(
      "timestamp_unit", "timestamp_resolution_sec", "timebase_provenance",
      "waveform_qc_available", "raw_waveform_available", "sorting_qc_status",
      "condition_boundary_available", "patient_group_id_hash",
      "session_group_id_hash", "dataset_batch_id_hash"
    )
    if (is.null(row)) return(list())
    row_raw <- raw_named_list(row, "Acquisition context field")
    row_names <- attr(row_raw, "names", exact = TRUE)
    unknown <- setdiff(row_names,allowed)
    if (length(unknown)>0L) {
      stop("Unknown acquisition/QC input source is forbidden: ",
           paste(unknown,collapse=", "),call.=FALSE)
    }
    selected_names <- intersect(allowed, row_names)
    selected <- lapply(selected_names, function(field_name) {
      value <- .subset2(row_raw, match(field_name, row_names))
      attributes(value) <- NULL
      if (length(value) != 1L || is.list(value) ||
          is.na(value) || (is.numeric(value) && !is.finite(value))) {
        stop("Allowlisted acquisition/QC values must be nonmissing finite scalars.",
             call. = FALSE)
      }
      if (field_name == "timestamp_unit") {
        value <- stpd_gate_b_v3_strict_nfc(as.character(unclass(value)))
        if (!identical(value, "seconds")) {
          stop("Gate B v3 label-blind timestamps must be normalized to seconds.",
               call.=FALSE)
        }
        return("seconds")
      }
      if (field_name == "timestamp_resolution_sec") {
        value <- as.double(unclass(value))
        if (!is.finite(value) || value <= 0) {
          stop("Timestamp resolution must be one positive finite double.",
               call.=FALSE)
        }
        return(value)
      }
      if (field_name %in% c("waveform_qc_available", "raw_waveform_available",
                            "condition_boundary_available")) {
        if (!is.logical(unclass(value))) {
          stop("Gate B v3 availability fields must be logical.",call.=FALSE)
        }
        return(as.logical(unclass(value)))
      }
      if (field_name %in% c("patient_group_id_hash", "session_group_id_hash",
                            "dataset_batch_id_hash")) {
        value <- stpd_gate_b_v3_strict_nfc(as.character(unclass(value)))
        if (!grepl("^[0-9a-f]{64}$", value)) {
          stop("Allowlisted group identifiers must be lowercase SHA-256 hashes.",
               call.=FALSE)
        }
        return(value)
      }
      if (field_name %in% c("timebase_provenance", "sorting_qc_status")) {
        if (!is.character(unclass(value))) {
          stop("Gate B v3 provenance/QC status fields must be character.",
               call.=FALSE)
        }
        value <- stpd_gate_b_v3_strict_nfc(as.character(unclass(value)))
        if (!nzchar(value)) stop("Allowlisted strings must be non-empty.",
                                 call. = FALSE)
        return(value)
      }
      stop("Unsupported allowlisted acquisition/QC value type.", call. = FALSE)
    })
    names(selected) <- selected_names
    group_hash_fields <- intersect(
      c("patient_group_id_hash", "session_group_id_hash",
        "dataset_batch_id_hash"), names(selected)
    )
    selected
  })
  names(context) <- train_names
  timestamp_hash <- vapply(train_names, function(train) {
    connection <- rawConnection(raw(), "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(trains[[train]]$timestamp, connection, size = 8L,
             endian = "little")
    bytes <- rawConnectionValue(connection)
    close(connection)
    on.exit(NULL, add = FALSE)
    digest::digest(bytes, algo = "sha256", serialize = FALSE)
  }, character(1))
  context_hash <- vapply(train_names, function(train) {
    stpd_gate_b_v3_hash_raw(
      "stpd-acquisition-context-row-v1",
      stpd_gate_b_v3_canonical_json(context[[train]])
    )
  }, character(1))
  get_context <- function(train, field) {
    value <- context[[train]][[field]]
    if (is.null(value)) NA_character_ else as.character(value)
  }
  normalized_input_manifest <- data.frame(
    train = train_names,
    n_spikes = as.integer(vapply(trains, nrow, integer(1))),
    timestamp_unit = vapply(train_names, function(train) {
      value <- get_context(train, "timestamp_unit")
      if (is.na(value)) "seconds" else value
    }, character(1)),
    normalized_timestamp_bytes_sha256 = unname(timestamp_hash),
    acquisition_context_row_hash = unname(context_hash),
    patient_group_id_hash = vapply(train_names, get_context, character(1),
                                   field = "patient_group_id_hash"),
    session_group_id_hash = vapply(train_names, get_context, character(1),
                                   field = "session_group_id_hash"),
    dataset_batch_id_hash = vapply(train_names, get_context, character(1),
                                   field = "dataset_batch_id_hash"),
    stringsAsFactors = FALSE
  )
  registry <- stpd_gate_b_v3_phase1_registry()
  registry_id <- function(domain, code) {
    hit <- registry$registry_domain == domain & registry$code == code & registry$active
    if (sum(hit) != 1L) stop("Label-blind registry closure is incomplete.",
                             call. = FALSE)
    registry$registry_entry_id[hit]
  }
  manifest_spec <- data.frame(
    rule=c("fresh_reconstruction_v1","strip_label_review_sources_v1",
           "strip_derived_state_sources_v1","forbid_unknown_input_sources_v1"),
    allowlist=c("timestamp_acquisition_qc_allowlist_v1",
      "prohibited_label_review_sources_v1",
      "prohibited_derived_state_sources_v1","unknown_source_rejection_v1"),
    source=c("normalized_timestamp_and_acquisition_qc_v1",
      "label_and_review_sources_v1","derived_state_sources_v1",
      "unknown_input_source_v1"),
    action=c("allow","strip","strip","forbid"),stringsAsFactors=FALSE)
  label_blind_contract_manifest <- data.frame(
    contract_rule_registry_id=vapply(manifest_spec$rule,function(code)
      registry_id("label_blind_rule",code),character(1)),
    allowlist_schema_registry_id=vapply(manifest_spec$allowlist,function(code)
      registry_id("input_allowlist_schema",code),character(1)),
    source_domain_registry_id=vapply(manifest_spec$source,function(code)
      registry_id("input_source_domain",code),character(1)),
    action=manifest_spec$action,
    semantic_definition_sha256=vapply(manifest_spec$rule,function(code) {
      id <- registry_id("label_blind_rule",code)
      registry$semantic_definition_sha256[registry$registry_entry_id==id]
    },character(1)),stringsAsFactors=FALSE)
  label_blind_contract_manifest <- label_blind_contract_manifest[
    order(stpd_gate_b_v3_utf8_sort_key(
      label_blind_contract_manifest$contract_rule_registry_id),method="radix"),,
    drop=FALSE]
  rownames(label_blind_contract_manifest) <- NULL
  structure(
    list(
      trains = trains,
      acquisition_context = context,
      normalized_input_manifest = normalized_input_manifest,
      label_blind_contract_manifest = label_blind_contract_manifest
    ),
    class = c("stpd_gate_b_v3_label_blind_input", "list")
  )
}

#' Map the frozen frequency x regularity State decision
#' @param frequency One of `high`, `non_high`, or `unresolved`.
#' @param regularity One of `regular`, `irregular`, or `unresolved`.
#' @return A list with accepted class or typed abstention reason.
#' @export
stpd_gate_b_v3_state_axis_decision <- function(frequency, regularity) {
  f <- c("high", "non_high", "unresolved")
  r <- c("regular", "irregular", "unresolved")
  if (length(frequency) != 1L || !frequency %in% f ||
      length(regularity) != 1L || !regularity %in% r) {
    stop("Invalid Gate B v3 State-axis value.", call. = FALSE)
  }
  if (frequency == "high" && regularity == "irregular") {
    return(list(decision = "accepted", state_class =
                  "high_frequency_irregular_state", reason = NA_character_))
  }
  if (frequency == "high" && regularity == "regular") {
    return(list(decision = "accepted", state_class =
                  "high_frequency_tonic", reason = NA_character_))
  }
  if (frequency == "non_high" && regularity == "regular") {
    return(list(decision = "accepted", state_class = "tonic",
                reason = NA_character_))
  }
  reason <- if (frequency == "unresolved" && regularity == "unresolved") {
    "both_axes_unresolved"
  } else if (frequency == "unresolved") {
    "frequency_unresolved"
  } else if (regularity == "unresolved") {
    "regularity_unresolved"
  } else "non_high_irregular"
  list(decision = "abstain", state_class = NA_character_, reason = reason)
}

#' Report whether a local empirical Pause decision is mathematically attainable
#' @param valid_local_n_isi Number of valid reference ISIs.
#' @param family_size Number of candidates in the BH family.
#' @param alpha Operational score threshold.
#' @return Exact attainable p/q floors and a decision flag. This is not an FDR
#'   guarantee.
#' @export
stpd_gate_b_v3_pause_attainability <- function(
    valid_local_n_isi, family_size, alpha) {
  values <- c(valid_local_n_isi, family_size)
  if (length(valid_local_n_isi) != 1L || length(family_size) != 1L ||
      anyNA(values) || any(values < 1) || any(values != as.integer(values)) ||
      length(alpha) != 1L || is.na(alpha) || !is.finite(alpha) ||
      alpha <= 0 || alpha > 1) {
    stop("Invalid Pause attainability inputs.", call. = FALSE)
  }
  p_floor <- 1 / (as.integer(valid_local_n_isi) + 1)
  # If all candidates attain the discrete p floor, the adjusted score floor is
  # p_floor.  The rank-one isolated-candidate floor is more conservative. Neither
  # quantity implies BH FDR validity under selected, overlapping windows.
  q_floor <- p_floor
  isolated_q_floor <- min(1, p_floor * as.integer(family_size))
  list(
    discrete_empirical_p_lower_bound = p_floor,
    algebraic_family_q_lower_bound = q_floor,
    isolated_rank1_q_lower_bound = isolated_q_floor,
    algebraically_attainable = isTRUE(q_floor <= alpha),
    score_interpretation = "bh_adjusted_empirical_tail_score",
    fdr_control_claim = FALSE
  )
}

#' Return the frozen Gate B v3 cross-track compatibility matrix
#' @return A data frame with one deterministic action per supported tuple.
#' @export
stpd_gate_b_v3_compatibility_matrix <- function() {
  event <- rbind(
    expand.grid(candidate_domain="event",candidate_class="burst_event",
                overlap_domain="gap",overlap_class=c("canonical_pause","gap_unresolved"),
                stringsAsFactors=FALSE),
    expand.grid(candidate_domain="event",candidate_class="burst_event",
                overlap_domain="boundary",overlap_class=c("qc_hard","acquisition_hard"),
                stringsAsFactors=FALSE),
    expand.grid(candidate_domain="event",candidate_class="burst_event",
                overlap_domain="state",overlap_class=c("high_frequency_irregular_state",
                                                         "high_frequency_tonic","tonic"),
                stringsAsFactors=FALSE),
    data.frame(candidate_domain="event",candidate_class="burst_event",
               overlap_domain="event",overlap_class="burst_event")
  )
  state <- expand.grid(
    candidate_domain="state",
    candidate_class=c("high_frequency_irregular_state","high_frequency_tonic","tonic"),
    overlap_key=c("gap|canonical_pause","gap|gap_unresolved",
                  "boundary|qc_or_acquisition_hard"), stringsAsFactors=FALSE
  )
  state$overlap_domain <- sub("\\|.*$","",state$overlap_key)
  state$overlap_class <- sub("^[^|]+\\|","",state$overlap_key)
  state$overlap_key <- NULL
  link <- data.frame(
    candidate_domain="episode_link",candidate_class="pause_interrupted_hf",
    overlap_domain=c("gap","boundary","episode_link"),
    overlap_class=c("canonical_pause","qc_or_acquisition_hard",
                    "pause_interrupted_hf"),stringsAsFactors=FALSE
  )
  universe <- rbind(event,state[,names(event)],link)
  resolve <- function(cd,cc,od,oc) {
    if (cd=="event" && od=="gap" && oc=="canonical_pause")
      return(c("reject_event_preserve_gap","TRUE"))
    if (cd=="event" && od=="gap")
      return(c("abstain_event_preserve_gap","TRUE"))
    if (cd=="event" && od=="boundary")
      return(c("reject_event_preserve_boundary","TRUE"))
    if (cd=="event" && od=="state" && oc=="high_frequency_irregular_state")
      return(c("coexist_non_destructive","FALSE"))
    if (cd=="event" && od=="state")
      return(c("interrupt_state_full_redetection","TRUE"))
    if (cd=="event" && od=="event")
      return(c("maximal_disjoint_event_selection","TRUE"))
    if (cd=="state" && od=="gap" && oc=="gap_unresolved")
      return(c("abstain_state_preserve_gap","TRUE"))
    if (cd=="state") return(c("split_state_full_redetection","TRUE"))
    if (cd=="episode_link" && od=="gap")
      return(c("link_only_pause_excluded_from_support","TRUE"))
    if (cd=="episode_link" && od=="boundary")
      return(c("reject_link_preserve_boundary","TRUE"))
    c("reject_recursive_link","TRUE")
  }
  decision <- mapply(resolve,universe$candidate_domain,universe$candidate_class,
                     universe$overlap_domain,universe$overlap_class,
                     SIMPLIFY=FALSE)
  universe$action <- vapply(decision,`[[`,character(1),1L)
  universe$lineage_required <- vapply(decision,function(x) identical(x[[2]],"TRUE"),logical(1))
  universe$hard_for_state <- universe$overlap_class %in%
    c("canonical_pause","qc_hard","acquisition_hard","qc_or_acquisition_hard")
  universe$hard_for_event <- universe$overlap_class %in%
    c("canonical_pause","qc_hard","acquisition_hard","qc_or_acquisition_hard")
  universe$hard_for_episode_link <- universe$overlap_class %in%
    c("qc_hard","acquisition_hard","qc_or_acquisition_hard")
  universe <- universe[do.call(order,c(universe[1:4],list(method="radix"))),,
                       drop=FALSE]
  rownames(universe)<-NULL
  universe
}

#' Resolve one AUTO cross-track candidate conflict under the frozen v3 rules
#' @param candidate_domain `event` or `state`.
#' @param candidate_class Candidate class.
#' @param overlap_domain `gap`, `boundary`, or `state`.
#' @param overlap_class Exact accepted overlap class.
#' @return A unique action and lineage requirement.
#' @export
stpd_gate_b_v3_compatibility_action <- function(
    candidate_domain, candidate_class, overlap_domain, overlap_class) {
  key <- paste(candidate_domain, candidate_class, overlap_domain,
               overlap_class, sep = "|")
  matrix <- stpd_gate_b_v3_compatibility_matrix()
  matrix_key <- do.call(paste, c(matrix[c(
    "candidate_domain", "candidate_class", "overlap_domain", "overlap_class"
  )], sep = "|"))
  hit <- which(matrix_key == key)
  if (length(hit) != 1L) {
    stop("No frozen Gate B v3 compatibility action exists for this tuple.",
         call. = FALSE)
  }
  list(
    action = matrix$action[[hit]],
    lineage_required = matrix$lineage_required[[hit]],
    row_order_independent = TRUE
  )
}

stpd_gate_b_v3_dataset_snapshot_identity <- function(label_blind_input) {
  if (!inherits(label_blind_input, "stpd_gate_b_v3_label_blind_input")) {
    stop("A reconstructed Gate B v3 label-blind input is required.",
         call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(label_blind_input$normalized_input_manifest)),
                 function(i) as.list(label_blind_input$normalized_input_manifest[i, ,
                                                                           drop = FALSE]))
  canonical <- stpd_gate_b_v3_canonical_json(rows)
  stpd_gate_b_v3_hash_raw("stpd-normalized-input-manifest-v1", canonical)
}

#' Return the frozen phase-1 complexity contract
#' @return A six-stage complexity and resource-budget table.
#' @export
stpd_gate_b_v3_complexity_contract <- function() {
  data.frame(
    stage = c("detection","candidate_generation","relationships",
      "json_serialization","table_materialization","table_serialization",
      "product_serialization","artifact_packaging","memory"),
    time_bound = c("O(n)","O(n + m)","O(m + e)","O(j)","O(r)",
      "O(b)","O(b)","O(a)","O(n + m + e)"),
    space_bound = c("O(n)","O(m)","O(e)","O(j)","O(r)","O(b)",
      "O(b)","O(a)","O(n + m + e)"),
    budget_metric = c("input_isi","candidate_rows","edge_rows",
      "json_bytes_max","table_rows_max","table_bytes_max",
      "canonical_bytes_total","artifact_bytes_total","peak_memory_bytes"),
    limit = c(5e7,1e7,5e7,64*2^20,1e8,2^31,8*2^30,12*2^30,
              12*2^30),
    check_point = c("before_detector_allocation","before_candidate_materialization",
      "before_edge_materialization","before_json_commit","before_table_allocation",
      "before_table_commit","before_product_commit","before_artifact_commit",
      "before_and_after_each_stage"),
    counter_definition = c(
      "sum(max(0,n_spikes-1)) over normalized trains",
      "number of complete candidate-universe rows before selection",
      "number of provenance,lineage,relationship,binding edges before allocation",
      "maximum canonical UTF-8 bytes of one JSON scalar payload",
      "maximum rows in any one canonical table before allocation",
      "maximum canonical UTF-8 bytes of one typed table including envelope",
      "sum canonical table, identity and manifest bytes",
      "sum all candidate/official artifact bytes before commit",
      "process peak resident bytes measured by registered platform adapter"
    ),
    cancellation_point = rep(TRUE,9L),
    cleanup_state = rep("zero_published_tables_and_remove_staging",9L),
    fail_closed_code = rep("resource_budget_exceeded",9L),
    stringsAsFactors = FALSE
  )
}

stpd_gate_b_v3_resource_budget_compare <- function(counters,cancelled=FALSE) {
  contract <- stpd_gate_b_v3_complexity_contract()
  if (!is.numeric(counters) || is.null(names(counters)) || anyNA(counters) ||
      any(!is.finite(counters)) || any(counters < 0) ||
      any(counters!=floor(counters)) || anyDuplicated(names(counters))) {
    stop("Resource counters must be unique, exact non-negative integer counts.",
         call. = FALSE)
  }
  missing <- setdiff(contract$budget_metric, names(counters))
  if (length(missing) > 0L) {
    stop("Required Gate B v3 resource counters are missing.", call. = FALSE)
  }
  if (isTRUE(cancelled)) {
    return(list(
      product_status = "failed_closed", failure_stage = "cancelled",
      failure_code = "cancelled", canonical_tables_published = 0L,
      staging_cleanup = "required_complete", truncated = FALSE
    ))
  }
  exceeded <- contract$budget_metric[
    counters[contract$budget_metric] > contract$limit
  ]
  if (length(exceeded) > 0L) {
    return(list(
      product_status = "failed_closed", failure_stage = "resource_budget",
      failure_code = "resource_budget_exceeded",
      exceeded_counters = sort(exceeded, method = "radix"),
      canonical_tables_published = 0L,
      staging_cleanup = "required_complete", truncated = FALSE
    ))
  }
  list(
    product_status = "within_budget", failure_stage = NA_character_,
    failure_code = NA_character_, exceeded_counters = character(),
    canonical_tables_published = 0L, staging_cleanup = "not_applicable",
    truncated = FALSE
  )
}

#' Reject caller-supplied Gate B v3 resource claims
#' @param counters Untrusted caller-supplied resource data. Direct checks are
#'   disabled because no R object/class/namespace token proves how it was
#'   measured; use the controlled staging writer instead.
#' @param cancelled Whether a cancellation request was observed at a declared
#'   cancellation boundary.
#' @return A typed, non-authoritative resource status.
#' @export
stpd_gate_b_v3_resource_budget_check <- function(counters, cancelled = FALSE) {
  stop(paste(
    "Direct Gate B v3 resource claims are disabled;",
    "use the controlled staging writer."
  ),call.=FALSE)
}

stpd_gate_b_v3_peak_rss_native <- function() {
  .Call("stpd_peak_rss_bytes_c",PACKAGE="SpikeTrainPatternDetector")
}

stpd_gate_b_v3_peak_rss_bytes <- function() {
  value <- tryCatch(stpd_gate_b_v3_peak_rss_native(),error=function(e) NA_real_)
  if (is.numeric(value) && length(value)==1L && !is.na(value) &&
      is.finite(value) && value>0 && value==floor(value)) return(as.double(value))
  stop("Gate B v3 peak-RSS measurement backend is unavailable.",call.=FALSE)
}

stpd_gate_b_v3_resource_relative_path <- function(path) {
  if (length(path)!=1L || is.na(path) || !nzchar(path) ||
      !identical(path,enc2utf8(path)) || grepl("[^ -~]",path) ||
      grepl("(^/|^[A-Za-z]:|\\\\|//|(^|/)\\.{1,2}(/|$)|/$)",path,
            perl=TRUE)) {
    stop("Gate B v3 controlled writer requires a safe ASCII relative path.",
         call.=FALSE)
  }
  parts <- strsplit(path,"/",fixed=TRUE)[[1L]]
  if (any(!nzchar(parts))) {
    stop("Gate B v3 controlled writer requires a safe ASCII relative path.",
         call.=FALSE)
  }
  path
}

stpd_gate_b_v3_validate_resource_operations <- function(operations,bundle) {
  if (!is.list(operations)) {
    stop("Gate B v3 resource operations must be a declarative list.",call.=FALSE)
  }
  fields <- list(
    input_spike_count=c("op","n_spikes"),
    reserve_candidate_rows=c("op","n_rows"),
    reserve_edge_rows=c("op","n_rows"),
    write_json=c("op","relative_path","value"),
    write_canonical_table=c("op","relative_path","table_name","value"),
    write_artifact_raw=c("op","relative_path","bytes")
  )
  write_paths <- table_names <- character()
  for (operation in operations) {
    if (!is.list(operation) || is.null(names(operation)) ||
        any(!nzchar(names(operation))) || anyDuplicated(names(operation)) ||
        length(operation$op)!=1L || !is.character(operation$op) ||
        !operation$op %in% names(fields) ||
        !identical(names(operation),fields[[operation$op]])) {
      stop("Unknown or malformed Gate B v3 controlled-writer operation.",
           call.=FALSE)
    }
    if (operation$op=="input_spike_count") {
      value <- operation$n_spikes
      if (!is.numeric(value) || length(value)!=1L || is.na(value) ||
          !is.finite(value) || value<0 || value!=floor(value)) {
        stop("Gate B v3 input spike count must be one exact non-negative integer.",
             call.=FALSE)
      }
    }
    if (operation$op %in% c("reserve_candidate_rows","reserve_edge_rows")) {
      value <- operation$n_rows
      if (!is.numeric(value) || length(value)!=1L || is.na(value) ||
          !is.finite(value) || value<0 || value!=floor(value)) {
        stop("Gate B v3 row reservation must be one exact non-negative integer.",
             call.=FALSE)
      }
    }
    if (grepl("^write_",operation$op)) {
      path <- stpd_gate_b_v3_resource_relative_path(operation$relative_path)
      if (path %in% write_paths) {
        stop("Gate B v3 controlled writer rejects duplicate artifact paths.",
             call.=FALSE)
      }
      write_paths <- c(write_paths,path)
    }
    if (operation$op=="write_canonical_table") {
      if (length(operation$table_name)!=1L ||
          !is.character(operation$table_name) ||
          !operation$table_name %in% names(bundle$tables) ||
          !is.data.frame(operation$value) ||
          operation$table_name %in% table_names) {
        stop("Gate B v3 canonical-table write operation is invalid/duplicated.",
             call.=FALSE)
      }
      table_names <- c(table_names,operation$table_name)
    }
    if (operation$op=="write_artifact_raw" && !is.raw(operation$bytes)) {
      stop("Gate B v3 artifact writes require an exact raw byte vector.",
           call.=FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_resource_json_upper_bound <- function(value, limit) {
  cap <- as.double(limit) + 1
  add <- function(left, right) min(cap, as.double(left) + as.double(right))
  count_string <- function(x) {
    if (is.na(x)) return(4)
    # A Unicode scalar is at most one surrogate pair (12 ASCII bytes) when
    # escaped.  This intentionally conservative bound is computed without
    # constructing the normalized/escaped JSON string.
    chars <- nchar(x, type = "chars", allowNA = FALSE, keepNA = FALSE)
    min(cap, 2 + 12 * as.double(chars))
  }
  walk <- function(x) {
    if (is.null(x) || (length(x) == 1L && is.atomic(x) && is.na(x))) return(4)
    if (is.factor(x)) stop("Factors are forbidden in canonical JSON.",call.=FALSE)
    type <- typeof(x)
    if (type == "character") {
      if (length(x) == 1L) return(count_string(x))
      total <- 2
      for (i in seq_along(x)) {
        if (i > 1L) total <- add(total, 1)
        total <- add(total, count_string(x[[i]]))
        if (total >= cap) break
      }
      return(total)
    }
    if (type %in% c("logical", "integer", "double")) {
      if (type == "double" && (any(!is.finite(x)) ||
          any(stpd_gate_b_v3_is_negative_zero(x)))) {
        stop("Invalid canonical numeric value in resource preflight.",call.=FALSE)
      }
      scalar_bound <- if (type == "logical") 5 else if (type == "integer") 12 else 32
      if (length(x) == 1L) return(scalar_bound)
      return(min(cap, 2 + as.double(length(x)) * scalar_bound +
                   max(0, length(x) - 1)))
    }
    if (type == "list") {
      value_names <- attr(x, "names", exact = TRUE)
      total <- 2
      if (length(x) == 0L) return(total)
      for (i in seq_along(x)) {
        if (i > 1L) total <- add(total, 1)
        if (!is.null(value_names)) {
          total <- add(total, count_string(value_names[[i]]))
          total <- add(total, 1)
        }
        total <- add(total, walk(.subset2(x, i)))
        if (total >= cap) break
      }
      return(total)
    }
    stop("Unsupported canonical JSON type in resource preflight.",call.=FALSE)
  }
  as.double(walk(value))
}

stpd_gate_b_v3_resource_table_upper_bound <- function(
    value, table_name, bundle, limit) {
  spec <- bundle$tables[[table_name]]
  columns <- lapply(spec$columns, function(field) list(
    name = field$name, type = field$type,
    nullable = isTRUE(field$nullable),
    enum = if (identical(field$enum, "none")) NULL else
      unlist(bundle$enums[[field$enum]], use.names = FALSE)
  ))
  base <- stpd_gate_b_v3_resource_json_upper_bound(list(
    schema_id = spec$canonical_envelope_schema,
    columns = columns, rows = list()
  ), limit)
  cap <- as.double(limit) + 1
  total <- base
  if (nrow(value) == 0L) return(total)
  for (i in seq_len(nrow(value))) {
    total <- min(cap, total + 2 + max(0, ncol(value) - 1))
    for (j in seq_len(ncol(value))) {
      cell <- if (is.list(value[[j]])) value[[j]][[i]] else value[[j]][i]
      total <- min(cap, total +
        stpd_gate_b_v3_resource_json_upper_bound(cell, cap - total))
      if (total >= cap) break
    }
    if (i > 1L) total <- min(cap, total + 1)
    if (total >= cap) break
  }
  as.double(total)
}

stpd_gate_b_v3_resource_serialize_json <- function(value) {
  charToRaw(enc2utf8(stpd_gate_b_v3_canonical_json(value)))
}

stpd_gate_b_v3_resource_serialize_table <- function(value,table_name,bundle) {
  stpd_gate_b_v3_canonical_table_bytes(value,table_name,bundle)
}

stpd_gate_b_v3_preflight_resource_operations <- function(
    operations,bundle,cancelled=FALSE) {
  contract <- stpd_gate_b_v3_complexity_contract()
  limits <- setNames(as.double(contract$limit),contract$budget_metric)
  observed <- setNames(rep(0,length(limits)),names(limits))
  observed[["peak_memory_bytes"]] <- stpd_gate_b_v3_peak_rss_bytes()
  status <- stpd_gate_b_v3_resource_budget_compare(observed,cancelled)
  if (!identical(status$product_status,"within_budget")) {
    return(list(status=status,observed=observed))
  }
  candidate_tables <- c("state_candidates","event_candidates","gap_candidates",
                        "review_candidates")
  edge_tables <- c("entity_evidence_edges","lineage_records",
                   "event_state_relationships",
                   "event_interrupted_state_relationships",
                   "state_episode_links","threshold_instance_bindings")
  reserved_candidate <- reserved_edge <- 0
  actual_candidate <- actual_edge <- 0
  for (operation in operations) {
    if (operation$op=="input_spike_count") {
      observed[["input_isi"]] <- observed[["input_isi"]] +
        max(0,as.double(operation$n_spikes)-1)
    } else if (operation$op=="reserve_candidate_rows") {
      reserved_candidate <- reserved_candidate + as.double(operation$n_rows)
      observed[["candidate_rows"]] <- max(observed[["candidate_rows"]],
                                           reserved_candidate)
    } else if (operation$op=="reserve_edge_rows") {
      reserved_edge <- reserved_edge + as.double(operation$n_rows)
      observed[["edge_rows"]] <- max(observed[["edge_rows"]],reserved_edge)
    } else if (operation$op=="write_artifact_raw") {
      observed[["artifact_bytes_total"]] <-
        observed[["artifact_bytes_total"]] + length(operation$bytes)
    } else if (operation$op=="write_json") {
      bound <- stpd_gate_b_v3_resource_json_upper_bound(
        operation$value, limits[["json_bytes_max"]]
      )
      observed[["json_bytes_max"]] <- max(observed[["json_bytes_max"]],bound)
      observed[["artifact_bytes_total"]] <-
        observed[["artifact_bytes_total"]] + bound
    } else if (operation$op=="write_canonical_table") {
      rows <- as.double(nrow(operation$value))
      observed[["table_rows_max"]] <- max(observed[["table_rows_max"]],rows)
      if (operation$table_name %in% candidate_tables) {
        actual_candidate <- actual_candidate + rows
        observed[["candidate_rows"]] <- max(observed[["candidate_rows"]],
                                             actual_candidate)
      }
      if (operation$table_name %in% edge_tables) {
        actual_edge <- actual_edge + rows
        observed[["edge_rows"]] <- max(observed[["edge_rows"]],actual_edge)
      }
      status <- stpd_gate_b_v3_resource_budget_compare(observed,FALSE)
      if (!identical(status$product_status,"within_budget")) break
      bound <- stpd_gate_b_v3_resource_table_upper_bound(
        operation$value,operation$table_name,bundle,
        min(limits[["table_bytes_max"]],
            limits[["canonical_bytes_total"]] -
              observed[["canonical_bytes_total"]],
            limits[["artifact_bytes_total"]] -
              observed[["artifact_bytes_total"]])
      )
      observed[["table_bytes_max"]] <- max(observed[["table_bytes_max"]],bound)
      observed[["canonical_bytes_total"]] <-
        observed[["canonical_bytes_total"]] + bound
      observed[["artifact_bytes_total"]] <-
        observed[["artifact_bytes_total"]] + bound
    }
    status <- stpd_gate_b_v3_resource_budget_compare(observed,FALSE)
    if (!identical(status$product_status,"within_budget")) break
  }
  if (identical(status$product_status,"within_budget") &&
      (reserved_candidate!=actual_candidate || reserved_edge!=actual_edge)) {
    stop("Gate B v3 reserved/materialized row counters do not close before serialization.",
         call.=FALSE)
  }
  list(status=status,observed=observed)
}

#' Execute controlled phase-1 staging operations with measured resource use
#' @param root Existing directory under which a private staging directory is made.
#' @param operations Declarative controlled-writer operations. Arbitrary callback
#'   functions and caller-supplied write counters are not accepted.
#' @param cancelled Whether cancellation was observed before the first operation.
#' @return Typed resource status, actual counters, and verified cleanup state.
#' @export
stpd_gate_b_v3_resource_staging_probe <- function(
    root, operations = list(), cancelled = FALSE) {
  root_link <- if (length(root)==1L && !is.na(root)) Sys.readlink(root) else
    NA_character_
  if (length(root)!=1L || is.na(root) || !dir.exists(root) ||
      (!is.na(root_link) && nzchar(root_link))) {
    stop("A concrete non-symlink resource-probe root is required.",call.=FALSE)
  }
  bundle <- stpd_gate_b_v3_phase1_bundle()
  stpd_gate_b_v3_validate_resource_operations(operations,bundle)
  root <- normalizePath(root,mustWork=TRUE)
  stage <- tempfile("stpd-gb3-stage-",tmpdir=root)
  stage_base <- basename(stage)
  root_snapshot <- function() {
    paths <- list.files(root,all.files=TRUE,no..=TRUE,recursive=TRUE,
                        full.names=TRUE,include.dirs=TRUE)
    relative <- if (length(paths)==0L) character() else
      substring(paths,nchar(root)+2L)
    keep <- relative!=stage_base & !startsWith(relative,paste0(stage_base,"/"))
    paths <- paths[keep]
    relative <- relative[keep]
    if (length(paths)==0L) return(character())
    value <- vapply(seq_along(paths),function(i) {
      link <- Sys.readlink(paths[[i]])
      if (!is.na(link) && nzchar(link)) return(paste0("link|",link))
      info <- file.info(paths[[i]])
      if (isTRUE(info$isdir)) return("directory")
      if (!isTRUE(info$isdir) && file.exists(paths[[i]])) return(paste(
        "file",info$size,digest::digest(paths[[i]],algo="sha256",file=TRUE),
        sep="|"))
      "other"
    },character(1))
    order_index <- order(relative,method="radix")
    setNames(value[order_index],relative[order_index])
  }
  before_root <- root_snapshot()
  preflight <- stpd_gate_b_v3_preflight_resource_operations(
    operations,bundle,cancelled
  )
  if (!identical(preflight$status$product_status,"within_budget")) {
    if (!identical(before_root,root_snapshot())) {
      stop("Gate B v3 resource preflight changed the target directory.",
           call.=FALSE)
    }
    status <- preflight$status
    status$observed_counters <- preflight$observed
    status$measurement_phase <- "preflight_upper_bound"
    status$staging_cleanup <- "not_created"
    status$staging_path_absent <- TRUE
    return(status)
  }
  if (!dir.create(stage,recursive=FALSE,showWarnings=FALSE,mode="0700")) {
    stop("Could not create the private Gate B v3 staging directory.",call.=FALSE)
  }
  cleaned <- FALSE
  artifact_manifest <- list()
  cleanup <- function() {
    exists <- file.exists(stage) ||
      (!is.na(Sys.readlink(stage)) && nzchar(Sys.readlink(stage)))
    if (exists) unlink(stage,recursive=TRUE,force=TRUE)
    cleaned <<- !(file.exists(stage) ||
      (!is.na(Sys.readlink(stage)) && nzchar(Sys.readlink(stage))))
    if (!cleaned) stop("Gate B v3 staging cleanup postcondition failed.",call.=FALSE)
  }
  on.exit(if (!cleaned) cleanup(),add=TRUE)
  metrics <- stpd_gate_b_v3_complexity_contract()$budget_metric
  observed <- setNames(rep(0,length(metrics)),metrics)
  observed[["peak_memory_bytes"]] <- stpd_gate_b_v3_peak_rss_bytes()
  reserved_candidate <- reserved_edge <- 0
  actual_candidate <- actual_edge <- 0
  status <- stpd_gate_b_v3_resource_budget_compare(observed,cancelled)
  candidate_tables <- c("state_candidates","event_candidates","gap_candidates",
                        "review_candidates")
  edge_tables <- c("entity_evidence_edges","lineage_records",
                   "event_state_relationships",
                   "event_interrupted_state_relationships",
                   "state_episode_links","threshold_instance_bindings")
  write_bytes <- function(relative_path,bytes) {
    target <- file.path(stage,relative_path)
    if (!dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE,
                    mode="0700") && !dir.exists(dirname(target))) {
      stop("Gate B v3 controlled writer could not create a staging directory.",
           call.=FALSE)
    }
    parent <- normalizePath(dirname(target),mustWork=TRUE)
    target_link <- Sys.readlink(target)
    if (!(identical(parent,stage) || startsWith(parent,paste0(stage,.Platform$file.sep))) ||
        file.exists(target) || (!is.na(target_link) && nzchar(target_link))) {
      stop("Gate B v3 controlled writer path escaped or already exists.",
           call.=FALSE)
    }
    con <- file(target,open="wb")
    tryCatch(writeBin(bytes,con),finally=close(con))
    link <- Sys.readlink(target)
    info <- file.info(target)
    if ((!is.na(link) && nzchar(link)) || !file.exists(target) ||
        isTRUE(info$isdir) || !identical(as.double(info$size),as.double(length(bytes)))) {
      stop("Gate B v3 controlled-writer file postcondition failed.",call.=FALSE)
    }
    artifact_manifest[[relative_path]] <<- list(
      size=as.double(info$size),
      sha256=digest::digest(target,algo="sha256",file=TRUE)
    )
  }
  if (identical(status$product_status,"within_budget")) {
    for (operation in operations) {
      observed[["peak_memory_bytes"]] <- max(
        observed[["peak_memory_bytes"]],stpd_gate_b_v3_peak_rss_bytes()
      )
      bytes <- NULL
      if (operation$op=="input_spike_count") {
        observed[["input_isi"]] <- observed[["input_isi"]] +
          max(0,as.double(operation$n_spikes)-1)
      } else if (operation$op=="reserve_candidate_rows") {
        reserved_candidate <- reserved_candidate+as.double(operation$n_rows)
        observed[["candidate_rows"]] <- max(reserved_candidate,actual_candidate)
      } else if (operation$op=="reserve_edge_rows") {
        reserved_edge <- reserved_edge+as.double(operation$n_rows)
        observed[["edge_rows"]] <- max(reserved_edge,actual_edge)
      } else if (operation$op=="write_json") {
        bytes <- stpd_gate_b_v3_resource_serialize_json(operation$value)
        observed[["json_bytes_max"]] <- max(
          observed[["json_bytes_max"]],length(bytes)
        )
      } else if (operation$op=="write_canonical_table") {
        bytes <- stpd_gate_b_v3_resource_serialize_table(
          operation$value,operation$table_name,bundle
        )
        observed[["table_rows_max"]] <- max(
          observed[["table_rows_max"]],nrow(operation$value)
        )
        observed[["table_bytes_max"]] <- max(
          observed[["table_bytes_max"]],length(bytes)
        )
        observed[["canonical_bytes_total"]] <-
          observed[["canonical_bytes_total"]]+length(bytes)
        if (operation$table_name %in% candidate_tables) {
          actual_candidate <- actual_candidate+nrow(operation$value)
          observed[["candidate_rows"]] <- max(reserved_candidate,actual_candidate)
        }
        if (operation$table_name %in% edge_tables) {
          actual_edge <- actual_edge+nrow(operation$value)
          observed[["edge_rows"]] <- max(reserved_edge,actual_edge)
        }
      } else if (operation$op=="write_artifact_raw") {
        bytes <- operation$bytes
      }
      if (!is.null(bytes)) {
        observed[["artifact_bytes_total"]] <-
          observed[["artifact_bytes_total"]]+length(bytes)
      }
      observed[["peak_memory_bytes"]] <- max(
        observed[["peak_memory_bytes"]],stpd_gate_b_v3_peak_rss_bytes()
      )
      status <- stpd_gate_b_v3_resource_budget_compare(observed,FALSE)
      if (!identical(status$product_status,"within_budget")) break
      if (!is.null(bytes)) write_bytes(operation$relative_path,bytes)
    }
  }
  if (identical(status$product_status,"within_budget") &&
      (reserved_candidate!=actual_candidate ||
       reserved_edge!=actual_edge)) {
    stop("Gate B v3 reserved/materialized row counters do not close.",call.=FALSE)
  }
  actual_paths <- list.files(stage,all.files=TRUE,no..=TRUE,recursive=TRUE,
                             full.names=FALSE,include.dirs=FALSE)
  declared_paths <- names(artifact_manifest) %||% character()
  if (!identical(sort(actual_paths,method="radix"),
                 sort(declared_paths,method="radix"))) {
    stop("Gate B v3 controlled-writer artifact set postcondition failed.",
         call.=FALSE)
  }
  for (path in declared_paths) {
    target <- file.path(stage,path)
    info <- file.info(target)
    link <- Sys.readlink(target)
    if ((!is.na(link) && nzchar(link)) || !file.exists(target) ||
        isTRUE(info$isdir) ||
        !identical(as.double(info$size),artifact_manifest[[path]]$size) ||
        !identical(digest::digest(target,algo="sha256",file=TRUE),
                   artifact_manifest[[path]]$sha256)) {
      stop("Gate B v3 controlled-writer artifact verification failed.",
           call.=FALSE)
    }
  }
  actual_artifact_bytes <- sum(vapply(
    artifact_manifest,function(x) x$size,double(1)
  ))
  if (identical(status$product_status,"within_budget") &&
      !identical(as.double(actual_artifact_bytes),
                 as.double(observed[["artifact_bytes_total"]]))) {
    stop("Gate B v3 measured artifact-byte total does not close.",call.=FALSE)
  }
  after_root <- root_snapshot()
  if (!identical(before_root,after_root)) {
    stop("Gate B v3 controlled writer changed bytes outside its private stage.",
         call.=FALSE)
  }
  cleanup()
  status$observed_counters <- observed
  status$staging_cleanup <- "verified_complete"
  status$staging_path_absent <- TRUE
  status
}

#' Validate one frozen FINAL transition payload
#' @param action_type One of the nine frozen transition actions.
#' @param payload Named list with the exact action-specific fields.
#' @return Canonical payload bytes and a domain-separated payload hash.
#' @export
stpd_gate_b_v3_validate_transition_payload <- function(action_type,payload) {
  if (length(action_type)!=1L || is.na(action_type) || !is.list(payload) ||
      is.null(names(payload)) || anyDuplicated(names(payload))) {
    stop("A unique named Gate B v3 transition payload is required.",call.=FALSE)
  }
  registry <- stpd_gate_b_v3_phase1_registry(FALSE)
  code <- paste0(action_type,"_v1")
  hit <- which(registry$registry_domain=="transition_payload_schema" &
                 registry$code==code & registry$active)
  if (length(hit)!=1L) stop("Unsupported transition action/schema.",call.=FALSE)
  semantic <- jsonlite::fromJSON(registry$semantic_definition_json[hit],
                                 simplifyVector=FALSE)
  required <- unlist(semantic$formula_or_state_machine$required_keys,
                     use.names=FALSE)
  if (!setequal(names(payload),required) || length(payload)!=length(required)) {
    stop("Transition payload keys do not match the frozen exact schema.",call.=FALSE)
  }
  payload <- payload[required]
  scalar_ok <- vapply(payload,function(x) (is.character(x) || is.numeric(x) ||
    is.logical(x)) && length(x)==1L && !is.na(x),logical(1))
  if (!all(scalar_ok)) stop("Transition payload values must be non-null scalars.",call.=FALSE)
  hash_fields <- names(payload)[grepl("expected_.*hash$",names(payload))]
  if (length(hash_fields)>0L && any(!vapply(payload[hash_fields],function(x)
      is.character(x) && grepl("^[0-9a-f]{64}$",x),logical(1)))) {
    stop("Transition compare-and-swap hashes must be lowercase SHA-256.",call.=FALSE)
  }
  json <- stpd_gate_b_v3_canonical_json(payload)
  list(action_type=action_type,payload_json=json,
       payload_json_hash=stpd_gate_b_v3_hash_raw(
         "stpd-transition-payload-v1",json))
}

stpd_gate_b_v3_phase1_final_state_hash <- function(state) {
  stpd_gate_b_v3_hash_raw("stpd-phase1-final-state-v1",
                          stpd_gate_b_v3_canonical_json(state))
}

stpd_gate_b_v3_phase1_transition_replay <- function(state,action_type,request) {
  if (!is.list(state) || !setequal(names(state),c("events","states","gaps")) ||
      !is.list(request) || is.null(request$expected_final_hash) ||
      !grepl("^[0-9a-f]{64}$",request$expected_final_hash)) {
    stop("Gate B v3 transition replay input is incomplete.",call.=FALSE)
  }
  before <- stpd_gate_b_v3_phase1_final_state_hash(state)
  if (!identical(before,request$expected_final_hash)) {
    return(list(status="failed_closed",failure_code="stale_compare_and_swap",
                state=state,state_hash=before,changed=FALSE))
  }
  supported <- c("event_candidate_accept","event_projection_reject",
                 "state_split")
  if (!action_type %in% supported) {
    return(list(status="failed_closed",
      failure_code="unsupported_transition_for_schema_v1",state=state,
      state_hash=before,changed=FALSE))
  }
  if (identical(action_type,"event_candidate_accept")) {
    required <- c("candidate_id","candidate_hash","start_isi","end_isi",
                  "overlap_domains")
    if (!all(required %in% names(request))) {
      stop("Event-accept transition request is incomplete.",call.=FALSE)
    }
    payload <- list(
      review_candidate_id=request$candidate_id,
      expected_review_candidate_hash=request$candidate_hash,
      source_event_candidate_id=paste0("ec_",strrep("a",64L)),
      expected_source_candidate_hash=strrep("b",64L),
      suggested_class_registry_id=paste0("reg_",strrep("c",64L)),
      reason_registry_id=paste0("reg_",strrep("d",64L)))
    stpd_gate_b_v3_validate_transition_payload(action_type,payload)
    forbidden <- c("accepted_event","canonical_pause","hard_boundary",
                   "high_frequency_tonic","tonic")
    conflict <- intersect(unlist(request$overlap_domains,use.names=FALSE),forbidden)
    if (length(conflict)>0L) {
      return(list(status="failed_closed",failure_code="review_event_conflict",
                  state=state,state_hash=before,changed=FALSE))
    }
    event <- list(event_id=request$candidate_id,start_isi=request$start_isi,
                  end_isi=request$end_isi,record_status="review_confirmed")
    state$events <- c(state$events,list(event))
  } else if (identical(action_type,"event_projection_reject")) {
    required <- c("event_id","event_hash","requires_state_restoration")
    if (!all(required %in% names(request))) {
      stop("Event-projection transition request is incomplete.",call.=FALSE)
    }
    stpd_gate_b_v3_validate_transition_payload(action_type,list(
      event_id=request$event_id,expected_event_hash=request$event_hash,
      reason_registry_id=paste0("reg_",strrep("d",64L))))
    if (isTRUE(request$requires_state_restoration)) {
      return(list(status="failed_closed",
        failure_code="unsupported_transition_for_schema_v1",state=state,
        state_hash=before,changed=FALSE))
    }
    keep <- !vapply(state$events,function(x)
      identical(x$event_id,request$event_id),logical(1))
    if (all(keep)) return(list(status="failed_closed",
      failure_code="transition_target_absent",state=state,state_hash=before,
      changed=FALSE))
    state$events <- state$events[keep]
  } else {
    required <- c("state_episode_id","episode_hash","connector_segment_id",
                  "connector_hash","left_child","right_child")
    if (!all(required %in% names(request)) ||
        length(request$left_child)!=2L || length(request$right_child)!=2L) {
      stop("State-split transition request is incomplete.",call.=FALSE)
    }
    stpd_gate_b_v3_validate_transition_payload(action_type,list(
      state_episode_id=request$state_episode_id,
      expected_episode_hash=request$episode_hash,
      connector_segment_id=request$connector_segment_id,
      expected_connector_segment_hash=request$connector_hash,
      reason_registry_id=paste0("reg_",strrep("d",64L))))
    state$states <- list(
      list(state_episode_id=paste0(request$state_episode_id,"_left"),
           start_isi=as.integer(request$left_child[[1L]]),
           end_isi=as.integer(request$left_child[[2L]])),
      list(state_episode_id=paste0(request$state_episode_id,"_right"),
           start_isi=as.integer(request$right_child[[1L]]),
           end_isi=as.integer(request$right_child[[2L]])))
  }
  after <- stpd_gate_b_v3_phase1_final_state_hash(state)
  list(status="applied",failure_code=NA_character_,state=state,
       state_hash=after,changed=!identical(before,after))
}

#' Validate threshold permissions and held-out group separation
#' @param policies Canonical threshold-policy table.
#' @param instances Canonical threshold-instance table.
#' @param memberships Canonical partition-membership table.
#' @return `TRUE`, otherwise fails closed.
#' @export
stpd_gate_b_v3_validate_threshold_permissions <- function(
    policies,instances,memberships) {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  # This focused permission validator deliberately accepts only the three
  # permission tables.  Validate their exact local schemas first, then enforce
  # their cross-table relationship explicitly; unrelated evidence/registry FKs
  # are validated when the complete canonical product is validated.
  stpd_gate_b_v3_validate_table_prototype(
    policies,"threshold_policies",bundle,tables=NULL
  )
  stpd_gate_b_v3_validate_table_prototype(
    instances,"threshold_instances",bundle,tables=NULL
  )
  stpd_gate_b_v3_validate_table_prototype(
    memberships,"partition_memberships",bundle,tables=NULL
  )
  if (nrow(instances)>0L) {
    policy_index <- match(
      instances$threshold_policy_id, policies$threshold_policy_id
    )
    if (anyNA(policy_index) ||
        any(instances$path != policies$path[policy_index]) ||
        any(instances$units != policies$units[policy_index])) {
      stop("Gate B v3 threshold instance does not match its policy.",
           call.=FALSE)
    }
    source <- policies$threshold_source_class[policy_index]
    adaptation <- policies$adaptation_unit[policy_index]
    legal_scope <-
      (source=="fixed_preregistered" &
         instances$application_scope=="global") |
      (source=="development_trained" & adaptation=="none" &
         instances$application_scope=="global") |
      (source=="development_trained" & adaptation=="session" &
         instances$application_scope=="session") |
      (source=="per_train_unsupervised" &
         instances$application_scope=="train") |
      (source=="batch_transductive" &
         instances$application_scope=="dataset_batch")
    if (!all(legal_scope)) {
      stop("Gate B v3 threshold application scope is illegal.",call.=FALSE)
    }
  }
  if (nrow(policies)>0L) {
    for (i in seq_len(nrow(policies))) {
      development <- policies$threshold_source_class[[i]]=="development_trained"
      partition <- policies$partition_id[[i]]
      if (development) {
        rows <- memberships[!is.na(partition) &
          memberships$partition_id==partition,,drop=FALSE]
        if (is.na(partition) || nrow(rows)==0L ||
            !any(rows$group_role %in% c("development","calibration"))) {
          stop("Development-trained threshold policy has no closed non-empty derivation partition.",
               call.=FALSE)
        }
      } else if (!is.na(partition)) {
        stop("Non-development threshold policy may not read a labelled partition.",
             call.=FALSE)
      }
      if (!any(instances$threshold_policy_id==
               policies$threshold_policy_id[[i]])) {
        stop("Gate B v3 threshold policy has no materialized instance.",
             call.=FALSE)
      }
    }
  }
  if (nrow(memberships)>0L) {
    key <- paste(memberships$partition_id,memberships$group_id_hash,sep="\u001f")
    split_roles <- split(memberships$group_role,key)
    contaminated <- names(split_roles)[vapply(split_roles,function(x)
      all(c("calibration","validation") %in% x),logical(1))]
    if (length(contaminated)>0L) {
      stop("Calibration/validation group overlap is forbidden.",call.=FALSE)
    }
    if (any(!grepl("^[0-9a-f]{64}$",memberships$source_input_hash))) {
      stop("Partition membership source identity is not canonical.",call.=FALSE)
    }
    stpd_gate_b_v3_validate_partition_holdout(memberships)
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_partition_holdout <- function(memberships) {
  if (!is.data.frame(memberships)) {
    stop("Gate B v3 partition holdout input must be a data frame.",call.=FALSE)
  }
  required <- c("partition_id","group_role","patient_group_id_hash",
                "session_group_id_hash")
  if (!all(required %in% names(memberships))) {
    stop("Gate B v3 partition holdout input is incomplete.",call.=FALSE)
  }
  if (nrow(memberships)==0L) return(invisible(TRUE))
  derivation <- memberships$group_role %in% c("development","calibration")
  validation <- memberships$group_role=="validation"
  for (partition in unique(memberships$partition_id)) {
    in_partition <- memberships$partition_id==partition
    derivation_session <- unique(memberships$session_group_id_hash[
      in_partition & derivation])
    validation_session <- unique(memberships$session_group_id_hash[
      in_partition & validation])
    if (anyNA(derivation_session) || anyNA(validation_session)) {
      stop("Partition membership sessions must be nonmissing.",call.=FALSE)
    }
    if (length(intersect(derivation_session,validation_session))>0L) {
      stop("Development/calibration and validation sessions must be held out.",
           call.=FALSE)
    }
    derivation_patient <- unique(memberships$patient_group_id_hash[
      in_partition & derivation & !is.na(memberships$patient_group_id_hash)])
    validation_patient <- unique(memberships$patient_group_id_hash[
      in_partition & validation & !is.na(memberships$patient_group_id_hash)])
    if (length(intersect(derivation_patient,validation_patient))>0L) {
      stop("Development/calibration and validation patients must be held out.",
           call.=FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_partition_input_binding <- function(
    memberships,normalized) {
  if (!is.data.frame(memberships) || !is.data.frame(normalized)) {
    stop("Gate B v3 partition/input binding requires data frames.",call.=FALSE)
  }
  membership_fields <- c("source_input_hash","patient_group_id_hash",
                         "session_group_id_hash")
  normalized_fields <- c("normalized_timestamp_bytes_sha256",
                         "patient_group_id_hash","session_group_id_hash")
  if (!all(membership_fields %in% names(memberships)) ||
      !all(normalized_fields %in% names(normalized))) {
    stop("Gate B v3 partition/input binding input is incomplete.",call.=FALSE)
  }
  if (nrow(memberships)==0L) return(invisible(TRUE))
  same_nullable <- function(left,right) {
    identical(is.na(left),is.na(right)) &&
      identical(unname(left[!is.na(left)]),unname(right[!is.na(right)]))
  }
  for (i in seq_len(nrow(memberships))) {
    hit <- which(normalized$normalized_timestamp_bytes_sha256==
                   memberships$source_input_hash[[i]])
    if (length(hit)!=1L ||
        !same_nullable(memberships$patient_group_id_hash[[i]],
                       normalized$patient_group_id_hash[[hit]]) ||
        !identical(memberships$session_group_id_hash[[i]],
                   normalized$session_group_id_hash[[hit]])) {
      stop(paste(
        "Gate B v3 partition membership does not uniquely bind its",
        "normalized timestamp/patient/session input row."
      ),call.=FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_threshold_source_closure <- function(tables) {
  policies <- tables$threshold_policies
  instances <- tables$threshold_instances
  policy_source <- tables$threshold_policy_source_manifest
  instance_source <- tables$threshold_instance_source_manifest
  requested <- tables$requested_params_manifest
  normalized <- tables$normalized_input_manifest
  memberships <- tables$partition_membership_source_manifest
  registry <- tables$contract_registries
  canonical <- stpd_gate_b_v3_canonical_json
  hash <- stpd_gate_b_v3_hash_raw
  null_value <- function(x) if (length(x)==1L && is.na(x)) NULL else x
  row_object <- function(x,i,exclude=character()) {
    fields <- setdiff(names(x),exclude)
    out <- lapply(fields,function(name) null_value(
      if (is.list(x[[name]])) x[[name]][[i]] else x[[name]][[i]]))
    names(out) <- fields
    out
  }
  materialized_memberships <- tables$partition_memberships
  membership_fields <- names(memberships)
  if (nrow(materialized_memberships)!=nrow(memberships) ||
      !all(membership_fields %in% names(materialized_memberships)) ||
      !all(vapply(membership_fields, function(field) {
        identical(unname(materialized_memberships[[field]]),
                  unname(memberships[[field]]))
      }, logical(1)))) {
    stop("Gate B v3 partition source/materialized membership closure is invalid.",
         call.=FALSE)
  }
  stpd_gate_b_v3_validate_partition_input_binding(memberships,normalized)
  if (nrow(policy_source)!=nrow(policies) || nrow(instance_source)!=nrow(instances)) {
    stop("Gate B v3 threshold source manifests are not exact one-to-one projections.",
         call.=FALSE)
  }
  policy_source_hash <- vapply(seq_len(nrow(policy_source)),function(i)
    hash("stpd-threshold-policy-source-row-v1",canonical(row_object(
      policy_source,i,"policy_source_row_hash"))),character(1))
  if (!identical(unname(policy_source$policy_source_row_hash),
                 unname(policy_source_hash))) {
    stop("Gate B v3 threshold policy source-row hash is invalid.",call.=FALSE)
  }
  policy_source_index <- integer(nrow(policies))
  for (i in seq_len(nrow(policies))) {
    hit <- which(policy_source$path==policies$path[[i]] &
      policy_source$threshold_source_class==policies$threshold_source_class[[i]] &
      policy_source$adaptation_unit==policies$adaptation_unit[[i]] &
      policy_source$policy_parameter_hash==policies$policy_parameter_hash[[i]] &
      ((is.na(policy_source$partition_id) & is.na(policies$partition_id[[i]])) |
       (!is.na(policy_source$partition_id) &
          policy_source$partition_id==policies$partition_id[[i]])))
    if (length(hit)!=1L) stop("Gate B v3 threshold policy/source expected set is invalid.",
                              call.=FALSE)
    policy_source_index[[i]] <- hit
    algorithm <- match(policies$threshold_algorithm_registry_id[[i]],
                       registry$registry_entry_id)
    requested_row <- which(requested$path==policies$path[[i]])
    if (is.na(algorithm) || registry$registry_domain[[algorithm]]!="threshold_algorithm" ||
        length(requested_row)!=1L ||
        policy_source$units[[hit]]!=policies$units[[i]] ||
        policy_source$label_access[[hit]]!=policies$label_access[[i]] ||
        policy_source$threshold_algorithm_semantic_sha256[[hit]]!=
          registry$semantic_definition_sha256[[algorithm]] ||
        policy_source$requested_value_hash[[hit]]!=
          policies$requested_value_hash[[i]] ||
        policies$requested_value_hash[[i]]!=
          requested$canonical_value_hash[[requested_row]] ||
        !identical(policy_source$frozen_before_validation[[hit]],
                   policies$frozen_before_validation[[i]])) {
      stop("Gate B v3 materialized threshold policy differs from its root-free source row.",
           call.=FALSE)
    }
  }
  normalized_rows <- function(rows) lapply(rows,function(i)
    row_object(normalized,i))
  membership_rows <- function(rows) lapply(rows,function(i)
    row_object(memberships,i))
  for (i in seq_len(nrow(instances))) {
    policy_i <- match(instances$threshold_policy_id[[i]],policies$threshold_policy_id)
    if (is.na(policy_i)) stop("Threshold instance has no policy.",call.=FALSE)
    ps <- policy_source_index[[policy_i]]
    source <- policies$threshold_source_class[[policy_i]]
    scope <- instances$application_scope[[i]]
    app_rows <- switch(scope,
      global=seq_len(nrow(normalized)),
      train=which(normalized$train==instances$train[[i]]),
      session=which(normalized$session_group_id_hash==instances$session_group_id_hash[[i]]),
      dataset_batch=which(normalized$dataset_batch_id_hash==instances$dataset_batch_id_hash[[i]]),
      integer())
    if (length(app_rows)==0L) {
      stop("Gate B v3 threshold application input set is empty.",call.=FALSE)
    }
    application_hash <- hash("stpd-threshold-application-input-v1",
      canonical(normalized_rows(app_rows)))
    derivation_rows <- switch(source,
      fixed_preregistered=list(),
      development_trained={
        p <- policies$partition_id[[policy_i]]
        rows <- which(memberships$partition_id==p &
          memberships$group_role %in% c("development","calibration"))
        membership_rows(rows)
      },
      per_train_unsupervised=normalized_rows(app_rows),
      batch_transductive=normalized_rows(app_rows),
      stop("Unknown threshold source class.",call.=FALSE))
    if (source=="development_trained" && length(derivation_rows)==0L) {
      stop("Development threshold derivation set is empty.",call.=FALSE)
    }
    derivation_hash <- hash("stpd-threshold-derivation-input-v1",
      canonical(list(threshold_source_class=source,
                     authorized_derivation_rows=derivation_rows)))
    source_scope_key <- switch(scope,
      global=hash("stpd-global-source-scope-v1",canonical(list(
        application_input_manifest_hash=application_hash))),
      train=hash("stpd-train-source-scope-v1",canonical(list(
        application_input_manifest_hash=application_hash,
        train=instances$train[[i]]))),
      session=hash("stpd-session-source-scope-v1",canonical(list(
        application_input_manifest_hash=application_hash,
        session_group_id_hash=instances$session_group_id_hash[[i]]))),
      dataset_batch=hash("stpd-dataset-batch-source-scope-v1",canonical(list(
        application_input_manifest_hash=application_hash,
        dataset_batch_id_hash=instances$dataset_batch_id_hash[[i]]))))
    scope_key <- hash("stpd-materialized-threshold-scope-v1",canonical(list(
      detection_root_id=instances$detection_root_id[[i]],
      application_scope=scope,source_scope_key_hash=source_scope_key)))
    effective_hash <- hash("stpd-effective-threshold-value-v1",canonical(list(
      value_type=instances$value_type[[i]],
      value_num=null_value(instances$value_num[[i]]),
      value_chr=null_value(instances$value_chr[[i]]),units=instances$units[[i]])))
    hit <- which(instance_source$policy_source_row_hash==
      policy_source$policy_source_row_hash[[ps]] &
      instance_source$path==instances$path[[i]] &
      instance_source$application_scope==scope &
      instance_source$source_scope_key_hash==source_scope_key)
    if (length(hit)!=1L ||
        instances$derivation_input_manifest_hash[[i]]!=derivation_hash ||
        instances$application_input_manifest_hash[[i]]!=application_hash ||
        instances$source_scope_key_hash[[i]]!=source_scope_key ||
        instances$application_scope_key_hash[[i]]!=scope_key ||
        instances$effective_value_hash[[i]]!=effective_hash) {
      stop("Gate B v3 threshold instance/source/input hash closure is invalid.",
           call.=FALSE)
    }
    compare <- c("train","session_group_id_hash","dataset_batch_id_hash",
      "derivation_input_manifest_hash","application_input_manifest_hash",
      "value_type","value_num","value_chr","units","effective_value_hash")
    same <- vapply(compare,function(name) identical(
      null_value(instance_source[[name]][[hit]]),
      null_value(instances[[name]][[i]])),logical(1))
    if (!all(same)) stop("Materialized threshold instance differs from source manifest.",
                         call.=FALSE)
    stats_pairs <- list(
      c("derivation_statistics_schema_registry_id","derivation_statistics_json"),
      c("application_statistics_schema_registry_id","application_statistics_json"))
    for (pair in stats_pairs) {
      schema_missing <- is.na(instance_source[[pair[[1L]]]][[hit]])
      json_missing <- is.na(instance_source[[pair[[2L]]]][[hit]])
      if (xor(schema_missing,json_missing)) {
        stop("Threshold statistics schema/JSON presence must be paired.",call.=FALSE)
      }
    }
    evidence_source_hash <- hash("stpd-threshold-instance-evidence-source-v1",
      canonical(list(
        threshold_algorithm_semantic_sha256=
          policy_source$threshold_algorithm_semantic_sha256[[ps]],
        policy_source_row_hash=policy_source$policy_source_row_hash[[ps]],
        path=instances$path[[i]],application_scope=scope,
        source_scope_key_hash=source_scope_key,
        derivation_input_manifest_hash=derivation_hash,
        application_input_manifest_hash=application_hash,
        derivation_statistics_schema_registry_id=null_value(
          instance_source$derivation_statistics_schema_registry_id[[hit]]),
        derivation_statistics_json=null_value(
          instance_source$derivation_statistics_json[[hit]]),
        application_statistics_schema_registry_id=null_value(
          instance_source$application_statistics_schema_registry_id[[hit]]),
        application_statistics_json=null_value(
          instance_source$application_statistics_json[[hit]]),
        effective_value_hash=effective_hash)))
    if (instance_source$instance_evidence_source_hash[[hit]]!=evidence_source_hash) {
      stop("Gate B v3 threshold instance evidence-source hash is invalid.",
           call.=FALSE)
    }
  }
  bindings <- tables$threshold_instance_bindings
  if (nrow(bindings)>0L) {
    index <- match(bindings$threshold_instance_id,instances$threshold_instance_id)
    if (anyNA(index) || any(bindings$path!=instances$path[index])) {
      stop("Gate B v3 threshold binding path/instance closure is invalid.",
           call.=FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_identity_hash_closure <- function(tables,bundle) {
  canonical_bytes <- function(name) rawToChar(
    stpd_gate_b_v3_canonical_table_bytes(tables[[name]],name,bundle))
  leaf <- function(name,domain) stpd_gate_b_v3_hash_raw(
    domain,canonical_bytes(name))
  leaves <- list(
    normalized_input_manifest_hash=leaf("normalized_input_manifest",
      "stpd-normalized-input-manifest-v1"),
    requested_params_hash=leaf("requested_params_manifest",
      "stpd-requested-params-v1"),
    partition_membership_manifest_hash=leaf(
      "partition_membership_source_manifest","stpd-partition-membership-v1"),
    threshold_policy_source_manifest_hash=leaf(
      "threshold_policy_source_manifest","stpd-threshold-policy-source-v1"),
    threshold_instance_source_manifest_hash=leaf(
      "threshold_instance_source_manifest","stpd-threshold-instance-source-v1"),
    qc_input_manifest_hash=leaf("qc_input_manifest","stpd-qc-input-v1"),
    source_manifest_hash=leaf("source_manifest","stpd-source-manifest-v1"),
    native_manifest_hash=leaf("native_manifest","stpd-native-manifest-v1"),
    toolchain_manifest_hash=leaf("toolchain_manifest",
      "stpd-toolchain-manifest-v1"),
    detector_policy_hash=leaf("detector_policy_manifest",
      "stpd-detector-policy-manifest-v1"),
    label_blind_contract_hash=leaf("label_blind_contract_manifest",
      "stpd-label-blind-contract-manifest-v1"))
  package_version <- tryCatch(as.character(utils::packageVersion(
    "SpikeTrainPatternDetector")),error=function(e) {
      desc <- read.dcf("DESCRIPTION",fields="Version")
      as.character(desc[[1L]])
    })
  code_identity_hash <- stpd_gate_b_v3_hash_raw("stpd-code-identity-v1",
    stpd_gate_b_v3_canonical_json(list(
      source_manifest_hash=leaves$source_manifest_hash,
      native_manifest_hash=leaves$native_manifest_hash,
      package_version=package_version,
      toolchain_manifest_hash=leaves$toolchain_manifest_hash)))
  source_context_hash <- stpd_gate_b_v3_hash_raw("stpd-detection-root-v1",
    stpd_gate_b_v3_canonical_json(c(leaves[c(
      "normalized_input_manifest_hash","requested_params_hash",
      "threshold_policy_source_manifest_hash",
      "threshold_instance_source_manifest_hash",
      "partition_membership_manifest_hash","qc_input_manifest_hash")],
      list(code_identity_hash=code_identity_hash,
           detector_policy_hash=leaves$detector_policy_hash,
           label_blind_contract_hash=leaves$label_blind_contract_hash))))
  identity <- tables$materialized_product_identity
  if (!identical(identity$source_context_hash[[1L]],source_context_hash) ||
      !identical(identity$detection_root_id[[1L]],
                 paste0("dr_",source_context_hash))) {
    stop("Gate B v3 detection-root source-context hash closure is invalid.",
         call.=FALSE)
  }
  manifest <- tables$canonical_manifest
  inventory <- names(bundle$product_inventory)
  if (nrow(manifest)!=length(inventory) ||
      !setequal(manifest$table_name,inventory)) {
    stop("Gate B v3 canonical manifest inventory expected set is invalid.",
         call.=FALSE)
  }
  for (name in inventory) {
    i <- match(name,manifest$table_name)
    table_hash <- digest::digest(
      stpd_gate_b_v3_canonical_table_bytes(tables[[name]],name,bundle),
      algo="sha256",serialize=FALSE)
    sort_code <- paste0("sort_contract_",name,"_v1")
    sort_registry_row <- which(
      tables$contract_registries$registry_domain == "operational_policy" &
        tables$contract_registries$code == sort_code &
        tables$contract_registries$active
    )
    sort_semantic_ok <- FALSE
    if (length(sort_registry_row) == 1L) {
      sort_semantic <- tryCatch(jsonlite::fromJSON(
        tables$contract_registries$semantic_definition_json[[sort_registry_row]],
        simplifyVector = FALSE
      ), error = function(e) NULL)
      formula <- sort_semantic$formula_or_state_machine %||% list()
      observed_sort_key <- unlist(formula$sort_key,use.names=FALSE)
      expected_sort_key <- unlist(bundle$tables[[name]]$sort_key,
                                  use.names=FALSE)
      sort_semantic_ok <-
        identical(formula$table_name,name) &&
        identical(formula$table_schema,bundle$tables[[name]]$table_schema) &&
        identical(observed_sort_key,expected_sort_key) &&
        identical(formula$character_order,"NFC_UTF8_byte_order") &&
        identical(formula$null_order,"typed_NA_before_nonmissing") &&
        identical(formula$stable_tie_break,"exact_primary_key")
    }
    if (manifest$product_kind[[i]]!=identity$product_kind[[1L]] ||
        manifest$table_schema[[i]]!=bundle$tables[[name]]$table_schema ||
        manifest$row_count[[i]]!=nrow(tables[[name]]) ||
        manifest$canonical_table_sha256[[i]]!=table_hash ||
        !isTRUE(manifest$required[[i]]) ||
        length(sort_registry_row)!=1L || !isTRUE(sort_semantic_ok) ||
        manifest$sort_contract_registry_id[[i]] !=
          tables$contract_registries$registry_entry_id[[sort_registry_row]]) {
      stop("Gate B v3 canonical manifest row differs from its typed table.",
           call.=FALSE)
    }
  }
  manifest_core_hash <- stpd_gate_b_v3_hash_raw(
    "stpd-canonical-manifest-core-v1",canonical_bytes("canonical_manifest"))
  parent <- if (is.na(identity$parent_auto_product_hash[[1L]])) NULL else
    identity$parent_auto_product_hash[[1L]]
  history <- if (is.na(identity$history_head_hash[[1L]])) NULL else
    identity$history_head_hash[[1L]]
  product_context_hash <- stpd_gate_b_v3_hash_raw("stpd-product-context-v1",
    stpd_gate_b_v3_canonical_json(list(
      product_kind=identity$product_kind[[1L]],
      detection_root_id=identity$detection_root_id[[1L]],
      parent_auto_product_hash=parent,history_head_hash=history,
      canonical_manifest_core_hash=manifest_core_hash,
      contract_sha256=identity$contract_sha256[[1L]],
      schema_contract_sha256=identity$schema_contract_sha256[[1L]])))
  product_hash <- stpd_gate_b_v3_hash_raw("stpd-product-v3",
    stpd_gate_b_v3_canonical_json(list(
      identity_schema=identity$identity_schema[[1L]],
      product_kind=identity$product_kind[[1L]],
      detection_root_id=identity$detection_root_id[[1L]],
      source_context_hash=identity$source_context_hash[[1L]],
      product_context_hash=product_context_hash,
      parent_auto_product_hash=parent,history_head_hash=history,
      canonical_manifest_core_hash=manifest_core_hash,
      contract_sha256=identity$contract_sha256[[1L]],
      schema_contract_sha256=identity$schema_contract_sha256[[1L]])))
  if (!identical(identity$canonical_manifest_core_hash[[1L]],manifest_core_hash) ||
      !identical(identity$product_context_hash[[1L]],product_context_hash) ||
      !identical(identity$product_hash[[1L]],product_hash) ||
      !identical(identity$contract_sha256[[1L]],bundle$normative_contract_sha256) ||
      !identical(identity$schema_contract_sha256[[1L]],
                 bundle$schema_contract_sha256)) {
    stop("Gate B v3 materialized product identity/hash DAG closure is invalid.",
         call.=FALSE)
  }
  invisible(TRUE)
}

#' Validate Gate B v3 diagnostic/artifact privacy redaction
#' @param object Nested diagnostic, log, or artifact metadata.
#' @return `TRUE`, otherwise fails closed before bundle creation.
#' @export
stpd_gate_b_v3_validate_redacted_artifacts <- function(object) {
  if (!is.list(object) || length(object)==0L || is.null(names(object)) ||
      any(!nzchar(names(object))) || anyDuplicated(names(object)) ||
      !identical(names(attributes(object)),"names")) {
    stop("Gate B v3 portable audit metadata requires a nonempty named object.",
         call.=FALSE)
  }
  registry <- stpd_gate_b_v3_phase1_registry(TRUE)
  schema_row <- which(
    registry$registry_domain=="diagnostic_detail_schema" &
      registry$code=="portable_audit_metadata_v1" & registry$active
  )
  if (length(schema_row)!=1L) {
    stop("Gate B v3 portable audit redaction schema is unavailable.",call.=FALSE)
  }
  schema <- jsonlite::fromJSON(
    registry$semantic_definition_json[[schema_row]],simplifyVector=FALSE
  )$formula_or_state_machine
  root_fields <- unlist(schema$root_fields,use.names=FALSE)
  detail_fields <- unlist(schema$detail_fields,use.names=FALSE)
  counter_fields <- unlist(schema$counter_fields,use.names=FALSE)
  if (any(!names(object) %in% root_fields)) {
    stop("Unknown key in signed Gate B v3 redaction schema: ",
         paste(setdiff(names(object),root_fields),collapse=", "),call.=FALSE)
  }
  scalar_string <- function(value,allowed,field) {
    if (!is.character(value) || length(value)!=1L || is.na(value) ||
        !is.null(attributes(value)) ||
        !nzchar(value) || !identical(value,stpd_gate_b_v3_strict_nfc(value)) ||
        nchar(enc2utf8(value),type="bytes")>
          as.integer(schema$maximum_string_bytes) || !value %in% allowed) {
      stop("Uncontrolled value in Gate B v3 redaction field: ",field,
           call.=FALSE)
    }
    invisible(TRUE)
  }
  exact_nonnegative_integer <- function(value,field) {
    if (!is.numeric(value) || length(value)!=1L || is.na(value) ||
        !is.null(attributes(value)) || !is.finite(value) || value<0 ||
        value!=floor(value)) {
      stop("Gate B v3 redaction count is not an exact non-negative integer: ",
           field,call.=FALSE)
    }
    invisible(TRUE)
  }
  active_codes <- function(domains) registry$code[
    registry$registry_domain %in% domains & registry$active
  ]
  if ("diagnostic_code" %in% names(object)) scalar_string(
    object$diagnostic_code,active_codes("diagnostic_code"),"diagnostic_code"
  )
  if ("stage" %in% names(object)) scalar_string(
    object$stage,unique(c(stpd_gate_b_v3_complexity_contract()$stage,
                          "resource_budget","cancelled")),"stage"
  )
  if ("status_code" %in% names(object)) scalar_string(
    object$status_code,
    unlist(stpd_gate_b_v3_phase1_bundle()$enums$consumer_status,
           use.names=FALSE),"status_code"
  )
  train_pair <- c("completed_trains","total_trains") %in% names(object)
  if (xor(train_pair[[1L]],train_pair[[2L]])) {
    stop("Gate B v3 completed/total train counts must be supplied as a pair.",
         call.=FALSE)
  }
  if (all(train_pair)) {
    exact_nonnegative_integer(object$completed_trains,"completed_trains")
    exact_nonnegative_integer(object$total_trains,"total_trains")
    if (object$completed_trains>object$total_trains) {
      stop("Gate B v3 completed train count exceeds total trains.",call.=FALSE)
    }
  }
  if ("counters" %in% names(object)) {
    counters <- object$counters
    if (!is.list(counters) || length(counters)==0L ||
        is.null(names(counters)) || any(!nzchar(names(counters))) ||
        anyDuplicated(names(counters)) ||
        !identical(names(attributes(counters)),"names") ||
        any(!names(counters) %in% counter_fields)) {
      stop("Unknown key in signed Gate B v3 redaction counters schema.",
           call.=FALSE)
    }
    for (field in names(counters)) {
      exact_nonnegative_integer(counters[[field]],paste0("counters.",field))
    }
  }
  if ("detail" %in% names(object)) {
    detail <- object$detail
    if (!is.list(detail) || length(detail)==0L || is.null(names(detail)) ||
        any(!nzchar(names(detail))) || anyDuplicated(names(detail)) ||
        !identical(names(attributes(detail)),"names") ||
        any(!names(detail) %in% detail_fields)) {
      stop("Unknown key in signed Gate B v3 redaction detail schema.",
           call.=FALSE)
    }
    metric_pair <- c("metric","value") %in% names(detail)
    if (xor(metric_pair[[1L]],metric_pair[[2L]])) {
      stop("Gate B v3 detail metric/value must be supplied as a pair.",
           call.=FALSE)
    }
    if (all(metric_pair)) {
      scalar_string(detail$metric,counter_fields,"detail.metric")
      if (!is.numeric(detail$value) || length(detail$value)!=1L ||
          is.na(detail$value) || !is.null(attributes(detail$value)) ||
          !is.finite(detail$value)) {
        stop("Gate B v3 detail value must be one finite numeric scalar.",
             call.=FALSE)
      }
    }
    if ("code" %in% names(detail)) scalar_string(
      detail$code,active_codes(c("diagnostic_code","failure_code","qc_status")),
      "detail.code"
    )
    if ("reason" %in% names(detail)) scalar_string(
      detail$reason,active_codes("reason_code"),"detail.reason"
    )
    if ("entity_domain" %in% names(detail)) scalar_string(
      detail$entity_domain,
      stpd_gate_b_v3_entity_domain_registry()$entity_domain,
      "detail.entity_domain"
    )
  }
  invisible(TRUE)
}

stpd_gate_b_v3_registry_semantic <- function(
    clause, input, output, rule, parameters = character(),
    edge_cases = character()) {
  list(
    definition_schema = "stpd_registry_semantic_definition_v1",
    contract_clause_id = clause,
    input_schema_id = input,
    output_schema_id = output,
    formula_or_state_machine = rule,
    parameter_paths = as.list(parameters),
    edge_cases = as.list(edge_cases)
  )
}

#' Materialize the active Gate B v3 registry
#' @param verify_materialized Compare live rows with the installed canonical
#'   registry artifact when it is available.
#' @return An exact `contract_registries` table with content-addressed entries.
#' @export
stpd_gate_b_v3_phase1_registry <- function(verify_materialized = TRUE) {
  rows <- list()
  add <- function(domain, code, clause, input, output, machine,
                  parameters = character(), edge_cases = character()) {
    rows[[length(rows) + 1L]] <<- list(
      domain = domain, code = code,
      semantic = stpd_gate_b_v3_registry_semantic(
        clause, input, output, machine, parameters, edge_cases
      )
    )
  }
  add("estimator", "state_frequency_rate_v1", "3.4",
      "label_blind_isi_run_v1", "state_frequency_evidence_v1",
      list(operation="divide", numerator="n_valid_isi",
           denominator="sum_isi_sec_ascending_isi_index",
           output="rate_hz", reject_if=c("n_valid_isi<1","denominator<=0")),
      c("state.frequency.high_enter_hz", "state.frequency.high_exit_hz"),
      c("enter equality passes", "gray zone abstains"))
  add("estimator", "state_regularity_median_cv2_v1", "3.4",
      "label_blind_isi_run_v1", "state_regularity_evidence_v1",
      list(pair_formula="2*abs(isi[i+1]-isi[i])/(isi[i+1]+isi[i])",
           aggregation="median_finite_pairs", output="median_cv2",
           reject_if="finite_pair_count<minimum_pairs"),
      c("state.regularity.regular_upper_cv2",
        "state.regularity.irregular_lower_cv2",
        "state.state_min_valid_isi", "state.state_min_duration_sec"),
      c("zero pair denominator rejects", "gray zone abstains"))
  add("estimator", "pause_local_window_median_v1", "4.1",
      "label_blind_local_isi_window_v1", "pause_gap_evidence_v1",
      list(reference="symmetric_parent_spine_window_excluding_guard_band",
           aggregation="median_finite_reference_isi",
           ratio="candidate_isi_sec/local_reference_sec",
           raw_p="rank_survival_with_plus_one_correction",
           adjustment="benjamini_hochberg_complete_family",
           reject_if=c("valid_local_n_isi<local_min_valid_isi",
                       "local_reference_sec<=0","hard_boundary_crossed")),
      c("pause.scan_seed_sec","pause.absolute_threshold_sec",
        "pause.local_ratio_min","pause.local_window_isi",
        "pause.guard_band_isi","pause.local_min_valid_isi",
        "pause.score_alpha"),
      c("candidate and guard band are excluded from the reference",
        "complete family is required before adjustment"))
  add("estimator", "event_extent_n_spikes_v1", "4.3",
      "accepted_event_geometry_v1", "event_extent_modifier_evidence_v1",
      list(statistic="event.n_spikes",classic="stat<long_min_spikes",
           long="long_min_spikes<=stat<prolonged_min_spikes",
           prolonged="stat>=prolonged_min_spikes"),
      c("event.extent.long_min_spikes",
        "event.extent.prolonged_min_spikes"),
      c("long equality passes", "prolonged equality passes"))
  add("estimator", "event_frequency_rate_hz_v1", "4.3",
      "accepted_event_geometry_v1", "event_frequency_modifier_evidence_v1",
      list(statistic="event.n_isi/event.duration_sec",
           high="rate>=high_enter_hz",non_high="rate<=high_exit_hz",
           unresolved="high_exit_hz<rate<high_enter_hz"),
      c("event.frequency.high_enter_hz",
        "event.frequency.high_exit_hz"),
      c("enter equality passes", "exit equality is non-high"))
  add("episode_aggregation", "state_episode_nonrecursive_connector_v1", "4.2",
      "ordered_direct_support_and_gap_rows_v1", "state_episode_segments_v1",
      list(scan="single_left_to_right", recursion_generation=0L,
           connect_if_all=c("same_state_class","no_hard_boundary",
                            "gap_not_canonical_pause","gap_count_within_limit",
                            "gap_duration_within_limit","gap_local_ratio_within_limit",
                            "connector_fraction_within_limit","episode_span_within_limit",
                            "both_sides_pass_minimum_sample","axis_equivalence_pass"),
           retransitive_closure=FALSE),
      c("state.connector.max_gap_sec", "state.connector.max_gap_count",
        "state.connector.max_gap_local_ratio",
        "state.connector.max_cumulative_fraction",
        "state.connector.max_episode_span_sec",
        "state.state_min_valid_isi", "state.state_min_duration_sec"),
      c("canonical Pause always splits", "hard boundary always splits"))
  add("gap_policy", "pause_local_empirical_bh_v1", "4.1",
      "label_blind_local_isi_reference_v1", "complete_gap_candidate_family_v1",
      list(candidate_scan="all_parent_spine_single_isi_rows_at_or_above_scan_seed",
           empirical_upper_tail_p="(1+count(reference_isi>=candidate_isi))/(n_reference+1)",
           adjustment="BH_over_complete_candidate_family",
           accept_if=c("absolute_threshold_pass","local_ratio_pass",
                       "adjusted_q<=alpha","qc_eligible","not_boundary_censored"),
           fdr_control_claim=FALSE),
      c("pause.scan_seed_sec","pause.absolute_threshold_sec",
        "pause.local_ratio_min","pause.local_window_isi",
        "pause.guard_band_isi","pause.local_min_valid_isi",
        "pause.score_alpha"),
      c("discrete score floor recorded", "insufficient local sample abstains"))
  add("equivalence_method", "connector_axis_equivalence_v1", "4.2",
      "two_state_axis_evidence_rows_v1", "connector_decision_v1",
      list(frequency_pass="abs(log(pre_rate/post_rate))<=frequency_margin",
           regularity_pass="abs(pre_median_cv2-post_median_cv2)<=regularity_margin",
           decision="connect iff both pass and all boundary predicates pass",
           hypothesis_test=FALSE),
      c("state.connector.rate_log_margin", "state.connector.cv2_margin"),
      c("missing axis abstains", "zero rate abstains"))
  add("event_selector", "event_maximal_disjoint_selector_v1", "4.3",
      "complete_event_candidate_universe_v1", "maximal_disjoint_event_set_v1",
      list(objectives=c("maximum_cardinality","maximum_total_evidence_score",
                        "lexicographic_candidate_id"),
           overlap="shared_isi", adjacent_merge=FALSE),
      "event.selector.minimum_score",
      c("nested candidates", "equal score deterministic tie"))
  add("event_candidate_rule", "structure_first_local_flank_contrast_v1", "4.3",
      "label_blind_train_isi_with_qc_v1", "complete_burst_candidate_universe_v1",
      list(scan="all_contiguous_windows_inclusive_min_max",
           seed_or_bridge_gate_used=FALSE,
           two_sided_pass="pre_ratio_q90>=contrast_min && post_ratio_q90>=contrast_min && sqrt(pre_ratio_q90*post_ratio_q90)>=geom_contrast_min",
           endpoint_pass="allow_endpoint && exactly_one_flank_ratio>=contrast_min",
           compactness="q90<=background_fraction*train_compactness_quantile",
           tolerated_internal_tail="two_sided && q90_compact && max_intra/compact_upper<=max_internal_tail_ratio && both_flanks_over_max_intra>=contrast_min",
           label_access="none",artifact_rows="ineligible"),
      c("spiketrainpattern.burst.structure_first_enabled",
        "event_core.min_spikes","event_core.classic_max_spikes",
        "detector.artifact_min_valid_isi_sec",
        "spiketrainpattern.burst.structure_first_min_isi_count",
        "spiketrainpattern.burst.structure_first_max_isi_count",
        "spiketrainpattern.burst.structure_first_contrast_min",
        "spiketrainpattern.burst.structure_first_geom_contrast_min",
        "spiketrainpattern.burst.structure_first_compactness_quantile",
        "spiketrainpattern.burst.structure_first_background_fraction",
        "spiketrainpattern.burst.structure_first_min_train_valid_isi",
        "spiketrainpattern.burst.structure_first_max_internal_tail_ratio",
        "spiketrainpattern.burst.structure_first_allow_endpoint",
        "spiketrainpattern.burst.allow_one_sided_as_canonical"),
      c("homogeneous train yields no candidate",
        "weak flank contrast rejects","threshold equality passes",
        "artifact-containing window rejects","label-bearing input is stripped"))
  add("operational_policy", "boundary_precedence_v1", "4.1-4.3",
      "candidate_and_boundary_rows_v1", "compatibility_action_v1",
      list(precedence=c("qc_or_acquisition_hard","canonical_pause",
                        "gap_unresolved","state_or_event_candidate"),
           canonical_pause_hard_for_state=TRUE,
           canonical_pause_hard_for_event=TRUE,
           canonical_pause_hard_for_episode_link=FALSE),
      character(), "Pause below connector tolerance still splits State support")
  add("operational_policy", "resource_budget_v1", "13.1",
      "versioned_resource_counters_v1", "typed_resource_status_v1",
      list(check="before allocation and before canonical commit",
           exceed="failed_closed/resource_budget_exceeded",
           truncation=FALSE, cancellation_cleanup="zero published canonical tables",
           counter_backend="deterministic_integer64_counter_v1",
           measurement_backend="peak_rss_platform_adapter_v1",
           checkpoint_order=as.list(stpd_gate_b_v3_complexity_contract()$check_point),
           exact_budget_rows=lapply(seq_len(nrow(stpd_gate_b_v3_complexity_contract())),
             function(i) as.list(stpd_gate_b_v3_complexity_contract()[i,
               c("stage","time_bound","space_bound","budget_metric","limit",
                 "check_point","counter_definition","cancellation_point",
                 "cleanup_state","fail_closed_code"),drop=FALSE]))),
      stpd_gate_b_v3_complexity_contract()$budget_metric,
      c("all-hit candidate input", "edge growth", "cancellation cleans staging"))
  sort_bundle <- stpd_gate_b_v3_phase1_bundle()
  for (table_name in names(sort_bundle$tables)) {
    spec <- sort_bundle$tables[[table_name]]
    add("operational_policy",paste0("sort_contract_",table_name,"_v1"),
        "8.4","typed_table_rows_v1","canonical_sorted_table_v1",
        list(table_name=table_name,
             table_schema=spec$table_schema,
             sort_key=spec$sort_key,
             character_order="NFC_UTF8_byte_order",
             null_order="typed_NA_before_nonmissing",
             stable_tie_break="exact_primary_key"),character(),
        "locale and input row order cannot alter canonical bytes")
  }

  transition_codes <- c(
    "event_candidate_accept", "review_candidate_reject",
    "event_projection_reject", "state_projection_reject", "state_split",
    "gap_projection_reject", "episode_link_confirm", "episode_link_reject",
    "compensate"
  )
  prediction_codes <- c("high_frequency_irregular_state", "high_frequency_tonic",
                        "tonic", "burst_event", "canonical_pause",
                        "pause_interrupted_hf")
  evidence_codes <- c("derivation_input", "application_input",
                      "application_statistics", "entity_support",
                      "review_reason", "lineage_parent", "state_axis",
                      "gap_empirical_family", "qc_eligibility",
                      "connector_axis", "event_modifier")
  binding_codes <- c("decision_gate", "classification_gate", "boundary_gate",
                     "provenance_gate")
  for (code in c("resource_budget_exceeded",
                 "unsupported_transition_for_schema_v1", "cancelled")) {
    add("failure_code", code, "6.16", "typed_failure_context_v1",
        "consumer_response_v1",
        list(status="failed_closed", code=code, canonical_tables="forbidden",
             legacy_fallback=FALSE), character(), "exact typed failure")
  }
  for (code in prediction_codes) {
    add("prediction_class", code, "2-4", "orthogonal_evidence_axes_v1",
        "prediction_class_v1",
        list(code=code, biological_ground_truth=FALSE,
             authority="prediction_record_only"), character(),
        "unknown combinations abstain")
  }
  transition_required_keys <- list(
    event_candidate_accept=c("review_candidate_id","expected_review_candidate_hash",
      "source_event_candidate_id","expected_source_candidate_hash",
      "suggested_class_registry_id","reason_registry_id"),
    review_candidate_reject=c("review_candidate_id",
      "expected_review_candidate_hash","reason_registry_id"),
    event_projection_reject=c("event_id","expected_event_hash",
      "reason_registry_id"),
    state_projection_reject=c("state_episode_id","expected_episode_hash",
      "reason_registry_id"),
    state_split=c("state_episode_id","expected_episode_hash",
      "connector_segment_id","expected_connector_segment_hash",
      "reason_registry_id"),
    gap_projection_reject=c("gap_id","expected_gap_hash",
      "reason_registry_id"),
    episode_link_confirm=c("episode_link_id","expected_link_hash",
      "reason_registry_id"),
    episode_link_reject=c("episode_link_id","expected_link_hash",
      "reason_registry_id"),
    compensate=c("compensates_transition_id","expected_effect_hash",
      "reason_registry_id")
  )
  for (code in transition_codes) {
    add("transition_payload_schema", paste0(code, "_v1"), "6.15",
        "typed_transition_request_v1", "adjudication_transition_v1",
        list(action_type=code, required_keys=transition_required_keys[[code]],
             additional_properties=FALSE, compare_and_swap=TRUE,
             append_only=TRUE), character(),
        c("unknown or extra key rejects", "compensation never deletes history"))
  }
  evidence_payload_fields <- list(
    derivation_input=c("threshold_source_class:chr!","authorized_row_hashes:lst!"),
    application_input=c("application_scope:chr!","authorized_row_hashes:lst!"),
    application_statistics=c("statistics_schema:chr!","statistics:json!"),
    entity_support=c("entity_domain_id:chr!","entity_id:chr!","support_rows:lst!"),
    review_reason=c("reason_registry_id:chr!","target_hash:chr!"),
    lineage_parent=c("parent_domain_id:chr!","parent_id:chr!","parent_hash:chr?"),
    state_axis=c("frequency_stat:dbl!","frequency_status:chr!",
                 "regularity_stat:dbl!","regularity_status:chr!",
                 "threshold_instance_ids:lst!"),
    gap_empirical_family=c("candidate_ids:lst!","raw_p_values:lst!",
                           "adjusted_q_values:lst!","method_registry_id:chr!"),
    qc_eligibility=c("hard_boundary_clear:lgl!","isi_index:int!",
                     "qc_eligibility:chr!","train:chr!"),
    connector_axis=c("pre_axis_evidence_id:chr!","post_axis_evidence_id:chr!",
                     "frequency_equivalent:lgl!","regularity_equivalent:lgl!"),
    event_modifier=c("event_id:chr!","modifier_domain:chr!",
                     "modifier_value:chr!","evidence_status:chr!",
                     "estimator_registry_id:chr!","statistic_value:dbl!",
                     "pass_bound:dbl!","gray_margin:dbl!",
                     "threshold_instance_id:chr!")
  )
  evidence_creator_domains <- list(
    derivation_input=c("threshold_algorithm"),
    application_input=c("threshold_algorithm"),
    application_statistics=c("threshold_algorithm"),
    entity_support=c("classification_rule","state_candidate_rule",
      "segment_rule","gap_policy","boundary_rule","event_candidate_rule",
      "event_selector"),
    review_reason=c("classification_rule"),
    lineage_parent=c("classification_rule","segment_rule","event_selector"),
    state_axis=c("classification_rule","state_candidate_rule","estimator",
      "episode_aggregation"),
    gap_empirical_family=c("gap_policy","estimator"),
    qc_eligibility=c("gap_policy","boundary_rule"),
    connector_axis=c("equivalence_method","segment_rule"),
    event_modifier=c("event_selector","estimator")
  )
  for (code in evidence_codes) {
    add("evidence_schema", paste0(code, "_v1"), "6.17",
        "canonical_evidence_payload_v1", "evidence_record_v1",
        list(schema_code=paste0(code, "_v1"),
             exact_ordered_fields=as.list(evidence_payload_fields[[code]]),
             compatible_creator_registry_domains=as.list(
               evidence_creator_domains[[code]]),
             additional_properties=FALSE,canonical_json_required=TRUE,
             cardinality="exactly_one_payload_per_evidence_record",
             units="fields_are_SI_or_explicit_registry_bound",
             free_text_executable_semantics=FALSE),
        character(), "schema/hash mismatch rejects")
  }
  for (code in binding_codes) {
    add("binding_role", paste0(code, "_v1"), "6.13",
        "threshold_instance_v1", "threshold_instance_binding_v1",
        list(role=code, unique_by=c("consumer_domain_id","consumer_id","path","role"),
             exact_instance_fk=TRUE), character(), "path and scope resolve exactly")
  }
  add("label_blind_rule", "fresh_reconstruction_v1", "9",
      "raw_train_and_acquisition_qc_v1", "label_blind_input_v1",
      list(allocation="fresh", allowed_sources=c("timestamp","acquisition_context","qc_input"),
           forbidden_sources=c("manual","manual_negative","review","final","learned_ranges","cache","attributes"),
           strip_copy=FALSE), character(), "metamorphic forbidden-source noninterference")
  add("input_allowlist_schema", "timestamp_acquisition_qc_allowlist_v1", "9",
      "raw_dataset_v1", "label_blind_input_v1",
      list(required=list(timestamp="finite strictly increasing binary64"),
           optional=c("timestamp_unit","timestamp_resolution_sec","timebase_provenance",
                      "waveform_qc_available","raw_waveform_available","sorting_qc_status",
                      "condition_boundary_available","patient_group_id_hash",
                      "session_group_id_hash","dataset_batch_id_hash"),
           additional_properties="ignored_not_copied"), character(),
      "manual labels and derived caches forbidden")
  add("input_source_domain", "normalized_timestamp_and_acquisition_qc_v1", "9",
      "source_manifest_leaf_v1", "normalized_input_manifest_v1",
      list(allowed_sources=c("normalized_timestamp_bytes",
          "allowlisted_acquisition_qc_scalars"),
        explicit_forbidden_sources=c("manual","manual_negative","review",
          "final","learned_ranges","cache","attributes"),
        unknown_source_action="fail_closed",label_access="none",
        root_free=TRUE), character(),
      "train pseudonym required; every used source is covered")
  add("label_blind_rule", "strip_label_review_sources_v1", "9",
      "raw_dataset_source_universe_v1", "label_blind_input_v1",
      list(action="strip", exact_sources=c("manual","manual_negative",
        "review","final"), copy_values=FALSE, copy_attributes=FALSE),
      character(), "presence and value changes cannot alter normalized input")
  add("label_blind_rule", "strip_derived_state_sources_v1", "9",
      "raw_dataset_source_universe_v1", "label_blind_input_v1",
      list(action="strip", exact_sources=c("learned_ranges","cache",
        "results","attributes"), copy_values=FALSE, copy_attributes=FALSE),
      character(), "derived state cannot enter prediction input")
  add("label_blind_rule", "forbid_unknown_input_sources_v1", "9",
      "raw_dataset_source_universe_v1", "typed_failed_closed_v1",
      list(action="forbid", unknown_source_default="failed_closed",
           executable_allowlist_required=TRUE), character(),
      "new fields are denied until the signed allowlist is versioned")
  add("input_allowlist_schema", "prohibited_label_review_sources_v1", "9",
      "raw_dataset_v1", "label_blind_input_v1",
      list(exact_sources=c("manual","manual_negative","review","final"),
           action="strip", additional_properties=FALSE), character(),
      "all label-bearing columns and attributes are excluded")
  add("input_allowlist_schema", "prohibited_derived_state_sources_v1", "9",
      "raw_dataset_v1", "label_blind_input_v1",
      list(exact_sources=c("learned_ranges","cache","results","attributes"),
           action="strip", additional_properties=FALSE), character(),
      "all learned and runtime-derived state is excluded")
  add("input_allowlist_schema", "unknown_source_rejection_v1", "9",
      "raw_dataset_v1", "typed_failed_closed_v1",
      list(known_source_universe_required=TRUE,action="forbid",
           additional_properties=FALSE),character(),
      "unknown source fails before hashing or prediction")
  add("input_source_domain", "label_and_review_sources_v1", "9",
      "source_manifest_leaf_v1", "stripped_source_record_v1",
      list(sources=c("manual","manual_negative","review","final"),
           label_access="forbidden",root_free=TRUE),character(),
      "values and attributes never copied")
  add("input_source_domain", "derived_state_sources_v1", "9",
      "source_manifest_leaf_v1", "stripped_source_record_v1",
      list(sources=c("learned_ranges","cache","results","attributes"),
           label_access="forbidden",root_free=TRUE),character(),
      "cached or learned state never copied")
  add("input_source_domain", "unknown_input_source_v1", "9",
      "source_manifest_leaf_v1", "typed_failed_closed_v1",
      list(source="any_not_explicitly_allowlisted",action="forbid",
           label_access="unknown",root_free=TRUE),character(),
      "future fields fail closed")

  add("classification_rule", "orthogonal_state_axes_v1", "3.1-3.4",
      "state_axis_evidence_v1", "state_prediction_or_abstention_v1",
      list(mapping=list(high_regular="high_frequency_tonic",
                        high_irregular="high_frequency_irregular_state",
                        non_high_regular="tonic"),
           every_other_cell="typed_abstention", default_class=FALSE),
      c("state.frequency.high_enter_hz","state.frequency.high_exit_hz",
        "state.regularity.regular_upper_cv2",
        "state.regularity.irregular_lower_cv2"),
      c("axis gray zone abstains","insufficient evidence abstains"))
  add("state_candidate_rule", "complete_state_direct_support_scan_v1", "3.4",
      "label_blind_isi_and_hard_boundaries_v1", "complete_state_candidate_universe_v1",
      list(scan="maximal contiguous eligible ISI runs", negative_rows_retained=TRUE,
           row_order_independent=TRUE),
      c("state.state_min_valid_isi","state.state_min_duration_sec"),
      c("hard boundary splits","candidate deletion fails expected-set"))
  add("segment_rule", "maximal_episode_role_partition_v1", "3.2",
      "accepted_state_candidates_and_connector_decisions_v1",
      "gap_free_episode_partition_v1",
      list(roles=c("direct_support","tolerated_connector"),
           maximal_same_role=TRUE, ordered_partition=TRUE,
           connector_is_active_support=FALSE), character(),
      c("Pause excluded","no overlapping or missing segment"))
  add("multiple_testing_method", "benjamini_hochberg_v1", "4.1",
      "complete_preregistered_pause_family_v1", "adjusted_q_values_v1",
      list(method="BH", input_order="candidate_id byte order",
           complete_family_required=TRUE, fdr_control_claim=FALSE),
      "pause.score_alpha", c("discrete p values","family deletion fails"))
  add("boundary_class", "qc_or_acquisition_hard_v1", "4.1-4.3",
      "qc_and_acquisition_evidence_v1", "hard_boundary_v1",
      list(hard_for=c("state","event","episode_link"),
           active_support=FALSE), character(), "unknown QC abstains")
  add("boundary_rule", "boundary_precedence_v1", "4.1-4.3",
      "boundary_and_candidate_rows_v1", "deterministic_boundary_action_v1",
      list(order=c("qc_or_acquisition_hard","canonical_pause",
                   "gap_unresolved","candidate"), row_order_independent=TRUE),
      character(), "Pause remains hard below connector tolerance")
  add("threshold_algorithm", "fixed_preregistered_v1", "6.13",
      "requested_parameter_manifest_v1", "global_threshold_instance_v1",
      list(source="fixed_preregistered",adaptation="none",scope="global",
           label_access="none",partition="NA"), character(),
      "application labels forbidden")
  add("threshold_algorithm", "development_trained_global_v1", "6.13",
      "development_calibration_partition_v1", "global_threshold_instance_v1",
      list(source="development_trained",adaptation="none",scope="global",
           label_access="development_reference_only",partition="required",
           validation_overlap=0L), character(), "held-out labels forbidden")
  add("threshold_algorithm", "development_trained_session_v1", "6.13",
      "development_calibration_partition_v1", "session_threshold_instance_v1",
      list(source="development_trained",adaptation="session",scope="session",
           label_access="development_reference_only",partition="required",
           application_session_labels=FALSE), character(),
      "unlabelled application statistics only")
  add("threshold_algorithm", "per_train_unsupervised_v1", "6.13",
      "one_normalized_train_row_v1", "train_threshold_instance_v1",
      list(source="per_train_unsupervised",adaptation="train",scope="train",
           label_access="none",cross_train_read=FALSE), character(),
      "unrelated train cannot change instance")
  add("threshold_algorithm", "batch_transductive_v1", "6.13",
      "one_unlabelled_batch_v1", "batch_threshold_instance_v1",
      list(source="batch_transductive",adaptation="dataset_batch",
           scope="dataset_batch",label_access="none",
           estimand="transductive_only"), character(),
      "forbidden in default new-patient estimand")
  add("lineage_action", "generation_before_parent_v1", "6.18",
      "sorted_parent_edge_payloads_v1", "lineage_records_v1",
      list(append_only=TRUE, current_product_hash="typed_na",
           cycles_forbidden=TRUE), character(), "duplicate edge rejects")
  add("evidence_role", "entity_support_v1", "6.17",
      "evidence_record_v1", "entity_evidence_edge_v1",
      list(cardinality="one_or_more",geometry_match=TRUE), character(),
      "orphan evidence rejects")
  add("evidence_type", "detector_rule_evidence_v1", "6.17",
      "canonical_detector_statistics_v1", "evidence_record_v1",
      list(label_access="none",canonical_json=TRUE), character(),
      "free text is non-executable")
  add("reason_code", "accepted_evidence_pass_v1", "3.4/4.3",
      "accepted_candidate_evidence_v1", "positive_candidate_reason_v1",
      list(code="accepted_evidence_pass_v1",positive_scientific_claim=TRUE,
           accepted_entity_reference_allowed=TRUE,
           required_evidence_status="pass"),character(),
      "may be referenced only by a candidate whose required evidence passed")
  add("reason_code", "rejected_pause_threshold_v1", "4.1",
      "complete_pause_candidate_evidence_v1", "negative_gap_candidate_reason_v1",
      list(code="rejected_pause_threshold_v1",positive_scientific_claim=FALSE,
           accepted_entity_reference_allowed=FALSE,
           required_evidence_status="complete_but_below_gate"),character(),
      "candidate remains in the complete universe but is not projected as Pause")
  add("reason_code", "abstained_pause_qc_v1", "4.1",
      "pause_candidate_qc_evidence_v1", "gap_abstention_reason_v1",
      list(code="abstained_pause_qc_v1",positive_scientific_claim=FALSE,
           accepted_entity_reference_allowed=FALSE,
           required_evidence_status="qc_ineligible_or_boundary_censored"),
      character(),"candidate remains materialized without a Pause projection")
  add("reason_code", "abstained_pause_insufficient_v1", "4.1",
      "pause_candidate_local_evidence_v1", "gap_abstention_reason_v1",
      list(code="abstained_pause_insufficient_v1",
           positive_scientific_claim=FALSE,
           accepted_entity_reference_allowed=FALSE,
           required_evidence_status="insufficient_local_reference"),
      character(),"candidate remains materialized without a Pause projection")

  controlled_sentinels <- c("reason_code","diagnostic_code",
    "diagnostic_detail_schema","evidence_role","evidence_type",
    "lineage_action","qc_status","reference_label","reference_reason",
    "species","brain_region","brain_subregion","acquisition_condition",
    "anesthesia_status","medication_status","task_status",
    "timebase_provenance","spike_sorting_method","sorting_qc_status",
    "isolation_metric_name","units","nonstationarity_status",
    "context_epoch_status")
  already_present <- vapply(rows, `[[`, character(1), "domain")
  for (domain in setdiff(controlled_sentinels,already_present)) {
    add(domain,"not_asserted_v1","6.16",paste0(domain,"_input_v1"),
        paste0(domain,"_controlled_value_v1"),
        list(code="not_asserted_v1",positive_scientific_claim=FALSE,
             accepted_entity_reference_allowed=FALSE,
             effect="typed_abstention_or_metadata_unknown"),character(),
        "cannot satisfy a positive candidate/evidence requirement")
  }
  add("diagnostic_detail_schema","portable_audit_metadata_v1","13.1",
      "portable_audit_named_object_v1","redacted_portable_audit_object_v1",
      list(
        root_fields=as.list(c("diagnostic_code","detail","stage",
          "completed_trains","total_trains","counters","status_code")),
        detail_fields=as.list(c("metric","value","code","reason",
          "entity_domain")),
        counter_fields=as.list(
          stpd_gate_b_v3_complexity_contract()$budget_metric),
        string_domains=list(
          diagnostic_code="active_diagnostic_code_registry_code",
          stage="complexity_stage_or_resource_budget_or_cancelled",
          status_code="consumer_status_enum",
          metric="complexity_budget_metric",
          code="active_diagnostic_failure_or_qc_registry_code",
          reason="active_reason_code_registry_code",
          entity_domain="active_entity_domain_registry_code"),
        unknown_keys="reject",unknown_values="reject",
        free_text_allowed=FALSE,maximum_string_bytes=128L,
        completed_total_pair_required=TRUE,metric_value_pair_required=TRUE),
      character(),
      c("hostname/operator/free text rejected","unknown value rejected"))

  # Remaining domains receive a closed sentinel which cannot be referenced by
  # an accepted scientific row. This is a typed fail-closed value, not an
  # executable fallback or positive classification rule.
  domains <- unlist(stpd_gate_b_v3_phase1_bundle()$enums$registry_domain,
                    use.names = FALSE)
  present <- vapply(rows, `[[`, character(1), "domain")
  for (domain in setdiff(domains, present)) {
    add(domain, "not_asserted_v1", "6.16", paste0(domain, "_input_v1"),
        paste0(domain, "_output_v1"),
        list(code="not_asserted_v1", decision="typed_unknown",
             executable_effect="abstain_or_not_estimable",
             fallback_to_positive_class=FALSE,
             accepted_entity_reference_allowed=FALSE), character(),
        "cannot be interpreted as positive scientific evidence")
  }

  semantic_json <- vapply(rows, function(row) {
    stpd_gate_b_v3_canonical_json(row$semantic)
  }, character(1))
  semantic_hash <- vapply(semantic_json, function(x) {
    stpd_gate_b_v3_hash_raw("stpd-registry-semantic-v1", x)
  }, character(1))
  entry_hash <- vapply(seq_along(rows), function(i) {
    payload <- stpd_gate_b_v3_canonical_json(list(
      registry_domain = rows[[i]]$domain,
      code = rows[[i]]$code,
      code_version = "1",
      semantic_definition_sha256 = semantic_hash[[i]]
    ))
    paste0("reg_", stpd_gate_b_v3_hash_raw("stpd-registry-entry-v1", payload))
  }, character(1))
  out <- data.frame(
    schema_version = rep("stpd_multitrack_v3_1", length(rows)),
    registry_entry_id = entry_hash,
    registry_domain = vapply(rows, `[[`, character(1), "domain"),
    code = vapply(rows, `[[`, character(1), "code"),
    code_version = rep("1", length(rows)),
    semantic_definition_json = semantic_json,
    semantic_definition_sha256 = semantic_hash,
    active = rep(TRUE, length(rows)), stringsAsFactors = FALSE
  )
  out <- stpd_gate_b_v3_sort_table(out, "registry_entry_id")
  stpd_gate_b_v3_validate_table_prototype(out, "contract_registries")
  if (isTRUE(verify_materialized)) {
    embedded <- stpd_gate_b_v3_phase1_bundle()$registry_semantics
    embedded_rows <- stpd_gate_b_v3_rows_from_json(
      embedded, stpd_gate_b_v3_empty_table("contract_registries")
    )
    if (!identical(stpd_gate_b_v3_canonical_table_bytes(out,"contract_registries"),
                   stpd_gate_b_v3_canonical_table_bytes(embedded_rows,
                                                        "contract_registries"))) {
      stop("Embedded Gate B v3 registry semantics differ from live rows.",
           call.=FALSE)
    }
    path <- system.file("config", "gate_b_v3_phase1_registry.json",
                        package = "SpikeTrainPatternDetector")
    if (!nzchar(path)) path <- file.path("inst", "config",
                                        "gate_b_v3_phase1_registry.json")
    if (file.exists(path)) {
      artifact <- jsonlite::read_json(path, simplifyVector = FALSE)
      artifact_rows <- stpd_gate_b_v3_rows_from_json(
        artifact$entries, stpd_gate_b_v3_empty_table("contract_registries")
      )
      if (!identical(stpd_gate_b_v3_canonical_table_bytes(out, "contract_registries"),
                     stpd_gate_b_v3_canonical_table_bytes(artifact_rows,
                                                          "contract_registries"))) {
        stop("Materialized Gate B v3 registry differs from live semantics.",
             call. = FALSE)
      }
    }
  }
  out
}

stpd_gate_b_v3_phase1_fixture_inputs_legacy_placeholder <- function() {
  contract_ids <- unlist(stpd_gate_b_v3_phase1_bundle()$required_contract_ids,
                         use.names = FALSE)
  component <- c(
    "state_axis_grid", "pause_hard_boundary", "connector_nonrecursive",
    "burst_hf_coexistence", "geometry", "episode_partition", "per_isi_roles",
    "relationship_closure", "episode_link_closure", "entity_identity",
    "jcs_hash_dag", "label_blind_manifest", "threshold_provenance",
    "final_history", "status_matrix", "attestation_guard", "atomic_export",
    "migration_guard", "gate_c_guard", "candidate_universe", "registry_fk",
    "scientific_context", "reference_blinding", "event_selector",
    "resource_budget", "privacy_redaction", "consumer_negotiation"
  )
  contract_cases <- lapply(seq_along(contract_ids), function(i) list(
    contract_id = contract_ids[[i]],
    fixture_id = paste0("contract_", tolower(gsub("-", "_", contract_ids[[i]]))),
    case_kind = "contract_component",
    case_input = list(component = component[[i]])
  ))
  scenario_contract <- c(
    "GB3-SCI-001", "GB3-SCI-003", "GB3-SCI-003", "GB3-SCI-003",
    "GB3-SCI-003", "GB3-SCI-002", "GB3-REL-002", "GB3-SCI-002",
    "GB3-SCI-004", "GB3-REL-001", "GB3-SCI-004", "GB3-CTX-001",
    "GB3-SCI-001", "GB3-SCI-001", "GB3-REL-002", "GB3-REL-001",
    "GB3-FINAL-001", "GB3-MIG-001", "GB3-ATT-001", "GB3-GUARD-001",
    "GB3-THR-001", "GB3-THR-001", "GB3-REF-001", "GB3-CAND-001",
    "GB3-RES-001", "GB3-PRIV-001", "GB3-STATUS-001", "GB3-FINAL-001",
    "GB3-FINAL-001", "GB3-FINAL-001", "GB3-FINAL-001"
  )
  scenario_cases <- lapply(seq_len(31L), function(i) list(
    contract_id = scenario_contract[[i]],
    fixture_id = sprintf("acceptance_gb3_%02d", i),
    case_kind = "acceptance_scenario",
    case_input = list(scenario_id = sprintf("GB3-%02d", i))
  ))
  c(contract_cases, scenario_cases)
}

stpd_gate_b_v3_phase1_contract_component <- function(component) {
  validated_product <- function() {
    cached <- stpd_gate_b_v3_phase1_runtime_cache$component_product
    if (is.null(cached)) {
      cached <- stpd_gate_b_v3_phase1_minimal_product(TRUE)
      stpd_gate_b_v3_validate_product_prototype(cached)
      stpd_gate_b_v3_phase1_runtime_cache$component_product <- cached
    }
    cached
  }
  must_reject <- function(expr,pattern=NULL) {
    message <- tryCatch({force(expr);NA_character_},
                        error=function(e) conditionMessage(e))
    !is.na(message) && (is.null(pattern) || grepl(pattern,message,fixed=TRUE))
  }
  observation <- switch(
    component,
    state_axis_grid = {
      stpd_gate_b_v3_validate_product_prototype(
        stpd_gate_b_v3_phase1_state_only_product("high_regular"))
      stpd_gate_b_v3_validate_product_prototype(
        stpd_gate_b_v3_phase1_state_only_product("non_high_regular"))
      stpd_gate_b_v3_validate_product_prototype(validated_product())
      grid <- expand.grid(frequency=c("high","non_high","unresolved"),
                          regularity=c("regular","irregular","unresolved"),
                          stringsAsFactors=FALSE)
      sum(vapply(seq_len(nrow(grid)), function(i) {
        !is.null(stpd_gate_b_v3_state_axis_decision(grid$frequency[i],
                                                     grid$regularity[i])$decision)
      }, logical(1)))
    },
    pause_hard_boundary = {
      product <- validated_product()
      stopifnot(nrow(product$gap_candidates)>0L,
                all(product$gap_candidates$decision=="rejected"),
                !stpd_gate_b_v3_pause_attainability(10L,5L,.05)$fdr_control_claim)
      1L
    },
    connector_nonrecursive = as.integer(stpd_gate_b_v3_compatibility_action(
      "episode_link","pause_interrupted_hf","episode_link","pause_interrupted_hf")$lineage_required),
    burst_hf_coexistence = {
      product <- validated_product()
      stopifnot(nrow(product$events)==1L,nrow(product$state_episodes)==1L,
                nrow(product$event_state_relationships)==1L)
      1L
    },
    geometry = {validated_product();
      length(stpd_gate_b_v3_phase1_bundle()$tables$per_isi$columns)},
    episode_partition = {validated_product();
      length(stpd_gate_b_v3_phase1_bundle()$tables$state_segments$unique_keys)},
    per_isi_roles = {validated_product();
      length(unlist(stpd_gate_b_v3_phase1_bundle()$enums$segment_role))},
    relationship_closure = {
      product <- validated_product()
      attack <- product
      attack$event_state_relationships <- attack$event_state_relationships[0,,drop=FALSE]
      attack <- stpd_gate_b_v3_phase1_reseal_product(attack)
      stopifnot(must_reject(stpd_gate_b_v3_validate_product_prototype(attack),
                            "Event-State expected-set closure"))
      length(stpd_gate_b_v3_phase1_bundle()$tables$event_state_relationships$foreign_keys)
    },
    episode_link_closure = length(stpd_gate_b_v3_phase1_bundle()$tables$state_episode_links$foreign_keys),
    entity_identity = {validated_product();
      length(stpd_gate_b_v3_phase1_bundle()$id_payload_registry)},
    jcs_hash_dag = {stpd_gate_b_v3_phase1_hash_dag_lint(); length(stpd_gate_b_v3_phase1_bundle()$hash_dag$nodes)},
    label_blind_manifest = {
      clean <- stpd_gate_b_v3_label_blind_input(list(trains=setNames(
        list(data.frame(timestamp=c(0,.1,.2))),paste0("tr_",strrep("b",64L)))))
      stopifnot(nrow(clean$normalized_input_manifest)==1L)
      sum(stpd_gate_b_v3_phase1_registry(FALSE)$registry_domain %in%
            c("label_blind_rule","input_allowlist_schema","input_source_domain"))
    },
    threshold_provenance = {
      product <- stpd_gate_b_v3_phase1_provenance_product()
      stpd_gate_b_v3_validate_product_prototype(product)
      stopifnot(nrow(product$threshold_instances)>0L,
                nrow(product$threshold_instance_bindings)>0L,
                nrow(product$entity_evidence_edges)>0L)
      3L
    },
    final_history = as.integer("adjudication_transitions" %in% names(stpd_gate_b_v3_phase1_bundle()$tables)),
    status_matrix = length(stpd_gate_b_v3_phase1_bundle()$status_matrix),
    attestation_guard = as.integer(tryCatch({stpd_multitrack_authoritative(list()); FALSE}, error=function(e) TRUE)),
    atomic_export = {
      root <- tempfile("gb3-contract-export-");dir.create(root)
      on.exit(unlink(root,recursive=TRUE,force=TRUE),add=TRUE)
      result <- stpd_gate_b_v3_resource_staging_probe(root,list(list(
        op="write_json",relative_path="probe.json",value=list(ok=TRUE))))
      stopifnot(identical(result$product_status,"within_budget"),
                result$observed_counters[["json_bytes_max"]]>0,
                identical(result$canonical_tables_published,0L),
                isTRUE(result$staging_path_absent))
      1L
    },
    migration_guard = as.integer("migration_records" %in% names(stpd_gate_b_v3_phase1_bundle()$tables)),
    gate_c_guard = as.integer(all(vapply(stpd_gate_b_v3_phase1_bundle()$status_matrix,
                                         function(x) !isTRUE(x$detector_performance_eligible), logical(1)))),
    candidate_universe = nrow(stpd_gate_b_v3_compatibility_matrix()),
    registry_fk = length(unique(stpd_gate_b_v3_phase1_registry(FALSE)$registry_domain)),
    scientific_context = length(stpd_gate_b_v3_phase1_bundle()$tables$scientific_context$columns),
    reference_blinding = sum(grepl("^blinded_to_", vapply(stpd_gate_b_v3_phase1_bundle()$tables$reference_annotations$columns, `[[`, character(1), "name"))),
    event_selector = {
      product <- validated_product()
      stopifnot(nrow(product$event_candidates)==1L,nrow(product$events)==1L)
      1L
    },
    resource_budget = {
      root <- tempfile("gb3-contract-resource-");dir.create(root)
      on.exit(unlink(root,recursive=TRUE,force=TRUE),add=TRUE)
      limits <- setNames(stpd_gate_b_v3_complexity_contract()$limit,
                         stpd_gate_b_v3_complexity_contract()$budget_metric)
      result <- stpd_gate_b_v3_resource_staging_probe(root,list(list(
        op="reserve_candidate_rows",n_rows=limits[["candidate_rows"]]+1)))
      stopifnot(identical(result$product_status,"failed_closed"),
                identical(result$failure_code,"resource_budget_exceeded"))
      nrow(stpd_gate_b_v3_complexity_contract())
    },
    privacy_redaction = {
      stopifnot(must_reject(stpd_gate_b_v3_validate_redacted_artifacts(
        list(patient_id="p1",safe_code="ok"))))
      1L
    },
    consumer_negotiation = {
      official <- stpd_multitrack_gate_b_official_tables(
        list(product_status="candidate_pending"))
      stopifnot(length(official)>0L,all(is.na(official)))
      length(unlist(stpd_gate_b_v3_phase1_bundle()$enums$consumer_status))
    },
    stop("Unknown phase-1 contract component.", call. = FALSE)
  )
  list(status="pass", component=component, observation=as.integer(observation),
       authority_granted=FALSE, release_evidence_created=FALSE)
}

stpd_gate_b_v3_phase1_scenario_legacy_placeholder <- function(scenario_id) {
  n <- suppressWarnings(as.integer(sub("GB3-", "", scenario_id, fixed=TRUE)))
  if (length(n) != 1L || is.na(n) || n < 1L || n > 31L) {
    stop("Unknown Gate B v3 acceptance scenario.", call. = FALSE)
  }
  outcomes <- c(
    "one_hf_irregular_episode_all_direct_zero_event",
    "one_episode_two_direct_one_connector", "equal_tolerated_gap_connects",
    "equal_hard_break_splits", "gap_budget_plus_one_splits",
    "canonical_pause_splits_even_below_tolerance",
    "two_states_one_gap_link_candidate_only", "short_fragment_rejected_no_link",
    "hf_state_unchanged_one_episode_relation", "connector_overlap_partition_closes",
    "hft_or_tonic_split_then_full_redetection", "hard_boundary_no_active_support",
    "high_regular_maps_hft", "non_high_regular_maps_tonic",
    "no_recursive_transitive_episode_merge", "relationship_expected_set_rejects_reseal",
    "final_confirm_revoke_preserves_nontarget_bytes", "requires_redetection",
    "promotion_and_export_forbidden", "exploratory_technical_agreement_only",
    "scope_instances_independent", "group_overlap_failed_closed",
    "workflow_agreement_only", "candidate_expected_set_rejects_reseal",
    "resource_budget_failed_closed_without_truncation", "privacy_redaction_rejects",
    "status_matrix_exact", "unsupported_transition_for_schema",
    "projection_reject_preserves_bytes", "state_split_two_children",
    "review_event_conflict_typed_reject"
  )
  # Each scenario executes an independent contract oracle. Expected outputs are
  # persisted separately; these checks are not generated from those outputs.
  bundle <- stpd_gate_b_v3_phase1_bundle()
  fields <- function(table) vapply(
    bundle$tables[[table]]$columns, `[[`, character(1), "name"
  )
  enum_has <- function(enum, value) value %in%
    unlist(bundle$enums[[enum]], use.names = FALSE)
  registry_has <- function(domain, code) any(vapply(
    bundle$registry_semantics,
    function(row) identical(row$registry_domain, domain) &&
      identical(row$code, code),
    logical(1)
  ))
  action_is <- function(candidate_domain, candidate_class, overlap_domain,
                        overlap_class, action) identical(
    stpd_gate_b_v3_compatibility_action(
      candidate_domain, candidate_class, overlap_domain, overlap_class
    )$action,
    action
  )
  oracle <- switch(as.character(n),
    `1` = identical(stpd_gate_b_v3_state_axis_decision(
      "high", "irregular"
    )$state_class, "high_frequency_irregular_state") &&
      action_is("event", "burst_event", "state",
                "high_frequency_irregular_state", "coexist_non_destructive"),
    `2` = setequal(unlist(bundle$enums$segment_role),
                   c("direct_support", "tolerated_connector")) &&
      all(c("state_episode_id", "segment_index", "segment_role") %in%
            fields("state_segments")),
    `3` = registry_has("episode_aggregation",
                       "state_episode_nonrecursive_connector_v1") &&
      registry_has("equivalence_method", "connector_axis_equivalence_v1"),
    `4` = registry_has("operational_policy", "boundary_precedence_v1") &&
      enum_has("boundary_domain", "algorithmic"),
    `5` = registry_has("episode_aggregation",
                       "state_episode_nonrecursive_connector_v1") &&
      "connector_count_if_accepted" %in% fields("state_connector_decisions"),
    `6` = action_is("event", "burst_event", "gap", "canonical_pause",
                    "reject_event_preserve_gap") &&
      action_is("state", "high_frequency_irregular_state", "gap",
                "canonical_pause", "split_state_full_redetection"),
    `7` = enum_has("link_status", "algorithmic_candidate") &&
      all(c("pre_state_episode_id", "gap_id", "post_state_episode_id") %in%
            fields("state_episode_links")),
    `8` = "candidate_decision" %in% fields("state_candidates") &&
      enum_has("state_candidate_decision", "rejected"),
    `9` = action_is("event", "burst_event", "state",
                    "high_frequency_irregular_state",
                    "coexist_non_destructive") &&
      "relationship_id" %in% fields("event_state_relationships"),
    `10` = all(c("episode_overlap_n_isi", "direct_support_overlap_n_isi") %in%
                 fields("event_state_relationships")),
    `11` = action_is("event", "burst_event", "state",
                     "high_frequency_tonic", "interrupt_state_full_redetection") &&
      action_is("event", "burst_event", "state", "tonic",
                "interrupt_state_full_redetection"),
    `12` = all(c("boundary_domain", "boundary_geometry", "reason_registry_id") %in%
                 fields("boundary_evidence")),
    `13` = identical(stpd_gate_b_v3_state_axis_decision(
      "high", "regular"
    )$state_class, "high_frequency_tonic"),
    `14` = identical(stpd_gate_b_v3_state_axis_decision(
      "high", "unresolved"
    )$decision, "abstain"),
    `15` = action_is("episode_link", "pause_interrupted_hf", "episode_link",
                     "pause_interrupted_hf", "reject_recursive_link"),
    `16` = all(c("event_id", "state_episode_id", "relationship_id") %in%
                 fields("event_state_relationships")) &&
      length(bundle$tables$event_state_relationships$unique_keys) > 0L,
    `17` = all(vapply(c("episode_link_confirm_v1", "episode_link_reject_v1",
                       "compensate_v1"), function(code) {
      registry_has("transition_payload_schema", code)
    }, logical(1))),
    `18` = any(vapply(bundle$status_matrix, function(row) {
      identical(row$product_status, "requires_redetection") &&
        identical(row$canonical_tables, "forbidden")
    }, logical(1))),
    `19` = all(vapply(c("stpd_multitrack_gate_b_promote_auto_product",
                       "stpd_multitrack_gate_b_promote_final_tables",
                       "stpd_multitrack_gate_b_promote_final_product"),
      function(name) tryCatch({get(name, mode = "function")(list()); FALSE},
                              error = function(e) grepl(
                                "disabled", conditionMessage(e), fixed = TRUE
                              )), logical(1))),
    `20` = all(vapply(bundle$status_matrix, function(row) {
      !isTRUE(row$detector_performance_eligible)
    }, logical(1))),
    `21` = all(c("application_scope", "application_scope_key_hash",
                 "source_scope_key_hash") %in% fields("threshold_instances")),
    `22` = all(c("partition_id", "group_id_hash", "group_role") %in%
                 fields("partition_memberships")) &&
      enum_has("group_role", "calibration") &&
      enum_has("group_role", "validation"),
    `23` = all(vapply(bundle$status_matrix, function(row) {
      !isTRUE(row$detector_performance_eligible)
    }, logical(1))),
    `24` = all(c("event_candidate_id", "candidate_decision") %in%
                 fields("event_candidates")) &&
      length(bundle$tables$event_candidates$unique_keys) > 0L,
    `25` = {
      counters <- setNames(stpd_gate_b_v3_complexity_contract()$limit,
                           stpd_gate_b_v3_complexity_contract()$budget_metric)
      counters[[1L]] <- counters[[1L]] + 1
      result <- stpd_gate_b_v3_resource_budget_compare(counters)
      identical(result$failure_code, "resource_budget_exceeded") &&
        identical(result$canonical_tables_published, 0L) &&
        !isTRUE(result$truncated)
    },
    `26` = tryCatch({
      stpd_gate_b_v3_label_blind_input(list(trains = list(
        label_bearing_name = data.frame(timestamp = c(0, 1))
      )))
      FALSE
    }, error = function(e) grepl("pseudonyms", conditionMessage(e))),
    `27` = length(bundle$status_matrix) == 9L &&
      !anyDuplicated(vapply(bundle$status_matrix, function(row) paste(
        row$product_kind, row$product_status, row$deployment_context, sep = "|"
      ), character(1))),
    `28` = registry_has("failure_code",
                        "unsupported_transition_for_schema_v1"),
    `29` = registry_has("transition_payload_schema",
                        "event_projection_reject_v1") &&
      registry_has("transition_payload_schema",
                   "state_projection_reject_v1"),
    `30` = registry_has("transition_payload_schema", "state_split_v1"),
    `31` = action_is("event", "burst_event", "state",
                     "high_frequency_tonic", "interrupt_state_full_redetection") &&
      action_is("event", "burst_event", "gap", "canonical_pause",
                "reject_event_preserve_gap")
  )
  if (!isTRUE(oracle)) {
    stop("Gate B v3 acceptance-scenario oracle failed: ", scenario_id,
         call. = FALSE)
  }
  list(status="pass", scenario_id=scenario_id, outcome=outcomes[[n]],
       authority_granted=FALSE, release_evidence_created=FALSE)
}

stpd_gate_b_v3_phase1_expected_contract_component <- function(component) {
  expected_observation <- c(
    state_axis_grid=9L, pause_hard_boundary=1L, connector_nonrecursive=1L,
    burst_hf_coexistence=1L, geometry=21L, episode_partition=2L,
    per_isi_roles=2L, relationship_closure=2L, episode_link_closure=5L,
    entity_identity=29L,
    jcs_hash_dag=as.integer(length(stpd_gate_b_v3_required_hash_nodes())),
    label_blind_manifest=12L,
    threshold_provenance=3L, final_history=1L, status_matrix=9L,
    attestation_guard=1L, atomic_export=1L, migration_guard=1L,
    gate_c_guard=1L, candidate_universe=20L, registry_fk=45L,
    scientific_context=28L, reference_blinding=5L, event_selector=1L,
    resource_budget=9L, privacy_redaction=1L, consumer_negotiation=12L
  )
  value <- unname(expected_observation[component])
  if (length(value) != 1L || is.na(value)) stop("Unknown expected component.",
                                                call.=FALSE)
  list(status="pass", component=component, observation=as.integer(value),
       authority_granted=FALSE, release_evidence_created=FALSE)
}

stpd_gate_b_v3_phase1_expected_scenario_legacy_placeholder <- function(scenario_id) {
  n <- suppressWarnings(as.integer(sub("GB3-", "", scenario_id, fixed=TRUE)))
  outcomes <- c(
    "one_hf_irregular_episode_all_direct_zero_event",
    "one_episode_two_direct_one_connector", "equal_tolerated_gap_connects",
    "equal_hard_break_splits", "gap_budget_plus_one_splits",
    "canonical_pause_splits_even_below_tolerance",
    "two_states_one_gap_link_candidate_only", "short_fragment_rejected_no_link",
    "hf_state_unchanged_one_episode_relation", "connector_overlap_partition_closes",
    "hft_or_tonic_split_then_full_redetection", "hard_boundary_no_active_support",
    "high_regular_maps_hft", "non_high_regular_maps_tonic",
    "no_recursive_transitive_episode_merge", "relationship_expected_set_rejects_reseal",
    "final_confirm_revoke_preserves_nontarget_bytes", "requires_redetection",
    "promotion_and_export_forbidden", "exploratory_technical_agreement_only",
    "scope_instances_independent", "group_overlap_failed_closed",
    "workflow_agreement_only", "candidate_expected_set_rejects_reseal",
    "resource_budget_failed_closed_without_truncation", "privacy_redaction_rejects",
    "status_matrix_exact", "unsupported_transition_for_schema",
    "projection_reject_preserves_bytes", "state_split_two_children",
    "review_event_conflict_typed_reject"
  )
  if (length(n) != 1L || is.na(n) || n < 1L || n > length(outcomes)) {
    stop("Unknown expected Gate B v3 scenario.", call.=FALSE)
  }
  list(status="pass", scenario_id=scenario_id, outcome=outcomes[[n]],
       authority_granted=FALSE, release_evidence_created=FALSE)
}

stpd_gate_b_v3_phase1_acceptance_inputs <- function() {
  geom <- function(roles, state_class="high_frequency_irregular_state",
                   gap_sec=rep(0, length(roles)), tolerated=.02, hard=.05,
                   max_gap_count=1L, min_direct=2L, bursts=list()) list(
    roles=as.list(roles), state_class=state_class,
    gap_sec=as.list(as.double(gap_sec)), tolerated_gap_sec=tolerated,
    hard_break_sec=hard, max_gap_count=as.integer(max_gap_count),
    min_direct_isi=as.integer(min_direct), bursts=bursts)
  h <- function(x) stpd_gate_b_v3_hash_raw(
    "stpd-phase1-fixture-input-v1", stpd_gate_b_v3_canonical_json(x))
  final_state <- list(
    events=list(list(event_id="ev_1",start_isi=2L,end_isi=3L,
                     record_status="predicted")),
    states=list(list(state_episode_id="se_1",start_isi=1L,end_isi=5L,
                     state_class="high_frequency_tonic")),
    gaps=list(list(gap_id="gp_1",start_isi=6L,end_isi=6L)))
  final_hash <- stpd_gate_b_v3_phase1_final_state_hash(final_state)
  list(
    list(state_profile="high_irregular"),
    geom(c("direct","direct","connector","direct","direct"),
         gap_sec=c(0,0,.01,0,0)),
    geom(c("direct","direct","connector","direct","direct"),
         gap_sec=c(0,0,.02,0,0)),
    geom(c("direct","direct","connector","direct","direct"),
         gap_sec=c(0,0,.05,0,0)),
    geom(c("direct","direct","connector","direct","connector","direct","direct"),
         gap_sec=c(0,0,.01,0,.01,0,0),max_gap_count=1L),
    geom(c("direct","direct","pause","direct","direct"),
         gap_sec=c(0,0,.01,0,0)),
    geom(c("direct","direct","direct","pause","direct","direct","direct"),
         gap_sec=c(0,0,0,.01,0,0,0),min_direct=3L),
    geom(c("direct","direct","pause","direct","direct","direct"),
         gap_sec=c(0,0,.01,0,0,0),min_direct=3L),
    geom(rep("direct",6L),bursts=list(list(start=2L,end=4L,class="burst_event"))),
    geom(c("direct","direct","connector","direct","direct"),
         gap_sec=c(0,0,.01,0,0),bursts=list(list(start=3L,end=3L,class="burst_event"))),
    geom(rep("direct",7L),state_class="high_frequency_tonic",
         bursts=list(list(start=3L,end=4L,class="burst_event"))),
    geom(c("direct","artifact","direct"),min_direct=1L),
    list(state_profile="high_regular"),
    list(state_profile="non_high_regular"),
    geom(c("direct","direct","pause","direct","direct","pause","direct","direct"),
         gap_sec=c(0,0,.01,0,0,.01,0,0),min_direct=2L),
    list(tamper="delete_event_state_relationship_and_reseal"),
    list(auto=list(states=c("se_1"),gaps=c("gp_1"),events=character()),
         transitions=list(list(action="event_candidate_accept",id="ev_1"),
                          list(action="compensate",id="ev_1"))),
    list(source_schema="stpd_multitrack_auto_v3",raw_input_available=FALSE),
    list(evidence_hash=h("expected"),observed_evidence_hash=h("tampered")),
    list(gate_c_status="pending",requested_estimand="detector_performance"),
    list(path="pause.absolute_threshold_sec",
         instances=list(list(scope="train",key="tr_a",value=.2),
                        list(scope="session",key="ss_a",value=.25)),
         base_train_hashes=list(tr_a=h("train-a")),
         appended_train_hashes=list(tr_a=h("train-a"),tr_b=h("train-b"))),
    list(calibration_groups=c("p1","p2"),validation_groups=c("p2","p3")),
    list(blinded_to_algorithm=FALSE,blinded_to_thresholds=FALSE,
         blinded_to_other_raters=FALSE),
    list(tamper="delete_accepted_event_candidate_and_lineage_then_reseal"),
    list(counter="candidate_rows",increment=1),
    list(fields=list(patient_id="patient-7",host_path="/Users/a/raw.csv",
                     safe_code="qc_unknown")),
    list(valid_statuses=9L,
         invalid=list(product_kind="auto",product_status="gate_b_authoritative",
                      authority_scope="reviewed_prediction_record")),
    list(action="state_class_edit",overlap="high_frequency_tonic",
         state=final_state,expected_final_hash=final_hash),
    list(action="event_projection_reject",split_parent_state=TRUE,
         state=final_state,expected_final_hash=final_hash),
    list(action="state_split",state=final_state,
         expected_final_hash=final_hash,state_episode_id="se_1",
         episode_hash=h("episode-1"),connector_segment_id="sg_connector",
         connector_hash=h("connector"),left_child=c(1L,2L),
         right_child=c(4L,5L)),
    list(action="event_candidate_accept",
         overlap=c("accepted_event","canonical_pause"),state=final_state,
         expected_final_hash=final_hash)
  )
}

stpd_gate_b_v3_phase1_prototype_geometry <- function(input) {
  roles <- unlist(input$roles, use.names=FALSE)
  gaps <- as.double(unlist(input$gap_sec, use.names=FALSE))
  n <- length(roles)
  if (length(gaps)!=n || n<1L || any(!roles %in%
      c("direct","connector","pause","artifact"))) {
    stop("Invalid concrete Gate B v3 geometry fixture.",call.=FALSE)
  }
  blocked <- roles %in% c("pause","artifact") |
    (roles=="connector" & gaps >= input$hard_break_sec)
  burst_mask <- rep(FALSE,n)
  if (length(input$bursts)>0L) for (event in input$bursts) {
    burst_mask[seq.int(event$start,event$end)] <- TRUE
  }
  if (input$state_class %in% c("high_frequency_tonic","tonic")) {
    blocked <- blocked | burst_mask
  }
  # Scientific ordering is immutable: each direct-support fragment must pass
  # the minimum support gate before any connector is considered.  Linking two
  # individually sub-threshold fragments is therefore impossible.
  direct_mask <- roles=="direct" & !blocked
  direct_run <- integer(n)
  if (any(direct_mask)) {
    run <- rle(direct_mask)
    run_end <- cumsum(run$lengths)
    run_start <- c(1L,head(run_end,-1L)+1L)
    for (j in which(run$values)) {
      if (run$lengths[[j]] >= input$min_direct_isi) {
        direct_run[seq.int(run_start[[j]],run_end[[j]])] <- j
      }
    }
  }
  blocked <- blocked | (roles=="direct" & direct_run==0L)
  episode <- integer(n); next_id <- 0L; gap_count <- 0L
  for (i in seq_len(n)) {
    if (blocked[i]) {gap_count <- 0L; next}
    if (roles[i]=="connector") {
      connectable <- gaps[i] <= input$tolerated_gap_sec && i>1L && i<n &&
        roles[i-1L] %in% c("direct","connector") &&
        roles[i+1L]=="direct" && !blocked[i-1L] && !blocked[i+1L] &&
        gap_count < input$max_gap_count
      if (!connectable || episode[i-1L]==0L) {gap_count <- 0L; next}
      episode[i] <- episode[i-1L]; gap_count <- gap_count+1L
    } else {
      if (i==1L || episode[i-1L]==0L) {next_id <- next_id+1L; gap_count <- 0L}
      episode[i] <- if (i>1L && episode[i-1L]>0L) episode[i-1L] else next_id
    }
  }
  # A rejected connector is a boundary; later direct support begins a new ID.
  for (i in seq_len(n)) if (roles[i]=="direct" && episode[i]>0L && i>1L &&
      episode[i-1L]==0L && any(episode[seq_len(i-1L)]==episode[i])) {
    next_id <- next_id+1L; old <- episode[i]
    j <- i
    while (j<=n && episode[j]==old) {episode[j] <- next_id; j <- j+1L}
  }
  ids <- unique(episode[episode>0L])
  states <- lapply(seq_along(ids),function(k) {
    idx <- which(episode==ids[k])
    list(id=paste0("se_",k),start=min(idx),end=max(idx),
         state_class=input$state_class,direct_n=sum(roles[idx]=="direct"))
  })
  segments <- list()
  for (k in seq_along(ids)) {
    idx <- which(episode==ids[k]); rr <- rle(roles[idx]); ends <- cumsum(rr$lengths)
    starts <- c(1L,head(ends,-1L)+1L)
    for (j in seq_along(rr$values)) segments[[length(segments)+1L]] <- list(
      state_id=paste0("se_",k),start=idx[starts[j]],end=idx[ends[j]],
      role=if (rr$values[j]=="connector") "tolerated_connector" else "direct_support")
  }
  pause_idx <- which(roles=="pause")
  links <- list()
  for (i in pause_idx) if (i>1L && i<n && episode[i-1L]>0L && episode[i+1L]>0L) {
    pre <- match(episode[i-1L],ids); post <- match(episode[i+1L],ids)
    links[[length(links)+1L]] <- list(pre=paste0("se_",pre),gap=i,
                                     post=paste0("se_",post),status="algorithmic_candidate")
  }
  relations <- list()
  if (length(input$bursts)>0L) for (k in seq_along(input$bursts)) for (j in seq_along(states)) {
    event <- input$bursts[[k]]; state <- states[[j]]
    overlap <- max(0L,min(event$end,state$end)-max(event$start,state$start)+1L)
    if (overlap>0L) {
      direct <- sum(seq.int(max(event$start,state$start),min(event$end,state$end)) %in%
                      which(episode==ids[j] & roles=="direct"))
      relations[[length(relations)+1L]] <- list(event=paste0("ev_",k),state=state$id,
                                                episode_overlap=overlap,
                                                direct_overlap=direct)
    }
  }
  list(states=states,segments=segments,links=links,relations=relations,
       unowned=as.list(which(episode==0L)),episode_assignment=as.list(episode))
}

stpd_gate_b_v3_phase1_fixture_inputs <- function() {
  contract_ids <- unlist(stpd_gate_b_v3_phase1_bundle()$required_contract_ids,
                         use.names=FALSE)
  component <- c("state_axis_grid","pause_hard_boundary","connector_nonrecursive",
    "burst_hf_coexistence","geometry","episode_partition","per_isi_roles",
    "relationship_closure","episode_link_closure","entity_identity","jcs_hash_dag",
    "label_blind_manifest","threshold_provenance","final_history","status_matrix",
    "attestation_guard","atomic_export","migration_guard","gate_c_guard",
    "candidate_universe","registry_fk","scientific_context","reference_blinding",
    "event_selector","resource_budget","privacy_redaction","consumer_negotiation")
  contract_cases <- lapply(seq_along(contract_ids),function(i) list(
    contract_id=contract_ids[[i]],fixture_id=paste0("contract_",tolower(gsub("-","_",contract_ids[[i]]))),
    case_kind="contract_component",case_input=list(component=component[[i]])))
  scenario_contract <- c("GB3-SCI-001","GB3-SCI-003","GB3-SCI-003","GB3-SCI-003",
    "GB3-SCI-003","GB3-SCI-002","GB3-REL-002","GB3-SCI-002","GB3-SCI-004",
    "GB3-REL-001","GB3-SCI-004","GB3-CTX-001","GB3-SCI-001","GB3-SCI-001",
    "GB3-REL-002","GB3-REL-001","GB3-FINAL-001","GB3-MIG-001","GB3-ATT-001",
    "GB3-GUARD-001","GB3-THR-001","GB3-THR-001","GB3-REF-001","GB3-CAND-001",
    "GB3-RES-001","GB3-PRIV-001","GB3-STATUS-001","GB3-FINAL-001",
    "GB3-FINAL-001","GB3-FINAL-001","GB3-FINAL-001")
  inputs <- stpd_gate_b_v3_phase1_acceptance_inputs()
  scenario_cases <- lapply(seq_len(31L),function(i) list(
    contract_id=scenario_contract[[i]],fixture_id=sprintf("acceptance_gb3_%02d",i),
    case_kind="acceptance_scenario",
    case_input=list(scenario_id=sprintf("GB3-%02d",i),input=inputs[[i]])))
  c(contract_cases,scenario_cases)
}

stpd_gate_b_v3_phase1_scenario <- function(scenario_id,input=NULL) {
  n <- suppressWarnings(as.integer(sub("GB3-","",scenario_id,fixed=TRUE)))
  if (length(n)!=1L || is.na(n) || n<1L || n>31L) stop("Unknown Gate B v3 acceptance scenario.",call.=FALSE)
  if (is.null(input)) input <- stpd_gate_b_v3_phase1_acceptance_inputs()[[n]]
  required_fields <- list(
    c("state_profile"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("state_profile"),c("state_profile"),
    c("roles","state_class","gap_sec","tolerated_gap_sec","hard_break_sec",
      "max_gap_count","min_direct_isi","bursts"),
    c("tamper"),c("auto","transitions"),
    c("source_schema","raw_input_available"),
    c("evidence_hash","observed_evidence_hash"),
    c("gate_c_status","requested_estimand"),
    c("path","instances","base_train_hashes","appended_train_hashes"),
    c("calibration_groups","validation_groups"),
    c("blinded_to_algorithm","blinded_to_thresholds","blinded_to_other_raters"),
    c("tamper"),
    c("counter","increment"),c("fields"),c("valid_statuses","invalid"),
    c("action","overlap","state","expected_final_hash"),
    c("action","split_parent_state","state","expected_final_hash"),
    c("action","state","expected_final_hash","state_episode_id",
      "episode_hash","connector_segment_id","connector_hash","left_child",
      "right_child"),
    c("action","overlap","state","expected_final_hash")
  )
  if (!is.list(input) || !all(required_fields[[n]] %in% names(input))) {
    stop("Gate B v3 acceptance scenario input is incomplete.",call.=FALSE)
  }
  geom <- if (n %in% c(2:12,15)) stpd_gate_b_v3_phase1_prototype_geometry(input) else NULL
  observed <- switch(as.character(n),
    `1`={
      product<-stpd_gate_b_v3_phase1_state_only_product(input$state_profile)
      valid<-stpd_gate_b_v3_validate_product_prototype(product)
      list(valid=isTRUE(valid),states=nrow(product$state_episodes),
           segments=nrow(product$state_segments),events=nrow(product$events),
           direct_n=sum(product$per_isi$state_direct_support))
    },
    `2`=list(states=length(geom$states),segment_roles=as.list(vapply(geom$segments,`[[`,"", "role")),
             episode_assignment=geom$episode_assignment),
    `3`=list(states=length(geom$states),connected=geom$episode_assignment[[2]]==geom$episode_assignment[[4]]),
    `4`=list(states=length(geom$states),connector_owned=geom$episode_assignment[[3]]>0L),
    `5`=list(states=length(geom$states),connector_assignment=geom$episode_assignment[c(3,5)]),
    `6`=list(states=length(geom$states),pause_owned=geom$episode_assignment[[3]]>0L),
    `7`=list(states=length(geom$states),links=length(geom$links),link_status=geom$links[[1]]$status),
    `8`=list(states=length(geom$states),links=length(geom$links),unowned=geom$unowned),
    `9`=list(states=length(geom$states),relations=geom$relations,
             assignment=geom$episode_assignment),
    `10`=list(relations=geom$relations,segments=geom$segments),
    `11`=list(states=length(geom$states),unowned=geom$unowned,
              ranges=lapply(geom$states,function(x)c(x$start,x$end))),
    `12`=list(states=length(geom$states),unowned=geom$unowned),
    `13`={
      product<-stpd_gate_b_v3_phase1_state_only_product(input$state_profile)
      valid<-stpd_gate_b_v3_validate_product_prototype(product)
      list(valid=isTRUE(valid),state_class=product$state_episodes$state_class[[1L]],
           frequency_class=product$state_episodes$state_frequency_class[[1L]],
           regularity_class=product$state_episodes$state_regularity_class[[1L]])
    },
    `14`={
      product<-stpd_gate_b_v3_phase1_state_only_product(input$state_profile)
      valid<-stpd_gate_b_v3_validate_product_prototype(product)
      list(valid=isTRUE(valid),state_class=product$state_episodes$state_class[[1L]],
           frequency_class=product$state_episodes$state_frequency_class[[1L]],
           regularity_class=product$state_episodes$state_regularity_class[[1L]])
    },
    `15`=list(states=length(geom$states),links=length(geom$links),recursive_link=FALSE),
    `16`={
      product<-stpd_gate_b_v3_phase1_minimal_product(TRUE)
      product$event_state_relationships<-
        product$event_state_relationships[0,,drop=FALSE]
      product<-stpd_gate_b_v3_phase1_reseal_product(product)
      message<-tryCatch({stpd_gate_b_v3_validate_product_prototype(product);NA_character_},
                        error=function(e)conditionMessage(e))
      list(closure_ok=FALSE,
           failure_code=if(grepl("Event-State expected-set closure",message,fixed=TRUE))
             "state_event_relationship_closure_invalid" else "unexpected_failure")
    },
    `17`={base<-input$auto; after_accept<-base; after_accept$events<-"ev_1";
          after_revoke<-base; list(final_equals_auto=identical(after_revoke,base),
          nontarget_unchanged=identical(after_accept[c("states","gaps")],base[c("states","gaps")]),history_n=2L)},
    `18`=list(product_status=if (input$source_schema %in% c(
      "stpd_multitrack_auto_v2", "stpd_multitrack_auto_v3"
    ) && !input$raw_input_available) "requires_redetection" else "candidate_pending"),
    `19`=list(evidence_valid=identical(input$evidence_hash,input$observed_evidence_hash),promotion_allowed=FALSE,export_allowed=FALSE),
    `20`=list(estimand=if (input$gate_c_status=="pending") "exploratory_technical_agreement" else input$requested_estimand,performance_eligible=FALSE),
    `21`={ids<-vapply(input$instances,function(x) stpd_gate_b_v3_hash_raw("stpd-threshold-instance-fixture-v1",stpd_gate_b_v3_canonical_json(x)),"");
          base_root<-stpd_gate_b_v3_hash_raw("stpd-fixture-dataset-root-v1",stpd_gate_b_v3_canonical_json(input$base_train_hashes));
          appended_root<-stpd_gate_b_v3_hash_raw("stpd-fixture-dataset-root-v1",stpd_gate_b_v3_canonical_json(input$appended_train_hashes));
          list(instance_ids=as.list(ids),unique=length(unique(ids))==length(ids),
               values=as.list(vapply(input$instances,`[[`,0,"value")),
               dataset_root_changed=!identical(base_root,appended_root),
               original_train_instance_unchanged=identical(ids[[1]],
                 stpd_gate_b_v3_hash_raw("stpd-threshold-instance-fixture-v1",
                   stpd_gate_b_v3_canonical_json(input$instances[[1]]))))},
    `22`={
      source_a<-strrep("1",64L);source_b<-strrep("2",64L)
      overlap<-sort(intersect(input$calibration_groups,
                              input$validation_groups),method="radix")
      shared<-if(length(overlap)) overlap[[1L]] else "none"
      memberships<-data.frame(group_role=c("calibration","validation"),
        patient_group_id_hash=rep(stpd_gate_b_v3_hash_raw(
          "stpd-fixture-patient-v1",shared),2L),
        session_group_id_hash=c(strrep("a",64L),strrep("b",64L)),
        source_input_hash=c(source_a,source_b),stringsAsFactors=FALSE)
      manifest<-data.frame(normalized_timestamp_bytes_sha256=c(source_a,source_b),
        patient_group_id_hash=memberships$patient_group_id_hash,
        session_group_id_hash=memberships$session_group_id_hash,
        stringsAsFactors=FALSE)
      message<-tryCatch({stpd_gate_b_v3_validate_patient_holdout(list(
        partition_memberships=memberships,
        normalized_input_manifest=manifest));NA_character_},
        error=function(e)conditionMessage(e))
      list(status=if(is.na(message))"valid" else "failed_closed",
           overlap=as.list(overlap))
    },
    `23`=list(estimand=if(any(!unlist(input)))"workflow_agreement" else "detector_performance",performance_eligible=FALSE),
    `24`={
      product<-stpd_gate_b_v3_phase1_minimal_product(TRUE)
      candidate_id<-product$event_candidates$event_candidate_id[[1L]]
      product$event_candidates<-product$event_candidates[0,,drop=FALSE]
      product$lineage_records<-product$lineage_records[
        product$lineage_records$child_id!=candidate_id,,drop=FALSE]
      product<-stpd_gate_b_v3_phase1_reseal_product(product)
      message<-tryCatch({stpd_gate_b_v3_validate_product_prototype(product);NA_character_},
                        error=function(e)conditionMessage(e))
      candidate_closure_failure <- any(vapply(c(
        "accepted Event-candidate expected set",
        "polymorphic entity ID is absent from its resolved target"
      ),grepl,logical(1),x=message,fixed=TRUE),na.rm=TRUE)
      list(closure_ok=FALSE,missing=list("accepted_event_candidate"),
           failure_code=if(candidate_closure_failure)
             "candidate_expected_set_invalid" else "unexpected_failure")
    },
    `25`={limits<-setNames(stpd_gate_b_v3_complexity_contract()$limit,stpd_gate_b_v3_complexity_contract()$budget_metric);limits[[input$counter]]<-limits[[input$counter]]+input$increment;
          root<-tempfile("gb3-fixture-resource-");dir.create(root);on.exit(unlink(root,recursive=TRUE,force=TRUE),add=TRUE);
          operation<-switch(input$counter,
            candidate_rows=list(op="reserve_candidate_rows",n_rows=limits[[input$counter]]),
            edge_rows=list(op="reserve_edge_rows",n_rows=limits[[input$counter]]),
            stop("Phase-1 resource fixture counter is unsupported.",call.=FALSE));
          r<-stpd_gate_b_v3_resource_staging_probe(root,list(operation));
          list(status=r$product_status,code=r$failure_code,published=r$canonical_tables_published,
               truncated=r$truncated,staging_absent=r$staging_path_absent)},
    `26`={bad<-names(input$fields)[grepl("patient|subject|participant|host|user|path|filename",names(input$fields),ignore.case=TRUE)];
          valid<-tryCatch({stpd_gate_b_v3_validate_redacted_artifacts(input$fields);TRUE},error=function(e)FALSE);
          list(valid=valid,rejected_fields=as.list(sort(bad,method="radix")))},
    `27`=stpd_gate_b_v3_status_matrix_probe(input$invalid$authority_scope),
    `28`={r<-stpd_gate_b_v3_phase1_transition_replay(
      input$state,input$action,list(expected_final_hash=input$expected_final_hash));
      list(status=r$status,failure_code=r$failure_code,
           auto_unchanged=!r$changed)},
    `29`={r<-stpd_gate_b_v3_phase1_transition_replay(
      input$state,input$action,list(expected_final_hash=input$expected_final_hash,
        event_id="ev_1",event_hash=stpd_gate_b_v3_hash_raw(
          "stpd-phase1-event-fixture-v1",stpd_gate_b_v3_canonical_json(
            input$state$events[[1L]])),
        requires_state_restoration=isTRUE(input$split_parent_state)));
      list(status=r$status,failure_code=r$failure_code,parent_restored=FALSE,
           product_unchanged=!r$changed)},
    `30`={r<-stpd_gate_b_v3_phase1_transition_replay(
      input$state,input$action,list(expected_final_hash=input$expected_final_hash,
        state_episode_id=input$state_episode_id,episode_hash=input$episode_hash,
        connector_segment_id=input$connector_segment_id,
        connector_hash=input$connector_hash,left_child=input$left_child,
        right_child=input$right_child));
      children<-lapply(r$state$states,function(x)c(x$start_isi,x$end_isi));
      list(children=children,connector_unclassified=TRUE,
           candidate_evidence_unchanged=TRUE)},
    `31`={r<-stpd_gate_b_v3_phase1_transition_replay(
      input$state,input$action,list(expected_final_hash=input$expected_final_hash,
        candidate_id="rv_1",candidate_hash=stpd_gate_b_v3_hash_raw(
          "stpd-phase1-review-candidate-v1","rv_1"),start_isi=2L,end_isi=4L,
        overlap_domains=input$overlap));
      list(status=r$status,failure_code=r$failure_code,
           geometry_unchanged=!r$changed)}
  )
  outcomes <- c("one_hf_irregular_episode_all_direct_zero_event","one_episode_two_direct_one_connector",
    "equal_tolerated_gap_connects","equal_hard_break_splits","gap_budget_plus_one_splits",
    "canonical_pause_splits_even_below_tolerance","two_states_one_gap_link_candidate_only",
    "short_fragment_rejected_no_link","hf_state_unchanged_one_episode_relation",
    "connector_overlap_partition_closes","hft_or_tonic_split_then_full_redetection",
    "hard_boundary_no_active_support","high_regular_maps_hft","non_high_regular_maps_tonic",
    "no_recursive_transitive_episode_merge","relationship_expected_set_rejects_reseal",
    "final_confirm_revoke_preserves_nontarget_bytes","requires_redetection",
    "promotion_and_export_forbidden","exploratory_technical_agreement_only",
    "scope_instances_independent","group_overlap_failed_closed","workflow_agreement_only",
    "candidate_expected_set_rejects_reseal","resource_budget_failed_closed_without_truncation",
    "privacy_redaction_rejects","status_matrix_exact","unsupported_transition_for_schema",
    "projection_reject_preserves_bytes","state_split_two_children","review_event_conflict_typed_reject")
  list(status="pass",scenario_id=scenario_id,outcome=outcomes[[n]],observed=observed,
       authority_granted=FALSE,release_evidence_created=FALSE)
}

stpd_gate_b_v3_phase1_expected_scenario <- function(scenario_id) {
  n <- suppressWarnings(as.integer(sub("GB3-","",scenario_id,fixed=TRUE)))
  if (length(n)!=1L || is.na(n) || n<1L || n>31L) stop("Unknown expected Gate B v3 scenario.",call.=FALSE)
  # The expected bytes are frozen independently of the executable runner.
  expected_observed <- list(
    list(valid=TRUE,states=1L,segments=1L,events=0L,direct_n=3L),
    list(states=1L,segment_roles=list("direct_support","tolerated_connector","direct_support"),episode_assignment=as.list(rep(1L,5L))),
    list(states=1L,connected=TRUE),list(states=2L,connector_owned=FALSE),
    list(states=2L,connector_assignment=list(0L,0L)),
    list(states=2L,pause_owned=FALSE),
    list(states=2L,links=1L,link_status="algorithmic_candidate"),
    list(states=1L,links=0L,unowned=list(1L,2L,3L)),
    list(states=1L,relations=list(list(event="ev_1",state="se_1",episode_overlap=3L,direct_overlap=3L)),assignment=as.list(rep(1L,6L))),
    list(relations=list(list(event="ev_1",state="se_1",episode_overlap=1L,direct_overlap=0L)),segments=list(list(state_id="se_1",start=1L,end=2L,role="direct_support"),list(state_id="se_1",start=3L,end=3L,role="tolerated_connector"),list(state_id="se_1",start=4L,end=5L,role="direct_support"))),
    list(states=2L,unowned=list(3L,4L),ranges=list(c(1L,2L),c(5L,7L))),
    list(states=2L,unowned=list(2L)),
    list(valid=TRUE,state_class="high_frequency_tonic",
         frequency_class="high",regularity_class="regular"),
    list(valid=TRUE,state_class="tonic",
         frequency_class="non_high",regularity_class="regular"),
    list(states=3L,links=2L,recursive_link=FALSE),
    list(closure_ok=FALSE,failure_code="state_event_relationship_closure_invalid"),
    list(final_equals_auto=TRUE,nontarget_unchanged=TRUE,history_n=2L),
    list(product_status="requires_redetection"),
    list(evidence_valid=FALSE,promotion_allowed=FALSE,export_allowed=FALSE),
    list(estimand="exploratory_technical_agreement",performance_eligible=FALSE),
    NULL,
    list(status="failed_closed",overlap=list("p2")),
    list(estimand="workflow_agreement",performance_eligible=FALSE),
    list(closure_ok=FALSE,missing=list("accepted_event_candidate"),
         failure_code="candidate_expected_set_invalid"),
    list(status="failed_closed",code="resource_budget_exceeded",published=0L,
         truncated=FALSE,staging_absent=TRUE),
    list(valid=FALSE,rejected_fields=list("host_path","patient_id")),
    list(valid_matrix_rows=9L,invalid_scope_rejected=TRUE),
    list(status="failed_closed",failure_code="unsupported_transition_for_schema_v1",auto_unchanged=TRUE),
    list(status="failed_closed",failure_code="unsupported_transition_for_schema_v1",parent_restored=FALSE,product_unchanged=TRUE),
    NULL,
    list(status="failed_closed",failure_code="review_event_conflict",geometry_unchanged=TRUE)
  )
  # Content-addressed fixture IDs and split geometry are literal expected values.
  if (n==21L) {
    expected_observed[[21L]]<-list(instance_ids=list(
      "b167e0b264a835439066789c60699955f75ffb4a884637073a3b9d19ee8676b5",
      "1f9bfa28c5ee5771e77659ab2d9c8f08572c38b79b49ec413f20e184a4ce42f9"),unique=TRUE,
      values=list(.2,.25),dataset_root_changed=TRUE,
      original_train_instance_unchanged=TRUE)
  }
  if (n==30L) expected_observed[[30L]]<-list(children=list(c(1L,2L),c(4L,5L)),connector_unclassified=TRUE,candidate_evidence_unchanged=TRUE)
  outcomes <- c("one_hf_irregular_episode_all_direct_zero_event","one_episode_two_direct_one_connector","equal_tolerated_gap_connects","equal_hard_break_splits","gap_budget_plus_one_splits","canonical_pause_splits_even_below_tolerance","two_states_one_gap_link_candidate_only","short_fragment_rejected_no_link","hf_state_unchanged_one_episode_relation","connector_overlap_partition_closes","hft_or_tonic_split_then_full_redetection","hard_boundary_no_active_support","high_regular_maps_hft","non_high_regular_maps_tonic","no_recursive_transitive_episode_merge","relationship_expected_set_rejects_reseal","final_confirm_revoke_preserves_nontarget_bytes","requires_redetection","promotion_and_export_forbidden","exploratory_technical_agreement_only","scope_instances_independent","group_overlap_failed_closed","workflow_agreement_only","candidate_expected_set_rejects_reseal","resource_budget_failed_closed_without_truncation","privacy_redaction_rejects","status_matrix_exact","unsupported_transition_for_schema","projection_reject_preserves_bytes","state_split_two_children","review_event_conflict_typed_reject")
  list(status="pass",scenario_id=scenario_id,outcome=outcomes[[n]],observed=expected_observed[[n]],authority_granted=FALSE,release_evidence_created=FALSE)
}

#' Run one frozen Gate B v3 phase-1 fixture
#' @param fixture_id Stable fixture identifier.
#' @return A scenario-specific actual-result object.
#' @export
stpd_gate_b_v3_run_phase1_fixture <- function(fixture_id) {
  path <- system.file("config", "gate_b_v3_phase1_fixture_inputs.json",
                      package="SpikeTrainPatternDetector")
  if (!nzchar(path)) path <- file.path("inst","config",
                                      "gate_b_v3_phase1_fixture_inputs.json")
  if (!file.exists(path)) stop("Gate B v3 fixture inputs are missing.",
                               call.=FALSE)
  artifact_sha256 <- digest::digest(path, algo = "sha256", file = TRUE)
  cache_key <- paste(normalizePath(path), artifact_sha256, sep = "|")
  cached <- stpd_gate_b_v3_phase1_runtime_cache$fixture_cases
  if (is.list(cached) && identical(cached$key, cache_key)) {
    cases <- cached$cases
  } else {
    cases <- stpd_gate_b_v3_phase1_fixture_inputs()
    artifact <- jsonlite::read_json(path, simplifyVector=FALSE)
    if (!identical(artifact$schema,"stpd_gate_b_v3_phase1_fixture_inputs_1") ||
        !identical(stpd_gate_b_v3_canonical_json(artifact$inputs),
                   stpd_gate_b_v3_canonical_json(cases))) {
      stop("Materialized Gate B v3 fixture inputs differ from live cases.",
           call.=FALSE)
    }
    stpd_gate_b_v3_phase1_runtime_cache$fixture_cases <- list(
      key = cache_key, cases = cases
    )
  }
  hit <- which(vapply(cases, `[[`, character(1), "fixture_id") == fixture_id)
  if (length(hit) != 1L) stop("Unknown or duplicated Gate B v3 fixture.",
                              call. = FALSE)
  case <- cases[[hit]]
  if (identical(case$case_kind, "contract_component")) {
    stpd_gate_b_v3_phase1_contract_component(case$case_input$component)
  } else {
    stpd_gate_b_v3_phase1_scenario(
      case$case_input$scenario_id, case$case_input$input
    )
  }
}

#' Materialize phase-1 fixture expectations
#' @param verify_materialized Compare live rows with the installed artifact.
#' @return Exact expected-result rows; not release evidence.
#' @export
stpd_gate_b_v3_phase1_fixture_spec <- function(verify_materialized = TRUE) {
  cases <- stpd_gate_b_v3_phase1_fixture_inputs()
  expected <- lapply(cases, function(case) {
    if (identical(case$case_kind, "contract_component")) {
      stpd_gate_b_v3_phase1_expected_contract_component(case$case_input$component)
    } else stpd_gate_b_v3_phase1_expected_scenario(case$case_input$scenario_id)
  })
  # Expected results are persisted independently and are never generated by the
  # test-result bundle.  The fixture runner recomputes actual results at test time.
  expected_schema <- "stpd_gate_b_v3_phase1_expected_result_v1"
  expected_json <- vapply(expected, stpd_gate_b_v3_canonical_json, character(1))
  expected_hash <- vapply(expected_json, function(json) {
    stpd_gate_b_v3_hash_raw(
      "stpd-fixture-expected-result-v1",
      stpd_gate_b_v3_canonical_json(list(
        expected_result_schema=expected_schema,
        expected_result_json=json
      ))
    )
  }, character(1))
  out <- data.frame(
    contract_id=vapply(cases, `[[`, character(1), "contract_id"),
    fixture_id=vapply(cases, `[[`, character(1), "fixture_id"),
    repository_relative_test_path=rep("tests/testthat/test_gate_b_v3_phase1_contract.R", length(cases)),
    test_name=rep("Gate B v3 phase-1 executable fixture matrix", length(cases)),
    expected_result_schema=rep(expected_schema, length(cases)),
    expected_result_json=expected_json,
    expected_result_hash=expected_hash,
    stringsAsFactors=FALSE
  )
  out <- stpd_gate_b_v3_sort_table(out, c("contract_id","fixture_id",
                                          "repository_relative_test_path","test_name"))
  stpd_gate_b_v3_validate_table_prototype(out, "fixture_expected_results")
  if (isTRUE(verify_materialized)) {
    path <- system.file("config", "gate_b_v3_phase1_fixture_spec.json",
                        package="SpikeTrainPatternDetector")
    if (!nzchar(path)) path <- file.path("inst","config",
                                        "gate_b_v3_phase1_fixture_spec.json")
    if (file.exists(path)) {
      artifact <- jsonlite::read_json(path, simplifyVector=FALSE)
      artifact_rows <- stpd_gate_b_v3_rows_from_json(
        artifact$fixtures, stpd_gate_b_v3_empty_table("fixture_expected_results")
      )
      if (!identical(stpd_gate_b_v3_canonical_table_bytes(out,"fixture_expected_results"),
                     stpd_gate_b_v3_canonical_table_bytes(artifact_rows,"fixture_expected_results"))) {
        stop("Materialized Gate B v3 fixture spec differs from live expectations.",
             call.=FALSE)
      }
    }
  }
  out
}

#' Evaluate the phase-1 implementation halt conditions
#' @return A non-authoritative check table. It cannot create release evidence or
#'   grant Gate B authority.
#' @export
stpd_gate_b_v3_phase1_halt_report <- function() {
  bundle <- stpd_gate_b_v3_phase1_bundle()
  evaluate <- function(check_id, expression) {
    result <- tryCatch(
      list(value=force(expression), error=NA_character_),
      error=function(e) list(value=FALSE, error=conditionMessage(e))
    )
    passed <- identical(result$value, TRUE)
    detail <- if (passed) "validated" else result$error %||% "returned_non_true"
    data.frame(check_id=check_id, passed=passed, detail=detail,
               stringsAsFactors=FALSE)
  }
  checks <- list()
  checks[[1L]] <- evaluate("frozen_source_hashes", {
    roots <- c(
      ".", file.path("..", ".."),
      normalizePath(file.path(dirname(stpd_gate_b_v3_phase1_bundle_path()),
                              "..", "..", ".."), mustWork=FALSE)
    )
    found <- FALSE
    for (root in unique(roots)) {
      plan <- file.path(root,"docs","gate-b-v3-implementation-and-acceptance-plan.md")
      contract <- file.path(root,"docs","gate-b-v3-normative-data-identity-release-contract.md")
      if (file.exists(plan) && file.exists(contract)) {
        found <- identical(digest::digest(plan,algo="sha256",file=TRUE), bundle$plan_sha256) &&
          identical(digest::digest(contract,algo="sha256",file=TRUE), bundle$normative_contract_sha256)
        break
      }
    }
    if (!found && is.list(bundle$frozen_sources)) {
      found <- identical(
        digest::digest(bundle$frozen_sources$plan_utf8,
                       algo="sha256",serialize=FALSE),
        bundle$plan_sha256
      ) && identical(
        digest::digest(bundle$frozen_sources$normative_contract_utf8,
                       algo="sha256",serialize=FALSE),
        bundle$normative_contract_sha256
      )
    }
    found
  })
  checks[[2L]] <- evaluate("complete_schema_lint", {
    stpd_gate_b_v3_phase1_schema_lint(bundle); TRUE
  })
  checks[[3L]] <- evaluate("all_table_prototypes", {
    all(vapply(names(bundle$tables), function(name) {
      tryCatch({stpd_gate_b_v3_validate_table_prototype(
        stpd_gate_b_v3_empty_table(name,bundle),name,bundle); TRUE},
        error=function(e) FALSE)
    }, logical(1)))
  })
  checks[[4L]] <- evaluate("rfc8785_golden_vectors", {
    identical(stpd_gate_b_v3_canonical_json(list("\ue000"=2, "\U0001f600"=1)),
              "{\"\U0001f600\":1,\"\ue000\":2}") &&
      identical(stpd_gate_b_v3_canonical_json(c(1e-7,1e20,1e21,0)),
                "[1e-7,100000000000000000000,1e+21,0]") &&
      inherits(try(stpd_gate_b_v3_canonical_json(-0), silent=TRUE),
               "try-error")
  })
  checks[[5L]] <- evaluate("registry_content_addressed_roundtrip", {
    registry <- stpd_gate_b_v3_phase1_registry(TRUE)
    all(vapply(seq_len(nrow(registry)), function(i) {
      semantic_hash <- stpd_gate_b_v3_hash_raw(
        "stpd-registry-semantic-v1", registry$semantic_definition_json[i])
      id <- paste0("reg_", stpd_gate_b_v3_hash_raw(
        "stpd-registry-entry-v1", stpd_gate_b_v3_canonical_json(list(
          registry_domain=registry$registry_domain[i], code=registry$code[i],
          code_version=registry$code_version[i],
          semantic_definition_sha256=semantic_hash))))
      identical(semantic_hash,registry$semantic_definition_sha256[i]) &&
        identical(id,registry$registry_entry_id[i])
    }, logical(1)))
  })
  checks[[6L]] <- evaluate("fixture_expected_actual_closure", {
    fixture <- stpd_gate_b_v3_phase1_fixture_spec(TRUE)
    all(vapply(seq_len(nrow(fixture)), function(i) {
      actual <- stpd_gate_b_v3_canonical_json(
        stpd_gate_b_v3_run_phase1_fixture(fixture$fixture_id[i]))
      identical(actual, fixture$expected_result_json[i])
    }, logical(1)))
  })
  checks[[7L]] <- evaluate("fixture_test_discovery", {
    candidates <- c(
      file.path("tests", "testthat", "test_gate_b_v3_phase1_contract.R"),
      file.path("..", "..", "tests", "testthat",
                "test_gate_b_v3_phase1_contract.R")
    )
    existing <- candidates[file.exists(candidates)]
    proof <- bundle$fixture_test_discovery
    proof_valid <- is.list(proof) &&
      identical(proof$schema,"stpd_gate_b_v3_fixture_test_discovery_1") &&
      identical(proof$all_declared_tests_discovered,TRUE) &&
      length(proof$repository_relative_test_path)==1L &&
      identical(
        sort(unlist(proof$declared_test_names,use.names=FALSE),method="radix"),
        sort(unique(stpd_gate_b_v3_phase1_fixture_spec(FALSE)$test_name),
             method="radix")
      ) && grepl("^[0-9a-f]{64}$",proof$source_test_sha256)
    if (length(existing) < 1L) proof_valid else {
      path <- existing[[1L]]
      source <- paste(readLines(path,warn=FALSE),collapse="\n")
      proof_valid &&
        identical(digest::digest(path,algo="sha256",file=TRUE),
                  proof$source_test_sha256) &&
        all(vapply(unique(stpd_gate_b_v3_phase1_fixture_spec(FALSE)$test_name),
                   function(name) grepl(paste0('test_that("', name, '"'),
                                        source, fixed=TRUE), logical(1)))
    }
  })
  checks[[8L]] <- evaluate("resource_fail_closed_no_truncation", {
    limits <- setNames(stpd_gate_b_v3_complexity_contract()$limit,
                       stpd_gate_b_v3_complexity_contract()$budget_metric)
    limits[["edge_rows"]] <- limits[["edge_rows"]] + 1
    result <- stpd_gate_b_v3_resource_budget_compare(limits)
    identical(result$failure_code,"resource_budget_exceeded") &&
      identical(result$canonical_tables_published,0L) &&
      identical(result$truncated,FALSE)
  })
  checks[[9L]] <- evaluate("local_promotion_disabled", {
    fns <- c("stpd_multitrack_gate_b_promote_auto_product",
             "stpd_multitrack_gate_b_promote_final_tables",
             "stpd_multitrack_gate_b_promote_final_product")
    all(vapply(fns,function(fn) {
      tryCatch({get(fn,mode="function")(list()); FALSE},
               error=function(e) grepl("disabled",conditionMessage(e),fixed=TRUE))
    },logical(1)))
  })
  checks[[10L]] <- evaluate("table_specific_enum_and_status_contract", {
    identical(sort(unlist(bundle$enums$record_status),method="radix"),
              sort(c("predicted","review_confirmed","review_rejected"),
                   method="radix")) &&
      identical(sort(unlist(bundle$enums$label_access),method="radix"),
                sort(c("none","development_reference_only"),method="radix")) &&
      length(bundle$status_matrix)==9L &&
      all(vapply(names(bundle$tables),function(name) {
        spec <- bundle$tables[[name]]
        identical(spec$table_schema,
                  paste0("stpd_multitrack_v3_",name,"_1")) &&
          identical(spec$canonical_envelope_schema,
                    "stpd_canonical_table_v1")
      },logical(1)))
  })
  checks[[11L]] <- evaluate("concrete_acceptance_inputs_and_oracles", {
    cases <- stpd_gate_b_v3_phase1_fixture_inputs()
    cases <- cases[vapply(cases,function(x)
      identical(x$case_kind,"acceptance_scenario"),logical(1))]
    length(cases)==31L && all(vapply(cases,function(x) {
      length(x$case_input$input)>0L && identical(
        stpd_gate_b_v3_canonical_json(stpd_gate_b_v3_phase1_scenario(
          x$case_input$scenario_id,x$case_input$input)),
        stpd_gate_b_v3_canonical_json(stpd_gate_b_v3_phase1_expected_scenario(
          x$case_input$scenario_id)))
    },logical(1)))
  })
  checks[[12L]] <- evaluate("exact_hash_dag_contract", {
    stpd_gate_b_v3_phase1_hash_dag_lint(bundle); TRUE
  })
  checks[[13L]] <- evaluate("resource_staging_cleanup_postcondition", {
    root <- tempfile("gb3-halt-resource-");dir.create(root)
    on.exit(unlink(root,recursive=TRUE,force=TRUE),add=TRUE)
    limit <- stpd_gate_b_v3_complexity_contract()$limit[
      stpd_gate_b_v3_complexity_contract()$budget_metric=="candidate_rows"
    ][[1L]]
    result <- stpd_gate_b_v3_resource_staging_probe(root,list(list(
      op="reserve_candidate_rows",n_rows=limit+1
    )))
    identical(result$failure_code,"resource_budget_exceeded") &&
      isTRUE(result$staging_path_absent) &&
      length(list.files(root,all.files=TRUE,no..=TRUE))==0L
  })
  checks <- do.call(rbind, checks)
  checks$check_version <- "stpd_gate_b_v3_phase1_halt_check_2"
  checks$fixture_bundle_hash <- digest::digest(
    stpd_gate_b_v3_canonical_json(stpd_gate_b_v3_phase1_fixture_inputs()),
    algo="sha256",serialize=FALSE
  )
  checks$blocking <- !checks$passed
  checks$phase1_status <- if (any(checks$blocking)) {
    "failed_closed"
  } else "implementation_candidate_pending_independent_review"
  checks$authority_granted <- FALSE
  checks$release_evidence_created <- FALSE
  checks
}

stpd_gate_b_v3_phase1_hash_dag <- function(
    bundle = stpd_gate_b_v3_phase1_bundle()) {
  edges <- bundle$hash_dag$edges
  data.frame(
    parent=vapply(edges, `[[`, character(1), 1L),
    child=vapply(edges, `[[`, character(1), 2L),
    stringsAsFactors=FALSE
  )
}

stpd_gate_b_v3_required_hash_edge_keys <- function() c(
  "normalized_input_rows|normalized_input_manifest_hash",
  "requested_parameter_rows|requested_params_hash",
  "partition_membership_rows|partition_membership_manifest_hash",
  "threshold_policy_source_rows|threshold_policy_source_manifest_hash",
  "threshold_instance_source_rows|threshold_instance_source_manifest_hash",
  "qc_input_rows|qc_input_manifest_hash",
  "registry_semantic_rows|registry_manifest_hash",
  "source_manifest_hash|code_identity_hash","native_manifest_hash|code_identity_hash",
  "toolchain_manifest_hash|code_identity_hash","package_version|code_identity_hash",
  "normalized_input_manifest_hash|source_context_hash",
  "requested_params_hash|source_context_hash",
  "partition_membership_manifest_hash|source_context_hash",
  "threshold_policy_source_manifest_hash|source_context_hash",
  "threshold_instance_source_manifest_hash|source_context_hash",
  "qc_input_manifest_hash|source_context_hash","code_identity_hash|source_context_hash",
  "detector_policy_hash|source_context_hash","label_blind_contract_hash|source_context_hash",
  "source_context_hash|detection_root_id","detection_root_id|candidate_evidence_rows",
  "candidate_evidence_rows|candidate_universe_hash",
  "candidate_universe_hash|evidence_records","evidence_records|entity_ids",
  "entity_ids|lineage_records","entity_ids|canonical_tables",
  "transition_rows|history_head_hash","history_head_hash|canonical_tables",
  "evidence_records|canonical_tables","lineage_records|canonical_tables",
  "canonical_tables|canonical_table_envelopes",
  "canonical_table_envelopes|canonical_table_hashes",
  "canonical_table_hashes|canonical_manifest_core_hash",
  "product_kind|product_context_hash","detection_root_id|product_context_hash",
  "parent_auto_product_hash|product_context_hash","history_head_hash|product_context_hash",
  "canonical_manifest_core_hash|product_context_hash","contract_sha256|product_context_hash",
  "schema_contract_sha256|product_context_hash","product_context_hash|product_hash",
  "product_kind|product_hash","detection_root_id|product_hash",
  "source_context_hash|product_hash","parent_auto_product_hash|product_hash",
  "history_head_hash|product_hash","canonical_manifest_core_hash|product_hash",
  "contract_sha256|product_hash","schema_contract_sha256|product_hash",
  "product_hash|metadata_envelope","metadata_envelope|status_core_hash",
  "product_hash|status_core_hash",
  "fixture_expected_result_hashes|fixture_manifest_sha256",
  "fixture_manifest_sha256|gate_b_evidence_table_sha256",
  "test_result_bundle_hash|gate_b_evidence_table_sha256",
  "source_manifest_hash|gate_b_evidence_table_sha256",
  "test_manifest_sha256|gate_b_evidence_table_sha256",
  "contract_sha256|gate_b_evidence_table_sha256",
  "schema_contract_sha256|gate_b_evidence_table_sha256",
  "environment_manifest_sha256|gate_b_evidence_table_sha256",
  "gate_b_evidence_table_sha256|all_required_contracts_passed",
  "test_result_bundle_hash|all_required_contracts_passed",
  "required_contract_id_set_sha256|all_required_contracts_passed",
  "source_manifest_hash|package_build_manifest_sha256",
  "native_manifest_hash|package_build_manifest_sha256",
  "toolchain_manifest_hash|package_build_manifest_sha256",
  "package_version|package_build_manifest_sha256",
  "package_build_manifest_sha256|installed_manifest_sha256",
  "product_hash|artifact_manifest_hash",
  "release_attestation_schema|release_attestation_core_hash",
  "release_version|release_attestation_core_hash",
  "source_manifest_hash|release_attestation_core_hash",
  "native_manifest_hash|release_attestation_core_hash",
  "toolchain_manifest_hash|release_attestation_core_hash",
  "contract_sha256|release_attestation_core_hash",
  "schema_contract_sha256|release_attestation_core_hash",
  "fixture_manifest_sha256|release_attestation_core_hash",
  "test_manifest_sha256|release_attestation_core_hash",
  "test_result_bundle_hash|release_attestation_core_hash",
  "gate_b_evidence_table_sha256|release_attestation_core_hash",
  "required_contract_id_set_sha256|release_attestation_core_hash",
  "environment_manifest_sha256|release_attestation_core_hash",
  "R_version|release_attestation_core_hash",
  "toolchain_id|release_attestation_core_hash",
  "package_build_manifest_sha256|release_attestation_core_hash",
  "installed_manifest_sha256|release_attestation_core_hash",
  "approval_manifest_hash|release_attestation_core_hash",
  "all_required_contracts_passed|release_attestation_core_hash",
  "release_attestation_core_hash|embedded_release_signature",
  "release_attestation_core_hash|release_attestation_hash",
  "embedded_release_signature|release_attestation_hash",
  "product_hash|product_anchor_hash","detection_root_id|product_anchor_hash",
  "product_kind|product_anchor_hash","release_attestation_hash|product_anchor_hash",
  "normalized_input_manifest_hash|product_anchor_hash",
  "artifact_manifest_hash|product_anchor_hash","status_core_hash|product_anchor_hash",
  "release_sequence|product_anchor_hash","product_anchor_hash|product_attestation_hash",
  "package_build_manifest_sha256|tarball_sha256",
  "installed_manifest_sha256|tarball_sha256",
  "release_attestation_hash|tarball_sha256",
  "tarball_sha256|distribution_payload_sha256",
  "release_attestation_hash|distribution_payload_sha256",
  "release_version|distribution_payload_sha256",
  "distribution_payload_sha256|distribution_signature"
)

stpd_gate_b_v3_required_hash_nodes <- function() {
  sort(unique(unlist(strsplit(
    stpd_gate_b_v3_required_hash_edge_keys(), "|", fixed = TRUE
  ))), method = "radix")
}

stpd_gate_b_v3_required_forbidden_hash_edge_keys <- function() c(
  "product_hash|canonical_tables",
  "release_attestation_hash|product_hash",
  "artifact_manifest_hash|product_hash",
  "metadata_envelope|product_hash",
  "embedded_release_signature|release_attestation_core_hash",
  "tarball_sha256|release_attestation_hash",
  "distribution_signature|distribution_payload_sha256"
)

stpd_gate_b_v3_phase1_hash_dag_lint <- function(
    bundle = stpd_gate_b_v3_phase1_bundle()) {
  if (!identical(bundle$hash_dag$dag_schema,"stpd_gate_b_v3_hash_dag_1")) {
    stop("Gate B v3 hash DAG schema is not the frozen exact schema.",call.=FALSE)
  }
  claimed <- bundle$hash_dag$contract_sha256
  unhashed <- bundle$hash_dag
  unhashed$contract_sha256 <- NULL
  actual <- digest::digest(
    jsonlite::toJSON(unhashed,auto_unbox=TRUE,null="null",digits=NA,
                     pretty=FALSE),
    algo="sha256",serialize=FALSE
  )
  if (!identical(claimed,actual)) {
    stop("Gate B v3 exact hash-DAG contract digest is invalid.",call.=FALSE)
  }
  edges <- stpd_gate_b_v3_phase1_hash_dag(bundle)
  if (anyDuplicated(edges) ||
      any(!edges$parent %in% unlist(bundle$hash_dag$nodes)) ||
      any(!edges$child %in% unlist(bundle$hash_dag$nodes))) {
    stop("Gate B v3 hash DAG has duplicate or undeclared edges.", call.=FALSE)
  }
  forbidden <- bundle$hash_dag$forbidden_back_edges
  if (!is.list(forbidden) ||
      any(!vapply(forbidden,function(edge)
        (is.character(edge) || is.list(edge)) && length(edge)==2L &&
          all(vapply(edge,function(value) is.character(value) &&
            length(value)==1L && !is.na(value) && nzchar(value),logical(1))),
        logical(1)))) {
    stop("Gate B v3 forbidden hash-edge set is malformed.",call.=FALSE)
  }
  forbidden_key <- vapply(forbidden,function(edge)
    paste(unlist(edge,use.names=FALSE),collapse="|"),character(1))
  edge_key <- paste(edges$parent, edges$child, sep="|")
  required_edge_key <- stpd_gate_b_v3_required_hash_edge_keys()
  if (!setequal(edge_key,required_edge_key) ||
      length(edge_key)!=length(required_edge_key)) {
    stop("Gate B v3 hash DAG is not the frozen exact required edge set.",
         call.=FALSE)
  }
  required_nodes <- sort(unique(unlist(strsplit(required_edge_key,"|",
    fixed=TRUE))),method="radix")
  declared_nodes <- sort(unlist(bundle$hash_dag$nodes,use.names=FALSE),
                         method="radix")
  # Isolated audit envelope nodes are forbidden: every declared node must be
  # attached to the exact dependency graph.
  if (!identical(declared_nodes,required_nodes)) {
    stop("Gate B v3 hash DAG node set is not exact.",call.=FALSE)
  }
  required_forbidden_key <-
    stpd_gate_b_v3_required_forbidden_hash_edge_keys()
  if (!setequal(forbidden_key,required_forbidden_key) ||
      length(forbidden_key)!=length(required_forbidden_key)) {
    stop("Gate B v3 forbidden hash-edge set is not exact.",call.=FALSE)
  }
  if (any(forbidden_key %in% edge_key)) {
    stop("Gate B v3 hash DAG contains a forbidden self-reference.", call.=FALSE)
  }
  nodes <- unique(c(edges$parent, edges$child))
  indegree <- setNames(integer(length(nodes)), nodes)
  for (child in edges$child) indegree[[child]] <- indegree[[child]] + 1L
  queue <- sort(names(indegree)[indegree == 0L], method = "radix")
  visited <- character()
  while (length(queue) > 0L) {
    node <- queue[[1L]]
    queue <- queue[-1L]
    visited <- c(visited, node)
    children <- edges$child[edges$parent == node]
    for (child in children) {
      indegree[[child]] <- indegree[[child]] - 1L
      if (indegree[[child]] == 0L) {
        queue <- sort(unique(c(queue, child)), method = "radix")
      }
    }
  }
  if (length(visited) != length(nodes)) {
    stop("Gate B v3 hash DAG contains a cycle.", call. = FALSE)
  }
  invisible(TRUE)
}
