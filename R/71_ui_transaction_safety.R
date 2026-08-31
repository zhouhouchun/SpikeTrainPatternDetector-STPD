# Phase 0 UI safety: complete, bounded session transactions for destructive
# Shiny actions.  These helpers are deliberately independent of modal UI code:
# the server decides when a user has confirmed an action, while this layer owns
# atomic capture, rollback, commit, and undo semantics.

stpd_ui_transaction_schema_version <- function() "stpd.ui.transaction/3"

stpd_ui_transaction_action_registry <- function() {
  data.frame(
    action_id = c(
      "clear_all_datasets",
      "replace_workspace",
      "remove_dataset",
      "clear_manual_labels",
      "clear_auto_labels",
      "clear_final_audit",
      "legacy_overwrite_manual",
      "manual_label_edit",
      "legacy_promotion_revert",
      "collapse_duplicate_spikes"
    ),
    default_scope = c(
      "session", "session", "dataset", "selection", "selection", "trains",
      "trains", "selection", "trains", "dataset"
    ),
    label_zh = c(
      "\u6E05\u7A7A\u5185\u5B58\u4E2D\u7684\u5168\u90E8\u6570\u636E\u96C6",
      "\u52A0\u8F7D\u5DE5\u4F5C\u533A\u5E76\u66FF\u6362\u5F53\u524D\u4F1A\u8BDD",
      "\u79FB\u9664\u5F53\u524D\u6570\u636E\u96C6",
      "\u6E05\u9664 MANUAL \u6807\u7B7E",
      "\u6E05\u9664 AUTO \u6807\u7B7E",
      "\u6E05\u9664\u6700\u7EC8\u5BA1\u8BA1\u5C42",
      "\u65E7\u7248\u8986\u76D6 MANUAL \u6807\u7B7E",
      "\u7F16\u8F91 MANUAL \u6807\u7B7E",
      "\u64A4\u9500\u65E7\u7248\u5019\u9009\u5347\u7EA7",
      "\u5408\u5E76\u91CD\u590D spike"
    ),
    label_en = c(
      "Clear all in-memory datasets",
      "Load workspace and replace current session",
      "Remove current dataset",
      "Clear MANUAL labels",
      "Clear AUTO labels",
      "Clear final audit layer",
      "Legacy overwrite of MANUAL labels",
      "Edit MANUAL labels",
      "Revert legacy candidate promotion",
      "Collapse duplicate spikes"
    ),
    stringsAsFactors = FALSE
  )
}

stpd_ui_transaction_internal_fields <- function() {
  c(
    ".stpd_ui_undo_stack",
    ".stpd_ui_transaction_sequence",
    ".stpd_ui_pending_transaction",
    "ui_pending_confirmation",
    "ui_confirmation_nonce",
    "pending_workspace_import",
    # The Phase 0 stack supersedes the former single-train-only snapshot.  It
    # must not become recursively embedded in every new transaction.
    "manual_undo_snapshot"
  )
}

stpd_ui_transaction_guard_fields <- function() {
  c(
    "datasets", "current_id", "ui_run_identity_by_dataset",
    "manual_detector_eval", "manual_detector_eval_identity",
    "scientific_validation", "scientific_validation_identity",
    "parameter_delta_preview", "parameter_sensitivity",
    "possible_burst_promotion_preview",
    "possible_burst_promotion_preview_identity",
    "final_audit_last_summary", "final_audit_last_events"
  )
}

stpd_ui_transaction_assert_store <- function(rv) {
  if (inherits(rv, "reactivevalues") || is.environment(rv)) return(invisible(TRUE))
  stop(
    "stpd_ui_transaction_*(): rv must be a shiny reactiveValues object or an environment.",
    call. = FALSE
  )
}

stpd_ui_transaction_deep_clone <- function(x) {
  bytes <- tryCatch(
    serialize(x, NULL, version = 3L),
    error = function(e) {
      stop(
        "UI transaction state cannot be serialized safely: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  tryCatch(
    unserialize(bytes),
    error = function(e) {
      stop(
        "UI transaction state cannot be cloned safely: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

stpd_ui_transaction_store_values <- function(rv) {
  stpd_ui_transaction_assert_store(rv)
  if (inherits(rv, "reactivevalues")) {
    return(shiny::isolate(shiny::reactiveValuesToList(rv, all.names = TRUE)))
  }
  nms <- sort(ls(envir = rv, all.names = TRUE))
  if (length(nms) == 0L) return(list())
  mget(nms, envir = rv, inherits = FALSE)
}

stpd_ui_transaction_store_get <- function(rv, name, default = NULL) {
  stpd_ui_transaction_assert_store(rv)
  if (inherits(rv, "reactivevalues")) {
    out <- shiny::isolate(rv[[name]])
    if (is.null(out)) default else out
  } else if (exists(name, envir = rv, inherits = FALSE)) {
    get(name, envir = rv, inherits = FALSE)
  } else {
    default
  }
}

stpd_ui_transaction_store_set <- function(rv, name, value) {
  stpd_ui_transaction_assert_store(rv)
  if (inherits(rv, "reactivevalues")) {
    rv[[name]] <- value
  } else {
    assign(name, value, envir = rv)
  }
  invisible(value)
}

stpd_ui_transaction_store_remove <- function(rv, name) {
  stpd_ui_transaction_assert_store(rv)
  if (inherits(rv, "reactivevalues")) {
    # reactiveValues does not expose a binding-removal API.  NULL is the Shiny
    # representation of an absent optional state value.
    rv[[name]] <- NULL
  } else if (exists(name, envir = rv, inherits = FALSE)) {
    rm(list = name, envir = rv)
  }
  invisible(TRUE)
}

stpd_ui_transaction_encode_state <- function(state) {
  raw <- serialize(state, NULL, version = 3L)
  compressed <- memCompress(raw, type = "gzip")
  list(
    state_payload = compressed,
    payload_compression = "gzip",
    uncompressed_bytes = as.numeric(length(raw)),
    compressed_bytes = as.numeric(length(compressed))
  )
}

stpd_ui_transaction_state_digest <- function(
    capture_scope, fields, present_fields, state_payload,
    payload_compression, uncompressed_bytes, compressed_bytes) {
  digest::digest(
    list(
      schema_version = stpd_ui_transaction_schema_version(),
      capture_scope = capture_scope,
      fields = fields,
      present_fields = present_fields,
      state_payload = state_payload,
      payload_compression = payload_compression,
      uncompressed_bytes = uncompressed_bytes,
      compressed_bytes = compressed_bytes
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

stpd_ui_transaction_snapshot_state <- function(snapshot) {
  if (!is.list(snapshot) || !identical(snapshot$payload_compression, "gzip") ||
      !is.raw(snapshot$state_payload)) {
    stop("Invalid compressed UI transaction payload.", call. = FALSE)
  }
  raw <- tryCatch(
    memDecompress(snapshot$state_payload, type = "gzip"),
    error = function(e) {
      stop("UI transaction payload cannot be decompressed.", call. = FALSE)
    }
  )
  if (!identical(as.integer(length(raw)),
                 suppressWarnings(as.integer(snapshot$uncompressed_bytes)))) {
    stop("UI transaction payload length is invalid.", call. = FALSE)
  }
  tryCatch(
    unserialize(raw),
    error = function(e) {
      stop("UI transaction payload cannot be restored.", call. = FALSE)
    }
  )
}

stpd_ui_transaction_capture <- function(rv, fields = NULL) {
  values <- stpd_ui_transaction_store_values(rv)
  internal <- stpd_ui_transaction_internal_fields()
  available <- sort(setdiff(names(values), internal))

  if (is.null(fields)) {
    capture_scope <- "all_session_state"
    fields <- available
  } else {
    capture_scope <- "explicit_fields"
    fields <- sort(unique(as.character(fields)))
    fields <- fields[!is.na(fields) & nzchar(fields)]
    if (any(fields %in% internal)) {
      stop("UI transaction internals cannot be included in a state snapshot.", call. = FALSE)
    }
  }

  present <- intersect(fields, available)
  state <- values[present]
  encoded <- stpd_ui_transaction_encode_state(state)
  sha <- stpd_ui_transaction_state_digest(
    capture_scope, fields, present,
    encoded$state_payload, encoded$payload_compression,
    encoded$uncompressed_bytes, encoded$compressed_bytes
  )

  structure(
    list(
      schema_version = stpd_ui_transaction_schema_version(),
      capture_scope = capture_scope,
      fields = fields,
      present_fields = present,
      state_payload = encoded$state_payload,
      payload_compression = encoded$payload_compression,
      uncompressed_bytes = encoded$uncompressed_bytes,
      compressed_bytes = encoded$compressed_bytes,
      state_sha256 = sha
    ),
    class = c("stpd_ui_transaction_snapshot", "list")
  )
}

stpd_ui_transaction_guard_sha256 <- function(rv) {
  values <- stpd_ui_transaction_store_values(rv)
  fields <- sort(stpd_ui_transaction_guard_fields())
  # reactiveValues cannot remove a binding once it has been created; assigning
  # NULL is its representation of an absent optional field.  Canonicalize both
  # cases to the same fixed-width list so a valid LIFO undo does not become
  # unsafe merely because an intermediate action introduced then removed an
  # optional UI result.
  state <- lapply(fields, function(field) {
    if (field %in% names(values)) values[[field]] else NULL
  })
  names(state) <- fields
  digest::digest(
    list(
      schema_version = "stpd.ui.transaction.guard/1",
      fields = fields,
      state = state
    ),
    algo = "sha256", serialize = TRUE
  )
}

stpd_ui_transaction_validate_snapshot <- function(snapshot) {
  required <- c(
    "schema_version", "capture_scope", "fields", "present_fields",
    "state_payload", "payload_compression", "uncompressed_bytes",
    "compressed_bytes", "state_sha256"
  )
  if (!is.list(snapshot) || !all(required %in% names(snapshot))) {
    stop("Invalid UI transaction snapshot.", call. = FALSE)
  }
  if (!identical(snapshot$schema_version, stpd_ui_transaction_schema_version())) {
    stop("Unsupported UI transaction snapshot schema.", call. = FALSE)
  }
  if (!(snapshot$capture_scope %in% c("all_session_state", "explicit_fields"))) {
    stop("Invalid UI transaction capture scope.", call. = FALSE)
  }
  fields <- sort(unique(as.character(snapshot$fields)))
  present <- sort(unique(as.character(snapshot$present_fields)))
  if (!identical(fields, snapshot$fields) || !identical(present, snapshot$present_fields)) {
    stop("UI transaction snapshot fields are not canonical.", call. = FALSE)
  }
  if (!all(present %in% fields) || !is.raw(snapshot$state_payload) ||
      !identical(snapshot$payload_compression, "gzip") ||
      !identical(as.integer(length(snapshot$state_payload)),
                 suppressWarnings(as.integer(snapshot$compressed_bytes)))) {
    stop("UI transaction snapshot field coverage is invalid.", call. = FALSE)
  }
  expected <- stpd_ui_transaction_state_digest(
    snapshot$capture_scope, fields, present,
    snapshot$state_payload, snapshot$payload_compression,
    snapshot$uncompressed_bytes, snapshot$compressed_bytes
  )
  if (!identical(as.character(snapshot$state_sha256), expected)) {
    stop("UI transaction snapshot integrity check failed.", call. = FALSE)
  }
  state <- stpd_ui_transaction_snapshot_state(snapshot)
  if (!is.list(state) || !identical(names(state), present)) {
    stop("UI transaction snapshot decoded field coverage is invalid.", call. = FALSE)
  }
  invisible(TRUE)
}

stpd_ui_transaction_snapshot_from <- function(snapshot_or_record) {
  if (inherits(snapshot_or_record, "stpd_ui_transaction_record") ||
      (is.list(snapshot_or_record) && "snapshot" %in% names(snapshot_or_record))) {
    snapshot_or_record <- snapshot_or_record$snapshot
  }
  stpd_ui_transaction_validate_snapshot(snapshot_or_record)
  snapshot_or_record
}

stpd_ui_transaction_restore <- function(rv, snapshot_or_record) {
  snapshot <- stpd_ui_transaction_snapshot_from(snapshot_or_record)
  current <- stpd_ui_transaction_store_values(rv)
  current_fields <- sort(setdiff(
    names(current),
    stpd_ui_transaction_internal_fields()
  ))
  target_fields <- if (identical(snapshot$capture_scope, "all_session_state")) {
    sort(unique(c(snapshot$fields, current_fields)))
  } else {
    snapshot$fields
  }

  # Clone once more at the restoration boundary.  Neither a retained snapshot
  # nor a record returned by pop/peek may share an environment or other mutable
  # object with the newly restored live session state.
  restored_state <- stpd_ui_transaction_snapshot_state(snapshot)
  for (field in target_fields) {
    if (field %in% snapshot$present_fields) {
      stpd_ui_transaction_store_set(rv, field, restored_state[[field]])
    } else {
      stpd_ui_transaction_store_remove(rv, field)
    }
  }
  invisible(TRUE)
}

stpd_ui_transaction_next_id <- function(rv) {
  field <- ".stpd_ui_transaction_sequence"
  seq_no <- suppressWarnings(as.integer(stpd_ui_transaction_store_get(rv, field, 0L)))
  if (length(seq_no) != 1L || is.na(seq_no) || seq_no < 0L) seq_no <- 0L
  seq_no <- seq_no + 1L
  stpd_ui_transaction_store_set(rv, field, seq_no)
  sprintf("ui-tx-%08d", seq_no)
}

stpd_ui_transaction_utc <- function(now = Sys.time()) {
  now <- tryCatch(as.POSIXct(now, tz = "UTC"), error = function(e) NA)
  if (length(now) != 1L || is.na(now)) {
    stop("UI transaction time must be a single valid date-time.", call. = FALSE)
  }
  format(now, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC", usetz = FALSE)
}

stpd_ui_transaction_normalize_ids <- function(x) {
  x <- as.character(x %||% character())
  sort(unique(x[!is.na(x) & nzchar(x)]))
}

stpd_ui_transaction_record <- function(
  rv,
  action_id,
  dataset_ids = character(),
  train_ids = character(),
  target_tracks = character(),
  target_count = NA_integer_,
  scope = NULL,
  details = list(),
  now = Sys.time(),
  fields = NULL,
  history_max_depth = 10L,
  history_max_bytes = 64 * 1024^2
) {
  registry <- stpd_ui_transaction_action_registry()
  action_id <- as.character(action_id %||% "")[1]
  idx <- match(action_id, registry$action_id)
  if (is.na(idx)) {
    stop("Unsupported destructive UI action_id: ", action_id, call. = FALSE)
  }

  scope <- as.character(scope %||% registry$default_scope[idx])[1]
  if (!(scope %in% c("session", "dataset", "trains", "selection"))) {
    stop("Invalid UI transaction scope.", call. = FALSE)
  }
  target_count <- suppressWarnings(as.integer(target_count))[1]
  if (!is.na(target_count) && target_count < 0L) {
    stop("UI transaction target_count cannot be negative.", call. = FALSE)
  }
  if (is.null(details)) details <- list()
  if (!is.list(details)) stop("UI transaction details must be a list.", call. = FALSE)

  dataset_ids <- stpd_ui_transaction_normalize_ids(dataset_ids)
  train_ids <- stpd_ui_transaction_normalize_ids(train_ids)
  target_tracks <- stpd_ui_transaction_normalize_ids(target_tracks)
  created_at_utc <- stpd_ui_transaction_utc(now)
  details <- stpd_ui_transaction_deep_clone(details)
  history_max_depth <- stpd_ui_transaction_max_depth(history_max_depth)
  history_max_bytes <- stpd_ui_transaction_max_bytes(history_max_bytes)
  snapshot <- stpd_ui_transaction_capture(rv, fields = fields)
  transaction_id <- stpd_ui_transaction_next_id(rv)
  metadata <- list(
    schema_version = stpd_ui_transaction_schema_version(),
    transaction_id = transaction_id,
    action_id = action_id,
    action_label_zh = registry$label_zh[idx],
    action_label_en = registry$label_en[idx],
    scope = scope,
    dataset_ids = dataset_ids,
    train_ids = train_ids,
    target_tracks = target_tracks,
    target_count = target_count,
    created_at_utc = created_at_utc,
    pre_state_sha256 = snapshot$state_sha256,
    post_state_sha256 = "",
    finalized_at_utc = "",
    history_max_depth = history_max_depth,
    history_max_bytes = history_max_bytes,
    details = details
  )

  structure(
    list(metadata = metadata, snapshot = snapshot),
    class = c("stpd_ui_transaction_record", "list")
  )
}

stpd_ui_transaction_validate_record <- function(record) {
  if (!inherits(record, "stpd_ui_transaction_record") ||
      !is.list(record$metadata) || is.null(record$snapshot)) {
    stop("Invalid UI transaction record.", call. = FALSE)
  }
  required <- c(
    "schema_version", "transaction_id", "action_id", "action_label_zh",
    "action_label_en", "scope", "dataset_ids", "train_ids",
    "target_tracks", "target_count", "created_at_utc",
    "pre_state_sha256", "post_state_sha256", "finalized_at_utc",
    "history_max_depth", "history_max_bytes", "details"
  )
  if (!all(required %in% names(record$metadata))) {
    stop("Incomplete UI transaction metadata.", call. = FALSE)
  }
  if (!(record$metadata$action_id %in%
        stpd_ui_transaction_action_registry()$action_id)) {
    stop("Unsupported UI transaction action metadata.", call. = FALSE)
  }
  stpd_ui_transaction_validate_snapshot(record$snapshot)
  if (!identical(record$metadata$pre_state_sha256, record$snapshot$state_sha256)) {
    stop("UI transaction metadata does not match its snapshot.", call. = FALSE)
  }
  stpd_ui_transaction_max_depth(record$metadata$history_max_depth)
  stpd_ui_transaction_max_bytes(record$metadata$history_max_bytes)
  invisible(TRUE)
}

stpd_ui_transaction_max_depth <- function(max_depth = 10L) {
  max_depth <- suppressWarnings(as.integer(max_depth))[1]
  if (is.na(max_depth) || max_depth < 1L || max_depth > 100L) {
    stop("UI transaction max_depth must be an integer from 1 to 100.", call. = FALSE)
  }
  max_depth
}

stpd_ui_transaction_max_bytes <- function(max_bytes = 64 * 1024^2) {
  max_bytes <- suppressWarnings(as.numeric(max_bytes))[1]
  if (!is.finite(max_bytes) || max_bytes < 1) {
    stop("UI transaction max_bytes must be a positive finite number.", call. = FALSE)
  }
  max_bytes
}

stpd_ui_transaction_record_bytes <- function(record) {
  if (!is.list(record) || is.null(record$snapshot)) {
    stop("Invalid UI transaction record in the undo stack.", call. = FALSE)
  }
  as.numeric(utils::object.size(record))
}

stpd_ui_transaction_trim_stack <- function(stack, max_depth, max_bytes) {
  max_depth <- stpd_ui_transaction_max_depth(max_depth)
  max_bytes <- stpd_ui_transaction_max_bytes(max_bytes)
  while (length(stack) > max_depth ||
         (length(stack) > 1L &&
          sum(vapply(stack, stpd_ui_transaction_record_bytes, numeric(1))) > max_bytes)) {
    stack <- stack[-1L]
  }
  stack
}

stpd_ui_transaction_stack <- function(rv) {
  stack <- stpd_ui_transaction_store_get(rv, ".stpd_ui_undo_stack", list())
  if (is.null(stack)) stack <- list()
  if (!is.list(stack)) stop("UI transaction undo stack is corrupted.", call. = FALSE)
  stack
}

stpd_ui_transaction_depth <- function(rv) length(stpd_ui_transaction_stack(rv))

stpd_ui_transaction_ids <- function(rv) {
  stack <- stpd_ui_transaction_stack(rv)
  if (length(stack) == 0L) return(character())
  vapply(
    stack,
    function(record) as.character(record$metadata$transaction_id %||% "")[1],
    character(1)
  )
}

stpd_ui_transaction_commit <- function(
    rv, record, max_depth = 10L, max_bytes = 64 * 1024^2,
    trim_history = TRUE) {
  stpd_ui_transaction_validate_record(record)
  max_depth <- stpd_ui_transaction_max_depth(max_depth)
  max_bytes <- stpd_ui_transaction_max_bytes(max_bytes)
  stack <- stpd_ui_transaction_stack(rv)
  stack[[length(stack) + 1L]] <- record
  if (isTRUE(trim_history)) {
    stack <- stpd_ui_transaction_trim_stack(stack, max_depth, max_bytes)
  }
  stpd_ui_transaction_store_set(rv, ".stpd_ui_undo_stack", stack)
  invisible(record)
}

stpd_ui_transaction_finalize <- function(rv, transaction_id = NULL,
                                         now = Sys.time()) {
  stack <- stpd_ui_transaction_stack(rv)
  if (length(stack) == 0L) return(NULL)
  transaction_id <- as.character(transaction_id %||% "")[1]
  ids <- vapply(
    stack,
    function(record) as.character(record$metadata$transaction_id %||% "")[1],
    character(1)
  )
  idx <- if (nzchar(transaction_id)) match(transaction_id, ids) else length(stack)
  if (is.na(idx) || idx < 1L) return(NULL)
  record <- stack[[idx]]
  stpd_ui_transaction_validate_record(record)
  if (nzchar(as.character(record$metadata$post_state_sha256 %||% "")[1])) {
    return(invisible(record))
  }
  record$metadata$post_state_sha256 <- stpd_ui_transaction_guard_sha256(rv)
  record$metadata$finalized_at_utc <- stpd_ui_transaction_utc(now)
  stack[[idx]] <- record
  stack <- stpd_ui_transaction_trim_stack(
    stack,
    record$metadata$history_max_depth,
    record$metadata$history_max_bytes
  )
  stpd_ui_transaction_store_set(rv, ".stpd_ui_undo_stack", stack)
  invisible(record)
}

stpd_ui_transaction_abort <- function(rv, transaction_id = NULL) {
  stack <- stpd_ui_transaction_stack(rv)
  if (length(stack) == 0L) return(NULL)
  transaction_id <- as.character(transaction_id %||% "")[1]
  idx <- length(stack)
  record <- stack[[idx]]
  record_id <- as.character(record$metadata$transaction_id %||% "")[1]
  if (nzchar(transaction_id) && !identical(transaction_id, record_id)) {
    stop("Only the newest UI transaction can be aborted safely.", call. = FALSE)
  }
  stpd_ui_transaction_validate_record(record)
  if (nzchar(as.character(record$metadata$post_state_sha256 %||% "")[1])) {
    stop("A finalized UI transaction cannot be aborted.", call. = FALSE)
  }

  # Restore before consuming the record. If restoration fails, the recoverable
  # snapshot remains at the top of the stack.
  stpd_ui_transaction_restore(rv, record)
  stack <- if (idx == 1L) list() else stack[-idx]
  stpd_ui_transaction_store_set(rv, ".stpd_ui_undo_stack", stack)
  invisible(stpd_ui_transaction_deep_clone(record$metadata))
}

stpd_ui_transaction_abort_new <- function(rv, prior_transaction_ids = character()) {
  prior_transaction_ids <- unique(as.character(prior_transaction_ids %||% character()))
  prior_transaction_ids <- prior_transaction_ids[
    !is.na(prior_transaction_ids) & nzchar(prior_transaction_ids)
  ]
  aborted <- list()
  repeat {
    stack <- stpd_ui_transaction_stack(rv)
    if (length(stack) == 0L) break
    record <- stack[[length(stack)]]
    record_id <- as.character(record$metadata$transaction_id %||% "")[1]
    finalized <- nzchar(as.character(
      record$metadata$post_state_sha256 %||% ""
    )[1])
    if (record_id %in% prior_transaction_ids || finalized) break
    aborted[[length(aborted) + 1L]] <- stpd_ui_transaction_abort(
      rv, transaction_id = record_id
    )
  }
  invisible(aborted)
}

stpd_ui_transaction_push <- function(
  rv,
  action_id,
  dataset_ids = character(),
  train_ids = character(),
  target_tracks = character(),
  target_count = NA_integer_,
  scope = NULL,
  details = list(),
  now = Sys.time(),
  fields = NULL,
  max_depth = 10L,
  max_bytes = 64 * 1024^2
) {
  stack <- stpd_ui_transaction_stack(rv)
  if (length(stack) > 0L &&
      !nzchar(as.character(stack[[length(stack)]]$metadata$post_state_sha256 %||% "")[1])) {
    stpd_ui_transaction_finalize(rv)
  }
  record <- stpd_ui_transaction_record(
    rv = rv,
    action_id = action_id,
    dataset_ids = dataset_ids,
    train_ids = train_ids,
    target_tracks = target_tracks,
    target_count = target_count,
    scope = scope,
    details = details,
    now = now,
    fields = fields,
    history_max_depth = max_depth,
    history_max_bytes = max_bytes
  )
  stpd_ui_transaction_commit(
    rv, record, max_depth = max_depth, max_bytes = max_bytes,
    trim_history = FALSE
  )
}

stpd_ui_transaction_peek <- function(rv) {
  stack <- stpd_ui_transaction_stack(rv)
  if (length(stack) == 0L) return(NULL)
  stpd_ui_transaction_deep_clone(stack[[length(stack)]])
}

stpd_ui_transaction_pop <- function(rv) {
  stack <- stpd_ui_transaction_stack(rv)
  if (length(stack) == 0L) return(NULL)
  record <- stpd_ui_transaction_deep_clone(stack[[length(stack)]])
  stack <- if (length(stack) == 1L) list() else stack[-length(stack)]
  stpd_ui_transaction_store_set(rv, ".stpd_ui_undo_stack", stack)
  record
}

stpd_ui_transaction_history <- function(rv) {
  stack <- stpd_ui_transaction_stack(rv)
  lapply(stack, function(record) stpd_ui_transaction_deep_clone(record$metadata))
}

stpd_ui_transaction_undo <- function(rv) {
  record <- stpd_ui_transaction_peek(rv)
  if (is.null(record)) return(NULL)
  post_sha <- as.character(record$metadata$post_state_sha256 %||% "")[1]
  current_sha <- stpd_ui_transaction_guard_sha256(rv)
  if (!nzchar(post_sha) || !identical(post_sha, current_sha)) {
    # A later, untracked detector/validation/import change would be lost by a
    # full-session restore.  Once this happens the older full snapshots can no
    # longer be applied safely, so invalidate the stack but never touch the
    # user's scientific state.
    stpd_ui_transaction_store_set(rv, ".stpd_ui_undo_stack", list())
    stop(
      "Undo is no longer safe because the dataset or scientific results changed after that action. The stale undo history was cleared; no data were modified.",
      call. = FALSE
    )
  }
  before_undo <- stpd_ui_transaction_capture(rv)

  restored <- tryCatch(
    {
      stpd_ui_transaction_restore(rv, record)
      TRUE
    },
    error = identity
  )
  if (inherits(restored, "error")) {
    try(stpd_ui_transaction_restore(rv, before_undo), silent = TRUE)
    stop(restored)
  }

  # Remove the record only after the state restoration succeeds.  A damaged
  # record therefore cannot consume the user's last recoverable snapshot.
  stack <- stpd_ui_transaction_stack(rv)
  stack <- if (length(stack) == 1L) list() else stack[-length(stack)]
  stpd_ui_transaction_store_set(rv, ".stpd_ui_undo_stack", stack)
  list(
    metadata = stpd_ui_transaction_deep_clone(record$metadata),
    remaining_depth = length(stack)
  )
}

stpd_ui_transaction_run <- function(
  rv,
  action_id,
  mutate,
  confirmed = TRUE,
  dataset_ids = character(),
  train_ids = character(),
  target_tracks = character(),
  target_count = NA_integer_,
  scope = NULL,
  details = list(),
  now = Sys.time(),
  fields = NULL,
  max_depth = 10L,
  max_bytes = 64 * 1024^2
) {
  stpd_ui_transaction_assert_store(rv)
  if (!isTRUE(confirmed)) {
    return(structure(
      list(
        applied = FALSE,
        cancelled = TRUE,
        value = NULL,
        metadata = NULL,
        undo_depth = stpd_ui_transaction_depth(rv)
      ),
      class = c("stpd_ui_transaction_result", "list")
    ))
  }
  if (!is.function(mutate)) {
    stop("UI transaction mutate must be a zero-argument function.", call. = FALSE)
  }

  sequence_field <- ".stpd_ui_transaction_sequence"
  sequence_values <- stpd_ui_transaction_store_values(rv)
  sequence_was_present <- sequence_field %in% names(sequence_values)
  sequence_before <- stpd_ui_transaction_store_get(rv, sequence_field, 0L)
  restore_sequence <- function() {
    if (sequence_was_present) {
      stpd_ui_transaction_store_set(rv, sequence_field, sequence_before)
    } else {
      stpd_ui_transaction_store_remove(rv, sequence_field)
    }
    invisible(TRUE)
  }

  record <- stpd_ui_transaction_record(
    rv = rv,
    action_id = action_id,
    dataset_ids = dataset_ids,
    train_ids = train_ids,
    target_tracks = target_tracks,
    target_count = target_count,
    scope = scope,
    details = details,
    now = now,
    fields = fields,
    history_max_depth = max_depth,
    history_max_bytes = max_bytes
  )

  value <- tryCatch(mutate(), error = identity)
  if (inherits(value, "error")) {
    rollback <- tryCatch(
      {
        stpd_ui_transaction_restore(rv, record)
        TRUE
      },
      error = identity
    )
    if (inherits(rollback, "error")) {
      stop(
        "Destructive UI action failed and its rollback also failed: ",
        conditionMessage(value), " / rollback: ", conditionMessage(rollback),
        call. = FALSE
      )
    }
    restore_sequence()
    stop(value)
  }

  record$metadata$post_state_sha256 <- stpd_ui_transaction_guard_sha256(rv)
  record$metadata$finalized_at_utc <- stpd_ui_transaction_utc()
  committed <- tryCatch(
    stpd_ui_transaction_commit(
      rv, record, max_depth = max_depth, max_bytes = max_bytes
    ),
    error = identity
  )
  if (inherits(committed, "error")) {
    rollback <- tryCatch(
      {
        stpd_ui_transaction_restore(rv, record)
        TRUE
      },
      error = identity
    )
    if (inherits(rollback, "error")) {
      stop(
        "Destructive UI action could not be recorded and rollback also failed: ",
        conditionMessage(committed), " / rollback: ", conditionMessage(rollback),
        call. = FALSE
      )
    }
    restore_sequence()
    stop(committed)
  }

  structure(
    list(
      applied = TRUE,
      cancelled = FALSE,
      value = value,
      metadata = stpd_ui_transaction_deep_clone(record$metadata),
      undo_depth = stpd_ui_transaction_depth(rv)
    ),
    class = c("stpd_ui_transaction_result", "list")
  )
}
