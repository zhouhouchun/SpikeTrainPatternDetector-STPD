# Phase B1: strict provider-coordinate normalization -------------------------

STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION <- "stpd_provider_adapter_protocol_v1"
STPD_PROVIDER_B1_MAX_CHARACTER_BYTES <- 65536L
STPD_PROVIDER_B1_TIME_ROUNDOFF_CAP_SEC <- 1e-12
STPD_PROVIDER_B1_MAX_SOURCE_FIELDS <- 1024L
STPD_PROVIDER_B1_MAX_TOTAL_SOURCE_FIELDS <- 5000000L

stpd_provider_adapter_protocol_v1 <- function() {
  list(
    protocol_version = STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION,
    provider_contract_sha256 = stpd_provider_contract_hash(),
    coordinate_version = STPD_PROVIDER_COORDINATE_VERSION,
    hash_primitive = list(
      algorithm = "SHA-256",
      preimage = "UTF8(domain) || 0x00 || UTF8(canonical_json(payload))",
      canonical_json = stpd_provider_identity_contract()$canonical_json
    ),
    timestamp_leaf = list(
      unit = "seconds", storage = "IEEE-754 binary64 little-endian",
      hash = "SHA-256 exact bytes without serialization or header",
      order = "original spike-row order",
      validity = "finite, no negative zero, strictly increasing, unique"
    ),
    timestamp_spine = list(
      domain = "stpd-dataset-timestamp-spine-v1",
      fields = c("protocol_version", "coordinate_version", "train_key",
                 "n_spikes", "timestamp_unit", "train_timestamp_sha256"),
      train_order = "UTF-8 bytewise radix order"
    ),
    dataset_snapshot = list(
      domain = "stpd-provider-dataset-snapshot-v1",
      fields = c("protocol_sha256", "timestamp_spine_sha256",
                 "acquisition_context_sha256", "qc_policy_sha256"),
      absence_sentinels = list(
        acquisition_context = list(
          domain = "stpd-provider-acquisition-context-none-v1",
          payload = list(status = "not_supplied")
        ),
        qc_policy = list(
          domain = "stpd-provider-qc-policy-none-v1",
          payload = list(status = "not_supplied")
        )
      ),
      excluded = c("manual", "reference", "auto_results", "review",
                   "created_at", "train_settings")
    ),
    coverage_manifest = list(
      domain = "stpd-provider-coverage-manifest-v1",
      payload_fields = c("input_mode", "rows"),
      row_fields = c("train_key", "n_spikes", "train_timestamp_sha256"),
      input_modes = c("interval", "per_isi"),
      train_order = "UTF-8 bytewise radix order",
      scope = "exactly the selected timestamp spine"
    ),
    source_record = list(
      domain = "stpd-provider-source-record-v1",
      representation = "ordered typed scalar fields",
      envelope = "fields",
      field_keys = c("name", "type", "is_na", "value"),
      allowed_types = c("null", "logical", "integer", "double", "character")
    ),
    time_alignment = list(
      rule = "all timestamps within declared tolerance; exactly one required",
      nearest_snap = FALSE,
      exact_seconds_tolerance_sec = 0,
      converted_unit_roundoff_cap_sec = STPD_PROVIDER_B1_TIME_ROUNDOFF_CAP_SEC,
      converted_unit_formula = paste(
        "min(1e-12, 8 * binary64_epsilon *",
        "max(1, abs(converted_start_sec), abs(converted_end_sec)))"
      ),
      declared_resolution_formula =
        "max(converted_unit_envelope, source_time_resolution_sec / 2)",
      local_gap_guard = "tolerance < half each matched spike local gap"
    ),
    normalization_audit_identity = list(
      domain = "stpd-normalization-audit-v1",
      authoritative_fields =
        stpd_provider_identity_contract()$payload_fields$normalization_audit,
      excluded_fields = c(
        "schema_version", "normalization_audit_id", "error_detail"
      )
    ),
    transaction = list(
      record_failures_are_audited = TRUE,
      batch_atomic_accept_requires_all_normalized = TRUE,
      partial_candidate_publication = FALSE,
      processed_train_coverage_required = TRUE,
      processed_train_coverage = "exactly the selected timestamp spine"
    ),
    resource_limits = c(
      max_trains = 100000L, max_total_timestamps = 10000000L,
      max_source_records = 1000000L,
      max_source_fields_per_record = STPD_PROVIDER_B1_MAX_SOURCE_FIELDS,
      max_total_source_fields = STPD_PROVIDER_B1_MAX_TOTAL_SOURCE_FIELDS,
      max_character_bytes = STPD_PROVIDER_B1_MAX_CHARACTER_BYTES
    )
  )
}

stpd_provider_adapter_protocol_hash_v1 <- function() {
  stpd_provider_hash_domain(
    "stpd-provider-adapter-protocol-v1",
    stpd_provider_adapter_protocol_v1()
  )
}

stpd_provider_b1_abort <- function(code, message, table = "normalization",
                                   row = NA_integer_, column = NA_character_,
                                   value = NULL,
                                   provider_run_id = NA_character_,
                                   source_record_key = NA_character_) {
  stpd_provider_contract_abort(
    code = code, message = message, table = table, row = row,
    column = column, provider_run_id = provider_run_id,
    source_record_key = source_record_key, offending_value = value
  )
}

stpd_provider_b1_sha_ok <- function(x) {
  is.character(x) && !is.object(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[0-9a-f]{64}$", x)
}

stpd_provider_b1_negative_zero <- function(x) {
  is.double(x) & x == 0 & is.infinite(1 / x) & (1 / x) < 0
}

stpd_provider_b1_utf8_scalar <- function(x, context, allow_empty = FALSE) {
  if (!is.character(x) || is.object(x) || length(x) != 1L || is.na(x) ||
      (!allow_empty && !nzchar(x)) || Encoding(x) == "bytes") {
    stpd_provider_b1_abort(
      "type_invalid",
      paste0(
        context, " must be one ",
        if (allow_empty) "UTF-8 string." else "non-empty UTF-8 string."
      ),
      column = context, value = x
    )
  }
  converted <- enc2utf8(x)
  byte_preserved <- identical(charToRaw(x), charToRaw(converted))
  if (!byte_preserved || !stringi::stri_enc_isutf8(converted) ||
      !identical(stringi::stri_trans_nfc(converted), converted)) {
    stpd_provider_b1_abort(
      "type_invalid",
      paste0(context, " must already be valid NFC UTF-8; no silent rewrite is allowed."),
      column = context, value = x
    )
  }
  if (nchar(converted, type = "bytes") >
      STPD_PROVIDER_B1_MAX_CHARACTER_BYTES) {
    stpd_provider_b1_abort(
      "resource_limit_exceeded", paste0(context, " exceeds the byte limit."),
      column = context
    )
  }
  converted
}

stpd_provider_b1_raw_named_list <- function(x, context) {
  if (typeof(x) != "list" || !is.null(attr(x, "class", exact = TRUE))) {
    stpd_provider_b1_abort("type_invalid", paste0(context, " must be a list."))
  }
  nms <- attr(x, "names", exact = TRUE)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stpd_provider_b1_abort(
      "type_invalid", paste0(context, " names must be unique and non-empty.")
    )
  }
  nms <- unname(vapply(
    nms, stpd_provider_b1_utf8_scalar, character(1),
    context = paste0(context, " name")
  ))
  out <- unclass(x)
  attributes(out) <- list(names = nms)
  out
}

stpd_provider_b1_one_row_data_frame <- function(x, context) {
  if (typeof(x) != "list" ||
      !identical(attr(x, "class", exact = TRUE), "data.frame")) {
    stpd_provider_b1_abort(
      "type_invalid", paste0(context, " must be a base data.frame."),
      table = context
    )
  }
  nms <- attr(x, "names", exact = TRUE)
  row_names <- attr(x, "row.names", exact = TRUE)
  if (!is.character(nms) || is.object(nms) || anyNA(nms) ||
      any(!nzchar(nms)) || anyDuplicated(nms) ||
      !typeof(row_names) %in% c("integer", "character") ||
      is.object(row_names) || length(row_names) != 1L) {
    stpd_provider_b1_abort(
      "type_invalid", paste0(context, " must contain exactly one row."),
      table = context
    )
  }
  raw <- unclass(x)
  attributes(raw) <- list(names = as.character(nms))
  out <- lapply(seq_along(raw), function(i) {
    column <- .subset2(raw, i)
    if (!is.null(attr(column, "class", exact = TRUE)) ||
        !is.null(attr(column, "dim", exact = TRUE)) || length(column) != 1L) {
      stpd_provider_b1_abort(
        "type_invalid", paste0(context, " must contain exactly one row."),
        table = context, column = nms[[i]]
      )
    }
    .subset2(column, 1L)
  })
  names(out) <- nms
  out
}

stpd_provider_b1_plain_source_record <- function(source_record) {
  if (identical(attr(source_record, "class", exact = TRUE), "data.frame")) {
    source_record <- stpd_provider_b1_one_row_data_frame(
      source_record, "source_record"
    )
  } else if (!is.null(attr(source_record, "class", exact = TRUE))) {
    stpd_provider_b1_abort(
      "executable_import_forbidden",
      "Classed source-record containers are forbidden.",
      table = "source_record"
    )
  }
  record <- stpd_provider_b1_raw_named_list(source_record, "Source record")
  if (length(record) > STPD_PROVIDER_B1_MAX_SOURCE_FIELDS) {
    stpd_provider_b1_abort(
      "resource_limit_exceeded", "Source record exceeds the field-count limit.",
      table = "source_record"
    )
  }
  record
}

stpd_provider_b1_utf8_sort_index <- function(x) {
  keys <- vapply(x, function(value) {
    paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(value)))), collapse = "")
  }, character(1))
  order(keys, method = "radix")
}

stpd_provider_b1_time_resolution <- function(x) {
  if (!typeof(x) %in% c("integer", "double") || is.object(x) ||
      length(x) != 1L || !is.finite(x) ||
      stpd_provider_b1_negative_zero(as.double(x)) || x < 0) {
    stpd_provider_b1_abort(
      "time_tolerance_invalid",
      "source_time_resolution_sec must be one finite non-negative base number."
    )
  }
  as.double(x)
}

stpd_provider_b1_conversion_tolerance <- function(seconds, source_time_unit) {
  if (identical(source_time_unit, "s")) return(0)
  min(
    STPD_PROVIDER_B1_TIME_ROUNDOFF_CAP_SEC,
    8 * .Machine$double.eps * max(c(1, abs(seconds)))
  )
}

stpd_provider_train_timestamp_sha256_v1 <- function(timestamps_sec) {
  if (!is.numeric(timestamps_sec) || is.object(timestamps_sec)) {
    stpd_provider_b1_abort(
      "type_invalid", "Timestamp spine must be a base numeric vector.",
      table = "timestamp_spine", column = "timestamp_sec"
    )
  }
  timestamps_sec <- as.double(timestamps_sec)
  if (any(!is.finite(timestamps_sec)) ||
      any(stpd_provider_b1_negative_zero(timestamps_sec))) {
    stpd_provider_b1_abort(
      "coordinate_non_finite",
      "Timestamp spine contains a non-finite value or negative zero.",
      table = "timestamp_spine", column = "timestamp_sec"
    )
  }
  if (length(timestamps_sec) > 1L) {
    delta <- diff(timestamps_sec)
    if (any(delta == 0)) {
      stpd_provider_b1_abort(
        "timestamp_duplicate", "Timestamp spine contains a duplicate.",
        table = "timestamp_spine", column = "timestamp_sec"
      )
    }
    if (any(delta < 0)) {
      stpd_provider_b1_abort(
        "timestamp_non_monotonic",
        "Timestamp spine is not strictly increasing in original row order.",
        table = "timestamp_spine", column = "timestamp_sec"
      )
    }
  }
  connection <- rawConnection(raw(), open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(timestamps_sec, connection, size = 8L, endian = "little")
  bytes <- rawConnectionValue(connection)
  close(connection)
  on.exit(NULL, add = FALSE)
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

stpd_provider_b1_optional_sha <- function(x, domain, field) {
  if (is.null(x)) {
    return(stpd_provider_hash_domain(domain, list(status = "not_supplied")))
  }
  if (!stpd_provider_b1_sha_ok(x)) {
    stpd_provider_b1_abort(
      "sha256_invalid", paste0(field, " must be lowercase SHA-256."),
      table = "timestamp_spine", column = field, value = x
    )
  }
  x
}

stpd_provider_build_spine_v1 <- function(
    ds, selected_trains = NULL, acquisition_context_sha256 = NULL,
    qc_policy_sha256 = NULL) {
  ds_raw <- stpd_provider_b1_raw_named_list(ds, "Provider dataset")
  trains_index <- match("trains", names(ds_raw))
  if (is.na(trains_index)) {
    stpd_provider_b1_abort(
      "required_field_missing", "Provider dataset is missing trains.",
      table = "timestamp_spine", column = "trains"
    )
  }
  trains <- stpd_provider_b1_raw_named_list(
    .subset2(ds_raw, trains_index), "Provider train container"
  )
  train_names <- names(trains)
  if (is.null(selected_trains)) selected_trains <- train_names
  if (!is.character(selected_trains) || is.object(selected_trains) ||
      anyNA(selected_trains) ||
      !length(selected_trains) || anyDuplicated(selected_trains)) {
    stpd_provider_b1_abort(
      "type_invalid",
      "selected_trains must be a non-empty unique character vector.",
      table = "timestamp_spine", column = "selected_trains"
    )
  }
  selected_trains <- unname(vapply(
    selected_trains, stpd_provider_b1_utf8_scalar, character(1),
    context = "train_key"
  ))
  train_names_utf8 <- unname(vapply(
    train_names, stpd_provider_b1_utf8_scalar, character(1),
    context = "train_key"
  ))
  if (anyDuplicated(train_names_utf8)) {
    stpd_provider_b1_abort(
      "type_invalid", "UTF-8 train keys are not unique.",
      table = "timestamp_spine", column = "train_key"
    )
  }
  missing <- setdiff(selected_trains, train_names_utf8)
  if (length(missing)) {
    stpd_provider_b1_abort(
      "train_not_found", "A selected train is absent from the dataset.",
      table = "timestamp_spine", column = "train_key", value = missing[[1L]]
    )
  }
  limits <- stpd_provider_adapter_protocol_v1()$resource_limits
  if (length(selected_trains) > limits[["max_trains"]]) {
    stpd_provider_b1_abort(
      "resource_limit_exceeded", "Train count exceeds the B1 limit.",
      table = "timestamp_spine"
    )
  }
  selected_trains <- selected_trains[stpd_provider_b1_utf8_sort_index(
    selected_trains
  )]
  selected_train_hits <- match(selected_trains, train_names_utf8)
  timestamps <- vector("list", length(selected_trains))
  names(timestamps) <- selected_trains
  leaf_hash <- character(length(selected_trains))
  n_spikes <- integer(length(selected_trains))
  total <- 0
  for (i in seq_along(selected_trains)) {
    train <- selected_trains[[i]]
    dat <- .subset2(trains, selected_train_hits[[i]])
    dat_type <- typeof(dat)
    dat_class <- attr(dat, "class", exact = TRUE)
    dat_names <- attr(dat, "names", exact = TRUE)
    if (dat_type != "list" || !identical(dat_class, "data.frame") ||
        is.null(dat_names) || anyNA(dat_names) || any(!nzchar(dat_names)) ||
        anyDuplicated(dat_names)) {
      stpd_provider_b1_abort(
        "type_invalid", "Each train must be a data frame with unique columns.",
        table = "timestamp_spine", row = i, value = train
      )
    }
    timestamp_index <- match("timestamp_sec", dat_names)
    if (is.na(timestamp_index)) {
      stpd_provider_b1_abort(
        "required_field_missing", "Train is missing timestamp_sec.",
        table = "timestamp_spine", row = i, column = "timestamp_sec",
        value = train
      )
    }
    dat_raw <- unclass(dat)
    attributes(dat_raw) <- list(names = as.character(dat_names))
    value <- .subset2(dat_raw, timestamp_index)
    value_attributes <- attributes(value)
    allowed_attributes <- is.null(value_attributes) ||
      identical(names(value_attributes), "names")
    if (is.object(value) || !allowed_attributes ||
        !typeof(value) %in% c("integer", "double")) {
      stpd_provider_b1_abort(
        "type_invalid",
        "timestamp_sec must be an unclassed base numeric vector.",
        table = "timestamp_spine", row = i, column = "timestamp_sec"
      )
    }
    attributes(value) <- NULL
    value <- as.double(value)
    n_spikes[[i]] <- length(value)
    total <- total + length(value)
    if (total > limits[["max_total_timestamps"]]) {
      stpd_provider_b1_abort(
        "resource_limit_exceeded", "Timestamp count exceeds the B1 limit.",
        table = "timestamp_spine"
      )
    }
    leaf_hash[[i]] <- stpd_provider_train_timestamp_sha256_v1(value)
    timestamps[[i]] <- value
  }
  manifest <- data.frame(
    protocol_version = rep(STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION,
                           length(selected_trains)),
    coordinate_version = rep(STPD_PROVIDER_COORDINATE_VERSION,
                             length(selected_trains)),
    train_key = selected_trains,
    n_spikes = as.integer(n_spikes),
    timestamp_unit = rep("seconds", length(selected_trains)),
    train_timestamp_sha256 = leaf_hash,
    stringsAsFactors = FALSE
  )
  timestamp_spine_sha256 <- stpd_provider_hash_domain(
    "stpd-dataset-timestamp-spine-v1",
    lapply(seq_len(nrow(manifest)), function(i) {
      as.list(manifest[i, , drop = FALSE])
    })
  )
  acquisition_context_sha256 <- stpd_provider_b1_optional_sha(
    acquisition_context_sha256, "stpd-provider-acquisition-context-none-v1",
    "acquisition_context_sha256"
  )
  qc_policy_sha256 <- stpd_provider_b1_optional_sha(
    qc_policy_sha256, "stpd-provider-qc-policy-none-v1", "qc_policy_sha256"
  )
  protocol_sha256 <- stpd_provider_adapter_protocol_hash_v1()
  dataset_snapshot_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-dataset-snapshot-v1",
    list(
      protocol_sha256 = protocol_sha256,
      timestamp_spine_sha256 = timestamp_spine_sha256,
      acquisition_context_sha256 = acquisition_context_sha256,
      qc_policy_sha256 = qc_policy_sha256
    )
  )
  structure(
    list(
      protocol_version = STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION,
      protocol_sha256 = protocol_sha256,
      coordinate_version = STPD_PROVIDER_COORDINATE_VERSION,
      dataset_snapshot_sha256 = dataset_snapshot_sha256,
      timestamp_spine_sha256 = timestamp_spine_sha256,
      acquisition_context_sha256 = acquisition_context_sha256,
      qc_policy_sha256 = qc_policy_sha256,
      train_manifest = manifest,
      timestamps = timestamps
    ),
    class = "stpd_provider_spine_v1"
  )
}

stpd_provider_b1_plain_spine <- function(spine) {
  expected_fields <- c(
    "protocol_version", "protocol_sha256", "coordinate_version",
    "dataset_snapshot_sha256", "timestamp_spine_sha256",
    "acquisition_context_sha256", "qc_policy_sha256", "train_manifest",
    "timestamps"
  )
  if (typeof(spine) != "list" ||
      !identical(attr(spine, "class", exact = TRUE),
                 "stpd_provider_spine_v1") ||
      !identical(attr(spine, "names", exact = TRUE), expected_fields)) {
    stpd_provider_b1_abort(
      "type_invalid", "A structurally verified provider spine is required.",
      table = "timestamp_spine"
    )
  }
  out <- unclass(spine)
  attributes(out) <- list(names = expected_fields)
  if (!identical(.subset2(out, 1L), STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION) ||
      !identical(.subset2(out, 3L), STPD_PROVIDER_COORDINATE_VERSION)) {
    stpd_provider_b1_abort(
      "hash_mismatch", "Provider spine protocol or coordinate version differs.",
      table = "timestamp_spine"
    )
  }
  hash_fields <- c(2L, 4L, 5L, 6L, 7L)
  if (any(!vapply(out[hash_fields], stpd_provider_b1_sha_ok, logical(1)))) {
    stpd_provider_b1_abort(
      "sha256_invalid", "Provider spine contains an invalid identity hash.",
      table = "timestamp_spine"
    )
  }
  if (!identical(.subset2(out, 2L),
                 stpd_provider_adapter_protocol_hash_v1())) {
    stpd_provider_b1_abort(
      "hash_mismatch", "Provider spine protocol hash differs.",
      table = "timestamp_spine", column = "protocol_sha256"
    )
  }

  manifest <- .subset2(out, 8L)
  manifest_fields <- c(
    "protocol_version", "coordinate_version", "train_key", "n_spikes",
    "timestamp_unit", "train_timestamp_sha256"
  )
  if (typeof(manifest) != "list" ||
      !identical(attr(manifest, "class", exact = TRUE), "data.frame") ||
      !identical(attr(manifest, "names", exact = TRUE), manifest_fields)) {
    stpd_provider_b1_abort(
      "type_invalid", "Provider spine manifest has an invalid base schema.",
      table = "timestamp_spine"
    )
  }
  manifest_raw <- unclass(manifest)
  attributes(manifest_raw) <- list(names = manifest_fields)
  column_lengths <- vapply(seq_along(manifest_raw), function(i) {
    value <- .subset2(manifest_raw, i)
    if (!is.null(attr(value, "class", exact = TRUE)) ||
        !is.null(attr(value, "dim", exact = TRUE))) {
      stpd_provider_b1_abort(
        "type_invalid", "Provider spine manifest columns must be unclassed.",
        table = "timestamp_spine", column = manifest_fields[[i]]
      )
    }
    length(value)
  }, integer(1))
  if (!length(column_lengths) || !all(column_lengths == column_lengths[[1L]]) ||
      column_lengths[[1L]] < 1L) {
    stpd_provider_b1_abort(
      "type_invalid", "Provider spine manifest must contain one or more trains.",
      table = "timestamp_spine"
    )
  }
  train_count <- column_lengths[[1L]]
  row_names <- attr(manifest, "row.names", exact = TRUE)
  if (!typeof(row_names) %in% c("integer", "character") ||
      is.object(row_names) || length(row_names) != train_count) {
    stpd_provider_b1_abort(
      "type_invalid", "Provider spine manifest row identity is invalid.",
      table = "timestamp_spine"
    )
  }
  protocol_values <- .subset2(manifest_raw, 1L)
  coordinate_values <- .subset2(manifest_raw, 2L)
  train_keys <- .subset2(manifest_raw, 3L)
  n_spikes <- .subset2(manifest_raw, 4L)
  timestamp_units <- .subset2(manifest_raw, 5L)
  leaf_hashes <- .subset2(manifest_raw, 6L)
  if (!is.character(protocol_values) || anyNA(protocol_values) ||
      !all(protocol_values == STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION) ||
      !is.character(coordinate_values) || anyNA(coordinate_values) ||
      !all(coordinate_values == STPD_PROVIDER_COORDINATE_VERSION) ||
      !is.character(train_keys) || anyNA(train_keys) || anyDuplicated(train_keys) ||
      !identical(typeof(n_spikes), "integer") || anyNA(n_spikes) ||
      any(n_spikes < 0L) || !is.character(timestamp_units) ||
      anyNA(timestamp_units) ||
      !all(timestamp_units == "seconds") || !is.character(leaf_hashes) ||
      any(!vapply(leaf_hashes, stpd_provider_b1_sha_ok, logical(1)))) {
    stpd_provider_b1_abort(
      "type_invalid", "Provider spine manifest values violate the protocol.",
      table = "timestamp_spine"
    )
  }
  checked_keys <- unname(vapply(
    train_keys, stpd_provider_b1_utf8_scalar, character(1), context = "train_key"
  ))
  if (!identical(checked_keys, train_keys) ||
      !identical(stpd_provider_b1_utf8_sort_index(train_keys),
                 seq_along(train_keys))) {
    stpd_provider_b1_abort(
      "type_invalid", "Provider spine train order is not canonical.",
      table = "timestamp_spine", column = "train_key"
    )
  }

  timestamps <- .subset2(out, 9L)
  if (typeof(timestamps) != "list" ||
      !is.null(attr(timestamps, "class", exact = TRUE)) ||
      !identical(attr(timestamps, "names", exact = TRUE), train_keys) ||
      length(timestamps) != train_count) {
    stpd_provider_b1_abort(
      "type_invalid", "Provider spine timestamp leaves are invalid.",
      table = "timestamp_spine"
    )
  }
  timestamp_raw <- unclass(timestamps)
  attributes(timestamp_raw) <- list(names = train_keys)
  limits <- stpd_provider_adapter_protocol_v1()$resource_limits
  declared_total_timestamps <- sum(as.double(n_spikes))
  if (train_count > limits[["max_trains"]] ||
      declared_total_timestamps > limits[["max_total_timestamps"]]) {
    stpd_provider_b1_abort(
      "resource_limit_exceeded", "Provider spine exceeds B1 resource limits.",
      table = "timestamp_spine"
    )
  }
  computed_leaf_hashes <- character(train_count)
  for (i in seq_len(train_count)) {
    value <- .subset2(timestamp_raw, i)
    if (!typeof(value) %in% c("integer", "double") || is.object(value) ||
        !is.null(attributes(value))) {
      stpd_provider_b1_abort(
        "type_invalid", "Provider spine timestamp leaves must be plain numeric.",
        table = "timestamp_spine", row = i, column = "timestamp_sec"
      )
    }
    if (length(value) != n_spikes[[i]]) {
      stpd_provider_b1_abort(
        "train_timestamp_hash_mismatch", "Timestamp count differs from manifest.",
        table = "timestamp_spine", row = i, column = "n_spikes"
      )
    }
    value <- as.double(value)
    computed_leaf_hashes[[i]] <- stpd_provider_train_timestamp_sha256_v1(value)
    timestamp_raw[[i]] <- value
  }
  if (!identical(computed_leaf_hashes, leaf_hashes)) {
    stpd_provider_b1_abort(
      "train_timestamp_hash_mismatch", "A timestamp leaf hash differs.",
      table = "timestamp_spine", column = "train_timestamp_sha256"
    )
  }
  manifest_rows <- lapply(seq_len(train_count), function(i) {
    list(
      protocol_version = protocol_values[[i]],
      coordinate_version = coordinate_values[[i]],
      train_key = train_keys[[i]], n_spikes = n_spikes[[i]],
      timestamp_unit = timestamp_units[[i]],
      train_timestamp_sha256 = leaf_hashes[[i]]
    )
  })
  timestamp_spine_sha256 <- stpd_provider_hash_domain(
    "stpd-dataset-timestamp-spine-v1", manifest_rows
  )
  if (!identical(timestamp_spine_sha256, .subset2(out, 5L))) {
    stpd_provider_b1_abort(
      "train_timestamp_hash_mismatch", "Timestamp spine hash differs.",
      table = "timestamp_spine", column = "timestamp_spine_sha256"
    )
  }
  dataset_snapshot_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-dataset-snapshot-v1",
    list(
      protocol_sha256 = .subset2(out, 2L),
      timestamp_spine_sha256 = .subset2(out, 5L),
      acquisition_context_sha256 = .subset2(out, 6L),
      qc_policy_sha256 = .subset2(out, 7L)
    )
  )
  if (!identical(dataset_snapshot_sha256, .subset2(out, 4L))) {
    stpd_provider_b1_abort(
      "dataset_snapshot_mismatch", "Provider dataset snapshot hash differs.",
      table = "timestamp_spine", column = "dataset_snapshot_sha256"
    )
  }
  manifest_clean <- structure(
    manifest_raw, class = "data.frame", row.names = seq_len(train_count)
  )
  out$train_manifest <- manifest_clean
  out$timestamps <- timestamp_raw
  out
}

stpd_provider_verify_run_context_v1 <- function(run_row, spine) {
  spine <- stpd_provider_b1_plain_spine(spine)
  if (identical(attr(run_row, "class", exact = TRUE), "data.frame")) {
    row <- stpd_provider_b1_one_row_data_frame(run_row, "provider_runs")
  } else if (!is.null(attr(run_row, "class", exact = TRUE))) {
    stpd_provider_b1_abort(
      "executable_import_forbidden", "Classed provider-run rows are forbidden.",
      table = "provider_runs"
    )
  } else {
    row <- stpd_provider_b1_raw_named_list(run_row, "provider_runs")
  }
  required <- c("provider_run_id", "dataset_snapshot_sha256",
                "timestamp_spine_sha256", "contract_sha256")
  if (!all(required %in% names(row))) {
    stpd_provider_b1_abort(
      "required_field_missing", "Run context is missing identity fields.",
      table = "provider_runs"
    )
  }
  if (!stpd_provider_b1_sha_ok(row$provider_run_id)) {
    stpd_provider_b1_abort(
      "sha256_invalid", "provider_run_id must be lowercase SHA-256.",
      table = "provider_runs", column = "provider_run_id"
    )
  }
  if (!identical(row$contract_sha256, stpd_provider_contract_hash())) {
    stpd_provider_b1_abort(
      "hash_mismatch", "Run uses a different provider contract.",
      table = "provider_runs", column = "contract_sha256",
      provider_run_id = row$provider_run_id
    )
  }
  if (!identical(row$timestamp_spine_sha256, spine$timestamp_spine_sha256)) {
    stpd_provider_b1_abort(
      "train_timestamp_hash_mismatch", "Run timestamp spine does not match data.",
      table = "provider_runs", column = "timestamp_spine_sha256",
      provider_run_id = row$provider_run_id
    )
  }
  if (!identical(row$dataset_snapshot_sha256, spine$dataset_snapshot_sha256)) {
    stpd_provider_b1_abort(
      "dataset_snapshot_mismatch", "Run dataset snapshot does not match data.",
      table = "provider_runs", column = "dataset_snapshot_sha256",
      provider_run_id = row$provider_run_id
    )
  }
  invisible(TRUE)
}

stpd_provider_source_record_sha256_v1 <- function(source_record) {
  record <- stpd_provider_b1_plain_source_record(source_record)
  fields <- lapply(seq_along(record), function(i) {
    name <- names(record)[[i]]
    value <- record[[i]]
    if (is.null(value)) {
      return(list(name = name, type = "null", is_na = FALSE, value = NULL))
    }
    if (is.factor(value) || is.object(value) || is.list(value) ||
        length(value) != 1L || is.raw(value)) {
      stpd_provider_b1_abort(
        "executable_import_forbidden",
        "Source records must contain only scalar pure-data fields.",
        table = "source_record", column = name
      )
    }
    type <- typeof(value)
    if (!type %in% c("logical", "integer", "double", "character")) {
      stpd_provider_b1_abort(
        "executable_import_forbidden", "Unsupported source-record field type.",
        table = "source_record", column = name, value = type
      )
    }
    is_na <- length(value) == 1L && is.na(value)
    if (type == "double" && !is_na &&
        (!is.finite(value) || stpd_provider_b1_negative_zero(value))) {
      stpd_provider_b1_abort(
        "coordinate_non_finite", "Source-record doubles must be finite.",
        table = "source_record", column = name
      )
    }
    if (type == "character" && !is_na) {
      value <- stpd_provider_b1_utf8_scalar(
        value, paste0("source field ", name), allow_empty = TRUE
      )
    }
    list(name = name, type = type, is_na = is_na,
         value = if (is_na) NULL else unname(value))
  })
  stpd_provider_hash_domain(
    "stpd-provider-source-record-v1",
    list(fields = unname(fields))
  )
}

stpd_provider_b1_coordinate_fields <- function() {
  c(
    "source_record_key", "train_key", "source_coordinate_convention",
    "source_index_base", "source_interval_closure", "source_time_unit",
    "source_start_index", "source_end_index", "source_start_time",
    "source_end_time", "transformation"
  )
}

stpd_provider_b1_is_null_numeric <- function(x) {
  is.null(x) || (
    typeof(x) %in% c("integer", "double") && !is.object(x) &&
      length(x) == 1L && is.na(x)
  )
}

stpd_provider_b1_coordinate_spec <- function(
    spec, registry = stpd_provider_contract_registry()) {
  if (identical(attr(spec, "class", exact = TRUE), "data.frame")) {
    spec <- stpd_provider_b1_one_row_data_frame(spec, "coordinate_spec")
  } else if (!is.null(attr(spec, "class", exact = TRUE))) {
    stpd_provider_b1_abort(
      "executable_import_forbidden", "Classed coordinate specs are forbidden.",
      table = "coordinate_spec"
    )
  }
  spec <- stpd_provider_b1_raw_named_list(spec, "Coordinate spec")
  fields <- stpd_provider_b1_coordinate_fields()
  if (!identical(sort(names(spec), method = "radix"),
                 sort(fields, method = "radix"))) {
    code <- if (length(setdiff(fields, names(spec)))) {
      "required_field_missing"
    } else {
      "unknown_column"
    }
    stpd_provider_b1_abort(
      code, "Coordinate spec fields must match the frozen B1 schema exactly."
    )
  }
  spec <- spec[fields]
  spec$source_record_key <- stpd_provider_b1_utf8_scalar(
    spec$source_record_key, "source_record_key"
  )
  spec$train_key <- stpd_provider_b1_utf8_scalar(spec$train_key, "train_key")
  declaration <- fields[3:6]
  spec[declaration] <- lapply(declaration, function(name) {
    stpd_provider_b1_utf8_scalar(spec[[name]], name)
  })
  spec$transformation <- stpd_provider_b1_utf8_scalar(
    spec$transformation, "transformation"
  )
  contract <- registry$coordinate_source_contract
  hit <- contract$source_coordinate_convention ==
    spec$source_coordinate_convention &
    contract$source_index_base == spec$source_index_base &
    contract$transformation == spec$transformation
  if (sum(hit) != 1L) {
    stpd_provider_b1_abort(
      "coordinate_convention_missing",
      "Coordinate convention/base/transformation is not allowlisted.",
      source_record_key = spec$source_record_key
    )
  }
  contract <- contract[which(hit), , drop = FALSE]
  allowed_closure <- strsplit(contract$allowed_closures, "|", fixed = TRUE)[[1L]]
  if (!(spec$source_interval_closure %in% allowed_closure)) {
    stpd_provider_b1_abort(
      "interval_closure_missing", "Interval closure is not authorized.",
      source_record_key = spec$source_record_key
    )
  }
  allowed_unit <- strsplit(contract$allowed_time_units, "|", fixed = TRUE)[[1L]]
  if (!(spec$source_time_unit %in% allowed_unit)) {
    stpd_provider_b1_abort(
      "time_unit_unsupported", "Time unit is not authorized.",
      source_record_key = spec$source_record_key
    )
  }
  source_is_index <- identical(contract$source_field_group, "index")
  closure_adjustment <- NULL
  if (source_is_index) {
    if (any(!vapply(spec[c("source_start_time", "source_end_time")],
                    stpd_provider_b1_is_null_numeric, logical(1)))) {
      stpd_provider_b1_abort(
        "coordinate_roundtrip_mismatch",
        "Index coordinates cannot carry source time fields.",
        source_record_key = spec$source_record_key
      )
    }
    adjustment <- registry$integer_closure_adjustments
    adjustment_hit <- adjustment$source_interval_closure ==
      spec$source_interval_closure
    if (sum(adjustment_hit) != 1L) {
      stpd_provider_b1_abort(
        "interval_closure_missing", "Integer closure adjustment is not frozen.",
        source_record_key = spec$source_record_key
      )
    }
    closure_adjustment <- adjustment[which(adjustment_hit), , drop = FALSE]
  } else {
    if (any(!vapply(spec[c("source_start_index", "source_end_index")],
                    stpd_provider_b1_is_null_numeric, logical(1)))) {
      stpd_provider_b1_abort(
        "coordinate_roundtrip_mismatch",
        "Time coordinates cannot carry source index fields.",
        source_record_key = spec$source_record_key
      )
    }
  }
  structure(
    list(
      spec = spec, contract = contract, source_is_index = source_is_index,
      closure_adjustment = closure_adjustment
    ),
    class = "stpd_provider_coordinate_spec_v1"
  )
}

stpd_provider_b1_scalar_number <- function(x) {
  if (is.null(x) || !typeof(x) %in% c("integer", "double") || is.object(x) ||
      length(x) != 1L) {
    return(list(status = "type_invalid", value = NA_real_))
  }
  value <- as.double(x)
  if (!is.finite(value) || stpd_provider_b1_negative_zero(value)) {
    return(list(status = "coordinate_non_finite", value = NA_real_))
  }
  list(status = "ok", value = value)
}

stpd_provider_b1_rejected_audit <- function(
    provider_run_id, spec, source_record_sha256, tolerance_sec, code,
    ambiguity = FALSE, source_index = c(NA_integer_, NA_integer_),
    source_time = c(NA_real_, NA_real_)) {
  row <- stpd_provider_normalization_audit_prototype()
  row[1L, ] <- list(
    STPD_PROVIDER_BUNDLE_VERSION, NA_character_, provider_run_id,
    spec$source_record_key, source_record_sha256, spec$train_key,
    spec$source_coordinate_convention, spec$source_index_base,
    spec$source_interval_closure, spec$source_time_unit,
    as.integer(source_index[[1L]]), as.integer(source_index[[2L]]),
    as.double(source_time[[1L]]), as.double(source_time[[2L]]),
    spec$transformation, as.double(tolerance_sec),
    NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_real_, NA_real_,
    as.logical(ambiguity), "rejected", as.character(code),
    paste0("B1 normalization rejected: ", code)
  )
  row$normalization_audit_id <- stpd_provider_normalization_audit_id_v1(row)
  row
}

stpd_provider_b1_success_audit <- function(
    provider_run_id, spec, source_record_sha256, tolerance_sec,
    source_index, source_time, geometry) {
  row <- stpd_provider_normalization_audit_prototype()
  row[1L, ] <- list(
    STPD_PROVIDER_BUNDLE_VERSION, NA_character_, provider_run_id,
    spec$source_record_key, source_record_sha256, spec$train_key,
    spec$source_coordinate_convention, spec$source_index_base,
    spec$source_interval_closure, spec$source_time_unit,
    as.integer(source_index[[1L]]), as.integer(source_index[[2L]]),
    as.double(source_time[[1L]]), as.double(source_time[[2L]]),
    spec$transformation, as.double(tolerance_sec),
    as.integer(geometry$start_isi), as.integer(geometry$end_isi),
    as.integer(geometry$start_spike), as.integer(geometry$end_spike),
    as.double(geometry$start_time_sec), as.double(geometry$end_time_sec),
    FALSE, "normalized", NA_character_, NA_character_
  )
  row$normalization_audit_id <- stpd_provider_normalization_audit_id_v1(row)
  row
}

stpd_provider_b1_time_matches <- function(timestamps, target, tolerance) {
  left <- findInterval(target - tolerance, timestamps, left.open = TRUE) + 1L
  right <- findInterval(target + tolerance, timestamps)
  count <- max(0L, right - left + 1L)
  list(count = count, index = if (count == 1L) left else NA_integer_)
}

stpd_provider_b1_local_gap <- function(timestamps, index) {
  gaps <- numeric()
  if (index > 1L) gaps <- c(gaps, timestamps[[index]] - timestamps[[index - 1L]])
  if (index < length(timestamps)) {
    gaps <- c(gaps, timestamps[[index + 1L]] - timestamps[[index]])
  }
  if (length(gaps)) min(gaps) else Inf
}

stpd_provider_normalize_one_v1 <- function(
    source_record, coordinate_spec, spine, provider_run_id,
    source_time_resolution_sec = 0, train_index = NULL,
    .spine_prevalidated = FALSE, .coordinate_spec_prevalidated = FALSE) {
  if (!isTRUE(.spine_prevalidated)) {
    spine <- stpd_provider_b1_plain_spine(spine)
  }
  if (!stpd_provider_b1_sha_ok(provider_run_id)) {
    stpd_provider_b1_abort(
      "sha256_invalid", "provider_run_id must be lowercase SHA-256.",
      column = "provider_run_id", value = provider_run_id
    )
  }
  source_time_resolution_sec <- stpd_provider_b1_time_resolution(
    source_time_resolution_sec
  )
  parsed <- if (isTRUE(.coordinate_spec_prevalidated) &&
                identical(attr(coordinate_spec, "class", exact = TRUE),
                          "stpd_provider_coordinate_spec_v1")) {
    coordinate_spec
  } else {
    stpd_provider_b1_coordinate_spec(coordinate_spec)
  }
  spec <- parsed$spec
  source_record_sha256 <- stpd_provider_source_record_sha256_v1(source_record)
  tolerance_sec <- if (parsed$source_is_index) 0 else source_time_resolution_sec / 2
  source_index <- c(NA_integer_, NA_integer_)
  source_time <- c(NA_real_, NA_real_)
  train_hit <- if (is.null(train_index)) {
    match(spec$train_key, spine$train_manifest$train_key)
  } else {
    parsed_train_index <- stpd_provider_b1_scalar_number(train_index)
    if (!identical(parsed_train_index$status, "ok") ||
        parsed_train_index$value != trunc(parsed_train_index$value) ||
        parsed_train_index$value < 1 ||
        parsed_train_index$value > .Machine$integer.max) {
      stpd_provider_b1_abort(
        "train_timestamp_hash_mismatch", "Precomputed train index is invalid.",
        source_record_key = spec$source_record_key
      )
    }
    as.integer(parsed_train_index$value)
  }
  if (!is.na(train_hit) &&
      (train_hit < 1L || train_hit > nrow(spine$train_manifest) ||
       !identical(spine$train_manifest$train_key[[train_hit]], spec$train_key))) {
    stpd_provider_b1_abort(
      "train_timestamp_hash_mismatch", "Precomputed train index is invalid.",
      source_record_key = spec$source_record_key
    )
  }
  if (is.na(train_hit)) {
    audit <- stpd_provider_b1_rejected_audit(
      provider_run_id, spec, source_record_sha256, tolerance_sec,
      "train_not_found"
    )
    return(list(audit = audit, geometry = NULL))
  }
  timestamps <- spine$timestamps[[train_hit]]
  train_hash <- spine$train_manifest$train_timestamp_sha256[[train_hit]]
  n <- length(timestamps)
  if (parsed$source_is_index) {
    parsed_index <- list(
      stpd_provider_b1_scalar_number(spec$source_start_index),
      stpd_provider_b1_scalar_number(spec$source_end_index)
    )
    status <- vapply(parsed_index, `[[`, character(1), "status")
    if (any(status == "type_invalid")) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0, "type_invalid"
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (any(status != "ok")) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_non_finite"
      )
      return(list(audit = audit, geometry = NULL))
    }
    raw_index <- vapply(parsed_index, `[[`, double(1), "value")
    if (any(raw_index != trunc(raw_index)) ||
        any(raw_index < -.Machine$integer.max) ||
        any(raw_index > .Machine$integer.max)) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_non_integer"
      )
      return(list(audit = audit, geometry = NULL))
    }
    source_index <- as.integer(raw_index)
    lower <- if (spec$source_index_base == "zero") 0L else 1L
    if (source_index[[1L]] < lower || source_index[[2L]] < lower) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_out_of_range", source_index = source_index
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (source_index[[2L]] < source_index[[1L]]) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_reversed", source_index = source_index
      )
      return(list(audit = audit, geometry = NULL))
    }
    adjustment <- parsed$closure_adjustment
    closed <- c(
      as.double(source_index[[1L]]) +
        as.double(adjustment$start_adjustment[[1L]]),
      as.double(source_index[[2L]]) +
        as.double(adjustment$end_adjustment[[1L]])
    )
    if (closed[[2L]] < closed[[1L]]) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_empty", source_index = source_index
      )
      return(list(audit = audit, geometry = NULL))
    }
    canonical <- switch(
      spec$transformation,
      identity_train_row_one_based = closed,
      diff_one_based_plus_one = closed + 1,
      diff_zero_based_plus_two = closed + 2,
      spike_one_based_span_to_isi = c(closed[[1L]] + 1, closed[[2L]]),
      spike_zero_based_span_to_isi = c(closed[[1L]] + 2,
                                       closed[[2L]] + 1),
      per_isi_one_based_runs_plus_one = closed + 1,
      per_isi_zero_based_runs_plus_two = closed + 2,
      NULL
    )
    if (is.null(canonical) || canonical[[2L]] < canonical[[1L]]) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_empty", source_index = source_index
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (canonical[[1L]] < 2L || canonical[[2L]] > n) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, 0,
        "coordinate_out_of_range", source_index = source_index
      )
      return(list(audit = audit, geometry = NULL))
    }
    canonical <- as.integer(canonical)
  } else {
    parsed_time <- list(
      stpd_provider_b1_scalar_number(spec$source_start_time),
      stpd_provider_b1_scalar_number(spec$source_end_time)
    )
    status <- vapply(parsed_time, `[[`, character(1), "status")
    if (any(status == "type_invalid")) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "type_invalid"
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (any(status != "ok")) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "coordinate_non_finite"
      )
      return(list(audit = audit, geometry = NULL))
    }
    raw_time <- vapply(parsed_time, `[[`, double(1), "value")
    source_time <- as.double(raw_time)
    factor <- c(s = 1, ms = 1e-3, us = 1e-6)[[spec$source_time_unit]]
    seconds <- source_time * factor
    machine_tolerance <- stpd_provider_b1_conversion_tolerance(
      seconds, spec$source_time_unit
    )
    tolerance_sec <- max(tolerance_sec, machine_tolerance)
    if (seconds[[2L]] < seconds[[1L]]) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "coordinate_reversed", source_time = source_time
      )
      return(list(audit = audit, geometry = NULL))
    }
    start_match <- stpd_provider_b1_time_matches(
      timestamps, seconds[[1L]], tolerance_sec
    )
    end_match <- stpd_provider_b1_time_matches(
      timestamps, seconds[[2L]], tolerance_sec
    )
    if (start_match$count > 1L || end_match$count > 1L) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "time_alignment_ambiguous", ambiguity = TRUE, source_time = source_time
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (start_match$count == 0L || end_match$count == 0L) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "time_alignment_missing", source_time = source_time
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (tolerance_sec > 0 &&
        (tolerance_sec >= 0.5 * stpd_provider_b1_local_gap(
          timestamps, start_match$index
        ) || tolerance_sec >= 0.5 * stpd_provider_b1_local_gap(
          timestamps, end_match$index
        ))) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "time_tolerance_invalid", source_time = source_time
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (end_match$index < start_match$index) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "coordinate_reversed", source_time = source_time
      )
      return(list(audit = audit, geometry = NULL))
    }
    if (end_match$index == start_match$index) {
      audit <- stpd_provider_b1_rejected_audit(
        provider_run_id, spec, source_record_sha256, tolerance_sec,
        "coordinate_empty", source_time = source_time
      )
      return(list(audit = audit, geometry = NULL))
    }
    canonical <- c(start_match$index + 1L, end_match$index)
  }
  geometry <- list(
    start_isi = as.integer(canonical[[1L]]),
    end_isi = as.integer(canonical[[2L]]),
    start_spike = as.integer(canonical[[1L]] - 1L),
    end_spike = as.integer(canonical[[2L]]),
    start_time_sec = as.double(timestamps[[canonical[[1L]] - 1L]]),
    end_time_sec = as.double(timestamps[[canonical[[2L]]]]),
    train_timestamp_sha256 = train_hash
  )
  audit <- stpd_provider_b1_success_audit(
    provider_run_id, spec, source_record_sha256, tolerance_sec,
    source_index, source_time, geometry
  )
  list(audit = audit, geometry = geometry)
}

stpd_provider_b1_empty_geometry <- function() {
  data.frame(
    source_record_key = character(), source_record_sha256 = character(),
    normalization_audit_id = character(), train_key = character(),
    train_timestamp_sha256 = character(), canonical_start_isi = integer(),
    canonical_end_isi = integer(), canonical_start_spike = integer(),
    canonical_end_spike = integer(), canonical_start_time_sec = double(),
    canonical_end_time_sec = double(), stringsAsFactors = FALSE
  )
}

stpd_provider_b1_coverage_sha256_v1 <- function(input_mode, coverage_manifest) {
  stpd_provider_hash_domain(
    "stpd-provider-coverage-manifest-v1",
    list(
      input_mode = input_mode,
      rows = lapply(seq_len(nrow(coverage_manifest)), function(i) {
        as.list(coverage_manifest[i, , drop = FALSE])
      })
    )
  )
}

stpd_provider_normalize_batch_v1 <- function(
    records, coordinate_specs, spine, provider_run_id,
    source_time_resolution_sec = 0, processed_trains,
    per_isi_coverage_trains = NULL) {
  spine <- stpd_provider_b1_plain_spine(spine)
  source_time_resolution_sec <- stpd_provider_b1_time_resolution(
    source_time_resolution_sec
  )
  if (typeof(records) != "list" ||
      !is.null(attr(records, "class", exact = TRUE)) ||
      typeof(coordinate_specs) != "list" ||
      !is.null(attr(coordinate_specs, "class", exact = TRUE)) ||
      length(records) != length(coordinate_specs)) {
    stpd_provider_b1_abort(
      "type_invalid", "records and coordinate_specs must be equal-length lists."
    )
  }
  limit <- stpd_provider_adapter_protocol_v1()$resource_limits[[
    "max_source_records"
  ]]
  if (length(records) > limit) {
    stpd_provider_b1_abort(
      "resource_limit_exceeded", "Source-record count exceeds the B1 limit."
    )
  }
  plain_records <- lapply(records, stpd_provider_b1_plain_source_record)
  total_source_fields <- sum(vapply(plain_records, length, double(1)))
  if (total_source_fields > STPD_PROVIDER_B1_MAX_TOTAL_SOURCE_FIELDS) {
    stpd_provider_b1_abort(
      "resource_limit_exceeded", "Total source-record fields exceed the B1 limit."
    )
  }
  if (!stpd_provider_b1_sha_ok(provider_run_id)) {
    stpd_provider_b1_abort(
      "sha256_invalid", "provider_run_id must be lowercase SHA-256."
    )
  }
  if (missing(processed_trains) || !is.character(processed_trains) ||
      is.object(processed_trains) ||
      !length(processed_trains) || anyNA(processed_trains) ||
      anyDuplicated(processed_trains)) {
    stpd_provider_b1_abort(
      "coordinate_roundtrip_mismatch",
      "A batch requires an explicit non-empty processed-train coverage manifest."
    )
  }
  processed_trains <- unname(vapply(
    processed_trains, stpd_provider_b1_utf8_scalar, character(1),
    context = "processed_train"
  ))
  unknown_processed <- setdiff(
    processed_trains, spine$train_manifest$train_key
  )
  if (length(unknown_processed)) {
    stpd_provider_b1_abort(
      "train_not_found", "The processed-train manifest names an unknown train.",
      value = unknown_processed[[1L]]
    )
  }
  if (!setequal(processed_trains, spine$train_manifest$train_key)) {
    stpd_provider_b1_abort(
      "coordinate_roundtrip_mismatch",
      paste(
        "The processed-train manifest must equal the selected timestamp spine;",
        "build a new subset spine for a subset run."
      )
    )
  }
  registry <- stpd_provider_contract_registry()
  parsed_specs <- lapply(
    coordinate_specs, stpd_provider_b1_coordinate_spec, registry = registry
  )
  source_keys <- vapply(parsed_specs, function(x) x$spec$source_record_key,
                        character(1))
  if (anyDuplicated(source_keys)) {
    stpd_provider_b1_abort(
      "primary_key_duplicate", "source_record_key must be unique within a run.",
      column = "source_record_key"
    )
  }
  record_train_vector <- vapply(parsed_specs, function(x) {
    x$spec$train_key
  }, character(1))
  record_trains <- unique(record_train_vector)
  if (length(setdiff(record_trains, processed_trains))) {
    stpd_provider_b1_abort(
      "train_not_found",
      "A source record is outside the processed-train coverage manifest."
    )
  }
  conventions <- vapply(parsed_specs, function(x) {
    x$spec$source_coordinate_convention
  }, character(1))
  is_per_isi <- conventions == "per_isi_diff_index"
  per_isi_mode <- any(is_per_isi) || !is.null(per_isi_coverage_trains)
  if (per_isi_mode) {
    if (length(is_per_isi) && !all(is_per_isi)) {
      stpd_provider_b1_abort(
        "coordinate_roundtrip_mismatch",
        "A per-ISI run cannot mix interval coordinate conventions."
      )
    }
    if (is.null(per_isi_coverage_trains) ||
        !is.character(per_isi_coverage_trains) ||
        is.object(per_isi_coverage_trains) ||
        anyNA(per_isi_coverage_trains) ||
        anyDuplicated(per_isi_coverage_trains)) {
      stpd_provider_b1_abort(
        "coordinate_roundtrip_mismatch",
        "A per-ISI run requires an explicit unique coverage-train manifest."
      )
    }
    per_isi_coverage_trains <- unname(vapply(
      per_isi_coverage_trains, stpd_provider_b1_utf8_scalar, character(1),
      context = "per_isi_coverage_train"
    ))
    if (!setequal(per_isi_coverage_trains, processed_trains)) {
      stpd_provider_b1_abort(
        "coordinate_roundtrip_mismatch",
        "Per-ISI and processed-train coverage manifests disagree."
      )
    }
    bases <- unique(vapply(parsed_specs, function(x) {
      x$spec$source_index_base
    }, character(1)))
    transforms <- unique(vapply(parsed_specs, function(x) {
      x$spec$transformation
    }, character(1)))
    closures <- unique(vapply(parsed_specs, function(x) {
      x$spec$source_interval_closure
    }, character(1)))
    if (length(parsed_specs) &&
        (length(bases) != 1L || length(transforms) != 1L ||
         !identical(closures, "closed"))) {
      stpd_provider_b1_abort(
        "coordinate_roundtrip_mismatch",
        "Per-ISI input requires one base/transformation and closed singleton rows."
      )
    }
    if (!length(parsed_specs)) {
      coverage_rows <- match(
        per_isi_coverage_trains, spine$train_manifest$train_key
      )
      if (any(spine$train_manifest$n_spikes[coverage_rows] >= 2L)) {
        stpd_provider_b1_abort(
          "coordinate_roundtrip_mismatch",
          "An empty per-ISI input cannot cover a train that contains ISIs."
        )
      }
    }
    coverage_order <- stpd_provider_b1_utf8_sort_index(per_isi_coverage_trains)
    per_isi_train_hits <- match(
      per_isi_coverage_trains, spine$train_manifest$train_key
    )
    per_isi_processed_hits <- match(per_isi_coverage_trains, processed_trains)
    record_processed_hits <- match(record_train_vector, processed_trains)
    rows_by_processed_train <- split(
      seq_along(record_train_vector),
      factor(record_processed_hits, levels = seq_along(processed_trains))
    )
    for (coverage_position in coverage_order) {
      train <- per_isi_coverage_trains[[coverage_position]]
      train_hit <- per_isi_train_hits[[coverage_position]]
      if (is.na(train_hit)) {
        stpd_provider_b1_abort(
          "train_not_found", "Per-ISI coverage names an unknown train.",
          value = train
        )
      }
      rows <- rows_by_processed_train[[
        per_isi_processed_hits[[coverage_position]]
      ]]
      parsed_start <- lapply(rows, function(i) {
        stpd_provider_b1_scalar_number(parsed_specs[[i]]$spec$source_start_index)
      })
      parsed_end <- lapply(rows, function(i) {
        stpd_provider_b1_scalar_number(parsed_specs[[i]]$spec$source_end_index)
      })
      start_status <- vapply(parsed_start, `[[`, character(1), "status")
      end_status <- vapply(parsed_end, `[[`, character(1), "status")
      start <- vapply(parsed_start, `[[`, double(1), "value")
      end <- vapply(parsed_end, `[[`, double(1), "value")
      n_spikes <- spine$train_manifest$n_spikes[[train_hit]]
      expected <- if (n_spikes < 2L) integer() else if (bases == "one") {
        seq_len(n_spikes - 1L)
      } else {
        seq.int(0L, n_spikes - 2L)
      }
      observed <- if (length(start) && all(start_status == "ok") &&
                      all(end_status == "ok") && all(is.finite(start)) &&
                      all(start == trunc(start)) && all(start == end)) {
        sort(as.integer(start), method = "radix")
      } else {
        integer()
      }
      if (!identical(observed, as.integer(expected))) {
        stpd_provider_b1_abort(
          "coordinate_roundtrip_mismatch",
          "Per-ISI positions must be singleton, unique, and cover exactly n-1 elements.",
          value = train
        )
      }
    }
  } else if (!is.null(per_isi_coverage_trains)) {
    stpd_provider_b1_abort(
      "coordinate_roundtrip_mismatch",
      "per_isi_coverage_trains is only valid for a per-ISI run."
    )
  }
  coverage_order <- stpd_provider_b1_utf8_sort_index(processed_trains)
  coverage_manifest <- spine$train_manifest[
    match(processed_trains[coverage_order], spine$train_manifest$train_key),
    c("train_key", "n_spikes", "train_timestamp_sha256"), drop = FALSE
  ]
  rownames(coverage_manifest) <- NULL
  input_mode <- if (per_isi_mode) "per_isi" else "interval"
  coverage_sha256 <- stpd_provider_b1_coverage_sha256_v1(
    input_mode, coverage_manifest
  )
  record_train_hits <- match(record_train_vector, spine$train_manifest$train_key)
  parts <- lapply(seq_along(records), function(i) {
    stpd_provider_normalize_one_v1(
      source_record = plain_records[[i]], coordinate_spec = parsed_specs[[i]],
      spine = spine, provider_run_id = provider_run_id,
      source_time_resolution_sec = source_time_resolution_sec,
      train_index = record_train_hits[[i]], .spine_prevalidated = TRUE,
      .coordinate_spec_prevalidated = TRUE
    )
  })
  audit <- dplyr::bind_rows(c(
    list(stpd_provider_bundle_prototypes()$normalization_audit),
    lapply(parts, `[[`, "audit")
  ))
  if (nrow(audit)) {
    audit <- audit[order(audit$normalization_audit_id, method = "radix"),
                   , drop = FALSE]
    rownames(audit) <- NULL
  }
  atomic_accept <- !nrow(audit) || all(audit$normalization_status == "normalized")
  geometry <- stpd_provider_b1_empty_geometry()
  if (atomic_accept && length(parts)) {
    geometry <- dplyr::bind_rows(lapply(seq_along(parts), function(i) {
      item <- parts[[i]]
      row <- item$audit
      data.frame(
        source_record_key = row$source_record_key,
        source_record_sha256 = row$source_record_sha256,
        normalization_audit_id = row$normalization_audit_id,
        train_key = row$train_key,
        train_timestamp_sha256 = item$geometry$train_timestamp_sha256,
        canonical_start_isi = item$geometry$start_isi,
        canonical_end_isi = item$geometry$end_isi,
        canonical_start_spike = item$geometry$start_spike,
        canonical_end_spike = item$geometry$end_spike,
        canonical_start_time_sec = item$geometry$start_time_sec,
        canonical_end_time_sec = item$geometry$end_time_sec,
        stringsAsFactors = FALSE
      )
    }))
    geometry <- geometry[order(geometry$normalization_audit_id, method = "radix"),
                         , drop = FALSE]
    rownames(geometry) <- NULL
  }
  structure(
    list(
      protocol_version = STPD_PROVIDER_ADAPTER_PROTOCOL_VERSION,
      protocol_sha256 = spine$protocol_sha256,
      timestamp_spine_sha256 = spine$timestamp_spine_sha256,
      dataset_snapshot_sha256 = spine$dataset_snapshot_sha256,
      input_mode = input_mode,
      coverage_sha256 = coverage_sha256,
      coverage_manifest = coverage_manifest,
      atomic_accept = atomic_accept,
      normalization_audit = audit,
      normalized_geometry = geometry
    ),
    class = "stpd_provider_normalization_batch_v1"
  )
}
