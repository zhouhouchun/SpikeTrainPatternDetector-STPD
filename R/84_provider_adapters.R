# Phase B2: allowlisted provider adapters ------------------------------------

# This file intentionally stops at provider normalization.  It neither invokes
# the multitrack composer nor reads review/reference annotations.

STPD_PROVIDER_IMPORT_REQUEST_VERSION <- "stpd_provider_import_request_v1"
STPD_EXTERNAL_INTERVAL_ARTIFACT_VERSION <- "stpd_external_interval_artifact_v1"
STPD_EXTERNAL_PER_ISI_ARTIFACT_VERSION <- "stpd_external_per_isi_artifact_v1"
STPD_PROVIDER_ADAPTER_DEPENDENCY_MANIFEST_VERSION <-
  "stpd_provider_adapter_dependency_manifest_v1"

STPD_PROVIDER_B2_MAX_TRAINS <- 1000L
STPD_PROVIDER_B2_MAX_TIMESTAMPS <- 2000000L
STPD_PROVIDER_B2_MAX_SOURCE_RECORDS <- 250000L
STPD_PROVIDER_B2_MAX_ARTIFACT_BYTES <- 64L * 1024L * 1024L
STPD_PROVIDER_B2_MAX_MISI_WINDOWS_PER_TRAIN <- 250000L
STPD_PROVIDER_B2_MAX_MISI_WINDOWS_TOTAL <- 1000000L
STPD_PROVIDER_B2_MAX_INTERNAL_OUTPUT_BYTES <- 128L * 1024L * 1024L

stpd_provider_b2_abort <- function(code, message, table = "provider_import",
                                   row = NA_integer_, column = NA_character_,
                                   provider_run_id = NA_character_,
                                   source_record_key = NA_character_,
                                   value = NULL) {
  stpd_provider_contract_abort(
    code = code, message = message, table = table, row = row,
    column = column, provider_run_id = provider_run_id,
    source_record_key = source_record_key, offending_value = value
  )
}

stpd_provider_b2_sha_ok <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[0-9a-f]{64}$", x)
}

stpd_provider_b2_utf8_scalar <- function(x, field, allow_empty = FALSE) {
  if (!is.character(x) || is.object(x) || length(x) != 1L || is.na(x) ||
      Encoding(x) == "bytes") {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " must be one unclassed character scalar."),
      column = field, value = typeof(x)
    )
  }
  y <- enc2utf8(x)
  byte_preserved <- identical(charToRaw(x), charToRaw(y))
  if (!byte_preserved || !isTRUE(stringi::stri_enc_isutf8(y)) ||
      !identical(stringi::stri_trans_nfc(y), y)) {
    stpd_provider_b2_abort(
      "type_invalid",
      paste0(field, " must already be valid NFC UTF-8; no rewrite is allowed."),
      column = field
    )
  }
  if (nchar(y, type = "bytes") > STPD_PROVIDER_B1_MAX_CHARACTER_BYTES) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded", paste0(field, " exceeds the byte limit."),
      column = field
    )
  }
  if (!allow_empty && !nzchar(y)) {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " cannot be empty."), column = field
    )
  }
  unname(y)
}

stpd_provider_b2_plain_named_list <- function(x, field, exact_names = NULL,
                                              require_order = TRUE) {
  if (typeof(x) != "list" || !is.null(attr(x, "class", exact = TRUE))) {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " must be a plain named list."),
      column = field
    )
  }
  if (!length(x) && is.null(names(x))) names(x) <- character()
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x))) {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " must be a plain named list."),
      column = field
    )
  }
  if (!is.null(exact_names)) {
    missing <- setdiff(exact_names, names(x))
    extra <- setdiff(names(x), exact_names)
    if (length(missing)) {
      stpd_provider_b2_abort(
        "required_field_missing", paste0(field, " is missing a field."),
        column = missing[[1L]]
      )
    }
    if (length(extra)) {
      stpd_provider_b2_abort(
        "unknown_column", paste0(field, " contains an unknown field."),
        column = extra[[1L]]
      )
    }
    if (require_order && !identical(names(x), exact_names)) {
      stpd_provider_b2_abort(
        "unknown_column", paste0(field, " fields must use the frozen order."),
        column = field
      )
    }
  }
  x
}

stpd_provider_b2_plain_parameters <- function(parameters, allowed, defaults) {
  parameters <- stpd_provider_b2_plain_named_list(
    parameters, "parameters", require_order = FALSE
  )
  extra <- setdiff(names(parameters), allowed)
  if (length(extra)) {
    stpd_provider_b2_abort(
      "unknown_column", "parameters contains an unsupported parameter.",
      column = extra[[1L]]
    )
  }
  out <- defaults
  for (name in names(parameters)) out[name] <- list(parameters[[name]])
  out
}

stpd_provider_b2_scalar_number <- function(x, field, lower = -Inf,
                                           upper = Inf, integer = FALSE) {
  if (!is.numeric(x) || is.object(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < lower || x > upper ||
      (integer && (x != trunc(x) || x < -.Machine$integer.max ||
                   x > .Machine$integer.max))) {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " is outside its numeric contract."),
      column = field, value = x
    )
  }
  if (integer) as.integer(x) else as.double(x)
}

stpd_provider_b2_scalar_logical <- function(x, field) {
  if (!is.logical(x) || is.object(x) || length(x) != 1L || is.na(x)) {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " must be one logical scalar."),
      column = field, value = x
    )
  }
  unname(x)
}

stpd_provider_b2_scalar_enum <- function(x, field, choices) {
  y <- stpd_provider_b2_utf8_scalar(x, field)
  if (!(y %in% choices)) {
    stpd_provider_b2_abort(
      "enum_invalid", paste0(field, " is not allowlisted."),
      column = field, value = y
    )
  }
  y
}

stpd_provider_b2_raw_json <- function(x) {
  jsonlite::toJSON(
    stpd_provider_canonicalize(x), auto_unbox = TRUE, null = "null",
    na = "null", digits = NA, pretty = FALSE
  )
}

stpd_provider_b2_raw_json_sha256 <- function(x) {
  digest::digest(
    charToRaw(enc2utf8(as.character(stpd_provider_b2_raw_json(x)))),
    algo = "sha256", serialize = FALSE
  )
}

stpd_provider_b2_source_sha256 <- function(x) {
  stpd_provider_source_record_sha256_v1(x)
}

stpd_provider_adapter_registry_v1 <- function() {
  adapters <- data.frame(
    adapter_key = c(
      "native_stpd_postcomposer_v1", "mean_isi_v1", "logisi_newbd_v1",
      "external_interval_v1", "external_per_isi_v1"
    ),
    adapter_version = rep("1.0.0", 5L),
    provider_key = c(
      "native_stpd", "mean_isi", "logisi_newbd",
      "external_data_import", "external_data_import"
    ),
    provider_kind = c(
      "native_stpd", "mean_isi", "logisi_newbd",
      "external_data_import", "external_data_import"
    ),
    capability_profile = c(
      "native_composed_v1", "burst_interval_threshold_v1",
      "burst_interval_threshold_v1", "external_interval_v1",
      "external_per_isi_v1"
    ),
    stringsAsFactors = FALSE
  )
  routes <- data.frame(
    adapter_key = c(
      "native_stpd_postcomposer_v1", "native_stpd_postcomposer_v1",
      rep("mean_isi_v1", 3L), rep("logisi_newbd_v1", 3L),
      rep("external_interval_v1", 2L), rep("external_per_isi_v1", 2L)
    ),
    output_role = c(
      "automatic_prediction", "automatic_prediction",
      "automatic_prediction", "candidate_support", "calibration_output",
      "automatic_prediction", "candidate_support", "calibration_output",
      "automatic_prediction", "candidate_support",
      "automatic_prediction", "candidate_support"
    ),
    generation_mode = c(
      "native_only", "support_to_native",
      "external_only", "external_only", "support_to_native",
      "external_only", "external_only", "support_to_native",
      rep("external_only", 4L)
    ),
    enabled = c(
      FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
      TRUE, TRUE, TRUE, TRUE
    ),
    gate_code = c(
      rep("native_gate_b_pending", 2L), NA_character_, NA_character_,
      "calibration_disabled_phase_b2", NA_character_, NA_character_,
      "calibration_disabled_phase_b2", rep(NA_character_, 4L)
    ),
    stringsAsFactors = FALSE
  )
  list(adapters = adapters, routes = routes)
}

stpd_provider_b2_adapter_registry <- stpd_provider_adapter_registry_v1

stpd_provider_b2_validate_request <- function(request) {
  fields <- c(
    "schema_version", "adapter_key", "adapter_version", "output_role",
    "generation_mode", "selected_train_keys", "parameters"
  )
  request <- stpd_provider_b2_plain_named_list(
    request, "request", exact_names = fields, require_order = TRUE
  )
  schema <- stpd_provider_b2_utf8_scalar(request$schema_version,
                                         "schema_version")
  if (!identical(schema, STPD_PROVIDER_IMPORT_REQUEST_VERSION)) {
    stpd_provider_b2_abort(
      "schema_version_unsupported", "Unsupported provider-import request schema.",
      column = "schema_version", value = schema
    )
  }
  adapter_key <- stpd_provider_b2_utf8_scalar(request$adapter_key,
                                               "adapter_key")
  adapter_version <- stpd_provider_b2_utf8_scalar(request$adapter_version,
                                                   "adapter_version")
  output_role <- stpd_provider_b2_scalar_enum(
    request$output_role, "output_role",
    stpd_provider_contract_enums()$output_role
  )
  generation_mode <- stpd_provider_b2_scalar_enum(
    request$generation_mode, "generation_mode",
    stpd_provider_contract_enums()$generation_mode
  )
  keys <- request$selected_train_keys
  if (!is.character(keys) || is.object(keys) || !length(keys) || anyNA(keys) ||
      anyDuplicated(keys)) {
    stpd_provider_b2_abort(
      "type_invalid", "selected_train_keys must be a unique character vector.",
      column = "selected_train_keys"
    )
  }
  keys <- unname(vapply(
    keys, stpd_provider_b2_utf8_scalar, character(1), field = "train_key"
  ))
  if (length(keys) > STPD_PROVIDER_B2_MAX_TRAINS) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded", "Selected-train count exceeds the B2 limit.",
      column = "selected_train_keys"
    )
  }
  parameters <- stpd_provider_b2_plain_named_list(
    request$parameters, "parameters", require_order = FALSE
  )
  registry <- stpd_provider_b2_adapter_registry()
  a <- registry$adapters[
    registry$adapters$adapter_key == adapter_key &
      registry$adapters$adapter_version == adapter_version, , drop = FALSE
  ]
  if (!nrow(a)) {
    stpd_provider_b2_abort(
      "adapter_not_allowlisted", "Adapter key/version is not allowlisted.",
      column = "adapter_key", value = adapter_key
    )
  }
  route <- registry$routes[
    registry$routes$adapter_key == adapter_key &
      registry$routes$output_role == output_role &
      registry$routes$generation_mode == generation_mode, , drop = FALSE
  ]
  if (nrow(route) != 1L) {
    stpd_provider_b2_abort(
      "provider_mode_role_invalid", "Adapter does not expose this role/mode.",
      column = "output_role", value = paste(output_role, generation_mode)
    )
  }
  if (!isTRUE(route$enabled[[1L]])) {
    gate <- route$gate_code[[1L]]
    code <- if (identical(gate, "native_gate_b_pending")) {
      "native_gate_b_pending"
    } else {
      "provider_mode_role_invalid"
    }
    stpd_provider_b2_abort(
      code, paste0("Adapter route is disabled: ", gate, "."),
      column = "generation_mode", value = gate
    )
  }
  list(
    schema_version = schema, adapter_key = adapter_key,
    adapter_version = adapter_version, output_role = output_role,
    generation_mode = generation_mode, selected_train_keys = keys,
    parameters = parameters, adapter = a[1L, , drop = FALSE]
  )
}

stpd_provider_b2_manifest_path <- function() {
  installed <- system.file(
    "provider-adapter-dependency-manifest-v1.json",
    package = "SpikeTrainPatternDetector"
  )
  candidates <- c(
    installed,
    file.path("inst", "provider-adapter-dependency-manifest-v1.json")
  )
  candidates <- candidates[nzchar(candidates)]
  hits <- candidates[file.exists(candidates)]
  if (!length(hits)) {
    stpd_provider_b2_abort(
      "required_field_missing",
      "The frozen provider-adapter dependency manifest is absent.",
      table = "provider_adapter_dependency_manifest"
    )
  }
  normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE)
}

stpd_provider_b2_parse_json_object <- function(text, field) {
  parsed <- tryCatch(
    jsonlite::fromJSON(
      text, simplifyVector = FALSE, simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    stpd_provider_b2_abort(
      "type_invalid", paste0(field, " is not valid JSON."), column = field
    )
  }
  stpd_provider_b2_plain_named_list(parsed, field, require_order = FALSE)
}

stpd_provider_b2_load_dependency_manifest <- function(adapter_key,
                                                       adapter_version) {
  path <- stpd_provider_b2_manifest_path()
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  if (length(bytes) > STPD_PROVIDER_B2_MAX_ARTIFACT_BYTES) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded", "Dependency manifest is unexpectedly large.",
      table = "provider_adapter_dependency_manifest"
    )
  }
  text <- tryCatch(rawToChar(bytes), error = function(e) NA_character_)
  if (length(text) != 1L || is.na(text) || any(bytes == as.raw(0L)) ||
      !isTRUE(stringi::stri_enc_isutf8(text))) {
    stpd_provider_b2_abort(
      "type_invalid", "Dependency manifest must be raw UTF-8 JSON.",
      table = "provider_adapter_dependency_manifest"
    )
  }
  root_fields <- c(
    "schema_version", "provider_contract_sha256",
    "provider_adapter_protocol_sha256", "adapters"
  )
  manifest <- stpd_provider_b2_parse_json_object(
    text, "provider_adapter_dependency_manifest"
  )
  manifest <- stpd_provider_b2_plain_named_list(
    manifest, "provider_adapter_dependency_manifest",
    exact_names = root_fields, require_order = TRUE
  )
  if (!identical(manifest$schema_version,
                 STPD_PROVIDER_ADAPTER_DEPENDENCY_MANIFEST_VERSION)) {
    stpd_provider_b2_abort(
      "schema_version_unsupported", "Dependency-manifest schema differs.",
      table = "provider_adapter_dependency_manifest",
      column = "schema_version"
    )
  }
  expected_contract <- stpd_provider_contract_hash()
  expected_protocol <- stpd_provider_adapter_protocol_hash_v1()
  if (!identical(manifest$provider_contract_sha256, expected_contract) ||
      !identical(manifest$provider_adapter_protocol_sha256,
                 expected_protocol)) {
    stpd_provider_b2_abort(
      "hash_mismatch", "Dependency manifest is bound to different contracts.",
      table = "provider_adapter_dependency_manifest"
    )
  }
  if (typeof(manifest$adapters) != "list" ||
      !is.null(attr(manifest$adapters, "class", exact = TRUE))) {
    stpd_provider_b2_abort(
      "type_invalid", "Dependency-manifest adapters must be an array.",
      table = "provider_adapter_dependency_manifest", column = "adapters"
    )
  }
  fields <- c(
    "adapter_key", "adapter_version", "adapter_code_sha256",
    "provider_code_sha256", "dependency_files"
  )
  entries <- lapply(seq_along(manifest$adapters), function(i) {
    entry <- stpd_provider_b2_plain_named_list(
      manifest$adapters[[i]], "dependency adapter", exact_names = fields,
      require_order = TRUE
    )
    entry$adapter_key <- stpd_provider_b2_utf8_scalar(
      entry$adapter_key, "adapter_key"
    )
    entry$adapter_version <- stpd_provider_b2_utf8_scalar(
      entry$adapter_version, "adapter_version"
    )
    for (field in c("adapter_code_sha256", "provider_code_sha256")) {
      if (!stpd_provider_b2_sha_ok(entry[[field]])) {
        stpd_provider_b2_abort(
          "sha256_invalid", "Dependency-manifest code hash is invalid.",
          table = "provider_adapter_dependency_manifest", row = i,
          column = field
        )
      }
    }
    dependency_files <- entry$dependency_files
    if (typeof(dependency_files) != "list" ||
        !is.null(attr(dependency_files, "class", exact = TRUE)) ||
        !length(dependency_files)) {
      stpd_provider_b2_abort(
        "type_invalid", "dependency_files must be a non-empty object array.",
        table = "provider_adapter_dependency_manifest", row = i,
        column = "dependency_files"
      )
    }
    dependency_files <- lapply(seq_along(dependency_files), function(j) {
      file <- stpd_provider_b2_plain_named_list(
        dependency_files[[j]], "dependency file",
        exact_names = c("path", "sha256"), require_order = TRUE
      )
      path_value <- stpd_provider_b2_utf8_scalar(file$path, "path")
      if (startsWith(path_value, "/") || grepl("(^|/)\\.\\.(/|$)", path_value) ||
          grepl("\\\\", path_value)) {
        stpd_provider_b2_abort(
          "type_invalid", "Dependency paths must be safe package-relative paths.",
          table = "provider_adapter_dependency_manifest", row = i,
          column = "path", value = path_value
        )
      }
      sha <- stpd_provider_b2_utf8_scalar(file$sha256, "sha256")
      if (!stpd_provider_b2_sha_ok(sha)) {
        stpd_provider_b2_abort(
          "sha256_invalid", "Dependency-file hash is invalid.",
          table = "provider_adapter_dependency_manifest", row = i,
          column = "sha256", value = sha
        )
      }
      list(path = path_value, sha256 = sha)
    })
    dependency_paths <- vapply(dependency_files, `[[`, character(1), "path")
    if (anyDuplicated(dependency_paths)) {
      stpd_provider_b2_abort(
        "primary_key_duplicate", "A dependency path is repeated.",
        table = "provider_adapter_dependency_manifest", row = i,
        column = "dependency_files"
      )
    }
    order_index <- order(enc2utf8(dependency_paths), method = "radix")
    if (!identical(order_index, seq_along(dependency_paths))) {
      stpd_provider_b2_abort(
        "hash_mismatch", "Dependency files are not in frozen UTF-8 path order.",
        table = "provider_adapter_dependency_manifest", row = i,
        column = "dependency_files"
      )
    }
    dependency_files <- dependency_files[order_index]
    derived_adapter_sha <- stpd_provider_hash_domain(
      "stpd-provider-adapter-code-v1",
      list(
        provider_contract_sha256 = expected_contract,
        provider_adapter_protocol_sha256 = expected_protocol,
        adapter_key = entry$adapter_key,
        adapter_version = entry$adapter_version,
        dependency_files = dependency_files
      )
    )
    if (!identical(entry$adapter_code_sha256, derived_adapter_sha)) {
      stpd_provider_b2_abort(
        "hash_mismatch",
        "adapter_code_sha256 is not derived from the dependency closure.",
        table = "provider_adapter_dependency_manifest", row = i,
        column = "adapter_code_sha256"
      )
    }
    provider_spec <- switch(
      entry$adapter_key,
      native_stpd_postcomposer_v1 = list(
        provider_key = "native_stpd", provider_version = "1.0.0",
        status = "native_gate_b_pending", paths = character()
      ),
      mean_isi_v1 = list(
        provider_key = "mean_isi", provider_version = "1.0.0",
        status = NULL, paths = "R/33_support_misi.R"
      ),
      logisi_newbd_v1 = list(
        provider_key = "logisi_newbd", provider_version = "1.0.0",
        status = NULL, paths = "R/34_support_logisi.R"
      ),
      external_interval_v1 = list(
        provider_key = "external_data_import",
        provider_version = "unverified_external_declaration_v1",
        status = "not_runtime_authority", paths = character()
      ),
      external_per_isi_v1 = list(
        provider_key = "external_data_import",
        provider_version = "unverified_external_declaration_v1",
        status = "not_runtime_authority", paths = character()
      )
    )
    if (is.null(provider_spec)) {
      stpd_provider_b2_abort(
        "adapter_not_allowlisted", "Dependency manifest contains an unknown adapter.",
        table = "provider_adapter_dependency_manifest", row = i,
        column = "adapter_key", value = entry$adapter_key
      )
    }
    provider_dependencies <- list()
    if (length(provider_spec$paths)) {
      provider_hits <- match(provider_spec$paths, dependency_paths)
      if (anyNA(provider_hits)) {
        stpd_provider_b2_abort(
          "required_field_missing",
          "Dependency manifest omits an internal provider source file.",
          table = "provider_adapter_dependency_manifest", row = i,
          column = "dependency_files"
        )
      }
      provider_dependencies <- dependency_files[provider_hits]
    }
    provider_payload <- list(
      provider_key = provider_spec$provider_key,
      provider_version = provider_spec$provider_version
    )
    if (!is.null(provider_spec$status)) {
      provider_payload$status <- provider_spec$status
    }
    provider_payload$dependency_files <- provider_dependencies
    derived_provider_sha <- stpd_provider_hash_domain(
      "stpd-provider-code-v1", provider_payload
    )
    if (!identical(entry$provider_code_sha256, derived_provider_sha)) {
      stpd_provider_b2_abort(
        "hash_mismatch",
        "provider_code_sha256 is not derived from the provider source closure.",
        table = "provider_adapter_dependency_manifest", row = i,
        column = "provider_code_sha256"
      )
    }
    entry$dependency_files <- dependency_files
    entry
  })
  keys <- vapply(entries, function(x) {
    paste(x$adapter_key, x$adapter_version, sep = "\r")
  }, character(1))
  if (anyDuplicated(keys)) {
    stpd_provider_b2_abort(
      "primary_key_duplicate", "Dependency manifest repeats an adapter.",
      table = "provider_adapter_dependency_manifest"
    )
  }
  expected_adapters <- stpd_provider_adapter_registry_v1()$adapters
  expected_keys <- paste(
    expected_adapters$adapter_key, expected_adapters$adapter_version, sep = "\r"
  )
  if (!setequal(keys, expected_keys) || length(keys) != length(expected_keys)) {
    stpd_provider_b2_abort(
      "adapter_not_allowlisted",
      "Dependency manifest must contain exactly the frozen adapter registry.",
      table = "provider_adapter_dependency_manifest"
    )
  }
  hit <- which(keys == paste(adapter_key, adapter_version, sep = "\r"))
  if (length(hit) != 1L) {
    stpd_provider_b2_abort(
      "adapter_not_allowlisted", "Dependency manifest lacks this adapter.",
      table = "provider_adapter_dependency_manifest", value = adapter_key
    )
  }
  entries[[hit]]
}

stpd_provider_b2_coordinate_profiles <- function() {
  list(
    train_row_isi_one_closed_v1 = c(
      convention = "train_row_isi_index", base = "one", closure = "closed",
      unit = "not_applicable", transformation = "identity_train_row_one_based"
    ),
    diff_isi_one_closed_v1 = c(
      convention = "diff_timestamp_isi_index", base = "one",
      closure = "closed", unit = "not_applicable",
      transformation = "diff_one_based_plus_one"
    ),
    diff_isi_zero_closed_v1 = c(
      convention = "diff_timestamp_isi_index", base = "zero",
      closure = "closed", unit = "not_applicable",
      transformation = "diff_zero_based_plus_two"
    ),
    spike_span_one_closed_v1 = c(
      convention = "spike_index_span", base = "one", closure = "closed",
      unit = "not_applicable", transformation = "spike_one_based_span_to_isi"
    ),
    spike_span_zero_closed_v1 = c(
      convention = "spike_index_span", base = "zero", closure = "closed",
      unit = "not_applicable", transformation = "spike_zero_based_span_to_isi"
    ),
    spike_time_s_closed_v1 = c(
      convention = "spike_time_span", base = "not_applicable",
      closure = "closed", unit = "s",
      transformation = "exact_spike_time_span_to_isi"
    ),
    spike_time_ms_closed_v1 = c(
      convention = "spike_time_span", base = "not_applicable",
      closure = "closed", unit = "ms",
      transformation = "exact_spike_time_span_to_isi"
    ),
    spike_time_us_closed_v1 = c(
      convention = "spike_time_span", base = "not_applicable",
      closure = "closed", unit = "us",
      transformation = "exact_spike_time_span_to_isi"
    ),
    per_isi_one_closed_v1 = c(
      convention = "per_isi_diff_index", base = "one", closure = "closed",
      unit = "not_applicable",
      transformation = "per_isi_one_based_runs_plus_one"
    ),
    per_isi_zero_closed_v1 = c(
      convention = "per_isi_diff_index", base = "zero", closure = "closed",
      unit = "not_applicable",
      transformation = "per_isi_zero_based_runs_plus_two"
    )
  )
}

stpd_provider_b2_coordinate_spec <- function(source_record_key, train_key,
                                              profile_id, start, end) {
  profiles <- stpd_provider_b2_coordinate_profiles()
  if (!(profile_id %in% names(profiles))) {
    stpd_provider_b2_abort(
      "coordinate_convention_missing", "Unknown coordinate_profile_id.",
      column = "coordinate_profile_id", value = profile_id
    )
  }
  p <- profiles[[profile_id]]
  time_mode <- identical(unname(p[["convention"]]), "spike_time_span")
  list(
    source_record_key = source_record_key,
    train_key = train_key,
    source_coordinate_convention = unname(p[["convention"]]),
    source_index_base = unname(p[["base"]]),
    source_interval_closure = unname(p[["closure"]]),
    source_time_unit = unname(p[["unit"]]),
    source_start_index = if (time_mode) NULL else start,
    source_end_index = if (time_mode) NULL else end,
    source_start_time = if (time_mode) start else NULL,
    source_end_time = if (time_mode) end else NULL,
    transformation = unname(p[["transformation"]])
  )
}

stpd_provider_b2_nullable_character <- function(x, field) {
  if (is.null(x)) return(NA_character_)
  stpd_provider_b2_utf8_scalar(x, field)
}

stpd_provider_b2_nullable_number <- function(x, field) {
  if (is.null(x)) return(NA_real_)
  stpd_provider_b2_scalar_number(x, field)
}

stpd_provider_b2_validate_candidate_semantics <- function(
    semantic_track, proposed_label, provider_decision, score_name,
    score_value, score_direction, uncertainty_kind, uncertainty_lower,
    uncertainty_upper, evidence_manifest_sha256,
    enums = stpd_provider_contract_enums(),
    track_labels = stpd_provider_contract_registry()$track_labels) {
  semantic_track <- stpd_provider_b2_scalar_enum(
    semantic_track, "semantic_track", enums$candidate_semantic_track
  )
  proposed_label <- stpd_provider_b2_scalar_enum(
    proposed_label, "proposed_label", enums$proposed_label
  )
  allowed <- track_labels[[semantic_track]]
  if (!(proposed_label %in% allowed)) {
    stpd_provider_b2_abort(
      "label_track_mismatch", "Label is not valid on the selected track.",
      column = "proposed_label", value = proposed_label
    )
  }
  provider_decision <- stpd_provider_b2_scalar_enum(
    provider_decision, "provider_decision", enums$provider_decision
  )
  score_name <- stpd_provider_b2_nullable_character(score_name, "score_name")
  score_value <- stpd_provider_b2_nullable_number(score_value, "score_value")
  score_direction <- stpd_provider_b2_scalar_enum(
    score_direction, "score_direction", enums$score_direction
  )
  score_missing <- is.na(score_name) && is.na(score_value)
  if ((!score_missing && (is.na(score_name) || is.na(score_value) ||
                          score_direction == "not_applicable")) ||
      (score_missing && score_direction != "not_applicable")) {
    stpd_provider_b2_abort(
      "score_contract_invalid", "Score fields are internally inconsistent.",
      column = "score_name"
    )
  }
  uncertainty_kind <- stpd_provider_b2_scalar_enum(
    uncertainty_kind, "uncertainty_kind", enums$uncertainty_kind
  )
  uncertainty_lower <- stpd_provider_b2_nullable_number(
    uncertainty_lower, "uncertainty_lower"
  )
  uncertainty_upper <- stpd_provider_b2_nullable_number(
    uncertainty_upper, "uncertainty_upper"
  )
  missing_uncertainty <- is.na(uncertainty_lower) && is.na(uncertainty_upper)
  if ((uncertainty_kind == "none" && !missing_uncertainty) ||
      (uncertainty_kind != "none" &&
       (is.na(uncertainty_lower) || is.na(uncertainty_upper) ||
        uncertainty_lower > uncertainty_upper))) {
    stpd_provider_b2_abort(
      "uncertainty_contract_invalid",
      "Uncertainty fields are internally inconsistent.",
      column = "uncertainty_kind"
    )
  }
  evidence <- stpd_provider_b2_nullable_character(
    evidence_manifest_sha256, "evidence_manifest_sha256"
  )
  if (!is.na(evidence) && !stpd_provider_b2_sha_ok(evidence)) {
    stpd_provider_b2_abort(
      "sha256_invalid", "Evidence-manifest hash is invalid.",
      column = "evidence_manifest_sha256", value = evidence
    )
  }
  list(
    semantic_track = semantic_track, proposed_label = proposed_label,
    provider_decision = provider_decision, score_name = score_name,
    score_value = score_value, score_direction = score_direction,
    uncertainty_kind = uncertainty_kind,
    uncertainty_lower = uncertainty_lower,
    uncertainty_upper = uncertainty_upper,
    evidence_manifest_sha256 = evidence
  )
}

stpd_provider_b2_decode_raw_json <- function(artifact) {
  if (typeof(artifact) != "raw" || is.object(artifact)) {
    stpd_provider_b2_abort(
      "executable_import_forbidden",
      "External artifacts must be supplied as an unclassed raw JSON vector.",
      column = "artifact", value = typeof(artifact)
    )
  }
  if (!length(artifact)) {
    stpd_provider_b2_abort(
      "required_field_missing", "External artifact cannot be empty.",
      column = "artifact"
    )
  }
  if (length(artifact) > STPD_PROVIDER_B2_MAX_ARTIFACT_BYTES) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded", "External artifact exceeds the B2 byte limit.",
      column = "artifact"
    )
  }
  if (any(artifact == as.raw(0L))) {
    stpd_provider_b2_abort(
      "type_invalid", "External JSON cannot contain NUL bytes.",
      column = "artifact"
    )
  }
  text <- tryCatch(rawToChar(artifact), error = function(e) NA_character_)
  if (length(text) != 1L || is.na(text) ||
      !isTRUE(stringi::stri_enc_isutf8(text))) {
    stpd_provider_b2_abort(
      "type_invalid", "External artifact must be valid UTF-8 JSON.",
      column = "artifact"
    )
  }
  value <- stpd_provider_b2_parse_json_object(text, "artifact")
  if (as.double(utils::object.size(value)) >
      STPD_PROVIDER_B2_MAX_INTERNAL_OUTPUT_BYTES) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded",
      "Parsed external artifact exceeds the B2 in-memory limit.",
      column = "artifact"
    )
  }
  list(
    value = value,
    raw_sha256 = digest::digest(artifact, algo = "sha256", serialize = FALSE)
  )
}

stpd_provider_b2_external_common_root <- function(root, expected_fields,
                                                  expected_schema) {
  root <- stpd_provider_b2_plain_named_list(
    root, "artifact", exact_names = expected_fields, require_order = TRUE
  )
  if (!identical(root$schema_version, expected_schema)) {
    stpd_provider_b2_abort(
      "schema_version_unsupported", "External artifact schema differs.",
      column = "schema_version", value = root$schema_version
    )
  }
  provider_version <- stpd_provider_b2_utf8_scalar(
    root$provider_version, "provider_version"
  )
  if (identical(provider_version, "latest")) {
    stpd_provider_b2_abort(
      "provider_capability_mismatch", "provider_version cannot be latest.",
      column = "provider_version"
    )
  }
  provider_code <- stpd_provider_b2_utf8_scalar(
    root$provider_code_sha256, "provider_code_sha256"
  )
  if (!stpd_provider_b2_sha_ok(provider_code)) {
    stpd_provider_b2_abort(
      "sha256_invalid", "provider_code_sha256 is invalid.",
      column = "provider_code_sha256", value = provider_code
    )
  }
  information_access <- stpd_provider_b2_scalar_enum(
    root$information_access, "information_access",
    stpd_provider_contract_enums()$information_access
  )
  if (!identical(information_access, "unknown")) {
    stpd_provider_b2_abort(
      "label_blind_leakage",
      "A pure-data external artifact cannot self-certify information access; v1 requires unknown.",
      column = "information_access", value = information_access
    )
  }
  profile_id <- stpd_provider_b2_utf8_scalar(
    root$coordinate_profile_id, "coordinate_profile_id"
  )
  if (!(profile_id %in% names(stpd_provider_b2_coordinate_profiles()))) {
    stpd_provider_b2_abort(
      "coordinate_convention_missing", "Unknown coordinate_profile_id.",
      column = "coordinate_profile_id", value = profile_id
    )
  }
  if (typeof(root$records) != "list" ||
      !is.null(attr(root$records, "class", exact = TRUE))) {
    stpd_provider_b2_abort(
      "type_invalid", "records must be a JSON array of objects.",
      column = "records"
    )
  }
  if (length(root$records) > STPD_PROVIDER_B2_MAX_SOURCE_RECORDS) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded", "Source-record count exceeds the B2 limit.",
      column = "records"
    )
  }
  list(
    root = root, provider_version = provider_version,
    provider_code_sha256 = provider_code,
    information_access = information_access,
    profile_id = profile_id
  )
}

stpd_provider_b2_parse_external_interval <- function(root) {
  root_fields <- c(
    "schema_version", "provider_version", "provider_code_sha256",
    "information_access", "coordinate_profile_id", "records"
  )
  common <- stpd_provider_b2_external_common_root(
    root, root_fields, STPD_EXTERNAL_INTERVAL_ARTIFACT_VERSION
  )
  contract_registry <- stpd_provider_contract_registry()
  if (grepl("^per_isi_", common$profile_id)) {
    stpd_provider_b2_abort(
      "provider_capability_mismatch",
      "The interval adapter cannot use a per-ISI coordinate profile.",
      column = "coordinate_profile_id"
    )
  }
  record_fields <- c(
    "source_record_key", "train_key", "source_start", "source_end",
    "semantic_track", "proposed_label", "provider_decision", "score_name",
    "score_value", "score_direction", "uncertainty_kind",
    "uncertainty_lower", "uncertainty_upper", "evidence_manifest_sha256"
  )
  records <- vector("list", length(common$root$records))
  specs <- vector("list", length(common$root$records))
  semantics <- vector("list", length(common$root$records))
  keys <- character(length(common$root$records))
  for (i in seq_along(common$root$records)) {
    record <- stpd_provider_b2_plain_named_list(
      common$root$records[[i]], "interval record", exact_names = record_fields,
      require_order = TRUE
    )
    key <- stpd_provider_b2_utf8_scalar(record$source_record_key,
                                         "source_record_key")
    train <- stpd_provider_b2_utf8_scalar(record$train_key, "train_key")
    start <- stpd_provider_b2_scalar_number(record$source_start,
                                             "source_start")
    end <- stpd_provider_b2_scalar_number(record$source_end, "source_end")
    semantics[[i]] <- do.call(
      stpd_provider_b2_validate_candidate_semantics,
      c(
        record[c(
          "semantic_track", "proposed_label", "provider_decision", "score_name",
          "score_value", "score_direction", "uncertainty_kind",
          "uncertainty_lower", "uncertainty_upper",
          "evidence_manifest_sha256"
        )],
        list(
          enums = contract_registry$enums,
          track_labels = contract_registry$track_labels
        )
      )
    )
    # Canonicalize the provider-owned JSON record to frozen R scalar types.
    record$source_record_key <- key
    record$train_key <- train
    record$source_start <- start
    record$source_end <- end
    records[[i]] <- record
    specs[[i]] <- stpd_provider_b2_coordinate_spec(
      key, train, common$profile_id, start, end
    )
    keys[[i]] <- key
  }
  if (anyDuplicated(keys)) {
    stpd_provider_b2_abort(
      "primary_key_duplicate", "source_record_key must be unique.",
      column = "source_record_key"
    )
  }
  c(common[c(
    "provider_version", "provider_code_sha256", "information_access",
    "profile_id"
  )], list(records = records, specs = specs, semantics = semantics,
           per_isi = FALSE))
}

stpd_provider_b2_parse_external_per_isi <- function(root) {
  root_fields <- c(
    "schema_version", "provider_version", "provider_code_sha256",
    "information_access", "coordinate_profile_id", "semantic_track",
    "target_label", "records"
  )
  common <- stpd_provider_b2_external_common_root(
    root, root_fields, STPD_EXTERNAL_PER_ISI_ARTIFACT_VERSION
  )
  contract_registry <- stpd_provider_contract_registry()
  if (!(common$profile_id %in%
        c("per_isi_one_closed_v1", "per_isi_zero_closed_v1"))) {
    stpd_provider_b2_abort(
      "provider_capability_mismatch",
      "The per-ISI adapter requires a per-ISI coordinate profile.",
      column = "coordinate_profile_id"
    )
  }
  semantic_track <- stpd_provider_b2_scalar_enum(
    common$root$semantic_track, "semantic_track",
    contract_registry$enums$candidate_semantic_track
  )
  target_label <- stpd_provider_b2_scalar_enum(
    common$root$target_label, "target_label",
    contract_registry$enums$proposed_label
  )
  if (!(target_label %in%
        contract_registry$track_labels[[semantic_track]])) {
    stpd_provider_b2_abort(
      "label_track_mismatch", "Per-ISI target_label is on the wrong track.",
      column = "target_label"
    )
  }
  record_fields <- c(
    "source_record_key", "train_key", "diff_index", "provider_decision",
    "score_name", "score_value", "score_direction", "uncertainty_kind",
    "uncertainty_lower", "uncertainty_upper", "evidence_manifest_sha256"
  )
  records <- vector("list", length(common$root$records))
  specs <- vector("list", length(common$root$records))
  semantics <- vector("list", length(common$root$records))
  keys <- character(length(common$root$records))
  for (i in seq_along(common$root$records)) {
    record <- stpd_provider_b2_plain_named_list(
      common$root$records[[i]], "per-ISI record", exact_names = record_fields,
      require_order = TRUE
    )
    key <- stpd_provider_b2_utf8_scalar(record$source_record_key,
                                         "source_record_key")
    train <- stpd_provider_b2_utf8_scalar(record$train_key, "train_key")
    index <- stpd_provider_b2_scalar_number(
      record$diff_index, "diff_index", lower = 0, integer = TRUE
    )
    sem_args <- c(
      list(semantic_track = semantic_track, proposed_label = target_label),
      record[c(
        "provider_decision", "score_name", "score_value", "score_direction",
        "uncertainty_kind", "uncertainty_lower", "uncertainty_upper",
        "evidence_manifest_sha256"
      )]
    )
    semantics[[i]] <- do.call(
      stpd_provider_b2_validate_candidate_semantics,
      c(
        sem_args,
        list(
          enums = contract_registry$enums,
          track_labels = contract_registry$track_labels
        )
      )
    )
    if (!(semantics[[i]]$provider_decision %in% c("positive", "negative"))) {
      stpd_provider_b2_abort(
        "provider_capability_mismatch",
        "A per-ISI binary channel permits only positive or negative decisions.",
        row = i, column = "provider_decision"
      )
    }
    record$source_record_key <- key
    record$train_key <- train
    record$diff_index <- index
    records[[i]] <- record
    specs[[i]] <- stpd_provider_b2_coordinate_spec(
      key, train, common$profile_id, index, index
    )
    keys[[i]] <- key
  }
  if (anyDuplicated(keys)) {
    stpd_provider_b2_abort(
      "primary_key_duplicate", "source_record_key must be unique.",
      column = "source_record_key"
    )
  }
  c(common[c(
    "provider_version", "provider_code_sha256", "information_access",
    "profile_id"
  )], list(records = records, specs = specs, semantics = semantics,
           per_isi = TRUE))
}

stpd_provider_b2_misi_parameters <- function(parameters) {
  defaults <- list(
    min_valid_isi_sec = 0.001,
    min_isi_count = 2L,
    max_isi_count = NULL,
    max_windows = STPD_PROVIDER_B2_MAX_MISI_WINDOWS_PER_TRAIN,
    min_spikes = 3L,
    min_duration_sec = 0
  )
  p <- stpd_provider_b2_plain_parameters(
    parameters, names(defaults), defaults
  )
  p$min_valid_isi_sec <- stpd_provider_b2_scalar_number(
    p$min_valid_isi_sec, "min_valid_isi_sec", lower = .Machine$double.eps
  )
  p$min_isi_count <- stpd_provider_b2_scalar_number(
    p$min_isi_count, "min_isi_count", lower = 2, upper = 1000000,
    integer = TRUE
  )
  if (!is.null(p$max_isi_count)) {
    p$max_isi_count <- stpd_provider_b2_scalar_number(
      p$max_isi_count, "max_isi_count", lower = p$min_isi_count,
      upper = 1000000, integer = TRUE
    )
  }
  p$max_windows <- stpd_provider_b2_scalar_number(
    p$max_windows, "max_windows", lower = 1,
    upper = STPD_PROVIDER_B2_MAX_MISI_WINDOWS_PER_TRAIN, integer = TRUE
  )
  p$min_spikes <- stpd_provider_b2_scalar_number(
    p$min_spikes, "min_spikes", lower = 3, upper = 1000000,
    integer = TRUE
  )
  p$min_duration_sec <- stpd_provider_b2_scalar_number(
    p$min_duration_sec, "min_duration_sec", lower = 0
  )
  p
}

stpd_provider_b2_logisi_parameters <- function(parameters) {
  defaults <- list(
    min_valid_isi_sec = 0.001,
    min_num_spikes = 5L,
    core_reference_sec = 0.100,
    max_reasonable_threshold_sec = 1.0,
    fallback_ch = TRUE,
    fallback_maxISI_sec = 0.100,
    multiple_core_mode = "split_by_core",
    bin_width_log10 = 0.05,
    lowess_span = 0.20,
    min_peak_distance = 3L,
    intraburst_peak_window_ms = 100,
    void_threshold = 0.7,
    valley_selection = "max_void"
  )
  p <- stpd_provider_b2_plain_parameters(
    parameters, names(defaults), defaults
  )
  p$min_valid_isi_sec <- stpd_provider_b2_scalar_number(
    p$min_valid_isi_sec, "min_valid_isi_sec", lower = .Machine$double.eps
  )
  p$min_num_spikes <- stpd_provider_b2_scalar_number(
    p$min_num_spikes, "min_num_spikes", lower = 3, upper = 1000000,
    integer = TRUE
  )
  for (field in c(
    "core_reference_sec", "max_reasonable_threshold_sec",
    "fallback_maxISI_sec", "bin_width_log10", "lowess_span",
    "intraburst_peak_window_ms", "void_threshold"
  )) {
    lower <- if (identical(field, "void_threshold")) 0 else
      .Machine$double.eps
    upper <- if (field %in% c("lowess_span", "void_threshold")) 1 else Inf
    p[[field]] <- stpd_provider_b2_scalar_number(
      p[[field]], field, lower = lower, upper = upper
    )
  }
  p$min_peak_distance <- stpd_provider_b2_scalar_number(
    p$min_peak_distance, "min_peak_distance", lower = 1, upper = 1000000,
    integer = TRUE
  )
  p$fallback_ch <- stpd_provider_b2_scalar_logical(p$fallback_ch, "fallback_ch")
  p$multiple_core_mode <- stpd_provider_b2_scalar_enum(
    p$multiple_core_mode, "multiple_core_mode",
    c("split_by_core", "join_loose_window")
  )
  p$valley_selection <- stpd_provider_b2_scalar_enum(
    p$valley_selection, "valley_selection",
    c("max_void", "first_eligible")
  )
  p
}

stpd_provider_b2_algorithm_record <- function(train_key, row) {
  values <- lapply(row, function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (length(x) != 1L) {
      stpd_provider_b2_abort(
        "type_invalid", "Algorithm output contains a non-scalar cell.",
        table = "provider_output"
      )
    }
    unname(x)
  })
  c(list(train_key = train_key), values)
}

stpd_provider_b2_threshold_source <- function(train_key, threshold_name,
                                              threshold_value,
                                              threshold_status) {
  list(
    train_key = train_key,
    threshold_name = threshold_name,
    threshold_value = as.double(threshold_value),
    threshold_status = as.character(threshold_status)
  )
}

stpd_provider_b2_train_scope_sha256 <- function(spine, train_key) {
  hit <- match(train_key, spine$train_manifest$train_key)
  stpd_provider_hash_domain(
    "stpd-provider-threshold-scope-train-v1",
    list(
      dataset_snapshot_sha256 = spine$dataset_snapshot_sha256,
      train_key = train_key,
      train_timestamp_sha256 =
        spine$train_manifest$train_timestamp_sha256[[hit]]
    )
  )
}

stpd_provider_b2_run_misi <- function(spine, parameters) {
  p <- stpd_provider_b2_misi_parameters(parameters)
  estimated_windows <- sum(vapply(spine$timestamps, function(x) {
    n <- max(0L, length(x) - 1L)
    max_k <- if (is.null(p$max_isi_count)) n else min(n, p$max_isi_count)
    if (n < p$min_isi_count || max_k < p$min_isi_count) return(0)
    sum(pmax(0, n - seq.int(p$min_isi_count, max_k) + 1))
  }, double(1)))
  if (estimated_windows > STPD_PROVIDER_B2_MAX_MISI_WINDOWS_TOTAL) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded",
      "Requested Mean-ISI search exceeds the B2 total-window limit.",
      column = "max_windows", value = estimated_windows
    )
  }
  records <- list()
  specs <- list()
  semantics <- list()
  thresholds <- list()
  raw_trains <- vector("list", length(spine$timestamps))
  names(raw_trains) <- names(spine$timestamps)
  unresolved <- character()
  for (train in names(spine$timestamps)) {
    x <- spine$timestamps[[train]]
    res <- stpd_detect_misi_bursts_article(
      spike_times_sec = x,
      min_valid_isi_sec = p$min_valid_isi_sec,
      min_isi_count = p$min_isi_count,
      max_isi_count = if (is.null(p$max_isi_count)) Inf else p$max_isi_count,
      max_windows = p$max_windows,
      min_spikes = p$min_spikes,
      min_duration_sec = p$min_duration_sec,
      collapse_exact_duplicates = FALSE
    )
    status <- if (is.data.frame(res$threshold) && nrow(res$threshold)) {
      as.character(res$threshold$threshold_status[[1L]])
    } else {
      "unresolved_missing_threshold"
    }
    ml <- if (is.data.frame(res$threshold) && nrow(res$threshold)) {
      as.double(res$threshold$ML_sec[[1L]])
    } else NA_real_
    if (!identical(status, "resolved") || !is.finite(ml)) {
      unresolved <- c(unresolved, paste0(train, ":", status))
    } else {
      source <- stpd_provider_b2_threshold_source(
        train, "mean_isi_ML_sec", ml, status
      )
      thresholds[[length(thresholds) + 1L]] <- list(
        threshold_name = "mean_isi_ML_sec",
        parameter_path = "mean_isi.ML_sec",
        threshold_value = ml, threshold_unit = "sec",
        threshold_role = "effective_parameter", scope_type = "train",
        scope_sha256 = stpd_provider_b2_train_scope_sha256(spine, train),
        semantic_track = "event", target_label = "burst",
        source_kind = "unsupervised_data_derived",
        source_record_sha256 = stpd_provider_b2_source_sha256(source),
        comparison_operator = "le", evidence_manifest_sha256 = NA_character_
      )
    }
    bursts <- res$bursts
    truncated <- grepl("truncated_at_k_", as.character(res$method_warning),
                       fixed = TRUE) ||
      (is.data.frame(bursts) && nrow(bursts) &&
       any(grepl("^truncated_at_k_", bursts$search_status)))
    if (isTRUE(truncated)) {
      unresolved <- c(unresolved, paste0(train, ":search_truncated"))
    }
    if (is.data.frame(bursts) && nrow(bursts)) {
      for (i in seq_len(nrow(bursts))) {
        row <- as.list(bursts[i, , drop = FALSE])
        source <- stpd_provider_b2_algorithm_record(train, row)
        key <- paste0(train, ":mean_isi_burst:", as.integer(i))
        records[[length(records) + 1L]] <- source
        specs[[length(specs) + 1L]] <- stpd_provider_b2_coordinate_spec(
          key, train, "diff_isi_one_closed_v1",
          as.integer(bursts$start_isi[[i]]),
          as.integer(bursts$end_isi[[i]])
        )
        semantics[[length(semantics) + 1L]] <- list(
          semantic_track = "event", proposed_label = "burst",
          provider_decision = "positive", score_name = NA_character_,
          score_value = NA_real_, score_direction = "not_applicable",
          uncertainty_kind = "none", uncertainty_lower = NA_real_,
          uncertainty_upper = NA_real_, evidence_manifest_sha256 = NA_character_
        )
      }
    }
    raw_trains[[train]] <- list(
      threshold = res$threshold, bursts = res$bursts,
      windows = res$windows, isi_table = res$isi_table,
      method_warning = res$method_warning
    )
  }
  list(
    parameters = p, records = records, specs = specs, semantics = semantics,
    thresholds = thresholds, raw_output = list(
      method = "mean_isi_article", parameters = p, trains = raw_trains
    ),
    unresolved = unresolved, per_isi = FALSE,
    provider_version = "1.0.0", information_access = "label_blind"
  )
}

stpd_provider_b2_run_logisi <- function(spine, parameters) {
  p <- stpd_provider_b2_logisi_parameters(parameters)
  records <- list()
  specs <- list()
  semantics <- list()
  thresholds <- list()
  raw_trains <- vector("list", length(spine$timestamps))
  names(raw_trains) <- names(spine$timestamps)
  unresolved <- character()
  for (train in names(spine$timestamps)) {
    x <- spine$timestamps[[train]]
    res <- stpd_detect_logisi_newBD_pasquale(
      spike_times_sec = x,
      min_valid_isi_sec = p$min_valid_isi_sec,
      min_num_spikes = p$min_num_spikes,
      core_reference_sec = p$core_reference_sec,
      max_reasonable_threshold_sec = p$max_reasonable_threshold_sec,
      fallback_ch = p$fallback_ch,
      fallback_maxISI_sec = p$fallback_maxISI_sec,
      multiple_core_mode = p$multiple_core_mode,
      bin_width_log10 = p$bin_width_log10,
      lowess_span = p$lowess_span,
      min_peak_distance = p$min_peak_distance,
      intraburst_peak_window_ms = p$intraburst_peak_window_ms,
      void_threshold = p$void_threshold,
      valley_selection = p$valley_selection
    )
    th <- res$threshold
    reported <- if (!is.null(th$threshold_sec) && length(th$threshold_sec)) {
      suppressWarnings(as.double(th$threshold_sec[[1L]]))
    } else NA_real_
    status <- if (!is.null(th$threshold_status) && length(th$threshold_status)) {
      as.character(th$threshold_status[[1L]])
    } else "unresolved_missing_threshold"
    use_fallback <- !is.finite(reported) ||
      reported > p$max_reasonable_threshold_sec
    if (use_fallback && p$fallback_ch) {
      max1 <- p$fallback_maxISI_sec
      max2 <- p$fallback_maxISI_sec
      source_kind <- "provider_default"
    } else if (is.finite(reported) && reported > p$core_reference_sec) {
      max1 <- p$core_reference_sec
      max2 <- reported
      source_kind <- "unsupervised_data_derived"
    } else if (is.finite(reported)) {
      max1 <- reported
      max2 <- reported
      source_kind <- "unsupervised_data_derived"
    } else {
      max1 <- NA_real_
      max2 <- NA_real_
      source_kind <- "unsupervised_data_derived"
    }
    # Fallback results remain in the immutable raw output for diagnosis, but an
    # unresolved train makes the entire provider run rejected.  It must never
    # masquerade as a valid all-negative automatic prediction.
    resolved <- is.finite(max1) && is.finite(max2) &&
      !grepl("unresolved", status, fixed = TRUE)
    if (!resolved) {
      unresolved <- c(unresolved, paste0(train, ":", status))
    } else {
      for (item in list(
        list(name = "logisi_maxISI1_sec", path = "logisi_newbd.maxISI1_sec",
             value = max1),
        list(name = "logisi_maxISI2_sec", path = "logisi_newbd.maxISI2_sec",
             value = max2)
      )) {
        source <- stpd_provider_b2_threshold_source(
          train, item$name, item$value, status
        )
        thresholds[[length(thresholds) + 1L]] <- list(
          threshold_name = item$name, parameter_path = item$path,
          threshold_value = as.double(item$value), threshold_unit = "sec",
          threshold_role = "effective_parameter", scope_type = "train",
          scope_sha256 = stpd_provider_b2_train_scope_sha256(spine, train),
          semantic_track = "event", target_label = "burst",
          source_kind = source_kind,
          source_record_sha256 = if (source_kind == "provider_default") {
            NA_character_
          } else stpd_provider_b2_source_sha256(source),
          comparison_operator = "lt", evidence_manifest_sha256 = NA_character_
        )
      }
      if (is.finite(reported)) {
        source <- stpd_provider_b2_threshold_source(
          train, "logisi_reported_threshold_sec", reported, status
        )
        thresholds[[length(thresholds) + 1L]] <- list(
          threshold_name = "logisi_reported_threshold_sec",
          parameter_path = NA_character_, threshold_value = reported,
          threshold_unit = "sec", threshold_role = "reported",
          scope_type = "train",
          scope_sha256 = stpd_provider_b2_train_scope_sha256(spine, train),
          semantic_track = "event", target_label = "burst",
          source_kind = "unsupervised_data_derived",
          source_record_sha256 = stpd_provider_b2_source_sha256(source),
          comparison_operator = "lt", evidence_manifest_sha256 = NA_character_
        )
      }
    }
    bursts <- res$bursts
    if (is.data.frame(bursts) && nrow(bursts)) {
      for (i in seq_len(nrow(bursts))) {
        row <- as.list(bursts[i, , drop = FALSE])
        source <- stpd_provider_b2_algorithm_record(train, row)
        key <- paste0(train, ":logisi_burst:", as.integer(i))
        records[[length(records) + 1L]] <- source
        specs[[length(specs) + 1L]] <- stpd_provider_b2_coordinate_spec(
          key, train, "diff_isi_one_closed_v1",
          as.integer(bursts$start_isi[[i]]),
          as.integer(bursts$end_isi[[i]])
        )
        semantics[[length(semantics) + 1L]] <- list(
          semantic_track = "event", proposed_label = "burst",
          provider_decision = "positive", score_name = NA_character_,
          score_value = NA_real_, score_direction = "not_applicable",
          uncertainty_kind = "none", uncertainty_lower = NA_real_,
          uncertainty_upper = NA_real_, evidence_manifest_sha256 = NA_character_
        )
      }
    }
    raw_trains[[train]] <- list(
      threshold = res$threshold, bursts = res$bursts,
      core_segments = res$core_segments, loose_segments = res$loose_segments,
      effective = list(maxISI1_sec = max1, maxISI2_sec = max2),
      method_warning = res$method_warning
    )
  }
  list(
    parameters = p, records = records, specs = specs, semantics = semantics,
    thresholds = thresholds, raw_output = list(
      method = "pasquale_logisi_newBD", parameters = p, trains = raw_trains
    ),
    unresolved = unresolved, per_isi = FALSE,
    provider_version = "1.0.0", information_access = "label_blind"
  )
}

stpd_provider_b2_empty_bundle <- function() {
  prototypes <- stpd_provider_bundle_prototypes()
  lapply(prototypes, function(x) x[0L, , drop = FALSE])
}

stpd_provider_b2_one_typed_row <- function(table) {
  prototype <- stpd_provider_bundle_prototypes()[[table]]
  out <- prototype[NA_integer_, , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_b2_provider_run_row <- function(
    adapter, request, spine, run_status, provider_version,
    information_access, input_artifact_sha256, params_sha256,
    raw_output_sha256, provider_code_sha256, adapter_code_sha256) {
  row <- stpd_provider_b2_one_typed_row("provider_runs")
  authority <- if (identical(run_status, "rejected")) {
    "none_candidate"
  } else if (identical(request$output_role, "automatic_prediction")) {
    "automatic_prediction_record"
  } else {
    "support_evidence_only"
  }
  row$schema_version <- STPD_PROVIDER_BUNDLE_VERSION
  row$provider_key <- as.character(adapter$provider_key[[1L]])
  row$provider_kind <- as.character(adapter$provider_kind[[1L]])
  row$provider_version <- provider_version
  row$adapter_key <- request$adapter_key
  row$adapter_version <- request$adapter_version
  row$output_role <- request$output_role
  row$authority_scope <- authority
  row$generation_mode <- request$generation_mode
  row$information_access <- information_access
  row$capability_profile <- as.character(adapter$capability_profile[[1L]])
  row$run_status <- run_status
  row$dataset_snapshot_sha256 <- spine$dataset_snapshot_sha256
  row$input_artifact_sha256 <- input_artifact_sha256
  row$timestamp_spine_sha256 <- spine$timestamp_spine_sha256
  row$params_sha256 <- params_sha256
  row$calibration_bundle_id <- NA_character_
  row$raw_output_sha256 <- raw_output_sha256
  row$normalized_output_sha256 <- paste(rep("0", 64L), collapse = "")
  row$provider_code_sha256 <- provider_code_sha256
  row$adapter_code_sha256 <- adapter_code_sha256
  row$contract_sha256 <- stpd_provider_contract_hash()
  row$created_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  row$provider_run_id <- stpd_provider_run_id_v1(row)
  row
}

stpd_provider_b2_geometry_key <- function(dat) {
  paste(
    dat$train_key, dat$train_timestamp_sha256,
    dat$canonical_start_isi, dat$canonical_end_isi,
    dat$canonical_start_spike, dat$canonical_end_spike,
    format(dat$canonical_start_time_sec, digits = 17, scientific = TRUE),
    format(dat$canonical_end_time_sec, digits = 17, scientific = TRUE),
    sep = "\r"
  )
}

stpd_provider_b2_check_materialized_rows <- function(source_rows,
                                                      implied_parent_rows) {
  total <- as.double(source_rows) + as.double(implied_parent_rows)
  if (length(total) != 1L || !is.finite(total) || total < 0 ||
      total > STPD_PROVIDER_B2_MAX_SOURCE_RECORDS) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded",
      "Candidate materialization exceeds the B2 row limit.",
      table = "candidate_intervals"
    )
  }
  invisible(total)
}

stpd_provider_b2_materialize_candidates <- function(batch, semantics,
                                                    provider_run_id,
                                                    output_role) {
  prototype <- stpd_provider_bundle_prototypes()$candidate_intervals
  if (!isTRUE(batch$atomic_accept) || !length(semantics)) {
    return(prototype[0L, , drop = FALSE])
  }
  if (length(semantics) != nrow(batch$normalization_audit)) {
    stpd_provider_b2_abort(
      "coordinate_roundtrip_mismatch",
      "Candidate semantics and normalization records differ in length."
    )
  }
  audit_keys <- vapply(semantics, function(x) x$source_record_key,
                       character(1))
  if (anyDuplicated(audit_keys)) {
    stpd_provider_b2_abort(
      "primary_key_duplicate", "Candidate semantics repeat a source key."
    )
  }
  geometry <- batch$normalized_geometry
  semantic_rows <- match(geometry$source_record_key, audit_keys)
  if (anyNA(semantic_rows)) {
    stpd_provider_b2_abort(
      "coordinate_roundtrip_mismatch",
      "Normalized geometry lacks matching candidate semantics."
    )
  }
  items <- semantics[semantic_rows]
  n <- nrow(geometry)
  if (!n) return(prototype[0L, , drop = FALSE])
  out <- prototype[rep(NA_integer_, n), , drop = FALSE]
  rownames(out) <- NULL
  out$schema_version <- rep(STPD_PROVIDER_BUNDLE_VERSION, n)
  out$provider_run_id <- rep(provider_run_id, n)
  out$source_record_key <- geometry$source_record_key
  out$source_record_sha256 <- geometry$source_record_sha256
  out$normalization_audit_id <- geometry$normalization_audit_id
  out$train_key <- geometry$train_key
  out$train_timestamp_sha256 <- geometry$train_timestamp_sha256
  out$record_kind <- rep(
    if (output_role == "automatic_prediction") {
      "automatic_assertion"
    } else {
      "support"
    },
    n
  )
  out$provider_decision <- vapply(
    items, `[[`, character(1), "provider_decision"
  )
  out$semantic_track <- vapply(items, `[[`, character(1), "semantic_track")
  out$proposed_label <- vapply(items, `[[`, character(1), "proposed_label")
  out$canonical_start_isi <- as.integer(geometry$canonical_start_isi)
  out$canonical_end_isi <- as.integer(geometry$canonical_end_isi)
  out$canonical_start_spike <- as.integer(geometry$canonical_start_spike)
  out$canonical_end_spike <- as.integer(geometry$canonical_end_spike)
  out$canonical_start_time_sec <- as.double(geometry$canonical_start_time_sec)
  out$canonical_end_time_sec <- as.double(geometry$canonical_end_time_sec)
  out$canonical_interval_closure <- rep("closed", n)
  out$score_name <- vapply(items, `[[`, character(1), "score_name")
  out$score_value <- as.double(vapply(items, `[[`, numeric(1), "score_value"))
  out$score_direction <- vapply(
    items, `[[`, character(1), "score_direction"
  )
  out$uncertainty_kind <- vapply(
    items, `[[`, character(1), "uncertainty_kind"
  )
  out$uncertainty_lower <- as.double(vapply(
    items, `[[`, numeric(1), "uncertainty_lower"
  ))
  out$uncertainty_upper <- as.double(vapply(
    items, `[[`, numeric(1), "uncertainty_upper"
  ))
  out$evidence_manifest_sha256 <- vapply(
    items, `[[`, character(1), "evidence_manifest_sha256"
  )
  out$candidate_id <- stpd_provider_candidate_ids_v1(out)
  all_keys <- stpd_provider_b2_geometry_key(out)
  broad_rows <- which(
    out$provider_decision == "positive" & out$semantic_track == "state" &
      out$proposed_label == "broad_hfs"
  )
  broad_keys <- all_keys[broad_rows]
  duplicate_parent_keys <- unique(broad_keys[duplicated(broad_keys)])
  if (length(duplicate_parent_keys)) {
    stpd_provider_b2_abort(
      "primary_key_duplicate",
      "A positive HFS subtype has multiple explicit Broad-HFS parents.",
      table = "candidate_intervals", column = "proposed_label"
    )
  }
  subtype_rows <- which(
    out$provider_decision == "positive" &
      out$proposed_label %in% c("hft", "hf_irregular")
  )
  subtype_keys <- sort(unique(all_keys[subtype_rows]), method = "radix")
  missing_parent_keys <- setdiff(subtype_keys, unique(broad_keys))
  stpd_provider_b2_check_materialized_rows(
    nrow(out), length(missing_parent_keys)
  )
  if (length(missing_parent_keys)) {
    ordered_subtype_rows <- subtype_rows[order(
      all_keys[subtype_rows], out$candidate_id[subtype_rows], method = "radix"
    )]
    representative_rows <- ordered_subtype_rows[
      !duplicated(all_keys[ordered_subtype_rows])
    ]
    representative_by_key <- setNames(
      representative_rows, all_keys[representative_rows]
    )
    parent_rows <- unname(representative_by_key[missing_parent_keys])
    if (anyNA(parent_rows)) {
      stpd_provider_b2_abort(
        "coordinate_roundtrip_mismatch",
        "An implied Broad-HFS parent lacks subtype geometry.",
        table = "candidate_intervals"
      )
    }
    parents <- out[parent_rows, , drop = FALSE]
    parents$proposed_label <- "broad_hfs"
    parents$score_name <- NA_character_
    parents$score_value <- NA_real_
    parents$score_direction <- "not_applicable"
    parents$uncertainty_kind <- "none"
    parents$uncertainty_lower <- NA_real_
    parents$uncertainty_upper <- NA_real_
    parents$candidate_id <- stpd_provider_candidate_ids_v1(parents)
    out <- rbind(out, parents)
  }
  out <- out[order(out$candidate_id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_b2_materialize_thresholds <- function(threshold_specs,
                                                   provider_run_id) {
  prototype <- stpd_provider_bundle_prototypes()$thresholds
  if (!length(threshold_specs)) return(prototype[0L, , drop = FALSE])
  rows <- lapply(threshold_specs, function(item) {
    row <- stpd_provider_b2_one_typed_row("thresholds")
    row$schema_version <- STPD_PROVIDER_BUNDLE_VERSION
    row$provider_run_id <- provider_run_id
    row$calibration_bundle_id <- NA_character_
    for (field in names(item)) row[[field]] <- item[[field]]
    row$threshold_value <- as.double(row$threshold_value)
    row$threshold_id <- stpd_provider_threshold_id_v1(row)
    row
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$threshold_id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_b2_bind_rows <- function(table, rows) {
  prototype <- stpd_provider_bundle_prototypes()[[table]]
  if (!length(rows)) return(prototype[0L, , drop = FALSE])
  for (i in seq_along(rows)) {
    if (!is.data.frame(rows[[i]]) ||
        !identical(names(rows[[i]]), names(prototype))) {
      stpd_provider_b2_abort(
        "type_invalid", "Bundle tables cannot be combined safely.",
        table = table, row = i
      )
    }
  }
  out <- do.call(rbind, c(list(prototype[0L, , drop = FALSE]), rows))
  primary <- stpd_provider_contract_registry()$tables[[table]]$primary_key
  if (nrow(out)) {
    out <- out[order(out[[primary]], method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

stpd_provider_combine_bundles_v1 <- function(bundles) {
  if (typeof(bundles) != "list" || is.object(bundles) || !length(bundles)) {
    stpd_provider_b2_abort(
      "type_invalid", "bundles must be a non-empty plain list.",
      table = "bundle"
    )
  }
  invisible(lapply(bundles, stpd_validate_provider_bundle))
  table_names <- names(stpd_provider_bundle_prototypes())
  out <- setNames(lapply(table_names, function(table) {
    stpd_provider_b2_bind_rows(table, lapply(bundles, `[[`, table))
  }), table_names)
  stpd_validate_provider_bundle(out)
  out
}

stpd_provider_b2_internal_input_sha256 <- function(spine, request,
                                                   parameters) {
  stpd_provider_hash_domain("stpd-provider-input-artifact-v1", list(
    adapter_key = request$adapter_key,
    adapter_version = request$adapter_version,
    output_role = request$output_role,
    generation_mode = request$generation_mode,
    dataset_snapshot_sha256 = spine$dataset_snapshot_sha256,
    timestamp_spine_sha256 = spine$timestamp_spine_sha256,
    train_manifest = spine$train_manifest,
    parameters = parameters
  ))
}

stpd_provider_b2_semantic_analysis <- function(batch, specs, semantics) {
  empty <- list(issues = character(), missing_parent_keys = character())
  if (!isTRUE(batch$atomic_accept) || !length(semantics)) return(empty)
  keys <- vapply(specs, `[[`, character(1), "source_record_key")
  geometry_rows <- match(keys, batch$normalized_geometry$source_record_key)
  if (anyNA(geometry_rows)) {
    stpd_provider_b2_abort(
      "coordinate_roundtrip_mismatch",
      "Semantic preflight lacks matching normalized geometry."
    )
  }
  geometry <- batch$normalized_geometry[geometry_rows, , drop = FALSE]
  labels <- vapply(semantics, `[[`, character(1), "proposed_label")
  decisions <- vapply(semantics, `[[`, character(1), "provider_decision")
  tracks <- vapply(semantics, `[[`, character(1), "semantic_track")
  geometry_keys <- stpd_provider_b2_geometry_key(geometry)
  state_positive <- tracks == "state" & decisions == "positive"
  broad_keys <- geometry_keys[
    state_positive & labels == "broad_hfs"
  ]
  duplicate_parent_keys <- sort(
    unique(broad_keys[duplicated(broad_keys)]), method = "radix"
  )
  hft_keys <- unique(geometry_keys[state_positive & labels == "hft"])
  hfi_keys <- unique(
    geometry_keys[state_positive & labels == "hf_irregular"]
  )
  conflict_keys <- sort(intersect(hft_keys, hfi_keys), method = "radix")
  subtype_keys <- sort(unique(c(hft_keys, hfi_keys)), method = "radix")
  missing_parent_keys <- setdiff(subtype_keys, unique(broad_keys))
  issues <- c(
    if (length(duplicate_parent_keys)) {
      paste0(duplicate_parent_keys, ":multiple_broad_hfs_parents")
    } else character(),
    if (length(conflict_keys)) {
      paste0(conflict_keys, ":conflicting_hfs_subtypes")
    } else character()
  )
  list(
    issues = sort(unique(issues), method = "radix"),
    missing_parent_keys = missing_parent_keys
  )
}

stpd_provider_b2_semantic_issues <- function(batch, specs, semantics) {
  stpd_provider_b2_semantic_analysis(batch, specs, semantics)$issues
}

stpd_provider_b2_finalize_bundle <- function(
    adapter_result, adapter, request, spine, input_artifact_sha256,
    raw_output_sha256, provider_code_sha256, adapter_code_sha256) {
  params_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-parameters-v1", adapter_result$parameters
  )
  preflight_id <- stpd_provider_hash_domain(
    "stpd-provider-import-preflight-v1",
    list(
      adapter_key = request$adapter_key,
      adapter_version = request$adapter_version,
      output_role = request$output_role,
      generation_mode = request$generation_mode,
      dataset_snapshot_sha256 = spine$dataset_snapshot_sha256,
      input_artifact_sha256 = input_artifact_sha256,
      params_sha256 = params_sha256
    )
  )
  preflight <- stpd_provider_normalize_batch_v1(
    records = adapter_result$records,
    coordinate_specs = adapter_result$specs,
    spine = spine, provider_run_id = preflight_id,
    source_time_resolution_sec = 0,
    processed_trains = request$selected_train_keys,
    per_isi_coverage_trains = if (isTRUE(adapter_result$per_isi)) {
      request$selected_train_keys
    } else NULL
  )
  semantic_analysis <- stpd_provider_b2_semantic_analysis(
    preflight, adapter_result$specs, adapter_result$semantics
  )
  semantic_issues <- semantic_analysis$issues
  unresolved <- c(adapter_result$unresolved, semantic_issues)
  if (isTRUE(preflight$atomic_accept) && !length(unresolved)) {
    stpd_provider_b2_check_materialized_rows(
      nrow(preflight$normalized_geometry),
      length(semantic_analysis$missing_parent_keys)
    )
  }
  run_status <- if (isTRUE(preflight$atomic_accept) && !length(unresolved)) {
    "complete"
  } else {
    "rejected"
  }
  run <- stpd_provider_b2_provider_run_row(
    adapter = adapter, request = request, spine = spine,
    run_status = run_status,
    provider_version = adapter_result$provider_version,
    information_access = adapter_result$information_access,
    input_artifact_sha256 = input_artifact_sha256,
    params_sha256 = params_sha256, raw_output_sha256 = raw_output_sha256,
    provider_code_sha256 = provider_code_sha256,
    adapter_code_sha256 = adapter_code_sha256
  )
  final_batch <- stpd_provider_normalize_batch_v1(
    records = adapter_result$records,
    coordinate_specs = adapter_result$specs,
    spine = spine, provider_run_id = run$provider_run_id[[1L]],
    source_time_resolution_sec = 0,
    processed_trains = request$selected_train_keys,
    per_isi_coverage_trains = if (isTRUE(adapter_result$per_isi)) {
      request$selected_train_keys
    } else NULL
  )
  if (!identical(final_batch$atomic_accept, preflight$atomic_accept)) {
    stpd_provider_b2_abort(
      "provider_nondeterministic_output",
      "Preflight and final normalization disagree on batch acceptance.",
      provider_run_id = run$provider_run_id[[1L]]
    )
  }
  bundle <- stpd_provider_b2_empty_bundle()
  bundle$provider_runs <- run
  bundle$normalization_audit <- final_batch$normalization_audit
  if (identical(run_status, "complete")) {
    semantics <- lapply(seq_along(adapter_result$semantics), function(i) {
      c(
        list(source_record_key = adapter_result$specs[[i]]$source_record_key),
        adapter_result$semantics[[i]]
      )
    })
    bundle$candidate_intervals <- stpd_provider_b2_materialize_candidates(
      final_batch, semantics, run$provider_run_id[[1L]], request$output_role
    )
    bundle$thresholds <- stpd_provider_b2_materialize_thresholds(
      adapter_result$thresholds, run$provider_run_id[[1L]]
    )
  }
  run_id <- run$provider_run_id[[1L]]
  bundle$provider_runs$normalized_output_sha256 <-
    stpd_provider_normalized_output_sha256_v1(bundle, run_id)
  stpd_validate_provider_bundle(bundle)
  diagnostics <- list(
    run_status = run_status,
    rejection_code = if (!isTRUE(final_batch$atomic_accept)) {
      "normalization_batch_rejected"
    } else if (length(semantic_issues)) {
      "provider_semantic_rejected"
    } else if (length(unresolved)) {
      "provider_threshold_unresolved"
    } else NA_character_,
    rejection_detail = if (length(unresolved)) {
      paste(sort(unresolved, method = "radix"), collapse = ";")
    } else NA_character_,
    protocol_sha256 = final_batch$protocol_sha256,
    coverage_sha256 = final_batch$coverage_sha256
  )
  attr(bundle, "stpd_provider_import_diagnostics") <- diagnostics
  bundle
}

stpd_import_provider_batch <- function(dataset, request, artifact = NULL) {
  request <- stpd_provider_b2_validate_request(request)
  spine <- stpd_provider_build_spine_v1(
    dataset, selected_trains = request$selected_train_keys
  )
  total_timestamps <- sum(spine$train_manifest$n_spikes)
  if (total_timestamps > STPD_PROVIDER_B2_MAX_TIMESTAMPS) {
    stpd_provider_b2_abort(
      "resource_limit_exceeded", "Timestamp count exceeds the B2 limit.",
      table = "timestamp_spine"
    )
  }
  dependency <- stpd_provider_b2_load_dependency_manifest(
    request$adapter_key, request$adapter_version
  )
  external <- request$adapter_key %in%
    c("external_interval_v1", "external_per_isi_v1")
  if (external) {
    if (length(request$parameters)) {
      stpd_provider_b2_abort(
        "unknown_column",
        "External pure-data adapters require an empty parameters object.",
        column = names(request$parameters)[[1L]]
      )
    }
    decoded <- stpd_provider_b2_decode_raw_json(artifact)
    adapter_result <- if (request$adapter_key == "external_interval_v1") {
      stpd_provider_b2_parse_external_interval(decoded$value)
    } else {
      stpd_provider_b2_parse_external_per_isi(decoded$value)
    }
    adapter_result$parameters <- list(
      artifact_schema_version = decoded$value$schema_version,
      coordinate_profile_id = adapter_result$profile_id,
      semantic_track = if (request$adapter_key == "external_per_isi_v1") {
        decoded$value$semantic_track
      } else NULL,
      target_label = if (request$adapter_key == "external_per_isi_v1") {
        decoded$value$target_label
      } else NULL
    )
    adapter_result$thresholds <- list()
    adapter_result$raw_output <- NULL
    adapter_result$unresolved <- character()
    input_artifact_sha256 <- decoded$raw_sha256
    raw_output_sha256 <- decoded$raw_sha256
    provider_code_sha256 <- stpd_provider_hash_domain(
      "stpd-external-unverified-provider-code-declaration-v1",
      list(
        provider_version = adapter_result$provider_version,
        declared_provider_code_sha256 = adapter_result$provider_code_sha256
      )
    )
  } else {
    if (!is.null(artifact)) {
      stpd_provider_b2_abort(
        "executable_import_forbidden",
        "Mean-ISI and LogISI adapters do not accept an artifact.",
        column = "artifact"
      )
    }
    adapter_result <- if (request$adapter_key == "mean_isi_v1") {
      stpd_provider_b2_run_misi(spine, request$parameters)
    } else if (request$adapter_key == "logisi_newbd_v1") {
      stpd_provider_b2_run_logisi(spine, request$parameters)
    } else {
      stpd_provider_b2_abort(
        "native_gate_b_pending", "Native adapter remains disabled at Gate B.",
        column = "adapter_key"
      )
    }
    if (length(adapter_result$records) > STPD_PROVIDER_B2_MAX_SOURCE_RECORDS ||
        as.double(utils::object.size(adapter_result$raw_output)) >
          STPD_PROVIDER_B2_MAX_INTERNAL_OUTPUT_BYTES) {
      stpd_provider_b2_abort(
        "resource_limit_exceeded", "Provider output exceeds the B2 limit.",
        table = "provider_output"
      )
    }
    input_artifact_sha256 <- stpd_provider_b2_internal_input_sha256(
      spine, request, adapter_result$parameters
    )
    raw_output_sha256 <- stpd_provider_b2_raw_json_sha256(
      adapter_result$raw_output
    )
    provider_code_sha256 <- dependency$provider_code_sha256
  }
  stpd_provider_b2_finalize_bundle(
    adapter_result = adapter_result, adapter = request$adapter,
    request = request, spine = spine,
    input_artifact_sha256 = input_artifact_sha256,
    raw_output_sha256 = raw_output_sha256,
    provider_code_sha256 = provider_code_sha256,
    adapter_code_sha256 = dependency$adapter_code_sha256
  )
}
