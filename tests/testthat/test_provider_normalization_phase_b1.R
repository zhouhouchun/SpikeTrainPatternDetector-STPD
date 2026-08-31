b1_internal <- function(name) {
  getFromNamespace(name, "SpikeTrainPatternDetector")
}

b1_sha <- function(letter = "a") paste(rep(letter, 64L), collapse = "")

b1_dataset <- function(timestamps = c(0, 0.1, 0.2, 0.3), train = "t1") {
  trains <- list(data.frame(timestamp_sec = as.double(timestamps)))
  names(trains) <- train
  make_dataset("b1", "synthetic", trains)
}

b1_spec <- function(
    key = "r1", train = "t1",
    convention = "diff_timestamp_isi_index", base = "one",
    closure = "closed", unit = "not_applicable",
    start_index = 1, end_index = 1,
    start_time = NA_real_, end_time = NA_real_,
    transformation = "diff_one_based_plus_one") {
  list(
    source_record_key = key,
    train_key = train,
    source_coordinate_convention = convention,
    source_index_base = base,
    source_interval_closure = closure,
    source_time_unit = unit,
    source_start_index = start_index,
    source_end_index = end_index,
    source_start_time = start_time,
    source_end_time = end_time,
    transformation = transformation
  )
}

b1_record <- function(key = "r1", start = 1L, end = 1L,
                      label = "burst") {
  list(record_key = key, start = start, end = end, label = label)
}

b1_expect_error <- function(expr, code) {
  error <- tryCatch(force(expr), error = function(e) e)
  expect_s3_class(error, "stpd_provider_contract_error")
  expect_true(inherits(error, code))
  expect_identical(error$code, code)
  invisible(error)
}

test_that("B1 companion leaves the frozen Phase-A contract unchanged", {
  expect_identical(
    stpd_provider_contract_hash(),
    "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"
  )
  protocol <- b1_internal("stpd_provider_adapter_protocol_v1")()
  expect_identical(protocol$protocol_version,
                   "stpd_provider_adapter_protocol_v1")
  expect_identical(
    b1_internal("stpd_provider_adapter_protocol_hash_v1")(),
    "5b0cc6ca2fe6bdf2bb7bd2bddb066632ea3b0646b1096be87c77bec292a26877"
  )
  expect_identical(
    protocol$hash_primitive$preimage,
    "UTF8(domain) || 0x00 || UTF8(canonical_json(payload))"
  )
  expect_false(protocol$time_alignment$nearest_snap)
  expect_true(protocol$transaction$batch_atomic_accept_requires_all_normalized)
})

test_that("timestamp leaf uses frozen binary64 little-endian bytes", {
  leaf <- b1_internal("stpd_provider_train_timestamp_sha256_v1")
  expect_identical(
    leaf(double()),
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  )
  expect_identical(
    leaf(0),
    "af5570f5a1810b7af78caf4bc70a660f0df51e42baf91d4de5b2328de0e83dfc"
  )
  hash <- leaf(c(0, 0.1, 0.2, 0.3))
  expect_identical(
    hash,
    "1f8ca26fda2a620f4272dfe8264145b694be8cac4a09125b5fccc32450cbccd1"
  )
  connection <- rawConnection(raw(), "wb")
  writeBin(c(0, 0.1, 0.2, 0.3), connection, size = 8L, endian = "little")
  bytes <- rawConnectionValue(connection)
  close(connection)
  expect_identical(hash, digest::digest(bytes, "sha256", serialize = FALSE))
})

test_that("spine validates original order without repair", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  spine <- build(b1_dataset())
  expect_s3_class(spine, "stpd_provider_spine_v1")
  expect_identical(spine$timestamps$t1, c(0, 0.1, 0.2, 0.3))
  expect_equal(spine$train_manifest$n_spikes, 4L)
  expect_match(spine$timestamp_spine_sha256, "^[0-9a-f]{64}$")
  expect_match(spine$dataset_snapshot_sha256, "^[0-9a-f]{64}$")
  expect_identical(
    spine$timestamp_spine_sha256,
    "34f09f001cb4848ad14623f73ba01dd403dee599f2d9b4cf0c3ddeaba63f44e5"
  )
  expect_identical(
    spine$dataset_snapshot_sha256,
    "dceced8315cdaaf5b9bed7450c49ab5ada83d0d737de2331e4c8767ea7c3f994"
  )
  expect_identical(
    spine$acquisition_context_sha256,
    "7d8bf6bf4dac8e6ba54c19ba576dbd21d3a54b82b9c22117d6055f6540439051"
  )
  expect_identical(
    spine$qc_policy_sha256,
    "babd38b1109e862fc3a986e1b473cd3c0f395a19e01b4a785773aa5639d23bbe"
  )

  b1_expect_error(build(b1_dataset(c(0, 0.1, 0.1))),
                  "timestamp_duplicate")
  b1_expect_error(build(b1_dataset(c(0, 0.2, 0.1))),
                  "timestamp_non_monotonic")
  b1_expect_error(build(b1_dataset(c(0, Inf))),
                  "coordinate_non_finite")
  b1_expect_error(build(b1_dataset(c(0, -0.0, 0.1))),
                  "coordinate_non_finite")
})

test_that("timestamp manifest uses exact UTF-8 bytewise train ordering", {
  trains <- list(
    data.frame(timestamp_sec = c(0, 0.1)),
    data.frame(timestamp_sec = c(0, 0.2)),
    data.frame(timestamp_sec = c(0, 0.3))
  )
  names(trains) <- c("z", "é", "aa")
  spine <- b1_internal("stpd_provider_build_spine_v1")(
    make_dataset("utf8", "synthetic", trains)
  )
  expected <- c("aa", "z", "é")
  expect_identical(spine$train_manifest$train_key, expected)
  expect_identical(
    spine$timestamp_spine_sha256,
    "218765546ab8f21010bf661eb0f8ce76ced7443475206a85cbff987df59bd508"
  )
})

test_that("snapshot identity is train-order invariant and excludes mutable labels", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  trains_a <- list(
    b = data.frame(timestamp_sec = c(0, 0.2)),
    a = data.frame(timestamp_sec = c(0, 0.1))
  )
  trains_b <- trains_a[c("a", "b")]
  ds_a <- make_dataset("order-a", "synthetic", trains_a)
  ds_b <- make_dataset("order-b", "synthetic", trains_b)
  ds_b$manual <- list(secret_reference = "must-not-enter-provider-identity")
  ds_b$review <- list(decision = "changed")
  ds_b$created_at <- "2099-01-01T00:00:00Z"
  a <- build(ds_a)
  b <- build(ds_b)
  expect_identical(a$timestamp_spine_sha256, b$timestamp_spine_sha256)
  expect_identical(a$dataset_snapshot_sha256, b$dataset_snapshot_sha256)

  with_context <- build(ds_a, acquisition_context_sha256 = b1_sha("b"))
  with_qc <- build(ds_a, qc_policy_sha256 = b1_sha("c"))
  expect_false(identical(a$dataset_snapshot_sha256,
                         with_context$dataset_snapshot_sha256))
  expect_false(identical(a$dataset_snapshot_sha256,
                         with_qc$dataset_snapshot_sha256))
})

test_that("empty and one-spike trains have identity but no ISI support", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  one <- b1_internal("stpd_provider_normalize_one_v1")
  for (timestamps in list(double(), 0)) {
    spine <- build(b1_dataset(timestamps))
    expect_equal(spine$train_manifest$n_spikes, length(timestamps))
    out <- one(b1_record(), b1_spec(), spine, b1_sha())
    expect_identical(out$audit$normalization_status, "rejected")
    expect_identical(out$audit$error_code, "coordinate_out_of_range")
    expect_null(out$geometry)
  }
})

test_that("all frozen index conventions map first and last ISI exactly", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(
    b1_dataset(seq(0, 0.5, by = 0.1))
  )
  one <- b1_internal("stpd_provider_normalize_one_v1")
  cases <- list(
    b1_spec(convention = "train_row_isi_index", base = "one",
            start_index = 2, end_index = 6,
            transformation = "identity_train_row_one_based"),
    b1_spec(start_index = 1, end_index = 5),
    b1_spec(convention = "diff_timestamp_isi_index", base = "zero",
            start_index = 0, end_index = 4,
            transformation = "diff_zero_based_plus_two"),
    b1_spec(convention = "spike_index_span", base = "one",
            start_index = 1, end_index = 6,
            transformation = "spike_one_based_span_to_isi"),
    b1_spec(convention = "spike_index_span", base = "zero",
            start_index = 0, end_index = 5,
            transformation = "spike_zero_based_span_to_isi"),
    b1_spec(convention = "per_isi_diff_index", base = "one",
            start_index = 1, end_index = 5,
            transformation = "per_isi_one_based_runs_plus_one"),
    b1_spec(convention = "per_isi_diff_index", base = "zero",
            start_index = 0, end_index = 4,
            transformation = "per_isi_zero_based_runs_plus_two")
  )
  for (i in seq_along(cases)) {
    cases[[i]]$source_record_key <- paste0("r", i)
    out <- one(b1_record(paste0("r", i)), cases[[i]], spine, b1_sha())
    expect_identical(out$geometry$start_isi, 2L)
    expect_identical(out$geometry$end_isi, 6L)
    expect_identical(out$geometry$start_spike, 1L)
    expect_identical(out$geometry$end_spike, 6L)
    expect_equal(out$geometry$start_time_sec, 0)
    expect_equal(out$geometry$end_time_sec, 0.5)
  }
})

test_that("integer closure is applied before coordinate transformation", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(
    b1_dataset(seq(0, 0.5, by = 0.1))
  )
  one <- b1_internal("stpd_provider_normalize_one_v1")
  expected <- list(
    closed = c(2L, 5L),
    left_closed_right_open = c(2L, 4L),
    left_open_right_closed = c(3L, 5L),
    open = c(3L, 4L)
  )
  i <- 0L
  for (closure in names(expected)) {
    i <- i + 1L
    spec <- b1_spec(key = paste0("c", i), closure = closure,
                    start_index = 1, end_index = 4)
    out <- one(b1_record(paste0("c", i)), spec, spine, b1_sha())
    expect_identical(
      c(out$geometry$start_isi, out$geometry$end_isi), expected[[closure]]
    )
  }

  empty <- one(
    b1_record("empty"),
    b1_spec(key = "empty", closure = "left_closed_right_open",
            start_index = 1, end_index = 1),
    spine, b1_sha()
  )
  expect_identical(empty$audit$error_code, "coordinate_empty")

  singleton <- one(
    b1_record("single"), b1_spec(key = "single", start_index = 1,
                                 end_index = 1),
    spine, b1_sha()
  )
  expect_identical(singleton$geometry$start_isi, 2L)
  expect_identical(singleton$geometry$end_isi, 2L)

  one_spike_span <- one(
    b1_record("one_spike"),
    b1_spec(key = "one_spike", convention = "spike_index_span",
            base = "one", start_index = 2, end_index = 2,
            transformation = "spike_one_based_span_to_isi"),
    spine, b1_sha()
  )
  expect_identical(one_spike_span$audit$error_code, "coordinate_empty")

  noninteger <- one(
    b1_record("noninteger"),
    b1_spec(key = "noninteger", start_index = 1.5, end_index = 2),
    spine, b1_sha()
  )
  expect_identical(noninteger$audit$error_code, "coordinate_non_integer")

  reversed <- one(
    b1_record("reversed"),
    b1_spec(key = "reversed", start_index = 3, end_index = 2),
    spine, b1_sha()
  )
  expect_identical(reversed$audit$error_code, "coordinate_reversed")
})

test_that("time spans require unique s, ms, or us boundary matches", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  one <- b1_internal("stpd_provider_normalize_one_v1")
  units <- list(s = c(0.1, 0.3), ms = c(100, 300), us = c(100000, 300000))
  i <- 0L
  for (unit in names(units)) {
    i <- i + 1L
    value <- units[[unit]]
    spec <- b1_spec(
      key = paste0("time", i), convention = "spike_time_span",
      base = "not_applicable", unit = unit,
      start_index = NA_real_, end_index = NA_real_,
      start_time = value[[1L]], end_time = value[[2L]],
      transformation = "exact_spike_time_span_to_isi"
    )
    out <- one(b1_record(paste0("time", i)), spec, spine, b1_sha())
    expect_identical(out$geometry$start_isi, 3L)
    expect_identical(out$geometry$end_isi, 4L)
    expect_equal(out$geometry$start_time_sec, 0.1)
    expect_equal(out$geometry$end_time_sec, 0.3)
  }

  missing <- one(
    b1_record("missing"),
    b1_spec(
      key = "missing", convention = "spike_time_span",
      base = "not_applicable", unit = "s",
      start_index = NA_real_, end_index = NA_real_,
      start_time = 0.11, end_time = 0.3,
      transformation = "exact_spike_time_span_to_isi"
    ),
    spine, b1_sha()
  )
  expect_identical(missing$audit$error_code, "time_alignment_missing")

  ambiguous <- one(
    b1_record("ambiguous"),
    b1_spec(
      key = "ambiguous", convention = "spike_time_span",
      base = "not_applicable", unit = "s",
      start_index = NA_real_, end_index = NA_real_,
      start_time = 0.05, end_time = 0.3,
      transformation = "exact_spike_time_span_to_isi"
    ),
    spine, b1_sha(), source_time_resolution_sec = 0.1
  )
  expect_identical(ambiguous$audit$error_code, "time_alignment_ambiguous")
  expect_true(ambiguous$audit$ambiguity_detected)

  unsafe_tolerance <- one(
    b1_record("unsafe_tolerance"),
    b1_spec(
      key = "unsafe_tolerance", convention = "spike_time_span",
      base = "not_applicable", unit = "s",
      start_index = NA_real_, end_index = NA_real_,
      start_time = 0.1, end_time = 0.3,
      transformation = "exact_spike_time_span_to_isi"
    ),
    spine, b1_sha(), source_time_resolution_sec = 0.1
  )
  expect_identical(unsafe_tolerance$audit$error_code,
                   "time_tolerance_invalid")

  same_spike <- one(
    b1_record("same"),
    b1_spec(
      key = "same", convention = "spike_time_span",
      base = "not_applicable", unit = "s",
      start_index = NA_real_, end_index = NA_real_,
      start_time = 0.2, end_time = 0.2,
      transformation = "exact_spike_time_span_to_isi"
    ),
    spine, b1_sha()
  )
  expect_identical(same_spike$audit$error_code, "coordinate_empty")
})

test_that("normalization audit identities are frozen and Phase-A compatible", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  one <- b1_internal("stpd_provider_normalize_one_v1")
  good <- one(b1_record("good"), b1_spec(key = "good"), spine, b1_sha())
  bad <- one(
    b1_record("bad", 99L, 99L),
    b1_spec(key = "bad", start_index = 99, end_index = 99),
    spine, b1_sha()
  )
  time_us <- one(
    b1_record("time_us", 100000L, 300000L),
    b1_spec(
      key = "time_us", convention = "spike_time_span",
      base = "not_applicable", unit = "us",
      start_index = NA_real_, end_index = NA_real_,
      start_time = 100000, end_time = 300000,
      transformation = "exact_spike_time_span_to_isi"
    ),
    spine, b1_sha()
  )
  expect_identical(
    good$audit$normalization_audit_id,
    "0c8173d0c0b9639f1eb50e1c967b055cf9929745cb1f51e8c441693b24d5620f"
  )
  expect_identical(
    bad$audit$normalization_audit_id,
    "58b2bfd0e75b36fbe6cce39316e5444a75557de88c45637100d714fe66b0fb8f"
  )
  expect_identical(
    time_us$audit$normalization_audit_id,
    "d7419541570d91b79fca36df9049b74c74b8acf93fbd1f305cd30e2f4b99602c"
  )
  expect_identical(time_us$audit$tolerance_sec, 8 * .Machine$double.eps)

  audit_id <- b1_internal("stpd_provider_normalization_audit_id_v1")
  detail_changed <- good$audit
  detail_changed$error_detail <- "non-authoritative diagnostic text"
  expect_identical(audit_id(detail_changed),
                   good$audit$normalization_audit_id)
  authoritative_changed <- good$audit
  authoritative_changed$tolerance_sec <- 1e-9
  expect_false(identical(audit_id(authoritative_changed),
                         good$audit$normalization_audit_id))

  bundle <- stpd_provider_bundle_prototypes()
  bundle$normalization_audit <- dplyr::bind_rows(
    good$audit, bad$audit, time_us$audit
  )
  expect_invisible(b1_internal("stpd_provider_validate_audit")(
    bundle, stpd_provider_contract_registry()
  ))
})

test_that("per-ISI input requires exact n-minus-one singleton coverage", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  specs <- lapply(seq_len(3L), function(i) {
    b1_spec(
      key = paste0("p", i), convention = "per_isi_diff_index",
      base = "one", start_index = i, end_index = i,
      transformation = "per_isi_one_based_runs_plus_one"
    )
  })
  records <- lapply(seq_len(3L), function(i) b1_record(paste0("p", i), i, i))
  out <- batch(records, specs, spine, b1_sha(), processed_trains = "t1",
               per_isi_coverage_trains = "t1")
  expect_true(out$atomic_accept)
  expect_equal(nrow(out$normalized_geometry), 3L)
  expect_identical(out$normalized_geometry$canonical_start_isi,
                   out$normalized_geometry$canonical_end_isi)

  b1_expect_error(
    batch(records[-3L], specs[-3L], spine, b1_sha(),
          processed_trains = "t1",
          per_isi_coverage_trains = "t1"),
    "coordinate_roundtrip_mismatch"
  )
  b1_expect_error(
    batch(c(records, list(b1_record("p4", 4, 4))),
          c(specs, list(b1_spec(
            key = "p4", convention = "per_isi_diff_index", base = "one",
            start_index = 4, end_index = 4,
            transformation = "per_isi_one_based_runs_plus_one"
          ))), spine, b1_sha(), processed_trains = "t1",
          per_isi_coverage_trains = "t1"),
    "coordinate_roundtrip_mismatch"
  )
  duplicate <- specs
  duplicate[[3L]]$source_start_index <- 2
  duplicate[[3L]]$source_end_index <- 2
  b1_expect_error(
    batch(records, duplicate, spine, b1_sha(), processed_trains = "t1",
          per_isi_coverage_trains = "t1"),
    "coordinate_roundtrip_mismatch"
  )
})

test_that("a record rejection prevents partial normalized geometry", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  records <- list(b1_record("good"), b1_record("bad", 99, 99))
  specs <- list(
    b1_spec(key = "good", start_index = 1, end_index = 1),
    b1_spec(key = "bad", start_index = 99, end_index = 99)
  )
  records_before <- serialize(records, NULL, version = 3L)
  specs_before <- serialize(specs, NULL, version = 3L)
  out <- batch(records, specs, spine, b1_sha(), processed_trains = "t1")
  expect_false(out$atomic_accept)
  expect_equal(nrow(out$normalization_audit), 2L)
  expect_setequal(out$normalization_audit$normalization_status,
                  c("normalized", "rejected"))
  expect_equal(nrow(out$normalized_geometry), 0L)
  expect_identical(serialize(records, NULL, version = 3L), records_before)
  expect_identical(serialize(specs, NULL, version = 3L), specs_before)

  repeat_out <- batch(records, specs, spine, b1_sha(),
                      processed_trains = "t1")
  expect_identical(serialize(repeat_out, NULL, version = 3L),
                   serialize(out, NULL, version = 3L))
})

test_that("successful and rejected audits satisfy the frozen Phase-A validator", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  out <- batch(
    list(b1_record("good"), b1_record("bad", 99, 99)),
    list(b1_spec(key = "good"), b1_spec(key = "bad", start_index = 99,
                                        end_index = 99)),
    spine, b1_sha(), processed_trains = "t1"
  )
  bundle <- stpd_provider_bundle_prototypes()
  bundle$normalization_audit <- out$normalization_audit
  expect_invisible(b1_internal("stpd_provider_validate_audit")(
    bundle, stpd_provider_contract_registry()
  ))
})

test_that("source record identity is typed, ordered, and pure-data only", {
  hash <- b1_internal("stpd_provider_source_record_sha256_v1")
  a <- list(x = 1L, y = 1.0, z = "burst")
  expect_identical(
    hash(a),
    "86b263d9afdda2eb5b0a11e8d17cffd823955b921059d4c0e0fa8773ca5292c1"
  )
  expect_false(identical(hash(a), hash(list(y = 1.0, x = 1L, z = "burst"))))
  expect_false(identical(hash(a), hash(list(x = 1.0, y = 1.0, z = "burst"))))
  expect_identical(
    hash(list(x = NULL)),
    "496661ba1cb2dee3c287017a59f77b7f4da8fe9a2e86e1bb9f087c349313a9fc"
  )
  expect_identical(
    hash(list(x = NA_integer_)),
    "7dda0b9ab80b028c397be20a683c381f10abb1c74f24cc6dd6653ea91a73c33d"
  )
  expect_identical(
    hash(list(x = "")),
    "d70c0c00bdc8afcf1afc3f0bce48b4b1965d57374bd968bbcf23e65f7d179161"
  )
  expect_false(identical(hash(list(x = NULL)), hash(list(x = NA_integer_))))
  b1_expect_error(hash(list(x = function() NULL)),
                  "executable_import_forbidden")
  b1_expect_error(hash(list(x = integer())), "executable_import_forbidden")
  b1_expect_error(hash(list(x = c(1L, 2L))),
                  "executable_import_forbidden")
})

test_that("pure-data boundaries reject classed inputs without S3 dispatch", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  hash <- b1_internal("stpd_provider_source_record_sha256_v1")
  dispatched <- FALSE
  names.hostile_provider_input <- function(x) {
    dispatched <<- TRUE
    stop("must not dispatch")
  }
  hostile_record <- structure(
    list(x = 1L), class = "hostile_provider_input"
  )
  b1_expect_error(hash(hostile_record), "executable_import_forbidden")
  expect_false(dispatched)

  hostile_train <- b1_dataset()$trains$t1
  class(hostile_train) <- c("hostile_provider_input", "data.frame")
  hostile_ds <- b1_dataset()
  hostile_ds$trains$t1 <- hostile_train
  b1_expect_error(build(hostile_ds), "type_invalid")
  expect_false(dispatched)

  factor_ds <- b1_dataset()
  factor_ds$trains$t1$timestamp_sec <- factor(c("0", "0.1", "0.2", "0.3"))
  b1_expect_error(build(factor_ds), "type_invalid")
  date_ds <- b1_dataset()
  date_ds$trains$t1$timestamp_sec <- as.Date("2026-01-01") + 0:3
  b1_expect_error(build(date_ds), "type_invalid")
  rm(names.hostile_provider_input)
})

test_that("run context verifies actual spine and dataset identities", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  verify <- b1_internal("stpd_provider_verify_run_context_v1")
  row <- list(
    provider_run_id = b1_sha(),
    dataset_snapshot_sha256 = spine$dataset_snapshot_sha256,
    timestamp_spine_sha256 = spine$timestamp_spine_sha256,
    contract_sha256 = stpd_provider_contract_hash()
  )
  expect_invisible(verify(row, spine))
  bad <- row
  bad$timestamp_spine_sha256 <- b1_sha("b")
  b1_expect_error(verify(bad, spine), "train_timestamp_hash_mismatch")
  bad <- row
  bad$dataset_snapshot_sha256 <- b1_sha("c")
  b1_expect_error(verify(bad, spine), "dataset_snapshot_mismatch")
})

test_that("systemic declaration and source-key errors fail before output", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  specs <- list(b1_spec(key = "same"), b1_spec(key = "same", start_index = 2,
                                               end_index = 2))
  b1_expect_error(
    batch(list(b1_record("a"), b1_record("b")), specs, spine, b1_sha(),
          processed_trains = "t1"),
    "primary_key_duplicate"
  )
  point <- b1_spec(closure = "point")
  b1_expect_error(
    batch(list(b1_record()), list(point), spine, b1_sha(),
          processed_trains = "t1"),
    "interval_closure_missing"
  )
})

test_that("coordinate scalars are strict and integer offsets cannot overflow", {
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  one <- b1_internal("stpd_provider_normalize_one_v1")
  character_index <- one(
    b1_record("character"),
    b1_spec(key = "character", start_index = "1", end_index = "1"),
    spine, b1_sha()
  )
  expect_identical(character_index$audit$error_code, "type_invalid")
  logical_index <- one(
    b1_record("logical"),
    b1_spec(key = "logical", start_index = TRUE, end_index = TRUE),
    spine, b1_sha()
  )
  expect_identical(logical_index$audit$error_code, "type_invalid")
  maximum_index <- one(
    b1_record("maximum"),
    b1_spec(key = "maximum", start_index = .Machine$integer.max,
            end_index = .Machine$integer.max),
    spine, b1_sha()
  )
  expect_identical(maximum_index$audit$error_code,
                   "coordinate_out_of_range")
})

test_that("empty batches still verify spine, resolution, and full coverage", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  spine <- build(b1_dataset())
  out <- batch(list(), list(), spine, b1_sha(), processed_trains = "t1")
  expect_true(out$atomic_accept)
  expect_identical(
    out$coverage_sha256,
    "e05615a412fccfefd26cc38d7d9fb5b356260b72a88b0e512729f4fb404e0daa"
  )
  expect_equal(nrow(out$normalization_audit), 0L)
  expect_equal(nrow(out$normalized_geometry), 0L)

  b1_expect_error(
    batch(list(), list(), list(train_manifest = data.frame()), b1_sha(),
          processed_trains = "t1"),
    "type_invalid"
  )
  b1_expect_error(
    batch(list(), list(), spine, b1_sha(), source_time_resolution_sec = "0",
          processed_trains = "t1"),
    "time_tolerance_invalid"
  )
  b1_expect_error(
    batch(list(), list(), spine, b1_sha()),
    "coordinate_roundtrip_mismatch"
  )

  one_spike <- build(b1_dataset(0))
  per_isi_empty <- batch(
    list(), list(), one_spike, b1_sha(), processed_trains = "t1",
    per_isi_coverage_trains = "t1"
  )
  expect_true(per_isi_empty$atomic_accept)
  b1_expect_error(
    batch(list(), list(), spine, b1_sha(), processed_trains = "t1",
          per_isi_coverage_trains = "t1"),
    "coordinate_roundtrip_mismatch"
  )
})

test_that("spine and provider-run preflights fail with typed errors", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  verify <- b1_internal("stpd_provider_verify_run_context_v1")
  spine <- build(b1_dataset())
  tampered <- spine
  tampered$train_manifest$protocol_version[[1L]] <- NA_character_
  b1_expect_error(
    batch(list(), list(), tampered, b1_sha(), processed_trains = "t1"),
    "type_invalid"
  )
  b1_expect_error(build(b1_dataset(), selected_trains = character()),
                  "type_invalid")

  row <- data.frame(
    provider_run_id = rep(b1_sha(), 2L),
    dataset_snapshot_sha256 = rep(spine$dataset_snapshot_sha256, 2L),
    timestamp_spine_sha256 = rep(spine$timestamp_spine_sha256, 2L),
    contract_sha256 = rep(stpd_provider_contract_hash(), 2L),
    stringsAsFactors = FALSE
  )
  b1_expect_error(verify(row, spine), "type_invalid")
})

test_that("processed-train coverage equals the selected timestamp spine", {
  build <- b1_internal("stpd_provider_build_spine_v1")
  batch <- b1_internal("stpd_provider_normalize_batch_v1")
  trains <- list(
    t1 = data.frame(timestamp_sec = c(0, 0.1)),
    t2 = data.frame(timestamp_sec = c(0, 0.2))
  )
  spine <- build(make_dataset("coverage", "synthetic", trains))
  b1_expect_error(
    batch(list(), list(), spine, b1_sha(), processed_trains = "t1"),
    "coordinate_roundtrip_mismatch"
  )
  subset_spine <- build(
    make_dataset("coverage", "synthetic", trains), selected_trains = "t1"
  )
  expect_true(batch(list(), list(), subset_spine, b1_sha(),
                    processed_trains = "t1")$atomic_accept)
})

test_that("atomic rejection materializes as a valid five-table provider bundle", {
  version <- stpd_provider_contract_versions()[["bundle"]]
  prototypes <- stpd_provider_bundle_prototypes()
  spine <- b1_internal("stpd_provider_build_spine_v1")(b1_dataset())
  run_id <- b1_internal("stpd_provider_run_id_v1")
  output_hash <- b1_internal("stpd_provider_normalized_output_sha256_v1")

  run <- prototypes$provider_runs[0L, , drop = FALSE]
  run[1L, ] <- list(
    version, NA_character_, "mean_isi", "mean_isi", "1.0.0",
    "mean_isi_v1", "1.0.0", "automatic_prediction", "none_candidate",
    "external_only", "label_blind", "burst_interval_threshold_v1",
    "rejected", spine$dataset_snapshot_sha256, b1_sha("b"),
    spine$timestamp_spine_sha256, b1_sha("d"), NA_character_, b1_sha("e"),
    b1_sha("f"), b1_sha("1"), b1_sha("2"),
    stpd_provider_contract_hash(), "2026-08-27T00:00:00Z"
  )
  run$provider_run_id <- run_id(run)

  normalized <- b1_internal("stpd_provider_normalize_batch_v1")(
    list(b1_record("good"), b1_record("bad", 99L, 99L)),
    list(
      b1_spec(key = "good"),
      b1_spec(key = "bad", start_index = 99, end_index = 99)
    ),
    spine, run$provider_run_id, processed_trains = "t1"
  )
  expect_false(normalized$atomic_accept)
  expect_equal(nrow(normalized$normalized_geometry), 0L)

  bundle <- list(
    provider_runs = run,
    candidate_intervals = prototypes$candidate_intervals,
    thresholds = prototypes$thresholds,
    normalization_audit = normalized$normalization_audit,
    calibration_lineage = prototypes$calibration_lineage
  )
  bundle$provider_runs$normalized_output_sha256 <- output_hash(
    bundle, bundle$provider_runs$provider_run_id
  )
  expect_identical(stpd_validate_provider_bundle(bundle), TRUE)
})
