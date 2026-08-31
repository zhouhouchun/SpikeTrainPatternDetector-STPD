#!/usr/bin/env Rscript

Sys.setenv(STPD_GATE_B_PHASE1_GENERATING = "1")
pkgload::load_all(".", quiet = TRUE)

as_rows <- function(x) {
  lapply(seq_len(nrow(x)), function(i) {
    lapply(x, function(column) if (is.list(column)) column[[i]] else column[[i]])
  })
}

bundle <- stpd_gate_b_v3_phase1_bundle()
registry <- stpd_gate_b_v3_phase1_registry(verify_materialized = FALSE)
fixtures <- stpd_gate_b_v3_phase1_fixture_spec(verify_materialized = FALSE)
fixture_inputs <- stpd_gate_b_v3_phase1_fixture_inputs()

registry_payload <- list(
  schema = "stpd_gate_b_v3_phase1_registry_bundle_1",
  plan_sha256 = bundle$plan_sha256,
  normative_contract_sha256 = bundle$normative_contract_sha256,
  authoritative = FALSE,
  entries = as_rows(registry)
)
fixture_payload <- list(
  schema = "stpd_gate_b_v3_phase1_fixture_spec_1",
  plan_sha256 = bundle$plan_sha256,
  normative_contract_sha256 = bundle$normative_contract_sha256,
  release_evidence = FALSE,
  fixtures = as_rows(fixtures)
)

fixture_test_paths <- unique(fixtures$repository_relative_test_path)
if (length(fixture_test_paths) != 1L ||
    !file.exists(fixture_test_paths[[1L]])) {
  stop("Gate B v3 fixture test source is missing or ambiguous.", call.=FALSE)
}
fixture_test_source <- paste(
  readLines(fixture_test_paths[[1L]], warn=FALSE, encoding="UTF-8"),
  collapse="\n"
)
fixture_test_names <- sort(unique(fixtures$test_name), method="radix")
fixture_tests_discovered <- all(vapply(
  fixture_test_names,
  function(name) grepl(
    paste0('test_that("',name,'"'), fixture_test_source, fixed=TRUE
  ),
  logical(1)
))
if (!fixture_tests_discovered) {
  stop("A declared Gate B v3 fixture test is not discoverable.", call.=FALSE)
}

# The executable schema bundle is the single phase-1 authority.  Embed the
# complete active semantic registry and its own domain-separated content hash;
# the separately materialized registry JSON is a byte-preserving convenience
# artifact, not a second authority.
bundle$registry_semantics <- registry_payload$entries
bundle$fixture_test_discovery <- list(
  schema="stpd_gate_b_v3_fixture_test_discovery_1",
  repository_relative_test_path=fixture_test_paths[[1L]],
  source_test_sha256=digest::digest(
    fixture_test_paths[[1L]],algo="sha256",file=TRUE
  ),
  declared_test_names=as.list(fixture_test_names),
  all_declared_tests_discovered=TRUE
)
bundle$schema_contract_sha256 <- NULL
bundle$schema_contract_sha256 <- stpd_gate_b_v3_hash_raw(
  "stpd-gate-b-v3-schema-contract-v1",
  getFromNamespace("stpd_gate_b_v3_canonical_json",
                   "SpikeTrainPatternDetector")(bundle)
)
jsonlite::write_json(
  bundle, "inst/config/gate_b_v3_phase1_schema_bundle.json",
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
)

jsonlite::write_json(
  registry_payload, "inst/config/gate_b_v3_phase1_registry.json",
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
)
jsonlite::write_json(
  fixture_payload, "inst/config/gate_b_v3_phase1_fixture_spec.json",
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
)
jsonlite::write_json(
  list(schema="stpd_gate_b_v3_phase1_fixture_inputs_1",
       plan_sha256=bundle$plan_sha256,
       normative_contract_sha256=bundle$normative_contract_sha256,
       inputs=fixture_inputs),
  "inst/config/gate_b_v3_phase1_fixture_inputs.json",
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
)
cat("inst/config/gate_b_v3_phase1_registry.json\n")
cat("inst/config/gate_b_v3_phase1_fixture_spec.json\n")
cat("inst/config/gate_b_v3_phase1_fixture_inputs.json\n")
Sys.unsetenv("STPD_GATE_B_PHASE1_GENERATING")
