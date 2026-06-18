test_that("parameter YAML loads fully under a non-UTF-8 (C) locale", {
  # Regression: opening the config with a UTF-8 connection still re-encodes to
  # the native charset, so in LC_CTYPE=C the reader hit an invalid multibyte
  # byte and silently dropped eventness_schema / parameter_contract, blocking
  # package load. The raw-bytes-marked-UTF-8 reader must load all sections
  # regardless of locale.
  old <- Sys.getlocale("LC_CTYPE")
  set_ok <- tryCatch(Sys.setlocale("LC_CTYPE", "C") != "", warning = function(w) FALSE, error = function(e) FALSE)
  skip_if_not(isTRUE(set_ok), "cannot switch to C locale on this platform")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)

  required <- c("schema_version", "runtime_defaults", "product_defaults",
                "key_schema", "eventness_schema", "parameter_contract")

  read_utf8 <- getFromNamespace("stpd_read_yaml_utf8", "SpikeTrainPatternDetector")
  cfg_path <- getFromNamespace("stpd_parameter_config_path", "SpikeTrainPatternDetector")()
  cfg_direct <- read_utf8(cfg_path)
  expect_true(all(required %in% names(cfg_direct)))

  cfg <- getFromNamespace("stpd_parameter_config", "SpikeTrainPatternDetector")(reload = TRUE)
  expect_true(all(required %in% names(cfg)))
})
