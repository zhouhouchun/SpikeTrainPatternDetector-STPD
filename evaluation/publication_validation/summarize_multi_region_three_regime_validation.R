#!/usr/bin/env Rscript

# Combine the frozen three-region/three-regime validation outputs into a
# publication-facing F1 table.  This script never reruns or modifies detection.

options(stringsAsFactors = FALSE)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
root <- file.path(repo, "test-results", "multi_region_three_regime_20260831")
regions <- c("GPE", "STN", "GPI")
regimes <- c("automatic", "partial_known", "full_params")

read_one <- function(region, regime) {
  path <- file.path(root, region, regime)
  required <- c(
    "pooled_observed_metrics.csv", "cluster_bootstrap_95ci.csv", "protocol.csv"
  )
  missing <- required[!file.exists(file.path(path, required))]
  if (length(missing)) {
    stop(region, "/", regime, " is incomplete: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  metric <- read.csv(file.path(path, required[[1L]]), check.names = FALSE)
  ci <- read.csv(file.path(path, required[[2L]]), check.names = FALSE)
  protocol <- read.csv(file.path(path, required[[3L]]), check.names = FALSE)
  ci_f1 <- ci[ci$metric == "F1", c(
    "level", "axis", "label", "ci_low", "ci_high", "bootstrap_n", "ci_method"
  )]
  out <- merge(metric, ci_f1, by = c("level", "axis", "label"), all.x = TRUE)
  out$region <- region
  out$regime <- regime
  out$interpretation <- protocol$interpretation[[1L]]
  out$tonic_reference_role <- protocol$tonic_reference_role[[1L]]
  out
}

all_metrics <- do.call(rbind, unlist(lapply(regions, function(region) {
  lapply(regimes, function(regime) read_one(region, regime))
}), recursive = FALSE))

is_primary <-
  (all_metrics$axis == "event" & all_metrics$label %in% c("burst", "pause")) |
  (all_metrics$axis == "state_hf_family" &
     all_metrics$label == "high_frequency_spiking") |
  (all_metrics$region == "GPI" & all_metrics$axis == "state_strict" &
     all_metrics$label == "tonic")
primary <- all_metrics[is_primary & all_metrics$level %in% c("interval", "event"), ]
primary$pattern <- ifelse(
  primary$label == "high_frequency_spiking", "Broad HFS",
  ifelse(primary$label == "burst", "Burst",
         ifelse(primary$label == "pause", "Pause", "Tonic"))
)
primary <- primary[order(
  match(primary$region, regions), match(primary$regime, regimes),
  match(primary$level, c("interval", "event")),
  match(primary$pattern, c("Burst", "Pause", "Broad HFS", "Tonic"))
), ]

write.csv(all_metrics, file.path(root, "combined_metrics_with_ci.csv"), row.names = FALSE)
write.csv(primary, file.path(root, "primary_f1_summary.csv"), row.names = FALSE)

fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))
lines <- c(
  "# PD GPe/STN/GPi three-regime validation",
  "",
  "Automatic uses no manual examples. Partial-known is five-fold held-out",
  "recording-group validation with at most ten training episodes per pattern.",
  "Full-parameters is same-data resubstitution and is only an adaptation upper bound.",
  "GPi Tonic is a formal State endpoint; STN Tonic is descriptive review-only.",
  "",
  "| Region | Regime | Level | Pattern | Precision | Recall | F1 (95% cluster CI) | Support |",
  "|---|---|---|---|---:|---:|---:|---:|"
)
for (ii in seq_len(nrow(primary))) {
  z <- primary[ii, ]
  ci <- paste0(fmt(z$F1), " (", fmt(z$ci_low), "–", fmt(z$ci_high), ")")
  lines <- c(lines, paste(
    "|", z$region, "|", z$regime, "|", z$level, "|", z$pattern, "|",
    fmt(z$precision), "|", fmt(z$recall), "|", ci, "|", z$support, "|"
  ))
}
writeLines(lines, file.path(root, "primary_f1_summary.md"), useBytes = TRUE)
cat("Wrote combined validation summary to", root, "\n")
