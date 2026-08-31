#!/usr/bin/env Rscript

# Build a compact, R-generated publication figure and per-method cluster
# bootstrap confidence intervals for the direct Mean-ISI versus LogISI/newBD
# Burst comparison.

options(stringsAsFactors = FALSE)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(file.path(dirname(script_path), "..", ".."))
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
out_dir <- file.path(repo, "test-results", "method_comparison",
                     "meanisi_vs_logisi_burst")
stopifnot(dir.exists(out_dir))

metrics <- read.csv(file.path(out_dir, "support_metrics.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE)
draws <- readRDS(file.path(out_dir, "paired_cluster_bootstrap_draws.rds"))

group_columns <- c("dataset", "region", "estimand", "scale", "variant")
groups <- unique(draws[group_columns])
ci_rows <- list()
for (ii in seq_len(nrow(groups))) {
  group <- groups[ii, , drop = FALSE]
  keep <- rep(TRUE, nrow(draws))
  for (name in names(group)) keep <- keep & draws[[name]] == group[[name]]
  z <- draws[keep, , drop = FALSE]
  for (method in c("Mean-ISI", "LogISI/newBD")) {
    prefix <- if (method == "Mean-ISI") "meanisi" else "logisi"
    for (metric in c("precision", "recall", "f1")) {
      values <- z[[paste0(prefix, "_", metric)]]
      values <- values[is.finite(values)]
      ci <- if (length(values)) stats::quantile(
        values, c(.025, .975), names = FALSE, type = 6
      ) else c(NA_real_, NA_real_)
      observed_row <- metrics[
        metrics$dataset == group$dataset &
          metrics$region == group$region &
          metrics$estimand == group$estimand &
          metrics$scale == group$scale &
          metrics$variant == group$variant &
          metrics$method == method, , drop = FALSE
      ]
      ci_rows[[length(ci_rows) + 1L]] <- data.frame(
        group, method = method, metric = metric,
        observed = if (nrow(observed_row)) observed_row[[metric]][[1L]] else NA_real_,
        bootstrap_mean = if (length(values)) mean(values) else NA_real_,
        ci_low = ci[[1L]], ci_high = ci[[2L]], bootstrap_n = length(values),
        bootstrap_unit = if (group$region == "synthetic")
          "Template_ID" else "train",
        stringsAsFactors = FALSE
      )
    }
  }
}
ci_table <- do.call(rbind, ci_rows)
write.csv(ci_table, file.path(out_dir, "cluster_bootstrap_method_metrics_95ci.csv"),
          row.names = FALSE)

plot_data <- ci_table[
  ci_table$variant == "article_default" &
    ((ci_table$region == "synthetic" &
        ci_table$estimand == "strict_mechanism") |
       ci_table$region != "synthetic"), , drop = FALSE
]
plot_data$scope <- ifelse(
  plot_data$region == "synthetic",
  paste0("Synthetic ", plot_data$scale),
  paste0("Real ", plot_data$region)
)
plot_data$scope <- factor(
  plot_data$scope,
  levels = c("Synthetic 1x", "Synthetic 4x", "Synthetic 10x",
             "Real GPe", "Real STN", "Real GPi")
)
plot_data$metric <- factor(
  plot_data$metric, levels = c("precision", "recall", "f1"),
  labels = c("Precision", "Recall", "F1")
)
plot_data$method <- factor(
  plot_data$method, levels = c("Mean-ISI", "LogISI/newBD")
)

palette <- c("Mean-ISI" = "#6A51A3", "LogISI/newBD" = "#D95F0E")
position <- ggplot2::position_dodge(width = .46)
figure <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = metric, y = observed, colour = method, group = method)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = pmax(0, ci_low), ymax = pmin(1, ci_high)),
    width = .12, linewidth = .45, position = position
  ) +
  ggplot2::geom_point(size = 2.1, position = position) +
  ggplot2::facet_wrap(~scope, nrow = 2) +
  ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2),
                              expand = ggplot2::expansion(mult = c(0, .03))) +
  ggplot2::labs(x = NULL, y = "Burst ISI-support metric", colour = NULL) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "top",
    legend.justification = "left",
    panel.spacing = grid::unit(8, "pt")
  )

ggplot2::ggsave(
  file.path(out_dir, "meanisi_logisi_burst_performance.pdf"),
  figure, width = 180, height = 112, units = "mm", device = grDevices::cairo_pdf
)
ggplot2::ggsave(
  file.path(out_dir, "meanisi_logisi_burst_performance.png"),
  figure, width = 180, height = 112, units = "mm", dpi = 600,
  bg = "white"
)

caption <- paste(
  "Burst detection by the Mean-ISI and Pasquale LogISI/newBD support methods.",
  "Points show pooled ISI-support precision, recall, and F1; bars are 95%",
  "cluster-bootstrap intervals (synthetic: Template_ID; real: train).",
  "Synthetic v2.3 holdout results use strict mechanism truth and retain the",
  "three exact time-scale projections as separate numerical tests. Real-data",
  "results use manually reviewed intervals from event-reference-eligible",
  "GPe, STN, and GPi trains; reviewed blank event labels are treated as other.",
  "Both detectors are label-blind. Article-style minimum event sizes are used",
  "(Mean-ISI: 3 spikes; LogISI/newBD: 5 spikes)."
)
writeLines(caption, file.path(out_dir, "figure_caption.md"), useBytes = TRUE)

message("Report figure written to: ", out_dir)
