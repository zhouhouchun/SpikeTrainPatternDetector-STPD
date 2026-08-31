#!/usr/bin/env Rscript

# Publication-ready runtime figures generated from committed benchmark tables.
# Author: Zhou Houchun

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.", call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "analysis", "figures", "plot_performance_runtime.R")
}
root_default <- file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
root <- normalizePath(
  Sys.getenv("STPD_PUBLIC_REPO", unset = root_default),
  mustWork = TRUE
)
output_dir <- file.path(root, "analysis", "figures", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

theme_stpd <- function() {
  ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      legend.position = "none",
      panel.grid = ggplot2::element_blank()
    )
}

runtime <- utils::read.csv(
  file.path(root, "results", "performance", "real_three_region_runtime.csv"),
  stringsAsFactors = FALSE
)
runtime <- runtime[runtime$regime == "automatic", , drop = FALSE]
runtime$region <- factor(runtime$region, levels = c("GPe", "STN", "GPi"))

p_region <- ggplot2::ggplot(
  runtime,
  ggplot2::aes(x = region, y = elapsed_minutes, fill = region)
) +
  ggplot2::geom_col(width = 0.62, colour = "black", linewidth = 0.25) +
  ggplot2::scale_fill_manual(values = c(
    GPe = "#4477AA", STN = "#66CCEE", GPi = "#228833"
  )) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.06))) +
  ggplot2::labs(x = NULL, y = "Elapsed time (min)") +
  theme_stpd()

ggplot2::ggsave(
  file.path(output_dir, "runtime_three_region_automatic.pdf"),
  p_region, width = 3.35, height = 2.55, units = "in",
  device = grDevices::pdf, useDingbats = FALSE, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "runtime_three_region_automatic.png"),
  p_region, width = 3.35, height = 2.55, units = "in", dpi = 300, bg = "white"
)

run_paths <- c(
  core = file.path(
    root, "results", "performance", "repeated_core_one_train",
    "detector_runtime_runs.csv"
  ),
  app_default = file.path(
    root, "results", "performance", "repeated_app_default_one_train",
    "detector_runtime_runs.csv"
  )
)
runs <- do.call(rbind, lapply(run_paths, function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE)
}))
runs$profile_label <- factor(
  runs$profile,
  levels = c("core", "app_default"),
  labels = c("Core", "App default")
)

p_profile <- ggplot2::ggplot(
  runs,
  ggplot2::aes(x = profile_label, y = elapsed_seconds, colour = profile_label)
) +
  ggplot2::geom_boxplot(width = 0.42, outlier.shape = NA, linewidth = 0.45) +
  ggplot2::geom_point(
    size = 2.0,
    position = ggplot2::position_jitter(width = 0.045, seed = 20260831L)
  ) +
  ggplot2::scale_colour_manual(values = c("Core" = "#4477AA", "App default" = "#CC6677")) +
  ggplot2::labs(x = NULL, y = "Elapsed time (s)") +
  theme_stpd()

ggplot2::ggsave(
  file.path(output_dir, "runtime_one_train_profiles.pdf"),
  p_profile, width = 3.35, height = 2.55, units = "in",
  device = grDevices::pdf, useDingbats = FALSE, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "runtime_one_train_profiles.png"),
  p_profile, width = 3.35, height = 2.55, units = "in", dpi = 300, bg = "white"
)

cat("Performance figure outputs written to:", output_dir, "\n")
