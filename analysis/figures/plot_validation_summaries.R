#!/usr/bin/env Rscript

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required to render validation figures.", call. = FALSE)
}

repo_override <- Sys.getenv("STPD_PUBLIC_REPO", unset = "")
if (nzchar(repo_override)) {
  repo_root <- normalizePath(repo_override, mustWork = TRUE)
} else {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else NA_character_
  repo_root <- if (is.character(script_path) && !is.na(script_path)) {
    normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
  } else {
    normalizePath(getwd(), mustWork = TRUE)
  }
}

output_dir <- file.path(repo_root, "analysis", "figures", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_publication_plot <- function(plot, stem, width, height) {
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".pdf")), plot,
    width = width, height = height, units = "in",
    device = grDevices::pdf, useDingbats = FALSE, bg = "white"
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".png")), plot,
    width = width, height = height, units = "in",
    dpi = 300, bg = "white"
  )
}

pattern_labels <- c(
  burst = "Burst",
  pause = "Pause",
  high_frequency_spiking = "Broad HFS",
  tonic = "Tonic",
  broad_hfs = "Broad HFS"
)
regime_labels <- c(
  automatic = "Automatic",
  partial_known = "Partial-known",
  full_params = "Full-parameter"
)
palette <- c(
  Automatic = "#0072B2",
  `Partial-known` = "#E69F00",
  `Full-parameter` = "#009E73"
)

theme_publication <- function() {
  ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "black"),
      panel.spacing = grid::unit(0.8, "lines")
    )
}

real_path <- file.path(repo_root, "results", "real_validation", "primary_f1_summary.csv")
if (file.exists(real_path)) {
  real <- utils::read.csv(real_path, check.names = FALSE, stringsAsFactors = FALSE)
  real$pattern_key <- tolower(gsub("[^A-Za-z0-9]+", "_", real$pattern))
  real$pattern_key[real$pattern_key == "broad_hfs"] <- "broad_hfs"
  real <- real[
    real$pattern_key %in% names(pattern_labels) &
      real$level %in% c("interval", "event") &
      is.finite(real$F1),
    , drop = FALSE
  ]
  real$Pattern <- factor(unname(pattern_labels[real$pattern_key]),
                         levels = c("Tonic", "Broad HFS", "Pause", "Burst"))
  real$Regime <- factor(unname(regime_labels[real$regime]),
                        levels = unname(regime_labels))
  real$Region <- factor(
    real$region,
    levels = c("GPE", "STN", "GPI"),
    labels = c("GPe", "STN", "GPi")
  )

  for (target_level in c("interval", "event")) {
    x <- real[real$level == target_level, , drop = FALSE]
    if (!nrow(x)) stop("No rows available for real validation level: ", target_level, call. = FALSE)
    p <- ggplot2::ggplot(
      x,
      ggplot2::aes(x = F1, y = Pattern, color = Regime, shape = Regime)
    ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = ci_low, xmax = ci_high),
        orientation = "y", width = 0, linewidth = 0.45,
        position = ggplot2::position_dodge(width = 0.5),
        na.rm = TRUE
      ) +
      ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::facet_wrap(~Region, nrow = 1, scales = "free_y") +
      ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
      ggplot2::scale_color_manual(values = palette, drop = FALSE) +
      ggplot2::scale_shape_manual(values = c(16, 17, 15), drop = FALSE) +
      ggplot2::labs(x = "F1 score (95% cluster-bootstrap CI)", y = NULL) +
      theme_publication()
    save_publication_plot(
      p,
      paste0("real_validation_", target_level, "_f1"),
      width = 8.2,
      height = 3.8
    )
  }
}

synthetic_path <- file.path(
  repo_root, "results", "synthetic_validation", "v2.3.0",
  "template_cluster_bootstrap_95ci.csv"
)
if (file.exists(synthetic_path)) {
  synthetic <- utils::read.csv(synthetic_path, check.names = FALSE, stringsAsFactors = FALSE)
  synthetic_iou <- suppressWarnings(as.numeric(synthetic$IoU))
  synthetic <- synthetic[
    synthetic$Estimand == "mechanism" &
      synthetic$Variant == "strict_raw" &
      synthetic$metric == "F1" &
      synthetic$Target %in% c("burst", "pause", "broad_hfs", "tonic") &
      ((synthetic$Level == "episode" & is.finite(synthetic_iou) & abs(synthetic_iou - 0.5) < 1e-12) |
         synthetic$Level == "isi_support"),
    , drop = FALSE
  ]
  synthetic$Pattern <- factor(
    unname(pattern_labels[synthetic$Target]),
    levels = c("Burst", "Pause", "Broad HFS", "Tonic")
  )
  synthetic$Level_label <- factor(
    ifelse(synthetic$Level == "isi_support", "ISI support", "Episode (IoU >= 0.50)"),
    levels = c("ISI support", "Episode (IoU >= 0.50)")
  )
  synthetic$Scale <- factor(
    paste0(synthetic$Scale_Factor, "x"),
    levels = c("1x", "4x", "10x")
  )
  pattern_palette <- c(
    Burst = "#D55E00",
    Pause = "#0072B2",
    `Broad HFS` = "#CC79A7",
    Tonic = "#009E73"
  )

  p <- ggplot2::ggplot(
    synthetic,
    ggplot2::aes(x = Scale, y = observed, color = Pattern, group = Pattern, shape = Pattern)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = 0.08, linewidth = 0.45, position = ggplot2::position_dodge(width = 0.22)
    ) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.22)) +
    ggplot2::facet_wrap(~Level_label, nrow = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
    ggplot2::scale_color_manual(values = pattern_palette, drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18), drop = FALSE) +
    ggplot2::labs(x = "Paired time-scale projection", y = "F1 score (95% template-bootstrap CI)") +
    theme_publication()
  save_publication_plot(p, "synthetic_v2_3_f1", width = 7.4, height = 3.9)
}

cat("Figure outputs written to:", output_dir, "\n")
