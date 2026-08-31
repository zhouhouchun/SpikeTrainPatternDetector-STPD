#!/usr/bin/env Rscript

# Build the R/ggplot2 publication figure for the unified truth-referenced
# Mean-ISI, LogISI/newBD, and STPD Burst comparison.

options(stringsAsFactors = FALSE)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
out_dir <- path.expand(Sys.getenv(
  "STPD_THREE_METHOD_RESULT_DIR",
  unset = file.path(repo, "test-results", "method_comparison",
                    "three_method_truth_accuracy_current")
))
if (!dir.exists(out_dir)) stop("Three-method result directory is unavailable.",
                               call. = FALSE)

ci <- read.csv(file.path(out_dir, "cluster_bootstrap_95ci.csv"),
               check.names = FALSE, stringsAsFactors = FALSE)
plot_data <- ci[ci$metric %in% c("precision", "recall", "f1"), , drop = FALSE]
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
  plot_data$method, levels = c("Mean-ISI", "LogISI/newBD", "STPD")
)

palette <- c(
  "Mean-ISI" = "#6A51A3", "LogISI/newBD" = "#D95F0E",
  "STPD" = "#1B9E77"
)
position <- ggplot2::position_dodge(width = .60)
figure <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = metric, y = observed, colour = method, group = method)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = pmax(0, ci_low), ymax = pmin(1, ci_high)),
    width = .10, linewidth = .42, position = position
  ) +
  ggplot2::geom_point(size = 1.9, position = position) +
  ggplot2::facet_wrap(~scope, nrow = 2) +
  ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, .2),
    expand = ggplot2::expansion(mult = c(0, .03))
  ) +
  ggplot2::labs(x = NULL, y = "Burst ISI-support metric", colour = NULL) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "top", legend.justification = "left",
    panel.spacing = grid::unit(8, "pt")
  )

ggplot2::ggsave(
  file.path(out_dir, "three_method_burst_accuracy.pdf"), figure,
  width = 180, height = 112, units = "mm", device = grDevices::cairo_pdf
)
ggplot2::ggsave(
  file.path(out_dir, "three_method_burst_accuracy.png"), figure,
  width = 180, height = 112, units = "mm", dpi = 600, bg = "white"
)

caption <- paste(
  "Truth-referenced Burst detection by Mean-ISI, LogISI/newBD, and STPD.",
  "Points show pooled ISI-support precision, recall, and F1; bars show 95%",
  "cluster-bootstrap intervals (synthetic: Template_ID; real: Group_ID; 1,000",
  "replicates). Synthetic v2.3 holdout projections are reported separately",
  "at 1x, 4x, and 10x. Real GPe, STN, and GPi panels use the same frozen",
  "reference-eligible trains and truth masks for all methods. All three",
  "detectors are label-blind in this zero-example comparison."
)
writeLines(caption, file.path(out_dir, "figure_caption.md"), useBytes = TRUE)

manifest_path <- file.path(out_dir, "manifest_checksums.csv")
if (file.exists(manifest_path)) {
  manifest_files <- list.files(out_dir, full.names = TRUE)
  manifest_files <- manifest_files[basename(manifest_files) !=
                                     "manifest_checksums.csv"]
  manifest <- data.frame(
    file = basename(manifest_files),
    sha256 = vapply(
      manifest_files, digest::digest, character(1), algo = "sha256",
      serialize = FALSE, file = TRUE
    ), stringsAsFactors = FALSE
  )
  write.csv(manifest, manifest_path, row.names = FALSE)
}

message("Three-method figure written to: ", out_dir)
