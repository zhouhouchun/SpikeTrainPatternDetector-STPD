#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "analysis", "manuscript_figures", "plot_three_method_structure.R")
}
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
input_file <- file.path(
  repo, "results", "method_comparison", "three_method_truth_accuracy_current",
  "event_metrics.csv"
)
output_dir <- file.path(repo, "analysis", "manuscript_figures", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

method_levels <- c("STPD", "Mean-ISI", "LogISI/newBD")
context_levels <- c("GPe", "GPi", "STN", "Synthetic 1×", "Synthetic 4×", "Synthetic 10×")

selected <- raw |>
  filter(iou_threshold == 0.25) |>
  filter(
    (grepl("^real_", dataset) & estimand == "reviewed_manual_reference") |
      (dataset == "synthetic_v2.3_holdout" & estimand == "strict_mechanism")
  ) |>
  mutate(
    context = case_when(
      dataset == "real_GPe" ~ "GPe",
      dataset == "real_GPi" ~ "GPi",
      dataset == "real_STN" ~ "STN",
      dataset == "synthetic_v2.3_holdout" & scale == "1x" ~ "Synthetic 1×",
      dataset == "synthetic_v2.3_holdout" & scale == "4x" ~ "Synthetic 4×",
      dataset == "synthetic_v2.3_holdout" & scale == "10x" ~ "Synthetic 10×",
      TRUE ~ NA_character_
    ),
    event_count_ratio = predicted_event_n / truth_event_n
  ) |>
  filter(!is.na(context), method %in% method_levels) |>
  mutate(
    context = factor(context, levels = context_levels),
    method = factor(method, levels = method_levels)
  )

stopifnot(nrow(selected) == length(method_levels) * length(context_levels))
stopifnot(all(is.finite(selected$event_count_ratio)))
stopifnot(all(selected$mean_matched_iou >= 0 & selected$mean_matched_iou <= 1))
stopifnot(all(selected$fragmentation_rate >= 0 & selected$fragmentation_rate <= 1))
stopifnot(all(selected$merge_rate >= 0 & selected$merge_rate <= 1))

long <- bind_rows(
  selected |> transmute(context, method, metric = "Predicted/reference\nevent count", value = event_count_ratio),
  selected |> transmute(context, method, metric = "Mean matched IoU", value = mean_matched_iou),
  selected |> transmute(context, method, metric = "Reference-event\nfragmentation", value = fragmentation_rate),
  selected |> transmute(context, method, metric = "Prediction merge", value = merge_rate)
) |>
  mutate(metric = factor(
    metric,
    levels = c(
      "Predicted/reference\nevent count",
      "Mean matched IoU",
      "Reference-event\nfragmentation",
      "Prediction merge"
    )
  ))

method_colours <- c(
  "STPD" = "#0072B2",
  "Mean-ISI" = "#009E73",
  "LogISI/newBD" = "#D55E00"
)
method_shapes <- c("STPD" = 16, "Mean-ISI" = 17, "LogISI/newBD" = 15)

reference_lines <- data.frame(
  metric = factor(
    c("Predicted/reference\nevent count", "Mean matched IoU", "Reference-event\nfragmentation", "Prediction merge"),
    levels = levels(long$metric)
  ),
  y = c(1, 0, 0, 0)
)

p <- ggplot(long, aes(x = context, y = value, colour = method, shape = method)) +
  geom_hline(
    data = reference_lines,
    aes(yintercept = y),
    inherit.aes = FALSE,
    linewidth = 0.35,
    colour = "grey55",
    linetype = "dashed"
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 2.4,
    stroke = 0.55
  ) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = method_colours, drop = FALSE) +
  scale_shape_manual(values = method_shapes, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.06, 0.10))) +
  labs(x = NULL, y = NULL, colour = NULL, shape = NULL) +
  theme_classic(base_size = 9, base_family = "Helvetica") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, margin = margin(b = 5)),
    axis.text.x = element_text(angle = 32, hjust = 1, vjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.line = element_line(linewidth = 0.35, colour = "black"),
    axis.ticks = element_line(linewidth = 0.35, colour = "black"),
    panel.spacing = grid::unit(10, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(b = 1),
    legend.key.width = grid::unit(12, "pt"),
    plot.margin = margin(4, 6, 4, 4)
  )

png_file <- file.path(output_dir, "Figure_5_three_method_structure.png")
pdf_file <- file.path(output_dir, "Figure_5_three_method_structure.pdf")
tiff_file <- file.path(output_dir, "Figure_5_three_method_structure.tiff")

ggsave(png_file, p, width = 180, height = 125, units = "mm", dpi = 600, bg = "white")
ggsave(pdf_file, p, width = 180, height = 125, units = "mm", device = cairo_pdf, bg = "white")
ggsave(tiff_file, p, width = 180, height = 125, units = "mm", dpi = 600, compression = "lzw", bg = "white")

summary_file <- file.path(output_dir, "Figure_5_three_method_structure_source.csv")
write.csv(
  selected |>
    select(
      context, method, truth_event_n, predicted_event_n, event_count_ratio,
      mean_matched_iou, fragmentation_rate, merge_rate
    ),
  summary_file,
  row.names = FALSE
)

message("Wrote: ", png_file)
message("Wrote: ", pdf_file)
message("Wrote: ", tiff_file)
message("Wrote: ", summary_file)
