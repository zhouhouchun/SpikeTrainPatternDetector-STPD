#!/usr/bin/env Rscript

# Programmatic manuscript figures for the STPD reviewer revision.
# Author: Zhou Houchun
# All quantitative panels read frozen CSV outputs from v1.2.2-rev1.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(grid)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "analysis", "manuscript_figures", "build_manuscript_figures.R")
}
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
out_dir <- file.path(repo, "analysis", "manuscript_figures", "generated")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

theme_stpd <- function(base_size = 10.5) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1.5, hjust = 0),
      plot.subtitle = element_text(size = base_size - 0.5, color = "#3F4A5A"),
      axis.title = element_text(face = "plain"),
      strip.background = element_rect(fill = "#EEF2F5", color = NA),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_line(color = "#E3E7EB", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}

save_plot <- function(plot, stem, width, height, dpi = 320) {
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot,
         width = width, height = height, units = "in", dpi = dpi,
         bg = "white")
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot,
         width = width, height = height, units = "in", device = grDevices::pdf,
         bg = "white")
}

save_grid <- function(draw_fun, stem, width, height, dpi = 320) {
  png(file.path(out_dir, paste0(stem, ".png")),
      width = width, height = height, units = "in", res = dpi,
      type = "cairo", bg = "white")
  draw_fun()
  dev.off()
  pdf(file.path(out_dir, paste0(stem, ".pdf")), width = width, height = height,
      family = "Helvetica", useDingbats = FALSE)
  draw_fun()
  dev.off()
}

# Figure 2 ------------------------------------------------------------------
# Analytical boundary, six-stage workflow, retained audit evidence, and
# validation/downstream use. The visual hierarchy follows the stronger
# flowchart logic of the original manuscript while preserving the current
# release semantics. It is generated entirely in R.

draw_workflow <- function() {
  grid.newpage()

  navy <- "#23384D"
  ink <- "#172635"
  line <- "#40576B"
  muted <- "#657786"
  blue_fill <- "#E7F0F7"
  teal_fill <- "#E5F2EE"
  amber_fill <- "#FFF3D6"
  violet_fill <- "#EEE8F6"
  grey_fill <- "#F4F6F8"

  rounded_box <- function(x, y, w, h, fill, border = line, lwd = 1.15,
                          radius = 0.025) {
    grid.roundrect(
      x = unit(x, "npc"), y = unit(y, "npc"),
      width = unit(w, "npc"), height = unit(h, "npc"),
      r = unit(radius, "snpc"),
      gp = gpar(fill = fill, col = border, lwd = lwd)
    )
  }

  label_block <- function(x, y, heading, body, heading_size = 8.5,
                          body_size = 7.2, col = ink, lineheight = 1.05) {
    grid.text(
      heading, x = unit(x, "npc"), y = unit(y + 0.018, "npc"),
      gp = gpar(fontfamily = "Helvetica", fontsize = heading_size,
                fontface = "bold", col = col)
    )
    grid.text(
      body, x = unit(x, "npc"), y = unit(y - 0.020, "npc"),
      gp = gpar(fontfamily = "Helvetica", fontsize = body_size,
                col = col, lineheight = lineheight)
    )
  }

  stage_box <- function(n, x, y, heading, body, fill, accent) {
    w <- 0.265
    h <- 0.125
    rounded_box(x, y, w, h, fill, border = line, lwd = 1.2, radius = 0.03)
    grid.roundrect(
      x = unit(x - w / 2 + 0.033, "npc"), y = unit(y, "npc"),
      width = unit(0.048, "npc"), height = unit(0.071, "npc"),
      r = unit(0.02, "snpc"),
      gp = gpar(fill = accent, col = NA)
    )
    grid.text(
      as.character(n), x = unit(x - w / 2 + 0.033, "npc"),
      y = unit(y, "npc"),
      gp = gpar(fontfamily = "Helvetica", fontsize = 10.3,
                fontface = "bold", col = "white")
    )
    text_x <- x - w / 2 + 0.072
    grid.text(
      heading, x = unit(text_x, "npc"), y = unit(y + 0.024, "npc"),
      just = "left",
      gp = gpar(fontfamily = "Helvetica", fontsize = 8.2,
                fontface = "bold", col = ink)
    )
    grid.text(
      body, x = unit(text_x, "npc"), y = unit(y - 0.025, "npc"),
      just = "left",
      gp = gpar(fontfamily = "Helvetica", fontsize = 6.95,
                col = ink, lineheight = 1.02)
    )
  }

  small_box <- function(x, y, w, h, heading, body, fill = "white",
                        border = line, heading_size = 7.6,
                        body_size = 6.6) {
    rounded_box(x, y, w, h, fill, border = border, lwd = 0.95, radius = 0.018)
    label_block(x, y, heading, body, heading_size, body_size,
                col = ink, lineheight = 1.02)
  }

  arrow_path <- function(x, y, col = line, lwd = 1.45) {
    grid.lines(
      x = unit(x, "npc"), y = unit(y, "npc"),
      arrow = arrow(length = unit(0.075, "in"), type = "closed"),
      gp = gpar(col = col, fill = col, lwd = lwd,
                linejoin = "mitre", lineend = "butt")
    )
  }

  # Six-stage operational workflow.
  rounded_box(0.50, 0.765, 0.94, 0.405, "#FBFCFD", border = navy,
              lwd = 1.35, radius = 0.035)
  grid.text(
    "AUDITABLE ANNOTATION WORKFLOW",
    x = unit(0.055, "npc"), y = unit(0.925, "npc"), just = "left",
    gp = gpar(fontfamily = "Helvetica", fontsize = 8.2,
              fontface = "bold", col = navy)
  )

  xs <- c(0.18, 0.50, 0.82)
  y_top <- 0.835
  y_bottom <- 0.645

  stage_box(1, xs[1], y_top, "Input",
            "timestamps or annotated tables\nwith declared units", blue_fill, "#3478A8")
  stage_box(2, xs[2], y_top, "Pre-detection quality control",
            "duplicates and ordering;\nrefractory-period checks;\ndataset identity", blue_fill, "#3478A8")
  stage_box(3, xs[3], y_top, "Resolve thresholds",
            "automatic, manual or external\nsources; preview and inspect;\nfreeze and hash", amber_fill, "#B47B00")
  stage_box(4, xs[1], y_bottom, "Generate candidates",
            "STPD grammar; Mean-ISI;\nLogISI/newBD; external providers;\nEvent · Gap · State · Review", teal_fill, "#2C806E")
  stage_box(5, xs[2], y_bottom, "Arbitrate and review",
            "deterministic interval arbitration;\nreview; manual locks;\nrevisions and vetoes", violet_fill, "#72549A")
  stage_box(6, xs[3], y_bottom, "Export public results",
            "selected/rejected intervals;\nthresholds and decisions;\nprovenance and hashes", grey_fill, "#4D6476")

  arrow_path(c(0.313, 0.362), c(y_top, y_top))
  arrow_path(c(0.633, 0.682), c(y_top, y_top))
  arrow_path(c(0.82, 0.82, 0.18, 0.18),
             c(0.770, 0.746, 0.746, 0.711))
  arrow_path(c(0.313, 0.362), c(y_bottom, y_bottom))
  arrow_path(c(0.633, 0.682), c(y_bottom, y_bottom))

  # Evidence retained throughout the workflow.
  arrow_path(c(0.50, 0.50), c(0.560, 0.525), col = navy, lwd = 1.35)
  grid.text(
    "AUDIT EVIDENCE RETAINED THROUGHOUT",
    x = unit(0.055, "npc"), y = unit(0.497, "npc"), just = "left",
    gp = gpar(fontfamily = "Helvetica", fontsize = 8.0,
              fontface = "bold", col = navy)
  )
  grid.lines(
    x = unit(c(0.39, 0.96), "npc"), y = unit(c(0.497, 0.497), "npc"),
    gp = gpar(col = "#A8B2BB", lwd = 1.0, lty = 3)
  )

  audit_x <- c(0.15, 0.39, 0.63, 0.87)
  small_box(audit_x[1], 0.438, 0.205, 0.085,
            "Input / QC records", "import decisions\nand dataset identity", "#F5F8FA")
  small_box(audit_x[2], 0.438, 0.205, 0.085,
            "Parameter records", "effective values · sources\nand change preview", "#FFF8E8")
  small_box(audit_x[3], 0.438, 0.205, 0.085,
            "Detector records", "proposed/rejected candidates\nand diagnostic evidence", "#EEF7F4")
  small_box(audit_x[4], 0.438, 0.205, 0.085,
            "Review / run records", "final decisions · software/data/\nparameter hashes", "#F2EEF7")

  # Validation and downstream use are deliberately outside candidate creation.
  grid.text(
    "VALIDATION AND DOWNSTREAM USE",
    x = unit(0.055, "npc"), y = unit(0.347, "npc"), just = "left",
    gp = gpar(fontfamily = "Helvetica", fontsize = 8.0,
              fontface = "bold", col = navy)
  )
  grid.lines(
    x = unit(c(0.37, 0.96), "npc"), y = unit(c(0.347, 0.347), "npc"),
    gp = gpar(col = "#A8B2BB", lwd = 1.0, lty = 3)
  )

  bottom_x <- c(0.15, 0.39, 0.63, 0.87)
  small_box(bottom_x[1], 0.267, 0.205, 0.112,
            "Reference evidence", "expert intervals or frozen\nsimulator truth", "#F5F8FA")
  small_box(bottom_x[2], 0.267, 0.205, 0.112,
            "Common scoring", "ISI support · event IoU · P/R/F1\nboundary and fragmentation", "#E8F1F8")
  small_box(bottom_x[3], 0.267, 0.205, 0.112,
            "Runtime / scalability", "trains · timestamps\nand candidate density", "#EEF7F4")
  small_box(bottom_x[4], 0.267, 0.205, 0.112,
            "Exploratory views", "visualization only;\nno hidden evidence feeds\nthe final labels", "#FFF6E0")
  arrow_path(c(0.253, 0.283), c(0.267, 0.267), col = line, lwd = 1.15)
}

save_grid(draw_workflow, "Figure_2_auditable_workflow", 7.2, 5.05, dpi = 600)

# Figure 3 ------------------------------------------------------------------
# Event-primary evidence: protected synthetic holdouts and real common-mask
# Burst agreement. Different estimands remain in separate facets.

read_synthetic_evidence <- function(version, panel_label) {
  read.csv(file.path(
    repo, "results/synthetic_validation", version,
    "template_cluster_bootstrap_95ci.csv"
  ), stringsAsFactors = FALSE) %>%
    filter(
      Estimand == "mechanism", Variant == "strict_raw", Level == "episode",
      IoU == "0.25", metric == "F1", Target %in% c("burst", "pause", "broad_hfs")
    ) %>%
    mutate(
      panel = panel_label,
      target = recode(Target, burst = "Burst", pause = "Pause", broad_hfs = "Broad HFS"),
      scale = factor(paste0(Scale_Factor, "×"), levels = c("1×", "4×", "10×")),
      label = paste(target, scale, sep = " · ")
    ) %>%
    transmute(panel, label, target, series = scale, observed, ci_low, ci_high)
}

syn_v22_plot <- read_synthetic_evidence(
  "v2.2.0",
  "Synthetic v2.2 clean mechanism benchmark\n(15 protected Template_ID clusters per scale)"
)
syn_v23_plot <- read_synthetic_evidence(
  "v2.3.0",
  "Synthetic v2.3 clean-overlap supplement\n(15 protected Template_ID clusters per scale)"
)

real_ci <- read.csv(file.path(
  repo, "results/method_comparison/three_method_truth_accuracy_current/event_cluster_bootstrap_95ci.csv"
), stringsAsFactors = FALSE)

real_plot <- real_ci %>%
  filter(
    grepl("^real_", dataset), method == "STPD", iou_threshold == 0.25,
    tolower(metric) == "f1"
  ) %>%
  mutate(
    panel = "Real single-expert common-mask Burst references\n(57 trains; 53,151 reviewed ISIs; 13 Group_ID clusters per region)",
    label = paste("Burst", region, sep = " · "),
    target = "Burst",
    series = region
  ) %>%
  transmute(panel, label, target, series, observed, ci_low, ci_high)

evidence_plot <- bind_rows(syn_v22_plot, syn_v23_plot, real_plot) %>%
  mutate(label = factor(label, levels = rev(unique(label))))

p2 <- ggplot(evidence_plot,
             aes(x = observed, y = label, xmin = ci_low, xmax = ci_high,
                 color = target, shape = series)) +
  geom_vline(xintercept = 0.5, linetype = "dotted",
             color = "#9AA4AE", linewidth = 0.35) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             color = "#9AA4AE", linewidth = 0.35) +
  geom_errorbar(orientation = "y", width = 0.18, linewidth = 0.7) +
  geom_point(size = 2.6, stroke = 0.7) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c(Burst = "#2C6EBA", Pause = "#C44E52", `Broad HFS` = "#7A4FA3")) +
  labs(
    title = "Event-level F1 at IoU >= 0.25",
    subtitle = "Points are pooled estimates; bars are cluster-bootstrap 95% intervals",
    x = "F1", y = NULL, color = "Target", shape = "Scale / region"
  ) +
  theme_stpd() +
  theme(legend.position = "bottom")

save_plot(p2, "Figure_3_event_primary_evidence", 7.2, 8.5)

# Figure 4 ------------------------------------------------------------------
# Common-mask real Burst comparison. This compares specified implementations,
# not universal method families.

event_ci <- real_ci %>%
  filter(grepl("^real_", dataset), iou_threshold == 0.25) %>%
  mutate(
    metric = recode(tolower(metric), precision = "Precision", recall = "Recall", f1 = "F1"),
    method = factor(method, levels = c("Mean-ISI", "LogISI/newBD", "STPD")),
    region = factor(region, levels = c("GPe", "STN", "GPi"))
  )

p3 <- ggplot(event_ci,
             aes(x = observed, y = method, xmin = ci_low, xmax = ci_high,
                 color = metric, shape = metric)) +
  geom_errorbar(orientation = "y", width = 0.14, linewidth = 0.55,
                 position = position_dodge(width = 0.42)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.42)) +
  facet_wrap(~region, ncol = 3) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_color_manual(values = c(Precision = "#326891", Recall = "#D17A22", F1 = "#2F855A")) +
  labs(
    title = "Common-mask real Burst comparison",
    subtitle = "Specified automatic implementations; IoU >= 0.25; 1,000 paired Group_ID bootstrap draws",
    x = "Metric estimate", y = NULL, color = "Metric", shape = "Metric"
  ) +
  theme_stpd(9.8) +
  theme(axis.text.y = element_text(size = 8.8), legend.position = "bottom")

save_plot(p3, "Figure_4_three_method_real_burst", 7.2, 4.0)

# Figure 6 ------------------------------------------------------------------
# Synthetic event error decomposition at IoU >= 0.50. Panel A shows event
# splitting/merging rates; Panel B shows matched boundary-error distributions.

fm <- read.csv(file.path(
  repo, "results/synthetic_validation/v2.3.0/fragmentation_merge_summary.csv"
), stringsAsFactors = FALSE) %>%
  filter(
    Estimand == "mechanism", Variant == "strict_raw",
    Target %in% c("burst", "pause", "broad_hfs")
  ) %>%
  mutate(
    Target = recode(Target, burst = "Burst", pause = "Pause", broad_hfs = "Broad HFS"),
    Kind = recode(Kind, truth_fragmentation = "Truth fragmentation",
                  prediction_merge = "Prediction merge"),
    Scale = factor(paste0(Scale_Factor, "×"), levels = c("1×", "4×", "10×"))
  )

p4a <- ggplot(fm, aes(x = Target, y = Error, color = Scale, group = Scale)) +
  geom_point(position = position_dodge(width = 0.45), size = 2.3) +
  facet_wrap(~Kind, nrow = 1) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 0.16)) +
  scale_color_manual(values = c(`1×` = "#2C6EBA", `4×` = "#7A4FA3", `10×` = "#D17A22")) +
  labs(title = "A  Fragmentation and merge rates", x = NULL, y = "Affected-event proportion",
       color = "Scale") +
  theme_stpd(9.4) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

boundary <- read.csv(file.path(
  repo, "results/synthetic_validation/v2.3.0/boundary_errors_iou_050.csv"
), stringsAsFactors = FALSE) %>%
  filter(
    Estimand == "mechanism", Variant == "strict_raw",
    Target %in% c("burst", "pause", "broad_hfs")
  ) %>%
  mutate(
    Target = recode(Target, burst = "Burst", pause = "Pause", broad_hfs = "Broad HFS"),
    Scale = factor(paste0(Scale_Factor, "×"), levels = c("1×", "4×", "10×"))
  ) %>%
  select(Target, Scale, Start_Boundary_Error_ISI, End_Boundary_Error_ISI) %>%
  tidyr::pivot_longer(
    cols = c(Start_Boundary_Error_ISI, End_Boundary_Error_ISI),
    names_to = "Boundary", values_to = "Error_ISI"
  ) %>%
  mutate(Boundary = recode(Boundary,
                           Start_Boundary_Error_ISI = "Onset",
                           End_Boundary_Error_ISI = "Offset"))

p4b <- ggplot(boundary, aes(x = Target, y = Error_ISI, fill = Boundary)) +
  geom_hline(yintercept = 0, color = "#68737D", linewidth = 0.35) +
  geom_boxplot(width = 0.62, outlier.size = 0.45, outlier.alpha = 0.35,
               position = position_dodge(width = 0.7)) +
  facet_wrap(~Scale, nrow = 1) +
  coord_cartesian(ylim = c(-12, 12)) +
  scale_fill_manual(values = c(Onset = "#80B1D3", Offset = "#FDB462")) +
  labs(title = "B  Matched boundary error at IoU >= 0.50",
       subtitle = "Error in ISI indices; positive values indicate later boundaries",
       x = NULL, y = "Predicted - reference boundary (ISIs)", fill = "Boundary") +
  theme_stpd(9.4) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

draw_error_decomposition <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.88, 1.12), "null"))))
  print(p4a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p4b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  popViewport()
}

save_grid(draw_error_decomposition, "Figure_6_event_error_decomposition", 7.2, 7.2)

cat("Wrote manuscript figures to", out_dir, "\n")
