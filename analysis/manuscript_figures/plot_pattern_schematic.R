#!/usr/bin/env Rscript

# Deterministic schematic used for manuscript Figure 1.
# Author: Zhou Houchun
# This is an illustrative diagram, not an accuracy-analysis input.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "analysis", "manuscript_figures", "plot_pattern_schematic.R")
}
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
out_dir <- file.path(repo, "analysis", "manuscript_figures", "generated")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

make_regular <- function(start, end, isi) seq(start, end, by = isi)
make_patterned <- function(start, isi) start + cumsum(c(0, isi))

# Fixed timestamps make the figure byte-stable apart from graphics-device metadata.
spikes <- bind_rows(
  data.frame(train = "spike train 1", time = c(
    make_patterned(0.0, c(.40,.10,.45,.17,.20,.55,.65,.18,.95,.33,.20)),
    make_regular(4.8, 7.0, .085),
    make_patterned(7.3, c(.22,.35,.28,.17,.30,.55,.15,.16,.12,.24,.41,.33)),
    make_regular(12.0, 16.4, .48),
    make_patterned(16.6, c(.17,.20,.60,.25,.22,.38,.55,.17,.20,.36,.49,.25)),
    make_patterned(21.6, c(.055,.06,.05,.065,.07,.58,.22,.50,.31,.22,.48))
  )),
  data.frame(train = "spike train 2", time = c(
    make_patterned(1.2, c(.17,.32,.23,.18,.42,.16,.20,.14)),
    make_regular(3.1, 8.0, .30),
    make_regular(9.1, 14.0, .27),
    make_patterned(15.0, c(.15,.25,.14,.48,.20,.18,.33,.19,.27,.16,.50,.25,.14,.20,.29,.25)),
    make_patterned(20.2, c(.06,.06,.07,.08,.55,.25,.18,.12,.44,.22,.17,.33,.14,.20))
  )),
  data.frame(train = "spike train 3", time = c(
    make_regular(0.0, .50, .065),
    make_patterned(1.8, c(.70,.18,.55,.60)),
    make_regular(3.0, 4.2, .075),
    make_regular(4.35, 6.2, .32),
    make_regular(7.5, 8.8, .075),
    make_patterned(9.6, c(.58,.08,.07,.09,.60,.17,.20,.56)),
    make_regular(13.7, 15.2, .35),
    make_regular(17.0, 20.8, .39),
    make_regular(21.0, 22.1, .075),
    make_patterned(22.4, c(.35,.22,.41,.20,.38,.16,.25,.40))
  )),
  data.frame(train = "spike train 4", time = c(
    make_regular(0.0, 7.2, .18),
    make_regular(8.8, 16.2, .17),
    make_regular(17.7, 26.0, .16)
  ))
) |>
  filter(time <= 26) |>
  mutate(train = factor(train, levels = rev(paste("spike train", 1:4))))

segments <- data.frame(
  train = c(
    "spike train 1", "spike train 1", "spike train 1", "spike train 1", "spike train 1",
    "spike train 2", "spike train 2", "spike train 2", "spike train 2", "spike train 2",
    "spike train 3", "spike train 3", "spike train 3", "spike train 3", "spike train 3",
    "spike train 4", "spike train 4", "spike train 4", "spike train 4", "spike train 4"
  ),
  start = c(0, 4.8, 7.0, 12.0, 16.4, 1.2, 3.1, 8.0, 9.1, 14.0,
            0, .50, 3.0, 4.2, 7.5, 0, 7.2, 8.8, 16.2, 17.7),
  end = c(4.8, 7.0, 12.0, 16.4, 26.0, 3.1, 8.0, 9.1, 14.0, 26.0,
          .50, 3.0, 4.2, 7.5, 26.0, 7.2, 8.8, 16.2, 17.7, 26.0),
  regime = c(
    "Other/background", "Burst", "Other/background", "Tonic", "Other/background",
    "Other/background", "Broad high-frequency spiking", "Other/background",
    "Broad high-frequency spiking", "Other/background",
    "Burst", "Other/background", "Burst", "Other/background", "Other/background",
    "Broad high-frequency spiking", "Canonical pause", "Broad high-frequency spiking",
    "Canonical pause", "Broad high-frequency spiking"
  ), stringsAsFactors = FALSE
) |>
  mutate(train = factor(train, levels = levels(spikes$train)))

labels <- data.frame(
  train = factor(c("spike train 1", "spike train 1", "spike train 2", "spike train 3",
                   "spike train 4", "spike train 4"), levels = levels(spikes$train)),
  time = c(5.9, 14.2, 5.8, 3.6, 8.0, 21.8),
  regime = c("Burst", "Tonic", "Broad high-frequency spiking", "Burst",
             "Canonical pause", "Broad high-frequency spiking"),
  y_offset = c(-.34, -.34, -.34, -.34, -.34, -.34),
  stringsAsFactors = FALSE
)

palette <- c(
  "Burst" = "#E69F00",
  "Tonic" = "#009E73",
  "Broad high-frequency spiking" = "#F8766D",
  "Canonical pause" = "#0072B2",
  "Other/background" = "#6F7B86"
)

p <- ggplot() +
  geom_segment(
    data = segments,
    aes(x = start, xend = end, y = train, yend = train, colour = regime),
    linewidth = 2.5, lineend = "butt"
  ) +
  geom_segment(
    data = spikes,
    aes(x = time, xend = time, y = as.numeric(train) - .22,
        yend = as.numeric(train) + .22),
    linewidth = .35, colour = "black"
  ) +
  geom_text(
    data = labels,
    aes(x = time, y = as.numeric(train) + y_offset, label = regime, colour = regime),
    size = 3.3, family = "Helvetica", fontface = "plain", show.legend = FALSE
  ) +
  annotate("segment", x = 0, xend = 5, y = .55, yend = .55, linewidth = .65) +
  annotate("text", x = 2.5, y = .30, label = "5 seconds", family = "Helvetica", size = 3.0) +
  scale_colour_manual(values = palette, guide = "none") +
  scale_y_discrete(drop = FALSE) +
  scale_x_continuous(limits = c(-.2, 26.3), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = "Helvetica") +
  theme(
    axis.text.y = element_text(size = 10.5, colour = "black", hjust = 1),
    plot.margin = margin(8, 8, 24, 8)
  )

ggsave(file.path(out_dir, "Figure_1_pattern_schematic.png"), p,
       width = 180, height = 78, units = "mm", dpi = 600, bg = "white")
ggsave(file.path(out_dir, "Figure_1_pattern_schematic.pdf"), p,
       width = 180, height = 78, units = "mm", device = grDevices::cairo_pdf,
       bg = "white")
utils::write.csv(spikes, file.path(out_dir, "Figure_1_pattern_schematic_spikes.csv"),
                 row.names = FALSE)
utils::write.csv(segments, file.path(out_dir, "Figure_1_pattern_schematic_regimes.csv"),
                 row.names = FALSE)

message("Wrote manuscript Figure 1 and its source tables to: ", out_dir)
