#!/usr/bin/env Rscript

# R/ggplot2 publication illustration for the current three-method Burst
# agreement audit. Two STN trains and fixed first-10-second windows are selected
# without consulting manual labels or detector agreement scores.

options(stringsAsFactors = FALSE)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(file.path(dirname(script_path), "..", ".."))
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
first_existing <- function(paths) {
  found <- paths[file.exists(paths)]
  if (!length(found)) stop("Required input is unavailable: ", paths[[1L]], call. = FALSE)
  found[[1L]]
}
out_dir <- file.path(repo, "test-results", "method_comparison",
                     "three_method_agreement_current")
events <- read.csv(file.path(out_dir, "three_method_burst_events.csv"),
                   check.names = FALSE, stringsAsFactors = FALSE)
scope <- read.csv(file.path(out_dir, "comparison_scope.csv"),
                  check.names = FALSE, stringsAsFactors = FALSE)

stn_trains <- sort(scope$train[scope$region == "STN"], method = "radix")
selected <- head(stn_trains, 2L)
stopifnot(length(selected) == 2L)

workbook <- first_existing(c(
  file.path(repo, "PD_STN",
    "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"),
  file.path(repo, "data", "real", "STN",
    "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx")
))
sheets <- readxl::excel_sheets(workbook)

spike_rows <- list()
event_rows <- list()
selection_rows <- list()
method_levels <- c("Mean-ISI", "LogISI/newBD", "STPD")
method_y <- c("Mean-ISI" = 3, "LogISI/newBD" = 2, "STPD" = 1)
display_names <- stats::setNames(c("Spike train 1", "Spike train 2"), selected)

for (train in selected) {
  sheet <- sheets[which(vapply(sheets, function(sheet) {
    z <- readxl::read_excel(workbook, sheet = sheet, n_max = 1)
    identical(as.character(z$train_id[[1L]]), train)
  }, logical(1)))[1L]]
  z <- as.data.frame(readxl::read_excel(workbook, sheet = sheet),
                     stringsAsFactors = FALSE)
  z <- z[order(as.integer(z$isi_index), method = "radix"), , drop = FALSE]
  x <- c(as.numeric(z$left_timestamp_us[[1L]]),
         as.numeric(z$right_timestamp_us)) / 1e6
  window_start <- x[[1L]]
  window_end <- window_start + 10
  relative <- x - window_start

  for (method in method_levels) {
    visible_spikes <- relative >= 0 & relative <= 10
    spike_rows[[length(spike_rows) + 1L]] <- data.frame(
      display_train = display_names[[train]], method = method,
      y = method_y[[method]], time_s = relative[visible_spikes],
      stringsAsFactors = FALSE
    )
    q <- events[events$region == "STN" & events$train == train &
                  events$method == method, , drop = FALSE]
    if (nrow(q)) {
      start_index <- pmax(1L, as.integer(q$start_isi) - 1L)
      end_index <- pmin(length(x), as.integer(q$end_isi))
      start_time <- x[start_index] - window_start
      end_time <- x[end_index] - window_start
      keep <- end_time >= 0 & start_time <= 10
      if (any(keep)) {
        event_rows[[length(event_rows) + 1L]] <- data.frame(
          display_train = display_names[[train]], method = method,
          y = method_y[[method]],
          start_s = pmax(0, start_time[keep]),
          end_s = pmin(10, end_time[keep]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  selection_rows[[length(selection_rows) + 1L]] <- data.frame(
    display_train = display_names[[train]], source_train = train,
    window_start_sec = window_start, window_end_sec = window_end,
    selection_rule = "first 10 seconds from first spike; train selected by lexical order among STN event-reference-eligible trains",
    stringsAsFactors = FALSE
  )
}

spikes <- do.call(rbind, spike_rows)
spans <- if (length(event_rows)) do.call(rbind, event_rows) else data.frame()
selection <- do.call(rbind, selection_rows)
write.csv(selection, file.path(out_dir, "figure_selection_and_windows.csv"),
          row.names = FALSE)

spikes$display_train <- factor(spikes$display_train,
  levels = c("Spike train 1", "Spike train 2"))
spikes$method <- factor(spikes$method, levels = method_levels)
if (nrow(spans)) {
  spans$display_train <- factor(spans$display_train,
    levels = c("Spike train 1", "Spike train 2"))
  spans$method <- factor(spans$method, levels = method_levels)
}

palette <- c("Mean-ISI" = "#6A51A3", "LogISI/newBD" = "#D95F0E",
             "STPD" = "#2171B5")
figure <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = spikes,
    ggplot2::aes(x = time_s, xend = time_s, y = y - .24, yend = y + .24),
    linewidth = .24, colour = "#2F3640", alpha = .72
  ) +
  ggplot2::geom_segment(
    data = spans,
    ggplot2::aes(x = start_s, xend = end_s, y = y, yend = y,
                 colour = method),
    linewidth = 2.0, lineend = "butt"
  ) +
  ggplot2::facet_wrap(~display_train, ncol = 1) +
  ggplot2::scale_colour_manual(values = palette, guide = "none") +
  ggplot2::scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2),
                              expand = ggplot2::expansion(mult = c(.005, .005))) +
  ggplot2::scale_y_continuous(
    limits = c(.58, 3.42), breaks = c(3, 2, 1), labels = method_levels,
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(x = "Time (s)", y = NULL) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", hjust = 0),
    panel.spacing = grid::unit(4, "pt"),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(4, 5, 4, 4)
  )

ggplot2::ggsave(
  file.path(out_dir, "three_method_two_train_raster.pdf"),
  figure, width = 180, height = 82, units = "mm", device = grDevices::cairo_pdf
)
ggplot2::ggsave(
  file.path(out_dir, "three_method_two_train_raster.png"),
  figure, width = 180, height = 82, units = "mm", dpi = 600, bg = "white"
)

caption <- paste(
  "Burst outputs from Mean-ISI, Pasquale LogISI/newBD, and STPD in two",
  "event-reference-eligible STN spike trains. Black ticks are spike times;",
  "coloured horizontal segments are method-specific Burst detections.",
  "For each train, the first 10 s beginning at its first spike are shown.",
  "The two trains were selected by lexical order, without reference to manual",
  "Burst labels, detector output, or agreement scores. Manual reference is not",
  "displayed because this figure illustrates method agreement rather than",
  "truth-referenced performance."
)
writeLines(caption, file.path(out_dir, "three_method_two_train_raster_caption.md"),
           useBytes = TRUE)

message("Three-method figure written to: ", out_dir)
