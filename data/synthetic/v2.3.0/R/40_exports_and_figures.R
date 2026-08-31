scale_table_v23 <- function(data, sample_key, time_columns) {
  rows <- lapply(seq_len(nrow(sample_key)), function(i) {
    key <- sample_key[i, ]; x <- data[data$Template_ID == key$Template_ID, , drop = FALSE]
    if (!nrow(x)) return(NULL)
    x$Sample_ID <- key$Sample_ID; x$Scale_Factor <- key$Scale_Factor; x$B_s <- key$B_s; x$Split <- key$Split
    for (column in time_columns) if (column %in% names(x)) x[[sub("_u$", "_s", column)]] <- x[[column]] * key$B_s
    x
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

make_sample_key_v23 <- function(template_manifest) {
  pairs <- expand.grid(Template_ID = template_manifest$Template_ID, Scale_Factor = parameters$scale_factors,
                       stringsAsFactors = FALSE)
  set.seed(parameters$sample_blinding_seed); pairs <- pairs[sample(seq_len(nrow(pairs))), ]
  pairs$Sample_ID <- sprintf("S%04d", seq_len(nrow(pairs))); pairs <- pairs[order(pairs$Template_ID, pairs$Scale_Factor), ]
  pairs$B_s <- parameters$anchor_B_s * pairs$Scale_Factor
  batch_scale <- unlist(parameters$workbook_blind_batches, use.names = TRUE)
  pairs$Workbook_Batch <- names(batch_scale)[match(pairs$Scale_Factor, as.numeric(batch_scale))]
  pairs$Split <- ifelse(pairs$Template_ID %in% parameters$development_templates, "development", "holdout")
  pairs$Template_Duration_u <- template_manifest$Template_Duration_u[match(pairs$Template_ID, template_manifest$Template_ID)]
  pairs$Recording_Duration_s <- pairs$Template_Duration_u * pairs$B_s
  pairs[, c("Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Workbook_Batch", "Split",
            "Template_Duration_u", "Recording_Duration_s")]
}

canonicalize_xlsx_archive <- function(path) {
  archive_path <- normalizePath(path, mustWork = TRUE); stage <- tempfile("xlsx_canonical_")
  dir.create(stage, recursive = TRUE); on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(archive_path, exdir = stage)
  core_path <- file.path(stage, "docProps", "core.xml"); core <- paste(readLines(core_path, warn = FALSE), collapse = "")
  core <- gsub("<dcterms:created[^>]*>[^<]*</dcterms:created>",
               "<dcterms:created xsi:type=\"dcterms:W3CDTF\">2026-08-29T00:00:00Z</dcterms:created>", core)
  writeLines(core, core_path, useBytes = TRUE)
  relationship_files <- list.files(file.path(stage, "xl", "worksheets", "_rels"), pattern = "\\.rels$", full.names = TRUE)
  for (relationship_path in relationship_files) {
    xml <- paste(readLines(relationship_path, warn = FALSE), collapse = "")
    xml <- gsub("<Relationship[^>]+relationships/drawing[^>]*/>", "", xml)
    xml <- gsub("<Relationship[^>]+relationships/vmlDrawing[^>]*/>", "", xml)
    writeLines(xml, relationship_path, useBytes = TRUE)
  }
  content_types_path <- file.path(stage, "[Content_Types].xml")
  content_types <- paste(readLines(content_types_path, warn = FALSE), collapse = "")
  content_types <- gsub("<Override[^>]+PartName=\"/xl/drawings/[^\"]+\"[^>]*/>", "", content_types)
  writeLines(content_types, content_types_path, useBytes = TRUE)
  files <- sort(list.files(stage, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  Sys.setFileTime(file.path(stage, files), as.POSIXct("2026-08-29 00:00:00", tz = "UTC"))
  old_wd <- setwd(stage); on.exit(setwd(old_wd), add = TRUE); unlink(archive_path)
  status <- utils::zip(zipfile = archive_path, files = files, flags = "-X -q")
  if (!identical(status, 0L)) stop("Unable to create deterministic XLSX archive.", call. = FALSE)
  invisible(archive_path)
}

write_detector_workbook_v23 <- function(detector_input, sample_key, path) {
  workbook <- openxlsx::createWorkbook(creator = "R")
  header_style <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E78", textDecoration = "bold", halign = "center")
  time_style <- openxlsx::createStyle(numFmt = "0.000000000000000")
  for (batch_name in names(parameters$workbook_blind_batches)) {
    ids <- sample_key$Sample_ID[sample_key$Workbook_Batch == batch_name]
    x <- detector_input[detector_input$Sample_ID %in% ids, , drop = FALSE]
    openxlsx::addWorksheet(workbook, batch_name, gridLines = TRUE)
    openxlsx::writeDataTable(workbook, batch_name, x, tableStyle = "TableStyleMedium2", withFilter = TRUE)
    openxlsx::addStyle(workbook, batch_name, header_style, rows = 1L, cols = 1:3, gridExpand = TRUE, stack = TRUE)
    if (nrow(x)) openxlsx::addStyle(workbook, batch_name, time_style, rows = 2:(nrow(x) + 1L), cols = 3L, gridExpand = TRUE)
    openxlsx::freezePane(workbook, batch_name, firstRow = TRUE); openxlsx::setColWidths(workbook, batch_name, 1:3, c(14, 14, 23))
  }
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE); canonicalize_xlsx_archive(path)
}

select_calibration_v23 <- function(events_u, states_u, sample_key) {
  events <- events_u[events_u$Template_ID %in% parameters$development_templates, , drop = FALSE]
  states <- states_u[states_u$Template_ID %in% parameters$development_templates, , drop = FALSE]
  pick_group <- function(x, column, values, counts) do.call(rbind, lapply(seq_along(values), function(i) {
    y <- x[x[[column]] == values[i], , drop = FALSE]
    y <- y[order(!y$Is_Four_Spike_Boundary, y$Template_ID, y$Event_ID), , drop = FALSE]
    if (nrow(y) < counts[i]) stop("Insufficient frozen calibration episodes.", call. = FALSE)
    y[seq_len(counts[i]), , drop = FALSE]
  }))
  standalone <- events[events$Event_Context == "standalone_burst", ]; hfs_burst <- events[events$Event_Context == "burst_in_hfs", ]
  burst_pick <- rbind(
    pick_group(standalone, "Pulse_Strength_Stratum", c("weak", "moderate", "strong"), c(2L, 2L, 1L)),
    pick_group(hfs_burst, "Pulse_Strength_Stratum", c("weak", "moderate", "strong"), c(1L, 2L, 2L)))
  complex <- events[events$Pause_Subtype == "complex_multi_gap", ]; canonical <- events[events$Pause_Subtype == "canonical", ]
  pause_pick <- rbind(complex[order(complex$Event_ID), , drop = FALSE][seq_len(2L), ],
                      canonical[order(canonical$Event_ID), , drop = FALSE][seq_len(8L), ])
  tonic_pick <- states[states$State_Label == "Tonic", ]; tonic_pick <- tonic_pick[order(tonic_pick$Tonic_Subtype, tonic_pick$State_Envelope_ID), ]
  hfs_pick <- do.call(rbind, lapply(names(c(regular = 4L, intermediate = 3L, irregular = 3L)), function(regime) {
    n <- c(regular = 4L, intermediate = 3L, irregular = 3L)[regime]
    y <- states[states$State_Label == "Broad_HFS" & states$Regularity_Regime == regime, ]
    y <- y[order(!y$Contains_Injected_Burst, y$State_Envelope_ID), ]; y[seq_len(n), , drop = FALSE]
  }))
  base <- rbind(
    data.frame(Calibration_Mode = "Burst", Episode_ID = burst_pick$Event_ID, Template_ID = burst_pick$Template_ID,
               Start_u = burst_pick$Realized_Start_u, End_u = burst_pick$Realized_End_u, Duration_u = burst_pick$Realized_Duration_u),
    data.frame(Calibration_Mode = "Pause", Episode_ID = pause_pick$Event_ID, Template_ID = pause_pick$Template_ID,
               Start_u = pause_pick$Realized_Start_u, End_u = pause_pick$Realized_End_u, Duration_u = pause_pick$Realized_Duration_u),
    data.frame(Calibration_Mode = "Tonic", Episode_ID = tonic_pick$State_Envelope_ID, Template_ID = tonic_pick$Template_ID,
               Start_u = tonic_pick$Start_u, End_u = tonic_pick$End_u, Duration_u = tonic_pick$Duration_u),
    data.frame(Calibration_Mode = "Broad_HFS", Episode_ID = hfs_pick$State_Envelope_ID, Template_ID = hfs_pick$Template_ID,
               Start_u = hfs_pick$Start_u, End_u = hfs_pick$End_u, Duration_u = hfs_pick$Duration_u))
  scaled <- merge(base, sample_key, by = "Template_ID", sort = FALSE)
  scaled$Start_s <- scaled$Start_u * scaled$B_s; scaled$End_s <- scaled$End_u * scaled$B_s; scaled$Duration_s <- scaled$Duration_u * scaled$B_s
  scaled <- scaled[order(scaled$Scale_Factor, match(scaled$Calibration_Mode, c("Burst", "Pause", "Tonic", "Broad_HFS")), scaled$Episode_ID), ]
  scaled$Calibration_Row <- seq_len(nrow(scaled))
  scaled[, c("Calibration_Row", "Calibration_Mode", "Episode_ID", "Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Start_s", "End_s", "Duration_s")]
}

make_distribution_summary_v23 <- function(intervals, events, states, separators, borderline) {
  event_key <- ifelse(events$Event_Label == "Burst", paste("Burst", events$Event_Context, events$Pulse_Strength_Stratum, sep = " / "),
                      paste("Pause", events$Pause_Subtype, sep = " / "))
  event_summary <- do.call(rbind, lapply(split(events, event_key), function(x) data.frame(
    Level = "primary_event", Component = event_key[match(x$Event_ID[1], events$Event_ID)], N = nrow(x),
    Median_Duration_u = median(x$Realized_Duration_u), Median_Spikes = median(x$N_Realized_Spikes),
    Median_ISI_u = NA_real_, Median_Rate_per_B = NA_real_, Median_CV = NA_real_)))
  state_key <- ifelse(states$State_Label == "Tonic", paste("Tonic", states$Tonic_Subtype, sep = " / "),
                      paste("Broad_HFS", states$Regularity_Regime, sep = " / "))
  state_summary <- do.call(rbind, lapply(split(states, state_key), function(x) data.frame(
    Level = "state", Component = state_key[match(x$State_Envelope_ID[1], states$State_Envelope_ID)], N = nrow(x),
    Median_Duration_u = median(x$Duration_u), Median_Spikes = median(x$N_Boundary_Spikes),
    Median_ISI_u = median(x$Observed_Mean_ISI_u), Median_Rate_per_B = median(1 / x$Observed_Mean_ISI_u), Median_CV = median(x$CV))))
  separator_summary <- data.frame(Level = "secondary_event", Component = "Contextual_Separator", N = nrow(separators),
                                  Median_Duration_u = median(separators$Duration_u), Median_Spikes = median(separators$N_Boundary_Spikes),
                                  Median_ISI_u = median(separators$Duration_u), Median_Rate_per_B = median(1 / separators$Duration_u), Median_CV = NA_real_)
  borderline_summary <- data.frame(Level = "secondary_event", Component = "Borderline_Gap", N = nrow(borderline),
                                   Median_Duration_u = median(borderline$Duration_u), Median_Spikes = median(borderline$N_Boundary_Spikes),
                                   Median_ISI_u = median(borderline$Duration_u), Median_Rate_per_B = median(1 / borderline$Duration_u), Median_CV = NA_real_)
  out <- rbind(event_summary, state_summary, separator_summary, borderline_summary); rownames(out) <- NULL; out
}

make_plots_v23 <- function(intervals, spikes, evidence, quota, output_dir) {
  x <- intervals
  x$State_Rail <- ifelse(x$State_Label == "Tonic", paste0("State: Tonic (", x$Tonic_Subtype, ")"),
                         ifelse(x$State_Label == "Broad_HFS", "State: Broad HFS", "State: none"))
  x$Event_Rail <- ifelse(x$Event_Label == "Burst", "Event: Burst",
                         ifelse(x$Event_Label == "Pause", paste0("Event: Pause (", x$Pause_Subtype, ")"), "Event: none"))
  x$Secondary_Rail <- ifelse(x$Contextual_Separator_Label == parameters$contextual_separator$secondary_label,
                             "Secondary: contextual separator",
                             ifelse(x$Borderline_Gap_Label == parameters$borderline_gap$secondary_label,
                                    "Secondary: borderline gap", "Secondary: none"))
  colors <- c("State: none" = "#D9D9D9", "State: Tonic (generic_stress)" = "#009E73",
              "State: Tonic (stn_like_empirical)" = "#33A02C", "State: Broad HFS" = "#CC6677",
              "Event: none" = "#D9D9D9", "Event: Burst" = "#E69F00", "Event: Pause (canonical)" = "#0072B2",
              "Event: Pause (complex_multi_gap)" = "#56B4E9", "Secondary: none" = "#D9D9D9",
              "Secondary: contextual separator" = "#9467BD",
              "Secondary: borderline gap" = "#A6761D")
  shown <- setdiff(names(colors), c("State: none", "Event: none", "Secondary: none"))
  template_ids <- sprintf("TPL_%03d", 1:20); y_map <- setNames(rev(seq_along(template_ids)), template_ids)
  anchor <- x[x$Scale_Factor == 1, ]; anchor$Y <- unname(y_map[anchor$Template_ID])
  anchor_spikes <- spikes[spikes$Scale_Factor == 1, ]; anchor_spikes$Y <- unname(y_map[anchor_spikes$Template_ID])
  base_plot <- ggplot2::ggplot(anchor) +
    ggplot2::geom_segment(ggplot2::aes(x = Start_u, xend = End_u, y = Y + .13, yend = Y + .13, color = State_Rail), linewidth = 1.7) +
    ggplot2::geom_segment(ggplot2::aes(x = Start_u, xend = End_u, y = Y, yend = Y, color = Event_Rail), linewidth = 1.7) +
    ggplot2::geom_segment(ggplot2::aes(x = Start_u, xend = End_u, y = Y - .13, yend = Y - .13, color = Secondary_Rail), linewidth = 1.3) +
    ggplot2::geom_segment(data = anchor_spikes, ggplot2::aes(x = Time_u, xend = Time_u, y = Y - .32, yend = Y + .32),
                          inherit.aes = FALSE, color = "#111111", linewidth = .18) +
    ggplot2::scale_color_manual(values = colors, breaks = shown, na.translate = FALSE) +
    ggplot2::scale_y_continuous(breaks = unname(y_map), labels = names(y_map), expand = ggplot2::expansion(add = .55)) +
    ggplot2::labs(x = "Dimensionless time (B units)", y = NULL, color = "Frozen truth",
                  title = "Twenty dimensionless v2.3 clean-overlap spike-train templates",
                  subtitle = "Black ticks: spikes | upper: State | middle: primary Event | lower: secondary gap labels") +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), legend.position = "top")
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "all_dimensionless_templates_multitrack.png"), base_plot,
                  width = 16, height = 12, dpi = 300, bg = "white")
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "all_dimensionless_templates_multitrack.pdf"), base_plot,
                  width = 16, height = 12, device = grDevices::pdf)
  selected <- sprintf("TPL_%03d", 1:6); pair_i <- x[x$Template_ID %in% selected, ]; pair_s <- spikes[spikes$Template_ID %in% selected, ]
  pair_map <- setNames(rev(seq_along(selected)), selected); pair_i$Y <- unname(pair_map[pair_i$Template_ID]); pair_s$Y <- unname(pair_map[pair_s$Template_ID])
  p2 <- ggplot2::ggplot(pair_i) +
    ggplot2::geom_segment(ggplot2::aes(x = Start_s, xend = End_s, y = Y + .12, yend = Y + .12, color = State_Rail), linewidth = 1.5) +
    ggplot2::geom_segment(ggplot2::aes(x = Start_s, xend = End_s, y = Y, yend = Y, color = Event_Rail), linewidth = 1.5) +
    ggplot2::geom_segment(ggplot2::aes(x = Start_s, xend = End_s, y = Y - .12, yend = Y - .12, color = Secondary_Rail), linewidth = 1.1) +
    ggplot2::geom_segment(data = pair_s, ggplot2::aes(x = Time_s, xend = Time_s, y = Y - .31, yend = Y + .31),
                          inherit.aes = FALSE, color = "#111111", linewidth = .18) +
    ggplot2::scale_color_manual(values = colors, breaks = shown, na.translate = FALSE) +
    ggplot2::scale_y_continuous(breaks = unname(pair_map), labels = names(pair_map)) +
    ggplot2::facet_wrap(~Scale_Factor, scales = "free_x", ncol = 1, labeller = ggplot2::label_both) +
    ggplot2::labs(x = "Time (s)", y = NULL, color = "Frozen truth", title = "Exact paired 1×, 4× and 10× time projections") +
    ggplot2::theme_classic(base_size = 10) + ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), legend.position = "top")
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "paired_scale_multitrack_overview.png"), p2,
                  width = 15, height = 13, dpi = 300, bg = "white")
  pheno <- evidence; pheno$Y <- unname(y_map[pheno$Template_ID])
  phenotype_colors <- c("no_burst_evidence" = "#BDBDBD", "ambiguous_burst_like" = "#9467BD",
                        "clear_burst_like" = "#D55E00")
  p3 <- base_plot +
    ggplot2::geom_segment(data = pheno, ggplot2::aes(x = Start_u, xend = End_u, y = Y - .24, yend = Y - .24),
                          inherit.aes = FALSE, linewidth = 1.0,
                          color = unname(phenotype_colors[pheno$Phenotype_Label])) +
    ggplot2::labs(title = "Mechanism truth and independently audited observable phenotype",
                  subtitle = "State, primary Event and secondary gaps remain distinct; thin bottom marks phenotype evidence")
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "phenotype_evidence_audit.png"), p3,
                  width = 16, height = 12, dpi = 300, bg = "white")
  q <- quota[quota$Context == "hfs", ]; balance <- as.data.frame(table(q$Split, q$HFS_Regime, q$Strength), stringsAsFactors = FALSE)
  names(balance) <- c("Split", "HFS_Regime", "Strength", "N")
  p4 <- ggplot2::ggplot(balance, ggplot2::aes(Strength, HFS_Regime, fill = N)) +
    ggplot2::geom_tile(color = "white") + ggplot2::geom_text(ggplot2::aes(label = N), size = 5) +
    ggplot2::facet_wrap(~Split) + ggplot2::scale_fill_gradient(low = "#EFF3FF", high = "#2171B5") +
    ggplot2::labs(x = "Frozen Burst strength", y = "HFS regularity", title = "Frozen HFS-internal Burst quota coverage") + ggplot2::theme_classic(base_size = 11)
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "hfs_burst_quota_balance.png"), p4,
                  width = 8, height = 4.8, dpi = 300, bg = "white")

  overlap <- x[x$Scale_Factor == 1, , drop = FALSE]
  overlap$Overlap_Component <- ifelse(overlap$Event_Label == "Burst", "Burst",
    ifelse(overlap$Direct_HFS_Support, "Broad HFS direct",
      ifelse(overlap$State_Label == "Tonic", "Tonic strict",
        ifelse(overlap$Event_Label == "Pause", "Primary Pause",
          ifelse(overlap$Borderline_Gap_Label == parameters$borderline_gap$secondary_label,
                 "Borderline gap (secondary)", NA_character_)))))
  overlap <- overlap[!is.na(overlap$Overlap_Component), , drop = FALSE]
  overlap$Overlap_Component <- factor(overlap$Overlap_Component,
    levels = c("Burst", "Broad HFS direct", "Tonic strict", "Borderline gap (secondary)", "Primary Pause"))
  overlap_colors <- c("Burst" = "#E69F00", "Broad HFS direct" = "#CC6677", "Tonic strict" = "#009E73",
                      "Borderline gap (secondary)" = "#A6761D", "Primary Pause" = "#0072B2")
  p5 <- ggplot2::ggplot(overlap, ggplot2::aes(x = ISI_u, color = Overlap_Component)) +
    ggplot2::stat_ecdf(linewidth = 1.0, geom = "step") +
    ggplot2::scale_x_log10() + ggplot2::scale_color_manual(values = overlap_colors, drop = FALSE) +
    ggplot2::labs(x = "Dimensionless ISI (B units, log scale)", y = "Empirical cumulative proportion",
                  color = "Mechanism component", title = "Dimensionless ISI overlap audit",
                  subtitle = "Descriptive mother-template distribution; no detector output or acceptance target is used") +
    ggplot2::theme_classic(base_size = 11) + ggplot2::theme(legend.position = "top")
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "dimensionless_isi_overlap_ecdf.png"), p5,
                  width = 10, height = 6, dpi = 300, bg = "white")
  ggplot2::ggsave(file.path(output_dir, "qc", "figures", "dimensionless_isi_overlap_ecdf.pdf"), p5,
                  width = 10, height = 6, device = grDevices::pdf)
}
