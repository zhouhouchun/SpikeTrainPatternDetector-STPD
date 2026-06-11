# DBS trajectory-style spike train figure helpers.
# The view is a schematic visualization: anatomical shapes orient the reader,
# while electrode depth and spike ticks are derived from the loaded dataset.

stpd_dbs_track_even_sample <- function(n, max_n) {
  n <- suppressWarnings(as.integer(n %||% 0L))[1]
  max_n <- suppressWarnings(as.integer(max_n %||% n))[1]
  if (!is.finite(n) || n <= 0L) return(integer(0))
  if (!is.finite(max_n) || max_n <= 0L || n <= max_n) return(seq_len(n))
  unique(pmax(1L, pmin(n, round(seq(1, n, length.out = max_n)))))
}

stpd_dbs_track_patterns <- function() {
  c("burst", "long_burst", "possible_burst", "tonic",
    "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
}

stpd_dbs_track_metadata <- function(ds, metadata = NULL) {
  if (is.null(ds) || is.null(ds$trains)) return(data.frame())
  trains <- names(ds$trains)
  if (length(trains) == 0) return(data.frame())
  meta <- metadata
  if (is.null(meta) || !is.data.frame(meta) || !("train" %in% names(meta))) {
    meta <- ds$meta$train_metadata %||% NULL
  }
  if (is.null(meta) || !is.data.frame(meta) || !("train" %in% names(meta))) {
    meta <- parse_spike_train_column_metadata(trains, dataset_name = ds$meta$display_name %||% "")
  }
  meta <- meta[as.character(meta$train) %in% trains, , drop = FALSE]
  if (!("side" %in% names(meta))) meta$side <- NA_character_
  if (!("structure" %in% names(meta))) meta$structure <- "unknown"
  if (!("recording_depth" %in% names(meta))) meta$recording_depth <- NA_real_
  meta$side <- toupper(as.character(meta$side))
  meta$structure <- as.character(meta$structure)
  meta$recording_depth <- suppressWarnings(as.numeric(meta$recording_depth))
  meta
}

stpd_dbs_track_combine_datasets <- function(datasets, ids = NULL) {
  if (is.null(datasets) || length(datasets) == 0L) {
    return(list(trains = list(), meta = list(display_name = "Combined DBS datasets", source = "combined", train_metadata = data.frame())))
  }
  if (is.null(ids) || length(ids) != length(datasets)) ids <- names(datasets)
  if (is.null(ids) || length(ids) != length(datasets)) ids <- paste0("dataset_", seq_along(datasets))
  ids <- as.character(ids)
  bad_id <- is.na(ids) | !nzchar(ids)
  ids[bad_id] <- paste0("dataset_", which(bad_id))

  out_trains <- list()
  meta_list <- list()
  used_names <- character(0)
  display_names <- character(0)
  unit_in <- NULL

  for (ii in seq_along(datasets)) {
    ds <- datasets[[ii]]
    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0L) next
    id <- ids[ii]
    display <- as.character(ds$meta$display_name %||% id)[1]
    if (is.na(display) || !nzchar(display)) display <- id
    display_names <- c(display_names, display)
    unit_in <- unit_in %||% (ds$meta$unit_in %||% "s")

    meta <- stpd_dbs_track_metadata(ds)
    if (is.null(meta) || nrow(meta) == 0L) next
    meta$source_train <- as.character(meta$train)
    meta$dataset <- display
    meta$dataset_id <- id

    train_map <- setNames(character(0), character(0))
    for (tr in names(ds$trains)) {
      base_nm <- paste(display, tr, sep = "::")
      nm <- base_nm
      kk <- 2L
      while (nm %in% used_names) {
        nm <- paste0(base_nm, "#", kk)
        kk <- kk + 1L
      }
      used_names <- c(used_names, nm)
      out_trains[[nm]] <- ds$trains[[tr]]
      train_map[[tr]] <- nm
    }
    meta$train <- unname(train_map[as.character(meta$source_train)])
    meta <- meta[!is.na(meta$train) & nzchar(meta$train), , drop = FALSE]
    meta_list[[length(meta_list) + 1L]] <- meta
  }

  display_names <- unique(display_names[nzchar(display_names)])
  list(
    trains = out_trains,
    meta = list(
      display_name = if (length(display_names) == 0L) "Combined DBS datasets" else paste(display_names, collapse = " + "),
      source = "combined",
      unit_in = unit_in %||% "s",
      train_metadata = dplyr::bind_rows(meta_list)
    )
  )
}

stpd_dbs_track_collapse_nucleus_labels <- function(labels) {
  if (!is.data.frame(labels) || nrow(labels) == 0L || !("label" %in% names(labels))) {
    return(labels)
  }
  if (!("active" %in% names(labels))) labels$active <- FALSE
  if (!("represented" %in% names(labels))) labels$represented <- labels$active
  labels$active <- isTRUE(labels$active) | as.logical(labels$active)
  labels$represented <- isTRUE(labels$represented) | as.logical(labels$represented)
  labels$active[is.na(labels$active)] <- FALSE
  labels$represented[is.na(labels$represented)] <- FALSE

  out <- lapply(split(labels, as.character(labels$label)), function(x) {
    x_num <- suppressWarnings(as.numeric(x$x))
    y_num <- suppressWarnings(as.numeric(x$y))
    x_num <- x_num[is.finite(x_num)]
    y_num <- y_num[is.finite(y_num)]
    if (length(x_num) == 0L || length(y_num) == 0L) return(data.frame())
    data.frame(
      x = mean(range(x_num)),
      y = mean(range(y_num)),
      label = as.character(x$label[1]),
      side = "center",
      represented = any(x$represented),
      active = any(x$active),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(out)
}

stpd_dbs_track_prepare <- function(ds, metadata = NULL, selected_trains = NULL,
                                   structures = NULL, sides = c("L", "R"),
                                   start_sec = 0, window_sec = 0.5,
                                   max_trains_per_side = 0,
                                   time_origin = c("aligned", "raw"),
                                   pattern_mode = c("audit_final", "final", "auto", "manual", "none"),
                                   auto_others = FALSE,
                                   min_isi_sec = 0.001) {
  time_origin <- match.arg(time_origin)
  pattern_mode <- match.arg(pattern_mode)
  meta <- stpd_dbs_track_metadata(ds, metadata = metadata)
  if (nrow(meta) == 0) return(list(rows = data.frame(), spikes = data.frame(), meta = meta))

  start_sec <- suppressWarnings(as.numeric(start_sec %||% 0))[1]
  window_sec <- suppressWarnings(as.numeric(window_sec %||% 0.5))[1]
  if (!is.finite(start_sec) || start_sec < 0) start_sec <- 0
  if (!is.finite(window_sec) || window_sec <= 0) window_sec <- 0.5
  min_isi_sec <- suppressWarnings(as.numeric(min_isi_sec %||% 0.001))[1]
  if (!is.finite(min_isi_sec) || min_isi_sec < 0) min_isi_sec <- 0.001

  sides <- toupper(as.character(sides %||% c("L", "R")))
  sides <- intersect(sides, c("L", "R"))
  if (length(sides) == 0) sides <- c("L", "R")

  structures <- as.character(structures %||% character(0))
  structures <- structures[!is.na(structures) & nzchar(structures)]

  keep <- !is.na(meta$side) & meta$side %in% sides & is.finite(meta$recording_depth)
  if (length(structures) > 0) keep <- keep & as.character(meta$structure) %in% structures
  depth_reference <- meta[keep, , drop = FALSE]
  if (nrow(depth_reference) > 0) {
    ref_cols <- intersect(c("train", "structure", "side", "trajectory", "recording_depth", "recording_depth_label"), names(depth_reference))
    depth_reference <- unique(depth_reference[, ref_cols, drop = FALSE])
  }
  if (!is.null(selected_trains) && length(selected_trains) > 0) {
    keep <- keep & as.character(meta$train) %in% as.character(selected_trains)
  }
  rows <- meta[keep, , drop = FALSE]
  if (nrow(rows) == 0) return(list(rows = rows, spikes = data.frame(), meta = meta, depth_reference = depth_reference))

  order_cols <- c("side", "structure", "trajectory", "recording_depth", "train")
  order_cols <- intersect(order_cols, names(rows))
  rows <- rows[do.call(order, c(rows[order_cols], list(na.last = TRUE))), , drop = FALSE]

  max_trains_per_side <- suppressWarnings(as.integer(max_trains_per_side %||% 0L))[1]
  if (!is.finite(max_trains_per_side)) max_trains_per_side <- 0L
  rows <- dplyr::bind_rows(lapply(split(rows, rows$side), function(x) {
    x[stpd_dbs_track_even_sample(nrow(x), max_trains_per_side), , drop = FALSE]
  }))
  if (nrow(rows) == 0) return(list(rows = rows, spikes = data.frame(), meta = meta, depth_reference = depth_reference))
  rows$display_index <- seq_len(nrow(rows))

  train_map <- ds$trains
  spike_rows <- lapply(seq_len(nrow(rows)), function(ii) {
    tr <- as.character(rows$train[ii])
    dat <- train_map[[tr]]
    if (is.null(dat) || !("timestamp_sec" %in% names(dat))) return(data.frame())
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ts <- ts[is.finite(ts)]
    if (length(ts) == 0) return(data.frame())
    t_plot <- if (identical(time_origin, "aligned")) ts - min(ts, na.rm = TRUE) else ts
    keep_sp <- t_plot >= start_sec & t_plot <= (start_sec + window_sec)
    if (!any(keep_sp)) {
      return(data.frame(
        train = character(0), spike_time_sec = numeric(0), spike_time_rel = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      train = tr,
      spike_time_sec = t_plot[keep_sp],
      spike_time_rel = (t_plot[keep_sp] - start_sec) / window_sec,
      stringsAsFactors = FALSE
    )
  })
  spikes <- dplyr::bind_rows(spike_rows)
  if (nrow(spikes) > 0) spikes <- dplyr::left_join(spikes, rows[, c("train", "display_index"), drop = FALSE], by = "train")

  pattern_rows <- lapply(seq_len(nrow(rows)), function(ii) {
    if (identical(pattern_mode, "none")) return(data.frame())
    tr <- as.character(rows$train[ii])
    dat <- train_map[[tr]]
    if (is.null(dat) || !("timestamp_sec" %in% names(dat))) return(data.frame())
    n <- nrow(dat)
    if (!is.finite(n) || n < 2L) return(data.frame())
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    if (length(ts) != n || !any(is.finite(ts))) return(data.frame())
    t_plot <- if (identical(time_origin, "aligned")) ts - min(ts, na.rm = TRUE) else ts
    manual <- as.character(dat$pattern_manual %||% rep("", n)); manual[is.na(manual)] <- ""
    auto <- as.character(dat$pattern_auto %||% rep("", n)); auto[is.na(auto)] <- ""
    isi <- suppressWarnings(as.numeric(dat$ISI_sec %||% rep(NA_real_, n)))
    final <- compute_final_pattern(manual, auto, isi, auto_others = isTRUE(auto_others), min_isi_sec = min_isi_sec)
    audit_final <- stpd_audit_final_labels(dat, min_isi_sec = min_isi_sec,
                                           auto_others = isTRUE(auto_others),
                                           prefer_stored = TRUE)
    pat <- switch(pattern_mode, manual = manual, auto = auto, final = final,
                  audit_final = audit_final, final)
    source_kind <- if (identical(pattern_mode, "manual")) {
      rep("manual", n)
    } else if (identical(pattern_mode, "auto")) {
      rep("auto", n)
    } else {
      ifelse(manual != "", "manual", ifelse(auto != "", "auto", "final"))
    }
    idx <- seq.int(2L, n)
    p2 <- as.character(pat[idx]); p2[is.na(p2)] <- ""
    keep_pat <- nzchar(p2) & p2 %in% stpd_dbs_track_patterns()
    if (!any(keep_pat)) return(data.frame())
    s <- t_plot[idx - 1L]
    e <- t_plot[idx]
    keep_window <- is.finite(s) & is.finite(e) & e >= start_sec & s <= (start_sec + window_sec) & e > s
    keep <- keep_pat & keep_window
    if (!any(keep)) return(data.frame())
    idx <- idx[keep]
    s <- pmax(s[keep], start_sec)
    e <- pmin(e[keep], start_sec + window_sec)
    data.frame(
      train = tr,
      idx = idx,
      pattern = p2[keep],
      source_kind = source_kind[idx],
      start_sec = s,
      end_sec = e,
      start_rel = (s - start_sec) / window_sec,
      end_rel = (e - start_sec) / window_sec,
      stringsAsFactors = FALSE
    )
  })
  pattern_segments <- dplyr::bind_rows(pattern_rows)
  if (nrow(pattern_segments) > 0) {
    pattern_segments <- dplyr::left_join(
      pattern_segments,
      rows[, c("train", "display_index"), drop = FALSE],
      by = "train"
    )
  }

  rows$n_spikes_window <- vapply(as.character(rows$train), function(tr) sum(as.character(spikes$train) == tr), integer(1))
  rows$window_start_sec <- start_sec
  rows$window_end_sec <- start_sec + window_sec
  rows$window_sec <- window_sec
  rows$time_origin <- time_origin

  list(rows = rows, spikes = spikes, pattern_segments = pattern_segments, meta = meta, depth_reference = depth_reference)
}

stpd_dbs_track_ellipse <- function(cx, cy, rx, ry, n = 160, angle = 0) {
  theta <- seq(0, 2 * pi, length.out = n)
  ca <- cos(angle); sa <- sin(angle)
  x0 <- rx * cos(theta); y0 <- ry * sin(theta)
  data.frame(x = cx + x0 * ca - y0 * sa, y = cy + x0 * sa + y0 * ca)
}

stpd_dbs_track_smooth_closed <- function(pts, n = 140) {
  pts <- pts[is.finite(pts$x) & is.finite(pts$y), , drop = FALSE]
  if (nrow(pts) < 4) return(pts)
  n <- suppressWarnings(as.integer(n %||% 140L))[1]
  if (!is.finite(n) || n < 16L) n <- 140L
  pts <- rbind(pts, pts[1, , drop = FALSE])
  tt <- seq_len(nrow(pts))
  out <- seq(1, nrow(pts), length.out = n)
  data.frame(
    x = stats::spline(tt, pts$x, xout = out, method = "periodic")$y,
    y = stats::spline(tt, pts$y, xout = out, method = "periodic")$y
  )
}

stpd_dbs_track_transform_points <- function(pts, cx, cy, mirror = 1, angle = 0,
                                            scale_x = 1, scale_y = 1) {
  scale_x <- suppressWarnings(as.numeric(scale_x %||% 1))[1]
  scale_y <- suppressWarnings(as.numeric(scale_y %||% 1))[1]
  if (!is.finite(scale_x) || scale_x <= 0) scale_x <- 1
  if (!is.finite(scale_y) || scale_y <= 0) scale_y <- 1
  x0 <- suppressWarnings(as.numeric(pts$x)) * scale_x * mirror
  y0 <- suppressWarnings(as.numeric(pts$y)) * scale_y
  ca <- cos(angle); sa <- sin(angle)
  data.frame(
    x = cx + x0 * ca - y0 * sa,
    y = cy + x0 * sa + y0 * ca
  )
}

stpd_dbs_track_nucleus_catalog <- function() {
  list(
    GPe = list(
      name = "GPe",
      center_y = -0.10,
      angle = 0.06,
      label_dx = 0.60,
      label_dy = 0.02,
      color = "rgba(240,90,40,0.54)",
      points = data.frame(
        x = c(-0.58, -0.68, -0.66, -0.54, -0.34, -0.08, 0.22, 0.48, 0.63, 0.64, 0.50, 0.24, -0.06, -0.34, -0.54, -0.66),
        y = c(-0.62, -0.40, -0.12,  0.20,  0.48,  0.67, 0.70, 0.56, 0.32, 0.04, -0.24, -0.48, -0.64, -0.71, -0.70, -0.60)
      )
    ),
    GPi = list(
      name = "GPi",
      center_y = -0.50,
      angle = -0.06,
      label_dx = 0.54,
      label_dy = -0.01,
      color = "rgba(14,165,233,0.48)",
      points = data.frame(
        x = c(-0.62, -0.52, -0.34, -0.10, 0.18, 0.40, 0.53, 0.47, 0.27, -0.02, -0.30, -0.52, -0.64),
        y = c(-0.43, -0.18,  0.02,  0.16, 0.14, 0.02, -0.20, -0.42, -0.59, -0.69, -0.65, -0.55, -0.46)
      )
    ),
    STN = list(
      name = "STN",
      center_y = -0.92,
      angle = -0.30,
      label_dx = 0.42,
      label_dy = -0.01,
      color = "rgba(34,197,94,0.52)",
      points = data.frame(
        x = c(-0.55, -0.36, -0.10, 0.20, 0.47, 0.60, 0.54, 0.31, 0.02, -0.30, -0.54),
        y = c(-0.03,  0.13,  0.21, 0.18, 0.05, -0.12, -0.29, -0.41, -0.43, -0.32, -0.12)
      )
    )
  )
}

stpd_dbs_track_nucleus_shape <- function(name, side_x = 0.72, n = 140,
                                         scale_x = 1, scale_y = 1,
                                         center_y = NULL) {
  catalog <- stpd_dbs_track_nucleus_catalog()
  nu <- catalog[[as.character(name)[1]]]
  if (is.null(nu)) stop("Unknown DBS trajectory nucleus: ", as.character(name)[1], call. = FALSE)
  side_x <- suppressWarnings(as.numeric(side_x))[1]
  if (!is.finite(side_x)) side_x <- 0.72
  center_y <- suppressWarnings(as.numeric(center_y %||% nu$center_y))[1]
  if (!is.finite(center_y)) center_y <- nu$center_y
  mirror <- if (side_x < 0) -1 else 1
  pts <- stpd_dbs_track_transform_points(
    nu$points,
    cx = side_x,
    cy = center_y,
    mirror = mirror,
    angle = mirror * (nu$angle %||% 0),
    scale_x = scale_x,
    scale_y = scale_y
  )
  list(
    name = nu$name,
    points = stpd_dbs_track_smooth_closed(pts, n = n),
    label_x = side_x + mirror * (nu$label_dx %||% 0.38),
    label_y = center_y + (nu$label_dy %||% 0),
    color = nu$color,
    center_y = center_y
  )
}

stpd_dbs_track_focus_side_x <- function(side) {
  side <- toupper(as.character(side %||% NA_character_))
  ifelse(side == "L", -1.62, 1.62)
}

stpd_dbs_track_focus_bounds <- function() {
  c(top = 1.25, bottom = -1.25)
}

stpd_dbs_track_axis_range <- function(..., pad = 0.35, fallback = c(-1, 1)) {
  vals <- suppressWarnings(as.numeric(unlist(list(...), use.names = FALSE)))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) return(fallback)
  rng <- range(vals, na.rm = TRUE)
  if (!all(is.finite(rng))) return(fallback)
  pad <- suppressWarnings(as.numeric(pad %||% 0))[1]
  if (!is.finite(pad) || pad < 0) pad <- 0
  if (abs(diff(rng)) < 1e-8) rng <- rng + c(-1, 1)
  rng + c(-pad, pad)
}

stpd_dbs_track_raster_bounds <- function(side, length_multiplier = 1.5) {
  side <- toupper(as.character(side %||% NA_character_))
  length_multiplier <- suppressWarnings(as.numeric(length_multiplier %||% 1.5))[1]
  if (!is.finite(length_multiplier) || length_multiplier <= 0) length_multiplier <- 1.5
  near_offset <- 2.05
  base_length <- 2.20
  raster_length <- base_length * length_multiplier
  near_x <- ifelse(side == "L", -near_offset, near_offset)
  data.frame(
    x0 = ifelse(side == "L", near_x - raster_length, near_x),
    x1 = ifelse(side == "L", near_x, near_x + raster_length),
    stringsAsFactors = FALSE
  )
}

stpd_dbs_track_focus_nucleus_shape <- function(name, side_x = 0.38,
                                               y_top = 1.25, y_bottom = -1.25,
                                               width = NULL, n = 180) {
  catalog <- stpd_dbs_track_nucleus_catalog()
  nu <- catalog[[as.character(name)[1]]]
  if (is.null(nu)) stop("Unknown DBS trajectory nucleus: ", as.character(name)[1], call. = FALSE)
  side_x <- suppressWarnings(as.numeric(side_x))[1]
  if (!is.finite(side_x)) side_x <- 0.38
  y_top <- suppressWarnings(as.numeric(y_top %||% 1.25))[1]
  y_bottom <- suppressWarnings(as.numeric(y_bottom %||% -1.25))[1]
  if (!is.finite(y_top)) y_top <- 1.25
  if (!is.finite(y_bottom)) y_bottom <- -1.25
  if (y_top <= y_bottom) {
    tmp <- y_top
    y_top <- y_bottom
    y_bottom <- tmp
  }
  default_width <- switch(as.character(name)[1], GPe = 1.72, GPi = 1.38, STN = 2.10, 1.34)
  width <- suppressWarnings(as.numeric(width %||% default_width))[1]
  if (!is.finite(width) || width <= 0) width <- default_width
  n_val <- suppressWarnings(as.integer(n %||% 180L))[1]
  if (!is.finite(n_val) || n_val < 56L) n_val <- 180L
  mirror <- if (side_x < 0) -1 else 1
  target_h <- y_top - y_bottom
  center_y <- (y_top + y_bottom) / 2
  if (identical(as.character(name)[1], "STN")) {
    cap_half <- max(0.20, width * 0.105)
    x_rel <- c(
      cap_half,
      width * 0.13, width * 0.20, width * 0.26, width * 0.30, width * 0.32,
      width * 0.30, width * 0.25, width * 0.17, cap_half,
      -cap_half,
      -width * 0.18, -width * 0.31, -width * 0.45, -width * 0.56,
      -width * 0.64, -width * 0.67, -width * 0.65, -width * 0.58,
      -width * 0.48, -width * 0.36, -width * 0.24, -cap_half,
      cap_half
    )
    y_frac <- c(
      1.00,
      0.88, 0.74, 0.58, 0.39, 0.18, -0.04, -0.28, -0.56, -1.00,
      -1.00,
      -0.96, -0.84, -0.66, -0.42, -0.16, 0.10, 0.34, 0.55,
      0.72, 0.85, 0.94, 1.00,
      1.00
    )
    y_abs <- center_y + y_frac * target_h / 2
    pts <- data.frame(
      x = side_x + mirror * x_rel,
      y = y_abs,
      stringsAsFactors = FALSE
    )
    return(list(
      name = nu$name,
      points = pts,
      label_x = side_x - sign(side_x) * width * 0.28,
      label_y = center_y - target_h * 0.12,
      color = nu$color,
      center_y = center_y
    ))
  }
  base <- stpd_dbs_track_smooth_closed(nu$points, n = n_val)
  bx <- range(base$x, na.rm = TRUE)
  by <- range(base$y, na.rm = TRUE)
  sx <- width / max(diff(bx), 1e-6)
  sy <- target_h / max(diff(by), 1e-6)
  pts <- data.frame(
    x = side_x + mirror * (base$x - mean(bx)) * sx,
    y = center_y + (base$y - mean(by)) * sy,
    stringsAsFactors = FALSE
  )
  pts$x <- pts$x + mirror * 0.095 * (center_y - pts$y) / max(target_h, 1e-6)
  cap_half <- max(0.18, width * 0.15)
  support <- data.frame(
    x = c(side_x - cap_half, side_x + cap_half, side_x - cap_half, side_x + cap_half),
    y = c(y_top, y_top, y_bottom, y_bottom),
    stringsAsFactors = FALSE
  )
  pts_all <- rbind(pts, support)
  hull <- grDevices::chull(pts_all$x, pts_all$y)
  pts <- pts_all[c(hull, hull[1]), , drop = FALSE]
  list(
    name = nu$name,
    points = pts,
    label_x = side_x + sign(side_x) * width * 0.36,
    label_y = (y_top + y_bottom) / 2,
    color = nu$color,
    center_y = (y_top + y_bottom) / 2
  )
}

stpd_dbs_track_context_nucleus_shape <- function(name, side_x = 0.38,
                                                 y_top = 1.25, y_bottom = -1.25,
                                                 n = 180) {
  name <- as.character(name %||% "STN")[1]
  side_x <- suppressWarnings(as.numeric(side_x))[1]
  if (!is.finite(side_x)) side_x <- 0.38
  y_top <- suppressWarnings(as.numeric(y_top %||% 1.25))[1]
  y_bottom <- suppressWarnings(as.numeric(y_bottom %||% -1.25))[1]
  if (!is.finite(y_top)) y_top <- 1.25
  if (!is.finite(y_bottom)) y_bottom <- -1.25
  if (y_top <= y_bottom) {
    tmp <- y_top
    y_top <- y_bottom
    y_bottom <- tmp
  }
  span <- y_top - y_bottom
  mirror <- if (side_x < 0) -1 else 1
  spec <- switch(
    name,
    GPe = list(dx =  0.60, cy = y_top - 0.39 * span, sx = 1.18, sy = 1.22, label_dx =  0.46),
    GPi = list(dx =  0.30, cy = y_top - 0.58 * span, sx = 1.22, sy = 1.42, label_dx =  0.36),
    STN = list(dx =  0.18, cy = y_top - 0.86 * span, sx = 0.86, sy = 0.72, label_dx =  0.22),
    list(dx = 0, cy = (y_top + y_bottom) / 2, sx = 1.1, sy = 1.5, label_dx = 0.32)
  )
  shape <- stpd_dbs_track_nucleus_shape(
    name,
    side_x = side_x + mirror * spec$dx,
    n = n,
    scale_x = spec$sx,
    scale_y = spec$sy,
    center_y = spec$cy
  )
  shape$label_x <- side_x + mirror * spec$dx + mirror * spec$label_dx
  shape$label_y <- spec$cy
  shape
}

stpd_dbs_track_context_model_shape <- function(name, target_structure = NULL, side_x = 0.38,
                                               y_top = 1.25, y_bottom = -1.25,
                                               n = 180) {
  catalog <- stpd_dbs_track_nucleus_catalog()
  name <- as.character(name %||% "STN")[1]
  if (!(name %in% names(catalog))) name <- "STN"
  target_structure <- as.character(target_structure %||% name)[1]
  if (!(target_structure %in% names(catalog))) target_structure <- name
  side_x <- suppressWarnings(as.numeric(side_x))[1]
  if (!is.finite(side_x)) side_x <- 0.38
  y_top <- suppressWarnings(as.numeric(y_top %||% 1.25))[1]
  y_bottom <- suppressWarnings(as.numeric(y_bottom %||% -1.25))[1]
  if (!is.finite(y_top)) y_top <- 1.25
  if (!is.finite(y_bottom)) y_bottom <- -1.25
  if (y_top <= y_bottom) {
    tmp <- y_top
    y_top <- y_bottom
    y_bottom <- tmp
  }
  if (identical(name, target_structure)) {
    shape <- stpd_dbs_track_focus_nucleus_shape(
      name,
      side_x = side_x,
      y_top = y_top,
      y_bottom = y_bottom,
      n = n
    )
    shape$model_target <- target_structure
    shape$model_scale <- 1
    return(shape)
  }
  target_shape <- stpd_dbs_track_context_nucleus_shape(
    target_structure,
    side_x = side_x,
    y_top = y_top,
    y_bottom = y_bottom,
    n = n
  )
  target_pts <- target_shape$points
  target_h <- diff(range(target_pts$y, na.rm = TRUE))
  scale <- (y_top - y_bottom) / max(target_h, 1e-6)
  target_cx <- mean(range(target_pts$x, na.rm = TRUE))
  target_cy <- mean(range(target_pts$y, na.rm = TRUE))
  desired_cx <- side_x
  desired_cy <- (y_top + y_bottom) / 2

  shape <- stpd_dbs_track_context_nucleus_shape(
    name,
    side_x = side_x,
    y_top = y_top,
    y_bottom = y_bottom,
    n = n
  )
  pts <- shape$points
  pts$x <- desired_cx + (pts$x - target_cx) * scale
  pts$y <- desired_cy + (pts$y - target_cy) * scale
  shape$points <- pts
  shape$label_x <- desired_cx + (shape$label_x - target_cx) * scale
  shape$label_y <- desired_cy + (shape$label_y - target_cy) * scale
  shape$center_y <- desired_cy + (shape$center_y - target_cy) * scale
  shape$model_target <- target_structure
  shape$model_scale <- scale
  shape
}

stpd_dbs_track_add_polygon <- function(p, pts, fill = "#ffffff", line = "#111827", opacity = 1,
                                       name = "", hoverinfo = "skip") {
  add_trace(
    p, data = pts, x = ~x, y = ~y,
    type = "scatter", mode = "lines", fill = "toself",
    fillcolor = fill, line = list(color = line, width = 1.2),
    opacity = opacity, hoverinfo = hoverinfo, name = name,
    showlegend = FALSE, inherit = FALSE
  )
}

stpd_dbs_track_rescale_depth <- function(depth, reference_depth = NULL,
                                         direction = c("larger_deeper", "larger_shallower"),
                                         y_top = 1.15, y_bottom = -1.30) {
  direction <- match.arg(direction)
  depth <- suppressWarnings(as.numeric(depth))
  reference_depth <- suppressWarnings(as.numeric(reference_depth %||% depth))
  reference_depth <- reference_depth[is.finite(reference_depth)]
  y_top <- suppressWarnings(as.numeric(y_top %||% 1.15))[1]
  y_bottom <- suppressWarnings(as.numeric(y_bottom %||% -1.30))[1]
  if (!is.finite(y_top)) y_top <- 1.15
  if (!is.finite(y_bottom)) y_bottom <- -1.30
  if (!any(is.finite(depth))) return(rep(0, length(depth)))
  if (length(reference_depth) == 0) reference_depth <- depth[is.finite(depth)]
  rng <- range(reference_depth, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || abs(diff(rng)) < 1e-9) {
    return(rep(0, length(depth)))
  }
  pos <- (depth - rng[1]) / diff(rng)
  if (identical(direction, "larger_shallower")) {
    y_bottom + pos * (y_top - y_bottom)
  } else {
    y_top - pos * (y_top - y_bottom)
  }
}

stpd_dbs_track_depth_ticks <- function(depth, max_ticks = 8L) {
  depth <- sort(unique(suppressWarnings(as.numeric(depth))))
  depth <- depth[is.finite(depth)]
  if (length(depth) <= max_ticks) return(depth)
  rng <- range(depth, na.rm = TRUE)
  ticks <- pretty(rng, n = max(3L, max_ticks - 1L))
  ticks <- ticks[ticks >= rng[1] & ticks <= rng[2]]
  sort(unique(ticks))
}

stpd_dbs_track_trajectory_label <- function(x) {
  x <- toupper(as.character(x %||% NA_character_))
  x[is.na(x) | !nzchar(x)] <- "T?"
  x
}

stpd_dbs_track_trajectory_offsets <- function(x, levels = NULL, span = 0.24) {
  labels <- stpd_dbs_track_trajectory_label(x)
  if (is.null(levels) || length(levels) == 0L) {
    levels <- sort(unique(labels))
  } else {
    levels <- sort(unique(stpd_dbs_track_trajectory_label(levels)))
  }
  if (length(levels) <= 1L) {
    out <- rep(0, length(labels))
  } else {
    offsets <- seq(-span / 2, span / 2, length.out = length(levels))
    names(offsets) <- levels
    out <- unname(offsets[labels])
  }
  out[!is.finite(out)] <- 0
  out
}

stpd_dbs_track_display_lanes <- function(side, depth_y, preferred_gap = 0.24,
                                         y_min = NULL, y_max = NULL) {
  depth_y <- suppressWarnings(as.numeric(depth_y))
  n <- length(depth_y)
  if (n == 0) return(numeric(0))
  side <- rep_len(toupper(as.character(side %||% NA_character_)), n)
  out <- depth_y
  preferred_gap <- suppressWarnings(as.numeric(preferred_gap %||% 0.24))[1]
  if (!is.finite(preferred_gap) || preferred_gap <= 0) preferred_gap <- 0.24
  y_min <- suppressWarnings(as.numeric(y_min %||% NA_real_))[1]
  y_max <- suppressWarnings(as.numeric(y_max %||% NA_real_))[1]
  for (ss in unique(side)) {
    idx <- which(side == ss & is.finite(depth_y))
    if (length(idx) <= 1L) next
    ord <- idx[order(-depth_y[idx], seq_along(idx), na.last = TRUE)]
    original <- depth_y[ord]
    if (is.finite(y_min) && is.finite(y_max) && y_max > y_min) {
      if (diff(range(original, na.rm = TRUE)) < 1e-9) {
        span <- min(preferred_gap * max(0L, length(ord) - 1L), y_max - y_min)
        adjusted <- mean(c(y_min, y_max)) + seq(span / 2, -span / 2, length.out = length(ord))
      } else {
        gap <- min(preferred_gap, (y_max - y_min) / max(1L, length(ord) - 1L))
        adjusted <- y_max - (seq_along(ord) - 1L) * gap
        if (length(adjusted) > 1L && tail(adjusted, 1) > y_min) {
          adjusted <- seq(y_max, y_min, length.out = length(ord))
        }
      }
    } else {
      adjusted <- original
      for (ii in seq.int(2L, length(adjusted))) {
        adjusted[ii] <- min(adjusted[ii], adjusted[ii - 1L] - preferred_gap)
      }
      adjusted <- adjusted + (mean(original) - mean(adjusted))
    }
    out[ord] <- adjusted
  }
  out
}

stpd_dbs_track_dot_base_color <- function(structure) {
  structure <- as.character(structure %||% "STN")[1]
  switch(
    structure,
    GPe = "#f05a28",
    GPi = "#0ea5e9",
    STN = "#22c55e",
    "#8b5cf6"
  )
}

stpd_dbs_track_structure_label_color <- function(structure) {
  structure <- as.character(structure %||% "STN")[1]
  switch(
    structure,
    GPe = "#c2410c",
    GPi = "#2563eb",
    STN = "#16a34a",
    "#0f172a"
  )
}

stpd_dbs_track_draw_groups <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L || !("group" %in% names(df))) return(character(0))
  group_info <- unique(df[, intersect(c("group", "active", "side", "structure", "depth_layer"), names(df)), drop = FALSE])
  if (!("active" %in% names(group_info))) group_info$active <- FALSE
  if (!("side" %in% names(group_info))) group_info$side <- ""
  if (!("structure" %in% names(group_info))) group_info$structure <- ""
  if (!("depth_layer" %in% names(group_info))) group_info$depth_layer <- 0
  group_info$active <- as.logical(group_info$active)
  group_info$active[is.na(group_info$active)] <- FALSE
  group_info$structure_rank <- match(as.character(group_info$structure), c("GPe", "STN", "GPi"))
  group_info$structure_rank[is.na(group_info$structure_rank)] <- 99L
  group_info <- group_info[order(
    group_info$active,
    group_info$side,
    group_info$structure_rank,
    suppressWarnings(as.integer(group_info$depth_layer)),
    group_info$group
  ), , drop = FALSE]
  unique(as.character(group_info$group))
}

stpd_dbs_track_wire_color <- function(structure, active = TRUE) {
  structure <- as.character(structure %||% "STN")[1]
  active <- isTRUE(active)
  alpha <- if (active) 0.74 else 0.42
  base <- switch(
    structure,
    GPe = "#ffffff",
    GPi = "#475569",
    STN = "#475569",
    "#475569"
  )
  grDevices::adjustcolor(base, alpha.f = alpha)
}

stpd_dbs_track_dot_fill <- function(structure, shade = 0.5, alpha = 1) {
  base <- stpd_dbs_track_dot_base_color(structure)
  shade <- suppressWarnings(as.numeric(shade %||% 0.5))[1]
  if (!is.finite(shade)) shade <- 0.5
  shade <- max(0, min(1, shade))
  alpha <- suppressWarnings(as.numeric(alpha %||% 1))[1]
  if (!is.finite(alpha)) alpha <- 1
  alpha <- max(0, min(1, alpha))
  rgb <- grDevices::col2rgb(base)[, 1]
  light <- rgb * 0.22 + 255 * 0.78
  dark <- rgb * 0.38
  shaded <- light * (1 - shade) + dark * shade
  grDevices::rgb(shaded[1], shaded[2], shaded[3], alpha = alpha * 255, maxColorValue = 255)
}

stpd_dbs_track_alpha_color <- function(color, alpha = 1) {
  alpha <- suppressWarnings(as.numeric(alpha %||% 1))
  alpha[!is.finite(alpha)] <- 1
  alpha <- pmax(0, pmin(1, alpha))
  rgb <- grDevices::col2rgb(color)[, 1]
  vapply(
    alpha,
    function(a) grDevices::rgb(rgb[1], rgb[2], rgb[3], alpha = a * 255, maxColorValue = 255),
    character(1)
  )
}

stpd_dbs_track_sphere_highlight_color <- function(structure, layer = c("side", "inner"), alpha = 1) {
  layer <- match.arg(layer)
  structure <- as.character(structure %||% "STN")[1]
  base <- switch(
    structure,
    GPe = if (identical(layer, "side")) "#fdba74" else "#ffffff",
    GPi = if (identical(layer, "side")) "#bae6fd" else "#ffffff",
    STN = if (identical(layer, "side")) "#bbf7d0" else "#ffffff",
    if (identical(layer, "side")) "#ddd6fe" else "#ffffff"
  )
  stpd_dbs_track_alpha_color(base, alpha)
}

stpd_dbs_track_point_in_polygon <- function(x, y, polygon) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  px <- suppressWarnings(as.numeric(polygon$x))
  py <- suppressWarnings(as.numeric(polygon$y))
  ok <- is.finite(px) & is.finite(py)
  px <- px[ok]
  py <- py[ok]
  if (length(px) < 3L || length(x) == 0L) return(rep(FALSE, length(x)))
  if (length(px) > 1L && isTRUE(all.equal(px[1], px[length(px)])) && isTRUE(all.equal(py[1], py[length(py)]))) {
    px <- px[-length(px)]
    py <- py[-length(py)]
  }
  inside <- rep(FALSE, length(x))
  finite_xy <- is.finite(x) & is.finite(y)
  if (!any(finite_xy)) return(inside)
  jj <- length(px)
  for (ii in seq_along(px)) {
    crosses <- ((py[ii] > y[finite_xy]) != (py[jj] > y[finite_xy])) &
      (x[finite_xy] < (px[jj] - px[ii]) * (y[finite_xy] - py[ii]) / (py[jj] - py[ii] + 1e-12) + px[ii])
    inside[finite_xy] <- xor(inside[finite_xy], crosses)
    jj <- ii
  }
  inside
}

stpd_dbs_track_nucleus_dot_cloud <- function(points, structure, side = "R",
                                             active = TRUE, n_x = 30L, n_y = 40L) {
  if (!is.data.frame(points) || nrow(points) < 3L) return(data.frame())
  structure <- as.character(structure %||% "STN")[1]
  side <- toupper(as.character(side %||% "R"))[1]
  active <- isTRUE(active)
  n_x <- max(8L, suppressWarnings(as.integer(n_x %||% 28L))[1])
  n_y <- max(12L, suppressWarnings(as.integer(n_y %||% 50L))[1])
  pts <- points[is.finite(points$x) & is.finite(points$y), c("x", "y"), drop = FALSE]
  if (nrow(pts) < 3L) return(data.frame())
  target_n <- max(1850L, round(n_x * n_y * 1.80))
  candidate_n <- target_n * 14L
  id <- seq_len(candidate_n)
  fract <- function(z) z - floor(z)
  x_rng <- range(pts$x, na.rm = TRUE)
  y_rng <- range(pts$y, na.rm = TRUE)
  x_span <- max(diff(x_rng), 1e-6)
  y_span <- max(diff(y_rng), 1e-6)
  grid <- data.frame(
    x = x_rng[1] + x_span * fract(id * 0.754877666 + 0.072 * sin(id * 0.113) + 0.031 * sin(id * 0.017)),
    y = y_rng[1] + y_span * fract(id * 0.569840291 + 0.061 * sin(id * 0.097) + 0.029 * cos(id * 0.023)),
    seed_id = id,
    stringsAsFactors = FALSE
  )
  grid <- grid[stpd_dbs_track_point_in_polygon(grid$x, grid$y, pts), , drop = FALSE]
  if (nrow(grid) > target_n) {
    keep_score <- fract(grid$seed_id * 0.61803398875 + 0.141 * sin(grid$seed_id * 0.071))
    grid <- grid[order(keep_score), , drop = FALSE]
    grid <- grid[seq_len(target_n), , drop = FALSE]
  }
  if (nrow(grid) == 0L) return(data.frame())
  side_sign <- if (identical(side, "L")) -1 else 1
  texture <- 0.5 + 0.5 * sin(grid$x * 8.7 + grid$y * 5.1 + 0.63 * sin(grid$seed_id * 0.043))
  texture <- 0.65 * texture + 0.35 * (0.5 + 0.5 * cos(grid$x * 13.1 - grid$y * 7.4 + grid$seed_id * 0.019))
  pseudo_depth <- 0.5 + 0.5 * sin(grid$x * 6.1 - grid$y * 8.4 + grid$seed_id * 0.053)
  pseudo_depth <- 0.58 * pseudo_depth + 0.42 * (0.5 + 0.5 * cos(grid$x * 3.7 + grid$y * 6.8 + grid$seed_id * 0.031))
  size_noise <- 0.5 + 0.5 * sin(grid$x * 23.7 + grid$y * 17.1 + grid$seed_id * 0.41)
  shade_noise <- 0.5 + 0.5 * sin(grid$x * 15.3 - grid$y * 19.9 + grid$seed_id * 0.31)
  grid$x <- grid$x + side_sign * (pseudo_depth - 0.5) * 0.26 + (texture - 0.5) * 0.032
  grid$y <- grid$y - (pseudo_depth - 0.5) * 0.100 + (size_noise - 0.5) * 0.018
  keep_display <- stpd_dbs_track_point_in_polygon(grid$x, grid$y, pts)
  grid <- grid[keep_display, , drop = FALSE]
  texture <- texture[keep_display]
  pseudo_depth <- pseudo_depth[keep_display]
  size_noise <- size_noise[keep_display]
  shade_noise <- shade_noise[keep_display]
  if (nrow(grid) == 0L) return(data.frame())
  front_boost <- pseudo_depth^1.55
  grid$dot_size <- if (active) {
    0.30 + 0.56 * size_noise + 0.98 * front_boost + 0.16 * texture
  } else {
    0.23 + 0.38 * size_noise + 0.66 * front_boost + 0.10 * texture
  }
  grid$dot_shade <- if (active) {
    0.10 + 0.20 * shade_noise + 0.12 * texture + 0.60 * front_boost
  } else {
    0.08 + 0.15 * shade_noise + 0.07 * texture + 0.38 * front_boost
  }
  grid$dot_shade <- pmax(0.08, pmin(if (active) 0.92 else 0.66, grid$dot_shade))
  grid$dot_alpha <- if (active) {
    pmax(0.14, pmin(0.82, 0.16 + 0.06 * texture + 0.46 * front_boost + 0.06 * shade_noise))
  } else {
    pmax(0.09, pmin(0.50, 0.11 + 0.05 * texture + 0.29 * front_boost + 0.04 * shade_noise))
  }
  grid$pseudo_depth <- pseudo_depth
  grid$dot_color <- mapply(
    function(s, a) stpd_dbs_track_dot_fill(structure, s, a),
    grid$dot_shade, grid$dot_alpha,
    USE.NAMES = FALSE
  )
  grid$dot_shadow_x <- grid$x + side_sign * (0.010 + 0.010 * front_boost)
  grid$dot_shadow_y <- grid$y - (0.010 + 0.008 * front_boost)
  grid$dot_side_x <- grid$x + side_sign * (0.006 + 0.005 * front_boost)
  grid$dot_side_y <- grid$y + 0.002
  grid$dot_inner_x <- grid$x - side_sign * (0.006 + 0.003 * front_boost)
  grid$dot_inner_y <- grid$y + 0.008 + 0.003 * front_boost
  grid$dot_shadow_size <- grid$dot_size * (1.15 + 0.08 * front_boost)
  grid$dot_body_size <- grid$dot_size
  grid$dot_side_size <- grid$dot_size * (0.58 + 0.07 * front_boost)
  grid$dot_inner_size <- pmax(0.06, grid$dot_size * (0.20 + 0.05 * front_boost))
  grid$dot_shadow_color <- stpd_dbs_track_alpha_color(
    "#0f172a",
    if (active) pmax(0.035, pmin(0.18, 0.045 + 0.12 * front_boost)) else pmax(0.030, pmin(0.13, 0.035 + 0.075 * front_boost))
  )
  grid$dot_side_color <- stpd_dbs_track_sphere_highlight_color(
    structure,
    layer = "side",
    alpha = if (active) pmax(0.12, pmin(0.42, 0.14 + 0.22 * front_boost + 0.05 * texture)) else pmax(0.08, pmin(0.30, 0.09 + 0.15 * front_boost + 0.03 * texture))
  )
  grid$dot_inner_color <- stpd_dbs_track_sphere_highlight_color(
    structure,
    layer = "inner",
    alpha = if (active) pmax(0.20, pmin(0.62, 0.22 + 0.30 * front_boost + 0.06 * texture)) else pmax(0.11, pmin(0.36, 0.12 + 0.17 * front_boost + 0.03 * texture))
  )
  grid$dot_line <- if (active) "rgba(255,255,255,0.62)" else "rgba(255,255,255,0.30)"
  keep_layer_centers <- stpd_dbs_track_point_in_polygon(grid$x, grid$y, pts) &
    stpd_dbs_track_point_in_polygon(grid$dot_shadow_x, grid$dot_shadow_y, pts) &
    stpd_dbs_track_point_in_polygon(grid$dot_side_x, grid$dot_side_y, pts) &
    stpd_dbs_track_point_in_polygon(grid$dot_inner_x, grid$dot_inner_y, pts)
  grid <- grid[keep_layer_centers, , drop = FALSE]
  if (nrow(grid) == 0L) return(data.frame())
  grid$structure <- structure
  grid$side <- side
  grid$active <- active
  grid$group <- paste(side, structure, "dots", sep = "_")
  grid <- grid[order(grid$pseudo_depth, grid$seed_id), , drop = FALSE]
  grid
}

stpd_dbs_track_dot_flow_frames <- function(dots, polygon, n_frames = 96L, amplitude = 0.12) {
  if (!is.data.frame(dots) || nrow(dots) == 0L) return(data.frame())
  if (!is.data.frame(polygon) || nrow(polygon) < 3L) return(data.frame())
  n_frames <- suppressWarnings(as.integer(n_frames %||% 96L))[1]
  if (!is.finite(n_frames) || n_frames < 12L) n_frames <- 96L
  amplitude <- suppressWarnings(as.numeric(amplitude %||% 0.12))[1]
  if (!is.finite(amplitude) || amplitude <= 0) amplitude <- 0.12
  seed <- suppressWarnings(as.numeric(dots$seed_id %||% seq_len(nrow(dots))))
  seed[!is.finite(seed)] <- seq_len(nrow(dots))[!is.finite(seed)]
  depth <- suppressWarnings(as.numeric(dots$pseudo_depth %||% 0.5))
  depth[!is.finite(depth)] <- 0.5
  depth <- pmax(0, pmin(1, depth))
  rand01 <- function(x) {
    out <- sin(x) * 43758.5453123
    out - floor(out)
  }
  inside_shift <- function(dx, dy) {
    stpd_dbs_track_point_in_polygon(dots$x + dx, dots$y + dy, polygon) &
      stpd_dbs_track_point_in_polygon(dots$dot_shadow_x + dx, dots$dot_shadow_y + dy, polygon) &
      stpd_dbs_track_point_in_polygon(dots$dot_side_x + dx, dots$dot_side_y + dy, polygon) &
      stpd_dbs_track_point_in_polygon(dots$dot_inner_x + dx, dots$dot_inner_y + dy, polygon)
  }
  phase_x1 <- 2 * pi * rand01(seed * 12.9898 + dots$x * 37.719)
  phase_y1 <- 2 * pi * rand01(seed * 78.233 + dots$y * 19.417)
  phase_x2 <- 2 * pi * rand01(seed * 39.3467 + dots$x * 11.137 - dots$y * 5.77)
  phase_y2 <- 2 * pi * rand01(seed * 21.1731 - dots$x * 7.719 + dots$y * 13.91)
  freq_x1 <- 1 + floor(5 * rand01(seed * 0.754877666 + 0.13))
  freq_y1 <- 1 + floor(5 * rand01(seed * 0.569840291 + 0.37))
  freq_x2 <- 2 + floor(7 * rand01(seed * 0.618033989 + 0.71))
  freq_y2 <- 2 + floor(7 * rand01(seed * 0.414213562 + 0.29))
  local_amp <- amplitude * (0.48 + 0.88 * rand01(seed * 1.271 + 11.37)) * (0.76 + 0.30 * depth)
  frames <- vector("list", n_frames)
  for (ff in seq_len(n_frames)) {
    theta <- 2 * pi * (ff - 1) / n_frames
    cand_dx <- local_amp * (
      0.58 * cos(freq_x1 * theta + phase_x1) +
        0.31 * sin(freq_x2 * theta + phase_x2) +
        0.11 * cos((freq_x1 + freq_x2 + 1) * theta + phase_y2)
    )
    cand_dy <- local_amp * (
      0.58 * sin(freq_y1 * theta + phase_y1) +
        0.31 * cos(freq_y2 * theta + phase_y2) +
        0.11 * sin((freq_y1 + freq_y2 + 1) * theta + phase_x2)
    )
    inside <- inside_shift(cand_dx, cand_dy)

    if (!all(inside)) {
      for (fac in c(0.78, 0.58, 0.38, 0.22, 0.10)) {
        retry_dx <- fac * cand_dx
        retry_dy <- fac * cand_dy
        retry_inside <- inside_shift(retry_dx, retry_dy)
        use_retry <- !inside & retry_inside
        cand_dx[use_retry] <- retry_dx[use_retry]
        cand_dy[use_retry] <- retry_dy[use_retry]
        inside[use_retry] <- TRUE
        if (all(inside)) break
      }
    }
    if (!all(inside)) {
      bounce_dx <- -0.35 * cand_dx
      bounce_dy <- -0.35 * cand_dy
      bounce_inside <- inside_shift(bounce_dx, bounce_dy)
      use_bounce <- !inside & bounce_inside
      cand_dx[use_bounce] <- bounce_dx[use_bounce]
      cand_dy[use_bounce] <- bounce_dy[use_bounce]
      inside[use_bounce] <- TRUE
    }
    cand_dx[!inside] <- 0
    cand_dy[!inside] <- 0

    out <- dots
    out$x <- dots$x + cand_dx
    out$y <- dots$y + cand_dy
    out$dot_shadow_x <- dots$dot_shadow_x + cand_dx
    out$dot_shadow_y <- dots$dot_shadow_y + cand_dy
    out$dot_side_x <- dots$dot_side_x + cand_dx
    out$dot_side_y <- dots$dot_side_y + cand_dy
    out$dot_inner_x <- dots$dot_inner_x + cand_dx
    out$dot_inner_y <- dots$dot_inner_y + cand_dy
    out$flow_frame <- sprintf("flow_%02d", ff)
    frames[[ff]] <- out
  }
  dplyr::bind_rows(frames)
}

stpd_dbs_track_autoplay_particles <- function(p, enabled = TRUE) {
  if (!isTRUE(enabled) || !requireNamespace("htmlwidgets", quietly = TRUE)) return(p)
  htmlwidgets::onRender(
    p,
    "
function(el, x) {
  function startParticleFlow() {
    if (!window.Plotly) return;
    var gd = document.getElementById(el.id) || el;
    var frameSource = [];
    if (gd && gd._transitionData && gd._transitionData._frames) {
      frameSource = gd._transitionData._frames;
    } else if (x && x.frames) {
      frameSource = x.frames;
    }
    var frameNames = frameSource.map(function(f) { return f && f.name; }).filter(function(name) { return !!name; });
    if (!frameNames.length) return;
    if (el._stpdParticleFlowTimer) window.clearInterval(el._stpdParticleFlowTimer);
    if (el._stpdParticleFlowResumeTimer) window.clearTimeout(el._stpdParticleFlowResumeTimer);
    var frameIndex = 0;
    el._stpdParticleFlowState = el._stpdParticleFlowState || {pointerDown: false, pausedUntil: 0};
    var flowState = el._stpdParticleFlowState;
    function now() {
      return Date.now ? Date.now() : new Date().getTime();
    }
    function pauseFor(ms) {
      flowState.pausedUntil = Math.max(flowState.pausedUntil || 0, now() + ms);
    }
    function currentRange(axisName) {
      if (!gd || !gd._fullLayout || !gd._fullLayout[axisName] || !gd._fullLayout[axisName].range) return null;
      return gd._fullLayout[axisName].range.slice();
    }
    function sameRange(a, b) {
      if (!a || !b || a.length !== 2 || b.length !== 2) return false;
      return Math.abs(a[0] - b[0]) < 1e-9 && Math.abs(a[1] - b[1]) < 1e-9;
    }
    function restoreRanges(xr, yr) {
      if (!xr || !yr || !window.Plotly || flowState.pointerDown) return;
      var nowX = currentRange('xaxis');
      var nowY = currentRange('yaxis');
      if (sameRange(xr, nowX) && sameRange(yr, nowY)) return;
      window.Plotly.relayout(gd, {
        'xaxis.range[0]': xr[0],
        'xaxis.range[1]': xr[1],
        'yaxis.range[0]': yr[0],
        'yaxis.range[1]': yr[1]
      });
    }
    if (!el._stpdParticleFlowInteractionBound) {
      el._stpdParticleFlowInteractionBound = true;
      gd.addEventListener('mousedown', function() {
        flowState.pointerDown = true;
        pauseFor(2500);
      }, {passive: true});
      gd.addEventListener('touchstart', function() {
        flowState.pointerDown = true;
        pauseFor(2500);
      }, {passive: true});
      gd.addEventListener('wheel', function() {
        pauseFor(1400);
      }, {passive: true});
      window.addEventListener('mouseup', function() {
        flowState.pointerDown = false;
        pauseFor(1200);
      }, {passive: true});
      window.addEventListener('touchend', function() {
        flowState.pointerDown = false;
        pauseFor(1200);
      }, {passive: true});
      if (gd.on) {
        gd.on('plotly_relayouting', function() { pauseFor(1600); });
        gd.on('plotly_relayout', function() { pauseFor(900); });
        gd.on('plotly_doubleclick', function() { pauseFor(900); });
      }
    }
    var step = function() {
      if (!document.body.contains(el)) {
        window.clearInterval(el._stpdParticleFlowTimer);
        return;
      }
      if (flowState.pointerDown || now() < (flowState.pausedUntil || 0)) return;
      var xr = currentRange('xaxis');
      var yr = currentRange('yaxis');
      var animation = Plotly.animate(gd, [frameNames[frameIndex]], {
        frame: {duration: 160, redraw: false},
        transition: {duration: 0},
        mode: 'immediate'
      });
      if (animation && animation.then) {
        animation.then(function() { restoreRanges(xr, yr); });
      }
      frameIndex = (frameIndex + 1) % frameNames.length;
    };
    window.setTimeout(step, 250);
    el._stpdParticleFlowTimer = window.setInterval(step, 185);
  }
  window.setTimeout(startParticleFlow, 450);
}
"
  )
}

stpd_dbs_track_nucleus_shell_layers <- function(points, structure, side = "R",
                                                active = TRUE, n_layers = 7L) {
  if (!is.data.frame(points) || nrow(points) < 3L) return(data.frame())
  structure <- as.character(structure %||% "STN")[1]
  side <- toupper(as.character(side %||% "R"))[1]
  active <- isTRUE(active)
  pts <- points[is.finite(points$x) & is.finite(points$y), c("x", "y"), drop = FALSE]
  if (nrow(pts) < 3L) return(data.frame())
  n_layers <- max(3L, suppressWarnings(as.integer(n_layers %||% 7L))[1])
  cx <- mean(range(pts$x))
  cy <- mean(range(pts$y))
  side_sign <- if (identical(side, "L")) -1 else 1
  base <- stpd_dbs_track_dot_base_color(structure)
  layer_vals <- seq(1, 0, length.out = n_layers)
  dplyr::bind_rows(lapply(seq_along(layer_vals), function(ii) {
    depth <- layer_vals[ii]
    scale <- 1 + depth * 0.075
    x <- cx + (pts$x - cx) * scale - side_sign * depth * 0.155
    y <- cy + (pts$y - cy) * (1 + depth * 0.035) + depth * 0.050
    fill_alpha <- 0
    line_alpha <- 0
    data.frame(
      x = x,
      y = y,
      structure = structure,
      side = side,
      active = active,
      depth_layer = ii,
      group = paste(side, structure, "shell", ii, sep = "_"),
      fill_color = grDevices::adjustcolor(base, alpha.f = fill_alpha),
      line_color = grDevices::adjustcolor("#ffffff", alpha.f = line_alpha),
      line_width = 0,
      stringsAsFactors = FALSE
    )
  }))
}

stpd_dbs_track_nucleus_wire_lines <- function(points, structure, side = "R",
                                              active = TRUE,
                                              n_lat = 9L, n_lon = 10L, n_per = 110L) {
  if (!is.data.frame(points) || nrow(points) < 3L) return(data.frame())
  structure <- as.character(structure %||% "STN")[1]
  side <- toupper(as.character(side %||% "R"))[1]
  active <- isTRUE(active)
  pts <- points[is.finite(points$x) & is.finite(points$y), c("x", "y"), drop = FALSE]
  if (nrow(pts) < 3L) return(data.frame())
  cx <- mean(range(pts$x))
  cy <- mean(range(pts$y))
  rx <- max(diff(range(pts$x)) / 2, 1e-6)
  ry <- max(diff(range(pts$y)) / 2, 1e-6)
  side_sign <- if (identical(side, "L")) -1 else 1
  n_per <- max(24L, suppressWarnings(as.integer(n_per %||% 90L))[1])
  out <- list()
  add_line <- function(df, id) {
    keep <- stpd_dbs_track_point_in_polygon(df$x, df$y, pts)
    df <- df[keep, , drop = FALSE]
    if (nrow(df) < 2L) return(NULL)
    df$structure <- structure
    df$side <- side
    df$active <- active
    df$group <- paste(side, structure, id, sep = "_")
    rbind(df, data.frame(x = NA_real_, y = NA_real_, structure = structure, side = side,
                         active = active,
                         group = paste(side, structure, id, sep = "_"), stringsAsFactors = FALSE))
  }
  lat_vals <- seq(-0.78, 0.78, length.out = max(3L, n_lat))
  for (kk in seq_along(lat_vals)) {
    lat <- lat_vals[kk]
    half <- rx * sqrt(pmax(0, 1 - lat^2))
    t <- seq(-1, 1, length.out = n_per)
    df <- data.frame(
      x = cx + half * t,
      y = cy + ry * lat + side_sign * 0.050 * ry * sin(pi * (t + 1) / 2) * (1 - abs(lat)),
      stringsAsFactors = FALSE
    )
    out[[length(out) + 1L]] <- add_line(df, paste0("lat", kk))
  }
  lon_vals <- seq(-0.80, 0.80, length.out = max(3L, n_lon))
  y_norm <- seq(-0.94, 0.94, length.out = n_per)
  for (kk in seq_along(lon_vals)) {
    lon <- lon_vals[kk]
    half <- sqrt(pmax(0, 1 - y_norm^2))
    df <- data.frame(
      x = cx + rx * lon * half + side_sign * 0.060 * rx * y_norm,
      y = cy + ry * y_norm,
      stringsAsFactors = FALSE
    )
    out[[length(out) + 1L]] <- add_line(df, paste0("lon", kk))
  }
  dplyr::bind_rows(out)
}

stpd_dbs_track_plotly <- function(prep, time_unit = c("s", "ms"), show_labels = TRUE,
                                  show_anatomical_context = FALSE,
                                  depth_direction = c("larger_deeper", "larger_shallower")) {
  time_unit <- match.arg(time_unit)
  depth_direction <- match.arg(depth_direction)
  rows <- prep$rows %||% data.frame()
  spikes <- prep$spikes %||% data.frame()
  pattern_segments <- prep$pattern_segments %||% data.frame()
  if (nrow(rows) == 0) {
    return(layout(
      plot_ly(),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        text = "No LT/RT spike trains with parseable depth are available.",
        showarrow = FALSE
      ))
    ))
  }

  rows$side <- toupper(as.character(rows$side))
  rows$trajectory_label <- stpd_dbs_track_trajectory_label(rows$trajectory %||% NA_character_)
  depth_reference <- prep$depth_reference %||% rows
  if (!is.data.frame(depth_reference) || nrow(depth_reference) == 0) depth_reference <- rows
  if (!("side" %in% names(depth_reference))) depth_reference$side <- NA_character_
  if (!("trajectory" %in% names(depth_reference))) depth_reference$trajectory <- NA_character_
  if (!("recording_depth" %in% names(depth_reference))) depth_reference$recording_depth <- NA_real_
  if (!("structure" %in% names(depth_reference))) depth_reference$structure <- NA_character_
  rows$recording_depth <- suppressWarnings(as.numeric(rows$recording_depth))
  depth_reference$side <- toupper(as.character(depth_reference$side))
  depth_reference$trajectory_label <- stpd_dbs_track_trajectory_label(depth_reference$trajectory)
  depth_reference$recording_depth <- suppressWarnings(as.numeric(depth_reference$recording_depth))
  depth_reference$structure <- as.character(depth_reference$structure)
  missing_ref_structure <- is.na(depth_reference$structure) | !nzchar(depth_reference$structure)
  if (any(missing_ref_structure) && "train" %in% names(depth_reference) && "train" %in% names(rows)) {
    matched_structure <- rows$structure[match(depth_reference$train, rows$train)]
    use_match <- missing_ref_structure & !is.na(matched_structure) & nzchar(matched_structure)
    depth_reference$structure[use_match] <- matched_structure[use_match]
    missing_ref_structure <- is.na(depth_reference$structure) | !nzchar(depth_reference$structure)
  }
  depth_reference$structure[missing_ref_structure] <- rows$structure[1] %||% "STN"

  focus_bounds <- stpd_dbs_track_focus_bounds()
  depth_top <- unname(focus_bounds["top"])
  depth_bottom <- unname(focus_bounds["bottom"])
  visible_structures <- unique(as.character(rows$structure))
  visible_structures <- visible_structures[!is.na(visible_structures) & nzchar(visible_structures)]
  plot_sides <- sort(unique(rows$side[rows$side %in% c("L", "R")]))
  if (length(plot_sides) == 0L) plot_sides <- c("L", "R")
  nuclei <- stpd_dbs_track_nucleus_catalog()[c("GPe", "GPi", "STN")]
  use_standard_layout <- isTRUE(show_anatomical_context) ||
    sum(visible_structures %in% names(nuclei)) > 1L
  nuclei_to_draw <- Filter(function(nu) {
    isTRUE(use_standard_layout) || nu$name %in% visible_structures
  }, nuclei)
  target_structure <- visible_structures[visible_structures %in% names(nuclei)]
  target_structure <- if (length(target_structure) > 0L) target_structure[1] else (visible_structures[1] %||% "STN")

  shape_df <- dplyr::bind_rows(lapply(plot_sides, function(ss) {
    side_x <- stpd_dbs_track_focus_side_x(ss)
    dplyr::bind_rows(lapply(nuclei_to_draw, function(nu) {
      active <- nu$name %in% visible_structures
      shape <- stpd_dbs_track_shape_for_layout(
        nu$name,
        side_x = side_x,
        target_structure = target_structure,
        y_top = depth_top,
        y_bottom = depth_bottom,
        n = 220,
        active = active,
        show_anatomical_context = use_standard_layout
      )
      background_alpha <- switch(nu$name, STN = 0.15, GPe = 0.24, GPi = 0.24, 0.18)
      pts <- shape$points
      pts$side <- ss
      pts$structure <- nu$name
      pts$group <- paste(ss, nu$name, sep = "_")
      pts$represented <- active
      pts$active <- active
      pts$fill_alpha <- ifelse(active, 0.58, background_alpha)
      pts
    }))
  }))
  nucleus_label_df <- dplyr::bind_rows(lapply(plot_sides, function(ss) {
    side_x <- stpd_dbs_track_focus_side_x(ss)
    dplyr::bind_rows(lapply(nuclei_to_draw, function(nu) {
      active <- nu$name %in% visible_structures
      shape <- stpd_dbs_track_shape_for_layout(
        nu$name,
        side_x = side_x,
        target_structure = target_structure,
        y_top = depth_top,
        y_bottom = depth_bottom,
        n = if (isTRUE(use_standard_layout)) 80 else if (isTRUE(active)) 40 else 80,
        active = active,
        show_anatomical_context = use_standard_layout
      )
      data.frame(
        x = shape$label_x,
        y = shape$label_y,
        label = nu$name,
        side = ss,
        represented = active,
        active = active,
        stringsAsFactors = FALSE
      )
    }))
  }))
  nucleus_label_df <- stpd_dbs_track_collapse_nucleus_labels(nucleus_label_df)

  rows$depth_y <- NA_real_
  rows$trajectory_offset <- 0
  rows$lane_y <- NA_real_
  for (ss in unique(rows$side)) {
    idx_side <- which(rows$side == ss)
    if (length(idx_side) == 0L) next
    ref_idx <- which(depth_reference$side == ss & is.finite(depth_reference$recording_depth))
    rows$trajectory_offset[idx_side] <- stpd_dbs_track_trajectory_offsets(
      rows$trajectory_label[idx_side],
      levels = depth_reference$trajectory_label[ref_idx],
      span = 0.18
    )
    for (structure_name in unique(rows$structure[idx_side])) {
      idx <- idx_side[rows$structure[idx_side] == structure_name]
      if (length(idx) == 0L) next
      structure_ref_idx <- ref_idx[as.character(depth_reference$structure[ref_idx]) == structure_name]
      if (length(structure_ref_idx) == 0L) structure_ref_idx <- ref_idx
      ref_depth <- depth_reference$recording_depth[structure_ref_idx]
      if (!any(is.finite(ref_depth))) ref_depth <- rows$recording_depth[idx]
      y_bounds <- stpd_dbs_track_shape_y_bounds(
        shape_df,
        side = ss,
        structure = structure_name,
        fallback_top = depth_top,
        fallback_bottom = depth_bottom
      )
      mapped_y <- stpd_dbs_track_rescale_depth(
        rows$recording_depth[idx],
        reference_depth = ref_depth,
        direction = depth_direction,
        y_top = unname(y_bounds["top"]),
        y_bottom = unname(y_bounds["bottom"])
      )
      finite_ref_depth <- ref_depth[is.finite(ref_depth)]
      if (length(finite_ref_depth) < 2L ||
          diff(range(finite_ref_depth, na.rm = TRUE)) <= 1e-9) {
        mapped_y[is.finite(rows$recording_depth[idx])] <- mean(unname(y_bounds))
      }
      rows$depth_y[idx] <- mapped_y
      rows$lane_y[idx] <- stpd_dbs_track_display_lanes(
        rows$side[idx],
        rows$depth_y[idx],
        preferred_gap = 0.24,
        y_min = unname(y_bounds["bottom"]),
        y_max = unname(y_bounds["top"])
      )
    }
  }
  missing_depth_y <- !is.finite(rows$depth_y)
  if (any(missing_depth_y)) {
    rows$depth_y[missing_depth_y] <- 0
  }
  rows$electrode_x <- stpd_dbs_track_focus_side_x(rows$side) + rows$trajectory_offset
  missing_lane_y <- !is.finite(rows$lane_y)
  if (any(missing_lane_y)) rows$lane_y[missing_lane_y] <- rows$depth_y[missing_lane_y]
  rows$lane_offset_y <- rows$lane_y - rows$depth_y
  raster_bounds <- stpd_dbs_track_raster_bounds(rows$side, length_multiplier = 1.5)
  rows$raster_x0 <- raster_bounds$x0
  rows$raster_x1 <- raster_bounds$x1
  rows$raster_label_x <- ifelse(rows$side == "L", rows$raster_x0 - 0.20, rows$raster_x1 + 0.20)

  if (nrow(spikes) > 0) {
    spikes <- dplyr::left_join(
      spikes,
      rows[, c("train", "side", "depth_y", "lane_y", "raster_x0", "raster_x1"), drop = FALSE],
      by = "train"
    )
    spikes$x <- spikes$raster_x0 + pmax(0, pmin(1, spikes$spike_time_rel)) * (spikes$raster_x1 - spikes$raster_x0)
  }
  if (nrow(pattern_segments) > 0) {
    pattern_segments <- dplyr::left_join(
      pattern_segments,
      rows[, c("train", "side", "depth_y", "lane_y", "raster_x0", "raster_x1", "trajectory_label", "recording_depth"), drop = FALSE],
      by = "train"
    )
    pattern_segments$x0 <- pattern_segments$raster_x0 + pmax(0, pmin(1, pattern_segments$start_rel)) * (pattern_segments$raster_x1 - pattern_segments$raster_x0)
    pattern_segments$x1 <- pattern_segments$raster_x0 + pmax(0, pmin(1, pattern_segments$end_rel)) * (pattern_segments$raster_x1 - pattern_segments$raster_x0)
    pattern_segments$y <- pattern_segments$lane_y
    pattern_segments <- pattern_segments[
      is.finite(pattern_segments$x0) & is.finite(pattern_segments$x1) &
        is.finite(pattern_segments$y) & pattern_segments$x1 > pattern_segments$x0,
      , drop = FALSE
    ]
  }

  visible_structures <- unique(as.character(rows$structure))
  visible_structures <- visible_structures[!is.na(visible_structures) & nzchar(visible_structures)]
  plot_sides <- sort(unique(rows$side[rows$side %in% c("L", "R")]))
  if (length(plot_sides) == 0L) plot_sides <- c("L", "R")
  hi <- function(s) s %in% visible_structures
  nucleus_style <- function(s, fill) {
    if (hi(s)) list(fill = fill, line = "#111827", opacity = 0.82) else list(fill = "#e5e7eb", line = "#cbd5e1", opacity = 0.26)
  }

  p <- plot_ly()
  p <- add_segments(
    p, x = 0, xend = 0, y = depth_bottom - 0.18, yend = depth_top + 0.18,
    line = list(color = "#d1d5db", width = 1.1, dash = "dot"),
    showlegend = FALSE, hoverinfo = "skip"
  )

  nuclei <- stpd_dbs_track_nucleus_catalog()[c("GPe", "GPi", "STN")]
  nuclei_to_draw <- Filter(function(nu) {
    isTRUE(show_anatomical_context) || hi(nu$name)
  }, nuclei)
  target_structure <- visible_structures[visible_structures %in% names(nuclei)]
  target_structure <- if (length(target_structure) > 0L) target_structure[1] else (visible_structures[1] %||% "STN")
  if (length(nuclei_to_draw) > 1L) {
    nuclei_to_draw <- nuclei_to_draw[order(vapply(nuclei_to_draw, function(nu) hi(nu$name), logical(1)))]
  }
  for (side_x in stpd_dbs_track_focus_side_x(plot_sides)) {
    for (nu in nuclei_to_draw) {
      st <- nucleus_style(nu$name, nu$color)
      shape <- if (isTRUE(show_anatomical_context)) {
        stpd_dbs_track_context_model_shape(
          nu$name,
          target_structure = target_structure,
          side_x = side_x,
          y_top = depth_top,
          y_bottom = depth_bottom,
          n = 180
        )
      } else {
        stpd_dbs_track_focus_nucleus_shape(
          nu$name,
          side_x = side_x,
          y_top = depth_top,
          y_bottom = depth_bottom,
          n = 180
        )
      }
      p <- stpd_dbs_track_add_polygon(
        p,
        shape$points,
        fill = st$fill, line = st$line, opacity = st$opacity, name = nu$name
      )
    }
  }

  track_rows <- unique(rows[, c("side", "trajectory_label", "electrode_x"), drop = FALSE])
  track_rows <- track_rows[order(track_rows$side, track_rows$electrode_x), , drop = FALSE]
  side_has <- unique(track_rows$side)
  for (ii in seq_len(nrow(track_rows))) {
    ss <- track_rows$side[ii]
    ex <- track_rows$electrode_x[ii]
    side_track_n <- sum(track_rows$side == ss)
    track_text <- paste0(
      ifelse(ss == "L", "LT", "RT"),
      if (side_track_n > 1L) paste0(" ", track_rows$trajectory_label[ii]) else "",
      " DBS trajectory schematic"
    )
    p <- add_segments(
      p, x = ex, xend = ex, y = depth_top, yend = depth_bottom,
      line = list(color = "#111827", width = if (side_track_n > 1L) 3.8 else 5.5),
      showlegend = FALSE, hoverinfo = "text",
      text = track_text,
      inherit = FALSE
    )
    p <- add_segments(
      p, x = ex - 0.055, xend = ex + 0.055, y = depth_top, yend = depth_top,
      line = list(color = "#111827", width = if (side_track_n > 1L) 3.8 else 5.5),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    if (side_track_n > 1L) {
      p <- add_text(
        p, x = ex, y = depth_top + 0.18,
        text = track_rows$trajectory_label[ii],
        textfont = list(size = 9.5, color = "#334155"),
        showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
      )
    }
  }

  for (ss in side_has) {
    ref_depth <- depth_reference$recording_depth[depth_reference$side == ss & is.finite(depth_reference$recording_depth)]
    tick_depth <- stpd_dbs_track_depth_ticks(ref_depth, max_ticks = 8L)
    if (length(tick_depth) == 0L) next
    tick_y <- stpd_dbs_track_rescale_depth(
      tick_depth,
      reference_depth = ref_depth,
      direction = depth_direction,
      y_top = depth_top,
      y_bottom = depth_bottom
    )
    side_tracks <- track_rows[track_rows$side == ss, , drop = FALSE]
    for (jj in seq_len(nrow(side_tracks))) {
      tick_df <- data.frame(
        x0 = side_tracks$electrode_x[jj] - 0.055,
        x1 = side_tracks$electrode_x[jj] + 0.055,
        y = tick_y
      )
      p <- add_segments(
        p, data = tick_df,
        x = ~x0, xend = ~x1, y = ~y, yend = ~y,
        line = list(color = "#f8fafc", width = 1.5),
        showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
      )
    }
    label_x <- if (ss == "L") min(side_tracks$electrode_x, na.rm = TRUE) - 0.16 else max(side_tracks$electrode_x, na.rm = TRUE) + 0.16
    label_df <- data.frame(
      x = label_x,
      y = tick_y,
      label = paste0("D", formatC(tick_depth, format = "fg", digits = 4)),
      stringsAsFactors = FALSE
    )
    p <- add_text(
      p, data = label_df,
      x = ~x, y = ~y, text = ~label,
      textfont = list(size = 8.3, color = "#475569"),
      textposition = if (ss == "L") "middle right" else "middle left",
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  for (ii in seq_len(nrow(rows))) {
    rr <- rows[ii, , drop = FALSE]
    depth_y <- rr$depth_y[1]
    lane_y <- rr$lane_y[1]
    if (!is.finite(depth_y)) next
    if (!is.finite(lane_y)) lane_y <- depth_y
    p <- add_segments(
      p,
      x = rr$electrode_x[1], xend = ifelse(rr$side[1] == "L", rr$raster_x1[1], rr$raster_x0[1]),
      y = depth_y, yend = lane_y,
      line = list(color = "rgba(100,116,139,0.34)", width = 1),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_markers(
      p, x = rr$electrode_x[1], y = depth_y,
      marker = list(size = 7, color = "#ffffff", line = list(color = "#111827", width = 1.4)),
      showlegend = FALSE, hoverinfo = "text",
      text = paste0(
        "Train: ", stpd_html_escape(rr$train[1]),
        "<br>Side: ", ifelse(rr$side[1] == "L", "LT", "RT"),
        "<br>Trajectory: ", stpd_html_escape(rr$trajectory_label[1]),
        "<br>Depth: D", signif(rr$recording_depth[1], 5),
        if (isTRUE(abs(rr$lane_offset_y[1]) > 1e-8)) "<br>Display row adjusted to avoid overlap" else "",
        "<br>Spikes in window: ", rr$n_spikes_window[1]
      ),
      inherit = FALSE
    )
    p <- add_segments(
      p, x = rr$raster_x0[1], xend = rr$raster_x1[1],
      y = lane_y, yend = lane_y,
      line = list(color = "rgba(15,23,42,0.32)", width = 1.1),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    if (isTRUE(show_labels)) {
      lab <- paste0(
        ifelse(rr$side[1] == "L", "LT", "RT"),
        " ", rr$trajectory_label[1],
        " D", signif(rr$recording_depth[1], 5),
        " | ", stpd_html_escape(rr$train[1]),
        " | ", rr$n_spikes_window[1], " spikes / ",
        signif(rr$window_sec[1], 4), " s"
      )
      p <- add_text(
        p, x = rr$raster_label_x[1], y = lane_y + 0.055,
        text = lab,
        textfont = list(size = 9.5, color = "#334155"),
        textposition = ifelse(rr$side[1] == "L", "middle right", "middle left"),
        showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
      )
    }
  }

  if (nrow(pattern_segments) > 0) {
    legend_seen <- character(0)
    for (pat in stpd_dbs_track_patterns()) {
      for (src in c("manual", "auto", "final")) {
        sub_pat <- pattern_segments[
          as.character(pattern_segments$pattern) == pat &
            as.character(pattern_segments$source_kind) == src,
          , drop = FALSE
        ]
        if (nrow(sub_pat) == 0) next
        style <- pattern_strip_style(pat, source = if (identical(src, "manual")) "manual" else "auto")
        legend_name <- paste0(if (identical(src, "manual")) "MANUAL " else "AUTO ", pat)
        show_leg <- !(legend_name %in% legend_seen)
        legend_seen <- unique(c(legend_seen, legend_name))
        p <- add_segments(
          p, data = sub_pat,
          x = ~x0, xend = ~x1,
          y = ~y, yend = ~y,
          line = list(width = pattern_strip_line_width(), color = style$color, dash = style$dash),
          hoverinfo = "text",
          text = ~paste0(
            "Pattern: ", pattern,
            "<br>Source: ", source_kind,
            "<br>Train: ", stpd_html_escape(train),
            "<br>Side: ", ifelse(side == "L", "LT", "RT"),
            "<br>Trajectory: ", stpd_html_escape(trajectory_label),
            "<br>Depth: D", signif(recording_depth, 5),
            "<br>Interval: ", signif(start_sec, 6), "-", signif(end_sec, 6), " s"
          ),
          name = legend_name,
          legendgroup = legend_name,
          showlegend = show_leg,
          inherit = FALSE
        )
      }
    }
  }

  if (nrow(spikes) > 0) {
    spike_h <- 0.032
    spikes$y0 <- spikes$lane_y - spike_h
    spikes$y1 <- spikes$lane_y + spike_h
    p <- add_segments(
      p, data = spikes,
      x = ~x, xend = ~x,
      y = ~y0, yend = ~y1,
      line = list(color = "#020617", width = 0.85),
      showlegend = FALSE, hoverinfo = "text",
      text = ~paste0(
        "Train: ", stpd_html_escape(train),
        "<br>Spike time: ", signif(spike_time_sec, 6), " s",
        "<br>Window position: ", signif(spike_time_rel, 4)
      ),
      inherit = FALSE
    )
  }

  scale_label <- if (time_unit == "ms") {
    paste0(signif(unique(rows$window_sec)[1] * 1000, 5), " ms window")
  } else {
    paste0(signif(unique(rows$window_sec)[1], 5), " s window")
  }
  scale_rows <- unique(rows[, c("side", "raster_x0", "raster_x1"), drop = FALSE])
  for (ii in seq_len(nrow(scale_rows))) {
    ss <- scale_rows$side[ii]
    x0 <- scale_rows$raster_x0[ii]; x1 <- scale_rows$raster_x1[ii]
    y0 <- -1.68
    p <- add_segments(
      p, x = x0, xend = x1, y = y0, yend = y0,
      line = list(color = "#0f172a", width = 2.2),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_text(
      p, x = (x0 + x1) / 2, y = y0 - 0.09,
      text = paste0(ifelse(ss == "L", "LT rows: ", "RT rows: "), scale_label),
      textfont = list(size = 10.5, color = "#0f172a"),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  side_label_df <- data.frame(
    x = stpd_dbs_track_focus_side_x(plot_sides),
    y = rep(depth_top + 0.36, length(plot_sides)),
    label = ifelse(plot_sides == "L", "LT", "RT"),
    stringsAsFactors = FALSE
  )
  p <- add_text(
    p, data = side_label_df,
    x = ~x, y = ~y,
    text = ~label, textfont = list(size = 13, color = "#0f172a"),
    showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
  )
  if (length(nuclei_to_draw) > 0) {
    label_rows <- dplyr::bind_rows(lapply(plot_sides, function(side_id) {
      side_x <- stpd_dbs_track_focus_side_x(side_id)
      dplyr::bind_rows(lapply(nuclei_to_draw, function(nu) {
        shape <- stpd_dbs_track_focus_nucleus_shape(
          nu$name,
          side_x = side_x,
          y_top = depth_top,
          y_bottom = depth_bottom,
          n = 40
        )
        data.frame(
          x = shape$label_x,
          y = shape$label_y,
          label = nu$name,
          active = hi(nu$name),
          stringsAsFactors = FALSE
        )
      }))
    }))
    p <- add_text(
      p, data = label_rows,
      x = ~x, y = ~y, text = ~label,
      textfont = list(size = 9.5, color = "#0f172a"),
      opacity = ~ifelse(active, 1, 0.35),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  title <- paste0(
    "Target nucleus depth view | ",
    stpd_html_escape(paste(unique(rows$structure), collapse = "/")),
    " | ", nrow(rows), " trains"
  )
  depth_note <- if (identical(depth_direction, "larger_shallower")) {
    "D range is mapped to the visible target-nucleus bounds; larger D is plotted shallower."
  } else {
    "D range is mapped to the visible target-nucleus bounds; larger D is plotted deeper."
  }
  x_range <- stpd_dbs_track_axis_range(
    rows$raster_x0, rows$raster_x1, rows$raster_label_x, rows$electrode_x,
    spikes$x, pattern_segments$x0, pattern_segments$x1,
    stpd_dbs_track_focus_side_x(plot_sides), 0,
    pad = 0.45,
    fallback = c(-4.75, 4.75)
  )
  y_range <- stpd_dbs_track_axis_range(
    depth_bottom - 0.55, depth_top + 0.55, rows$depth_y, rows$lane_y,
    spikes$y0, spikes$y1, pattern_segments$y,
    pad = 0.22,
    fallback = c(-1.88, 1.82)
  )
  layout(
    p,
    title = list(text = title, x = 0.02, xanchor = "left", font = list(size = 15, color = "#0f172a")),
    hoverlabel = stpd_hoverlabel_style(),
    xaxis = list(visible = FALSE, range = x_range, fixedrange = FALSE),
    yaxis = list(visible = FALSE, range = y_range, fixedrange = FALSE),
    dragmode = "pan",
    uirevision = "dbs_track_depth_view",
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    margin = list(l = 18, r = 18, t = 54, b = 34),
    showlegend = nrow(pattern_segments) > 0,
    legend = list(orientation = "h", x = 0, y = 1.08, font = list(size = 9)),
    annotations = list(list(
      x = 0.5, y = 0.02, xref = "paper", yref = "paper",
      text = paste("Target nucleus schematic; spike-train rows are arranged by parsed side and D depth.", depth_note),
      showarrow = FALSE, font = list(size = 10, color = "#64748b")
    ))
  ) %>% config(
    displaylogo = FALSE,
    scrollZoom = TRUE,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )
}

stpd_dbs_track_plotly_dot_model <- function(prep, time_unit = c("s", "ms"), show_labels = TRUE,
                                            show_anatomical_context = FALSE,
                                            depth_direction = c("larger_deeper", "larger_shallower"),
                                            animate_particles = FALSE) {
  time_unit <- match.arg(time_unit)
  depth_direction <- match.arg(depth_direction)
  layout_data <- stpd_dbs_track_static_layout_data(
    prep,
    show_anatomical_context = show_anatomical_context,
    depth_direction = depth_direction
  )
  rows <- layout_data$rows %||% data.frame()
  if (nrow(rows) == 0) {
    return(layout(
      plot_ly(),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      plot_bgcolor = "#ffffff",
      paper_bgcolor = "#ffffff",
      annotations = list(list(
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        text = "No LT/RT spike trains with parseable depth are available.",
        showarrow = FALSE
      ))
    ))
  }

  shape_df <- layout_data$shape_df %||% data.frame()
  nucleus_label_df <- layout_data$nucleus_label_df %||% data.frame()
  track_rows <- layout_data$track_rows %||% data.frame()
  tick_df <- layout_data$tick_df %||% data.frame()
  spikes <- layout_data$spikes %||% data.frame()
  pattern_segments <- layout_data$pattern_segments %||% data.frame()
  depth_top <- layout_data$depth_top %||% 1.25
  depth_bottom <- layout_data$depth_bottom %||% -1.25
  plot_sides <- layout_data$plot_sides %||% c("L", "R")
  visible_structures <- layout_data$visible_structures %||% unique(as.character(rows$structure))
  shell_df <- data.frame()
  dot_df <- data.frame()
  shape_groups <- list()
  if (nrow(shape_df) > 0) {
    shape_groups <- split(shape_df, shape_df$group)
    shell_df <- dplyr::bind_rows(lapply(shape_groups, function(sh) {
      stpd_dbs_track_nucleus_shell_layers(
        sh,
        structure = unique(as.character(sh$structure))[1],
        side = unique(as.character(sh$side))[1],
        active = isTRUE(unique(sh$active)[1])
      )
    }))
    dot_df <- dplyr::bind_rows(lapply(shape_groups, function(sh) {
      stpd_dbs_track_nucleus_dot_cloud(
        sh,
        structure = unique(as.character(sh$structure))[1],
        side = unique(as.character(sh$side))[1],
        active = isTRUE(unique(sh$active)[1])
      )
    }))
  }

  if (nrow(pattern_segments) > 0) {
    pattern_segments$duration_rel <- pmax(0, pattern_segments$end_rel - pattern_segments$start_rel)
  }
  rows$site_color <- vapply(rows$structure, stpd_dbs_track_structure_label_color, character(1))

  p <- plot_ly()
  p <- add_segments(
    p, x = 0, xend = 0, y = depth_bottom - 0.24, yend = depth_top + 0.24,
    line = list(color = "rgba(148,163,184,0.34)", width = 1.1, dash = "dot"),
    showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
  )

  if (nrow(shell_df) > 0) {
    for (gg in stpd_dbs_track_draw_groups(shell_df)) {
      sh <- shell_df[shell_df$group == gg, , drop = FALSE]
      if (nrow(sh) == 0) next
      structure <- unique(as.character(sh$structure))[1]
      side <- unique(as.character(sh$side))[1]
      is_active <- isTRUE(unique(sh$active)[1])
      p <- add_trace(
        p, data = sh,
        x = ~x, y = ~y,
        type = "scatter", mode = "lines", fill = "toself",
        fillcolor = unique(sh$fill_color)[1],
        line = list(color = unique(sh$line_color)[1], width = unique(sh$line_width)[1]),
        name = paste0(ifelse(side == "L", "LT ", "RT "), structure, " shell"),
        legendgroup = paste0(structure, "_nucleus"),
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste0(
          ifelse(side == "L", "LT", "RT"), " ", structure,
          "<br>2.5D translucent dot-node shell",
          if (!is_active) "<br>Context only" else ""
        ),
        inherit = FALSE
      )
    }
  }

  if (nrow(dot_df) > 0) {
    for (gg in stpd_dbs_track_draw_groups(dot_df)) {
      dd <- dot_df[dot_df$group == gg, , drop = FALSE]
      if (nrow(dd) == 0) next
      structure <- unique(as.character(dd$structure))[1]
      side <- unique(as.character(dd$side))[1]
      is_active <- isTRUE(unique(dd$active)[1])
      sphere_scale <- 4.2
      shape_key <- paste(side, structure, sep = "_")
      flow_dd <- data.frame()
      use_flow <- isTRUE(animate_particles) &&
        is_active &&
        structure %in% visible_structures &&
        !is.null(shape_groups[[shape_key]])
      if (isTRUE(use_flow)) {
        flow_dd <- stpd_dbs_track_dot_flow_frames(
          dd,
          polygon = shape_groups[[shape_key]],
          n_frames = 96L,
          amplitude = 0.12
        )
        if (nrow(flow_dd) == 0L) use_flow <- FALSE
      }
      dd_plot <- if (isTRUE(use_flow)) flow_dd else dd
      frame_formula <- if (isTRUE(use_flow)) ~flow_frame else NULL
      p <- add_markers(
        p, data = dd_plot,
        x = ~dot_shadow_x, y = ~dot_shadow_y,
        frame = frame_formula,
        marker = list(
          size = dd_plot$dot_shadow_size * sphere_scale,
          color = dd_plot$dot_shadow_color,
          symbol = "circle",
          line = list(color = "rgba(255,255,255,0)", width = 0)
        ),
        name = paste0(ifelse(side == "L", "LT ", "RT "), structure, " dot shadow"),
        legendgroup = paste0(structure, "_nucleus"),
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
      p <- add_markers(
        p, data = dd_plot,
        x = ~x, y = ~y,
        frame = frame_formula,
        marker = list(
          size = dd_plot$dot_body_size * sphere_scale,
          color = dd_plot$dot_color,
          symbol = "circle",
          line = list(color = unique(dd_plot$dot_line)[1], width = if (is_active) 0.08 else 0.03)
        ),
        name = paste0(ifelse(side == "L", "LT ", "RT "), structure, " dot cloud"),
        legendgroup = paste0(structure, "_nucleus"),
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste0(
          ifelse(side == "L", "LT", "RT"), " ", structure,
          "<br>surface-node dot cloud for the 2.5D shell",
          if (!is_active) "<br>Context only" else ""
        ),
        inherit = FALSE
      )
      p <- add_markers(
        p, data = dd_plot,
        x = ~dot_side_x, y = ~dot_side_y,
        frame = frame_formula,
        marker = list(
          size = dd_plot$dot_side_size * sphere_scale,
          color = dd_plot$dot_side_color,
          symbol = "circle",
          line = list(color = "rgba(255,255,255,0)", width = 0)
        ),
        name = paste0(ifelse(side == "L", "LT ", "RT "), structure, " side highlight"),
        legendgroup = paste0(structure, "_nucleus"),
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
      p <- add_markers(
        p, data = dd_plot,
        x = ~dot_inner_x, y = ~dot_inner_y,
        frame = frame_formula,
        marker = list(
          size = dd_plot$dot_inner_size * sphere_scale,
          color = dd_plot$dot_inner_color,
          symbol = "circle",
          line = list(color = "rgba(255,255,255,0)", width = 0)
        ),
        name = paste0(ifelse(side == "L", "LT ", "RT "), structure, " inner highlight"),
        legendgroup = paste0(structure, "_nucleus"),
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
    }
  }

  if (nrow(nucleus_label_df) > 0) {
    nucleus_label_df$text_color <- vapply(nucleus_label_df$label, stpd_dbs_track_structure_label_color, character(1))
    nucleus_label_df <- nucleus_label_df[order(nucleus_label_df$active), , drop = FALSE]
    for (ii in seq_len(nrow(nucleus_label_df))) {
      rr <- nucleus_label_df[ii, , drop = FALSE]
      p <- add_text(
        p, x = rr$x, y = rr$y, text = paste0("<b>", rr$label, "</b>"),
        textfont = list(size = 14.5, color = rr$text_color),
        opacity = if (isTRUE(rr$active)) 1 else 0.62,
        showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
      )
    }
  }

  if (nrow(track_rows) > 0) {
    p <- add_segments(
      p, data = track_rows,
      x = ~shadow_x, xend = ~shadow_x, y = ~depth_top, yend = ~depth_bottom,
      line = list(color = "rgba(15,23,42,0.16)", width = 15),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~electrode_x, xend = ~electrode_x, y = ~depth_top, yend = ~depth_bottom,
      line = list(color = "rgba(203,213,225,0.94)", width = 13),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~electrode_x, xend = ~electrode_x, y = ~depth_top, yend = ~depth_bottom,
      line = list(color = "rgba(248,250,252,0.96)", width = 8.8),
      showlegend = FALSE, hoverinfo = "text",
      text = ~paste0(ifelse(side == "L", "LT", "RT"), " ", trajectory_label, " DBS trajectory"),
      inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~left_rim_x, xend = ~left_rim_x, y = ~depth_top, yend = ~depth_bottom,
      line = list(color = "rgba(100,116,139,0.52)", width = 1.8),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~right_rim_x, xend = ~right_rim_x, y = ~depth_top, yend = ~depth_bottom,
      line = list(color = "rgba(56,189,248,0.70)", width = 1.5),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~highlight_x, xend = ~highlight_x, y = ~depth_top, yend = ~depth_bottom,
      line = list(color = "rgba(255,255,255,0.90)", width = 2.2),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~cap_x0, xend = ~cap_x1, y = ~depth_top, yend = ~depth_top,
      line = list(color = "rgba(203,213,225,0.92)", width = 5.8),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = track_rows,
      x = ~cap_x0, xend = ~cap_x1, y = ~depth_bottom, yend = ~depth_bottom,
      line = list(color = "rgba(203,213,225,0.82)", width = 4.8),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_text(
      p, data = track_rows,
      x = ~electrode_x, y = ~I(depth_top + 0.18), text = ~trajectory_label,
      textfont = list(size = 10.5, color = "#334155"),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  if (nrow(tick_df) > 0) {
    p <- add_segments(
      p, data = tick_df,
      x = ~x0, xend = ~x1, y = ~y, yend = ~y,
      line = list(color = "rgba(226,232,240,0.94)", width = 4.2),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_segments(
      p, data = tick_df,
      x = ~x0, xend = ~x1, y = ~y, yend = ~y,
      line = list(color = "rgba(56,189,248,0.70)", width = 1.25),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_text(
      p, data = tick_df,
      x = ~label_x, y = ~y, text = ~label,
      textfont = list(size = 8.7, color = "#334155"),
      textposition = ~ifelse(hjust > 0.5, "middle right", "middle left"),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  connector_df <- rows[
    is.finite(rows$electrode_x) & is.finite(rows$depth_y) & is.finite(rows$lane_y),
    , drop = FALSE
  ]
  if (nrow(connector_df) > 0) {
    connector_df$xend <- ifelse(connector_df$side == "L", connector_df$raster_x1, connector_df$raster_x0)
    p <- add_segments(
      p, data = connector_df,
      x = ~electrode_x, xend = ~xend, y = ~depth_y, yend = ~lane_y,
      line = list(color = "rgba(100,116,139,0.28)", width = 1),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  if (nrow(rows) > 0) {
    rows$site_text <- paste0(
      "Train: ", stpd_html_escape(rows$train),
      "<br>Side: ", ifelse(rows$side == "L", "LT", "RT"),
      "<br>Trajectory: ", stpd_html_escape(rows$trajectory_label),
      "<br>Depth: D", signif(rows$recording_depth, 5),
      ifelse(abs(rows$lane_offset_y) > 1e-8, "<br>Display row adjusted to avoid overlap", ""),
      "<br>Spikes in window: ", rows$n_spikes_window
    )
    p <- add_markers(
      p, data = rows,
      x = ~electrode_x, y = ~depth_y,
      marker = list(size = 9, color = rows$site_color,
                    line = list(color = "#ffffff", width = 1.8), symbol = "circle"),
      showlegend = FALSE, hoverinfo = "text", text = ~site_text, inherit = FALSE
    )
    p <- add_segments(
      p, data = rows,
      x = ~raster_x0, xend = ~raster_x1, y = ~lane_y, yend = ~lane_y,
      line = list(color = "rgba(51,65,85,0.46)", width = 1),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  if (nrow(pattern_segments) > 0) {
    legend_seen <- character(0)
    for (legend_name in unique(as.character(pattern_segments$legend_name))) {
      sub_pat <- pattern_segments[as.character(pattern_segments$legend_name) == legend_name, , drop = FALSE]
      if (nrow(sub_pat) == 0) next
      first_src <- as.character(sub_pat$source_kind[1])
      first_pat <- as.character(sub_pat$pattern[1])
      style <- pattern_strip_style(first_pat, source = if (identical(first_src, "manual")) "manual" else "auto")
      show_leg <- !(legend_name %in% legend_seen)
      legend_seen <- unique(c(legend_seen, legend_name))
      p <- add_segments(
        p, data = sub_pat,
        x = ~x0, xend = ~x1,
        y = ~y, yend = ~y,
        line = list(width = pattern_strip_line_width(), color = sub_pat$pattern_color[1], dash = style$dash),
        hoverinfo = "text",
        text = ~paste0(
          "Pattern: ", pattern,
          "<br>Source: ", source_kind,
          "<br>Train: ", stpd_html_escape(train),
          "<br>Side: ", ifelse(side == "L", "LT", "RT"),
          "<br>Trajectory: ", stpd_html_escape(trajectory_label),
          "<br>Depth: D", signif(recording_depth, 5),
          "<br>Interval: ", signif(start_sec, 6), "-", signif(end_sec, 6), " s"
        ),
        name = legend_name,
        legendgroup = legend_name,
        showlegend = show_leg,
        inherit = FALSE
      )
    }
  }

  if (nrow(spikes) > 0) {
    spike_h <- 0.034
    p <- add_segments(
      p, data = spikes,
      x = ~x, xend = ~x,
      y = ~I(lane_y - spike_h), yend = ~I(lane_y + spike_h),
      line = list(color = "#020617", width = 0.75),
      showlegend = FALSE, hoverinfo = "text",
      text = ~paste0(
        "Train: ", stpd_html_escape(train),
        "<br>Spike time: ", signif(spike_time_sec, 6), " s",
        "<br>Window position: ", signif(spike_time_rel, 4)
      ),
      inherit = FALSE
    )
  }

  if (isTRUE(show_labels)) {
    label_rows <- rows
    label_rows$short_label <- paste0(
      ifelse(label_rows$side == "L", "LT", "RT"),
      " ", label_rows$trajectory_label,
      " D", signif(label_rows$recording_depth, 5),
      " | ", label_rows$n_spikes_window, " spikes / ",
      signif(label_rows$window_sec, 4), " s"
    )
    p <- add_text(
      p, data = label_rows,
      x = ~raster_label_x, y = ~I(lane_y + 0.062),
      text = ~short_label,
      textfont = list(size = 9.5, color = "#253247"),
      textposition = "top left",
      showlegend = FALSE, hoverinfo = "text",
      hovertext = ~paste0("Train: ", stpd_html_escape(train)),
      inherit = FALSE
    )
  }

  scale_label <- if (identical(time_unit, "ms")) {
    paste0(signif(unique(rows$window_sec)[1] * 1000, 5), " ms window")
  } else {
    paste0(signif(unique(rows$window_sec)[1], 5), " s window")
  }
  scale_rows <- unique(rows[, c("side", "raster_x0", "raster_x1"), drop = FALSE])
  scale_rows$y <- depth_bottom - 0.43
  scale_rows$label <- paste0(ifelse(scale_rows$side == "L", "LT rows: ", "RT rows: "), scale_label)
  if (nrow(scale_rows) > 0) {
    p <- add_segments(
      p, data = scale_rows,
      x = ~raster_x0, xend = ~raster_x1, y = ~y, yend = ~y,
      line = list(color = "#0f172a", width = 2),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
    p <- add_text(
      p, data = scale_rows,
      x = ~I((raster_x0 + raster_x1) / 2), y = ~I(y - 0.10), text = ~label,
      textfont = list(size = 10.5, color = "#0f172a"),
      showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
    )
  }

  side_label_df <- data.frame(
    x = stpd_dbs_track_focus_side_x(plot_sides),
    y = rep(depth_top + 0.36, length(plot_sides)),
    label = ifelse(plot_sides == "L", "LT", "RT"),
    stringsAsFactors = FALSE
  )
  p <- add_text(
    p, data = side_label_df,
    x = ~x, y = ~y, text = ~label,
    textfont = list(size = 14, color = "#0f172a"),
    showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
  )

  title <- paste0(
    "2.5D target-nucleus dot-node shell | ",
    stpd_html_escape(paste(unique(rows$structure), collapse = "/")),
    " | ", nrow(rows), " trains"
  )
  depth_note <- if (identical(depth_direction, "larger_shallower")) {
    "larger D is plotted shallower"
  } else {
    "larger D is plotted deeper"
  }
  x_range <- stpd_dbs_track_axis_range(
    rows$raster_x0, rows$raster_x1, rows$raster_label_x, rows$electrode_x,
    shape_df$x, shell_df$x,
    dot_df$x, dot_df$dot_shadow_x, dot_df$dot_side_x, dot_df$dot_inner_x,
    tick_df$x0, tick_df$x1, tick_df$label_x,
    track_rows$electrode_x, track_rows$shadow_x, track_rows$left_rim_x,
    track_rows$right_rim_x, track_rows$highlight_x, track_rows$cap_x0, track_rows$cap_x1,
    spikes$x, pattern_segments$x0, pattern_segments$x1,
    stpd_dbs_track_focus_side_x(plot_sides), 0,
    pad = 0.55,
    fallback = c(-4.75, 4.75)
  )
  y_range <- stpd_dbs_track_axis_range(
    depth_bottom - 0.60, depth_top + 0.58, rows$depth_y, rows$lane_y,
    shape_df$y, shell_df$y,
    dot_df$y, dot_df$dot_shadow_y, dot_df$dot_side_y, dot_df$dot_inner_y,
    tick_df$y, track_rows$depth_top, track_rows$depth_bottom,
    spikes$y0, spikes$y1, pattern_segments$y,
    pad = 0.26,
    fallback = c(-1.9, 1.9)
  )
  p <- layout(
    p,
    title = list(text = title, x = 0.02, xanchor = "left", font = list(size = 15, color = "#0f172a")),
    hoverlabel = stpd_hoverlabel_style(),
    xaxis = list(visible = FALSE, range = x_range, fixedrange = FALSE),
    yaxis = list(visible = FALSE, range = y_range, fixedrange = FALSE),
    dragmode = "pan",
    uirevision = "dbs_track_dot_model",
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    margin = list(l = 18, r = 18, t = 58, b = 36),
    showlegend = nrow(pattern_segments) > 0,
    legend = list(orientation = "h", x = 0, y = 1.08, font = list(size = 9)),
    annotations = list(list(
      x = 0.5, y = 0.02, xref = "paper", yref = "paper",
      text = paste0("2D pseudo-3D dot-node target model on white background; spike timestamps and D-depth positions are data-derived; ", depth_note, "."),
      showarrow = FALSE, font = list(size = 10, color = "#64748b")
    ))
  )
  if (isTRUE(animate_particles) && nrow(dot_df) > 0 && "animation_opts" %in% getNamespaceExports("plotly")) {
    p <- plotly::animation_opts(
      p,
      frame = 160,
      transition = 0,
      redraw = FALSE,
      mode = "immediate"
    )
    if ("animation_slider" %in% getNamespaceExports("plotly")) {
      p <- plotly::animation_slider(p, hide = TRUE)
    }
  }
  p <- p %>% config(
    displaylogo = FALSE,
    scrollZoom = TRUE,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )
  stpd_dbs_track_autoplay_particles(p, enabled = isTRUE(animate_particles) && nrow(dot_df) > 0)
}

stpd_dbs_track_3d_color <- function(name, alpha = 1) {
  name <- as.character(name %||% "STN")[1]
  alpha <- suppressWarnings(as.numeric(alpha %||% 1))[1]
  if (!is.finite(alpha)) alpha <- 1
  alpha <- max(0, min(1, alpha))
  switch(
    name,
    GPe = sprintf("rgba(240,90,40,%.3f)", alpha),
    GPi = sprintf("rgba(185,214,238,%.3f)", alpha),
    STN = sprintf("rgba(236,241,245,%.3f)", alpha),
    sprintf("rgba(148,163,184,%.3f)", alpha)
  )
}

stpd_dbs_track_3d_side_center <- function(side, plot_sides = c("L", "R")) {
  side <- toupper(as.character(side %||% "R"))[1]
  plot_sides <- toupper(as.character(plot_sides %||% c("L", "R")))
  plot_sides <- intersect(plot_sides, c("L", "R"))
  if (length(plot_sides) <= 1L) return(0)
  if (identical(side, "L")) -0.62 else 0.62
}

stpd_dbs_track_3d_nucleus_spec <- function(structure, side = "R", plot_sides = c("L", "R")) {
  structure <- as.character(structure %||% "STN")[1]
  side <- toupper(as.character(side %||% "R"))[1]
  sign_x <- if (identical(side, "L")) -1 else 1
  base_x <- stpd_dbs_track_3d_side_center(side, plot_sides = plot_sides)
  switch(
    structure,
    GPe = list(
      center = c(base_x - sign_x * 0.34, 0.04, 0.00),
      radii = c(0.48, 0.34, 0.92),
      rot_z = sign_x * 0.08,
      rot_x = -0.10
    ),
    GPi = list(
      center = c(base_x - sign_x * 0.07, -0.02, -0.16),
      radii = c(0.44, 0.30, 0.64),
      rot_z = sign_x * -0.10,
      rot_x = 0.06
    ),
    STN = list(
      center = c(base_x + sign_x * 0.30, -0.08, -0.78),
      radii = c(0.42, 0.24, 0.38),
      rot_z = sign_x * -0.12,
      rot_x = 0.04
    ),
    list(
      center = c(base_x, 0, 0),
      radii = c(0.40, 0.30, 0.50),
      rot_z = 0,
      rot_x = 0
    )
  )
}

stpd_dbs_track_3d_ellipsoid_mesh <- function(center, radii, n_theta = 34L, n_phi = 16L,
                                             rot_z = 0, rot_x = 0) {
  center <- suppressWarnings(as.numeric(center))
  radii <- suppressWarnings(as.numeric(radii))
  if (length(center) != 3L || any(!is.finite(center))) center <- c(0, 0, 0)
  if (length(radii) != 3L || any(!is.finite(radii)) || any(radii <= 0)) radii <- c(1, 1, 1)
  n_theta <- max(12L, suppressWarnings(as.integer(n_theta %||% 34L))[1])
  n_phi <- max(8L, suppressWarnings(as.integer(n_phi %||% 16L))[1])
  theta <- seq(0, 2 * pi, length.out = n_theta + 1L)[seq_len(n_theta)]
  phi <- seq(-pi / 2, pi / 2, length.out = n_phi)
  grid <- expand.grid(theta = theta, phi = phi)
  x <- radii[1] * cos(grid$phi) * cos(grid$theta)
  y <- radii[2] * cos(grid$phi) * sin(grid$theta)
  z <- radii[3] * sin(grid$phi)
  cz <- cos(rot_z); sz <- sin(rot_z)
  xz <- x * cz - y * sz
  yz <- x * sz + y * cz
  cx <- cos(rot_x); sx <- sin(rot_x)
  yx <- yz * cx - z * sx
  zx <- yz * sx + z * cx
  x <- xz + center[1]
  y <- yx + center[2]
  z <- zx + center[3]

  face_i <- integer(0); face_j <- integer(0); face_k <- integer(0)
  vertex_id <- function(pp, tt) (pp - 1L) * n_theta + ((tt - 1L) %% n_theta)
  for (pp in seq_len(n_phi - 1L)) {
    for (tt in seq_len(n_theta)) {
      a <- vertex_id(pp, tt)
      b <- vertex_id(pp, tt + 1L)
      c <- vertex_id(pp + 1L, tt)
      d <- vertex_id(pp + 1L, tt + 1L)
      face_i <- c(face_i, a, b)
      face_j <- c(face_j, b, d)
      face_k <- c(face_k, c, c)
    }
  }
  list(x = x, y = y, z = z, i = face_i, j = face_j, k = face_k,
       n_theta = n_theta, n_phi = n_phi)
}

stpd_dbs_track_3d_wireframe <- function(mesh, lat_step = 3L, lon_step = 4L) {
  n_theta <- mesh$n_theta
  n_phi <- mesh$n_phi
  mat <- matrix(seq_along(mesh$x), nrow = n_theta, ncol = n_phi)
  parts <- list()
  for (pp in unique(c(1L, seq(2L, n_phi - 1L, by = lat_step), n_phi))) {
    idx <- c(mat[, pp], mat[1, pp])
    parts[[length(parts) + 1L]] <- data.frame(x = mesh$x[idx], y = mesh$y[idx], z = mesh$z[idx])
    parts[[length(parts) + 1L]] <- data.frame(x = NA_real_, y = NA_real_, z = NA_real_)
  }
  for (tt in seq(1L, n_theta, by = lon_step)) {
    idx <- mat[tt, ]
    parts[[length(parts) + 1L]] <- data.frame(x = mesh$x[idx], y = mesh$y[idx], z = mesh$z[idx])
    parts[[length(parts) + 1L]] <- data.frame(x = NA_real_, y = NA_real_, z = NA_real_)
  }
  dplyr::bind_rows(parts)
}

stpd_dbs_track_add_3d_nucleus <- function(p, structure, side, plot_sides,
                                          active = TRUE, show_surface = TRUE) {
  spec <- stpd_dbs_track_3d_nucleus_spec(structure, side = side, plot_sides = plot_sides)
  mesh <- stpd_dbs_track_3d_ellipsoid_mesh(
    center = spec$center,
    radii = spec$radii,
    rot_z = spec$rot_z,
    rot_x = spec$rot_x
  )
  opacity <- if (isTRUE(active)) 0.18 else 0.08
  wire_opacity <- if (isTRUE(active)) 0.88 else 0.38
  if (isTRUE(show_surface)) {
    p <- add_trace(
      p,
      type = "mesh3d",
      x = mesh$x, y = mesh$y, z = mesh$z,
      i = mesh$i, j = mesh$j, k = mesh$k,
      color = stpd_dbs_track_3d_color(structure, 1),
      opacity = opacity,
      name = paste(side, structure),
      hoverinfo = "text",
      text = paste0(ifelse(side == "L", "LT", "RT"), " ", structure, " schematic"),
      showlegend = FALSE,
      inherit = FALSE
    )
  }
  wf <- stpd_dbs_track_3d_wireframe(mesh)
  add_trace(
    p,
    type = "scatter3d",
    mode = "lines",
    x = wf$x, y = wf$y, z = wf$z,
    line = list(color = stpd_dbs_track_3d_color(structure, wire_opacity), width = if (isTRUE(active)) 2.4 else 1.4),
    name = paste(side, structure, "wire"),
    hoverinfo = "skip",
    showlegend = FALSE,
    inherit = FALSE
  )
}

stpd_dbs_track_pattern_color <- function(pattern, source_kind = "auto") {
  style <- pattern_strip_style(as.character(pattern %||% "others")[1],
                               source = if (identical(as.character(source_kind %||% "auto")[1], "manual")) "manual" else "auto")
  style$color %||% "#64748b"
}

stpd_dbs_track_static_nucleus_fill <- function(structure) {
  structure <- as.character(structure %||% "STN")[1]
  switch(
    structure,
    GPe = "#f05a28",
    GPi = "#b8cce0",
    STN = "#d8dee6",
    "#cbd5e1"
  )
}

stpd_dbs_track_shape_y_bounds <- function(shape_df, side, structure,
                                          fallback_top = 1.25, fallback_bottom = -1.25) {
  fallback_top <- suppressWarnings(as.numeric(fallback_top %||% 1.25))[1]
  fallback_bottom <- suppressWarnings(as.numeric(fallback_bottom %||% -1.25))[1]
  if (!is.finite(fallback_top)) fallback_top <- 1.25
  if (!is.finite(fallback_bottom)) fallback_bottom <- -1.25
  if (fallback_top <= fallback_bottom) {
    tmp <- fallback_top
    fallback_top <- fallback_bottom
    fallback_bottom <- tmp
  }
  if (!is.data.frame(shape_df) || nrow(shape_df) == 0L ||
      !all(c("side", "structure", "y") %in% names(shape_df))) {
    return(c(top = fallback_top, bottom = fallback_bottom))
  }
  idx <- which(
    toupper(as.character(shape_df$side)) == toupper(as.character(side %||% ""))[1] &
      as.character(shape_df$structure) == as.character(structure %||% "")[1] &
      is.finite(shape_df$y)
  )
  if (length(idx) == 0L) return(c(top = fallback_top, bottom = fallback_bottom))
  rng <- range(shape_df$y[idx], na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) <= 1e-9) {
    return(c(top = fallback_top, bottom = fallback_bottom))
  }
  c(top = rng[2], bottom = rng[1])
}

stpd_dbs_track_shape_for_layout <- function(name, side_x, target_structure,
                                            y_top, y_bottom, n = 220,
                                            active = TRUE,
                                            show_anatomical_context = FALSE) {
  name <- as.character(name %||% "STN")[1]
  if (isTRUE(show_anatomical_context)) {
    stpd_dbs_track_context_model_shape(
      name,
      target_structure = target_structure,
      side_x = side_x,
      y_top = y_top,
      y_bottom = y_bottom,
      n = n
    )
  } else if (isTRUE(active)) {
    stpd_dbs_track_focus_nucleus_shape(
      name,
      side_x = side_x,
      y_top = y_top,
      y_bottom = y_bottom,
      n = n
    )
  } else {
    stpd_dbs_track_context_nucleus_shape(
      name,
      side_x = side_x,
      y_top = y_top,
      y_bottom = y_bottom,
      n = n
    )
  }
}

stpd_dbs_track_static_layout_data <- function(prep, show_anatomical_context = FALSE,
                                              depth_direction = c("larger_deeper", "larger_shallower")) {
  depth_direction <- match.arg(depth_direction)
  rows <- prep$rows %||% data.frame()
  spikes <- prep$spikes %||% data.frame()
  pattern_segments <- prep$pattern_segments %||% data.frame()
  if (nrow(rows) == 0) {
    return(list(rows = rows, spikes = spikes, pattern_segments = pattern_segments))
  }

  rows$side <- toupper(as.character(rows$side))
  rows$trajectory_label <- stpd_dbs_track_trajectory_label(rows$trajectory %||% NA_character_)
  rows$structure <- as.character(rows$structure %||% "STN")
  depth_reference <- prep$depth_reference %||% rows
  if (!is.data.frame(depth_reference) || nrow(depth_reference) == 0) depth_reference <- rows
  if (!("side" %in% names(depth_reference))) depth_reference$side <- NA_character_
  if (!("trajectory" %in% names(depth_reference))) depth_reference$trajectory <- NA_character_
  if (!("recording_depth" %in% names(depth_reference))) depth_reference$recording_depth <- NA_real_
  if (!("structure" %in% names(depth_reference))) depth_reference$structure <- NA_character_
  rows$recording_depth <- suppressWarnings(as.numeric(rows$recording_depth))
  depth_reference$side <- toupper(as.character(depth_reference$side))
  depth_reference$trajectory_label <- stpd_dbs_track_trajectory_label(depth_reference$trajectory)
  depth_reference$recording_depth <- suppressWarnings(as.numeric(depth_reference$recording_depth))
  depth_reference$structure <- as.character(depth_reference$structure)
  missing_ref_structure <- is.na(depth_reference$structure) | !nzchar(depth_reference$structure)
  if (any(missing_ref_structure) && "train" %in% names(depth_reference) && "train" %in% names(rows)) {
    matched_structure <- rows$structure[match(depth_reference$train, rows$train)]
    use_match <- missing_ref_structure & !is.na(matched_structure) & nzchar(matched_structure)
    depth_reference$structure[use_match] <- matched_structure[use_match]
    missing_ref_structure <- is.na(depth_reference$structure) | !nzchar(depth_reference$structure)
  }
  depth_reference$structure[missing_ref_structure] <- rows$structure[1] %||% "STN"

  focus_bounds <- stpd_dbs_track_focus_bounds()
  depth_top <- unname(focus_bounds["top"])
  depth_bottom <- unname(focus_bounds["bottom"])
  visible_structures <- unique(as.character(rows$structure))
  visible_structures <- visible_structures[!is.na(visible_structures) & nzchar(visible_structures)]
  plot_sides <- sort(unique(rows$side[rows$side %in% c("L", "R")]))
  if (length(plot_sides) == 0L) plot_sides <- c("L", "R")
  nuclei <- stpd_dbs_track_nucleus_catalog()[c("GPe", "GPi", "STN")]
  use_standard_layout <- isTRUE(show_anatomical_context) ||
    sum(visible_structures %in% names(nuclei)) > 1L
  nuclei_to_draw <- Filter(function(nu) {
    isTRUE(use_standard_layout) || nu$name %in% visible_structures
  }, nuclei)
  target_structure <- visible_structures[visible_structures %in% names(nuclei)]
  target_structure <- if (length(target_structure) > 0L) target_structure[1] else (visible_structures[1] %||% "STN")

  shape_df <- dplyr::bind_rows(lapply(plot_sides, function(ss) {
    side_x <- stpd_dbs_track_focus_side_x(ss)
    dplyr::bind_rows(lapply(nuclei_to_draw, function(nu) {
      active <- nu$name %in% visible_structures
      shape <- stpd_dbs_track_shape_for_layout(
        nu$name,
        side_x = side_x,
        target_structure = target_structure,
        y_top = depth_top,
        y_bottom = depth_bottom,
        n = 220,
        active = active,
        show_anatomical_context = use_standard_layout
      )
      background_alpha <- switch(nu$name, STN = 0.15, GPe = 0.24, GPi = 0.24, 0.18)
      pts <- shape$points
      pts$side <- ss
      pts$structure <- nu$name
      pts$group <- paste(ss, nu$name, sep = "_")
      pts$represented <- active
      pts$active <- active
      pts$fill_alpha <- ifelse(active, 0.58, background_alpha)
      pts
    }))
  }))
  nucleus_label_df <- dplyr::bind_rows(lapply(plot_sides, function(ss) {
    side_x <- stpd_dbs_track_focus_side_x(ss)
    dplyr::bind_rows(lapply(nuclei_to_draw, function(nu) {
      active <- nu$name %in% visible_structures
      shape <- stpd_dbs_track_shape_for_layout(
        nu$name,
        side_x = side_x,
        target_structure = target_structure,
        y_top = depth_top,
        y_bottom = depth_bottom,
        n = if (isTRUE(use_standard_layout)) 80 else if (isTRUE(active)) 40 else 80,
        active = active,
        show_anatomical_context = use_standard_layout
      )
      data.frame(
        x = shape$label_x,
        y = shape$label_y,
        label = nu$name,
        side = ss,
        represented = active,
        active = active,
        stringsAsFactors = FALSE
      )
    }))
  }))
  nucleus_label_df <- stpd_dbs_track_collapse_nucleus_labels(nucleus_label_df)

  rows$depth_y <- NA_real_
  rows$trajectory_offset <- 0
  rows$lane_y <- NA_real_
  for (ss in unique(rows$side)) {
    idx_side <- which(rows$side == ss)
    if (length(idx_side) == 0L) next
    ref_idx <- which(depth_reference$side == ss & is.finite(depth_reference$recording_depth))
    rows$trajectory_offset[idx_side] <- stpd_dbs_track_trajectory_offsets(
      rows$trajectory_label[idx_side],
      levels = depth_reference$trajectory_label[ref_idx],
      span = 0.18
    )
    for (structure_name in unique(rows$structure[idx_side])) {
      idx <- idx_side[rows$structure[idx_side] == structure_name]
      if (length(idx) == 0L) next
      structure_ref_idx <- ref_idx[as.character(depth_reference$structure[ref_idx]) == structure_name]
      if (length(structure_ref_idx) == 0L) structure_ref_idx <- ref_idx
      ref_depth <- depth_reference$recording_depth[structure_ref_idx]
      if (!any(is.finite(ref_depth))) ref_depth <- rows$recording_depth[idx]
      y_bounds <- stpd_dbs_track_shape_y_bounds(
        shape_df,
        side = ss,
        structure = structure_name,
        fallback_top = depth_top,
        fallback_bottom = depth_bottom
      )
      mapped_y <- stpd_dbs_track_rescale_depth(
        rows$recording_depth[idx],
        reference_depth = ref_depth,
        direction = depth_direction,
        y_top = unname(y_bounds["top"]),
        y_bottom = unname(y_bounds["bottom"])
      )
      finite_ref_depth <- ref_depth[is.finite(ref_depth)]
      if (length(finite_ref_depth) < 2L ||
          diff(range(finite_ref_depth, na.rm = TRUE)) <= 1e-9) {
        mapped_y[is.finite(rows$recording_depth[idx])] <- mean(unname(y_bounds))
      }
      rows$depth_y[idx] <- mapped_y
      rows$lane_y[idx] <- stpd_dbs_track_display_lanes(
        rows$side[idx],
        rows$depth_y[idx],
        preferred_gap = 0.24,
        y_min = unname(y_bounds["bottom"]),
        y_max = unname(y_bounds["top"])
      )
    }
  }
  missing_depth_y <- !is.finite(rows$depth_y)
  if (any(missing_depth_y)) {
    rows$depth_y[missing_depth_y] <- 0
  }
  rows$electrode_x <- stpd_dbs_track_focus_side_x(rows$side) + rows$trajectory_offset
  missing_lane_y <- !is.finite(rows$lane_y)
  if (any(missing_lane_y)) rows$lane_y[missing_lane_y] <- rows$depth_y[missing_lane_y]
  rows$lane_offset_y <- rows$lane_y - rows$depth_y
  raster_bounds <- stpd_dbs_track_raster_bounds(rows$side, length_multiplier = 1.5)
  rows$raster_x0 <- raster_bounds$x0
  rows$raster_x1 <- raster_bounds$x1
  rows$raster_label_x <- rows$raster_x0 + 0.02
  rows$label_hjust <- 0

  if (nrow(spikes) > 0) {
    spikes <- dplyr::left_join(
      spikes,
      rows[, c("train", "lane_y", "raster_x0", "raster_x1"), drop = FALSE],
      by = "train"
    )
    spikes$x <- spikes$raster_x0 + pmax(0, pmin(1, spikes$spike_time_rel)) * (spikes$raster_x1 - spikes$raster_x0)
    spikes$y0 <- spikes$lane_y - 0.032
    spikes$y1 <- spikes$lane_y + 0.032
  }

  if (nrow(pattern_segments) > 0) {
    pattern_segments <- dplyr::left_join(
      pattern_segments,
      rows[, c("train", "side", "lane_y", "raster_x0", "raster_x1",
               "trajectory_label", "recording_depth"), drop = FALSE],
      by = "train"
    )
    pattern_segments$x0 <- pattern_segments$raster_x0 + pmax(0, pmin(1, pattern_segments$start_rel)) * (pattern_segments$raster_x1 - pattern_segments$raster_x0)
    pattern_segments$x1 <- pattern_segments$raster_x0 + pmax(0, pmin(1, pattern_segments$end_rel)) * (pattern_segments$raster_x1 - pattern_segments$raster_x0)
    pattern_segments$y <- pattern_segments$lane_y
    pattern_segments <- pattern_segments[
      is.finite(pattern_segments$x0) & is.finite(pattern_segments$x1) &
        is.finite(pattern_segments$y) & pattern_segments$x1 > pattern_segments$x0,
      , drop = FALSE
    ]
    if (nrow(pattern_segments) > 0) {
      pattern_segments$legend_name <- paste0(
        ifelse(as.character(pattern_segments$source_kind) == "manual", "MANUAL ",
               ifelse(as.character(pattern_segments$source_kind) == "final", "FINAL ", "AUTO ")),
        as.character(pattern_segments$pattern)
      )
      pattern_segments$pattern_color <- vapply(
        seq_len(nrow(pattern_segments)),
        function(ii) stpd_dbs_track_pattern_color(pattern_segments$pattern[ii], pattern_segments$source_kind[ii]),
        character(1)
      )
    }
  }

  track_rows <- unique(rows[, c("side", "trajectory_label", "electrode_x"), drop = FALSE])
  track_rows <- track_rows[order(track_rows$side, track_rows$electrode_x), , drop = FALSE]
  track_rows$depth_top <- depth_top
  track_rows$depth_bottom <- depth_bottom
  track_rows$cap_x0 <- track_rows$electrode_x - 0.045
  track_rows$cap_x1 <- track_rows$electrode_x + 0.045
  track_rows$shadow_x <- track_rows$electrode_x + 0.018
  track_rows$left_rim_x <- track_rows$electrode_x - 0.022
  track_rows$right_rim_x <- track_rows$electrode_x + 0.022
  track_rows$highlight_x <- track_rows$electrode_x - 0.010

  structure_track_rows <- unique(rows[, c("side", "structure", "trajectory_label", "electrode_x"), drop = FALSE])
  structure_track_rows <- structure_track_rows[
    order(structure_track_rows$side, structure_track_rows$structure, structure_track_rows$electrode_x),
    , drop = FALSE
  ]
  tick_df <- dplyr::bind_rows(lapply(unique(structure_track_rows$side), function(ss) {
    side_structures <- unique(as.character(rows$structure[rows$side == ss]))
    side_structures <- side_structures[!is.na(side_structures) & nzchar(side_structures)]
    dplyr::bind_rows(lapply(side_structures, function(structure_name) {
      ref_idx <- which(
        depth_reference$side == ss &
          as.character(depth_reference$structure) == structure_name &
          is.finite(depth_reference$recording_depth)
      )
      if (length(ref_idx) == 0L) {
        ref_idx <- which(depth_reference$side == ss & is.finite(depth_reference$recording_depth))
      }
      ref_depth <- depth_reference$recording_depth[ref_idx]
      if (!any(is.finite(ref_depth))) {
        ref_depth <- rows$recording_depth[rows$side == ss & rows$structure == structure_name]
      }
      tick_depth <- stpd_dbs_track_depth_ticks(ref_depth, max_ticks = if (length(side_structures) > 1L) 5L else 8L)
      if (length(tick_depth) == 0L) return(data.frame())
      y_bounds <- stpd_dbs_track_shape_y_bounds(
        shape_df,
        side = ss,
        structure = structure_name,
        fallback_top = depth_top,
        fallback_bottom = depth_bottom
      )
      tick_y <- stpd_dbs_track_rescale_depth(
        tick_depth,
        reference_depth = ref_depth,
        direction = depth_direction,
        y_top = unname(y_bounds["top"]),
        y_bottom = unname(y_bounds["bottom"])
      )
      finite_ref_depth <- ref_depth[is.finite(ref_depth)]
      if (length(finite_ref_depth) < 2L ||
          diff(range(finite_ref_depth, na.rm = TRUE)) <= 1e-9) {
        tick_y[is.finite(tick_depth)] <- mean(unname(y_bounds))
      }
      side_tracks <- structure_track_rows[
        structure_track_rows$side == ss & structure_track_rows$structure == structure_name,
        , drop = FALSE
      ]
      if (nrow(side_tracks) == 0L) return(data.frame())
      label_x <- if (ss == "L") {
        min(track_rows$electrode_x[track_rows$side == ss], na.rm = TRUE) - 0.16
      } else {
        max(track_rows$electrode_x[track_rows$side == ss], na.rm = TRUE) + 0.16
      }
      dplyr::bind_rows(lapply(seq_len(nrow(side_tracks)), function(ii) {
        data.frame(
          side = ss,
          structure = structure_name,
          x0 = side_tracks$electrode_x[ii] - 0.055,
          x1 = side_tracks$electrode_x[ii] + 0.055,
          label_x = label_x,
          y = tick_y,
          label = paste0("D", formatC(tick_depth, format = "fg", digits = 4)),
          hjust = ifelse(ss == "L", 1, 0),
          label_color = stpd_dbs_track_structure_label_color(structure_name),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }))

  list(
    rows = rows,
    spikes = spikes,
    pattern_segments = pattern_segments,
    shape_df = shape_df,
    nucleus_label_df = nucleus_label_df,
    track_rows = track_rows,
    tick_df = tick_df,
    depth_top = depth_top,
    depth_bottom = depth_bottom,
    plot_sides = plot_sides,
    visible_structures = visible_structures
  )
}

stpd_dbs_track_ggplot_static <- function(prep, time_unit = c("s", "ms"), show_labels = TRUE,
                                         show_anatomical_context = FALSE,
                                         depth_direction = c("larger_deeper", "larger_shallower"),
                                         title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required for static DBS figure export.", call. = FALSE)
  }
  time_unit <- match.arg(time_unit)
  depth_direction <- match.arg(depth_direction)
  layout_data <- stpd_dbs_track_static_layout_data(
    prep,
    show_anatomical_context = show_anatomical_context,
    depth_direction = depth_direction
  )
  rows <- layout_data$rows %||% data.frame()
  if (nrow(rows) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No LT/RT spike trains with parseable depth are available.", size = 4) +
        ggplot2::theme_void()
    )
  }
  rows$site_color <- vapply(rows$structure, stpd_dbs_track_structure_label_color, character(1))

  shape_df <- layout_data$shape_df %||% data.frame()
  nucleus_label_df <- layout_data$nucleus_label_df %||% data.frame()
  track_rows <- layout_data$track_rows %||% data.frame()
  tick_df <- layout_data$tick_df %||% data.frame()
  spikes <- layout_data$spikes %||% data.frame()
  pattern_segments <- layout_data$pattern_segments %||% data.frame()
  depth_top <- layout_data$depth_top %||% 1.25
  depth_bottom <- layout_data$depth_bottom %||% -1.25
  plot_sides <- layout_data$plot_sides %||% c("L", "R")
  visible_structures <- layout_data$visible_structures %||% unique(as.character(rows$structure))
  shell_df <- data.frame()
  dot_df <- data.frame()
  if (nrow(shape_df) > 0) {
    shape_groups <- split(shape_df, shape_df$group)
    shell_df <- dplyr::bind_rows(lapply(shape_groups, function(sh) {
      stpd_dbs_track_nucleus_shell_layers(
        sh,
        structure = unique(as.character(sh$structure))[1],
        side = unique(as.character(sh$side))[1],
        active = isTRUE(unique(sh$active)[1])
      )
    }))
    dot_df <- dplyr::bind_rows(lapply(shape_groups, function(sh) {
      stpd_dbs_track_nucleus_dot_cloud(
        sh,
        structure = unique(as.character(sh$structure))[1],
        side = unique(as.character(sh$side))[1],
        active = isTRUE(unique(sh$active)[1])
      )
    }))
  }

  if (is.null(title) || !nzchar(as.character(title)[1])) {
    title <- paste0("Target nucleus spike-train map | ", paste(unique(rows$structure), collapse = "/"), " | ", nrow(rows), " trains")
  }
  scale_label <- if (identical(time_unit, "ms")) {
    paste0(signif(unique(rows$window_sec)[1] * 1000, 5), " ms window")
  } else {
    paste0(signif(unique(rows$window_sec)[1], 5), " s window")
  }
  scale_rows <- unique(rows[, c("side", "raster_x0", "raster_x1"), drop = FALSE])
  scale_rows$y <- depth_bottom - 0.43
  scale_rows$label <- paste0(ifelse(scale_rows$side == "L", "LT rows: ", "RT rows: "), scale_label)
  side_label_df <- data.frame(
    x = stpd_dbs_track_focus_side_x(plot_sides),
    y = rep(depth_top + 0.36, length(plot_sides)),
    label = ifelse(plot_sides == "L", "LT", "RT"),
    stringsAsFactors = FALSE
  )
  label_rows <- rows
  label_rows$label <- paste0(
    ifelse(label_rows$side == "L", "LT", "RT"),
    " ", label_rows$trajectory_label,
    " D", signif(label_rows$recording_depth, 5),
    " | ", label_rows$n_spikes_window, " spikes"
  )
  connector_df <- rows[
    is.finite(rows$electrode_x) & is.finite(rows$depth_y) & is.finite(rows$lane_y),
    , drop = FALSE
  ]
  connector_df$xend <- ifelse(connector_df$side == "L", connector_df$raster_x1, connector_df$raster_x0)

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = data.frame(x = 0, xend = 0, y = depth_bottom - 0.18, yend = depth_top + 0.18),
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = 0.28, color = "#cbd5e1", linetype = "dotted"
    )
  if (nrow(shape_df) > 0) {
    p <- p +
      ggplot2::geom_polygon(
        data = shape_df,
        ggplot2::aes(x = x, y = y, group = group),
        fill = "#ffffff", color = "#ffffff", linewidth = 0, alpha = 0,
        show.legend = FALSE
      )
  }
  if (nrow(shell_df) > 0) {
    for (gg in stpd_dbs_track_draw_groups(shell_df)) {
      sh <- shell_df[shell_df$group == gg, , drop = FALSE]
      if (nrow(sh) == 0) next
      p <- p +
        ggplot2::geom_polygon(
          data = sh,
          ggplot2::aes(x = x, y = y, group = group),
          fill = unique(sh$fill_color)[1],
          color = unique(sh$line_color)[1],
          linewidth = unique(sh$line_width)[1] * 0.18,
          show.legend = FALSE
        )
    }
  }
  if (nrow(dot_df) > 0) {
    dot_df <- dot_df[order(dot_df$active, dot_df$pseudo_depth, dot_df$seed_id), , drop = FALSE]
    p <- p +
      ggplot2::geom_point(
        data = dot_df,
        ggplot2::aes(x = dot_shadow_x, y = dot_shadow_y, size = dot_shadow_size),
        color = dot_df$dot_shadow_color, shape = 16, stroke = 0, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = dot_df,
        ggplot2::aes(x = x, y = y, size = dot_body_size),
        color = dot_df$dot_color, shape = 16, stroke = 0, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = dot_df,
        ggplot2::aes(x = dot_side_x, y = dot_side_y, size = dot_side_size),
        color = dot_df$dot_side_color, shape = 16, stroke = 0, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = dot_df,
        ggplot2::aes(x = dot_inner_x, y = dot_inner_y, size = dot_inner_size),
        color = dot_df$dot_inner_color, shape = 16, stroke = 0, show.legend = FALSE
      ) +
      ggplot2::scale_size_identity()
  }
  if (nrow(nucleus_label_df) > 0) {
    nucleus_label_df <- nucleus_label_df[order(nucleus_label_df$active), , drop = FALSE]
    nucleus_label_df$label_alpha <- ifelse(nucleus_label_df$active, 1, 0.62)
    nucleus_label_df$text_color <- vapply(nucleus_label_df$label, stpd_dbs_track_structure_label_color, character(1))
    p <- p +
      ggplot2::geom_text(
        data = nucleus_label_df,
        ggplot2::aes(x = x, y = y, label = label),
        size = 3.25, color = nucleus_label_df$text_color, alpha = nucleus_label_df$label_alpha,
        fontface = "bold", show.legend = FALSE
      )
  }
  if (nrow(track_rows) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = shadow_x, xend = shadow_x, y = depth_top, yend = depth_bottom),
        color = "#0f172a", linewidth = 2.85, alpha = 0.14, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = electrode_x, xend = electrode_x, y = depth_top, yend = depth_bottom),
        color = "#cbd5e1", linewidth = 2.35, alpha = 0.94, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = electrode_x, xend = electrode_x, y = depth_top, yend = depth_bottom),
        color = "#f8fafc", linewidth = 1.55, alpha = 0.98, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = left_rim_x, xend = left_rim_x, y = depth_top, yend = depth_bottom),
        color = "#64748b", linewidth = 0.34, alpha = 0.56, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = right_rim_x, xend = right_rim_x, y = depth_top, yend = depth_bottom),
        color = "#38bdf8", linewidth = 0.30, alpha = 0.70, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = highlight_x, xend = highlight_x, y = depth_top, yend = depth_bottom),
        color = "#ffffff", linewidth = 0.34, alpha = 0.90, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = cap_x0, xend = cap_x1, y = depth_top, yend = depth_top),
        color = "#cbd5e1", linewidth = 1.20, alpha = 0.92, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = track_rows,
        ggplot2::aes(x = cap_x0, xend = cap_x1, y = depth_bottom, yend = depth_bottom),
        color = "#cbd5e1", linewidth = 1.05, alpha = 0.80, lineend = "round"
      )
  }
  if (nrow(tick_df) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = tick_df,
        ggplot2::aes(x = x0, xend = x1, y = y, yend = y),
        color = "#e2e8f0", linewidth = 0.70, alpha = 0.94, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = tick_df,
        ggplot2::aes(x = x0, xend = x1, y = y, yend = y),
        color = "#38bdf8", linewidth = 0.20, alpha = 0.68, lineend = "round"
      ) +
      ggplot2::geom_text(
        data = tick_df,
        ggplot2::aes(x = label_x, y = y, label = label, hjust = hjust),
        size = 2.3, color = "#475569", lineheight = 0.9
      )
  }
  if (nrow(connector_df) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = connector_df,
        ggplot2::aes(x = electrode_x, xend = xend, y = depth_y, yend = lane_y),
        color = "#94a3b8", linewidth = 0.25, alpha = 0.48
      ) +
      ggplot2::geom_point(
        data = connector_df,
        ggplot2::aes(x = electrode_x, y = depth_y),
        size = 1.55, shape = 21, stroke = 0.42, fill = connector_df$site_color, color = "#ffffff"
      )
  }
  p <- p +
    ggplot2::geom_segment(
      data = rows,
      ggplot2::aes(x = raster_x0, xend = raster_x1, y = lane_y, yend = lane_y),
      color = "#334155", linewidth = 0.23, alpha = 0.52
    )
  if (nrow(pattern_segments) > 0) {
    pattern_colors <- stats::setNames(pattern_segments$pattern_color, pattern_segments$legend_name)
    pattern_colors <- pattern_colors[!duplicated(names(pattern_colors))]
    p <- p +
      ggplot2::geom_segment(
        data = pattern_segments,
        ggplot2::aes(x = x0, xend = x1, y = y, yend = y, color = legend_name),
        linewidth = 1.25, lineend = "butt"
      ) +
      ggplot2::scale_color_manual(values = pattern_colors, breaks = names(pattern_colors), name = NULL)
  }
  if (nrow(spikes) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = spikes,
        ggplot2::aes(x = x, xend = x, y = y0, yend = y1),
        color = "#020617", linewidth = 0.18, alpha = 0.92
      )
  }
  if (isTRUE(show_labels)) {
    p <- p +
      ggplot2::geom_text(
        data = label_rows,
        ggplot2::aes(x = raster_label_x, y = lane_y + 0.055, label = label, hjust = label_hjust),
        size = 2.25, color = "#334155", check_overlap = TRUE
      )
  }
  p +
    ggplot2::geom_segment(
      data = scale_rows,
      ggplot2::aes(x = raster_x0, xend = raster_x1, y = y, yend = y),
      color = "#0f172a", linewidth = 0.55
    ) +
    ggplot2::geom_text(
      data = scale_rows,
      ggplot2::aes(x = (raster_x0 + raster_x1) / 2, y = y - 0.10, label = label),
      size = 2.65, color = "#0f172a"
    ) +
    ggplot2::geom_text(
      data = side_label_df,
      ggplot2::aes(x = x, y = y, label = label),
      size = 3.8, color = "#0f172a", fontface = "bold"
    ) +
    ggplot2::coord_equal(
      xlim = stpd_dbs_track_axis_range(
        rows$raster_x0, rows$raster_x1, rows$raster_label_x,
        shape_df$x, tick_df$label_x, track_rows$cap_x0, track_rows$cap_x1,
        pad = 0.32,
        fallback = c(-5.75, 5.75)
      ),
      ylim = c(depth_bottom - 0.72, depth_top + 0.52),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::labs(
      title = title,
      caption = "Schematic target-nucleus map; D-depth positions and spike timestamps are data-derived, not patient MRI/CT coordinates."
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      plot.title = ggplot2::element_text(face = "bold", color = "#0f172a", size = 12, hjust = 0),
      plot.caption = ggplot2::element_text(color = "#64748b", size = 7.5, hjust = 0.5),
      legend.position = "top",
      legend.justification = "left",
      legend.text = ggplot2::element_text(size = 7.5, color = "#334155"),
      legend.key.width = grid::unit(16, "pt"),
      legend.key.height = grid::unit(8, "pt"),
      plot.margin = ggplot2::margin(8, 28, 14, 28, unit = "pt")
    )
}

stpd_dbs_track_plotly_3d <- function(prep, time_unit = c("s", "ms"), show_labels = TRUE,
                                     show_anatomical_context = FALSE,
                                     depth_direction = c("larger_deeper", "larger_shallower")) {
  time_unit <- match.arg(time_unit)
  depth_direction <- match.arg(depth_direction)
  rows <- prep$rows %||% data.frame()
  spikes <- prep$spikes %||% data.frame()
  pattern_segments <- prep$pattern_segments %||% data.frame()
  if (nrow(rows) == 0) {
    return(layout(
      plot_ly(),
      scene = list(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), zaxis = list(visible = FALSE)),
      annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                              text = "No LT/RT spike trains with parseable depth are available.",
                              showarrow = FALSE))
    ))
  }

  rows$side <- toupper(as.character(rows$side))
  rows$trajectory_label <- stpd_dbs_track_trajectory_label(rows$trajectory %||% NA_character_)
  rows$structure <- as.character(rows$structure %||% "STN")
  visible_structures <- unique(as.character(rows$structure))
  visible_structures <- visible_structures[!is.na(visible_structures) & nzchar(visible_structures)]
  plot_sides <- sort(unique(rows$side[rows$side %in% c("L", "R")]))
  if (length(plot_sides) == 0L) plot_sides <- c("L", "R")
  valid_nuclei <- c("GPe", "GPi", "STN")
  target_structure_for_side <- function(ss) {
    side_structures <- unique(as.character(rows$structure[rows$side == ss]))
    side_structures <- side_structures[side_structures %in% valid_nuclei]
    if (length(side_structures) > 0L) return(side_structures[1])
    fallback <- visible_structures[visible_structures %in% valid_nuclei]
    if (length(fallback) > 0L) fallback[1] else "STN"
  }
  target_by_side <- stats::setNames(vapply(plot_sides, target_structure_for_side, character(1)), plot_sides)

  depth_reference <- prep$depth_reference %||% rows
  if (!is.data.frame(depth_reference) || nrow(depth_reference) == 0) depth_reference <- rows
  if (!("side" %in% names(depth_reference))) depth_reference$side <- NA_character_
  if (!("trajectory" %in% names(depth_reference))) depth_reference$trajectory <- NA_character_
  if (!("recording_depth" %in% names(depth_reference))) depth_reference$recording_depth <- NA_real_
  depth_reference$side <- toupper(as.character(depth_reference$side))
  depth_reference$trajectory_label <- stpd_dbs_track_trajectory_label(depth_reference$trajectory)
  depth_reference$recording_depth <- suppressWarnings(as.numeric(depth_reference$recording_depth))

  rows$depth_z <- NA_real_
  rows$lane_z <- NA_real_
  rows$trajectory_offset <- 0
  rows$electrode_x3 <- NA_real_
  rows$electrode_y3 <- NA_real_
  rows$target_structure <- NA_character_
  rows$raster_y3 <- NA_real_
  rows$raster_x0 <- NA_real_
  rows$raster_x1 <- NA_real_
  rows$raster_label_x <- NA_real_
  for (ss in plot_sides) {
    idx <- which(rows$side == ss)
    if (length(idx) == 0) next
    target_structure <- target_by_side[[ss]] %||% "STN"
    target_spec <- stpd_dbs_track_3d_nucleus_spec(target_structure, side = ss, plot_sides = plot_sides)
    z_top <- target_spec$center[3] + target_spec$radii[3] * 0.82
    z_bottom <- target_spec$center[3] - target_spec$radii[3] * 0.82
    ref_idx <- which(depth_reference$side == ss & is.finite(depth_reference$recording_depth))
    ref_depth <- depth_reference$recording_depth[ref_idx]
    rows$depth_z[idx] <- stpd_dbs_track_rescale_depth(
      rows$recording_depth[idx],
      reference_depth = ref_depth,
      direction = depth_direction,
      y_top = z_top,
      y_bottom = z_bottom
    )
    rows$lane_z[idx] <- stpd_dbs_track_display_lanes(
      rows$side[idx], rows$depth_z[idx],
      preferred_gap = 0.085,
      y_min = z_bottom,
      y_max = z_top
    )
    rows$trajectory_offset[idx] <- stpd_dbs_track_trajectory_offsets(
      rows$trajectory_label[idx],
      levels = depth_reference$trajectory_label[ref_idx],
      span = 0.16
    )
    rows$target_structure[idx] <- target_structure
    rows$electrode_x3[idx] <- target_spec$center[1] + rows$trajectory_offset[idx]
    rows$electrode_y3[idx] <- target_spec$center[2]
    rows$raster_y3[idx] <- target_spec$center[2] - 0.78
    if (identical(ss, "L")) {
      rows$raster_x0[idx] <- target_spec$center[1] - 2.00
      rows$raster_x1[idx] <- target_spec$center[1] - 0.72
      rows$raster_label_x[idx] <- target_spec$center[1] - 2.10
    } else {
      rows$raster_x0[idx] <- target_spec$center[1] + 0.72
      rows$raster_x1[idx] <- target_spec$center[1] + 2.00
      rows$raster_label_x[idx] <- target_spec$center[1] + 2.10
    }
  }

  if (nrow(spikes) > 0) {
    spikes <- dplyr::left_join(
      spikes,
      rows[, c("train", "side", "lane_z", "raster_y3", "raster_x0", "raster_x1"), drop = FALSE],
      by = "train"
    )
    spikes$x <- spikes$raster_x0 + pmax(0, pmin(1, spikes$spike_time_rel)) * (spikes$raster_x1 - spikes$raster_x0)
  }
  if (nrow(pattern_segments) > 0) {
    pattern_segments <- dplyr::left_join(
      pattern_segments,
      rows[, c("train", "side", "lane_z", "raster_y3", "raster_x0", "raster_x1",
               "trajectory_label", "recording_depth"), drop = FALSE],
      by = "train"
    )
    pattern_segments$x0 <- pattern_segments$raster_x0 + pmax(0, pmin(1, pattern_segments$start_rel)) * (pattern_segments$raster_x1 - pattern_segments$raster_x0)
    pattern_segments$x1 <- pattern_segments$raster_x0 + pmax(0, pmin(1, pattern_segments$end_rel)) * (pattern_segments$raster_x1 - pattern_segments$raster_x0)
    pattern_segments <- pattern_segments[
      is.finite(pattern_segments$x0) & is.finite(pattern_segments$x1) &
        is.finite(pattern_segments$lane_z) & pattern_segments$x1 > pattern_segments$x0,
      , drop = FALSE
    ]
  }

  p <- plot_ly()
  nuclei_to_draw <- if (isTRUE(show_anatomical_context)) valid_nuclei else {
    intersect(valid_nuclei, visible_structures)
  }
  if (length(nuclei_to_draw) == 0L) nuclei_to_draw <- unique(as.character(target_by_side))
  for (ss in plot_sides) {
    for (nu in nuclei_to_draw) {
      active <- nu %in% visible_structures
      p <- stpd_dbs_track_add_3d_nucleus(
        p, nu, side = ss, plot_sides = plot_sides,
        active = isTRUE(active),
        show_surface = TRUE
      )
    }
  }
  if (isTRUE(show_labels)) {
    nucleus_labels <- dplyr::bind_rows(lapply(plot_sides, function(ss) {
      dplyr::bind_rows(lapply(nuclei_to_draw, function(nu) {
        spec <- stpd_dbs_track_3d_nucleus_spec(nu, side = ss, plot_sides = plot_sides)
        sign_x <- if (identical(ss, "L")) -1 else 1
        data.frame(
          x = spec$center[1] + sign_x * (spec$radii[1] + 0.10),
          y = spec$center[2],
          z = spec$center[3] + spec$radii[3] * 0.18,
          label = nu,
          active = nu %in% visible_structures,
          stringsAsFactors = FALSE
        )
      }))
    }))
    if (nrow(nucleus_labels) > 0) {
      p <- add_trace(
        p, type = "scatter3d", mode = "text",
        x = nucleus_labels$x, y = nucleus_labels$y, z = nucleus_labels$z,
        text = nucleus_labels$label,
        textfont = list(size = 12, color = ifelse(nucleus_labels$active, "#0f172a", "rgba(71,85,105,0.55)")),
        hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
      )
    }
  }

  track_rows <- unique(rows[, c("side", "trajectory_label", "target_structure", "electrode_x3", "electrode_y3"), drop = FALSE])
  track_rows <- track_rows[is.finite(track_rows$electrode_x3) & is.finite(track_rows$electrode_y3), , drop = FALSE]
  for (ii in seq_len(nrow(track_rows))) {
    ss <- track_rows$side[ii]
    target_structure <- track_rows$target_structure[ii] %||% "STN"
    target_spec <- stpd_dbs_track_3d_nucleus_spec(target_structure, side = ss, plot_sides = plot_sides)
    z_top <- target_spec$center[3] + target_spec$radii[3] * 0.82
    z_bottom <- target_spec$center[3] - target_spec$radii[3] * 0.82
    ex <- track_rows$electrode_x3[ii]
    ey <- track_rows$electrode_y3[ii]
    p <- add_trace(
      p, type = "scatter3d", mode = "lines",
      x = c(ex, ex), y = c(ey, ey), z = c(z_top, z_bottom),
      line = list(color = "rgba(255,255,245,0.96)", width = 14),
      hoverinfo = "text",
      text = paste0(ifelse(ss == "L", "LT", "RT"), " ", track_rows$trajectory_label[ii], " ", target_structure, " target electrode segment"),
      showlegend = FALSE, inherit = FALSE
    )
    contact_z <- seq(z_top, z_bottom, length.out = 6L)
    p <- add_trace(
      p, type = "scatter3d", mode = "markers",
      x = rep(ex, length(contact_z)), y = rep(ey, length(contact_z)), z = contact_z,
      marker = list(size = 5.5, color = rep(c("#f8fafc", "#111827", "#8b5cf6"), length.out = length(contact_z)),
                    line = list(color = "#ffffff", width = 0.6)),
      hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
    )
  }

  for (ii in seq_len(nrow(rows))) {
    rr <- rows[ii, , drop = FALSE]
    if (!all(is.finite(c(rr$electrode_x3[1], rr$electrode_y3[1], rr$depth_z[1], rr$lane_z[1])))) next
    near_x <- if (identical(rr$side[1], "L")) rr$raster_x1[1] else rr$raster_x0[1]
    p <- add_trace(
      p, type = "scatter3d", mode = "lines",
      x = c(rr$electrode_x3[1], near_x),
      y = c(rr$electrode_y3[1], rr$raster_y3[1]),
      z = c(rr$depth_z[1], rr$lane_z[1]),
      line = list(color = "rgba(148,163,184,0.34)", width = 1.1),
      hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
    )
    p <- add_trace(
      p, type = "scatter3d", mode = "markers",
      x = rr$electrode_x3[1], y = rr$electrode_y3[1], z = rr$depth_z[1],
      marker = list(size = 3.8, color = "#ffffff", line = list(color = "#0f172a", width = 1)),
      hoverinfo = "text",
      text = paste0(
        "Train: ", stpd_html_escape(rr$train[1]),
        "<br>Side: ", ifelse(rr$side[1] == "L", "LT", "RT"),
        "<br>Trajectory: ", stpd_html_escape(rr$trajectory_label[1]),
        "<br>Depth: D", signif(rr$recording_depth[1], 5),
        "<br>Spikes in window: ", rr$n_spikes_window[1]
      ),
      showlegend = FALSE, inherit = FALSE
    )
    p <- add_trace(
      p, type = "scatter3d", mode = "lines",
      x = c(rr$raster_x0[1], rr$raster_x1[1]),
      y = c(rr$raster_y3[1], rr$raster_y3[1]),
      z = c(rr$lane_z[1], rr$lane_z[1]),
      line = list(color = "rgba(15,23,42,0.34)", width = 1.2),
      hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
    )
  }

  if (nrow(pattern_segments) > 0) {
    legend_seen <- character(0)
    for (pat in stpd_dbs_track_patterns()) {
      for (src in c("manual", "auto", "final")) {
        sub_pat <- pattern_segments[
          as.character(pattern_segments$pattern) == pat &
            as.character(pattern_segments$source_kind) == src,
          , drop = FALSE
        ]
        if (nrow(sub_pat) == 0) next
        legend_name <- paste0(if (identical(src, "manual")) "MANUAL " else "AUTO ", pat)
        show_leg <- !(legend_name %in% legend_seen)
        legend_seen <- unique(c(legend_seen, legend_name))
        seg <- dplyr::bind_rows(lapply(seq_len(nrow(sub_pat)), function(ii) {
          data.frame(
            x = c(sub_pat$x0[ii], sub_pat$x1[ii], NA_real_),
            y = c(sub_pat$raster_y3[ii], sub_pat$raster_y3[ii], NA_real_),
            z = c(sub_pat$lane_z[ii], sub_pat$lane_z[ii], NA_real_)
          )
        }))
        p <- add_trace(
          p, type = "scatter3d", mode = "lines",
          x = seg$x, y = seg$y, z = seg$z,
          line = list(color = stpd_dbs_track_pattern_color(pat, src), width = 7),
          name = legend_name,
          legendgroup = legend_name,
          showlegend = show_leg,
          hoverinfo = "skip",
          inherit = FALSE
        )
      }
    }
  }

  if (nrow(spikes) > 0) {
    spike_h <- 0.025
    spike_segments <- dplyr::bind_rows(lapply(seq_len(nrow(spikes)), function(ii) {
      data.frame(
        x = c(spikes$x[ii], spikes$x[ii], NA_real_),
        y = c(spikes$raster_y3[ii], spikes$raster_y3[ii], NA_real_),
        z = c(spikes$lane_z[ii] - spike_h, spikes$lane_z[ii] + spike_h, NA_real_)
      )
    }))
    p <- add_trace(
      p, type = "scatter3d", mode = "lines",
      x = spike_segments$x, y = spike_segments$y, z = spike_segments$z,
      line = list(color = "rgba(2,6,23,0.88)", width = 1),
      name = "spikes", hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
    )
  }

  if (isTRUE(show_labels)) {
    label_rows <- rows[is.finite(rows$raster_label_x) & is.finite(rows$raster_y3) & is.finite(rows$lane_z), , drop = FALSE]
    if (nrow(label_rows) > 0) {
      label_rows$label <- paste0(
        ifelse(label_rows$side == "L", "LT", "RT"),
        " ", label_rows$trajectory_label,
        " D", signif(label_rows$recording_depth, 5),
        " | ", label_rows$n_spikes_window, " spikes"
      )
      p <- add_trace(
        p, type = "scatter3d", mode = "text",
        x = label_rows$raster_label_x,
        y = label_rows$raster_y3,
        z = label_rows$lane_z + 0.035,
        text = label_rows$label,
        textfont = list(size = 9, color = "#334155"),
        hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
      )
    }
  }

  title <- paste0(
    "3D target-nucleus model | ",
    stpd_html_escape(paste(unique(rows$structure), collapse = "/")),
    " | ", nrow(rows), " trains"
  )
  layout(
    p,
    title = list(text = title, x = 0.02, xanchor = "left", font = list(size = 15, color = "#0f172a")),
    hoverlabel = stpd_hoverlabel_style(),
    scene = list(
      xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      zaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      aspectmode = "data",
      camera = list(eye = list(x = 1.75, y = -2.25, z = 1.15))
    ),
    paper_bgcolor = "#ffffff",
    plot_bgcolor = "#ffffff",
    margin = list(l = 0, r = 0, t = 52, b = 0),
    showlegend = nrow(pattern_segments) > 0,
    legend = list(orientation = "h", x = 0, y = 1.06, font = list(size = 9)),
    annotations = list(list(
      x = 0.5, y = 0.015, xref = "paper", yref = "paper",
      text = "Schematic 3D model; transparent nuclei are illustrative and depth/spike timing are data-derived.",
      showarrow = FALSE, font = list(size = 10, color = "#64748b")
    ))
  ) %>% config(
    displaylogo = FALSE,
    scrollZoom = TRUE,
    displayModeBar = TRUE
  )
}
