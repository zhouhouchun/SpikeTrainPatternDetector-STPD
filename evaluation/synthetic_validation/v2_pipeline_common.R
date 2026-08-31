options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

stpd_v2_stop <- function(...) stop(..., call. = FALSE)

stpd_v2_env_path <- function(name, default = NULL, must_work = TRUE) {
  value <- Sys.getenv(name, unset = default %||% "")
  if (!nzchar(value)) stpd_v2_stop("Missing environment variable: ", name)
  normalizePath(value, mustWork = must_work)
}

stpd_v2_sha256_file <- function(path) {
  if (!file.exists(path)) stpd_v2_stop("Missing file: ", path)
  digest::digest(path, algo = "sha256", file = TRUE)
}

stpd_v2_assert_sha256 <- function(path, expected) {
  observed <- stpd_v2_sha256_file(path)
  if (!identical(observed, as.character(expected)[1L])) {
    stpd_v2_stop("SHA-256 mismatch for ", basename(path), ".")
  }
  invisible(observed)
}

stpd_v2_read_workbook <- function(path, batch = NULL) {
  sheets <- readxl::excel_sheets(path)
  expected <- c("Batch_A", "Batch_B", "Batch_C")
  if (!identical(sheets, expected)) {
    stpd_v2_stop("The blinded workbook must contain Batch_A, Batch_B, Batch_C.")
  }
  if (!is.null(batch)) {
    batch <- as.character(batch)[1L]
    if (!(batch %in% expected)) stpd_v2_stop("Invalid blinded batch: ", batch)
    sheets <- batch
  }
  out <- setNames(lapply(sheets, function(sheet) {
    z <- as.data.frame(readxl::read_xlsx(path, sheet = sheet),
                       stringsAsFactors = FALSE)
    if (!identical(names(z), c("Sample_ID", "Spike_Index", "Time_s"))) {
      stpd_v2_stop("Unexpected detector columns in ", sheet, ".")
    }
    z$Sample_ID <- as.character(z$Sample_ID)
    z$Spike_Index <- as.integer(z$Spike_Index)
    z$Time_s <- as.numeric(z$Time_s)
    if (anyNA(z$Sample_ID) || any(!nzchar(z$Sample_ID)) ||
        any(!is.finite(z$Spike_Index)) || any(!is.finite(z$Time_s))) {
      stpd_v2_stop("Non-finite or missing blinded input in ", sheet, ".")
    }
    ids <- unique(z$Sample_ID)
    if (length(ids) != 20L) stpd_v2_stop(sheet, " must contain 20 samples.")
    checked <- lapply(ids, function(id) {
      q <- z[z$Sample_ID == id, , drop = FALSE]
      q <- q[order(q$Spike_Index, method = "radix"), , drop = FALSE]
      if (!identical(q$Spike_Index, seq_len(nrow(q))) ||
          any(diff(q$Time_s) <= 0)) {
        stpd_v2_stop("Invalid spike order for ", id, " in ", sheet, ".")
      }
      q
    })
    z <- do.call(rbind, checked)
    rownames(z) <- NULL
    z
  }), sheets)
  if (anyDuplicated(unlist(lapply(out, function(z) unique(z$Sample_ID))))) {
    stpd_v2_stop("A Sample_ID appears in more than one blinded batch.")
  }
  out
}

stpd_v2_assert_blinded_workbook_path <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  if (!identical(basename(path), "spike_timestamps_blinded.xlsx") ||
      !identical(basename(dirname(path)), "detector_inputs")) {
    stpd_v2_stop(
      "Stage B accepts only detector_inputs/spike_timestamps_blinded.xlsx."
    )
  }
  components <- tolower(strsplit(path, .Platform$file.sep, fixed = TRUE)[[1L]])
  if (any(components %in% c("truth", "protected", "calibration", "metadata"))) {
    stpd_v2_stop("A protected path was supplied to Stage B.")
  }
  invisible(path)
}

stpd_v2_read_blinded_holdout <- function(path, expected_batch = NULL) {
  z <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!identical(names(z), c("Sample_ID", "Spike_Index", "Time_s"))) {
    stpd_v2_stop("Unexpected sanitized Stage-A detector-input columns.")
  }
  z$Sample_ID <- as.character(z$Sample_ID)
  z$Spike_Index <- as.integer(z$Spike_Index)
  z$Time_s <- as.numeric(z$Time_s)
  if (anyNA(z$Sample_ID) || any(!nzchar(z$Sample_ID)) ||
      any(!is.finite(z$Spike_Index)) || any(!is.finite(z$Time_s))) {
    stpd_v2_stop("Non-finite or missing sanitized detector input.")
  }
  ids <- unique(z$Sample_ID)
  if (length(ids) != 15L) {
    stpd_v2_stop("Each sanitized Stage-A input must contain 15 holdout samples.")
  }
  rows <- lapply(ids, function(id) {
    q <- z[z$Sample_ID == id, , drop = FALSE]
    q <- q[order(q$Spike_Index, method = "radix"), , drop = FALSE]
    if (!identical(q$Spike_Index, seq_len(nrow(q))) ||
        any(diff(q$Time_s) <= 0)) {
      stpd_v2_stop("Invalid sanitized spike order for ", id, ".")
    }
    q
  })
  z <- do.call(rbind, rows)
  rownames(z) <- NULL
  stpd_v2_assert_blind_artifact(z)
  attr(z, "batch_id") <- as.character(expected_batch %||% "")
  z
}

stpd_v2_spike_lists <- function(batch_table) {
  split_table <- split(batch_table, batch_table$Sample_ID)
  lapply(split_table, function(z) {
    z <- z[order(z$Spike_Index, method = "radix"), , drop = FALSE]
    as.numeric(z$Time_s)
  })
}

stpd_v2_batch_selection <- function(value = Sys.getenv("STPD_V2_BATCH")) {
  allowed <- c("Batch_A", "Batch_B", "Batch_C")
  value <- trimws(as.character(value %||% ""))[1L]
  if (!nzchar(value)) return(allowed)
  if (!(value %in% allowed)) {
    stpd_v2_stop("STPD_V2_BATCH must be Batch_A, Batch_B, or Batch_C.")
  }
  value
}

stpd_v2_recursive_names <- function(x) {
  out <- names(x) %||% character()
  if (is.list(x)) {
    out <- c(out, unlist(lapply(x, stpd_v2_recursive_names), use.names = FALSE))
  }
  out
}

stpd_v2_recursive_characters <- function(x) {
  if (is.character(x)) return(x)
  if (!is.list(x)) return(character())
  unlist(lapply(x, stpd_v2_recursive_characters), use.names = FALSE)
}

stpd_v2_assert_blind_artifact <- function(x) {
  forbidden_names <- c(
    "Template_ID", "Scale_Factor", "Split", "State_Label", "Event_Label",
    "Phenotype_Label", "Generator_Provenance"
  )
  bad_names <- intersect(stpd_v2_recursive_names(x), forbidden_names)
  chars <- stpd_v2_recursive_characters(x)
  bad_values <- chars[grepl("TPL_[0-9]{3}", chars)]
  if (length(bad_names) || length(bad_values)) {
    stpd_v2_stop("A protected label or grouping field entered a blind artifact.")
  }
  invisible(TRUE)
}

stpd_v2_match_spike <- function(time, value) {
  distance <- abs(time - as.numeric(value))
  index <- which.min(distance)
  tolerance <- max(1e-10, max(abs(time), na.rm = TRUE) * 2e-10)
  if (!length(index) || !is.finite(distance[index]) || distance[index] > tolerance) {
    stpd_v2_stop("A calibration boundary does not match a recorded spike.")
  }
  index
}

stpd_v2_calibration_inputs <- function(calibration, batch_table) {
  spikes <- stpd_v2_spike_lists(batch_table)
  calibration <- calibration[calibration$Sample_ID %in% names(spikes), , drop = FALSE]
  expected_modes <- c("Burst", "Pause", "Tonic", "Broad_HFS")
  if (!identical(sort(unique(calibration$Calibration_Mode), method = "radix"),
                 sort(expected_modes, method = "radix")) ||
      any(table(calibration$Calibration_Mode) != 10L)) {
    stpd_v2_stop("Each blinded batch requires 10 calibration episodes per mode.")
  }
  mode_pattern <- c(
    Burst = "burst", Pause = "pause", Tonic = "tonic",
    Broad_HFS = "high_frequency_spiking"
  )
  mode_axis <- c(Burst = "event", Pause = "event", Tonic = "state",
                 Broad_HFS = "state")
  selected <- data.frame(
    Train_ID = as.character(calibration$Sample_ID),
    Group_ID = as.character(calibration$Template_ID),
    Axis = unname(mode_axis[calibration$Calibration_Mode]),
    Episode = as.character(calibration$Episode_ID),
    Pattern = unname(mode_pattern[calibration$Calibration_Mode]),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(calibration)), function(i) {
    id <- as.character(calibration$Sample_ID[i])
    time <- spikes[[id]]
    left <- stpd_v2_match_spike(time, calibration$Start_s[i])
    right <- stpd_v2_match_spike(time, calibration$End_s[i])
    if (right <= left) stpd_v2_stop("Invalid calibration episode geometry.")
    index <- seq.int(left + 1L, right)
    data.frame(
      Train_ID = id, Group_ID = as.character(calibration$Template_ID[i]),
      Right_Spike_Index = index, ISI_s = diff(time)[index - 1L],
      Axis = unname(mode_axis[calibration$Calibration_Mode[i]]),
      Pattern = unname(mode_pattern[calibration$Calibration_Mode[i]]),
      Episode = as.character(calibration$Episode_ID[i]),
      stringsAsFactors = FALSE
    )
  })
  intervals <- do.call(rbind, rows)
  if (anyDuplicated(selected[c("Train_ID", "Axis", "Episode", "Pattern")])) {
    stpd_v2_stop("Duplicated calibration episode key.")
  }
  list(spikes = spikes, selected = selected, intervals = intervals)
}

stpd_v2_write_csv <- function(x, path) {
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE), path,
                   row.names = FALSE, na = "")
  invisible(path)
}

stpd_v2_file_manifest <- function(batch_id, files, roles) {
  stopifnot(length(files) == length(roles))
  data.frame(
    Batch_ID = rep(as.character(batch_id), length(files)),
    File_Role = as.character(roles), File_Name = basename(files),
    SHA256 = vapply(files, stpd_v2_sha256_file, character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_v2_scientific_content_sha256 <- function(intervals, episodes, gaps, states) {
  canonicalize <- function(value, volatile = character()) {
    value <- as.data.frame(value, stringsAsFactors = FALSE)
    value <- value[setdiff(names(value), volatile)]
    if (nrow(value) && ncol(value)) {
      ordering <- lapply(value, function(column) {
        if (is.numeric(column) || is.integer(column) || is.logical(column)) {
          column
        } else {
          as.character(column)
        }
      })
      index <- do.call(order, c(ordering, list(na.last = TRUE, method = "radix")))
      value <- value[index, , drop = FALSE]
    }
    rownames(value) <- NULL
    value
  }
  payload <- list(
    intervals = canonicalize(intervals),
    episodes = canonicalize(episodes),
    gaps = canonicalize(gaps, c("run_id", "gap_id")),
    states = canonicalize(
      states, c("run_id", "state_id", "state_episode_id")
    )
  )
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

stpd_v2_metrics <- function(tp, fp, fn) {
  tp <- sum(as.numeric(tp)); fp <- sum(as.numeric(fp)); fn <- sum(as.numeric(fn))
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (2 * tp + fp + fn > 0) 2 * tp / (2 * tp + fp + fn) else NA_real_
  c(tp = tp, fp = fp, fn = fn, precision = precision, recall = recall, F1 = f1)
}

stpd_v2_iou <- function(a0, a1, b0, b1) {
  overlap <- pmax(0L, pmin(a1, b1) - pmax(a0, b0) + 1L)
  union <- pmax(a1, b1) - pmin(a0, b0) + 1L
  ifelse(union > 0L, overlap / union, 0)
}

stpd_v2_runs <- function(index, positive) {
  index <- as.integer(index); positive <- as.logical(positive)
  keep <- which(positive & is.finite(index))
  if (!length(keep)) return(data.frame(start_isi = integer(), end_isi = integer()))
  values <- index[keep]
  split_id <- cumsum(c(TRUE, diff(values) != 1L))
  do.call(rbind, lapply(split(values, split_id), function(z) {
    data.frame(start_isi = min(z), end_isi = max(z))
  }))
}

stpd_v2_complete_counts <- function(counts, sample_key, signatures = NULL) {
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  sample_key <- as.data.frame(sample_key, stringsAsFactors = FALSE)
  count_key <- c(
    "Estimand", "Variant", "Level", "IoU", "Target",
    "Template_ID", "Scale_Factor"
  )
  required_counts <- c(
    count_key, "Batch_ID", "Sample_ID", "tp", "fp", "fn"
  )
  required_samples <- c(
    "Sample_ID", "Template_ID", "Scale_Factor", "Batch_ID"
  )
  if (!all(required_counts %in% names(counts)) ||
      !all(required_samples %in% names(sample_key))) {
    stpd_v2_stop("Count completion input is missing required columns.")
  }
  signature_columns <- c("Estimand", "Variant", "Level", "IoU", "Target")
  signatures <- if (is.null(signatures)) {
    unique(counts[signature_columns])
  } else {
    signatures <- as.data.frame(signatures, stringsAsFactors = FALSE)
    if (!all(signature_columns %in% names(signatures))) {
      stpd_v2_stop("Expected metric signatures are incomplete.")
    }
    unique(signatures[signature_columns])
  }
  sample_match <- match(counts$Sample_ID, sample_key$Sample_ID)
  if (anyNA(sample_match) ||
      any(as.character(counts$Template_ID) !=
          as.character(sample_key$Template_ID[sample_match])) ||
      any(as.character(counts$Scale_Factor) !=
          as.character(sample_key$Scale_Factor[sample_match])) ||
      any(as.character(counts$Batch_ID) !=
          as.character(sample_key$Batch_ID[sample_match]))) {
    stpd_v2_stop("A metric count has inconsistent protected sample metadata.")
  }
  grid <- merge(signatures, sample_key[required_samples], by = NULL, sort = FALSE)
  merge_key <- c(
    "Estimand", "Variant", "Level", "IoU", "Target", "Batch_ID",
    "Scale_Factor", "Template_ID", "Sample_ID"
  )
  if (anyDuplicated(counts[merge_key])) {
    stpd_v2_stop("Duplicated pre-completion metric count key.")
  }
  count_keys <- do.call(paste, c(counts[merge_key], sep = "\r"))
  grid_keys <- do.call(paste, c(grid[merge_key], sep = "\r"))
  if (any(!count_keys %in% grid_keys)) {
    stpd_v2_stop("A metric count falls outside the declared completion grid.")
  }
  out <- merge(
    grid, counts, by = merge_key, all.x = TRUE, sort = FALSE
  )
  for (column in c("tp", "fp", "fn")) {
    out[[column]][is.na(out[[column]])] <- 0
    out[[column]] <- as.numeric(out[[column]])
  }
  expected <- nrow(signatures) * nrow(sample_key)
  if (nrow(out) != expected || anyDuplicated(out[merge_key])) {
    stpd_v2_stop("Metric zero-row completion did not close its Cartesian grid.")
  }
  scale_n <- aggregate(
    Scale_Factor ~ Estimand + Variant + Level + IoU + Target + Template_ID,
    out, function(x) length(unique(x))
  )
  if (any(scale_n$Scale_Factor != 3L)) {
    stpd_v2_stop("Every Template_ID must carry all three scales into bootstrap.")
  }
  out[order(
    out$Estimand, out$Variant, out$Level, out$IoU, out$Target,
    out$Template_ID, out$Scale_Factor, method = "radix"
  ), , drop = FALSE]
}

stpd_v2_bootstrap <- function(counts, n_bootstrap = 2000L, seed = 20260827L) {
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  clusters <- sort(unique(counts$Template_ID), method = "radix")
  cluster_scale_n <- aggregate(
    Scale_Factor ~ Template_ID, counts, function(x) length(unique(x))
  )
  if (!length(clusters) || any(cluster_scale_n$Scale_Factor != 3L)) {
    stpd_v2_stop(
      "Template-cluster bootstrap requires every template at all three scales."
    )
  }
  group_cols <- setdiff(names(counts), c("Template_ID", "Sample_ID", "tp", "fp", "fn"))
  summarize_once <- function(x) {
    key <- interaction(x[group_cols], drop = TRUE, lex.order = TRUE)
    do.call(rbind, lapply(split(x, key), function(z) {
      out <- z[1L, group_cols, drop = FALSE]
      m <- stpd_v2_metrics(z$tp, z$fp, z$fn)
      cbind(out, as.data.frame(as.list(m), stringsAsFactors = FALSE))
    }))
  }
  observed <- summarize_once(counts)
  set.seed(as.integer(seed))
  draws <- vector("list", as.integer(n_bootstrap))
  for (b in seq_len(as.integer(n_bootstrap))) {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    z <- do.call(rbind, lapply(seq_along(sampled), function(i) {
      q <- counts[counts$Template_ID == sampled[i], , drop = FALSE]
      q$Template_ID <- paste0(q$Template_ID, "__draw", i)
      q
    }))
    s <- summarize_once(z)
    s$bootstrap_id <- b
    draws[[b]] <- s
  }
  draws <- do.call(rbind, draws)
  metric_names <- c("precision", "recall", "F1")
  summary <- do.call(rbind, lapply(seq_len(nrow(observed)), function(i) {
    hit <- rep(TRUE, nrow(draws))
    for (column in group_cols) {
      hit <- hit & as.character(draws[[column]]) == as.character(observed[[column]][i])
    }
    z <- draws[hit, , drop = FALSE]
    do.call(rbind, lapply(metric_names, function(metric) {
      value <- as.numeric(z[[metric]]); value <- value[is.finite(value)]
      data.frame(
        observed[i, group_cols, drop = FALSE], metric = metric,
        observed = as.numeric(observed[[metric]][i]), bootstrap_n = length(value),
        ci_low = if (length(value)) unname(quantile(value, .025, type = 7)) else NA_real_,
        ci_high = if (length(value)) unname(quantile(value, .975, type = 7)) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  }))
  list(observed = observed, summary = summary, draws = draws)
}
