#!/usr/bin/env Rscript

# Reconstruct a label-blind, wide timestamp CSV from the public STN workbook.
# Author: Zhou Houchun

stpd_prepare_public_runtime_input <- function(
    workbook_path,
    output_path,
    metadata_path = sub("\\.csv$", "_metadata.csv", output_path)) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required.", call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required.", call. = FALSE)
  }

  sheets <- readxl::excel_sheets(workbook_path)
  trains <- vector("list", length(sheets))
  metadata <- vector("list", length(sheets))

  for (i in seq_along(sheets)) {
    z <- as.data.frame(
      readxl::read_excel(workbook_path, sheet = sheets[[i]]),
      stringsAsFactors = FALSE
    )
    needed <- c("train_id", "isi_index", "left_timestamp_us",
                "right_timestamp_us", "isi_us")
    missing <- setdiff(needed, names(z))
    if (length(missing)) {
      stop("Sheet '", sheets[[i]], "' lacks: ", paste(missing, collapse = ", "),
           call. = FALSE)
    }

    z$isi_index <- suppressWarnings(as.integer(z$isi_index))
    z$left_timestamp_us <- suppressWarnings(as.numeric(z$left_timestamp_us))
    z$right_timestamp_us <- suppressWarnings(as.numeric(z$right_timestamp_us))
    z$isi_us <- suppressWarnings(as.numeric(z$isi_us))
    z <- z[is.finite(z$isi_index) & is.finite(z$left_timestamp_us) &
             is.finite(z$right_timestamp_us) & is.finite(z$isi_us), , drop = FALSE]
    z <- z[order(z$isi_index), , drop = FALSE]
    if (!nrow(z)) stop("Sheet '", sheets[[i]], "' has no valid ISIs.", call. = FALSE)
    if (anyDuplicated(z$isi_index)) {
      stop("Sheet '", sheets[[i]], "' contains duplicate isi_index values.", call. = FALSE)
    }
    if (any(abs((z$right_timestamp_us - z$left_timestamp_us) - z$isi_us) > 0.5)) {
      stop("Timestamp/ISI inconsistency in sheet '", sheets[[i]], "'.", call. = FALSE)
    }
    if (nrow(z) > 1L && any(z$left_timestamp_us[-1L] !=
                            z$right_timestamp_us[-nrow(z)])) {
      stop("Non-contiguous interval sequence in sheet '", sheets[[i]], "'.",
           call. = FALSE)
    }

    train_ids <- unique(trimws(as.character(z$train_id)))
    train_ids <- train_ids[nzchar(train_ids)]
    if (length(train_ids) != 1L) {
      stop("Sheet '", sheets[[i]], "' must contain exactly one train_id.", call. = FALSE)
    }
    timestamps_us <- c(z$left_timestamp_us[[1]], z$right_timestamp_us)
    if (is.unsorted(timestamps_us, strictly = TRUE)) {
      stop("Non-increasing timestamps in sheet '", sheets[[i]], "'.", call. = FALSE)
    }
    trains[[i]] <- timestamps_us / 1e6
    names(trains)[[i]] <- train_ids[[1]]
    metadata[[i]] <- data.frame(
      sheet = sheets[[i]],
      train_id = train_ids[[1]],
      spike_n = length(timestamps_us),
      isi_n = nrow(z),
      start_seconds = timestamps_us[[1]] / 1e6,
      end_seconds = timestamps_us[[length(timestamps_us)]] / 1e6,
      stringsAsFactors = FALSE
    )
  }

  max_n <- max(lengths(trains))
  wide <- as.data.frame(lapply(trains, function(x) {
    length(x) <- max_n
    x
  }), check.names = FALSE)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(wide, output_path, row.names = FALSE, na = "")
  metadata <- do.call(rbind, metadata)
  metadata$source_workbook_sha256 <- digest::digest(
    workbook_path, algo = "sha256", file = TRUE
  )
  metadata$output_csv_sha256 <- digest::digest(
    output_path, algo = "sha256", file = TRUE
  )
  utils::write.csv(metadata, metadata_path, row.names = FALSE)
  invisible(list(timestamps = wide, metadata = metadata))
}

if (sys.nframe() == 0L) {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(script_arg)) {
    sub("^--file=", "", script_arg[[1]])
  } else {
    file.path(getwd(), "evaluation", "performance", "prepare_public_runtime_input.R")
  }
  repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
  input <- file.path(repo, "data", "real", "STN", "PD_STN_manual_isi_labels.xlsx")
  output <- file.path(
    repo, "data", "derived", "runtime", "PD_STN_public_runtime_timestamps.csv"
  )
  stpd_prepare_public_runtime_input(input, output)
  message("Wrote label-blind runtime input: ", output)
}
