# Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(colourpicker)

PATTERN_LABELS <- c("Burst", "Pause", "Tonic", "high_frequency_tonic", "high_frequency_spiking", "Noisy")
NON_PATTERN_INTERVAL_LABELS <- c("Latency", "Interburst_Gap", "Stimulus_Gap")
INTERVAL_LABELS <- c(PATTERN_LABELS, NON_PATTERN_INTERVAL_LABELS)
SCORABLE_PATTERN_LABELS <- PATTERN_LABELS
SPIKE_PATTERN_LEVELS <- PATTERN_LABELS
NATURE_PATTERN_COLORS <- c(
  "Burst" = "#D55E00",
  "Pause" = "#0072B2",
  "Tonic" = "#009E73",
  "high_frequency_tonic" = "#CC79A7",
  "high_frequency_spiking" = "#332288",
  "Noisy" = "#EBEBEB"
)
DISTRIBUTION_NOISY_LINE_COLOR <- "#6B7280"
DISTRIBUTION_NOISY_FILL_COLOR <- "#9CA3AF"
LATENCY_INTERVAL_COLOR <- "#D9EB12"
NON_PATTERN_INTERVAL_COLORS <- c(
  "Interburst_Gap" = "#B8BCC6",
  "Stimulus_Gap" = "#C7C2E6"
)

SIMULATOR_VERSION <- "V13.5.0"
SIMULATOR_ID <- "Spike_train_simulator_V13_5_0_HF_modes"
SCHEMA_VERSION <- "1.0.0"

# V13.5.0 high-frequency state extension
# - high_frequency_tonic: sustained high-rate regular state constrained by
#   accepted ISI, spike-count, CV, CV2, LV, and maximum/mean (MM) ranges.
# - high_frequency_spiking: sustained state (default >=30 boundary spikes)
#   with a predominant short-ISI component, a limited moderate-ISI bridge
#   component, duration control, and optional within-band trend/serial dependence.
# - Both labels are supported by the Shiny controls, manual hft/hfs sequence
#   tokens, benchmark presets, ground-truth exports, summaries, and scorers.
# These are simulator reference labels; they do not constitute biological
# ground truth outside the explicitly reported generation rules.

config_hash_from_config <- function(config) {
  if (!requireNamespace("digest", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(NA_character_)
  }
  payload <- jsonlite::toJSON(config, auto_unbox = TRUE, null = "null", na = "null", digits = 15)
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

# V13.5.0: detector-visible benchmark inputs are separated from ground-truth
# and audit tables in exported benchmark packages.
# Noisy may contain isolated Burst-like or Tonic-like ISIs, because one isolated
# ISI does not define a Burst/Tonic pattern by itself. It must not, however,
# become a sustained Tonic-like run or drift toward Pause-like long intervals.
# Therefore Noisy is sampled inside a physiological envelope from the absolute
# refractory period to the Tonic upper scale, with a safety margin below the
# Pause lower bound. Pause-like Noisy intervals remain disallowed because Pause
# is itself a single-ISI class.
NOISY_CONTEXT_GUARD_S <- 0.030
NOISY_PAUSE_GUARD_S <- 0.100
NOISY_PAUSE_GUARD_RATIO <- 0.10
NOISY_TONIC_UPPER_MULTIPLIER <- 1.00
NOISY_MIN_MM_RATIO <- 1.50
NOISY_MIN_RUN_CV <- 0.22
NOISY_MIN_RUN_CV2 <- 0.28
NOISY_TONICLIKE_MIN_ISI_COUNT <- 3L
BURST_MIN_BOUNDARY_SPIKES <- 3L
TONIC_MIN_BOUNDARY_SPIKES <- 3L
HF_TONIC_MIN_BOUNDARY_SPIKES <- 6L
HF_SPIKING_MIN_BOUNDARY_SPIKES <- 30L
HF_PATTERN_LABELS <- c("high_frequency_tonic", "high_frequency_spiking")
FEATURE_TUNING_POPULATION_MODES <- c("same_unit_trials", "random_heterogeneous_population", "coverage_balanced_population", "sparse_responsive_population", "one_hot_target")
FEATURE_UNIT_CLASSES <- c("tuned_excitatory", "tuned_suppressive", "tuned_biphasic", "nonresponsive")
VALID_PRE_STIMULUS_STATES <- c("Burst", "Pause", "Tonic", "high_frequency_tonic", "high_frequency_spiking", "Noisy")

make_empty_spike_df <- function() {
  data.frame(
    Episode = integer(0),
    Time = numeric(0),
    Episode_Context = character(0),
    Spike_Role = character(0),
    stringsAsFactors = FALSE
  )
}

make_empty_episode_df <- function() {
  data.frame(
    Episode = integer(0),
    Pattern = character(0),
    Episode_Scope = character(0),
    Latency_Context = character(0),
    Latency_Model = character(0),
    Start = numeric(0),
    End = numeric(0),
    Episode_Duration = numeric(0),
    Core_Start = numeric(0),
    Core_End = numeric(0),
    Core_Duration = numeric(0),
    First_Spike_Time = numeric(0),
    Last_Spike_Time = numeric(0),
    N_Spikes = integer(0),
    N_ISIs = integer(0),
    N_Boundary_Spikes = integer(0),
    N_New_Spikes = integer(0),
    N_Shared_Boundary_Spikes = integer(0),
    Mean_Within_Episode_ISI = numeric(0),
    CV_Within_Episode_ISI = numeric(0),
    Mean_CV2_Within_Episode_ISI = numeric(0),
    LV_Within_Episode_ISI = numeric(0),
    Core_ISI_Rate_Hz = numeric(0),
    Episode_Inclusive_Rate_Hz = numeric(0),
    Stimulus_ID = integer(0),
    Stimulus_Phase = character(0),
    Evoked = logical(0),
    Evoked_Response_Type = character(0),
    Response_Epoch = character(0),
    Stimulus_Onset_s = numeric(0),
    Time_From_Stimulus_Onset_s = numeric(0),
    Contains_Stimulus_Onset = logical(0),
    Event_Epoch_Type = character(0),
    Event_Epoch_Source = character(0),
    Event_Epoch_Generation_Rule = character(0),
    stringsAsFactors = FALSE
  )
}

make_empty_interval_df <- function() {
  data.frame(
    Train = integer(0),
    Interval_ID = integer(0),
    Left_Spike_Index = integer(0),
    Right_Spike_Index = integer(0),
    Left_Spike_Time_s = numeric(0),
    Right_Spike_Time_s = numeric(0),
    Start_Time_s = numeric(0),
    End_Time_s = numeric(0),
    ISI_s = numeric(0),
    Interval = numeric(0),
    ISI_Label = character(0),
    Episode = integer(0),
    ISI_Scope = character(0),
    Left_Spike_Role = character(0),
    Right_Spike_Role = character(0),
    Left_Episode_Context = character(0),
    Right_Episode_Context = character(0),
    Is_Manual_Fixed = logical(0),
    Interval_Source = character(0),
    Run_Position = numeric(0),
    Run_Length = integer(0),
    Temporal_Rho = numeric(0),
    Temporal_Trend = numeric(0),
    Stimulus_ID = integer(0),
    Stimulus_Phase = character(0),
    Evoked = logical(0),
    Evoked_Response_Type = character(0),
    Response_Epoch = character(0),
    Stimulus_Onset_s = numeric(0),
    Time_From_Stimulus_Onset_s = numeric(0),
    Contains_Stimulus_Onset = logical(0),
    stringsAsFactors = FALSE
  )
}

make_empty_stimulus_df <- function() {
  data.frame(
    Train = integer(0),
    Stimulus_ID = integer(0),
    Onset_s = numeric(0),
    Duration_s = numeric(0),
    Strength = numeric(0),
    Protocol = character(0),
    Stimulus_Type = character(0),
    Channel = character(0),
    Repetition_Index = integer(0),
    Inter_Stimulus_Interval_s = numeric(0),
    Pair_ID = integer(0),
    Is_Standard = logical(0),
    Is_Deviant = logical(0),
    Feature_Modality = character(0),
    Stimulus_Feature_Value = numeric(0),
    Stimulus_Position_X = numeric(0),
    Stimulus_Position_Y = numeric(0),
    Preferred_Feature_Value = numeric(0),
    Null_Feature_Value = numeric(0),
    Feature_Distance_To_Preferred = numeric(0),
    Feature_Distance_To_Null = numeric(0),
    Feature_Excitation = numeric(0),
    Feature_Suppression = numeric(0),
    Feature_Selectivity = numeric(0),
    Feature_Response_Class = character(0),
    External_Strength = numeric(0),
    Feature_Drive = numeric(0),
    Feature_Matched = logical(0),
    Drive_Above_Threshold = logical(0),
    Response_Kernel = character(0),
    Response_Eligible = logical(0),
    Feature_Response_Eligible = logical(0),
    Feature_Response_Reason = character(0),
    Unit_ID = integer(0),
    Unit_Tuning_Mode = character(0),
    Unit_Class = character(0),
    Unit_Responsive = logical(0),
    Unit_Preferred_Feature_Value = numeric(0),
    Unit_Null_Feature_Value = numeric(0),
    Unit_Place_Field_Center_X = numeric(0),
    Unit_Place_Field_Center_Y = numeric(0),
    Unit_Place_Field_Width = numeric(0),
    Unit_Place_Field_Radius = numeric(0),
    Place_Field_Distance = numeric(0),
    Place_Field_Drive = numeric(0),
    Unit_Tuning_Width = numeric(0),
    Unit_Suppression_Width = numeric(0),
    Unit_Max_Response_Gain = numeric(0),
    Unit_Response_Threshold = numeric(0),
    Unit_Response_Reliability = numeric(0),
    stringsAsFactors = FALSE
  )
}

make_empty_unit_df <- function() {
  data.frame(
    Train = integer(0),
    Unit_ID = integer(0),
    Unit_Tuning_Mode = character(0),
    Unit_Class = character(0),
    Unit_Responsive = logical(0),
    Feature_Modality = character(0),
    Preferred_Feature_Value = numeric(0),
    Null_Feature_Value = numeric(0),
    Place_Field_Center_X = numeric(0),
    Place_Field_Center_Y = numeric(0),
    Place_Field_Width = numeric(0),
    Place_Field_Radius = numeric(0),
    Environment_X_Min = numeric(0),
    Environment_X_Max = numeric(0),
    Environment_Y_Min = numeric(0),
    Environment_Y_Max = numeric(0),
    Feature_Period = numeric(0),
    Feature_Tuning_Width = numeric(0),
    Feature_Suppression_Width = numeric(0),
    Feature_Response_Threshold = numeric(0),
    Feature_Max_Response_Gain = numeric(0),
    Feature_Response_Reliability = numeric(0),
    Preferred_Response_Type = character(0),
    Null_Response_Type = character(0),
    Population_Mode = character(0),
    Profile_Source = character(0),
    stringsAsFactors = FALSE
  )
}


make_empty_unit_stimulus_drive_df <- function() {
  data.frame(
    Train = integer(0),
    Unit_ID = integer(0),
    Stimulus_ID = integer(0),
    Onset_s = numeric(0),
    Duration_s = numeric(0),
    Stimulus_Type = character(0),
    Protocol = character(0),
    Channel = character(0),
    Repetition_Index = integer(0),
    External_Strength = numeric(0),
    External_Strength_Source = character(0),
    Unit_Modulated_Strength = numeric(0),
    Feature_Modality = character(0),
    Stimulus_Feature_Value = numeric(0),
    Stimulus_Position_X = numeric(0),
    Stimulus_Position_Y = numeric(0),
    Unit_Class = character(0),
    Unit_Responsive = logical(0),
    Unit_Tuning_Mode = character(0),
    Preferred_Feature_Value = numeric(0),
    Null_Feature_Value = numeric(0),
    Place_Field_Center_X = numeric(0),
    Place_Field_Center_Y = numeric(0),
    Place_Field_Width = numeric(0),
    Place_Field_Radius = numeric(0),
    Place_Field_Distance = numeric(0),
    Place_Field_Drive = numeric(0),
    Feature_Distance_To_Preferred = numeric(0),
    Feature_Distance_To_Null = numeric(0),
    Feature_Excitation = numeric(0),
    Feature_Suppression = numeric(0),
    Feature_Selectivity = numeric(0),
    Feature_Drive = numeric(0),
    Feature_Matched = logical(0),
    Drive_Above_Threshold = logical(0),
    Response_Kernel = character(0),
    Response_Eligible = logical(0),
    Feature_Response_Eligible = logical(0),
    Feature_Response_Reason = character(0),
    Feature_Response_Class = character(0),
    Unit_Response_Threshold = numeric(0),
    Unit_Response_Reliability = numeric(0),
    Unit_Max_Response_Gain = numeric(0),
    Response_Type = character(0),
    Response_Generated_OK = logical(0),
    Response_Plan_Feasible = logical(0),
    Response_Plan_Start_s = numeric(0),
    Response_Plan_End_s = numeric(0),
    Response_Plan_Min_Duration_s = numeric(0),
    Response_Plan_Required_Components = character(0),
    Response_Plan_Failure_Reason = character(0),
    Response_Rolled_Back = logical(0),
    Response_Commit_OK = logical(0),
    Evoked_Burst_Count = integer(0),
    Evoked_Spike_Count = integer(0),
    Evoked_Suppression_Duration_s = numeric(0),
    Scorable_Evoked_Pause_Duration_s = numeric(0),
    Evoked_Pause_Duration_s = numeric(0),
    Response_Probability = numeric(0),
    Response_Attempted = logical(0),
    Response_Failure_Reason = character(0),
    Response_Failure_Class = character(0),
    Response_Gain = numeric(0),
    stringsAsFactors = FALSE
  )
}

build_unit_stimulus_drive_table <- function(stimuli, responses = NULL) {
  if (is.null(stimuli) || nrow(stimuli) == 0) return(make_empty_unit_stimulus_drive_df())
  x <- stimuli
  get_col <- function(nm, default = NA) {
    if (nm %in% names(x)) x[[nm]] else rep(default, nrow(x))
  }
  if ("External_Strength" %in% names(x)) {
    external_strength <- suppressWarnings(as.numeric(x$External_Strength))
    external_strength_source <- rep("explicit_external_strength", nrow(x))
    external_strength_source[!is.finite(external_strength)] <- "explicit_external_strength_missing"
  } else if ("Strength" %in% names(x)) {
    external_strength <- suppressWarnings(as.numeric(x$Strength))
    external_strength_source <- rep("fallback_strength_legacy", nrow(x))
    external_strength_source[!is.finite(external_strength)] <- "missing"
  } else {
    external_strength <- rep(NA_real_, nrow(x))
    external_strength_source <- rep("missing", nrow(x))
  }
  out <- data.frame(
    Train = suppressWarnings(as.integer(get_col("Train", NA_integer_))),
    Unit_ID = suppressWarnings(as.integer(get_col("Unit_ID", NA_integer_))),
    Stimulus_ID = suppressWarnings(as.integer(get_col("Stimulus_ID", NA_integer_))),
    Onset_s = suppressWarnings(as.numeric(get_col("Onset_s", NA_real_))),
    Duration_s = suppressWarnings(as.numeric(get_col("Duration_s", NA_real_))),
    Stimulus_Type = as.character(get_col("Stimulus_Type", NA_character_)),
    Protocol = as.character(get_col("Protocol", NA_character_)),
    Channel = as.character(get_col("Channel", NA_character_)),
    Repetition_Index = suppressWarnings(as.integer(get_col("Repetition_Index", NA_integer_))),
    External_Strength = external_strength,
    External_Strength_Source = external_strength_source,
    Unit_Modulated_Strength = suppressWarnings(as.numeric(get_col("Strength", NA_real_))),
    Feature_Modality = as.character(get_col("Feature_Modality", NA_character_)),
    Stimulus_Feature_Value = suppressWarnings(as.numeric(get_col("Stimulus_Feature_Value", NA_real_))),
    Stimulus_Position_X = suppressWarnings(as.numeric(get_col("Stimulus_Position_X", NA_real_))),
    Stimulus_Position_Y = suppressWarnings(as.numeric(get_col("Stimulus_Position_Y", NA_real_))),
    Unit_Class = as.character(get_col("Unit_Class", NA_character_)),
    Unit_Responsive = as.logical(get_col("Unit_Responsive", NA)),
    Unit_Tuning_Mode = as.character(get_col("Unit_Tuning_Mode", NA_character_)),
    Preferred_Feature_Value = suppressWarnings(as.numeric(get_col("Unit_Preferred_Feature_Value", get_col("Preferred_Feature_Value", NA_real_)))),
    Null_Feature_Value = suppressWarnings(as.numeric(get_col("Unit_Null_Feature_Value", get_col("Null_Feature_Value", NA_real_)))),
    Place_Field_Center_X = suppressWarnings(as.numeric(get_col("Unit_Place_Field_Center_X", NA_real_))),
    Place_Field_Center_Y = suppressWarnings(as.numeric(get_col("Unit_Place_Field_Center_Y", NA_real_))),
    Place_Field_Width = suppressWarnings(as.numeric(get_col("Unit_Place_Field_Width", NA_real_))),
    Place_Field_Radius = suppressWarnings(as.numeric(get_col("Unit_Place_Field_Radius", NA_real_))),
    Place_Field_Distance = suppressWarnings(as.numeric(get_col("Place_Field_Distance", NA_real_))),
    Place_Field_Drive = suppressWarnings(as.numeric(get_col("Place_Field_Drive", NA_real_))),
    Feature_Distance_To_Preferred = suppressWarnings(as.numeric(get_col("Feature_Distance_To_Preferred", NA_real_))),
    Feature_Distance_To_Null = suppressWarnings(as.numeric(get_col("Feature_Distance_To_Null", NA_real_))),
    Feature_Excitation = suppressWarnings(as.numeric(get_col("Feature_Excitation", NA_real_))),
    Feature_Suppression = suppressWarnings(as.numeric(get_col("Feature_Suppression", NA_real_))),
    Feature_Selectivity = suppressWarnings(as.numeric(get_col("Feature_Selectivity", NA_real_))),
    Feature_Drive = suppressWarnings(as.numeric(get_col("Feature_Drive", NA_real_))),
    Feature_Matched = as.logical(get_col("Feature_Matched", FALSE)),
    Drive_Above_Threshold = as.logical(get_col("Drive_Above_Threshold", FALSE)),
    Response_Kernel = as.character(get_col("Response_Kernel", NA_character_)),
    Response_Eligible = as.logical(get_col("Response_Eligible", get_col("Feature_Response_Eligible", FALSE))),
    Feature_Response_Eligible = as.logical(get_col("Feature_Response_Eligible", get_col("Response_Eligible", FALSE))),
    Feature_Response_Reason = as.character(get_col("Feature_Response_Reason", NA_character_)),
    Feature_Response_Class = as.character(get_col("Feature_Response_Class", NA_character_)),
    Unit_Response_Threshold = suppressWarnings(as.numeric(get_col("Unit_Response_Threshold", NA_real_))),
    Unit_Response_Reliability = suppressWarnings(as.numeric(get_col("Unit_Response_Reliability", NA_real_))),
    Unit_Max_Response_Gain = suppressWarnings(as.numeric(get_col("Unit_Max_Response_Gain", NA_real_))),
    stringsAsFactors = FALSE
  )
  out$Response_Type <- NA_character_
  out$Response_Generated_OK <- NA
  out$Response_Plan_Feasible <- NA
  out$Response_Plan_Start_s <- NA_real_
  out$Response_Plan_End_s <- NA_real_
  out$Response_Plan_Min_Duration_s <- NA_real_
  out$Response_Plan_Required_Components <- NA_character_
  out$Response_Plan_Failure_Reason <- NA_character_
  out$Response_Rolled_Back <- NA
  out$Response_Commit_OK <- NA
  out$Evoked_Burst_Count <- NA_integer_
  out$Evoked_Spike_Count <- NA_integer_
  out$Evoked_Suppression_Duration_s <- NA_real_
  out$Scorable_Evoked_Pause_Duration_s <- NA_real_
  out$Evoked_Pause_Duration_s <- NA_real_
  out$Response_Probability <- NA_real_
  out$Response_Attempted <- NA
  out$Response_Failure_Reason <- NA_character_
  out$Response_Failure_Class <- NA_character_
  out$Response_Gain <- NA_real_
  if (!is.null(responses) && nrow(responses) > 0) {
    keep <- c(
      "Train", "Stimulus_ID", "Response_Type", "Response_Generated_OK",
      "Response_Plan_Feasible", "Response_Plan_Start_s", "Response_Plan_End_s",
      "Response_Plan_Min_Duration_s", "Response_Plan_Required_Components",
      "Response_Plan_Failure_Reason", "Response_Rolled_Back", "Response_Commit_OK",
      "Evoked_Burst_Count", "Evoked_Spike_Count", "Evoked_Suppression_Duration_s",
      "Scorable_Evoked_Pause_Duration_s", "Evoked_Pause_Duration_s",
      "Response_Probability", "Response_Attempted", "Response_Failure_Reason", "Response_Failure_Class", "Response_Gain"
    )
    keep <- intersect(keep, names(responses))
    if (all(c("Train", "Stimulus_ID") %in% keep)) {
      rr <- responses[, keep, drop = FALSE]
      merged <- merge(out, rr, by = c("Train", "Stimulus_ID"), all.x = TRUE, suffixes = c("", ".response"))
      for (nm in c(
        "Response_Type", "Response_Generated_OK",
        "Response_Plan_Feasible", "Response_Plan_Start_s", "Response_Plan_End_s",
        "Response_Plan_Min_Duration_s", "Response_Plan_Required_Components",
        "Response_Plan_Failure_Reason", "Response_Rolled_Back", "Response_Commit_OK",
        "Evoked_Burst_Count", "Evoked_Spike_Count", "Evoked_Suppression_Duration_s",
        "Scorable_Evoked_Pause_Duration_s", "Evoked_Pause_Duration_s",
        "Response_Probability", "Response_Attempted", "Response_Failure_Reason", "Response_Failure_Class", "Response_Gain"
      )) {
        rnm <- paste0(nm, ".response")
        if (rnm %in% names(merged)) {
          merged[[nm]] <- ifelse(is.na(merged[[rnm]]), merged[[nm]], merged[[rnm]])
          merged[[rnm]] <- NULL
        }
      }
      out <- merged[order(merged$Train, merged$Stimulus_ID), names(make_empty_unit_stimulus_drive_df()), drop = FALSE]
      rownames(out) <- NULL
      return(out)
    }
  }
  out <- out[, names(make_empty_unit_stimulus_drive_df()), drop = FALSE]
  rownames(out) <- NULL
  out
}

validate_unit_stimulus_drive_table <- function(unit_drive) {
  if (is.null(unit_drive) || nrow(unit_drive) == 0) {
    return(data.frame(
      Validation = "unit_stimulus_drive_table",
      N_Rows = 0L,
      N_Units = 0L,
      N_Stimuli = 0L,
      Responsive_Fraction = NA_real_,
      Eligible_Fraction = NA_real_,
      Generated_Response_Fraction = NA_real_,
      Drive_Distance_Correlation = NA_real_,
      Drive_Distance_Correlation_Global = NA_real_,
      Drive_Distance_Correlation_Median_WithinUnit = NA_real_,
      Drive_Distance_Correlation_Fraction_Negative = NA_real_,
      Reason_Consistency_OK = TRUE,
      Reason_Inconsistency_Count = 0L,
      Pass = TRUE,
      Diagnostic = "empty_or_stimulation_disabled",
      stringsAsFactors = FALSE
    ))
  }
  n_units <- length(unique(unit_drive$Unit_ID[is.finite(unit_drive$Unit_ID)]))
  n_stim <- length(unique(unit_drive$Stimulus_ID[is.finite(unit_drive$Stimulus_ID)]))
  responsive_fraction <- mean(isTRUE(unit_drive$Unit_Responsive) | unit_drive$Unit_Responsive %in% TRUE, na.rm = TRUE)
  eligible_fraction <- mean(unit_drive$Feature_Response_Eligible %in% TRUE, na.rm = TRUE)
  generated_fraction <- mean(unit_drive$Response_Generated_OK %in% TRUE, na.rm = TRUE)

  corr_global <- NA_real_
  corr_within_median <- NA_real_
  corr_within_frac_negative <- NA_real_
  if (all(c("Place_Field_Distance", "Place_Field_Drive") %in% names(unit_drive))) {
    ok_global <- is.finite(unit_drive$Place_Field_Distance) & is.finite(unit_drive$Place_Field_Drive)
    if (sum(ok_global) >= 3 && stats::sd(unit_drive$Place_Field_Distance[ok_global]) > 0 && stats::sd(unit_drive$Place_Field_Drive[ok_global]) > 0) {
      corr_global <- suppressWarnings(stats::cor(unit_drive$Place_Field_Distance[ok_global], unit_drive$Place_Field_Drive[ok_global], method = "spearman"))
    }
    train_unit_key <- if (all(c("Train", "Unit_ID") %in% names(unit_drive))) {
      paste(unit_drive$Train, unit_drive$Unit_ID, sep = "__")
    } else if ("Unit_ID" %in% names(unit_drive)) {
      as.character(unit_drive$Unit_ID)
    } else {
      rep("unit_1", nrow(unit_drive))
    }
    corrs <- unlist(lapply(split(unit_drive, train_unit_key), function(x) {
      ok <- is.finite(x$Place_Field_Distance) & is.finite(x$Place_Field_Drive)
      if (sum(ok) < 3) return(NA_real_)
      if (stats::sd(x$Place_Field_Distance[ok]) <= 0 || stats::sd(x$Place_Field_Drive[ok]) <= 0) return(NA_real_)
      suppressWarnings(stats::cor(x$Place_Field_Distance[ok], x$Place_Field_Drive[ok], method = "spearman"))
    }), use.names = FALSE)
    corrs <- corrs[is.finite(corrs)]
    if (length(corrs) > 0) {
      corr_within_median <- stats::median(corrs)
      corr_within_frac_negative <- mean(corrs < 0)
    }
  }
  corr <- if (is.finite(corr_within_median)) corr_within_median else corr_global

  reason_ok <- TRUE
  reason_bad <- 0L
  if (all(c("Feature_Response_Reason", "Feature_Response_Class", "Unit_Responsive") %in% names(unit_drive))) {
    reason <- as.character(unit_drive$Feature_Response_Reason)
    response_eligible <- if ("Response_Eligible" %in% names(unit_drive)) unit_drive$Response_Eligible %in% TRUE else unit_drive$Feature_Response_Eligible %in% TRUE
    feature_matched <- if ("Feature_Matched" %in% names(unit_drive)) unit_drive$Feature_Matched %in% TRUE else response_eligible
    drive_above <- if ("Drive_Above_Threshold" %in% names(unit_drive)) unit_drive$Drive_Above_Threshold %in% TRUE else response_eligible
    kernel <- if ("Response_Kernel" %in% names(unit_drive)) as.character(unit_drive$Response_Kernel) else rep(NA_character_, nrow(unit_drive))
    responsive <- unit_drive$Unit_Responsive %in% TRUE
    cls <- as.character(unit_drive$Feature_Response_Class)
    radius <- if ("Unit_Place_Field_Radius" %in% names(unit_drive)) {
      suppressWarnings(as.numeric(unit_drive$Unit_Place_Field_Radius))
    } else if ("Place_Field_Radius" %in% names(unit_drive)) {
      suppressWarnings(as.numeric(unit_drive$Place_Field_Radius))
    } else {
      rep(NA_real_, nrow(unit_drive))
    }
    distance <- if ("Place_Field_Distance" %in% names(unit_drive)) suppressWarnings(as.numeric(unit_drive$Place_Field_Distance)) else rep(NA_real_, nrow(unit_drive))
    place_rows <- is.finite(distance) & is.finite(radius)
    place_responsive_classes <- c("preferred_excitatory", "preferred_suppressive", "preferred_biphasic")
    preferred_classes <- c("preferred_excitatory", "preferred_suppressive", "preferred_biphasic")
    responsive_classes <- c(preferred_classes, "null_suppressive")
    bad <- rep(FALSE, nrow(unit_drive))
    bad <- bad | (!responsive & reason != "unit_nonresponsive")
    bad <- bad | (place_rows & responsive & distance > radius & reason != "outside_place_field_radius")
    bad <- bad | (place_rows & responsive & distance <= radius & !drive_above & reason != "subthreshold_place_field_drive")
    bad <- bad | (place_rows & responsive & distance <= radius & drive_above & cls %in% place_responsive_classes & kernel == "no_response" & reason != "place_field_no_response_kernel")
    bad <- bad | (place_rows & response_eligible & reason != "place_field_matched")
    bad <- bad | (place_rows & responsive & distance <= radius & drive_above & !(cls %in% place_responsive_classes) & reason != "inside_place_field_neutral")
    bad <- bad | (!place_rows & responsive & !(cls %in% responsive_classes) & reason != "feature_mismatch_neutral")
    bad <- bad | (!place_rows & feature_matched & !drive_above & reason != "subthreshold_feature_drive")
    bad <- bad | (!place_rows & feature_matched & drive_above & cls == "null_suppressive" & kernel == "no_response" & reason != "null_feature_no_response_kernel")
    bad <- bad | (!place_rows & feature_matched & drive_above & cls != "null_suppressive" & kernel == "no_response" & reason != "preferred_feature_no_response_kernel")
    bad <- bad | (!place_rows & response_eligible & cls %in% preferred_classes & reason != "preferred_feature_matched")
    bad <- bad | (!place_rows & response_eligible & cls == "null_suppressive" & reason != "null_feature_matched")
    reason_bad <- sum(bad, na.rm = TRUE)
    reason_ok <- reason_bad == 0L
  }
  pass <- TRUE
  diag <- "ok"
  if (is.finite(corr) && corr > 0.05) { pass <- FALSE; diag <- "place_field_drive_not_decreasing_with_distance" }
  if (!isTRUE(reason_ok)) { pass <- FALSE; diag <- paste(c(diag[diag != "ok"], "feature_response_reason_inconsistent"), collapse = ";") }
  data.frame(
    Validation = "unit_stimulus_drive_table",
    N_Rows = nrow(unit_drive),
    N_Units = n_units,
    N_Stimuli = n_stim,
    Responsive_Fraction = responsive_fraction,
    Eligible_Fraction = eligible_fraction,
    Generated_Response_Fraction = generated_fraction,
    Drive_Distance_Correlation = corr,
    Drive_Distance_Correlation_Global = corr_global,
    Drive_Distance_Correlation_Median_WithinUnit = corr_within_median,
    Drive_Distance_Correlation_Fraction_Negative = corr_within_frac_negative,
    Reason_Consistency_OK = reason_ok,
    Reason_Inconsistency_Count = reason_bad,
    Pass = pass,
    Diagnostic = diag,
    stringsAsFactors = FALSE
  )
}

make_empty_response_df <- function() {
  data.frame(
    Train = integer(0),
    Stimulus_ID = integer(0),
    Response_Type = character(0),
    Response_Latency_s = numeric(0),
    Response_Window_Start_s = numeric(0),
    Response_Window_End_s = numeric(0),
    Generated_Response_Start_s = numeric(0),
    Generated_Response_End_s = numeric(0),
    Expected_Response_Window_s = numeric(0),
    Response_Plan_Feasible = logical(0),
    Response_Plan_Start_s = numeric(0),
    Response_Plan_End_s = numeric(0),
    Response_Plan_Min_Duration_s = numeric(0),
    Response_Plan_Required_Components = character(0),
    Response_Plan_Failure_Reason = character(0),
    Response_Rolled_Back = logical(0),
    Response_Commit_OK = logical(0),
    Evoked_Burst_Count = integer(0),
    Evoked_Spike_Count = integer(0),
    Evoked_Suppression_Duration_s = numeric(0),
    Scorable_Evoked_Pause_Duration_s = numeric(0),
    Evoked_Pause_Duration_s = numeric(0),
    Rebound_Burst = logical(0),
    Adaptation_State_Pre = numeric(0),
    Adaptation_State_Post = numeric(0),
    Adaptation_Source = character(0),
    Stimulus_Adaptation_Load = numeric(0),
    Response_Adaptation_Load = numeric(0),
    Response_Gain = numeric(0),
    Response_Probability = numeric(0),
    Response_Attempted = logical(0),
    Response_Load = numeric(0),
    Response_Generated_OK = logical(0),
    Response_Truncated = logical(0),
    Response_Failure_Reason = character(0),
    Response_Failure_Class = character(0),
    Window_Limited = logical(0),
    Suppression_Index = numeric(0),
    Pre_Stimulus_State = character(0),
    Pre_Stimulus_Label = character(0),
    Feature_Modality = character(0),
    Stimulus_Feature_Value = numeric(0),
    Stimulus_Position_X = numeric(0),
    Stimulus_Position_Y = numeric(0),
    Preferred_Feature_Value = numeric(0),
    Null_Feature_Value = numeric(0),
    Feature_Distance_To_Preferred = numeric(0),
    Feature_Distance_To_Null = numeric(0),
    Feature_Excitation = numeric(0),
    Feature_Suppression = numeric(0),
    Feature_Selectivity = numeric(0),
    Feature_Response_Class = character(0),
    External_Strength = numeric(0),
    Feature_Drive = numeric(0),
    Feature_Matched = logical(0),
    Drive_Above_Threshold = logical(0),
    Response_Kernel = character(0),
    Response_Eligible = logical(0),
    Feature_Response_Eligible = logical(0),
    Feature_Response_Reason = character(0),
    Unit_ID = integer(0),
    Unit_Tuning_Mode = character(0),
    Unit_Class = character(0),
    Unit_Responsive = logical(0),
    Unit_Preferred_Feature_Value = numeric(0),
    Unit_Null_Feature_Value = numeric(0),
    Unit_Place_Field_Center_X = numeric(0),
    Unit_Place_Field_Center_Y = numeric(0),
    Unit_Place_Field_Width = numeric(0),
    Unit_Place_Field_Radius = numeric(0),
    Place_Field_Distance = numeric(0),
    Place_Field_Drive = numeric(0),
    Unit_Tuning_Width = numeric(0),
    Unit_Suppression_Width = numeric(0),
    Unit_Max_Response_Gain = numeric(0),
    Unit_Response_Threshold = numeric(0),
    Unit_Response_Reliability = numeric(0),
    stringsAsFactors = FALSE
  )
}

classify_response_failure <- function(reason,
                                      generated_ok = FALSE,
                                      plan_feasible = NA,
                                      rolled_back = FALSE,
                                      window_limited = FALSE) {
  reason <- paste(as.character(reason), collapse = ";")
  reason_lower <- tolower(reason)
  if (!nzchar(reason_lower) || identical(reason_lower, "none")) {
    return(if (isTRUE(generated_ok)) "none" else "other_no_response")
  }
  if (grepl("feature_kernel_no_response", reason_lower)) return("other_no_response")
  if (grepl("optional_", reason_lower)) return("optional_component_failed")
  if (grepl("probabilistic_response_failure", reason_lower)) return("probabilistic_no_response")
  if (isFALSE(plan_feasible) ||
      grepl("response_plan_infeasible|response_window_too_short_for_minimal_plan", reason_lower)) {
    return("preflight_infeasible")
  }
  if (isTRUE(rolled_back) || grepl("response_commit_rolled_back", reason_lower)) return("commit_failed")
  if (grepl("refractory", reason_lower)) return("refractory_limited")
  if (isTRUE(window_limited) || grepl("window_too_short|window_limited|latency", reason_lower)) {
    return("window_limited")
  }
  if (isTRUE(generated_ok)) return("none")
  "other_no_response"
}

make_empty_event_epoch_df <- function() {
	  data.frame(
	    Train = integer(0),
	    Event_Epoch_ID = integer(0),
	    Stimulus_ID = integer(0),
	    Epoch_Type = character(0),
	    Epoch_Class = character(0),
	    Response_Component = character(0),
	    Epoch_Source = character(0),
	    Epoch_Generation_Rule = character(0),
	    Start_s = numeric(0),
	    End_s = numeric(0),
	    Duration_s = numeric(0),
	    Interval_ID_Start = integer(0),
	    Interval_ID_End = integer(0),
	    N_Intervals = integer(0),
	    N_Boundary_Spikes = integer(0),
	    Left_Spike_ID = integer(0),
	    Right_Spike_ID = integer(0),
	    Bounded_By_Spikes = logical(0),
	    Scorable = logical(0),
	    ISI_Label = character(0),
	    Response_Type = character(0),
	    Stimulus_Onset_s = numeric(0),
	    Time_From_Stimulus_Onset_s = numeric(0),
	    Source = character(0),
	    stringsAsFactors = FALSE
	  )
	}

	event_epochs_from_intervals <- function(intervals) {
	  if (is.null(intervals) || nrow(intervals) == 0) return(make_empty_event_epoch_df())
	  train_col <- if ("Train" %in% names(intervals)) suppressWarnings(as.integer(intervals$Train)) else rep(1L, nrow(intervals))
	  interval_id_col <- if ("Interval_ID" %in% names(intervals)) suppressWarnings(as.integer(intervals$Interval_ID)) else seq_len(nrow(intervals))
	  x <- intervals[order(train_col, interval_id_col), , drop = FALSE]
	  response_epoch <- as.character(value_or(x$Response_Epoch, rep(NA_character_, nrow(x))))
	  response_epoch[is.na(response_epoch)] <- ""
	  scope <- as.character(value_or(x$ISI_Scope, rep(NA_character_, nrow(x))))
	  scope[is.na(scope)] <- ""
	  event_scopes <- c(
	    "stimulus_latency", "stimulus_spanning_gap", "interburst_gap",
	    "evoked_suppression", "post_burst_pause", "evoked_pause",
	    "no_response_baseline", "failed_response_baseline",
	    "baseline_recovery", "post_stimulus_recovery",
	    "uploaded_event_spanning_interval"
	  )
	  burst_epoch <- grepl("^(evoked_burst|early_burst|rebound_burst)_[0-9]+$", response_epoch)
	  is_event <- as.character(x$ISI_Label) %in% NON_PATTERN_INTERVAL_LABELS |
	    scope %in% event_scopes |
	    (nzchar(response_epoch) & response_epoch %in% event_scopes) |
	    burst_epoch
	  x <- x[is_event, , drop = FALSE]
	  if (nrow(x) == 0) return(make_empty_event_epoch_df())
	  response_epoch <- as.character(value_or(x$Response_Epoch, rep(NA_character_, nrow(x))))
	  response_epoch[is.na(response_epoch)] <- ""
	  scope <- as.character(value_or(x$ISI_Scope, rep(NA_character_, nrow(x))))
	  scope[is.na(scope)] <- ""
	  raw_component <- response_epoch
	  raw_component[!nzchar(raw_component)] <- scope[!nzchar(raw_component)]
	  raw_component[!nzchar(raw_component)] <- as.character(x$ISI_Label[!nzchar(raw_component)])
	  raw_component[is.na(raw_component)] <- ""
	  normalize_epoch_type <- function(component) {
	    if (grepl("^(evoked_burst|early_burst)_[0-9]+$", component)) return("evoked_burst_epoch")
	    if (grepl("^rebound_burst_[0-9]+$", component)) return("rebound_burst_epoch")
	    if (component %in% c("evoked_suppression", "post_burst_pause", "evoked_pause")) return("suppression_epoch")
	    if (component %in% c("baseline_recovery", "post_stimulus_recovery")) return("recovery_epoch")
	    if (component %in% c("no_response_baseline", "failed_response_baseline")) return("response_failure_baseline_epoch")
	    if (component %in% c("stimulus_latency", "response_latency", "Latency")) return("response_latency_epoch")
	    if (component %in% c("interburst_gap", "Interburst_Gap")) return("interburst_gap_epoch")
	    if (component %in% c("stimulus_spanning_gap", "Stimulus_Gap")) return("stimulus_spanning_gap_epoch")
	    component
	  }
	  inferred_epoch_type <- vapply(raw_component, normalize_epoch_type, character(1))
	  explicit_epoch_type <- as.character(value_or(x$Event_Epoch_Type, rep(NA_character_, nrow(x))))
	  explicit_epoch_type[is.na(explicit_epoch_type)] <- ""
	  epoch_type <- ifelse(nzchar(explicit_epoch_type), explicit_epoch_type, inferred_epoch_type)
	  epoch_class <- ifelse(epoch_type %in% c("evoked_burst_epoch", "rebound_burst_epoch"), "evoked_spiking",
	                        ifelse(epoch_type %in% c("suppression_epoch"), "suppression",
	                               ifelse(epoch_type %in% c("recovery_epoch"), "recovery",
	                                      ifelse(epoch_type %in% c("response_latency_epoch", "interburst_gap_epoch", "stimulus_spanning_gap_epoch"), "timing",
	                                             ifelse(epoch_type %in% c("response_failure_baseline_epoch"), "response_failure", "event_epoch")))))
	  train <- if ("Train" %in% names(x)) suppressWarnings(as.integer(x$Train)) else rep(NA_integer_, nrow(x))
	  stim_id <- if ("Stimulus_ID" %in% names(x)) suppressWarnings(as.integer(x$Stimulus_ID)) else rep(NA_integer_, nrow(x))
	  resp_type <- as.character(value_or(x$Evoked_Response_Type, rep(NA_character_, nrow(x))))
	  resp_type[is.na(resp_type)] <- ""
	  source <- as.character(value_or(x$Interval_Source, rep(NA_character_, nrow(x))))
	  source[is.na(source)] <- ""
	  epoch_source <- as.character(value_or(x$Event_Epoch_Source, rep(NA_character_, nrow(x))))
	  epoch_source[is.na(epoch_source) | !nzchar(epoch_source)] <- source[is.na(epoch_source) | !nzchar(epoch_source)]
	  epoch_source[is.na(epoch_source)] <- ""
	  epoch_rule <- as.character(value_or(x$Event_Epoch_Generation_Rule, rep(NA_character_, nrow(x))))
	  epoch_rule[is.na(epoch_rule)] <- ""
	  interval_id <- if ("Interval_ID" %in% names(x)) suppressWarnings(as.integer(x$Interval_ID)) else seq_len(nrow(x))
	  start_time <- as.numeric(x$Start_Time_s)
	  end_time <- as.numeric(x$End_Time_s)
	  key <- paste(ifelse(is.na(train), "", train),
	               ifelse(is.na(stim_id), "", stim_id),
	               epoch_type,
	               raw_component,
	               resp_type,
	               source,
	               sep = "\r")
	  breaks <- rep(TRUE, nrow(x))
	  if (nrow(x) > 1L) {
	    for (i in 2:nrow(x)) {
	      contiguous_id <- is.finite(interval_id[i]) && is.finite(interval_id[i - 1L]) &&
	        interval_id[i] == interval_id[i - 1L] + 1L
	      contiguous_time <- is.finite(start_time[i]) && is.finite(end_time[i - 1L]) &&
	        abs(start_time[i] - end_time[i - 1L]) <= 1e-8
	      breaks[i] <- !identical(key[i], key[i - 1L]) || !isTRUE(contiguous_id) || !isTRUE(contiguous_time)
	    }
	  }
	  group_id <- cumsum(breaks)
	  rows <- lapply(split(seq_len(nrow(x)), group_id), function(idx) {
	    left_ids <- suppressWarnings(as.integer(x$Left_Spike_Index[idx]))
	    right_ids <- suppressWarnings(as.integer(x$Right_Spike_Index[idx]))
	    boundary_ids <- unique(c(left_ids[is.finite(left_ids)], right_ids[is.finite(right_ids)]))
	    labs <- unique(as.character(x$ISI_Label[idx]))
	    labs <- labs[!is.na(labs) & nzchar(labs)]
	    onset_vals <- as.numeric(value_or(x$Stimulus_Onset_s[idx], rep(NA_real_, length(idx))))
	    stim_onset <- if (any(is.finite(onset_vals))) onset_vals[which(is.finite(onset_vals))[1]] else NA_real_
	    first <- idx[1]
	    last <- idx[length(idx)]
	    data.frame(
	      Train = train[first],
	      Event_Epoch_ID = NA_integer_,
	      Stimulus_ID = stim_id[first],
	      Epoch_Type = epoch_type[first],
	      Epoch_Class = epoch_class[first],
	      Response_Component = raw_component[first],
	      Epoch_Source = epoch_source[first],
	      Epoch_Generation_Rule = epoch_rule[first],
	      Start_s = start_time[first],
	      End_s = end_time[last],
	      Duration_s = end_time[last] - start_time[first],
	      Interval_ID_Start = interval_id[first],
	      Interval_ID_End = interval_id[last],
	      N_Intervals = length(idx),
	      N_Boundary_Spikes = length(boundary_ids),
	      Left_Spike_ID = suppressWarnings(as.integer(x$Left_Spike_Index[first])),
	      Right_Spike_ID = suppressWarnings(as.integer(x$Right_Spike_Index[last])),
	      Bounded_By_Spikes = all(is.finite(as.numeric(x$Left_Spike_Time_s[idx]))) &&
	        all(is.finite(as.numeric(x$Right_Spike_Time_s[idx]))),
	      Scorable = all(as.character(x$ISI_Label[idx]) %in% SCORABLE_PATTERN_LABELS),
	      ISI_Label = if (length(labs) == 1L) labs else paste(labs, collapse = "|"),
	      Response_Type = resp_type[first],
	      Stimulus_Onset_s = stim_onset,
	      Time_From_Stimulus_Onset_s = if (is.finite(stim_onset)) end_time[last] - stim_onset else NA_real_,
	      Source = source[first],
	      stringsAsFactors = FALSE
	    )
	  })
	  out <- do.call(rbind, rows)
	  out$Event_Epoch_ID <- seq_len(nrow(out))
	  out$Stimulus_ID[is.na(out$Stimulus_ID)] <- NA_integer_
	  rownames(out) <- NULL
	  out[, names(make_empty_event_epoch_df()), drop = FALSE]
	}

isi_regularity_metrics <- function(intervals) {
  intervals <- as.numeric(intervals)
  intervals <- intervals[is.finite(intervals) & intervals > 0]
  n <- length(intervals)
  mean_isi <- if (n > 0) mean(intervals) else NA_real_
  cv <- if (n > 1 && is.finite(mean_isi) && mean_isi > 0) sd(intervals) / mean_isi else NA_real_
  cv2 <- NA_real_
  lv <- NA_real_
  mm <- if (n > 0 && is.finite(mean_isi) && mean_isi > 0) max(intervals) / mean_isi else NA_real_

  if (n > 1) {
    prev <- intervals[-n]
    next_isi <- intervals[-1]
    denom <- prev + next_isi
    valid <- is.finite(denom) & denom > 0
    if (any(valid)) {
      delta <- next_isi[valid] - prev[valid]
      denom <- denom[valid]
      cv2 <- mean(2 * abs(delta) / denom)
      lv <- mean(3 * (delta / denom)^2)
    }
  }

  list(mean = mean_isi, cv = cv, cv2 = cv2, lv = lv, mm = mm)
}

normalize_pattern_ratios <- function(ratios) {
  ratios <- ratios[SPIKE_PATTERN_LEVELS]
  ratios[is.na(ratios)] <- 0
  if (sum(ratios) <= 0) {
    ratios <- rep(1, length(SPIKE_PATTERN_LEVELS))
    names(ratios) <- SPIKE_PATTERN_LEVELS
  }
  ratios / sum(ratios)
}

parse_pattern_sequence_strict <- function(seq_str) {
  if (is.null(seq_str) || length(seq_str) == 0) {
    return(list(tokens = NULL, error = NULL, clean = ""))
  }
  seq_str <- paste(seq_str, collapse = "")
  if (is.na(seq_str)) {
    return(list(tokens = NULL, error = NULL, clean = ""))
  }
  seq_clean <- tolower(gsub("[,;[:space:]]+", "", seq_str))
  if (nchar(seq_clean) == 0) {
    return(list(tokens = NULL, error = NULL, clean = seq_clean))
  }

  token_pattern <- "((?:hft|hfs|[bpnt]))_?([0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)?(s(?=$|hft|hfs|[bpnt]|\\*))?(?:\\*(\\d+))?"
  matches <- gregexpr(token_pattern, seq_clean, perl = TRUE)[[1]]
  if (identical(matches, -1L)) {
    return(list(tokens = NULL, error = "Pattern Sequence contains no valid tokens.", clean = seq_clean))
  }

  tokens <- regmatches(seq_clean, list(matches))[[1]]
  if (length(tokens) == 0 || paste(tokens, collapse = "") != seq_clean) {
    return(list(
      tokens = NULL,
      error = paste0(
        "Pattern Sequence contains invalid characters or malformed tokens: '",
        seq_str,
        "'. Use tokens such as b5, p3, p1.2s, n2, t4, hft12, hfs40, and optional *k repeats."
      ),
      clean = seq_clean
    ))
  }

  parsed <- vector("list", length(tokens))
  for (i in seq_along(tokens)) {
    parts <- regmatches(tokens[i], regexec(paste0("^", token_pattern, "$"), tokens[i], perl = TRUE))[[1]]
    abbrev <- parts[2]
    value_text <- if (length(parts) >= 3 && !is.na(parts[3]) && parts[3] != "") parts[3] else NA_character_
    unit_suffix <- if (length(parts) >= 4 && !is.na(parts[4]) && parts[4] != "") parts[4] else NA_character_
    repeat_text <- if (length(parts) >= 5 && !is.na(parts[5]) && parts[5] != "") parts[5] else NA_character_
    repeat_count <- if (!is.na(repeat_text)) as.integer(repeat_text) else 1L

    if (!is.finite(repeat_count) || repeat_count < 1) {
      return(list(tokens = NULL, error = paste0("Repeat count must be a positive integer in token '", tokens[i], "'."), clean = seq_clean))
    }

    if (!is.na(unit_suffix) && abbrev != "p") {
      return(list(tokens = NULL, error = paste0("Only Pause tokens may use the seconds suffix in token '", tokens[i], "'."), clean = seq_clean))
    }

    value <- NA_real_
    if (!is.na(value_text)) {
      value <- as.numeric(value_text)
      if (!is.finite(value) || value <= 0) {
        return(list(tokens = NULL, error = paste0("Token value must be positive in token '", tokens[i], "'."), clean = seq_clean))
      }
      if (abbrev == "p" && is.na(unit_suffix) && value != as.integer(value)) {
        return(list(tokens = NULL, error = paste0("Pause-count token must use a positive integer unless it has the seconds suffix in token '", tokens[i], "'."), clean = seq_clean))
      }
      if (abbrev %in% c("b", "n", "t", "hft", "hfs") && value != as.integer(value)) {
        return(list(tokens = NULL, error = paste0("Spike-count token must use a positive integer in token '", tokens[i], "'."), clean = seq_clean))
      }
    }

    if (abbrev == "p" && is.na(unit_suffix) && !is.na(value)) {
      repeat_count <- repeat_count * as.integer(value)
      value <- NA_real_
    }

    pattern_name <- switch(abbrev,
                           "b" = "Burst",
                           "p" = "Pause",
                           "n" = "Noisy",
                           "t" = "Tonic",
                           "hft" = "high_frequency_tonic",
                           "hfs" = "high_frequency_spiking")
    parsed[[i]] <- list(Pattern = pattern_name, Value = value, Repeat = repeat_count)
  }

  list(tokens = parsed, error = NULL, clean = seq_clean)
}


rinvgauss1 <- function(mean, shape) {
  mean <- as.numeric(mean)[1]
  shape <- as.numeric(shape)[1]
  if (!is.finite(mean) || !is.finite(shape) || mean <= 0 || shape <= 0) return(NA_real_)
  v <- stats::rnorm(1)^2
  x <- mean + (mean^2 * v) / (2 * shape) -
    (mean / (2 * shape)) * sqrt(4 * mean * shape * v + mean^2 * v^2)
  if (!is.finite(x) || x <= 0) return(NA_real_)
  if (stats::runif(1) <= mean / (mean + x)) x else mean^2 / x
}

pinvgauss_value <- function(x, mean, shape) {
  x <- as.numeric(x)
  mean <- as.numeric(mean)[1]
  shape <- as.numeric(shape)[1]
  out <- rep(NA_real_, length(x))
  if (!is.finite(mean) || !is.finite(shape) || mean <= 0 || shape <= 0) return(out)
  out[x <= 0] <- 0
  finite_pos <- is.finite(x) & x > 0
  if (any(finite_pos)) {
    xx <- x[finite_pos]
    root <- sqrt(shape / xx)
    a <- root * (xx / mean - 1)
    b <- -root * (xx / mean + 1)
    term1 <- stats::pnorm(a)
    log_term2 <- 2 * shape / mean + stats::pnorm(b, log.p = TRUE)
    term2 <- ifelse(log_term2 > log(.Machine$double.xmax), Inf, exp(log_term2))
    vals <- term1 + term2
    high_inf <- is.infinite(vals) & vals > 0
    vals[high_inf] <- 1
    vals <- pmin(pmax(vals, 0), 1)
    out[finite_pos] <- vals
  }
  out[is.infinite(x) & x > 0] <- 1
  out
}

qinvgauss_value <- function(p, mean, shape) {
  p <- pmin(pmax(as.numeric(p), 0), 1)
  mean <- as.numeric(mean)[1]
  shape <- as.numeric(shape)[1]
  out <- rep(NA_real_, length(p))
  if (!is.finite(mean) || !is.finite(shape) || mean <= 0 || shape <= 0) return(out)
  for (i in seq_along(p)) {
    pp <- p[i]
    if (!is.finite(pp)) next
    if (pp <= 0) { out[i] <- 0; next }
    if (pp >= 1) { out[i] <- Inf; next }
    lower <- 0
    upper <- max(mean, 1e-12)
    guard <- 0L
    while (pinvgauss_value(upper, mean, shape) < pp && guard < 200L) {
      upper <- upper * 2
      guard <- guard + 1L
      if (!is.finite(upper) || upper > .Machine$double.xmax / 4) break
    }
    if (!is.finite(upper) || pinvgauss_value(upper, mean, shape) < pp) {
      out[i] <- NA_real_
    } else {
      out[i] <- tryCatch(
        stats::uniroot(function(z) pinvgauss_value(z, mean, shape) - pp,
                       lower = lower, upper = upper, tol = 1e-10)$root,
        error = function(e) NA_real_
      )
    }
  }
  out
}

dinvgauss_value <- function(x, mean, shape) {
  x <- as.numeric(x)
  mean <- as.numeric(mean)[1]
  shape <- as.numeric(shape)[1]
  out <- rep(0, length(x))
  if (!is.finite(mean) || !is.finite(shape) || mean <= 0 || shape <= 0) return(rep(NA_real_, length(x)))
  valid <- is.finite(x) & x > 0
  out[valid] <- sqrt(shape / (2 * pi * x[valid]^3)) *
    exp(-shape * (x[valid] - mean)^2 / (2 * mean^2 * x[valid]))
  out
}

distribution_nominal_mean <- function(dist_type, params) {
  if (identical(dist_type, "Exponential")) return(as.numeric(params$mean)[1])
  if (identical(dist_type, "Gamma")) return(as.numeric(params$shape)[1] * as.numeric(params$scale)[1])
  if (identical(dist_type, "Normal")) return(as.numeric(params$mean)[1])
  if (identical(dist_type, "Uniform")) return((as.numeric(params$min)[1] + as.numeric(params$max)[1]) / 2)
  if (identical(dist_type, "Lognormal")) return(exp(as.numeric(params$meanlog)[1] + 0.5 * as.numeric(params$sdlog)[1]^2))
  if (identical(dist_type, "Inverse Gaussian")) return(as.numeric(params$mean)[1])
  NA_real_
}

coerce_temporal_dependence <- function(dep) {
  rho <- if (!is.null(dep$rho) && is.finite(dep$rho)) as.numeric(dep$rho) else 0
  trend <- if (!is.null(dep$trend) && is.finite(dep$trend)) as.numeric(dep$trend) else 0
  rho <- min(max(rho, -0.95), 0.95)
  trend <- min(max(trend, -3), 3)
  list(rho = rho, trend = trend)
}

sample_raw_interval_from_config <- function(dist_type, params) {
  if (dist_type == "Exponential") {
    if (is.null(params$mean) || is.na(params$mean) || params$mean <= 0) return(NA_real_)
    rexp(1, rate = 1 / params$mean)
  } else if (dist_type == "Gamma") {
    if (is.null(params$shape) || is.null(params$scale) ||
        is.na(params$shape) || is.na(params$scale) ||
        params$shape <= 0 || params$scale <= 0) return(NA_real_)
    rgamma(1, shape = params$shape, scale = params$scale)
  } else if (dist_type == "Normal") {
    if (is.null(params$mean) || is.null(params$sd) ||
        is.na(params$mean) || is.na(params$sd) || params$sd < 0) return(NA_real_)
    if (params$sd == 0) return(ifelse(params$mean > 0, params$mean, NA_real_))
    val <- rnorm(1, mean = params$mean, sd = params$sd)
    ifelse(val > 0, val, NA_real_)
  } else if (dist_type == "Uniform") {
    if (is.null(params$min) || is.null(params$max) ||
        is.na(params$min) || is.na(params$max) || params$max <= params$min) return(NA_real_)
    runif(1, min = params$min, max = params$max)
  } else if (dist_type == "Lognormal") {
    if (is.null(params$meanlog) || is.null(params$sdlog) ||
        is.na(params$meanlog) || is.na(params$sdlog) || !is.finite(params$meanlog) ||
        !is.finite(params$sdlog) || params$sdlog < 0) return(NA_real_)
    if (params$sdlog == 0) return(exp(params$meanlog))
    rlnorm(1, meanlog = params$meanlog, sdlog = params$sdlog)
  } else if (dist_type == "Inverse Gaussian") {
    if (is.null(params$mean) || is.null(params$shape) ||
        is.na(params$mean) || is.na(params$shape) || params$mean <= 0 || params$shape <= 0) return(NA_real_)
    rinvgauss1(params$mean, params$shape)
  } else {
    NA_real_
  }
}

interval_distribution_error <- function(dist_type, params, pattern = "Interval") {
  prefix <- paste0(pattern, " ")
  if (identical(dist_type, "Exponential")) {
    if (is.null(params$mean) || !is.finite(params$mean) || params$mean <= 0) {
      return(paste0(prefix, "Exponential mean must be a positive finite number."))
    }
  } else if (identical(dist_type, "Gamma")) {
    if (is.null(params$shape) || is.null(params$scale) ||
        !is.finite(params$shape) || !is.finite(params$scale) ||
        params$shape <= 0 || params$scale <= 0) {
      return(paste0(prefix, "Gamma shape and scale must be positive finite numbers."))
    }
  } else if (identical(dist_type, "Normal")) {
    if (is.null(params$mean) || is.null(params$sd) ||
        !is.finite(params$mean) || !is.finite(params$sd) || params$sd < 0) {
      return(paste0(prefix, "Normal mean must be finite and sd must be finite and non-negative."))
    }
    if (params$sd == 0 && params$mean <= 0) {
      return(paste0(prefix, "degenerate Normal mean must be positive when sd is zero."))
    }
  } else if (identical(dist_type, "Uniform")) {
    if (is.null(params$min) || is.null(params$max) ||
        !is.finite(params$min) || !is.finite(params$max) || params$max <= params$min) {
      return(paste0(prefix, "Uniform max must be greater than min, and both must be finite."))
    }
  } else if (identical(dist_type, "Lognormal")) {
    if (is.null(params$meanlog) || is.null(params$sdlog) ||
        !is.finite(params$meanlog) || !is.finite(params$sdlog) || params$sdlog < 0) {
      return(paste0(prefix, "Lognormal meanlog must be finite and sdlog must be finite and non-negative."))
    }
  } else if (identical(dist_type, "Inverse Gaussian")) {
    if (is.null(params$mean) || is.null(params$shape) ||
        !is.finite(params$mean) || !is.finite(params$shape) || params$mean <= 0 || params$shape <= 0) {
      return(paste0(prefix, "Inverse Gaussian mean and shape lambda must be positive finite numbers."))
    }
  } else {
    return(paste0(prefix, "distribution type is not supported."))
  }
  NULL
}

dist_cdf_value <- function(dist_type, params, x) {
  x <- as.numeric(x)
  if (!is.null(interval_distribution_error(dist_type, params))) return(rep(NA_real_, length(x)))
  if (identical(dist_type, "Exponential")) {
    pexp(x, rate = 1 / params$mean)
  } else if (identical(dist_type, "Gamma")) {
    pgamma(x, shape = params$shape, scale = params$scale)
  } else if (identical(dist_type, "Normal")) {
    if (params$sd == 0) return(ifelse(x < params$mean, 0, 1))
    pnorm(x, mean = params$mean, sd = params$sd)
  } else if (identical(dist_type, "Uniform")) {
    punif(x, min = params$min, max = params$max)
  } else if (identical(dist_type, "Lognormal")) {
    if (params$sdlog == 0) return(ifelse(x < exp(params$meanlog), 0, 1))
    plnorm(x, meanlog = params$meanlog, sdlog = params$sdlog)
  } else if (identical(dist_type, "Inverse Gaussian")) {
    pinvgauss_value(x, mean = params$mean, shape = params$shape)
  } else {
    rep(NA_real_, length(x))
  }
}

dist_quantile_value <- function(dist_type, params, p) {
  p <- pmin(pmax(as.numeric(p), 0), 1)
  if (!is.null(interval_distribution_error(dist_type, params))) return(rep(NA_real_, length(p)))
  if (identical(dist_type, "Exponential")) {
    qexp(p, rate = 1 / params$mean)
  } else if (identical(dist_type, "Gamma")) {
    qgamma(p, shape = params$shape, scale = params$scale)
  } else if (identical(dist_type, "Normal")) {
    if (params$sd == 0) return(rep(params$mean, length(p)))
    qnorm(p, mean = params$mean, sd = params$sd)
  } else if (identical(dist_type, "Uniform")) {
    qunif(p, min = params$min, max = params$max)
  } else if (identical(dist_type, "Lognormal")) {
    if (params$sdlog == 0) return(rep(exp(params$meanlog), length(p)))
    qlnorm(p, meanlog = params$meanlog, sdlog = params$sdlog)
  } else if (identical(dist_type, "Inverse Gaussian")) {
    qinvgauss_value(p, mean = params$mean, shape = params$shape)
  } else {
    rep(NA_real_, length(p))
  }
}

dist_density_value <- function(dist_type, params, x) {
  x <- as.numeric(x)
  if (!is.null(interval_distribution_error(dist_type, params))) return(rep(NA_real_, length(x)))
  if (identical(dist_type, "Exponential")) {
    dexp(x, rate = 1 / params$mean)
  } else if (identical(dist_type, "Gamma")) {
    dgamma(x, shape = params$shape, scale = params$scale)
  } else if (identical(dist_type, "Normal")) {
    if (params$sd == 0) return(point_mass_density_on_grid(x, params$mean))
    dnorm(x, mean = params$mean, sd = params$sd)
  } else if (identical(dist_type, "Uniform")) {
    dunif(x, min = params$min, max = params$max)
  } else if (identical(dist_type, "Lognormal")) {
    if (params$sdlog == 0) return(point_mass_density_on_grid(x, exp(params$meanlog)))
    dlnorm(x, meanlog = params$meanlog, sdlog = params$sdlog)
  } else if (identical(dist_type, "Inverse Gaussian")) {
    dinvgauss_value(x, mean = params$mean, shape = params$shape)
  } else {
    rep(NA_real_, length(x))
  }
}

point_mass_density_on_grid <- function(x_seq, point) {
  x_seq <- as.numeric(x_seq)
  y <- rep(0, length(x_seq))
  if (!is.finite(point) || length(x_seq) == 0 || all(!is.finite(x_seq))) return(rep(NA_real_, length(x_seq)))
  finite_x <- x_seq[is.finite(x_seq)]
  if (length(finite_x) == 0 || point < min(finite_x) || point > max(finite_x)) return(rep(NA_real_, length(x_seq)))
  idx <- which.min(abs(x_seq - point))
  if (length(idx) == 0 || !is.finite(x_seq[idx])) return(rep(NA_real_, length(x_seq)))
  y[idx] <- 1
  if (length(x_seq) >= 2) {
    area <- sum(diff(x_seq) * (head(y, -1) + tail(y, -1)) / 2)
    if (is.finite(area) && area > 0) y <- y / area
  }
  y
}

normalize_interval_segments <- function(segments) {
  empty <- data.frame(Start = numeric(0), End = numeric(0))
  if (is.null(segments) || nrow(segments) == 0) return(empty)
  if (!all(c("Start", "End") %in% names(segments))) return(empty)

  segments <- data.frame(
    Start = as.numeric(segments$Start),
    End = as.numeric(segments$End)
  )
  segments <- segments[is.finite(segments$Start) & is.finite(segments$End) & segments$End >= segments$Start, , drop = FALSE]
  if (nrow(segments) == 0) return(empty)
  segments <- segments[order(segments$Start, segments$End), , drop = FALSE]

  out <- empty
  for (i in seq_len(nrow(segments))) {
    seg <- segments[i, , drop = FALSE]
    if (nrow(out) == 0) {
      out <- rbind(out, seg)
    } else if (seg$Start <= out$End[nrow(out)]) {
      out$End[nrow(out)] <- max(out$End[nrow(out)], seg$End)
    } else {
      out <- rbind(out, seg)
    }
  }
  rownames(out) <- NULL
  out
}

segment_from_range <- function(rng) {
  if (length(rng) != 2 || any(!is.finite(rng)) || rng[2] < rng[1]) {
    return(data.frame(Start = numeric(0), End = numeric(0)))
  }
  normalize_interval_segments(data.frame(Start = as.numeric(rng[1]), End = as.numeric(rng[2])))
}

intersect_interval_segments <- function(segments, limits) {
  empty <- data.frame(Start = numeric(0), End = numeric(0))
  segments <- normalize_interval_segments(segments)
  if (nrow(segments) == 0 || length(limits) != 2 || any(is.na(limits))) return(empty)
  low_limit <- as.numeric(limits[1])
  high_limit <- as.numeric(limits[2])
  if (high_limit < low_limit) return(empty)

  out <- empty
  for (i in seq_len(nrow(segments))) {
    st <- max(segments$Start[i], low_limit)
    en <- min(segments$End[i], high_limit)
    if (is.finite(st) && is.finite(en) && en >= st) {
      out <- rbind(out, data.frame(Start = st, End = en))
    }
  }
  normalize_interval_segments(out)
}

subtract_interval_segments <- function(segments, blocked) {
  segments <- normalize_interval_segments(segments)
  empty <- data.frame(Start = numeric(0), End = numeric(0))
  if (nrow(segments) == 0) return(empty)
  if (length(blocked) != 2 || any(!is.finite(blocked)) || blocked[2] < blocked[1]) return(segments)

  out <- empty
  for (i in seq_len(nrow(segments))) {
    seg_start <- segments$Start[i]
    seg_end <- segments$End[i]
    if (blocked[2] <= seg_start || blocked[1] >= seg_end) {
      out <- rbind(out, data.frame(Start = seg_start, End = seg_end))
    } else {
      if (blocked[1] > seg_start) {
        out <- rbind(out, data.frame(Start = seg_start, End = min(blocked[1], seg_end)))
      }
      if (blocked[2] < seg_end) {
        out <- rbind(out, data.frame(Start = max(blocked[2], seg_start), End = seg_end))
      }
    }
  }
  normalize_interval_segments(out)
}

x_in_interval_segments <- function(x, segments) {
  x <- as.numeric(x)
  segments <- normalize_interval_segments(segments)
  if (nrow(segments) == 0) return(rep(FALSE, length(x)))
  Reduce(`|`, lapply(seq_len(nrow(segments)), function(i) {
    x >= segments$Start[i] & x <= segments$End[i]
  }))
}

interval_segments_mass <- function(dist_type, params, segments) {
  segments <- normalize_interval_segments(segments)
  if (nrow(segments) == 0) return(0)
  err <- interval_distribution_error(dist_type, params)
  if (!is.null(err)) return(NA_real_)

  if (identical(dist_type, "Normal") && isTRUE(params$sd == 0)) {
    return(ifelse(any(params$mean >= segments$Start & params$mean <= segments$End), 1, 0))
  }
  if (identical(dist_type, "Lognormal") && isTRUE(params$sdlog == 0)) {
    point <- exp(params$meanlog)
    return(ifelse(any(point >= segments$Start & point <= segments$End), 1, 0))
  }

  masses <- dist_cdf_value(dist_type, params, segments$End) -
    dist_cdf_value(dist_type, params, segments$Start)
  masses[!is.finite(masses) | masses < 0] <- 0
  sum(masses)
}

sample_truncated_interval_from_segments <- function(dist_type, params, segments) {
  segments <- normalize_interval_segments(segments)
  if (nrow(segments) == 0) return(NA_real_)
  err <- interval_distribution_error(dist_type, params)
  if (!is.null(err)) return(NA_real_)

  if (identical(dist_type, "Normal") && isTRUE(params$sd == 0)) {
    return(ifelse(any(params$mean >= segments$Start & params$mean <= segments$End), params$mean, NA_real_))
  }
  if (identical(dist_type, "Lognormal") && isTRUE(params$sdlog == 0)) {
    point <- exp(params$meanlog)
    return(ifelse(any(point >= segments$Start & point <= segments$End), point, NA_real_))
  }

  p_start <- dist_cdf_value(dist_type, params, segments$Start)
  p_end <- dist_cdf_value(dist_type, params, segments$End)
  masses <- p_end - p_start
  masses[!is.finite(masses) | masses <= 0] <- 0
  total_mass <- sum(masses)
  if (!is.finite(total_mass) || total_mass <= 0) return(NA_real_)

  chosen <- sample(seq_len(nrow(segments)), size = 1, prob = masses)
  p <- runif(1, min = p_start[chosen], max = p_end[chosen])
  value <- dist_quantile_value(dist_type, params, p)
  if (!is.finite(value)) return(NA_real_)
  value <- min(max(value, segments$Start[chosen]), segments$End[chosen])
  ifelse(is.finite(value), value, NA_real_)
}

interval_acceptance_mass <- function(dist_type, params, rng) {
  if (length(rng) != 2 || any(is.na(rng)) || rng[2] < rng[1]) return(NA_real_)
  interval_segments_mass(dist_type, params, segment_from_range(rng))
}

effective_interval_range_from_config <- function(config, pattern) {
  pat_cfg <- config$patterns[[pattern]]
  if (is.null(pat_cfg) || is.null(pat_cfg$interval_range)) return(c(NA_real_, NA_real_))
  rng <- as.numeric(pat_cfg$interval_range)
  if (length(rng) != 2 || any(!is.finite(rng)) || rng[2] < rng[1]) {
    return(c(NA_real_, NA_real_))
  }
  if (!is.null(config$inter_event_gap) && is.finite(config$inter_event_gap)) {
    rng[1] <- max(rng[1], as.numeric(config$inter_event_gap))
  }
  if (rng[2] < rng[1]) return(c(NA_real_, NA_real_))
  rng
}

noisy_specificity_from_config <- function(config) {
  cfg <- config$noisy_specificity
  if (is.null(cfg)) cfg <- list()

  raw_guard <- if (!is.null(cfg$context_guard_s) && is.finite(as.numeric(cfg$context_guard_s))) {
    as.numeric(cfg$context_guard_s)
  } else if (!is.null(cfg$clean_guard_s) && is.finite(as.numeric(cfg$clean_guard_s))) {
    as.numeric(cfg$clean_guard_s)
  } else if (!is.null(cfg$tolerance) && is.finite(as.numeric(cfg$tolerance))) {
    as.numeric(cfg$tolerance)
  } else {
    NOISY_CONTEXT_GUARD_S
  }
  context_guard_s <- max(raw_guard, 0)

  raw_mm <- if (!is.null(cfg$mm_ratio) && is.finite(as.numeric(cfg$mm_ratio))) {
    as.numeric(cfg$mm_ratio)
  } else {
    NOISY_MIN_MM_RATIO
  }
  mm_ratio <- max(NOISY_MIN_MM_RATIO, raw_mm, 1.000001)

  raw_pause_guard <- if (!is.null(cfg$pause_guard_s) && is.finite(as.numeric(cfg$pause_guard_s))) {
    as.numeric(cfg$pause_guard_s)
  } else {
    NOISY_PAUSE_GUARD_S
  }
  pause_guard_s <- max(0, raw_pause_guard)

  raw_pause_guard_ratio <- if (!is.null(cfg$pause_guard_ratio) && is.finite(as.numeric(cfg$pause_guard_ratio))) {
    as.numeric(cfg$pause_guard_ratio)
  } else {
    NOISY_PAUSE_GUARD_RATIO
  }
  pause_guard_ratio <- max(0, raw_pause_guard_ratio)

  raw_tonic_upper_multiplier <- if (!is.null(cfg$tonic_upper_multiplier) && is.finite(as.numeric(cfg$tonic_upper_multiplier))) {
    as.numeric(cfg$tonic_upper_multiplier)
  } else {
    NOISY_TONIC_UPPER_MULTIPLIER
  }
  tonic_upper_multiplier <- max(0.05, raw_tonic_upper_multiplier)

  list(
    clean_label = TRUE,
    contextual_mode_overlap = TRUE,
    avoid_mode_overlap = FALSE,
    avoid_pause_overlap = TRUE,
    limit_upper_to_tonic = TRUE,
    avoid_near_pause = TRUE,
    tolerance = context_guard_s,
    clean_guard_s = context_guard_s,
    context_guard_s = context_guard_s,
    pause_guard_s = pause_guard_s,
    pause_guard_ratio = pause_guard_ratio,
    tonic_upper_multiplier = tonic_upper_multiplier,
    mm_ratio = mm_ratio,
    min_run_cv = NOISY_MIN_RUN_CV,
    min_run_cv2 = NOISY_MIN_RUN_CV2,
    toniclike_min_isi_count = NOISY_TONICLIKE_MIN_ISI_COUNT
  )
}

noisy_physiological_envelope <- function(config) {
  spec <- noisy_specificity_from_config(config)
  lower <- if (!is.null(config$inter_event_gap) && is.finite(config$inter_event_gap)) {
    max(0, as.numeric(config$inter_event_gap))
  } else {
    0
  }

  upper_candidates <- numeric(0)
  tonic_rng <- effective_interval_range_from_config(config, "Tonic")
  if (length(tonic_rng) == 2 && all(is.finite(tonic_rng)) && tonic_rng[2] >= tonic_rng[1]) {
    upper_candidates <- c(upper_candidates, tonic_rng[2] * spec$tonic_upper_multiplier)
  }

  pause_rng <- effective_interval_range_from_config(config, "Pause")
  if (length(pause_rng) == 2 && all(is.finite(pause_rng)) && pause_rng[2] >= pause_rng[1]) {
    pause_guard <- max(spec$pause_guard_s, spec$pause_guard_ratio * pause_rng[1])
    upper_candidates <- c(upper_candidates, pause_rng[1] - pause_guard)
  }

  upper_candidates <- upper_candidates[is.finite(upper_candidates)]
  upper <- if (length(upper_candidates) > 0) min(upper_candidates) else Inf
  if (!is.finite(upper) || upper < lower) return(c(NA_real_, NA_real_))
  c(lower, upper)
}

noisy_mode_zone <- function(value, config, guard_s = 0) {
  if (!is.finite(value) || value <= 0) return(NA_character_)
  for (label in c("high_frequency_spiking", "high_frequency_tonic", "Burst", "Tonic", "Pause")) {
    rng <- effective_interval_range_from_config(config, label)
    if (length(rng) == 2 && all(is.finite(rng)) && rng[2] >= rng[1]) {
      rng <- c(max(0, rng[1] - guard_s), rng[2] + guard_s)
      if (value >= rng[1] && value <= rng[2]) return(label)
    }
  }
  NA_character_
}

noisy_same_zone_pair_violation <- function(values, config, guard_s = 0) {
  values <- as.numeric(values)
  values <- values[is.finite(values) & values > 0]
  if (length(values) < 2) return(FALSE)
  zones <- vapply(values, noisy_mode_zone, character(1), config = config, guard_s = guard_s)
  for (i in seq_len(length(zones) - 1L)) {
    if (!is.na(zones[i]) && zones[i] %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking") && identical(zones[i], zones[i + 1L])) {
      return(TRUE)
    }
  }
  FALSE
}

interval_sequence_groups <- function(intervals) {
  if (is.null(intervals) || nrow(intervals) == 0) return(list())
  x <- intervals
  if (!"Train" %in% names(x)) x$Train <- 1L
  if (!"Interval_ID" %in% names(x)) x$Interval_ID <- seq_len(nrow(x))
  x$Train <- suppressWarnings(as.integer(x$Train))
  x$Interval_ID <- suppressWarnings(as.integer(x$Interval_ID))
  x$Train[!is.finite(x$Train)] <- 1L
  bad_interval_id <- !is.finite(x$Interval_ID)
  x$Interval_ID[bad_interval_id] <- seq_len(nrow(x))[bad_interval_id]
  x <- x[order(x$Train, x$Interval_ID), , drop = FALSE]
  split(x, x$Train, drop = TRUE)
}

count_global_noisy_same_zone_pair_violations <- function(intervals, config, guard_s = 0) {
  if (is.null(intervals) || nrow(intervals) < 2) return(0L)
  count <- 0L
  for (x in interval_sequence_groups(intervals)) {
    if (nrow(x) < 2) next
    for (i in seq_len(nrow(x) - 1L)) {
      if (!identical(as.character(x$ISI_Label[i]), "Noisy") ||
          !identical(as.character(x$ISI_Label[i + 1L]), "Noisy")) next
      z1 <- noisy_mode_zone(x$ISI_s[i], config, guard_s = guard_s)
      z2 <- noisy_mode_zone(x$ISI_s[i + 1L], config, guard_s = guard_s)
      if (!is.na(z1) && z1 %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking") && identical(z1, z2)) {
        count <- count + 1L
      }
    }
  }
  count
}

forbidden_hf_burst_adjacency <- function(pattern, previous_pattern) {
  pattern <- as.character(pattern)[1]
  previous_pattern <- as.character(previous_pattern)[1]
  if (is.na(pattern) || is.na(previous_pattern)) return(FALSE)
  (identical(pattern, "Burst") && previous_pattern %in% HF_PATTERN_LABELS) ||
    (pattern %in% HF_PATTERN_LABELS && identical(previous_pattern, "Burst"))
}

apply_neighbor_mm_segments <- function(segments, pattern, previous_isi = NA_real_, previous_pattern = NA_character_, spec = NULL) {
  segments <- normalize_interval_segments(segments)
  if (nrow(segments) == 0) return(segments)
  if (is.null(spec)) spec <- list(mm_ratio = 1.5)
  if (!is.finite(previous_isi) || previous_isi <= 0 ||
      is.na(previous_pattern) || !previous_pattern %in% SPIKE_PATTERN_LEVELS) {
    return(segments)
  }
  one_is_noisy <- xor(pattern == "Noisy", previous_pattern == "Noisy")
  if (!one_is_noisy) return(segments)

  lower_allowed <- intersect_interval_segments(segments, c(-Inf, previous_isi / spec$mm_ratio))
  upper_allowed <- intersect_interval_segments(segments, c(previous_isi * spec$mm_ratio, Inf))
  normalize_interval_segments(rbind(lower_allowed, upper_allowed))
}

effective_pattern_segments_from_config <- function(config, pattern, previous_isi = NA_real_, previous_pattern = NA_character_) {
  if (forbidden_hf_burst_adjacency(pattern, previous_pattern)) {
    return(data.frame(Start = numeric(0), End = numeric(0)))
  }

  rng <- effective_interval_range_from_config(config, pattern)
  segments <- segment_from_range(rng)
  if (nrow(segments) == 0) return(segments)

  spec <- noisy_specificity_from_config(config)

  if (identical(pattern, "Noisy")) {
    # Noisy is benchmark noise, not a slow Pause-like state. Its candidate range
    # is therefore clipped to a physiological envelope: absolute refractory period
    # up to the Tonic upper scale, with a margin below the Pause lower bound.
    envelope <- noisy_physiological_envelope(config)
    if (length(envelope) == 2 && all(is.finite(envelope)) && envelope[2] >= envelope[1]) {
      segments <- intersect_interval_segments(segments, envelope)
    } else {
      return(data.frame(Start = numeric(0), End = numeric(0)))
    }

    # Pause is itself a single long ISI; a Pause-like Noisy ISI would be
    # indistinguishable from Pause, so it is always excluded. Burst-like and
    # Tonic-like Noisy ISIs are allowed only when contextually isolated.
    pause_block <- effective_interval_range_from_config(config, "Pause")
    if (length(pause_block) == 2 && all(is.finite(pause_block)) && pause_block[2] >= pause_block[1]) {
      pause_block <- c(max(0, pause_block[1] - spec$context_guard_s), pause_block[2] + spec$context_guard_s)
      segments <- subtract_interval_segments(segments, pause_block)
    }

    # If the immediately preceding interval is Burst-like or Tonic-like, the
    # current Noisy ISI must not occupy the same zone. This prevents a Noisy
    # singleton from visually extending a Burst/Tonic episode, and also prevents
    # two consecutive similar Noisy ISIs from forming an apparent Burst/Tonic.
    prev_zone <- noisy_mode_zone(previous_isi, config, guard_s = 0)
    zones_to_block <- character(0)
    if (previous_pattern %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking")) {
      zones_to_block <- c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking")
    } else if (identical(previous_pattern, "Noisy") && !is.na(prev_zone) && prev_zone %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking")) {
      zones_to_block <- prev_zone
    }
    for (zone in unique(zones_to_block)) {
      blocked <- effective_interval_range_from_config(config, zone)
      if (length(blocked) == 2 && all(is.finite(blocked)) && blocked[2] >= blocked[1]) {
        blocked <- c(max(0, blocked[1] - spec$context_guard_s), blocked[2] + spec$context_guard_s)
        segments <- subtract_interval_segments(segments, blocked)
      }
    }
  }

  if (pattern %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking") && identical(previous_pattern, "Noisy")) {
    # A true Burst/Tonic run must not begin immediately after a Noisy ISI that
    # is itself Burst-like or Tonic-like. Otherwise the singleton Noisy would be
    # visually absorbed into the neighboring patterned episode.
    prev_zone <- noisy_mode_zone(previous_isi, config, guard_s = 0)
    if (!is.na(prev_zone) && prev_zone %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking")) {
      return(data.frame(Start = numeric(0), End = numeric(0)))
    }
  }

  apply_neighbor_mm_segments(segments, pattern, previous_isi, previous_pattern, spec)
}

minimum_segment_start <- function(segments) {
  segments <- normalize_interval_segments(segments)
  if (nrow(segments) == 0) return(NA_real_)
  min(segments$Start)
}

sample_interval_from_config_segments <- function(config, pattern, previous_isi = NA_real_, previous_pattern = NA_character_) {
  pat_cfg <- config$patterns[[pattern]]
  if (is.null(pat_cfg)) return(NA_real_)
  segments <- effective_pattern_segments_from_config(config, pattern, previous_isi, previous_pattern)
  sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
}

manual_sequence_supplied <- function(config) {
  !is.null(config$pattern_sequence) && length(config$pattern_sequence) > 0
}

active_patterns_from_config <- function(config) {
  if (stimulation_enabled(config)) {
    return(SPIKE_PATTERN_LEVELS)
  }
  if (manual_sequence_supplied(config)) {
    seq_patterns <- vapply(config$pattern_sequence, function(x) x$Pattern, character(1))
    return(unique(seq_patterns[seq_patterns %in% SPIKE_PATTERN_LEVELS]))
  }

  ratios <- normalize_pattern_ratios(config$ratios)
  active <- names(ratios)[is.finite(ratios) & ratios > 0]
  active[active %in% SPIKE_PATTERN_LEVELS]
}

pattern_requires_interval_distribution <- function(config, pattern) {
  if (!manual_sequence_supplied(config)) return(TRUE)
  entries <- config$pattern_sequence[vapply(config$pattern_sequence, function(x) identical(x$Pattern, pattern), logical(1))]
  if (length(entries) == 0) return(FALSE)

  # Non-Pause manual tokens require the pattern's ISI distribution because their labeled intervals are sampled directly.
  if (!identical(pattern, "Pause")) return(TRUE)

  # If the sequence starts with a fixed Pause and leading-silence is explicitly disabled,
  # the simulator must still sample a positive first-spike latency before the true Pause ISI.
  # That latency uses the configured initial-latency model, which may require the Pause distribution.
  first_entry <- config$pattern_sequence[[1]]
  first_is_fixed_pause <- !is.null(first_entry) && identical(first_entry$Pattern, "Pause") &&
    is.finite(suppressWarnings(as.numeric(first_entry$Value)))
  if (isTRUE(first_is_fixed_pause) && identical(config$leading_silence_initial_pause, FALSE)) {
    mode <- if (!is.null(config$initial_latency_model)) as.character(config$initial_latency_model)[1] else "residual_life"
    if (mode %in% c("residual_life", "same_distribution")) return(TRUE)
  }

  # Pause tokens with an explicit seconds suffix (e.g. p1.0s) use the fixed value, not the Pause distribution.
  # Random Pause tokens (e.g. p1 or p3) still require the Pause interval distribution.
  any(vapply(entries, function(x) {
    value <- suppressWarnings(as.numeric(x$Value))
    length(value) == 0 || is.na(value) || !is.finite(value)
  }, logical(1)))
}

validate_spike_count_range <- function(pattern, rng) {
  rng <- suppressWarnings(as.integer(round(as.numeric(rng))))
  if (length(rng) != 2 || any(!is.finite(rng)) || rng[2] < rng[1] || rng[1] < 1) {
    return(sprintf("%s spike-count range must be a positive integer range.", pattern))
  }
  if (identical(pattern, "Burst") && rng[2] < BURST_MIN_BOUNDARY_SPIKES) {
    return(sprintf("Burst spike-count range must allow at least %d boundary spikes so that a Burst is not just an isolated ISI.", BURST_MIN_BOUNDARY_SPIKES))
  }
  if (identical(pattern, "Tonic") && rng[2] < TONIC_MIN_BOUNDARY_SPIKES) {
    return(sprintf("Tonic spike-count range must allow at least %d boundary spikes so that CV/CV2/LV can be evaluated.", TONIC_MIN_BOUNDARY_SPIKES))
  }
  if (identical(pattern, "high_frequency_tonic") && rng[2] < HF_TONIC_MIN_BOUNDARY_SPIKES) {
    return(sprintf("high_frequency_tonic spike-count range must allow at least %d boundary spikes so that sustained regularity can be evaluated.", HF_TONIC_MIN_BOUNDARY_SPIKES))
  }
  if (identical(pattern, "high_frequency_spiking") && rng[2] < HF_SPIKING_MIN_BOUNDARY_SPIKES) {
    return(sprintf("high_frequency_spiking spike-count range must allow at least %d boundary spikes because this label represents a sustained state rather than a burst packet.", HF_SPIKING_MIN_BOUNDARY_SPIKES))
  }
  if (!pattern %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking") && rng[2] < 2L) {
    return(sprintf("%s spike-count range must allow at least 2 spikes so that the pattern can define at least one labeled ISI.", pattern))
  }
  NULL
}

validate_nonnegative_range_core <- function(rng) {
  length(rng) == 2 && all(is.finite(rng)) && rng[1] >= 0 && rng[2] >= rng[1]
}

validate_sim_config_core <- function(config) {
  errors <- character(0)
  warnings <- character(0)
  add_error <- function(message) errors <<- unique(c(errors, message))
  add_warning <- function(message) warnings <<- unique(c(warnings, message))

  if (is.null(config$total_time) || !is.finite(config$total_time) || config$total_time <= 0) {
    add_error("Total time must be a positive finite number.")
  }
  if (!is.null(config$inter_event_gap) && (!is.finite(config$inter_event_gap) || config$inter_event_gap < 0)) {
    add_error("Absolute refractory period must be a non-negative finite number.")
  }
  if (is.null(config$patterns) || !all(SPIKE_PATTERN_LEVELS %in% names(config$patterns))) {
    add_error(sprintf("All %d pattern configurations must be present.", length(SPIKE_PATTERN_LEVELS)))
    return(list(errors = errors, warnings = warnings))
  }

  initial_latency_model <- if (!is.null(config$initial_latency_model)) as.character(config$initial_latency_model)[1] else "residual_life"
  if (!initial_latency_model %in% c("residual_life", "same_distribution", "uniform")) {
    add_error("Initial latency model must be one of: residual_life, same_distribution, uniform.")
  }

  ratios <- normalize_pattern_ratios(config$ratios)
  active <- active_patterns_from_config(config)
  if (length(active) == 0) active <- SPIKE_PATTERN_LEVELS

  spec <- noisy_specificity_from_config(config)
  if ("Noisy" %in% active && (!is.finite(spec$mm_ratio) || spec$mm_ratio <= 1)) {
    add_error("Noisy adjacency MM threshold must be greater than 1.")
  }

  for (pattern in active) {
    pat_cfg <- config$patterns[[pattern]]
    requires_distribution <- pattern_requires_interval_distribution(config, pattern)
    if (isTRUE(requires_distribution)) {
      dist_error <- interval_distribution_error(pat_cfg$dist_type, pat_cfg$params, pattern)
      if (!is.null(dist_error)) add_error(dist_error)
    }

    rng_raw <- as.numeric(pat_cfg$interval_range)
    if (length(rng_raw) != 2 || any(!is.finite(rng_raw)) || rng_raw[2] < rng_raw[1] || rng_raw[1] < 0) {
      add_error(sprintf("%s accepted interval range must be a finite non-negative range.", pattern))
    }
    rng_eff <- effective_interval_range_from_config(config, pattern)
    if (length(rng_eff) != 2 || any(!is.finite(rng_eff)) || rng_eff[2] < rng_eff[1]) {
      add_error(sprintf("%s effective interval range is empty after applying the absolute refractory period.", pattern))
    }

    segments <- effective_pattern_segments_from_config(config, pattern)
    if (nrow(segments) == 0) {
      add_error(sprintf(
        "%s has no feasible interval segment after accepted range, absolute refractory period, contextual Noisy clean-label rules and static feasibility checks.",
        pattern
      ))
    } else if (isTRUE(requires_distribution)) {
      mass <- interval_segments_mass(pat_cfg$dist_type, pat_cfg$params, segments)
      if (!is.finite(mass) || mass <= 0) {
        add_error(sprintf(
          "%s has no positive probability mass after accepted range, absolute refractory period, contextual Noisy clean-label rules and static feasibility checks.",
          pattern
        ))
      } else if (is.finite(mass) && mass < 1e-5) {
        add_warning(sprintf("%s effective acceptance probability is very small (%.4g); simulation may be slow or short.", pattern, mass))
      }
    }

    dep <- coerce_temporal_dependence(if (!is.null(pat_cfg$temporal_dependence)) pat_cfg$temporal_dependence else list(rho = 0, trend = 0))
    raw_rho <- if (!is.null(pat_cfg$temporal_dependence$rho)) as.numeric(pat_cfg$temporal_dependence$rho) else 0
    raw_trend <- if (!is.null(pat_cfg$temporal_dependence$trend)) as.numeric(pat_cfg$temporal_dependence$trend) else 0
    if (!is.finite(raw_rho) || raw_rho < -0.95 || raw_rho > 0.95) {
      add_error(sprintf("%s ISI serial correlation rho must be finite and within [-0.95, 0.95].", pattern))
    }
    if (!is.finite(raw_trend) || raw_trend < -3 || raw_trend > 3) {
      add_error(sprintf("%s ISI trend log-slope must be finite and within [-3, 3].", pattern))
    }

    if (!identical(pattern, "Pause")) {
      count_error <- validate_spike_count_range(pattern, pat_cfg$spike_count_range)
      if (!is.null(count_error)) add_error(count_error)
    }
  }

  for (regular_label in intersect(c("Tonic", "high_frequency_tonic"), active)) {
    regular_ranges <- config$patterns[[regular_label]]$regularity_ranges
    if (!is.null(regular_ranges)) {
      required_metrics <- c("cv", "cv2", "lv")
      if (identical(regular_label, "high_frequency_tonic")) required_metrics <- c(required_metrics, "mm")
      valid_metrics <- vapply(required_metrics, function(metric) {
        validate_nonnegative_range_core(as.numeric(regular_ranges[[metric]]))
      }, logical(1))
      if (!all(valid_metrics)) {
        add_error(sprintf("%s %s ranges must be finite non-negative ranges.", regular_label, paste(toupper(required_metrics), collapse = "/")))
      }
    }
  }

  if ("high_frequency_spiking" %in% active) {
    rules <- config$patterns$high_frequency_spiking$state_rules
    if (is.null(rules)) {
      add_error("high_frequency_spiking state_rules must be present.")
    } else {
      short_rng <- as.numeric(rules$short_isi_range)
      bridge_rng <- as.numeric(rules$bridge_isi_range)
      if (!validate_nonnegative_range_core(short_rng) || short_rng[1] <= 0) add_error("high_frequency_spiking short_isi_range must be a positive finite range.")
      if (!validate_nonnegative_range_core(bridge_rng) || bridge_rng[1] < short_rng[2]) add_error("high_frequency_spiking bridge_isi_range must begin at or above the short-ISI upper bound.")
      sf_min <- safe_num(rules$short_fraction_min, NA_real_)
      sf_target <- safe_num(rules$target_short_fraction, NA_real_)
      bridge_max <- safe_num(rules$bridge_fraction_max, NA_real_)
      if (!is.finite(sf_min) || sf_min < 0 || sf_min > 1) add_error("high_frequency_spiking short_fraction_min must be within [0, 1].")
      if (!is.finite(sf_target) || sf_target < sf_min || sf_target > 1) add_error("high_frequency_spiking target_short_fraction must be within [short_fraction_min, 1].")
      if (!is.finite(bridge_max) || bridge_max < 0 || bridge_max > 1) add_error("high_frequency_spiking bridge_fraction_max must be within [0, 1].")
      overall_rng <- as.numeric(config$patterns$high_frequency_spiking$interval_range)
      if (validate_nonnegative_range_core(overall_rng)) {
        if (validate_nonnegative_range_core(short_rng) && (short_rng[1] < overall_rng[1] || short_rng[2] > overall_rng[2])) {
          add_error("high_frequency_spiking short_isi_range must be contained in its accepted interval range.")
        }
        if (validate_nonnegative_range_core(bridge_rng) && (bridge_rng[1] < overall_rng[1] || bridge_rng[2] > overall_rng[2])) {
          add_error("high_frequency_spiking bridge_isi_range must be contained in its accepted interval range.")
        }
      }
      min_duration <- safe_num(rules$min_duration_s, NA_real_)
      if (!is.finite(min_duration) || min_duration < 0) add_error("high_frequency_spiking min_duration_s must be a finite non-negative number.")
      max_consec <- suppressWarnings(as.integer(rules$max_consecutive_bridge))
      if (!is.finite(max_consec) || max_consec < 0) add_error("high_frequency_spiking max_consecutive_bridge must be a non-negative integer.")
    }
  }

  obs <- if (!is.null(config$observation)) config$observation else NULL
  if (!is.null(obs) && isTRUE(obs$enabled)) {
    obs_detection_probability <- suppressWarnings(as.numeric(obs$detection_probability)[1])
    obs_false_positive_rate_hz <- suppressWarnings(as.numeric(obs$false_positive_rate_hz)[1])
    obs_jitter_sd_s <- suppressWarnings(as.numeric(obs$jitter_sd_s)[1])
    obs_time_bias_s <- suppressWarnings(as.numeric(obs$time_bias_s)[1])
    obs_dead_time_s <- suppressWarnings(as.numeric(obs$dead_time_s)[1])
    obs_seed_offset <- suppressWarnings(as.numeric(obs$seed_offset)[1])
    if (!is.finite(obs_detection_probability) || obs_detection_probability < 0 || obs_detection_probability > 1) {
      add_error("Observation detection probability must be finite and within [0, 1].")
    }
    if (!is.finite(obs_false_positive_rate_hz) || obs_false_positive_rate_hz < 0) {
      add_error("Observation false-positive rate must be a finite non-negative number.")
    } else if (is.finite(config$total_time) && obs_false_positive_rate_hz * config$total_time > 1e5) {
      add_warning("Observation false-positive settings are expected to create more than 100000 false events per train; simulation may be slow or memory-intensive.")
    }
    if (!is.finite(obs_jitter_sd_s) || obs_jitter_sd_s < 0) {
      add_error("Observation timestamp jitter SD must be finite and non-negative.")
    }
    if (!is.finite(obs_time_bias_s)) {
      add_error("Observation timestamp bias must be finite.")
    }
    if (!is.finite(obs_dead_time_s) || obs_dead_time_s < 0) {
      add_error("Observation detector dead time must be finite and non-negative.")
    }
    if (!is.finite(obs_seed_offset) || obs_seed_offset < 1 || abs(obs_seed_offset - round(obs_seed_offset)) > .Machine$double.eps^0.5) {
      add_error("Observation seed offset must be a positive integer.")
    }
  }

  list(errors = errors, warnings = warnings)
}




# -----------------------------------------------------------------------------
# V13 stimulus-response core
# -----------------------------------------------------------------------------
# Stimulation is implemented as a phenomenological ISI-label generator. A stimulus
# schedule modulates the next labeled-ISI sequence through an excitatory, suppressive,
# biphasic, pause-rebound, or state-dependent response kernel. Repeated stimuli update
# a low-dimensional adaptation state with exponential recovery. This module is intended
# for benchmark-grade stimulus-response spike trains, not for conductance-level membrane
# simulation.

stimulation_enabled <- function(config) {
  isTRUE(!is.null(config$stimulation) && isTRUE(config$stimulation$enabled))
}

sanitize_stimulation_config <- function(stim) {
  if (is.null(stim)) stim <- list()
  stim$enabled <- isTRUE(stim$enabled)
  stim$experiment_preset <- tolower(as.character(value_or(stim$experiment_preset, "custom"))[1])
  if (!stim$experiment_preset %in% c("custom", "intensity_response", "repeated_adaptation", "stimulus_suppression", "biphasic_burst_pause", "paired_pulse_recovery", "oddball_adaptation", "state_dependent", "state_dependent_balanced", "feature_tuning")) stim$experiment_preset <- "custom"
  if (!identical(stim$experiment_preset, "custom")) stim$enabled <- TRUE
  stim$protocol <- as.character(value_or(stim$protocol, "regular"))[1]
  if (!stim$protocol %in% c("regular", "intensity_ramp", "repeated", "paired_pulse", "oddball", "manual", "feature_tuning")) stim$protocol <- "regular"
  stim$response_type <- as.character(value_or(stim$response_type, "excitatory_burst"))[1]
  if (!stim$response_type %in% c("excitatory_burst", "suppressive_pause", "biphasic", "pause_rebound", "state_dependent", "feature_tuned")) stim$response_type <- "excitatory_burst"
  if (identical(stim$experiment_preset, "intensity_response")) {
    stim$protocol <- "intensity_ramp"; stim$response_type <- "excitatory_burst"; stim$adaptation_enabled <- FALSE
  } else if (identical(stim$experiment_preset, "repeated_adaptation")) {
    stim$protocol <- "repeated"; stim$response_type <- "excitatory_burst"; stim$adaptation_enabled <- TRUE
  } else if (identical(stim$experiment_preset, "stimulus_suppression")) {
    stim$protocol <- "regular"; stim$response_type <- "suppressive_pause"; stim$adaptation_enabled <- FALSE
  } else if (identical(stim$experiment_preset, "biphasic_burst_pause")) {
    stim$protocol <- "regular"; stim$response_type <- "biphasic"; stim$adaptation_enabled <- TRUE
  } else if (identical(stim$experiment_preset, "paired_pulse_recovery")) {
    stim$protocol <- "paired_pulse"; stim$response_type <- "excitatory_burst"; stim$adaptation_enabled <- TRUE
  } else if (identical(stim$experiment_preset, "oddball_adaptation")) {
    stim$protocol <- "oddball"; stim$response_type <- "excitatory_burst"; stim$adaptation_enabled <- TRUE
  } else if (stim$experiment_preset %in% c("state_dependent", "state_dependent_balanced")) {
    stim$protocol <- "regular"; stim$response_type <- "state_dependent"; stim$adaptation_enabled <- TRUE
  } else if (identical(stim$experiment_preset, "feature_tuning")) {
    stim$protocol <- "feature_tuning"; stim$response_type <- "feature_tuned"; stim$adaptation_enabled <- TRUE
  }
  stim$start_s <- max(0, safe_num(stim$start_s, 5))
  stim$duration_s <- max(0, safe_num(stim$duration_s, 0.05))
  stim$n_stimuli <- max(1L, as.integer(round(safe_num(stim$n_stimuli, 5))))
  stim$inter_stimulus_interval_s <- max(0.001, safe_num(stim$inter_stimulus_interval_s, 5))
  stim$paired_pulse_interval_s <- max(0.001, safe_num(stim$paired_pulse_interval_s, 0.5))
  stim$strength <- min(1, max(0, safe_num(stim$strength, 0.7)))
  stim$strength_end <- min(1, max(0, safe_num(stim$strength_end, stim$strength)))
  stim$strength_jitter <- max(0, safe_num(stim$strength_jitter, 0.0))
  stim$deviant_probability <- min(0.95, max(0.01, safe_num(stim$deviant_probability, 0.2)))
  stim$deviant_strength <- min(1, max(0, safe_num(stim$deviant_strength, 0.9)))
  stim$manual_times <- as.character(value_or(stim$manual_times, ""))[1]
  stim$manual_strengths <- as.character(value_or(stim$manual_strengths, ""))[1]
  stim$feature_modality <- tolower(as.character(value_or(stim$feature_modality, "orientation"))[1])
  if (identical(stim$feature_modality, "place_field_2d") || identical(stim$feature_modality, "place_field") || identical(stim$feature_modality, "xy_position")) {
    stim$feature_modality <- "spatial_2d"
  }
  if (!stim$feature_modality %in% c("orientation", "motion_direction", "auditory_frequency", "color_hue", "spatial_position", "tactile_location", "spatial_2d")) {
    stim$feature_modality <- "orientation"
  }
  stim$feature_values <- as.character(value_or(stim$feature_values, ""))[1]
  stim$feature_xy_values <- as.character(value_or(stim$feature_xy_values, "0,0; 25,0; 0,25; -25,0; 0,-25; 25,25; -25,25; 25,-25; -25,-25"))[1]
  stim$place_field_x_min <- safe_num(stim$place_field_x_min, -50)
  stim$place_field_x_max <- safe_num(stim$place_field_x_max, 50)
  stim$place_field_y_min <- safe_num(stim$place_field_y_min, -50)
  stim$place_field_y_max <- safe_num(stim$place_field_y_max, 50)
  if (!is.finite(stim$place_field_x_min) || !is.finite(stim$place_field_x_max) || stim$place_field_x_max <= stim$place_field_x_min) { stim$place_field_x_min <- -50; stim$place_field_x_max <- 50 }
  if (!is.finite(stim$place_field_y_min) || !is.finite(stim$place_field_y_max) || stim$place_field_y_max <= stim$place_field_y_min) { stim$place_field_y_min <- -50; stim$place_field_y_max <- 50 }
  stim$place_field_center_x <- safe_num(stim$place_field_center_x, 0)
  stim$place_field_center_y <- safe_num(stim$place_field_center_y, 0)
  stim$place_field_width <- max(1e-6, safe_num(stim$place_field_width, 18))
  stim$place_field_radius <- max(stim$place_field_width, safe_num(stim$place_field_radius, 2.5 * stim$place_field_width))
  stim$preferred_feature <- safe_num(stim$preferred_feature, if (identical(stim$feature_modality, "auditory_frequency")) 4000 else 15)
  stim$null_feature <- safe_num(stim$null_feature, if (identical(stim$feature_modality, "auditory_frequency")) 12000 else 90)
  raw_feature_period <- safe_num(stim$feature_period, NA_real_)
  stim$feature_period <- if (stim$feature_modality %in% c("orientation", "motion_direction", "color_hue")) {
    default_period <- if (identical(stim$feature_modality, "orientation")) 180 else 360
    if (is.finite(raw_feature_period) && raw_feature_period > 0) raw_feature_period else default_period
  } else {
    NA_real_
  }
  stim$feature_tuning_width <- max(1e-6, safe_num(stim$feature_tuning_width, if (identical(stim$feature_modality, "auditory_frequency")) 0.45 else if (is_2d_feature_modality(stim$feature_modality)) stim$place_field_width else 25))
  stim$feature_suppression_width <- max(1e-6, safe_num(stim$feature_suppression_width, stim$feature_tuning_width))
  stim$feature_min_gain <- min(1, max(0, safe_num(stim$feature_min_gain, 0.05)))
  # Compatibility no-op: clean feature-response semantics treats off-target,
  # subthreshold, nonresponsive, and no_response-kernel rows as baseline/no evoked
  # response rather than weak response attempts.
  stim$feature_neutral_response_probability <- 0
  stim$feature_population_mode <- as.character(value_or(stim$feature_population_mode, "coverage_balanced_population"))[1]
  if (identical(stim$feature_population_mode, "heterogeneous_population")) {
    stim$feature_population_mode <- "random_heterogeneous_population"
  }
  if (!stim$feature_population_mode %in% FEATURE_TUNING_POPULATION_MODES) stim$feature_population_mode <- "coverage_balanced_population"
  stim$feature_responsive_fraction <- min(1, max(0, safe_num(stim$feature_responsive_fraction, 0.35)))
  stim$feature_suppressive_fraction <- min(1, max(0, safe_num(stim$feature_suppressive_fraction, 0.10)))
  stim$feature_biphasic_fraction <- min(1, max(0, safe_num(stim$feature_biphasic_fraction, 0.05)))
  stim$feature_response_threshold <- min(1, max(0, safe_num(stim$feature_response_threshold, 0.35)))
  # Compatibility no-op; see feature_neutral_response_probability above.
  stim$feature_weak_response_probability <- 0
  forced_states <- as.character(value_or(stim$pre_stimulus_state_sequence, character(0)))
  forced_states <- forced_states[nzchar(forced_states) & forced_states %in% VALID_PRE_STIMULUS_STATES]
  stim$pre_stimulus_state_sequence <- forced_states
  stim$feature_population_jitter <- max(0, safe_num(stim$feature_population_jitter, 0.25))
  stim$feature_unit_max_gain <- min(5, max(0, safe_num(stim$feature_unit_max_gain, 1.0)))
  stim$feature_unit_response_reliability <- min(1, max(0, safe_num(stim$feature_unit_response_reliability, 1.0)))
  stim$feature_target_unit <- max(1L, as.integer(round(safe_num(stim$feature_target_unit, 1))))
  stim$unit_id <- as.integer(round(safe_num(stim$unit_id, NA_real_)))
  stim$unit_tuning_mode <- as.character(value_or(stim$unit_tuning_mode, stim$feature_population_mode))[1]
  stim$unit_class <- as.character(value_or(stim$unit_class, "tuned_excitatory"))[1]
  if (!stim$unit_class %in% FEATURE_UNIT_CLASSES) stim$unit_class <- "tuned_excitatory"
  stim$unit_responsive <- !identical(stim$unit_responsive, FALSE)
  stim$unit_max_response_gain <- min(5, max(0, safe_num(stim$unit_max_response_gain, stim$feature_unit_max_gain)))
  stim$unit_response_threshold <- min(1, max(0, safe_num(stim$unit_response_threshold, stim$feature_response_threshold)))
  stim$unit_response_reliability <- min(1, max(0, safe_num(stim$unit_response_reliability, stim$feature_unit_response_reliability)))
  stim$feature_preferred_response <- as.character(value_or(stim$feature_preferred_response, "excitatory_burst"))[1]
  if (!stim$feature_preferred_response %in% c("excitatory_burst", "biphasic", "suppressive_pause", "pause_rebound", "no_response")) stim$feature_preferred_response <- "excitatory_burst"
  stim$feature_null_response <- as.character(value_or(stim$feature_null_response, "no_response"))[1]
  if (!stim$feature_null_response %in% c("suppressive_pause", "pause_rebound", "no_response", "excitatory_burst", "biphasic")) stim$feature_null_response <- "no_response"
  stim$response_latency_median_s <- max(0.001, safe_num(stim$response_latency_median_s, 0.08))
  stim$response_latency_sdlog <- max(0, safe_num(stim$response_latency_sdlog, 0.25))
  stim$response_probability <- min(1, max(0, safe_num(stim$response_probability, 1.0)))
  stim$max_evoked_bursts <- max(0L, as.integer(round(safe_num(stim$max_evoked_bursts, 3))))
  stim$burst_lambda_base <- max(0, safe_num(stim$burst_lambda_base, 0.3))
  stim$burst_lambda_strength <- max(0, safe_num(stim$burst_lambda_strength, 2.5))
  stim$evoked_burst_spike_min <- max(BURST_MIN_BOUNDARY_SPIKES, as.integer(round(safe_num(stim$evoked_burst_spike_min, 3))))
  stim$evoked_burst_spike_max <- max(stim$evoked_burst_spike_min, as.integer(round(safe_num(stim$evoked_burst_spike_max, 7))))
  stim$pause_duration_min_s <- max(0.001, safe_num(stim$pause_duration_min_s, 0.5))
  stim$pause_duration_max_s <- max(stim$pause_duration_min_s, safe_num(stim$pause_duration_max_s, 1.4))
  stim$pause_duration_cv <- min(2, max(0, safe_num(stim$pause_duration_cv, 0.35)))
  stim$post_burst_pause_probability <- min(1, max(0, safe_num(stim$post_burst_pause_probability, 0.25)))
  stim$rebound_probability <- min(1, max(0, safe_num(stim$rebound_probability, 0.35)))
  stim$response_window_s <- max(0.01, safe_num(stim$response_window_s, 1.5))
  stim$pre_stimulus_window_s <- max(0, safe_num(stim$pre_stimulus_window_s, 1.0))
  stim$baseline_recovery_enabled <- isTRUE(value_or(stim$baseline_recovery_enabled, TRUE))
  stim$baseline_recovery_mode <- as.character(value_or(stim$baseline_recovery_mode, "Noisy"))[1]
  if (!stim$baseline_recovery_mode %in% c("Noisy", "Tonic", "ratio")) stim$baseline_recovery_mode <- "Noisy"
  stim$pre_stimulus_guard_s <- max(0, safe_num(stim$pre_stimulus_guard_s, 0.02))
  stim$burst_load_weight <- max(0, safe_num(stim$burst_load_weight, 1.0))
  stim$pause_load_weight <- max(0, safe_num(stim$pause_load_weight, 1.0))
  stim$reference_pause_s <- max(0.001, safe_num(stim$reference_pause_s, 1.0))
  stim$adaptation_enabled <- isTRUE(stim$adaptation_enabled)
  stim$adaptation_increment <- max(0, safe_num(stim$adaptation_increment, 0.35))
  stim$adaptation_tau_s <- max(0.001, safe_num(stim$adaptation_tau_s, 12))
  stim$adaptation_source <- tolower(as.character(value_or(stim$adaptation_source, "mixed"))[1])
  if (!stim$adaptation_source %in% c("stimulus", "response", "mixed")) stim$adaptation_source <- "mixed"
  stim$response_floor <- min(1, max(0, safe_num(stim$response_floor, 0.15)))
  stim$force_mixed_oddball <- isTRUE(stim$force_mixed_oddball)
  stim$channel <- as.character(value_or(stim$channel, "A"))[1]
  stim
}

parse_numeric_csv <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  vals <- unlist(strsplit(as.character(x)[1], "[,;\\s]+"), use.names = FALSE)
  vals <- vals[nzchar(vals)]
  nums <- suppressWarnings(as.numeric(vals))
  nums[is.finite(nums)]
}

default_feature_values <- function(modality) {
  modality <- tolower(as.character(value_or(modality, "orientation"))[1])
  if (identical(modality, "auditory_frequency")) {
    c(1000, 2000, 4000, 8000, 12000, 16000, 4000, 12000)
  } else if (is_2d_feature_modality(modality)) {
    seq_len(nrow(default_feature_xy_values()))
  } else if (identical(modality, "spatial_position")) {
    c(-40, -20, 0, 20, 40, 0, -30, 30)
  } else if (identical(modality, "tactile_location")) {
    c(1, 2, 3, 4, 5, 1, 3, 5)
  } else if (modality %in% c("motion_direction", "color_hue")) {
    c(0, 45, 90, 135, 180, 225, 270, 315)
  } else {
    c(15, 45, 90, 135, 180, 225, 270, 315)
  }
}

feature_distance <- function(value, target, period = NA_real_, modality = "orientation") {
  value <- as.numeric(value)
  target <- as.numeric(target)
  if (!is.finite(value) || !is.finite(target)) return(NA_real_)
  modality <- tolower(as.character(value_or(modality, "orientation"))[1])
  if (identical(modality, "auditory_frequency")) {
    if (value <= 0 || target <= 0) return(NA_real_)
    return(abs(log2(value / target)))
  }
  if (is.finite(period) && period > 0) {
    delta <- abs((value - target) %% period)
    return(min(delta, period - delta))
  }
  abs(value - target)
}


is_2d_feature_modality <- function(modality) {
  modality <- tolower(as.character(value_or(modality, "orientation"))[1])
  modality %in% c("spatial_2d", "place_field_2d", "place_field", "xy_position")
}

parse_xy_pairs <- function(x) {
  if (is.null(x) || length(x) == 0) return(data.frame(x = numeric(0), y = numeric(0)))
  txt <- as.character(x)[1]
  if (!nzchar(txt)) return(data.frame(x = numeric(0), y = numeric(0)))
  # Accept semicolon/newline/pipe-separated pairs such as "0,0; 25,10; -10 5".
  chunks <- unlist(strsplit(txt, "[;|\\n]+"), use.names = FALSE)
  chunks <- trimws(chunks)
  chunks <- chunks[nzchar(chunks)]
  if (length(chunks) == 0) return(data.frame(x = numeric(0), y = numeric(0)))
  xs <- numeric(0); ys <- numeric(0)
  for (ch in chunks) {
    vals <- suppressWarnings(as.numeric(unlist(strsplit(ch, "[,\\s]+"), use.names = FALSE)))
    vals <- vals[is.finite(vals)]
    if (length(vals) >= 2) {
      xs <- c(xs, vals[1]); ys <- c(ys, vals[2])
    }
  }
  data.frame(x = xs, y = ys)
}

default_feature_xy_values <- function(stim = NULL) {
  env <- if (is.null(stim)) list() else stim
  xmin <- safe_num(env$place_field_x_min, -50)
  xmax <- safe_num(env$place_field_x_max, 50)
  ymin <- safe_num(env$place_field_y_min, -50)
  ymax <- safe_num(env$place_field_y_max, 50)
  cx <- (xmin + xmax) / 2; cy <- (ymin + ymax) / 2
  span_x <- max(1e-6, xmax - xmin); span_y <- max(1e-6, ymax - ymin)
  dx <- span_x / 4; dy <- span_y / 4
  pts <- data.frame(
    x = c(cx, cx + dx, cx, cx - dx, cx, cx + dx, cx - dx, cx + dx, cx - dx),
    y = c(cy, cy, cy + dy, cy, cy - dy, cy + dy, cy + dy, cy - dy, cy - dy)
  )
  pts$x <- pmin(xmax, pmax(xmin, pts$x))
  pts$y <- pmin(ymax, pmax(ymin, pts$y))
  pts
}

coerce_feature_xy_values <- function(feature_values, stim) {
  if (is.data.frame(feature_values) && all(c("x", "y") %in% names(feature_values))) {
    out <- data.frame(x = as.numeric(feature_values$x), y = as.numeric(feature_values$y))
    out <- out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
    return(out)
  }
  if (is.list(feature_values) && all(c("x", "y") %in% names(feature_values))) {
    out <- data.frame(x = as.numeric(feature_values$x), y = as.numeric(feature_values$y))
    out <- out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
    return(out)
  }
  out <- parse_xy_pairs(value_or(stim$feature_xy_values, ""))
  if (nrow(out) == 0) out <- default_feature_xy_values(stim)
  out
}

sample_place_field_centers <- function(stim, n_train) {
  n_train <- max(1L, as.integer(n_train))
  mode <- as.character(value_or(stim$feature_population_mode, "coverage_balanced_population"))[1]
  xy_grid <- parse_xy_pairs(value_or(stim$feature_xy_values, ""))
  # Coverage-balanced populations are a benchmark design option: they deliberately
  # align some place-field centers with the tested stimulus positions so every
  # condition has a chance to evoke responses. Random heterogeneous populations
  # instead sample centers from the environment and are more appropriate when the
  # goal is naturalistic population heterogeneity.
  if (mode %in% c("coverage_balanced_population") && nrow(xy_grid) > 0) {
    xy_grid <- xy_grid[rep(seq_len(nrow(xy_grid)), length.out = n_train), , drop = FALSE]
    rownames(xy_grid) <- NULL
    return(xy_grid)
  }
  xmin <- safe_num(stim$place_field_x_min, -50)
  xmax <- safe_num(stim$place_field_x_max, 50)
  ymin <- safe_num(stim$place_field_y_min, -50)
  ymax <- safe_num(stim$place_field_y_max, 50)
  if (!is.finite(xmin) || !is.finite(xmax) || xmax <= xmin) { xmin <- -50; xmax <- 50 }
  if (!is.finite(ymin) || !is.finite(ymax) || ymax <= ymin) { ymin <- -50; ymax <- 50 }
  data.frame(
    x = stats::runif(n_train, xmin, xmax),
    y = stats::runif(n_train, ymin, ymax)
  )
}


place_field_gaussian_drive <- function(x, y, center_x, center_y, width) {
  x <- as.numeric(x); y <- as.numeric(y)
  center_x <- as.numeric(center_x)[1]; center_y <- as.numeric(center_y)[1]
  width <- max(1e-6, as.numeric(width)[1])
  d <- sqrt((x - center_x)^2 + (y - center_y)^2)
  list(distance = d, drive = exp(-0.5 * (d / width)^2))
}


feature_response_kernel_for_class <- function(response_class, stim) {
  cls <- as.character(response_class)
  out <- rep("no_response", length(cls))
  preferred_kernel <- as.character(value_or(stim$feature_preferred_response, "excitatory_burst"))[1]
  null_kernel <- as.character(value_or(stim$feature_null_response, "no_response"))[1]
  allowed <- c("excitatory_burst", "biphasic", "suppressive_pause", "pause_rebound", "no_response")
  if (!preferred_kernel %in% allowed) preferred_kernel <- "excitatory_burst"
  if (!null_kernel %in% allowed) null_kernel <- "no_response"
  out[cls == "preferred_excitatory"] <- preferred_kernel
  out[cls == "preferred_biphasic"] <- "biphasic"
  out[cls == "preferred_suppressive"] <- "suppressive_pause"
  out[cls == "null_suppressive"] <- null_kernel
  out[!out %in% allowed] <- "no_response"
  out
}

feature_tuning_value_metrics <- function(feature_values, stim) {
  modality <- tolower(as.character(value_or(stim$feature_modality, "orientation"))[1])
  xy_values <- NULL
  if (is_2d_feature_modality(modality)) {
    xy_values <- coerce_feature_xy_values(feature_values, stim)
    n <- nrow(xy_values)
  } else {
    n <- length(feature_values)
  }
  unit_id <- as.integer(round(safe_num(stim$unit_id, NA_real_)))
  unit_mode <- as.character(value_or(stim$unit_tuning_mode, value_or(stim$feature_population_mode, "same_unit_trials")))[1]
  unit_class <- as.character(value_or(stim$unit_class, "tuned_excitatory"))[1]
  if (!unit_class %in% FEATURE_UNIT_CLASSES) unit_class <- "tuned_excitatory"
  unit_responsive <- !identical(stim$unit_responsive, FALSE) && !identical(unit_class, "nonresponsive")
  if (n == 0) {
    return(data.frame(
      Feature_Modality = character(0), Stimulus_Feature_Value = numeric(0),
      Stimulus_Position_X = numeric(0), Stimulus_Position_Y = numeric(0),
      Preferred_Feature_Value = numeric(0), Null_Feature_Value = numeric(0),
      Feature_Distance_To_Preferred = numeric(0), Feature_Distance_To_Null = numeric(0),
      Feature_Excitation = numeric(0), Feature_Suppression = numeric(0), Feature_Selectivity = numeric(0),
      Feature_Response_Class = character(0), External_Strength = numeric(0), Feature_Drive = numeric(0),
      Feature_Matched = logical(0), Drive_Above_Threshold = logical(0), Response_Kernel = character(0),
      Response_Eligible = logical(0), Feature_Response_Eligible = logical(0), Feature_Response_Reason = character(0),
      Unit_ID = integer(0), Unit_Tuning_Mode = character(0), Unit_Class = character(0), Unit_Responsive = logical(0),
      Unit_Preferred_Feature_Value = numeric(0), Unit_Null_Feature_Value = numeric(0),
      Unit_Place_Field_Center_X = numeric(0), Unit_Place_Field_Center_Y = numeric(0),
      Unit_Place_Field_Width = numeric(0), Unit_Place_Field_Radius = numeric(0),
      Place_Field_Distance = numeric(0), Place_Field_Drive = numeric(0),
      Unit_Tuning_Width = numeric(0),
      Unit_Suppression_Width = numeric(0), Unit_Max_Response_Gain = numeric(0), Unit_Response_Threshold = numeric(0),
      Unit_Response_Reliability = numeric(0), stringsAsFactors = FALSE
    ))
  }
  if (is_2d_feature_modality(modality)) {
    center_x <- safe_num(stim$place_field_center_x, 0)
    center_y <- safe_num(stim$place_field_center_y, 0)
    width_exc <- max(1e-6, safe_num(stim$place_field_width, safe_num(stim$feature_tuning_width, 18)))
    radius <- max(width_exc, safe_num(stim$place_field_radius, 2.5 * width_exc))
    response_threshold <- min(1, max(0, safe_num(stim$unit_response_threshold, safe_num(stim$feature_response_threshold, 0.35))))
    max_gain <- min(5, max(0, safe_num(stim$unit_max_response_gain, safe_num(stim$feature_unit_max_gain, 1))))
    reliability <- min(1, max(0, safe_num(stim$unit_response_reliability, safe_num(stim$feature_unit_response_reliability, 1))))
    pf <- place_field_gaussian_drive(xy_values$x, xy_values$y, center_x, center_y, width_exc)
    d_pref <- pf$distance
    excitation <- pf$drive
    d_null <- rep(NA_real_, n)
    suppression <- if (unit_class %in% c("tuned_suppressive")) excitation else rep(0, n)
    selectivity <- excitation
    raw_drive <- pmin(1, pmax(0, excitation * max_gain))
    response_class <- rep("neutral_baseline", n)
    if (!unit_responsive) {
      response_class[] <- "nonresponsive"
    } else if (identical(unit_class, "tuned_excitatory")) {
      response_class <- ifelse(raw_drive >= response_threshold, "preferred_excitatory", "neutral_baseline")
    } else if (identical(unit_class, "tuned_suppressive")) {
      response_class <- ifelse(raw_drive >= response_threshold, "preferred_suppressive", "neutral_baseline")
    } else if (identical(unit_class, "tuned_biphasic")) {
      response_class <- ifelse(raw_drive >= response_threshold, "preferred_biphasic", "neutral_baseline")
    } else {
      response_class[] <- "nonresponsive"
    }
    response_kernel <- feature_response_kernel_for_class(response_class, stim)
    drive <- ifelse(response_class == "neutral_baseline", safe_num(stim$feature_min_gain, 0), raw_drive)
    drive <- pmin(1, pmax(0, drive))
    place_responsive_classes <- c("preferred_excitatory", "preferred_suppressive", "preferred_biphasic")
    unit_ok <- rep(isTRUE(unit_responsive), n)
    feature_matched <- unit_ok & d_pref <= radius
    drive_above_threshold <- unit_ok & raw_drive >= response_threshold
    response_eligible <- feature_matched & drive_above_threshold & response_class %in% place_responsive_classes & response_kernel != "no_response"
    reason <- rep("inside_place_field_neutral", n)
    reason[!unit_ok] <- "unit_nonresponsive"
    reason[unit_ok & d_pref > radius] <- "outside_place_field_radius"
    reason[unit_ok & d_pref <= radius & !drive_above_threshold] <- "subthreshold_place_field_drive"
    reason[unit_ok & d_pref <= radius & drive_above_threshold & response_kernel == "no_response" & response_class %in% place_responsive_classes] <- "place_field_no_response_kernel"
    reason[response_eligible] <- "place_field_matched"
    return(data.frame(
      Feature_Modality = rep(modality, n),
      Stimulus_Feature_Value = rep(NA_real_, n),
      Stimulus_Position_X = xy_values$x,
      Stimulus_Position_Y = xy_values$y,
      Preferred_Feature_Value = rep(NA_real_, n),
      Null_Feature_Value = rep(NA_real_, n),
      Feature_Distance_To_Preferred = d_pref,
      Feature_Distance_To_Null = d_null,
      Feature_Excitation = excitation,
      Feature_Suppression = suppression,
      Feature_Selectivity = selectivity,
      Feature_Response_Class = response_class,
      External_Strength = rep(safe_num(stim$strength, NA_real_), n),
      Feature_Drive = drive,
      Feature_Matched = feature_matched,
      Drive_Above_Threshold = drive_above_threshold,
      Response_Kernel = response_kernel,
      Response_Eligible = response_eligible,
      Feature_Response_Eligible = response_eligible,
      Feature_Response_Reason = reason,
      Unit_ID = rep(unit_id, n),
      Unit_Tuning_Mode = rep(unit_mode, n),
      Unit_Class = rep(unit_class, n),
      Unit_Responsive = rep(unit_responsive, n),
      Unit_Preferred_Feature_Value = rep(NA_real_, n),
      Unit_Null_Feature_Value = rep(NA_real_, n),
      Unit_Place_Field_Center_X = rep(center_x, n),
      Unit_Place_Field_Center_Y = rep(center_y, n),
      Unit_Place_Field_Width = rep(width_exc, n),
      Unit_Place_Field_Radius = rep(radius, n),
      Place_Field_Distance = d_pref,
      Place_Field_Drive = excitation,
      Unit_Tuning_Width = rep(width_exc, n),
      Unit_Suppression_Width = rep(width_exc, n),
      Unit_Max_Response_Gain = rep(max_gain, n),
      Unit_Response_Threshold = rep(response_threshold, n),
      Unit_Response_Reliability = rep(reliability, n),
      stringsAsFactors = FALSE
    ))
  }

  preferred <- safe_num(stim$preferred_feature, NA_real_)
  null <- safe_num(stim$null_feature, NA_real_)
  period <- safe_num(stim$feature_period, NA_real_)
  width_exc <- max(1e-6, safe_num(stim$feature_tuning_width, 25))
  width_sup <- max(1e-6, safe_num(stim$feature_suppression_width, width_exc))
  response_threshold <- min(1, max(0, safe_num(stim$unit_response_threshold, safe_num(stim$feature_response_threshold, 0.35))))
  max_gain <- min(5, max(0, safe_num(stim$unit_max_response_gain, safe_num(stim$feature_unit_max_gain, 1))))
  reliability <- min(1, max(0, safe_num(stim$unit_response_reliability, safe_num(stim$feature_unit_response_reliability, 1))))
  d_pref <- vapply(feature_values, feature_distance, numeric(1), target = preferred, period = period, modality = modality)
  d_null <- vapply(feature_values, feature_distance, numeric(1), target = null, period = period, modality = modality)
  excitation <- exp(-0.5 * (d_pref / width_exc)^2)
  suppression <- exp(-0.5 * (d_null / width_sup)^2)
  selectivity <- excitation - suppression
  margin <- 0.15
  response_class <- rep("neutral_baseline", n)
  if (!unit_responsive) {
    response_class[] <- "nonresponsive"
  } else if (unit_class %in% c("tuned_excitatory")) {
    response_class <- ifelse(selectivity >= margin, "preferred_excitatory",
                             ifelse(selectivity <= -margin, "null_suppressive", "neutral_baseline"))
  } else if (identical(unit_class, "tuned_suppressive")) {
    response_class <- ifelse(excitation >= response_threshold, "preferred_suppressive", "neutral_baseline")
  } else if (identical(unit_class, "tuned_biphasic")) {
    response_class <- ifelse(excitation >= response_threshold, "preferred_biphasic",
                             ifelse(suppression >= response_threshold, "null_suppressive", "neutral_baseline"))
  } else {
    response_class[] <- "nonresponsive"
  }
  response_kernel <- feature_response_kernel_for_class(response_class, stim)
  raw_drive <- ifelse(
    response_class %in% c("preferred_excitatory", "preferred_biphasic", "preferred_suppressive"), excitation,
    ifelse(response_class == "null_suppressive", suppression,
           ifelse(response_class == "neutral_baseline", safe_num(stim$feature_min_gain, 0), 0))
  )
  drive <- pmin(1, pmax(0, raw_drive * max_gain))
  responsive_classes <- c("preferred_excitatory", "null_suppressive", "preferred_suppressive", "preferred_biphasic")
  unit_ok <- rep(isTRUE(unit_responsive), n)
  feature_matched <- unit_ok & response_class %in% responsive_classes
  drive_above_threshold <- feature_matched & drive >= response_threshold
  response_eligible <- feature_matched & drive_above_threshold & response_kernel != "no_response"
  reason <- rep("feature_mismatch_neutral", n)
  reason[!unit_ok] <- "unit_nonresponsive"
  reason[feature_matched & !drive_above_threshold] <- "subthreshold_feature_drive"
  reason[feature_matched & drive_above_threshold & response_kernel == "no_response" & response_class == "null_suppressive"] <- "null_feature_no_response_kernel"
  reason[feature_matched & drive_above_threshold & response_kernel == "no_response" & response_class != "null_suppressive"] <- "preferred_feature_no_response_kernel"
  reason[response_eligible & response_class == "null_suppressive"] <- "null_feature_matched"
  reason[response_eligible & response_class != "null_suppressive"] <- "preferred_feature_matched"
  data.frame(
    Feature_Modality = rep(modality, n),
    Stimulus_Feature_Value = as.numeric(feature_values),
    Stimulus_Position_X = rep(NA_real_, n),
    Stimulus_Position_Y = rep(NA_real_, n),
    Preferred_Feature_Value = rep(preferred, n),
    Null_Feature_Value = rep(null, n),
    Feature_Distance_To_Preferred = d_pref,
    Feature_Distance_To_Null = d_null,
    Feature_Excitation = excitation,
    Feature_Suppression = suppression,
    Feature_Selectivity = selectivity,
    Feature_Response_Class = response_class,
    External_Strength = rep(safe_num(stim$strength, NA_real_), n),
    Feature_Drive = drive,
    Feature_Matched = feature_matched,
    Drive_Above_Threshold = drive_above_threshold,
    Response_Kernel = response_kernel,
    Response_Eligible = response_eligible,
    Feature_Response_Eligible = response_eligible,
    Feature_Response_Reason = reason,
    Unit_ID = rep(unit_id, n),
    Unit_Tuning_Mode = rep(unit_mode, n),
    Unit_Class = rep(unit_class, n),
    Unit_Responsive = rep(unit_responsive, n),
    Unit_Preferred_Feature_Value = rep(preferred, n),
    Unit_Null_Feature_Value = rep(null, n),
    Unit_Place_Field_Center_X = rep(NA_real_, n),
    Unit_Place_Field_Center_Y = rep(NA_real_, n),
    Unit_Place_Field_Width = rep(NA_real_, n),
    Unit_Place_Field_Radius = rep(NA_real_, n),
    Place_Field_Distance = rep(NA_real_, n),
    Place_Field_Drive = rep(NA_real_, n),
    Unit_Tuning_Width = rep(width_exc, n),
    Unit_Suppression_Width = rep(width_sup, n),
    Unit_Max_Response_Gain = rep(max_gain, n),
    Unit_Response_Threshold = rep(response_threshold, n),
    Unit_Response_Reliability = rep(reliability, n),
    stringsAsFactors = FALSE
  )
}


feature_values_for_unit_sampling <- function(stim) {
  vals <- parse_numeric_csv(stim$feature_values)
  if (length(vals) == 0) vals <- default_feature_values(stim$feature_modality)
  vals[is.finite(vals)]
}

opponent_feature_value <- function(preferred, stim, feature_values = NULL) {
  preferred <- as.numeric(preferred)[1]
  if (!is.finite(preferred)) return(NA_real_)
  modality <- tolower(as.character(value_or(stim$feature_modality, "orientation"))[1])
  period <- safe_num(stim$feature_period, NA_real_)
  if (is.finite(period) && period > 0 && modality %in% c("orientation", "motion_direction", "color_hue")) {
    return((preferred + period / 2) %% period)
  }
  vals <- if (is.null(feature_values)) feature_values_for_unit_sampling(stim) else feature_values
  vals <- vals[is.finite(vals)]
  if (length(vals) > 0) {
    d <- vapply(vals, feature_distance, numeric(1), target = preferred, period = period, modality = modality)
    return(vals[which.max(d)])
  }
  safe_num(stim$null_feature, preferred)
}

sample_preferred_feature_values <- function(stim, n_train) {
  n_train <- max(1L, as.integer(n_train))
  modality <- tolower(as.character(value_or(stim$feature_modality, "orientation"))[1])
  period <- safe_num(stim$feature_period, NA_real_)
  vals <- feature_values_for_unit_sampling(stim)
  mode <- as.character(value_or(stim$feature_population_mode, "coverage_balanced_population"))[1]
  if (mode %in% c("coverage_balanced_population") && length(vals) > 0) {
    vals <- unique(vals[is.finite(vals)])
    if (identical(modality, "auditory_frequency")) vals <- vals[vals > 0]
    if (length(vals) > 0) return(rep(vals, length.out = n_train))
  }
  if (modality %in% c("orientation", "motion_direction", "color_hue") && is.finite(period) && period > 0) {
    return(stats::runif(n_train, min = 0, max = period))
  }
  if (identical(modality, "auditory_frequency")) {
    vals <- vals[vals > 0]
    if (length(vals) == 0) vals <- default_feature_values("auditory_frequency")
    lo <- min(vals, na.rm = TRUE); hi <- max(vals, na.rm = TRUE)
    if (is.finite(lo) && is.finite(hi) && hi > lo && lo > 0) return(2 ^ stats::runif(n_train, log2(lo), log2(hi)))
  }
  if (length(vals) >= 1) return(sample(vals, n_train, replace = TRUE))
  rep(safe_num(stim$preferred_feature, 0), n_train)
}

unit_lognormal_multiplier <- function(n, cv) {
  cv <- max(0, safe_num(cv, 0))
  if (!is.finite(cv) || cv <= 0) return(rep(1, n))
  sdlog <- sqrt(log1p(cv^2))
  stats::rlnorm(n, meanlog = -0.5 * sdlog^2, sdlog = sdlog)
}

sample_feature_unit_classes <- function(stim, n_train) {
  n_train <- max(1L, as.integer(n_train))
  mode <- as.character(value_or(stim$feature_population_mode, "coverage_balanced_population"))[1]
  if (identical(mode, "same_unit_trials")) return(rep("tuned_excitatory", n_train))
  if (identical(mode, "one_hot_target")) {
    target <- max(1L, min(n_train, as.integer(round(safe_num(stim$feature_target_unit, 1)))))
    cls <- rep("nonresponsive", n_train); cls[target] <- "tuned_excitatory"; return(cls)
  }
  responsive_fraction <- if (identical(mode, "sparse_responsive_population")) min(0.35, stim$feature_responsive_fraction) else stim$feature_responsive_fraction
  p_bi <- min(stim$feature_biphasic_fraction, responsive_fraction)
  p_sup <- min(stim$feature_suppressive_fraction, max(0, responsive_fraction - p_bi))
  p_exc <- max(0, responsive_fraction - p_sup - p_bi)
  p_non <- max(0, 1 - p_exc - p_sup - p_bi)
  probs <- c(tuned_excitatory = p_exc, tuned_suppressive = p_sup, tuned_biphasic = p_bi, nonresponsive = p_non)
  if (sum(probs) <= 0) probs["nonresponsive"] <- 1
  sample(names(probs), n_train, replace = TRUE, prob = probs / sum(probs))
}

make_unit_tuning_profiles <- function(config, n_train, seed = NULL) {
  n_train <- max(1L, as.integer(n_train))
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  if (!is.null(seed)) {
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }
  stim <- sanitize_stimulation_config(config$stimulation)
  if (!isTRUE(stim$enabled) || !(identical(stim$protocol, "feature_tuning") || identical(stim$response_type, "feature_tuned") || identical(stim$experiment_preset, "feature_tuning"))) {
    out <- make_empty_unit_df()
    return(out)
  }
  vals <- feature_values_for_unit_sampling(stim)
  mode <- as.character(value_or(stim$feature_population_mode, "coverage_balanced_population"))[1]
  if (identical(mode, "heterogeneous_population")) mode <- "random_heterogeneous_population"
  if (!mode %in% FEATURE_TUNING_POPULATION_MODES) mode <- "coverage_balanced_population"
  modality <- tolower(as.character(value_or(stim$feature_modality, "orientation"))[1])
  period <- safe_num(stim$feature_period, NA_real_)
  cls <- sample_feature_unit_classes(stim, n_train)
  responsive <- cls != "nonresponsive"
  is_2d <- is_2d_feature_modality(modality)
  place_centers <- if (is_2d) sample_place_field_centers(stim, n_train) else data.frame(x = rep(NA_real_, n_train), y = rep(NA_real_, n_train))
  if (is_2d && (identical(mode, "same_unit_trials") || identical(mode, "one_hot_target"))) {
    place_centers$x[] <- safe_num(stim$place_field_center_x, 0)
    place_centers$y[] <- safe_num(stim$place_field_center_y, 0)
  }
  preferred <- if (is_2d) rep(NA_real_, n_train) else sample_preferred_feature_values(stim, n_train)
  if (!is_2d && (identical(mode, "same_unit_trials") || identical(mode, "one_hot_target"))) {
    preferred[] <- safe_num(stim$preferred_feature, preferred[1])
  }
  null_vals <- if (is_2d) rep(NA_real_, n_train) else vapply(preferred, opponent_feature_value, numeric(1), stim = stim, feature_values = vals)
  if (!is_2d && (identical(mode, "same_unit_trials") || identical(mode, "one_hot_target"))) {
    null_vals[] <- safe_num(stim$null_feature, null_vals[1])
  }
  base_width <- if (is_2d) stim$place_field_width else stim$feature_tuning_width
  base_supp_width <- if (is_2d) stim$place_field_width else stim$feature_suppression_width
  widths <- pmax(1e-6, base_width * unit_lognormal_multiplier(n_train, stim$feature_population_jitter))
  supp_widths <- pmax(1e-6, base_supp_width * unit_lognormal_multiplier(n_train, stim$feature_population_jitter))
  place_radii <- if (is_2d) pmax(widths, stim$place_field_radius * unit_lognormal_multiplier(n_train, stim$feature_population_jitter)) else rep(NA_real_, n_train)
  max_gain <- pmin(5, pmax(0, stim$feature_unit_max_gain * unit_lognormal_multiplier(n_train, stim$feature_population_jitter)))
  max_gain[!responsive] <- 0
  reliability <- rep(stim$feature_unit_response_reliability, n_train); reliability[!responsive] <- 0
  if (identical(mode, "same_unit_trials") && n_train > 1L) {
    # In this mode, each train is a repeated trial from the same neuron. Trial-to-trial
    # variability should arise from stochastic responses and ISI sampling, not from
    # different tuning widths, place-field radii, gains, or reliability values.
    cls[] <- cls[1]
    responsive[] <- responsive[1]
    preferred[] <- preferred[1]
    null_vals[] <- null_vals[1]
    place_centers$x[] <- place_centers$x[1]
    place_centers$y[] <- place_centers$y[1]
    widths[] <- widths[1]
    supp_widths[] <- supp_widths[1]
    place_radii[] <- place_radii[1]
    max_gain[] <- max_gain[1]
    reliability[] <- reliability[1]
  }
  pref_resp <- ifelse(cls == "tuned_suppressive", "suppressive_pause", ifelse(cls == "tuned_biphasic", "biphasic", ifelse(cls == "nonresponsive", "no_response", stim$feature_preferred_response)))
  null_resp <- ifelse(cls == "nonresponsive", "no_response", stim$feature_null_response)
  data.frame(
    Train = seq_len(n_train), Unit_ID = seq_len(n_train), Unit_Tuning_Mode = rep(mode, n_train),
    Unit_Class = cls, Unit_Responsive = responsive, Feature_Modality = rep(modality, n_train),
    Preferred_Feature_Value = preferred, Null_Feature_Value = null_vals,
    Place_Field_Center_X = place_centers$x,
    Place_Field_Center_Y = place_centers$y,
    Place_Field_Width = if (is_2d) widths else rep(NA_real_, n_train),
    Place_Field_Radius = place_radii,
    Environment_X_Min = rep(if (is_2d) stim$place_field_x_min else NA_real_, n_train),
    Environment_X_Max = rep(if (is_2d) stim$place_field_x_max else NA_real_, n_train),
    Environment_Y_Min = rep(if (is_2d) stim$place_field_y_min else NA_real_, n_train),
    Environment_Y_Max = rep(if (is_2d) stim$place_field_y_max else NA_real_, n_train),
    Feature_Period = rep(period, n_train),
    Feature_Tuning_Width = widths, Feature_Suppression_Width = supp_widths,
    Feature_Response_Threshold = rep(stim$feature_response_threshold, n_train),
    Feature_Max_Response_Gain = max_gain, Feature_Response_Reliability = reliability,
    Preferred_Response_Type = pref_resp, Null_Response_Type = null_resp,
    Population_Mode = rep(mode, n_train), Profile_Source = rep(ifelse(identical(mode, "same_unit_trials"), "shared_unit_profile", "unit_specific_profile"), n_train),
    stringsAsFactors = FALSE
  )
}

apply_unit_profile_to_config <- function(config, unit_profile) {
  if (is.null(unit_profile) || nrow(unit_profile) == 0 || is.null(config$stimulation)) return(config)
  cfg <- config; stim <- cfg$stimulation
  stim$unit_id <- as.integer(unit_profile$Unit_ID[1])
  stim$unit_tuning_mode <- as.character(unit_profile$Unit_Tuning_Mode[1])
  stim$unit_class <- as.character(unit_profile$Unit_Class[1])
  stim$unit_responsive <- isTRUE(unit_profile$Unit_Responsive[1])
  stim$preferred_feature <- as.numeric(unit_profile$Preferred_Feature_Value[1])
  stim$null_feature <- as.numeric(unit_profile$Null_Feature_Value[1])
  if ("Place_Field_Center_X" %in% names(unit_profile)) stim$place_field_center_x <- as.numeric(unit_profile$Place_Field_Center_X[1])
  if ("Place_Field_Center_Y" %in% names(unit_profile)) stim$place_field_center_y <- as.numeric(unit_profile$Place_Field_Center_Y[1])
  if ("Place_Field_Width" %in% names(unit_profile) && is.finite(as.numeric(unit_profile$Place_Field_Width[1]))) stim$place_field_width <- as.numeric(unit_profile$Place_Field_Width[1])
  if ("Place_Field_Radius" %in% names(unit_profile) && is.finite(as.numeric(unit_profile$Place_Field_Radius[1]))) stim$place_field_radius <- as.numeric(unit_profile$Place_Field_Radius[1])
  stim$feature_tuning_width <- as.numeric(unit_profile$Feature_Tuning_Width[1])
  stim$feature_suppression_width <- as.numeric(unit_profile$Feature_Suppression_Width[1])
  stim$unit_max_response_gain <- as.numeric(unit_profile$Feature_Max_Response_Gain[1])
  stim$unit_response_threshold <- as.numeric(unit_profile$Feature_Response_Threshold[1])
  stim$unit_response_reliability <- as.numeric(unit_profile$Feature_Response_Reliability[1])
  stim$feature_preferred_response <- as.character(unit_profile$Preferred_Response_Type[1])
  stim$feature_null_response <- as.character(unit_profile$Null_Response_Type[1])
  if (!isTRUE(unit_profile$Unit_Responsive[1])) {
    stim$response_probability <- 0
    stim$feature_preferred_response <- "no_response"
    stim$feature_null_response <- "no_response"
    stim$feature_min_gain <- 0
  }
  cfg$stimulation <- stim
  cfg
}

stimulus_schedule_from_config <- function(config) {
  stim <- sanitize_stimulation_config(config$stimulation)
  if (!isTRUE(stim$enabled)) return(make_empty_stimulus_df())
  total_time <- safe_num(config$total_time, NA_real_)
  if (!is.finite(total_time) || total_time <= 0) return(make_empty_stimulus_df())

  onsets <- numeric(0)
  strengths <- numeric(0)
  is_standard <- logical(0)
  is_deviant <- logical(0)
  stim_type <- character(0)
  feature_values <- numeric(0)
  feature_x <- numeric(0)
  feature_y <- numeric(0)
  external_schedule_used <- FALSE
  ext_sched <- stim$external_schedule
  if (is.data.frame(ext_sched) && nrow(ext_sched) > 0) {
    external_schedule_used <- TRUE
    onsets <- suppressWarnings(as.numeric(ext_sched$Onset_s))
    strengths <- if ("External_Strength" %in% names(ext_sched)) suppressWarnings(as.numeric(ext_sched$External_Strength)) else suppressWarnings(as.numeric(ext_sched$Strength))
    if (length(strengths) != length(onsets)) strengths <- rep(stim$strength, length(onsets))
    is_standard <- if ("Is_Standard" %in% names(ext_sched)) as.logical(ext_sched$Is_Standard) else rep(TRUE, length(onsets))
    is_deviant <- if ("Is_Deviant" %in% names(ext_sched)) as.logical(ext_sched$Is_Deviant) else rep(FALSE, length(onsets))
    stim_type <- if ("Stimulus_Type" %in% names(ext_sched)) as.character(ext_sched$Stimulus_Type) else rep(stim$protocol, length(onsets))
    feature_values <- if ("Stimulus_Feature_Value" %in% names(ext_sched)) suppressWarnings(as.numeric(ext_sched$Stimulus_Feature_Value)) else rep(NA_real_, length(onsets))
    feature_x <- if ("Stimulus_Position_X" %in% names(ext_sched)) suppressWarnings(as.numeric(ext_sched$Stimulus_Position_X)) else rep(NA_real_, length(onsets))
    feature_y <- if ("Stimulus_Position_Y" %in% names(ext_sched)) suppressWarnings(as.numeric(ext_sched$Stimulus_Position_Y)) else rep(NA_real_, length(onsets))
  } else if (identical(stim$protocol, "manual")) {
    onsets <- parse_numeric_csv(stim$manual_times)
    strengths_in <- parse_numeric_csv(stim$manual_strengths)
    if (length(strengths_in) == 0) strengths_in <- rep(stim$strength, length(onsets))
    strengths <- rep(strengths_in, length.out = length(onsets))
    is_standard <- rep(TRUE, length(onsets))
    is_deviant <- rep(FALSE, length(onsets))
    stim_type <- rep("manual", length(onsets))
    feature_values <- rep(NA_real_, length(onsets))
    feature_x <- rep(NA_real_, length(onsets)); feature_y <- rep(NA_real_, length(onsets))
  } else if (identical(stim$protocol, "paired_pulse")) {
    pair_n <- max(1L, ceiling(stim$n_stimuli / 2))
    starts <- stim$start_s + seq_len(pair_n) - 1L
    starts <- stim$start_s + (seq_len(pair_n) - 1L) * stim$inter_stimulus_interval_s
    onsets <- as.numeric(rbind(starts, starts + stim$paired_pulse_interval_s))
    onsets <- head(onsets, stim$n_stimuli)
    strengths <- rep(stim$strength, length(onsets))
    is_standard <- rep(TRUE, length(onsets))
    is_deviant <- rep(FALSE, length(onsets))
    stim_type <- rep(c("paired_pulse_1", "paired_pulse_2"), length.out = length(onsets))
    feature_values <- rep(NA_real_, length(onsets))
    feature_x <- rep(NA_real_, length(onsets)); feature_y <- rep(NA_real_, length(onsets))
  } else {
    onsets <- stim$start_s + (seq_len(stim$n_stimuli) - 1L) * stim$inter_stimulus_interval_s
    if (identical(stim$protocol, "intensity_ramp")) {
      strengths <- if (length(onsets) > 1) seq(stim$strength, stim$strength_end, length.out = length(onsets)) else stim$strength
      is_standard <- rep(TRUE, length(onsets))
      is_deviant <- rep(FALSE, length(onsets))
      stim_type <- rep("intensity_ramp", length(onsets))
      feature_values <- rep(NA_real_, length(onsets))
      feature_x <- rep(NA_real_, length(onsets)); feature_y <- rep(NA_real_, length(onsets))
    } else if (identical(stim$protocol, "oddball")) {
      is_deviant <- stats::runif(length(onsets)) < stim$deviant_probability
      if (isTRUE(stim$force_mixed_oddball) && length(is_deviant) >= 2) {
        if (!any(is_deviant)) is_deviant[length(is_deviant)] <- TRUE
        if (all(is_deviant)) is_deviant[1] <- FALSE
      }
      is_standard <- !is_deviant
      strengths <- ifelse(is_deviant, stim$deviant_strength, stim$strength)
      stim_type <- ifelse(is_deviant, "deviant", "standard")
      feature_values <- rep(NA_real_, length(onsets))
      feature_x <- rep(NA_real_, length(onsets)); feature_y <- rep(NA_real_, length(onsets))
    } else if (identical(stim$protocol, "feature_tuning")) {
      if (is_2d_feature_modality(stim$feature_modality)) {
        feature_xy <- parse_xy_pairs(stim$feature_xy_values)
        if (nrow(feature_xy) == 0) feature_xy <- default_feature_xy_values(stim)
        feature_xy <- feature_xy[rep(seq_len(nrow(feature_xy)), length.out = length(onsets)), , drop = FALSE]
        feature_x <- feature_xy$x
        feature_y <- feature_xy$y
        feature_values <- rep(NA_real_, length(onsets))
        stim_type <- paste0("spatial_2d_", format(feature_x, trim = TRUE, scientific = FALSE), "_", format(feature_y, trim = TRUE, scientific = FALSE))
      } else {
        feature_in <- parse_numeric_csv(stim$feature_values)
        if (length(feature_in) == 0) feature_in <- default_feature_values(stim$feature_modality)
        feature_values <- rep(feature_in, length.out = length(onsets))
        feature_x <- rep(NA_real_, length(onsets)); feature_y <- rep(NA_real_, length(onsets))
        stim_type <- paste0(stim$feature_modality, "_", format(feature_values, trim = TRUE, scientific = FALSE))
      }
      strengths <- rep(stim$strength, length(onsets))
      is_standard <- rep(TRUE, length(onsets))
      is_deviant <- rep(FALSE, length(onsets))
    } else {
      strengths <- rep(stim$strength, length(onsets))
      is_standard <- rep(TRUE, length(onsets))
      is_deviant <- rep(FALSE, length(onsets))
      stim_type <- rep(ifelse(identical(stim$protocol, "repeated"), "repeated", "regular"), length(onsets))
      feature_values <- rep(NA_real_, length(onsets))
      feature_x <- rep(NA_real_, length(onsets)); feature_y <- rep(NA_real_, length(onsets))
    }
  }

  if (length(onsets) == 0) return(make_empty_stimulus_df())
  if (!isTRUE(external_schedule_used) && stim$strength_jitter > 0) {
    strengths <- pmin(1, pmax(0, strengths + stats::rnorm(length(strengths), 0, stim$strength_jitter)))
  }
  keep <- is.finite(onsets) & onsets >= 0 & onsets <= total_time
  onsets <- onsets[keep]
  strengths <- strengths[keep]
  is_standard <- is_standard[keep]
  is_deviant <- is_deviant[keep]
  stim_type <- stim_type[keep]
  feature_values <- feature_values[keep]
  feature_x <- feature_x[keep]
  feature_y <- feature_y[keep]
  if (length(onsets) == 0) return(make_empty_stimulus_df())
  ord <- order(onsets)
  onsets <- onsets[ord]
  strengths <- strengths[ord]
  is_standard <- is_standard[ord]
  is_deviant <- is_deviant[ord]
  stim_type <- stim_type[ord]
  feature_values <- feature_values[ord]
  feature_x <- feature_x[ord]
  feature_y <- feature_y[ord]
  pair_id <- rep(NA_integer_, length(onsets))
  if (identical(stim$protocol, "paired_pulse")) {
    pair_id <- rep(seq_len(ceiling(length(onsets) / 2)), each = 2, length.out = length(onsets))
  }
  feature_metrics <- if (identical(stim$protocol, "feature_tuning")) {
    if (is_2d_feature_modality(stim$feature_modality)) {
      feature_tuning_value_metrics(data.frame(x = feature_x, y = feature_y), stim)
    } else {
      feature_tuning_value_metrics(feature_values, stim)
    }
  } else {
    data.frame(
      Feature_Modality = rep(NA_character_, length(onsets)),
      Stimulus_Feature_Value = rep(NA_real_, length(onsets)),
      Stimulus_Position_X = rep(NA_real_, length(onsets)),
      Stimulus_Position_Y = rep(NA_real_, length(onsets)),
      Preferred_Feature_Value = rep(NA_real_, length(onsets)),
      Null_Feature_Value = rep(NA_real_, length(onsets)),
      Feature_Distance_To_Preferred = rep(NA_real_, length(onsets)),
      Feature_Distance_To_Null = rep(NA_real_, length(onsets)),
      Feature_Excitation = rep(NA_real_, length(onsets)),
      Feature_Suppression = rep(NA_real_, length(onsets)),
      Feature_Selectivity = rep(NA_real_, length(onsets)),
      Feature_Response_Class = rep(NA_character_, length(onsets)),
      External_Strength = rep(NA_real_, length(onsets)),
      Feature_Drive = rep(NA_real_, length(onsets)),
      Feature_Matched = rep(FALSE, length(onsets)),
      Drive_Above_Threshold = rep(FALSE, length(onsets)),
      Response_Kernel = rep(NA_character_, length(onsets)),
      Response_Eligible = rep(FALSE, length(onsets)),
      Feature_Response_Eligible = rep(FALSE, length(onsets)),
      Feature_Response_Reason = rep(NA_character_, length(onsets)),
      Unit_ID = rep(NA_integer_, length(onsets)),
      Unit_Tuning_Mode = rep(NA_character_, length(onsets)),
      Unit_Class = rep(NA_character_, length(onsets)),
      Unit_Responsive = rep(NA, length(onsets)),
      Unit_Preferred_Feature_Value = rep(NA_real_, length(onsets)),
      Unit_Null_Feature_Value = rep(NA_real_, length(onsets)),
      Unit_Place_Field_Center_X = rep(NA_real_, length(onsets)),
      Unit_Place_Field_Center_Y = rep(NA_real_, length(onsets)),
      Unit_Place_Field_Width = rep(NA_real_, length(onsets)),
      Unit_Place_Field_Radius = rep(NA_real_, length(onsets)),
      Place_Field_Distance = rep(NA_real_, length(onsets)),
      Place_Field_Drive = rep(NA_real_, length(onsets)),
      Unit_Tuning_Width = rep(NA_real_, length(onsets)),
      Unit_Suppression_Width = rep(NA_real_, length(onsets)),
      Unit_Max_Response_Gain = rep(NA_real_, length(onsets)),
      Unit_Response_Threshold = rep(NA_real_, length(onsets)),
      Unit_Response_Reliability = rep(NA_real_, length(onsets)),
      stringsAsFactors = FALSE
    )
  }
  external_strengths <- strengths
  if (identical(stim$protocol, "feature_tuning") && nrow(feature_metrics) == length(strengths)) {
    drive <- if ("Feature_Drive" %in% names(feature_metrics)) feature_metrics$Feature_Drive else rep(stim$feature_min_gain, length(strengths))
    strengths <- pmin(1, pmax(0, strengths * pmax(stim$feature_min_gain, drive)))
  }
  data.frame(
    Train = rep(NA_integer_, length(onsets)),
    Stimulus_ID = seq_along(onsets),
    Onset_s = onsets,
    Duration_s = rep(stim$duration_s, length(onsets)),
    Strength = strengths,
    Protocol = rep(stim$protocol, length(onsets)),
    Stimulus_Type = stim_type,
    Channel = rep(stim$channel, length(onsets)),
    Repetition_Index = seq_along(onsets),
    Inter_Stimulus_Interval_s = c(NA_real_, diff(onsets)),
    Pair_ID = pair_id,
    Is_Standard = is_standard,
    Is_Deviant = is_deviant,
    Feature_Modality = feature_metrics$Feature_Modality,
    Stimulus_Feature_Value = feature_metrics$Stimulus_Feature_Value,
    Stimulus_Position_X = feature_metrics$Stimulus_Position_X,
    Stimulus_Position_Y = feature_metrics$Stimulus_Position_Y,
    Preferred_Feature_Value = feature_metrics$Preferred_Feature_Value,
    Null_Feature_Value = feature_metrics$Null_Feature_Value,
    Feature_Distance_To_Preferred = feature_metrics$Feature_Distance_To_Preferred,
    Feature_Distance_To_Null = feature_metrics$Feature_Distance_To_Null,
    Feature_Excitation = feature_metrics$Feature_Excitation,
    Feature_Suppression = feature_metrics$Feature_Suppression,
    Feature_Selectivity = feature_metrics$Feature_Selectivity,
    Feature_Response_Class = feature_metrics$Feature_Response_Class,
    External_Strength = external_strengths,
    Feature_Drive = feature_metrics$Feature_Drive,
    Feature_Matched = feature_metrics$Feature_Matched,
    Drive_Above_Threshold = feature_metrics$Drive_Above_Threshold,
    Response_Kernel = feature_metrics$Response_Kernel,
    Response_Eligible = feature_metrics$Response_Eligible,
    Feature_Response_Eligible = feature_metrics$Feature_Response_Eligible,
    Feature_Response_Reason = feature_metrics$Feature_Response_Reason,
    Unit_ID = feature_metrics$Unit_ID,
    Unit_Tuning_Mode = feature_metrics$Unit_Tuning_Mode,
    Unit_Class = feature_metrics$Unit_Class,
    Unit_Responsive = feature_metrics$Unit_Responsive,
    Unit_Preferred_Feature_Value = feature_metrics$Unit_Preferred_Feature_Value,
    Unit_Null_Feature_Value = feature_metrics$Unit_Null_Feature_Value,
    Unit_Place_Field_Center_X = feature_metrics$Unit_Place_Field_Center_X,
    Unit_Place_Field_Center_Y = feature_metrics$Unit_Place_Field_Center_Y,
    Unit_Place_Field_Width = feature_metrics$Unit_Place_Field_Width,
    Unit_Place_Field_Radius = feature_metrics$Unit_Place_Field_Radius,
    Place_Field_Distance = feature_metrics$Place_Field_Distance,
    Place_Field_Drive = feature_metrics$Place_Field_Drive,
    Unit_Tuning_Width = feature_metrics$Unit_Tuning_Width,
    Unit_Suppression_Width = feature_metrics$Unit_Suppression_Width,
    Unit_Max_Response_Gain = feature_metrics$Unit_Max_Response_Gain,
    Unit_Response_Threshold = feature_metrics$Unit_Response_Threshold,
    Unit_Response_Reliability = feature_metrics$Unit_Response_Reliability,
    stringsAsFactors = FALSE
  )
}


stimulus_external_schedule_from_config <- function(config, seed = NULL) {
  cfg <- config
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  if (!is.null(seed)) {
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(as.integer(seed))
  }
  if (!is.null(cfg$stimulation)) cfg$stimulation$external_schedule <- NULL
  sched <- stimulus_schedule_from_config(cfg)
  if (is.null(sched) || nrow(sched) == 0) return(make_empty_stimulus_df())
  # Store only objective stimulus fields. Unit-specific tuning metrics are recomputed
  # inside each unit-specific simulation. External_Strength is the unmodulated stimulus strength.
  sched$Strength <- if ("External_Strength" %in% names(sched)) sched$External_Strength else sched$Strength
  unit_cols <- grep("^(Unit_|Feature_Response_|Feature_Drive$|Feature_Matched$|Drive_Above_Threshold$|Response_Kernel$|Response_Eligible$|Place_Field_|Preferred_Feature_Value$|Null_Feature_Value$|Feature_Excitation$|Feature_Suppression$|Feature_Selectivity$|Feature_Distance_)", names(sched), value = TRUE)
  for (nm in unit_cols) {
    if (is.logical(sched[[nm]])) sched[[nm]] <- NA else if (is.numeric(sched[[nm]]) || is.integer(sched[[nm]])) sched[[nm]] <- NA_real_ else sched[[nm]] <- NA_character_
  }
  sched
}

stimulus_response_gain <- function(adaptation_state, floor = 0.15) {
  floor <- min(1, max(0, floor))
  floor + (1 - floor) * exp(-max(0, adaptation_state))
}

stimulus_sample_interval <- function(config, label, previous_isi = NA_real_, previous_label = NA_character_, max_attempts = 200L) {
  pat_cfg <- config$patterns[[label]]
  if (is.null(pat_cfg)) return(NA_real_)
  max_attempts <- max(1L, as.integer(max_attempts))
  for (i in seq_len(max_attempts)) {
    segments <- effective_pattern_segments_from_config(config, label, previous_isi, previous_label)
    if (nrow(segments) == 0) return(NA_real_)
    val <- sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
    if (is.finite(val) && val > 0) return(val)
  }
  NA_real_
}


metric_range_ok_global <- function(value, rng) {
  length(rng) == 2 && all(is.finite(rng)) && rng[2] >= rng[1] &&
    is.finite(value) && value >= rng[1] && value <= rng[2]
}

stim_metric_range_ok <- function(value, rng) {
  rng <- suppressWarnings(as.numeric(rng))
  if (length(rng) != 2 || any(!is.finite(rng)) || rng[2] < rng[1]) return(FALSE)
  if (!is.finite(value)) return(FALSE)
  value >= rng[1] && value <= rng[2]
}

sample_closed_int <- function(min_value, max_value) {
  lo <- as.integer(round(safe_num(min_value, NA_real_)))
  hi <- as.integer(round(safe_num(max_value, NA_real_)))
  if (!is.finite(lo) || !is.finite(hi)) return(NA_integer_)
  if (hi < lo) hi <- lo
  if (identical(lo, hi)) return(lo)
  sample(seq.int(lo, hi), size = 1)
}

stimulus_sample_run <- function(config, label, n_intervals, previous_isi = NA_real_, previous_label = NA_character_, max_attempts = 120L) {
  n_intervals <- max(1L, as.integer(n_intervals))
  max_attempts <- max(1L, as.integer(max_attempts))
  for (attempt in seq_len(max_attempts)) {
    vals <- numeric(n_intervals)
    prev_val <- previous_isi
    prev_label <- previous_label
    ok <- TRUE
    for (i in seq_len(n_intervals)) {
      vals[i] <- stimulus_sample_interval(config, label, prev_val, prev_label)
      if (!is.finite(vals[i]) || vals[i] <= 0) { ok <- FALSE; break }
      prev_val <- vals[i]
      prev_label <- label
    }
    if (!ok) next
    if (label %in% c("Tonic", "high_frequency_tonic")) {
      ranges <- config$patterns[[label]]$regularity_ranges
      if (!is.null(ranges)) {
        metrics <- isi_regularity_metrics(vals)
        checks <- c(
          cv = stim_metric_range_ok(metrics$cv, ranges$cv),
          cv2 = stim_metric_range_ok(metrics$cv2, ranges$cv2),
          lv = stim_metric_range_ok(metrics$lv, ranges$lv)
        )
        if (!is.null(ranges$mm)) checks <- c(checks, mm = stim_metric_range_ok(metrics$mm, ranges$mm))
        pass <- all(checks)
        if (!isTRUE(pass)) next
      }
    }
    if (identical(label, "Noisy")) {
      if (isTRUE(config$avoid_noisy_burst_runs)) {
        brng <- effective_interval_range_from_config(config, "Burst")
        if (length(brng) == 2 && all(is.finite(brng)) && length(vals) >= 3) {
          burst_like <- vals >= brng[1] & vals <= brng[2]
          if (any(stats::filter(as.numeric(burst_like), rep(1, 3), sides = 1) >= 3, na.rm = TRUE)) next
        }
      }
      if (noisy_same_zone_pair_violation(vals, config, guard_s = noisy_specificity_from_config(config)$context_guard_s)) next
      metrics <- isi_regularity_metrics(vals)
      spec <- noisy_specificity_from_config(config)
      if (length(vals) >= spec$toniclike_min_isi_count && is.finite(metrics$cv) && is.finite(metrics$cv2) &&
          (metrics$cv < spec$min_run_cv || metrics$cv2 < spec$min_run_cv2)) next
    }
    return(vals)
  }
  rep(NA_real_, n_intervals)
}

simulate_stimulus_sequence_core <- function(config, seed = NULL) {
  if (!is.null(seed) && length(seed) == 1 && is.finite(seed)) set.seed(as.integer(seed))
  stim <- sanitize_stimulation_config(config$stimulation)
  stim_table <- stimulus_schedule_from_config(config)
  if (!isTRUE(stim$enabled) || nrow(stim_table) == 0) {
    cfg <- config; cfg$stimulation$enabled <- FALSE
    out <- simulate_spike_train_core(cfg, seed = seed)
    out$stimuli <- make_empty_stimulus_df()
    out$responses <- make_empty_response_df()
    out$event_epochs <- make_empty_event_epoch_df()
    return(out)
  }

  total_time <- safe_num(config$total_time, 25)
  warnings <- character(0)
  add_warning <- function(msg) warnings <<- unique(c(warnings, msg))
  ratios <- normalize_pattern_ratios(config$ratios)
  if (sum(ratios) <= 0) ratios <- normalize_pattern_ratios(setNames(rep(1, 4), SPIKE_PATTERN_LEVELS))

  current_time <- 0
  has_spike <- FALSE
  spike_times <- numeric(0)
  interval_rows <- list(); interval_i <- 0L
  latency_rows <- list(); latency_i <- 0L
  response_rows <- list(); response_i <- 0L
  last_isi <- NA_real_; last_label <- NA_character_
  last_append_failure_reason <- NA_character_

  absolute_refractory_s <- function() {
    gap <- safe_num(config$inter_event_gap, 0)
    if (!is.finite(gap) || gap < 0) 0 else gap
  }

  capture_stim_state <- function() {
    list(
      current_time = current_time,
      has_spike = has_spike,
      spike_times = spike_times,
      interval_rows = interval_rows,
      interval_i = interval_i,
      latency_rows = latency_rows,
      latency_i = latency_i,
      last_isi = last_isi,
      last_label = last_label,
      last_append_failure_reason = last_append_failure_reason
    )
  }

  restore_stim_state <- function(state) {
    current_time <<- state$current_time
    has_spike <<- state$has_spike
    spike_times <<- state$spike_times
    interval_rows <<- state$interval_rows
    interval_i <<- state$interval_i
    latency_rows <<- state$latency_rows
    latency_i <<- state$latency_i
    last_isi <<- state$last_isi
    last_label <<- state$last_label
    last_append_failure_reason <<- state$last_append_failure_reason
    invisible(TRUE)
  }

  choose_baseline_label <- function(excluded = character(0)) {
    w <- ratios
    if (length(excluded) > 0) w[excluded] <- 0
    if (sum(w) <= 0) return(NA_character_)
    sample(names(w), 1, prob = w)
  }

  baseline_label_candidates <- function(use_recovery_mode = FALSE, excluded = character(0)) {
    base <- names(ratios)[ratios > 0]
    if (length(base) == 0) base <- SPIKE_PATTERN_LEVELS
    if (isTRUE(use_recovery_mode) && isTRUE(stim$baseline_recovery_enabled)) {
      mode <- as.character(stim$baseline_recovery_mode)[1]
      if (mode %in% c("Noisy", "Tonic")) {
        base <- if (identical(mode, "Tonic")) c("Tonic", "Noisy") else mode
      }
    }
    base <- base[base %in% SPIKE_PATTERN_LEVELS]
    if (length(excluded) > 0) base <- setdiff(base, excluded)
    unique(base)
  }

  baseline_min_intervals <- function(label) {
    if (identical(label, "Burst")) return(max(1L, BURST_MIN_BOUNDARY_SPIKES - 1L))
    if (identical(label, "Tonic")) return(max(1L, TONIC_MIN_BOUNDARY_SPIKES - 1L))
    if (identical(label, "high_frequency_tonic")) return(max(1L, HF_TONIC_MIN_BOUNDARY_SPIKES - 1L))
    if (identical(label, "high_frequency_spiking")) return(max(1L, HF_SPIKING_MIN_BOUNDARY_SPIKES - 1L))
    1L
  }

  sample_baseline_run_intervals <- function(label) {
    if (!label %in% SPIKE_PATTERN_LEVELS) return(1L)
    if (identical(label, "Pause")) return(1L)
    pat_cfg <- config$patterns[[label]]
    min_intervals <- baseline_min_intervals(label)
    if (is.null(pat_cfg) || is.null(pat_cfg$spike_count_range)) return(min_intervals)
    n_spikes <- sample_closed_int(pat_cfg$spike_count_range[1], pat_cfg$spike_count_range[2])
    if (!is.finite(n_spikes)) return(min_intervals)
    max(min_intervals, as.integer(n_spikes) - 1L)
  }

  order_baseline_candidates <- function(candidates) {
    candidates <- unique(as.character(candidates[candidates %in% SPIKE_PATTERN_LEVELS]))
    if (length(candidates) == 0) return(candidates)
    first <- candidates[1]
    rest <- setdiff(candidates, first)
    if (length(rest) == 0) return(first)
    w <- ratios[rest]
    w[!is.finite(w) | w < 0] <- 0
    rest <- if (sum(w) > 0) sample(rest, length(rest), prob = w) else sample(rest, length(rest))
    c(first, rest)
  }

  last_non_latency_label <- function() {
    if (interval_i > 0) {
      for (i in rev(seq_len(interval_i))) {
        lab <- as.character(interval_rows[[i]]$ISI_Label[1])
        if (lab %in% SCORABLE_PATTERN_LABELS) return(lab)
      }
    }
    if (last_label %in% SCORABLE_PATTERN_LABELS) last_label else NA_character_
  }

  pre_stimulus_state_label <- function(onset) {
    if (!is.finite(onset) || interval_i <= 0 || stim$pre_stimulus_window_s <= 0) {
      return(last_non_latency_label())
    }
    win_start <- max(0, onset - stim$pre_stimulus_window_s)
    rows <- do.call(rbind, interval_rows)
    labels <- as.character(rows$ISI_Label)
    overlap <- pmax(0, pmin(as.numeric(rows$End_Time_s), onset) - pmax(as.numeric(rows$Start_Time_s), win_start))
    keep <- overlap > 0 & labels %in% SCORABLE_PATTERN_LABELS
    if (!any(keep)) return(last_non_latency_label())
    duration_by_label <- tapply(overlap[keep], labels[keep], sum)
    names(duration_by_label)[which.max(duration_by_label)]
  }

  response_load_value <- function(evoked_spikes, pause_duration) {
    stim$burst_load_weight * max(0, safe_num(evoked_spikes, 0)) +
      stim$pause_load_weight * max(0, safe_num(pause_duration, 0)) / stim$reference_pause_s
  }

  append_latency_spike <- function(time_value, context = "Noisy", model = "stimulus_latency",
                                   stimulus_id = NA_integer_, phase = "latency",
                                   response_type = NA_character_, epoch = "latency",
                                   stimulus_onset = NA_real_,
                                   pattern = "Latency", episode_scope = NULL) {
    if (!is.finite(time_value) || time_value <= current_time || time_value > total_time + 1e-12) return(FALSE)
    start_time <- current_time
    if (isTRUE(has_spike)) {
      dur <- time_value - start_time
      min_gap <- absolute_refractory_s()
      if (is.finite(min_gap) && dur < min_gap - 1e-12) {
        last_append_failure_reason <<- "absolute_refractory_violation"
        return(FALSE)
      }
    }
    current_time <<- time_value
    spike_times <<- c(spike_times, current_time)
    has_spike <<- TRUE
    contains_onset <- is.finite(stimulus_onset) && start_time <= stimulus_onset && current_time >= stimulus_onset
    time_from_onset <- if (is.finite(stimulus_onset)) current_time - stimulus_onset else NA_real_
    scope_value <- if (!is.null(episode_scope) && length(episode_scope) > 0 && nzchar(as.character(episode_scope)[1])) {
      as.character(episode_scope)[1]
    } else if (is.finite(stimulus_onset)) {
      "stimulus_latency"
    } else if (start_time <= 1e-12) {
      "initial_latency"
    } else {
      "stimulus_latency"
    }
    latency_i <<- latency_i + 1L
    latency_rows[[latency_i]] <<- data.frame(
      Episode = NA_integer_, Pattern = as.character(pattern)[1], Episode_Scope = scope_value,
      Latency_Context = context, Latency_Model = model,
      Start = start_time, End = current_time, Episode_Duration = current_time - start_time,
      Core_Start = NA_real_, Core_End = NA_real_, Core_Duration = NA_real_,
      First_Spike_Time = current_time, Last_Spike_Time = current_time,
      N_Spikes = 1L, N_ISIs = 0L, N_Boundary_Spikes = 1L, N_New_Spikes = 1L, N_Shared_Boundary_Spikes = 0L,
      Mean_Within_Episode_ISI = NA_real_, CV_Within_Episode_ISI = NA_real_, Mean_CV2_Within_Episode_ISI = NA_real_, LV_Within_Episode_ISI = NA_real_,
      Core_ISI_Rate_Hz = NA_real_, Episode_Inclusive_Rate_Hz = NA_real_,
      Stimulus_ID = ifelse(is.na(stimulus_id), NA_integer_, as.integer(stimulus_id)),
      Stimulus_Phase = phase,
      Evoked = FALSE,
      Evoked_Response_Type = as.character(value_or(response_type, NA_character_)),
      Response_Epoch = epoch,
      Stimulus_Onset_s = stimulus_onset,
      Time_From_Stimulus_Onset_s = time_from_onset,
      Contains_Stimulus_Onset = contains_onset,
      stringsAsFactors = FALSE
    )
    TRUE
  }

  event_epoch_metadata <- function(label, source, phase, epoch, scope) {
    label <- as.character(value_or(label, NA_character_))[1]
    source <- as.character(value_or(source, NA_character_))[1]
    phase <- as.character(value_or(phase, NA_character_))[1]
    epoch <- as.character(value_or(epoch, NA_character_))[1]
    scope <- as.character(value_or(scope, NA_character_))[1]
    vals <- c(label, source, phase, epoch, scope)
    vals[is.na(vals)] <- ""
    label <- vals[1]; source <- vals[2]; phase <- vals[3]; epoch <- vals[4]; scope <- vals[5]
    epoch_type <- NA_character_
    epoch_source <- NA_character_
    rule <- NA_character_
    if (grepl("^(evoked_burst|early_burst)_[0-9]+$", epoch)) {
      epoch_type <- "evoked_burst_epoch"
      epoch_source <- "stimulation_core"
      rule <- if (grepl("^early_burst_", epoch)) "append_evoked_burst_block_biphasic_early" else "append_evoked_burst_block"
    } else if (grepl("^rebound_burst_[0-9]+$", epoch)) {
      epoch_type <- "rebound_burst_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_rebound_burst_block"
    } else if (epoch %in% c("evoked_suppression", "post_burst_pause", "evoked_pause") ||
               scope %in% c("evoked_suppression", "post_burst_pause", "evoked_pause")) {
      epoch_type <- "suppression_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_evoked_pause_or_suppression"
    } else if (epoch %in% c("baseline_recovery", "post_stimulus_recovery") ||
               scope %in% c("baseline_recovery", "post_stimulus_recovery")) {
      epoch_type <- "recovery_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_baseline_until_recovery"
    } else if (epoch %in% c("no_response_baseline", "failed_response_baseline") ||
               scope %in% c("no_response_baseline", "failed_response_baseline")) {
      epoch_type <- "response_failure_baseline_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_baseline_until_response_failure"
    } else if (epoch %in% c("response_latency", "stimulus_latency") ||
               scope %in% c("response_latency", "stimulus_latency") ||
               identical(label, "Latency")) {
      epoch_type <- "response_latency_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_response_latency_gap"
    } else if (epoch %in% c("interburst_gap") || scope %in% c("interburst_gap") ||
               identical(label, "Interburst_Gap")) {
      epoch_type <- "interburst_gap_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_evoked_interburst_gap"
    } else if (epoch %in% c("stimulus_spanning_gap") || scope %in% c("stimulus_spanning_gap") ||
               identical(label, "Stimulus_Gap")) {
      epoch_type <- "stimulus_spanning_gap_epoch"
      epoch_source <- "stimulation_core"
      rule <- "append_stimulus_spanning_gap"
    }
    list(type = epoch_type, source = epoch_source, rule = rule)
  }

  append_interval <- function(label, duration, source = "baseline", stimulus_id = NA_integer_,
                              phase = "baseline", evoked = FALSE,
                              response_type = NA_character_, epoch = "baseline",
                              scope = NULL, stimulus_onset = NA_real_,
                              max_end_time = total_time) {
    if (!is.finite(duration) || duration <= 0) {
      last_append_failure_reason <<- "invalid_interval_duration"
      return(FALSE)
    }
    hard_end <- min(total_time, if (is.finite(max_end_time)) max_end_time else total_time)
    end_time <- current_time + duration
    if (!is.finite(end_time) || end_time > hard_end + 1e-12) {
      last_append_failure_reason <<- "window_too_short"
      return(FALSE)
    }
    if (!isTRUE(has_spike)) {
      return(append_latency_spike(end_time, context = label, model = source,
                                  stimulus_id = stimulus_id, phase = phase,
                                  response_type = response_type, epoch = epoch,
                                  stimulus_onset = stimulus_onset))
    }
    min_gap <- absolute_refractory_s()
    if (is.finite(min_gap) && duration < min_gap - 1e-12) {
      last_append_failure_reason <<- "absolute_refractory_violation"
      return(FALSE)
    }
    scope_value <- if (!is.null(scope) && length(scope) > 0 && nzchar(as.character(scope)[1])) {
      as.character(scope)[1]
    } else if (identical(label, "Pause")) {
      "pause_isi"
    } else if (identical(label, "Latency")) {
      "stimulus_latency"
    } else if (identical(label, "Interburst_Gap")) {
      "interburst_gap"
    } else if (identical(label, "Stimulus_Gap")) {
      "stimulus_spanning_gap"
    } else {
      "within_episode"
    }
    pat_td <- if (!is.null(config$patterns[[label]])) config$patterns[[label]]$temporal_dependence else NULL
    contains_onset <- is.finite(stimulus_onset) && current_time < stimulus_onset && end_time >= stimulus_onset
    time_from_onset <- if (is.finite(stimulus_onset)) end_time - stimulus_onset else NA_real_
    event_meta <- event_epoch_metadata(label, source, phase, epoch, scope_value)
    interval_i <<- interval_i + 1L
    interval_rows[[interval_i]] <<- data.frame(
      Interval_Seq_ID = interval_i,
      Start_Time_s = current_time,
      End_Time_s = end_time,
      ISI_s = duration,
      ISI_Label = label,
      ISI_Scope = scope_value,
      Is_Manual_Fixed = FALSE,
      Interval_Source = source,
      Run_Position = NA_real_,
      Run_Length = NA_integer_,
      Temporal_Rho = if (!is.null(pat_td$rho)) as.numeric(pat_td$rho) else 0,
      Temporal_Trend = if (!is.null(pat_td$trend)) as.numeric(pat_td$trend) else 0,
      Stimulus_ID = ifelse(is.na(stimulus_id), NA_integer_, as.integer(stimulus_id)),
      Stimulus_Phase = phase,
      Evoked = isTRUE(evoked),
      Evoked_Response_Type = as.character(value_or(response_type, NA_character_)),
      Response_Epoch = epoch,
      Stimulus_Onset_s = stimulus_onset,
      Time_From_Stimulus_Onset_s = time_from_onset,
      Contains_Stimulus_Onset = contains_onset,
      Event_Epoch_Type = event_meta$type,
      Event_Epoch_Source = event_meta$source,
      Event_Epoch_Generation_Rule = event_meta$rule,
      stringsAsFactors = FALSE
    )
    current_time <<- end_time
    spike_times <<- c(spike_times, current_time)
    last_isi <<- duration
    last_label <<- label
    last_append_failure_reason <<- NA_character_
    TRUE
  }

  append_label_run <- function(label, n_intervals, source = "baseline", stimulus_id = NA_integer_,
                               phase = "baseline", evoked = FALSE,
                               response_type = NA_character_, epoch = "baseline",
                               scope = NULL, stimulus_onset = NA_real_,
                               max_end_time = total_time) {
    last_append_failure_reason <<- NA_character_
    n_intervals <- max(1L, as.integer(n_intervals))
    if (forbidden_hf_burst_adjacency(label, last_label)) {
      last_append_failure_reason <<- "forbidden_hf_burst_adjacency"
      return(FALSE)
    }
    vals <- stimulus_sample_run(config, label, n_intervals, last_isi, last_label)
    if (any(!is.finite(vals)) || any(vals <= 0)) {
      last_append_failure_reason <<- paste0("no_feasible_", tolower(label), "_interval")
      return(FALSE)
    }
    min_gap <- absolute_refractory_s()
    if (is.finite(min_gap) && any(vals < min_gap - 1e-12)) {
      last_append_failure_reason <<- "absolute_refractory_violation"
      return(FALSE)
    }
    hard_end <- min(total_time, if (is.finite(max_end_time)) max_end_time else total_time)
    if (current_time + sum(vals) > hard_end + 1e-12) {
      last_append_failure_reason <<- "window_too_short"
      return(FALSE)
    }
    for (j in seq_along(vals)) {
      if (!append_interval(label, vals[j], source = source, stimulus_id = stimulus_id,
                           phase = phase, evoked = evoked, response_type = response_type,
                           epoch = epoch, scope = scope,
                           stimulus_onset = stimulus_onset,
                           max_end_time = max_end_time)) return(FALSE)
    }
    # update run metadata for the most recently appended run
    idx <- seq.int(interval_i - length(vals) + 1L, interval_i)
    for (k in seq_along(idx)) {
      interval_rows[[idx[k]]]$Run_Position <- if (length(idx) > 1L) (k - 1) / (length(idx) - 1) else 0
      interval_rows[[idx[k]]]$Run_Length <- length(idx)
    }
    TRUE
  }

  sample_fit_interval <- function(label, max_duration, previous_isi = last_isi, previous_label = last_label) {
    max_duration <- as.numeric(max_duration)[1]
    if (!is.finite(max_duration) || max_duration <= .Machine$double.eps) return(NA_real_)
    pat_cfg <- config$patterns[[label]]
    if (is.null(pat_cfg)) return(NA_real_)
    if (forbidden_hf_burst_adjacency(label, previous_label)) return(NA_real_)
    segments <- effective_pattern_segments_from_config(config, label, previous_isi, previous_label)
    segments <- intersect_interval_segments(segments, c(.Machine$double.eps, max_duration))
    if (nrow(segments) == 0) return(NA_real_)
    sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
  }

  append_baseline_until <- function(target_time, phase = "pre_stimulus_baseline",
                                    source = "baseline", epoch = "baseline",
                                    use_recovery_mode = FALSE, guard_s = 0,
                                    stimulus_id = NA_integer_,
                                    stimulus_onset = NA_real_,
                                    response_type = NA_character_) {
    if (isTRUE(use_recovery_mode) && !isTRUE(stim$baseline_recovery_enabled)) return(TRUE)
    target_time <- min(total_time, max(current_time, target_time))
    guard_s <- max(0, safe_num(guard_s, 0))
    target_time <- max(current_time, target_time - guard_s)
    attempts <- 0L
    while (current_time < target_time - 1e-9 && attempts < 5000L) {
      attempts <- attempts + 1L
      candidates <- baseline_label_candidates(use_recovery_mode = use_recovery_mode)
      candidates <- order_baseline_candidates(candidates)
      if (length(candidates) == 0) break
      appended <- FALSE
      for (label in candidates) {
        remaining <- target_time - current_time
        if (!isTRUE(has_spike)) {
          val <- sample_fit_interval(label, remaining)
          if (!is.finite(val) || val <= 0 || current_time + val > target_time + 1e-12) next
          appended <- append_latency_spike(
            current_time + val, context = label, model = source,
            stimulus_id = stimulus_id,
            phase = phase, response_type = response_type, epoch = epoch,
            stimulus_onset = stimulus_onset
          )
        } else {
          if (label %in% c("Burst", "Tonic", "high_frequency_tonic", "high_frequency_spiking")) {
            n_intervals <- sample_baseline_run_intervals(label)
            run_state <- capture_stim_state()
            appended <- append_label_run(
              label, n_intervals, source = source, phase = phase, evoked = FALSE,
              response_type = response_type, epoch = epoch, scope = "within_episode",
              stimulus_id = stimulus_id,
              stimulus_onset = stimulus_onset,
              max_end_time = target_time
            )
            if (!isTRUE(appended)) restore_stim_state(run_state)
            if (!isTRUE(appended) && n_intervals > baseline_min_intervals(label)) {
              run_state <- capture_stim_state()
              appended <- append_label_run(
                label, baseline_min_intervals(label), source = source, phase = phase, evoked = FALSE,
                response_type = response_type, epoch = epoch, scope = "within_episode",
                stimulus_id = stimulus_id,
                stimulus_onset = stimulus_onset,
                max_end_time = target_time
              )
              if (!isTRUE(appended)) restore_stim_state(run_state)
            }
          } else {
            val <- sample_fit_interval(label, remaining)
            if (!is.finite(val) || val <= 0 || current_time + val > target_time + 1e-12) next
            scope_value <- if (identical(label, "Pause")) "pause_isi" else "within_episode"
            appended <- append_interval(
              label, val, source = source, phase = phase, evoked = FALSE,
              response_type = response_type, epoch = epoch, scope = scope_value,
              stimulus_id = stimulus_id,
              stimulus_onset = stimulus_onset,
              max_end_time = target_time
            )
          }
        }
        if (isTRUE(appended)) break
      }
      if (!isTRUE(appended)) break
    }
    TRUE
  }

  sample_response_latency <- function() {
    stats::rlnorm(1, meanlog = log(stim$response_latency_median_s), sdlog = stim$response_latency_sdlog)
  }

  sample_unit_mean_lognormal <- function(cv) {
    cv <- max(0, safe_num(cv, 0))
    if (!is.finite(cv) || cv <= 0) return(1)
    sdlog <- sqrt(log1p(cv^2))
    stats::rlnorm(1, meanlog = -0.5 * sdlog^2, sdlog = sdlog)
  }

  effective_response_probability <- function(strength, gain, stim_row = NULL) {
    # Response probability is an explicit trial-level reliability control.
    # In feature-tuned population mode, unit-specific tuning gates whether a unit
    # is eligible for an evoked response to the currently presented external feature.
    if (identical(stim$protocol, "feature_tuning") || identical(stim$response_type, "feature_tuned")) {
      eligible <- if (!is.null(stim_row) && "Response_Eligible" %in% names(stim_row)) {
        isTRUE(stim_row$Response_Eligible[1])
      } else if (!is.null(stim_row) && "Feature_Response_Eligible" %in% names(stim_row)) {
        isTRUE(stim_row$Feature_Response_Eligible[1])
      } else {
        FALSE
      }
      kernel <- if (!is.null(stim_row) && "Response_Kernel" %in% names(stim_row)) as.character(stim_row$Response_Kernel[1]) else NA_character_
      if (!isTRUE(eligible) || identical(kernel, "no_response")) {
        return(0)
      }
      reliability <- if (!is.null(stim_row) && "Unit_Response_Reliability" %in% names(stim_row)) {
        safe_num(stim_row$Unit_Response_Reliability[1], 1)
      } else {
        safe_num(stim$unit_response_reliability, 1)
      }
      return(min(1, max(0, safe_num(stim$response_probability, 1) * reliability)))
    }
    min(1, max(0, safe_num(stim$response_probability, 1)))
  }

  response_type_for_stimulus <- function(stim_row, gain, pre_state = NA_character_) {
    rt <- stim$response_type
    if (identical(stim$protocol, "feature_tuning") || identical(rt, "feature_tuned")) {
      eligible <- if ("Response_Eligible" %in% names(stim_row)) {
        isTRUE(stim_row$Response_Eligible[1])
      } else if ("Feature_Response_Eligible" %in% names(stim_row)) {
        isTRUE(stim_row$Feature_Response_Eligible[1])
      } else {
        FALSE
      }
      kernel <- if ("Response_Kernel" %in% names(stim_row)) as.character(stim_row$Response_Kernel[1]) else NA_character_
      if (!isTRUE(eligible) || !nzchar(kernel) || is.na(kernel) || identical(kernel, "no_response")) return("no_response")
      return(kernel)
    }
    if (!identical(rt, "state_dependent")) return(rt)
    if (identical(pre_state, "Pause")) return("pause_rebound")
    if (pre_state %in% c("Tonic", "Noisy")) {
      if (stim_row$Strength >= 0.55) return("suppressive_pause")
      return("excitatory_burst")
    }
    "biphasic"
  }


  burst_min_interval_for_plan <- function() {
    rng <- effective_interval_range_from_config(config, "Burst")
    val <- if (length(rng) >= 1 && is.finite(rng[1])) rng[1] else absolute_refractory_s()
    max(absolute_refractory_s(), safe_num(val, absolute_refractory_s()))
  }

  pause_min_interval_for_plan <- function() {
    pause_floor <- safe_num(stim$pause_duration_min_s, absolute_refractory_s())
    max(absolute_refractory_s(), pause_floor)
  }

  response_min_duration_for_plan <- function(response_type, include_optional_rebound = FALSE) {
    rt <- as.character(value_or(response_type, "no_response"))[1]
    burst_min <- max(1L, as.integer(stim$evoked_burst_spike_min) - 1L) * burst_min_interval_for_plan()
    pause_min <- pause_min_interval_for_plan()
    if (identical(rt, "excitatory_burst")) return(burst_min)
    if (identical(rt, "suppressive_pause")) return(pause_min)
    if (identical(rt, "biphasic")) return(burst_min + pause_min)
    if (identical(rt, "pause_rebound")) {
      base <- pause_min
      if (isTRUE(include_optional_rebound)) base <- base + burst_min
      return(base)
    }
    Inf
  }

  response_required_components_for_plan <- function(response_type) {
    rt <- as.character(value_or(response_type, "no_response"))[1]
    if (identical(rt, "excitatory_burst")) return("latency+burst")
    if (identical(rt, "suppressive_pause")) return("latency+suppression")
    if (identical(rt, "biphasic")) return("latency+burst+suppression")
    if (identical(rt, "pause_rebound")) return("latency+suppression(+rebound)")
    "none"
  }

  plan_stimulus_response <- function(onset, planned_response_end, response_type, sampled_latency) {
    rt <- as.character(value_or(response_type, "no_response"))[1]
    min_gap <- absolute_refractory_s()
    out <- list(
      feasible = FALSE,
      start = NA_real_,
      end = planned_response_end,
      latency = sampled_latency,
      min_duration = NA_real_,
      required_components = response_required_components_for_plan(rt),
      failure_reason = "response_type_no_response"
    )
    if (!rt %in% c("excitatory_burst", "suppressive_pause", "biphasic", "pause_rebound")) return(out)
    if (!is.finite(onset) || !is.finite(planned_response_end) || planned_response_end <= onset) {
      out$failure_reason <- "response_window_not_positive"
      return(out)
    }
    latency <- safe_num(sampled_latency, 0)
    if (!is.finite(latency) || latency < 0) latency <- 0
    response_start <- onset + latency
    if (isTRUE(has_spike)) {
      response_start <- max(response_start, current_time + min_gap)
    } else {
      response_start <- max(response_start, current_time)
    }
    out$start <- response_start
    out$latency <- response_start - onset
    if (!is.finite(response_start) || response_start > planned_response_end + 1e-12) {
      out$failure_reason <- "response_latency_refractory_window_too_short"
      return(out)
    }
    min_required <- response_min_duration_for_plan(rt)
    out$min_duration <- min_required
    if (!is.finite(min_required)) {
      out$failure_reason <- "response_min_duration_not_finite"
      return(out)
    }
    available <- planned_response_end - response_start
    if (!is.finite(available) || available < min_required - 1e-12) {
      out$failure_reason <- "response_window_too_short_for_minimal_plan"
      return(out)
    }
    out$feasible <- TRUE
    out$failure_reason <- "none"
    out
  }

  response_completion_ok <- function(response_type, burst_info, suppression_duration) {
    rt <- as.character(value_or(response_type, "no_response"))[1]
    burst_spikes <- if (!is.null(burst_info$n_spikes)) safe_num(burst_info$n_spikes, 0) else 0
    suppression_duration <- safe_num(suppression_duration, 0)
    if (identical(rt, "excitatory_burst")) return(burst_spikes > 0)
    if (identical(rt, "suppressive_pause")) return(suppression_duration > 0)
    if (identical(rt, "biphasic")) return(burst_spikes > 0 && suppression_duration > 0)
    if (identical(rt, "pause_rebound")) return(suppression_duration > 0)
    FALSE
  }

  append_evoked_burst_block <- function(stim_id, response_type, strength, gain,
                                        epoch_prefix = "evoked_burst",
                                        stimulus_onset = NA_real_,
                                        max_end_time = total_time) {
    lambda <- stim$burst_lambda_base + stim$burst_lambda_strength * strength * gain
    n_bursts <- min(stim$max_evoked_bursts, stats::rpois(1, lambda = max(0, lambda)))
    response_drive <- max(0, safe_num(strength, 0) * safe_num(gain, 0))
    if (n_bursts <= 0 && response_drive > 0.25) n_bursts <- 1L
    if (stim$max_evoked_bursts >= 2L && response_drive >= 0.55 && n_bursts < 2L &&
        stats::runif(1) < min(0.85, 0.25 + response_drive)) {
      n_bursts <- 2L
    }
    if (stim$max_evoked_bursts >= 3L && response_drive >= 0.78 && n_bursts < 3L &&
        stats::runif(1) < min(0.80, response_drive - 0.20)) {
      n_bursts <- 3L
    }
    if (stim$max_evoked_bursts >= 4L && response_drive >= 0.95 && n_bursts < 4L &&
        stats::runif(1) < 0.45) {
      n_bursts <- 4L
    }
    evoked_spikes <- 0L
    successful_bursts <- 0L
    failure_reasons <- character(0)
    response_truncated <- FALSE
    window_limited <- FALSE
    sample_evoked_interburst_gap <- function(future_bursts) {
      hard_end <- min(total_time, if (is.finite(max_end_time)) max_end_time else total_time)
      available <- hard_end - current_time
      if (!is.finite(available) || available <= 0) return(NA_real_)
      global_min <- if (!is.null(config$inter_event_gap) && is.finite(config$inter_event_gap)) {
        max(0, as.numeric(config$inter_event_gap))
      } else {
        .Machine$double.eps
      }
      brng <- effective_interval_range_from_config(config, "Burst")
      burst_min <- if (length(brng) >= 1 && is.finite(brng[1])) brng[1] else global_min
      burst_max <- if (length(brng) >= 2 && is.finite(brng[2])) brng[2] else max(0.08, burst_min)
      min_gap <- max(global_min, min(0.10, burst_max * 0.75), 0.055)
      future_min <- max(0L, as.integer(future_bursts)) *
        max(1L, as.integer(stim$evoked_burst_spike_min) - 1L) *
        max(global_min, burst_min)
      max_gap <- min(0.18, available - future_min)
      if (!is.finite(max_gap) || max_gap < min_gap) return(NA_real_)
      stats::runif(1, min = min_gap, max = max_gap)
    }
    for (b in seq_len(max(0L, n_bursts))) {
      n_spikes <- sample_closed_int(stim$evoked_burst_spike_min, stim$evoked_burst_spike_max)
      if (!is.finite(n_spikes) || n_spikes < 2L) {
        failure_reasons <- unique(c(failure_reasons, "invalid_evoked_burst_spike_count"))
        response_truncated <- TRUE
        break
      }
      ok <- append_label_run("Burst", n_spikes - 1L, source = "stimulus_response",
                             stimulus_id = stim_id, phase = "evoked_window",
                             evoked = TRUE, response_type = response_type,
                             epoch = sprintf("%s_%d", epoch_prefix, b),
                             scope = "within_episode",
                             stimulus_onset = stimulus_onset,
                             max_end_time = max_end_time)
      if (isTRUE(ok)) {
        evoked_spikes <- evoked_spikes + n_spikes
        successful_bursts <- successful_bursts + 1L
      } else {
        reason <- as.character(value_or(last_append_failure_reason, "no_feasible_burst_interval"))
        if (successful_bursts <= 0L) {
          failure_reasons <- unique(c(failure_reasons, reason))
          response_truncated <- TRUE
          window_limited <- window_limited || identical(reason, "window_too_short")
        }
        break
      }
      if (b < n_bursts && current_time < total_time) {
        gap <- sample_evoked_interburst_gap(n_bursts - b)
        if (is.finite(gap) && current_time + gap <= max_end_time + 1e-12) {
          append_interval("Interburst_Gap", gap, source = "stimulus_interburst_gap",
                          stimulus_id = stim_id, phase = "evoked_window",
                          evoked = TRUE, response_type = response_type,
                          epoch = "interburst_gap", scope = "interburst_gap",
                          stimulus_onset = stimulus_onset,
                          max_end_time = max_end_time)
        } else {
          break
        }
      }
    }
    list(
      n_bursts = successful_bursts,
      n_spikes = evoked_spikes,
      planned_bursts = as.integer(n_bursts),
      response_truncated = response_truncated,
      window_limited = window_limited,
      failure_reasons = failure_reasons
    )
  }

  append_evoked_pause <- function(stim_id, response_type, strength, gain,
                                  epoch = "evoked_pause",
                                  stimulus_onset = NA_real_,
                                  max_end_time = total_time) {
    # Stimulus-locked pauses are event epochs. They may be shorter than the
    # baseline Pause ISI class, especially for TAN/CIN cue-locked pauses.
    pause_floor <- stim$pause_duration_min_s
    pause_ceiling <- max(pause_floor, stim$pause_duration_max_s)
    dur_mean <- pause_floor + (pause_ceiling - pause_floor) * min(1, max(0, strength * gain))
    dur <- dur_mean * sample_unit_mean_lognormal(stim$pause_duration_cv)
    dur <- min(pause_ceiling, max(pause_floor, dur))
    if (!is.finite(dur) || dur <= 0) {
      return(list(duration = 0, suppression_duration = 0, scorable_pause_duration = 0,
                  scorable_pause = FALSE, response_truncated = FALSE,
                  window_limited = FALSE, failure_reasons = "no_feasible_pause_interval"))
    }
    hard_end <- min(total_time, if (is.finite(max_end_time)) max_end_time else total_time)
    remaining <- hard_end - current_time
    if (!is.finite(remaining) || remaining < pause_floor - 1e-12) {
      return(list(duration = 0, suppression_duration = 0, scorable_pause_duration = 0,
                  scorable_pause = FALSE, response_truncated = TRUE,
                  window_limited = TRUE, failure_reasons = "pause_window_too_short"))
    }
    response_truncated <- FALSE
    window_limited <- FALSE
    if (current_time + dur > hard_end) {
      dur <- hard_end - current_time
      response_truncated <- TRUE
      window_limited <- TRUE
    }
    if (!is.finite(dur) || dur < pause_floor - 1e-12) {
      return(list(duration = 0, suppression_duration = 0, scorable_pause_duration = 0,
                  scorable_pause = FALSE, response_truncated = TRUE,
                  window_limited = TRUE, failure_reasons = "window_too_short"))
    }
    pause_rng <- effective_interval_range_from_config(config, "Pause")
    scorable_pause <- length(pause_rng) == 2 && all(is.finite(pause_rng)) &&
      dur >= pause_rng[1] - 1e-12 && dur <= pause_rng[2] + 1e-12
    interval_label <- if (isTRUE(scorable_pause)) "Pause" else "Stimulus_Gap"
    interval_scope <- if (isTRUE(scorable_pause)) "pause_isi" else epoch
    ok <- append_interval(interval_label, dur, source = "stimulus_response",
                          stimulus_id = stim_id, phase = "evoked_window",
                          evoked = TRUE, response_type = response_type,
                          epoch = epoch, scope = interval_scope,
                          stimulus_onset = stimulus_onset,
                          max_end_time = max_end_time)
    if (isTRUE(ok)) {
      list(duration = dur,
           suppression_duration = dur,
           scorable_pause_duration = if (isTRUE(scorable_pause)) dur else 0,
           scorable_pause = isTRUE(scorable_pause),
           response_truncated = response_truncated, window_limited = window_limited,
           failure_reasons = if (response_truncated) "window_limited" else character(0))
    } else {
      reason <- as.character(value_or(last_append_failure_reason, "no_feasible_pause_interval"))
      list(duration = 0, suppression_duration = 0, scorable_pause_duration = 0,
           scorable_pause = FALSE, response_truncated = TRUE,
           window_limited = identical(reason, "window_too_short"), failure_reasons = reason)
    }
  }

  adaptation_states <- new.env(parent = emptyenv())
  adaptation_last_onsets <- new.env(parent = emptyenv())
  adaptation_key_for_stimulus <- function(stim_row) {
    protocol <- as.character(stim_row$Protocol[1])
    type <- as.character(stim_row$Stimulus_Type[1])
    channel <- as.character(stim_row$Channel[1])
    type_key <- if (identical(protocol, "oddball")) type else "shared"
    paste(channel, type_key, sep = "|")
  }
  get_adaptation_state <- function(key, onset) {
    state <- if (exists(key, envir = adaptation_states, inherits = FALSE)) get(key, envir = adaptation_states, inherits = FALSE) else 0
    last_onset <- if (exists(key, envir = adaptation_last_onsets, inherits = FALSE)) get(key, envir = adaptation_last_onsets, inherits = FALSE) else NA_real_
    if (stim$adaptation_enabled && is.finite(last_onset) && is.finite(onset)) {
      state <- state * exp(-max(0, onset - last_onset) / stim$adaptation_tau_s)
    }
    assign(key, state, envir = adaptation_states)
    state
  }
  set_adaptation_state <- function(key, state, onset) {
    assign(key, state, envir = adaptation_states)
    assign(key, onset, envir = adaptation_last_onsets)
    invisible(state)
  }
  forced_pre_stimulus_state_for_index <- function(index) {
    states <- as.character(value_or(stim$pre_stimulus_state_sequence, character(0)))
    states <- states[nzchar(states) & states %in% VALID_PRE_STIMULUS_STATES]
    if (length(states) == 0) return(NA_character_)
    states[((as.integer(index) - 1L) %% length(states)) + 1L]
  }
  for (sidx in seq_len(nrow(stim_table))) {
    stim_row <- stim_table[sidx, , drop = FALSE]
    onset <- stim_row$Onset_s[1]
    if (!is.finite(onset) || onset > total_time) next
    baseline_phase <- if (sidx == 1L) "pre_stimulus_baseline" else "baseline_recovery"
    append_baseline_until(
      onset,
      phase = baseline_phase,
      source = baseline_phase,
      epoch = baseline_phase,
      use_recovery_mode = TRUE,
      guard_s = stim$pre_stimulus_guard_s,
      stimulus_id = stim_row$Stimulus_ID[1],
      stimulus_onset = onset
    )
    planned_response_end <- min(total_time, onset + stim$response_window_s)
    adaptation_key <- adaptation_key_for_stimulus(stim_row)
    adaptation_state <- get_adaptation_state(adaptation_key, onset)
    adaptation_pre <- adaptation_state
    gain <- stimulus_response_gain(adaptation_state, stim$response_floor)
    pre_response_label <- last_non_latency_label()
    pre_response_state <- pre_stimulus_state_label(onset)
    forced_pre_response_state <- forced_pre_stimulus_state_for_index(sidx)
    if (!is.na(forced_pre_response_state) && nzchar(forced_pre_response_state)) {
      pre_response_label <- forced_pre_response_state
      pre_response_state <- forced_pre_response_state
    }
    rt <- response_type_for_stimulus(stim_row, gain, pre_response_state)
    response_probability <- effective_response_probability(stim_row$Strength[1], gain, stim_row)
    designed_no_response_kernel <- identical(rt, "no_response") &&
      (identical(stim$protocol, "feature_tuning") || identical(stim$response_type, "feature_tuned"))
    response_attempted <- if (isTRUE(designed_no_response_kernel)) FALSE else stats::runif(1) <= response_probability
    burst_info <- list(n_bursts = 0L, n_spikes = 0L)
    evoked_suppression_duration <- 0
    scorable_pause_duration <- 0
    rebound <- FALSE
    response_failure_reasons <- character(0)
    response_truncated <- FALSE
    window_limited <- FALSE
    generated_response_start <- NA_real_
    response_state_snapshot <- NULL
    response_plan <- list(
      feasible = FALSE,
      start = NA_real_,
      end = planned_response_end,
      latency = NA_real_,
      min_duration = NA_real_,
      required_components = response_required_components_for_plan(rt),
      failure_reason = "response_not_attempted"
    )
    response_rolled_back <- FALSE
    if (isTRUE(response_attempted)) {
      sampled_latency <- sample_response_latency()
      response_plan <- plan_stimulus_response(onset, planned_response_end, rt, sampled_latency)
      if (!isTRUE(response_plan$feasible)) {
        response_failure_reasons <- unique(c(response_failure_reasons, "response_plan_infeasible", response_plan$failure_reason))
        response_truncated <- TRUE
        window_limited <- window_limited || grepl("window|refractory|latency", response_plan$failure_reason)
        append_baseline_until(
          planned_response_end,
          phase = "infeasible_response_baseline",
          source = "infeasible_response_baseline",
          epoch = "infeasible_response_baseline",
          use_recovery_mode = TRUE,
          guard_s = 0,
          stimulus_id = stim_row$Stimulus_ID[1],
          stimulus_onset = onset,
          response_type = rt
        )
      } else {
        response_state_snapshot <- capture_stim_state()
        response_path_open <- TRUE
        latency_ok <- TRUE
        response_start <- response_plan$start
        if (response_start > current_time + 1e-12) {
          if (isTRUE(has_spike)) {
            dur <- response_start - current_time
            if (current_time < onset - 1e-12) {
              latency_ok <- append_interval("Stimulus_Gap", dur, source = "stimulus_spanning_gap",
                                            stimulus_id = stim_row$Stimulus_ID[1],
                                            phase = "stimulus_spanning_gap", evoked = FALSE,
                                            response_type = rt, epoch = "stimulus_spanning_gap",
                                            scope = "stimulus_spanning_gap",
                                            stimulus_onset = onset,
                                            max_end_time = planned_response_end)
            } else {
              latency_ok <- append_interval("Latency", dur, source = "stimulus_response_latency",
                                            stimulus_id = stim_row$Stimulus_ID[1],
                                            phase = "response_latency", evoked = FALSE,
                                            response_type = rt, epoch = "response_latency",
                                            scope = "stimulus_latency",
                                            stimulus_onset = onset,
                                            max_end_time = planned_response_end)
            }
          } else {
            spanning_gap <- current_time < onset - 1e-12
            latency_ok <- append_latency_spike(
              response_start,
              context = rt,
              model = if (isTRUE(spanning_gap)) "stimulus_spanning_gap" else "stimulus_response_latency",
              stimulus_id = stim_row$Stimulus_ID[1],
              phase = if (isTRUE(spanning_gap)) "stimulus_spanning_gap" else "response_latency",
              response_type = rt,
              epoch = if (isTRUE(spanning_gap)) "stimulus_spanning_gap" else "response_latency",
              stimulus_onset = onset,
              pattern = if (isTRUE(spanning_gap)) "Stimulus_Gap" else "Latency",
              episode_scope = if (isTRUE(spanning_gap)) "stimulus_spanning_gap" else "stimulus_latency"
            )
          }
        }
        if (!isTRUE(latency_ok)) {
          reason <- as.character(value_or(last_append_failure_reason, "response_latency_not_feasible"))
          response_failure_reasons <- unique(c(response_failure_reasons, reason))
          response_truncated <- TRUE
          window_limited <- window_limited || identical(reason, "window_too_short") || grepl("refractory", reason)
          response_path_open <- FALSE
        }
        if (isTRUE(response_path_open)) {
          generated_response_start <- current_time
          if (identical(rt, "excitatory_burst")) {
            burst_info <- append_evoked_burst_block(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                                    stimulus_onset = onset, max_end_time = planned_response_end)
            response_failure_reasons <- unique(c(response_failure_reasons, burst_info$failure_reasons))
            response_truncated <- response_truncated || isTRUE(burst_info$response_truncated)
            window_limited <- window_limited || isTRUE(burst_info$window_limited)
            if (burst_info$n_spikes > 0 && stats::runif(1) < stim$post_burst_pause_probability) {
              pause_info <- append_evoked_pause(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                                epoch = "post_burst_pause",
                                                stimulus_onset = onset,
                                                max_end_time = planned_response_end)
              evoked_suppression_duration <- evoked_suppression_duration + pause_info$suppression_duration
              scorable_pause_duration <- scorable_pause_duration + pause_info$scorable_pause_duration
              response_failure_reasons <- unique(c(response_failure_reasons, pause_info$failure_reasons))
              response_truncated <- response_truncated || isTRUE(pause_info$response_truncated)
              window_limited <- window_limited || isTRUE(pause_info$window_limited)
            }
          } else if (identical(rt, "suppressive_pause")) {
            pause_info <- append_evoked_pause(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                              epoch = "evoked_suppression",
                                              stimulus_onset = onset,
                                              max_end_time = planned_response_end)
            evoked_suppression_duration <- evoked_suppression_duration + pause_info$suppression_duration
            scorable_pause_duration <- scorable_pause_duration + pause_info$scorable_pause_duration
            response_failure_reasons <- unique(c(response_failure_reasons, pause_info$failure_reasons))
            response_truncated <- response_truncated || isTRUE(pause_info$response_truncated)
            window_limited <- window_limited || isTRUE(pause_info$window_limited)
          } else if (identical(rt, "biphasic")) {
            burst_info <- append_evoked_burst_block(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                                    epoch_prefix = "early_burst",
                                                    stimulus_onset = onset,
                                                    max_end_time = planned_response_end)
            response_failure_reasons <- unique(c(response_failure_reasons, burst_info$failure_reasons))
            response_truncated <- response_truncated || isTRUE(burst_info$response_truncated)
            window_limited <- window_limited || isTRUE(burst_info$window_limited)
            if (burst_info$n_spikes > 0) {
              pause_info <- append_evoked_pause(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                                epoch = "post_burst_pause",
                                                stimulus_onset = onset,
                                                max_end_time = planned_response_end)
              evoked_suppression_duration <- evoked_suppression_duration + pause_info$suppression_duration
              scorable_pause_duration <- scorable_pause_duration + pause_info$scorable_pause_duration
              response_failure_reasons <- unique(c(response_failure_reasons, pause_info$failure_reasons))
              response_truncated <- response_truncated || isTRUE(pause_info$response_truncated)
              window_limited <- window_limited || isTRUE(pause_info$window_limited)
            }
          } else if (identical(rt, "pause_rebound")) {
            pause_info <- append_evoked_pause(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                              epoch = "evoked_pause",
                                              stimulus_onset = onset,
                                              max_end_time = planned_response_end)
            evoked_suppression_duration <- evoked_suppression_duration + pause_info$suppression_duration
            scorable_pause_duration <- scorable_pause_duration + pause_info$scorable_pause_duration
            response_failure_reasons <- unique(c(response_failure_reasons, pause_info$failure_reasons))
            response_truncated <- response_truncated || isTRUE(pause_info$response_truncated)
            window_limited <- window_limited || isTRUE(pause_info$window_limited)
            if (pause_info$suppression_duration > 0 && stats::runif(1) < stim$rebound_probability) {
              burst_info <- append_evoked_burst_block(stim_row$Stimulus_ID[1], rt, stim_row$Strength[1], gain,
                                                      epoch_prefix = "rebound_burst",
                                                      stimulus_onset = onset,
                                                      max_end_time = planned_response_end)
              rebound <- burst_info$n_spikes > 0
              rebound_failure_reasons <- as.character(burst_info$failure_reasons)
              rebound_failure_reasons <- rebound_failure_reasons[nzchar(rebound_failure_reasons)]
              if (length(rebound_failure_reasons) > 0) {
                rebound_failure_reasons <- paste0("optional_rebound_", rebound_failure_reasons)
              }
              response_failure_reasons <- unique(c(response_failure_reasons, rebound_failure_reasons))
              response_truncated <- response_truncated || isTRUE(burst_info$response_truncated)
              window_limited <- window_limited || isTRUE(burst_info$window_limited)
            }
          }
        }
      }
    } else {
      response_failure_reasons <- unique(c(response_failure_reasons, if (isTRUE(designed_no_response_kernel)) "feature_kernel_no_response" else "probabilistic_response_failure"))
      append_baseline_until(
        planned_response_end,
        phase = "no_response_baseline",
        source = "no_response_baseline",
        epoch = "no_response_baseline",
        use_recovery_mode = TRUE,
        guard_s = 0,
        stimulus_id = stim_row$Stimulus_ID[1],
        stimulus_onset = onset,
        response_type = rt
      )
    }
    response_generated <- response_completion_ok(rt, burst_info, evoked_suppression_duration)
    if (isTRUE(response_attempted) && !isTRUE(response_generated) && !is.null(response_state_snapshot)) {
      restore_stim_state(response_state_snapshot)
      generated_response_start <- NA_real_
      burst_info <- list(n_bursts = 0L, n_spikes = 0L)
      evoked_suppression_duration <- 0
      scorable_pause_duration <- 0
      rebound <- FALSE
      response_generated <- FALSE
      response_rolled_back <- TRUE
      response_failure_reasons <- unique(c(response_failure_reasons, "response_commit_rolled_back_no_complete_response"))
      append_baseline_until(
        planned_response_end,
        phase = "failed_response_baseline",
        source = "failed_response_baseline",
        epoch = "failed_response_baseline",
        use_recovery_mode = TRUE,
        guard_s = 0,
        stimulus_id = stim_row$Stimulus_ID[1],
        stimulus_onset = onset,
        response_type = rt
      )
    }
    generated_response_end <- if (isTRUE(response_generated)) current_time else NA_real_
    actual_latency <- if (isTRUE(response_generated) && is.finite(onset) && is.finite(generated_response_start)) generated_response_start - onset else NA_real_
    response_failure_reasons <- response_failure_reasons[nzchar(response_failure_reasons)]
    failure_reason <- if (length(response_failure_reasons) > 0) {
      paste(response_failure_reasons, collapse = ";")
    } else if (isTRUE(response_generated)) {
      "none"
    } else {
      "model_no_response"
    }
    load <- response_load_value(burst_info$n_spikes, evoked_suppression_duration)
    stimulus_adaptation_load <- safe_num(stim_row$Strength[1], 0)
    response_adaptation_load <- if (isTRUE(response_generated)) stimulus_adaptation_load * max(1, load) / 5 else 0
    if (stim$adaptation_enabled) {
      if (identical(stim$adaptation_source, "stimulus")) {
        adaptation_state <- adaptation_state + stim$adaptation_increment * stimulus_adaptation_load
      } else if (identical(stim$adaptation_source, "response")) {
        adaptation_state <- adaptation_state + stim$adaptation_increment * response_adaptation_load
      } else {
        adaptation_state <- adaptation_state + stim$adaptation_increment *
          (0.5 * stimulus_adaptation_load + 0.5 * response_adaptation_load)
      }
    }
    set_adaptation_state(adaptation_key, adaptation_state, onset)
    response_i <- response_i + 1L
    response_rows[[response_i]] <- data.frame(
      Train = NA_integer_, Stimulus_ID = stim_row$Stimulus_ID[1], Response_Type = rt,
      Response_Latency_s = actual_latency,
      Response_Window_Start_s = onset, Response_Window_End_s = planned_response_end,
      Generated_Response_Start_s = generated_response_start,
      Generated_Response_End_s = generated_response_end,
      Expected_Response_Window_s = stim$response_window_s,
      Response_Plan_Feasible = isTRUE(response_plan$feasible),
      Response_Plan_Start_s = safe_num(response_plan$start, NA_real_),
      Response_Plan_End_s = safe_num(response_plan$end, planned_response_end),
      Response_Plan_Min_Duration_s = safe_num(response_plan$min_duration, NA_real_),
      Response_Plan_Required_Components = as.character(value_or(response_plan$required_components, NA_character_)),
      Response_Plan_Failure_Reason = as.character(value_or(response_plan$failure_reason, NA_character_)),
      Response_Rolled_Back = isTRUE(response_rolled_back),
      Response_Commit_OK = isTRUE(response_generated) && !isTRUE(response_rolled_back),
      Evoked_Burst_Count = as.integer(burst_info$n_bursts), Evoked_Spike_Count = as.integer(burst_info$n_spikes),
      Evoked_Suppression_Duration_s = evoked_suppression_duration,
      Scorable_Evoked_Pause_Duration_s = scorable_pause_duration,
      Evoked_Pause_Duration_s = scorable_pause_duration,
      Adaptation_State_Pre = adaptation_pre, Adaptation_State_Post = adaptation_state,
      Adaptation_Source = stim$adaptation_source,
      Stimulus_Adaptation_Load = stimulus_adaptation_load,
      Response_Adaptation_Load = response_adaptation_load,
	      Response_Gain = gain, Response_Probability = response_probability,
	      Response_Attempted = isTRUE(response_attempted), Response_Load = load,
      Response_Generated_OK = isTRUE(response_generated),
      Response_Truncated = isTRUE(response_truncated),
      Response_Failure_Reason = failure_reason,
      Response_Failure_Class = classify_response_failure(
        failure_reason,
        generated_ok = isTRUE(response_generated),
        plan_feasible = isTRUE(response_plan$feasible),
        rolled_back = isTRUE(response_rolled_back),
        window_limited = isTRUE(window_limited)
      ),
      Window_Limited = isTRUE(window_limited),
      Suppression_Index = ifelse(identical(rt, "suppressive_pause"), min(1, evoked_suppression_duration / max(stim$response_window_s, 1e-9)), NA_real_),
      Rebound_Burst = isTRUE(rebound),
      Pre_Stimulus_State = as.character(value_or(pre_response_state, NA_character_)),
      Pre_Stimulus_Label = as.character(value_or(pre_response_label, NA_character_)),
      Feature_Modality = as.character(value_or(stim_row$Feature_Modality[1], NA_character_)),
      Stimulus_Feature_Value = safe_num(stim_row$Stimulus_Feature_Value[1], NA_real_),
      Stimulus_Position_X = safe_num(stim_row$Stimulus_Position_X[1], NA_real_),
      Stimulus_Position_Y = safe_num(stim_row$Stimulus_Position_Y[1], NA_real_),
      Preferred_Feature_Value = safe_num(stim_row$Preferred_Feature_Value[1], NA_real_),
      Null_Feature_Value = safe_num(stim_row$Null_Feature_Value[1], NA_real_),
      Feature_Distance_To_Preferred = safe_num(stim_row$Feature_Distance_To_Preferred[1], NA_real_),
      Feature_Distance_To_Null = safe_num(stim_row$Feature_Distance_To_Null[1], NA_real_),
      Feature_Excitation = safe_num(stim_row$Feature_Excitation[1], NA_real_),
      Feature_Suppression = safe_num(stim_row$Feature_Suppression[1], NA_real_),
      Feature_Selectivity = safe_num(stim_row$Feature_Selectivity[1], NA_real_),
      Feature_Response_Class = as.character(value_or(stim_row$Feature_Response_Class[1], NA_character_)),
      External_Strength = safe_num(stim_row$External_Strength[1], NA_real_),
      Feature_Drive = safe_num(stim_row$Feature_Drive[1], NA_real_),
      Feature_Matched = isTRUE(if ("Feature_Matched" %in% names(stim_row)) stim_row$Feature_Matched[1] else FALSE),
      Drive_Above_Threshold = isTRUE(if ("Drive_Above_Threshold" %in% names(stim_row)) stim_row$Drive_Above_Threshold[1] else FALSE),
      Response_Kernel = as.character(value_or(if ("Response_Kernel" %in% names(stim_row)) stim_row$Response_Kernel[1] else NA_character_, NA_character_)),
      Response_Eligible = isTRUE(if ("Response_Eligible" %in% names(stim_row)) stim_row$Response_Eligible[1] else stim_row$Feature_Response_Eligible[1]),
      Feature_Response_Eligible = isTRUE(stim_row$Feature_Response_Eligible[1]),
      Feature_Response_Reason = as.character(value_or(stim_row$Feature_Response_Reason[1], NA_character_)),
      Unit_ID = as.integer(round(safe_num(stim_row$Unit_ID[1], NA_real_))),
      Unit_Tuning_Mode = as.character(value_or(stim_row$Unit_Tuning_Mode[1], NA_character_)),
      Unit_Class = as.character(value_or(stim_row$Unit_Class[1], NA_character_)),
      Unit_Responsive = isTRUE(stim_row$Unit_Responsive[1]),
      Unit_Preferred_Feature_Value = safe_num(stim_row$Unit_Preferred_Feature_Value[1], NA_real_),
      Unit_Null_Feature_Value = safe_num(stim_row$Unit_Null_Feature_Value[1], NA_real_),
      Unit_Place_Field_Center_X = safe_num(stim_row$Unit_Place_Field_Center_X[1], NA_real_),
      Unit_Place_Field_Center_Y = safe_num(stim_row$Unit_Place_Field_Center_Y[1], NA_real_),
      Unit_Place_Field_Width = safe_num(stim_row$Unit_Place_Field_Width[1], NA_real_),
      Unit_Place_Field_Radius = safe_num(stim_row$Unit_Place_Field_Radius[1], NA_real_),
      Place_Field_Distance = safe_num(stim_row$Place_Field_Distance[1], NA_real_),
      Place_Field_Drive = safe_num(stim_row$Place_Field_Drive[1], NA_real_),
      Unit_Tuning_Width = safe_num(stim_row$Unit_Tuning_Width[1], NA_real_),
      Unit_Suppression_Width = safe_num(stim_row$Unit_Suppression_Width[1], NA_real_),
      Unit_Max_Response_Gain = safe_num(stim_row$Unit_Max_Response_Gain[1], NA_real_),
      Unit_Response_Threshold = safe_num(stim_row$Unit_Response_Threshold[1], NA_real_),
      Unit_Response_Reliability = safe_num(stim_row$Unit_Response_Reliability[1], NA_real_),
      stringsAsFactors = FALSE
    )
  }
  append_baseline_until(
    total_time,
    phase = "post_stimulus_recovery",
    source = "baseline_recovery",
    epoch = "post_stimulus_recovery",
    use_recovery_mode = TRUE,
    guard_s = 0
  )

  interval_seq <- if (length(interval_rows) > 0) do.call(rbind, interval_rows) else data.frame()
  if (length(spike_times) > 0) {
    spike_times <- sort(as.numeric(spike_times[is.finite(spike_times)]))
    spike_times <- spike_times[!duplicated(signif(spike_times, 14))]
  }
  if (nrow(interval_seq) > 0) {
    interval_seq <- interval_seq[order(interval_seq$Interval_Seq_ID), , drop = FALSE]
    for (col in c("Event_Epoch_Type", "Event_Epoch_Source", "Event_Epoch_Generation_Rule")) {
      if (!col %in% names(interval_seq)) interval_seq[[col]] <- NA_character_
    }
    stim_cmp <- ifelse(is.na(interval_seq$Stimulus_ID), -1L, interval_seq$Stimulus_ID)
    break_flags <- c(TRUE,
                     interval_seq$ISI_Label[-1] != interval_seq$ISI_Label[-nrow(interval_seq)] |
                       stim_cmp[-1] != stim_cmp[-nrow(interval_seq)] |
                       interval_seq$Response_Epoch[-1] != interval_seq$Response_Epoch[-nrow(interval_seq)])
    interval_seq$Episode <- cumsum(break_flags) + length(latency_rows)
  } else {
    interval_seq$Episode <- integer(0)
  }

  match_time_index <- function(time_value) {
    if (length(spike_times) == 0 || !is.finite(time_value)) return(NA_integer_)
    which.min(abs(spike_times - time_value))
  }
  intervals <- make_empty_interval_df()
  if (nrow(interval_seq) > 0) {
    intervals <- data.frame(
      Train = rep(1L, nrow(interval_seq)), Interval_ID = seq_len(nrow(interval_seq)),
      Left_Spike_Index = vapply(interval_seq$Start_Time_s, match_time_index, integer(1)),
      Right_Spike_Index = vapply(interval_seq$End_Time_s, match_time_index, integer(1)),
      Left_Spike_Time_s = interval_seq$Start_Time_s, Right_Spike_Time_s = interval_seq$End_Time_s,
      Start_Time_s = interval_seq$Start_Time_s, End_Time_s = interval_seq$End_Time_s,
      ISI_s = interval_seq$ISI_s, Interval = interval_seq$ISI_s, ISI_Label = interval_seq$ISI_Label, Episode = interval_seq$Episode,
      ISI_Scope = interval_seq$ISI_Scope, Left_Spike_Role = NA_character_, Right_Spike_Role = NA_character_,
      Left_Episode_Context = NA_character_, Right_Episode_Context = NA_character_, Is_Manual_Fixed = interval_seq$Is_Manual_Fixed,
      Interval_Source = interval_seq$Interval_Source, Run_Position = interval_seq$Run_Position, Run_Length = interval_seq$Run_Length,
      Temporal_Rho = interval_seq$Temporal_Rho, Temporal_Trend = interval_seq$Temporal_Trend,
      Stimulus_ID = interval_seq$Stimulus_ID, Stimulus_Phase = interval_seq$Stimulus_Phase, Evoked = interval_seq$Evoked,
      Evoked_Response_Type = interval_seq$Evoked_Response_Type, Response_Epoch = interval_seq$Response_Epoch,
      Stimulus_Onset_s = interval_seq$Stimulus_Onset_s,
      Time_From_Stimulus_Onset_s = interval_seq$Time_From_Stimulus_Onset_s,
      Contains_Stimulus_Onset = interval_seq$Contains_Stimulus_Onset,
      Event_Epoch_Type = interval_seq$Event_Epoch_Type,
      Event_Epoch_Source = interval_seq$Event_Epoch_Source,
      Event_Epoch_Generation_Rule = interval_seq$Event_Epoch_Generation_Rule,
      stringsAsFactors = FALSE
    )
  }

  episode_rows <- list(); epn <- 0L
  if (length(latency_rows) > 0) {
    for (lr in latency_rows) { epn <- epn + 1L; lr$Episode <- epn; episode_rows[[epn]] <- lr }
  }
  if (nrow(intervals) > 0) {
    for (episode_id in sort(unique(intervals$Episode))) {
      idx <- which(intervals$Episode == episode_id); idx <- idx[order(intervals$Interval_ID[idx])]
      vals <- intervals$ISI_s[idx]; label <- intervals$ISI_Label[idx[1]]; reg <- isi_regularity_metrics(vals)
      start_time <- intervals$Start_Time_s[idx[1]]; end_time <- intervals$End_Time_s[idx[length(idx)]]
      n_isis <- length(idx); n_boundary <- n_isis + 1L; dur <- end_time - start_time
      stim_onset_vals <- intervals$Stimulus_Onset_s[idx]
      stim_onset <- if (any(is.finite(stim_onset_vals))) stim_onset_vals[which(is.finite(stim_onset_vals))[1]] else NA_real_
      scope_values <- unique(as.character(intervals$ISI_Scope[idx]))
      scope_values <- scope_values[!is.na(scope_values) & nzchar(scope_values)]
      event_epoch_scopes <- c("evoked_suppression", "post_burst_pause", "evoked_pause",
                              "failed_response_baseline", "no_response_baseline", "post_stimulus_recovery")
      episode_scope <- if (all(intervals$ISI_Scope[idx] %in% "stimulus_latency")) {
        "stimulus_latency"
      } else if (all(intervals$ISI_Scope[idx] %in% "interburst_gap")) {
        "interburst_gap"
      } else if (all(intervals$ISI_Scope[idx] %in% "stimulus_spanning_gap")) {
        "stimulus_spanning_gap"
      } else if (all(intervals$ISI_Scope[idx] %in% "baseline_recovery")) {
        "baseline_recovery"
      } else if (length(scope_values) == 1L && scope_values %in% event_epoch_scopes) {
        scope_values
      } else {
        "interval_run"
      }
      epn <- epn + 1L
      episode_rows[[epn]] <- data.frame(
        Episode = episode_id, Pattern = label, Episode_Scope = episode_scope, Latency_Context = NA_character_, Latency_Model = NA_character_,
        Start = start_time, End = end_time, Episode_Duration = dur, Core_Start = start_time, Core_End = end_time, Core_Duration = dur,
        First_Spike_Time = start_time, Last_Spike_Time = end_time, N_Spikes = n_boundary, N_ISIs = n_isis,
        N_Boundary_Spikes = n_boundary, N_New_Spikes = n_boundary - as.integer(min(idx) > 1L), N_Shared_Boundary_Spikes = as.integer(min(idx) > 1L),
        Mean_Within_Episode_ISI = reg$mean, CV_Within_Episode_ISI = reg$cv, Mean_CV2_Within_Episode_ISI = reg$cv2, LV_Within_Episode_ISI = reg$lv,
        Core_ISI_Rate_Hz = if (dur > 0) n_isis / dur else NA_real_, Episode_Inclusive_Rate_Hz = if (dur > 0) n_boundary / dur else NA_real_,
        Stimulus_ID = if (all(is.na(intervals$Stimulus_ID[idx]))) NA_integer_ else intervals$Stimulus_ID[idx][which(!is.na(intervals$Stimulus_ID[idx]))[1]],
        Stimulus_Phase = as.character(na.omit(intervals$Stimulus_Phase[idx]))[1],
        Evoked = any(intervals$Evoked[idx] %in% TRUE), Evoked_Response_Type = as.character(na.omit(intervals$Evoked_Response_Type[idx]))[1],
        Response_Epoch = as.character(na.omit(intervals$Response_Epoch[idx]))[1],
        Stimulus_Onset_s = stim_onset,
        Time_From_Stimulus_Onset_s = if (is.finite(stim_onset)) end_time - stim_onset else NA_real_,
        Contains_Stimulus_Onset = any(intervals$Contains_Stimulus_Onset[idx] %in% TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  episodes <- if (length(episode_rows) > 0) { out <- do.call(rbind, episode_rows); out <- out[order(out$Episode), , drop = FALSE]; rownames(out) <- NULL; out } else make_empty_episode_df()

  spikes <- if (length(spike_times) > 0) {
    right_labels <- left_labels <- rep(NA_character_, length(spike_times)); right_eps <- left_eps <- rep(NA_integer_, length(spike_times))
    if (nrow(intervals) > 0) {
      for (i in seq_len(nrow(intervals))) {
        li <- intervals$Left_Spike_Index[i]; ri <- intervals$Right_Spike_Index[i]
        if (is.finite(li) && li >= 1 && li <= length(spike_times)) { right_labels[li] <- intervals$ISI_Label[i]; right_eps[li] <- intervals$Episode[i] }
        if (is.finite(ri) && ri >= 1 && ri <= length(spike_times)) { left_labels[ri] <- intervals$ISI_Label[i]; left_eps[ri] <- intervals$Episode[i] }
      }
    }
    role <- ifelse(!is.na(left_labels) & !is.na(right_labels) & left_labels != right_labels, "shared_boundary_spike", "episode_spike")
    data.frame(Episode = ifelse(!is.na(right_eps), right_eps, left_eps), Time = spike_times, Episode_Context = ifelse(!is.na(right_labels), right_labels, left_labels), Spike_Role = role, stringsAsFactors = FALSE)
  } else make_empty_spike_df()
  if (nrow(intervals) > 0 && nrow(spikes) > 0) {
    intervals$Left_Spike_Role <- spikes$Spike_Role[intervals$Left_Spike_Index]
    intervals$Right_Spike_Role <- spikes$Spike_Role[intervals$Right_Spike_Index]
    intervals$Left_Episode_Context <- spikes$Episode_Context[intervals$Left_Spike_Index]
    intervals$Right_Episode_Context <- spikes$Episode_Context[intervals$Right_Spike_Index]
  }
  response_table <- if (length(response_rows) > 0) {
    out <- do.call(rbind, response_rows)
    resp_cols <- names(make_empty_response_df())
    for (col in setdiff(resp_cols, names(out))) out[[col]] <- NA
    out[, resp_cols, drop = FALSE]
  } else make_empty_response_df()
  event_epochs <- event_epochs_from_intervals(intervals)
  if (nrow(stim_table) > 0) stim_table$Train <- 1L
  if (nrow(response_table) > 0) response_table$Train <- 1L
  if (nrow(event_epochs) > 0) event_epochs$Train <- 1L
  rownames(spikes) <- rownames(intervals) <- rownames(episodes) <- NULL
  rownames(event_epochs) <- NULL
  list(spikes = spikes, episodes = episodes, intervals = intervals, stimuli = stim_table,
       responses = response_table, event_epochs = event_epochs, warnings = warnings)
}


simulate_spike_train_core <- function(config, seed = NULL) {
  validation <- validate_sim_config_core(config)
  if (length(validation$errors) > 0) {
    stop(paste(validation$errors, collapse = "\n"), call. = FALSE)
  }
  if (stimulation_enabled(config) && !isTRUE(config$.inside_stimulation_core)) {
    cfg_stim <- config
    cfg_stim$.inside_stimulation_core <- TRUE
    return(simulate_stimulus_sequence_core(cfg_stim, seed = seed))
  }
  if (!is.null(seed) && length(seed) == 1 && is.finite(seed)) {
    set.seed(as.integer(seed))
  }

  total_time <- as.numeric(config$total_time)
  current_time <- 0
  warnings <- unique(validation$warnings)
  pattern_duration <- setNames(rep(0, length(SPIKE_PATTERN_LEVELS)), SPIKE_PATTERN_LEVELS)
  interval_rows <- list()
  leading_episode_rows <- list()
  interval_seq_id <- 0L
  leading_episode_count <- 0L
  leading_silence_end_times <- numeric(0)
  spike_times <- numeric(0)
  has_current_spike <- FALSE
  last_isi_value <- NA_real_
  last_isi_pattern <- NA_character_
  recent_noisy_history <- numeric(0)
  initial_pause_warning_added <- FALSE
  initial_latency_warning_added <- FALSE
  noisy_min_warning_added <- FALSE
  noisy_variability_warning_added <- FALSE
  # Biological-recording default: t = 0 is the recording-window boundary, not a spike.
  # The first real spike must occur at t > 0. For an initial Pause, this option controls
  # whether the first Pause duration is interpreted as leading latency rather than as a true ISI.
  leading_silence_initial_pause <- !identical(config$leading_silence_initial_pause, FALSE)

  add_warning <- function(message) {
    warnings <<- unique(c(warnings, message))
  }

  get_pattern_config <- function(pattern) {
    config$patterns[[pattern]]
  }

  effective_interval_range <- function(pattern) {
    effective_interval_range_from_config(config, pattern)
  }

  value_in_segments <- function(value, segments) {
    is.finite(value) && any(x_in_interval_segments(value, segments))
  }

  sample_bounded_interval <- function(pattern, previous_isi = NA_real_, previous_pattern = NA_character_, exclude_ranges = NULL) {
    pat_cfg <- get_pattern_config(pattern)
    segments <- effective_pattern_segments_from_config(config, pattern, previous_isi, previous_pattern)
    had_segments_before_exclusion <- nrow(segments) > 0
    exclusion_applied <- FALSE
    if (!is.null(exclude_ranges) && length(exclude_ranges) > 0) {
      if (is.numeric(exclude_ranges) && length(exclude_ranges) == 2) exclude_ranges <- list(exclude_ranges)
      for (excluded in exclude_ranges) {
        if (length(excluded) == 2 && all(is.finite(excluded)) && excluded[2] >= excluded[1]) {
          exclusion_applied <- TRUE
          segments <- subtract_interval_segments(segments, excluded)
        }
      }
    }
    if (nrow(segments) == 0) {
      if (isTRUE(exclusion_applied) && isTRUE(had_segments_before_exclusion)) {
        add_warning(sprintf(
          "%s has no feasible interval segment after excluding intervals that would create a forbidden Noisy burst-like run.",
          pattern
        ))
        return(NA_real_)
      }
      rng <- effective_interval_range(pattern)
      rng_text <- if (length(rng) == 2 && all(is.finite(rng))) {
        sprintf("[%.6g, %.6g]", rng[1], rng[2])
      } else {
        "<empty>"
      }
      add_warning(sprintf(
        "%s has no feasible interval segment under the current accepted range %s, absolute refractory period, contextual Noisy clean-label and adjacency rules.",
        pattern, rng_text
      ))
      return(NA_real_)
    }

    mass <- interval_segments_mass(pat_cfg$dist_type, pat_cfg$params, segments)
    if (!is.finite(mass) || mass <= 0) {
      add_warning(sprintf(
        "%s interval distribution has zero probability mass in the feasible segment set; adjust distribution parameters or accepted ranges.",
        pattern
      ))
      return(NA_real_)
    }

    val <- sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
    if (!is.finite(val)) {
      add_warning(sprintf(
        "%s truncated interval sampler failed despite a positive feasible mass estimate %.4g.",
        pattern, mass
      ))
      return(NA_real_)
    }
    val
  }


  pattern_temporal_dependence <- function(pattern) {
    pat_cfg <- get_pattern_config(pattern)
    dep <- if (!is.null(pat_cfg$temporal_dependence)) pat_cfg$temporal_dependence else list(rho = 0, trend = 0)
    coerce_temporal_dependence(dep)
  }

  pattern_nominal_interval_mean <- function(pattern) {
    pat_cfg <- get_pattern_config(pattern)
    mean_val <- distribution_nominal_mean(pat_cfg$dist_type, pat_cfg$params)
    base_segments <- effective_pattern_segments_from_config(config, pattern, NA_real_, NA_character_)
    if (!is.finite(mean_val) || nrow(base_segments) == 0) {
      if (nrow(base_segments) == 0) return(NA_real_)
      mean_val <- mean(c(min(base_segments$Start), max(base_segments$End)))
    }
    min(max(mean_val, min(base_segments$Start)), max(base_segments$End))
  }

  run_target_mean <- function(pattern, interval_index, n_intervals) {
    dep <- pattern_temporal_dependence(pattern)
    base_mean <- pattern_nominal_interval_mean(pattern)
    if (!is.finite(base_mean) || base_mean <= 0) return(NA_real_)
    n_intervals <- max(1L, as.integer(n_intervals))
    pos <- if (n_intervals > 1L) (as.numeric(interval_index) - 1) / (n_intervals - 1) - 0.5 else 0
    base_mean * exp(dep$trend * pos)
  }

  sample_temporally_adjusted_interval <- function(pattern, interval_index, n_intervals, run_values,
                                                  previous_isi = NA_real_, previous_pattern = NA_character_,
                                                  exclude_ranges = NULL, max_attempts = 120L) {
    dep <- pattern_temporal_dependence(pattern)
    if (abs(dep$rho) < 1e-12 && abs(dep$trend) < 1e-12) {
      return(sample_bounded_interval(pattern, previous_isi, previous_pattern, exclude_ranges))
    }

    target_mu <- run_target_mean(pattern, interval_index, n_intervals)
    base_mu <- pattern_nominal_interval_mean(pattern)
    if (!is.finite(target_mu) || !is.finite(base_mu) || base_mu <= 0) {
      return(sample_bounded_interval(pattern, previous_isi, previous_pattern, exclude_ranges))
    }

    segments <- effective_pattern_segments_from_config(config, pattern, previous_isi, previous_pattern)
    if (!is.null(exclude_ranges) && length(exclude_ranges) > 0) {
      if (is.numeric(exclude_ranges) && length(exclude_ranges) == 2) exclude_ranges <- list(exclude_ranges)
      for (excluded in exclude_ranges) {
        if (length(excluded) == 2 && all(is.finite(excluded)) && excluded[2] >= excluded[1]) {
          segments <- subtract_interval_segments(segments, excluded)
        }
      }
    }
    if (nrow(segments) == 0) return(NA_real_)

    prev_target_mu <- if (interval_index > 1L) run_target_mean(pattern, interval_index - 1L, n_intervals) else NA_real_
    prev_run_val <- if (interval_index > 1L && length(run_values) >= interval_index - 1L) run_values[interval_index - 1L] else NA_real_
    innovation_scale <- sqrt(max(1e-8, 1 - dep$rho^2))

    for (attempt in seq_len(max_attempts)) {
      base <- sample_bounded_interval(pattern, previous_isi, previous_pattern, exclude_ranges)
      if (!is.finite(base) || base <= 0) next
      proposed <- target_mu + innovation_scale * (base - base_mu)
      if (interval_index > 1L && is.finite(prev_run_val) && is.finite(prev_target_mu)) {
        proposed <- proposed + dep$rho * (prev_run_val - prev_target_mu)
      }
      if (is.finite(proposed) && proposed > 0 && any(x_in_interval_segments(proposed, segments))) {
        return(as.numeric(proposed))
      }
    }

    add_warning(sprintf(
      "%s temporal-dependence sampler failed for interval %d/%d with rho=%.3g and trend=%.3g; adjust temporal dependence or accepted ranges.",
      pattern, as.integer(interval_index), as.integer(n_intervals), dep$rho, dep$trend
    ))
    NA_real_
  }

  has_three_burst_like_isis <- function(intervals) {
    intervals <- as.numeric(intervals)
    intervals <- intervals[is.finite(intervals)]
    if (length(intervals) < 3) return(FALSE)
    brng <- effective_interval_range("Burst")
    if (length(brng) != 2 || any(!is.finite(brng))) return(FALSE)
    flags <- intervals >= brng[1] & intervals <= brng[2]
    rr <- rle(flags)
    any(rr$values & rr$lengths >= 3)
  }

  noisy_run_variability_ok <- function(intervals) {
    intervals <- as.numeric(intervals)
    intervals <- intervals[is.finite(intervals) & intervals > 0]
    spec <- noisy_specificity_from_config(config)
    # One or two Noisy ISIs can be valid benchmark noise if they are not two
    # consecutive Burst-like/Tonic-like ISIs. CV/CV2 is not stable for such short runs.
    if (length(intervals) < spec$toniclike_min_isi_count) return(TRUE)
    m <- isi_regularity_metrics(intervals)
    cv_ok <- is.finite(m$cv) && m$cv >= spec$min_run_cv
    cv2_ok <- is.finite(m$cv2) && m$cv2 >= spec$min_run_cv2
    # Longer Noisy runs must be visibly irregular, not merely non-identical.
    # Both global variability and local CV2 must exceed the clean-label floor.
    isTRUE(cv_ok && cv2_ok)
  }

  noisy_clean_run_pass <- function(intervals) {
    intervals <- as.numeric(intervals)
    intervals <- intervals[is.finite(intervals) & intervals > 0]
    if (length(intervals) == 0) return(FALSE)
    zones <- vapply(intervals, noisy_mode_zone, character(1), config = config, guard_s = 0)
    if (any(zones == "Pause", na.rm = TRUE)) return(FALSE)
    if (noisy_same_zone_pair_violation(intervals, config, guard_s = 0)) return(FALSE)
    if (has_three_burst_like_isis(intervals)) return(FALSE)
    noisy_run_variability_ok(intervals)
  }

  sample_event_intervals <- function(pattern, n_intervals, previous_isi = NA_real_, previous_pattern = NA_character_) {
    if (n_intervals <= 0) return(numeric(0))

    vals <- numeric(n_intervals)
    prev_val <- previous_isi
    prev_pat <- previous_pattern
    burst_rng <- effective_interval_range("Burst")
    noisy_context <- numeric(0)
    tracked_history <- numeric(0)
    if (pattern == "Noisy") {
      # Use the already committed contiguous Noisy run, not only this sampled
      # segment. Adjacent Noisy segments are merged into one episode downstream,
      # so clean-label variability must be checked on the combined run.
      noisy_context <- as.numeric(recent_noisy_history[is.finite(recent_noisy_history)])
      tracked_history <- noisy_context
    }
    if (pattern == "Noisy" && isTRUE(config$avoid_noisy_burst_runs)) {
      if (length(tracked_history) == 0 && is.finite(previous_isi) && identical(previous_pattern, pattern)) {
        tracked_history <- c(previous_isi)
      }
    }

    burst_like <- function(value) {
      length(burst_rng) == 2 && all(is.finite(burst_rng)) &&
        is.finite(value) && value >= burst_rng[1] && value <= burst_rng[2]
    }

    for (i in seq_len(n_intervals)) {
      exclude_ranges <- NULL
      if (pattern == "Noisy" && isTRUE(config$avoid_noisy_burst_runs) && length(tracked_history) >= 2) {
        recent <- tail(tracked_history, 2)
        if (all(vapply(recent, burst_like, logical(1)))) {
          exclude_ranges <- list(burst_rng)
        }
      }

      vals[i] <- sample_temporally_adjusted_interval(
        pattern,
        interval_index = i,
        n_intervals = n_intervals,
        run_values = vals,
        previous_isi = prev_val,
        previous_pattern = prev_pat,
        exclude_ranges = exclude_ranges
      )
      if (!is.finite(vals[i])) return(vals)
      prev_val <- vals[i]
      prev_pat <- pattern
      if (pattern == "Noisy" && isTRUE(config$avoid_noisy_burst_runs)) {
        tracked_history <- c(tracked_history, vals[i])
      }
    }

    if (pattern == "Noisy") {
      combined_vals <- c(noisy_context, vals)
      if (isTRUE(config$avoid_noisy_burst_runs) && has_three_burst_like_isis(combined_vals)) {
        add_warning("Noisy interval sampler produced 3 consecutive burst-like ISIs despite constructive exclusion; this indicates an infeasible Noisy range after constraints.")
        vals[] <- NA_real_
      }
      if (!any(is.na(vals)) && !noisy_clean_run_pass(combined_vals)) {
        if (!isTRUE(noisy_variability_warning_added)) {
          add_warning("Noisy contextual clean-label rule rejected a Noisy run: a Noisy singleton may be Burst-like/Tonic-like only when isolated, but consecutive same-zone Noisy ISIs, Pause-like Noisy ISIs, or overly regular longer Noisy runs are not allowed.")
          noisy_variability_warning_added <<- TRUE
        }
        vals[] <- NA_real_
      }
    }

    vals
  }

  metric_range_ok <- function(value, rng) {
    length(rng) == 2 &&
      all(is.finite(rng)) &&
      rng[2] >= rng[1] &&
      is.finite(value) &&
      value >= rng[1] &&
      value <= rng[2]
  }

  regular_state_intervals_pass <- function(pattern, intervals) {
    ranges <- get_pattern_config(pattern)$regularity_ranges
    if (is.null(ranges)) return(TRUE)
    metrics <- isi_regularity_metrics(intervals)
    checks <- c(
      cv = metric_range_ok(metrics$cv, ranges$cv),
      cv2 = metric_range_ok(metrics$cv2, ranges$cv2),
      lv = metric_range_ok(metrics$lv, ranges$lv)
    )
    if (!is.null(ranges$mm)) checks <- c(checks, mm = metric_range_ok(metrics$mm, ranges$mm))
    all(checks)
  }

  sample_regular_state_intervals <- function(pattern, n_intervals, max_attempts = NULL, previous_isi = NA_real_, previous_pattern = NA_character_) {
    if (is.null(max_attempts)) {
      max_attempts <- if (!is.null(config$tonic_sampler_max_attempts)) as.integer(config$tonic_sampler_max_attempts) else 2000L
    }
    max_attempts <- max(1L, as.integer(max_attempts))
    if (n_intervals < 2) {
      add_warning(sprintf("%s regularity metrics require at least two labeled ISIs; increase its spike-count range.", pattern))
      return(rep(NA_real_, max(n_intervals, 1L)))
    }

    failed_attempt_warnings <- character(0)
    for (attempt in seq_len(max_attempts)) {
      warnings_before_attempt <- warnings
      vals <- numeric(n_intervals)
      prev_val <- previous_isi
      prev_pat <- previous_pattern
      for (i in seq_len(n_intervals)) {
        vals[i] <- sample_temporally_adjusted_interval(
          pattern,
          interval_index = i,
          n_intervals = n_intervals,
          run_values = vals,
          previous_isi = prev_val,
          previous_pattern = prev_pat
        )
        prev_val <- vals[i]
        prev_pat <- pattern
      }
      if (!any(is.na(vals)) && regular_state_intervals_pass(pattern, vals)) return(vals)
      failed_attempt_warnings <- unique(c(failed_attempt_warnings, setdiff(warnings, warnings_before_attempt)))
      warnings <<- warnings_before_attempt
    }

    warnings <<- unique(c(warnings, failed_attempt_warnings))
    metric_names <- names(get_pattern_config(pattern)$regularity_ranges)
    add_warning(sprintf(
      "%s interval sampler failed after %d sequence attempts because %s constraints were not satisfied. Relax the regularity ranges or adjust the ISI distribution.",
      pattern, max_attempts, paste(toupper(metric_names), collapse = "/")
    ))
    rep(NA_real_, n_intervals)
  }

  hf_spiking_intervals_pass <- function(intervals, rules) {
    intervals <- as.numeric(intervals)
    if (length(intervals) == 0 || any(!is.finite(intervals)) || any(intervals <= 0)) return(FALSE)
    short_upper <- max(as.numeric(rules$short_isi_range))
    bridge_upper <- max(as.numeric(rules$bridge_isi_range))
    short_flag <- intervals <= short_upper
    bridge_flag <- intervals > short_upper & intervals <= bridge_upper
    large_flag <- intervals > bridge_upper
    max_consec_bridge <- if (any(bridge_flag)) max(rle(bridge_flag)$lengths[rle(bridge_flag)$values]) else 0L
    duration_ok <- sum(intervals) >= max(0, safe_num(rules$min_duration_s, 0))
    mean(short_flag) >= safe_num(rules$short_fraction_min, 0.70) &&
      mean(bridge_flag | large_flag) <= safe_num(rules$bridge_fraction_max, 0.20) &&
      !any(large_flag) &&
      max_consec_bridge <= max(0L, as.integer(rules$max_consecutive_bridge)) &&
      duration_ok
  }

  sample_high_frequency_spiking_intervals <- function(n_intervals, max_attempts = NULL, previous_isi = NA_real_, previous_pattern = NA_character_) {
    pat_cfg <- get_pattern_config("high_frequency_spiking")
    rules <- pat_cfg$state_rules
    if (is.null(max_attempts)) max_attempts <- max(200L, as.integer(if (!is.null(config$run_sampler_max_attempts)) config$run_sampler_max_attempts else 80L) * 10L)
    max_attempts <- max(1L, as.integer(max_attempts))
    if (n_intervals < HF_SPIKING_MIN_BOUNDARY_SPIKES - 1L) {
      add_warning(sprintf("high_frequency_spiking requires at least %d boundary spikes.", HF_SPIKING_MIN_BOUNDARY_SPIKES))
      return(rep(NA_real_, max(1L, n_intervals)))
    }

    short_rng <- sort(as.numeric(rules$short_isi_range))
    bridge_rng <- sort(as.numeric(rules$bridge_isi_range))
    accepted_rng <- effective_interval_range("high_frequency_spiking")
    global_floor <- max(0, safe_num(config$inter_event_gap, 0))
    if (length(accepted_rng) == 2 && all(is.finite(accepted_rng))) {
      short_rng <- c(max(short_rng[1], accepted_rng[1], global_floor), min(short_rng[2], accepted_rng[2]))
      bridge_rng <- c(max(bridge_rng[1], short_rng[2], accepted_rng[1], global_floor), min(bridge_rng[2], accepted_rng[2]))
    } else {
      short_rng[1] <- max(short_rng[1], global_floor)
      bridge_rng[1] <- max(bridge_rng[1], short_rng[2], global_floor)
    }
    if (short_rng[2] < short_rng[1] || bridge_rng[2] < bridge_rng[1]) {
      add_warning("high_frequency_spiking short/bridge bands are empty after applying the accepted range and absolute refractory period.")
      return(rep(NA_real_, n_intervals))
    }

    base_segments <- effective_pattern_segments_from_config(config, "high_frequency_spiking", previous_isi, previous_pattern)
    short_segments <- intersect_interval_segments(base_segments, short_rng)
    bridge_segments <- intersect_interval_segments(base_segments, bridge_rng)
    short_mass <- if (nrow(short_segments) > 0) interval_segments_mass(pat_cfg$dist_type, pat_cfg$params, short_segments) else 0
    bridge_mass <- if (nrow(bridge_segments) > 0) interval_segments_mass(pat_cfg$dist_type, pat_cfg$params, bridge_segments) else 0
    if (nrow(short_segments) == 0 || !is.finite(short_mass) || short_mass <= 0) {
      add_warning("high_frequency_spiking has no probability mass in its configured short-ISI band.")
      return(rep(NA_real_, n_intervals))
    }

    target_short <- min(1, max(safe_num(rules$short_fraction_min, 0.70), safe_num(rules$target_short_fraction, 0.85)))
    max_bridge_fraction <- min(1, max(0, safe_num(rules$bridge_fraction_max, 0.20)))
    max_consecutive <- max(0L, as.integer(rules$max_consecutive_bridge))
    desired_bridge_n <- min(floor(n_intervals * max_bridge_fraction), round(n_intervals * (1 - target_short)))
    if (desired_bridge_n > 0L && (nrow(bridge_segments) == 0 || !is.finite(bridge_mass) || bridge_mass <= 0)) {
      add_warning("high_frequency_spiking has no probability mass in its moderate bridge-ISI band; the epoch will be generated with short ISIs only.")
      desired_bridge_n <- 0L
    }

    # Optional trend and serial dependence are applied within the short and bridge
    # bands on the log-ISI scale. Dependence is reset when the band changes so a
    # tolerated bridge does not pull the following short ISI out of its HF band.
    dep <- pattern_temporal_dependence("high_frequency_spiking")
    band_center <- function(segments) {
      segments <- normalize_interval_segments(segments)
      if (nrow(segments) == 0) return(NA_real_)
      lo <- min(segments$Start)
      hi <- max(segments$End)
      if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi < lo) return(NA_real_)
      exp((log(lo) + log(hi)) / 2)
    }
    short_center <- band_center(short_segments)
    bridge_center <- band_center(bridge_segments)
    sample_band_value <- function(i, is_bridge, bridge_flag, vals) {
      segments <- if (isTRUE(is_bridge)) bridge_segments else short_segments
      center <- if (isTRUE(is_bridge)) bridge_center else short_center
      base <- sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
      if (!is.finite(base) || base <= 0 || !is.finite(center) || center <= 0) return(base)
      if (abs(dep$rho) < 1e-12 && abs(dep$trend) < 1e-12) return(base)

      pos <- if (n_intervals > 1L) (i - 1) / (n_intervals - 1) - 0.5 else 0
      target_center <- center * exp(dep$trend * pos)
      target_center <- min(max(target_center, min(segments$Start)), max(segments$End))
      innovation_scale <- sqrt(max(1e-8, 1 - dep$rho^2))

      for (proposal_attempt in seq_len(80L)) {
        if (proposal_attempt > 1L) {
          base <- sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
          if (!is.finite(base) || base <= 0) next
        }
        ar_term <- 0
        if (i > 1L && isTRUE(bridge_flag[i - 1L] == is_bridge) && is.finite(vals[i - 1L]) && vals[i - 1L] > 0) {
          prev_pos <- if (n_intervals > 1L) (i - 2) / (n_intervals - 1) - 0.5 else 0
          prev_target <- center * exp(dep$trend * prev_pos)
          prev_target <- min(max(prev_target, min(segments$Start)), max(segments$End))
          ar_term <- dep$rho * log(vals[i - 1L] / prev_target)
        }
        proposal <- exp(log(target_center) + ar_term + innovation_scale * log(base / center))
        if (is.finite(proposal) && proposal > 0 && any(x_in_interval_segments(proposal, segments))) return(proposal)
      }
      base
    }

    for (attempt in seq_len(max_attempts)) {
      bridge_flag <- rep(FALSE, n_intervals)
      if (desired_bridge_n > 0L && max_consecutive > 0L) {
        candidate_order <- sample(seq_len(n_intervals), n_intervals)
        for (idx in candidate_order) {
          trial <- bridge_flag
          trial[idx] <- TRUE
          rr <- rle(trial)
          longest <- if (any(rr$values)) max(rr$lengths[rr$values]) else 0L
          if (longest <= max_consecutive) bridge_flag <- trial
          if (sum(bridge_flag) >= desired_bridge_n) break
        }
      }

      vals <- numeric(n_intervals)
      for (i in seq_len(n_intervals)) {
        vals[i] <- sample_band_value(i, bridge_flag[i], bridge_flag, vals)
      }
      if (hf_spiking_intervals_pass(vals, rules)) return(vals)
    }

    add_warning(sprintf(
      "high_frequency_spiking sampler failed after %d attempts; relax short/bridge fractions, duration, or ISI ranges.",
      max_attempts
    ))
    rep(NA_real_, n_intervals)
  }

  minimum_boundary_spikes_for_pattern <- function(pattern) {
    if (identical(pattern, "Pause")) return(2L)
    if (identical(pattern, "Burst")) return(BURST_MIN_BOUNDARY_SPIKES)
    if (identical(pattern, "Tonic")) return(TONIC_MIN_BOUNDARY_SPIKES)
    if (identical(pattern, "high_frequency_tonic")) return(HF_TONIC_MIN_BOUNDARY_SPIKES)
    if (identical(pattern, "high_frequency_spiking")) return(HF_SPIKING_MIN_BOUNDARY_SPIKES)
    if (identical(pattern, "Noisy")) return(2L)
    2L
  }

  sample_spike_count <- function(pattern) {
    rng <- get_pattern_config(pattern)$spike_count_range
    rng <- as.integer(round(rng))
    min_required <- minimum_boundary_spikes_for_pattern(pattern)
    if (length(rng) == 2 && pattern != "Pause") {
      rng[1] <- max(rng[1], min_required)
      rng[2] <- max(rng[2], rng[1])
    }
    if (length(rng) != 2 || any(is.na(rng)) || rng[2] < rng[1]) {
      fallback <- min_required
      add_warning(sprintf("%s spike-count range is invalid for interval-label generation; using the minimum count that defines a labeled run.", pattern))
      return(fallback)
    }
    sample_closed_int(rng[1], rng[2])
  }

  min_spike_count <- function(pattern, value = NA_real_) {
    if (pattern == "Pause") return(2L)
    if (!is.na(value) && is.finite(value) && value > 0) {
      n <- as.integer(round(value))
    } else {
      rng <- get_pattern_config(pattern)$spike_count_range
      default_min <- minimum_boundary_spikes_for_pattern(pattern)
      n <- if (length(rng) == 2 && all(is.finite(rng))) as.integer(round(min(rng))) else default_min
    }
    max(minimum_boundary_spikes_for_pattern(pattern), n)
  }

  minimum_run_duration <- function(pattern, n_intervals = NULL, previous_isi = last_isi_value, previous_pattern = last_isi_pattern) {
    if (is.null(n_intervals) || !is.finite(n_intervals)) {
      n_intervals <- if (pattern == "Pause") 1L else max(1L, min_spike_count(pattern) - 1L)
    }
    n_intervals <- as.integer(max(1L, n_intervals))
    first_segments <- effective_pattern_segments_from_config(config, pattern, previous_isi, previous_pattern)
    first_min <- minimum_segment_start(first_segments)
    if (!is.finite(first_min)) return(NA_real_)
    base_duration <- if (n_intervals == 1L) {
      first_min
    } else {
      internal_segments <- effective_pattern_segments_from_config(config, pattern, NA_real_, NA_character_)
      internal_min <- minimum_segment_start(internal_segments)
      if (!is.finite(internal_min)) return(NA_real_)
      first_min + (n_intervals - 1L) * internal_min
    }

    # If no real spike has occurred yet, a positive first-spike latency is required.
    # For an initial Pause in leading-silence mode, the first Pause duration itself is
    # that latency and is already represented in base_duration.
    if (!isTRUE(has_current_spike) && !(identical(pattern, "Pause") && isTRUE(leading_silence_initial_pause))) {
      latency_segments <- effective_pattern_segments_from_config(config, pattern, NA_real_, NA_character_)
      latency_min <- minimum_segment_start(latency_segments)
      if (!is.finite(latency_min)) return(NA_real_)
      base_duration <- latency_min + base_duration
    }
    base_duration
  }

  choose_interval_label <- function(excluded = character(0)) {
    ratios <- config$ratios
    if (config$generation_mode == "time") {
      remaining_target <- ratios * total_time - pattern_duration
      weights <- pmax(remaining_target, 0)
      if (sum(weights) <= 0) weights <- ratios
    } else {
      weights <- ratios
    }
    # Same-label continuation is allowed in V13.5.0; adjacent intervals with the same label
    # are collapsed into one episode summary after the ISI sequence has been generated.
    if (length(excluded) > 0) weights[excluded] <- 0
    for (pat in names(weights)) {
      min_dur <- minimum_run_duration(pat)
      if (!is.finite(min_dur) || current_time + min_dur > total_time + 1e-12) {
        weights[pat] <- 0
      }
    }
    if (sum(weights) <= 0) {
      add_warning("No feasible interval label with a positive ratio was available under the current duration, range, and adjacency rules.")
      return(NA_character_)
    }
    sample(names(weights), size = 1, prob = weights)
  }

  initial_latency_model_value <- function() {
    mode <- if (!is.null(config$initial_latency_model)) as.character(config$initial_latency_model)[1] else "residual_life"
    if (!mode %in% c("residual_life", "same_distribution", "uniform")) mode <- "residual_life"
    mode
  }

  sample_same_distribution_latency <- function(label, max_latency = Inf) {
    pat_cfg <- get_pattern_config(label)
    segments <- effective_pattern_segments_from_config(config, label, NA_real_, NA_character_)
    segments <- intersect_interval_segments(segments, c(.Machine$double.eps, max_latency))
    if (nrow(segments) == 0) return(NA_real_)
    mass <- interval_segments_mass(pat_cfg$dist_type, pat_cfg$params, segments)
    if (!is.finite(mass) || mass <= 0) return(NA_real_)
    sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
  }

  sample_residual_life_latency <- function(label, max_latency = Inf, max_attempts = NULL) {
    if (is.null(max_attempts)) {
      max_attempts <- if (!is.null(config$residual_life_latency_max_attempts)) as.integer(config$residual_life_latency_max_attempts) else 500L
    }
    max_attempts <- max(1L, as.integer(max_attempts))
    # Equilibrium-renewal approximation for a randomly cut recording window.
    # Draw a length-biased full ISI, then draw the residual waiting time within that interval.
    pat_cfg <- get_pattern_config(label)
    full_segments <- effective_pattern_segments_from_config(config, label, NA_real_, NA_character_)
    full_segments <- normalize_interval_segments(full_segments)
    if (nrow(full_segments) == 0) return(NA_real_)
    full_max <- max(full_segments$End)
    if (!is.finite(full_max) || full_max <= 0) return(NA_real_)
    mass <- interval_segments_mass(pat_cfg$dist_type, pat_cfg$params, full_segments)
    if (!is.finite(mass) || mass <= 0) return(NA_real_)
    for (attempt in seq_len(max_attempts)) {
      full_isi <- sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, full_segments)
      if (!is.finite(full_isi) || full_isi <= 0) next
      if (stats::runif(1) > min(1, full_isi / full_max)) next
      residual <- stats::runif(1, min = .Machine$double.eps, max = full_isi)
      if (is.finite(residual) && residual > 0 && residual <= max_latency) return(residual)
    }
    NA_real_
  }

  sample_initial_latency <- function(label, max_latency = Inf) {
    max_latency <- as.numeric(max_latency)
    if (!is.finite(max_latency) || max_latency <= 0) return(NA_real_)
    mode <- initial_latency_model_value()
    val <- NA_real_
    if (identical(mode, "uniform")) {
      val <- stats::runif(1, min = .Machine$double.eps, max = max_latency)
    } else if (identical(mode, "same_distribution")) {
      val <- sample_same_distribution_latency(label, max_latency = max_latency)
    } else {
      val <- sample_residual_life_latency(label, max_latency = max_latency)
      if (!is.finite(val) || val <= 0) {
        val <- sample_same_distribution_latency(label, max_latency = max_latency)
      }
    }
    if (!is.finite(val) || val <= 0) {
      add_warning(sprintf(
        "No feasible positive initial latency was available before the first %s-labeled ISI run within the remaining time %.6g s under the '%s' latency model. The recording boundary at 0 s is not allowed to be a spike.",
        label, max_latency, mode
      ))
      return(NA_real_)
    }
    val
  }

  append_initial_latency <- function(label, max_latency = Inf, source = "simulated") {
    if (isTRUE(has_current_spike)) return(TRUE)
    latency <- sample_initial_latency(label, max_latency = max_latency)
    if (!is.finite(latency) || latency <= 0) return(FALSE)
    start_time <- current_time
    end_time <- start_time + latency
    if (!is.finite(end_time) || end_time > total_time + 1e-12) return(FALSE)
    current_time <<- end_time
    spike_times <<- c(spike_times, current_time)
    leading_silence_end_times <<- c(leading_silence_end_times, current_time)
    has_current_spike <<- TRUE

    leading_episode_count <<- leading_episode_count + 1L
    initial_latency_row <- data.frame(
      Episode = NA_integer_,
      Pattern = "Latency",
      Episode_Scope = "initial_latency",
      Latency_Context = label,
      Latency_Model = initial_latency_model_value(),
      Start = start_time,
      End = end_time,
      Episode_Duration = latency,
      Core_Start = NA_real_,
      Core_End = NA_real_,
      Core_Duration = NA_real_,
      First_Spike_Time = end_time,
      Last_Spike_Time = end_time,
      N_Spikes = 1L,
      N_ISIs = 0L,
      N_Boundary_Spikes = 1L,
      N_New_Spikes = 1L,
      N_Shared_Boundary_Spikes = 0L,
      Mean_Within_Episode_ISI = NA_real_,
      CV_Within_Episode_ISI = NA_real_,
      Mean_CV2_Within_Episode_ISI = NA_real_,
      LV_Within_Episode_ISI = NA_real_,
      Core_ISI_Rate_Hz = NA_real_,
      Episode_Inclusive_Rate_Hz = NA_real_,
      stringsAsFactors = FALSE
    )
    tmp_leading_episode_rows <- leading_episode_rows
    tmp_leading_episode_rows[[leading_episode_count]] <- initial_latency_row
    leading_episode_rows <<- tmp_leading_episode_rows

    if (!isTRUE(initial_latency_warning_added)) {
      add_warning("The first real spike was generated after a positive initial latency; t = 0 is treated as the recording boundary, not as a spike. This latency is not entered into the interval table as an ISI.")
      initial_latency_warning_added <<- TRUE
    }
    TRUE
  }

  ensure_left_boundary_spike <- function(label, max_latency = Inf, source = "simulated") {
    if (isTRUE(has_current_spike)) return(TRUE)
    append_initial_latency(label, max_latency = max_latency, source = source)
  }

  append_labeled_interval <- function(label, duration, fixed = FALSE, source = "simulated",
                                      run_position = NA_real_, run_length = NA_integer_,
                                      temporal_rho = NA_real_, temporal_trend = NA_real_) {
    if (!is.finite(duration) || duration <= 0) return(FALSE)
    if (current_time + duration > total_time + 1e-12) return(FALSE)
    if (!ensure_left_boundary_spike(label, max_latency = total_time - current_time - duration, source = source)) return(FALSE)
    if (current_time + duration > total_time + 1e-12) return(FALSE)
    start_time <- current_time
    end_time <- start_time + duration
    interval_seq_id <<- interval_seq_id + 1L
    scope <- if (identical(label, "Pause")) "pause_isi" else "within_episode"
    interval_row <- data.frame(
      Interval_Seq_ID = interval_seq_id,
      Start_Time_s = start_time,
      End_Time_s = end_time,
      ISI_s = duration,
      ISI_Label = label,
      ISI_Scope = scope,
      Is_Manual_Fixed = isTRUE(fixed),
      Interval_Source = source,
      Run_Position = as.numeric(run_position),
      Run_Length = as.integer(run_length),
      Temporal_Rho = as.numeric(temporal_rho),
      Temporal_Trend = as.numeric(temporal_trend),
      stringsAsFactors = FALSE
    )
    tmp_interval_rows <- interval_rows
    tmp_interval_rows[[interval_seq_id]] <- interval_row
    interval_rows <<- tmp_interval_rows
    current_time <<- end_time
    spike_times <<- c(spike_times, current_time)
    tmp_pattern_duration <- pattern_duration
    tmp_pattern_duration[label] <- tmp_pattern_duration[label] + duration
    pattern_duration <<- tmp_pattern_duration
    last_isi_value <<- duration
    last_isi_pattern <<- label
    if (identical(label, "Noisy")) {
      recent_noisy_history <<- c(recent_noisy_history, duration)
    } else {
      recent_noisy_history <<- numeric(0)
    }
    TRUE
  }

  append_leading_silence <- function(duration, latency_context = "Pause", latency_model = "specified_duration") {
    if (!is.finite(duration) || duration <= 0) return(FALSE)
    if (current_time + duration > total_time + 1e-12) return(FALSE)
    start_time <- current_time
    end_time <- start_time + duration
    current_time <<- end_time
    spike_times <<- c(spike_times, current_time)
    leading_silence_end_times <<- c(leading_silence_end_times, current_time)
    has_current_spike <<- TRUE
    leading_episode_count <<- leading_episode_count + 1L
    leading_episode_row <- data.frame(
      Episode = NA_integer_,
      Pattern = "Latency",
      Episode_Scope = "leading_latency",
      Latency_Context = latency_context,
      Latency_Model = latency_model,
      Start = start_time,
      End = end_time,
      Episode_Duration = duration,
      Core_Start = NA_real_,
      Core_End = NA_real_,
      Core_Duration = NA_real_,
      First_Spike_Time = end_time,
      Last_Spike_Time = end_time,
      N_Spikes = 1L,
      N_ISIs = 0L,
      N_Boundary_Spikes = 1L,
      N_New_Spikes = 1L,
      N_Shared_Boundary_Spikes = 0L,
      Mean_Within_Episode_ISI = NA_real_,
      CV_Within_Episode_ISI = NA_real_,
      Mean_CV2_Within_Episode_ISI = NA_real_,
      LV_Within_Episode_ISI = NA_real_,
      Core_ISI_Rate_Hz = NA_real_,
      Episode_Inclusive_Rate_Hz = NA_real_,
      stringsAsFactors = FALSE
    )
    tmp_leading_episode_rows <- leading_episode_rows
    tmp_leading_episode_rows[[leading_episode_count]] <- leading_episode_row
    leading_episode_rows <<- tmp_leading_episode_rows
    if (!isTRUE(initial_pause_warning_added)) {
      add_warning("Initial Pause was treated as leading silence by default: the recording boundary at 0 s is not a spike, so this first duration is recorded as latency rather than as a biological ISI and is not entered into the interval table.")
      initial_pause_warning_added <<- TRUE
    }
    TRUE
  }

  validate_manual_interval_value <- function(pattern, value, previous_isi = NA_real_, previous_pattern = NA_character_) {
    segments <- effective_pattern_segments_from_config(config, pattern, previous_isi, previous_pattern)
    value_in_segments(value, segments)
  }

  sample_pause_duration_sequence <- function(n_intervals, fixed_durations = NULL, source = "manual") {
    if (n_intervals <= 0) return(numeric(0))
    if (is.null(fixed_durations)) fixed_durations <- rep(NA_real_, n_intervals)
    if (length(fixed_durations) < n_intervals) fixed_durations <- rep(fixed_durations, length.out = n_intervals)

    vals <- numeric(n_intervals)
    prev_val <- last_isi_value
    prev_pat <- last_isi_pattern
    for (i in seq_len(n_intervals)) {
      fixed_value <- suppressWarnings(as.numeric(fixed_durations[i]))
      if (is.finite(fixed_value)) {
        if (!validate_manual_interval_value("Pause", fixed_value, prev_val, prev_pat)) {
          pause_segments <- effective_pattern_segments_from_config(config, "Pause", prev_val, prev_pat)
          segment_text <- if (nrow(pause_segments) > 0) {
            paste(sprintf("[%.6g, %.6g]", pause_segments$Start, pause_segments$End), collapse = ", ")
          } else {
            "<empty>"
          }
          add_warning(sprintf(
            "Manual Pause duration %.6g s is outside the feasible Pause segment set %s after accepted-range, absolute refractory period, and adjacency checks.",
            fixed_value, segment_text
          ))
          return(NULL)
        }
        vals[i] <- fixed_value
      } else {
        vals[i] <- sample_temporally_adjusted_interval(
          "Pause",
          interval_index = i,
          n_intervals = n_intervals,
          run_values = vals,
          previous_isi = prev_val,
          previous_pattern = prev_pat
        )
        if (!is.finite(vals[i])) return(NULL)
      }
      prev_val <- vals[i]
      prev_pat <- "Pause"
    }
    vals
  }

  generate_run_durations <- function(pattern, n_intervals, fixed_durations = NULL, source = "simulated", max_attempts = NULL) {
    if (is.null(max_attempts)) {
      max_attempts <- if (!is.null(config$run_sampler_max_attempts)) as.integer(config$run_sampler_max_attempts) else 80L
    }
    max_attempts <- max(1L, as.integer(max_attempts))
    n_intervals <- as.integer(n_intervals)
    if (!is.finite(n_intervals) || n_intervals <= 0L) return(numeric(0))
    if (forbidden_hf_burst_adjacency(pattern, last_isi_pattern)) {
      add_warning(sprintf(
        "%s cannot be generated immediately after %s: Burst and high-frequency tonic/spiking states must be separated by another label to avoid ambiguous episode boundaries.",
        pattern, last_isi_pattern
      ))
      return(NULL)
    }

    if (identical(pattern, "Pause")) {
      vals <- sample_pause_duration_sequence(n_intervals, fixed_durations, source = source)
      if (is.null(vals) || any(!is.finite(vals)) || any(vals <= 0)) return(NULL)
      if (current_time + sum(vals) > total_time + 1e-12) return(NULL)
      return(vals)
    }

    failed_attempt_warnings <- character(0)
    for (attempt in seq_len(max_attempts)) {
      warnings_before_attempt <- warnings
      vals <- if (pattern %in% c("Tonic", "high_frequency_tonic")) {
        sample_regular_state_intervals(pattern, n_intervals, previous_isi = last_isi_value, previous_pattern = last_isi_pattern)
      } else if (identical(pattern, "high_frequency_spiking")) {
        sample_high_frequency_spiking_intervals(n_intervals, previous_isi = last_isi_value, previous_pattern = last_isi_pattern)
      } else {
        sample_event_intervals(pattern, n_intervals, previous_isi = last_isi_value, previous_pattern = last_isi_pattern)
      }
      if (!any(is.na(vals)) && all(is.finite(vals)) && all(vals > 0) && current_time + sum(vals) <= total_time + 1e-12) {
        return(vals)
      }
      failed_attempt_warnings <- unique(c(failed_attempt_warnings, setdiff(warnings, warnings_before_attempt)))
      warnings <<- warnings_before_attempt
    }
    warnings <<- unique(c(warnings, failed_attempt_warnings))
    add_warning(sprintf(
      "%s interval run could not be generated within the remaining %.6g s after %d attempts.",
      pattern, max(0, total_time - current_time), as.integer(max_attempts)
    ))
    NULL
  }

  append_interval_run <- function(pattern, n_intervals, fixed_durations = NULL, source = "manual") {
    n_intervals <- as.integer(n_intervals)
    if (!is.finite(n_intervals) || n_intervals <= 0L) return(TRUE)

    if (identical(pattern, "Pause") && !isTRUE(has_current_spike) && current_time <= 1e-12 && isTRUE(leading_silence_initial_pause)) {
      first_fixed <- if (!is.null(fixed_durations) && length(fixed_durations) >= 1L) fixed_durations[1] else NA_real_
      first_duration <- sample_pause_duration_sequence(1L, first_fixed, source = source)
      if (is.null(first_duration) || !is.finite(first_duration[1]) || first_duration[1] <= 0) return(FALSE)
      latency_model <- if (is.finite(suppressWarnings(as.numeric(first_fixed)))) "specified_duration" else "pause_distribution_duration"
      if (!append_leading_silence(first_duration[1], latency_context = "Pause", latency_model = latency_model)) return(FALSE)
      n_intervals <- n_intervals - 1L
      if (n_intervals <= 0L) return(TRUE)
      fixed_durations <- if (!is.null(fixed_durations) && length(fixed_durations) >= 2L) fixed_durations[-1] else rep(NA_real_, n_intervals)
    }

    durations <- generate_run_durations(pattern, n_intervals, fixed_durations = fixed_durations, source = source)
    if (is.null(durations) || length(durations) != n_intervals) return(FALSE)
    if (!isTRUE(has_current_spike)) {
      max_initial_latency <- total_time - current_time - sum(durations)
      if (!ensure_left_boundary_spike(pattern, max_latency = max_initial_latency, source = source)) return(FALSE)
    }
    fixed_flags <- if (!is.null(fixed_durations) && length(fixed_durations) > 0) {
      is.finite(suppressWarnings(as.numeric(rep(fixed_durations, length.out = n_intervals))))
    } else {
      rep(FALSE, n_intervals)
    }
    if (current_time + sum(durations) > total_time + 1e-12) return(FALSE)
    dep <- pattern_temporal_dependence(pattern)
    for (i in seq_along(durations)) {
      run_position <- if (length(durations) > 1L) (i - 1) / (length(durations) - 1) else 0
      if (!append_labeled_interval(
        pattern, durations[i], fixed = fixed_flags[i], source = source,
        run_position = run_position, run_length = length(durations),
        temporal_rho = dep$rho, temporal_trend = dep$trend
      )) return(FALSE)
    }
    TRUE
  }

  expanded_manual_entries <- function(pattern_seq) {
    if (is.null(pattern_seq) || length(pattern_seq) == 0) return(list())
    entries <- list()
    idx <- 0L
    for (entry in pattern_seq) {
      repeat_count <- max(1L, as.integer(entry$Repeat))
      for (rep_i in seq_len(repeat_count)) {
        idx <- idx + 1L
        entries[[idx]] <- list(
          Pattern = entry$Pattern,
          Value = entry$Value
        )
      }
    }
    entries
  }

  group_manual_entries <- function(entries) {
    groups <- list()
    group_i <- 0L
    for (entry in entries) {
      pattern <- entry$Pattern
      value <- suppressWarnings(as.numeric(entry$Value))
      if (group_i > 0L && identical(groups[[group_i]]$Pattern, pattern)) {
        if (identical(pattern, "Pause")) {
          groups[[group_i]]$Fixed_Durations <- c(groups[[group_i]]$Fixed_Durations, if (is.finite(value)) value else NA_real_)
        } else {
          groups[[group_i]]$Spike_Counts <- c(groups[[group_i]]$Spike_Counts, if (is.finite(value)) value else NA_real_)
        }
      } else {
        group_i <- group_i + 1L
        if (identical(pattern, "Pause")) {
          groups[[group_i]] <- list(Pattern = pattern, Fixed_Durations = c(if (is.finite(value)) value else NA_real_))
        } else {
          groups[[group_i]] <- list(Pattern = pattern, Spike_Counts = c(if (is.finite(value)) value else NA_real_))
        }
      }
    }
    groups
  }

  interval_count_from_spike_counts <- function(pattern, spike_counts) {
    if (identical(pattern, "Pause")) return(length(spike_counts))
    if (length(spike_counts) == 0) return(max(1L, min_spike_count(pattern) - 1L))
    counts <- vapply(spike_counts, function(x) {
      if (is.finite(x) && x > 0) {
        n <- as.integer(round(x))
      } else {
        n <- sample_spike_count(pattern)
      }
      if (identical(pattern, "Burst")) {
        max(BURST_MIN_BOUNDARY_SPIKES, n)
      } else if (identical(pattern, "Tonic")) {
        max(TONIC_MIN_BOUNDARY_SPIKES, n)
      } else if (identical(pattern, "Noisy")) {
        max(2L, n)
      } else {
        max(2L, n)
      }
    }, integer(1))
    sum(pmax(1L, counts - 1L))
  }

  run_manual_sequence <- function(pattern_seq) {
    groups <- group_manual_entries(expanded_manual_entries(pattern_seq))
    if (length(groups) == 0) return(TRUE)
    for (group in groups) {
      pattern <- group$Pattern
      if (identical(pattern, "Pause")) {
        n_intervals <- length(group$Fixed_Durations)
        ok <- append_interval_run(pattern, n_intervals, fixed_durations = group$Fixed_Durations, source = "manual")
      } else {
        n_intervals <- interval_count_from_spike_counts(pattern, group$Spike_Counts)
        ok <- append_interval_run(pattern, n_intervals, source = "manual")
      }
      if (!isTRUE(ok)) {
        add_warning(sprintf("Stopped manual interval-label sequence at %.4f s because label run '%s' could not fit or be generated.", current_time, pattern))
        return(FALSE)
      }
      if (current_time >= total_time) break
    }
    TRUE
  }

  run_automatic_sequence <- function() {
    max_auto_runs <- if (!is.null(config$max_auto_runs)) as.integer(config$max_auto_runs) else 10000L
    max_auto_runs <- max(1L, max_auto_runs)
    auto_run_counter <- 0L
    while (current_time < total_time - 1e-12) {
      auto_run_counter <- auto_run_counter + 1L
      if (auto_run_counter > max_auto_runs) {
        add_warning(sprintf("Stopped at %.4f s after reaching the automatic-run safety cap of %d runs; increase max_auto_runs for long simulations.", current_time, max_auto_runs))
        break
      }
      result_ok <- FALSE
      failed_attempt_warnings <- character(0)
      excluded <- character(0)
      for (attempt in seq_len(length(SPIKE_PATTERN_LEVELS))) {
        candidate_pattern <- choose_interval_label(excluded = excluded)
        if (is.na(candidate_pattern)) break
        n_intervals <- if (identical(candidate_pattern, "Pause")) {
          1L
        } else {
          max(1L, sample_spike_count(candidate_pattern) - 1L)
        }
        warnings_before_attempt <- warnings
        ok <- append_interval_run(candidate_pattern, n_intervals, source = "auto")
        if (isTRUE(ok)) {
          result_ok <- TRUE
          break
        }
        failed_attempt_warnings <- unique(c(failed_attempt_warnings, setdiff(warnings, warnings_before_attempt)))
        warnings <<- warnings_before_attempt
        excluded <- unique(c(excluded, candidate_pattern))
      }
      if (!isTRUE(result_ok)) {
        warnings <<- unique(c(warnings, failed_attempt_warnings))
        add_warning(sprintf("Stopped at %.4f s because no feasible interval-label run fit in the remaining time.", current_time))
        break
      }
    }
    TRUE
  }

  if (!is.null(config$pattern_sequence)) {
    run_manual_sequence(config$pattern_sequence)
  } else {
    run_automatic_sequence()
  }

  if (is.finite(current_time) && is.finite(total_time) && current_time < total_time - 1e-9) {
    add_warning(sprintf("Achieved duration %.4f s is shorter than requested duration %.4f s.", current_time, total_time))
  }

  interval_seq <- if (length(interval_rows) > 0) {
    do.call(rbind, interval_rows)
  } else {
    data.frame(
      Interval_Seq_ID = integer(0),
      Start_Time_s = numeric(0),
      End_Time_s = numeric(0),
      ISI_s = numeric(0),
      ISI_Label = character(0),
      ISI_Scope = character(0),
      Is_Manual_Fixed = logical(0),
      Interval_Source = character(0),
      Run_Position = numeric(0),
      Run_Length = integer(0),
      Temporal_Rho = numeric(0),
      Temporal_Trend = numeric(0),
      Event_Epoch_Type = character(0),
      Event_Epoch_Source = character(0),
      Event_Epoch_Generation_Rule = character(0),
      stringsAsFactors = FALSE
    )
  }

  if (length(spike_times) > 0) {
    spike_times <- sort(as.numeric(spike_times[is.finite(spike_times)]))
    spike_times <- spike_times[!duplicated(signif(spike_times, 14))]
  }

  if (nrow(interval_seq) > 0) {
    interval_seq <- interval_seq[order(interval_seq$Interval_Seq_ID), , drop = FALSE]
    for (col in c("Event_Epoch_Type", "Event_Epoch_Source", "Event_Epoch_Generation_Rule")) {
      if (!col %in% names(interval_seq)) interval_seq[[col]] <- NA_character_
    }
    break_flags <- c(TRUE, interval_seq$ISI_Label[-1] != interval_seq$ISI_Label[-nrow(interval_seq)])
    run_ids <- cumsum(break_flags)
    interval_seq$Episode <- as.integer(run_ids + leading_episode_count)
  } else {
    interval_seq$Episode <- integer(0)
  }

  match_time_index <- function(time_value) {
    if (length(spike_times) == 0 || !is.finite(time_value)) return(NA_integer_)
    which.min(abs(spike_times - time_value))
  }

  intervals <- make_empty_interval_df()
  if (nrow(interval_seq) > 0) {
    intervals <- data.frame(
      Train = rep(1L, nrow(interval_seq)),
      Interval_ID = seq_len(nrow(interval_seq)),
      Left_Spike_Index = vapply(interval_seq$Start_Time_s, match_time_index, integer(1)),
      Right_Spike_Index = vapply(interval_seq$End_Time_s, match_time_index, integer(1)),
      Left_Spike_Time_s = interval_seq$Start_Time_s,
      Right_Spike_Time_s = interval_seq$End_Time_s,
      Start_Time_s = interval_seq$Start_Time_s,
      End_Time_s = interval_seq$End_Time_s,
      ISI_s = interval_seq$ISI_s,
      Interval = interval_seq$ISI_s,
      ISI_Label = interval_seq$ISI_Label,
      Episode = interval_seq$Episode,
      ISI_Scope = interval_seq$ISI_Scope,
      Left_Spike_Role = NA_character_,
      Right_Spike_Role = NA_character_,
      Left_Episode_Context = NA_character_,
      Right_Episode_Context = NA_character_,
      Is_Manual_Fixed = interval_seq$Is_Manual_Fixed,
      Interval_Source = interval_seq$Interval_Source,
      Run_Position = interval_seq$Run_Position,
      Run_Length = interval_seq$Run_Length,
      Temporal_Rho = interval_seq$Temporal_Rho,
      Temporal_Trend = interval_seq$Temporal_Trend,
      Event_Epoch_Type = interval_seq$Event_Epoch_Type,
      Event_Epoch_Source = interval_seq$Event_Epoch_Source,
      Event_Epoch_Generation_Rule = interval_seq$Event_Epoch_Generation_Rule,
      stringsAsFactors = FALSE
    )
  }

  episode_rows <- list()
  episode_count <- 0L
  if (length(leading_episode_rows) > 0) {
    for (lead_row in leading_episode_rows) {
      episode_count <- episode_count + 1L
      lead_row$Episode <- episode_count
      episode_rows[[episode_count]] <- lead_row
    }
  }

  if (nrow(intervals) > 0) {
    for (episode_id in sort(unique(intervals$Episode))) {
      idx <- which(intervals$Episode == episode_id)
      idx <- idx[order(intervals$Interval_ID[idx])]
      label <- intervals$ISI_Label[idx[1]]
      vals <- intervals$ISI_s[idx]
      regularity <- isi_regularity_metrics(vals)
      start_time <- intervals$Start_Time_s[idx[1]]
      end_time <- intervals$End_Time_s[idx[length(idx)]]
      duration <- end_time - start_time
      n_isis <- length(idx)
      n_boundary <- n_isis + 1L
      shared_with_latency <- length(leading_silence_end_times) > 0 && any(abs(start_time - leading_silence_end_times) <= 1e-9)
      shared_left <- as.integer(min(idx) > 1L || isTRUE(shared_with_latency))
      episode_count <- episode_count + 1L
      episode_rows[[episode_count]] <- data.frame(
        Episode = episode_id,
        Pattern = label,
        Episode_Scope = "interval_run",
        Latency_Context = NA_character_,
        Latency_Model = NA_character_,
        Start = start_time,
        End = end_time,
        Episode_Duration = duration,
        Core_Start = start_time,
        Core_End = end_time,
        Core_Duration = duration,
        First_Spike_Time = start_time,
        Last_Spike_Time = end_time,
        N_Spikes = n_boundary,
        N_ISIs = n_isis,
        N_Boundary_Spikes = n_boundary,
        N_New_Spikes = n_boundary - shared_left,
        N_Shared_Boundary_Spikes = shared_left,
        Mean_Within_Episode_ISI = regularity$mean,
        CV_Within_Episode_ISI = regularity$cv,
        Mean_CV2_Within_Episode_ISI = regularity$cv2,
        LV_Within_Episode_ISI = regularity$lv,
        Core_ISI_Rate_Hz = if (duration > 0) n_isis / duration else NA_real_,
        Episode_Inclusive_Rate_Hz = if (duration > 0) n_boundary / duration else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  episodes <- if (length(episode_rows) > 0) {
    out <- do.call(rbind, episode_rows)
    out <- out[order(out$Episode), , drop = FALSE]
    rownames(out) <- NULL
    out
  } else {
    make_empty_episode_df()
  }

  spikes <- if (length(spike_times) > 0) {
    spike_indices <- seq_along(spike_times)
    left_labels <- rep(NA_character_, length(spike_times))
    right_labels <- rep(NA_character_, length(spike_times))
    left_episodes <- rep(NA_integer_, length(spike_times))
    right_episodes <- rep(NA_integer_, length(spike_times))
    if (nrow(intervals) > 0) {
      for (i in seq_len(nrow(intervals))) {
        li <- intervals$Left_Spike_Index[i]
        ri <- intervals$Right_Spike_Index[i]
        if (is.finite(li) && li >= 1 && li <= length(spike_times)) {
          right_labels[li] <- intervals$ISI_Label[i]
          right_episodes[li] <- intervals$Episode[i]
        }
        if (is.finite(ri) && ri >= 1 && ri <= length(spike_times)) {
          left_labels[ri] <- intervals$ISI_Label[i]
          left_episodes[ri] <- intervals$Episode[i]
        }
      }
    }
    episode_context <- ifelse(!is.na(right_labels), right_labels, left_labels)
    spike_episode <- ifelse(!is.na(right_episodes), right_episodes, left_episodes)
    spike_role <- rep("event_spike", length(spike_times))
    for (i in seq_along(spike_times)) {
      has_left <- !is.na(left_labels[i])
      has_right <- !is.na(right_labels[i])
      is_leading_silence_end <- length(leading_silence_end_times) > 0 &&
        any(abs(spike_times[i] - leading_silence_end_times) <= 1e-9)
      if (isTRUE(is_leading_silence_end) && has_right) {
        spike_role[i] <- "leading_silence_end_shared_boundary_spike"
      } else if (isTRUE(is_leading_silence_end)) {
        spike_role[i] <- "leading_silence_end_spike"
      } else if (has_left && has_right && !identical(left_labels[i], right_labels[i])) {
        spike_role[i] <- "shared_boundary_spike"
      } else if (!has_left && has_right && identical(right_labels[i], "Pause")) {
        spike_role[i] <- "pause_left_boundary_spike"
      } else if (has_left && !has_right && identical(left_labels[i], "Pause")) {
        spike_role[i] <- "pause_right_boundary_spike"
      } else {
        spike_role[i] <- "episode_spike"
      }
    }
    data.frame(
      Episode = suppressWarnings(as.integer(spike_episode)),
      Time = spike_times,
      Episode_Context = episode_context,
      Spike_Role = spike_role,
      stringsAsFactors = FALSE
    )
  } else {
    make_empty_spike_df()
  }

  if (nrow(intervals) > 0 && nrow(spikes) > 0) {
    intervals$Left_Spike_Role <- spikes$Spike_Role[intervals$Left_Spike_Index]
    intervals$Right_Spike_Role <- spikes$Spike_Role[intervals$Right_Spike_Index]
    intervals$Left_Episode_Context <- spikes$Episode_Context[intervals$Left_Spike_Index]
    intervals$Right_Episode_Context <- spikes$Episode_Context[intervals$Right_Spike_Index]
  }

  rownames(spikes) <- NULL
  rownames(episodes) <- NULL
  rownames(intervals) <- NULL
  list(spikes = spikes, episodes = episodes, intervals = intervals,
       event_epochs = make_empty_event_epoch_df(), warnings = warnings)
}


value_or <- function(value, fallback) {
  if (is.null(value)) return(fallback)
  if (length(value) == 1 && is.na(value)) return(fallback)
  value
}



# -----------------------------------------------------------------------------
# V13.5.0 validation and benchmark suite
# -----------------------------------------------------------------------------
# These functions are intentionally independent of the Shiny rendering layer.
# They operate directly on simulate_spike_train_core(config, seed) and its
# spike / interval / episode truth tables, so they can be reused in unit tests,
# method papers, and automated CI smoke tests.

clone_config <- function(config) {
  unserialize(serialize(config, NULL))
}

safe_num <- function(x, fallback = NA_real_) {
  x <- suppressWarnings(as.numeric(x))[1]
  if (is.finite(x)) x else fallback
}

validation_runtime_config <- function(config) {
  # Interactive validation should report infeasible settings promptly rather than
  # spending a long time in retry-heavy samplers. Normal generation is unaffected.
  cfg <- clone_config(config)
  if (is.null(cfg$tonic_sampler_max_attempts)) cfg$tonic_sampler_max_attempts <- 120L
  if (is.null(cfg$run_sampler_max_attempts)) cfg$run_sampler_max_attempts <- 20L
  if (is.null(cfg$residual_life_latency_max_attempts)) cfg$residual_life_latency_max_attempts <- 80L
  if (is.null(cfg$max_auto_runs)) cfg$max_auto_runs <- 500L
  cfg
}

validation_error_table <- function(block, err) {
  data.frame(
    Validation_Block = block,
    Status = "error",
    Error = conditionMessage(err),
    stringsAsFactors = FALSE
  )
}

run_validation_block <- function(block, expr) {
  tryCatch(expr, error = function(err) validation_error_table(block, err))
}

pattern_abbrev <- function(pattern) {
  switch(pattern,
         "Burst" = "b",
         "Pause" = "p",
         "Tonic" = "t",
         "high_frequency_tonic" = "hft",
         "high_frequency_spiking" = "hfs",
         "Noisy" = "n",
         "b")
}

validation_sequence_for_label <- function(label, n_intervals = 20L) {
  n_intervals <- max(1L, as.integer(n_intervals))
  if (identical(label, "Pause")) {
    return(paste0("p", n_intervals))
  }
  paste0(pattern_abbrev(label), n_intervals + 1L)
}

validation_nominal_interval_mean <- function(config, pattern) {
  pat_cfg <- config$patterns[[pattern]]
  if (is.null(pat_cfg)) return(NA_real_)
  mean_val <- distribution_nominal_mean(pat_cfg$dist_type, pat_cfg$params)
  segments <- effective_pattern_segments_from_config(config, pattern, NA_real_, NA_character_)
  if (nrow(segments) == 0) return(mean_val)
  if (!is.finite(mean_val)) mean_val <- mean(c(min(segments$Start), max(segments$End)))
  min(max(mean_val, min(segments$Start)), max(segments$End))
}

validation_total_time_for_run <- function(config, label, n_intervals = 20L, multiplier = 2.5) {
  mean_isi <- validation_nominal_interval_mean(config, label)
  if (!is.finite(mean_isi) || mean_isi <= 0) {
    rng <- effective_interval_range_from_config(config, label)
    mean_isi <- if (length(rng) == 2 && all(is.finite(rng))) mean(rng) else 0.5
  }
  max(5, safe_num(config$total_time, 25), multiplier * mean_isi * (n_intervals + 4))
}

set_config_manual_sequence <- function(config, sequence_text) {
  cfg <- clone_config(config)
  parsed <- parse_pattern_sequence_strict(sequence_text)
  if (!is.null(parsed$error)) stop(parsed$error, call. = FALSE)
  cfg$pattern_sequence <- parsed$tokens
  cfg
}

set_config_temporal <- function(config, pattern, rho = 0, trend = 0) {
  cfg <- clone_config(config)
  if (!is.null(cfg$patterns[[pattern]])) {
    cfg$patterns[[pattern]]$temporal_dependence <- list(rho = rho, trend = trend)
  }
  cfg
}

validation_simulate_safe <- function(config, seed) {
  cfg <- validation_runtime_config(config)
  tryCatch(
    simulate_spike_train_core(cfg, seed = seed),
    error = function(err) list(spikes = make_empty_spike_df(), intervals = make_empty_interval_df(), episodes = make_empty_episode_df(), warnings = conditionMessage(err), error = conditionMessage(err))
  )
}

collect_label_intervals <- function(config, label, seeds = 1:10, n_intervals = 20L,
                                    force_true_pause_isis = TRUE) {
  seq_txt <- validation_sequence_for_label(label, n_intervals)
  cfg <- set_config_manual_sequence(config, seq_txt)
  cfg$total_time <- validation_total_time_for_run(cfg, label, n_intervals)
  if (isTRUE(force_true_pause_isis)) cfg$leading_silence_initial_pause <- FALSE
  rows <- list()
  errors <- character(0)
  idx <- 0L
  for (seed in seeds) {
    sim <- validation_simulate_safe(cfg, seed)
    if (!is.null(sim$error)) errors <- c(errors, paste0("seed ", seed, ": ", sim$error))
    if (!is.null(sim$intervals) && nrow(sim$intervals) > 0) {
      ii <- sim$intervals[sim$intervals$ISI_Label == label & is.finite(sim$intervals$ISI_s) & sim$intervals$ISI_s > 0, , drop = FALSE]
      if (nrow(ii) > 0) {
        ii$Validation_Seed <- seed
        idx <- idx + 1L
        rows[[idx]] <- ii
      }
    }
  }
  out <- if (length(rows) > 0) do.call(rbind, rows) else make_empty_interval_df()
  attr(out, "errors") <- unique(errors)
  out
}

truncated_cdf_function <- function(config, pattern) {
  pat_cfg <- config$patterns[[pattern]]
  if (is.null(pat_cfg)) return(NULL)
  segments <- effective_pattern_segments_from_config(config, pattern, NA_real_, NA_character_)
  if (nrow(segments) == 0) return(NULL)
  masses <- vapply(seq_len(nrow(segments)), function(i) {
    dist_cdf_value(pat_cfg$dist_type, pat_cfg$params, segments$End[i]) -
      dist_cdf_value(pat_cfg$dist_type, pat_cfg$params, segments$Start[i])
  }, numeric(1))
  total_mass <- sum(masses[is.finite(masses)])
  if (!is.finite(total_mass) || total_mass <= 0) return(NULL)
  function(x) {
    x <- as.numeric(x)
    out <- rep(0, length(x))
    for (j in seq_along(x)) {
      xx <- x[j]
      if (!is.finite(xx)) { out[j] <- NA_real_; next }
      acc <- 0
      for (i in seq_len(nrow(segments))) {
        if (xx <= segments$Start[i]) next
        upper <- min(xx, segments$End[i])
        if (upper > segments$Start[i]) {
          acc <- acc + dist_cdf_value(pat_cfg$dist_type, pat_cfg$params, upper) -
            dist_cdf_value(pat_cfg$dist_type, pat_cfg$params, segments$Start[i])
        }
      }
      out[j] <- min(max(acc / total_mass, 0), 1)
    }
    out
  }
}

degenerate_distribution_point <- function(dist_type, params) {
  if (identical(dist_type, "Normal") && !is.null(params$sd) && isTRUE(params$sd == 0)) return(as.numeric(params$mean)[1])
  if (identical(dist_type, "Lognormal") && !is.null(params$sdlog) && isTRUE(params$sdlog == 0)) return(exp(as.numeric(params$meanlog)[1]))
  NA_real_
}

static_distribution_applicable <- function(config, pattern) {
  dep <- coerce_temporal_dependence(config$patterns[[pattern]]$temporal_dependence)
  if (abs(dep$rho) > 1e-12 || abs(dep$trend) > 1e-12) return(FALSE)
  if (identical(pattern, "Tonic")) return(FALSE)
  if (identical(pattern, "Noisy") && isTRUE(config$avoid_noisy_burst_runs)) return(FALSE)
  TRUE
}

safe_ks_uniform <- function(u) {
  u <- u[is.finite(u) & u >= 0 & u <= 1]
  if (length(u) < 5) return(c(D = NA_real_, p = NA_real_))
  out <- tryCatch(stats::ks.test(u, "punif"), warning = function(w) suppressWarnings(stats::ks.test(u, "punif")), error = function(e) NULL)
  if (is.null(out)) return(c(D = NA_real_, p = NA_real_))
  c(D = unname(out$statistic), p = out$p.value)
}

safe_ks_two_sample <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 5 || length(y) < 5) return(c(D = NA_real_, p = NA_real_))
  out <- tryCatch(stats::ks.test(x, y), warning = function(w) suppressWarnings(stats::ks.test(x, y)), error = function(e) NULL)
  if (is.null(out)) return(c(D = NA_real_, p = NA_real_))
  c(D = unname(out$statistic), p = out$p.value)
}

run_distribution_validation <- function(config, seeds = 1:10, n_intervals = 20L, reference_offset = 10000L) {
  rows <- list()
  idx <- 0L
  for (label in SPIKE_PATTERN_LEVELS) {
    samples_df <- collect_label_intervals(config, label, seeds = seeds, n_intervals = n_intervals)
    ref_df <- collect_label_intervals(config, label, seeds = seeds + reference_offset, n_intervals = n_intervals)
    x <- samples_df$ISI_s
    y <- ref_df$ISI_s
    pat_cfg <- config$patterns[[label]]
    degenerate_point <- degenerate_distribution_point(pat_cfg$dist_type, pat_cfg$params)
    is_degenerate <- is.finite(degenerate_point)
    static_ok <- static_distribution_applicable(config, label) && !is_degenerate
    cdf_fun <- if (isTRUE(static_ok)) truncated_cdf_function(config, label) else NULL
    ks_static <- c(D = NA_real_, p = NA_real_)
    if (!is.null(cdf_fun) && length(x) > 0) {
      ks_static <- safe_ks_uniform(cdf_fun(x))
    }
    ks_eff <- if (is_degenerate) c(D = NA_real_, p = NA_real_) else safe_ks_two_sample(x, y)
    deg_max_err <- if (is_degenerate && length(x) > 0) max(abs(x - degenerate_point), na.rm = TRUE) else NA_real_
    status <- if (length(x) < max(5L, length(seeds))) {
      "low_sample_or_failed"
    } else if (is_degenerate && is.finite(deg_max_err) && deg_max_err > 1e-9) {
      "degenerate_point_mismatch"
    } else {
      "ok"
    }
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      Label = label,
      Distribution = pat_cfg$dist_type,
      N = length(x),
      Reference_N = length(y),
      Mean_ISI = if (length(x) > 0) mean(x, na.rm = TRUE) else NA_real_,
      Median_ISI = if (length(x) > 0) stats::median(x, na.rm = TRUE) else NA_real_,
      Q05 = if (length(x) > 0) as.numeric(stats::quantile(x, 0.05, na.rm = TRUE)) else NA_real_,
      Q95 = if (length(x) > 0) as.numeric(stats::quantile(x, 0.95, na.rm = TRUE)) else NA_real_,
      Is_Degenerate_Distribution = is_degenerate,
      Degenerate_Point_s = degenerate_point,
      Degenerate_MaxAbs_Error_s = deg_max_err,
      Analytic_Static_Applicable = isTRUE(static_ok),
      Static_PIT_KS_D = ks_static["D"],
      Static_PIT_KS_p = ks_static["p"],
      Effective_TwoSample_KS_D = ks_eff["D"],
      Effective_TwoSample_KS_p = ks_eff["p"],
      Status = status,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

check_no_three_burst_like_noisy <- function(intervals, config) {
  if (is.null(intervals) || nrow(intervals) < 3) return(TRUE)
  brng <- effective_interval_range_from_config(config, "Burst")
  if (length(brng) != 2 || any(!is.finite(brng))) return(TRUE)
  for (x in interval_sequence_groups(intervals)) {
    flags <- x$ISI_Label == "Noisy" & is.finite(x$ISI_s) & x$ISI_s >= brng[1] & x$ISI_s <= brng[2]
    rr <- rle(flags)
    if (any(rr$values & rr$lengths >= 3)) return(FALSE)
  }
  TRUE
}

check_noisy_clean_label_constraints <- function(intervals, config) {
  if (is.null(intervals) || nrow(intervals) == 0) return(TRUE)
  spec <- noisy_specificity_from_config(config)
  groups <- interval_sequence_groups(intervals)
  x <- do.call(rbind, groups)
  noisy <- x[x$ISI_Label == "Noisy" & is.finite(x$ISI_s), , drop = FALSE]
  if (nrow(noisy) == 0) return(TRUE)

  # Noisy must stay inside the benchmark Noisy envelope: from the absolute
  # refractory period up to the Tonic upper scale, with a safety margin below
  # the Pause lower bound. This prevents long, slow, Tonic/Pause-looking Noisy
  # runs such as the previous hard-preset failure case.
  envelope <- noisy_physiological_envelope(config)
  if (length(envelope) != 2 || any(!is.finite(envelope)) || envelope[2] < envelope[1]) return(FALSE)
  if (any(noisy$ISI_s < envelope[1] - 1e-12 | noisy$ISI_s > envelope[2] + 1e-12, na.rm = TRUE)) return(FALSE)

  # Noisy must never be Pause-like, because Pause is already a single-ISI class.
  zones_all <- vapply(noisy$ISI_s, noisy_mode_zone, character(1), config = config, guard_s = 0)
  if (any(zones_all == "Pause", na.rm = TRUE)) return(FALSE)

  # Two consecutive Noisy ISIs must not occupy the same Burst-like or Tonic-like
  # zone. A singleton Noisy ISI may be Burst-like/Tonic-like if it is isolated.
  if (count_global_noisy_same_zone_pair_violations(x, config, guard_s = 0) > 0L) return(FALSE)
  noisy_groups <- split(noisy, interaction(noisy$Train, noisy$Episode, drop = TRUE))
  for (g in noisy_groups) {
    vals <- as.numeric(g$ISI_s)
    vals <- vals[is.finite(vals) & vals > 0]
    if (length(vals) == 0) return(FALSE)
    if (noisy_same_zone_pair_violation(vals, config, guard_s = 0)) return(FALSE)
    if (length(vals) >= spec$toniclike_min_isi_count) {
      m <- isi_regularity_metrics(vals)
      cv_ok <- is.finite(m$cv) && m$cv >= spec$min_run_cv
      cv2_ok <- is.finite(m$cv2) && m$cv2 >= spec$min_run_cv2
      # A long Noisy run must be visibly irregular. Requiring both CV and CV2
      # prevents a slow, nearly periodic Noisy train from masquerading as Tonic.
      if (!isTRUE(cv_ok && cv2_ok)) return(FALSE)
    }
  }

  # Cross-label adjacency: a mode-like Noisy ISI must not touch any true
  # Burst/Tonic/HF interval. Even if the zones differ, this prevents a singleton
  # Noisy event from being visually absorbed into a patterned neighborhood.
  mode_labels <- c("Burst", "Tonic", HF_PATTERN_LABELS)
  for (x_train in groups) {
    if (nrow(x_train) < 2) next
    for (i in seq_len(nrow(x_train) - 1L)) {
      a <- x_train[i, , drop = FALSE]
      b <- x_train[i + 1L, , drop = FALSE]
      if (identical(a$ISI_Label, "Noisy") && b$ISI_Label %in% mode_labels) {
        zone <- noisy_mode_zone(a$ISI_s, config, guard_s = 0)
        if (!is.na(zone) && zone %in% mode_labels) return(FALSE)
      }
      if (identical(b$ISI_Label, "Noisy") && a$ISI_Label %in% mode_labels) {
        zone <- noisy_mode_zone(b$ISI_s, config, guard_s = 0)
        if (!is.na(zone) && zone %in% mode_labels) return(FALSE)
      }
    }
  }
  TRUE
}

check_noisy_mm_adjacency <- function(intervals, config) {
  if (is.null(intervals) || nrow(intervals) < 2) return(TRUE)
  spec <- noisy_specificity_from_config(config)
  for (x in interval_sequence_groups(intervals)) {
    if (nrow(x) < 2) next
    for (i in seq_len(nrow(x) - 1L)) {
      one_is_noisy <- xor(x$ISI_Label[i] == "Noisy", x$ISI_Label[i + 1L] == "Noisy")
      if (!one_is_noisy) next
      other_label <- if (x$ISI_Label[i] == "Noisy") x$ISI_Label[i + 1L] else x$ISI_Label[i]
      if (!other_label %in% SPIKE_PATTERN_LEVELS) next
      a <- as.numeric(x$ISI_s[i]); b <- as.numeric(x$ISI_s[i + 1L])
      if (!is.finite(a) || !is.finite(b) || min(a, b) <= 0) return(FALSE)
      if (max(a, b) / min(a, b) + 1e-12 < spec$mm_ratio) return(FALSE)
    }
  }
  TRUE
}

check_interval_spike_consistency <- function(spikes, intervals) {
  if (is.null(spikes) || nrow(spikes) == 0) return(nrow(intervals) == 0)
  times <- sort(as.numeric(spikes$Time))
  if (any(!is.finite(times)) || any(times <= 0) || any(diff(times) <= 0)) return(FALSE)
  expected_n <- max(0L, length(times) - 1L)
  if (is.null(intervals) || nrow(intervals) != expected_n) return(FALSE)
  if (expected_n == 0L) return(TRUE)
  x <- intervals[order(intervals$Interval_ID), , drop = FALSE]
  starts_ok <- max(abs(as.numeric(x$Start_Time_s) - times[-length(times)]), na.rm = TRUE) <= 1e-8
  ends_ok <- max(abs(as.numeric(x$End_Time_s) - times[-1]), na.rm = TRUE) <= 1e-8
  isi_ok <- max(abs(as.numeric(x$ISI_s) - (as.numeric(x$End_Time_s) - as.numeric(x$Start_Time_s))), na.rm = TRUE) <= 1e-8
  all(starts_ok, ends_ok, isi_ok, !is.na(x$Episode))
}

check_episode_time_contiguity <- function(intervals) {
  if (is.null(intervals) || nrow(intervals) == 0) return(TRUE)
  groups <- split(intervals[order(intervals$Interval_ID), , drop = FALSE], intervals$Episode)
  for (g in groups) {
    if (nrow(g) <= 1) next
    if (any(abs(g$Start_Time_s[-1] - g$End_Time_s[-nrow(g)]) > 1e-8)) return(FALSE)
  }
  TRUE
}

run_simulator_invariant_suite <- function(config, seed = 1L) {
  cases <- list(
    list(Name = "p1s_default_leading_latency", Sequence = "p1s", Leading = TRUE, Expected_Intervals_Min = 0L),
    list(Name = "p1sb4_default_shared_after_latency", Sequence = "p1sb4", Leading = TRUE, Expected_Intervals_Min = 3L),
    list(Name = "b4_positive_initial_latency", Sequence = "b4", Leading = TRUE, Expected_Intervals_Min = 3L),
    list(Name = "t5_positive_initial_latency", Sequence = "t5", Leading = TRUE, Expected_Intervals_Min = 4L),
    list(Name = "n2_positive_initial_latency_allowed_single_noisy_isi", Sequence = "n2", Leading = TRUE, Expected_Intervals_Min = 1L),
    list(Name = "p1s_true_pause_after_positive_latency", Sequence = "p1s", Leading = FALSE, Expected_Intervals_Min = 1L)
  )
  rows <- list()
  for (i in seq_along(cases)) {
    cc <- cases[[i]]
    cfg <- set_config_manual_sequence(config, cc$Sequence)
    cfg$total_time <- max(safe_num(config$total_time, 25), 8)
    cfg$leading_silence_initial_pause <- isTRUE(cc$Leading)
    sim <- validation_simulate_safe(cfg, seed + i - 1L)
    spike_times <- if (!is.null(sim$spikes) && nrow(sim$spikes) > 0) sim$spikes$Time else numeric(0)
    intervals <- if (!is.null(sim$intervals)) sim$intervals else make_empty_interval_df()
    episodes <- if (!is.null(sim$episodes)) sim$episodes else make_empty_episode_df()
    no_zero_spike <- length(spike_times) == 0 || all(is.finite(spike_times) & spike_times > 0)
    no_boundary_interval <- nrow(intervals) == 0 || all(is.finite(intervals$Start_Time_s) & intervals$Start_Time_s > 0)
    first_spike_positive <- length(spike_times) > 0 && min(spike_times) > 0
    strictly_increasing <- length(spike_times) <= 1 || all(diff(sort(spike_times)) > 0)
    spikes_within_window <- length(spike_times) == 0 || all(is.finite(spike_times) & spike_times <= cfg$total_time + 1e-8)
    no_latency_in_interval_table <- nrow(intervals) == 0 || !any(intervals$ISI_Scope %in% c("leading_latency", "initial_latency"), na.rm = TRUE)
    interval_min_ok <- nrow(intervals) >= cc$Expected_Intervals_Min
    expected_interval_count <- max(0L, length(spike_times) - 1L)
    interval_count_exact <- nrow(intervals) == expected_interval_count
    interval_spike_consistency <- check_interval_spike_consistency(sim$spikes, intervals)
    episode_contiguity_ok <- check_episode_time_contiguity(intervals)
    no_noisy_burst_triplets <- check_no_three_burst_like_noisy(intervals, cfg)
    noisy_mm_ok <- check_noisy_mm_adjacency(intervals, cfg)
    noisy_clean_label_ok <- check_noisy_clean_label_constraints(intervals, cfg)
    leading_scope_ok <- if (isTRUE(cc$Leading) && grepl("^p", cc$Sequence)) any(episodes$Episode_Scope == "leading_latency") else TRUE
    latency_pattern_ok <- !any(episodes$Episode_Scope %in% c("leading_latency", "initial_latency") & episodes$Pattern %in% SPIKE_PATTERN_LEVELS)
    rows[[i]] <- data.frame(
      Test = cc$Name,
      Sequence = cc$Sequence,
      Leading_Silence = isTRUE(cc$Leading),
      First_Spike_s = if (length(spike_times) > 0) min(spike_times) else NA_real_,
      N_Spikes = length(spike_times),
      N_Intervals = nrow(intervals),
      Expected_Intervals = expected_interval_count,
      No_Zero_Spike = no_zero_spike,
      No_Recording_Boundary_Interval = no_boundary_interval,
      First_Spike_Positive = first_spike_positive,
      Interval_Count_OK = interval_min_ok,
      Interval_Count_Exact_OK = interval_count_exact,
      Strictly_Increasing_Spikes = strictly_increasing,
      Spikes_Within_Window = spikes_within_window,
      No_Latency_In_Interval_Table = no_latency_in_interval_table,
      Interval_Spike_Consistency_OK = interval_spike_consistency,
      Episode_Contiguity_OK = episode_contiguity_ok,
      Noisy_No_Three_BurstLike_OK = no_noisy_burst_triplets,
      Noisy_MM_Adjacency_OK = noisy_mm_ok,
      Noisy_Clean_Label_OK = noisy_clean_label_ok,
      Leading_Scope_OK = leading_scope_ok,
      Latency_Pattern_OK = latency_pattern_ok,
      Pass = no_zero_spike && no_boundary_interval && first_spike_positive && interval_min_ok && interval_count_exact && strictly_increasing && spikes_within_window && no_latency_in_interval_table && interval_spike_consistency && episode_contiguity_ok && no_noisy_burst_triplets && noisy_mm_ok && noisy_clean_label_ok && leading_scope_ok && latency_pattern_ok,
      Diagnostics = paste(unique(sim$warnings), collapse = " | "),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

estimate_run_temporal_stats <- function(intervals, label = NULL) {
  if (is.null(intervals) || nrow(intervals) == 0) {
    return(data.frame(Episode = integer(0), Label = character(0), N = integer(0), Lag1_Correlation = numeric(0), LogISI_Slope = numeric(0), stringsAsFactors = FALSE))
  }
  x <- intervals
  if (!is.null(label)) x <- x[x$ISI_Label == label, , drop = FALSE]
  if (nrow(x) == 0) return(data.frame(Episode = integer(0), Label = character(0), N = integer(0), Lag1_Correlation = numeric(0), LogISI_Slope = numeric(0), stringsAsFactors = FALSE))
  groups <- split(x, paste(x$Episode, x$ISI_Label, sep = "|"))
  rows <- list()
  idx <- 0L
  for (g in groups) {
    g <- g[order(g$Interval_ID), , drop = FALSE]
    isi <- as.numeric(g$ISI_s)
    isi <- isi[is.finite(isi) & isi > 0]
    if (length(isi) < 4) next
    lag1 <- suppressWarnings(stats::cor(isi[-length(isi)], isi[-1]))
    pos <- seq_along(isi)
    pos <- if (length(isi) > 1) (pos - 1) / (length(isi) - 1) - 0.5 else rep(0, length(isi))
    slope <- tryCatch(as.numeric(stats::coef(stats::lm(log(isi) ~ pos))[2]), error = function(e) NA_real_)
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      Episode = g$Episode[1],
      Label = g$ISI_Label[1],
      N = length(isi),
      Lag1_Correlation = lag1,
      LogISI_Slope = slope,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(data.frame(Episode = integer(0), Label = character(0), N = integer(0), Lag1_Correlation = numeric(0), LogISI_Slope = numeric(0), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

run_temporal_dependence_validation <- function(config, seeds = 1:8, n_intervals = 20L,
                                               patterns = c("Burst", "Tonic", "Noisy"),
                                               rho_values = c(-0.6, 0, 0.6),
                                               trend_values = c(-1, 0, 1)) {
  rows <- list()
  idx <- 0L
  temporal_direction_summary <- function(tab, target, metric_col) {
    n_runs <- if (!is.null(tab)) nrow(tab) else 0L
    if (n_runs <= 0L || is.null(tab[[metric_col]])) {
      return(list(direction_ok = NA, status = "no_valid_runs"))
    }
    metric_mean <- mean(tab[[metric_col]], na.rm = TRUE)
    if (!is.finite(metric_mean)) {
      return(list(direction_ok = NA, status = "insufficient_finite_stats"))
    }
    direction_ok <- if (target == 0) TRUE else sign(metric_mean) == sign(target)
    list(
      direction_ok = direction_ok,
      status = if (isTRUE(direction_ok)) "ok" else "direction_mismatch"
    )
  }
  cap_positive_int <- function(value, cap) {
    value <- suppressWarnings(as.integer(value))[1]
    if (!is.finite(value)) value <- cap
    max(1L, min(value, as.integer(cap)))
  }
  temporal_runtime_config <- function(cfg, pattern) {
    cfg <- validation_runtime_config(cfg)
    cfg$run_sampler_max_attempts <- cap_positive_int(cfg$run_sampler_max_attempts, 5L)
    cfg$residual_life_latency_max_attempts <- cap_positive_int(cfg$residual_life_latency_max_attempts, 20L)
    if (identical(pattern, "Tonic")) {
      cfg$tonic_sampler_max_attempts <- cap_positive_int(cfg$tonic_sampler_max_attempts, 4L)
    }
    cfg
  }
  for (pattern in patterns) {
    if (!pattern %in% SPIKE_PATTERN_LEVELS || identical(pattern, "Pause")) next
    seq_txt <- validation_sequence_for_label(pattern, n_intervals)
    for (rho in rho_values) {
      cfg <- temporal_runtime_config(set_config_manual_sequence(set_config_temporal(config, pattern, rho = rho, trend = 0), seq_txt), pattern)
      cfg$total_time <- validation_total_time_for_run(cfg, pattern, n_intervals, multiplier = 4)
      vals <- list()
      for (seed in seeds) {
        sim <- validation_simulate_safe(cfg, seed)
        vals[[length(vals) + 1L]] <- estimate_run_temporal_stats(sim$intervals, pattern)
      }
      tab <- do.call(rbind, vals)
      n_runs <- if (!is.null(tab)) nrow(tab) else 0L
      lag1_mean <- if (n_runs > 0L) mean(tab$Lag1_Correlation, na.rm = TRUE) else NA_real_
      slope_mean <- if (n_runs > 0L) mean(tab$LogISI_Slope, na.rm = TRUE) else NA_real_
      if (!is.finite(lag1_mean)) lag1_mean <- NA_real_
      if (!is.finite(slope_mean)) slope_mean <- NA_real_
      direction <- temporal_direction_summary(tab, rho, "Lag1_Correlation")
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        Validation = "rho_sweep",
        Label = pattern,
        Target_Rho = rho,
        Target_Trend = 0,
        N_Runs = n_runs,
        Has_Valid_Runs = n_runs > 0L,
        Empirical_Lag1_Mean = lag1_mean,
        Empirical_LogSlope_Mean = slope_mean,
        Direction_OK = direction$direction_ok,
        Status = direction$status,
        stringsAsFactors = FALSE
      )
    }
    for (trend in trend_values) {
      cfg <- temporal_runtime_config(set_config_manual_sequence(set_config_temporal(config, pattern, rho = 0, trend = trend), seq_txt), pattern)
      cfg$total_time <- validation_total_time_for_run(cfg, pattern, n_intervals, multiplier = 5)
      vals <- list()
      for (seed in seeds) {
        sim <- validation_simulate_safe(cfg, seed + 5000L)
        vals[[length(vals) + 1L]] <- estimate_run_temporal_stats(sim$intervals, pattern)
      }
      tab <- do.call(rbind, vals)
      n_runs <- if (!is.null(tab)) nrow(tab) else 0L
      lag1_mean <- if (n_runs > 0L) mean(tab$Lag1_Correlation, na.rm = TRUE) else NA_real_
      slope_mean <- if (n_runs > 0L) mean(tab$LogISI_Slope, na.rm = TRUE) else NA_real_
      if (!is.finite(lag1_mean)) lag1_mean <- NA_real_
      if (!is.finite(slope_mean)) slope_mean <- NA_real_
      direction <- temporal_direction_summary(tab, trend, "LogISI_Slope")
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        Validation = "trend_sweep",
        Label = pattern,
        Target_Rho = 0,
        Target_Trend = trend,
        N_Runs = n_runs,
        Has_Valid_Runs = n_runs > 0L,
        Empirical_Lag1_Mean = lag1_mean,
        Empirical_LogSlope_Mean = slope_mean,
        Direction_OK = direction$direction_ok,
        Status = direction$status,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

interval_cv2_values <- function(isi) {
  isi <- as.numeric(isi)
  if (length(isi) < 2) return(rep(NA_real_, length(isi)))
  out <- rep(NA_real_, length(isi))
  for (i in seq_along(isi)) {
    lo <- max(1, i - 2)
    hi <- min(length(isi), i + 2)
    win <- isi[lo:hi]
    if (length(win) >= 2) {
      prev <- win[-length(win)]
      nxt <- win[-1]
      denom <- prev + nxt
      valid <- denom > 0 & is.finite(denom)
      if (any(valid)) out[i] <- mean(2 * abs(nxt[valid] - prev[valid]) / denom[valid])
    }
  }
  out
}

simple_threshold_interval_detector <- function(spike_times, config) {
  spike_times <- sort(as.numeric(spike_times[is.finite(spike_times)]))
  if (length(spike_times) < 2) return(character(0))
  isi <- diff(spike_times)
  pred <- rep("Noisy", length(isi))
  available <- rep(TRUE, length(isi))

  valid_range <- function(rng) {
    length(rng) == 2 && all(is.finite(rng)) && rng[2] >= rng[1]
  }
  in_range <- function(x, rng) {
    valid_range(rng) & is.finite(x) & x >= rng[1] & x <= rng[2]
  }
  bool_runs <- function(flag) {
    flag <- as.logical(flag)
    flag[is.na(flag)] <- FALSE
    if (length(flag) == 0 || !any(flag)) {
      return(data.frame(start = integer(0), end = integer(0)))
    }
    d <- diff(c(FALSE, flag, FALSE))
    data.frame(start = which(d == 1), end = which(d == -1) - 1L)
  }
  max_consecutive_true <- function(flag) {
    rr <- rle(as.logical(flag))
    if (!any(rr$values)) return(0L)
    as.integer(max(rr$lengths[rr$values]))
  }
  metric_in_range <- function(value, rng) {
    if (is.null(rng)) return(TRUE)
    valid_range(as.numeric(rng)) && is.finite(value) && value >= rng[1] && value <= rng[2]
  }
  regularity_pass <- function(vals, ranges, include_mm = FALSE) {
    if (is.null(ranges)) return(TRUE)
    metrics <- isi_regularity_metrics(vals)
    checks <- c(
      metric_in_range(metrics$cv, ranges$cv),
      metric_in_range(metrics$cv2, ranges$cv2),
      metric_in_range(metrics$lv, ranges$lv)
    )
    if (isTRUE(include_mm)) checks <- c(checks, metric_in_range(metrics$mm, ranges$mm))
    all(checks)
  }
  assign_valid_runs <- function(flag, label, min_n_isi = 1L, validator = NULL) {
    runs <- bool_runs(flag & available)
    if (nrow(runs) == 0) return(invisible(NULL))
    for (j in seq_len(nrow(runs))) {
      idx <- runs$start[j]:runs$end[j]
      if (length(idx) < max(1L, as.integer(min_n_isi))) next
      vals <- isi[idx]
      if (!is.null(validator) && !isTRUE(validator(vals))) next
      pred[idx] <<- label
      available[idx] <<- FALSE
    }
    invisible(NULL)
  }
  min_isi_count <- function(pattern, fallback_spikes) {
    rng <- config$patterns[[pattern]]$spike_count_range
    n_spikes <- if (length(rng) == 2 && all(is.finite(rng))) min(as.integer(round(rng))) else fallback_spikes
    max(1L, n_spikes - 1L)
  }

  brng <- effective_interval_range_from_config(config, "Burst")
  prng <- effective_interval_range_from_config(config, "Pause")
  trng <- effective_interval_range_from_config(config, "Tonic")
  hftrng <- effective_interval_range_from_config(config, "high_frequency_tonic")
  hfsrng <- effective_interval_range_from_config(config, "high_frequency_spiking")

  # Pause is a single-long-ISI class and therefore has priority over state runs.
  if (valid_range(prng)) {
    pause_idx <- in_range(isi, prng)
    pred[pause_idx] <- "Pause"
    available[pause_idx] <- FALSE
  }

  # High-frequency spiking is evaluated as a sustained epoch, not ISI by ISI.
  if (valid_range(hfsrng) && !is.null(config$patterns$high_frequency_spiking)) {
    rules <- config$patterns$high_frequency_spiking$state_rules
    short_rng <- as.numeric(rules$short_isi_range)
    bridge_rng <- as.numeric(rules$bridge_isi_range)
    if (valid_range(short_rng) && valid_range(bridge_rng)) {
      hfs_flag <- available & in_range(isi, hfsrng) & isi <= bridge_rng[2]
      hfs_validator <- function(vals) {
        short <- in_range(vals, short_rng)
        bridge <- vals > short_rng[2] & vals <= bridge_rng[2]
        all(short | bridge) &&
          mean(short) >= safe_num(rules$short_fraction_min, 0.70) &&
          mean(bridge) <= safe_num(rules$bridge_fraction_max, 0.20) &&
          max_consecutive_true(bridge) <= max(0L, as.integer(rules$max_consecutive_bridge)) &&
          sum(vals) >= max(0, safe_num(rules$min_duration_s, 0))
      }
      assign_valid_runs(
        hfs_flag,
        "high_frequency_spiking",
        min_n_isi = max(HF_SPIKING_MIN_BOUNDARY_SPIKES - 1L, min_isi_count("high_frequency_spiking", HF_SPIKING_MIN_BOUNDARY_SPIKES)),
        validator = hfs_validator
      )
    }
  }

  # High-frequency tonic is a regular high-rate state. It is evaluated before
  # burst packets so that a long regular run is not fragmented into burst labels.
  if (valid_range(hftrng) && !is.null(config$patterns$high_frequency_tonic)) {
    hft_ranges <- config$patterns$high_frequency_tonic$regularity_ranges
    assign_valid_runs(
      available & in_range(isi, hftrng),
      "high_frequency_tonic",
      min_n_isi = max(HF_TONIC_MIN_BOUNDARY_SPIKES - 1L, min_isi_count("high_frequency_tonic", HF_TONIC_MIN_BOUNDARY_SPIKES)),
      validator = function(vals) regularity_pass(vals, hft_ranges, include_mm = TRUE)
    )
  }

  if (valid_range(brng)) {
    assign_valid_runs(
      available & in_range(isi, brng),
      "Burst",
      min_n_isi = min_isi_count("Burst", BURST_MIN_BOUNDARY_SPIKES)
    )
  }

  if (valid_range(trng) && !is.null(config$patterns$Tonic)) {
    tonic_ranges <- config$patterns$Tonic$regularity_ranges
    assign_valid_runs(
      available & in_range(isi, trng),
      "Tonic",
      min_n_isi = min_isi_count("Tonic", TONIC_MIN_BOUNDARY_SPIKES),
      validator = function(vals) regularity_pass(vals, tonic_ranges, include_mm = FALSE)
    )
  }

  pred
}


score_interval_labels <- function(true_labels, pred_labels) {
  true_labels <- as.character(true_labels)
  pred_labels <- as.character(pred_labels)
  original_pred_length <- length(pred_labels)
  n_true <- length(true_labels)
  if (n_true == 0) {
    labels <- SCORABLE_PATTERN_LABELS
    per_class <- do.call(rbind, lapply(labels, function(label) {
      data.frame(Label = label, TP = 0L, FP = 0L, FN = 0L, Precision = NA_real_, Recall = NA_real_, F1 = NA_real_, stringsAsFactors = FALSE)
    }))
    return(list(per_class = per_class, accuracy = NA_real_, macro_f1 = NA_real_,
                n_true = 0L, n_pred_original = original_pred_length,
                n_scorable = 0L, n_ignored_nonpattern = 0L,
                n_unclassified = 0L, n_extra_predictions = max(0L, original_pred_length),
                length_mismatch = original_pred_length != 0L))
  }
  if (length(pred_labels) < n_true) {
    pred_labels <- c(pred_labels, rep("Unclassified", n_true - length(pred_labels)))
  } else if (length(pred_labels) > n_true) {
    pred_labels <- pred_labels[seq_len(n_true)]
  }
  scorable_mask <- true_labels %in% SCORABLE_PATTERN_LABELS
  ignored_nonpattern <- sum(!scorable_mask)
  true_scored <- true_labels[scorable_mask]
  pred_scored <- pred_labels[scorable_mask]
  labels <- SCORABLE_PATTERN_LABELS
  rows <- list()
  for (label in labels) {
    tp <- sum(true_scored == label & pred_scored == label)
    fp <- sum(true_scored != label & pred_scored == label)
    fn <- sum(true_scored == label & pred_scored != label)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
    rows[[label]] <- data.frame(Label = label, TP = tp, FP = fp, FN = fn, Precision = precision, Recall = recall, F1 = f1, stringsAsFactors = FALSE)
  }
  per_class <- do.call(rbind, rows)
  macro_mask <- (per_class$TP + per_class$FP + per_class$FN) > 0
  list(
    per_class = per_class,
    accuracy = if (length(true_scored) > 0) mean(true_scored == pred_scored) else NA_real_,
    macro_f1 = if (any(macro_mask)) mean(per_class$F1[macro_mask]) else NA_real_,
    macro_f1_labels = paste(per_class$Label[macro_mask], collapse = ";"),
    n_true = n_true,
    n_pred_original = original_pred_length,
    n_scorable = length(true_scored),
    n_ignored_nonpattern = ignored_nonpattern,
    n_unclassified = sum(pred_labels == "Unclassified"),
    n_extra_predictions = max(0L, original_pred_length - n_true),
    length_mismatch = original_pred_length != n_true
  )
}

prediction_episodes_from_labels <- function(intervals, pred_labels) {
  if (is.null(intervals) || nrow(intervals) == 0) {
    return(data.frame(Pred_Episode_ID = integer(0), Pred_Label = character(0), Pred_Start = numeric(0), Pred_End = numeric(0), stringsAsFactors = FALSE))
  }
  pred_labels <- as.character(pred_labels)
  if (length(pred_labels) < nrow(intervals)) {
    pred_labels <- c(pred_labels, rep("Unclassified", nrow(intervals) - length(pred_labels)))
  }
  if (length(pred_labels) > nrow(intervals)) {
    pred_labels <- pred_labels[seq_len(nrow(intervals))]
  }
  ord <- order(intervals$Interval_ID)
  intervals <- intervals[ord, , drop = FALSE]
  pred_labels <- pred_labels[ord]
  n <- length(pred_labels)
  scorable_true <- as.character(intervals$ISI_Label) %in% SCORABLE_PATTERN_LABELS
  scorable_pred <- pred_labels %in% SCORABLE_PATTERN_LABELS
  keep <- scorable_true & scorable_pred
  if (!any(keep)) {
    return(data.frame(Pred_Episode_ID = integer(0), Pred_Label = character(0), Pred_Start = numeric(0), Pred_End = numeric(0), stringsAsFactors = FALSE))
  }
  interval_id <- suppressWarnings(as.integer(intervals$Interval_ID))
  time_gap <- c(NA_real_, abs(intervals$Start_Time_s[-1] - intervals$End_Time_s[-n]))
  stim_id <- if ("Stimulus_ID" %in% names(intervals)) ifelse(is.na(intervals$Stimulus_ID), -1L, suppressWarnings(as.integer(intervals$Stimulus_ID))) else rep(-1L, n)
  epoch <- if ("Response_Epoch" %in% names(intervals)) as.character(intervals$Response_Epoch) else rep("", n)
  epoch[is.na(epoch)] <- ""
  breaks <- rep(FALSE, n)
  breaks[1] <- TRUE
  if (n > 1L) {
    for (i in 2:n) {
      breaks[i] <- !isTRUE(keep[i]) ||
        !isTRUE(keep[i - 1L]) ||
        pred_labels[i] != pred_labels[i - 1L] ||
        (!is.na(interval_id[i]) && !is.na(interval_id[i - 1L]) && interval_id[i] != interval_id[i - 1L] + 1L) ||
        (is.finite(time_gap[i]) && time_gap[i] > 1e-8) ||
        stim_id[i] != stim_id[i - 1L] ||
        epoch[i] != epoch[i - 1L]
    }
  }
  run_ids <- cumsum(breaks)
  rows <- list()
  for (id in unique(run_ids)) {
    idx <- which(run_ids == id)
    idx <- idx[keep[idx]]
    if (length(idx) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      Pred_Episode_ID = as.integer(length(rows) + 1L),
      Pred_Label = pred_labels[idx[1]],
      Pred_Start = intervals$Start_Time_s[idx[1]],
      Pred_End = intervals$End_Time_s[idx[length(idx)]],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(Pred_Episode_ID = integer(0), Pred_Label = character(0), Pred_Start = numeric(0), Pred_End = numeric(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

episode_iou_value <- function(a_start, a_end, b_start, b_end) {
  inter <- max(0, min(a_end, b_end) - max(a_start, b_start))
  union <- max(a_end, b_end) - min(a_start, b_start)
  if (union > 0) inter / union else 0
}

score_episode_predictions <- function(true_episodes, pred_episodes, iou_threshold = 0.5) {
  true_episodes <- true_episodes[true_episodes$Episode_Scope == "interval_run" & true_episodes$Pattern %in% SCORABLE_PATTERN_LABELS, , drop = FALSE]
  pred_episodes <- pred_episodes[pred_episodes$Pred_Label %in% SCORABLE_PATTERN_LABELS, , drop = FALSE]
  if (nrow(true_episodes) == 0 || nrow(pred_episodes) == 0) {
    tp <- 0L; fp <- nrow(pred_episodes); fn <- nrow(true_episodes)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    f1 <- if ((tp + fp + fn) == 0) NA_real_ else if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
    return(data.frame(TP = tp, FP = fp, FN = fn, Precision = precision, Recall = recall, F1 = f1, Mean_IoU = NA_real_, MedianAbs_Onset_Error_s = NA_real_, MedianAbs_Offset_Error_s = NA_real_, stringsAsFactors = FALSE))
  }
  pairs <- expand.grid(True_Row = seq_len(nrow(true_episodes)), Pred_Row = seq_len(nrow(pred_episodes)))
  pairs$Label_OK <- true_episodes$Pattern[pairs$True_Row] == pred_episodes$Pred_Label[pairs$Pred_Row]
  pairs$IoU <- mapply(function(i, j) episode_iou_value(true_episodes$Start[i], true_episodes$End[i], pred_episodes$Pred_Start[j], pred_episodes$Pred_End[j]), pairs$True_Row, pairs$Pred_Row)
  pairs <- pairs[pairs$Label_OK & pairs$IoU >= iou_threshold, , drop = FALSE]
  pairs <- pairs[order(-pairs$IoU), , drop = FALSE]
  used_t <- integer(0); used_p <- integer(0); keep <- logical(nrow(pairs))
  if (nrow(pairs) > 0) {
    for (k in seq_len(nrow(pairs))) {
      ti <- pairs$True_Row[k]; pj <- pairs$Pred_Row[k]
      if (!(ti %in% used_t) && !(pj %in% used_p)) {
        keep[k] <- TRUE; used_t <- c(used_t, ti); used_p <- c(used_p, pj)
      }
    }
  }
  matched <- pairs[keep, , drop = FALSE]
  tp <- nrow(matched); fp <- nrow(pred_episodes) - tp; fn <- nrow(true_episodes) - tp
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1 <- if ((tp + fp + fn) == 0) NA_real_ else if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  onset_err <- offset_err <- numeric(0)
  if (tp > 0) {
    onset_err <- pred_episodes$Pred_Start[matched$Pred_Row] - true_episodes$Start[matched$True_Row]
    offset_err <- pred_episodes$Pred_End[matched$Pred_Row] - true_episodes$End[matched$True_Row]
  }
  data.frame(
    TP = tp, FP = fp, FN = fn,
    Precision = precision, Recall = recall, F1 = f1,
    Mean_IoU = if (tp > 0) mean(matched$IoU) else NA_real_,
    MedianAbs_Onset_Error_s = if (length(onset_err) > 0) stats::median(abs(onset_err)) else NA_real_,
    MedianAbs_Offset_Error_s = if (length(offset_err) > 0) stats::median(abs(offset_err)) else NA_real_,
    stringsAsFactors = FALSE
  )
}


benchmark_quality_metrics <- function(intervals, episodes, config) {
  if (is.null(intervals)) intervals <- make_empty_interval_df()
  if (is.null(episodes)) episodes <- make_empty_episode_df()

  clean_label <- 1L
  notes <- character(0)
  spec <- noisy_specificity_from_config(config)

  interval_groups <- interval_sequence_groups(intervals)
  x <- if (length(interval_groups) > 0) do.call(rbind, interval_groups) else make_empty_interval_df()
  noisy <- x[x$ISI_Label == "Noisy" & is.finite(x$ISI_s), , drop = FALSE]

  noisy_burst_like <- 0L
  noisy_tonic_like <- 0L
  noisy_pause_like <- 0L
  noisy_same_zone_pair_violations <- 0L
  noisy_global_same_zone_pair_violations <- 0L
  noisy_mode_adjacency_violations <- 0L
  noisy_toniclike_regular_runs <- 0L
  noisy_above_envelope <- 0L
  noisy_near_pause <- 0L
  hf_burst_adjacency_violations <- 0L
  min_noisy_boundary_spikes <- NA_real_

  if (nrow(noisy) > 0) {
    envelope <- noisy_physiological_envelope(config)
    if (length(envelope) == 2 && all(is.finite(envelope)) && envelope[2] >= envelope[1]) {
      noisy_above_envelope <- sum(noisy$ISI_s > envelope[2] + 1e-12, na.rm = TRUE)
    }
    pause_rng_for_guard <- effective_interval_range_from_config(config, "Pause")
    if (length(pause_rng_for_guard) == 2 && all(is.finite(pause_rng_for_guard)) && pause_rng_for_guard[2] >= pause_rng_for_guard[1]) {
      pause_guard <- max(spec$pause_guard_s, spec$pause_guard_ratio * pause_rng_for_guard[1])
      noisy_near_pause <- sum(noisy$ISI_s >= pause_rng_for_guard[1] - pause_guard - 1e-12, na.rm = TRUE)
    }
    zones_noisy <- vapply(noisy$ISI_s, noisy_mode_zone, character(1), config = config, guard_s = 0)
    noisy_burst_like <- sum(zones_noisy == "Burst", na.rm = TRUE)
    noisy_tonic_like <- sum(zones_noisy == "Tonic", na.rm = TRUE)
    noisy_pause_like <- sum(zones_noisy == "Pause", na.rm = TRUE)

    noisy_groups <- split(noisy, interaction(noisy$Train, noisy$Episode, drop = TRUE))
    noisy_boundary_counts <- numeric(0)
    for (g in noisy_groups) {
      vals <- as.numeric(g$ISI_s)
      vals <- vals[is.finite(vals) & vals > 0]
      noisy_boundary_counts <- c(noisy_boundary_counts, length(vals) + 1L)
      if (noisy_same_zone_pair_violation(vals, config, guard_s = 0)) {
        noisy_same_zone_pair_violations <- noisy_same_zone_pair_violations + 1L
      }
      if (length(vals) >= spec$toniclike_min_isi_count) {
        m <- isi_regularity_metrics(vals)
        cv_ok <- is.finite(m$cv) && m$cv >= spec$min_run_cv
        cv2_ok <- is.finite(m$cv2) && m$cv2 >= spec$min_run_cv2
        if (!isTRUE(cv_ok && cv2_ok)) noisy_toniclike_regular_runs <- noisy_toniclike_regular_runs + 1L
      }
    }
    min_noisy_boundary_spikes <- if (length(noisy_boundary_counts) > 0) min(noisy_boundary_counts) else NA_real_
  }
  noisy_global_same_zone_pair_violations <- count_global_noisy_same_zone_pair_violations(x, config, guard_s = 0)

  for (x_train in interval_groups) {
    if (nrow(x_train) < 2) next
    for (i in seq_len(nrow(x_train) - 1L)) {
      a <- as.character(x_train$ISI_Label[i])
      b <- as.character(x_train$ISI_Label[i + 1L])
      if (forbidden_hf_burst_adjacency(b, a)) {
        hf_burst_adjacency_violations <- hf_burst_adjacency_violations + 1L
      }
      mode_labels <- c("Burst", "Tonic", HF_PATTERN_LABELS)
      if (identical(a, "Noisy") && b %in% mode_labels) {
        zone <- noisy_mode_zone(x_train$ISI_s[i], config, guard_s = 0)
        if (!is.na(zone) && zone %in% mode_labels) noisy_mode_adjacency_violations <- noisy_mode_adjacency_violations + 1L
      }
	      if (identical(b, "Noisy") && a %in% mode_labels) {
	        zone <- noisy_mode_zone(x_train$ISI_s[i + 1L], config, guard_s = 0)
	        if (!is.na(zone) && zone %in% mode_labels) noisy_mode_adjacency_violations <- noisy_mode_adjacency_violations + 1L
	      }
	    }
	  }

	  burst_eps <- episodes[episodes$Pattern == "Burst" & episodes$Episode_Scope == "interval_run", , drop = FALSE]
  tonic_eps <- episodes[episodes$Pattern == "Tonic" & episodes$Episode_Scope == "interval_run", , drop = FALSE]
  min_burst_boundary_spikes <- if (nrow(burst_eps) > 0 && "N_Boundary_Spikes" %in% names(burst_eps)) suppressWarnings(min(as.numeric(burst_eps$N_Boundary_Spikes), na.rm = TRUE)) else NA_real_
  min_tonic_boundary_spikes <- if (nrow(tonic_eps) > 0 && "N_Boundary_Spikes" %in% names(tonic_eps)) suppressWarnings(min(as.numeric(tonic_eps$N_Boundary_Spikes), na.rm = TRUE)) else NA_real_
  if (!is.finite(min_burst_boundary_spikes)) min_burst_boundary_spikes <- NA_real_
  if (!is.finite(min_tonic_boundary_spikes)) min_tonic_boundary_spikes <- NA_real_
  if (!is.finite(min_noisy_boundary_spikes)) min_noisy_boundary_spikes <- NA_real_

  if (noisy_pause_like > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d Noisy ISIs were Pause-like", noisy_pause_like))
  }
  if (noisy_above_envelope > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d Noisy ISIs exceeded the Tonic-upper / Pause-guard envelope", noisy_above_envelope))
  }
  if (noisy_near_pause > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d Noisy ISIs were too close to the Pause lower bound", noisy_near_pause))
  }
  if (noisy_same_zone_pair_violations > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d Noisy runs contained consecutive Burst-like/Tonic-like ISIs in the same zone", noisy_same_zone_pair_violations))
  }
  if (noisy_global_same_zone_pair_violations > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d adjacent Noisy ISI pairs crossed episode/event boundaries but remained in the same Burst-like/Tonic-like zone", noisy_global_same_zone_pair_violations))
  }
  if (noisy_mode_adjacency_violations > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d mode-like Noisy ISIs touched a Burst/Tonic/HF interval", noisy_mode_adjacency_violations))
  }
  if (noisy_toniclike_regular_runs > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d Noisy runs were too regular and tonic-like", noisy_toniclike_regular_runs))
  }
  if (hf_burst_adjacency_violations > 0L) {
    clean_label <- 0L
    notes <- c(notes, sprintf("%d Burst/HF interval adjacencies were present", hf_burst_adjacency_violations))
  }
  if (nrow(burst_eps) > 0 && is.finite(min_burst_boundary_spikes) && min_burst_boundary_spikes < BURST_MIN_BOUNDARY_SPIKES) {
    clean_label <- 0L
    notes <- c(notes, sprintf("minimum Burst boundary-spike count was %.0f", min_burst_boundary_spikes))
  }
  if (nrow(tonic_eps) > 0 && is.finite(min_tonic_boundary_spikes) && min_tonic_boundary_spikes < TONIC_MIN_BOUNDARY_SPIKES) {
    clean_label <- 0L
    notes <- c(notes, sprintf("minimum Tonic boundary-spike count was %.0f", min_tonic_boundary_spikes))
  }

  list(
    Benchmark_Clean_Label_OK = clean_label,
    Noisy_BurstLike_Intervals = as.integer(noisy_burst_like),
    Noisy_TonicLike_Intervals = as.integer(noisy_tonic_like),
    Noisy_PauseLike_Intervals = as.integer(noisy_pause_like),
    Noisy_Above_TonicUpper_or_PauseGuard_Intervals = as.integer(noisy_above_envelope),
    Noisy_NearPause_Intervals = as.integer(noisy_near_pause),
    Noisy_SameZone_Pair_Violations = as.integer(noisy_same_zone_pair_violations),
    Noisy_Global_SameZone_Pair_Violations = as.integer(noisy_global_same_zone_pair_violations),
    Noisy_Mode_Adjacency_Violations = as.integer(noisy_mode_adjacency_violations),
    Noisy_TonicLike_or_TooRegular_Runs = as.integer(noisy_toniclike_regular_runs),
    HF_Burst_Adjacency_Violations = as.integer(hf_burst_adjacency_violations),
    Min_Burst_Boundary_Spikes = min_burst_boundary_spikes,
    Min_Tonic_Boundary_Spikes = min_tonic_boundary_spikes,
    Min_Noisy_Boundary_Spikes = min_noisy_boundary_spikes,
    Benchmark_Quality_Note = if (length(notes) == 0) "clean" else paste(notes, collapse = "; ")
  )
}

make_detection_benchmark_config <- function(base_config, difficulty = c("easy", "moderate", "hard")) {
  difficulty <- match.arg(difficulty)
  cfg <- clone_config(base_config)
  cfg$pattern_sequence <- NULL
  cfg$leading_silence_initial_pause <- TRUE
  cfg$total_time <- max(safe_num(cfg$total_time, 25), 30)
  cfg$generation_mode <- "event"
  cfg$ratios <- normalize_pattern_ratios(c(Burst = 18, Pause = 15, Tonic = 22, high_frequency_tonic = 15, high_frequency_spiking = 15, Noisy = 15))
  cfg$benchmark_quality_policy <- "contextual_noisy_refractory_envelope"

  if (is.null(cfg$noisy_specificity)) cfg$noisy_specificity <- list()
  set_noisy_specificity <- function(mm_ratio, avoid_overlap = TRUE) {
    cfg$avoid_noisy_burst_runs <<- TRUE
    cfg$noisy_specificity$avoid_mode_overlap <<- FALSE
    cfg$noisy_specificity$contextual_mode_overlap <<- TRUE
    cfg$noisy_specificity$tolerance <<- NOISY_CONTEXT_GUARD_S
    cfg$noisy_specificity$clean_guard_s <<- NOISY_CONTEXT_GUARD_S
    cfg$noisy_specificity$context_guard_s <<- NOISY_CONTEXT_GUARD_S
    cfg$noisy_specificity$pause_guard_s <<- NOISY_PAUSE_GUARD_S
    cfg$noisy_specificity$pause_guard_ratio <<- NOISY_PAUSE_GUARD_RATIO
    cfg$noisy_specificity$tonic_upper_multiplier <<- NOISY_TONIC_UPPER_MULTIPLIER
    cfg$noisy_specificity$mm_ratio <<- max(NOISY_MIN_MM_RATIO, as.numeric(mm_ratio))
  }

  apply_uniform <- function(pattern, rng, spike_rng = NULL, rho = 0, trend = 0) {
    pat_cfg <- cfg$patterns[[pattern]]
    pat_cfg$dist_type <- "Uniform"
    pat_cfg$params <- list(min = rng[1], max = rng[2])
    pat_cfg$interval_range <- rng
    pat_cfg$temporal_dependence <- list(rho = rho, trend = trend)
    if (!is.null(spike_rng)) pat_cfg$spike_count_range <- spike_rng
    cfg_tmp <- cfg
    cfg_tmp$patterns[[pattern]] <- pat_cfg
    cfg <<- cfg_tmp
  }

  # These benchmark presets are intentionally label-clean. They are not meant to be
  # ambiguous naturalistic examples. The generator now allows an isolated Noisy
  # ISI to be Burst-like/Tonic-like, but the built-in presets remain conservative
  # so that benchmark figures are visually readable.
  if (identical(difficulty, "easy")) {
    cfg$inter_event_gap <- 0.002
    set_noisy_specificity(mm_ratio = 1.8, avoid_overlap = TRUE)
    apply_uniform("Burst", c(0.010, 0.035), c(3, 6), trend = 0.15)
    apply_uniform("Pause", c(0.90, 1.60), NULL)
    apply_uniform("Tonic", c(0.40, 0.50), c(4, 8), rho = 0.25)
    apply_uniform("high_frequency_tonic", c(0.028, 0.038), c(8, 24), rho = 0.20)
    apply_uniform("high_frequency_spiking", c(0.003, 0.020), c(30, 70), rho = 0)
    apply_uniform("Noisy", c(0.10, 0.24), c(3, 7), rho = 0)
    cfg$patterns$Tonic$regularity_ranges <- list(cv = c(0, 0.22), cv2 = c(0, 0.30), lv = c(0, 0.16))
    cfg$patterns$high_frequency_tonic$regularity_ranges <- list(cv = c(0, 0.18), cv2 = c(0, 0.24), lv = c(0, 0.15), mm = c(1, 1.25))
    cfg$patterns$high_frequency_spiking$state_rules <- modifyList(cfg$patterns$high_frequency_spiking$state_rules, list(short_isi_range = c(0.003, 0.012), bridge_isi_range = c(0.012, 0.020), target_short_fraction = 0.90, short_fraction_min = 0.80, bridge_fraction_max = 0.15, max_consecutive_bridge = 2L, min_duration_s = 0.25))
  } else if (identical(difficulty, "moderate")) {
    cfg$inter_event_gap <- 0.002
    set_noisy_specificity(mm_ratio = 1.6, avoid_overlap = TRUE)
    apply_uniform("Burst", c(0.006, 0.045), c(3, 6), trend = 0.25)
    apply_uniform("Pause", c(0.85, 1.50), NULL)
    apply_uniform("Tonic", c(0.36, 0.56), c(4, 8), rho = 0.35)
    apply_uniform("high_frequency_tonic", c(0.024, 0.040), c(8, 24), rho = 0.25)
    apply_uniform("high_frequency_spiking", c(0.003, 0.020), c(30, 70), rho = 0)
    apply_uniform("Noisy", c(0.08, 0.28), c(3, 7), rho = 0)
    cfg$patterns$Tonic$regularity_ranges <- list(cv = c(0, 0.32), cv2 = c(0, 0.45), lv = c(0, 0.30))
    cfg$patterns$high_frequency_tonic$regularity_ranges <- list(cv = c(0, 0.22), cv2 = c(0, 0.28), lv = c(0, 0.22), mm = c(1, 1.35))
    cfg$patterns$high_frequency_spiking$state_rules <- modifyList(cfg$patterns$high_frequency_spiking$state_rules, list(short_isi_range = c(0.003, 0.012), bridge_isi_range = c(0.012, 0.020), target_short_fraction = 0.90, short_fraction_min = 0.80, bridge_fraction_max = 0.15, max_consecutive_bridge = 2L, min_duration_s = 0.20))
  } else {
    cfg$inter_event_gap <- 0.002
    set_noisy_specificity(mm_ratio = 1.35, avoid_overlap = TRUE)
    apply_uniform("Burst", c(0.006, 0.050), c(3, 7), trend = 0.40)
    apply_uniform("Pause", c(0.78, 1.45), NULL)
    apply_uniform("Tonic", c(0.32, 0.62), c(4, 9), rho = 0.45, trend = 0.10)
    apply_uniform("high_frequency_tonic", c(0.022, 0.042), c(8, 26), rho = 0.35, trend = 0.05)
    apply_uniform("high_frequency_spiking", c(0.003, 0.022), c(30, 80), rho = 0)
    apply_uniform("Noisy", c(0.07, 0.30), c(3, 8), rho = 0)
    cfg$patterns$Tonic$regularity_ranges <- list(cv = c(0, 0.48), cv2 = c(0, 0.70), lv = c(0, 0.55))
    cfg$patterns$high_frequency_tonic$regularity_ranges <- list(cv = c(0, 0.30), cv2 = c(0, 0.45), lv = c(0, 0.35), mm = c(1, 1.45))
    cfg$patterns$high_frequency_spiking$state_rules <- modifyList(cfg$patterns$high_frequency_spiking$state_rules, list(short_isi_range = c(0.003, 0.012), bridge_isi_range = c(0.012, 0.022), target_short_fraction = 0.88, short_fraction_min = 0.78, bridge_fraction_max = 0.18, max_consecutive_bridge = 2L, min_duration_s = 0.18))
  }
  cfg
}

run_detection_benchmark_suite <- function(config, seeds = 1:10, difficulties = c("easy", "moderate", "hard")) {
  rows <- list(); idx <- 0L
  for (difficulty in difficulties) {
    cfg <- make_detection_benchmark_config(config, difficulty)
    for (seed in seeds) {
      sim <- validation_simulate_safe(cfg, seed)
      if (!is.null(sim$error) || nrow(sim$spikes) < 2 || nrow(sim$intervals) == 0) {
        idx <- idx + 1L
        rows[[idx]] <- data.frame(
          Difficulty = difficulty, Seed = seed, Success = 0, Failure = 1, N_Intervals = 0L,
          Interval_Accuracy = NA_real_, Interval_MacroF1 = NA_real_,
          Prediction_Length_Mismatch = NA, Unclassified_Intervals = NA_integer_,
          Scorable_Intervals = NA_integer_, Nonpattern_Intervals_Ignored = NA_integer_,
          Episode_Precision = NA_real_, Episode_Recall = NA_real_, Episode_F1 = NA_real_,
          Episode_Mean_IoU = NA_real_, MedianAbs_Onset_Error_s = NA_real_, MedianAbs_Offset_Error_s = NA_real_,
          N_Unclassified = NA_integer_, N_Extra_Predictions = NA_integer_,
          Benchmark_Clean_Label_OK = NA_integer_, Noisy_BurstLike_Intervals = NA_integer_,
          Noisy_TonicLike_Intervals = NA_integer_, Noisy_PauseLike_Intervals = NA_integer_,
          Noisy_Above_TonicUpper_or_PauseGuard_Intervals = NA_integer_, Noisy_NearPause_Intervals = NA_integer_,
          Noisy_SameZone_Pair_Violations = NA_integer_, Noisy_Mode_Adjacency_Violations = NA_integer_,
          Noisy_TonicLike_or_TooRegular_Runs = NA_integer_, HF_Burst_Adjacency_Violations = NA_integer_,
          Min_Burst_Boundary_Spikes = NA_real_,
          Min_Tonic_Boundary_Spikes = NA_real_, Min_Noisy_Boundary_Spikes = NA_real_,
          Status = if (!is.null(sim$error)) as.character(sim$error) else "failed_or_empty_simulation",
          stringsAsFactors = FALSE
        )
        next
      }
      pred <- simple_threshold_interval_detector(sim$spikes$Time, cfg)
      int_score <- score_interval_labels(sim$intervals$ISI_Label, pred)
      pred_ep <- prediction_episodes_from_labels(sim$intervals, pred)
      ep_score <- score_episode_predictions(sim$episodes, pred_ep)
      quality <- benchmark_quality_metrics(sim$intervals, sim$episodes, cfg)
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        Difficulty = difficulty,
        Seed = seed,
        Success = 1,
        Failure = 0,
        N_Intervals = nrow(sim$intervals),
        Interval_Accuracy = int_score$accuracy,
        Interval_MacroF1 = int_score$macro_f1,
        Prediction_Length_Mismatch = isTRUE(int_score$length_mismatch),
        Unclassified_Intervals = int_score$n_unclassified,
        Scorable_Intervals = int_score$n_scorable,
        Nonpattern_Intervals_Ignored = int_score$n_ignored_nonpattern,
        Episode_Precision = ep_score$Precision,
        Episode_Recall = ep_score$Recall,
        Episode_F1 = ep_score$F1,
        Episode_Mean_IoU = ep_score$Mean_IoU,
        MedianAbs_Onset_Error_s = ep_score$MedianAbs_Onset_Error_s,
        MedianAbs_Offset_Error_s = ep_score$MedianAbs_Offset_Error_s,
        N_Unclassified = int_score$n_unclassified,
        N_Extra_Predictions = int_score$n_extra_predictions,
        Benchmark_Clean_Label_OK = quality$Benchmark_Clean_Label_OK,
        Noisy_BurstLike_Intervals = quality$Noisy_BurstLike_Intervals,
        Noisy_TonicLike_Intervals = quality$Noisy_TonicLike_Intervals,
        Noisy_PauseLike_Intervals = quality$Noisy_PauseLike_Intervals,
        Noisy_Above_TonicUpper_or_PauseGuard_Intervals = quality$Noisy_Above_TonicUpper_or_PauseGuard_Intervals,
        Noisy_NearPause_Intervals = quality$Noisy_NearPause_Intervals,
        Noisy_SameZone_Pair_Violations = quality$Noisy_SameZone_Pair_Violations,
        Noisy_Mode_Adjacency_Violations = quality$Noisy_Mode_Adjacency_Violations,
        Noisy_TonicLike_or_TooRegular_Runs = quality$Noisy_TonicLike_or_TooRegular_Runs,
        HF_Burst_Adjacency_Violations = quality$HF_Burst_Adjacency_Violations,
        Min_Burst_Boundary_Spikes = quality$Min_Burst_Boundary_Spikes,
        Min_Tonic_Boundary_Spikes = quality$Min_Tonic_Boundary_Spikes,
        Min_Noisy_Boundary_Spikes = quality$Min_Noisy_Boundary_Spikes,
        Status = if (identical(quality$Benchmark_Clean_Label_OK, 1L)) "ok" else paste("label_quality_warning", quality$Benchmark_Quality_Note),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  raw <- do.call(rbind, rows)
  aggregate_validation_metrics(raw, group_cols = "Difficulty")
}

aggregate_validation_metrics <- function(df, group_cols) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  groups <- split(df, df[group_cols], drop = TRUE)
  rows <- list()
  for (nm in names(groups)) {
    g <- groups[[nm]]
    numeric_cols <- names(g)[vapply(g, is.numeric, logical(1))]
    numeric_cols <- setdiff(numeric_cols, c("Seed"))
    summary <- lapply(numeric_cols, function(col) mean(g[[col]], na.rm = TRUE))
    names(summary) <- paste0(numeric_cols, "_Mean")
    rows[[length(rows) + 1L]] <- data.frame(Group = nm, N = nrow(g), as.data.frame(summary), stringsAsFactors = FALSE, check.names = FALSE)
  }
  do.call(rbind, rows)
}

make_baseline_sim_from_intervals <- function(spike_times, labels = NULL, model = "Baseline") {
  spike_times <- sort(as.numeric(spike_times[is.finite(spike_times) & spike_times > 0]))
  if (length(spike_times) == 0) {
    return(list(spikes = make_empty_spike_df(), intervals = make_empty_interval_df(), episodes = make_empty_episode_df(), warnings = character(0)))
  }
  spikes <- data.frame(Episode = NA_integer_, Time = spike_times, Episode_Context = NA_character_, Spike_Role = "event_spike", stringsAsFactors = FALSE)
  if (length(spike_times) < 2) return(list(spikes = spikes, intervals = make_empty_interval_df(), episodes = make_empty_episode_df(), warnings = character(0)))
  isi <- diff(spike_times)
  if (is.null(labels) || length(labels) < length(isi)) labels <- rep(model, length(isi))
  labels <- as.character(labels[seq_along(isi)])
  breaks <- c(TRUE, labels[-1] != labels[-length(labels)])
  ep <- cumsum(breaks)
  intervals <- data.frame(
    Train = 1L,
    Interval_ID = seq_along(isi),
    Left_Spike_Index = seq_along(isi),
    Right_Spike_Index = seq_along(isi) + 1L,
    Left_Spike_Time_s = spike_times[-length(spike_times)],
    Right_Spike_Time_s = spike_times[-1],
    Start_Time_s = spike_times[-length(spike_times)],
    End_Time_s = spike_times[-1],
    ISI_s = isi,
    Interval = isi,
    ISI_Label = labels,
    Episode = ep,
    ISI_Scope = "baseline_interval",
    Left_Spike_Role = "event_spike",
    Right_Spike_Role = "event_spike",
    Left_Episode_Context = NA_character_,
    Right_Episode_Context = NA_character_,
    Is_Manual_Fixed = FALSE,
    Interval_Source = model,
    Run_Position = NA_real_,
    Run_Length = NA_integer_,
    Temporal_Rho = NA_real_,
    Temporal_Trend = NA_real_,
    Event_Epoch_Type = NA_character_,
    Event_Epoch_Source = NA_character_,
    Event_Epoch_Generation_Rule = NA_character_,
    stringsAsFactors = FALSE
  )
  episodes <- make_empty_episode_df()
  list(spikes = spikes, intervals = intervals, episodes = episodes, warnings = character(0))
}

sample_static_labeled_interval <- function(config, label, previous_isi = NA_real_, previous_pattern = NA_character_) {
  pat_cfg <- config$patterns[[label]]
  segments <- effective_pattern_segments_from_config(config, label, previous_isi, previous_pattern)
  if (nrow(segments) == 0) return(NA_real_)
  sample_truncated_interval_from_segments(pat_cfg$dist_type, pat_cfg$params, segments)
}

simulate_hpp_baseline <- function(total_time, rate_hz, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  total_time <- safe_num(total_time, 25)
  rate_hz <- safe_num(rate_hz, 1)
  if (!is.finite(rate_hz) || rate_hz <= 0) rate_hz <- 1 / max(total_time, 1)
  times <- numeric(0); t <- 0
  while (t < total_time) {
    t <- t + stats::rexp(1, rate = rate_hz)
    if (t < total_time && t > 0) times <- c(times, t)
  }
  make_baseline_sim_from_intervals(times, model = "Poisson")
}

simulate_renewal_mixture_baseline <- function(config, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  total_time <- safe_num(config$total_time, 25)
  ratios <- normalize_pattern_ratios(config$ratios)
  labels <- character(0); intervals <- numeric(0)
  first_label <- sample(names(ratios), 1, prob = ratios)
  t <- sample_static_labeled_interval(config, first_label)
  if (!is.finite(t) || t <= 0) t <- .Machine$double.eps
  spike_times <- c(t)
  prev_isi <- NA_real_; prev_label <- NA_character_
  while (t < total_time) {
    label <- sample(names(ratios), 1, prob = ratios)
    isi <- sample_static_labeled_interval(config, label, prev_isi, prev_label)
    if (!is.finite(isi) || isi <= 0) break
    if (t + isi >= total_time) break
    t <- t + isi
    spike_times <- c(spike_times, t)
    intervals <- c(intervals, isi)
    labels <- c(labels, label)
    prev_isi <- isi; prev_label <- label
  }
  make_baseline_sim_from_intervals(spike_times, labels = labels, model = "Renewal")
}

simulate_markov_label_baseline <- function(config, seed = NULL, persistence = 0.85) {
  if (!is.null(seed)) set.seed(seed)
  total_time <- safe_num(config$total_time, 25)
  ratios <- normalize_pattern_ratios(config$ratios)
  labels_all <- names(ratios)
  current_label <- sample(labels_all, 1, prob = ratios)
  t <- sample_static_labeled_interval(config, current_label)
  if (!is.finite(t) || t <= 0) t <- .Machine$double.eps
  spike_times <- c(t); labels <- character(0)
  prev_isi <- NA_real_; prev_label <- NA_character_
  while (t < total_time) {
    if (stats::runif(1) > persistence || length(labels) == 0) {
      current_label <- sample(labels_all, 1, prob = ratios)
    }
    isi <- sample_static_labeled_interval(config, current_label, prev_isi, prev_label)
    if (!is.finite(isi) || isi <= 0) break
    if (t + isi >= total_time) break
    t <- t + isi
    spike_times <- c(spike_times, t)
    labels <- c(labels, current_label)
    prev_isi <- isi; prev_label <- current_label
  }
  make_baseline_sim_from_intervals(spike_times, labels = labels, model = "Markov")
}

extract_feature_vector <- function(sim, config = NULL) {
  spikes <- sim$spikes
  intervals <- sim$intervals
  times <- if (!is.null(spikes) && nrow(spikes) > 0) sort(spikes$Time) else numeric(0)
  isi <- if (!is.null(intervals) && nrow(intervals) > 0) intervals$ISI_s else if (length(times) > 1) diff(times) else numeric(0)
  isi <- isi[is.finite(isi) & isi > 0]
  metrics <- isi_regularity_metrics(isi)
  acf1 <- if (length(isi) > 2) suppressWarnings(stats::cor(isi[-length(isi)], isi[-1])) else NA_real_
  total_duration <- if (!is.null(config)) safe_num(config$total_time, NA_real_) else if (length(times) > 0) max(times) else NA_real_
  brng <- if (!is.null(config)) effective_interval_range_from_config(config, "Burst") else c(NA_real_, NA_real_)
  prng <- if (!is.null(config)) effective_interval_range_from_config(config, "Pause") else c(NA_real_, NA_real_)
  short_frac <- if (length(isi) > 0 && length(brng) == 2 && all(is.finite(brng))) mean(isi <= brng[2]) else NA_real_
  long_frac <- if (length(isi) > 0 && length(prng) == 2 && all(is.finite(prng))) mean(isi >= prng[1]) else NA_real_
  c(
    firing_rate = if (is.finite(total_duration) && total_duration > 0) length(times) / total_duration else NA_real_,
    mean_isi = metrics$mean,
    cv = metrics$cv,
    cv2 = metrics$cv2,
    lv = metrics$lv,
    acf1 = acf1,
    short_isi_fraction = short_frac,
    long_isi_fraction = long_frac
  )
}

run_baseline_comparison_suite <- function(config, seeds = 1:10, difficulty = "moderate") {
  cfg <- make_detection_benchmark_config(config, difficulty)
  feature_rows <- list(); idx <- 0L
  for (seed in seeds) {
    ref <- validation_simulate_safe(cfg, seed)
    ref_feat <- extract_feature_vector(ref, cfg)
    ref_rate <- ref_feat["firing_rate"]
    models <- list(
      V13 = ref,
      Poisson = simulate_hpp_baseline(cfg$total_time, ref_rate, seed + 1000L),
      Renewal = simulate_renewal_mixture_baseline(cfg, seed + 2000L),
      Markov = simulate_markov_label_baseline(cfg, seed + 3000L, persistence = 0.85)
    )
    for (model in names(models)) {
      feat <- extract_feature_vector(models[[model]], cfg)
      rel_err <- mean(abs(feat - ref_feat) / (abs(ref_feat) + 1e-9), na.rm = TRUE)
      idx <- idx + 1L
      feature_rows[[idx]] <- data.frame(
        Model = model,
        Seed = seed,
        Mean_Relative_Feature_Error_to_V13 = if (identical(model, "V13")) 0 else rel_err,
        as.data.frame(as.list(feat)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  raw <- do.call(rbind, feature_rows)
  aggregate_validation_metrics(raw, group_cols = "Model")
}

make_stimulation_validation_config <- function(config, preset) {
  cfg <- clone_config(config)
  cfg$total_time <- max(30, safe_num(cfg$total_time, 25))
  if (is.null(cfg$validation)) cfg$validation <- list()
  cfg$validation$check_noisy_clean_label_constraints <- TRUE
  cfg$stimulation <- list(
    enabled = TRUE,
    experiment_preset = preset,
    start_s = 3,
    duration_s = 0.02,
    n_stimuli = 6,
    inter_stimulus_interval_s = 3.5,
    paired_pulse_interval_s = 0.25,
    strength = 0.75,
    strength_end = 1.0,
    strength_jitter = 0,
    manual_times = "",
    manual_strengths = "",
    feature_modality = "orientation",
    feature_values = "15,45,90,135,180,225,270,315",
    preferred_feature = 15,
    null_feature = 90,
    feature_period = 180,
    feature_tuning_width = 25,
    feature_suppression_width = 25,
    feature_min_gain = 0.05,
    feature_preferred_response = "excitatory_burst",
    feature_null_response = "no_response",
    deviant_probability = 0.35,
    deviant_strength = 1.0,
    response_latency_median_s = 0.08,
    response_latency_sdlog = 0.20,
    response_probability = 1.0,
    response_window_s = 0.80,
    baseline_recovery_enabled = TRUE,
    baseline_recovery_mode = "Noisy",
    pre_stimulus_guard_s = 0.02,
    max_evoked_bursts = 3,
    burst_lambda_base = 0.25,
    burst_lambda_strength = 2.8,
    evoked_burst_spike_min = 3,
    evoked_burst_spike_max = 7,
    pause_duration_min_s = 0.18,
    pause_duration_max_s = 0.32,
    pause_duration_cv = 0.20,
    post_burst_pause_probability = 0.0,
    rebound_probability = 0.55,
    pre_stimulus_window_s = 1.0,
    burst_load_weight = 1.0,
    pause_load_weight = 1.0,
    reference_pause_s = 1.0,
    adaptation_enabled = TRUE,
    adaptation_increment = 0.35,
    adaptation_tau_s = 12,
    adaptation_source = "mixed",
    response_floor = 0.15,
    force_mixed_oddball = FALSE,
    channel = "A"
  )
  if (identical(preset, "intensity_response")) {
    cfg$stimulation$strength <- 0.2
    cfg$stimulation$strength_end <- 1.0
    cfg$stimulation$n_stimuli <- 8
    cfg$stimulation$inter_stimulus_interval_s <- 3.0
    cfg$stimulation$response_window_s <- 1.05
    cfg$stimulation$max_evoked_bursts <- 4
    cfg$stimulation$burst_lambda_base <- 0.60
    cfg$stimulation$burst_lambda_strength <- 4.0
    cfg$stimulation$evoked_burst_spike_max <- 4
  } else if (identical(preset, "paired_pulse_recovery")) {
    cfg$stimulation$n_stimuli <- 8
    cfg$stimulation$inter_stimulus_interval_s <- 5.0
    cfg$stimulation$paired_pulse_interval_s <- 0.60
    cfg$stimulation$response_window_s <- 0.95
    cfg$stimulation$max_evoked_bursts <- 3
    cfg$stimulation$evoked_burst_spike_max <- 4
  } else if (identical(preset, "oddball_adaptation")) {
    cfg$stimulation$n_stimuli <- 36
    cfg$stimulation$inter_stimulus_interval_s <- 0.50
    cfg$stimulation$deviant_probability <- 0.10
    cfg$stimulation$force_mixed_oddball <- TRUE
    cfg$stimulation$response_window_s <- 0.42
    cfg$stimulation$max_evoked_bursts <- 3
    cfg$stimulation$evoked_burst_spike_min <- 4
    cfg$stimulation$evoked_burst_spike_max <- 4
  } else if (identical(preset, "stimulus_suppression")) {
    cfg$stimulation$response_latency_median_s <- 0.09
    cfg$stimulation$response_window_s <- 0.55
    cfg$stimulation$pause_duration_min_s <- 0.15
    cfg$stimulation$pause_duration_max_s <- 0.30
    cfg$stimulation$pause_duration_cv <- 0.20
  } else if (identical(preset, "biphasic_burst_pause")) {
    cfg$stimulation$response_latency_median_s <- 0.06
    cfg$stimulation$response_window_s <- 0.80
    cfg$stimulation$max_evoked_bursts <- 2
    cfg$stimulation$burst_lambda_base <- 0.35
    cfg$stimulation$burst_lambda_strength <- 2.2
    cfg$stimulation$post_burst_pause_probability <- 1.0
    cfg$stimulation$evoked_burst_spike_min <- 3
    cfg$stimulation$evoked_burst_spike_max <- 4
    cfg$stimulation$pause_duration_min_s <- 0.18
    cfg$stimulation$pause_duration_max_s <- 0.32
    cfg$stimulation$pause_duration_cv <- 0.20
  } else if (identical(preset, "state_dependent_balanced")) {
    cfg$stimulation$experiment_preset <- "state_dependent_balanced"
    cfg$stimulation$protocol <- "regular"
    cfg$stimulation$response_type <- "state_dependent"
    cfg$stimulation$n_stimuli <- 16
    cfg$stimulation$inter_stimulus_interval_s <- 1.6
    cfg$stimulation$strength <- 0.75
    cfg$stimulation$strength_end <- 0.75
    cfg$stimulation$response_probability <- 1.0
    cfg$stimulation$response_latency_median_s <- 0.06
    cfg$stimulation$response_latency_sdlog <- 0.15
    cfg$stimulation$response_window_s <- 0.95
    cfg$stimulation$pre_stimulus_state_sequence <- c("Burst", "Tonic", "Noisy", "Pause")
    cfg$stimulation$max_evoked_bursts <- 2
    cfg$stimulation$burst_lambda_base <- 0.40
    cfg$stimulation$burst_lambda_strength <- 2.4
    cfg$stimulation$evoked_burst_spike_min <- 3
    cfg$stimulation$evoked_burst_spike_max <- 4
    cfg$stimulation$pause_duration_min_s <- 0.16
    cfg$stimulation$pause_duration_max_s <- 0.34
    cfg$stimulation$pause_duration_cv <- 0.20
    cfg$stimulation$post_burst_pause_probability <- 1.0
    cfg$stimulation$rebound_probability <- 0.70
    cfg$stimulation$adaptation_increment <- 0.10
    cfg$stimulation$adaptation_tau_s <- 4.0
    cfg$stimulation$response_floor <- 0.45
  } else if (identical(preset, "feature_tuning")) {
    cfg$stimulation$n_stimuli <- 16
    cfg$stimulation$inter_stimulus_interval_s <- 1.5
    cfg$stimulation$strength <- 0.85
    cfg$stimulation$strength_end <- 0.85
    cfg$stimulation$response_latency_median_s <- 0.07
    cfg$stimulation$response_window_s <- 0.75
    cfg$stimulation$feature_modality <- "orientation"
    cfg$stimulation$feature_values <- "15,45,90,135,15,90,180,270,15,45,90,135,180,225,270,315"
    cfg$stimulation$preferred_feature <- 15
    cfg$stimulation$null_feature <- 90
    cfg$stimulation$feature_period <- 180
    cfg$stimulation$feature_tuning_width <- 25
    cfg$stimulation$feature_suppression_width <- 25
    cfg$stimulation$feature_min_gain <- 0.05
    cfg$stimulation$feature_preferred_response <- "excitatory_burst"
    cfg$stimulation$feature_null_response <- "no_response"
    cfg$stimulation$max_evoked_bursts <- 3
    cfg$stimulation$burst_lambda_base <- 0.35
    cfg$stimulation$burst_lambda_strength <- 3.0
    cfg$stimulation$evoked_burst_spike_min <- 3
    cfg$stimulation$evoked_burst_spike_max <- 5
    cfg$stimulation$pause_duration_min_s <- 0.16
    cfg$stimulation$pause_duration_max_s <- 0.32
    cfg$stimulation$pause_duration_cv <- 0.20
    cfg$stimulation$post_burst_pause_probability <- 0.10
    cfg$stimulation$adaptation_increment <- 0.15
    cfg$stimulation$adaptation_tau_s <- 4.0
    cfg$stimulation$response_floor <- 0.25
  }
  cfg
}

make_manuscript_stimulation_validation_config <- function(config, preset) {
  cfg <- make_stimulation_validation_config(config, preset)
  if (is.null(cfg$validation)) cfg$validation <- list()
  cfg$validation$config_profile <- "manuscript_stimulation_validation"
  # Manuscript validation repeatedly inserts short baseline-recovery fragments
  # between stimuli. Ratio-based recovery keeps those fragments representative of
  # the configured pattern mixture, while short Noisy fragments prevent recovery
  # segments from becoming artifactually tonic-like under clean-label checks.
  cfg$patterns$Noisy$spike_count_range <- c(1, 3)
  cfg$stimulation$baseline_recovery_mode <- "ratio"
  cfg
}

validation_spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || length(unique(x[ok])) < 2 || length(unique(y[ok])) < 2) return(NA_real_)
  suppressWarnings(as.numeric(cor(x[ok], y[ok], method = "spearman")))
}

validation_finite_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) > 0) mean(x) else NA_real_
}

validation_slope <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2 || length(unique(x[ok])) < 2) return(NA_real_)
  suppressWarnings(as.numeric(coef(lm(y[ok] ~ x[ok]))[2]))
}

stimulation_preset_quantitative_metrics <- function(preset, stimuli, responses, event_epochs = NULL) {
  out <- list(
    metric = "not_applicable",
    effect_size = NA_real_,
    trend_ok = NA,
    slope = NA_real_,
    correlation = NA_real_,
    status = "not_applicable",
    note = "no quantitative trend expected"
  )
  if (is.null(stimuli) || is.null(responses) || nrow(stimuli) == 0 || nrow(responses) == 0) {
    out$note <- "empty stimulation output"
    out$status <- "empty_output"
    return(out)
  }
  key_cols <- intersect(c("Train", "Stimulus_ID"), intersect(names(stimuli), names(responses)))
  joined <- if (length(key_cols) == 2) {
    merge(stimuli, responses, by = key_cols, all = FALSE, suffixes = c("_stim", ""))
  } else {
    responses
  }
  if (!"Repetition_Index" %in% names(joined)) joined$Repetition_Index <- seq_len(nrow(joined))
  if (!"Strength" %in% names(joined)) joined$Strength <- NA_real_
  if (!"Evoked_Spike_Count" %in% names(joined)) joined$Evoked_Spike_Count <- NA_real_
  if (!"Response_Gain" %in% names(joined)) joined$Response_Gain <- NA_real_
  if (!"Evoked_Suppression_Duration_s" %in% names(joined)) joined$Evoked_Suppression_Duration_s <- NA_real_
  if (!"Suppression_Index" %in% names(joined)) joined$Suppression_Index <- NA_real_

  if (identical(preset, "intensity_response")) {
    x <- as.numeric(joined$Strength)
    y <- as.numeric(joined$Evoked_Spike_Count)
    rho <- validation_spearman(x, y)
    slope <- validation_slope(x, y)
    out$metric <- "spearman_strength_evoked_spikes"
    out$effect_size <- rho
    out$slope <- slope
    out$correlation <- rho
    out$trend_ok <- is.finite(rho) && is.finite(slope) && rho > 0 && slope > 0
    out$note <- sprintf("Spearman(strength, evoked spikes)=%.3g; slope=%.3g", rho, slope)
  } else if (identical(preset, "repeated_adaptation")) {
    x <- as.numeric(joined$Repetition_Index)
    y <- as.numeric(joined$Response_Gain)
    rho <- validation_spearman(x, y)
    slope <- validation_slope(x, y)
    out$metric <- "spearman_repetition_response_gain"
    out$effect_size <- rho
    out$slope <- slope
    out$correlation <- rho
    out$trend_ok <- is.finite(rho) && is.finite(slope) && rho < 0 && slope < 0
    out$note <- sprintf("Spearman(repetition, response gain)=%.3g; slope=%.3g", rho, slope)
  } else if (identical(preset, "stimulus_suppression")) {
    suppression <- as.numeric(joined$Suppression_Index)
    if (!any(is.finite(suppression))) {
      suppression <- as.numeric(joined$Evoked_Suppression_Duration_s)
    }
    effect <- validation_finite_mean(suppression)
    out$metric <- "mean_suppression_index_or_duration"
    out$effect_size <- effect
    out$trend_ok <- is.finite(effect) && effect > 0
    out$note <- sprintf("mean suppression metric=%.3g", effect)
  } else if (identical(preset, "biphasic_burst_pause")) {
    event_epochs <- if (!is.null(event_epochs)) event_epochs else make_empty_event_epoch_df()
    if (nrow(event_epochs) > 0 && all(c("Stimulus_ID", "Epoch_Type", "Start_s") %in% names(event_epochs))) {
      stim_ids <- unique(event_epochs$Stimulus_ID[is.finite(event_epochs$Stimulus_ID)])
      order_ok <- vapply(stim_ids, function(stim_id) {
        ee <- event_epochs[event_epochs$Stimulus_ID == stim_id, , drop = FALSE]
        burst_start <- suppressWarnings(min(ee$Start_s[ee$Epoch_Type %in% c("evoked_burst_epoch", "rebound_burst_epoch")], na.rm = TRUE))
        suppression_start <- suppressWarnings(min(ee$Start_s[ee$Epoch_Type %in% c("suppression_epoch")], na.rm = TRUE))
        is.finite(burst_start) && is.finite(suppression_start) && burst_start <= suppression_start + 1e-9
      }, logical(1))
      effect <- if (length(order_ok) > 0) mean(order_ok) else NA_real_
    } else {
      effect <- NA_real_
    }
    out$metric <- "fraction_burst_before_suppression"
    out$effect_size <- effect
    out$trend_ok <- is.finite(effect) && effect >= 0.5
    out$note <- sprintf("fraction with burst before suppression=%.3g", effect)
  } else if (identical(preset, "paired_pulse_recovery")) {
    if ("Stimulus_Type" %in% names(joined) && "Pair_ID" %in% names(joined)) {
      p1 <- joined[joined$Stimulus_Type == "paired_pulse_1", c("Pair_ID", "Response_Gain"), drop = FALSE]
      p2 <- joined[joined$Stimulus_Type == "paired_pulse_2", c("Pair_ID", "Response_Gain"), drop = FALSE]
      paired <- merge(p1, p2, by = "Pair_ID", suffixes = c("_P1", "_P2"))
      ratio <- paired$Response_Gain_P2 / pmax(paired$Response_Gain_P1, .Machine$double.eps)
      effect <- validation_finite_mean(ratio)
    } else {
      effect <- NA_real_
    }
    out$metric <- "mean_p2_p1_response_gain_ratio"
    out$effect_size <- effect
    out$trend_ok <- is.finite(effect) && effect < 1
    out$note <- sprintf("mean P2/P1 response-gain ratio=%.3g", effect)
  } else if (identical(preset, "oddball_adaptation")) {
    if ("Stimulus_Type" %in% names(joined)) {
      standard <- joined$Evoked_Spike_Count[joined$Stimulus_Type == "standard"]
      deviant <- joined$Evoked_Spike_Count[joined$Stimulus_Type == "deviant"]
      effect <- validation_finite_mean(deviant) / pmax(validation_finite_mean(standard), .Machine$double.eps)
    } else {
      effect <- NA_real_
    }
    out$metric <- "deviant_standard_evoked_spike_ratio"
    out$effect_size <- effect
    out$trend_ok <- is.finite(effect) && effect > 1
    out$note <- sprintf("deviant/standard evoked-spike ratio=%.3g", effect)
  } else if (identical(preset, "feature_tuning")) {
    cls <- if ("Feature_Response_Class" %in% names(joined)) as.character(joined$Feature_Response_Class) else rep(NA_character_, nrow(joined))
    response_type <- if ("Response_Type" %in% names(joined)) as.character(joined$Response_Type) else rep(NA_character_, nrow(joined))
    pref_idx <- cls %in% c("preferred_excitatory", "preferred_biphasic")
    null_idx <- cls == "null_suppressive"
    baseline_idx <- cls %in% c("neutral_baseline", "nonresponsive")
    pref_spikes <- joined$Evoked_Spike_Count[pref_idx]
    comparison_spikes <- joined$Evoked_Spike_Count[null_idx | baseline_idx]
    pref_supp <- joined$Evoked_Suppression_Duration_s[pref_idx]
    null_supp <- joined$Evoked_Suppression_Duration_s[null_idx]
    spike_delta <- validation_finite_mean(pref_spikes) - validation_finite_mean(comparison_spikes)
    null_requires_suppression <- any(null_idx & response_type %in% c("suppressive_pause", "pause_rebound", "biphasic"), na.rm = TRUE)
    suppression_delta <- if (isTRUE(null_requires_suppression)) {
      validation_finite_mean(null_supp) - validation_finite_mean(pref_supp)
    } else {
      NA_real_
    }
    effect <- if (isTRUE(null_requires_suppression)) min(spike_delta, suppression_delta) else spike_delta
    out$metric <- if (isTRUE(null_requires_suppression)) {
      "preferred_spike_and_optional_null_suppression_separation"
    } else {
      "preferred_spike_over_nonpreferred_baseline_separation"
    }
    out$effect_size <- effect
    out$slope <- NA_real_
    out$correlation <- NA_real_
    out$trend_ok <- is.finite(spike_delta) && spike_delta > 0 &&
      (!isTRUE(null_requires_suppression) || (is.finite(suppression_delta) && suppression_delta > 0))
    out$note <- if (isTRUE(null_requires_suppression)) {
      sprintf("preferred-nonpreferred spike delta=%.3g; null-preferred suppression delta=%.3g",
              spike_delta, suppression_delta)
    } else {
      sprintf("preferred-nonpreferred spike delta=%.3g; null/opponent kernel configured as baseline/no-response",
              spike_delta)
    }
  } else if (preset %in% c("state_dependent", "state_dependent_balanced")) {
    has_state <- "Pre_Stimulus_State" %in% names(joined) && "Response_Type" %in% names(joined)
    state <- if (has_state) as.character(joined$Pre_Stimulus_State) else character(0)
    response_type <- if (has_state) as.character(joined$Response_Type) else character(0)
    ok_state <- has_state & nzchar(state) & nzchar(response_type) &
      !is.na(state) & !is.na(response_type) &
      !state %in% c("NA", "unknown", "none") &
      !response_type %in% c("NA", "no_response")
    state <- state[ok_state]
    response_type <- response_type[ok_state]
    tab <- if (length(state) > 0) table(state, response_type) else matrix(integer(0), nrow = 0, ncol = 0)
    state_counts <- if (length(tab) > 0) rowSums(tab) else numeric(0)
    response_counts <- if (length(tab) > 0) colSums(tab) else numeric(0)
    n_states <- sum(state_counts > 0)
    n_response_types <- sum(response_counts > 0)
    min_state_count <- if (length(state_counts) > 0) min(state_counts[state_counts > 0]) else 0
    n_valid <- sum(tab)
    out$metric <- "state_response_contingency_cramers_v"
    required_min_state_count <- if (identical(preset, "state_dependent_balanced")) 3 else 2
    if (n_valid < 6 || n_states < 2 || n_response_types < 2 || min_state_count < required_min_state_count) {
      out$effect_size <- NA_real_
      out$correlation <- NA_real_
      out$trend_ok <- NA
      out$status <- "insufficient_state_coverage"
      out$note <- sprintf(
        "insufficient_state_coverage: n_valid=%d; states=%d; response_types=%d; min_state_count=%d; required_min_state_count=%d",
        as.integer(n_valid), as.integer(n_states), as.integer(n_response_types), as.integer(min_state_count),
        as.integer(required_min_state_count)
      )
    } else {
      chi <- suppressWarnings(as.numeric(stats::chisq.test(tab, correct = FALSE)$statistic))
      denom <- as.numeric(n_valid) * max(1, min(n_states - 1, n_response_types - 1))
      cramers_v <- if (is.finite(chi) && denom > 0) sqrt(chi / denom) else NA_real_
      diversity <- n_response_types / max(1, n_states)
      out$effect_size <- cramers_v
      out$correlation <- cramers_v
      out$trend_ok <- is.finite(cramers_v) && cramers_v >= 0.10
      out$status <- "evaluated"
      out$note <- sprintf(
        "state-response contingency Cramer's V=%.3g; states=%d; response_types=%d; min_state_count=%d; diversity=%.3g",
        cramers_v, as.integer(n_states), as.integer(n_response_types), as.integer(min_state_count), diversity
      )
    }
  }
  if (identical(out$status, "not_applicable") && !is.na(out$trend_ok)) out$status <- "evaluated"
  out
}

stimulation_preset_phenomenology <- function(preset, stimuli, responses) {
  if (is.null(stimuli) || is.null(responses) || nrow(stimuli) == 0 || nrow(responses) == 0) {
    return(list(ok = FALSE, note = "empty stimulation output"))
  }
  ok <- TRUE
  note <- "ok"
  if (identical(preset, "intensity_response")) {
    ok <- sum(responses$Evoked_Spike_Count, na.rm = TRUE) > 0
    note <- if (ok) "evoked spikes generated across intensity ramp" else "no evoked spikes in intensity ramp"
  } else if (identical(preset, "repeated_adaptation")) {
    gains <- responses$Response_Gain[is.finite(responses$Response_Gain)]
    ok <- length(gains) >= 2 && tail(gains, 1) <= gains[1] + 1e-9
    note <- if (ok) "response gain decreases with repeated stimulation" else "response gain did not decrease"
  } else if (identical(preset, "stimulus_suppression")) {
    suppression <- if ("Evoked_Suppression_Duration_s" %in% names(responses)) responses$Evoked_Suppression_Duration_s else responses$Evoked_Pause_Duration_s
    ok <- sum(suppression, na.rm = TRUE) > 0
    note <- if (ok) "suppressive pauses generated" else "no suppressive pause generated"
  } else if (identical(preset, "biphasic_burst_pause")) {
    suppression <- if ("Evoked_Suppression_Duration_s" %in% names(responses)) responses$Evoked_Suppression_Duration_s else responses$Evoked_Pause_Duration_s
    ok <- sum(responses$Evoked_Burst_Count, na.rm = TRUE) > 0 && sum(suppression, na.rm = TRUE) > 0
    note <- if (ok) "biphasic burst and pause components generated" else "missing burst or pause component"
  } else if (identical(preset, "paired_pulse_recovery")) {
    joined <- merge(stimuli[, c("Stimulus_ID", "Pair_ID", "Stimulus_Type"), drop = FALSE],
                    responses[, c("Stimulus_ID", "Response_Gain"), drop = FALSE],
                    by = "Stimulus_ID", all = FALSE)
    p1 <- joined[joined$Stimulus_Type == "paired_pulse_1", , drop = FALSE]
    p2 <- joined[joined$Stimulus_Type == "paired_pulse_2", , drop = FALSE]
    if (nrow(p1) > 0 && nrow(p2) > 0) {
      paired <- merge(p1[, c("Pair_ID", "Response_Gain"), drop = FALSE],
                      p2[, c("Pair_ID", "Response_Gain"), drop = FALSE],
                      by = "Pair_ID", suffixes = c("_P1", "_P2"))
      ok <- nrow(paired) > 0 && mean(paired$Response_Gain_P2 <= paired$Response_Gain_P1 + 1e-9, na.rm = TRUE) >= 0.5
      note <- if (ok) "second pulse gain is depressed in most pairs" else "paired-pulse depression not evident"
    } else {
      ok <- FALSE
      note <- "paired-pulse schedule incomplete"
    }
  } else if (identical(preset, "oddball_adaptation")) {
    types <- unique(as.character(stimuli$Stimulus_Type))
    ok <- all(c("standard", "deviant") %in% types)
    note <- if (ok) "standard and deviant streams represented with stimulus-specific adaptation state" else "oddball stream lacks standard or deviant events"
  } else if (preset %in% c("state_dependent", "state_dependent_balanced")) {
    state <- if ("Pre_Stimulus_State" %in% names(responses)) as.character(responses$Pre_Stimulus_State) else character(0)
    response_type <- if ("Response_Type" %in% names(responses)) as.character(responses$Response_Type) else character(0)
    state <- state[nzchar(state) & !is.na(state) & !state %in% c("NA", "unknown", "none")]
    response_type <- response_type[nzchar(response_type) & !is.na(response_type) & !response_type %in% c("NA", "no_response")]
    n_states <- length(unique(state))
    n_response_types <- length(unique(response_type))
    ok <- if (identical(preset, "state_dependent_balanced")) {
      n_states >= 3 && n_response_types >= 2
    } else {
      n_response_types > 0
    }
    note <- if (ok) {
      paste("state-dependent coverage: states=", n_states, "; response_types=", n_response_types, sep = "")
    } else {
      paste("insufficient state-dependent coverage: states=", n_states, "; response_types=", n_response_types, sep = "")
    }
  } else if (identical(preset, "feature_tuning")) {
    joined <- merge(stimuli, responses, by = "Stimulus_ID", all = FALSE, suffixes = c("_stim", ""))
    classes <- if ("Feature_Response_Class" %in% names(joined)) unique(as.character(joined$Feature_Response_Class)) else character(0)
    cls <- if ("Feature_Response_Class" %in% names(joined)) as.character(joined$Feature_Response_Class) else rep(NA_character_, nrow(joined))
    response_type <- if ("Response_Type" %in% names(joined)) as.character(joined$Response_Type) else rep(NA_character_, nrow(joined))
    pref_idx <- cls %in% c("preferred_excitatory", "preferred_biphasic")
    null_idx <- cls == "null_suppressive"
    has_pref <- any(cls %in% c("preferred_excitatory", "preferred_biphasic")) &&
      sum(joined$Evoked_Spike_Count[pref_idx], na.rm = TRUE) > 0
    has_null_class <- "null_suppressive" %in% classes
    null_requires_suppression <- any(null_idx & response_type %in% c("suppressive_pause", "pause_rebound", "biphasic"), na.rm = TRUE)
    has_null_suppression <- !isTRUE(null_requires_suppression) ||
      sum(joined$Evoked_Suppression_Duration_s[null_idx], na.rm = TRUE) > 0
    ok <- has_pref && has_null_class && has_null_suppression
    note <- if (ok && isTRUE(null_requires_suppression)) {
      "preferred features evoke burst responses and optional null/opponent suppressive kernel is expressed"
    } else if (ok) {
      "preferred features evoke burst responses; null/opponent stimuli are represented as baseline/no-response by default"
    } else {
      "feature tuning lacks preferred evoked response or null/opponent coverage"
    }
  }
  list(ok = isTRUE(ok), note = note)
}

run_stimulation_validation_suite <- function(config, seed = 1L) {
  presets <- c(
    "intensity_response",
    "repeated_adaptation",
    "stimulus_suppression",
    "biphasic_burst_pause",
    "paired_pulse_recovery",
    "oddball_adaptation",
    "state_dependent",
    "state_dependent_balanced",
    "feature_tuning"
  )
  rows <- list()
  for (i in seq_along(presets)) {
    preset <- presets[i]
    cfg <- make_manuscript_stimulation_validation_config(config, preset)
    sim <- validation_simulate_safe(cfg, as.integer(seed) + i - 1L)
    intervals <- if (!is.null(sim$intervals)) sim$intervals else make_empty_interval_df()
    episodes <- if (!is.null(sim$episodes)) sim$episodes else make_empty_episode_df()
    stimuli <- if (!is.null(sim$stimuli)) sim$stimuli else make_empty_stimulus_df()
    responses <- if (!is.null(sim$responses)) sim$responses else make_empty_response_df()
    event_epochs <- if (!is.null(sim$event_epochs)) sim$event_epochs else make_empty_event_epoch_df()
    quality <- if (is.null(sim$error)) benchmark_quality_metrics(intervals, episodes, cfg) else NULL
    interval_spike_ok <- is.null(sim$error) && check_interval_spike_consistency(sim$spikes, intervals)
    episode_contiguity_ok <- is.null(sim$error) && check_episode_time_contiguity(intervals)
    noisy_clean_ok <- is.null(sim$error) && !is.null(quality) && identical(quality$Benchmark_Clean_Label_OK, 1L)
    latency_label_ok <- nrow(intervals) == 0 || !any(intervals$Stimulus_Phase == "response_latency" & intervals$ISI_Label %in% SPIKE_PATTERN_LEVELS, na.rm = TRUE)
    interburst_gap_label_ok <- nrow(intervals) == 0 || !any(intervals$Response_Epoch == "interburst_gap" & intervals$ISI_Label == "Latency", na.rm = TRUE)
    stimulus_spanning_gap_label_ok <- nrow(intervals) == 0 || !any(intervals$Response_Epoch == "stimulus_spanning_gap" & intervals$ISI_Label == "Latency", na.rm = TRUE)
    response_count_ok <- nrow(stimuli) > 0 && nrow(responses) == nrow(stimuli)
    response_cols <- c("Generated_Response_Start_s", "Generated_Response_End_s", "Expected_Response_Window_s",
                       "Response_Plan_Feasible", "Response_Plan_Start_s", "Response_Plan_End_s",
                       "Response_Plan_Min_Duration_s", "Response_Plan_Required_Components",
                       "Response_Plan_Failure_Reason", "Response_Rolled_Back", "Response_Commit_OK",
                       "Evoked_Suppression_Duration_s", "Scorable_Evoked_Pause_Duration_s",
                       "Response_Load", "Response_Generated_OK", "Response_Truncated",
                       "Response_Failure_Reason", "Response_Failure_Class",
                       "Window_Limited", "Pre_Stimulus_State")
    response_columns_ok <- all(response_cols %in% names(responses))
    response_failure_audit_ok <- response_columns_ok && nrow(responses) > 0 &&
      all(is.logical(responses$Response_Generated_OK) | responses$Response_Generated_OK %in% c(TRUE, FALSE)) &&
      all(nzchar(as.character(responses$Response_Failure_Reason))) &&
      all(nzchar(as.character(responses$Response_Failure_Class)))
    stim_cfg <- sanitize_stimulation_config(cfg$stimulation)
    pause_floor <- safe_num(stim_cfg$pause_duration_min_s, NA_real_)
    stimulus_pause_rows <- intervals[
      intervals$ISI_Label == "Pause" &
        intervals$Interval_Source == "stimulus_response" &
        is.finite(intervals$ISI_s) &
        intervals$ISI_s > 0,
      ,
      drop = FALSE
    ]
    stimulus_pause_duration_ok <- !is.finite(pause_floor) || nrow(stimulus_pause_rows) == 0 ||
      all(stimulus_pause_rows$ISI_s >= pause_floor - 1e-9)
    window_ok <- nrow(responses) == 0 || all(
      is.finite(responses$Response_Window_Start_s) &
        is.finite(responses$Response_Window_End_s) &
        responses$Response_Window_End_s >= responses$Response_Window_Start_s &
        responses$Generated_Response_End_s <= responses$Response_Window_End_s + 1e-8,
      na.rm = TRUE
    )
    onset_fields_ok <- nrow(intervals) == 0 ||
      all(c("Stimulus_Onset_s", "Time_From_Stimulus_Onset_s", "Contains_Stimulus_Onset") %in% names(intervals))
    min_refractory <- safe_num(cfg$inter_event_gap, 0)
    refractory_ok <- nrow(intervals) == 0 || !is.finite(min_refractory) || min_refractory <= 0 ||
      all(intervals$ISI_s[is.finite(intervals$ISI_s) & intervals$ISI_s > 0] >= min_refractory - 1e-9)
    phen <- stimulation_preset_phenomenology(preset, stimuli, responses)
    quant <- stimulation_preset_quantitative_metrics(preset, stimuli, responses, event_epochs)
    quantitative_trend_ok <- if (is.na(quant$trend_ok)) NA else isTRUE(quant$trend_ok)
    balanced_state_underpowered <- identical(preset, "state_dependent_balanced") &&
      identical(as.character(quant$status), "insufficient_state_coverage")
    quantitative_pass_ok <- if (isTRUE(balanced_state_underpowered)) {
      FALSE
    } else {
      is.na(quantitative_trend_ok) || isTRUE(quantitative_trend_ok)
    }
    failure_class <- if ("Response_Failure_Class" %in% names(responses)) {
      as.character(responses$Response_Failure_Class)
    } else if (nrow(responses) > 0) {
      mapply(
        classify_response_failure,
        responses$Response_Failure_Reason,
        generated_ok = responses$Response_Generated_OK %in% TRUE,
        plan_feasible = responses$Response_Plan_Feasible %in% TRUE,
        rolled_back = responses$Response_Rolled_Back %in% TRUE,
        window_limited = responses$Window_Limited %in% TRUE,
        USE.NAMES = FALSE
      )
    } else {
      character(0)
    }
    pass <- is.null(sim$error) && interval_spike_ok && episode_contiguity_ok && noisy_clean_ok &&
      latency_label_ok && response_count_ok && response_columns_ok && response_failure_audit_ok &&
      interburst_gap_label_ok && stimulus_spanning_gap_label_ok &&
      stimulus_pause_duration_ok && window_ok && onset_fields_ok && refractory_ok && phen$ok &&
      quantitative_pass_ok
    rows[[i]] <- data.frame(
      Preset = preset,
      Validation_Role = stimulation_validation_role(preset),
      Seed = as.integer(seed) + i - 1L,
      N_Spikes = if (!is.null(sim$spikes)) nrow(sim$spikes) else 0L,
      N_Intervals = nrow(intervals),
      N_Episodes = nrow(episodes),
      N_Stimuli = nrow(stimuli),
      N_Responses = nrow(responses),
      Interval_Spike_Consistency_OK = interval_spike_ok,
      Episode_Contiguity_OK = episode_contiguity_ok,
      Clean_Label_OK = noisy_clean_ok,
      Response_Latency_Not_Pattern_OK = latency_label_ok,
      Interburst_Gap_Not_Latency_OK = interburst_gap_label_ok,
      Stimulus_Spanning_Gap_Not_Latency_OK = stimulus_spanning_gap_label_ok,
      Response_Count_OK = response_count_ok,
      Response_Columns_OK = response_columns_ok,
      Response_Failure_Audit_OK = response_failure_audit_ok,
      Stimulus_Pause_Duration_OK = stimulus_pause_duration_ok,
      Response_Window_OK = window_ok,
      Stimulus_Onset_Metadata_OK = onset_fields_ok,
      Absolute_Refractory_OK = refractory_ok,
      Phenomenology_OK = phen$ok,
      Quantitative_Effect_Metric = quant$metric,
      Quantitative_Effect_Size = quant$effect_size,
      Quantitative_Slope = quant$slope,
      Quantitative_Correlation = quant$correlation,
      Quantitative_Status = quant$status,
      Quantitative_Trend_OK = quantitative_trend_ok,
      N_Preflight_Infeasible = sum(failure_class == "preflight_infeasible", na.rm = TRUE),
      N_Commit_Rollback = sum(responses$Response_Rolled_Back %in% TRUE, na.rm = TRUE),
      N_Optional_Component_Failed = sum(failure_class == "optional_component_failed", na.rm = TRUE),
      N_Probabilistic_No_Response = sum(failure_class == "probabilistic_no_response", na.rm = TRUE),
      N_Window_Limited_Failure = sum(failure_class == "window_limited", na.rm = TRUE),
      N_Refractory_Limited_Failure = sum(failure_class == "refractory_limited", na.rm = TRUE),
      N_Other_No_Response = sum(failure_class == "other_no_response", na.rm = TRUE),
      Pass = pass,
      Diagnostics = paste(c(if (!is.null(sim$error)) sim$error else character(0),
                            if (!is.null(quality)) quality$Benchmark_Quality_Note else character(0),
                            phen$note,
                            if (isTRUE(balanced_state_underpowered)) "balanced_state_dependent_underpowered" else character(0),
                            quant$note), collapse = " | "),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}


validation_logical_rate <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  vals <- as.logical(x)
  if (all(is.na(vals))) return(NA_real_)
  mean(vals, na.rm = TRUE)
}

validation_numeric_sum <- function(x) {
  if (is.null(x) || length(x) == 0) return(0)
  vals <- suppressWarnings(as.numeric(x))
  sum(vals, na.rm = TRUE)
}

validation_numeric_mean <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  vals <- suppressWarnings(as.numeric(x))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

validation_ci95 <- function(x) {
  vals <- suppressWarnings(as.numeric(x))
  vals <- vals[is.finite(vals)]
  n <- length(vals)
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  if (n == 1) return(c(lower = NA_real_, upper = NA_real_))
  m <- mean(vals)
  se <- stats::sd(vals) / sqrt(n)
  c(lower = m - 1.96 * se, upper = m + 1.96 * se)
}

stimulation_validation_role <- function(preset) {
  preset <- as.character(value_or(preset, "custom"))[1]
  if (identical(preset, "state_dependent")) return("default_behavior_check")
  if (identical(preset, "state_dependent_balanced")) return("balanced_quantitative_validation")
  "preset_quantitative_validation"
}

run_stimulation_validation_replicates <- function(config, seeds = 1:100) {
  seeds <- unique(as.integer(seeds))
  seeds <- seeds[is.finite(seeds)]
  if (length(seeds) == 0) seeds <- 1L

  raw_list <- lapply(seeds, function(seed) {
    tab <- run_stimulation_validation_suite(config, seed = seed)
    if (is.null(tab) || nrow(tab) == 0) {
      tab <- data.frame(
        Preset = NA_character_,
        Seed = as.integer(seed),
        Pass = FALSE,
        Quantitative_Effect_Metric = "not_available",
        Quantitative_Effect_Size = NA_real_,
        Quantitative_Trend_OK = NA,
        Quantitative_Status = "no_rows",
        Diagnostics = "no stimulation validation rows were produced",
        stringsAsFactors = FALSE
      )
    }
    tab$Replicate_Seed <- as.integer(seed)
    tab
  })
  raw <- do.call(rbind, raw_list)
  if (is.null(raw) || nrow(raw) == 0) {
    return(list(raw = data.frame(), summary = data.frame()))
  }

  required_cols <- list(
    Validation_Role = "not_available",
    Quantitative_Effect_Metric = "not_available",
    Quantitative_Status = "not_available",
    Quantitative_Effect_Size = NA_real_,
    Quantitative_Trend_OK = NA,
    Pass = FALSE,
    Phenomenology_OK = NA,
    N_Preflight_Infeasible = 0,
    N_Commit_Rollback = 0,
    N_Optional_Component_Failed = 0,
    N_Probabilistic_No_Response = 0,
    N_Window_Limited_Failure = 0,
    N_Refractory_Limited_Failure = 0,
    N_Other_No_Response = 0
  )
  for (nm in names(required_cols)) {
    if (!nm %in% names(raw)) raw[[nm]] <- required_cols[[nm]]
  }

  groups <- split(raw, interaction(raw$Preset, raw$Quantitative_Effect_Metric, drop = TRUE))
  summary <- do.call(rbind, lapply(groups, function(g) {
    effect <- suppressWarnings(as.numeric(g$Quantitative_Effect_Size))
    effect <- effect[is.finite(effect)]
    n_effect <- length(effect)
    ci <- validation_ci95(effect)
    trend_ok <- as.logical(g$Quantitative_Trend_OK)
    trend_evaluated <- !is.na(trend_ok)
    insufficient <- as.character(g$Quantitative_Status) == "insufficient_state_coverage"
    data.frame(
      Preset = as.character(g$Preset[1]),
      Validation_Role = as.character(g$Validation_Role[1]),
      Metric = as.character(g$Quantitative_Effect_Metric[1]),
      N_Seeds = length(unique(g$Replicate_Seed)),
      N_Replicates = nrow(g),
      N_Effect_Size = n_effect,
      Mean_Effect_Size = if (n_effect > 0) mean(effect) else NA_real_,
      SD_Effect_Size = if (n_effect > 1) stats::sd(effect) else NA_real_,
      CI95_Lower = ci[["lower"]],
      CI95_Upper = ci[["upper"]],
      Pass_Rate = validation_logical_rate(g$Pass),
      Phenomenology_Pass_Rate = validation_logical_rate(g$Phenomenology_OK),
      Trend_Pass_Rate = if (any(trend_evaluated)) mean(trend_ok[trend_evaluated], na.rm = TRUE) else NA_real_,
      Trend_Evaluated_Rate = mean(trend_evaluated, na.rm = TRUE),
      Insufficient_Coverage_Rate = mean(insufficient, na.rm = TRUE),
      Mean_Preflight_Infeasible = validation_numeric_mean(g$N_Preflight_Infeasible),
      Mean_Commit_Rollback = validation_numeric_mean(g$N_Commit_Rollback),
      Mean_Optional_Component_Failed = validation_numeric_mean(g$N_Optional_Component_Failed),
      Mean_Probabilistic_No_Response = validation_numeric_mean(g$N_Probabilistic_No_Response),
      Mean_Window_Limited_Failure = validation_numeric_mean(g$N_Window_Limited_Failure),
      Mean_Refractory_Limited_Failure = validation_numeric_mean(g$N_Refractory_Limited_Failure),
      Mean_Other_No_Response = validation_numeric_mean(g$N_Other_No_Response),
      Total_Preflight_Infeasible = validation_numeric_sum(g$N_Preflight_Infeasible),
      Total_Commit_Rollback = validation_numeric_sum(g$N_Commit_Rollback),
      Total_Optional_Component_Failed = validation_numeric_sum(g$N_Optional_Component_Failed),
      Total_Probabilistic_No_Response = validation_numeric_sum(g$N_Probabilistic_No_Response),
      Total_Window_Limited_Failure = validation_numeric_sum(g$N_Window_Limited_Failure),
      Total_Refractory_Limited_Failure = validation_numeric_sum(g$N_Refractory_Limited_Failure),
      Total_Other_No_Response = validation_numeric_sum(g$N_Other_No_Response),
      stringsAsFactors = FALSE
    )
  }))
  preset_order <- c(
    "intensity_response",
    "repeated_adaptation",
    "stimulus_suppression",
    "biphasic_burst_pause",
    "paired_pulse_recovery",
    "oddball_adaptation",
    "state_dependent",
    "state_dependent_balanced",
    "feature_tuning"
  )
  summary <- summary[order(match(summary$Preset, preset_order), summary$Metric), , drop = FALSE]
  rownames(summary) <- NULL
  list(raw = raw, summary = summary)
}

run_full_validation_suite <- function(config, seed_base = 1L, seed_count = 8L, n_intervals = 20L) {
  seed_count <- max(1L, as.integer(seed_count))
  n_intervals <- max(4L, as.integer(n_intervals))
  seeds <- seq.int(as.integer(seed_base), length.out = seed_count)
  stimulation_reps <- run_stimulation_validation_replicates(config, seeds = seeds + 1000L)
  list(
    invariants = run_validation_block("invariants", run_simulator_invariant_suite(config, seed = seed_base)),
    distribution = run_validation_block("distribution", run_distribution_validation(config, seeds = seeds, n_intervals = n_intervals)),
    temporal = run_validation_block("temporal", run_temporal_dependence_validation(config, seeds = seeds, n_intervals = max(8L, n_intervals))),
    stimulation = run_validation_block("stimulation", stimulation_reps$summary),
    stimulation_raw = run_validation_block("stimulation_raw", stimulation_reps$raw),
    detection = run_validation_block("detection", run_detection_benchmark_suite(config, seeds = seeds)),
    baselines = run_validation_block("baselines", run_baseline_comparison_suite(config, seeds = seeds, difficulty = "moderate"))
  )
}

validation_suite_to_long_table <- function(results) {
  rows <- list(); idx <- 0L
  for (name in names(results)) {
    tab <- results[[name]]
    if (is.null(tab) || nrow(tab) == 0) next
    tab <- as.data.frame(tab, stringsAsFactors = FALSE)
    tab$Validation_Block <- name
    idx <- idx + 1L
    rows[[idx]] <- tab
  }
  if (length(rows) == 0) return(data.frame())
  # Preserve all columns by row-binding through a superset schema.
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(df) {
    missing <- setdiff(all_cols, names(df))
    for (m in missing) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, rows)
}

APP_TEXT <- list(
  en = list(
    language = "Language",
    app_title = "Spike train simulator",
    app_subtitle = "Interval-label-first neural spike train generator with stimulus-response kernels, adaptation, validation suite, benchmark labels, heavy-tailed ISI distributions, and temporal dependence",
    run = "Run simulation",
    upload_dataset = "User dataset upload",
    use_uploaded_dataset = "Use uploaded spike train dataset",
    uploaded_dataset_file = "Spike train CSV/TSV file",
    uploaded_dataset_hint = "The first row must contain column names. Columns whose names contain \"event\" are treated as event/stimulus timestamps; all other numeric columns are spike trains. Empty cells are allowed. If no event columns are present, the file is still used for spike-train visualization and ISI analysis.",
    uploaded_time_unit = "Timestamp unit",
    uploaded_time_unit_s = "Seconds",
    uploaded_time_unit_ms = "Milliseconds",
    uploaded_auto_label = "Heuristically label uploaded ISIs using the current accepted ranges",
    uploaded_auto_label_hint = "Uploaded labels are analysis annotations, not simulator ground truth: high-frequency spiking, high-frequency tonic, Burst, Pause and Tonic are assigned heuristically from the current ranges, in that priority order; remaining intervals are labeled Noisy.",
    uploaded_missing_file = "Please choose a CSV/TSV file before loading an uploaded dataset.",
    uploaded_bad_file = "The uploaded dataset could not be read as a delimited table with a header row.",
    uploaded_no_spike_columns = "No spike-train columns were found. Use at least one non-event numeric column.",
    uploaded_no_spikes = "No valid spike timestamps were found in the uploaded spike-train columns.",
    general = "Run settings",
    reproduction = "Reproduction",
    reproduction_code = "Reproduction code",
    reproduction_code_hint = "Paste a reproduction code from another run, load it, then click Run simulation to regenerate the same dataset.",
    load_reproduction_code = "Load reproduction parameters",
    download_reproduction_code = "Download reproduction code",
    reproduction_loaded = "Reproduction parameters loaded. Click Run simulation to regenerate the dataset.",
    reproduction_match = "Loaded reproduction target matches the current result.",
    reproduction_mismatch = "Loaded reproduction target does not match the current result; check simulator version or rerun after loading the parameters.",
    err_reproduction_code = "Invalid reproduction code. Please paste the complete code beginning with STSR1.",
    pattern_mix = "Pattern mix",
    sequence = "Manual sequence",
    burst = "Burst",
    pause = "Pause",
    tonic = "Tonic",
    high_frequency_tonic = "High-frequency tonic",
    high_frequency_spiking = "High-frequency spiking",
    noisy = "Noisy",
    burst_settings = "Burst parameters",
    pause_settings = "Pause parameters",
    tonic_settings = "Tonic parameters",
    hft_settings = "High-frequency tonic parameters",
    hfs_settings = "High-frequency spiking parameters",
    noisy_settings = "Noisy parameters",
    seed = "Random seed",
    generation_key = "Generation key",
    generation_key_hint = "Save this key with the parameters. Pasting the same key and using the same settings regenerates the same spike train dataset.",
    train_count = "Spike trains",
    total_time = "Total time (s)",
    isi_xmax = "Distribution x-axis max (s)",
    inter_gap = "Absolute refractory period (s)",
    auto_inter_gap = "Auto from the smallest structured-state ISI",
    auto_inter_gap_hint = "When enabled, the absolute refractory period follows the smallest lower bound among Burst, high-frequency tonic and high-frequency spiking. Turn it off to enter a custom value.",
    selected_trains = "Displayed spike trains (max 10)",
    distribution_train = "Distribution spike train",
    distribution_train_a = "Distribution spike train A",
    distribution_train_b = "Distribution spike train B",
    distribution_scope_note = "View up to two generated spike trains side by side. The PDF export includes all generated spike trains.",
    sidebar_toggle = "Collapse parameter panel",
    sidebar_expand = "Show parameter panel",
    spike_window = "Visible spike train duration / resolution (s)",
    spike_window_hint = "Set how many seconds are visible at once. The full spike train is loaded above; use the horizontal scrollbar under the raster to browse without redrawing.",
    spike_resolution_hint = "Display resolution: %.3g ms/pixel; smallest spike-generating ISI setting: %.3g ms.",
    spike_resolution_ok = "The current visible duration can visually resolve the smallest configured spike ISI.",
    spike_resolution_warning = "The current visible duration cannot visually resolve the smallest configured spike ISI; nearby spike ticks may overlap visually. Use a visible duration <= %.3g s for full visual separation.",
    fit_resolution_window = "Use resolvable duration",
    spike_single_title = "Spike at %.6g s.",
    spike_cluster_title = "%d nearby spikes may overlap at display resolution (%.6g-%.6g s). Shorten the visible duration to separate them.",
    pattern_sequence = "Pattern sequence",
    sequence_hint = "Use b/n/t/hft/hfs to specify the number of boundary spikes in a labeled ISI run; hft denotes high-frequency tonic and hfs denotes sustained high-frequency spiking. Internally these tokens are expanded into ISI labels first. p specifies Pause durations and n means Noisy. p3 means three sampled Pause durations; p1s means one fixed 1-second Pause duration. Adjacent label runs share boundary spikes after the first real spike. Examples: p1sn2t4hft15hfs40, p1.0sb5hft12p0.8st6.",
    leading_silence_initial_pause = "Treat initial Pause as leading silence (recommended default)",
    leading_silence_hint = "Recommended for realistic recording windows: an initial Pause is treated as latency from the recording boundary to the first real spike, not as an ISI. Disable only when the first Pause duration should be a true ISI after an automatically generated positive first-spike latency.",
    initial_latency_model = "Initial latency model",
    initial_latency_model_hint = "Controls the positive first-spike latency when the first real spike is not already created by leading silence. Residual-life is the biologically preferred recording-window default.",
    latency_residual_life = "Residual-life / equilibrium renewal",
    latency_same_distribution = "Same distribution as first ISI label",
    latency_uniform = "Uniform within remaining window",
    generation_mode = "Ratio interpretation",
    mode_event = "Interval-run probability",
    mode_time = "Approximate time occupancy",
    ratio_hint = "When no manual sequence is supplied, the six pattern ratios control the automatic selection of the next ISI-label run and are normalized automatically.",
    avoid_noisy = "Enforce clean Noisy label rules",
    noisy_mm_ratio = "Noisy adjacency MM threshold",
    noisy_mm_hint = "Contextual clean-label rule: Noisy ISIs are bounded from the absolute refractory period to the Tonic upper scale and kept away from the Pause lower bound. A singleton Noisy ISI may enter the Burst/Tonic range only when isolated; two consecutive Noisy ISIs cannot occupy the same Burst/Tonic-like zone; adjacent Noisy/non-Noisy ISIs still satisfy the MM ratio.",
    ratio = "Ratio (%)",
    dist = "Distribution",
    dist_exponential = "Exponential",
    dist_gamma = "Gamma",
    dist_normal = "Normal",
    dist_uniform = "Uniform",
    dist_lognormal = "Lognormal",
    dist_invgauss = "Inverse Gaussian",
    mean_isi = "Mean ISI (s)",
    mean_pause = "Mean pause duration (s)",
    meanlog = "Log mean",
    sdlog = "Log standard deviation",
    invgauss_mean = "IG mean (s)",
    invgauss_shape = "IG shape lambda",
    shape = "Shape k",
    scale = "Scale theta (s)",
    sd = "Standard deviation (s)",
    isi_rho = "ISI serial correlation rho",
    isi_trend = "ISI trend log-slope",
    isi_temporal_hint = "rho controls within-run persistence (positive) or alternation (negative). Trend controls directed change: positive lengthens ISIs across the run, negative shortens them. Both default to 0.",
    tonic_cv_range = "Accepted tonic CV range",
    tonic_cv2_range = "Accepted tonic mean CV2 range",
    tonic_lv_range = "Accepted tonic LV range",
    hft_cv_range = "Accepted HF-tonic CV range",
    hft_cv2_range = "Accepted HF-tonic mean CV2 range",
    hft_lv_range = "Accepted HF-tonic LV range",
    hft_mm_range = "Accepted HF-tonic maximum/mean range",
    hfs_short_isi_range = "HF-spiking short-ISI band (s)",
    hfs_bridge_isi_range = "HF-spiking tolerated moderate-ISI band (s)",
    hfs_target_short_fraction = "Target short-ISI fraction",
    hfs_short_fraction_min = "Minimum short-ISI fraction",
    hfs_bridge_fraction_max = "Maximum moderate-ISI fraction",
    hfs_max_consecutive_bridge = "Maximum consecutive moderate ISIs",
    hfs_min_duration = "Minimum HF-spiking duration (s)",
    min_isi = "Minimum ISI (s)",
    max_isi = "Maximum ISI (s)",
    min_pause = "Minimum pause duration (s)",
    max_pause = "Maximum pause duration (s)",
    spike_range = "Spike count range",
    accepted_isi = "Accepted ISI range (s)",
    accepted_pause = "Accepted pause duration range (s)",
    spike_color = "Spike color",
    line_color = "Line color",
    pause_interval_color = "Pause interval color",
    pause_line_color = "Pause duration line color",
    tab_summary = "Summary",
    tab_spike = "Spike train view",
    tab_dist = "Distributions",
    tab_spike_data = "Spike data",
    tab_episode_data = "Episode data",
    theory_heading = "Effective target curves",
    empirical_heading = "Empirical intervals",
    interval_table_heading = "Interval table",
    show_target_overlay = "Show target density overlay",
    download_distributions = "Download all ISI distributions PDF",
    download_plots = "Download PDF",
    download_spikes = "Download latent spike audit CSV",
    download_latent_detector_input = "Download detector-visible latent input CSV",
    download_observed_audit = "Download observed spike audit CSV",
    download_observed_detector_input = "Download detector-visible observed input CSV",
    download_spike_matrix = "Download spike matrix CSV",
    download_per_train_csv_zip = "Download per-train CSV ZIP",
    download_spike_details = "Download interval table CSV",
    download_episodes = "Download episodes CSV",
    spike_matrix_note = "Spike events are real action-potential timestamps with left/right interval context. The wide spike matrix is available as one CSV; the per-train ZIP writes one CSV file per spike train and also includes summary plus self-contained reproduction code. Pattern labels live in the interval table.",
    ratio_interpretation = "Ratio interpretation",
    normalized_ratios = "Normalized ratio settings",
    pause_model = "Patterns are represented primarily as ISI/interval labels and episode states. Spike rows are real events; a spike may be the shared boundary between the left interval label and the right interval label. Pause is a long ISI label only when both boundaries are real spikes. By default, an initial p1s is leading latency from the recording boundary to the first spike. If leading-silence mode is disabled, the first spike is still generated at t > 0 and p1s becomes a true Pause ISI between the first and second real spikes.",
    model_scope = "Model scope: descriptive interval-label-first spike-train simulator with optional phenomenological stimulus-response kernels; ISI labels are generated first and accumulated into spike times. Heavy-tailed interval distributions and optional within-run temporal dependence are phenomenological timing controls. The recording boundary at t = 0 is not used as a spike. It is not a conductance-based, membrane-potential, synaptic-network, or other biophysical dynamics model.",
    reproducibility = "Reproducibility",
    reproducibility_seed = "Generation key",
    derived_rng_seed = "Derived RNG seed",
    verification_code = "Result verification code",
    verification_hash = "Full SHA-256 hash",
    verification_hint = "Generation key regenerates the dataset; verification code only checks whether the regenerated result is identical.",
    reproduction_code_label = "Self-contained reproduction code",
    reproduction_code_summary = "Share this code so another user can load all parameters and regenerate the same spike train dataset.",
    generated_train_count = "Generated spike trains",
    current_train_count = "Current spike-train count control",
    duration_check = "Requested vs achieved duration",
    duration_shortfall_warning = "At least one spike train ended before the requested duration. Inspect diagnostics and relax the interval, count, or regularity constraints if full-length trains are required.",
    stale_train_count = "The spike-train count control no longer matches the last generated simulation. Click Run simulation before viewing or downloading this result.",
    stale_train_count_download = "The current spike-train count control differs from the last generated simulation. Click Run simulation again before downloading.",
    target_actual = "Approximate time occupancy: target vs actual",
    diagnostics = "Simulation diagnostics",
    no_episode = "This spike train has no episodes; please adjust parameters and run again.",
    no_theory = "No valid theoretical distributions are available for the selected spike train.",
    no_empirical = "No empirical intervals are available for the selected spike train within the selected x-axis range.",
    empty_spike = "No spike data available. Pattern labels are stored in the interval table.",
    empty_episode = "No episode data available; please adjust parameters.",
    err_total_time = "Total Time must be a positive finite number.",
    err_gap = "Absolute refractory period must be a non-negative finite number.",
    err_train_count = "Number of Spike Trains must be at least 1.",
    err_tonic_regularity = "Tonic CV/CV2/LV ranges must be finite non-negative ranges.",
    err_noisy_mm = "Noisy adjacency MM threshold must be a finite value greater than 1.",
    x_time = "Time (s)",
    x_interval = "Labeled inter-spike interval (s)",
    y_density = "Truncated probability density",
    y_emp_density = "Empirical density",
    legend_spike = "Spike pattern",
    legend_silent = "Silent interval",
    legend_pattern = "Pattern",
    plot_spike_title = "Spike train raster view",
    plot_theory_title = "Effective labeled-ISI density",
    plot_empirical_title = "Simulated intervals with target density overlay",
    plot_empirical_no_overlay_title = "Simulated interval density"
  ),
  zh = list(
    language = "语言",
    app_title = "Spike train模拟生成器",
    app_subtitle = "由 ISI label 序列驱动，支持刺激响应、重复刺激适应、验证与基准测试、重尾分布和时序依赖的神经元 spike train 生成器",
    run = "运行模拟",
    upload_dataset = "用户数据上传",
    use_uploaded_dataset = "使用上传的 spike train 数据集",
    uploaded_dataset_file = "Spike train CSV/TSV 文件",
    uploaded_dataset_hint = "第一行必须是列名。列名中包含“event”的列会被解释为事件/刺激时间戳；其他数值列会被解释为 spike train。允许空单元格。即使没有 event 列，也会用于 spike train 可视化和 ISI 分析。",
    uploaded_time_unit = "时间戳单位",
    uploaded_time_unit_s = "秒",
    uploaded_time_unit_ms = "毫秒",
    uploaded_auto_label = "按当前接受范围为上传 ISI 启发式打标签",
    uploaded_auto_label_hint = "上传数据的标签只是分析注释，不是模拟器 ground truth：按高频持续放电、高频强直放电、Burst、Pause、Tonic 的优先顺序使用当前范围进行启发式标注，其余 interval 标为 Noisy。",
    uploaded_missing_file = "请先选择 CSV/TSV 文件，再加载上传数据集。",
    uploaded_bad_file = "无法把上传文件读取为带表头的分隔符表格。",
    uploaded_no_spike_columns = "没有找到 spike train 列。请至少保留一个非 event 的数值列。",
    uploaded_no_spikes = "上传的 spike train 列中没有找到有效 spike 时间戳。",
    general = "运行设置",
    reproduction = "复现",
    reproduction_code = "复现码",
    reproduction_code_hint = "粘贴其他运行结果中的复现码，加载后点击“运行模拟”，即可重新生成同一批数据。",
    load_reproduction_code = "加载复现参数",
    download_reproduction_code = "下载复现码",
    reproduction_loaded = "复现参数已加载。请点击“运行模拟”重新生成数据。",
    reproduction_match = "已加载复现目标与当前结果一致。",
    reproduction_mismatch = "已加载复现目标与当前结果不一致；请检查模拟器版本，或加载参数后重新运行。",
    err_reproduction_code = "复现码无效。请粘贴完整的 STSR1 开头复现码。",
    pattern_mix = "模式比例",
    sequence = "手动序列",
    burst = "爆发",
    pause = "暂停",
    tonic = "节律",
    high_frequency_tonic = "高频强直放电",
    high_frequency_spiking = "高频持续放电",
    noisy = "噪声",
    burst_settings = "爆发参数",
    pause_settings = "暂停参数",
    tonic_settings = "节律参数",
    hft_settings = "高频强直放电参数",
    hfs_settings = "高频持续放电参数",
    noisy_settings = "噪声参数",
    seed = "随机种子",
    generation_key = "生成密钥",
    generation_key_hint = "请把这个密钥与参数一起保存。粘贴同一个密钥并使用同一组设置，可重新生成同一批 spike train 数据。",
    train_count = "Spike train 数量",
    total_time = "总时长 (s)",
    isi_xmax = "分布图 x 轴上限 (s)",
    inter_gap = "绝对不应期 (s)",
    auto_inter_gap = "自动使用结构化模式中的最小 ISI",
    auto_inter_gap_hint = "开启后，绝对不应期会跟随 Burst、高频强直放电和高频持续放电接受范围中最小的下限；关闭后可手动自定义。",
    selected_trains = "显示 Spike train（最多 10 条）",
    distribution_train = "分布分析 Spike train",
    distribution_train_a = "分布分析 Spike train A",
    distribution_train_b = "分布分析 Spike train B",
    distribution_scope_note = "页面可并排查看最多两条已生成 spike train；PDF 会一次性导出全部 spike train。",
    sidebar_toggle = "折叠参数栏",
    sidebar_expand = "展开参数栏",
    spike_window = "Spike train 可见时长 / 分辨率 (s)",
    spike_window_hint = "设置当前视野一次显示多少秒。上方会一次性加载完整 spike train；使用光栅图下方的横向滚动条即可浏览，不需要刷新 plot。",
    spike_resolution_hint = "显示分辨率：%.3g ms/像素；当前最小 spike-generating ISI 设置：%.3g ms。",
    spike_resolution_ok = "当前可见时长可以在显示尺度上分辨最小设置 ISI。",
    spike_resolution_warning = "当前可见时长无法在显示尺度上分辨最小设置 ISI；相邻 spike tick 可能在视觉上重叠。若要完全视觉分离，可见时长应 <= %.3g s。",
    fit_resolution_window = "使用可分辨时长",
    spike_single_title = "%.6g s 处的 spike。",
    spike_cluster_title = "%d 个相邻 spike 在当前显示分辨率下可能重叠（%.6g-%.6g s）。缩短可见时长可将其分开。",
    pattern_sequence = "模式序列",
    sequence_hint = "b/n/t/hft/hfs 表示一个 ISI 标签 run 中的边界 spike 数；hft 表示高频强直放电，hfs 表示高频持续放电。算法内部会先展开成 ISI label 序列。p 表示 Pause 时长，n 表示噪声。p3 表示 3 个随机 Pause 时长；p1s 表示 1 个固定 1 秒 Pause 时长。首个真实 spike 之后，相邻 label run 共享边界 spike。示例：p1sn2t4hft15hfs40、p1.0sb5hft12p0.8st6。",
    leading_silence_initial_pause = "将初始 Pause 视为 leading silence（推荐默认）",
    leading_silence_hint = "推荐用于真实记录窗口：初始 Pause 表示从记录边界到第一个真实 spike 的潜伏期，而不是 ISI。只有当首个 Pause 时长应作为正初始潜伏期之后的真实 ISI 时才关闭。",
    initial_latency_model = "初始潜伏期模型",
    initial_latency_model_hint = "控制首个真实 spike 的正潜伏期。Residual-life 是记录窗口随机切入时更合理的默认模型。",
    latency_residual_life = "残余寿命 / 平衡 renewal",
    latency_same_distribution = "使用首个 ISI label 的同一分布",
    latency_uniform = "在剩余窗口内均匀采样",
    generation_mode = "比例解释方式",
    mode_event = "ISI-label run 概率",
    mode_time = "近似时间占比",
    ratio_hint = "未填写手动序列时，六类比例控制下一个 ISI-label run 的自动选择，并会自动归一化。",
    avoid_noisy = "强制启用 Noisy 清洁标签规则",
    noisy_mm_ratio = "噪声邻近 MM 阈值",
    noisy_mm_hint = "上下文清洁标签规则：Noisy ISI 被限制在绝对不应期到 Tonic 上限附近，并与 Pause 最小 ISI 保持安全距离。单个 Noisy ISI 可以进入 Burst/Tonic 区间，但必须是孤立的；连续两个 Noisy ISI 不能处于同一个 Burst/Tonic-like 区间；相邻 Noisy/非 Noisy ISI 仍需满足 MM ratio。",
    ratio = "比例 (%)",
    dist = "分布",
    dist_exponential = "指数分布",
    dist_gamma = "Gamma 分布",
    dist_normal = "正态分布",
    dist_uniform = "均匀分布",
    dist_lognormal = "对数正态分布",
    dist_invgauss = "逆高斯分布",
    mean_isi = "平均 ISI (s)",
    mean_pause = "平均暂停时长 (s)",
    meanlog = "对数均值",
    sdlog = "对数标准差",
    invgauss_mean = "逆高斯均值 (s)",
    invgauss_shape = "逆高斯形状参数 lambda",
    shape = "形状参数 k",
    scale = "尺度参数 theta (s)",
    sd = "标准差 (s)",
    isi_rho = "ISI 序列相关 rho",
    isi_trend = "ISI 趋势 log-slope",
    isi_temporal_hint = "rho 控制同一 run 内相邻 ISI 的持续相关（正值）或交替相关（负值）。Trend 控制有方向的变化：正值表示 ISI 逐渐变长，负值表示逐渐变短。二者默认均为 0。",
    tonic_cv_range = "Tonic 接受 CV 范围",
    tonic_cv2_range = "Tonic 接受平均 CV2 范围",
    tonic_lv_range = "Tonic 接受 LV 范围",
    hft_cv_range = "高频强直放电接受 CV 范围",
    hft_cv2_range = "高频强直放电接受平均 CV2 范围",
    hft_lv_range = "高频强直放电接受 LV 范围",
    hft_mm_range = "高频强直放电接受最大值/均值范围",
    hfs_short_isi_range = "高频持续放电短 ISI 区间 (s)",
    hfs_bridge_isi_range = "高频持续放电可容许中等 ISI 区间 (s)",
    hfs_target_short_fraction = "目标短 ISI 比例",
    hfs_short_fraction_min = "最小短 ISI 比例",
    hfs_bridge_fraction_max = "最大中等 ISI 比例",
    hfs_max_consecutive_bridge = "最多连续中等 ISI 数",
    hfs_min_duration = "高频持续放电最短时长 (s)",
    min_isi = "最小 ISI (s)",
    max_isi = "最大 ISI (s)",
    min_pause = "最小暂停时长 (s)",
    max_pause = "最大暂停时长 (s)",
    spike_range = "Spike 个数范围",
    accepted_isi = "接受的 ISI 范围 (s)",
    accepted_pause = "接受的暂停时长范围 (s)",
    spike_color = "Spike 颜色",
    line_color = "曲线颜色",
    pause_interval_color = "暂停区间颜色",
    pause_line_color = "暂停时长曲线颜色",
    tab_summary = "参数摘要",
    tab_spike = "Spike train 视图",
    tab_dist = "ISI / 暂停分布",
    tab_spike_data = "Spike 数据",
    tab_episode_data = "Episode 数据",
    theory_heading = "有效目标分布曲线",
    empirical_heading = "生成数据的经验间隔",
    interval_table_heading = "Interval 表",
    show_target_overlay = "显示目标密度曲线叠加",
    download_distributions = "下载全部 ISI 分布 PDF",
    download_plots = "下载 PDF",
    download_spikes = "下载潜在 spike 审计 CSV",
    download_latent_detector_input = "下载 detector 可见潜在 spike 输入 CSV",
    download_observed_audit = "下载观测 spike 审计 CSV",
    download_observed_detector_input = "下载 detector 可见观测 spike 输入 CSV",
    download_spike_matrix = "下载 Spike 矩阵 CSV",
    download_per_train_csv_zip = "下载每条 train 独立 CSV ZIP",
    download_spike_details = "下载 Interval 表 CSV",
    download_episodes = "下载 Episode CSV",
    spike_matrix_note = "Spike 表显示真实动作电位时间及其左/右 interval 上下文。宽表 spike matrix 可作为一个 CSV 下载；每条 train 独立 CSV ZIP 会为每条 spike train 写一个单独 CSV 文件，并包含 summary 和自包含复现码。模式标签位于 interval 表中。",
    ratio_interpretation = "比例解释方式",
    normalized_ratios = "归一化比例设置",
    pause_model = "模式主要表示为 ISI/interval 标签和 episode 状态。Spike 行是真实事件；同一个 spike 可以是左侧 interval 标签和右侧 interval 标签的共享边界。只有左右边界都是真实 spike 时，Pause 才是长 ISI 标签。默认情况下，初始 p1s 表示从记录边界到第一个 spike 的 leading latency；如果关闭 leading-silence，第一个 spike 仍在 t > 0，p1s 表示第一个和第二个真实 spike 之间的真实 Pause ISI。",
    model_scope = "模型范围：这是描述性的 ISI-label-first spike train 模拟器，并提供可选的现象学刺激-响应 kernel；算法先生成 ISI 标签和时长，再累加得到 spike 时间。重尾 ISI 分布和可选的 run 内时序依赖属于现象学 timing 控制。t = 0 记录边界不会被当作 spike。它不是电导型、膜电位型、突触网络型或其他生物物理动力学模型。",
    reproducibility = "可重复性",
    reproducibility_seed = "生成密钥",
    derived_rng_seed = "派生 RNG seed",
    verification_code = "结果验证码",
    verification_hash = "完整 SHA-256 哈希",
    verification_hint = "生成密钥用于重新生成数据；结果验证码只用于核对重新生成后的结果是否完全一致。",
    reproduction_code_label = "自包含复现码",
    reproduction_code_summary = "把这段复现码分享给其他用户，对方即可加载全部参数并重新生成同一批 spike train 数据。",
    generated_train_count = "本次已生成 spike train 数",
    current_train_count = "当前 Spike train 数量控件值",
    duration_check = "请求时长与实际达成时长",
    duration_shortfall_warning = "至少有一条 spike train 未达到请求时长。如果必须生成完整时长，请检查诊断信息，并放宽 ISI、spike 数或规则性约束。",
    stale_train_count = "当前 Spike train 数量控件与最后一次模拟结果不一致。请先点击“运行模拟”，再查看或下载结果。",
    stale_train_count_download = "当前 Spike train 数量控件与最后一次模拟结果不一致。请先点击“运行模拟”，再下载结果。",
    target_actual = "近似时间占比：目标与实际",
    diagnostics = "模拟诊断",
    no_episode = "这条 spike train 没有生成 episode；请调整参数后重新运行。",
    no_theory = "所选 spike train 没有可显示的有效理论分布。",
    no_empirical = "所选 spike train 在当前 x 轴范围内没有可显示的经验间隔。",
    empty_spike = "没有 spike 数据。模式标签记录在 interval 表中。",
    empty_episode = "没有 episode 数据；请调整参数。",
    err_total_time = "总时长必须是正的有限数。",
    err_gap = "绝对不应期必须是非负有限数。",
    err_train_count = "Spike train 数量至少为 1。",
    err_tonic_regularity = "Tonic 的 CV/CV2/LV 范围必须是非负有限区间。",
    err_noisy_mm = "噪声邻近 MM 阈值必须是大于 1 的有限数。",
    x_time = "时间 (s)",
    x_interval = "带标签的 spike 间隔 ISI (s)",
    y_density = "截断概率密度",
    y_emp_density = "经验密度",
    legend_spike = "Spike 模式",
    legend_silent = "静默区间",
    legend_pattern = "模式",
    plot_spike_title = "Spike train 光栅视图",
    plot_theory_title = "有效带标签 ISI 密度",
    plot_empirical_title = "模拟间隔与目标密度叠加",
    plot_empirical_no_overlay_title = "模拟间隔经验密度"
  )
)

tr <- function(lang, key) {
  lang <- if (!is.null(lang) && lang %in% names(APP_TEXT)) lang else "en"
  value_or(APP_TEXT[[lang]][[key]], value_or(APP_TEXT$en[[key]], key))
}

pattern_labels <- function(lang) {
  c(
    "Burst" = tr(lang, "burst"),
    "Pause" = tr(lang, "pause"),
    "Tonic" = tr(lang, "tonic"),
    "high_frequency_tonic" = tr(lang, "high_frequency_tonic"),
    "high_frequency_spiking" = tr(lang, "high_frequency_spiking"),
    "Noisy" = tr(lang, "noisy")
  )
}

LATIN_PLOT_FONT <- "Times New Roman"
CJK_PLOT_FONT <- "Songti SC"

app_font_stack <- function() {
  "'Times New Roman', Times, 'Songti SC', 'PingFang SC', 'Hiragino Sans GB', 'Heiti SC', serif"
}

plot_font_family <- function(lang) {
  if (identical(lang, "zh")) CJK_PLOT_FONT else LATIN_PLOT_FONT
}

nature_plot_theme <- function(base_size = 10, base_family = LATIN_PLOT_FONT) {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1, hjust = 0, margin = margin(b = 8)),
      axis.title = element_text(face = "plain", size = base_size),
      axis.text = element_text(color = "#222222", size = base_size - 1),
      axis.line = element_line(color = "#222222", linewidth = 0.35),
      axis.ticks = element_line(color = "#222222", linewidth = 0.35),
      axis.ticks.length = unit(2, "mm"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(9, "mm"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 16, 16, 12)
    )
}

open_pdf_device <- function(file, width = 10, height = 5, family = LATIN_PLOT_FONT) {
  if (isTRUE(capabilities("aqua"))) {
    ok <- tryCatch({
      grDevices::quartz(file = file, type = "pdf", width = width, height = height, family = family)
      TRUE
    }, error = function(err) FALSE)
    if (isTRUE(ok)) return("quartz")
  }

  if (isTRUE(capabilities("cairo"))) {
    ok <- tryCatch({
      grDevices::cairo_pdf(filename = file, width = width, height = height, family = family)
      TRUE
    }, warning = function(warn) FALSE, error = function(err) FALSE)
    if (isTRUE(ok)) return("cairo")
  }

  grDevices::pdf(file = file, width = width, height = height, family = "Times", useDingbats = FALSE)
  "pdf"
}

# Spike train simulator current model note:
# The labeled ISI sequence is the generative primitive. Spike rows are real action-potential
# events derived from cumulative ISI durations. Pattern labels live in the interval table and
# episode summaries, not as intrinsic spike types. The recording boundary at t = 0 is never
# treated as a spike; the first real spike always has positive latency. The validation suite
# tests simulator invariants, distributional fidelity, temporal dependence, detection benchmarks,
# and Poisson/renewal/Markov baselines. Contextual Noisy rules are used to prevent Noisy intervals
# from forming ambiguous Burst-like or Tonic-like fragments near matching labeled episodes.

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#f6f8fb",
    fg = "#172033",
    primary = "#2563eb",
    secondary = "#64748b",
    success = "#0f766e",
    base_font = app_font_stack()
  ),
  tags$head(
    tags$title("Spike train simulator"),
    tags$style(HTML("
      body {
        background: #f6f8fb;
        font-family: 'Times New Roman', Times, 'Songti SC', 'PingFang SC', 'Hiragino Sans GB', 'Heiti SC', serif;
      }
      .app-shell {
        max-width: none;
        width: calc(100vw - 32px);
        margin: 0 auto;
        padding: 20px 16px 28px;
      }
      .app-header {
        align-items: center;
        display: grid;
        gap: 16px;
        grid-template-columns: minmax(340px, 460px) minmax(0, 1fr) minmax(150px, 300px);
        margin-bottom: 18px;
      }
      .app-header > .shiny-html-output {
        min-width: 0;
      }
      .top-navigation {
        align-items: center;
        display: flex;
        gap: 10px;
        min-width: 0;
        overflow: hidden;
      }
      .app-title {
        font-size: 28px;
        font-weight: 720;
        letter-spacing: 0;
        line-height: 1.15;
        margin: 0;
      }
      .app-subtitle {
        color: #64748b;
        font-size: 14px;
        margin: 5px 0 0;
      }
      .language-control {
        justify-self: end;
        min-width: 150px;
      }
      .language-control .form-group {
        margin-bottom: 0;
      }
      .language-control select {
        height: 36px;
        min-height: 36px;
      }
      .workspace-grid {
        align-items: start;
        display: grid;
        gap: 16px;
        grid-template-columns: minmax(340px, 460px) minmax(0, 1fr);
      }
      .workspace-grid.sidebar-collapsed {
        grid-template-columns: minmax(0, 1fr);
      }
      .workspace-grid.sidebar-collapsed .sidebar-shell {
        display: none;
      }
      .workspace-toolbar {
        align-items: center;
        display: flex;
        justify-content: flex-start;
        margin: 0 0 10px;
      }
      .sidebar-toggle {
        flex: 0 0 auto;
        display: inline-flex;
      }
      .sidebar-toggle .form-group,
      .sidebar-toggle .checkbox {
        margin: 0;
      }
      .sidebar-toggle .shiny-input-container {
        width: auto !important;
      }
      .sidebar-toggle input[type='checkbox'] {
        display: none;
      }
      .sidebar-toggle label {
        align-items: center;
        background: #f8fafc;
        border: 1px solid #cbd5e1;
        border-radius: 6px;
        color: #334155;
        cursor: pointer;
        display: inline-flex;
        font-size: 12px;
        font-weight: 700;
        gap: 6px;
        line-height: 1;
        margin: 0;
        height: 36px;
        min-height: 36px;
        padding: 0 10px;
        user-select: none;
      }
      .sidebar-toggle label:hover {
        background: #eef2ff;
        border-color: #93c5fd;
        color: #1d4ed8;
      }
      .sidebar-toggle label:before {
        content: '◀';
        font-size: 11px;
        margin-right: 6px;
      }
      .sidebar-toggle.sidebar-is-collapsed label:before {
        content: '▶';
      }
      .sidebar-shell {
        background: #ffffff;
        border: 1px solid #dbe4f0;
        border-radius: 8px;
        box-shadow: 0 10px 28px rgba(15, 23, 42, 0.06);
        max-height: calc(100vh - 124px);
        overflow-y: auto;
        padding: 14px;
      }
      .run-button {
        border-radius: 6px;
        font-weight: 680;
        margin-bottom: 12px;
        min-height: 42px;
        width: 100%;
      }
      .top-tab-nav {
        align-items: center;
        display: flex;
        flex: 1 1 auto;
        gap: 4px;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        scrollbar-width: thin;
        white-space: nowrap;
        width: 100%;
      }
      .top-tabs-output,
      .top-tabs-output .shiny-html-output {
        flex: 1 1 auto;
        min-width: 0;
        overflow: hidden;
        width: 100%;
      }
      .top-tab-link {
        align-items: center;
        background: transparent;
        border: 1px solid transparent;
        border-radius: 6px;
        color: #475569;
        cursor: pointer;
        display: inline-flex;
        flex: 0 0 auto;
        font-size: 13px;
        font-weight: 700;
        height: 36px;
        line-height: 1.1;
        min-height: 36px;
        padding: 0 11px;
      }
      .top-tab-link:hover {
        background: #f8fafc;
        border-color: #cbd5e1;
        color: #1d4ed8;
      }
      .top-tab-link.active {
        background: #eef2ff;
        border-color: #bfdbfe;
        color: #2563eb;
      }
      .control-group {
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        margin-bottom: 10px;
        overflow: hidden;
        background: #ffffff;
      }
      .control-group summary {
        background: #f8fafc;
        color: #172033;
        cursor: pointer;
        font-size: 14px;
        font-weight: 700;
        list-style: none;
        padding: 11px 12px;
      }
      .control-group summary::-webkit-details-marker {
        display: none;
      }
      .control-group summary:after {
        color: #64748b;
        content: '+';
        float: right;
        font-weight: 700;
      }
      .control-group[open] summary:after {
        content: '-';
      }
      .control-body {
        border-top: 1px solid #e2e8f0;
        padding: 12px;
      }
      .control-grid {
        display: grid;
        gap: 10px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .control-grid.one {
        grid-template-columns: 1fr;
      }
      .form-group {
        margin-bottom: 10px;
      }
      .form-group label {
        color: #334155;
        font-size: 12px;
        font-weight: 650;
        margin-bottom: 4px;
      }
      .help-block {
        color: #64748b;
        font-size: 12px;
        line-height: 1.35;
        margin: 4px 0 8px;
      }
      .stim-literature-card {
        background: #f8fafc;
        border: 1px solid #dbe4f0;
        border-radius: 8px;
        color: #334155;
        font-size: 12px;
        line-height: 1.45;
        margin: 8px 0 12px;
        padding: 10px;
      }
      .stim-literature-card strong {
        color: #172033;
      }
      .stim-literature-card ul {
        margin: 6px 0 0 18px;
        padding: 0;
      }
      .stim-literature-card a {
        color: #2563eb;
      }
      .main-surface {
        background: #ffffff;
        border: 1px solid #dbe4f0;
        border-radius: 8px;
        box-shadow: 0 10px 28px rgba(15, 23, 42, 0.06);
        padding: 14px;
      }
      .nav-tabs {
        border-bottom-color: #dbe4f0;
      }
      .nav-tabs .nav-link {
        color: #475569;
        font-weight: 650;
      }
      .nav-tabs .nav-link.active {
        color: #2563eb;
      }
      .tab-content {
        padding-top: 14px;
      }
      .section-heading {
        color: #172033;
        font-size: 15px;
        font-weight: 700;
        margin: 6px 0 10px;
      }
      .distribution-control-grid,
      .distribution-plot-grid {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .distribution-control-grid {
        align-items: end;
        max-width: 560px;
      }
      .distribution-plot-panel {
        min-width: 0;
      }
      .time-window-control {
        border-top: 1px solid #e2e8f0;
        margin: 12px 0 12px;
        padding: 10px 0 0;
      }
      .time-window-control .form-group {
        margin-bottom: 4px;
      }
      .spike-svg-wrap {
        height: 520px;
        overflow-x: scroll;
        overflow-y: hidden;
        width: 100%;
      }
      .spike-svg {
        display: block;
        height: 520px;
        min-width: 1600px;
        width: auto;
      }
      .resolution-note {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 6px;
        color: #475569;
        font-size: 12px;
        line-height: 1.4;
        margin: 0 0 12px;
        padding: 8px 10px;
      }
      .resolution-note.warn {
        background: #fff7ed;
        border-color: #fed7aa;
        color: #9a3412;
      }
      .resolution-note .btn {
        margin-top: 6px;
      }
      .table-filter-bar {
        align-items: end;
        border: 1px solid #e2e8f0;
        border-radius: 6px;
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        margin: 10px 0 12px;
        max-width: 680px;
        padding: 10px;
      }
      .table-filter-bar .form-group {
        flex: 1 1 260px;
        margin-bottom: 0;
      }
      .reproduction-code-box {
        background: #ffffff;
        border: 1px solid #cbd5e1;
        border-radius: 6px;
        color: #172033;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
        font-size: 11px;
        line-height: 1.35;
        min-height: 92px;
        overflow-wrap: anywhere;
        padding: 8px;
        resize: vertical;
        width: 100%;
      }
      table.table {
        font-size: 12px;
      }
      @media (max-width: 900px) {
        .app-shell {
          width: calc(100vw - 20px);
          padding-left: 10px;
          padding-right: 10px;
        }
        .app-header {
          align-items: stretch;
          grid-template-columns: 1fr;
        }
        .top-navigation {
          justify-content: flex-start;
          width: 100%;
        }
        .top-tab-nav {
          flex-wrap: wrap;
          overflow-x: visible;
        }
        .language-control {
          justify-self: stretch;
          width: 100%;
        }
        .sidebar-toggle {
          width: 100%;
        }
        .sidebar-toggle label {
          justify-content: center;
          width: 100%;
        }
        .sidebar-shell {
          max-height: none;
          margin-bottom: 14px;
        }
        .workspace-grid {
          grid-template-columns: 1fr;
        }
        .control-grid {
          grid-template-columns: 1fr;
        }
        .distribution-control-grid,
        .distribution-plot-grid {
          grid-template-columns: 1fr;
        }
      }
    ")),
    tags$script(HTML("
      function setRunButtonLanguage() {
        var lang = document.getElementById('language');
        var run = document.getElementById('run');
        if (!lang || !run) return;
        run.textContent = lang.value === 'zh' ? '运行模拟' : 'Run simulation';
      }
      function setSidebarToggleLanguage() {
        var lang = document.getElementById('language');
        var checkbox = document.getElementById('sidebar_collapsed');
        var labelText = document.querySelector('.sidebar-toggle label span');
        if (!labelText || !checkbox) return;
        var zh = !lang || lang.value === 'zh';
        labelText.textContent = checkbox.checked
          ? (zh ? '展开参数栏' : 'Show parameter panel')
          : (zh ? '折叠参数栏' : 'Collapse parameter panel');
      }
      function applySidebarState() {
        var checkbox = document.getElementById('sidebar_collapsed');
        var grid = document.getElementById('workspace_grid');
        var toggle = document.querySelector('.sidebar-toggle');
        if (!checkbox || !grid) return;
        grid.classList.toggle('sidebar-collapsed', checkbox.checked);
        if (toggle) toggle.classList.toggle('sidebar-is-collapsed', checkbox.checked);
        setSidebarToggleLanguage();
      }
      document.addEventListener('shiny:connected', function() {
        setRunButtonLanguage();
        applySidebarState();
      });
      document.addEventListener('change', function(event) {
        if (!event.target) return;
        if (event.target.id === 'language') {
          setRunButtonLanguage();
          setSidebarToggleLanguage();
        }
        if (event.target.id === 'sidebar_collapsed') applySidebarState();
      });
    "))
  ),
  div(
    class = "app-shell",
    div(
      class = "app-header",
      uiOutput("title_ui"),
      div(
        class = "top-navigation",
        div(
          class = "sidebar-toggle",
          checkboxInput("sidebar_collapsed", "折叠参数栏", value = FALSE, width = NULL)
        ),
        div(class = "top-tabs-output", uiOutput("top_tabs_ui"))
      ),
      div(
        class = "language-control",
        selectInput(
          "language",
          label = "语言/Language",
          choices = c("中文" = "zh", "English" = "en"),
          selected = "zh",
          selectize = FALSE
        )
      )
    ),
    div(
      id = "workspace_grid",
      class = "workspace-grid",
      div(
        class = "sidebar-shell",
        actionButton("run", "运行模拟", class = "btn-primary run-button"),
        uiOutput("controls_ui")
      ),
      div(class = "main-surface", uiOutput("tabs_ui"))
    )
  )
)

server <- function(input, output, session) {

  pattern_levels <- SPIKE_PATTERN_LEVELS
  empty_spike_df <- make_empty_spike_df
  empty_episode_df <- make_empty_episode_df
  loaded_reproduction_expected <- reactiveVal(NULL)
  loaded_reproduction_config <- reactiveVal(NULL)

  current_lang <- reactive(value_or(input$language, "zh"))

  input_value <- function(id, default) {
    value_or(isolate(input[[id]]), default)
  }

  normalize_generation_key <- function(value) {
    key <- value_or(value, "")
    key <- paste(as.character(key), collapse = "")
    key <- trimws(key)
    if (identical(key, "")) key <- "12345"
    key
  }

  derive_seed_from_key <- function(key) {
    key <- normalize_generation_key(key)
    rng_max <- 2147483646L

    if (grepl("^[0-9]+$", key)) {
      numeric_key <- suppressWarnings(as.numeric(key))
      if (is.finite(numeric_key) && numeric_key >= 1 && numeric_key <= rng_max &&
          numeric_key == floor(numeric_key)) {
        return(as.integer(numeric_key))
      }
    }

    if (!requireNamespace("digest", quietly = TRUE)) {
      fallback <- suppressWarnings(sum(as.integer(charToRaw(key))))
      return(as.integer(max(1L, fallback %% rng_max)))
    }

    hash <- digest::digest(paste0("Spike_Train_Simulator_generation_key|", key), algo = "sha256", serialize = FALSE)
    chunks <- substring(hash, seq(1, nchar(hash), by = 6), pmin(seq(6, nchar(hash), by = 6), nchar(hash)))
    seed <- 0
    for (chunk in chunks) {
      value <- strtoi(chunk, base = 16)
      if (is.finite(value)) seed <- (seed * 16777216 + value) %% rng_max
    }
    seed <- as.integer(seed)
    if (!is.finite(seed) || seed < 1L) seed <- 1L
    seed
  }

  reproduction_input_spec <- function() {
    list(
      language = list(type = "select", default = "zh"),
      generation_key = list(type = "text", default = "12345"),
      spike_train_number = list(type = "numeric", default = 1),
      total_time = list(type = "numeric", default = 25),
      isi_xmax = list(type = "numeric", default = 2),
      inter_event_gap = list(type = "numeric", default = burst_min_isi_value()),
	      auto_inter_event_gap = list(type = "logical", default = TRUE),
	      generation_mode = list(type = "select", default = "time"),
	      benchmark_task_mode = list(type = "select", default = "clean"),
		      ratio_burst = list(type = "numeric", default = 15),
      ratio_pause = list(type = "numeric", default = 15),
      ratio_tonic = list(type = "numeric", default = 20),
      ratio_hft = list(type = "numeric", default = 15),
      ratio_hfs = list(type = "numeric", default = 15),
      ratio_noisy = list(type = "numeric", default = 20),
      avoid_noisy_burst_runs = list(type = "logical", default = TRUE),
      pattern_sequence = list(type = "text", default = ""),
      leading_silence_initial_pause = list(type = "logical", default = TRUE),
      initial_latency_model = list(type = "select", default = "residual_life"),
      dist_burst = list(type = "select", default = "Gamma"),
      burst_exp_mean = list(type = "numeric", default = 0.024),
      burst_gamma_shape = list(type = "numeric", default = 2),
      burst_gamma_scale = list(type = "numeric", default = 0.012),
      burst_norm_mean = list(type = "numeric", default = 0.024),
      burst_norm_sd = list(type = "numeric", default = 0.008),
      burst_lognorm_meanlog = list(type = "numeric", default = log(0.024)),
      burst_lognorm_sdlog = list(type = "numeric", default = 0.35),
      burst_invgauss_mean = list(type = "numeric", default = 0.024),
      burst_invgauss_shape = list(type = "numeric", default = 0.25),
      burst_unif_min = list(type = "numeric", default = 0.006),
      burst_unif_max = list(type = "numeric", default = 0.045),
      spike_range_burst = list(type = "numeric_vector", default = c(3, 6)),
      interval_range_burst = list(type = "numeric_vector", default = c(0.006, 0.045)),
      burst_isi_rho = list(type = "numeric", default = 0),
      burst_isi_trend = list(type = "numeric", default = 0),
      col_burst_line = list(type = "color", default = NATURE_PATTERN_COLORS["Burst"]),
      dist_pause = list(type = "select", default = "Exponential"),
      pause_exp_mean = list(type = "numeric", default = 1),
      pause_gamma_shape = list(type = "numeric", default = 2),
      pause_gamma_scale = list(type = "numeric", default = 0.5),
      pause_norm_mean = list(type = "numeric", default = 1),
      pause_norm_sd = list(type = "numeric", default = 0.2),
      pause_lognorm_meanlog = list(type = "numeric", default = 0),
      pause_lognorm_sdlog = list(type = "numeric", default = 0.45),
      pause_invgauss_mean = list(type = "numeric", default = 1),
      pause_invgauss_shape = list(type = "numeric", default = 2),
      pause_unif_min = list(type = "numeric", default = 0.7),
      pause_unif_max = list(type = "numeric", default = 1.5),
      pause_duration_range = list(type = "numeric_vector", default = c(0.7, 1.5)),
      pause_isi_rho = list(type = "numeric", default = 0),
      pause_isi_trend = list(type = "numeric", default = 0),
      col_pause_line = list(type = "color", default = NATURE_PATTERN_COLORS["Pause"]),
      dist_tonic = list(type = "select", default = "Normal"),
      tonic_exp_mean = list(type = "numeric", default = 0.45),
      tonic_gamma_shape = list(type = "numeric", default = 30),
      tonic_gamma_scale = list(type = "numeric", default = 0.015),
      tonic_norm_mean = list(type = "numeric", default = 0.45),
      tonic_norm_sd = list(type = "numeric", default = 0.03),
      tonic_lognorm_meanlog = list(type = "numeric", default = log(0.45)),
      tonic_lognorm_sdlog = list(type = "numeric", default = 0.08),
      tonic_invgauss_mean = list(type = "numeric", default = 0.45),
      tonic_invgauss_shape = list(type = "numeric", default = 50),
      tonic_unif_min = list(type = "numeric", default = 0.38),
      tonic_unif_max = list(type = "numeric", default = 0.52),
      spike_range_tonic = list(type = "numeric_vector", default = c(4, 8)),
      interval_range_tonic = list(type = "numeric_vector", default = c(0.38, 0.52)),
      tonic_cv_range = list(type = "numeric_vector", default = c(0, 0.18)),
      tonic_cv2_range = list(type = "numeric_vector", default = c(0, 0.25)),
      tonic_lv_range = list(type = "numeric_vector", default = c(0, 0.06)),
      tonic_isi_rho = list(type = "numeric", default = 0),
      tonic_isi_trend = list(type = "numeric", default = 0),
      col_tonic_line = list(type = "color", default = NATURE_PATTERN_COLORS["Tonic"]),
      dist_hft = list(type = "select", default = "Normal"),
      hft_exp_mean = list(type = "numeric", default = 0.032),
      hft_gamma_shape = list(type = "numeric", default = 40),
      hft_gamma_scale = list(type = "numeric", default = 0.0008),
      hft_norm_mean = list(type = "numeric", default = 0.032),
      hft_norm_sd = list(type = "numeric", default = 0.003),
      hft_lognorm_meanlog = list(type = "numeric", default = log(0.032)),
      hft_lognorm_sdlog = list(type = "numeric", default = 0.10),
      hft_invgauss_mean = list(type = "numeric", default = 0.032),
      hft_invgauss_shape = list(type = "numeric", default = 0.5),
      hft_unif_min = list(type = "numeric", default = 0.026),
      hft_unif_max = list(type = "numeric", default = 0.038),
      spike_range_hft = list(type = "numeric_vector", default = c(8, 24)),
      interval_range_hft = list(type = "numeric_vector", default = c(0.024, 0.040)),
      hft_cv_range = list(type = "numeric_vector", default = c(0, 0.22)),
      hft_cv2_range = list(type = "numeric_vector", default = c(0, 0.28)),
      hft_lv_range = list(type = "numeric_vector", default = c(0, 0.22)),
      hft_mm_range = list(type = "numeric_vector", default = c(1, 1.25)),
      hft_isi_rho = list(type = "numeric", default = 0.20),
      hft_isi_trend = list(type = "numeric", default = 0),
      col_hft_line = list(type = "color", default = NATURE_PATTERN_COLORS["high_frequency_tonic"]),
      dist_hfs = list(type = "select", default = "Gamma"),
      hfs_exp_mean = list(type = "numeric", default = 0.008),
      hfs_gamma_shape = list(type = "numeric", default = 3),
      hfs_gamma_scale = list(type = "numeric", default = 0.0025),
      hfs_norm_mean = list(type = "numeric", default = 0.008),
      hfs_norm_sd = list(type = "numeric", default = 0.002),
      hfs_lognorm_meanlog = list(type = "numeric", default = log(0.008)),
      hfs_lognorm_sdlog = list(type = "numeric", default = 0.20),
      hfs_invgauss_mean = list(type = "numeric", default = 0.008),
      hfs_invgauss_shape = list(type = "numeric", default = 0.12),
      hfs_unif_min = list(type = "numeric", default = 0.003),
      hfs_unif_max = list(type = "numeric", default = 0.012),
      spike_range_hfs = list(type = "numeric_vector", default = c(30, 70)),
      interval_range_hfs = list(type = "numeric_vector", default = c(0.003, 0.020)),
      hfs_short_isi_range = list(type = "numeric_vector", default = c(0.003, 0.012)),
      hfs_bridge_isi_range = list(type = "numeric_vector", default = c(0.012, 0.020)),
      hfs_target_short_fraction = list(type = "numeric", default = 0.90),
      hfs_short_fraction_min = list(type = "numeric", default = 0.80),
      hfs_bridge_fraction_max = list(type = "numeric", default = 0.15),
      hfs_max_consecutive_bridge = list(type = "numeric", default = 2),
      hfs_min_duration = list(type = "numeric", default = 0.20),
      hfs_isi_rho = list(type = "numeric", default = 0),
      hfs_isi_trend = list(type = "numeric", default = 0),
      col_hfs_line = list(type = "color", default = NATURE_PATTERN_COLORS["high_frequency_spiking"]),
      dist_noisy = list(type = "select", default = "Uniform"),
      noisy_exp_mean = list(type = "numeric", default = 0.16),
      noisy_gamma_shape = list(type = "numeric", default = 1),
      noisy_gamma_scale = list(type = "numeric", default = 0.5),
      noisy_norm_mean = list(type = "numeric", default = 0.16),
      noisy_norm_sd = list(type = "numeric", default = 0.05),
      noisy_lognorm_meanlog = list(type = "numeric", default = log(0.16)),
      noisy_lognorm_sdlog = list(type = "numeric", default = 0.5),
      noisy_invgauss_mean = list(type = "numeric", default = 0.16),
      noisy_invgauss_shape = list(type = "numeric", default = 0.5),
      noisy_unif_min = list(type = "numeric", default = 0.08),
      noisy_unif_max = list(type = "numeric", default = 0.28),
      spike_range_noisy = list(type = "numeric_vector", default = c(3, 7)),
      interval_range_noisy = list(type = "numeric_vector", default = c(0.08, 0.28)),
      noisy_mm_ratio = list(type = "numeric", default = 1.5),
      noisy_avoid_mode_overlap = list(type = "logical", default = FALSE),
      noisy_isi_rho = list(type = "numeric", default = 0),
      noisy_isi_trend = list(type = "numeric", default = 0),
      col_noisy_line = list(type = "color", default = NATURE_PATTERN_COLORS["Noisy"]),
      stim_enabled = list(type = "logical", default = FALSE),
      stim_experiment_preset = list(type = "select", default = "custom"),
      stim_protocol = list(type = "select", default = "regular"),
      stim_response_type = list(type = "select", default = "excitatory_burst"),
      stim_start_s = list(type = "numeric", default = 5),
      stim_duration_s = list(type = "numeric", default = 0.05),
      stim_n = list(type = "numeric", default = 8),
      stim_isi_s = list(type = "numeric", default = 3),
      stim_pair_interval_s = list(type = "numeric", default = 0.5),
      stim_strength = list(type = "numeric", default = 0.8),
      stim_strength_end = list(type = "numeric", default = 1.0),
      stim_strength_jitter = list(type = "numeric", default = 0),
      stim_deviant_probability = list(type = "numeric", default = 0.2),
      stim_deviant_strength = list(type = "numeric", default = 1.0),
      stim_manual_times = list(type = "text", default = ""),
      stim_manual_strengths = list(type = "text", default = ""),
      stim_feature_modality = list(type = "select", default = "orientation"),
      stim_feature_values = list(type = "text", default = "15,45,90,135,180,225,270,315"),
      stim_feature_xy_values = list(type = "text", default = "0,0; 25,0; 0,25; -25,0; 0,-25; 25,25; -25,25; 25,-25; -25,-25"),
      stim_place_field_x_min = list(type = "numeric", default = -50),
      stim_place_field_x_max = list(type = "numeric", default = 50),
      stim_place_field_y_min = list(type = "numeric", default = -50),
      stim_place_field_y_max = list(type = "numeric", default = 50),
      stim_place_field_center_x = list(type = "numeric", default = 0),
      stim_place_field_center_y = list(type = "numeric", default = 0),
      stim_place_field_width = list(type = "numeric", default = 18),
      stim_place_field_radius = list(type = "numeric", default = 45),
      stim_preferred_feature = list(type = "numeric", default = 15),
      stim_null_feature = list(type = "numeric", default = 90),
      stim_feature_period = list(type = "numeric", default = 180),
      stim_feature_tuning_width = list(type = "numeric", default = 25),
      stim_feature_suppression_width = list(type = "numeric", default = 25),
      stim_feature_min_gain = list(type = "numeric", default = 0.05),
      stim_feature_population_mode = list(type = "select", default = "coverage_balanced_population"),
      stim_feature_responsive_fraction = list(type = "numeric", default = 0.35),
      stim_feature_suppressive_fraction = list(type = "numeric", default = 0.10),
      stim_feature_biphasic_fraction = list(type = "numeric", default = 0.05),
      stim_feature_response_threshold = list(type = "numeric", default = 0.35),
      stim_feature_preferred_response = list(type = "select", default = "excitatory_burst"),
      stim_feature_null_response = list(type = "select", default = "no_response"),
      stim_feature_population_jitter = list(type = "numeric", default = 0.25),
      stim_feature_unit_max_gain = list(type = "numeric", default = 1.0),
      stim_feature_unit_response_reliability = list(type = "numeric", default = 1.0),
      stim_feature_target_unit = list(type = "numeric", default = 1),
      stim_latency_median_s = list(type = "numeric", default = 0.08),
      stim_latency_sdlog = list(type = "numeric", default = 0.25),
      stim_response_probability = list(type = "numeric", default = 1.0),
      stim_max_evoked_bursts = list(type = "numeric", default = 3),
      stim_burst_lambda_base = list(type = "numeric", default = 0.2),
      stim_burst_lambda_strength = list(type = "numeric", default = 2.5),
      stim_burst_spike_min = list(type = "numeric", default = 3),
      stim_burst_spike_max = list(type = "numeric", default = 7),
      stim_pause_min_s = list(type = "numeric", default = 0.5),
      stim_pause_max_s = list(type = "numeric", default = 1.4),
      stim_pause_duration_cv = list(type = "numeric", default = 0.35),
      stim_post_burst_pause_probability = list(type = "numeric", default = 0.25),
      stim_rebound_probability = list(type = "numeric", default = 0.35),
      stim_response_window_s = list(type = "numeric", default = 1.5),
      stim_baseline_recovery_enabled = list(type = "logical", default = TRUE),
      stim_baseline_recovery_mode = list(type = "select", default = "Noisy"),
      stim_pre_stimulus_guard_s = list(type = "numeric", default = 0.02),
      stim_adaptation_enabled = list(type = "logical", default = TRUE),
      stim_adaptation_increment = list(type = "numeric", default = 0.35),
      stim_adaptation_tau_s = list(type = "numeric", default = 12),
      stim_response_floor = list(type = "numeric", default = 0.15),
      obs_enabled = list(type = "logical", default = FALSE),
      obs_detection_probability = list(type = "numeric", default = 0.98),
      obs_false_positive_rate_hz = list(type = "numeric", default = 0),
      obs_jitter_sd_ms = list(type = "numeric", default = 0.2),
      obs_time_bias_ms = list(type = "numeric", default = 0),
      obs_dead_time_ms = list(type = "numeric", default = 0.6),
      obs_seed_offset = list(type = "numeric", default = 200000)
    )
  }

  coerce_reproduction_value <- function(value, spec) {
    if (is.null(value)) value <- spec$default
    if (spec$type %in% c("numeric", "integer")) {
      out <- suppressWarnings(as.numeric(value))[1]
      if (!is.finite(out)) out <- as.numeric(spec$default)[1]
      if (identical(spec$type, "integer")) out <- as.integer(round(out))
      return(out)
    }
    if (identical(spec$type, "numeric_vector")) {
      out <- suppressWarnings(as.numeric(unlist(value, use.names = FALSE)))
      out <- out[is.finite(out)]
      default <- as.numeric(spec$default)
      if (length(out) < length(default)) out <- default
      return(out[seq_along(default)])
    }
    if (identical(spec$type, "logical")) {
      if (is.logical(value)) return(isTRUE(value[1]))
      return(tolower(as.character(value)[1]) %in% c("true", "1", "yes", "y"))
    }
    trimws(as.character(value)[1])
  }

  collect_reproduction_settings <- function() {
    spec <- reproduction_input_spec()
    settings <- lapply(names(spec), function(id) {
      coerce_reproduction_value(input_value(id, spec[[id]]$default), spec[[id]])
    })
    names(settings) <- names(spec)
    settings
  }

  build_reproduction_payload <- function(settings, sim = NULL) {
    payload <- list(
      format = "STSR1",
      simulator = SIMULATOR_ID,
      simulator_version = SIMULATOR_VERSION,
      schema_version = SCHEMA_VERSION,
      config_hash = if (!is.null(sim) && !is.null(sim$config)) config_hash_from_config(sim$config) else NA_character_,
      schema_notes = list(
        Feature_Response_Eligible = "Deprecated compatibility alias of Response_Eligible; it does not mean Feature_Matched.",
        Feature_Drive = "Modulated unit-specific drive; raw tuning components are Feature_Excitation and Feature_Suppression.",
        deprecated_noop_config_fields = c("feature_neutral_response_probability", "feature_weak_response_probability")
      ),
      settings = settings
    )
    if (!is.null(sim)) {
      if (!is.null(sim$config) && !identical(value_or(sim$source, ""), "uploaded_dataset") &&
          requireNamespace("jsonlite", quietly = TRUE)) {
        payload$simulation_config_json <- jsonlite::toJSON(
          sim$config,
          auto_unbox = TRUE,
          null = "null",
          na = "null",
          digits = 15
        )
      }
      payload$expected <- list(
        train_count = generated_train_count(sim),
        generation_key = sim$generation_key,
        derived_rng_seed = sim$seed,
        verification_code = sim$verification_code,
        verification_hash = sim$verification_hash
      )
    }
    payload
  }

  reproduction_payload_config <- function(payload) {
    config_json <- payload$simulation_config_json
    if (is.null(config_json) || !nzchar(as.character(config_json)[1]) ||
        !requireNamespace("jsonlite", quietly = TRUE)) {
      return(NULL)
    }
    cfg <- jsonlite::fromJSON(as.character(config_json)[1], simplifyVector = TRUE)
    if (!is.null(cfg$ratios)) {
      ratios <- suppressWarnings(as.numeric(unlist(cfg$ratios, use.names = FALSE)))
      if (length(ratios) >= length(SPIKE_PATTERN_LEVELS)) {
        ratios <- ratios[seq_along(SPIKE_PATTERN_LEVELS)]
        names(ratios) <- SPIKE_PATTERN_LEVELS
        cfg$ratios <- normalize_pattern_ratios(ratios)
      }
    }
    cfg
  }

  encode_reproduction_code <- function(payload) {
    if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("base64enc", quietly = TRUE)) {
      return(NA_character_)
    }
    payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null", digits = 15)
    encoded <- base64enc::base64encode(charToRaw(payload_json), linewidth = 0)
    paste0("STSR1.", encoded)
  }

  decode_reproduction_code <- function(code) {
    if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("base64enc", quietly = TRUE)) {
      stop("Missing jsonlite or base64enc.", call. = FALSE)
    }
    code <- paste(as.character(value_or(code, "")), collapse = "")
    code <- gsub("[[:space:]]+", "", code)
    if (!grepl("^STSR1\\.", code)) stop("Invalid reproduction code.", call. = FALSE)
    encoded <- sub("^STSR1\\.", "", code)
    payload_json <- rawToChar(base64enc::base64decode(encoded))
    payload <- jsonlite::fromJSON(payload_json, simplifyVector = FALSE)
    if (!identical(payload$format, "STSR1") || is.null(payload$settings)) {
      stop("Invalid reproduction payload.", call. = FALSE)
    }
    payload
  }

  selected_train_values <- function(n_train) {
    selected <- isolate(input$selected_trains)
    if (is.null(selected) || length(selected) == 0) {
      return(seq_len(min(10L, n_train)))
    }
    selected <- unique(as.integer(selected))
    selected <- selected[is.finite(selected) & selected >= 1 & selected <= n_train]
    if (length(selected) == 0) selected <- seq_len(min(10L, n_train))
    head(selected, 10L)
  }

  selected_distribution_train <- function(n_train, reactive = TRUE, input_id = "distribution_train", default_train = 1L) {
    selected <- if (isTRUE(reactive)) input[[input_id]] else isolate(input[[input_id]])
    selected <- suppressWarnings(as.integer(selected[1]))
    if (length(selected) == 0 || !is.finite(selected) || selected < 1L) selected <- default_train
    max(1L, min(as.integer(n_train), selected))
  }

  train_label <- function(lang, train_id) {
    sprintf("Spike train %d", as.integer(train_id))
  }

  train_label_from_sim <- function(sim, train_id, lang = current_lang()) {
    train_id <- suppressWarnings(as.integer(train_id))
    fallback <- train_label(lang, train_id)
    if (is.null(sim) || !is.finite(train_id)) return(fallback)

    if (!is.null(sim$train_labels) && length(sim$train_labels) >= train_id) {
      label <- as.character(sim$train_labels[[train_id]])
      if (nzchar(trimws(label))) return(label)
    }

    if (!is.null(sim$combined_spikes) && nrow(sim$combined_spikes) > 0 &&
        all(c("Train", "Train_Label") %in% names(sim$combined_spikes))) {
      labels <- unique(as.character(sim$combined_spikes$Train_Label[as.integer(sim$combined_spikes$Train) == train_id]))
      labels <- labels[!is.na(labels) & nzchar(trimws(labels))]
      if (length(labels) > 0) return(labels[1])
    }

    fallback
  }

  train_choices_for_sim <- function(n_train, sim = NULL, lang = current_lang()) {
    n_train <- max(1L, as.integer(n_train))
    choices <- as.character(seq_len(n_train))
    names(choices) <- vapply(seq_len(n_train), function(i) train_label_from_sim(sim, i, lang), character(1))
    choices
  }

  train_plot_title <- function(lang, title, train_id) {
    sprintf("%s (%s)", title, train_label(lang, train_id))
  }

  selected_train_patterns <- function(sim, train_id) {
    if (is.null(sim)) return(character(0))
    episodes <- sim$combined_episodes[sim$combined_episodes$Train == train_id, , drop = FALSE]
    intervals <- if (!is.null(sim$combined_intervals)) {
      sim$combined_intervals[sim$combined_intervals$Train == train_id, , drop = FALSE]
    } else {
      build_interval_table(
        sim$combined_spikes[sim$combined_spikes$Train == train_id, , drop = FALSE],
        episodes
      )
    }
    patterns <- unique(c(
      if ("Pattern" %in% names(episodes)) episodes$Pattern else character(0),
      if ("ISI_Label" %in% names(intervals)) intervals$ISI_Label else character(0)
    ))
    patterns[patterns %in% pattern_levels]
  }

  safe_total_time <- function() {
    total_time <- as.numeric(value_or(input$total_time, 25))
    if (!is.finite(total_time) || total_time <= 0) total_time <- 25
    total_time
  }

  sim_total_time <- function(sim = NULL) {
    candidates <- safe_total_time()
    if (!is.null(sim)) {
      if (!is.null(sim$config$total_time)) candidates <- c(candidates, suppressWarnings(as.numeric(sim$config$total_time)))
      if (!is.null(sim$combined_spikes) && nrow(sim$combined_spikes) > 0 && "Time" %in% names(sim$combined_spikes)) {
        candidates <- c(candidates, suppressWarnings(max(as.numeric(sim$combined_spikes$Time), na.rm = TRUE)))
      }
      if (!is.null(sim$combined_intervals) && nrow(sim$combined_intervals) > 0 && "End_Time_s" %in% names(sim$combined_intervals)) {
        candidates <- c(candidates, suppressWarnings(max(as.numeric(sim$combined_intervals$End_Time_s), na.rm = TRUE)))
      }
      if (!is.null(sim$combined_stimuli) && nrow(sim$combined_stimuli) > 0 && "Onset_s" %in% names(sim$combined_stimuli)) {
        candidates <- c(candidates, suppressWarnings(max(as.numeric(sim$combined_stimuli$Onset_s), na.rm = TRUE)))
      }
    }
    candidates <- candidates[is.finite(candidates) & candidates > 0]
    if (length(candidates) == 0) return(25)
    max(candidates, 0.001)
  }

  active_total_time <- function() {
    sim <- NULL
    if (exists("all_spike_trains", inherits = FALSE)) {
      sim <- tryCatch(all_spike_trains(), error = function(err) NULL)
    }
    sim_total_time(sim)
  }

  current_train_count <- function() {
    n_train <- suppressWarnings(as.integer(value_or(input$spike_train_number, 1)))
    if (length(n_train) == 0 || !is.finite(n_train[1]) || n_train[1] < 1) return(1L)
    max(1L, n_train[1])
  }

  generated_train_count <- function(sim) {
    if (!is.null(sim$train_count) && is.finite(sim$train_count)) return(as.integer(sim$train_count))
    max(1L, length(sim$spikes_list))
  }

  train_count_matches_current_input <- function(sim) {
    if (!is.null(sim$source) && identical(as.character(sim$source), "uploaded_dataset")) return(TRUE)
    isTRUE(generated_train_count(sim) == current_train_count())
  }

  require_current_train_count <- function(sim, lang = current_lang()) {
    if (!train_count_matches_current_input(sim)) {
      stop(tr(lang, "stale_train_count_download"), call. = FALSE)
    }
    TRUE
  }

  burst_min_isi_value <- function() {
    # Backward-compatible function name. V13.5.0 derives the automatic floor from
    # the smallest lower bound among the structured short-ISI classes.
    ranges <- list(
      value_or(input$interval_range_burst, c(0.006, 0.045)),
      value_or(input$interval_range_hft, c(0.024, 0.040)),
      value_or(input$hfs_short_isi_range, c(0.003, 0.012))
    )
    lower <- suppressWarnings(vapply(ranges, function(rng) {
      x <- as.numeric(rng)
      if (length(x) >= 1L && is.finite(x[1]) && x[1] >= 0) x[1] else NA_real_
    }, numeric(1)))
    lower <- lower[is.finite(lower)]
    if (length(lower) == 0) return(0)
    min(lower)
  }

  auto_inter_event_gap_enabled <- function() {
    isTRUE(value_or(input$auto_inter_event_gap, TRUE))
  }

  effective_inter_event_gap <- function() {
    if (auto_inter_event_gap_enabled()) return(burst_min_isi_value())
    gap <- as.numeric(value_or(input$inter_event_gap, burst_min_isi_value()))
    if (!is.finite(gap) || gap < 0) return(NA_real_)
    gap
  }

  normalize_spike_window <- function(window, total_time = safe_total_time(), default_width = 10) {
    total_time <- max(0.001, as.numeric(total_time))
    default_width <- min(total_time, max(0.001, as.numeric(default_width)))
    default_window <- c(0, default_width)

    if (is.null(window) || length(window) != 2) return(default_window)
    window <- sort(as.numeric(window))
    if (any(!is.finite(window))) return(default_window)

    window <- pmin(pmax(window, 0), total_time)
    if (diff(window) <= 0) {
      center <- min(max(mean(window), default_width / 2), total_time - default_width / 2)
      return(c(center - default_width / 2, center + default_width / 2))
    }

    window
  }

  spike_time_window <- function() {
    normalize_spike_window(input$spike_time_window)
  }

  normalize_spike_visible_seconds <- function(value, total_time = safe_total_time(), default_width = 10) {
    total_time <- max(0.001, as.numeric(total_time))
    default_width <- min(total_time, max(0.001, as.numeric(default_width)))
    value <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(value) || value <= 0) value <- default_width
    min(total_time, max(0.001, value))
  }

  spike_visible_seconds <- function(total_time = safe_total_time()) {
    normalize_spike_visible_seconds(input$spike_visible_seconds, total_time = total_time)
  }

  spike_svg_base_plot_width <- function() 1498

  spike_svg_plot_width <- function(total_time = safe_total_time(), visible_seconds = spike_visible_seconds(total_time)) {
    total_time <- max(0.001, as.numeric(total_time))
    visible_seconds <- normalize_spike_visible_seconds(visible_seconds, total_time = total_time)
    min(120000, max(spike_svg_base_plot_width(), spike_svg_base_plot_width() * total_time / visible_seconds))
  }

  spike_min_separation_px <- function() 4

  min_spike_generating_isi <- function() {
    ranges <- list(input$interval_range_burst, input$interval_range_hft, input$hfs_short_isi_range, input$interval_range_tonic, input$interval_range_noisy)
    global_min <- effective_inter_event_gap()
    mins <- vapply(ranges, function(rng) {
      if (length(rng) != 2 || any(!is.finite(rng))) return(NA_real_)
      min_val <- as.numeric(rng[1])
      if (is.finite(global_min)) min_val <- max(min_val, global_min)
      min_val
    }, numeric(1))
    mins <- mins[is.finite(mins) & mins > 0]
    if (length(mins) == 0) return(NA_real_)
    min(mins)
  }

  spike_resolution_state <- function() {
    total_time <- active_total_time()
    visible_seconds <- spike_visible_seconds(total_time)
    min_isi <- min_spike_generating_isi()
    px_width <- spike_svg_base_plot_width()
    ms_per_pixel <- visible_seconds / px_width * 1000
    recommended_window <- if (is.finite(min_isi)) {
      min_isi * px_width / spike_min_separation_px()
    } else {
      NA_real_
    }
    can_resolve <- is.finite(min_isi) && visible_seconds <= recommended_window
    list(
      visible_seconds = visible_seconds,
      min_isi = min_isi,
      ms_per_pixel = ms_per_pixel,
      recommended_window = recommended_window,
      can_resolve = can_resolve
    )
  }

  spike_pdf_full_window <- function(sim) {
    total_time <- sim_total_time(sim)
    spike_max <- if (!is.null(sim$combined_spikes) && nrow(sim$combined_spikes) > 0) {
      max(sim$combined_spikes$Time[is.finite(sim$combined_spikes$Time)], na.rm = TRUE)
    } else {
      NA_real_
    }
    if (!is.finite(spike_max)) spike_max <- total_time
    c(0, max(total_time, spike_max, 0.001))
  }

  actual_min_positive_isi <- function(spikes) {
    spikes <- real_spike_rows(spikes)
    if (is.null(spikes) || nrow(spikes) == 0 || !"Train" %in% names(spikes) || !"Time" %in% names(spikes)) {
      return(NA_real_)
    }
    intervals <- unlist(lapply(split(spikes$Time, spikes$Train), function(times) {
      times <- sort(as.numeric(times))
      times <- times[is.finite(times)]
      if (length(times) < 2) return(numeric(0))
      diff(times)
    }), use.names = FALSE)
    intervals <- intervals[is.finite(intervals) & intervals > 0]
    if (length(intervals) == 0) return(NA_real_)
    min(intervals)
  }

  spike_pdf_dimensions <- function(sim, window) {
    duration <- max(diff(window), 0.001)
    min_isi <- actual_min_positive_isi(sim$combined_spikes)
    if (!is.finite(min_isi) || min_isi <= 0) min_isi <- min_spike_generating_isi()
    if (!is.finite(min_isi) || min_isi <= 0) min_isi <- duration / 120

    target_gap_pt <- 2.8
    width <- duration / min_isi * target_gap_pt / 72
    width <- max(11.69, width)
    width <- min(width, 200)
    list(width = width, height = 8.27, min_isi = min_isi)
  }

  dist_choices <- function(lang) {
    setNames(
      c("Exponential", "Gamma", "Normal", "Uniform", "Lognormal", "Inverse Gaussian"),
      c(tr(lang, "dist_exponential"), tr(lang, "dist_gamma"), tr(lang, "dist_normal"), tr(lang, "dist_uniform"), tr(lang, "dist_lognormal"), tr(lang, "dist_invgauss"))
    )
  }

  mode_choices <- function(lang) {
    setNames(c("event", "time"), c(tr(lang, "mode_event"), tr(lang, "mode_time")))
  }

  control_group <- function(title, ..., open = FALSE) {
    tags$details(
      class = "control-group",
      open = if (isTRUE(open)) "open" else NULL,
      tags$summary(title),
      div(class = "control-body", ...)
    )
  }

  stim_literature_metadata <- function(preset) {
    preset <- as.character(value_or(preset, "custom"))[1]
    entries <- list(
      custom = list(
        model_zh = "自定义刺激响应：参数不自动套用单一文献。",
        model_en = "Custom stimulus response: parameters are not tied to one fixed paper.",
        params_zh = "请根据目标细胞类型、刺激模态和实验协议手动设置。",
        params_en = "Set parameters manually for the target cell type, stimulus modality, and protocol.",
        refs = list()
      ),
      intensity_response = list(
        model_zh = "强度-响应曲线：参考 dopamine neuron 的 reward value / intensity-dependent phasic response。",
        model_en = "Intensity-response: inspired by dopamine-neuron reward-value/intensity-dependent phasic responses.",
        params_zh = "采用 0.2→1.0 强度渐变、约 80 ms 响应潜伏期、1 到数个短 phasic burst clusters；强度主要调节 evoked burst 数量和 spike 数，而不是制造整次响应缺失。",
        params_en = "Uses a 0.2→1.0 strength ramp, ~80 ms response latency, and one to several short phasic burst clusters; strength modulates evoked burst and spike counts rather than causing whole-response dropouts.",
        refs = list(
          list(text = "Tobler, Fiorillo & Schultz 2005 Science: adaptive coding of reward value by dopamine neurons.", url = "https://pubmed.ncbi.nlm.nih.gov/15761155/"),
          list(text = "Schultz, Dayan & Montague 1997 Science: dopamine prediction/reward phasic signals.", url = "https://www.its.caltech.edu/~jkenny/nb250c/papers/Schultz-1997.pdf")
        )
      ),
      repeated_adaptation = list(
        model_zh = "重复刺激适应：参考 auditory cortex neurons 的多时间尺度刺激适应。",
        model_en = "Repeated-stimulus adaptation: based on multi-timescale adaptation in auditory cortical neurons.",
        params_zh = "采用 1 s 左右重复刺激间隔、逐次降低 response gain，并允许数秒尺度恢复。",
        params_en = "Uses ~1 s repeated-stimulus spacing, trial-by-trial gain reduction, and recovery over seconds.",
        refs = list(
          list(text = "Ulanovsky, Las, Farkas & Nelken 2004 J Neurosci: multiple time scales of adaptation in auditory cortex neurons.", url = "https://www.jneurosci.org/content/24/46/10440"),
          list(text = "BMC Neuroscience 2009: response degeneration can last roughly 1.0–1.8 s after click trains.", url = "https://link.springer.com/article/10.1186/1471-2202-10-10")
        )
      ),
      stimulus_suppression = list(
        model_zh = "刺激诱发抑制/Pause：参考 striatal TAN/CIN 的 cue-locked pause response。",
        model_en = "Stimulus-induced suppression/pause: based on cue-locked pause responses in striatal TANs/CINs.",
        params_zh = "采用约 90 ms onset、0.15–0.30 s pause。这里的 Pause 是 stimulus-locked suppression epoch，不等同于自发长 pause ISI。",
        params_en = "Uses ~90 ms onset and a 0.15–0.30 s pause. This Pause is a stimulus-locked suppression epoch, not the same as a baseline long-pause ISI.",
        refs = list(
          list(text = "Aosaki, Graybiel & Kimura 1994 J Neurosci: TAN pause occurred about 90 ms after click cue.", url = "https://pubmed.ncbi.nlm.nih.gov/8207500/"),
          list(text = "Zhang & Cragg 2017 Frontiers: TAN pause commonly lasts about 200 ms and may include residual spikes.", url = "https://www.frontiersin.org/journals/systems-neuroscience/articles/10.3389/fnsys.2017.00080/full")
        )
      ),
      biphasic_burst_pause = list(
        model_zh = "Burst-Pause 双相反应：参考 TAN/CIN 的 initial excitation/burst → pause → rebound 结构。",
        model_en = "Biphasic burst-pause: based on the TAN/CIN initial excitation/burst → pause → rebound motif.",
        params_zh = "采用约 60 ms early burst、约 90 ms pause onset、0.18–0.32 s pause；early excitation 可由 1–2 个短 burst clusters 组成，随后进入 pause/rebound 阶段。",
        params_en = "Uses ~60 ms early burst, ~90 ms pause onset, and a 0.18–0.32 s pause; early excitation may contain 1–2 short burst clusters before the pause/rebound phase.",
        refs = list(
          list(text = "Zhang & Cragg 2017 Frontiers: initial excitation around 60 ms, pause around 90 ms lasting about 200 ms, followed by rebound.", url = "https://www.frontiersin.org/journals/systems-neuroscience/articles/10.3389/fnsys.2017.00080/full"),
          list(text = "Prager et al. 2020 Nature Communications: neighboring ChINs show burst-pause-rebound, pause-rebound, and isolated pause variants.", url = "https://www.nature.com/articles/s41467-020-18882-y")
        )
      ),
      paired_pulse_recovery = list(
        model_zh = "双脉冲恢复：参考 auditory paired-pulse suppression / forward suppression。",
        model_en = "Paired-pulse recovery: based on auditory paired-pulse suppression / forward suppression.",
        params_zh = "采用 0.6 s paired-pulse interval；第二脉冲 response gain 被压低，随后按数秒尺度恢复。",
        params_en = "Uses a 0.6 s paired-pulse interval; the second pulse has reduced response gain followed by recovery over seconds.",
        refs = list(
          list(text = "BMC Neuroscience 2009: probe responses tested at 1.0, 1.8, and 3.6 s showed recovery from response degeneration.", url = "https://link.springer.com/article/10.1186/1471-2202-10-10"),
          list(text = "Miyazato et al. 2017: auditory paired-pulse suppression peaked at a 600 ms conditioning-test interval.", url = "https://pmc.ncbi.nlm.nih.gov/articles/PMC5436751/")
        )
      ),
      oddball_adaptation = list(
        model_zh = "Oddball / deviant：参考 auditory stimulus-specific adaptation，标准刺激适应、稀有 deviant 保持更强响应。",
        model_en = "Oddball/deviant: based on auditory stimulus-specific adaptation, where standards adapt more than rare deviants.",
        params_zh = "采用 deviant probability = 0.10、约 0.5 s ISI；标准刺激逐渐适应，deviant 使用更强 response gain。",
        params_en = "Uses deviant probability = 0.10 and ~0.5 s ISI; standard responses adapt while deviant responses retain stronger gain.",
        refs = list(
          list(text = "Ulanovsky, Las & Nelken 2003 Nature Neuroscience: processing of low-probability sounds by cortical neurons.", url = "https://www.weizmann.ac.il/brain-sciences/labs/ulanovsky/sites/brain-sciences.labs.ulanovsky/files/2024-11/Ulanovsky2003.pdf"),
          list(text = "Malmierca et al. 2012 Frontiers: oddball SSA commonly uses 10% deviant probability and 250–500 ms ISIs.", url = "https://www.frontiersin.org/journals/neural-circuits/articles/10.3389/fncir.2012.00089/full")
        )
      ),
      state_dependent = list(
        model_zh = "状态依赖反应：参考 ongoing state 对 stimulus-evoked activity 的调制。",
        model_en = "State-dependent response: based on modulation of stimulus-evoked activity by ongoing network state.",
        params_zh = "响应核根据刺激前主导状态切换：baseline 近似 Noisy/Tonic 时偏向 suppressive pause，Pause 状态后可出现 rebound。",
        params_en = "The response kernel switches with the pre-stimulus state: Noisy/Tonic-like baseline favors suppressive pause, while a pre-existing Pause can produce rebound.",
        refs = list(
          list(text = "Mazzoni et al. 2018 Scientific Reports: stimulus-evoked activity depends on ongoing network state variables.", url = "https://www.nature.com/articles/s41598-018-23853-x"),
          list(text = "Finn et al. / Churchland-related work: stimulus onset interacts with ongoing variability and state.", url = "https://pmc.ncbi.nlm.nih.gov/articles/PMC3545177/")
        )
      ),
      feature_tuning = list(
        model_zh = "特征调谐反应：文献约束的合成 tuning-profile benchmark，用于表示神经元对取向、运动方向、声音频率、颜色、空间位置或触觉位置的选择性；它不是单篇实验的原始 spike train 复刻。",
        model_en = "Feature-tuned response: a literature-constrained synthetic tuning-profile benchmark for selectivity to orientation, motion direction, sound frequency, color, spatial position, or tactile location; it is not a replay of raw spike trains from one experiment.",
        params_zh = "默认预设生成 8 个 motion-direction tuned neurons，每条 spike train 是一个神经元；preferred direction 对齐到 0/45/90/.../315°。默认模型只假设优选特征附近产生较强 evoked Burst，null/opponent 与中性特征回到 baseline/no-response。若手动选择 null/opponent suppressive kernel，才会生成刺激诱发 suppression/Pause；该选项应解释为抑制型响应模型，而不是通用方向调谐的默认规律。声音频率使用 log2 频率距离；二维空间位置使用 Gaussian place-field。",
        params_en = "The default preset generates 8 motion-direction tuned neurons, one neuron per spike train, with preferred directions aligned to 0/45/90/.../315 degrees. By default, only preferred features evoke stronger burst-like responses, whereas null/opponent and neutral features return to baseline/no-response. Stimulus-locked suppression/Pause is generated only if the optional null/opponent suppressive kernel is selected; that option should be interpreted as a suppressive-response model rather than a generic default of direction tuning. Sound frequency uses log2 distance; 2D spatial position uses Gaussian place fields.",
        refs = list(
          list(text = "Hubel & Wiesel 1962 J Physiol: classic receptive-field and orientation-selective visual cortical responses.", url = "https://pmc.ncbi.nlm.nih.gov/articles/PMC1359523/"),
          list(text = "Georgopoulos, Kalaska, Caminiti & Massey 1982 J Neurosci: preferred-direction modulation of primate motor cortical discharge.", url = "https://www.jneurosci.org/content/2/11/1527"),
          list(text = "Albright 1984 J Neurophysiol: direction and orientation selectivity in macaque MT.", url = "https://pubmed.ncbi.nlm.nih.gov/6520628/"),
          list(text = "O'Keefe & Dostrovsky 1971 Brain Research: hippocampal place-cell spatial firing.", url = "https://pubmed.ncbi.nlm.nih.gov/5124915/"),
          list(text = "Mountcastle 1957 J Neurophysiol: modality and topographic properties of somatosensory cortical neurons.", url = "https://pubmed.ncbi.nlm.nih.gov/13439410/"),
          list(text = "Shapley & Hawken 2011 Vision Research: cortical color single- and double-opponent cells.", url = "https://pmc.ncbi.nlm.nih.gov/articles/PMC3121536/"),
          list(text = "Formisano et al. 2003 Neuron: tonotopic organization / frequency tuning in human auditory cortex.", url = "https://pubmed.ncbi.nlm.nih.gov/14622588/"),
          list(text = "Aosaki, Graybiel & Kimura 1994 J Neurosci: striatal TAN pause responses support the optional suppressive/Pause kernel, not generic feature tuning.", url = "https://pubmed.ncbi.nlm.nih.gov/8207500/")
        )
      )
    )
    if (!preset %in% names(entries)) preset <- "custom"
    entries[[preset]]
  }

  stim_literature_card <- function(preset, lang) {
    meta <- stim_literature_metadata(preset)
    title <- if (identical(lang, "zh")) "文献依据与参数定义" else "Literature basis and parameter definition"
    model <- if (identical(lang, "zh")) meta$model_zh else meta$model_en
    params <- if (identical(lang, "zh")) meta$params_zh else meta$params_en
    ref_title <- if (identical(lang, "zh")) "参考文章" else "References"
    ref_nodes <- if (length(meta$refs) == 0) {
      list(tags$li(if (identical(lang, "zh")) "自定义预设：未绑定固定文献。" else "Custom preset: no fixed literature binding."))
    } else {
      lapply(meta$refs, function(ref) tags$li(tags$a(href = ref$url, target = "_blank", ref$text)))
    }
    tags$div(
      class = "stim-literature-card",
      tags$div(tags$strong(title)),
      tags$div(model),
      tags$div(params),
      tags$div(tags$strong(ref_title)),
      tags$ul(ref_nodes)
    )
  }

  output$title_ui <- renderUI({
    lang <- current_lang()
    div(
      div(class = "app-title", tr(lang, "app_title")),
      div(class = "app-subtitle", tr(lang, "app_subtitle"))
    )
  })

  output$stim_literature_card_ui <- renderUI({
    lang <- current_lang()
    preset <- value_or(input$stim_experiment_preset, "custom")
    stim_literature_card(preset, lang)
  })

  main_tab_values <- c("summary", "spike", "distributions", "spike_data", "observation", "episode_data", "stimulus_data", "stimulus_analysis", "validation")
  active_main_tab <- reactiveVal("summary")

  main_tab_labels <- function(lang) {
    c(
      summary = tr(lang, "tab_summary"),
      spike = tr(lang, "tab_spike"),
      distributions = tr(lang, "tab_dist"),
      spike_data = tr(lang, "tab_spike_data"),
      observation = if (identical(lang, "zh")) "观测噪声" else "Observation noise",
      episode_data = tr(lang, "tab_episode_data"),
      stimulus_data = if (identical(lang, "zh")) "刺激数据" else "Stimulus data",
      stimulus_analysis = if (identical(lang, "zh")) "刺激对齐分析" else "Stimulus-aligned analysis",
      validation = if (identical(lang, "zh")) "验证与基准" else "Validation & benchmarks"
    )
  }

  output$top_tabs_ui <- renderUI({
    lang <- current_lang()
    active_tab <- active_main_tab()
    if (!active_tab %in% main_tab_values) active_tab <- "summary"
    labels <- main_tab_labels(lang)
    tags$div(
      class = "top-tab-nav",
      lapply(main_tab_values, function(value) {
        cls <- paste("top-tab-link", if (identical(value, active_tab)) "active" else "")
        tags$button(
          type = "button",
          class = cls,
          onclick = sprintf("Shiny.setInputValue('top_tab_selected', '%s', {priority: 'event'});", value),
          labels[[value]]
        )
      })
    )
  })
  outputOptions(output, "top_tabs_ui", suspendWhenHidden = FALSE)

  observeEvent(input$top_tab_selected, {
    selected <- as.character(input$top_tab_selected)[1]
    if (selected %in% main_tab_values) {
      active_main_tab(selected)
      updateTabsetPanel(session, "main_tabs", selected = selected)
    }
  }, ignoreInit = TRUE)

  output$controls_ui <- renderUI({
    lang <- current_lang()
    sim_for_choices <- if (!is.null(input$run) && input$run > 0) {
      tryCatch(all_spike_trains(), error = function(err) NULL)
    } else {
      NULL
    }
    n_train <- if (!is.null(sim_for_choices)) {
      generated_train_count(sim_for_choices)
    } else {
      max(1L, as.integer(input_value("spike_train_number", 1)))
    }
    dist <- dist_choices(lang)
    train_choices <- train_choices_for_sim(n_train, sim_for_choices, lang)
    burst_min_isi <- isolate(burst_min_isi_value())
    inter_gap_value <- if (isTRUE(input_value("auto_inter_event_gap", TRUE))) {
      burst_min_isi
    } else {
      input_value("inter_event_gap", burst_min_isi)
    }

    tagList(
      control_group(
        tr(lang, "reproduction"),
        textAreaInput(
          "reproduction_code_input",
          tr(lang, "reproduction_code"),
          value = "",
          rows = 4,
          width = "100%"
        ),
        actionButton("load_reproduction_code", tr(lang, "load_reproduction_code"), class = "btn-secondary"),
        helpText(tr(lang, "reproduction_code_hint")),
        open = FALSE
      ),

      control_group(
        tr(lang, "upload_dataset"),
        checkboxInput(
          "use_uploaded_dataset",
          tr(lang, "use_uploaded_dataset"),
          value = input_value("use_uploaded_dataset", FALSE)
        ),
        fileInput(
          "uploaded_spike_dataset",
          tr(lang, "uploaded_dataset_file"),
          accept = c(".csv", ".tsv", ".txt", "text/csv", "text/tab-separated-values", "text/plain")
        ),
        selectInput(
          "uploaded_time_unit",
          tr(lang, "uploaded_time_unit"),
          choices = setNames(c("s", "ms"), c(tr(lang, "uploaded_time_unit_s"), tr(lang, "uploaded_time_unit_ms"))),
          selected = input_value("uploaded_time_unit", "s"),
          selectize = FALSE
        ),
        checkboxInput(
          "uploaded_auto_label_intervals",
          tr(lang, "uploaded_auto_label"),
          value = input_value("uploaded_auto_label_intervals", TRUE)
        ),
        helpText(tr(lang, "uploaded_dataset_hint")),
        helpText(tr(lang, "uploaded_auto_label_hint")),
        open = FALSE
      ),

      control_group(
        tr(lang, "general"),
        div(
          class = "control-grid",
          textInput("generation_key", tr(lang, "generation_key"), value = input_value("generation_key", as.character(input_value("seed", 12345)))),
          numericInput("spike_train_number", tr(lang, "train_count"), value = input_value("spike_train_number", 1), min = 1, step = 1),
          numericInput("total_time", tr(lang, "total_time"), value = input_value("total_time", 25), min = 0.001, step = 1),
          numericInput("isi_xmax", tr(lang, "isi_xmax"), value = input_value("isi_xmax", 2), min = 0.1, step = 0.1),
          numericInput("inter_event_gap", tr(lang, "inter_gap"), value = inter_gap_value, min = 0, step = 0.001),
          selectInput(
            "selected_trains",
            tr(lang, "selected_trains"),
            choices = train_choices,
            selected = as.character(selected_train_values(n_train)),
            multiple = TRUE
          )
        ),
        helpText(tr(lang, "generation_key_hint")),
        checkboxInput("auto_inter_event_gap", tr(lang, "auto_inter_gap"), value = input_value("auto_inter_event_gap", TRUE)),
        helpText(tr(lang, "auto_inter_gap_hint")),
        open = TRUE
      ),

      control_group(
        tr(lang, "pattern_mix"),
        selectInput("generation_mode", tr(lang, "generation_mode"), choices = mode_choices(lang), selected = input_value("generation_mode", "time"), selectize = FALSE),
        helpText(tr(lang, "ratio_hint")),
        div(
          class = "control-grid",
          sliderInput("ratio_burst", paste(tr(lang, "burst"), tr(lang, "ratio")), min = 0, max = 100, value = input_value("ratio_burst", 15)),
          sliderInput("ratio_pause", paste(tr(lang, "pause"), tr(lang, "ratio")), min = 0, max = 100, value = input_value("ratio_pause", 15)),
          sliderInput("ratio_tonic", paste(tr(lang, "tonic"), tr(lang, "ratio")), min = 0, max = 100, value = input_value("ratio_tonic", 20)),
          sliderInput("ratio_hft", paste(tr(lang, "high_frequency_tonic"), tr(lang, "ratio")), min = 0, max = 100, value = input_value("ratio_hft", 15)),
          sliderInput("ratio_hfs", paste(tr(lang, "high_frequency_spiking"), tr(lang, "ratio")), min = 0, max = 100, value = input_value("ratio_hfs", 15)),
          sliderInput("ratio_noisy", paste(tr(lang, "noisy"), tr(lang, "ratio")), min = 0, max = 100, value = input_value("ratio_noisy", 20))
        ),
        checkboxInput("avoid_noisy_burst_runs", tr(lang, "avoid_noisy"), value = input_value("avoid_noisy_burst_runs", TRUE)),
        open = TRUE
      ),

      control_group(
        tr(lang, "sequence"),
        textInput(
          "pattern_sequence",
          tr(lang, "pattern_sequence"),
          value = input_value("pattern_sequence", ""),
          placeholder = "b5p2n2t4hft15hfs40 or p1.0sb5hft12p0.8st6"
        ),
        helpText(tr(lang, "sequence_hint")),
        checkboxInput(
          "leading_silence_initial_pause",
          tr(lang, "leading_silence_initial_pause"),
          value = input_value("leading_silence_initial_pause", TRUE)
        ),
        helpText(tr(lang, "leading_silence_hint")),
        selectInput(
          "initial_latency_model",
          tr(lang, "initial_latency_model"),
          choices = setNames(
            c("residual_life", "same_distribution", "uniform"),
            c(tr(lang, "latency_residual_life"), tr(lang, "latency_same_distribution"), tr(lang, "latency_uniform"))
          ),
          selected = input_value("initial_latency_model", "residual_life"),
          selectize = FALSE
        ),
        helpText(tr(lang, "initial_latency_model_hint")),
        open = FALSE
      ),

      control_group(
        if (identical(lang, "zh")) "刺激模拟" else "Stimulation",
        checkboxInput("stim_enabled", if (identical(lang, "zh")) "启用刺激响应模拟" else "Enable stimulus-response simulation", value = input_value("stim_enabled", FALSE)),
        selectInput(
          "stim_experiment_preset",
          if (identical(lang, "zh")) "实验预设" else "Experiment preset",
          choices = setNames(
            c("custom", "intensity_response", "repeated_adaptation", "stimulus_suppression", "biphasic_burst_pause", "paired_pulse_recovery", "oddball_adaptation", "state_dependent", "feature_tuning"),
            if (identical(lang, "zh")) c("自定义", "强度-响应曲线", "重复刺激适应", "刺激诱发抑制/暂停", "Burst-Pause 双相反应", "双脉冲恢复", "Oddball / deviant", "状态依赖反应", "特征调谐反应") else c("Custom", "Intensity-response", "Repeated-stimulus adaptation", "Stimulus-induced suppression", "Biphasic burst-pause", "Paired-pulse recovery", "Oddball / deviant", "State-dependent response", "Feature-tuned response")
          ),
          selected = input_value("stim_experiment_preset", "custom"),
          selectize = FALSE
        ),
        selectInput(
          "stim_protocol",
          if (identical(lang, "zh")) "刺激协议" else "Stimulus protocol",
          choices = setNames(c("regular", "intensity_ramp", "repeated", "paired_pulse", "oddball", "manual", "feature_tuning"), if (identical(lang, "zh")) c("规则重复", "强度渐变", "重复同一刺激", "双脉冲", "Oddball", "手动时间", "特征调谐") else c("Regular", "Intensity ramp", "Repeated identical", "Paired pulse", "Oddball", "Manual times", "Feature tuning")),
          selected = input_value("stim_protocol", "regular"), selectize = FALSE
        ),
        selectInput(
          "stim_response_type",
          if (identical(lang, "zh")) "响应核" else "Response kernel",
          choices = setNames(c("excitatory_burst", "suppressive_pause", "biphasic", "pause_rebound", "state_dependent", "feature_tuned"), if (identical(lang, "zh")) c("刺激诱发 Burst", "刺激诱发抑制/Pause", "Burst 后接 Pause", "Pause 后 rebound Burst", "状态依赖混合", "特征调谐混合") else c("Excitatory burst", "Suppressive pause", "Burst then pause", "Pause then rebound burst", "State-dependent mixed", "Feature-tuned mixed")),
          selected = input_value("stim_response_type", "excitatory_burst"), selectize = FALSE
        ),
        uiOutput("stim_literature_card_ui"),
        div(class = "control-grid",
            numericInput("stim_start_s", if (identical(lang, "zh")) "首个刺激时间 (s)" else "First stimulus onset (s)", value = input_value("stim_start_s", 5), min = 0, step = 0.1),
            numericInput("stim_n", if (identical(lang, "zh")) "刺激次数" else "Number of stimuli", value = input_value("stim_n", 8), min = 1, step = 1),
            numericInput("stim_isi_s", if (identical(lang, "zh")) "刺激间隔 (s)" else "Inter-stimulus interval (s)", value = input_value("stim_isi_s", 3), min = 0.001, step = 0.1),
            numericInput("stim_duration_s", if (identical(lang, "zh")) "刺激持续时间 (s)" else "Stimulus duration (s)", value = input_value("stim_duration_s", 0.05), min = 0, step = 0.01)),
        div(class = "control-grid",
            numericInput("stim_strength", if (identical(lang, "zh")) "刺激强度起点" else "Stimulus strength", value = input_value("stim_strength", 0.8), min = 0, max = 1, step = 0.05),
            numericInput("stim_strength_end", if (identical(lang, "zh")) "刺激强度终点" else "Final strength", value = input_value("stim_strength_end", 1.0), min = 0, max = 1, step = 0.05),
            numericInput("stim_strength_jitter", if (identical(lang, "zh")) "强度抖动" else "Strength jitter", value = input_value("stim_strength_jitter", 0), min = 0, max = 1, step = 0.02),
            numericInput("stim_pair_interval_s", if (identical(lang, "zh")) "双脉冲间隔 (s)" else "Paired-pulse interval (s)", value = input_value("stim_pair_interval_s", 0.5), min = 0.001, step = 0.05)),
        div(class = "control-grid",
            numericInput("stim_deviant_probability", if (identical(lang, "zh")) "Deviant 概率" else "Deviant probability", value = input_value("stim_deviant_probability", 0.2), min = 0, max = 1, step = 0.05),
            numericInput("stim_deviant_strength", if (identical(lang, "zh")) "Deviant 强度" else "Deviant strength", value = input_value("stim_deviant_strength", 1.0), min = 0, max = 1, step = 0.05)),
        textInput("stim_manual_times", if (identical(lang, "zh")) "手动刺激时间，例如 5,10,15" else "Manual stimulus times, e.g. 5,10,15", value = input_value("stim_manual_times", "")),
        textInput("stim_manual_strengths", if (identical(lang, "zh")) "手动刺激强度，可选" else "Manual strengths, optional", value = input_value("stim_manual_strengths", "")),
        div(class = "control-grid",
            selectInput(
              "stim_feature_modality",
              if (identical(lang, "zh")) "调谐特征模态" else "Tuned feature modality",
              choices = setNames(
                c("orientation", "motion_direction", "auditory_frequency", "color_hue", "spatial_position", "spatial_2d", "tactile_location"),
                if (identical(lang, "zh")) c("视觉取向", "运动方向", "声音频率", "颜色 hue", "一维空间位置", "二维位置 / Place field", "触觉位置") else c("Visual orientation", "Motion direction", "Sound frequency", "Color hue", "1D spatial position", "2D position / place field", "Tactile location")
              ),
              selected = input_value("stim_feature_modality", "orientation"),
              selectize = FALSE
            ),
            numericInput("stim_preferred_feature", if (identical(lang, "zh")) "优选特征值" else "Preferred feature value", value = input_value("stim_preferred_feature", 15), step = 1),
            numericInput("stim_null_feature", if (identical(lang, "zh")) "Null / opponent 特征值" else "Null/opponent feature value", value = input_value("stim_null_feature", 90), step = 1),
            numericInput("stim_feature_period", if (identical(lang, "zh")) "圆周周期，可空 (度)" else "Circular period, optional", value = input_value("stim_feature_period", 180), min = 0, step = 1)),
        textInput("stim_feature_values", if (identical(lang, "zh")) "一维刺激特征序列，例如 15,45,90,135" else "1D stimulus feature values, e.g. 15,45,90,135", value = input_value("stim_feature_values", "15,45,90,135,180,225,270,315")),
        textInput("stim_feature_xy_values", if (identical(lang, "zh")) "二维刺激位置序列，例如 0,0; 25,0; 0,25" else "2D stimulus positions, e.g. 0,0; 25,0; 0,25", value = input_value("stim_feature_xy_values", "0,0; 25,0; 0,25; -25,0; 0,-25; 25,25; -25,25; 25,-25; -25,-25")),
        div(class = "control-grid",
            numericInput("stim_place_field_x_min", if (identical(lang, "zh")) "环境 X 最小值" else "Environment X min", value = input_value("stim_place_field_x_min", -50), step = 1),
            numericInput("stim_place_field_x_max", if (identical(lang, "zh")) "环境 X 最大值" else "Environment X max", value = input_value("stim_place_field_x_max", 50), step = 1),
            numericInput("stim_place_field_y_min", if (identical(lang, "zh")) "环境 Y 最小值" else "Environment Y min", value = input_value("stim_place_field_y_min", -50), step = 1),
            numericInput("stim_place_field_y_max", if (identical(lang, "zh")) "环境 Y 最大值" else "Environment Y max", value = input_value("stim_place_field_y_max", 50), step = 1)),
        div(class = "control-grid",
            numericInput("stim_place_field_center_x", if (identical(lang, "zh")) "共享/目标 place-field 中心 X" else "Shared/target place-field center X", value = input_value("stim_place_field_center_x", 0), step = 1),
            numericInput("stim_place_field_center_y", if (identical(lang, "zh")) "共享/目标 place-field 中心 Y" else "Shared/target place-field center Y", value = input_value("stim_place_field_center_y", 0), step = 1),
            numericInput("stim_place_field_width", if (identical(lang, "zh")) "2D Gaussian place-field 宽度 σ" else "2D Gaussian place-field width σ", value = input_value("stim_place_field_width", 18), min = 0.001, step = 1),
            numericInput("stim_place_field_radius", if (identical(lang, "zh")) "Place-field 响应半径" else "Place-field response radius", value = input_value("stim_place_field_radius", 45), min = 0.001, step = 1)),
        div(class = "control-grid",
            numericInput("stim_feature_tuning_width", if (identical(lang, "zh")) "优选调谐宽度" else "Preferred tuning width", value = input_value("stim_feature_tuning_width", 25), min = 0.001, step = 1),
            numericInput("stim_feature_suppression_width", if (identical(lang, "zh")) "Null/opponent 调制宽度" else "Null/opponent modulation width", value = input_value("stim_feature_suppression_width", 25), min = 0.001, step = 1),
            numericInput("stim_feature_min_gain", if (identical(lang, "zh")) "中性特征最小驱动" else "Neutral feature minimum drive", value = input_value("stim_feature_min_gain", 0.05), min = 0, max = 1, step = 0.01)),
        div(class = "control-grid",
            selectInput("stim_feature_population_mode", if (identical(lang, "zh")) "特征调谐群体模式" else "Feature-tuned population mode",
                        choices = setNames(c("same_unit_trials", "random_heterogeneous_population", "coverage_balanced_population", "sparse_responsive_population", "one_hot_target"),
                                           if (identical(lang, "zh")) c("同一调谐神经元重复试次", "随机异质调谐群体", "覆盖平衡调谐群体", "稀疏响应群体", "单个目标神经元响应") else c("Same tuned unit across trials", "Random heterogeneous population", "Coverage-balanced population", "Sparse responsive population", "One target unit only")),
                        selected = input_value("stim_feature_population_mode", "coverage_balanced_population"), selectize = FALSE),
            numericInput("stim_feature_responsive_fraction", if (identical(lang, "zh")) "响应神经元比例" else "Responsive unit fraction", value = input_value("stim_feature_responsive_fraction", 0.35), min = 0, max = 1, step = 0.05),
            numericInput("stim_feature_target_unit", if (identical(lang, "zh")) "目标神经元编号" else "Target unit index", value = input_value("stim_feature_target_unit", 1), min = 1, step = 1)),
        tags$div(class = "muted-note", if (identical(lang, "zh")) "空间调谐是 benchmark-oriented 的 2D Gaussian place-field-like 模型；它不模拟动物轨迹、速度、theta 相位、phase precession、remapping 或 grid periodicity。覆盖平衡群体会把部分 place-field center 对齐到测试刺激位置以保证 benchmark coverage；随机异质群体则从环境范围中采样。" else "Spatial tuning is a benchmark-oriented 2D Gaussian place-field-like model; it does not model trajectory, velocity, theta phase, phase precession, remapping, or grid periodicity. Coverage-balanced populations align some field centers to tested stimulus positions for benchmark coverage; random heterogeneous populations sample centers from the environment."),
        div(class = "control-grid",
            numericInput("stim_feature_suppressive_fraction", if (identical(lang, "zh")) "响应单元中抑制型比例" else "Suppressive fraction among units", value = input_value("stim_feature_suppressive_fraction", 0.10), min = 0, max = 1, step = 0.05),
            numericInput("stim_feature_biphasic_fraction", if (identical(lang, "zh")) "响应单元中双相比例" else "Biphasic fraction among units", value = input_value("stim_feature_biphasic_fraction", 0.05), min = 0, max = 1, step = 0.05),
            numericInput("stim_feature_response_threshold", if (identical(lang, "zh")) "特征响应阈值" else "Feature response threshold", value = input_value("stim_feature_response_threshold", 0.35), min = 0, max = 1, step = 0.05)),
        div(class = "control-grid",
            selectInput("stim_feature_preferred_response", if (identical(lang, "zh")) "优选特征响应核" else "Preferred-feature response kernel",
                        choices = setNames(c("excitatory_burst", "biphasic", "suppressive_pause", "pause_rebound", "no_response"),
                                           if (identical(lang, "zh")) c("诱发 Burst", "Burst-Pause 双相", "诱发抑制/Pause", "Pause 后 rebound", "无诱发响应/基线") else c("Evoked burst", "Burst-pause biphasic", "Suppressive pause", "Pause-rebound", "No evoked response / baseline")),
                        selected = input_value("stim_feature_preferred_response", "excitatory_burst"), selectize = FALSE),
            selectInput("stim_feature_null_response", if (identical(lang, "zh")) "Null/opponent 特征响应核" else "Null/opponent-feature response kernel",
                        choices = setNames(c("no_response", "suppressive_pause", "pause_rebound", "biphasic", "excitatory_burst"),
                                           if (identical(lang, "zh")) c("无诱发响应/基线", "诱发抑制/Pause", "Pause 后 rebound", "Burst-Pause 双相", "诱发 Burst") else c("No evoked response / baseline", "Suppressive pause", "Pause-rebound", "Burst-pause biphasic", "Evoked burst")),
                        selected = input_value("stim_feature_null_response", "no_response"), selectize = FALSE)),
        div(class = "control-grid",
            numericInput("stim_feature_population_jitter", if (identical(lang, "zh")) "单元调谐异质性 CV" else "Unit tuning heterogeneity CV", value = input_value("stim_feature_population_jitter", 0.25), min = 0, max = 2, step = 0.05),
            numericInput("stim_feature_unit_max_gain", if (identical(lang, "zh")) "单元最大响应增益" else "Unit max response gain", value = input_value("stim_feature_unit_max_gain", 1.0), min = 0, max = 5, step = 0.1),
            numericInput("stim_feature_unit_response_reliability", if (identical(lang, "zh")) "单元响应可靠性" else "Unit response reliability", value = input_value("stim_feature_unit_response_reliability", 1.0), min = 0, max = 1, step = 0.05)),
        tags$hr(),
        div(class = "control-grid",
            numericInput("stim_latency_median_s", if (identical(lang, "zh")) "响应潜伏期中位数 (s)" else "Response latency median (s)", value = input_value("stim_latency_median_s", 0.08), min = 0.001, step = 0.01),
            numericInput("stim_latency_sdlog", if (identical(lang, "zh")) "潜伏期 log 抖动" else "Latency log jitter", value = input_value("stim_latency_sdlog", 0.25), min = 0, step = 0.05),
            numericInput("stim_response_probability", if (identical(lang, "zh")) "响应概率" else "Response probability", value = input_value("stim_response_probability", 1.0), min = 0, max = 1, step = 0.02),
            numericInput("stim_max_evoked_bursts", if (identical(lang, "zh")) "最大诱发 Burst 数" else "Max evoked bursts", value = input_value("stim_max_evoked_bursts", 3), min = 0, step = 1),
            numericInput("stim_burst_lambda_strength", if (identical(lang, "zh")) "强度→Burst 增益" else "Strength-to-burst gain", value = input_value("stim_burst_lambda_strength", 2.5), min = 0, step = 0.1)),
        div(class = "control-grid",
            numericInput("stim_burst_lambda_base", if (identical(lang, "zh")) "基础 Burst 率" else "Baseline burst drive", value = input_value("stim_burst_lambda_base", 0.2), min = 0, step = 0.1),
            numericInput("stim_burst_spike_min", if (identical(lang, "zh")) "诱发 Burst 最少 spike" else "Min spikes per evoked burst", value = input_value("stim_burst_spike_min", 3), min = 3, step = 1),
            numericInput("stim_burst_spike_max", if (identical(lang, "zh")) "诱发 Burst 最多 spike" else "Max spikes per evoked burst", value = input_value("stim_burst_spike_max", 7), min = 3, step = 1)),
        div(class = "control-grid",
            numericInput("stim_pause_min_s", if (identical(lang, "zh")) "诱发 Pause 最短 (s)" else "Evoked pause min (s)", value = input_value("stim_pause_min_s", 0.5), min = 0.001, step = 0.05),
            numericInput("stim_pause_max_s", if (identical(lang, "zh")) "诱发 Pause 最长 (s)" else "Evoked pause max (s)", value = input_value("stim_pause_max_s", 1.4), min = 0.001, step = 0.05),
            numericInput("stim_pause_duration_cv", if (identical(lang, "zh")) "Pause 时长 CV" else "Pause duration CV", value = input_value("stim_pause_duration_cv", 0.35), min = 0, max = 2, step = 0.05),
            numericInput("stim_post_burst_pause_probability", if (identical(lang, "zh")) "Burst 后 Pause 概率" else "Post-burst pause probability", value = input_value("stim_post_burst_pause_probability", 0.25), min = 0, max = 1, step = 0.05),
            numericInput("stim_rebound_probability", if (identical(lang, "zh")) "Rebound Burst 概率" else "Rebound burst probability", value = input_value("stim_rebound_probability", 0.35), min = 0, max = 1, step = 0.05),
            numericInput("stim_response_window_s", if (identical(lang, "zh")) "响应窗口 (s)" else "Response window (s)", value = input_value("stim_response_window_s", 1.5), min = 0.01, step = 0.05)),
        checkboxInput("stim_baseline_recovery_enabled", if (identical(lang, "zh")) "刺激间隔恢复到基线发放" else "Recover baseline firing between stimuli", value = input_value("stim_baseline_recovery_enabled", TRUE)),
        div(class = "control-grid",
            selectInput(
              "stim_baseline_recovery_mode",
              if (identical(lang, "zh")) "基线恢复模式" else "Baseline recovery mode",
              choices = setNames(c("Noisy", "Tonic", "ratio"), if (identical(lang, "zh")) c("Noisy", "Tonic", "按主模式比例") else c("Noisy", "Tonic", "Ratio-based")),
              selected = input_value("stim_baseline_recovery_mode", "Noisy"),
              selectize = FALSE
            ),
            numericInput("stim_pre_stimulus_guard_s", if (identical(lang, "zh")) "刺激前保护间隔 (s)" else "Pre-stimulus guard (s)", value = input_value("stim_pre_stimulus_guard_s", 0.02), min = 0, step = 0.005)),
        checkboxInput("stim_adaptation_enabled", if (identical(lang, "zh")) "启用重复刺激适应/恢复" else "Enable repeated-stimulus adaptation/recovery", value = input_value("stim_adaptation_enabled", TRUE)),
        div(class = "control-grid",
            numericInput("stim_adaptation_increment", if (identical(lang, "zh")) "适应增量" else "Adaptation increment", value = input_value("stim_adaptation_increment", 0.35), min = 0, step = 0.05),
            numericInput("stim_adaptation_tau_s", if (identical(lang, "zh")) "恢复时间常数 (s)" else "Recovery time constant (s)", value = input_value("stim_adaptation_tau_s", 12), min = 0.001, step = 0.5),
            numericInput("stim_response_floor", if (identical(lang, "zh")) "响应下限" else "Response floor", value = input_value("stim_response_floor", 0.15), min = 0, max = 1, step = 0.05)),
        tags$div(class = "muted-note", if (identical(lang, "zh")) "刺激模块生成 stimulus table 和 response table，并在 interval / episode 表中标记 Evoked、Stimulus_ID、Response_Epoch。刺激 onset 本身不是 spike。" else "The stimulation module exports stimulus and response tables and marks Evoked, Stimulus_ID, and Response_Epoch in interval/episode tables. Stimulus onset is not a spike."),
        open = FALSE
      ),

      control_group(
        if (identical(lang, "zh")) "观测噪声 / spike 检测" else "Observation noise / spike detection",
        checkboxInput(
          "obs_enabled",
          if (identical(lang, "zh")) "启用观测噪声层" else "Enable observation-noise layer",
          value = input_value("obs_enabled", FALSE)
        ),
        tags$div(
          class = "muted-note",
          if (identical(lang, "zh")) {
            "潜在 spike train 是算法真值；观测 spike train 模拟记录与 spike sorting 之后实际可见的数据。启用后会按检出概率、伪阳性率、时间戳抖动和检测死区生成观测事件，同时保留潜在到观测的映射。"
          } else {
            "Latent spike trains remain the ground truth; observed spike trains emulate what is visible after recording and spike sorting. When enabled, detection probability, false positives, timestamp jitter, and detector dead time produce observed events plus a latent-to-observed map."
          }
        ),
        div(
          class = "control-grid",
          numericInput(
            "obs_detection_probability",
            if (identical(lang, "zh")) "真实 spike 检出概率" else "True-spike detection probability",
            value = input_value("obs_detection_probability", 0.98),
            min = 0, max = 1, step = 0.01
          ),
          numericInput(
            "obs_false_positive_rate_hz",
            if (identical(lang, "zh")) "伪阳性率 (Hz / train)" else "False-positive rate (Hz / train)",
            value = input_value("obs_false_positive_rate_hz", 0),
            min = 0, step = 0.01
          ),
          numericInput(
            "obs_jitter_sd_ms",
            if (identical(lang, "zh")) "时间戳抖动 SD (ms)" else "Timestamp jitter SD (ms)",
            value = input_value("obs_jitter_sd_ms", 0.2),
            min = 0, step = 0.05
          ),
          numericInput(
            "obs_time_bias_ms",
            if (identical(lang, "zh")) "时间戳系统偏移 (ms)" else "Timestamp bias (ms)",
            value = input_value("obs_time_bias_ms", 0),
            step = 0.05
          ),
          numericInput(
            "obs_dead_time_ms",
            if (identical(lang, "zh")) "检测死区 / 合并窗口 (ms)" else "Detector dead time / merge window (ms)",
            value = input_value("obs_dead_time_ms", 0.6),
            min = 0, step = 0.05
          ),
          numericInput(
            "obs_seed_offset",
            if (identical(lang, "zh")) "观测噪声随机种子偏移" else "Observation-noise seed offset",
            value = input_value("obs_seed_offset", 200000),
            min = 1, step = 1
          )
        ),
        open = FALSE
      ),

      control_group(
        tr(lang, "burst_settings"),
        selectInput("dist_burst", tr(lang, "dist"), choices = dist, selected = input_value("dist_burst", "Gamma"), selectize = FALSE),
        conditionalPanel(
          condition = "input.dist_burst == 'Exponential'",
          numericInput("burst_exp_mean", tr(lang, "mean_isi"), value = input_value("burst_exp_mean", 0.024), min = 0, step = 0.001)
        ),
        conditionalPanel(
          condition = "input.dist_burst == 'Gamma'",
          div(
            class = "control-grid",
            numericInput("burst_gamma_shape", tr(lang, "shape"), value = input_value("burst_gamma_shape", 2), min = 0, step = 0.1),
            numericInput("burst_gamma_scale", tr(lang, "scale"), value = input_value("burst_gamma_scale", 0.012), min = 0, step = 0.001)
          )
        ),
        conditionalPanel(
          condition = "input.dist_burst == 'Normal'",
          div(
            class = "control-grid",
            numericInput("burst_norm_mean", tr(lang, "mean_isi"), value = input_value("burst_norm_mean", 0.024), min = 0, step = 0.001),
            numericInput("burst_norm_sd", tr(lang, "sd"), value = input_value("burst_norm_sd", 0.008), min = 0, step = 0.001)
          )
        ),

        conditionalPanel(
          condition = "input.dist_burst == 'Lognormal'",
          div(
            class = "control-grid",
            numericInput("burst_lognorm_meanlog", tr(lang, "meanlog"), value = input_value("burst_lognorm_meanlog", log(0.024)), step = 0.05),
            numericInput("burst_lognorm_sdlog", tr(lang, "sdlog"), value = input_value("burst_lognorm_sdlog", 0.35), min = 0, step = 0.05)
          )
        ),
        conditionalPanel(
          condition = "input.dist_burst == 'Inverse Gaussian'",
          div(
            class = "control-grid",
            numericInput("burst_invgauss_mean", tr(lang, "invgauss_mean"), value = input_value("burst_invgauss_mean", 0.024), min = 0, step = 0.001),
            numericInput("burst_invgauss_shape", tr(lang, "invgauss_shape"), value = input_value("burst_invgauss_shape", 0.25), min = 0, step = 0.05)
          )
        ),
        conditionalPanel(
          condition = "input.dist_burst == 'Uniform'",
          div(
            class = "control-grid",
            numericInput("burst_unif_min", tr(lang, "min_isi"), value = input_value("burst_unif_min", 0.006), min = 0, step = 0.001),
            numericInput("burst_unif_max", tr(lang, "max_isi"), value = input_value("burst_unif_max", 0.045), min = 0, step = 0.001)
          )
        ),
        sliderInput("spike_range_burst", tr(lang, "spike_range"), min = 1, max = 30, value = input_value("spike_range_burst", c(3, 6))),
        sliderInput("interval_range_burst", tr(lang, "accepted_isi"), min = 0.001, max = 2, value = input_value("interval_range_burst", c(0.006, 0.045))),
        div(class = "control-grid",
            numericInput("burst_isi_rho", tr(lang, "isi_rho"), value = input_value("burst_isi_rho", 0), min = -0.95, max = 0.95, step = 0.05),
            numericInput("burst_isi_trend", tr(lang, "isi_trend"), value = input_value("burst_isi_trend", 0), min = -3, max = 3, step = 0.1)),
        helpText(tr(lang, "isi_temporal_hint")),
        colourInput("col_burst_line", tr(lang, "line_color"), value = input_value("col_burst_line", NATURE_PATTERN_COLORS["Burst"]))
      ),

      control_group(
        tr(lang, "pause_settings"),
        selectInput("dist_pause", tr(lang, "dist"), choices = dist, selected = input_value("dist_pause", "Exponential"), selectize = FALSE),
        conditionalPanel(
          condition = "input.dist_pause == 'Exponential'",
          numericInput("pause_exp_mean", tr(lang, "mean_pause"), value = input_value("pause_exp_mean", 1.0), min = 0, step = 0.05)
        ),
        conditionalPanel(
          condition = "input.dist_pause == 'Gamma'",
          div(
            class = "control-grid",
            numericInput("pause_gamma_shape", tr(lang, "shape"), value = input_value("pause_gamma_shape", 2), min = 0, step = 0.1),
            numericInput("pause_gamma_scale", tr(lang, "scale"), value = input_value("pause_gamma_scale", 0.5), min = 0, step = 0.05)
          )
        ),
        conditionalPanel(
          condition = "input.dist_pause == 'Normal'",
          div(
            class = "control-grid",
            numericInput("pause_norm_mean", tr(lang, "mean_pause"), value = input_value("pause_norm_mean", 1.0), min = 0, step = 0.05),
            numericInput("pause_norm_sd", tr(lang, "sd"), value = input_value("pause_norm_sd", 0.2), min = 0, step = 0.01)
          )
        ),

        conditionalPanel(
          condition = "input.dist_pause == 'Lognormal'",
          div(
            class = "control-grid",
            numericInput("pause_lognorm_meanlog", tr(lang, "meanlog"), value = input_value("pause_lognorm_meanlog", 0), step = 0.05),
            numericInput("pause_lognorm_sdlog", tr(lang, "sdlog"), value = input_value("pause_lognorm_sdlog", 0.45), min = 0, step = 0.05)
          )
        ),
        conditionalPanel(
          condition = "input.dist_pause == 'Inverse Gaussian'",
          div(
            class = "control-grid",
            numericInput("pause_invgauss_mean", tr(lang, "invgauss_mean"), value = input_value("pause_invgauss_mean", 1), min = 0, step = 0.05),
            numericInput("pause_invgauss_shape", tr(lang, "invgauss_shape"), value = input_value("pause_invgauss_shape", 2), min = 0, step = 0.1)
          )
        ),
        conditionalPanel(
          condition = "input.dist_pause == 'Uniform'",
          div(
            class = "control-grid",
            numericInput("pause_unif_min", tr(lang, "min_pause"), value = input_value("pause_unif_min", 0.7), min = 0, step = 0.05),
            numericInput("pause_unif_max", tr(lang, "max_pause"), value = input_value("pause_unif_max", 1.5), min = 0, step = 0.05)
          )
        ),
        sliderInput("pause_duration_range", tr(lang, "accepted_pause"), min = 0.01, max = 5, value = input_value("pause_duration_range", c(0.7, 1.5))),
        div(class = "control-grid",
            numericInput("pause_isi_rho", tr(lang, "isi_rho"), value = input_value("pause_isi_rho", 0), min = -0.95, max = 0.95, step = 0.05),
            numericInput("pause_isi_trend", tr(lang, "isi_trend"), value = input_value("pause_isi_trend", 0), min = -3, max = 3, step = 0.1)),
        helpText(tr(lang, "isi_temporal_hint")),
        colourInput("col_pause_line", tr(lang, "line_color"), value = input_value("col_pause_line", NATURE_PATTERN_COLORS["Pause"]))
      ),

      control_group(
        tr(lang, "tonic_settings"),
        selectInput("dist_tonic", tr(lang, "dist"), choices = dist, selected = input_value("dist_tonic", "Normal"), selectize = FALSE),
        conditionalPanel(
          condition = "input.dist_tonic == 'Exponential'",
          numericInput("tonic_exp_mean", tr(lang, "mean_isi"), value = input_value("tonic_exp_mean", 0.45), min = 0, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.dist_tonic == 'Gamma'",
          div(
            class = "control-grid",
            numericInput("tonic_gamma_shape", tr(lang, "shape"), value = input_value("tonic_gamma_shape", 30), min = 0, step = 0.5),
            numericInput("tonic_gamma_scale", tr(lang, "scale"), value = input_value("tonic_gamma_scale", 0.015), min = 0, step = 0.001)
          )
        ),
        conditionalPanel(
          condition = "input.dist_tonic == 'Normal'",
          div(
            class = "control-grid",
            numericInput("tonic_norm_mean", tr(lang, "mean_isi"), value = input_value("tonic_norm_mean", 0.45), min = 0, step = 0.01),
            numericInput("tonic_norm_sd", tr(lang, "sd"), value = input_value("tonic_norm_sd", 0.03), min = 0, step = 0.005)
          )
        ),

        conditionalPanel(
          condition = "input.dist_tonic == 'Lognormal'",
          div(
            class = "control-grid",
            numericInput("tonic_lognorm_meanlog", tr(lang, "meanlog"), value = input_value("tonic_lognorm_meanlog", log(0.45)), step = 0.05),
            numericInput("tonic_lognorm_sdlog", tr(lang, "sdlog"), value = input_value("tonic_lognorm_sdlog", 0.08), min = 0, step = 0.01)
          )
        ),
        conditionalPanel(
          condition = "input.dist_tonic == 'Inverse Gaussian'",
          div(
            class = "control-grid",
            numericInput("tonic_invgauss_mean", tr(lang, "invgauss_mean"), value = input_value("tonic_invgauss_mean", 0.45), min = 0, step = 0.01),
            numericInput("tonic_invgauss_shape", tr(lang, "invgauss_shape"), value = input_value("tonic_invgauss_shape", 50), min = 0, step = 1)
          )
        ),
        conditionalPanel(
          condition = "input.dist_tonic == 'Uniform'",
          div(
            class = "control-grid",
            numericInput("tonic_unif_min", tr(lang, "min_isi"), value = input_value("tonic_unif_min", 0.38), min = 0, step = 0.01),
            numericInput("tonic_unif_max", tr(lang, "max_isi"), value = input_value("tonic_unif_max", 0.52), min = 0, step = 0.01)
          )
        ),
        sliderInput("spike_range_tonic", tr(lang, "spike_range"), min = 3, max = 30, value = input_value("spike_range_tonic", c(4, 8))),
        sliderInput("interval_range_tonic", tr(lang, "accepted_isi"), min = 0.001, max = 2, value = input_value("interval_range_tonic", c(0.38, 0.52))),
        sliderInput("tonic_cv_range", tr(lang, "tonic_cv_range"), min = 0, max = 2, value = input_value("tonic_cv_range", c(0, 0.18)), step = 0.01),
        sliderInput("tonic_cv2_range", tr(lang, "tonic_cv2_range"), min = 0, max = 2, value = input_value("tonic_cv2_range", c(0, 0.25)), step = 0.01),
        sliderInput("tonic_lv_range", tr(lang, "tonic_lv_range"), min = 0, max = 3, value = input_value("tonic_lv_range", c(0, 0.06)), step = 0.01),
        div(class = "control-grid",
            numericInput("tonic_isi_rho", tr(lang, "isi_rho"), value = input_value("tonic_isi_rho", 0), min = -0.95, max = 0.95, step = 0.05),
            numericInput("tonic_isi_trend", tr(lang, "isi_trend"), value = input_value("tonic_isi_trend", 0), min = -3, max = 3, step = 0.1)),
        helpText(tr(lang, "isi_temporal_hint")),
        colourInput("col_tonic_line", tr(lang, "line_color"), value = input_value("col_tonic_line", NATURE_PATTERN_COLORS["Tonic"]))
      ),


      control_group(
        tr(lang, "hft_settings"),
        selectInput("dist_hft", tr(lang, "dist"), choices = dist, selected = input_value("dist_hft", "Normal"), selectize = FALSE),
        conditionalPanel(condition = "input.dist_hft == 'Exponential'",
          numericInput("hft_exp_mean", tr(lang, "mean_isi"), value = input_value("hft_exp_mean", 0.032), min = 0, step = 0.001)),
        conditionalPanel(condition = "input.dist_hft == 'Gamma'",
          div(class = "control-grid",
            numericInput("hft_gamma_shape", tr(lang, "shape"), value = input_value("hft_gamma_shape", 40), min = 0, step = 0.5),
            numericInput("hft_gamma_scale", tr(lang, "scale"), value = input_value("hft_gamma_scale", 0.0008), min = 0, step = 0.0001))),
        conditionalPanel(condition = "input.dist_hft == 'Normal'",
          div(class = "control-grid",
            numericInput("hft_norm_mean", tr(lang, "mean_isi"), value = input_value("hft_norm_mean", 0.032), min = 0, step = 0.001),
            numericInput("hft_norm_sd", tr(lang, "sd"), value = input_value("hft_norm_sd", 0.003), min = 0, step = 0.0005))),
        conditionalPanel(condition = "input.dist_hft == 'Lognormal'",
          div(class = "control-grid",
            numericInput("hft_lognorm_meanlog", tr(lang, "meanlog"), value = input_value("hft_lognorm_meanlog", log(0.032)), step = 0.05),
            numericInput("hft_lognorm_sdlog", tr(lang, "sdlog"), value = input_value("hft_lognorm_sdlog", 0.10), min = 0, step = 0.01))),
        conditionalPanel(condition = "input.dist_hft == 'Inverse Gaussian'",
          div(class = "control-grid",
            numericInput("hft_invgauss_mean", tr(lang, "invgauss_mean"), value = input_value("hft_invgauss_mean", 0.032), min = 0, step = 0.001),
            numericInput("hft_invgauss_shape", tr(lang, "invgauss_shape"), value = input_value("hft_invgauss_shape", 0.5), min = 0, step = 0.05))),
        conditionalPanel(condition = "input.dist_hft == 'Uniform'",
          div(class = "control-grid",
            numericInput("hft_unif_min", tr(lang, "min_isi"), value = input_value("hft_unif_min", 0.026), min = 0, step = 0.001),
            numericInput("hft_unif_max", tr(lang, "max_isi"), value = input_value("hft_unif_max", 0.038), min = 0, step = 0.001))),
        sliderInput("spike_range_hft", tr(lang, "spike_range"), min = HF_TONIC_MIN_BOUNDARY_SPIKES, max = 100, value = input_value("spike_range_hft", c(8, 24))),
        sliderInput("interval_range_hft", tr(lang, "accepted_isi"), min = 0.001, max = 0.20, value = input_value("interval_range_hft", c(0.024, 0.040)), step = 0.001),
        sliderInput("hft_cv_range", tr(lang, "hft_cv_range"), min = 0, max = 1, value = input_value("hft_cv_range", c(0, 0.22)), step = 0.01),
        sliderInput("hft_cv2_range", tr(lang, "hft_cv2_range"), min = 0, max = 1.5, value = input_value("hft_cv2_range", c(0, 0.28)), step = 0.01),
        sliderInput("hft_lv_range", tr(lang, "hft_lv_range"), min = 0, max = 2, value = input_value("hft_lv_range", c(0, 0.22)), step = 0.01),
        sliderInput("hft_mm_range", tr(lang, "hft_mm_range"), min = 1, max = 3, value = input_value("hft_mm_range", c(1, 1.25)), step = 0.01),
        div(class = "control-grid",
          numericInput("hft_isi_rho", tr(lang, "isi_rho"), value = input_value("hft_isi_rho", 0.20), min = -0.95, max = 0.95, step = 0.05),
          numericInput("hft_isi_trend", tr(lang, "isi_trend"), value = input_value("hft_isi_trend", 0), min = -3, max = 3, step = 0.1)),
        helpText(tr(lang, "isi_temporal_hint")),
        colourInput("col_hft_line", tr(lang, "line_color"), value = input_value("col_hft_line", NATURE_PATTERN_COLORS["high_frequency_tonic"]))
      ),

      control_group(
        tr(lang, "hfs_settings"),
        helpText(if (identical(lang, "zh")) "高频持续放电按长状态生成：多数 ISI 来自短 ISI 分布，并允许少量中等 ISI；默认至少 30 个边界 spike。" else "High-frequency spiking is generated as a long state: most ISIs come from the short-ISI distribution, with a limited number of tolerated moderate ISIs; the default minimum is 30 boundary spikes."),
        selectInput("dist_hfs", if (identical(lang, "zh")) "短 ISI 分布" else "Short-ISI distribution", choices = dist, selected = input_value("dist_hfs", "Gamma"), selectize = FALSE),
        conditionalPanel(condition = "input.dist_hfs == 'Exponential'",
          numericInput("hfs_exp_mean", tr(lang, "mean_isi"), value = input_value("hfs_exp_mean", 0.008), min = 0, step = 0.001)),
        conditionalPanel(condition = "input.dist_hfs == 'Gamma'",
          div(class = "control-grid",
            numericInput("hfs_gamma_shape", tr(lang, "shape"), value = input_value("hfs_gamma_shape", 3), min = 0, step = 0.2),
            numericInput("hfs_gamma_scale", tr(lang, "scale"), value = input_value("hfs_gamma_scale", 0.0025), min = 0, step = 0.0005))),
        conditionalPanel(condition = "input.dist_hfs == 'Normal'",
          div(class = "control-grid",
            numericInput("hfs_norm_mean", tr(lang, "mean_isi"), value = input_value("hfs_norm_mean", 0.008), min = 0, step = 0.001),
            numericInput("hfs_norm_sd", tr(lang, "sd"), value = input_value("hfs_norm_sd", 0.002), min = 0, step = 0.0005))),
        conditionalPanel(condition = "input.dist_hfs == 'Lognormal'",
          div(class = "control-grid",
            numericInput("hfs_lognorm_meanlog", tr(lang, "meanlog"), value = input_value("hfs_lognorm_meanlog", log(0.008)), step = 0.05),
            numericInput("hfs_lognorm_sdlog", tr(lang, "sdlog"), value = input_value("hfs_lognorm_sdlog", 0.20), min = 0, step = 0.01))),
        conditionalPanel(condition = "input.dist_hfs == 'Inverse Gaussian'",
          div(class = "control-grid",
            numericInput("hfs_invgauss_mean", tr(lang, "invgauss_mean"), value = input_value("hfs_invgauss_mean", 0.008), min = 0, step = 0.001),
            numericInput("hfs_invgauss_shape", tr(lang, "invgauss_shape"), value = input_value("hfs_invgauss_shape", 0.12), min = 0, step = 0.05))),
        conditionalPanel(condition = "input.dist_hfs == 'Uniform'",
          div(class = "control-grid",
            numericInput("hfs_unif_min", tr(lang, "min_isi"), value = input_value("hfs_unif_min", 0.003), min = 0, step = 0.001),
            numericInput("hfs_unif_max", tr(lang, "max_isi"), value = input_value("hfs_unif_max", 0.012), min = 0, step = 0.001))),
        sliderInput("spike_range_hfs", tr(lang, "spike_range"), min = HF_SPIKING_MIN_BOUNDARY_SPIKES, max = 200, value = input_value("spike_range_hfs", c(30, 70))),
        sliderInput("interval_range_hfs", tr(lang, "accepted_isi"), min = 0.001, max = 0.10, value = input_value("interval_range_hfs", c(0.003, 0.020)), step = 0.001),
        sliderInput("hfs_short_isi_range", tr(lang, "hfs_short_isi_range"), min = 0.001, max = 0.05, value = input_value("hfs_short_isi_range", c(0.003, 0.012)), step = 0.001),
        sliderInput("hfs_bridge_isi_range", tr(lang, "hfs_bridge_isi_range"), min = 0.001, max = 0.10, value = input_value("hfs_bridge_isi_range", c(0.012, 0.020)), step = 0.001),
        div(class = "control-grid",
          numericInput("hfs_target_short_fraction", tr(lang, "hfs_target_short_fraction"), value = input_value("hfs_target_short_fraction", 0.90), min = 0, max = 1, step = 0.01),
          numericInput("hfs_short_fraction_min", tr(lang, "hfs_short_fraction_min"), value = input_value("hfs_short_fraction_min", 0.80), min = 0, max = 1, step = 0.01),
          numericInput("hfs_bridge_fraction_max", tr(lang, "hfs_bridge_fraction_max"), value = input_value("hfs_bridge_fraction_max", 0.15), min = 0, max = 1, step = 0.01),
          numericInput("hfs_max_consecutive_bridge", tr(lang, "hfs_max_consecutive_bridge"), value = input_value("hfs_max_consecutive_bridge", 2), min = 0, max = 10, step = 1),
          numericInput("hfs_min_duration", tr(lang, "hfs_min_duration"), value = input_value("hfs_min_duration", 0.20), min = 0, step = 0.05)),
        div(class = "control-grid",
          numericInput("hfs_isi_rho", tr(lang, "isi_rho"), value = input_value("hfs_isi_rho", 0), min = -0.95, max = 0.95, step = 0.05),
          numericInput("hfs_isi_trend", tr(lang, "isi_trend"), value = input_value("hfs_isi_trend", 0), min = -3, max = 3, step = 0.1)),
        helpText(tr(lang, "isi_temporal_hint")),
        colourInput("col_hfs_line", tr(lang, "line_color"), value = input_value("col_hfs_line", NATURE_PATTERN_COLORS["high_frequency_spiking"]))
      ),

      control_group(
        tr(lang, "noisy_settings"),
        selectInput("dist_noisy", tr(lang, "dist"), choices = dist, selected = input_value("dist_noisy", "Uniform"), selectize = FALSE),
        conditionalPanel(
          condition = "input.dist_noisy == 'Exponential'",
          numericInput("noisy_exp_mean", tr(lang, "mean_isi"), value = input_value("noisy_exp_mean", 0.16), min = 0, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.dist_noisy == 'Gamma'",
          div(
            class = "control-grid",
            numericInput("noisy_gamma_shape", tr(lang, "shape"), value = input_value("noisy_gamma_shape", 1), min = 0, step = 0.1),
            numericInput("noisy_gamma_scale", tr(lang, "scale"), value = input_value("noisy_gamma_scale", 0.5), min = 0, step = 0.01)
          )
        ),
        conditionalPanel(
          condition = "input.dist_noisy == 'Normal'",
          div(
            class = "control-grid",
            numericInput("noisy_norm_mean", tr(lang, "mean_isi"), value = input_value("noisy_norm_mean", 0.16), min = 0, step = 0.01),
            numericInput("noisy_norm_sd", tr(lang, "sd"), value = input_value("noisy_norm_sd", 0.05), min = 0, step = 0.01)
          )
        ),

        conditionalPanel(
          condition = "input.dist_noisy == 'Lognormal'",
          div(
            class = "control-grid",
            numericInput("noisy_lognorm_meanlog", tr(lang, "meanlog"), value = input_value("noisy_lognorm_meanlog", log(0.16)), step = 0.05),
            numericInput("noisy_lognorm_sdlog", tr(lang, "sdlog"), value = input_value("noisy_lognorm_sdlog", 0.5), min = 0, step = 0.05)
          )
        ),
        conditionalPanel(
          condition = "input.dist_noisy == 'Inverse Gaussian'",
          div(
            class = "control-grid",
            numericInput("noisy_invgauss_mean", tr(lang, "invgauss_mean"), value = input_value("noisy_invgauss_mean", 0.16), min = 0, step = 0.01),
            numericInput("noisy_invgauss_shape", tr(lang, "invgauss_shape"), value = input_value("noisy_invgauss_shape", 0.5), min = 0, step = 0.05)
          )
        ),
        conditionalPanel(
          condition = "input.dist_noisy == 'Uniform'",
          div(
            class = "control-grid",
            numericInput("noisy_unif_min", tr(lang, "min_isi"), value = input_value("noisy_unif_min", 0.08), min = 0, step = 0.01),
            numericInput("noisy_unif_max", tr(lang, "max_isi"), value = input_value("noisy_unif_max", 0.28), min = 0, step = 0.05)
          )
        ),
        sliderInput("spike_range_noisy", tr(lang, "spike_range"), min = 2, max = 50, value = range(pmax(as.numeric(input_value("spike_range_noisy", c(3, 7))), 2))),
        sliderInput("interval_range_noisy", tr(lang, "accepted_isi"), min = 0.001, max = 3, value = input_value("interval_range_noisy", c(0.08, 0.28))),
        numericInput("noisy_mm_ratio", tr(lang, "noisy_mm_ratio"), value = max(NOISY_MIN_MM_RATIO, as.numeric(input_value("noisy_mm_ratio", NOISY_MIN_MM_RATIO))), min = NOISY_MIN_MM_RATIO, step = 0.05),
        tags$div(class = "muted-note", if (identical(lang, "zh")) sprintf("Noisy 使用上下文清洁标签规则：候选范围会被限制在绝对不应期到 Tonic 上限附近，并与 Pause 最小 ISI 保持至少 %.0f ms 的安全距离；单个 mode-like Noisy ISI 可以存在，但不能紧挨 Burst/Tonic，且禁止连续两个同类 mode-like Noisy。上下文保护带：%.0f ms。", 1000 * NOISY_PAUSE_GUARD_S, 1000 * NOISY_CONTEXT_GUARD_S) else sprintf("Contextual Noisy clean-label rules are enforced: Noisy candidates are clipped to the absolute-refractory-to-Tonic-upper envelope and kept at least %.0f ms below the Pause lower bound; a singleton mode-like Noisy ISI may exist, but it must not touch Burst/Tonic and two consecutive Noisy ISIs cannot occupy the same mode-like zone. Context guard: %.0f ms.", 1000 * NOISY_PAUSE_GUARD_S, 1000 * NOISY_CONTEXT_GUARD_S)),
        helpText(tr(lang, "noisy_mm_hint")),
        div(class = "control-grid",
            numericInput("noisy_isi_rho", tr(lang, "isi_rho"), value = input_value("noisy_isi_rho", 0), min = -0.95, max = 0.95, step = 0.05),
            numericInput("noisy_isi_trend", tr(lang, "isi_trend"), value = input_value("noisy_isi_trend", 0), min = -3, max = 3, step = 0.1)),
        helpText(tr(lang, "isi_temporal_hint")),
        colourInput("col_noisy_line", tr(lang, "line_color"), value = input_value("col_noisy_line", NATURE_PATTERN_COLORS["Noisy"]))
      )
    )
  })

  output$tabs_ui <- renderUI({
    lang <- current_lang()
    active_tab <- active_main_tab()
    if (!active_tab %in% main_tab_values) active_tab <- "summary"
    tabsetPanel(
      id = "main_tabs",
      selected = active_tab,
      type = "hidden",
      tabPanel(
        tr(lang, "tab_summary"),
        value = "summary",
        downloadButton("downloadReproduction", tr(lang, "download_reproduction_code"), class = "btn-success"),
        br(), br(),
        htmlOutput("param_summary")
      ),
      tabPanel(
        tr(lang, "tab_spike"),
        value = "spike",
        uiOutput("spike_resolution_ui"),
        uiOutput("spike_plot"),
        uiOutput("spike_window_ui"),
        downloadButton("downloadPlot", tr(lang, "download_plots"), class = "btn-success")
      ),
      tabPanel(
        tr(lang, "tab_dist"),
        value = "distributions",
        uiOutput("distribution_train_ui"),
        p(class = "muted-note", tr(lang, "distribution_scope_note")),
        div(class = "section-heading", tr(lang, "theory_heading")),
        div(
          class = "distribution-plot-grid",
          div(class = "distribution-plot-panel", plotOutput("theoretical_isi_plot", height = "320px")),
          div(class = "distribution-plot-panel", plotOutput("theoretical_isi_plot_b", height = "320px"))
        ),
        div(class = "section-heading", tr(lang, "empirical_heading")),
        checkboxInput(
          "show_target_density_overlay",
          tr(lang, "show_target_overlay"),
          value = input_value("show_target_density_overlay", TRUE)
        ),
        div(
          class = "distribution-plot-grid",
          div(class = "distribution-plot-panel", plotOutput("empirical_isi_plot", height = "320px")),
          div(class = "distribution-plot-panel", plotOutput("empirical_isi_plot_b", height = "320px"))
        ),
        downloadButton("downloadDistributionPlot", tr(lang, "download_distributions"), class = "btn-success")
      ),
      tabPanel(
        tr(lang, "tab_spike_data"),
        value = "spike_data",
        p(class = "muted-note", tr(lang, "spike_matrix_note")),
        uiOutput("spike_data_train_filter_ui"),
        downloadButton("downloadSpikeEvents", tr(lang, "download_spikes"), class = "btn-success"),
        downloadButton("downloadLatentDetectorInput", tr(lang, "download_latent_detector_input"), class = "btn-success"),
        downloadButton("downloadData", tr(lang, "download_spike_matrix"), class = "btn-success"),
        downloadButton("downloadPerTrainCsvZip", tr(lang, "download_per_train_csv_zip"), class = "btn-success"),
        downloadButton("downloadDetailedData", tr(lang, "download_spike_details"), class = "btn-success"),
        p(
          class = "muted-note",
          if (identical(lang, "zh")) {
            "审计 CSV 含有 ground truth，不应作为检测器输入；正式 benchmark 请使用 detector 可见输入 CSV。"
          } else {
            "Audit CSV files contain ground truth and should not be used as detector input; use detector-visible input CSVs for benchmarking."
          }
        ),
        br(), br(),
        DTOutput("spike_table"),
        div(class = "section-heading", tr(lang, "interval_table_heading")),
        DTOutput("interval_table")
      ),
      tabPanel(
        if (identical(lang, "zh")) "观测噪声" else "Observation noise",
        value = "observation",
        p(
          class = "muted-note",
          if (identical(lang, "zh")) {
            "潜在 spike 事件是模拟真值；观测 spike 事件是加入漏检、伪阳性、时间戳抖动和检测死区后的 detector 可见 spike train。观测映射表记录每个潜在 spike 是否被检出、漏检、被记录边界裁剪或被检测死区合并。"
          } else {
            "Latent spike events are the simulator ground truth; observed spike events include missed detections, false positives, timestamp jitter, and detector dead time. The observation map records whether each latent spike was detected, missed, clipped, or merged by dead time."
          }
        ),
        downloadButton("downloadObservedSpikeEvents", tr(lang, "download_observed_audit"), class = "btn-success"),
        downloadButton("downloadObservedDetectorInput", tr(lang, "download_observed_detector_input"), class = "btn-success"),
        downloadButton("downloadObservationMap", if (identical(lang, "zh")) "下载观测映射表 CSV" else "Download observation map CSV", class = "btn-success"),
        p(
          class = "muted-note",
          if (identical(lang, "zh")) {
            "观测审计和映射表含有 ground truth，仅用于评分和复现；检测器预测时只能读取 detector 可见观测输入。"
          } else {
            "Observed audit and mapping tables contain ground truth for scoring and reproducibility; detectors should read only the detector-visible observed input during prediction."
          }
        ),
        br(), br(),
        h4(if (identical(lang, "zh")) "观测噪声摘要" else "Observation summary"),
        plotOutput("observation_summary_plot", height = "280px"),
        DTOutput("observation_summary_table"),
        br(),
        h4(if (identical(lang, "zh")) "观测 spike 事件" else "Observed spike events"),
        DTOutput("observed_spike_table"),
        br(),
        h4(if (identical(lang, "zh")) "潜在到观测映射" else "Latent-to-observed map"),
        DTOutput("observation_map_table")
      ),
      tabPanel(
        if (identical(lang, "zh")) "验证与基准" else "Validation & benchmarks",
        value = "validation",
        p(class = "muted-note", if (identical(lang, "zh")) "直接调用核心生成器执行验证：不变量、目标分布、时序依赖、刺激响应、检测基准、观测噪声退化曲线，以及 Poisson / renewal / Markov 基线模型对比。建议先用较少随机种子做快速完整性检查，再增加样本量生成论文图表。" else "Runs validation blocks directly against the core simulator: invariants, target distributions, temporal dependence, stimulation responses, detector benchmark, observation-noise degradation, and Poisson / renewal / Markov baseline comparison. Start with a small seed count for smoke tests, then increase it for manuscript figures."),
        div(
          class = "control-grid",
	          selectInput(
	            "benchmark_preset",
	            if (identical(lang, "zh")) "基准预设" else "Benchmark preset",
	            choices = if (identical(lang, "zh")) c("自定义" = "custom", "简单" = "easy", "中等" = "moderate", "困难" = "hard") else c("Custom" = "custom", "Easy" = "easy", "Moderate" = "moderate", "Hard" = "hard"),
	            selected = input_value("benchmark_preset", "custom"),
	            selectize = FALSE
	          ),
	          selectInput(
	            "benchmark_task_mode",
	            if (identical(lang, "zh")) "基准模式" else "Benchmark mode",
		            choices = setNames(
		              c("clean", "realistic_stress"),
		              if (identical(lang, "zh")) c("清洁潜在间隔基准", "观测记录压力基准") else c("Clean latent benchmark", "Observed recording-stress benchmark")
		            ),
	            selected = input_value("benchmark_task_mode", "clean"),
	            selectize = FALSE
	          ),
	          selectInput(
	            "benchmark_package_type",
	            if (identical(lang, "zh")) "导出包类型" else "Package type",
	            choices = if (identical(lang, "zh")) {
	              c("完整复现包" = "complete", "预测包：仅公开输入" = "prediction", "评分包：真值与脚本" = "scoring")
	            } else {
	              c("Complete reproducibility package" = "complete", "Prediction package: public inputs only" = "prediction", "Scoring package: truth and scripts" = "scoring")
	            },
	            selected = input_value("benchmark_package_type", "complete"),
	            selectize = FALSE
	          ),
	          actionButton("apply_benchmark_preset", if (identical(lang, "zh")) "应用预设到当前模拟参数" else "Apply preset to simulator", class = "btn-secondary"),
	          downloadButton("downloadBenchmarkDataset", if (identical(lang, "zh")) "下载当前预设数据 ZIP" else "Download preset dataset ZIP", class = "btn-success")
	        ),
		        p(class = "muted-note", if (identical(lang, "zh")) "简单 / 中等 / 困难预设使用上下文清洁标签检测基准参数。清洁潜在间隔基准关闭观测噪声，主要用于 latent interval 协议；观测记录压力基准会自动加入温和漏检、时间戳抖动、伪阳性和检测死区，主要用于 observed time-overlap 协议。它是观测/记录层压力测试，不是生物学标签模糊压力测试。自定义模式只保留当前手动设置。" else "Easy / Moderate / Hard use contextual clean-label detector benchmark presets. Clean latent benchmark disables observation noise and targets the latent interval protocol; Observed recording-stress benchmark adds mild missed detections, timestamp jitter, false positives, and detector dead time for the observed time-overlap protocol. It is an observation/recording-layer stress test, not a biological label-ambiguity stress test. Custom keeps the current manual settings."),
        div(class = "control-grid",
            numericInput("validation_seed_base", if (identical(lang, "zh")) "验证 seed 起点" else "Validation seed base", value = input_value("validation_seed_base", 1), min = 1, step = 1),
            numericInput("validation_seed_count", if (identical(lang, "zh")) "每项 seed 数" else "Seeds per block", value = input_value("validation_seed_count", 3), min = 1, max = 100, step = 1),
            numericInput("validation_run_length", if (identical(lang, "zh")) "每个 label run 的 ISI 数" else "ISIs per label run", value = input_value("validation_run_length", 8), min = 4, max = 200, step = 1)
        ),
        actionButton("run_validation_suite", if (identical(lang, "zh")) "运行验证套件" else "Run validation suite", class = "btn-primary"),
        downloadButton("downloadValidationSuite", if (identical(lang, "zh")) "下载验证结果 CSV" else "Download validation CSV", class = "btn-success"),
        br(), br(),
        div(class = "section-heading", if (identical(lang, "zh")) "1. Simulator invariants" else "1. Simulator invariants"),
        DTOutput("validation_invariants_table"),
        div(class = "section-heading", if (identical(lang, "zh")) "2. 分布验证" else "2. Distribution validation"),
        DTOutput("validation_distribution_table"),
        div(class = "section-heading", if (identical(lang, "zh")) "3. 时序依赖验证" else "3. Temporal dependence validation"),
        DTOutput("validation_temporal_table"),
        div(class = "section-heading", if (identical(lang, "zh")) "4. 刺激响应预设验证：多 seed 汇总" else "4. Stimulation preset validation: multi-seed summary"),
        DTOutput("validation_stimulation_table"),
        div(class = "section-heading", if (identical(lang, "zh")) "4b. 刺激响应预设验证：逐 seed 原始结果" else "4b. Stimulation preset validation: per-seed raw results"),
        DTOutput("validation_stimulation_raw_table"),
        div(class = "section-heading", if (identical(lang, "zh")) "5. 检测基准" else "5. Detection benchmark"),
        DTOutput("validation_detection_table"),
        div(class = "section-heading", if (identical(lang, "zh")) "6. Baseline 对比" else "6. Baseline comparison"),
        DTOutput("validation_baseline_table")
      ),
      tabPanel(
        tr(lang, "tab_episode_data"),
        value = "episode_data",
        downloadButton("downloadEpisodes", tr(lang, "download_episodes"), class = "btn-success"),
        br(), br(),
        DTOutput("episode_table")
      ),
      tabPanel(
        if (identical(lang, "zh")) "刺激数据" else "Stimulus data",
        value = "stimulus_data",
        downloadButton("downloadUnits", if (identical(lang, "zh")) "下载神经元调谐表" else "Download unit tuning table", class = "btn-success"),
        downloadButton("downloadUnitStimulusDrive", if (identical(lang, "zh")) "下载 unit × stimulus drive 表" else "Download unit × stimulus drive table", class = "btn-success"),
        downloadButton("downloadStimuli", if (identical(lang, "zh")) "下载刺激表" else "Download stimulus table", class = "btn-success"),
	        downloadButton("downloadResponses", if (identical(lang, "zh")) "下载响应表" else "Download response table", class = "btn-success"),
	        downloadButton("downloadEventEpochs", if (identical(lang, "zh")) "下载事件 epoch 表" else "Download event epoch table", class = "btn-success"),
	        downloadButton("downloadNWBMapping", if (identical(lang, "zh")) "下载 NWB 映射 JSON" else "Download NWB mapping JSON", class = "btn-success"),
	        br(), br(),
	        p(
	          class = "muted-note",
	          if (identical(lang, "zh")) {
	            "事件 epoch 表记录刺激相关的非模式时间结构，例如响应潜伏期、burst 间隔、跨刺激间隔、诱发性抑制、恢复期和响应失败后的基线发放。这些 epoch 不等同于 Burst / Pause / Tonic / high_frequency_tonic / high_frequency_spiking / Noisy 发放模式标签。"
	          } else {
	            "The event epoch table stores stimulus-linked non-pattern timing structures, including response latency, interburst gaps, stimulus-spanning intervals, evoked suppression, recovery, and response-failure baseline epochs. These epochs are not the same as Burst / Pause / Tonic / high_frequency_tonic / high_frequency_spiking / Noisy firing-pattern labels."
	          }
	        ),
	        h4(if (identical(lang, "zh")) "刺激-响应摘要" else "Stimulus-response summary"),
        plotOutput("stimulus_response_summary_plot", height = "280px"),
        br(),
        h4(if (identical(lang, "zh")) "特征调谐映射图" else "Feature tuning map"),
        plotOutput("feature_tuning_map_plot", height = "360px"),
        br(),
        h4(if (identical(lang, "zh")) "神经元调谐表" else "Unit tuning table"),
        DTOutput("unit_table"),
        br(),
        h4(if (identical(lang, "zh")) "Unit × stimulus drive 表" else "Unit × stimulus drive table"),
        DTOutput("unit_stimulus_drive_table"),
        br(),
        h4(if (identical(lang, "zh")) "刺激表" else "Stimulus table"),
        DTOutput("stimulus_table"),
        br(),
        h4(if (identical(lang, "zh")) "响应表" else "Response table"),
        DTOutput("response_table"),
        br(),
        h4(if (identical(lang, "zh")) "事件 epoch 表" else "Event epoch table"),
        DTOutput("event_epoch_table")
      ),
      tabPanel(
        if (identical(lang, "zh")) "刺激对齐分析" else "Stimulus-aligned analysis",
        value = "stimulus_analysis",
        p(class = "muted-note", if (identical(lang, "zh")) "将 spike train 按刺激 onset 对齐，查看 trial-locked response、PSTH、响应潜伏期和重复刺激响应变化。" else "Align spike trains to stimulus onset to inspect trial-locked responses, PSTH, response latency, and repetition-dependent response metrics."),
        div(
          class = "control-grid",
          numericInput("stim_align_pre_s", if (identical(lang, "zh")) "刺激前窗口 (s)" else "Pre-stimulus window (s)", value = input_value("stim_align_pre_s", 1.0), min = 0, step = 0.1),
          numericInput("stim_align_post_s", if (identical(lang, "zh")) "刺激后窗口 (s)" else "Post-stimulus window (s)", value = input_value("stim_align_post_s", 1.5), min = 0.05, step = 0.1),
          numericInput("stim_psth_bin_s", if (identical(lang, "zh")) "PSTH bin 宽度 (s)" else "PSTH bin width (s)", value = input_value("stim_psth_bin_s", 0.05), min = 0.001, step = 0.005)
        ),
        h4(if (identical(lang, "zh")) "刺激对齐 raster" else "Stimulus-aligned raster"),
        plotOutput("stimulus_aligned_raster_plot", height = "360px"),
        h4(if (identical(lang, "zh")) "PSTH / 平均发放率" else "PSTH / mean firing rate"),
        plotOutput("stimulus_aligned_psth_plot", height = "300px"),
        h4(if (identical(lang, "zh")) "特征调谐映射图" else "Feature tuning map"),
        plotOutput("feature_tuning_map_plot_analysis", height = "360px"),
        h4(if (identical(lang, "zh")) "响应潜伏期分布" else "Response latency distribution"),
        plotOutput("stimulus_latency_hist_plot", height = "260px"),
        h4(if (identical(lang, "zh")) "刺激响应指标" else "Stimulus-response metrics"),
        plotOutput("stimulus_response_metric_plot", height = "300px")
      )
    )
  })


  stimulation_experiment_settings <- function(preset) {
    preset <- as.character(value_or(preset, "custom"))[1]
    common <- list(stim_enabled = TRUE, stim_start_s = 5, stim_n = 8, stim_isi_s = 3,
                   stim_duration_s = 0.05, stim_latency_median_s = 0.08, stim_latency_sdlog = 0.30,
                   stim_response_probability = 1.0, stim_pause_duration_cv = 0.35,
                   stim_post_burst_pause_probability = 0.0,
                   stim_baseline_recovery_enabled = TRUE, stim_baseline_recovery_mode = "Noisy",
                   stim_pre_stimulus_guard_s = 0.02,
                   stim_feature_modality = "orientation",
                   stim_feature_values = "15,45,90,135,180,225,270,315",
                   stim_feature_xy_values = "0,0; 25,0; 0,25; -25,0; 0,-25; 25,25; -25,25; 25,-25; -25,-25",
                   stim_place_field_x_min = -50, stim_place_field_x_max = 50,
                   stim_place_field_y_min = -50, stim_place_field_y_max = 50,
                   stim_place_field_center_x = 0, stim_place_field_center_y = 0,
                   stim_place_field_width = 18, stim_place_field_radius = 45,
                   stim_preferred_feature = 15, stim_null_feature = 90,
                   stim_feature_period = 180,
                   stim_feature_tuning_width = 25, stim_feature_suppression_width = 25,
                   stim_feature_min_gain = 0.05,
                   stim_feature_population_mode = "coverage_balanced_population",
                   stim_feature_responsive_fraction = 0.35,
                   stim_feature_suppressive_fraction = 0.10,
                   stim_feature_biphasic_fraction = 0.05,
                   stim_feature_response_threshold = 0.35,
                   stim_feature_preferred_response = "excitatory_burst",
                   stim_feature_null_response = "no_response",
                   stim_feature_population_jitter = 0.25,
                   stim_feature_unit_max_gain = 1.0,
                   stim_feature_unit_response_reliability = 1.0,
                   stim_feature_target_unit = 1,
                   stim_adaptation_enabled = TRUE, stim_adaptation_increment = 0.35,
                   stim_adaptation_tau_s = 12, stim_response_floor = 0.15)
    if (identical(preset, "intensity_response")) {
      modifyList(common, list(stim_protocol = "intensity_ramp", stim_response_type = "excitatory_burst",
                              stim_n = 8, stim_isi_s = 3.0, stim_strength = 0.2, stim_strength_end = 1.0,
                              stim_response_window_s = 1.05, stim_max_evoked_bursts = 4,
                              stim_burst_spike_min = 3, stim_burst_spike_max = 4,
                              stim_burst_lambda_base = 0.60, stim_burst_lambda_strength = 4.0,
                              stim_post_burst_pause_probability = 0.0))
    } else if (identical(preset, "repeated_adaptation")) {
      modifyList(common, list(stim_protocol = "repeated", stim_response_type = "excitatory_burst",
                              stim_n = 20, stim_isi_s = 1.0, stim_strength = 0.75, stim_strength_end = 0.75,
                              stim_response_window_s = 0.65, stim_adaptation_increment = 0.35,
                              stim_adaptation_tau_s = 3.0, stim_response_floor = 0.25,
                              stim_max_evoked_bursts = 3, stim_burst_lambda_base = 0.35,
                              stim_burst_lambda_strength = 2.8, stim_burst_spike_min = 3,
                              stim_burst_spike_max = 4))
    } else if (identical(preset, "stimulus_suppression")) {
      modifyList(common, list(stim_protocol = "regular", stim_response_type = "suppressive_pause",
                              stim_n = 10, stim_isi_s = 3.0, stim_strength = 0.8, stim_strength_end = 0.8,
                              stim_latency_median_s = 0.09, stim_latency_sdlog = 0.20,
                              stim_response_probability = 1.0,
                              stim_pause_min_s = 0.15, stim_pause_max_s = 0.30,
                              stim_pause_duration_cv = 0.20, stim_response_window_s = 0.55,
                              stim_adaptation_increment = 0.10, stim_response_floor = 0.4))
    } else if (identical(preset, "biphasic_burst_pause")) {
      modifyList(common, list(stim_protocol = "regular", stim_response_type = "biphasic",
                              stim_n = 10, stim_isi_s = 3.0, stim_strength = 0.75,
                              stim_latency_median_s = 0.06, stim_latency_sdlog = 0.18,
                              stim_max_evoked_bursts = 2, stim_burst_lambda_base = 0.35,
                              stim_burst_lambda_strength = 2.2, stim_burst_spike_min = 3,
                              stim_burst_spike_max = 4, stim_post_burst_pause_probability = 1.0,
                              stim_pause_min_s = 0.18, stim_pause_max_s = 0.32,
                              stim_pause_duration_cv = 0.20, stim_response_window_s = 0.95))
    } else if (identical(preset, "paired_pulse_recovery")) {
      modifyList(common, list(stim_protocol = "paired_pulse", stim_response_type = "excitatory_burst",
                              stim_n = 12, stim_isi_s = 5.0, stim_pair_interval_s = 0.6,
                              stim_strength = 0.80, stim_response_window_s = 0.80,
                              stim_adaptation_increment = 0.65,
                              stim_adaptation_tau_s = 1.5, stim_response_floor = 0.25,
                              stim_max_evoked_bursts = 3, stim_burst_lambda_base = 0.35,
                              stim_burst_lambda_strength = 3.0, stim_burst_spike_min = 3,
                              stim_burst_spike_max = 4))
    } else if (identical(preset, "oddball_adaptation")) {
      modifyList(common, list(stim_protocol = "oddball", stim_response_type = "excitatory_burst",
                              stim_n = 36, stim_isi_s = 0.5, stim_strength = 0.55,
                              stim_response_window_s = 0.42,
                              stim_deviant_probability = 0.1, stim_deviant_strength = 1.0,
                              stim_adaptation_increment = 0.35, stim_adaptation_tau_s = 2.5,
                              stim_response_floor = 0.25, stim_max_evoked_bursts = 3,
                              stim_burst_lambda_base = 0.25, stim_burst_lambda_strength = 2.6,
                              stim_burst_spike_min = 4, stim_burst_spike_max = 4))
    } else if (identical(preset, "state_dependent")) {
      modifyList(common, list(stim_protocol = "regular", stim_response_type = "state_dependent",
                              stim_n = 12, stim_isi_s = 2.5, stim_strength = 0.7,
                              stim_response_window_s = 0.80,
                              stim_pause_min_s = 0.15, stim_pause_max_s = 0.35,
                              stim_pause_duration_cv = 0.30,
                              stim_post_burst_pause_probability = 0.4, stim_rebound_probability = 0.4,
                              stim_adaptation_increment = 0.20, stim_adaptation_tau_s = 4.0,
                              stim_response_floor = 0.30))
    } else if (identical(preset, "feature_tuning")) {
      modifyList(common, list(stim_protocol = "feature_tuning", stim_response_type = "feature_tuned",
                              spike_train_number = 8,
                              total_time = 30,
                              stim_n = 16, stim_isi_s = 1.5, stim_strength = 0.95,
                              stim_strength_end = 0.95, stim_response_window_s = 1.25,
                              stim_latency_median_s = 0.07, stim_latency_sdlog = 0.20,
                              stim_feature_modality = "motion_direction",
                              stim_feature_values = "0,45,90,135,180,225,270,315,0,45,90,135,180,225,270,315,0,45,90,135,180,225,270,315",
                              stim_preferred_feature = 0, stim_null_feature = 180,
                              stim_feature_period = 360,
                              stim_feature_tuning_width = 28,
                              stim_feature_suppression_width = 32,
                              stim_feature_min_gain = 0.03,
                              stim_feature_population_mode = "coverage_balanced_population",
                              stim_feature_responsive_fraction = 1.00,
                              stim_feature_suppressive_fraction = 0.00,
                              stim_feature_biphasic_fraction = 0.00,
                              stim_feature_response_threshold = 0.25,
                              stim_feature_preferred_response = "excitatory_burst",
                              stim_feature_null_response = "no_response",
                              stim_feature_population_jitter = 0.05,
                              stim_feature_unit_max_gain = 1.25,
                              stim_feature_unit_response_reliability = 1.0,
                              stim_max_evoked_bursts = 4,
                              stim_burst_lambda_base = 0.25, stim_burst_lambda_strength = 3.4,
                              stim_burst_spike_min = 4, stim_burst_spike_max = 6,
                              stim_pause_min_s = 0.75, stim_pause_max_s = 1.10,
                              stim_pause_duration_cv = 0.15,
                              stim_post_burst_pause_probability = 0.05,
                              stim_adaptation_increment = 0.15,
                              stim_adaptation_tau_s = 4.0,
                              stim_response_floor = 0.20))
    } else {
      list()
    }
  }

  observeEvent(input$stim_experiment_preset, {
    preset <- input_value("stim_experiment_preset", "custom")
    if (identical(preset, "custom")) return(invisible(FALSE))
    settings <- stimulation_experiment_settings(preset)
    if (length(settings) == 0) return(invisible(FALSE))
    for (id in names(settings)) {
      val <- settings[[id]]
      if (is.logical(val)) {
        updateCheckboxInput(session, id, value = isTRUE(val))
      } else if (id %in% c("stim_feature_values", "stim_feature_xy_values", "stim_manual_times", "stim_manual_strengths")) {
        updateTextInput(session, id, value = as.character(val))
      } else if (is.character(val)) {
        updateSelectInput(session, id, selected = val)
      } else {
        updateNumericInput(session, id, value = val)
      }
    }
    showNotification(if (identical(current_lang(), "zh")) "刺激实验预设已应用。" else "Stimulation experiment preset applied.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$stim_feature_modality, {
    modality <- as.character(input_value("stim_feature_modality", "orientation"))[1]
    defaults <- switch(
      modality,
      auditory_frequency = list(values = "1000,2000,4000,8000,12000,16000,4000,12000",
                                preferred = 4000, null = 12000, period = 0,
                                width = 0.45, suppression_width = 0.45),
      motion_direction = list(values = "0,45,90,135,180,225,270,315",
                              preferred = 0, null = 180, period = 360,
                              width = 28, suppression_width = 32),
      color_hue = list(values = "0,45,90,135,180,225,270,315",
                       preferred = 0, null = 180, period = 360,
                       width = 35, suppression_width = 35),
      spatial_position = list(values = "-40,-20,0,20,40,0,-30,30",
                              preferred = 0, null = 40, period = 0,
                              width = 15, suppression_width = 15),
      spatial_2d = list(values = "0", xy_values = "0,0; 25,0; 0,25; -25,0; 0,-25; 25,25; -25,25; 25,-25; -25,-25",
                        preferred = 0, null = 0, period = 0,
                        width = 18, suppression_width = 18,
                        center_x = 0, center_y = 0, radius = 45),
      tactile_location = list(values = "1,2,3,4,5,1,3,5",
                              preferred = 1, null = 5, period = 0,
                              width = 1.0, suppression_width = 1.0),
      list(values = "15,45,90,135,180,225,270,315",
           preferred = 15, null = 90, period = 180,
           width = 25, suppression_width = 25)
    )
    updateTextInput(session, "stim_feature_values", value = defaults$values)
    if (!is.null(defaults$xy_values)) updateTextInput(session, "stim_feature_xy_values", value = defaults$xy_values)
    if (!is.null(defaults$center_x)) updateNumericInput(session, "stim_place_field_center_x", value = defaults$center_x)
    if (!is.null(defaults$center_y)) updateNumericInput(session, "stim_place_field_center_y", value = defaults$center_y)
    if (!is.null(defaults$radius)) updateNumericInput(session, "stim_place_field_radius", value = defaults$radius)
    updateNumericInput(session, "stim_preferred_feature", value = defaults$preferred)
    updateNumericInput(session, "stim_null_feature", value = defaults$null)
    updateNumericInput(session, "stim_feature_period", value = defaults$period)
    updateNumericInput(session, "stim_feature_tuning_width", value = defaults$width)
    updateNumericInput(session, "stim_feature_suppression_width", value = defaults$suppression_width)
  }, ignoreInit = TRUE)

  apply_reproduction_settings <- function(settings) {
    spec <- reproduction_input_spec()
    settings <- settings[intersect(names(settings), names(spec))]

    for (id in names(settings)) {
      value <- coerce_reproduction_value(settings[[id]], spec[[id]])
      type <- spec[[id]]$type
      if (type %in% c("text", "color")) {
        if (identical(type, "color") && exists("updateColourInput", mode = "function")) {
          updateColourInput(session, id, value = value)
        } else {
          updateTextInput(session, id, value = value)
        }
      } else if (identical(type, "select")) {
        updateSelectInput(session, id, selected = value)
      } else if (identical(type, "logical")) {
        updateCheckboxInput(session, id, value = isTRUE(value))
      } else if (identical(type, "numeric")) {
        updateNumericInput(session, id, value = value)
      } else if (identical(type, "numeric_vector")) {
        updateSliderInput(session, id, value = as.numeric(value))
      }
    }

    if ("spike_train_number" %in% names(settings)) {
      n_train <- max(1L, as.integer(coerce_reproduction_value(settings$spike_train_number, spec$spike_train_number)))
      train_choices <- as.character(seq_len(n_train))
      updateSelectInput(session, "selected_trains", choices = train_choices, selected = head(train_choices, 10L))
    }
  }

  valid_benchmark_preset <- function(difficulty) {
    difficulty %in% c("easy", "moderate", "hard")
  }

	  selected_benchmark_preset <- function() {
	    tolower(as.character(input_value("benchmark_preset", "custom"))[1])
	  }

	  selected_benchmark_task_mode <- function() {
	    mode <- tolower(as.character(input_value("benchmark_task_mode", "clean"))[1])
	    if (!mode %in% c("clean", "realistic_stress")) mode <- "clean"
	    mode
	  }

	  apply_benchmark_task_mode <- function(cfg, mode = "clean") {
	    mode <- tolower(as.character(value_or(mode, "clean"))[1])
	    if (!mode %in% c("clean", "realistic_stress")) mode <- "clean"
	    cfg$benchmark_task_mode <- mode
	    if (identical(mode, "realistic_stress")) {
	      cfg$benchmark_observation_profile <- "mild_recording_and_sorting_noise"
	      cfg$observation <- list(
	        enabled = TRUE,
	        detection_probability = 0.95,
	        false_positive_rate_hz = 0.25,
	        jitter_sd_s = 0.00025,
	        time_bias_s = 0,
	        dead_time_s = 0.0006,
	        seed_offset = 200000L,
	        mode = "bernoulli_detection_plus_false_positives"
	      )
	    } else {
	      cfg$benchmark_observation_profile <- "identity_observation_clean_labels"
	      cfg$observation <- list(
	        enabled = FALSE,
	        detection_probability = 1,
	        false_positive_rate_hz = 0,
	        jitter_sd_s = 0,
	        time_bias_s = 0,
	        dead_time_s = 0,
	        seed_offset = 200000L,
	        mode = "identity"
	      )
	    }
	    cfg
	  }

	  benchmark_config_from_ui <- function(difficulty) {
	    difficulty <- match.arg(tolower(as.character(difficulty)[1]), c("easy", "moderate", "hard"))
	    base_config <- build_sim_config(NULL)
	    cfg <- make_detection_benchmark_config(base_config, difficulty)
	    apply_benchmark_task_mode(cfg, selected_benchmark_task_mode())
	  }

  set_distribution_settings_from_config <- function(settings, prefix, pat_cfg) {
    dist_type <- as.character(value_or(pat_cfg$dist_type, "Uniform"))
    settings[[paste0("dist_", prefix)]] <- dist_type
    params <- pat_cfg$params
    if (is.null(params)) params <- list()
    rng <- as.numeric(value_or(pat_cfg$interval_range, c(NA_real_, NA_real_)))
    if (length(rng) < 2 || any(!is.finite(rng[1:2]))) rng <- c(NA_real_, NA_real_)

    if (identical(dist_type, "Exponential")) {
      settings[[paste0(prefix, "_exp_mean")]] <- safe_num(params$mean, settings[[paste0(prefix, "_exp_mean")]])
    } else if (identical(dist_type, "Gamma")) {
      settings[[paste0(prefix, "_gamma_shape")]] <- safe_num(params$shape, settings[[paste0(prefix, "_gamma_shape")]])
      settings[[paste0(prefix, "_gamma_scale")]] <- safe_num(params$scale, settings[[paste0(prefix, "_gamma_scale")]])
    } else if (identical(dist_type, "Normal")) {
      settings[[paste0(prefix, "_norm_mean")]] <- safe_num(params$mean, settings[[paste0(prefix, "_norm_mean")]])
      settings[[paste0(prefix, "_norm_sd")]] <- safe_num(params$sd, settings[[paste0(prefix, "_norm_sd")]])
    } else if (identical(dist_type, "Lognormal")) {
      settings[[paste0(prefix, "_lognorm_meanlog")]] <- safe_num(params$meanlog, settings[[paste0(prefix, "_lognorm_meanlog")]])
      settings[[paste0(prefix, "_lognorm_sdlog")]] <- safe_num(params$sdlog, settings[[paste0(prefix, "_lognorm_sdlog")]])
    } else if (identical(dist_type, "Inverse Gaussian")) {
      settings[[paste0(prefix, "_invgauss_mean")]] <- safe_num(params$mean, settings[[paste0(prefix, "_invgauss_mean")]])
      settings[[paste0(prefix, "_invgauss_shape")]] <- safe_num(params$shape, settings[[paste0(prefix, "_invgauss_shape")]])
    } else if (identical(dist_type, "Uniform")) {
      settings[[paste0(prefix, "_unif_min")]] <- safe_num(params$min, rng[1])
      settings[[paste0(prefix, "_unif_max")]] <- safe_num(params$max, rng[2])
    }

    settings
  }

	  benchmark_settings_from_config <- function(cfg) {
	    settings <- collect_reproduction_settings()
	    settings$benchmark_task_mode <- as.character(value_or(cfg$benchmark_task_mode, "clean"))
	    settings$generation_mode <- as.character(value_or(cfg$generation_mode, "event"))
	    settings$pattern_sequence <- ""
	    settings$total_time <- safe_num(cfg$total_time, settings$total_time)
    settings$inter_event_gap <- safe_num(cfg$inter_event_gap, settings$inter_event_gap)
    settings$auto_inter_event_gap <- FALSE
    settings$leading_silence_initial_pause <- isTRUE(cfg$leading_silence_initial_pause)
    settings$avoid_noisy_burst_runs <- isTRUE(cfg$avoid_noisy_burst_runs)

    ratios <- normalize_pattern_ratios(cfg$ratios)
    settings$ratio_burst <- 100 * safe_num(ratios["Burst"], 0.25)
    settings$ratio_pause <- 100 * safe_num(ratios["Pause"], 0.25)
    settings$ratio_tonic <- 100 * safe_num(ratios["Tonic"], 0.25)
    settings$ratio_hft <- 100 * safe_num(ratios["high_frequency_tonic"], 0)
    settings$ratio_hfs <- 100 * safe_num(ratios["high_frequency_spiking"], 0)
    settings$ratio_noisy <- 100 * safe_num(ratios["Noisy"], 0.25)

	    spec <- noisy_specificity_from_config(cfg)
	    settings$noisy_mm_ratio <- spec$mm_ratio
	    settings$noisy_avoid_mode_overlap <- isTRUE(spec$avoid_mode_overlap)

	    obs <- if (!is.null(cfg$observation)) cfg$observation else list()
	    settings$obs_enabled <- isTRUE(obs$enabled)
	    settings$obs_detection_probability <- safe_num(obs$detection_probability, if (isTRUE(obs$enabled)) 0.95 else 1)
	    settings$obs_false_positive_rate_hz <- safe_num(obs$false_positive_rate_hz, 0)
	    settings$obs_jitter_sd_ms <- 1000 * safe_num(obs$jitter_sd_s, 0)
	    settings$obs_time_bias_ms <- 1000 * safe_num(obs$time_bias_s, 0)
	    settings$obs_dead_time_ms <- 1000 * safe_num(obs$dead_time_s, 0)
	    settings$obs_seed_offset <- as.integer(round(safe_num(obs$seed_offset, 200000)))

	    pattern_map <- list(
      Burst = list(prefix = "burst", interval_id = "interval_range_burst", spike_id = "spike_range_burst", rho_id = "burst_isi_rho", trend_id = "burst_isi_trend"),
      Pause = list(prefix = "pause", interval_id = "pause_duration_range", spike_id = NULL, rho_id = "pause_isi_rho", trend_id = "pause_isi_trend"),
      Tonic = list(prefix = "tonic", interval_id = "interval_range_tonic", spike_id = "spike_range_tonic", rho_id = "tonic_isi_rho", trend_id = "tonic_isi_trend"),
      high_frequency_tonic = list(prefix = "hft", interval_id = "interval_range_hft", spike_id = "spike_range_hft", rho_id = "hft_isi_rho", trend_id = "hft_isi_trend"),
      high_frequency_spiking = list(prefix = "hfs", interval_id = "interval_range_hfs", spike_id = "spike_range_hfs", rho_id = "hfs_isi_rho", trend_id = "hfs_isi_trend"),
      Noisy = list(prefix = "noisy", interval_id = "interval_range_noisy", spike_id = "spike_range_noisy", rho_id = "noisy_isi_rho", trend_id = "noisy_isi_trend")
    )

    for (pattern in names(pattern_map)) {
      pat_cfg <- cfg$patterns[[pattern]]
      if (is.null(pat_cfg)) next
      map <- pattern_map[[pattern]]
      settings <- set_distribution_settings_from_config(settings, map$prefix, pat_cfg)
      if (!is.null(pat_cfg$interval_range)) settings[[map$interval_id]] <- as.numeric(pat_cfg$interval_range)
      if (!is.null(map$spike_id) && !is.null(pat_cfg$spike_count_range)) settings[[map$spike_id]] <- as.numeric(pat_cfg$spike_count_range)
      temporal <- pat_cfg$temporal_dependence
      settings[[map$rho_id]] <- safe_num(temporal$rho, settings[[map$rho_id]])
      settings[[map$trend_id]] <- safe_num(temporal$trend, settings[[map$trend_id]])
    }

    tonic_reg <- cfg$patterns$Tonic$regularity_ranges
    if (!is.null(tonic_reg$cv)) settings$tonic_cv_range <- as.numeric(tonic_reg$cv)
    if (!is.null(tonic_reg$cv2)) settings$tonic_cv2_range <- as.numeric(tonic_reg$cv2)
    if (!is.null(tonic_reg$lv)) settings$tonic_lv_range <- as.numeric(tonic_reg$lv)

    hft_reg <- cfg$patterns$high_frequency_tonic$regularity_ranges
    if (!is.null(hft_reg$cv)) settings$hft_cv_range <- as.numeric(hft_reg$cv)
    if (!is.null(hft_reg$cv2)) settings$hft_cv2_range <- as.numeric(hft_reg$cv2)
    if (!is.null(hft_reg$lv)) settings$hft_lv_range <- as.numeric(hft_reg$lv)
    if (!is.null(hft_reg$mm)) settings$hft_mm_range <- as.numeric(hft_reg$mm)

    hfs_rules <- cfg$patterns$high_frequency_spiking$state_rules
    if (!is.null(hfs_rules$short_isi_range)) settings$hfs_short_isi_range <- as.numeric(hfs_rules$short_isi_range)
    if (!is.null(hfs_rules$bridge_isi_range)) settings$hfs_bridge_isi_range <- as.numeric(hfs_rules$bridge_isi_range)
    settings$hfs_target_short_fraction <- safe_num(hfs_rules$target_short_fraction, settings$hfs_target_short_fraction)
    settings$hfs_short_fraction_min <- safe_num(hfs_rules$short_fraction_min, settings$hfs_short_fraction_min)
    settings$hfs_bridge_fraction_max <- safe_num(hfs_rules$bridge_fraction_max, settings$hfs_bridge_fraction_max)
    settings$hfs_max_consecutive_bridge <- as.integer(round(safe_num(hfs_rules$max_consecutive_bridge, settings$hfs_max_consecutive_bridge)))
    settings$hfs_min_duration <- safe_num(hfs_rules$min_duration_s, settings$hfs_min_duration)

    settings
  }

  observeEvent(input$apply_benchmark_preset, {
    if (is.null(input$apply_benchmark_preset) || input$apply_benchmark_preset < 1) {
      return(invisible(FALSE))
    }
    lang <- current_lang()
    difficulty <- selected_benchmark_preset()
    if (!valid_benchmark_preset(difficulty)) {
      showNotification(if (identical(lang, "zh")) "请先选择简单、中等或困难基准预设。" else "Choose Easy, Moderate, or Hard first.", type = "warning")
      return(invisible(FALSE))
    }

    cfg <- benchmark_config_from_ui(difficulty)
    settings <- benchmark_settings_from_config(cfg)
    apply_reproduction_settings(settings)
    updateCheckboxInput(session, "auto_inter_event_gap", value = FALSE)
    updateNumericInput(session, "inter_event_gap", value = settings$inter_event_gap)
    difficulty_label_zh <- c(easy = "简单", moderate = "中等", hard = "困难")[[difficulty]]
    if (is.null(difficulty_label_zh) || is.na(difficulty_label_zh)) difficulty_label_zh <- difficulty
    showNotification(
      if (identical(lang, "zh")) paste0("已应用", difficulty_label_zh, "基准预设。") else paste0("Applied ", tools::toTitleCase(difficulty), " benchmark preset."),
      type = "message"
    )
    invisible(TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input$load_reproduction_code, {
    lang <- current_lang()
    result <- tryCatch({
      payload <- decode_reproduction_code(input$reproduction_code_input)
      apply_reproduction_settings(payload$settings)
      loaded_reproduction_expected(payload$expected)
      loaded_reproduction_config(reproduction_payload_config(payload))
      showNotification(tr(lang, "reproduction_loaded"), type = "message")
      TRUE
    }, error = function(err) {
      loaded_reproduction_expected(NULL)
      loaded_reproduction_config(NULL)
      showNotification(tr(lang, "err_reproduction_code"), type = "error")
      FALSE
    })
    result
  })

  output$spike_window_ui <- renderUI({
    lang <- current_lang()
    total_time <- active_total_time()
    visible_seconds <- normalize_spike_visible_seconds(isolate(input$spike_visible_seconds), total_time = total_time)
    min_visible <- min(total_time, max(0.001, total_time / 1000))

    div(
      class = "time-window-control",
      sliderInput(
        "spike_visible_seconds",
        tr(lang, "spike_window"),
        min = min_visible,
        max = total_time,
        value = visible_seconds,
        step = max(total_time / 1000, 0.001),
        ticks = FALSE,
        width = "100%"
      ),
      helpText(tr(lang, "spike_window_hint"))
    )
  })

  output$distribution_train_ui <- renderUI({
    lang <- current_lang()
    sim <- if (!is.null(input$run) && input$run > 0) {
      tryCatch(all_spike_trains(), error = function(err) NULL)
    } else {
      NULL
    }
    n_train <- if (!is.null(sim)) generated_train_count(sim) else current_train_count()
    train_choices <- train_choices_for_sim(n_train, sim, lang)

    div(
      class = "distribution-control-grid",
      selectInput(
        "distribution_train",
        tr(lang, "distribution_train_a"),
        choices = train_choices,
        selected = as.character(selected_distribution_train(n_train, reactive = FALSE, default_train = 1L)),
        multiple = FALSE,
        selectize = FALSE,
        width = "100%"
      ),
      selectInput(
        "distribution_train_b",
        tr(lang, "distribution_train_b"),
        choices = train_choices,
        selected = as.character(selected_distribution_train(n_train, reactive = FALSE, input_id = "distribution_train_b", default_train = min(2L, n_train))),
        multiple = FALSE,
        selectize = FALSE,
        width = "100%"
      )
    )
  })

  output$spike_resolution_ui <- renderUI({
    lang <- current_lang()
    state <- spike_resolution_state()
    if (!is.finite(state$min_isi) || !is.finite(state$ms_per_pixel)) return(NULL)

    note <- sprintf(
      tr(lang, "spike_resolution_hint"),
      state$ms_per_pixel,
      state$min_isi * 1000
    )

    if (isTRUE(state$can_resolve)) {
      div(
        class = "resolution-note",
        div(note),
        div(tr(lang, "spike_resolution_ok"))
      )
    } else {
      div(
        class = "resolution-note warn",
        div(note),
        div(sprintf(tr(lang, "spike_resolution_warning"), state$recommended_window)),
        actionButton("fit_resolution_window", tr(lang, "fit_resolution_window"), class = "btn btn-outline-secondary btn-sm")
      )
    }
  })

  observeEvent(input$fit_resolution_window, {
    state <- spike_resolution_state()
    total_time <- active_total_time()
    if (!is.finite(state$recommended_window) || state$recommended_window <= 0) return(NULL)

    width <- min(total_time, state$recommended_window)
    updateSliderInput(session, "spike_visible_seconds", value = width)
  })

  translate_names <- function(cols, labels) {
    out <- labels[cols]
    missing <- is.na(out)
    out[missing] <- cols[missing]
    unname(out)
  }

  diagnostic_pattern_label <- function(pattern, lang) {
    if (!identical(lang, "zh")) return(pattern)
    labels <- c(
      Burst = "爆发",
      Pause = "暂停",
      Tonic = "节律",
      Noisy = "噪声",
      Latency = "潜伏期",
      Interval = "区间"
    )
    translated <- unname(labels[pattern])
    value_or(translated, pattern)
  }

  diagnostic_latency_model_label <- function(model, lang) {
    if (!identical(lang, "zh")) return(model)
    labels <- c(
      residual_life = "残余寿命 / 平衡 renewal",
      same_distribution = "与首个 ISI 标签同分布",
      uniform = "均匀分布",
      specified_duration = "指定时长",
      pause_distribution_duration = "暂停分布采样时长"
    )
    translated <- unname(labels[model])
    value_or(translated, model)
  }

  diagnostic_match <- function(x, pattern) {
    m <- regexec(pattern, x, perl = TRUE)
    hit <- regmatches(x, m)[[1]]
    if (length(hit) == 0) return(NULL)
    hit[-1]
  }

  diagnostic_translate_fallback_zh <- function(x) {
    replacements <- c(
      "absolute refractory period" = "绝对不应期",
      "contextual Noisy clean-label" = "上下文噪声清洁标签",
      "clean-label" = "清洁标签",
      "accepted range" = "接受范围",
      "accepted-range" = "接受范围",
      "adjacency rules" = "邻接规则",
      "feasible interval segment" = "可行 ISI 区段",
      "feasible segment set" = "可行区段集合",
      "positive probability mass" = "正概率质量",
      "probability mass" = "概率质量",
      "interval distribution" = "ISI 分布",
      "interval sampler" = "ISI 采样器",
      "truncated interval sampler" = "截断 ISI 采样器",
      "initial latency" = "初始潜伏期",
      "recording boundary" = "记录边界",
      "interval table" = "interval 表",
      "biological ISI" = "生物学 ISI",
      "labeled ISI" = "带标签 ISI",
      "interval-label" = "ISI 标签",
      "label run" = "标签 run",
      "spike-count range" = "spike 数范围",
      "remaining time" = "剩余时间",
      "automatic-run safety cap" = "自动 run 安全上限",
      "simulation may be slow or short" = "模拟可能变慢，或生成时长不足",
      "not satisfied" = "未满足",
      "adjust distribution parameters or accepted ranges" = "请调整分布参数或接受范围",
      "Relax tonic regularity ranges or adjust the tonic ISI distribution" = "请放宽节律规则性范围，或调整节律 ISI 分布"
    )
    for (needle in names(replacements)) {
      x <- gsub(needle, replacements[[needle]], x, fixed = TRUE)
    }
    x <- gsub("\\bBurst\\b", "爆发", x, perl = TRUE)
    x <- gsub("\\bPause\\b", "暂停", x, perl = TRUE)
    x <- gsub("\\bTonic\\b", "节律", x, perl = TRUE)
    x <- gsub("\\bNoisy\\b", "噪声", x, perl = TRUE)
    x <- gsub("\\bLatency\\b", "潜伏期", x, perl = TRUE)
    x <- gsub("\\bSpike train\\b", "Spike train", x, perl = TRUE)
    x
  }

  translate_diagnostic_message <- function(message, lang) {
    message <- as.character(value_or(message, ""))
    if (!identical(lang, "zh") || !nzchar(message)) return(message)

    m <- diagnostic_match(message, "^Spike train ([0-9]+):\\s*(.*)$")
    if (!is.null(m)) {
      return(sprintf("Spike train %s：%s", m[1], translate_diagnostic_message(m[2], lang)))
    }

    exact <- c(
      "Total time must be a positive finite number." = "总时长必须是正的有限数。",
      "Absolute refractory period must be a non-negative finite number." = "绝对不应期必须是非负有限数。",
      "All six pattern configurations must be present." = "六类模式配置必须全部存在。",
      "Initial latency model must be one of: residual_life, same_distribution, uniform." = "初始潜伏期模型必须为 residual_life、same_distribution 或 uniform 之一。",
      "Noisy adjacency MM threshold must be greater than 1." = "噪声邻接 MM 阈值必须大于 1。",
      "Tonic CV/CV2/LV ranges must be finite non-negative ranges." = "节律模式的 CV/CV2/LV 范围必须是非负有限区间。",
      "Noisy interval sampler produced 3 consecutive burst-like ISIs despite constructive exclusion; this indicates an infeasible Noisy range after constraints." = "噪声 ISI 采样器在构造性排除后仍产生了 3 个连续 burst-like ISI；这说明当前约束下噪声范围不可行。",
      "Noisy contextual clean-label rule rejected a Noisy run: a Noisy singleton may be Burst-like/Tonic-like only when isolated, but consecutive same-zone Noisy ISIs, Pause-like Noisy ISIs, or overly regular longer Noisy runs are not allowed." = "噪声上下文清洁标签规则拒绝了一个噪声 run：单个噪声 ISI 可以在孤立条件下呈 burst-like 或 tonic-like，但不允许连续同区间噪声 ISI、pause-like 噪声 ISI，或过于规则的长噪声 run。",
      "Tonic regularity metrics require at least two tonic-labeled ISIs; increase the tonic spike-count range." = "节律规则性指标至少需要 2 个节律标签 ISI；请增大节律 spike 数范围。",
      "No feasible interval label with a positive ratio was available under the current duration, range, and adjacency rules." = "在当前总时长、范围和邻接规则下，没有任何具有正比例的 ISI 标签可行。",
      "The first real spike was generated after a positive initial latency; t = 0 is treated as the recording boundary, not as a spike. This latency is not entered into the interval table as an ISI." = "第一个真实 spike 在一个正的初始潜伏期后生成；t = 0 被视为记录边界，而不是 spike。该潜伏期不会作为 ISI 写入 interval 表。",
      "Initial Pause was treated as leading silence by default: the recording boundary at 0 s is not a spike, so this first duration is recorded as latency rather than as a biological ISI and is not entered into the interval table." = "初始 Pause 默认按起始静默处理：0 s 记录边界不是 spike，因此第一段时长记录为潜伏期，而不是生物学 ISI，也不会写入 interval 表。",
      "Observation detection probability must be finite and within [0, 1]." = "观测层真实 spike 检出概率必须有限，并位于 [0, 1]。",
      "Observation false-positive rate must be a finite non-negative number." = "观测层伪阳性率必须是非负有限数。",
      "Observation false-positive settings are expected to create more than 100000 false events per train; simulation may be slow or memory-intensive." = "当前观测层伪阳性参数预计每条 spike train 产生超过 100000 个伪阳性事件；模拟可能变慢或占用较多内存。",
      "Observation timestamp jitter SD must be finite and non-negative." = "观测层时间戳抖动 SD 必须是非负有限数。",
      "Observation timestamp bias must be finite." = "观测层时间戳系统偏移必须有限。",
      "Observation detector dead time must be finite and non-negative." = "观测层 detector dead time / 合并窗口必须是非负有限数。",
      "Observation seed offset must be a positive integer." = "观测层 seed offset 必须是正整数。"
    )
    if (message %in% names(exact)) return(unname(exact[[message]]))

    m <- diagnostic_match(message, "^(.+) Exponential mean must be a positive finite number\\.$")
    if (!is.null(m)) return(sprintf("%s：指数分布均值必须是正的有限数。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) Gamma shape and scale must be positive finite numbers\\.$")
    if (!is.null(m)) return(sprintf("%s：Gamma 分布的 shape 和 scale 必须是正的有限数。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) Normal mean must be finite and sd must be finite and non-negative\\.$")
    if (!is.null(m)) return(sprintf("%s：Normal 分布均值必须有限，标准差必须是非负有限数。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) degenerate Normal mean must be positive when sd is zero\\.$")
    if (!is.null(m)) return(sprintf("%s：当 Normal 分布标准差为 0 时，退化均值必须为正。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) Uniform max must be greater than min, and both must be finite\\.$")
    if (!is.null(m)) return(sprintf("%s：Uniform 分布上限必须大于下限，且二者都必须有限。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) Lognormal meanlog must be finite and sdlog must be finite and non-negative\\.$")
    if (!is.null(m)) return(sprintf("%s：Lognormal 分布的 meanlog 必须有限，sdlog 必须是非负有限数。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) Inverse Gaussian mean and shape lambda must be positive finite numbers\\.$")
    if (!is.null(m)) return(sprintf("%s：Inverse Gaussian 分布的 mean 和 shape lambda 必须是正的有限数。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) distribution type is not supported\\.$")
    if (!is.null(m)) return(sprintf("%s：不支持当前分布类型。", diagnostic_pattern_label(m[1], lang)))

    m <- diagnostic_match(message, "^(.+) spike-count range must be a positive integer range\\.$")
    if (!is.null(m)) return(sprintf("%s：spike 数范围必须是正整数区间。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^Burst spike-count range must allow at least ([0-9]+) boundary spikes so that a Burst is not just an isolated ISI\\.$")
    if (!is.null(m)) return(sprintf("爆发 spike 数范围必须至少允许 %s 个边界 spike，避免将单个孤立 ISI 误定义为 burst。", m[1]))
    m <- diagnostic_match(message, "^Tonic spike-count range must allow at least ([0-9]+) boundary spikes so that CV/CV2/LV can be evaluated\\.$")
    if (!is.null(m)) return(sprintf("节律 spike 数范围必须至少允许 %s 个边界 spike，才能评估 CV/CV2/LV。", m[1]))
    m <- diagnostic_match(message, "^(.+) spike-count range must allow at least 2 spikes so that the pattern can define at least one labeled ISI\\.$")
    if (!is.null(m)) return(sprintf("%s：spike 数范围必须至少允许 2 个 spike，才能定义至少 1 个带标签 ISI。", diagnostic_pattern_label(m[1], lang)))

    m <- diagnostic_match(message, "^(.+) accepted interval range must be a finite non-negative range\\.$")
    if (!is.null(m)) return(sprintf("%s：接受的 ISI 范围必须是非负有限区间。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) effective interval range is empty after applying the absolute refractory period\\.$")
    if (!is.null(m)) return(sprintf("%s：应用绝对不应期后，有效 ISI 范围为空。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) has no feasible interval segment after accepted range, absolute refractory period, contextual Noisy clean-label rules and static feasibility checks\\.$")
    if (!is.null(m)) return(sprintf("%s：经过接受范围、绝对不应期、上下文噪声清洁标签规则和静态可行性检查后，已无可行 ISI 区段。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) has no positive probability mass after accepted range, absolute refractory period, contextual Noisy clean-label rules and static feasibility checks\\.$")
    if (!is.null(m)) return(sprintf("%s：经过接受范围、绝对不应期、上下文噪声清洁标签规则和静态可行性检查后，可行区域内没有正概率质量。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) effective acceptance probability is very small \\(([^)]+)\\); simulation may be slow or short\\.$")
    if (!is.null(m)) return(sprintf("%s：有效接受概率很小（%s）；模拟可能变慢，或生成时长不足。", diagnostic_pattern_label(m[1], lang), m[2]))
    m <- diagnostic_match(message, "^(.+) ISI serial correlation rho must be finite and within \\[-0\\.95, 0\\.95\\]\\.$")
    if (!is.null(m)) return(sprintf("%s：ISI 序列相关系数 rho 必须有限，并位于 [-0.95, 0.95]。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) ISI trend log-slope must be finite and within \\[-3, 3\\]\\.$")
    if (!is.null(m)) return(sprintf("%s：ISI 趋势 log-slope 必须有限，并位于 [-3, 3]。", diagnostic_pattern_label(m[1], lang)))

    m <- diagnostic_match(message, "^(.+) has no feasible interval segment after excluding intervals that would create a forbidden Noisy burst-like run\\.$")
    if (!is.null(m)) return(sprintf("%s：排除会形成禁用噪声 burst-like run 的区间后，已无可行 ISI 区段。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) has no feasible interval segment under the current accepted range (.+), absolute refractory period, contextual Noisy clean-label and adjacency rules\\.$")
    if (!is.null(m)) return(sprintf("%s：在当前接受范围 %s、绝对不应期、上下文噪声清洁标签和邻接规则下，已无可行 ISI 区段。", diagnostic_pattern_label(m[1], lang), ifelse(m[2] == "<empty>", "空集", m[2])))
    m <- diagnostic_match(message, "^(.+) interval distribution has zero probability mass in the feasible segment set; adjust distribution parameters or accepted ranges\\.$")
    if (!is.null(m)) return(sprintf("%s：ISI 分布在可行区段集合内的概率质量为 0；请调整分布参数或接受范围。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^(.+) truncated interval sampler failed despite a positive feasible mass estimate ([^\\.]+)\\.$")
    if (!is.null(m)) return(sprintf("%s：尽管估计存在正的可行概率质量（%s），截断 ISI 采样器仍采样失败。", diagnostic_pattern_label(m[1], lang), m[2]))
    m <- diagnostic_match(message, "^Tonic interval sampler failed after ([0-9]+) sequence attempts because CV/CV2/LV constraints were not satisfied\\. Relax tonic regularity ranges or adjust the tonic ISI distribution\\.$")
    if (!is.null(m)) return(sprintf("节律 ISI 采样器在 %s 次序列尝试后仍未满足 CV/CV2/LV 约束。请放宽节律规则性范围，或调整节律 ISI 分布。", m[1]))
    m <- diagnostic_match(message, "^(.+) spike-count range is invalid for interval-label generation; using the minimum count that defines a labeled run\\.$")
    if (!is.null(m)) return(sprintf("%s：spike 数范围不适合生成 ISI 标签；已使用能定义该标签 run 的最小 spike 数。", diagnostic_pattern_label(m[1], lang)))
    m <- diagnostic_match(message, "^No feasible positive initial latency was available before the first (.+)-labeled ISI run within the remaining ([^ ]+) s under the '(.+)' latency model\\. The recording boundary at 0 s is not allowed to be a spike\\.$")
    if (!is.null(m)) return(sprintf("在 %s 潜伏期模型下，首个 %s 标签 ISI run 之前、剩余 %s s 内没有可行的正初始潜伏期。0 s 记录边界不允许作为 spike。", diagnostic_latency_model_label(m[3], lang), diagnostic_pattern_label(m[1], lang), m[2]))
    m <- diagnostic_match(message, "^Manual Pause duration ([^ ]+) s is outside the feasible Pause segment set (.+) after accepted-range, absolute refractory period, and adjacency checks\\.$")
    if (!is.null(m)) return(sprintf("手动 Pause 时长 %s s 超出可行 Pause 区段集合 %s；该集合已综合接受范围、绝对不应期和邻接规则。", m[1], ifelse(m[2] == "<empty>", "空集", m[2])))
    m <- diagnostic_match(message, "^(.+) interval run could not be generated within the remaining ([^ ]+) s after ([0-9]+) attempts\\.$")
    if (!is.null(m)) return(sprintf("%s：在剩余 %s s 内尝试 %s 次后，仍无法生成该 ISI run。", diagnostic_pattern_label(m[1], lang), m[2], m[3]))
    m <- diagnostic_match(message, "^Stopped manual interval-label sequence at ([0-9\\.]+) s because label run '(.+)' could not fit or be generated\\.$")
    if (!is.null(m)) return(sprintf("手动 ISI 标签序列在 %.4f s 停止：%s 标签 run 无法放入剩余时间或无法生成。", as.numeric(m[1]), diagnostic_pattern_label(m[2], lang)))
    m <- diagnostic_match(message, "^Stopped at ([0-9\\.]+) s after reaching the automatic-run safety cap of ([0-9]+) runs; increase max_auto_runs for long simulations\\.$")
    if (!is.null(m)) return(sprintf("模拟在 %.4f s 停止：已达到自动 run 安全上限 %s。若要生成更长序列，请增大 max_auto_runs。", as.numeric(m[1]), m[2]))
    m <- diagnostic_match(message, "^Stopped at ([0-9\\.]+) s because no feasible interval-label run fit in the remaining time\\.$")
    if (!is.null(m)) return(sprintf("模拟在 %.4f s 停止：剩余时间内没有可放入的可行 ISI 标签 run。", as.numeric(m[1])))
    m <- diagnostic_match(message, "^Achieved duration ([0-9\\.]+) s is shorter than requested duration ([0-9\\.]+) s\\.$")
    if (!is.null(m)) return(sprintf("实际达成时长 %.4f s 短于请求时长 %.4f s。", as.numeric(m[1]), as.numeric(m[2])))

    diagnostic_translate_fallback_zh(message)
  }

  translate_diagnostic_messages <- function(lang, messages) {
    vapply(messages, translate_diagnostic_message, character(1), lang = lang, USE.NAMES = FALSE)
  }

  summary_col_labels <- function(lang, cols) {
    en <- c(
      Train = "Spike train",
      Total_Spikes = "Total real spikes",
      Global_Mean_Rate_Hz = "Global mean rate (Hz)",
      Requested_Duration_s = "Requested duration (s)",
      Achieved_Duration_s = "Achieved duration (s)",
      Duration_Shortfall_s = "Duration shortfall (s)",
      Duration_Completion_pct = "Duration completion (%)",
      Global_Mean_ISI_s = "Global mean ISI (s)",
      Global_CV_ISI = "Global CV ISI",
      Mean_Within_Episode_ISI_s = "Mean within-episode ISI (s)",
      Within_Episode_CV_ISI = "Within-episode CV ISI",
      Burst_Episodes = "Burst episodes",
      Pause_Episodes = "Pause episodes",
      Tonic_Episodes = "Tonic episodes",
      HFT_Episodes = "High-frequency tonic episodes",
      HFS_Episodes = "High-frequency spiking episodes",
      Noisy_Episodes = "Noisy episodes",
      Burst_ISIs = "Burst ISIs",
      Pause_ISIs = "Pause ISIs",
      Tonic_ISIs = "Tonic ISIs",
      HFT_ISIs = "High-frequency tonic ISIs",
      HFS_ISIs = "High-frequency spiking ISIs",
      Noisy_ISIs = "Noisy ISIs",
      Latency_Episodes = "Latency rows",
      Leading_Latency_Count = "Leading latency rows",
      Initial_Latency_Count = "Initial latency rows",
      Latency_Time_s = "Latency time (s)",
      Leading_Latency_Time_s = "Leading latency time (s)",
      Initial_Latency_Time_s = "Initial latency time (s)",
      Burst_Actual_Time_pct = "Burst actual time (%)",
      Pause_Actual_Time_pct = "Pause actual time (%)",
      Tonic_Actual_Time_pct = "Tonic actual time (%)",
      HFT_Actual_Time_pct = "High-frequency tonic actual time (%)",
      HFS_Actual_Time_pct = "High-frequency spiking actual time (%)",
      Noisy_Actual_Time_pct = "Noisy actual time (%)"
    )
    zh <- c(
      Train = "Spike train",
      Total_Spikes = "真实 spike 数",
      Global_Mean_Rate_Hz = "全局平均频率 (Hz)",
      Requested_Duration_s = "请求时长 (s)",
      Achieved_Duration_s = "实际达成时长 (s)",
      Duration_Shortfall_s = "时长缺口 (s)",
      Duration_Completion_pct = "时长完成度 (%)",
      Global_Mean_ISI_s = "全局平均 ISI (s)",
      Global_CV_ISI = "全局 ISI 变异系数",
      Mean_Within_Episode_ISI_s = "Episode 内平均 ISI (s)",
      Within_Episode_CV_ISI = "Episode 内 ISI 变异系数",
      Burst_Episodes = "爆发 episode 数",
      Pause_Episodes = "暂停 episode 数",
      Tonic_Episodes = "节律 episode 数",
      HFT_Episodes = "高频强直 episode 数",
      HFS_Episodes = "高频持续放电 episode 数",
      Noisy_Episodes = "噪声 episode 数",
      Burst_ISIs = "爆发 ISI 数",
      Pause_ISIs = "暂停 ISI 数",
      Tonic_ISIs = "节律 ISI 数",
      HFT_ISIs = "高频强直 ISI 数",
      HFS_ISIs = "高频持续放电 ISI 数",
      Noisy_ISIs = "噪声 ISI 数",
      Latency_Episodes = "潜伏期记录数",
      Leading_Latency_Count = "起始静默潜伏期记录数",
      Initial_Latency_Count = "初始潜伏期记录数",
      Latency_Time_s = "潜伏期时长 (s)",
      Leading_Latency_Time_s = "起始静默潜伏期时长 (s)",
      Initial_Latency_Time_s = "初始潜伏期时长 (s)",
      Burst_Actual_Time_pct = "爆发实际时间 (%)",
      Pause_Actual_Time_pct = "暂停实际时间 (%)",
      Tonic_Actual_Time_pct = "节律实际时间 (%)",
      HFT_Actual_Time_pct = "高频强直实际时间 (%)",
      HFS_Actual_Time_pct = "高频持续放电实际时间 (%)",
      Noisy_Actual_Time_pct = "噪声实际时间 (%)"
    )
    translate_names(cols, if (lang == "zh") zh else en)
  }

  target_col_labels <- function(lang, cols) {
    en <- c(Train = "Spike train", Pattern = "Pattern", Target_Time_pct = "Target time (%)", Actual_Time_pct = "Actual time (%)", Delta_pct = "Delta (%)")
    zh <- c(Train = "Spike train", Pattern = "模式", Target_Time_pct = "目标时间 (%)", Actual_Time_pct = "实际时间 (%)", Delta_pct = "差值 (%)")
    translate_names(cols, if (lang == "zh") zh else en)
  }

  duration_col_labels <- function(lang, cols) {
    en <- c(
      Train = "Spike train",
      Requested_Duration_s = "Requested duration (s)",
      Achieved_Duration_s = "Achieved duration (s)",
      Duration_Shortfall_s = "Duration shortfall (s)",
      Duration_Completion_pct = "Duration completion (%)"
    )
    zh <- c(
      Train = "Spike train",
      Requested_Duration_s = "请求时长 (s)",
      Achieved_Duration_s = "实际达成时长 (s)",
      Duration_Shortfall_s = "时长缺口 (s)",
      Duration_Completion_pct = "时长完成度 (%)"
    )
    translate_names(cols, if (lang == "zh") zh else en)
  }

  spike_col_labels <- function(lang, cols) {
    en <- c(
      Train = "Spike train",
      Episode = "Generating episode",
      Spike_Index = "Spike index",
      Time = "Spike timestamp (s)",
      Episode_Context = "Generating/right episode context",
      Spike_Role = "Spike role",
      Previous_Spike_Time_s = "Previous spike timestamp (s)",
      ISI_From_Previous_s = "Left ISI from previous spike (s)",
      ISI_Label = "Left ISI label",
      ISI_Scope = "Left ISI scope",
      ISI_Episode = "Left ISI episode",
      Next_Spike_Time_s = "Next spike timestamp (s)",
      ISI_To_Next_s = "Right ISI to next spike (s)",
      Right_ISI_Label = "Right ISI label",
      Right_ISI_Scope = "Right ISI scope",
      Right_ISI_Episode = "Right ISI episode"
    )
    zh <- c(
      Train = "Spike train",
      Episode = "生成该 spike 的 episode",
      Spike_Index = "Spike 序号",
      Time = "Spike 时间戳 (s)",
      Episode_Context = "生成 / 右侧 episode 上下文",
      Spike_Role = "Spike 角色",
      Previous_Spike_Time_s = "前一个 spike 时间戳 (s)",
      ISI_From_Previous_s = "左侧 ISI：与前一 spike 的间隔 (s)",
      ISI_Label = "左侧 ISI 标记",
      ISI_Scope = "左侧 ISI 范围",
      ISI_Episode = "左侧 ISI episode",
      Next_Spike_Time_s = "后一个 spike 时间戳 (s)",
      ISI_To_Next_s = "右侧 ISI：与后一 spike 的间隔 (s)",
      Right_ISI_Label = "右侧 ISI 标记",
      Right_ISI_Scope = "右侧 ISI 范围",
      Right_ISI_Episode = "右侧 ISI episode"
    )
    translate_names(cols, if (lang == "zh") zh else en)
  }

  interval_col_labels <- function(lang, cols) {
    en <- c(
      Train = "Spike train",
      Interval_ID = "Interval ID",
      Left_Spike_Index = "Left spike index",
      Right_Spike_Index = "Right spike index",
      Left_Spike_Time_s = "Left spike time (s)",
      Right_Spike_Time_s = "Right spike time (s)",
      Start_Time_s = "Interval start (s)",
      End_Time_s = "Interval end (s)",
      ISI_s = "ISI (s)",
      Interval = "ISI (s)",
      ISI_Label = "ISI label",
      Episode = "Interval episode",
      ISI_Scope = "ISI scope",
      Left_Spike_Role = "Left spike role",
      Right_Spike_Role = "Right spike role",
      Left_Episode_Context = "Left spike episode context",
      Right_Episode_Context = "Right spike episode context",
      Is_Manual_Fixed = "Manual fixed duration",
      Interval_Source = "Interval source"
    )
    zh <- c(
      Train = "Spike train",
      Interval_ID = "Interval 编号",
      Left_Spike_Index = "左边界 spike 序号",
      Right_Spike_Index = "右边界 spike 序号",
      Left_Spike_Time_s = "左边界 spike 时间 (s)",
      Right_Spike_Time_s = "右边界 spike 时间 (s)",
      Start_Time_s = "Interval 开始 (s)",
      End_Time_s = "Interval 结束 (s)",
      ISI_s = "ISI (s)",
      Interval = "ISI (s)",
      ISI_Label = "ISI 标记",
      Episode = "Interval 所属 episode",
      ISI_Scope = "ISI 范围",
      Left_Spike_Role = "左边界 spike 角色",
      Right_Spike_Role = "右边界 spike 角色",
      Left_Episode_Context = "左 spike episode 上下文",
      Right_Episode_Context = "右 spike episode 上下文",
      Is_Manual_Fixed = "手动固定时长",
      Interval_Source = "Interval 来源"
    )
    translate_names(cols, if (lang == "zh") zh else en)
  }

  episode_col_labels <- function(lang, cols = NULL) {
    en <- c(
      Train = "Spike train", Episode = "Episode", Pattern = "Pattern", Episode_Scope = "Episode Scope", Latency_Context = "Latency Context", Latency_Model = "Latency Model", Start = "Start (s)", End = "End (s)",
      Episode_Duration = "Episode Duration (s)", Core_Start = "Core Start (s)", Core_End = "Core End (s)",
      Core_Duration = "Core Duration (s)", First_Spike_Time = "First Spike Time (s)", Last_Spike_Time = "Last Spike Time (s)",
      N_Spikes = "Boundary Spikes", N_ISIs = "Labeled ISIs", N_Boundary_Spikes = "Boundary Spikes",
      N_New_Spikes = "New Spikes", N_Shared_Boundary_Spikes = "Shared Boundary Spikes",
      Mean_Within_Episode_ISI = "Mean Episode ISI (s)", CV_Within_Episode_ISI = "Episode ISI CV",
      Mean_CV2_Within_Episode_ISI = "Episode Mean CV2", LV_Within_Episode_ISI = "Episode LV",
      Core_ISI_Rate_Hz = "Core ISI Rate (Hz)", Episode_Inclusive_Rate_Hz = "Episode Inclusive Rate (Hz)"
    )
    zh <- c(
      Train = "Spike train", Episode = "Episode", Pattern = "模式", Episode_Scope = "Episode 范围", Latency_Context = "潜伏期上下文", Latency_Model = "潜伏期模型", Start = "开始 (s)", End = "结束 (s)",
      Episode_Duration = "Episode 时长 (s)", Core_Start = "核心开始 (s)", Core_End = "核心结束 (s)",
      Core_Duration = "核心时长 (s)", First_Spike_Time = "首个 spike 时间 (s)", Last_Spike_Time = "末个 spike 时间 (s)",
      N_Spikes = "边界 spike 数", N_ISIs = "标记 ISI 数", N_Boundary_Spikes = "边界 spike 数",
      N_New_Spikes = "新增 spike 数", N_Shared_Boundary_Spikes = "共享边界 spike 数",
      Mean_Within_Episode_ISI = "Episode 平均 ISI (s)", CV_Within_Episode_ISI = "Episode ISI CV",
      Mean_CV2_Within_Episode_ISI = "Episode 平均 CV2", LV_Within_Episode_ISI = "Episode LV",
      Core_ISI_Rate_Hz = "核心 ISI 频率 (Hz)", Episode_Inclusive_Rate_Hz = "含保护区 episode 频率 (Hz)"
    )
    if (is.null(cols)) return(if (lang == "zh") unname(zh) else unname(en))
    translate_names(cols, if (lang == "zh") zh else en)
  }

  real_spike_rows <- function(spikes) {
    # In the V13.5.0 architecture, every row in the spike table is a real spike event derived from the generated ISI sequence.
    # Pattern labels live in the interval table, not in spike rows.
    spikes
  }

  plot_interval_runs <- function(intervals, allowed_labels, window) {
    out <- data.frame(
      Train = integer(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      Segment_Label = character(0),
      N_Intervals = integer(0),
      stringsAsFactors = FALSE
    )
    if (is.null(intervals) || nrow(intervals) == 0) return(out)
    required <- c("Train", "Start_Time_s", "End_Time_s", "ISI_Label")
    if (!all(required %in% names(intervals))) return(out)

    start_time <- suppressWarnings(as.numeric(intervals$Start_Time_s))
    end_time <- suppressWarnings(as.numeric(intervals$End_Time_s))
    if ("Left_Spike_Time_s" %in% names(intervals)) {
      left_time <- suppressWarnings(as.numeric(intervals$Left_Spike_Time_s))
      start_time[is.finite(left_time)] <- left_time[is.finite(left_time)]
    }
    if ("Right_Spike_Time_s" %in% names(intervals)) {
      right_time <- suppressWarnings(as.numeric(intervals$Right_Spike_Time_s))
      end_time[is.finite(right_time)] <- right_time[is.finite(right_time)]
    }

    keep <- is.finite(start_time) &
      is.finite(end_time) &
      end_time > start_time &
      end_time >= window[1] &
      start_time <= window[2] &
      intervals$ISI_Label %in% allowed_labels

    x <- intervals[
      keep,
      ,
      drop = FALSE
    ]
    if (nrow(x) == 0) return(out)
    x$.Plot_Start_Time <- start_time[keep]
    x$.Plot_End_Time <- end_time[keep]

    if (!"Interval_ID" %in% names(x)) x$Interval_ID <- seq_len(nrow(x))
    if (!"Episode" %in% names(x)) x$Episode <- NA_integer_
    x$Train <- suppressWarnings(as.integer(x$Train))
    x$Interval_ID <- suppressWarnings(as.integer(x$Interval_ID))
    x$Episode <- suppressWarnings(as.integer(x$Episode))
    x <- x[order(x$Train, x$.Plot_Start_Time, x$.Plot_End_Time, x$Interval_ID), , drop = FALSE]

    breaks <- rep(TRUE, nrow(x))
    if (nrow(x) > 1L) {
      for (i in 2:nrow(x)) {
        same_train <- identical(x$Train[i], x$Train[i - 1L])
        same_label <- identical(as.character(x$ISI_Label[i]), as.character(x$ISI_Label[i - 1L]))
        contiguous_time <- abs(as.numeric(x$.Plot_Start_Time[i]) - as.numeric(x$.Plot_End_Time[i - 1L])) <= 1e-8
        same_episode <- TRUE
        if (is.finite(x$Episode[i]) && is.finite(x$Episode[i - 1L])) {
          same_episode <- identical(x$Episode[i], x$Episode[i - 1L])
        }
        breaks[i] <- !(same_train && same_label && contiguous_time && same_episode)
      }
    }

    groups <- split(seq_len(nrow(x)), cumsum(breaks))
    rows <- lapply(groups, function(idx) {
      idx <- idx[order(x$.Plot_Start_Time[idx], x$.Plot_End_Time[idx], x$Interval_ID[idx])]
      label <- as.character(x$ISI_Label[idx[1]])
      data.frame(
        Train = x$Train[idx[1]],
        Plot_Start = max(as.numeric(x$.Plot_Start_Time[idx[1]]), window[1]),
        Plot_End = min(as.numeric(x$.Plot_End_Time[idx[length(idx)]]), window[2]),
        Segment_Label = label,
        N_Intervals = length(idx),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out <- out[is.finite(out$Plot_Start) & is.finite(out$Plot_End) & out$Plot_End > out$Plot_Start, , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  add_spike_biology_columns <- function(spikes) {
    # Kept as a no-op wrapper because downstream views call it. The spike table is already
    # restricted to real biological events by construction.
    spikes
  }

  translate_pattern_values <- function(df, lang) {
    if (nrow(df) == 0) return(df)
    labels <- pattern_labels(lang)
    labels <- c(
      labels,
      Latency = if (identical(lang, "zh")) "响应潜伏期" else "Response latency",
      Interburst_Gap = if (identical(lang, "zh")) "Burst 间隔" else "Interburst gap",
      Stimulus_Gap = if (identical(lang, "zh")) "跨刺激间隔" else "Stimulus-spanning gap"
    )
    pattern_cols <- intersect(c("Pattern", "ISI_Label", "Right_ISI_Label", "Episode_Context", "Left_Episode_Context", "Right_Episode_Context", "Latency_Context"), names(df))
    for (col in pattern_cols) {
      translated <- labels[df[[col]]]
      needs_translation <- !is.na(translated)
      df[[col]][needs_translation] <- unname(translated[needs_translation])
    }

    if ("Spike_Role" %in% names(df)) {
      role_labels <- if (identical(lang, "zh")) {
        c(
          episode_spike = "真实 episode spike",
          event_spike = "真实事件 spike",
          shared_boundary_spike = "真实共享边界 spike",
          pause_left_boundary_spike = "真实 Pause 左边界 spike",
          pause_right_boundary_spike = "真实 Pause 右边界 spike",
          leading_silence_end_spike = "leading silence 结束 spike",
          leading_silence_end_shared_boundary_spike = "初始潜伏期结束且作为后续 episode 起点的 spike"
        )
      } else {
        c(
          episode_spike = "Real episode spike",
          event_spike = "Real event spike",
          shared_boundary_spike = "Real shared-boundary spike",
          pause_left_boundary_spike = "Real Pause left-boundary spike",
          pause_right_boundary_spike = "Real Pause right-boundary spike",
          leading_silence_end_spike = "Leading-silence end spike",
          leading_silence_end_shared_boundary_spike = "Initial-latency end and subsequent episode-start spike"
        )
      }
      df$Spike_Role <- as.character(df$Spike_Role)
      translated <- role_labels[df$Spike_Role]
      needs_translation <- !is.na(translated)
      df$Spike_Role[needs_translation] <- unname(translated[needs_translation])
    }

    scope_cols <- intersect(c("ISI_Scope", "Right_ISI_Scope"), names(df))
    if (length(scope_cols) > 0) {
      scope_labels <- if (identical(lang, "zh")) {
        c(
          within_episode = "episode 内 ISI",
          pause_isi = "Pause 长 ISI",
          stimulus_latency = "响应潜伏期",
          interburst_gap = "Burst 间隔",
          stimulus_spanning_gap = "跨刺激间隔",
          evoked_suppression = "诱发性抑制",
          post_burst_pause = "Burst 后 pause",
          evoked_pause = "诱发 pause",
          failed_response_baseline = "失败响应后的基线发放",
          no_response_baseline = "无响应基线发放",
          post_stimulus_recovery = "刺激后恢复期"
        )
      } else {
        c(
          within_episode = "Within-episode ISI",
          pause_isi = "Pause long ISI",
          stimulus_latency = "Response latency",
          interburst_gap = "Interburst gap",
          stimulus_spanning_gap = "Stimulus-spanning gap",
          evoked_suppression = "Evoked suppression",
          post_burst_pause = "Post-burst pause",
          evoked_pause = "Evoked pause",
          failed_response_baseline = "Failed-response baseline",
          no_response_baseline = "No-response baseline",
          post_stimulus_recovery = "Post-stimulus recovery"
        )
      }
      for (scope_col in scope_cols) {
        df[[scope_col]] <- as.character(df[[scope_col]])
        translated <- scope_labels[df[[scope_col]]]
        needs_translation <- !is.na(translated)
        df[[scope_col]][needs_translation] <- unname(translated[needs_translation])
      }
    }

    if ("Episode_Scope" %in% names(df)) {
      episode_scope_labels <- if (identical(lang, "zh")) {
        c(
          interval_run = "真实 ISI label run",
          leading_latency = "记录窗口起始潜伏期",
          initial_latency = "正初始 spike 潜伏期",
          stimulus_latency = "响应潜伏期",
          interburst_gap = "Burst 间隔",
          stimulus_spanning_gap = "跨刺激间隔",
          evoked_suppression = "诱发性抑制",
          post_burst_pause = "Burst 后 pause",
          evoked_pause = "诱发 pause",
          failed_response_baseline = "失败响应后的基线发放",
          no_response_baseline = "无响应基线发放",
          post_stimulus_recovery = "刺激后恢复期"
        )
      } else {
        c(
          interval_run = "True ISI-label run",
          leading_latency = "Recording-window leading latency",
          initial_latency = "Positive initial spike latency",
          stimulus_latency = "Response latency",
          interburst_gap = "Interburst gap",
          stimulus_spanning_gap = "Stimulus-spanning gap",
          evoked_suppression = "Evoked suppression",
          post_burst_pause = "Post-burst pause",
          evoked_pause = "Evoked pause",
          failed_response_baseline = "Failed-response baseline",
          no_response_baseline = "No-response baseline",
          post_stimulus_recovery = "Post-stimulus recovery"
        )
      }
      df$Episode_Scope <- as.character(df$Episode_Scope)
      translated <- episode_scope_labels[df$Episode_Scope]
      needs_translation <- !is.na(translated)
      df$Episode_Scope[needs_translation] <- unname(translated[needs_translation])
    }

    df
  }

  export_pattern_values <- function(df) {
    if (nrow(df) == 0) return(df)
    labels <- c(
      pattern_labels("en"),
      Latency = "Response latency",
      Interburst_Gap = "Interburst gap",
      Stimulus_Gap = "Stimulus-spanning gap"
    )
    pattern_cols <- intersect(
      c("Pattern", "ISI_Label", "Right_ISI_Label", "Episode_Context", "Left_Episode_Context",
        "Right_Episode_Context", "Latency_Context", "Label", "Pred_Label"),
      names(df)
    )
    for (col in pattern_cols) {
      translated <- labels[df[[col]]]
      needs_translation <- !is.na(translated)
      df[[col]][needs_translation] <- unname(translated[needs_translation])
    }
    df
  }

  spike_colors <- function() {
    line_colors()
  }

  distribution_line_colors <- function() {
    cols <- line_colors()
    cols["Noisy"] <- DISTRIBUTION_NOISY_LINE_COLOR
    cols
  }

  distribution_fill_colors <- function() {
    cols <- spike_colors()
    cols["Noisy"] <- DISTRIBUTION_NOISY_FILL_COLOR
    cols
  }

  color_value <- function(input_value, fallback) {
    unname(value_or(input_value, fallback))
  }

  normalize_spike_event_table <- function(spikes) {
    if (is.null(spikes)) spikes <- make_empty_spike_df()
    if (!"Train" %in% names(spikes)) spikes$Train <- integer(nrow(spikes)) + 1L
    if (!"Episode" %in% names(spikes)) spikes$Episode <- NA_integer_
    if (!"Time" %in% names(spikes)) spikes$Time <- NA_real_
    if (!"Episode_Context" %in% names(spikes)) {
      spikes$Episode_Context <- NA_character_
    }
    if (!"Spike_Role" %in% names(spikes)) spikes$Spike_Role <- "episode_spike"
    spikes$Train <- suppressWarnings(as.integer(spikes$Train))
    spikes$Episode <- suppressWarnings(as.integer(spikes$Episode))
    spikes$Time <- suppressWarnings(as.numeric(spikes$Time))
    spikes$Episode_Context <- as.character(spikes$Episode_Context)
    spikes$Spike_Role <- as.character(spikes$Spike_Role)
    spikes
  }

  interval_assignment_from_episodes <- function(episodes, train, start_time, end_time, fallback = NA_character_) {
    fallback_label <- if (fallback %in% pattern_levels) fallback else NA_character_
    fallback_scope <- if (!is.na(fallback_label) && fallback_label == "Pause") "pause_isi" else "within_episode"

    if (!is.null(episodes) && nrow(episodes) > 0 &&
        all(c("Train", "Start", "End", "Pattern") %in% names(episodes))) {
      candidates <- episodes[
        episodes$Train == train &
          is.finite(episodes$Start) &
          is.finite(episodes$End) &
          episodes$Start < end_time - 1e-12 &
          episodes$End > start_time + 1e-12,
        ,
        drop = FALSE
      ]
      if (nrow(candidates) > 0) {
        overlap <- pmin(candidates$End, end_time) - pmax(candidates$Start, start_time)
        overlap[!is.finite(overlap)] <- 0
        if (any(overlap > 0)) {
          selected <- candidates[which.max(overlap), , drop = FALSE]
          label <- as.character(selected$Pattern[1])
          scope <- "within_episode"
          if (identical(label, "Pause")) {
            scope <- "pause_isi"
          }
          return(list(label = label, scope = scope, episode = suppressWarnings(as.integer(selected$Episode[1]))))
        }
      }
    }
    list(label = fallback_label, scope = fallback_scope, episode = NA_integer_)
  }

  build_interval_table <- function(spikes, episodes = NULL) {
    spikes <- normalize_spike_event_table(spikes)
    spikes <- real_spike_rows(spikes)
    if (nrow(spikes) == 0) return(make_empty_interval_df())

    if (is.null(episodes)) episodes <- make_empty_episode_df()
    if (!"Train" %in% names(episodes)) episodes$Train <- integer(nrow(episodes)) + 1L
    if (nrow(episodes) > 0) {
      episodes$Train <- suppressWarnings(as.integer(episodes$Train))
      episodes$Start <- suppressWarnings(as.numeric(episodes$Start))
      episodes$End <- suppressWarnings(as.numeric(episodes$End))
    }

    spikes <- spikes[order(spikes$Train, spikes$Time, seq_len(nrow(spikes))), , drop = FALSE]
    rows <- list()
    row_i <- 0L
    for (train in sort(unique(spikes$Train[is.finite(spikes$Train)]))) {
      idx <- which(spikes$Train == train & is.finite(spikes$Time))
      if (length(idx) < 2) next
      idx <- idx[order(spikes$Time[idx], idx)]
      train_times <- as.numeric(spikes$Time[idx])
      train_roles <- as.character(spikes$Spike_Role[idx])
      train_context <- as.character(spikes$Episode_Context[idx])
      for (j in seq_len(length(idx) - 1L)) {
        start_time <- train_times[j]
        end_time <- train_times[j + 1L]
        if (!is.finite(start_time) || !is.finite(end_time) || end_time <= start_time) next
        fallback <- train_context[j + 1L]
        assignment <- interval_assignment_from_episodes(episodes, train, start_time, end_time, fallback)
        row_i <- row_i + 1L
        label <- assignment$label
        rows[[row_i]] <- data.frame(
          Train = as.integer(train),
          Interval_ID = row_i,
          Left_Spike_Index = as.integer(j),
          Right_Spike_Index = as.integer(j + 1L),
          Left_Spike_Time_s = start_time,
          Right_Spike_Time_s = end_time,
          Start_Time_s = start_time,
          End_Time_s = end_time,
          ISI_s = end_time - start_time,
          Interval = end_time - start_time,
          ISI_Label = label,
          Episode = assignment$episode,
          ISI_Scope = assignment$scope,
          Left_Spike_Role = train_roles[j],
          Right_Spike_Role = train_roles[j + 1L],
          Left_Episode_Context = train_context[j],
          Right_Episode_Context = train_context[j + 1L],
          Is_Manual_Fixed = NA,
          Interval_Source = "derived_from_spikes",
          Run_Position = NA_real_,
          Run_Length = NA_integer_,
          Temporal_Rho = NA_real_,
          Temporal_Trend = NA_real_,
          Stimulus_ID = NA_integer_,
          Stimulus_Phase = NA_character_,
          Evoked = FALSE,
          Evoked_Response_Type = NA_character_,
          Response_Epoch = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(rows) == 0) return(make_empty_interval_df())
    out <- do.call(rbind, rows)
    out <- out[order(out$Train, out$Start_Time_s, out$End_Time_s), , drop = FALSE]
    out$Interval_ID <- ave(out$Interval_ID, out$Train, FUN = seq_along)
    rownames(out) <- NULL
    out
  }

  annotate_spike_isis <- function(spikes, episodes = NULL, intervals = NULL) {
    spikes <- normalize_spike_event_table(spikes)

    if (nrow(spikes) == 0) {
      spikes$Spike_Index <- integer(0)
      spikes$Previous_Spike_Time_s <- numeric(0)
      spikes$ISI_From_Previous_s <- numeric(0)
      spikes$ISI_Label <- character(0)
      spikes$ISI_Scope <- character(0)
      spikes$ISI_Episode <- integer(0)
      spikes$Next_Spike_Time_s <- numeric(0)
      spikes$ISI_To_Next_s <- numeric(0)
      spikes$Right_ISI_Label <- character(0)
      spikes$Right_ISI_Scope <- character(0)
      spikes$Right_ISI_Episode <- integer(0)
      spikes <- add_spike_biology_columns(spikes)
      return(spikes)
    }

    spikes <- spikes[order(spikes$Train, spikes$Time, seq_len(nrow(spikes))), , drop = FALSE]
    spikes$Spike_Index <- NA_integer_
    spikes$Previous_Spike_Time_s <- NA_real_
    spikes$ISI_From_Previous_s <- NA_real_
    spikes$ISI_Label <- NA_character_
    spikes$ISI_Scope <- NA_character_
    spikes$ISI_Episode <- NA_integer_
    spikes$Next_Spike_Time_s <- NA_real_
    spikes$ISI_To_Next_s <- NA_real_
    spikes$Right_ISI_Label <- NA_character_
    spikes$Right_ISI_Scope <- NA_character_
    spikes$Right_ISI_Episode <- NA_integer_

    for (train in unique(spikes$Train)) {
      idx <- which(spikes$Train == train)
      idx <- idx[order(spikes$Time[idx], idx)]
      spikes$Spike_Index[idx] <- seq_along(idx)
    }

    if (is.null(intervals)) intervals <- build_interval_table(spikes, episodes)
    if (nrow(intervals) > 0) {
      for (i in seq_len(nrow(intervals))) {
        train <- intervals$Train[i]
        left_i <- intervals$Left_Spike_Index[i]
        right_i <- intervals$Right_Spike_Index[i]
        left_idx <- which(spikes$Train == train & spikes$Spike_Index == left_i)
        right_idx <- which(spikes$Train == train & spikes$Spike_Index == right_i)
        if (length(left_idx) == 1L) {
          spikes$Next_Spike_Time_s[left_idx] <- intervals$Right_Spike_Time_s[i]
          spikes$ISI_To_Next_s[left_idx] <- intervals$ISI_s[i]
          spikes$Right_ISI_Label[left_idx] <- intervals$ISI_Label[i]
          spikes$Right_ISI_Scope[left_idx] <- intervals$ISI_Scope[i]
          spikes$Right_ISI_Episode[left_idx] <- intervals$Episode[i]
        }
        if (length(right_idx) == 1L) {
          spikes$Previous_Spike_Time_s[right_idx] <- intervals$Left_Spike_Time_s[i]
          spikes$ISI_From_Previous_s[right_idx] <- intervals$ISI_s[i]
          spikes$ISI_Label[right_idx] <- intervals$ISI_Label[i]
          spikes$ISI_Scope[right_idx] <- intervals$ISI_Scope[i]
          spikes$ISI_Episode[right_idx] <- intervals$Episode[i]
        }
      }
    }

    add_spike_biology_columns(spikes)
  }

  spike_isi_table <- function(spikes) {
    spikes <- add_spike_biology_columns(spikes)
    cols <- c("Train", "Train_Label", "Episode", "Spike_Index", "Time", "Episode_Context", "Spike_Role",
              "Previous_Spike_Time_s", "ISI_From_Previous_s", "ISI_Label", "ISI_Scope", "ISI_Episode",
              "Next_Spike_Time_s", "ISI_To_Next_s", "Right_ISI_Label", "Right_ISI_Scope", "Right_ISI_Episode")
    missing <- setdiff(cols, names(spikes))
    for (col in missing) spikes[[col]] <- NA
    spikes[, cols, drop = FALSE]
  }

  observation_config_from_sim <- function(config) {
    obs <- if (!is.null(config$observation)) config$observation else list()
    enabled <- isTRUE(obs$enabled)
    list(
      enabled = enabled,
      detection_probability = min(1, max(0, safe_num(obs$detection_probability, 1))),
      false_positive_rate_hz = max(0, safe_num(obs$false_positive_rate_hz, 0)),
      jitter_sd_s = max(0, safe_num(obs$jitter_sd_s, 0)),
      time_bias_s = safe_num(obs$time_bias_s, 0),
      dead_time_s = max(0, safe_num(obs$dead_time_s, 0)),
      seed_offset = max(1L, as.integer(round(safe_num(obs$seed_offset, 200000)))),
      mode = as.character(value_or(obs$mode, if (enabled) "bernoulli_detection_plus_false_positives" else "identity"))[1]
    )
  }

  make_empty_observed_spike_df <- function() {
    data.frame(
      Train = integer(0),
      Observed_Spike_Index = integer(0),
      Time = numeric(0),
      Observation_Source = character(0),
      Observation_Status = character(0),
      Latent_Spike_Index = integer(0),
      Latent_Time_s = numeric(0),
      Episode = integer(0),
      Episode_Context = character(0),
      Spike_Role = character(0),
      Jitter_s = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  make_empty_observation_map_df <- function() {
    data.frame(
      Train = integer(0),
      Latent_Spike_Index = integer(0),
      Latent_Time_s = numeric(0),
      Observed_Event_ID = integer(0),
      Observed_Spike_Index = integer(0),
      Observed_Time_s = numeric(0),
      Merged_To_Event_ID = integer(0),
      Observation_Source = character(0),
      Observation_Status = character(0),
      Jitter_s = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  observation_model_label <- function(obs) {
    if (!isTRUE(obs$enabled)) return("identity")
    sprintf(
      "detect=%.3g_falseHz=%.3g_jitterMs=%.3g_biasMs=%.3g_deadMs=%.3g",
      obs$detection_probability,
      obs$false_positive_rate_hz,
      1000 * obs$jitter_sd_s,
      1000 * obs$time_bias_s,
      1000 * obs$dead_time_s
    )
  }

  observation_summary_from_map <- function(obs_map, observed, train_ids, total_time, model_label, candidates = NULL) {
    if (length(train_ids) == 0) {
      return(data.frame(
        Train = integer(0),
        Observation_Model = character(0),
        Latent_Spikes = integer(0),
        Detected_True_Spikes = integer(0),
        Missed_True_Spikes = integer(0),
        False_Positive_Spikes = integer(0),
        Clipped_True_Spikes = integer(0),
        DeadTime_Merged_True_Spikes = integer(0),
        DeadTime_Merged_False_Positives = integer(0),
        Observed_Spikes = integer(0),
        Detection_Rate = numeric(0),
        False_Positive_Rate_Hz = numeric(0),
        Mean_Abs_Jitter_ms = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    if (is.null(candidates)) candidates <- data.frame()
    do.call(rbind, lapply(train_ids, function(train_id) {
      map_train <- obs_map[obs_map$Train == train_id, , drop = FALSE]
      latent_train <- map_train[map_train$Observation_Source == "latent_true_spike", , drop = FALSE]
      observed_train <- observed[observed$Train == train_id, , drop = FALSE]
      duration <- total_time
      detected_true <- sum(latent_train$Observation_Status == "detected", na.rm = TRUE)
      false_pos <- sum(map_train$Observation_Status == "false_positive", na.rm = TRUE)
      dead_false <- 0L
      if (nrow(candidates) > 0 && all(c("Keep_After_Dead_Time", "Observation_Source", "Train") %in% names(candidates))) {
        dead_false <- sum(!candidates$Keep_After_Dead_Time & candidates$Observation_Source == "false_positive" & candidates$Train == train_id, na.rm = TRUE)
      }
      detected_jitter <- latent_train$Jitter_s[latent_train$Observation_Status == "detected"]
      mean_abs_jitter <- if (any(is.finite(detected_jitter))) 1000 * mean(abs(detected_jitter), na.rm = TRUE) else NA_real_
      data.frame(
        Train = train_id,
        Observation_Model = model_label,
        Latent_Spikes = nrow(latent_train),
        Detected_True_Spikes = detected_true,
        Missed_True_Spikes = sum(latent_train$Observation_Status == "missed", na.rm = TRUE),
        False_Positive_Spikes = false_pos,
        Clipped_True_Spikes = sum(latent_train$Observation_Status == "clipped_outside_recording", na.rm = TRUE),
        DeadTime_Merged_True_Spikes = sum(latent_train$Observation_Status == "merged_by_dead_time", na.rm = TRUE),
        DeadTime_Merged_False_Positives = dead_false,
        Observed_Spikes = nrow(observed_train),
        Detection_Rate = if (nrow(latent_train) > 0) detected_true / nrow(latent_train) else NA_real_,
        False_Positive_Rate_Hz = if (duration > 0) false_pos / duration else NA_real_,
        Mean_Abs_Jitter_ms = mean_abs_jitter,
        stringsAsFactors = FALSE
      )
    }))
  }

  make_identity_observation <- function(spikes, config = NULL) {
    if (is.null(spikes) || nrow(spikes) == 0) {
      return(list(
        observed_spikes = make_empty_observed_spike_df(),
        observation_map = make_empty_observation_map_df(),
        observation_summary = data.frame(),
        model_label = "identity"
      ))
    }
    x <- spike_isi_table(spikes)
    x <- x[order(x$Train, x$Time, x$Spike_Index), , drop = FALSE]
    x$Observed_Spike_Index <- ave(x$Time, x$Train, FUN = seq_along)
    observed <- data.frame(
      Train = x$Train,
      Observed_Spike_Index = as.integer(x$Observed_Spike_Index),
      Time = x$Time,
      Observation_Source = "latent_true_spike",
      Observation_Status = "detected",
      Latent_Spike_Index = x$Spike_Index,
      Latent_Time_s = x$Time,
      Episode = x$Episode,
      Episode_Context = x$Episode_Context,
      Spike_Role = x$Spike_Role,
      Jitter_s = 0,
      stringsAsFactors = FALSE
    )
    obs_map <- data.frame(
      Train = x$Train,
      Latent_Spike_Index = x$Spike_Index,
      Latent_Time_s = x$Time,
      Observed_Event_ID = seq_len(nrow(x)),
      Observed_Spike_Index = as.integer(x$Observed_Spike_Index),
      Observed_Time_s = x$Time,
      Merged_To_Event_ID = NA_integer_,
      Observation_Source = "latent_true_spike",
      Observation_Status = "detected",
      Jitter_s = 0,
      stringsAsFactors = FALSE
    )
    train_ids <- sort(unique(as.integer(x$Train)))
    total_time <- if (!is.null(config) && is.finite(safe_num(config$total_time, NA_real_))) safe_num(config$total_time, NA_real_) else max(x$Time, na.rm = TRUE)
    summary <- observation_summary_from_map(obs_map, observed, train_ids, total_time, "identity")
    list(observed_spikes = observed, observation_map = obs_map, observation_summary = summary, model_label = "identity")
  }

  apply_dead_time_filter <- function(events, dead_time_s) {
    if (nrow(events) == 0 || !is.finite(dead_time_s) || dead_time_s <= 0) {
      events$Keep_After_Dead_Time <- TRUE
      events$Dead_Time_Dropped_By <- NA_integer_
      return(events)
    }
    events <- events[order(events$Train, events$Observed_Time_s, events$Source_Priority, events$Observed_Event_ID), , drop = FALSE]
    events$Keep_After_Dead_Time <- FALSE
    events$Dead_Time_Dropped_By <- NA_integer_
    for (train_id in unique(events$Train)) {
      idx <- which(events$Train == train_id)
      last_keep_row <- NA_integer_
      last_keep_time <- NA_real_
      for (row_idx in idx) {
        event_time <- events$Observed_Time_s[row_idx]
        if (!is.finite(last_keep_time) || event_time - last_keep_time >= dead_time_s - 1e-12) {
          events$Keep_After_Dead_Time[row_idx] <- TRUE
          last_keep_row <- row_idx
          last_keep_time <- event_time
        } else {
          events$Dead_Time_Dropped_By[row_idx] <- events$Observed_Event_ID[last_keep_row]
        }
      }
    }
    events
  }

  apply_observation_model_to_spikes <- function(spikes, config, seed = 1L, total_time = NULL) {
    obs <- observation_config_from_sim(config)
    if (!isTRUE(obs$enabled)) return(make_identity_observation(spikes, config))
    if (is.null(spikes) || nrow(spikes) == 0) {
      return(list(
        observed_spikes = make_empty_observed_spike_df(),
        observation_map = make_empty_observation_map_df(),
        observation_summary = data.frame(),
        model_label = observation_model_label(obs)
      ))
    }

    latent <- spike_isi_table(spikes)
    latent <- latent[order(latent$Train, latent$Time, latent$Spike_Index), , drop = FALSE]
    if (is.null(total_time) || !is.finite(total_time)) {
      total_time <- safe_num(config$total_time, max(latent$Time, na.rm = TRUE))
    }
    total_time <- max(0, safe_num(total_time, max(latent$Time, na.rm = TRUE)))
    set.seed(as.integer(seed) + obs$seed_offset)

    candidate_rows <- list()
    candidate_idx <- 0L
    latent_status <- rep("missed", nrow(latent))
    latent_observed_time <- rep(NA_real_, nrow(latent))
    latent_jitter <- rep(NA_real_, nrow(latent))
    latent_event_id <- rep(NA_integer_, nrow(latent))
    latent_merge_to_event_id <- rep(NA_integer_, nrow(latent))

    for (i in seq_len(nrow(latent))) {
      detected <- stats::runif(1) <= obs$detection_probability
      if (!isTRUE(detected)) next
      jitter <- obs$time_bias_s + if (obs$jitter_sd_s > 0) stats::rnorm(1, 0, obs$jitter_sd_s) else 0
      obs_time <- latent$Time[i] + jitter
      if (!is.finite(obs_time) || obs_time < 0 || obs_time > total_time) {
        latent_status[i] <- "clipped_outside_recording"
        latent_jitter[i] <- jitter
        next
      }
      candidate_idx <- candidate_idx + 1L
      latent_status[i] <- "detected"
      latent_observed_time[i] <- obs_time
      latent_jitter[i] <- jitter
      latent_event_id[i] <- candidate_idx
      candidate_rows[[candidate_idx]] <- data.frame(
        Observed_Event_ID = candidate_idx,
        Train = latent$Train[i],
        Observed_Time_s = obs_time,
        Observation_Source = "latent_true_spike",
        Latent_Row = i,
        Latent_Spike_Index = latent$Spike_Index[i],
        Latent_Time_s = latent$Time[i],
        Episode = latent$Episode[i],
        Episode_Context = latent$Episode_Context[i],
        Spike_Role = latent$Spike_Role[i],
        Jitter_s = jitter,
        Source_Priority = 0L,
        stringsAsFactors = FALSE
      )
    }

    train_ids <- sort(unique(as.integer(latent$Train)))
    for (train_id in train_ids) {
      n_false <- stats::rpois(1, obs$false_positive_rate_hz * total_time)
      if (!is.finite(n_false) || n_false <= 0) next
      false_times <- sort(stats::runif(n_false, min = 0, max = total_time))
      for (ft in false_times) {
        candidate_idx <- candidate_idx + 1L
        candidate_rows[[candidate_idx]] <- data.frame(
          Observed_Event_ID = candidate_idx,
          Train = train_id,
          Observed_Time_s = ft,
          Observation_Source = "false_positive",
          Latent_Row = NA_integer_,
          Latent_Spike_Index = NA_integer_,
          Latent_Time_s = NA_real_,
          Episode = NA_integer_,
          Episode_Context = NA_character_,
          Spike_Role = "observed_false_positive",
          Jitter_s = NA_real_,
          Source_Priority = 1L,
          stringsAsFactors = FALSE
        )
      }
    }

    candidates <- if (length(candidate_rows) > 0) do.call(rbind, candidate_rows) else data.frame()
    if (nrow(candidates) == 0) {
      obs_map <- data.frame(
        Train = latent$Train,
        Latent_Spike_Index = latent$Spike_Index,
        Latent_Time_s = latent$Time,
        Observed_Event_ID = NA_integer_,
        Observed_Spike_Index = NA_integer_,
        Observed_Time_s = NA_real_,
        Merged_To_Event_ID = NA_integer_,
        Observation_Source = "latent_true_spike",
        Observation_Status = latent_status,
        Jitter_s = latent_jitter,
        stringsAsFactors = FALSE
      )
      empty_observed <- make_empty_observed_spike_df()
      summary <- observation_summary_from_map(
        obs_map,
        empty_observed,
        train_ids,
        total_time,
        observation_model_label(obs),
        candidates = candidates
      )
      return(list(observed_spikes = make_empty_observed_spike_df(), observation_map = obs_map,
                  observation_summary = summary, model_label = observation_model_label(obs)))
    }

    candidates <- apply_dead_time_filter(candidates, obs$dead_time_s)
    dropped_true <- which(!candidates$Keep_After_Dead_Time & candidates$Observation_Source == "latent_true_spike" & is.finite(candidates$Latent_Row))
    if (length(dropped_true) > 0) {
      latent_status[candidates$Latent_Row[dropped_true]] <- "merged_by_dead_time"
      latent_event_id[candidates$Latent_Row[dropped_true]] <- candidates$Dead_Time_Dropped_By[dropped_true]
      latent_observed_time[candidates$Latent_Row[dropped_true]] <- candidates$Observed_Time_s[dropped_true]
      latent_merge_to_event_id[candidates$Latent_Row[dropped_true]] <- candidates$Dead_Time_Dropped_By[dropped_true]
    }

    kept <- candidates[candidates$Keep_After_Dead_Time, , drop = FALSE]
    kept <- kept[order(kept$Train, kept$Observed_Time_s, kept$Observed_Event_ID), , drop = FALSE]
    kept$Observed_Spike_Index <- ave(kept$Observed_Time_s, kept$Train, FUN = seq_along)
    kept_lookup <- kept[, c("Observed_Event_ID", "Observed_Spike_Index", "Observed_Time_s"), drop = FALSE]

    obs_map <- data.frame(
      Train = latent$Train,
      Latent_Spike_Index = latent$Spike_Index,
      Latent_Time_s = latent$Time,
      Observed_Event_ID = latent_event_id,
      Observed_Spike_Index = NA_integer_,
      Observed_Time_s = latent_observed_time,
      Merged_To_Event_ID = latent_merge_to_event_id,
      Observation_Source = "latent_true_spike",
      Observation_Status = latent_status,
      Jitter_s = latent_jitter,
      stringsAsFactors = FALSE
    )
    hit <- match(obs_map$Observed_Event_ID, kept_lookup$Observed_Event_ID)
    obs_map$Observed_Spike_Index[!is.na(hit)] <- kept_lookup$Observed_Spike_Index[hit[!is.na(hit)]]
    obs_map$Observed_Time_s[!is.na(hit)] <- kept_lookup$Observed_Time_s[hit[!is.na(hit)]]

    false_kept <- kept[kept$Observation_Source == "false_positive", , drop = FALSE]
    if (nrow(false_kept) > 0) {
      false_map <- data.frame(
        Train = false_kept$Train,
        Latent_Spike_Index = NA_integer_,
        Latent_Time_s = NA_real_,
        Observed_Event_ID = false_kept$Observed_Event_ID,
        Observed_Spike_Index = as.integer(false_kept$Observed_Spike_Index),
        Observed_Time_s = false_kept$Observed_Time_s,
        Merged_To_Event_ID = NA_integer_,
        Observation_Source = "false_positive",
        Observation_Status = "false_positive",
        Jitter_s = NA_real_,
        stringsAsFactors = FALSE
      )
      obs_map <- rbind(obs_map, false_map)
    }

    observed <- data.frame(
      Train = kept$Train,
      Observed_Spike_Index = as.integer(kept$Observed_Spike_Index),
      Time = kept$Observed_Time_s,
      Observation_Source = kept$Observation_Source,
      Observation_Status = ifelse(kept$Observation_Source == "false_positive", "false_positive", "detected"),
      Latent_Spike_Index = kept$Latent_Spike_Index,
      Latent_Time_s = kept$Latent_Time_s,
      Episode = kept$Episode,
      Episode_Context = kept$Episode_Context,
      Spike_Role = ifelse(kept$Observation_Source == "false_positive", "observed_false_positive", kept$Spike_Role),
      Jitter_s = kept$Jitter_s,
      stringsAsFactors = FALSE
    )
    observed <- observed[order(observed$Train, observed$Observed_Spike_Index), , drop = FALSE]
    rownames(observed) <- NULL
    rownames(obs_map) <- NULL

    summary <- observation_summary_from_map(
      obs_map,
      observed,
      train_ids,
      total_time,
      observation_model_label(obs),
      candidates = candidates
    )
    list(observed_spikes = observed, observation_map = obs_map, observation_summary = summary,
         model_label = observation_model_label(obs))
  }

  observed_spike_events_table <- function(sim) {
    observed <- if (!is.null(sim$combined_observed_spikes)) sim$combined_observed_spikes else make_empty_observed_spike_df()
    cols <- names(make_empty_observed_spike_df())
    missing <- setdiff(cols, names(observed))
    for (col in missing) observed[[col]] <- NA
    observed[, cols, drop = FALSE]
  }

  latent_spike_events_input_table <- function(sim) {
    spikes <- if (!is.null(sim$combined_spikes)) sim$combined_spikes else make_empty_spike_df()
    if (is.null(spikes) || nrow(spikes) == 0) {
      return(data.frame(Train = integer(0), Spike_Index = integer(0), Time = numeric(0), stringsAsFactors = FALSE))
    }
    train <- if ("Train" %in% names(spikes)) suppressWarnings(as.integer(spikes$Train)) else rep(1L, nrow(spikes))
    time <- if ("Time" %in% names(spikes)) suppressWarnings(as.numeric(spikes$Time)) else rep(NA_real_, nrow(spikes))
    valid <- is.finite(train) & is.finite(time)
    if (!any(valid)) {
      return(data.frame(Train = integer(0), Spike_Index = integer(0), Time = numeric(0), stringsAsFactors = FALSE))
    }
    out <- data.frame(
      Train = train[valid],
      Time = time[valid],
      stringsAsFactors = FALSE
    )
    out <- out[order(out$Train, out$Time), , drop = FALSE]
    out$Spike_Index <- ave(out$Time, out$Train, FUN = seq_along)
    out <- out[, c("Train", "Spike_Index", "Time"), drop = FALSE]
    rownames(out) <- NULL
    out
  }

  observed_spike_events_input_table <- function(sim) {
    observed <- observed_spike_events_table(sim)
    if (is.null(observed) || nrow(observed) == 0) {
      return(data.frame(Train = integer(0), Observed_Spike_Index = integer(0), Time = numeric(0), stringsAsFactors = FALSE))
    }
    out <- data.frame(
      Train = suppressWarnings(as.integer(observed$Train)),
      Observed_Spike_Index = suppressWarnings(as.integer(observed$Observed_Spike_Index)),
      Time = suppressWarnings(as.numeric(observed$Time)),
      stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$Train) & is.finite(out$Observed_Spike_Index) & is.finite(out$Time), , drop = FALSE]
    out <- out[order(out$Train, out$Observed_Spike_Index, out$Time), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  external_stimulus_table_input <- function(stimulus_data, strict = TRUE) {
    external_cols <- c(
      "Train", "Stimulus_ID", "Onset_s", "Duration_s", "Stimulus_Type", "Protocol",
      "Channel", "External_Strength", "Feature_Modality", "Stimulus_Feature_Value",
      "Stimulus_Position_X", "Stimulus_Position_Y", "Is_Standard", "Is_Deviant",
      "Pair_ID", "Repetition_Index", "Inter_Stimulus_Interval_s"
    )
    if (is.null(stimulus_data) || nrow(stimulus_data) == 0) {
      out <- data.frame(stringsAsFactors = FALSE)
      for (col in external_cols) out[[col]] <- switch(
        col,
        Train = integer(0),
        Stimulus_ID = integer(0),
        Pair_ID = integer(0),
        Repetition_Index = integer(0),
        Is_Standard = logical(0),
        Is_Deviant = logical(0),
        Onset_s = numeric(0),
        Duration_s = numeric(0),
        External_Strength = numeric(0),
        Stimulus_Feature_Value = numeric(0),
        Stimulus_Position_X = numeric(0),
        Stimulus_Position_Y = numeric(0),
        Inter_Stimulus_Interval_s = numeric(0),
        character(0)
      )
      return(out[, external_cols, drop = FALSE])
    }
    if (!"External_Strength" %in% names(stimulus_data)) {
      msg <- paste(
        "External_Strength is required for detector-visible external stimulus input.",
        "Refusing to fall back to unit-modulated Strength.",
        "This object may have been generated before External_Strength was introduced or modified outside the simulator.",
        "Re-run the simulation with V13.5.0 or later to produce strict detector-visible stimulus inputs."
      )
      if (isTRUE(strict)) {
        stop(msg, call. = FALSE)
      }
      stimulus_data$External_Strength <- NA_real_
    }
    keep <- intersect(external_cols, names(stimulus_data))
    out <- stimulus_data[, keep, drop = FALSE]
    missing <- setdiff(external_cols, names(out))
    for (col in missing) {
      out[[col]] <- switch(
        col,
        Train = NA_integer_,
        Stimulus_ID = NA_integer_,
        Pair_ID = NA_integer_,
        Repetition_Index = NA_integer_,
        Is_Standard = NA,
        Is_Deviant = NA,
        Onset_s = NA_real_,
        Duration_s = NA_real_,
        External_Strength = NA_real_,
        Stimulus_Feature_Value = NA_real_,
        Stimulus_Position_X = NA_real_,
        Stimulus_Position_Y = NA_real_,
        Inter_Stimulus_Interval_s = NA_real_,
        NA_character_
      )
    }
    out <- out[, external_cols, drop = FALSE]
    out$External_Strength <- suppressWarnings(as.numeric(out$External_Strength))
    bad_strength <- !is.finite(out$External_Strength)
    if (nrow(out) > 0 && any(bad_strength)) {
      msg <- paste(
        "External_Strength must be finite and non-missing for every detector-visible external stimulus input row.",
        paste0("Invalid row count: ", sum(bad_strength), "."),
        "Use an explicit default such as External_Strength = 1.0 for stimuli without a graded strength.",
        "Refusing to export ambiguous detector-visible stimulus inputs."
      )
      if (isTRUE(strict)) {
        stop(msg, call. = FALSE)
      }
    }
    order_cols <- intersect(c("Train", "Onset_s", "Stimulus_ID"), names(out))
    if (length(order_cols) > 0 && nrow(out) > 0) {
      ord <- do.call(order, out[order_cols])
      out <- out[ord, , drop = FALSE]
    }
    rownames(out) <- NULL
    out
  }

  interval_table <- function(spikes, episodes = NULL, intervals = NULL) {
    if (is.null(intervals)) intervals <- build_interval_table(spikes, episodes)
    intervals
  }

  spike_train_matrix_table <- function(spikes, digits = 12) {
    if (is.null(spikes) || nrow(spikes) == 0 || !"Train" %in% names(spikes) || !"Time" %in% names(spikes)) {
      return(data.frame(Spike_Index = integer(0), stringsAsFactors = FALSE))
    }

    train_ids <- sort(unique(as.integer(spikes$Train[is.finite(spikes$Train)])))
    if (length(train_ids) == 0) {
      return(data.frame(Spike_Index = integer(0), stringsAsFactors = FALSE))
    }

    train_times <- lapply(train_ids, function(train_id) {
      times <- sort(as.numeric(spikes$Time[spikes$Train == train_id]))
      times[is.finite(times)]
    })
    max_spikes <- max(vapply(train_times, length, integer(1)), 0L)
    if (max_spikes == 0) {
      return(data.frame(Spike_Index = integer(0), stringsAsFactors = FALSE))
    }

    out <- data.frame(Spike_Index = seq_len(max_spikes), stringsAsFactors = FALSE)
    for (i in seq_along(train_ids)) {
      col <- rep(NA_real_, max_spikes)
      times <- train_times[[i]]
      if (length(times) > 0) col[seq_along(times)] <- round(times, digits)
      out[[paste0("Train_", train_ids[i], "_Time_s")]] <- col
    }
    out
  }

  spike_matrix_col_labels <- function(lang, cols) {
    labels <- cols
    labels[cols == "Spike_Index"] <- if (lang == "zh") "Spike 序号" else "Spike index"
    train_cols <- grepl("^Train_[0-9]+_Time_s$", cols)
    if (any(train_cols)) {
      train_id <- sub("^Train_([0-9]+)_Time_s$", "\\1", cols[train_cols])
      labels[train_cols] <- if (lang == "zh") {
        paste0("Spike train ", train_id, " 时间戳 (s)")
      } else {
        paste0("Spike train ", train_id, " timestamp (s)")
      }
    }
    labels
  }

  achieved_duration_for_train <- function(spikes, episodes, requested_duration = safe_total_time()) {
    candidates <- numeric(0)
    if (!is.null(episodes) && nrow(episodes) > 0 && "End" %in% names(episodes)) {
      episode_end <- suppressWarnings(as.numeric(episodes$End))
      candidates <- c(candidates, episode_end[is.finite(episode_end)])
    }
    if (!is.null(spikes) && nrow(spikes) > 0 && "Time" %in% names(spikes)) {
      spike_time <- suppressWarnings(as.numeric(spikes$Time))
      candidates <- c(candidates, spike_time[is.finite(spike_time)])
    }

    achieved <- if (length(candidates) > 0) max(candidates) else 0
    if (!is.finite(achieved)) achieved <- 0
    achieved <- max(0, achieved)
    if (is.finite(requested_duration) && requested_duration > 0) {
      achieved <- min(achieved, requested_duration)
    }
    achieved
  }

  duration_summary_df <- function(sim, requested_duration = safe_total_time(), digits = 6) {
    n_train <- generated_train_count(sim)
    if (!is.finite(requested_duration) || requested_duration <= 0) requested_duration <- NA_real_

    rows <- lapply(seq_len(n_train), function(train_id) {
      train_spikes <- if (length(sim$spikes_list) >= train_id) {
        sim$spikes_list[[train_id]]
      } else {
        sim$combined_spikes[sim$combined_spikes$Train == train_id, , drop = FALSE]
      }
      train_episodes <- if (length(sim$episodes_list) >= train_id) {
        sim$episodes_list[[train_id]]
      } else {
        sim$combined_episodes[sim$combined_episodes$Train == train_id, , drop = FALSE]
      }
      achieved <- achieved_duration_for_train(train_spikes, train_episodes, requested_duration)
      shortfall <- if (is.finite(requested_duration)) max(0, requested_duration - achieved) else NA_real_
      completion <- if (is.finite(requested_duration) && requested_duration > 0) {
        100 * achieved / requested_duration
      } else {
        NA_real_
      }

      data.frame(
        Train = train_id,
        Requested_Duration_s = round(requested_duration, digits),
        Achieved_Duration_s = round(achieved, digits),
        Duration_Shortfall_s = round(shortfall, digits),
        Duration_Completion_pct = round(completion, 2),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, rows)
  }

  add_duration_columns <- function(df, sim) {
    duration_df <- duration_summary_df(sim, digits = 12)
    duration_cols <- setdiff(names(duration_df), "Train")

    if ("Train" %in% names(df)) {
      idx <- match(as.integer(df$Train), duration_df$Train)
      for (col in duration_cols) {
        df[[col]] <- duration_df[[col]][idx]
      }
      return(df)
    }

    if (nrow(df) == 0) {
      df$Requested_Duration_s <- numeric(0)
      for (train_id in duration_df$Train) {
        df[[paste0("Train_", train_id, "_Achieved_Duration_s")]] <- numeric(0)
        df[[paste0("Train_", train_id, "_Duration_Shortfall_s")]] <- numeric(0)
        df[[paste0("Train_", train_id, "_Duration_Completion_pct")]] <- numeric(0)
      }
      return(df)
    }

    df$Requested_Duration_s <- duration_df$Requested_Duration_s[1]
    for (i in seq_len(nrow(duration_df))) {
      train_id <- duration_df$Train[i]
      df[[paste0("Train_", train_id, "_Achieved_Duration_s")]] <- duration_df$Achieved_Duration_s[i]
      df[[paste0("Train_", train_id, "_Duration_Shortfall_s")]] <- duration_df$Duration_Shortfall_s[i]
      df[[paste0("Train_", train_id, "_Duration_Completion_pct")]] <- duration_df$Duration_Completion_pct[i]
    }
    df
  }

  add_reproducibility_columns <- function(df, sim) {
    cfg_hash <- if (!is.null(sim) && !is.null(sim$config)) config_hash_from_config(sim$config) else NA_character_
    generation_key <- if (!is.null(sim) && !is.null(sim$generation_key)) as.character(sim$generation_key)[1] else NA_character_
    seed_value <- if (!is.null(sim) && !is.null(sim$seed)) suppressWarnings(as.integer(sim$seed)[1]) else NA_integer_
    verification_code <- if (!is.null(sim) && !is.null(sim$verification_code)) as.character(sim$verification_code)[1] else NA_character_
    if (nrow(df) == 0) {
      df$Schema_Version <- character(0)
      df$Simulator_Version <- character(0)
      df$Config_Hash <- character(0)
      df$Generation_Key <- character(0)
      df$Derived_RNG_Seed <- integer(0)
      df$Verification_Code <- character(0)
      return(df)
    }
    df$Schema_Version <- rep(SCHEMA_VERSION, nrow(df))
    df$Simulator_Version <- rep(SIMULATOR_VERSION, nrow(df))
    df$Config_Hash <- rep(cfg_hash, nrow(df))
    df$Generation_Key <- rep(generation_key, nrow(df))
    df$Derived_RNG_Seed <- rep(seed_value, nrow(df))
    df$Verification_Code <- rep(verification_code, nrow(df))
    df
  }

  round_numeric_df <- function(df, digits = 12) {
    if (nrow(df) == 0) return(df)
    for (col in names(df)) {
      if (is.numeric(df[[col]])) df[[col]] <- signif(df[[col]], digits)
    }
    df
  }

  hash_numeric_code <- function(hash) {
    hash <- tolower(as.character(hash))
    if (length(hash) == 0 || nchar(hash[1]) < 12) return(NA_character_)
    chunks <- substring(substr(hash[1], 1, 12), seq(1, 10, by = 3), seq(3, 12, by = 3))
    values <- strtoi(chunks, base = 16)
    if (any(!is.finite(values))) return(NA_character_)
    code <- 0
    for (value in values) {
      code <- (code * 4096 + value) %% 1000000000000
    }
    sprintf("%012.0f", code)
  }

  simulation_verification <- function(seed, train_count, config, spikes, episodes,
                                      intervals = NULL, stimuli = NULL, responses = NULL,
                                      event_epochs = NULL) {
    if (!requireNamespace("digest", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
      return(list(code = NA_character_, hash = NA_character_))
    }
    if (is.null(intervals)) intervals <- build_interval_table(spikes, episodes)
    if (is.null(stimuli)) stimuli <- make_empty_stimulus_df()
    if (is.null(responses)) responses <- make_empty_response_df()
    if (is.null(event_epochs)) event_epochs <- event_epochs_from_intervals(intervals)

    payload <- list(
      simulator = SIMULATOR_ID,
      simulator_version = SIMULATOR_VERSION,
      schema_version = SCHEMA_VERSION,
      config_hash = config_hash_from_config(config),
      generation_key = value_or(config$generation_key, NA_character_),
      seed = if (is.finite(seed)) as.integer(seed) else NA_integer_,
      train_count = as.integer(train_count),
      config = config,
      spike_matrix = spike_train_matrix_table(spikes, digits = 12),
      spike_event_table = round_numeric_df(spike_isi_table(spikes), digits = 12),
      interval_table = round_numeric_df(intervals, digits = 12),
      episodes = round_numeric_df(episodes, digits = 12),
      stimulus_table = round_numeric_df(stimuli, digits = 12),
      stimulus_response_table = round_numeric_df(responses, digits = 12),
      event_epoch_table = round_numeric_df(event_epochs, digits = 12)
    )
    payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null", digits = 15)
    hash <- digest::digest(payload_json, algo = "sha256", serialize = FALSE)
    list(code = hash_numeric_code(hash), hash = hash)
  }

  stimulus_trial_table <- function(sim) {
    stim <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df()
    if (nrow(stim) == 0) return(data.frame())
    stim <- stim[order(stim$Train, stim$Onset_s, stim$Stimulus_ID), , drop = FALSE]
    stim$Trial_Key <- seq_len(nrow(stim))
    stim
  }

  stimulus_aligned_spike_table <- function(sim, window = c(-1, 1.5)) {
    spikes <- if (!is.null(sim$combined_spikes)) sim$combined_spikes else make_empty_spike_df()
    trials <- stimulus_trial_table(sim)
    if (nrow(spikes) == 0 || nrow(trials) == 0) return(data.frame())
    rows <- list()
    idx <- 0L
    for (i in seq_len(nrow(trials))) {
      train_id <- trials$Train[i]
      train_spikes <- spikes[spikes$Train == train_id & is.finite(spikes$Time), , drop = FALSE]
      if (nrow(train_spikes) == 0) next
      rel <- as.numeric(train_spikes$Time) - trials$Onset_s[i]
      keep <- is.finite(rel) & rel >= window[1] & rel <= window[2]
      if (!any(keep)) next
      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        Train = train_id,
        Trial_Key = trials$Trial_Key[i],
        Stimulus_ID = trials$Stimulus_ID[i],
        Stimulus_Type = as.character(trials$Stimulus_Type[i]),
        Strength = as.numeric(trials$Strength[i]),
        Relative_Time_s = rel[keep],
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0) return(data.frame())
    do.call(rbind, rows)
  }

  stimulus_psth_tables <- function(sim, window = c(-1, 1.5), bin_width = 0.05) {
    trials <- stimulus_trial_table(sim)
    if (nrow(trials) == 0) return(list(summary = data.frame(), trial_bins = data.frame()))
    bin_width <- max(0.001, safe_num(bin_width, 0.05))
    n_bins <- max(1L, as.integer(ceiling(diff(window) / bin_width)))
    bins <- data.frame(
      Bin = seq.int(0L, n_bins - 1L),
      Bin_Start_s = window[1] + seq.int(0L, n_bins - 1L) * bin_width,
      stringsAsFactors = FALSE
    )
    bins$Bin_Center_s <- bins$Bin_Start_s + bin_width / 2
    trial_grid <- merge(trials[, c("Trial_Key", "Stimulus_Type"), drop = FALSE], bins, by = NULL)
    aligned <- stimulus_aligned_spike_table(sim, window = window)
    if (nrow(aligned) > 0) {
      aligned$Bin <- floor((aligned$Relative_Time_s - window[1]) / bin_width)
      aligned <- aligned[aligned$Bin >= 0 & aligned$Bin < n_bins, , drop = FALSE]
    }
    if (nrow(aligned) > 0) {
      counts <- aggregate(rep(1L, nrow(aligned)), by = list(Trial_Key = aligned$Trial_Key, Bin = aligned$Bin), FUN = sum)
      names(counts)[3] <- "Spike_Count"
      key_grid <- paste(trial_grid$Trial_Key, trial_grid$Bin, sep = "|")
      key_counts <- paste(counts$Trial_Key, counts$Bin, sep = "|")
      trial_grid$Spike_Count <- 0L
      hit <- match(key_grid, key_counts)
      trial_grid$Spike_Count[!is.na(hit)] <- counts$Spike_Count[hit[!is.na(hit)]]
    } else {
      trial_grid$Spike_Count <- 0L
    }
    trial_grid$Rate_Hz <- trial_grid$Spike_Count / bin_width
    split_bins <- split(trial_grid, trial_grid$Bin)
    summary_rows <- lapply(split_bins, function(df) {
      data.frame(
        Bin = df$Bin[1],
        Bin_Start_s = df$Bin_Start_s[1],
        Bin_Center_s = df$Bin_Center_s[1],
        Mean_Rate_Hz = mean(df$Rate_Hz),
        SEM_Rate_Hz = if (nrow(df) > 1) stats::sd(df$Rate_Hz) / sqrt(nrow(df)) else NA_real_,
        N_Trials = nrow(df),
        Total_Spikes = sum(df$Spike_Count),
        stringsAsFactors = FALSE
      )
    })
    list(summary = do.call(rbind, summary_rows), trial_bins = trial_grid)
  }

  nwb_mapping_payload <- function(sim = NULL) {
    list(
      schema_name = "SpikeTrainSimulatorV13_multi_table_ground_truth",
      schema_version = SCHEMA_VERSION,
      simulator_version = SIMULATOR_VERSION,
      simulator_id = SIMULATOR_ID,
      config_hash = if (!is.null(sim) && !is.null(sim$config)) config_hash_from_config(sim$config) else NA_character_,
      purpose = "NWB-compatible schema mapping for latent spike events, stimulus trials, response annotations, intervals, episodes, and event epochs.",
      native_nwb_export = FALSE,
      format_note = "This JSON describes how simulator tables map onto NWB objects. It is not a native .nwb file export.",
      schema_notes = list(
        Feature_Response_Eligible = "Deprecated compatibility alias of Response_Eligible; it does not mean Feature_Matched.",
        Feature_Drive = "Modulated unit-specific drive. Raw tuning components are represented by Feature_Excitation and Feature_Suppression.",
        External_Strength = "Unmodulated external stimulus strength. Unit-modulated Strength is scoring-only audit metadata and is not detector-visible.",
        external_stimulus_table_input = "Detector-visible stimulus input may be duplicated per Train for detector convenience; repeated rows contain only externally defined stimulus attributes and no unit-specific drive, tuning, response, or audit fields.",
        deprecated_noop_config_fields = c("feature_neutral_response_probability", "feature_weak_response_probability"),
        validation_roles = list(
          state_dependent = "default_behavior_check; insufficient state coverage is reported as indeterminate rather than a biological failure.",
          state_dependent_balanced = "balanced_quantitative_validation; insufficient state coverage is a validation failure for this preset."
        )
      ),
      nwb_targets = list(
        spike_events = list(nwb_object = "Units", fields = list(Time = "spike_times", Train = "unit_id / custom unit grouping")),
        observed_spike_events = list(nwb_object = "Units or processing/observed_units", fields = list(Time = "observed_spike_times", Observation_Source = "observation_source", Latent_Spike_Index = "latent_spike_reference")),
        observation_map = list(nwb_object = "processing/observation_model/custom table", fields = list(Latent_Time_s = "latent_spike_time", Observed_Time_s = "observed_spike_time", Observation_Status = "detected / missed / false_positive / merged", Merged_To_Event_ID = "observed event absorbing a dead-time-merged latent spike")),
        stimulus_table = list(nwb_object = "Trials or TimeIntervals", fields = list(Onset_s = "start_time", Duration_s = "stop_time - start_time", Stimulus_Type = "stimulus_type", External_Strength = "stimulus_strength")),
        response_table = list(nwb_object = "processing/behavior or custom TimeIntervals", fields = list(Response_Window_Start_s = "start_time", Response_Window_End_s = "stop_time", Response_Type = "response_type", Response_Latency_s = "response_latency")),
        unit_stimulus_drive_table = list(nwb_object = "processing/stimulus_response/custom table", fields = list(Train = "unit_id", Stimulus_ID = "stimulus_id", Feature_Drive = "unit_specific_stimulus_drive", Feature_Matched = "feature_match", Drive_Above_Threshold = "drive_threshold_flag", Response_Kernel = "response_kernel", Response_Eligible = "evoked_response_eligibility", Feature_Response_Eligible = "legacy_response_eligibility", Feature_Response_Reason = "response_reason")),
        interval_table = list(nwb_object = "custom TimeIntervals or NDX candidate", fields = list(Start_Time_s = "start_time", End_Time_s = "stop_time", ISI_Label = "interval_label", Stimulus_ID = "stimulus_id")),
        episode_table = list(nwb_object = "custom TimeIntervals", fields = list(Start = "start_time", End = "stop_time", Pattern = "episode_label", Episode_Scope = "episode_scope")),
	        event_epoch_table = list(
	          nwb_object = "TimeIntervals",
	          fields = list(
	            Start_s = "start_time",
	            End_s = "stop_time",
	            Epoch_Type = "epoch_type, e.g. evoked_burst_epoch / rebound_burst_epoch / suppression_epoch / recovery_epoch",
	            Epoch_Class = "coarse event class: evoked_spiking / suppression / recovery / timing / response_failure",
	            Response_Component = "source response component such as evoked_burst_1, rebound_burst_1, or post_stimulus_recovery",
	            Interval_ID_Start = "first latent interval included in the epoch",
	            Interval_ID_End = "last latent interval included in the epoch",
	            Scorable = "whether all underlying intervals are scorable pattern intervals"
	          )
	        )
	      ),
      exported_tables = list(
        detector_inputs = "detector_inputs/*_input.csv files contain only detector-visible spike and external stimulus fields",
        latent_spike_events_audit = "ground_truth/*_latent_spike_events_audit.csv / *_spike_events_audit.csv",
        observed_spike_events_audit = "ground_truth/*_observed_spike_events_audit.csv",
        observation_map = "ground_truth/*_observation_map.csv linking latent spikes, observed detections, missed spikes, false positives, and dead-time merges",
        interval_ground_truth = "ground_truth/*_interval_table.csv",
        episode_ground_truth = "ground_truth/*_episodes.csv",
        stimulus_events_audit = "ground_truth/*_stimulus_table_audit.csv",
        stimulus_responses = "ground_truth/*_stimulus_response_table.csv",
        unit_stimulus_drive = "ground_truth/*_unit_stimulus_drive_table.csv",
        event_epochs = "ground_truth/*_event_epoch_table.csv",
        config = "metadata/*_reproduction_code.txt or config JSON in validation outputs"
      ),
      generated_train_count = if (!is.null(sim)) generated_train_count(sim) else NA_integer_,
      observation_model = if (!is.null(sim)) value_or(sim$observation_model, "identity") else NA_character_,
      verification_hash = if (!is.null(sim)) value_or(sim$verification_hash, NA_character_) else NA_character_
    )
  }

  line_colors <- function() {
    c(
      "Burst" = color_value(input$col_burst_line, NATURE_PATTERN_COLORS["Burst"]),
      "Pause" = color_value(input$col_pause_line, NATURE_PATTERN_COLORS["Pause"]),
      "Tonic" = color_value(input$col_tonic_line, NATURE_PATTERN_COLORS["Tonic"]),
      "high_frequency_tonic" = color_value(input$col_hft_line, NATURE_PATTERN_COLORS["high_frequency_tonic"]),
      "high_frequency_spiking" = color_value(input$col_hfs_line, NATURE_PATTERN_COLORS["high_frequency_spiking"]),
      "Noisy" = color_value(input$col_noisy_line, NATURE_PATTERN_COLORS["Noisy"])
    )
  }

  ratio_vector <- function() {
    ratios <- c(
      "Burst" = input$ratio_burst,
      "Pause" = input$ratio_pause,
      "Tonic" = input$ratio_tonic,
      "high_frequency_tonic" = input$ratio_hft,
      "high_frequency_spiking" = input$ratio_hfs,
      "Noisy" = input$ratio_noisy
    )
    normalize_pattern_ratios(ratios)
  }

  get_dist_type <- function(pattern) {
    switch(pattern,
           "Burst" = input$dist_burst,
           "Pause" = input$dist_pause,
           "Tonic" = input$dist_tonic,
           "high_frequency_tonic" = input$dist_hft,
           "high_frequency_spiking" = input$dist_hfs,
           "Noisy" = input$dist_noisy)
  }

  get_interval_range <- function(pattern) {
    rng <- switch(pattern,
                  "Burst" = input$interval_range_burst,
                  "Pause" = input$pause_duration_range,
                  "Tonic" = input$interval_range_tonic,
                  "high_frequency_tonic" = input$interval_range_hft,
                  "high_frequency_spiking" = input$interval_range_hfs,
                  "Noisy" = input$interval_range_noisy)
    global_min <- effective_inter_event_gap()
    if (length(rng) == 2 && is.finite(global_min)) rng[1] <- max(as.numeric(rng[1]), global_min)
    rng
  }

  valid_nonnegative_range <- function(rng) {
    length(rng) == 2 && all(is.finite(rng)) && rng[1] >= 0 && rng[2] >= rng[1]
  }

  get_params <- function(pattern) {
    dist_type <- get_dist_type(pattern)
    if (dist_type == "Exponential") {
      list(mean = switch(pattern,
                         "Burst" = input$burst_exp_mean,
                         "Pause" = input$pause_exp_mean,
                         "Tonic" = input$tonic_exp_mean,
                         "high_frequency_tonic" = input$hft_exp_mean,
                         "high_frequency_spiking" = input$hfs_exp_mean,
                         "Noisy" = input$noisy_exp_mean))
    } else if (dist_type == "Gamma") {
      list(shape = switch(pattern,
                          "Burst" = input$burst_gamma_shape,
                          "Pause" = input$pause_gamma_shape,
                          "Tonic" = input$tonic_gamma_shape,
                          "high_frequency_tonic" = input$hft_gamma_shape,
                          "high_frequency_spiking" = input$hfs_gamma_shape,
                          "Noisy" = input$noisy_gamma_shape),
           scale = switch(pattern,
                          "Burst" = input$burst_gamma_scale,
                          "Pause" = input$pause_gamma_scale,
                          "Tonic" = input$tonic_gamma_scale,
                          "high_frequency_tonic" = input$hft_gamma_scale,
                          "high_frequency_spiking" = input$hfs_gamma_scale,
                          "Noisy" = input$noisy_gamma_scale))
    } else if (dist_type == "Normal") {
      list(mean = switch(pattern,
                         "Burst" = input$burst_norm_mean,
                         "Pause" = input$pause_norm_mean,
                         "Tonic" = input$tonic_norm_mean,
                         "high_frequency_tonic" = input$hft_norm_mean,
                         "high_frequency_spiking" = input$hfs_norm_mean,
                         "Noisy" = input$noisy_norm_mean),
           sd = switch(pattern,
                       "Burst" = input$burst_norm_sd,
                       "Pause" = input$pause_norm_sd,
                       "Tonic" = input$tonic_norm_sd,
                       "high_frequency_tonic" = input$hft_norm_sd,
                       "high_frequency_spiking" = input$hfs_norm_sd,
                       "Noisy" = input$noisy_norm_sd))
    } else if (dist_type == "Uniform") {
      list(min = switch(pattern,
                        "Burst" = input$burst_unif_min,
                        "Pause" = input$pause_unif_min,
                        "Tonic" = input$tonic_unif_min,
                        "high_frequency_tonic" = input$hft_unif_min,
                        "high_frequency_spiking" = input$hfs_unif_min,
                        "Noisy" = input$noisy_unif_min),
           max = switch(pattern,
                        "Burst" = input$burst_unif_max,
                        "Pause" = input$pause_unif_max,
                        "Tonic" = input$tonic_unif_max,
                        "high_frequency_tonic" = input$hft_unif_max,
                        "high_frequency_spiking" = input$hfs_unif_max,
                        "Noisy" = input$noisy_unif_max))
    } else if (dist_type == "Lognormal") {
      list(meanlog = switch(pattern,
                            "Burst" = input$burst_lognorm_meanlog,
                            "Pause" = input$pause_lognorm_meanlog,
                            "Tonic" = input$tonic_lognorm_meanlog,
                            "high_frequency_tonic" = input$hft_lognorm_meanlog,
                            "high_frequency_spiking" = input$hfs_lognorm_meanlog,
                            "Noisy" = input$noisy_lognorm_meanlog),
           sdlog = switch(pattern,
                          "Burst" = input$burst_lognorm_sdlog,
                          "Pause" = input$pause_lognorm_sdlog,
                          "Tonic" = input$tonic_lognorm_sdlog,
                          "high_frequency_tonic" = input$hft_lognorm_sdlog,
                          "high_frequency_spiking" = input$hfs_lognorm_sdlog,
                          "Noisy" = input$noisy_lognorm_sdlog))
    } else if (dist_type == "Inverse Gaussian") {
      list(mean = switch(pattern,
                         "Burst" = input$burst_invgauss_mean,
                         "Pause" = input$pause_invgauss_mean,
                         "Tonic" = input$tonic_invgauss_mean,
                         "high_frequency_tonic" = input$hft_invgauss_mean,
                         "high_frequency_spiking" = input$hfs_invgauss_mean,
                         "Noisy" = input$noisy_invgauss_mean),
           shape = switch(pattern,
                          "Burst" = input$burst_invgauss_shape,
                          "Pause" = input$pause_invgauss_shape,
                          "Tonic" = input$tonic_invgauss_shape,
                          "high_frequency_tonic" = input$hft_invgauss_shape,
                          "high_frequency_spiking" = input$hfs_invgauss_shape,
                          "Noisy" = input$noisy_invgauss_shape))
    } else {
      list()
    }
  }

  density_noisy_specificity <- function() {
    list(
      avoid_mode_overlap = FALSE,
      contextual_mode_overlap = TRUE,
      tolerance = NOISY_CONTEXT_GUARD_S,
      clean_guard_s = NOISY_CONTEXT_GUARD_S,
      pause_guard_s = NOISY_PAUSE_GUARD_S,
      pause_guard_ratio = NOISY_PAUSE_GUARD_RATIO,
      tonic_upper_multiplier = NOISY_TONIC_UPPER_MULTIPLIER,
      mm_ratio = as.numeric(value_or(input$noisy_mm_ratio, NOISY_MIN_MM_RATIO))
    )
  }

  subtract_interval_segment <- function(segments, blocked) {
    if (is.null(segments) || nrow(segments) == 0) return(segments)
    if (length(blocked) != 2 || any(!is.finite(blocked)) || blocked[2] < blocked[1]) {
      return(segments)
    }

    out <- data.frame(Start = numeric(0), End = numeric(0))
    for (i in seq_len(nrow(segments))) {
      seg <- as.numeric(segments[i, c("Start", "End")])
      if (blocked[2] <= seg[1] || blocked[1] >= seg[2]) {
        out <- rbind(out, data.frame(Start = seg[1], End = seg[2]))
      } else {
        if (blocked[1] > seg[1]) {
          out <- rbind(out, data.frame(Start = seg[1], End = min(blocked[1], seg[2])))
        }
        if (blocked[2] < seg[2]) {
          out <- rbind(out, data.frame(Start = max(blocked[2], seg[1]), End = seg[2]))
        }
      }
    }

    out <- out[is.finite(out$Start) & is.finite(out$End) & out$End > out$Start, , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  effective_density_segments <- function(pattern) {
    rng <- get_interval_range(pattern)
    if (length(rng) != 2 || any(is.na(rng)) || rng[2] <= rng[1]) {
      return(data.frame(Start = numeric(0), End = numeric(0)))
    }

    segments <- data.frame(Start = as.numeric(rng[1]), End = as.numeric(rng[2]))
    if (identical(pattern, "Noisy")) {
      spec <- density_noisy_specificity()
      # Static display clips Noisy to the absolute-refractory-to-Tonic-upper
      # envelope and keeps it below the Pause lower bound. Burst-like/Tonic-like
      # singleton Noisy intervals remain context-dependent and are handled by
      # simulation-based effective density when context matters.
      tonic_rng <- get_interval_range("Tonic")
      pause_rng <- get_interval_range("Pause")
      upper_candidates <- numeric(0)
      if (length(tonic_rng) == 2 && all(is.finite(tonic_rng)) && tonic_rng[2] >= tonic_rng[1]) {
        upper_candidates <- c(upper_candidates, tonic_rng[2] * spec$tonic_upper_multiplier)
      }
      if (length(pause_rng) == 2 && all(is.finite(pause_rng)) && pause_rng[2] >= pause_rng[1]) {
        pause_guard <- max(spec$pause_guard_s, spec$pause_guard_ratio * pause_rng[1])
        upper_candidates <- c(upper_candidates, pause_rng[1] - pause_guard)
      }
      upper_candidates <- upper_candidates[is.finite(upper_candidates)]
      if (length(upper_candidates) > 0) {
        segments <- intersect_interval_segments(segments, c(effective_inter_event_gap(), min(upper_candidates)))
      }
      blocked <- get_interval_range("Pause")
      if (length(blocked) == 2 && all(is.finite(blocked)) && blocked[2] >= blocked[1]) {
        blocked <- c(max(0, blocked[1] - spec$tolerance), blocked[2] + spec$tolerance)
        segments <- subtract_interval_segment(segments, blocked)
      }
    }

    segments
  }

  x_in_segments <- function(x, segments) {
    if (nrow(segments) == 0) return(rep(FALSE, length(x)))
    Reduce(`|`, lapply(seq_len(nrow(segments)), function(i) {
      x >= segments$Start[i] & x <= segments$End[i]
    }))
  }

  sample_spike_count_for_density <- function(pattern) {
    rng <- switch(pattern,
                  "Burst" = input$spike_range_burst,
                  "Tonic" = input$spike_range_tonic,
                  "Noisy" = input$spike_range_noisy,
                  c(1, 1))
    rng <- as.integer(round(rng))
    if (length(rng) == 2 && pattern == "Tonic") rng[1] <- max(rng[1], 3L)
    if (length(rng) != 2 || any(is.na(rng)) || rng[2] < rng[1] || rng[1] < 1) {
      return(if (pattern == "Tonic") 3L else 1L)
    }
    candidates <- seq.int(rng[1], rng[2])
    candidates[sample.int(length(candidates), size = 1)]
  }

  sample_interval_for_density <- function(pattern, segments, max_attempts = 1000) {
    params <- get_params(pattern)
    dist_type <- get_dist_type(pattern)
    sample_truncated_interval_from_segments(dist_type, params, segments)
  }

  tonic_density_intervals_pass <- function(intervals) {
    ranges <- list(cv = input$tonic_cv_range, cv2 = input$tonic_cv2_range, lv = input$tonic_lv_range)
    metrics <- isi_regularity_metrics(intervals)
    valid_nonnegative_range(ranges$cv) && valid_nonnegative_range(ranges$cv2) &&
      valid_nonnegative_range(ranges$lv) &&
      is.finite(metrics$cv) && metrics$cv >= ranges$cv[1] && metrics$cv <= ranges$cv[2] &&
      is.finite(metrics$cv2) && metrics$cv2 >= ranges$cv2[1] && metrics$cv2 <= ranges$cv2[2] &&
      is.finite(metrics$lv) && metrics$lv >= ranges$lv[1] && metrics$lv <= ranges$lv[2]
  }

  density_cache <- new.env(parent = emptyenv())
  density_cache_order <- character(0)

  density_cache_key <- function(pattern, x_seq, target_samples, max_replicates) {
    parsed_sequence <- parse_pattern_sequence_strict(value_or(input$pattern_sequence, ""))
    cfg <- build_sim_config(parsed_sequence$tokens)
    payload <- list(
      pattern = pattern,
      x_min = min(x_seq, na.rm = TRUE),
      x_max = max(x_seq, na.rm = TRUE),
      x_n = length(x_seq),
      target_samples = target_samples,
      max_replicates = max_replicates,
      config = cfg
    )
    if (requireNamespace("digest", quietly = TRUE)) {
      paste0("density_", digest::digest(payload, algo = "sha256"))
    } else {
      paste(capture.output(str(payload, max.level = 10, vec.len = 1000, give.attr = FALSE)), collapse = "\n")
    }
  }

  density_cache_key_from_config <- function(config, pattern, x_seq, target_samples, max_replicates) {
    payload <- list(
      pattern = pattern,
      x_min = min(x_seq, na.rm = TRUE),
      x_max = max(x_seq, na.rm = TRUE),
      x_n = length(x_seq),
      target_samples = target_samples,
      max_replicates = max_replicates,
      config = config
    )
    if (requireNamespace("digest", quietly = TRUE)) {
      paste0("density_", digest::digest(payload, algo = "sha256"))
    } else {
      paste(capture.output(str(payload, max.level = 10, vec.len = 1000, give.attr = FALSE)), collapse = "\n")
    }
  }

  density_cache_get <- function(key) {
    if (!is.na(key) && exists(key, envir = density_cache, inherits = FALSE)) {
      return(get(key, envir = density_cache, inherits = FALSE))
    }
    NULL
  }

  density_cache_put <- function(key, value, max_entries = 48L) {
    if (is.na(key)) return(invisible(FALSE))
    assign(key, value, envir = density_cache)
    density_cache_order <<- c(density_cache_order[density_cache_order != key], key)
    if (length(density_cache_order) > max_entries) {
      to_remove <- head(density_cache_order, length(density_cache_order) - max_entries)
      rm(list = intersect(to_remove, ls(envir = density_cache, all.names = TRUE)), envir = density_cache)
      density_cache_order <<- tail(density_cache_order, max_entries)
    }
    invisible(TRUE)
  }

  simulated_effective_interval_density <- function(pattern, x_seq, target_samples = 300, max_replicates = 10) {
    cache_key <- tryCatch(density_cache_key(pattern, x_seq, target_samples, max_replicates), error = function(err) NA_character_)
    cached_density <- density_cache_get(cache_key)
    if (!is.null(cached_density)) return(cached_density)

    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(910003L + match(pattern, pattern_levels))

    parsed_sequence <- parse_pattern_sequence_strict(value_or(input$pattern_sequence, ""))
    cfg <- build_sim_config(parsed_sequence$tokens)
    density_time <- max(10, min(30, max(safe_total_time(), as.numeric(value_or(input$isi_xmax, 2)) * 12)))
    cfg$total_time <- density_time

    samples <- numeric(0)
    for (rep_i in seq_len(max_replicates)) {
      sim <- tryCatch(simulate_spike_train_core(cfg), error = function(err) NULL)
      if (is.null(sim)) next

      rows <- if (!is.null(sim$intervals)) sim$intervals else build_interval_table(sim$spikes, sim$episodes)
      if (nrow(rows) > 0) rows$Train <- 1L
      rows <- rows[
        is.finite(rows$ISI_s) &
          rows$ISI_s > 0 &
          rows$ISI_Label == pattern &
          rows$ISI_Scope %in% empirical_interval_scopes(),
        ,
        drop = FALSE
      ]
      if (nrow(rows) > 0) {
        samples <- c(samples, rows$ISI_s)
        if (length(samples) >= target_samples) break
      }
    }

    samples <- samples[is.finite(samples) & samples > 0]
    if (length(samples) < 30 || length(unique(samples)) < 2) {
      y <- rep(NA_real_, length(x_seq))
      density_cache_put(cache_key, y)
      return(y)
    }
    dens <- stats::density(samples, from = min(x_seq), to = max(x_seq), n = length(x_seq))
    y <- approx(dens$x, dens$y, xout = x_seq, rule = 2)$y
    y[x_seq < 0] <- 0
    area <- sum(diff(x_seq) * (head(y, -1) + tail(y, -1)) / 2)
    if (is.finite(area) && area > 0) y <- y / area
    density_cache_put(cache_key, y)
    y
  }

  simulated_effective_interval_density_from_config <- function(config, pattern, x_seq, target_samples = 300, max_replicates = 10) {
    cache_key <- tryCatch(density_cache_key_from_config(config, pattern, x_seq, target_samples, max_replicates), error = function(err) NA_character_)
    cached_density <- density_cache_get(cache_key)
    if (!is.null(cached_density)) return(cached_density)

    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(910003L + match(pattern, pattern_levels))

    cfg <- config
    density_time <- max(10, min(30, max(as.numeric(value_or(cfg$total_time, 10)), as.numeric(value_or(input$isi_xmax, 2)) * 12)))
    cfg$total_time <- density_time

    samples <- numeric(0)
    for (rep_i in seq_len(max_replicates)) {
      sim <- tryCatch(simulate_spike_train_core(cfg), error = function(err) NULL)
      if (is.null(sim)) next

      rows <- if (!is.null(sim$intervals)) sim$intervals else build_interval_table(sim$spikes, sim$episodes)
      if (nrow(rows) > 0) rows$Train <- 1L
      rows <- rows[
        is.finite(rows$ISI_s) &
          rows$ISI_s > 0 &
          rows$ISI_Label == pattern &
          rows$ISI_Scope %in% empirical_interval_scopes(),
        ,
        drop = FALSE
      ]
      if (nrow(rows) > 0) {
        samples <- c(samples, rows$ISI_s)
        if (length(samples) >= target_samples) break
      }
    }

    samples <- samples[is.finite(samples) & samples > 0]
    if (length(samples) < 30 || length(unique(samples)) < 2) {
      y <- rep(NA_real_, length(x_seq))
      density_cache_put(cache_key, y)
      return(y)
    }
    dens <- stats::density(samples, from = min(x_seq), to = max(x_seq), n = length(x_seq))
    y <- approx(dens$x, dens$y, xout = x_seq, rule = 2)$y
    y[x_seq < 0] <- 0
    area <- sum(diff(x_seq) * (head(y, -1) + tail(y, -1)) / 2)
    if (is.finite(area) && area > 0) y <- y / area
    density_cache_put(cache_key, y)
    y
  }

  static_interval_density_from_config <- function(config, pattern, x_seq) {
    pat_cfg <- config$patterns[[pattern]]
    if (is.null(pat_cfg)) return(rep(NA_real_, length(x_seq)))
    segments <- effective_pattern_segments_from_config(config, pattern)
    if (nrow(segments) == 0) return(rep(NA_real_, length(x_seq)))

    dens <- dist_density_value(pat_cfg$dist_type, pat_cfg$params, x_seq)
    if (all(is.na(dens))) return(dens)
    in_range <- x_in_segments(x_seq, segments)
    dens[!in_range] <- 0
    mass <- sum(vapply(seq_len(nrow(segments)), function(i) {
      dist_cdf_value(pat_cfg$dist_type, pat_cfg$params, segments$End[i]) -
        dist_cdf_value(pat_cfg$dist_type, pat_cfg$params, segments$Start[i])
    }, numeric(1)))
    if (is.finite(mass) && mass > 0) dens <- dens / mass
    dens
  }

  static_interval_density <- function(pattern, x_seq) {
    dist_type <- get_dist_type(pattern)
    params <- get_params(pattern)
    segments <- effective_density_segments(pattern)

    if (nrow(segments) == 0) return(rep(NA_real_, length(x_seq)))

    dens <- rep(NA_real_, length(x_seq))
    cdf_fun <- NULL

    if (dist_type == "Exponential") {
      if (is.null(params$mean) || is.na(params$mean) || params$mean <= 0) return(rep(NA_real_, length(x_seq)))
      dens <- dexp(x_seq, rate = 1 / params$mean)
      cdf_fun <- function(z) pexp(z, rate = 1 / params$mean)
    } else if (dist_type == "Gamma") {
      if (is.null(params$shape) || is.null(params$scale) ||
          is.na(params$shape) || is.na(params$scale) ||
          params$shape <= 0 || params$scale <= 0) return(rep(NA_real_, length(x_seq)))
      dens <- dgamma(x_seq, shape = params$shape, scale = params$scale)
      cdf_fun <- function(z) pgamma(z, shape = params$shape, scale = params$scale)
    } else if (dist_type == "Normal") {
      if (is.null(params$mean) || is.null(params$sd) ||
          is.na(params$mean) || is.na(params$sd) || params$sd < 0) return(rep(NA_real_, length(x_seq)))
      if (params$sd == 0) {
        if (!any(params$mean >= segments$Start & params$mean <= segments$End)) return(rep(NA_real_, length(x_seq)))
        return(point_mass_density_on_grid(x_seq, params$mean))
      }
      dens <- dnorm(x_seq, mean = params$mean, sd = params$sd)
      cdf_fun <- function(z) pnorm(z, mean = params$mean, sd = params$sd)
    } else if (dist_type == "Uniform") {
      if (is.null(params$min) || is.null(params$max) ||
          is.na(params$min) || is.na(params$max) || params$max <= params$min) return(rep(NA_real_, length(x_seq)))
      dens <- dunif(x_seq, min = params$min, max = params$max)
      cdf_fun <- function(z) punif(z, min = params$min, max = params$max)
    } else if (dist_type == "Lognormal") {
      if (is.null(params$meanlog) || is.null(params$sdlog) ||
          is.na(params$meanlog) || is.na(params$sdlog) || !is.finite(params$meanlog) ||
          !is.finite(params$sdlog) || params$sdlog < 0) return(rep(NA_real_, length(x_seq)))
      if (params$sdlog == 0) {
        point <- exp(params$meanlog)
        if (!any(point >= segments$Start & point <= segments$End)) return(rep(NA_real_, length(x_seq)))
        return(point_mass_density_on_grid(x_seq, point))
      }
      dens <- dlnorm(x_seq, meanlog = params$meanlog, sdlog = params$sdlog)
      cdf_fun <- function(z) plnorm(z, meanlog = params$meanlog, sdlog = params$sdlog)
    } else if (dist_type == "Inverse Gaussian") {
      if (is.null(params$mean) || is.null(params$shape) ||
          is.na(params$mean) || is.na(params$shape) || params$mean <= 0 || params$shape <= 0) return(rep(NA_real_, length(x_seq)))
      dens <- dinvgauss_value(x_seq, mean = params$mean, shape = params$shape)
      cdf_fun <- function(z) pinvgauss_value(z, mean = params$mean, shape = params$shape)
    } else {
      return(rep(NA_real_, length(x_seq)))
    }

    in_range <- x_in_segments(x_seq, segments)
    dens[!in_range] <- 0
    mass <- sum(vapply(seq_len(nrow(segments)), function(i) {
      cdf_fun(segments$End[i]) - cdf_fun(segments$Start[i])
    }, numeric(1)))
    if (is.finite(mass) && mass > 0) dens <- dens / mass
    dens
  }


  density_requires_effective_simulation <- function(pattern) {
    rho <- switch(pattern,
                  "Burst" = input_value("burst_isi_rho", 0),
                  "Pause" = input_value("pause_isi_rho", 0),
                  "Tonic" = input_value("tonic_isi_rho", 0),
                  "high_frequency_tonic" = input_value("hft_isi_rho", 0),
                  "high_frequency_spiking" = 0,
                  "Noisy" = input_value("noisy_isi_rho", 0),
                  0)
    trend <- switch(pattern,
                    "Burst" = input_value("burst_isi_trend", 0),
                    "Pause" = input_value("pause_isi_trend", 0),
                    "Tonic" = input_value("tonic_isi_trend", 0),
                    "high_frequency_tonic" = input_value("hft_isi_trend", 0),
                    "high_frequency_spiking" = 0,
                    "Noisy" = input_value("noisy_isi_trend", 0),
                    0)
    pattern %in% c("Tonic", "high_frequency_tonic", "high_frequency_spiking", "Noisy") ||
      (is.finite(as.numeric(rho)) && abs(as.numeric(rho)) > 1e-12) ||
      (is.finite(as.numeric(trend)) && abs(as.numeric(trend)) > 1e-12)
  }

  density_requires_effective_simulation_from_config <- function(config, pattern) {
    pat_cfg <- config$patterns[[pattern]]
    dep <- if (!is.null(pat_cfg)) pat_cfg$temporal_dependence else NULL
    rho <- as.numeric(value_or(dep$rho, 0))
    trend <- as.numeric(value_or(dep$trend, 0))
    pattern %in% c("Tonic", "high_frequency_tonic", "high_frequency_spiking", "Noisy") ||
      (is.finite(rho) && abs(rho) > 1e-12) ||
      (is.finite(trend) && abs(trend) > 1e-12)
  }

  interval_density <- function(pattern, x_seq, config = NULL) {
    if (!is.null(config)) {
      static_density <- static_interval_density_from_config(config, pattern, x_seq)
      if (!density_requires_effective_simulation_from_config(config, pattern)) return(static_density)

      simulated_density <- tryCatch(
        simulated_effective_interval_density_from_config(config, pattern, x_seq),
        error = function(err) rep(NA_real_, length(x_seq))
      )
      if (!all(is.na(simulated_density))) return(simulated_density)
      return(static_density)
    }

    static_density <- static_interval_density(pattern, x_seq)
    if (!density_requires_effective_simulation(pattern)) return(static_density)

    simulated_density <- tryCatch(
      simulated_effective_interval_density(pattern, x_seq),
      error = function(err) rep(NA_real_, length(x_seq))
    )
    if (!all(is.na(simulated_density))) return(simulated_density)
    static_density
  }

  build_theoretical_df <- function(patterns = NULL, config = NULL, x_max = NULL) {
    x_max <- as.numeric(value_or(x_max, input$isi_xmax))
    if (!is.finite(x_max) || x_max <= 0) x_max <- 2
    x_seq <- seq(0.001, x_max, length.out = 600)
    ratios <- if (!is.null(config) && !is.null(config$ratios)) {
      normalize_pattern_ratios(config$ratios)
    } else {
      ratio_vector()
    }
    dist_lines <- data.frame(x = numeric(0), y = numeric(0), Pattern = character(0), stringsAsFactors = FALSE)
    use_pattern_filter <- !is.null(patterns)
    if (isTRUE(use_pattern_filter)) {
      patterns <- unique(patterns)
      patterns <- patterns[patterns %in% pattern_levels]
      if (length(patterns) == 0) return(dist_lines)
    }
    manual_patterns <- character(0)
    if (!is.null(config)) {
      if (!is.null(config$pattern_sequence) && length(config$pattern_sequence) > 0) {
        manual_patterns <- unique(vapply(config$pattern_sequence, function(x) x$Pattern, character(1)))
        manual_patterns <- manual_patterns[manual_patterns %in% pattern_levels]
      }
    } else {
      parsed_manual <- parse_pattern_sequence_strict(value_or(input$pattern_sequence, ""))
      if (!is.null(parsed_manual$tokens) && length(parsed_manual$tokens) > 0) {
        manual_patterns <- unique(vapply(parsed_manual$tokens, function(x) x$Pattern, character(1)))
        manual_patterns <- manual_patterns[manual_patterns %in% pattern_levels]
      }
    }

    for (pat in pattern_levels) {
      if (isTRUE(use_pattern_filter) && !(pat %in% patterns)) next
      if (!isTRUE(use_pattern_filter) && length(manual_patterns) > 0 && !(pat %in% manual_patterns)) next
      if (ratios[pat] <= 0 && length(manual_patterns) == 0) next
      dens <- interval_density(pat, x_seq, config = config)
      if (all(is.na(dens))) next
      dist_lines <- rbind(
        dist_lines,
        data.frame(x = x_seq, y = dens, Pattern = pat, stringsAsFactors = FALSE)
      )
    }
    dist_lines
  }

  build_sim_config <- function(pattern_sequence = NULL) {
    list(
      total_time = input$total_time,
      inter_event_gap = effective_inter_event_gap(),
	      generation_mode = input$generation_mode,
	      benchmark_task_mode = as.character(input_value("benchmark_task_mode", "clean")),
	      ratios = ratio_vector(),
      pattern_sequence = pattern_sequence,
      leading_silence_initial_pause = isTRUE(input$leading_silence_initial_pause),
      initial_latency_model = as.character(value_or(input$initial_latency_model, "residual_life")),
      avoid_noisy_burst_runs = isTRUE(input$avoid_noisy_burst_runs),
      noisy_specificity = list(
        avoid_mode_overlap = FALSE,
        contextual_mode_overlap = TRUE,
        tolerance = NOISY_CONTEXT_GUARD_S,
        clean_guard_s = NOISY_CONTEXT_GUARD_S,
        context_guard_s = NOISY_CONTEXT_GUARD_S,
        pause_guard_s = NOISY_PAUSE_GUARD_S,
        pause_guard_ratio = NOISY_PAUSE_GUARD_RATIO,
        tonic_upper_multiplier = NOISY_TONIC_UPPER_MULTIPLIER,
        mm_ratio = as.numeric(input$noisy_mm_ratio)
      ),
      stimulation = list(
        enabled = isTRUE(input_value("stim_enabled", FALSE)),
        experiment_preset = as.character(input_value("stim_experiment_preset", "custom")),
        protocol = as.character(input_value("stim_protocol", "regular")),
        response_type = as.character(input_value("stim_response_type", "excitatory_burst")),
        start_s = as.numeric(input_value("stim_start_s", 5)),
        duration_s = as.numeric(input_value("stim_duration_s", 0.05)),
        n_stimuli = as.integer(round(as.numeric(input_value("stim_n", 8)))),
        inter_stimulus_interval_s = as.numeric(input_value("stim_isi_s", 3)),
        paired_pulse_interval_s = as.numeric(input_value("stim_pair_interval_s", 0.5)),
        strength = as.numeric(input_value("stim_strength", 0.8)),
        strength_end = as.numeric(input_value("stim_strength_end", 1.0)),
        strength_jitter = as.numeric(input_value("stim_strength_jitter", 0)),
        manual_times = as.character(input_value("stim_manual_times", "")),
        manual_strengths = as.character(input_value("stim_manual_strengths", "")),
        feature_modality = as.character(input_value("stim_feature_modality", "orientation")),
        feature_values = as.character(input_value("stim_feature_values", "15,45,90,135,180,225,270,315")),
        feature_xy_values = as.character(input_value("stim_feature_xy_values", "0,0; 25,0; 0,25; -25,0; 0,-25; 25,25; -25,25; 25,-25; -25,-25")),
        place_field_x_min = as.numeric(input_value("stim_place_field_x_min", -50)),
        place_field_x_max = as.numeric(input_value("stim_place_field_x_max", 50)),
        place_field_y_min = as.numeric(input_value("stim_place_field_y_min", -50)),
        place_field_y_max = as.numeric(input_value("stim_place_field_y_max", 50)),
        place_field_center_x = as.numeric(input_value("stim_place_field_center_x", 0)),
        place_field_center_y = as.numeric(input_value("stim_place_field_center_y", 0)),
        place_field_width = as.numeric(input_value("stim_place_field_width", 18)),
        place_field_radius = as.numeric(input_value("stim_place_field_radius", 45)),
        preferred_feature = as.numeric(input_value("stim_preferred_feature", 15)),
        null_feature = as.numeric(input_value("stim_null_feature", 90)),
        feature_period = as.numeric(input_value("stim_feature_period", 180)),
        feature_tuning_width = as.numeric(input_value("stim_feature_tuning_width", 25)),
        feature_suppression_width = as.numeric(input_value("stim_feature_suppression_width", 25)),
        feature_min_gain = as.numeric(input_value("stim_feature_min_gain", 0.05)),
        feature_population_mode = as.character(input_value("stim_feature_population_mode", "coverage_balanced_population")),
        feature_responsive_fraction = as.numeric(input_value("stim_feature_responsive_fraction", 0.35)),
        feature_suppressive_fraction = as.numeric(input_value("stim_feature_suppressive_fraction", 0.10)),
        feature_biphasic_fraction = as.numeric(input_value("stim_feature_biphasic_fraction", 0.05)),
        feature_response_threshold = as.numeric(input_value("stim_feature_response_threshold", 0.35)),
        feature_preferred_response = as.character(input_value("stim_feature_preferred_response", "excitatory_burst")),
        feature_null_response = as.character(input_value("stim_feature_null_response", "no_response")),
        feature_population_jitter = as.numeric(input_value("stim_feature_population_jitter", 0.25)),
        feature_unit_max_gain = as.numeric(input_value("stim_feature_unit_max_gain", 1.0)),
        feature_unit_response_reliability = as.numeric(input_value("stim_feature_unit_response_reliability", 1.0)),
        feature_target_unit = as.integer(round(as.numeric(input_value("stim_feature_target_unit", 1)))),
        deviant_probability = as.numeric(input_value("stim_deviant_probability", 0.2)),
        deviant_strength = as.numeric(input_value("stim_deviant_strength", 1.0)),
        response_latency_median_s = as.numeric(input_value("stim_latency_median_s", 0.08)),
        response_latency_sdlog = as.numeric(input_value("stim_latency_sdlog", 0.25)),
        response_probability = as.numeric(input_value("stim_response_probability", 1.0)),
        max_evoked_bursts = as.integer(round(as.numeric(input_value("stim_max_evoked_bursts", 3)))),
        burst_lambda_base = as.numeric(input_value("stim_burst_lambda_base", 0.2)),
        burst_lambda_strength = as.numeric(input_value("stim_burst_lambda_strength", 2.5)),
        evoked_burst_spike_min = as.integer(round(as.numeric(input_value("stim_burst_spike_min", 3)))),
        evoked_burst_spike_max = as.integer(round(as.numeric(input_value("stim_burst_spike_max", 7)))),
        pause_duration_min_s = as.numeric(input_value("stim_pause_min_s", 0.5)),
        pause_duration_max_s = as.numeric(input_value("stim_pause_max_s", 1.4)),
        pause_duration_cv = as.numeric(input_value("stim_pause_duration_cv", 0.35)),
        post_burst_pause_probability = as.numeric(input_value("stim_post_burst_pause_probability", 0.25)),
        rebound_probability = as.numeric(input_value("stim_rebound_probability", 0.35)),
        response_window_s = as.numeric(input_value("stim_response_window_s", 1.5)),
        baseline_recovery_enabled = isTRUE(input_value("stim_baseline_recovery_enabled", TRUE)),
        baseline_recovery_mode = as.character(input_value("stim_baseline_recovery_mode", "Noisy")),
        pre_stimulus_guard_s = as.numeric(input_value("stim_pre_stimulus_guard_s", 0.02)),
        adaptation_enabled = isTRUE(input_value("stim_adaptation_enabled", TRUE)),
        adaptation_increment = as.numeric(input_value("stim_adaptation_increment", 0.35)),
        adaptation_tau_s = as.numeric(input_value("stim_adaptation_tau_s", 12)),
        adaptation_source = "mixed",
        response_floor = as.numeric(input_value("stim_response_floor", 0.15)),
        channel = "A"
      ),
      observation = list(
        enabled = isTRUE(input_value("obs_enabled", FALSE)),
        detection_probability = as.numeric(input_value("obs_detection_probability", 0.98)),
        false_positive_rate_hz = as.numeric(input_value("obs_false_positive_rate_hz", 0)),
        jitter_sd_s = as.numeric(input_value("obs_jitter_sd_ms", 0.2)) / 1000,
        time_bias_s = as.numeric(input_value("obs_time_bias_ms", 0)) / 1000,
        dead_time_s = as.numeric(input_value("obs_dead_time_ms", 0.6)) / 1000,
        seed_offset = as.integer(round(as.numeric(input_value("obs_seed_offset", 200000)))),
        mode = "bernoulli_detection_plus_false_positives"
      ),
      patterns = list(
        Burst = list(
          dist_type = input$dist_burst,
          params = get_params("Burst"),
          interval_range = input$interval_range_burst,
          spike_count_range = input$spike_range_burst,
          temporal_dependence = list(rho = as.numeric(input$burst_isi_rho), trend = as.numeric(input$burst_isi_trend))
        ),
        Pause = list(
          dist_type = input$dist_pause,
          params = get_params("Pause"),
          interval_range = input$pause_duration_range,
          spike_count_range = c(0, 0),
          temporal_dependence = list(rho = as.numeric(input$pause_isi_rho), trend = as.numeric(input$pause_isi_trend))
        ),
        Tonic = list(
          dist_type = input$dist_tonic,
          params = get_params("Tonic"),
          interval_range = input$interval_range_tonic,
          spike_count_range = input$spike_range_tonic,
          temporal_dependence = list(rho = as.numeric(input$tonic_isi_rho), trend = as.numeric(input$tonic_isi_trend)),
          regularity_ranges = list(
            cv = input$tonic_cv_range,
            cv2 = input$tonic_cv2_range,
            lv = input$tonic_lv_range
          )
        ),
        high_frequency_tonic = list(
          dist_type = input$dist_hft,
          params = get_params("high_frequency_tonic"),
          interval_range = input$interval_range_hft,
          spike_count_range = input$spike_range_hft,
          temporal_dependence = list(rho = as.numeric(input$hft_isi_rho), trend = as.numeric(input$hft_isi_trend)),
          regularity_ranges = list(
            cv = input$hft_cv_range,
            cv2 = input$hft_cv2_range,
            lv = input$hft_lv_range,
            mm = input$hft_mm_range
          )
        ),
        high_frequency_spiking = list(
          dist_type = input$dist_hfs,
          params = get_params("high_frequency_spiking"),
          interval_range = input$interval_range_hfs,
          spike_count_range = input$spike_range_hfs,
          temporal_dependence = list(rho = as.numeric(input$hfs_isi_rho), trend = as.numeric(input$hfs_isi_trend)),
          state_rules = list(
            short_isi_range = input$hfs_short_isi_range,
            bridge_isi_range = input$hfs_bridge_isi_range,
            target_short_fraction = as.numeric(input$hfs_target_short_fraction),
            short_fraction_min = as.numeric(input$hfs_short_fraction_min),
            bridge_fraction_max = as.numeric(input$hfs_bridge_fraction_max),
            max_consecutive_bridge = as.integer(round(input$hfs_max_consecutive_bridge)),
            min_duration_s = as.numeric(input$hfs_min_duration)
          )
        ),
        Noisy = list(
          dist_type = input$dist_noisy,
          params = get_params("Noisy"),
          interval_range = input$interval_range_noisy,
          spike_count_range = input$spike_range_noisy,
          temporal_dependence = list(rho = as.numeric(input$noisy_isi_rho), trend = as.numeric(input$noisy_isi_trend))
        )
      )
    )
  }

  simulate_single_train <- function(config) {
    simulate_spike_train_core(config)
  }

  rng_state_to_text <- function(state) {
    if (is.null(state) || length(state) == 0) return(NA_character_)
    paste(as.integer(state), collapse = " ")
  }

  rng_state_hash <- function(state) {
    state_text <- rng_state_to_text(state)
    if (is.na(state_text) || !requireNamespace("digest", quietly = TRUE)) return(NA_character_)
    digest::digest(state_text, algo = "sha256", serialize = FALSE)
  }

  current_rng_state <- function() {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      integer(0)
    }
  }

  train_seed_audit_row <- function(train_id, generation_key, dataset_seed, before_state, after_state, note = "synthetic_train_rng_state") {
    rng_kind <- RNGkind()
    rng_kind_value <- function(index) {
      if (length(rng_kind) >= index) as.character(rng_kind[[index]]) else NA_character_
    }
    data.frame(
      Train = as.integer(train_id),
      Generation_Key = as.character(generation_key),
      Dataset_Derived_RNG_Seed = as.integer(dataset_seed),
      RNGkind_Kind = rng_kind_value(1),
      RNGkind_Normal = rng_kind_value(2),
      RNGkind_Sample = rng_kind_value(3),
      RNG_State_Before_Length = length(before_state),
      RNG_State_Before_SHA256 = rng_state_hash(before_state),
      RNG_State_Before = rng_state_to_text(before_state),
      RNG_State_After_Length = length(after_state),
      RNG_State_After_SHA256 = rng_state_hash(after_state),
      RNG_State_After = rng_state_to_text(after_state),
      Reconstruct_Note = note,
      stringsAsFactors = FALSE
    )
  }

  fallback_train_seed_audit <- function(n_train, generation_key, dataset_seed, note = "no_per_train_synthetic_rng_state") {
    n_train <- max(1L, as.integer(n_train))
    empty_state <- integer(0)
    do.call(rbind, lapply(seq_len(n_train), function(train_id) {
      train_seed_audit_row(train_id, generation_key, dataset_seed, empty_state, empty_state, note)
    }))
  }

  train_seed_table <- function(sim) {
    if (!is.null(sim$train_seed_audit) && nrow(sim$train_seed_audit) > 0) {
      return(sim$train_seed_audit)
    }
    fallback_train_seed_audit(
      generated_train_count(sim),
      value_or(sim$generation_key, NA_character_),
      value_or(sim$seed, NA_integer_),
      note = if (!is.null(sim$source) && identical(sim$source, "uploaded_dataset")) {
        "uploaded_dataset_no_synthetic_train_rng"
      } else {
        "train_rng_state_not_recorded"
      }
    )
  }

  simulate_config_dataset <- function(sim_config, n_train, generation_key_value, reproduction_settings = NULL) {
    n_train <- max(1L, as.integer(n_train))
    generation_key_value <- normalize_generation_key(generation_key_value)
    seed_value <- derive_seed_from_key(generation_key_value)
    sim_config$generation_key <- generation_key_value
    sim_config$derived_rng_seed <- seed_value
    if (is.null(reproduction_settings)) reproduction_settings <- collect_reproduction_settings()
    reproduction_settings$generation_key <- generation_key_value
    set.seed(seed_value)
    external_stimulus_schedule <- if (stimulation_enabled(sim_config)) stimulus_external_schedule_from_config(sim_config, seed = seed_value) else make_empty_stimulus_df()
    if (stimulation_enabled(sim_config) && !is.null(sim_config$stimulation)) {
      sim_config$stimulation$external_schedule <- external_stimulus_schedule
    }
    unit_profiles <- make_unit_tuning_profiles(sim_config, n_train, seed = seed_value)

    all_spikes_list <- vector("list", n_train)
    all_episodes_list <- vector("list", n_train)
    all_intervals_list <- vector("list", n_train)
    all_stimuli_list <- vector("list", n_train)
    all_responses_list <- vector("list", n_train)
    all_event_epochs_list <- vector("list", n_train)
    train_seed_rows <- vector("list", n_train)
    all_warnings <- character(0)

    for (i in seq_len(n_train)) {
      train_config <- sim_config
      if (!is.null(unit_profiles) && nrow(unit_profiles) >= i) {
        train_config <- apply_unit_profile_to_config(sim_config, unit_profiles[i, , drop = FALSE])
      }
      if (stimulation_enabled(train_config) && !is.null(train_config$stimulation)) {
        train_config$stimulation$external_schedule <- external_stimulus_schedule
      }
      rng_before <- current_rng_state()
      sim <- simulate_single_train(train_config)
      rng_after <- current_rng_state()
      train_seed_rows[[i]] <- train_seed_audit_row(i, generation_key_value, seed_value, rng_before, rng_after)
      if (length(sim$warnings) > 0) {
        all_warnings <- c(all_warnings, paste0("Spike train ", i, ": ", sim$warnings))
      }

      if (nrow(sim$spikes) > 0) {
        sim$spikes$Train <- i
        if (!"Spike_Role" %in% names(sim$spikes)) sim$spikes$Spike_Role <- "event_spike"
        if (!"Episode_Context" %in% names(sim$spikes)) sim$spikes$Episode_Context <- NA_character_
        sim$spikes <- sim$spikes[, c("Train", "Episode", "Time", "Episode_Context", "Spike_Role")]
      } else {
        sim$spikes <- data.frame(Train = integer(0), Episode = integer(0), Time = numeric(0), Episode_Context = character(0), Spike_Role = character(0), stringsAsFactors = FALSE)
      }

      if (nrow(sim$episodes) > 0) {
        sim$episodes$Train <- i
        episode_base_cols <- c("Train", names(make_empty_episode_df()))
        missing_episode_cols <- setdiff(episode_base_cols, names(sim$episodes))
        for (col in missing_episode_cols) sim$episodes[[col]] <- NA
        sim$episodes <- sim$episodes[, episode_base_cols, drop = FALSE]
      } else {
        sim$episodes <- data.frame(Train = integer(0), make_empty_episode_df(), stringsAsFactors = FALSE)
      }

      if (!is.null(sim$intervals) && nrow(sim$intervals) > 0) {
        sim$intervals$Train <- i
        base_cols <- names(make_empty_interval_df())
        missing_interval_cols <- setdiff(base_cols, names(sim$intervals))
        for (col in missing_interval_cols) sim$intervals[[col]] <- NA
        sim$intervals <- sim$intervals[, base_cols, drop = FALSE]
      } else {
        sim$intervals <- make_empty_interval_df()
      }

      if (!is.null(sim$stimuli) && nrow(sim$stimuli) > 0) {
        sim$stimuli$Train <- i
        stim_cols <- names(make_empty_stimulus_df())
        missing_stim_cols <- setdiff(stim_cols, names(sim$stimuli))
        for (col in missing_stim_cols) sim$stimuli[[col]] <- NA
        sim$stimuli <- sim$stimuli[, stim_cols, drop = FALSE]
      } else {
        sim$stimuli <- make_empty_stimulus_df()
      }

      if (!is.null(sim$responses) && nrow(sim$responses) > 0) {
        sim$responses$Train <- i
        resp_cols <- names(make_empty_response_df())
        missing_resp_cols <- setdiff(resp_cols, names(sim$responses))
        for (col in missing_resp_cols) sim$responses[[col]] <- NA
        sim$responses <- sim$responses[, resp_cols, drop = FALSE]
      } else {
        sim$responses <- make_empty_response_df()
      }

      if (is.null(sim$event_epochs)) {
        sim$event_epochs <- event_epochs_from_intervals(sim$intervals)
      }
      if (!is.null(sim$event_epochs) && nrow(sim$event_epochs) > 0) {
        sim$event_epochs$Train <- i
        epoch_cols <- names(make_empty_event_epoch_df())
        missing_epoch_cols <- setdiff(epoch_cols, names(sim$event_epochs))
        for (col in missing_epoch_cols) sim$event_epochs[[col]] <- NA
        sim$event_epochs <- sim$event_epochs[, epoch_cols, drop = FALSE]
      } else {
        sim$event_epochs <- make_empty_event_epoch_df()
      }

      all_spikes_list[[i]] <- sim$spikes
      all_episodes_list[[i]] <- sim$episodes
      all_intervals_list[[i]] <- sim$intervals
      all_stimuli_list[[i]] <- sim$stimuli
      all_responses_list[[i]] <- sim$responses
      all_event_epochs_list[[i]] <- sim$event_epochs
    }

    combined_spikes <- do.call(rbind, all_spikes_list)
    combined_episodes <- do.call(rbind, all_episodes_list)
    combined_intervals <- do.call(rbind, all_intervals_list)
    combined_stimuli <- do.call(rbind, all_stimuli_list)
    combined_responses <- do.call(rbind, all_responses_list)
    combined_event_epochs <- do.call(rbind, all_event_epochs_list)
    combined_units <- if (!is.null(unit_profiles) && nrow(unit_profiles) > 0) unit_profiles else make_empty_unit_df()
    combined_unit_stimulus_drive <- build_unit_stimulus_drive_table(combined_stimuli, combined_responses)
    rownames(combined_spikes) <- NULL
    rownames(combined_episodes) <- NULL
    rownames(combined_intervals) <- NULL
    rownames(combined_stimuli) <- NULL
    rownames(combined_responses) <- NULL
    rownames(combined_event_epochs) <- NULL
    if (nrow(combined_units) > 0) rownames(combined_units) <- NULL
    if (nrow(combined_unit_stimulus_drive) > 0) rownames(combined_unit_stimulus_drive) <- NULL
    train_seed_audit <- do.call(rbind, train_seed_rows)
    rownames(train_seed_audit) <- NULL
    combined_spikes <- annotate_spike_isis(combined_spikes, combined_episodes, combined_intervals)
    observed <- apply_observation_model_to_spikes(combined_spikes, sim_config, seed = seed_value, total_time = sim_config$total_time)
    verification <- simulation_verification(seed_value, n_train, sim_config, combined_spikes, combined_episodes,
                                            combined_intervals, combined_stimuli, combined_responses,
                                            combined_event_epochs)

    result <- list(spikes_list = all_spikes_list,
                   episodes_list = all_episodes_list,
                   intervals_list = all_intervals_list,
                   combined_spikes = combined_spikes,
                   combined_episodes = combined_episodes,
                   combined_intervals = combined_intervals,
                   combined_stimuli = combined_stimuli,
                   combined_responses = combined_responses,
                   combined_event_epochs = combined_event_epochs,
                   combined_units = combined_units,
                   combined_unit_stimulus_drive = combined_unit_stimulus_drive,
                   unit_stimulus_drive_validation = validate_unit_stimulus_drive_table(combined_unit_stimulus_drive),
                   combined_observed_spikes = observed$observed_spikes,
                   observation_map = observed$observation_map,
                   observation_summary = observed$observation_summary,
                   observation_model = observed$model_label,
                   train_count = n_train,
                   generation_key = generation_key_value,
                   seed = seed_value,
                   train_seed_audit = train_seed_audit,
                   verification_code = verification$code,
                   verification_hash = verification$hash,
                   warnings = unique(all_warnings),
                   config = sim_config)
    result$reproduction_code <- encode_reproduction_code(build_reproduction_payload(reproduction_settings, result))
    result
  }

  build_uploaded_minimal_config <- function() {
    burst_range <- suppressWarnings(as.numeric(input_value("interval_range_burst", c(0.006, 0.045))))
    tonic_range <- suppressWarnings(as.numeric(input_value("interval_range_tonic", c(0.38, 0.52))))
    hft_range <- suppressWarnings(as.numeric(input_value("interval_range_hft", c(0.024, 0.040))))
    hfs_range <- suppressWarnings(as.numeric(input_value("interval_range_hfs", c(0.003, 0.020))))
    hfs_short_range <- suppressWarnings(as.numeric(input_value("hfs_short_isi_range", c(0.003, 0.012))))
    hfs_bridge_range <- suppressWarnings(as.numeric(input_value("hfs_bridge_isi_range", c(0.012, 0.020))))
    pause_range <- suppressWarnings(as.numeric(input_value("pause_duration_range", c(0.7, 1.5))))
    noisy_range <- suppressWarnings(as.numeric(input_value("interval_range_noisy", c(0.08, 0.28))))
    if (length(burst_range) < 2 || any(!is.finite(burst_range[1:2]))) burst_range <- c(0.006, 0.045)
    if (length(tonic_range) < 2 || any(!is.finite(tonic_range[1:2]))) tonic_range <- c(0.38, 0.52)
    if (length(hft_range) < 2 || any(!is.finite(hft_range[1:2]))) hft_range <- c(0.024, 0.040)
    if (length(hfs_range) < 2 || any(!is.finite(hfs_range[1:2]))) hfs_range <- c(0.003, 0.020)
    if (length(hfs_short_range) < 2 || any(!is.finite(hfs_short_range[1:2]))) hfs_short_range <- c(0.003, 0.012)
    if (length(hfs_bridge_range) < 2 || any(!is.finite(hfs_bridge_range[1:2]))) hfs_bridge_range <- c(0.012, 0.020)
    if (length(pause_range) < 2 || any(!is.finite(pause_range[1:2]))) pause_range <- c(0.7, 1.5)
    if (length(noisy_range) < 2 || any(!is.finite(noisy_range[1:2]))) noisy_range <- c(0.08, 0.28)
    list(
      total_time = safe_total_time(),
      inter_event_gap = effective_inter_event_gap(),
      generation_mode = "uploaded_dataset",
      benchmark_task_mode = as.character(input_value("benchmark_task_mode", "clean")),
      ratios = normalize_pattern_ratios(c(Burst = 0.25, Pause = 0.25, Tonic = 0.25, high_frequency_tonic = 0, high_frequency_spiking = 0, Noisy = 0.25)),
      pattern_sequence = list(),
      leading_silence_initial_pause = FALSE,
      initial_latency_model = "residual_life",
      avoid_noisy_burst_runs = TRUE,
      noisy_specificity = list(mm_ratio = as.numeric(input_value("noisy_mm_ratio", 1.5))),
      stimulation = list(enabled = FALSE, source = "uploaded_event_table"),
      observation = list(
        enabled = isTRUE(input_value("obs_enabled", FALSE)),
        detection_probability = as.numeric(input_value("obs_detection_probability", 0.98)),
        false_positive_rate_hz = as.numeric(input_value("obs_false_positive_rate_hz", 0)),
        jitter_sd_s = as.numeric(input_value("obs_jitter_sd_ms", 0.2)) / 1000,
        time_bias_s = as.numeric(input_value("obs_time_bias_ms", 0)) / 1000,
        dead_time_s = as.numeric(input_value("obs_dead_time_ms", 0.6)) / 1000,
        seed_offset = as.integer(round(as.numeric(input_value("obs_seed_offset", 200000)))),
        mode = "bernoulli_detection_plus_false_positives"
      ),
      patterns = list(
        Burst = list(dist_type = "Uniform", params = list(min = burst_range[1], max = burst_range[2]), interval_range = burst_range, spike_count_range = c(3, 6), temporal_dependence = list(rho = 0, trend = 0)),
        Pause = list(dist_type = "Uniform", params = list(min = pause_range[1], max = pause_range[2]), interval_range = pause_range, spike_count_range = c(0, 0), temporal_dependence = list(rho = 0, trend = 0)),
        Tonic = list(dist_type = "Uniform", params = list(min = tonic_range[1], max = tonic_range[2]), interval_range = tonic_range, spike_count_range = c(4, 8), temporal_dependence = list(rho = 0, trend = 0), regularity_ranges = list(cv = c(0, 0.18), cv2 = c(0, 0.25), lv = c(0, 0.06))),
        high_frequency_tonic = list(dist_type = "Uniform", params = list(min = hft_range[1], max = hft_range[2]), interval_range = hft_range, spike_count_range = c(8, 24), temporal_dependence = list(rho = 0.2, trend = 0), regularity_ranges = list(cv = c(0, 0.22), cv2 = c(0, 0.28), lv = c(0, 0.22), mm = c(1, 1.25))),
        high_frequency_spiking = list(dist_type = "Gamma", params = list(shape = 3, scale = 0.0025), interval_range = hfs_range, spike_count_range = c(30, 70), temporal_dependence = list(rho = 0, trend = 0), state_rules = list(short_isi_range = hfs_short_range, bridge_isi_range = hfs_bridge_range, target_short_fraction = 0.90, short_fraction_min = 0.80, bridge_fraction_max = 0.15, max_consecutive_bridge = 2L, min_duration_s = 0.20)),
        Noisy = list(dist_type = "Uniform", params = list(min = noisy_range[1], max = noisy_range[2]), interval_range = noisy_range, spike_count_range = c(3, 7), temporal_dependence = list(rho = 0, trend = 0))
      )
    )
  }

  complete_columns <- function(df, template_cols, extra_cols = character(0)) {
    cols <- unique(c(template_cols, extra_cols))
    for (col in setdiff(cols, names(df))) df[[col]] <- NA
    df[, cols, drop = FALSE]
  }

  uploaded_time_multiplier <- function() {
    unit <- tolower(as.character(input_value("uploaded_time_unit", "s"))[1])
    if (identical(unit, "ms")) return(0.001)
    1
  }

  read_uploaded_spike_table <- function(upload) {
    if (is.null(upload) || is.null(upload$datapath) || !file.exists(upload$datapath)) {
      stop(tr(current_lang(), "uploaded_missing_file"), call. = FALSE)
    }
    path <- upload$datapath
    ext <- tolower(tools::file_ext(as.character(upload$name)))
    na_values <- c("", "NA", "NaN", "nan", "NULL", "null")
    attempts <- if (identical(ext, "csv")) {
      list(
        function() utils::read.csv(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = na_values),
        function() utils::read.delim(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = na_values)
      )
    } else {
      list(
        function() utils::read.delim(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = na_values),
        function() utils::read.csv(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = na_values),
        function() utils::read.table(path, header = TRUE, sep = ";", check.names = FALSE, stringsAsFactors = FALSE, na.strings = na_values)
      )
    }
    last_error <- NULL
    for (attempt in attempts) {
      tab <- tryCatch(attempt(), error = function(err) { last_error <<- err; NULL })
      if (!is.null(tab) && ncol(tab) > 0) {
        names(tab) <- make.unique(ifelse(nzchar(trimws(names(tab))), trimws(names(tab)), paste0("Column_", seq_along(names(tab)))))
        return(tab)
      }
    }
    stop(tr(current_lang(), "uploaded_bad_file"), call. = FALSE)
  }

  numeric_timestamp_column <- function(x, multiplier = 1) {
    if (is.factor(x)) x <- as.character(x)
    out <- suppressWarnings(as.numeric(trimws(as.character(x))))
    out <- out[is.finite(out)]
    out <- out * multiplier
    out <- out[is.finite(out) & out >= 0]
    sort(unique(out))
  }

  range_contains_value <- function(value, range_value) {
    rng <- suppressWarnings(as.numeric(range_value))
    if (length(rng) < 2 || any(!is.finite(rng[1:2]))) return(FALSE)
    rng <- sort(rng[1:2])
    is.finite(value) && value >= rng[1] && value <= rng[2]
  }

  classify_uploaded_isi <- function(isi_s, auto_label = TRUE) {
    if (!isTRUE(auto_label) || !is.finite(isi_s) || isi_s <= 0) return("Noisy")
    if (range_contains_value(isi_s, input_value("hfs_short_isi_range", c(0.003, 0.012)))) return("high_frequency_spiking")
    if (range_contains_value(isi_s, input_value("interval_range_hft", c(0.024, 0.040)))) return("high_frequency_tonic")
    if (range_contains_value(isi_s, input_value("interval_range_burst", c(0.006, 0.045)))) return("Burst")
    if (range_contains_value(isi_s, input_value("pause_duration_range", c(0.7, 1.5)))) return("Pause")
    if (range_contains_value(isi_s, input_value("interval_range_tonic", c(0.38, 0.52)))) return("Tonic")
    "Noisy"
  }

  build_uploaded_stimulus_table <- function(event_table, train_ids) {
    if (is.null(event_table) || nrow(event_table) == 0 || length(train_ids) == 0) return(make_empty_stimulus_df())
    stim_rows <- list()
    row_i <- 0L
    for (train_id in train_ids) {
      events <- event_table[order(event_table$Onset_s, event_table$Event_Column), , drop = FALSE]
      isi <- c(NA_real_, diff(events$Onset_s))
      for (j in seq_len(nrow(events))) {
        row_i <- row_i + 1L
        stim_rows[[row_i]] <- data.frame(
          Train = as.integer(train_id),
          Stimulus_ID = as.integer(events$Stimulus_ID[j]),
          Onset_s = events$Onset_s[j],
          Duration_s = 0,
          Strength = 1,
          Protocol = "uploaded_event_table",
          Stimulus_Type = as.character(events$Event_Column[j]),
          Channel = as.character(events$Event_Column[j]),
          Repetition_Index = as.integer(j),
          Inter_Stimulus_Interval_s = isi[j],
          Pair_ID = NA_integer_,
          Is_Standard = NA,
          Is_Deviant = NA,
          Feature_Modality = "uploaded_event",
          Stimulus_Feature_Value = NA_real_,
          Stimulus_Position_X = NA_real_,
          Stimulus_Position_Y = NA_real_,
          Preferred_Feature_Value = NA_real_,
          Null_Feature_Value = NA_real_,
          Feature_Distance_To_Preferred = NA_real_,
          Feature_Distance_To_Null = NA_real_,
          Feature_Excitation = NA_real_,
          Feature_Suppression = NA_real_,
          Feature_Selectivity = NA_real_,
          Feature_Response_Class = "external_event",
          External_Strength = 1,
          Feature_Drive = NA_real_,
          Feature_Matched = NA,
          Drive_Above_Threshold = NA,
          Response_Kernel = "uploaded_event_only",
          Response_Eligible = FALSE,
          Feature_Response_Eligible = FALSE,
          Feature_Response_Reason = "uploaded_event_no_simulated_response",
          stringsAsFactors = FALSE
        )
      }
    }
    out <- if (length(stim_rows) > 0) do.call(rbind, stim_rows) else make_empty_stimulus_df()
    complete_columns(out, names(make_empty_stimulus_df()))
  }

  build_uploaded_intervals <- function(train_times, train_labels, event_table, auto_label = TRUE) {
    interval_rows <- list()
    row_i <- 0L
    for (train_id in seq_along(train_times)) {
      times <- train_times[[train_id]]
      if (length(times) < 2) next
      for (j in seq_len(length(times) - 1L)) {
        start_time <- times[j]
        end_time <- times[j + 1L]
        isi <- end_time - start_time
        if (!is.finite(isi) || isi <= 0) next
        label <- classify_uploaded_isi(isi, auto_label)
        matching_events <- if (!is.null(event_table) && nrow(event_table) > 0) {
          event_table[event_table$Onset_s > start_time & event_table$Onset_s <= end_time, , drop = FALSE]
        } else {
          event_table
        }
        has_event <- !is.null(matching_events) && nrow(matching_events) > 0
        stim_id <- if (has_event) as.integer(matching_events$Stimulus_ID[1]) else NA_integer_
        stim_onset <- if (has_event) as.numeric(matching_events$Onset_s[1]) else NA_real_
        row_i <- row_i + 1L
        interval_rows[[row_i]] <- data.frame(
          Train = as.integer(train_id),
          Interval_ID = as.integer(j),
          Left_Spike_Index = as.integer(j),
          Right_Spike_Index = as.integer(j + 1L),
          Left_Spike_Time_s = start_time,
          Right_Spike_Time_s = end_time,
          Start_Time_s = start_time,
          End_Time_s = end_time,
          ISI_s = isi,
          Interval = isi,
          ISI_Label = label,
          Episode = NA_integer_,
          ISI_Scope = if (has_event) "uploaded_event_spanning_interval" else "uploaded_spike_interval",
          Left_Spike_Role = "uploaded_spike",
          Right_Spike_Role = "uploaded_spike",
          Left_Episode_Context = NA_character_,
          Right_Episode_Context = NA_character_,
          Is_Manual_Fixed = TRUE,
          Interval_Source = if (isTRUE(auto_label)) "uploaded_dataset_heuristic_label" else "uploaded_dataset_visualization_label",
          Run_Position = NA_real_,
          Run_Length = NA_integer_,
          Temporal_Rho = NA_real_,
          Temporal_Trend = NA_real_,
          Stimulus_ID = stim_id,
          Stimulus_Phase = if (has_event) "uploaded_event" else NA_character_,
          Evoked = FALSE,
          Evoked_Response_Type = NA_character_,
          Response_Epoch = if (has_event) "uploaded_event_spanning_interval" else NA_character_,
          Stimulus_Onset_s = stim_onset,
          Time_From_Stimulus_Onset_s = if (is.finite(stim_onset)) end_time - stim_onset else NA_real_,
          Contains_Stimulus_Onset = has_event,
          Train_Label = train_labels[[train_id]],
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(interval_rows) == 0) return(data.frame(make_empty_interval_df(), Train_Label = character(0), stringsAsFactors = FALSE))
    intervals <- do.call(rbind, interval_rows)
    intervals <- intervals[order(intervals$Train, intervals$Start_Time_s, intervals$End_Time_s), , drop = FALSE]
    intervals$Interval_ID <- ave(intervals$Interval_ID, intervals$Train, FUN = seq_along)
    for (train_id in unique(intervals$Train)) {
      idx <- which(intervals$Train == train_id)
      idx <- idx[order(intervals$Interval_ID[idx])]
      breaks <- c(TRUE, intervals$ISI_Label[idx][-1] != intervals$ISI_Label[idx][-length(idx)])
      run_id <- cumsum(breaks)
      intervals$Run_Position[idx] <- ave(seq_along(idx), run_id, FUN = seq_along)
      intervals$Run_Length[idx] <- ave(rep(1L, length(idx)), run_id, FUN = length)
    }
    rownames(intervals) <- NULL
    complete_columns(intervals, names(make_empty_interval_df()), extra_cols = "Train_Label")
  }

  assign_uploaded_episode_ids <- function(intervals) {
    if (is.null(intervals) || nrow(intervals) == 0) return(intervals)
    intervals <- intervals[order(intervals$Train, intervals$Interval_ID), , drop = FALSE]
    intervals$Episode <- NA_integer_
    for (train_id in sort(unique(intervals$Train))) {
      idx <- which(intervals$Train == train_id)
      if (length(idx) == 0) next
      idx <- idx[order(intervals$Interval_ID[idx])]
      labels <- as.character(intervals$ISI_Label[idx])
      breaks <- c(TRUE, labels[-1] != labels[-length(labels)])
      intervals$Episode[idx] <- as.integer(cumsum(breaks))
    }
    rownames(intervals) <- NULL
    intervals
  }

  make_uploaded_episodes_from_intervals <- function(intervals) {
    if (is.null(intervals) || nrow(intervals) == 0) {
      return(data.frame(Train = integer(0), make_empty_episode_df(), Train_Label = character(0), stringsAsFactors = FALSE))
    }
    intervals <- intervals[order(intervals$Train, intervals$Interval_ID), , drop = FALSE]
    rows <- list()
    row_i <- 0L
    for (train_id in sort(unique(intervals$Train))) {
      idx_train <- which(intervals$Train == train_id)
      if (length(idx_train) == 0) next
      labels <- as.character(intervals$ISI_Label[idx_train])
      breaks <- c(TRUE, labels[-1] != labels[-length(labels)])
      groups <- split(idx_train, cumsum(breaks))
      episode_id <- 0L
      for (idx in groups) {
        idx <- idx[order(intervals$Interval_ID[idx])]
        episode_id <- episode_id + 1L
        intervals$Episode[idx] <- episode_id
        vals <- intervals$ISI_s[idx]
        reg <- isi_regularity_metrics(vals)
        start_time <- intervals$Start_Time_s[idx[1]]
        end_time <- intervals$End_Time_s[idx[length(idx)]]
        duration <- end_time - start_time
        n_isis <- length(idx)
        n_boundary <- n_isis + 1L
        stim_onset_vals <- intervals$Stimulus_Onset_s[idx]
        stim_onset <- if (any(is.finite(stim_onset_vals))) stim_onset_vals[which(is.finite(stim_onset_vals))[1]] else NA_real_
        row_i <- row_i + 1L
        rows[[row_i]] <- data.frame(
          Train = as.integer(train_id),
          Episode = episode_id,
          Pattern = as.character(intervals$ISI_Label[idx[1]]),
          Episode_Scope = "uploaded_interval_run",
          Latency_Context = NA_character_,
          Latency_Model = NA_character_,
          Start = start_time,
          End = end_time,
          Episode_Duration = duration,
          Core_Start = start_time,
          Core_End = end_time,
          Core_Duration = duration,
          First_Spike_Time = start_time,
          Last_Spike_Time = end_time,
          N_Spikes = n_boundary,
          N_ISIs = n_isis,
          N_Boundary_Spikes = n_boundary,
          N_New_Spikes = n_boundary,
          N_Shared_Boundary_Spikes = 0L,
          Mean_Within_Episode_ISI = reg$mean,
          CV_Within_Episode_ISI = reg$cv,
          Mean_CV2_Within_Episode_ISI = reg$cv2,
          LV_Within_Episode_ISI = reg$lv,
          Core_ISI_Rate_Hz = if (duration > 0) n_isis / duration else NA_real_,
          Episode_Inclusive_Rate_Hz = if (duration > 0) n_boundary / duration else NA_real_,
          Stimulus_ID = if (all(is.na(intervals$Stimulus_ID[idx]))) NA_integer_ else intervals$Stimulus_ID[idx][which(!is.na(intervals$Stimulus_ID[idx]))[1]],
          Stimulus_Phase = if (any(intervals$Contains_Stimulus_Onset[idx] %in% TRUE)) "uploaded_event" else NA_character_,
          Evoked = FALSE,
          Evoked_Response_Type = NA_character_,
          Response_Epoch = if (any(intervals$Contains_Stimulus_Onset[idx] %in% TRUE)) "uploaded_event_spanning_interval" else NA_character_,
          Stimulus_Onset_s = stim_onset,
          Time_From_Stimulus_Onset_s = if (is.finite(stim_onset)) end_time - stim_onset else NA_real_,
          Contains_Stimulus_Onset = any(intervals$Contains_Stimulus_Onset[idx] %in% TRUE),
          Event_Epoch_Type = NA_character_,
          Event_Epoch_Source = "uploaded_dataset",
          Event_Epoch_Generation_Rule = "derived_from_uploaded_spike_times",
          Train_Label = as.character(intervals$Train_Label[idx[1]]),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) == 0) return(data.frame(Train = integer(0), make_empty_episode_df(), Train_Label = character(0), stringsAsFactors = FALSE))
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    complete_columns(out, c("Train", names(make_empty_episode_df())), extra_cols = "Train_Label")
  }

  build_uploaded_spike_rows <- function(train_times, train_labels, intervals) {
    rows <- list()
    row_i <- 0L
    for (train_id in seq_along(train_times)) {
      times <- train_times[[train_id]]
      if (length(times) == 0) next
      train_intervals <- intervals[intervals$Train == train_id, , drop = FALSE]
      left_labels <- right_labels <- rep(NA_character_, length(times))
      left_eps <- right_eps <- rep(NA_integer_, length(times))
      if (nrow(train_intervals) > 0) {
        for (i in seq_len(nrow(train_intervals))) {
          li <- as.integer(train_intervals$Left_Spike_Index[i])
          ri <- as.integer(train_intervals$Right_Spike_Index[i])
          if (is.finite(li) && li >= 1L && li <= length(times)) {
            right_labels[li] <- train_intervals$ISI_Label[i]
            right_eps[li] <- train_intervals$Episode[i]
          }
          if (is.finite(ri) && ri >= 1L && ri <= length(times)) {
            left_labels[ri] <- train_intervals$ISI_Label[i]
            left_eps[ri] <- train_intervals$Episode[i]
          }
        }
      }
      for (j in seq_along(times)) {
        row_i <- row_i + 1L
        context <- if (!is.na(right_labels[j])) right_labels[j] else left_labels[j]
        episode <- if (!is.na(right_eps[j])) right_eps[j] else left_eps[j]
        role <- if (!is.na(left_labels[j]) && !is.na(right_labels[j]) && !identical(left_labels[j], right_labels[j])) {
          "shared_boundary_spike"
        } else {
          "uploaded_spike"
        }
        rows[[row_i]] <- data.frame(
          Train = as.integer(train_id),
          Episode = episode,
          Time = times[j],
          Episode_Context = context,
          Spike_Role = role,
          Train_Label = train_labels[[train_id]],
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) == 0) return(data.frame(Train = integer(0), make_empty_spike_df(), Train_Label = character(0), stringsAsFactors = FALSE))
    out <- do.call(rbind, rows)
    out <- out[order(out$Train, out$Time), , drop = FALSE]
    rownames(out) <- NULL
    complete_columns(out, c("Train", names(make_empty_spike_df())), extra_cols = "Train_Label")
  }

  simulate_uploaded_dataset <- function(upload, sim_config, generation_key_value, reproduction_settings = NULL) {
    lang <- current_lang()
    tab <- read_uploaded_spike_table(upload)
    multiplier <- uploaded_time_multiplier()
    event_cols <- grepl("event", names(tab), ignore.case = TRUE)
    spike_cols <- which(!event_cols)
    validate(
      need(length(spike_cols) > 0, tr(lang, "uploaded_no_spike_columns"))
    )
    train_labels <- make.unique(trimws(names(tab)[spike_cols]))
    train_times <- lapply(spike_cols, function(col_i) numeric_timestamp_column(tab[[col_i]], multiplier = multiplier))
    keep <- vapply(train_times, length, integer(1)) > 0
    validate(
      need(any(keep), tr(lang, "uploaded_no_spikes"))
    )
    if (!all(keep)) {
      train_labels <- train_labels[keep]
      train_times <- train_times[keep]
    }
    n_train <- length(train_times)

    event_rows <- list()
    event_i <- 0L
    if (any(event_cols)) {
      for (col_i in which(event_cols)) {
        onsets <- numeric_timestamp_column(tab[[col_i]], multiplier = multiplier)
        if (length(onsets) == 0) next
        for (onset in onsets) {
          event_i <- event_i + 1L
          event_rows[[event_i]] <- data.frame(
            Stimulus_ID = event_i,
            Event_Column = names(tab)[[col_i]],
            Onset_s = onset,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    event_table <- if (length(event_rows) > 0) {
      out <- do.call(rbind, event_rows)
      out <- out[order(out$Onset_s, out$Event_Column), , drop = FALSE]
      out$Stimulus_ID <- seq_len(nrow(out))
      rownames(out) <- NULL
      out
    } else {
      data.frame(Stimulus_ID = integer(0), Event_Column = character(0), Onset_s = numeric(0), stringsAsFactors = FALSE)
    }

    auto_label <- isTRUE(input_value("uploaded_auto_label_intervals", TRUE))
    combined_intervals <- build_uploaded_intervals(train_times, train_labels, event_table, auto_label = auto_label)
    combined_intervals <- assign_uploaded_episode_ids(combined_intervals)
    combined_episodes <- make_uploaded_episodes_from_intervals(combined_intervals)
    if (nrow(combined_intervals) > 0 && nrow(combined_episodes) > 0) {
      combined_intervals$Left_Episode_Context <- combined_intervals$ISI_Label
      combined_intervals$Right_Episode_Context <- combined_intervals$ISI_Label
    }
    combined_spikes <- build_uploaded_spike_rows(train_times, train_labels, combined_intervals)
    combined_stimuli <- build_uploaded_stimulus_table(event_table, seq_len(n_train))
    combined_responses <- make_empty_response_df()
    combined_event_epochs <- event_epochs_from_intervals(combined_intervals)
    if (nrow(combined_event_epochs) > 0) {
      combined_event_epochs$Epoch_Source <- "uploaded_dataset"
      combined_event_epochs$Epoch_Generation_Rule <- "derived_from_uploaded_event_spanning_intervals"
    }
    combined_units <- make_empty_unit_df()
    combined_unit_stimulus_drive <- make_empty_unit_stimulus_drive_df()

    total_time <- max(c(
      unlist(train_times, use.names = FALSE),
      event_table$Onset_s,
      as.numeric(input_value("total_time", 25)),
      0.001
    ), na.rm = TRUE)
    if (!is.finite(total_time) || total_time <= 0) total_time <- 0.001
    sim_config$total_time <- total_time
    sim_config$source <- "uploaded_dataset"
    sim_config$uploaded_dataset <- list(
      file_name = as.character(upload$name),
      time_unit = as.character(input_value("uploaded_time_unit", "s")),
      spike_train_columns = train_labels,
      event_columns = names(tab)[event_cols],
      interval_label_policy = if (isTRUE(auto_label)) "heuristic_current_accepted_ranges" else "visualization_all_noisy",
      event_detection_policy = "column_name_contains_event_case_insensitive"
    )
    if (!is.null(sim_config$stimulation)) {
      sim_config$stimulation$enabled <- FALSE
      sim_config$stimulation$source <- "uploaded_event_table"
    }

    generation_key_value <- normalize_generation_key(generation_key_value)
    seed_value <- derive_seed_from_key(generation_key_value)
    train_seed_audit <- fallback_train_seed_audit(
      n_train,
      generation_key_value,
      seed_value,
      note = "uploaded_dataset_no_synthetic_train_rng"
    )
    combined_spikes <- annotate_spike_isis(combined_spikes, combined_episodes, combined_intervals)
    observed <- apply_observation_model_to_spikes(combined_spikes, sim_config, seed = seed_value, total_time = total_time)
    verification <- simulation_verification(seed_value, n_train, sim_config, combined_spikes, combined_episodes,
                                            combined_intervals, combined_stimuli, combined_responses,
                                            combined_event_epochs)
    spikes_list <- lapply(seq_len(n_train), function(i) combined_spikes[combined_spikes$Train == i, , drop = FALSE])
    episodes_list <- lapply(seq_len(n_train), function(i) combined_episodes[combined_episodes$Train == i, , drop = FALSE])
    intervals_list <- lapply(seq_len(n_train), function(i) combined_intervals[combined_intervals$Train == i, , drop = FALSE])
    warnings <- character(0)
    if (length(spike_cols) != n_train) {
      warnings <- c(warnings, if (identical(lang, "zh")) {
        sprintf("上传数据集中有 %d 个 spike train 列没有有效 spike 时间戳，已跳过。", length(spike_cols) - n_train)
      } else {
        sprintf("Uploaded dataset skipped %d spike-train column(s) with no valid spike timestamps.", length(spike_cols) - n_train)
      })
    }
    if (nrow(event_table) == 0) {
      warnings <- c(warnings, if (identical(lang, "zh")) {
        "上传数据集没有包含有效时间戳的 event 列；刺激/事件对齐分析将不可用，但 spike train 可视化和 ISI 分析仍可使用。"
      } else {
        "Uploaded dataset contains no event columns with valid timestamps; stimulus-aligned analyses will be unavailable."
      })
    }
    if (isTRUE(auto_label)) {
      warnings <- c(warnings, if (identical(lang, "zh")) {
        "上传 interval 标签是根据当前接受 ISI 范围得到的启发式分析注释，不是模拟器 ground truth。"
      } else {
        "Uploaded interval labels are heuristic annotations derived from the current accepted ISI ranges, not simulator ground truth."
      })
    }

    result <- list(
      source = "uploaded_dataset",
      spikes_list = spikes_list,
      episodes_list = episodes_list,
      intervals_list = intervals_list,
      combined_spikes = combined_spikes,
      combined_episodes = combined_episodes,
      combined_intervals = combined_intervals,
      combined_stimuli = combined_stimuli,
      combined_responses = combined_responses,
      combined_event_epochs = combined_event_epochs,
      combined_units = combined_units,
      combined_unit_stimulus_drive = combined_unit_stimulus_drive,
      unit_stimulus_drive_validation = validate_unit_stimulus_drive_table(combined_unit_stimulus_drive),
      combined_observed_spikes = observed$observed_spikes,
      observation_map = observed$observation_map,
      observation_summary = observed$observation_summary,
      observation_model = observed$model_label,
      train_count = n_train,
      train_labels = train_labels,
      generation_key = generation_key_value,
      seed = seed_value,
      train_seed_audit = train_seed_audit,
      verification_code = verification$code,
      verification_hash = verification$hash,
      warnings = unique(warnings),
      config = sim_config
    )
    if (is.null(reproduction_settings)) reproduction_settings <- collect_reproduction_settings()
    reproduction_settings$input_source <- "uploaded_dataset"
    reproduction_settings$uploaded_dataset_name <- as.character(upload$name)
    result$reproduction_code <- encode_reproduction_code(build_reproduction_payload(reproduction_settings, result))
    result
  }

  all_spike_trains <- eventReactive(input$run, {
    lang <- isolate(current_lang())
    exact_reproduction_config <- loaded_reproduction_config()
    exact_reproduction_expected <- loaded_reproduction_expected()
    if (!is.null(exact_reproduction_config)) {
      n_train <- if (!is.null(exact_reproduction_expected$train_count)) {
        as.integer(exact_reproduction_expected$train_count)
      } else {
        max(1L, as.integer(input_value("spike_train_number", 1)))
      }
      generation_key_value <- if (!is.null(exact_reproduction_expected$generation_key)) {
        as.character(exact_reproduction_expected$generation_key)
      } else {
        normalize_generation_key(value_or(input$generation_key, value_or(input$seed, "12345")))
      }
      reproduction_settings <- collect_reproduction_settings()
      return(simulate_config_dataset(exact_reproduction_config, n_train, generation_key_value, reproduction_settings))
    }
    if (isTRUE(input_value("use_uploaded_dataset", FALSE))) {
      validate(
        need(!is.null(input$uploaded_spike_dataset), tr(lang, "uploaded_missing_file"))
      )
      sim_config <- tryCatch(build_sim_config(NULL), error = function(err) build_uploaded_minimal_config())
      generation_key_value <- normalize_generation_key(value_or(input$generation_key, value_or(input$seed, "12345")))
      reproduction_settings <- collect_reproduction_settings()
      return(simulate_uploaded_dataset(input$uploaded_spike_dataset, sim_config, generation_key_value, reproduction_settings))
    }

    parsed_sequence <- parse_pattern_sequence_strict(input$pattern_sequence)
    sequence_error <- parsed_sequence$error
    inter_event_gap_value <- effective_inter_event_gap()
    if (!is.null(sequence_error) && lang == "zh") {
      sequence_error <- if (grepl("Noisy", sequence_error, fixed = TRUE)) {
        "模式序列包含非法字符或格式错误。请使用 b5、p3、p1.2s、n2、t4 或 *k 重复语法。"
      } else {
        "模式序列包含非法字符或格式错误。请使用 b5、p3、p1.2s、n2、t4 或 *k 重复语法。"
      }
    }

    manual_active_patterns <- character(0)
    if (is.null(parsed_sequence$error) && !is.null(parsed_sequence$tokens) && length(parsed_sequence$tokens) > 0) {
      manual_active_patterns <- unique(vapply(parsed_sequence$tokens, function(x) x$Pattern, character(1)))
      manual_active_patterns <- manual_active_patterns[manual_active_patterns %in% pattern_levels]
    }
    manual_sequence_active <- length(manual_active_patterns) > 0
    needs_tonic_validation <- !manual_sequence_active || "Tonic" %in% manual_active_patterns
    needs_noisy_validation <- !manual_sequence_active || "Noisy" %in% manual_active_patterns

    validate(
      need(is.finite(input$total_time) && input$total_time > 0, tr(lang, "err_total_time")),
      need(is.finite(inter_event_gap_value) && inter_event_gap_value >= 0, tr(lang, "err_gap")),
      need(is.finite(input$spike_train_number) && input$spike_train_number >= 1, tr(lang, "err_train_count")),
      need(!needs_tonic_validation ||
             (valid_nonnegative_range(input$tonic_cv_range) &&
                valid_nonnegative_range(input$tonic_cv2_range) &&
                valid_nonnegative_range(input$tonic_lv_range)), tr(lang, "err_tonic_regularity")),
      need(!needs_noisy_validation ||
             (is.finite(input$noisy_mm_ratio) && input$noisy_mm_ratio > 1), tr(lang, "err_noisy_mm")),
      need(is.null(parsed_sequence$error), sequence_error)
    )

    sim_config <- build_sim_config(parsed_sequence$tokens)
    config_validation <- validate_sim_config_core(sim_config)
    validate(
      need(length(config_validation$errors) == 0, paste(translate_diagnostic_messages(lang, config_validation$errors), collapse = "\n"))
    )

    n_train <- max(1L, as.integer(input$spike_train_number))
    generation_key_value <- normalize_generation_key(value_or(input$generation_key, value_or(input$seed, "12345")))
    reproduction_settings <- collect_reproduction_settings()
    simulate_config_dataset(sim_config, n_train, generation_key_value, reproduction_settings)
  })

  observeEvent(all_spike_trains(), {
    sim <- all_spike_trains()
    n_train <- generated_train_count(sim)
    train_choices <- train_choices_for_sim(n_train, sim, current_lang())
    updateSelectInput(session, "selected_trains", choices = train_choices, selected = head(train_choices, 10L))
    updateSelectInput(session, "distribution_train", choices = train_choices, selected = as.character(selected_distribution_train(n_train, reactive = FALSE, default_train = 1L)))
    updateSelectInput(session, "distribution_train_b", choices = train_choices, selected = as.character(selected_distribution_train(n_train, reactive = FALSE, input_id = "distribution_train_b", default_train = min(2L, n_train))))
  })

  observeEvent(input$spike_train_number, {
    n_train <- current_train_count()
    train_choices <- train_choices_for_sim(n_train, NULL, current_lang())
    updateSelectInput(session, "selected_trains", choices = train_choices, selected = head(train_choices, 10L))
    updateSelectInput(session, "distribution_train", choices = train_choices, selected = as.character(selected_distribution_train(n_train, reactive = FALSE, default_train = 1L)))
    updateSelectInput(session, "distribution_train_b", choices = train_choices, selected = as.character(selected_distribution_train(n_train, reactive = FALSE, input_id = "distribution_train_b", default_train = min(2L, n_train))))
  }, ignoreInit = TRUE)

  observeEvent(input$interval_range_burst, {
    if (auto_inter_event_gap_enabled()) {
      updateNumericInput(session, "inter_event_gap", value = burst_min_isi_value())
    }
  }, ignoreInit = FALSE)

  observeEvent(list(input$interval_range_hft, input$hfs_short_isi_range), {
    if (auto_inter_event_gap_enabled()) {
      updateNumericInput(session, "inter_event_gap", value = burst_min_isi_value())
    }
  }, ignoreInit = TRUE)

  observeEvent(input$auto_inter_event_gap, {
    if (auto_inter_event_gap_enabled()) {
      updateNumericInput(session, "inter_event_gap", value = burst_min_isi_value())
    }
  }, ignoreInit = TRUE)


  validation_suite_results <- eventReactive(input$run_validation_suite, {
    parsed_sequence <- parse_pattern_sequence_strict(value_or(input$pattern_sequence, ""))
    validate(need(is.null(parsed_sequence$error), parsed_sequence$error))
    cfg <- build_sim_config(parsed_sequence$tokens)
    cfg$total_time <- max(safe_num(cfg$total_time, 25), 25)
    seed_base <- max(1L, as.integer(input_value("validation_seed_base", 1)))
    seed_count <- max(1L, as.integer(input_value("validation_seed_count", 3)))
    n_intervals <- max(4L, as.integer(input_value("validation_run_length", 8)))
    seeds <- seq.int(as.integer(seed_base), length.out = seed_count)
    withProgress(message = if (identical(current_lang(), "zh")) "正在运行验证套件" else "Running validation suite", value = 0, {
      res <- list()
      incProgress(0.08, detail = if (identical(current_lang(), "zh")) "1/6 不变量检查" else "1/6 simulator invariants")
      res$invariants <- run_validation_block("invariants", run_simulator_invariant_suite(cfg, seed = seed_base))
      incProgress(0.19, detail = if (identical(current_lang(), "zh")) "2/6 分布验证" else "2/6 distribution validation")
      res$distribution <- run_validation_block("distribution", run_distribution_validation(cfg, seeds = seeds, n_intervals = n_intervals))
      incProgress(0.19, detail = if (identical(current_lang(), "zh")) "3/6 时序依赖验证" else "3/6 temporal dependence")
      res$temporal <- run_validation_block("temporal", run_temporal_dependence_validation(cfg, seeds = seeds, n_intervals = max(8L, n_intervals)))
      incProgress(0.14, detail = if (identical(current_lang(), "zh")) "4/6 刺激响应预设多 seed 验证" else "4/6 multi-seed stimulation validation")
      stim_reps <- run_stimulation_validation_replicates(cfg, seeds = seeds + 1000L)
      res$stimulation <- run_validation_block("stimulation", stim_reps$summary)
      res$stimulation_raw <- run_validation_block("stimulation_raw", stim_reps$raw)
      incProgress(0.18, detail = if (identical(current_lang(), "zh")) "5/6 检测基准" else "5/6 detection benchmark")
      res$detection <- run_validation_block("detection", run_detection_benchmark_suite(cfg, seeds = seeds))
      incProgress(0.16, detail = if (identical(current_lang(), "zh")) "6/6 baseline 对比" else "6/6 baseline comparison")
      res$baselines <- run_validation_block("baselines", run_baseline_comparison_suite(cfg, seeds = seeds, difficulty = "moderate"))
      incProgress(0.06, detail = if (identical(current_lang(), "zh")) "完成" else "done")
      res
    })
  }, ignoreInit = TRUE)

  render_validation_dt <- function(block_name) {
    renderDT({
      res <- validation_suite_results()
      tab <- res[[block_name]]
      if (is.null(tab) || nrow(tab) == 0) {
        return(datatable(data.frame(Message = "No validation rows were produced."), options = list(pageLength = 5, scrollX = TRUE)))
      }
      datatable(tab, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  }

  output$validation_invariants_table <- render_validation_dt("invariants")
  output$validation_distribution_table <- render_validation_dt("distribution")
  output$validation_temporal_table <- render_validation_dt("temporal")
  output$validation_stimulation_table <- render_validation_dt("stimulation")
  output$validation_stimulation_raw_table <- render_validation_dt("stimulation_raw")
  output$validation_detection_table <- render_validation_dt("detection")
  output$validation_baseline_table <- render_validation_dt("baselines")

  output$downloadValidationSuite <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_validation_suite.csv",
    content = function(file) {
      res <- validation_suite_results()
      utils::write.csv(export_pattern_values(validation_suite_to_long_table(res)), file, row.names = FALSE)
    }
  )

  benchmark_dataset_for_difficulty <- function(difficulty) {
    difficulty <- match.arg(tolower(as.character(difficulty)[1]), c("easy", "moderate", "hard"))
    cfg <- benchmark_config_from_ui(difficulty)
    config_validation <- validate_sim_config_core(cfg)
    if (length(config_validation$errors) > 0) {
      stop(paste(translate_diagnostic_messages(current_lang(), config_validation$errors), collapse = "\n"), call. = FALSE)
    }
    generation_key_value <- normalize_generation_key(value_or(input$generation_key, value_or(input$seed, "12345")))
    n_train <- max(1L, as.integer(input_value("spike_train_number", 1)))
	    settings <- benchmark_settings_from_config(cfg)
	    settings$benchmark_preset <- difficulty
	    settings$benchmark_task_mode <- as.character(value_or(cfg$benchmark_task_mode, selected_benchmark_task_mode()))
	    sim <- simulate_config_dataset(cfg, n_train, generation_key_value, settings)
	    sim$benchmark_difficulty <- difficulty
	    sim$benchmark_task_mode <- as.character(value_or(cfg$benchmark_task_mode, selected_benchmark_task_mode()))
	    sim$benchmark_observation_profile <- as.character(value_or(cfg$benchmark_observation_profile, "identity_observation_clean_labels"))
	    sim
	  }

  add_benchmark_column <- function(df, difficulty) {
    df$Benchmark_Difficulty <- rep(difficulty, nrow(df))
    df
  }

  benchmark_task_readme_text <- function(difficulty, benchmark_mode = "clean", observation_profile = "identity_observation_clean_labels", package_type = "complete") {
	    benchmark_mode <- as.character(value_or(benchmark_mode, "clean"))[1]
	    observation_profile <- as.character(value_or(observation_profile, "identity_observation_clean_labels"))[1]
	    package_type <- as.character(value_or(package_type, "complete"))[1]
	    if (!package_type %in% c("complete", "prediction", "scoring")) package_type <- "complete"
	    primary_protocol <- if (identical(benchmark_mode, "realistic_stress")) {
	      "observed time-overlap challenge"
	    } else {
	      "latent interval benchmark"
	    }
	    paste(
	      "# Spike train benchmark task",
	      "",
	      paste0("Difficulty: ", difficulty),
	      paste0("Benchmark mode: ", benchmark_mode),
	      paste0("Observation profile: ", observation_profile),
	      paste0("Package type: ", package_type),
	      paste0("Primary recommended protocol: ", primary_protocol),
	      "",
	      "This task package strictly separates detector-visible inputs from ground-truth and audit annotations.",
	      "During prediction, detector data inputs must come only from detector_inputs/.",
	      "Public task instructions, prediction templates, and scoring protocol descriptions may be read for format information, but must not be used as prediction data.",
	      "Files under ground_truth/ are hidden until scoring and must not be used during prediction.",
	      if (identical(package_type, "prediction")) "This prediction package intentionally excludes ground_truth/. Use the paired scoring package for evaluation after predictions are submitted." else "",
	      if (identical(package_type, "prediction")) "Scoring scripts are included to document the required output format and evaluation protocol; they cannot produce a valid final score until ground-truth files are released in the scoring package." else "",
	      if (identical(package_type, "scoring")) "This scoring package contains hidden ground-truth resources and is not a detector prediction input package." else "",
      "",
	      "Protocol 1: latent interval benchmark",
			      "- Detector-visible input: detector_inputs/*_latent_spike_events_input.csv",
		      "- Input columns: Train, Spike_Index, Time.",
		      "- Prediction file: detector_predictions_latent_interval.csv",
		      "- Required columns: Train, Interval_ID, Pred_Label",
		      "- Template: scoring/detector_predictions_latent_interval_template.csv",
		      "- Scoring script: scoring/score_latent_interval.R",
		      "- Matching key: Train + Interval_ID. Interval_ID is only unique within each spike train.",
	      "- Key-audit fields in score_latent_interval_summary.csv: N_Extra_Prediction_Keys, N_Invalid_Train_Interval_Keys, N_Duplicate_Prediction_Keys, and Prediction_Key_Audit_Pass.",
		      "- Duplicate Train + Interval_ID prediction rows are audited and the first row is used for scoring; prediction keys absent from the truth table are audited rather than silently accepted.",
		      "- Invalid Pred_Label values are not matched to any firing-pattern class; they are counted in N_Invalid_Pred_Labels and set Invalid_Prediction_Audit_Pass = FALSE.",
		      "- Use case: algorithmic interval-label classification when the detector is allowed to operate on the simulator's latent intervals.",
	      "",
	      "Protocol 2: observed time-overlap challenge",
		      "- Detector-visible input: detector_inputs/*_observed_spike_events_input.csv",
	      "- Input columns: Train, Observed_Spike_Index, Time.",
	      "- Prediction file: detector_predictions_observed_time_overlap.csv",
	      "- Required columns: Pred_Start_s, Pred_End_s, Pred_Label; Train is optional and defaults to 1.",
	      "- Template: scoring/detector_predictions_observed_time_overlap_template.csv",
		      "- Scoring script: scoring/score_observed_time_overlap.R",
		      "- Default report: IoU thresholds 0.10, 0.30, and 0.50; IoU >= 0.30 is the recommended primary manuscript metric, with 0.50 as a stringent sensitivity check.",
		      "- Default matching: matching = greedy, a deterministic one-to-one matcher by descending IoU within each label and Train. This remains dependency-free, fast, and transparent.",
		      "- Manuscript-scale optional matching: pass matching = optimal as the fourth scorer argument. If the clue package is installed, the scorer uses Hungarian/LSAP assignment; otherwise it uses a dependency-free exact solver for small candidate sets and stops with an installation hint for larger cases.",
		      "- Optional fifth scorer argument: exact_fallback_limit, default 10 truth and prediction intervals per label when clue is not installed.",
		      "- Scorer outputs include Requested_Matching, Matching_Method, Clue_Available, Clue_Version, and Exact_Fallback_Limit for reproducibility.",
		      "- Invalid Pred_Label values are excluded from temporal matching and reported through N_Invalid_Pred_Labels and Invalid_Prediction_Audit_Pass.",
		      "- Example: Rscript scoring/score_observed_time_overlap.R detector_predictions_observed_time_overlap.csv ground_truth/benchmark_<difficulty>_interval_table.csv 0.10,0.30,0.50 optimal",
		      "- Use case: realistic detector challenge after missed spikes, jitter, false positives, and dead-time merging. Predictions are matched to latent scorable intervals by one-to-one temporal IoU within each Train.",
	      "",
	      "Mode semantics:",
	      "- clean: observation noise is disabled; use this for label-clean latent interval detector benchmarking.",
	      "- realistic_stress: an observed recording-stress benchmark mode. A mild recording/spike-sorting observation layer is enabled; use this for observed spike event detection under imperfect sampling. This mode stresses the observation layer, not biological label ambiguity.",
      "",
      "Shared detector-visible inputs:",
	      "- detector_inputs/*_external_stimulus_table_input.csv",
	      "- This table contains only external stimulus timing, External_Strength, feature value, position, protocol, channel, and repetition metadata.",
	      "- The external stimulus input may be repeated per Train for detector convenience; repeated rows contain no unit-specific drive, tuning, response, or audit fields.",
	      "- External_Strength is the unmodulated external stimulus strength. Unit-modulated Strength, Feature_Drive, Response_Kernel, and Response_Eligible are scoring-only audit fields and are not detector-visible.",
	      "- Non-empty detector-visible external stimulus input requires finite, non-missing External_Strength. The exporter refuses fallback from Strength to avoid unit-specific leakage.",
      "",
      "Ground truth / scoring-only tables:",
	      "- ground_truth/*_interval_table.csv",
	      "- ground_truth/*_episodes.csv",
	      "- ground_truth/*_stimulus_response_table.csv",
	      "- ground_truth/*_event_epoch_table.csv",
	      "- ground_truth/*_unit_tuning_table.csv",
	      "- ground_truth/*_unit_stimulus_drive_table.csv",
	      "- ground_truth/*_observation_map.csv",
      "",
      "Auxiliary reproducibility files:",
	      "- metadata/*_manifest.csv",
	      "- metadata/*_reproduction_code.txt",
	      "- metadata/Train_Seeds.csv (per-train RNG-state audit)",
	      "- metadata/software_parameters.yaml",
	      "- metadata/nwb_mapping.json (NWB-compatible schema mapping only; this is not a native .nwb file export)",
	      "- metadata/model_specification.md, when available",
	      "- metadata/schema_dictionary.csv, when available",
	      "- predictions/*_reference_detector_*_predictions.csv (raw reference detector predictions for reproducibility audits, not detector inputs)",
      "- scoring/score_latent_interval.R",
      "- scoring/score_observed_time_overlap.R",
      "",
	      "Observation model: detector_inputs/*_observed_spike_events_input.csv contains only detector-visible observed spike timestamps after the configured observation-noise layer. ground_truth/*_observed_spike_events_audit.csv and ground_truth/*_observation_map.csv link latent spikes to observed detections, missed spikes, false positives, and dead-time merges. They are scoring-only and must not be used by a detector.",
	      "",
	      "Event epoch table: event_epoch_table.csv stores stimulus-linked non-pattern timing structures such as response latency, interburst gaps, stimulus-spanning intervals, evoked suppression, recovery, and response-failure baseline epochs. These epochs are not Burst/Pause/Tonic/high_frequency_tonic/high_frequency_spiking/Noisy firing-pattern labels and are scoring-only unless a task explicitly declares them as detector targets.",
	      "",
	      "Strict data-input rule: detection algorithms must not use files outside detector_inputs/ as prediction data. Public task instructions and prediction templates may be used only to understand file formats and scoring protocols.",
      sep = "\n"
    )
  }

  benchmark_model_specification_text <- function() {
    paste(
      "# Spike Train Simulator V13 Model Specification",
      "",
      paste0("Schema version: ", SCHEMA_VERSION),
      paste0("Simulator version: ", SIMULATOR_VERSION),
      "",
      "## Benchmark Contract",
      "",
      "Detector-visible data inputs are restricted to detector_inputs/.",
      "Public task instructions, prediction templates, and scoring protocol descriptions may be read for format information.",
      "Files under ground_truth/ are withheld from detectors until scoring and must not be used during prediction.",
      "",
      "Detector-visible files:",
      "- detector_inputs/*_latent_spike_events_input.csv: Train, Spike_Index, Time",
      "- detector_inputs/*_observed_spike_events_input.csv: Train, Observed_Spike_Index, Time",
      "- detector_inputs/*_external_stimulus_table_input.csv: external stimulus timing, External_Strength, feature value, position, protocol, channel, pair, standard/deviant, and repetition metadata",
      "The external stimulus input may be repeated per Train for detector convenience. Repeated rows contain only externally defined stimulus attributes and no unit-specific drive, tuning, response, or audit fields.",
      "",
      "Ground-truth/scoring-only files:",
      "- ground_truth/*_interval_table.csv",
      "- ground_truth/*_episodes.csv",
      "- ground_truth/*_stimulus_response_table.csv",
      "- ground_truth/*_event_epoch_table.csv",
      "- ground_truth/*_unit_tuning_table.csv",
      "- ground_truth/*_unit_stimulus_drive_table.csv",
      "- ground_truth/*_observation_map.csv",
      "- ground_truth/*_latent_spike_events_audit.csv",
      "- ground_truth/*_observed_spike_events_audit.csv",
      "- ground_truth/*_stimulus_table_audit.csv",
      "",
      "External_Strength is the unmodulated external stimulus strength. Unit-modulated Strength, Feature_Drive, Response_Kernel, and Response_Eligible are scoring-only audit fields.",
      "For non-empty detector-visible external stimulus input, External_Strength is required and must be finite; the exporter refuses fallback from Strength to avoid unit-specific leakage.",
      "",
      "Scorable pattern labels are Burst, Pause, Tonic, high_frequency_tonic, high_frequency_spiking, and Noisy. Latency, Interburst_Gap, and Stimulus_Gap are non-pattern timing annotations.",
      "Invalid prediction labels are not matched to any firing-pattern class. They are excluded from class matching, counted in N_Invalid_Pred_Labels, and make Invalid_Prediction_Audit_Pass false.",
      "",
      "Feature-response audit chain: Feature_Matched -> Drive_Above_Threshold -> Response_Kernel -> Response_Eligible -> Response_Attempted -> Response_Generated_OK.",
      "",
      "Prediction-package scoring scripts document the required output format and evaluation protocol. They cannot produce a valid final score until ground-truth files are released in the paired scoring package.",
      sep = "\n"
    )
  }

  benchmark_schema_dictionary_df <- function() {
    row <- function(table, field, type, detector_visible, ground_truth, scoring_only, description) {
      data.frame(
        Table = table,
        Field = field,
        Type = type,
        Detector_Visible = isTRUE(detector_visible),
        Ground_Truth = isTRUE(ground_truth),
        Scoring_Only = isTRUE(scoring_only),
        Description = description,
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, list(
      row("detector_inputs/latent_spike_events_input", "Train", "integer", TRUE, FALSE, FALSE, "Spike train identifier visible to detector."),
      row("detector_inputs/latent_spike_events_input", "Spike_Index", "integer", TRUE, FALSE, FALSE, "Latent spike index visible to latent-interval detector."),
      row("detector_inputs/latent_spike_events_input", "Time", "numeric", TRUE, FALSE, FALSE, "Latent spike timestamp visible to latent-interval detector."),
      row("detector_inputs/observed_spike_events_input", "Train", "integer", TRUE, FALSE, FALSE, "Spike train identifier visible to detector."),
      row("detector_inputs/observed_spike_events_input", "Observed_Spike_Index", "integer", TRUE, FALSE, FALSE, "Observed spike index after observation noise."),
      row("detector_inputs/observed_spike_events_input", "Time", "numeric", TRUE, FALSE, FALSE, "Observed spike timestamp visible to detector."),
      row("detector_inputs/external_stimulus_table_input", "Train", "integer", TRUE, FALSE, FALSE, "Per-train duplicate identifier for detector convenience; not a unit-specific drive field."),
      row("detector_inputs/external_stimulus_table_input", "Stimulus_ID", "integer", TRUE, FALSE, FALSE, "External stimulus identifier."),
      row("detector_inputs/external_stimulus_table_input", "Onset_s", "numeric", TRUE, FALSE, FALSE, "External stimulus onset time."),
      row("detector_inputs/external_stimulus_table_input", "Duration_s", "numeric", TRUE, FALSE, FALSE, "External stimulus duration."),
      row("detector_inputs/external_stimulus_table_input", "External_Strength", "numeric", TRUE, FALSE, FALSE, "Required finite, non-missing unmodulated external stimulus strength; exporter refuses fallback from unit-modulated Strength."),
      row("detector_inputs/external_stimulus_table_input", "Stimulus_Type", "character", TRUE, FALSE, FALSE, "External stimulus type label."),
      row("detector_inputs/external_stimulus_table_input", "Protocol", "character", TRUE, FALSE, FALSE, "External stimulus protocol label."),
      row("detector_inputs/external_stimulus_table_input", "Channel", "character", TRUE, FALSE, FALSE, "External stimulus channel."),
      row("detector_inputs/external_stimulus_table_input", "Feature_Modality", "character", TRUE, FALSE, FALSE, "External feature modality such as orientation, frequency, color, tactile location, or spatial_2d."),
      row("detector_inputs/external_stimulus_table_input", "Stimulus_Feature_Value", "numeric", TRUE, FALSE, FALSE, "One-dimensional external feature value when applicable."),
      row("detector_inputs/external_stimulus_table_input", "Stimulus_Position_X", "numeric", TRUE, FALSE, FALSE, "2D external stimulus x coordinate when applicable."),
      row("detector_inputs/external_stimulus_table_input", "Stimulus_Position_Y", "numeric", TRUE, FALSE, FALSE, "2D external stimulus y coordinate when applicable."),
      row("detector_inputs/external_stimulus_table_input", "Is_Standard", "logical", TRUE, FALSE, FALSE, "Whether this external stimulus is an oddball standard."),
      row("detector_inputs/external_stimulus_table_input", "Is_Deviant", "logical", TRUE, FALSE, FALSE, "Whether this external stimulus is an oddball deviant."),
      row("detector_inputs/external_stimulus_table_input", "Pair_ID", "integer", TRUE, FALSE, FALSE, "Paired-pulse pair identifier when applicable."),
      row("detector_inputs/external_stimulus_table_input", "Repetition_Index", "integer", TRUE, FALSE, FALSE, "Stimulus repetition index."),
      row("detector_inputs/external_stimulus_table_input", "Inter_Stimulus_Interval_s", "numeric", TRUE, FALSE, FALSE, "Interval from previous stimulus when applicable."),
      row("ground_truth/interval_table", "Interval_ID", "integer", FALSE, TRUE, TRUE, "Latent interval key within Train."),
      row("ground_truth/interval_table", "ISI_Label", "character", FALSE, TRUE, TRUE, "Latent interval ground-truth label."),
      row("ground_truth/response_table", "Response_Plan_Feasible", "logical", FALSE, TRUE, TRUE, "Whether response preflight found a feasible minimal response plan."),
      row("ground_truth/response_table", "Response_Commit_OK", "logical", FALSE, TRUE, TRUE, "Whether the planned response was successfully committed."),
      row("ground_truth/response_table", "Response_Rolled_Back", "logical", FALSE, TRUE, TRUE, "Whether a failed response attempt was transactionally rolled back."),
      row("ground_truth/response_table", "Response_Failure_Class", "character", FALSE, TRUE, TRUE, "Coarse response failure class."),
      row("ground_truth/event_epoch_table", "Epoch_Type", "character", FALSE, TRUE, TRUE, "Fine event epoch type."),
      row("ground_truth/event_epoch_table", "Epoch_Class", "character", FALSE, TRUE, TRUE, "Coarse event class."),
      row("ground_truth/event_epoch_table", "Scorable", "logical", FALSE, TRUE, TRUE, "Whether all underlying intervals are scorable pattern intervals."),
      row("ground_truth/observation_map", "Observation_Status", "character", FALSE, TRUE, TRUE, "Latent-to-observed detection status."),
      row("ground_truth/stimulus_table_audit", "Strength", "numeric", FALSE, TRUE, TRUE, "Unit-modulated strength; scoring-only audit field."),
      row("ground_truth/stimulus_table_audit", "Feature_Drive", "numeric", FALSE, TRUE, TRUE, "Unit-specific feature drive; scoring-only audit field."),
      row("ground_truth/unit_stimulus_drive_table", "Feature_Matched", "logical", FALSE, TRUE, TRUE, "Whether the stimulus matched the unit tuning field."),
      row("ground_truth/unit_stimulus_drive_table", "External_Strength_Source", "character", FALSE, TRUE, TRUE, "Audit source for External_Strength: explicit field, legacy fallback from Strength, or missing."),
      row("ground_truth/unit_stimulus_drive_table", "Drive_Above_Threshold", "logical", FALSE, TRUE, TRUE, "Whether modulated drive crossed the response threshold."),
      row("ground_truth/unit_stimulus_drive_table", "Response_Kernel", "character", FALSE, TRUE, TRUE, "Response kernel selected by feature-response rules."),
      row("ground_truth/unit_stimulus_drive_table", "Response_Eligible", "logical", FALSE, TRUE, TRUE, "Whether an evoked response should be attempted."),
      row("ground_truth/unit_stimulus_drive_table", "Response_Attempted", "logical", FALSE, TRUE, TRUE, "Whether stochastic response reliability allowed an attempt."),
      row("ground_truth/unit_stimulus_drive_table", "Response_Generated_OK", "logical", FALSE, TRUE, TRUE, "Whether the response was successfully generated and committed."),
      row("ground_truth/unit_stimulus_drive_table", "Response_Failure_Class", "character", FALSE, TRUE, TRUE, "Coarse response failure class when present."),
      row("metadata/benchmark_manifest", "Benchmark_Package_Type", "character", FALSE, FALSE, FALSE, "Export package type: complete, prediction, or scoring."),
      row("metadata/benchmark_manifest", "Detector_Input_Contract", "character", FALSE, FALSE, FALSE, "Statement restricting detector data inputs to detector_inputs/."),
      row("metadata/benchmark_manifest", "Public_Task_Instructions_Contract", "character", FALSE, FALSE, FALSE, "Statement allowing public instructions and templates for format information but not prediction data."),
      row("metadata/benchmark_manifest", "External_Stimulus_Input_Granularity", "character", FALSE, FALSE, FALSE, "Declares that external stimulus input may be duplicated per Train for detector convenience."),
      row("metadata/benchmark_manifest", "External_Stimulus_Input_Strict_Contract", "character", FALSE, FALSE, FALSE, "Declares strict External_Strength requirement and no fallback from Strength."),
      row("metadata/benchmark_manifest", "N_Missing_External_Strength_Input", "integer", FALSE, FALSE, FALSE, "Number of detector-visible external stimulus input rows with missing, NaN, or infinite External_Strength."),
      row("metadata/benchmark_manifest", "External_Strength_Input_Audit_Pass", "logical", FALSE, FALSE, FALSE, "TRUE when every non-empty detector-visible external stimulus input row has finite External_Strength."),
      row("metadata/benchmark_manifest", "N_Fallback_External_Strength", "integer", FALSE, FALSE, FALSE, "Number of ground-truth audit rows using legacy fallback from Strength."),
      row("metadata/benchmark_manifest", "N_Missing_External_Strength_Source", "integer", FALSE, FALSE, FALSE, "Number of ground-truth audit rows with missing External_Strength source."),
      row("metadata/benchmark_manifest", "External_Strength_Source_Warning", "logical", FALSE, FALSE, FALSE, "TRUE when any fallback or missing External_Strength source appears in the audit tables.")
    ))
  }

	  latent_interval_scoring_script_text <- function() {
	    paste(
	      "#!/usr/bin/env Rscript",
	      "args <- commandArgs(trailingOnly = TRUE)",
	      "finite_mean <- function(x) { x <- x[is.finite(x)]; if (length(x) > 0) mean(x) else NA_real_ }",
	      "prediction_path <- if (length(args) >= 1) args[[1]] else 'detector_predictions_latent_interval.csv'",
	      "truth_path <- if (length(args) >= 2) args[[2]] else list.files(pattern = '_interval_table.csv$', full.names = TRUE, recursive = TRUE)[1]",
	      "if (!file.exists(prediction_path)) stop('Missing detector_predictions_latent_interval.csv. Required columns: Train, Interval_ID, Pred_Label.')",
	      "if (is.na(truth_path) || !file.exists(truth_path)) stop('Missing interval ground truth table.')",
	      "truth <- read.csv(truth_path, stringsAsFactors = FALSE)",
	      "pred <- read.csv(prediction_path, stringsAsFactors = FALSE)",
	      "if (!'Train' %in% names(truth)) truth$Train <- 1L",
	      "if (!all(c('Train', 'Interval_ID', 'Pred_Label') %in% names(pred))) stop('Prediction file must contain Train, Interval_ID, and Pred_Label.')",
	      "truth$Train <- as.integer(truth$Train)",
	      "truth$Interval_ID <- as.integer(truth$Interval_ID)",
		      "pred$Train <- suppressWarnings(as.integer(pred$Train))",
		      "pred$Interval_ID <- suppressWarnings(as.integer(pred$Interval_ID))",
		      "pred$Pred_Label <- as.character(pred$Pred_Label)",
		      "valid_labels <- c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')",
		      "n_invalid_pred_labels <- sum(!is.na(pred$Pred_Label) & !pred$Pred_Label %in% valid_labels)",
		      "pred$Pred_Label[!pred$Pred_Label %in% valid_labels] <- 'Invalid_Label'",
		      "truth_key <- paste(truth$Train, truth$Interval_ID, sep = '::')",
		      "pred_key_valid <- is.finite(pred$Train) & is.finite(pred$Interval_ID)",
		      "pred_key <- rep(NA_character_, nrow(pred))",
		      "pred_key[pred_key_valid] <- paste(pred$Train[pred_key_valid], pred$Interval_ID[pred_key_valid], sep = '::')",
		      "n_invalid_train_interval_keys <- sum(!pred_key_valid)",
		      "n_duplicate_prediction_keys <- sum(duplicated(pred_key[pred_key_valid]))",
		      "valid_unique_prediction_keys <- unique(pred_key[pred_key_valid])",
		      "n_extra_prediction_keys <- sum(!valid_unique_prediction_keys %in% unique(truth_key))",
		      "pred_for_scoring <- pred[pred_key_valid & !duplicated(pred_key), c('Train', 'Interval_ID', 'Pred_Label'), drop = FALSE]",
		      "dat <- merge(truth, pred_for_scoring, by = c('Train', 'Interval_ID'), all.x = TRUE)",
	      "dat$Pred_Label[is.na(dat$Pred_Label)] <- 'Unclassified'",
      "scorable <- dat$ISI_Label %in% c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')",
      "dat <- dat[scorable, , drop = FALSE]",
      "labels <- c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')",
      "rows <- lapply(labels, function(label) {",
      "  tp <- sum(dat$ISI_Label == label & dat$Pred_Label == label)",
      "  fp <- sum(dat$ISI_Label != label & dat$Pred_Label == label)",
      "  fn <- sum(dat$ISI_Label == label & dat$Pred_Label != label)",
      "  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0",
      "  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0",
      "  f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0",
      "  data.frame(Label = label, TP = tp, FP = fp, FN = fn, Precision = precision, Recall = recall, F1 = f1)",
      "})",
      "per_class <- do.call(rbind, rows)",
      "macro_mask <- (per_class$TP + per_class$FP + per_class$FN) > 0",
	      "summary <- data.frame(Interval_Accuracy = if (nrow(dat) > 0) mean(dat$ISI_Label == dat$Pred_Label) else NA_real_, Macro_F1 = if (any(macro_mask)) mean(per_class$F1[macro_mask]) else NA_real_, Macro_F1_Labels = paste(per_class$Label[macro_mask], collapse = ';'), N_Scorable = nrow(dat), N_Prediction_Rows = nrow(pred), N_Prediction_Rows_Used = nrow(pred_for_scoring), N_Extra_Prediction_Keys = n_extra_prediction_keys, N_Invalid_Train_Interval_Keys = n_invalid_train_interval_keys, N_Duplicate_Prediction_Keys = n_duplicate_prediction_keys, N_Invalid_Pred_Labels = n_invalid_pred_labels, Prediction_Key_Audit_Pass = n_extra_prediction_keys == 0 && n_invalid_train_interval_keys == 0 && n_duplicate_prediction_keys == 0, Invalid_Prediction_Audit_Pass = n_invalid_pred_labels == 0)",
      "write.csv(per_class, 'score_latent_interval_per_class.csv', row.names = FALSE)",
      "write.csv(summary, 'score_latent_interval_summary.csv', row.names = FALSE)",
      "print(summary)",
      sep = "\n"
    )
  }

	  observed_time_overlap_scoring_script_text <- function() {
	    paste(
	      "#!/usr/bin/env Rscript",
	      "args <- commandArgs(trailingOnly = TRUE)",
	      "finite_mean <- function(x) { x <- x[is.finite(x)]; if (length(x) > 0) mean(x) else NA_real_ }",
	      "prediction_path <- if (length(args) >= 1) args[[1]] else 'detector_predictions_observed_time_overlap.csv'",
	      "truth_path <- if (length(args) >= 2) args[[2]] else list.files(pattern = '_interval_table.csv$', full.names = TRUE, recursive = TRUE)[1]",
	      "parse_iou_thresholds <- function(x) { vals <- suppressWarnings(as.numeric(strsplit(paste(x, collapse = ','), ',', fixed = TRUE)[[1]])); vals <- vals[is.finite(vals) & vals >= 0 & vals <= 1]; unique(vals) }",
	      "iou_thresholds <- if (length(args) >= 3) parse_iou_thresholds(args[[3]]) else c(0.10, 0.30, 0.50)",
	      "if (length(iou_thresholds) == 0) stop('IoU thresholds must be finite values between 0 and 1, e.g. 0.10,0.30,0.50.')",
	      "matching <- if (length(args) >= 4) tolower(args[[4]]) else 'greedy'",
	      "if (!matching %in% c('greedy', 'optimal')) stop(\"matching must be 'greedy' or 'optimal'.\")",
	      "exact_fallback_limit <- if (length(args) >= 5) suppressWarnings(as.integer(args[[5]])) else 10L",
	      "if (!is.finite(exact_fallback_limit) || exact_fallback_limit < 1L) exact_fallback_limit <- 10L",
	      "clue_available <- requireNamespace('clue', quietly = TRUE)",
	      "clue_version <- if (clue_available) as.character(utils::packageVersion('clue')) else NA_character_",
	      "matching_method_used <- if (identical(matching, 'greedy')) 'greedy_descending_iou_by_label_and_train' else if (clue_available) 'optimal_lsap_clue_by_label_and_train' else 'optimal_exact_base_r_by_label_and_train'",
      "if (!file.exists(prediction_path)) stop('Missing detector_predictions_observed_time_overlap.csv. Required columns: Pred_Start_s, Pred_End_s, Pred_Label; Train is optional.')",
      "if (is.na(truth_path) || !file.exists(truth_path)) stop('Missing interval ground truth table.')",
      "truth <- read.csv(truth_path, stringsAsFactors = FALSE)",
      "pred <- read.csv(prediction_path, stringsAsFactors = FALSE)",
      "required_truth <- c('Start_Time_s', 'End_Time_s', 'ISI_Label')",
      "if (!all(required_truth %in% names(truth))) stop('Truth table must contain Start_Time_s, End_Time_s, and ISI_Label.')",
      "if (!all(c('Pred_Start_s', 'Pred_End_s', 'Pred_Label') %in% names(pred))) stop('Prediction file must contain Pred_Start_s, Pred_End_s, and Pred_Label.')",
      "if (!'Train' %in% names(truth)) truth$Train <- 1L",
      "if (!'Train' %in% names(pred)) pred$Train <- 1L",
      "labels <- c('Burst', 'Pause', 'Tonic', 'high_frequency_tonic', 'high_frequency_spiking', 'Noisy')",
      "n_invalid_pred_labels <- sum(!is.na(pred$Pred_Label) & !as.character(pred$Pred_Label) %in% labels)",
      "truth <- truth[truth$ISI_Label %in% labels & is.finite(truth$Start_Time_s) & is.finite(truth$End_Time_s) & truth$End_Time_s > truth$Start_Time_s, , drop = FALSE]",
      "pred <- pred[pred$Pred_Label %in% labels & is.finite(pred$Pred_Start_s) & is.finite(pred$Pred_End_s) & pred$Pred_End_s > pred$Pred_Start_s, , drop = FALSE]",
      "iou_pair <- function(a0, a1, b0, b1) {",
      "  inter <- max(0, min(a1, b1) - max(a0, b0))",
      "  uni <- max(a1, b1) - min(a0, b0)",
      "  if (!is.finite(uni) || uni <= 0) return(0)",
      "  inter / uni",
      "}",
	      "empty_match <- function() data.frame(Truth_Row = integer(0), Pred_Row = integer(0), IoU = numeric(0))",
	      "greedy_match <- function(candidates, n_truth, n_pred) {",
	      "  if (nrow(candidates) == 0) return(empty_match())",
	      "  candidates <- candidates[order(-candidates$IoU), , drop = FALSE]",
	      "  matched_truth <- integer(0)",
	      "  matched_pred <- integer(0)",
	      "  rows <- empty_match()",
	      "  for (ci in seq_len(nrow(candidates))) {",
	      "    ti <- as.integer(candidates$Truth_Row[ci])",
	      "    pi <- as.integer(candidates$Pred_Row[ci])",
	      "    if (ti %in% matched_truth || pi %in% matched_pred) next",
	      "    matched_truth <- c(matched_truth, ti)",
	      "    matched_pred <- c(matched_pred, pi)",
	      "    rows <- rbind(rows, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = candidates$IoU[ci]))",
	      "  }",
	      "  rows",
	      "}",
	      "optimal_lsap_match <- function(candidates, n_truth, n_pred) {",
	      "  if (nrow(candidates) == 0) return(empty_match())",
	      "  side <- max(n_truth, n_pred, 1L)",
	      "  weight <- matrix(0, nrow = side, ncol = side)",
	      "  iou_lookup <- matrix(NA_real_, nrow = side, ncol = side)",
	      "  for (ci in seq_len(nrow(candidates))) {",
	      "    ti <- as.integer(candidates$Truth_Row[ci])",
	      "    pi <- as.integer(candidates$Pred_Row[ci])",
	      "    score <- as.numeric(candidates$IoU[ci])",
	      "    if (!is.finite(score) || ti < 1L || pi < 1L || ti > n_truth || pi > n_pred) next",
	      "    w <- 1e6 + score",
	      "    if (w > weight[ti, pi]) {",
	      "      weight[ti, pi] <- w",
	      "      iou_lookup[ti, pi] <- score",
	      "    }",
	      "  }",
	      "  assignment <- clue::solve_LSAP(weight, maximum = TRUE)",
	      "  rows <- empty_match()",
	      "  for (ti in seq_len(n_truth)) {",
	      "    pi <- as.integer(assignment[ti])",
	      "    if (pi <= n_pred && weight[ti, pi] > 0) rows <- rbind(rows, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = iou_lookup[ti, pi]))",
	      "  }",
	      "  rows",
	      "}",
	      "exact_small_match <- function(candidates, n_truth, n_pred, limit = 10L) {",
	      "  if (nrow(candidates) == 0) return(empty_match())",
	      "  if (n_truth > limit || n_pred > limit) {",
	      "    stop(paste0(\"matching='optimal' needs package 'clue' for this candidate size. Install clue, rerun with matching='greedy', or use exact fallback only for <= \", limit, \" truth and prediction intervals per label.\"))",
	      "  }",
	      "  weight <- matrix(0, nrow = n_truth, ncol = n_pred)",
	      "  iou_lookup <- matrix(NA_real_, nrow = n_truth, ncol = n_pred)",
	      "  for (ci in seq_len(nrow(candidates))) {",
	      "    ti <- as.integer(candidates$Truth_Row[ci])",
	      "    pi <- as.integer(candidates$Pred_Row[ci])",
	      "    score <- as.numeric(candidates$IoU[ci])",
	      "    if (!is.finite(score) || ti < 1L || pi < 1L || ti > n_truth || pi > n_pred) next",
	      "    w <- 1e6 + score",
	      "    if (w > weight[ti, pi]) {",
	      "      weight[ti, pi] <- w",
	      "      iou_lookup[ti, pi] <- score",
	      "    }",
	      "  }",
	      "  memo <- new.env(parent = emptyenv())",
	      "  choice <- new.env(parent = emptyenv())",
	      "  solve_state <- function(ti, mask) {",
	      "    if (ti > n_truth) return(0)",
	      "    key <- paste(ti, mask, sep = ':')",
	      "    if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))",
	      "    best <- solve_state(ti + 1L, mask)",
	      "    best_pred <- 0L",
	      "    available <- which(weight[ti, ] > 0)",
	      "    if (length(available) > 0) {",
	      "      for (pi in available) {",
	      "        bit <- bitwShiftL(1L, pi - 1L)",
	      "        if (bitwAnd(mask, bit) != 0L) next",
	      "        val <- weight[ti, pi] + solve_state(ti + 1L, bitwOr(mask, bit))",
	      "        if (val > best) {",
	      "          best <- val",
	      "          best_pred <- as.integer(pi)",
	      "        }",
	      "      }",
	      "    }",
	      "    assign(key, best, envir = memo)",
	      "    assign(key, best_pred, envir = choice)",
	      "    best",
	      "  }",
	      "  invisible(solve_state(1L, 0L))",
	      "  rows <- empty_match()",
	      "  mask <- 0L",
	      "  for (ti in seq_len(n_truth)) {",
	      "    key <- paste(ti, mask, sep = ':')",
	      "    pi <- if (exists(key, envir = choice, inherits = FALSE)) get(key, envir = choice, inherits = FALSE) else 0L",
	      "    if (pi > 0L) {",
	      "      rows <- rbind(rows, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = iou_lookup[ti, pi]))",
	      "      mask <- bitwOr(mask, bitwShiftL(1L, pi - 1L))",
	      "    }",
	      "  }",
	      "  rows",
	      "}",
	      "select_matches <- function(candidates, n_truth, n_pred) {",
	      "  if (identical(matching, 'greedy')) return(greedy_match(candidates, n_truth, n_pred))",
	      "  if (clue_available) return(optimal_lsap_match(candidates, n_truth, n_pred))",
	      "  exact_small_match(candidates, n_truth, n_pred, exact_fallback_limit)",
	      "}",
	      "score_one_threshold <- function(min_iou) {",
	      "match_one_label <- function(label) {",
	      "  t <- truth[truth$ISI_Label == label, , drop = FALSE]",
      "  p <- pred[pred$Pred_Label == label, , drop = FALSE]",
      "  candidates <- data.frame()",
      "  if (nrow(t) > 0 && nrow(p) > 0) {",
      "    for (ti in seq_len(nrow(t))) {",
      "      same_train <- which(as.integer(p$Train) == as.integer(t$Train[ti]))",
      "      if (length(same_train) == 0) next",
      "      for (pi in same_train) {",
      "        score <- iou_pair(t$Start_Time_s[ti], t$End_Time_s[ti], p$Pred_Start_s[pi], p$Pred_End_s[pi])",
      "        if (is.finite(score) && score >= min_iou) {",
      "          candidates <- rbind(candidates, data.frame(Truth_Row = ti, Pred_Row = pi, IoU = score))",
      "        }",
      "      }",
      "    }",
      "  }",
	      "  matched <- select_matches(candidates, nrow(t), nrow(p))",
	      "  matched_truth <- as.integer(matched$Truth_Row)",
	      "  matched_pred <- as.integer(matched$Pred_Row)",
	      "  matched_iou <- as.numeric(matched$IoU)",
	      "  onset_err <- if (nrow(matched) > 0) p$Pred_Start_s[matched_pred] - t$Start_Time_s[matched_truth] else numeric(0)",
	      "  offset_err <- if (nrow(matched) > 0) p$Pred_End_s[matched_pred] - t$End_Time_s[matched_truth] else numeric(0)",
      "  tp <- nrow(matched)",
      "  fp <- max(0L, nrow(p) - tp)",
      "  fn <- max(0L, nrow(t) - tp)",
      "  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0",
      "  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0",
      "  f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0",
	      "  data.frame(Min_IoU = min_iou, Requested_Matching = matching, Matching_Method = matching_method_used, Clue_Available = clue_available, Clue_Version = clue_version, Exact_Fallback_Limit = exact_fallback_limit, Label = label, Truth_Intervals = nrow(t), Predicted_Intervals = nrow(p), TP = tp, FP = fp, FN = fn, Precision = precision, Recall = recall, F1 = f1, Mean_IoU = if (length(matched_iou) > 0) mean(matched_iou) else NA_real_, Mean_Onset_Error_s = if (length(onset_err) > 0) mean(onset_err) else NA_real_, Mean_Offset_Error_s = if (length(offset_err) > 0) mean(offset_err) else NA_real_)",
	      "}",
	      "per_class <- do.call(rbind, lapply(labels, match_one_label))",
      "tp <- sum(per_class$TP)",
      "fp <- sum(per_class$FP)",
      "fn <- sum(per_class$FN)",
      "micro_precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0",
      "micro_recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0",
      "micro_f1 <- if ((micro_precision + micro_recall) > 0) 2 * micro_precision * micro_recall / (micro_precision + micro_recall) else 0",
      "macro_mask <- (per_class$TP + per_class$FP + per_class$FN) > 0",
	      "summary <- data.frame(Min_IoU = min_iou, Requested_Matching = matching, Matching_Method = matching_method_used, Clue_Available = clue_available, Clue_Version = clue_version, Exact_Fallback_Limit = exact_fallback_limit, Truth_Intervals = nrow(truth), Predicted_Intervals = nrow(pred), N_Invalid_Pred_Labels = n_invalid_pred_labels, Invalid_Prediction_Audit_Pass = n_invalid_pred_labels == 0, TP = tp, FP = fp, FN = fn, Micro_Precision = micro_precision, Micro_Recall = micro_recall, Micro_F1 = micro_f1, Macro_F1 = if (any(macro_mask)) mean(per_class$F1[macro_mask]) else NA_real_, Macro_F1_Labels = paste(per_class$Label[macro_mask], collapse = ';'), Mean_Matched_IoU = finite_mean(per_class$Mean_IoU), Primary_Metric = min_iou == 0.30)",
	      "list(per_class = per_class, summary = summary)",
	      "}",
	      "scored <- lapply(iou_thresholds, score_one_threshold)",
	      "per_class <- do.call(rbind, lapply(scored, `[[`, 'per_class'))",
	      "summary <- do.call(rbind, lapply(scored, `[[`, 'summary'))",
	      "write.csv(per_class, 'score_observed_time_overlap_per_class.csv', row.names = FALSE)",
      "write.csv(summary, 'score_observed_time_overlap_summary.csv', row.names = FALSE)",
      "print(summary)",
      sep = "\n"
    )
  }

  software_parameters_payload <- function(sim, difficulty, package_type = "complete") {
    list(
      simulator = list(
        id = SIMULATOR_ID,
        version = SIMULATOR_VERSION,
        schema_version = SCHEMA_VERSION,
        config_hash = config_hash_from_config(sim$config),
        verification_code = value_or(sim$verification_code, NA_character_),
        verification_hash = value_or(sim$verification_hash, NA_character_)
      ),
      generation = list(
        benchmark_difficulty = difficulty,
        benchmark_package_type = package_type,
        benchmark_mode = as.character(value_or(sim$benchmark_task_mode, value_or(sim$config$benchmark_task_mode, "clean"))),
        benchmark_observation_profile = as.character(value_or(sim$benchmark_observation_profile, value_or(sim$config$benchmark_observation_profile, "identity_observation_clean_labels"))),
        generation_key = value_or(sim$generation_key, NA_character_),
        derived_rng_seed = value_or(sim$seed, NA_integer_),
        train_count = generated_train_count(sim),
        observation_model = value_or(sim$observation_model, "identity")
      ),
      rng = list(
        note = "Per-train .Random.seed states are stored in metadata/Train_Seeds.csv. The dataset-level derived seed initializes the simulation.",
        train_seed_table = "metadata/Train_Seeds.csv"
      ),
      config = sim$config
    )
  }

  write_software_parameters_yaml <- function(payload, file) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      writeLines(yaml::as.yaml(payload), file)
    } else if (requireNamespace("jsonlite", quietly = TRUE)) {
      writeLines(c(
        "# yaml package was unavailable; JSON-compatible parameter payload follows.",
        jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = 15)
      ), file)
    } else {
      writeLines("# No yaml/jsonlite serializer available.", file)
    }
  }

  reference_latent_interval_predictions <- function(sim, config = NULL, detector_name = "simple_threshold_reference") {
    config <- value_or(config, sim$config)
    intervals <- if (!is.null(sim$combined_intervals)) sim$combined_intervals else make_empty_interval_df()
    spikes <- if (!is.null(sim$combined_spikes)) sim$combined_spikes else make_empty_spike_df()
    if (nrow(intervals) == 0) {
      return(data.frame(Train = integer(0), Interval_ID = integer(0), Pred_Label = character(0),
                        Detector_Name = character(0), stringsAsFactors = FALSE))
    }
    rows <- list()
    for (train_id in sort(unique(as.integer(intervals$Train)))) {
      train_intervals <- intervals[as.integer(intervals$Train) == train_id, , drop = FALSE]
      train_intervals <- train_intervals[order(train_intervals$Interval_ID), , drop = FALSE]
      train_spikes <- spikes[as.integer(spikes$Train) == train_id, , drop = FALSE]
      train_spikes <- train_spikes[order(train_spikes$Time), , drop = FALSE]
      pred <- simple_threshold_interval_detector(train_spikes$Time, config)
      if (length(pred) < nrow(train_intervals)) pred <- c(pred, rep("Unclassified", nrow(train_intervals) - length(pred)))
      if (length(pred) > nrow(train_intervals)) pred <- pred[seq_len(nrow(train_intervals))]
      rows[[length(rows) + 1L]] <- data.frame(
        Train = as.integer(train_id),
        Interval_ID = as.integer(train_intervals$Interval_ID),
        Pred_Label = as.character(pred),
        Detector_Name = detector_name,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0) return(data.frame(Train = integer(0), Interval_ID = integer(0), Pred_Label = character(0), Detector_Name = character(0), stringsAsFactors = FALSE))
    do.call(rbind, rows)
  }

  reference_observed_time_overlap_predictions <- function(sim, config = NULL, detector_name = "simple_threshold_reference") {
    config <- value_or(config, sim$config)
    observed <- observed_spike_events_input_table(sim)
    if (nrow(observed) < 2) {
      return(data.frame(Train = integer(0), Pred_Start_s = numeric(0), Pred_End_s = numeric(0),
                        Pred_Label = character(0), Detector_Name = character(0), stringsAsFactors = FALSE))
    }
    rows <- list()
    for (train_id in sort(unique(as.integer(observed$Train)))) {
      obs_train <- observed[as.integer(observed$Train) == train_id, , drop = FALSE]
      obs_train <- obs_train[order(obs_train$Time, obs_train$Observed_Spike_Index), , drop = FALSE]
      if (nrow(obs_train) < 2) next
      pred <- simple_threshold_interval_detector(obs_train$Time, config)
      n_pred <- min(length(pred), nrow(obs_train) - 1L)
      if (n_pred <= 0) next
      rows[[length(rows) + 1L]] <- data.frame(
        Train = as.integer(train_id),
        Pred_Start_s = obs_train$Time[seq_len(n_pred)],
        Pred_End_s = obs_train$Time[seq_len(n_pred) + 1L],
        Pred_Label = as.character(pred[seq_len(n_pred)]),
        Left_Observed_Spike_Index = as.integer(obs_train$Observed_Spike_Index[seq_len(n_pred)]),
        Right_Observed_Spike_Index = as.integer(obs_train$Observed_Spike_Index[seq_len(n_pred) + 1L]),
        Detector_Name = detector_name,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0) return(data.frame(Train = integer(0), Pred_Start_s = numeric(0), Pred_End_s = numeric(0), Pred_Label = character(0), Detector_Name = character(0), stringsAsFactors = FALSE))
    do.call(rbind, rows)
  }

  write_benchmark_dataset_zip <- function(sim, difficulty, file, package_type = "complete") {
    package_type <- tolower(as.character(value_or(package_type, "complete"))[1])
    if (!package_type %in% c("complete", "prediction", "scoring")) package_type <- "complete"
    temp_dir <- tempfile("spike_benchmark_dataset_")
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    spike_events <- spike_isi_table(sim$combined_spikes)
    spike_events <- add_duration_columns(spike_events, sim)
    spike_events <- export_pattern_values(spike_events)
    spike_events <- add_benchmark_column(spike_events, difficulty)
    spike_events <- add_reproducibility_columns(spike_events, sim)
    latent_spike_events <- spike_events
    observed_spike_events <- observed_spike_events_table(sim)
    observed_spike_events <- add_duration_columns(observed_spike_events, sim)
    observed_spike_events <- add_benchmark_column(observed_spike_events, difficulty)
    observed_spike_events <- add_reproducibility_columns(observed_spike_events, sim)
    observed_spike_events$Observation_Model <- rep(value_or(sim$observation_model, "identity"), nrow(observed_spike_events))

    observation_map <- if (!is.null(sim$observation_map)) sim$observation_map else make_empty_observation_map_df()
    observation_map <- add_duration_columns(observation_map, sim)
    observation_map <- add_benchmark_column(observation_map, difficulty)
    observation_map <- add_reproducibility_columns(observation_map, sim)
    observation_map$Observation_Model <- rep(value_or(sim$observation_model, "identity"), nrow(observation_map))

    interval_details <- sim$combined_intervals
    interval_details <- add_duration_columns(interval_details, sim)
    interval_details <- export_pattern_values(interval_details)
    interval_details <- add_benchmark_column(interval_details, difficulty)
    interval_details <- add_reproducibility_columns(interval_details, sim)

    episode_data <- sim$combined_episodes
    episode_data <- add_duration_columns(episode_data, sim)
    episode_data <- export_pattern_values(episode_data)
    episode_data <- add_benchmark_column(episode_data, difficulty)
    episode_data <- add_reproducibility_columns(episode_data, sim)

    event_epoch_data <- if (!is.null(sim$combined_event_epochs)) sim$combined_event_epochs else make_empty_event_epoch_df()
    event_epoch_data <- add_duration_columns(event_epoch_data, sim)
    event_epoch_data <- export_pattern_values(event_epoch_data)
    event_epoch_data <- add_benchmark_column(event_epoch_data, difficulty)
    event_epoch_data <- add_reproducibility_columns(event_epoch_data, sim)

    unit_data <- if (!is.null(sim$combined_units)) sim$combined_units else make_empty_unit_df()
    unit_data <- add_duration_columns(unit_data, sim)
    unit_data <- add_benchmark_column(unit_data, difficulty)
    unit_data <- add_reproducibility_columns(unit_data, sim)

    unit_drive_data <- if (!is.null(sim$combined_unit_stimulus_drive)) sim$combined_unit_stimulus_drive else make_empty_unit_stimulus_drive_df()
    unit_drive_data <- add_duration_columns(unit_drive_data, sim)
    unit_drive_data <- add_benchmark_column(unit_drive_data, difficulty)
    unit_drive_data <- add_reproducibility_columns(unit_drive_data, sim)
    external_strength_sources <- if ("External_Strength_Source" %in% names(unit_drive_data)) as.character(unit_drive_data$External_Strength_Source) else character(0)
    n_external_strength_fallback <- sum(external_strength_sources == "fallback_strength_legacy", na.rm = TRUE)
    n_external_strength_missing <- sum(external_strength_sources %in% c("missing", "explicit_external_strength_missing"), na.rm = TRUE)
    external_strength_source_warning <- (n_external_strength_fallback + n_external_strength_missing) > 0

    stimulus_data <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df()
    stimulus_data <- add_duration_columns(stimulus_data, sim)
    stimulus_data <- export_pattern_values(stimulus_data)
    stimulus_data <- add_benchmark_column(stimulus_data, difficulty)
    stimulus_data <- add_reproducibility_columns(stimulus_data, sim)

    response_data <- if (!is.null(sim$combined_responses)) sim$combined_responses else make_empty_response_df()
    response_data <- add_duration_columns(response_data, sim)
    response_data <- export_pattern_values(response_data)
    response_data <- add_benchmark_column(response_data, difficulty)
    response_data <- add_reproducibility_columns(response_data, sim)

    train_seed_data <- train_seed_table(sim)
    latent_reference_predictions <- reference_latent_interval_predictions(sim, sim$config)
    observed_reference_predictions <- reference_observed_time_overlap_predictions(sim, sim$config)

    latent_spike_events_input <- latent_spike_events_input_table(sim)
    observed_spike_events_input <- observed_spike_events_input_table(sim)
    external_stimulus_input <- external_stimulus_table_input(if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df(), strict = TRUE)
    external_strength_input_values <- if ("External_Strength" %in% names(external_stimulus_input)) {
      suppressWarnings(as.numeric(external_stimulus_input$External_Strength))
    } else {
      numeric(0)
    }
    n_missing_external_strength_input <- sum(!is.finite(external_strength_input_values))
    external_strength_input_audit_pass <- nrow(external_stimulus_input) == 0 || n_missing_external_strength_input == 0

    quality_config <- if (valid_benchmark_preset(difficulty)) benchmark_config_from_ui(difficulty) else sim$config
    quality <- benchmark_quality_metrics(sim$combined_intervals, sim$combined_episodes, quality_config)
	    manifest <- data.frame(
	      Schema_Version = SCHEMA_VERSION,
	      Simulator_Version = SIMULATOR_VERSION,
	      Simulator_ID = SIMULATOR_ID,
	      Config_Hash = config_hash_from_config(sim$config),
	      Benchmark_Difficulty = difficulty,
	      Benchmark_Package_Type = package_type,
	      Benchmark_Mode = as.character(value_or(sim$benchmark_task_mode, value_or(sim$config$benchmark_task_mode, "clean"))),
	      Benchmark_Observation_Profile = as.character(value_or(sim$benchmark_observation_profile, value_or(sim$config$benchmark_observation_profile, "identity_observation_clean_labels"))),
	      Benchmark_Quality_Policy = "contextual_noisy_refractory_envelope",
	      Benchmark_Clean_Label_OK = quality$Benchmark_Clean_Label_OK,
      Noisy_BurstLike_Intervals = quality$Noisy_BurstLike_Intervals,
      Noisy_TonicLike_Intervals = quality$Noisy_TonicLike_Intervals,
      Noisy_PauseLike_Intervals = quality$Noisy_PauseLike_Intervals,
      Noisy_Above_TonicUpper_or_PauseGuard_Intervals = quality$Noisy_Above_TonicUpper_or_PauseGuard_Intervals,
      Noisy_NearPause_Intervals = quality$Noisy_NearPause_Intervals,
      Noisy_SameZone_Pair_Violations = quality$Noisy_SameZone_Pair_Violations,
      Noisy_Global_SameZone_Pair_Violations = quality$Noisy_Global_SameZone_Pair_Violations,
      Noisy_Mode_Adjacency_Violations = quality$Noisy_Mode_Adjacency_Violations,
      Noisy_TonicLike_or_TooRegular_Runs = quality$Noisy_TonicLike_or_TooRegular_Runs,
      HF_Burst_Adjacency_Violations = quality$HF_Burst_Adjacency_Violations,
      Min_Burst_Boundary_Spikes = quality$Min_Burst_Boundary_Spikes,
      Min_Tonic_Boundary_Spikes = quality$Min_Tonic_Boundary_Spikes,
      Min_Noisy_Boundary_Spikes = quality$Min_Noisy_Boundary_Spikes,
      Benchmark_Quality_Note = quality$Benchmark_Quality_Note,
      Generation_Key = sim$generation_key,
      Derived_RNG_Seed = sim$seed,
      Train_Count = generated_train_count(sim),
	      Observation_Model = value_or(sim$observation_model, "identity"),
	      Primary_Scoring_Protocol = ifelse(identical(as.character(value_or(sim$benchmark_task_mode, value_or(sim$config$benchmark_task_mode, "clean"))), "realistic_stress"), "observed_time_overlap", "latent_interval"),
		      Detector_Visible_Input_Directory = "detector_inputs/",
		      Ground_Truth_Directory = "ground_truth/",
		      Metadata_Directory = "metadata/",
		      Scoring_Directory = "scoring/",
		      Detector_Visible_Latent_Input = paste0("detector_inputs/benchmark_", difficulty, "_latent_spike_events_input.csv"),
		      Detector_Visible_Observed_Input = paste0("detector_inputs/benchmark_", difficulty, "_observed_spike_events_input.csv"),
		      Detector_Visible_External_Stimulus_Input = paste0("detector_inputs/benchmark_", difficulty, "_external_stimulus_table_input.csv"),
		      Train_Seed_Audit = "metadata/Train_Seeds.csv",
		      Software_Parameters_YAML = "metadata/software_parameters.yaml",
		      Reference_Latent_Interval_Predictions = paste0("predictions/benchmark_", difficulty, "_reference_detector_latent_interval_predictions.csv"),
		      Reference_Observed_Time_Overlap_Predictions = paste0("predictions/benchmark_", difficulty, "_reference_detector_observed_time_overlap_predictions.csv"),
		      Detector_Input_Contract = "Detector data inputs must come only from detector_inputs/ during prediction; all ground_truth/ files are withheld until scoring.",
		      Public_Task_Instructions_Contract = "README_task.md, prediction templates, and scoring protocol descriptions may be read for format information, but must not be used as prediction data.",
		      External_Stimulus_Strength_Field = "External_Strength",
		      External_Stimulus_Strength_Note = "External_Strength is unmodulated external stimulus strength. Unit-modulated Strength is scoring-only and remains in ground_truth/*_stimulus_table_audit.csv.",
		      External_Stimulus_Input_Granularity = "per_train_duplicate_for_detector_convenience",
		      External_Stimulus_Input_Granularity_Note = "Repeated Train rows contain only externally defined stimulus attributes and no unit-specific drive, tuning, response, or audit fields.",
		      External_Stimulus_Input_Strict_Contract = "External_Strength is required for non-empty detector-visible external stimulus input; fallback from Strength is refused to prevent unit-specific leakage.",
		      N_Missing_External_Strength_Input = n_missing_external_strength_input,
		      External_Strength_Input_Audit_Pass = external_strength_input_audit_pass,
		      N_Fallback_External_Strength = n_external_strength_fallback,
		      N_Missing_External_Strength_Source = n_external_strength_missing,
		      External_Strength_Source_Warning = external_strength_source_warning,
		      Benchmark_Task_Format = "dual_protocol_latent_interval_and_observed_time_overlap",
		      Latent_Interval_Scoring = "scoring/score_latent_interval.R",
		      Observed_Time_Overlap_Scoring = "scoring/score_observed_time_overlap.R",
		      Latent_Interval_Prediction_Key = "Train + Interval_ID",
		      Observed_Time_Overlap_IoU_Thresholds = "0.10;0.30;0.50",
		      Observed_Time_Overlap_Primary_Min_IoU = 0.30,
		      Observed_Time_Overlap_Default_Matching = "greedy_descending_iou_by_label_and_train",
		      Observed_Time_Overlap_Optional_Matching = "optimal_lsap_clue_or_exact_base_r_by_label_and_train",
		      Feature_Response_Eligible_Schema_Note = "Deprecated compatibility alias of Response_Eligible; not a synonym for Feature_Matched.",
		      Feature_Drive_Schema_Note = "Feature_Drive is modulated unit-specific drive; raw tuning components are Feature_Excitation and Feature_Suppression.",
		      Deprecated_NoOp_Config_Fields = "feature_neutral_response_probability;feature_weak_response_probability",
		      Verification_Code = sim$verification_code,
	      Verification_Hash = sim$verification_hash,
	      stringsAsFactors = FALSE
	    )

    rel_files <- c(
      latent_spike_events_input = file.path("detector_inputs", paste0("benchmark_", difficulty, "_latent_spike_events_input.csv")),
      observed_spike_events_input = file.path("detector_inputs", paste0("benchmark_", difficulty, "_observed_spike_events_input.csv")),
      external_stimulus_input = file.path("detector_inputs", paste0("benchmark_", difficulty, "_external_stimulus_table_input.csv")),
      spike_events_audit = file.path("ground_truth", paste0("benchmark_", difficulty, "_spike_events_audit.csv")),
      latent_spike_events_audit = file.path("ground_truth", paste0("benchmark_", difficulty, "_latent_spike_events_audit.csv")),
      observed_spike_events_audit = file.path("ground_truth", paste0("benchmark_", difficulty, "_observed_spike_events_audit.csv")),
      observation_map = file.path("ground_truth", paste0("benchmark_", difficulty, "_observation_map.csv")),
      intervals = file.path("ground_truth", paste0("benchmark_", difficulty, "_interval_table.csv")),
      episodes = file.path("ground_truth", paste0("benchmark_", difficulty, "_episodes.csv")),
      event_epochs = file.path("ground_truth", paste0("benchmark_", difficulty, "_event_epoch_table.csv")),
      units = file.path("ground_truth", paste0("benchmark_", difficulty, "_unit_tuning_table.csv")),
      unit_stimulus_drive = file.path("ground_truth", paste0("benchmark_", difficulty, "_unit_stimulus_drive_table.csv")),
      stimuli_audit = file.path("ground_truth", paste0("benchmark_", difficulty, "_stimulus_table_audit.csv")),
      responses = file.path("ground_truth", paste0("benchmark_", difficulty, "_stimulus_response_table.csv")),
	      manifest = file.path("metadata", paste0("benchmark_", difficulty, "_manifest.csv")),
	      reproduction = file.path("metadata", paste0("benchmark_", difficulty, "_reproduction_code.txt")),
	      train_seeds = file.path("metadata", "Train_Seeds.csv"),
	      software_parameters = file.path("metadata", "software_parameters.yaml"),
	      nwb_mapping = file.path("metadata", "nwb_mapping.json"),
	      model_specification = file.path("metadata", "model_specification.md"),
	      schema_dictionary = file.path("metadata", "schema_dictionary.csv"),
	      reference_latent_predictions = file.path("predictions", paste0("benchmark_", difficulty, "_reference_detector_latent_interval_predictions.csv")),
	      reference_observed_predictions = file.path("predictions", paste0("benchmark_", difficulty, "_reference_detector_observed_time_overlap_predictions.csv")),
	      readme = "README_task.md",
	      latent_interval_template = file.path("scoring", "detector_predictions_latent_interval_template.csv"),
	      observed_time_overlap_template = file.path("scoring", "detector_predictions_observed_time_overlap_template.csv"),
	      latent_interval_scoring = file.path("scoring", "score_latent_interval.R"),
	      observed_time_overlap_scoring = file.path("scoring", "score_observed_time_overlap.R")
	    )
    files <- file.path(temp_dir, rel_files)
    names(files) <- names(rel_files)
    invisible(vapply(unique(dirname(files)), dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))

    utils::write.csv(latent_spike_events_input, files[["latent_spike_events_input"]], row.names = FALSE)
    utils::write.csv(observed_spike_events_input, files[["observed_spike_events_input"]], row.names = FALSE)
    utils::write.csv(external_stimulus_input, files[["external_stimulus_input"]], row.names = FALSE)
    utils::write.csv(spike_events, files[["spike_events_audit"]], row.names = FALSE)
    utils::write.csv(latent_spike_events, files[["latent_spike_events_audit"]], row.names = FALSE)
    utils::write.csv(observed_spike_events, files[["observed_spike_events_audit"]], row.names = FALSE)
    utils::write.csv(observation_map, files[["observation_map"]], row.names = FALSE)
    utils::write.csv(interval_details, files[["intervals"]], row.names = FALSE)
    utils::write.csv(episode_data, files[["episodes"]], row.names = FALSE)
    utils::write.csv(event_epoch_data, files[["event_epochs"]], row.names = FALSE)
    utils::write.csv(unit_data, files[["units"]], row.names = FALSE)
    utils::write.csv(unit_drive_data, files[["unit_stimulus_drive"]], row.names = FALSE)
    utils::write.csv(stimulus_data, files[["stimuli_audit"]], row.names = FALSE)
    utils::write.csv(response_data, files[["responses"]], row.names = FALSE)
    utils::write.csv(manifest, files[["manifest"]], row.names = FALSE)
    utils::write.csv(train_seed_data, files[["train_seeds"]], row.names = FALSE)
    write_software_parameters_yaml(
      software_parameters_payload(sim, difficulty, package_type = package_type),
      files[["software_parameters"]]
    )
    utils::write.csv(latent_reference_predictions, files[["reference_latent_predictions"]], row.names = FALSE)
    utils::write.csv(observed_reference_predictions, files[["reference_observed_predictions"]], row.names = FALSE)
    writeLines(value_or(sim$reproduction_code, ""), files[["reproduction"]])
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(nwb_mapping_payload(sim), files[["nwb_mapping"]], auto_unbox = TRUE, pretty = TRUE, null = "null")
    } else {
      writeLines("{}", files[["nwb_mapping"]])
	    }
	    if (file.exists(file.path("docs", "model_specification.md"))) {
	      file.copy(file.path("docs", "model_specification.md"), files[["model_specification"]], overwrite = TRUE)
	    } else {
	      writeLines(benchmark_model_specification_text(), files[["model_specification"]])
	    }
	    if (file.exists(file.path("docs", "schema_dictionary.csv"))) {
	      file.copy(file.path("docs", "schema_dictionary.csv"), files[["schema_dictionary"]], overwrite = TRUE)
	    } else {
	      utils::write.csv(benchmark_schema_dictionary_df(), files[["schema_dictionary"]], row.names = FALSE)
	    }
	    benchmark_mode <- as.character(value_or(sim$benchmark_task_mode, value_or(sim$config$benchmark_task_mode, "clean")))
	    observation_profile <- as.character(value_or(sim$benchmark_observation_profile, value_or(sim$config$benchmark_observation_profile, "identity_observation_clean_labels")))
	    writeLines(benchmark_task_readme_text(difficulty, benchmark_mode, observation_profile, package_type), files[["readme"]])
		    utils::write.csv(data.frame(Train = integer(0), Interval_ID = integer(0), Pred_Label = character(0)), files[["latent_interval_template"]], row.names = FALSE)
	    utils::write.csv(data.frame(Train = integer(0), Pred_Start_s = numeric(0), Pred_End_s = numeric(0), Pred_Label = character(0)), files[["observed_time_overlap_template"]], row.names = FALSE)
	    writeLines(latent_interval_scoring_script_text(), files[["latent_interval_scoring"]])
	    writeLines(observed_time_overlap_scoring_script_text(), files[["observed_time_overlap_scoring"]])

    prediction_file_names <- c(
      "latent_spike_events_input",
      "observed_spike_events_input",
      "external_stimulus_input",
      "readme",
      "model_specification",
      "schema_dictionary",
      "nwb_mapping",
      "latent_interval_template",
      "observed_time_overlap_template",
      "latent_interval_scoring",
      "observed_time_overlap_scoring"
    )
    scoring_file_names <- setdiff(names(rel_files), c("latent_spike_events_input", "observed_spike_events_input", "external_stimulus_input"))
    included_names <- switch(
      package_type,
      prediction = prediction_file_names,
      scoring = scoring_file_names,
      names(rel_files)
    )
    included_names <- intersect(included_names, names(rel_files))
    included_rel_files <- unname(rel_files[included_names])

    old_wd <- getwd()
    setwd(temp_dir)
    on.exit(setwd(old_wd), add = TRUE)
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::zipr(file, included_rel_files, root = temp_dir, include_directories = FALSE, mode = "mirror")
    } else {
      utils::zip(file, included_rel_files, flags = "-q")
    }
  }

  output$downloadBenchmarkDataset <- downloadHandler(
	    filename = function() {
	      difficulty <- selected_benchmark_preset()
	      if (!valid_benchmark_preset(difficulty)) difficulty <- "custom"
	      mode <- selected_benchmark_task_mode()
	      package_type <- tolower(as.character(input_value("benchmark_package_type", "complete"))[1])
	      if (!package_type %in% c("complete", "prediction", "scoring")) package_type <- "complete"
	      paste0("SPIKE_TRAIN_SIMULATOR_V13_5_0_benchmark_", difficulty, "_", mode, "_", package_type, "_package.zip")
	    },
    content = function(file) {
      difficulty <- selected_benchmark_preset()
      if (!valid_benchmark_preset(difficulty)) {
        stop("Choose Easy, Moderate, or Hard before downloading a benchmark dataset.", call. = FALSE)
      }
      sim <- benchmark_dataset_for_difficulty(difficulty)
      package_type <- tolower(as.character(input_value("benchmark_package_type", "complete"))[1])
      if (!package_type %in% c("complete", "prediction", "scoring")) package_type <- "complete"
      write_benchmark_dataset_zip(sim, difficulty, file, package_type = package_type)
    }
  )

  empirical_interval_scopes <- function() {
    c("within_episode", "pause_isi")
  }

  get_empirical_intervals <- function(spikes, episodes, intervals = NULL) {
    out <- data.frame(Train = integer(0), Episode = integer(0), Interval = numeric(0), Pattern = character(0), stringsAsFactors = FALSE)

    if (is.null(intervals)) intervals <- build_interval_table(spikes, episodes)
    if (nrow(intervals) > 0) {
      isi_rows <- intervals[
        is.finite(intervals$ISI_s) &
          intervals$ISI_s > 0 &
          intervals$ISI_Label %in% pattern_levels &
          intervals$ISI_Scope %in% empirical_interval_scopes(),
        ,
        drop = FALSE
      ]
      if (nrow(isi_rows) > 0) {
        out <- data.frame(
          Train = isi_rows$Train,
          Episode = isi_rows$Episode,
          Interval = isi_rows$ISI_s,
          Pattern = isi_rows$ISI_Label,
          stringsAsFactors = FALSE
        )
      }
    }

    out
  }

  get_train_stats <- function(spike_train, episode_data, interval_data = NULL, train_num) {
    real_spike_train <- real_spike_rows(spike_train)
    n_spikes <- nrow(real_spike_train)
    total_time <- input$total_time
    achieved_duration <- achieved_duration_for_train(spike_train, episode_data, total_time)
    duration_shortfall <- if (is.finite(total_time) && total_time > 0) max(0, total_time - achieved_duration) else NA_real_
    duration_completion <- if (is.finite(total_time) && total_time > 0) 100 * achieved_duration / total_time else NA_real_
    isi_all <- if (n_spikes > 1) diff(sort(real_spike_train$Time)) else numeric(0)
    cv_all <- if (length(isi_all) > 1 && mean(isi_all) > 0) sd(isi_all) / mean(isi_all) else NA_real_
    mean_isi_all <- if (length(isi_all) > 0) mean(isi_all) else NA_real_
    interval_rows <- if (!is.null(interval_data)) interval_data else build_interval_table(spike_train, episode_data)
    within_isi_all <- numeric(0)
    if (nrow(interval_rows) > 0 && "ISI_Scope" %in% names(interval_rows)) {
      within_isi_all <- interval_rows$ISI_s[
        is.finite(interval_rows$ISI_s) &
          interval_rows$ISI_s > 0 &
          interval_rows$ISI_Scope == "within_episode"
      ]
    }
    mean_within_isi <- if (length(within_isi_all) > 0) mean(within_isi_all) else NA_real_
    cv_within_isi <- if (length(within_isi_all) > 1 && mean(within_isi_all) > 0) {
      sd(within_isi_all) / mean(within_isi_all)
    } else {
      NA_real_
    }
    isi_counts <- setNames(rep(0, length(pattern_levels)), pattern_levels)
    if (nrow(interval_rows) > 0) {
      count_mask <- interval_rows$ISI_Label %in% pattern_levels &
        interval_rows$ISI_Scope %in% empirical_interval_scopes()
      tmp <- table(interval_rows$ISI_Label[count_mask])
      isi_counts[names(tmp)] <- as.integer(tmp)
    }

    episode_counts <- setNames(rep(0, length(pattern_levels)), pattern_levels)
    time_by_pattern <- setNames(rep(0, length(pattern_levels)), pattern_levels)
    latency_data <- if (nrow(episode_data) > 0 && "Episode_Scope" %in% names(episode_data)) {
      episode_data[episode_data$Episode_Scope %in% c("leading_latency", "initial_latency"), , drop = FALSE]
    } else {
      episode_data[0, , drop = FALSE]
    }
    if (nrow(episode_data) > 0) {
      pattern_episode_data <- episode_data
      if ("Episode_Scope" %in% names(pattern_episode_data)) {
        pattern_episode_data <- pattern_episode_data[pattern_episode_data$Episode_Scope == "interval_run", , drop = FALSE]
      }
      pattern_episode_data <- pattern_episode_data[pattern_episode_data$Pattern %in% pattern_levels, , drop = FALSE]
      if (nrow(pattern_episode_data) > 0) {
        tmp_ep <- table(pattern_episode_data$Pattern)
        episode_counts[names(tmp_ep)] <- as.integer(tmp_ep)
        tmp_time <- tapply(pattern_episode_data$Episode_Duration, pattern_episode_data$Pattern, sum)
        time_by_pattern[names(tmp_time)] <- as.numeric(tmp_time)
      }
    }
    latency_time <- if (nrow(latency_data) > 0) sum(latency_data$Episode_Duration, na.rm = TRUE) else 0
    leading_latency_data <- if (nrow(latency_data) > 0 && "Episode_Scope" %in% names(latency_data)) {
      latency_data[latency_data$Episode_Scope == "leading_latency", , drop = FALSE]
    } else {
      latency_data[0, , drop = FALSE]
    }
    initial_latency_data <- if (nrow(latency_data) > 0 && "Episode_Scope" %in% names(latency_data)) {
      latency_data[latency_data$Episode_Scope == "initial_latency", , drop = FALSE]
    } else {
      latency_data[0, , drop = FALSE]
    }
    leading_latency_time <- if (nrow(leading_latency_data) > 0) sum(leading_latency_data$Episode_Duration, na.rm = TRUE) else 0
    initial_latency_time <- if (nrow(initial_latency_data) > 0) sum(initial_latency_data$Episode_Duration, na.rm = TRUE) else 0

    data.frame(
      Train = train_num,
      Total_Spikes = n_spikes,
      Requested_Duration_s = round(total_time, 4),
      Achieved_Duration_s = round(achieved_duration, 4),
      Duration_Shortfall_s = round(duration_shortfall, 4),
      Duration_Completion_pct = round(duration_completion, 2),
      Global_Mean_Rate_Hz = round(n_spikes / total_time, 3),
      Global_Mean_ISI_s = round(mean_isi_all, 4),
      Global_CV_ISI = round(cv_all, 3),
      Mean_Within_Episode_ISI_s = round(mean_within_isi, 4),
      Within_Episode_CV_ISI = round(cv_within_isi, 3),
      Burst_Episodes = unname(episode_counts["Burst"]),
      Pause_Episodes = unname(episode_counts["Pause"]),
      Tonic_Episodes = unname(episode_counts["Tonic"]),
      HFT_Episodes = unname(episode_counts["high_frequency_tonic"]),
      HFS_Episodes = unname(episode_counts["high_frequency_spiking"]),
      Noisy_Episodes = unname(episode_counts["Noisy"]),
      Burst_ISIs = unname(isi_counts["Burst"]),
      Pause_ISIs = unname(isi_counts["Pause"]),
      Tonic_ISIs = unname(isi_counts["Tonic"]),
      HFT_ISIs = unname(isi_counts["high_frequency_tonic"]),
      HFS_ISIs = unname(isi_counts["high_frequency_spiking"]),
      Noisy_ISIs = unname(isi_counts["Noisy"]),
      Latency_Episodes = nrow(latency_data),
      Leading_Latency_Count = nrow(leading_latency_data),
      Initial_Latency_Count = nrow(initial_latency_data),
      Latency_Time_s = round(latency_time, 4),
      Leading_Latency_Time_s = round(leading_latency_time, 4),
      Initial_Latency_Time_s = round(initial_latency_time, 4),
      Burst_Actual_Time_pct = round(100 * time_by_pattern["Burst"] / total_time, 1),
      Pause_Actual_Time_pct = round(100 * time_by_pattern["Pause"] / total_time, 1),
      Tonic_Actual_Time_pct = round(100 * time_by_pattern["Tonic"] / total_time, 1),
      HFT_Actual_Time_pct = round(100 * time_by_pattern["high_frequency_tonic"] / total_time, 1),
      HFS_Actual_Time_pct = round(100 * time_by_pattern["high_frequency_spiking"] / total_time, 1),
      Noisy_Actual_Time_pct = round(100 * time_by_pattern["Noisy"] / total_time, 1),
      stringsAsFactors = FALSE
    )
  }

  output$param_summary <- renderUI({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    stats_df <- do.call(rbind, lapply(seq_along(sim$spikes_list), function(i) {
      get_train_stats(sim$spikes_list[[i]], sim$episodes_list[[i]], sim$intervals_list[[i]], i)
    }))
    duration_df <- duration_summary_df(sim, requested_duration = input$total_time, digits = 4)

    make_html_table <- function(df) {
      paste0(
        "<table class='table table-condensed table-striped'><tr>",
        paste(paste0("<th>", htmltools::htmlEscape(names(df)), "</th>"), collapse = ""),
        "</tr>",
        paste(apply(df, 1, function(row) {
          paste0("<tr>", paste(paste0("<td>", htmltools::htmlEscape(row), "</td>"), collapse = ""), "</tr>")
        }), collapse = ""),
        "</table>"
      )
    }

    ratio_mode <- ifelse(input$generation_mode == "time", tr(lang, "mode_time"), tr(lang, "mode_event"))
    labels <- pattern_labels(lang)
    ratio_txt <- paste(
      paste0(labels[names(ratio_vector())], "=", round(100 * ratio_vector(), 1), "%"),
      collapse = "; "
    )

    generated_n <- generated_train_count(sim)
    requested_n <- current_train_count()
    generation_key_text <- value_or(sim$generation_key, "NA")
    seed_text <- if (!is.null(sim$seed) && is.finite(sim$seed)) as.character(sim$seed) else "NA"
    verification_code_text <- value_or(sim$verification_code, "NA")
    verification_hash_text <- value_or(sim$verification_hash, "NA")
    reproduction_code_text <- value_or(sim$reproduction_code, "")
    expected <- loaded_reproduction_expected()
    reproduction_match_html <- ""
    if (!is.null(expected) && !is.null(expected$verification_hash)) {
      is_match <- identical(as.character(expected$verification_hash), as.character(sim$verification_hash))
      reproduction_match_html <- paste0(
        "<div class='alert ", if (is_match) "alert-success" else "alert-warning", "'>",
        htmltools::htmlEscape(tr(lang, if (is_match) "reproduction_match" else "reproduction_mismatch")),
        "</div>"
      )
    }
    train_count_html <- paste0(
      "<p><b>", htmltools::htmlEscape(tr(lang, "generated_train_count")), ":</b> ",
      htmltools::htmlEscape(generated_n),
      " <span class='muted-note'>(",
      htmltools::htmlEscape(tr(lang, "current_train_count")), ": ",
      htmltools::htmlEscape(requested_n),
      ")</span></p>"
    )
    stale_train_count_html <- ""
    if (!train_count_matches_current_input(sim)) {
      stale_train_count_html <- paste0(
        "<div class='alert alert-warning'>",
        htmltools::htmlEscape(tr(lang, "stale_train_count")),
        "</div>"
      )
    }
    duration_display <- duration_df
    names(duration_display) <- duration_col_labels(lang, names(duration_display))
    duration_shortfall_html <- ""
    if (any(duration_df$Duration_Shortfall_s > 1e-9, na.rm = TRUE)) {
      duration_shortfall_html <- paste0(
        "<div class='alert alert-warning'>",
        htmltools::htmlEscape(tr(lang, "duration_shortfall_warning")),
        "</div>"
      )
    }
    duration_html <- paste0(
      "<h4>", htmltools::htmlEscape(tr(lang, "duration_check")), "</h4>",
      duration_shortfall_html,
      make_html_table(duration_display)
    )
    reproducibility_html <- paste0(
      "<div class='resolution-note'>",
      "<div><b>", htmltools::htmlEscape(tr(lang, "reproducibility")), "</b></div>",
      "<div>", htmltools::htmlEscape(tr(lang, "reproducibility_seed")), ": <code>",
      htmltools::htmlEscape(generation_key_text), "</code></div>",
      "<div>", htmltools::htmlEscape(tr(lang, "derived_rng_seed")), ": <code>",
      htmltools::htmlEscape(seed_text), "</code></div>",
      "<div>", htmltools::htmlEscape(tr(lang, "verification_code")), ": <code>",
      htmltools::htmlEscape(verification_code_text), "</code></div>",
      "<div class='muted-note' style='word-break: break-all;'>",
      htmltools::htmlEscape(tr(lang, "verification_hash")), ": <code>",
      htmltools::htmlEscape(verification_hash_text), "</code></div>",
      "<div class='muted-note'>", htmltools::htmlEscape(tr(lang, "verification_hint")), "</div>",
      reproduction_match_html,
      "<div><b>", htmltools::htmlEscape(tr(lang, "reproduction_code_label")), "</b></div>",
      "<div class='muted-note'>", htmltools::htmlEscape(tr(lang, "reproduction_code_summary")), "</div>",
      "<textarea class='reproduction-code-box' readonly>",
      htmltools::htmlEscape(reproduction_code_text),
      "</textarea>",
      "</div>"
    )

    target_actual_html <- ""
    if (input$generation_mode == "time") {
      actual_cols <- c(
        "Burst" = "Burst_Actual_Time_pct",
        "Pause" = "Pause_Actual_Time_pct",
        "Tonic" = "Tonic_Actual_Time_pct",
        "high_frequency_tonic" = "HFT_Actual_Time_pct",
        "high_frequency_spiking" = "HFS_Actual_Time_pct",
        "Noisy" = "Noisy_Actual_Time_pct"
      )
      target_actual_df <- do.call(rbind, lapply(seq_len(nrow(stats_df)), function(i) {
        actual <- as.numeric(unlist(stats_df[i, actual_cols], use.names = FALSE))
        data.frame(
          Train = stats_df$Train[i],
          Pattern = unname(labels[pattern_levels]),
          Target_Time_pct = round(100 * sim$config$ratios[pattern_levels], 1),
          Actual_Time_pct = actual,
          Delta_pct = round(actual - 100 * sim$config$ratios[pattern_levels], 1),
          stringsAsFactors = FALSE
        )
      }))
      names(target_actual_df) <- target_col_labels(lang, names(target_actual_df))
      target_actual_html <- paste0(
        "<h4>", htmltools::htmlEscape(tr(lang, "target_actual")), "</h4>",
        make_html_table(target_actual_df)
      )
    }

    warnings_html <- ""
    if (length(sim$warnings) > 0) {
      diagnostic_messages <- translate_diagnostic_messages(lang, sim$warnings)
      warnings_html <- paste0(
        "<div class='alert alert-warning'><b>", htmltools::htmlEscape(tr(lang, "diagnostics")), ":</b><ul>",
        paste(paste0("<li>", htmltools::htmlEscape(diagnostic_messages), "</li>"), collapse = ""),
        "</ul></div>"
      )
    }

    stats_display <- stats_df
    names(stats_display) <- summary_col_labels(lang, names(stats_display))

    HTML(paste0(
      train_count_html,
      stale_train_count_html,
      reproducibility_html,
      "<p><b>", htmltools::htmlEscape(tr(lang, "ratio_interpretation")), ":</b> ", htmltools::htmlEscape(ratio_mode), "</p>",
      "<p><b>", htmltools::htmlEscape(tr(lang, "normalized_ratios")), ":</b> ", htmltools::htmlEscape(ratio_txt), "</p>",
      "<p><b>Pause:</b> ", htmltools::htmlEscape(tr(lang, "pause_model")), "</p>",
      "<p><b>", htmltools::htmlEscape(if (identical(lang, "zh")) "模型范围" else "Model scope"), ":</b> ", htmltools::htmlEscape(tr(lang, "model_scope")), "</p>",
      duration_html,
      warnings_html,
      target_actual_html,
      make_html_table(stats_display)
    ))
  })

  make_spike_plot <- function(train_spikes, train_episodes, train_ids, lang_override = NULL, window_override = NULL, train_intervals = NULL, train_stimuli = NULL) {
    lang <- if (!is.null(lang_override)) lang_override else current_lang()
    labels <- pattern_labels(lang)
    window <- if (!is.null(window_override)) sort(as.numeric(window_override)) else spike_time_window()
    if (length(window) != 2 || any(!is.finite(window)) || diff(window) <= 0) {
      window <- spike_time_window()
    }
    train_ids <- sort(unique(as.integer(train_ids)))
    train_ids <- head(train_ids[is.finite(train_ids)], 10L)
    if (length(train_ids) == 0) return(NULL)

    plot_train_label <- function(train_id) {
      labels_found <- character(0)
      if (!is.null(train_spikes) && nrow(train_spikes) > 0 && all(c("Train", "Train_Label") %in% names(train_spikes))) {
        labels_found <- c(labels_found, as.character(train_spikes$Train_Label[as.integer(train_spikes$Train) == train_id]))
      }
      if (!is.null(train_episodes) && nrow(train_episodes) > 0 && all(c("Train", "Train_Label") %in% names(train_episodes))) {
        labels_found <- c(labels_found, as.character(train_episodes$Train_Label[as.integer(train_episodes$Train) == train_id]))
      }
      labels_found <- labels_found[!is.na(labels_found) & nzchar(trimws(labels_found))]
      if (length(labels_found) > 0) return(labels_found[1])
      train_label(lang, train_id)
    }

    y_map <- data.frame(
      Train = train_ids,
      y = rev(seq_along(train_ids)),
      Label = vapply(train_ids, plot_train_label, character(1)),
      stringsAsFactors = FALSE
    )
    train_spikes <- merge(train_spikes, y_map, by = "Train", all.x = FALSE)
    raw_train_spikes <- train_spikes
    train_spikes <- real_spike_rows(train_spikes)

    interval_segments <- data.frame(
      Train = integer(0),
      y = numeric(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      Pattern = character(0),
      stringsAsFactors = FALSE
    )
    latency_segments <- data.frame(
      Train = integer(0),
      y = numeric(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      stringsAsFactors = FALSE
    )
    timing_segments <- data.frame(
      Train = integer(0),
      y = numeric(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      Timing_Label = character(0),
      stringsAsFactors = FALSE
    )
    raw_intervals <- if (!is.null(train_intervals)) train_intervals else build_interval_table(raw_train_spikes, train_episodes)
    if (nrow(raw_intervals) > 0) {
      latency_segments <- plot_interval_runs(raw_intervals, "Latency", window)
      if (nrow(latency_segments) > 0) {
        latency_segments <- merge(latency_segments, y_map, by = "Train", all.x = FALSE)
        latency_segments <- latency_segments[latency_segments$Plot_End > latency_segments$Plot_Start, , drop = FALSE]
        latency_segments <- latency_segments[, c("Train", "y", "Plot_Start", "Plot_End"), drop = FALSE]
      }
      timing_segments <- plot_interval_runs(raw_intervals, names(NON_PATTERN_INTERVAL_COLORS), window)
      if (nrow(timing_segments) > 0) {
        timing_segments$Timing_Label <- timing_segments$Segment_Label
        timing_segments <- merge(timing_segments, y_map, by = "Train", all.x = FALSE)
        timing_segments <- timing_segments[timing_segments$Plot_End > timing_segments$Plot_Start, , drop = FALSE]
        timing_segments <- timing_segments[, c("Train", "y", "Plot_Start", "Plot_End", "Timing_Label"), drop = FALSE]
      }
      interval_segments <- plot_interval_runs(raw_intervals, pattern_levels, window)
      if (nrow(interval_segments) > 0) {
        interval_segments$Pattern <- interval_segments$Segment_Label
        interval_segments <- merge(interval_segments, y_map, by = "Train", all.x = FALSE)
        interval_segments <- interval_segments[interval_segments$Plot_End > interval_segments$Plot_Start, , drop = FALSE]
        interval_segments <- interval_segments[, c("Train", "y", "Plot_Start", "Plot_End", "Pattern", "N_Intervals"), drop = FALSE]
      }
    }

    if (nrow(train_spikes) > 0) {
      train_spikes <- train_spikes[
        is.finite(train_spikes$Time) &
          train_spikes$Time >= window[1] &
          train_spikes$Time <= window[2],
        ,
        drop = FALSE
      ]
      if (nrow(train_spikes) > 0) {
        train_spikes <- train_spikes[order(train_spikes$Train, train_spikes$Time), , drop = FALSE]
        train_spikes <- train_spikes[!duplicated(train_spikes[c("Train", "Time")]), , drop = FALSE]
        spike_width <- diff(window) / 600
        train_spikes$Spike_Xmin <- pmax(train_spikes$Time - spike_width / 2, window[1])
        train_spikes$Spike_Xmax <- pmin(train_spikes$Time + spike_width / 2, window[2])
      }
    }
    p <- ggplot()
    if (nrow(latency_segments) > 0) {
      p <- p + geom_segment(
        data = latency_segments,
        aes(x = Plot_Start, xend = Plot_End, y = y, yend = y),
        color = LATENCY_INTERVAL_COLOR,
        linewidth = 1.35,
        linetype = "longdash",
        lineend = "butt",
        alpha = 0.95,
        inherit.aes = FALSE
      )
    }
    if (nrow(timing_segments) > 0) {
      p <- p + geom_segment(
        data = timing_segments,
        aes(x = Plot_Start, xend = Plot_End, y = y, yend = y, color = Timing_Label),
        linewidth = 1.2,
        linetype = "longdash",
        lineend = "butt",
        alpha = 0.85,
        inherit.aes = FALSE
      )
    }
    if (nrow(interval_segments) > 0) {
      p <- p + geom_segment(
        data = interval_segments,
        aes(x = Plot_Start, xend = Plot_End, y = y, yend = y, color = Pattern),
        linewidth = 2.1,
        lineend = "butt"
      )
    }
    if (nrow(train_spikes) > 0) {
      p <- p + geom_rect(
        data = train_spikes,
        aes(xmin = Spike_Xmin, xmax = Spike_Xmax, ymin = y - 0.33, ymax = y + 0.33),
        fill = "#000000",
        color = NA,
        alpha = 1,
        inherit.aes = FALSE
      )
    }

    if (!is.null(train_stimuli) && nrow(train_stimuli) > 0 && "Onset_s" %in% names(train_stimuli)) {
      stim_rows <- train_stimuli[is.finite(train_stimuli$Onset_s) & train_stimuli$Onset_s >= window[1] & train_stimuli$Onset_s <= window[2], , drop = FALSE]
      if (nrow(stim_rows) > 0) {
        p <- p + geom_vline(data = stim_rows, aes(xintercept = Onset_s), color = "#7C3AED", linewidth = 0.25, linetype = "dashed", alpha = 0.8, inherit.aes = FALSE)
      }
    }

    timing_labels <- c(
      labels,
      Interburst_Gap = if (identical(lang, "zh")) "Burst 间隔" else "Interburst gap",
      Stimulus_Gap = if (identical(lang, "zh")) "跨刺激间隔" else "Stimulus-spanning gap"
    )

    p +
      scale_x_continuous(limits = window, expand = expansion(mult = 0.01)) +
      scale_y_continuous(
        limits = c(0.35, length(train_ids) + 0.65),
        breaks = y_map$y,
        labels = y_map$Label,
        expand = expansion(mult = 0.02)
      ) +
      scale_color_manual(values = c(spike_colors(), NON_PATTERN_INTERVAL_COLORS), labels = timing_labels, drop = FALSE) +
      labs(x = tr(lang, "x_time"), y = "", color = tr(lang, "legend_pattern"),
           title = tr(lang, "plot_spike_title")) +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang)) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top"
      )
  }

  make_spike_svg <- function(train_spikes, train_episodes, train_ids, train_intervals = NULL, train_stimuli = NULL) {
    lang <- current_lang()
    labels <- pattern_labels(lang)
    colors <- line_colors()
    train_ids <- sort(unique(as.integer(train_ids)))
    train_ids <- head(train_ids[is.finite(train_ids)], 10L)
    if (length(train_ids) == 0) return("")

    plot_train_label <- function(train_id) {
      labels_found <- character(0)
      if (!is.null(train_spikes) && nrow(train_spikes) > 0 && all(c("Train", "Train_Label") %in% names(train_spikes))) {
        labels_found <- c(labels_found, as.character(train_spikes$Train_Label[as.integer(train_spikes$Train) == train_id]))
      }
      if (!is.null(train_episodes) && nrow(train_episodes) > 0 && all(c("Train", "Train_Label") %in% names(train_episodes))) {
        labels_found <- c(labels_found, as.character(train_episodes$Train_Label[as.integer(train_episodes$Train) == train_id]))
      }
      labels_found <- labels_found[!is.na(labels_found) & nzchar(trimws(labels_found))]
      if (length(labels_found) > 0) return(labels_found[1])
      train_label(lang, train_id)
    }

    max_candidates <- safe_total_time()
    if (!is.null(train_spikes) && nrow(train_spikes) > 0 && "Time" %in% names(train_spikes)) {
      max_candidates <- c(max_candidates, suppressWarnings(max(train_spikes$Time[is.finite(train_spikes$Time)], na.rm = TRUE)))
    }
    if (!is.null(train_intervals) && nrow(train_intervals) > 0 && "End_Time_s" %in% names(train_intervals)) {
      max_candidates <- c(max_candidates, suppressWarnings(max(train_intervals$End_Time_s[is.finite(train_intervals$End_Time_s)], na.rm = TRUE)))
    }
    if (!is.null(train_stimuli) && nrow(train_stimuli) > 0 && "Onset_s" %in% names(train_stimuli)) {
      max_candidates <- c(max_candidates, suppressWarnings(max(train_stimuli$Onset_s[is.finite(train_stimuli$Onset_s)], na.rm = TRUE)))
    }
    total_window_s <- max(max_candidates[is.finite(max_candidates)], 0.001)
    window <- c(0, total_window_s)
    visible_seconds <- spike_visible_seconds(total_window_s)
    plot_left <- 132
    label_x <- plot_left - 18
    plot_margin_right <- 20
    plot_width <- spike_svg_plot_width(total_window_s, visible_seconds)
    svg_width <- plot_left + plot_width + plot_margin_right
    svg_height <- 520
    plot_right <- plot_left + plot_width
    plot_top <- 112
    plot_bottom <- 432
    axis_y <- 470
    window_width <- max(diff(window), .Machine$double.eps)
    spike_stroke_width <- 2
    stroke_clip_pad_px <- max(2, ceiling(spike_stroke_width / 2) + 1)
    data_left <- plot_left + stroke_clip_pad_px
    data_right <- plot_right - stroke_clip_pad_px
    data_width <- max(1, data_right - data_left)

    x_scale <- function(x) {
      data_left + (x - window[1]) / window_width * data_width
    }

    y_values <- if (length(train_ids) == 1) {
      (plot_top + plot_bottom) / 2
    } else {
      seq(plot_top, plot_bottom, length.out = length(train_ids))
    }
    y_map <- data.frame(
      Train = train_ids,
      y_px = y_values,
      Label = vapply(train_ids, plot_train_label, character(1)),
      stringsAsFactors = FALSE
    )
    lane_step <- if (length(y_values) > 1) min(diff(y_values)) else 44
    spike_height <- min(32, max(22, lane_step * 0.68))

    train_spikes <- merge(train_spikes, y_map, by = "Train", all.x = FALSE)
    raw_train_spikes <- train_spikes
    train_spikes <- real_spike_rows(train_spikes)

    interval_segments <- data.frame(
      Train = integer(0),
      y_px = numeric(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      X1 = numeric(0),
      X2 = numeric(0),
      Pattern = character(0),
      stringsAsFactors = FALSE
    )
    latency_segments <- data.frame(
      Train = integer(0),
      y_px = numeric(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      X1 = numeric(0),
      X2 = numeric(0),
      stringsAsFactors = FALSE
    )
    timing_segments <- data.frame(
      Train = integer(0),
      y_px = numeric(0),
      Plot_Start = numeric(0),
      Plot_End = numeric(0),
      X1 = numeric(0),
      X2 = numeric(0),
      Timing_Label = character(0),
      stringsAsFactors = FALSE
    )
    raw_intervals <- if (!is.null(train_intervals)) train_intervals else build_interval_table(raw_train_spikes, train_episodes)
    if (nrow(raw_intervals) > 0) {
      latency_segments <- plot_interval_runs(raw_intervals, "Latency", window)
      if (nrow(latency_segments) > 0) {
        latency_segments$X1 <- x_scale(latency_segments$Plot_Start)
        latency_segments$X2 <- x_scale(latency_segments$Plot_End)
        latency_segments <- merge(latency_segments, y_map, by = "Train", all.x = FALSE)
        latency_segments <- latency_segments[latency_segments$Plot_End > latency_segments$Plot_Start, , drop = FALSE]
        latency_segments <- latency_segments[, c("Train", "y_px", "Plot_Start", "Plot_End", "X1", "X2"), drop = FALSE]
      }
      timing_segments <- plot_interval_runs(raw_intervals, names(NON_PATTERN_INTERVAL_COLORS), window)
      if (nrow(timing_segments) > 0) {
        timing_segments$X1 <- x_scale(timing_segments$Plot_Start)
        timing_segments$X2 <- x_scale(timing_segments$Plot_End)
        timing_segments$Timing_Label <- timing_segments$Segment_Label
        timing_segments <- merge(timing_segments, y_map, by = "Train", all.x = FALSE)
        timing_segments <- timing_segments[timing_segments$Plot_End > timing_segments$Plot_Start, , drop = FALSE]
        timing_segments <- timing_segments[, c("Train", "y_px", "Plot_Start", "Plot_End", "X1", "X2", "Timing_Label"), drop = FALSE]
      }
      interval_segments <- plot_interval_runs(raw_intervals, pattern_levels, window)
      if (nrow(interval_segments) > 0) {
        interval_segments$X1 <- x_scale(interval_segments$Plot_Start)
        interval_segments$X2 <- x_scale(interval_segments$Plot_End)
        interval_segments$Pattern <- interval_segments$Segment_Label
        interval_segments <- merge(interval_segments, y_map, by = "Train", all.x = FALSE)
        interval_segments <- interval_segments[interval_segments$Plot_End > interval_segments$Plot_Start, , drop = FALSE]
        interval_segments <- interval_segments[, c("Train", "y_px", "Plot_Start", "Plot_End", "X1", "X2", "Pattern", "N_Intervals"), drop = FALSE]
      }
    }

    if (nrow(train_spikes) > 0) {
      train_spikes <- train_spikes[
        is.finite(train_spikes$Time) &
          train_spikes$Time >= window[1] &
          train_spikes$Time <= window[2],
        ,
        drop = FALSE
      ]
      if (nrow(train_spikes) > 0) {
        train_spikes <- train_spikes[order(train_spikes$Train, train_spikes$Time), , drop = FALSE]
        train_spikes$Spike_X <- pmin(pmax(x_scale(train_spikes$Time), data_left), data_right)
        train_spikes$Spike_Y1 <- train_spikes$y_px - spike_height / 2
        train_spikes$Spike_Y2 <- train_spikes$y_px + spike_height / 2
      }
    }

    esc <- function(value) htmltools::htmlEscape(as.character(value))
    num <- function(value) format(round(as.numeric(value), 3), trim = TRUE, scientific = FALSE)

    title <- esc(tr(lang, "plot_spike_title"))
    axis_label <- esc(tr(lang, "x_time"))
    aria <- esc(paste(tr(lang, "plot_spike_title"), paste0(window[1], "-", window[2], "s")))

    latency_label <- if (identical(lang, "zh")) "响应潜伏期" else "Response latency"
    event_gap_label <- if (identical(lang, "zh")) "事件间隔" else "Event gap"
    legend_items <- lapply(pattern_levels, function(pat) {
      list(
        label = as.character(labels[pat]),
        color = as.character(colors[pat]),
        stroke_width = 6,
        dash = NA_character_,
        opacity = NA_real_
      )
    })
    legend_items <- c(
      legend_items,
      list(
        list(label = latency_label, color = LATENCY_INTERVAL_COLOR, stroke_width = 4, dash = "7,5", opacity = NA_real_),
        list(label = event_gap_label, color = NON_PATTERN_INTERVAL_COLORS[["Stimulus_Gap"]], stroke_width = 4, dash = "6,5", opacity = 0.85)
      )
    )
    estimate_svg_text_width <- function(text, font_size = 13) {
      chars <- strsplit(as.character(text), "", fixed = FALSE, useBytes = FALSE)[[1]]
      if (length(chars) == 0) return(0)
      sum(vapply(chars, function(ch) {
        if (nchar(ch, type = "bytes") > 1) {
          font_size
        } else if (identical(ch, " ")) {
          font_size * 0.35
        } else if (identical(ch, "-")) {
          font_size * 0.40
        } else if (grepl("[A-Z]", ch)) {
          font_size * 0.62
        } else {
          font_size * 0.53
        }
      }, numeric(1)))
    }
    legend_x <- plot_left
    legend_y <- 60
    legend_row_step <- 28
    legend_swatch_width <- 28
    legend_text_gap <- 38
    legend_item_gap <- 28
    legend_right <- svg_width - plot_margin_right
    legend_cursor_x <- legend_x
    legend_cursor_y <- legend_y
    legend_parts <- character(0)
    for (item in legend_items) {
      item_width <- legend_text_gap + estimate_svg_text_width(item$label) + legend_item_gap
      if (legend_cursor_x > legend_x && legend_cursor_x + item_width > legend_right) {
        legend_cursor_x <- legend_x
        legend_cursor_y <- legend_cursor_y + legend_row_step
      }
      dash_attr <- if (!is.na(item$dash) && nzchar(item$dash)) sprintf(' stroke-dasharray="%s"', esc(item$dash)) else ""
      opacity_attr <- if (is.finite(item$opacity)) sprintf(' opacity="%s"', num(item$opacity)) else ""
      legend_parts <- c(
        legend_parts,
        sprintf(
          '<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="%s" stroke-width="%s" stroke-linecap="butt"%s%s />',
          num(legend_cursor_x), num(legend_cursor_x + legend_swatch_width),
          num(legend_cursor_y), num(legend_cursor_y),
          esc(item$color), num(item$stroke_width), dash_attr, opacity_attr
        ),
        sprintf(
          '<text x="%s" y="%s" font-size="13" fill="#111827">%s</text>',
          num(legend_cursor_x + legend_text_gap), num(legend_cursor_y + 4), esc(item$label)
        )
      )
      legend_cursor_x <- legend_cursor_x + item_width
    }
    y_label_parts <- vapply(seq_len(nrow(y_map)), function(i) {
      sprintf(
        '<text x="%s" y="%s" text-anchor="end" dominant-baseline="middle" font-size="13" fill="#4b5563">%s</text>',
        num(label_x), num(y_map$y_px[i]), esc(y_map$Label[i])
      )
    }, character(1))

    interval_parts <- character(0)
    if (nrow(interval_segments) > 0) {
      interval_parts <- vapply(seq_len(nrow(interval_segments)), function(i) {
        pat <- interval_segments$Pattern[i]
        sprintf(
          '<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="%s" stroke-width="6" stroke-linecap="butt"><title>%s</title></line>',
          num(interval_segments$X1[i]),
          num(interval_segments$X2[i]),
          num(interval_segments$y_px[i]),
          num(interval_segments$y_px[i]),
          esc(colors[pat]),
          esc(sprintf("%s ISI: %.6g-%.6g s", labels[pat], interval_segments$Plot_Start[i], interval_segments$Plot_End[i]))
        )
      }, character(1))
    }

    latency_parts <- character(0)
    if (nrow(latency_segments) > 0) {
      latency_parts <- vapply(seq_len(nrow(latency_segments)), function(i) {
        sprintf(
          '<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="%s" stroke-width="4" stroke-linecap="butt" stroke-dasharray="7,5" opacity="0.95"><title>%s</title></line>',
          num(latency_segments$X1[i]),
          num(latency_segments$X2[i]),
          num(latency_segments$y_px[i]),
          num(latency_segments$y_px[i]),
          esc(LATENCY_INTERVAL_COLOR),
          esc(sprintf("%s: %.6g-%.6g s", latency_label, latency_segments$Plot_Start[i], latency_segments$Plot_End[i]))
        )
      }, character(1))
    }

    timing_label_lookup <- c(
      Interburst_Gap = if (identical(lang, "zh")) "Burst 间隔" else "Interburst gap",
      Stimulus_Gap = if (identical(lang, "zh")) "跨刺激间隔" else "Stimulus-spanning gap"
    )
    timing_parts <- character(0)
    if (nrow(timing_segments) > 0) {
      timing_parts <- vapply(seq_len(nrow(timing_segments)), function(i) {
        timing_label <- as.character(timing_segments$Timing_Label[i])
        timing_text <- as.character(value_or(timing_label_lookup[[timing_label]], timing_label))
        timing_color <- as.character(value_or(NON_PATTERN_INTERVAL_COLORS[[timing_label]], "#B8BCC6"))
        sprintf(
          '<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="%s" stroke-width="4" stroke-linecap="butt" stroke-dasharray="6,5" opacity="0.85"><title>%s</title></line>',
          num(timing_segments$X1[i]),
          num(timing_segments$X2[i]),
          num(timing_segments$y_px[i]),
          num(timing_segments$y_px[i]),
          esc(timing_color),
          esc(sprintf("%s: %.6g-%.6g s", timing_text, timing_segments$Plot_Start[i], timing_segments$Plot_End[i]))
        )
      }, character(1))
    }

    spike_parts <- character(0)
    if (nrow(train_spikes) > 0) {
      spike_parts <- vapply(seq_len(nrow(train_spikes)), function(i) {
        title_text <- sprintf(tr(lang, "spike_single_title"), train_spikes$Time[i])
        sprintf(
          '<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="#000000" stroke-width="%s" stroke-linecap="butt" vector-effect="non-scaling-stroke" shape-rendering="crispEdges"><title>%s</title></line>',
          num(train_spikes$Spike_X[i]),
          num(train_spikes$Spike_X[i]),
          num(train_spikes$Spike_Y1[i]),
          num(train_spikes$Spike_Y2[i]),
          num(spike_stroke_width),
          esc(title_text)
        )
      }, character(1))
    }

    stimulus_parts <- character(0)
    if (!is.null(train_stimuli) && nrow(train_stimuli) > 0 && "Onset_s" %in% names(train_stimuli)) {
      stim_rows <- train_stimuli[is.finite(train_stimuli$Onset_s) & train_stimuli$Onset_s >= window[1] & train_stimuli$Onset_s <= window[2], , drop = FALSE]
      if (nrow(stim_rows) > 0) {
        stimulus_parts <- vapply(seq_len(nrow(stim_rows)), function(i) {
          x <- x_scale(stim_rows$Onset_s[i])
          sprintf('<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="#7C3AED" stroke-width="1.2" stroke-dasharray="5,4" opacity="0.85"><title>Stimulus %s at %.6g s, strength %.3g</title></line>',
                  num(x), num(x), num(plot_top - 35), num(plot_bottom + 35), esc(stim_rows$Stimulus_ID[i]), stim_rows$Onset_s[i], stim_rows$Strength[i])
        }, character(1))
      }
    }

    tick_n <- max(5, min(160, ceiling(window_width / visible_seconds) * 5))
    ticks <- pretty(window, n = tick_n)
    ticks <- ticks[ticks >= window[1] & ticks <= window[2]]
    tick_parts <- vapply(ticks, function(tick) {
      x <- x_scale(tick)
      label <- format(round(tick, 3), trim = TRUE, scientific = FALSE)
      paste0(
        sprintf('<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="#333333" stroke-width="1" />',
                num(x), num(x), num(axis_y), num(axis_y + 7)),
        sprintf('<text x="%s" y="%s" text-anchor="middle" font-size="13" fill="#4b5563">%s</text>',
                num(x), num(axis_y + 24), esc(label))
      )
    }, character(1))
    clip_x <- data_left - stroke_clip_pad_px
    clip_width <- data_width + 2 * stroke_clip_pad_px

    svg <- paste0(
      '<div class="spike-svg-wrap">',
      '<svg class="spike-svg" viewBox="0 0 ', svg_width, ' ', svg_height, '" ',
      'width="', svg_width, '" height="', svg_height, '" ',
      'xmlns="http://www.w3.org/2000/svg" role="img" aria-label="', aria, '" ',
      'style="font-family: ', app_font_stack(), ';">',
      '<rect x="0" y="0" width="', num(svg_width), '" height="520" fill="#ffffff" />',
      '<text x="', num(plot_left), '" y="28" font-size="18" font-weight="700" fill="#111827">', title, '</text>',
      paste(legend_parts, collapse = ""),
      paste(y_label_parts, collapse = ""),
      '<g clip-path="url(#spike-clip)">',
      paste(stimulus_parts, collapse = ""),
      paste(latency_parts, collapse = ""),
      paste(timing_parts, collapse = ""),
      paste(interval_parts, collapse = ""),
      paste(spike_parts, collapse = ""),
      '</g>',
      '<line x1="', num(data_left), '" x2="', num(data_right), '" y1="', num(axis_y), '" y2="', num(axis_y), '" stroke="#333333" stroke-width="1" />',
      paste(tick_parts, collapse = ""),
      '<text x="', num((data_left + data_right) / 2), '" y="510" text-anchor="middle" font-size="14" fill="#333333">', axis_label, '</text>',
      '<defs><clipPath id="spike-clip"><rect x="', num(clip_x), '" y="', num(plot_top - 40), '" width="', num(clip_width), '" height="', num(plot_bottom - plot_top + 80), '" /></clipPath></defs>',
      '</svg>',
      '</div>'
    )

    svg
  }

  output$spike_plot <- renderUI({
    req(all_spike_trains())
    sim <- all_spike_trains()
    n_train <- generated_train_count(sim)
    sel <- selected_train_values(n_train)
    train_spikes <- sim$combined_spikes[sim$combined_spikes$Train %in% sel, ]
    train_episodes <- sim$combined_episodes[sim$combined_episodes$Train %in% sel, ]
    train_intervals <- sim$combined_intervals[sim$combined_intervals$Train %in% sel, ]
    train_stimuli <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli[sim$combined_stimuli$Train %in% sel, , drop = FALSE] else make_empty_stimulus_df()
    validate(need(nrow(train_spikes) > 0 || nrow(train_episodes) > 0,
                  tr(current_lang(), "no_episode")))

    HTML(make_spike_svg(train_spikes, train_episodes, sel, train_intervals = train_intervals, train_stimuli = train_stimuli))
  })

  make_theoretical_plot <- function(train_id = NULL, patterns = NULL, lang_override = NULL, config = NULL) {
    lang <- if (!is.null(lang_override)) lang_override else current_lang()
    dist_lines <- build_theoretical_df(patterns = patterns, config = config)
    if (nrow(dist_lines) == 0) return(NULL)
    title <- tr(lang, "plot_theory_title")
    if (!is.null(train_id)) title <- train_plot_title(lang, title, train_id)

    ggplot(dist_lines, aes(x = x, y = y, color = Pattern)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = distribution_line_colors(), labels = pattern_labels(lang), drop = FALSE) +
      labs(x = tr(lang, "x_interval"), y = tr(lang, "y_density"),
           color = tr(lang, "legend_pattern"), title = title) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
  }

  render_theoretical_distribution_plot <- function(input_id, default_train) {
    renderPlot({
      req(all_spike_trains())
      req(input$isi_xmax)
      sim <- all_spike_trains()
      train_id <- selected_distribution_train(generated_train_count(sim), input_id = input_id, default_train = default_train)
      patterns <- selected_train_patterns(sim, train_id)
      p <- make_theoretical_plot(train_id = train_id, patterns = patterns, config = sim$config)
      validate(need(!is.null(p), tr(current_lang(), "no_theory")))
      p
    })
  }

  output$theoretical_isi_plot <- render_theoretical_distribution_plot("distribution_train", 1L)
  output$theoretical_isi_plot_b <- render_theoretical_distribution_plot("distribution_train_b", 2L)

  make_empirical_plot <- function(spikes, episodes, intervals_table = NULL, train_id = NULL, patterns = NULL, lang_override = NULL, config = NULL) {
    lang <- if (!is.null(lang_override)) lang_override else current_lang()
    intervals <- get_empirical_intervals(spikes, episodes, intervals_table)
    intervals <- intervals[is.finite(intervals$Interval) & intervals$Interval > 0 & intervals$Interval <= input$isi_xmax, ]
    if (nrow(intervals) == 0) return(NULL)
    title <- if (isTRUE(input$show_target_density_overlay)) {
      tr(lang, "plot_empirical_title")
    } else {
      tr(lang, "plot_empirical_no_overlay_title")
    }
    if (!is.null(train_id)) title <- train_plot_title(lang, title, train_id)

    p <- ggplot(intervals, aes(x = Interval, fill = Pattern, color = Pattern)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.22, position = "identity", linewidth = 0.55) +
      scale_fill_manual(values = distribution_fill_colors(), labels = pattern_labels(lang), drop = FALSE) +
      scale_color_manual(values = distribution_line_colors(), labels = pattern_labels(lang), drop = FALSE) +
      labs(x = tr(lang, "x_interval"), y = tr(lang, "y_emp_density"),
           fill = tr(lang, "legend_pattern"), color = tr(lang, "legend_pattern"),
           title = title) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE), color = "none") +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))

    if (isTRUE(input$show_target_density_overlay)) {
      dist_lines <- build_theoretical_df(patterns = patterns, config = config)
      if (nrow(dist_lines) > 0) {
        p <- p + geom_line(
          data = dist_lines,
          aes(x = x, y = y, color = Pattern),
          inherit.aes = FALSE,
          linewidth = 0.9
        )
      }
    }

    p
  }

  render_empirical_distribution_plot <- function(input_id, default_train) {
    renderPlot({
      req(all_spike_trains())
      sim <- all_spike_trains()
      train_id <- selected_distribution_train(generated_train_count(sim), input_id = input_id, default_train = default_train)
      train_spikes <- sim$combined_spikes[sim$combined_spikes$Train == train_id, , drop = FALSE]
      train_episodes <- sim$combined_episodes[sim$combined_episodes$Train == train_id, , drop = FALSE]
      train_intervals <- sim$combined_intervals[sim$combined_intervals$Train == train_id, , drop = FALSE]
      patterns <- selected_train_patterns(sim, train_id)
      p <- make_empirical_plot(train_spikes, train_episodes, train_intervals, train_id = train_id, patterns = patterns, config = sim$config)
      validate(need(!is.null(p), tr(current_lang(), "no_empirical")))
      p
    })
  }

  output$empirical_isi_plot <- render_empirical_distribution_plot("distribution_train", 1L)
  output$empirical_isi_plot_b <- render_empirical_distribution_plot("distribution_train_b", 2L)

  print_distribution_pdf_page <- function(theory_plot, empirical_plot) {
    plots <- Filter(Negate(is.null), list(theory_plot, empirical_plot))
    if (length(plots) == 0) return(FALSE)
    grid::grid.newpage()
    layout <- grid::grid.layout(length(plots), 1, heights = grid::unit(rep(1, length(plots)), "null"))
    grid::pushViewport(grid::viewport(layout = layout))
    on.exit(grid::popViewport(), add = TRUE)
    for (i in seq_along(plots)) {
      print(plots[[i]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1))
    }
    TRUE
  }

  output$downloadDistributionPlot <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_isi_distributions_all_trains.pdf"
    },
    content = function(file) {
      sim <- all_spike_trains()
      device_lang <- isolate(current_lang())
      require_current_train_count(sim, device_lang)
      n_train <- generated_train_count(sim)

      device_type <- open_pdf_device(file, width = 11.69, height = 8.27, family = plot_font_family(device_lang))
      on.exit(dev.off(), add = TRUE)
      pdf_lang <- if (identical(device_type, "pdf")) "en" else device_lang

      printed_any <- FALSE
      for (train_id in seq_len(n_train)) {
        train_spikes <- sim$combined_spikes[sim$combined_spikes$Train == train_id, , drop = FALSE]
        train_episodes <- sim$combined_episodes[sim$combined_episodes$Train == train_id, , drop = FALSE]
        train_intervals <- sim$combined_intervals[sim$combined_intervals$Train == train_id, , drop = FALSE]
        patterns <- selected_train_patterns(sim, train_id)
        theory_plot <- make_theoretical_plot(train_id = train_id, patterns = patterns, lang_override = pdf_lang, config = sim$config)
        empirical_plot <- make_empirical_plot(
          train_spikes,
          train_episodes,
          train_intervals,
          train_id = train_id,
          patterns = patterns,
          lang_override = pdf_lang,
          config = sim$config
        )
        printed_any <- print_distribution_pdf_page(theory_plot, empirical_plot) || printed_any
      }

      if (!printed_any) {
        stop(tr(device_lang, "no_empirical"), call. = FALSE)
      }
    }
  )
  table_options <- function(empty_text) {
    list(
      pageLength = 20,
      lengthMenu = c(10, 20, 50, 100),
      autoWidth = TRUE,
      scrollX = TRUE,
      language = list(emptyTable = empty_text)
    )
  }

  parse_train_id_query <- function(query, n_train) {
    n_train <- suppressWarnings(as.integer(n_train[1]))
    if (!is.finite(n_train) || n_train < 1L) return(integer(0))
    query <- paste(as.character(value_or(query, "")), collapse = ",")
    query <- trimws(query)
    if (!nzchar(query)) return(seq_len(n_train))
    tokens <- unlist(strsplit(gsub("[，；;]+", ",", query), ",", fixed = FALSE), use.names = FALSE)
    tokens <- trimws(tokens[nzchar(trimws(tokens))])
    if (length(tokens) == 0) return(seq_len(n_train))
    selected <- integer(0)
    for (token in tokens) {
      if (grepl("^[0-9]+[[:space:]]*[-:][[:space:]]*[0-9]+$", token)) {
        bounds <- suppressWarnings(as.integer(unlist(strsplit(token, "[[:space:]]*[-:][[:space:]]*"), use.names = FALSE)))
        if (length(bounds) == 2 && all(is.finite(bounds))) {
          selected <- c(selected, seq(min(bounds), max(bounds)))
        }
      } else if (grepl("^[0-9]+$", token)) {
        selected <- c(selected, suppressWarnings(as.integer(token)))
      }
    }
    selected <- unique(selected[is.finite(selected) & selected >= 1L & selected <= n_train])
    as.integer(selected)
  }

  selected_spike_data_trains <- function(sim) {
    parse_train_id_query(input_value("spike_data_train_query", ""), generated_train_count(sim))
  }

  filter_table_by_spike_data_trains <- function(tab, sim) {
    if (is.null(tab) || nrow(tab) == 0 || !"Train" %in% names(tab)) return(tab)
    selected <- selected_spike_data_trains(sim)
    tab[as.integer(tab$Train) %in% selected, , drop = FALSE]
  }

  output$spike_data_train_filter_ui <- renderUI({
    req(all_spike_trains())
    lang <- current_lang()
    n_train <- generated_train_count(all_spike_trains())
    label <- if (identical(lang, "zh")) "显示 Spike train 编号" else "Displayed Spike train IDs"
    placeholder <- if (identical(lang, "zh")) "例如 1,3,5-8；留空显示全部" else "e.g. 1,3,5-8; leave blank for all"
    note <- if (identical(lang, "zh")) {
      sprintf("已生成 %d 条 Spike train。表格保留普通搜索框；数值列不再使用范围滑块。", n_train)
    } else {
      sprintf("%d spike trains generated. Tables keep the standard search box; numeric range sliders are disabled.", n_train)
    }
    tags$div(
      class = "table-filter-bar",
      textInput(
        "spike_data_train_query",
        label,
        value = input_value("spike_data_train_query", ""),
        placeholder = placeholder
      ),
      tags$div(class = "muted-note", note)
    )
  })

  translate_observation_values <- function(df, lang) {
    if (!identical(lang, "zh") || is.null(df) || nrow(df) == 0) return(df)
    status_map <- c(
      detected = "已检出",
      missed = "漏检",
      false_positive = "伪阳性",
      clipped_outside_recording = "超出记录窗口被裁剪",
      merged_by_dead_time = "检测死区合并"
    )
    source_map <- c(
      latent_true_spike = "真实 latent spike",
      false_positive = "伪阳性"
    )
    if ("Observation_Status" %in% names(df)) {
      hit <- match(df$Observation_Status, names(status_map))
      df$Observation_Status[!is.na(hit)] <- status_map[hit[!is.na(hit)]]
    }
    if ("Observation_Source" %in% names(df)) {
      hit <- match(df$Observation_Source, names(source_map))
      df$Observation_Source[!is.na(hit)] <- source_map[hit[!is.na(hit)]]
    }
    df
  }

  output$spike_table <- renderDT({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    spike_display <- spike_isi_table(sim$combined_spikes)
    spike_display <- filter_table_by_spike_data_trains(spike_display, sim)
    spike_display <- translate_pattern_values(spike_display, lang)
    datatable(
      spike_display,
      rownames = FALSE,
      colnames = spike_col_labels(lang, names(spike_display)),
      options = table_options(tr(lang, "empty_spike"))
    )
  })

  output$observation_summary_plot <- renderPlot({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    summary <- if (!is.null(sim$observation_summary)) sim$observation_summary else data.frame()
    validate(need(nrow(summary) > 0, if (identical(lang, "zh")) "没有观测噪声摘要。" else "No observation summary."))
    plot_df <- rbind(
      data.frame(Train = summary$Train, Metric = "Detected true", Count = summary$Detected_True_Spikes),
      data.frame(Train = summary$Train, Metric = "Missed true", Count = summary$Missed_True_Spikes),
      data.frame(Train = summary$Train, Metric = "False positive", Count = summary$False_Positive_Spikes),
      data.frame(Train = summary$Train, Metric = "Dead-time merged", Count = summary$DeadTime_Merged_True_Spikes + summary$DeadTime_Merged_False_Positives)
    )
    if (identical(lang, "zh")) {
      plot_df$Metric <- factor(
        plot_df$Metric,
        levels = c("Detected true", "Missed true", "False positive", "Dead-time merged"),
        labels = c("真实 spike 已检出", "真实 spike 漏检", "伪阳性", "死区合并")
      )
    }
    ggplot(plot_df, aes(x = factor(Train), y = Count, fill = Metric)) +
      geom_col(position = "stack", width = 0.72) +
      scale_fill_manual(values = c("#287D6E", "#CB6A27", "#8B5CF6", "#6B7280")) +
      labs(
        x = if (identical(lang, "zh")) "Spike train" else "Spike train",
        y = if (identical(lang, "zh")) "事件数" else "Event count",
        fill = NULL
      ) +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
  })

  output$observation_summary_table <- renderDT({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$observation_summary)) sim$observation_summary else data.frame()
    datatable(
      translate_observation_values(tab, lang),
      rownames = FALSE,
      options = table_options(if (identical(lang, "zh")) "没有观测噪声摘要。" else "No observation summary.")
    )
  })

  output$observed_spike_table <- renderDT({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    tab <- observed_spike_events_table(sim)
    datatable(
      translate_observation_values(tab, lang),
      rownames = FALSE,
      options = table_options(if (identical(lang, "zh")) "没有观测 spike 事件。" else "No observed spike events.")
    )
  })

  output$observation_map_table <- renderDT({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$observation_map)) sim$observation_map else make_empty_observation_map_df()
    datatable(
      translate_observation_values(tab, lang),
      rownames = FALSE,
      options = table_options(if (identical(lang, "zh")) "没有观测映射表。" else "No observation map.")
    )
  })

  output$interval_table <- renderDT({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    interval_display <- filter_table_by_spike_data_trains(sim$combined_intervals, sim)
    interval_display <- translate_pattern_values(interval_display, lang)
    datatable(
      interval_display,
      rownames = FALSE,
      colnames = interval_col_labels(lang, names(interval_display)),
      options = table_options(tr(lang, "no_empirical"))
    )
  })


  output$stimulus_response_summary_plot <- renderPlot({
    req(all_spike_trains())
    sim <- all_spike_trains()
    resp <- if (!is.null(sim$combined_responses)) sim$combined_responses else make_empty_response_df()
    stim <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df()
    validate(need(nrow(resp) > 0 && nrow(stim) > 0, if (identical(current_lang(), "zh")) "没有刺激响应数据。" else "No stimulus-response data."))
    key_cols <- intersect(c("Train", "Stimulus_ID"), intersect(names(resp), names(stim)))
    dat <- if (length(key_cols) == 2) merge(resp, stim, by = key_cols, all.x = TRUE, suffixes = c("", "_stim")) else resp
    if (!"Repetition_Index" %in% names(dat)) dat$Repetition_Index <- seq_len(nrow(dat))
    if (!"Strength" %in% names(dat)) dat$Strength <- NA_real_
    if (!"Evoked_Suppression_Duration_s" %in% names(dat)) dat$Evoked_Suppression_Duration_s <- dat$Evoked_Pause_Duration_s
    metric_df <- rbind(
      data.frame(Train = dat$Train, Repetition_Index = dat$Repetition_Index, Metric = "Evoked burst count", Value = dat$Evoked_Burst_Count, Response_Type = dat$Response_Type),
      data.frame(Train = dat$Train, Repetition_Index = dat$Repetition_Index, Metric = "Evoked suppression duration", Value = dat$Evoked_Suppression_Duration_s, Response_Type = dat$Response_Type),
      data.frame(Train = dat$Train, Repetition_Index = dat$Repetition_Index, Metric = "Response gain", Value = dat$Response_Gain, Response_Type = dat$Response_Type)
    )
    metric_df <- metric_df[is.finite(metric_df$Value), , drop = FALSE]
    validate(need(nrow(metric_df) > 0, if (identical(current_lang(), "zh")) "没有可绘制的刺激响应指标。" else "No plottable stimulus-response metrics."))
    ggplot(metric_df, aes(x = Repetition_Index, y = Value, color = Metric, group = interaction(Train, Metric))) +
      geom_line(linewidth = 0.55, alpha = 0.75) +
      geom_point(size = 1.7, alpha = 0.9) +
      labs(x = if (identical(current_lang(), "zh")) "刺激序号" else "Stimulus repetition",
           y = if (identical(current_lang(), "zh")) "响应指标" else "Response metric",
           color = NULL) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(current_lang()))
  })

  feature_response_plot_class <- function(cls) {
    cls <- as.character(cls)
    cls[!nzchar(cls) | is.na(cls)] <- "other"
    cls[!cls %in% c("preferred_excitatory", "preferred_biphasic", "null_suppressive",
                    "preferred_suppressive", "neutral_baseline", "nonresponsive")] <- "other"
    cls
  }

  feature_response_symbol <- function(cls) {
    cls <- feature_response_plot_class(cls)
    out <- rep("", length(cls))
    out[cls == "preferred_excitatory"] <- "B"
    out[cls == "preferred_biphasic"] <- "B/P"
    out[cls %in% c("null_suppressive", "preferred_suppressive")] <- "S"
    out[cls == "nonresponsive"] <- "NR"
    out
  }

  committed_feature_response_symbol <- function(cls, evoked_spikes = NA_real_,
                                                scorable_pause = NA_real_,
                                                suppression = NA_real_) {
    cls <- feature_response_plot_class(cls)
    evoked_spikes <- suppressWarnings(as.numeric(evoked_spikes))
    scorable_pause <- suppressWarnings(as.numeric(scorable_pause))
    suppression <- suppressWarnings(as.numeric(suppression))
    has_burst <- is.finite(evoked_spikes) & evoked_spikes > 0
    has_pause <- is.finite(scorable_pause) & scorable_pause > 0
    has_suppression <- is.finite(suppression) & suppression > 0
    out <- rep("", length(cls))
    out[has_burst & has_pause] <- "B/P"
    out[has_burst & !has_pause] <- "B"
    out[!has_burst & has_pause] <- "P"
    out[!has_burst & !has_pause & has_suppression] <- "S"
    missing_response_metrics <- !is.finite(evoked_spikes) & !is.finite(scorable_pause) & !is.finite(suppression)
    fallback <- missing_response_metrics & !nzchar(out)
    out[fallback] <- feature_response_symbol(cls[fallback])
    out
  }

  feature_value_suffix <- function(modality) {
    modality <- tolower(as.character(value_or(modality, ""))[1])
    if (modality %in% c("orientation", "motion_direction", "color_hue")) return(" deg")
    if (identical(modality, "auditory_frequency")) return(" Hz")
    ""
  }

  format_feature_value <- function(x, modality = "") {
    x <- as.numeric(x)
    suffix <- feature_value_suffix(modality)
    ifelse(is.finite(x), paste0(format(round(x, 3), trim = TRUE, scientific = FALSE), suffix), "NA")
  }

  make_feature_tuning_map_plot <- function(sim, lang) {
    drive <- if (!is.null(sim$combined_unit_stimulus_drive)) sim$combined_unit_stimulus_drive else make_empty_unit_stimulus_drive_df()
    validate(need(nrow(drive) > 0, if (identical(lang, "zh")) "没有 unit × stimulus drive 数据。" else "No unit-by-stimulus drive data."))
    if (!"Feature_Modality" %in% names(drive)) {
      validate(need(FALSE, if (identical(lang, "zh")) "当前数据没有特征调谐字段。" else "No feature-tuning fields are available."))
    }
    drive <- drive[!is.na(drive$Feature_Modality) & nzchar(as.character(drive$Feature_Modality)), , drop = FALSE]
    validate(need(nrow(drive) > 0, if (identical(lang, "zh")) "当前模拟没有启用特征调谐刺激。" else "Feature-tuned stimulation is not enabled in this simulation."))
    modality <- tolower(as.character(drive$Feature_Modality[which(!is.na(drive$Feature_Modality) & nzchar(as.character(drive$Feature_Modality)))[1]]))
    if (!nzchar(modality)) modality <- "feature"

    fill_values <- c(
      preferred_excitatory = "#C7631E",
      preferred_biphasic = "#8B5CF6",
      null_suppressive = "#2F73B7",
      preferred_suppressive = "#60A5FA",
      neutral_baseline = "#EBEBEB",
      nonresponsive = "#F9FAFB",
      other = "#D1D5DB"
    )
    class_labels <- if (identical(lang, "zh")) {
      c(preferred_excitatory = "优选: Burst", preferred_biphasic = "优选: 双相",
        null_suppressive = "Null/opponent 区域", preferred_suppressive = "优选: 抑制核",
        neutral_baseline = "基线/弱反应", nonresponsive = "无响应", other = "其他")
    } else {
      c(preferred_excitatory = "Preferred: Burst", preferred_biphasic = "Preferred: biphasic",
        null_suppressive = "Null/opponent zone", preferred_suppressive = "Preferred: suppressive kernel",
        neutral_baseline = "Baseline/weak", nonresponsive = "Nonresponsive", other = "Other")
    }

    if (is_2d_feature_modality(modality)) {
      keep <- is.finite(drive$Stimulus_Position_X) & is.finite(drive$Stimulus_Position_Y) & is.finite(drive$Feature_Drive)
      validate(need(any(keep), if (identical(lang, "zh")) "没有可绘制的二维空间调谐数据。" else "No plottable 2D spatial tuning data."))
      center_x_col <- if ("Unit_Place_Field_Center_X" %in% names(drive)) "Unit_Place_Field_Center_X" else "Place_Field_Center_X"
      center_y_col <- if ("Unit_Place_Field_Center_Y" %in% names(drive)) "Unit_Place_Field_Center_Y" else "Place_Field_Center_Y"
      raw <- data.frame(
        Train = as.integer(drive$Train[keep]),
        Unit_ID = as.integer(drive$Unit_ID[keep]),
        Stimulus_Position_X = as.numeric(drive$Stimulus_Position_X[keep]),
        Stimulus_Position_Y = as.numeric(drive$Stimulus_Position_Y[keep]),
        Feature_Response_Class = feature_response_plot_class(drive$Feature_Response_Class[keep]),
        Feature_Drive = pmin(1, pmax(0, as.numeric(drive$Feature_Drive[keep]))),
        Evoked_Spike_Count = if ("Evoked_Spike_Count" %in% names(drive)) as.numeric(drive$Evoked_Spike_Count[keep]) else NA_real_,
        Scorable_Evoked_Pause_Duration_s = if ("Scorable_Evoked_Pause_Duration_s" %in% names(drive)) as.numeric(drive$Scorable_Evoked_Pause_Duration_s[keep]) else NA_real_,
        Evoked_Suppression_Duration_s = if ("Evoked_Suppression_Duration_s" %in% names(drive)) as.numeric(drive$Evoked_Suppression_Duration_s[keep]) else NA_real_,
        Center_X = as.numeric(drive[[center_x_col]][keep]),
        Center_Y = as.numeric(drive[[center_y_col]][keep]),
        stringsAsFactors = FALSE
      )
      raw$Unit_Label <- paste0("Train ", raw$Train, " / field ", format(round(raw$Center_X, 2), trim = TRUE), ",", format(round(raw$Center_Y, 2), trim = TRUE))
      group_cols <- c("Train", "Unit_ID", "Unit_Label", "Stimulus_Position_X", "Stimulus_Position_Y", "Feature_Response_Class")
      plot_df <- stats::aggregate(
        raw[c("Feature_Drive", "Evoked_Spike_Count", "Scorable_Evoked_Pause_Duration_s", "Evoked_Suppression_Duration_s")],
        by = raw[group_cols],
        FUN = function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
      )
      plot_df$Feature_Response_Class <- factor(plot_df$Feature_Response_Class, levels = names(fill_values))
      plot_df$Response_Symbol <- committed_feature_response_symbol(
        plot_df$Feature_Response_Class,
        plot_df$Evoked_Spike_Count,
        plot_df$Scorable_Evoked_Pause_Duration_s,
        plot_df$Evoked_Suppression_Duration_s
      )
      unit_levels <- unique(plot_df$Unit_Label[order(plot_df$Train, plot_df$Unit_ID)])
      plot_df$Unit_Label <- factor(plot_df$Unit_Label, levels = unit_levels)
      ggplot(plot_df, aes(x = Stimulus_Position_X, y = Stimulus_Position_Y)) +
        geom_point(aes(fill = Feature_Response_Class, alpha = Feature_Drive), shape = 21, size = 4.4, color = "#111827", stroke = 0.25) +
        geom_text(aes(label = Response_Symbol), size = 2.6, color = "#111827") +
        facet_wrap(~ Unit_Label, ncol = min(4, length(unit_levels))) +
        scale_fill_manual(values = fill_values, labels = class_labels, drop = FALSE) +
        scale_alpha_continuous(range = c(0.35, 1), guide = "none") +
        coord_equal() +
        labs(x = if (identical(lang, "zh")) "空间 X" else "Spatial X",
             y = if (identical(lang, "zh")) "空间 Y" else "Spatial Y",
             fill = if (identical(lang, "zh")) "响应类型" else "Response class") +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        nature_plot_theme(base_size = 9, base_family = plot_font_family(lang))
    } else {
      pref_col <- if ("Unit_Preferred_Feature_Value" %in% names(drive)) "Unit_Preferred_Feature_Value" else "Preferred_Feature_Value"
      null_col <- if ("Unit_Null_Feature_Value" %in% names(drive)) "Unit_Null_Feature_Value" else "Null_Feature_Value"
      keep <- is.finite(drive$Stimulus_Feature_Value) & is.finite(drive$Feature_Drive)
      validate(need(any(keep), if (identical(lang, "zh")) "没有可绘制的一维特征调谐数据。" else "No plottable one-dimensional feature tuning data."))
      raw <- data.frame(
        Train = as.integer(drive$Train[keep]),
        Unit_ID = as.integer(drive$Unit_ID[keep]),
        Preferred_Value = as.numeric(drive[[pref_col]][keep]),
        Null_Value = as.numeric(drive[[null_col]][keep]),
        Stimulus_Feature_Value = as.numeric(drive$Stimulus_Feature_Value[keep]),
        Feature_Response_Class = feature_response_plot_class(drive$Feature_Response_Class[keep]),
        Feature_Drive = pmin(1, pmax(0, as.numeric(drive$Feature_Drive[keep]))),
        Evoked_Spike_Count = if ("Evoked_Spike_Count" %in% names(drive)) as.numeric(drive$Evoked_Spike_Count[keep]) else NA_real_,
        Scorable_Evoked_Pause_Duration_s = if ("Scorable_Evoked_Pause_Duration_s" %in% names(drive)) as.numeric(drive$Scorable_Evoked_Pause_Duration_s[keep]) else NA_real_,
        Evoked_Suppression_Duration_s = if ("Evoked_Suppression_Duration_s" %in% names(drive)) as.numeric(drive$Evoked_Suppression_Duration_s[keep]) else NA_real_,
        stringsAsFactors = FALSE
      )
      raw$Unit_Label <- paste0("Train ", raw$Train, " / pref ", format_feature_value(raw$Preferred_Value, modality))
      group_cols <- c("Train", "Unit_ID", "Unit_Label", "Preferred_Value", "Null_Value", "Stimulus_Feature_Value", "Feature_Response_Class")
      plot_df <- stats::aggregate(
        raw[c("Feature_Drive", "Evoked_Spike_Count", "Scorable_Evoked_Pause_Duration_s", "Evoked_Suppression_Duration_s")],
        by = raw[group_cols],
        FUN = function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
      )
      plot_df$Feature_Response_Class <- factor(plot_df$Feature_Response_Class, levels = names(fill_values))
      plot_df$Response_Symbol <- committed_feature_response_symbol(
        plot_df$Feature_Response_Class,
        plot_df$Evoked_Spike_Count,
        plot_df$Scorable_Evoked_Pause_Duration_s,
        plot_df$Evoked_Suppression_Duration_s
      )
      unit_levels <- unique(plot_df$Unit_Label[order(plot_df$Train, plot_df$Unit_ID)])
      plot_df$Unit_Label <- factor(plot_df$Unit_Label, levels = rev(unit_levels))
      breaks <- sort(unique(plot_df$Stimulus_Feature_Value))
      ggplot(plot_df, aes(x = Stimulus_Feature_Value, y = Unit_Label)) +
        geom_tile(aes(fill = Feature_Response_Class, alpha = Feature_Drive), color = "white", linewidth = 0.55, height = 0.86) +
        geom_text(aes(label = Response_Symbol), size = 3.3, color = "#111827", fontface = "bold") +
        scale_fill_manual(values = fill_values, labels = class_labels, drop = FALSE) +
        scale_alpha_continuous(range = c(0.35, 1), guide = "none") +
        scale_x_continuous(breaks = breaks, labels = format_feature_value(breaks, modality), expand = expansion(mult = c(0.01, 0.01))) +
        labs(x = if (identical(lang, "zh")) "刺激特征值" else "Stimulus feature value",
             y = if (identical(lang, "zh")) "Spike train / 神经元优选值" else "Spike train / preferred value",
             fill = if (identical(lang, "zh")) "响应类型" else "Response class") +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
    }
  }

  output$feature_tuning_map_plot <- renderPlot({
    req(all_spike_trains())
    make_feature_tuning_map_plot(all_spike_trains(), current_lang())
  })

  output$feature_tuning_map_plot_analysis <- renderPlot({
    req(all_spike_trains())
    make_feature_tuning_map_plot(all_spike_trains(), current_lang())
  })

  output$unit_table <- renderDT({
    req(all_spike_trains())
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$combined_units)) sim$combined_units else make_empty_unit_df()
    datatable(export_pattern_values(tab), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$unit_stimulus_drive_table <- renderDT({
    req(all_spike_trains())
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$combined_unit_stimulus_drive)) sim$combined_unit_stimulus_drive else make_empty_unit_stimulus_drive_df()
    DT::datatable(export_pattern_values(tab), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$stimulus_table <- renderDT({
    req(all_spike_trains())
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df()
    datatable(export_pattern_values(tab), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$response_table <- renderDT({
    req(all_spike_trains())
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$combined_responses)) sim$combined_responses else make_empty_response_df()
    datatable(export_pattern_values(tab), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$event_epoch_table <- renderDT({
    req(all_spike_trains())
    sim <- all_spike_trains()
    tab <- if (!is.null(sim$combined_event_epochs)) sim$combined_event_epochs else make_empty_event_epoch_df()
    datatable(export_pattern_values(tab), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  stimulus_alignment_window <- reactive({
    pre <- max(0, safe_num(input$stim_align_pre_s, 1.0))
    post <- max(0.05, safe_num(input$stim_align_post_s, 1.5))
    c(-pre, post)
  })

  output$stimulus_aligned_raster_plot <- renderPlot({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    window <- stimulus_alignment_window()
    aligned <- stimulus_aligned_spike_table(sim, window = window)
    validate(need(nrow(aligned) > 0, if (identical(lang, "zh")) "没有可用于刺激对齐 raster 的 spike / stimulus 数据。" else "No spike / stimulus data available for stimulus-aligned raster."))
    trial_labels <- unique(aligned[, c("Train", "Trial_Key", "Stimulus_ID", "Stimulus_Type"), drop = FALSE])
    trial_labels <- trial_labels[order(trial_labels$Trial_Key), , drop = FALSE]
    aligned$Trial_Display <- match(aligned$Trial_Key, trial_labels$Trial_Key)
    ggplot(aligned, aes(x = Relative_Time_s, y = Trial_Display)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#7C3AED", linewidth = 0.5) +
      geom_segment(aes(xend = Relative_Time_s, y = Trial_Display - 0.38, yend = Trial_Display + 0.38),
                   linewidth = 0.35, color = "black") +
      scale_y_continuous(breaks = seq_len(nrow(trial_labels)),
                         labels = paste0("T", trial_labels$Train, " S", trial_labels$Stimulus_ID),
                         expand = expansion(mult = c(0.02, 0.04))) +
      labs(x = if (identical(lang, "zh")) "相对刺激 onset 时间 (s)" else "Time from stimulus onset (s)",
           y = if (identical(lang, "zh")) "Trial" else "Trial") +
      coord_cartesian(xlim = window) +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
  })

  output$stimulus_aligned_psth_plot <- renderPlot({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    window <- stimulus_alignment_window()
    bin_width <- max(0.001, safe_num(input$stim_psth_bin_s, 0.05))
    psth <- stimulus_psth_tables(sim, window = window, bin_width = bin_width)$summary
    validate(need(nrow(psth) > 0, if (identical(lang, "zh")) "没有可用于 PSTH 的刺激 trial。" else "No stimulus trials available for PSTH."))
    psth$SEM_Rate_Hz[!is.finite(psth$SEM_Rate_Hz)] <- 0
    ggplot(psth, aes(x = Bin_Center_s, y = Mean_Rate_Hz)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#7C3AED", linewidth = 0.5) +
      geom_ribbon(aes(ymin = pmax(0, Mean_Rate_Hz - SEM_Rate_Hz), ymax = Mean_Rate_Hz + SEM_Rate_Hz),
                  fill = "#9CA3AF", alpha = 0.25) +
      geom_line(color = "#2563EB", linewidth = 0.75) +
      labs(x = if (identical(lang, "zh")) "相对刺激 onset 时间 (s)" else "Time from stimulus onset (s)",
           y = if (identical(lang, "zh")) "平均发放率 (Hz)" else "Mean firing rate (Hz)") +
      coord_cartesian(xlim = window) +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
  })

  output$stimulus_latency_hist_plot <- renderPlot({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    resp <- if (!is.null(sim$combined_responses)) sim$combined_responses else make_empty_response_df()
    resp <- resp[is.finite(resp$Response_Latency_s), , drop = FALSE]
    validate(need(nrow(resp) > 0, if (identical(lang, "zh")) "没有可绘制的响应潜伏期。" else "No response latencies available."))
    ggplot(resp, aes(x = Response_Latency_s, fill = Response_Type)) +
      geom_histogram(bins = 24, alpha = 0.65, color = "white", linewidth = 0.25, position = "identity") +
      labs(x = if (identical(lang, "zh")) "响应潜伏期 (s)" else "Response latency (s)",
           y = if (identical(lang, "zh")) "计数" else "Count",
           fill = if (identical(lang, "zh")) "响应类型" else "Response type") +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
  })

  output$stimulus_response_metric_plot <- renderPlot({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    resp <- if (!is.null(sim$combined_responses)) sim$combined_responses else make_empty_response_df()
    stim <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df()
    validate(need(nrow(resp) > 0 && nrow(stim) > 0, if (identical(lang, "zh")) "没有刺激响应指标。" else "No stimulus-response metrics."))
    key_cols <- intersect(c("Train", "Stimulus_ID"), intersect(names(resp), names(stim)))
    dat <- if (length(key_cols) == 2) merge(resp, stim, by = key_cols, all.x = TRUE, suffixes = c("", "_stim")) else resp
    if (!"Repetition_Index" %in% names(dat)) dat$Repetition_Index <- seq_len(nrow(dat))
    metric_df <- rbind(
      data.frame(Repetition_Index = dat$Repetition_Index, Metric = "Evoked spikes", Value = dat$Evoked_Spike_Count, stringsAsFactors = FALSE),
      data.frame(Repetition_Index = dat$Repetition_Index, Metric = "Response gain", Value = dat$Response_Gain, stringsAsFactors = FALSE),
      data.frame(Repetition_Index = dat$Repetition_Index, Metric = "Suppression index", Value = dat$Suppression_Index, stringsAsFactors = FALSE)
    )
    metric_df <- metric_df[is.finite(metric_df$Value), , drop = FALSE]
    validate(need(nrow(metric_df) > 0, if (identical(lang, "zh")) "没有可绘制的响应指标。" else "No plottable response metrics."))
    ggplot(metric_df, aes(x = Repetition_Index, y = Value, color = Metric)) +
      geom_line(linewidth = 0.6, alpha = 0.8) +
      geom_point(size = 1.6, alpha = 0.9) +
      facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
      labs(x = if (identical(lang, "zh")) "刺激序号" else "Stimulus repetition",
           y = if (identical(lang, "zh")) "指标值" else "Metric value",
           color = NULL) +
      guides(color = "none") +
      nature_plot_theme(base_size = 10, base_family = plot_font_family(lang))
  })

  output$episode_table <- renderDT({
    req(all_spike_trains())
    lang <- current_lang()
    sim <- all_spike_trains()
    episode_display <- translate_pattern_values(sim$combined_episodes, lang)
    datatable(
      episode_display,
      rownames = FALSE,
      colnames = episode_col_labels(lang, names(episode_display)),
      options = table_options(tr(lang, "empty_episode"))
    )
  })


  output$downloadUnits <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_unit_tuning_table.csv",
    content = function(file) {
      sim <- all_spike_trains()
      unit_data <- if (!is.null(sim$combined_units)) sim$combined_units else make_empty_unit_df()
      unit_data <- add_reproducibility_columns(export_pattern_values(unit_data), sim)
      utils::write.csv(unit_data, file, row.names = FALSE)
    }
  )

  output$downloadUnitStimulusDrive <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_unit_stimulus_drive_table.csv",
    content = function(file) {
      sim <- all_spike_trains()
      unit_drive_data <- if (!is.null(sim$combined_unit_stimulus_drive)) sim$combined_unit_stimulus_drive else make_empty_unit_stimulus_drive_df()
      unit_drive_data <- add_reproducibility_columns(export_pattern_values(unit_drive_data), sim)
      utils::write.csv(unit_drive_data, file, row.names = FALSE)
    }
  )

  output$downloadStimuli <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_stimulus_table.csv",
    content = function(file) {
      sim <- all_spike_trains()
      stimulus_data <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli else make_empty_stimulus_df()
      stimulus_data <- add_reproducibility_columns(export_pattern_values(stimulus_data), sim)
      utils::write.csv(stimulus_data, file, row.names = FALSE)
    }
  )

  output$downloadResponses <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_response_table.csv",
    content = function(file) {
      sim <- all_spike_trains()
      response_data <- if (!is.null(sim$combined_responses)) sim$combined_responses else make_empty_response_df()
      response_data <- add_reproducibility_columns(export_pattern_values(response_data), sim)
      utils::write.csv(response_data, file, row.names = FALSE)
    }
  )

  output$downloadEventEpochs <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_event_epoch_table.csv",
    content = function(file) {
      sim <- all_spike_trains()
      event_epoch_data <- if (!is.null(sim$combined_event_epochs)) sim$combined_event_epochs else make_empty_event_epoch_df()
      event_epoch_data <- add_reproducibility_columns(export_pattern_values(event_epoch_data), sim)
      utils::write.csv(event_epoch_data, file, row.names = FALSE)
    }
  )

  output$downloadNWBMapping <- downloadHandler(
    filename = function() "SPIKE_TRAIN_SIMULATOR_V13_5_0_nwb_mapping.json",
    content = function(file) {
      sim <- all_spike_trains()
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("jsonlite is required to write the NWB mapping JSON.", call. = FALSE)
      }
      jsonlite::write_json(nwb_mapping_payload(sim), file, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_spike_matrix.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      require_current_train_count(sim, isolate(current_lang()))
      spike_matrix <- add_duration_columns(spike_train_matrix_table(sim$combined_spikes), sim)
      write.csv(add_reproducibility_columns(spike_matrix, sim), file, row.names = FALSE)
    }
  )

  write_per_train_spike_csv_zip <- function(sim, file) {
    spike_events <- spike_isi_table(sim$combined_spikes)
    spike_events <- add_duration_columns(spike_events, sim)
    spike_events <- export_pattern_values(spike_events)
    spike_events <- add_reproducibility_columns(spike_events, sim)
    summary_rows <- do.call(rbind, lapply(seq_along(sim$spikes_list), function(i) {
      get_train_stats(sim$spikes_list[[i]], sim$episodes_list[[i]], sim$intervals_list[[i]], i)
    }))
    summary_rows <- add_reproducibility_columns(summary_rows, sim)

    train_ids <- sort(unique(as.integer(spike_events$Train[is.finite(spike_events$Train)])))
    if (length(train_ids) == 0) {
      train_ids <- seq_len(generated_train_count(sim))
    }

    tmpdir <- tempfile("spike_train_per_train_csv_")
    dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

    csv_files <- character(0)
    for (train_id in train_ids) {
      train_rows <- spike_events[as.integer(spike_events$Train) == train_id, , drop = FALSE]
      train_file <- file.path(tmpdir, sprintf("Spike_train_%03d_spike_events.csv", as.integer(train_id)))
      utils::write.csv(train_rows, train_file, row.names = FALSE)
      csv_files <- c(csv_files, train_file)
    }

    summary_file <- file.path(tmpdir, "Summary_all_trains.csv")
    utils::write.csv(summary_rows, summary_file, row.names = FALSE)
    reproduction_file <- file.path(tmpdir, "Self_contained_reproduction_code.txt")
    writeLines(value_or(sim$reproduction_code, ""), reproduction_file)
    train_seed_file <- file.path(tmpdir, "Train_Seeds.csv")
    utils::write.csv(train_seed_table(sim), train_seed_file, row.names = FALSE)
    software_parameters_file <- file.path(tmpdir, "software_parameters.yaml")
    write_software_parameters_yaml(
      software_parameters_payload(sim, value_or(sim$benchmark_difficulty, "interactive_export"), package_type = "per_train_csv_zip"),
      software_parameters_file
    )

    manifest <- data.frame(
      File = c(basename(summary_file), basename(reproduction_file), basename(train_seed_file), basename(software_parameters_file), basename(csv_files)),
      Train = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, train_ids),
      Rows = c(nrow(summary_rows), NA_integer_, nrow(train_seed_table(sim)), NA_integer_, vapply(csv_files, function(path) {
          train_id <- as.integer(sub("^Spike_train_([0-9]+)_.*$", "\\1", basename(path)))
          nrow(spike_events[as.integer(spike_events$Train) == train_id, , drop = FALSE])
        }, integer(1))),
      Contents = c(
        "Per-train simulation summary",
        "Self-contained reproduction code",
        "Per-train RNG-state audit",
        "Software parameters in YAML format",
        rep("Per-train spike event audit table", length(csv_files))
      ),
      stringsAsFactors = FALSE
    )
    manifest_file <- file.path(tmpdir, "README_manifest.csv")
    utils::write.csv(manifest, manifest_file, row.names = FALSE)
    csv_files <- c(manifest_file, summary_file, reproduction_file, train_seed_file, software_parameters_file, csv_files)

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(tmpdir)
    status <- utils::zip(zipfile = file, files = basename(csv_files), flags = "-q")
    if (!identical(status, 0L)) {
      stop("Failed to create per-train CSV ZIP export.", call. = FALSE)
    }
  }

  output$downloadPerTrainCsvZip <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_per_train_spike_events_csv.zip"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      write_per_train_spike_csv_zip(sim, file)
    },
    contentType = "application/zip"
  )

  output$downloadReproduction <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_reproduction_code.txt"
    },
    content = function(file) {
      sim <- all_spike_trains()
      require_current_train_count(sim, isolate(current_lang()))
      writeLines(value_or(sim$reproduction_code, ""), file)
    }
  )

  output$downloadSpikeEvents <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_latent_spike_events_audit.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      spike_events <- spike_isi_table(sim$combined_spikes)
      spike_events <- add_duration_columns(spike_events, sim)
      spike_events <- export_pattern_values(spike_events)
      write.csv(add_reproducibility_columns(spike_events, sim), file, row.names = FALSE)
    }
  )

  output$downloadLatentDetectorInput <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_latent_spike_events_input.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      write.csv(latent_spike_events_input_table(sim), file, row.names = FALSE)
    }
  )

  output$downloadObservedSpikeEvents <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_observed_spike_events_audit.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      observed_events <- observed_spike_events_table(sim)
      observed_events <- add_duration_columns(observed_events, sim)
      write.csv(add_reproducibility_columns(observed_events, sim), file, row.names = FALSE)
    }
  )

  output$downloadObservedDetectorInput <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_observed_spike_events_input.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      write.csv(observed_spike_events_input_table(sim), file, row.names = FALSE)
    }
  )

  output$downloadObservationMap <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_observation_map.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      obs_map <- if (!is.null(sim$observation_map)) sim$observation_map else make_empty_observation_map_df()
      obs_map <- add_duration_columns(obs_map, sim)
      write.csv(add_reproducibility_columns(obs_map, sim), file, row.names = FALSE)
    }
  )

  output$downloadDetailedData <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_interval_table.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      interval_details <- sim$combined_intervals
      interval_details <- add_duration_columns(interval_details, sim)
      interval_details <- export_pattern_values(interval_details)
      write.csv(add_reproducibility_columns(interval_details, sim), file, row.names = FALSE)
    }
  )

  output$downloadEpisodes <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_episodes.csv"
    },
    content = function(file) {
      sim <- all_spike_trains()
      lang <- isolate(current_lang())
      require_current_train_count(sim, lang)
      episode_data <- sim$combined_episodes
      episode_data <- add_duration_columns(episode_data, sim)
      episode_data <- export_pattern_values(episode_data)
      write.csv(add_reproducibility_columns(episode_data, sim), file, row.names = FALSE)
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      "SPIKE_TRAIN_SIMULATOR_V13_5_0_spike_train_raster.pdf"
    },
    content = function(file) {
      sim <- all_spike_trains()
      require_current_train_count(sim, isolate(current_lang()))
      full_window <- spike_pdf_full_window(sim)
      pdf_size <- spike_pdf_dimensions(sim, full_window)
      device_lang <- isolate(current_lang())
      device_type <- open_pdf_device(
        file,
        width = pdf_size$width,
        height = pdf_size$height,
        family = plot_font_family(device_lang)
      )
      on.exit(dev.off(), add = TRUE)
      pdf_lang <- if (identical(device_type, "pdf")) "en" else device_lang

      for (i in seq_along(sim$spikes_list)) {
        train_spikes <- sim$combined_spikes[sim$combined_spikes$Train == i, ]
        train_episodes <- sim$combined_episodes[sim$combined_episodes$Train == i, ]
        train_intervals <- sim$combined_intervals[sim$combined_intervals$Train == i, ]
        train_stimuli <- if (!is.null(sim$combined_stimuli)) sim$combined_stimuli[sim$combined_stimuli$Train == i, , drop = FALSE] else make_empty_stimulus_df()
        print(make_spike_plot(
          train_spikes,
          train_episodes,
          i,
          lang_override = pdf_lang,
          window_override = full_window,
          train_intervals = train_intervals,
          train_stimuli = train_stimuli
        ))
      }
    }
  )
}

shinyApp(ui, server)
