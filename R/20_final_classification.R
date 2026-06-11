# eventness audit centralized final classification audit.
# Detector outputs are preserved by default; this layer adds biologically
# interpretable recommendations: event-like vs state-like family, subtype,
# decision path and reporting layer.

final_classify_candidate_core <- function(feature, params = default_params_sec(), preserve_detector_class = TRUE) {
  if (is.null(feature) || nrow(as.data.frame(feature)) == 0) {
    return(data.frame(
      final_class = character(), confidence_tier = character(), review_required = logical(),
      decision_reason = character(), biological_warning = character(),
      recommended_family = character(), recommended_subtype = character(),
      recommended_final_class = character(), decision_path = character(),
      eventness_zone = character(), recommended_review_required = logical(),
      recommendation_confidence_tier = character(), recommended_uncertainty_reason = character(),
      long_burst_definition_status = character(),
      stringsAsFactors = FALSE
    ))
  }
  row <- as.data.frame(feature)[1, , drop = FALSE]
  get_chr <- function(nm, default = "") { if (nm %in% names(row) && !is.na(row[[nm]][1])) as.character(row[[nm]][1]) else default }
  get_num <- function(nm, default = NA_real_) { if (nm %in% names(row)) { v <- suppressWarnings(as.numeric(row[[nm]][1])); if (is.finite(v)) return(v) }; default }
  get_log <- function(nm, default = FALSE) { if (nm %in% names(row)) { v <- row[[nm]][1]; if (!is.na(v)) return(isTRUE(v) || identical(tolower(as.character(v)), "true")) }; default }

  cls0 <- get_chr("final_candidate_class", get_chr("class", get_chr("raw_candidate_class", "")))
  source <- get_chr("candidate_source", get_chr("source", ""))
  reason <- get_chr("uncertainty_reason", "")
  ref_n <- get_num("refractory_suspect_n", 0)
  boundary <- grepl("boundary", source, ignore.case = TRUE) || get_log("boundary_burst", FALSE) || is.finite(get_num("one_flank_ratio", NA_real_))
  local_comp <- grepl("local_compression", source, ignore.case = TRUE) || get_log("local_compression_burst", FALSE)

  final <- if (nzchar(cls0)) cls0 else "unclassified"
  action <- (params$detector$refractory_suspect_action %||% params$burst$refractory_suspect_action %||% "demote_to_possible")

  # Preserve legacy post-policy detector semantics. These rules are conservative
  # and define final_class, not the new recommended subtype audit.
  if (ref_n > 0 && final %in% c("burst", "long_burst") && action %in% c("demote_to_possible", "split_at_suspect", "exclude_suspect_isi_and_reevaluate", "mark_multiunit_contamination")) {
    final <- "possible_burst"
    reason <- paste(c(reason, "contains_refractory_suspect_ISI"), collapse = ";")
  }
  if (boundary && final %in% c("burst", "long_burst", "unclassified")) {
    final <- "possible_burst"
    reason <- paste(c(reason, "boundary_one_sided_evidence"), collapse = ";")
  }
  if (local_comp && final == "burst" && (params$burst$local_compression_candidate_class %||% "possible_burst") == "possible_burst") {
    final <- "possible_burst"
    reason <- paste(c(reason, "local_compression_review_candidate"), collapse = ";")
  }

  review <- final %in% c("possible_burst", "possible_pause", "unclassified", "rejected") || grepl("refractory|boundary|review|near_miss|local_compression", reason, ignore.case = TRUE)
  confidence <- if (final %in% c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause") && !review) {
    "high_confidence"
  } else if (review) {
    "review_required"
  } else {
    "contextual"
  }
  warning <- candidate_biological_warning(final, reason, source)
  if (final == "long_burst" && !nzchar(warning)) warning <- "Structural long_burst: distinguish from sustained high-rate epoch by context, waveform quality, and validation."

  rec <- tryCatch(stpd_recommend_family_subtype(row, params = params), error = function(e) {
    data.frame(recommended_family = "unresolved", recommended_subtype = "unresolved", recommended_final_class = final,
               decision_path = paste0("recommendation_failed: ", conditionMessage(e)),
               eventness_zone = "unknown", recommended_review_required = TRUE,
               recommendation_confidence_tier = "audit_error", recommended_uncertainty_reason = conditionMessage(e),
               long_burst_definition_status = "unresolved", stringsAsFactors = FALSE)
  })

  rec_chr <- function(nm, default = "") if (nm %in% names(rec) && length(rec[[nm]]) > 0 && !is.na(rec[[nm]][1])) as.character(rec[[nm]][1]) else default
  rec_log <- function(nm, default = FALSE) if (nm %in% names(rec) && length(rec[[nm]]) > 0 && !is.na(rec[[nm]][1])) isTRUE(rec[[nm]][1]) || identical(tolower(as.character(rec[[nm]][1])), "true") else default

  data.frame(
    final_class = final,
    confidence_tier = confidence,
    review_required = review,
    decision_reason = reason,
    biological_warning = warning,
    recommended_family = rec_chr("recommended_family", "unresolved"),
    recommended_subtype = rec_chr("recommended_subtype", "unresolved"),
    recommended_final_class = rec_chr("recommended_final_class", final),
    decision_path = rec_chr("decision_path", ""),
    eventness_zone = rec_chr("eventness_zone", "unknown"),
    recommended_review_required = rec_log("recommended_review_required", FALSE),
    recommendation_confidence_tier = rec_chr("recommendation_confidence_tier", "audit_contextual"),
    recommended_uncertainty_reason = rec_chr("recommended_uncertainty_reason", ""),
    long_burst_definition_status = rec_chr("long_burst_definition_status", ""),
    stringsAsFactors = FALSE
  )
}

final_classify_candidates_core <- function(features, params = default_params_sec(), preserve_detector_class = TRUE) {
  if (is.null(features) || nrow(features) == 0) return(tibble::tibble())
  rows <- lapply(seq_len(nrow(features)), function(ii) final_classify_candidate(features[ii, , drop = FALSE], params, preserve_detector_class = preserve_detector_class))
  dec <- dplyr::bind_rows(rows)
  feature_tbl <- tibble::as_tibble(features)
  dec_tbl <- tibble::as_tibble(dec)
  # eventness audit: candidate features already contain audit primitives such as
  # eventness_zone and long_burst_definition_status. Do not bind duplicate
  # columns from the recommendation object; keep the feature-table values as
  # the single source for these audit primitives.
  dec_tbl <- dec_tbl[, setdiff(names(dec_tbl), intersect(names(dec_tbl), names(feature_tbl))), drop = FALSE]
  out <- dplyr::bind_cols(feature_tbl, dec_tbl)
  out$recommended_reporting_layer <- dplyr::case_when(
    out$confidence_tier == "high_confidence" ~ "Events_high_confidence",
    out$final_class %in% c("possible_burst", "possible_pause") ~ "Events_review_candidates",
    out$final_class %in% c("burst", "long_burst", "possible_burst") ~ "Events_burst_family_candidates",
    TRUE ~ "Context / audit only"
  )
  out$final_classification_note <- dplyr::case_when(
    out$final_class == "long_burst" ~ "Structural long_burst: confirm event-like biology versus sustained high-rate epoch.",
    out$final_class == "possible_burst" ~ "Review candidate; do not count as high-confidence burst unless reviewed.",
    out$final_class %in% c("high_frequency_tonic", "high_frequency_spiking") ~ "High-frequency epoch; interpret by cell type and sorting quality.",
    TRUE ~ ""
  )
  out$recommended_interpretation_note <- dplyr::case_when(
    out$recommended_family == "burst_event" & out$recommended_subtype == "high_frequency_burst" ~ "Eventness is high; regularity alone should not reclassify this candidate as high-frequency tonic.",
    out$recommended_subtype == "long_burst" & out$long_burst_definition_status == "strict_pass" ~ "Recommended long_burst satisfies strict eventness audit structural criteria; still confirm biology vs sustained high-rate epoch.",
    out$recommended_subtype == "long_burst" ~ paste0("Recommended structural long_burst requires review; strictness status: ", out$long_burst_definition_status),
    out$eventness_zone == "medium_eventness_review" ~ "Medium eventness: treat subtype recommendation as review-only until confirmed by context/manual audit.",
    out$recommended_family == "state_epoch" ~ "Recommended state-like epoch: classification uses low eventness plus rate/regularity/extent.",
    TRUE ~ ""
  )
  out
}

final_classify_candidate <- final_classify_candidate_core
final_classify_candidates <- final_classify_candidates_core
