# Lightweight in-app localization for the Shiny UI.
# The first implementation is deliberately client-side so switching language
# does not rebuild the Shiny page or clear uploaded data.

stpd_i18n_json_quote <- function(x) {
  x <- enc2utf8(as.character(x %||% ""))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  paste0("\"", x, "\"")
}

stpd_i18n_json_object <- function(x) {
  if (length(x) == 0) return("{}")
  nms <- names(x)
  paste0(
    "{",
    paste0(stpd_i18n_json_quote(nms), ":", stpd_i18n_json_quote(unname(x)), collapse = ","),
    "}"
  )
}

stpd_i18n_exact_dictionary <- function() {
  c(
    "\u8BED\u8A00 / Language" = "Language",
    "\u4E2D\u6587" = "Chinese",
    "\u5BFC\u5165\u6570\u636E" = "Import data",
    "\u5173\u952E\u53C2\u6570" = "Key parameters",
    "\u8FD0\u884C\u68C0\u6D4B" = "Run detection",
    "\u5DEE\u5F02 / \u9A8C\u8BC1" = "Differences / validation",
    "\u5BFC\u51FA\u7ED3\u679C" = "Export results",
    "\u89E3\u91CA\u6CE8\u610F\u4E8B\u9879" = "Interpretation notice",
    "\u6570\u636E\u4E0E QC" = "Data and QC",
    "\u9884\u8BBE\u4E0E\u8FD0\u884C" = "Presets and run",
    "\u7ED3\u679C\u4E0E\u590D\u73B0\u6587\u4EF6" = "Results and reproducibility files",
    "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE" = "Aligned timestamp plot",
    "\u539F\u59CB\u65F6\u95F4\u6233\u56FE" = "Raw timestamp plot",
    "ISI \u65F6\u95F4\u5256\u9762" = "ISI time profile",
    "ISI \u72B6\u6001\u7A7A\u95F4" = "ISI state space",
    "ISI \u72B6\u6001\u7A7A\u95F4\u5206\u6790" = "ISI state-space analysis",
    "\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8" = "Event-Aligned Activity",
    "\u795E\u7ECF\u6D41\u5F62" = "Neural Manifold",
    "PCA \u8F68\u8FF9" = "PCA trajectory",
    "Isomap \u8F68\u8FF9" = "Isomap trajectory",
    "\u6838\u5FC3\u8BC1\u636E" = "Core evidence",
    "\u63A2\u7D22\u5C42" = "Exploration",
    "\u6A21\u578B\u5C42" = "Model layer",
    "3D \u81EA\u7531\u65CB\u8F6C" = "3D",
    "\u7279\u5F81\u8868" = "Feature table",
    "\u8DE8 train \u7EDF\u8BA1" = "Train-level statistics",
    "\u70B9\u989C\u8272\u6807\u7B7E" = "point-color labels",
    "\u6700\u7EC8\u6807\u7B7E" = "final labels",
    "MANUAL \u4F18\u5148" = "MANUAL priority",
    "\u5C40\u90E8 ISI \u534A\u7A97 k" = "local ISI half-window k",
    "\u72B6\u6001\u7A7A\u95F4 scaling" = "state-space scaling",
    "Winsorize \u6781\u7AEF logISI" = "winsorize extreme logISI",
    "pause / \u957F ISI \u5904\u65AD\u7EBF" = "break lines at pause / long ISI",
    "\u65AD\u7EBF ISI \u9608\u503C\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "line-break ISI threshold (current unit)",
    "\u65F6\u95F4\u8303\u56F4" = "time range",
    "\u5168\u65F6\u957F" = "full duration",
    "\u540C\u6B65 raster \u65F6\u95F4\u7A97" = "sync raster time window",
    "\u81EA\u5B9A\u4E49\u7A97\u53E3" = "custom window",
    "Z \u8F74\uFF083D\uFF09" = "Z axis (3D)",
    "\u8FD1\u90BB\u6570 k" = "neighbors k",
    "\u6700\u5927\u70B9\u6570" = "maximum points",
    "Surrogate \u6B21\u6570" = "Surrogate count",
    "Block shuffle \u5757\u957F\uFF08ISI\u6570\uFF09" = "Block shuffle length (ISI count)",
    "\u63A2\u7D22\u6700\u5927\u70B9\u6570" = "exploration maximum points",
    "GMM \u5019\u9009 state \u6570" = "GMM candidate state count",
    "\u533A\u95F4\u76F4\u65B9\u56FE" = "Interval histogram",
    "\u6570\u636E\u96C6 ISI \u76F4\u65B9\u56FE" = "Dataset ISI histogram",
    "\u7ED3\u6784\u5019\u9009" = "Structure candidates",
    "\u9608\u503C\u9884\u89C8" = "Threshold preview",
    "\u652F\u6301\u65B9\u6CD5" = "Support methods",
    "\u624B\u52A8\u6807\u8BB0 vs \u68C0\u6D4B\u5668\u62A5\u544A" = "Manual labels vs detector report",
    "\u79D1\u5B66\u9A8C\u8BC1" = "Scientific validation",
    "\u6279\u5904\u7406 / API" = "Batch / API",
    "\u65B9\u6CD5 / \u5BA1\u8BA1\u8BF4\u660E" = "Methods / audit notes",
    "\u68C0\u6D4B\u5668 / \u53C2\u6570" = "Detector / parameters",
    "\u81EA\u9002\u5E94 train \u8C03\u53C2" = "Adaptive train tuning",
    "\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B" = "Neural-network model",
    "\u6570\u636E QC" = "Data QC",
    "\u4E8B\u4EF6 / \u8F93\u51FA" = "Events / output",
    "Seed / Bridge \u8BCA\u65AD" = "Seed / Bridge diagnostics",
    "\u7591\u4F3C" = "possible",
    "\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002" = "Please upload at least one dataset.",
    "\u539F\u59CB\u6587\u4EF6\u65F6\u95F4\u5355\u4F4D" = "raw file time unit",
    "\u539F\u59CB CSV \u7B2C\u4E00\u884C\u5305\u542B\u5217\u540D" = "raw CSV first row contains column names",
    "\u4E0A\u4F20\u539F\u59CB\u65F6\u95F4\u6233 CSV \u6587\u4EF6" = "upload raw timestamp CSV file",
    "\u5DF2\u6807\u8BB0\u6587\u4EF6\u65F6\u95F4\u5355\u4F4D" = "annotated file time unit",
    "\u4E0A\u4F20\u5DF2\u6807\u8BB0 CSV \u6587\u4EF6" = "upload annotated CSV file",
    "\u52A0\u8F7D\u5DE5\u4F5C\u533A\uFF08.rds\uFF09" = "load workspace (.rds)",
    "\u4FDD\u5B58\u5DE5\u4F5C\u533A" = "save workspace",
    "burst \u68C0\u6D4B\u4E2D\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u7684\u5904\u7406\u65B9\u5F0F" = "handling strategy for suspected refractory-period ISIs during burst detection",
    "\u5C06 burst \u964D\u7EA7\u4E3A\u53EF\u590D\u6838 possible_burst" = "demote burst to reviewable possible_burst",
    "\u5728\u7591\u4F3C ISI \u5904\u5207\u5206\u5019\u9009" = "split candidates at suspected ISIs",
    "\u6392\u9664\u7591\u4F3C ISI \u5E76\u91CD\u65B0\u8BC4\u4F30\u7247\u6BB5" = "exclude suspected ISIs and reevaluate segments",
    "\u62D2\u7EDD\u6574\u4E2A burst \u5019\u9009" = "reject the entire burst candidate",
    "\u6807\u8BB0\u53EF\u80FD\u5B58\u5728\u591A\u5355\u5143\u6C61\u67D3" = "mark possible multi-unit contamination",
    "\u9AD8\u7EA7 QC\uFF1A\u91CD\u590D timestamp \u4E0E\u5408\u5E76" = "Advanced QC: duplicate timestamps and merging",
    "\u6BCF\u6761 spike train \u5185\u91CD\u590D\u65F6\u95F4\u6233\u7684\u5904\u7406\u7B56\u7565" = "handling policy for duplicate timestamps within each spike train",
    "\u62A5\u9519\uFF1A\u4FDD\u6301\u4E0D\u53D8" = "error: keep unchanged",
    "\u8B66\u544A\uFF1A\u4FDD\u6301\u4E0D\u53D8" = "warning: keep unchanged",
    "\u5408\u5E76\u5B8C\u5168\u91CD\u590D\u65F6\u95F4\u6233" = "merge exact duplicate timestamps",
    "\u5408\u5E76\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u7684\u91CD\u590D timestamp" = "merge duplicate timestamps in the current dataset",
    "\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u4E2D\u7684\u91CD\u590D timestamp" = "merge duplicate timestamps in all datasets",
    "\u57FA\u5E95\u795E\u7ECF\u8282 spike train \u5DE5\u4F5C\u53F0\uFF1A\u5BFC\u5165\u6570\u636E\u3001\u8BBE\u7F6E\u5173\u952E\u53C2\u6570\u3001\u8FD0\u884C\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\uFF0C\u7136\u540E\u590D\u6838\u5DEE\u5F02\u3001\u9A8C\u8BC1\u548C\u5BFC\u51FA\u3002" =
      "Basal ganglia spike-train workbench: import data, set key parameters, run event-grammar detection, then review differences, validate, and export.",
    "\u672C\u7A0B\u5E8F\u751F\u6210\u5019\u9009\u4E8B\u4EF6\u548C\u53EF\u590D\u6838\u6807\u7B7E\uFF0C\u5E76\u4E0D\u662F\u65E0\u504F\u7684\u6700\u7EC8\u5206\u7C7B\u5668\u3002\u8BF7\u5206\u522B\u62A5\u544A\u9AD8\u7F6E\u4FE1\u4E8B\u4EF6\u3001\u5F85\u590D\u6838\u5019\u9009\u4E8B\u4EF6\u548C burst-family \u6C47\u603B\u7ED3\u679C\u3002" =
      "This program generates candidate events and reviewable labels; it is not an unbiased final classifier. Report high-confidence events, review candidates, and burst-family summaries separately.",
    "CSV / RDS\uFF0CQC \u548C\u6570\u636E\u96C6\u9009\u62E9" = "CSV / RDS, QC, and dataset selection",
    "Basic \u53C2\u6570\u4F18\u5148\uFF0C\u4E13\u5BB6\u9879\u6298\u53E0" = "Basic parameters first; expert options are collapsed",
    "\u9884\u8BBE + \u5F53\u524D train \u5FEB\u901F\u6267\u884C" = "Presets plus fast execution on current trains",
    "delta preview\u3001IoU\u3001\u654F\u611F\u6027" = "Delta preview, IoU, and sensitivity",
    "CSV / ZIP / YAML \u53EF\u590D\u73B0\u8BB0\u5F55" = "Reproducible CSV / ZIP / YAML records",
    "\u4F2A\u8FF9\u9608\u503C\u662F\u786C\u6027\u7684\u6700\u5C0F\u6709\u6548 ISI\u3002\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u4ECD\u4FDD\u7559\u4E3A\u6709\u6548 ISI\uFF0C\u4F46\u4F1A\u88AB\u6807\u8BB0\u4E3A\u53EF\u7591\uFF1B\u9ED8\u8BA4\u7B56\u7565\u4F1A\u5C06\u53D7\u5F71\u54CD\u7684 burst \u5019\u9009\u964D\u7EA7\u4E3A possible_burst \u4EE5\u4FBF\u590D\u6838\u3002" =
      "The artifact threshold is a hard minimum-valid-ISI gate. Suspected refractory ISIs remain valid but are flagged as suspicious; by default, affected burst candidates are demoted to possible_burst for review.",
    "\u5B8C\u5168\u91CD\u590D timestamp \u4F1A\u4EA7\u751F 0 ISI\u3002\u53EA\u6709\u5728\u786E\u8BA4\u91CD\u590D\u884C\u662F\u5BFC\u51FA\u91CD\u590D\u800C\u975E\u4E0D\u540C\u5355\u4F4D/\u4E8B\u4EF6\u65F6\uFF0C\u624D\u5E94\u5408\u5E76\u3002" =
      "Exact duplicate timestamps produce 0 ISIs. Merge only when you have confirmed that duplicate rows are export duplicates, not different units or events.",
    "\u4E00\u952E\u5408\u5E76\u4F1A\u5220\u9664\u6BCF\u6761 train \u5185\u5B8C\u5168\u91CD\u590D\u7684 spike timestamp\uFF0C\u4EC5\u4FDD\u7559\u7B2C\u4E00\u6B21\u51FA\u73B0\u3002\u4FDD\u7559 spike \u4E0A\u7684 MANUAL \u6807\u7B7E\u4F1A\u88AB\u4FDD\u7559\uFF1BAUTO \u7ED3\u679C\u4F1A\u88AB\u6E05\u7A7A\uFF0C\u9700\u8981\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u3002" =
      "One-click merge removes exact duplicate spike timestamps within each train and keeps only the first occurrence. MANUAL labels on retained spikes are preserved; AUTO results are cleared and must be rerun.",
    "\u5E38\u89C4\u4F7F\u7528\u65F6\u4FDD\u6301\u9ED8\u8BA4\u5373\u53EF\u3002\u8FD9\u4E9B\u63A7\u4EF6\u53EA\u5F71\u54CD\u56FE\u50CF\u5448\u73B0\u548C\u5C40\u90E8\u6D4F\u89C8\u3002" =
      "For routine use, keep the defaults. These controls only affect rendering and local browsing.",
    "\u6240\u6709 spike tick \u5747\u7ED8\u5236\u4E3A\u76F8\u540C\u7684\u9ED1\u8272\u5B9E\u7EBF\uFF1B\u6A21\u5F0F/\u6765\u6E90\u4FE1\u606F\u53EA\u901A\u8FC7\u6C34\u5E73\u6761\u5E26\u548C\u53E0\u52A0\u5C42\u663E\u793A\uFF0C\u4E0D\u901A\u8FC7 spike \u989C\u8272\u6DF1\u6D45\u6216\u7C97\u7EC6\u8868\u793A\u3002" =
      "All spike ticks are drawn as identical solid black lines. Pattern/source information is shown only with horizontal bands and overlays, not by spike color or thickness.",
    "Mean-ISI \u4E0E Pasquale logISIH/newBD \u652F\u6301\u5C42\u4EC5\u63D0\u4F9B\u9608\u503C\u8BC1\u636E\uFF1BAUTO \u6807\u7B7E\u4ECD\u7531\u4E3B\u68C0\u6D4B\u5668\u548C\u590D\u6838\u6D41\u7A0B\u63A7\u5236\u3002" =
      "Mean-ISI and Pasquale logISIH/newBD support layers provide threshold evidence only; AUTO labels remain controlled by the main detector and review workflow.",
    "Raster \u6807\u7B7E\u663E\u793A\uFF1A\u4EC5\u4F7F\u7528\u6A21\u5F0F\u989C\u8272\u6761\u5E26\uFF1B\u5782\u76F4 spike tick \u4FDD\u6301\u7EDF\u4E00\u9ED1\u8272\u5B9E\u7EBF\u3002" =
      "Raster label display uses pattern-colored bands only; vertical spike ticks remain uniform solid black lines.",
    "\u9700\u8981\u534A\u76D1\u7763\u6821\u51C6\u6216\u91D1\u6807\u7B7E\u65F6\u518D\u5C55\u5F00\u3002" =
      "Expand this only when you need semi-supervised calibration or gold labels.",
    "Aligned raster\uFF1A\u8BF7\u4F7F\u7528\u6846\u9009\u3002burst/tonic \u9700\u8981\u4ECE\u540C\u4E00\u6761 train \u4E2D\u9009\u62E9\u81F3\u5C11 2 \u4E2A spike\uFF1Bpause/others/high-frequency \u548C NOT-burst \u5F3A\u8D1F\u4F8B\u53EF\u4F7F\u7528\u65F6\u95F4\u8303\u56F4\u9009\u62E9\u3002" =
      "Aligned raster: use box select. burst/tonic require at least 2 spikes from the same train; pause/others/high-frequency and NOT-burst hard negatives can use a time-range selection.",
    "\u89E3\u91CA\uFF1A\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E = \u591A\u4E2A\u77ED ISI \u4E14\u53D8\u5F02\u6027\u4F4E\uFF1B\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E = \u591A\u4E2A\u77ED ISI\uFF0C\u4F46 ISI \u957F\u77ED\u4E0D\u89C4\u5219\u3002" =
      "Interpretation: HF tonic = multiple short ISIs with low variability; HF spiking = multiple short ISIs with irregular ISI lengths.",
    "\u4F7F\u7528\u7C07 A/B \u6BD4\u8F83\u4E24\u4E2A\u89C6\u89C9\u76F8\u4F3C\u7684\u7C07\uFF0C\u67E5\u770B\u4E00\u4E2A\u88AB\u63A5\u53D7\u800C\u53E6\u4E00\u4E2A\u88AB\u62D2\u7EDD\u7684\u539F\u56E0\u3002" =
      "Use cluster A/B to compare two visually similar clusters and inspect why one was accepted while another was rejected.",
    "\u9884\u8BBE\u53EA\u8BBE\u7F6E\u5173\u952E\u7B56\u7565\u9608\u503C\u3002\u5B8C\u6574\u53C2\u6570\u548C params_hash \u4F1A\u5BFC\u51FA\u4EE5\u4FDD\u8BC1\u53EF\u590D\u73B0\u6027\u3002" =
      "Presets only set key strategy thresholds. Full parameters and params_hash are exported for reproducibility.",
    "\u624B\u52A8\u6807\u8BB0\u4F1A\u5199\u5165\u5F53\u524D UI \u53C2\u6570\uFF0C\u5E76\u5C06\u4E8B\u4EF6\u8BED\u6CD5\u9608\u503C\u8BBE\u4E3A MANUAL \u4F18\u5148\u3002" =
      "Manual labels are written into the current UI parameters and make event-grammar thresholds MANUAL-priority.",
	    "PCA / Isomap \u5750\u6807\u53EA\u4F7F\u7528 label-free ISI \u7ED3\u6784\u7279\u5F81\uFF1Bburst / pause / HF \u7B49\u6807\u7B7E\u53EA\u4F5C\u4E3A\u56FE\u4E0A\u989C\u8272\u53E0\u52A0\u3002" =
	      "PCA / Isomap coordinates use only label-free ISI structure features; burst / pause / HF labels are color overlays only.",
	    "Isomap \u4F7F\u7528\u540C\u4E00\u5957 label-free ISI \u7279\u5F81\uFF1B\u70B9\u6570\u8FC7\u591A\u65F6\u6309\u65F6\u95F4\u5747\u5300\u62BD\u6837\u3002k \u592A\u5C0F\u53EF\u80FD\u65AD\u56FE\uFF0Ck \u592A\u5927\u53EF\u80FD\u628A\u4E0D\u540C\u72B6\u6001\u8D70\u6377\u5F84\u8FDE\u5728\u4E00\u8D77\u3002" =
	      "Isomap uses the same label-free ISI features. If there are too many points, they are sampled evenly over time. Too small a k can disconnect the graph; too large a k can create shortcuts between states.",
	    "2D / 3D \u8F74\u9009\u62E9" = "2D / 3D axis selection",
	    "X \u8F74" = "X axis",
	    "Y \u8F74" = "Y axis",
	    "Z \u8F74\uFF08\u4EC5 3D\uFF09" = "Z axis (3D only)",
	    "Isomap \u8BBE\u7F6E" = "Isomap settings",
	    "Isomap 2D X \u8F74" = "Isomap 2D X axis",
	    "Isomap 2D Y \u8F74" = "Isomap 2D Y axis",
	    "Isomap 3D X \u8F74" = "Isomap 3D X axis",
	    "Isomap 3D Y \u8F74" = "Isomap 3D Y axis",
	    "Isomap 3D Z \u8F74" = "Isomap 3D Z axis",
	    "\u65F6\u95F4\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "time (current unit)",
	    "ISI\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "ISI (current unit)",
	    "\u5C40\u90E8\u53D1\u653E\u7387 Hz" = "local firing rate Hz",
	    "\u5C40\u90E8 CV2" = "local CV2",
	    "\u5C40\u90E8 LV" = "local LV",
	    "\u591A train \u6A21\u5F0F\u4E3A\u6BCF\u6761 spike train \u4F7F\u7528\u72EC\u7ACB X-Y \u8F74\u3002\u65F6\u95F4 X \u8F74\u4F7F\u7528\u771F\u5B9E spike \u65F6\u95F4\u6233\uFF08\u79D2\uFF09\uFF1B\u663E\u793A\u5355\u4F4D\u63A7\u5236 Y \u8F74 ISI \u6570\u503C\u3002\u70B9\u51FB ISI \u70B9/\u7EBF\u6BB5\u53EF\u9501\u5B9A\u6C34\u5E73\u53C2\u8003\u7EBF\u3002" =
	      "Multi-train mode uses independent X-Y axes for each spike train. The time X axis uses real spike timestamps in seconds; the display unit controls Y-axis ISI values. Click an ISI point/segment to lock a horizontal reference line.",
    "\u9ED8\u8BA4\u53EA\u5C55\u793A\u751F\u7269\u5B66\u7528\u6237\u6700\u5E38\u8C03\u7684 Basic \u53C2\u6570\uFF1BAdvanced / Expert \u4ECD\u53EF\u901A\u8FC7\u4E0A\u65B9\u7B5B\u9009\u8BBF\u95EE\u3002" =
      "By default, only the Basic parameters most often adjusted by biology users are shown; Advanced / Expert remain available via the filter above.",
    "\u9884\u89C8\u662F dry-run\uFF1A\u53EA\u6BD4\u8F83 AUTO \u4E8B\u4EF6\u5DEE\u5F02\uFF0C\u4E0D\u8986\u76D6\u6B63\u5F0F\u68C0\u6D4B\u7ED3\u679C\u3002" =
      "Preview is a dry run: it only compares AUTO event differences and does not overwrite official detection results.",
    "\u5BFC\u51FA\u6587\u4EF6\u5305\u542B\u5F53\u524D UI \u53C2\u6570\u6811\u3001schema \u7248\u672C\u3001params_hash \u548C\u9A8C\u8BC1\u6458\u8981\u3002\u5BFC\u5165\u540E\u4F1A\u56DE\u586B\u4E13\u7528 UI \u4E0E contract-generated UI\u3002" =
      "The export contains the current UI parameter tree, schema version, params_hash, and validation summary. Importing fills both dedicated UI controls and contract-generated UI.",
    "Round-trip" = "Round-trip",
    "logISI phase portrait" = "logISI phase portrait",
    "Isomap 3D" = "Isomap 3D",
    "Mean-ISI" = "Mean-ISI",
    "LogISI / newBD" = "LogISI / newBD"
  )
}

stpd_i18n_phrase_dictionary <- function() {
  c(
    "\u7206\u53D1\uFF08burst\uFF09" = "burst",
    "\u957F\u7206\u53D1\uFF08long burst\uFF09" = "long burst",
    "\u7591\u4F3C\u7206\u53D1\uFF08possible burst\uFF09" = "possible burst",
    "\u5F3A\u76F4\u53D1\u653E\uFF08tonic\uFF09" = "tonic",
    "\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E\uFF08HF tonic\uFF09" = "HF tonic",
    "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\uFF08HF spiking\uFF09" = "HF spiking",
    "\u6682\u505C\uFF08pause\uFF09" = "pause",
    "\u5176\u4ED6\uFF08others\uFF09" = "others",
    "\u975E\u7206\u53D1 / \u5F3A\u8D1F\u4F8B" = "not burst / hard negative",
    "\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E" = "HF tonic",
    "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E" = "HF spiking",
    "\u7591\u4F3C burst" = "possible burst",
    "\u7591\u4F3C\u7206\u53D1" = "possible burst",
    "\u7591\u4F3C" = "possible",
    "\u8BCA\u65AD" = "diagnostics",
    "\u4E0E" = "and",
    "\u4E2D" = "in",
    "\u5904" = "at",
    "\u7684" = "",
    "\u4E3A" = "to",
    "\u5C06" = "",
    "\u957F\u7206\u53D1" = "long burst",
    "\u7206\u53D1" = "burst",
    "\u5F3A\u76F4" = "tonic",
    "\u6682\u505C" = "pause",
    "\u5176\u4ED6" = "others",
    "\u4E00\u952E\u5408\u5E76" = "one-click merge",
    "\u5B8C\u5168\u91CD\u590D\u65F6\u95F4\u6233" = "exact duplicate timestamps",
    "\u91CD\u590D\u65F6\u95F4\u6233" = "duplicate timestamps",
    "\u91CD\u590D timestamp" = "duplicate timestamps",
    "\u5F53\u524D\u6570\u636E\u96C6" = "current dataset",
    "\u6240\u6709\u6570\u636E\u96C6" = "all datasets",
    "\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6" = "loaded datasets",
    "\u6570\u636E\u96C6\u5C42\u7EA7" = "dataset-level",
    "\u6570\u636E\u96C6" = "dataset",
    "\u539F\u59CB\u65F6\u95F4\u6233" = "raw timestamps",
    "\u65F6\u95F4\u6233" = "timestamps",
    "\u5DE5\u4F5C\u533A" = "workspace",
    "\u539F\u59CB\u6587\u4EF6" = "raw file",
    "\u5DF2\u6807\u8BB0\u6587\u4EF6" = "annotated file",
    "\u4E0A\u4F20" = "upload",
    "\u52A0\u8F7D" = "load",
    "\u4FDD\u5B58" = "save",
    "\u6E05\u7A7A\u5185\u5B58" = "clear memory",
    "\u79FB\u9664" = "remove",
    "\u5BFC\u5165\u6570\u636E" = "import data",
    "\u5BFC\u5165" = "import",
    "\u5BFC\u51FA\u7ED3\u679C" = "export results",
    "\u5BFC\u51FA" = "export",
    "\u4E0B\u8F7D" = "download",
    "\u7ED3\u679C" = "results",
    "\u590D\u73B0\u6587\u4EF6" = "reproducibility files",
    "\u5173\u952E\u53C2\u6570" = "key parameters",
    "\u53C2\u6570\u654F\u611F\u6027" = "parameter sensitivity",
    "\u53C2\u6570\u5DEE\u5F02" = "parameter delta",
    "\u53C2\u6570" = "parameters",
    "\u9884\u8BBE\u4E0E\u8FD0\u884C" = "presets and run",
    "\u5206\u6790\u9884\u8BBE" = "analysis preset",
    "\u9884\u8BBE" = "preset",
    "\u8FD0\u884C\u68C0\u6D4B" = "run detection",
    "\u8FD0\u884C\u68C0\u6D4B\u5668" = "run detector",
    "\u68C0\u6D4B\u5668" = "detector",
    "\u68C0\u6D4B" = "detection",
    "\u5019\u9009\u4E8B\u4EF6" = "candidate events",
    "\u5019\u9009" = "candidate",
    "\u4E8B\u4EF6\u7EA7" = "event-level",
    "\u4E8B\u4EF6" = "events",
    "\u6807\u7B7E" = "labels",
    "\u624B\u52A8\u6807\u8BB0" = "manual labels",
    "\u624B\u52A8" = "manual",
    "\u81EA\u52A8\u68C0\u6D4B" = "auto detection",
    "\u81EA\u52A8" = "auto",
    "\u6700\u7EC8\u6807\u7B7E" = "final labels",
    "\u6700\u7EC8" = "final",
    "\u53EF\u590D\u6838" = "reviewable",
    "\u590D\u6838" = "review",
    "\u9AD8\u7F6E\u4FE1" = "high-confidence",
    "\u5019\u9009\u5BB6\u65CF" = "candidate family",
    "\u6307\u6807\u89E3\u91CA" = "metric interpretation",
    "\u9A8C\u8BC1\u6307\u6807" = "validation metrics",
    "\u9A8C\u8BC1\u5EFA\u8BAE" = "validation guidance",
    "\u79D1\u5B66\u9A8C\u8BC1" = "scientific validation",
    "\u9A8C\u8BC1" = "validation",
    "\u5DEE\u5F02" = "differences",
    "\u5DEE\u5F02\u9884\u89C8" = "delta preview",
    "\u53D8\u66F4\u9884\u89C8" = "change preview",
    "\u9884\u89C8" = "preview",
    "\u5C40\u90E8\u5DEE\u5F02\u91CD\u8DD1" = "local delta rerun",
    "\u91CD\u8DD1" = "rerun",
    "\u654F\u611F\u6027" = "sensitivity",
    "\u6279\u5904\u7406" = "batch processing",
    "\u65B9\u6CD5 / \u5BA1\u8BA1\u8BF4\u660E" = "methods / audit notes",
    "\u5BA1\u8BA1\u8BF4\u660E" = "audit notes",
    "\u5BA1\u8BA1" = "audit",
    "\u8BF4\u660E" = "notes",
    "\u89E3\u91CA\u6CE8\u610F\u4E8B\u9879" = "interpretation notice",
    "\u89E3\u91CA" = "interpretation",
    "\u6CE8\u610F\u4E8B\u9879" = "notice",
    "\u663E\u793A\u5355\u4F4D" = "display unit",
    "\u663E\u793A\u6A21\u5F0F" = "display mode",
    "\u663E\u793A\u5C42\u7EA7" = "display level",
    "\u663E\u793A\u65F6\u95F4\u7A97" = "display time window",
    "\u65F6\u95F4\u8303\u56F4" = "time range",
    "\u65F6\u95F4\u7A97\u53E3" = "time window",
    "\u65F6\u95F4\u7A97" = "time window",
    "\u65F6\u95F4" = "time",
    "\u5168\u65F6\u957F" = "full duration",
    "\u81EA\u5B9A\u4E49\u7A97\u53E3" = "custom window",
    "\u81EA\u5B9A\u4E49\u5256\u9762\u7A97\u53E3" = "custom profile window",
    "\u540C\u6B65 raster \u65F6\u95F4\u7A97" = "sync raster time window",
    "\u540C\u6B65\u4E3B raster \u65F6\u95F4\u7A97" = "sync main raster time window",
    "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE" = "aligned timestamp plot",
    "\u539F\u59CB\u65F6\u95F4\u6233\u56FE" = "raw timestamp plot",
    "\u5BF9\u9F50\u65F6\u95F4" = "aligned time",
    "\u5F53\u524D\u5355\u4F4D" = "current unit",
    "\u5F53\u524D\u663E\u793A\u5355\u4F4D" = "current display unit",
    "\u65F6\u95F4\u5355\u4F4D" = "time unit",
    "\u4F2A\u8FF9/\u4E0D\u5E94\u671F\u9608\u503C\u5355\u4F4D" = "artifact/refractory threshold unit",
    "\u4F2A\u8FF9 / \u6700\u5C0F\u6709\u6548 ISI \u9608\u503C" = "artifact / minimum valid ISI threshold",
    "\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u9608\u503C" = "suspected refractory ISI threshold",
    "\u4F2A\u8FF9" = "artifact",
    "\u4E0D\u5E94\u671F" = "refractory period",
    "\u9608\u503C\u6765\u6E90\u4F18\u5148\u7EA7" = "threshold source priority",
    "\u9608\u503C\u6765\u6E90" = "threshold source",
    "\u5B9E\u9645\u68C0\u6D4B\u9608\u503C" = "actual detection thresholds",
    "\u7528\u6237\u81EA\u5B9A\u4E49\u9608\u503C\u8986\u76D6" = "user threshold override",
    "\u7528\u6237\u9608\u503C" = "user thresholds",
    "\u9608\u503C" = "thresholds",
    "\u6700\u5C0F\u6709\u6548" = "minimum valid",
    "\u6700\u5C0F" = "minimum",
    "\u6700\u5927" = "maximum",
    "\u4E0A\u9650" = "upper bound",
    "\u4E0B\u754C" = "lower bound",
    "\u4E0B\u9650" = "lower bound",
    "\u7EDD\u5BF9\u6700\u5C0F\u503C" = "absolute minimum",
    "\u7EDD\u5BF9\u6700\u5927\u503C" = "absolute maximum",
    "\u767E\u5206\u4F4D\u533A\u95F4" = "percentile range",
    "\u767E\u5206\u4F4D" = "percentile",
    "\u8303\u56F4" = "range",
    "\u95E8\u63A7" = "gate",
    "\u5904\u7406\u65B9\u5F0F" = "handling strategy",
    "\u5904\u7406\u7B56\u7565" = "handling policy",
    "\u4EC5\u8B66\u544A" = "warn only",
    "\u8B66\u544A" = "warning",
    "\u62A5\u9519" = "error",
    "\u4FDD\u6301\u4E0D\u53D8" = "keep unchanged",
    "\u5408\u5E76" = "merge",
    "\u6392\u9664" = "exclude",
    "\u62D2\u7EDD" = "reject",
    "\u964D\u7EA7" = "demote",
    "\u5207\u5206" = "split",
    "\u6807\u8BB0" = "mark",
    "\u53EF\u80FD\u5B58\u5728\u591A\u5355\u5143\u6C61\u67D3" = "possible multi-unit contamination",
    "\u591A\u5355\u5143\u6C61\u67D3" = "multi-unit contamination",
    "\u9AD8\u7EA7 QC" = "advanced QC",
    "\u9AD8\u7EA7" = "advanced",
    "\u4E13\u5BB6\u9879" = "expert options",
    "\u4E13\u5BB6" = "expert",
    "\u663E\u793A\u3001\u7B5B\u9009\u4E0E\u53E0\u52A0\u5C42" = "display, filters, and overlays",
    "\u7B5B\u9009" = "filter",
    "\u8FC7\u6EE4\u5668" = "filter",
    "\u53E0\u52A0\u5C42" = "overlay",
    "\u7ED8\u5236" = "draw",
    "\u9690\u85CF" = "hide",
    "\u59CB\u7EC8" = "always",
    "\u5B8C\u6574\u4EA4\u4E92" = "full interaction",
    "\u7B80\u5316\u60AC\u505C/\u9009\u62E9" = "simplified hover/selection",
    "\u7EC6\u8282\u5C42\u7EA7" = "detail level",
    "\u60AC\u505C" = "hover",
    "\u6846\u9009" = "box select",
    "\u9009\u62E9\u6A21\u5F0F" = "select pattern",
    "\u9009\u62E9" = "select",
    "\u7F13\u5B58\u9009\u62E9" = "cached selection",
    "\u6240\u9009\u533A\u57DF" = "selected region",
    "\u6240\u9009\u7C07" = "selected cluster",
    "\u7C07" = "cluster",
    "\u64A4\u9500\u4E0A\u4E00\u6B21\u624B\u52A8\u64CD\u4F5C" = "undo last manual action",
    "\u64A4\u9500" = "undo",
    "\u6E05\u9664\u5168\u90E8" = "clear all",
    "\u6E05\u9664\u6240\u9009" = "clear selected",
    "\u6E05\u9664" = "clear",
    "\u6821\u6B63" = "correction",
    "\u6821\u51C6" = "calibration",
    "\u91D1\u6807\u7B7E" = "gold labels",
    "\u534A\u76D1\u7763" = "semi-supervised",
    "\u5F53\u524D\u53EF\u89C1" = "currently visible",
    "\u53EF\u89C1" = "visible",
    "\u6BCF\u9875\u53EF\u89C1" = "visible per page",
    "\u5206\u9875\u663E\u793A" = "paged display",
    "\u8BB0\u5F55\u6761\u76EE" = "record entries",
    "\u5143\u6570\u636E" = "metadata",
    "\u5217\u540D\u5206\u7EC4" = "column-name grouping",
    "\u7ED3\u6784" = "structure",
    "\u8F68\u8FF9" = "trajectory",
    "\u6DF1\u5EA6" = "depth",
    "\u5DE6\u53F3\u4FA7" = "left/right side",
    "\u72B6\u6001\u7A7A\u95F4\u5206\u6790" = "state-space analysis",
    "\u72B6\u6001\u7A7A\u95F4" = "state space",
    "\u8F74\u9009\u62E9" = "axis selection",
    "\u70B9\u989C\u8272\u6807\u7B7E" = "point-color labels",
    "\u5C40\u90E8 ISI \u534A\u7A97" = "local ISI half-window",
    "\u5C40\u90E8\u53D1\u653E\u7387" = "local firing rate",
    "\u5C40\u90E8" = "local",
    "\u53D1\u653E\u7387" = "firing rate",
    "\u65AD\u7EBF" = "line break",
    "\u957F ISI" = "long ISI",
    "\u5904\u65AD\u7EBF" = "break lines at",
    "\u6781\u7AEF" = "extreme",
    "\u6700\u5927\u70B9\u6570" = "maximum points",
    "\u8FD1\u90BB\u6570" = "neighbors",
    "\u65F6\u95F4\u5256\u9762" = "time profile",
    "\u5256\u9762\u663E\u793A\u6A21\u5F0F" = "profile display mode",
    "\u5256\u9762\u65F6\u95F4\u8303\u56F4" = "profile time range",
    "\u591A\u9762\u677F\u6700\u5927" = "maximum multi-panel",
    "\u591A\u6761" = "multiple",
    "\u5355\u6761" = "single",
    "\u72EC\u7ACB\u9762\u677F" = "separate panels",
    "\u805A\u7126" = "focus",
    "\u53C2\u8003\u7EBF\u5BB9\u5DEE" = "reference-line tolerance",
    "\u53C2\u8003\u7EBF" = "reference line",
    "\u9501\u5B9A" = "lock",
    "\u9634\u5F71\u663E\u793A\u5DF2\u6807\u8BB0\u533A\u95F4" = "shade labeled intervals",
    "\u5DF2\u6807\u8BB0\u533A\u95F4" = "labeled intervals",
    "\u533A\u95F4" = "interval",
    "\u4FDD\u5B58\u5F53\u524D\u663E\u793A train \u7684\u9608\u503C" = "save thresholds for displayed trains",
    "\u6E05\u9664\u5F53\u524D\u663E\u793A train \u7684\u9608\u503C" = "clear thresholds for displayed trains",
    "\u6E05\u9664\u6240\u6709 train-specific \u9608\u503C" = "clear all train-specific thresholds",
    "\u5355\u6761\u8BB0\u5F55\u9608\u503C" = "single-record thresholds",
    "\u76F4\u65B9\u56FE\u7C7B\u578B" = "histogram type",
    "\u76F4\u65B9\u56FE\u6A21\u5F0F" = "histogram mode",
    "\u76F4\u65B9\u56FE" = "histogram",
    "\u539F\u59CB\u5408\u5E76 ISI" = "raw pooled ISI",
    "\u5E73\u8861\u6BD4\u4F8B" = "balanced proportions",
    "\u5F52\u4E00\u5316" = "normalized",
    "\u5206\u5E03" = "distribution",
    "\u5BF9\u6570\u5C3A\u5EA6" = "log scale",
    "\u663E\u793A\u6A21\u5F0F\u9608\u503C\u533A\u95F4" = "show pattern threshold ranges",
    "\u6765\u6E90" = "source",
    "\u7528\u6237\u8F93\u5165" = "user input",
    "\u5B9E\u9645\u4F7F\u7528" = "effective",
    "\u9ED8\u8BA4\u503C" = "default values",
    "\u9ED8\u8BA4" = "default",
    "\u5199\u5165" = "write to",
    "\u7ED3\u6784\u5B66\u4E60" = "structure learning",
    "\u8868\u578B" = "phenotype",
    "\u5019\u9009\u7C7B\u522B" = "candidate category",
    "\u6392\u5E8F\u4F9D\u636E" = "sort by",
    "\u4E0A\u4E00\u4E2A" = "previous",
    "\u4E0B\u4E00\u4E2A" = "next",
    "\u8DF3\u8F6C\u5230 raster \u4E2D\u9009\u4E2D\u9879" = "jump to selected item in raster",
    "\u5E94\u7528\u9608\u503C\u5E76\u7ACB\u5373\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668" = "apply thresholds and rerun detector immediately",
    "\u5E94\u7528\u5EFA\u8BAE\u9608\u503C" = "apply suggested thresholds",
    "\u63A5\u53D7\u4E3A MANUAL \u6807\u7B7E" = "accept as MANUAL label",
    "\u652F\u6301\u65B9\u6CD5" = "support methods",
    "\u652F\u6301\u5C42" = "support layer",
    "\u9608\u503C\u652F\u6301" = "threshold support",
    "\u652F\u6301" = "support",
    "\u6587\u7AE0\u65B9\u6CD5" = "paper method",
    "\u9762\u677F" = "panel",
    "\u6761\u5E26" = "bands",
    "\u5305\u7EDC" = "envelope",
    "\u9605\u8BFB\u6307\u5357" = "reading guide",
    "\u68C0\u6D4B\u7ED3\u679C" = "detection results",
    "\u6458\u8981" = "summary",
    "\u8BAD\u7EC3\u6570\u636E" = "training data",
    "\u7279\u5F81\u8868\u4F7F\u7528\u7684\u6807\u7B7E" = "label source for feature table",
    "\u673A\u5668\u5B66\u4E60\u6807\u7B7E\u6A21\u5F0F" = "machine-learning label mode",
    "\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B" = "neural-network model",
    "\u6A21\u578B\u6458\u8981" = "model summary",
    "\u8BAD\u7EC3\u6A21\u578B" = "train model",
    "\u52A0\u8F7D\u5DF2\u8BAD\u7EC3\u6A21\u578B" = "load trained model",
    "\u5E94\u7528\u5230\u5F53\u524D\u6570\u636E\u96C6" = "apply to current dataset",
    "\u8BC4\u4F30\u6A21\u5F0F" = "evaluation mode",
    "\u8BC4\u4F30\u6807\u7B7E" = "evaluation labels",
    "\u8BC4\u4F30\u6A21\u578B" = "evaluate model",
    "\u8BC4\u4F30" = "evaluation",
    "\u7279\u5F81\u9884\u89C8" = "feature preview",
    "\u6700\u8FD1\u9884\u6D4B\u8868" = "recent prediction table",
    "\u9690\u85CF\u5355\u5143\u6570" = "hidden units",
    "\u6743\u91CD\u8870\u51CF" = "weight decay",
    "\u6700\u5927\u8FED\u4EE3\u6B21\u6570" = "maximum iterations",
    "\u5E94\u7528\u7F6E\u4FE1\u5EA6\u622A\u65AD" = "apply confidence cutoff",
    "\u4E8B\u4EF6\u8BED\u6CD5\u4FDD\u62A4" = "event-grammar guardrails",
    "\u8BAD\u7EC3" = "training",
    "\u6A21\u578B" = "model",
    "\u6570\u636E QC" = "data QC",
    "\u8D28\u91CF\u68C0\u67E5" = "quality check",
    "\u4F2A\u8FF9 ISI \u8BE6\u60C5" = "artifact ISI details",
    "\u91CD\u590D\u65F6\u95F4\u6233\u8BE6\u60C5" = "duplicate timestamp details",
    "\u4E8B\u4EF6 / \u8F93\u51FA" = "events / output",
    "\u4E8B\u4EF6\u8868\u6765\u6E90" = "event table source",
    "\u91CD\u53E0\u540E\u6700\u5C0F\u89C4\u6A21\u5F3A\u5236\u68C0\u67E5" = "post-overlap minimum-size enforcement",
    "\u6700\u5C0F\u89C4\u6A21" = "minimum size",
    "\u5F3A\u5236\u68C0\u67E5" = "enforcement check",
    "\u6BD4\u8F83" = "comparison",
    "\u624B\u52A8\u6807\u8BB0 vs \u68C0\u6D4B\u5668\u62A5\u544A" = "manual labels vs detector report",
    "\u6DF7\u6DC6\u77E9\u9635\u8BA1\u6570" = "confusion matrix counts",
    "\u624B\u52A8\u4E8B\u4EF6\u7EA7\u91CD\u53E0" = "manual event-level overlap",
    "\u8BAD\u7EC3\u96C6" = "training set",
    "\u9A8C\u8BC1\u96C6" = "validation set",
    "\u5212\u5206" = "split",
    "\u968F\u673A\u79CD\u5B50" = "random seed",
    "\u8FC7\u62DF\u5408\u62A5\u544A" = "overfitting report",
    "\u4E8B\u4EF6\u5339\u914D\u660E\u7EC6" = "event match details",
    "\u53C2\u6570\u53D8\u4F53" = "parameter variant",
    "\u65B9\u6CD5\u62A5\u544A" = "method report",
    "\u9884\u8BBE\u76EE\u5F55" = "preset catalog",
    "\u53C2\u6570\u6CBB\u7406\u6458\u8981" = "parameter governance summary",
    "\u5E73\u7A33\u6027 QC" = "stationarity QC",
    "\u8FC7\u62DF\u5408\u8B66\u544A\u62A5\u544A" = "overfitting warning report",
    "\u8BED\u4E49\u4E00\u81F4\u6027\u62A5\u544A" = "semantic consistency report",
    "\u5019\u9009\u7279\u5F81\u5BA1\u8BA1\u9884\u89C8" = "candidate-feature audit preview",
    "\u6700\u7EC8\u5206\u7C7B\u5BA1\u8BA1\u9884\u89C8" = "final-classification audit preview",
    "\u8FC1\u79FB\u8DEF\u7EBF\u56FE" = "migration roadmap",
    "\u68C0\u6D4B\u5668 / \u53C2\u6570" = "detector / parameters",
    "\u663E\u793A\u5C42\u7EA7" = "display level",
    "\u5237\u65B0\u9A8C\u8BC1" = "refresh validation",
    "\u5DEE\u5F02\u4E8B\u4EF6" = "delta events",
    "\u81EA\u9002\u5E94 train \u8C03\u53C2" = "adaptive train tuning",
    "\u65E7\u7248" = "legacy",
    "\u624B\u52A8\u6807\u8BB0\u5F15\u5BFC\u6821\u51C6" = "manual-label-guided calibration",
    "\u76D1\u7763\u5206\u7C7B\u5668" = "supervised classifier",
    "\u6CDB\u5316\u80FD\u529B" = "generalization",
    "\u5DF2\u4FDD\u5B58" = "saved",
    "\u767E\u5206\u4F4D\u8868" = "percentile table",
    "\u8868" = "table",
    "\u884C\u6570" = "rows",
    "\u663E\u793A\u884C\u6570" = "rows to display",
    "\u5BBD\u5EA6" = "width",
    "\u7EBF\u6027" = "linear",
    "\u5355\u4FA7" = "one-sided",
    "\u8FB9\u754C" = "boundary",
    "\u7EAF\u5EA6" = "purity",
    "\u52A8\u6001\u4F18\u5148\u7EA7" = "dynamic priority",
    "\u5408\u7406" = "reasonable",
    "\u56DE\u9000" = "fallback",
    "\u8FD0\u884C" = "run",
    "\u5E94\u7528" = "apply",
    "\u542F\u7528" = "enable",
    "\u5305\u542B" = "include",
    "\u5F53\u524D" = "current",
    "\u6240\u6709" = "all",
    "\u5168\u90E8" = "all",
    "\u54EA\u4E2A" = "which",
    "\u54EA\u4E9B" = "which",
    "\u7C7B\u522B" = "category",
    "\u6765\u6E90" = "source",
    "\u6A21\u5F0F" = "pattern",
    "\u56FE\u4F8B" = "legend",
    "\u56FE" = "plot",
    "\u8868\u683C" = "table",
    "\u5217" = "columns",
    "\u884C" = "rows",
    "\u6570" = "count",
    "\u503C" = "value",
    "\u5355\u4F4D" = "unit",
    "\u6587\u4EF6" = "file",
    "\u5F53\u524D UI" = "current UI",
    "\u65E0\u91CF\u7EB2" = "dimensionless",
    "\u8F83\u5C11" = "fewer",
    "\u66F4\u5FEB" = "faster"
  )
}

stpd_i18n_assets <- function(default_lang = "zh") {
  exact_json <- stpd_i18n_json_object(stpd_i18n_exact_dictionary())
  phrase_json <- stpd_i18n_json_object(stpd_i18n_phrase_dictionary())
  default_lang <- if (identical(default_lang, "en")) "en" else "zh"
  tagList(
    tags$style(HTML("
      .app-header-main {
        display: flex;
        gap: 16px;
        align-items: flex-start;
        justify-content: space-between;
      }
      .app-title-block {
        min-width: 0;
      }
      .stpd-language-toggle {
        flex: 0 0 auto;
        min-width: 170px;
        padding: 8px 10px;
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #f8fafc;
      }
      .stpd-language-label {
        display: block;
        margin-bottom: 5px;
        color: #475569;
        font-size: 12px;
        font-weight: 800;
      }
      .stpd-language-toggle .form-group {
        margin-bottom: 0;
      }
      .stpd-language-toggle .radio-inline {
        margin-right: 8px;
        color: #1f2937;
        font-size: 12px;
        font-weight: 700;
      }
      @media (max-width: 900px) {
        .app-header-main {
          display: block;
        }
        .stpd-language-toggle {
          margin-top: 10px;
          width: 100%;
        }
      }
    ")),
    tags$script(HTML(paste0("
      (function() {
        const exact = ", exact_json, ";
        const phrases = ", phrase_json, ";
        const defaultLang = ", stpd_i18n_json_quote(default_lang), ";
        const textOriginals = new WeakMap();
        const attrOriginalPrefix = 'data-stpd-i18n-original-';
        const cjkPattern = /[\\u3400-\\u9FFF\\uF900-\\uFAFF]/;
        const attrNames = ['title', 'placeholder', 'aria-label', 'data-original-title'];
        const phraseKeys = Object.keys(phrases).sort(function(a, b) { return b.length - a.length; });
        let currentLang = localStorage.getItem('stpd_ui_language') || defaultLang;
        let applying = false;
        let scheduled = false;

        function trimInfo(text) {
          const leading = (text.match(/^\\s*/) || [''])[0];
          const trailing = (text.match(/\\s*$/) || [''])[0];
          return { leading: leading, core: text.trim(), trailing: trailing };
        }

        function tidyEnglish(text) {
          return text
            .replace(/[\\uFF08]/g, '(')
            .replace(/[\\uFF09]/g, ')')
            .replace(/[\\uFF1A]/g, ': ')
            .replace(/[\\uFF1B]/g, '; ')
            .replace(/[\\uFF0C]/g, ', ')
            .replace(/[\\u3002]/g, '. ')
            .replace(/[\\u201C\\u201D]/g, '\"')
            .replace(/\\s+/g, ' ')
            .replace(/\\s+([,.;:!?%)\\]])/g, '$1')
            .replace(/([([{])\\s+/g, '$1')
            .replace(/\\s*\\/\\s*/g, ' / ')
            .replace(/\\s+-\\s+/g, ' - ')
            .replace(/[\\u3400-\\u9FFF\\uF900-\\uFAFF]+/g, ' ')
            .trim();
        }

        function needsBoundarySpace(ch) {
          return !!ch && /[A-Za-z0-9_\\)\\]\\}\\u3400-\\u9FFF\\uF900-\\uFAFF]/.test(ch);
        }

        function needsForwardSpace(ch) {
          return !!ch && /[A-Za-z0-9_\\(\\[\\{\\u3400-\\u9FFF\\uF900-\\uFAFF]/.test(ch);
        }

        function replacePhraseWithContext(text, key, replacement) {
          let out = '';
          let start = 0;
          let idx = text.indexOf(key, start);
          while (idx !== -1) {
            out += text.slice(start, idx);
            const prev = idx > 0 ? text.charAt(idx - 1) : '';
            const next = text.charAt(idx + key.length);
            let repl = replacement;
            if (repl && needsBoundarySpace(prev) && !/^\\s/.test(repl)) repl = ' ' + repl;
            if (repl && needsForwardSpace(next) && !/\\s$/.test(repl)) repl = repl + ' ';
            out += repl;
            start = idx + key.length;
            idx = text.indexOf(key, start);
          }
          out += text.slice(start);
          return out;
        }

        function translateText(source) {
          if (currentLang !== 'en') return source;
          if (!source || !source.trim()) return source;
          const bits = trimInfo(source);
          if (Object.prototype.hasOwnProperty.call(exact, bits.core)) {
            return bits.leading + exact[bits.core] + bits.trailing;
          }
          let out = bits.core;
          for (const key of phraseKeys) {
            if (out.indexOf(key) !== -1) out = replacePhraseWithContext(out, key, phrases[key]);
          }
          out = tidyEnglish(out);
          return bits.leading + out + bits.trailing;
        }

        function shouldSkipElement(el) {
          if (!el || el.nodeType !== 1) return false;
          if (el.closest('script, style, textarea, pre, code, .dataTable tbody')) return true;
          return false;
        }

        function translateTextNode(node) {
          if (!node || node.nodeType !== Node.TEXT_NODE || !node.nodeValue || !node.nodeValue.trim()) return;
          const parent = node.parentElement;
          if (shouldSkipElement(parent)) return;
          const cur = node.nodeValue;
          let original = textOriginals.get(node);
          if (!original || (cjkPattern.test(cur) && cur !== original)) {
            original = cur;
            textOriginals.set(node, original);
          }
          const next = currentLang === 'en' ? translateText(original) : original;
          if (node.nodeValue !== next) node.nodeValue = next;
        }

        function originalAttrName(attr) {
          return attrOriginalPrefix + attr.replace(/[^A-Za-z0-9_-]/g, '_');
        }

        function translateElementAttrs(el) {
          if (!el || el.nodeType !== 1 || shouldSkipElement(el)) return;
          const attrs = attrNames.slice();
          if (el.matches('input[type=\"button\"], input[type=\"submit\"], input[type=\"reset\"]')) attrs.push('value');
          attrs.forEach(function(attr) {
            if (!el.hasAttribute(attr)) return;
            const storeName = originalAttrName(attr);
            const cur = el.getAttribute(attr);
            let original = el.getAttribute(storeName);
            if (!original || (cur && cjkPattern.test(cur) && cur !== original)) {
              original = cur;
              el.setAttribute(storeName, original);
            }
            const next = currentLang === 'en' ? translateText(original) : original;
            if (cur !== next) el.setAttribute(attr, next);
          });
        }

        function walk(root) {
          if (!root) return;
          if (root.nodeType === Node.TEXT_NODE) {
            translateTextNode(root);
            return;
          }
          if (root.nodeType !== Node.ELEMENT_NODE && root.nodeType !== Node.DOCUMENT_NODE && root.nodeType !== Node.DOCUMENT_FRAGMENT_NODE) return;
          if (root.nodeType === Node.ELEMENT_NODE) translateElementAttrs(root);
          const walker = document.createTreeWalker(root, NodeFilter.SHOW_ELEMENT | NodeFilter.SHOW_TEXT);
          let node;
          while ((node = walker.nextNode())) {
            if (node.nodeType === Node.TEXT_NODE) translateTextNode(node);
            else translateElementAttrs(node);
          }
        }

        function setLanguage(lang) {
          currentLang = lang === 'en' ? 'en' : 'zh';
          localStorage.setItem('stpd_ui_language', currentLang);
          document.documentElement.lang = currentLang === 'en' ? 'en' : 'zh-Hans';
          document.body.classList.toggle('stpd-lang-en', currentLang === 'en');
          document.body.classList.toggle('stpd-lang-zh', currentLang !== 'en');
          applying = true;
          walk(document.body);
          applying = false;
        }

        function scheduleApply() {
          if (applying || scheduled) return;
          scheduled = true;
          window.requestAnimationFrame(function() {
            scheduled = false;
            setLanguage(currentLang);
          });
        }

        function syncToggle() {
          const radios = document.querySelectorAll('input[name=\"ui_language\"]');
          radios.forEach(function(radio) {
            radio.checked = radio.value === currentLang;
          });
        }

        document.addEventListener('change', function(event) {
          const target = event.target;
          if (target && target.name === 'ui_language') {
            setLanguage(target.value);
            syncToggle();
          }
        });

        document.addEventListener('DOMContentLoaded', function() {
          syncToggle();
          setLanguage(currentLang);
          const observer = new MutationObserver(function() {
            scheduleApply();
          });
          observer.observe(document.body, {
            childList: true,
            subtree: true,
            characterData: true,
            attributes: true,
            attributeFilter: attrNames.concat(['value'])
          });
        });

        window.stpdSetLanguage = function(lang) {
          setLanguage(lang);
          syncToggle();
        };
      })();
    ")))
  )
}
