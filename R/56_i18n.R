# Lightweight in-app localization for the Shiny UI.
# The first implementation is deliberately client-side so switching language
# does not rebuild the Shiny page or clear uploaded data.

# Dynamic Shiny outputs cannot rely on the client-side Chinese-to-English
# translator alone: DataTables deliberately skip their body cells and an
# English-first notification cannot be reconstructed in Chinese.  Keep one
# keyed, bilingual catalogue for server-generated prose.  Scientific tokens,
# schema names, identifiers, file names, and raw technical diagnostics are
# inserted as values and are never translated or rewritten here.
stpd_ui_copy_catalog <- function() {
  list(
    qc_table_unavailable = c(
      zh = "\u6682\u65E0\u53EF\u7528\u7684\u6570\u636E\u8D28\u91CF\u68C0\u67E5\u8868\u3002",
      en = "No data-quality table is available."
    ),
    qc_passed = c(
      zh = "\u6570\u636E\u8D28\u91CF\u68C0\u67E5\u901A\u8FC7\u3002",
      en = "Data-quality check passed."
    ),
    qc_warning_summary = c(
      zh = "{n} \u6761 train \u5B58\u5728\u6570\u636E\u8D28\u91CF\u8B66\u544A\u3002\u8BF7\u6253\u5F00\u201C\u6570\u636E QC\u201D\u9875\u67E5\u770B\u8BE6\u60C5\u3002",
      en = "{n} train(s) have data-quality warnings. Open the Data QC tab for details."
    ),
    technical_detail = c(
      zh = "\u6280\u672F\u8BE6\u60C5\uFF1A{detail}",
      en = "Technical details: {detail}"
    ),
    unknown_error = c(zh = "\u672A\u77E5\u9519\u8BEF\u3002", en = "Unknown error."),
    data_loading_default = c(zh = "\u6B63\u5728\u52A0\u8F7D spike train \u6570\u636E", en = "Loading spike-train data"),
    data_loading_raw = c(zh = "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train", en = "Loading raw spike trains"),
    data_loading_labeled = c(zh = "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train", en = "Loading labeled spike trains"),
    data_loading_failed = c(zh = "\u6570\u636E\u52A0\u8F7D\u5931\u8D25", en = "Data loading failed"),
    data_loaded_plot_pending = c(zh = "\u6570\u636E\u5DF2\u8FDB\u5165\u5185\u5B58\uFF0C\u56FE\u5F62\u5373\u5C06\u5237\u65B0", en = "Data are in memory; the plot will refresh shortly"),
    data_loading_plot_pending = c(zh = "\u6B63\u5728\u52A0\u8F7D spike train \u5E76\u51C6\u5907\u56FE\u5F62", en = "Loading spike trains and preparing the plot"),
    preparing_raw_csv = c(zh = "\u51C6\u5907\u8BFB\u53D6 {n} \u4E2A\u539F\u59CB CSV \u6587\u4EF6", en = "Preparing to read {n} raw CSV file(s)"),
    preparing_labeled_csv = c(zh = "\u51C6\u5907\u8BFB\u53D6 {n} \u4E2A\u5DF2\u6807\u8BB0 CSV \u6587\u4EF6", en = "Preparing to read {n} labeled CSV file(s)"),
    reading_raw_csv = c(zh = "\u8BFB\u53D6\u539F\u59CB CSV {index}/{total}\uFF1A{name}", en = "Reading raw CSV {index}/{total}: {name}"),
    reading_labeled_csv = c(zh = "\u8BFB\u53D6\u5DF2\u6807\u8BB0 CSV {index}/{total}\uFF1A{name}", en = "Reading labeled CSV {index}/{total}: {name}"),
    skipped_duplicate_dataset = c(zh = "\u5DF2\u8DF3\u8FC7\u91CD\u590D\u6570\u636E\u96C6\uFF1A{name}", en = "Skipped duplicate dataset: {name}"),
    raw_file_loading_failed = c(zh = "\u539F\u59CB\u6587\u4EF6\u52A0\u8F7D\u5931\u8D25", en = "Raw-file loading failed"),
    labeled_file_loading_failed = c(zh = "\u5DF2\u6807\u8BB0\u6587\u4EF6\u52A0\u8F7D\u5931\u8D25", en = "Labeled-file loading failed"),
    parsed_trains_building_dataset = c(zh = "\u5DF2\u89E3\u6790 {n} \u6761 train\uFF0C\u6B63\u5728\u6784\u5EFA\u6570\u636E\u96C6", en = "Parsed {n} train(s); building the dataset"),
    running_import_qc = c(zh = "\u6B63\u5728\u8FD0\u884C\u5BFC\u5165 QC", en = "Running import QC"),
    refreshing_dataset_and_plot = c(zh = "\u6B63\u5728\u5237\u65B0\u6570\u636E\u96C6\u9009\u62E9\u548C\u56FE\u5F62\u7A97\u53E3", en = "Refreshing the dataset selector and plot window"),
    syncing_display_window = c(zh = "\u6B63\u5728\u540C\u6B65\u663E\u793A\u65F6\u95F4\u7A97", en = "Synchronizing the display time window"),
    task_events_recognized = c(zh = "\u5DF2\u8BC6\u522B {n} \u4E2A\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6\u65F6\u95F4\u6233\uFF08\u4E0D\u53C2\u4E0E\u6838\u5FC3\u68C0\u6D4B\uFF09\u3002", en = "Recognized {n} task/behavior event timestamp(s); they do not participate in core detection."),
    data_load_complete = c(zh = "\u52A0\u8F7D\u5B8C\u6210", en = "Loading complete"),
    raw_datasets_loaded = c(zh = "\u5DF2\u52A0\u8F7D {loaded} \u4E2A\u539F\u59CB\u6570\u636E\u96C6\uFF0C\u5931\u8D25 {failed} \u4E2A\u3002", en = "Loaded {loaded} raw dataset(s); {failed} failed."),
    labeled_datasets_loaded = c(zh = "\u5DF2\u52A0\u8F7D {loaded} \u4E2A\u5DF2\u6807\u8BB0\u6570\u636E\u96C6\uFF0C\u5931\u8D25 {failed} \u4E2A\u3002", en = "Loaded {loaded} labeled dataset(s); {failed} failed."),
    no_new_datasets = c(zh = "\u6CA1\u6709\u65B0\u6570\u636E\u96C6\u8FDB\u5165\u5185\u5B58\u3002\u8BF7\u68C0\u67E5\u6587\u4EF6\u683C\u5F0F\u3001\u91CD\u590D\u6570\u636E\u96C6\u6216\u9519\u8BEF\u901A\u77E5\u3002", en = "No new dataset entered memory. Check the file format, duplicate datasets, or error notifications."),
    no_new_dataset_title = c(zh = "\u672A\u52A0\u8F7D\u65B0\u6570\u636E\u96C6", en = "No new dataset loaded"),
    no_target_trains = c(
      zh = "\u6CA1\u6709\u53EF\u7528\u4E8E\u8FD0\u884C\u68C0\u6D4B\u5668\u7684\u76EE\u6807 train\u3002",
      en = "No target trains are available for detector execution."
    ),
    no_selected_trains = c(
      zh = "\u672A\u9009\u62E9\u7528\u4E8E\u8FD0\u884C\u68C0\u6D4B\u5668\u7684 train\u3002",
      en = "No trains are selected for detector execution."
    ),
    detector_scope_specified = c(zh = "\u6307\u5B9A train\uFF1A{n} \u6761", en = "specified trains: {n}"),
    detector_scope_selected = c(zh = "\u4EC5\u6240\u9009 train\uFF1A{n} \u6761", en = "selected trains only: {n}"),
    detector_scope_all = c(zh = "\u5168\u90E8 train\uFF1A{n} \u6761", en = "all trains: {n}"),
    detector_rebuild_complete = c(
      zh = "\u68C0\u6D4B\u5668\u3001\u8BCA\u65AD\u8868\u3001\u5019\u9009\u8D26\u672C\u548C\u7ED3\u679C\u5C42\u5DF2\u91CD\u5EFA",
      en = "Detector, diagnostics, candidate ledger, and result layers rebuilt"
    ),
    detector_progress_prepare = c(zh = "\u6B63\u5728\u8FDB\u884C\u68C0\u6D4B\u524D QC", en = "Running pre-detection QC"),
    detector_progress_thresholds = c(zh = "\u6B63\u5728\u89E3\u6790 dataset/MANUAL \u9608\u503C", en = "Resolving dataset/MANUAL thresholds"),
    detector_progress_train_start = c(zh = "\u6B63\u5728\u68C0\u6D4B train {index}/{total}\uFF1A{train}", en = "Detecting train {index}/{total}: {train}"),
    detector_progress_train_done = c(zh = "\u5DF2\u5B8C\u6210 train {index}/{total}\uFF1A{train}", en = "Finished train {index}/{total}: {train}"),
    detector_progress_assemble_events = c(zh = "\u6B63\u5728\u91CD\u5EFA\u4E8B\u4EF6\u8868", en = "Rebuilding event tables"),
    detector_progress_diagnostics = c(zh = "\u6B63\u5728\u6C47\u603B\u8BCA\u65AD\u8868", en = "Collecting diagnostics"),
    detector_progress_ledger = c(zh = "\u6B63\u5728\u91CD\u5EFA candidate/event ledger", en = "Rebuilding candidate/event ledgers"),
    detector_progress_features = c(zh = "\u6B63\u5728\u8BA1\u7B97\u5019\u9009\u7279\u5F81", en = "Computing candidate features"),
    detector_progress_final_audits = c(zh = "\u6B63\u5728\u8BA1\u7B97\u6700\u7EC8\u5206\u7C7B\u5BA1\u8BA1", en = "Computing final classification audits"),
    detector_progress_report_tables = c(zh = "\u6B63\u5728\u751F\u6210\u9A8C\u8BC1\u4E0E\u62A5\u544A\u8868", en = "Building validation and report tables"),
    detector_progress_public_ledgers = c(zh = "\u6B63\u5728\u540C\u6B65\u516C\u5171\u5019\u9009\u4E0E\u4E8B\u4EF6\u5BA1\u8BA1\u8868", en = "Synchronizing public candidate and event audits"),
    detector_progress_public_features = c(zh = "\u6B63\u5728\u540C\u6B65\u516C\u5171\u5019\u9009\u7279\u5F81", en = "Synchronizing public candidate features"),
    detector_progress_public_final = c(zh = "\u6B63\u5728\u540C\u6B65\u516C\u5171\u6700\u7EC8\u51B3\u7B56", en = "Synchronizing public final decisions"),
    detector_progress_distributional_evidence = c(zh = "\u6B63\u5728\u8BA1\u7B97\u5206\u5E03\u8BC1\u636E\u548C train \u7EA7 phenotype \u6458\u8981", en = "Computing distributional evidence and firing-phenotype summaries"),
    detector_progress_public_reports = c(zh = "\u6B63\u5728\u751F\u6210\u4E00\u81F4\u6027\u548C\u9A8C\u8BC1\u6458\u8981", en = "Computing consistency and validation summaries"),
    detector_progress_public_complete = c(zh = "\u516C\u5171\u68C0\u6D4B\u8F93\u51FA\u5DF2\u5C31\u7EEA", en = "Public detector outputs are ready"),
    detector_progress_complete = c(zh = "\u68C0\u6D4B\u7ED3\u679C\u5C42\u5DF2\u540C\u6B65", en = "Detector result layers are synchronized"),
    detector_progress_default = c(zh = "\u6B63\u5728\u8FD0\u884C\u68C0\u6D4B\u5668", en = "Running the detector"),
    detector_complete = c(
      zh = paste0(
        "\u68C0\u6D4B\u5668\u5DF2\u5B8C\u6210\uFF08{scope}\uFF09\u3002AUTO \u6807\u7B7E\u3001\u8BCA\u65AD\u8868\u3001\u5019\u9009\u8D26\u672C\u548C\u7ED3\u679C\u5C42\u5DF2\u540C\u6B65\u3002",
        "\u4E8B\u4EF6\u8BA1\u6570\uFF1Aburst={burst}\uFF0Clong_burst={long_burst}\uFF0Cpossible_burst={possible_burst}\uFF0C",
        "pause={pause}\uFF0Ctonic={tonic}\uFF0Chf_tonic={hf_tonic}\uFF0Chf_spiking={hf_spiking}\u3002"
      ),
      en = paste0(
        "Detector complete ({scope}). AUTO labels, diagnostics, candidate ledger, and result layers are synchronized. ",
        "Event counts: burst={burst}, long_burst={long_burst}, possible_burst={possible_burst}, pause={pause}, ",
        "tonic={tonic}, hf_tonic={hf_tonic}, hf_spiking={hf_spiking}."
      )
    ),
    validating_formal_export = c(
      zh = "\u6B63\u5728\u9A8C\u8BC1\u6B63\u5F0F\u5BFC\u51FA\u95E8\u63A7\u3002",
      en = "Validating the formal export gate."
    ),
    validating_current_run = c(
      zh = "\u6B63\u5728\u9A8C\u8BC1\u5F53\u524D\u5B8C\u6574\u8FD0\u884C\u7684\u8EAB\u4EFD\u548C\u8303\u56F4\u3002",
      en = "Validating current full-run identity and scope."
    ),
    preparing_labeled_trains = c(
      zh = "\u6B63\u5728\u4E3A {n} \u6761 train \u51C6\u5907\u6807\u7B7E\u5217\u3002",
      en = "Preparing labeled columns for {n} train(s)."
    ),
    formal_export_empty = c(
      zh = "\u6B63\u5F0F\u5BFC\u51FA\u672A\u751F\u6210\u975E\u7A7A\u6587\u4EF6\u3002",
      en = "Formal export did not generate a non-empty file."
    ),
    formal_zip_failed = c(
      zh = "\u6B63\u5F0F ZIP \u9A8C\u8BC1\u5931\u8D25\uFF08{code}\uFF09\uFF1A{detail}",
      en = "Formal ZIP verification failed ({code}): {detail}"
    ),
    writing_detector_artifacts = c(
      zh = "\u6B63\u5728\u5199\u5165\u68C0\u6D4B\u5668\u5019\u9009\u548C\u8BCA\u65AD\u4EA7\u7269\u3002",
      en = "Writing detector candidate and diagnostic artifacts."
    ),
    writing_provenance = c(
      zh = "\u6B63\u5728\u5199\u5165\u6709\u6548\u53C2\u6570\u3001\u8FD0\u884C\u5143\u6570\u636E\u3001\u8B66\u544A\u548C\u6EAF\u6E90\u4FE1\u606F\u3002",
      en = "Writing effective parameters, run metadata, warnings, and provenance."
    ),
    formal_labeled_csv = c(zh = "\u6B63\u5F0F\u5DF2\u6807\u8BB0 CSV", en = "Formal labeled CSV"),
    formal_results_zip = c(zh = "\u6B63\u5F0F\u7ED3\u679C ZIP", en = "Formal results ZIP"),
    formal_export_ready_detail = c(
      zh = "\u6B63\u5F0F\u5BFC\u51FA\u95E8\u63A7\u5DF2\u901A\u8FC7\uFF1B\u5F53\u524D\u5B8C\u6574\u8FD0\u884C\u3001\u53C2\u6570\u4E0E\u68C0\u6D4B\u4EA7\u7269\u8EAB\u4EFD\u53EF\u9A8C\u8BC1\u3002",
      en = "The formal export gate passed; the current full run, parameters, and detector-output identity are verifiable."
    ),
    formal_export_blocked_detail = c(
      zh = "\u5F53\u524D\u8FD0\u884C\u672A\u901A\u8FC7\u6B63\u5F0F\u5BFC\u51FA\u95E8\u63A7\uFF08code={code}\uFF09\u3002\u8BF7\u6839\u636E\u68C0\u6D4B\u72B6\u6001\u5B8C\u6210\u5168\u91CF\u8FD0\u884C\u6216\u91CD\u65B0\u8FD0\u884C\u3002",
      en = "The current run did not pass the formal export gate (code={code}). Complete or rerun a full detection according to the detector status."
    ),
    formal_export_progress_detail = c(
      zh = "\u5BFC\u51FA\u9636\u6BB5\uFF1A{phase}",
      en = "Export phase: {phase}"
    ),
    no_task_events = c(
      zh = "\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u672A\u627E\u5230\u5185\u5D4C\u7684\u4EFB\u52A1\u4E8B\u4EF6\u5217\u3002",
      en = "No embedded task-event columns were found in the current dataset."
    ),
    no_valid_profile_isi = c(
      zh = "\u6240\u9009\u5256\u9762\u7A97\u53E3\u4E2D\u6CA1\u6709\u6709\u6548 ISI\u3002",
      en = "No valid ISIs are available in the selected profile window."
    ),
    select_spike_train = c(
      zh = "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 spike train / neuron\u3002",
      en = "Select at least one spike train / neuron."
    ),
    select_two_spike_trains = c(
      zh = "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E24\u6761 spike train / neuron\u3002",
      en = "Select at least two spike trains / neurons."
    ),
    select_task_event = c(
      zh = "\u8BF7\u4ECE Event / Event_* \u5217\u4E2D\u52A0\u8F7D\u6216\u9009\u62E9\u81F3\u5C11\u4E00\u4E2A\u4EFB\u52A1\u4E8B\u4EF6\u3002",
      en = "Load or select at least one task event from Event / Event_* columns."
    ),
    event_aligned_failed = c(zh = "\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8\u8BA1\u7B97\u5931\u8D25\u3002", en = "Event-aligned activity failed."),
    behavior_csv_failed = c(zh = "\u884C\u4E3A CSV \u52A0\u8F7D\u5931\u8D25\u3002", en = "Behavior CSV loading failed."),
    trial_event_csv_failed = c(zh = "\u8BD5\u6B21/\u4E8B\u4EF6 CSV \u52A0\u8F7D\u5931\u8D25\u3002", en = "Trial/event CSV loading failed."),
    neural_manifold_insufficient_bins = c(
      zh = "\u6709\u6548\u65F6\u95F4 bin \u592A\u5C11\uFF0C\u65E0\u6CD5\u6784\u5EFA\u795E\u7ECF\u6D41\u5F62\u3002",
      en = "There are not enough valid time bins for a neural manifold."
    ),
    neural_manifold_failed = c(zh = "\u795E\u7ECF\u6D41\u5F62\u8BA1\u7B97\u5931\u8D25\u3002", en = "Neural-manifold computation failed."),
    no_box_points = c(
      zh = "\u5F53\u524D\u6846\u9009\u672A\u6355\u83B7\u4EFB\u4F55\u6709\u9650\u5750\u6807\u70B9\u3002",
      en = "No finite points were captured by the current box selection."
    ),
    box_select_first = c(
      zh = "\u8BF7\u5148\u5728\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE\u4E0A\u4F7F\u7528\u6846\u9009\u5DE5\u5177\u3002",
      en = "Use Box Select on the aligned timestamp plot first."
    ),
    no_train_rows = c(zh = "\u5F53\u524D\u56FE\u4E2D\u6CA1\u6709\u53EF\u7528\u7684 train \u884C\u3002", en = "No train rows are available in the current plot."),
    selection_one_train_row = c(zh = "\u4E00\u6B21\u6846\u9009\u5FC5\u987B\u4F4D\u4E8E\u540C\u4E00\u6761 train \u884C\u5185\u3002", en = "The selection must stay within one train row."),
    selection_one_train = c(zh = "\u4E00\u6B21\u6846\u9009\u5FC5\u987B\u4F4D\u4E8E\u540C\u4E00\u6761 train \u5185\u3002", en = "The selection must stay within one train."),
    train_too_short = c(zh = "\u8BE5 train \u4EC5\u6709 1 \u4E2A\u6216\u66F4\u5C11 spike\u3002", en = "This train has one or fewer spikes."),
    no_isi_in_selection = c(zh = "\u5F53\u524D\u6846\u9009\u672A\u8986\u76D6\u4EFB\u4F55 ISI\u3002", en = "The selection covers no ISI."),
    select_valid_pattern = c(zh = "\u8BF7\u9009\u62E9\u6709\u6548\u6A21\u5F0F\u3002", en = "Select a valid pattern."),
    need_two_spikes = c(zh = "\u81F3\u5C11\u9700\u8981 2 \u4E2A\u4E0D\u540C spike\u3002", en = "At least two different spikes are required."),
    no_isi_from_selection = c(zh = "\u5F53\u524D spike \u6846\u9009\u672A\u751F\u6210\u4EFB\u4F55 ISI\u3002", en = "The current spike selection generates no ISI."),
    cleared_manual_labels = c(zh = "\u5DF2\u6E05\u9664\u6240\u9009\u533A\u57DF\u4E2D\u7684 {n} \u4E2A MANUAL \u6807\u7B7E\u3002", en = "Cleared {n} MANUAL label(s) from the selected region."),
    cleared_auto_labels = c(zh = "\u5DF2\u6E05\u9664\u6240\u9009\u533A\u57DF\u4E2D\u7684 {n} \u4E2A AUTO \u6807\u7B7E\u3002", en = "Cleared {n} AUTO label(s) from the selected region."),
    select_train_burst_range = c(zh = "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train \u7528\u4E8E\u5206\u914D burst-ISI \u8303\u56F4\u3002", en = "Select at least one train for burst-ISI range assignment."),
    invalid_percentile_range = c(zh = "\u767E\u5206\u4F4D\u8303\u56F4\u65E0\u6548\u3002", en = "The percentile range is invalid."),
    select_train_to_clear = c(zh = "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761\u8981\u6E05\u9664\u7684 train\u3002", en = "Select at least one train to clear."),
    cleared_burst_range = c(zh = "\u5DF2\u6E05\u9664 {n} \u6761 train \u7684 train-specific burst-ISI \u8303\u56F4\u3002", en = "Cleared the train-specific burst-ISI range for {n} train(s)."),
    no_manual_burst_isi = c(zh = "\u672A\u627E\u5230 MANUAL burst ISI\u3002\u8BF7\u5148\u6807\u8BB0\u82E5\u5E72 burst\uFF0C\u518D\u5B66\u4E60\u8303\u56F4\u3002", en = "No MANUAL burst ISIs were found. Label several bursts before learning ranges."),
    no_manual_tonic_isi = c(zh = "\u672A\u627E\u5230 MANUAL tonic ISI\u3002\u8BF7\u5148\u6807\u8BB0 tonic \u65F6\u6BB5\uFF0C\u518D\u5B66\u4E60\u8303\u56F4\u3002", en = "No MANUAL tonic ISIs were found. Label tonic periods before learning ranges."),
    no_manual_pause_isi = c(zh = "\u672A\u627E\u5230 MANUAL pause ISI\u3002\u8BF7\u5148\u6807\u8BB0 pause \u533A\u95F4\uFF0C\u518D\u5B66\u4E60\u8303\u56F4\u3002", en = "No MANUAL pause ISIs were found. Label pause intervals before learning ranges."),
    no_manual_highfreq_isi = c(zh = "\u672A\u627E\u5230 MANUAL high-frequency tonic/spiking ISI\u3002\u8BF7\u5148\u6807\u8BB0\u9AD8\u9891\u65F6\u6BB5\uFF0C\u518D\u5B66\u4E60\u951A\u70B9\u3002", en = "No MANUAL high-frequency tonic/spiking ISIs were found. Label high-frequency periods before learning anchors."),
    no_estimated_params = c(zh = "\u5C1A\u65E0\u53EF\u7528\u7684\u4F30\u8BA1\u53C2\u6570\u3002", en = "No estimated parameters are available yet."),
    no_near_miss_generated = c(zh = "\u5C1A\u672A\u751F\u6210 near-miss \u5019\u9009\u3002\u8BF7\u5148\u8FD0\u884C\u68C0\u6D4B\u5668\u3002\u5982\u679C\u4ECD\u4E3A\u7A7A\uFF0C\u5219\u5F53\u524D\u6570\u636E/\u53C2\u6570\u53EF\u80FD\u6CA1\u6709\u63A5\u8FD1\u9608\u503C\u8FB9\u754C\u7684\u5019\u9009\uFF0C\u6216 near-miss \u751F\u6210\u9650\u5236\u8FC7\u7A84\u3002", en = "No near-miss candidates have been generated. Run the detector first. If this remains empty, the current data/parameters have no candidates close to a threshold boundary, or the near-miss generation limits are too narrow."),
    no_near_miss_after_filter = c(zh = "\u5DF2\u751F\u6210 {total} \u4E2A near-miss \u5019\u9009\uFF0C\u4F46\u5F53\u524D\u7B5B\u9009\u7ED3\u679C\u4E3A 0\u3002\u7B5B\u9009\uFF1Apattern={pattern}\uFF0Ccategory={category}\uFF0Cparameter={parameter}\uFF0Crelative_change <= {relative}\u3002\u53EF\u5C06 pattern/category/parameter \u8BBE\u4E3A all\uFF0C\u6216\u63D0\u9AD8\u5141\u8BB8\u7684\u76F8\u5BF9\u8C03\u6574\u3002", en = "Generated {total} near-miss candidate(s), but none match the current filters. Filters: pattern={pattern}, category={category}, parameter={parameter}, relative_change <= {relative}. Set pattern/category/parameter to all, or increase the allowed relative adjustment."),
    near_miss_count = c(zh = "near-miss \u5019\u9009\uFF1A\u7B5B\u9009\u540E {filtered} \u4E2A / \u603B\u8BA1 {total} \u4E2A\u3002", en = "Near-miss candidates: {filtered} filtered / {total} total."),
    no_near_miss_selected = c(zh = "\u5C1A\u672A\u9009\u62E9 near-miss \u5019\u9009\u3002", en = "No near-miss candidate is selected."),
    candidate_train_not_found = c(zh = "\u627E\u4E0D\u5230\u6240\u9009\u5019\u9009\u5BF9\u5E94\u7684 train\u3002", en = "The train for the selected candidate was not found."),
    invalid_candidate_isi_range = c(zh = "\u5019\u9009 ISI \u8303\u56F4\u65E0\u6548\u3002", en = "The candidate ISI range is invalid."),
    no_applicable_near_miss_threshold = c(zh = "\u5C1A\u672A\u9009\u62E9\u53EF\u5E94\u7528\u7684 near-miss \u9608\u503C\u3002", en = "No applicable near-miss threshold is selected."),
    detector_runner_unavailable = c(zh = "\u5F53\u524D Shiny session \u4E2D\u6CA1\u6709\u53EF\u7528\u7684\u68C0\u6D4B\u5668 runner\u3002", en = "The detector runner is not available in the current Shiny session."),
    no_logisi_data = c(zh = "\u6682\u65E0\u53EF\u7528\u7684 logISI \u6570\u636E\u3002", en = "No logISI data are available."),
    bin_width_positive = c(zh = "bin \u5BBD\u5FC5\u987B\u5927\u4E8E 0\u3002", en = "Bin width must be greater than 0."),
    no_finite_values_plot = c(zh = "\u6CA1\u6709\u53EF\u7ED8\u5236\u7684\u6709\u9650\u6570\u503C\u3002", en = "There are no finite values to plot."),
    no_structure_candidates = c(zh = "\u6682\u65E0\u7ED3\u6784\u5019\u9009\u3002\u8BF7\u8FD0\u884C\u68C0\u6D4B\u5668\u6216\u8C03\u6574\u7ED3\u6784\u53C2\u6570\u3002", en = "No structure candidates are available. Run the detector or adjust the structure parameters."),
    no_structure_after_filter = c(zh = "\u5F53\u524D\u7B5B\u9009\u540E\u6CA1\u6709\u7ED3\u6784\u5019\u9009\u3002", en = "No structure candidates remain after the current filter."),
    no_structure_to_plot = c(zh = "\u6CA1\u6709\u53EF\u7ED8\u5236\u7684\u7ED3\u6784\u5019\u9009\u3002", en = "No structure candidates are available to plot."),
    no_finite_core_q = c(zh = "\u6CA1\u6709\u6709\u9650\u7684 core q \u6570\u503C\u3002", en = "No finite core q values are available."),
    no_weighted_core_isi = c(zh = "\u6CA1\u6709\u52A0\u6743 core ISI \u6570\u503C\u3002", en = "No weighted core ISI values are available."),
    no_structure_seed_diagnostics = c(zh = "\u5C1A\u65E0\u7ED3\u6784 seed \u8BCA\u65AD\u3002\u8BF7\u5148\u8FD0\u884C\u68C0\u6D4B\u5668\u3002", en = "No structure-seed diagnostics are available yet. Run the detector first."),
    no_structure_bridge_diagnostics = c(zh = "\u5C1A\u65E0\u7ED3\u6784 bridge \u8BCA\u65AD\u3002\u8BF7\u5148\u8FD0\u884C\u68C0\u6D4B\u5668\u3002", en = "No structure-bridge diagnostics are available yet. Run the detector first."),
    no_bridge_after_filter = c(zh = "\u5F53\u524D\u7B5B\u9009\u540E\u6CA1\u6709 bridge \u8BCA\u65AD\u884C\u3002", en = "No bridge rows remain after the current filter."),
    no_structure_final_diagnostics = c(zh = "\u5C1A\u65E0\u7ED3\u6784 final-candidate \u8BCA\u65AD\u3002\u8BF7\u5148\u8FD0\u884C\u68C0\u6D4B\u5668\u3002", en = "No structure final-candidate diagnostics are available yet. Run the detector first."),
    no_candidate_after_filter = c(zh = "\u5F53\u524D\u7B5B\u9009\u540E\u6CA1\u6709\u5019\u9009\u884C\u3002", en = "No candidate rows remain after the current filter."),
    no_finite_diagnostics = c(zh = "\u6CA1\u6709\u6709\u9650\u7684\u8BCA\u65AD\u6570\u503C\u3002", en = "No finite diagnostic values are available."),
    no_diagnostics_to_plot = c(zh = "\u6CA1\u6709\u53EF\u7ED8\u5236\u7684\u8BCA\u65AD\u6570\u503C\u3002", en = "No diagnostic values are available to plot."),
    no_seed_candidates = c(zh = "\u6682\u65E0 seed \u5019\u9009\u3002", en = "No seed candidates are available."),
    no_finite_seed_pairs = c(zh = "\u6CA1\u6709\u6709\u9650\u7684 seed q90 percentile / score \u6570\u503C\u5BF9\u3002", en = "No finite seed q90 percentile / score pairs are available."),
    no_bridge_candidates = c(zh = "\u6682\u65E0 bridge \u5019\u9009\u3002", en = "No bridge candidates are available."),
    no_finite_bridge_pairs = c(zh = "\u6CA1\u6709\u6709\u9650\u7684 bridge percentile / ratio \u6570\u503C\u5BF9\u3002", en = "No finite bridge percentile / ratio pairs are available."),
    no_final_burst_candidates = c(zh = "\u6682\u65E0 final burst \u5019\u9009\u3002", en = "No final-burst candidates are available."),
    no_finite_final_pairs = c(zh = "\u6CA1\u6709\u6709\u9650\u7684 final edge contrast / score \u6570\u503C\u5BF9\u3002", en = "No finite final edge-contrast / score pairs are available."),
    no_diagnostic_rows = c(zh = "\u6682\u65E0\u8BCA\u65AD\u884C\u3002", en = "No diagnostic rows are available."),
    no_qc_rows = c(zh = "\u6682\u65E0 QC \u884C\u3002", en = "No QC rows are available."),
    no_artifact_isi = c(zh = "\u5F53\u524D\u9608\u503C\u4E0B\u6CA1\u6709\u4F2A\u8FF9 ISI\u3002", en = "No artifact ISIs fall below the current threshold."),
    evaluating_manual = c(zh = "\u6B63\u5728\u5BF9\u7167 MANUAL \u6807\u7B7E\u8BC4\u4F30\u68C0\u6D4B\u5668", en = "Evaluating the detector against MANUAL labels"),
    no_manual_valid_isi = c(zh = "\u672A\u627E\u5230\u53EF\u7528\u4E8E\u8BC4\u4F30\u7684\u6709\u6548 MANUAL ISI\u3002", en = "No manually labeled valid ISIs were found for evaluation."),
    manual_evaluation_finished = c(zh = "MANUAL \u4E0E\u68C0\u6D4B\u5668\u5BF9\u7167\u8BC4\u4F30\u5DF2\u5B8C\u6210\u3002\u8BF7\u6253\u5F00\u201CMANUAL \u4E0E\u68C0\u6D4B\u5668\u62A5\u544A\u201D\u9875\u67E5\u770B\u3002", en = "Manual-vs-detector evaluation finished. Open the Manual-vs-detector report tab."),
    no_manual_evaluation = c(zh = "\u5C1A\u65E0 MANUAL \u4E0E\u68C0\u6D4B\u5668\u5BF9\u7167\u8BC4\u4F30\u3002", en = "No manual-vs-detector evaluation is available yet."),
    running_scientific_validation = c(zh = "\u6B63\u5728\u8FD0\u884C\u79D1\u5B66\u9A8C\u8BC1\u62A5\u544A", en = "Running the scientific-validation report"),
    scientific_validation_finished = c(zh = "\u79D1\u5B66\u9A8C\u8BC1\u62A5\u544A\u5DF2\u5B8C\u6210\u3002\u8BF7\u6253\u5F00\u201C\u79D1\u5B66\u9A8C\u8BC1\u201D\u9875\u67E5\u770B\u3002", en = "The scientific-validation report is complete. Open the Scientific validation tab."),
    no_scientific_validation = c(zh = "\u5C1A\u65E0\u79D1\u5B66\u9A8C\u8BC1\u62A5\u544A\u3002", en = "No scientific-validation report is available yet."),
    running_parameter_sensitivity = c(zh = "\u6B63\u5728\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF", en = "Running the event-level parameter-sensitivity scan"),
    no_datasets_loaded = c(zh = "\u5C1A\u672A\u52A0\u8F7D\u6570\u636E\u96C6\u3002", en = "No datasets are loaded."),
    detector_not_run_summary = c(zh = "\u5C1A\u65E0\u68C0\u6D4B\u5668\u91CD\u8DD1\u6458\u8981\u3002", en = "No detector rerun summary is available yet."),
    batch_not_started = c(zh = "\u5C1A\u672A\u8FD0\u884C\u6279\u5904\u7406\u3002", en = "Batch processing has not been run yet."),
    batch_running = c(zh = "\u6B63\u5728\u5BF9\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u6279\u91CF\u8FD0\u884C\u68C0\u6D4B\u5668", en = "Batch-running the detector on all loaded datasets"),
    batch_last_run = c(zh = "\u4E0A\u6B21\u6279\u5904\u7406\uFF1A{n} \u4E2A\u6570\u636E\u96C6\uFF0C{time}", en = "Last batch run: {n} dataset(s), {time}"),
    batch_partial = c(
      zh = "\u6279\u5904\u7406\u5DF2\u5B8C\u6210\uFF0C\u4F46\u6709\u6570\u636E\u96C6\u88AB\u8DF3\u8FC7\uFF1A\u6210\u529F {ok}/{total}\uFF0C\u5931\u8D25 {failed}\u3002\u6280\u672F\u8BE6\u60C5\uFF1A{detail}",
      en = "Batch processing completed with skipped datasets: {ok}/{total} succeeded and {failed} failed. Technical details: {detail}"
    ),
    detector_last_run = c(zh = "\u4E0A\u6B21\u68C0\u6D4B\u5668\u8FD0\u884C\uFF1A{time}", en = "Last detector run: {time}"),
    scope_label = c(zh = "\u8303\u56F4\uFF1A{scope}", en = "Scope: {scope}"),
    event_count_change = c(zh = "\u4E8B\u4EF6\u8BA1\u6570\u53D8\u5316\uFF1A", en = "Event count change:"),
    no_parameter_contract_issues = c(zh = "\u672A\u53D1\u73B0\u53C2\u6570 contract \u95EE\u9898\u3002", en = "No parameter-contract issues were found."),
    no_visible_parameter_contract_issues = c(zh = "\u5F53\u524D {level} \u7EA7\u522B\u4E0B\u672A\u53D1\u73B0\u53EF\u89C1\u7684\u53C2\u6570 contract \u95EE\u9898\u3002", en = "No parameter-contract issues are visible at level: {level}."),
    last_yaml_import_summary = c(zh = "\u4E0A\u6B21 YAML \u5BFC\u5165\uFF1A{status} | {name} | {hash} | \u9519\u8BEF={errors}", en = "Last YAML import: {status} | {name} | {hash} | errors={errors}"),
    parameter_validation_summary = c(
      zh = "\u5F53\u524D params_hash\uFF1A{hash}\n\u9A8C\u8BC1\u603B\u8BA1\uFF1A{errors} \u4E2A\u9519\u8BEF\u3001{warnings} \u4E2A\u8B66\u544A\u3001{infos} \u6761\u4FE1\u606F\u3002\n{level} \u7EA7\u522B\u53EF\u89C1\uFF1A\u9519\u8BEF\u59CB\u7EC8\u663E\u793A\uFF1B{visible_warnings} \u4E2A\u8B66\u544A\u3001{visible_infos} \u6761\u4FE1\u606F\u3002{import}",
      en = "Current params_hash: {hash}\nValidation total: {errors} error(s), {warnings} warning(s), {infos} info item(s).\nVisible at {level}: errors are always shown; {visible_warnings} warning(s), {visible_infos} info item(s).{import}"
    ),
    parameter_change_preview_failed = c(zh = "\u53C2\u6570\u53D8\u66F4\u9884\u89C8\u5931\u8D25\u3002{detail}", en = "Parameter-change preview failed. {detail}"),
    current_value = c(zh = "\u5F53\u524D\u503C\uFF1A{value}", en = "Current value: {value}"),
    near_miss_detail = c(
      zh = "\u5019\u9009\uFF1A{id}\n\u6A21\u5F0F/\u7C7B\u522B\uFF1A{pattern} / {category}\ntrain\uFF1A{train} | timestamp {start}-{end} {unit} | \u5BF9\u9F50 {aligned_start}-{aligned_end} {unit}\nspike/ISI\uFF1A{spikes} / {isi} | \u6301\u7EED\u65F6\u95F4\uFF1A{duration} {unit} | \u9891\u7387\uFF1A{rate} Hz | CV/LV/MM\uFF1A{cv} / {lv} / {mm}\n\u53C2\u6570\uFF1A{parameter} ({direction})\n\u5F53\u524D\u503C\uFF1A{current}\n\u6240\u9700\u503C\uFF1A{required}\n\u76F8\u5BF9\u8C03\u6574\uFF1A{relative}%\n\u5931\u8D25\u9879\u6570\uFF1A{failures}\n\u539F\u56E0\uFF1A{reason}\n\u6280\u672F\u8BE6\u60C5\uFF1A{details}",
      en = "Candidate: {id}\nPattern/category: {pattern} / {category}\nTrain: {train} | timestamp {start}-{end} {unit} | aligned {aligned_start}-{aligned_end} {unit}\nSpikes/ISI: {spikes} / {isi} | duration: {duration} {unit} | rate: {rate} Hz | CV/LV/MM: {cv} / {lv} / {mm}\nParameter: {parameter} ({direction})\nCurrent value: {current}\nRequired value: {required}\nRelative adjustment: {relative}%\nFailure count: {failures}\nReason: {reason}\nTechnical details: {details}"
    ),
    current_auto_burst_label = c(zh = "\u5F53\u524D AUTO burst-family \u6807\u7B7E", en = "Current AUTO burst-family label"),
    no_post_overlap_fragments = c(zh = "\u6700\u8FD1\u4E00\u6B21 AUTO \u8FD0\u884C\u672A\u79FB\u9664\u4EFB\u4F55 overlap \u540E\u4F4E\u4E8E\u6700\u5C0F\u5C3A\u5BF8\u7684 fragment\u3002", en = "No post-overlap minimum-size fragments were removed in the latest AUTO run."),
    no_intervals_for_source = c(zh = "\u672A\u627E\u5230 source={source} \u7684\u533A\u95F4\u3002", en = "No intervals were found for source: {source}."),
    run_support_for = c(zh = "\u8BF7\u8FD0\u884C {method} support \u4EE5\u751F\u6210{artifact}\u3002", en = "Run {method} support to generate {artifact}."),
    no_support_artifact = c(zh = "\u5C1A\u65E0 {artifact}\u3002", en = "No {artifact} is available yet."),
    batch_export_unique_ids = c(zh = "\u6279\u91CF\u5BFC\u51FA\u8981\u6C42\u6BCF\u4E2A\u6570\u636E\u96C6 ID \u975E\u7A7A\u4E14\u552F\u4E00\u3002", en = "Batch export requires unique, non-empty dataset IDs."),
    batch_export_gate_failed = c(zh = "\u4E00\u4E2A\u6216\u591A\u4E2A\u6570\u636E\u96C6\u672A\u901A\u8FC7\u6B63\u5F0F\u6279\u91CF\u5BFC\u51FA\u95E8\u63A7\u3002", en = "One or more datasets failed the formal batch-export gate."),
    batch_zip_failed = c(zh = "\u6279\u91CF ZIP \u9A8C\u8BC1\u5931\u8D25\uFF08{code}\uFF09\uFF1A{detail}", en = "Batch ZIP verification failed ({code}): {detail}"),
    no_visible_trains = c(zh = "\u5F53\u524D\u6CA1\u6709\u53EF\u89C1\u7684 spike train\u3002", en = "No spike trains are currently visible."),
    no_support_window_data = c(zh = "support overlay \u7A97\u53E3\u4E2D\u6CA1\u6709 spike/ISI\u3002", en = "There are no spikes/ISIs in the support-overlay window."),
    no_support_strips = c(zh = "\u5C1A\u65E0 support burst ISI \u6761\u5E26\u3002\u8BF7\u5148\u5728\u5DE6\u4FA7\u8FD0\u884C Mean-ISI support \u548C/\u6216 LogISI support\uFF0C\u5E76\u4FDD\u6301\u9009\u4E2D\u5BF9\u5E94\u7684 overlay \u9009\u9879\u3002", en = "No support burst ISI strips are available. Run Mean-ISI support and/or LogISI support in the left panel, and keep the corresponding overlay option selected."),
    no_support_visible_trains = c(zh = "\u5F53\u524D\u53EF\u89C1 train \u4E2D\u6CA1\u6709 support burst ISI \u6761\u5E26\u3002\u53EF\u5173\u95ED\u201C\u4EC5\u5BF9\u5F53\u524D\u53EF\u89C1 train \u8FD0\u884C\u201D\u4EE5\u6269\u5927\u8303\u56F4\uFF0C\u6216\u9009\u62E9\u542B support \u5019\u9009\u7684 train\u3002", en = "No support burst ISI strips are available for the currently visible spike trains. Run support on more trains by turning off 'Run on currently visible trains only', or select trains containing support candidates."),
    no_support_time_window = c(zh = "\u5F53\u524D\u65F6\u95F4\u7A97\u53E3\u4E2D\u6CA1\u6709 support burst ISI \u6761\u5E26\u3002\u53EF\u53D6\u6D88\u201C\u4E0E\u4E3B raster \u65F6\u95F4\u7A97\u53E3\u540C\u6B65\u201D\uFF0C\u6216\u5C06\u4E3B raster \u7F29\u653E\u5230\u5305\u542B support \u5019\u9009\u7684\u7A97\u53E3\u3002", en = "No support burst ISI strips are available in the current time window. Disable synchronization with the main raster window, or zoom the main raster to a window containing support candidates."),
    support_mapping_mismatch = c(zh = "support \u5019\u9009\u5B58\u5728\uFF0C\u4F46\u65E0\u6CD5\u5C06\u5176 ISI \u533A\u95F4\u6620\u5C04\u5230\u53EF\u89C1 spike train\u3002\u8FD9\u901A\u5E38\u8868\u793A support \u7ED3\u679C\u4E2D\u5B58\u5728\u7D22\u5F15/\u65F6\u95F4\u4E0D\u5339\u914D\u3002", en = "Support candidates exist, but their ISI intervals could not be mapped to the visible spike trains. This usually indicates an index/time mismatch in the support result.")
  )
}

stpd_ui_copy <- function(key, lang = "zh", ...) {
  key <- as.character(key %||% "")[1]
  entry <- stpd_ui_copy_catalog()[[key]]
  if (is.null(entry)) stop("Unknown UI-copy key: ", key, call. = FALSE)
  lang <- if (identical(as.character(lang %||% "zh")[1], "en")) "en" else "zh"
  out <- unname(entry[[lang]])
  placeholders <- regmatches(out, gregexpr("\\{[A-Za-z0-9_]+\\}", out, perl = TRUE))[[1]]
  if (length(placeholders) == 1L && identical(placeholders, "")) placeholders <- character()
  required <- unique(substring(placeholders, 2L, nchar(placeholders) - 1L))
  values <- list(...)
  if (length(values) > 0L) {
    if (is.null(names(values)) || any(!nzchar(names(values)))) {
      stop("UI-copy interpolation values must be named.", call. = FALSE)
    }
  }
  missing <- setdiff(required, names(values))
  if (length(missing) > 0L) {
    stop("Missing UI-copy interpolation value(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(placeholders) > 0L) {
    positions <- gregexpr("\\{[A-Za-z0-9_]+\\}", out, perl = TRUE)[[1]]
    lengths <- attr(positions, "match.length")
    pieces <- character()
    cursor <- 1L
    for (ii in seq_along(positions)) {
      start <- positions[[ii]]
      stop <- start + lengths[[ii]] - 1L
      if (start > cursor) pieces <- c(pieces, substr(out, cursor, start - 1L))
      nm <- substr(out, start + 1L, stop - 1L)
      value <- as.character(values[[nm]] %||% "")[1]
      if (is.na(value)) value <- ""
      pieces <- c(pieces, value)
      cursor <- stop + 1L
    }
    if (cursor <= nchar(out, type = "chars")) {
      pieces <- c(pieces, substr(out, cursor, nchar(out, type = "chars")))
    }
    out <- paste0(pieces, collapse = "")
  }
  out
}

stpd_ui_condition_detail <- function(e, lang = "zh") {
  detail <- tryCatch(conditionMessage(e), error = function(err) as.character(e %||% ""))[1]
  if (is.na(detail) || !nzchar(trimws(detail))) detail <- stpd_ui_copy("unknown_error", lang = lang)
  stpd_ui_copy("technical_detail", lang = lang, detail = detail)
}

stpd_ui_dt_language <- function(lang = "zh") {
  if (identical(as.character(lang %||% "zh")[1], "en")) return(list())
  list(
    emptyTable = "\u8868\u4E2D\u6CA1\u6709\u53EF\u7528\u6570\u636E",
    info = "\u663E\u793A\u7B2C _START_ \u81F3 _END_ \u9879\uFF0C\u5171 _TOTAL_ \u9879",
    infoEmpty = "\u6682\u65E0\u53EF\u663E\u793A\u7684\u9879\u76EE",
    infoFiltered = "\uFF08\u4ECE _MAX_ \u9879\u4E2D\u7B5B\u9009\uFF09",
    lengthMenu = "\u6BCF\u9875\u663E\u793A _MENU_ \u9879",
    loadingRecords = "\u6B63\u5728\u52A0\u8F7D\u2026",
    processing = "\u6B63\u5728\u5904\u7406\u2026",
    search = "\u641C\u7D22\uFF1A",
    zeroRecords = "\u672A\u627E\u5230\u5339\u914D\u8BB0\u5F55",
    paginate = list(
      first = "\u9996\u9875", previous = "\u4E0A\u4E00\u9875",
      `next` = "\u4E0B\u4E00\u9875", last = "\u672B\u9875"
    ),
    aria = list(
      sortAscending = "\uFF1A\u6FC0\u6D3B\u540E\u5347\u5E8F\u6392\u5217",
      sortDescending = "\uFF1A\u6FC0\u6D3B\u540E\u964D\u5E8F\u6392\u5217"
    )
  )
}

stpd_ui_localize_dt_filter_html <- function(widget, lang = "zh") {
  if (identical(as.character(lang %||% "zh")[1], "en")) return(widget)
  filter_html <- (widget$x %||% list())$filterHTML
  if (is.null(filter_html) || !length(filter_html)) return(widget)

  filter_html <- gsub(
    "placeholder='All'", "placeholder='\u5168\u90E8'",
    as.character(filter_html), fixed = TRUE
  )
  filter_html <- gsub(
    'placeholder="All"', 'placeholder="\u5168\u90E8"',
    filter_html, fixed = TRUE
  )
  widget$x$filterHTML <- filter_html
  widget
}

stpd_ui_table_copy_dictionary <- function() {
  c(
    "No clusters have been saved. Box-select a cluster in the raster, then click 'Set selected cluster A/B'." = "\u5C1A\u672A\u4FDD\u5B58\u7C07\u3002\u8BF7\u5728 raster \u4E2D\u6846\u9009\u7C07\uFF0C\u7136\u540E\u70B9\u51FB\u201C\u8BBE\u7F6E\u6240\u9009\u7C07 A/B\u201D\u3002",
    "No ISIs match the current filters." = "\u6CA1\u6709 ISI \u7B26\u5408\u5F53\u524D\u8FC7\u6EE4\u6761\u4EF6\u3002",
    "Click 'Run local difference preview' to display the summary." = "\u70B9\u51FB\u201C\u8FD0\u884C\u5C40\u90E8\u5DEE\u5F02\u9884\u89C8\u201D\u540E\u663E\u793A\u6C47\u603B\u3002",
    "No pattern-count differences are available." = "\u6682\u65E0 pattern \u6570\u91CF\u5DEE\u5F02\u3002",
    "No added, removed, relabeled, or boundary-shifted events were found." = "\u672A\u53D1\u73B0\u65B0\u589E\u3001\u6D88\u5931\u3001\u6807\u7B7E\u53D8\u5316\u6216\u8FB9\u754C\u53D8\u5316\u4E8B\u4EF6\u3002",
    "No logISI data are available." = "\u6CA1\u6709 logISI \u6570\u636E\u3002",
    "No data are available for this interval type." = "\u8BE5\u533A\u95F4\u7C7B\u578B\u65E0\u6570\u636E\u3002",
    "No valid ISIs are available." = "\u65E0\u6709\u6548 ISI\u3002",
    "No resolved threshold results are available." = "\u5C1A\u65E0\u9608\u503C\u89E3\u6790\u7ED3\u679C\u3002",
    "No manual pattern events are available. Structural learning results will appear here after manually labeling burst / HF / tonic / pause events." = "\u5C1A\u65E0\u624B\u52A8\u6A21\u5F0F\u4E8B\u4EF6\u3002\u624B\u52A8\u6807\u8BB0 burst / HF / tonic / pause \u540E\u4F1A\u5728\u6B64\u663E\u793A\u7ED3\u6784\u5B66\u4E60\u7ED3\u679C\u3002",
    "No exact duplicate timestamps are present in the current dataset." = "\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u6CA1\u6709\u5B8C\u5168\u91CD\u590D timestamp\u3002",
    "Run event-level parameter sensitivity to display the summary." = "\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u540E\u663E\u793A\u6458\u8981\u3002",
    "bridge ratio exceeds threshold" = "bridge \u6BD4\u503C\u8D85\u8FC7\u9608\u503C",
    "equivalent seed-core inflation needed" = "\u9700\u8981\u63D0\u9AD8\u7B49\u6548 seed-core \u9608\u503C",
    "raw bridge ISI exceeds threshold" = "\u539F\u59CB bridge ISI \u8D85\u8FC7\u9608\u503C",
    "merged bridge edge contrast min below threshold" = "\u5408\u5E76 bridge \u7684\u6700\u5C0F\u8FB9\u7F18\u5BF9\u6BD4\u5EA6\u4F4E\u4E8E\u9608\u503C",
    "merged bridge edge contrast geom below threshold" = "\u5408\u5E76 bridge \u7684\u51E0\u4F55\u5E73\u5747\u8FB9\u7F18\u5BF9\u6BD4\u5EA6\u4F4E\u4E8E\u9608\u503C",
    "local-compression median/core ratio below threshold" = "\u5C40\u90E8\u538B\u7F29\u7684\u4E2D\u4F4D\u6570/core \u6BD4\u503C\u4F4E\u4E8E\u9608\u503C",
    "local-compression core q90 percentile above threshold" = "\u5C40\u90E8\u538B\u7F29\u7684 core q90 \u767E\u5206\u4F4D\u9AD8\u4E8E\u9608\u503C",
    "local-compression edge contrast min below threshold" = "\u5C40\u90E8\u538B\u7F29\u7684\u6700\u5C0F\u8FB9\u7F18\u5BF9\u6BD4\u5EA6\u4F4E\u4E8E\u9608\u503C",
    "local-compression edge contrast geom below threshold" = "\u5C40\u90E8\u538B\u7F29\u7684\u51E0\u4F55\u5E73\u5747\u8FB9\u7F18\u5BF9\u6BD4\u5EA6\u4F4E\u4E8E\u9608\u503C",
    "boundary one-sided flank/core ratio below threshold" = "\u8FB9\u754C\u5355\u4FA7 flank/core \u6BD4\u503C\u4F4E\u4E8E\u9608\u503C",
    "boundary local median/core ratio below threshold" = "\u8FB9\u754C\u5C40\u90E8\u4E2D\u4F4D\u6570/core \u6BD4\u503C\u4F4E\u4E8E\u9608\u503C",
    "boundary core q90 percentile above threshold" = "\u8FB9\u754C core q90 \u767E\u5206\u4F4D\u9AD8\u4E8E\u9608\u503C",
    "final seed-core edge contrast min below threshold" = "\u6700\u7EC8 seed-core \u6700\u5C0F\u8FB9\u7F18\u5BF9\u6BD4\u5EA6\u4F4E\u4E8E\u9608\u503C",
    "final seed-core edge contrast geom below threshold" = "\u6700\u7EC8 seed-core \u51E0\u4F55\u5E73\u5747\u8FB9\u7F18\u5BF9\u6BD4\u5EA6\u4F4E\u4E8E\u9608\u503C",
    "final score below high-confidence cutoff" = "\u6700\u7EC8\u8BC4\u5206\u4F4E\u4E8E\u9AD8\u7F6E\u4FE1\u5EA6\u754C\u503C",
    "final duration exceeds threshold" = "\u6700\u7EC8\u6301\u7EED\u65F6\u95F4\u8D85\u8FC7\u9608\u503C",
    "mean ISI below tonic_T_min" = "\u5E73\u5747 ISI \u4F4E\u4E8E tonic_T_min",
    "mean ISI above tonic_T_max" = "\u5E73\u5747 ISI \u9AD8\u4E8E tonic_T_max",
    "LV above tonic_LV_core" = "LV \u9AD8\u4E8E tonic_LV_core",
    "seed ratio above threshold" = "seed \u6BD4\u503C\u9AD8\u4E8E\u9608\u503C",
    "max/mean above threshold" = "\u6700\u5927\u503C/\u5747\u503C\u9AD8\u4E8E\u9608\u503C",
    "min/mean below threshold" = "\u6700\u5C0F\u503C/\u5747\u503C\u4F4E\u4E8E\u9608\u503C",
    "effective ISI below pause seed threshold" = "\u6709\u6548 ISI \u4F4E\u4E8E pause seed \u9608\u503C",
    "ISI/local_median below pause alpha" = "ISI/local_median \u4F4E\u4E8E pause alpha",
    "global median pause guard above candidate" = "\u5168\u5C40\u4E2D\u4F4D\u6570 pause \u4FDD\u62A4\u9608\u503C\u9AD8\u4E8E\u5019\u9009\u503C",
    "ISI below pause strong threshold" = "ISI \u4F4E\u4E8E pause \u5F3A\u9608\u503C",
    "low_ISI_count" = "\u6709\u6548 ISI \u6570\u91CF\u8F83\u5C11",
    "HF_spiking / extreme-fast dominant" = "HF spiking / \u6781\u5FEB\u653E\u7535\u5360\u4E3B\u5BFC",
    "pause-prone / sparse burst-core" = "pause \u503E\u5411 / burst-core \u7A00\u758F",
    "burst-capable" = "\u5177\u5907 burst \u503E\u5411",
    "tonic-dominant / few burst-core ISIs" = "tonic \u5360\u4E3B\u5BFC / burst-core ISI \u5F88\u5C11",
    "mixed / review" = "\u6DF7\u5408\u578B / \u9700\u8981\u590D\u6838",
    "insufficient spikes" = "spike \u6570\u4E0D\u8DB3",
    "too few valid temporal bins" = "\u6709\u6548\u65F6\u95F4\u7BB1\u8FC7\u5C11",
    "possible overfit: calibration recall much higher than validation" = "\u53EF\u80FD\u8FC7\u62DF\u5408\uFF1A\u6821\u51C6\u96C6\u53EC\u56DE\u7387\u660E\u663E\u9AD8\u4E8E\u9A8C\u8BC1\u96C6",
    "no large recall gap detected" = "\u672A\u53D1\u73B0\u660E\u663E\u7684\u53EC\u56DE\u7387\u5DEE\u8DDD",
    "At least three time bins and two varying numeric train-pattern features are required." = "\u81F3\u5C11\u9700\u8981 3 \u4E2A\u65F6\u95F4\u7BB1\u548C 2 \u4E2A\u5177\u6709\u53D8\u5F02\u7684\u6570\u503C\u578B train \u6A21\u5F0F\u7279\u5F81\u3002",
    "SVD of the centered/scaled feature matrix; components are orthogonal." = "\u5BF9\u4E2D\u5FC3\u5316/\u7F29\u653E\u540E\u7684\u7279\u5F81\u77E9\u9635\u8FDB\u884C SVD\uFF1B\u5404\u5206\u91CF\u5F7C\u6B64\u6B63\u4EA4\u3002",
    "Linearly dependent train-pattern features were removed before ML factor analysis." = "\u5728\u6700\u5927\u4F3C\u7136\u56E0\u5B50\u5206\u6790\u524D\uFF0C\u5DF2\u79FB\u9664\u7EBF\u6027\u76F8\u5173\u7684 train \u6A21\u5F0F\u7279\u5F81\u3002",
    "Not enough variables or time bins for maximum-likelihood factor analysis with non-negative degrees of freedom." = "\u53D8\u91CF\u6216\u65F6\u95F4\u7BB1\u4E0D\u8DB3\uFF0C\u65E0\u6CD5\u5728\u975E\u8D1F\u81EA\u7531\u5EA6\u4E0B\u8FDB\u884C\u6700\u5927\u4F3C\u7136\u56E0\u5B50\u5206\u6790\u3002",
    "Linear Gaussian model X = Lambda f + epsilon; Cov(X) = Lambda Lambda' + Psi." = "\u7EBF\u6027\u9AD8\u65AF\u6A21\u578B X = Lambda f + epsilon\uFF1BCov(X) = Lambda Lambda' + Psi\u3002",
    "Uniqueness is the feature-specific residual variance Psi." = "\u72EC\u7279\u6027\u662F\u7279\u5F81\u7279\u5F02\u7684\u6B8B\u5DEE\u65B9\u5DEE Psi\u3002",
    "Euclidean kNN graph, shortest-path geodesic distances, then classical MDS." = "\u5148\u6784\u5EFA\u6B27\u6C0F\u8DDD\u79BB kNN \u56FE\uFF0C\u518D\u8BA1\u7B97\u6700\u77ED\u8DEF\u5F84\u6D4B\u5730\u8DDD\u79BB\uFF0C\u6700\u540E\u6267\u884C\u7ECF\u5178 MDS\u3002",
    "Minimizes KL divergence between high-dimensional Gaussian affinities and low-dimensional Student-t affinities." = "\u6700\u5C0F\u5316\u9AD8\u7EF4\u9AD8\u65AF\u76F8\u4F3C\u5EA6\u4E0E\u4F4E\u7EF4 Student-t \u76F8\u4F3C\u5EA6\u4E4B\u95F4\u7684 KL \u6563\u5EA6\u3002",
    "Builds a fuzzy simplicial set and optimizes a low-dimensional cross-entropy objective." = "\u6784\u5EFA\u6A21\u7CCA\u5355\u7EAF\u5F62\u96C6\u5408\uFF0C\u5E76\u4F18\u5316\u4F4E\u7EF4\u4EA4\u53C9\u71B5\u76EE\u6807\u3002",
    "Isomap graph has no finite geodesic distances." = "Isomap \u56FE\u4E2D\u6CA1\u6709\u6709\u9650\u7684\u6D4B\u5730\u8DDD\u79BB\u3002",
    "R package 'reticulate' is not installed." = "\u672A\u5B89\u88C5 R \u5305 reticulate\u3002",
    "No detector score column available for calibration." = "\u6CA1\u6709\u53EF\u7528\u4E8E\u6821\u51C6\u7684\u68C0\u6D4B\u5668\u8BC4\u5206\u5217\u3002",
    "Detector events exist, but all scores are missing or non-finite." = "\u5B58\u5728\u68C0\u6D4B\u5668\u4E8B\u4EF6\uFF0C\u4F46\u6240\u6709\u8BC4\u5206\u5747\u7F3A\u5931\u6216\u4E0D\u662F\u6709\u9650\u6570\u3002",
    "Brier score is reported because scores are bounded in [0, 1]." = "\u7531\u4E8E\u8BC4\u5206\u9650\u5B9A\u5728 [0, 1]\uFF0C\u56E0\u6B64\u62A5\u544A Brier \u8BC4\u5206\u3002",
    "Scores are treated as raw detector ranks; empirical precision by bin is reported without probability-scale Brier scoring." = "\u8BC4\u5206\u6309\u68C0\u6D4B\u5668\u539F\u59CB\u6392\u5E8F\u503C\u5904\u7406\uFF1B\u62A5\u544A\u5404\u65F6\u95F4\u7BB1\u7684\u7ECF\u9A8C\u7CBE\u786E\u7387\uFF0C\u4E0D\u8BA1\u7B97\u6982\u7387\u5C3A\u5EA6\u7684 Brier \u8BC4\u5206\u3002",
    "No finite training scores." = "\u6CA1\u6709\u6709\u9650\u7684\u8BAD\u7EC3\u8BC4\u5206\u3002",
    "Constant calibrator used because calibration labels or scores lack variation." = "\u7531\u4E8E\u6821\u51C6\u6807\u7B7E\u6216\u8BC4\u5206\u7F3A\u5C11\u53D8\u5F02\uFF0C\u4F7F\u7528\u5E38\u6570\u6821\u51C6\u5668\u3002",
    "Constant calibrator used because Platt fit was unstable." = "\u7531\u4E8E Platt \u62DF\u5408\u4E0D\u7A33\u5B9A\uFF0C\u4F7F\u7528\u5E38\u6570\u6821\u51C6\u5668\u3002",
    "Platt logistic calibrator fit on calibration split only." = "Platt \u903B\u8F91\u6821\u51C6\u5668\u4EC5\u5728\u6821\u51C6\u5B50\u96C6\u4E0A\u62DF\u5408\u3002",
    "Isotonic calibrator fit on calibration split only." = "\u4FDD\u5E8F\u6821\u51C6\u5668\u4EC5\u5728\u6821\u51C6\u5B50\u96C6\u4E0A\u62DF\u5408\u3002",
    "estimated expected surrogate events divided by observed detector events, capped at 1" = "\u4F30\u8BA1\u7684\u9884\u671F\u66FF\u4EE3\u4E8B\u4EF6\u6570\u9664\u4EE5\u89C2\u6D4B\u5230\u7684\u68C0\u6D4B\u5668\u4E8B\u4EF6\u6570\uFF0C\u4E0A\u9650\u4E3A 1",
    "not estimable when observed detector event count is zero" = "\u89C2\u6D4B\u5230\u7684\u68C0\u6D4B\u5668\u4E8B\u4EF6\u6570\u4E3A 0 \u65F6\u65E0\u6CD5\u4F30\u8BA1",
    "No manual events available; validation cannot estimate performance." = "\u6CA1\u6709\u53EF\u7528\u7684\u4EBA\u5DE5\u4E8B\u4EF6\uFF1B\u9A8C\u8BC1\u65E0\u6CD5\u4F30\u8BA1\u6027\u80FD\u3002",
    "Calibration/validation report based on manual labels. Use validation split for methods reporting; calibration split is for tuning feedback." = "\u57FA\u4E8E\u4EBA\u5DE5\u6807\u7B7E\u7684\u6821\u51C6/\u9A8C\u8BC1\u62A5\u544A\u3002\u65B9\u6CD5\u5B66\u62A5\u544A\u5E94\u4F7F\u7528\u9A8C\u8BC1\u5B50\u96C6\uFF1B\u6821\u51C6\u5B50\u96C6\u4EC5\u7528\u4E8E\u8C03\u53C2\u53CD\u9988\u3002",
    "No MANUAL labels available; event-level validation not computed." = "\u6CA1\u6709\u53EF\u7528\u7684 MANUAL \u6807\u7B7E\uFF1B\u672A\u8BA1\u7B97\u4E8B\u4EF6\u7EA7\u9A8C\u8BC1\u3002",
    "final-source agreement audit: predictions include MANUAL-first final labels and must not be interpreted as unbiased detector performance." = "\u6700\u7EC8\u6765\u6E90\u4E00\u81F4\u6027\u5BA1\u8BA1\uFF1A\u9884\u6D4B\u5305\u542B MANUAL \u4F18\u5148\u7684\u6700\u7EC8\u6807\u7B7E\uFF0C\u4E0D\u5F97\u5C06\u5176\u89E3\u91CA\u4E3A\u65E0\u504F\u7684\u68C0\u6D4B\u5668\u6027\u80FD\u3002",
    "auto-source candidate-family: burst/long_burst/possible_burst are merged into burst_family; use as candidate sensitivity, not high-confidence burst accuracy." = "AUTO \u6765\u6E90\u5019\u9009\u5BB6\u65CF\uFF1Aburst/long_burst/possible_burst \u5408\u5E76\u4E3A burst_family\uFF1B\u4EC5\u7528\u4E8E\u5019\u9009\u654F\u611F\u6027\uFF0C\u4E0D\u4EE3\u8868\u9AD8\u7F6E\u4FE1\u5EA6 burst \u51C6\u786E\u7387\u3002",
    "auto-source strict: possible_burst remains a separate review class." = "AUTO \u6765\u6E90\u4E25\u683C\u6A21\u5F0F\uFF1Apossible_burst \u4FDD\u6301\u4E3A\u72EC\u7ACB\u590D\u6838\u7C7B\u522B\u3002",
    "PHATE-like fallback: diffusion potential plus metric MDS; install/use phateR for canonical PHATE." = "PHATE \u98CE\u683C\u56DE\u9000\u65B9\u6848\uFF1A\u6269\u6563\u52BF\u52A0\u5EA6\u91CF MDS\uFF1B\u5982\u9700\u89C4\u8303 PHATE\uFF0C\u8BF7\u5B89\u88C5\u5E76\u4F7F\u7528 phateR\u3002",
    "Unable to fit factor-analysis model." = "\u65E0\u6CD5\u62DF\u5408\u56E0\u5B50\u5206\u6790\u6A21\u578B\u3002",
    "Need at least five time bins." = "\u81F3\u5C11\u9700\u8981 5 \u4E2A\u65F6\u95F4\u7BB1\u3002",
    "Need at least five time bins for Isomap." = "Isomap \u81F3\u5C11\u9700\u8981 5 \u4E2A\u65F6\u95F4\u7BB1\u3002",
    "Need at least five time bins for PHATE." = "PHATE \u81F3\u5C11\u9700\u8981 5 \u4E2A\u65F6\u95F4\u7BB1\u3002",
    "Install the Rtsne package to enable t-SNE." = "\u8BF7\u5B89\u88C5 Rtsne \u5305\u4EE5\u542F\u7528 t-SNE\u3002",
    "Need enough sampled bins for a valid perplexity." = "\u9700\u8981\u8DB3\u591F\u7684\u62BD\u6837\u65F6\u95F4\u7BB1\uFF0C\u624D\u80FD\u4F7F\u7528\u6709\u6548\u7684\u56F0\u60D1\u5EA6\u3002",
    "Install the uwot package to enable UMAP." = "\u8BF7\u5B89\u88C5 uwot \u5305\u4EE5\u542F\u7528 UMAP\u3002",
    "Need at least five sampled bins." = "\u81F3\u5C11\u9700\u8981 5 \u4E2A\u62BD\u6837\u65F6\u95F4\u7BB1\u3002",
    "candidate event generator + semi-supervised review platform; not an unbiased final truth classifier" = "\u5019\u9009\u4E8B\u4EF6\u751F\u6210\u5668\u4E0E\u534A\u76D1\u7763\u590D\u6838\u5E73\u53F0\uFF1B\u4E0D\u662F\u65E0\u504F\u7684\u6700\u7EC8\u771F\u503C\u5206\u7C7B\u5668",
    "Use strict high-confidence metrics and held-out train/dataset validation for publication-level analysis. Candidate-family metrics estimate candidate sensitivity only." = "\u53D1\u8868\u7EA7\u5206\u6790\u5E94\u4F7F\u7528\u4E25\u683C\u7684\u9AD8\u7F6E\u4FE1\u5EA6\u6307\u6807\u53CA\u7559\u51FA train/\u6570\u636E\u96C6\u9A8C\u8BC1\u3002\u5019\u9009\u5BB6\u65CF\u6307\u6807\u4EC5\u4F30\u8BA1\u5019\u9009\u654F\u611F\u6027\u3002",
    "Run stpd_detect() before auditing results." = "\u8BF7\u5148\u8FD0\u884C stpd_detect()\uFF0C\u518D\u5BA1\u8BA1\u7ED3\u679C\u3002",
    "Final events exist but event_audit is empty." = "\u5B58\u5728\u6700\u7EC8\u4E8B\u4EF6\uFF0C\u4F46 event_audit \u4E3A\u7A7A\u3002",
    "Candidates exist but candidate_features is empty." = "\u5B58\u5728\u5019\u9009\uFF0C\u4F46 candidate_features \u4E3A\u7A7A\u3002",
    "Candidate features exist but final_decisions is empty." = "\u5B58\u5728\u5019\u9009\u7279\u5F81\uFF0C\u4F46 final_decisions \u4E3A\u7A7A\u3002",
    "Candidate ledger should contain detector candidates only; use event_audit for final events." = "\u5019\u9009\u53F0\u8D26\u5E94\u4EC5\u5305\u542B\u68C0\u6D4B\u5668\u5019\u9009\uFF1B\u6700\u7EC8\u4E8B\u4EF6\u5E94\u4F7F\u7528 event_audit\u3002",
    "Candidate rows should be traceable to a detector run." = "\u5019\u9009\u884C\u5E94\u53EF\u8FFD\u6EAF\u5230\u4E00\u6B21\u68C0\u6D4B\u5668\u8FD0\u884C\u3002",
    "Candidate rows should include parameter hash." = "\u5019\u9009\u884C\u5E94\u5305\u542B\u53C2\u6570\u54C8\u5E0C\u3002",
    "possible_burst is a review class and should not be silently merged into high-confidence burst metrics." = "possible_burst \u662F\u590D\u6838\u7C7B\u522B\uFF0C\u4E0D\u5E94\u9759\u9ED8\u5408\u5E76\u5230\u9AD8\u7F6E\u4FE1\u5EA6 burst \u6307\u6807\u4E2D\u3002",
    "Euclidean kNN graph." = "\u6B27\u6C0F\u8DDD\u79BB kNN \u56FE\u3002",
    "Largest connected component is embedded when the kNN graph is disconnected." = "\u5F53 kNN \u56FE\u4E0D\u8FDE\u901A\u65F6\uFF0C\u4EC5\u5D4C\u5165\u6700\u5927\u7684\u8FDE\u901A\u5206\u91CF\u3002",
    "Number of connected components in the kNN graph." = "kNN \u56FE\u4E2D\u7684\u8FDE\u901A\u5206\u91CF\u6570\u91CF\u3002",
    "1 - cor(geodesic distance, embedding distance)^2." = "1 - cor\uFF08\u6D4B\u5730\u8DDD\u79BB\uFF0C\u5D4C\u5165\u8DDD\u79BB\uFF09^2\u3002",
    "Canonical phateR backend." = "\u89C4\u8303\u7684 phateR \u540E\u7AEF\u3002",
    "SVD of the scaled population activity matrix." = "\u5BF9\u7F29\u653E\u540E\u7684\u7FA4\u4F53\u6D3B\u52A8\u77E9\u9635\u8FDB\u884C SVD\u3002",
    "Gaussian-smoothed factor-analysis trajectory; not a full GPFA EM/Kalman implementation." = "\u9AD8\u65AF\u5E73\u6ED1\u7684\u56E0\u5B50\u5206\u6790\u8F68\u8FF9\uFF1B\u5E76\u975E\u5B8C\u6574\u7684 GPFA EM/Kalman \u5B9E\u73B0\u3002",
    "Linear Gaussian factor analysis." = "\u7EBF\u6027\u9AD8\u65AF\u56E0\u5B50\u5206\u6790\u3002",
    "Feature-specific private variance; lower values imply more shared population structure." = "\u5404\u7279\u5F81\u7684\u79C1\u6709\u65B9\u5DEE\uFF1B\u6570\u503C\u8D8A\u4F4E\uFF0C\u8868\u793A\u5171\u4EAB\u7FA4\u4F53\u7ED3\u6784\u8D8A\u5F3A\u3002",
    "uwot implementation." = "uwot \u5B9E\u73B0\u3002",
    "Rtsne implementation." = "Rtsne \u5B9E\u73B0\u3002",
    "Behavior-guided linear proxy; use external CEBRA for contrastive neural embedding in final analyses." = "\u884C\u4E3A\u5F15\u5BFC\u7684\u7EBF\u6027\u4EE3\u7406\uFF1B\u6700\u7EC8\u5206\u6790\u5E94\u4F7F\u7528\u5916\u90E8 CEBRA \u5B8C\u6210\u5BF9\u6BD4\u5F0F\u795E\u7ECF\u5D4C\u5165\u3002",
    "Correlation between supervised neural axis and behavior in available bins." = "\u53EF\u7528\u65F6\u95F4 bin \u4E2D\u76D1\u7763\u795E\u7ECF\u8F74\u4E0E\u884C\u4E3A\u7684\u76F8\u5173\u6027\u3002",
    "Embedding is computed from binned population activity, not detector-derived event labels." = "\u5D4C\u5165\u7531\u5206\u7BB1\u540E\u7684\u7FA4\u4F53\u6D3B\u52A8\u8BA1\u7B97\uFF0C\u800C\u4E0D\u662F\u7531\u68C0\u6D4B\u5668\u751F\u6210\u7684\u4E8B\u4EF6\u6807\u7B7E\u8BA1\u7B97\u3002",
    "No numeric behavior variable with enough bins." = "\u6CA1\u6709\u5177\u6709\u8DB3\u591F\u65F6\u95F4 bin \u7684\u6570\u503C\u578B\u884C\u4E3A\u53D8\u91CF\u3002",
    "Linear behavior decoder failed." = "\u7EBF\u6027\u884C\u4E3A\u89E3\u7801\u5668\u5931\u8D25\u3002",
    "Blocked 70/30 time split; linear decoder from 3D manifold coordinates to numeric behavior." = "\u91C7\u7528\u6309\u65F6\u95F4\u5206\u5757\u7684 70/30 \u5212\u5206\uFF1B\u4F7F\u7528\u7EBF\u6027\u89E3\u7801\u5668\u5C06 3D \u6D41\u5F62\u5750\u6807\u89E3\u7801\u4E3A\u6570\u503C\u578B\u884C\u4E3A\u3002",
    "Need at least ten bins and three neurons." = "\u81F3\u5C11\u9700\u8981 10 \u4E2A\u65F6\u95F4 bin \u548C 3 \u4E2A\u795E\u7ECF\u5143\u3002",
    "Each sampled neuron is held out from PCA coordinates, then predicted on a later time split." = "\u6BCF\u4E2A\u62BD\u6837\u795E\u7ECF\u5143\u5747\u4ECE PCA \u5750\u6807\u4E2D\u7559\u51FA\uFF0C\u5E76\u5728\u8F83\u665A\u7684\u65F6\u95F4\u5206\u5757\u4E0A\u9884\u6D4B\u3002",
    "Need enough bins and neurons." = "\u9700\u8981\u8DB3\u591F\u7684\u65F6\u95F4 bin \u548C\u795E\u7ECF\u5143\u3002",
    "Fraction of scaled population variance captured by first three PCs." = "\u524D\u4E09\u4E2A PC \u89E3\u91CA\u7684\u7F29\u653E\u7FA4\u4F53\u65B9\u5DEE\u6BD4\u4F8B\u3002",
    "Independent within-neuron time shuffling destroys temporal co-activation." = "\u5404\u795E\u7ECF\u5143\u5185\u90E8\u72EC\u7ACB\u8FDB\u884C\u65F6\u95F4 shuffle\uFF0C\u4EE5\u7834\u574F\u65F6\u95F4\u5171\u6FC0\u6D3B\u3002",
    "Within-bin neuron-value shuffling preserves population magnitude but disrupts neuron identity." = "\u5728\u6BCF\u4E2A bin \u5185 shuffle \u795E\u7ECF\u5143\u6570\u503C\uFF0C\u4FDD\u7559\u7FA4\u4F53\u5E45\u5EA6\u4F46\u7834\u574F\u795E\u7ECF\u5143\u8EAB\u4EFD\u3002",
    "Large centroid distances are tested against event-label permutation and circular time-shift controls." = "\u8F83\u5927\u7684\u8D28\u5FC3\u8DDD\u79BB\u9700\u4E0E\u4E8B\u4EF6\u6807\u7B7E\u7F6E\u6362\u548C\u5FAA\u73AF\u65F6\u95F4\u5E73\u79FB\u5BF9\u7167\u6BD4\u8F83\u3002",
    "No event-state labels are available." = "\u6CA1\u6709\u53EF\u7528\u7684\u4E8B\u4EF6\u72B6\u6001\u6807\u7B7E\u3002",
    "Need at least ten labeled bins and two event classes excluding unlabeled." = "\u81F3\u5C11\u9700\u8981 10 \u4E2A\u5DF2\u6807\u8BB0\u65F6\u95F4 bin\uFF0C\u4EE5\u53CA\u4E24\u4E2A\u4E0D\u542B\u672A\u6807\u8BB0\u9879\u7684\u4E8B\u4EF6\u7C7B\u522B\u3002",
    "Blocked time split does not contain enough event classes in train/test." = "\u5206\u5757\u65F6\u95F4\u5212\u5206\u7684\u8BAD\u7EC3\u96C6\u6216\u6D4B\u8BD5\u96C6\u4E2D\u6CA1\u6709\u8DB3\u591F\u7684\u4E8B\u4EF6\u7C7B\u522B\u3002",
    "Nearest-centroid classifier from 3D manifold coordinates to event state; blocked 70/30 time split." = "\u4F7F\u7528\u6700\u8FD1\u8D28\u5FC3\u5206\u7C7B\u5668\u5C06 3D \u6D41\u5F62\u5750\u6807\u6620\u5C04\u5230\u4E8B\u4EF6\u72B6\u6001\uFF1B\u91C7\u7528\u6309\u65F6\u95F4\u5206\u5757\u7684 70/30 \u5212\u5206\u3002",
    "Mean per-class recall for event-state decoding." = "\u4E8B\u4EF6\u72B6\u6001\u89E3\u7801\u7684\u7C7B\u522B\u5E73\u5747\u53EC\u56DE\u7387\u3002",
    "Permutation p-value against shuffled event labels; small values mean event labels align with manifold geometry beyond class imbalance." = "\u76F8\u5BF9\u4E8E\u968F\u673A\u7F6E\u6362\u4E8B\u4EF6\u6807\u7B7E\u7684 p \u503C\uFF1B\u8F83\u5C0F\u503C\u8868\u793A\u4E8B\u4EF6\u6807\u7B7E\u4E0E\u6D41\u5F62\u51E0\u4F55\u7684\u5BF9\u5E94\u8D85\u51FA\u4E86\u7C7B\u522B\u4E0D\u5E73\u8861\u7684\u5F71\u54CD\u3002",
    "No 3D manifold coordinates." = "\u6CA1\u6709 3D \u6D41\u5F62\u5750\u6807\u3002",
    "Event-state regressors are unavailable." = "\u4E8B\u4EF6\u72B6\u6001\u56DE\u5F52\u53D8\u91CF\u4E0D\u53EF\u7528\u3002",
    "Blocked time split leaves too few behavior test bins." = "\u5206\u5757\u65F6\u95F4\u5212\u5206\u540E\u5269\u4F59\u7684\u884C\u4E3A\u6D4B\u8BD5 bin \u592A\u5C11\u3002",
    "Blocked 70/30 behavior decoding comparison: 3D manifold alone versus 3D manifold plus event-state regressors." = "\u6309\u65F6\u95F4\u5206\u5757\u7684 70/30 \u884C\u4E3A\u89E3\u7801\u6BD4\u8F83\uFF1A\u4EC5\u4F7F\u7528 3D \u6D41\u5F62\uFF0C\u6216\u4F7F\u7528 3D \u6D41\u5F62\u52A0\u4E8B\u4EF6\u72B6\u6001\u56DE\u5F52\u53D8\u91CF\u3002",
    "Latent speed/curvature around event onset; compare against time-shift controls before biological interpretation." = "\u4E8B\u4EF6\u8D77\u59CB\u9644\u8FD1\u7684\u6F5C\u5728\u901F\u5EA6/\u66F2\u7387\uFF1B\u8FDB\u884C\u751F\u7269\u5B66\u89E3\u91CA\u524D\u5E94\u4E0E\u65F6\u95F4\u5E73\u79FB\u5BF9\u7167\u6BD4\u8F83\u3002",
    "Distance between burst and pause centroids in the 3D manifold." = "3D \u6D41\u5F62\u4E2D burst \u4E0E pause \u8D28\u5FC3\u4E4B\u95F4\u7684\u8DDD\u79BB\u3002",
    "Permutation p-value for burst-pause centroid distance under shuffled event labels." = "\u968F\u673A\u7F6E\u6362\u4E8B\u4EF6\u6807\u7B7E\u540E burst\u2013pause \u8D28\u5FC3\u8DDD\u79BB\u7684\u7F6E\u6362 p \u503C\u3002",
    "Circular time-shift p-value preserving label run structure while breaking exact alignment." = "\u5FAA\u73AF\u65F6\u95F4\u5E73\u79FB p \u503C\uFF1A\u4FDD\u7559\u6807\u7B7E\u8FDE\u7EED\u6BB5\u7ED3\u6784\uFF0C\u540C\u65F6\u7834\u574F\u7CBE\u786E\u5BF9\u9F50\u3002",
    "No 3D manifold coordinates are available." = "\u6CA1\u6709\u53EF\u7528\u7684 3D \u6D41\u5F62\u5750\u6807\u3002",
    "Neighborhood preservation from population space to manifold." = "\u4ECE\u7FA4\u4F53\u7A7A\u95F4\u5230\u6D41\u5F62\u7684\u90BB\u57DF\u4FDD\u6301\u5EA6\u3002",
    "Neighborhood preservation from manifold back to population space." = "\u4ECE\u6D41\u5F62\u8FD4\u56DE\u7FA4\u4F53\u7A7A\u95F4\u7684\u90BB\u57DF\u4FDD\u6301\u5EA6\u3002",
    "Run sensitivity by changing this control and comparing validation metrics." = "\u6539\u53D8\u8BE5\u63A7\u4EF6\u5E76\u6BD4\u8F83\u9A8C\u8BC1\u6307\u6807\uFF0C\u4EE5\u8FDB\u884C\u654F\u611F\u6027\u5206\u6790\u3002",
    "For UMAP/t-SNE, repeat with different seeds and compare trustworthiness/behavior decoding." = "\u5BF9\u4E8E UMAP/t-SNE\uFF0C\u8BF7\u4F7F\u7528\u4E0D\u540C random seed \u91CD\u590D\u8FD0\u884C\uFF0C\u5E76\u6BD4\u8F83 trustworthiness/\u884C\u4E3A\u89E3\u7801\u3002",
    "Python sliceTCA backend has not produced a reconstruction." = "Python sliceTCA \u540E\u7AEF\u5C1A\u672A\u751F\u6210\u91CD\u5EFA\u7ED3\u679C\u3002",
    "Reconstruction shape does not match tensor shape." = "\u91CD\u5EFA\u7ED3\u679C\u7684\u5F62\u72B6\u4E0E\u5F20\u91CF\u5F62\u72B6\u4E0D\u4E00\u81F4\u3002",
    "Reconstruction quality on the fitted tensor; use held-out block CV for publication-grade model selection." = "\u8FD9\u662F\u62DF\u5408\u5F20\u91CF\u4E0A\u7684\u91CD\u5EFA\u8D28\u91CF\uFF1B\u53D1\u8868\u7EA7\u6A21\u578B\u9009\u62E9\u5E94\u4F7F\u7528\u7559\u51FA\u5206\u5757\u4EA4\u53C9\u9A8C\u8BC1\u3002",
    "Ranks are ordered as trial-slicing, neuron-slicing, time-slicing components." = "\u79E9\u4F9D\u6B21\u5BF9\u5E94 trial-slicing\u3001neuron-slicing \u548C time-slicing \u5206\u91CF\u3002",
    "Enable 'Run Python sliceTCA backend' after installing numpy, torch and slicetca." = "\u5B89\u88C5 numpy\u3001torch \u548C slicetca \u540E\uFF0C\u518D\u542F\u7528\u201C\u8FD0\u884C Python sliceTCA \u540E\u7AEF\u201D\u3002",
    "Official Python slicetca backend completed." = "\u5B98\u65B9 Python slicetca \u540E\u7AEF\u5DF2\u5B8C\u6210\u3002",
    "No sliceTCA tensor is available." = "\u6682\u65E0\u53EF\u7528\u7684 sliceTCA \u5F20\u91CF\u3002",
    "No neural manifold can be built from the selected spike trains." = "\u65E0\u6CD5\u4ECE\u6240\u9009 spike train \u6784\u5EFA\u795E\u7ECF\u6D41\u5F62\u3002",
    "No spike trains are loaded." = "\u5C1A\u672A\u52A0\u8F7D spike train\u3002",
    "No event-state annotations are available." = "\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u72B6\u6001\u6CE8\u91CA\u3002",
    "No embedded bins have event-state labels." = "\u5DF2\u5D4C\u5165\u7684\u65F6\u95F4 bin \u4E2D\u6CA1\u6709\u4E8B\u4EF6\u72B6\u6001\u6807\u7B7E\u3002",
    "No event state has enough embedded bins." = "\u6CA1\u6709\u4EFB\u4F55\u4E8B\u4EF6\u72B6\u6001\u5305\u542B\u8DB3\u591F\u7684\u5DF2\u5D4C\u5165\u65F6\u95F4 bin\u3002",
    "No event-state manifold geometry is available." = "\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u72B6\u6001\u6D41\u5F62\u51E0\u4F55\u3002",
    "Need at least two event states with enough bins." = "\u81F3\u5C11\u9700\u8981\u4E24\u4E2A\u62E5\u6709\u8DB3\u591F\u65F6\u95F4 bin \u7684\u4E8B\u4EF6\u72B6\u6001\u3002",
    "No valid event-state distance tests." = "\u6682\u65E0\u6709\u6548\u7684\u4E8B\u4EF6\u72B6\u6001\u8DDD\u79BB\u68C0\u9A8C\u3002",
    "Event-label decoding was skipped." = "\u5DF2\u8DF3\u8FC7\u4E8B\u4EF6\u6807\u7B7E\u89E3\u7801\u3002",
    "No event-triggered manifold trajectory is available." = "\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u89E6\u53D1\u6D41\u5F62\u8F68\u8FF9\u3002",
    "No burst/pause onsets are available in the current bins." = "\u5F53\u524D\u65F6\u95F4 bin \u4E2D\u6CA1\u6709\u53EF\u7528\u7684 burst/pause \u8D77\u70B9\u3002",
    "No event-triggered manifold trajectory rows." = "\u6682\u65E0\u4E8B\u4EF6\u89E6\u53D1\u6D41\u5F62\u8F68\u8FF9\u884C\u3002",
    "No task-event-triggered manifold trajectory is available." = "\u6682\u65E0\u53EF\u7528\u7684\u4EFB\u52A1\u4E8B\u4EF6\u89E6\u53D1\u6D41\u5F62\u8F68\u8FF9\u3002",
    "No manifold bins fall inside the selected task-event peri-event windows." = "\u9009\u5B9A\u7684\u4EFB\u52A1\u4E8B\u4EF6\u5468\u8FB9\u7A97\u53E3\u5185\u6CA1\u6709\u6D41\u5F62\u65F6\u95F4 bin\u3002",
    "No task-event trajectory rows." = "\u6682\u65E0\u4EFB\u52A1\u4E8B\u4EF6\u8F68\u8FF9\u884C\u3002",
    "No latent speed/curvature event summary is available." = "\u6682\u65E0\u53EF\u7528\u7684\u6F5C\u5728\u901F\u5EA6/\u66F2\u7387\u4E8B\u4EF6\u6458\u8981\u3002",
    "No event onsets are available." = "\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u8D77\u70B9\u3002",
    "Install the R package 'reticulate' to enable the optional Python backend." = "\u8BF7\u5B89\u88C5 R \u5305 reticulate \u4EE5\u542F\u7528\u53EF\u9009 Python \u540E\u7AEF\u3002",
    "Python has not been initialized; call stpd_install_slicetca_backend() or run the sliceTCA backend to check modules." = "Python \u5C1A\u672A\u521D\u59CB\u5316\uFF1B\u8BF7\u8C03\u7528 stpd_install_slicetca_backend() \u6216\u8FD0\u884C sliceTCA \u540E\u7AEF\u4EE5\u68C0\u67E5\u6A21\u5757\u3002",
    "Need at least two trial/event times for sliceTCA." = "sliceTCA \u81F3\u5C11\u9700\u8981\u4E24\u4E2A trial/\u4E8B\u4EF6\u65F6\u95F4\u70B9\u3002",
    "Need at least two selected neurons for sliceTCA." = "sliceTCA \u81F3\u5C11\u9700\u8981\u4E24\u4E2A\u6240\u9009\u795E\u7ECF\u5143\u3002",
    "Need at least two valid spike trains with timestamps." = "\u81F3\u5C11\u9700\u8981\u4E24\u6761\u542B\u6709 timestamp \u7684\u6709\u6548 spike train\u3002",
    "sliceTCA tensor is not ready." = "sliceTCA \u5F20\u91CF\u5C1A\u672A\u5C31\u7EEA\u3002",
    "Freeze this single-file script as a reference prototype; stop adding detection classes here." =
      "\u5C06\u8BE5\u5355\u6587\u4EF6\u811A\u672C\u56FA\u5B9A\u4E3A\u53C2\u8003\u539F\u578B\uFF1B\u4E0D\u518D\u5728\u6B64\u6DFB\u52A0\u68C0\u6D4B\u7C7B\u522B\u3002",
    "Move IO/QC/features/candidates/final_classification/ledger/evaluation/export/UI into separate package modules." =
      "\u5C06 IO/QC/\u7279\u5F81/\u5019\u9009/\u6700\u7EC8\u5206\u7C7B/ledger/\u8BC4\u4F30/\u5BFC\u51FA/UI \u62C6\u5206\u5230\u72EC\u7ACB\u5305\u6A21\u5757\u4E2D\u3002",
    "Create golden tests for boundary burst, refractory policies, HF packets, pause boundaries, ledgers, and exports." =
      "\u4E3A\u8FB9\u754C burst\u3001\u4E0D\u5E94\u671F\u7B56\u7565\u3001HF \u4E8B\u4EF6\u5305\u3001pause \u8FB9\u754C\u3001ledger \u548C\u5BFC\u51FA\u5EFA\u7ACB\u9EC4\u91D1\u56DE\u5F52\u6D4B\u8BD5\u3002",
    "Add held-out train/dataset evaluation, event-level IoU, and threshold sensitivity reports before publication use." =
      "\u5728\u7528\u4E8E\u53D1\u8868\u524D\uFF0C\u589E\u52A0\u7559\u51FA train/\u6570\u636E\u96C6\u8BC4\u4F30\u3001\u4E8B\u4EF6\u7EA7 IoU \u548C\u9608\u503C\u654F\u611F\u6027\u62A5\u544A\u3002",
    "After modularization, move ISI percentiles, rolling medians, candidate scans, and interval joins to Rcpp/data.table." =
      "\u5B8C\u6210\u6A21\u5757\u5316\u540E\uFF0C\u5C06 ISI \u767E\u5206\u4F4D\u6570\u3001\u6EDA\u52A8\u4E2D\u4F4D\u6570\u3001\u5019\u9009\u626B\u63CF\u548C\u533A\u95F4\u8FDE\u63A5\u8FC1\u79FB\u5230 Rcpp/data.table\u3002",
    "The current script is valuable as a behavior reference, not as a maintainable production core." =
      "\u5F53\u524D\u811A\u672C\u7684\u4EF7\u503C\u5728\u4E8E\u4F5C\u4E3A\u884C\u4E3A\u53C2\u8003\uFF0C\u800C\u4E0D\u662F\u53EF\u7EF4\u62A4\u7684\u751F\u4EA7\u6838\u5FC3\u3002",
    "Separation of concerns prevents UI/API/export drift from changing scientific labels silently." =
      "\u5173\u6CE8\u70B9\u5206\u79BB\u53EF\u9632\u6B62 UI/API/\u5BFC\u51FA\u504F\u79FB\u9759\u9ED8\u6539\u53D8\u79D1\u5B66\u6807\u7B7E\u3002",
    "Regression tests protect biologically meaningful edge cases from future patches." =
      "\u56DE\u5F52\u6D4B\u8BD5\u53EF\u4FDD\u62A4\u5177\u6709\u751F\u7269\u5B66\u610F\u4E49\u7684\u8FB9\u754C\u60C5\u5F62\uFF0C\u907F\u514D\u5B83\u4EEC\u88AB\u540E\u7EED\u8865\u4E01\u7834\u574F\u3002",
    "Interactive threshold tuning can overfit manual labels without held-out evidence." =
      "\u82E5\u7F3A\u5C11\u7559\u51FA\u8BC1\u636E\uFF0C\u4EA4\u4E92\u5F0F\u9608\u503C\u8C03\u6574\u53EF\u80FD\u8FC7\u62DF\u5408\u624B\u52A8\u6807\u7B7E\u3002",
    "Performance optimization is safest once feature computation and classification boundaries are explicit." =
      "\u5728\u7279\u5F81\u8BA1\u7B97\u548C\u5206\u7C7B\u8FB9\u754C\u660E\u786E\u540E\u518D\u505A\u6027\u80FD\u4F18\u5316\u6700\u4E3A\u5B89\u5168\u3002",
    "Use current metrics as calibration feedback unless held-out train/dataset evaluation is performed." =
      "\u9664\u975E\u5DF2\u8FDB\u884C\u7559\u51FA train/\u6570\u636E\u96C6\u8BC4\u4F30\uFF0C\u5426\u5219\u5E94\u5C06\u5F53\u524D\u6307\u6807\u4EC5\u89C6\u4E3A\u6821\u51C6\u53CD\u9988\u3002",
    "Report strict high-confidence, review-candidate, and burst-family metrics separately; do not merge possible_burst into burst silently." =
      "\u5E94\u5206\u5F00\u62A5\u544A\u4E25\u683C\u9AD8\u7F6E\u4FE1\u5EA6\u3001\u590D\u6838\u5019\u9009\u548C burst-family \u6307\u6807\uFF1B\u4E0D\u5F97\u5C06 possible_burst \u9759\u9ED8\u5408\u5E76\u5230 burst\u3002"
  )
}

stpd_ui_localize_table_copy <- function(data, lang = "zh") {
  if (!is.data.frame(data)) return(data)
  lang <- if (identical(as.character(lang %||% "zh")[1], "en")) "en" else "zh"
  dictionary <- stpd_ui_table_copy_dictionary()
  reverse_dictionary <- stats::setNames(names(dictionary), unname(dictionary))
  text_columns <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x), logical(1))]
  if (identical(lang, "en") && length(text_columns) > 0L) {
    text_values <- unlist(lapply(data[text_columns], as.character), use.names = FALSE)
    if (!any(text_values %in% names(reverse_dictionary)) &&
        !any(vapply(text_values, stpd_i18n_contains_cjk, logical(1)))) {
      return(data)
    }
  }
  for (nm in names(data)) {
    if (!is.character(data[[nm]]) && !is.factor(data[[nm]])) next
    was_factor <- is.factor(data[[nm]])
    values <- as.character(data[[nm]])
    if (identical(lang, "en")) {
      hits <- match(values, names(reverse_dictionary))
      replace <- !is.na(hits)
      values[replace] <- unname(reverse_dictionary[hits[replace]])
      has_cjk <- vapply(values, stpd_i18n_contains_cjk, logical(1))
      if (any(has_cjk)) {
        values[has_cjk] <- vapply(
          values[has_cjk],
          stpd_i18n_translate_text,
          character(1),
          lang = "en"
        )
      }
    } else {
      hits <- match(values, names(dictionary))
      replace <- !is.na(hits)
      values[replace] <- unname(dictionary[hits[replace]])
      weak_isi <- grepl("^<([0-9]+) valid ISIs; stationarity check weak$", values, perl = TRUE)
      values[weak_isi] <- sub(
        "^<([0-9]+) valid ISIs; stationarity check weak$",
        "<\\1 \u4E2A\u6709\u6548 ISI\uFF1B\u5E73\u7A33\u6027\u68C0\u67E5\u8BC1\u636E\u8F83\u5F31",
        values[weak_isi], perl = TRUE
      )
      drift <- grepl("^sliding median ISI drift ratio=([^;]+); pause/global thresholds may be state-dependent$", values, perl = TRUE)
      values[drift] <- sub(
        "^sliding median ISI drift ratio=([^;]+); pause/global thresholds may be state-dependent$",
        "\u6ED1\u52A8\u4E2D\u4F4D ISI \u6F02\u79FB\u6BD4=\\1\uFF1Bpause/\u5168\u5C40\u9608\u503C\u53EF\u80FD\u4F9D\u8D56\u72B6\u6001",
        values[drift], perl = TRUE
      )
      phate_error <- grepl("^PHATE-like fallback: diffusion potential plus metric MDS; install/use phateR for canonical PHATE[.] phateR error:", values)
      values[phate_error] <- sub(
        "^PHATE-like fallback: diffusion potential plus metric MDS; install/use phateR for canonical PHATE[.] phateR error:[ ]*",
        "PHATE \u98CE\u683C\u56DE\u9000\u65B9\u6848\uFF1A\u6269\u6563\u52BF\u52A0\u5EA6\u91CF MDS\uFF1B\u5982\u9700\u89C4\u8303 PHATE\uFF0C\u8BF7\u5B89\u88C5\u5E76\u4F7F\u7528 phateR\u3002 \u6280\u672F\u8BE6\u60C5\uFF1A",
        values[phate_error]
      )
      if (identical(nm, "note") && "status" %in% names(data)) {
        status <- tolower(as.character(data$status %||% ""))
        raw_failure <- status %in% c("failed", "error") & nzchar(values) &
          !vapply(values, stpd_i18n_contains_cjk, logical(1))
        values[raw_failure] <- paste0("\u6280\u672F\u8BE6\u60C5\uFF1A", values[raw_failure])
      }
    }
    data[[nm]] <- if (was_factor) factor(values, levels = unique(values)) else values
  }
  data
}

# Parameter validation is part of the scientific control surface, so its
# finite set of contract messages must be readable in the selected UI
# language.  Keep paths, type names, choice values, and numeric limits intact;
# only localize the explanatory prose around those technical values.
stpd_ui_localize_parameter_issue_text <- function(x, lang = "zh") {
  values <- as.character(x %||% "")
  if (identical(as.character(lang %||% "zh")[1], "en")) return(values)

  exact <- c(
    "key parameter missing from params; schema default will be applied" =
      "\u53C2\u6570\u4E2D\u7F3A\u5C11\u5173\u952E\u9879\uFF1B\u5C06\u4F7F\u7528 schema \u9ED8\u8BA4\u503C\u3002",
    "parameter not present in registry; retained as legacy/extension parameter" =
      "\u53C2\u6570\u672A\u5728 registry \u4E2D\u6CE8\u518C\uFF1B\u5C06\u4F5C\u4E3A\u65E7\u7248/\u6269\u5C55\u53C2\u6570\u4FDD\u7559\u3002",
    "logical parameter has missing or invalid value" =
      "logical \u53C2\u6570\u7F3A\u5931\u6216\u53D6\u503C\u65E0\u6548\u3002",
    "numeric parameter has missing or invalid value" =
      "numeric \u53C2\u6570\u7F3A\u5931\u6216\u53D6\u503C\u65E0\u6548\u3002",
    "integer parameter has missing or invalid value" =
      "integer \u53C2\u6570\u7F3A\u5931\u6216\u53D6\u503C\u65E0\u6548\u3002",
    "integer parameter must be finite" =
      "integer \u53C2\u6570\u5FC5\u987B\u662F\u6709\u9650\u6570\u503C\u3002",
    "integer parameter contains a fractional value" =
      "integer \u53C2\u6570\u4E2D\u542B\u6709\u5C0F\u6570\u3002",
    "invalid YAML numeric value" = "YAML \u4E2D\u7684 numeric \u503C\u65E0\u6548\u3002",
    "invalid YAML integer value" = "YAML \u4E2D\u7684 integer \u503C\u65E0\u6548\u3002",
    "invalid YAML logical value" = "YAML \u4E2D\u7684 logical \u503C\u65E0\u6548\u3002",
    "burst seed lower bound must be strictly less than seed upper bound" =
      "burst \u7684 seed \u4E0B\u754C\u5FC5\u987B\u4E25\u683C\u5C0F\u4E8E seed \u4E0A\u754C\u3002",
    "burst seed upper bound must not exceed bridge upper bound" =
      "burst \u7684 seed \u4E0A\u754C\u4E0D\u5F97\u8D85\u8FC7 bridge \u4E0A\u754C\u3002",
    "possible burst contrast must not exceed canonical burst contrast" =
      "possible_burst \u5BF9\u6BD4\u5EA6\u4E0D\u5F97\u9AD8\u4E8E canonical burst \u5BF9\u6BD4\u5EA6\u3002",
    "classic burst minimum spikes must not exceed classic burst maximum spikes" =
      "\u7ECF\u5178 burst \u7684\u6700\u5C0F spike \u6570\u4E0D\u5F97\u8D85\u8FC7\u5176\u6700\u5927 spike \u6570\u3002",
    "classic burst maximum spikes must be strictly less than long burst minimum spikes" =
      "\u7ECF\u5178 burst \u7684\u6700\u5927 spike \u6570\u5FC5\u987B\u4E25\u683C\u5C0F\u4E8E long_burst \u7684\u6700\u5C0F spike \u6570\u3002",
    "long burst maximum spikes set to 0 enables the legacy unbounded sentinel; prolonged-burst sizing may be unreachable" =
      "long_burst \u6700\u5927 spike \u6570\u8BBE\u4E3A 0 \u4F1A\u542F\u7528\u65E7\u7248\u65E0\u4E0A\u9650\u6807\u8BB0\uFF1Bprolonged burst \u7684\u89C4\u6A21\u5206\u7C7B\u53EF\u80FD\u65E0\u6CD5\u5230\u8FBE\u3002",
    "long burst minimum spikes must not exceed long burst maximum spikes" =
      "long_burst \u7684\u6700\u5C0F spike \u6570\u4E0D\u5F97\u8D85\u8FC7\u5176\u6700\u5927 spike \u6570\u3002",
    "long burst maximum spikes must be strictly less than prolonged burst minimum spikes" =
      "long_burst \u7684\u6700\u5927 spike \u6570\u5FC5\u987B\u4E25\u683C\u5C0F\u4E8E prolonged burst \u7684\u6700\u5C0F spike \u6570\u3002",
    "prolonged burst minimum spikes must not exceed prolonged burst maximum spikes" =
      "prolonged burst \u7684\u6700\u5C0F spike \u6570\u4E0D\u5F97\u8D85\u8FC7\u5176\u6700\u5927 spike \u6570\u3002",
    "tonic minimum ISI must be strictly less than tonic maximum ISI" =
      "tonic \u7684\u6700\u5C0F ISI \u5FC5\u987B\u4E25\u683C\u5C0F\u4E8E\u6700\u5927 ISI\u3002",
    "tonic maximum ISI must not exceed tonic bridge upper bound" =
      "tonic \u7684\u6700\u5927 ISI \u4E0D\u5F97\u8D85\u8FC7 bridge \u4E0A\u754C\u3002",
    "pause minimum ISI must not exceed pause maximum ISI" =
      "pause \u7684\u6700\u5C0F ISI \u4E0D\u5F97\u8D85\u8FC7\u6700\u5927 ISI\u3002",
    "pause maximum ISI must not exceed pause bridge upper bound" =
      "pause \u7684\u6700\u5927 ISI \u4E0D\u5F97\u8D85\u8FC7 bridge \u4E0A\u754C\u3002",
    "high-frequency spiking short-ISI upper bound must not exceed its q90 bound" =
      "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u7684\u77ED ISI \u4E0A\u754C\u4E0D\u5F97\u8D85\u8FC7 q90 \u4E0A\u754C\u3002",
    "high-frequency spiking q90 bound must not exceed its epoch bridge bound" =
      "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u7684 q90 \u4E0A\u754C\u4E0D\u5F97\u8D85\u8FC7 epoch bridge \u4E0A\u754C\u3002",
    "high-frequency spiking epoch bridge bound must not exceed its tolerated-gap bound" =
      "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u7684 epoch bridge \u4E0A\u754C\u4E0D\u5F97\u8D85\u8FC7\u53EF\u5BB9\u5FCD\u95F4\u9699\u4E0A\u754C\u3002",
    "pause seed below the high-frequency spiking connector requires instance-level Pause boundary resolution" =
      "pause seed \u4F4E\u4E8E\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E connector \u65F6\uFF0C\u5FC5\u987B\u4F7F\u7528\u9010\u5B9E\u4F8B Pause \u8FB9\u754C\u89E3\u6790\u3002",
    "an active high-frequency spiking direct-support maximum-ISI cap must not exceed the epoch bridge bound" =
      "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u7684 direct-support \u6700\u5927 ISI \u4E0A\u9650\u4E0D\u5F97\u8D85\u8FC7 epoch bridge \u4E0A\u754C\u3002",
    "enabled event-grammar user band values must be finite and strictly positive" =
      "\u5DF2\u542F\u7528\u7684 event grammar \u7528\u6237\u9891\u5E26\u503C\u5FC5\u987B\u662F\u6709\u9650\u7684\u4E25\u683C\u6B63\u6570\u3002",
    "enabled event-grammar user seed lower bound must be strictly less than its seed upper bound" =
      "\u5DF2\u542F\u7528\u7684 event grammar \u7528\u6237 seed \u4E0B\u754C\u5FC5\u987B\u4E25\u683C\u5C0F\u4E8E seed \u4E0A\u754C\u3002",
    "enabled event-grammar user seed upper bound must not exceed its bridge upper bound" =
      "\u5DF2\u542F\u7528\u7684 event grammar \u7528\u6237 seed \u4E0A\u754C\u4E0D\u5F97\u8D85\u8FC7 bridge \u4E0A\u754C\u3002",
    "enabled event-grammar user burst contrast must be finite and at least 1" =
      "\u5DF2\u542F\u7528\u7684 event grammar \u7528\u6237 burst \u5BF9\u6BD4\u5EA6\u5FC5\u987B\u662F\u6709\u9650\u6570\u4E14\u81F3\u5C11\u4E3A 1\u3002",
    "enabled event-grammar user burst contrast must not be below the canonical possible-burst contrast" =
      "\u5DF2\u542F\u7528\u7684 event grammar \u7528\u6237 burst \u5BF9\u6BD4\u5EA6\u4E0D\u5F97\u4F4E\u4E8E canonical possible_burst \u5BF9\u6BD4\u5EA6\u3002"
  )
  hits <- match(values, names(exact))
  replace <- !is.na(hits)
  values[replace] <- unname(exact[hits[replace]])

  replace_pattern <- function(pattern, replacement) {
    hit <- grepl(pattern, values, perl = TRUE)
    values[hit] <<- sub(pattern, replacement, values[hit], perl = TRUE)
  }
  replace_pattern(
    "^type mismatch: expected ([^,]+), got (.+)$",
    "\u7C7B\u578B\u4E0D\u5339\u914D\uFF1A\u5E94\u4E3A \\1\uFF0C\u5B9E\u9645\u4E3A \\2\u3002"
  )
  replace_pattern(
    "^value outside choices: (.*)$",
    "\u53D6\u503C\u4E0D\u5728\u5141\u8BB8\u7684\u9009\u9879\u4E2D\uFF1A\\1\u3002"
  )
  replace_pattern(
    "^value below contract minimum (.*)$",
    "\u6570\u503C\u4F4E\u4E8E contract \u6700\u5C0F\u503C \\1\u3002"
  )
  replace_pattern(
    "^value above contract maximum (.*)$",
    "\u6570\u503C\u9AD8\u4E8E contract \u6700\u5927\u503C \\1\u3002"
  )
  values
}

stpd_ui_localize_parameter_issues <- function(issues, lang = "zh") {
  if (!is.data.frame(issues) || !"issue" %in% names(issues)) return(issues)
  issues$issue <- stpd_ui_localize_parameter_issue_text(issues$issue, lang = lang)
  issues
}

stpd_i18n_json_quote <- function(x) {
  x <- enc2utf8(as.character(x %||% ""))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  # Keep generated dictionaries inert inside an inline <script>. These are
  # valid JSON escapes and prevent a future dictionary value containing an
  # HTML closing tag from terminating the script element.
  x <- gsub("<", "\\u003C", x, fixed = TRUE)
  x <- gsub(">", "\\u003E", x, fixed = TRUE)
  x <- gsub("&", "\\u0026", x, fixed = TRUE)
  x <- gsub("\u2028", "\\u2028", x, fixed = TRUE)
  x <- gsub("\u2029", "\\u2029", x, fixed = TRUE)
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

stpd_i18n_supplemental_exact_dictionary <- function() {
  c(
    "\u5168\u5C40\u9884\u89C8\uFF1A\u9634\u5F71\u533A\u57DF\u8868\u793A\u5F53\u524D\u65F6\u95F4\u7A97" = "Global overview: the shaded region marks the current time window",
    "\u5BA1\u8BA1/\u5EFA\u8BAE\u9608\u503C\u3002\u5BF9\u4E8E\u8FDE\u7EED\u9AD8\u9891\u65F6\u671F\uFF0CISI \u6570\u901A\u5E38\u7B49\u4E8E spike \u6570\u51CF 1\uFF1B\u4F2A\u8FF9 ISI \u4E0D\u80FD\u4F5C\u4E3A\u6709\u6548\u652F\u6301\u8BC1\u636E\u3002" =
      "Audit/recommendation threshold. For a contiguous high-frequency epoch, ISI count is usually spike count minus 1; artifact ISIs are not valid support.",
    "\u56DE\u5230\u57FA\u7EBF\u7684\u5BB9\u5DEE" = "Return-to-baseline tolerance",
    "\u524D/\u540E ISI \u56DE\u5230\u8FDC\u7AEF\u57FA\u7EBF\u7684\u500D\u6570\u5BB9\u5DEE\u3002" = "Tolerance multiplier for pre/post ISIs returning to the remote baseline.",
    "\u5019\u9009\u4E0E\u8FDC\u7AEF\u4E0A\u4E0B\u6587\u4E4B\u95F4\u7684\u95F4\u9694\uFF08ISI \u6570\uFF09" = "Gap between the candidate and remote context (ISI count)",
    "\u524D/\u540E ISI \u6BD4\u503C" = "pre/post ratio",
    "\u4E0B\u4E00\u6B65 \u0394 logISI" = "next \u0394 logISI",
    "\u72B6\u6001\u7A7A\u95F4\u7F29\u653E" = "State-space scaling",
    "\u7A33\u5065\u7F29\u653E\uFF08\u4E2D\u4F4D\u6570/MAD\uFF09" = "Robust median/MAD",
    "Z-score \u6807\u51C6\u5316" = "Z-score",
    "\u5BF9\u6781\u7AEF logISI \u505A Winsorize \u622A\u5C3E" = "Winsorize extreme logISI values",
    "\u66FF\u4EE3\u6570\u636E\uFF08surrogate\uFF09\u6B21\u6570" = "Surrogate count",
    "\u5757\u7F6E\u6362\uFF08block shuffle\uFF09\u5757\u957F\uFF08ISI \u6570\uFF09" = "Block-shuffle length (ISI count)",
    "Diffusion / PHATE \u8FD1\u90BB\u6570 k" = "Diffusion / PHATE kNN",
    "Isomap \u626B\u63CF k" = "Isomap sweep k",
    "RQA \u590D\u73B0\u7387" = "RQA recurrence rate",
    "GMM \u5019\u9009\u72B6\u6001\u6570" = "GMM candidate state count",
    "\u65E0\u6807\u7B7E ISI \u7279\u5F81\uFF5C\u6807\u7B7E\u4EC5\u4F5C\u53E0\u52A0\u663E\u793A" = "Label-free ISI features | labels as overlays",
    "PCA \u65B9\u5DEE / \u8F7D\u8377" = "PCA variance / loading",
    "\u89E3\u91CA\u65B9\u5DEE" = "Explained variance",
    "\u7279\u5F81\u8F7D\u8377" = "Feature loading",
    "Isomap \u8BCA\u65AD" = "Isomap diagnostics",
    "\u8F6C\u79FB\u77E9\u9635" = "Transition matrix",
    "\u8F6C\u79FB\u8868" = "Transition table",
    "\u9A7B\u7559\u65F6\u95F4" = "Dwell time",
    "\u9A7B\u7559\u533A\u6BB5" = "Dwell segments",
    "\u8F6C\u79FB\u71B5" = "Transition entropy",
    "\u5E8F\u5217\u6A21\u4F53\u9891\u7387" = "Motif frequency",
    "\u66FF\u4EE3\u6570\u636E\u68C0\u9A8C" = "Surrogate tests",
    "\u6269\u6563\u6620\u5C04\uFF08Diffusion map\uFF09" = "Diffusion map",
    "RQA / \u590D\u73B0\u5206\u6790" = "RQA / recurrence analysis",
    "RQA \u6307\u6807" = "RQA metrics",
    "Isomap \u53C2\u6570\u626B\u63CF" = "Isomap sweep",
    "\u89C4\u5219 / GMM" = "Rule / GMM",
    "\u57FA\u4E8E\u89C4\u5219\u7684\u72B6\u6001" = "Rule-based states",
    "GMM \u8BCA\u65AD" = "GMM diagnostics",
    "GMM \u72B6\u6001\u7EDF\u8BA1" = "GMM state stats",
    "\u89E3\u7801\u533A\u6BB5" = "Decoded segments",
    "\u6807\u7B7E\u4E00\u81F4\u6027" = "Label agreement",
    "\u6A21\u578B\u6570\u636E" = "Model data",
    "\u6A21\u578B\u6458\u8981" = "Model summary",
    "logISI \u76F8\u56FE" = "logISI phase portrait",
    "\u72B6\u6001\u8F68\u8FF9" = "State trajectory",
    "\u591A train \u6A21\u5F0F\u72B6\u6001\u8F68\u8FF9\uFF5C\u975E\u540C\u6B65\u8BB0\u5F55\u65F6\u6309\u4F2A\u7FA4\u4F53\u89E3\u91CA" = "Multi-train pattern-state trajectory | pseudo-population when trains are not simultaneous",
    "\u65F6\u95F4 bin \u5BBD\u5EA6\uFF08ms\uFF09" = "Bin width (ms)",
    "\u9AD8\u65AF\u5E73\u6ED1 \u03C3\uFF08bin\uFF09" = "Gaussian smoothing sigma (bins)",
    "\u8D77\u59CB\u65F6\u95F4\uFF08s\uFF09" = "Start (s)",
    "\u7ED3\u675F\u65F6\u95F4\uFF08s\uFF1B0 = \u81EA\u52A8\uFF09" = "End (s; 0 = auto)",
    "3D \u5750\u6807" = "3D coordinates",
    "\u76F4\u63A5\u6A21\u5F0F\u5750\u6807\u8F74" = "Direct pattern axes",
    "PCA\uFF1A\u7EBF\u6027\u6B63\u4EA4\u65B9\u5DEE\u8F74" = "PCA: linear orthogonal variance axes",
    "\u56E0\u5B50\u5206\u6790\uFF1A\u7EBF\u6027\u9AD8\u65AF\u6F5C\u5728\u56E0\u5B50" = "Factor analysis: linear Gaussian latent factors",
    "Isomap\uFF1A\u6D4B\u5730\u6D41\u5F62\u5D4C\u5165" = "Isomap: geodesic manifold embedding",
    "t-SNE\uFF1A\u5C40\u90E8\u90BB\u57DF\u5D4C\u5165" = "t-SNE: local-neighborhood embedding",
    "UMAP\uFF1A\u6A21\u7CCA\u62D3\u6251\u5D4C\u5165" = "UMAP: fuzzy topological embedding",
    "Z \u8F74" = "Z axis",
    "burst \u5BB6\u65CF\u653E\u7535\u7387\uFF08Hz/train\uFF09" = "Burst-family rate (Hz/train)",
    "pause \u5360\u636E\u6BD4\u4F8B" = "Pause occupancy fraction",
    "tonic \u5BB6\u65CF\u653E\u7535\u7387\uFF08Hz/train\uFF09" = "Tonic-family rate (Hz/train)",
    "HF spiking \u653E\u7535\u7387\uFF08Hz/train\uFF09" = "HF spiking rate (Hz/train)",
    "HF spiking \u5360\u636E\u6BD4\u4F8B" = "HF spiking occupancy fraction",
    "\u603B\u4F53\u653E\u7535\u7387\uFF08Hz/train\uFF09" = "Overall firing rate (Hz/train)",
    "burst \u5BB6\u65CF\u5360\u636E\u6BD4\u4F8B" = "Burst-family occupancy fraction",
    "tonic \u5BB6\u65CF\u5360\u636E\u6BD4\u4F8B" = "Tonic-family occupancy fraction",
    "others \u5360\u636E\u6BD4\u4F8B" = "Others occupancy fraction",
    "\u672A\u6807\u8BB0\u5360\u636E\u6BD4\u4F8B" = "Unlabeled occupancy fraction",
    "\u8FD1\u90BB\u6570" = "Nearest neighbors",
    "t-SNE \u56F0\u60D1\u5EA6" = "t-SNE perplexity",
    "\u5D4C\u5165\u968F\u673A\u79CD\u5B50" = "Embedding random seed",
    "\u6700\u5927\u5D4C\u5165 bin \u6570" = "Max embedded bins",
    "\u540C\u6B65\u8BB0\u5F55\u65F6\u8BF7\u4F7F\u7528\u539F\u59CB timestamp \u6A21\u5F0F\u3002\u76F4\u63A5\u5750\u6807\u8F74\u5C55\u793A\u53EF\u89E3\u91CA\u7684\u6A21\u5F0F\u5BB6\u65CF\u6D3B\u52A8\u3002PCA \u548C\u56E0\u5B50\u5206\u6790\u662F\u7EBF\u6027\u6458\u8981\uFF1BIsomap\u3001t-SNE \u548C UMAP \u662F\u63A2\u7D22\u6027\u975E\u7EBF\u6027\u5D4C\u5165\uFF0C\u5E94\u5C06\u5176\u89E3\u91CA\u4E3A\u90BB\u57DF/\u51E0\u4F55\u89C6\u56FE\uFF0C\u800C\u4E0D\u662F\u9884\u6D4B\u5206\u7C7B\u5668\u3002\u70B9\u989C\u8272\u53D6\u81EA\u672A\u5E73\u6ED1\u7684\u9010 bin \u6A21\u5F0F\u5360\u636E\u72B6\u6001\uFF0C\u5750\u6807\u53EF\u9009\u62E9\u5E73\u6ED1\u3002\u82E5\u5404 train \u5E76\u975E\u540C\u6B65\u8BB0\u5F55\uFF0C\u5E94\u5C06\u7ED3\u679C\u89E3\u91CA\u4E3A\u6A21\u5F0F\u72B6\u6001/\u4F2A\u7FA4\u4F53\u8F68\u8FF9\uFF0C\u800C\u4E0D\u662F\u4E25\u683C\u7684\u540C\u6B65\u795E\u7ECF\u6D41\u5F62\u3002" = "For simultaneous recordings, use raw timestamp mode. Direct axes show interpretable pattern-family activity. PCA and factor analysis are linear summaries; Isomap, t-SNE, and UMAP are exploratory nonlinear embeddings and should be interpreted as neighborhood/geometry views, not predictive classifiers. Point color is assigned from unsmoothed per-bin pattern occupancy, while coordinates can optionally be smoothed. If trains were not recorded simultaneously, interpret the result as a pattern-state / pseudo-population trajectory, not a strict simultaneous neural manifold.",
    "\u8054\u5408\u72B6\u6001\u56FE / \u72B6\u6001\u5BF9\u77E9\u9635" = "Joint-state map / state-pair matrix",
    "\u8054\u5408\u72B6\u6001\uFF1A\u89C2\u6D4B\u503C\u4E0E\u671F\u671B\u503C" = "Observed vs expected joint states",
    "\u8054\u5408\u72B6\u6001\u8F6C\u79FB" = "Joint-state transitions",
    "\u9010 bin \u8054\u5408\u72B6\u6001" = "Per-bin joint states",
    "\u72B6\u6001\u8F68\u8FF9\u6570\u636E" = "State trajectory data",
    "\u65F6\u95F4 bin \u7279\u5F81" = "Time-bin features",
    "\u5D4C\u5165\u8BCA\u65AD" = "Embedding diagnostics",
    "\u7EBF\u6027\u8F7D\u8377" = "Linear loadings",
    "\u4E3B\u5BFC\u72B6\u6001\u8F6C\u79FB" = "Dominant-state transitions",
    "\u4E8B\u4EF6\u5BF9\u9F50 raster\u3001PSTH\u3001\u7FA4\u4F53\u653E\u7535\u7387\u3001\u795E\u7ECF\u5143\u70ED\u56FE\u4E0E spike-count \u540C\u6B65\u6027" = "Event-aligned raster, PSTH, population rate, neuron heatmap, and spike-count synchrony",
    "\u4E8B\u4EF6\u524D\u7A97\u53E3\uFF08s\uFF09" = "Pre-event window (s)",
    "\u4E8B\u4EF6\u540E\u7A97\u53E3\uFF08s\uFF09" = "Post-event window (s)",
    "\u57FA\u7EBF\u8D77\u59CB\u65F6\u95F4\uFF08s\uFF09" = "Baseline start (s)",
    "\u57FA\u7EBF\u7ED3\u675F\u65F6\u95F4\uFF08s\uFF09" = "Baseline end (s)",
    "\u60AC\u505C\u63D0\u793A\u4E2D\u7684 spike \u6807\u7B7E\u6765\u6E90" = "Spike label source for hover",
    "\u4E92\u76F8\u5173\u56FE\u6700\u5927\u65F6\u6EDE\uFF08ms\uFF09" = "Cross-correlogram max lag (ms)",
    "\u4E92\u76F8\u5173\u56FE bin \u5BBD\u5EA6\uFF08ms\uFF09" = "Cross-correlogram bin (ms)",
    "\u6700\u5927\u4E92\u76F8\u5173\u914D\u5BF9\u6570" = "Max correlogram pairs",
    "raster \u6700\u5927\u663E\u793A spike \u6570" = "Max raster spikes displayed",
    "\u795E\u7ECF\u5143\u7EA7\u653E\u7535\u7387\u8BC1\u636E" = "Neuron-level firing-rate evidence",
    "\u540C\u6B65\u6027 / \u76F8\u5173\u6027" = "Synchrony / correlation",
    "\u4E8B\u4EF6\u5BF9\u9F50\u6570\u636E\u8868" = "Event-aligned data tables",
    "\u6458\u8981" = "Summary",
    "\u7FA4\u4F53 PSTH" = "Population PSTH",
    "\u795E\u7ECF\u5143 PSTH" = "Neuron PSTH",
    "spike-count \u76F8\u5173\u6027" = "Spike-count correlation",
    "\u7FA4\u4F53 spike-count / \u653E\u7535\u7387\u6D41\u5F62\uFF5C\u4E8B\u4EF6\u6807\u7B7E\u4EC5\u4F5C\u4E8B\u540E\u6CE8\u91CA" = "Population spike-count / firing-rate manifold | event labels are post hoc annotations",
    "\u6D3B\u52A8\u91CF\u53D8\u6362" = "Activity transform",
    "log1p \u653E\u7535\u7387" = "log1p firing rate",
    "\u653E\u7535\u7387\uFF08Hz\uFF09" = "Firing rate (Hz)",
    "\u539F\u59CB spike count" = "Raw spike count",
    "\u795E\u7ECF\u5143\u5C3A\u5EA6\u53D8\u6362" = "Neuron scaling",
    "\u4E0D\u7F29\u653E" = "None",
    "\u8D77\u59CB\u65F6\u95F4\uFF08s\uFF1B\u7559\u7A7A/0 = \u81EA\u52A8\uFF09" = "Start (s; blank/0 = auto)",
    "3D \u6D41\u5F62\u65B9\u6CD5" = "3D manifold method",
    "PCA\uFF1A\u900F\u660E\u7684\u7EBF\u6027\u57FA\u7EBF" = "PCA: transparent linear baseline",
    "FA\uFF1A\u5171\u4EAB\u53D8\u5F02 / \u79C1\u6709\u566A\u58F0" = "FA: shared variability / private noise",
    "GPFA \u98CE\u683C\uFF1A\u5E73\u6ED1\u7684 FA \u8F68\u8FF9" = "GPFA-style: smoothed FA trajectory",
    "Isomap\uFF1A\u6D4B\u5730\u6D41\u5F62" = "Isomap: geodesic manifold",
    "PHATE\uFF1A\u8FDB\u7A0B / \u5206\u652F\u89C6\u56FE" = "PHATE: progression / branch view",
    "t-SNE\uFF1A\u5C40\u90E8\u90BB\u57DF\u89C6\u56FE" = "t-SNE: local-neighborhood view",
    "CEBRA \u98CE\u683C\uFF1A\u76D1\u7763\u5F0F\u884C\u4E3A\u8F74" = "CEBRA-style supervised behavior axis",
    "PHATE \u6269\u6563\u65F6\u95F4" = "PHATE diffusion time",
    "\u4E8B\u4EF6\u6807\u7B7E\u6765\u6E90" = "Event label source",
    "\u4E8B\u4EF6\u7F6E\u6362 / \u65F6\u95F4\u5E73\u79FB\u5BF9\u7167\u6B21\u6570" = "Event permutation / shift controls",
    "\u4E8B\u4EF6\u89E6\u53D1\u7A97\u53E3\uFF08bin\uFF09" = "Event-trigger window (bins)",
    "\u884C\u4E3A / \u8FD0\u52A8 CSV" = "Behavior / movement CSV",
    "\u7528\u4E8E sliceTCA \u7684 trial / \u8FD0\u52A8\u4E8B\u4EF6 CSV" = "Trial / movement-event CSV for sliceTCA",
    "\u4F7F\u7528\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u5185\u5D4C\u7684\u4EFB\u52A1\u4E8B\u4EF6" = "Use task events embedded in current dataset",
    "\u6D41\u5F62\u6CE8\u91CA\u7684\u4EFB\u52A1\u4E8B\u4EF6\u524D\u7A97\u53E3\uFF08s\uFF09" = "Task-event pre window for manifold annotation (s)",
    "\u6D41\u5F62\u6CE8\u91CA\u7684\u4EFB\u52A1\u4E8B\u4EF6\u540E\u7A97\u53E3\uFF08s\uFF09" = "Task-event post window for manifold annotation (s)",
    "sliceTCA \u4E8B\u4EF6\u524D\u7A97\u53E3\uFF08s\uFF09" = "sliceTCA pre-event window (s)",
    "sliceTCA \u4E8B\u4EF6\u540E\u7A97\u53E3\uFF08s\uFF09" = "sliceTCA post-event window (s)",
    "sliceTCA \u79E9\uFF08trial, neuron, time\uFF09" = "sliceTCA ranks trial,neuron,time",
    "sliceTCA \u6700\u5927\u8FED\u4EE3\u6B21\u6570\uFF08max_iter\uFF09" = "sliceTCA max_iter",
    "sliceTCA \u5B66\u4E60\u7387\uFF08learning_rate\uFF09" = "sliceTCA learning_rate",
    "\u8FD0\u884C Python sliceTCA \u540E\u7AEF" = "Run Python sliceTCA backend",
    "\u53EF\u7528\u65F6\u7ED8\u5236 sliceTCA \u91CD\u5EFA\u7ED3\u679C" = "Plot sliceTCA reconstruction when available",
    "\u6761\u4EF6\u5141\u8BB8\u65F6\u5E94\u4F18\u5148\u7528\u672C\u9762\u677F\u5206\u6790\u540C\u6B65\u8BB0\u5F55\u3002\u5750\u6807\u7531\u5206 bin \u7684\u7FA4\u4F53\u6D3B\u52A8\u8BA1\u7B97\uFF0C\u800C\u4E0D\u662F\u7531 burst/pause/tonic \u6807\u7B7E\u751F\u6210\u3002\u884C\u4E3A\u53D8\u91CF\u53EA\u7528\u4E8E\u7740\u8272/\u89E3\u7801\u6216\u76D1\u7763\u5F0F\u884C\u4E3A\u8F74\u5206\u6790\u3002" = "Use this panel for simultaneous recordings whenever possible. Coordinates are computed from binned population activity, not burst/pause/tonic labels. Behavior variables are used for color/decoding or supervised behavior-axis analysis.",
    "\u9A8C\u8BC1\u4E0E\u65B9\u6CD5\u8BF4\u660E" = "Validation and method notes",
    "\u9A8C\u8BC1\u6307\u6807" = "Validation metrics",
    "\u65B9\u6CD5\u5EFA\u8BAE" = "Method recommendations",
    "\u5F20\u91CF / sliceTCA" = "Tensor / sliceTCA",
    "sliceTCA \u5F20\u91CF\u6458\u8981" = "sliceTCA tensor summary",
    "Python \u540E\u7AEF\u8BCA\u65AD" = "Python backend diagnostics",
    "\u91CD\u5EFA\u6307\u6807" = "Reconstruction metrics",
    "trial-time \u5750\u6807 / \u4E8B\u4EF6\u6807\u7B7E" = "Trial-time coordinates / event labels",
    "\u4E8B\u4EF6\u72B6\u6001\u51E0\u4F55" = "Event-state geometry",
    "\u6309\u4E8B\u4EF6\u72B6\u6001\u6C47\u603B\u7684\u8D28\u5FC3 / \u79BB\u6563\u5EA6" = "Centroid / dispersion by event state",
    "\u72B6\u6001\u8DDD\u79BB\u4E0E\u5BF9\u7167" = "State distances and controls",
    "\u4E8B\u4EF6\u89E3\u7801\u4E0E\u52A8\u529B\u5B66" = "Event decoding and dynamics",
    "\u4E8B\u4EF6\u89E6\u53D1\u7684 3D \u8F68\u8FF9" = "Event-triggered 3D trajectory",
    "\u7FA4\u4F53\u77E9\u9635\u4E0E\u8F7D\u8377" = "Population matrix and loadings",
    "\u65F6\u95F4 bin \u6D3B\u52A8 / \u5750\u6807" = "Time-bin activity / coordinates",
    "\u7EBF\u6027\u8F7D\u8377 / \u79C1\u6709\u65B9\u5DEE" = "Linear loadings / private variance",
    "\u7A97\u53E3\u6458\u8981" = "Window summary",
    "Spike Train Pattern Detector \u662F\u5019\u9009\u4E8B\u4EF6\u751F\u6210\u5668\u548C\u534A\u76D1\u7763\u590D\u6838\u5E73\u53F0\uFF0C\u4E0D\u662F\u65E0\u504F\u7684\u6700\u7EC8\u771F\u503C\u5206\u7C7B\u5668\u3002\u62A5\u544A\u7ED3\u679C\u65F6\uFF0C\u5E94\u533A\u5206\u9AD8\u7F6E\u4FE1\u5EA6\u4E8B\u4EF6\u3001\u590D\u6838\u5019\u9009\u548C\u6A21\u5F0F\u5BB6\u65CF\u7EA7\u6458\u8981\uFF0C\u5E76\u5728\u53EF\u884C\u65F6\u63D0\u4F9B\u9A8C\u8BC1\u7ED3\u679C\u3002" = "Spike Train Pattern Detector is a candidate-event generator and semi-supervised review platform, not an unbiased final ground-truth classifier. Results should be reported as high-confidence events, review candidates, and family-level summaries with validation where available.",
    "\u5E94\u5206\u522B\u62A5\u544A\u9AD8\u7F6E\u4FE1\u5EA6\u4E8B\u4EF6\u3001\u590D\u6838\u5019\u9009\uFF08possible_burst\uFF09\u548C burst \u5BB6\u65CF\u6458\u8981\u3002" = "High-confidence events, review candidates (possible_burst), and burst-family summaries should be reported separately.",
    "long_burst \u662F\u4F9D\u636E spike \u6570\u3001\u6301\u7EED\u65F6\u95F4\u3001\u77ED ISI \u6BD4\u4F8B\u548C\u4E24\u4FA7\u5BF9\u6BD4\u5EA6\u5B9A\u4E49\u7684\u7ED3\u6784/\u4E8B\u4EF6\u578B\u6807\u7B7E\uFF1B\u4E0D\u80FD\u81EA\u52A8\u5C06\u5176\u89E3\u91CA\u4E3A\u72EC\u7ACB\u7684\u751F\u7269\u5B66\u673A\u5236\u3002" = "long_burst is a structural/event-like label based on spike count, duration, short-ISI fraction, and flank contrast; it should not be interpreted automatically as a distinct biological mechanism.",
    "high_frequency_tonic \u548C high_frequency_spiking \u662F\u72B6\u6001/\u65F6\u671F\u578B\u6807\u7B7E\uFF1B\u5176\u751F\u7269\u5B66\u89E3\u91CA\u53D6\u51B3\u4E8E\u7EC6\u80DE\u7C7B\u578B\u3001\u5B9E\u9A8C\u5236\u5907\u548C spike sorting \u8D28\u91CF\u3002" = "high_frequency_tonic and high_frequency_spiking are state/epoch-style labels. Their biological interpretation depends on cell type, preparation, and spike-sorting quality.",
    "\u4F7F\u7528 MANUAL \u6807\u7B7E\u8FDB\u884C\u4EA4\u4E92\u5F0F\u8C03\u53C2\u53EF\u80FD\u5BFC\u81F4\u8FC7\u62DF\u5408\u3002\u7528\u4E8E\u53D1\u8868\u7EA7\u5206\u6790\u65F6\uFF0C\u5E94\u91C7\u7528\u7559\u51FA\u7684 train/\u6570\u636E\u96C6\u3001\u4E8B\u4EF6\u7EA7\u6307\u6807\uFF0C\u5E76\u62A5\u544A\u9884\u8BBE\u540D\u79F0\u548C params_hash\u3002" = "Interactive tuning with manual labels can overfit. For publication-grade analysis, use held-out trains/datasets, event-level metrics, and report the preset name and params_hash.",
    "\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u53EF\u80FD\u63D0\u793A spike sorting\u3001multi-unit \u6DF7\u6742\u6216 timestamp \u95EE\u9898\uFF1B\u9ED8\u8BA4\u7B56\u7565\u662F\u4FDD\u5B88\u590D\u6838/\u964D\u7EA7\uFF0C\u800C\u4E0D\u662F\u9759\u9ED8\u63A5\u53D7\u3002" = "Refractory-suspect ISIs indicate possible spike-sorting/multi-unit/timestamp issues; default handling is conservative review/demotion rather than silent acceptance.",
    "\u4FDD\u5B88\u578B single-unit" = "Conservative single-unit",
    "\u5747\u8861\u578B single-unit" = "Balanced single-unit",
    "\u7075\u654F\u63A2\u7D22\u578B" = "Sensitive exploratory",
    "\u5FEB\u901F\u653E\u7535\u4E2D\u95F4\u795E\u7ECF\u5143" = "Fast-spiking interneuron",
    "\u4E25\u683C\u5904\u7406\u4F2A\u8FF9/\u7591\u4F3C\u4E0D\u5E94\u671F ISI\uFF1B\u91C7\u7528\u4F18\u5148\u590D\u6838\u7684 possible_burst \u7B56\u7565\u3002" = "Strict artifact/refractory handling; review-first possible_burst policy.",
    "\u9ED8\u8BA4\u7684\u5747\u8861\u578B\u5019\u9009\u4E8B\u4EF6\u751F\u6210\u6A21\u5F0F\u3002" = "Default balanced candidate-generation mode.",
    "\u53EC\u56DE\u7387\u66F4\u9AD8\uFF0C\u590D\u6838\u5019\u9009\u66F4\u591A\uFF1B\u82E5\u65E0\u7559\u51FA\u9A8C\u8BC1\uFF0C\u4E0D\u5EFA\u8BAE\u76F4\u63A5\u7528\u4E8E\u6700\u7EC8\u53D1\u8868\u3002" = "Higher recall; more review candidates; not recommended for final publication without held-out validation.",
    "\u5141\u8BB8\u5C06\u7A33\u5B9A\u9AD8\u9891\u653E\u7535\u89E3\u91CA\u4E3A tonic\uFF1B\u5BF9 burst \u664B\u7EA7\u4FDD\u6301\u4FDD\u5B88\u3002" = "Allows stable high-rate tonic interpretation; conservative burst promotion.",
    "\u4EE5\u8B66\u544A\u4E3A\u4E3B\u7684\u4E0D\u5E94\u671F\u7B56\u7565\uFF1B\u9002\u7528\u4E8E\u6781\u77ED ISI \u53EF\u80FD\u53CD\u6620\u7FA4\u4F53\u6D3B\u52A8\u7684\u60C5\u51B5\u3002" = "Warn-focused refractory policy; suitable when very short ISIs may reflect population activity.",
    "\u7ED3\u6784\u578B long_burst \u5019\u9009\uFF1A\u4EC5\u6EE1\u8DB3\u4E8B\u4EF6\u578B\u5224\u636E\uFF1B\u5E94\u7ED3\u5408\u4E0A\u4E0B\u6587\u548C\u9A8C\u8BC1\uFF0C\u4E0E\u6301\u7EED\u9AD8\u653E\u7535\u7387\u65F6\u671F\u533A\u5206\u3002" = "Structural long_burst candidate: event-like criteria only; distinguish from sustained high-rate epoch by context and validation.",
    "\u590D\u6838\u5019\u9009\uFF1A" = "Review candidate: ",
    "\u9AD8\u7F6E\u4FE1\u5EA6\u8BC1\u636E\u4E0D\u8DB3" = "insufficient high-confidence evidence",
    "\u590D\u6838\u5019\u9009\uFF1A\u9AD8\u7F6E\u4FE1\u5EA6\u8BC1\u636E\u4E0D\u8DB3" = "Review candidate: insufficient high-confidence evidence",
    "\u9AD8\u9891\u65F6\u671F\u6807\u7B7E\uFF1A\u89E3\u91CA\u53D6\u51B3\u4E8E\u7EC6\u80DE\u7C7B\u578B\u548C spike sorting\uFF1B\u672A\u7ECF\u590D\u6838\u4E0D\u8981\u4E0E burst \u5408\u5E76\u3002" = "High-frequency epoch label: interpretation depends on cell type and spike sorting; do not merge with burst without review.",
    "\u5305\u542B\u7591\u4F3C\u4E0D\u5E94\u671F\u8BC1\u636E\uFF1B\u8BF7\u590D\u6838 spike sorting / multi-unit \u6DF7\u6742\u3002" = "Contains refractory-suspect evidence; review spike sorting / multi-unit contamination.",
    "\u5F53\u524D\u5B58\u50A8\u7684\u6700\u7EC8\u4E8B\u4EF6\u6570\u3002" = "Number of final events currently stored.",
    "Candidate ledger \u4EC5\u5305\u542B\u5019\u9009\u9636\u6BB5\u8BB0\u5F55\uFF1B\u7EAF tonic/\u9AD8\u9891\u8F93\u51FA\u65F6\u53EF\u4EE5\u4E3A\u7A7A\u3002" = "Candidate ledger contains candidate-stage records only; it can be empty for pure tonic/high-frequency outputs.",
    "Event ledger \u5C06\u6700\u7EC8\u63D0\u53D6\u4E8B\u4EF6\u4E0E\u5019\u9009\u8BB0\u5F55\u5206\u5F00\u5B58\u50A8\u3002" = "Event ledger stores final extracted events separately from candidates.",
    "\u7528\u4E8E\u5BA1\u8BA1/\u5BFC\u51FA\u7684\u7279\u5F81\u8868\u5E94\u7531 ledger \u6D3E\u751F\u3002" = "Feature table should be derived from ledger for audit/export.",
    "\u6309\u6A21\u5F0F\u7EDF\u8BA1\u7684\u6700\u7EC8\u4E8B\u4EF6\u6570\u3002" = "Final event count by pattern.",
    "Candidate ledger \u6700\u7EC8\u7C7B\u522B\u8BA1\u6570\u3002" = "Candidate ledger final class count.",
    "\u5019\u9009\u4FDD\u7559\u7528\u4E8E\u5BA1\u8BA1\uFF0C\u4F46\u672A\u5199\u5165 AUTO \u6807\u7B7E\u3002" = "Candidates retained for audit but not written to AUTO labels.",
    "\u9AD8\u7F6E\u4FE1\u5EA6\u5C42\u4E0D\u5305\u542B possible_burst\u3002" = "High-confidence layer excludes possible_burst.",
    "Review \u5C42\u5305\u542B possible_burst \u4E8B\u4EF6\u3002" = "Review layer contains possible_burst events.",
    "Burst-family \u5C42\u4EC5\u5305\u542B burst/long_burst/possible_burst \u4E8B\u4EF6\uFF0C\u7528\u4E8E\u89E3\u91CA\u5019\u9009\u53EC\u56DE\u3002" = "Burst-family layer contains only burst/long_burst/possible_burst events for candidate-recall interpretation.",
    "\u5728\u62A5\u544A\u4E2D\u5C06\u9AD8\u7F6E\u4FE1\u5EA6\u4E8B\u4EF6\u4E0E possible/\u590D\u6838\u5019\u9009\u5206\u5F00\u3002" = "Separate high-confidence events from possible/review candidates in reports.",
    "\u4F7F\u7528\u7559\u51FA\u7684 train \u6216\u6570\u636E\u96C6\u83B7\u5F97\u5C3D\u91CF\u65E0\u504F\u7684\u6027\u80FD\u4F30\u8BA1\u3002" = "Use held-out trains or datasets for unbiased performance estimates.",
    "\u62A5\u544A params_hash\u3001preset_name\u3001\u4E0D\u5E94\u671F\u7B56\u7565\u548C tonic-like \u7B56\u7565\u3002" = "Report params_hash, preset_name, refractory policy, and tonic-like policy.",
    "\u9664\u9010 ISI \u6DF7\u6DC6\u77E9\u9635\u5916\uFF0C\u8FD8\u5E94\u4F7F\u7528\u4E8B\u4EF6\u7EA7\u91CD\u53E0 / IoU\u3002" = "Use event-level overlap / IoU in addition to per-ISI confusion matrices.",
    "\u5BF9\u4E8E\u975E\u5E73\u7A33\u8BB0\u5F55\uFF0C\u5E94\u5148\u6309\u884C\u4E3A/\u5B9E\u9A8C\u72B6\u6001\u5206\u6BB5\uFF0C\u518D\u89E3\u91CA pause \u9608\u503C\u3002" = "For nonstationary recordings, segment by behavioral/experimental state before interpreting pause thresholds.",
    "possible_burst \u88AB\u6709\u610F\u8BBE\u8BA1\u4E3A\u53EF\u590D\u6838\u7C7B\u522B\uFF1B\u82E5\u9759\u9ED8\u5E76\u5165 burst\uFF0C\u4F1A\u5938\u5927 burst \u6027\u80FD\u3002" = "possible_burst is intentionally reviewable and can inflate burst performance if merged silently.",
    "\u7531 MANUAL \u6807\u7B7E\u5B66\u4E60\u7684\u8303\u56F4\u548C\u9608\u503C\u8C03\u8282\u53EF\u80FD\u5BF9\u6821\u51C6\u5B50\u96C6\u8FC7\u62DF\u5408\u3002" = "Manual-learned ranges and threshold tuning can overfit the calibration subset.",
    "\u53EF\u590D\u73B0\u6027\u8981\u6C42\u53C2\u6570\u53EF\u8FFD\u6EAF\u3002" = "Parameter traceability is required for reproducibility.",
    "\u9010 ISI \u6307\u6807\u53EF\u80FD\u9AD8\u4F30\u957F\u4E8B\u4EF6\u7684\u4E00\u81F4\u6027\uFF0C\u5E76\u4F4E\u4F30\u8FB9\u754C\u8BEF\u5DEE\u3002" = "Per-ISI metrics can overestimate agreement for long events and understate boundary errors.",
    "\u5168\u5C40\u4E2D\u4F4D\u6570\u4FDD\u62A4\u5047\u8BBE\u5B58\u5728\u6709\u610F\u4E49\u7684\u57FA\u7EBF\u5206\u5E03\uFF1B\u975E\u5E73\u7A33\u6570\u636E\u4F1A\u8FDD\u53CD\u8BE5\u5047\u8BBE\u3002" = "Global median guards assume a meaningful baseline distribution; nonstationary data violate this assumption.",
    "\u57FA\u7840\u53C2\u6570" = "Basic parameters",
    "\u57FA\u7840\u53C2\u6570\u654F\u611F\u6027" = "Basic parameter sensitivity",
    "\u57FA\u7840" = "Basic",
    "\u9AD8\u7EA7" = "Advanced",
    "\u5168\u90E8" = "All",
    "\u9ED8\u8BA4\u53EA\u5C55\u793A\u751F\u7269\u5B66\u7528\u6237\u6700\u5E38\u8C03\u7684\u57FA\u7840\u53C2\u6570\uFF1B\u9AD8\u7EA7 / \u4E13\u5BB6\u53C2\u6570\u4ECD\u53EF\u901A\u8FC7\u4E0A\u65B9\u7B5B\u9009\u8BBF\u95EE\u3002" = "By default, only the basic parameters most often adjusted by biological users are shown; advanced and expert parameters remain available through the filter above.",
    "\u65E7\u7248/\u8BCA\u65AD\uFF1A\u7ECF\u5178 burst bridge \u9608\u503C" =
      "Legacy/diagnostic: classic burst bridge threshold",
    "\u65E7\u7248/\u8BCA\u65AD\uFF1A\u7ECF\u5178 burst seed \u9608\u503C" =
      "Legacy/diagnostic: classic burst seed threshold",
    "\u65E7\u7248/\u8BCA\u65AD\uFF1AHF spiking \u6700\u5927\u5BB9\u5FCD ISI" =
      "Legacy/diagnostic: HF-spiking maximum tolerated ISI",
    "\u65E7\u7248/\u8BCA\u65AD\uFF1AHF \u72B6\u6001\u77ED ISI \u4E0A\u9650" =
      "Legacy/diagnostic: HF-state short-ISI upper bound",
    "\u65E7\u7248/\u8BCA\u65AD\uFF1A\u5F3A pause ISI \u9608\u503C" =
      "Legacy/diagnostic: strong-pause ISI threshold",
    "\u65E7\u7248/\u8BCA\u65AD\u517C\u5BB9\u53C2\u6570\u3002\u5F53\u524D\u6D3B\u52A8 AUTO event grammar \u4E0D\u6D88\u8D39\u6B64\u503C\uFF1B\u4EC5\u4F9B\u65E7\u7248/\u5907\u7528\u68C0\u6D4B\u548C\u8BCA\u65AD\u8BB0\u5F55\u4F7F\u7528\u3002" =
      "Legacy/diagnostic compatibility parameter. The active AUTO event grammar does not consume this value; it is retained only for legacy/fallback detection and diagnostic records.",
    "\u65E7\u7248/near-miss \u8BCA\u65AD\u517C\u5BB9\u53C2\u6570\u3002\u5F53\u524D\u6D3B\u52A8 AUTO event grammar \u7684 HF-spiking short band \u6765\u81EA\u89E3\u6790\u540E\u7684\u751F\u6548\u9608\u503C\uFF0C\u4E0D\u6D88\u8D39\u6B64\u503C\uFF1B\u4EC5\u4F9B\u517C\u5BB9\u8DEF\u5F84\u548C\u8BCA\u65AD\u8BB0\u5F55\u4F7F\u7528\u3002" =
      "Legacy/near-miss diagnostic compatibility parameter. The active AUTO event grammar obtains the HF-spiking short band from resolved effective thresholds and does not consume this value; it is retained only for compatibility paths and diagnostic records.",
    "\u65E7\u7248/near-miss \u8BCA\u65AD\u517C\u5BB9\u53C2\u6570\u3002\u5F53\u524D\u6D3B\u52A8 AUTO event grammar \u4E0D\u6D88\u8D39\u6B64\u503C\uFF1B\u4EC5\u4F9B\u65E7\u7248/near-miss \u8BCA\u65AD\u548C\u517C\u5BB9\u8BB0\u5F55\u4F7F\u7528\u3002" =
      "Legacy/near-miss diagnostic compatibility parameter. The active AUTO event grammar does not consume this value; it is retained only for legacy/near-miss diagnostics and compatibility records.",
    "\u9009\u62E9 user\u3001manual\u3001histogram \u548C default \u9608\u503C\u4E4B\u95F4\u7684\u4F18\u5148\u7EA7\u3002auto \u6309 user \u2192 manual \u2192 histogram \u2192 default \u89E3\u6790\uFF1B\u68C0\u6D4B\u5B9E\u9645\u4F7F\u7528\u7684\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Selects priority among user, manual, histogram, and default thresholds. auto resolves user \u2192 manual \u2192 histogram \u2192 default; see the \u2018Threshold source / effective detection threshold\u2019 table for the actual value and source used by detection.",
    "burst \u5185\u90E8\u53EF\u6865\u63A5\u7684\u4E2D\u7B49 ISI \u4E0A\u9650\u3002\u8C03\u9AD8\u4F1A\u8BA9\u4E8B\u4EF6\u8DE8\u8FC7\u66F4\u957F\u7684\u5C0F\u95F4\u9699\u5E76\u5408\u5E76\u4E3A\u540C\u4E00\u5019\u9009\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Upper bound for moderate ISIs that may be bridged inside a burst. Increasing it lets events span longer small gaps and merge into one candidate. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "burst \u6838\u5FC3\u76F8\u5BF9 pre/post \u80CC\u666F\u7684\u6700\u5C0F\u5BF9\u6BD4\u5EA6\u3002\u8C03\u9AD8\u66F4\u4FDD\u5B88\uFF0C\u8981\u6C42\u4E8B\u4EF6\u66F4\u7A81\u663E\uFF1B\u8C03\u4F4E\u4F1A\u589E\u52A0\u53EC\u56DE\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Minimum contrast of a burst core relative to its pre/post background. Increasing it is more conservative and requires a more prominent event; decreasing it increases recall. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "burst \u7D27\u51D1\u6838\u5FC3\u5141\u8BB8\u7684\u6700\u77ED ISI\u3002\u901A\u5E38\u8D34\u8FD1\u6709\u6548 ISI \u4E0B\u9650\uFF1B\u8C03\u4F4E\u53EF\u80FD\u7EB3\u5165\u4F2A\u8FF9\uFF0C\u8C03\u9AD8\u4F1A\u5FFD\u7565\u6700\u77ED\u7684\u6838\u5FC3\u95F4\u9694\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Shortest ISI allowed in a compact burst core. It is usually near the minimum valid ISI; decreasing it may admit artifacts, while increasing it ignores the shortest core intervals. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "burst \u7D27\u51D1\u6838\u5FC3\u7684\u77ED ISI \u4E0A\u9650\u3002\u8C03\u9AD8\u4F1A\u628A\u8F83\u6162\u7684 spike \u7C07\u7EB3\u5165 burst seed\uFF0C\u63D0\u9AD8\u53EC\u56DE\u4F46\u53EF\u80FD\u589E\u52A0\u5047\u9633\u6027\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Short-ISI upper bound for a compact burst core. Increasing it includes slower spike clusters in the burst seed, improving recall but potentially increasing false positives. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "\u89C4\u5219\u5F3A\u76F4\u53D1\u653E\u5141\u8BB8\u7684\u6700\u957F ISI\u3002\u8C03\u9AD8\u4F1A\u7EB3\u5165\u66F4\u6162\u7684\u7A33\u5B9A\u653E\u7535\uFF0C\u8C03\u4F4E\u4F1A\u66F4\u4E25\u683C\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Longest ISI allowed for regular tonic firing. Increasing it includes slower stable firing; decreasing it is stricter. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "\u89C4\u5219\u5F3A\u76F4\u53D1\u653E\u5141\u8BB8\u7684\u6700\u77ED ISI\u3002\u8C03\u9AD8\u4F1A\u6392\u9664\u8FC7\u5FEB\u7247\u6BB5\uFF0C\u5E2E\u52A9\u533A\u5206 HF tonic / HF spiking\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Shortest ISI allowed for regular tonic firing. Increasing it excludes overly fast fragments and helps distinguish HF tonic / HF spiking. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "\u8FDB\u5165 pause \u5019\u9009\u7684\u957F ISI \u8D77\u70B9\u9608\u503C\u3002\u8C03\u9AD8\u4F1A\u53EA\u4FDD\u7559\u66F4\u957F\u6682\u505C\uFF0C\u8C03\u4F4E\u4F1A\u589E\u52A0\u77ED\u6682\u505C\u5019\u9009\u3002\u5728 auto \u6765\u6E90\u7B56\u7565\u4E0B\uFF0C\u672C\u8F93\u5165\u4EC5\u4F5C\u4E3A default \u56DE\u9000\u503C\uFF1B\u68C0\u6D4B\u5B9E\u9645\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u503C\uFF0C\u5F53\u524D\u503C\u4E0E\u6765\u6E90\u8BF7\u67E5\u770B\u201C\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C\u201D\u8868\u3002" =
      "Long-ISI entry threshold for a pause candidate. Increasing it retains only longer pauses; decreasing it adds shorter pause candidates. Under the auto source policy, this input is only the default fallback; detection reads the resolved effective value. See the \u2018Threshold source / effective detection threshold\u2019 table for its current value and source.",
    "\u4ECE MANUAL HF \u5B66\u4E60\u65E7\u7248/near-miss \u8BCA\u65AD\u951A\u70B9" =
      "Learn legacy/near-miss diagnostic anchors from MANUAL HF",
    "tonic/pause \u951A\u70B9\u53EF\u7528\u4E8E\u5F53\u524D\u76F8\u5E94\u6D3B\u52A8\u5C42\u7684\u5C3A\u5EA6\u5B9A\u4F4D\uFF1BHF \u951A\u70B9\u4EC5\u4F9B\u65E7\u7248/near-miss \u8BCA\u65AD\uFF0C\u5F53\u524D\u6D3B\u52A8 AUTO event grammar \u4E0D\u6D88\u8D39 HF \u951A\u70B9\u3002\u6700\u7EC8 AUTO \u4ECD\u7531\u5C40\u90E8\u7ED3\u6784\u3001\u8FDE\u7EED\u6027\u548C\u5BF9\u6BD4\u51B3\u5B9A\u3002" =
      "Tonic/pause anchors may be used to scale the corresponding active layer; HF anchors are for legacy/near-miss diagnostics only and are not consumed by the active AUTO event grammar. Final AUTO decisions still depend on local structure, continuity, and contrast.",
    "\u65E7\u7248/near-miss \u8BCA\u65AD\u9AD8\u9891\u951A\u70B9" =
      "Legacy/near-miss diagnostic high-frequency anchors",
    "Core q90 \u4E0E\u6301\u7EED\u65F6\u95F4" = "Core q90 vs duration",
    "Core q90 \u4E0E LV" = "Core q90 vs LV"
  )
}

stpd_i18n_static_ui_exact_dictionary <- function() {
  c(
    "扫描范围" = "Scan range",
    "Tonic CV / LV / MM 规则性阈值" = "Tonic CV / LV / MM regularity thresholds",
    "Tonic State episode 数" = "Tonic State episode count",
    "Tonic direct-support ISI 数" = "Tonic direct-support ISI count",
    "Broad HFS State episode 数" = "Broad HFS State episode count",
    "Broad HFS direct-support ISI 数" = "Broad HFS direct-support ISI count",
    "Tonic–HFS 冲突裁决变化数" = "Tonic–HFS conflict-resolution change count",
    "Tonic 规则性范围会在参数合同允许的范围内逐个小幅收紧/放宽 CV/LV/MM，并直接显示 Tonic/Broad-HFS State episode、direct-support ISI 和边界仲裁的预期变化。这是 non-authoritative dry-run，不会自动采用阈值或改写正式结果；超过硬安全上限或缺少 LOGO 校准证据的放宽会明确标为 invalid/unavailable。" =
      "The Tonic regularity scan tightens or relaxes CV/LV/MM one small step at a time within the parameter contract and directly shows expected changes in Tonic/Broad-HFS State episodes, direct-support ISIs, and boundary arbitration. This non-authoritative dry run never adopts thresholds or rewrites formal results; relaxations beyond hard safety limits or without LOGO calibration evidence are explicitly marked invalid/unavailable.",
    "State episode 变化明细" = "State episode change details",
    "State direct-support ISI 变化明细" = "State direct-support ISI change details",
    "Tonic–HFS 冲突裁决变化明细" = "Tonic–HFS conflict-resolution change details",
    "预览是试运行：比较 AUTO Event、Tonic/Broad-HFS State、direct support 和边界仲裁差异。它不覆盖正式检测结果，也不会自动采用预览阈值。" =
      "The preview is a dry run comparing differences in AUTO Events, Tonic/Broad-HFS States, direct support, and boundary arbitration. It does not overwrite formal detection results or automatically adopt preview thresholds.",
    "试运行差异" = "Dry-run differences",
    "Event 数量与边界变化" = "Event count and boundary changes",
    "Tonic / Broad HFS State episode 变化" = "Tonic / Broad HFS State episode changes",
    "State direct-support ISI 变化" = "State direct-support ISI changes",
    "Tonic–HFS 边界仲裁变化" = "Tonic–HFS boundary-arbitration changes",
    "\u5404\u6A21\u5F0F\u4E13\u5C5E\u6700\u5C0FISI / \u6700\u5927ISI\u95E8\u63A7" = "Pattern-specific minimum-ISI / maximum-ISI gates",
    "\u53EF\u9009\u7684\u6700\u7EC8\u4E8B\u4EF6\u7EA7 ISI \u95E8\u63A7\u30020 \u8868\u793A\u4E0D\u542F\u7528\u5BF9\u5E94\u95E8\u63A7\u3002\u6570\u503C\u5355\u4F4D\u8DDF\u968F\u201C\u4F2A\u8FF9/\u4E0D\u5E94\u671F\u9608\u503C\u5355\u4F4D\u201D\u3002" =
      "Optional final event-level ISI gates. 0 disables the corresponding gate. Values use the Artifact / refractory-threshold unit.",
    "\u7B49\u5F85\u52A0\u8F7D" = "Waiting to load",
    "\u4F7F\u7528\u89E3\u6790\u51FA\u7684 train \u5143\u6570\u636E\u8FC7\u6EE4\u5668" = "Use the parsed train-metadata filters",
    "\u89E3\u6790\u51FA\u7684 train \u5143\u6570\u636E / \u5217\u540D\u5206\u7EC4" = "Parsed train metadata / column-name grouping",
    "\u7CFB\u7EDF\u4F1A\u5C3D\u53EF\u80FD\u4ECE spike-train \u5217\u540D\u63A8\u65AD\u7ED3\u6784\u3001\u5DE6\u53F3\u4FA7\u3001\u8F68\u8FF9\u3001\u6DF1\u5EA6\u3001wire/unit \u548C flag\u3002" =
      "Where possible, the system infers structure, laterality, trajectory, depth, wire/unit, and flag from spike-train column names.",
    "\u6BCF\u9875\u53EF\u89C1 train \u6570" = "Visible trains per page",
    "\u663E\u793A\u65F6\u95F4\u7A97\u957F\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "Display-window length (current unit)",
    "\u59CB\u7EC8\u7ED8\u5236\u624B\u52A8\u6807\u8BB0\u7684 others" = "Always show manually labeled others",
    "\u7ED8\u5236\u9009\u4E2D\u5019\u9009 / \u4E34\u754C\u5019\u9009\u9884\u89C8\u53E0\u52A0\u5C42" = "Show the selected-candidate / near-miss preview overlay",
    "\u7ED8\u5236\u88AB\u62D2\u7EDD/\u964D\u7EA7\u7684 burst \u7C7B\u5019\u9009\u5BA1\u8BA1\u53E0\u52A0\u5C42" = "Show the audit overlay for rejected/demoted burst-class candidates",
    "\u663E\u793A\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6\u865A\u7EBF" = "Show dashed task/behavior-event lines",
    "\u4E8B\u4EF6\u8DF3\u8F6C\u524D\u7A97\u53E3\uFF08s\uFF09" = "Pre-event jump window (s)",
    "\u4E8B\u4EF6\u8DF3\u8F6C\u540E\u7A97\u53E3\uFF08s\uFF09" = "Post-event jump window (s)",
    "\u8DF3\u8F6C\u5230\u9009\u4E2D\u4EFB\u52A1\u4E8B\u4EF6" = "Jump to the selected task event",
    "\u5728 FINAL \u89C6\u56FE\u4E2D\u5C06\u5269\u4F59\u6709\u6548 ISI \u81EA\u52A8\u6807\u4E3A\u201C\u5176\u4ED6\u201D" = "In the FINAL view, automatically label remaining valid ISIs as others",
    "\u6805\u683C\u56FE\u53E0\u52A0\u6807\u7B7E" = "Raster overlay labels",
    "\u6805\u683C\u56FE\u6807\u7B7E\u663E\u793A\uFF1A\u4EC5\u4F7F\u7528\u6A21\u5F0F\u989C\u8272\u6761\u5E26\uFF1B\u5782\u76F4\u8109\u51B2\u523B\u7EBF\u4FDD\u6301\u7EDF\u4E00\u9ED1\u8272\u5B9E\u7EBF\u3002" =
      "Raster labels use pattern-colored bands only; vertical spike ticks remain uniform solid black lines.",
    "\u6805\u683C\u56FE\u7EC6\u8282\u5C42\u7EA7" = "Raster detail level",
    "\u6BCF\u6B21\u65B0\u6846\u9009\u540E\u81EA\u52A8\u7528\u5F53\u524D\u6A21\u5F0F\u6807\u8BB0" = "After each new box selection, label automatically with the current pattern",
    "\u8BBE\u7F6E\u6240\u9009\u7C07 A" = "Set selected cluster A",
    "\u8BBE\u7F6E\u6240\u9009\u7C07 B" = "Set selected cluster B",
    "\u5728\u6240\u9009\u533A\u57DF\u5185\u8981\u6E05\u9664\u7684\u6A21\u5F0F\uFF08\u7A7A = \u4EFB\u610F\uFF09" = "Patterns to clear within the selected region (empty = any)",
    "Legacy \u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C\uFF08\u5355\u6807\u7B7E\u517C\u5BB9\u5C42\uFF09" = "Legacy final-audit result (single-label compatibility layer)",
    "\u751F\u6210/\u91CD\u5EFA\uFF08\u4FDD\u7559 possible\uFF09" = "Build/rebuild (keep possible)",
    "\u6E05\u9664\u5BA1\u8BA1\u5C42" = "Clear audit layer",
    "Legacy\uFF1Apossible_burst \u5355\u6807\u7B7E\u6279\u91CF\u5347\u7EA7\uFF08\u517C\u5BB9\u5DE5\u5177\uFF09" = "Legacy: possible_burst single-label bulk promotion (compatibility tool)",
    "\u5BA1\u8BA1\u8BB0\u5F55" = "Audit records",
    "\u5C06\u9884\u8BBE\u5E94\u7528\u5230\u5173\u952E\u53C2\u6570" = "Apply preset to key parameters",
    "\u4ECE MANUAL \u66F4\u65B0\u53C2\u6570" = "Update parameters from MANUAL",
    "\u9700\u8981\u68C0\u6D4B\u7684\u6A21\u5F0F" = "Patterns to detect",
    "\u4E13\u5BB6\uFF1A\u4E25\u683C\u53EA\u8FD0\u884C\u4E0A\u9762\u52FE\u9009\u7684\u6A21\u5F0F" = "Expert: run only the patterns selected above",
    "\u5173\u95ED\u65F6\uFF0C\u81EA\u52A8\u68C0\u6D4B\u4F1A\u81EA\u52A8\u8865\u5168 burst / tonic / pause / HF \u7B49\u9ED8\u8BA4\u6838\u5FC3\u6A21\u5F0F\uFF0C\u907F\u514D\u9690\u85CF\u591A\u9009\u6846\u8BEF\u5173\u6389 tonic \u6216 pause\u3002" =
      "When disabled, automatic detection adds the default core patterns, including burst / tonic / pause / HF, so hidden checkboxes cannot inadvertently disable tonic or pause.",
    "\u68C0\u6D4B\u5668\u5C06\u5269\u4F59\u6709\u6548 ISI \u586B\u5145\u4E3A others\uFF08AUTO\uFF09" = "Fill remaining valid ISIs as others (AUTO)",
    "\u4F30\u8BA1\u53C2\u6570" = "Estimate parameters",
    "\u5C06\u4F30\u8BA1\u503C\u5E94\u7528\u5230 UI" = "Apply estimates to the UI",
    "\u6240\u9009 train \u7684\u65E7\u7248\u767E\u5206\u4F4D\u533A\u95F4" = "Legacy percentile range for selected trains",
    "\u7EDD\u5BF9\u6700\u5C0F\u503C\uFF08\u7A7A = \u767E\u5206\u4F4D\uFF09" = "Absolute minimum (blank = percentile)",
    "\u7EDD\u5BF9\u6700\u5927\u503C\uFF08\u7A7A = \u767E\u5206\u4F4D\uFF09" = "Absolute maximum (blank = percentile)",
    "\u65E7\u7248\u5DF2\u4FDD\u5B58 burst \u8303\u56F4\u903B\u8F91" = "Saved legacy burst-range logic",
    "\u4EC5\u767E\u5206\u4F4D" = "Percentiles only",
    "\u4EC5\u7EDD\u5BF9\u503C" = "Absolute values only",
    "\u5C06\u5DF2\u4FDD\u5B58\u8303\u56F4\u4F5C\u4E3A\u786C\u7EA6\u675F" = "Treat saved ranges as hard constraints",
    "\u5F3A\u5236\u4F7F\u7528\u5B66\u4E60\u5F97\u5230\u7684\u7EDD\u5BF9\u4E0B\u754C" = "Enforce the learned absolute lower bound",
    "\u5B66\u4E60\u4E0A\u754C\u7684\u767E\u5206\u4F4D\u6269\u5C55" = "Percentile expansion of the learned upper bound",
    "\u5B66\u4E60\u4E0A\u754C\u7684 IQR/MAD \u6269\u5C55" = "IQR/MAD expansion of the learned upper bound",
    "\u4ECE\u624B\u52A8\u6807\u8BB0\u7684 burst \u6821\u51C6\u8303\u56F4" = "Calibrate range from manually labeled bursts",
    "\u7A97\u53E3\u957F\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "Window length (current unit)",
    "\u4EC5\u805A\u7126\u5355\u6761 train" = "Focus on one train only",
    "\u591A\u9762\u677F\u6700\u5927 train \u6570" = "Maximum trains in multiple panels",
    "\u6BCF\u6761 train \u5168\u65F6\u957F" = "Full duration for each train",
    "ISI \u7D22\u5F15" = "ISI index",
    "\u5C06\u9501\u5B9A\u53C2\u8003\u7EBF\u5E94\u7528\u5230\u6240\u6709\u53EF\u89C1\u9762\u677F" = "Apply the locked reference line to all visible panels",
    "\u53C2\u8003 -> burst \u7EBF" = "Reference -> burst line",
    "\u53C2\u8003 -> pause \u7EBF" = "Reference -> pause line",
    "\u53C2\u8003 -> tonic \u4E0B\u754C" = "Reference -> tonic lower bound",
    "\u53C2\u8003 -> tonic \u4E0A\u754C" = "Reference -> tonic upper bound",
    "\u9608\u503C\u7EBF\u89E3\u91CA" = "Threshold-line interpretation",
    "\u8F6F\u951A\u70B9\uFF08\u63A8\u8350\uFF0C\u7ED3\u6784\u8BED\u6CD5\u4ECD\u9700\u901A\u8FC7\uFF09" = "Soft anchor (recommended; structure grammar must still pass)",
    "\u786C\u9608\u503C\uFF08\u663E\u5F0F\u7EA6\u675F\uFF09" = "Hard threshold (explicit constraint)",
    "\u5F53\u524D\u5256\u9762 train(s)" = "Current profile train(s)",
    "\u81EA\u9009 train(s)" = "User-selected train(s)",
    "\u4FDD\u5B58\u540E\u7ACB\u5373\u91CD\u8DD1\u68C0\u6D4B\u5668" = "Rerun the detector immediately after saving",
    "\u4FDD\u5B58\u9608\u503C\u7EBF" = "Save threshold lines",
    "\u4FDD\u5B58\u5E76\u6309\u8FD9\u4E9B\u9608\u503C\u68C0\u6D4B" = "Save and detect using these thresholds",
    "\u65F6\u95F4\u539F\u70B9" = "Time origin",
    "\u6BCF\u6761 train \u4ECE\u9996\u4E2A spike \u5BF9\u9F50" = "Align each train to its first spike",
    "\u539F\u59CB timestamp\uFF08\u9700\u540C\u6B65\u8BB0\u5F55\uFF09" = "Original timestamp (requires simultaneous recordings)",
    "\u624B\u52A8\u4F18\u5148" = "MANUAL priority",
    "\u539F\u59CB timestamp\uFF08\u540C\u6B65\u8BB0\u5F55\uFF09" = "Original timestamp (simultaneous recordings)",
    "burst \u5185\u90E8 ISI" = "Intra-burst ISI",
    "\u957F\u7206\u53D1\u5185\u90E8 ISI" = "Intra-long-burst ISI",
    "\u7591\u4F3C burst \u5185\u90E8 ISI" = "Intra-possible-burst ISI",
    "burst \u524D ISI" = "Pre-burst ISI",
    "burst \u540E ISI" = "Post-burst ISI",
    "burst \u95F4\u9694" = "Inter-burst interval",
    "possible_burst \u95F4\u9694" = "Inter-possible_burst interval",
    "tonic \u5185\u90E8 ISI" = "Intra-tonic ISI",
    "\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E\u5185\u90E8 ISI" = "Intra-HF-tonic ISI",
    "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u5185\u90E8 ISI" = "Intra-HF-spiking ISI",
    "tonic \u524D ISI" = "Pre-tonic ISI",
    "tonic \u540E ISI" = "Post-tonic ISI",
    "tonic \u95F4\u9694" = "Inter-tonic interval",
    "pause \u6301\u7EED\u65F6\u95F4" = "Pause duration",
    "tonic \u524D LV" = "Pre-tonic LV",
    "tonic \u540E LV" = "Post-tonic LV",
    "burst \u5373\u65F6\u5BF9\u6BD4 min/q90" = "Burst immediate contrast: min/q90",
    "burst \u5373\u65F6\u5BF9\u6BD4 geom/q90" = "Burst immediate contrast: geom/q90",
    "burst \u5373\u65F6\u5BF9\u6BD4 pct/q90" = "Burst immediate contrast: pct/q90",
    "burst \u5373\u65F6\u5BF9\u6BD4 min/max" = "Burst immediate contrast: min/max",
    "burst \u5373\u65F6\u5BF9\u6BD4 geom/max" = "Burst immediate contrast: geom/max",
    "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 min/q90" = "Burst contextual contrast: min/q90",
    "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 geom/q90" = "Burst contextual contrast: geom/q90",
    "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 pct/q90" = "Burst contextual contrast: pct/q90",
    "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 min/max" = "Burst contextual contrast: min/max",
    "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 geom/max" = "Burst contextual contrast: geom/max",
    "possible \u4E0A\u4E0B\u6587\u5BF9\u6BD4 min/q90" = "Possible-burst contextual contrast: min/q90",
    "possible \u4E0A\u4E0B\u6587\u5BF9\u6BD4 geom/q90" = "Possible-burst contextual contrast: geom/q90",
    "\u6309\u6807\u7B7E\u5206\u7EC4\u7684 log10(ISI)" = "log10(ISI) grouped by label",
    "Bin \u5BBD\u5EA6\uFF08\u7EBF\u6027\u76F4\u65B9\u56FE\uFF1B\u5F53\u524D\u5355\u4F4D\u6216\u65E0\u91CF\u7EB2\uFF09" = "Bin width (linear histogram; current unit or dimensionless)",
    "\u53E0\u52A0\u5F52\u4E00\u5316\u539F\u59CB\u5206\u5E03\u4E0E train-balanced \u5206\u5E03" = "Overlay normalized raw and train-balanced distributions",
    "X \u8F74\u6700\u5927\u503C\uFF080 = \u81EA\u52A8\uFF1B\u5F53\u524D\u663E\u793A\u5355\u4F4D\uFF09" = "X-axis maximum (0 = auto; current display unit)",
    "Y \u8F74\u4F7F\u7528\u5BF9\u6570\u5C3A\u5EA6" = "Use a logarithmic Y axis",
    "\u663E\u793A\u6A21\u5F0F\u9608\u503C\u533A\u95F4\uFF08\u70B9\u9009\u6A21\u5F0F\u540E\u81EA\u52A8\u542F\u7528\uFF09" = "Show pattern threshold ranges (enabled automatically after selecting a pattern)",
    "\u663E\u793A\u4F2A\u8FF9 / \u4E0D\u5E94\u671F\u9608\u503C" = "Show artifact / refractory thresholds",
    "\u5F3A\u5236\u4F18\u5148\u4F7F\u7528\u7528\u6237\u8F93\u5165" = "Force user-input priority",
    "\u5F3A\u5236\u4F18\u5148\u4F7F\u7528\u624B\u52A8\u6807\u8BB0\u63A8\u5BFC" = "Force manual-label-derived priority",
    "\u4F7F\u7528\u9ED8\u8BA4\u503C" = "Use defaults",
    "\u663E\u793A\u54EA\u4E9B\u6A21\u5F0F\u533A\u95F4" = "Pattern ranges to show",
    "\u9AD8\u9891\u5F3A\u76F4 HF tonic" = "High-frequency tonic (HF tonic)",
    "\u663E\u793A\u54EA\u4E9B\u9608\u503C\u6765\u6E90" = "Threshold sources to show",
    "\u7528\u6237\u81EA\u5B9A\u4E49\u9608\u503C\u8986\u76D6\uFF08\u6700\u9AD8\u4F18\u5148\u7EA7\uFF09" = "User-defined threshold overrides (highest priority)",
    "burst seed \u4E0A\u754C" = "burst seed upper bound",
    "burst bridge \u4E0A\u754C" = "burst bridge upper bound",
    "burst \u5BF9\u6BD4\u5EA6 S" = "burst contrast S",
    "\u4E25\u683C\u8981\u6C42 burst \u5185 q95 \u2264 bridge \u4E0A\u754C\uFF08\u9ED8\u8BA4\u5173\u95ED\uFF09" = "Strictly require intra-burst q95 <= the bridge upper bound (off by default)",
    "HF spiking seed \u4E0A\u754C" = "HF spiking seed upper bound",
    "HF spiking bridge \u4E0A\u754C" = "HF spiking bridge upper bound",
    "HF tonic seed \u4E0A\u754C" = "HF tonic seed upper bound",
    "HF tonic bridge \u4E0A\u754C" = "HF tonic bridge upper bound",
    "tonic seed \u4E0A\u754C" = "tonic seed upper bound",
    "tonic bridge \u4E0A\u754C" = "tonic bridge upper bound",
    "pause seed \u4E0A\u754C" = "pause seed upper bound",
    "pause bridge \u4E0A\u754C" = "pause bridge upper bound",
    "\u767E\u5206\u4F4D\u5217\u663E\u793A\u6570\u636E\u96C6\u5C42\u7EA7 seed band \u5728\u6BCF\u6761 spike train \u5185\u7684\u4F4D\u7F6E\uFF1B\u8FD9\u4E9B\u662F\u7528\u4E8E\u8868\u578B\u89E3\u91CA\u7684\u8BCA\u65AD\u8F93\u51FA\u3002" =
      "Percentile columns show where the dataset-level seed band lies within each spike train; these are diagnostic outputs for phenotype interpretation.",
    "\u52A0\u6743 core ISI \u76F4\u65B9\u56FE" = "Weighted core-ISI histogram",
    "Core \u8303\u56F4\u56FE" = "Core range plot",
    "Core q90 \u9608\u503C\u5F71\u54CD" = "Effect of the core q90 threshold",
    "\u8FB9\u7F18\u5BF9\u6BD4 min \u76F4\u65B9\u56FE" = "Minimum edge-contrast histogram",
    "\u8FB9\u7F18\u5BF9\u6BD4 geom \u76F4\u65B9\u56FE" = "Geometric edge-contrast histogram",
    "Bin \u5BBD\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\u6216\u65E0\u91CF\u7EB2\uFF09" = "Bin width (current unit or dimensionless)",
    "\u5305\u542B\u88AB\u62D2\u7EDD\u7684\u7ED3\u6784\u5019\u9009" = "Include rejected structure candidates",
    "\u7ED3\u6784\u8868\u663E\u793A\u884C\u6570" = "Rows shown in the structure table",
    "Seed \u6301\u7EED\u65F6\u95F4" = "Seed duration",
    "Seed \u8FB9\u7F18\u5BF9\u6BD4 min" = "Seed edge contrast: min",
    "Seed \u8FB9\u7F18\u5BF9\u6BD4 geom" = "Seed edge contrast: geom",
    "Bridge \u539F\u59CB\u6700\u5927 ISI" = "Bridge raw maximum ISI",
    "Bridge / \u81A8\u80C0\u540E max(seed q90)" = "Bridge / post-expansion max(seed q90)",
    "Bridge / \u81A8\u80C0\u540E geom(seed q90)" = "Bridge / post-expansion geom(seed q90)",
    "Bridge \u5408\u5E76\u8FB9\u7F18\u5BF9\u6BD4 min" = "Merged-bridge edge contrast: min",
    "Bridge \u5408\u5E76\u8FB9\u7F18\u5BF9\u6BD4 geom" = "Merged-bridge edge contrast: geom",
    "\u6700\u7EC8\u5019\u9009\u8FB9\u7F18\u5BF9\u6BD4 min" = "Final-candidate edge contrast: min",
    "\u6700\u7EC8\u5019\u9009\u8BC4\u5206" = "Final-candidate score",
    "\u6700\u7EC8\u5019\u9009\u6301\u7EED\u65F6\u95F4" = "Final-candidate duration",
    "\u5305\u542B\u88AB\u62D2\u7EDD bridge/\u5019\u9009\u884C" = "Include rejected bridge/candidate rows",
    "\u8BCA\u65AD\u6563\u70B9\u56FE" = "Diagnostic scatterplot",
    "Seed q90 \u767E\u5206\u4F4D \u4E0E \u8BC4\u5206" = "Seed q90 percentile vs score",
    "Bridge \u767E\u5206\u4F4D \u4E0E \u6BD4\u7387" = "Bridge percentile vs ratio",
    "\u6700\u7EC8\u8BC4\u5206\u4E0E\u8FB9\u7F18\u5BF9\u6BD4" = "Final score vs edge contrast",
    "tonic \u7A97\u53E3" = "tonic window",
    "\u5355\u4E2A ISI" = "Single ISI",
    "\u663E\u793A\u6240\u9700\u76F8\u5BF9\u8C03\u6574 \u2264 \u8BE5\u503C\u7684\u5019\u9009" = "Show candidates requiring a relative adjustment <= this value",
    "\u5931\u8D25\u9608\u503C\u6700\u5C11\uFF0C\u7136\u540E\u8C03\u6574\u6700\u5C0F" = "Fewest failed thresholds, then smallest adjustment",
    "\u8C03\u6574\u6700\u5C0F" = "Smallest adjustment",
    "\u6700\u9AD8\u8BC4\u5206" = "Highest score",
    "\u5C06\u6240\u9009\u9879\u63A5\u53D7\u4E3A MANUAL \u6807\u7B7E" = "Accept the selected item as a MANUAL label",
    "\u652F\u6301\u65B9\u6CD5\u4E3A burst-ISI \u9608\u503C\u6821\u51C6\u63D0\u4F9B\u5916\u90E8\u8BC1\u636E\uFF1B\u4E0D\u4F1A\u5199\u5165 AUTO \u6807\u7B7E\uFF0C\u4E5F\u4E0D\u4F1A\u66FF\u4EE3\u4E3B\u68C0\u6D4B\u5668\u3002" =
      "Support methods provide external evidence for burst-ISI threshold calibration; they do not write AUTO labels or replace the primary detector.",
    "\u4EC5\u5728\u5F53\u524D\u53EF\u89C1 trains \u4E0A\u8FD0\u884C" = "Run only on currently visible trains",
    "\u6700\u5C0F\u8FDE\u7EED ISI \u6570 k" = "Minimum consecutive ISI count k",
    "\u6700\u5927\u8FDE\u7EED ISI \u6570 k\uFF080 = \u5B8C\u6574\u641C\u7D22\uFF09" = "Maximum consecutive ISI count k (0 = exhaustive search)",
    "\u6700\u5927\u6D4B\u8BD5\u7A97\u53E3\u4FDD\u62A4" = "Maximum test-window guard",
    "\u6BCF\u4E2A\u652F\u6301 burst \u7684\u6700\u5C0F spike \u6570" = "Minimum spikes per supported burst",
    "\u652F\u6301 burst \u6700\u5C0F\u6301\u7EED\u65F6\u95F4\uFF08ms\uFF1B0 = \u4E0D\u542F\u7528\uFF09" = "Minimum supported-burst duration (ms; 0 = disabled)",
    "\u4E0E burst-family \u5019\u9009\u7684\u91CD\u53E0\u6BD4\u4F8B" = "Overlap fraction with burst-family candidates",
    "\u5C06 Mean-ISI \u9608\u503C\u5BFC\u5165 burst \u53C2\u6570" = "Import Mean-ISI threshold into Burst parameters",
    "\u5C06 LogISI \u9608\u503C\u5BFC\u5165 burst \u53C2\u6570" = "Import LogISI threshold into Burst parameters",
    "\u5BFC\u5165\u65F6\u5BF9\u5DF2\u89E3\u6790 train \u7684\u9608\u503C\u53D6\u7A33\u5065\u4E2D\u4F4D\u6570\uFF0C\u4EC5\u66F4\u65B0 burst seed/bridge \u53C2\u6570\u5E76\u5207\u6362\u4E3A\u7528\u6237\u9608\u503C\u6765\u6E90\uFF1B\u9700\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u540E\u624D\u4F1A\u4EA7\u751F\u65B0 AUTO \u7ED3\u679C\u3002" =
      "Import takes the robust median across resolved trains, updates only the Burst seed/bridge parameters, and switches to the user threshold source; rerun detection to generate new AUTO results.",
    "\u652F\u6301 burst \u6761\u5E26\u5728\u54EA\u91CC\uFF1F" = "Where are the supported-burst bands?",
    "\u53E0\u52A0\u652F\u6301\u65B9\u6CD5\u68C0\u6D4B\u7ED3\u679C" = "Overlay support-method detections",
    "\u540C\u65F6\u7ED8\u5236\u534A\u900F\u660E\u4E8B\u4EF6\u5305\u7EDC\u6761" = "Also show translucent event-envelope bands",
    "\u540C\u65F6\u663E\u793A\u5F53\u524D AUTO burst-family \u6761\u5E26" = "Also show current AUTO burst-family bands",
    "\u5C06 Pasquale \u7B49\u4EBA\u7684 logISIH \u9608\u503C\u4F30\u8BA1\u548C newBD \u903B\u8F91\u4F5C\u4E3A\u652F\u6301\u5C42\u5B9E\u73B0\u3002" =
      "Implements the logISIH threshold estimation and newBD logic of Pasquale et al. as a support layer.",
    "burst \u5185\u5CF0\u503C\u7A97\u53E3\uFF08ms\uFF09" = "Intra-burst peak window (ms)",
    "Core \u53C2\u8003 maxISI1\uFF08ms\uFF09" = "Core reference maxISI1 (ms)",
    "\u5408\u7406 ISIth \u6700\u5927\u503C\uFF08ms\uFF09" = "Maximum plausible ISIth (ms)",
    "ISIth \u65E0\u6CD5\u89E3\u6790\u65F6\u56DE\u9000\u5230 100 ms CH-style \u68C0\u6D4B\u5668" = "Fall back to the 100 ms CH-style detector when ISIth cannot be resolved",
    "\u652F\u6301\u65B9\u6CD5\u68C0\u6D4B\u5230\u7684 burst \u5019\u9009\u4EE5\u65B9\u6CD5\u989C\u8272\u6761\u663E\u793A\u5728\u76F8\u90BB spike \u7684 ISI \u533A\u95F4\u4E0A\uFF1A\u4E0A\u65B9 ISI \u6761 = Mean-ISI (#8A7FFF)\uFF1B\u4E0B\u65B9 ISI \u6761 = LogISI / newBD (#F58E90)\u3002\u9ED1\u8272\u7AD6\u7EBF\u4ECD\u8868\u793A spike timestamp\u3002\u9ED8\u8BA4\u5173\u95ED\u534A\u900F\u660E\u4E8B\u4EF6\u5305\u7EDC\u3002\u542F\u7528\u65F6\uFF0C\u4E2D\u95F4\u6761\u5E26\u8868\u793A\u5F53\u524D AUTO burst-family\u3002" =
      "Burst candidates detected by support methods are shown as method-colored bands over ISI intervals between adjacent spikes: upper ISI band = Mean-ISI (#8A7FFF); lower ISI band = LogISI / newBD (#F58E90). Black vertical lines continue to represent spike timestamps. Translucent event envelopes are off by default; when enabled, the middle band represents the current AUTO burst-family.",
    "\u6309 train \u7684 ML \u9608\u503C" = "Per-train ML thresholds",
    "\u6309 train \u7684 ISIth \u9608\u503C" = "Per-train ISIth thresholds",
    "logISIH \u8868" = "logISIH table",
    "\u4E25\u683C\u9AD8\u7F6E\u4FE1\uFF1Apossible_burst \u5355\u72EC\u7EDF\u8BA1" = "Strict high confidence: report possible_burst separately",
    "\u590D\u6838\u8F85\u52A9\uFF1A\u5355\u72EC\u62A5\u544A\u6700\u7EC8\u590D\u6838\u6807\u7B7E" = "Review-assisted: report final reviewed labels separately",
    "\u624B\u52A8\u6807\u8BB0 ISI \u7684\u9010\u7C7B\u6307\u6807" = "Per-class metrics for manually labeled ISIs",
    "\u9A8C\u8BC1 train \u6BD4\u4F8B" = "Validation-train fraction",
    "\u4E25\u683C\u9AD8\u7F6E\u4FE1" = "Strict high confidence",
    "\u5019\u9009\u5BB6\u65CF / \u5019\u9009\u53EC\u56DE" = "Candidate family / candidate recall",
    "\u8FD0\u884C\u79D1\u5B66\u9A8C\u8BC1\u62A5\u544A" = "Run the scientific-validation report",
    "\u6700\u591A\u626B\u63CF\u53C2\u6570" = "Maximum parameters to scan",
    "\u6700\u591A\u626B\u63CF train" = "Maximum trains to scan",
    "\u5355\u53C2\u6570\u76F8\u5BF9\u6270\u52A8" = "Single-parameter relative perturbation",
    "\u66F2\u7EBF\u6307\u6807" = "Curve metric",
    "\u6821\u51C6\u6307\u6807" = "Calibration metrics",
    "\u9A8C\u8BC1\u4E8B\u4EF6\u5339\u914D" = "Validation-event matches",
    "\u4E8B\u4EF6\u7EA7\u6307\u6807\u4E0E\u53C2\u6570\u53D8\u4F53" = "Event-level metrics vs parameter variants",
    "\u5F53\u524D\u5185\u5B58\u4E2D\u7684\u6279\u5904\u7406" = "In-memory batch processing",
    "\u4F7F\u7528\u5F53\u524D UI \u53C2\u6570\u5904\u7406\u5185\u5B58\u4E2D\u5DF2\u52A0\u8F7D\u7684\u6240\u6709\u6570\u636E\u96C6\u3002\u7ED3\u679C\u53EF\u5BFC\u51FA\u4E3A\u5355\u4E2A ZIP\u3002\u547D\u4EE4\u884C\u4F7F\u7528\u65F6\u53EF\u8C03\u7528\u5305 API \u4E2D\u7684 stpd_detect() \u6216 run_detector()\u3002" =
      "Process all datasets currently loaded in memory with the current UI parameters. Results can be exported as a single ZIP. From the command line, use the package API stpd_detect() or run_detector().",
    "\u5BF9\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u8FD0\u884C\u68C0\u6D4B\u5668" = "Run the detector on all loaded datasets",
    "\u89E3\u91CA\u4E0E\u53EF\u590D\u73B0\u6027\u8BF4\u660E" = "Interpretation and reproducibility notes",
    "\u53C2\u6570\u62A5\u544A\uFF1A\u5F53\u524D\u503C\u4E0E\u9ED8\u8BA4\u503C / \u9884\u8BBE" = "Parameter report: current values vs defaults / preset",
    "\u4EC5\u4F9B\u53C2\u8003\uFF1A\u6ED1\u52A8\u4E2D\u4F4D ISI \u5927\u5E45\u6F02\u79FB\u63D0\u793A\u72B6\u6001\u53D8\u5316\uFF1Bpause/global \u9608\u503C\u53EF\u80FD\u9700\u8981\u5206\u6BB5\u5904\u7406\u3002" =
      "For reference only: large drift in the moving median ISI suggests a state change; pause/global thresholds may require segment-wise handling.",
    "\u5206\u5E03\u8BC1\u636E\u5C42\uFF08\u5019\u9009\u4E8B\u4EF6\uFF09" = "Distributional-evidence layer (candidate events)",
    "\u5BF9\u7167\u53C2\u6570" = "Baseline parameters",
    "\u6700\u8FD1\u6B63\u5F0F\u8FD0\u884C\u53C2\u6570" = "Most recent formal-run parameters",
    "\u6700\u591A train" = "Maximum trains",
    "\u5339\u914D IoU" = "Matching IoU",
    "\u5C06\u5F53\u524D UI \u53C2\u6570\u5199\u5165\u4E34\u65F6 YAML\uFF0C\u518D\u8BFB\u56DE\u5E76\u6BD4\u8F83 hash\uFF0C\u7528\u4E8E\u9632\u6B62\u5BFC\u5165/\u5BFC\u51FA\u4EA7\u751F\u9759\u9ED8\u6F02\u79FB\u3002" =
      "Write the current UI parameters to a temporary YAML file, read them back, and compare hashes to prevent silent drift during import/export.",
    "\u5C06\u8BE5\u533A\u95F4\u7528\u4E8E\u4FA7\u680F\u201C\u4FDD\u5B58\u8303\u56F4\u201D" = "Use this range for Save range in the sidebar",
    "\u4E3A\u6240\u9009 trains \u4FDD\u5B58\u8303\u56F4" = "Save ranges for selected trains",
    "\u6E05\u9664\u6240\u9009 trains \u7684\u8303\u56F4" = "Clear ranges for selected trains",
    "\u4ECE MANUAL bursts \u6821\u51C6 burst \u8303\u56F4" = "Calibrate the burst range from MANUAL bursts",
    "ISI \u8868" = "ISI table",
    "\u663E\u793A\u6307\u5B9A\u767E\u5206\u4F4D\u5185\u7684 ISI" = "Show ISIs within the specified percentiles",
    "\u624B\u52A8\u6821\u51C6\u7684 tonic-ISI \u8303\u56F4" = "Manually calibrated tonic-ISI ranges",
    "\u624B\u52A8\u6821\u51C6\u7684 pause-ISI \u8303\u56F4" = "Manually calibrated pause-ISI ranges",
    "QC \u6839\u636E timestamp \u548C ISI \u8BA1\u7B97\u3002\u8868\u683C\u5217\u4FDD\u7559\u6240\u6709 QC \u6307\u6807\uFF1Bwarning_message \u53EA\u5217\u51FA\u5F53\u524D\u89E6\u53D1\u7684 warning/error\u3002" =
      "QC is computed from timestamps and ISIs. The table retains all QC metrics; warning_message lists only currently triggered warnings/errors.",
    "\u4E0B\u65B9\u884C\u663E\u793A\u4F4E\u4E8E\u5F53\u524D\u4F2A\u8FF9/\u6700\u5C0F\u6709\u6548\u9608\u503C\u7684\u5177\u4F53 ISI \u533A\u95F4\u3002" =
      "The rows below show the specific ISI intervals below the current artifact/minimum-valid threshold.",
    "\u4E0B\u65B9\u884C\u8BC6\u522B\u5F53\u524D\u6392\u5E8F train \u4E2D\u7684\u5B8C\u5168\u91CD\u590D timestamp\u3002\u8FD9\u4E9B\u4F1A\u4EA7\u751F 0 ISI\uFF0C\u9664\u975E\u4F60\u5728\u5BFC\u5165\u65F6\u660E\u786E\u9009\u62E9\u8B66\u544A/\u5408\u5E76\u7B56\u7565\uFF0C\u5426\u5219\u4F1A\u88AB\u89C6\u4E3A\u6570\u636E\u5B8C\u6574\u6027\u9519\u8BEF\u3002" =
      "The rows below identify exact duplicate timestamps in the currently sorted trains. These produce 0 ISIs and are treated as data-integrity errors unless you explicitly choose a warn/merge policy during import.",
    "\u6700\u7EC8\u4F18\u5148\u7EA7/\u91CD\u53E0\u89E3\u6790\u540E\u88AB\u79FB\u9664\u7684\u7247\u6BB5\u3002\u4F8B\u5982\uFF0C\u77ED\u4E8E HF spiking \u6700\u5C0F spike \u6570\u7684\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u7247\u6BB5\u4F1A\u4ECE AUTO \u6807\u7B7E\u4E2D\u79FB\u9664\uFF0C\u800C\u4E0D\u4F1A\u663E\u793A\u4E3A\u6709\u6548\u4E8B\u4EF6\u3002" =
      "Segments removed after final priority/overlap resolution. For example, an HF-spiking segment shorter than the minimum HF-spiking spike count is removed from AUTO labels rather than shown as a valid event."
  )
}


stpd_i18n_exact_dictionary <- function() {
  out <- c(
    "\u8BED\u8A00 / Language" = "Language",
    "\u4E2D\u6587" = "Chinese",
    "\u6570\u636E" = "Data",
    "\u53C2\u6570" = "Parameters",
    "\u53C2\u6570\u9879" = "Params",
    "\u68C0\u6D4B" = "Detect",
    "\u6D4F\u89C8" = "Explore",
    "\u590D\u6838" = "Review",
    "\u9A8C\u8BC1" = "Validate",
    "\u5BFC\u51FA" = "Export",
    "\u5206\u6790" = "Analysis",
    "\u4E13\u5BB6" = "Expert / Diagnostics",
    "\u5BFC\u5165\u6570\u636E" = "Import data",
    "\u6D4F\u89C8\u2026" = "Browse...",
    "\u672A\u9009\u62E9\u6587\u4EF6" = "No file selected",
    "\u4E0A\u4F20\u539F\u59CB timestamp CSV" = "Upload raw timestamp CSV",
    "\u8F93\u5165\u5355\u4F4D" = "Input unit",
    "\u91CD\u590D timestamp \u5904\u7406\u7B56\u7565" = "Duplicate-timestamp policy",
    "\u8FD0\u884C\u68C0\u6D4B\u5668\u5F15\u64CE" = "Run detector engine",
    "\u8BBE\u7F6E\u5173\u952E\u53C2\u6570" = "Set key parameters",
    "\u590D\u6838\u4E0E\u9A8C\u8BC1" = "Review and validate",
    "\u5173\u952E\u53C2\u6570" = "Key parameters",
    "\u8FD0\u884C\u68C0\u6D4B" = "Run detection",
    "\u6A21\u5F0F\u68C0\u6D4B" = "Pattern detection",
    "\u8FD0\u884C\u6A21\u5F0F\u68C0\u6D4B" = "Run pattern detection",
    "\u5F00\u59CB\u6A21\u5F0F\u68C0\u6D4B" = "Start pattern detection",
    "\u4F7F\u7528\u5F53\u524D\u53C2\u6570\u68C0\u6D4B burst\u3001long burst\u3001tonic\u3001HF spiking \u548C pause \u7B49\u6838\u5FC3\u6A21\u5F0F\u3002" = "Use the current parameters to detect core patterns such as burst, long burst, tonic, HF spiking, and pause.",
    "\u6B63\u5F0F\u8FD0\u884C\u9ED8\u8BA4\u5904\u7406\u5168\u90E8 trains\uFF1B\u5982\u9700\u5FEB\u901F\u68C0\u67E5\uFF0C\u53EF\u9009\u62E9\u4EC5\u68C0\u6D4B\u5F53\u524D\u53EF\u89C1 trains\u3002" = "A formal run processes all trains by default; for a quick check, select only the currently visible trains.",
    "\u5FEB\u901F\u9884\u89C8\uFF1A\u4EC5\u68C0\u6D4B\u5F53\u524D\u53EF\u89C1 trains\uFF08\u6B63\u5F0F\u8FD0\u884C\u9ED8\u8BA4\u5168\u90E8 trains\uFF09" = "Quick preview: detect only the currently visible trains (formal runs use all trains by default)",
    "\u53EA\u6709\u660E\u786E\u70B9\u51FB\u6B64\u6309\u94AE\u624D\u4F1A\u5F00\u59CB\uFF1B\u5207\u6362\u9875\u9762\u4E0D\u4F1A\u81EA\u52A8\u8FD0\u884C\u3002" = "Detection starts only when this button is explicitly clicked; changing pages never starts it automatically.",
    "\u5DEE\u5F02 / \u9A8C\u8BC1" = "Differences / validation",
    "\u5BFC\u51FA\u7ED3\u679C" = "Export results",
    "\u8DF3\u8F6C\u5230\u8FD0\u884C\u63A7\u4EF6\uFF0C\u4E0D\u4F1A\u81EA\u52A8\u542F\u52A8" = "Go to the run controls; detection will not start automatically",
    "\u4E3B\u529F\u80FD\u5BFC\u822A" = "Main feature navigation",
    "\u901A\u77E5" = "Notifications",
    "\u67E5\u770B\u672C session \u7684\u754C\u9762\u901A\u77E5" = "View interface notifications from this session",
    "\u754C\u9762\u901A\u77E5" = "Interface notifications",
    "\u4EC5\u4FDD\u7559\u672C session\uFF1B\u4E0D\u5C5E\u4E8E\u79D1\u5B66\u5BA1\u8BA1\u8BB0\u5F55\u3002" = "Kept only for this session; not part of the scientific audit record.",
    "\u6E05\u9664\u5217\u8868" = "Clear list",
    "\u5173\u95ED\u754C\u9762\u901A\u77E5" = "Close interface notifications",
    "\u6298\u53E0/\u5C55\u5F00\u5DE6\u4FA7\u63A7\u5236\u680F" = "Collapse / expand the left control panel",
    "\u6298\u53E0\u63A7\u5236\u680F" = "Collapse controls",
    "\u5C55\u5F00\u63A7\u5236\u680F" = "Expand controls",
    "\u6298\u53E0\u540E\u53F3\u4FA7\u56FE\u4F1A\u81EA\u52A8\u91CD\u7B97\u5BBD\u5EA6" = "Plots resize automatically when the control panel is collapsed",
    "\u672C session \u5C1A\u65E0\u754C\u9762\u901A\u77E5\u3002" = "There are no interface notifications in this session yet.",
    "\u9519\u8BEF\uFF1A" = "Error: ",
    "\u8B66\u544A\uFF1A" = "Warning: ",
    "\u6D88\u606F\uFF1A" = "Message: ",
    "\u9519\u8BEF" = "Error",
    "\u8B66\u544A" = "Warning",
    "\u6D88\u606F" = "Message",
    "\u6570\u636E\u96C6\uFF1A" = "Dataset: ",
    "\u672A\u52A0\u8F7D" = "Not loaded",
    "\u672A\u8FD0\u884C" = "Not run",
    "\u68C0\u6D4B\u5668\u8FD0\u884C\u672A\u5B8C\u6210\uFF1A\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002" = "Detection did not run: upload at least one dataset.",
    "\u672A\u547D\u540D\u6570\u636E\u96C6" = "Unnamed dataset",
    "\u53C2\u6570\u65E0\u6548" = "Invalid parameters",
    "\u5F53\u524D UI \u53C2\u6570\u65E0\u6CD5\u6807\u51C6\u5316\uFF1B\u4E0D\u80FD\u5224\u5B9A\u68C0\u6D4B\u7ED3\u679C\u662F\u5426\u4E3A\u5F53\u524D\u3002" = "The current UI parameters cannot be normalized, so the detector result cannot be verified as current.",
    "\u68C0\u6D4B\u533A\u95F4\uFF08Legacy \u5355\u6807\u7B7E\u89C6\u56FE\uFF09" = "Detected intervals (Legacy single-label view)",
    "\u6B64\u8868\u662F Legacy \u5355\u6807\u7B7E\u533A\u95F4\u6295\u5F71\uFF0C\u4E0D\u662F Phase 2A/2B \u591A\u8F68\u8868\u3002possible_burst \u663E\u793A\u4E3A Review\uFF0C\u4E0D\u5E94\u89C6\u4E3A\u5DF2\u786E\u8BA4 Event\u3002\u70B9\u51FB\u8868\u683C\u884C\u53EF\u8DF3\u8F6C\u5230\u76F8\u5E94 timestamp \u7A97\u53E3\u3002" = "This table is a Legacy single-label interval projection, not a Phase 2A/2B multi-track table. possible_burst is shown as Review and must not be treated as a confirmed Event. Click a row to open its timestamp window.",
    "\u533A\u95F4\u8868\u6765\u6E90" = "Interval-table source",
    "\u81EA\u52A8\u68C0\u6D4B\uFF08AUTO\uFF09" = "Automatic detection (AUTO)",
    "Legacy \u6700\u7EC8\u6807\u7B7E" = "Legacy final labels",
    "Legacy \u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "Legacy final-audit result",
    "\u8868\u683C\u8BE6\u7EC6\u7A0B\u5EA6" = "Table detail level",
    "\u7CBE\u7B80" = "Compact",
    "\u5B8C\u6574\u8BCA\u65AD\u5B57\u6BB5" = "Full diagnostic fields",
    "\u70B9\u51FB\u884C\u540E\u8DF3\u8F6C" = "Jump after clicking a row",
    "\u81EA\u52A8\u9009\u62E9 timestamp \u89C6\u56FE" = "Choose timestamp view automatically",
    "\u6240\u9009\u533A\u95F4\u5DF2\u8FC7\u671F\uFF1B\u8BF7\u5728\u5F53\u524D\u8868\u683C\u4E2D\u91CD\u65B0\u9009\u62E9\u3002" = "The selected interval is stale; select it again in the current table.",
    "\u8BE5 AUTO \u533A\u95F4\u6765\u81EA\u5DF2\u8FC7\u671F\u7684\u6700\u8FD1\u8FD0\u884C\u8303\u56F4\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u540E\u518D\u5B9A\u4F4D\u3002" = "This AUTO interval belongs to a stale recent run scope. Run detection again before opening it.",
    "\u6240\u9009 train \u5DF2\u88AB\u5F53\u524D\u5143\u6570\u636E\u8FC7\u6EE4\u5668\u6392\u9664\uFF1B\u672A\u81EA\u52A8\u6539\u52A8\u8FC7\u6EE4\u6761\u4EF6\u3002" = "The selected train is excluded by the current metadata filter; the filter was not changed automatically.",
    "\u5DF2\u5B9A\u4F4D\u5230\u6240\u9009\u68C0\u6D4B\u533A\u95F4\u7684 timestamp \u7A97\u53E3\u3002" = "Opened the timestamp window for the selected detected interval.",
    "\u5C1A\u672A\u751F\u6210\u6B63\u5F0F\u5BFC\u51FA\u6587\u4EF6\u3002" = "No formal export file has been generated yet.",
    "\u4E0A\u4E00\u6B21\u5BFC\u51FA\u5C5E\u4E8E\u5176\u4ED6\u6570\u636E\u96C6" = "The previous export belongs to another dataset",
    "\u670D\u52A1\u5668\u5DF2\u751F\u6210\u5BFC\u51FA\u6587\u4EF6" = "The server generated the export file",
    "\u5BFC\u51FA\u751F\u6210\u5931\u8D25" = "Export generation failed",
    "\u5BFC\u51FA\u5DF2\u963B\u6B62" = "Export blocked",
    "\u6B63\u5728\u51C6\u5907\u6B63\u5F0F\u5BFC\u51FA" = "Preparing formal export",
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
    "Provider \u590D\u6838\u5DE5\u4F5C\u53F0" = "Provider review workbench",
    "\u4F9B\u5E94\u5668\u8F93\u51FA\u3001\u4EBA\u5DE5\u88C1\u51B3\u4E0E\u53C2\u8003\u771F\u503C\u5206\u5C42\u4FDD\u5B58\u3002" = "Provider output, human adjudication, and reference truth are stored in separate layers.",
    "\u6BCF\u6B21\u53EA\u9009\u62E9\u4E00\u4E2A provider run\uFF1B\u4E0D\u4F1A\u9690\u5F0F\u5408\u5E76 provider\u3001track \u6216 label\u3002" = "Select exactly one provider run at a time; providers, tracks, and labels are never pooled implicitly.",
    "1. \u5BFC\u5165\u4E25\u683C\u4EA7\u54C1" = "1. Import strict products",
    "\u88C1\u51B3\u4EA7\u54C1\uFF08\u53EF\u9009 .rds\uFF09" = "Adjudication product (optional .rds)",
    "\u8BC4\u4EF7\u53C2\u8003\uFF08\u53EF\u9009 .rds\uFF09" = "Evaluation reference (optional .rds)",
    "\u4EC5\u5BFC\u5165\u672C\u8F6F\u4EF6\u751F\u6210\u7684\u53D7\u4FE1 RDS\u3002\u5BFC\u5165\u540E\u4F1A\u7ACB\u5373\u9A8C\u8BC1\u7C7B\u578B\u3001\u8BED\u4E49\u548C SHA-256 \u7236\u5B50\u5173\u7CFB\u3002" = "Import only trusted RDS files produced by this software. Type, semantics, and SHA-256 parentage are validated immediately.",
    "\u5BA1\u67E5\u80CC\u666F" = "Review backdrop",
    "\u88C1\u51B3\u540E\u4EA7\u54C1" = "Adjudicated product",
    "\u79D1\u5B66\u8D23\u4EFB\u4EBA\u4EE3\u53F7" = "Scientific-owner pseudonym",
    "\u9009\u62E9\u7406\u7531" = "Selection rationale",
    "\u751F\u6210\u53EF\u5BA1\u8BA1\u590D\u6838\u89C6\u56FE" = "Build auditable review view",
    "2. \u8FFD\u52A0\u4EBA\u5DE5\u88C1\u51B3" = "2. Append human adjudication",
    "\u88C1\u51B3\u52A8\u4F5C" = "Adjudication action",
    "\u63A5\u53D7\u539F\u8FB9\u754C" = "Accept as is",
    "\u62D2\u7EDD" = "Reject",
    "\u8C03\u6574\u8FB9\u754C" = "Adjust bounds",
    "\u64A4\u9500\u5F53\u524D\u88C1\u51B3" = "Void current decision",
    "\u65B0 start ISI" = "New start ISI",
    "\u65B0 end ISI" = "New end ISI",
    "\u8C03\u6574\u8FB9\u754C\u8981\u6C42\u5F53\u524D\u6570\u636E\u96C6\u4E0E provider snapshot \u7CBE\u786E\u5339\u914D\u3002" = "Boundary adjustment requires the current dataset to match the provider snapshot exactly.",
    "\u590D\u6838\u8005\u4EE3\u53F7" = "Reviewer pseudonym",
    "\u88C1\u51B3\u7406\u7531" = "Adjudication rationale",
    "\u8FFD\u52A0\u88C1\u51B3\uFF08\u4E0D\u6539\u5199 AUTO\uFF09" = "Append adjudication (preserve AUTO)",
    "3. \u660E\u786E\u8BC4\u4EF7\u76EE\u6807" = "3. Declare the scoring estimand",
    "Event IoU \u9608\u503C" = "Event IoU threshold",
    "\u8FD0\u884C\u89C4\u8303\u5316\u8BC4\u4EF7" = "Run normalized scoring",
    "\u4E0B\u8F7D\u4E8B\u52A1\u6027 ZIP" = "Download transactional ZIP",
    "Provider \u76EE\u5F55\uFF08\u4FDD\u7559\u5168\u90E8 run\uFF09" = "Provider catalog (all runs retained)",
    "AUTO \u4E0E\u88C1\u51B3\u540E\u5DEE\u5F02" = "AUTO vs adjudicated differences",
    "\u5F53\u524D\u9009\u4E2D\u533A\u95F4" = "Currently selected intervals",
    "\u5173\u7CFB / Regime" = "Relationships / Regimes",
    "Event / State / Gap \u5173\u7CFB" = "Event / State / Gap relationships",
    "\u63CF\u8FF0\u6027 Regime\uFF08\u975E\u771F\u503C\uFF09" = "Descriptive Regimes (not truth)",
    "\u51B2\u7A81\u5BA1\u8BA1" = "Conflict audit",
    "\u8BC4\u4EF7\u7ED3\u679C" = "Scoring results",
    "\u6309 track \u548C label \u7684\u6307\u6807" = "Metrics by track and label",
    "\u5339\u914D\u660E\u7EC6" = "Match details",
    "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A" = "Manual labels vs detector report",
    "\u79D1\u5B66\u9A8C\u8BC1" = "Scientific validation",
    "\u6279\u5904\u7406 / API" = "Batch / API",
    "\u65B9\u6CD5 / \u5BA1\u8BA1\u8BF4\u660E" = "Methods / audit notes",
    "\u68C0\u6D4B\u5668 / \u53C2\u6570" = "Detector / parameters",
    "\u81EA\u9002\u5E94 train \u8C03\u53C2" = "Adaptive train tuning",
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
    "spike train \u5DE5\u4F5C\u53F0\uFF1A\u5BFC\u5165\u6570\u636E\u3001\u8BBE\u7F6E\u5173\u952E\u53C2\u6570\u3001\u8FD0\u884C\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\uFF0C\u7136\u540E\u590D\u6838\u5DEE\u5F02\u3001\u9A8C\u8BC1\u548C\u5BFC\u51FA\u3002" =
      "Spike-train workbench: import data, set key parameters, run event-grammar detection, then review differences, validate, and export.",
    "\u8109\u51B2\u5E8F\u5217\uFF08spike train\uFF09\u5DE5\u4F5C\u53F0\uFF1A\u5BFC\u5165\u6570\u636E\u3001\u8BBE\u7F6E\u5173\u952E\u53C2\u6570\u3001\u8FD0\u884C\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\uFF0C\u7136\u540E\u590D\u6838\u5DEE\u5F02\u3001\u9A8C\u8BC1\u548C\u5BFC\u51FA\u3002" =
      "Spike-train workbench: import data, set key parameters, run event-grammar detection, then review differences, validate, and export.",
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
    "\u5BF9\u9F50\u6805\u683C\u56FE\uFF1A\u8BF7\u4F7F\u7528\u6846\u9009\u3002burst/tonic \u9700\u8981\u4ECE\u540C\u4E00\u6761 train \u4E2D\u9009\u62E9\u81F3\u5C11 2 \u4E2A spike\uFF1Bpause/others/\u9AD8\u9891\u6A21\u5F0F\u548C NOT-burst \u5F3A\u8D1F\u4F8B\u53EF\u4F7F\u7528\u65F6\u95F4\u8303\u56F4\u9009\u62E9\u3002" =
      "Aligned raster: use box select. burst/tonic require at least 2 spikes from the same train; pause/others/high-frequency and NOT-burst hard negatives can use a time-range selection.",
    "\u5F53\u524D\u6700\u7EC8\u5BA1\u8BA1\u6458\u8981" = "Current final-audit summary",
    "\u5F53\u524D\u5BA1\u8BA1\u4E8B\u4EF6\u8BB0\u5F55" = "Current audit event records",
    "\u5BA1\u8BA1\u5386\u53F2\u8BB0\u5F55" = "Audit history log",
    "\u767E\u5206\u4F4D\u6216\u7EDD\u5BF9\u503C" = "Percentile OR absolute value",
    "\u767E\u5206\u4F4D\u4E0E\u7EDD\u5BF9\u503C" = "Percentile AND absolute value",
    "\u6BCF\u6761 train \u7684 ISI \u9608\u503C\uFF08\u5355\u6761\u8BB0\u5F55\u9608\u503C\uFF09" = "Per-train ISI thresholds (single-record thresholds)",
    "\u5355\u4F4D\u8DDF\u968F\u663E\u793A\u5355\u4F4D\u30020 = \u4E0D\u542F\u7528\u3002\u53EF\u5728\u56FE\u4E2D\u70B9\u51FB\u4E00\u4E2A ISI \u4F5C\u4E3A\u53C2\u8003\uFF0C\u518D\u4E00\u952E\u8BBE\u7F6E burst \u9608\u503C\u7EBF\u3001pause \u9608\u503C\u7EBF\u6216 tonic \u7684\u4E24\u6761\u9608\u503C\u7EBF\u3002\u8F6F\u951A\u70B9\u662F\u63A8\u8350\u6A21\u5F0F\uFF1B\u786C\u9608\u503C\u9700\u663E\u5F0F\u9009\u62E9\u3002" =
      "Units follow the display unit. 0 = disabled. Click an ISI in the plot as a reference, then set the burst threshold line, pause threshold line, or both tonic threshold lines with one click. Soft anchors are recommended; hard thresholds require explicit selection.",
    "\u8109\u51B2\u7AD6\u7EBF\u9AD8\u5EA6\uFF08\u5CF0\u7535\u4F4D\u7EBF\u9AD8\u5EA6\uFF09" = "Spike tick height (peak-potential line height)",
    "\u6240\u6709\u8109\u51B2\u523B\u7EBF\u5747\u7ED8\u5236\u4E3A\u76F8\u540C\u7684\u9ED1\u8272\u5B9E\u7EBF\uFF1B\u6A21\u5F0F/\u6765\u6E90\u4FE1\u606F\u53EA\u901A\u8FC7\u6C34\u5E73\u6761\u5E26\u548C\u53E0\u52A0\u5C42\u663E\u793A\uFF0C\u4E0D\u901A\u8FC7\u8109\u51B2\u7684\u989C\u8272\u6DF1\u6D45\u6216\u7C97\u7EC6\u8868\u793A\u3002" =
      "All spike ticks are drawn as identical solid black lines. Pattern/source information is shown only with horizontal bands and overlays, not by spike color or thickness.",
    "\u5728\u6805\u683C\u56FE\u4E2D\u7ED8\u5236\u81EA\u52A8/\u9690\u5F0F\u201C\u5176\u4ED6\u201D" = "Draw automatic/implicit others in the raster",
    "\u5728\u6805\u683C\u56FE\u4E2D\u7ED8\u5236\u201C\u7591\u4F3C burst\u201D" = "Draw possible burst in the raster",
    "\u540C\u6B65\u6805\u683C\u56FE\u65F6\u95F4\u7A97" = "Synchronize with raster time window",
    "\u4E8B\u4EF6\u5BF9\u9F50\u6805\u683C\u56FE\u3001PSTH\u3001\u7FA4\u4F53\u653E\u7535\u7387\u3001\u795E\u7ECF\u5143\u70ED\u56FE\u4E0E\u8109\u51B2\u8BA1\u6570\u540C\u6B65\u6027" =
      "Event-aligned raster, PSTH, population rate, neuron heatmap, and spike-count synchrony",
    "\u6805\u683C\u56FE\u6700\u5927\u663E\u793A\u8109\u51B2\u6570" = "Maximum raster spikes displayed",
    "\u8FD9\u4E00\u5C42\u4F18\u5148\u56DE\u7B54\u4E8B\u4EF6\u524D\u540E\u653E\u7535\u7387\u3001\u5355\u5143\u54CD\u5E94\u548C\u8109\u51B2\u8BA1\u6570\u540C\u6B65\u6027\uFF1B\u4E0D\u6539\u53D8\u6838\u5FC3 burst/pause/tonic \u68C0\u6D4B\u7ED3\u679C\u3002" =
      "This layer focuses on peri-event firing rates, single-unit responses, and spike-count synchrony; it does not alter the core burst/pause/tonic detection results.",
    "\u539F\u59CB\u8109\u51B2\u8BA1\u6570" = "Raw spike count",
    "\u8BE5\u56FE\u6C47\u603B\u5F53\u524D\u6570\u636E\u96C6\u6240\u6709\u6709\u6548 ISI\uFF0C\u5E2E\u52A9\u7528\u6237\u9009\u62E9\u6570\u636E\u96C6\u5C42\u7EA7\u7684 burst seed ISI \u533A\u95F4\u3002\u6BCF\u6761 train \u7684\u767E\u5206\u4F4D\u4EC5\u4F5C\u4E3A\u8F93\u51FA/\u5BA1\u8BA1\u6307\u6807\uFF0C\u4E0D\u4F5C\u4E3A\u786C\u6027 seed \u95E8\u63A7\u3002" =
      "This plot pools all valid ISIs in the current dataset to help choose a dataset-level burst seed ISI range. Per-train percentiles are output/audit metrics only and are not hard seed gates.",
    "\u81EA\u52A8\u4F18\u5148\u7EA7\uFF1A\u7528\u6237 > \u624B\u52A8\u6807\u8BB0 > \u76F4\u65B9\u56FE > \u9ED8\u8BA4" = "Automatic priority: user > manual labels > histogram > default",
    "\u5F3A\u5236\u4F18\u5148\u4F7F\u7528\u76F4\u65B9\u56FE\u5EFA\u8BAE" = "Force histogram suggestions first",
    "\u76F4\u65B9\u56FE\u81EA\u52A8\u5EFA\u8BAE" = "Automatic histogram suggestions",
    "\u521D\u59CB\u72B6\u6001\u53EA\u663E\u793A\u6570\u636E\u96C6\u81EA\u8EAB ISI \u5206\u5E03\uFF1B\u70B9\u9009\u67D0\u4E2A\u6A21\u5F0F\u540E\u4F1A\u81EA\u52A8\u663E\u793A\u76F4\u65B9\u56FE\u81EA\u52A8\u5EFA\u8BAE\u3002\u5982\u9700\u67E5\u770B\u5B9E\u9645\u4F7F\u7528 / \u7528\u6237\u8F93\u5165 / \u624B\u52A8\u6807\u8BB0\u6765\u6E90\uFF0C\u8BF7\u518D\u52FE\u9009\u5BF9\u5E94\u6765\u6E90\u3002" =
      "Initially, only the dataset's own ISI distribution is shown. Selecting a pattern automatically shows histogram suggestions. Select the corresponding sources to inspect effective, user-input, or manual-label values.",
    "\u5C06\u76F4\u65B9\u56FE\u5EFA\u8BAE\u5199\u5165\u7528\u6237\u9608\u503C" = "Write histogram suggestions to user thresholds",
    "\u6BCF\u4E00\u884C\u663E\u793A\u4E00\u4E2A\u6A21\u5F0F\u53C2\u6570\u7684\u7528\u6237\u8F93\u5165\u3001\u624B\u52A8\u6807\u8BB0\u63A8\u5BFC\u3001\u76F4\u65B9\u56FE\u81EA\u52A8\u5EFA\u8BAE\u3001\u9ED8\u8BA4\u503C\u3001\u5B9E\u9645\u4F7F\u7528\u503C\u548C\u6765\u6E90\u3002" =
      "Each row shows a pattern parameter's user input, manual-label estimate, automatic histogram suggestion, default, effective value, and source.",
    "\u6BCF\u6761 train \u7684 seed-band \u8868\u578B\uFF08\u5355\u6761\u8BB0\u5F55 seed-band \u8868\u578B\uFF09" = "Per-train seed-band phenotype (single-record seed-band phenotype)",
    "\u4E34\u754C\u5019\u9009\u63A2\u7D22\u5668" = "Near-miss explorer",
    "\u8DF3\u8F6C\u5230\u6805\u683C\u56FE\u4E2D\u9009\u4E2D\u9879" = "Jump to the selected item in the raster",
    "\u9884\u89C8\u884C\u662F\u53CD\u4E8B\u5B9E\u7ED3\u679C\uFF1A\u82E5\u6240\u9009\u5019\u9009\u53EA\u9700\u6539\u4E00\u4E2A\u9608\u503C\u4E14\u80FD\u901A\u8FC7\u6700\u7EC8 AUTO \u95E8\u63A7\uFF0C\u201C\u5E94\u7528\u5EFA\u8BAE\u9608\u503C\u201D\u4F1A\u66F4\u65B0\u8BE5\u53C2\u6570\uFF1B\u82E5\u9700\u591A\u4E2A\u9608\u503C\u540C\u65F6\u653E\u5BBD\u6216\u4ECD\u4F1A\u88AB\u6700\u7EC8\u95E8\u63A7\u963B\u65AD\uFF0C\u5219\u4E0D\u6539\u5168\u5C40\u9608\u503C\uFF0C\u800C\u662F\u5C06\u5DF2\u590D\u6838\u7684\u8BE5\u5177\u4F53\u5019\u9009\u5199\u4E3A MANUAL \u6700\u7EC8\u6807\u7B7E\u3002" =
      "Preview rows are counterfactual results. If a selected candidate needs only one threshold change and can pass the final AUTO gate, Apply suggested threshold updates that parameter. If several thresholds must be relaxed or the final gate still blocks it, global thresholds are left unchanged and the reviewed candidate is written as a MANUAL final label.",
    "\u652F\u6301\u65B9\u6CD5\u6805\u683C\u56FE X \u8F74" = "Support-method raster X axis",
    "\u5BF9\u9F50\u65F6\u95F4\uFF0C\u4E0E\u4E3B\u6805\u683C\u56FE\u540C\u5C3A\u5EA6" = "Aligned time, on the same scale as the main raster",
    "\u539F\u59CB\u8109\u51B2\u65F6\u95F4\u6233\uFF08\u79D2\uFF09" = "Original spike timestamp (seconds)",
    "\u540C\u6B65\u4E3B\u6805\u683C\u56FE\u65F6\u95F4\u7A97" = "Synchronize the main raster time window",
    "\u8FD0\u884C Mean-ISI \u548C/\u6216 LogISI \u652F\u6301\u540E\uFF0C\u5019\u9009 burst \u4F1A\u6807\u8BB0\u5728\u76F8\u90BB\u8109\u51B2\u4E4B\u95F4\u7684 ISI \u533A\u95F4\u4E0A\uFF0C\u800C\u4E0D\u662F\u5B64\u7ACB\u8109\u51B2\u523B\u7EBF\u4E0A\u3002Mean-ISI ISI \u6761\u5E26\u7ED8\u5236\u5728\u6BCF\u6761 spike-train \u884C\u7A0D\u4E0A\u65B9\uFF08#8A7FFF\uFF09\uFF1BLogISI / newBD ISI \u6761\u5E26\u7ED8\u5236\u5728\u7A0D\u4E0B\u65B9\uFF08#F58E90\uFF09\u3002\u53EF\u9009\u4E8B\u4EF6\u5305\u7EDC\u4EE5\u534A\u900F\u660E\u65B9\u5F0F\u663E\u793A\u5B8C\u6574\u5019\u9009\u8303\u56F4\u3002" =
      "After running Mean-ISI and/or LogISI support, candidate bursts are marked on ISI intervals between adjacent spikes rather than on isolated spike ticks. Mean-ISI bands are drawn just above each spike-train row (#8A7FFF); LogISI / newBD bands are drawn just below (#F58E90). Optional event envelopes show the full candidate span with transparency.",
    "\u4F7F\u7528\u5F53\u524D\u5B66\u4E60\u5230\u7684\u6BCF\u6761 train \u8303\u56F4" = "Use the current learned per-train ranges",
    "\u5F71\u5B50\u8BC4\u4F30\u65F6\u7981\u7528\u5DF2\u5B66\u4E60\u8303\u56F4" = "Disable learned ranges during shadow evaluation",
    "\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A MANUAL \u6807\u7B7E\u7684\u5F71\u5B50\u68C0\u6D4B\uFF0C\u7136\u540E\u5728\u624B\u52A8\u6807\u8BB0\u5B50\u96C6\u4E0A\u6BD4\u8F83 AUTO \u9884\u6D4B\u4E0E MANUAL \u6807\u7B7E\u3002\u4E0D\u4F1A\u4FEE\u6539\u6570\u636E\u96C6\u3002" =
      "Run a shadow detection without locking MANUAL labels, then compare AUTO predictions with MANUAL labels on the manually labeled subset. This does not modify the dataset.",
    "\u4F7F\u7528\u5F53\u524D\u5DF2\u5B66\u4E60\u8303\u56F4\u63D0\u4F9B\u6821\u51C6/\u62A5\u544A\u89C6\u89D2\uFF1B\u7981\u7528\u5DF2\u5B66\u4E60\u8303\u56F4\u66F4\u63A5\u8FD1\u65E0\u201C\u6BCF\u6761 train \u7684 MANUAL \u5148\u9A8C\u201D\u7684\u89C4\u5219\u68C0\u6D4B\u5668\u76F2\u68C0\u3002" =
      "Using learned ranges provides a calibration/reporting view; disabling them is closer to a blind rule-detector test without per-train MANUAL priors.",
    "\u79D1\u5B66\u9A8C\u8BC1\u4EE5\u624B\u52A8\u6807\u7B7E\u4F5C\u4E3A\u771F\u503C\uFF0C\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A\u624B\u52A8\u533A\u95F4\u7684\u5F71\u5B50\u68C0\u6D4B\uFF0C\u5C06\u5DF2\u6807\u8BB0 train \u5206\u6210\u6821\u51C6/\u9A8C\u8BC1\u96C6\uFF0C\u5E76\u62A5\u544A\u4E8B\u4EF6\u7EA7\u6307\u6807\u3002" =
      "Scientific validation treats manual labels as truth, runs shadow detection without locking manual intervals, splits labeled trains into calibration/validation sets, and reports event-level metrics.",
    "\u8DF3\u8F6C\u5230\u6805\u683C\u56FE\u4E2D\u9009\u4E2D\u5019\u9009" = "Jump to the selected candidate in the raster",
    "\u9009\u4E2D\u4E00\u884C\u540E\u53EF\u76F4\u63A5\u8DF3\u8F6C\u5E76\u5728\u6805\u683C\u56FE\u4E2D\u9AD8\u4EAE\u8BE5\u5019\u9009\u533A\u95F4\u3002" = "Select a row to jump directly to and highlight that candidate interval in the raster.",
    "train \u7EA7\u5206\u5E03\u8868\u578B" = "Train-level distribution phenotype",
    "\u8109\u51B2\u8BA1\u6570 PMF / Fano \u56E0\u5B50" = "Spike-count PMF / Fano factor",
    "\u624B\u52A8\u6821\u51C6\u7684\u9AD8\u9891\u8F6F\u951A\u70B9" = "Manually calibrated high-frequency soft anchors",
    "\u6BCF\u6761 train \u7684 ISI \u767E\u5206\u4F4D\u8868" = "Per-train ISI percentile table",
    "\u5B8C\u6574\u663E\u793A\u8109\u51B2\u6570\u4E0A\u9650" = "Full-render spike limit",
    "\u4EA4\u4E92\u8109\u51B2\u6570\u4E0A\u9650" = "Interactive spike limit",
    "\u60AC\u505C\u63D0\u793A\u4E2D\u7684\u8109\u51B2\u6807\u7B7E\u6765\u6E90" = "Spike label source in hover text",
    "\u8109\u51B2\u8BA1\u6570\u76F8\u5173\u6027" = "Spike-count correlation",
    "\u7FA4\u4F53\u8109\u51B2\u8BA1\u6570 / \u653E\u7535\u7387\u6D41\u5F62\uFF5C\u4E8B\u4EF6\u6807\u7B7E\u4EC5\u4F5C\u4E8B\u540E\u6CE8\u91CA" = "Population spike-count / firing-rate manifold | event labels are post hoc annotations",
    "\u5355\u4FA7\u8FB9\u754C burst \u5BF9\u6BD4\u5EA6 S" = "One-sided boundary burst contrast S",
    "\u5141\u8BB8\u5E72\u51C0\u5355\u4FA7\u8FB9\u754C\u76F4\u63A5\u4F5C\u4E3A\u6807\u51C6 burst" = "Allow a clean one-sided boundary directly as a canonical burst",
    "\u5EFA\u8BAE\u9ED8\u8BA4\u4FDD\u6301 q95 \u8F6F\u60E9\u7F5A\uFF1Aq90 \u662F\u6838\u5FC3\u95E8\u63A7\uFF0Cq95 \u53EA\u964D\u4F4E\u5206\u6570\uFF0C\u907F\u514D\u4E00\u4E2A\u7A0D\u5927\u7684\u5185\u90E8 ISI \u6740\u6389\u7C7B\u4F3C 16-4-3-5-3-2-3-15 \u7684\u7ECF\u5178 burst\u3002" =
      "Keep the q95 soft penalty by default: q90 is the core gate and q95 only lowers the score, preventing one moderately large internal ISI from eliminating a classic burst such as 16-4-3-5-3-2-3-15.",
    "\u5168\u5C40\u9884\u89C8\uFF1A\u9634\u5F71\u533A\u57DF\u8868\u793A\u539F\u59CB\u65F6\u95F4\u6233\u56FE\u7684\u5F53\u524D\u65F6\u95F4\u7A97" =
      "Global overview: the shaded region indicates the current time window in the raw-timestamp plot.",
    "\u624B\u52A8 burst \u4F1A\u5B66\u4E60 burst \u5185 ISI\uFF0C\u4E5F\u4F1A\u5B66\u4E60 burst \u524D/\u540E ISI \u4E0E\u5185\u90E8 q90 \u7684\u6BD4\u503C\u3002" = "Manual bursts learn intra-burst ISIs and the ratio of pre/post-burst ISIs to intra-q90.",
    "\u5C06 Chen \u7B49\u4EBA\u7684\u81EA\u9002\u5E94\u5E73\u5747\u8109\u51B2\u95F4\u9694\u65B9\u6CD5\u4F5C\u4E3A\u652F\u6301\u5C42\u5B9E\u73B0\u3002" = "Implements the adaptive mean inter-spike interval method of Chen et al. as a support layer.",
    "spike train \u4E0A\u7684\u652F\u6301\u65B9\u6CD5 burst ISI \u6761\u5E26" = "Support-method burst ISI bands on spike trains",
    "\u65E7\u7248\u6BCF\u6761 train \u7684 burst-ISI \u8303\u56F4" = "Legacy per-train burst-ISI ranges",
    "\u4ECE MANUAL tonic \u5B66\u4E60 tonic \u8F6F\u951A\u70B9" = "Learn tonic soft anchors from MANUAL tonic labels",
    "\u4ECE MANUAL pause \u5B66\u4E60 pause \u8F6F\u951A\u70B9" = "Learn pause soft anchors from MANUAL pause labels",
    "\u4ECE MANUAL HF \u5B66\u4E60\u9AD8\u9891\u8F6F\u951A\u70B9" = "Learn high-frequency soft anchors from MANUAL HF labels",
    "tonic/pause/HF \u951A\u70B9\u53EA\u63D0\u4F9B\u5C3A\u5EA6\u5B9A\u4F4D\u548C\u6709\u754C\u52A0\u6743\uFF1B\u6700\u7EC8 AUTO \u4ECD\u7531\u5C40\u90E8\u7ED3\u6784\u3001\u8FDE\u7EED\u6027\u548C\u5BF9\u6BD4\u51B3\u5B9A\u3002" = "Tonic/pause/HF anchors only provide scale localization and bounded weighting; final AUTO labels are still determined by local structure, continuity, and contrast.",
    "\u65E7\u7248\uFF1A\u52A0\u5165\u6BCF\u6761 train \u7684 ISI \u767E\u5206\u4F4D\u6761\u4EF6\uFF08\u6838\u5FC3\u68C0\u6D4B\u5FFD\u7565\uFF09" = "Legacy: add per-train ISI percentile conditions (ignored by the core detector)",
    "\u5728\u6805\u683C\u56FE\u4E2D\u6846\u9009\u4E00\u4E2A\u7C07\uFF0C\u5E76\u5728\u5DE6\u4FA7\u9762\u677F\u70B9\u51FB\u201C\u8BBE\u7F6E\u6240\u9009\u7C07 A/B\u201D\u3002\u8868\u683C\u4F1A\u62A5\u544A\u8109\u51B2\u6570\u3001\u6301\u7EED\u65F6\u95F4\u3001\u8FB9\u7F18\u5BF9\u6BD4\u3001\u53D8\u5F02\u6027\u548C\u5F53\u524D\u6807\u7B7E\u7684\u5BA2\u89C2\u5DEE\u5F02\u3002" = "Box-select a cluster in the raster, then click Set selected cluster A/B in the left panel. The table reports objective differences in spike count, duration, edge contrast, variability, and current label.",
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
    "\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A manual \u6807\u7B7E\u7684 shadow \u68C0\u6D4B\uFF0C\u7136\u540E\u5728\u624B\u52A8\u6807\u8BB0\u5B50\u96C6\u4E0A\u6BD4\u8F83 AUTO \u9884\u6D4B\u4E0E manual \u6807\u7B7E\u3002\u4E0D\u4F1A\u4FEE\u6539\u6570\u636E\u96C6\u3002" =
      "Run a shadow detection without locking manual labels, then compare AUTO predictions with manual labels on the manually annotated subset. This does not modify the dataset.",
    "\u4E25\u683C\u6A21\u5F0F\u5C06 possible_burst \u4F5C\u4E3A\u72EC\u7ACB\u590D\u6838\u7C7B\u522B\u3002\u5019\u9009\u5BB6\u65CF\u6A21\u5F0F\u4EC5\u5408\u5E76 burst + possible_burst \u4EE5\u8BC4\u4F30\u5019\u9009\u751F\u6210\u7075\u654F\u5EA6\u3002" =
      "Strict mode reports possible_burst as a separate Review category. Candidate-family mode combines only burst + possible_burst to evaluate candidate-generation sensitivity.",
    "\u4F7F\u7528\u5F53\u524D\u5DF2\u5B66\u4E60\u8303\u56F4\u63D0\u4F9B\u6821\u51C6/\u62A5\u544A\u89C6\u89D2\uFF1B\u7981\u7528\u5DF2\u5B66\u4E60\u8303\u56F4\u66F4\u63A5\u8FD1\u65E0 train-specific manual \u5148\u9A8C\u7684\u89C4\u5219\u68C0\u6D4B\u5668\u76F2\u68C0\u3002" =
      "Use the currently learned ranges for a calibration/reporting view. Disable learned ranges for a rule-based detector evaluation without train-specific manual priors.",
    "\u79D1\u5B66\u9A8C\u8BC1\u4EE5\u624B\u52A8\u6807\u7B7E\u4F5C\u4E3A\u771F\u503C\uFF0C\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A\u624B\u52A8\u533A\u95F4\u7684 shadow \u68C0\u6D4B\uFF0C\u5C06\u5DF2\u6807\u8BB0 train \u5206\u6210\u6821\u51C6/\u9A8C\u8BC1\u96C6\uFF0C\u5E76\u62A5\u544A\u4E8B\u4EF6\u7EA7\u6307\u6807\u3002" =
      "Scientific validation treats manual labels as truth, runs shadow detection without locking manual intervals, splits annotated trains into calibration and validation sets, and reports event-level metrics.",
    "\u4E25\u683C\u6A21\u5F0F\u5C06 possible_burst \u4F5C\u4E3A\u590D\u6838\u7C7B\u3002\u5019\u9009\u5BB6\u65CF\u6A21\u5F0F\u5408\u5E76 burst + long_burst + possible_burst\uFF0C\u7528\u4E8E\u8BC4\u4F30\u5019\u9009\u53EC\u56DE\uFF0C\u800C\u975E\u9AD8\u7F6E\u4FE1 burst \u51C6\u786E\u7387\u3002" =
      "Strict mode reports possible_burst as a Review category. Candidate-family mode combines burst + long_burst + possible_burst to evaluate candidate recall, not high-confidence burst precision.",
    "\u5C1A\u65E0\u68C0\u6D4B\u5668\u91CD\u8DD1\u6458\u8981\u3002" = "No detector rerun summary yet.",
    "\u5C1A\u65E0\u9608\u503C\u5E94\u7528/\u91CD\u8DD1\u6458\u8981\u3002" = "No threshold-application or rerun summary yet.",
    "\u5C1A\u672A\u8FD0\u884C\u6279\u5904\u7406\u3002" = "Batch processing has not been run yet.",
    "\u5C1A\u672A\u8FD0\u884C\u5C40\u90E8\u5DEE\u5F02\u91CD\u8DD1\u9884\u89C8\u3002" = "No local-difference rerun preview has been run yet.",
    "\u5C1A\u672A\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u3002" = "No event-level parameter-sensitivity scan has been run yet.",
    "\u5C1A\u672A\u9884\u89C8 possible_burst \u6279\u91CF\u5347\u7EA7\u3002" = "No possible_burst bulk-promotion preview has been run yet.",
    "\u5C1A\u672A\u751F\u6210\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C\u3002\u82E5\u672A\u751F\u6210\uFF0C\u4E0B\u6E38 audit_final \u4F1A\u56DE\u9000\u5230\u5F53\u524D final \u6807\u7B7E\u3002" =
      "No final audit result has been generated. Until one is generated, downstream audit_final views fall back to the current final labels.",
    "\u8BF7\u5148\u52A0\u8F7D\u6570\u636E\u96C6\u3002" = "Please load a dataset first.",
    "\u6CA1\u6709\u9501\u5B9A\u7684\u53C2\u8003 ISI\u3002" = "No reference ISI is locked.",
    "\u68C0\u6D4B\u72B6\u6001" = "Detection status",
    "MANUAL \u5BF9\u7167\u62A5\u544A\u72B6\u6001" = "MANUAL comparison report status",
    "\u79D1\u5B66\u9A8C\u8BC1\u62A5\u544A\u72B6\u6001" = "Scientific validation report status",
    "\u672A\u52A0\u8F7D\u6570\u636E" = "No data loaded",
    "\u5BFC\u5165\u6570\u636E\u540E\u5C06\u5728\u8FD9\u91CC\u663E\u793A\u68C0\u6D4B\u72B6\u6001\u3002" = "Detection status will appear here after data are imported.",
    "\u5C1A\u672A\u8FD0\u884C" = "Not run yet",
    "\u5F53\u524D\u6570\u636E\u96C6\u8FD8\u6CA1\u6709\u68C0\u6D4B\u5668\u8FD0\u884C\u7ED3\u679C\u3002" = "The current dataset has no detector run result yet.",
    "\u6765\u81EA\u5176\u4ED6\u6570\u636E\u96C6" = "From another dataset",
    "\u8BE5\u7ED3\u679C\u4E0D\u5C5E\u4E8E\u5F53\u524D\u6570\u636E\u96C6\uFF0C\u4E0D\u80FD\u4F5C\u4E3A\u5F53\u524D\u7ED3\u679C\u4F7F\u7528\u3002" = "This result does not belong to the current dataset and cannot be used as its current result.",
    "\u6570\u636E\u5DF2\u6539\u53D8" = "Data changed",
    "\u8F93\u5165\u6570\u636E\u5728\u68C0\u6D4B\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u3002" = "Input data changed after detection; rerun detection.",
    "\u5F53\u524D\u53C2\u6570\u4E5F\u5DF2\u6539\u53D8\u3002" = "The current parameters also changed.",
    "\u53C2\u6570\u5DF2\u6539\u53D8" = "Parameters changed",
    "\u5F53\u524D\u53C2\u6570\u4E0E\u8BE5\u6B21\u8FD0\u884C\u7684\u53C2\u6570\u5FEB\u7167\u4E0D\u540C\uFF1B\u73B0\u6709\u7ED3\u679C\u4FDD\u7559\uFF0C\u4F46\u9700\u8981\u91CD\u65B0\u8FD0\u884C\u3002" = "Current parameters differ from this run's parameter snapshot. Existing results are retained, but a rerun is required.",
    "Train \u7EA7\u68C0\u6D4B\u8BBE\u7F6E\u5DF2\u6539\u53D8" = "Train-level detection settings changed",
    "Train-specific \u9608\u503C\u6216\u8303\u56F4\u5728\u8FD0\u884C\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u3002" = "Train-specific thresholds or ranges changed after the run; rerun detection.",
    "AUTO \u7ED3\u679C\u5DF2\u624B\u52A8\u6539\u52A8" = "AUTO results were manually changed",
    "AUTO \u6807\u7B7E\u6216\u5206\u6570\u5728\u68C0\u6D4B\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u4E8B\u4EF6\u8868\u548C\u8BCA\u65AD\u53EF\u80FD\u5DF2\u4E0D\u540C\u6B65\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u3002" = "AUTO labels or scores changed after detection, so the event table and diagnostics may be out of sync. Rerun detection.",
    "MANUAL \u8BC1\u636E\u5DF2\u6539\u53D8" = "MANUAL evidence changed",
    "MANUAL/NOT-burst \u6807\u7B7E\u5728\u68C0\u6D4B\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u5F53\u524D AUTO \u4ECD\u4FDD\u7559\uFF0C\u4F46 manual-aware \u8FD0\u884C\u4E0E\u4E0B\u6E38\u5BA1\u8BA1\u9700\u8981\u91CD\u65B0\u540C\u6B65\u3002" = "MANUAL/NOT-burst labels changed after detection. Current AUTO results are retained, but manual-aware runs and downstream audits must be resynchronized.",
    "\u65E0\u6CD5\u9A8C\u8BC1\u7ED3\u679C\u8EAB\u4EFD" = "Cannot verify result identity",
    "\u7ED3\u679C\u7F3A\u5C11\u5B8C\u6574\u7684\u6570\u636E\u3001\u53C2\u6570\u6216 train \u8303\u56F4\u5FEB\u7167\uFF0C\u4E0D\u80FD\u786E\u8BA4\u5B83\u662F\u5426\u4ECD\u4E3A\u5F53\u524D\u7ED3\u679C\u3002" = "The result lacks a complete data, parameter, or train-scope snapshot, so its current identity cannot be confirmed.",
    "\u90E8\u5206 train \u7ED3\u679C\u4E3A\u5F53\u524D" = "Results are current for some trains",
    "\u7ED3\u679C\u4E3A\u5F53\u524D" = "Results are current",
    "\u5C1A\u672A\u9A8C\u8BC1" = "Not validated yet",
    "\u5F53\u524D\u6570\u636E\u96C6\u8FD8\u6CA1\u6709\u9A8C\u8BC1\u7ED3\u679C\u3002" = "The current dataset has no validation result yet.",
    "\u9A8C\u8BC1\u6765\u81EA\u5176\u4ED6\u6570\u636E\u96C6" = "Validation is from another dataset",
    "\u8BE5\u9A8C\u8BC1\u62A5\u544A\u4E0D\u5C5E\u4E8E\u5F53\u524D\u6570\u636E\u96C6\u3002" = "This validation report does not belong to the current dataset.",
    "\u9A8C\u8BC1\u6240\u7528\u6570\u636E\u5DF2\u6539\u53D8" = "Validation data changed",
    "\u5F53\u524D spike \u6570\u636E\u4E0E\u9A8C\u8BC1\u65F6\u7684\u6570\u636E\u5FEB\u7167\u4E0D\u540C\uFF1B\u8BE5\u62A5\u544A\u5DF2\u8FC7\u671F\u3002" = "Current spike data differ from the validation snapshot; this report is stale.",
    "\u68C0\u6D4B\u8FD0\u884C\u5DF2\u6539\u53D8" = "Detection run changed",
    "\u9A8C\u8BC1\u62A5\u544A\u7ED1\u5B9A\u7684\u68C0\u6D4B\u8FD0\u884C\u3001\u53C2\u6570\u6216 Train \u7EA7\u8BBE\u7F6E\u5DF2\u4E0D\u518D\u662F\u5F53\u524D\u7248\u672C\u3002" = "The detector run, parameters, or Train-level settings bound to this validation report are no longer current.",
    "\u9A8C\u8BC1\u8BBE\u7F6E\u5DF2\u6539\u53D8" = "Validation settings changed",
    "\u72EC\u7ACB\u9A8C\u8BC1\u6240\u7528\u7684\u53C2\u6570\u6216 Train \u7EA7\u68C0\u6D4B\u8BBE\u7F6E\u5DF2\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002" = "Parameters or Train-level detection settings used by the independent validation changed; validate again.",
    "\u4EBA\u5DE5\u771F\u503C\u5DF2\u6539\u53D8" = "Manual truth changed",
    "MANUAL/NOT-burst \u771F\u503C\u5728\u9A8C\u8BC1\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002" = "MANUAL/NOT-burst truth changed after validation; validate again.",
    "Review \u771F\u503C\u5DF2\u6539\u53D8" = "Review truth changed",
    "Review \u786E\u8BA4\u6216\u64A4\u9500\u72B6\u6001\u5728\u9A8C\u8BC1\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002" = "Review confirmation or revocation state changed after validation; validate again.",
    "\u9A8C\u8BC1\u771F\u503C\u5DF2\u6539\u53D8" = "Validation truth changed",
    "\u5916\u90E8\u53C2\u8003\u771F\u503C\u4E0E\u9A8C\u8BC1\u65F6\u7684\u771F\u503C\u5FEB\u7167\u4E0D\u540C\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002" = "External reference truth differs from the validation snapshot; validate again.",
    "\u65E0\u6CD5\u9A8C\u8BC1\u62A5\u544A\u8EAB\u4EFD" = "Cannot verify report identity",
    "\u62A5\u544A\u7F3A\u5C11\u5B8C\u6574\u7684\u6570\u636E\u3001\u8FD0\u884C\u3001\u771F\u503C\u6216 train \u8303\u56F4\u5FEB\u7167\u3002" = "The report lacks a complete data, run, truth, or train-scope snapshot.",
    "\u90E8\u5206 train \u9A8C\u8BC1\u4E3A\u5F53\u524D" = "Validation is current for some trains",
    "\u9A8C\u8BC1\u4E3A\u5F53\u524D" = "Validation is current",
    "\u672A\u5206\u6790" = "Not analyzed",
    "\u5DF2\u5206\u6790" = "Analyzed",
    "\u7ED3\u679C\u8FC7\u671F" = "Results stale",
    "\u72B6\u6001\u672A\u77E5" = "Status unknown",
    "\u786E\u8BA4\u6267\u884C" = "Confirm action",
    "\u53D6\u6D88" = "Cancel",
    "\u64A4\u9500\u4E0A\u4E00\u6B21\u53D8\u66F4" = "Undo last change",
    "\u672C session \u6700\u591A\u4FDD\u7559\u6700\u8FD1 5 \u6B21\u53EF\u64A4\u9500\u53D8\u66F4\uFF0C\u5E76\u53D7 64 MB \u538B\u7F29\u5386\u53F2\u4E0A\u9650\u7EA6\u675F\uFF1B\u5927\u6570\u636E\u96C6\u4E0D\u4F1A\u65E0\u754C\u589E\u957F\u5185\u5B58\u3002" = "This session retains at most the 5 most recent undoable changes and caps compressed history at 64 MB, preventing unbounded memory growth for large datasets.",
    "\u6CA1\u6709\u53EF\u64A4\u9500\u7684\u4E0A\u4E00\u6B21\u53D8\u66F4\u3002" = "There is no previous change to undo.",
    "Phase 2B \u5DF2\u6FC0\u6D3B" = "Phase 2B is active",
    "Legacy possible\u2192real \u5347\u7EA7\u5DF2\u7981\u7528\u3002\u5019\u9009\u7EA7 Review\u2192Event Confirm/Revoke \u5F53\u524D\u4EC5\u901A\u8FC7 API \u63D0\u4F9B\uFF0C\u672C\u9636\u6BB5 UI \u4E0D\u6267\u884C\u8BE5\u8F6C\u6362\u3002" = "Legacy possible\u2192real promotion is disabled. Candidate-level Review\u2192Event Confirm/Revoke is currently API-only; this UI phase does not execute that transition.",
    "Legacy \u6279\u91CF\u5347\u7EA7\u5DF2\u7981\u7528" = "Legacy bulk promotion is disabled",
    "\u5F53\u524D\u6570\u636E\u96C6\u5DF2\u5305\u542B Phase 2B Review \u72B6\u6001\u3002Legacy \u8DEF\u5F84\u4E0D\u5F97\u4FEE\u6539\u8BE5\u5BA1\u5B9A\u94FE\uFF0C\u4E5F\u4E0D\u5F97\u4F5C\u4E3A\u66FF\u4EE3\u64CD\u4F5C\u3002" = "The current dataset contains Phase 2B Review state. The Legacy path must not modify this adjudication chain or be used as a substitute workflow.",
    "\u8BE5\u5DE5\u5177\u53EA\u4FEE\u6539 Legacy MANUAL \u5355\u6807\u7B7E\u5C42\uFF0C\u4E0D\u4F1A\u521B\u5EFA Phase 2B Review\u2192Event \u8F6C\u6362\u3002\u6807\u51C6\u754C\u9762\u4E0D\u5141\u8BB8\u8986\u76D6\u5DF2\u6709 MANUAL / NOT-burst \u8BC1\u636E\u3002" = "This tool modifies only the Legacy MANUAL single-label layer; it does not create a Phase 2B Review\u2192Event transition. The standard interface does not allow existing MANUAL / NOT-burst evidence to be overwritten.",
    "Phase 2B Review \u72B6\u6001\u5DF2\u6FC0\u6D3B\uFF0CLegacy possible\u2192real \u5347\u7EA7\u4E0D\u53EF\u7528\u3002" = "Phase 2B Review state is active; Legacy possible\u2192real promotion is unavailable.",
    "Phase 2B Review \u72B6\u6001\u5DF2\u6FC0\u6D3B\uFF0CLegacy \u6279\u91CF\u5347\u7EA7\u5DF2\u7981\u7528\u3002" = "Phase 2B Review state is active; Legacy bulk promotion is disabled.",
    "Phase 2B Review \u72B6\u6001\u5DF2\u6FC0\u6D3B\uFF0CLegacy \u6279\u91CF\u5347\u7EA7\u4E0D\u53EF\u7528\u3002" = "Phase 2B Review state is active; Legacy bulk promotion is unavailable.",
    "Legacy \u5347\u7EA7\u9884\u89C8\u7F3A\u5931\u6216\u5DF2\u8FC7\u671F\u3002\u8BF7\u5728\u5F53\u524D\u6570\u636E/\u6807\u7B7E\u72B6\u6001\u4E0B\u91CD\u65B0\u9884\u89C8\u3002" = "The Legacy promotion preview is missing or stale. Preview again using the current data and label state.",
    "Legacy \u5347\u7EA7\u9884\u89C8\u5DF2\u8FC7\u671F\uFF1B\u8BF7\u91CD\u65B0\u9884\u89C8\u3002" = "The Legacy promotion preview is stale; preview again.",
    "\u5F53\u524D Legacy \u9884\u89C8\u4E2D\u6CA1\u6709\u53EF\u5347\u7EA7\u7684 possible_burst\u3002" = "The current Legacy preview has no eligible possible_burst candidates.",
    "\u8FD9\u4F1A\u91CD\u5EFA Legacy \u5355\u6807\u7B7E audit_final \u5C42\u3002\u5B83\u4E0D\u662F Phase 2B \u5019\u9009\u7EA7\u5BA1\u5B9A\uFF0C\u4E0D\u4F1A\u521B\u5EFA Review\u2192Event transition\u3002" = "This rebuilds the Legacy single-label audit_final layer. It is not Phase 2B candidate-level adjudication and does not create a Review\u2192Event transition.",
    "\u786E\u8BA4 Legacy possible\u2192real \u5347\u7EA7" = "Confirm Legacy possible\u2192real promotion",
    "\u786E\u8BA4\u6267\u884C Legacy \u6279\u91CF\u5347\u7EA7" = "Confirm Legacy bulk promotion",
    "\u786E\u8BA4\u6E05\u9664 Legacy \u5BA1\u8BA1\u5C42" = "Confirm clearing the Legacy audit layer",
    "\u5C06\u6E05\u9664\u6240\u9009 trains \u7684 pattern_audit_final \u6D3E\u751F\u5C42\uFF0C\u4E0B\u6E38 audit_final \u663E\u793A\u5C06\u56DE\u9000\u5230 final\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002" = "This clears the pattern_audit_final derived layer for the selected trains. Downstream audit_final views will fall back to final. This can be undone in the current session.",
    "\u786E\u8BA4\u6E05\u9664\u6240\u9009 MANUAL \u6807\u7B7E" = "Confirm clearing selected MANUAL labels",
    "\u5C06\u6E05\u9664\u5F53\u524D Raster \u9009\u533A\u4E2D\u5339\u914D\u7684 MANUAL/NOT-burst \u8BC1\u636E\uFF0C\u5E76\u4F7F\u4F9D\u8D56\u8FD9\u4E9B\u771F\u503C\u7684\u9A8C\u8BC1\u7ED3\u679C\u8FC7\u671F\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002" = "This clears matching MANUAL/NOT-burst evidence in the current Raster selection and makes validation results that depend on this truth stale. This can be undone in the current session.",
    "\u786E\u8BA4\u6E05\u9664\u6240\u9009 AUTO \u6807\u7B7E" = "Confirm clearing selected AUTO labels",
    "\u5C06\u6E05\u9664\u5F53\u524D Raster \u9009\u533A\u4E2D\u5339\u914D\u7684 AUTO \u6807\u7B7E\u3002\u8FD0\u884C\u6458\u8981\u4E0E\u4E8B\u4EF6\u8868\u5C06\u88AB\u6807\u8BB0\u4E3A\u8FC7\u671F\uFF1B\u672C session \u4E2D\u53EF\u64A4\u9500\u3002" = "This clears matching AUTO labels in the current Raster selection. The run summary and event table will be marked stale. This can be undone in the current session.",
    "\u786E\u8BA4\u6E05\u9664\u5168\u90E8 MANUAL \u6807\u7B7E" = "Confirm clearing all MANUAL labels",
    "\u786E\u8BA4\u6E05\u9664\u5168\u90E8 AUTO \u6807\u7B7E" = "Confirm clearing all AUTO labels",
    "\u786E\u8BA4\u6E05\u7A7A\u5185\u5B58" = "Confirm clearing memory",
    # `\u6E05\u7A7A` is a server-validated confirmation token, not a label.
    # Keep it verbatim in English so the translated prompt cannot instruct a
    # user to enter a token that the server will reject.
    "\u8BF7\u8F93\u5165\u201C\u6E05\u7A7A\u201D\u540E\u518D\u786E\u8BA4\u3002" = "Type \"\u6E05\u7A7A\" before confirming.",
    "\u786E\u8BA4\u79FB\u9664\u6570\u636E\u96C6" = "Confirm removing the dataset",
    "\u786E\u8BA4\u5408\u5E76\u91CD\u590D timestamp" = "Confirm merging duplicate timestamps",
    "\u5C06\u5220\u9664\u5F53\u524D\u6570\u636E\u96C6\u5185\u5B8C\u5168\u91CD\u590D\u7684 spike timestamp\uFF0C\u5E76\u4F7F AUTO \u4E0E\u68C0\u6D4B\u7ED3\u679C\u5931\u6548\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002" = "This deletes exact duplicate spike timestamps in the current dataset and invalidates AUTO and detection results. This can be undone in the current session.",
    "\u786E\u8BA4\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u7684\u91CD\u590D timestamp" = "Confirm merging duplicate timestamps in all datasets",
    # Single-character labels are safe only as full-node matches. They must
    # never participate in arbitrary substring replacement.
    "\u7C07" = "Cluster",
    "\u8868" = "Table",
    "\u56FE" = "Plot",
    "\u5217" = "Columns",
    "\u884C" = "Rows",
    "\u6570" = "Count",
    "\u503C" = "Value",
    "Round-trip" = "Round-trip",
    "\u5F80\u8FD4\u4E00\u81F4\u6027" = "Round-trip",
    "\u9AD8\u7EA7 / \u4E13\u5BB6\uFF1Aburst \u9644\u5C5E\u6709\u610F\u4E49\u7ED3\u6784" = "Advanced / Expert: burst-related interesting structure",
    "\u9AD8\u7EA7 / \u4E13\u5BB6\uFF1A\u6A21\u5F0F\u9009\u62E9\u3001\u81EA\u9002\u5E94\u4F30\u8BA1\u4E0E\u65E7\u7248\u6821\u51C6" = "Advanced / Expert: pattern selection, adaptive estimation, and legacy calibration",
    "\u5206\u7BB1\u5BBD\u5EA6\uFF08\u5F53\u524D\u663E\u793A\u5355\u4F4D\uFF09" = "Bin width (current display unit)",
    "\u8BE5\u5C42\u4E0D\u6539\u5199 AUTO \u6216 MANUAL\uFF1B\u5B83\u5728\u68C0\u6D4B\u540E\u751F\u6210 pattern_audit_final\u3002\u70B9\u51FB\u201C\u5C06 possible \u5347\u7EA7\u4E3A real\u201D\u540E\uFF0Cpossible_* \u4F1A\u5728\u6700\u7EC8\u5BA1\u8BA1\u5C42\u53D8\u6210\u5BF9\u5E94\u771F\u5B9E\u6A21\u5F0F\uFF0C\u4E0B\u6E38\u6D41\u5F62/\u72B6\u6001\u8F68\u8FF9\u9ED8\u8BA4\u4F7F\u7528\u8FD9\u4E00\u5C42\u3002" =
      "This layer does not overwrite AUTO or MANUAL; it creates pattern_audit_final after detection. After possible is promoted to real, possible_* becomes the corresponding real pattern in the final-audit layer, which is used by downstream manifold and state-trajectory views by default.",
    "\u65E7\u7248\u6BCF\u6761 train \u7684 burst-ISI \u8303\u56F4\u3002\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\u4F7F\u7528\u6570\u636E\u96C6/\u624B\u52A8 ISI \u79CD\u5B50\u533A\u95F4\uFF1B\u8FD9\u4E9B\u63A7\u4EF6\u4EC5\u5728\u663E\u5F0F\u542F\u7528\u65E7\u7248/\u5907\u7528\u6821\u51C6\u65F6\u4F7F\u7528\u3002" =
      "Legacy per-train burst-ISI ranges. Event-grammar detection uses the dataset/manual ISI seed band; these controls are used only when legacy or fallback calibration is explicitly enabled.",
    "\u6838\u5FC3\u68C0\u6D4B\u5668\u53EA\u8BFB\u53D6\u89E3\u6790\u540E\u7684\u751F\u6548\u9608\u503C\u3002\u6765\u6E90\u4F18\u5148\u7EA7\u4E3A\uFF1A\u7528\u6237\u8F93\u5165 > \u624B\u52A8\u6807\u8BB0\u7ED3\u6784\u5B66\u4E60 > \u76F4\u65B9\u56FE\u5EFA\u8BAE > \u9ED8\u8BA4\u3002" =
      "The core detector reads only resolved effective thresholds. Source priority is user input > structure learned from manual labels > histogram suggestion > default.",
    "\u6838\u5FC3\u68C0\u6D4B\u4F7F\u7528\u6570\u636E\u96C6/\u624B\u52A8 ISI \u533A\u95F4\u3002\u672C\u9762\u677F\u53EA\u7528\u4E8E\u4FDD\u5B58\u6216\u6E05\u9664\u65E7\u7248\u6BCF\u6761 train \u7684\u8303\u56F4\uFF0C\u4F5C\u4E3A\u56DE\u9000/\u8BCA\u65AD\u7528\u9014\u3002" =
      "Core detection uses dataset/manual ISI bands. This panel only saves or clears legacy per-train ranges for fallback and diagnostic use.",
    "\u8FD9\u91CC\u4EC5\u5217\u51FA\u76F8\u5BF9\u9ED8\u8BA4\u503C\u5DF2\u6539\u53D8\u7684\u53C2\u6570\uFF0C\u5E76\u6807\u6CE8\u57FA\u7840 / \u9AD8\u7EA7 / \u4E13\u5BB6\u5C42\u7EA7\u548C\u4E3B\u8981\u68C0\u6D4B\u5F71\u54CD\u3002" =
      "This lists only parameters that differ from their defaults, with Basic / Advanced / Expert level and primary detection impact.",
    "\u6A21\u5757\u5316\u53C2\u8003\uFF1A\u65B9\u6CD5\u62A5\u544A\u53EF\u4F7F\u7528\u8BE5\u8868\u3002\u5B83\u5217\u51FA\u5168\u90E8\u53C2\u6570\uFF0C\u5E76\u9AD8\u4EAE\u4E0E\u9ED8\u8BA4\u503C\u6216\u6240\u9009\u9884\u8BBE\u4E0D\u540C\u7684\u503C\u3002" =
      "Modular reference: use this table in methods reporting. It lists all parameters and highlights values that differ from defaults or the selected preset.",
    "\u5206\u5E03\u8BC1\u636E\u4EC5\u4F5C\u4E3A\u5BA1\u8BA1/\u652F\u6301\u5C42\uFF1A\u5B83\u91CF\u5316\u5C40\u90E8 ISI CDF/tail\u3001logISI KS/W1\u3001CV2/LV/LvR \u7B49\u8BC1\u636E\uFF0C\u4E0D\u76F4\u63A5\u6539\u5199\u68C0\u6D4B\u6807\u7B7E\u6216\u8FB9\u754C\u3002" =
      "The distribution evidence is an audit/support layer only. It quantifies local ISI CDF/tail, logISI KS/W1, CV2/LV/LvR, and related evidence without directly changing detection labels or boundaries.",
    "Train \u7EA7\u5206\u5E03\u8868\u578B" = "Train-level distribution phenotype",
    "Spike-count PMF / Fano \u56E0\u5B50" = "Spike-count PMF / Fano factor",
    "\u6A21\u5757\u5316\u8FC1\u79FB\u8DEF\u7EBF\u56FE" = "Modular migration roadmap",
    "\u79CD\u5B50 / \u6865\u63A5\u8BCA\u65AD" = "Seed / Bridge diagnostics",
    "\u57FA\u7840\u53C2\u6570\u4F18\u5148\uFF0C\u4E13\u5BB6\u9879\u6298\u53E0" = "Basic parameters first; expert options are collapsed",
    "\u5DEE\u5F02\u9884\u89C8\u3001IoU\u3001\u654F\u611F\u6027" = "Delta preview, IoU, and sensitivity",
    "\u7ED8\u5236\u53C2\u6570\u5DEE\u5F02\u8BD5\u8FD0\u884C\u4E8B\u4EF6\u53E0\u52A0\u5C42" = "show parameter-delta dry-run event overlay",
    "\u7ED8\u5236 burst \u9644\u5C5E\u6709\u610F\u4E49\u7ED3\u6784\u53E0\u52A0\u5C42" = "show burst-related interesting-structure overlay",
    "\u60AC\u505C\u63D0\u793A\u4E2D\u663E\u793A\u6269\u5C55 ISI \u533A\u95F4\u6307\u6807" = "show extended ISI interval metrics in hover",
    "\u9644\u5C5E\u8109\u51B2\u5305\u6700\u5C0F ISI\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "related packet minimum ISI (current unit)",
    "\u9644\u5C5E\u8109\u51B2\u5305\u6700\u5927 ISI\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "related packet maximum ISI (current unit)",
    "\u9644\u5C5E\u8109\u51B2\u5305\u6700\u5C0F ISI \u6570" = "related packet minimum ISI count",
    "\u9644\u5C5E\u8109\u51B2\u5305\u6700\u5927 ISI \u6570" = "related packet maximum ISI count",
    "\u65E7\u7248\uFF1A\u5728\u5907\u7528\u68C0\u6D4B\u5668\u4E2D\u4F7F\u7528\u5DF2\u4FDD\u5B58\u7684\u6BCF\u6761 train burst-ISI \u8303\u56F4" = "Legacy: use saved per-train burst-ISI ranges in the fallback detector",
    "\u663E\u793A\u6BCF\u6761 train \u7684\u9608\u503C\u7EBF" = "show per-train threshold lines",
    "\u6E05\u9664\u6240\u6709\u6BCF\u6761 train \u7684\u9608\u503C\uFF08\u5355\u6761\u8BB0\u5F55\u9608\u503C\uFF09" = "clear all per-train thresholds (single-record thresholds)",
    "\u6BCF\u6761 train \u4ECE\u9996\u4E2A spike \u5BF9\u9F50\uFF08\u4F2A\u7FA4\u4F53\uFF09" = "align each train to its first spike (pseudo-population)",
    "\u7528\u4E8E sliceTCA \u7684\u8BD5\u6B21 / \u8FD0\u52A8\u4E8B\u4EF6 CSV" = "Trial / movement-event CSV for sliceTCA",
    "\u8BD5\u6B21-\u65F6\u95F4\u5750\u6807 / \u4E8B\u4EF6\u6807\u7B7E" = "Trial-time coordinates / event labels",
    "\u5B8F\u5E73\u5747 F1" = "macro F1",
    "\u5B8F\u5E73\u5747\u7CBE\u786E\u7387" = "macro precision",
    "\u5B8F\u5E73\u5747\u53EC\u56DE\u7387" = "macro recall",
    "\u8FD9\u662F burst \u7684\u9644\u5C5E motif\uFF0C\u4E0D\u6539\u5199\u4E3B AUTO \u6807\u7B7E\u3002\u9644\u5C5E\u8109\u51B2\u5305\u5FC5\u987B\u76F4\u63A5\u7D27\u8D34\u5DF2\u786E\u8BA4\u7684 burst/long_burst\uFF1B\u9ED8\u8BA4\u4E0D\u5141\u8BB8\u4E2D\u95F4\u6709\u7A7A\u767D ISI\u3002" =
      "This is a burst-related motif and does not overwrite the main AUTO label. The related packet must directly adjoin a confirmed burst/long_burst; blank ISIs between them are not allowed by default.",
    "\u672C\u9875\u6BD4\u8F83\u4E0D\u9501\u5B9A MANUAL \u533A\u95F4\u7684\u5F71\u5B50 AUTO \u68C0\u6D4B\u7ED3\u679C\u4E0E\u5F53\u524D MANUAL \u53C2\u8003\u6807\u8BB0\uFF1B\u5B83\u4E0D\u662F\u5916\u90E8\u72EC\u7ACB\u9A8C\u8BC1\uFF0C\u4E5F\u4E0D\u80FD\u5355\u72EC\u8BC1\u660E\u751F\u7269\u5B66\u6709\u6548\u6027\u3002" =
      "This page compares shadow AUTO detection, run without locking MANUAL intervals, with the current MANUAL reference annotations. It is not external independent validation and does not by itself establish biological validity.",
    "\u672C\u9875\u5728\u5DF2\u6807\u8BB0 trains \u5185\u8FDB\u884C\u6821\u51C6/\u9A8C\u8BC1\u62C6\u5206\u5E76\u8FD0\u884C\u5F71\u5B50\u68C0\u6D4B\uFF1B\u8FD9\u4E0D\u7B49\u4E8E\u5916\u90E8\u6CDB\u5316\u8BC1\u636E\uFF0C\u4E5F\u4E0D\u80FD\u5355\u72EC\u8BC1\u660E\u751F\u7269\u5B66\u6709\u6548\u6027\u3002" =
      "This page splits annotated trains into calibration and validation subsets and runs shadow detection. It is not evidence of external generalization and does not by itself establish biological validity.",
    "\u9884\u89C8\u662F\u8BD5\u8FD0\u884C\uFF1A\u53EA\u6BD4\u8F83 AUTO \u4E8B\u4EF6\u5DEE\u5F02\uFF0C\u4E0D\u8986\u76D6\u6B63\u5F0F\u68C0\u6D4B\u7ED3\u679C\u3002" =
      "The preview is a dry run: it compares AUTO event differences without overwriting formal detection results.",
    "\u5BFC\u51FA\u6587\u4EF6\u5305\u542B\u5F53\u524D UI \u53C2\u6570\u6811\u3001schema \u7248\u672C\u3001params_hash \u548C\u9A8C\u8BC1\u6458\u8981\u3002\u5BFC\u5165\u540E\u4F1A\u56DE\u586B\u4E13\u7528 UI \u4E0E\u7531\u53C2\u6570\u5408\u540C\u751F\u6210\u7684 UI\u3002" =
      "The export contains the current UI parameter tree, schema version, params_hash, and validation summary. Importing fills both dedicated UI controls and parameter-contract-generated UI.",
    "\u8BE5\u529F\u80FD\u4ECE\u624B\u52A8\u793A\u4F8B\u5B66\u4E60\u6BCF\u6761 train \u7684\u8F6F\u951A\u70B9\u3002\u624B\u52A8\u6807\u8BB0\u662F\u5C3A\u5EA6\u951A\u70B9\uFF0C\u4E0D\u662F\u786C\u8FB9\u754C\u6216\u76D1\u7763\u5206\u7C7B\u5668\uFF1B\u8BF7\u4F7F\u7528\u201C\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A\u201D\u9A8C\u8BC1\u6CDB\u5316\u80FD\u529B\u3002" =
      "This feature learns per-train soft anchors from manual examples. Manual labels are scale anchors, not hard boundaries or a supervised classifier; use the manual-labels-vs-detector report to validate generalization.",
    "\u4EC5\u4FDD\u7559\u672C\u4F1A\u8BDD\uFF1B\u4E0D\u5C5E\u4E8E\u79D1\u5B66\u5BA1\u8BA1\u8BB0\u5F55\u3002\u5207\u6362\u8BED\u8A00\u65F6\u4F1A\u6E05\u9664\u672C\u5217\u8868\uFF0C\u907F\u514D\u663E\u793A\u65E7\u8BED\u8A00\u6587\u6848\u3002" =
      "This list is retained only for the current session and is not a scientific audit record. Changing language clears it so stale copy from the previous language is not shown.",
    "\u67E5\u770B\u672C\u4F1A\u8BDD\u7684\u754C\u9762\u901A\u77E5" = "View interface notifications from this session",
    "\u672C\u4F1A\u8BDD\u5C1A\u65E0\u754C\u9762\u901A\u77E5\u3002" = "There are no interface notifications in this session yet.",
    "\u672C\u4F1A\u8BDD\u6700\u591A\u4FDD\u7559\u6700\u8FD1 5 \u6B21\u53EF\u64A4\u9500\u53D8\u66F4\uFF0C\u5E76\u53D7 64 MB \u538B\u7F29\u5386\u53F2\u4E0A\u9650\u7EA6\u675F\uFF1B\u5927\u6570\u636E\u96C6\u4E0D\u4F1A\u65E0\u754C\u589E\u957F\u5185\u5B58\u3002" =
      "This session retains at most the 5 most recent undoable changes and caps compressed history at 64 MB, preventing unbounded memory growth for large datasets.",
    "logISI phase portrait" = "logISI phase portrait",
    "Isomap 3D" = "Isomap 3D",
    "Mean-ISI" = "Mean-ISI",
    "LogISI / newBD" = "LogISI / newBD",
    stpd_i18n_supplemental_exact_dictionary(),
    stpd_i18n_static_ui_exact_dictionary()
  )
  duplicate_keys <- unique(names(out)[duplicated(names(out))])
  if (length(duplicate_keys) > 0L) {
    stop(
      "Duplicate exact UI-copy key(s): ",
      paste(duplicate_keys, collapse = ", "),
      call. = FALSE
    )
  }
  out
}

stpd_i18n_phrase_dictionary <- function() {
  c(
    # These status phrases also occur inside dynamic raster-axis labels such
    # as `Train_04 [\u5DF2\u5206\u6790]`; exact-node translation alone cannot cover
    # that case. Multi-character, semantically stable keys are safe here.
    "\u72B6\u6001\u672A\u77E5" = "status unknown",
    "\u672A\u5206\u6790" = "not analyzed",
    "\u5DF2\u5206\u6790" = "analyzed",
    "\u7ED3\u679C\u8FC7\u671F" = "results stale",
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
    "\u4E0B\u8F7D\u5F53\u524D\u5168\u91CF\u6807\u8BB0 CSV\uFF08\u5BBD\u8868\uFF09" = "download current full-run labeled CSV (wide)",
    "\u4E0B\u8F7D\u5F53\u524D\u5168\u91CF\u7ED3\u679C ZIP" = "download current full-run results ZIP",
    "\u6B63\u5F0F\u6807\u8BB0 CSV\uFF1A\u9700\u5148\u5B8C\u6210\u5168\u91CF\u8FD0\u884C" = "formal labeled CSV: complete a full run first",
    "\u6B63\u5F0F\u7ED3\u679C ZIP\uFF1A\u9700\u5148\u5B8C\u6210\u5168\u91CF\u8FD0\u884C" = "formal results ZIP: complete a full run first",
    "\u8BF7\u5148\u5BFC\u5165\u6570\u636E\u5E76\u5B8C\u6210\u4E00\u6B21\u5168\u90E8 trains \u7684\u68C0\u6D4B\u3002" = "Import data and complete one all-train detection run first.",
    "\u5F53\u524D\u53EA\u8FD0\u884C\u4E86\u90E8\u5206 trains\u3002\u8BF7\u53D6\u6D88\u201C\u4EC5\u68C0\u6D4B\u5F53\u524D\u53EF\u89C1 trains\u201D\uFF0C\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002" = "Only part of the dataset was run. Clear 'detect currently visible trains only', rerun all trains, and then export.",
    "\u7ED3\u679C\u5BF9\u8C61\u7684\u8FD0\u884C\u8EAB\u4EFD\u4E0E\u5F53\u524D\u754C\u9762\u5FEB\u7167\u4E0D\u4E00\u81F4\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002" = "The result object's run identity does not match the current UI snapshot. Rerun all trains before export.",
    "\u5F53\u524D\u754C\u9762\u53C2\u6570\u65E0\u6CD5\u5F62\u6210\u6709\u6548\u5FEB\u7167\u3002\u8BF7\u68C0\u67E5\u53C2\u6570\u5E76\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002" = "The current UI parameters cannot form a valid snapshot. Check them and rerun all trains before export.",
    "\u6700\u7EC8\u5BA1\u8BA1\u91CD\u5EFA\u5931\u8D25\uFF1A" = "Final-audit rebuild failed: ",
    "\u6700\u7EC8\u5BA1\u8BA1 possible \u5347\u7EA7\u5931\u8D25\uFF1A" = "Final-audit possible promotion failed: ",
    "\u6E05\u9664\u6700\u7EC8\u5BA1\u8BA1\u5C42\u5931\u8D25\uFF1A" = "Clearing the final-audit layer failed: ",
    "\u68C0\u6D4B\u7ED3\u679C\u8EAB\u4EFD\u5DF2\u6539\u53D8\u6216\u65E0\u6CD5\u9A8C\u8BC1\uFF1B\u8BF7\u5148\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\uFF0C\u518D\u4FEE\u6539 Legacy \u5BA1\u8BA1\u5C42\u3002" = "The detector-output identity changed or cannot be verified. Rerun detection before changing the Legacy audit layer.",
    "\u68C0\u6D4B\u8FD0\u884C\u8EAB\u4EFD\u5DF2\u6539\u53D8\uFF1B\u672A\u63D0\u4EA4 Legacy \u5BA1\u8BA1\u5C42\u4FEE\u6539\u3002" = "The detector run identity changed; the Legacy audit-layer change was not committed.",
    "Legacy \u5BA1\u8BA1\u64CD\u4F5C\u4FEE\u6539\u4E86\u5141\u8BB8\u8303\u56F4\u4E4B\u5916\u7684\u68C0\u6D4B\u4EA7\u7269\uFF1B\u5DF2\u53D6\u6D88\u6574\u4E2A\u63D0\u4EA4\u3002" = "The Legacy audit operation changed a detector artifact outside its allowlist; the entire commit was cancelled.",
    "\u4FEE\u6539\u540E\u7684 Legacy \u5BA1\u8BA1\u5C42\u65E0\u6CD5\u5B8C\u6574\u9A8C\u8BC1\uFF1B\u672A\u63D0\u4EA4\u66F4\u6539\u3002" = "The modified Legacy audit layer could not be fully verified; no change was committed.",
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
    "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A" = "manual labels vs detector report",
    "\u8BC4\u4F30\u68C0\u6D4B\u5668\u4E0E\u624B\u52A8\u6807\u7B7E" = "evaluate detector against manual labels",
    "\u4F30\u8BA1\u76EE\u6807\uFF1Adetector_performance\u3002" = "Estimand: detector_performance.",
    "\u672C\u9875\u6BD4\u8F83\u4E0D\u9501\u5B9A MANUAL \u533A\u95F4\u7684 shadow AUTO \u4E0E\u5F53\u524D MANUAL \u53C2\u8003\u6807\u8BB0\uFF1B\u5B83\u4E0D\u662F\u5916\u90E8\u72EC\u7ACB\u9A8C\u8BC1\uFF0C\u4E5F\u4E0D\u80FD\u5355\u72EC\u8BC1\u660E\u751F\u7269\u5B66\u6709\u6548\u6027\u3002" = "This page compares shadow AUTO, run without locking MANUAL intervals, with the current MANUAL reference annotations. It is not external independent validation and does not by itself establish biological validity.",
    "\u672C\u9875\u5728\u5DF2\u6807\u8BB0 trains \u5185\u8FDB\u884C calibration/validation \u62C6\u5206\u5E76\u8FD0\u884C shadow \u68C0\u6D4B\uFF1B\u8FD9\u4E0D\u7B49\u4E8E\u5916\u90E8\u6CDB\u5316\u8BC1\u636E\uFF0C\u4E5F\u4E0D\u80FD\u5355\u72EC\u8BC1\u660E\u751F\u7269\u5B66\u6709\u6548\u6027\u3002" = "This page splits annotated trains into calibration and validation subsets and runs shadow detection. It is not evidence of external generalization and does not by itself establish biological validity.",
    "\u6DF7\u6DC6\u77E9\u9635\u8BA1\u6570" = "confusion matrix counts",
    "\u624B\u52A8\u4E8B\u4EF6\u7EA7\u91CD\u53E0" = "manual event-level overlap",
    "\u8BAD\u7EC3\u96C6" = "training set",
    "\u9A8C\u8BC1\u96C6" = "validation set",
    "\u5212\u5206" = "split",
    "\u968F\u673A\u79CD\u5B50" = "random seed",
    "\u8FC7\u62DF\u5408\u62A5\u544A" = "overfitting report",
    "\u4E8B\u4EF6\u5339\u914D\u660E\u7EC6" = "event match details",
    "\u4E8B\u4EF6\u5339\u914D\u660E\u7EC6\u4E0E\u53C2\u6570\u53D8\u4F53" = "event match details vs parameter variants",
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
    "\u8868\u683C" = "table",
    "\u5355\u4F4D" = "unit",
    "\u6587\u4EF6" = "file",
    "\u5F53\u524D UI" = "current UI",
    "\u65E0\u91CF\u7EB2" = "dimensionless",
    "\u8F83\u5C11" = "fewer",
    "\u66F4\u5FEB" = "faster"
  )
}

# Dynamic, human-readable status lines use anchored templates rather than
# fragment replacement. Captures are inserted into {1}, {2}, ... markers.
# Anchoring is deliberate: an unknown or changed message must fail closed to
# its original text instead of becoming a plausible but incomplete sentence.
stpd_i18n_template_dictionary <- function() {
  c(
    "^\u6B63\u5728\u67E5\u770B\uFF1A([0-9]+)/([0-9]+) trains$" =
      "Viewing: {1}/{2} trains",
    "^\u4E0A\u6B21\u68C0\u6D4B\uFF1A([^ ]+) trains$" =
      "Last detection: {1} trains",
    "^\u6A21\u5F0F\uFF1A\u672A\u8FD0\u884C$" =
      "Mode: Not run",
    "^\u6A21\u5F0F\uFF1A(.+)$" =
      "Mode: {1}",
    "^\u65E0\u6CD5\u5B9A\u4F4D\u6240\u9009\u533A\u95F4\uFF1A(.+)$" =
      "Could not locate the selected interval: {1}",
    "^\u4E3A\u907F\u514D\u8BEF\u64CD\u4F5C\uFF0C\u8BF7\u8F93\u5165\uFF1A\u6E05\u7A7A$" =
      "To prevent accidental changes, type this token unchanged: \u6E05\u7A7A",
    "^\u5F53\u524D\u7ED3\u679C\u8986\u76D6 ([0-9]+)/([0-9]+) \u6761 train\uFF1B\u672A\u8FD0\u884C\u7684 train \u4E0D\u5E94\u663E\u793A\u4E3A\u9634\u6027\u3002$" =
      "Current results cover {1}/{2} train(s); trains that were not run must not be shown as negative.",
    "^\u6570\u636E\u3001\u53C2\u6570\u548C\u8FD0\u884C\u8303\u56F4\u4E00\u81F4\uFF08([0-9]+)/([0-9]+) \u6761 train\uFF09\u3002$" =
      "Data, parameters, and run scope match ({1}/{2} train(s)).",
    "^\u8BE5\u62A5\u544A\u8986\u76D6 ([0-9]+)/([0-9]+) \u6761 train\u3002$" =
      "This report covers {1}/{2} train(s).",
    "^\u6570\u636E\u3001\u8FD0\u884C\u3001\u771F\u503C\u548C\u9A8C\u8BC1\u8303\u56F4\u4E00\u81F4\uFF08([0-9]+)/([0-9]+) \u6761 train\uFF09\u3002$" =
      "Data, run, truth, and validation scope match ({1}/{2} train(s)).",
    "^\u5C06\u6E05\u9664\u5F53\u524D\u6570\u636E\u96C6 ([0-9]+) \u6761 trains \u4E2D\u7EA6 ([0-9]+) \u4E2A MANUAL/NOT-burst \u6807\u8BB0\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002$" =
      "This will clear approximately {2} MANUAL/NOT-burst labels across {1} train(s) in the current dataset. It can be undone in this session.",
    "^\u5C06\u6E05\u9664\u5F53\u524D\u6570\u636E\u96C6\u4E2D ([0-9]+) \u4E2A AUTO \u6807\u7B7E\u53CA\u5176\u5206\u6570\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002$" =
      "This will clear {1} AUTO label(s) and their scores from the current dataset. It can be undone in this session.",
    "^\u5C06\u4ECE\u5F53\u524D session \u79FB\u9664 ([0-9]+) \u4E2A\u6570\u636E\u96C6\uFF0C\u5E76\u6E05\u9664\u4E0E\u5B83\u4EEC\u7ED1\u5B9A\u7684\u754C\u9762\u9A8C\u8BC1\u72B6\u6001\u3002\u6B64\u64CD\u4F5C\u53EF\u5728\u672C session \u4E2D\u64A4\u9500\u3002$" =
      "This will remove {1} dataset(s) from the current session and clear the UI validation state bound to them. It can be undone in this session.",
    "^\u5C06\u68C0\u67E5\u5E76\u4FEE\u6539 ([0-9]+) \u4E2A\u6570\u636E\u96C6\uFF0C\u6709\u53D8\u5316\u7684\u6570\u636E\u96C6\u5176 AUTO \u4E0E\u68C0\u6D4B\u7ED3\u679C\u5C06\u5931\u6548\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002$" =
      "This will inspect and modify {1} dataset(s). AUTO labels and detection results will be invalidated in any dataset that changes. It can be undone in this session.",
    "^\u5C06\u6309\u5DF2\u9884\u89C8\u7684\u56FA\u5B9A\u8303\u56F4\uFF0C\u628A ([0-9]+) \u4E2A possible_burst event / ([0-9]+) \u4E2A ISI \u5199\u5165 Legacy MANUAL burst\u3002\u4E0D\u4F1A\u8986\u76D6\u5DF2\u6709 MANUAL/NOT-burst\uFF0C\u4E5F\u4E0D\u4F1A\u521B\u5EFA Phase 2B transition\u3002$" =
      "Using the fixed previewed scope, this will write {1} possible_burst event(s) / {2} ISI(s) to Legacy MANUAL burst. It will not overwrite existing MANUAL/NOT-burst evidence or create a Phase 2B transition.",
    "^\u9884\u89C8\u5B8C\u6210\uFF1A([0-9]+) \u6761 train\uFF1B\u53EF\u5347\u7EA7 ([0-9]+) \u4E2A possible_burst event / ([0-9]+) \u4E2A ISI\u3002$" =
      "Preview complete: {1} train(s); {2} possible_burst event(s) / {3} ISI(s) are eligible for promotion.",
    "^\u5DF2\u6267\u884C\u5347\u7EA7\uFF1A([0-9]+) \u4E2A event / ([0-9]+) \u4E2A ISI \u5DF2\u4ECE AUTO possible_burst \u5199\u4E3A MANUAL burst\uFF1BAUTO \u539F\u59CB\u6807\u7B7E\u548C override \u5BA1\u8BA1\u5DF2\u4FDD\u7559\u3002$" =
      "Promotion complete: {1} event(s) / {2} ISI(s) were written from AUTO possible_burst to MANUAL burst; the original AUTO labels and override audit were preserved.",
    "^\u5DF2\u64A4\u56DE\uFF1A([0-9]+) \u4E2A event / ([0-9]+) \u4E2A ISI\u3002\u540E\u7EED\u88AB\u624B\u52A8\u6539\u52A8\u8FC7\u7684\u884C\u672A\u88AB\u8986\u76D6\u3002$" =
      "Reverted: {1} event(s) / {2} ISI(s). Rows changed manually afterward were not overwritten.",
    "^\u5DF2\u5B8C\u6210\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\uFF1A([0-9]+) \u4E2A\u53C2\u6570\uFF0C([0-9]+) \u6761 train\uFF1B([0-9]+) \u4E2A\u53D8\u4F53\u4EA7\u751F\u4E8B\u4EF6\u5DEE\u5F02\u3002$" =
      "Parameter-sensitivity scan complete: {1} parameter(s), {2} train(s); {3} variant(s) produced event differences.",
    "^\u5DF2\u6062\u590D ([0-9]+) \u4E2A\u6570\u636E\u96C6\u3002$" =
      "Restored {1} dataset(s).",
    "^\u6B63\u5728\u68C0\u6D4B train ([0-9]+)/([0-9]+)(: .+)?$" =
      "Detecting train {1}/{2}{3}",
    "^\u5DF2\u5B8C\u6210 train ([0-9]+)/([0-9]+)(: .+)?$" =
      "Completed train {1}/{2}{3}"
  )
}

stpd_i18n_contains_cjk <- function(x) {
  grepl("[\u3400-\u9FFF\uF900-\uFAFF]", x %||% "", perl = TRUE)
}

stpd_i18n_apply_template <- function(text, pattern, replacement) {
  hit <- regexec(pattern, text, perl = TRUE)
  captures <- regmatches(text, hit)[[1]]
  if (length(captures) == 0) return(NULL)
  captures <- captures[-1]
  out <- replacement
  for (ii in seq_along(captures)) {
    out <- gsub(paste0("{", ii, "}"), captures[[ii]] %||% "", out, fixed = TRUE)
  }
  out
}

# Server-side mirror of the client safety contract, kept internal for focused
# unit tests and future server-rendered copy. The UI still switches language
# client-side without rebuilding uploaded state.
stpd_i18n_translate_text <- function(source, lang = "en") {
  source <- enc2utf8(as.character(source %||% ""))[1]
  if (!identical(lang, "en") || !nzchar(trimws(source))) return(source)
  leading <- regmatches(source, regexpr("^\\s*", source, perl = TRUE))
  trailing <- regmatches(source, regexpr("\\s*$", source, perl = TRUE))
  core <- trimws(source)

  exact <- stpd_i18n_exact_dictionary()
  if (core %in% names(exact)) {
    return(paste0(leading, unname(exact[[core]]), trailing))
  }

  templates <- stpd_i18n_template_dictionary()
  for (pattern in names(templates)) {
    translated <- stpd_i18n_apply_template(core, pattern, unname(templates[[pattern]]))
    if (!is.null(translated)) return(paste0(leading, translated, trailing))
  }

  phrases <- stpd_i18n_phrase_dictionary()
  keys <- names(phrases)
  keys <- keys[nchar(keys, type = "chars") > 1L]
  keys <- keys[order(nchar(keys, type = "chars"), decreasing = TRUE)]
  out <- core
  for (key in keys) out <- gsub(key, unname(phrases[[key]]), out, fixed = TRUE)

  # Never delete residual Chinese. A partially translated sentence is more
  # misleading than an explicitly untranslated one, especially in scientific
  # guidance and destructive-action copy.
  if (stpd_i18n_contains_cjk(out)) return(source)

  out <- gsub("\uFF08", "(", out, fixed = TRUE)
  out <- gsub("\uFF09", ")", out, fixed = TRUE)
  out <- gsub("\uFF1A", ": ", out, fixed = TRUE)
  out <- gsub("\uFF1B", "; ", out, fixed = TRUE)
  out <- gsub("\uFF0C", ", ", out, fixed = TRUE)
  out <- gsub("\u3002", ". ", out, fixed = TRUE)
  out <- gsub("[[:space:]]+", " ", out, perl = TRUE)
  out <- gsub("[[:space:]]+([,.;:!?%)\\]])", "\\1", out, perl = TRUE)
  out <- trimws(out)
  paste0(leading, out, trailing)
}

stpd_i18n_assets <- function(default_lang = "zh") {
  exact_json <- stpd_i18n_json_object(stpd_i18n_exact_dictionary())
  phrase_json <- stpd_i18n_json_object(stpd_i18n_phrase_dictionary())
  template_json <- stpd_i18n_json_object(stpd_i18n_template_dictionary())
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
        const templates = ", template_json, ";
        const defaultLang = ", stpd_i18n_json_quote(default_lang), ";
        const textOriginals = new WeakMap();
        const textApplied = new WeakMap();
        const attrOriginalPrefix = 'data-stpd-i18n-original-';
        const attrAppliedPrefix = 'data-stpd-i18n-applied-';
        const cjkPattern = /[\\u3400-\\u9FFF\\uF900-\\uFAFF]/;
        const attrNames = ['title', 'placeholder', 'aria-label', 'data-original-title'];
        const phraseKeys = Object.keys(phrases)
          .filter(function(key) { return Array.from(key).length > 1; })
          .sort(function(a, b) { return b.length - a.length; });
        const templateKeys = Object.keys(templates);
        const translatablePreIds = new Set([
          'batch_status',
          'detector_before_after_summary',
          'final_audit_status',
          'isi_profile_ref_text',
          'methodological_warning',
          'near_miss_details',
          'near_miss_rerun_summary',
          'parameter_delta_preview_status',
          'parameter_sensitivity_status',
          'possible_burst_promotion_status'
        ]);
        function readStoredLanguage() {
          try {
            const stored = window.localStorage.getItem('stpd_ui_language');
            return stored === 'en' || stored === 'zh' ? stored : '';
          } catch (error) {
            return '';
          }
        }

        function writeStoredLanguage(lang) {
          try {
            window.localStorage.setItem('stpd_ui_language', lang);
          } catch (error) {
            // Storage can be unavailable in private or embedded browser modes.
          }
        }

        let currentLang = readStoredLanguage() || defaultLang;
        let reportedLang = '';
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
          for (const pattern of templateKeys) {
            const match = bits.core.match(new RegExp(pattern));
            if (!match) continue;
            const translated = templates[pattern].replace(/\\{([0-9]+)\\}/g, function(marker, index) {
              return match[Number(index)] || '';
            });
            return bits.leading + translated + bits.trailing;
          }
          let out = bits.core;
          for (const key of phraseKeys) {
            if (out.indexOf(key) !== -1) out = replacePhraseWithContext(out, key, phrases[key]);
          }
          if (cjkPattern.test(out)) return source;
          out = tidyEnglish(out);
          return bits.leading + out + bits.trailing;
        }

        function shouldSkipElement(el) {
          if (!el || el.nodeType !== 1) return false;
          if (el.closest('script, style, textarea, code, .dataTable tbody')) return true;
          const pre = el.closest('pre');
          if (pre && !translatablePreIds.has(pre.id) && !pre.classList.contains('stpd-i18n-status')) return true;
          return false;
        }

        function translateTextNode(node) {
          if (!node || node.nodeType !== Node.TEXT_NODE || !node.nodeValue || !node.nodeValue.trim()) return;
          const parent = node.parentElement;
          if (shouldSkipElement(parent)) return;
          const cur = node.nodeValue;
          let original = textOriginals.get(node);
          const lastApplied = textApplied.get(node);
          if (!original || (typeof lastApplied === 'string' && cur !== lastApplied)) {
            original = cur;
            textOriginals.set(node, original);
          }
          if (parent && parent.classList.contains('shiny-notification-content-text')) {
            parent.setAttribute('data-stpd-i18n-source-text', original.trim());
          }
          const next = currentLang === 'en' ? translateText(original) : original;
          if (node.nodeValue !== next) node.nodeValue = next;
          textApplied.set(node, next);
        }

        function originalAttrName(attr) {
          return attrOriginalPrefix + attr.replace(/[^A-Za-z0-9_-]/g, '_');
        }

        function appliedAttrName(attr) {
          return attrAppliedPrefix + attr.replace(/[^A-Za-z0-9_-]/g, '_');
        }

        function translateElementAttrs(el) {
          if (!el || el.nodeType !== 1 || shouldSkipElement(el)) return;
          const attrs = attrNames.slice();
          if (el.matches('input[type=\"button\"], input[type=\"submit\"], input[type=\"reset\"]')) attrs.push('value');
          attrs.forEach(function(attr) {
            if (!el.hasAttribute(attr)) return;
            const storeName = originalAttrName(attr);
            const appliedName = appliedAttrName(attr);
            const cur = el.getAttribute(attr);
            let original = el.getAttribute(storeName);
            const lastApplied = el.getAttribute(appliedName);
            if (!original || (lastApplied !== null && cur !== lastApplied)) {
              original = cur;
              el.setAttribute(storeName, original);
            }
            const next = currentLang === 'en' ? translateText(original) : original;
            if (cur !== next) el.setAttribute(attr, next);
            el.setAttribute(appliedName, next);
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

        function reportLanguage() {
          if (reportedLang === currentLang) return;
          if (!window.Shiny || typeof window.Shiny.setInputValue !== 'function') return;
          window.Shiny.setInputValue('ui_language', currentLang, { priority: 'event' });
          reportedLang = currentLang;
        }

        function setLanguage(lang) {
          currentLang = lang === 'en' ? 'en' : 'zh';
          writeStoredLanguage(currentLang);
          document.documentElement.lang = currentLang === 'en' ? 'en' : 'zh-Hans';
          document.body.classList.toggle('stpd-lang-en', currentLang === 'en');
          document.body.classList.toggle('stpd-lang-zh', currentLang !== 'en');
          applying = true;
          walk(document.body);
          applying = false;
          reportLanguage();
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

        document.addEventListener('shiny:connected', function() {
          reportedLang = '';
          reportLanguage();
        });

        window.stpdSetLanguage = function(lang) {
          setLanguage(lang);
          syncToggle();
        };
      })();
    ")))
  )
}
