import Foundation
import STPDCore
import STPDTabularIO
import SwiftUI

private enum ScientificImportReviewStep: String, CaseIterable, Identifiable {
    case source
    case datasetMeaning
    case columnsAndGroups
    case eventAttributes
    case review

    var id: Self { self }

    var title: String {
        switch self {
        case .source: "Source"
        case .datasetMeaning: "Dataset meaning"
        case .columnsAndGroups: "Columns & groups"
        case .eventAttributes: "Events & attributes"
        case .review: "Review"
        }
    }

    var systemImage: String {
        switch self {
        case .source: "doc.badge.gearshape"
        case .datasetMeaning: "waveform.path.ecg"
        case .columnsAndGroups: "rectangle.3.group"
        case .eventAttributes: "tag"
        case .review: "checklist"
        }
    }
}

struct ScientificImportSheet: View {
    @Bindable var coordinator: ScientificImportCoordinator
    let onClose: () -> Void

    @State private var selectedStep: ScientificImportReviewStep = .source
    @State private var selectedAttributeIDs: Set<UUID> = []
    @State private var originCandidatePage = 0

    var body: some View {
        NavigationSplitView {
            List(ScientificImportReviewStep.allCases, selection: $selectedStep) { step in
                Label(step.title, systemImage: step.systemImage)
                    .tag(step)
                    .foregroundStyle(isStepAvailable(step) ? .primary : .tertiary)
            }
            .navigationSplitViewColumnWidth(min: 190, ideal: 220, max: 260)
        } detail: {
            VStack(spacing: 0) {
                detail
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                Divider()
                footer
            }
        }
        .frame(minWidth: 1_080, idealWidth: 1_240, minHeight: 720, idealHeight: 800)
        .interactiveDismissDisabled(coordinator.isBusy)
        .onChange(of: coordinator.phase) { _, newPhase in
            if newPhase == .reviewingScientificMeaning && selectedStep == .source {
                selectedStep = .datasetMeaning
            }
        }
    }

    @ViewBuilder
    private var detail: some View {
        switch selectedStep {
        case .source:
            sourceStep
        case .datasetMeaning:
            if coordinator.stagedImport != nil {
                datasetMeaningStep
            } else {
                unavailableStageB
            }
        case .columnsAndGroups:
            if coordinator.stagedImport != nil {
                columnsAndGroupsStep
            } else {
                unavailableStageB
            }
        case .eventAttributes:
            if coordinator.stagedImport != nil {
                eventAttributesStep
            } else {
                unavailableStageB
            }
        case .review:
            if coordinator.stagedImport != nil {
                reviewStep
            } else {
                unavailableStageB
            }
        }
    }

    private var sourceStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                stepHeader(
                    "Bind the source",
                    subtitle: "Stage A records only the exact file, worksheet, and header rule. It makes no scientific assumptions."
                )

                if coordinator.phase == .readingSource {
                    ProgressView("Reading one bounded source snapshot…")
                        .controlSize(.large)
                        .frame(maxWidth: .infinity, minHeight: 260)
                } else if let source = coordinator.source {
                    sourceIdentityCard(source)
                    sourceDecisionCard(source)

                    if let error = coordinator.failureMessage {
                        errorBanner(error)
                    }

                    if let staged = coordinator.stagedImport {
                        sourcePreview(staged)
                    }
                } else if let error = coordinator.failureMessage {
                    errorBanner(error)
                    ContentUnavailableView(
                        "Source not bound",
                        systemImage: "doc.badge.exclamationmark",
                        description: Text("Close this review and choose a supported CSV or XLSX file.")
                    )
                    .frame(maxWidth: .infinity, minHeight: 260)
                } else {
                    ContentUnavailableView(
                        "No source selected",
                        systemImage: "doc",
                        description: Text("Choose Import Data from the toolbar or File menu.")
                    )
                    .frame(maxWidth: .infinity, minHeight: 260)
                }
            }
            .padding(24)
        }
    }

    private var datasetMeaningStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                stepHeader(
                    "Confirm dataset meaning",
                    subtitle: "These decisions apply to the whole file. No value is inferred from headers or timestamp magnitudes."
                )

                GroupBox("Timestamp contract") {
                    VStack(alignment: .leading, spacing: 12) {
                        Picker("Source timestamp unit", selection: sourceTimeUnitBinding) {
                            Text("Unresolved").tag("")
                            Text("Seconds (s)").tag(SpikeTimeUnit.seconds.rawValue)
                            Text("Milliseconds (ms)").tag(SpikeTimeUnit.milliseconds.rawValue)
                        }
                        .frame(maxWidth: 360, alignment: .leading)

                        Text("Accepted timestamps must map exactly to signed integer microseconds. Display precision is 6 decimals for seconds and 3 decimals for milliseconds; the importer never silently rounds. Negative recording-elapsed timestamps are blockers. Signed event-relative timestamps may be valid only with an explicitly confirmed event origin.")
                            .font(.callout)
                            .foregroundStyle(.secondary)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.vertical, 6)
                }

                GroupBox("Activity represented by this file") {
                    VStack(alignment: .leading, spacing: 12) {
                        Picker("Activity mode", selection: activityModeBinding) {
                            Text("Unresolved").tag(nil as ScientificDatasetActivityMode?)
                            Text("Putative single-unit").tag(
                                ScientificDatasetActivityMode.putativeSingleUnit as ScientificDatasetActivityMode?
                            )
                            Text("Intentional multi-unit — Experimental").tag(
                                ScientificDatasetActivityMode.intentionalMultiUnit as ScientificDatasetActivityMode?
                            )
                            Text("Unknown or uncertain").tag(
                                ScientificDatasetActivityMode.unknownOrUncertain as ScientificDatasetActivityMode?
                            )
                        }
                        .frame(maxWidth: 420, alignment: .leading)

                        Text(activityModeExplanation)
                            .font(.callout)
                            .foregroundStyle(.secondary)

                        Text("Changing activity mode clears every spike-column duplicate decision, because coincident timestamps have different scientific meaning across modes. Reconfirm each policy explicitly.")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.vertical, 6)
                }

                authorityNotice
            }
            .padding(24)
        }
    }

    private var columnsAndGroupsStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                stepHeader(
                    "Define columns and event groups",
                    subtitle: "Every source column is assigned exactly once. Each contiguous group must follow S+ E*: one or more spike trains, followed by zero or more event columns."
                )

                if let form = coordinator.manifestForm {
                    LazyVStack(alignment: .leading, spacing: 16) {
                        ForEach(form.columns) { column in
                            columnEditor(id: column.id)
                        }
                    }
                }
            }
            .padding(24)
        }
    }

    private var eventAttributesStep: some View {
        ScrollView([.horizontal, .vertical]) {
            VStack(alignment: .leading, spacing: 16) {
                stepHeader(
                    "Confirm event attributes",
                    subtitle: "Every key needs an explicit type, Scientific or Presentation role, unit decision, and empty-string policy. Batch role assignment still writes one decision per key."
                )

                HStack(spacing: 10) {
                    Button {
                        originCandidatePage = 0
                        Task { await coordinator.refreshPreflight() }
                    } label: {
                        if coordinator.phase == .preflightingSourceFacts {
                            ProgressView()
                                .controlSize(.small)
                        } else {
                            Label("Discover source facts", systemImage: "magnifyingglass")
                        }
                    }
                    .buttonStyle(.borderedProminent)
                    .disabled(coordinator.isBusy)

                    Button {
                        mutateForm { $0.addAttribute() }
                    } label: {
                        Label("Add attribute", systemImage: "plus")
                    }

                    Button("Set selected: Scientific") {
                        mutateForm { $0.applyAttributeRole(.scientific, to: selectedAttributeIDs) }
                    }
                    .disabled(selectedAttributeIDs.isEmpty)

                    Button("Set selected: Presentation") {
                        mutateForm { $0.applyAttributeRole(.presentation, to: selectedAttributeIDs) }
                    }
                    .disabled(selectedAttributeIDs.isEmpty)

                    Button(role: .destructive) {
                        mutateForm { $0.removeAttributes(ids: selectedAttributeIDs) }
                        selectedAttributeIDs.removeAll()
                    } label: {
                        Label("Remove selected", systemImage: "trash")
                    }
                    .disabled(selectedAttributeIDs.isEmpty)
                }

                attributeHeader
                if let form = coordinator.manifestForm, form.attributes.isEmpty {
                    Text("No attribute definitions yet. Add keys manually, or run Discover source facts to add unresolved keys found beside event timestamps.")
                        .foregroundStyle(.secondary)
                        .padding(.vertical, 18)
                } else if let form = coordinator.manifestForm {
                    LazyVStack(alignment: .leading, spacing: 8) {
                        ForEach(form.attributes) { attribute in
                            attributeEditor(id: attribute.id)
                            Divider()
                        }
                    }
                }

                Text("An inline @x.unit=… value is only an import suggestion. The confirmed manifest unit above is the sole authority; conflicts block validation.")
                    .font(.callout)
                    .foregroundStyle(.secondary)

                GroupBox("Event-column metadata grammar") {
                    HStack(alignment: .top, spacing: 20) {
                        Text(
                            """
                            event_stimulus
                            1.250000
                            @intensity=2.5
                            @intensity.unit=mW
                            @note=
                            2.000000
                            @condition=reward
                            """
                        )
                        .font(.system(.caption, design: .monospaced))
                        .textSelection(.enabled)

                        Text("Each timestamp starts one event occurrence. Consecutive @key=value rows in that same event column belong to that occurrence until the next timestamp or blank. @note= means an explicitly present empty string only when note is confirmed as String with Allow Empty String; omitting the @note row means the key is missing. The .unit row is a suggestion and cannot override the confirmed manifest.")
                            .font(.callout)
                            .foregroundStyle(.secondary)
                            .frame(maxWidth: 620, alignment: .leading)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.vertical, 6)
                }

                preflightFacts
            }
            .padding(24)
            .frame(minWidth: 1_050, alignment: .topLeading)
        }
    }

    private var reviewStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                stepHeader(
                    "Review before validation",
                    subtitle: "The next boundary will run resolve → exact normalization → independent validation and show source-located blockers."
                )

                if let staged = coordinator.stagedImport {
                    sourcePreview(staged)
                }

                reviewOutcome

                authorityNotice

                Label(
                    "PreparedScientificImport is not yet a canonical dataset, confirmation receipt, or analysis-authority grant. This preparation step never replaces active data.",
                    systemImage: "lock.shield"
                )
                .foregroundStyle(.orange)
                .padding(14)
                .background(.orange.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
            }
            .padding(24)
        }
    }

    private var unavailableStageB: some View {
        ContentUnavailableView(
            "Bind source facts first",
            systemImage: "lock",
            description: Text("Select the worksheet and header rule in Source, then bind the exact table snapshot.")
        )
    }

    private var footer: some View {
        HStack(spacing: 12) {
            Text(phaseDescription)
                .font(.callout)
                .foregroundStyle(.secondary)
            Spacer()

            if selectedStep == .source,
               coordinator.source != nil,
               coordinator.phase != .readingSource {
                Button {
                    Task { await coordinator.bindSourceFacts() }
                } label: {
                    if coordinator.phase == .stagingSource {
                        ProgressView()
                            .controlSize(.small)
                    } else {
                        Text(coordinator.stagedImport == nil ? "Bind source facts" : "Rebind source facts")
                    }
                }
                .buttonStyle(.borderedProminent)
                .disabled(!coordinator.canBindSourceFacts || coordinator.isBusy)
            }

            if selectedStep == .review, coordinator.stagedImport != nil {
                Button {
                    Task { await coordinator.validateScientificReview() }
                } label: {
                    if coordinator.phase == .validatingScientificMeaning {
                        ProgressView()
                            .controlSize(.small)
                    } else {
                        Text("Run exact validation")
                    }
                }
                .buttonStyle(.borderedProminent)
                .disabled(coordinator.isBusy)
            }

            Button("Cancel review", role: .cancel) {
                coordinator.cancel()
                onClose()
            }
        }
        .padding(.horizontal, 18)
        .padding(.vertical, 12)
    }

    private func sourceIdentityCard(_ source: BoundedScientificSource) -> some View {
        GroupBox("Exact source snapshot") {
            Grid(alignment: .leading, horizontalSpacing: 18, verticalSpacing: 8) {
                GridRow {
                    Text("File").foregroundStyle(.secondary)
                    Text(source.displayName)
                }
                GridRow {
                    Text("Format").foregroundStyle(.secondary)
                    Text(source.format.rawValue.uppercased())
                }
                GridRow {
                    Text("Size").foregroundStyle(.secondary)
                    Text(ByteCountFormatter.string(fromByteCount: Int64(source.byteCount), countStyle: .file))
                }
                GridRow {
                    Text("SHA-256").foregroundStyle(.secondary)
                    Text(source.sourceSHA256)
                        .font(.system(.caption, design: .monospaced))
                        .textSelection(.enabled)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 6)
        }
    }

    private func sourceDecisionCard(_ source: BoundedScientificSource) -> some View {
        GroupBox("Explicit source decisions") {
            VStack(alignment: .leading, spacing: 14) {
                if source.format == .xlsx {
                    Picker("Worksheet", selection: worksheetBinding) {
                        Text("Unresolved").tag(nil as UInt32?)
                        ForEach(coordinator.worksheets, id: \.sheetID) { worksheet in
                            Text("\(worksheet.name) — \(visibilityLabel(worksheet.visibility))")
                                .tag(worksheet.sheetID as UInt32?)
                        }
                    }
                    .frame(maxWidth: 520, alignment: .leading)
                    Text("Hidden worksheets remain visible in this list and must still be selected explicitly.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }

                Picker("First logical row", selection: headerDecisionBinding) {
                    Text("Unresolved").tag(nil as CanonicalTabularHeaderDecision?)
                    Text("Use as column headers").tag(
                        CanonicalTabularHeaderDecision.firstRecordIsHeader as CanonicalTabularHeaderDecision?
                    )
                    Text("Treat as data (headerless)").tag(
                        CanonicalTabularHeaderDecision.headerless as CanonicalTabularHeaderDecision?
                    )
                }
                .frame(maxWidth: 520, alignment: .leading)

                if coordinator.headerDecision == .headerless {
                    Label(
                        "Headerless input is restricted to one recording-elapsed group containing spike columns only; event columns are not allowed.",
                        systemImage: "exclamationmark.triangle"
                    )
                    .font(.callout)
                    .foregroundStyle(.orange)
                }

                Text("Changing either decision discards all Stage B choices. Nothing is migrated silently between source bindings.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 6)
        }
    }

    private func sourcePreview(_ staged: StagedScientificImport) -> some View {
        let visibleColumns = Array(staged.columns.prefix(12))
        let visibleRowCount = min(staged.dataRowCount, 20)
        return GroupBox("Read-only table preview") {
            VStack(alignment: .leading, spacing: 8) {
                ScrollView([.horizontal, .vertical]) {
                    VStack(alignment: .leading, spacing: 0) {
                        HStack(spacing: 0) {
                            previewCell("Row", isHeader: true, width: 52)
                            ForEach(visibleColumns, id: \.sourceColumn) { column in
                                previewCell(
                                    column.header ?? "Column \(column.sourceColumn.oneBasedIndex)",
                                    isHeader: true
                                )
                            }
                        }
                        ForEach(0..<visibleRowCount, id: \.self) { row in
                            HStack(spacing: 0) {
                                previewCell("\(row + 1)", isHeader: true, width: 52)
                                ForEach(visibleColumns, id: \.sourceColumn) { column in
                                    previewCell(stagedCellText(column.cells[row]), isHeader: false)
                                }
                            }
                        }
                    }
                }
                .frame(maxHeight: 380)

                if staged.columns.count > visibleColumns.count || staged.dataRowCount > visibleRowCount {
                    Text("Preview shows the first \(visibleColumns.count) of \(staged.columns.count) columns and \(visibleRowCount) of \(staged.dataRowCount) data rows. All rows and columns remain bound in staging.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            .padding(.vertical, 6)
        }
    }

    private func columnEditor(id: UUID) -> some View {
        let form = columnForm(id: id)
        let index = coordinator.manifestForm?.columns.firstIndex(where: { $0.id == id }) ?? 0
        let sourceIndex = form?.sourceColumn.oneBasedIndex ?? index + 1
        let isGroupStart = index == 0 || form?.startsNewGroup == true
        return GroupBox {
            VStack(alignment: .leading, spacing: 12) {
                HStack(alignment: .firstTextBaseline) {
                    Text("Column \(sourceIndex)")
                        .font(.headline)
                    Text(form?.header.map { "Header: \($0)" } ?? "Headerless")
                        .foregroundStyle(.secondary)
                    Spacer()

                    if index == 0 {
                        Text("Starts group 1")
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                    } else {
                        Picker("Boundary", selection: columnBoundaryBinding(id)) {
                            Text("Unresolved").tag(nil as Bool?)
                            Text("Continue group").tag(false as Bool?)
                            Text("Start new group").tag(true as Bool?)
                        }
                        .labelsHidden()
                        .frame(width: 170)
                    }
                }

                if isGroupStart {
                    HStack(spacing: 12) {
                        TextField("Group semantic ID", text: columnTextBinding(id, \.groupSemanticIDText))
                            .frame(width: 220)
                        Picker("Time basis", selection: columnOptionalBinding(id, \.groupTimeBasis)) {
                            Text("Unresolved").tag(nil as ScientificImportTimeBasisChoice?)
                            Text("Recording elapsed").tag(
                                ScientificImportTimeBasisChoice.recordingElapsed as ScientificImportTimeBasisChoice?
                            )
                            Text("Event-relative").tag(
                                ScientificImportTimeBasisChoice.eventRelative as ScientificImportTimeBasisChoice?
                            )
                        }
                        .frame(width: 250)

                        if form?.groupTimeBasis == .eventRelative {
                            TextField(
                                "Origin event column",
                                text: columnTextBinding(id, \.eventOriginColumnText)
                            )
                            .frame(width: 150)
                            TextField(
                                "Origin data row",
                                text: columnTextBinding(id, \.eventOriginDataRowText)
                            )
                            .frame(width: 140)
                        }
                    }
                }

                HStack(spacing: 12) {
                    Picker("Role", selection: columnOptionalBinding(id, \.role)) {
                        Text("Unresolved").tag(nil as ScientificImportColumnRole?)
                        Text("Spike train").tag(
                            ScientificImportColumnRole.spikeTrain as ScientificImportColumnRole?
                        )
                        Text("Event definition").tag(
                            ScientificImportColumnRole.eventDefinition as ScientificImportColumnRole?
                        )
                    }
                    .frame(width: 190)

                    TextField("Column semantic ID", text: columnTextBinding(id, \.semanticIDText))
                        .frame(width: 210)

                    if form?.role == .eventDefinition {
                        TextField("Event type ID", text: columnTextBinding(id, \.eventTypeIDText))
                            .frame(width: 180)
                    }

                    Picker("Order", selection: columnOptionalBinding(id, \.orderDecision)) {
                        Text("Order unresolved").tag(nil as TimestampOrderDecision?)
                        Text("Preserve source order").tag(
                            TimestampOrderDecision.preserveSourceOrder as TimestampOrderDecision?
                        )
                        Text("Stable ascending sort").tag(
                            TimestampOrderDecision.stableAscendingSort as TimestampOrderDecision?
                        )
                    }
                    .frame(width: 220)

                    if form?.role == .spikeTrain {
                        Picker("Exact duplicates", selection: columnOptionalBinding(id, \.duplicateDecision)) {
                            Text("Duplicates unresolved").tag(nil as ExactDuplicateDecision?)
                            Text("Preserve multiplicity").tag(
                                ExactDuplicateDecision.preserveMultiplicity as ExactDuplicateDecision?
                            )
                            if coordinator.manifestForm?.activityMode == .putativeSingleUnit {
                                Text("Virtual collapse in SU analysis").tag(
                                    ExactDuplicateDecision.collapseExact as ExactDuplicateDecision?
                                )
                            }
                        }
                        .frame(width: 210)
                        .disabled(coordinator.manifestForm?.activityMode == nil)
                        .help(
                            duplicateDecisionHelp
                        )
                    }
                }
            }
            .padding(.vertical, 5)
        }
    }

    private var attributeHeader: some View {
        HStack(spacing: 8) {
            Text("").frame(width: 24)
            Text("Key").frame(width: 170, alignment: .leading)
            Text("Type").frame(width: 150, alignment: .leading)
            Text("Role").frame(width: 150, alignment: .leading)
            Text("Unit").frame(width: 160, alignment: .leading)
            Text("Specified symbol").frame(width: 150, alignment: .leading)
            Text("Empty string").frame(width: 180, alignment: .leading)
        }
        .font(.caption.weight(.semibold))
        .foregroundStyle(.secondary)
    }

    private func attributeEditor(id: UUID) -> some View {
        let form = attributeForm(id: id)
        return HStack(spacing: 8) {
            Toggle(
                "Select attribute",
                isOn: Binding(
                    get: { selectedAttributeIDs.contains(id) },
                    set: { selected in
                        if selected { selectedAttributeIDs.insert(id) }
                        else { selectedAttributeIDs.remove(id) }
                    }
                )
            )
            .labelsHidden()
            .frame(width: 24)

            TextField("key", text: attributeTextBinding(id, \.keyText))
                .frame(width: 170)
            Picker("Type", selection: attributeOptionalBinding(id, \.scalarType)) {
                Text("Unresolved").tag(nil as EventAttributeScalarType?)
                ForEach(EventAttributeScalarType.allCases, id: \.self) { type in
                    Text(attributeTypeLabel(type)).tag(type as EventAttributeScalarType?)
                }
            }
            .labelsHidden()
            .frame(width: 150)
            Picker("Role", selection: attributeOptionalBinding(id, \.role)) {
                Text("Unresolved").tag(nil as EventAttributeRole?)
                Text("Scientific").tag(EventAttributeRole.scientific as EventAttributeRole?)
                Text("Presentation").tag(EventAttributeRole.presentation as EventAttributeRole?)
            }
            .labelsHidden()
            .frame(width: 150)
            Picker("Unit", selection: attributeOptionalBinding(id, \.unitChoice)) {
                Text("Unresolved").tag(nil as ScientificImportAttributeUnitChoice?)
                Text("N/A").tag(
                    ScientificImportAttributeUnitChoice.notApplicable as ScientificImportAttributeUnitChoice?
                )
                Text("Dimensionless").tag(
                    ScientificImportAttributeUnitChoice.dimensionless as ScientificImportAttributeUnitChoice?
                )
                Text("Specified").tag(
                    ScientificImportAttributeUnitChoice.specified as ScientificImportAttributeUnitChoice?
                )
            }
            .labelsHidden()
            .frame(width: 160)
            TextField("e.g. mW", text: attributeTextBinding(id, \.specifiedUnitText))
                .frame(width: 150)
                .disabled(form?.unitChoice != .specified)
            Picker("Empty string", selection: attributeOptionalBinding(id, \.emptyStringPolicy)) {
                Text("Unresolved").tag(nil as EventEmptyStringPolicy?)
                Text("Forbid").tag(EventEmptyStringPolicy.forbid as EventEmptyStringPolicy?)
                Text("Allow explicit empty").tag(
                    EventEmptyStringPolicy.allowExplicitEmptyString as EventEmptyStringPolicy?
                )
            }
            .labelsHidden()
            .frame(width: 180)
        }
    }

    private var authorityNotice: some View {
        VStack(alignment: .leading, spacing: 6) {
            Label("Authority remains locked", systemImage: "lock.shield")
                .font(.headline)
            Text("Putative single-unit data may become eligible only after all decisions are complete, exact normalization and independent validation have zero blockers, and the user explicitly confirms. Multi-unit and unknown/uncertain data remain browse-only and cannot run the single-unit detector.")
                .font(.callout)
                .foregroundStyle(.secondary)
        }
        .padding(14)
        .background(.blue.opacity(0.07), in: RoundedRectangle(cornerRadius: 8))
    }

    @ViewBuilder
    private var preflightFacts: some View {
        if !coordinator.preflightFormIssues.isEmpty {
            issueList(
                title: "Preflight form blockers",
                messages: coordinator.preflightFormIssues.map { String(describing: $0) },
                additionalCount: 0
            )
        } else if let report = coordinator.preflightReport {
            VStack(alignment: .leading, spacing: 12) {
                Text("Source-located preflight")
                    .font(.headline)

                if report.hasIssues {
                    issueList(
                        title: "Preflight findings",
                        messages: report.issues.map { String(describing: $0) },
                        additionalCount: report.additionalIssueCount
                    )
                }

                if !report.columnFacts.isEmpty {
                    Text("Timestamp facts")
                        .font(.subheadline.weight(.semibold))
                    Grid(alignment: .leading, horizontalSpacing: 18, verticalSpacing: 6) {
                        GridRow {
                            Text("Group / column")
                            Text("Role")
                            Text("Valid")
                            Text("Negative source values")
                            Text("Descents")
                            Text("Exact duplicates")
                        }
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                        ForEach(Array(report.columnFacts.prefix(100).enumerated()), id: \.offset) { _, fact in
                            GridRow {
                                Text("G\(fact.groupIndex) / C\(fact.sourceColumn.oneBasedIndex)")
                                Text(fact.role == .spikeTrain ? "Spike" : "Event")
                                Text("\(fact.validTimestampCount)")
                                Text("\(fact.negativeTimestampCount)")
                                Text("\(fact.sourceOrderDescentCount)")
                                Text("\(fact.exactDuplicateTimestampCount)")
                            }
                            .font(.caption.monospacedDigit())
                        }
                    }
                    if report.columnFacts.count > 100 {
                        Text("Showing the first 100 of \(report.columnFacts.count) classified columns.")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                }

                if !report.discoveredEventAttributes.isEmpty {
                    Text("Discovered event attributes")
                        .font(.subheadline.weight(.semibold))
                    ForEach(
                        Array(report.discoveredEventAttributes.prefix(100).enumerated()),
                        id: \.offset
                    ) { _, attribute in
                        let shownUnitSuggestions = attribute.inlineUnitSuggestions
                            .prefix(10)
                            .map(\.rawUnit)
                        HStack(alignment: .firstTextBaseline, spacing: 12) {
                            Text(attribute.key.canonicalText)
                                .font(.system(.callout, design: .monospaced))
                                .frame(width: 220, alignment: .leading)
                            Text("\(attribute.valueSightings.count) value sighting(s)")
                            if !shownUnitSuggestions.isEmpty {
                                Text(
                                    "Inline unit suggestions: \(shownUnitSuggestions.joined(separator: ", "))"
                                        + (attribute.inlineUnitSuggestions.count > shownUnitSuggestions.count
                                            ? " … (+\(attribute.inlineUnitSuggestions.count - shownUnitSuggestions.count))"
                                            : "")
                                )
                                    .foregroundStyle(.secondary)
                            }
                        }
                    }
                }

                if !report.eventOriginCandidates.isEmpty {
                    Text("Event-relative origin candidates")
                        .font(.subheadline.weight(.semibold))
                    let pageSize = 50
                    let pageCount = max(
                        1,
                        (report.eventOriginCandidates.count + pageSize - 1) / pageSize
                    )
                    let shownPage = min(originCandidatePage, pageCount - 1)
                    let start = shownPage * pageSize
                    let end = min(start + pageSize, report.eventOriginCandidates.count)

                    HStack(spacing: 10) {
                        Button("Previous") {
                            originCandidatePage = max(0, shownPage - 1)
                        }
                        .disabled(shownPage == 0)
                        Button("Next") {
                            originCandidatePage = min(pageCount - 1, shownPage + 1)
                        }
                        .disabled(shownPage + 1 >= pageCount)
                        Text("Page \(shownPage + 1) of \(pageCount) · candidates \(start + 1)–\(end) of \(report.eventOriginCandidates.count)")
                            .font(.caption.monospacedDigit())
                            .foregroundStyle(.secondary)
                    }

                    ForEach(Array(report.eventOriginCandidates[start..<end]), id: \.self) { candidate in
                        VStack(alignment: .leading, spacing: 4) {
                            HStack(spacing: 10) {
                                Text(originCandidateLocationText(candidate))
                                    .font(.system(.caption, design: .monospaced))
                                Button("Use as origin") {
                                    applyOriginCandidate(candidate)
                                }
                                .controlSize(.small)
                            }
                            if !candidate.scientificAttributes.isEmpty {
                                Text("Scientific identity fields: \(originCandidateAttributesText(candidate))")
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                                    .textSelection(.enabled)
                            }
                        }
                    }
                }
            }
            .padding(14)
            .background(.secondary.opacity(0.06), in: RoundedRectangle(cornerRadius: 8))
        } else {
            Text("Run Discover source facts after assigning timestamp unit, group boundaries, and column roles. Discovered keys are added with type, role, unit, and empty policy still unresolved.")
                .font(.callout)
                .foregroundStyle(.secondary)
        }
    }

    @ViewBuilder
    private var reviewOutcome: some View {
        switch coordinator.reviewOutcome {
        case .none:
            Label(
                "Validation has not run for the current draft.",
                systemImage: "circle.dotted"
            )
            .foregroundStyle(.secondary)
        case .formIssues(let issues):
            issueList(
                title: "Form blockers",
                messages: issues.map { String(describing: $0) },
                additionalCount: 0
            )
        case .planIssues(let issues):
            issueList(
                title: "Manifest blockers",
                messages: issues.map { String(describing: $0) },
                additionalCount: 0
            )
        case .normalizationIssues(let issues, let additionalCount):
            issueList(
                title: "Exact-data blockers",
                messages: issues.map { String(describing: $0) },
                additionalCount: additionalCount
            )
        case .validation(let report):
            if report.hasBlockingIssues {
                issueList(
                    title: "Independent validation blockers",
                    messages: report.blockingIssues.map { String(describing: $0) },
                    additionalCount: report.additionalBlockingIssueCount
                )
            } else {
                VStack(alignment: .leading, spacing: 8) {
                    Label(
                        "Exact normalization and independent validation passed.",
                        systemImage: "checkmark.seal.fill"
                    )
                    .font(.headline)
                    .foregroundStyle(.green)
                    if !report.warnings.isEmpty || report.additionalWarningCount > 0 {
                        Text("Warnings: \(report.warnings.count + report.additionalWarningCount)")
                            .foregroundStyle(.orange)
                    }
                    Text("This proves reproducibility of the prepared value only; it does not grant scientific authority or change the active dataset.")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
                .padding(14)
                .frame(maxWidth: .infinity, alignment: .leading)
                .background(.green.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
            }
        }
    }

    private func issueList(
        title: String,
        messages: [String],
        additionalCount: Int
    ) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Label(title, systemImage: "xmark.octagon.fill")
                .font(.headline)
                .foregroundStyle(.red)
            ForEach(Array(messages.enumerated()), id: \.offset) { _, message in
                Text("• \(message)")
                    .font(.system(.caption, design: .monospaced))
                    .textSelection(.enabled)
            }
            if additionalCount > 0 {
                Text("…and \(additionalCount) additional blocker(s).")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .padding(14)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.red.opacity(0.07), in: RoundedRectangle(cornerRadius: 8))
    }

    private func stepHeader(_ title: String, subtitle: String) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.title2.weight(.semibold))
            Text(subtitle)
                .foregroundStyle(.secondary)
        }
    }

    private func errorBanner(_ message: String) -> some View {
        Label(message, systemImage: "xmark.octagon.fill")
            .foregroundStyle(.red)
            .padding(12)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.red.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
    }

    private func previewCell(_ text: String, isHeader: Bool, width: CGFloat = 160) -> some View {
        Text(text)
            .font(isHeader ? .caption.weight(.semibold) : .caption.monospaced())
            .lineLimit(1)
            .truncationMode(.middle)
            .frame(width: width, height: 28, alignment: .leading)
            .padding(.horizontal, 6)
            .background(isHeader ? Color.secondary.opacity(0.10) : Color.clear)
            .overlay(Rectangle().stroke(Color.secondary.opacity(0.16), lineWidth: 0.5))
    }

    private func stagedCellText(_ value: StagedCellValue) -> String {
        switch value {
        case .blank:
            return ""
        case .text(let rawText):
            return rawText
        case .spreadsheetNumber(let rawLexeme):
            return rawLexeme
        }
    }

    private func visibilityLabel(_ visibility: CanonicalXLSXWorksheetVisibility) -> String {
        switch visibility {
        case .visible: "visible"
        case .hidden: "hidden"
        case .veryHidden: "very hidden"
        }
    }

    private func attributeTypeLabel(_ type: EventAttributeScalarType) -> String {
        switch type {
        case .string: "String"
        case .integer: "Integer"
        case .exactDecimal: "Exact decimal"
        case .boolean: "Boolean"
        }
    }

    private var activityModeExplanation: String {
        switch coordinator.manifestForm?.activityMode {
        case .putativeSingleUnit:
            "Single-unit interpretation is still provisional until the later authority gate succeeds."
        case .intentionalMultiUnit:
            "Experimental placeholder only. Single-unit thresholds and detectors will not be reused."
        case .unknownOrUncertain:
            "The table may be reviewed, but no definitive biological interpretation or authoritative detector output is allowed."
        case .none:
            "Choose the acquisition meaning explicitly; one file cannot mix activity modes by column."
        }
    }

    private var duplicateDecisionHelp: String {
        switch coordinator.manifestForm?.activityMode {
        case .putativeSingleUnit:
            "Canonical raw data always retains every duplicate timestamp. Choose whether a later putative-single-unit analysis view may virtually collapse exact duplicates; source and canonical multiplicity are never erased."
        case .intentionalMultiUnit, .unknownOrUncertain:
            "Canonical raw data preserves multiplicity. Multi-unit and uncertain data cannot borrow the putative-single-unit virtual-collapse policy."
        case .none:
            "Choose the activity mode before confirming how exact duplicate timestamps are represented."
        }
    }

    private var phaseDescription: String {
        switch coordinator.phase {
        case .idle: "No import review is active."
        case .readingSource: "Reading one exact bounded snapshot…"
        case .awaitingSourceDecisions: "Waiting for explicit Stage A decisions."
        case .stagingSource: "Binding the selected table…"
        case .reviewingScientificMeaning: "Source facts are bound; scientific choices remain a draft."
        case .preflightingSourceFacts: "Scanning exact timestamps and event metadata with the Core grammar…"
        case .validatingScientificMeaning: "Running exact normalization and independent validation…"
        case .validatedPreparation: "Prepared value validated; authority and active data remain locked."
        case .failed: "The import could not be completed; the active dataset was not changed."
        }
    }

    private func isStepAvailable(_ step: ScientificImportReviewStep) -> Bool {
        step == .source || coordinator.stagedImport != nil
    }

    private var headerDecisionBinding: Binding<CanonicalTabularHeaderDecision?> {
        Binding(
            get: { coordinator.headerDecision },
            set: { decision in
                if let decision { coordinator.selectHeaderDecision(decision) }
            }
        )
    }

    private var worksheetBinding: Binding<UInt32?> {
        Binding(
            get: { coordinator.selectedWorksheetSheetID },
            set: { sheetID in
                if let sheetID { coordinator.selectWorksheet(sheetID: sheetID) }
            }
        )
    }

    private var sourceTimeUnitBinding: Binding<String> {
        Binding(
            get: { coordinator.manifestForm?.sourceTimeUnit?.rawValue ?? "" },
            set: { rawValue in
                mutateForm { form in
                    form.sourceTimeUnit = SpikeTimeUnit(rawValue: rawValue)
                }
            }
        )
    }

    private var activityModeBinding: Binding<ScientificDatasetActivityMode?> {
        Binding(
            get: { coordinator.manifestForm?.activityMode },
            set: { value in mutateForm { $0.selectActivityMode(value) } }
        )
    }

    private func columnBoundaryBinding(_ id: UUID) -> Binding<Bool?> {
        columnOptionalBinding(id, \.startsNewGroup)
    }

    private func columnTextBinding(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportColumnDecisionForm, String>
    ) -> Binding<String> {
        Binding(
            get: { columnForm(id: id)?[keyPath: keyPath] ?? "" },
            set: { value in
                mutateForm { form in
                    form.updateColumn(id: id) { column in
                        column[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func columnOptionalBinding<Value: Hashable>(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportColumnDecisionForm, Value?>
    ) -> Binding<Value?> {
        Binding(
            get: { columnForm(id: id)?[keyPath: keyPath] },
            set: { value in
                mutateForm { form in
                    form.updateColumn(id: id) { column in
                        column[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func attributeTextBinding(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportAttributeDecisionForm, String>
    ) -> Binding<String> {
        Binding(
            get: { attributeForm(id: id)?[keyPath: keyPath] ?? "" },
            set: { value in
                mutateForm { form in
                    form.updateAttribute(id: id) { attribute in
                        attribute[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func attributeOptionalBinding<Value: Hashable>(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportAttributeDecisionForm, Value?>
    ) -> Binding<Value?> {
        Binding(
            get: { attributeForm(id: id)?[keyPath: keyPath] },
            set: { value in
                mutateForm { form in
                    form.updateAttribute(id: id) { attribute in
                        attribute[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func columnForm(id: UUID) -> ScientificImportColumnDecisionForm? {
        coordinator.manifestForm?.columns.first(where: { $0.id == id })
    }

    private func attributeForm(id: UUID) -> ScientificImportAttributeDecisionForm? {
        coordinator.manifestForm?.attributes.first(where: { $0.id == id })
    }

    private func originCandidateLocationText(
        _ candidate: ScientificImportEventOriginCandidate
    ) -> String {
        let eventID = candidate.eventDefinitionID?.semanticID.canonicalText
            ?? "unresolved-event-ID"
        return "G\(candidate.groupIndex) · \(eventID) · C\(candidate.sourceColumn.oneBasedIndex) · data row \(candidate.timestampCell.oneBasedDataRowIndex) · \(candidate.sourceTick.microseconds) µs"
    }

    private func originCandidateAttributesText(
        _ candidate: ScientificImportEventOriginCandidate
    ) -> String {
        candidate.scientificAttributes.map { attribute in
            let prefix = String(attribute.rawValue.prefix(80))
            let suffix = attribute.rawValue.count > prefix.count ? "…" : ""
            return "\(attribute.key.canonicalText)=\(prefix)\(suffix)"
        }
        .joined(separator: ", ")
    }

    private func applyOriginCandidate(_ candidate: ScientificImportEventOriginCandidate) {
        guard coordinator.preflightReport?.eventOriginCandidates.contains(candidate) == true else {
            return
        }
        mutateForm { form in
            let groupStartIndices = form.columns.indices.filter { index in
                index == 0 || form.columns[index].startsNewGroup == true
            }
            let groupOffset = candidate.groupIndex - 1
            guard groupStartIndices.indices.contains(groupOffset) else { return }
            let startIndex = groupStartIndices[groupOffset]
            let groupStartID = form.columns[startIndex].id
            form.updateColumn(id: groupStartID) { column in
                column.groupTimeBasis = .eventRelative
                column.eventOriginColumnText = String(candidate.sourceColumn.oneBasedIndex)
                column.eventOriginDataRowText =
                    String(candidate.timestampCell.oneBasedDataRowIndex)
            }
        }
    }

    private func mutateForm(_ mutation: (inout ScientificImportManifestForm) -> Void) {
        guard var form = coordinator.manifestForm else { return }
        mutation(&form)
        coordinator.manifestForm = form
    }
}
