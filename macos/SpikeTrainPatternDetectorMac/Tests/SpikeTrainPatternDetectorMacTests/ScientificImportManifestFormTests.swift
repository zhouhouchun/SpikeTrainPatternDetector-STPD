import Foundation
@testable import SpikeTrainPatternDetectorMac
import STPDCore
import STPDTabularIO
import Testing

@Suite("Scientific import manifest form")
struct ScientificImportManifestFormTests {
    @Test("A new form contains no silent scientific choices")
    func newFormIsUnresolved() throws {
        let staged = try makeStagedImport(headers: ["unit_A", "event_stimulus"])

        let form = ScientificImportManifestForm(stagedImport: staged)

        #expect(form.sourceTimeUnit == nil)
        #expect(form.activityMode == nil)
        #expect(form.columns.count == 2)
        #expect(form.columns[0].startsNewGroup == true)
        #expect(form.columns[1].startsNewGroup == nil)
        #expect(form.columns.allSatisfy { $0.role == nil })
        #expect(form.columns.allSatisfy { $0.orderDecision == nil })
        #expect(form.columns.allSatisfy { $0.duplicateDecision == nil })
        #expect(form.attributes.isEmpty)
    }

    @Test("Explicit contiguous S+ E* choices build the approved two-group layout")
    func buildsApprovedTwoGroupLayout() throws {
        let staged = try makeStagedImport(
            headers: [
                "unit_A", "unit_B", "event_stimulus", "event_reward",
                "unit_C", "unit_D", "event_light"
            ]
        )
        var form = ScientificImportManifestForm(stagedImport: staged)
        form.sourceTimeUnit = .seconds
        form.activityMode = .putativeSingleUnit

        configureGroupStart(&form.columns[0], id: "group_1")
        configureSpike(&form.columns[0], id: "unit_A")
        form.columns[1].startsNewGroup = false
        configureSpike(&form.columns[1], id: "unit_B")
        form.columns[2].startsNewGroup = false
        configureEvent(&form.columns[2], id: "stimulus", type: "stimulus")
        form.columns[3].startsNewGroup = false
        configureEvent(&form.columns[3], id: "reward", type: "reward")

        form.columns[4].startsNewGroup = true
        configureGroupStart(&form.columns[4], id: "group_2")
        configureSpike(&form.columns[4], id: "unit_C")
        form.columns[5].startsNewGroup = false
        configureSpike(&form.columns[5], id: "unit_D")
        form.columns[6].startsNewGroup = false
        configureEvent(&form.columns[6], id: "light", type: "light")

        let draft = try form.makeManifestDraft(boundTo: staged)
        let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)

        #expect(plan.eventScopeGroups.count == 2)
        #expect(plan.eventScopeGroups[0].spikeTrains.map(\.sourceColumn.oneBasedIndex) == [1, 2])
        #expect(plan.eventScopeGroups[0].eventDefinitions.map(\.sourceColumn.oneBasedIndex) == [3, 4])
        #expect(plan.eventScopeGroups[1].spikeTrains.map(\.sourceColumn.oneBasedIndex) == [5, 6])
        #expect(plan.eventScopeGroups[1].eventDefinitions.map(\.sourceColumn.oneBasedIndex) == [7])
    }

    @Test("Every boundary after the first column requires an explicit decision")
    func missingBoundaryBlocksDraft() throws {
        let staged = try makeStagedImport(headers: ["unit_A", "unit_B"])
        var form = ScientificImportManifestForm(stagedImport: staged)
        configureGroupStart(&form.columns[0], id: "group")
        configureSpike(&form.columns[0], id: "unit_A")
        configureSpike(&form.columns[1], id: "unit_B")

        #expect(throws: ScientificImportManifestFormBuildError.self) {
            try form.makeManifestDraft(boundTo: staged)
        }

        do {
            _ = try form.makeManifestDraft(boundTo: staged)
            Issue.record("Expected an explicit-boundary failure")
        } catch let error as ScientificImportManifestFormBuildError {
            #expect(error.issues == [.missingGroupBoundary(column: 2)])
        }
    }

    @Test("A spike after an event in the same group is blocked")
    func spikeAfterEventIsBlocked() throws {
        let staged = try makeStagedImport(headers: ["unit_A", "event", "unit_B"])
        var form = ScientificImportManifestForm(stagedImport: staged)
        configureGroupStart(&form.columns[0], id: "group")
        configureSpike(&form.columns[0], id: "unit_A")
        form.columns[1].startsNewGroup = false
        configureEvent(&form.columns[1], id: "event", type: "stimulus")
        form.columns[2].startsNewGroup = false
        configureSpike(&form.columns[2], id: "unit_B")

        do {
            _ = try form.makeManifestDraft(boundTo: staged)
            Issue.record("Expected S+ E* grammar enforcement")
        } catch let error as ScientificImportManifestFormBuildError {
            #expect(error.issues.contains(.spikeAfterEventWithinGroup(column: 3)))
        }
    }

    @Test("Attribute roles may be assigned in one batch but remain per-key decisions")
    func batchAttributeRoleAssignment() throws {
        let staged = try makeStagedImport(headers: ["unit_A"])
        var form = ScientificImportManifestForm(stagedImport: staged)
        form.addAttribute(keyText: "intensity")
        form.addAttribute(keyText: "condition")
        form.addAttribute(keyText: "note")

        let selected = Set(form.attributes.prefix(2).map(\.id))
        form.applyAttributeRole(.scientific, to: selected)

        #expect(form.attributes[0].role == .scientific)
        #expect(form.attributes[1].role == .scientific)
        #expect(form.attributes[2].role == nil)
    }

    @Test("A form cannot be rebound to a same-shaped table with different source cells")
    func exactSourceBindingCannotBeRebound() throws {
        let reviewed = try makeStagedImport(
            headers: ["unit_A"],
            cellsByColumn: [[.text(rawText: "1.000000")]]
        )
        let replacement = try makeStagedImport(
            headers: ["unit_A"],
            cellsByColumn: [[.text(rawText: "2.000000")]]
        )
        let form = ScientificImportManifestForm(stagedImport: reviewed)

        do {
            _ = try form.makeManifestDraft(boundTo: replacement)
            Issue.record("Expected the exact staged-source binding to reject rebinding")
        } catch let error as ScientificImportManifestFormBuildError {
            #expect(error.issues == [.sourceBindingChanged])
        }
    }

    @Test("Equivalent tables with different exact source bytes cannot share one form")
    func exactTransactionDigestCannotBeRebound() throws {
        let lineFeedSource = try CanonicalCSVStagingReader.read(
            data: Data("unit_A\n1.000000\n".utf8),
            headerDecision: .firstRecordIsHeader,
            limits: .supportedDatasetEnvelope
        )
        let carriageReturnLineFeedSource = try CanonicalCSVStagingReader.read(
            data: Data("unit_A\r\n1.000000\r\n".utf8),
            headerDecision: .firstRecordIsHeader,
            limits: .supportedDatasetEnvelope
        )
        #expect(lineFeedSource.columns == carriageReturnLineFeedSource.columns)
        #expect(lineFeedSource.sourceTransactionBinding
            != carriageReturnLineFeedSource.sourceTransactionBinding)
        let form = ScientificImportManifestForm(stagedImport: lineFeedSource)

        do {
            _ = try form.makeManifestDraft(boundTo: carriageReturnLineFeedSource)
            Issue.record("Expected the exact source-byte transaction binding to reject rebinding")
        } catch let error as ScientificImportManifestFormBuildError {
            #expect(error.issues == [.sourceBindingChanged])
        }
    }

    @Test("Stable attribute IDs keep delayed edits attached to the intended row")
    func stableAttributeIDsSurviveEarlierRowRemoval() throws {
        let staged = try makeStagedImport(headers: ["unit_A"])
        var form = ScientificImportManifestForm(stagedImport: staged)
        form.addAttribute(keyText: "first")
        form.addAttribute(keyText: "second")
        form.addAttribute(keyText: "third")
        let firstID = form.attributes[0].id
        let secondID = form.attributes[1].id
        let thirdID = form.attributes[2].id

        form.removeAttributes(ids: [firstID])
        let updated = form.updateAttribute(id: secondID) { attribute in
            attribute.keyText = "second_edited"
        }

        #expect(updated)
        #expect(form.attributes.map(\.id) == [secondID, thirdID])
        #expect(form.attributes.map(\.keyText) == ["second_edited", "third"])
    }

    @Test("A delayed column edit from a replaced form cannot target the new source row")
    func staleColumnIDCannotEditReplacementForm() throws {
        let staged = try makeStagedImport(headers: ["unit_A"])
        let oldForm = ScientificImportManifestForm(stagedImport: staged)
        var replacementForm = ScientificImportManifestForm(stagedImport: staged)
        let staleID = oldForm.columns[0].id

        let updated = replacementForm.updateColumn(id: staleID) { column in
            column.semanticIDText = "wrong_target"
        }

        #expect(!updated)
        #expect(replacementForm.columns[0].semanticIDText.isEmpty)
    }

    @Test("Changing activity mode invalidates every contextual duplicate decision")
    func activityModeChangeClearsDuplicateDecisions() throws {
        let staged = try makeStagedImport(headers: ["unit_A", "unit_B"])
        var form = ScientificImportManifestForm(stagedImport: staged)
        form.selectActivityMode(.putativeSingleUnit)
        form.columns[0].duplicateDecision = .collapseExact
        form.columns[1].duplicateDecision = .preserveMultiplicity

        form.selectActivityMode(.intentionalMultiUnit)

        #expect(form.activityMode == .intentionalMultiUnit)
        #expect(form.columns.allSatisfy { $0.duplicateDecision == nil })

        form.columns[0].duplicateDecision = .preserveMultiplicity
        form.selectActivityMode(.unknownOrUncertain)
        #expect(form.columns.allSatisfy { $0.duplicateDecision == nil })
    }

    @Test("Specified attribute units require an explicit nonblank symbol")
    func specifiedUnitRequiresSymbol() throws {
        let staged = try makeStagedImport(headers: ["unit_A"])
        var form = ScientificImportManifestForm(stagedImport: staged)
        configureGroupStart(&form.columns[0], id: "group")
        configureSpike(&form.columns[0], id: "unit_A")
        form.sourceTimeUnit = .seconds
        form.activityMode = .putativeSingleUnit
        form.addAttribute(keyText: "intensity")
        form.attributes[0].scalarType = .exactDecimal
        form.attributes[0].role = .scientific
        form.attributes[0].unitChoice = .specified
        form.attributes[0].emptyStringPolicy = .forbid

        do {
            _ = try form.makeManifestDraft(boundTo: staged)
            Issue.record("Expected the missing specified-unit symbol to block")
        } catch let error as ScientificImportManifestFormBuildError {
            #expect(error.issues == [.missingSpecifiedUnit(definition: 1)])
        }
    }

    private func makeStagedImport(
        headers: [String],
        cellsByColumn: [[StagedCellValue]]? = nil
    ) throws -> StagedScientificImport {
        let columns = try headers.enumerated().map { offset, header in
            StagedScientificColumn(
                sourceColumn: try StagedSourceColumnReference(oneBasedIndex: offset + 1),
                header: header,
                cells: cellsByColumn?[offset] ?? []
            )
        }
        return try StagedScientificImport(
            source: .commaSeparatedValues,
            columns: columns,
            suggestions: .none
        )
    }

    private func configureGroupStart(
        _ column: inout ScientificImportColumnDecisionForm,
        id: String
    ) {
        column.startsNewGroup = true
        column.groupSemanticIDText = id
        column.groupTimeBasis = .recordingElapsed
    }

    private func configureSpike(
        _ column: inout ScientificImportColumnDecisionForm,
        id: String
    ) {
        column.role = .spikeTrain
        column.semanticIDText = id
        column.orderDecision = .preserveSourceOrder
        column.duplicateDecision = .preserveMultiplicity
    }

    private func configureEvent(
        _ column: inout ScientificImportColumnDecisionForm,
        id: String,
        type: String
    ) {
        column.role = .eventDefinition
        column.semanticIDText = id
        column.eventTypeIDText = type
        column.orderDecision = .preserveSourceOrder
    }
}
