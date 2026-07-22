import Foundation
import STPDCore
import Testing

// P8 — pure tests for the manual hard-threshold SCOPE model: defaults, current/selected/all resolution and
// application, the fresh-load reset vs same-dataset-reinstall preserve rule, and the DetectionInputsSignature staleness
// behavior. No detector math is exercised (this phase is state/model/UI wiring only).

// MARK: - defaults

@Test func scope_defaultIsAllTrains() {
    #expect(ManualThresholdScopeKind.default == .allTrains)
    #expect(ManualThresholdScope.allTrains.kind == .allTrains)
    #expect(ManualThresholdScope.allTrains.trainIDs.isEmpty)
    // `.allTrains` targets every train and never "targets no train".
    #expect(ManualThresholdScope.allTrains.appliesTo(trainID: "anything"))
    #expect(!ManualThresholdScope.allTrains.targetsNoTrain)
    #expect(ManualThresholdScope.allTrains.resolvedCount(allTrainCount: 5) == 5)
    #expect(ManualThresholdScopeKind.allCases == [.allTrains, .currentTrain, .selectedTrains])
}

// MARK: - selected-train-only application

@Test func scope_selectedTrainsAppliesOnlyToSelected() {
    let all = ["t1", "t2", "t3", "t4"]
    let scope = ManualThresholdScope.resolve(
        kind: .selectedTrains, focusedTrainID: "t1", selectedTrainIDs: ["t2", "t4"], allTrainIDs: all)
    #expect(scope.kind == .selectedTrains)
    #expect(scope.trainIDs == ["t2", "t4"])             // sorted, only the selected ones
    #expect(scope.appliesTo(trainID: "t2"))
    #expect(scope.appliesTo(trainID: "t4"))
    #expect(!scope.appliesTo(trainID: "t1"))            // focused but not selected → not targeted
    #expect(!scope.appliesTo(trainID: "t3"))
    #expect(scope.resolvedCount(allTrainCount: 4) == 2)
    #expect(!scope.targetsNoTrain)
}

@Test func scope_selectedTrainsIgnoresIDsNotInDataset() {
    let scope = ManualThresholdScope.resolve(
        kind: .selectedTrains, focusedTrainID: nil, selectedTrainIDs: ["t2", "ghost"], allTrainIDs: ["t1", "t2", "t3"])
    #expect(scope.trainIDs == ["t2"])   // a stale selection id not in the dataset is dropped
}

@Test func scope_selectedTrainsWithEmptySelection_targetsNoTrain() {
    let scope = ManualThresholdScope.resolve(
        kind: .selectedTrains, focusedTrainID: "t1", selectedTrainIDs: [], allTrainIDs: ["t1", "t2"])
    #expect(scope.trainIDs.isEmpty)
    #expect(scope.targetsNoTrain)                        // SAFETY: never silently falls back to "all"
    #expect(!scope.appliesTo(trainID: "t1"))
    #expect(scope.resolvedCount(allTrainCount: 2) == 0)
}

// MARK: - current-train application + fallbacks

@Test func scope_currentTrainAppliesOnlyToFocused() {
    let scope = ManualThresholdScope.resolve(
        kind: .currentTrain, focusedTrainID: "t3", selectedTrainIDs: ["t1", "t2"], allTrainIDs: ["t1", "t2", "t3"])
    #expect(scope.kind == .currentTrain)
    #expect(scope.trainIDs == ["t3"])
    #expect(scope.appliesTo(trainID: "t3"))
    #expect(!scope.appliesTo(trainID: "t1"))
}

@Test func scope_currentTrainWithNoFocus_targetsNoTrain() {
    let scope = ManualThresholdScope.resolve(
        kind: .currentTrain, focusedTrainID: nil, selectedTrainIDs: ["t1"], allTrainIDs: ["t1", "t2"])
    #expect(scope.targetsNoTrain)
    let stale = ManualThresholdScope.resolve(
        kind: .currentTrain, focusedTrainID: "ghost", selectedTrainIDs: [], allTrainIDs: ["t1", "t2"])
    #expect(stale.targetsNoTrain)   // focused id not in the dataset → empty, not "all"
}

// MARK: - all-train application

@Test func scope_allTrainsAppliesEverywhereRegardlessOfSelection() {
    let scope = ManualThresholdScope.resolve(
        kind: .allTrains, focusedTrainID: "t1", selectedTrainIDs: ["t2"], allTrainIDs: ["t1", "t2", "t3"])
    #expect(scope == .allTrains)             // selection/focus are irrelevant when scope is all
    #expect(scope.appliesTo(trainID: "t1"))
    #expect(scope.appliesTo(trainID: "t2"))
    #expect(scope.appliesTo(trainID: "t3"))
    #expect(scope.resolvedCount(allTrainCount: 3) == 3)
}

// MARK: - fresh dataset reset vs same-dataset reinstall preserve

@Test func scope_freshDatasetLoadResetsToAllTrains() {
    // A fresh dataset load resets the manual thresholds; the scope follows and resets to `.allTrains`.
    #expect(ManualThresholdScopeKind.selectedTrains.afterDatasetInstall(manualThresholdsReset: true) == .allTrains)
    #expect(ManualThresholdScopeKind.currentTrain.afterDatasetInstall(manualThresholdsReset: true) == .allTrains)
    #expect(ManualThresholdScopeKind.allTrains.afterDatasetInstall(manualThresholdsReset: true) == .allTrains)
}

@Test func scope_sameDatasetReinstallPreservesScopeWhenThresholdsPreserved() {
    // A same-dataset duplicate-policy reinstall preserves the manual thresholds (manualThresholdsReset == false),
    // so the scope is preserved too — preserved ONLY because the thresholds are.
    #expect(ManualThresholdScopeKind.selectedTrains.afterDatasetInstall(manualThresholdsReset: false) == .selectedTrains)
    #expect(ManualThresholdScopeKind.currentTrain.afterDatasetInstall(manualThresholdsReset: false) == .currentTrain)
    #expect(ManualThresholdScopeKind.allTrains.afterDatasetInstall(manualThresholdsReset: false) == .allTrains)
}

// MARK: - DetectionInputsSignature staleness

private func signature(scope: ManualThresholdScope) -> DetectionInputsSignature {
    DetectionInputsSignature(
        datasetID: "ds",
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        stateTuning: StatePatternDetectorTuning(),
        detectorParameters: .defaults,
        manualThresholdProfile: .automatic,
        manualThresholdScope: scope)
}

@Test func scope_defaultScopeKeepsSignatureByteIdentical() {
    // Omitting the scope (existing call sites) and passing `.allTrains` produce equal signatures — no spurious staleness.
    let omitted = DetectionInputsSignature(
        datasetID: "ds", bandSettings: TrainAdaptiveBandSettings(), qualitySettings: SpikeQualitySettings(),
        stateTuning: StatePatternDetectorTuning(), detectorParameters: .defaults, manualThresholdProfile: .automatic)
    #expect(omitted == signature(scope: .allTrains))
}

@Test func scope_changingScopeMarksSignatureStale() {
    let all = signature(scope: .allTrains)
    let selected = signature(scope: ManualThresholdScope(kind: .selectedTrains, trainIDs: ["t1"]))
    #expect(all != selected)                                   // changing scope kind flips the signature
    let selectedOther = signature(scope: ManualThresholdScope(kind: .selectedTrains, trainIDs: ["t2"]))
    #expect(selected != selectedOther)                         // changing the resolved target set also flips it
    let selectedSame = signature(scope: ManualThresholdScope(kind: .selectedTrains, trainIDs: ["t1"]))
    #expect(selected == selectedSame)                          // same scope → stable
}
