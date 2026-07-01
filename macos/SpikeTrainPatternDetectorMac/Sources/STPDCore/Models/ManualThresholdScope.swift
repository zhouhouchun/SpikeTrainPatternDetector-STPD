import Foundation

/// P8 — the user's SCOPE intent for manual HARD thresholds: which spike trains a manual hard numeric threshold should
/// target. The default `.allTrains` reproduces today's global behavior. This is a pure value model of INTENT — P8 wires
/// it into document state, the detection-input signature (so changing scope marks results stale), and the UI; it does
/// NOT change detector math (how thresholds are applied per train) yet.
///
/// Naming note: distinct from the unrelated `SpikeTrainSelectionScope`, which is a display/navigation page concept.
public enum ManualThresholdScopeKind: String, Codable, Hashable, Sendable, CaseIterable, Identifiable {
    case allTrains       // default — every train (today's global manual-threshold behavior)
    case currentTrain    // the focused / current train only
    case selectedTrains  // the selected visible trains only

    public var id: String { rawValue }

    /// The default scope (= today's global behavior). Mirrors `ManualThresholdFields.automaticDefaults` as the
    /// reset/default contract used by the document.
    public static let `default`: ManualThresholdScopeKind = .allTrains

    /// The scope KIND after a dataset install. Scope is tied to the manual thresholds: it resets to `.allTrains`
    /// exactly when the manual thresholds are reset (a fresh dataset load), and is preserved otherwise (a same-dataset
    /// reinstall, e.g. a duplicate-timestamp policy change, which keeps the user's thresholds). Pure and testable; the
    /// document applies this same rule by resetting scope only inside its manual-threshold reset.
    public func afterDatasetInstall(manualThresholdsReset: Bool) -> ManualThresholdScopeKind {
        manualThresholdsReset ? .allTrains : self
    }
}

/// A RESOLVED manual-threshold scope: the kind plus the concrete set of train IDs it targets. This is the value placed
/// in `DetectionInputsSignature`, so a change of kind OR of the resolved train set marks detection stale.
public struct ManualThresholdScope: Equatable, Sendable, Codable {
    public let kind: ManualThresholdScopeKind
    /// The resolved target train IDs (sorted for stable equality). Empty for `.allTrains` (which means "every train",
    /// not "no train").
    public let trainIDs: [String]

    /// The default (global) scope.
    public static let allTrains = ManualThresholdScope(kind: .allTrains, trainIDs: [])

    public init(kind: ManualThresholdScopeKind, trainIDs: [String]) {
        self.kind = kind
        // `.allTrains` carries no explicit ids (it covers everything); other kinds keep a stable, de-duplicated set.
        self.trainIDs = kind == .allTrains ? [] : Array(Set(trainIDs)).sorted()
    }

    /// Pure resolution from the UI selection state. SAFETY (P8): a current/selected scope that resolves to no train
    /// yields an EMPTY target set — it never silently falls back to "all trains".
    public static func resolve(
        kind: ManualThresholdScopeKind,
        focusedTrainID: String?,
        selectedTrainIDs: Set<String>,
        allTrainIDs: [String]
    ) -> ManualThresholdScope {
        switch kind {
        case .allTrains:
            return .allTrains
        case .currentTrain:
            if let focusedTrainID, allTrainIDs.contains(focusedTrainID) {
                return ManualThresholdScope(kind: .currentTrain, trainIDs: [focusedTrainID])
            }
            return ManualThresholdScope(kind: .currentTrain, trainIDs: [])
        case .selectedTrains:
            return ManualThresholdScope(kind: .selectedTrains, trainIDs: allTrainIDs.filter(selectedTrainIDs.contains))
        }
    }

    /// Whether the scope targets `trainID`. `.allTrains` targets every train; other kinds target only the resolved set.
    public func appliesTo(trainID: String) -> Bool {
        switch kind {
        case .allTrains: return true
        case .currentTrain, .selectedTrains: return trainIDs.contains(trainID)
        }
    }

    /// True when the scope resolves to no train at all (a `.currentTrain` / `.selectedTrains` scope with nothing
    /// focused/selected) — the UI uses this to warn that a hard gate would currently target nothing.
    public var targetsNoTrain: Bool {
        kind != .allTrains && trainIDs.isEmpty
    }

    /// The number of trains the scope resolves to, for the UI count indicator. `.allTrains` reports the full count.
    public func resolvedCount(allTrainCount: Int) -> Int {
        kind == .allTrains ? allTrainCount : trainIDs.count
    }

    /// P10: the stable, machine-readable decision-path note recorded when a manual HARD gate is applied to an in-scope
    /// train (`trainID`). It is appended ONLY for trains a hard gate actually reached, so its mere presence is the audit
    /// signal that the scope let a hard gate through. The raw token is never localized.
    ///   `.allTrains`       → `manual_threshold_scope=all_trains`
    ///   `.currentTrain`    → `manual_threshold_scope=current_train(train_id=<trainID>)`
    ///   `.selectedTrains`  → `manual_threshold_scope=selected_trains(count=<n>,train_id=<trainID>)`
    public func hardGateProvenanceNote(trainID: String) -> String {
        switch kind {
        case .allTrains:
            return "manual_threshold_scope=all_trains"
        case .currentTrain:
            return "manual_threshold_scope=current_train(train_id=\(trainID))"
        case .selectedTrains:
            return "manual_threshold_scope=selected_trains(count=\(trainIDs.count),train_id=\(trainID))"
        }
    }
}

/// P10 — pure, display-only reader of the `manual_threshold_scope=...` decision-path note (written by
/// `ManualThresholdScope.hardGateProvenanceNote`). It classifies which scope let a hard gate through, and renders a
/// localized one-line label for the candidate inspector. The raw machine token is never localized. A candidate with no
/// scope note (out-of-scope / soft-only / automatic) reads as `.none` and renders nothing.
public struct ManualThresholdScopeProvenance: Equatable, Sendable {
    public enum Kind: String, Sendable, Equatable {
        case none, allTrains, currentTrain, selectedTrains
    }

    public let kind: Kind
    public var isPresent: Bool { kind != .none }

    public init(kind: Kind) { self.kind = kind }
    public static let absent = ManualThresholdScopeProvenance(kind: .none)

    public static func parse(decisionPath: String) -> ManualThresholdScopeProvenance {
        if decisionPath.contains("manual_threshold_scope=selected_trains") { return .init(kind: .selectedTrains) }
        if decisionPath.contains("manual_threshold_scope=current_train") { return .init(kind: .currentTrain) }
        if decisionPath.contains("manual_threshold_scope=all_trains") { return .init(kind: .allTrains) }
        return .absent
    }

    /// A localized "Hard gate applied by scope: <scope>" line, or nil when no scope note is present.
    public func label(language: STPDLanguage) -> String? {
        let scopeName: String
        switch kind {
        case .none: return nil
        case .allTrains: scopeName = STPDLocalization.text("全部序列", language: language)
        case .currentTrain: scopeName = STPDLocalization.text("当前序列", language: language)
        case .selectedTrains: scopeName = STPDLocalization.text("所选序列", language: language)
        }
        let prefix = STPDLocalization.text("按范围应用硬门控", language: language)
        return language == .zh ? "\(prefix)：\(scopeName)" : "\(prefix): \(scopeName)"
    }
}
