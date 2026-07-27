import Foundation

/// Whether a manual annotation asserts a pattern (positive) or vetoes an auto interpretation
/// (negative). Polarity is intrinsic to the label, so it is derived rather than stored separately —
/// this makes a `(polarity, label)` mismatch unrepresentable.
public enum ManualAnnotationPolarity: String, Codable, Hashable, Sendable {
    case positive
    case negative
}

/// Provenance for the human identity attached to a manual scientific decision.
///
/// `unknown` is retained for backward-compatible decoding/import, but result-package export rejects
/// it whenever the annotation has authority over a public ISI or a valid spike-only mark.
public enum ManualAnnotationIdentitySource: String, Codable, Hashable, Sendable {
    case userProvided = "user_provided"
    case localAccount = "local_account"
    case imported
    case unknown
}

/// The manual annotation vocabulary the reviewer can author. The positive labels mirror the R/Shiny
/// `pattern_manual` set exactly; the single negative label mirrors R's `pattern_manual_negative`
/// product value `not_burst`.
///
/// Extension seam: future Mac-only positive subtypes (e.g. `irregular_tonic`, `hf_burst`) and future
/// veto labels (e.g. `not_tonic`) can be added here without breaking the JSON/CSV schema. Only the
/// labels in `consumedVetoLabels` are treated as active vetoes by the projector, so adding a new case
/// is inert until the projector is taught to consume it.
public enum ManualAnnotationLabel: String, Codable, Hashable, Sendable, CaseIterable {
    // Positive labels (R `pattern_manual`-compatible).
    case burst
    case longBurst = "long_burst"
    case tonic
    case highFrequencyTonic = "high_frequency_tonic"
    case highFrequencySpiking = "high_frequency_spiking"
    case pause
    case other

    // Negative / veto labels (Phase 1A: `notBurst` only).
    case notBurst = "not_burst"

    public var polarity: ManualAnnotationPolarity {
        Self.consumedVetoLabels.contains(self) ? .negative : .positive
    }

    /// The canonical per-ISI pattern string this label contributes as a FINAL label. Negative/veto
    /// labels never become a final label, so this is `nil` for them.
    public var finalPatternString: String? {
        polarity == .positive ? rawValue : nil
    }

    /// The set of veto labels the projector actively consumes. Phase 1A consumes only `notBurst`;
    /// any other (future or unknown) negative label is inert until explicitly added here.
    public static let consumedVetoLabels: Set<ManualAnnotationLabel> = [.notBurst]

    /// Short human-readable name for pickers / lists.
    public var displayName: String {
        switch self {
        case .burst: return "Burst"
        case .longBurst: return "Long burst"
        case .tonic: return "Tonic"
        case .highFrequencyTonic: return "HF tonic"
        case .highFrequencySpiking: return "HF spiking"
        case .pause: return "Pause"
        case .other: return "Other"
        case .notBurst: return "Not burst (veto)"
        }
    }

    /// Map an auto detector label to the manual POSITIVE label a reviewer would author for it.
    /// Returns nil for non-event labels (`profile`/`reject`) — the caller decides the fallback.
    /// `highFrequencyBurst` and `possibleBurst` map to `burst` (there is no Mac-only `hfBurst`
    /// manual label in Phase 1A, and accepting a possible burst as a manual burst mirrors R).
    public init?(autoLabel: ClassicAnchorLabel) {
        switch autoLabel {
        case .burst: self = .burst
        case .longBurst: self = .longBurst
        case .highFrequencyBurst: self = .burst
        case .possibleBurst: self = .burst
        case .tonic: self = .tonic
        case .highFrequencyTonic: self = .highFrequencyTonic
        case .highFrequencySpiking: self = .highFrequencySpiking
        case .pause: self = .pause
        case .profile, .reject: return nil
        }
    }
}

/// A user-authored manual annotation over a single spike train. Independent of auto-detected
/// candidates: `trainID` + `startSec`/`endSec` are authoritative geometry; the index fields are an
/// optional cache that is recomputed from time whenever a train is available (see
/// `ManualAnnotationGeometryResolver`).
public struct ManualAnnotation: Identifiable, Codable, Hashable, Sendable {
    public var id: UUID
    public var trainID: String
    public var label: ManualAnnotationLabel
    public var startSec: Double
    public var endSec: Double
    public var startISIIndex: Int?
    public var endISIIndex: Int?
    public var startSpikeIndex: Int?
    public var endSpikeIndex: Int?
    public var note: String?
    public var annotator: String?
    public var annotatorIdentitySource: ManualAnnotationIdentitySource?
    public var createdAt: Date
    public var updatedAt: Date

    public init(
        id: UUID = UUID(),
        trainID: String,
        label: ManualAnnotationLabel,
        startSec: Double,
        endSec: Double,
        startISIIndex: Int? = nil,
        endISIIndex: Int? = nil,
        startSpikeIndex: Int? = nil,
        endSpikeIndex: Int? = nil,
        note: String? = nil,
        annotator: String? = nil,
        annotatorIdentitySource: ManualAnnotationIdentitySource? = nil,
        createdAt: Date = Date(),
        updatedAt: Date = Date()
    ) {
        self.id = id
        self.trainID = trainID
        self.label = label
        self.startSec = startSec
        self.endSec = endSec
        self.startISIIndex = startISIIndex
        self.endISIIndex = endISIIndex
        self.startSpikeIndex = startSpikeIndex
        self.endSpikeIndex = endSpikeIndex
        self.note = note
        self.annotator = annotator
        self.annotatorIdentitySource = annotatorIdentitySource
        self.createdAt = createdAt
        self.updatedAt = updatedAt
    }

    /// Derived from the label — never stored, so polarity can never contradict the label.
    public var polarity: ManualAnnotationPolarity { label.polarity }

    /// The time range with start ≤ end (a reversed authored range is normalized here, not mutated).
    public var normalizedStartSec: Double { Swift.min(startSec, endSec) }
    public var normalizedEndSec: Double { Swift.max(startSec, endSec) }
}

/// The stable per-dataset persistence key for manual annotations. Pure and candidate-independent, so
/// a detector rerun never changes it and two different datasets map to different keys — this is the
/// load/store decision that keeps one dataset's annotations from leaking into another. Lives in
/// STPDCore so the isolation rule is unit-testable without the app layer.
public enum ManualAnnotationDatasetKey {
    public static func make(name: String, sourceDescription: String) -> String {
        "\(name)\u{1}\(sourceDescription)"
    }

    public static func make(for dataset: SpikeDataset) -> String {
        make(name: dataset.name, sourceDescription: dataset.sourceDescription)
    }
}
