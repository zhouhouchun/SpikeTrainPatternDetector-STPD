import Foundation

/// A **read-only** per-ISI diagnostic: for an arbitrary inter-spike interval on the raster (especially an
/// unlabeled / "Other" one that visually looks like a missed burst/pause), it explains the ISI's relationship to
/// the train's resolved adaptive burst seed band, and why it did not become a selected burst seed. It is a pure
/// projection of EXISTING detector outputs (the per-train `AdaptiveBand` seed/bridge bounds + whether a public
/// candidate already covers the ISI) — it never re-runs detection, changes selection, or invents thresholds.
///
/// Unavailable fields are explicit: when the detector has not run (no seed band) the diagnostic says so rather
/// than guessing, and the per-train adaptive pause threshold is intentionally `nil` unless the user supplied a
/// manual pause limit (the adaptive pause floor is not surfaced per-train by the pipeline).
public struct PerISIDiagnostic: Hashable, Sendable {
    public let belongsToCandidate: Bool
    public let candidateLabel: String?
    public let seedLowerSec: Double?
    public let seedUpperSec: Double?
    public let bridgeUpperSec: Double?
    /// A short tag for a compact row, e.g. "in candidate", "in seed band", "above seed upper".
    public let bandRelationTag: String
    /// A one-paragraph human explanation suitable for a wrapped hover-card footer.
    public let explanation: String

    public init(
        belongsToCandidate: Bool,
        candidateLabel: String?,
        seedLowerSec: Double?,
        seedUpperSec: Double?,
        bridgeUpperSec: Double?,
        bandRelationTag: String,
        explanation: String
    ) {
        self.belongsToCandidate = belongsToCandidate
        self.candidateLabel = candidateLabel
        self.seedLowerSec = seedLowerSec
        self.seedUpperSec = seedUpperSec
        self.bridgeUpperSec = bridgeUpperSec
        self.bandRelationTag = bandRelationTag
        self.explanation = explanation
    }
}

public enum PerISIDiagnosticBuilder {
    /// Diagnose one ISI against the train's resolved adaptive seed band.
    ///
    /// - Parameters:
    ///   - isiSec: the inter-spike interval (seconds).
    ///   - coveringCandidateLabel: the readable label of a public candidate covering this ISI, or `nil` ("Other").
    ///   - seedLowerSec / seedUpperSec / bridgeUpperSec: the train's resolved adaptive burst band (`nil` if the
    ///     detector has not run).
    ///   - manualPauseLowerSec: the active *manual* pause ISI floor (seconds), or `nil` if none (the adaptive
    ///     per-train pause floor is not exposed and is reported as unavailable).
    public static func diagnose(
        isiSec: Double,
        coveringCandidateLabel: String?,
        seedLowerSec: Double?,
        seedUpperSec: Double?,
        bridgeUpperSec: Double?,
        manualPauseLowerSec: Double?
    ) -> PerISIDiagnostic {
        if let label = coveringCandidateLabel, !label.isEmpty {
            return PerISIDiagnostic(
                belongsToCandidate: true,
                candidateLabel: readable(label),
                seedLowerSec: seedLowerSec,
                seedUpperSec: seedUpperSec,
                bridgeUpperSec: bridgeUpperSec,
                bandRelationTag: "in candidate",
                explanation: "Part of a \(readable(label)) candidate — click to focus it and inspect the full label explanation."
            )
        }

        guard let lower = seedLowerSec, let upper = seedUpperSec,
              lower.isFinite, upper.isFinite, upper > 0 else {
            return PerISIDiagnostic(
                belongsToCandidate: false,
                candidateLabel: nil,
                seedLowerSec: seedLowerSec,
                seedUpperSec: seedUpperSec,
                bridgeUpperSec: bridgeUpperSec,
                bandRelationTag: "no band",
                explanation: "No adaptive burst seed band for this train yet — run the detector to diagnose why this ISI was not selected."
            )
        }

        let band = "\(ms(lower))–\(ms(upper)) ms"
        let tag: String
        var why: String
        if isiSec < lower {
            tag = "below seed lower"
            why = "This ISI (\(ms(isiSec)) ms) is shorter than the burst seed band \(band) — faster than the burst-core ISIs here, so it is not a burst seed."
        } else if isiSec <= upper {
            tag = "in seed band"
            why = "This ISI (\(ms(isiSec)) ms) falls inside the burst seed band \(band), but this interval is not part of a selected burst — it lacks enough contiguous seed ISIs / spikes to form one."
        } else if let bridge = bridgeUpperSec, bridge.isFinite, isiSec <= bridge {
            tag = "within bridge"
            why = "This ISI (\(ms(isiSec)) ms) is above the seed upper (\(band)) but within the bridge tolerance (\(ms(bridge)) ms) — it was not bridged into a burst here."
        } else {
            tag = "above seed upper"
            let bridgePart = (bridgeUpperSec?.isFinite == true) ? " / bridge \(ms(bridgeUpperSec!)) ms" : ""
            why = "This ISI (\(ms(isiSec)) ms) is above the burst seed band \(band)\(bridgePart) — the gap is too long to be a burst-core ISI."
        }

        if let pause = manualPauseLowerSec, pause.isFinite, pause > 0 {
            if isiSec >= pause {
                why += " It is at/above the manual pause threshold (\(ms(pause)) ms); if it is not labeled a pause, state/precedence took priority."
            } else {
                why += " It is below the manual pause threshold (\(ms(pause)) ms), so it is not a pause."
            }
        }

        return PerISIDiagnostic(
            belongsToCandidate: false,
            candidateLabel: nil,
            seedLowerSec: lower,
            seedUpperSec: upper,
            bridgeUpperSec: bridgeUpperSec,
            bandRelationTag: tag,
            explanation: why
        )
    }

    /// Readable seed-band text (`lower–upper ms`), or `nil` when no band is available.
    public static func seedBandText(lowerSec: Double?, upperSec: Double?) -> String? {
        guard let lower = lowerSec, let upper = upperSec, lower.isFinite, upper.isFinite, upper > 0 else {
            return nil
        }
        return "\(ms(lower))–\(ms(upper)) ms"
    }

    private static func ms(_ seconds: Double) -> String {
        guard seconds.isFinite else { return "—" }
        return String(format: "%.3g", seconds * 1000)
    }

    private static func readable(_ token: String) -> String {
        token.replacingOccurrences(of: "_", with: " ")
    }
}
