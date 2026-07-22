import Foundation

/// A **read-only** per-ISI diagnostic: for an arbitrary inter-spike interval on the raster (especially an
/// unlabeled / "Other" one that visually looks like a missed burst/pause), it reports the ISI's relationship to
/// the train's resolved adaptive burst seed band and its observed candidate membership. It is a pure
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
        guard isiSec.isFinite, isiSec >= 0 else {
            return PerISIDiagnostic(
                belongsToCandidate: false,
                candidateLabel: nil,
                seedLowerSec: nil,
                seedUpperSec: nil,
                bridgeUpperSec: nil,
                bandRelationTag: "invalid ISI",
                explanation: "This ISI is non-finite or negative, so no detector-band comparison is valid."
            )
        }

        let band = validBand(lowerSec: seedLowerSec, upperSec: seedUpperSec)
        let bridge = band.flatMap { bounds -> Double? in
            guard let value = bridgeUpperSec, value.isFinite, value >= bounds.upper else { return nil }
            return value
        }
        let pause = manualPauseLowerSec.flatMap { value in
            value.isFinite && value > 0 ? value : nil
        }

        if let label = coveringCandidateLabel, !label.isEmpty {
            return PerISIDiagnostic(
                belongsToCandidate: true,
                candidateLabel: readable(label),
                seedLowerSec: band?.lower,
                seedUpperSec: band?.upper,
                bridgeUpperSec: bridge,
                bandRelationTag: "in candidate",
                explanation: "Part of a \(readable(label)) candidate — click to focus it and inspect the full label explanation."
            )
        }

        guard let (lower, upper) = band else {
            return PerISIDiagnostic(
                belongsToCandidate: false,
                candidateLabel: nil,
                seedLowerSec: nil,
                seedUpperSec: nil,
                bridgeUpperSec: nil,
                bandRelationTag: "no band",
                explanation: "No adaptive burst seed band is available for this train yet — run the detector to populate the band comparison."
            )
        }

        let bandText = "\(ms(lower))–\(ms(upper)) ms"
        let tag: String
        var why: String
        if isiSec < lower {
            tag = "below seed lower"
            why = "This ISI (\(ms(isiSec)) ms) is below the burst seed band \(bandText) and is not part of a selected burst. This comparison alone does not identify which detector gate excluded it."
        } else if isiSec <= upper {
            tag = "in seed band"
            why = "This ISI (\(ms(isiSec)) ms) falls inside the burst seed band \(bandText), but it is not part of a selected burst. Possible explanations include run length, boundary evidence, arbitration, or selection; this diagnostic does not carry the causal provenance needed to choose among them."
        } else if let bridge, isiSec <= bridge {
            tag = "within bridge"
            why = "This ISI (\(ms(isiSec)) ms) is above the seed upper (\(bandText)) but within the bridge tolerance (\(ms(bridge)) ms), and it is not part of a selected burst. The available inputs do not establish whether bridging, boundary evidence, or later arbitration excluded it."
        } else {
            tag = "above seed upper"
            let bridgePart = bridge.map { " / bridge \(ms($0)) ms" } ?? ""
            why = "This ISI (\(ms(isiSec)) ms) is above the burst seed band \(bandText)\(bridgePart) and is not part of a selected burst. This magnitude comparison is observational and does not by itself prove the detector's causal rejection path."
        }

        if let pause {
            if isiSec >= pause {
                why += " It is at/above the configured manual pause threshold (\(ms(pause)) ms); that comparison alone does not establish why the final label is not pause."
            } else {
                why += " It is below the configured manual pause threshold (\(ms(pause)) ms); that comparison alone does not establish that it is not a pause."
            }
        }

        return PerISIDiagnostic(
            belongsToCandidate: false,
            candidateLabel: nil,
            seedLowerSec: lower,
            seedUpperSec: upper,
            bridgeUpperSec: bridge,
            bandRelationTag: tag,
            explanation: why
        )
    }

    /// Readable seed-band text (`lower–upper ms`), or `nil` when no band is available.
    public static func seedBandText(lowerSec: Double?, upperSec: Double?) -> String? {
        guard let (lower, upper) = validBand(lowerSec: lowerSec, upperSec: upperSec) else { return nil }
        return "\(ms(lower))–\(ms(upper)) ms"
    }

    private static func validBand(lowerSec: Double?, upperSec: Double?) -> (lower: Double, upper: Double)? {
        guard let lower = lowerSec, let upper = upperSec,
              lower.isFinite, upper.isFinite, lower >= 0, upper > 0, lower <= upper else {
            return nil
        }
        return (lower, upper)
    }

    private static func ms(_ seconds: Double) -> String {
        guard seconds.isFinite else { return "—" }
        return String(format: "%.3g", seconds * 1000)
    }

    private static func readable(_ token: String) -> String {
        token.replacingOccurrences(of: "_", with: " ")
    }
}
