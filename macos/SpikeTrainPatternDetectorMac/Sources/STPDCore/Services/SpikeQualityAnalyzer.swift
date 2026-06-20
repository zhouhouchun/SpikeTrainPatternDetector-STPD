import Foundation

public enum SpikeQualityAnalyzer {
    public static func analyze(
        dataset: SpikeDataset,
        settings: SpikeQualitySettings = SpikeQualitySettings()
    ) -> SpikeDatasetQualityReport {
        let rows = dataset.trains.map { train in
            quality(for: train, settings: settings)
        }

        let artifactRows = dataset.trains.flatMap { train in
            artifactDetails(for: train, settings: settings)
        }

        let duplicateRows = dataset.trains.flatMap { train in
            duplicateDetails(for: train)
        }

        return SpikeDatasetQualityReport(
            rows: rows,
            artifactDetails: artifactRows,
            duplicateDetails: duplicateRows
        )
    }

    public static func quality(
        for train: SpikeTrain,
        settings: SpikeQualitySettings = SpikeQualitySettings()
    ) -> SpikeTrainQuality {
        let timestamps = train.timestampsSec
        let isiValues = train.isiSec.map { $0 }
        let finiteISI = isiValues.compactMap { $0 }.filter(\.isFinite)
        let finiteISIExcludingFirst = isiValues.dropFirst().compactMap { $0 }.filter(\.isFinite)

        let artifactFlags = isiValues.enumerated().map { index, value in
            index > 0 && isArtifactISI(value, threshold: settings.artifactThresholdSec)
        }

        let refractoryFlags = isiValues.enumerated().map { index, value in
            index > 0 && isRefractorySuspectISI(
                value,
                artifactThreshold: settings.artifactThresholdSec,
                refractoryThreshold: settings.refractorySuspectThresholdSec
            )
        }

        let validISI = isiValues.enumerated().compactMap { index, value -> Double? in
            guard index > 0, let value, value.isFinite, value >= settings.artifactThresholdSec else {
                return nil
            }
            return value
        }

        let duration = durationSec(for: train)
        let isiRate = duration.flatMap { duration -> Double? in
            guard duration > 0, timestamps.count >= 2 else {
                return nil
            }
            return Double(timestamps.count - 1) / duration
        }

        let timestampDiffs = zip(timestamps, timestamps.dropFirst()).map { $1 - $0 }
        let duplicateTimestampCount = timestampDiffs.filter { $0.isFinite && $0 == 0 }.count
        let zeroOrNegativeTimestampStepCount = timestampDiffs.filter { $0.isFinite && $0 <= 0 }.count
        let zeroOrNegativeISICount = isiValues.enumerated().filter { index, value in
            guard index > 0, let value, value.isFinite else {
                return false
            }
            return value <= 0
        }.count

        let artifactCount = artifactFlags.filter { $0 }.count
        let refractoryCount = refractoryFlags.filter { $0 }.count
        let denominator = max(timestamps.count - 1, 1)
        let artifactFraction = timestamps.count >= 2 ? Double(artifactCount) / Double(denominator) : nil
        let refractoryFraction = timestamps.count >= 2 ? Double(refractoryCount) / Double(denominator) : nil
        let percentileStatus = percentileStatus(validISICount: validISI.count)

        var level: QualityWarningLevel = .ok
        let hardDuplicatePolicy = train.duplicateTimestampPolicy == .errorKeep
        let duplicateIntegrityProblem = zeroOrNegativeTimestampStepCount > 0 || duplicateTimestampCount > 0
        let nonDuplicateNonpositiveISICount = max(0, zeroOrNegativeISICount - duplicateTimestampCount)

        if duration == nil || duration ?? 0 <= 0 {
            level = .error
        }
        if level == .ok && (nonDuplicateNonpositiveISICount > 0 || (duplicateIntegrityProblem && hardDuplicatePolicy)) {
            level = .error
        }
        if level == .ok && (
            duplicateIntegrityProblem ||
            train.inputWasUnsorted ||
            train.inputDuplicateTimestampStepCount > 0 ||
            train.inputZeroOrNegativeStepCount > 0 ||
            train.droppedDuplicateTimestampCount > 0 ||
            artifactCount > 0 ||
            refractoryCount > 0 ||
            validISI.count < 50
        ) {
            level = .warning
        }
        if level == .ok, let artifactFraction, artifactFraction > 0.05 {
            level = .warning
        }
        if level == .ok, let isiRate, isiRate > 200 {
            level = .warning
        }

        let messages = warningMessages(
            train: train,
            duration: duration,
            duplicateTimestampCount: duplicateTimestampCount,
            zeroOrNegativeISICount: zeroOrNegativeISICount,
            zeroOrNegativeTimestampStepCount: zeroOrNegativeTimestampStepCount,
            artifactCount: artifactCount,
            refractoryCount: refractoryCount,
            artifactPreview: finiteValues(for: isiValues, flags: artifactFlags),
            refractoryPreview: finiteValues(for: isiValues, flags: refractoryFlags),
            artifactFraction: artifactFraction,
            isiRate: isiRate,
            validISICount: validISI.count,
            percentileStatus: percentileStatus,
            settings: settings
        )

        return SpikeTrainQuality(
            trainName: train.name,
            warningLevel: level,
            warningMessage: messages.joined(separator: "; "),
            spikeCount: timestamps.count,
            firstSpikeSec: timestamps.first,
            lastSpikeSec: timestamps.last,
            durationSec: duration,
            firingRateHz: isiRate,
            rawMinISISec: finiteISIExcludingFirst.min(),
            minValidISISec: validISI.min(),
            artifactMinISISec: finiteValues(for: isiValues, flags: artifactFlags).min(),
            medianISISec: median(validISI),
            maxISISec: finiteISI.max(),
            duplicateTimestampCount: duplicateTimestampCount,
            zeroOrNegativeISICount: zeroOrNegativeISICount,
            zeroOrNegativeTimestampStepCount: zeroOrNegativeTimestampStepCount,
            inputWasUnsorted: train.inputWasUnsorted,
            inputNonmonotonicStepCount: train.inputNonmonotonicStepCount,
            inputDuplicateTimestampStepCount: train.inputDuplicateTimestampStepCount,
            inputZeroOrNegativeStepCount: train.inputZeroOrNegativeStepCount,
            droppedDuplicateTimestampCount: train.droppedDuplicateTimestampCount,
            duplicateTimestampPolicy: train.duplicateTimestampPolicy,
            artifactISICount: artifactCount,
            artifactFraction: artifactFraction,
            refractorySuspectISICount: refractoryCount,
            refractorySuspectFraction: refractoryFraction,
            validISICount: validISI.count,
            percentileStatus: percentileStatus
        )
    }

    private static func durationSec(for train: SpikeTrain) -> Double? {
        guard let first = train.timestampsSec.first, let last = train.timestampsSec.last else {
            return nil
        }

        let duration = last - first
        guard duration.isFinite, duration > 0 else {
            return nil
        }
        return duration
    }

    private static func artifactDetails(
        for train: SpikeTrain,
        settings: SpikeQualitySettings
    ) -> [ArtifactISIDetail] {
        train.isiSec.enumerated().compactMap { index, value in
            guard index > 0, let value, value.isFinite, isArtifactISI(value, threshold: settings.artifactThresholdSec) else {
                return nil
            }

            return ArtifactISIDetail(
                trainName: train.name,
                isiIndex: index + 1,
                leftSpikeTimeSec: train.timestampsSec.indices.contains(index - 1) ? train.timestampsSec[index - 1] : nil,
                rightSpikeTimeSec: train.timestampsSec[index],
                isiSec: value,
                thresholdSec: settings.artifactThresholdSec
            )
        }
    }

    private static func duplicateDetails(for train: SpikeTrain) -> [DuplicateTimestampDetail] {
        var groups: [Double: [(sortedIndex: Int, inputIndex: Int)]] = [:]

        for index in train.timestampsSec.indices {
            let timestamp = train.timestampsSec[index]
            let inputIndex = train.inputOrderIndices.indices.contains(index) ? train.inputOrderIndices[index] : index + 1
            groups[timestamp, default: []].append((index + 1, inputIndex))
        }

        return groups
            .filter { $0.value.count > 1 }
            .map { timestamp, rows in
                DuplicateTimestampDetail(
                    trainName: train.name,
                    timestampSec: timestamp,
                    duplicateCount: rows.count,
                    sortedRowIndices: rows.map(\.sortedIndex),
                    inputOrderIndices: rows.map(\.inputIndex),
                    policy: train.duplicateTimestampPolicy
                )
            }
            .sorted {
                if $0.trainName == $1.trainName {
                    return $0.timestampSec < $1.timestampSec
                }
                return $0.trainName < $1.trainName
            }
    }

    private static func isArtifactISI(_ value: Double?, threshold: Double) -> Bool {
        guard let value, value.isFinite else {
            return false
        }

        let tolerance = max(1e-12, abs(threshold) * 1e-6)
        return value < threshold - tolerance
    }

    private static func isRefractorySuspectISI(
        _ value: Double?,
        artifactThreshold: Double,
        refractoryThreshold: Double
    ) -> Bool {
        guard refractoryThreshold > artifactThreshold, let value, value.isFinite else {
            return false
        }

        let minTolerance = max(1e-12, abs(artifactThreshold) * 1e-6)
        let refractoryTolerance = max(1e-12, abs(refractoryThreshold) * 1e-6)
        return value >= artifactThreshold - minTolerance && value < refractoryThreshold - refractoryTolerance
    }

    private static func percentileStatus(validISICount: Int) -> String {
        if validISICount < 30 {
            return "disabled_lt30"
        }
        if validISICount < 50 {
            return "weak_lt50"
        }
        return "reliable"
    }

    private static func median(_ values: [Double]) -> Double? {
        guard !values.isEmpty else {
            return nil
        }

        let sorted = values.sorted()
        let middle = sorted.count / 2
        if sorted.count.isMultiple(of: 2) {
            return (sorted[middle - 1] + sorted[middle]) / 2
        }
        return sorted[middle]
    }

    private static func finiteValues(for values: [Double?], flags: [Bool]) -> [Double] {
        zip(values, flags).compactMap { value, flag in
            guard flag, let value, value.isFinite else {
                return nil
            }
            return value
        }
    }

    private static func warningMessages(
        train: SpikeTrain,
        duration: Double?,
        duplicateTimestampCount: Int,
        zeroOrNegativeISICount: Int,
        zeroOrNegativeTimestampStepCount: Int,
        artifactCount: Int,
        refractoryCount: Int,
        artifactPreview: [Double],
        refractoryPreview: [Double],
        artifactFraction: Double?,
        isiRate: Double?,
        validISICount: Int,
        percentileStatus: String,
        settings: SpikeQualitySettings
    ) -> [String] {
        var messages: [String] = []

        if duration == nil {
            messages.append("Invalid duration: fewer than two distinct timestamps remain for this train.")
        }
        if duplicateTimestampCount > 0 {
            messages.append("Duplicate timestamps retained: \(duplicateTimestampCount) zero-interval step(s). Policy: \(train.duplicateTimestampPolicy.title).")
        }
        let nonDuplicateNonpositiveISICount = max(0, zeroOrNegativeISICount - duplicateTimestampCount)
        if nonDuplicateNonpositiveISICount > 0 {
            messages.append("Non-positive ISI after sorting: \(nonDuplicateNonpositiveISICount) interval(s).")
        }
        let nonDuplicateTimestampStepCount = max(0, zeroOrNegativeTimestampStepCount - duplicateTimestampCount)
        if nonDuplicateTimestampStepCount > 0 && nonDuplicateTimestampStepCount != nonDuplicateNonpositiveISICount {
            messages.append("Non-increasing timestamp step(s) after sorting: \(nonDuplicateTimestampStepCount).")
        }
        if train.droppedDuplicateTimestampCount > 0 {
            var text = "Exact duplicate timestamps collapsed: \(train.droppedDuplicateTimestampCount) spike(s) removed from this train."
            let inputProblemCount = max(train.inputDuplicateTimestampStepCount, train.inputZeroOrNegativeStepCount)
            if inputProblemCount > 0 {
                text += " Original input had \(inputProblemCount) adjacent duplicate or non-increasing timestamp step(s)."
            }
            text += " This remains a warning so the cleanup is visible in the QC audit trail."
            messages.append(text)
        }
        if artifactCount > 0 {
            let preview = previewText(artifactPreview, displayUnit: settings.displayUnit)
            messages.append("Artifact ISI: \(artifactCount) interval(s) below the min valid ISI threshold \(format(settings.artifactThresholdSec, displayUnit: settings.displayUnit)) \(settings.displayUnit.rawValue). Examples: [\(preview)] \(settings.displayUnit.rawValue).")
        }
        if refractoryCount > 0 {
            let preview = previewText(refractoryPreview, displayUnit: settings.displayUnit)
            messages.append("Refractory-suspect ISI: \(refractoryCount) interval(s) between the min valid ISI and refractory threshold \(format(settings.refractorySuspectThresholdSec, displayUnit: settings.displayUnit)) \(settings.displayUnit.rawValue). Examples: [\(preview)] \(settings.displayUnit.rawValue).")
        }
        if train.inputWasUnsorted {
            messages.append("Input order was unsorted: \(train.inputNonmonotonicStepCount) decreasing step(s). Timestamps were sorted before QC and detection.")
        }
        if train.droppedDuplicateTimestampCount == 0 &&
            train.inputDuplicateTimestampStepCount > 0 &&
            train.inputDuplicateTimestampStepCount != duplicateTimestampCount {
            messages.append("Original input order had \(train.inputDuplicateTimestampStepCount) adjacent duplicate timestamp step(s).")
        }
        if train.droppedDuplicateTimestampCount == 0 &&
            train.inputZeroOrNegativeStepCount > 0 &&
            train.inputZeroOrNegativeStepCount != max(zeroOrNegativeTimestampStepCount, zeroOrNegativeISICount) {
            messages.append("Original input order had \(train.inputZeroOrNegativeStepCount) zero or negative timestamp step(s).")
        }
        if let artifactFraction, artifactFraction > 0.05 {
            messages.append("Artifact fraction is high: \(format(artifactFraction)) of ISI intervals are below the artifact threshold.")
        }
        if validISICount < 30 {
            messages.append("Percentile-based QC is disabled because only \(validISICount) valid ISI interval(s) remain; at least 30 are needed.")
        } else if validISICount < 50 {
            messages.append("Percentile-based QC is weak because only \(validISICount) valid ISI interval(s) remain; 50 or more is preferred.")
        }
        if let isiRate, isiRate > 200 {
            messages.append("Very high firing rate estimate: \(format(isiRate)) Hz.")
        }

        return messages
    }

    private static func previewText(
        _ values: [Double],
        displayUnit: QualityDisplayUnit,
        maxCount: Int = 12
    ) -> String {
        let head = values.prefix(maxCount).map {
            format($0, displayUnit: displayUnit)
        }
        let more = values.count > maxCount ? " +\(values.count - maxCount) more" : ""
        return head.joined(separator: ", ") + more
    }

    private static func format(_ value: Double, displayUnit: QualityDisplayUnit) -> String {
        format(value * displayUnit.scaleFromSeconds)
    }

    private static func format(_ value: Double) -> String {
        guard value.isFinite else {
            return ""
        }

        var text = String(format: "%.6f", value)
        while text.contains(".") && text.last == "0" {
            text.removeLast()
        }
        if text.last == "." {
            text.removeLast()
        }
        return text == "-0" ? "0" : text
    }
}
