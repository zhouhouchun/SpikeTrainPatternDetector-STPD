import Foundation

public enum NeuroExplorerNEXError: Error, Equatable, Sendable, LocalizedError {
    case sourceTooShort
    case invalidMagicNumber
    case unsupportedVersion(Int32)
    case invalidTimestampFrequency
    case invalidVariableCount
    case truncatedVariableHeader(index: Int)
    case unsupportedVariableType(type: Int32, name: String)
    case invalidVariableGeometry(name: String)
    case timestampOutsideClassicNEXRange(trainID: String, microseconds: Int64)
    case timestampIsNotWholeMicrosecond(trainID: String, seconds: Double)
    case markerValueTooLarge(trainID: String)
    case outputTooLarge
    case noSpikeVariables

    public var errorDescription: String? {
        switch self {
        case .sourceTooShort:
            return "NEX 文件不完整。"
        case .invalidMagicNumber:
            return "所选文件不是有效的 NeuroExplorer .nex 文件。"
        case .unsupportedVersion(let version):
            return "暂不支持 NEX 文件版本 \(version)。"
        case .invalidTimestampFrequency:
            return "NEX 时间戳频率无效。"
        case .invalidVariableCount:
            return "NEX 变量数量无效。"
        case .truncatedVariableHeader(let index):
            return "NEX 第 \(index) 个变量头不完整。"
        case .unsupportedVariableType(let type, let name):
            return "NEX 变量 \(name) 使用了不支持的类型 \(type)。"
        case .invalidVariableGeometry(let name):
            return "NEX 变量 \(name) 的数据范围无效或文件已截断。"
        case .timestampOutsideClassicNEXRange(let trainID, let microseconds):
            return "\(trainID) 的时间戳 \(microseconds) µs 超出经典 .nex 的 32 位范围；请改用 CSV 或 XLSX。"
        case .timestampIsNotWholeMicrosecond(let trainID, let seconds):
            return "\(trainID) 的时间戳 \(seconds) s 不能无损表示为整数微秒；请改用 CSV 或 XLSX。"
        case .markerValueTooLarge(let trainID):
            return "\(trainID) 的标记备注过长，无法写入经典 .nex marker。"
        case .outputTooLarge:
            return "生成的 .nex 文件超过经典格式的 32 位偏移范围。"
        case .noSpikeVariables:
            return "NEX 文件中没有可作为 spike train 读取的 Neuron 或 Waveform 变量。"
        }
    }
}

public struct NeuroExplorerNEXImportedTrain: Hashable, Sendable {
    public let name: String
    public let timestampsSeconds: [Double]
}

public struct NeuroExplorerNEXImportedEvent: Hashable, Sendable {
    public let name: String
    public let timestampSeconds: Double
    public let sourceVariableType: String
}

public struct NeuroExplorerNEXReadResult: Hashable, Sendable {
    public let trains: [NeuroExplorerNEXImportedTrain]
    public let events: [NeuroExplorerNEXImportedEvent]
    public let ignoredVariableNames: [String]
    public let timestampFrequencyHz: Double
}

public enum NeuroExplorerNEXDatasetAdapter {
    /// Builds the existing non-authoritative browsing/manual-annotation surface. Event-like NEX
    /// variables remain TaskEvents and therefore never participate in spike detection or ISI QC.
    public static func dataset(
        from result: NeuroExplorerNEXReadResult,
        name: String,
        sourceDescription: String,
        duplicateTimestampPolicy: DuplicateTimestampPolicy
    ) -> SpikeDataset {
        let trains = result.trains.map {
            SpikeTrain(
                name: $0.name,
                timestampsSec: $0.timestampsSeconds,
                duplicateTimestampPolicy: duplicateTimestampPolicy
            )
        }
        let events = result.events.enumerated().map { offset, event in
            TaskEvent(
                id: "nex:\(offset + 1)",
                name: event.name,
                timeSec: event.timestampSeconds,
                column: event.name,
                eventIndex: offset + 1,
                trialID: "nex_import",
                source: "\(sourceDescription) · \(event.sourceVariableType)"
            )
        }
        return SpikeDataset(
            name: name,
            sourceDescription: sourceDescription,
            trains: trains,
            taskEvents: events
        )
    }
}

/// Bounded in-memory reader/writer for the classic NeuroExplorer `.nex` format.
///
/// The implementation follows the published 544-byte file header, 208-byte variable header and
/// little-endian 32-bit timestamp layout. Import treats Neuron and Waveform variables as spike
/// trains; Event, Marker and Interval boundaries enter the separate event layer. Continuous and
/// population-vector variables are reported as ignored and never become spikes.
public enum NeuroExplorerNEXCodec {
    public static let timestampFrequencyHz = 1_000_000.0
    private static let magicNumber: Int32 = 827_868_494
    private static let fileVersion: Int32 = 106
    private static let fileHeaderSize = 544
    private static let variableHeaderSize = 208
    private static let maximumMarkerValueBytes = 4_096

    public static func read(_ data: Data) throws -> NeuroExplorerNEXReadResult {
        guard data.count >= fileHeaderSize else { throw NeuroExplorerNEXError.sourceTooShort }
        let reader = BinaryReader(data)
        guard try reader.int32(at: 0) == magicNumber else {
            throw NeuroExplorerNEXError.invalidMagicNumber
        }
        let version = try reader.int32(at: 4)
        guard version >= 100 && version <= 106 else {
            throw NeuroExplorerNEXError.unsupportedVersion(version)
        }
        let frequency = try reader.double(at: 264)
        guard frequency.isFinite && frequency > 0 else {
            throw NeuroExplorerNEXError.invalidTimestampFrequency
        }
        let variableCount32 = try reader.int32(at: 280)
        guard variableCount32 >= 0 else { throw NeuroExplorerNEXError.invalidVariableCount }
        let variableCount = Int(variableCount32)
        let (headersBytes, headerOverflow) = variableCount.multipliedReportingOverflow(
            by: variableHeaderSize
        )
        let (dataStart, startOverflow) = fileHeaderSize.addingReportingOverflow(headersBytes)
        guard !headerOverflow, !startOverflow, dataStart <= data.count else {
            throw NeuroExplorerNEXError.invalidVariableCount
        }

        var trains: [NeuroExplorerNEXImportedTrain] = []
        var events: [NeuroExplorerNEXImportedEvent] = []
        var ignored: [String] = []
        var importedTrainNameCounts: [String: Int] = [:]
        for index in 0..<variableCount {
            let offset = fileHeaderSize + index * variableHeaderSize
            guard offset + variableHeaderSize <= data.count else {
                throw NeuroExplorerNEXError.truncatedVariableHeader(index: index + 1)
            }
            let type = try reader.int32(at: offset)
            let name = try reader.fixedString(at: offset + 8, length: 64)
            let dataOffset32 = try reader.int32(at: offset + 72)
            let count32 = try reader.int32(at: offset + 76)
            guard dataOffset32 >= 0, count32 >= 0 else {
                throw NeuroExplorerNEXError.invalidVariableGeometry(name: name)
            }
            let dataOffset = Int(dataOffset32)
            let count = Int(count32)
            guard dataOffset >= dataStart else {
                throw NeuroExplorerNEXError.invalidVariableGeometry(name: name)
            }

            switch type {
            case 0, 3: // Neuron / Waveform. Waveform timestamps precede waveform samples.
                let ticks = try readTicks(
                    reader: reader,
                    offset: dataOffset,
                    count: count,
                    variableName: name
                )
                let baseName = name.isEmpty ? "NEX spike \(index + 1)" : name
                let occurrence = importedTrainNameCounts[baseName, default: 0] + 1
                importedTrainNameCounts[baseName] = occurrence
                let uniqueName = occurrence == 1 ? baseName : "\(baseName) [\(occurrence)]"
                trains.append(NeuroExplorerNEXImportedTrain(
                    name: uniqueName,
                    timestampsSeconds: ticks.map { Double($0) / frequency }
                ))
            case 1: // Event
                let ticks = try readTicks(
                    reader: reader,
                    offset: dataOffset,
                    count: count,
                    variableName: name
                )
                appendEvents(
                    ticks: ticks,
                    frequency: frequency,
                    name: name,
                    type: "event",
                    to: &events
                )
            case 2: // Interval: starts followed by ends.
                let (pairedCount, pairedOverflow) = count.multipliedReportingOverflow(by: 2)
                guard !pairedOverflow else {
                    throw NeuroExplorerNEXError.invalidVariableGeometry(name: name)
                }
                let byteCount = try checkedByteCount(count: pairedCount, name: name)
                guard dataOffset <= data.count, byteCount <= data.count - dataOffset else {
                    throw NeuroExplorerNEXError.invalidVariableGeometry(name: name)
                }
                let starts = try readTicks(
                    reader: reader,
                    offset: dataOffset,
                    count: count,
                    variableName: name
                )
                let ends = try readTicks(
                    reader: reader,
                    offset: dataOffset + count * 4,
                    count: count,
                    variableName: name
                )
                appendEvents(
                    ticks: starts,
                    frequency: frequency,
                    name: name + " start",
                    type: "interval_start",
                    to: &events
                )
                appendEvents(
                    ticks: ends,
                    frequency: frequency,
                    name: name + " end",
                    type: "interval_end",
                    to: &events
                )
            case 6: // Marker timestamps precede marker fields.
                let ticks = try readTicks(
                    reader: reader,
                    offset: dataOffset,
                    count: count,
                    variableName: name
                )
                appendEvents(
                    ticks: ticks,
                    frequency: frequency,
                    name: name,
                    type: "marker",
                    to: &events
                )
            case 4, 5: // Population vector / Continuous: not spike timestamps.
                ignored.append(name)
            default:
                throw NeuroExplorerNEXError.unsupportedVariableType(type: type, name: name)
            }
        }
        guard !trains.isEmpty else { throw NeuroExplorerNEXError.noSpikeVariables }
        return NeuroExplorerNEXReadResult(
            trains: trains,
            events: events,
            ignoredVariableNames: ignored,
            timestampFrequencyHz: frequency
        )
    }

    public static func writeManualISIResult(trains: [ManualISINEXTrain]) throws -> Data {
        var variables: [WriteVariable] = []
        for (trainOffset, train) in trains.enumerated() {
            let prefix = String(format: "T%03d", trainOffset + 1)
            variables.append(try neuronVariable(name: "\(prefix)_SPIKES", train: train))
            if !train.rows.isEmpty {
                variables.append(try markerVariable(name: "\(prefix)_ISI_LABELS", train: train))
                variables.append(contentsOf: try intervalVariables(prefix: prefix, train: train))
            }
        }
        guard !variables.isEmpty else { throw NeuroExplorerNEXError.noSpikeVariables }

        let headersSize = fileHeaderSize + variables.count * variableHeaderSize
        var runningOffset = headersSize
        var resolved: [WriteVariable] = []
        resolved.reserveCapacity(variables.count)
        for variable in variables {
            guard runningOffset <= Int(Int32.max),
                  variable.payload.count <= Int(Int32.max) - runningOffset else {
                throw NeuroExplorerNEXError.outputTooLarge
            }
            resolved.append(variable.withDataOffset(Int32(runningOffset)))
            runningOffset += variable.payload.count
        }

        let allTicks = trains.flatMap { $0.spikeTimestampsMicroseconds }
            + trains.flatMap { $0.rows.flatMap { [$0.leftTimestampMicroseconds, $0.rightTimestampMicroseconds] } }
        let beginTick = allTicks.min() ?? 0
        let endTick = allTicks.max() ?? 0
        guard let begin = Int32(exactly: beginTick), let end = Int32(exactly: endTick) else {
            throw NeuroExplorerNEXError.timestampOutsideClassicNEXRange(
                trainID: "dataset",
                microseconds: abs(beginTick) > abs(endTick) ? beginTick : endTick
            )
        }

        var output = Data()
        output.reserveCapacity(runningOffset)
        output.appendLittleEndian(magicNumber)
        output.appendLittleEndian(fileVersion)
        output.appendFixedUTF8(
            "STPD manual ISI result; Neuron=spikes, Marker=per-ISI labels, Interval=merged labels",
            length: 256
        )
        output.appendLittleEndian(timestampFrequencyHz.bitPattern)
        output.appendLittleEndian(begin)
        output.appendLittleEndian(end)
        output.appendLittleEndian(Int32(resolved.count))
        output.append(Data(repeating: 0, count: 260))
        for variable in resolved { variable.appendHeader(to: &output) }
        for variable in resolved { output.append(variable.payload) }
        return output
    }

    public static func wholeMicroseconds(
        seconds: Double,
        trainID: String
    ) throws -> Int64 {
        guard seconds.isFinite else {
            throw NeuroExplorerNEXError.timestampIsNotWholeMicrosecond(
                trainID: trainID,
                seconds: seconds
            )
        }
        let scaled = seconds * timestampFrequencyHz
        let rounded = scaled.rounded()
        guard abs(scaled - rounded) <= 1e-6,
              rounded >= Double(Int64.min), rounded <= Double(Int64.max) else {
            throw NeuroExplorerNEXError.timestampIsNotWholeMicrosecond(
                trainID: trainID,
                seconds: seconds
            )
        }
        return Int64(rounded)
    }

    private static func neuronVariable(
        name: String,
        train: ManualISINEXTrain
    ) throws -> WriteVariable {
        var payload = Data()
        for tick in train.spikeTimestampsMicroseconds {
            guard let value = Int32(exactly: tick) else {
                throw NeuroExplorerNEXError.timestampOutsideClassicNEXRange(
                    trainID: train.trainID,
                    microseconds: tick
                )
            }
            payload.appendLittleEndian(value)
        }
        return WriteVariable(
            type: 0,
            name: name,
            count: Int32(train.spikeTimestampsMicroseconds.count),
            nMarkers: 0,
            markerLength: 0,
            payload: payload
        )
    }

    private static func markerVariable(
        name: String,
        train: ManualISINEXTrain
    ) throws -> WriteVariable {
        let fieldNames = [
            "train_id", "train_name", "isi_index", "left_us", "right_us",
            "state", "event", "other", "state_annotation", "event_annotation",
            "other_annotation", "burst_veto", "review_note", "authority",
        ]
        let values: [[String]] = fieldNames.indices.map { field in
            train.rows.map { row in
                switch field {
                case 0: train.trainID
                case 1: train.trainName
                case 2: String(row.isiIndex)
                case 3: String(row.leftTimestampMicroseconds)
                case 4: String(row.rightTimestampMicroseconds)
                case 5: row.statePattern
                case 6: row.eventPattern
                case 7: row.otherPattern
                case 8: row.stateAnnotationID
                case 9: row.eventAnnotationID
                case 10: row.otherAnnotationID
                case 11: row.burstVeto ? "true" : "false"
                case 12: row.reviewNote
                default: row.authorityStatus
                }
            }
        }
        let markerLength = max(1, values.flatMap { $0 }.map { $0.utf8.count + 1 }.max() ?? 1)
        guard markerLength <= maximumMarkerValueBytes else {
            throw NeuroExplorerNEXError.markerValueTooLarge(trainID: train.trainID)
        }
        var payload = Data()
        for row in train.rows {
            guard let tick = Int32(exactly: row.rightTimestampMicroseconds) else {
                throw NeuroExplorerNEXError.timestampOutsideClassicNEXRange(
                    trainID: train.trainID,
                    microseconds: row.rightTimestampMicroseconds
                )
            }
            payload.appendLittleEndian(tick)
        }
        for (fieldOffset, fieldName) in fieldNames.enumerated() {
            payload.appendFixedUTF8(fieldName, length: 64)
            for value in values[fieldOffset] {
                payload.appendFixedUTF8(value, length: markerLength)
            }
        }
        return WriteVariable(
            type: 6,
            name: name,
            count: Int32(train.rows.count),
            nMarkers: Int32(fieldNames.count),
            markerLength: Int32(markerLength),
            payload: payload
        )
    }

    private static func intervalVariables(
        prefix: String,
        train: ManualISINEXTrain
    ) throws -> [WriteVariable] {
        let tracks: [(String, (ManualISINEXRow) -> String)] = [
            ("S", { $0.statePattern }),
            ("E", { $0.eventPattern }),
            ("O", { $0.otherPattern }),
        ]
        var result: [WriteVariable] = []
        for (trackCode, labelOf) in tracks {
            let grouped = Dictionary(grouping: train.rows.filter { !labelOf($0).isEmpty }) {
                labelOf($0)
            }
            for label in grouped.keys.sorted() {
                let ordered = (grouped[label] ?? []).sorted { $0.isiIndex < $1.isiIndex }
                var intervals: [(Int64, Int64)] = []
                var previousISIIndex: Int?
                for row in ordered {
                    if let last = intervals.last,
                       previousISIIndex.map({ $0 + 1 == row.isiIndex }) == true,
                       last.1 == row.leftTimestampMicroseconds {
                        intervals[intervals.count - 1].1 = row.rightTimestampMicroseconds
                    } else {
                        intervals.append((row.leftTimestampMicroseconds, row.rightTimestampMicroseconds))
                    }
                    previousISIIndex = row.isiIndex
                }
                var payload = Data()
                for interval in intervals {
                    guard let tick = Int32(exactly: interval.0) else {
                        throw NeuroExplorerNEXError.timestampOutsideClassicNEXRange(
                            trainID: train.trainID,
                            microseconds: interval.0
                        )
                    }
                    payload.appendLittleEndian(tick)
                }
                for interval in intervals {
                    guard let tick = Int32(exactly: interval.1) else {
                        throw NeuroExplorerNEXError.timestampOutsideClassicNEXRange(
                            trainID: train.trainID,
                            microseconds: interval.1
                        )
                    }
                    payload.appendLittleEndian(tick)
                }
                result.append(WriteVariable(
                    type: 2,
                    name: nexVariableName("\(prefix)_\(trackCode)_\(label)"),
                    count: Int32(intervals.count),
                    nMarkers: 0,
                    markerLength: 0,
                    payload: payload
                ))
            }
        }
        return result
    }

    private static func readTicks(
        reader: BinaryReader,
        offset: Int,
        count: Int,
        variableName: String
    ) throws -> [Int32] {
        let byteCount = try checkedByteCount(count: count, name: variableName)
        guard offset >= 0, offset <= reader.data.count,
              byteCount <= reader.data.count - offset else {
            throw NeuroExplorerNEXError.invalidVariableGeometry(name: variableName)
        }
        return try (0..<count).map { try reader.int32(at: offset + $0 * 4) }
    }

    private static func checkedByteCount(count: Int, name: String) throws -> Int {
        let (result, overflow) = count.multipliedReportingOverflow(by: 4)
        guard !overflow else { throw NeuroExplorerNEXError.invalidVariableGeometry(name: name) }
        return result
    }

    private static func appendEvents(
        ticks: [Int32],
        frequency: Double,
        name: String,
        type: String,
        to events: inout [NeuroExplorerNEXImportedEvent]
    ) {
        events.append(contentsOf: ticks.map {
            NeuroExplorerNEXImportedEvent(
                name: name.isEmpty ? "NEX event" : name,
                timestampSeconds: Double($0) / frequency,
                sourceVariableType: type
            )
        })
    }

    private static func nexVariableName(_ raw: String) -> String {
        let ascii = raw.unicodeScalars.map { scalar -> Character in
            let value = scalar.value
            let accepted = (0x30...0x39).contains(value) || (0x41...0x5A).contains(value)
                || (0x61...0x7A).contains(value) || value == 0x5F || value == 0x2D
            return accepted ? Character(String(scalar)) : "_"
        }
        return String(ascii.prefix(63))
    }
}

private struct BinaryReader {
    let data: Data

    init(_ data: Data) { self.data = data }

    func int32(at offset: Int) throws -> Int32 {
        guard offset >= 0, offset + 4 <= data.count else {
            throw NeuroExplorerNEXError.sourceTooShort
        }
        var value: Int32 = 0
        _ = withUnsafeMutableBytes(of: &value) { destination in
            data.copyBytes(to: destination, from: offset..<(offset + 4))
        }
        return Int32(littleEndian: value)
    }

    func double(at offset: Int) throws -> Double {
        guard offset >= 0, offset + 8 <= data.count else {
            throw NeuroExplorerNEXError.sourceTooShort
        }
        var bits: UInt64 = 0
        _ = withUnsafeMutableBytes(of: &bits) { destination in
            data.copyBytes(to: destination, from: offset..<(offset + 8))
        }
        return Double(bitPattern: UInt64(littleEndian: bits))
    }

    func fixedString(at offset: Int, length: Int) throws -> String {
        guard offset >= 0, length >= 0, offset + length <= data.count else {
            throw NeuroExplorerNEXError.sourceTooShort
        }
        let bytes = data[offset..<(offset + length)].prefix { $0 != 0 }
        return String(decoding: bytes, as: UTF8.self)
    }
}

private struct WriteVariable {
    let type: Int32
    let name: String
    let dataOffset: Int32
    let count: Int32
    let nMarkers: Int32
    let markerLength: Int32
    let payload: Data

    init(
        type: Int32,
        name: String,
        dataOffset: Int32 = 0,
        count: Int32,
        nMarkers: Int32,
        markerLength: Int32,
        payload: Data
    ) {
        self.type = type
        self.name = name
        self.dataOffset = dataOffset
        self.count = count
        self.nMarkers = nMarkers
        self.markerLength = markerLength
        self.payload = payload
    }

    func withDataOffset(_ offset: Int32) -> WriteVariable {
        WriteVariable(
            type: type,
            name: name,
            dataOffset: offset,
            count: count,
            nMarkers: nMarkers,
            markerLength: markerLength,
            payload: payload
        )
    }

    func appendHeader(to data: inout Data) {
        data.appendLittleEndian(type)
        data.appendLittleEndian(Int32(102))
        data.appendFixedUTF8(name, length: 64)
        data.appendLittleEndian(dataOffset)
        data.appendLittleEndian(count)
        data.appendLittleEndian(Int32(0)) // Wire
        data.appendLittleEndian(Int32(0)) // Unit
        data.appendLittleEndian(Int32(0)) // Gain
        data.appendLittleEndian(Int32(0)) // Filter
        data.appendLittleEndian(Double(0).bitPattern) // XPos
        data.appendLittleEndian(Double(0).bitPattern) // YPos
        data.appendLittleEndian(Double(0).bitPattern) // SamplingRate
        data.appendLittleEndian(Double(0).bitPattern) // ADtoMV
        data.appendLittleEndian(Int32(0)) // NPointsWave
        data.appendLittleEndian(nMarkers)
        data.appendLittleEndian(markerLength)
        data.appendLittleEndian(Double(0).bitPattern) // MVOffset
        data.appendLittleEndian(Double(0).bitPattern) // PreThrTime
        data.append(Data(repeating: 0, count: 52))
    }
}

private extension Data {
    mutating func appendLittleEndian<T: FixedWidthInteger>(_ value: T) {
        var little = value.littleEndian
        Swift.withUnsafeBytes(of: &little) { append(contentsOf: $0) }
    }

    mutating func appendFixedUTF8(_ value: String, length: Int) {
        var bytes = Array(value.utf8.prefix(Swift.max(0, length - 1)))
        if bytes.count < length { bytes.append(contentsOf: repeatElement(0, count: length - bytes.count)) }
        append(contentsOf: bytes.prefix(length))
    }
}
