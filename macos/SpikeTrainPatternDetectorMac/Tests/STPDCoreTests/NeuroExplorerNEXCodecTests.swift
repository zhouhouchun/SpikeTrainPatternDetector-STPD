import Foundation
import Testing
@testable import STPDCore

private func nexInt32(_ data: Data, at offset: Int) -> Int32 {
    var value: Int32 = 0
    _ = withUnsafeMutableBytes(of: &value) {
        data.copyBytes(to: $0, from: offset..<(offset + 4))
    }
    return Int32(littleEndian: value)
}

private func replaceNEXInt32(_ data: inout Data, at offset: Int, with value: Int32) {
    var little = value.littleEndian
    withUnsafeBytes(of: &little) {
        data.replaceSubrange(offset..<(offset + 4), with: $0)
    }
}

private func replaceNEXFixedString(_ data: inout Data, at offset: Int, value: String, length: Int) {
    var bytes = Array(value.utf8.prefix(max(0, length - 1)))
    bytes.append(contentsOf: repeatElement(0, count: length - bytes.count))
    data.replaceSubrange(offset..<(offset + length), with: bytes)
}

@Test
func classicNEXManualResultRoundTripsSpikesAndKeepsEventsSeparate() throws {
    let rows = [
        ManualISINEXRow(
            isiIndex: 1,
            leftTimestampMicroseconds: 1_000,
            rightTimestampMicroseconds: 2_000,
            statePattern: "tonic",
            eventPattern: "",
            otherPattern: "",
            authorityStatus: "draft"
        ),
        ManualISINEXRow(
            isiIndex: 2,
            leftTimestampMicroseconds: 2_000,
            rightTimestampMicroseconds: 4_000,
            statePattern: "tonic",
            eventPattern: "burst",
            otherPattern: "",
            authorityStatus: "draft"
        ),
    ]
    let data = try NeuroExplorerNEXCodec.writeManualISIResult(trains: [
        ManualISINEXTrain(
            trainID: "unit-A",
            trainName: "Unit A",
            spikeTimestampsMicroseconds: [1_000, 2_000, 4_000],
            rows: rows
        ),
    ])

    #expect(nexInt32(data, at: 0) == 827_868_494)
    #expect(nexInt32(data, at: 4) == 106)
    #expect(nexInt32(data, at: 280) == 4) // neuron, marker, tonic, burst

    let result = try NeuroExplorerNEXCodec.read(data)
    #expect(result.trains.count == 1)
    #expect(result.trains[0].timestampsSeconds == [0.001, 0.002, 0.004])
    #expect(result.events.filter { $0.sourceVariableType == "marker" }.count == 2)
    #expect(result.events.filter { $0.sourceVariableType == "interval_start" }.count == 2)
    #expect(result.events.filter { $0.sourceVariableType == "interval_end" }.count == 2)

    let dataset = NeuroExplorerNEXDatasetAdapter.dataset(
        from: result,
        name: "NEX fixture",
        sourceDescription: "test.nex",
        duplicateTimestampPolicy: .errorKeep
    )
    #expect(dataset.trains.count == 1)
    #expect(dataset.taskEvents.count == result.events.count)
    #expect(dataset.totalSpikeCount == 3)
}

@Test
func classicNEXRejectsInvalidMagicAndOutOfRangeTimestamp() throws {
    #expect(throws: NeuroExplorerNEXError.invalidMagicNumber) {
        try NeuroExplorerNEXCodec.read(Data(repeating: 0, count: 544))
    }

    let train = ManualISINEXTrain(
        trainID: "too-late",
        trainName: "Too late",
        spikeTimestampsMicroseconds: [Int64(Int32.max) + 1],
        rows: []
    )
    #expect(throws: NeuroExplorerNEXError.timestampOutsideClassicNEXRange(
        trainID: "too-late",
        microseconds: Int64(Int32.max) + 1
    )) {
        try NeuroExplorerNEXCodec.writeManualISIResult(trains: [train])
    }
}

@Test
func intervalWriterDoesNotBridgeAFilteredOutISI() throws {
    let rows = [
        ManualISINEXRow(
            isiIndex: 1,
            leftTimestampMicroseconds: 1_000,
            rightTimestampMicroseconds: 2_000,
            statePattern: "tonic",
            eventPattern: "",
            otherPattern: "",
            authorityStatus: "draft"
        ),
        ManualISINEXRow(
            isiIndex: 3,
            leftTimestampMicroseconds: 2_000,
            rightTimestampMicroseconds: 4_000,
            statePattern: "tonic",
            eventPattern: "",
            otherPattern: "",
            authorityStatus: "draft"
        ),
    ]
    let data = try NeuroExplorerNEXCodec.writeManualISIResult(trains: [
        ManualISINEXTrain(
            trainID: "unit-A",
            trainName: "Unit A",
            spikeTimestampsMicroseconds: [1_000, 2_000, 4_000],
            rows: rows
        ),
    ])
    let result = try NeuroExplorerNEXCodec.read(data)
    #expect(result.events.filter { $0.sourceVariableType == "interval_start" }.count == 2)
}

@Test
func classicNEXRejectsHeaderPointingPayloadAndMakesDuplicateTrainNamesUnique() throws {
    let trains = [
        ManualISINEXTrain(
            trainID: "unit-A",
            trainName: "Unit A",
            spikeTimestampsMicroseconds: [1_000, 2_000],
            rows: []
        ),
        ManualISINEXTrain(
            trainID: "unit-B",
            trainName: "Unit B",
            spikeTimestampsMicroseconds: [3_000, 4_000],
            rows: []
        ),
    ]
    var duplicateNames = try NeuroExplorerNEXCodec.writeManualISIResult(trains: trains)
    replaceNEXFixedString(&duplicateNames, at: 544 + 8, value: "same-unit", length: 64)
    replaceNEXFixedString(&duplicateNames, at: 544 + 208 + 8, value: "same-unit", length: 64)

    let imported = try NeuroExplorerNEXCodec.read(duplicateNames)
    #expect(imported.trains.map(\.name) == ["same-unit", "same-unit [2]"])

    var invalidOffset = duplicateNames
    replaceNEXInt32(&invalidOffset, at: 544 + 72, with: 0)
    #expect(throws: NeuroExplorerNEXError.invalidVariableGeometry(name: "same-unit")) {
        try NeuroExplorerNEXCodec.read(invalidOffset)
    }
}
