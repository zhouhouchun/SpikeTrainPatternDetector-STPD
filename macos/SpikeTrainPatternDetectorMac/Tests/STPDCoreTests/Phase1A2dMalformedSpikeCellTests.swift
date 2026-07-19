import STPDCore
import Testing

@Test
func phase1A2dInternalMalformedCellFailsClosedBeforeFabricatingISI() {
    let error = phase1A2dParseError("""
    train_a
    0.10
    BAD_TOKEN
    0.30
    """)

    let description = error?.errorDescription ?? ""
    #expect(description.contains("BAD_TOKEN"))
    #expect(description.contains("train_a"))
    #expect(description.contains("line 3"))
    #expect(description.contains("artificially long interval"))
}

@Test
func phase1A2dInternalMissingAndNonFiniteTokensAreMalformed() {
    for token in ["NA", "NaN", "null", "Inf", "-Inf", "Infinity"] {
        let error = phase1A2dParseError("train_a\n0.10\n\(token)\n0.30\n")
        let description = error?.errorDescription ?? ""
        #expect(description.contains("\"\(token)\""))
        #expect(description.contains("line 3"))
    }
}

@Test
func phase1A2dTrailingMissingValuePaddingRemainsAccepted() throws {
    for token in ["NA", "nan", "Null"] {
        let dataset = try CSVSpikeMatrixParser.parse(
            contents: "train_a,train_b\n0.10,0.50\n0.20,\(token)\n0.30,\(token)\n",
            datasetName: "trailing_padding",
            sourceDescription: "inline",
            unit: .seconds
        )

        #expect(dataset.trains.map(\.name) == ["train_a", "train_b"])
        #expect(dataset.trains[0].timestampsSec == [0.10, 0.20, 0.30])
        #expect(dataset.trains[1].timestampsSec == [0.50])
    }
}

@Test
func phase1A2dSparseBlankAndRaggedCellsRemainAccepted() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: "train_a,train_b\n0.10,0.50\n,0.60\n0.30\n0.40,0.80\n",
        datasetName: "sparse_matrix",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains[0].timestampsSec == [0.10, 0.30, 0.40])
    #expect(dataset.trains[1].timestampsSec == [0.50, 0.60, 0.80])
}

@Test
func phase1A2dTrailingGarbageAndInfinityRemainMalformed() {
    for token in ["BAD_TOKEN", "Inf", "-Inf", "Infinity"] {
        let error = phase1A2dParseError("train_a\n0.10\n0.20\n\(token)\n")
        let description = error?.errorDescription ?? ""
        #expect(description.contains("\"\(token)\""))
        #expect(description.contains("line 4"))
    }
}

@Test
func phase1A2dAllInvalidNonemptySpikeColumnIsMalformed() {
    for token in ["BAD_TOKEN", "NA", "nan", "Inf"] {
        let error = phase1A2dParseError(
            "train_a,train_b\n0.10,\(token)\n0.20,\(token)\n"
        )
        let description = error?.errorDescription ?? ""
        #expect(description.contains("train_b"))
        #expect(description.contains("\"\(token)\""))
        #expect(description.contains("line 2"))
    }
}

@Test
func phase1A2dHeaderlessMalformedCellReportsPhysicalFileLine() {
    let error = phase1A2dParseError(
        "0.10\nBAD_TOKEN\n0.30\n",
        hasHeader: false
    )
    let description = error?.errorDescription ?? ""
    #expect(description.contains("Train 1"))
    #expect(description.contains("line 2"))
}

@Test
func phase1A2dMalformedCellReportsPhysicalLineAcrossBlankAndCRLFRows() {
    let error = phase1A2dParseError(
        "train_a\r\n0.10\r\n\r\nBAD_TOKEN\r\n0.30\r\n"
    )
    let description = error?.errorDescription ?? ""
    #expect(description.contains("BAD_TOKEN"))
    #expect(description.contains("line 4"))
}

@Test
func phase1A2dMalformedCSVQuotingCannotNormalizeInvalidTokens() {
    let malformedRows = [
        "N\"A\"",
        "0.\"30\"",
        "\"0.30"
    ]

    for malformedRow in malformedRows {
        let error = phase1A2dParseError(
            "train_a\n0.10\n\(malformedRow)\n0.40\n"
        )
        let description = error?.errorDescription ?? ""
        #expect(description.contains("Malformed CSV syntax"))
        #expect(description.contains("line 3"))
    }
}

@Test
func phase1A2dValidQuotedFieldsRemainAccepted() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: "\"train,a\",train_b\n\"0.10\",\"0.20\"\n\"0.30\",0.40\n",
        datasetName: "quoted_fields",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.map(\.name) == ["train,a", "train_b"])
    #expect(dataset.trains[0].timestampsSec == [0.10, 0.30])
    #expect(dataset.trains[1].timestampsSec == [0.20, 0.40])
}

@Test
func phase1A2dBOMPrefixedQuotedHeaderRemainsAccepted() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: "\u{FEFF}\"train,a\",train_b\n\"0.10\",0.20\n\"0.30\",0.40\n",
        datasetName: "bom_quoted_header",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.map(\.name) == ["train,a", "train_b"])
    #expect(dataset.trains[0].timestampsSec == [0.10, 0.30])
    #expect(dataset.trains[1].timestampsSec == [0.20, 0.40])
}

@Test
func phase1A2dMalformedSecondColumnQuoteCarriesStructuredProvenance() {
    let error = phase1A2dParseError(
        "train_a,train_b\n0.10,0.20\n0.30,\"0.40\n"
    )
    guard let error,
          case .malformedCSV(
              let line,
              let column,
              let columnIndex,
              let token,
              let reason
          ) = error else {
        Issue.record("Expected structured malformedCSV provenance")
        return
    }

    #expect(line == 3)
    #expect(column == "train_b")
    #expect(columnIndex == 2)
    #expect(token == "\"0.40")
    #expect(reason == "unterminated quoted field")
}

@Test
func phase1A2dClosingQuoteMustBeFollowedImmediatelyByDelimiterOrEndOfLine() {
    for token in ["\"0.40\"x", "\"0.40\" "] {
        let error = phase1A2dParseError(
            "train_a,train_b\n0.10,0.20\n0.30,\(token)\n"
        )
        guard let error,
              case .malformedCSV(
                  let line,
                  let column,
                  let columnIndex,
                  let reportedToken,
                  let reason
              ) = error else {
            Issue.record("Expected strict post-closing-quote failure")
            continue
        }

        #expect(line == 3)
        #expect(column == "train_b")
        #expect(columnIndex == 2)
        #expect(reportedToken == token)
        #expect(reason == "unexpected character after a closing quote")
    }
}

@Test
func phase1A2dEscapedQuotesAndTrailingEmptyHeaderFieldRemainAccepted() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: "\"train \"\"A\"\"\",\n\"0.10\",0.20\n\"0.30\",0.40\n",
        datasetName: "escaped_header",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.map(\.name) == ["train \"A\"", "Train 2"])
    #expect(dataset.trains[0].timestampsSec == [0.10, 0.30])
    #expect(dataset.trains[1].timestampsSec == [0.20, 0.40])
}

@Test
func phase1A2dOverWidthHeaderRowFailsClosed() {
    let error = phase1A2dParseError(
        "train_a\n0.10\n0.20,BAD_TOKEN\n0.30\n"
    )
    guard let error,
          case .malformedCSV(
              let line,
              let column,
              let columnIndex,
              let token,
              _
          ) = error else {
        Issue.record("Expected structured over-width malformedCSV failure")
        return
    }

    #expect(line == 3)
    #expect(column == "Column 2")
    #expect(columnIndex == 2)
    #expect(token == "BAD_TOKEN")
    let description = error.errorDescription ?? ""
    #expect(description.contains("2 fields"))
    #expect(description.contains("1 columns"))
}

@Test
func phase1A2dDuplicateHeadersReportPhysicalColumnIndex() {
    let error = phase1A2dParseError(
        "unit,unit\n0.10,0.20\n0.30,BAD_TOKEN\n"
    )
    let description = error?.errorDescription ?? ""
    #expect(description.contains("column \"unit\" (#2)"))
    #expect(description.contains("line 3"))
    #expect(description.contains("BAD_TOKEN"))
}

@Test
func phase1A2dDiagnosticTextEscapesControlCharactersAndQuotes() {
    let error = phase1A2dParseError(
        "\"train \"\"quoted\"\"\"\n0.10\nBAD\tTOKEN\n0.30\n"
    )
    let description = error?.errorDescription ?? ""

    #expect(description.contains("BAD\\tTOKEN"))
    #expect(description.contains("\\\"quoted\\\""))
    #expect(!description.contains("\t"))
}

@Test
func phase1A2dMalformedTaskEventCellsDoNotCorruptSpikeParsing() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        train_a,event_stim
        0.10,5
        0.20,BAD_TOKEN
        0.30,Inf
        0.40,8
        """,
        datasetName: "task_events",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.count == 1)
    #expect(dataset.trains[0].timestampsSec == [0.10, 0.20, 0.30, 0.40])
    #expect(dataset.taskEvents.map(\.timeSec) == [5.0, 8.0])
    #expect(dataset.taskEvents.map(\.eventIndex) == [1, 4])
}

@Test
func phase1A2dValidInputAndDuplicatePolicyRemainUnchanged() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        train_a,event
        0.30,5
        0.10,
        0.10,8
        0.20,
        """,
        datasetName: "valid_control",
        sourceDescription: "inline",
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )

    let train = try #require(dataset.trains.first)
    #expect(train.timestampsSec == [0.10, 0.20, 0.30])
    #expect(train.droppedDuplicateTimestampCount == 1)
    #expect(train.inputWasUnsorted)
    #expect(dataset.taskEvents.map(\.timeSec) == [5.0, 8.0])
}

private func phase1A2dParseError(
    _ csv: String,
    hasHeader: Bool = true
) -> CSVSpikeMatrixParserError? {
    do {
        _ = try CSVSpikeMatrixParser.parse(
            contents: csv,
            datasetName: "phase1a2d",
            sourceDescription: "inline",
            unit: .seconds,
            hasHeader: hasHeader
        )
        Issue.record("Expected malformed spike-cell input to fail closed")
        return nil
    } catch let error as CSVSpikeMatrixParserError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}
