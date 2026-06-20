import Foundation

public enum ClassicAnchorReviewStatusCSVImporter {
    public enum ImportError: Error, LocalizedError {
        case missingColumn(String)

        public var errorDescription: String? {
            switch self {
            case .missingColumn(let column):
                return "Review CSV is missing the required \(column) column."
            }
        }
    }

    public static func importStatuses(contents: String) throws -> [String: String] {
        let rows = parseCSV(contents)
        guard let header = rows.first else {
            return [:]
        }

        guard let candidateIDIndex = header.firstIndex(of: "candidate_id") else {
            throw ImportError.missingColumn("candidate_id")
        }
        guard let reviewStatusIndex = header.firstIndex(of: "review_status") else {
            throw ImportError.missingColumn("review_status")
        }

        var statuses: [String: String] = [:]
        for row in rows.dropFirst() {
            guard candidateIDIndex < row.count, reviewStatusIndex < row.count else {
                continue
            }

            let candidateID = row[candidateIDIndex].trimmingCharacters(in: .whitespacesAndNewlines)
            let reviewStatus = row[reviewStatusIndex].trimmingCharacters(in: .whitespacesAndNewlines)
            guard !candidateID.isEmpty, !reviewStatus.isEmpty else {
                continue
            }
            statuses[candidateID] = reviewStatus
        }
        return statuses
    }

    private static func parseCSV(_ csv: String) -> [[String]] {
        var rows: [[String]] = []
        var row: [String] = []
        var field = ""
        var isQuoted = false
        var index = csv.startIndex

        while index < csv.endIndex {
            let character = csv[index]

            if character == "\"" {
                let next = csv.index(after: index)
                if isQuoted, next < csv.endIndex, csv[next] == "\"" {
                    field.append("\"")
                    index = csv.index(after: next)
                    continue
                }
                isQuoted.toggle()
            } else if character == ",", !isQuoted {
                row.append(field)
                field = ""
            } else if character == "\n", !isQuoted {
                row.append(field)
                if !row.allSatisfy(\.isEmpty) {
                    rows.append(row)
                }
                row = []
                field = ""
            } else if character != "\r" {
                field.append(character)
            }

            index = csv.index(after: index)
        }

        if !field.isEmpty || !row.isEmpty {
            row.append(field)
            rows.append(row)
        }

        return rows
    }
}
