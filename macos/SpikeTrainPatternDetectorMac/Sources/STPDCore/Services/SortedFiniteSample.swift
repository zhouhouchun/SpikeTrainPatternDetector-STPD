import Foundation

struct SortedFiniteSample {
    let values: [Double]

    init(_ values: [Double], positiveOnly: Bool = false) {
        if positiveOnly {
            self.values = values.filter { $0.isFinite && $0 > 0 }.sorted()
        } else {
            self.values = values.filter(\.isFinite).sorted()
        }
    }

    init(sortedFiniteValues values: [Double]) {
        self.values = values
    }

    var isEmpty: Bool {
        values.isEmpty
    }

    var count: Int {
        values.count
    }

    func quantile(_ probability: Double) -> Double? {
        guard !values.isEmpty else {
            return nil
        }
        guard values.count > 1 else {
            return values[0]
        }

        let p = min(max(probability, 0), 1)
        let position = p * Double(values.count - 1)
        let lower = Int(floor(position))
        let upper = Int(ceil(position))
        if lower == upper {
            return values[lower]
        }

        let fraction = position - Double(lower)
        return values[lower] + fraction * (values[upper] - values[lower])
    }
}
