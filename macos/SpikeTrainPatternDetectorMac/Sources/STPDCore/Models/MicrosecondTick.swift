/// An exact timestamp on the app's canonical signed-microsecond grid.
///
/// This type deliberately does not conform to `Codable`: result-package persistence and
/// scientific identity must encode `microseconds` through an explicit canonical contract.
public struct MicrosecondTick: Hashable, Comparable, Sendable {
    public let microseconds: Int64

    public init(microseconds: Int64) {
        self.microseconds = microseconds
    }

    public static let zero = MicrosecondTick(microseconds: 0)

    public static func < (lhs: MicrosecondTick, rhs: MicrosecondTick) -> Bool {
        lhs.microseconds < rhs.microseconds
    }

    /// Returns `self - origin`, rejecting arithmetic that cannot be represented by `Int64`.
    public func rebased(relativeTo origin: MicrosecondTick) throws -> MicrosecondTick {
        let (value, overflow) = microseconds.subtractingReportingOverflow(origin.microseconds)
        guard !overflow else { throw MicrosecondArithmeticError.overflow }
        return MicrosecondTick(microseconds: value)
    }

    /// Returns the signed interval `self - earlier`, rejecting arithmetic overflow.
    public func interval(since earlier: MicrosecondTick) throws -> Int64 {
        let (value, overflow) = microseconds.subtractingReportingOverflow(earlier.microseconds)
        guard !overflow else { throw MicrosecondArithmeticError.overflow }
        return value
    }
}

public enum MicrosecondArithmeticError: Error, Equatable, Sendable {
    case overflow
}
