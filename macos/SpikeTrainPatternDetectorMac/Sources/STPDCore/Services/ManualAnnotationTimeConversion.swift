import Foundation

/// Pure raster time-axis conversion for manual annotations.
///
/// `ManualAnnotation.startSec/endSec` are authoritative **raw** train-timestamp seconds (the geometry
/// resolver works against `SpikeTrain.timestampsSec`). The raster, however, may display **aligned**
/// time, where `aligned = raw - firstTimestamp`. These helpers convert between a displayed time and
/// the stored raw time. They take the train's first raw timestamp and an `aligned` flag rather than
/// the app-layer `RasterTimeMode`, so the conversion is unit-testable in STPDCore.
public enum ManualAnnotationTimeConversion {
    /// Displayed time (as drawn on the raster) → raw train-timestamp seconds to STORE in a
    /// `ManualAnnotation`. In raw mode this is the identity; in aligned mode it adds the train's
    /// first raw timestamp.
    public static func rawSec(displaySec: Double, firstTimestampSec: Double, aligned: Bool) -> Double {
        aligned ? displaySec + firstTimestampSec : displaySec
    }

    /// Stored raw time → displayed time to DRAW for the given mode. In raw mode this is the identity;
    /// in aligned mode it subtracts the train's first raw timestamp.
    public static func displaySec(rawSec: Double, firstTimestampSec: Double, aligned: Bool) -> Double {
        aligned ? rawSec - firstTimestampSec : rawSec
    }
}
