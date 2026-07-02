import Foundation

// MARK: - Phase D2 — compute D1 ISI distributions from a real SpikeDataset
//
// A pure, read-only service that turns a `SpikeDataset` into the Phase D1 distribution models
// (`TrainISIDistribution` per train + a pooled/train-balanced `DatasetISIDistribution`). It does NOT
// touch detectors, band resolution, or the pipeline — wiring it into the detection run at the
// `ClassicAnchorDetectionPipeline` seam is the separate follow-up slice (D2-wire).
//
// It reproduces the codebase's verified QC semantics by delegating to `TrainISIDistribution.from`:
//   • the structural `SpikeTrain.isiSec[0]` placeholder is skipped (never counted);
//   • a finite ISI is kept iff `value >= floor` (INCLUSIVE lower bound);
//   • only `value < floor` is excluded (recorded in the D1 exclusion summary).
//
// The inclusion floor is supplied by the CALLER under a deliberately NEUTRAL name
// (`minimumValidISISec`) because the identical computation must reproduce two distinct real paths
// that key on two different thresholds:
//   • the QC / display path — `SpikeQualitySettings.artifactThresholdSec` (default 0.0009);
//   • the detector / band path — `TrainAdaptiveBandSettings.minValidISISec` (default 0.001).
// The service is agnostic to which one a caller means; it only applies the floor it is given.
public enum DatasetISIDistributionService {
    /// Compute the per-train `TrainISIDistribution`s and the dataset-level `DatasetISIDistribution`
    /// (pooled + train-balanced) for `dataset`, keeping ISIs at or above `minimumValidISISec`.
    public static func compute(
        dataset: SpikeDataset,
        minimumValidISISec: Double
    ) -> DatasetISIDistribution {
        let perTrainDistributions = dataset.trains.map { train in
            TrainISIDistribution.from(
                trainID: train.id,
                trainName: train.name,
                rawISISec: train.isiSec,
                artifactThresholdSec: minimumValidISISec,
                spikeCount: train.spikeCount
            )
        }
        return DatasetISIDistribution(
            datasetName: dataset.name,
            trainDistributions: perTrainDistributions
        )
    }
}
