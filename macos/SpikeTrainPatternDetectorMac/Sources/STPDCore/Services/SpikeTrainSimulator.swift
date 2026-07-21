import Foundation

/// SIM-1B — a pure, deterministic spike-train simulator primitive. It generates synthetic `SpikeTrain` values for
/// preview/testing only: no R runtime, no document/detector/UI coupling, no annotations, no manual thresholds.
///
/// It mirrors the MINIMAL R reference `generated_stimulus_spike_trains/generate_tonic_repeated_stimulus_burst_pause.R`
/// (NOT the 16k Shiny simulator): truncated `Normal/Gamma/Exp` samplers assembled into a cumulative-ISI spike train.
/// As in that reference, the ONLY rejection is **value-range truncation** — there is no CV/CV2/LV re-roll here. The
/// regularity metrics (`STPDStatistics.coefficientOfVariation`/`…2`/`localVariation`) are evaluation OUTPUTS computed
/// downstream, never generator inputs.
///
/// RNG note: a portable SplitMix64 makes a given `seed` reproduce the same sequence on every run/platform. It does NOT
/// reproduce R's `rnorm/rgamma/rexp` byte-for-byte (impossible across RNG implementations); parity with R is
/// distributional (moments/ranges), which is exactly how the tests assert it.
public struct SeededSplitMix64: RandomNumberGenerator, Sendable {
    private var state: UInt64

    public init(seed: UInt64) { state = seed }

    public mutating func next() -> UInt64 {
        state &+= 0x9E37_79B9_7F4A_7C15
        var z = state
        z = (z ^ (z >> 30)) &* 0xBF58_476D_1CE4_E5B9
        z = (z ^ (z >> 27)) &* 0x94D0_49BB_1331_11EB
        return z ^ (z >> 31)
    }
}

public struct SpikeTrainSimulator {
    public private(set) var rng: SeededSplitMix64

    public init(seed: UInt64) { rng = SeededSplitMix64(seed: seed) }

    // MARK: - Uniform + distribution draws (each draw is self-contained and order-deterministic)

    /// A uniform double in [0, 1) using 53 mantissa bits.
    public mutating func uniform() -> Double {
        Double(rng.next() >> 11) * (1.0 / 9_007_199_254_740_992.0)
    }

    /// One standard-normal deviate via Box–Muller. The paired deviate is intentionally discarded so every call is
    /// self-contained (clarity over a tiny efficiency gain) — the stream stays deterministic regardless of call order.
    public mutating func standardNormal() -> Double {
        let u1 = Swift.max(uniform(), 1e-12)
        let u2 = uniform()
        return (-2.0 * Foundation.log(u1)).squareRoot() * Foundation.cos(2.0 * Double.pi * u2)
    }

    /// Gamma(shape, scale) via Marsaglia–Tsang (exact for shape ≥ 1; a boost handles shape < 1). Mean = shape·scale.
    public mutating func gamma(shape: Double, scale: Double) -> Double {
        guard shape > 0, scale > 0 else { return 0 }
        if shape < 1 {
            let boosted = gamma(shape: shape + 1, scale: scale)
            return boosted * Foundation.pow(Swift.max(uniform(), 1e-12), 1.0 / shape)
        }
        let d = shape - 1.0 / 3.0
        let c = 1.0 / (9.0 * d).squareRoot()
        while true {
            let x = standardNormal()
            let v0 = 1.0 + c * x
            if v0 <= 0 { continue }
            let v = v0 * v0 * v0
            let u = uniform()
            if u < 1.0 - 0.0331 * (x * x) * (x * x) { return d * v * scale }
            if Foundation.log(Swift.max(u, 1e-12)) < 0.5 * x * x + d * (1.0 - v + Foundation.log(v)) {
                return d * v * scale
            }
        }
    }

    /// Exponential with the given mean (= 1/rate).
    public mutating func exponential(mean: Double) -> Double {
        -mean * Foundation.log(Swift.max(1.0 - uniform(), 1e-12))
    }

    // MARK: - Truncated samplers (range rejection, mirrors R `sample_truncated`)

    /// Re-draw a Normal(mean, sd) until it lands in [lower, upper]; clamps to the nearer bound only if `maxAttempts` is
    /// exhausted (never reached for sane parameters). Mirrors the R reference's range truncation.
    public mutating func truncatedNormal(
        mean: Double, sd: Double, lower: Double, upper: Double, maxAttempts: Int = 10_000
    ) -> Double {
        var last = mean
        for _ in 0..<maxAttempts {
            last = mean + sd * standardNormal()
            if last.isFinite, last >= lower, last <= upper { return last }
        }
        return Swift.min(Swift.max(last, lower), upper)
    }

    public mutating func truncatedGamma(
        shape: Double, scale: Double, lower: Double, upper: Double, maxAttempts: Int = 10_000
    ) -> Double {
        var last = shape * scale
        for _ in 0..<maxAttempts {
            last = gamma(shape: shape, scale: scale)
            if last.isFinite, last >= lower, last <= upper { return last }
        }
        return Swift.min(Swift.max(last, lower), upper)
    }

    public mutating func truncatedExponential(
        mean: Double, lower: Double, upper: Double, maxAttempts: Int = 10_000
    ) -> Double {
        var last = mean
        for _ in 0..<maxAttempts {
            last = exponential(mean: mean)
            if last.isFinite, last >= lower, last <= upper { return last }
        }
        return Swift.min(Swift.max(last, lower), upper)
    }

    // MARK: - Generators

    /// A tonic spike train: cumulative truncated-Normal ISIs from t=0 up to `durationSec`. Defaults reproduce the R
    /// reference's tonic baseline (Normal(0.45, 0.03) truncated to [0.38, 0.52]). The spread is set by `sdSec` and the
    /// truncation window — widen both to model an irregular ("high-jitter") tonic train.
    public mutating func tonicTrain(
        name: String = "sim_tonic",
        durationSec: Double = 30,
        meanSec: Double = 0.45,
        sdSec: Double = 0.03,
        lowerSec: Double = 0.38,
        upperSec: Double = 0.52
    ) -> SpikeTrain {
        var times: [Double] = [0.0]
        guard durationSec > 0, meanSec > 0, lowerSec > 0, upperSec > lowerSec else {
            return SpikeTrain(name: name, timestampsSec: times)
        }
        var t = 0.0
        appendTonicBaseline(into: &times, current: &t, stopBeforeSec: durationSec,
                            meanSec: meanSec, sdSec: sdSec, lowerSec: lowerSec, upperSec: upperSec)
        return SpikeTrain(name: name, timestampsSec: times)
    }

    /// Append truncated-Normal tonic ISIs (cumulative) from `t` until the next spike would exceed `stopBeforeSec`. Shared
    /// by `tonicTrain` and `burstResponseTrain` so there is one tonic-baseline implementation. The cap is a defensive
    /// bound (far above any real spike count) and never binds for sane parameters, so `tonicTrain` output is unchanged.
    private mutating func appendTonicBaseline(
        into times: inout [Double], current t: inout Double, stopBeforeSec: Double,
        meanSec: Double, sdSec: Double, lowerSec: Double, upperSec: Double
    ) {
        let span = stopBeforeSec - t
        guard span > 0, lowerSec > 0 else { return }
        let cap = Int((span / Swift.max(lowerSec, 1e-6)).rounded(.up)) + 8
        var added = 0
        while added < cap {
            let isi = truncatedNormal(mean: meanSec, sd: sdSec, lower: lowerSec, upper: upperSec)
            let next = t + isi
            if next > stopBeforeSec { break }
            times.append(next)
            t = next
            added += 1
        }
    }

    /// Burst-kernel pure helper (SIM-1B): `count` intra-burst ISIs drawn from a truncated Gamma, defaulting to the R
    /// reference's burst kernel (Gamma(shape 2, scale 0.012) truncated to [0.006, 0.045]). Returns ISIs only — assembling
    /// them into stimulus-locked responses is deferred (no UI / response wiring in this step). For a k-spike burst pass
    /// `count = k - 1`.
    public mutating func burstISIs(
        count: Int,
        shape: Double = 2,
        scaleSec: Double = 0.012,
        lowerSec: Double = 0.006,
        upperSec: Double = 0.045
    ) -> [Double] {
        guard count > 0 else { return [] }
        var isis: [Double] = []
        isis.reserveCapacity(count)
        for _ in 0..<count {
            isis.append(truncatedGamma(shape: shape, scale: scaleSec, lower: lowerSec, upper: upperSec))
        }
        return isis
    }

    /// A uniform integer in `range` (inclusive), drawn from `uniform()` so it is deterministic and Swift-version-
    /// independent (unlike `Int.random(in:using:)`). Used to pick spikes-per-burst.
    public mutating func sampleInt(in range: ClosedRange<Int>) -> Int {
        guard range.upperBound > range.lowerBound else { return range.lowerBound }
        let span = range.upperBound - range.lowerBound + 1
        let k = Int(uniform() * Double(span))
        return range.lowerBound + Swift.min(k, span - 1)
    }

    /// SIM-2B — a burst-response spike train: a tonic baseline (the shared `appendTonicBaseline`) with a compact burst
    /// PACKET placed at each onset in `burstOnsetsSec`. Each packet's intra-burst ISIs are drawn from the SIM-1B burst
    /// kernel (`burstISIs`, default Gamma(2, 0.012) truncated to [0.006, 0.045]); spikes-per-packet is sampled from
    /// `spikesPerBurst`. The packet's first spike is placed at its onset (floored to `interEventGapSec` after the prior
    /// spike), and the baseline resumes after it. No pause/recovery gap is generated here (deferred). Pure/deterministic:
    /// the same seed + parameters yield identical timestamps. These are GENERATOR outputs — detector labels are evaluated
    /// downstream, never baked into generation.
    public mutating func burstResponseTrain(
        name: String = "sim_burst_response",
        durationSec: Double = 30,
        baselineMeanSec: Double = 0.45,
        baselineSDSec: Double = 0.03,
        baselineLowerSec: Double = 0.38,
        baselineUpperSec: Double = 0.52,
        burstOnsetsSec: [Double],
        spikesPerBurst: ClosedRange<Int> = 3...6,
        interEventGapSec: Double = 0.003,
        burstShape: Double = 2,
        burstScaleSec: Double = 0.012,
        burstLowerSec: Double = 0.006,
        burstUpperSec: Double = 0.045
    ) -> SpikeTrain {
        var times: [Double] = [0.0]
        guard durationSec > 0, baselineMeanSec > 0, baselineLowerSec > 0,
              baselineUpperSec > baselineLowerSec, spikesPerBurst.lowerBound >= 1 else {
            return SpikeTrain(name: name, timestampsSec: times)
        }
        var t = 0.0
        let onsets = burstOnsetsSec.filter { $0 > 0 && $0 <= durationSec }.sorted()
        for onset in onsets {
            // Fill the tonic baseline up to (just before) this onset, then place the burst packet.
            appendTonicBaseline(into: &times, current: &t, stopBeforeSec: onset,
                                meanSec: baselineMeanSec, sdSec: baselineSDSec,
                                lowerSec: baselineLowerSec, upperSec: baselineUpperSec)
            let packetSpikes = sampleInt(in: spikesPerBurst)
            let start = Swift.max(onset, t + interEventGapSec)
            if start > durationSec { continue }
            times.append(start)
            t = start
            if packetSpikes > 1 {
                for isi in burstISIs(count: packetSpikes - 1, shape: burstShape, scaleSec: burstScaleSec,
                                     lowerSec: burstLowerSec, upperSec: burstUpperSec) {
                    let next = t + isi
                    if next > durationSec { break }
                    times.append(next)
                    t = next
                }
            }
        }
        // Fill the tonic baseline out to the end.
        appendTonicBaseline(into: &times, current: &t, stopBeforeSec: durationSec,
                            meanSec: baselineMeanSec, sdSec: baselineSDSec,
                            lowerSec: baselineLowerSec, upperSec: baselineUpperSec)
        return SpikeTrain(name: name, timestampsSec: times)
    }
}
