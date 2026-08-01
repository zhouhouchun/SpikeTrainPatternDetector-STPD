import Foundation
import STPDCore
import Testing

// P9 — the manual hard-threshold SCOPE now affects detector math per train. These tests run the real pipeline on a
// two-train dataset and prove: `.allTrains` is byte-identical to the global path; a hard gate scoped to one train
// applies ONLY to that train (the other stays automatic); a scope that resolves to no train applies the hard gate
// nowhere (never a silent fallback to all); and soft anchors are unaffected by scope.

/// Two structurally-identical burst trains ("a", "b"): 5 tonic blocks (0.40 s) separated by 4-ISI bursts (0.008 s).
private func twoBurstTrains() throws -> SpikeDataset {
    var t = 0.0
    var times = [0.0]
    for block in 0..<5 {
        for _ in 0..<6 { t += 0.40; times.append(t) }
        if block < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } }
    }
    let rows = times.map { String(format: "%.6f,%.6f", $0, $0) }
    let csv = (["a,b"] + rows).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p9", sourceDescription: "p9")
}

/// A hard burst min-spikes gate large enough to reject the 5-spike bursts (the ONLY active field, so a train outside
/// the scope sees an all-automatic profile).
private let hardBurstGate = ManualThresholdProfile(
    burst: BurstManualThresholds(minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 50)))

private func run(_ ds: SpikeDataset, _ profile: ManualThresholdProfile, _ scope: ManualThresholdScope)
    -> ClassicAnchorDetectionRun {
    ClassicAnchorDetectionPipeline.run(dataset: ds, manualThresholdProfile: profile, manualThresholdScope: scope)
}

private func burstCount(_ run: ClassicAnchorDetectionRun, _ trainID: String) -> Int {
    (run.result(for: trainID)?.candidates ?? [])
        .filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }.count
}

/// A per-train candidate digest (label · span · selection) for byte-level output comparison.
private func digest(_ run: ClassicAnchorDetectionRun, _ trainID: String) -> [String] {
    (run.result(for: trainID)?.candidates ?? [])
        .map { "\($0.finalLabel.rawValue)|\($0.startISIIndex)-\($0.endISIIndex)|\($0.selectedForAuto)|\($0.decisionPath)" }
        .sorted()
}

@Test func p9_hardGateChangesBurstOutput_sanity() throws {
    // Establishes the gate is genuinely active: with no manual profile both trains have bursts; the hard min-spikes
    // gate (allTrains) removes them. Without this, the scope tests below would be vacuous.
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let auto = run(ds, .automatic, .allTrains)
    let gated = run(ds, hardBurstGate, .allTrains)
    #expect(burstCount(auto, a) > 0)
    #expect(burstCount(auto, b) > 0)
    #expect(burstCount(gated, a) < burstCount(auto, a))   // the hard gate suppresses bursts
    #expect(burstCount(gated, b) < burstCount(auto, b))
}

@Test func p9_allTrainsScopeIsByteIdenticalToGlobal() throws {
    // Requirement 4: `.allTrains` (and omitting the scope) is byte-identical to the global hard-gate application.
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let omitted = ClassicAnchorDetectionPipeline.run(dataset: ds, manualThresholdProfile: hardBurstGate) // default .allTrains
    let explicit = run(ds, hardBurstGate, .allTrains)
    #expect(digest(omitted, a) == digest(explicit, a))
    #expect(digest(omitted, b) == digest(explicit, b))
}

@Test func p9_selectedScopeAppliesGateOnlyToSelectedTrain() throws {
    // Requirement 5: the in-scope train is gated (burst output suppressed exactly like the global run); the out-of-scope
    // train stays automatic (its bursts survive, unlike the global run). Burst count is the per-train signal — a train's
    // OWN gate is what suppresses its bursts. (The dataset-level bridge aggregate is leave-one-out, so a train's audit
    // metadata can shift when ANOTHER train's gating changes; that is a consistent reflection of each train's resolved
    // bands, not a gate applied out of scope — hence we assert the per-train burst outcome rather than a full digest.)
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let auto = run(ds, .automatic, .allTrains)
    let gateAll = run(ds, hardBurstGate, .allTrains)
    let gateA = run(ds, hardBurstGate, ManualThresholdScope(kind: .selectedTrains, trainIDs: [a]))
    // Train A (in scope): gated exactly like the global run, and strictly fewer bursts than automatic.
    #expect(burstCount(gateA, a) == burstCount(gateAll, a))
    #expect(burstCount(gateA, a) < burstCount(auto, a))
    // Train B (out of scope): identical to automatic (the hard gate did NOT apply), and unlike the global-gated run.
    #expect(burstCount(gateA, b) == burstCount(auto, b))
    #expect(burstCount(gateA, b) > burstCount(gateAll, b))
}

@Test func p9_scopeResolvingToNoTrainAppliesGateNowhere() throws {
    // Requirement 6: a current/selected scope that resolves to no train applies the hard gate to NO train — never a
    // silent fallback to all trains.
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let auto = run(ds, .automatic, .allTrains)
    let noFocus = ManualThresholdScope.resolve(
        kind: .currentTrain, focusedTrainID: nil, selectedTrainIDs: [], allTrainIDs: [a, b])
    #expect(noFocus.targetsNoTrain)
    let gateNone = run(ds, hardBurstGate, noFocus)
    #expect(digest(gateNone, a) == digest(auto, a))   // every train stays automatic
    #expect(digest(gateNone, b) == digest(auto, b))
}

@Test func p9_softAnchorsAreUnaffectedByScope() throws {
    // Requirement 3: soft anchors stay global — scoping does not change a soft-only profile's per-train output.
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let soft = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.060)))
    let softAll = run(ds, soft, .allTrains)
    let softScopedToA = run(ds, soft, ManualThresholdScope(kind: .selectedTrains, trainIDs: [a]))
    // Out-of-scope train B is identical: dropping hard gates leaves soft anchors intact (a soft-only profile is a no-op
    // for the scope), so both trains match the global soft-anchor run.
    #expect(digest(softScopedToA, a) == digest(softAll, a))
    #expect(digest(softScopedToA, b) == digest(softAll, b))
}

@Test func p9_droppingHardGatesKeepsSoftAndAutomatic() {
    // Unit-level: droppingHardGates demotes ONLY hard gates; soft anchors / automatic / learned provenance are kept,
    // and a profile with no hard gates is returned unchanged.
    let mixed = ManualThresholdProfile(
        burst: BurstManualThresholds(
            seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.02),
            bridgeUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.05),
            minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 10)),
        pause: PauseManualThresholds(isiLower: ManualISIThreshold(mode: .softAnchor, valueSec: 0.5)),
        learnedProvenanceByKey: ["burst.bridge_upper_sec": "learned"])
    let dropped = mixed.droppingHardGates()
    #expect(dropped.burst.seedUpperISI.mode == .automatic)              // hard gate dropped
    #expect(dropped.burst.minSpikes.mode == .automatic)                 // hard count dropped
    #expect(dropped.burst.bridgeUpperISI == mixed.burst.bridgeUpperISI) // soft anchor kept
    #expect(dropped.pause.isiLower == mixed.pause.isiLower)             // soft anchor kept
    #expect(dropped.learnedProvenanceByKey == mixed.learnedProvenanceByKey)
    // A soft-only / automatic profile is unchanged.
    let softOnly = ManualThresholdProfile(
        pause: PauseManualThresholds(isiLower: ManualISIThreshold(mode: .softAnchor, valueSec: 0.5)))
    #expect(softOnly.droppingHardGates() == softOnly)
    #expect(ManualThresholdProfile.automatic.droppingHardGates() == .automatic)
}


// MARK: - P10: manual_threshold_scope provenance note (decisionPath audit)

/// A hard burst SEED-UPPER gate (the only active field) — it resolves through the ISI band resolver and records
/// `resolved_threshold[burst.seed_upper_sec]=user_hard_gate`, so an in-scope train's burst candidates carry the scope note.
private let hardSeedGate = ManualThresholdProfile(
    burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.020)))

/// All `manual_threshold_scope=...` notes found on a train's candidates.
private func scopeNotes(_ run: ClassicAnchorDetectionRun, _ trainID: String) -> [String] {
    (run.result(for: trainID)?.candidates ?? []).compactMap { c in
        c.decisionPath.components(separatedBy: ";").first { $0.hasPrefix("manual_threshold_scope=") }
    }
}

@Test func p10_allTrainsHardGateAddsAllTrainsScopeNote() throws {
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let r = run(ds, hardSeedGate, .allTrains)
    // Both trains are in scope; every scope note reads all_trains (and never a current/selected token).
    #expect(!scopeNotes(r, a).isEmpty)
    #expect(!scopeNotes(r, b).isEmpty)
    #expect(scopeNotes(r, a).allSatisfy { $0 == "manual_threshold_scope=all_trains" })
    #expect(scopeNotes(r, b).allSatisfy { $0 == "manual_threshold_scope=all_trains" })
}

@Test func p10_selectedScopeNoteOnlyOnInScopeTrain() throws {
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let r = run(ds, hardSeedGate, ManualThresholdScope(kind: .selectedTrains, trainIDs: [a]))
    // In-scope train A carries the selected-scope note with the exact count + its own train id.
    #expect(!scopeNotes(r, a).isEmpty)
    #expect(scopeNotes(r, a).allSatisfy { $0 == "manual_threshold_scope=selected_trains(count=1,train_id=\(a))" })
    // Out-of-scope train B carries NO scope note (its hard gate was dropped to automatic).
    #expect(scopeNotes(r, b).isEmpty)
}

@Test func p10_noTrainScopeHasNoNoteAnywhere() throws {
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let noFocus = ManualThresholdScope.resolve(
        kind: .currentTrain, focusedTrainID: nil, selectedTrainIDs: [], allTrainIDs: [a, b])
    let r = run(ds, hardSeedGate, noFocus)
    #expect(scopeNotes(r, a).isEmpty)
    #expect(scopeNotes(r, b).isEmpty)
}

@Test func p10_softOnlyProfileHasNoScopeNote() throws {
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let soft = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.060)))
    // Soft anchor scoped to A: no hard gate anywhere ⇒ no scope note on any train, regardless of scope.
    let r = run(ds, soft, ManualThresholdScope(kind: .selectedTrains, trainIDs: [a]))
    #expect(scopeNotes(r, a).isEmpty)
    #expect(scopeNotes(r, b).isEmpty)
}

@Test func p10_currentTrainScopeNoteFormat() throws {
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let r = run(ds, hardSeedGate, ManualThresholdScope(kind: .currentTrain, trainIDs: [a]))
    #expect(scopeNotes(r, a).allSatisfy { $0 == "manual_threshold_scope=current_train(train_id=\(a))" })
    #expect(!scopeNotes(r, a).isEmpty)
    #expect(scopeNotes(r, b).isEmpty)
}

@Test func p10_scopeNoteIsPerFamily_hardGatedFamilyOnly() throws {
    // Per-family discrimination at the pipeline level (covers a NON-burst hard gate + a mixed hard+soft case): on an
    // in-scope train, the scope note lands on the HARD-gated family's candidates but NOT on a family that is only
    // soft-anchored. Here tonic is hard-gated and burst is soft-anchored on the same in-scope train A.
    let ds = try twoBurstTrains()
    let a = ds.trains[0].id, b = ds.trains[1].id
    let mixed = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.060)),
        tonic: TonicManualThresholds(isiUpper: ManualISIThreshold(mode: .hardGate, valueSec: 0.600)))
    let r = run(ds, mixed, ManualThresholdScope(kind: .selectedTrains, trainIDs: [a]))
    func familyNotes(_ id: String, _ family: ClassicAnchorLabel) -> [String] {
        (r.result(for: id)?.candidates ?? []).filter { $0.finalLabel == family }
            .compactMap { c in c.decisionPath.components(separatedBy: ";").first { $0.hasPrefix("manual_threshold_scope=") } }
    }
    // tonic (HARD-gated) candidates on A carry the selected-scope note...
    #expect(!familyNotes(a, .tonic).isEmpty)
    #expect(familyNotes(a, .tonic).allSatisfy { $0 == "manual_threshold_scope=selected_trains(count=1,train_id=\(a))" })
    // ...while burst (only SOFT-anchored) candidates on the same train do NOT.
    #expect(familyNotes(a, .burst).isEmpty)
    // Out-of-scope train B carries no scope note on any family.
    #expect(scopeNotes(r, b).isEmpty)
}
