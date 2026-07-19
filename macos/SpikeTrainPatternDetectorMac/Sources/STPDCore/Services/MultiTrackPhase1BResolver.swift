import Foundation

/// Ordered Phase 1B compatibility pass for event, gap, state, and review tracks.
///
/// The order is intentional:
/// 1. select events and initial gaps;
/// 2. complete the monotonic pause floor;
/// 3. reselect the final event/gap tracks;
/// 4. split states at selected gap/event boundaries;
/// 5. resolve HFS packet dominance from selected canonical events;
/// 6. merge caller-authorized irregular-tonic fragments with full-span revalidation;
/// 7. perform final four-track arbitration.
///
/// State splitting deliberately precedes event-derived HFS rejection. Otherwise
/// an HFS parent that spans a selected pause could be rejected as a whole before
/// its biologically independent left/right fragments are re-evaluated.

public struct MultiTrackPhase1BResolution: Sendable {
    public let candidates: [ClassicAnchorCandidate]
    public let hfsBurstArbitrationAuditRows: [HFSBurstArbitrationAuditRow]

    public init(
        candidates: [ClassicAnchorCandidate],
        hfsBurstArbitrationAuditRows: [HFSBurstArbitrationAuditRow]
    ) {
        self.candidates = candidates
        self.hfsBurstArbitrationAuditRows = hfsBurstArbitrationAuditRows
    }
}

public enum MultiTrackPhase1BResolver {
    public static func resolve(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        pauseSettings: PauseDetectorSettings,
        stateSettings: StatePatternDetectorSettings,
        stagePrefix: String = "phase1b",
        additionalBlockedPauseISIIndices: Set<Int> = []
    ) -> [ClassicAnchorCandidate] {
        resolveWithAudit(
            train: train,
            candidates: candidates,
            pauseSettings: pauseSettings,
            stateSettings: stateSettings,
            stagePrefix: stagePrefix,
            additionalBlockedPauseISIIndices: additionalBlockedPauseISIIndices
        ).candidates
    }

    /// Phase 1B resolution plus a diagnostic-only HFS-vs-burst audit.
    ///
    /// Detection candidate generation is unchanged. Resolution may add fully revalidated,
    /// caller-authorized irregular-tonic continuity states before final arbitration. The
    /// audit is built after final four-track arbitration from the exact pre-protection,
    /// packet-evidence, protected, and final candidate pools.
    public static func resolveWithAudit(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        pauseSettings: PauseDetectorSettings,
        stateSettings: StatePatternDetectorSettings,
        stagePrefix: String = "phase1b",
        additionalBlockedPauseISIIndices: Set<Int> = [],
        auditSettings: HFSBurstArbitrationAuditSettings = HFSBurstArbitrationAuditSettings()
    ) -> MultiTrackPhase1BResolution {
        guard !candidates.isEmpty else {
            return MultiTrackPhase1BResolution(
                candidates: [],
                hfsBurstArbitrationAuditRows: []
            )
        }

        let initialPool = uniqueCandidatesByIdentity(candidates)
        let preliminary = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(initialPool)

        let burstLocalCompletions = tagPipelineStage(
            BurstLocalCompletionDetector.detect(
                train: train,
                candidates: preliminary,
                settings: stateSettings
            ),
            "\(stagePrefix)_burst_local_completion"
        )
        let withCompletedBursts = uniqueCandidatesByIdentity(initialPool + burstLocalCompletions)
        let burstResolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            withCompletedBursts
        )

        let pauseCompletions = tagPipelineStage(
            PauseMonotonicCompletionDetector.detect(
                train: train,
                candidates: burstResolved,
                settings: pauseSettings,
                additionalBlockedISIIndices: additionalBlockedPauseISIIndices
            ),
            "\(stagePrefix)_pause_floor_completion"
        )
        let withCompletedGaps = uniqueCandidatesByIdentity(withCompletedBursts + pauseCompletions)
        let eventGapResolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            withCompletedGaps
        )
        let selectedEvents = selectedEventCandidates(in: eventGapResolved)
        let selectedGaps = selectedGapCandidates(in: eventGapResolved)

        var effectiveStateSettings = stateSettings
        effectiveStateSettings.highFrequencySpikingInternalPacketizationPolicy =
            .multiTrackEventOverlay

        let splitStates = tagPipelineStage(
            StateEventCompatibilityResolver.splitStateCandidates(
                train: train,
                candidates: eventGapResolved,
                selectedEvents: selectedEvents,
                selectedGaps: selectedGaps,
                settings: effectiveStateSettings
            ),
            "\(stagePrefix)_state_track_split"
        )
        let withSplitStates = uniqueCandidatesByIdentity(eventGapResolved + splitStates)

        // Selected canonical events are the authoritative packet evidence.
        // Raw, duplicate, and unselected burst proposals never decide HFS
        // dominance. Applying this after state splitting lets each gap-bounded
        // HFS child be evaluated independently.
        let protectedPool = HFSpikingProtection.apply(
            to: withSplitStates,
            selectedEvents: selectedEvents,
            mode: .multiTrack
        )
        // Conservative state-level continuity: merge adjacent irregular-tonic fragments across
        // tiny non-event, non-pause gaps (revalidated). Runs after event/gap selection so it
        // can refuse selected pauses/events, and before final arbitration so the merged state
        // is chosen deterministically over its child fragments.
        // Freeze state-track authority before continuity transformation. Only states selected
        // in this pass may be merged, and state candidates that already lost this pass remain
        // audit-visible but cannot be re-promoted merely because their winning fragments were
        // consumed into one longer candidate.
        let eligibleStateIdentities = Set(
            protectedPool.lazy.filter {
                $0.arbitrationTrack == .state && $0.isEligibleForAutoSelection
            }.map(StatePatternDetector.CandidateIdentity.init)
        )
        let continuityArbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            protectedPool
        )
        let selectedStateIdentities = Set(
            continuityArbitrated.lazy.filter {
                $0.arbitrationTrack == .state &&
                    $0.selectedForAuto &&
                    $0.isEligibleForAutoSelection
            }.map(StatePatternDetector.CandidateIdentity.init)
        )
        let continuityAuthorityPool = continuityArbitrated.map { candidate in
            let identity = StatePatternDetector.CandidateIdentity(candidate)
            guard eligibleStateIdentities.contains(identity),
                  !selectedStateIdentities.contains(identity) else {
                return candidate
            }
            var frozen = candidate
            frozen.stateContinuityAuthorityFrozen = true
            return frozen
        }
        let authorizedIrregularTonicFragmentIDs = Set(
            continuityAuthorityPool.lazy.filter {
                $0.selectedForAuto &&
                    $0.isEligibleForAutoSelection &&
                    !$0.stateContinuityMergeTerminal &&
                    $0.finalLabel == .tonic &&
                    $0.stateTonicSubtype == "irregular"
            }.map(\.id)
        )
        let mergeResult = StatePatternDetector.mergeIrregularTonicMicroGapsWithAuthority(
            train: train,
            candidates: continuityAuthorityPool,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps,
            settings: effectiveStateSettings,
            authorizedFragmentIDs: authorizedIrregularTonicFragmentIDs
        )
        let mergedPool = uniqueCandidatesByIdentity(mergeResult.candidates)
        let finalSelectionPool = mergedPool.filter {
            $0.arbitrationTrack != .state ||
                $0.selectedForAuto ||
                mergeResult.generatedCandidateIdentities.contains(
                    StatePatternDetector.CandidateIdentity($0)
                )
        }
        let finalResolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            uniqueCandidatesByIdentity(finalSelectionPool)
        )
        var finalResolvedByIdentity: [
            StatePatternDetector.CandidateIdentity: ClassicAnchorCandidate
        ] = [:]
        for candidate in finalResolved {
            finalResolvedByIdentity[StatePatternDetector.CandidateIdentity(candidate)] = candidate
        }
        let finalCandidates = mergedPool.map { candidate in
            finalResolvedByIdentity[StatePatternDetector.CandidateIdentity(candidate)] ?? candidate
        }

        let effectiveAuditSettings = auditSettings.fillingMissingFallbacks(
            pauseLikeThresholdSec: effectiveStateSettings.highFrequencySpikingPauseBreakSec ??
                effectiveStateSettings.highFrequencySpikingToleratedGapSec,
            burstSeedUpperSec: effectiveStateSettings.burstSeedUpperSec,
            burstBridgeUpperSec: effectiveStateSettings.highFrequencySpikingEpochBridgeSec
        )
        let auditRows = HFSBurstArbitrationAudit.build(
            train: train,
            preProtectionCandidates: withSplitStates,
            selectedEventsUsedForPacketization: selectedEvents,
            protectedCandidates: protectedPool,
            finalCandidates: finalCandidates,
            pipelineStage: stagePrefix,
            settings: effectiveAuditSettings
        )

        return MultiTrackPhase1BResolution(
            candidates: finalCandidates,
            hfsBurstArbitrationAuditRows: auditRows
        )
    }

    private static func selectedEventCandidates(
        in candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        candidates.filter {
                $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily
        }
    }

    private static func selectedGapCandidates(
        in candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        candidates.filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .gap &&
                $0.finalLabel == .pause
        }
    }

    private static func uniqueCandidatesByIdentity(
        _ candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        var latestByIdentity: [
            StatePatternDetector.CandidateIdentity: ClassicAnchorCandidate
        ] = [:]
        var order: [StatePatternDetector.CandidateIdentity] = []
        order.reserveCapacity(candidates.count)

        for candidate in candidates {
            let identity = StatePatternDetector.CandidateIdentity(candidate)
            if latestByIdentity[identity] == nil {
                order.append(identity)
            }
            latestByIdentity[identity] = candidate
        }
        return order.compactMap { latestByIdentity[$0] }
    }

    private static func tagPipelineStage(
        _ candidates: [ClassicAnchorCandidate],
        _ stage: String
    ) -> [ClassicAnchorCandidate] {
        candidates.map { candidate in
            let tag = "pipeline_stage=\(stage)"
            guard !candidate.decisionPath.contains(tag) else {
                return candidate
            }
            let trimmed = candidate.decisionPath.trimmingCharacters(in: .whitespacesAndNewlines)
            let decisionPath = trimmed.isEmpty ? tag : "\(candidate.decisionPath);\(tag)"
            return candidate.withDiagnosticOverride(
                decisionPath: decisionPath,
                selectedForAuto: candidate.selectedForAuto,
                selectionStatus: candidate.selectionStatus
            )
        }
    }
}
