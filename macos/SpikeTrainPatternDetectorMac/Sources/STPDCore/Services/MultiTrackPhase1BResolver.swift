import Foundation

/// Ordered Phase 1B compatibility pass for event, gap, state, and review tracks.
///
/// The order is intentional:
/// 1. select events and initial gaps;
/// 2. complete the monotonic pause floor;
/// 3. freeze Burst topology and adjudicate contextual/inter-burst gaps;
/// 4. reselect authoritative event/gap tracks independently of state;
/// 5. split states at selected gap/event boundaries and consume spanning parents;
/// 6. resolve HFS packet dominance from selected canonical events;
/// 7. enforce Tonic-family/HFS direct-support authority after removing frozen interruptions;
/// 8. merge caller-authorized irregular-tonic fragments with full-span revalidation;
/// 9. re-project support on generated envelopes and perform final four-track arbitration.
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

        // Pause completion is calibrated from the event/gap tracks before any
        // state candidate can out-rank established pause evidence. A selected
        // canonical Pause is a hard state boundary; a selected brief interruption
        // remains an internal state gap. Both remain available as monotonic-floor
        // evidence even when an unsplit state parent overlaps them.
        let pauseCompletionAuthority = ClassicAnchorCandidateArbitrator.arbitrate(
            withCompletedBursts
        )

        let pauseCompletions = tagPipelineStage(
            PauseMonotonicCompletionDetector.detect(
                train: train,
                candidates: pauseCompletionAuthority,
                settings: pauseSettings,
                additionalBlockedISIIndices: additionalBlockedPauseISIIndices
            ),
            "\(stagePrefix)_pause_floor_completion"
        )
        let withCompletedGaps = uniqueCandidatesByIdentity(withCompletedBursts + pauseCompletions)
        // Event/gap authority must be established before state competition. In
        // particular, a selected canonical Pause cannot first be deselected merely
        // because an unsplit state parent overlaps it. Brief interruptions remain
        // gap evidence without acquiring hard-boundary authority.
        // Freeze event topology after Burst local completion and ordinary Pause
        // completion. Contextual/inter-burst Pause reads this topology once and
        // cannot feed back into Burst core generation, thresholds, or completion.
        let preContextEventGapAuthority = ClassicAnchorCandidateArbitrator.arbitrate(
            withCompletedGaps
        )
        let frozenBurstEvents = selectedEventCandidates(in: preContextEventGapAuthority)
        let contextualInterburstGaps = tagPipelineStage(
            ContextualInterburstPauseDetector.detect(
                train: train,
                frozenBurstCandidates: frozenBurstEvents,
                settings: pauseSettings,
                existingCandidates: preContextEventGapAuthority
            ),
            "\(stagePrefix)_contextual_interburst_pause"
        )
        let withContextualGaps = uniqueCandidatesByIdentity(
            withCompletedGaps + contextualInterburstGaps
        )
        let eventGapAuthority = ClassicAnchorCandidateArbitrator.arbitrate(
            withContextualGaps
        )
        let selectedEvents = selectedEventCandidates(in: eventGapAuthority)
        let selectedGaps = selectedGapCandidates(in: eventGapAuthority)
        let eventGapResolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            withContextualGaps
        )

        var effectiveStateSettings = stateSettings
        effectiveStateSettings.highFrequencySpikingInternalPacketizationPolicy =
            .multiTrackEventOverlay

        let splitResolution = StateEventCompatibilityResolver.resolveStateCandidates(
            train: train,
            candidates: eventGapResolved,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps,
            settings: effectiveStateSettings
        )
        let splitStates = tagPipelineStage(
            splitResolution.fragments,
            "\(stagePrefix)_state_track_split"
        )
        let boundaryConsumedPool = markingHardBoundaryConsumedStates(
            eventGapResolved,
            resolution: splitResolution,
            stage: "\(stagePrefix)_state_track_split"
        )
        let withSplitStates = uniqueCandidatesByIdentity(boundaryConsumedPool + splitStates)

        // Selected canonical events are the authoritative packet evidence.
        // Raw, duplicate, and unselected burst proposals never decide HFS
        // dominance. Applying this after state splitting lets each gap-bounded
        // HFS child be evaluated independently.
        let protectedPool = HFSpikingProtection.apply(
            to: withSplitStates,
            selectedEvents: selectedEvents,
            mode: .multiTrack
        )
        let supportAuthorizedPool = StateSupportAuthorityResolver.apply(
            to: protectedPool,
            train: train,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps,
            settings: effectiveStateSettings
        )
        // Conservative state-level continuity: merge adjacent irregular-tonic fragments across
        // tiny non-event gaps (revalidated). Runs after event/gap selection so it can refuse
        // canonical Pause anchors and selected events while permitting a bounded brief
        // interruption. It runs before final arbitration so the merged state is chosen
        // deterministically over its child fragments.
        // Freeze state-track authority before continuity transformation. Only states selected
        // in this pass may be merged, and state candidates that already lost this pass remain
        // audit-visible but cannot be re-promoted merely because their winning fragments were
        // consumed into one longer candidate.
        let eligibleStateIdentities = Set(
            supportAuthorizedPool.lazy.filter {
                $0.arbitrationTrack == .state && $0.isEligibleForAutoSelection
            }.map(StatePatternDetector.CandidateIdentity.init)
        )
        let continuityArbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            supportAuthorizedPool
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
        let finalSupportPool = StateSupportAuthorityResolver.apply(
            to: mergedPool,
            train: train,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps,
            settings: effectiveStateSettings
        )
        let finalSelectionPool = finalSupportPool.filter {
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
        let finalCandidates = finalSupportPool.map { candidate in
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
            protectedCandidates: supportAuthorizedPool,
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

    private static func markingHardBoundaryConsumedStates(
        _ candidates: [ClassicAnchorCandidate],
        resolution: StateEventCompatibilityResolution,
        stage: String
    ) -> [ClassicAnchorCandidate] {
        guard !resolution.consumedStateCandidateIdentities.isEmpty else {
            return candidates
        }

        return candidates.map { candidate in
            let identity = StatePatternDetector.CandidateIdentity(candidate)
            guard resolution.consumedStateCandidateIdentities.contains(identity) else {
                return candidate
            }

            let boundaryIDs = resolution
                .boundaryCandidateIDsByConsumedStateCandidateIdentity[identity] ?? []
            let tokens = [
                "state_hard_boundary_consumed=true",
                "state_hard_boundary_ids=\(boundaryIDs.isEmpty ? "none" : boundaryIDs.joined(separator: ","))",
                "pipeline_stage=\(stage)_parent_consumed"
            ]
            let existingTokens = Set(
                candidate.decisionPath
                    .split(separator: ";")
                    .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
            )
            let additions = tokens.filter { !existingTokens.contains($0) }
            let decisionPath = additions.reduce(candidate.decisionPath) { partial, token in
                let trimmed = partial.trimmingCharacters(in: .whitespacesAndNewlines)
                return trimmed.isEmpty ? token : "\(partial);\(token)"
            }
            var consumed = candidate.withDiagnosticOverride(
                decisionPath: decisionPath,
                selectedForAuto: false,
                selectionStatus: "not_selected__state_parent_consumed_by_hard_boundary"
            )
            // Reuse the typed pre-continuity authority lock: a consumed parent
            // remains audit-visible but can never be reselected or merged later.
            consumed.stateContinuityAuthorityFrozen = true
            return consumed
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
