public enum CanonicalScientificImportProjectionError: Error, Equatable, Sendable {
    case missingSourceTransactionBinding
    /// The assembled dataset-global spike-train registry and its group references are not a strict,
    /// fully-referenced, canonically ordered one-group-per-train partition, so no fingerprint may be
    /// produced for it. (Complete scientific validity is established upstream by the independent
    /// validator that produces `CanonicalProjectionValidatedImport`.)
    case invalidRegistryPartition(CanonicalRegistryPartitionError)
}

/// A canonical scientific value retained strictly for shadow comparison and later identity work.
/// Possession of this value grants no detector, result-package, review, or export authority.
public struct ShadowCanonicalScientificImport: Hashable, Sendable {
    public let dataset: CanonicalScientificDataset
    /// A deterministic, non-authoritative content fingerprint of `dataset`.
    public let fingerprint: CanonicalScientificDatasetFingerprint
    public let sourceTransactionBinding: StagedSourceTransactionBinding

    internal init(
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        sourceTransactionBinding: StagedSourceTransactionBinding
    ) {
        self.dataset = dataset
        self.fingerprint = fingerprint
        self.sourceTransactionBinding = sourceTransactionBinding
    }
}

/// Projects only independently replay-validated scientific content into a dataset-global canonical
/// model and its non-authoritative fingerprint. Presentation and source-layout facts are excluded
/// from the dataset; the exact source transaction binding is retained beside it but never enters the
/// canonical digest.
public enum CanonicalScientificImportProjector {
    public static func project(
        _ validated: CanonicalProjectionValidatedImport
    ) throws -> ShadowCanonicalScientificImport {
        let prepared = validated.preparedImport
        guard let sourceTransactionBinding = prepared.provenance.resolvedPlan.source
            .sourceTransactionBinding else {
            throw CanonicalScientificImportProjectionError.missingSourceTransactionBinding
        }

        let registry = projectGlobalSpikeTrainRegistry(prepared.data.eventScopeGroups)
        let groups = prepared.data.eventScopeGroups
            .map(projectGroup)
            .sorted {
                semanticText($0.semanticID).utf8.lexicographicallyPrecedes(
                    semanticText($1.semanticID).utf8
                )
            }
        let definitions = prepared.data.scientificAttributeDefinitions
            .map(projectAttributeDefinition)
            .sorted {
                $0.key.canonicalText.utf8.lexicographicallyPrecedes($1.key.canonicalText.utf8)
            }

        let dataset = CanonicalScientificDataset(
            activityMode: prepared.data.activityMode,
            spikeTrains: registry,
            eventScopeGroups: groups,
            scientificAttributeDefinitions: definitions
        )

        let fingerprint: CanonicalScientificDatasetFingerprint
        do {
            fingerprint = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
        } catch let error as CanonicalRegistryPartitionError {
            throw CanonicalScientificImportProjectionError.invalidRegistryPartition(error)
        }

        return ShadowCanonicalScientificImport(
            dataset: dataset,
            fingerprint: fingerprint,
            sourceTransactionBinding: sourceTransactionBinding
        )
    }

    /// Builds each canonical spike train and orders the dataset-global registry by canonical
    /// semantic ID. Any duplicate or unreferenced entry is rejected by the fingerprinter's
    /// registry/partition check before a digest is produced.
    private static func projectGlobalSpikeTrainRegistry(
        _ groups: [PreparedEventScopeGroup]
    ) -> [CanonicalSpikeTrain] {
        groups
            .flatMap(\.spikeTrains)
            .map { CanonicalSpikeTrain(semanticID: $0.semanticID, rawTimestamps: $0.timestamps) }
            .sorted {
                semanticText($0.semanticID).utf8.lexicographicallyPrecedes(
                    semanticText($1.semanticID).utf8
                )
            }
    }

    private static func projectGroup(
        _ group: PreparedEventScopeGroup
    ) -> CanonicalEventScopeGroup {
        let references = group.spikeTrains
            .map(\.semanticID)
            .sorted {
                semanticText($0).utf8.lexicographicallyPrecedes(semanticText($1).utf8)
            }
        let eventDefinitions = group.eventDefinitions
            .map(projectEventDefinition)
            .sorted {
                semanticText($0.semanticID).utf8.lexicographicallyPrecedes(
                    semanticText($1.semanticID).utf8
                )
            }

        let timeBasis: CanonicalEventScopeTimeBasis
        switch group.timeBasis {
        case .recordingElapsed:
            timeBasis = .recordingElapsed
        case .eventRelative(let origin):
            timeBasis = .eventRelative(
                origin: CanonicalEventOrigin(
                    eventDefinitionID: origin.eventDefinitionID,
                    tick: origin.tick,
                    scientificAttributes: projectAttributes(origin.scientificAttributes)
                )
            )
        }

        return CanonicalEventScopeGroup(
            semanticID: group.semanticID,
            timeBasis: timeBasis,
            spikeTrainReferences: references,
            eventDefinitions: eventDefinitions
        )
    }

    private static func projectEventDefinition(
        _ definition: PreparedEventDefinition
    ) -> CanonicalEventDefinition {
        let occurrences = definition.occurrences.map { occurrence in
            CanonicalEventOccurrence(
                tick: occurrence.tick,
                scientificAttributes: projectAttributes(occurrence.scientificAttributes)
            )
        }.sorted(by: occurrenceIsOrderedBefore)

        return CanonicalEventDefinition(
            semanticID: definition.semanticID,
            eventTypeID: definition.eventTypeID,
            occurrences: occurrences
        )
    }

    private static func projectAttributeDefinition(
        _ definition: ResolvedEventAttributeDefinitionPlan
    ) -> CanonicalEventAttributeDefinition {
        CanonicalEventAttributeDefinition(
            key: definition.key,
            scalarType: definition.scalarType,
            unit: definition.unit,
            emptyStringPolicy: definition.emptyStringPolicy
        )
    }

    private static func projectAttributes(
        _ attributes: [PreparedEventAttribute]
    ) -> [CanonicalEventAttributeValue] {
        attributes
            .map { CanonicalEventAttributeValue(key: $0.key, value: $0.value) }
            .sorted {
                $0.key.canonicalText.utf8.lexicographicallyPrecedes($1.key.canonicalText.utf8)
            }
    }

    private static func semanticText(_ id: ScientificEventScopeGroupID) -> String {
        id.semanticID.canonicalText
    }

    private static func semanticText(_ id: ScientificSpikeTrainID) -> String {
        id.semanticID.canonicalText
    }

    private static func semanticText(_ id: ScientificEventDefinitionID) -> String {
        id.semanticID.canonicalText
    }

    /// Same-tick occurrences are a scientific multiset distinguished by their Scientific
    /// attributes, not by source-row position. This ordering removes that provenance-only tie
    /// while retaining every occurrence, including exact scientific duplicates for validation.
    private static func occurrenceIsOrderedBefore(
        _ lhs: CanonicalEventOccurrence,
        _ rhs: CanonicalEventOccurrence
    ) -> Bool {
        if lhs.tick != rhs.tick { return lhs.tick < rhs.tick }
        return attributesAreOrderedBefore(
            lhs.scientificAttributes,
            rhs.scientificAttributes
        )
    }

    private static func attributesAreOrderedBefore(
        _ lhs: [CanonicalEventAttributeValue],
        _ rhs: [CanonicalEventAttributeValue]
    ) -> Bool {
        for (left, right) in zip(lhs, rhs) {
            if left.key != right.key {
                return left.key.canonicalText.utf8.lexicographicallyPrecedes(
                    right.key.canonicalText.utf8
                )
            }
            if left.value != right.value {
                return attributeValueIsOrderedBefore(left.value, right.value)
            }
        }
        return lhs.count < rhs.count
    }

    private static func attributeValueIsOrderedBefore(
        _ lhs: EventAttributeValue,
        _ rhs: EventAttributeValue
    ) -> Bool {
        let left = attributeValueSortKey(lhs)
        let right = attributeValueSortKey(rhs)
        if left.tag != right.tag { return left.tag < right.tag }
        return left.text.utf8.lexicographicallyPrecedes(right.text.utf8)
    }

    private static func attributeValueSortKey(
        _ value: EventAttributeValue
    ) -> (tag: Int, text: String) {
        switch value {
        case .string(let value):
            return (0, value.canonicalText)
        case .integer(let value):
            return (1, value.canonicalText)
        case .exactDecimal(let value):
            return (2, value.canonicalText)
        case .boolean(let value):
            return (3, value ? "1" : "0")
        }
    }
}
