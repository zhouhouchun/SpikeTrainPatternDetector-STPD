import Foundation

/// An external task / stimulus timestamp parsed from an `event`-style CSV column. A task event is an
/// annotation layer: it is NOT a spike train and must never participate in burst/pause/tonic detection,
/// QC, manual annotation, reviewed-CSV export, or neural PCA coordinates. Mirrors the R task-event model
/// (`R/03_data_io.R`: `stpd_extract_task_events_from_data_frame` / `stpd_normalize_task_events`).
public struct TaskEvent: Identifiable, Hashable, Sendable {
    /// Stable unique id (R `event_id`).
    public let id: String
    /// Display name, cleaned from the source column (R `event_name`); empty → "Event".
    public let name: String
    /// Event time in seconds, scaled with the same unit as spike timestamps (R `event_time_sec`).
    public let timeSec: Double
    /// The originating (cleaned) CSV column name (R `event_column`).
    public let column: String
    /// 1-based data-row index of the finite cell this event came from (R `event_index`).
    public let eventIndex: Int
    /// Stable unique trial id (R `trial_id`).
    public let trialID: String
    /// Provenance (file/source description) (R `source`).
    public let source: String

    public init(
        id: String,
        name: String,
        timeSec: Double,
        column: String,
        eventIndex: Int,
        trialID: String,
        source: String
    ) {
        self.id = id
        self.name = name
        self.timeSec = timeSec
        self.column = column
        self.eventIndex = eventIndex
        self.trialID = trialID
        self.source = source
    }
}
