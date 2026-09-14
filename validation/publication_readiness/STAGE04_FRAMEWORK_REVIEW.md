# Stage 04 framework review — AUTO v3 multitrack product

Date: 2026-08-30
Scope: product schema only; no official AUTO promotion.

## Decision

PASS. The minimum AUTO product uses separate Event, State and Gap tables;
`per_isi` is a deterministic projection rather than a second source of truth.
Candidate/evidence tables precede episodes and segments in the frozen hash DAG,
and parent identifiers bind all materialized rows to one detection root.

## Stop-check review

- Burst/Pause Events and Tonic/Broad-HFS States are not mutually exclusive
  tracks.
- Predicted Pause, QC exclusions and hard boundaries cannot be active State
  support.
- Connector rows retain episode membership but have null active-State fields.
- Detached byte integrity cannot claim execution authenticity or release
  authority.
- Missing parent, foreign key, canonical sort or manifest closure fails closed.

No AUTO detector run or official product was created. P0/P1/P2 framework
defects found: 0/0/0. Stage 5 may begin.
