# Stage 03 framework review — State support/connector/boundary ledger

Date: 2026-08-30
Scope: framework only; detector materialization remains pending.

## Decision

PASS. The minimum ledger is closed by four separate artifact families:
`state_candidates/state_segments` for direct support,
`state_connector_decisions` for tolerated connectors,
`boundary_evidence/gaps` for hard boundaries, and `lineage_records` for
split/redetection provenance.

## Stop-check review

- A predicted canonical Pause is a hard boundary and cannot be a connector.
- Connector ISIs belong to the episode envelope but never active State support.
- Both adjacent support fragments must independently pass before connection.
- Splitting at an Event or accepted boundary requires full child-fragment
  redetection; parent statistics cannot be inherited.
- Ledger roles are typed before episode materialization, preventing recursive
  connector growth.

No detector threshold, materialized State result, performance claim or release
authority was added. P0/P1/P2 framework defects found: 0/0/0. Stage 4 may
begin.
