# Stage 06 framework review — evidence, export, UI, migration and signing

Date: 2026-08-30
Scope: delivery architecture only; no official export or signature.

## Decision

PASS. The framework orders evidence coverage before atomic generation export,
requires version-negotiating consumers, separates candidate inspection from
official access, and leaves cryptographic release authority outside detector
execution.

## Stop-check review

- Missing required contract evidence blocks promotion.
- Tables are staged and verified as one generation; partial/stale mixtures fail
  closed and cannot replace the last valid generation.
- v2 data cannot be guessed into v3 because connector provenance is absent;
  redetection is required.
- UI labels candidate, abstained and official products distinctly and must not
  imply biological truth through display color.
- Attestation prototypes contain hashes and approval slots but no embedded
  private key or self-approval path.

Production signing and extended interoperability formats remain deferred.
P0/P1/P2 framework defects found: 0/0/0. Stage 7 may begin.
