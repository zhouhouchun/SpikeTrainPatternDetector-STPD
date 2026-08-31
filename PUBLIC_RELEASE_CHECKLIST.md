# Public release checklist

- [x] Public author display standardized to `Zhou Houchun`.
- [x] Detector source, package metadata, tests, and evaluation scripts included.
- [x] Synthetic v2.2.0 and v2.3.0 generator source, inputs, truth, and QC included.
- [x] GPe/STN/GPi manual-label workbooks structurally audited and excluded from the default public bundle pending documented redistribution clearance.
- [x] Compact real and synthetic validation result tables included.
- [x] Reproducible runtime benchmark, workload table, environment metadata, and R figures included.
- [x] Every distributed scientific figure mapped to an R generation script.
- [x] Generated binary objects and files larger than GitHub's 100 MB limit excluded.
- [x] `R CMD build` and installation/load/document/example checks completed with status OK.
- [x] Core Burst/Pause/HFS hierarchy and coexistence tests passed on the public snapshot.
- [x] Freeze real-data annotation bytes and eligibility; publish region-only workbook names without `draft` or personal surnames.
- [x] Record the manuscript source and ethics/consent basis in `data/real/SOURCE_AND_LICENSE.md`.
- [ ] Document institutional redistribution authorization and a licence before publicly distributing patient-level workbooks.
- [x] Run the full R regression suite on the final source snapshot (141 files, 999 test blocks, 0 failures/errors/warnings; 1 optional fresh-baseline test skipped by design).
- [x] Run the public-bundle scanner and verify the SHA-256 manifest.
- [ ] Create the public GitHub repository and push only the clean release history.
