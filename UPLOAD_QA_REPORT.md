# Upload-package quality assurance report

Author: Zhou Houchun
Prepared: 2026-09-01

## Release identity

- Source checkout: public tag `v1.2.2-rev1`
- Source commit: `77e53daeead789399db259c54a4e36c40d20e703`
- Frozen algorithm manifest: 128 files
- Frozen algorithm verification after packaging: `128/128` SHA-256 matches
- Detector-source changes introduced by packaging: none

## Checks completed

| Check | Result |
|---|---|
| Parse all distributed R files | PASS |
| `git diff --check` | PASS |
| Targeted publication-validation path/SHA and HFS semantic tests | PASS |
| `R CMD build` | PASS |
| Install built source package into an isolated temporary library | PASS |
| Load installed package and read version 1.2.2 | PASS |
| Synthetic v2.2 release-contract verification | PASS |
| Synthetic v2.3 release-lock and contract verification | PASS |
| Re-run all manuscript figure scripts from repository-relative paths | PASS |
| Verify real-workbook SHA-256 values against annotation freeze | PASS |
| Reconstruct public STN runtime input from workbook | PASS |
| Compare reconstructed input with historical normalized benchmark input | PASS: 23 trains and all 16,728 spike timestamps identical; column order may differ |
| Scan public distributed filenames for personal surnames | PASS: all real workbooks use region-only names |
| Scan for assistant- or platform-specific working traces | PASS: none found |
| Scan for actual local home-directory, Desktop, temporary-item and attachment paths | PASS, with only deliberate synthetic redaction-test fixtures retained |
| Scan for `.DS_Store`, editor backups, `.Rhistory`, `.RData`, and files >90 MB | PASS: none found |
| Verify that the upload manifest is limited to Git-tracked release files | PASS: ignored local compiler outputs (`.o`/`.so`) are excluded |

## Deliberate test-fixture exceptions

Several frozen unit-test/config fixtures contain fictional paths such as
`/Users/a/raw.csv` or `/Users/example/private`. They test that exported audit
records redact host paths. They do not identify the author, participant, or
current computer and remain inside the authoritative frozen algorithm/test
evidence.

One frozen diagnostic launcher retains the historical study-key string
`real_grechishnikova_2017...` as its default ignored output-directory name.
It is not a distributed patient file or participant identifier and was not
changed because doing so would alter the authoritative 128-file algorithm
freeze. All distributed real-data filenames are region-only.

## Scope of this QA

The complete 999-block regression suite was not rerun merely to reorganize the
public materials because no frozen algorithm file changed. Its successful
results remain under `results/release_freeze/`. The directly affected
publication-path test was rerun after anonymizing workbook paths and passed.

The upload manifest is generated from files tracked by Git. Local compiler
outputs can remain in a developer checkout, but they are ignored by Git, absent
from GitHub/Zenodo source archives, and intentionally excluded from the public
SHA-256 manifest.

The data workbooks remain single-expert frozen references. Their inclusion
supports reproducibility and agreement analysis; it does not convert them into
an independently collected external validation cohort.

## Upload decision

The folder is technically ready for author review, commit, and GitHub upload.
Before publishing a new release, verify that the manuscript and reviewer
response cite the same final commit or tag and retain the same wording for:

1. recording-group-held-out calibration;
2. same-data resubstitution as a sensitivity analysis;
3. real-reference agreement rather than external validation;
4. the separate software and human-derived-data use terms.
