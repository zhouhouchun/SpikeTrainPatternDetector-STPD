# Data included in the public release

## Synthetic benchmark data

`data/synthetic/` contains frozen v2.2.0 and v2.3.0 benchmark snapshots. Each
snapshot includes the R generator, parameters, blinded detector inputs,
multitrack truth tables, quality-control tables, checksums, and R-generated QC
figures. The 1x, 4x, and 10x projections are paired time-scale transforms of the
same dimensionless templates and must not be treated as independent biological
replicates.

## Real manual reference labels and public metadata

`data/real/` contains the publicly cleared GPe, STN, and GPi manual ISI-label
workbooks under region-only filenames, together with the frozen structural
summary, byte hashes, eligibility counts, and source/ethics record. In this
frozen snapshot:

- GPe: 16 spike-train worksheets;
- STN: 23 spike-train worksheets;
- GPi: 23 spike-train worksheets;
- each worksheet is a flat, formula-free table;
- a string scan found no direct name, date-of-birth, address, telephone,
  e-mail, passport, or medical-record-number fields.

The worksheet identifiers are experimental recording identifiers, not names.
Blank ISI labels are interpreted as `other` only where the frozen validation
protocol explicitly states this. The release builder includes the unchanged
frozen workbooks by default. A code-and-synthetic-only bundle can be generated
by setting `STPD_INCLUDE_REAL_DATA=0`.

## Redistribution checkpoint

Public release was authorized by the repository owner, Zhou Houchun, on
2026-08-31. Public availability does not waive the requirement to cite the
software and clinical-data context, and it does not by itself grant a separate
licence for unrelated redistribution or commercial reuse.
