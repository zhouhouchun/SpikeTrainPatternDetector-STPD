# Data included in the public release

## Synthetic benchmark data

`data/synthetic/` contains frozen v2.2.0 and v2.3.0 benchmark snapshots. Each
snapshot includes the R generator, parameters, blinded detector inputs,
multitrack truth tables, quality-control tables, checksums, and R-generated QC
figures. The 1x, 4x, and 10x projections are paired time-scale transforms of the
same dimensionless templates and must not be treated as independent biological
replicates.

## Real manual reference labels and public metadata

The local validation snapshot contains GPe, STN, and GPi manual ISI-label
workbooks. The default public bundle does **not** distribute these patient-level
workbooks. `data/real/` instead contains the frozen structural summary, byte
hashes, eligibility counts, and the source/ethics/redistribution record. In the
local frozen snapshot:

- GPe: 16 spike-train worksheets;
- STN: 23 spike-train worksheets;
- GPi: 23 spike-train worksheets;
- each worksheet is a flat, formula-free table;
- a string scan found no direct name, date-of-birth, address, telephone,
  e-mail, passport, or medical-record-number fields.

The worksheet identifiers are experimental recording identifiers, not names.
Blank ISI labels are interpreted as `other` only where the frozen validation
protocol explicitly states this. If redistribution is later authorized, the
release builder can include the unchanged frozen workbooks under region-only
public filenames by setting `STPD_INCLUDE_REAL_DATA=1`.

## Redistribution checkpoint

Patient-level workbooks may be added only after the repository owner confirms
the redistribution terms for the source biological recordings and derived
manual labels. Until then, public reproducibility relies on the synthetic
benchmarks, aggregate real-data metrics, frozen eligibility metadata, and the
published analysis code.
