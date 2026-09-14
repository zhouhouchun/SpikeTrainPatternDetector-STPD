# PS and RGS Shiny integration contract

Poisson Surprise (PS) and Robust Gaussian Surprise (RGS) are auxiliary
support methods. Their Shiny controls call the same package-core functions as
scripted analyses. Neither method writes, replaces, or silently promotes STPD
`AUTO` labels.

## Run identity and stale results

Each explicit PS or RGS run records the dataset identifier, selected train
scope, selected-scope timestamp hash, method parameters, reference-definition
hash, package version, code commit, run identifier, and UTC timestamp. A result
is withdrawn from tables, overlays, and downloads when the dataset, selected
trains, timestamps, parameters, calibration set, or RGS group definition no
longer matches the recorded run.

The interface distinguishes `not_run`, `running`, `success`, `zero_events`,
`not_estimable`, `stale`, and `error`. Zero events are therefore not displayed
as a computation failure.

## RGS reference modes

`same_data` fits and applies the reference on the same selected train scope and
is labelled exploratory. `held_out` requires declared calibration trains and
excludes them from prediction targets. Reference groups may be a single group,
a stable train-metadata field, an uploaded CSV, or an editable train-group
table. Every selected train must have exactly one non-empty group assignment.

RGS keeps the candidate-count probability and the family-wise alpha as separate
parameters. Publication mode fails closed when its reference is not estimable;
the reference diagnostics remain the primary interpretation record.

## Displays and exports

The support raster can overlay Mean-ISI, LogISI/newBD, PS, and RGS Burst
intervals with distinct colours and offsets. Only current results are shown.
PS and RGS downloads are created in unique temporary workspaces, contain stable
empty tables when no events are detected, and are validated for required
members before delivery. The default RGS RDS is compact and does not duplicate
raw spike timestamps.
