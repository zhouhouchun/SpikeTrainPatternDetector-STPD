# Simulation warning interpretation

The rows in `metadata/Simulation_Warnings.csv` are retained for auditability. In this dataset they are non-fatal boundary notes rather than failed train generation.

Observed warning classes:

- `No feasible interval label with a positive ratio...`: the generator reached the end of the requested 30 s recording window and no additional interval could be inserted without violating duration, interval-range, or HF/burst adjacency constraints. Generation stops rather than forcing an invalid interval.
- `Achieved duration ... is shorter than requested duration 30.0000 s`: the final spike can occur before 30 s. The recording duration remains 30 s; the silent right tail is right-censored and is not an inter-spike interval because no later spike exists.
- `The first real spike was generated after a positive initial latency...`: t = 0 is the recording boundary, not an artificial spike. Initial silence is not counted as an ISI.
- `Initial Pause was treated as leading silence...`: a pause at the very beginning is represented as leading latency, not as a biological ISI between two observed spikes.

The acceptance checks in `metadata/Quality_Audit.csv` and `metadata/Dataset_Summary.csv` are the primary pass/fail indicators. For this package, all spike ordering, interval count, refractory, noisy/clean label, and HF/burst adjacency checks pass.
