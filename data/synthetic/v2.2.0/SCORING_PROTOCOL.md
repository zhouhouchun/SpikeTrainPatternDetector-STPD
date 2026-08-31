# Scoring protocol v2.2.0

Report 1x, 4x and 10x separately. Use Template_ID as the paired/cluster-bootstrap unit; never treat the three scale projections as independent templates.

Report two estimands in parallel:

1. Strict mechanism: all injected Burst/Pause events and all generated Tonic/Broad HFS States.
2. Observable phenotype: timestamp-eligible Tonic States and the frozen Burst phenotype evidence rules, with ambiguous and no-evidence coverage disclosed.

For each target report ISI-support precision/recall/F1, episode IoU precision/recall/F1, boundary error and fragmentation/merge rates. Background is an explicit negative region: any target prediction there is an FP. Contextual separators are scored separately and excluded from primary Pause macro-averages. Broad HFS direct support, outer State envelope and boundary observability are separate outputs; left/right HFS States across canonical Pause are scored separately even when related by one Composite HFS Regime.

Primary comparisons are: frozen-detector v2.1 versus v2.2, separately recalibrated workflows using the same development templates, and generator-only phenotype/boundary audits. An increase in STPD F1 is not a generator acceptance criterion.
