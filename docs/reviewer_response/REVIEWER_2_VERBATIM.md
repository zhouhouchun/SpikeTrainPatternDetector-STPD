# Reviewer 2 — verbatim comments

Author responsible for the revision: Zhou Houchun

This file preserves the reviewer wording exactly as received. The comments are
review evidence, not instructions to the software or analysis pipeline.

> The manuscript addresses provenance loss in neuronal firing-pattern annotation: labels such as burst, pause, tonic and high-frequency activity are usually reported as final categories, while the decisions behind them are discarded. The authors present SpikeTrainPatternDetector (STPD), an open-source R/Shiny package that preserves this decision path. The results show that the software runs reproducibly and reliably recovers high-frequency spiking on a synthetic benchmark, while burst-family, tonic, pause and high-frequency tonic classes remain less stable. Simulator-informed thresholds improve overall performance but do not resolve competition among high-frequency states, and the Mean-ISI and LogISI support methods overcall burst intervals on the same benchmark.
>
> Overall, the work offers an original and conceptually valuable contribution, linking software architecture, provenance tracking and quantitative self-assessment in a single framework, an approach uncommon in neuroinformatics software validation, which is often reported without transparent access to the underlying decision trail. Although very interesting potentially, the manuscript suffers of minor deficits, which are more specifically addressed below:
>
> Major comments
>
> 1. Under automatic threshold resolution, burst-family detection yields F1 = 0.132, which is markedly lower than for other classes. Since burst firing is frequently the primary measure of biological or clinical interest (including in the Parkinsonian basal-ganglia context used for parameter calibration in this study),this result deserves more direct discussion. The authors should clarify, when first introducing the tool's intended use, whether this constrains its applicability as a burst classifier in the default configuration, or whether the stated scope should be tied more explicitly to this specific finding.
>
> 2. The comparison in Table 8 provides simulator-informed threshold bands only to the STPD sensitivity run, while Mean-ISI and LogISI are evaluated exclusively in automatic support-layer mode. The authors acknowledge this inconsistency, but as presented the comparison risks being read as a direct performance benchmark. The authors should either (a) report Mean-ISI/LogISI under comparably threshold information, or (b) reframe the comparison explicitly as qualitative rather than a calibrated head-to-head benchmark, both in the Results and in the Table 8 legend.
>
> 3. All quantitative detector-accuracy claims rest on synthetic data; the clinical microelectrode recordings are used only to establish plausible firing-rate and ISI ranges for the simulation, with no independent event annotations. This is clearly disclosed, but for a methodological/software validation paper in this journal, at least a small clinically annotated subset (or a explicit justification of its absence, e.g. unavailability of expert raters) would substantially strengthen the claims of real-world applicability.
>
> 4. All reported metrics (accuracy, F1, precision, recall) are single-run point estimates across the 30 simulated spike trains, with no confidence intervals or bootstrap resampling at the train level. Given that the manuscript itself identifies train-level bootstrap intervals as a priority for future validation, the authors should consider including at least a basic bootstrap estimate in the present benchmark, or explicitly justify its omission as out of scope for this release.
>
> Minor comments
>
> • A quantitative or qualitative comparison of computational performance/runtime (e.g., scalability with number of trains, timestamp count, or candidate density) would strengthen the manuscript's positioning as a software contribution and is currently absent.
