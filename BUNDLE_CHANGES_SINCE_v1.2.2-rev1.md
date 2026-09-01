# Packaging changes after `v1.2.2-rev1`

The upload directory was created from public tag `v1.2.2-rev1` at commit
`77e53daeead789399db259c54a4e36c40d20e703`. The following additions or
clarifications were made without changing any file in the authoritative
128-file algorithm freeze:

1. Added portable R source and generated outputs for manuscript Figures 1–6.
2. Added a label-blind converter from the public STN workbook to the wide CSV
   used by the runtime benchmark, and changed the benchmark's default input to
   that public derived file.
3. Corrected the data-publication provenance to identify the N. N. Burdenko
   source institution, Alexey Sedov's source-data curation/public-dissemination
   role, and Zhou Houchun's repository-preparation role.
4. Added explicit data-use terms separating human-derived workbooks from the
   MIT software licence.
5. Harmonized the description of full-parameter same-data resubstitution as a
   sensitivity analysis, not a guaranteed performance upper bound.
6. Updated citation metadata for the reviewer-evidence revision and added
   upload instructions, QA evidence, and a complete upload manifest.
7. Restricted the upload manifest to Git-tracked release files so ignored local
   compiler outputs cannot be listed as if they were part of GitHub/Zenodo
   source archives.

Any future detector-source change requires a new algorithm freeze, regression
run, and evidence tag. These packaging changes do not make such a change.
