# Real-data source, ethics, and public-release provenance

The workbooks in this directory contain de-identified spike timestamps and
frozen single-expert ISI annotations used for agreement analyses. They did not
participate in fitting train-specific parameters. The same frozen references
were used only at the final validation stage. During late software review,
their aggregate firing-pattern ranges informed coarse inspection of general
defaults such as contrast settings; no record-specific threshold fitting was
performed. The reported results must therefore be read as agreement with these
particular references, not as an independently collected external validation
cohort.

| Region | Recording source and grouping | Ethics and consent | Public-release provenance |
|---|---|---|---|
| GPe | Intraoperative microelectrode recording during standard-of-care bilateral DBS implantation at the N. N. Burdenko National Medical Research Center of Neurosurgery, Moscow, Russia. GPe and GPi derive from the same pallidal participant/dataset. | The original clinical protocol was approved by the institutional Ethics Committee and written informed consent was obtained. The present work is a retrospective software analysis of de-identified timestamps; no research-specific trajectory, recording session, stimulation procedure, or invasive intervention was added. | The source institution is the N. N. Burdenko National Medical Research Center of Neurosurgery. Alexey Sedov led the curation and public dissemination of the source data. Zhou Houchun prepared, audited, and documented the repository copy. |
| STN | Intraoperative microelectrode recording during standard-of-care bilateral DBS implantation at the same institution. One participant contributed bilateral STN recordings; the 23 trains are not 23 independent patients. | Same statement as above. | Same statement as above. |
| GPi | Intraoperative microelectrode recording during standard-of-care bilateral DBS implantation at the same institution. GPe and GPi derive from the same pallidal participant/dataset. | Same statement as above. | Same statement as above. |

Direct identifiers were removed before analysis. The workbooks retain
experimental recording identifiers needed to distinguish trains. A structural
screen found no personal-name, date-of-birth, address, telephone, e-mail,
passport, or medical-record-number fields. This technical screen does not
replace the institutional, consent, and data-governance statements above.

The repository MIT licence applies to software code, not automatically to the
human-derived data. See `DATA_USE_TERMS.md` before reusing the workbooks.
