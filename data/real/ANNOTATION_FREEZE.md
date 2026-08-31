# Real-data annotation freeze for reviewer analyses

Author: Zhou Houchun
Freeze date: 2026-08-31

This record freezes the annotation workbooks used by the reviewer-response
analyses without changing the private source files. Public release copies use
region-only names and contain no patient or source-person surname in the file
name. The SHA-256 values below identify the exact public bytes.

| Region | Public workbook | Sheets | Rows | Reference-eligible trains | Scored ISIs | SHA-256 |
|---|---|---:|---:|---:|---:|---|
| GPe | `PD_GPe_manual_isi_labels.xlsx` | 16 | 28,635 | 13 | 23,270 | `9ee5c50f04b15c265c7840c4a5235f4cee406252ff788d289f6de073218251a7` |
| STN | `PD_STN_manual_isi_labels.xlsx` | 23 | 16,705 | 23 | 13,839 | `42d246192b9051b82fc979e696619d804051503a21ca65183fd59abaa781d30e` |
| GPi | `PD_GPi_manual_isi_labels.xlsx` | 23 | 19,819 | 21 | 16,042 | `eac8ec1be5dc17d7e92163461fc6af19821e4b73613ca69afdad2fe50695e26d` |

The reference-eligible scope is the frozen mask used by the unified
three-method experiment: `review_status == manually_labeled`. Noneligible
trains remain in the workbooks for provenance but do not enter the reported
accuracy denominators. Blank pattern cells retain the prespecified `other`
interpretation where applicable.

This is a scientific-byte and eligibility freeze, not a redistribution
authorization. Publication/source citations, ethics wording, original-data
licences, derived-label licence, and redistribution permission must still be
completed in `data/real/SOURCE_AND_LICENSE.md` before a public GitHub push.
