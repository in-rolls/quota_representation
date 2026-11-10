# Literature sources

This archive supports the manuscript and Appendix J. It follows `quota_spending/lit`: local PDFs in `papers/`, with version and evidence records in `studies/`. It currently covers the core persistence comparisons and the introduction and theoretical-framework sources collected during the audit; it is not a systematic literature review.

- `papers/`: local PDF copies, ignored by Git as in quota_spending.
- `papers.csv`: source URLs, source versions, local origins, and SHA-256 checksums.
- `studies/<key>.yaml`: study records, relevant pages, checks, and interpretation notes. Keys match `ms/quota_representation.bib`, except separately named supplements and working-paper versions.
- `ms/quota_representation.bib`: the manuscript's canonical bibliography.

The analysis inputs remain under `data/additional_evidence`; the literature copy does not replace those pinned inputs. Checks marked confirmed concern the stated claim, not every result in a paper.

## Beaman et al. (2009)

Open `papers/beaman_et_al_2009_qje.pdf` at PDF page 15 (printed page 1511), Table III. Columns 1–3 concern Pradhan appointments; columns 4–9 concern ward-level candidacy and wins. Appendix J uses the four history-specific Pradhan estimates in columns 1–2. The pooled column 3 is not an additional independent study. Ward-level outcomes concern spillovers of reservation of the GP-head office, not persistence in that same office.

The two adjacent-election estimates are 2.7 and 0.3 percentage points. The repeated-reservation estimate for 2008 is 7.6 points; the 1998-only delayed estimate is 5.6 points. Standard errors and sample sizes are recorded in `studies/beaman2009powerful.yaml` and match `data/additional_evidence/wb_published_estimates.csv`.

## Version distinction

The local Beaman et al. (2012) NIH manuscript reports 25% for the parental aspiration-gap reduction in its abstract; the published abstract reports 20%. The manuscript citation uses the published 20% and 32% figures, verified in the [MIT publication record](https://dspace.mit.edu/entities/publication/7a019642-cc68-4157-adbe-eb8452984e0d). Do not overwrite one version with another or silently treat their numbers as interchangeable.

## De Paola, Scoppa, and Lombardo (2010)

The local PDF is University of Calabria Working Paper 10/2009. The manuscript cites the published 2010 article for the qualitative persistence finding. The 1.4–1.5 percentage-point council estimates in the study record were checked in Table 2 of the working paper, not the final article. This candidate-list quota study contextualizes expectations; it is not an input to the Indian same-office meta-analysis.

## Theoretical framework

The archive also includes Gajwani–Zhang, Ban–Rao, Beaman et al. (2011), Goyal (2024), Afridi et al. (2013 working paper), and Sood’s undated qualifications draft. The [claim map](theory_claims.md) records their roles and interpretation limits. The manuscript now follows the agreed eight-paragraph framework.
