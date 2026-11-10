# WB extension: units, joins and missingness

The persistence extension starts from the existing hash-verified Birbhum
`gp_2003_followup` file. Its key is `gp_id`, a research-source identifier,
not a national administrative code. One row is one GP, with 1998 and 2003
reservation variables in wide columns and a separately measured 2006 outcome.
It is not a GP-year panel. `selection_frame` retains all 165 rows and the same
unique key. This extension performs no new cross-source outcome merge.

History eligibility precedes outcome availability: explicit 1998/2003 history
and no detected cross-source 1998 category conflict produce 151 GPs. Restricting
to offices not reserved for women in 2003 produces 99; 94 have usable outcomes.
Caste-reserved offices can belong to this women-open frame. Five unresolved
outcomes are bounded as binary values, never imputed into the model. A valid
measurement can exist on an ineligible-history row; all estimators additionally
require history eligibility. The raw outcome remains alongside its masked copy.

All three adjustment specifications use identical GP IDs. 1998 caste strata
and administrative block are the earlier allocation controls. 2003 caste
status is post-exposure for a 1998 treatment and appears only as a sensitivity.
The observed-case estimand is an equally weighted association within the
selected women-open sample. The bounds concern descriptive means in the
99-GP finite frame, not causal effects or sampling confidence intervals.

The official-source module separately checks the five parsed export hashes,
schemas, source scopes and original source-file hashes specified in
`official_source_manifest.csv`. It does not append these rows to Birbhum.
`official_nadia_2013_pairs` joins head and deputy records one-to-one by the
same document's `source_gp_key`; it preserves repeated names and the one blank
head category. This key identifies a source serial, not proven cross-year
administrative continuity. `official_south24_2018_positive_comparison` compares
sets by office tier, block, name and category after case and whitespace
normalization. Positive mentions may repeat a GP across axes, offices or
stages. Counts of mentions must not be described as unique GP counts.

Neither an absent positive mention nor the missing opposite category axis
becomes an unreserved office. Only the complete Nadia synopsis with explicit
full-category codes supplies negative reservation axes. No winner sex is
inferred from either a person's name or an office's reservation status.
