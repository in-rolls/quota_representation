# WB persistence extension audit

Scope: the separate WB extension and its new selection/official-source modules.
This is not an audit of the parent Rajasthan/UP paper. Existing Birbhum results
were independently rerun; the extension is exploratory and was planned after
the original 94-GP result was known. No causal effect is claimed.

## Claims and estimands

| Claim | Unit, population and time | Quantity and uncertainty |
|---|---|---|
| Prior reservation and later female officeholding | One GP; 94 observed Birbhum GPs with verified histories, women-open in 2003; exposure 1998, outcome current Pradhan in 2006 | Equally weighted difference and OLS adjustments; HC2/HC3 model-based 95% intervals |
| Effect of unresolved outcome measurements on descriptive means | One GP; the 99 history-verified, women-open offices, including five unresolved outcomes | Sharp binary bounds on the finite-roster mean difference; not a sampling interval or causal bound |
| Coverage of new official material | Source office rows or positive reservation-axis mentions, separated by year, district, tier and stage | Descriptive coverage and same-source/same-cycle consistency; no persistence estimate |

The exposure is reservation of the Pradhan office. The outcome is recorded sex
of its incumbent at follow-up. This does not measure the original election
winner, individual reelection, or 10-/15-year persistence. Women-open offices
can remain caste-reserved. Source GP identifiers are retained, but do not prove
cross-year administrative continuity outside the research archive.

## What changes for the research question

The original estimate is +1.19 percentage points (HC2 95% interval -16.09 to
+18.47). Adjustment using 1998 caste strata and block gives +2.47 points
(-18.39 to +23.33); also conditioning on 2003 caste status gives +0.71 points
(-20.56 to +21.97). These are different conditional associations on the same
94 observations, not corrections to a previously erroneous coefficient.

Of 165 source GPs, 151 have explicit nonconflicting histories; 99 are women-open
in 2003. The observed 94 comprise 28 prior-reserved GPs (five women) and 66 other
GPs (11 women). The full 99 comprise 30 and 69 respectively, with two and three
unresolved outcomes. All 32 binary completions put the descriptive difference
between -3.62 and +7.39 points. This bound does not solve selection into the
women-open group, turnover, confounding, or uncertainty beyond this roster.
The interval estimates remain too wide to establish a small persistence effect.

The five official exports checked by the new module contain no recorded later
GP-head winner sex. This is a statement about these parsed sources, not proof
that Pradhan rosters or formation records do not exist elsewhere. Nadia has
187 paired head/deputy reservation records and 186 observed head categories;
South 24 draft/final lists agree on 425 positive mentions. Neither provides
the missing later incumbent outcome for a persistence analysis. Ward-member
results do not substitute for Pradhan outcomes.

## Check matrix

| Check | Evidence and disposition |
|---|---|
| Tier 1: denominator/unit | 165 source /151 explicit-history /99 women-open /94 measured; one row per research GP. Positive mentions and ward results remain separate. |
| Tier 1: missing versus zero | Invalid ID/year/sex measurements masked; raw fields retained. Missing history is not untreated. Source blank and positive-list absence stay unknown. |
| Tier 1: row conservation | Unique source key asserted; all 165 rows retained; ordered exclusions exported; all five unresolved eligible outcomes remain in bounds. |
| Tier 1: provenance | Shared raw/code manifests and five official-export SHA-256 pins verified; all 68 original official source files checked. Generated reports read computed results. |
| Tier 1: internal consistency | Sex contradictions masked in shared preparation; 32 actual missing-outcome completions independently reproduce bounds; same 94 IDs across specifications. |
| Tier 2 A: EDA | Source profiles and audit helper log retained; missingness, exposure cells, leverage and block support exported. No p-value-selected sample. |
| Tier 2 B: joins | No new persistence outcome merge. Official Nadia pairing is one-to-one by document GP serial; duplicate/missing-key tests fail closed. |
| Tier 2 C: construction | Binary outcome measured in 2006, not inferred from reservation. Proportions converted to percentage points only for prose/plot. No index or ratio outcome. |
| Tier 2 D: estimand | Equal-GP observed association; finite-roster bounds separately labeled. No state-wide extrapolation, factorial interpretation or repeated-term causal effect. |
| Tier 2 E: inference | HC2/HC3 explicitly requested; pre-only design rank22 of22, residual df72. Independent lm plus sandwich HC2 matches estimatr to 1e-12. Assignment/sampling dependence remains unverified. |
| Tier 2 F: skew | Binary outcome has no heavy-tail transformation issue. Rare events and sparse block treatment support limit precision; existing leverage and leave-block-out outputs retained. |
| Tier 3: significance/forking | Plan discloses earlier unblinding; all fixed specifications reported. No significance-filtered districts or outcomes. Existing Type S/M scenarios are conditional calculations, not evidence of actual bias. |
| Tier 3: treatment/measurement | Explicit historical source dates; independent questionnaire sex checks; 2006 survey outcome acknowledged. Actual 2003 winner turnover cannot be verified here. |
| Tier 3: attrition | Arm-specific missing counts, ordered exclusions and sharp binary bounds replace an assumption of random attrition. |
| Tier 3: covariates/adjustment | 1998 caste strata and block are the earlier allocation controls; 2003 caste status is a post-exposure sensitivity. Adjusted/unadjusted samples are identical. |
| Tier 3: permutation/simulation | No reconstructed assignment schedule or validated exchangeability: unrestricted permutation is inapplicable. Exhaustive fake-data binary completion tests validate bounds, not causal coverage. |
| Tier 3: replication/multilevel | Only one district/transition supplies this outcome; four history cells are descriptive. No claims based on different significance levels or estimated heterogeneity. |
| Tier 4: identification | Observational cross-section and selection branches apply. Rotation and conditioning on later women-open status can select on prior exposure; fixed effects do not establish randomization. |
| Tier 4: model/support | Full-rank designs and leverage exported; binary linear models target mean associations. No generated regressor, IV, RD, machine learning, event study or DiD is used. |
| Tier 4: uncertainty | HC2/HC3 are conditional model-based intervals. GP is the observation unit; the recovered data do not justify design-based inference from a presumed independent lottery. |

## Reproductions, rejected concerns and unresolved checks

`Rscript wb/run.R` rebuilds the analyses and `Rscript wb/tests/run_tests.R`
checks contracts, sharp bounds, independent OLS/HC2 values, source tampering,
Nadia pairing, source blanks and positive-list absence policies. R lint passes.
The selection figure was rendered and visually inspected. A separate agent
reviewed the selection code and recomputed the results; this is a computational
review, not human double entry of the underlying historical survey.

The data-audit helper's optional statsmodels leverage calculation was unavailable
in the Python environment; the R design-matrix and leverage outputs cover that
check directly. Markdown reports are not supported by the skill's LaTeX
provenance resolver; generated reports and independently checked CSV values
provide the numerical comparison instead. No parent manuscript was revised.

Rejected concerns: the 94 count is not duplicate inflation; all keys are unique.
The five unresolved records do not alone explain a large positive observed
association. Blank head-category serial 123 in Nadia cannot be filled from its
deputy category. Agreement of draft/final positive lists does not establish a
complete office universe. The missing winner-sex series is not evidence that
no such records exist in archives.

Untestable here: counterfactual officeholding, unrecorded turnover, the original
assignment list/order for every GP, survey sampling dependence, and official
cross-year boundaries/IDs for later district lists. Recovering named Pradhan
formation/incumbent rosters with recorded sex and dates is the highest-value
next data acquisition for this particular question.
