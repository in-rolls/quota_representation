# Persistence across subsequent open elections

Exploratory analysis specified November 7, 2026, after the existing next-election and final-election history results had been examined. This is not a preregistration or a blinded study. New trajectory outcomes were not estimated when these choices were recorded. Treatment-history support and missingness were inspected; an independent design review checked eligibility and clustering.

## Question and comparisons

Does the difference in women's electoral success after a reserved term remain at a second or third consecutive open election, without another reservation intervening? The unit is a linked Gram Panchayat in the existing four-election Rajasthan or Uttar Pradesh panel. The outcome is whether a woman wins, separately measured at each follow-up election. Reservation histories do not define winner sex.

Primary: compare 1000 with 0000, in chronological order 2005, 2010, 2015, 2020 (Rajasthan) or 2021 (UP). Follow the same GPs at the first, second, and third open elections after the 2005 quota. Require outcomes observed at every displayed follow-up so changes over time cannot be caused by changing GP composition.

Secondary, specified before fitting: 0100 versus 0000, and 1100 versus 0000, following the same GPs at both 2015 and 2020/2021. These distinguish time after reservation from the number of observed reserved terms. They are separate history contrasts, not independent studies or a randomized comparison of exposure intensity. Controls overlap across profiles. No claim about first-ever reservation or never-exposed controls is possible before 2005.

Sensitivity: for the 2005 quota comparison, require open status only through each follow-up, rather than throughout the entire remaining panel; require the current outcome observed. Report separately because the samples change. Also report available-outcome estimates within the fixed history cohorts, without requiring other follow-up outcomes observed, to assess missingness exclusions.

## Estimation and inference

Use linear probability models with 2005 district–block (samiti) fixed effects and clustering. Existing panel block partitions map one-to-one across waves; fixed baseline clusters also contain repeated-GP dependence. Use the repository's explicit MODEL_SSC and retain singleton fixed-effect groups. These are within-block comparisons with treatment-variance weights, not automatically equal-GP average treatment effects.

For each trajectory, estimate each follow-up on the same complete-case GP sample. Estimate later-minus-first differences by regressing the paired outcome difference on treatment with the same fixed effects and clusters. Use 9,999 null-imposed Rademacher wild cluster bootstrap draws, fixed recorded seeds, inverted 95% intervals, and the existing fwildclusterboot conventions. Report cluster-t intervals too. The primary temporal-comparison family has eight tests (two states, two differences for the 2005 profile and one for each secondary profile); report Holm-adjusted bootstrap p-values alongside unadjusted values.

A stacked model with block-by-followup fixed effects, baseline treatment and treatment-by-followup deviations, clustered on baseline blocks, provides a joint Wald test of equal contrasts across follow-ups. Its treatment coefficients must reproduce the separate-model estimates. Display pointwise rather than simultaneous intervals. Do not infer decay by comparing significance labels or overlapping intervals.

## Identification and diagnostics

The question concerns persistence without renewed reservation. Conditioning on an all-open future history need not break initial randomization when future assignment depends only on prior assignments and the controlled baseline strata. Causal interpretation requires exchangeability of the compared histories within strata, overlap, and no differential selection through linkage or missing outcomes. Future eligibility based on additional outcome predictors could violate these assumptions. Describe the estimates as conditional reservation-history contrasts and keep this qualification explicit; do not declare constrained randomization invalid merely because assignment depends on prior reservation.

Report treatment and control counts, observed female-winner rates, total and mixed-treatment blocks, GPs in mixed blocks, eligible versus complete-case counts, missing outcomes by horizon, and selection into successive all-open histories by original assignment. The Rajasthan primary complete-case cohort has 329 GPs, only 192 in 58 mixed blocks, so precision must be assessed from the actual sample. The UP primary cohort has 6,825 GPs. Do not use 2005 winner sex as a pretreatment control: it is downstream of 2005 reservation.

Do not pool these trajectories into Appendix J's next-election meta-analysis. Add a separate manuscript appendix and a short main-results subsection; generated exhibits and prose macros must use the same result files. Independently verify eligibility, row conservation, within-block estimates, paired differences, uncertainty, figure values, and manuscript numbers. A nonsignificant result does not establish disappearance or absence of persistence.

## Completed checks

An independent read-only review reconstructed all 34 level models, eight paired temporal changes, and six joint Wald tests from the source panels. Estimates, clustered standard errors, counts, raw outcome means, and Holm adjustments agreed numerically. Local validation also reproduced one complete seeded bootstrap run. After seeing Rajasthan's secondary trajectories, we added a leave-one-block-out influence diagnostic for every fixed-cohort level estimate; this diagnostic was not specified before estimation. Removing any single block preserves the negative first-election and positive second-election signs in both Rajasthan secondary comparisons. No specification or observation was discarded based on those results.
