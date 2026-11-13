# WB extension: sample selection and usable new evidence

This exploratory addendum is written after the earlier 94-GP result and source
counts were known, before fitting the additional adjustment models. It is not
a preregistration. The original analysis remains a separate output.

The target question is whether a prior women-reserved GP-head office predicts
later female officeholding when the office is no longer women-reserved. Here
the outcome is measured at the 2006 follow-up, not the 2003 election itself.

1. Keep the 165-GP reservation roster intact. Define the history-eligible frame
   from explicit 1998 and 2003 dates and absence of detected category conflicts,
   before conditioning on an interview or observed sex. Show all four exposure
   histories, the share entering a women-open 2003 office, and each measurement
   exclusion. Unknown history does not become zero.
2. For the history-eligible gender-open frame, treat absent, inconsistent-ID,
   invalid-year and conflicting-sex outcomes as unobserved. Compute sharp
   finite-roster bounds on the prior-quota difference using binary outcome
   bounds [0, 1]. These are descriptive identification bounds, not confidence
   intervals or causal bounds. No assumptions of random attrition or monotonic
   selection. Preserve the observed-case HC 2 result alongside the bounds.
3. Compare the unadjusted model, adjustment for 1998 caste strata plus block,
   and the older model that additionally conditions on 2003 caste reservations.
   The latter is a post-exposure sensitivity, not the preferred adjustment.
   Use identical complete cases for direct specification comparisons, and
   explicit HC 2 plus HC 3 sensitivity. No p-value-based sample selection,
   propensity modeling, unrestricted permutations or claims of randomization.
4. Tabulate all four 1998/2003 reservation-history outcome means. These are
   descriptions of different groups, not a factorial experiment or estimates
   of repeated-treatment effects. Do not use reserved-office compliance as a
   substitute for an independently recorded winner sex.
5. Inventory each newly parsed official GP-office source by district, cycle,
   stage and row unit. Determine whether it actually supplies a verified later
   GP-head winner outcome and a dated GP crosswalk. Source cells, positive
   reservation-axis mentions, office rows and ward/candidate events stay
   separate. No outcome linkage by names or by reservation status.

Report all fixed comparisons, their sample sizes and intervals. Existing
block support, leverage and leave-one-block-out diagnostics remain available.
The causal assignment order, sampling dependence and replacement process are
not identified from the recovered binary treatment alone. HC 2/HC 3 intervals
are descriptive model-based uncertainty and do not validate those assumptions.

Dictionary/recode/join contracts remain in dictionary.csv and shared source
preparation. New derived-table definitions and no-pooling contract will be
recorded in extension_dictionary.csv and extension_join_contract.md. Existing
published results are not overwritten by this extension.
