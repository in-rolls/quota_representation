# Pre-estimation review disposition

The frozen `pap.md` was not edited. It was committed before estimation in the
source repository at `4e62554` and tagged `wb-analysis-plan-20260907`.

- The main sample flow is generated from the sequential predicates in
  `scripts/01_officeholding.R` and checked against the fixed sequence
  165 -> 110 -> 109 -> 99 -> 99 -> 96 -> 95 -> 95 -> 94. Each step preserves
  the removed GP names in `tabs/sample_flow.csv`.
- The main outcome continues to use the explicit A8 current-Pradhan check and
  becomes missing when an observed A0/A1 field contradicts it. Names and
  honorifics never define sex.
- Ganpur (survey spelling Gonpur) is the sole numeric-ID disagreement. It is
  excluded in the main sample and admitted only by the named sensitivity.
- The adjusted model is retained as a secondary sensitivity. Its full-rank
  design has 94 observations, 24 columns, 70 residual degrees of freedom, and
  sparse within-block treatment support. Leverage, block support, HC3, and
  leave-one-block-out results are generated for inspection.
- `shared_code_manifest.csv` pins the shared preparation code, contracts,
  source manifest, and synchronized dictionary. `renv.lock` records the full
  dependency closure used by the scripts.

The strongest unresolved rival explanation is selection into an observed 2006
incumbent record after turnover, survey availability, identity, history, and
sex-consistency restrictions. Conditioning on a 2003 office open with respect
to the women's quota under rotation can also select on prior assignment.
