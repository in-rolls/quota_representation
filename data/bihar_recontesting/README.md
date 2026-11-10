# Bihar incumbents contesting again, 2016–2021

This exploratory analysis links recorded 2016 Mukhiya winners to the 2021 candidate list in the same gram panchayat. The primary comparison includes all winners of prior quota and non-quota offices, including women elected without a quota. A secondary comparison includes only women winners; it conditions on who won and is descriptive. Bihar's two-election reservation regime makes this a career comparison during continued reservation, not a test of persistence after withdrawal. No 2021 reservation status is inferred from undated current-term feeds. These estimates remain separate from the paper's open-election recontesting results and persistence meta-analysis.

## Sources and eligibility

`scripts/14_bihar_recontesting.R` reads six checksum-pinned releases directly from `local_elections_bihar`: the 2016–2021 Mukhiya office linkage, 2016 seats, winners and candidates, and 2021 seats and candidates. Their hashes and resolved paths appear in `tabs/bihar_recontesting_sources.csv`. No election source files are copied here. The 2016 winner-to-candidate join uses district, block, office and source row; the later candidate list uses the linked 2021 office ID. Candidate serial numbers distinguish people within an office.

The analysis requires a known prior reservation and recorded winner sex, a winner and family-member name, and a later candidate roster whose row count agrees with the released seat record. Result-only candidates cannot substitute for a candidate list. Missing rosters are excluded, not counted as non-recontesting. Prior quota offices with recorded male winners are excluded as source conflicts. The coverage table traces all linked offices through these restrictions. Exact office-name links provide a separate sensitivity analysis; the main sample includes the upstream reviewed linkage passes. Office-name linkage does not independently establish unchanged boundaries.

## Identity matching

Most 2016 names are printed in Latin script and the 2021 names in Hindi. Raw exact-name comparisons therefore cannot measure recontesting adequately. `14a_bihar_transliteration.py` builds a frozen Hindi-to-Latin lookup using the local `indicate` model, beam width five. Each distinct word is decoded once; the model treats words independently. Latin source names are preserved. `transliteration.json` records input hashes, software versions, model-asset hashes and the lookup hash. The ordinary R pipeline reads this lookup and makes no model or paid API calls.

The raw model sometimes emits digits in a name (for example, `अली` becomes `0li`). Such outputs, and empty outputs, use ICU transliteration in the R analysis. This avoids dropping whole villages because of a model error in any candidate's name. Source numerals remain part of an identity. Both approaches normalize case, punctuation and spacing.

- Strict matching requires exact agreement of both transliterated person and father/husband names.
- Generous matching permits at most two edits and 20% of the longer person name, and three edits and 25% of the longer family-member name. Nonexact matches require at least five characters in both strings.
- Both rules reject conflicting recorded sex and implausible ages. Recorded ages outside 21–110 are unavailable; a later age below 26 or more than ten years away from the expected five-year increase conflicts with incumbency.
- Offices with multiple strict matches or an exact name-pair match contradicted by age or sex are excluded from the common sample. Generous matching records any compatible candidate. Neither rule supplies verified identities or a formal bound on the true rate.

A separate ICU-only sensitivity applies the same edit-distance thresholds to generic transliterations. It misses many plausible cross-script identities, particularly men's names, and produces a much larger group difference. It is retained as a measurement diagnostic. The local-model analysis was introduced after inspecting the initial raw-script and ICU results; all estimates are exploratory. Spot checks of model-only and generous matches support the correction, but do not constitute a labeled validation sample.

## Estimates and checks

`tabs/bihar_recontesting_estimates.csv` reports rates, denominators, differences and block-clustered 95% intervals, using explicit finite-sample corrections and a t reference with G−1 degrees of freedom. Additional specifications adjust for block and prior caste reservation or restrict to exact office-name links. These intervals reflect sampling variation, not uncertainty about identity or office linkage. Candidate links, including contradictory exact matches, remain in `candidate_links.csv`; `analysis.parquet` retains the full linked-office frame and exclusion indicators.

A negative control rotates each incumbent to a different office's candidate list within the same block. Its accidental-match rate is recorded separately. This probes common-name collisions; it does not estimate all matching error within the correct village, where relatives may share names. The validation script independently reconstructs edit distances, match indicators, denominators, OLS coefficients and clustered covariance, and verifies input and lookup hashes.

Run the analysis and its checks with:

```sh
Rscript --vanilla scripts/14_bihar_recontesting.R
Rscript --vanilla scripts/98i_validate_bihar_recontesting.R
```

To regenerate the lookup, install the `indicate` and `torch` versions recorded in `transliteration.json`, plus pandas, pyarrow and PyYAML. Set `INDICATE_DATA_DIR` to the directory containing the matching Hindi-to-English model assets and run `python scripts/14a_bihar_transliteration.py`. An existing lookup is reused only if its hash and model signature match. The script checks pinned upstream inputs before reading them.
