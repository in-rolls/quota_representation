# West Bengal: prior quotas and women's later officeholding

This R extension asks quota_representation's persistence question using named Birbhum
reservation history for 1998 and 2003 and a current-officeholder survey in 2006.
Read the generated [RESULTS.md](RESULTS.md) for the original comparison and
[SELECTION_RESULTS.md](SELECTION_RESULTS.md) for the new sample bounds and
adjustment comparisons. [OFFICIAL_SOURCES.md](OFFICIAL_SOURCES.md) explains
which additional official reservations can support this research question.
The outcome is follow-up incumbency in the 2003 term, not a verified 2003 election
winner. The sample is one district and one transition, so it does not supply
10- or 15-year persistence evidence.

The 99-GP women-open frame is a subset of Birbhum's 165-GP historical roster,
not the size of the WB collection. Five outcomes are unresolved; 94 enter the
observed-case comparison. Later reservation sources add substantial coverage,
but the audited official exports do not yet supply a linked series of observed
Pradhan winner sex for estimating longer persistence.

## Broader source availability

The Birbhum analysis is a pilot from acquired survey files, not an exhaustive
WB sample-selection rule. A subsequent [source availability audit](../../local_elections/data/west_bengal/discovery/vintage_search/head_outcome_availability.md)
identified a higher-priority six-district election package for this question.
Its official metadata reports 1,316 physical cases, reservation fields for
1998/2003/2008, and sex fields with 1,141 valid values for 2003 and 1,309 for
2008. These are metadata counts, not verified unique GPs or complete model
observations. Access currently requires a guestbook entry; the microdata,
sex coding and winner-versus-incumbent definition remain unverified.

Anonymous stable GP identifiers can support within-source analysis once their
uniqueness and continuity are checked. Acquire and audit that package before
choosing further district estimates. Nadia's official reservation records do
not contain observed Pradhan sex; its separate demand analysis belongs under
`quota_spending` and is not an additional head-sex sample. The audit preserves source
URLs, raw metadata responses, retrieval times and hashes. Existing model inputs
and estimates here have not changed.

## Run

From the quota_representation root:

```sh
Rscript --vanilla wb/run.R
Rscript --vanilla wb/tests/run_tests.R
```

`--prepare-only` stops after source preparation and contracts. R source
preparation is shared with `../quota_spending/wb/scripts/00_common.R`; set `WB_QUOTA`
to the quota_spending WB folder if the repositories are not siblings. Set
`WB_LOCAL_ELECTIONS` to override the raw-source repository location. The shared
[source manifest](../../quota_spending/wb/source_manifest.csv) pins all raw inputs by
SHA-256. The `shared_code_manifest.csv` also pins the shared R code, dictionary,
source manifest and contracts used for this version.

Required packages and their versions are recorded in [renv.lock](renv.lock)
and `logs/sessionInfo.txt`. Every run recomputes the WB analysis tables and audits. The main analysis runner
builds this module before producing the cross-setting synthesis.

## Interpretation

The main comparison restricts to offices not reserved for women in 2003; SC/ST
quotas may remain. It compares prior women-reserved versus prior-unreserved GPs,
with one equally weighted observation per GP. Explicit current-questionnaire
sex fields define the outcome; conflicting answers remain missing. Reservation
and officeholder gender are never treated as equivalent.

The unadjusted contrast is primary. Block/caste-adjusted estimates, HC3,
leave-one-block-out results and leverage/support diagnostics are secondary.
Prior/current assignment can depend on rotation and changing caste strata;
conditioning on a current open office may select differently by prior quota.
Fixed effects and non-rejection of an independence test do not establish
random assignment. These are exploratory associations with HC2 intervals.

The main sample excludes ambiguous election histories, observed category
conflicts, the Ganpur/Gonpur numeric-ID discrepancy and the invalid 2000 visit year.
Sensitivity tables relax the interview-ID restriction or permit histories
contained within 1998–2003, and separately restrict to exact GP spellings.
Missing interviews, categories and sex fields are retained in the source audit,
not imputed. Normal-model power/Type S/Type M scenarios use a fixed observed SE
and do not validate the unknown assignment process.

## Outputs

- `data/cd_gp_identifiers.*`, `cd_village_identifiers.*`,
  `beaman_gp_identifiers.*`: inspectable name/ID crosswalks.
- `data/gp_2003_followup.parquet`:165 source GPs with eligibility and raw timing
  fields; the model's available-case sample is smaller.
- `tabs/followup_officeholding.*`, `open_office_counts.csv`: primary and adjusted
  results with denominators.
- `tabs/reservation_transitions.csv`: explicit-history women/SC/ST transitions.
- `tabs/selection_by_reservation_history.csv`: ambiguous histories and exclusions.
- `tabs/followup_sensitivity.*`, `followup_se_ladder.*`,
  `followup_leave_one_block_out.*`, `followup_block_support.csv`,
  `followup_leverage.csv`, `power_scenarios.csv`: scope and uncertainty checks.
- [dictionary.csv](dictionary.csv), `data/source_dictionary.csv`, `data/*audit*`,
  [pap.md](pap.md), `logs/design_review.md`: definitions, provenance and review.

The 1998 first-stage reconstruction is also exported. The released archive has
8 female officeholders among107 unreserved GPs, while the paper's6.5% implies7.
No record is changed to force agreement with the paper.

The extension has a separate [exploratory plan](extension_plan.md),
[derived-variable dictionary](extension_dictionary.csv),
[join and missingness contract](extension_join_contract.md), and
[analysis audit](AUDIT.md). New outputs use `selection_*` and `official_*`
names; the parent Rajasthan/UP analysis is unchanged.
