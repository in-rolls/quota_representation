# Electoral gender quotas in rural India

Research code and manuscript for *The Limits of Electoral Gender Quotas in Rural Local Bodies*, by Varun Karekurve-Ramachandra and Gaurav Sood. The analysis follows local elections in Rajasthan (2005–2020) and Uttar Pradesh (2005–2021).

## Reproduce

Restore the R packages pinned in `renv.lock`, then run from the repository root:

```sh
Rscript -e 'renv::restore()'
Rscript --vanilla scripts/99_run_all.R
bash ms/compile.sh
```

The default runner rebuilds study panels from the pinned upstream election files, attaches Census covariates, reconstructs the phone records, and regenerates all main-text and appendix analyses. It also rebuilds the Birbhum survey analysis from raw files in `../local_elections`, using the checksum-pinned shared preparation code in `../quota_spending/wb`. It then runs the numerical validation suites, including pooling, additional settings, temporal persistence, recontesting, characteristics, and descriptive measurement checks. The archived survey sampler is never rerun.

`--from-panels` skips the UP/Census-panel preparation and phone reconstruction, using existing study panels and reviewed survey outputs instead. It still regenerates the estimates and the West Bengal extension. The full source rebuild is the reproducibility check; the shorter route is for analysis work with existing inputs. Validation covers join cardinality, missing outcomes, denominators, independent coefficient and covariance reconstruction, seeded bootstraps, and source hashes. To rerun those checks alone:

```sh
Rscript scripts/98_validate.R
```

R 4.6 and XeLaTeX were used for this revision. The PDF build also requires `codespell` and checks manuscript prose and generated TeX tables with its standard spelling and British-to-American dictionaries. Bibliographic titles are preserved. `fwildclusterboot` uses its R engine; Julia is not required.

In addition to the pinned election sources and licensed SHRUG inputs below,
the source rebuild requires these author-held
files at their original paths:

- `data/shrug_gp_xwalk/data/shrug_LGD_matched.csv`
- `data/raj/source/phone_survey_response/sampled_nos_full_analysis.xlsx`
- `data/raj/source/phone_survey_response/sampled_mobile_nos_open_seats.xlsx`
- `data/raj/source/phone_survey_response/jaipur_audit.xlsx`

These files are excluded from Git. Obtain the geography files from the authors
and arrange access to the original interview workbooks before running the full
source rebuild. A public checkout can reproduce the electoral results from the
committed panels; it cannot independently rebuild the survey tables or those
geographic crosswalks.

## Data and analysis

`scripts/05g_short_term_pooling.R` averages the six next-election contrasts with equal state and election weights, reconciles geographic block names across cycles, and estimates their joint cluster covariance. It generates the main pooled-effect figure and sensitivity results in `tabs/short_term_pooled*`; `scripts/98e_validate_pooling.R` checks the covariance against a stacked regression and verifies geography, weighting, and bootstrap reproduction. The analysis runner includes both scripts.

Rajasthan election sources and standardized files come from a pinned commit of `local_elections_rajasthan`. Rajasthan, UP, Delhi, and Bihar election inputs, the local SHRUG–LGD crosswalk, and SHRUG 2.0 inputs are pinned by SHA-256 in `data/manifest.yaml`. Election files resolve from the versioned cache, a sibling checkout, or the pinned GitHub source. UP adjacent-year and four-election panels, and the March 17 Weaver wide file, are produced once in `local_elections_up`. Both studies pin the same shared UP panels. Rajasthan survey linkage, model recodes and outcome joins remain here. Reviewed contact classifications are in `data/raj/phone_contacts_reviewed.parquet`; `07b` links recorded officeholder contacts to candidate identities in `data/raj/phone_respondents_reviewed.parquet`. The earlier linkage is preserved in `data/raj/phone_candidate_links.parquet` as a historical artifact; neither estimation nor validation requires it. The completed survey and its historical sampling frame remain local under `data/raj/source/phone_survey_response/`; the sampler is excluded from the analysis runner. Obtain licensed SHRUG inputs from [Development Data Lab](https://www.devdatalab.org/shrug_download/), retaining their attribution files. The default external cache is `~/data`; set `INDIA_DATA_HOME` to use another location. The SHRUG–LGD crosswalk is in `data/shrug_gp_xwalk/`; Rajasthan and UP LGD inputs resolve from their pinned upstream caches.

Rajasthan event identities, election histories and election-to-LGD links are produced in `local_elections_rajasthan` and read directly through the shared cache. Their former local copies and construction scripts have been removed. `03a` joins the pinned geographic bridge to those election panels and aggregates study Census/SHRUG covariates; it does not rematch Rajasthan GPs. Source-row IDs accompany the study panels. The migrated inputs preserve every existing released model-input value and row position. Candidate identity and candidate-name uniqueness are separate: different candidates may share a name. Ambiguous links remain unavailable. Primary reservation classifications follow the original GP source; exclusions of conflicts with candidate records are sensitivity analyses. UP matching preserves Hindi vowel marks and numeric distinctions, requires mutually unique geographic links, and composes independent adjacent pairs through common source IDs. `02b` adds study variables and excludes unknown reservation after linkage; it does not rematch GPs.

Cache entries use `INDIA_DATA_HOME/{provider}/{ref}/{relative_path}` (default `INDIA_DATA_HOME=~/data`). A sibling file is read directly only when its checksum matches the pinned revision; otherwise the resolver downloads that revision. Downloads are verified in a temporary file before installation. Cached files are checked on every read.

Keep election source archives and reusable releases in their upstream repositories; add new study inputs to the manifest rather than copying them here. Historical Rajasthan spreadsheets and gazettes are maintained in `local_elections_rajasthan/data/raw/sarpanch/background/`, and the 2014 delimitation export in `local_elections_delimitation_rajasthan/data/processed/`. Redundant local copies were removed after verifying byte-for-byte agreement with committed upstream files. Study-specific recodes, survey records, reviewed links, and generated analysis datasets remain here. The third-party SHRUG–LGD package retains its original contents and attribution.

Village counts are summed. GP distance is the unweighted mean of observed constituent village-to-town distances, with the minimum recorded distance as a sensitivity analysis. A facility is available if observed in any constituent village; absence requires observed absence in every village. Coverage accompanies these aggregates.

Scripts `04–07` produce descriptive comparisons and regressions. Main inference clusters by district–samiti in Rajasthan and district–block in UP, with explicit small-sample adjustments and retained singleton groups. HC1 results are supplied as a sensitivity analysis. Primary fixed-effects coefficients and cumulative contrasts also receive 9,999 null-imposed Rademacher wild cluster bootstrap draws with inverted confidence intervals. `tabs/model_inference.csv` records the sample, contrast, cluster counts, comparison-cell counts, covariance choice and bootstrap seed. The history-support table reports how many GPs and clusters contribute to each history. Restricted cumulative models use the same final election as the full model; passing an independence test is a sample restriction, not proof of random assignment.

Script `08a` uses the completed interview records. The archived sampler `08b` is deliberately outside the runner: rerunning an analysis must not redraw a completed survey. The archived initial selections contain 578 quota-seat and 593 open-seat records; the completed call files contain 500 and 507. The reason for that reduction is not documented in the retained files. The [phone review](data/phone_review/README.md) documents source-preserving corrections, Jaipur reservation classifications from the official roster, and bounds for unresolved respondent identities. Contact includes any documented attempt, transfer, or callback.

The Weaver appendix uses the supplied source panel, with its final wave labeled 2021 in the exhibits (coded 2020 in the source). Its models use complete 2011 Census district–block identifiers; the original election-specific 2015 block fields are blank throughout the matched short-run sample.

The temporal persistence analysis (`scripts/11_persistence_horizons.R`) follows the same GPs through consecutive open elections after the last observed reserved term. Its [exploratory analysis plan](analysis_plans/persistence_horizons.md) distinguishes duration from cumulative exposure. The script generates `tabs/persistence_horizon*.csv`, the analysis records in `data/persistence_horizon_analysis.parquet`, and the figure and tables for the persistence appendix. Source hashes, sample support, paired changes, multiplicity adjustments, and leave-one-block-out diagnostics accompany the estimates. `scripts/98c_validate_persistence.R` independently reconstructs the samples, within-block estimates, clustered covariance and temporal tests, and reproduces a seeded bootstrap result. To run this extension alone:

```sh
Rscript --vanilla scripts/11_persistence_horizons.R
Rscript --vanilla scripts/98c_validate_persistence.R
```

## Versions

`pre-corrections-20260912` preserves the manuscript and code before these revisions; it is not a certification that every earlier exhibit reproduces. Current scripts overwrite the current derived outputs. Git and release tags preserve earlier versions. The `v1.0` release uses the revised linkage and inference, reports cumulative contrasts explicitly, and treats the phone findings as evidence consistent with proxy governance. The phone audit does not establish who governs or explain the electoral effects.

UP geographic assignments now come from `gp_lgd_bridge.parquet` at the UP commit pinned in `data/manifest.yaml`. The historical bridge explicitly uses the 2005–2010 linked universe with known reservation in both waves; expanding that reference universe is a separate linkage choice. `03b` joins by the source anchor (`key_2010`, or `key_2015` for 2015–2021) and retains this study’s SHRUG aggregation. The upstream bridge preserves geographic identities and includes Unicode-safe distances and two documented native-name identity reviews; these diagnostic changes leave the study populations and distance-based LGD selections unchanged. Local UP geographic producers and duplicate directories have been removed.

<!-- adjacent:start -->

<!-- adjacent:end -->

## Winners contesting the next election

The recontesting appendix links Rajasthan's 2015 winners to 2020 candidates and UP's 2015 winners to 2021 candidates in seats fully open at follow-up. It reports strict and generous matching scenarios, including transliteration, edit distance, relative names and available age/sex checks. [Design and scope](analysis_plans/recontesting.md). The candidacy appendix addresses aggregate women's candidacy instead.

```sh
Rscript --vanilla scripts/12_recontesting.R
Rscript --vanilla scripts/98d_validate_recontesting.R
```

Both are included in `scripts/99_run_all.R`. The existing source manifest pins the input files; `tabs/recontesting_sources.csv` records the hashes used. Generated candidate links and nearest nonmatches are retained under `data/recontesting/`. These are possible identities under stated rules, not verified person identifiers. Tests independently reconstruct match counts and clustered estimates from the source rosters.

## Measurement checks

Two further exploratory extensions read upstream releases directly: [UP urban heads, 2012–2017](data/up_urban/README.md), and [Bihar incumbents contesting again, 2016–2021](data/bihar_recontesting/README.md). Scripts `13` and `14` generate their analysis files and tables; `98h` and `98i` validate them. All four are included in the main runner. The UP comparison concerns later non-quota offices; Bihar concerns careers during continued reservation. They remain separate from the manuscript's pooled results. Bihar uses a frozen, hash-checked local-model transliteration lookup; the analysis runner requires no Python model execution.

Election coverage counts distinct offices with complete geographic identities, using cleaned GP names. Reservation percentages exclude unknown or conflicting status. Census balance tables use total SC/ST population, facility availability percentages, and per-capita counts only for variables defined as counts. The three water-source code fields remain in the raw data but are excluded from these diagnostics because their numeric coding has not been verified as a quantity or a binary indicator. Maternity and child welfare centers are distinguished from family welfare centers.

Characteristic comparisons treat ages outside 21–110 as unavailable for age only; raw ages and the exclusion counts remain in `tabs/candidate_age_screening.csv`. Twenty-one is the minimum age for office; 110 is a plausibility screen. The recorded children totals include apparent entry errors and are retained in diagnostic CSVs but omitted from manuscript tables. These measurement checks do not change the electoral outcome or treatment coding.

## Earlier pooled-corpus explorations

The separate `scripts/99_run_exploratory.R` driver preserves the state and city explorations previously published on the remote branch. Their original pooled-corpus snapshot is pinned separately as `local_elections_exploratory`; the existing cache is reused. `scripts/98j_validate_exploratory.R` retains their numerical reproduction checks. These explorations include name-inferred outcomes and other measurement or assignment limitations; they are outside the manuscript synthesis. The main `99_run_all.R` driver generates and validates the reviewed manuscript results, including the additional urban comparisons documented in [the evidence README](data/additional_evidence/README.md).
