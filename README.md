# Electoral gender quotas in rural India

Research code and manuscript for *The Limits of Electoral Gender Quotas in Rural Local Bodies*, by Varun Karekurve-Ramachandra and Gaurav Sood. The analysis follows local elections in Rajasthan (2005–2020) and Uttar Pradesh (2005–2021).

## Reproduce

Restore the R packages pinned in `renv.lock`, then run from the repository root:

```sh
Rscript -e 'renv::restore()'
Rscript scripts/99_run_all.R --from-panels
bash ms/compile.sh
```

With `--from-panels`, the runner uses pinned upstream Rajasthan election panels alongside the committed study analysis panels and survey tables, regenerates the electoral estimates, figures and numerical manuscript inputs, then runs `scripts/98_validate.R`. Licensed SHRUG inputs are still required for the covariate analyses. Validation checks join cardinality, unresolved matches, outcome missingness, phone denominators, independently estimated cumulative contrasts, and seeded bootstrap reproduction. To rerun those checks alone:

```sh
Rscript scripts/98_validate.R
```

R 4.6 and XeLaTeX were used for this revision. `fwildclusterboot` uses its R engine; Julia is not required.

To rebuild from source, omit `--from-panels`. In addition to the pinned election
sources and licensed SHRUG inputs below, that route requires these author-held
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

Rajasthan election sources and standardized files come from a pinned commit of `local_elections_rajasthan`. Rajasthan and UP election files and SHRUG 2.0 inputs are pinned by SHA-256 in `data/manifest.yaml`. Election files resolve from the versioned cache, a sibling checkout, or the pinned GitHub source. UP adjacent-year and four-election panels, and the March 17 Weaver wide file, are produced once in `local_elections_up`. Both studies pin the same shared UP panels. Rajasthan survey linkage, model recodes and outcome joins remain here. The 35 survey candidate-identity links are in `data/raj/phone_candidate_links.parquet`. The completed survey and its historical sampling frame remain local under `data/raj/source/phone_survey_response/`; the sampler is excluded from the analysis runner. Obtain licensed SHRUG inputs from [Development Data Lab](https://www.devdatalab.org/shrug_download/), retaining their attribution files. The default external cache is `~/data`; set `INDIA_DATA_HOME` to use another location. The SHRUG–LGD crosswalk is in `data/shrug_gp_xwalk/`; Rajasthan and UP LGD inputs resolve from their pinned upstream caches.

Rajasthan event identities, election histories and election-to-LGD links are produced in `local_elections_rajasthan` and read directly through the shared cache. Their former local copies and construction scripts have been removed. `03a` joins the pinned geographic bridge to those election panels and aggregates study Census/SHRUG covariates; it does not rematch Rajasthan GPs. Source-row IDs accompany the study panels. The migrated inputs preserve every existing released model-input value and row position. Candidate identity and candidate-name uniqueness are separate: different candidates may share a name. Ambiguous links remain unavailable. Primary reservation classifications follow the original GP source; exclusions of conflicts with candidate records are sensitivity analyses. UP matching preserves Hindi vowel marks and numeric distinctions, requires mutually unique geographic links, and composes independent adjacent pairs through common source IDs. `02b` adds study variables and excludes unknown reservation after linkage; it does not rematch GPs.

Cache entries use `INDIA_DATA_HOME/{provider}/{ref}/{relative_path}` (default `INDIA_DATA_HOME=~/data`). A sibling file is imported only when its checksum matches the pinned revision; otherwise the resolver downloads that revision. Downloads are verified in a temporary file before installation. Cached files are checked on every read.

Village counts are summed. GP distance is the unweighted mean of observed constituent village-to-town distances, with the minimum recorded distance as a sensitivity analysis. A facility is available if observed in any constituent village; absence requires observed absence in every village. Coverage accompanies these aggregates.

Scripts `04–07` produce descriptive comparisons and regressions. Main inference clusters by district–samiti in Rajasthan and district–block in UP, with explicit small-sample adjustments and retained singleton groups. HC1 results are supplied as a sensitivity analysis. Primary fixed-effects coefficients and cumulative contrasts also receive 9,999 null-imposed Rademacher wild cluster bootstrap draws with inverted confidence intervals. `tabs/model_inference.csv` records the sample, contrast, cluster counts, comparison-cell counts, covariance choice and bootstrap seed. The history-support table reports how many GPs and clusters contribute to each history. Restricted cumulative models use the same final election as the full model; passing an independence test is a sample restriction, not proof of random assignment.

Script `08a` uses the completed interview records. The archived sampler `08b` is deliberately outside the runner: rerunning an analysis must not redraw a completed survey. The archived initial selections contain 578 quota-seat and 593 open-seat records; the completed call files contain 500 and 507. The reason for that reduction is not documented in the retained files. Officeholder identity conflicts in seven female-held open-seat calls remain unresolved.

The Weaver appendix uses the supplied source panel, with its final wave labeled 2021 in the exhibits (coded 2020 in the source). Its models use complete 2011 Census district–block identifiers; the original election-specific 2015 block fields are blank throughout the matched short-run sample.

## Versions

`pre-corrections-20260912` preserves the manuscript and code before these revisions; it is not a certification that every earlier exhibit reproduces. Current scripts overwrite the current derived outputs. Git and release tags preserve earlier versions. The `v1.0` release uses the revised linkage and inference, reports cumulative contrasts explicitly, and treats the phone findings as evidence consistent with proxy governance. The phone audit does not establish who governs or explain the electoral effects.

UP geographic assignments now come from `up_gp_lgd_bridge.parquet` at the UP commit pinned in `data/manifest.yaml`. The historical bridge explicitly uses the 2005–2010 linked universe with known reservation in both waves; expanding that reference universe is a separate linkage choice. `03b` joins by the source anchor (`key_2010`, or `key_2015` for 2015–2021) and retains this study’s SHRUG aggregation. The upstream bridge preserves geographic identities and includes Unicode-safe distances and two documented native-name identity reviews; these diagnostic changes leave the study populations and distance-based LGD selections unchanged. Local UP geographic producers and duplicate directories have been removed.

<!-- adjacent:start -->

<!-- adjacent:end -->
