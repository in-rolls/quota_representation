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

## Bihar extension

`scripts/02c_bihar_panels.R` and `scripts/10_bihar.R` add Bihar Mukhiya (gram panchayat head) seats with the same estimand: among seats open in an election, does a women's quota in the previous election change the chance that a woman wins? Seats, reservations, recorded winner sex and seat links are read from the pooled `local_elections` release (`master_bihar`, `master_extras`, `supplemental_seat_links`), pinned in `data/manifest.yaml`; the links themselves are made in `local_elections_bihar`. No GP identifier survives between Bihar elections, so those links pair GP names within the same block, with Devanagari names romanized by indicate and assigned one-to-one by preclink. Among 2016–2021 links made on names alone, ward counts agree for 97.3% of GPs, against 23.9% for random pairs in the block. The 2011–2016 link has no ward counts, so it rests on name similarity alone.

Women's quotas were not reassigned between 2016 and 2021: 98% of linked GPs keep their 2016 status, and only 13 GPs reserved in 2016 are open in 2021. Effects are therefore estimated only for 2011 to 2016, when 71% of women's seats became open; 2016–2021 appears only in the transition and rotation tables. The 2011 quota comes from 23 district winner reports. The primary sample uses only labels that state women's reservation. The inclusive sensitivity reads a bare category label as not reserved for women, where the same district's report prints women's labels. Bihar has two usable elections, so the cumulative-history models are not estimated. SHRUG covariate balance is not estimated either, because no Bihar GP–SHRUG crosswalk exists.

## Haryana extension

`scripts/02d_haryana_panels.R` and `scripts/11_haryana.R` add Haryana sarpanch seats with the same estimand, read from the pooled `local_elections` release. Seat links come from `local_elections_haryana`: GP names matched one-to-one by preclink inside reviewed district and block crosswalks. Among 2010–2016 links made on names alone, ward counts agree for 48% of GPs, against 14% for random pairs in the block. Wards were redrawn after the 2011 census.

**Where inference happens.** Haryana's notifications record no winner sex. The outcome is the pooled `winner_female_probability_from_name`, made upstream from the printed name: naampy's first-name score plus fixed surname and title markers, recalibrated on 2022 winners, whose sex the 2020 amendment fixes (grouped cross-validated AUC 0.993). Averaged over seats it estimates a share of women. Effects are therefore on the expected share of women among winners, not on a recorded count; winners printed only as placeholders drop out.

Seven 2010 blocks print far fewer women's seats than the statutory third while the commission's own reservation table and the winners' names disagree; their 411 linked GPs are dropped (pooled flag `reservation_source_conflict`). Haryana rotates women's seats: 95% of 2010 women's seats are open in 2016, so the within-district independence restriction retains no district and the comparison rests on the rotation, as in Rajasthan and UP. Effects are estimated for 2010–2016 only. Under the 2020 amendment only women contest women's offices and only persons other than women contest the rest, so no woman can win an open 2022 seat; 2016–2022 appears only in the transition and rotation tables.

## Delhi and Mumbai councils

`scripts/02e_city_panels.R` and `scripts/12_cities.R` apply the same estimand to two municipal councils, 2007–2012: does a women's quota in 2007 raise the chance that a woman wins the ward when it is open in 2012? Both cities kept one ward map for 2007 and 2012 and redrew it for 2017, so this is the only pair in which a ward number names the same place twice. The inputs are pinned files in `local_elections`, not the pooled master, which holds rural seats only.

- **Delhi** (272 wards; 134 open in 2012). Categories, assembly constituencies and winner gender come from Goyal's replication deposit. Its 2007 categories match the commission's 2007 order, including five wards another file leaves blank. The 2007 order gave every third non-SC ward in a fixed list to women; the 2012 seats were drawn afresh (52% of 2007 women's wards reserved again, against 50% of the rest).
- **Mumbai** (227 wards; 112 open in 2012). The 2007 women's seats and both councils' winner sex come from the Karekurve-Ramachandra and Lee deposit (Dataverse IO9SLQ); the 2012 categories come from `ulb_ward_2012`. Mumbai rotates: 75 of 76 wards reserved in 2007 are open in 2012. The 37 wards open in both years were therefore chosen by the rotation rule, probably on the 2002 categories, which are not held. If those wards were reserved in 2002, the Mumbai contrast compares a quota one term ago with a quota two terms ago, and understates the effect of having had one.

Women rarely win open council seats in either city (6 of 134 in Delhi, 7 of 112 in Mumbai), so the estimates are imprecise and Fisher's exact test is reported beside them. Delhi has no constituency fixed effects: only one of its 68 constituencies holds both a woman winner and open wards that differ in their 2007 quota.

## Versions

`pre-corrections-20260912` preserves the manuscript and code before these revisions; it is not a certification that every earlier exhibit reproduces. Current scripts overwrite the current derived outputs. Git and release tags preserve earlier versions. The `v1.0` release uses the revised linkage and inference, reports cumulative contrasts explicitly, and treats the phone findings as evidence consistent with proxy governance. The phone audit does not establish who governs or explain the electoral effects.

UP geographic assignments now come from `up_gp_lgd_bridge.parquet` at the UP commit pinned in `data/manifest.yaml`. The historical bridge explicitly uses the 2005–2010 linked universe with known reservation in both waves; expanding that reference universe is a separate linkage choice. `03b` joins by the source anchor (`key_2010`, or `key_2015` for 2015–2021) and retains this study’s SHRUG aggregation. The upstream bridge preserves geographic identities and includes Unicode-safe distances and two documented native-name identity reviews; these diagnostic changes leave the study populations and distance-based LGD selections unchanged. Local UP geographic producers and duplicate directories have been removed.

<!-- adjacent:start -->

## 🔗 Adjacent Repositories

- [in-rolls/quota_elite_quality](https://github.com/in-rolls/quota_elite_quality) — Effect of Gender Quotas on Some Qualities of Elites
- [in-rolls/local_elections_rajasthan](https://github.com/in-rolls/local_elections_rajasthan) — Rajasthan GP Election Reservation Status and Results for 2020--2022
- [in-rolls/quota_spending](https://github.com/in-rolls/quota_spending) — Effects of Randomly Assigned Reservations for Women Leaders in Indian Local Government on Allocation and Development Outcomes
- [in-rolls/local_elections_bihar](https://github.com/in-rolls/local_elections_bihar) — Bihar panchayat elections: 2016 candidates and votes for six offices; 2021 mukhiya candidates, results, winners and seat reservations
- [in-rolls/local_elections_kerala](https://github.com/in-rolls/local_elections_kerala) — Kerala Local Government Seat Reservation Data and Winner Attributes

_Powered by [Adjacent](https://github.com/gojiplus/adjacent)_

<!-- adjacent:end -->
