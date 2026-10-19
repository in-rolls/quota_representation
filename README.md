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

## Uttarakhand extension

`scripts/02f_uttarakhand_panels.R` and `scripts/13_uttarakhand.R` add Uttarakhand pradhan (gram panchayat head) seats with the same estimand, for 2008–2014 and 2014–2019, read from the pooled `local_elections` release. Haridwar polls on its own cycle (2010, 2015) and is left out. Seat links come from `local_elections_uttarakhand`. All 89 block names agree across the three elections, so GP names are matched one-to-one inside the same block, with place qualifiers (upper/lower village, Khurd/Kalan, part numbers) as vetoes. Fuzzy links share a candidate name across elections as often as exact-name links (7.6% against 8.3% in 2008–2014), against 3.1% for random pairs in the block. 5,403 GPs link for 2008–2014 and 5,042 for 2014–2019.

**Where inference happens.** The results portal records no winner sex. The outcome is the pooled `winner_female_probability_from_name`, made upstream from the Devanagari name: a character model on the name, naampy on its indicate romanization, and fixed name markers, fit on recorded candidate sex in Uttar Pradesh and Bihar. On names one state's model never saw, the combined score reaches AUC 0.965–0.987 in the other state. Winners of Uttarakhand women's seats, who must be women, average 0.96–0.97. Effects are on the expected share of women among winners.

Uttarakhand rotates women's seats within caste categories, which also rotate: 87% of 2008 women's seats are open in 2014, and 82% of 2008 open seats are reserved. The open seats without a prior quota (462 in 2014, 446 in 2019) are the rotation's exceptions, and most of them changed caste category in between. Each model is therefore also estimated within (earlier category, later category) cells as well as blocks. On seats open in the later election, a prior quota goes with a lower expected share of women winning: about 3 points in 2014 (8.4% against 11.8%) and 4–5 points in 2019 (14.7% against 19.0%). The placebo fails for 2008–2014. Among seats open in 2008, those reserved in 2014 had elected 4 points fewer women in 2008, the size of the main estimate, so the 2008–2014 contrast may reflect which seats the rotation left open rather than the quota. The 2014–2019 placebo is 0.005.

## Tamil Nadu extension

`scripts/02g_tamil_nadu_panels.R` and `scripts/14_tamil_nadu.R` add Tamil Nadu village panchayat presidents with the same estimand, from 2011 to the next ordinary election: 2019 in 27 districts, 2021 in the nine districts carved out after 2011. Both read the pooled `local_elections` release. Seat links come from `local_elections_tamil_nadu`. Seats are matched one-to-one by folded Tamil name inside a hand-coded union crosswalk; unions, not districts, are the blocking unit, because six districts were split. 12,234 seats link. Linked pairs share a candidate name across the two elections 10.8% of the time, against 0.12% for random pairs in the same unions.

The outcome is the winner's recorded sex, printed on the State Election Commission's result sheets, so no name inference is involved. Two things differ from the other states:

- **The quota grew.** Women's reservation was one third of seats in 2011 and one half from 2019.
- **Eight years separate the elections,** because the 2016 election was cancelled.

Rotation is close to strict: 98% of 2011 women's seats are open in the later round, and 75% of 2011 open seats become women's seats. On the 6,108 seats open in the later round, a 2011 women's quota goes with 1.1–1.5 points more women winning (14.3% against 13.1%). Wild-bootstrap p-values are 0.26 with union fixed effects and 0.15 with union and caste-transition fixed effects. The placebo fails: among seats open in 2011, those reserved later had elected 2 points fewer women in 2011 (p < 0.01). As in Uttarakhand 2008–2014, the seats left open twice differ beforehand.

## Jharkhand extension

`scripts/02h_jharkhand_panels.R` and `scripts/15_jharkhand.R` add Jharkhand mukhiya seats, 2015 to 2022, read from the pooled `local_elections` release. Seat links come from `local_elections_jharkhand`: Form 23 results OCRed on-device, GP names matched one-to-one inside a hand-coded block crosswalk. 3,831 seats link.

Form 23 prints the seat's reservation, not the winner's sex. The outcome is the pooled `winner_female_probability_from_name`, from the same model as Uttarakhand's. The model is weaker on Scheduled Tribe seats: winners of ST women's seats, who must be women, average 0.88, against 0.98 on other women's seats. Santal, Munda, Oraon and Christian given names are rare in the UP and Bihar training labels. Every model is therefore run three ways: all seats, non-ST seats only, and ST seats rescaled by that recall.

Rotation is near total: 92% of 2015 open seats became women's seats in 2022. So among 1,772 seats open in 2022, only 143 also had no quota in 2015 (18 among seats ST in both years), and those 143 are the whole comparison group. On seats open in 2022, a 2015 quota goes with 5–7 points more women winning with block fixed effects. The raw gap is 22.0% against 11.4%. Wild-bootstrap p-values are 0.10–0.26, and the estimates hold across the three treatments of ST seats. The placebo passes: 0.009 for all seats and 0.04 for non-ST seats.

## Municipal councils

`scripts/02e_city_panels.R` and `scripts/12_cities.R` apply the same estimand to municipal councils: does a women's quota in one election raise the chance that a woman wins the ward when it is open in the next? A council enters only for a pair of elections on one ward map. The inputs are pinned files in `local_elections`, not the pooled master, which holds rural seats only.

- **Delhi 2007–2012** (272 wards; 134 open in 2012). Categories, assembly constituencies and winner gender come from Goyal's replication deposit. Its 2007 categories match the commission's 2007 order, including five wards another file leaves blank. The 2007 order gave every third non-SC ward in a fixed list to women; the 2012 seats were drawn afresh (52% of 2007 women's wards reserved again, against 50% of the rest). Delhi redrew its wards for 2017 and 2022.
- **Mumbai 2007–2012** (227 wards; 112 open in 2012). The 2007 women's seats and both councils' winner sex come from the Karekurve-Ramachandra and Lee deposit (Dataverse IO9SLQ); the 2012 categories come from `ulb_ward_2012`. Mumbai rotates: 75 of 76 wards reserved in 2007 are open in 2012. The 37 wards open in both years were therefore chosen by the rotation rule, probably on the 2002 categories, which are not held. If those wards were reserved in 2002, the Mumbai contrast compares a quota one term ago with a quota two terms ago, and understates the effect of having had one.
- **Kolkata 2010–2015** (141 wards; 94 open in 2015). Lok Dhaba's candidate files (republished by OpenCity) give every candidate and each ward's category. Ward electorates correlate at 0.96 by ward number across the two elections, so the map held; wards 142–144 were added in 2015. The files record no sex. Winners of women's seats are women by law; the rest take `local_elections`' `kolkata_winner_sex`, coded from first names, with names used for either sex excluded and bounded both ways. 40% of 2010 women's wards are reserved again in 2015, against 30% of the rest.

Women rarely win open seats in Delhi (6 of 134) and Mumbai (7 of 112), so those estimates are imprecise; Fisher's exact test is reported beside every model. Delhi has no constituency fixed effects: only one of its 68 constituencies holds both a woman winner and open wards that differ in their 2007 quota. In Kolkata, women won 37% of open 2015 wards that had been reserved in 2010 and 14% of the rest; 17 of the 19 women winning open wards in 2015 are the ward's 2010 councillor, counting two spelling variants. Among wards open in 2010, those reserved in 2015 had elected fewer women in 2010 (placebo −0.14), so the 2015 draw is not clean; that pattern leaves more sitting women among the comparison wards.

Kerala is not included. Its grama panchayat wards keep one map from 2015 to 2020 and sex is recorded, but the rotation is nearly deterministic: 98% of 2015 open wards are reserved in 2020 and 93% of 2015 women's wards are open. The few exceptions look mislabelled (women win 46% of the wards open in both years), so there is no credible comparison group.

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
