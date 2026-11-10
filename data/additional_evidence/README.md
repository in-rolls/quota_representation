# Additional electoral persistence evidence

This appendix compares women’s electoral success after prior reservation ends. It includes reproduced Mumbai estimates, exploratory Delhi, Chandigarh, Bengaluru, and Bihar comparisons, published West Bengal appointment estimates, and a separate Birbhum survey reanalysis. The initial comparisons were assembled on November 5, 2026; Chandigarh and typed upstream inputs were added on November 9. Delhi and Bihar are descriptive; Bihar’s statute retains reservation over two elections.

From the repository root:

```sh
Rscript --vanilla scripts/10_additional_evidence.R
Rscript --vanilla scripts/98b_validate_additional_evidence.R
bash ms/compile.sh
```

The generator verifies raw inputs against `source_manifest.csv`; Delhi and Bihar files also resolve through the versioned upstream cache in `data/manifest.yaml`. Regenerated analysis inputs are recorded, rather than frozen, in `tabs/additional_input_provenance.csv`. Repository HEADs give context; SHA-256 hashes identify the actual files, including working-tree changes. Generation needs the sibling source repositories. The manifest pins published upstream commits and verifies each input against its committed bytes. Compiling the saved paper exhibits does not. The appendix script is included in the full analysis runner, after rebuilding its West Bengal inputs. It can also be run separately after those dependencies exist. R versions are captured in `sessionInfo.txt`; the synthesis additionally uses `metafor` (version recorded in the session information).

## Mumbai 1997–2002

Source: [Bhavnani's author replication page](https://rikhilbhavnani.com/replication.html), [archive](https://rikhilbhavnani.com/files/BhavnaniAPSR2009Replication.zip), and [article](https://rbhavnani.github.io/files/BhavnaniQuotasAPSR.pdf). The original `.dta` and `.do` files are preserved unchanged in `../local_elections/data/maharashtra/raw/mumbai/bhavnani_2009/`. The analysis reads the typed `data/maharashtra/release/mumbai/bhavnani_comparison_1997_2002.parquet` handoff from that repository; its eligibility flag retains the published sample. They were downloaded on November 5, 2026. No general data license is asserted here.

One row in the original file is a 2002 ward. There are 227 rows. The article limits the historical comparison to 173 wards with largely unchanged boundaries (p. 26, note 8), and Table 3 uses 118 of these that were not women-reserved in 2002. Preserve that sample by selecting nonmissing `controltreat`: 0 identifies 81 controls, 1 identifies 37 treated wards. `reservedforwomen` is the **2002** quota, and `genderofwinner` is the 2002 female-winner indicator. `groups` is `1_NN` or `2_RN` in the selected sample. The 33 other women-open 2002 wards have missing treatment classification and do not become controls.

The reproduced numerator counts are 3 and 8 female winners. The point estimate matches the published comparison. The displayed Newcombe–Wilson interval is a new calculation; it is not the article's one-sided proportion-test inference.

## Delhi 2007–2012

Source: Goyal's [replication deposit](https://doi.org/10.7910/DVN/9XPV4I), `CandidateElectionData_original.xlsx`, held upstream as `data/delhi/interim/qualification_audit/sources/goyal_candidates.xlsx`. Its [file download](https://dataverse.harvard.edu/api/access/datafile/7423671), readme and codebook are identified in the upstream source manifest and pinned here. The upstream build reads the workbook as text. This analysis now consumes `../local_elections_delhi/data/release/persistence/wards_2007_2012.parquet`, which preserves the reviewed outcomes, explicit geographic link, and source hashes. Its 272 reservation histories and outcomes match the previous manuscript input exactly.

Use `eci_pos == 1` to identify winners and `commonname` as the ward identifier. Both 2007 and 2012 contain the complete set of 272 identifiers, unique within election. Join one-to-one on that identifier. The [SEC notification of January 27, 2012](https://sec.delhi.gov.in/sites/default/files/SEC/circulars-orders/notification-regarding-reservation-seats-2012_1.pdf) explicitly reuses the wards established by the February 2007 delimitation. The notification is retained at `../local_elections_delhi/data/raw/2012/reservation_order.pdf`. Do not extend this bridge to 2017 or 2022.

The 37 name differences after case/outer-space normalization were reviewed against the official 2007 reservation order retained upstream. Sixteen are spacing or punctuation. Consequential-looking differences also resolve in that order: wards 186–188 already had Sangam Vihar West/Central/East suffixes; ward 198 was Pul Pehlad, 258 Kardampuri, and 267 Nehru Vihar. All 272 2007 reservation codes agree with the order (271 extracted comparisons plus direct review of multiline ward 115). This review was performed by an assistant and is not human double entry.

Treatment is the 2007 category W or SCW. G and SC are known non-women-reserved categories. Restrict outcomes to 2012 G or SC offices; retain caste reservations. The research source supplies `gender` and `gender_doubt`. All 134 open-office winners originally have M/F and `gender_doubt == no`; that flag alone is not validation. The source gender remains preserved alongside the audited outcome.

`../local_elections/data/delhi/interim/sex_review.csv` records two corrections and one unresolved code. The [official 2016 government directory](https://delhishelterboard.in/main/wp-content/uploads/2019/06/Online-Directory-2016-Delhi-Govt.pdf), pp.321 and 334, identifies Raj Kumari, ward 111, as Ms. and Mahak Singh, ward 254, as Sh.; the original research file codes them M and F respectively. The ADR women-winners list independently confirms ward 111. These corrections offset in the treated group's aggregate count but repair individual outcomes. The original source codes ward 115's Dimple Chadha as F, while contemporaneous reporting refers to the councillor with male pronouns; the official 2016 directory, p. 316 provides no sex-specific title. This unresolved outcome is missing, not assigned from the name or quota. The primary sample has 133 observations (controls 2/89; treated 3/44). Both binary values for the missing outcome are evaluated on the 134-office roster. The untouched source comparison is 3/90 versus 3/44.

The directory and seven ADR list pages are retained under `../local_elections/data/delhi/raw/sex_validation/`, with URLs in `sources.json`. ADR pages encode table HTML inside JavaScript; the audit decoded the string transformation without executing fetched code. The extracted list has 122 distinct wards and is not assumed to cover every winner. Absence from that list is never coded as male. The review used explicit public textual sex markers, not appearance or personal names. The four other positive records are supported by the directory or ADR list. Corrected and original fields remain separate.

The primary comparison is unadjusted, with HC2/t uncertainty. Sensitivities add prior SC status and corporation, or restrict to unchanged SC status. The 2012 corporation membership follows the official notification: North is wards 1–100 and 149–152; South is the remaining wards through 208; East is 209–272. The unchanged-SC restriction is post-exposure sample selection. The deterministic population/order-based allocation rule does not establish random assignment; all Delhi estimates are descriptive associations. Specification choices followed inspection of coverage and transitions, before computing these persistence differences.

## Bihar 2011–2016

Sources are the Bihar State Election Commission's historical Mukhiya reports, 2016 candidate results, reviewed winner records, and one-to-one links in the sibling `local_elections_bihar`. Exact input files are pinned in `source_manifest.csv`. The [2009 Act amendment](https://sec.bihar.gov.in/Documents/Panchayat_Act_Rule/ACT_Bihar%20Panchayat%20Raj%20Amendment%20Act%202009.pdf), section 4, retains reservation for two consecutive elections beginning 2006. The [2006 election rules](https://sec.bihar.gov.in/Documents/Panchayat_Act_Rule/RULE_Bihar%20Panchayat%20Election%20Rules%202006%20-%28%20English%29.pdf), rule 14, use population ordering within categories. The corresponding PDFs and 2016 amendment are preserved here. This is a descriptive comparison under a two-election regime; 2006 reservation is not separately measured.

The generator joins 3342 links to 2011 treatment records and 2016 seats using unique identifiers. It excludes repeated 2016 seat codes and preserves missing prior reservation. It reads winner sex from explicit candidate fields. Accepted vote-dominance decisions require agreement on office, candidate serial, candidate name, and invariant nonmissing sex. Unique frame-matching official winner-list rows recover outcomes only for absent result pages. Existing outcomes are never overwritten. Six recoveries enter the eligible sample: five reviewed vote-dominance records and one direct winner-list record. Among 219 eligible offices observed in both winner sources, no name or sex disagreement was found. One disagreement outside the historical links is irrelevant to this sample.

The final eligible roster has 1310 offices; 1295 observed outcomes comprise 72/527 women in controls and 128/768 among prior-reserved offices. Fifteen missing outcomes are 7 control and 8 treated. The 530 linked open offices with unknown prior reservation are excluded; missing-outcome bounds do not cover these exclusions or incomplete geographic coverage. The linkage sensitivity `Name score = 1` uses exact agreement after normalization/transliteration, not a verified boundary crosswalk. Prior caste is a separate adjustment; unknown caste is retained as a category.

Block-cluster inference uses `sandwich::vcovCL(type="HC1",cadjust=TRUE)` and t(G−1) critical values. The finite-sample multiplier is G/(G−1)×(N−1)/(N−rank(X)). Independent NumPy reconstruction and R matrix calculations agree. The sole tied linked source winner was explicitly elected by lot and belongs to a currently women-reserved office, so is excluded from this comparison. The 2016–2021 pair does not measure quota withdrawal: the statutory regime carries reservation through both elections, and the available current reservation feed does not establish an election date.

## West Bengal

`wb_published_estimates.csv` transcribes Table III, columns 1–2, p. 1511 of [Beaman et al. (2009)](https://cpb-us-w2.wpmucdn.com/campuspress.yale.edu/dist/7/2986/files/2019/06/QJE-2009_Powerful-Women_Beaman-Chattopadhyay-Duflo-Topalova-Pande.pdf), preserved as `beaman_2009.pdf`. The four coefficients, standard errors, eligible sample sizes and baseline means were checked against the rendered table. The generator computes normal 95% intervals from rounded published coefficients/SEs. These are published estimates, not a microdata replication. The 2008 contrasts share one regression; Birbhum is part of the six-district sample. District selection was logistical rather than statewide random sampling.

`wb/data/selection_frame.csv` and `wb/tabs/selection_*` provide the separate Birbhum survey comparison. The outcome is 2006 officeholding, not verified 2003 appointment. Existing HC2 estimates and all 32 completions of five missing outcomes are independently checked. The full runner rebuilds these files from the original survey and reservation sources in `local_elections`, using the checksum-pinned shared preparation code in `quota_spending/wb`.

## Other comparisons and coverage

`tabs/model_inference.csv` supplies six full-sample fixed-effects next-election estimates, eight cumulative-history contrasts, and inverted wild cluster bootstrap intervals. Original main-paper models are unchanged. Repeated elections overlap; Weaver results are not independent additional settings.

Haryana's historical notifications and Uttarakhand's election records lack observed winner sex. Available probability scores were separately examined, not treated as the target outcome. Haryana calibration among legally women-reserved offices showed differential underprediction by prior treatment, larger than an open-office score contrast. No historical open-office validation establishes the measurement error. Uttarakhand does have adjacent-election links; its exclusion concerns outcome measurement, not absence of a panel. Karnataka's 5855-row,206-variable historical roster measures president reservations and population demographics, not president winner sex. Additional held 2015 GP aggregate totals and 2016 block/district council records do not provide the corresponding historical president outcome. These holdings are excluded from effect estimates.

Haryana's [2020 amendment](https://haryanadp.gov.in/wp-content/uploads/2025/04/Gazette_182-2020_Ext_10771.pdf), section 9(6), reserves offices for women or persons other than women in the 2022 election. The latter category is not open to women, so that election does not supply the paper's open-office comparison.

## Chandigarh 2006–2011

The analysis reads `../local_elections/data/chandigarh/release/ward_panel_2006_2011.parquet`. The official 2011 report confirms unchanged boundaries and documents the lotteries, exclusions under rotation, and caste-population rankings. The upstream source report, schema, dictionary and manual rule transcription retain the evidence. All 26 wards link uniquely; 17 were nonquota in 2011. The comparison uses independently printed candidate honorifics for winner sex: one woman among nine previously reserved wards, none among eight previously nonreserved wards.

We report the raw conditional difference with the same Newcombe–Wilson interval used for the other urban ward comparisons, plus HC2 adjustment for prior and current caste reservation. Each observed caste-transition group contains both treatment histories. This small comparison joins the setting-level synthesis; its omission is included in both the pooled-effect and rural–urban sensitivity analyses. It does not justify an unrestricted permutation of treatment across the observed wards.

## Bengaluru and the second Chandigarh cycle

The analysis reads the reviewed Bengaluru panel at `../local_elections/data/karnataka/build/urban_expansion/ward_panel_2010_2015.parquet` and Chandigarh's canonical `data/chandigarh/release/ward_panel_2011_2016.parquet` upstream. Both preserve original-source provenance and stable ward identifiers. Bengaluru retains 101 later nonquota wards: 25 previously reserved and 76 previously nonreserved. Among resolved outcomes, women won 0/24 and 3/76 respectively. Ward 112 has conflicting sex sources and remains missing. The complete-roster risk difference ranges from −3.95 to 0.05 percentage points. Caste adjustments, unchanged-caste restriction, and exclusion of conflicting primary/secondary quota codes are separate sensitivities.

Chandigarh's 2016 comparison has no woman winner in either arm (0/6 and 0/11). The raw difference is zero, but its uncertainty is not. All urban ward contrasts use Newcombe intervals combining independent Wilson score intervals (Newcombe 1998, DOI `10.1002/(SICI)1097-0258(19980430)17:8<873::AID-SIM779>3.0.CO;2-I`). The point estimate remains the raw difference. For inverse-variance synthesis only, each arm's event proportion is regularized as `(x + 0.5)/(n + 1)`, and the variance is the sum of `p*(1-p)/n` across arms. `metafor::escalc(measure="RD", add=.5, to="all", addyi=FALSE, addvi=TRUE, vtype="UB")` implements this formula; despite the API label, the regularized variance is not asserted unbiased or conservative. A sensitivity uses one instead of one-half added to each cell. The score intervals are not converted into standard errors.

Chandigarh's two comparisons are averaged within city, with maximum feasible positive correlation as the primary variance assumption. Bengaluru adds one independent geographic setting; Chandigarh adds no new setting. The synthesis therefore has four urban and three rural settings. `tabs/additional_urban_expansion.csv` separates changes due to data from changes due to the variance method: the original six-setting HC2 analysis, the same data with regularized variances, each addition alone, and both additions. `tabs/additional_urban_missing_outcomes.csv` recomputes the city contrast and rural–urban synthesis for each unresolved outcome value, changing one city's missing outcome at a time.

## Outputs and validation

The upstream Delhi Parquet preserves source and reviewed fields; `bihar_2011_2016.csv` preserves eligibility, linkage scores, source outcomes and recoveries. The `tabs/additional_*` CSVs retain coefficients in probability units; TeX tables, macros and figures use percentage points. `additional_all_estimates.csv` supplies all 26 distinct primary contrasts, including the Rajasthan and UP history contrasts tabulated separately: `figs/additional_persistence.pdf` for next-election comparisons and `figs/additional_histories.pdf` for the additional settings’ longer histories and follow-up outcomes. Original standard errors are retained separately from the displayed bootstrap intervals. `additional_samples.tex` combines the additional microdata sample counts; `additional_sensitivity.tex` combines alternative specifications. These tables have distinct roles and do not repeat every forest-plot result.

The exploratory meta-analysis is specified in [synthesis_design.md](synthesis_design.md). `additional_meta_inputs.csv` contains 13 next-election contrasts; `additional_meta_settings.csv` records seven setting averages, assumed sampling variances, and normalized random-effects weights. `additional_meta_sensitivity.csv/.tex` reports all correlation, geographic, duration, and election-selection sensitivities. The manuscript quotes the pooled estimate and interval through generated macros. This is a selected-settings descriptive synthesis, not a national causal effect or systematic review. The primary within-setting correlation is assumed. A sensitivity uses estimated joint block covariance for Rajasthan and UP. Bihar is included in a separate eight-setting sensitivity; overlapping Birbhum and cumulative-history contrasts are not counted as independent studies. Numerical results and labels are generated from the same estimates.

The implementation follows the official [`metafor::rma.uni` documentation](https://wviechtb.github.io/metafor/reference/rma.uni.html): REML and `test="adhoc"`, which bounds the Knapp–Hartung variance multiplier below by one. The absolute REML convergence threshold is tightened to `1e-12` because the inputs are in probability units; the default `1e-5` threshold left differences detectable by independent likelihood optimization. The validation independently minimizes the restricted likelihood and rederives the weighted mean, variance multiplier, and t intervals without using `metafor`. The sensitivity plan is retrospective, recorded before fitting the pooled models but after observing the component results.

The validation script checks source identities, eligibility, recoveries, independent arm-wise differences, explicit-matrix HC2 and clustered covariance, missing-outcome bounds, published-value transcription, independent REML and modified Knapp–Hartung calculations, complete overview coverage, and agreement with unchanged main-paper estimates. The manuscript contains data, design and findings; reproduction commands and file locations are confined to this documentation.

## Direct rural–urban test

`additional_rural_urban.csv/.tex` reports a meta-regression of the same setting
averages on an intercept and a rural indicator (Mumbai/Delhi/Chandigarh/Bengaluru urban;
Rajasthan/UP/West Bengal rural). It fits one common residual heterogeneity
variance by REML and uses modified Knapp–Hartung t(k−2) inference. The
rural-minus-urban coefficient, its interval, and its two-sided p-value are
exported as macros for both the main Results section and the rural–urban appendix. All pooled
sensitivity cases are also fitted with the moderator. Bihar adds a fourth rural
setting; omitting a city leaves three urban settings and is an influence
diagnostic. The design was recorded retrospectively before fitting this model.

Validation independently reconstructs the two-column weighted design matrix,
minimizes the restricted likelihood, and derives the moderator coefficient,
modified variance, interval, and p-value for every sensitivity. An independent
Python/SciPy review also reproduces the primary estimate. The test concerns
differences in reservation-persistence contrasts, not baseline officeholding
rates or the causal effect of urbanization.

The former manuscript copies under `mumbai_1997_2002/` and `delhi_reservation_order_2012.pdf` were removed after byte-for-byte comparison with their upstream originals. The derived `delhi_2007_2012.csv` was replaced by direct consumption of the upstream typed panel after checking all assignments, outcomes and missingness.

Joint completion of the Delhi and Bengaluru missing outcomes is retained in `tabs/additional_urban_joint_missing_outcomes.csv` (all four combinations).
