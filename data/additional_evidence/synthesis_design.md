# Exploratory synthesis design

Recorded November 5, 2026, before fitting the pooled models, after seeing all
component estimates. This is a retrospective synthesis of the assembled evidence,
not a preregistered analysis or a systematic literature review.

The target is the average association between prior women's reservation and female
officeholding at the next election without a women's quota, across the included
geographic settings. It is not a national population average or an identified
common causal effect. Selection into open offices, observational comparisons,
different offices and differing control histories remain substantive limitations.

The primary synthesis includes Rajasthan, Uttar Pradesh, Mumbai, Delhi, and West
Bengal. Average the three adjacent-election estimates equally within Rajasthan
and Uttar Pradesh, and the two next-election appointment estimates equally within
West Bengal (1998 to 2003 and 2003-only to 2008). These are next-election contrasts,
not necessarily exactly one lifetime term of exposure: Rajasthan and Uttar Pradesh
do not restrict earlier histories. West Bengal's 2008 comparison holds earlier
exposure absent. Mumbai and Delhi each supply one estimate. Keep full-sample,
fixed-effects main-paper estimates and source standard errors; do not derive
standard errors from inverted bootstrap intervals.

For m estimates in a setting, use the arithmetic mean and sampling variance
[(1-rho) sum(se^2) + rho (sum(se))^2] / m^2. Cross-election covariances are not
available in the assembled estimates. The primary working correlation is rho=1,
the upper bound on the sampling variance of this mean given the marginal standard
errors. This does NOT guarantee a conservative random-effects confidence interval.
Repeat at rho=0, .5, and the setting-specific equicorrelation lower PSD boundary
-1/(m-1). Report that dependence is assumed, not estimated.

Fit one REML random-effects model to the five setting averages. Use inverse
(sampling variance + between-setting variance) weights and a modified
Knapp--Hartung interval: its variance multiplier is at least one, with t(k-1)
critical values. Geographic settings, not election rows, determine k. Assume
independent sampling errors across settings; shared national shocks and common
measurement errors are not addressed. With five selected settings, pooled
inference and heterogeneity estimates are necessarily model-dependent.

Sensitivity analyses: include Bihar's withdrawal under a two-election quota
regime; exclude West Bengal to retain directly elected offices; omit each setting
in turn; use only the earliest or latest available next-election contrast per
setting. Retain all results in the machine-readable output. No result-dependent
choice of setting, wave, model, or reported standard error.

Display all 22 distinct primary contrasts across two figures on a common scale:
10 next-election contrasts plus the pooled summary; 8 main-paper cumulative
history contrasts, Bihar, 2 longer-history West Bengal appointment contrasts, and
the Birbhum follow-up survey. Do not pool the cumulative contrasts or the survey
as independent studies: they overlap primary samples and measure different
contrasts or timings. Alternative model specifications remain sensitivity
analyses, not additional studies. Tables document sample counts and sensitivity
results; do not duplicate the complete forest plots in regression tables.

Independent read-only design review supported the five-setting primary and
identified the distinction between adjacent-election and exactly-one-term
exposure. That correction is incorporated here.

## Direct rural–urban comparison (November 6, 2026)

Added at the author's request after the overall synthesis and all component
estimates were known. The target is the rural-minus-urban difference between
setting-average next-election reservation contrasts. Classify Mumbai and Delhi
as urban, Rajasthan, Uttar Pradesh and West Bengal as rural; Bihar is rural in
the broader-duration sensitivity. Use the existing setting averages and assumed
sampling variances. Fit a REML meta-regression with an intercept and a rural
indicator, one common residual between-setting variance, and modified
Knapp–Hartung inference with k-2 degrees of freedom. Report the coefficient,
95% confidence interval and two-sided p-value for a zero rural–urban difference.
Use the same dependence, geographic and temporal sensitivities as the pooled
analysis. Do not infer a group difference by comparing separate significance
levels. This comparison describes five selected settings, including only two
cities; geography is confounded with office type, assignment rules and time.
The moderator is not randomized and cannot identify an effect of urbanization.

## Urban extension (November 9, 2026)

The source search added Chandigarh's 2006–2011 and 2011–2016 comparisons and Bengaluru's 2010–2015 comparison. The updated synthesis has 13 next-election contrasts averaged into seven settings: four urban and three rural. Chandigarh contributes one city average across its two cycles. UP urban head elections remain a separate descriptive comparison because geographic continuity and comparable assignment are unverified.

The newly validated Chandigarh 2016 sample has no woman winner in either group. Its ordinary plug-in variance collapses to zero. After observing that boundary case, we changed every unadjusted urban ward interval to Newcombe–Wilson and every urban variance used in synthesis to the same half-cell regularization, preserving raw risk differences. This is a retrospective inference amendment, not a precommitted method. The main rural and published West Bengal estimates and standard errors remain as specified above. A one-cell correction and omission of each city check sensitivity. The existing within-setting dependence assumptions still apply.

The expansion table separates changes in data from this variance change by reproducing the old six-setting HC2 synthesis, applying the variance amendment to those same data, and then adding each new comparison separately and together. The source inventory determines inclusion, without choosing settings by direction or significance. Both unresolved urban outcomes are evaluated separately and jointly over all binary completions. The complete overview now retains 26 contrasts, including the separate UP urban comparison and the longer-history results.


## Election-weight and covariance comparison (November 9, 2026)

Added after inspecting the expanded results, at the author's request. Keep the
existing correlation-one calculation as a reference and report five alternatives
in one table for both the overall contrast and the rural-minus-urban difference.
All alternatives use the estimated joint block-cluster covariance for Rajasthan
and UP. Equal election weights use assumed correlations of zero, .5, and one
only for West Bengal and Chandigarh. GLS election weights use zero and .5 for
those two settings; a correlation-one covariance matrix is singular and is not
inverted. No unknown correlation is claimed to have been estimated.

For a within-setting covariance matrix V, GLS weights are
V^{-1}1 / (1'V^{-1}1), with sampling variance a'Va. They minimize sampling
variance for a common within-setting effect. With heterogeneous election effects,
they define a different weighted contrast from the equal-election average.
This is a two-stage sensitivity analysis, not a fitted multilevel model of
within-setting effect heterogeneity. Both stages retain exactly the same source
contrasts, urban variance regularization, and seven geographic settings. The
second stage retains REML and modified Knapp–Hartung inference (six degrees of
freedom overall, five for the rural–urban coefficient). Export every election's
weight and the setting estimate and variance for each specification. The compact
comparison does not replace or select among results according to significance.
