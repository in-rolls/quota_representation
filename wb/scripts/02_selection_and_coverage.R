# Explore sample selection, binary-outcome bounds and adjustment choices.
# Writes wb/data/selection_frame.*, wb/tabs/selection_*, and wb/SELECTION_RESULTS.md.

wb_selection_frame <- function(gp03) {
  wb_assert_key(gp03, "gp_id")
  x <- gp03[c(
    "gp_id", "gp", "block", "q98", "q03", "sc98", "st98", "sc03", "st03",
    "explicit03", "history_strict", "survey_observed", "id_consistent",
    "survey_year_valid", "sex_conflict", "female_followup", "strict_history03"
  )]
  x$history_eligible <- x$explicit03 & x$history_strict &
    !is.na(x$q98) & !is.na(x$q03)
  x$measurement_valid <- x$survey_observed & !is.na(x$id_consistent) &
    x$id_consistent & x$survey_year_valid & !is.na(x$female_followup)
  x$measurement_valid[is.na(x$measurement_valid)] <- FALSE
  x$audited_outcome <- ifelse(x$measurement_valid, x$female_followup, NA_real_)
  x$exclusion_reason <- ifelse(!x$history_eligible, "history_not_verified",
    ifelse(!x$survey_observed, "no_current_interview",
      ifelse(is.na(x$id_consistent) | !x$id_consistent, "interview_id_unresolved",
        ifelse(!x$survey_year_valid, "visit_year_unresolved",
          ifelse(is.na(x$female_followup), "sex_unresolved", "observed")
        )
      )
    )
  )
  stopifnot(nrow(x) == nrow(gp03), !anyNA(x$history_eligible))
  x
}

wb_binary_bounds <- function(x) {
  stopifnot(all(x$q98 %in% 0:1), all(x$audited_outcome %in% c(0, 1, NA)))
  arms <- dplyr::bind_rows(lapply(0:1, function(a) {
    y <- x$audited_outcome[x$q98 == a]
    stopifnot(length(y) > 0L)
    observed <- sum(!is.na(y))
    women <- sum(y == 1, na.rm = TRUE)
    data.frame(
      q98 = a, frame_gps = length(y), observed_gps = observed,
      missing_gps = sum(is.na(y)), observed_women = women,
      observed_share = if (observed > 0) women / observed else NA_real_,
      lower_share = women / length(y),
      upper_share = (women + sum(is.na(y))) / length(y)
    )
  }))
  contrast <- data.frame(
    frame_gps = sum(arms$frame_gps), observed_gps = sum(arms$observed_gps),
    observed_difference = arms$observed_share[2] - arms$observed_share[1],
    lower_difference = arms$lower_share[2] - arms$upper_share[1],
    upper_difference = arms$upper_share[2] - arms$lower_share[1],
    uncertainty = "Finite-roster identification bounds; not a sampling CI or causal bound"
  )
  list(arms = arms, contrast = contrast)
}

wb_selection_extension <- function(d, out) {
  x <- wb_selection_frame(d$gp03)
  wb_save(x, "selection_frame", out)
  frame <- x[x$history_eligible, ]
  histories <- frame |>
    dplyr::group_by(q98, q03) |>
    dplyr::summarise(
      frame_gps = dplyr::n(), observed_gps = sum(!is.na(audited_outcome)),
      missing_gps = sum(is.na(audited_outcome)),
      observed_women = sum(audited_outcome == 1, na.rm = TRUE),
      female_share = if (sum(!is.na(audited_outcome)) > 0) mean(audited_outcome, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  wb_table(histories, "selection_history_outcomes", out)
  opening <- frame |>
    dplyr::group_by(q98) |>
    dplyr::summarise(
      history_frame_gps = dplyr::n(), women_open_2003 = sum(q03 == 0),
      open_share = mean(q03 == 0), .groups = "drop"
    )
  wb_table(opening, "selection_into_open_offices", out)
  reasons <- x |>
    dplyr::group_by(q98, q03, exclusion_reason) |>
    dplyr::summarise(gps = dplyr::n(), .groups = "drop")
  wb_table(reasons, "selection_exclusions", out)
  opened <- frame[frame$q03 == 0, ]
  bounds <- wb_binary_bounds(opened)
  wb_table(bounds$arms, "selection_missing_outcome_arms", out)
  wb_table(bounds$contrast, "selection_missing_outcome_bounds", out)
  common <- opened[stats::complete.cases(opened[c(
    "audited_outcome", "q98", "sc98", "st98", "sc03", "st03", "block"
  )]), ]
  stopifnot(setequal(common$gp_id, x$gp_id[x$strict_history03 & x$q03 == 0 & x$measurement_valid]))
  formulas <- c(
    "No adjustment" = "q98",
    "1998 caste strata and block" = "q98 + sc98 + st98 + factor(block)",
    "Also condition on 2003 caste quotas" = "q98 + sc98 + st98 + sc03 + st03 + factor(block)"
  )
  estimates <- dplyr::bind_rows(lapply(names(formulas), function(label) {
    dplyr::bind_rows(lapply(c("HC2", "HC3"), function(se) {
      wb_estimate(common, "audited_outcome", formulas[[label]], "q98", label, se_type = se)
    }))
  }))
  wb_table(estimates, "selection_adjustment_comparison", out)
  support <- dplyr::bind_rows(lapply(names(formulas), function(label) {
    wb_support(common, "audited_outcome", formulas[[label]], "q98", label)
  }))
  wb_table(support, "selection_adjustment_support", out)
  primary <- estimates[estimates$model == "No adjustment" & estimates$se_type == "HC2", ]
  independently <- with(common, mean(audited_outcome[q98 == 1]) - mean(audited_outcome[q98 == 0]))
  stopifnot(abs(primary$estimate - independently) < 1e-12)
  figure <- estimates[estimates$se_type == "HC2", ]
  figure$model <- factor(figure$model, levels = rev(names(formulas)))
  figure[c("estimate", "conf.low", "conf.high")] <-
    lapply(figure[c("estimate", "conf.low", "conf.high")], function(z) 100 * z)
  wb_plot(figure, "selection_adjustment", out,
    "WB follow-up officeholding: adjustment sensitivity",
    "Prior-quota association in percentage points (HC2 95% interval)"
  )
  b <- bounds$contrast
  writeLines(c(
    "# WB: what selection and adjustment change", "",
    "Generated by scripts/02_selection_and_coverage.R. Exploratory descriptive analysis; see extension_plan.md.", "",
    "The outcome is a female current Pradhan observed in 2006, not a verified 2003 election winner.",
    "One GP is one observation. The source frame is retained before excluding missing interviews or outcomes.", "",
    sprintf("The explicit-history frame contains %d GPs; %d were not women-reserved in 2003.",
      nrow(frame), nrow(opened)
    ),
    sprintf("Of those %d open offices, %d have usable sex measurements and %d are unobserved or unresolved.",
      nrow(opened), sum(opened$measurement_valid), sum(!opened$measurement_valid)
    ), "",
    as.character(knitr::kable(bounds$arms, digits = 3)), "",
    sprintf(paste(
      "Allowing each unresolved outcome to be either male or female gives a difference",
      "within the history-verified, women-open roster bounded by %.2f to %.2f percentage points."
    ),
    100 * b$lower_difference, 100 * b$upper_difference
    ),
    "These are sharp binary-outcome bounds for this finite roster. They are not confidence intervals,",
    "do not solve confounding or selection into a women-open office, and do not bound the causal effect.", "",
    sprintf("The observed-case difference is %.2f percentage points (HC2 95%% interval %.2f to %.2f; n=%d).",
      100 * primary$estimate, 100 * primary$conf.low, 100 * primary$conf.high, primary$n
    ), "",
    as.character(knitr::kable(
      estimates[c("model", "se_type", "estimate", "conf.low", "conf.high", "n")], digits = 3
    )), "",
    "All adjustment comparisons use the same complete cases. The 1998 caste categories describe the allocation",
    "strata; the model additionally conditioning on 2003 caste categories is a post-exposure sensitivity.",
    "Neither model establishes random assignment. HC2/HC3 quantify model-based uncertainty",
    "conditional on the sample.", "",
    "## Exposure histories and selection", "",
    as.character(knitr::kable(opening, digits = 3)), "",
    as.character(knitr::kable(histories, digits = 3)), "",
    "The four history cells are not a factorial experiment. Differences in later quota eligibility can reflect",
    "the rotation rule and changing caste strata. No sample is selected using an independence-test p-value.", "",
    "New official-source descriptions are in OFFICIAL_SOURCES.md. They are not appended to this follow-up model."
  ), file.path(out, "SELECTION_RESULTS.md"))
  invisible(list(frame = x, estimates = estimates, bounds = bounds))
}
