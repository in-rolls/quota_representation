# Reservation transitions and female follow-up officeholding in the 2003 term.
# Writes wb/tabs/, wb/figs/ and the generated wb/RESULTS.md.

wb_officeholding_sample_flow <- function(gp03) {
  steps <- list(
    "All reservation-roster GPs" = rep(TRUE, nrow(gp03)),
    "Not reserved for women in 2003" = gp03$q03 == 0,
    "Explicit 2003 election year" = gp03$explicit03,
    "Explicit 1998 previous term" = gp03$explicit98,
    "No detected 1998 category conflict" = !gp03$cross_source_conflict98,
    "Current-Pradhan survey available" = gp03$survey_observed,
    "Survey GP identifiers agree" = !is.na(gp03$id_consistent) & gp03$id_consistent,
    "Visit year is 2006" = gp03$survey_year_valid,
    "Non-conflicting observed sex" = !is.na(gp03$female_followup)
  )
  keep <- rep(TRUE, nrow(gp03))
  rows <- lapply(seq_along(steps), function(i) {
    previous <- keep
    criterion <- steps[[i]]
    criterion[is.na(criterion)] <- FALSE
    keep <<- keep & criterion
    removed <- previous & !keep
    data.frame(
      step = names(steps)[i],
      removed = sum(removed),
      remaining = sum(keep),
      gp_names = paste(gp03$gp[removed], collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

wb_officeholding <- function(d, out) {
  g <- d$gp98[d$gp98$source_complete, ]
  first <- wb_estimate(g, "female98", "q98", "q98", "1998 first stage")
  first$published_difference <- 0.935
  first$archive_control_women <- sum(g$q98 == 0 & g$female98 == 1)
  first$archive_controls <- sum(g$q98 == 0)
  wb_table(first, "cd_first_stage", out)
  hist <- d$gp03[d$gp03$explicit03 & d$gp03$history_strict, ]
  transitions <- dplyr::bind_rows(lapply(c("q", "sc", "st"), function(category) {
    tab <- as.data.frame(table(
      prior = factor(hist[[paste0(category, "98")]], levels = 0:1),
      current = factor(hist[[paste0(category, "03")]], levels = 0:1)
    ))
    tab$category <- category
    tab$row_share <- ave(tab$Freq, tab$prior, FUN = function(n) n / sum(n))
    tab
  }))
  wb_table(transitions, "reservation_transitions", out)
  sample_flow <- wb_officeholding_sample_flow(d$gp03)
  stopifnot(identical(sample_flow$remaining, c(
    165L, 110L, 109L, 99L, 99L,
    96L, 95L, 95L, 94L
  )))
  wb_table(sample_flow, "sample_flow", out)
  eligible <- d$gp03$strict_history03 & d$gp03$q03 == 0
  x <- d$gp03[eligible, ]
  adj <- "q98 + sc98 + st98 + sc03 + st03 + factor(block)"
  same <- x[stats::complete.cases(x[c(
    "female_followup", "q98", "sc98", "st98",
    "sc03", "st03", "block"
  )]), ]
  estimates <- dplyr::bind_rows(
    wb_estimate(x, "female_followup", "q98", "q98", "Open offices, unadjusted"),
    wb_estimate(same, "female_followup", "q98", "q98", "Adjusted complete cases, unadjusted"),
    wb_estimate(same, "female_followup", adj, "q98", "Open offices, adjusted")
  )
  wb_table(estimates, "followup_officeholding", out)
  support <- wb_support(same, "female_followup", adj, "q98", "Adjusted")
  wb_table(support, "followup_leverage", out)
  blocks <- same |>
    dplyr::group_by(block) |>
    dplyr::summarise(
      n = dplyr::n(), prior_reserved = sum(q98 == 1),
      prior_unreserved = sum(q98 == 0), .groups = "drop"
    )
  wb_table(blocks, "followup_block_support", out)
  loo <- dplyr::bind_rows(lapply(sort(unique(same$block)), function(b) {
    z <- same[same$block != b, ]
    r <- wb_estimate(z, "female_followup", adj, "q98", "Leave one block out")
    r$omitted_block <- b
    r
  }))
  wb_table(loo, "followup_leave_one_block_out", out)
  wb_selection_audit(d, out)
  within <- d$gp03[
    d$gp03$strict03 & d$gp03$previous_within_1998_term &
      !d$gp03$cross_source_conflict98 & d$gp03$q03 == 0,
  ]
  repair <- d$gp03[
    d$gp03$explicit03 & d$gp03$survey_observed & d$gp03$survey_year_valid &
      d$gp03$history_strict & d$gp03$q03 == 0,
  ]
  sensitivity <- dplyr::bind_rows(
    wb_estimate(within, "female_followup", "q98", "q98", "Include inferred 1998 terms"),
    wb_estimate(repair, "female_followup", "q98", "q98", "Allow interview-ID repair"),
    wb_estimate(x[x$exact_gp_name, ], "female_followup", "q98", "q98", "Exact GP spelling")
  )
  wb_table(sensitivity, "followup_sensitivity", out)
  wb_table(dplyr::bind_rows(
    wb_ladder(x, "female_followup", "q98", "q98", "Unadjusted"),
    wb_ladder(same, "female_followup", adj, "q98", "Adjusted")
  ), "followup_se_ladder", out)
  wb_table(wb_power(estimates$estimate[1], estimates$std.error[1]), "power_scenarios", out)
  counts <- x |>
    dplyr::group_by(q98) |>
    dplyr::summarise(
      eligible_gps = dplyr::n(), observed_sex = sum(!is.na(female_followup)),
      female_officeholders = sum(female_followup == 1, na.rm = TRUE),
      share = mean(female_followup, na.rm = TRUE), .groups = "drop"
    )
  wb_table(counts, "open_office_counts", out)
  figure <- estimates[c(1, 3), ]
  figure[c("estimate", "conf.low", "conf.high")] <-
    lapply(figure[c("estimate", "conf.low", "conf.high")], function(z) 100 * z)
  wb_plot(
    figure, "followup_officeholding", out,
    "1998 quota and women's follow-up officeholding",
    "Difference in percentage points (95% interval)"
  )
  writeLines(c(
    "# West Bengal: reservation history and women's officeholding", "",
    "Generated by scripts/01_officeholding.R. See pap.md and data/validation.json.", "",
    "The outcome is a female current Pradhan in the 2006 follow-up survey, among offices",
    "not reserved for women in 2003. It is not a verified original-election winner record.",
    "SC/ST office reservations may remain. Gender comes from explicit questionnaire fields;",
    "contradictory sex fields remain missing. No name-based gender inference is used.", "",
    as.character(knitr::kable(counts, digits = 3)), "",
    as.character(knitr::kable(estimates[c("model", "estimate", "conf.low", "conf.high", "n", "treated_units")],
      digits = 3
    )), "",
    "Coefficients above are probability differences. These are exploratory associations:",
    "rotation and changing caste strata can make selection into open offices depend on prior",
    "treatment, and current officeholders need not be original winners. Block effects do not",
    "establish assignment independence. Wide intervals do not demonstrate no persistence.", "",
    "Transition counts, identity/history sensitivities, SE comparisons and normal-model power",
    "scenarios are in tabs/. There is only one prior-to-current transition; no long-run claim is made."
  ), file.path(out, "RESULTS.md"))
  invisible(estimates)
}
