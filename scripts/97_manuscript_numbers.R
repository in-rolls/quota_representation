library(tidyverse)
library(here)
source(here("scripts/00_config.R"))

inference <- read_csv(here("tabs/model_inference.csv"), show_col_types = FALSE) |>
  filter(fixed_effects, variance == "cluster")
numbers <- character()
broad_balance <- read_csv(here("tabs/census_balance.csv"), show_col_types = FALSE) |>
  filter(comparison == "raj_05_10") |>
  distinct(joint_p)
stopifnot(nrow(broad_balance) == 1L)
numbers["broadRajFirstP"] <- format_p_value(broad_balance$joint_p, latex = FALSE)
pooled <- read_csv(here("tabs/short_term_pooled.csv"), show_col_types = FALSE)
pool_primary <- pooled |> filter(specification == "Equal states and transitions")
stopifnot(nrow(pool_primary) == 1L)
for (name in c("estimate", "conf_low", "conf_high", "control_mean")) {
  macro <- c(estimate = "poolEstimate", conf_low = "poolLow", conf_high = "poolHigh", control_mean = "poolControl")[[name]]
  numbers[macro] <- format_number(100 * pool_primary[[name]])
}
numbers["poolSampleWeighted"] <- format_number(100 * pooled$estimate[pooled$specification == "Analysis-sample weights"])
leave_election <- pooled |> filter(grepl("^Omit (raj|up)_", specification))
numbers["poolOmitMin"] <- format_number(100 * min(leave_election$estimate))
numbers["poolOmitMax"] <- format_number(100 * max(leave_election$estimate))
numbers["poolRaj"] <- format_number(100 * pooled$estimate[pooled$specification == "Omit Uttar Pradesh"])
numbers["poolUp"] <- format_number(100 * pooled$estimate[pooled$specification == "Omit Rajasthan"])
linked_gp_counts <- c(raj = 0L, up = 0L)
for (state in names(linked_gp_counts)) {
  period <- if (state == "raj") "05_20" else "05_21"
  panel <- read_analysis_parquet(election_panel_path(state, paste0(state, "_", period, ".parquet")))
  key <- if (state == "raj") "match_key" else "gp_id"
  stopifnot(!anyNA(panel[[key]]), !anyDuplicated(panel[[key]]))
  linked_gp_counts[[state]] <- nrow(panel)
}
numbers["linkedGpCount"] <- format(sum(linked_gp_counts), big.mark = ",", trim = TRUE)
numbers["abstractGpCount"] <- format(floor(sum(linked_gp_counts) / 1000) * 1000, big.mark = ",", trim = TRUE)
for (state in c("raj", "up")) {
  periods <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (i in seq_along(periods)) {
    row <- inference |> filter(.data$state == .env$state, period == periods[[i]], sample == "full")
    prefix <- paste0(state, "Short", c("One", "Two", "Three")[[i]])
    numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
    numbers[paste0(prefix, "Low")] <- sprintf("%.1f", 100 * row$conf_low)
    numbers[paste0(prefix, "High")] <- sprintf("%.1f", 100 * row$conf_high)
  }
  for (sample in c("full", "restricted")) {
    rows <- inference |> filter(
      .data$state == .env$state, .data$sample == .env$sample,
      contrast %in% c("111-000", "111-001")
    )
    for (contrast in c("111-000", "111-001")) {
      row <- rows |> filter(.data$contrast == .env$contrast)
      prefix <- paste0(
        state, if (sample == "full") "Full" else "Restricted",
        if (contrast == "111-000") "Always" else "Extra"
      )
      numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
      numbers[paste0(prefix, "Low")] <- sprintf("%.1f", 100 * row$bootstrap_low)
      numbers[paste0(prefix, "High")] <- sprintf("%.1f", 100 * row$bootstrap_high)
      numbers[paste0(prefix, "N")] <- format(row$n, big.mark = ",", scientific = FALSE, trim = TRUE)
      numbers[paste0(prefix, "LeftN")] <- format(row$left_n, big.mark = ",", trim = TRUE)
      numbers[paste0(prefix, "RightN")] <- format(row$right_n, big.mark = ",", trim = TRUE)
      numbers[paste0(prefix, "SharedClusters")] <- as.character(row$common_clusters)
    }
  }
}
short <- inference |> filter(sample == "full", !str_detect(contrast, "-"))
stopifnot(nrow(short) == 6L, all(is.finite(short$bootstrap_high)))
numbers["abstractShortMin"] <- sprintf("%.0f", 100 * min(short$estimate))
numbers["abstractShortMax"] <- sprintf("%.0f", 100 * max(short$estimate))
numbers["abstractExcludedGain"] <- as.character(ceiling(100 * max(short$bootstrap_high)))
numbers["shortMdeMin"] <- sprintf("%.1f", 100 * min(short$mde_80))
numbers["shortMdeMax"] <- sprintf("%.1f", 100 * max(short$mde_80))
up_baselines <- short |> filter(state == "up") |> pull(right_mean)
numbers["upControlMin"] <- sprintf("%.0f", 100 * min(up_baselines))
numbers["upControlMax"] <- sprintf("%.0f", 100 * max(up_baselines))
long <- inference |> filter(sample == "full", contrast == "111-000")
stopifnot(nrow(long) == 2L, all(is.finite(long$left_mean)))
numbers["abstractLongMin"] <- sprintf("%.0f", 100 * min(long$estimate))
numbers["abstractLongMax"] <- sprintf("%.0f", 100 * max(long$estimate))
numbers["abstractLongWomenMin"] <- sprintf("%.0f", 100 * min(long$left_mean))
numbers["abstractLongWomenMax"] <- sprintf("%.0f", 100 * max(long$left_mean))
numbers["longMdeMin"] <- sprintf("%.1f", 100 * min(long$mde_80))
numbers["longMdeMax"] <- sprintf("%.1f", 100 * max(long$mde_80))
two_terms <- inference |> filter(sample == "full", contrast %in% c("101-001", "011-001"))
stopifnot(nrow(two_terms) == 4L, all(is.finite(two_terms$estimate)),
  all(two_terms$bootstrap_low <= 0), all(two_terms$bootstrap_high >= 0))
numbers["twoTermExtraMin"] <- sprintf("%.1f", 100 * min(two_terms$estimate))
numbers["twoTermExtraMax"] <- sprintf("%.1f", 100 * max(two_terms$estimate))
next_samples <- read_csv(here("tabs/next_election_samples.csv"), show_col_types = FALSE)
stopifnot(nrow(next_samples) == 6L)
numbers["controlPriorWomenMin"] <- sprintf("%.1f", 100 * min(next_samples$control_women_share))
numbers["controlPriorWomenMax"] <- sprintf("%.1f", 100 * max(next_samples$control_women_share))
for (state in c("raj", "up")) {
  restricted <- inference |> filter(.data$state == .env$state, sample == "restricted", !str_detect(contrast, "-"))
  periods <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15")
  for (i in seq_along(periods)) {
    row <- restricted |> filter(period == periods[[i]])
    stopifnot(nrow(row) == 1L)
    prefix <- paste0(state, "RestrictedShort", c("One", "Two", "Three")[[i]])
    numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
    numbers[paste0(prefix, "N")] <- format(row$n, big.mark = ",", trim = TRUE)
    numbers[paste0(prefix, "Low")] <- sprintf("%.1f", 100 * row$conf_low)
    numbers[paste0(prefix, "High")] <- sprintf("%.1f", 100 * row$conf_high)
  }
}
covariate_estimates <- read_csv(here("tabs/short_term_covariate_estimates.csv"), show_col_types = FALSE)
sample_checks <- read_csv(here("tabs/short_term_sample_comparisons.csv"), show_col_types = FALSE) |>
  select(state, period, specification, estimate) |>
  pivot_wider(names_from = specification, values_from = estimate)
numbers["maxWithinSampleAdjustment"] <- sprintf("%.1f", 100 * max(abs(sample_checks$adjusted - sample_checks$common)))
allocation_checks <- read_csv(here("tabs/short_term_allocation_checks.csv"), show_col_types = FALSE)
caste_row <- allocation_checks |> filter(state == "raj", period == "10_15", specification == "treatment_block_caste")
stopifnot(nrow(caste_row) == 1L)
numbers["rajCasteShortTwo"] <- sprintf("%.1f", 100 * caste_row$estimate)

contrast_order <- c("101-001", "011-001", "111-001")
contrast_labels <- c("One extra term, separated", "One extra term, consecutive", "Two extra terms")
contrast_dates <- c("(2005 added)", "(2010 added)", "(2005 and 2010 added)")
main_contrasts <- inference |>
  filter(contrast %in% contrast_order, sample %in% c("full", "restricted"))
stopifnot(nrow(main_contrasts) == 12L,
  !anyDuplicated(main_contrasts[c("state", "contrast", "sample")]),
  !anyNA(main_contrasts[c("estimate", "bootstrap_low", "bootstrap_high")]))
contrast_rows <- lapply(seq_along(contrast_order), function(i) {
  estimates <- intervals <- character()
  for (state in c("raj", "up")) {
    for (sample in c("full", "restricted")) {
      row <- main_contrasts |>
        filter(.data$state == .env$state, .data$sample == .env$sample, contrast == contrast_order[i])
      stopifnot(nrow(row) == 1L)
      estimates <- c(estimates, format_number(100 * row$estimate))
      intervals <- c(intervals, sprintf("[%s, %s]", format_number(100 * row$bootstrap_low),
        format_number(100 * row$bootstrap_high)))
    }
  }
  rbind(c(contrast_labels[i], estimates), c(contrast_dates[i], intervals))
})
contrast_table <- do.call(rbind, contrast_rows) |>
  knitr::kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lrrrr", linesep = "",
    col.names = c("Added reservation", "Full", "Restricted", "Full", "Restricted")) |>
  kableExtra::add_header_above(c(" " = 1, "Rajasthan (2020)" = 2, "Uttar Pradesh (2021)" = 2)) |>
  kableExtra::row_spec(c(2, 4), extra_latex_after = "\\addlinespace[0.5em]")
writeLines(contrast_table, here("tabs/additional_term_main.tex"))

for (i in c(2L, 4L, 8L)) {
  row <- covariate_estimates |> filter(model == i, str_detect(term, "^treat_"))
  stopifnot(nrow(row) == 1L)
  prefix <- c(`2` = "rajAdjustedShortOne", `4` = "rajAdjustedShortTwo", `8` = "upAdjustedShortOne")[[as.character(i)]]
  numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
}
weaver <- read_csv(here("tabs/weaver_estimates.csv"), show_col_types = FALSE)
for (i in c(2L, 4L)) {
  row <- weaver |> filter(model == i, str_detect(term, "^reservation_female_"))
  stopifnot(nrow(row) == 1L)
  prefix <- if (i == 2L) "weaverShortOne" else "weaverShortTwo"
  numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
  numbers[paste0(prefix, "N")] <- format(row$n, big.mark = ",", trim = TRUE)
}

candidacy <- read_csv(here("tabs/candidacy_estimates.csv"), show_col_types = FALSE)
candidate_prefix <- c(women_present = "candAny", prop_women = "candShare",
  num_women = "candCount", fem_vote_share = "candVotes")
for (outcome in names(candidate_prefix)) {
  row <- candidacy |> filter(model == paste0(outcome, "_fe"), term == "treat_2015")
  stopifnot(nrow(row) == 1L)
  factor <- if (outcome == "num_women") 1 else 100
  prefix <- candidate_prefix[[outcome]]
  numbers[prefix] <- format_number(factor * row$estimate)
  numbers[paste0(prefix, "Low")] <- format_number(factor * row$conf.low)
  numbers[paste0(prefix, "High")] <- format_number(factor * row$conf.high)
}

quality <- read_csv(here("tabs/candidate_characteristics.csv"), show_col_types = FALSE)
for (sample in c("Winners", "Women winners")) {
  for (variable in c("Age", "Graduate", "Unemployed", "Log assets")) {
    row <- quality |> filter(.data$sample == .env$sample, .data$variable == .env$variable)
    prefix <- paste0(if (sample == "Winners") "all" else "women", str_remove_all(variable, " "))
    factor <- if (variable %in% c("Graduate", "Unemployed")) 100 else 1
    numbers[paste0(prefix, "Open")] <- sprintf("%.1f", factor * row$open)
    numbers[paste0(prefix, "Quota")] <- sprintf("%.1f", factor * row$quota)
  }
}
phone <- read_csv(here("tabs/phone_contact_counts.csv"), show_col_types = FALSE)
phone_sensitivity <- read_csv(here("tabs/phone_contact_sensitivity.csv"), show_col_types = FALSE)
phone_prefix <- c(phone_survey = "phoneQuota", phone_survey_openseats = "phoneOpen",
  jaipur_urban_phone_survey_quota = "jaipurQuota", jaipur_urban_phone_survey_open = "jaipurOpen")
for (survey in names(phone_prefix)) {
  prefix <- phone_prefix[[survey]]
  answered <- phone |> filter(.data$survey == .env$survey, category == "Answered")
  contact <- phone_sensitivity |> filter(.data$survey == .env$survey)
  numbers[paste0(prefix, "Sampled")] <- as.character(answered$denominator)
  numbers[paste0(prefix, "Answered")] <- as.character(answered$n)
  numbers[paste0(prefix, "ResponsePct")] <- sprintf("%.0f", 100 * answered$n / answered$denominator)
  numbers[paste0(prefix, "MemberN")] <- as.character(contact$recorded)
  numbers[paste0(prefix, "MemberPct")] <- sprintf("%.1f", 100 * contact$share)
  numbers[paste0(prefix, "MemberWhole")] <- sprintf("%.0f", 100 * contact$share)
  numbers[paste0(prefix, "Lower")] <- sprintf("%.1f", 100 * contact$lower)
  numbers[paste0(prefix, "Upper")] <- sprintf("%.1f", 100 * contact$upper)
}
rural_contacts <- phone |> filter(survey %in% c("phone_survey", "phone_survey_openseats"), category == "Answered")
numbers["abstractPhoneN"] <- format(round(sum(rural_contacts$denominator), -3), big.mark = ",", trim = TRUE)
quota <- phone |> filter(survey == "phone_survey")
answered_n <- quota$n[quota$category == "Answered"]
male_n <- unique(quota$denominator[quota$section == "Recorded male non-members"])
relatives_n <- sum(quota$n[quota$category %in% c("Spouse", "Child", "Other recorded relative")])
numbers["phoneQuotaMaleN"] <- as.character(male_n)
numbers["phoneQuotaMalePct"] <- sprintf("%.1f", 100 * male_n / answered_n)
numbers["phoneQuotaMaleWhole"] <- sprintf("%.0f", 100 * male_n / answered_n)
numbers["phoneQuotaRelativePct"] <- sprintf("%.0f", 100 * relatives_n / answered_n)
count_names <- c(Spouse = "Spouse", Child = "Child", OtherRelative = "Other recorded relative",
  NonmemberTotal = "Did not transfer", NoTransfer = "Did not transfer", Transferred = "Transferred",
  TransferUnknown = "Transfer status unavailable", TransferNA = "Transfer recorded as not applicable")
for (name in names(count_names)) {
  row <- quota |> filter(category == count_names[[name]])
  numbers[paste0("phoneQuota", name, "N")] <- as.character(if (name == "NonmemberTotal") row$denominator else row$n)
}
respondent_characteristics <- read_csv(here("tabs/phone_respondent_characteristics.csv"), show_col_types = FALSE)
numbers["phoneRespondentAge"] <- sprintf("%.1f", respondent_characteristics$mean[respondent_characteristics$variable == "Age"])
stopifnot(length(numbers) > 0, !anyNA(numbers), !anyDuplicated(names(numbers)))
writeLines(paste0("\\newcommand{\\", names(numbers), "}{", numbers, "}"), here("tabs/numbers.tex"))
