library(tidyverse)
library(arrow)
library(haven)
library(here)
library(kableExtra)
source(here("scripts/00_config.R"))

candidates <- read_raj(raj_product_path("candidates_2020_events.parquet")) |>
  filter(candidate_key_unique, gp_event_unique) |>
  mutate(
    treat = primary_female_reserved,
    education_status = na_if(na_if(str_to_lower(str_trim(education_status)), "nan"), ""),
    grad_status = if_else(is.na(education_status), NA_integer_, as.integer(
      education_status %in% c(
        "postgraduate", "graduate",
        "professional graduate", "professional post graduate"
      )
    )),
    total_children = as.numeric(children_before27111995) + as.numeric(children_on_or_after28111995),
    age_raw = as.numeric(age),
    age = screen_characteristic_age(age_raw),
    unemployed = as.integer(str_to_lower(contesting_candidate_occupation) == "unemployed"),
    assets = as.numeric(total_value_of_capital_assets),
    log_assets = log1p(psych::winsor(assets, trim = 0.1, na.rm = TRUE))
  )
winner_keys <- read_raj(raj_product_path("winners_2020_events.parquet")) |>
  filter(winner_key_unique, gp_event_unique) |>
  select(event_key, name_of_contesting_candidate = winner_candidate_name)
winners <- candidates |>
  filter(candidate_name_unique) |>
  semi_join(winner_keys,
    by = c("event_key", "name_of_contesting_candidate"), na_matches = "never"
  )
variables <- c("age", "total_children", "grad_status", "unemployed", "log_assets")
labels <- c("Age", "Children (recorded)", "Graduate", "Unemployed", "Log assets")

samples <- list(
  Winners = winners, Candidates = candidates,
  `Women winners` = filter(winners, candidate_female == 1),
  `Women candidates` = filter(candidates, candidate_female == 1)
)
age_screen_summary <- function(sample, sample_name, state, year) {
  sample |>
    filter(!is.na(treat)) |>
    summarise(
      source_n = n(), missing_age = sum(is.na(age_raw)),
      excluded_age = sum(!is.na(age_raw) & is.na(age)),
      age_n = sum(!is.na(age)),
      raw_mean = mean(age_raw, na.rm = TRUE),
      screened_mean = mean(age, na.rm = TRUE), .by = treat
    ) |>
    rename(quota = treat) |>
    mutate(state = state, year = year, sample = sample_name, .before = 1) |>
    mutate(minimum_age = 21L, maximum_age = 110L)
}
age_screening <- imap_dfr(samples, ~ age_screen_summary(.x, .y, "Rajasthan", 2020L))
comparisons <- imap_dfr(samples, function(sample, sample_name) {
  map2_dfr(variables, labels, function(outcome, label) {
    observed <- sample |> filter(!is.na(.data[[outcome]]), !is.na(treat))
    test <- t.test(reformulate("treat", outcome), data = observed)
    tibble(
      sample = sample_name, variable = label,
      open = mean(observed[[outcome]][observed$treat == 0]),
      quota = mean(observed[[outcome]][observed$treat == 1]),
      difference = open - quota, p = test$p.value,
      open_n = sum(observed$treat == 0), quota_n = sum(observed$treat == 1)
    )
  })
})
write_csv(comparisons, here("tabs/candidate_characteristics.csv"))
for (women_only in c(FALSE, TRUE)) {
  displayed <- comparisons |>
    filter(str_starts(sample, "Women") == women_only, variable != "Children (recorded)") |>
    mutate(across(c(open, quota, difference), ~ format_number(.x * ifelse(variable %in% c("Graduate", "Unemployed"), 100, 1))),
      variable = if_else(variable %in% c("Graduate", "Unemployed"), paste0(variable, " (%)"), variable)) |>
    select(sample, variable, open, quota, difference) |>
    pivot_wider(
      names_from = sample, values_from = c(open, quota, difference),
      names_vary = "slowest"
    )
  displayed |>
    kbl(
      format = "latex", booktabs = TRUE, digits = 2,
      caption = if (women_only) "Candidate Characteristics Among Women: Quota vs. Open Seats" else "Candidate Characteristics: Quota vs. Open Seats (Rajasthan 2020)",
      label = if (women_only) "candidate_women" else "main_cand_char",
      col.names = c("Characteristic", rep(c("Open", "Quota", "Difference"), 2))
    ) |>
    add_header_above(setNames(
      c(1, 3, 3),
      c(" ", if (women_only) c("Women winners", "Women candidates") else c("Winners", "Candidates"))
    )) |>
    kable_styling(font_size = 8, latex_options = if (women_only) "hold_position" else "basic") |>
    footnote(
      general = paste(
        "Descriptive open-minus-quota comparisons, conditional on candidacy or winning.",
        "Graduation and unemployment are percentages; their differences are percentage points. Other rows use their stated units.",
        "Graduation rates exclude records with missing education.",
        "Age means exclude recorded ages outside 21--110 years; other outcomes retain those records.",
        "Assets winsorized at the 10th and 90th percentiles of the candidate sample before taking logs.",
        "These comparisons do not identify a causal effect on candidate quality."
      ),
      threeparttable = TRUE
    ) |>
    save_kable(here("tabs", if (women_only) "cand_characteristics_women.tex" else "cand_characteristics_combined.tex"))
}

respondent_links <- read_parquet(here("data/raj/phone_contacts_reviewed.parquet")) |>
  filter(survey == "phone_survey", any_answered == "yes", any_officeholder_contact == "yes") |>
  transmute(event_key, survey_name = str_to_lower(str_squish(winner_candidate_name)), excel_row)
respondents <- winners |>
  mutate(survey_name = str_to_lower(str_squish(name_of_contesting_candidate))) |>
  inner_join(respondent_links, by = c("event_key", "survey_name"),
    relationship = "one-to-one", na_matches = "never")
stopifnot(nrow(respondents) == nrow(respondent_links), !anyNA(respondents$event_key))
write_parquet(respondents, here("data/raj/phone_respondents_reviewed.parquet"))
respondent_means <- respondents |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "mean") |>
  mutate(
    n = map_int(variable, ~ sum(!is.na(respondents[[.x]]))),
    variable = labels[match(variable, variables)]
  )
respondent_means |>
  filter(variable != "Children (recorded)") |>
  mutate(mean = format_number(mean * ifelse(variable %in% c("Graduate", "Unemployed"), 100, 1)),
    variable = if_else(variable %in% c("Graduate", "Unemployed"), paste0(variable, " (%)"), variable)) |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 2, col.names = c("Characteristic", "Mean", "Observed N"),
    caption = "Characteristics of Representatives with Recorded Phone Contact",
    label = "phone_reply_char"
  ) |>
  kable_styling(font_size = 8, latex_options = "hold_position") |>
  footnote(
    general = paste("Quota-seat officeholders recorded as reached on at least one attempt, including transfers and callbacks. N =", nrow(respondents)),
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs/mean_values_respondents.tex"))
write_csv(respondent_means, here("tabs/phone_respondent_characteristics.csv"))
age_screening <- bind_rows(age_screening,
  age_screen_summary(respondents, "Reached quota officeholders", "Rajasthan", 2020L))

weaver <- read_dta(ref_path("weaver_data_2.dta.gz")) |>
  mutate(
    year = case_when(election == -1 ~ 2010L, election == 0 ~ 2015L, election == 1 ~ 2021L),
    winner_age_raw = as.numeric(winner_age),
    winner_age = screen_characteristic_age(winner_age_raw)
  )
age_screening <- bind_rows(age_screening, map_dfr(c(2010L, 2015L, 2021L), function(wave) {
  weaver |>
    filter(year == wave) |>
    transmute(treat = reservation_female, age_raw = winner_age_raw, age = winner_age) |>
    age_screen_summary("Winners", "UP", wave)
}))
write_csv(age_screening, here("tabs/candidate_age_screening.csv"))
up_vars <- c("winner_age", "winner_education", "winner_total_assets_asinh")
up_comparisons <- map_dfr(c(2010, 2015, 2021), function(wave) {
  map2_dfr(up_vars, c("Age", "Education", "Assets (asinh)"), function(outcome, label) {
    observed <- weaver |> filter(year == wave, !is.na(.data[[outcome]]), !is.na(reservation_female))
    tibble(
      year = wave, variable = label,
      open = if (nrow(observed)) mean(observed[[outcome]][observed$reservation_female == 0]) else NA_real_,
      quota = if (nrow(observed)) mean(observed[[outcome]][observed$reservation_female == 1]) else NA_real_,
      difference = open - quota,
      open_n = sum(observed$reservation_female == 0), quota_n = sum(observed$reservation_female == 1)
    )
  })
})
write_csv(up_comparisons, here("tabs/up_candidate_characteristics.csv"))
up_comparisons |>
  mutate(across(c(open, quota, difference), format_number)) |>
  select(year, variable, open, quota, difference) |>
  pivot_wider(names_from = year, values_from = c(open, quota, difference), names_vary = "slowest") |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Winner Characteristics: Quota vs. Open Seats (Uttar Pradesh)", label = "up_cand_char",
    col.names = c("Characteristic", rep(c("Open", "Quota", "Difference"), 3))
  ) |>
  add_header_above(c(" " = 1, "2010" = 3, "2015" = 3, "2021" = 3)) |>
  kable_styling(font_size = 8) |>
  footnote(general = paste(
    "Descriptive comparisons using the Weaver data. Differences are open minus quota.",
    "The last wave is coded 2020 in the source labels",
    "and corresponds to the 2021 election. Assets are available only in that wave.",
    "Age means exclude recorded ages outside 21--110 years; other outcomes retain those records."
  ), threeparttable = TRUE) |>
  save_kable(here("tabs/up_cand_characteristics.tex"))
