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
    grad_status = if_else(is.na(education_status), NA_integer_, as.integer(
      str_to_lower(education_status) %in% c(
        "postgraduate", "graduate",
        "professional graduate", "professional post graduate"
      )
    )),
    total_children = as.numeric(children_before27111995) + as.numeric(children_on_or_after28111995),
    age = as.numeric(age),
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
labels <- c("Age", "Children", "Graduate", "Unemployed", "Log assets")

samples <- list(
  Winners = winners, Candidates = candidates,
  `Women winners` = filter(winners, candidate_female == 1),
  `Women candidates` = filter(candidates, candidate_female == 1)
)
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
    filter(str_starts(sample, "Women") == women_only) |>
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
    kable_styling(font_size = 8) |>
    footnote(
      general = paste(
        "Descriptive open-minus-quota comparisons, conditional on candidacy or winning.",
        "Assets winsorized at the 10th and 90th percentiles of the candidate sample before taking logs.",
        "These comparisons do not identify a causal effect on candidate quality."
      ),
      threeparttable = TRUE
    ) |>
    save_kable(here("tabs", if (women_only) "cand_characteristics_women.tex" else "cand_characteristics_combined.tex"))
}

respondent_links <- read_parquet(here("data/raj/phone_candidate_links.parquet"))
respondents <- winners |>
  inner_join(respondent_links,
    by = c(
      "event_key", "contesting_candidate_serial_no", "name_of_contesting_candidate",
      "father_husband_of_contesting_candidate"
    ),
    relationship = "one-to-one", na_matches = "never"
  )
respondent_means <- respondents |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "mean") |>
  mutate(variable = labels[match(variable, variables)])
respondent_means |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 2, col.names = c("Characteristic", "Mean"),
    caption = "Characteristics of Representatives Recorded as Answering Phone Calls",
    label = "phone_reply_char"
  ) |>
  kable_styling(font_size = 8) |>
  footnote(
    general = paste("Quota-seat respondents. N =", nrow(respondents)),
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs/mean_values_respondents.tex"))
write_csv(respondent_means |> mutate(n = nrow(respondents)), here("tabs/phone_respondent_characteristics.csv"))

weaver <- read_dta(ref_path("weaver_data_2.dta.gz")) |>
  mutate(year = case_when(election == -1 ~ 2010L, election == 0 ~ 2015L, election == 1 ~ 2021L))
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
  select(year, variable, open, quota, difference) |>
  pivot_wider(names_from = year, values_from = c(open, quota, difference), names_vary = "slowest") |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Candidate Characteristics: Quota vs. Open Seats (Uttar Pradesh)", label = "up_cand_char",
    col.names = c("Characteristic", rep(c("Open", "Quota", "Difference"), 3))
  ) |>
  add_header_above(c(" " = 1, "2010" = 3, "2015" = 3, "2021" = 3)) |>
  kable_styling(font_size = 8) |>
  footnote(general = paste(
    "Descriptive comparisons using the Weaver data. The last wave is coded 2020 in the source labels",
    "and corresponds to the 2021 election. Assets are available only in that wave."
  ), threeparttable = TRUE) |>
  save_kable(here("tabs/up_cand_characteristics.tex"))
