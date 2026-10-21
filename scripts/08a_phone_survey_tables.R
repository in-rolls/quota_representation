library(tidyverse)
library(readxl)
library(janitor)
library(kableExtra)
library(here)
library(arrow)

survey_dir <- here("data/raj/source/phone_survey_response")
rural_quota <- read_excel(file.path(survey_dir, "sampled_nos_full_analysis.xlsx"),
  sheet = "Sheet1", col_types = "text"
) |> clean_names()
rural_open <- read_excel(file.path(survey_dir, "sampled_mobile_nos_open_seats.xlsx"),
  sheet = "Sheet1", col_types = "text"
) |>
  clean_names() |>
  rename(respondent_gender = relative_gender)
urban <- read_excel(file.path(survey_dir, "jaipur_audit.xlsx"), sheet = "Sheet1", col_types = "text") |>
  clean_names() |>
  filter(!is.na(attempt_to_reach)) |>
  mutate(
    treat_status = parse_double(treat_status),
    phone_responded = parse_double(phone_responded),
    phone_answered = if_else(phone_responded == 1, "yes", "no")
  ) |>
  rename(phone_answered_by = respondent)
stopifnot(all(urban$treat_status %in% 0:1))

surveys <- list(
  phone_survey = rural_quota,
  phone_survey_openseats = rural_open,
  jaipur_urban_phone_survey_quota = filter(urban, treat_status == 1),
  jaipur_urban_phone_survey_open = filter(urban, treat_status == 0)
)
contact_categories <- c(
  "Recorded as elected representative", "Male non-member",
  "Female non-member", "Non-member, sex unavailable", "Identity unavailable"
)
contact_counts <- list()
for (survey_name in names(surveys)) {
  survey <- surveys[[survey_name]] |>
    mutate(phone_answered_by = str_replace_all(phone_answered_by, "-", "_"))
  stopifnot(nrow(survey) > 0)
  answered <- survey |>
    filter(phone_answered == "yes") |>
    mutate(category = case_when(
      phone_answered_by == "member" ~ contact_categories[[1]],
      phone_answered_by == "non_member" & respondent_gender == "male" ~ contact_categories[[2]],
      phone_answered_by == "non_member" & respondent_gender == "female" ~ contact_categories[[3]],
      phone_answered_by == "non_member" ~ contact_categories[[4]],
      TRUE ~ contact_categories[[5]]
    ))
  initial <- tibble(
    category = c("Answered", "No answer recorded"),
    n = c(nrow(answered), nrow(survey) - nrow(answered)), denominator = nrow(survey),
    section = "Initial contact"
  )
  contact <- answered |>
    count(category) |>
    complete(category = contact_categories, fill = list(n = 0L)) |>
    arrange(match(category, contact_categories)) |>
    mutate(denominator = nrow(answered), section = "Among answered")
  stopifnot(sum(contact$n) == nrow(answered))
  rows <- bind_rows(initial, contact)
  if (survey_name == "phone_survey") {
    male <- answered |>
      filter(phone_answered_by == "non_member", respondent_gender == "male") |>
      mutate(category = case_when(
        relationship == "spouse" ~ "Spouse",
        relationship == "child" ~ "Child",
        relationship %in% c("father", "family_member") ~ "Other recorded relative",
        relationship == "self" ~ "Relationship recorded as self",
        TRUE ~ "Relationship unavailable"
      )) |>
      count(category) |>
      mutate(denominator = sum(n), section = "Among male non-members")
    transfers <- answered |>
      filter(phone_answered_by == "non_member") |>
      mutate(category = case_when(
        did_transfer == "yes" ~ "Transferred",
        did_transfer == "no" ~ "Did not transfer",
        did_transfer == "not_applicable" ~ "Transfer recorded as not applicable",
        TRUE ~ "Transfer status unavailable"
      )) |>
      count(category) |>
      mutate(denominator = sum(n), section = "Among non-members")
    rows <- bind_rows(rows, male, transfers)
  }
  contact_counts[[survey_name]] <- rows |> mutate(survey = survey_name)
  section_sizes <- rows |> count(section, name = "rows")
  section_sizes <- section_sizes |> slice(match(unique(.env$rows$section), section))
  rows |>
    transmute(Category = category, `N (%)` = sprintf("%d (%.1f)", n, 100 * n / denominator)) |>
    kbl(format = "latex", booktabs = TRUE, align = c("l", "r")) |>
    pack_rows(
      index = setNames(section_sizes$rows, section_sizes$section),
      italic = TRUE, bold = FALSE, indent = FALSE
    ) |>
    save_kable(here("tabs", paste0(survey_name, ".tex")))
}
write_csv(bind_rows(contact_counts), here("tabs/phone_contact_counts.csv"))

# The call sheet lacks GP names. Only unique phase/district/samiti/winner links identify a seat.
lookup_columns <- c("election_type", "election_duration", "district", "panchayat_samiti", "winner_candidate_name")
winner_lookup <- read_raj(raj_product_path("winners_2020_events.parquet")) |>
  mutate(across(all_of(lookup_columns), ~ str_to_lower(str_squish(.x)))) |>
  add_count(across(all_of(lookup_columns)), name = "survey_key_records") |>
  filter(survey_key_records == 1, winner_key_unique, gp_event_unique) |>
  select(all_of(lookup_columns), female_winner_2020)
open_linked <- rural_open |>
  mutate(
    survey_row = row_number(),
    across(all_of(lookup_columns), ~ str_to_lower(str_squish(.x)))
  ) |>
  left_join(winner_lookup, by = lookup_columns, relationship = "many-to-one", na_matches = "never")
stopifnot(nrow(open_linked) == nrow(rural_open))
open_linkage_counts <- open_linked |>
  summarise(
    sampled = n(), answered = sum(phone_answered == "yes", na.rm = TRUE),
    recorded_member = sum(phone_answered == "yes" & phone_answered_by == "member", na.rm = TRUE),
    recorded_member_female = sum(phone_answered == "yes" & phone_answered_by == "member" &
      respondent_gender == "female", na.rm = TRUE),
    recorded_member_male = sum(phone_answered == "yes" & phone_answered_by == "member" &
      respondent_gender == "male", na.rm = TRUE), .by = female_winner_2020
  )
write_csv(open_linkage_counts, here("tabs/open_seat_phone_linkage.csv"))
write_parquet(open_linked |> select(
  survey_row, female_winner_2020, phone_answered,
  phone_answered_by, respondent_gender
), here("data/raj/open_seat_phone_linkage.parquet"))
