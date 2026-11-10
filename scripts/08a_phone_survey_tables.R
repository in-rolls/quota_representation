library(tidyverse)
library(readxl)
library(janitor)
library(kableExtra)
library(here)
library(arrow)

source(here("scripts/00_config.R"))
survey_dir <- here("data/raj/source/phone_survey_response")
review_dir <- here("data/phone_review")
manifest <- read_csv(file.path(review_dir, "source_manifest.csv"), show_col_types = FALSE)
stopifnot(all(map_chr(manifest$path, ~ digest::digest(here(.x), algo = "sha256", file = TRUE)) == manifest$sha256))
corrections <- read_csv(file.path(review_dir, "contact_corrections.csv"), show_col_types = FALSE)
uncertainty <- read_csv(file.path(review_dir, "identity_uncertainty.csv"), show_col_types = FALSE)
stopifnot(!anyDuplicated(corrections[c("file", "excel_row", "field")]))

read_survey <- function(filename) {
  read_excel(file.path(survey_dir, filename), sheet = "Sheet1", col_types = "text") |>
    clean_names() |>
    mutate(excel_row = row_number() + 1L, source_file = filename,
      across(where(is.character), str_trim))
}
rural_quota <- read_survey("sampled_nos_full_analysis.xlsx")
rural_open <- read_survey("sampled_mobile_nos_open_seats.xlsx") |>
  rename(respondent_gender = relative_gender)
urban <- read_survey("jaipur_audit.xlsx") |>
  filter(!is.na(attempt_to_reach)) |>
  mutate(
    corporation = recode(x1, "jaipur" = "GREATER", "Jaipur Heritage" = "HERITAGE"),
    ward = parse_double(ward_no),
    recorded_treat_status = parse_double(treat_status),
    phone_answered = if_else(parse_double(phone_responded) == 1, "yes", "no")
  ) |>
  rename(phone_answered_by = respondent)
roster <- read_csv(file.path(review_dir, "jaipur_2020_roster.csv"), show_col_types = FALSE)
stopifnot(!anyDuplicated(roster[c("corporation", "ward")]), nrow(roster) == 250)
urban <- urban |>
  left_join(roster, by = c("corporation", "ward"), relationship = "one-to-one") |>
  mutate(treat_status = new_treat, female_winner_2020 = as.integer(new_officeholder_sex == "Female"))
stopifnot(nrow(urban) == 248, !anyNA(urban$treat_status),
  sum(urban$treat_status != urban$recorded_treat_status) == 11)

# Only unique election-phase/district/samiti/winner records identify a rural seat.
lookup_columns <- c("election_type", "election_duration", "district", "panchayat_samiti", "winner_candidate_name")
winner_lookup <- read_raj(raj_product_path("winners_2020_events.parquet")) |>
  mutate(across(all_of(lookup_columns), ~ str_to_lower(str_squish(.x)))) |>
  add_count(across(all_of(lookup_columns)), name = "survey_key_records") |>
  filter(survey_key_records == 1, winner_key_unique, gp_event_unique) |>
  select(all_of(lookup_columns), event_key, female_winner_2020)
link_winners <- function(survey) {
  linked <- survey |>
    mutate(across(all_of(lookup_columns), ~ str_to_lower(str_squish(.x)))) |>
    left_join(winner_lookup, by = lookup_columns, relationship = "many-to-one", na_matches = "never")
  stopifnot(nrow(linked) == nrow(survey))
  linked
}
quota_lookup <- read_raj(raj_product_path("winners_2020_events.parquet")) |>
  mutate(survey_key = str_to_lower(str_squish(paste0(
    district, panchayat_samiti, name_of_gram_panchyat, winner_candidate_name
  )))) |>
  add_count(election_type, election_duration, survey_key, name = "survey_key_records") |>
  filter(survey_key_records == 1, winner_key_unique, gp_event_unique) |>
  select(election_type, election_duration, survey_key, event_key, female_winner_2020)
rural_quota <- rural_quota |>
  mutate(survey_key = str_to_lower(str_squish(key))) |>
  left_join(quota_lookup, by = c("election_type", "election_duration", "survey_key"),
    relationship = "many-to-one", na_matches = "never")
stopifnot(nrow(rural_quota) == 500)
rural_open <- link_winners(rural_open)

review_contacts <- function(survey) {
  survey <- survey |>
    mutate(
      phone_answered_by = str_replace_all(phone_answered_by, "-", "_"),
      any_answered = phone_answered,
      any_officeholder_contact = case_when(
        phone_answered_by == "member" ~ "yes",
        phone_answered_by == "non_member" ~ "no", TRUE ~ "unknown"
      )
    )
  changes <- corrections |> filter(file == unique(survey$source_file))
  for (i in seq_len(nrow(changes))) {
    row <- match(changes$excel_row[i], survey$excel_row)
    field <- changes$field[i]
    stopifnot(!is.na(row), survey[[field]][row] == changes$old[i])
    survey[[field]][row] <- changes$new[i]
  }
  flagged <- uncertainty |> filter(file == unique(survey$source_file))
  survey |>
    mutate(
      identity_disputed = excel_row %in% flagged$excel_row |
        coalesce(phone_answered_by == "member" & respondent_gender %in% c("male", "female") &
          (respondent_gender == "female") != female_winner_2020, FALSE),
      contact_sensitivity = if_else(identity_disputed, "unknown", any_officeholder_contact)
    )
}
rural_quota <- review_contacts(rural_quota)
rural_open <- review_contacts(rural_open)
urban <- review_contacts(urban)

surveys <- list(
  phone_survey = rural_quota,
  phone_survey_openseats = rural_open,
  jaipur_urban_phone_survey_quota = filter(urban, treat_status == 1),
  jaipur_urban_phone_survey_open = filter(urban, treat_status == 0)
)
contact_categories <- c(
  "Officeholder contact recorded", "Male non-member only",
  "Female non-member only", "Non-member, sex unavailable", "Identity unavailable"
)
contact_counts <- list()
for (survey_name in names(surveys)) {
  survey <- surveys[[survey_name]] |>
    mutate(phone_answered_by = str_replace_all(phone_answered_by, "-", "_"))
  stopifnot(nrow(survey) > 0)
  answered <- survey |>
    filter(any_answered == "yes") |>
    mutate(category = case_when(
      any_officeholder_contact == "yes" ~ contact_categories[[1]],
      phone_answered_by == "non_member" & respondent_gender == "male" ~ contact_categories[[2]],
      phone_answered_by == "non_member" & respondent_gender == "female" ~ contact_categories[[3]],
      phone_answered_by == "non_member" ~ contact_categories[[4]],
      TRUE ~ contact_categories[[5]]
    ))
  initial <- tibble(
    category = c("Answered", "No answer recorded"),
    n = c(nrow(answered), nrow(survey) - nrow(answered)), denominator = nrow(survey),
    section = "Contact attempts"
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
      mutate(denominator = sum(n), section = "Recorded male non-members")
    transfers <- answered |>
      filter(phone_answered_by == "non_member") |>
      mutate(category = case_when(
        did_transfer == "yes" ~ "Transferred",
        did_transfer == "no" ~ "Did not transfer",
        did_transfer == "not_applicable" ~ "Transfer recorded as not applicable",
        TRUE ~ "Transfer status unavailable"
      )) |>
      count(category) |>
      mutate(denominator = sum(n), section = "Recorded non-members")
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

phone_summary <- tibble(
  Outcome = c("Sampled representatives", "Answered: N (\\% of sampled)",
    "Officeholder reached: N (\\% of answered)")
)
for (survey_name in c("phone_survey", "phone_survey_openseats")) {
  counts <- contact_counts[[survey_name]]
  answered <- counts |> filter(category == "Answered")
  officeholder <- counts |> filter(category == "Officeholder contact recorded")
  stopifnot(nrow(answered) == 1L, nrow(officeholder) == 1L)
  column <- if (survey_name == "phone_survey") "Quota seats" else "Non-quota seats"
  phone_summary[[column]] <- c(
    as.character(answered$denominator),
    sprintf("%d (%.1f)", answered$n, 100 * answered$n / answered$denominator),
    sprintf("%d (%.1f)", officeholder$n, 100 * officeholder$n / officeholder$denominator)
  )
}
phone_summary |>
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", "r", "r")) |>
  save_kable(here("tabs/phone_audit_main.tex"))

reviewed <- bind_rows(imap(surveys, ~ mutate(.x, survey = .y))) |>
  select(any_of(c("source_file", "excel_row", "survey", "key", "election_type", "election_duration",
    "district", "winner_candidate_name", "event_key", "female_winner_2020", "corporation", "ward",
    "recorded_treat_status", "treat_status", "phone_answered", "phone_answered_by", "respondent_gender",
    "relationship", "did_transfer", "any_answered", "any_officeholder_contact", "identity_disputed", "contact_sensitivity")))
write_parquet(reviewed, here("data/raj/phone_contacts_reviewed.parquet"))
sensitivity <- reviewed |>
  filter(any_answered == "yes") |>
  summarise(
    answered = n(), recorded = sum(any_officeholder_contact == "yes"),
    definite = sum(contact_sensitivity == "yes"),
    unknown = sum(contact_sensitivity == "unknown"), .by = survey
  ) |>
  mutate(share = recorded / answered, lower = definite / answered, upper = (definite + unknown) / answered)
write_csv(sensitivity, here("tabs/phone_contact_sensitivity.csv"))
sensitivity |>
  mutate(Sample = c("Rural quota", "Rural open", "Jaipur quota", "Jaipur open")) |>
  transmute(Sample, Answered = answered, `Recorded contact (\\%)` = sprintf("%.1f", 100 * share),
    `Classification range (\\%)` = sprintf("[%.1f, %.1f]", 100 * lower, 100 * upper)) |>
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", "r", "r", "r")) |>
  save_kable(here("tabs/phone_contact_sensitivity.tex"))

open_linked <- rural_open |> mutate(survey_row = excel_row - 1L)
open_linkage_counts <- open_linked |>
  summarise(
    sampled = n(), answered = sum(any_answered == "yes", na.rm = TRUE),
    recorded_member = sum(any_answered == "yes" & any_officeholder_contact == "yes", na.rm = TRUE),
    recorded_member_female = sum(any_answered == "yes" & phone_answered_by == "member" &
      respondent_gender == "female", na.rm = TRUE),
    recorded_member_male = sum(any_answered == "yes" & phone_answered_by == "member" &
      respondent_gender == "male", na.rm = TRUE), .by = female_winner_2020
  )
write_csv(open_linkage_counts, here("tabs/open_seat_phone_linkage.csv"))
write_parquet(open_linked |> select(
  survey_row, female_winner_2020, phone_answered = any_answered,
  phone_answered_by, respondent_gender, any_officeholder_contact, contact_sensitivity
), here("data/raj/open_seat_phone_linkage.parquet"))
