library(testthat)
library(dplyr)
library(readr)
library(haven)
library(arrow)
library(here)
source(here("scripts/00_config.R"))

test_that("age screening excludes impossible entries and retains boundary ages", {
  raw <- c(NA_real_, NaN, -Inf, Inf, -1, 0, 1, 18, 20, 20.9, 21, 42, 110, 110.1, 140)
  expected <- c(rep(NA_real_, 10), 21, 42, 110, NA_real_, NA_real_)
  expect_equal(screen_characteristic_age(raw), expected)
  expect_equal(length(screen_characteristic_age(raw)), length(raw))
})

candidates <- read_raj(raj_product_path("candidates_2020_events.parquet")) |>
  filter(candidate_key_unique, gp_event_unique)
winner_ids <- read_raj(raj_product_path("winners_2020_events.parquet")) |>
  filter(winner_key_unique, gp_event_unique) |>
  select(event_key, name_of_contesting_candidate = winner_candidate_name)
winners <- candidates |>
  filter(candidate_name_unique) |>
  semi_join(winner_ids, by = c("event_key", "name_of_contesting_candidate"), na_matches = "never")
samples <- list(
  Winners = winners, Candidates = candidates,
  `Women winners` = filter(winners, candidate_female == 1),
  `Women candidates` = filter(candidates, candidate_female == 1)
)
reported <- read_csv(here("tabs/candidate_characteristics.csv"), show_col_types = FALSE)
screening <- read_csv(here("tabs/candidate_age_screening.csv"), show_col_types = FALSE)

check_age_summary <- function(raw, reported_mean, reported_n, screen_row) {
  valid <- which(!is.na(raw) & raw >= 21 & raw <= 110)
  expect_equal(reported_mean, mean(raw[valid]), tolerance = 1e-12)
  expect_equal(reported_n, length(valid))
  expect_equal(nrow(screen_row), 1L)
  expect_equal(screen_row$source_n, length(raw))
  expect_equal(screen_row$missing_age, sum(is.na(raw)))
  expect_equal(screen_row$excluded_age, sum(!is.na(raw)) - length(valid))
  expect_equal(screen_row$age_n, length(valid))
  expect_equal(screen_row$raw_mean, mean(raw, na.rm = TRUE), tolerance = 1e-12)
  expect_equal(screen_row$screened_mean, mean(raw[valid]), tolerance = 1e-12)
  expect_equal(screen_row$minimum_age, 21)
  expect_equal(screen_row$maximum_age, 110)
}

test_that("Rajasthan age means and denominators reconstruct from upstream records", {
  for (sample_name in names(samples)) {
    d <- samples[[sample_name]]
    for (quota in 0:1) {
      raw <- d$age[which(d$primary_female_reserved == quota)]
      row <- reported |> filter(sample == sample_name, variable == "Age")
      group <- if (quota == 1) "quota" else "open"
      screen_row <- screening |>
        filter(state == "Rajasthan", sample == sample_name, .data$quota == .env$quota)
      check_age_summary(raw, row[[group]], row[[paste0(group, "_n")]], screen_row)
      other <- reported |> filter(sample == sample_name, variable %in% c("Unemployed", "Log assets"))
      expect_equal(other[[paste0(group, "_n")]], rep(length(raw), nrow(other)))
    }
  }
  excluded <- screening |>
    filter(state == "Rajasthan", sample == "Candidates") |>
    arrange(quota)
  expect_equal(excluded$excluded_age, c(45, 38))
})

test_that("UP age screening is wave-specific and does not change other outcomes", {
  source <- read_dta(ref_path("weaver_data_2.dta.gz"))
  up_reported <- read_csv(here("tabs/up_candidate_characteristics.csv"), show_col_types = FALSE)
  for (wave in c(2010L, 2015L, 2021L)) {
    election <- c(`2010` = -1, `2015` = 0, `2021` = 1)[as.character(wave)]
    for (quota in 0:1) {
      d <- source[which(source$election == election & source$reservation_female == quota), ]
      row <- up_reported |> filter(year == wave, variable == "Age")
      group <- if (quota == 1) "quota" else "open"
      screen_row <- screening |> filter(state == "UP", year == wave, .data$quota == .env$quota)
      check_age_summary(d$winner_age, row[[group]], row[[paste0(group, "_n")]], screen_row)
      education <- up_reported |> filter(year == wave, variable == "Education")
      expect_equal(education[[paste0(group, "_n")]], sum(!is.na(d$winner_education)))
      expect_equal(education[[group]], mean(d$winner_education, na.rm = TRUE), tolerance = 1e-12)
    }
  }
  expect_equal(sum(screening$excluded_age[screening$state == "UP"]), 140)
})

test_that("phone respondent ages preserve raw values and use the same screen", {
  respondents <- read_parquet(here("data/raj/phone_respondents_reviewed.parquet"))
  raw <- candidates |>
    select(event_key, contesting_candidate_serial_no, source_age = age)
  checked <- respondents |>
    left_join(raw, by = c("event_key", "contesting_candidate_serial_no"), relationship = "one-to-one")
  expect_equal(nrow(checked), 38L)
  expect_equal(checked$age_raw, checked$source_age)
  expect_equal(checked$age, screen_characteristic_age(checked$source_age))
  row <- read_csv(here("tabs/phone_respondent_characteristics.csv"), show_col_types = FALSE) |>
    filter(variable == "Age")
  screen_row <- screening |> filter(sample == "Reached quota officeholders")
  check_age_summary(checked$source_age, row$mean, row$n, screen_row)
})

message("Validated characteristic ages, source exclusions, denominators, and retained raw values.")
