# 02g_tamil_nadu_panels.R
# Tamil Nadu village panchayat president panel from the pooled corpus: 2011 linked
# to the next ordinary election, held in 2019 (27 districts) or 2021 (the nine
# districts carved out after 2011).
#
# Outcome. The SEC's result sheets print each candidate's sex, so the outcome is
# the winner's recorded sex (pooled extra `winner_gender`), not a name-based
# inference.
#
# Quota size. Women's reservation was one third of seats in 2011 and one half from
# 2019 (2016 amendment; the 2016 election was cancelled). The later round is
# therefore not the same rule repeated, and seats open in the later round are a
# smaller share than in states with a constant one-third rule.
#
# Links. No identifier survives between elections. local_elections_tamil_nadu
# pairs seats one-to-one by folded Tamil name inside hand-crosswalked unions
# (six districts were split after 2011, so unions, not districts, are the
# blocking unit). Each link is its sibling's claim, not an official identifier.
#
# Output: data/tn/tn_11_later.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_pooled.R"))

heads <- pooled_heads("tamil_nadu", extras = "winner_gender") |>
  mutate(
    female_winner = case_when(
      winner_gender == "female" ~ 1, winner_gender == "male" ~ 0, .default = NA_real_
    ),
    caste = caste_reservation,
    dalit = as.integer(caste %in% "SC")
  )

by_year <- function(years, suffix) {
  heads |>
    filter(year %in% years) |>
    transmute(row_id, year, district, block, treat = woman_reserved, caste, dalit, female_winner) |>
    rename_with(~ paste0(.x, "_", suffix), -row_id) |>
    rename(!!paste0("row_id_", suffix) := row_id)
}

links <- pooled_links("Tamil Nadu") |> filter(from_year == 2011, to_year %in% c(2019, 2021))

tn <- links |>
  transmute(row_id_2011 = from_row_id, row_id_later = to_row_id, later_year = to_year, name_score) |>
  inner_join(by_year(2011, "2011"), by = "row_id_2011", relationship = "one-to-one") |>
  inner_join(by_year(c(2019, 2021), "later"), by = "row_id_later", relationship = "one-to-one") |>
  filter(!is.na(treat_2011), !is.na(treat_later), !is.na(caste_2011), !is.na(caste_later)) |>
  mutate(
    gp_id = row_id_later,
    # the later union is the comparison unit; district names changed with the split
    dist_block_later = str_c(district_later, block_later, sep = "_"),
    caste_transition = str_c(caste_2011, caste_later, sep = "->"),
    case = str_c(treat_2011, treat_later)
  )

stopifnot(nrow(tn) > 5000, !anyNA(tn$gp_id), !anyDuplicated(tn$gp_id))
dir.create(here("data/tn"), showWarnings = FALSE)
write_parquet(tn, here("data/tn/tn_11_later.parquet"))
message("Tamil Nadu panel: ", nrow(tn), " linked seats (", sum(tn$later_year == 2019), " to 2019, ",
        sum(tn$later_year == 2021), " to 2021)")
