# 02i_telangana_panels.R
# Telangana sarpanch (gram panchayat head) panel from the pooled corpus, 2019-2025.
#
# Outcome. Neither the 2019 listing nor the 2025 reports record the winner's sex,
# so the outcome is the pooled `winner_woman_probability`, made
# upstream in local_elections_telangana: naampy on the name's words plus fixed
# Telugu name endings, combined by a logistic regression fit on Telangana
# assembly candidates' recorded sex (cross-validated AUC 0.982). Winners of
# women's seats, who must be women, average 0.915 (2019) and 0.909 (2025).
# Averaged over seats it estimates a share of women.
#
# Quota rules. Women's seats are half of each caste category's seats per mandal,
# rounded down (2025: 45.9% statewide). 2025 cut BC reservation to 17%, so
# caste transitions differ from 2019; the caste-transition fixed effects absorb
# that.
#
# Links. The sibling links seats by the commission's GP codes and by names;
# `link_pass` says which route(s) made each pair. 16_telangana.R reruns every
# model on pairs both routes made.
#
# Blocks. The 2025 mandal is the comparison unit.
#
# Output: data/tg/tg_19_25.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_pooled.R"))

heads <- pooled_heads("telangana", extras = "winner_woman_probability") |>
  mutate(female_winner = as.numeric(winner_woman_probability), caste = reserved_caste)

by_year <- function(y) {
  heads |>
    filter(year == y) |>
    transmute(row_id, district, block, treat = reserved_woman, caste, female_winner) |>
    rename_with(~ paste0(.x, "_", y), -row_id) |>
    rename(!!paste0("row_id_", y) := row_id)
}

links <- pooled_links("Telangana") |> filter(from_year == 2019, to_year == 2025)

tg <- links |>
  transmute(row_id_2019 = from_row_id, row_id_2025 = to_row_id, link_pass, name_score) |>
  inner_join(by_year(2019), by = "row_id_2019", relationship = "one-to-one") |>
  inner_join(by_year(2025), by = "row_id_2025", relationship = "one-to-one") |>
  filter(!is.na(treat_2019), !is.na(treat_2025), !is.na(caste_2019), !is.na(caste_2025)) |>
  mutate(
    gp_id = row_id_2025,
    dist_block_2025 = str_c(district_2025, block_2025, sep = "_"),
    caste_transition = str_c(caste_2019, caste_2025, sep = "->"),
    case = str_c(treat_2019, treat_2025)
  )

stopifnot(nrow(tg) > 11000, !anyNA(tg$gp_id), !anyDuplicated(tg$gp_id))
dir.create(here("data/tg"), showWarnings = FALSE)
write_parquet(tg, here("data/tg/tg_19_25.parquet"))
message("Telangana panel: ", nrow(tg), " linked seats")
