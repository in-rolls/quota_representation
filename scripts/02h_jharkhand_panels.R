# 02h_jharkhand_panels.R
# Jharkhand mukhiya (gram panchayat head) panel from the pooled corpus, 2015-2022.
#
# Outcome. Form 23 prints the seat's reservation, not the winner's sex, so the
# outcome is the pooled `winner_female_probability_from_name`, made upstream in
# local_elections_jharkhand with the same model as Uttarakhand's (a character
# model on the Devanagari name, naampy on its indicate romanization, fixed name
# markers; fit on recorded candidate sex in Uttar Pradesh and Bihar). Averaged
# over seats it estimates a share of women.
#
# The model is weaker on Scheduled Tribe seats. Winners of ST women's seats, who
# must be women, average 0.876 (2015) and 0.888 (2022), against 0.977 and 0.976
# on other women's seats: Santal, Munda, Oraon and Christian given names are rare
# in the UP and Bihar labels. 14_jharkhand.R therefore also reports non-ST seats
# alone and ST seats rescaled by that recall.
#
# Quota rules. 2022 dropped OBC reservation; former BC seats are unreserved, so
# a BC->NONE caste transition is the rule change, not a rotation.
#
# Blocks. Block names are missing on many rows; the block number is what
# identifies a block, carried in the pooled gp_no as "<block_no>/<gp_no>". Block
# numbers agree across 2015 and 2022 except in Latehar, which the sibling's
# crosswalk handles; the later block is the comparison unit here.
#
# Output: data/jh/jh_15_22.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_pooled.R"))

heads <- pooled_heads("jharkhand", extras = "winner_female_probability_from_name") |>
  mutate(
    female_winner = as.numeric(winner_female_probability_from_name),
    caste = caste_reservation,
    st = as.integer(caste %in% "ST"),
    block_no = str_extract(gp_no, "^[^/]+")
  )

by_year <- function(y) {
  heads |>
    filter(year == y) |>
    transmute(row_id, district, block_no, treat = woman_reserved, caste, st, female_winner) |>
    rename_with(~ paste0(.x, "_", y), -row_id) |>
    rename(!!paste0("row_id_", y) := row_id)
}

links <- pooled_links("Jharkhand") |> filter(from_year == 2015, to_year == 2022)

jh <- links |>
  transmute(row_id_2015 = from_row_id, row_id_2022 = to_row_id, name_score) |>
  inner_join(by_year(2015), by = "row_id_2015", relationship = "one-to-one") |>
  inner_join(by_year(2022), by = "row_id_2022", relationship = "one-to-one") |>
  filter(!is.na(treat_2015), !is.na(treat_2022), !is.na(caste_2015), !is.na(caste_2022)) |>
  mutate(
    gp_id = row_id_2022,
    dist_block_2022 = str_c(district_2022, block_no_2022, sep = "_"),
    caste_transition = str_c(caste_2015, caste_2022, sep = "->"),
    case = str_c(treat_2015, treat_2022)
  )

stopifnot(nrow(jh) > 3000, !anyNA(jh$gp_id), !anyDuplicated(jh$gp_id))
dir.create(here("data/jh"), showWarnings = FALSE)
write_parquet(jh, here("data/jh/jh_15_22.parquet"))
message("Jharkhand panel: ", nrow(jh), " linked seats")
