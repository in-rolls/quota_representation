# 02c_bihar_panels.R
# Bihar Mukhiya (GP head) panels for 2011-2016 and 2016-2021, from the pooled corpus
# Seats, reservations, recorded winner sex and seat links all come from the pinned
# local_elections release; this joins them and adds study variables.
# Output: data/bh/bh_11_16.parquet, data/bh/bh_16_21.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_pooled.R"))

# A woman won when the recorded winner is female. Seats without a recorded winner
# stay missing; an "other" winner is not a woman.
female <- function(gender) if_else(is.na(gender), NA_integer_, as.integer(gender == "female"))

heads <- pooled_heads("bihar", extras = c("winner_gender", "code_repeated")) |>
  mutate(
    obc = as.integer(caste_reservation %in% "BC"),
    dalit = as.integer(caste_reservation %in% c("SC", "ST")),
    female_winner = female(winner_gender)
  )

# The form reuses one code for two places in a few parents, so those seats' pages
# could belong to either; they are left out.
gp_2016 <- heads |>
  filter(year == 2016, is.na(code_repeated) | code_repeated != "1") |>
  transmute(
    gp_id = row_id,
    district_2016 = str_remove(district, "^\\d+ - "),
    block_2016 = str_remove(block, "^\\d+ - "),
    reservation_2016 = reservation_raw,
    treat_2016 = woman_reserved, obc_2016 = obc, dalit_2016 = dalit,
    female_winner_2016 = female_winner,
    dist_block_2016 = str_c(district_2016, block_2016, sep = "_")
  )

# The 2011 reports print the seat label as written; the pooled woman_reserved is
# set only where the label says so. The inclusive sensitivity also reads a bare
# category ("अनारक्षित") as not reserved for women, but only in districts whose
# reports print women's labels at all.
winners_2011 <- heads |>
  filter(year == 2011) |>
  group_by(district) |>
  mutate(prints_women = any(woman_reserved %in% 1L)) |>
  ungroup() |>
  transmute(
    row_id_2011 = row_id,
    treat_2011 = woman_reserved,
    treat_2011_inclusive = case_when(
      !is.na(woman_reserved) ~ woman_reserved,
      !is.na(caste_reservation) & prints_women ~ 0L,
      .default = NA_integer_
    ),
    female_winner_2011 = female_winner, obc_2011 = obc, dalit_2011 = dalit,
    reservation_2011 = reservation_raw
  )

links <- pooled_links("Bihar")

bh_11_16 <- links |>
  filter(from_year == 2011, to_year == 2016) |>
  transmute(row_id_2011 = from_row_id, gp_id = to_row_id, name_score_11_16 = name_score) |>
  inner_join(winners_2011, by = "row_id_2011", relationship = "one-to-one") |>
  inner_join(gp_2016, by = "gp_id", relationship = "one-to-one") |>
  filter(!is.na(treat_2016), !is.na(treat_2011_inclusive)) |>
  mutate(
    district_2011 = district_2016, dist_block_2011 = dist_block_2016,
    case = str_c(treat_2011, treat_2016)
  )

gp_2021 <- heads |>
  filter(year == 2021) |>
  transmute(
    row_id_2021 = row_id, reservation_2021 = reservation_raw,
    treat_2021 = woman_reserved, obc_2021 = obc, dalit_2021 = dalit,
    female_winner_2021 = female_winner
  )

bh_16_21 <- links |>
  filter(from_year == 2016, to_year == 2021) |>
  transmute(gp_id = from_row_id, row_id_2021 = to_row_id, name_score_16_21 = name_score, link_pass_16_21 = link_pass) |>
  inner_join(gp_2016, by = "gp_id", relationship = "one-to-one") |>
  inner_join(gp_2021, by = "row_id_2021", relationship = "one-to-one") |>
  filter(!is.na(treat_2016), !is.na(treat_2021)) |>
  mutate(
    district_2021 = district_2016, dist_block_2021 = dist_block_2016,
    case = str_c(treat_2016, treat_2021)
  )

for (panel in list(bh_11_16, bh_16_21)) stopifnot(!anyNA(panel$gp_id), !anyDuplicated(panel$gp_id))
dir.create(here("data/bh"), showWarnings = FALSE)
write_parquet(bh_11_16, here("data/bh/bh_11_16.parquet"))
write_parquet(bh_16_21, here("data/bh/bh_16_21.parquet"))
message(
  "Bihar panels: 2011-2016 ", nrow(bh_11_16), " GPs (", sum(!is.na(bh_11_16$treat_2011)),
  " with an explicit 2011 women label); 2016-2021 ", nrow(bh_16_21), " GPs"
)
