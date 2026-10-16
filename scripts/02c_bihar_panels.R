# 02c_bihar_panels.R
# Bihar Mukhiya (GP head) panels for 2011-2016 and 2016-2021
# Seat links are made upstream in local_elections_bihar; this joins reservations and
# winners to them and adds study variables.
# Output: data/bh/bh_11_16.parquet, data/bh/bh_16_21.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))

read_bihar <- function(file) as.data.frame(read_parquet(bihar_path(file)))

# A woman won when the recorded winner is female. Seats without a recorded winner
# stay missing; the single "other" winner is not a woman.
female <- function(gender) if_else(is.na(gender), NA_integer_, as.integer(gender == "female"))

seats_2016 <- read_bihar("data/2016/seats.parquet") |>
  filter(office == "mukhiya", !code_repeated) |>
  transmute(
    district_code, block_code, unit_code,
    district_2016 = str_remove(district, "^\\d+ - "),
    block_2016 = str_remove(block, "^\\d+ - "),
    reservation_2016 = seat_reservation,
    treat_2016 = as.integer(str_detect(seat_reservation, "महिला")),
    obc_2016 = as.integer(str_detect(seat_reservation, "पिछ")),
    dalit_2016 = as.integer(str_detect(seat_reservation, "अनुसूचित"))
  )
winners_2016 <- read_bihar("data/2016/winners.parquet") |>
  filter(office == "mukhiya") |>
  transmute(district_code, block_code, unit_code, female_winner_2016 = female(gender))
stopifnot(!anyDuplicated(winners_2016[c("district_code", "block_code", "unit_code")]))
gp_2016 <- seats_2016 |>
  left_join(winners_2016, by = c("district_code", "block_code", "unit_code"), relationship = "one-to-one") |>
  mutate(
    gp_id = str_c(district_code, block_code, unit_code, sep = "_"),
    dist_block_2016 = str_c(district_2016, block_2016, sep = "_")
  )

# The 2011 reports print the seat label as written. The upstream recode marks women's
# reservation only where the label says so ("महिला" or "अन्य"). The inclusive
# sensitivity also reads a bare category ("अनारक्षित") as not reserved for women,
# but only in districts whose reports print women's labels at all.
winners_2011 <- read_bihar("data/2011/mukhiya_reports/winner_records.parquet") |>
  group_by(district_raw) |>
  mutate(prints_women = any(reservation_women %in% TRUE)) |>
  ungroup() |>
  transmute(
    record_id_2011 = record_id,
    treat_2011 = as.integer(reservation_women),
    treat_2011_inclusive = case_when(
      !is.na(reservation_women) ~ as.integer(reservation_women),
      !is.na(reservation_category) & prints_women ~ 0L,
      .default = NA_integer_
    ),
    female_winner_2011 = female(gender),
    obc_2011 = as.integer(reservation_category %in% c("BC", "EBC")),
    dalit_2011 = as.integer(reservation_category %in% c("SC", "ST")),
    reservation_2011 = reservation_raw
  )

links_11_16 <- read_bihar("data/linkage/mukhiya_2011_2016.parquet")
bh_11_16 <- links_11_16 |>
  transmute(record_id_2011, gp_id = str_c(district_code, block_code, unit_code, sep = "_"), name_score_11_16 = name_score) |>
  inner_join(winners_2011, by = "record_id_2011", relationship = "one-to-one") |>
  inner_join(gp_2016, by = "gp_id", relationship = "one-to-one") |>
  filter(!is.na(treat_2016), !is.na(treat_2011_inclusive)) |>
  mutate(
    district_2011 = district_2016, dist_block_2011 = dist_block_2016,
    case = str_c(treat_2011, treat_2016)
  )

reservations_2021 <- read_bihar("data/2021/current_reservations.parquet") |>
  filter(post_id == 3) |>
  transmute(
    unit_id_2021 = unit_id,
    reservation_2021 = seat_reservation,
    treat_2021 = as.integer(str_detect(seat_reservation, "\\(महिला\\)")),
    obc_2021 = as.integer(str_detect(seat_reservation, "पिछ")),
    dalit_2021 = as.integer(str_detect(seat_reservation, "अनुसूचित"))
  )
winners_2021 <- read_bihar("data/2021/winners.parquet") |>
  filter(post_id == 3) |>
  transmute(unit_id_2021 = unit_id, female_winner_2021 = female(candidate_gender))
stopifnot(!anyDuplicated(winners_2021$unit_id_2021), !anyDuplicated(reservations_2021$unit_id_2021))

links_16_21 <- read_bihar("data/linkage/mukhiya_2016_2021.parquet")
bh_16_21 <- links_16_21 |>
  transmute(
    gp_id = str_c(district_code, block_code, unit_code, sep = "_"), unit_id_2021,
    name_score_16_21 = name_score, link_pass_16_21 = link_pass
  ) |>
  inner_join(gp_2016, by = "gp_id", relationship = "one-to-one") |>
  inner_join(reservations_2021, by = "unit_id_2021", relationship = "one-to-one") |>
  left_join(winners_2021, by = "unit_id_2021", relationship = "one-to-one") |>
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
