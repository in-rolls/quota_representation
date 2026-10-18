# 02e_city_panels.R
# Ward panels for two municipal councils, 2007-2012: Delhi (MCD, 272 wards) and
# Mumbai (BMC, 227 wards)
#
# Why these years. Both cities kept one ward map for 2007 and 2012 and redrew it
# for 2017, so 2007-2012 is the only pair in which a ward number names the same
# place in both elections. Delhi's ward names agree for all 272 wards.
#
# Delhi. Goyal's replication deposit ("Local political representation as a
# pathway to power", APSR) holds one row per ward and election: the category,
# the assembly constituency and the top three candidates with their gender, as
# Goyal coded it from the commission's result sheets. Its 2007 categories match
# the commission's 2007 order on every ward (the five wards another file leaves
# blank were checked against the order). That order put women's seats on every
# third ward of the list of 226 non-SC seats; the 2012 seats were drawn afresh,
# and in this panel 52% of 2007 women's wards are reserved again against 50% of
# the rest.
#
# Mumbai. The 2007 women's seats and both councils' winner sex come from the
# Karekurve-Ramachandra & Lee deposit (Harvard Dataverse IO9SLQ): its 2011 rows
# are the 2007 council and its 2013 rows the 2012 council, sex as the authors
# coded it. 2007 caste categories are not in it, so the 2007 treatment is a
# women's seat of any caste. The 2012 categories come from local_elections'
# ulb_ward_2012, which agrees with the deposit's 2012 flag on 226 of 227 wards.
# Mumbai rotates: 75 of the 76 wards reserved in 2007 are open in 2012, so the
# 37 wards open in both years were chosen by the rotation, not by a draw. The
# 2002 categories, which probably decided that choice, are not held.
#
# Kolkata (KMC, 2010-2015, wards 1-141). Lok Dhaba's candidate files, as
# republished by OpenCity, give every candidate's votes and each ward's category
# but no sex. Ward electorates correlate at 0.96 across the two elections by ward
# number (0.55 with the neighbouring number), so the map held; wards 142-144 were
# added in 2015. Winner sex is local_elections' kolkata_winner_sex: female on a
# women's seat by law, otherwise coded from the first name, with names used for
# either sex left uncertain (NA here, bounded in 12_cities). 2010 women's seats
# are reserved again in 2015 at 40%, against 30% for the rest, so Kolkata does
# not rotate strictly.
#
# Output: data/cities/del_07_12.parquet, data/cities/mum_07_12.parquet,
#   data/cities/kol_10_15.parquet

library(arrow)
library(dplyr)
library(here)
library(readr)
library(stringr)
library(tidyr)

source(here("scripts/00_config.R"))

goyal <- read_tsv(
  pooled_path("data/delhi/qualification_audit/sources/goyal_mapping.tab"),
  col_types = cols(.default = col_character())
)

delhi_year <- function(y) {
  goyal |>
    filter(election == as.character(y)) |>
    transmute(
      ward = wardno, ward_name = wardname, ac = acno, corporation = mcdcorp,
      category = resstatus,
      treat = as.integer(resstatus %in% c("W", "SCW")),
      dalit = as.integer(resstatus %in% c("SC", "SCW")),
      female_winner = case_when(gender1 == "F" ~ 1, gender1 == "M" ~ 0),
      female_candidates = as.integer(female_cand),
      candidates = as.integer(ncand),
      sc_share_2001 = as.numeric(wardscper2001)
    ) |>
    rename_with(~ paste0(.x, "_", y), -ward)
}

del_07_12 <- inner_join(delhi_year(2007), delhi_year(2012), by = "ward", relationship = "one-to-one") |>
  mutate(ward_id = paste0("del_", ward), case = paste0(treat_2007, treat_2012))
stopifnot(
  nrow(del_07_12) == 272,
  all(toupper(trimws(del_07_12$ward_name_2007)) == toupper(trimws(del_07_12$ward_name_2012))),
  !anyNA(del_07_12$female_winner_2012), !anyNA(del_07_12$treat_2007)
)

deposit <- read_tsv(
  pooled_path("data/maharashtra/mumbai/dataverse_IO9SLQ/mumbai_full.tab"),
  col_types = cols(.default = col_character())
)
council <- function(row_year, y) {
  deposit |>
    filter(year == row_year) |>
    transmute(
      ward = as.integer(ward),
      deposit_category = na_if(reservation_status, ""),
      female_winner = case_when(sex == "Female" ~ 1, sex == "Male" ~ 0)
    ) |>
    rename_with(~ paste0(.x, "_", y), -ward)
}
roster_2012 <- read_csv(
  pooled_path("data/maharashtra/ulb_ward_2012.csv"),
  col_types = cols(.default = col_character())
) |>
  transmute(
    ward = as.integer(ward_no), category_2012 = reservation,
    treat_2012 = as.integer(woman_reserved), dalit_2012 = as.integer(caste_reservation %in% "SC")
  )

mum_07_12 <- council("2011", 2007) |>
  mutate(treat_2007 = as.integer(deposit_category_2007 %in% "W")) |>
  inner_join(council("2013", 2012), by = "ward", relationship = "one-to-one") |>
  inner_join(roster_2012, by = "ward", relationship = "one-to-one") |>
  mutate(ward_id = paste0("mum_", ward), case = paste0(treat_2007, treat_2012))
stopifnot(nrow(mum_07_12) == 227, sum(mum_07_12$treat_2007) == 76, !anyNA(mum_07_12$treat_2012))

kmc <- function(y) {
  candidates <- read_csv(
    pooled_path(paste0("data/opencity/kolkata_kmc_", y, "_candidates.csv")),
    col_types = cols(.default = col_character())
  )
  wards <- candidates |>
    group_by(ward = as.integer(Ward_No)) |>
    summarise(
      category = first(Ward_Reservation), candidates = sum(Candidate_Name != "NOTA"),
      electors = as.numeric(first(Total_Electors)), .groups = "drop"
    )
  sex <- read_csv(
    pooled_path("data/opencity/kolkata_winner_sex.csv"),
    col_types = cols(.default = col_character())
  ) |>
    filter(year == as.character(y)) |>
    transmute(ward = as.integer(ward_no), winner, winner_sex = winner_sex_from_name, sex_basis = basis)
  inner_join(wards, sex, by = "ward", relationship = "one-to-one") |>
    mutate(
      treat = as.integer(str_detect(category, "Women")),
      dalit = as.integer(str_detect(category, "Scheduled Caste")),
      female_winner = case_when(winner_sex == "female" ~ 1, winner_sex == "male" ~ 0)
    ) |>
    rename_with(~ paste0(.x, "_", y), -ward)
}
kol_10_15 <- inner_join(kmc(2010), kmc(2015), by = "ward", relationship = "one-to-one") |>
  mutate(ward_id = paste0("kol_", ward), case = paste0(treat_2010, treat_2015))
stopifnot(
  nrow(kol_10_15) == 141, sum(kol_10_15$treat_2010) == 47,
  all(kol_10_15$female_winner_2015[kol_10_15$treat_2015 == 1] == 1)
)

dir.create(here("data/cities"), showWarnings = FALSE)
write_parquet(del_07_12, here("data/cities/del_07_12.parquet"))
write_parquet(mum_07_12, here("data/cities/mum_07_12.parquet"))
write_parquet(kol_10_15, here("data/cities/kol_10_15.parquet"))
message(
  "City panels: Delhi ", nrow(del_07_12), " wards (", sum(del_07_12$treat_2012 == 0), " open in 2012); Mumbai ",
  nrow(mum_07_12), " wards (", sum(mum_07_12$treat_2012 == 0), " open in 2012); Kolkata ",
  nrow(kol_10_15), " wards (", sum(kol_10_15$treat_2015 == 0), " open in 2015)"
)
