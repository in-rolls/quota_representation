# 02f_uttarakhand_panels.R
# Uttarakhand pradhan (gram panchayat head) panels from the pooled corpus:
# 2008-2014 and 2014-2019
#
# Outcome. The results portal records no winner sex, so the outcome is the pooled
# `winner_woman_probability`, made upstream in
# local_elections_uttarakhand: a character model on the Devanagari name plus
# naampy on its indicate romanization and fixed name markers, fit on recorded
# candidate sex in Uttar Pradesh and Bihar. Averaged over seats it estimates a
# share of women, so effects are on the expected share of women among winners.
#
# Links. No GP identifier survives between elections. The state repository pairs
# GPs one-to-one by folded name inside the same block (all 89 block names agree
# across the three elections); place qualifiers (upper/lower, Khurd/Kalan,
# numbers) veto a pair. Linked pairs share a candidate name across elections at
# the same rate whether the names matched exactly or fuzzily.
#
# Caste categories. Uttarakhand rotates caste categories as well as women's
# seats, and GPs whose category changed are where most consecutive women's seats
# and consecutive open seats are, so each panel carries both years' caste
# categories and their transition for the models in 13_uttarakhand.R.
#
# Haridwar polls on its own cycle (2010, 2015) and is not linked; it is left out.
#
# Output: data/uk/uk_08_14.parquet, data/uk/uk_14_19.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_pooled.R"))

heads <- pooled_heads("uttarakhand", extras = "winner_woman_probability") |>
  mutate(
    female_winner = as.numeric(winner_woman_probability),
    caste = coalesce(reserved_caste, "GEN"),
    dalit = as.integer(caste %in% "SC")
  )

by_year <- function(y) {
  heads |>
    filter(year == y) |>
    transmute(row_id, district, block, treat = reserved_woman, caste, dalit, female_winner) |>
    rename_with(~ paste0(.x, "_", y), -row_id) |>
    rename(!!paste0("row_id_", y) := row_id)
}

links <- pooled_links("Uttarakhand")

panel <- function(a, b) {
  links |>
    filter(from_year == a, to_year == b) |>
    transmute(!!paste0("row_id_", a) := from_row_id, !!paste0("row_id_", b) := to_row_id, name_score) |>
    inner_join(by_year(a), by = paste0("row_id_", a), relationship = "one-to-one") |>
    inner_join(by_year(b), by = paste0("row_id_", b), relationship = "one-to-one") |>
    filter(!is.na(.data[[paste0("treat_", a)]]), !is.na(.data[[paste0("treat_", b)]])) |>
    mutate(
      gp_id = .data[[paste0("row_id_", b)]],
      !!paste0("dist_block_", b) := str_c(.data[[paste0("district_", b)]], .data[[paste0("block_", b)]], sep = "_"),
      caste_transition = str_c(.data[[paste0("caste_", a)]], .data[[paste0("caste_", b)]], sep = "->"),
      case = str_c(.data[[paste0("treat_", a)]], .data[[paste0("treat_", b)]])
    )
}

uk_08_14 <- panel(2008, 2014)
uk_14_19 <- panel(2014, 2019)
for (p in list(uk_08_14, uk_14_19)) {
  stopifnot(nrow(p) > 4500, !anyNA(p$gp_id), !anyDuplicated(p$gp_id))
}
dir.create(here("data/uk"), showWarnings = FALSE)
write_parquet(uk_08_14, here("data/uk/uk_08_14.parquet"))
write_parquet(uk_14_19, here("data/uk/uk_14_19.parquet"))
message("Uttarakhand panels: 2008-2014 ", nrow(uk_08_14), " GPs; 2014-2019 ", nrow(uk_14_19), " GPs")
