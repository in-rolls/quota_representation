# 02d_haryana_panels.R
# Haryana sarpanch panels from the pooled corpus: 2010-2016 (estimated) and
# 2016-2022 (described only)
#
# Where inference happens. Haryana's notifications record no winner sex, so the
# outcome is the pooled `winner_woman_probability`: a probability that
# the winner is a woman, made upstream in local_elections_haryana from the printed
# name (naampy's first-name score plus fixed name markers, recalibrated on 2022
# winners, whose sex the 2020 amendment fixes). Averaged over seats it estimates a
# share of women, so the models below estimate effects on the expected share of
# women among winners, not on a recorded count. Winners printed as placeholders
# carry no probability and drop out.
#
# Why 2016-2022 is not estimated. The 2020 amendment lets only women contest the
# women's offices and only persons other than women contest the rest, so in 2022 a
# woman cannot win an open seat whatever the 2016 quota was.
#
# 2010 blocks whose printed categories conflict with the commission's own
# reservation table (pooled quality flag `reservation_source_conflict`) are
# dropped wherever 2010 is used.
#
# Output: data/hr/hr_10_16.parquet, data/hr/hr_16_22.parquet

library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_pooled.R"))

heads <- pooled_heads("haryana", extras = "winner_woman_probability") |>
  mutate(
    female_winner = as.numeric(winner_woman_probability),
    dalit = as.integer(reserved_caste %in% "SC"),
    conflict = str_detect(coalesce(quality_flags, ""), "reservation_source_conflict")
  )

by_year <- function(y) {
  heads |>
    filter(year == y) |>
    transmute(
      row_id,
      district = district, block = block,
      treat = reserved_woman, dalit, female_winner, conflict
    ) |>
    rename_with(~ paste0(.x, "_", y), -row_id) |>
    rename(!!paste0("row_id_", y) := row_id)
}

links <- pooled_links("Haryana")

panel <- function(a, b) {
  links |>
    filter(from_year == a, to_year == b) |>
    transmute(!!paste0("row_id_", a) := from_row_id, !!paste0("row_id_", b) := to_row_id, name_score, link_pass) |>
    inner_join(by_year(a), by = paste0("row_id_", a), relationship = "one-to-one") |>
    inner_join(by_year(b), by = paste0("row_id_", b), relationship = "one-to-one") |>
    filter(!is.na(.data[[paste0("treat_", a)]]), !is.na(.data[[paste0("treat_", b)]])) |>
    mutate(
      gp_id = .data[[paste0("row_id_", b)]],
      !!paste0("dist_block_", b) := str_c(.data[[paste0("district_", b)]], .data[[paste0("block_", b)]], sep = "_"),
      case = str_c(.data[[paste0("treat_", a)]], .data[[paste0("treat_", b)]])
    )
}

hr_10_16_all <- panel(2010, 2016)
dropped <- sum(hr_10_16_all$conflict_2010)
hr_10_16 <- filter(hr_10_16_all, !conflict_2010)
hr_16_22 <- panel(2016, 2022)

for (p in list(hr_10_16, hr_16_22)) stopifnot(!anyNA(p$gp_id), !anyDuplicated(p$gp_id))
dir.create(here("data/hr"), showWarnings = FALSE)
write_parquet(hr_10_16, here("data/hr/hr_10_16.parquet"))
write_parquet(hr_16_22, here("data/hr/hr_16_22.parquet"))
message(
  "Haryana panels: 2010-2016 ", nrow(hr_10_16), " GPs (", dropped,
  " dropped from 2010 blocks with conflicting categories); 2016-2022 ", nrow(hr_16_22), " GPs"
)
