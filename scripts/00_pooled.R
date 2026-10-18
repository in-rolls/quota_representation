# 00_pooled.R
# Readers for the pooled local_elections release (pinned in data/manifest.yaml).
#
# The pooled master holds one row per seat. State-specific and derived columns -
# recorded winner sex, the name-based probability, Bihar's repeated-code flag -
# live long-form in master_extras (row_id, column, value), and the states' seat
# links between elections live in supplemental_seat_links. A link is the state
# repository's claim that two records name one seat (names matched inside a
# reviewed block pair), not an official identifier.

library(arrow)
library(dplyr)
library(tidyr)

# GP-head rows for one state, with the requested extras as columns. woman_reserved
# is integer 0/1 with NA where the source does not state it.
pooled_heads <- function(state_slug, extras = character()) {
  heads <- open_dataset(pooled_path(paste0("data/master/master_", state_slug, ".parquet"))) |>
    filter(tier == "gp_head") |>
    select(
      row_id, year, district, block, gram_panchayat, caste_reservation,
      woman_reserved, reservation_raw, winner, quality_flags
    ) |>
    collect() |>
    mutate(
      year = as.integer(year),
      woman_reserved = as.integer(woman_reserved),
      caste_reservation = na_if(as.character(caste_reservation), "")
    )
  if (length(extras)) {
    wide <- open_dataset(pooled_path("data/master/master_extras.parquet")) |>
      filter(column %in% extras) |>
      collect() |>
      filter(row_id %in% heads$row_id) |>
      pivot_wider(names_from = column, values_from = value)
    for (missing in setdiff(extras, names(wide))) wide[[missing]] <- NA_character_
    heads <- left_join(heads, wide, by = "row_id", relationship = "one-to-one")
  }
  heads
}

pooled_links <- function(state) {
  read_parquet(pooled_path("data/master/supplemental_seat_links.parquet")) |>
    filter(state == !!state, tier == "gp_head") |>
    mutate(across(c(from_year, to_year), as.integer), name_score = as.numeric(name_score))
}
