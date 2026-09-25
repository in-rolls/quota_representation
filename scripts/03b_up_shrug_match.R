# Join the canonical UP election-to-LGD bridge to study SHRUG covariates.
# Election identities and geographic matching are produced in local_elections_up.

library(tidyverse)
library(arrow)
library(here)
source(here("scripts/00_config.R"))

bridge <- read_parquet(up_path("panels/gp_lgd_bridge.parquet"))
stopifnot(!anyNA(bridge[c("panel", "anchor_key")]), !anyDuplicated(bridge[c("panel", "anchor_key")]))

message("\n=== LOADING SHRUG COVARIATES ===")

shrug_pca <- read_csv(shrug_path("shrug-pca01-csv/pc01_pca_clean_shrid.csv.zip"), show_col_types = FALSE)
shrug_vd <- read_csv(shrug_path("shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip"), show_col_types = FALSE)

message("SHRUG PCA rows: ", nrow(shrug_pca))
message("SHRUG VD rows: ", nrow(shrug_vd))

shrug_lgd_full <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"), show_col_types = FALSE) %>%
  filter(tolower(state_name) == "uttar pradesh")

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}
availability <- function(x) {
  if (any(x > 0, na.rm = TRUE)) {
    return(1L)
  }
  if (anyNA(x)) {
    return(NA_integer_)
  }
  0L
}
facility_fields <- c(
  "pc01_vd_edu_fac", "pc01_vd_medi_fac",
  "pc01_vd_power_supl", "pc01_vd_bank_fac"
)
stopifnot(
  !anyDuplicated(shrug_lgd_full$shrid2),
  !anyDuplicated(shrug_pca$shrid2), !anyDuplicated(shrug_vd$shrid2)
)
shrug_covars <- shrug_lgd_full %>%
  left_join(shrug_pca, by = "shrid2") %>%
  left_join(shrug_vd, by = "shrid2") %>%
  filter(!is.na(LGD_code)) %>%
  group_by(LGD_code) %>%
  summarize(
    shrid2 = min(shrid2),
    n_villages = n(),
    across(starts_with("pc01_pca_"), sum_or_na),
    across(starts_with("pc01_vd_") & !all_of(c(facility_fields, "pc01_vd_dist_town")), sum_or_na),
    across(all_of(facility_fields), ~ sum(!is.na(.x)), .names = "{.col}_n_observed"),
    across(all_of(facility_fields), availability),
    n_distance_observed = sum(!is.na(pc01_vd_dist_town)),
    pc01_vd_dist_town_sum = sum_or_na(pc01_vd_dist_town),
    pc01_vd_dist_town_min = if (all(is.na(pc01_vd_dist_town))) NA_real_ else min(pc01_vd_dist_town, na.rm = TRUE),
    pc01_vd_dist_town = if (all(is.na(pc01_vd_dist_town))) NA_real_ else mean(pc01_vd_dist_town, na.rm = TRUE),
    .groups = "drop"
  )

message("SHRUG covariates aggregated to ", n_distinct(shrug_covars$LGD_code), " LGD GPs")


process_panel <- function(period, panel_id, anchor) {
  panel <- read_parquet(here("data/up", paste0("up_", period, ".parquet")))
  geography <- bridge %>%
    filter(.data$panel == panel_id) %>%
    select(anchor_key, lgd_gp_code, lgd_gp_name, lgd_block_code, lgd_block_name,
      block_match_type, gp_match_type, match_distance, match_confidence, mapping_review_id)
  stopifnot(!anyNA(panel[[anchor]]), !anyDuplicated(panel[[anchor]]),
    setequal(panel[[anchor]], geography$anchor_key))
  panel_final <- panel %>%
    left_join(geography, by = setNames("anchor_key", anchor),
      relationship = "one-to-one", na_matches = "never") %>%
    left_join(shrug_covars, by = c("lgd_gp_code" = "LGD_code"), relationship = "many-to-one")
  stopifnot(nrow(panel_final) == nrow(panel), identical(panel_final[names(panel)], panel))
  write_parquet(panel_final, here("data/up", paste0("shrug_gp_up_", period, "_block.parquet")))
  message(period, ": ", nrow(panel_final), " election rows; ",
    sum(!is.na(panel_final$lgd_gp_code)), " LGD links; ",
    sum(!is.na(panel_final$shrid2)), " SHRUG links")
  invisible(panel_final)
}

process_panel("05_10", "2005_2010", "key_2010")
process_panel("10_15", "2010_2015", "key_2010")
process_panel("15_21", "2015_2021", "key_2015")
process_panel("05_21", "2005_2010_2015_2021", "key_2010")
