# Attach study-specific Census covariates to the canonical Rajasthan election panels.
library(arrow)
library(dplyr)
library(readr)
library(here)
source(here("scripts/00_config.R"))

raj_mapping <- read_raj(raj_product_path("gp_lgd_crosswalk.parquet"))
stopifnot(!anyDuplicated(raj_mapping$match_key))

shrug_pca <- read_csv(shrug_path("shrug-pca01-csv/pc01_pca_clean_shrid.csv.zip"), show_col_types = FALSE)
shrug_vd <- read_csv(shrug_path("shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip"), show_col_types = FALSE)

message("SHRUG PCA rows: ", nrow(shrug_pca))
message("SHRUG VD rows: ", nrow(shrug_vd))

shrug_lgd_full <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"), show_col_types = FALSE) %>%
  filter(state_name == "rajasthan")

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

for (period in c("05_10", "10_15", "15_20", "05_20")) {
  panel <- read_raj(raj_product_path(paste0("raj_", period, ".parquet")))
  result <- panel |>
    left_join(raj_mapping, by = "match_key", relationship = "many-to-one") |>
    left_join(shrug_covars, by = c("lgd_gp_code" = "LGD_code"), relationship = "many-to-one")
  stopifnot(nrow(result) == nrow(panel))
  write_parquet(result, here("data/raj", paste0("shrug_gp_raj_", period, "_block.parquet")))
  message(period, ": ", nrow(result), " election rows; ",
    sum(!is.na(result$lgd_gp_code)), " with LGD; ", sum(!is.na(result$shrid2)), " with SHRUG"
  )
}
