# 01g_audit_crosswalk_provenance.R
# Audit crosswalk provenance, key integrity, and LGD validity.
# Output: data/crosswalks/audit/01g_crosswalk_provenance_audit.csv

library(readr)
library(dplyr)
library(tidyr)
library(here)
source(here("scripts/00_config.R"))

message("=== Crosswalk Provenance Audit ===")

dir.create(here("tabs"), showWarnings = FALSE)
dir.create(here("data/crosswalks/audit"), showWarnings = FALSE, recursive = TRUE)

required_inputs <- c(
  raj_path("data/reference/geography/raj_district_xwalk.csv"),
  raj_path("data/reference/geography/raj_samiti_std.csv"),
  raj_path("data/reference/geography/raj_samiti_xwalk.csv"),
  sibling_path("data/manual/crosswalks/active/up_block_xwalk.csv"),
  sibling_path("data/reference/lgd/lgd_up_block_gp.csv"),
  raj_path("data/reference/geography/lgd_raj_block_gp.csv")
)

missing_required <- required_inputs[!file.exists(required_inputs)]
if (length(missing_required) > 0) {
  stop(
    "Missing required crosswalk inputs:\n",
    paste(missing_required, collapse = "\n")
  )
}

crosswalk_catalog <- tibble::tribble(
  ~file, ~producer_script, ~consumer_scripts, ~key_cols,
  "data/reference/lgd/lgd_up_block_gp.csv", "local_elections_up", "01g_audit_crosswalk_provenance.R;03d_audit_shrug_coverage.R", "gp_code",
  "data/manual/crosswalks/active/up_block_xwalk.csv", "local_elections_up", "01g_audit_crosswalk_provenance.R", "elex_district,elex_block",
  "data/reference/geography/raj_district_xwalk.csv", "local_elections_rajasthan", "01g_audit_crosswalk_provenance.R", "elex_district_raw",
  "data/reference/geography/raj_samiti_std.csv", "local_elections_rajasthan", "01g_audit_crosswalk_provenance.R", "district_std,samiti_raw",
  "data/reference/geography/raj_samiti_xwalk.csv", "local_elections_rajasthan", "01g_audit_crosswalk_provenance.R", "elex_district,elex_samiti",
  "data/reference/geography/lgd_raj_block_gp.csv", "local_elections_rajasthan", "01g_audit_crosswalk_provenance.R", "gp_code"
)

crosswalk_path <- function(path) {
  if (startsWith(path, "data/reference/geography/")) raj_path(path) else sibling_path(path)
}

read_any_csv <- function(path) {
  read_csv(crosswalk_path(path), show_col_types = FALSE)
}

safe_n_unique_keys <- function(df, key_cols) {
  keys <- trimws(unlist(strsplit(key_cols, ",")))
  if (length(keys) == 0 || any(!keys %in% names(df))) {
    return(NA_integer_)
  }
  nrow(distinct(df, across(all_of(keys))))
}

safe_key_unique <- function(df, key_cols) {
  keys <- trimws(unlist(strsplit(key_cols, ",")))
  if (length(keys) == 0 || any(!keys %in% names(df))) {
    return(NA)
  }
  nrow(df) == nrow(distinct(df, across(all_of(keys))))
}

file_stats <- crosswalk_catalog %>%
  rowwise() %>%
  mutate(
    abs_path = crosswalk_path(file),
    exists = file.exists(abs_path),
    modified_time = if (exists) as.character(file.info(abs_path)$mtime) else NA_character_,
    n_rows = if (exists) nrow(read_any_csv(file)) else NA_integer_,
    n_unique_keys = if (exists) safe_n_unique_keys(read_any_csv(file), key_cols) else NA_integer_,
    key_is_unique = if (exists) safe_key_unique(read_any_csv(file), key_cols) else NA
  ) %>%
  ungroup()

# LGD validity checks for active matching crosswalks
lgd_up_blocks <- read_any_csv("data/reference/lgd/lgd_up_block_gp.csv") %>% distinct(block_code)
lgd_raj_block_gp <- read_any_csv("data/reference/geography/lgd_raj_block_gp.csv")

up_block_xwalk <- read_any_csv("data/manual/crosswalks/active/up_block_xwalk.csv")
raj_samiti_xwalk <- read_any_csv("data/reference/geography/raj_samiti_xwalk.csv")

up_block_invalid <- up_block_xwalk %>%
  anti_join(lgd_up_blocks %>% distinct(block_code), by = c("lgd_block_code" = "block_code")) %>%
  nrow()

raj_block_invalid <- raj_samiti_xwalk %>%
  anti_join(lgd_raj_block_gp %>% distinct(block_code), by = c("lgd_block_code" = "block_code")) %>%
  nrow()

validity <- tibble::tribble(
  ~file, ~validity_check, ~invalid_rows,
  "data/manual/crosswalks/active/up_block_xwalk.csv", "lgd_block_code in lgd_up_blocks$block_code", up_block_invalid,
  "data/reference/geography/raj_samiti_xwalk.csv", "lgd_block_code in lgd_raj_block_gp$block_code", raj_block_invalid
)

audit <- file_stats %>%
  left_join(
    validity %>% group_by(file) %>% summarize(invalid_rows = sum(invalid_rows), .groups = "drop"),
    by = "file"
  ) %>%
  mutate(invalid_rows = coalesce(invalid_rows, 0L))

write_csv(audit, here("data/crosswalks/audit/01g_crosswalk_provenance_audit.csv"))
message("Saved: data/crosswalks/audit/01g_crosswalk_provenance_audit.csv")
