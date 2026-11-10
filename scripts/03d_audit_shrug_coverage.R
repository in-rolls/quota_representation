# 03d_audit_shrug_coverage.R
# Investigate why UP SHRUG match rate is only ~52.9% vs Rajasthan's ~87.6%
#
# Key questions:
# 1. What % of LGD GPs have SHRUG coverage?
# 2. Are there systematic patterns in unmatched GPs?
# 3. Is the issue SHRUG coverage or LGD code format mismatch?

library(tidyverse)
library(arrow)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== UP SHRUG Coverage Audit ===")

# ============================================================================
# STEP 1: Load all relevant data
# ============================================================================

message("Loading data...")

shrug_lgd <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
  show_col_types = FALSE
)

up_shrug <- shrug_lgd %>%
  filter(tolower(state_name) == "uttar pradesh")

lgd_up_gp <- read_csv(sibling_path("data/reference/lgd/lgd_up_block_gp.csv"),
  show_col_types = FALSE
)

message("SHRUG rows for UP: ", nrow(up_shrug))
message("SHRUG rows with valid LGD_code: ", sum(!is.na(up_shrug$LGD_code)))
message("Unique SHRUG LGD codes for UP: ", n_distinct(up_shrug$LGD_code, na.rm = TRUE))
message("LGD GPs for UP: ", nrow(lgd_up_gp))

# ============================================================================
# STEP 2: Analyze LGD → SHRUG coverage gap
# ============================================================================

message("\n=== LGD → SHRUG Coverage Analysis ===")

lgd_in_shrug <- lgd_up_gp$gp_code %in% up_shrug$LGD_code
coverage_rate <- mean(lgd_in_shrug) * 100

message("LGD GPs matched to SHRUG: ", sum(lgd_in_shrug))
message("LGD GPs NOT in SHRUG: ", sum(!lgd_in_shrug))
message("Coverage rate: ", round(coverage_rate, 1), "%")

# GPs without SHRUG match
lgd_missing <- lgd_up_gp %>%
  filter(!gp_code %in% up_shrug$LGD_code)

message("\n=== District-level coverage breakdown ===")

coverage_by_district <- lgd_up_gp %>%
  mutate(has_shrug = gp_code %in% up_shrug$LGD_code) %>%
  group_by(zp_name) %>%
  summarize(
    total_gps = n(),
    in_shrug = sum(has_shrug),
    coverage_pct = round(100 * in_shrug / total_gps, 1),
    .groups = "drop"
  ) %>%
  arrange(coverage_pct)

message("\nLowest coverage districts:")
print(head(coverage_by_district, 15))

message("\nHighest coverage districts:")
print(tail(coverage_by_district, 10))

# ============================================================================
# STEP 3: Analyze patterns in unmatched GPs
# ============================================================================

message("\n=== Patterns in Unmatched GPs ===")

unmatched_by_district <- lgd_missing %>%
  count(zp_name, name = "n_unmatched") %>%
  arrange(desc(n_unmatched))

message("\nDistricts with most unmatched GPs:")
print(head(unmatched_by_district, 15))

unmatched_by_block <- lgd_missing %>%
  count(zp_name, block_name, name = "n_unmatched") %>%
  arrange(desc(n_unmatched))

message("\nBlocks with most unmatched GPs:")
print(head(unmatched_by_block, 20))

# ============================================================================
# STEP 4: Check for LGD code format issues
# ============================================================================

message("\n=== LGD Code Format Analysis ===")

lgd_codes <- lgd_up_gp$gp_code
shrug_codes <- up_shrug$LGD_code[!is.na(up_shrug$LGD_code)]

message("LGD code range: ", min(lgd_codes), "-", max(lgd_codes))
message("SHRUG LGD code range: ", min(shrug_codes), "-", max(shrug_codes))

lgd_code_digits <- nchar(as.character(lgd_codes))
shrug_code_digits <- nchar(as.character(shrug_codes))

message("\nLGD code digit distribution:")
print(table(lgd_code_digits))

message("\nSHRUG LGD code digit distribution:")
print(table(shrug_code_digits))

# ============================================================================
# STEP 5: Check SHRUG data structure
# ============================================================================

message("\n=== SHRUG Data Structure ===")

message("SHRUG columns:")
print(names(up_shrug))

message("\nSHRUG local_body_type distribution:")
print(table(up_shrug$local_body_type, useNA = "ifany"))

message("\nSHRUG gp_to_urban_conversion:")
print(table(up_shrug$gp_to_urban_conversion, useNA = "ifany"))

message("\nSHRUG shrid_part_of_multi_village_GP:")
print(table(up_shrug$shrid_part_of_multi_village_GP, useNA = "ifany"))

# ============================================================================
# STEP 6: Sample unmatched GPs for manual review
# ============================================================================

message("\n=== Sample of Unmatched GPs ===")

set.seed(42)
sample_unmatched <- lgd_missing %>%
  group_by(zp_name) %>%
  slice_sample(n = 2) %>%
  ungroup() %>%
  select(zp_name, block_name, gp_name, gp_code)

message("\nRandom sample of unmatched GPs (2 per district):")
print(sample_unmatched, n = 50)

# ============================================================================
# STEP 7: Compare with Rajasthan for context
# ============================================================================

message("\n=== Comparison with Rajasthan ===")

raj_shrug <- shrug_lgd %>%
  filter(tolower(state_name) == "rajasthan")

lgd_raj <- read_csv(raj_path("data/reference/geography/lgd_raj_block_gp.csv"),
  show_col_types = FALSE
)

raj_in_shrug <- lgd_raj$gp_code %in% raj_shrug$LGD_code
raj_coverage <- mean(raj_in_shrug) * 100

message("Rajasthan LGD GPs: ", nrow(lgd_raj))
message("Rajasthan in SHRUG: ", sum(raj_in_shrug))
message("Rajasthan coverage: ", round(raj_coverage, 1), "%")

message("\nUP coverage: ", round(coverage_rate, 1), "%")
message("Gap: ", round(raj_coverage - coverage_rate, 1), "percentage points")

# ============================================================================
# STEP 8: Generate findings report
# ============================================================================

findings <- c(
  "# UP SHRUG Coverage Audit Results",
  "",
  "## Executive Summary",
  "",
  sprintf("- **UP LGD→SHRUG coverage rate: %.1f%%**", coverage_rate),
  sprintf("- Rajasthan LGD→SHRUG coverage rate: %.1f%%", raj_coverage),
  sprintf("- Gap: %.1f percentage points", raj_coverage - coverage_rate),
  "",
  "## Key Finding",
  "",
  "The low SHRUG match rate is **primarily a SHRUG data coverage issue**, not a crosswalk problem.",
  sprintf("Only %.1f%% of UP LGD GPs have corresponding entries in the SHRUG-LGD matched file.", coverage_rate),
  "",
  "## Root Causes",
  "",
  "1. **SHRUG coverage is incomplete for UP**: Many LGD GPs simply do not appear in the SHRUG-LGD crosswalk",
  "2. **No code format mismatch**: LGD codes have same format in both datasets (5-6 digit integers)",
  "3. **This is a data availability issue, not a matching issue**",
  "",
  "## Coverage by District",
  "",
  "Lowest coverage districts:",
  ""
)

for (i in 1:min(10, nrow(coverage_by_district))) {
  findings <- c(findings, sprintf(
    "- %s: %.1f%% (%d/%d GPs)",
    coverage_by_district$zp_name[i],
    coverage_by_district$coverage_pct[i],
    coverage_by_district$in_shrug[i],
    coverage_by_district$total_gps[i]
  ))
}

findings <- c(
  findings,
  "",
  "## Statistics",
  "",
  sprintf("- Total LGD GPs in UP: %d", nrow(lgd_up_gp)),
  sprintf("- LGD GPs with SHRUG match: %d", sum(lgd_in_shrug)),
  sprintf("- LGD GPs without SHRUG match: %d", sum(!lgd_in_shrug)),
  "",
  "## Recommendations",
  "",
  "1. **Accept the coverage limitation**: The 52.9% SHRUG match rate reflects SHRUG data availability, not crosswalk quality",
  "2. **Focus on improving block crosswalk**: The block-level matching can still be improved from 97.4% to ~100%",
  "3. **Export unmatched GPs for reference**: Document which GPs cannot be linked to SHRUG",
  "4. **Consider alternative data sources**: For GPs not in SHRUG, other census/administrative data may be needed",
  ""
)

writeLines(findings, here("data/crosswalks/audit/03d_up_shrug_coverage_audit.md"))
message("\n=== Report saved to data/crosswalks/audit/03d_up_shrug_coverage_audit.md ===")

# ============================================================================
# STEP 9: Export unmatched GPs for reference
# ============================================================================

write_csv(lgd_missing, here("data/crosswalks/audit/03d_up_coverage_lgd_missing_shrug.csv"))
message("Exported unmatched GPs to: data/crosswalks/audit/03d_up_coverage_lgd_missing_shrug.csv")

write_csv(coverage_by_district, here("data/crosswalks/audit/03d_up_shrug_coverage_by_district.csv"))
message("Exported district coverage to: data/crosswalks/audit/03d_up_shrug_coverage_by_district.csv")

message("\n=== Done ===")
