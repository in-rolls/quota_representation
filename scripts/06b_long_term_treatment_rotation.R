# 06b_long_term_treatment_rotation.R
# Long-term treatment rotation analysis: Tests rotation across all transitions
# Uses 4-way panels to validate rotation mandate over longer periods
# Includes: 05→10, 10→15, 15→20 (Raj) and 05→10, 10→15, 15→21 (UP)
# Output: tabs/long_term_treatment_rotation.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Long-Term Treatment Rotation Analysis ===")

# =============================================================================
# Load 4-way panels (contains GPs present in all 4 election years)
# =============================================================================

raj_05_20 <- read_raj(raj_product_path("raj_05_20.parquet"))
up_05_21 <- read_parquet(here("data/up/up_05_21.parquet"))

message("\nLoaded 4-way panels:")
message("Raj 05-20 (4-way): ", nrow(raj_05_20), " GPs")
message("UP 05-21 (4-way): ", nrow(up_05_21), " GPs")

# =============================================================================
# Rajasthan 2005 → 2010: treat_2010 ~ treat_2005
# =============================================================================
message("\n--- Rajasthan 2005 → 2010 (4-way panel) ---")

m_raj_0510_nofe <- feols(treat_2010 ~ treat_2005,
  data = raj_05_20,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_0510_fe <- feols(treat_2010 ~ treat_2005 | dist_samiti_2010,
  data = raj_05_20,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

message("No FE coef: ", round(coef(m_raj_0510_nofe)["treat_2005"], 4))
message("FE coef: ", round(coef(m_raj_0510_fe)["treat_2005"], 4))

# =============================================================================
# Rajasthan 2010 → 2015: treat_2015 ~ treat_2010
# =============================================================================
message("\n--- Rajasthan 2010 → 2015 (4-way panel) ---")

m_raj_1015_nofe <- feols(treat_2015 ~ treat_2010,
  data = raj_05_20,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_1015_fe <- feols(treat_2015 ~ treat_2010 | dist_samiti_2015,
  data = raj_05_20,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

message("No FE coef: ", round(coef(m_raj_1015_nofe)["treat_2010"], 4))
message("FE coef: ", round(coef(m_raj_1015_fe)["treat_2010"], 4))

# =============================================================================
# UP 2005 → 2010: treat_2010 ~ treat_2005
# =============================================================================
message("\n--- UP 2005 → 2010 (4-way panel) ---")

m_up_0510_nofe <- feols(treat_2010 ~ treat_2005,
  data = up_05_21,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_0510_fe <- feols(treat_2010 ~ treat_2005 | dist_block_2010,
  data = up_05_21,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

message("No FE coef: ", round(coef(m_up_0510_nofe)["treat_2005"], 4))
message("FE coef: ", round(coef(m_up_0510_fe)["treat_2005"], 4))

# =============================================================================
# UP 2010 → 2015: treat_2015 ~ treat_2010
# =============================================================================
message("\n--- UP 2010 → 2015 (4-way panel) ---")

m_up_1015_nofe <- feols(treat_2015 ~ treat_2010,
  data = up_05_21,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_1015_fe <- feols(treat_2015 ~ treat_2010 | dist_block_2015,
  data = up_05_21,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

message("No FE coef: ", round(coef(m_up_1015_nofe)["treat_2010"], 4))
message("FE coef: ", round(coef(m_up_1015_fe)["treat_2010"], 4))

# =============================================================================
# Rajasthan 2015 → 2020: treat_2020 ~ treat_2015
# =============================================================================
message("\n--- Rajasthan 2015 → 2020 (4-way panel) ---")

m_raj_1520_nofe <- feols(treat_2020 ~ treat_2015,
  data = raj_05_20,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_1520_fe <- feols(treat_2020 ~ treat_2015 | dist_samiti_2020,
  data = raj_05_20,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

message("No FE coef: ", round(coef(m_raj_1520_nofe)["treat_2015"], 4))
message("FE coef: ", round(coef(m_raj_1520_fe)["treat_2015"], 4))

# =============================================================================
# UP 2015 → 2021: treat_2021 ~ treat_2015
# =============================================================================
message("\n--- UP 2015 → 2021 (4-way panel) ---")

m_up_1521_nofe <- feols(treat_2021 ~ treat_2015,
  data = up_05_21,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_1521_fe <- feols(treat_2021 ~ treat_2015 | dist_block_2021,
  data = up_05_21,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

message("No FE coef: ", round(coef(m_up_1521_nofe)["treat_2015"], 4))
message("FE coef: ", round(coef(m_up_1521_fe)["treat_2015"], 4))

# =============================================================================
# Combined Output Table: 12 columns (3 transitions × 2 states × 2 FE specs)
# =============================================================================
message("\n=== Generating output table ===")

all_models <- list(
  m_raj_0510_nofe, m_raj_0510_fe,
  m_raj_1015_nofe, m_raj_1015_fe,
  m_raj_1520_nofe, m_raj_1520_fe,
  m_up_0510_nofe, m_up_0510_fe,
  m_up_1015_nofe, m_up_1015_fe,
  m_up_1521_nofe, m_up_1521_fe
)

dict_rotation <- c(
  "treat_2005" = "$\\text{Quota}_{t-1}$",
  "treat_2010" = "$\\text{Quota}_{t-1}$",
  "treat_2015" = "$\\text{Quota}_{t-1}$",
  "(Intercept)" = "Intercept",
  "dist_samiti_2010" = "(District, Samiti)",
  "dist_samiti_2015" = "(District, Samiti)",
  "dist_samiti_2020" = "(District, Samiti)",
  "dist_block_2010" = "(District, Block)",
  "dist_block_2015" = "(District, Block)",
  "dist_block_2021" = "(District, Block)"
)

notes_text <- paste0(
  "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
  "Outcome: quota status in election $t$. Treatment: quota status in election $t-1$. ",
  "Sample includes GPs present in all four election years (4-way panel). ",
  "A negative coefficient indicates rotation away from previously reserved GPs, consistent with the rotation mandate. ",
  "Standard errors clustered by district-samiti (Rajasthan) or district-block (UP)."
)

aer_etable(all_models,
  file = here("tabs", "long_term_treatment_rotation.tex"),
  headers = list(
    rep(c("Rajasthan", "Uttar Pradesh"), c(6, 6)),
    rep(c("05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$20", "05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$21"), c(2, 2, 2, 2, 2, 2)),
    c(
      "No FE", "FE", "No FE", "FE", "No FE", "FE",
      "No FE", "FE", "No FE", "FE", "No FE", "FE"
    )
  ),
  keep = c("(Intercept)", "%treat_"),
  notes = notes_text,
  dict = dict_rotation
)

message("Created: tabs/long_term_treatment_rotation.tex")

message("\n=== Long-Term Treatment Rotation Analysis Complete ===")
