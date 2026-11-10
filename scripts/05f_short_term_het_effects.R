# 05f_short_term_het_effects.R
# Consolidated heterogeneous effects table for ALL panels
# All three interactions (near_town, high_infra, high_f_literacy) in single models
# Uses new SHRUG-matched data from block matching (02c_shrug_all_panels_block.R)
# Output: tabs/het_effects_combined.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Consolidated Heterogeneous Effects Analysis (All Panels) ===")

# =============================================================================
# Helper function: add heterogeneity variables
# =============================================================================

add_het_vars <- function(df) {
  df %>%
    mutate(
      near_town = ifelse(pc01_vd_dist_town <= median(pc01_vd_dist_town, na.rm = TRUE), 1, 0),
      infra_index = rowSums(across(
        c(
          pc01_vd_edu_fac, pc01_vd_medi_fac,
          pc01_vd_power_supl, pc01_vd_bank_fac
        ),
        ~ as.numeric(.)
      ), na.rm = FALSE),
      high_infra = ifelse(infra_index > median(infra_index, na.rm = TRUE), 1, 0),
      female_lit_rate = ifelse(pc01_pca_tot_f > pc01_pca_f_06 & pc01_pca_f_lit >= 0 & pc01_pca_f_lit <= pc01_pca_tot_f - pc01_pca_f_06, pc01_pca_f_lit / (pc01_pca_tot_f - pc01_pca_f_06), NA_real_),
      high_f_literacy = ifelse(female_lit_rate > median(female_lit_rate, na.rm = TRUE), 1, 0)
    )
}

# =============================================================================
# RAJASTHAN: Load and prepare new SHRUG-matched data (block matching)
# =============================================================================

message("\n--- Rajasthan ---")

raj_shrug_05_10 <- read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")) %>%
  add_het_vars()

raj_shrug_10_15 <- read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")) %>%
  add_het_vars()

raj_shrug_15_20 <- read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")) %>%
  add_het_vars()

message("Raj 05-10 rows: ", nrow(raj_shrug_05_10))
message("Raj 10-15 rows: ", nrow(raj_shrug_10_15))
message("Raj 15-20 rows: ", nrow(raj_shrug_15_20))

# =============================================================================
# UTTAR PRADESH: Load and prepare SHRUG-matched data for all panels
# =============================================================================

message("\n--- Uttar Pradesh ---")

up_shrug_05_10 <- read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")) %>%
  add_het_vars()

up_shrug_10_15 <- read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")) %>%
  add_het_vars()

up_shrug_15_21 <- read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")) %>%
  add_het_vars()

message("UP 05-10 GPs: ", nrow(up_shrug_05_10))
message("UP 10-15 rows: ", nrow(up_shrug_10_15))
message("UP 15-21 rows: ", nrow(up_shrug_15_21))

# =============================================================================
# Filter to open seats and complete cases
# =============================================================================

raj_05_10_open <- raj_shrug_05_10 %>%
  filter(treat_2010 == 0) %>%
  filter(!is.na(near_town) & !is.na(high_infra) & !is.na(high_f_literacy))

raj_10_15_open <- raj_shrug_10_15 %>%
  filter(treat_2015 == 0) %>%
  filter(!is.na(near_town) & !is.na(high_infra) & !is.na(high_f_literacy))

raj_15_20_open <- raj_shrug_15_20 %>%
  filter(treat_2020 == 0 & !is.na(female_winner_2020)) %>%
  filter(!is.na(near_town) & !is.na(high_infra) & !is.na(high_f_literacy))

up_05_10_open <- up_shrug_05_10 %>%
  filter(treat_2010 == 0) %>%
  filter(!is.na(near_town) & !is.na(high_infra) & !is.na(high_f_literacy))

up_10_15_open <- up_shrug_10_15 %>%
  filter(treat_2015 == 0) %>%
  filter(!is.na(near_town) & !is.na(high_infra) & !is.na(high_f_literacy))

up_15_21_open <- up_shrug_15_21 %>%
  filter(treat_2021 == 0) %>%
  filter(!is.na(near_town) & !is.na(high_infra) & !is.na(high_f_literacy))

message("\nRaj 05-10 open seats with all het vars: ", nrow(raj_05_10_open))
message("Raj 10-15 open seats with all het vars: ", nrow(raj_10_15_open))
message("Raj 15-20 open seats with all het vars: ", nrow(raj_15_20_open))
message("UP 05-10 open seats with all het vars: ", nrow(up_05_10_open))
message("UP 10-15 open seats with all het vars: ", nrow(up_10_15_open))
message("UP 15-21 open seats with all het vars: ", nrow(up_15_21_open))

# =============================================================================
# Run combined interaction models for ALL panels
# =============================================================================

message("\n=== Running consolidated models ===")

models <- list()

# Rajasthan 05-10: No FE
models[[1]] <- feols(female_winner_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * high_f_literacy,
  data = raj_05_10_open,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# Rajasthan 05-10: With FE
models[[2]] <- feols(female_winner_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * high_f_literacy | dist_samiti_2010,
  data = raj_05_10_open,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# Rajasthan 10-15: No FE
models[[3]] <- feols(female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * high_f_literacy,
  data = raj_10_15_open,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# Rajasthan 10-15: With FE
models[[4]] <- feols(female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * high_f_literacy | dist_samiti_2015,
  data = raj_10_15_open,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# Rajasthan 15-20: No FE
models[[5]] <- feols(female_winner_2020 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * high_f_literacy,
  data = raj_15_20_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# Rajasthan 15-20: With FE
models[[6]] <- feols(female_winner_2020 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * high_f_literacy | dist_samiti_2020,
  data = raj_15_20_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# UP 05-10: No FE
models[[7]] <- feols(female_winner_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * high_f_literacy,
  data = up_05_10_open,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# UP 05-10: With FE
models[[8]] <- feols(female_winner_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * high_f_literacy | dist_block_2010,
  data = up_05_10_open,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# UP 10-15: No FE
models[[9]] <- feols(female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * high_f_literacy,
  data = up_10_15_open,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# UP 10-15: With FE
models[[10]] <- feols(female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * high_f_literacy | dist_block_2015,
  data = up_10_15_open,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# UP 15-21: No FE
models[[11]] <- feols(female_winner_2021 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * high_f_literacy,
  data = up_15_21_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# UP 15-21: With FE
models[[12]] <- feols(female_winner_2021 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * high_f_literacy | dist_block_2021,
  data = up_15_21_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Output table
# =============================================================================

dict_combined <- c(
  "female_winner_2010" = "Woman Elected",
  "female_winner_2015" = "Woman Elected",
  "female_winner_2020" = "Woman Elected",
  "female_winner_2021" = "Woman Elected",
  "treat_2005" = "$\\text{Quota}_{t-1}$",
  "treat_2010" = "$\\text{Quota}_{t-1}$",
  "treat_2015" = "$\\text{Quota}_{t-1}$",
  "near_town" = "Near Town",
  "high_infra" = "High Infrastructure",
  "high_f_literacy" = "High Female Literacy",
  "treat_2005:near_town" = "$\\text{Quota}_{t-1} \\times$ Near Town",
  "treat_2005:high_infra" = "$\\text{Quota}_{t-1} \\times$ High Infra",
  "treat_2005:high_f_literacy" = "$\\text{Quota}_{t-1} \\times$ High F. Literacy",
  "treat_2010:near_town" = "$\\text{Quota}_{t-1} \\times$ Near Town",
  "treat_2010:high_infra" = "$\\text{Quota}_{t-1} \\times$ High Infra",
  "treat_2010:high_f_literacy" = "$\\text{Quota}_{t-1} \\times$ High F. Literacy",
  "treat_2015:near_town" = "$\\text{Quota}_{t-1} \\times$ Near Town",
  "treat_2015:high_infra" = "$\\text{Quota}_{t-1} \\times$ High Infra",
  "treat_2015:high_f_literacy" = "$\\text{Quota}_{t-1} \\times$ High F. Literacy",
  "(Intercept)" = "Intercept",
  "dist_samiti_2010" = "(District, Samiti)",
  "dist_samiti_2015" = "(District, Samiti)",
  "dist_samiti_2020" = "(District, Samiti)",
  "dist_block_2010" = "(District, Block)",
  "dist_block_2015" = "(District, Block)",
  "dist_block_2021" = "(District, Block)"
)

aer_etable(models,
  file = here("tabs", "het_effects_combined.tex"),
  headers = list(
    rep(c("Rajasthan", "Uttar Pradesh"), c(6, 6)),
    rep(c("05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$20", "05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$21"), c(2, 2, 2, 2, 2, 2)),
    c(
      "No FE", "FE", "No FE", "FE", "No FE", "FE",
      "No FE", "FE", "No FE", "FE", "No FE", "FE"
    )
  ),
  keep = c("(Intercept)", "%treat_", "%near_town", "%high_infra", "%high_f_literacy"),
  notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Outcome: woman elected in open seat. Near Town = 1 if distance to nearest town $\\leq$ median. High Infrastructure = 1 if sum of (education facility, medical facility, power supply, bank facility) $>$ median. High Female Literacy = 1 if female literacy rate $>$ median (females aged 7+ who are literate / total females aged 7+). All variables from Census 2001 Village Directory via SHRUG (LGD Block Panchayat matching). Standard errors clustered by district-samiti (Rajasthan) or district-block (UP).",
  dict = dict_combined
)

message("\nCreated: tabs/het_effects_combined.tex")
message("\n=== Consolidated Heterogeneous Effects Analysis Complete ===")

# Compare mean and minimum observed village-to-town distances on the same moderators.
distance_models <- list()
distance_stats <- list()
heterogeneity_support <- list()
for (state in c("raj", "up")) {
  panels <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (panel in panels) {
    years <- paste0("20", strsplit(panel, "_")[[1]])
    outcome <- paste0("female_winner_", years[2])
    treatment <- paste0("treat_", years[1])
    current <- paste0("treat_", years[2])
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])
    raw <- read_parquet(here("data", state, paste0("shrug_gp_", state, "_", panel, "_block.parquet")))
    for (distance in c("mean", "minimum")) {
      data <- raw
      if (distance == "minimum") data$pc01_vd_dist_town <- data$pc01_vd_dist_town_min
      data <- add_het_vars(data) %>% filter(.data[[current]] == 0)
      required <- c(outcome, treatment, block, "near_town", "high_infra", "high_f_literacy")
      usable <- data %>% filter(if_all(all_of(required), ~ !is.na(.)))
      heterogeneity_support[[length(heterogeneity_support) + 1L]] <- data.frame(state, panel, distance,
        open_n = nrow(data), analysis_n = nrow(usable), missing_infrastructure = sum(is.na(data$high_infra)),
        high_infrastructure_n = sum(data$high_infra == 1, na.rm = TRUE)
      )
      formula <- as.formula(paste(outcome, "~", treatment, "* (near_town + high_infra + high_f_literacy) |", block))
      model <- feols(formula,
        data = usable, vcov = as.formula(paste("~", block)),
        ssc = MODEL_SSC, fixef.rm = "none"
      )
      id <- paste(state, panel, distance)
      distance_models[[id]] <- model
      distance_stats[[id]] <- broom::tidy(model, conf.int = TRUE) %>% mutate(state = state, panel = panel, distance = distance, n = nobs(model))
    }
  }
}
write.csv(bind_rows(distance_stats), here("tabs/heterogeneity_distance_sensitivity.csv"), row.names = FALSE)
write.csv(bind_rows(heterogeneity_support), here("tabs/heterogeneity_sample_support.csv"), row.names = FALSE)
bind_rows(distance_stats) |>
  filter(grepl(":", term)) |>
  mutate(
    State = recode(state, raj = "Rajasthan", up = "Uttar Pradesh"),
    Elections = vapply(strsplit(panel, "_"), function(years) paste0("20", years, collapse = "--"), character(1)),
    Characteristic = recode(sub(".*:", "", term), near_town = "Near town",
      high_infra = "High infrastructure", high_f_literacy = "High female literacy"),
    value = paste0(format_number(100 * estimate), " [", format_number(100 * conf.low),
      ", ", format_number(100 * conf.high), "]")
  ) |>
  select(State, Elections, Characteristic, distance, value) |>
  tidyr::pivot_wider(names_from = distance, values_from = value) |>
  knitr::kable(format = "latex", booktabs = TRUE, align = c("l", "l", "l", "r", "r"),
    col.names = c("State", "Elections", "Characteristic", "Mean distance", "Minimum distance")) |>
  as.character() |>
  writeLines(here("tabs/heterogeneity_distance_sensitivity.tex"))
