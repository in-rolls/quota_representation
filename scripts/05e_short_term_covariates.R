# 05e_short_term_covariates.R
# Short-term effects with SHRUG census covariates for ALL panels
# Adds controls: log(population), SC share, literacy share, female share, infrastructure index
# Uses new SHRUG-matched data from block matching (02c_shrug_all_panels_block.R)
# Output: tabs/short_term_shrug_covariates.tex

library(dplyr)
library(tidyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Short-Term Effects with SHRUG Covariates (All Panels) ===")

# =============================================================================
# Helper function: add SHRUG covariates
# =============================================================================

add_shrug_covariates <- function(df) {
  df %>%
    mutate(
      log_pop = log(pc01_pca_tot_p + 1),
      sc_share = ifelse(pc01_pca_tot_p > 0, pc01_pca_p_sc / pc01_pca_tot_p, NA),
      lit_share = ifelse(pc01_pca_tot_p > 0, pc01_pca_p_lit / pc01_pca_tot_p, NA),
      female_share = ifelse(pc01_pca_tot_p > 0, pc01_pca_tot_f / pc01_pca_tot_p, NA),
      infra_index = rowSums(across(
        c(
          pc01_vd_edu_fac, pc01_vd_medi_fac,
          pc01_vd_power_supl, pc01_vd_bank_fac
        ),
        ~ as.numeric(.)
      ), na.rm = FALSE)
    )
}

# =============================================================================
# RAJASTHAN: Load new SHRUG-matched data (block matching)
# =============================================================================

message("\n--- Rajasthan ---")

raj_shrug_05_10 <- read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")) %>%
  add_shrug_covariates()

raj_shrug_10_15 <- read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")) %>%
  add_shrug_covariates()

raj_shrug_15_20 <- read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")) %>%
  add_shrug_covariates()

message("Raj 05-10 rows: ", nrow(raj_shrug_05_10))
message("Raj 10-15 rows: ", nrow(raj_shrug_10_15))
message("Raj 15-20 rows: ", nrow(raj_shrug_15_20))

# =============================================================================
# UTTAR PRADESH: Load SHRUG-matched data
# =============================================================================

message("\n--- Uttar Pradesh ---")

up_shrug_05_10 <- read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")) %>%
  add_shrug_covariates()

up_shrug_10_15 <- read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")) %>%
  add_shrug_covariates()

up_shrug_15_21 <- read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")) %>%
  add_shrug_covariates()

message("UP 05-10 GPs: ", nrow(up_shrug_05_10))
message("UP 10-15 rows: ", nrow(up_shrug_10_15))
message("UP 15-21 rows: ", nrow(up_shrug_15_21))

# =============================================================================
# Rajasthan 2005 → 2010 with SHRUG covariates
# =============================================================================
message("\n--- Rajasthan 2005 → 2010 ---")

raj_05_10_open <- raj_shrug_05_10 %>%
  filter(treat_2010 == 0) %>%
  filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share) & !is.na(female_share))

message("Open seats with covariates: ", nrow(raj_05_10_open))

m_raj_0510_nofe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share + female_share + infra_index,
  data = raj_05_10_open,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_0510_fe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share + female_share + infra_index | dist_samiti_2010,
  data = raj_05_10_open,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Rajasthan 2010 → 2015 with SHRUG covariates
# =============================================================================
message("\n--- Rajasthan 2010 → 2015 ---")

raj_10_15_open <- raj_shrug_10_15 %>%
  filter(treat_2015 == 0) %>%
  filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share) & !is.na(female_share))

message("Open seats with covariates: ", nrow(raj_10_15_open))

m_raj_1015_nofe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share + female_share + infra_index,
  data = raj_10_15_open,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_1015_fe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share + female_share + infra_index | dist_samiti_2015,
  data = raj_10_15_open,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Rajasthan 2015 → 2020 with SHRUG covariates
# =============================================================================
message("\n--- Rajasthan 2015 → 2020 ---")

raj_15_20_open <- raj_shrug_15_20 %>%
  filter(treat_2020 == 0 & !is.na(female_winner_2020)) %>%
  filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share) & !is.na(female_share))

message("Open seats with covariates: ", nrow(raj_15_20_open))

m_raj_1520_nofe <- feols(female_winner_2020 ~ treat_2015 + log_pop + sc_share + lit_share + female_share + infra_index,
  data = raj_15_20_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_1520_fe <- feols(female_winner_2020 ~ treat_2015 + log_pop + sc_share + lit_share + female_share + infra_index | dist_samiti_2020,
  data = raj_15_20_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP 2005 → 2010 with SHRUG covariates
# =============================================================================
message("\n--- UP 2005 → 2010 ---")

up_05_10_open <- up_shrug_05_10 %>%
  filter(treat_2010 == 0) %>%
  filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share) & !is.na(female_share))

message("Open seats with covariates: ", nrow(up_05_10_open))

m_up_0510_nofe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share + female_share + infra_index,
  data = up_05_10_open,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_0510_fe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share + female_share + infra_index | dist_block_2010,
  data = up_05_10_open,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP 2010 → 2015 with SHRUG covariates
# =============================================================================
message("\n--- UP 2010 → 2015 ---")

up_10_15_open <- up_shrug_10_15 %>%
  filter(treat_2015 == 0) %>%
  filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share) & !is.na(female_share))

message("Open seats with covariates: ", nrow(up_10_15_open))

m_up_1015_nofe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share + female_share + infra_index,
  data = up_10_15_open,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_1015_fe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share + female_share + infra_index | dist_block_2015,
  data = up_10_15_open,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP 2015 → 2021 with SHRUG covariates
# =============================================================================
message("\n--- UP 2015 → 2021 ---")

up_15_21_open <- up_shrug_15_21 %>%
  filter(treat_2021 == 0) %>%
  filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share) & !is.na(female_share))

message("Open seats with covariates: ", nrow(up_15_21_open))

m_up_1521_nofe <- feols(female_winner_2021 ~ treat_2015 + log_pop + sc_share + lit_share + female_share + infra_index,
  data = up_15_21_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_1521_fe <- feols(female_winner_2021 ~ treat_2015 + log_pop + sc_share + lit_share + female_share + infra_index | dist_block_2021,
  data = up_15_21_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Combined Output Table
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

dict_shrug <- c(
  "female_winner_2010" = "Woman Elected",
  "female_winner_2015" = "Woman Elected",
  "female_winner_2020" = "Woman Elected",
  "female_winner_2021" = "Woman Elected",
  "treat_2005" = "$\\text{Quota}_{t-1}$",
  "treat_2010" = "$\\text{Quota}_{t-1}$",
  "treat_2015" = "$\\text{Quota}_{t-1}$",
  "log_pop" = "Log(Population)",
  "sc_share" = "SC population (\\%)",
  "lit_share" = "Literacy (\\%)",
  "female_share" = "Female population (\\%)",
  "infra_index" = "Infrastructure Index",
  "(Intercept)" = "Intercept",
  "dist_samiti_2010" = "(District, Samiti)",
  "dist_samiti_2015" = "(District, Samiti)",
  "dist_samiti_2020" = "(District, Samiti)",
  "dist_block_2010" = "(District, Block)",
  "dist_block_2015" = "(District, Block)",
  "dist_block_2021" = "(District, Block)"
)

aer_etable(all_models,
  file = here("tabs", "short_term_shrug_covariates.tex"),
  headers = list(
    rep(c("Rajasthan", "Uttar Pradesh"), c(6, 6)),
    rep(c("05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$20", "05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$21"), c(2, 2, 2, 2, 2, 2)),
    c(
      "No FE", "FE", "No FE", "FE", "No FE", "FE",
      "No FE", "FE", "No FE", "FE", "No FE", "FE"
    )
  ),
  keep = c("(Intercept)", "%treat_", "%log_pop", "%sc_share", "%lit_share", "%female_share", "%infra_index"),
  notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Outcome: woman elected in open seat. Census covariates from 2001 Census via SHRUG (LGD Block Panchayat matching). Population, SC share, literacy, and female share computed at GP level. Infrastructure index is the sum of education, medical, power, and banking facilities. Sample restricted to GPs with SHRUG match and non-missing covariates. Standard errors clustered by district-samiti (Rajasthan) or district-block (UP).",
  dict = dict_shrug
)

message("Created: tabs/short_term_shrug_covariates.tex")

message("\n=== Short-Term with SHRUG Covariates Complete ===")

readr::write_csv(purrr::imap_dfr(all_models, ~ broom::tidy(.x, conf.int = TRUE) |>
  dplyr::mutate(model = .y, n = nobs(.x))), here("tabs/short_term_covariate_estimates.csv"))

sample_comparisons <- list()
open_balance <- list()
allocation_checks <- list()
for (state in c("raj", "up")) {
  periods <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (period in periods) {
    years <- paste0("20", strsplit(period, "_")[[1]])
    treatment <- paste0("treat_", years[1])
    current <- paste0("treat_", years[2])
    outcome <- paste0("female_winner_", years[2])
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])
    baseline_block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[1])
    caste <- paste0(if (state == "raj") "caste_category_" else "reservation_class_", years[1])
    data <- read_parquet(here("data", state, paste0("shrug_gp_", state, "_", period, "_block.parquet"))) |>
      add_shrug_covariates() |> filter(.data[[current]] == 0)
    if (state == "raj") {
      district_name <- data[[paste0("district_std_", years[1])]]
      samiti_name <- data[[paste0("samiti_std_", years[1])]]
      data[[baseline_block]] <- ifelse(is.na(district_name) | is.na(samiti_name), NA_character_,
        paste(district_name, samiti_name, sep = "::"))
    }
    covariates <- c("log_pop", "sc_share", "lit_share", "female_share", "infra_index")
    complete <- data[complete.cases(data[c(outcome, treatment, block, covariates)]), ]
    plain <- as.formula(paste(outcome, "~", treatment, "|", block))
    adjusted <- as.formula(paste(outcome, "~", treatment, "+", paste(covariates, collapse = " + "), "|", block))
    fits <- list(
      full = feols(plain, data = data, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"),
      common = feols(plain, data = complete, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"),
      adjusted = feols(adjusted, data = complete, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none")
    )
    stopifnot(nobs(fits$common) == nobs(fits$adjusted))
    sample_comparisons[[paste(state, period)]] <- purrr::imap_dfr(fits, ~ broom::tidy(.x, conf.int = TRUE) |>
      filter(term == treatment) |> mutate(state = state, period = period, specification = .y, n = nobs(.x)))
    joint <- feols(as.formula(paste(treatment, "~", paste(covariates, collapse = " + "), "|", block)),
      data = complete, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none")
    open_balance[[paste(state, period)]] <- data.frame(
      state, period, n = nobs(joint), blocks = n_distinct(complete[[block]]),
      joint_p = wald(joint, keep = covariates, print = FALSE)$p
    )
    allocation_sample <- data[complete.cases(data[c(outcome, treatment, block, baseline_block, caste)]), ]
    allocation_sample$allocation_group <- interaction(allocation_sample[[baseline_block]], allocation_sample[[caste]], drop = TRUE)
    allocation_fits <- list(
      outcome_block = feols(plain, data = allocation_sample,
        vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"),
      treatment_block = feols(as.formula(paste(outcome, "~", treatment, "|", baseline_block)), data = allocation_sample,
        vcov = as.formula(paste("~", baseline_block)), ssc = MODEL_SSC, fixef.rm = "none"),
      treatment_block_caste = feols(as.formula(paste(outcome, "~", treatment, "| allocation_group")), data = allocation_sample,
        vcov = as.formula(paste("~", baseline_block)), ssc = MODEL_SSC, fixef.rm = "none")
    )
    allocation_checks[[paste(state, period)]] <- purrr::imap_dfr(allocation_fits, ~ broom::tidy(.x, conf.int = TRUE) |>
      filter(term == treatment) |> mutate(state = state, period = period, specification = .y, n = nobs(.x)))
  }
}
sample_comparisons <- bind_rows(sample_comparisons)
open_balance <- bind_rows(open_balance)
allocation_checks <- bind_rows(allocation_checks)
readr::write_csv(sample_comparisons, here("tabs/short_term_sample_comparisons.csv"))
readr::write_csv(open_balance, here("tabs/open_sample_balance.csv"))
readr::write_csv(allocation_checks, here("tabs/short_term_allocation_checks.csv"))

format_estimate <- function(estimate, low, high) {
  sprintf("%.1f [%.1f, %.1f]", 100 * estimate, 100 * low, 100 * high)
}
write_check_table <- function(rows, filename, headers) {
  table <- knitr::kable(rows, format = "latex", booktabs = TRUE, escape = FALSE,
    col.names = headers, align = paste0("l", strrep("r", ncol(rows) - 1))) |>
    kableExtra::pack_rows("Rajasthan", 1, 3) |>
    kableExtra::pack_rows("Uttar Pradesh", 4, 6)
  writeLines(table, here("tabs", filename))
}
sample_table <- sample_comparisons |>
  mutate(value = paste0("\\shortstack{", format_estimate(estimate, conf.low, conf.high),
    "\\\\$N=", format(n, big.mark = ",", trim = TRUE), "$}")) |>
  select(state, period, specification, value) |>
  pivot_wider(names_from = specification, values_from = value) |>
  mutate(elections = gsub("_", "--20", paste0("20", period))) |>
  select(elections, full, common, adjusted)
write_check_table(sample_table, "short_term_sample_comparisons.tex",
  c("Elections", "Full sample", "Census sample", "Census sample, adjusted"))
allocation_table <- allocation_checks |>
  mutate(value = format_estimate(estimate, conf.low, conf.high)) |>
  select(state, period, n, specification, value) |>
  pivot_wider(names_from = specification, values_from = value) |>
  mutate(elections = gsub("_", "--20", paste0("20", period)), n = format(n, big.mark = ",", trim = TRUE)) |>
  select(elections, n, outcome_block, treatment_block, treatment_block_caste)
write_check_table(allocation_table, "short_term_allocation_checks.tex",
  c("Elections", "$N$", "Outcome block", "Treatment block", "Block $\\times$ caste"))
balance_table <- open_balance |>
  mutate(elections = gsub("_", "--20", paste0("20", period)),
    n = format(n, big.mark = ",", trim = TRUE),
    joint_p = format_p_value(joint_p)) |>
  select(elections, n, blocks, joint_p)
write_check_table(balance_table, "open_sample_balance.tex", c("Elections", "$N$", "Blocks", "Joint $p$-value"))
