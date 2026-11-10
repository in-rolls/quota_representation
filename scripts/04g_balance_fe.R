# 04g_balance_fe.R
# Balance tests conditional on district-samiti/block fixed effects
#
# Output:
#   - tabs/balance_fe_raj.tex
#   - tabs/balance_fe_up.tex

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(arrow)
library(fixest)
library(kableExtra)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_census.R"))

message("=== Balance Tests with District-Samiti/Block Fixed Effects ===")

balance_vars <- CENSUS_BALANCE

# =============================================================================
# Helper: Run FE balance test for one panel
# =============================================================================

run_fe_balance <- function(data, treat_var, fe_var) {
  avail_vars <- names(balance_vars)

  data_clean <- census_balance_covariates(data) %>%
    filter(!is.na(.data[[treat_var]]), !is.na(.data[[fe_var]])) %>%
    drop_na(all_of(avail_vars))

  # Run regression for each covariate: covariate ~ treatment | district
  results <- map_dfr(avail_vars, function(v) {
    fml <- as.formula(paste0(v, " ~ ", treat_var, " | ", fe_var))
    mod <- feols(fml, data = data_clean, vcov = as.formula(paste("~", fe_var)), ssc = MODEL_SSC, fixef.rm = "none", warn = FALSE, notes = FALSE)
    tidy(mod) %>%
      filter(term == treat_var) %>%
      mutate(variable = v,
        estimate = estimate * ifelse(v %in% CENSUS_BALANCE_PERCENT, 100, 1),
        std.error = std.error * ifelse(v %in% CENSUS_BALANCE_PERCENT, 100, 1))
  })

  # Joint Wald test with clustered standard errors
  fml_joint <- as.formula(paste0(treat_var, " ~ ", paste(avail_vars, collapse = " + "), " | ", fe_var))
  mod_joint <- feols(fml_joint, data = data_clean, vcov = as.formula(paste("~", fe_var)), ssc = MODEL_SSC, fixef.rm = "none", warn = FALSE, notes = FALSE)
  joint_p <- tryCatch(
    {
      wald(mod_joint, keep = avail_vars)$p
    },
    error = function(e) NA_real_
  )

  list(
    results = results %>%
      mutate(label = balance_vars[variable]) %>%
      select(label, estimate, std.error, p.value),
    joint_p = joint_p,
    n = nrow(data_clean)
  )
}

# =============================================================================
# Rajasthan
# =============================================================================

message("\n=== RAJASTHAN ===")

raj_0510 <- run_fe_balance(
  read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")),
  "treat_2005", "dist_samiti_2010"
)
raj_1015 <- run_fe_balance(
  read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")),
  "treat_2010", "dist_samiti_2015"
)
raj_1520 <- run_fe_balance(
  read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")),
  "treat_2015", "dist_samiti_2020"
)

# Combine into wide format
raj_table <- raj_0510$results %>%
  rename(coef_0510 = estimate, se_0510 = std.error, p_0510 = p.value) %>%
  left_join(
    raj_1015$results %>%
      rename(coef_1015 = estimate, se_1015 = std.error, p_1015 = p.value),
    by = "label"
  ) %>%
  left_join(
    raj_1520$results %>%
      rename(coef_1520 = estimate, se_1520 = std.error, p_1520 = p.value),
    by = "label"
  )

# Add footer row
raj_footer <- tibble(
  label = c("N", "Joint p (Wald)"),
  coef_0510 = c(raj_0510$n, raj_0510$joint_p),
  se_0510 = NA, p_0510 = NA,
  coef_1015 = c(raj_1015$n, raj_1015$joint_p),
  se_1015 = NA, p_1015 = NA,
  coef_1520 = c(raj_1520$n, raj_1520$joint_p),
  se_1520 = NA, p_1520 = NA
)

raj_out <- bind_rows(raj_table, raj_footer) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

kable(raj_out,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Variable", rep(c("Coef", "SE", "p"), 3)),
  caption = "Rajasthan: Balance Tests with District-Samiti/Block Fixed Effects",
  label = "balance_fe_raj",
  align = c("l", rep("r", 9)),
  escape = FALSE
) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(
    " " = 1,
    "2005→2010" = 3,
    "2010→2015" = 3,
    "2015→2020" = 3
  )) %>%
  footnote(general = "Each row regresses the covariate on quota assignment with block fixed effects and clustering. Percentage outcomes use percentage-point differences. Facility counts are per 1,000 residents; availability indicates any linked village with the facility, with complete coverage required for absence. Water-source code sums are excluded because their aggregation does not preserve availability.") %>%
  save_kable(here("tabs/balance_fe_raj.tex"))

message("Created: tabs/balance_fe_raj.tex")

# =============================================================================
# Uttar Pradesh
# =============================================================================

message("\n=== UTTAR PRADESH ===")

up_0510 <- run_fe_balance(
  read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")),
  "treat_2005", "dist_block_2010"
)
up_1015 <- run_fe_balance(
  read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")),
  "treat_2010", "dist_block_2015"
)
up_1521 <- run_fe_balance(
  read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")),
  "treat_2015", "dist_block_2021"
)

up_table <- up_0510$results %>%
  rename(coef_0510 = estimate, se_0510 = std.error, p_0510 = p.value) %>%
  left_join(
    up_1015$results %>%
      rename(coef_1015 = estimate, se_1015 = std.error, p_1015 = p.value),
    by = "label"
  ) %>%
  left_join(
    up_1521$results %>%
      rename(coef_1521 = estimate, se_1521 = std.error, p_1521 = p.value),
    by = "label"
  )

up_footer <- tibble(
  label = c("N", "Joint p (Wald)"),
  coef_0510 = c(up_0510$n, up_0510$joint_p),
  se_0510 = NA, p_0510 = NA,
  coef_1015 = c(up_1015$n, up_1015$joint_p),
  se_1015 = NA, p_1015 = NA,
  coef_1521 = c(up_1521$n, up_1521$joint_p),
  se_1521 = NA, p_1521 = NA
)

up_out <- bind_rows(up_table, up_footer) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

kable(up_out,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Variable", rep(c("Coef", "SE", "p"), 3)),
  caption = "Uttar Pradesh: Balance Tests with District-Samiti/Block Fixed Effects",
  label = "balance_fe_up",
  align = c("l", rep("r", 9)),
  escape = FALSE
) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(
    " " = 1,
    "2005→2010" = 3,
    "2010→2015" = 3,
    "2015→2021" = 3
  )) %>%
  footnote(general = "Each row regresses the covariate on quota assignment with block fixed effects and clustering. Percentage outcomes use percentage-point differences. Facility counts are per 1,000 residents; availability indicates any linked village with the facility, with complete coverage required for absence. Water-source code sums are excluded because their aggregation does not preserve availability.") %>%
  save_kable(here("tabs/balance_fe_up.tex"))

message("Created: tabs/balance_fe_up.tex")

message("\n=== FE Balance Tests Complete ===")
