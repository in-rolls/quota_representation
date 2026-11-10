# 04d_balance.R
# Descriptive balance and block-adjusted joint tests
# Tests whether treatment assignment (YEAR1 quota) is balanced on observables
# Uses SHRUG-matched data from block matching
#
# Output:
#   - tabs/balance_raj.tex (combined, used in manuscript)
#   - tabs/balance_up.tex (combined, used in manuscript)

library(readr)
library(dplyr)
library(arrow)
library(tidyr)
library(purrr)
library(here)
library(fixest)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))
source(here("scripts/00_census.R"))

message("=== Descriptive Balance and Block-Adjusted Joint Tests ===")

balance_vars <- as.list(CENSUS_BALANCE)
normalize_balance_vars <- census_balance_covariates

# =============================================================================
# Helper: Descriptive pooled likelihood-ratio statistic
# =============================================================================

compute_lr_stat <- function(data, treatment_var, covariates) {
  fml <- as.formula(paste(treatment_var, "~", paste(covariates, collapse = " + ")))
  mod <- tryCatch(
    {
      glm(fml, data = data, family = binomial(link = "logit"))
    },
    error = function(e) NULL
  )

  if (is.null(mod)) {
    return(NA)
  }

  null_mod <- glm(as.formula(paste(treatment_var, "~ 1")), data = data, family = binomial(link = "logit"))
  lr_test <- anova(null_mod, mod, test = "LRT")
  return(lr_test$Deviance[2])
}

# =============================================================================
# Helper: Compute 4-way group means
# =============================================================================

compute_group_means <- function(data, treat_y1, treat_y2, variables) {
  data <- data %>%
    mutate(
      group = case_when(
        !!sym(treat_y1) == 0 & !!sym(treat_y2) == 0 ~ "NF-NF",
        !!sym(treat_y1) == 0 & !!sym(treat_y2) == 1 ~ "NF-F",
        !!sym(treat_y1) == 1 & !!sym(treat_y2) == 0 ~ "F-NF",
        !!sym(treat_y1) == 1 & !!sym(treat_y2) == 1 ~ "F-F"
      )
    )

  group_means <- data %>%
    group_by(group) %>%
    summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE)), n = n(), .groups = "drop")

  return(group_means)
}

# =============================================================================
# Helper: Compute standardized differences (Cohen's d variant)
# =============================================================================

compute_std_diff <- function(data, treatment_var, variables) {
  sapply(variables, function(v) {
    quota_rows <- data[[treatment_var]] == 1
    open_rows <- data[[treatment_var]] == 0

    m1 <- mean(data[[v]][quota_rows], na.rm = TRUE)
    m2 <- mean(data[[v]][open_rows], na.rm = TRUE)
    s1 <- sd(data[[v]][quota_rows], na.rm = TRUE)
    s2 <- sd(data[[v]][open_rows], na.rm = TRUE)

    pooled_sd <- sqrt((s1^2 + s2^2) / 2)
    if (is.na(pooled_sd) || pooled_sd == 0) {
      return(NA_real_)
    }

    abs(m1 - m2) / pooled_sd
  })
}

# =============================================================================
# Helper: Compute balance statistics for a panel
# =============================================================================

compute_balance_stats <- function(data, treat_y1, treat_y2, panel_name) {
  message("\nComputing balance for", panel_name)
  message("Sample N: ", nrow(data))

  if (nrow(data) < 100) {
    message("WARNING: Sample too small for reliable inference")
    return(NULL)
  }

  # Get available balance variables
  avail_vars <- intersect(names(balance_vars), names(data))
  message("Available balance variables: ", length(avail_vars))

  if (length(avail_vars) == 0) {
    message("ERROR: No balance variables found in data")
    return(NULL)
  }

  # Drop rows with NA in treatment or covariates
  data_complete <- data %>%
    filter(!is.na(!!sym(treat_y1)) & !is.na(!!sym(treat_y2))) %>%
    drop_na(all_of(avail_vars))
  message("Complete cases: ", nrow(data_complete))

  # Compute 4-way group means
  group_means <- compute_group_means(data_complete, treat_y1, treat_y2, avail_vars)

  # T-tests for Y1 treatment
  ttest_results <- map_dfr(avail_vars, function(v) {
    t_result <- tryCatch(
      {
        t.test(data_complete[[v]] ~ data_complete[[treat_y1]])
      },
      error = function(e) list(p.value = NA)
    )

    data.frame(
      Variable = balance_vars[[v]],
      var_name = v,
      Mean_Treat = mean(data_complete[[v]][data_complete[[treat_y1]] == 1], na.rm = TRUE),
      Mean_Control = mean(data_complete[[v]][data_complete[[treat_y1]] == 0], na.rm = TRUE),
      p_ttest = t_result$p.value
    )
  })

  # Descriptive pooled joint test
  message("Computing pooled likelihood-ratio statistic...")
  fstat <- compute_lr_stat(data_complete, treat_y1, avail_vars)

  # Compute p-value from chi-squared distribution
  df <- length(avail_vars)
  if (!is.na(fstat)) {
    p_fstat <- 1 - pchisq(fstat, df = df)
    message("LR statistic: ", round(fstat, 2), ", df:", df, ", p:", round(p_fstat, 4))
  } else {
    p_fstat <- NA
    message("LR computation failed")
  }

  block <- paste0(if (startsWith(panel_name, "Raj")) "dist_samiti_" else "dist_block_", sub("treat_", "", treat_y2))
  joint <- feols(as.formula(paste(treat_y1, "~", paste(avail_vars, collapse = " + "), "|", block)),
    data = data_complete, vcov = as.formula(paste("~", block)),
    ssc = MODEL_SSC, fixef.rm = "none"
  )
  p_block <- wald(joint, keep = avail_vars, print = FALSE)$p

  # Sample sizes
  n_treat <- sum(data_complete[[treat_y1]] == 1)
  n_control <- sum(data_complete[[treat_y1]] == 0)

  # Compute standardized differences
  std_diffs <- compute_std_diff(data_complete, treat_y1, avail_vars)
  max_std_diff <- max(std_diffs, na.rm = TRUE)
  message("Standardized differences (max): ", round(max_std_diff, 3))

  list(
    group_means = group_means,
    ttest_results = ttest_results,
    n_treat = n_treat,
    n_control = n_control,
    n_total = nrow(data_complete),
    fstat = fstat,
    df = df,
    p_fstat = p_fstat,
    p_block = p_block,
    panel_name = panel_name,
    std_diffs = std_diffs,
    max_std_diff = max_std_diff
  )
}

# =============================================================================
# Load SHRUG-matched panels and compute balance
# =============================================================================

# Rajasthan 05-10
message("\n=== Rajasthan 05-10 ===")
raj_05_10 <- read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")) %>%
  normalize_balance_vars()
raj_stats_0510 <- compute_balance_stats(raj_05_10, "treat_2005", "treat_2010", "Raj 05-10")

# Rajasthan 10-15
message("\n=== Rajasthan 10-15 ===")
raj_10_15 <- read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")) %>%
  normalize_balance_vars()
raj_stats_1015 <- compute_balance_stats(raj_10_15, "treat_2010", "treat_2015", "Raj 10-15")

# Rajasthan 15-20
message("\n=== Rajasthan 15-20 ===")
raj_15_20 <- read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")) %>%
  normalize_balance_vars()
raj_stats_1520 <- compute_balance_stats(raj_15_20, "treat_2015", "treat_2020", "Raj 15-20")

# =============================================================================
# Combined Balance Table (all Rajasthan panels)
# =============================================================================

message("\n=== Creating Combined Balance Table ===")

if (!is.null(raj_stats_0510) && !is.null(raj_stats_1015) && !is.null(raj_stats_1520)) {
  fmt <- function(x, digits = 1) {
    ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
  }

  combined_tex <- c(
    "\\begin{table}[htbp]",
    "\\caption{Rajasthan: Balance Tests for Quota Assignment}",
    "\\label{tab:balance_raj_2005_2010}",
    "{\\centering\\scriptsize",
    "\\begin{tabular}{@{}lrrr rrr rrr@{}}",
    "\\toprule",
    "& \\multicolumn{3}{c}{2005$\\rightarrow$2010} & \\multicolumn{3}{c}{2010$\\rightarrow$2015} & \\multicolumn{3}{c}{2015$\\rightarrow$2020} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
    "Variable & Quota & Open & p & Quota & Open & p & Quota & Open & p \\\\",
    "\\midrule"
  )

  # Variables
  for (i in 1:nrow(raj_stats_0510$ttest_results)) {
    v0510 <- raj_stats_0510$ttest_results[i, ]
    v1015 <- raj_stats_1015$ttest_results[i, ]
    v1520 <- raj_stats_1520$ttest_results[i, ]

    line <- paste0(
      v0510$Variable, " & ",
      fmt(v0510$Mean_Treat * ifelse(v0510$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", fmt(v0510$Mean_Control * ifelse(v0510$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", format_p_value(v0510$p_ttest), " & ",
      fmt(v1015$Mean_Treat * ifelse(v1015$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", fmt(v1015$Mean_Control * ifelse(v1015$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", format_p_value(v1015$p_ttest), " & ",
      fmt(v1520$Mean_Treat * ifelse(v1520$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", fmt(v1520$Mean_Control * ifelse(v1520$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", format_p_value(v1520$p_ttest), " \\\\"
    )
    combined_tex <- c(combined_tex, line)
  }

  combined_tex <- c(
    combined_tex,
    "\\midrule",
    paste0(
      "N & \\multicolumn{3}{c}{", raj_stats_0510$n_total, "} & ",
      "\\multicolumn{3}{c}{", raj_stats_1015$n_total, "} & ",
      "\\multicolumn{3}{c}{", raj_stats_1520$n_total, "} \\\\"
    ),
    paste0(
      "Joint p (LR) & \\multicolumn{3}{c}{", format_p_value(raj_stats_0510$p_fstat), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(raj_stats_1015$p_fstat), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(raj_stats_1520$p_fstat), "} \\\\"
    ),
    paste0(
      "Joint p (block) & \\multicolumn{3}{c}{", format_p_value(raj_stats_0510$p_block), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(raj_stats_1015$p_block), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(raj_stats_1520$p_block), "} \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par}",
    "",
    "\\vspace{0.5ex}",
    "\\parbox{\\linewidth}{\\scriptsize \\emph{Notes:} Balance tests for quota assignment across all Rajasthan panels. ``Quota'' = GPs reserved for women in base year; ``Open'' = GPs not reserved. Covariates from the 2001 Census Village Directory (SHRUG). Availability is the percentage of GPs with an observed facility in at least one linked village; absence requires complete observed coverage. Counts per 1k use GP population. Maternity/child centers are maternity and child welfare centers. Water-source code sums are excluded because they do not identify facility counts or availability. p-values from t-tests. Joint p (LR) is a descriptive pooled logistic likelihood ratio test. Joint p (block) is a Wald test with district-samiti/block fixed effects and standard errors clustered at that level. These are balance diagnostics, not randomization inference.}",
    "\\end{table}"
  )

  writeLines(combined_tex, here("tabs/balance_raj.tex"))
  message("Created: tabs/balance_raj.tex")
}

# =============================================================================
# UTTAR PRADESH Balance Tables
# =============================================================================

message("\n=== Uttar Pradesh 05-10 ===")
up_05_10 <- read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")) %>%
  normalize_balance_vars()
up_stats_0510 <- compute_balance_stats(up_05_10, "treat_2005", "treat_2010", "UP 05-10")

message("\n=== Uttar Pradesh 10-15 ===")
up_10_15 <- read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")) %>%
  normalize_balance_vars()
up_stats_1015 <- compute_balance_stats(up_10_15, "treat_2010", "treat_2015", "UP 10-15")

message("\n=== Uttar Pradesh 15-21 ===")
up_15_21 <- read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")) %>%
  normalize_balance_vars()
up_stats_1521 <- compute_balance_stats(up_15_21, "treat_2015", "treat_2021", "UP 15-21")

# Combined UP Balance Table
message("\n=== Creating Combined UP Balance Table ===")

if (!is.null(up_stats_0510) && !is.null(up_stats_1015) && !is.null(up_stats_1521)) {
  combined_up_tex <- c(
    "\\begin{table}[htbp]",
    "\\caption{Uttar Pradesh: Balance Tests for Quota Assignment}",
    "\\label{tab:balance_up_2005_2010}",
    "{\\centering\\scriptsize",
    "\\begin{tabular}{@{}lrrr rrr rrr@{}}",
    "\\toprule",
    "& \\multicolumn{3}{c}{2005$\\rightarrow$2010} & \\multicolumn{3}{c}{2010$\\rightarrow$2015} & \\multicolumn{3}{c}{2015$\\rightarrow$2021} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
    "Variable & Quota & Open & p & Quota & Open & p & Quota & Open & p \\\\",
    "\\midrule"
  )

  for (i in 1:nrow(up_stats_0510$ttest_results)) {
    v0510 <- up_stats_0510$ttest_results[i, ]
    v1015 <- up_stats_1015$ttest_results[i, ]
    v1521 <- up_stats_1521$ttest_results[i, ]

    line <- paste0(
      v0510$Variable, " & ",
      fmt(v0510$Mean_Treat * ifelse(v0510$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", fmt(v0510$Mean_Control * ifelse(v0510$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", format_p_value(v0510$p_ttest), " & ",
      fmt(v1015$Mean_Treat * ifelse(v1015$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", fmt(v1015$Mean_Control * ifelse(v1015$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", format_p_value(v1015$p_ttest), " & ",
      fmt(v1521$Mean_Treat * ifelse(v1521$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", fmt(v1521$Mean_Control * ifelse(v1521$var_name %in% CENSUS_BALANCE_PERCENT, 100, 1)), " & ", format_p_value(v1521$p_ttest), " \\\\"
    )
    combined_up_tex <- c(combined_up_tex, line)
  }

  combined_up_tex <- c(
    combined_up_tex,
    "\\midrule",
    paste0(
      "N & \\multicolumn{3}{c}{", up_stats_0510$n_total, "} & ",
      "\\multicolumn{3}{c}{", up_stats_1015$n_total, "} & ",
      "\\multicolumn{3}{c}{", up_stats_1521$n_total, "} \\\\"
    ),
    paste0(
      "Joint p (LR) & \\multicolumn{3}{c}{", format_p_value(up_stats_0510$p_fstat), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(up_stats_1015$p_fstat), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(up_stats_1521$p_fstat), "} \\\\"
    ),
    paste0(
      "Joint p (block) & \\multicolumn{3}{c}{", format_p_value(up_stats_0510$p_block), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(up_stats_1015$p_block), "} & ",
      "\\multicolumn{3}{c}{", format_p_value(up_stats_1521$p_block), "} \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par}",
    "",
    "\\vspace{0.5ex}",
    "\\parbox{\\linewidth}{\\scriptsize \\emph{Notes:} Balance tests for quota assignment across all Uttar Pradesh panels. ``Quota'' = GPs reserved for women in base year; ``Open'' = GPs not reserved. Covariates from the 2001 Census Village Directory (SHRUG). Availability is the percentage of GPs with an observed facility in at least one linked village; absence requires complete observed coverage. Counts per 1k use GP population. Maternity/child centers are maternity and child welfare centers. Water-source code sums are excluded because they do not identify facility counts or availability. p-values from t-tests. Joint p (LR) is a descriptive pooled logistic likelihood ratio test. Joint p (block) is a Wald test with district-samiti/block fixed effects and standard errors clustered at that level. These are balance diagnostics, not randomization inference.}",
    "\\end{table}"
  )

  writeLines(combined_up_tex, here("tabs/balance_up.tex"))
  message("Created: tabs/balance_up.tex")
}

# =============================================================================
# Log Standardized Differences
# =============================================================================

message("\n=== STANDARDIZED DIFFERENCES SUMMARY ===")

std_diff_log <- c(
  "Standardized Differences for Balance Covariates",
  "================================================",
  paste0("Generated: ", Sys.time()),
  "",
  "Formula: |mean_quota - mean_open| / sqrt((sd_quota^2 + sd_open^2) / 2)",
  "Threshold for concern: 0.20 (Austin 2009)",
  "",
  "=== RAJASTHAN ===",
  ""
)

if (!is.null(raj_stats_0510)) {
  std_diff_log <- c(
    std_diff_log,
    "Panel 2005->2010:",
    paste0("  Max standardized diff: ", round(raj_stats_0510$max_std_diff, 4)),
    paste0("  By covariate:")
  )
  for (nm in names(raj_stats_0510$std_diffs)) {
    std_diff_log <- c(
      std_diff_log,
      paste0("    ", balance_vars[[nm]], ": ", round(raj_stats_0510$std_diffs[nm], 4))
    )
  }
  std_diff_log <- c(std_diff_log, "")
}

if (!is.null(raj_stats_1015)) {
  std_diff_log <- c(
    std_diff_log,
    "Panel 2010->2015:",
    paste0("  Max standardized diff: ", round(raj_stats_1015$max_std_diff, 4)),
    paste0("  By covariate:")
  )
  for (nm in names(raj_stats_1015$std_diffs)) {
    std_diff_log <- c(
      std_diff_log,
      paste0("    ", balance_vars[[nm]], ": ", round(raj_stats_1015$std_diffs[nm], 4))
    )
  }
  std_diff_log <- c(std_diff_log, "")
}

if (!is.null(raj_stats_1520)) {
  std_diff_log <- c(
    std_diff_log,
    "Panel 2015->2020:",
    paste0("  Max standardized diff: ", round(raj_stats_1520$max_std_diff, 4)),
    paste0("  By covariate:")
  )
  for (nm in names(raj_stats_1520$std_diffs)) {
    std_diff_log <- c(
      std_diff_log,
      paste0("    ", balance_vars[[nm]], ": ", round(raj_stats_1520$std_diffs[nm], 4))
    )
  }
  std_diff_log <- c(std_diff_log, "")
}

raj_all_std_diffs <- c(
  if (!is.null(raj_stats_0510)) raj_stats_0510$std_diffs else NULL,
  if (!is.null(raj_stats_1015)) raj_stats_1015$std_diffs else NULL,
  if (!is.null(raj_stats_1520)) raj_stats_1520$std_diffs else NULL
)
raj_max_all <- max(raj_all_std_diffs, na.rm = TRUE)
std_diff_log <- c(
  std_diff_log,
  paste0("RAJASTHAN MAX (all panels): ", round(raj_max_all, 4)),
  ""
)

std_diff_log <- c(
  std_diff_log,
  "=== UTTAR PRADESH ===",
  ""
)

if (!is.null(up_stats_0510)) {
  std_diff_log <- c(
    std_diff_log,
    "Panel 2005->2010:",
    paste0("  Max standardized diff: ", round(up_stats_0510$max_std_diff, 4)),
    paste0("  By covariate:")
  )
  for (nm in names(up_stats_0510$std_diffs)) {
    std_diff_log <- c(
      std_diff_log,
      paste0("    ", balance_vars[[nm]], ": ", round(up_stats_0510$std_diffs[nm], 4))
    )
  }
  std_diff_log <- c(std_diff_log, "")
}

if (!is.null(up_stats_1015)) {
  std_diff_log <- c(
    std_diff_log,
    "Panel 2010->2015:",
    paste0("  Max standardized diff: ", round(up_stats_1015$max_std_diff, 4)),
    paste0("  By covariate:")
  )
  for (nm in names(up_stats_1015$std_diffs)) {
    std_diff_log <- c(
      std_diff_log,
      paste0("    ", balance_vars[[nm]], ": ", round(up_stats_1015$std_diffs[nm], 4))
    )
  }
  std_diff_log <- c(std_diff_log, "")
}

if (!is.null(up_stats_1521)) {
  std_diff_log <- c(
    std_diff_log,
    "Panel 2015->2021:",
    paste0("  Max standardized diff: ", round(up_stats_1521$max_std_diff, 4)),
    paste0("  By covariate:")
  )
  for (nm in names(up_stats_1521$std_diffs)) {
    std_diff_log <- c(
      std_diff_log,
      paste0("    ", balance_vars[[nm]], ": ", round(up_stats_1521$std_diffs[nm], 4))
    )
  }
  std_diff_log <- c(std_diff_log, "")
}

up_all_std_diffs <- c(
  if (!is.null(up_stats_0510)) up_stats_0510$std_diffs else NULL,
  if (!is.null(up_stats_1015)) up_stats_1015$std_diffs else NULL,
  if (!is.null(up_stats_1521)) up_stats_1521$std_diffs else NULL
)
up_max_all <- max(up_all_std_diffs, na.rm = TRUE)
std_diff_log <- c(
  std_diff_log,
  paste0("UP MAX (all panels): ", round(up_max_all, 4)),
  ""
)

overall_max <- max(c(raj_all_std_diffs, up_all_std_diffs), na.rm = TRUE)
std_diff_log <- c(
  std_diff_log,
  "=== OVERALL SUMMARY ===",
  paste0("Maximum standardized difference (all states, all panels): ", round(overall_max, 4))
)

writeLines(std_diff_log, here("tabs/std_diff_log.txt"))
message("Created: tabs/std_diff_log.txt")

cat("\n")
cat(std_diff_log, sep = "\n")

message("\n=== Balance Tables Complete ===")

balance_results <- list(raj_05_10 = raj_stats_0510, raj_10_15 = raj_stats_1015, raj_15_20 = raj_stats_1520,
  up_05_10 = up_stats_0510, up_10_15 = up_stats_1015, up_15_21 = up_stats_1521)
write_csv(imap_dfr(balance_results, ~ mutate(.x$ttest_results, comparison = .y,
  n = .x$n_total, joint_p = .x$p_block)), here("tabs/census_balance.csv"))
