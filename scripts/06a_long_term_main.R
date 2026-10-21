# 06a_long_term_main.R
# Long-term effects analysis using 4-way panels
# Tests: Do accumulated quotas (2005, 2010, 2015) affect 2020/2021 outcomes?
# Output: tabs/long_term_combined.tex

library(dplyr)
library(purrr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Long-Term Effects Analysis ===")

# =============================================================================
# Load 4-way panels
# =============================================================================

raj_05_20 <- read_raj(raj_product_path("raj_05_20.parquet"))
up_05_21 <- read_parquet(here("data/up/up_05_21.parquet"))

message("\nLoaded 4-way panels:")
message("Raj 05-20: ", nrow(raj_05_20), " GPs")
message("UP 05-21: ", nrow(up_05_21), " GPs")

# =============================================================================
# Rajasthan Long-Term Models (outcome: 2020, open seats only)
# =============================================================================
message("\n--- Rajasthan Long-Term (2005-2020) ---")

raj_open <- raj_05_20 %>%
  filter(treat_2020 == 0 & !is.na(female_winner_2020))
message("Open seats in 2020 with winner data: ", nrow(raj_open))

m_raj_lt_nofe <- feols(female_winner_2020 ~ treat_2005 * treat_2010 * treat_2015,
  data = raj_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_lt_fe <- feols(female_winner_2020 ~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020,
  data = raj_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP Long-Term Models (outcome: 2021, open seats only)
# =============================================================================
message("\n--- UP Long-Term (2005-2021) ---")

up_open <- filter(up_05_21, treat_2021 == 0)
message("Open seats in 2021: ", nrow(up_open))

m_up_lt_nofe <- feols(female_winner_2021 ~ treat_2005 * treat_2010 * treat_2015,
  data = up_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_lt_fe <- feols(
  female_winner_2021 ~ treat_2005 * treat_2010 * treat_2015 |
    dist_block_2021,
  data = up_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)


# =============================================================================
# Output Tables
# =============================================================================
message("\n=== Generating output tables ===")

# Long-term interaction models (both Rajasthan and UP)
dict_lt <- c(
  "female_winner_2020" = "Woman Elected",
  "female_winner_2021" = "Woman Elected",
  "treat_2005" = "$\\text{Quota}_{2005}$",
  "treat_2010" = "$\\text{Quota}_{2010}$",
  "treat_2015" = "$\\text{Quota}_{2015}$",
  "(Intercept)" = "Intercept",
  "dist_samiti_2020" = "(District, Samiti)",
  "dist_block_2021" = "(District, Block)"
)

all_lt_models <- list(m_raj_lt_nofe, m_raj_lt_fe, m_up_lt_nofe, m_up_lt_fe)

aer_etable(all_lt_models,
  file = here("tabs", "long_term_combined.tex"),
  headers = list(
    rep(c("Rajasthan", "Uttar Pradesh"), c(2, 2)),
    c("No FE", "FE", "No FE", "FE")
  ),
  keep = c("(Intercept)", "%treat_"),
  notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat (2020 for Rajasthan, 2021 for UP). Sample restricted to GPs where seat was not reserved for women in outcome year. Treatment variables indicate quota status in each prior election year. Standard errors clustered by district-samiti (Rajasthan) or district-block (UP).",
  dict = dict_lt
)

message("Created: tabs/long_term_combined.tex")

# =============================================================================
# Coefficient Plots
# =============================================================================
message("\n=== Generating coefficient plots ===")

library(ggplot2)
library(broom)
library(tidyr)

if (!dir.exists(here("figs"))) dir.create(here("figs"))

coef_long <- broom::tidy(m_raj_lt_fe, conf.int = TRUE) %>%
  filter(!grepl("Intercept|dist_samiti", term)) %>%
  mutate(
    label = case_when(
      term == "treat_2015" ~ "Quota 2015",
      term == "treat_2010" ~ "Quota 2010",
      term == "treat_2005" ~ "Quota 2005",
      term == "treat_2010:treat_2015" ~ "2015 × 2010",
      term == "treat_2005:treat_2015" ~ "2015 × 2005",
      term == "treat_2005:treat_2010" ~ "2010 × 2005",
      term == "treat_2005:treat_2010:treat_2015" ~ "2015 × 2010 × 2005",
      TRUE ~ term
    )
  )

coef_long$label <- factor(coef_long$label, levels = rev(c(
  "Quota 2005", "Quota 2010", "Quota 2015",
  "2010 × 2005", "2015 × 2005", "2015 × 2010", "2015 × 2010 × 2005"
)))

p_long <- ggplot(coef_long, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    orientation = "y",
    aes(xmin = conf.low, xmax = conf.high),
    width = 0, color = COLORS_PUB["secondary"], linewidth = 0.6
  ) +
  geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
  labs(x = "Effect on Pr(Woman Elected in 2020)", y = NULL) +
  theme_pub() +
  theme(axis.text.y = element_text(hjust = 0))

ggsave(here("figs", "long_term_coefplot.pdf"), p_long,
  width = FIG_WIDTH_FULL, height = 4.5, device = cairo_pdf
)
message("Created: figs/long_term_coefplot.pdf")

# Treatment persistence plot
persistence_data <- raj_05_20 %>%
  mutate(
    persist_05_10 = ifelse(treat_2005 == 1, treat_2010, NA),
    persist_10_15 = ifelse(treat_2010 == 1, treat_2015, NA),
    persist_15_20 = ifelse(treat_2015 == 1, treat_2020, NA)
  ) %>%
  summarise(
    trans_05_10 = mean(persist_05_10, na.rm = TRUE),
    trans_10_15 = mean(persist_10_15, na.rm = TRUE),
    trans_15_20 = mean(persist_15_20, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "transition", values_to = "proportion") %>%
  mutate(
    transition = case_when(
      transition == "trans_05_10" ~ "2005 -> 2010",
      transition == "trans_10_15" ~ "2010 -> 2015",
      transition == "trans_15_20" ~ "2015 -> 2020"
    )
  )

persistence_data$transition <- factor(persistence_data$transition,
  levels = c("2005 -> 2010", "2010 -> 2015", "2015 -> 2020")
)

p_persist <- ggplot(persistence_data, aes(x = transition, y = proportion)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_col(fill = COLORS_PUB["primary"], width = 0.6) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "Electoral Transition", y = "Proportion Remaining Quota") +
  annotate("text",
    x = 0.6, y = 0.53, label = "Random (50%)",
    color = "gray50", hjust = 0, size = 3
  ) +
  theme_pub()

ggsave(here("figs", "treatment_persistence.pdf"), p_persist,
  width = FIG_WIDTH_FULL, height = FIG_HEIGHT, device = cairo_pdf
)
message("Created: figs/treatment_persistence.pdf")

message("\n=== Long-Term Analysis Complete ===")
