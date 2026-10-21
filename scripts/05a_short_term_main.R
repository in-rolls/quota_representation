# 05a_short_term_main.R
# Short-term effects analysis for all 2-way panels
# Tests: Does quota in YEAR1 increase women's election in YEAR2?
# Output: tabs/short_term_combined.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Short-Term Effects Analysis ===")

# =============================================================================
# Load all panels
# =============================================================================

raj_05_10 <- read_raj(raj_product_path("raj_05_10.parquet"))
raj_10_15 <- read_raj(raj_product_path("raj_10_15.parquet"))
raj_15_20 <- read_raj(raj_product_path("raj_15_20.parquet"))

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))
up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

message("\nLoaded panels:")
message("Raj 05-10: ", nrow(raj_05_10), " GPs")
message("Raj 10-15: ", nrow(raj_10_15), " GPs")
message("Raj 15-20: ", nrow(raj_15_20), " GPs")
message("UP 05-10: ", nrow(up_05_10), " GPs")
message("UP 10-15: ", nrow(up_10_15), " GPs")
message("UP 15-21: ", nrow(up_15_21), " GPs")

# =============================================================================
# Rajasthan 2005 → 2010 (Open seats in 2010)
# =============================================================================
message("\n--- Rajasthan 2005 → 2010 ---")

raj_05_10_open <- filter(raj_05_10, treat_2010 == 0)
message("Open seats: ", nrow(raj_05_10_open))

m_raj_0510_nofe <- feols(female_winner_2010 ~ treat_2005,
  data = raj_05_10_open,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_0510_fe <- feols(female_winner_2010 ~ treat_2005 | dist_samiti_2010,
  data = raj_05_10_open,
  vcov = ~dist_samiti_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Rajasthan 2010 → 2015 (Open seats in 2015)
# Gender inferred from names where seat is reserved for women or name patterns
# =============================================================================
message("\n--- Rajasthan 2010 → 2015 ---")

raj_10_15_open <- raj_10_15 %>%
  filter(treat_2015 == 0, !is.na(female_winner_2015))
message("Open seats with winner data: ", nrow(raj_10_15_open))

m_raj_1015_nofe <- feols(female_winner_2015 ~ treat_2010,
  data = raj_10_15_open,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_1015_fe <- feols(female_winner_2015 ~ treat_2010 | dist_samiti_2015,
  data = raj_10_15_open,
  vcov = ~dist_samiti_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Rajasthan 2015 → 2020 (Open seats in 2020)
# =============================================================================
message("\n--- Rajasthan 2015 → 2020 ---")

raj_15_20_open <- filter(raj_15_20, treat_2020 == 0 & !is.na(female_winner_2020))
message("Open seats with winner data: ", nrow(raj_15_20_open))

m_raj_1520_nofe <- feols(female_winner_2020 ~ treat_2015,
  data = raj_15_20_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_raj_1520_fe <- feols(female_winner_2020 ~ treat_2015 | dist_samiti_2020,
  data = raj_15_20_open,
  vcov = ~dist_samiti_2020,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP 2005 → 2010 (Open seats in 2010)
# =============================================================================
message("\n--- UP 2005 → 2010 ---")

up_05_10_open <- filter(up_05_10, treat_2010 == 0)
message("Open seats: ", nrow(up_05_10_open))

m_up_0510_nofe <- feols(female_winner_2010 ~ treat_2005,
  data = up_05_10_open,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_0510_fe <- feols(female_winner_2010 ~ treat_2005 | dist_block_2010,
  data = up_05_10_open,
  vcov = ~dist_block_2010,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP 2010 → 2015 (Open seats in 2015)
# =============================================================================
message("\n--- UP 2010 → 2015 ---")

up_10_15_open <- filter(up_10_15, treat_2015 == 0)
message("Open seats: ", nrow(up_10_15_open))

m_up_1015_nofe <- feols(female_winner_2015 ~ treat_2010,
  data = up_10_15_open,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_1015_fe <- feols(female_winner_2015 ~ treat_2010 | dist_block_2015,
  data = up_10_15_open,
  vcov = ~dist_block_2015,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# UP 2015 → 2021 (Open seats in 2021)
# =============================================================================
message("\n--- UP 2015 → 2021 ---")

up_15_21_open <- filter(up_15_21, treat_2021 == 0)
message("Open seats: ", nrow(up_15_21_open))

m_up_1521_nofe <- feols(female_winner_2021 ~ treat_2015,
  data = up_15_21_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)
m_up_1521_fe <- feols(female_winner_2021 ~ treat_2015 | dist_block_2021,
  data = up_15_21_open,
  vcov = ~dist_block_2021,
  fixef.rm = "none",
  ssc = MODEL_SSC
)

# =============================================================================
# Combined Output Table: All panels, NO FE and FE
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

dict_combined <- c(
  "female_winner_2010" = "Woman Elected",
  "female_winner_2015" = "Woman Elected",
  "female_winner_2020" = "Woman Elected",
  "female_winner_2021" = "Woman Elected",
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

aer_etable(all_models,
  file = here("tabs", "short_term_combined.tex"),
  headers = list(
    rep(c("Rajasthan", "Uttar Pradesh"), c(6, 6)),
    rep(c("05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$20", "05$\\rightarrow$10", "10$\\rightarrow$15", "15$\\rightarrow$21"), c(2, 2, 2, 2, 2, 2)),
    c(
      "No FE", "FE", "No FE", "FE", "No FE", "FE",
      "No FE", "FE", "No FE", "FE", "No FE", "FE"
    )
  ),
  keep = c("(Intercept)", "%treat_"),
  notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Outcome: woman elected in open seat. Sample restricted to GPs where seat was not reserved for women in outcome year. Standard errors clustered by district-samiti (Rajasthan) or district-block (UP).",
  dict = dict_combined
)

message("Created: tabs/short_term_combined.tex")

# =============================================================================
# Coefficient Plots
# =============================================================================
message("\n=== Generating coefficient plots ===")

library(ggplot2)
library(broom)

if (!dir.exists(here("figs"))) dir.create(here("figs"))

tidy_model <- function(model, label, state = NULL) {
  df <- broom::tidy(model, conf.int = TRUE) %>%
    filter(!grepl("Intercept|dist_samiti|dist_block", term)) %>%
    mutate(model = label)
  if (!is.null(state)) df$state <- state
  df
}

coef_raj <- bind_rows(
  tidy_model(m_raj_0510_fe, "2005 → 2010", "Rajasthan"),
  tidy_model(m_raj_1015_fe, "2010 → 2015", "Rajasthan"),
  tidy_model(m_raj_1520_fe, "2015 → 2020", "Rajasthan")
)

coef_raj$model <- factor(coef_raj$model, levels = rev(c(
  "2005 → 2010", "2010 → 2015", "2015 → 2020"
)))

p_short_raj <- ggplot(coef_raj, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    orientation = "y",
    aes(xmin = conf.low, xmax = conf.high),
    width = 0, color = COLORS_PUB["secondary"], linewidth = 0.6
  ) +
  geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
  labs(x = "Effect on Pr(Woman Elected)", y = NULL) +
  theme_pub() +
  theme(axis.text.y = element_text(hjust = 0))

ggsave(here("figs", "short_term_coefplot.pdf"), p_short_raj,
  width = FIG_WIDTH_FULL, height = 3.5, device = cairo_pdf
)
message("Created: figs/short_term_coefplot.pdf")

coef_up <- bind_rows(
  tidy_model(m_up_0510_fe, "2005 → 2010", "UP"),
  tidy_model(m_up_1015_fe, "2010 → 2015", "UP"),
  tidy_model(m_up_1521_fe, "2015 → 2021", "UP")
)

coef_up$model <- factor(coef_up$model, levels = rev(c(
  "2005 → 2010", "2010 → 2015", "2015 → 2021"
)))

p_short_up <- ggplot(coef_up, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    orientation = "y",
    aes(xmin = conf.low, xmax = conf.high),
    width = 0, color = COLORS_PUB["secondary"], linewidth = 0.6
  ) +
  geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
  labs(x = "Effect on Pr(Woman Elected)", y = NULL) +
  theme_pub() +
  theme(axis.text.y = element_text(hjust = 0))

ggsave(here("figs", "short_term_coefplot_up.pdf"), p_short_up,
  width = FIG_WIDTH_FULL, height = 3.5, device = cairo_pdf
)
message("Created: figs/short_term_coefplot_up.pdf")

coef_combined <- bind_rows(
  coef_raj,
  coef_up %>% mutate(state = "Uttar Pradesh")
)
coef_combined$model <- factor(coef_combined$model, levels = rev(c(
  "2005 → 2010", "2010 → 2015", "2015 → 2020", "2015 → 2021"
)))
coef_combined$state <- factor(coef_combined$state, levels = c("Rajasthan", "Uttar Pradesh"))

p_short_combined <- ggplot(coef_combined, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    orientation = "y",
    aes(xmin = conf.low, xmax = conf.high),
    width = 0, color = COLORS_PUB["secondary"], linewidth = 0.6
  ) +
  geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
  facet_wrap(~state, ncol = 2) +
  labs(x = "Effect on Pr(Woman Elected)", y = NULL) +
  theme_pub() +
  theme(
    axis.text.y = element_text(hjust = 0),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(here("figs", "short_term_coefplot_combined.pdf"), p_short_combined,
  width = FIG_WIDTH_FULL * 1.5, height = 3.5, device = cairo_pdf
)
message("Created: figs/short_term_coefplot_combined.pdf")

message("\n=== Short-Term Analysis Complete ===")
