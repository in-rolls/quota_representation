# 09_weaver_replication.R
# Weaver Data Replication for UP 2010-2021 (source labels the last wave 2020)
# Outputs: tabs/weaver_short_term.tex, tabs/weaver_long_term.tex

library(arrow)
library(dplyr)
library(fixest)
library(haven)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# Load Weaver data
jw <- read_parquet(up_path("weaver/weaver_20250317_wide.parquet")) %>%
  mutate(
    across(where(is.labelled), as.numeric),
    census_block = interaction(anchor_pc11_district_id, anchor_pc11_cdblock_id, drop = TRUE)
  )

message("=== Weaver Data Replication ===")
message("Total GPs: ", nrow(jw))
message("Variables: ", ncol(jw))
message("Unique anchor Census blocks: ", nlevels(jw$census_block), "\n")

# =============================================================================
# 1. SHORT-TERM EFFECTS
# =============================================================================

message("--- Short-Term Effects ---")

# Filter data for each outcome year
jw_open_2015 <- jw %>% filter(reservation_female_2015 == 0)
jw_open_2020 <- jw %>% filter(reservation_female_2020 == 0)

# 2010 -> 2015: No FE
m_10_15 <- feols(winner_female_2015 ~ reservation_female_2010,
  data = jw_open_2015, vcov = ~census_block, ssc = MODEL_SSC, fixef.rm = "none"
)

# 2010 -> 2015: With FE
m_10_15_fe <- feols(winner_female_2015 ~ reservation_female_2010 | census_block,
  data = jw_open_2015, vcov = ~census_block, ssc = MODEL_SSC, fixef.rm = "none"
)

# 2015 -> 2020: No FE
m_15_20 <- feols(winner_female_2020 ~ reservation_female_2015,
  data = jw_open_2020, vcov = ~census_block, ssc = MODEL_SSC, fixef.rm = "none"
)

# 2015 -> 2021: With FE, using the earliest observed Census block anchor
m_15_20_fe <- feols(winner_female_2020 ~ reservation_female_2015 | census_block,
  data = jw_open_2020, vcov = ~census_block, ssc = MODEL_SSC, fixef.rm = "none"
)

message(
  "2010->2015: No FE =", round(coef(m_10_15)["reservation_female_2010"], 4),
  ", FE =", round(coef(m_10_15_fe)["reservation_female_2010"], 4), "\n"
)
message(
  "2015->2020: No FE =", round(coef(m_15_20)["reservation_female_2015"], 4),
  ", FE =", round(coef(m_15_20_fe)["reservation_female_2015"], 4), "\n\n"
)

models_short_term <- list(m_10_15, m_10_15_fe, m_15_20, m_15_20_fe)

aer_etable(models_short_term,
  title = "Short-Term Effects: Weaver UP Replication",
  label = "tab:weaver_short",
  notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat in the subsequent election. Columns [i]-[ii] show effects of 2010 quota on 2015 outcomes; columns [iii]-[iv] show effects of 2015 quota on 2021 outcomes (coded 2020 in the source). Models [i] and [iii] use 2011 Census district--block clustered SE; the fixed-effects models use the same clustering. Models [ii] and [iv] add 2011 Census district--block FE.",
  placement = "htbp",
  headers = c("2015", "2015 (FE)", "2021", "2021 (FE)"),
  dict = c(
    "winner_female_2015" = "Woman Elected",
    "winner_female_2020" = "Woman Elected",
    "reservation_female_2010" = "$\\text{Quota}_{t-1}$",
    "reservation_female_2015" = "$\\text{Quota}_{t-1}$",
    "census_block" = "2011 Census block"
  ),
  file = here("tabs", "weaver_short_term.tex")
)

message("Saved: tabs/weaver_short_term.tex")

# =============================================================================
# 2. LONG-TERM EFFECTS
# =============================================================================

message("--- Long-Term Effects ---")

# Full interaction model: No FE
m_long <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015,
  data = jw_open_2020, vcov = ~census_block, ssc = MODEL_SSC, fixef.rm = "none"
)

# Full interaction model: With FE, using the earliest observed Census block anchor
m_long_fe <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015 | census_block,
  data = jw_open_2020, vcov = ~census_block, ssc = MODEL_SSC, fixef.rm = "none"
)

message("Long-term interaction model:")
message("  No FE: N =", nobs(m_long))
message("  FE: N =", nobs(m_long_fe), "\n")

models_long_term <- list(m_long, m_long_fe)

aer_etable(models_long_term,
  title = "Long-Term Effects: Weaver UP Replication",
  label = "tab:weaver_long",
  notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat in 2021 (coded 2020 in the source). All models include 2010 quota, 2015 quota, and their interaction. Model [i] uses 2011 Census district--block clustered SE; the fixed-effects model uses the same clustering. Model [ii] adds 2011 Census district--block FE.",
  placement = "htbp",
  headers = c("No FE", "FE"),
  dict = c(
    "winner_female_2020" = "Woman Elected",
    "reservation_female_2010" = "$\\text{Quota}_{2010}$",
    "reservation_female_2015" = "$\\text{Quota}_{2015}$",
    "reservation_female_2010:reservation_female_2015" = "$\\text{Quota}_{2010} \\times \\text{Quota}_{2015}$",
    "census_block" = "2011 Census block"
  ),
  file = here("tabs", "weaver_long_term.tex")
)

message("Saved: tabs/weaver_long_term.tex")

message("=== Weaver Replication Complete ===")

readr::write_csv(purrr::imap_dfr(c(models_short_term, models_long_term), ~ broom::tidy(.x, conf.int = TRUE) |> dplyr::mutate(model = .y, n = nobs(.x))), here("tabs/weaver_estimates.csv"))
