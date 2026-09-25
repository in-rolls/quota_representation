# Descriptive Tables: Data Sources and Treatment Status
# Uses kableExtra for table generation (consistent with other scripts)

library(here)
library(readr)
library(arrow)
library(dplyr)
library(kableExtra)
source(here::here("scripts/00_utils.R"))
source(here::here("scripts/00_config.R"))

# =============================================================================
# Load data and compute empirical statistics
# =============================================================================

# Rajasthan year files
raj_2005 <- read_parquet(raj_path("data/fin/source_2005_std.parquet"))
raj_2010 <- read_parquet(raj_path("data/fin/source_2010_std.parquet"))
raj_2015 <- read_parquet(raj_path("data/fin/source_2015_std.parquet"))
raj_2020 <- read_parquet(raj_path("data/fin/source_2020_std.parquet"))

raj_stats <- list(
  gp_2005 = nrow(raj_2005),
  gp_2010 = nrow(raj_2010),
  gp_2015 = nrow(raj_2015),
  gp_2020 = nrow(raj_2020),
  samiti_2005 = n_distinct(paste(raj_2005$district_raw, raj_2005$samiti_raw)),
  samiti_2010 = n_distinct(paste(raj_2010$district_raw, raj_2010$samiti_raw)),
  samiti_2015 = n_distinct(paste(raj_2015$district_raw, raj_2015$samiti_raw)),
  samiti_2020 = n_distinct(paste(raj_2020$district_raw, raj_2020$samiti_raw)),
  dist_2005 = n_distinct(raj_2005$district_raw),
  dist_2010 = n_distinct(raj_2010$district_raw),
  dist_2015 = n_distinct(raj_2015$district_raw),
  dist_2020 = n_distinct(raj_2020$district_raw),
  max_gp = max(nrow(raj_2005), nrow(raj_2010), nrow(raj_2015), nrow(raj_2020)),
  women_res_2005 = round(100 * mean(raj_2005$female_reserved, na.rm = TRUE), 1),
  women_res_2010 = round(100 * mean(raj_2010$female_reserved, na.rm = TRUE), 1),
  women_res_2015 = round(100 * mean(raj_2015$female_reserved, na.rm = TRUE), 1),
  women_res_2020 = round(100 * mean(raj_2020$female_reserved, na.rm = TRUE), 1)
)

# UP year files
up_2005 <- read_parquet(up_path("gp/gp_head_winner_records_2005.parquet"))
up_2010 <- read_parquet(up_path("gp/gp_head_winner_records_2010.parquet"))
up_2015 <- read_parquet(up_path("gp/gp_head_winner_records_2015.parquet"))
up_2021 <- read_parquet(up_path("gp/gp_head_candidates_2021.parquet"))

up_2021_winners <- up_2021[up_2021$result == "विजेता", ]

up_stats <- list(
  gp_2005 = n_distinct(paste(up_2005$district_name, up_2005$block_name, up_2005$gp_name)),
  gp_2010 = n_distinct(paste(up_2010$district_name, up_2010$block_name, up_2010$gp_name)),
  gp_2015 = n_distinct(paste(up_2015$district_name, up_2015$block_name, up_2015$gp_name)),
  gp_2021 = n_distinct(paste(up_2021$district_name, up_2021$block_name, up_2021$gp)),
  block_2005 = n_distinct(paste(up_2005$district_name, up_2005$block_name)),
  block_2010 = n_distinct(paste(up_2010$district_name, up_2010$block_name)),
  block_2015 = n_distinct(paste(up_2015$district_name, up_2015$block_name)),
  block_2021 = n_distinct(paste(up_2021$district_name, up_2021$block_name)),
  dist_2005 = n_distinct(up_2005$district_name),
  dist_2010 = n_distinct(up_2010$district_name),
  dist_2015 = n_distinct(up_2015$district_name),
  dist_2021 = n_distinct(up_2021$district_name),
  max_gp = max(
    n_distinct(paste(up_2005$district_name, up_2005$block_name, up_2005$gp_name)),
    n_distinct(paste(up_2010$district_name, up_2010$block_name, up_2010$gp_name)),
    n_distinct(paste(up_2015$district_name, up_2015$block_name, up_2015$gp_name)),
    n_distinct(paste(up_2021$district_name, up_2021$block_name, up_2021$gp))
  ),
  women_res_2005 = round(100 * mean(grepl("Female", up_2005$gp_res_status_fin_eng, ignore.case = TRUE), na.rm = TRUE), 1),
  women_res_2010 = round(100 * mean(grepl("Female", up_2010$gp_res_status_fin_eng, ignore.case = TRUE), na.rm = TRUE), 1),
  women_res_2015 = round(100 * mean(grepl("Female", up_2015$gp_reservation_status_eng, ignore.case = TRUE), na.rm = TRUE), 1),
  women_res_2021 = round(100 * mean(grepl("Female", up_2021_winners$gp_reservation_status_eng, ignore.case = TRUE), na.rm = TRUE), 1)
)

phone_counts <- read_csv(here("tabs/phone_contact_counts.csv"), show_col_types = FALSE) |>
  filter(section == "Initial contact", category == "Answered")
quota_counts <- filter(phone_counts, survey == "phone_survey")
open_counts <- filter(phone_counts, survey == "phone_survey_openseats")
stopifnot(nrow(quota_counts) == 1L, nrow(open_counts) == 1L)
phone_stats <- list(
  quota_sampled = quota_counts$denominator,
  quota_answered = quota_counts$n,
  open_sampled = open_counts$denominator,
  open_answered = open_counts$n
)

# =============================================================================
# Table 1: Data Sources
# =============================================================================

data_sources_df <- data.frame(
  Source = c(
    "Rajasthan SEC", "Uttar Pradesh SEC", "Candidate Affidavits",
    "Local Government Directory", "SHRUG 2.0", "Census Village Directory",
    "Phone Audit (Quota)", "Phone Audit (Open)"
  ),
  Coverage = c(
    paste0(format(raj_stats$max_gp, big.mark = ","), " GPs"),
    paste0(format(up_stats$max_gp, big.mark = ","), " GPs"),
    "Rajasthan 2020", "India", "India", "India",
    "Rajasthan", "Rajasthan"
  ),
  Years = c(
    "2005, 2010, 2015, 2020", "2005, 2010, 2015", "2020",
    "2020", "1991, 2001, 2011", "2001", "2024", "2024"
  ),
  Variables = c(
    "Reservation status, winner sex", "Reservation status, winner sex",
    "Age, education, assets", "GP-village mapping",
    "Village-level covariates", "Infrastructure, services",
    paste0(phone_stats$quota_sampled, " sampled, ", phone_stats$quota_answered, " answered"),
    paste0(phone_stats$open_sampled, " sampled, ", phone_stats$open_answered, " answered")
  )
)

data_sources_table <- kbl(data_sources_df,
  format = "latex", booktabs = TRUE,
  col.names = c("Data Source", "Coverage", "Years", "Key Variables"),
  caption = "Data Sources", label = "data_sources",
  align = c("l", "c", "c", "c")
) %>%
  kable_styling(latex_options = "hold_position", font_size = 8) %>%
  pack_rows("Panel A: Electoral Data", 1, 3, italic = TRUE) %>%
  pack_rows("Panel B: Geographic \\\\& Socioeconomic Data", 4, 6, italic = TRUE, escape = FALSE) %>%
  pack_rows("Panel C: Primary Data", 7, 8, italic = TRUE) %>%
  footnote(
    general = "SEC = State Election Commission. GP = Gram Panchayat. SHRUG = Socioeconomic High-resolution Rural-Urban Geographic Platform for India. LGD = Local Government Directory.",
    threeparttable = TRUE
  )

save_kable(data_sources_table, file = "tabs/data_sources.tex")

# =============================================================================
# Table 2: Treatment Status (Hypothetical illustration)
# =============================================================================

treat_status_df <- data.frame(
  GP = 1:16,
  Y2005 = c("W", "O", "W", "O", "W", "O", "W", "O", "W", "O", "W", "O", "W", "O", "W", "O"),
  Y2010 = c("O", "W", "W", "O", "O", "W", "W", "O", "O", "W", "W", "O", "O", "W", "W", "O"),
  Y2015 = c("W", "O", "O", "W", "O", "W", "W", "O", "W", "O", "O", "W", "O", "W", "W", "O"),
  Y2020 = c("O", "W", "O", "W", "W", "O", "O", "W", "W", "O", "W", "O", "O", "W", "W", "O")
)

treat_status_table <- kbl(treat_status_df,
  format = "latex", booktabs = TRUE,
  col.names = c("GP", "2005", "2010", "2015", "2020"),
  caption = "Hypothetical Quota Assignment Across Election Cycles",
  label = "treat_status",
  align = c("c", "c", "c", "c", "c")
) %>%
  kable_styling(latex_options = "hold_position", font_size = 8) %>%
  footnote(
    general = "W = Quota seat (reserved for women); O = Open seat (anyone can contest). This table illustrates the randomized assignment of gender quotas across four electoral cycles for a hypothetical set of 16 Gram Panchayats.",
    threeparttable = TRUE
  )

save_kable(treat_status_table, file = "tabs/treat_status.tex")

# =============================================================================
# Table 3: GP Summary (Derived from data)
# =============================================================================

gp_summary_df <- data.frame(
  Metric = c(
    "Gram Panchayats", "Panchayat Samitis", "Districts", "Women Reserved (\\%)",
    "Gram Panchayats", "Panchayat Samitis", "Districts", "Women Reserved (\\%)"
  ),
  Y2005 = c(
    format(raj_stats$gp_2005, big.mark = ","), raj_stats$samiti_2005,
    raj_stats$dist_2005, raj_stats$women_res_2005,
    format(up_stats$gp_2005, big.mark = ","), up_stats$block_2005,
    up_stats$dist_2005, up_stats$women_res_2005
  ),
  Y2010 = c(
    format(raj_stats$gp_2010, big.mark = ","), raj_stats$samiti_2010,
    raj_stats$dist_2010, raj_stats$women_res_2010,
    format(up_stats$gp_2010, big.mark = ","), up_stats$block_2010,
    up_stats$dist_2010, up_stats$women_res_2010
  ),
  Y2015 = c(
    format(raj_stats$gp_2015, big.mark = ","), raj_stats$samiti_2015,
    raj_stats$dist_2015, raj_stats$women_res_2015,
    format(up_stats$gp_2015, big.mark = ","), up_stats$block_2015,
    up_stats$dist_2015, up_stats$women_res_2015
  ),
  Y2020 = c(
    format(raj_stats$gp_2020, big.mark = ","), raj_stats$samiti_2020,
    raj_stats$dist_2020, raj_stats$women_res_2020,
    format(up_stats$gp_2021, big.mark = ","), up_stats$block_2021,
    up_stats$dist_2021, up_stats$women_res_2021
  )
)

gp_summary_table <- kbl(gp_summary_df,
  format = "latex", booktabs = TRUE,
  col.names = c("", "2005", "2010", "2015", "2020"),
  caption = "Summary of Gram Panchayats by Election Year",
  label = "gp_summary_combined",
  align = c("l", "r", "r", "r", "r"),
  escape = FALSE
) %>%
  kable_styling(latex_options = "hold_position", font_size = 8) %>%
  pack_rows("Panel A: Rajasthan", 1, 4, italic = TRUE) %>%
  pack_rows("Panel B: Uttar Pradesh", 5, 8, italic = TRUE) %>%
  footnote(
    general = "GP counts represent the total number of Gram Panchayats for which we have electoral data. The number of GPs increased between 2010 and 2020 due to delimitation (redistricting). The 2020 column for UP contains 2021 election data.",
    threeparttable = TRUE
  )

save_kable(gp_summary_table, file = "tabs/gp_summary.tex")

# =============================================================================
# Table 4: State vs Local Totals (External data)
# =============================================================================

state_local_df <- data.frame(
  Level = c("All State Legislators", "Gram Panchayats", "Total Elected Representatives"),
  Total = c("$\\approx$ 40,000", "$\\approx$ 250,000", "$>$ 2,000,000"),
  Women = c("$\\approx$ 2,000", "---", "$\\approx$ 1,000,000"),
  Pct = c("5.0", "---", "$\\approx$ 50.0")
)

state_local_table <- kbl(state_local_df,
  format = "latex", booktabs = TRUE,
  col.names = c("Level of Government", "Total Representatives", "Women", "\\% Women"),
  caption = "Women's Representation: State vs Local Government",
  label = "state_local_totals",
  align = c("l", "r", "r", "r"),
  escape = FALSE
) %>%
  kable_styling(latex_options = "hold_position", font_size = 8) %>%
  pack_rows("State Legislatures (1950--2024)", 1, 1, italic = TRUE) %>%
  pack_rows("Local Bodies (Current)", 2, 3, italic = TRUE) %>%
  footnote(
    general = "State legislature data compiled from historical records of all state elections since 1950. Local body data from the Ministry of Panchayati Raj and Local Government Directory (2024). The contrast illustrates how gender quotas have dramatically increased women's descriptive representation at the local level compared to higher levels where quotas are not mandated.",
    threeparttable = TRUE
  )

save_kable(state_local_table, file = "tabs/state_local_totals.tex")

message("Generated tabs/data_sources.tex, tabs/treat_status.tex, tabs/gp_summary.tex, and tabs/state_local_totals.tex")

# =============================================================================
# Figure: India Map highlighting Rajasthan and UP
# =============================================================================

message("\n--- Generating India Map ---")

library(sf)
library(ggplot2)

if (!dir.exists(here("figs"))) dir.create(here("figs"))

india_states <- st_read(here("data/shapefiles/India_State_Boundary.shp"), quiet = TRUE)
india_states$name <- india_states$State_Name
india_states$highlight <- case_when(
  grepl("Rajasthan", india_states$name, ignore.case = TRUE) ~ "Rajasthan",
  grepl("Uttar Pradesh", india_states$name, ignore.case = TRUE) ~ "Uttar Pradesh",
  TRUE ~ "Other"
)

india_states$highlight <- factor(
  india_states$highlight,
  levels = c("Rajasthan", "Uttar Pradesh", "Other")
)

p <- ggplot(india_states) +
  geom_sf(aes(fill = highlight), color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "Rajasthan" = "#4A4A4A",
      "Uttar Pradesh" = "#808080",
      "Other" = "#E8E8E8"
    ),
    name = NULL
  ) +
  theme_pub() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  here("figs", "india_map_up_raj.pdf"),
  p,
  width = 5,
  height = 6,
  device = cairo_pdf
)
message("Saved: figs/india_map_up_raj.pdf")
