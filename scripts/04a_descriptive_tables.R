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
raj_2005 <- read_raj(raj_path("data/release/2005/sarpanch_results.parquet"))
raj_2010 <- read_raj(raj_path("data/release/2010/sarpanch_results.parquet"))
raj_2015 <- read_raj(raj_path("data/release/2015/sarpanch_results.parquet"))
raj_2020 <- read_raj(raj_path("data/release/2020/sarpanch_reservations.parquet"))

election_coverage <- function(data, state, year, district, block, gp, reservation) {
  clean_name <- function(x) {
    value <- stringr::str_to_lower(stringr::str_squish(stringi::stri_trans_nfc(as.character(x))))
    dplyr::na_if(value, "")
  }
  records <- tibble(
    district = clean_name(data[[district]]), block = clean_name(data[[block]]),
    gp = clean_name(data[[gp]]), reservation = reservation
  )
  stopifnot(all(is.na(records$reservation) | records$reservation %in% 0:1))
  identified <- records |> filter(if_all(c(district, block, gp), ~ !is.na(.x)))
  offices <- identified |> summarise(
    reservation_conflict = n_distinct(reservation, na.rm = TRUE) > 1L,
    reservation = if (n_distinct(reservation, na.rm = TRUE) == 1L) {
      unique(reservation[!is.na(reservation)])
    } else NA_integer_,
    .by = c(district, block, gp)
  )
  tibble(state, year, source_records = nrow(records), identified_gps = nrow(offices),
    missing_identity_records = nrow(records) - nrow(identified),
    duplicate_identity_records = nrow(identified) - nrow(offices),
    blocks = nrow(distinct(filter(records, !is.na(district), !is.na(block)), district, block)),
    districts = n_distinct(records$district, na.rm = TRUE),
    known_reservation_gps = sum(!is.na(offices$reservation)),
    quota_gps = sum(offices$reservation == 1L, na.rm = TRUE),
    conflicting_reservation_gps = sum(offices$reservation_conflict),
    quota_share = mean(offices$reservation, na.rm = TRUE))
}

raj_coverage <- bind_rows(lapply(c(2005, 2010, 2015, 2020), function(year) {
  data <- get(paste0("raj_", year))
  election_coverage(data, "Rajasthan", year, "district_raw", "samiti_raw", "gp_std", data$female_reserved)
}))
coverage_stats <- function(coverage, block_name) {
  stats <- list(max_gp = max(coverage$identified_gps))
  for (j in seq_len(nrow(coverage))) {
    row <- coverage[j, ]
    stats[[paste0("gp_", row$year)]] <- row$identified_gps
    stats[[paste0(block_name, "_", row$year)]] <- row$blocks
    stats[[paste0("dist_", row$year)]] <- row$districts
    stats[[paste0("women_res_", row$year)]] <- round(100 * row$quota_share, 1)
  }
  stats
}
raj_stats <- coverage_stats(raj_coverage, "samiti")

# The consolidated release contains one source winner record per GP and year.
up_records <- read_up_election_records()
up_coverage <- bind_rows(lapply(c(2005, 2010, 2015, 2021), function(year) {
  data <- filter(up_records, election_year == year)
  data$gp_identity <- if (year == 2021L) {
    ifelse(is.na(data$gp_number_raw) | is.na(data$gp_name_hindi), NA_character_,
      paste(data$gp_number_raw, "-", data$gp_name_hindi))
  } else data$gp_name_hindi
  election_coverage(data, "Uttar Pradesh", year,
    "district_name_hindi", "block_name_hindi", "gp_identity", data$reserved_woman)
}))
up_stats <- coverage_stats(up_coverage, "block")
write_csv(bind_rows(raj_coverage, up_coverage), here("tabs/election_coverage.csv"))

phone_counts <- read_csv(here("tabs/phone_contact_counts.csv"), show_col_types = FALSE) |>
  filter(section == "Contact attempts", category == "Answered")
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
    "2005, 2010, 2015, 2020", "2005, 2010, 2015, 2021", "2020",
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
    "Identified GPs", "Blocks", "Districts", "Women reserved (\\%)",
    "Identified GPs", "Blocks", "Districts", "Women reserved (\\%)"
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
  col.names = c("", "2005", "2010", "2015", "2020/2021"),
  caption = "Summary of Gram Panchayats by Election Year",
  label = "gp_summary_combined",
  align = c("l", "r", "r", "r", "r"),
  escape = FALSE
) %>%
  kable_styling(latex_options = "hold_position", font_size = 8) %>%
  pack_rows("Panel A: Rajasthan", 1, 4, italic = TRUE) %>%
  pack_rows("Panel B: Uttar Pradesh", 5, 8, italic = TRUE) %>%
  footnote(
    general = "Identified GPs are distinct complete district--block--GP names in the source files, using cleaned GP names. Missing identities are excluded from GP counts; repeated names within a block count once. Reservation shares use identified GPs with known, non-conflicting reservation. District and block counts use observed names. The last column contains Rajasthan 2020 and UP 2021. Coverage changes reflect both administrative changes and source availability.",
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

# Next-election samples and the composition of the non-quota group.
next_election_samples <- list()
for (state in c("raj", "up")) {
  periods <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (period in periods) {
    years <- paste0("20", strsplit(period, "_")[[1]])
    treatment <- paste0("treat_", years[1])
    current <- paste0("treat_", years[2])
    prior_winner <- paste0("female_winner_", years[1])
    outcome <- paste0("female_winner_", years[2])
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])
    panel <- read_analysis_parquet(election_panel_path(state, paste0(state, "_", period, ".parquet")))
    open <- panel |> filter(.data[[current]] == 0)
    analysis <- open[complete.cases(open[c(treatment, outcome, block)]), ]
    control <- analysis |> filter(.data[[treatment]] == 0)
    observed <- sum(!is.na(control[[prior_winner]]))
    women <- sum(control[[prior_winner]] == 1, na.rm = TRUE)
    stopifnot(observed > 0L, all(na.omit(control[[prior_winner]]) %in% 0:1))
    next_election_samples[[paste(state, period)]] <- data.frame(
      state, period, treatment_year = as.integer(years[1]), outcome_year = as.integer(years[2]),
      linked = nrow(panel), open_at_followup = nrow(open), analysis_n = nrow(analysis),
      quota_n = sum(analysis[[treatment]] == 1), control_n = nrow(control),
      control_sex_observed = observed, control_women = women,
      control_women_share = women / observed,
      control_followup_women_share = mean(control[[outcome]])
    )
  }
}
next_election_samples <- bind_rows(next_election_samples)
write_csv(next_election_samples, here("tabs/next_election_samples.csv"))
next_election_display <- next_election_samples |>
  transmute(
    Elections = paste(treatment_year, outcome_year, sep = "--"),
    Linked = linked, Analyzed = analysis_n, Quota = quota_n, Nonquota = control_n,
    SexObserved = control_sex_observed,
    Women = sprintf("%.1f", 100 * control_women_share)
  ) |>
  mutate(across(where(is.numeric), ~ format(.x, big.mark = ",", trim = TRUE)))
next_election_table <- kbl(next_election_display,
  format = "latex", booktabs = TRUE, escape = FALSE,
  col.names = c("Elections", "Linked", "Analyzed", "Quota", "Non-quota", "Sex observed", "Women (\\%)"),
  align = c("l", rep("r", 6))
) |>
  add_header_above(c(" " = 1, "GP counts" = 4, "Non-quota offices in $t$" = 2), escape = FALSE) |>
  pack_rows("Rajasthan", 1, 3, italic = TRUE) |>
  pack_rows("Uttar Pradesh", 4, 6, italic = TRUE)
writeLines(as.character(next_election_table), here("tabs/next_election_samples.tex"))

# =============================================================================
# Figure: India Map highlighting Rajasthan and UP
# =============================================================================

if ("--map" %in% commandArgs(trailingOnly = TRUE)) {
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
}
