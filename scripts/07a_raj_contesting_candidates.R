library(tidyverse)
library(arrow)
library(fixest)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

candidates <- read_raj(raj_product_path("candidates_2020_events.parquet"))
winners <- read_raj(raj_product_path("winners_2020_events.parquet"))
candidacy <- candidates |>
  filter(gp_event_unique) |>
  summarise(
    candidate_count = if (all(candidate_key_unique)) n() else NA_integer_,
    num_women = if (all(candidate_key_unique) && !anyNA(candidate_female)) {
      sum(candidate_female)
    } else {
      NA_integer_
    },
    prop_women = num_women / candidate_count,
    women_present = case_when(
      any(candidate_female == 1 & candidate_key_unique, na.rm = TRUE) ~ 1L,
      all(candidate_key_unique) && !anyNA(candidate_female) ~ 0L
    ), .by = match_key
  )
votes <- winners |>
  filter(winner_key_unique, gp_event_unique) |>
  mutate(
    across(
      c(vote_secure_by_winner, vote_secure_by_runnerup, total_valid_votes),
      ~ parse_number(as.character(.x), na = c("", "NA", "--", "-"))
    ),
    fem_vote_share = if_else(
      total_valid_votes > 0 & vote_secure_by_winner >= 0 & vote_secure_by_runnerup >= 0 &
        vote_secure_by_winner + vote_secure_by_runnerup <= total_valid_votes,
      (female_winner_2020 * vote_secure_by_winner +
        female_runnerup_2020 * vote_secure_by_runnerup) / total_valid_votes,
      NA_real_
    )
  ) |>
  select(match_key, fem_vote_share)

panel <- read_raj(raj_product_path("raj_05_20.parquet"))
analysis <- panel |>
  left_join(candidacy,
    by = c("match_key_2020" = "match_key"),
    relationship = "many-to-one", na_matches = "never"
  ) |>
  left_join(votes,
    by = c("match_key_2020" = "match_key"),
    relationship = "many-to-one", na_matches = "never"
  )
stopifnot(nrow(panel) == nrow(analysis))
write_parquet(analysis, here("data/raj/candidacy_analysis.parquet"))
open_seats <- analysis |> filter(treat_2020 == 0)
models <- list()
for (outcome in c("prop_women", "num_women", "women_present", "fem_vote_share")) {
  models[[paste0(outcome, "_ols")]] <- feols(
    as.formula(paste(outcome, "~ treat_2005 * treat_2010 * treat_2015")),
    data = open_seats, vcov = ~dist_samiti_2020, ssc = MODEL_SSC, fixef.rm = "none"
  )
  models[[paste0(outcome, "_fe")]] <- feols(
    as.formula(paste(outcome, "~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020")),
    data = open_seats, vcov = ~dist_samiti_2020, ssc = MODEL_SSC, fixef.rm = "none"
  )
}
aer_etable(models,
  file = here("tabs/raj_candidacy_combined.tex"),
  title = "Candidacy in Open Seats (2020, Rajasthan)", label = "tab:candidacy_combined",
  headers = list(rep(c("Prop. women", "Num. women", "Any woman", "Vote share"), each = 2), rep(c("No FE", "FE"), 4)),
  dict = DICT_RAJ,
  notes = paste(
    "The sample comprises GPs open in 2020 with observed treatment histories.",
    "Candidate records are linked within the same general-election phase and GP.",
    "The vote outcome is votes for women among the top two candidates divided by all valid votes;",
    "it does not include votes for women placed third or lower.",
    "Unresolved candidate counts or sex are unavailable where needed for the outcome.",
    "Standard errors clustered by district--samiti. Even columns add district--samiti fixed effects."
  )
)
model_results <- imap_dfr(models, ~ broom::tidy(.x, conf.int = TRUE) |>
  mutate(model = .y, observations = nobs(.x)))
write_csv(model_results, here("tabs/candidacy_estimates.csv"))
