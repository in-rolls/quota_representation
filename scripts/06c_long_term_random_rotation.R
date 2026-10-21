library(dplyr)
library(arrow)
library(fixest)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

models <- list()
historical_models <- list()
support <- list()
for (state in c("raj", "up")) {
  end <- if (state == "raj") "2020" else "2021"
  file <- if (state == "raj") "raj_05_20" else "up_05_21"
  district <- if (state == "raj") "district_std_2015" else "district_name_eng_2015"
  block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", end)
  outcome <- paste0("female_winner_", end)
  current <- paste0("treat_", end)
  data <- as.data.frame(read_election_panel(state, paste0(file, ".parquet")))
  transitions <- list(c("2005", "2010"), c("2010", "2015"), c("2015", end))
  eligible <- lapply(transitions, function(years) {
    p <- vapply(split(data, data[[district]]), function(d) {
      tab <- table(
        factor(d[[paste0("treat_", years[1])]], levels = 0:1),
        factor(d[[paste0("treat_", years[2])]], levels = 0:1)
      )
      if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
        return(NA_real_)
      }
      suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
    }, numeric(1))
    names(p)[!is.na(p) & p > 0.05]
  })
  for (k in 2:3) {
    selected <- Reduce(intersect, eligible[seq_len(k)])
    sample <- data[data[[district]] %in% selected & !is.na(data[[current]]) & data[[current]] == 0, ]
    n <- sum(complete.cases(sample[, c(outcome, "treat_2005", "treat_2010", "treat_2015", block)]))
    support[[paste(state, k)]] <- data.frame(state,
      outcome_year = end, transitions = k,
      districts = length(unique(data[[district]])), retained = length(selected), sample_n = n,
      status = if (n == 0) "No eligible observations" else "Estimable"
    )
    if (k == 2) {
      historical_sample <- data[data[[district]] %in% selected & !is.na(data$treat_2015) & data$treat_2015 == 0, ]
      historical_block <- if (state == "raj") "dist_samiti_2015" else "dist_block_2015"
      for (fe in c(FALSE, TRUE)) {
        historical_formula <- as.formula(paste(
          "female_winner_2015 ~ treat_2005 * treat_2010",
          if (fe) paste("|", historical_block) else ""
        ))
        historical_models[[paste(state, "2015", if (fe) "FE" else "No FE")]] <- feols(
          historical_formula,
          data = historical_sample,
          vcov = as.formula(paste("~", historical_block)), ssc = MODEL_SSC, fixef.rm = "none"
        )
      }
    }
    if (k == 3 || n == 0) next
    for (fe in c(FALSE, TRUE)) {
      formula <- as.formula(paste(
        outcome, "~ treat_2005 * treat_2010 * treat_2015",
        if (fe) paste("|", block) else ""
      ))
      models[[paste(state, if (fe) "FE" else "No FE")]] <- feols(formula,
        data = sample,
        vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"
      )
    }
  }
}
write.csv(bind_rows(support), here("tabs/long_term_restriction_support.csv"), row.names = FALSE)
aer_etable(models,
  file = here("tabs/long_term_random_rotation.tex"),
  headers = list(rep(c("Rajasthan 2020", "Uttar Pradesh 2021"), each = 2), rep(c("No FE", "FE"), 2)),
  dict = c(DICT_RAJ, DICT_UP), keep = "%treat_",
  notes = paste(
    "Outcome and full treatment-history specification match the main 2020/2021 analysis.",
    "Districts must have p > 0.05 in both 2005-2010 and 2010-2015 descriptive independence tests.",
    "Requiring all three transitions leaves no UP districts; this is unavailable evidence, not a null estimate.",
    "Selection on test results does not establish random assignment.",
    "Standard errors clustered by district-samiti or district-block; singleton groups retained."
  )
)

aer_etable(historical_models,
  file = here("tabs/long_term_restricted_2015.tex"),
  title = "Historical restricted-sample comparison: 2015 outcomes",
  label = "tab:long_term_restricted_2015", headers = list(rep(c("Rajasthan 2015", "Uttar Pradesh 2015"), each = 2), rep(c("No FE", "FE"), 2)),
  dict = c(DICT_RAJ, DICT_UP), keep = "%treat_",
  notes = paste(
    "Open seats in 2015, restricted using the same two-transition district tests.",
    "The outcome precedes the main 2020/2021 cumulative comparison.",
    "Standard errors clustered by district--samiti or district--block; singletons retained."
  )
)
