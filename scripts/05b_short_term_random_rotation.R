library(dplyr)
library(arrow)
library(fixest)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

models <- list()
eligibility <- list()
for (state in c("raj", "up")) {
  panels <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (panel in panels) {
    years <- paste0("20", strsplit(panel, "_")[[1]])
    treatment <- paste0("treat_", years[1])
    current <- paste0("treat_", years[2])
    outcome <- paste0("female_winner_", years[2])
    district <- paste0(if (state == "raj") "district_std_" else "district_name_eng_", years[2])
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])
    data <- as.data.frame(read_election_panel(state, paste0(state, "_", panel, ".parquet")))
    p <- vapply(split(data, data[[district]]), function(d) {
      tab <- table(factor(d[[treatment]], levels = 0:1), factor(d[[current]], levels = 0:1))
      if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
        return(NA_real_)
      }
      suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
    }, numeric(1))
    selected <- names(p)[!is.na(p) & p > 0.05]
    sample <- data[data[[district]] %in% selected & !is.na(data[[current]]) & data[[current]] == 0, ]
    eligibility[[paste(state, panel)]] <- data.frame(state, panel, districts = length(p), retained = length(selected), sample_n = sum(complete.cases(sample[, c(outcome, treatment, block)])))
    if (nrow(sample) == 0) next
    for (fe in c(FALSE, TRUE)) {
      formula <- as.formula(paste(outcome, "~", treatment, if (fe) paste("|", block) else ""))
      models[[paste(state, panel, if (fe) "FE" else "No FE")]] <- feols(formula,
        data = sample,
        vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"
      )
    }
  }
}
write.csv(bind_rows(eligibility), here("tabs/short_term_restriction_support.csv"), row.names = FALSE)
aer_etable(models,
  file = here("tabs/short_term_random_rotation.tex"),
  headers = list(
    rep(c("Rajasthan", "Uttar Pradesh"), c(6, 4)),
    rep(c("2005--2010", "2010--2015", "2015--2020", "2005--2010", "2010--2015"), each = 2),
    rep(c("No FE", "FE"), 5)
  ),
  dict = c(DICT_RAJ, DICT_UP), keep = "%treat_",
  notes = paste(
    "Outcome: woman elected in the next election, among seats open in that election.",
    "Districts are selected when the descriptive independence test for consecutive quota assignments has p > 0.05.",
    "Non-rejection does not establish random assignment. No UP districts qualify for 2015-2021.",
    "Standard errors are clustered by district-samiti or district-block; singleton groups are retained."
  )
)
