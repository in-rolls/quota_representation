library(here)
options(warn = 1)

scripts <- c(
  "02b_up_recode.R", "02c_bihar_panels.R", "02d_haryana_panels.R",
  "01g_audit_crosswalk_provenance.R",
  "03a_raj_shrug_match.R", "03b_up_shrug_match.R",
  "08a_phone_survey_tables.R", "04a_descriptive_tables.R", "04c_transition_matrices.R",
  "04d_balance.R", "04e_balance_electoral.R", "04g_balance_fe.R",
  "05a_short_term_main.R", "05b_short_term_random_rotation.R",
  "05c_short_term_treatment_rotation.R", "05d_short_term_placebo.R",
  "05e_short_term_covariates.R", "05f_short_term_het_effects.R",
  "06a_long_term_main.R", "06b_long_term_treatment_rotation.R",
  "06c_long_term_random_rotation.R", "09a_model_validation.R", "10_bihar.R", "11_haryana.R",
  "04f_power_analysis.R", "07a_raj_contesting_candidates.R",
  "07b_candidate_quality.R",
  "09_weaver_replication.R", "97_manuscript_numbers.R", "98_validate.R"
)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--from-panels"))
if ("--from-panels" %in% args) {
  scripts <- scripts[!grepl("^0[1-3]", scripts) & scripts != "08a_phone_survey_tables.R"]
  message("Using committed analysis panels and survey tables.")
}
for (script in scripts) {
  message("Running ", script)
  source(here("scripts", script), local = new.env(parent = globalenv()))
}
message("Analysis and validation complete.")
