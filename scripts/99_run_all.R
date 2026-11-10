library(here)
options(warn = 1)
for (directory in c("data/up", "data/raj", "tabs", "figs")) {
  dir.create(here(directory), recursive = TRUE, showWarnings = FALSE)
}

scripts <- c(
  "02b_up_recode.R",
  "01g_audit_crosswalk_provenance.R",
  "03a_raj_shrug_match.R", "03b_up_shrug_match.R",
  "08a_phone_survey_tables.R", "04a_descriptive_tables.R", "04c_transition_matrices.R",
  "04d_balance.R", "04e_balance_electoral.R", "04g_balance_fe.R",
  "05a_short_term_main.R", "05b_short_term_random_rotation.R",
  "05c_short_term_treatment_rotation.R", "05d_short_term_placebo.R",
  "05e_short_term_covariates.R", "05f_short_term_het_effects.R",
  "06a_long_term_main.R", "06b_long_term_treatment_rotation.R",
  "06c_long_term_random_rotation.R", "09a_model_validation.R", "05g_short_term_pooling.R",
  "04f_power_analysis.R", "07a_raj_contesting_candidates.R",
  "07b_candidate_quality.R",
  "09_weaver_replication.R", "13_up_urban.R", "10_additional_evidence.R", "11_persistence_horizons.R",
  "12_recontesting.R", "14_bihar_recontesting.R",
  "97_manuscript_numbers.R", "98_validate.R", "98b_validate_additional_evidence.R",
  "98c_validate_persistence.R", "98d_validate_recontesting.R", "98e_validate_pooling.R",
  "98f_validate_characteristics.R", "98g_validate_descriptives.R", "98h_validate_up_urban.R",
  "98i_validate_bihar_recontesting.R"
)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--from-panels"))
if ("--from-panels" %in% args) {
  scripts <- scripts[!grepl("^0[1-3]", scripts) & scripts != "08a_phone_survey_tables.R"]
  message("Using committed analysis panels and survey tables.")
}
for (script in scripts) {
  message("Running ", script)
  env <- new.env(parent = globalenv())
  source(here("scripts", script), local = env)
  if (script == "05g_short_term_pooling.R") env$build_short_term_pooling()
  if (script == "10_additional_evidence.R") {
    source(here("wb/run.R"), local = env)
    env$build_wb(here("wb"))
    source(here("wb/tests/run_tests.R"), local = env)
    env$run_wb_tests(here("wb"))
    env$build_additional_evidence()
  }
  if (script == "11_persistence_horizons.R") env$build_persistence_horizons()
  if (script == "12_recontesting.R") env$build_recontesting()
  if (script == "13_up_urban.R") env$build_up_urban()
  if (script == "14_bihar_recontesting.R") env$build_bihar_recontesting()
}
message("Analysis and validation complete.")
