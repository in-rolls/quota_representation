library(here)
options(warn = 1)
scripts <- c(
  "02c_bihar_panels.R", "02d_haryana_panels.R", "02e_city_panels.R",
  "02f_uttarakhand_panels.R", "02g_tamil_nadu_panels.R", "02h_jharkhand_panels.R",
  "02i_telangana_panels.R", "10_bihar.R", "11_haryana.R", "12_cities.R",
  "13_uttarakhand.R", "14_tamil_nadu.R", "15_jharkhand.R", "16_telangana.R",
  "98j_validate_exploratory.R"
)
for (script in scripts) {
  message("Running ", script)
  source(here("scripts", script), local = new.env(parent = globalenv()))
}
