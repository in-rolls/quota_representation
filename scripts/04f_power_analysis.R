library(dplyr)
library(ggplot2)
library(here)
source(here("scripts/00_config.R"))

inference <- read.csv(here("tabs/model_inference.csv"))
power <- inference %>% filter(fixed_effects, variance == "cluster", sample == "full")
write.csv(power, here("tabs/power_analysis.csv"), row.names = FALSE)
display <- power %>% transmute(
  State = recode(state, raj = "Rajasthan", up = "Uttar Pradesh"),
  Elections = vapply(strsplit(period, "_"), function(years) paste0("20", years, collapse = "--"), character(1)),
  Contrast = recode(contrast,
    treat_2005 = "Prior quota vs. no quota", treat_2010 = "Prior quota vs. no quota", treat_2015 = "Prior quota vs. no quota",
    `101-001` = "Extra term in 2005", `011-001` = "Extra term in 2010",
    `111-001` = "Two extra terms", `111-000` = "Three terms vs. none"
  ), N = format(n, big.mark = ",", trim = TRUE),
  `Estimate (pp)` = format_number(100 * estimate), `Cluster SE (pp)` = format_number(100 * standard_error),
  `95% CI (pp)` = sprintf("[%.1f, %.1f]", 100 * conf_low, 100 * conf_high),
  `Approx. MDE (pp)` = format_number(100 * mde_80)
)
write_precision <- function(rows, name, caption) {
  shown <- display[rows, ]
  if (name == "power_next_election") shown$Contrast <- NULL else shown$Elections <- NULL
  tex <- knitr::kable(shown,
    format = "latex", booktabs = TRUE, digits = 2, row.names = FALSE,
    caption = caption, label = name
  ) %>%
    kableExtra::kable_styling(font_size = 8, latex_options = "scale_down") %>%
    kableExtra::footnote(general = paste(
      "Normal-approximation MDEs use 80% power and two-sided 5% size,",
      "computed from each fitted model or full-covariance linear contrast.",
      "They are precision summaries, not evidence of a zero effect or exact bootstrap power."
    ), threeparttable = TRUE)
  writeLines(gsub("[[:blank:]]+\\n", "\n", as.character(tex)), here("tabs", paste0(name, ".tex")))
}
write_precision(grepl("^treat_", power$contrast), "power_next_election", "Precision of Next-Election Estimates")
write_precision(!grepl("^treat_", power$contrast), "power_additional_terms", "Precision of Reservation-History Contrasts")
plot <- ggplot(power, aes(x = 100 * mde_80, y = interaction(state, period, contrast, sep = " "))) +
  geom_point() +
  labs(x = "Approximate minimum detectable effect (percentage points)", y = NULL) +
  theme_pub()
ggsave(here("figs/mde_plot.pdf"), plot, width = 8, height = 6)
