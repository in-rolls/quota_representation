# 12_cities.R
# Delhi and Mumbai councils, 2007-2012: quota dependence and the short-term models
# Tests: Does a women's quota in 2007 raise the chance that a woman wins the ward
#   when it is open in 2012?
# Outcome: the winner's sex as coded in each city's replication deposit (see 02e).
# Output: tabs/cities_transitions.csv, tabs/cities_chi_sq.tex,
#   tabs/cities_treatment_reg.tex, tabs/cities_balance_electoral.csv,
#   tabs/cities_short_term.tex, tabs/cities_placebo.tex, tabs/cities_inference.csv,
#   tabs/cities_candidates.tex, figs/cities_short_term_coefplot.pdf
#
# Few women win open council seats here (6 of 134 in Delhi, 7 of 112 in Mumbai),
# so every estimate below is imprecise. Fisher's exact test is reported beside the
# regressions because a count that small is where the normal approximation behind
# a regression standard error is least trustworthy.

library(arrow)
library(dplyr)
library(fixest)
library(ggplot2)
library(here)
library(kableExtra)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))
setFixest_nthreads(1)

message("=== Delhi and Mumbai councils ===")

del <- as.data.frame(read_parquet(here("data/cities/del_07_12.parquet")))
mum <- as.data.frame(read_parquet(here("data/cities/mum_07_12.parquet")))
del$cluster_id <- as.integer(factor(del$ac_2012))

DICT_CITY <- c(
  "treat_2007" = "$\\text{Quota}_{2007}$", "treat_2012" = "$\\text{Quota}_{2012}$",
  "cluster_id" = "Assembly constituency", "city" = "City"
)
# Delhi wards nest in assembly constituencies; the Mumbai deposit carries no
# grouping above the ward, so Mumbai uses heteroskedasticity-robust errors.
cities <- list(
  Delhi = list(data = del, vcov = ~cluster_id, vcov_label = "clustered by assembly constituency"),
  Mumbai = list(data = mum, vcov = "hetero", vcov_label = "heteroskedasticity-robust")
)

# =============================================================================
# Quota dependence
# =============================================================================

transitions <- bind_rows(lapply(names(cities), function(city) {
  cities[[city]]$data |>
    count(prior = treat_2007, current = treat_2012) |>
    group_by(prior) |>
    mutate(share_of_prior = n / sum(n)) |>
    ungroup() |>
    mutate(city = city, .before = 1)
}))
write.csv(transitions, here("tabs/cities_transitions.csv"), row.names = FALSE)
print(transitions)

chi <- bind_rows(lapply(names(cities), function(city) {
  d <- cities[[city]]$data
  mat <- table(factor(d$treat_2007, 0:1), factor(d$treat_2012, 0:1))
  test <- chisq.test(mat)
  data.frame(City = city, statistic = unname(test$statistic), p.value = test$p.value, n = sum(mat))
}))
chi |>
  mutate(statistic = round(statistic, 2), p.value = signif(p.value, 2)) |>
  knitr::kable(
    format = "latex", booktabs = TRUE, col.names = c("City", "Chi-Squared", "P-Value", "Wards"),
    caption = "Chi-Squared Tests for Treatment Rotation (Delhi and Mumbai, 2007-2012)", label = "chi_sq_cities"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) |>
  save_kable(here("tabs/cities_chi_sq.tex"))

rotation <- lapply(cities, function(c) {
  feols(treat_2012 ~ treat_2007, data = c$data, vcov = c$vcov, ssc = MODEL_SSC)
})
aer_etable(rotation,
  file = here("tabs/cities_treatment_reg.tex"), dict = DICT_CITY, headers = list(names(cities)),
  notes = "Linear probability models of the 2012 women's quota on the 2007 one, all wards. Delhi standard errors clustered by assembly constituency; Mumbai heteroskedasticity-robust."
)

# =============================================================================
# Electoral balance: 2007 characteristics by the 2012 quota
# =============================================================================
# A balance test only in Delhi, where the 2012 seats were drawn afresh. In Mumbai
# the rotation sends 2007 women's seats to open in 2012, so 2007 women winners
# sit almost all in 2012 open wards by construction.

balance <- bind_rows(lapply(names(cities), function(city) {
  d <- cities[[city]]$data
  vars <- intersect(c("female_winner_2007", "dalit_2007", "sc_share_2001_2007"), names(d))
  bind_rows(lapply(vars, function(var) {
    x <- d[!is.na(d[[var]]), ]
    data.frame(
      city = city, variable = var, quota_mean = mean(x[[var]][x$treat_2012 == 1]),
      open_mean = mean(x[[var]][x$treat_2012 == 0]), p_value = t.test(x[[var]] ~ x$treat_2012)$p.value, n = nrow(x)
    )
  }))
}))
write.csv(balance, here("tabs/cities_balance_electoral.csv"), row.names = FALSE)
print(balance)

# =============================================================================
# Short-term models on wards open in 2012
# =============================================================================

open <- lapply(cities, function(c) c$data[c$data$treat_2012 == 0, ])
keep <- c("ward_id", "treat_2007", "female_winner_2012")
pooled <- bind_rows(
  mutate(open$Delhi[, keep], city = "Delhi"),
  mutate(open$Mumbai[, keep], city = "Mumbai")
)

# No constituency fixed effects for Delhi, unlike the panchayat models. Only one of
# its 68 constituencies holds both a woman winner and open wards that differ in
# their 2007 quota, so a within-constituency estimate would rest on that one
# constituency, and the wild bootstrap over it degenerates (it returned p = 0
# beside Fisher's 0.39). The two-city model's only fixed effect is the city.
specs <- list(
  list(id = "Delhi", data = open$Delhi, fe = FALSE, vcov = ~cluster_id),
  list(id = "Mumbai", data = open$Mumbai, fe = FALSE, vcov = "hetero"),
  list(id = "Both", data = pooled, fe = TRUE, vcov = "hetero")
)

models <- list()
rows <- list()
for (i in seq_along(specs)) {
  s <- specs[[i]]
  fe <- if (!s$fe) "" else if (s$id == "Both") "| city" else "| cluster_id"
  model <- feols(as.formula(paste("female_winner_2012 ~ treat_2007", fe)),
    data = s$data, vcov = s$vcov, ssc = MODEL_SSC, fixef.rm = "none"
  )
  stopifnot(nobs(model) == nrow(s$data))
  label <- paste(s$id, if (s$fe) "FE" else "No FE")
  models[[label]] <- model
  tab <- table(factor(s$data$treat_2007, 0:1), factor(s$data$female_winner_2012, 0:1))
  treated <- s$data$treat_2007 == 1
  rows[[i]] <- data.frame(
    city = s$id, fixed_effects = s$fe,
    variance = if (identical(s$vcov, "hetero")) "HC1" else "cluster",
    n = nobs(model), clusters = if (s$id == "Delhi") n_distinct(s$data$cluster_id) else NA_integer_,
    left_n = sum(treated), right_n = sum(!treated),
    left_women = sum(s$data$female_winner_2012[treated]), right_women = sum(s$data$female_winner_2012[!treated]),
    left_mean = mean(s$data$female_winner_2012[treated]), right_mean = mean(s$data$female_winner_2012[!treated]),
    estimate = coef(model)[["treat_2007"]], standard_error = se(model)[["treat_2007"]],
    p_value = pvalue(model)[["treat_2007"]],
    conf_low = confint(model)["treat_2007", 1], conf_high = confint(model)["treat_2007", 2],
    fisher_p = if (s$id == "Both") NA_real_ else fisher.test(tab)$p.value
  )
}
inference <- bind_rows(rows)
write.csv(inference, here("tabs/cities_inference.csv"), row.names = FALSE)
print(select(inference, city, fixed_effects, n, left_women, left_n, right_women, right_n, estimate, standard_error, fisher_p))

aer_etable(models,
  file = here("tabs/cities_short_term.tex"), dict = DICT_CITY, keep = "%treat_",
  headers = list(sub(" (No FE|FE)$", "", names(models)), sub(".* ", "", sub(" No FE$", " NoFE", names(models)))),
  notes = paste(
    "Outcome: the 2012 winner is a woman, among wards not reserved for women in 2012; winner sex as coded in the Goyal (Delhi) and Karekurve-Ramachandra and Lee (Mumbai) deposits.",
    "Delhi standard errors clustered by assembly constituency; Mumbai and the two-city model heteroskedasticity-robust. The Mumbai 2007 quota is a women's seat of any caste; Mumbai's 2012 open wards were chosen by rotation."
  )
)

# =============================================================================
# Placebo: does the 2012 quota predict a woman's 2007 win on wards open in 2007?
# =============================================================================

placebo <- lapply(cities, function(c) {
  d <- c$data[c$data$treat_2007 == 0 & !is.na(c$data$female_winner_2007), ]
  feols(female_winner_2007 ~ treat_2012, data = d, vcov = c$vcov, ssc = MODEL_SSC)
})
aer_etable(placebo,
  file = here("tabs/cities_placebo.tex"), dict = DICT_CITY, headers = list(names(cities)),
  notes = "Outcome: the 2007 winner is a woman, among wards open in 2007, on the 2012 quota. Delhi standard errors clustered by assembly constituency; Mumbai heteroskedasticity-robust. Mumbai's 2012 quota follows its rotation, so this contrast is not a placebo there in the same sense."
)

# =============================================================================
# Delhi candidates: do women stand more often in wards a quota has just left?
# =============================================================================

candidates <- list(
  `Women candidates` = feols(female_candidates_2012 ~ treat_2007, data = open$Delhi, vcov = ~cluster_id, ssc = MODEL_SSC),
  `Any woman` = feols(I(female_candidates_2012 > 0) ~ treat_2007, data = open$Delhi, vcov = ~cluster_id, ssc = MODEL_SSC),
  `All candidates` = feols(candidates_2012 ~ treat_2007, data = open$Delhi, vcov = ~cluster_id, ssc = MODEL_SSC)
)
aer_etable(candidates,
  file = here("tabs/cities_candidates.tex"), dict = DICT_CITY, headers = list(names(candidates)),
  notes = "Delhi wards not reserved for women in 2012. Candidate counts and gender from Goyal's deposit. Standard errors clustered by assembly constituency."
)
print(etable(candidates))

# =============================================================================
# Figure
# =============================================================================

plot_data <- inference |>
  mutate(
    label = paste0(city, if_else(fixed_effects, " FE", ""), "\n(", left_women, "/", left_n, " vs ", right_women, "/", right_n, ")"),
    low = conf_low, high = conf_high
  )
fig <- ggplot(plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, color = COLORS_PUB[1]) +
  geom_point(size = 2, color = COLORS_PUB[1]) +
  labs(x = "Effect of a 2007 quota on a woman winning the open 2012 ward", y = NULL) +
  theme_pub()
ggsave(here("figs/cities_short_term_coefplot.pdf"), fig,
  width = FIG_WIDTH_FULL, height = FIG_HEIGHT * 0.6, device = cairo_pdf
)
message("City outputs written to tabs/cities_* and figs/cities_short_term_coefplot.pdf")
