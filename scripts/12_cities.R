# 12_cities.R
# Municipal councils: Delhi and Mumbai 2007-2012, Kolkata 2010-2015
# Tests: Does a women's quota in one election raise the chance that a woman wins
#   the ward when it is open in the next?
# Outcome: the winner is a woman. Delhi and Mumbai take sex as coded in their
#   replication deposits; Kolkata's is coded from the law and first names (see 02e),
#   and its uncertain names are bounded both ways.
# Output: tabs/cities_transitions.csv, tabs/cities_chi_sq.tex,
#   tabs/cities_treatment_reg.tex, tabs/cities_balance_electoral.csv,
#   tabs/cities_short_term.tex, tabs/cities_placebo.tex, tabs/cities_inference.csv,
#   tabs/cities_candidates.tex, figs/cities_short_term_coefplot.pdf
#
# Few women win open council seats in Delhi (6 of 134) and Mumbai (7 of 112), so
# those estimates are imprecise. Fisher's exact test is reported beside every
# regression because counts this small are where the normal approximation behind
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

message("=== Municipal councils ===")

# Each city in common names: quota_prior and quota_next are the two elections'
# women's quotas, woman_prior and woman_next whether a woman won.
common <- function(d, prior, nxt) {
  d$quota_prior <- d[[paste0("treat_", prior)]]
  d$quota_next <- d[[paste0("treat_", nxt)]]
  d$woman_prior <- d[[paste0("female_winner_", prior)]]
  d$woman_next <- d[[paste0("female_winner_", nxt)]]
  d
}
del <- common(as.data.frame(read_parquet(here("data/cities/del_07_12.parquet"))), 2007, 2012)
del$cluster_id <- as.integer(factor(del$ac_2012))
mum <- common(as.data.frame(read_parquet(here("data/cities/mum_07_12.parquet"))), 2007, 2012)
kol <- common(as.data.frame(read_parquet(here("data/cities/kol_10_15.parquet"))), 2010, 2015)

DICT_CITY <- c(
  "quota_prior" = "$\\text{Quota}_{t-1}$", "quota_next" = "$\\text{Quota}_{t}$",
  "cluster_id" = "Assembly constituency", "city" = "City"
)
# Delhi wards nest in assembly constituencies. The Mumbai and Kolkata sources carry
# no grouping above the ward, so those use heteroskedasticity-robust errors.
cities <- list(
  `Delhi 2007-2012` = list(data = del, vcov = ~cluster_id),
  `Mumbai 2007-2012` = list(data = mum, vcov = "hetero"),
  `Kolkata 2010-2015` = list(data = kol, vcov = "hetero")
)
VCOV_NOTE <- "Delhi standard errors clustered by assembly constituency; Mumbai, Kolkata and the pooled model heteroskedasticity-robust."

# =============================================================================
# Quota dependence
# =============================================================================

transitions <- bind_rows(lapply(names(cities), function(city) {
  cities[[city]]$data |>
    count(prior = quota_prior, current = quota_next) |>
    group_by(prior) |>
    mutate(share_of_prior = n / sum(n)) |>
    ungroup() |>
    mutate(city = city, .before = 1)
}))
write.csv(transitions, here("tabs/cities_transitions.csv"), row.names = FALSE)
print(transitions)

chi <- bind_rows(lapply(names(cities), function(city) {
  d <- cities[[city]]$data
  mat <- table(factor(d$quota_prior, 0:1), factor(d$quota_next, 0:1))
  test <- chisq.test(mat)
  data.frame(City = city, statistic = unname(test$statistic), p.value = test$p.value, n = sum(mat))
}))
chi |>
  mutate(statistic = round(statistic, 2), p.value = signif(p.value, 2)) |>
  knitr::kable(
    format = "latex", booktabs = TRUE, col.names = c("Council", "Chi-Squared", "P-Value", "Wards"),
    caption = "Chi-Squared Tests for Treatment Rotation (Municipal Councils)", label = "chi_sq_cities"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) |>
  save_kable(here("tabs/cities_chi_sq.tex"))

rotation <- lapply(cities, function(c) {
  feols(quota_next ~ quota_prior, data = c$data, vcov = c$vcov, ssc = MODEL_SSC)
})
aer_etable(rotation,
  file = here("tabs/cities_treatment_reg.tex"), dict = DICT_CITY, headers = list(names(cities)),
  notes = paste("Linear probability models of the later women's quota on the earlier one, all wards.", VCOV_NOTE)
)

# =============================================================================
# Electoral balance: earlier characteristics by the later quota
# =============================================================================
# Informative in Delhi and Kolkata, where the later quota does not follow a strict
# rotation. In Mumbai the rotation sends 2007 women's seats to open in 2012, so
# 2007 women winners sit almost all in 2012 open wards by construction.

balance <- bind_rows(lapply(names(cities), function(city) {
  d <- cities[[city]]$data
  d$dalit_prior <- d[[grep("^dalit_20", names(d), value = TRUE)[1]]]
  vars <- intersect(c("woman_prior", "dalit_prior", "sc_share_2001_2007", "electors_2010"), names(d))
  bind_rows(lapply(vars, function(var) {
    x <- d[!is.na(d[[var]]), ]
    data.frame(
      city = city, variable = var, quota_mean = mean(x[[var]][x$quota_next == 1]),
      open_mean = mean(x[[var]][x$quota_next == 0]), p_value = t.test(x[[var]] ~ x$quota_next)$p.value, n = nrow(x)
    )
  }))
}))
write.csv(balance, here("tabs/cities_balance_electoral.csv"), row.names = FALSE)
print(balance)

# =============================================================================
# Short-term models on wards open in the later election
# =============================================================================

open <- lapply(cities, function(c) c$data[c$data$quota_next == 0, ])
keep <- c("ward_id", "quota_prior", "woman_next")
pooled <- bind_rows(lapply(names(open), function(city) mutate(open[[city]][, keep], city = city)))

# Kolkata names that serve either sex are NA in the main sample and bounded here:
# all coded women, then all coded men.
bound <- function(value) {
  d <- open[["Kolkata 2010-2015"]]
  d$woman_next[is.na(d$woman_next)] <- value
  d
}

# No constituency fixed effects for Delhi, unlike the panchayat models. Only one of
# its 68 constituencies holds both a woman winner and open wards that differ in
# their 2007 quota, so a within-constituency estimate would rest on that one
# constituency, and the wild bootstrap over it degenerates (it returned p = 0
# beside Fisher's 0.39). The pooled model's only fixed effect is the council.
specs <- list(
  list(id = "Delhi 2007-2012", sample = "main", data = open[["Delhi 2007-2012"]], vcov = ~cluster_id),
  list(id = "Mumbai 2007-2012", sample = "main", data = open[["Mumbai 2007-2012"]], vcov = "hetero"),
  list(id = "Kolkata 2010-2015", sample = "main", data = open[["Kolkata 2010-2015"]], vcov = "hetero"),
  list(id = "Kolkata 2010-2015", sample = "uncertain coded women", data = bound(1), vcov = "hetero"),
  list(id = "Kolkata 2010-2015", sample = "uncertain coded men", data = bound(0), vcov = "hetero"),
  list(id = "All three", sample = "main", data = pooled, vcov = "hetero", fe = "city")
)

models <- list()
rows <- list()
for (i in seq_along(specs)) {
  s <- specs[[i]]
  data <- s$data[!is.na(s$data$woman_next), ]
  formula <- as.formula(paste("woman_next ~ quota_prior", if (is.null(s$fe)) "" else paste("|", s$fe)))
  model <- feols(formula, data = data, vcov = s$vcov, ssc = MODEL_SSC, fixef.rm = "none")
  stopifnot(nobs(model) == nrow(data))
  if (s$sample == "main") models[[s$id]] <- model
  tab <- table(factor(data$quota_prior, 0:1), factor(data$woman_next, 0:1))
  treated <- data$quota_prior == 1
  rows[[i]] <- data.frame(
    city = s$id, sample = s$sample, fixed_effects = !is.null(s$fe),
    variance = if (identical(s$vcov, "hetero")) "HC1" else "cluster",
    n = nobs(model), uncertain_dropped = sum(is.na(s$data$woman_next)),
    clusters = if (startsWith(s$id, "Delhi")) n_distinct(data$cluster_id) else NA_integer_,
    left_n = sum(treated), right_n = sum(!treated),
    left_women = sum(data$woman_next[treated]), right_women = sum(data$woman_next[!treated]),
    left_mean = mean(data$woman_next[treated]), right_mean = mean(data$woman_next[!treated]),
    estimate = coef(model)[["quota_prior"]], standard_error = se(model)[["quota_prior"]],
    p_value = pvalue(model)[["quota_prior"]],
    conf_low = confint(model)["quota_prior", 1], conf_high = confint(model)["quota_prior", 2],
    fisher_p = if (is.null(s$fe)) fisher.test(tab)$p.value else NA_real_
  )
}
inference <- bind_rows(rows)
write.csv(inference, here("tabs/cities_inference.csv"), row.names = FALSE)
print(select(inference, city, sample, n, left_women, left_n, right_women, right_n, estimate, standard_error, fisher_p))

aer_etable(models,
  file = here("tabs/cities_short_term.tex"), dict = DICT_CITY, keep = "%quota_",
  headers = list(names(models)),
  notes = paste(
    "Outcome: a woman wins the ward, among wards not reserved for women in the later election.",
    "Winner sex as coded in the Goyal (Delhi) and Karekurve-Ramachandra and Lee (Mumbai) deposits; Kolkata's coded from the women's-seat rule and first names, with names used for either sex excluded (bounds in tabs/cities_inference.csv).",
    VCOV_NOTE, "The Mumbai 2007 quota is a women's seat of any caste; Mumbai's 2012 open wards were chosen by rotation."
  )
)

# =============================================================================
# Placebo: does the later quota predict a woman's earlier win on wards open then?
# =============================================================================

placebo <- lapply(cities, function(c) {
  d <- c$data[c$data$quota_prior == 0 & !is.na(c$data$woman_prior), ]
  feols(woman_prior ~ quota_next, data = d, vcov = c$vcov, ssc = MODEL_SSC)
})
aer_etable(placebo,
  file = here("tabs/cities_placebo.tex"), dict = DICT_CITY, headers = list(names(cities)),
  notes = paste(
    "Outcome: a woman won the ward in the earlier election, among wards open then, on the later quota.", VCOV_NOTE,
    "Mumbai's 2012 quota follows its rotation, so this contrast is not a placebo there in the same sense."
  )
)

# =============================================================================
# Delhi candidates: do women stand more often in wards a quota has just left?
# =============================================================================

d_open <- open[["Delhi 2007-2012"]]
candidates <- list(
  `Women candidates` = feols(female_candidates_2012 ~ quota_prior, data = d_open, vcov = ~cluster_id, ssc = MODEL_SSC),
  `Any woman` = feols(I(female_candidates_2012 > 0) ~ quota_prior, data = d_open, vcov = ~cluster_id, ssc = MODEL_SSC),
  `All candidates` = feols(candidates_2012 ~ quota_prior, data = d_open, vcov = ~cluster_id, ssc = MODEL_SSC)
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
  mutate(label = paste0(
    city, if_else(sample == "main", "", paste0(" (", sample, ")")),
    "\n(", left_women, "/", left_n, " vs ", right_women, "/", right_n, ")"
  ))
fig <- ggplot(plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), orientation = "y", width = 0.2, color = COLORS_PUB[1]) +
  geom_point(size = 2, color = COLORS_PUB[1]) +
  labs(x = "Effect of an earlier quota on a woman winning the open ward", y = NULL) +
  theme_pub()
ggsave(here("figs/cities_short_term_coefplot.pdf"), fig,
  width = FIG_WIDTH_FULL, height = FIG_HEIGHT * 0.8, device = cairo_pdf
)
message("City outputs written to tabs/cities_* and figs/cities_short_term_coefplot.pdf")
