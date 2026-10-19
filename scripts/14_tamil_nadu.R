# 14_tamil_nadu.R
# Tamil Nadu village panchayat presidents: quota dependence between 2011 and the
# next ordinary election (2019, or 2021 in the nine districts carved out after
# 2011), and the short-term models.
# Tests: Among seats open in the later election, does a 2011 women's quota raise
#   the share of women winning?
# Outcome: the winner's recorded sex (printed on the SEC result sheets).
#
# Two things differ from the other states. The quota grew from one third (2011)
# to one half (2019 on), so far fewer later seats are open and the rotation is
# not a one-third rule repeated. And eight years separate the two elections,
# with the 2016 election cancelled.
#
# Caste-transition fixed effects compare seats within the same pair of earlier
# and later caste categories, as in 13_uttarakhand.R.
#
# Output: tabs/tamil_nadu_transitions.csv, tabs/tamil_nadu_chi_sq.tex,
#   tabs/tamil_nadu_treatment_reg.tex, tabs/tamil_nadu_balance_electoral.csv,
#   tabs/tamil_nadu_support.csv, tabs/tamil_nadu_short_term.tex,
#   tabs/tamil_nadu_placebo.tex, tabs/tamil_nadu_inference.csv,
#   figs/tamil_nadu_short_term_coefplot.pdf

library(arrow)
library(dplyr)
library(fixest)
library(fwildclusterboot)
library(ggplot2)
library(here)
library(kableExtra)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))
setFixest_nthreads(1)

message("=== Tamil Nadu village panchayat president ===")

tn <- as.data.frame(read_parquet(here("data/tn/tn_11_later.parquet")))

DICT_UK <- c(
  "treat_2011" = "$\\text{Quota}_{2011}$", "treat_later" = "$\\text{Quota}_{2019/21}$",
  cluster_id = "(District, Union)", dist_block_later = "(District, Union)",
  caste_transition = "Caste category transition", transition_id = "Caste category transition"
)
panels <- list(
  `2011-2019/21` = list(data = tn, prior = "treat_2011", current = "treat_later", year = "later", prior_year = "2011")
)

# =============================================================================
# Quota dependence: transition matrices, chi-squared and rotation regressions
# =============================================================================

transitions <- bind_rows(lapply(names(panels), function(name) {
  p <- panels[[name]]
  p$data |>
    count(prior = .data[[p$prior]], current = .data[[p$current]]) |>
    group_by(prior) |>
    mutate(share_of_prior = n / sum(n)) |>
    ungroup() |>
    mutate(period = name, .before = 1)
}))
write.csv(transitions, here("tabs/tamil_nadu_transitions.csv"), row.names = FALSE)
print(transitions)

chi <- bind_rows(lapply(names(panels), function(name) {
  p <- panels[[name]]
  mat <- table(factor(p$data[[p$prior]], 0:1), factor(p$data[[p$current]], 0:1))
  test <- chisq.test(mat)
  data.frame(Comparison = name, statistic = unname(test$statistic), p.value = test$p.value, n = sum(mat))
}))
chi |>
  mutate(statistic = round(statistic, 2), p.value = signif(p.value, 2)) |>
  knitr::kable(
    format = "latex", booktabs = TRUE,
    col.names = c("Comparison", "Chi-Squared", "P-Value", "GPs"),
    caption = "Chi-Squared Tests for Treatment Rotation (Tamil Nadu)", label = "chi_sq_uk"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) |>
  save_kable(here("tabs/tamil_nadu_chi_sq.tex"))

rotation <- lapply(panels, function(p) {
  feols(as.formula(paste(p$current, "~", p$prior)),
    data = p$data, vcov = as.formula(paste0("~dist_block_", p$year)), ssc = MODEL_SSC, fixef.rm = "none"
  )
})
aer_etable(rotation,
  file = here("tabs/tamil_nadu_treatment_reg.tex"), dict = DICT_UK,
  headers = list(names(panels)),
  notes = "Linear probability models of the later women's quota on the 2011 one, all linked seats. Standard errors clustered by district-union."
)

# =============================================================================
# Electoral balance: earlier-election characteristics by current quota
# =============================================================================

balance <- bind_rows(lapply(names(panels), function(name) {
  p <- panels[[name]]
  bind_rows(lapply(c("female_winner", "dalit"), function(v) {
    var <- paste0(v, "_", p$prior_year)
    d <- p$data[!is.na(p$data[[var]]), ]
    test <- t.test(d[[var]] ~ d[[p$current]])
    data.frame(
      period = name, variable = var, quota_mean = mean(d[[var]][d[[p$current]] == 1]),
      open_mean = mean(d[[var]][d[[p$current]] == 0]), p_value = test$p.value, n = nrow(d)
    )
  }))
}))
write.csv(balance, here("tabs/tamil_nadu_balance_electoral.csv"), row.names = FALSE)
print(balance)

# =============================================================================
# Support: open seats with and without a prior quota, by caste transition
# =============================================================================

open_seats <- lapply(panels, function(p) {
  d <- p$data[p$data[[p$current]] == 0 & !is.na(p$data[[paste0("female_winner_", p$year)]]), ]
  # fwildclusterboot reads fixed effects through a numeric model frame
  d$cluster_id <- as.integer(factor(d[[paste0("dist_block_", p$year)]]))
  d$transition_id <- as.integer(factor(d$caste_transition))
  d
})
support <- bind_rows(lapply(names(panels), function(name) {
  p <- panels[[name]]
  open_seats[[name]] |>
    group_by(caste_transition) |>
    summarise(
      open_seats = n(), prior_quota = sum(.data[[p$prior]] == 1), no_prior_quota = sum(.data[[p$prior]] == 0),
      .groups = "drop"
    ) |>
    mutate(period = name, .before = 1)
}))
write.csv(support, here("tabs/tamil_nadu_support.csv"), row.names = FALSE)
print(support, n = Inf)

# =============================================================================
# Short-term models, with cluster, HC1 and wild cluster bootstrap inference
# =============================================================================

SPECS <- list(
  `No FE` = NULL,
  `Block FE` = "cluster_id",
  `Block + caste transition FE` = c("cluster_id", "transition_id")
)
models <- list()
rows <- list()
test_index <- 0L
for (name in names(panels)) {
  p <- panels[[name]]
  data <- open_seats[[name]]
  outcome <- paste0("female_winner_", p$year)
  for (spec in names(SPECS)) {
    fe <- SPECS[[spec]]
    formula <- as.formula(paste(outcome, "~", p$prior, if (length(fe)) paste("|", paste(fe, collapse = " + ")) else ""))
    model <- feols(formula, data = data, vcov = ~cluster_id, ssc = MODEL_SSC, fixef.rm = "none")
    stopifnot(nobs(model) == nrow(data))
    models[[paste(name, spec)]] <- model
    test_index <- test_index + 1L
    # Tamil Nadu seeds start apart from the other states' sequences.
    seed <- 160000L + test_index
    boot <- NULL
    if (length(fe)) {
      set.seed(seed)
      dqrng::dqset.seed(seed)
      boot <- boottest(model,
        param = p$prior, B = 9999, clustid = "cluster_id", fe = "cluster_id",
        type = "rademacher", impose_null = TRUE, p_val_type = "two-tailed", conf_int = TRUE,
        sign_level = 0.05, engine = "R", sampling = "dqrng", nthreads = 1, maxiter = 100,
        ssc = boot_ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df = "conventional")
      )
      stopifnot(boot$N == nobs(model), abs(boot$point_estimate - coef(model)[[p$prior]]) < 1e-10)
    }
    treated <- data[[p$prior]] == 1
    for (variance in c("cluster", "HC1")) {
      fitted <- summary(model, vcov = if (variance == "cluster") ~cluster_id else "hetero", ssc = MODEL_SSC)
      rows[[length(rows) + 1L]] <- data.frame(
        state = "tn", period = name, specification = spec, outcome, contrast = p$prior, variance,
        n = nobs(model), clusters = n_distinct(data$cluster_id), left_n = sum(treated), right_n = sum(!treated),
        left_mean = mean(data[[outcome]][treated]), right_mean = mean(data[[outcome]][!treated]),
        estimate = coef(fitted)[[p$prior]], standard_error = se(fitted)[[p$prior]],
        p_value = pvalue(fitted)[[p$prior]],
        conf_low = confint(fitted)[p$prior, 1], conf_high = confint(fitted)[p$prior, 2],
        bootstrap_seed = if (is.null(boot)) NA_integer_ else seed,
        bootstrap_B = if (is.null(boot)) NA_integer_ else boot$boot_iter,
        bootstrap_p = if (is.null(boot)) NA_real_ else boot$p_val,
        bootstrap_low = if (is.null(boot)) NA_real_ else boot$conf_int[1],
        bootstrap_high = if (is.null(boot)) NA_real_ else boot$conf_int[2]
      )
    }
  }
}
inference <- bind_rows(rows)
write.csv(inference, here("tabs/tamil_nadu_inference.csv"), row.names = FALSE)
print(filter(inference, variance == "cluster") |>
  select(period, specification, n, left_n, right_n, left_mean, right_mean, estimate, standard_error, bootstrap_p))

aer_etable(models,
  file = here("tabs/tamil_nadu_short_term.tex"), dict = DICT_UK,
  keep = "%treat_",
  headers = list(sub(" (No FE|Block.*)$", "", names(models)), sub("^[0-9-]+ ", "", names(models))),
  notes = paste(
    "Outcome: the winner is a woman (recorded sex on the SEC result sheet), among seats not reserved for women in the later election (2019, or 2021 in the nine new districts).",
    "Caste transition: the GP's caste category in the earlier and later elections. Standard errors clustered by district-block."
  )
)

# =============================================================================
# Placebo: does the later quota predict a woman's earlier win on open seats? (as in 05d)
# =============================================================================

placebo <- lapply(panels, function(p) {
  outcome <- paste0("female_winner_", p$prior_year)
  d <- p$data[p$data[[p$prior]] == 0 & !is.na(p$data[[outcome]]), ]
  block <- paste0("dist_block_", p$year)
  feols(as.formula(paste0(outcome, " ~ ", p$current, " | ", block, " + caste_transition")),
    data = d, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"
  )
})
aer_etable(placebo,
  file = here("tabs/tamil_nadu_placebo.tex"),
  dict = DICT_UK,
  headers = list(names(panels)),
  notes = "Outcome: the 2011 winner is a woman (recorded sex), among seats open in 2011, on the later quota, with union and caste-transition fixed effects. Standard errors clustered by district-union."
)

# =============================================================================
# Figure
# =============================================================================

plot_data <- inference |>
  filter(variance == "cluster") |>
  mutate(
    label = paste0(period, ", ", specification, "\n(", left_n, " prior-quota vs ", right_n, " no-quota open seats)"),
    low = coalesce(bootstrap_low, conf_low), high = coalesce(bootstrap_high, conf_high)
  )
fig <- ggplot(plot_data, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, color = COLORS_PUB[1]) +
  geom_point(size = 2, color = COLORS_PUB[1]) +
  labs(x = "Effect of prior quota on the share of women winning the open seat", y = NULL) +
  theme_pub()
ggsave(here("figs/tamil_nadu_short_term_coefplot.pdf"), fig,
  width = FIG_WIDTH_FULL * 1.15, height = FIG_HEIGHT * 0.9, device = cairo_pdf
)
message("Tamil Nadu outputs written to tabs/tamil_nadu_* and figs/tamil_nadu_short_term_coefplot.pdf")
