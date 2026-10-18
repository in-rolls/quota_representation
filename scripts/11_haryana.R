# 11_haryana.R
# Haryana sarpanch extension: quota dependence across elections and the short-term models
# Tests: Does a women's quota in 2010 raise the expected share of women winning the
#   seat when it is open in 2016?
# Outcome: the name-based probability that the winner is a woman (see 02d). Effects
#   are on an expected share, estimated from names, not on recorded winner sex.
# Output: tabs/haryana_transitions.csv, tabs/haryana_chi_sq.tex,
#   tabs/haryana_treatment_reg.tex, tabs/haryana_balance_electoral.csv,
#   tabs/haryana_restriction_support.csv, tabs/haryana_short_term.tex,
#   tabs/haryana_placebo.tex, tabs/haryana_inference.csv,
#   figs/haryana_short_term_coefplot.pdf

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

message("=== Haryana sarpanch ===")

hr_10_16 <- as.data.frame(read_parquet(here("data/hr/hr_10_16.parquet")))
hr_16_22 <- as.data.frame(read_parquet(here("data/hr/hr_16_22.parquet")))

DICT_HR <- c(
  "treat_2010" = "$\\text{Quota}_{2010}$", "treat_2016" = "$\\text{Quota}_{2016}$",
  "dist_block_2016" = "(District, Block)", "dist_block_2022" = "(District, Block)"
)
panels <- list(
  `2010-2016` = list(data = hr_10_16, prior = "treat_2010", current = "treat_2016", year = "2016"),
  `2016-2022` = list(data = hr_16_22, prior = "treat_2016", current = "treat_2022", year = "2022")
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
write.csv(transitions, here("tabs/haryana_transitions.csv"), row.names = FALSE)
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
    caption = "Chi-Squared Tests for Treatment Rotation (Haryana)", label = "chi_sq_hr"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) |>
  save_kable(here("tabs/haryana_chi_sq.tex"))

rotation <- lapply(panels, function(p) {
  feols(as.formula(paste(p$current, "~", p$prior)),
    data = p$data, vcov = as.formula(paste0("~dist_block_", p$year)), ssc = MODEL_SSC, fixef.rm = "none"
  )
})
aer_etable(rotation,
  file = here("tabs/haryana_treatment_reg.tex"), dict = DICT_HR,
  headers = list(names(panels)),
  notes = "Linear probability models of the current women's quota on the previous one, all linked GPs. Standard errors clustered by district-block."
)

# =============================================================================
# Electoral balance: earlier-election characteristics by current quota
# =============================================================================

balance <- bind_rows(lapply(names(panels), function(name) {
  p <- panels[[name]]
  prior_year <- sub("treat_", "", p$prior)
  bind_rows(lapply(c("female_winner", "dalit"), function(v) {
    var <- paste0(v, "_", prior_year)
    d <- p$data[!is.na(p$data[[var]]), ]
    test <- t.test(d[[var]] ~ d[[p$current]])
    data.frame(
      period = name, variable = var, quota_mean = mean(d[[var]][d[[p$current]] == 1]),
      open_mean = mean(d[[var]][d[[p$current]] == 0]), p_value = test$p.value, n = nrow(d)
    )
  }))
}))
write.csv(balance, here("tabs/haryana_balance_electoral.csv"), row.names = FALSE)
print(balance)

# =============================================================================
# Districts where consecutive quotas look independent (as in 05b)
# =============================================================================

eligible_districts <- function(data, first, second, district) {
  p <- vapply(split(data, data[[district]]), function(d) {
    tab <- table(factor(d[[first]], levels = 0:1), factor(d[[second]], levels = 0:1))
    if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
      return(NA_real_)
    }
    suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
  }, numeric(1))
  list(selected = names(p)[!is.na(p) & p > 0.05], tested = sum(!is.na(p)), districts = length(p))
}

with_sample <- function(panel, data, sample) {
  panel$data <- data
  panel$sample <- sample
  panel
}

# Effects are estimated for 2010-2016 only. Under the 2020 amendment no woman can
# win an open 2022 seat, so 2016-2022 is described above and not estimated.
estimated <- "2010-2016"
samples <- list()
support <- list()
for (name in names(panels)) {
  p <- panels[[name]]
  open <- p$data[p$data[[p$current]] == 0 & !is.na(p$data[[paste0("female_winner_", p$year)]]), ]
  if (name %in% estimated) samples[[paste(name, "full")]] <- with_sample(p, open, "full")
  e <- eligible_districts(p$data, p$prior, p$current, paste0("district_", p$year))
  restricted <- open[open[[paste0("district_", p$year)]] %in% e$selected, ]
  support[[name]] <- data.frame(
    period = name, districts = e$districts, tested = e$tested,
    retained = length(e$selected), open_n = nrow(open), restricted_n = nrow(restricted)
  )
  if (name %in% estimated && nrow(restricted) > 0 && length(unique(restricted[[p$prior]])) == 2) {
    samples[[paste(name, "restricted")]] <- with_sample(p, restricted, "restricted")
  }
}
write.csv(bind_rows(support), here("tabs/haryana_restriction_support.csv"), row.names = FALSE)
print(bind_rows(support))

# =============================================================================
# Short-term models, with cluster, HC1 and wild cluster bootstrap inference
# =============================================================================

models <- list()
rows <- list()
test_index <- 0L
for (id in names(samples)) {
  s <- samples[[id]]
  outcome <- paste0("female_winner_", s$year)
  block <- paste0("dist_block_", s$year)
  data <- s$data
  # fwildclusterboot extracts cluster labels through a numeric model frame.
  data$cluster_id <- as.integer(factor(data[[block]]))
  for (use_fe in c(FALSE, TRUE)) {
    formula <- as.formula(paste(outcome, "~", s$prior, if (use_fe) "| cluster_id" else ""))
    model <- feols(formula, data = data, vcov = ~cluster_id, ssc = MODEL_SSC, fixef.rm = "none")
    stopifnot(nobs(model) == nrow(data))
    models[[paste(id, if (use_fe) "FE" else "No FE")]] <- model
    test_index <- test_index + 1L
    # Haryana seeds start apart from the Rajasthan/UP and Bihar sequences.
    seed <- 140000L + test_index
    boot <- NULL
    if (use_fe) {
      set.seed(seed)
      dqrng::dqset.seed(seed)
      boot <- boottest(model,
        param = s$prior, B = 9999, clustid = "cluster_id", fe = "cluster_id",
        type = "rademacher", impose_null = TRUE, p_val_type = "two-tailed", conf_int = TRUE,
        sign_level = 0.05, engine = "R", sampling = "dqrng", nthreads = 1, maxiter = 100,
        ssc = boot_ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df = "conventional")
      )
      stopifnot(boot$N == nobs(model), abs(boot$point_estimate - coef(model)[[s$prior]]) < 1e-10)
    }
    treated <- data[[s$prior]] == 1
    for (variance in c("cluster", "HC1")) {
      fitted <- summary(model, vcov = if (variance == "cluster") ~cluster_id else "hetero", ssc = MODEL_SSC)
      rows[[length(rows) + 1L]] <- data.frame(
        state = "hr", period = sub(" .*", "", id), sample = s$sample, fixed_effects = use_fe,
        outcome, contrast = s$prior, variance, n = nobs(model),
        clusters = n_distinct(data$cluster_id), left_n = sum(treated), right_n = sum(!treated),
        left_mean = mean(data[[outcome]][treated]), right_mean = mean(data[[outcome]][!treated]),
        estimate = coef(fitted)[[s$prior]], standard_error = se(fitted)[[s$prior]],
        p_value = pvalue(fitted)[[s$prior]],
        conf_low = confint(fitted)[s$prior, 1], conf_high = confint(fitted)[s$prior, 2],
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
write.csv(inference, here("tabs/haryana_inference.csv"), row.names = FALSE)
print(filter(inference, variance == "cluster") |>
  select(period, sample, fixed_effects, n, left_n, left_mean, right_mean, estimate, standard_error, bootstrap_p))

aer_etable(models,
  file = here("tabs/haryana_short_term.tex"), dict = c(DICT_HR, cluster_id = "(District, Block)"),
  keep = "%treat_",
  headers = list(sub(" (No FE|FE)$", "", names(models)), sub(".* ", "", sub(" No FE$", " NoFE", names(models)))),
  notes = paste(
    "Outcome: the name-based probability that the 2016 sarpanch is a woman (an expected share, inferred from the printed name; Haryana records no winner sex), among seats not reserved for women in 2016.",
    "2010 blocks whose printed categories conflict with the commission's reservation table are excluded. Effects are estimated for 2010-2016 only; under the 2020 amendment no woman can win an open 2022 seat.",
    "Restricted samples keep districts whose descriptive independence test for consecutive quotas has p > 0.05; non-rejection does not establish random assignment. Standard errors clustered by district-block."
  )
)

# =============================================================================
# Placebo: does the later quota predict a woman's earlier win on open seats? (as in 05d)
# =============================================================================

placebo <- lapply(panels[estimated], function(p) {
  prior_year <- sub("treat_", "", p$prior)
  d <- p$data[p$data[[p$prior]] == 0 & !is.na(p$data[[paste0("female_winner_", prior_year)]]), ]
  block <- paste0("dist_block_", p$year)
  feols(as.formula(paste0("female_winner_", prior_year, " ~ ", p$current, " | ", block)),
    data = d, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"
  )
})
aer_etable(placebo,
  file = here("tabs/haryana_placebo.tex"),
  dict = DICT_HR, headers = list(estimated),
  notes = "Outcome: the name-based probability that the earlier sarpanch is a woman, among seats open in that election, on the later quota. Standard errors clustered by district-block."
)

# =============================================================================
# Figure
# =============================================================================

plot_data <- inference |>
  filter(variance == "cluster", fixed_effects) |>
  mutate(
    label = paste0(period, " ", sample, "\n(", left_n, " GPs with prior quota)"),
    low = coalesce(bootstrap_low, conf_low), high = coalesce(bootstrap_high, conf_high)
  )
fig <- ggplot(plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, color = COLORS_PUB[1]) +
  geom_point(size = 2, color = COLORS_PUB[1]) +
  labs(x = "Effect of prior quota on the expected share of women winning the open seat", y = NULL) +
  theme_pub()
ggsave(here("figs/haryana_short_term_coefplot.pdf"), fig,
  width = FIG_WIDTH_FULL, height = FIG_HEIGHT * 0.6, device = cairo_pdf
)
message("Haryana outputs written to tabs/haryana_* and figs/haryana_short_term_coefplot.pdf")
