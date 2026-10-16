# 10_bihar.R
# Bihar Mukhiya extension: quota dependence across elections and the short-term models
# Tests: Does a women's quota in 2011 (2016) raise the chance that a woman wins the seat
#   when it is open in 2016 (2021)?
# Output: tabs/bihar_transitions.csv, tabs/bihar_chi_sq.tex, tabs/bihar_treatment_reg.tex,
#   tabs/bihar_balance_electoral.csv, tabs/bihar_restriction_support.csv,
#   tabs/bihar_short_term.tex, tabs/bihar_placebo.tex, tabs/bihar_inference.csv,
#   figs/bihar_short_term_coefplot.pdf

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

message("=== Bihar Mukhiya ===")

bh_11_16_all <- as.data.frame(read_parquet(here("data/bh/bh_11_16.parquet")))
bh_16_21 <- as.data.frame(read_parquet(here("data/bh/bh_16_21.parquet")))
# Primary: 2011 reservation as printed. Inclusive: bare category labels read as not
# reserved for women where the district's report prints women's labels.
bh_11_16 <- filter(bh_11_16_all, !is.na(treat_2011))
bh_11_16_inclusive <- mutate(bh_11_16_all, treat_2011 = treat_2011_inclusive)

DICT_BH <- c(
  "treat_2011" = "$\\text{Quota}_{2011}$", "treat_2016" = "$\\text{Quota}_{2016}$",
  "dist_block_2016" = "(District, Block)", "dist_block_2021" = "(District, Block)"
)
panels <- list(
  `2011-2016` = list(data = bh_11_16, prior = "treat_2011", current = "treat_2016", year = "2016"),
  `2016-2021` = list(data = bh_16_21, prior = "treat_2016", current = "treat_2021", year = "2021")
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
write.csv(transitions, here("tabs/bihar_transitions.csv"), row.names = FALSE)
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
    caption = "Chi-Squared Tests for Treatment Rotation (Bihar)", label = "chi_sq_bh"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) |>
  save_kable(here("tabs/bihar_chi_sq.tex"))

rotation <- lapply(panels, function(p) {
  feols(as.formula(paste(p$current, "~", p$prior)),
    data = p$data, vcov = as.formula(paste0("~dist_block_", p$year)), ssc = MODEL_SSC, fixef.rm = "none"
  )
})
aer_etable(rotation,
  file = here("tabs/bihar_treatment_reg.tex"), dict = DICT_BH,
  headers = list(names(panels)),
  notes = "Linear probability models of the current women's quota on the previous one, all linked GPs. Standard errors clustered by district-block."
)

# =============================================================================
# Electoral balance: earlier-election characteristics by current quota
# =============================================================================

balance <- bind_rows(lapply(names(panels), function(name) {
  p <- panels[[name]]
  prior_year <- sub("treat_", "", p$prior)
  bind_rows(lapply(c("female_winner", "obc", "dalit"), function(v) {
    var <- paste0(v, "_", prior_year)
    d <- p$data[!is.na(p$data[[var]]), ]
    test <- t.test(d[[var]] ~ d[[p$current]])
    data.frame(
      period = name, variable = var, quota_mean = mean(d[[var]][d[[p$current]] == 1]),
      open_mean = mean(d[[var]][d[[p$current]] == 0]), p_value = test$p.value, n = nrow(d)
    )
  }))
}))
write.csv(balance, here("tabs/bihar_balance_electoral.csv"), row.names = FALSE)
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

samples <- list()
support <- list()
for (name in names(panels)) {
  p <- panels[[name]]
  open <- p$data[p$data[[p$current]] == 0 & !is.na(p$data[[paste0("female_winner_", p$year)]]), ]
  samples[[paste(name, "full")]] <- with_sample(p, open, "full")
  e <- eligible_districts(p$data, p$prior, p$current, paste0("district_", p$year))
  restricted <- open[open[[paste0("district_", p$year)]] %in% e$selected, ]
  support[[name]] <- data.frame(
    period = name, districts = e$districts, tested = e$tested,
    retained = length(e$selected), open_n = nrow(open), restricted_n = nrow(restricted)
  )
  if (nrow(restricted) > 0 && length(unique(restricted[[p$prior]])) == 2) {
    samples[[paste(name, "restricted")]] <- with_sample(p, restricted, "restricted")
  }
}
inclusive_open <- filter(bh_11_16_inclusive, treat_2016 == 0, !is.na(female_winner_2016))
samples[["2011-2016 inclusive"]] <- with_sample(panels[["2011-2016"]], inclusive_open, "inclusive_2011_labels")
write.csv(bind_rows(support), here("tabs/bihar_restriction_support.csv"), row.names = FALSE)
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
    # Bihar seeds start apart from the Rajasthan/UP sequence in 09a, so adding Bihar
    # leaves every earlier seed unchanged.
    seed <- 130000L + test_index
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
        state = "bh", period = sub(" .*", "", id), sample = s$sample, fixed_effects = use_fe,
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
write.csv(inference, here("tabs/bihar_inference.csv"), row.names = FALSE)
print(filter(inference, variance == "cluster") |>
  select(period, sample, fixed_effects, n, left_n, left_mean, right_mean, estimate, standard_error, bootstrap_p))

aer_etable(models,
  file = here("tabs/bihar_short_term.tex"), dict = c(DICT_BH, cluster_id = "(District, Block)"),
  keep = "%treat_",
  headers = list(sub(" (No FE|FE)$", "", names(models)), sub(".* ", "", sub(" No FE$", " NoFE", names(models)))),
  notes = paste(
    "Outcome: woman elected Mukhiya in the later election, among seats not reserved for women in that election.",
    "Restricted samples keep districts whose descriptive independence test for consecutive quotas has p > 0.05; non-rejection does not establish random assignment.",
    "Inclusive reads bare 2011 category labels as not reserved for women. Standard errors clustered by district-block."
  )
)

# =============================================================================
# Placebo: does the later quota predict a woman's earlier win on open seats? (as in 05d)
# =============================================================================

placebo <- lapply(panels, function(p) {
  prior_year <- sub("treat_", "", p$prior)
  d <- p$data[p$data[[p$prior]] == 0 & !is.na(p$data[[paste0("female_winner_", prior_year)]]), ]
  block <- paste0("dist_block_", p$year)
  feols(as.formula(paste0("female_winner_", prior_year, " ~ ", p$current, " | ", block)),
    data = d, vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none"
  )
})
aer_etable(placebo,
  file = here("tabs/bihar_placebo.tex"),
  dict = c(DICT_BH, "treat_2021" = "$\\text{Quota}_{2021}$"), headers = list(names(panels)),
  notes = "Outcome: woman elected in the earlier election, among seats open in that election, on the later quota. Standard errors clustered by district-block."
)

# =============================================================================
# Figure
# =============================================================================

plot_data <- inference |>
  filter(variance == "cluster", fixed_effects) |>
  mutate(
    label = paste0(period, " ", sub("_2011_labels", " 2011 labels", sample), "\n(", left_n, " GPs with prior quota)"),
    low = coalesce(bootstrap_low, conf_low), high = coalesce(bootstrap_high, conf_high)
  )
fig <- ggplot(plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, color = COLORS_PUB[1]) +
  geom_point(size = 2, color = COLORS_PUB[1]) +
  labs(x = "Effect of prior quota on a woman winning the open seat", y = NULL) +
  theme_pub()
ggsave(here("figs/bihar_short_term_coefplot.pdf"), fig,
  width = FIG_WIDTH_FULL, height = FIG_HEIGHT * 0.6, device = cairo_pdf
)
message("Bihar outputs written to tabs/bihar_* and figs/bihar_short_term_coefplot.pdf")
