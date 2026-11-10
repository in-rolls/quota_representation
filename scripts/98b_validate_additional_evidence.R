suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(testthat)
})

source("scripts/10_additional_evidence.R")
urban <- read_csv("tabs/additional_urban_persistence.csv", show_col_types = FALSE)
delhi <- appendix_delhi()
chandigarh <- appendix_chandigarh()
chandigarh_later <- appendix_chandigarh_later()
bengaluru <- appendix_bengaluru()
mumbai <- haven::read_dta(additional_source_path(paste0(
  "../local_elections/data/maharashtra/raw/mumbai/bhavnani_2009/",
  "Bhavnani APSR 2009 replication data.dta"
)))

test_that("Mumbai preserves the published comparison and its excluded wards", {
  expect_equal(nrow(mumbai), 227L)
  included <- mumbai[!is.na(mumbai$controltreat), ]
  expect_equal(nrow(included), 118L)
  expect_equal(as.numeric(table(included$controltreat)), c(81, 37))
  expect_equal(as.numeric(tapply(included$genderofwinner, included$controltreat, sum)), c(3, 8))
  expect_true(all(included$reservedforwomen == 0))
  expect_equal(sum(mumbai$reservedforwomen == 0 & is.na(mumbai$controltreat)), 33)
})

test_that("Delhi retains one-to-one ward identities and source-coded outcomes", {
  expect_equal(nrow(delhi), 272L)
  expect_equal(anyDuplicated(delhi$ward), 0L)
  expect_setequal(delhi$ward, as.character(1:272))
  expect_equal(sum(delhi$current_quota == 0), 134)
  opened <- delhi[delhi$current_quota == 0, ]
  expect_equal(opened$ward[is.na(opened$female)], "115")
  expect_equal(delhi$female[delhi$ward == 111], 1)
  expect_equal(delhi$female[delhi$ward == 254], 0)
  untouched <- delhi[!delhi$ward %in% c(111, 115, 254), ]
  expect_equal(untouched$female, as.numeric(untouched$gender == "F"))
  expect_equal(delhi$prior_quota, as.numeric(delhi$reservation_2007 %in% c("W", "SCW")))
  expect_equal(as.numeric(table(opened$prior_quota)), c(90, 44))
  expect_equal(as.numeric(tapply(opened$female, opened$prior_quota, sum, na.rm = TRUE)), c(2, 3))
  raw <- readxl::read_excel(
    additional_source_path("../local_elections/data/delhi/interim/qualification_audit/sources/goyal_candidates.xlsx"),
    col_types = "text"
  )
  w <- raw[raw$election == "2012" & !is.na(raw$eci_pos) & raw$eci_pos == "1", ]
  expect_equal(anyDuplicated(w$commonname), 0L)
  expect_identical(delhi$gender, w$gender[match(delhi$ward, as.integer(w$commonname))])
})

test_that("Chandigarh preserves the documented frame and one observed woman", {
  expect_equal(nrow(chandigarh), 26L)
  expect_setequal(chandigarh$ward, as.character(1:26))
  expect_identical(chandigarh$is_eligible, chandigarh$current_quota == 0L)
  opened <- chandigarh |> filter(is_eligible)
  expect_equal(nrow(opened), 17L)
  expect_false(anyNA(opened$female))
  expect_equal(as.numeric(table(opened$prior_quota)), c(8, 9))
  expect_equal(as.numeric(tapply(opened$female, opened$prior_quota, sum)), c(0, 1))
  expect_equal(sum(opened$prior_sc == 1), 3L)
  expect_equal(sum(opened$current_sc == 1), 3L)
})

test_that("urban risk differences, score intervals and regularized variances rederive", {
  samples <- list(
    "Chandigarh 2011--2016" = data.frame(
      q = chandigarh_later$prior_quota[chandigarh_later$is_eligible],
      y = chandigarh_later$female[chandigarh_later$is_eligible]
    ),
    "Bengaluru 2010--2015" = data.frame(
      q = bengaluru$prior_quota[bengaluru$is_eligible], y = bengaluru$female[bengaluru$is_eligible]
    ),
    "Chandigarh 2006--2011" = data.frame(
      q = chandigarh$prior_quota[chandigarh$is_eligible],
      y = chandigarh$female[chandigarh$is_eligible]
    ),
    "Mumbai 1997--2002" = data.frame(q = mumbai$controltreat, y = mumbai$genderofwinner),
    "Delhi 2007--2012" = data.frame(
      q = delhi$prior_quota[delhi$current_quota == 0],
      y = delhi$female[delhi$current_quota == 0]
    )
  )
  for (setting in names(samples)) {
    d <- na.omit(samples[[setting]])
    y0 <- d$y[d$q == 0]
    y1 <- d$y[d$q == 1]
    estimate <- mean(y1) - mean(y0)
    variance <- function(y) {
      p <- (sum(y) + .5) / (length(y) + 1)
      p * (1 - p) / length(y)
    }
    se <- sqrt(variance(y1) + variance(y0))
    row <- urban |> filter(.data$setting == .env$setting, specification == "Unadjusted")
    expect_equal(nrow(row), 1L)
    expect_equal(row$estimate, estimate, tolerance = 1e-12)
    expect_equal(row$standard_error, se, tolerance = 1e-12)
    wilson <- function(y) {
      z <- qnorm(.975)
      n <- length(y)
      p <- mean(y)
      (p + z^2 / (2 * n) + c(-1, 1) * z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
    }
    ci1 <- wilson(y1)
    ci0 <- wilson(y0)
    expected <- estimate + c(-sqrt((mean(y1) - ci1[1])^2 + (ci0[2] - mean(y0))^2),
      sqrt((mean(y0) - ci0[1])^2 + (ci1[2] - mean(y1))^2))
    expect_equal(c(row$conf_low, row$conf_high), expected, tolerance = 1e-12)
  }
})

test_that("new city-cycle outcomes preserve zero events and source uncertainty", {
  d <- chandigarh_later |> filter(is_eligible)
  expect_equal(as.numeric(table(d$prior_quota)), c(11, 6))
  expect_equal(sum(d$female), 0)
  row <- urban |> filter(setting == "Chandigarh 2011--2016", specification == "Unadjusted")
  expect_equal(row$estimate, 0)
  expect_gt(row$standard_error, .1)
  expect_lt(row$conf_low, -.25)
  expect_gt(row$conf_high, .38)
  d <- bengaluru |> filter(is_eligible)
  expect_equal(as.numeric(table(d$prior_quota)), c(76, 25))
  expect_true(all(is.na(d$female[d$outcome_conflict])))
  expect_equal(d$ward[is.na(d$female)], "112")
  expect_equal(sum(d$female[d$prior_quota == 0], na.rm = TRUE), 3)
  expect_equal(sum(d$female[d$prior_quota == 1], na.rm = TRUE), 0)
  expect_equal(bengaluru$prior_quota[bengaluru$ward == "5"], 0)
  expect_equal(bengaluru$current_quota[bengaluru$ward == "4"], 0)
})

test_that("Delhi sensitivities reproduce with explicit design-matrix HC2", {
  opened <- subset(delhi, current_quota == 0 & !is.na(female))
  for (spec in c("Prior SC and corporation", "Unchanged SC category")) {
    d <- if (spec == "Unchanged SC category") subset(opened, prior_sc == current_sc) else opened
    formula <- if (spec == "Unchanged SC category") {
      female ~ prior_quota
    } else {
      female ~ prior_quota + prior_sc + factor(corporation)
    }
    m <- lm(formula, d)
    x <- model.matrix(m)
    bread <- solve(crossprod(x))
    weighted <- x * as.numeric(residuals(m) / sqrt(1 - hatvalues(m)))
    covariance <- bread %*% crossprod(weighted) %*% bread
    row <- urban |> filter(specification == spec)
    expect_equal(row$estimate, unname(coef(m)["prior_quota"]), tolerance = 1e-12)
    expect_equal(row$standard_error, sqrt(covariance["prior_quota", "prior_quota"]), tolerance = 1e-12)
    expect_equal(row$n, nrow(d))
  }
})

test_that("Chandigarh adjustment retains support in each caste transition", {
  d <- chandigarh |> filter(is_eligible)
  expect_true(all(table(interaction(d$prior_sc, d$current_sc, drop = TRUE), d$prior_quota) > 0))
  x <- model.matrix(~ prior_quota + prior_sc + current_sc, d)
  expect_equal(qr(x)$rank, ncol(x))
  bread <- solve(crossprod(x))
  beta <- drop(bread %*% crossprod(x, d$female))
  residual <- d$female - drop(x %*% beta)
  leverage <- rowSums((x %*% bread) * x)
  meat <- crossprod(x * as.numeric(residual / sqrt(1 - leverage)))
  row <- urban |> filter(setting == "Chandigarh 2006--2011", specification == "Prior and current caste")
  expect_equal(row$estimate, unname(beta[2]), tolerance = 1e-12)
  expect_equal(row$standard_error, sqrt((bread %*% meat %*% bread)[2, 2]), tolerance = 1e-12)
})

test_that("West Bengal saved estimates and missing-outcome bounds rederive", {
  f <- read_csv("wb/data/selection_frame.csv", show_col_types = FALSE)
  f <- f |> filter(history_eligible, q03 == 0)
  observed <- f |> filter(!is.na(audited_outcome))
  expected <- read_csv("wb/tabs/selection_adjustment_comparison.csv", show_col_types = FALSE)
  forms <- list(
    "No adjustment" = audited_outcome ~ q98,
    "1998 caste strata and block" = audited_outcome ~ q98 + sc98 + st98 + factor(block),
    "Also condition on 2003 caste quotas" = audited_outcome ~ q98 + sc98 + st98 + sc03 + st03 + factor(block)
  )
  for (name in names(forms)) {
    m <- lm(forms[[name]], data = observed)
    row <- expected |> filter(model == name, se_type == "HC2")
    expect_equal(unname(coef(m)["q98"]), row$estimate, tolerance = 1e-10)
    expect_equal(sqrt(sandwich::vcovHC(m, type = "HC2")["q98", "q98"]), row$std.error, tolerance = 1e-10)
  }
  missing <- which(is.na(f$audited_outcome))
  completions <- expand.grid(rep(list(0:1), length(missing)))
  differences <- apply(completions, 1, function(values) {
    y <- f$audited_outcome
    y[missing] <- values
    mean(y[f$q98 == 1]) - mean(y[f$q98 == 0])
  })
  b <- read_csv("wb/tabs/selection_missing_outcome_bounds.csv", show_col_types = FALSE)
  expect_equal(range(differences), c(b$lower_difference, b$upper_difference), tolerance = 1e-12)
  expect_equal(nrow(observed), 94L)
})

test_that("comparison plot selects six main estimates without counting sensitivities twice", {
  plotted <- read_csv("tabs/additional_persistence_comparison.csv", show_col_types = FALSE)
  expect_equal(nrow(plotted), 12L)
  expect_equal(anyDuplicated(plotted[c("setting", "period")]), 0L)
  expect_equal(sum(plotted$evidence == "Main paper: election winners"), 6L)
  expect_equal(sum(plotted$evidence == "Urban election winners"), 5L)
  expect_equal(sum(plotted$evidence == "Two-term quota regime"), 1L)
  expect_true(all(plotted$conf_low < plotted$estimate & plotted$conf_high > plotted$estimate))
  source <- read_csv("tabs/model_inference.csv", show_col_types = FALSE) |>
    filter(sample == "full", fixed_effects, variance == "cluster", grepl("^treat_", contrast))
  existing <- plotted |> filter(evidence == "Main paper: election winners")
  expect_equal(existing$estimate, source$estimate)
  expect_equal(existing$conf_low, source$bootstrap_low)
  expect_equal(existing$conf_high, source$bootstrap_high)
})


test_that("Bihar preserves known outcomes and admits only documented recoveries", {
  d <- read_csv("data/additional_evidence/bihar_2011_2016.csv", show_col_types = FALSE)
  expect_equal(nrow(d), 3342L)
  expect_equal(anyDuplicated(d$record_id_2011), 0L)
  expect_equal(d$female[!is.na(d$source_female)], d$source_female[!is.na(d$source_female)])
  eligible <- d |> filter(prior_quota %in% 0:1, current_quota == 0)
  observed <- eligible |> filter(!is.na(female))
  expect_equal(nrow(eligible), 1310L)
  expect_equal(nrow(observed), 1295L)
  expect_equal(as.numeric(table(observed$prior_quota)), c(527, 768))
  expect_equal(as.numeric(tapply(observed$female, observed$prior_quota, sum)), c(72, 128))
  expect_equal(sum(eligible$outcome_source == "Reviewed vote dominance"), 5L)
  expect_equal(sum(eligible$outcome_source == "Official winner list"), 1L)
  expect_equal(sum(is.na(eligible$female)), 15L)
  expect_false(any(observed$tied %in% TRUE))
  primary <- read_csv("tabs/additional_bihar_persistence.csv", show_col_types = FALSE) |>
    filter(specification == "Unadjusted")
  expect_equal(primary$estimate, 128 / 768 - 72 / 527, tolerance = 1e-12)
  bounds <- read_csv("tabs/additional_bihar_bounds.csv", show_col_types = FALSE)
  expect_equal(bounds$low, 128 / 776 - 79 / 534, tolerance = 1e-12)
  expect_equal(bounds$high, 136 / 776 - 72 / 534, tolerance = 1e-12)
})

test_that("Bihar cluster covariance rederives from cluster score sums", {
  d <- read_csv("data/additional_evidence/bihar_2011_2016.csv", show_col_types = FALSE) |>
    filter(prior_quota %in% 0:1, current_quota == 0, !is.na(female))
  results <- read_csv("tabs/additional_bihar_persistence.csv", show_col_types = FALSE)
  for (spec in c("Unadjusted", "Block and prior caste", "Name score = 1", "Candidate results only")) {
    sample <- switch(spec,
      "Name score = 1" = filter(d, name_score == 1),
      "Candidate results only" = filter(d, outcome_source == "Candidate result"),
      d
    )
    formula <- if (spec == "Block and prior caste") {
      female ~ prior_quota + factor(cluster) + factor(prior_caste)
    } else {
      female ~ prior_quota
    }
    model <- lm(formula, sample)
    x <- model.matrix(model)
    x <- x[, model$qr$pivot[seq_len(model$rank)], drop = FALSE]
    bread <- solve(crossprod(x))
    scores <- rowsum(x * residuals(model), sample$cluster, reorder = FALSE)
    g <- nrow(scores)
    n <- nrow(x)
    covariance <- bread %*% crossprod(scores) %*% bread * g / (g - 1) * (n - 1) / (n - model$rank)
    se <- sqrt(covariance["prior_quota", "prior_quota"])
    row <- results |> filter(specification == spec)
    expect_equal(row$standard_error, se, tolerance = 1e-10)
    expect_equal(row$estimate, unname(coef(model)["prior_quota"]), tolerance = 1e-10)
    expect_equal(c(row$conf_low, row$conf_high), row$estimate + c(-1, 1) * qt(.975, g - 1) * se,
      tolerance = 1e-10
    )
    expect_equal(row$clusters, g)
  }
})

test_that("published West Bengal results remain distinct from the survey reanalysis", {
  published <- read_csv("data/additional_evidence/wb_published_estimates.csv", show_col_types = FALSE)
  plotted <- read_csv("tabs/additional_wb_comparison.csv", show_col_types = FALSE)
  expect_equal(published$estimate, c(.027, .003, .056, .076))
  expect_equal(published$se, c(.023, .026, .031, .041))
  expect_equal(published$n, c(870, 875, 875, 875))
  expect_equal(nrow(plotted), 5L)
  expect_equal(plotted$estimate[1:4], published$estimate)
  expect_equal(plotted$conf_low[1:4], published$estimate - qnorm(.975) * published$se)
  expect_equal(plotted$conf_high[1:4], published$estimate + qnorm(.975) * published$se)
})

test_that("the synthesis overview retains all distinct primary contrasts", {
  all <- read_csv("tabs/additional_all_estimates.csv", show_col_types = FALSE)
  expect_equal(nrow(all), 26L)
  expect_equal(anyDuplicated(all$label), 0L)
  expect_equal(as.numeric(table(all$panel)), c(12, 14))
  urban <- all |> filter(setting == "UP urban")
  urban_source <- read_csv("tabs/up_urban_estimates.csv", show_col_types = FALSE) |>
    filter(specification == "All linked heads")
  urban_interval <- read_csv("tabs/up_urban_bootstrap.csv", show_col_types = FALSE)
  expect_equal(nrow(urban), 1L)
  expect_equal(urban$estimate, urban_source$estimate)
  expect_equal(urban$n, urban_source$n)
  expect_equal(urban$conf_low, urban_interval$conf_low)
  expect_equal(urban$conf_high, urban_interval$conf_high)
  expect_false("UP urban" %in% read_csv("tabs/additional_meta_inputs.csv", show_col_types = FALSE)$setting)
  main <- read_csv("tabs/model_inference.csv", show_col_types = FALSE) |>
    filter(sample == "full", fixed_effects, variance == "cluster")
  overview_main <- all |> filter(setting %in% c("Rajasthan", "Uttar Pradesh"))
  for (state in c("raj", "up")) {
    setting <- if (state == "raj") "Rajasthan" else "Uttar Pradesh"
    reference <- main |>
      filter(.data$state == .env$state) |>
      arrange(estimate)
    shown <- overview_main |>
      filter(.data$setting == .env$setting) |>
      arrange(estimate)
    expect_equal(shown$estimate, reference$estimate)
    expect_equal(shown$conf_low, reference$bootstrap_low)
    expect_equal(shown$conf_high, reference$bootstrap_high)
  }
  expect_true(all(all$conf_low <= all$estimate & all$conf_high >= all$estimate))
})

test_that("meta-analysis uses settings rather than overlapping election rows", {
  inputs <- read_csv("tabs/additional_meta_inputs.csv", show_col_types = FALSE)
  settings <- read_csv("tabs/additional_meta_settings.csv", show_col_types = FALSE)
  expect_equal(nrow(inputs), 13L)
  expect_equal(nrow(settings), 7L)
  expect_setequal(settings$setting, c("Rajasthan", "Uttar Pradesh", "Mumbai", "Delhi", "Chandigarh", "Bengaluru", "West Bengal"))
  expect_equal(anyDuplicated(inputs[c("setting", "period")]), 0L)
  for (setting in settings$setting) {
    d <- inputs |> filter(.data$setting == .env$setting)
    observed <- settings |> filter(.data$setting == .env$setting)
    expect_equal(observed$estimate, mean(d$estimate))
    expect_equal(observed$variance, mean(d$standard_error)^2)
    for (rho in c(0, .5, 1, if (nrow(d) > 1) -1 / (nrow(d) - 1) else 0)) {
      covariance <- outer(d$standard_error, d$standard_error) * rho
      diag(covariance) <- d$standard_error^2
      expect_true(min(eigen(covariance, symmetric = TRUE)$values) > -1e-12)
      expect_lte(sum(covariance) / nrow(d)^2, observed$variance + 1e-12)
    }
  }
  published <- read_csv("data/additional_evidence/wb_published_estimates.csv", show_col_types = FALSE)
  expect_equal(inputs$estimate[inputs$setting == "West Bengal"], published$estimate[1:2])
  expect_equal(inputs$standard_error[inputs$setting == "West Bengal"], published$se[1:2])
  expect_equal(sum(settings$weight), 1)
})

test_that("REML and modified Knapp-Hartung rederive without the meta-analysis package", {
  inputs <- read_csv("tabs/additional_meta_inputs.csv", show_col_types = FALSE)
  results <- read_csv("tabs/additional_meta_sensitivity.csv", show_col_types = FALSE)
  gaps <- read_csv("tabs/additional_rural_urban.csv", show_col_types = FALSE)
  expect_identical(gaps$specification, results$specification)
  bihar <- read_csv("tabs/additional_bihar_persistence.csv", show_col_types = FALSE) |>
    filter(specification == "Unadjusted") |>
    transmute(setting = "Bihar", period = "2011--2016", estimate, standard_error)
  for (i in seq_len(nrow(results))) {
    label <- results$specification[i]
    d <- inputs
    if (startsWith(label, "Urban variance")) {
      arms <- read_csv("tabs/additional_urban_persistence_counts.csv", show_col_types = FALSE)
      corrected <- arms |> mutate(p = (women + 1) / (n + 2), variance = p * (1 - p) / (n + 1)) |>
        group_by(setting) |> summarise(se = sqrt(sum(variance)), .groups = "drop")
      for (j in seq_len(nrow(d))) {
        k <- match(paste(d$setting[j], d$period[j]), corrected$setting)
        if (!is.na(k)) d$standard_error[j] <- corrected$se[k]
      }
    }
    if (startsWith(label, "Omit ")) d <- filter(d, setting != sub("Omit ", "", label))
    if (startsWith(label, "Include Bihar")) d <- bind_rows(d, bihar)
    if (startsWith(label, "Earliest")) {
      d <- d |>
        group_by(setting) |>
        slice_head(n = 1) |>
        ungroup()
    }
    if (startsWith(label, "Latest")) {
      d <- d |>
        group_by(setting) |>
        slice_tail(n = 1) |>
        ungroup()
    }
    means <- vapply(split(d, d$setting), function(x) mean(x$estimate), numeric(1))
    variances <- vapply(split(d, d$setting), function(x) {
      if (label == "Estimated covariance in Rajasthan and UP" && x$setting[1] %in% c("Rajasthan", "Uttar Pradesh")) {
        joint <- read_csv("tabs/short_term_joint_covariance.csv", show_col_types = FALSE)
        state <- if (x$setting[1] == "Rajasthan") "raj" else "up"
        selected <- startsWith(joint$first, paste0(state, "_")) & startsWith(joint$second, paste0(state, "_"))
        return(sum(joint$covariance[selected]) / nrow(x)^2)
      }
      rho <- if (label == "Within-setting correlation = 0") 0 else if (label == "Within-setting correlation = 0.5") .5 else 1
      if (startsWith(label, "Minimum feasible")) rho <- if (nrow(x) > 1) -1 / (nrow(x) - 1) else 0
      covariance <- outer(x$standard_error, x$standard_error) * rho
      diag(covariance) <- x$standard_error^2
      sum(covariance) / nrow(x)^2
    }, numeric(1))
    loss <- function(tau2) {
      weights <- 1 / (variances + tau2)
      mu <- weighted.mean(means, weights)
      (sum(log(variances + tau2)) + log(sum(weights)) + sum(weights * (means - mu)^2)) / 2
    }
    opt <- optimize(loss, c(0, 1), tol = 1e-12)
    tau2 <- if (loss(0) <= opt$objective) 0 else opt$minimum
    weights <- 1 / (variances + tau2)
    estimate <- weighted.mean(means, weights)
    multiplier <- max(1, sum(weights * (means - estimate)^2) / (length(means) - 1))
    se <- sqrt(multiplier / sum(weights))
    expect_equal(results$settings[i], length(means))
    expect_equal(results$tau_squared[i], tau2, tolerance = 2e-6, scale = 1)
    expect_equal(results$estimate[i], estimate, tolerance = 2e-6, scale = 1)
    expect_equal(results$standard_error[i], se, tolerance = 2e-6, scale = 1)
    expect_equal(c(results$conf_low[i], results$conf_high[i]),
      estimate + c(-1, 1) * qt(.975, length(means) - 1) * se,
      tolerance = 2e-6, scale = 1
    )
    rural <- as.integer(!names(means) %in% c("Mumbai", "Delhi", "Chandigarh", "Bengaluru"))
    x <- cbind(intercept = 1, rural = rural)
    moderator_loss <- function(tau2) {
      w <- 1 / (variances + tau2)
      information <- crossprod(x, w * x)
      beta <- solve(information, crossprod(x, w * means))
      residual <- means - drop(x %*% beta)
      (sum(log(variances + tau2)) + as.numeric(determinant(information, logarithm = TRUE)$modulus) +
        sum(w * residual^2)) / 2
    }
    opt <- optimize(moderator_loss, c(0, 1), tol = 1e-12)
    tau2 <- if (moderator_loss(0) <= opt$objective) 0 else opt$minimum
    w <- 1 / (variances + tau2)
    covariance <- solve(crossprod(x, w * x))
    beta <- drop(covariance %*% crossprod(x, w * means))
    df <- length(means) - 2
    multiplier <- max(1, sum(w * (means - drop(x %*% beta))^2) / df)
    se <- sqrt(covariance[2, 2] * multiplier)
    expect_equal(gaps$urban_settings[i], sum(rural == 0))
    expect_equal(gaps$rural_settings[i], sum(rural == 1))
    expect_equal(gaps$df[i], df)
    expect_equal(gaps$estimate[i], unname(beta[2]), tolerance = 1e-7, scale = 1)
    expect_equal(gaps$urban_mean[i], unname(beta[1]), tolerance = 1e-7, scale = 1)
    expect_equal(gaps$rural_mean[i], sum(beta), tolerance = 1e-7, scale = 1)
    expect_equal(gaps$tau_squared[i], tau2, tolerance = 1e-7, scale = 1)
    expect_equal(gaps$standard_error[i], se, tolerance = 1e-7, scale = 1)
    expect_equal(c(gaps$conf_low[i], gaps$conf_high[i]),
      unname(beta[2]) + c(-1, 1) * qt(.975, df) * se,
      tolerance = 1e-7, scale = 1
    )
    expect_equal(gaps$p_value[i], unname(2 * pt(-abs(beta[2] / se), df)), tolerance = 1e-7, scale = 1)
  }
})

test_that("harmonized sample counts preserve missing outcomes", {
  samples <- read_csv("tabs/additional_samples.csv", show_col_types = FALSE)
  expect_equal(nrow(samples), 16L)
  urban <- samples |> filter(setting == "UP urban 2012--2017") |> arrange(prior_quota)
  source <- read_csv("tabs/up_urban_estimates.csv", show_col_types = FALSE) |>
    filter(specification == "All linked heads")
  expect_equal(urban$observed, c(source$nonquota_n, source$quota_n))
  expect_equal(urban$women, c(source$nonquota_women, source$quota_women))
  expect_true(all(samples$eligible >= samples$observed & samples$observed >= samples$women))
  expect_equal(samples$share, samples$women / samples$observed)
  missing <- samples |>
    group_by(setting) |>
    summarise(missing = sum(eligible - observed), .groups = "drop")
  expect_equal(missing$missing[match(c("Mumbai 1997--2002", "Delhi 2007--2012", "Bihar 2011--2016", "Birbhum 1998--2006"), missing$setting)], c(0, 1, 15, 5))
})

test_that("joint missing-outcome analysis includes all binary completions", {
  completed <- read_csv("tabs/additional_urban_joint_missing_outcomes.csv", show_col_types = FALSE)
  expect_equal(nrow(completed), 4L)
  expect_equal(anyDuplicated(completed$completion), 0L)
  expect_true(all(completed$urban_settings == 4L & completed$rural_settings == 3L))
  expect_equal(range(completed$estimate), c(-.0421205854, -.0080245276), tolerance = 1e-7)
  expect_equal(range(completed$p_value), c(.2654122173, .8435744490), tolerance = 1e-7)
  expect_true(all(completed$conf_low < 0 & completed$conf_high > 0))
  observed <- read_csv("tabs/additional_urban_missing_outcomes.csv", show_col_types = FALSE) |>
    filter(setting == "Bengaluru 2010--2015")
  expect_equal(range(observed$estimate), c(-3 / 76, 1 / 25 - 3 / 76), tolerance = 1e-12)
})

message("Additional persistence estimates, synthesis, sample exclusions, covariance, and bounds validated.")
