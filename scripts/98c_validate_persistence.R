library(testthat)
source("scripts/11_persistence_horizons.R")
estimates <- read_csv(here("tabs/persistence_horizons.csv"), show_col_types = FALSE)
changes <- read_csv(here("tabs/persistence_horizon_changes.csv"), show_col_types = FALSE)
joint <- read_csv(here("tabs/persistence_horizon_joint_tests.csv"), show_col_types = FALSE)
analysis <- read_parquet(here("data/persistence_horizon_analysis.parquet"))

manual_fe <- function(d) {
  x <- d$exposed - ave(d$exposed, d$block)
  y <- d$y - ave(d$y, d$block)
  beta <- sum(x * y) / sum(x^2)
  residual <- y - beta * x
  scores <- rowsum(x * residual, d$block)
  g <- nrow(scores)
  variance <- g / (g - 1) * (nrow(d) - 1) / (nrow(d) - 2) * sum(scores^2) / sum(x^2)^2
  c(estimate = beta, se = sqrt(variance))
}

test_that("follow-up eligibility and outcomes reproduce from source panels", {
  expect_equal(nrow(estimates), 34L)
  expect_equal(anyDuplicated(analysis[c("state", "profile", "sample", "horizon", "id")]), 0L)
  for (state in c("raj", "up")) {
    period <- if (state == "raj") "05_20" else "05_21"
    raw <- read_analysis_parquet(election_panel_path(state, paste0(state, "_", period, ".parquet")))
    id <- as.character(raw[[if (state == "raj") "source_id_2005" else "gp_id"]])
    years <- c(2005, 2010, 2015, if (state == "raj") 2020 else 2021)
    for (i in seq_len(nrow(persistence_specs))) {
      spec <- persistence_specs[i, ]
      history <- as.integer(strsplit(spec$history, "")[[1]])
      treated <- Reduce(`&`, lapply(1:4, function(j) raw[[paste0("treat_", years[j])]] == history[j]))
      controls <- Reduce(`&`, lapply(years, function(y) raw[[paste0("treat_", y)]] == 0))
      observed <- complete.cases(raw[paste0("female_winner_", years[spec$first_index:4])])
      rows <- analysis |> filter(.data$state == .env$state, profile == spec$profile, sample == "fixed")
      expect_setequal(unique(rows$id), id[(treated | controls) & observed])
      expect_true(all(table(rows$id) == 5 - spec$first_index))
      for (year in unique(rows$year)) {
        current <- rows |> filter(.data$year == .env$year)
        idx <- match(current$id, id)
        expect_equal(current$y, raw[[paste0("female_winner_", year)]][idx])
        expect_true(all(raw[[paste0("treat_", year)]][idx] == 0))
      }
    }
    primary <- analysis |> filter(.data$state == .env$state, profile == "quota2005", sample == "fixed", horizon == 1)
    expect_equal(nrow(primary), if (state == "raj") 329L else 6825L)
  }
})

test_that("all level estimates and cluster uncertainty reproduce independently", {
  for (i in seq_len(nrow(estimates))) {
    r <- estimates[i, ]
    d <- analysis |> filter(state == r$state, profile == r$profile, sample == r$sample, horizon == r$horizon)
    computed <- manual_fe(d)
    expect_equal(r$estimate, unname(computed["estimate"]), tolerance = 1e-10)
    expect_equal(r$standard_error, unname(computed["se"]), tolerance = 1e-10)
    expect_equal(r$n, nrow(d))
    expect_equal(r$treated_n, sum(d$exposed))
    expect_equal(r$control_n, sum(d$exposed == 0))
    expect_equal(r$control_mean, mean(d$y[d$exposed == 0]))
    expect_equal(r$treated_mean, mean(d$y[d$exposed == 1]))
    expect_equal(r$df, n_distinct(d$block) - 1)
  }
})

test_that("change tests retain pairing and account for covariance across outcomes", {
  expect_equal(nrow(changes), 8L)
  for (i in seq_len(nrow(changes))) {
    r <- changes[i, ]
    data <- analysis |> filter(state == r$state, profile == r$profile, sample == "fixed")
    first <- data |> filter(horizon == 1) |> select(id, y_first = y)
    difference <- data |> filter(horizon == r$horizon) |>
      left_join(first, by = "id", relationship = "one-to-one") |> mutate(y = y - y_first)
    computed <- manual_fe(difference)
    expect_equal(r$estimate, unname(computed["estimate"]), tolerance = 1e-10)
    expect_equal(r$standard_error, unname(computed["se"]), tolerance = 1e-10)
    levels <- estimates |> filter(state == r$state, profile == r$profile, sample == "fixed")
    expect_equal(r$estimate, levels$estimate[levels$horizon == r$horizon] - levels$estimate[levels$horizon == 1])
  }
  expect_equal(changes$bootstrap_p_holm, p.adjust(changes$bootstrap_p, method = "holm"))
})

test_that("stacked joint tests reproduce from block-by-election score covariance", {
  for (i in seq_len(nrow(joint))) {
    r <- joint[i, ]
    d <- analysis |> filter(state == r$state, profile == r$profile, sample == "fixed")
    h <- max(d$horizon)
    design <- cbind(d$exposed, sapply(2:h, function(j) d$exposed * (d$horizon == j)))
    fe <- interaction(d$block, d$horizon, drop = TRUE)
    x <- apply(design, 2, function(z) z - ave(z, fe))
    y <- d$y - ave(d$y, fe)
    bread <- solve(crossprod(x))
    beta <- bread %*% crossprod(x, y)
    scores <- rowsum(x * as.numeric(y - x %*% beta), d$block)
    g <- nrow(scores)
    vc <- bread %*% crossprod(scores) %*% bread * g / (g - 1) * (nrow(d) - 1) / (nrow(d) - h - 1)
    idx <- 2:h
    statistic <- as.numeric(t(beta[idx]) %*% solve(vc[idx, idx, drop = FALSE], beta[idx])) / length(idx)
    expect_equal(r$statistic, statistic, tolerance = 1e-8)
    expect_equal(r$p_value, pf(statistic, length(idx), g - 1, lower.tail = FALSE), tolerance = 1e-8)
  }
})

test_that("bootstrap settings and one complete seeded reproduction match", {
  fixed <- estimates |> filter(sample == "fixed")
  expect_equal(nrow(fixed), 14L)
  expect_true(all(is.finite(fixed$bootstrap_low) & is.finite(fixed$bootstrap_high)))
  expect_true(all(fixed$bootstrap_low < fixed$bootstrap_high))
  expect_true(all(fixed$bootstrap_p >= 0 & fixed$bootstrap_p <= 1))
  row <- fixed |> filter(state == "raj", profile == "quota2005", horizon == 1)
  d <- analysis |> filter(state == "raj", profile == "quota2005", sample == "fixed", horizon == 1)
  reproduced <- fit_persistence(d, "y", row$bootstrap_seed)
  expect_equal(reproduced$bootstrap_p, row$bootstrap_p)
  expect_equal(reproduced$bootstrap_low, row$bootstrap_low)
  expect_equal(reproduced$bootstrap_high, row$bootstrap_high)
})
message("Validated temporal persistence samples, source outcomes, within-block estimates, covariance, and bootstrap reproduction.")
