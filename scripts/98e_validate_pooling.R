library(testthat)
source("scripts/05g_short_term_pooling.R")
loaded <- load_pool_panels()
joint <- pool_joint_scores(loaded$panels)
pooled <- read_csv("tabs/short_term_pooled.csv", show_col_types = FALSE)
stored <- read_csv("tabs/short_term_joint_covariance.csv", show_col_types = FALSE)

test_that("the joint covariance agrees with a saturated stacked regression", {
  stacked <- bind_rows(loaded$panels)
  stacked$equation_block <- interaction(stacked$comparison, stacked$block, drop = TRUE)
  fit <- feols(y ~ 0 + i(comparison, x) | equation_block, stacked,
    vcov = ~cluster_block, ssc = ssc(K.adj = FALSE, G.adj = FALSE), fixef.rm = "none")
  ids <- sub(":x$", "", sub("^comparison::", "", names(coef(fit))))
  order <- match(joint$estimates$comparison, ids)
  expect_equal(unname(coef(fit)[order]), joint$estimates$estimate, tolerance = 1e-10)
  for (level in c("block", "district")) {
    g <- vapply(loaded$panels, function(d) n_distinct(d[[paste0("cluster_", level)]]), numeric(1))
    n <- vapply(loaded$panels, nrow, integer(1))
    correction <- sqrt(g / (g - 1) * (n - 1) / (n - 2))
    covariance <- vcov(fit, vcov = as.formula(paste0("~cluster_", level)),
      ssc = ssc(K.adj = FALSE, G.adj = FALSE))[order, order] * outer(correction, correction)
    target <- if (level == "block") stored$covariance else stored$district_covariance
    expect_equal(as.vector(covariance), target, tolerance = 1e-12)
  }
  expect_true(min(eigen(joint$covariance, symmetric = TRUE)$values) > 0)
  expect_equal(as.vector(joint$covariance), stored$covariance, tolerance = 1e-12)
})

test_that("repeated identical comparisons do not falsely double information", {
  d <- loaded$panels[[1]]
  repeated <- pool_joint_scores(list(first = d, second = d))
  expect_equal(repeated$covariance[1, 2], repeated$covariance[1, 1], tolerance = 1e-12)
  average <- pool_summary(repeated, c(.5, .5), "Repeated comparison")
  expect_equal(average$standard_error, repeated$estimates$standard_error[1], tolerance = 1e-12)
})

test_that("geographic aliases preserve clusters and harmonize all linked GP paths", {
  for (state in c("raj", "up")) {
    period <- if (state == "raj") "05_20" else "05_21"
    full <- read_analysis_parquet(election_panel_path(state, paste0(state, "_", period, ".parquet")))
    maps <- loaded$geography |> filter(.data$state == .env$state, level == "block")
    years <- c(2005, 2010, 2015, if (state == "raj") 2020 else 2021)
    canonical <- lapply(years, function(year) {
      keys <- pool_geography(full, state, year, "block")
      maps$canonical[match(keys, maps$original)]
    })
    expect_false(anyNA(unlist(canonical)))
    for (j in 2:4) expect_identical(canonical[[1]], canonical[[j]])
  }
  alias <- pool_aliases(data.frame(old = c("old", "middle"), new = c("middle", "new")))
  expect_true(all(alias$canonical == "new"))
  expect_error(pool_aliases(data.frame(old = c("a", "a"), new = c("b", "c"))))
  expect_error(pool_aliases(data.frame(old = c("a", "b"), new = c("b", "a"))), "Cyclic")
})

test_that("weights, baseline, omission rules and intervals reproduce", {
  weights <- read_csv("tabs/short_term_pooled_weights.csv", show_col_types = FALSE)
  components <- read_csv("tabs/short_term_pool_components.csv", show_col_types = FALSE)
  for (label in unique(weights$specification)) {
    current <- weights |> filter(specification == label)
    w <- current$weight[match(components$comparison, current$comparison)]
    observed <- pooled |> filter(specification == label)
    expect_equal(sum(w), 1)
    expect_equal(observed$estimate, sum(w * components$estimate), tolerance = 1e-12)
    expect_equal(observed$control_mean, sum(w * components$control_mean), tolerance = 1e-12)
    covariance <- matrix(if (label == "District clustering") stored$district_covariance else stored$covariance, 6)
    standard_error <- sqrt(drop(t(w) %*% covariance %*% w))
    expect_equal(observed$standard_error, standard_error, tolerance = 1e-12)
    expect_equal(observed$conf_low, observed$estimate - qt(.975, observed$df) * standard_error)
    expect_equal(observed$conf_high, observed$estimate + qt(.975, observed$df) * standard_error)
    if (grepl("^Omit (raj|up)_", label)) {
      expect_equal(as.numeric(tapply(w, components$state, sum)), c(.5, .5))
      expect_equal(w[components$comparison == sub("^Omit ", "", label)], 0)
    }
  }
  set.seed(5102026)
  score <- rowMeans(joint$scores)
  draws <- replicate(9999, sum(sample(c(-1, 1), length(score), replace = TRUE) * score))
  bootstrap <- pooled |> filter(specification == "Joint wild multiplier bootstrap")
  expect_equal(bootstrap$conf_low, bootstrap$estimate - unname(quantile(draws, .975)))
  expect_equal(bootstrap$conf_high, bootstrap$estimate - unname(quantile(draws, .025)))
  expect_equal(bootstrap$standard_error, sd(draws))
})

test_that("the figure reports all six contrasts and the dependent pooled interval", {
  figure <- read_csv("tabs/short_term_pooled_figure.csv", show_col_types = FALSE)
  expect_equal(nrow(figure), 7L)
  expect_equal(sum(figure$pooled), 1L)
  expect_equal(figure$estimate[figure$pooled], pooled$estimate[1])
  expect_equal(figure$conf_low[figure$pooled], pooled$conf_low[1])
  expect_equal(figure$conf_high[figure$pooled], pooled$conf_high[1])
  expect_true(all(100 * figure$conf_low > -6))
  expect_true(all(100 * figure$conf_high < 6))
})

message("Pooled estimates, cross-election covariance, geography, weights and bootstrap validated.")
