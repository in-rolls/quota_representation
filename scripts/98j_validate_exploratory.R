library(tidyverse)
library(arrow)
library(testthat)
library(fixest)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

source(here("scripts/00_pooled.R"))
test_that("Bihar panels follow one-to-one pooled seat links", {
  links <- pooled_links("Bihar")
  ends <- list(`11_16` = c(2011, 2016, "row_id_2011"), `16_21` = c(2016, 2021, "gp_id"))
  for (panel in names(ends)) {
    e <- ends[[panel]]
    l <- filter(links, from_year == as.integer(e[1]), to_year == as.integer(e[2]))
    p <- read_parquet(here("data/bh", paste0("bh_", panel, ".parquet")))
    expect_false(anyDuplicated(p$gp_id) > 0)
    expect_true(all(p[[e[3]]] %in% l$from_row_id))
    expect_lte(nrow(p), nrow(l))
    later <- paste0("treat_20", strsplit(panel, "_")[[1]][2])
    expect_true(all(p[[later]] %in% 0:1))
    # An unstated 2011 women's label stays missing; only the inclusive recode fills it.
    if (panel == "11_16") {
      expect_true(all(p$treat_2011 %in% c(0L, 1L, NA)))
      expect_true(all(p$treat_2011_inclusive %in% 0:1))
      expect_true(all(is.na(p$treat_2011) | p$treat_2011 == p$treat_2011_inclusive))
    } else {
      expect_true(all(p$treat_2016 %in% 0:1))
    }
  }
})

test_that("Haryana panels drop conflicting 2010 blocks and keep the inferred outcome a probability", {
  p <- read_parquet(here("data/hr/hr_10_16.parquet"))
  expect_false(anyDuplicated(p$gp_id) > 0)
  expect_false(any(p$conflict_2010))
  expect_true(all(p$treat_2010 %in% 0:1) && all(p$treat_2016 %in% 0:1))
  f <- p$female_winner_2016[!is.na(p$female_winner_2016)]
  expect_true(all(f >= 0 & f <= 1))
  # Women's seats should be won by women: the name model must say so on average.
  expect_gt(mean(p$female_winner_2016[p$treat_2016 == 1], na.rm = TRUE), 0.85)
  links <- filter(pooled_links("Haryana"), from_year == 2010, to_year == 2016)
  expect_true(all(p$gp_id %in% links$to_row_id))
})

haryana <- read_csv(here("tabs/haryana_inference.csv"), show_col_types = FALSE)
test_that("Haryana short-term estimate reproduces after independent within-block OLS", {
  d <- read_parquet(here("data/hr/hr_10_16.parquet")) |>
    filter(treat_2016 == 0) |>
    drop_na(female_winner_2016, dist_block_2016)
  within_x <- d$treat_2010 - ave(d$treat_2010, d$dist_block_2016)
  within_y <- d$female_winner_2016 - ave(d$female_winner_2016, d$dist_block_2016)
  independent <- lm(within_y ~ within_x - 1)
  covariance <- sandwich::vcovCL(independent, cluster = d$dist_block_2016, type = "HC0")
  covariance <- covariance * (nrow(d) - 1) / (nrow(d) - 2)
  row <- haryana |> filter(period == "2010-2016", sample == "full", fixed_effects, variance == "cluster")
  expect_equal(row$estimate, unname(coef(independent)), tolerance = 1e-9)
  expect_equal(row$standard_error, sqrt(drop(covariance)), tolerance = 1e-8)
  expect_equal(row$n, nrow(d))
})

test_that("Uttarakhand panels link one seat to one seat and the name model reads women's seats", {
  for (panel in c("08_14", "14_19")) {
    p <- read_parquet(here(paste0("data/uk/uk_", panel, ".parquet")))
    years <- c(`08_14` = "2014", `14_19` = "2019")[[panel]]
    expect_false(anyDuplicated(p$gp_id) > 0)
    f <- p[[paste0("female_winner_", years)]]
    expect_true(all(f[!is.na(f)] >= 0 & f[!is.na(f)] <= 1))
    expect_gt(mean(f[p[[paste0("treat_", years)]] == 1], na.rm = TRUE), 0.9)
  }
})

uttarakhand <- read_csv(here("tabs/uttarakhand_inference.csv"), show_col_types = FALSE)
test_that("Uttarakhand estimates reproduce: raw difference and independent within-block OLS", {
  d <- read_parquet(here("data/uk/uk_08_14.parquet")) |>
    filter(treat_2014 == 0) |>
    drop_na(female_winner_2014)
  raw <- mean(d$female_winner_2014[d$treat_2008 == 1]) - mean(d$female_winner_2014[d$treat_2008 == 0])
  rows <- uttarakhand |> filter(period == "2008-2014", variance == "cluster")
  expect_equal(rows$estimate[rows$specification == "No FE"], raw, tolerance = 1e-9)
  within_x <- d$treat_2008 - ave(d$treat_2008, d$dist_block_2014)
  within_y <- d$female_winner_2014 - ave(d$female_winner_2014, d$dist_block_2014)
  expect_equal(rows$estimate[rows$specification == "Block FE"], unname(coef(lm(within_y ~ within_x - 1))), tolerance = 1e-9)
  expect_true(all(rows$n == nrow(d)))
})

test_that("Tamil Nadu panel links one seat to one seat and women win the women's seats", {
  p <- read_parquet(here("data/tn/tn_11_later.parquet"))
  expect_false(anyDuplicated(p$gp_id) > 0)
  expect_false(anyDuplicated(p$row_id_2011) > 0)
  # recorded sex: every 2011 women's seat went to a woman; the later round has a
  # few seats recorded as won by a man
  expect_equal(mean(p$female_winner_2011[p$treat_2011 == 1], na.rm = TRUE), 1)
  expect_gt(mean(p$female_winner_later[p$treat_later == 1], na.rm = TRUE), 0.99)
})

tamil_nadu <- read_csv(here("tabs/tamil_nadu_inference.csv"), show_col_types = FALSE)
test_that("Tamil Nadu estimates reproduce: raw difference and independent within-union OLS", {
  d <- read_parquet(here("data/tn/tn_11_later.parquet")) |>
    filter(treat_later == 0) |>
    drop_na(female_winner_later)
  raw <- mean(d$female_winner_later[d$treat_2011 == 1]) - mean(d$female_winner_later[d$treat_2011 == 0])
  rows <- tamil_nadu |> filter(variance == "cluster")
  expect_equal(rows$estimate[rows$specification == "No FE"], raw, tolerance = 1e-9)
  within_x <- d$treat_2011 - ave(d$treat_2011, d$dist_block_later)
  within_y <- d$female_winner_later - ave(d$female_winner_later, d$dist_block_later)
  expect_equal(rows$estimate[rows$specification == "Block FE"], unname(coef(lm(within_y ~ within_x - 1))), tolerance = 1e-9)
  expect_true(all(rows$n == nrow(d)))
})

test_that("Jharkhand panel links one seat to one seat and the name model reads women's seats", {
  p <- read_parquet(here("data/jh/jh_15_22.parquet"))
  expect_false(anyDuplicated(p$gp_id) > 0)
  expect_false(anyDuplicated(p$row_id_2015) > 0)
  for (y in c("2015", "2022")) {
    f <- p[[paste0("female_winner_", y)]]
    expect_true(all(f[!is.na(f)] >= 0 & f[!is.na(f)] <= 1))
    # weaker on ST seats (about 0.88), so the bar is for the whole state
    expect_gt(mean(f[p[[paste0("treat_", y)]] == 1], na.rm = TRUE), 0.9)
  }
})

jharkhand <- read_csv(here("tabs/jharkhand_inference.csv"), show_col_types = FALSE)
test_that("Jharkhand estimates reproduce: raw difference and independent within-block OLS", {
  d <- read_parquet(here("data/jh/jh_15_22.parquet")) |>
    filter(treat_2022 == 0) |>
    drop_na(female_winner_2022)
  raw <- mean(d$female_winner_2022[d$treat_2015 == 1]) - mean(d$female_winner_2022[d$treat_2015 == 0])
  rows <- jharkhand |> filter(period == "All seats", variance == "cluster")
  expect_equal(rows$estimate[rows$specification == "No FE"], raw, tolerance = 1e-9)
  within_x <- d$treat_2015 - ave(d$treat_2015, d$dist_block_2022)
  within_y <- d$female_winner_2022 - ave(d$female_winner_2022, d$dist_block_2022)
  expect_equal(rows$estimate[rows$specification == "Block FE"], unname(coef(lm(within_y ~ within_x - 1))), tolerance = 1e-9)
  expect_true(all(rows$n == nrow(d)))
})

test_that("Telangana panel links one seat to one seat and the name model reads women's seats", {
  p <- read_parquet(here("data/tg/tg_19_25.parquet"))
  expect_false(anyDuplicated(p$gp_id) > 0)
  expect_false(anyDuplicated(p$row_id_2019) > 0)
  expect_setequal(unique(p$link_pass), c("code_and_name", "code_only", "name_only"))
  for (y in c("2019", "2025")) {
    f <- p[[paste0("female_winner_", y)]]
    expect_true(all(f[!is.na(f)] >= 0 & f[!is.na(f)] <= 1))
    expect_gt(mean(f[p[[paste0("treat_", y)]] == 1], na.rm = TRUE), 0.9)
  }
})

telangana <- read_csv(here("tabs/telangana_inference.csv"), show_col_types = FALSE)
test_that("Telangana estimates reproduce: raw difference and independent within-mandal OLS", {
  d <- read_parquet(here("data/tg/tg_19_25.parquet")) |>
    filter(treat_2025 == 0) |>
    drop_na(female_winner_2025)
  raw <- mean(d$female_winner_2025[d$treat_2019 == 1]) - mean(d$female_winner_2025[d$treat_2019 == 0])
  rows <- telangana |> filter(period == "All seats", variance == "cluster")
  expect_equal(rows$estimate[rows$specification == "No FE"], raw, tolerance = 1e-9)
  within_x <- d$treat_2019 - ave(d$treat_2019, d$dist_block_2025)
  within_y <- d$female_winner_2025 - ave(d$female_winner_2025, d$dist_block_2025)
  expect_equal(rows$estimate[rows$specification == "Mandal FE"], unname(coef(lm(within_y ~ within_x - 1))), tolerance = 1e-9)
  expect_true(all(rows$n == nrow(d)))
})

test_that("City panels keep one ward map and women win the women's seats", {
  del <- read_parquet(here("data/cities/del_07_12.parquet"))
  mum <- read_parquet(here("data/cities/mum_07_12.parquet"))
  expect_equal(c(nrow(del), nrow(mum)), c(272L, 227L))
  expect_false(anyDuplicated(c(del$ward_id, mum$ward_id)) > 0)
  # Wards reserved for women in 2012 must have elected women. Mumbai has one
  # exception in the deposit's coding.
  expect_equal(mean(del$female_winner_2012[del$treat_2012 == 1]), 1)
  expect_gte(sum(mum$female_winner_2012[mum$treat_2012 == 1], na.rm = TRUE), sum(mum$treat_2012) - 1)
  # The five wards another Delhi file leaves blank, read from the commission's
  # 2007 reservation order (local_elections data/delhi/reservation_order_*_2007.pdf).
  order_2007 <- c("3" = "W", "5" = "SC", "7" = "W", "9" = "SC", "47" = "W")
  expect_equal(del$category_2007[match(names(order_2007), del$ward)], unname(order_2007))
  expect_equal(c(sum(del$treat_2007), sum(mum$treat_2007)), c(92L, 76L))
})

cities <- read_csv(here("tabs/cities_inference.csv"), show_col_types = FALSE)
test_that("City estimates equal the raw difference in open-ward women's win rates", {
  councils <- list(
    `Delhi 2007-2012` = c("del_07_12", 2007, 2012), `Mumbai 2007-2012` = c("mum_07_12", 2007, 2012),
    `Kolkata 2010-2015` = c("kol_10_15", 2010, 2015)
  )
  for (council in names(councils)) {
    spec <- councils[[council]]
    p <- read_parquet(here("data/cities", paste0(spec[1], ".parquet")))
    p <- p[p[[paste0("treat_", spec[3])]] == 0 & !is.na(p[[paste0("female_winner_", spec[3])]]), ]
    won <- p[[paste0("female_winner_", spec[3])]]
    treated <- p[[paste0("treat_", spec[2])]] == 1
    row <- filter(cities, city == council, sample == "main")
    expect_equal(row$estimate, mean(won[treated]) - mean(won[!treated]), tolerance = 1e-12)
    expect_equal(c(row$left_women, row$right_women), c(sum(won[treated]), sum(won[!treated])))
  }
})

test_that("Kolkata keeps one ward map and codes women's-seat winners female", {
  kol <- read_parquet(here("data/cities/kol_10_15.parquet"))
  expect_equal(nrow(kol), 141L)
  # same boundaries: electorates by ward number track each other closely
  expect_gt(cor(kol$electors_2010, kol$electors_2015), 0.9)
  expect_true(all(kol$female_winner_2010[kol$treat_2010 == 1] == 1))
  open <- kol$treat_2015 == 0
  expect_true(all(kol$female_from_name_2015[open] %in% c(0, 1, NA)))
  expect_true(all(nzchar(kol$sex_method_2015)))
})

bihar <- read_csv(here("tabs/bihar_inference.csv"), show_col_types = FALSE)
test_that("Bihar short-term estimate reproduces after independent within-block OLS", {
  d <- read_parquet(here("data/bh/bh_11_16.parquet")) |>
    filter(!is.na(treat_2011), treat_2016 == 0) |>
    drop_na(female_winner_2016, dist_block_2016)
  within_x <- d$treat_2011 - ave(d$treat_2011, d$dist_block_2016)
  within_y <- d$female_winner_2016 - ave(d$female_winner_2016, d$dist_block_2016)
  independent <- lm(within_y ~ within_x - 1)
  covariance <- sandwich::vcovCL(independent, cluster = d$dist_block_2016, type = "HC0")
  # fixest's nonnested SSC counts the slope and one nested FE reference.
  covariance <- covariance * (nrow(d) - 1) / (nrow(d) - 2)
  row <- bihar |> filter(period == "2011-2016", sample == "full", fixed_effects, variance == "cluster")
  expect_equal(row$estimate, unname(coef(independent)), tolerance = 1e-9)
  expect_equal(row$standard_error, sqrt(drop(covariance)), tolerance = 1e-8)
  expect_equal(row$n, nrow(d))
  boot <- bihar |> filter(fixed_effects, variance == "cluster")
  expect_true(all(boot$bootstrap_B == 9999))
  expect_true(all(is.finite(boot$bootstrap_low) & is.finite(boot$bootstrap_high)))
})
