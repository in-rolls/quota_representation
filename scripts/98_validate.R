library(tidyverse)
library(arrow)
library(testthat)
library(fixest)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

test_that("source acquisition verifies cold-cache downloads and skips mismatched siblings", {
  root <- tempfile("pinned-source-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_envvar(INDIA_DATA_HOME = file.path(root, "cache"))
  revision <- paste(rep("a", 40), collapse = "")
  remote <- file.path(root, "remote", revision)
  dir.create(remote, recursive = TRUE)
  writeLines("pinned source", file.path(remote, "input.csv"))
  expected <- digest::digest(file.path(remote, "input.csv"), algo = "sha256", file = TRUE)
  sibling <- file.path(root, "sibling")
  dir.create(sibling)
  fixture <- list(cache_dir = file.path(root, "cache"), upstream = list(example = list(
    ref = revision, sibling = sibling, raw = paste0("file://", file.path(root, "remote")),
    files = list(input.csv = expected)
  )))
  resolver <- sibling_path
  environment(resolver) <- environment()
  .manifest <- function() fixture
  cold <- resolver("input.csv", "example")
  expect_identical(readLines(cold), "pinned source")
  unlink(cold)
  writeLines("other revision", file.path(sibling, "input.csv"))
  recovered <- resolver("input.csv", "example")
  expect_identical(readLines(recovered), "pinned source")
  expect_identical(readLines(file.path(sibling, "input.csv")), "other revision")
  unlink(recovered)
  writeLines("corrupt download", file.path(remote, "input.csv"))
  expect_error(resolver("input.csv", "example"), "Downloaded source hash mismatch")
  expect_false(file.exists(recovered))
})

candidates <- read_parquet(raj_product_path("candidates_2020_events.parquet"))
winners <- read_parquet(raj_product_path("winners_2020_events.parquet"))
test_that("sex linkage is unique within an election and invariant to source order", {
  expect_true(all(candidates$election_type == "General Election"))
  expect_setequal(unique(candidates$election_duration), c("JAN-MAR 2020", "SEP-OCT 2020"))
  expect_true(all(candidates$candidate_female[candidates$gender == "O"] == 0))
  linked <- list()
  for (reverse in c(FALSE, TRUE)) {
    source <- if (reverse) candidates |> slice(n():1) else candidates
    lookup <- source |>
      filter(candidate_name_unique) |>
      select(event_key, name = name_of_contesting_candidate, female = candidate_female)
    linked[[as.character(reverse)]] <- winners |>
      select(winner_source_row, event_key, winner_candidate_name) |>
      left_join(lookup,
        by = c("event_key", "winner_candidate_name" = "name"),
        relationship = "many-to-one", na_matches = "never"
      ) |>
      arrange(winner_source_row)
  }
  expect_equal(linked[[1]], linked[[2]])
  expect_equal(
    linked[[1]]$female,
    winners |> arrange(winner_source_row) |> pull(female_winner_2020)
  )
})

test_that("UP geographic bridge attaches by source identity and survives reordering", {
  bridge <- read_parquet(up_path("panels/gp_lgd_bridge.parquet"))
  expect_equal(anyDuplicated(bridge[c("panel", "anchor_key")]), 0L)
  panels <- c("05_10" = "2005_2010", "10_15" = "2010_2015",
    "15_21" = "2015_2021", "05_21" = "2005_2010_2015_2021")
  for (period in names(panels)) {
    anchor <- if (period == "15_21") "key_2015" else "key_2010"
    panel <- read_parquet(here("data/up", paste0("up_", period, ".parquet")))
    geography <- bridge |> filter(.data$panel == panels[[period]]) |>
      select(anchor_key, lgd_gp_code, mapping_review_id)
    expect_setequal(panel[[anchor]], geography$anchor_key)
    join <- function(rows) left_join(rows, geography, by = setNames("anchor_key", anchor),
      relationship = "one-to-one", na_matches = "never") |> arrange(.data[[anchor]])
    expect_identical(join(panel), join(panel |> slice(n():1)))
    matched <- read_parquet(here("data/up", paste0("shrug_gp_up_", period, "_block.parquet")))
    expect_identical(matched[names(panel)], panel)
    expected <- join(panel)
    actual <- matched |> arrange(.data[[anchor]])
    expect_identical(actual$lgd_gp_code, expected$lgd_gp_code)
    expect_identical(actual$mapping_review_id, expected$mapping_review_id)
  }
})

test_that("SHRUG joins preserve panel rows and observable facility definitions", {
  facilities <- c("pc01_vd_edu_fac", "pc01_vd_medi_fac", "pc01_vd_power_supl", "pc01_vd_bank_fac")
  for (state in c("raj", "up")) {
    panels <- if (state == "raj") c("05_10", "10_15", "15_20", "05_20") else c("05_10", "10_15", "15_21", "05_21")
    for (panel in panels) {
      original_path <- election_panel_path(state, paste0(state, "_", panel, ".parquet"))
      original <- read_parquet(original_path)
      matched <- read_parquet(here("data", state, paste0("shrug_gp_", state, "_", panel, "_block.parquet")))
      expect_equal(nrow(original), nrow(matched))
      if (state == "raj") expect_identical(matched[names(original)], original)
      for (facility in facilities) {
        expect_true(all(is.na(matched[[facility]]) | matched[[facility]] %in% 0:1))
        coverage <- matched[[paste0(facility, "_n_observed")]]
        expect_true(all(is.na(coverage) | coverage <= matched$n_villages))
      }
      expect_true(all(matched$pc01_vd_dist_town >= matched$pc01_vd_dist_town_min, na.rm = TRUE))
    }
  }
})

test_that("candidacy joins conserve rows and keep unknown outcomes unavailable", {
  analysis <- read_parquet(here("data/raj/candidacy_analysis.parquet"))
  panel <- read_parquet(raj_product_path("raj_05_20.parquet"))
  expect_equal(nrow(analysis), nrow(panel))
  expect_true(all(analysis$fem_vote_share >= 0 & analysis$fem_vote_share <= 1, na.rm = TRUE))
  expect_true(all(analysis$prop_women >= 0 & analysis$prop_women <= 1, na.rm = TRUE))
  unknown_events <- candidates |>
    summarise(
      unknown = any(!candidate_key_unique | is.na(candidate_female)),
      .by = match_key
    ) |>
    filter(unknown)
  expect_true(all(is.na(analysis$num_women[analysis$match_key_2020 %in% unknown_events$match_key])))
})

test_that("phone categories exhaust their stated denominators", {
  counts <- read_csv(here("tabs/phone_contact_counts.csv"), show_col_types = FALSE)
  totals <- counts |> summarise(n = sum(n), denominator = unique(denominator), .by = c(survey, section))
  expect_equal(totals$n, totals$denominator)
  expect_equal(counts |> filter(
    survey == "phone_survey", section == "Among male non-members",
    category %in% c("Spouse", "Child", "Other recorded relative")
  ) |> summarise(n = sum(n)) |> pull(n), 262)
  expected_contact <- tribble(
    ~survey, ~denominator, ~n,
    "phone_survey", 500, 377,
    "phone_survey_openseats", 507, 400,
    "jaipur_urban_phone_survey_quota", 78, 63,
    "jaipur_urban_phone_survey_open", 170, 146
  )
  expect_equal(counts |> filter(category == "Answered") |>
    select(survey, denominator, n), expected_contact)
  expect_equal(counts |>
    filter(category == "Recorded as elected representative") |>
    pull(n), c(35, 305, 20, 129))
  expect_true(all(counts$denominator > 0))
  quality <- read_csv(here("tabs/candidate_characteristics.csv"), show_col_types = FALSE)
  expect_false(anyNA(quality[c("open", "quota", "difference")]))
  expect_equal(quality$difference, quality$open - quality$quota)
})

inference <- read_csv(here("tabs/model_inference.csv"), show_col_types = FALSE)
test_that("reported cumulative contrasts reproduce after independent within-block OLS", {
  for (state in c("raj", "up")) {
    end <- if (state == "raj") "2020" else "2021"
    panel <- if (state == "raj") "05_20" else "05_21"
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", end)
    outcome <- paste0("female_winner_", end)
    source <- read_parquet(election_panel_path(state, paste0(state, "_", panel, ".parquet"))) |>
      filter(.data[[paste0("treat_", end)]] == 0) |>
      drop_na(all_of(c(block, outcome, "treat_2005", "treat_2010", "treat_2015")))
    x <- model.matrix(~ treat_2005 * treat_2010 * treat_2015, data = source)[, -1]
    within_x <- apply(x, 2, function(z) z - ave(z, source[[block]]))
    within_y <- source[[outcome]] - ave(source[[outcome]], source[[block]])
    independent <- lm(within_y ~ within_x - 1)
    b <- setNames(coef(independent), colnames(x))
    weights <- list(
      `111-000` = names(b),
      `111-001` = setdiff(names(b), "treat_2015"),
      `101-001` = c("treat_2005", "treat_2005:treat_2015"),
      `011-001` = c("treat_2010", "treat_2010:treat_2015")
    )
    reported <- inference |> filter(
      .data$state == .env$state, period == panel,
      sample == "full", fixed_effects, variance == "cluster"
    )
    covariance <- sandwich::vcovCL(independent, cluster = source[[block]], type = "HC0")
    # fixest's nonnested SSC counts the seven slopes and one nested FE reference.
    covariance <- covariance * (nrow(source) - 1) / (nrow(source) - 8)
    for (contrast in names(weights)) {
      w <- as.numeric(names(b) %in% weights[[contrast]])
      row <- reported |> filter(.data$contrast == .env$contrast)
      expect_equal(row$estimate, sum(b * w), tolerance = 1e-9)
      expect_equal(row$standard_error, sqrt(drop(t(w) %*% covariance %*% w)), tolerance = 1e-8)
      expect_equal(row$n, nrow(source))
    }
  }
})

test_that("bootstrap outputs use the declared draws and reproduce one seeded test", {
  primary <- inference |> filter(fixed_effects, variance == "cluster", sample %in% c("full", "restricted"))
  expect_true(all(primary$bootstrap_B == 9999))
  expect_true(all(primary$bootstrap_p >= 0 & primary$bootstrap_p <= 1))
  expect_true(all(is.finite(primary$bootstrap_low) & is.finite(primary$bootstrap_high)))
  bootstraps <- readRDS(here("data/model_bootstrap.rds"))
  expect_length(bootstraps, nrow(primary))
  d <- read_parquet(raj_product_path("raj_05_10.parquet")) |>
    filter(treat_2010 == 0) |>
    drop_na(female_winner_2010, treat_2005, dist_samiti_2010) |>
    mutate(dist_samiti_2010 = as.integer(factor(dist_samiti_2010)))
  model <- feols(female_winner_2010 ~ treat_2005 | dist_samiti_2010,
    data = d, vcov = ~dist_samiti_2010, ssc = MODEL_SSC, fixef.rm = "none"
  )
  expected <- primary |> filter(state == "raj", period == "05_10", sample == "full")
  set.seed(expected$bootstrap_seed)
  dqrng::dqset.seed(expected$bootstrap_seed)
  boot <- fwildclusterboot::boottest(model,
    param = "treat_2005", R = 1, B = 9999,
    clustid = "dist_samiti_2010", fe = "dist_samiti_2010", type = "rademacher",
    impose_null = TRUE, p_val_type = "two-tailed", conf_int = TRUE, sign_level = .05,
    engine = "R", sampling = "dqrng", nthreads = 1, maxiter = 100,
    ssc = fwildclusterboot::boot_ssc(
      adj = TRUE, fixef.K = "none",
      cluster.adj = TRUE, cluster.df = "conventional"
    )
  )
  expect_equal(boot$p_val, expected$bootstrap_p)
  expect_equal(as.numeric(boot$conf_int), c(expected$bootstrap_low, expected$bootstrap_high))
})
message("Validated joins, missingness, category totals, independent OLS contrasts, and bootstrap reproducibility.")

test_that("survey links attach by candidate identity and survive row reordering", {
  links <- read_parquet(here("data/raj/phone_candidate_links.parquet"))
  identity <- c(
    "event_key", "contesting_candidate_serial_no", "name_of_contesting_candidate",
    "father_husband_of_contesting_candidate"
  )
  expect_false(anyNA(links[identity]))
  expect_equal(anyDuplicated(links[identity]), 0L)
  expect_equal(nrow(anti_join(links, candidates, by = identity, na_matches = "never")), 0L)
  forward <- inner_join(candidates, links, by = identity, relationship = "one-to-one", na_matches = "never") |>
    arrange(key)
  reversed <- inner_join(candidates |> slice(n():1), links,
    by = identity,
    relationship = "one-to-one", na_matches = "never"
  ) |> arrange(key)
  expect_equal(forward, reversed)
  expect_equal(nrow(forward), 35L)
})
