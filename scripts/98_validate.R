library(tidyverse)
library(arrow)
library(testthat)
library(fixest)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

test_that("publication tables express rate effects in points without changing estimates", {
  data <- data.frame(
    y = c(0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    treat = rep(c(0, 0, 1, 1), 3),
    sc_share = c(.1, .4, .2, .3, .2, .6, .4, .1, .3, .1, .7, .5),
    block = rep(1:3, each = 4)
  )
  fit <- feols(y ~ treat + sc_share | block, data = data, vcov = ~block, ssc = MODEL_SSC)
  original <- list(coefficients = coef(fit), covariance = vcov(fit),
    residuals = residuals(fit), fit_statistics = fitstat(fit, ~r2 + n))
  data$y <- 100 * data$y
  data$sc_share <- 100 * data$sc_share
  refit <- feols(y ~ treat + sc_share | block, data = data, vcov = ~block, ssc = MODEL_SSC)
  output <- tempfile(fileext = ".tex")
  on.exit(unlink(output), add = TRUE)
  aer_etable(list(fit), output, dict = c(treat = "Quota", sc_share = "SC percentage"))
  rendered <- readLines(output)
  for (term in c("treat", "sc_share")) {
    label <- if (term == "treat") "Quota" else "SC percentage"
    row <- grep(paste0("^ *", label, " +&"), rendered)
    expect_length(row, 1L)
    values <- regmatches(rendered[c(row, row + 1)], gregexpr("-?[0-9]+[.][0-9]+", rendered[c(row, row + 1)]))
    expect_equal(as.numeric(values[[1]][1]), round(coef(refit)[[term]], 1))
    expect_equal(as.numeric(values[[2]][1]), round(se(refit)[[term]], 1))
  }
  expect_identical(list(coefficients = coef(fit), covariance = vcov(fit),
    residuals = residuals(fit), fit_statistics = fitstat(fit, ~r2 + n)), original)
  expect_match(paste(rendered, collapse = "\n"), "percentage points")
  expect_equal(format_p_value(c(0, .0009, .001, .025, NA), latex = FALSE),
    c("<0.001", "<0.001", "0.001", "0.025", "--"))
})

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
  fixture$upstream$example$local_only <- TRUE
  expect_error(resolver("input.csv", "example"), "Unpublished upstream source missing or changed")
  expect_false(dir.exists(file.path(root, "cache")))
  fixture$upstream$example$local_only <- FALSE
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
  writeLines("pinned source", file.path(sibling, "input.csv"))
  expect_identical(resolver("input.csv", "example"), normalizePath(file.path(sibling, "input.csv")))
  expect_false(file.exists(recovered))
})

candidates <- read_raj(raj_product_path("candidates_2020_events.parquet"))
winners <- read_raj(raj_product_path("winners_2020_events.parquet"))
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
  bridge <- read_up(up_path("linkage/gp_lgd_bridge.parquet"))
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
      original <- read_analysis_parquet(original_path)
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
  panel <- read_raj(raj_product_path("raj_05_20.parquet"))
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

test_that("candidacy history comparisons reproduce with within-block OLS", {
  analysis <- read_parquet(here("data/raj/candidacy_analysis.parquet")) |>
    filter(treat_2020 == 0)
  reported <- read_csv(here("tabs/candidacy_estimates.csv"), show_col_types = FALSE)
  for (outcome in c("women_present", "prop_women", "num_women", "fem_vote_share")) {
    data <- analysis |> drop_na(all_of(c(outcome, "dist_samiti_2020", "treat_2005", "treat_2010", "treat_2015")))
    group <- data$dist_samiti_2020
    x <- model.matrix(~ treat_2005 * treat_2010 * treat_2015, data = data)[, -1]
    within_x <- apply(x, 2, function(z) z - ave(z, group))
    within_y <- data[[outcome]] - ave(data[[outcome]], group)
    fit <- lm(within_y ~ within_x - 1)
    index <- which(colnames(x) == "treat_2015")
    estimate <- unname(coef(fit)[index])
    covariance <- sandwich::vcovCL(fit, cluster = group, type = "HC0") * (nrow(data) - 1) / (nrow(data) - 8)
    se <- sqrt(covariance[index, index])
    interval <- estimate + c(-1, 1) * qt(.975, n_distinct(group) - 1) * se
    row <- reported |> filter(model == paste0(outcome, "_fe"), term == "treat_2015")
    expect_equal(nrow(row), 1L)
    expect_equal(row$observations, nrow(data))
    expect_equal(row$estimate, estimate, tolerance = 1e-9)
    expect_equal(row$std.error, se, tolerance = 1e-8)
    expect_equal(c(row$conf.low, row$conf.high), interval, tolerance = 1e-8)
  }
})

test_that("phone categories exhaust their stated denominators", {
  counts <- read_csv(here("tabs/phone_contact_counts.csv"), show_col_types = FALSE)
  totals <- counts |> summarise(n = sum(n), denominator = unique(denominator), .by = c(survey, section))
  expect_equal(totals$n, totals$denominator)
  expect_equal(counts |> filter(
    survey == "phone_survey", section == "Recorded male non-members",
    category %in% c("Spouse", "Child", "Other recorded relative")
  ) |> summarise(n = sum(n)) |> pull(n), 265)
  expected_contact <- tribble(
    ~survey, ~denominator, ~n,
    "phone_survey", 500, 382,
    "phone_survey_openseats", 507, 404,
    "jaipur_urban_phone_survey_quota", 83, 66,
    "jaipur_urban_phone_survey_open", 165, 143
  )
  expect_equal(counts |> filter(category == "Answered") |>
    select(survey, denominator, n), expected_contact)
  expect_equal(counts |>
    filter(category == "Officeholder contact recorded") |>
    pull(n), c(38, 310, 22, 130))
  expect_true(all(counts$denominator > 0))
  quality <- read_csv(here("tabs/candidate_characteristics.csv"), show_col_types = FALSE)
  expect_false(anyNA(quality[c("open", "quota", "difference")]))
  expect_equal(quality$difference, quality$open - quality$quota)
})

test_that("graduation rates exclude unknown education from their denominators", {
  eligible <- candidates |>
    filter(candidate_key_unique, gp_event_unique)
  winner_keys <- winners |>
    filter(winner_key_unique, gp_event_unique) |>
    select(event_key, name_of_contesting_candidate = winner_candidate_name)
  elected <- eligible |>
    filter(candidate_name_unique) |>
    semi_join(winner_keys, by = c("event_key", "name_of_contesting_candidate"), na_matches = "never")
  samples <- list(
    Winners = elected, Candidates = eligible,
    `Women winners` = filter(elected, candidate_female == 1),
    `Women candidates` = filter(eligible, candidate_female == 1)
  )
  reported <- read_csv(here("tabs/candidate_characteristics.csv"), show_col_types = FALSE) |>
    filter(variable == "Graduate")
  expect_equal(sum(elected$education_status == "nan" & elected$primary_female_reserved == 0, na.rm = TRUE), 75L)
  expect_equal(sum(elected$education_status == "nan" & elected$primary_female_reserved == 1, na.rm = TRUE), 170L)
  for (sample_name in names(samples)) {
    d <- samples[[sample_name]]
    education <- tolower(trimws(d$education_status))
    observed <- !is.na(education) & !education %in% c("", "nan")
    graduate <- education %in% c("graduate", "postgraduate", "professional graduate", "professional post graduate")
    row <- reported |> filter(sample == sample_name)
    for (group in c("open", "quota")) {
      selected <- which(observed & d$primary_female_reserved == as.integer(group == "quota"))
      expect_equal(row[[paste0(group, "_n")]], length(selected))
      expect_equal(row[[group]], mean(graduate[selected]), tolerance = 1e-12)
    }
  }
  phone <- read_csv(here("tabs/phone_respondent_characteristics.csv"), show_col_types = FALSE) |>
    filter(variable == "Graduate")
  expect_equal(phone$n, 36)
  expect_equal(phone$mean, 13 / 36)
})

inference <- read_csv(here("tabs/model_inference.csv"), show_col_types = FALSE)
test_that("UP control means reproduce from pinned source election panels", {
  periods <- list(`05_10` = c(2005, 2010), `10_15` = c(2010, 2015), `15_21` = c(2015, 2021))
  for (period in names(periods)) {
    years <- periods[[period]]
    d <- read_up(up_path(paste0("linkage/gp_panel_", paste(years, collapse = "_"), ".parquet")))
    outcome <- d[[paste0("winner_woman_", years[2])]]
    controls <- which(
      d[[paste0("women_reserved_", years[1])]] == 0 &
        d[[paste0("women_reserved_", years[2])]] == 0 &
        !is.na(outcome) & !is.na(d[[paste0("district_name_eng_", years[2])]]) &
        !is.na(d[[paste0("block_name_eng_", years[2])]])
    )
    row <- inference |> filter(state == "up", .data$period == .env$period,
      sample == "full", fixed_effects, variance == "cluster")
    expect_equal(row$right_n, length(controls))
    expect_equal(row$right_mean, mean(outcome[controls]), tolerance = 1e-12)
  }
})

test_that("reported cumulative contrasts reproduce after independent within-block OLS", {
  for (state in c("raj", "up")) {
    end <- if (state == "raj") "2020" else "2021"
    panel <- if (state == "raj") "05_20" else "05_21"
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", end)
    outcome <- paste0("female_winner_", end)
    source <- read_analysis_parquet(election_panel_path(state, paste0(state, "_", panel, ".parquet"))) |>
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
  d <- read_raj(raj_product_path("raj_05_10.parquet")) |>
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
  links <- read_parquet(here("data/raj/phone_respondents_reviewed.parquet")) |>
    select(event_key, contesting_candidate_serial_no, name_of_contesting_candidate,
      father_husband_of_contesting_candidate)
  identity <- c(
    "event_key", "contesting_candidate_serial_no", "name_of_contesting_candidate",
    "father_husband_of_contesting_candidate"
  )
  expect_false(anyNA(links[identity]))
  expect_equal(anyDuplicated(links[identity]), 0L)
  expect_equal(nrow(anti_join(links, candidates, by = identity, na_matches = "never")), 0L)
  forward <- inner_join(candidates, links, by = identity, relationship = "one-to-one", na_matches = "never") |>
    arrange(across(all_of(identity)))
  reversed <- inner_join(candidates |> slice(n():1), links,
    by = identity,
    relationship = "one-to-one", na_matches = "never"
  ) |> arrange(across(all_of(identity)))
  expect_equal(forward, reversed)
  expect_equal(nrow(forward), 38L)
})


test_that("phone review retains source identities, later contacts, and classification uncertainty", {
  reviewed <- read_parquet(here("data/raj/phone_contacts_reviewed.parquet"))
  expect_equal(nrow(reviewed), 1255L)
  expect_equal(anyDuplicated(reviewed[c("source_file", "excel_row")]), 0L)
  changed_answer <- reviewed |> filter(phone_answered != any_answered)
  expect_equal(nrow(changed_answer), 11L)
  later_contact <- reviewed |> filter(phone_answered_by == "non_member", any_officeholder_contact == "yes")
  expect_equal(nrow(later_contact), 5L)
  expect_true(all(later_contact$respondent_gender == "male"))
  expect_true(all(later_contact$any_answered == "yes"))
  quota <- reviewed |> filter(survey == "phone_survey")
  expect_equal(quota$any_officeholder_contact[quota$excel_row == 65], "yes")
  expect_equal(quota$phone_answered_by[quota$excel_row == 65], "non_member")
  expect_equal(quota$contact_sensitivity[quota$excel_row == 316], "unknown")
  expect_equal(quota$any_officeholder_contact[quota$excel_row == 316], "no")
  urban <- reviewed |> filter(grepl("^jaipur", survey))
  expect_equal(sum(urban$treat_status != urban$recorded_treat_status), 11L)
  expected_quota <- urban |> filter(
    (corporation == "GREATER" & ward %in% c(5, 88, 109, 130, 143)) |
      (corporation == "HERITAGE" & ward %in% c(12, 16, 18))
  )
  expect_equal(nrow(expected_quota), 8L)
  expect_true(all(expected_quota$treat_status == 1))
  expect_true(all(urban$treat_status[urban$corporation == "GREATER" & urban$ward %in% 29:31] == 0))
  bounds <- read_csv(here("tabs/phone_contact_sensitivity.csv"), show_col_types = FALSE)
  expect_equal(bounds$definite, c(36, 300, 19, 130))
  expect_equal(bounds$unknown, c(15, 27, 4, 4))
  expect_true(all(bounds$lower <= bounds$share & bounds$upper >= bounds$share))
  expect_lt(bounds$upper[bounds$survey == "phone_survey"], bounds$lower[bounds$survey == "phone_survey_openseats"])
  respondents <- read_parquet(here("data/raj/phone_respondents_reviewed.parquet"))
  expect_equal(nrow(respondents), 38L)
  expect_setequal(respondents$excel_row, quota$excel_row[quota$any_answered == "yes" & quota$any_officeholder_contact == "yes"])
  expect_equal(sort(setdiff(respondents$excel_row, quota$excel_row[quota$phone_answered == "yes" & quota$phone_answered_by == "member"])), c(44L, 65L, 119L))
})

test_that("design sample counts and contemporaneous control shares match the fitted samples", {
  summary <- read_csv(here("tabs/next_election_samples.csv"), show_col_types = FALSE)
  fitted <- read_csv(here("tabs/model_inference.csv"), show_col_types = FALSE) |>
    filter(sample == "full", fixed_effects, variance == "cluster", !str_detect(contrast, "-"))
  expect_equal(nrow(summary), 6L)
  expect_equal(anyDuplicated(summary[c("state", "period")]), 0L)
  for (i in seq_len(nrow(summary))) {
    row <- summary[i, ]
    path <- election_panel_path(row$state, paste0(row$state, "_", row$period, ".parquet"))
    data <- as.data.frame(read_analysis_parquet(path))
    treat <- paste0("treat_", row$treatment_year)
    current <- paste0("treat_", row$outcome_year)
    outcome <- paste0("female_winner_", row$outcome_year)
    prior <- paste0("female_winner_", row$treatment_year)
    block <- paste0(if (row$state == "raj") "dist_samiti_" else "dist_block_", row$outcome_year)
    eligible <- data[!is.na(data[[current]]) & data[[current]] == 0, ]
    fit <- feols(as.formula(paste(outcome, "~", treat, "|", block)), data = eligible,
      vcov = as.formula(paste("~", block)), ssc = MODEL_SSC, fixef.rm = "none", notes = FALSE)
    sample <- eligible[fixest::obs(fit), ]
    control <- sample[sample[[treat]] == 0, ]
    known <- control[[prior]][!is.na(control[[prior]])]
    saved <- fitted |> filter(state == row$state, period == row$period)
    expect_equal(nrow(saved), 1L)
    expect_equal(row$linked, nrow(data))
    expect_equal(row$open_at_followup, nrow(eligible))
    expect_equal(row$analysis_n, nobs(fit))
    expect_equal(row$analysis_n, saved$n)
    expect_equal(row$quota_n, saved$left_n)
    expect_equal(row$control_n, saved$right_n)
    expect_equal(row$quota_n + row$control_n, row$analysis_n)
    expect_equal(row$control_sex_observed, length(known))
    expect_equal(row$control_women, sum(known))
    expect_equal(row$control_women_share, mean(known))
    expect_equal(row$control_followup_women_share, mean(control[[outcome]]))
    expect_equal(row$control_followup_women_share, saved$right_mean)
  }
  missing <- summary$control_n - summary$control_sex_observed
  expect_true(any(missing > 0))
  expect_true(all(summary$control_women_share[missing > 0] >
    (summary$control_women / summary$control_n)[missing > 0]))
})

test_that("new specification checks reproduce with independent within-group OLS", {
  comparisons <- read_csv(here("tabs/short_term_sample_comparisons.csv"), show_col_types = FALSE)
  allocation <- read_csv(here("tabs/short_term_allocation_checks.csv"), show_col_types = FALSE)
  balance <- read_csv(here("tabs/open_sample_balance.csv"), show_col_types = FALSE)
  original <- read_csv(here("tabs/short_term_covariate_estimates.csv"), show_col_types = FALSE)
  panels <- comparisons |> distinct(state, period)
  expect_equal(nrow(panels), 6L)
  for (i in seq_len(nrow(panels))) {
    state <- panels$state[i]
    period <- panels$period[i]
    years <- paste0("20", strsplit(period, "_")[[1]])
    outcome <- paste0("female_winner_", years[2])
    treatment <- paste0("treat_", years[1])
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])
    baseline_block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[1])
    caste <- paste0(if (state == "raj") "caste_category_" else "reservation_class_", years[1])
    source <- read_parquet(here("data", state, paste0("shrug_gp_", state, "_", period, "_block.parquet"))) |>
      filter(.data[[paste0("treat_", years[2])]] == 0)
    covariates <- cbind(
      log1p(source$pc01_pca_tot_p),
      source$pc01_pca_p_sc / source$pc01_pca_tot_p,
      source$pc01_pca_p_lit / source$pc01_pca_tot_p,
      source$pc01_pca_tot_f / source$pc01_pca_tot_p,
      source$pc01_vd_edu_fac + source$pc01_vd_medi_fac + source$pc01_vd_power_supl + source$pc01_vd_bank_fac
    )
    covariates[!is.finite(covariates)] <- NA_real_
    complete <- complete.cases(source[c(outcome, treatment, block)], covariates)
    rows <- comparisons |> filter(.data$state == .env$state, .data$period == .env$period)
    for (spec in c("full", "common", "adjusted")) {
      keep <- if (spec == "full") complete.cases(source[c(outcome, treatment, block)]) else complete
      group <- source[[block]][keep]
      x <- matrix(source[[treatment]][keep], ncol = 1)
      if (spec == "adjusted") x <- cbind(x, covariates[keep, ])
      x <- apply(x, 2, function(z) z - ave(z, group))
      y <- source[[outcome]][keep] - ave(source[[outcome]][keep], group)
      fit <- lm(y ~ x - 1)
      covariance <- sandwich::vcovCL(fit, cluster = group, type = "HC0") *
        (length(y) - 1) / (length(y) - ncol(x) - 1)
      row <- rows |> filter(specification == spec)
      expect_equal(row$n, length(y))
      expect_equal(row$estimate, unname(coef(fit)[1]), tolerance = 1e-9)
      expect_equal(row$std.error, sqrt(covariance[1, 1]), tolerance = 1e-8)
    }
    adjusted <- rows |> filter(specification == "adjusted")
    prior_fit <- original |> filter(model == 2 * i, term == treatment)
    expect_equal(adjusted$estimate, prior_fit$estimate, tolerance = 1e-9)
    expect_equal(adjusted$n, prior_fit$n)
    diagnostic <- balance |> filter(.data$state == .env$state, .data$period == .env$period)
    expect_equal(diagnostic$n, sum(complete))
    expect_equal(diagnostic$blocks, n_distinct(source[[block]][complete]))
    expect_true(diagnostic$joint_p >= 0 && diagnostic$joint_p <= 1)
    if (state == "raj") {
      district <- source[[paste0("district_std_", years[1])]]
      samiti <- source[[paste0("samiti_std_", years[1])]]
      source[[baseline_block]] <- ifelse(is.na(district) | is.na(samiti), NA_character_, paste(district, samiti))
    }
    source <- source[complete.cases(source[c(outcome, treatment, block, baseline_block, caste)]), ]
    for (spec in c("outcome_block", "treatment_block", "treatment_block_caste")) {
      group <- source[[if (spec == "outcome_block") block else baseline_block]]
      if (spec == "treatment_block_caste") group <- interaction(group, source[[caste]], drop = TRUE)
      y <- source[[outcome]] - ave(source[[outcome]], group)
      x <- source[[treatment]] - ave(source[[treatment]], group)
      row <- allocation |> filter(.data$state == .env$state, .data$period == .env$period, specification == spec)
      expect_equal(row$n, length(y))
      expect_equal(row$estimate, sum(x * y) / sum(x^2), tolerance = 1e-9)
    }
  }
})
