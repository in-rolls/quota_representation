library(testthat)
library(arrow)
library(dplyr)
library(readr)
source(here::here("scripts/00_config.R"))
source(here::here("scripts/00_census.R"))

coverage <- read_csv(here::here("tabs/election_coverage.csv"), show_col_types = FALSE)
test_that("election coverage uses cleaned complete identities and known GP reservation", {
  for (j in seq_len(nrow(coverage))) {
    row <- coverage[j, ]
    if (row$state == "Rajasthan") {
      raw <- read_raj(raj_year_path(row$year))
      keys <- raw[c("district_raw", "samiti_raw", "gp_std")]
      quota <- raw$female_reserved
    } else {
      raw <- read_up_election_records() |> filter(election_year == row$year)
      gp <- if (row$year == 2021L) {
        ifelse(is.na(raw$gp_number_raw) | is.na(raw$gp_name_hindi), NA_character_,
          paste(raw$gp_number_raw, "-", raw$gp_name_hindi))
      } else raw$gp_name_hindi
      keys <- data.frame(district = raw$district_name_hindi, block = raw$block_name_hindi, gp)
      quota <- raw$reserved_woman
    }
    keys <- as.data.frame(lapply(keys, function(x) {
      x <- stringr::str_to_lower(stringr::str_squish(stringi::stri_trans_nfc(as.character(x))))
      x[x == ""] <- NA_character_
      x
    }))
    complete <- complete.cases(keys)
    grouped <- split(quota[complete], do.call(paste, c(keys[complete, ], sep = "\r")))
    distinct_values <- lapply(grouped, function(x) unique(x[!is.na(x)]))
    unique_quota <- vapply(distinct_values, function(x) if (length(x) == 1L) x else NA_integer_, numeric(1))
    expect_equal(row$source_records, nrow(raw))
    expect_equal(row$identified_gps, length(grouped))
    expect_equal(row$missing_identity_records, sum(!complete))
    expect_equal(row$duplicate_identity_records, sum(complete) - length(grouped))
    expect_equal(row$known_reservation_gps, sum(!is.na(unique_quota)))
    expect_equal(row$quota_gps, sum(unique_quota == 1, na.rm = TRUE))
    expect_equal(row$conflicting_reservation_gps, sum(lengths(distinct_values) > 1L))
    expect_equal(row$quota_share, mean(unique_quota, na.rm = TRUE), tolerance = 1e-12)
    expect_equal(row$districts, length(unique(na.omit(keys[[1]]))))
    expect_equal(row$blocks, nrow(unique(keys[complete.cases(keys[1:2]), 1:2])))
  }
  expect_equal(coverage$identified_gps[coverage$state == "Uttar Pradesh" & coverage$year == 2010], 51757)
})

test_that("availability distinguishes absence, missingness, and counts", {
  expect_identical(census_availability(c(0, 0)), 0L)
  expect_identical(census_availability(c(0, 1, NA)), 1L)
  expect_identical(census_availability(c(0, NA)), NA_integer_)
  expect_identical(census_availability(c(NA_real_, NA_real_)), NA_integer_)
  expect_error(census_availability(c(1, 2)))
})

fields <- c("pc01_vd_medi_fac", "pc01_vd_bank_fac", "pc01_vd_power_supl", "pc01_vd_app_mr")
raw_vd <- read_csv(shrug_path("shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip"),
  col_select = all_of(c("shrid2", fields)), show_col_types = FALSE)
crosswalk <- read_csv(local_source_path("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
  show_col_types = FALSE) |>
  filter(tolower(state_name) %in% c("rajasthan", "uttar pradesh"), !is.na(LGD_code))

balance <- read_csv(here::here("tabs/census_balance.csv"), show_col_types = FALSE)
test_that("Census aggregates, denominators and balance statistics reconstruct", {
  for (state in c("raj", "up")) {
    geography <- crosswalk |> filter(tolower(state_name) == if (state == "raj") "rajasthan" else "uttar pradesh")
    source_rows <- match(geography$shrid2, raw_vd$shrid2)
    grouped <- split(seq_len(nrow(geography)), geography$LGD_code)
    availability <- lapply(fields, function(field) {
      values <- raw_vd[[field]][source_rows]
      vapply(grouped, function(indices) {
        x <- values[indices]
        if (1 %in% x) 1 else if (anyNA(x)) NA_real_ else 0
      }, numeric(1))
    })
    names(availability) <- fields
    periods <- c("05_10", "10_15", if (state == "raj") "15_20" else "15_21")
    for (period in periods) {
      data <- read_parquet(here::here("data", state, paste0("shrug_gp_", state, "_", period, "_block.parquet")))
      for (field in fields) {
        expect_equal(data[[field]], unname(availability[[field]][as.character(data$lgd_gp_code)]), tolerance = 1e-12)
      }
      independently <- data.frame(
        pc01_vd_t_p = data$pc01_vd_t_p,
        female_share = data$pc01_vd_t_f / data$pc01_vd_t_p,
        sc_share = data$pc01_vd_sc_p / data$pc01_vd_t_p,
        st_share = data$pc01_vd_st_p / data$pc01_vd_t_p,
        medical_available = data$pc01_vd_medi_fac,
        m_home_per1k = 1000 * data$pc01_vd_m_home / data$pc01_vd_t_p,
        mcw_per1k = 1000 * data$pc01_vd_mcw_cntr / data$pc01_vd_t_p,
        p_sch_per1k = 1000 * data$pc01_vd_p_sch / data$pc01_vd_t_p,
        m_sch_per1k = 1000 * data$pc01_vd_m_sch / data$pc01_vd_t_p,
        banking_available = data$pc01_vd_bank_fac,
        power_available = data$pc01_vd_power_supl,
        road_available = data$pc01_vd_app_mr
      )
      independently[!is.na(data$pc01_vd_t_p) & data$pc01_vd_t_p <= 0, 2:4] <- NA_real_
      independently[!is.na(data$pc01_vd_t_p) & data$pc01_vd_t_p <= 0, 6:9] <- NA_real_
      constructed <- census_balance_covariates(data)
      expect_equal(as.data.frame(constructed[names(independently)]), independently, tolerance = 1e-12)
      years <- paste0("20", strsplit(period, "_", fixed = TRUE)[[1]])
      treatment <- data[[paste0("treat_", years[1])]]
      complete <- complete.cases(independently, treatment, data[[paste0("treat_", years[2])]])
      rows <- balance |> filter(comparison == paste(state, period, sep = "_"))
      expect_equal(nrow(rows), 12L)
      block <- data[[paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])]][complete]
      expect_false(anyNA(block))
      x <- apply(independently[complete, ], 2, function(z) z - ave(z, block))
      y <- treatment[complete] - ave(treatment[complete], block)
      model <- lm.fit(x, y)
      bread <- solve(crossprod(x))
      scores <- rowsum(x * model$residuals, block)
      groups <- nrow(scores)
      covariance <- bread %*% crossprod(scores) %*% bread *
        groups / (groups - 1) * (nrow(x) - 1) / (nrow(x) - ncol(x) - 1)
      statistic <- drop(crossprod(model$coefficients, solve(covariance, model$coefficients))) / ncol(x)
      independent_p <- pf(statistic, ncol(x), groups - 1, lower.tail = FALSE)
      expect_equal(log(unique(rows$joint_p)), log(independent_p), tolerance = 1e-8)
      for (v in names(independently)) {
        row <- rows |> filter(var_name == v)
        expect_equal(row$n, sum(complete))
        expect_equal(row$Mean_Treat, mean(independently[[v]][complete & treatment == 1]), tolerance = 1e-12)
        expect_equal(row$Mean_Control, mean(independently[[v]][complete & treatment == 0]), tolerance = 1e-12)
      }
    }
  }
})
message("Election coverage, known-reservation denominators and Census balance variables validated.")
