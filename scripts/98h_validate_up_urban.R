library(testthat)
source(here::here("scripts/13_up_urban.R"))

prepared <- prepare_up_urban()
d <- prepared$links |> filter(included)
reported <- read_csv(here("tabs/up_urban_estimates.csv"), show_col_types = FALSE)

test_that("urban sample preserves distinct office identities and the quota contrast", {
  expect_false(anyDuplicated(prepared$links$record_id_2012) > 0)
  expect_false(anyDuplicated(prepared$links$record_id_2017) > 0)
  expect_true(all(d$current_quota == 0))
  expect_true(all(d$prior_quota %in% c(0, 1)))
  expect_true(all(d$y %in% c(0, 1)))
  expect_true(all(d$quota_2012[d$female_2012 == 0] == 0))
  expect_true(any(d$prior_quota == 0 & d$female_2012 == 1))
  expect_equal(nrow(prepared$records), sum(prepared$coverage$records))
  expect_equal(nrow(prepared$records), 2 * nrow(prepared$links) + nrow(prepared$unmatched))
  expect_true(all(prepared$records$record_id[prepared$records$identity_n > 1] %in%
    prepared$unmatched$record_id))
  expect_equal(urban_name("कानपुर नगर"), urban_name("कानपुरनगर"))
  expect_false(urban_name("नगर 1") == urban_name("नगर 2"))
  expect_false(urban_name("खेर") == urban_name("खैर"))
  expect_false(urban_name("बडा") == urban_name("बड़ा"))
})

manual_fit <- function(data, formula, cluster) {
  x <- model.matrix(formula, data)
  qr <- qr(x)
  x <- x[, sort(qr$pivot[seq_len(qr$rank)]), drop = FALSE]
  y <- data$y
  bread <- solve(crossprod(x))
  beta <- bread %*% crossprod(x, y)
  residual <- as.numeric(y - x %*% beta)
  scores <- rowsum(x * residual, data[[cluster]])
  n <- nrow(x)
  g <- nrow(scores)
  v <- bread %*% crossprod(scores) %*% bread * g / (g - 1) * (n - 1) / (n - ncol(x))
  c(estimate = unname(beta["prior_quota", 1]), standard_error = unname(sqrt(v["prior_quota", "prior_quota"])))
}

test_that("urban differences and clustered covariance reproduce independently", {
  subsets <- list(d, d, filter(d, office == "municipal_council_chair"),
    filter(d, office == "nagar_panchayat_chair"), filter(d, identical_spelling), filter(d, same_caste), d)
  for (i in seq_len(nrow(reported))) {
    r <- reported[i, ]
    data <- subsets[[i]]
    formula <- if (i == 2) y ~ prior_quota + factor(prior_caste) + factor(office) + factor(district) else y ~ prior_quota
    check <- manual_fit(data, formula, r$cluster)
    expect_equal(r$estimate, unname(check["estimate"]), tolerance = 1e-10)
    expect_equal(r$standard_error, unname(check["standard_error"]), tolerance = 1e-10)
    expect_equal(r$n, nrow(data))
    expect_equal(r$quota_women, sum(data$y[data$prior_quota == 1]))
    expect_equal(r$nonquota_women, sum(data$y[data$prior_quota == 0]))
    expect_equal(r$conf_low, r$estimate - qt(.975, r$clusters - 1) * r$standard_error)
  }
  expect_equal(reported$estimate[1], mean(d$y[d$prior_quota == 1]) - mean(d$y[d$prior_quota == 0]))
})

# Independently read the printed cells, bypassing the upstream parsers.
urban_html_rows <- function(path, expected_hash) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  raw <- readBin(con, "raw", n = 5e7)
  stopifnot(digest::digest(raw, "sha256", serialize = FALSE) == expected_hash)
  doc <- xml2::read_html(raw)
  rows <- xml2::xml_find_all(doc, "//tr")
  map(rows, ~ stri_trim_both(stri_replace_all_regex(
    xml2::xml_text(xml2::xml_find_all(.x, "./td|./th")), "\\s+", " ")))
}

validate_urban_sources <- function() {
  root <- normalizePath(file.path(here(), "../local_elections_up"))
  parsed_files <- c(
    `2012` = "data/collections/sec_historical_results/ulb2012/parsed/20260911T113301294231Z/winner_reservations.parquet",
    `2017` = "data/collections/sec_historical_results/ulb2017/combined/20260911/winner_reservations.parquet"
  )
  audit <- list()
  for (year in c(2012, 2017)) {
    parsed <- read_parquet(urban_path(parsed_files[[as.character(year)]]))
    releases <- urban_sources() |> filter(.data$year == .env$year) |>
      pull(path) |> map_dfr(read_parquet)
    joined <- releases |> left_join(parsed, by = c("source_observation_id" = "observation_id"),
      relationship = "one-to-one", suffix = c("_release", "_parsed"))
    stopifnot(nrow(joined) == nrow(releases), !anyNA(joined$source_sha256))
    for (hash in unique(joined$source_sha256)) {
      group <- joined |> filter(source_sha256 == hash)
      path <- if (year == 2012) file.path(root, "data/collections/sec_historical_results/ulb2012", group$source_path[1]) else
        file.path(root, group$source_response_repo_path[1])
      rows <- urban_html_rows(path, hash)
      for (i in seq_len(nrow(group))) {
        r <- group[i, ]
        if (year == 2012) {
          candidates <- keep(rows, ~ length(.x) == r$source_column_count && grepl("^[0-9]+$", .x[1]))
          shift <- as.integer(r$source_post_code_column_present)
          expected <- c(r$district_name_raw_release, r$body_name_raw, r$reserved_raw,
            r$candidate_name_raw, r$candidate_woman_raw)
          positions <- c(2, 5, 9 + shift, 10 + shift, 15 + shift)
        } else {
          candidates <- keep(rows, ~ length(.x) == 15 && .x[9] %in% c("महिला", "पुरुष"))
          expected <- c(r$district_name_raw_release, r$body_name_raw, r$reserved_raw,
            r$candidate_name_raw, r$candidate_woman_raw)
          positions <- c(1, 2, 3, 4, 9)
        }
        index <- r$source_row_on_page
        stopifnot(index >= 1, index <= length(candidates))
        observed <- candidates[[index]][positions]
        if (!is.na(r$reserved_woman)) {
          stopifnot(r$reserved_woman == as.integer(grepl("महिला", observed[3])))
        }
        agrees <- urban_name(expected) == urban_name(observed)
        audit[[length(audit) + 1L]] <- tibble(year, record_id = r$record_id,
          source_sha256 = hash, fields_checked = length(positions), fields_agree = sum(agrees),
          all_agree = all(agrees))
      }
    }
  }
  audit <- bind_rows(audit)
  write_csv(audit, here("tabs/up_urban_source_audit.csv"))
  test_that("urban release geography, reservation and sex match every held official source row", {
    expect_equal(nrow(audit), nrow(prepared$records))
    expect_true(all(audit$all_agree))
  })
}
validate_urban_sources()
message("UP urban identities, counts, contrasts, clustered covariance and source cells validated.")

test_that("urban wild-cluster interval reproduces with its recorded seed", {
  expected <- read_csv(here("tabs/up_urban_bootstrap.csv"), show_col_types = FALSE)
  expect_equal(urban_bootstrap(d, expected$seed), as_tibble(expected))
})
