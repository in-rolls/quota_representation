library(testthat)
source("scripts/12_recontesting.R")
d <- read_parquet(here("data/recontesting_analysis.parquet"))
e <- read_csv(here("tabs/recontesting_estimates.csv"), show_col_types = FALSE)
paths <- recontest_sources()

manual_recontest <- function(data, outcome) {
  x <- model.matrix(~ quota + factor(caste), data)[, -1, drop = FALSE]
  x <- apply(x, 2, function(v) v - ave(v, data$block))
  y <- data[[outcome]] - ave(data[[outcome]], data$block)
  fit <- lm.fit(x, y)
  stopifnot(fit$rank == ncol(x))
  bread <- solve(crossprod(x))
  scores <- rowsum(x * fit$residuals, data$block)
  g <- nrow(scores)
  v <- g / (g - 1) * (nrow(data) - 1) / (nrow(data) - ncol(x) - 1) *
    bread %*% crossprod(scores) %*% bread
  c(estimate = fit$coefficients[1], se = sqrt(v[1, 1]))
}

test_that("normalization preserves Hindi names and fuzzy rules reject short collisions", {
  expect_equal(normalize_person(c("  RAM  LAL ", "राम लाल", "रम लाल")), c("ram lal", "राम लाल", "रम लाल"))
  expect_false(normalize_person("राम") == normalize_person("रम"))
  expect_equal(latin_person("राम लाल"), "ramalala")
  expect_true(close_person("BHUVNESWARI", "BHUVNESHWARI"))
  expect_false(close_person("RAM", "RAJ"))
  expect_false(close_person(NA_character_, "RAM"))
})

test_that("the cohort uses all prior winners and fully open follow-up seats", {
  expect_equal(anyDuplicated(d[c("state", "id")]), 0L)
  for (st in c("raj", "up")) {
    p <- if (st == "raj") read_raj(paths[["raj_panel"]]) else read_up(paths[["up_panel"]])
    observed <- d[d$state == st, ]
    ids <- if (st == "raj") as.character(p$source_id_2015) else p$key_2015
    q <- if (st == "raj") p$treat_2015 else p$women_reserved_2015
    eligible <- if (st == "raj") p$treat_2020 == 0 & p$caste_category_2020 == "GEN" else
      p$women_reserved_2021 == 0 & p$reservation_class_2021 == "general"
    i <- match(observed$id, ids)
    expect_false(anyNA(i))
    expect_equal(nrow(observed), nrow(p))
    expect_equal(unname(observed$quota), unname(q[i]))
    expect_equal(unname(observed$eligible), unname(eligible[i]))
    expect_true(all(observed$eligible[observed$included]))
    expect_true(all(observed$roster_ok[observed$included]))
    expect_false(any(observed$conflict[observed$included]))
    expect_false(any(observed$ambiguous[observed$included]))
    expect_equal(anyDuplicated(observed$gp[observed$included]), 0L)
  }
})

test_that("source candidate rosters independently reproduce match numerators", {
  for (st in c("raj", "up")) {
    raw <- if (st == "raj") read_raj(paths[["raj_candidates"]]) else read_parquet(paths[["up_candidates"]])
    p <- if (st == "raj") read_raj(paths[["raj_panel"]]) else read_up(paths[["up_panel"]])
    rows <- d[d$state == st & d$included, ]
    if (st == "raj") {
      gp <- raw$match_key
      names <- raw$name_of_contesting_candidate
      relatives <- raw$father_husband_of_contesting_candidate
      sex <- raw$candidate_female
      age <- as.numeric(raw$age)
      expect_true(all(raw$gp_event_unique[gp %in% rows$gp]))
      w <- read_raj(paths[["raj_winners"]])
      roster_count <- table(gp)
      expect_equal(as.integer(roster_count[rows$gp]),
        as.integer(w$total_no_of_contesting_candidate[match(rows$gp, w$match_key)]))
    } else {
      gp <- raw$key
      names <- raw$candidate
      relatives <- raw$father_husband
      sex <- ifelse(raw$sex == "महिला", 1L, 0L)
      age <- as.numeric(raw$age)
      expected_gp <- raw$key[match(p$key_2021, paste0("up2021__", raw$id))]
      expect_equal(rows$gp, expected_gp[match(rows$id, p$key_2015)])
      winner_count <- tapply(raw$result == "विजेता", gp, sum)
      expect_true(all(winner_count[rows$gp] == 1L))
    }
    group <- split(seq_along(gp), gp)
    name_norm <- normalize_person(names)
    kin_norm <- normalize_person(relatives)
    name_latin <- latin_person(names)
    kin_latin <- latin_person(relatives)
    results <- vapply(seq_len(nrow(rows)), function(j) {
      ix <- group[[rows$gp[j]]]
      nr <- normalize_person(rows$prior_name[j])
      kr <- normalize_person(rows$prior_relative[j])
      exact <- name_norm[ix] == nr
      exact[is.na(exact)] <- FALSE
      kin_exact <- kin_norm[ix] == kr
      kin_exact[is.na(kin_exact)] <- FALSE
      strict <- if (st == "raj") exact else exact & kin_exact
      nx <- latin_person(rows$prior_name[j])
      kx <- latin_person(rows$prior_relative[j])
      dist <- as.numeric(adist(nx, name_latin[ix]))
      length_ok <- pmin(nchar(nx), nchar(name_latin[ix])) >= 5
      close <- dist == 0 | (length_ok & dist <= 2 & dist / pmax(nchar(nx), nchar(name_latin[ix])) <= .20)
      dist_kin <- as.numeric(adist(kx, kin_latin[ix]))
      kin_close <- dist_kin == 0 | (pmin(nchar(kx), nchar(kin_latin[ix])) >= 5 &
        dist_kin <= 3 & dist_kin / pmax(nchar(kx), nchar(kin_latin[ix])) <= .25)
      compatible <- (is.na(rows$prior_female[j]) | is.na(sex[ix]) | rows$prior_female[j] == sex[ix]) &
        (is.na(age[ix]) | age[ix] >= ifelse(st == "raj", 25, 26))
      c(primary = sum(strict) == 1, name_only = sum(exact) == 1,
        generous = any(close & compatible, na.rm = TRUE),
        fuzzy_supported = any(close & compatible & (st == "raj" | kin_close), na.rm = TRUE))
    }, logical(4))
    for (rule in rownames(results)) expect_equal(rows[[rule]], as.integer(results[rule, ]))
  }
})

test_that("rates and block-clustered estimates reproduce independently", {
  for (i in seq_len(nrow(e))) {
    r <- e[i, ]
    x <- d[d$state == r$state & d$included, ]
    y <- x[[r$rule]]
    expect_equal(nrow(x), r$n)
    expect_equal(sum(y[x$quota == 1]), r$quota_matches)
    expect_equal(sum(y[x$quota == 0]), r$nonquota_matches)
    expect_equal(mean(y[x$quota == 1]), r$quota_rate)
    expect_equal(mean(y[x$quota == 0]), r$nonquota_rate)
    manual <- manual_recontest(x, r$rule)
    expect_equal(unname(manual[1]), r$estimate, tolerance = 1e-10)
    expect_equal(unname(manual[2]), r$standard_error, tolerance = 1e-10)
    expect_equal(r$conf_low, r$estimate - qt(.975, r$df) * r$standard_error)
    expect_equal(r$conf_high, r$estimate + qt(.975, r$df) * r$standard_error)
  }
  for (st in c("raj", "up")) expect_equal(n_distinct(e$n[e$state == st]), 1L)
})
