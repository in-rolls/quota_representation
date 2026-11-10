library(testthat)
source(here::here("scripts/14_bihar_recontesting.R"))
p <- prepare_bihar_recontesting()
d <- p$base |> filter(included)
r <- read_csv(here("tabs/bihar_recontesting_estimates.csv"), show_col_types = FALSE)

test_that("Bihar joins preserve office and incumbent identities", {
  expect_equal(nrow(p$base), 7945L)
  expect_equal(anyDuplicated(p$base$id), 0L)
  expect_equal(anyDuplicated(p$base$unit_id), 0L)
  expect_true(all(d$prior_quota %in% 0:1))
  expect_true(any(d$prior_quota == 0 & d$prior_female == 1))
  expect_true(all(d$roster_ok & d$baseline_ok & !d$conflicting & !d$ambiguous))
  expect_true(all(d$strict <= d$generous))
  expect_equal(nrow(d), sum(read_csv(here("tabs/bihar_recontesting_coverage.csv"),
    show_col_types = FALSE)$included))
  expect_false(any(c("current_quota", "reserved_woman_2021") %in% names(d)))
  expected <- jsonlite::read_json(here("data/bihar_recontesting/transliteration.json"), simplifyVector = TRUE)
  for (key in names(expected$inputs)) {
    path <- sibling_path(paste0("data/release/", key), "local_elections_bihar")
    expect_identical(digest::digest(path, "sha256", file = TRUE), expected$inputs[[key]])
  }
})

test_that("Bihar identity rules retain sex, age and guardian checks", {
  pairs <- p$pairs
  n <- nrow(pairs)
  distance <- function(x, y) as.numeric(adist(x, y, counts = FALSE, partial = FALSE, fixed = TRUE,
    ignore.case = FALSE, costs = NULL)[cbind(seq_along(x), seq_along(y))])
  # Compute distances in small blocks to avoid an unnecessary all-pairs matrix.
  distances <- function(x, y) unlist(lapply(split(seq_along(x), ceiling(seq_along(x) / 100)),
    function(i) distance(x[i], y[i])), use.names = FALSE)
  name_distance <- distances(pairs$prior_latin, pairs$name_latin)
  relative_distance <- distances(pairs$prior_relative_latin, pairs$relative_latin)
  close <- function(x, y, delta, edits, fraction) {
    !is.na(delta) & (delta == 0 | (pmin(nchar(x), nchar(y)) >= 5 & delta <= edits &
      delta <= fraction * pmax(nchar(x), nchar(y))))
  }
  good <- !(pairs$sex_conflict | pairs$age_conflict)
  strict <- !is.na(name_distance) & !is.na(relative_distance) & name_distance == 0 & relative_distance == 0 & good
  generous <- close(pairs$prior_latin, pairs$name_latin, name_distance, 2, .20) &
    close(pairs$prior_relative_latin, pairs$relative_latin, relative_distance, 3, .25) & good
  expect_equal(length(strict), n)
  expect_identical(strict, pairs$strict)
  expect_identical(generous, pairs$generous)
  expect_false(any(pairs$strict & pairs$contradiction))
  expect_false(bihar_close("ram", "raj"))
  expect_true(bihar_close("rekhadevi", "rekhadevee"))
  ali <- p$lookup |> filter(name == "अली")
  expect_equal(nrow(ali), 1L)
  expect_true(ali$suspect)
  expect_identical(bihar_latin("अली", p$lookup), "ali")
  counts <- pairs |> summarise(strict_check = as.integer(any(strict)),
    generous_check = as.integer(any(generous)), .by = id)
  checked <- d |> left_join(counts, by = "id", relationship = "one-to-one")
  expect_identical(checked$strict, checked$strict_check)
  expect_identical(checked$generous, checked$generous_check)
})

manual_bihar <- function(data, rule, adjusted) {
  y <- data[[rule]]
  x <- if (adjusted) model.matrix(~prior_quota + factor(prior_caste), data) else model.matrix(~prior_quota, data)
  if (adjusted) {
    y <- y - ave(y, data$block)
    x <- apply(x[, -1, drop = FALSE], 2, function(z) z - ave(z, data$block))
  }
  qr <- qr(x)
  x <- x[, sort(qr$pivot[seq_len(qr$rank)]), drop = FALSE]
  bread <- solve(crossprod(x))
  beta <- bread %*% crossprod(x, y)
  residual <- as.numeric(y - x %*% beta)
  scores <- rowsum(x * residual, data$block)
  n <- nrow(x)
  g <- nrow(scores)
  k <- ncol(x) + as.integer(adjusted)
  v <- bread %*% crossprod(scores) %*% bread * g / (g - 1) * (n - 1) / (n - k)
  c(estimate = unname(beta["prior_quota", 1]), standard_error = unname(sqrt(v["prior_quota", "prior_quota"])))
}

test_that("Bihar rates and block-clustered contrasts reproduce independently", {
  for (i in seq_len(nrow(r))) {
    row <- r[i, ]
    sample <- switch(row$sample, `All winners` = d, `Women winners` = filter(d, prior_female == 1),
      `Exact GP names` = filter(d, link_pass == "name" & name_score == 1))
    manual <- manual_bihar(sample, row$rule, row$adjusted)
    expect_equal(row$estimate, unname(manual["estimate"]), tolerance = 1e-10)
    expect_equal(row$standard_error, unname(manual["standard_error"]), tolerance = 1e-10)
    expect_equal(row$n, nrow(sample))
    expect_equal(row$quota_matches, sum(sample[[row$rule]][sample$prior_quota == 1]))
    expect_equal(row$nonquota_matches, sum(sample[[row$rule]][sample$prior_quota == 0]))
    expect_equal(row$conf_low, row$estimate - qt(.975, row$df) * row$standard_error)
  }
})
message("Bihar office links, frozen transliterations, person matching, denominators and clustered estimates validated.")
