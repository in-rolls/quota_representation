suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})
source(here::here("scripts/00_config.R"))

urban_settings <- c("Mumbai", "Delhi", "Chandigarh", "Bengaluru")

appendix_binary <- function(data, setting, specification = "Unadjusted", add = .5) {
  stopifnot(!anyNA(data[c("female", "prior_quota")]),
    all(data$female %in% 0:1), all(data$prior_quota %in% 0:1))
  y1 <- data$female[data$prior_quota == 1]
  y0 <- data$female[data$prior_quota == 0]
  n1 <- length(y1)
  n0 <- length(y0)
  stopifnot(n1 > 1L, n0 > 1L)
  p1 <- mean(y1)
  p0 <- mean(y0)
  wilson <- function(y) suppressWarnings(stats::prop.test(sum(y), length(y), correct = FALSE)$conf.int)
  ci1 <- wilson(y1)
  ci0 <- wilson(y0)
  variance <- metafor::escalc(measure = "RD", ai = sum(y1), bi = n1 - sum(y1),
    ci = sum(y0), di = n0 - sum(y0), add = add, to = "all", addyi = FALSE,
    addvi = TRUE, drop00 = FALSE, vtype = "UB")
  tibble(setting, specification, estimate = p1 - p0,
    standard_error = sqrt(variance$vi),
    conf_low = p1 - p0 - sqrt((p1 - ci1[1])^2 + (ci0[2] - p0)^2),
    conf_high = p1 - p0 + sqrt((p0 - ci0[1])^2 + (ci1[2] - p1)^2),
    n = n1 + n0, df = NA_integer_,
    inference = "Newcombe-Wilson interval; half-cell regularized variance for synthesis")
}

appendix_meta <- function(inputs, correlation = 1, election_weights = "equal", unknown_correlation = 1) {
  stopifnot(election_weights %in% c("equal", "gls"),
    !anyDuplicated(inputs[c("setting", "period")]))
  joint <- if (identical(correlation, "empirical")) {
    read_csv("tabs/short_term_joint_covariance.csv", show_col_types = FALSE)
  } else NULL
  elections <- bind_rows(lapply(split(inputs, inputs$setting), function(d) {
    m <- nrow(d)
    rho <- if (identical(correlation, "lower")) {
      if (m > 1) -1 / (m - 1) else 0
    } else if (identical(correlation, "empirical")) unknown_correlation else correlation
    covariance <- rho * outer(d$standard_error, d$standard_error)
    diag(covariance) <- d$standard_error^2
    if (identical(correlation, "empirical") && d$setting[1] %in% c("Rajasthan", "Uttar Pradesh")) {
      state <- if (d$setting[1] == "Rajasthan") "raj" else "up"
      periods <- strsplit(d$period, "--", fixed = TRUE)
      ids <- vapply(periods, function(years) {
        paste(c(state, substr(years, 3, 4)), collapse = "_")
      }, character(1))
      pairs <- expand.grid(first = ids, second = ids, stringsAsFactors = FALSE)
      index <- match(paste(pairs$first, pairs$second), paste(joint$first, joint$second))
      stopifnot(!anyNA(index))
      covariance <- matrix(joint$covariance[index], m, m)
      stopifnot(isTRUE(all.equal(diag(covariance), d$standard_error^2, tolerance = 1e-10)))
      rho <- NA_real_
    }
    stopifnot(isSymmetric(covariance), min(eigen(covariance, symmetric = TRUE)$values) > -1e-12)
    a <- if (election_weights == "gls") {
      stopifnot(min(eigen(covariance, symmetric = TRUE)$values) > 0)
      precision <- solve(covariance, rep(1, m))
      drop(precision / sum(precision))
    } else rep(1 / m, m)
    d |> mutate(election_weight = a, rho = rho,
      setting_estimate = sum(a * estimate),
      setting_variance = drop(crossprod(a, covariance %*% a)))
  }))
  settings <- elections |>
    group_by(setting) |>
    summarise(comparisons = n(), estimate = first(setting_estimate),
      sum_variance = sum(standard_error^2), sum_se = sum(standard_error),
      rho = first(rho), variance = first(setting_variance), .groups = "drop")
  stopifnot(all(settings$variance > 0), nrow(settings) >= 3)
  fit <- metafor::rma.uni(
    yi = settings$estimate, vi = unname(settings$variance), method = "REML", test = "adhoc",
    control = list(threshold = 1e-12, maxiter = 10000)
  )
  settings$weight <- as.numeric(weights(fit)) / 100
  list(
    settings = settings,
    elections = elections,
    summary = tibble(
      estimate = as.numeric(coef(fit)), standard_error = fit$se,
      conf_low = fit$ci.lb, conf_high = fit$ci.ub, settings = fit$k,
      tau_squared = fit$tau2, q = fit$QE, i_squared = fit$I2,
      df = fit$k - 1, method = "REML, modified Knapp-Hartung t(k-1)"
    )
  )
}

appendix_rural_gap <- function(settings) {
  stopifnot(all(settings$setting %in% c(urban_settings, "Rajasthan", "Uttar Pradesh", "West Bengal", "Bihar")))
  rural <- as.integer(!settings$setting %in% urban_settings)
  stopifnot(length(unique(rural)) == 2, nrow(settings) > 2)
  fit <- metafor::rma.uni(
    yi = settings$estimate, vi = unname(settings$variance), mods = ~rural,
    method = "REML", test = "adhoc", control = list(threshold = 1e-12, maxiter = 10000)
  )
  tibble(
    estimate = as.numeric(coef(fit)[2]), standard_error = fit$se[2],
    conf_low = fit$ci.lb[2], conf_high = fit$ci.ub[2], p_value = fit$pval[2],
    urban_mean = as.numeric(coef(fit)[1]), rural_mean = sum(coef(fit)),
    urban_settings = sum(rural == 0), rural_settings = sum(rural == 1),
    tau_squared = fit$tau2, df = fit$k - 2,
    method = "REML rural indicator, modified Knapp-Hartung t(k-2)"
  )
}

appendix_hc2 <- function(data, formula, setting, specification) {
  model <- lm(formula, data = data)
  estimate <- unname(coef(model)["prior_quota"])
  se <- sqrt(sandwich::vcovHC(model, type = "HC2")["prior_quota", "prior_quota"])
  critical <- qt(0.975, df.residual(model))
  tibble(
    setting = setting, specification = specification, estimate = estimate,
    standard_error = se, conf_low = estimate - critical * se,
    conf_high = estimate + critical * se, n = nobs(model), df = df.residual(model),
    inference = "HC2, residual-df t interval"
  )
}

appendix_cluster <- function(data, formula, specification) {
  model <- lm(formula, data = data)
  stopifnot(nobs(model) == nrow(data))
  covariance <- sandwich::vcovCL(model, cluster = data$cluster, type = "HC1", cadjust = TRUE)
  estimate <- unname(coef(model)["prior_quota"])
  se <- sqrt(covariance["prior_quota", "prior_quota"])
  clusters <- n_distinct(data$cluster)
  critical <- qt(.975, clusters - 1)
  tibble(specification, estimate,
    standard_error = se,
    conf_low = estimate - critical * se, conf_high = estimate + critical * se,
    n = nobs(model), clusters, df = clusters - 1,
    inference = "Block-cluster HC1, t(G-1) interval"
  )
}

appendix_bihar <- function() {
  base <- "../local_elections_bihar/data/release/"
  read <- function(path) arrow::read_parquet(additional_source_path(paste0(base, path)))
  keys <- c("district_code", "block_code", "unit_code")
  links <- read("linkage/mukhiya_2011_2016.parquet")
  prior <- read("2011/mukhiya_reports/winner_records.parquet") |>
    transmute(
      record_id_2011 = record_id, prior_quota = reserved_woman,
      prior_caste = coalesce(reserved_caste, "Unknown"), prior_reservation = reserved_raw
    )
  seats <- read("2016/seats.parquet") |> filter(office == "mukhiya", !code_repeated)
  winners <- read("2016/winners.parquet") |> filter(office == "mukhiya")
  candidates <- read("2016/candidates.parquet") |> filter(office == "mukhiya")
  review <- read_csv(additional_source_path(paste0(base, "2016/winner_list/no_winner_reviewed.csv")),
    col_types = cols(.default = col_character(), winner_sr_no = col_double())
  ) |>
    filter(decision == "accept") |>
    inner_join(candidates, by = keys, relationship = "one-to-many") |>
    filter(sr_no == winner_sr_no)
  stopifnot(all(review$candidate_name == review$winner_name), !anyNA(review$candidate_woman))
  invariant <- review |>
    group_by(across(all_of(keys))) |>
    summarise(n_values = n_distinct(candidate_woman), .groups = "drop")
  stopifnot(all(invariant$n_values == 1))
  review <- review |>
    distinct(across(all_of(keys)), .keep_all = TRUE) |>
    select(all_of(keys), reviewed_female = candidate_woman)
  listed <- read("2016/winner_list/winner_list_records.parquet") |>
    filter(!unit_repeated, label_matches_frame) |>
    select(all_of(keys), listed_female = candidate_woman, listed_reservation = reserved_raw)
  d <- links |>
    left_join(prior, by = "record_id_2011", relationship = "one-to-one") |>
    left_join(seats |> select(all_of(keys), reserved_raw, status), by = keys, relationship = "one-to-one") |>
    left_join(winners |> select(all_of(keys), source_female = candidate_woman, tied, winner_basis),
      by = keys, relationship = "one-to-one"
    ) |>
    left_join(review, by = keys, relationship = "one-to-one") |>
    left_join(listed, by = keys, relationship = "one-to-one") |>
    mutate(
      reservation = coalesce(reserved_raw, listed_reservation),
      current_quota = if_else(is.na(reservation), NA_integer_, as.integer(grepl("महिला", reservation))),
      female = coalesce(
        source_female, reviewed_female,
        if_else(status == "no_record", listed_female, NA_real_)
      ),
      outcome_source = case_when(
        !is.na(source_female) ~ "Candidate result",
        !is.na(reviewed_female) ~ "Reviewed vote dominance",
        status == "no_record" & !is.na(listed_female) ~ "Official winner list",
        TRUE ~ "Missing"
      ),
      cluster = paste(district_code, block_code, sep = "_")
    )
  stopifnot(
    nrow(d) == nrow(links), !anyDuplicated(d$record_id_2011),
    !anyDuplicated(d[keys]), all(na.omit(d$female) %in% 0:1),
    all(d$winner_basis[d$tied %in% TRUE] == "lot")
  )
  write_csv(d, "data/additional_evidence/bihar_2011_2016.csv")
  eligible <- d |> filter(prior_quota %in% 0:1, current_quota == 0)
  observed <- eligible |> filter(!is.na(female))
  arms <- eligible |>
    group_by(prior_quota) |>
    summarise(
      eligible = n(), observed = sum(!is.na(female)), women = sum(female, na.rm = TRUE),
      share = mean(female, na.rm = TRUE), .groups = "drop"
    )
  estimates <- bind_rows(
    appendix_cluster(observed, female ~ prior_quota, "Unadjusted"),
    appendix_cluster(
      observed, female ~ prior_quota + factor(cluster) + factor(prior_caste),
      "Block and prior caste"
    ),
    appendix_cluster(filter(observed, name_score == 1), female ~ prior_quota, "Name score = 1"),
    appendix_cluster(
      filter(observed, outcome_source == "Candidate result"), female ~ prior_quota,
      "Candidate results only"
    )
  )
  a0 <- filter(arms, prior_quota == 0)
  a1 <- filter(arms, prior_quota == 1)
  bounds <- tibble(
    low = a1$women / a1$eligible - (a0$women + a0$eligible - a0$observed) / a0$eligible,
    high = (a1$women + a1$eligible - a1$observed) / a1$eligible - a0$women / a0$eligible
  )
  write_csv(arms, "tabs/additional_bihar_counts.csv")
  write_csv(estimates, "tabs/additional_bihar_persistence.csv")
  write_csv(bounds, "tabs/additional_bihar_bounds.csv")
  list(data = d, arms = arms, estimates = estimates, bounds = bounds)
}

appendix_delhi <- function() {
  d <- arrow::read_parquet(additional_source_path(
    "../local_elections_delhi/data/release/persistence/wards_2007_2012.parquet"
  ))
  stopifnot(nrow(d) == 272L, !anyDuplicated(d$ward_no),
    setequal(d$ward_no, as.character(1:272)), all(d$ward_no == d$prior_ward_no),
    all(d$year == 2012L), all(d$prior_year == 2007L))
  d |> transmute(
    ward = ward_no, name_2007 = prior_ward_name, name_2012 = ward_name,
    reservation_2007 = prior_reserved_raw, reservation_2012 = reserved_raw,
    prior_quota = as.integer(prior_reserved_woman), current_quota = as.integer(reserved_woman),
    prior_sc = as.integer(prior_reserved_caste == "SC"), current_sc = as.integer(reserved_caste == "SC"),
    corporation = body, gender = winner_woman_raw, female = as.numeric(winner_woman),
    decision = winner_woman_decision, outcome_source = winner_woman_source
  )
}

appendix_chandigarh <- function() {
  d <- arrow::read_parquet(additional_source_path(
    "../local_elections/data/chandigarh/release/ward_panel_2006_2011.parquet"
  ))
  stopifnot(nrow(d) == 26L, !anyDuplicated(d$ward_no),
    setequal(d$ward_no, as.character(1:26)))
  d |> transmute(
    ward = ward_no, prior_quota = as.integer(reserved_woman_2006),
    current_quota = as.integer(reserved_woman_2011),
    prior_sc = as.integer(reserved_caste_2006 == "SC"),
    current_sc = as.integer(reserved_caste_2011 == "SC"),
    female = as.numeric(winner_woman_from_honorific_2011), is_eligible
  )
}

appendix_chandigarh_later <- function() {
  d <- arrow::read_parquet(additional_source_path(
    "../local_elections/data/chandigarh/release/ward_panel_2011_2016.parquet"
  ))
  stopifnot(nrow(d) == 26L, !anyDuplicated(d$ward_no), setequal(d$ward_no, as.character(1:26)))
  d |> transmute(ward = ward_no,
    prior_quota = as.integer(reserved_woman_2011), current_quota = as.integer(reserved_woman_2016),
    earlier_quota = as.integer(reserved_woman_2006),
    prior_sc = as.integer(reserved_caste_2011 == "SC"),
    current_sc = as.integer(reserved_caste_2016 == "SC"),
    female = as.numeric(winner_woman_from_honorific_2016), is_eligible)
}

appendix_bengaluru <- function() {
  d <- arrow::read_parquet(additional_source_path(
    "../local_elections/data/karnataka/build/urban_expansion/ward_panel_2010_2015.parquet"
  ))
  stopifnot(nrow(d) == 198L, !anyDuplicated(d$ward_no), setequal(d$ward_no, as.character(1:198)))
  d |> transmute(ward = ward_no,
    prior_quota = as.integer(reserved_woman_2010), current_quota = as.integer(reserved_woman_2015),
    prior_caste = reserved_caste_2010, current_caste = reserved_caste_2015,
    female = as.numeric(winner_woman_2015),
    outcome_conflict = winner_woman_source_conflict_2015,
    quota_conflict = reserved_woman_source_conflict_2010 | reserved_woman_source_conflict_2015,
    is_eligible = current_quota == 0L)
}

build_additional_evidence <- function(root = getwd()) {
  old <- setwd(normalizePath(root))
  on.exit(setwd(old), add = TRUE)
  source("scripts/00_config.R", local = TRUE)
  manifest <- read_csv("data/additional_evidence/source_manifest.csv", show_col_types = FALSE)
  for (i in seq_len(nrow(manifest))) {
    path <- additional_source_path(manifest$path[i])
    stopifnot(file.exists(path))
    if (digest::digest(file = path, algo = "sha256") != manifest$sha256[i]) {
      stop("Appendix input changed: ", path)
    }
  }
  generated_inputs <- c(
    "tabs/model_inference.csv", "tabs/short_term_joint_covariance.csv",
    "tabs/up_urban_estimates.csv", "tabs/up_urban_bootstrap.csv",
    "wb/data/selection_frame.csv", "wb/tabs/selection_adjustment_comparison.csv",
    "wb/tabs/selection_missing_outcome_bounds.csv", "wb/tabs/selection_missing_outcome_arms.csv"
  )
  input_paths <- c(manifest$path, generated_inputs)
  write_csv(tibble(
    path = input_paths,
    kind = c(rep("source", nrow(manifest)), rep("generated", length(generated_inputs))),
    sha256 = vapply(input_paths, function(path) {
      digest::digest(file = additional_source_path(path), algo = "sha256")
    }, character(1))
  ), "tabs/additional_input_provenance.csv")
  mumbai <- arrow::read_parquet(additional_source_path(
    "../local_elections/data/maharashtra/release/mumbai/bhavnani_comparison_1997_2002.parquet"
  )) |>
    filter(is_eligible) |>
    transmute(ward = ward_no, prior_quota = as.numeric(prior_reserved_woman),
      female = as.numeric(winner_woman), current_quota = as.numeric(reserved_woman), group = groups_raw)
  stopifnot(!anyDuplicated(mumbai$ward), all(mumbai$current_quota == 0),
    all(mumbai$prior_quota %in% 0:1), all(mumbai$female %in% 0:1),
    all(mumbai$group == ifelse(mumbai$prior_quota == 1, "2_RN", "1_NN")))

  delhi <- appendix_delhi()
  delhi_open <- delhi |> filter(current_quota == 0, !is.na(female))
  chandigarh <- appendix_chandigarh()
  chandigarh_open <- chandigarh |> filter(is_eligible, !is.na(female))
  chandigarh_later <- appendix_chandigarh_later()
  bengaluru <- appendix_bengaluru()
  bengaluru_open <- bengaluru |> filter(is_eligible, !is.na(female))
  urban_frames <- list(
    "Mumbai 1997--2002" = mumbai,
    "Delhi 2007--2012" = delhi |> filter(current_quota == 0),
    "Chandigarh 2006--2011" = chandigarh |> filter(is_eligible),
    "Chandigarh 2011--2016" = chandigarh_later |> filter(is_eligible),
    "Bengaluru 2010--2015" = bengaluru |> filter(is_eligible)
  )
  urban_observed <- lapply(urban_frames, function(d) filter(d, !is.na(female)))
  unadjusted <- bind_rows(lapply(names(urban_observed), function(city) {
    appendix_binary(urban_observed[[city]], city)
  }))
  estimates <- bind_rows(
    unadjusted,
    appendix_hc2(chandigarh_open, female ~ prior_quota + prior_sc + current_sc,
      "Chandigarh 2006--2011", "Prior and current caste"),
    appendix_binary(chandigarh_later |> filter(is_eligible, earlier_quota == 0),
      "Chandigarh 2011--2016", "Exclude 2006 quota exposure"),
    appendix_hc2(bengaluru_open, female ~ prior_quota + factor(prior_caste) + factor(current_caste),
      "Bengaluru 2010--2015", "Prior and current caste"),
    appendix_binary(bengaluru_open |> filter(prior_caste == current_caste),
      "Bengaluru 2010--2015", "Unchanged caste category"),
    appendix_binary(bengaluru_open |> filter(!quota_conflict),
      "Bengaluru 2010--2015", "Exclude conflicting quota codes"),
    appendix_hc2(
      delhi_open, female ~ prior_quota + prior_sc + factor(corporation),
      "Delhi 2007--2012", "Prior SC and corporation"
    ),
    appendix_hc2(
      filter(delhi_open, prior_sc == current_sc), female ~ prior_quota,
      "Delhi 2007--2012", "Unchanged SC category"
    )
  )
  write_csv(estimates, "tabs/additional_urban_persistence.csv")
  arms <- bind_rows(urban_observed, .id = "setting") |>
    group_by(setting, prior_quota) |>
    summarise(n = n(), women = sum(female), share = mean(female), .groups = "drop")
  write_csv(arms, "tabs/additional_urban_persistence_counts.csv")
  eligible_arms <- bind_rows(urban_frames, .id = "setting") |>
    count(setting, prior_quota, name = "eligible")
  bihar <- appendix_bihar()
  wb <- read_csv("wb/tabs/selection_adjustment_comparison.csv", show_col_types = FALSE)
  wb_arms <- read_csv("wb/tabs/selection_missing_outcome_arms.csv", show_col_types = FALSE)
  bounds <- read_csv("wb/tabs/selection_missing_outcome_bounds.csv", show_col_types = FALSE)
  frame <- read_csv("wb/data/selection_frame.csv", show_col_types = FALSE)
  main <- read_csv("tabs/model_inference.csv", show_col_types = FALSE) |>
    filter(sample == "full", fixed_effects, variance == "cluster", grepl("^treat_", contrast)) |>
    transmute(
      setting = if_else(state == "raj", "Rajasthan", "Uttar Pradesh"),
      period = recode(period,
        "05_10" = "2005--2010", "10_15" = "2010--2015",
        "15_20" = "2015--2020", "15_21" = "2015--2021"
      ),
      estimate, standard_error, conf_low = bootstrap_low, conf_high = bootstrap_high, n,
      evidence = "Main paper: election winners", inference = "Wild cluster bootstrap, 9999 draws"
    )
  stopifnot(nrow(main) == 6, !anyNA(main$conf_low), !anyNA(main$conf_high))
  wb_primary <- wb |> filter(model == "No adjustment", se_type == "HC2")
  forest <- bind_rows(
    main,
    estimates |> filter(specification == "Unadjusted") |>
      transmute(
        period = sub("^[^ ]+ ", "", setting), setting = sub(" .*", "", setting),
        estimate, standard_error, conf_low, conf_high, n, evidence = "Urban election winners", inference
      ),
    bihar$estimates |> filter(specification == "Unadjusted") |> transmute(
      setting = "Bihar", period = "2011--2016 (two-term regime)", estimate, standard_error, conf_low, conf_high, n,
      evidence = "Two-term quota regime", inference
    )
  )
  write_csv(forest, "tabs/additional_persistence_comparison.csv")

  fmt <- function(x) formatC(x, digits = 1, format = "f")
  integer <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  interval <- function(lo, hi) paste0("[", fmt(lo), ", ", fmt(hi), "]")
  table <- function(body, headers, path, alignment) {
    writeLines(c(
      paste0("\\begin{tabular}{", alignment, "}"), "\\toprule",
      paste0(paste(headers, collapse = " & "), " \\\\"), "\\midrule",
      apply(body, 1, function(x) paste0(paste(x, collapse = " & "), " \\\\")),
      "\\bottomrule", "\\end{tabular}"
    ), path)
  }
  published <- read_csv("data/additional_evidence/wb_published_estimates.csv", show_col_types = FALSE) |>
    mutate(conf_low = estimate - qnorm(.975) * se, conf_high = estimate + qnorm(.975) * se)
  meta_inputs <- bind_rows(
    forest |> filter(setting != "Bihar") |> select(setting, period, estimate, standard_error),
    published |> filter(row_number() %in% 1:2) |> transmute(
      setting = "West Bengal", period = paste(exposure, outcome_year, sep = " to "),
      estimate, standard_error = se
    )
  )
  write_csv(meta_inputs, "tabs/additional_meta_inputs.csv")
  primary_meta <- appendix_meta(meta_inputs)
  write_csv(primary_meta$settings, "tabs/additional_meta_settings.csv")
  meta_results <- primary_meta$summary |> mutate(specification = "Primary: maximum within-setting correlation")
  gap_results <- appendix_rural_gap(primary_meta$settings) |>
    mutate(specification = "Primary: maximum within-setting correlation")
  add_meta <- function(data, label, correlation = 1) {
    synthesis <- appendix_meta(data, correlation)
    meta_results <<- bind_rows(meta_results, synthesis$summary |>
      mutate(specification = label))
    gap_results <<- bind_rows(gap_results, appendix_rural_gap(synthesis$settings) |>
      mutate(specification = label))
  }
  add_meta(meta_inputs, "Within-setting correlation = 0", 0)
  add_meta(meta_inputs, "Within-setting correlation = 0.5", .5)
  add_meta(meta_inputs, "Minimum feasible within-setting correlation", "lower")
  add_meta(meta_inputs, "Estimated covariance in Rajasthan and UP", "empirical")
  stronger_variance <- bind_rows(lapply(names(urban_observed), function(city) {
    appendix_binary(urban_observed[[city]], city, add = 1)
  })) |> transmute(period = sub("^[^ ]+ ", "", setting), setting = sub(" .*", "", setting),
    estimate, standard_error)
  add_meta(bind_rows(meta_inputs |> filter(!setting %in% urban_settings), stronger_variance),
    "Urban variance correction = 1")
  add_meta(bind_rows(meta_inputs, forest |> filter(setting == "Bihar") |>
    select(setting, period, estimate, standard_error)), "Include Bihar's two-election regime")
  for (setting in unique(meta_inputs$setting)) {
    add_meta(filter(meta_inputs, .data$setting != .env$setting), paste("Omit", setting))
  }
  add_meta(meta_inputs |> group_by(setting) |> slice_head(n = 1) |> ungroup(), "Earliest comparison per setting")
  add_meta(meta_inputs |> group_by(setting) |> slice_tail(n = 1) |> ungroup(), "Latest comparison per setting")
  write_csv(meta_results, "tabs/additional_meta_sensitivity.csv")
  write_csv(gap_results, "tabs/additional_rural_urban.csv")
  pooling_methods <- tibble(
    specification = c("Reference: equal weights, all correlations = 1",
      "Equal weights; unknown correlation = 1", "Equal weights; unknown correlation = 0.5",
      "Equal weights; unknown correlation = 0", "GLS weights; unknown correlation = 0.5",
      "GLS weights; unknown correlation = 0"),
    election_weights = c(rep("equal", 4), "gls", "gls"),
    unknown_correlation = c(1, 1, .5, 0, .5, 0)
  )
  pooling_fits <- lapply(seq_len(nrow(pooling_methods)), function(i) {
    appendix_meta(meta_inputs, correlation = if (i == 1) 1 else "empirical",
      election_weights = pooling_methods$election_weights[i],
      unknown_correlation = pooling_methods$unknown_correlation[i])
  })
  pooling_results <- bind_rows(lapply(seq_along(pooling_fits), function(i) {
    synthesis <- pooling_fits[[i]]
    gap <- appendix_rural_gap(synthesis$settings)
    bind_cols(pooling_methods[i, ], synthesis$summary |> select(-method),
      gap |> transmute(gap_estimate = estimate, gap_standard_error = standard_error,
        gap_low = conf_low, gap_high = conf_high, gap_p = p_value,
        gap_tau_squared = tau_squared, gap_df = df))
  }))
  write_csv(pooling_results, "tabs/additional_pooling_methods.csv")
  write_csv(bind_rows(lapply(seq_along(pooling_fits), function(i) {
    pooling_fits[[i]]$elections |> mutate(specification = pooling_methods$specification[i])
  })), "tabs/additional_pooling_election_weights.csv")
  table(
    pooling_results |> transmute(
      Weights = c("Equal (reference)", "Equal", "Equal", "Equal", "GLS", "GLS"),
      Correlation = c("1 everywhere", "1", "0.5", "0", "0.5", "0"),
      Pooled = paste(fmt(100 * estimate), interval(100 * conf_low, 100 * conf_high)),
      Gap = paste(fmt(100 * gap_estimate), interval(100 * gap_low, 100 * gap_high)),
      P = format_p_value(gap_p)),
    c("Election weights", "Assumed correlation", "Pooled [95\\% interval]", "Rural $-$ urban [95\\% interval]", "$p$"),
    "tabs/additional_pooling_methods.tex", "lcrrr"
  )
  previous_inputs <- meta_inputs |> filter(setting != "Bengaluru", !(setting == "Chandigarh" & period == "2011--2016"))
  previous_hc2 <- bind_rows(lapply(names(urban_observed)[1:3], function(city) {
    appendix_hc2(urban_observed[[city]], female ~ prior_quota, city, "Unadjusted")
  })) |> transmute(period = sub("^[^ ]+ ", "", setting), setting = sub(" .*", "", setting),
    estimate, standard_error)
  expansion_inputs <- list(
    "Previous data and HC2 inference" = bind_rows(previous_inputs |> filter(!setting %in% urban_settings), previous_hc2),
    "Previous data; regularized urban variances" = previous_inputs,
    "Add Chandigarh 2011--2016" = meta_inputs |> filter(setting != "Bengaluru"),
    "Add Bengaluru 2010--2015" = meta_inputs |> filter(!(setting == "Chandigarh" & period == "2011--2016")),
    "All additional data" = meta_inputs
  )
  expansion <- bind_rows(lapply(names(expansion_inputs), function(label) {
    synthesis <- appendix_meta(expansion_inputs[[label]])
    appendix_rural_gap(synthesis$settings) |>
      mutate(specification = label, pooled_estimate = synthesis$summary$estimate,
        pooled_low = synthesis$summary$conf_low, pooled_high = synthesis$summary$conf_high)
  }))
  write_csv(expansion, "tabs/additional_urban_expansion.csv")
  missing_bounds <- bind_rows(lapply(names(urban_frames), function(city) {
    d <- urban_frames[[city]]
    if (!anyNA(d$female)) return(NULL)
    index <- which(is.na(d$female))
    completions <- expand.grid(rep(list(0:1), length(index)))
    bind_rows(lapply(seq_len(nrow(completions)), function(i) {
      completed <- d
      completed$female[index] <- as.numeric(completions[i, ])
      row <- appendix_binary(completed, city)
      replaced <- meta_inputs
      selected <- replaced$setting == sub(" .*", "", city) & replaced$period == sub("^[^ ]+ ", "", city)
      stopifnot(sum(selected) == 1L)
      replaced$estimate[selected] <- row$estimate
      replaced$standard_error[selected] <- row$standard_error
      gap <- appendix_rural_gap(appendix_meta(replaced)$settings)
      row |> mutate(completion = paste(paste(d$ward[index], completed$female[index], sep = "="), collapse = ";"),
        gap_estimate = gap$estimate, gap_low = gap$conf_low, gap_high = gap$conf_high, gap_p = gap$p_value)
    }))
  }))
  write_csv(missing_bounds, "tabs/additional_urban_missing_outcomes.csv")
  completion_rows <- split(seq_len(nrow(missing_bounds)), missing_bounds$setting)
  joint_completions <- expand.grid(completion_rows)
  joint_bounds <- bind_rows(lapply(seq_len(nrow(joint_completions)), function(i) {
    rows <- missing_bounds[as.integer(joint_completions[i, ]), ]
    replaced <- meta_inputs
    for (j in seq_len(nrow(rows))) {
      selected <- paste(replaced$setting, replaced$period) == rows$setting[j]
      stopifnot(sum(selected) == 1L)
      replaced$estimate[selected] <- rows$estimate[j]
      replaced$standard_error[selected] <- rows$standard_error[j]
    }
    appendix_rural_gap(appendix_meta(replaced)$settings) |>
      mutate(completion = paste(paste(rows$setting, rows$completion, sep = ": "), collapse = "; "))
  }))
  write_csv(joint_bounds, "tabs/additional_urban_joint_missing_outcomes.csv")
  table(
    gap_results |> transmute(
      Specification = specification, Estimate = fmt(100 * estimate),
      CI = interval(100 * conf_low, 100 * conf_high), P = format_p_value(p_value)
    ),
    c("Specification", "Rural $-$ urban", "95\\% interval", "$p$"),
    "tabs/additional_rural_urban.tex", "lrrr"
  )
  table(
    meta_results |> transmute(
      Specification = specification, Settings = settings, Estimate = fmt(100 * estimate),
      CI = interval(100 * conf_low, 100 * conf_high)
    ),
    c("Specification", "Settings", "Difference", "95\\% interval"),
    "tabs/additional_meta_sensitivity.tex", "lrrr"
  )
  up_urban <- read_csv("tabs/up_urban_estimates.csv", show_col_types = FALSE)
  up_primary <- up_urban |> filter(specification == "All linked heads")
  up_adjusted <- up_urban |> filter(specification == "District, office type and prior caste adjusted")
  up_boot <- read_csv("tabs/up_urban_bootstrap.csv", show_col_types = FALSE)
  stopifnot(nrow(up_primary) == 1L, nrow(up_adjusted) == 1L, nrow(up_boot) == 1L,
    isTRUE(all.equal(up_primary$estimate, up_boot$estimate)))
  samples <- bind_rows(
    arms |> left_join(eligible_arms, by = c("setting", "prior_quota"), relationship = "one-to-one") |>
      transmute(setting, prior_quota, eligible, observed = n, women, share),
    tibble(setting = "UP urban 2012--2017", prior_quota = c(0L, 1L),
      eligible = c(up_primary$nonquota_n, up_primary$quota_n),
      observed = eligible, women = c(up_primary$nonquota_women, up_primary$quota_women),
      share = women / observed),
    bihar$arms |> mutate(setting = "Bihar 2011--2016"),
    wb_arms |> transmute(
      setting = "Birbhum 1998--2006", prior_quota = q98,
      eligible = frame_gps, observed = observed_gps, women = observed_women, share = observed_share
    )
  )
  write_csv(samples, "tabs/additional_samples.csv")
  table(
    samples |> transmute(
      Setting = setting, Prior = if_else(prior_quota == 1, "Reserved", "Not reserved"),
      Eligible = integer(eligible), Observed = integer(observed), Women = integer(women), Share = fmt(100 * share)
    ), c("Setting", "Prior quota", "Eligible", "Observed", "Women", "Women (\\%)"),
    "tabs/additional_samples.tex", "llrrrr"
  )
  sensitivities <- bind_rows(
    estimates |> filter(!grepl("Mumbai", setting)),
    bihar$estimates |> mutate(setting = "Bihar"),
    wb |> filter(se_type == "HC2") |> transmute(
      setting = "Birbhum", specification = model, estimate, conf_low = conf.low, conf_high = conf.high,
      n = n
    )
  ) |> arrange(setting)
  table(
    sensitivities |> transmute(
      Setting = setting, Specification = recode(specification,
        "1998 caste strata and block" = "Prior caste and block",
        "Also condition on 2003 caste quotas" = "Also current caste",
        "No adjustment" = "Unadjusted"
      ),
      Estimate = fmt(100 * estimate), CI = interval(100 * conf_low, 100 * conf_high), N = integer(n)
    ), c("Setting", "Specification", "Difference", "95\\% interval", "$N$"),
    "tabs/additional_sensitivity.tex", "llrrr"
  )
  macros <- character()
  macro <- function(name, value) {
    macros <<- c(macros, paste0("\\newcommand{\\", name, "}{", value, "}"))
  }
  macro("appUpUrbanN", integer(up_primary$n))
  macro("appUpUrbanEstimate", fmt(100 * up_primary$estimate))
  macro("appUpUrbanInterval", interval(100 * up_boot$conf_low, 100 * up_boot$conf_high))
  macro("appUpUrbanQuotaShare", fmt(100 * up_primary$quota_rate))
  macro("appUpUrbanNonquotaShare", fmt(100 * up_primary$nonquota_rate))
  macro("appUpUrbanAdjusted", fmt(100 * up_adjusted$estimate))
  macro("appUpUrbanAdjustedInterval", interval(100 * up_adjusted$conf_low, 100 * up_adjusted$conf_high))
  macro("appUpUrbanDivisions", up_primary$clusters)
  gap <- gap_results[1, ]
  macro("appRuralGap", fmt(100 * gap$estimate))
  macro("appRuralGapInterval", interval(100 * gap$conf_low, 100 * gap$conf_high))
  macro("appRuralGapP", format_p_value(gap$p_value, latex = FALSE))
  macro("appRuralMean", fmt(100 * gap$rural_mean))
  macro("appUrbanMean", fmt(100 * gap$urban_mean))
  macro("appRuralGapPoolingLow", fmt(100 * min(pooling_results$gap_estimate)))
  macro("appRuralGapPoolingHigh", fmt(100 * max(pooling_results$gap_estimate)))
  macro("appRuralGapMissingLow", fmt(100 * min(joint_bounds$estimate)))
  macro("appRuralGapMissingHigh", fmt(100 * max(joint_bounds$estimate)))
  macro("appMetaEstimate", fmt(100 * primary_meta$summary$estimate))
  macro("appMetaInterval", interval(100 * primary_meta$summary$conf_low, 100 * primary_meta$summary$conf_high))
  macro("appMetaSettings", primary_meta$summary$settings)
  macro("appMetaTau", fmt(100 * sqrt(primary_meta$summary$tau_squared)))
  omitted <- meta_results |> filter(grepl("^Omit", specification))
  macro("appMetaOmitRange", interval(100 * min(omitted$estimate), 100 * max(omitted$estimate)))
  macro("appMetaOmitLow", fmt(100 * min(omitted$estimate)))
  macro("appMetaOmitHigh", fmt(100 * max(omitted$estimate)))
  for (city in urban_settings) {
    omitted_city <- gap_results |> filter(specification == paste("Omit", city))
    stopifnot(nrow(omitted_city) == 1L)
    macro(paste0("appRuralGapOmit", city), fmt(100 * omitted_city$estimate))
  }
  city_macros <- c("Mumbai 1997--2002" = "Mumbai", "Delhi 2007--2012" = "Delhi",
    "Chandigarh 2006--2011" = "Chandigarh", "Chandigarh 2011--2016" = "ChandigarhLater",
    "Bengaluru 2010--2015" = "Bengaluru")
  for (label in names(city_macros)) {
    prefix <- paste0("app", city_macros[[label]])
    row <- estimates |> filter(setting == label, specification == "Unadjusted")
    stopifnot(nrow(row) == 1L)
    macro(paste0(prefix, "Estimate"), fmt(100 * row$estimate))
    macro(paste0(prefix, "Interval"), interval(100 * row$conf_low, 100 * row$conf_high))
    macro(paste0(prefix, "N"), row$n)
    city_arms <- arms |> filter(setting == label)
    for (arm in 0:1) {
      arm_name <- if (arm == 0) "Control" else "Treated"
      macro(paste0(prefix, arm_name, "Share"), fmt(100 * city_arms$share[city_arms$prior_quota == arm]))
      macro(paste0(prefix, arm_name, "Women"), city_arms$women[city_arms$prior_quota == arm])
      macro(paste0(prefix, arm_name, "N"), city_arms$n[city_arms$prior_quota == arm])
    }
  }
  bengaluru_bounds <- missing_bounds |> filter(setting == "Bengaluru 2010--2015")
  macro("appBengaluruBounds", interval(100 * min(bengaluru_bounds$estimate), 100 * max(bengaluru_bounds$estimate)))
  macro("appBengaluruMissing", sum(is.na(bengaluru$female) & bengaluru$is_eligible))
  macro("appBengaluruEligible", sum(bengaluru$is_eligible))
  macro("appChandigarhLinked", nrow(chandigarh))
  macro("appDelhiLinked", nrow(delhi))
  macro("appDelhiOpen", sum(delhi$current_quota == 0))
  macro("appDelhiUnknown", sum(delhi$current_quota == 0 & is.na(delhi$female)))
  macro("appWbControlShare", fmt(100 * wb_arms$observed_share[wb_arms$q98 == 0]))
  macro("appWbTreatedShare", fmt(100 * wb_arms$observed_share[wb_arms$q98 == 1]))
  macro("appWbControlWomen", wb_arms$observed_women[wb_arms$q98 == 0])
  macro("appWbTreatedWomen", wb_arms$observed_women[wb_arms$q98 == 1])
  macro("appWbControlN", wb_arms$observed_gps[wb_arms$q98 == 0])
  macro("appWbTreatedN", wb_arms$observed_gps[wb_arms$q98 == 1])
  macro("appWbBaselineEarly", fmt(100 * published$baseline[published$outcome_year == 2003]))
  macro("appWbBaselineLater", fmt(100 * unique(published$baseline[published$outcome_year == 2008])))
  macro("appWbEstimate", fmt(100 * wb_primary$estimate))
  macro("appWbInterval", interval(100 * wb_primary$conf.low, 100 * wb_primary$conf.high))
  macro("appWbRoster", nrow(frame))
  macro("appWbHistory", sum(frame$history_eligible))
  macro("appWbOpen", bounds$frame_gps)
  macro("appWbObserved", bounds$observed_gps)
  macro("appWbMissing", bounds$frame_gps - bounds$observed_gps)
  macro("appWbBounds", interval(100 * bounds$lower_difference, 100 * bounds$upper_difference))
  bp <- bihar$estimates |> filter(specification == "Unadjusted")
  ba <- bihar$estimates |> filter(specification == "Block and prior caste")
  macro("appBiharEstimate", fmt(100 * bp$estimate))
  macro("appBiharInterval", interval(100 * bp$conf_low, 100 * bp$conf_high))
  macro("appBiharAdjusted", fmt(100 * ba$estimate))
  macro("appBiharAdjustedInterval", interval(100 * ba$conf_low, 100 * ba$conf_high))
  macro("appBiharN", integer(bp$n))
  macro("appBiharLinked", integer(nrow(bihar$data)))
  macro("appBiharEligible", integer(sum(bihar$arms$eligible)))
  macro("appBiharMissing", sum(bihar$arms$eligible - bihar$arms$observed))
  macro("appBiharBlocks", bp$clusters)
  macro("appBiharUnknownPrior", sum(is.na(bihar$data$prior_quota) & bihar$data$current_quota == 0, na.rm = TRUE))
  macro("appBiharBounds", interval(100 * bihar$bounds$low, 100 * bihar$bounds$high))
  for (i in seq_len(nrow(published))) {
    macro(paste0("appWbPublished", c("A", "B", "C", "D")[i]), fmt(100 * published$estimate[i]))
    macro(
      paste0("appWbPublished", c("A", "B", "C", "D")[i], "Interval"),
      interval(100 * published$conf_low[i], 100 * published$conf_high[i])
    )
  }
  delhi_complete <- delhi |> filter(current_quota == 0)
  completions <- c(0, 1)
  delhi_bounds <- sapply(completions, function(value) {
    y <- coalesce(delhi_complete$female, value)
    mean(y[delhi_complete$prior_quota == 1]) - mean(y[delhi_complete$prior_quota == 0])
  })
  macro("appDelhiBounds", interval(100 * min(delhi_bounds), 100 * max(delhi_bounds)))

  next_election <- bind_rows(
    forest |> filter(setting != "Bihar") |> mutate(
      label = paste(setting, gsub("--", "\u2013", period)), position = seq.int(8 + n() - 1L, 8)
    ),
    up_primary |> transmute(setting = "UP urban", period = "2012--2017", estimate, standard_error,
      conf_low = up_boot$conf_low, conf_high = up_boot$conf_high, n,
      label = "UP urban 2012\u20132017*", position = 7),
    published |> slice(1:2) |> transmute(
      setting = "West Bengal", period = paste(exposure, outcome_year, sep = " to "),
      estimate, standard_error = se, conf_low, conf_high, n,
      label = c("West Bengal 1998\u20132003", "West Bengal 2003\u20132008"), position = c(5, 4)
    )
  ) |> mutate(panel = "Next election", pooled = FALSE)
  cumulative <- read_csv("tabs/model_inference.csv", show_col_types = FALSE) |>
    filter(sample == "full", fixed_effects, variance == "cluster", grepl("-", contrast)) |>
    transmute(
      setting = if_else(state == "raj", "Rajasthan", "Uttar Pradesh"),
      period, contrast, estimate, standard_error,
      conf_low = bootstrap_low, conf_high = bootstrap_high, n,
      label = paste(setting, gsub("-", " vs ", contrast)), position = c(16:13, 11:8)
    )
  histories <- bind_rows(
    cumulative,
    forest |> filter(setting == "Bihar") |> mutate(label = "Bihar 2011\u20132016", position = 6),
    published |> slice(3:4) |> transmute(
      setting = "West Bengal", period = paste(exposure, outcome_year, sep = " to "),
      estimate, standard_error = se, conf_low, conf_high, n,
      label = c("WB: 1998 only to 2008", "WB: 1998 + 2003 to 2008"), position = c(5, 4)
    ),
    wb_primary |> transmute(
      setting = "Birbhum", period = "1998--2006 survey", estimate, standard_error = std.error,
      conf_low = conf.low, conf_high = conf.high, n,
      label = "Birbhum 1998\u20132006", position = 1
    )
  ) |> mutate(panel = "Histories and follow-up", pooled = FALSE)
  write_csv(bind_rows(next_election, histories), "tabs/additional_all_estimates.csv")
  macro("appAllEstimates", nrow(next_election) + nrow(histories))
  writeLines(macros, "tabs/additional_numbers.tex")
  wplot <- bind_rows(
    published |> transmute(estimate, conf_low, conf_high,
      label = paste0("Reserved ", exposure, "\n", outcome_year, " appointment"), position = c(6, 5, 4, 3)
    ),
    wb_primary |> transmute(estimate,
      conf_low = conf.low, conf_high = conf.high,
      label = "Birbhum: reserved 1998\n2006 survey officeholder", position = 1
    )
  )
  write_csv(wplot, "tabs/additional_wb_comparison.csv")
  draw_forest <- function(data, headings, path, height) {
    data <- data |> mutate(
      numbers = paste0(fmt(100 * estimate), "  ", interval(100 * conf_low, 100 * conf_high)),
      sample = if_else(pooled, "", integer(n))
    )
    p <- ggplot(data, aes(x = 100 * estimate, y = position)) +
      annotate("segment",
        x = 0, xend = 0, y = .5, yend = max(data$position) + .4,
        color = FIG_STYLE$reference, linewidth = FIG_STYLE$reference_width, linetype = "dashed"
      ) +
      geom_segment(aes(x = 100 * conf_low, xend = 100 * conf_high, yend = position),
        linewidth = FIG_STYLE$interval_width, color = FIG_STYLE$interval) +
      geom_point(aes(shape = pooled), size = FIG_STYLE$point_size, color = FIG_STYLE$estimate) +
      scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
      geom_text(aes(x = -67, label = label), hjust = 0, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      geom_text(aes(x = 55, label = numbers), hjust = 0, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      geom_text(aes(x = 102, label = sample), hjust = 1, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      geom_text(
        data = headings, aes(x = -67, y = position, label = label),
        inherit.aes = FALSE, hjust = 0, fontface = "bold", size = FIG_STYLE$label_size, family = FIG_STYLE$family
      ) +
      annotate("text", x = 55, y = max(headings$position), label = "Estimate [95% CI]", hjust = 0, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      annotate("text", x = 102, y = max(headings$position), label = "N", hjust = 1, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      scale_x_continuous(breaks = seq(-30, 40, 10), limits = c(-68, 103), expand = c(0, 0)) +
      scale_y_continuous(breaks = NULL, expand = expansion(add = c(.45, .45))) +
      labs(x = "Difference in female officeholding (percentage points)", y = NULL) +
      theme_pub() +
      theme(
        axis.ticks.y = element_blank(),
        plot.margin = margin(5, 0, 5, 0)
      )
    ggsave(path, p, width = FIG_WIDTH_FULL, height = height, device = cairo_pdf)
  }
  draw_forest(
    bind_rows(next_election, primary_meta$summary |> mutate(
      label = paste0("Pooled: ", settings, " settings"), position = 1, n = NA_integer_, pooled = TRUE
    )),
    tibble(position = c(max(next_election$position) + 1.1, 6, 2.2), label = c(
      "Directly elected winners", "Council-selected Pradhans", "Random-effects synthesis"
    )), "figs/additional_persistence.pdf", 6.7
  )
  draw_forest(histories |> filter(!setting %in% c("Rajasthan", "Uttar Pradesh")),
    tibble(position = c(7, 2.2), label = c(
    "Longer exposure or delayed outcome", "Follow-up survey officeholder"
    )), "figs/additional_histories.pdf", 3.2)
  writeLines(trimws(capture.output(sessionInfo()), which = "right"), "data/additional_evidence/sessionInfo.txt")
  message("Persistence appendix: source hashes, samples, tables, macros, and figures generated.")
  invisible(list(mumbai = mumbai, delhi = delhi, chandigarh = chandigarh, urban = estimates, wb = wb, forest = forest))
}

if (sys.nframe() == 0L) build_additional_evidence()
