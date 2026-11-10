library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(arrow)
library(stringi)
library(fixest)
library(ggplot2)
library(here)
source(here("scripts/00_config.R"))

normalize_person <- function(x, compact = FALSE) {
  x <- stri_trans_tolower(stri_trans_nfc(x))
  x <- stri_replace_all_regex(x, "[\\p{P}\\p{Cf}]", "")
  x <- stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
  if (compact) x <- stri_replace_all_fixed(x, " ", "")
  x[x %in% c("", "na", "n/a", "null", "--")] <- NA_character_
  x
}

latin_person <- function(x) {
  normalize_person(stri_trans_general(normalize_person(x), "Any-Latin; Latin-ASCII"), TRUE)
}

close_person <- function(x, y, max_distance = 2L, max_fraction = .20) {
  x <- latin_person(x)
  y <- latin_person(y)
  distance <- stringdist::stringdist(x, y, method = "lv")
  size <- pmax(nchar(x), nchar(y))
  coalesce(distance == 0 | (pmin(nchar(x), nchar(y)) >= 5 &
    distance <= max_distance & distance / size <= max_fraction), FALSE)
}

recontest_sources <- function() {
  c(
    raj_panel = raj_product_path("raj_15_20.parquet"),
    raj_candidates = raj_product_path("candidates_2020_events.parquet"),
    raj_winners = raj_product_path("winners_2020_events.parquet"),
    up_panel = up_path("linkage/gp_panel_2015_2021.parquet"),
    up_candidates = sibling_path("data/build/gp_enriched/gp_head_candidates_2021.parquet")
  )
}

prepare_recontesting <- function(state, paths = recontest_sources()) {
  p <- if (state == "raj") read_raj(paths[["raj_panel"]]) else read_up(paths[["up_panel"]])
  c <- if (state == "raj") read_raj(paths[["raj_candidates"]]) else read_parquet(paths[["up_candidates"]])
  if (state == "raj") {
    w <- read_raj(paths[["raj_winners"]])
    base <- p |> transmute(id = as.character(source_id_2015), gp = match_key_2020,
      block = paste(district_std_2015, samiti_std_2015, sep = "::"),
      quota = treat_2015, caste = caste_category_2015,
      eligible = treat_2020 == 0 & caste_category_2020 == "GEN",
      prior_name = winner_name_2015, prior_relative = NA_character_,
      prior_female = winner_female_2015, next_winner = winner_name_2020)
    roster <- c |> transmute(gp = match_key, event_key,
      candidate_id = paste(event_key, contesting_candidate_serial_no, sep = "::"),
      name = name_of_contesting_candidate, relative = father_husband_of_contesting_candidate,
      female = candidate_female, candidate_age = as.numeric(age), key_ok = candidate_key_unique, event_ok = gp_event_unique)
    coverage <- roster |> summarise(roster_n = n(),
      roster_ok = all(key_ok & event_ok) && n_distinct(event_key) == 1L &&
        !anyNA(normalize_person(name)), .by = gp)
    winners <- w |> summarise(winner_event_ok = n() == 1L && all(winner_key_unique & gp_event_unique),
      declared_n = if (n() == 1L) total_no_of_contesting_candidate else NA_real_, .by = match_key)
    coverage <- coverage |> left_join(winners, by = c("gp" = "match_key"), relationship = "one-to-one") |>
      mutate(roster_ok = roster_ok & winner_event_ok & roster_n == declared_n)
  } else {
    stopifnot(!anyDuplicated(c$id), !anyNA(c$id))
    winner_gp <- c |> transmute(key_2021 = paste0("up2021__", id), gp = key)
    p <- p |> left_join(winner_gp, by = "key_2021", relationship = "many-to-one")
    base <- p |> transmute(id = key_2015, gp,
      block = paste(district_name_2015, block_name_2015, sep = "::"),
      quota = women_reserved_2015, caste = reservation_class_2015,
      eligible = women_reserved_2021 == 0 & reservation_class_2021 == "general",
      prior_name = elected_sarpanch_name_2015, prior_relative = father_husband_2015,
      prior_female = winner_woman_2015, next_winner = candidate_2021)
    roster <- c |> transmute(gp = key, candidate_id = as.character(id), name = candidate,
      relative = father_husband, candidate_age = as.numeric(age), female = case_when(sex == "महिला" ~ 1L, sex == "पुरुष" ~ 0L),
      is_winner = result == "विजेता")
    coverage <- roster |> summarise(roster_n = n(),
      roster_ok = sum(is_winner) == 1L && !anyNA(normalize_person(name)) &&
        !anyNA(normalize_person(relative)),
      recorded_winner = if (sum(is_winner) == 1L) name[is_winner] else NA_character_, .by = gp)
    base <- base |> left_join(coverage |> select(gp, recorded_winner), by = "gp", relationship = "many-to-one")
    base <- base |> mutate(winner_agrees = normalize_person(next_winner) == normalize_person(recorded_winner))
  }
  stopifnot(!anyNA(base$id), !anyDuplicated(base$id), !anyDuplicated(na.omit(base$gp)))
  base <- base |> left_join(coverage, by = "gp", relationship = "one-to-one", na_matches = "never") |>
    mutate(roster_ok = coalesce(roster_ok, FALSE),
      prior_name_norm = normalize_person(prior_name),
      prior_relative_norm = normalize_person(prior_relative),
      source_ok = !is.na(prior_name_norm) & !is.na(quota) & !is.na(caste) & !is.na(block))
  if (state == "up") {
    base <- base |> mutate(roster_ok = roster_ok & coalesce(winner_agrees, FALSE),
      source_ok = source_ok & !is.na(prior_relative_norm))
  }
  eligible <- base |> filter(eligible, roster_ok, source_ok)
  pairs <- eligible |> select(id, gp, prior_name, prior_relative, prior_female) |>
    inner_join(roster, by = "gp", relationship = "one-to-many", na_matches = "never") |>
    mutate(name_exact = coalesce(normalize_person(prior_name) == normalize_person(name), FALSE),
      name_compact = coalesce(normalize_person(prior_name, TRUE) == normalize_person(name, TRUE), FALSE),
      relative_exact = coalesce(normalize_person(prior_relative) == normalize_person(relative), FALSE),
      relative_compact = coalesce(normalize_person(prior_relative, TRUE) == normalize_person(relative, TRUE), FALSE),
      age_conflict = !is.na(candidate_age) & candidate_age < ifelse(state == "raj", 25, 26),
      name_close = close_person(prior_name, name),
      relative_close = close_person(prior_relative, relative, 3L, .25),
      sex_conflict = !is.na(prior_female) & !is.na(female) & prior_female != female,
      primary = if (state == "up") name_exact & relative_exact else name_exact,
      compact = if (state == "up") name_compact & relative_compact else name_compact,
      fuzzy_supported = (if (state == "up") name_close & relative_close else name_close) &
        !sex_conflict & !age_conflict,
      generous = name_close & !sex_conflict & !age_conflict)
  linked <- pairs |> summarise(primary_n = sum(primary), name_n = sum(name_exact), compact_n = sum(compact),
    conflict = any((sex_conflict | age_conflict) & (primary | name_exact | compact)),
    fuzzy_supported_n = sum(fuzzy_supported), generous_n = sum(generous), .by = id) |>
    mutate(ambiguous = primary_n > 1 | name_n > 1 | compact_n > 1,
      usable = !conflict & !ambiguous,
      primary = as.integer(primary_n == 1), name_only = as.integer(name_n == 1),
      compact = as.integer(compact_n == 1),
      fuzzy_supported = as.integer(fuzzy_supported_n > 0), generous = as.integer(generous_n > 0))
  base <- base |> left_join(linked, by = "id", relationship = "one-to-one") |>
    mutate(state, included = coalesce(eligible & roster_ok & source_ok & usable, FALSE))
  list(base = base, pairs = pairs)
}

fit_recontesting <- function(d, rule) {
  d$y <- d[[rule]]
  model <- feols(y ~ quota + i(caste) | block, data = d, vcov = ~block,
    ssc = MODEL_SSC, fixef.rm = "none", notes = FALSE)
  stopifnot(nobs(model) == nrow(d), "quota" %in% names(coef(model)))
  b <- unname(coef(model)["quota"])
  se <- unname(sqrt(diag(vcov(model)))["quota"])
  df <- degrees_freedom(model, "t")
  tibble(state = d$state[1], rule, n = nrow(d),
    quota_n = sum(d$quota == 1), nonquota_n = sum(d$quota == 0),
    quota_matches = sum(d$y[d$quota == 1]), nonquota_matches = sum(d$y[d$quota == 0]),
    quota_rate = mean(d$y[d$quota == 1]), nonquota_rate = mean(d$y[d$quota == 0]),
    estimate = b, standard_error = se, conf_low = b - qt(.975, df) * se,
    conf_high = b + qt(.975, df) * se, p_value = 2 * pt(-abs(b / se), df),
    clusters = n_distinct(d$block), mixed_clusters = sum(tapply(d$quota, d$block, n_distinct) == 2), df)
}

build_recontesting <- function() {
  paths <- recontest_sources()
  prepared <- set_names(map(c("raj", "up"), ~ prepare_recontesting(.x, paths)), c("raj", "up"))
  base <- bind_rows(map(prepared, "base"))
  analysis <- base |> filter(included)
  estimates <- map_dfr(c("raj", "up"), function(st) {
    rules <- if (st == "raj") c("primary", "compact", "generous") else c("primary", "compact", "name_only", "fuzzy_supported", "generous")
    map_dfr(rules, ~ fit_recontesting(filter(analysis, state == st), .x))
  })
  coverage <- base |> summarise(linked = n(), fully_open = sum(eligible, na.rm = TRUE),
    roster = sum(eligible & roster_ok, na.rm = TRUE),
    baseline = sum(eligible & roster_ok & source_ok, na.rm = TRUE),
    conflicting = sum(conflict, na.rm = TRUE), ambiguous = sum(ambiguous, na.rm = TRUE),
    included = sum(included), .by = c(state, quota))
  write_parquet(base, here("data/recontesting_analysis.parquet"))
  write_csv(estimates, here("tabs/recontesting_estimates.csv"))
  write_csv(coverage, here("tabs/recontesting_coverage.csv"))
  sources <- tibble(source = names(paths), path = unname(paths),
    sha256 = map_chr(paths, ~ digest::digest(.x, algo = "sha256", file = TRUE)))
  write_csv(sources, here("tabs/recontesting_sources.csv"))
  dir.create(here("data/recontesting"), showWarnings = FALSE)
  for (st in c("raj", "up")) {
    pairs <- prepared[[st]]$pairs
    candidates <- pairs |> filter(primary | name_exact | compact | fuzzy_supported | generous)
    write_csv(candidates, here("data/recontesting", paste0(st, "_candidate_links.csv")))
    unmatched <- pairs |> filter(!id %in% candidates$id)
    if (nrow(unmatched)) {
      unmatched$edit_distance <- stringdist::stringdist(
        normalize_person(unmatched$prior_name, TRUE), normalize_person(unmatched$name, TRUE), method = "lv")
      unmatched <- unmatched |> slice_min(edit_distance, n = 1, with_ties = TRUE, by = id)
    }
    write_csv(unmatched, here("data/recontesting", paste0(st, "_nearest_nonmatches.csv")))
  }
  fmt <- function(x) sprintf("%.1f", 100 * x)
  lab <- c(raj = "Rajasthan", up = "Uttar Pradesh")
  main <- estimates |> filter(rule == "primary")
  rows <- map_chr(seq_len(nrow(main)), function(i) {
    r <- main[i, ]
    paste0(lab[[r$state]], " & ", r$quota_matches, "/", r$quota_n, " (", fmt(r$quota_rate), ") & ",
      r$nonquota_matches, "/", r$nonquota_n, " (", fmt(r$nonquota_rate), ") & ", fmt(r$estimate),
      " & [", fmt(r$conf_low), ", ", fmt(r$conf_high), "] \\\\")
  })
  writeLines(c("\\begin{tabular}{lrrrr}", "\\toprule", " & \\multicolumn{2}{c}{Matched / prior winners (\\%)} & \\multicolumn{2}{c}{Adjusted difference (pp)} \\\\",
    "Cohort & Quota & Non-quota & Estimate & 95\\% interval \\\\", "\\midrule", rows, "\\bottomrule", "\\end{tabular}"),
    here("tabs/recontesting.tex"))
  covrows <- pmap_chr(coverage, function(state, quota, linked, fully_open, roster, baseline, conflicting, ambiguous, included) {
    paste0(lab[[state]], " & ", ifelse(quota == 1, "Quota", "Non-quota"), " & ", linked, " & ", fully_open,
      " & ", roster, " & ", included, " \\\\")
  })
  writeLines(c("\\begin{tabular}{llrrrr}", "\\toprule", "Cohort & Prior seat & Linked & Fully open & Roster checks & Analysis \\\\",
    "\\midrule", covrows, "\\bottomrule", "\\end{tabular}"), here("tabs/recontesting_coverage.tex"))
  macros <- character()
  for (st in c("raj", "up")) {
    r <- filter(main, state == st)
    vals <- c(N = format(r$n, big.mark = ",", trim = TRUE), QuotaRate = fmt(r$quota_rate), NonquotaRate = fmt(r$nonquota_rate),
      Difference = fmt(r$estimate), Low = fmt(r$conf_low), High = fmt(r$conf_high), Clusters = as.character(r$clusters))
    macros <- c(macros, paste0("\\newcommand{\\recontest", ifelse(st == "raj", "Raj", "Up"), names(vals), "}{", vals, "}"))
  }
  for (rule in c("compact", "name_only", "fuzzy_supported", "generous")) {
    for (st in unique(estimates$state[estimates$rule == rule])) {
      r <- filter(estimates, state == st, .data$rule == .env$rule)
      prefix <- paste0("\\recontest", ifelse(st == "raj", "Raj", "Up"), switch(rule, compact = "Compact", name_only = "Name", fuzzy_supported = "Fuzzy", generous = "Generous"))
      vals <- c(QuotaRate = fmt(r$quota_rate), NonquotaRate = fmt(r$nonquota_rate), Difference = fmt(r$estimate),
        Low = fmt(r$conf_low), High = fmt(r$conf_high))
      macros <- c(macros, paste0("\\newcommand{", prefix, names(vals), "}{", vals, "}"))
    }
  }
  writeLines(macros, here("tabs/recontesting_numbers.tex"))
  plot_data <- estimates |>
    filter((state == "raj" & rule %in% c("primary", "compact", "generous")) |
      (state == "up" & rule %in% c("primary", "fuzzy_supported", "generous"))) |>
    mutate(cohort = recode(state, raj = "Rajasthan: 2015 to 2020", up = "Uttar Pradesh: 2015 to 2021"),
      label = case_when(rule == "primary" ~ "Strict",
        rule == "compact" ~ "Spaces removed", rule == "fuzzy_supported" ~ "Fuzzy name and relative",
        TRUE ~ "Generous name match"), label = factor(label, levels = c("Generous name match", "Fuzzy name and relative", "Spaces removed", "Strict")))
  fig <- ggplot(plot_data, aes(x = 100 * estimate, y = label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = FIG_STYLE$reference, linewidth = FIG_STYLE$reference_width) +
    geom_linerange(aes(xmin = 100 * conf_low, xmax = 100 * conf_high), orientation = "y",
      color = FIG_STYLE$interval, linewidth = FIG_STYLE$interval_width) +
    geom_point(size = FIG_STYLE$point_size, color = FIG_STYLE$estimate) + facet_wrap(vars(cohort), ncol = 1, scales = "free_y") +
    labs(x = "Prior quota minus non-quota winners (percentage points)", y = NULL) + theme_pub() + theme(axis.ticks.y = element_blank())
  ggsave(here("figs/recontesting.pdf"), fig, width = FIG_WIDTH_FULL, height = 3.5, device = cairo_pdf)
  invisible(list(base = base, estimates = estimates, coverage = coverage))
}

if (sys.nframe() == 0L) build_recontesting()
