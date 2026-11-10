library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(arrow)
library(fixest)
library(here)
source(here("scripts/12_recontesting.R"))

# Exploratory continuation, not post-quota persistence: link each recorded 2016
# Mukhiya winner to the 2021 candidate roster of the same GP. Compare all prior
# quota and non-quota winners; women-only comparisons are selected descriptions.
# Strict matching requires exact local-model transliterations of both person
# and guardian names; generous matching permits edit-distance differences in both.
# Model outputs with digits or no text fall back to ICU transliteration. Digits
# already printed in Latin-script source names remain part of the identity.
# Initial raw-script matches exposed mixed writing systems; those diagnostic
# contrasts were seen before introducing the frozen local-model transliterations.
# Contradictory sex/age on exact identity matches and duplicate strict identities
# are excluded. Age discrepancies up to ten years from the expected five-year
# increase are tolerated. No 2021 quota is inferred from undated current feeds.

bihar_recontest_sources <- function() {
  files <- c(links = "linkage/mukhiya_2016_2021.parquet", seats2016 = "2016/seats.parquet",
    winners2016 = "2016/winners.parquet", candidates2016 = "2016/candidates.parquet",
    seats2021 = "2021/seats.parquet", candidates2021 = "2021/candidates.parquet")
  map_chr(files, ~ sibling_path(paste0("data/release/", .x), "local_elections_bihar"))
}

bihar_name_lookup <- function() {
  path <- here("data/bihar_recontesting/name_transliterations.csv")
  metadata <- jsonlite::read_json(here("data/bihar_recontesting/transliteration.json"), simplifyVector = TRUE)
  stopifnot(digest::digest(path, "sha256", file = TRUE) == metadata$output_sha256)
  for (key in names(metadata$inputs)) {
    input <- sibling_path(paste0("data/release/", key), "local_elections_bihar")
    stopifnot(digest::digest(input, "sha256", file = TRUE) == metadata$inputs[[key]])
  }
  d <- read_csv(path, show_col_types = FALSE)
  stopifnot(!anyDuplicated(d$name))
  d
}

bihar_latin <- function(x, lookup) {
  index <- match(x, lookup$name)
  stopifnot(all(is.na(x) | !is.na(index)))
  value <- normalize_person(lookup$latin[index], TRUE)
  fallback <- lookup$suspect[index] %in% TRUE & lookup$method[index] %in% "indicate model"
  value[fallback] <- latin_person(x[fallback])
  value
}

bihar_close <- function(x, y, edits = 2L, fraction = .20) {
  distance <- stringdist::stringdist(x, y, method = "lv")
  coalesce(distance == 0 | (pmin(nchar(x), nchar(y)) >= 5 &
    distance <= edits & distance / pmax(nchar(x), nchar(y)) <= fraction), FALSE)
}

bihar_pairs <- function(base, roster, lookup) {
  base |> select(id, unit_id, prior_name, prior_relative, prior_female, prior_age) |>
    inner_join(roster, by = "unit_id", relationship = "one-to-many") |>
    mutate(prior_latin = bihar_latin(prior_name, lookup), name_latin = bihar_latin(name, lookup),
      prior_relative_latin = bihar_latin(prior_relative, lookup), relative_latin = bihar_latin(relative, lookup),
      name_exact = coalesce(prior_latin == name_latin, FALSE),
      relative_exact = coalesce(prior_relative_latin == relative_latin, FALSE),
      name_close = bihar_close(prior_latin, name_latin),
      relative_close = bihar_close(prior_relative_latin, relative_latin, 3L, .25),
      sex_conflict = !is.na(prior_female) & !is.na(female) & prior_female != female,
      age_conflict = !is.na(age) & (age < 26 | (!is.na(prior_age) & abs(age - prior_age - 5) > 10)),
      contradiction = sex_conflict | age_conflict,
      strict = name_exact & relative_exact & !contradiction,
      generous = name_close & relative_close & !contradiction,
      icu = close_person(prior_name, name) & close_person(prior_relative, relative, 3L, .25) & !contradiction,
      strict_conflict = name_exact & relative_exact & contradiction)
}

prepare_bihar_recontesting <- function() {
  paths <- bihar_recontest_sources()
  lookup <- bihar_name_lookup()
  keys <- c("district_code", "block_code", "unit_code")
  links <- read_parquet(paths[["links"]]) |> rename(unit_id = unit_id_2021)
  seats <- read_parquet(paths[["seats2016"]]) |> filter(office == "mukhiya", !code_repeated) |>
    select(all_of(keys), reserved_raw, status2016 = status)
  winners <- read_parquet(paths[["winners2016"]]) |> filter(office == "mukhiya")
  candidates <- read_parquet(paths[["candidates2016"]]) |> filter(office == "mukhiya")
  winners <- winners |> select(all_of(keys), row, prior_name = candidate_name,
    prior_female = candidate_woman, prior_age_raw = age, winner_basis) |>
    left_join(candidates |> select(all_of(keys), row, prior_relative = father_husband_name),
      by = c(keys, "row"), relationship = "one-to-one")
  frame <- read_parquet(paths[["seats2021"]]) |> filter(post_id == 3) |>
    select(unit_id, candidate_records, result_only_candidates, status2021 = status)
  roster <- read_parquet(paths[["candidates2021"]]) |> filter(post_id == 3) |>
    transmute(unit_id, candidate_id = paste(unit_id, candidate_serial, sep = "::"),
      name = candidate_name, relative = guardian_name, female = candidate_woman,
      age = screen_characteristic_age(candidate_age), age_raw = candidate_age,
      in_candidate_list, source_sha256, source_row)
  stopifnot(!anyDuplicated(links[keys]), !anyDuplicated(links$unit_id), !anyDuplicated(winners[keys]),
    !anyDuplicated(roster$candidate_id))
  coverage <- roster |>
    mutate(name_known = !is.na(bihar_latin(name, lookup)), relative_known = !is.na(bihar_latin(relative, lookup))) |>
    summarise(roster_n = n(),
    names_complete = all(name_known & relative_known),
    in_list = all(in_candidate_list), .by = unit_id)
  base <- links |> left_join(seats, by = keys, relationship = "one-to-one") |>
    left_join(winners, by = keys, relationship = "one-to-one") |>
    left_join(frame, by = "unit_id", relationship = "one-to-one") |>
    left_join(coverage, by = "unit_id", relationship = "one-to-one") |>
    mutate(id = paste(district_code, block_code, unit_code, sep = "::"),
      block = paste(district_code, block_code, sep = "::"),
      prior_quota = if_else(is.na(reserved_raw), NA_integer_, as.integer(grepl("महिला", reserved_raw))),
      prior_caste = case_when(grepl("अनारक्षित", reserved_raw) ~ "General",
        grepl("अनुसूचित जनजाति", reserved_raw) ~ "ST", grepl("अनुसूचित जाति", reserved_raw) ~ "SC",
        grepl("पिछड़ा वर्ग", reserved_raw) ~ "BC"),
      prior_age = screen_characteristic_age(prior_age_raw),
      roster_ok = coalesce(roster_n == candidate_records & roster_n > 0 &
        names_complete & in_list & result_only_candidates == 0, FALSE),
      baseline_ok = !is.na(prior_quota) & !is.na(prior_caste) & !is.na(prior_female) &
        !is.na(bihar_latin(prior_name, lookup)) & !is.na(bihar_latin(prior_relative, lookup)),
      reservation_sex_conflict = coalesce(prior_quota == 1 & prior_female == 0, FALSE),
      source_ok = baseline_ok & roster_ok & !reservation_sex_conflict)
  eligible <- base |> filter(source_ok)
  pairs <- bihar_pairs(eligible, roster, lookup)
  matched <- pairs |> summarise(strict_n = sum(strict), icu_n = sum(icu),
    generous_n = sum(generous), conflicting = any(strict_conflict), .by = id) |>
    mutate(ambiguous = strict_n > 1,
      strict = as.integer(strict_n == 1), icu = as.integer(icu_n > 0),
      generous = as.integer(generous_n > 0))
  base <- base |> left_join(matched, by = "id", relationship = "one-to-one") |>
    mutate(included = coalesce(source_ok & !conflicting & !ambiguous, FALSE))
  stopifnot(nrow(base) == nrow(links))
  list(base = base, pairs = pairs, roster = roster, sources = paths, lookup = lookup)
}

bihar_recontest_fit <- function(data, rule, sample, adjusted = FALSE) {
  d <- data |> mutate(y = .data[[rule]])
  formula <- if (adjusted) y ~ prior_quota + i(prior_caste) | block else y ~ prior_quota
  model <- feols(formula, d, vcov = ~block, ssc = MODEL_SSC, fixef.rm = "none", notes = FALSE)
  stopifnot(nobs(model) == nrow(d), "prior_quota" %in% names(coef(model)))
  estimate <- unname(coef(model)["prior_quota"])
  standard_error <- unname(se(model)["prior_quota"])
  df <- degrees_freedom(model, "t")
  tibble(sample, rule, adjusted, estimate, standard_error,
    conf_low = estimate - qt(.975, df) * standard_error,
    conf_high = estimate + qt(.975, df) * standard_error,
    p_value = 2 * pt(-abs(estimate / standard_error), df), n = nrow(d),
    quota_n = sum(d$prior_quota == 1), nonquota_n = sum(d$prior_quota == 0),
    quota_matches = sum(d$y[d$prior_quota == 1]), nonquota_matches = sum(d$y[d$prior_quota == 0]),
    quota_rate = mean(d$y[d$prior_quota == 1]), nonquota_rate = mean(d$y[d$prior_quota == 0]),
    clusters = n_distinct(d$block), df)
}

build_bihar_recontesting <- function() {
  p <- prepare_bihar_recontesting()
  d <- p$base |> filter(included)
  results <- map_dfr(c("strict", "generous", "icu"), function(rule) {
    bind_rows(bihar_recontest_fit(d, rule, "All winners"),
      bihar_recontest_fit(d, rule, "All winners", TRUE),
      bihar_recontest_fit(filter(d, prior_female == 1), rule, "Women winners"),
      bihar_recontest_fit(filter(d, link_pass == "name" & name_score == 1), rule, "Exact GP names"))
  })
  coverage <- p$base |> summarise(linked = n(), baseline = sum(baseline_ok),
    roster = sum(baseline_ok & roster_ok), reservation_sex_conflicts = sum(reservation_sex_conflict),
    identity_conflicts = sum(conflicting, na.rm = TRUE), ambiguous = sum(ambiguous, na.rm = TRUE),
    included = sum(included), .by = prior_quota)
  # A fixed wrong-GP comparison probes accidental agreement; it is not a correction.
  placebo <- d |> arrange(block, unit_id) |> group_by(block) |>
    filter(n() > 1) |> mutate(unit_id = c(tail(unit_id, -1), first(unit_id))) |> ungroup()
  wrong_pairs <- bihar_pairs(placebo, p$roster, p$lookup)
  negative_control <- wrong_pairs |> summarise(strict = any(strict), icu = any(icu),
    generous = any(generous), .by = id) |> summarise(n = n(), across(c(strict, icu, generous), mean))
  dir.create(here("data/bihar_recontesting"), showWarnings = FALSE)
  write_parquet(p$base, here("data/bihar_recontesting/analysis.parquet"))
  write_csv(p$pairs |> filter(strict | icu | generous | strict_conflict),
    here("data/bihar_recontesting/candidate_links.csv"))
  write_csv(results, here("tabs/bihar_recontesting_estimates.csv"))
  write_csv(coverage, here("tabs/bihar_recontesting_coverage.csv"))
  write_csv(negative_control, here("tabs/bihar_recontesting_negative_control.csv"))
  write_csv(tibble(source = names(p$sources), path = unname(p$sources),
    sha256 = map_chr(p$sources, ~ digest::digest(.x, algo = "sha256", file = TRUE))),
    here("tabs/bihar_recontesting_sources.csv"))
  shown <- results |> filter(!adjusted, sample %in% c("All winners", "Women winners"), rule != "icu")
  rows <- pmap_chr(shown, function(sample, rule, quota_rate, nonquota_rate, estimate, conf_low, conf_high, n, ...) {
    sprintf("%s, %s & %.1f & %.1f & %.1f & [%.1f, %.1f] & %s \\\\",
      sample, rule, 100 * quota_rate, 100 * nonquota_rate, 100 * estimate,
      100 * conf_low, 100 * conf_high, format(n, big.mark = ","))
  })
  writeLines(c("\\begin{tabular}{lrrrrr}", "\\toprule",
    "Sample and matching & Quota (\\%) & Non-quota (\\%) & Gap (pp) & 95\\% interval & Winners \\\\",
    "\\midrule", rows, "\\bottomrule", "\\end{tabular}"), here("tabs/bihar_recontesting.tex"))
  print(results)
  invisible(p)
}

if (sys.nframe() == 0L) build_bihar_recontesting()
