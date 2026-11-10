library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(arrow)
library(stringi)
library(fixest)
library(here)
source(here("scripts/00_config.R"))

# Exploratory extension: 2012 quota assignment and 2017 female winners among
# later non-quota municipal/town heads. All prior non-quota offices are controls.
# Equal office weights; unadjusted difference primary; district, office type and
# prior caste adjustment secondary. Division-cluster HC1 with t(G-1) intervals.
# Exact normalized body names within reviewed districts and the same office type;
# duplicate identities, unknown prior quotas and missing outcomes are excluded.
# Geography and marginal outcomes were inspected before specifying these contrasts.
# No estimate establishes random assignment or unchanged municipal boundaries.

urban_path <- function(file) sibling_path(file, "local_elections_up_urban")

urban_name <- function(x) {
  x <- stri_trans_nfc(x)
  stri_replace_all_regex(x, "[\\s\\p{Cf}]+", "")
}

urban_sources <- function() {
  offices <- c("municipal_council_chair", "nagar_panchayat_chair")
  grid <- expand_grid(year = c(2012L, 2017L), office = offices)
  grid |> mutate(file = paste0("data/release/", year, "/", office, "_declared_winner.parquet"),
    path = map_chr(file, urban_path))
}

prepare_up_urban <- function() {
  crosswalk <- read_csv(local_source_path("data/up_urban/district_crosswalk.csv"), show_col_types = FALSE)
  stopifnot(!anyDuplicated(crosswalk$district_raw))
  sources <- urban_sources()
  records <- pmap_dfr(sources, function(year, office, file, path) {
    read_parquet(path) |> transmute(year, office, record_id, source_observation_id,
      district_raw = district_name_raw, body_raw = body_name_raw,
      name_key = urban_name(body_name_raw), female = case_when(
        candidate_woman_raw == "महिला" ~ 1L, candidate_woman_raw == "पुरुष" ~ 0L),
      quota = reserved_woman, caste = reserved_caste,
      assignment_usable, upstream_flags = map_chr(quality_flags, ~ paste(.x, collapse = ";")))
  }) |> left_join(crosswalk, by = "district_raw", relationship = "many-to-one") |>
    add_count(year, office, district, name_key, name = "identity_n")
  stopifnot(!anyNA(records$district), !anyNA(records$division), !anyNA(records$name_key),
    !anyDuplicated(records$record_id), all(records$quota %in% c(0, 1, NA)))
  earlier <- records |> filter(year == 2012, identity_n == 1)
  later <- records |> filter(year == 2017, identity_n == 1)
  links <- earlier |> inner_join(later, by = c("office", "district", "division", "name_key"),
    suffix = c("_2012", "_2017"), relationship = "one-to-one") |>
    mutate(prior_quota = quota_2012, current_quota = quota_2017, y = female_2017,
      prior_caste = caste_2012,
      included = !is.na(prior_quota) & !is.na(current_quota) & current_quota == 0 & !is.na(y),
      identical_spelling = body_raw_2012 == body_raw_2017,
      same_caste = caste_2012 == caste_2017)
  coverage <- records |> summarise(records = n(), unique_identities = sum(identity_n == 1),
    ambiguous_records = sum(identity_n > 1), missing_quota = sum(is.na(quota)),
    missing_sex = sum(is.na(female)), .by = c(year, office))
  unmatched <- records |> filter(!record_id %in% c(links$record_id_2012, links$record_id_2017))
  list(records = records, links = links, coverage = coverage, unmatched = unmatched, sources = sources)
}

urban_fit <- function(d, specification, adjusted = FALSE, cluster = "division") {
  formula <- if (adjusted) y ~ prior_quota + factor(prior_caste) + factor(office) + factor(district) else y ~ prior_quota
  model <- lm(formula, data = d)
  stopifnot(nobs(model) == nrow(d))
  covariance <- sandwich::vcovCL(model, cluster = d[[cluster]], type = "HC1", cadjust = TRUE)
  estimate <- unname(coef(model)["prior_quota"])
  standard_error <- sqrt(covariance["prior_quota", "prior_quota"])
  clusters <- n_distinct(d[[cluster]])
  df <- clusters - 1L
  tibble(specification, estimate, standard_error,
    conf_low = estimate - qt(.975, df) * standard_error,
    conf_high = estimate + qt(.975, df) * standard_error,
    p_value = 2 * pt(-abs(estimate / standard_error), df), n = nrow(d),
    quota_n = sum(d$prior_quota == 1), nonquota_n = sum(d$prior_quota == 0),
    quota_women = sum(d$y[d$prior_quota == 1]), nonquota_women = sum(d$y[d$prior_quota == 0]),
    quota_rate = mean(d$y[d$prior_quota == 1]), nonquota_rate = mean(d$y[d$prior_quota == 0]),
    cluster, clusters, df)
}

urban_bootstrap <- function(d, seed = 20261108L) {
  model <- lm(y ~ prior_quota, data = d)
  set.seed(seed)
  dqrng::dqset.seed(seed)
  boot <- fwildclusterboot::boottest(model, param = "prior_quota", B = 9999,
    clustid = "division", type = "rademacher", impose_null = TRUE,
    p_val_type = "two-tailed", conf_int = TRUE, sign_level = .05,
    engine = "R", sampling = "dqrng", nthreads = 1, maxiter = 100,
    bootstrap_type = "fnw11", ssc = fwildclusterboot::boot_ssc(
      adj = TRUE, cluster.adj = TRUE, cluster.df = "conventional"))
  stopifnot(length(boot$conf_int) == 2L, all(is.finite(boot$conf_int)))
  tibble(estimate = unname(coef(model)["prior_quota"]),
    conf_low = boot$conf_int[1], conf_high = boot$conf_int[2],
    p_value = boot$p_val, draws = 9999L, clusters = n_distinct(d$division), seed)
}

build_up_urban <- function() {
  prepared <- prepare_up_urban()
  d <- prepared$links |> filter(included)
  results <- bind_rows(
    urban_fit(d, "All linked heads"),
    urban_fit(d, "District, office type and prior caste adjusted", TRUE),
    urban_fit(filter(d, office == "municipal_council_chair"), "Municipal council heads"),
    urban_fit(filter(d, office == "nagar_panchayat_chair"), "Town council heads"),
    urban_fit(filter(d, identical_spelling), "Identical source body spelling"),
    urban_fit(filter(d, same_caste), "Unchanged caste category"),
    urban_fit(d, "District-cluster uncertainty", cluster = "district")
  )
  write_csv(urban_bootstrap(d), here("tabs/up_urban_bootstrap.csv"))
  transitions <- prepared$links |> count(office, prior_quota, current_quota, name = "offices")
  linkage <- prepared$records |> mutate(linked = record_id %in%
    c(prepared$links$record_id_2012, prepared$links$record_id_2017)) |>
    summarise(n = n(), linked = sum(linked), linked_share = mean(linked), .by = c(year, office, quota))
  influence <- map_dfr(sort(unique(d$division)), function(drop) {
    urban_fit(filter(d, division != drop), "Leave one division out") |> mutate(omitted_division = drop)
  })
  write_parquet(prepared$links, here("data/up_urban/analysis.parquet"))
  write_csv(prepared$unmatched, here("data/up_urban/unmatched_offices.csv"))
  write_csv(results, here("tabs/up_urban_estimates.csv"))
  write_csv(prepared$coverage, here("tabs/up_urban_coverage.csv"))
  write_csv(linkage, here("tabs/up_urban_linkage_coverage.csv"))
  write_csv(transitions, here("tabs/up_urban_transitions.csv"))
  write_csv(influence, here("tabs/up_urban_influence.csv"))
  write_csv(prepared$sources |> mutate(sha256 = map_chr(path,
    ~ digest::digest(.x, algo = "sha256", file = TRUE))), here("tabs/up_urban_sources.csv"))
  shown <- results |> slice(1:4)
  rows <- pmap_chr(shown, function(specification, estimate, conf_low, conf_high, n, ...) {
    sprintf("%s & %.1f & [%.1f, %.1f] & %s \\\\", specification,
      100 * estimate, 100 * conf_low, 100 * conf_high, format(n, big.mark = ","))
  })
  writeLines(c("\\begin{tabular}{lrrr}", "\\toprule",
    "Comparison & Difference (pp) & 95\\% interval & Offices \\\\", "\\midrule", rows,
    "\\bottomrule", "\\end{tabular}"), here("tabs/up_urban.tex"))
  print(results)
  invisible(prepared)
}

if (sys.nframe() == 0L) build_up_urban()
