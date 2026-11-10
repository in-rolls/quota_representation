# 00_config.R
# Central configuration for the quota_representation project
# Variable dictionaries, helper functions, and shared settings
# Note: File paths use here() directly in scripts, except data this repo does
# not own -- those are pinned in data/manifest.yaml and resolved by
# up_path()/ref_path() below.

library(here)
library(dplyr)
library(arrow)

MODEL_SSC <- fixest::ssc(
  K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE,
  G.df = "min", t.df = "min"
)
fixest::setFixest_nthreads(1)

format_number <- function(x, digits = 1) {
  rounded <- round(x, digits)
  rounded[!is.na(rounded) & rounded == 0] <- 0
  ifelse(is.na(rounded), "--", sprintf(paste0("%.", digits, "f"), rounded))
}

screen_characteristic_age <- function(age) {
  # Panchayat eligibility starts at 21; 110 is a plausibility screen, not a legal maximum.
  age <- as.numeric(age)
  age[!is.finite(age) | age < 21 | age > 110] <- NA_real_
  age
}

format_p_value <- function(x, latex = TRUE) {
  small <- if (latex) "$<0.001$" else "<0.001"
  ifelse(is.na(x), "--", ifelse(x < .001, small, sprintf("%.3f", x)))
}

# =============================================================================
# VARIABLE DICTIONARIES (for aer_etable)
# =============================================================================

DICT_RAJ_TREAT <- c(
  "treat_2005" = "$\\text{Quota}_{2005}$",
  "treat_2010" = "$\\text{Quota}_{2010}$",
  "treat_2015" = "$\\text{Quota}_{2015}$",
  "treat_2020" = "$\\text{Quota}_{2020}$"
)

DICT_UP_TREAT <- c(
  "treat_2005" = "$\\text{Quota}_{2005}$",
  "treat_2010" = "$\\text{Quota}_{2010}$",
  "treat_2015" = "$\\text{Quota}_{2015}$",
  "treat_2021" = "$\\text{Quota}_{2021}$"
)

DICT_FE_RAJ <- c(
  "dist_samiti_2020" = "(District, Samiti)",
  "dist_samiti_2015" = "(District, Samiti)",
  "dist_samiti_2010" = "(District, Samiti)"
)

DICT_FE_UP <- c(
  "dist_block_2021" = "(District, Block)",
  "dist_block_2015" = "(District, Block)",
  "dist_block_2010" = "(District, Block)"
)

DICT_DOSAGE <- c(
  "never_treated" = "Never Treated",
  "always_treated" = "Always Treated",
  "sometimes_treated" = "Sometimes Treated",
  "treatment_categoryAlways Treated" = "Always vs Never Treated",
  "treatment_categorySometimes Treated" = "Sometimes Treated",
  "count_treated" = "Num. Prior Quotas"
)

# Combined dictionaries for convenience
DICT_RAJ <- c(DICT_RAJ_TREAT, DICT_FE_RAJ, DICT_DOSAGE)
DICT_UP <- c(DICT_UP_TREAT, DICT_FE_UP, DICT_DOSAGE)


# =============================================================================
# NOTES TEMPLATES
# =============================================================================

NOTES_SIGNIF <- "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1."

NOTES_SHORT_TERM <- paste0(
  NOTES_SIGNIF,
  " The dependent variable is whether a woman was elected in an open seat. ",
  "The sample is restricted to GPs where the seat was open (not reserved for women) ",
  "in the outcome year. Standard errors clustered by district--block (samiti in Rajasthan)."
)

NOTES_LONG_TERM <- paste0(
  NOTES_SIGNIF,
  " The dependent variable is whether a woman was elected in an open seat. ",
  "Models include full interactions of treatment indicators across election cycles. ",
  "Standard errors clustered by district--block (samiti in Rajasthan)."
)

# =============================================================================
# PUBLICATION-QUALITY FIGURE SETTINGS
# =============================================================================

FIG_STYLE <- list(
  family = "sans", base_size = 10, label_size = 3.2,
  point_size = 2, interval_width = .45, reference_width = .3, path_width = .35,
  estimate = "#252525", interval = "#666666", reference = "#AAAAAA", path = "#888888"
)

theme_pub <- function(base_size = FIG_STYLE$base_size, base_family = FIG_STYLE$family) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = FIG_STYLE$reference, linewidth = .25),
      axis.text = ggplot2::element_text(color = FIG_STYLE$estimate),
      axis.title = ggplot2::element_text(color = FIG_STYLE$estimate, face = "plain"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(0.9)),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.1)),
      plot.subtitle = ggplot2::element_text(color = "gray40", hjust = 0),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

COLORS_PUB <- c(
  primary = FIG_STYLE$estimate,
  secondary = FIG_STYLE$interval,
  accent = "#315F70",
  highlight = "#27AE60",
  light = "#BDC3C7"
)

# Standard figure dimensions (inches)
FIG_WIDTH_FULL <- 6.5
FIG_WIDTH_HALF <- 3.25
FIG_HEIGHT <- 4.5

## ---------------------------------------------------------------------------
## Data this repo uses but does not own
## ---------------------------------------------------------------------------
## data/manifest.yaml pins a version and a sha256 for every file that another
## repository produces. sibling_path() resolves one: cache, then a sibling clone,
## then a download -- and verifies the checksum before returning, every time,
## including on the cache hit. A mismatch stops the run; it never silently hands
## back different data.
##
## The point is not disk space. quota_spending, quota_representation, local_elections and others all
## read the same UP sarpanch files, and until now each kept its own copy with
## nothing to notice when they drifted apart.

.manifest <- function() {
  yaml::read_yaml(here::here("data", "manifest.yaml"))
}

.cache_root <- function(man) {
  root <- Sys.getenv("INDIA_DATA_HOME", unset = man$cache_dir)
  path.expand(root)
}

local_source_path <- function(file) {
  expected <- .manifest()$local_inputs[[file]]
  if (is.null(expected)) stop("Unpinned local source: ", file, call. = FALSE)
  path <- here::here(file)
  if (!file.exists(path)) stop("Missing local source: ", path, call. = FALSE)
  if (!identical(digest::digest(path, algo = "sha256", file = TRUE), expected)) {
    stop("Local source hash mismatch: ", file, call. = FALSE)
  }
  path
}

sibling_path <- function(file, source = "local_elections_up") {
  man <- .manifest()
  spec <- man$upstream[[source]]
  if (is.null(spec)) {
    stop("no manifest entry for source '", source, "'", call. = FALSE)
  }

  want <- spec$files[[file]]
  if (is.null(want)) {
    stop(file, " is not pinned in data/manifest.yaml for ", source, ".\n",
      "  Add it with its sha256 rather than reading an unpinned copy.",
      call. = FALSE
    )
  }

  dest <- file.path(.cache_root(man), source, spec$ref, file)

  if (!file.exists(dest)) {
    sibling <- path.expand(spec$sibling)
    if (!startsWith(sibling, "/")) sibling <- file.path(here::here(), sibling)
    local <- file.path(sibling, file)
    if (file.exists(local) && identical(digest::digest(local, algo = "sha256", file = TRUE), want)) {
      return(normalizePath(local))
    }
    if (isTRUE(spec$local_only) || file %in% spec$local_only_files) {
      stop("Unpublished upstream source missing or changed: ", local,
        ". Restore the hash-pinned sibling file before rebuilding.", call. = FALSE)
    }
    if (!dir.exists(dirname(dest)) && !dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)) {
      stop("Cannot create source cache: ", dirname(dest), ". Set INDIA_DATA_HOME to a writable directory.", call. = FALSE)
    }
    temporary <- tempfile("source-", tmpdir = dirname(dest))
    on.exit(unlink(temporary), add = TRUE)
    url <- paste(spec$raw, spec$ref, file, sep = "/")
    message("Fetching ", file, " from ", source, "@", spec$ref)
    utils::download.file(url, temporary, mode = "wb", quiet = TRUE)
    received <- digest::digest(temporary, algo = "sha256", file = TRUE)
    if (!identical(received, want)) {
      stop("Downloaded source hash mismatch for ", source, "@", spec$ref, "/", file,
        "\n  expected ", want, "\n  got      ", received, call. = FALSE
      )
    }
    if (!file.rename(temporary, dest)) stop("Cannot install cached source: ", dest, call. = FALSE)
  }

  got <- digest::digest(dest, algo = "sha256", file = TRUE)
  if (!identical(got, want)) {
    unlink(dest)
    stop(file, " does not match the sha256 pinned in data/manifest.yaml.\n",
      "  expected ", want, "\n  got      ", got, "\n",
      "  Either ", source, "@", spec$ref, " changed, or the copy is corrupt.",
      call. = FALSE
    )
  }
  dest
}

## Convenience wrappers so call sites read as what they are.
up_path <- function(f) sibling_path(file.path("data/release", f))
ref_path <- function(f) sibling_path(file.path("data/reference/weaver", f))

## UP publishes NAMING.md names (reserved_raw_2015, reserved_woman_2015, ...);
## the analysis keeps one vocabulary across states. Each rule maps an upstream
## column back onto the name the panels carried before the rename; every
## renamed column was checked to hold identical values under its old name.
up_renames <- c(
  "^reserved_raw_english_([0-9]{4})$" = "gp_reservation_status_eng_\\1",
  "^reserved_raw_cleaned_english_([0-9]{4})$" = "gp_res_status_fin_eng_\\1",
  "^reserved_raw_cleaned_([0-9]{4})$" = "gp_res_status_fin_\\1",
  "^reserved_raw_([0-9]{4})$" = "gp_reservation_status_\\1",
  "^candidate_caste_raw_cleaned_([0-9]{4})$" = "candidate_res_status_fin_\\1",
  "^candidate_caste_raw_english_([0-9]{4})$" = "candidate_reservation_status_eng_\\1",
  "^candidate_caste_raw_(2005|2010)$" = "candidate_res_status_\\1",
  "^candidate_caste_raw_(2015|2021)$" = "candidate_reservation_status_\\1",
  "^winner_woman_raw_cleaned_([0-9]{4})$" = "cand_sex_fin_\\1",
  "^winner_woman_raw_([0-9]{4})$" = "sex_\\1",
  "^candidate_woman_raw_2021$" = "sex_2021",
  "^reserved_woman_([0-9]{4})$" = "women_reserved_\\1",
  "^reserved_caste_([0-9]{4})$" = "reservation_class_\\1"
)

read_up <- function(path) {
  rename_with(read_parquet(path), \(names) {
    for (pattern in names(up_renames)) {
      names <- sub(pattern, up_renames[[pattern]], names)
    }
    names
  })
}

## Reference datasets: public downloads shared by several repos, held once in the
## cache rather than copied into each. Unlike sibling_path() there is nothing to
## fetch from -- SHRUG is a manual download from devdatalab.org -- so an absent
## file is reported with the instructions to get it rather than failing obscurely.

reference_path <- function(rel, key = "shrug") {
  man <- .manifest()
  spec <- man$reference[[key]]
  if (is.null(spec)) {
    stop("no reference entry for '", key, "' in data/manifest.yaml", call. = FALSE)
  }

  dest <- file.path(.cache_root(man), spec$cache, rel)
  if (!file.exists(dest)) {
    stop(key, " file not in the cache: ", rel, "\n",
      "  expected at: ", dest, "\n",
      "  ", key, " ", spec$version, " is a manual download from ", spec$source,
      "\n  Set INDIA_DATA_HOME to point elsewhere if the cache lives on another disk.",
      call. = FALSE
    )
  }

  want <- spec$files[[rel]]
  if (!is.null(want)) {
    got <- digest::digest(dest, algo = "sha256", file = TRUE)
    if (!identical(got, want)) {
      stop(rel, " does not match the sha256 pinned in data/manifest.yaml.\n",
        "  expected ", want, "\n  got      ", got, "\n",
        "  The cached copy differs from the one the results were built on.",
        call. = FALSE
      )
    }
  }
  dest
}

shrug_path <- function(rel) reference_path(rel, "shrug")

raj_path <- function(file) sibling_path(file, source = "local_elections_rajasthan")

raj_product_path <- function(file) {
  products <- c(
    "raj_05_10.parquet" = "linkage/sarpanch_2005_2010.parquet",
    "raj_10_15.parquet" = "linkage/sarpanch_2010_2015.parquet",
    "raj_15_20.parquet" = "linkage/sarpanch_2015_2020.parquet",
    "raj_05_20.parquet" = "linkage/sarpanch_2005_2020.parquet",
    "gp_lgd_crosswalk.parquet" = "linkage/gp_lgd_crosswalk.parquet",
    "candidates_2020_events.parquet" = "2020/sarpanch_candidate_events.parquet",
    "winners_2020_events.parquet" = "2020/sarpanch_winner_events.parquet"
  )
  if (!file %in% names(products)) stop("Unknown Rajasthan analysis product: ", file)
  raj_path(file.path("data/release", products[[file]]))
}

## Rajasthan publishes NAMING.md names (reserved_woman_2015, winner_woman_2020,
## ...); the analysis here speaks one vocabulary across states (female_reserved_,
## caste_category_, winner_female_, female_winner_). This maps the one onto the
## other at the read, so no analysis script depends on the upstream names.
## female_winner_2020 is the results sheet's sex where it prints one, else the
## sex on the winner's own candidate record, as the panels carried it before
## the rename. The current release regenerates event source_id values from its
## canonical paths; retain those provenance IDs. Joins use election/person keys.
raj_renames <- c(
  contesting_candidate_serial_no = "candidate_serial_no",
  name_of_contesting_candidate = "candidate_name",
  father_husband_of_contesting_candidate = "candidate_relation_name",
  education_status = "candidate_education",
  contesting_candidate_occupation = "candidate_occupation",
  age = "candidate_age",
  total_value_of_capital_assets = "candidate_assets_inr",
  children_before27111995 = "candidate_children_before_1995_11_27",
  children_on_or_after28111995 = "candidate_children_from_1995_11_28",
  martial_status = "candidate_marital_status",
  name_of_gram_panchayat = "gram_panchayat",
  sr_no = "gp_no",
  winner_candidate_name = "winner_name",
  total_no_of_contesting_candidate = "contesting_candidates",
  elected_unoppose = "unopposed",
  total_electorate_votes = "electors",
  total_polled_votes = "votes_polled",
  rejected_votes = "votes_rejected",
  total_valid_votes = "votes_valid",
  poll_percent = "turnout_pct",
  view_pledge = "winner_affidavit_url",
  vote_secure_by_winner = "winner_votes",
  runnerup_candidate_name = "runner_up_name",
  vote_secure_by_runnerup = "runner_up_votes",
  total_no_of_nota_count = "votes_nota",
  tendered_votes = "votes_tendered",
  winner_sex_from_cand = "winner_woman_from_candidates_2020",
  candidate_reservation_raw = "reserved_raw_from_candidates",
  candidate_reservation_unique = "reserved_from_candidates_unique",
  candidate_reserved = "reserved_woman_from_candidates",
  candidate_caste = "reserved_caste_from_candidates",
  reservation_gender_conflict = "reserved_woman_conflict",
  reservation_caste_conflict = "reserved_caste_conflict",
  gender = "candidate_woman_raw",
  category_of_candidate = "candidate_caste_raw",
  primary_female_reserved = "primary_reserved_woman",
  primary_caste_category = "primary_reserved_caste",
  primary_reservation_raw = "primary_reserved_raw",
  primary_reservation_available = "primary_reserved_available",
  candidate_female = "candidate_woman",
  female_runnerup_2020 = "runner_up_woman_2020"
)

read_raj <- function(path) {
  file <- tools::file_path_sans_ext(basename(path))
  d <- rename(read_parquet(path), any_of(raj_renames))
  raw_name <- switch(file,
    sarpanch_candidate_events = "category_of_gram_panchayat",
    sarpanch_winner_events = "category_of_gram_panchyat",
    "reservation_raw"
  )
  d <- rename(d, any_of(setNames("reserved_raw", raw_name)))
  if (file == "sarpanch_winner_events") {
    d <- rename(d, name_of_gram_panchyat = name_of_gram_panchayat)
  }
  d <- rename_with(
    d,
    \(x) sub("^reserved_woman", "female_reserved", sub("^reserved_caste", "caste_category", x)),
    matches("^reserved_(woman|caste)(_[0-9]{4})?$")
  )
  winner_to <- if (file == "sarpanch_winner_events") "female_winner" else "winner_female"
  d <- rename_with(d, \(x) sub("^winner_woman", winner_to, x), matches("^winner_woman(_[0-9]{4})?$"))
  if (grepl("^sarpanch_[0-9]{4}_[0-9]{4}$", file)) {
    for (y in c("2005", "2010", "2015", "2020")) {
      from <- paste0("winner_female_", y)
      if (!from %in% names(d)) next
      d[[paste0("female_winner_", y)]] <- if (y == "2020") {
        coalesce(d[[from]], d$winner_sex_from_cand)
      } else {
        d[[from]]
      }
    }
  }
  d
}

read_election_panel <- function(state, file) {
  path <- election_panel_path(state, file)
  if (state == "raj") read_raj(path) else read_parquet(path)
}

## The pooled corpus: state repositories' seats in one schema, with recorded and
## name-inferred winner sex in separate columns and the states' seat links.
pooled_path <- function(file) sibling_path(file, source = "local_elections")

election_panel_path <- function(state, file) {
  if (state == "raj") raj_product_path(file) else here::here("data", state, file)
}

additional_source_path <- function(path) {
  for (provider in c("local_elections", "local_elections_bihar", "local_elections_delhi")) {
    prefix <- paste0("../", provider, "/")
    if (startsWith(path, prefix)) {
      return(sibling_path(substring(path, nchar(prefix) + 1L), source = provider))
    }
  }
  here::here(path)
}

# State source adapters preserve this study's variable vocabulary; derived local
# UP panels already use that vocabulary and must not be recoded a second time.
read_analysis_parquet <- function(path) {
  if (grepl("^sarpanch_", basename(path))) read_raj(path) else read_parquet(path)
}

read_up_election_records <- function() {
  read_parquet(up_path("linkage/gp_head_election_records.parquet"))
}

raj_year_path <- function(year) {
  file <- if (year == 2020L) "sarpanch_reservations.parquet" else "sarpanch_results.parquet"
  raj_path(file.path("data/release", as.character(year), file))
}
