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

# Publication-quality ggplot2 theme (Jackman-inspired)
theme_pub <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "gray30", fill = NA, linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = "gray30", linewidth = 0.3),
      axis.text = ggplot2::element_text(color = "gray20"),
      axis.title = ggplot2::element_text(color = "gray10", face = "plain"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(0.9)),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.1)),
      plot.subtitle = ggplot2::element_text(color = "gray40", hjust = 0)
    )
}

# Academic color palette
COLORS_PUB <- c(
  primary = "#2C3E50",
  secondary = "#7F8C8D",
  accent = "#C0392B",
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
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile("source-", tmpdir = dirname(dest))
    on.exit(unlink(temporary), add = TRUE)
    local <- file.path(spec$sibling, file)
    if (file.exists(local) && identical(digest::digest(local, algo = "sha256", file = TRUE), want)) {
      if (!file.copy(local, temporary)) stop("Cannot copy source file: ", local, call. = FALSE)
    } else {
      url <- paste(spec$raw, spec$ref, file, sep = "/")
      message("Fetching ", file, " from ", source, "@", spec$ref)
      utils::download.file(url, temporary, mode = "wb", quiet = TRUE)
    }
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
ref_path <- function(f) sibling_path(file.path("data/external/weaver", f))

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

raj_product_path <- function(file) raj_path(file.path("data/fin/elections", file))

election_panel_path <- function(state, file) {
  if (state == "raj") raj_product_path(file) else here::here("data", state, file)
}
