# Gen. Util. Functions

# Create match key from district, block/samiti, and GP components
make_match_key <- function(district, block, gp) {
  paste(tolower(trimws(district)), tolower(trimws(block)), tolower(trimws(gp)), sep = "_")
}

# =============================================================================
# TRANSITION MATRIX FUNCTIONS
# =============================================================================

#' Create transition matrix using tidyverse
#' @param data Data frame with treatment indicators
#' @param from Unquoted column name for "from" period
#' @param to Unquoted column name for "to" period
#' @return Matrix with from values as row names, to values as column names
make_transition_matrix <- function(data, from, to) {
  data %>%
    dplyr::count({{ from }}, {{ to }}) %>%
    tidyr::pivot_wider(
      names_from = {{ to }},
      values_from = n,
      values_fill = list(n = 0)
    ) %>%
    tibble::column_to_rownames(var = rlang::as_label(rlang::enquo(from))) %>%
    as.matrix()
}

# =============================================================================
# AER-STYLE TABLE FUNCTIONS
# =============================================================================

## AER-Style etable wrapper for fixest
library(fixest)

aer_etable <- function(models, file, dict = NULL, digits = "r1", notes = NULL,
                       title = NULL, label = NULL, placement = "htbp",
                       headers = NULL, digits.stats = "r2", outcome_scale = 100, ...) {
  stopifnot(length(outcome_scale) %in% c(1L, length(models)))
  scales <- rep(outcome_scale, length.out = length(models))
  display_models <- Map(function(model, scale) {
    display <- summary(model)
    multipliers <- rep(scale, length(coef(model)))
    names(multipliers) <- names(coef(model))
    percentage_covariates <- c("sc_share", "lit_share", "female_share")
    multipliers[names(multipliers) %in% percentage_covariates] <- scale / 100
    display$coefficients <- coef(model) * multipliers
    display$coeftable[, 1] <- display$coeftable[, 1] * multipliers
    display$coeftable[, 2] <- display$coeftable[, 2] * multipliers
    display$se <- display$se * multipliers
    display$cov.scaled <- vcov(model) * outer(multipliers, multipliers)
    display
  }, models, scales)
  units <- if (all(scales == 100)) {
    "Coefficients and standard errors are in percentage points; intercepts are percentages."
  } else {
    "Rate outcomes use percentage points for coefficients and standard errors; intercepts are percentages. Candidate-count outcomes use candidates per GP."
  }
  notes <- paste(notes, units)
  extra <- list(...)
  if (is.null(extra$drop.section) && all(vapply(models, function(m) length(m$fixef_vars) <= 1L, logical(1)))) {
    extra$drop.section <- "fixef"
    extra$extralines <- c(extra$extralines, list("_Block fixed effects" = vapply(models,
      function(model) if (length(model$fixef_vars) > 0) "Yes" else "No", character(1))))
  }
  do.call(fixest::etable, c(list(
    display_models,
    tex = TRUE, file = file, replace = TRUE,
    caption = title, label = label, placement = placement,
    float = !is.null(title) || !is.null(label),
    style.tex = fixest::style.tex(
      "aer",
      model.format = "[i]", depvar.style = "*",
      fixef.suffix = " FE", fontsize = "scriptsize"
    ),
    digits = digits, digits.stats = digits.stats, powerBelow = -10,
    fitstat = ~ r2 + n, se.row = FALSE, dict = dict,
    headers = if (is.null(headers)) list("auto") else headers,
    depvar = is.null(headers), notes = notes, tpt = TRUE, adjustbox = "max width=\\linewidth"
  ), extra))
  writeLines(trimws(readLines(file), which = "right"), file)
}
