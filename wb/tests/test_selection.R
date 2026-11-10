# Check sharp missing-outcome bounds and preserve unresolved measurements.
wb_test_selection <- function() {
  source(file.path(root, "scripts", "02_selection_and_coverage.R"), local = TRUE)
  x <- data.frame(q98 = c(0, 0, 1, 1), audited_outcome = c(0, NA, 1, NA))
  b <- wb_binary_bounds(x)$contrast
  completions <- expand.grid(a = 0:1, b = 0:1)
  differences <- apply(completions, 1, function(v) (1 + v[2]) / 2 - v[1] / 2)
  stopifnot(b$lower_difference == min(differences), b$upper_difference == max(differences))
  x$audited_outcome <- c(0, 1, 1, 1)
  complete <- wb_binary_bounds(x)$contrast
  stopifnot(complete$lower_difference == 0.5, complete$upper_difference == 0.5)
  x$audited_outcome <- NA_real_
  empty <- wb_binary_bounds(x)$contrast
  stopifnot(empty$lower_difference == -1, empty$upper_difference == 1, is.na(empty$observed_difference))
  bad <- x
  bad$q98[1] <- NA
  stopifnot(inherits(try(wb_binary_bounds(bad), silent = TRUE), "try-error"))
  bad <- x
  bad$audited_outcome[1] <- 2
  stopifnot(inherits(try(wb_binary_bounds(bad), silent = TRUE), "try-error"))
  toy <- data.frame(
    gp_id = letters[1:4], gp = letters[1:4], block = "A", q98 = c(0, 0, 1, 1), q03 = 0,
    sc98 = 0, st98 = 0, sc03 = 0, st03 = 0, explicit03 = TRUE,
    history_strict = c(TRUE, TRUE, TRUE, FALSE), survey_observed = c(TRUE, FALSE, TRUE, TRUE),
    id_consistent = c(TRUE, NA, FALSE, TRUE), survey_year_valid = TRUE,
    sex_conflict = FALSE, female_followup = c(0, NA, 1, 0), strict_history03 = c(TRUE, FALSE, FALSE, FALSE)
  )
  result <- wb_selection_frame(toy)
  stopifnot(
    nrow(result) == 4L, sum(result$history_eligible) == 3L,
    is.na(result$audited_outcome[2]), is.na(result$audited_outcome[3]),
    result$exclusion_reason[3] == "interview_id_unresolved",
    result$exclusion_reason[4] == "history_not_verified"
  )
  cat("Selection-frame and sharp-bound tests passed.\n")
}

wb_test_selection_rederivation <- function(root) {
  frame <- utils::read.csv(file.path(root, "data", "selection_frame.csv"))
  opened <- subset(frame, history_eligible & q03 == 0)
  stopifnot(nrow(frame) == 165L, sum(frame$history_eligible) == 151L, nrow(opened) == 99L)
  missing <- which(is.na(opened$audited_outcome))
  stopifnot(length(missing) == 5L)
  possibilities <- expand.grid(rep(list(0:1), length(missing)))
  differences <- apply(possibilities, 1, function(values) {
    y <- opened$audited_outcome
    y[missing] <- values
    mean(y[opened$q98 == 1]) - mean(y[opened$q98 == 0])
  })
  b <- wb_binary_bounds(opened)$contrast
  stopifnot(abs(b$lower_difference - min(differences)) < 1e-12)
  stopifnot(abs(b$upper_difference - max(differences)) < 1e-12)
  observed <- subset(opened, !is.na(audited_outcome))
  fit <- stats::lm(audited_outcome ~ q98 + sc98 + st98 + factor(block), data = observed)
  reference <- utils::read.csv(file.path(root, "tabs", "selection_adjustment_comparison.csv"))
  reference <- subset(reference, model == "1998 caste strata and block" & se_type == "HC2")
  stopifnot(nrow(observed) == 94L, nrow(reference) == 1L)
  stopifnot(abs(stats::coef(fit)["q98"] - reference$estimate) < 1e-12)
  independent_se <- sqrt(sandwich::vcovHC(fit, type = "HC2")["q98", "q98"])
  stopifnot(abs(independent_se - reference$std.error) < 1e-12)
  cat("All 32 missing-outcome completions and independent OLS/HC2 rederivation passed.\n")
}
