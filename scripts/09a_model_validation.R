library(arrow)
library(dplyr)
library(fixest)
library(fwildclusterboot)
library(here)
source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))
setFixest_nthreads(1)

bootstrap_results <- list()
inference_rows <- list()
support_rows <- list()
test_index <- 0L

eligible_districts <- function(data, first, second, district) {
  p <- vapply(split(data, data[[district]]), function(d) {
    tab <- table(factor(d[[first]], levels = 0:1), factor(d[[second]], levels = 0:1))
    if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
      return(NA_real_)
    }
    suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
  }, numeric(1))
  names(p)[!is.na(p) & p > 0.05]
}

fit_inference <- function(data, state, period, sample, outcome, treatment, block, cumulative = FALSE, bootstrap = TRUE) {
  required <- c(outcome, treatment, block)
  data <- data[complete.cases(data[, required]), , drop = FALSE]
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }
  # fwildclusterboot extracts cluster labels through a numeric model frame.
  data[[block]] <- as.integer(factor(data[[block]]))
  contrasts <- if (cumulative) {
    list(
      `111-000` = c("treat_2005", "treat_2010", "treat_2015", "treat_2005:treat_2010", "treat_2005:treat_2015", "treat_2010:treat_2015", "treat_2005:treat_2010:treat_2015"),
      `111-001` = c("treat_2005", "treat_2010", "treat_2005:treat_2010", "treat_2005:treat_2015", "treat_2010:treat_2015", "treat_2005:treat_2010:treat_2015"),
      `101-001` = c("treat_2005", "treat_2005:treat_2015"),
      `011-001` = c("treat_2010", "treat_2010:treat_2015")
    )
  } else {
    setNames(list(treatment), treatment)
  }
  if (cumulative) {
    support_rows[[paste(state, period, sample)]] <<- data %>%
      group_by(across(all_of(treatment))) %>%
      summarise(
        n = n(), clusters = n_distinct(.data[[block]]),
        female_winner_rate = mean(.data[[outcome]]), .groups = "drop"
      ) %>%
      mutate(state = state, period = period, sample = sample)
  }
  rhs <- paste(treatment, collapse = if (cumulative) " * " else " + ")
  for (use_fe in c(FALSE, TRUE)) {
    formula <- as.formula(paste(outcome, "~", rhs, if (use_fe) paste("|", block) else ""))
    model <- feols(formula,
      data = data, vcov = as.formula(paste("~", block)),
      ssc = MODEL_SSC, fixef.rm = "none"
    )
    stopifnot(nobs(model) == nrow(data))
    for (contrast in names(contrasts)) {
      terms <- contrasts[[contrast]]
      if (!all(terms %in% names(coef(model)))) stop("Unsupported contrast: ", state, " ", period, " ", sample, " ", contrast)
      if (cumulative) {
        history <- do.call(paste0, data[treatment])
        sides <- strsplit(contrast, "-", fixed = TRUE)[[1]]
        left <- history == sides[1]
        right <- history == sides[2]
      } else {
        left <- data[[treatment]] == 1
        right <- data[[treatment]] == 0
      }
      left_blocks <- unique(data[[block]][left])
      right_blocks <- unique(data[[block]][right])
      weights <- as.numeric(names(coef(model)) %in% terms)
      estimate <- sum(coef(model) * weights)
      test_index <<- test_index + 1L
      seed <- 120000L + test_index
      boot <- NULL
      id <- paste(state, period, sample, if (use_fe) "FE" else "NoFE", contrast, sep = "_")
      if (use_fe && bootstrap) {
        message("Wild cluster bootstrap: ", id, "; seed=", seed, "; B=9999")
        set.seed(seed)
        dqrng::dqset.seed(seed)
        run_bootstrap <- function() {
          boottest(model,
            param = terms, R = rep(1, length(terms)), r = 0,
            B = 9999, clustid = block, fe = block, type = "rademacher", impose_null = TRUE,
            p_val_type = "two-tailed", conf_int = TRUE, sign_level = 0.05,
            engine = "R", sampling = "dqrng", nthreads = 1, maxiter = 100,
            ssc = boot_ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df = "conventional")
          )
        }
        boot <- run_bootstrap()
        if (length(bootstrap_results) == 0L) {
          set.seed(seed)
          dqrng::dqset.seed(seed)
          repeated <- run_bootstrap()
          stopifnot(
            identical(boot$p_val, repeated$p_val), identical(boot$conf_int, repeated$conf_int),
            identical(boot$t_boot, repeated$t_boot)
          )
          message("Seeded bootstrap reproduction verified: ", id)
        }
        stopifnot(
          abs(boot$point_estimate - estimate) < 1e-10, length(boot$conf_int) == 2L,
          all(is.finite(boot$conf_int)), boot$conf_int[1] < boot$conf_int[2]
        )
        if (boot$N != nobs(model)) stop("Bootstrap and model samples differ: ", id)
        bootstrap_results[[id]] <<- boot
      }
      for (variance in c("cluster", "HC1")) {
        fitted <- summary(model, vcov = if (variance == "cluster") as.formula(paste("~", block)) else "hetero", ssc = MODEL_SSC)
        covariance <- vcov(fitted)
        standard_error <- sqrt(drop(t(weights) %*% covariance %*% weights))
        df <- degrees_freedom(fitted, "t")
        critical <- qt(0.975, df)
        ci <- if (is.null(boot)) c(NA_real_, NA_real_) else as.numeric(boot$conf_int)
        inference_rows[[length(inference_rows) + 1L]] <<- data.frame(
          state, period, sample,
          fixed_effects = use_fe, outcome, contrast, variance,
          n = nobs(model), clusters = n_distinct(data[[block]]),
          left_n = sum(left), right_n = sum(right), left_clusters = length(left_blocks),
          right_clusters = length(right_blocks), common_clusters = length(intersect(left_blocks, right_blocks)),
          left_mean = mean(data[[outcome]][left]), right_mean = mean(data[[outcome]][right]),
          estimate, standard_error,
          p_value = 2 * pt(-abs(estimate / standard_error), df),
          conf_low = estimate - critical * standard_error, conf_high = estimate + critical * standard_error,
          mde_80 = (qnorm(0.975) + qnorm(0.80)) * standard_error,
          bootstrap_seed = if (!is.null(boot)) seed else NA_integer_,
          bootstrap_B = if (is.null(boot)) NA_integer_ else boot$boot_iter,
          bootstrap_p = if (is.null(boot)) NA_real_ else boot$p_val,
          bootstrap_low = ci[1], bootstrap_high = ci[2]
        )
      }
    }
  }
}

for (state in c("raj", "up")) {
  panels <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (panel in panels) {
    years <- paste0("20", strsplit(panel, "_")[[1]])
    treatment <- paste0("treat_", years[1])
    current <- paste0("treat_", years[2])
    outcome <- paste0("female_winner_", years[2])
    district <- paste0(if (state == "raj") "district_std_" else "district_name_eng_", years[2])
    block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", years[2])
    data <- as.data.frame(read_election_panel(state, paste0(state, "_", panel, ".parquet")))
    open <- data[!is.na(data[[current]]) & data[[current]] == 0, ]
    eligible <- eligible_districts(data, treatment, current, district)
    fit_inference(open, state, panel, "full", outcome, treatment, block)
    fit_inference(open[open[[district]] %in% eligible, ], state, panel, "restricted", outcome, treatment, block)
    if (state == "raj" && years[2] == "2020") {
      conflict <- open$reservation_gender_conflict %in% TRUE | open$reservation_caste_conflict %in% TRUE
      fit_inference(open[!conflict, ], state, panel, "exclude_conflicts", outcome, treatment, block, bootstrap = FALSE)
    }
  }
  end <- if (state == "raj") "2020" else "2021"
  panel <- if (state == "raj") "05_20" else "05_21"
  district <- if (state == "raj") "district_std_2015" else "district_name_eng_2015"
  block <- paste0(if (state == "raj") "dist_samiti_" else "dist_block_", end)
  outcome <- paste0("female_winner_", end)
  current <- paste0("treat_", end)
  data <- as.data.frame(read_election_panel(state, paste0(state, "_", panel, ".parquet")))
  open <- data[!is.na(data[[current]]) & data[[current]] == 0, ]
  transitions <- list(c("2005", "2010"), c("2010", "2015"), c("2015", end))
  eligible <- lapply(transitions, function(years) eligible_districts(data, paste0("treat_", years[1]), paste0("treat_", years[2]), district))
  fit_inference(open, state, panel, "full", outcome, c("treat_2005", "treat_2010", "treat_2015"), block, TRUE)
  fit_inference(
    open[open[[district]] %in% Reduce(intersect, eligible[1:2]), ], state, panel,
    "restricted", outcome, c("treat_2005", "treat_2010", "treat_2015"), block, TRUE
  )
  if (state == "raj") {
    conflict <- open$reservation_gender_conflict %in% TRUE | open$reservation_caste_conflict %in% TRUE
    fit_inference(open[!conflict, ], state, panel, "exclude_conflicts", outcome,
      c("treat_2005", "treat_2010", "treat_2015"), block, TRUE,
      bootstrap = FALSE
    )
  }
}
inference <- bind_rows(inference_rows)
write.csv(inference, here("tabs/model_inference.csv"), row.names = FALSE)
saveRDS(bootstrap_results, here("data/model_bootstrap.rds"))
write.csv(bind_rows(support_rows), here("tabs/treatment_history_support.csv"), row.names = FALSE)

primary <- inference %>% filter(fixed_effects, variance == "cluster", sample %in% c("full", "restricted"))
summary_table <- function(data, file, caption) {
  display <- data %>% transmute(
    State = recode(state, raj = "Rajasthan", up = "Uttar Pradesh"),
    Period = paste0("20", sub("_", "--20", period)), Sample = sample,
    Contrast = contrast, `Cell N (L/R)` = paste0(left_n, "/", right_n),
    `Shared clusters` = common_clusters, `Estimate (pp)` = 100 * estimate,
    `Cluster CI` = sprintf("[%.2f, %.2f]", 100 * conf_low, 100 * conf_high),
    `Bootstrap p` = format.pval(bootstrap_p, digits = 3, eps = 1 / 10000),
    `Bootstrap CI` = sprintf("[%.2f, %.2f]", 100 * bootstrap_low, 100 * bootstrap_high)
  )
  tex <- knitr::kable(display, format = "latex", booktabs = TRUE, digits = 3, caption = caption, label = sub(".tex", "", file, fixed = TRUE)) %>%
    kableExtra::kable_styling(font_size = 8, latex_options = "scale_down")
  writeLines(as.character(tex), here("tabs", file))
}
summary_table(primary %>% filter(grepl("-", contrast)), "cumulative_contrasts.tex", "Cumulative contrasts: cell counts refer to left/right histories; shared clusters contain both histories")
summary_table(primary %>% filter(!grepl("-", contrast)), "short_term_bootstrap.tex", "Short-run estimates: cell counts refer to prior quota/open seats; shared clusters contain both groups")
message("Saved model inference, history support, bootstrap draws, and contrast tables.")
