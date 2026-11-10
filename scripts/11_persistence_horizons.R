library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(arrow)
library(fixest)
library(fwildclusterboot)
library(ggplot2)
library(kableExtra)
library(here)
source(here("scripts/00_config.R"))

persistence_specs <- tibble::tribble(
  ~profile, ~history, ~first_index, ~label,
  "quota2005", "1000", 2L, "Quota in 2005",
  "quota2010", "0100", 3L, "Quota in 2010; open in 2005",
  "quota2005and2010", "1100", 3L, "Quotas in 2005 and 2010"
)

load_persistence_panel <- function(state) {
  years <- c(2005L, 2010L, 2015L, if (state == "raj") 2020L else 2021L)
  file <- paste0(state, if (state == "raj") "_05_20.parquet" else "_05_21.parquet")
  path <- election_panel_path(state, file)
  raw <- read_analysis_parquet(path)
  id <- if (state == "raj") raw$source_id_2005 else raw$gp_id
  block <- if (state == "raj") {
    ifelse(is.na(raw$district_std_2005) | is.na(raw$samiti_std_2005), NA_character_,
      paste(raw$district_std_2005, raw$samiti_std_2005, sep = "::"))
  } else raw$dist_block_2005
  d <- tibble(id = as.character(id), block = block)
  for (j in seq_along(years)) {
    d[[paste0("q", j)]] <- raw[[paste0("treat_", years[j])]]
    d[[paste0("y", j)]] <- raw[[paste0("female_winner_", years[j])]]
  }
  stopifnot(!anyDuplicated(d$id), !anyNA(d$id), !anyNA(d$block),
    all(unlist(d[paste0("q", 1:4)]) %in% 0:1),
    all(is.na(unlist(d[paste0("y", 1:4)])) | unlist(d[paste0("y", 1:4)]) %in% 0:1))
  d$history <- do.call(paste0, d[paste0("q", 1:4)])
  list(data = d, years = years, sha256 = digest::digest(path, algo = "sha256", file = TRUE))
}

fit_persistence <- function(d, y, seed = NA_integer_) {
  sample <- d |> mutate(y = .data[[y]], block_id = as.integer(factor(block))) |>
    filter(!is.na(y))
  model <- feols(y ~ exposed | block_id, data = sample, vcov = ~block_id,
    ssc = MODEL_SSC, fixef.rm = "none")
  stopifnot(nobs(model) == nrow(sample), "exposed" %in% names(coef(model)))
  estimate <- unname(coef(model)["exposed"])
  se <- unname(sqrt(diag(vcov(model)))["exposed"])
  df <- degrees_freedom(model, "t")
  block_support <- sample |> summarise(n = n(), mixed = n_distinct(exposed) == 2L, .by = block)
  boot <- NULL
  if (!is.na(seed)) {
    set.seed(seed)
    dqrng::dqset.seed(seed)
    boot <- boottest(model, param = "exposed", B = 9999, clustid = "block_id", fe = "block_id",
      type = "rademacher", impose_null = TRUE, p_val_type = "two-tailed", conf_int = TRUE,
      sign_level = 0.05, engine = "R", sampling = "dqrng", nthreads = 1, maxiter = 100,
      ssc = boot_ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df = "conventional"))
    stopifnot(abs(boot$point_estimate - estimate) < 1e-10, boot$N == nrow(sample),
      length(boot$conf_int) == 2L, all(is.finite(boot$conf_int)))
  }
  tibble(n = nrow(sample), treated_n = sum(sample$exposed == 1), control_n = sum(sample$exposed == 0),
    clusters = nrow(block_support), mixed_clusters = sum(block_support$mixed),
    mixed_n = sum(block_support$n[block_support$mixed]),
    treated_mean = mean(sample$y[sample$exposed == 1]), control_mean = mean(sample$y[sample$exposed == 0]),
    estimate = estimate, standard_error = se, df = df,
    conf_low = estimate - qt(.975, df) * se, conf_high = estimate + qt(.975, df) * se,
    p_value = 2 * pt(-abs(estimate / se), df), bootstrap_seed = seed,
    bootstrap_p = if (is.null(boot)) NA_real_ else boot$p_val,
    bootstrap_low = if (is.null(boot)) NA_real_ else boot$conf_int[1],
    bootstrap_high = if (is.null(boot)) NA_real_ else boot$conf_int[2])
}

build_persistence_horizons <- function() {
  results <- differences <- support <- records <- joint <- retention <- sources <- list()
  seed_index <- 310000L
  for (state in c("raj", "up")) {
    panel <- load_persistence_panel(state)
    d <- panel$data
    sources[[state]] <- tibble(state, sha256 = panel$sha256, rows = nrow(d))
    for (i in seq_len(nrow(persistence_specs))) {
      spec <- persistence_specs[i, ]
      followups <- spec$first_index:4L
      columns <- paste0("y", followups)
      eligible <- d |> filter(history %in% c("0000", spec$history)) |>
        mutate(exposed = as.integer(history == spec$history))
      fixed <- eligible[complete.cases(eligible[columns]), ]
      for (j in followups) {
        h <- j - spec$first_index + 1L
        y <- paste0("y", j)
        support[[length(support) + 1L]] <- eligible |>
          summarise(eligible = n(), observed = sum(!is.na(.data[[y]])),
            complete_series = sum(complete.cases(pick(all_of(columns)))), .by = exposed) |>
          mutate(state, profile = spec$profile, horizon = h, year = panel$years[j])
        for (sample_name in c("fixed", "available")) {
          sample <- if (sample_name == "fixed") fixed else eligible
          seed_index <- seed_index + 1L
          seed <- if (sample_name == "fixed") seed_index else NA_integer_
          message(state, " ", spec$profile, " ", sample_name, " open election ", h)
          result <- fit_persistence(sample, y, seed) |>
            mutate(state, profile = spec$profile, sample = sample_name, horizon = h, year = panel$years[j])
          results[[length(results) + 1L]] <- result
          records[[length(records) + 1L]] <- sample |> filter(!is.na(.data[[y]])) |>
            transmute(state, profile = spec$profile, sample = sample_name, id, block, history,
              exposed, horizon = h, year = panel$years[j], y = .data[[y]])
        }
        if (h > 1L) {
          diff_sample <- fixed |> mutate(change = .data[[y]] - .data[[columns[1]]])
          seed_index <- seed_index + 1L
          differences[[length(differences) + 1L]] <- fit_persistence(diff_sample, "change", seed_index) |>
            mutate(state, profile = spec$profile, horizon = h, year = panel$years[j])
        }
      }
      stacked <- fixed |>
        select(id, block, exposed, all_of(columns)) |>
        pivot_longer(all_of(columns), names_to = "outcome", values_to = "y") |>
        mutate(horizon = match(outcome, columns), block_horizon = interaction(block, horizon, drop = TRUE))
      change_names <- paste0("change_", seq_along(columns)[-1])
      for (h in seq_along(columns)[-1]) stacked[[paste0("change_", h)]] <- stacked$exposed * (stacked$horizon == h)
      fml <- as.formula(paste("y ~ exposed +", paste(change_names, collapse = " + "), "| block_horizon"))
      fit <- feols(fml, data = stacked, vcov = ~block, ssc = MODEL_SSC, fixef.rm = "none")
      joint_test <- wald(fit, keep = "^change_", print = FALSE)
      joint[[length(joint) + 1L]] <- tibble(state, profile = spec$profile,
        statistic = joint_test$stat, p_value = joint_test$p, df1 = joint_test$df1, df2 = joint_test$df2)
      for (h in seq_along(columns)) {
        expected <- fit_persistence(fixed, columns[h])$estimate
        actual <- coef(fit)["exposed"] + if (h == 1L) 0 else coef(fit)[paste0("change_", h)]
        stopifnot(abs(expected - actual) < 1e-10)
      }
    }
    for (j in 2:4) {
      selected <- rowSums(d[paste0("q", 2:j)]) == 0L
      sample <- d[selected, ] |> mutate(exposed = q1)
      retention[[length(retention) + 1L]] <- d |>
        mutate(selected = selected, observed = selected & !is.na(.data[[paste0("y", j)]])) |>
        summarise(panel_n = n(), eligible = sum(selected), observed = sum(observed), .by = q1) |>
        rename(exposed = q1) |> mutate(state, horizon = j - 1L, year = panel$years[j])
      results[[length(results) + 1L]] <- fit_persistence(sample, paste0("y", j)) |>
        mutate(state, profile = "quota2005", sample = "through_followup", horizon = j - 1L, year = panel$years[j])
      records[[length(records) + 1L]] <- sample |> filter(!is.na(.data[[paste0("y", j)]])) |>
        transmute(state, profile = "quota2005", sample = "through_followup", id, block, history,
          exposed, horizon = j - 1L, year = panel$years[j], y = .data[[paste0("y", j)]])
    }
  }
  results <- bind_rows(results)
  differences <- bind_rows(differences) |> mutate(bootstrap_p_holm = p.adjust(bootstrap_p, method = "holm"))
  joint <- bind_rows(joint)
  write_csv(results, here("tabs/persistence_horizons.csv"))
  write_csv(differences, here("tabs/persistence_horizon_changes.csv"))
  write_csv(joint, here("tabs/persistence_horizon_joint_tests.csv"))
  write_csv(bind_rows(support), here("tabs/persistence_horizon_support.csv"))
  write_csv(bind_rows(retention), here("tabs/persistence_horizon_retention.csv"))
  write_csv(bind_rows(sources), here("tabs/persistence_horizon_sources.csv"))
  write_parquet(bind_rows(records), here("data/persistence_horizon_analysis.parquet"))

  influence <- persistence_influence(bind_rows(records))
  write_csv(influence, here("tabs/persistence_horizon_influence.csv"))
  write_persistence_exhibits(results, differences)
  invisible(list(estimates = results, changes = differences, joint = joint))
}

persistence_influence <- function(records) {
  records |> filter(sample == "fixed") |>
    group_by(state, profile, horizon, year) |>
    group_modify(function(d, key) {
      centered <- d |> mutate(dx = exposed - mean(exposed), dy = y - mean(y), .by = block)
      scores <- centered |> summarise(xy = sum(dx * dy), xx = sum(dx^2), .by = block)
      leave_one_out <- (sum(scores$xy) - scores$xy) / (sum(scores$xx) - scores$xx)
      tibble(estimate = sum(scores$xy) / sum(scores$xx),
        minimum = min(leave_one_out), maximum = max(leave_one_out), blocks = nrow(scores))
    }) |> ungroup()
}

write_persistence_exhibits <- function(results, differences) {
  pp <- function(x) sub("^-0\\.0$", "0.0", sprintf("%.1f", 100 * x))
  save_fragment <- function(table, path) {
    table <- gsub("\\begin{table}", "", as.character(table), fixed = TRUE)
    table <- gsub("\\end{table}", "", table, fixed = TRUE)
    writeLines(trimws(table, which = "right"), path)
  }
  format_p <- format_p_value
  state_labels <- c(raj = "Rajasthan", up = "Uttar Pradesh")
  fixed <- results |> filter(sample == "fixed") |>
    mutate(State = factor(state_labels[state], levels = state_labels),
      Profile = factor(profile, levels = persistence_specs$profile, labels = persistence_specs$label))
  ci_text <- function(low, high) paste0("[", pp(low), ", ", pp(high), "]")
  draw_forest <- function(data, path, height, left, right, limits, breaks, subtitle = NULL) {
    cursor <- nrow(data) + 2 * n_distinct(data$group)
    data$position <- NA_real_
    headings <- list()
    for (group in unique(data$group)) {
      selected <- which(data$group == group)
      headings[[group]] <- tibble(label = group, position = cursor)
      data$position[selected] <- cursor - seq_along(selected)
      cursor <- cursor - length(selected) - 2
    }
    headings <- bind_rows(headings)
    data <- data |> mutate(display = paste0(pp(estimate), " ", ci_text(bootstrap_low, bootstrap_high)))
    stopifnot(all(100 * data$bootstrap_low > left), all(100 * data$bootstrap_high < right))
    plot <- ggplot(data, aes(x = 100 * estimate, y = position)) +
      geom_vline(xintercept = 0, color = FIG_STYLE$reference,
        linetype = "dashed", linewidth = FIG_STYLE$reference_width) +
      geom_linerange(aes(xmin = 100 * bootstrap_low, xmax = 100 * bootstrap_high),
        orientation = "y", linewidth = FIG_STYLE$interval_width, color = FIG_STYLE$interval) +
      geom_point(color = FIG_STYLE$estimate, size = FIG_STYLE$point_size) +
      geom_text(aes(label = label), x = left, hjust = 0,
        size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      geom_text(aes(label = display), x = right, hjust = 0,
        size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      geom_text(data = headings, aes(y = position, label = label), x = left,
        inherit.aes = FALSE, hjust = 0, fontface = "bold",
        size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      annotate("text", x = right, y = max(headings$position), label = "Estimate [95% CI]",
        hjust = 0, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
      scale_x_continuous(limits = limits, breaks = breaks, expand = c(0, 0)) +
      scale_y_continuous(breaks = NULL, expand = expansion(add = .6)) +
      labs(x = "Difference in women winning (percentage points)", y = NULL, subtitle = subtitle) +
      theme_pub() + theme(axis.ticks.y = element_blank(), plot.margin = margin(5, 3, 5, 3))
    ggsave(path, plot, width = FIG_WIDTH_FULL, height = height, device = cairo_pdf)
    data
  }
  supplementary <- fixed |>
    arrange(Profile, horizon, State) |>
    mutate(group = as.character(Profile),
      label = paste0(c("First", "Second", "Third")[horizon], ": ", State))
  draw_forest(supplementary, here("figs/persistence_horizons.pdf"), 6.3,
    left = -62, right = 23, limits = c(-63, 55), breaks = seq(-20, 20, 10))

  primary <- fixed |> filter(profile == "quota2005") |> arrange(horizon, State) |>
    mutate(group = paste(c("First", "Second", "Third")[horizon], "open election"),
      label = as.character(State))
  changes <- differences |> filter(profile == "quota2005", horizon == 3L) |>
    mutate(State = factor(state_labels[state], levels = state_labels),
      group = "Change: third minus first", label = as.character(State)) |> arrange(State)
  stopifnot(nrow(primary) == 6L, nrow(changes) == 2L,
    !anyDuplicated(primary[c("state", "horizon")]),
    setequal(primary$state, c("raj", "up")), all(table(primary$state) == 3L))
  sizes <- primary |> distinct(State, n) |> arrange(State)
  stopifnot(nrow(sizes) == 2L)
  subtitle <- paste(paste0(sizes$State, ": ", format(sizes$n, big.mark = ",", trim = TRUE), " GPs"), collapse = "; ")
  main_data <- draw_forest(bind_rows(primary, changes), here("figs/persistence_main.pdf"), 4.8,
    left = -44, right = 15, limits = c(-45, 40), breaks = seq(-10, 10, 10), subtitle = subtitle)
  write_csv(main_data |> select(state, group, horizon, year, n, estimate, bootstrap_low, bootstrap_high),
    here("tabs/persistence_main_figure.csv"))

  table <- fixed |>
    transmute(State, History = paste0(persistence_specs$history[match(profile, persistence_specs$profile)], "--0000"),
      Election = year, `N (T/C)` = paste0(treated_n, "/", control_n),
      `Mixed blocks` = mixed_clusters, `Women T/C (\\%)` = sprintf("%.1f/%.1f", 100 * treated_mean, 100 * control_mean),
      `Estimate (pp)` = pp(estimate), `95\\% interval` = ci_text(bootstrap_low, bootstrap_high))
  table |> kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
    kable_styling(font_size = 8, latex_options = "scale_down") |>
    save_fragment(here("tabs/persistence_horizons.tex"))
  differences |>
    transmute(State = state_labels[state], History = paste0(persistence_specs$history[match(profile, persistence_specs$profile)], "--0000"),
      Comparison = if_else(horizon == 2L, "Second minus first", "Third minus first"),
      `Change (pp)` = pp(estimate), `95\\% interval` = ci_text(bootstrap_low, bootstrap_high),
      `Bootstrap p` = format_p(bootstrap_p), `Holm p` = format_p(bootstrap_p_holm)) |>
    kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
    kable_styling(font_size = 8, latex_options = "scale_down") |>
    save_fragment(here("tabs/persistence_horizon_changes.tex"))
  results |> filter(profile == "quota2005") |>
    transmute(State = state_labels[state], Sample = recode(sample, fixed = "Same GPs", available = "Available outcomes", through_followup = "Open through followup"),
      Election = year, `N (T/C)` = paste0(treated_n, "/", control_n),
      `Estimate (pp)` = pp(estimate), `Cluster 95\\% interval` = ci_text(conf_low, conf_high)) |>
    kbl(format = "latex", booktabs = TRUE, escape = FALSE) |>
    kable_styling(font_size = 8, latex_options = "scale_down") |>
    save_fragment(here("tabs/persistence_horizon_sensitivity.tex"))
  numbers <- character()
  for (state in names(state_labels)) {
    for (h in 1:3) {
      row <- fixed |> filter(.data$state == .env$state, profile == "quota2005", horizon == h)
      prefix <- paste0("persist", if (state == "raj") "Raj" else "Up", c("First", "Second", "Third")[h])
      numbers[prefix] <- pp(row$estimate)
      numbers[paste0(prefix, "Interval")] <- ci_text(row$bootstrap_low, row$bootstrap_high)
      numbers[paste0(prefix, "N")] <- format(row$n, big.mark = ",", trim = TRUE)
    }
    for (profile in c("quota2010", "quota2005and2010")) {
      profile_name <- if (profile == "quota2010") "Later" else "Double"
      for (h in 1:2) {
        row <- fixed |> filter(.data$state == .env$state, .data$profile == .env$profile, horizon == h)
        prefix <- paste0("persist", if (state == "raj") "Raj" else "Up", profile_name, c("First", "Second")[h])
        numbers[prefix] <- pp(row$estimate)
        numbers[paste0(prefix, "Interval")] <- ci_text(row$bootstrap_low, row$bootstrap_high)
      }
      row <- differences |> filter(.data$state == .env$state, .data$profile == .env$profile)
      prefix <- paste0("persist", if (state == "raj") "Raj" else "Up", profile_name, "Change")
      numbers[prefix] <- pp(row$estimate)
      numbers[paste0(prefix, "Interval")] <- ci_text(row$bootstrap_low, row$bootstrap_high)
    }
    row <- differences |> filter(.data$state == .env$state, profile == "quota2005", horizon == 3L)
    prefix <- paste0("persist", if (state == "raj") "Raj" else "Up", "Change")
    numbers[prefix] <- pp(row$estimate)
    numbers[paste0(prefix, "Interval")] <- ci_text(row$bootstrap_low, row$bootstrap_high)
    numbers[paste0(prefix, "P")] <- format_p_value(row$bootstrap_p, latex = FALSE)
  }
  stopifnot(!anyNA(numbers))
  writeLines(paste0("\\newcommand{\\", names(numbers), "}{", numbers, "}"), here("tabs/persistence_horizon_numbers.tex"))
}

if (sys.nframe() == 0L) build_persistence_horizons()
