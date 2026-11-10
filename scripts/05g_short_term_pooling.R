suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(fixest)
  library(ggplot2)
  library(here)
  library(readr)
})
source(here("scripts/00_config.R"))

pool_geography <- function(data, state, year, level) {
  district <- data[[paste0(if (state == "raj") "district_std_" else "district_name_eng_", year)]]
  if (level == "district") return(tolower(district))
  block <- data[[paste0(if (state == "raj") "samiti_std_" else "block_name_eng_", year)]]
  ifelse(is.na(district) | is.na(block), NA_character_, tolower(paste(district, block, sep = "_")))
}

pool_aliases <- function(edges) {
  edges <- unique(edges[complete.cases(edges), ])
  aliases <- unique(edges[edges$old != edges$new, c("old", "new")])
  stopifnot(!anyDuplicated(aliases$old))
  lookup <- setNames(aliases$new, aliases$old)
  canonical <- function(x) {
    for (iteration in seq_len(nrow(aliases) + 1L)) {
      replacement <- unname(lookup[x])
      next_x <- ifelse(is.na(replacement), x, replacement)
      if (identical(x, next_x)) return(x)
      x <- next_x
    }
    stop("Cyclic geographic aliases")
  }
  nodes <- sort(unique(c(edges$old, edges$new)))
  data.frame(original = nodes, canonical = canonical(nodes))
}

load_pool_panels <- function() {
  panels <- list()
  geography <- list()
  overlap <- list()
  for (state in c("raj", "up")) {
    periods <- c("05_10", "10_15", if (state == "raj") "15_20" else "15_21")
    raw <- lapply(periods, function(period) {
      read_analysis_parquet(election_panel_path(state, paste0(state, "_", period, ".parquet")))
    })
    years <- c(2005L, 2010L, 2015L, if (state == "raj") 2020L else 2021L)
    maps <- lapply(c("block", "district"), function(level) {
      edges <- bind_rows(lapply(seq_along(raw), function(j) {
        pairs <- unique(data.frame(
          old = pool_geography(raw[[j]], state, years[j], level),
          new = pool_geography(raw[[j]], state, years[j + 1L], level)
        ))
        pairs <- pairs[complete.cases(pairs), ]
        stopifnot(!anyDuplicated(pairs$old), !anyDuplicated(pairs$new))
        pairs
      }))
      pool_aliases(edges)
    })
    names(maps) <- c("block", "district")
    for (level in names(maps)) {
      geography[[paste(state, level)]] <- mutate(maps[[level]], state = state, level = level)
    }
    for (j in seq_along(raw)) {
      data <- raw[[j]]
      year <- years[j + 1L]
      block <- data[[paste0(if (state == "raj") "dist_samiti_" else "dist_block_", year)]]
      d <- data.frame(
        y = data[[paste0("female_winner_", year)]],
        x = data[[paste0("treat_", years[j])]],
        open = data[[paste0("treat_", year)]] == 0,
        block = block,
        prior_id = data[[paste0(if (state == "raj") "source_id_" else "key_", years[j])]],
        outcome_id = data[[paste0(if (state == "raj") "source_id_" else "key_", year)]]
      )
      for (level in names(maps)) {
        key <- pool_geography(data, state, year, level)
        d[[paste0("cluster_", level)]] <- paste(state, maps[[level]]$canonical[match(key, maps[[level]]$original)], sep = ":")
        stopifnot(all(!is.na(maps[[level]]$canonical[match(key[!is.na(key)], maps[[level]]$original)])))
      }
      d <- d[which(d$open & complete.cases(d[, c("y", "x", "block")])), ]
      stopifnot(!anyDuplicated(d$outcome_id), !anyDuplicated(d$prior_id),
        n_distinct(d$block) == n_distinct(d$cluster_block),
        !any(grepl(":NA$", d$cluster_block)), !any(grepl(":NA$", d$cluster_district)))
      d$state <- state
      d$period <- periods[j]
      d$comparison <- paste(state, periods[j], sep = "_")
      panels[[d$comparison[1]]] <- d
    }
    for (j in 1:2) {
      a <- panels[[paste(state, periods[j], sep = "_")]]
      b <- panels[[paste(state, periods[j + 1L], sep = "_")]]
      shared <- match(a$outcome_id, b$prior_id)
      keep <- !is.na(shared)
      stopifnot(all(a$cluster_block[keep] == b$cluster_block[shared[keep]]),
        all(a$cluster_district[keep] == b$cluster_district[shared[keep]]))
      overlap[[paste(state, j)]] <- data.frame(state, first = periods[j], second = periods[j + 1L], shared_gps = sum(keep))
    }
  }
  list(panels = panels, geography = bind_rows(geography), overlap = bind_rows(overlap))
}

pool_joint_scores <- function(panels, level = "block") {
  cluster <- paste0("cluster_", level)
  all_clusters <- sort(unique(unlist(lapply(panels, `[[`, cluster))))
  scores <- matrix(0, length(all_clusters), length(panels), dimnames = list(all_clusters, names(panels)))
  rows <- list()
  for (id in names(panels)) {
    d <- panels[[id]]
    fit <- feols(y ~ x | block, d, vcov = as.formula(paste("~", cluster)),
      ssc = MODEL_SSC, fixef.rm = "none", demeaned = TRUE)
    x <- drop(fit$X_demeaned)
    influence <- x * residuals(fit) / sum(x^2)
    grouped <- rowsum(influence, d[[cluster]], reorder = TRUE)
    # Equation-specific finite-sample corrections preserve the original marginal SEs.
    correction <- unname(se(fit)["x"]) / sqrt(sum(grouped^2))
    scores[rownames(grouped), id] <- grouped[, 1] * correction
    rows[[id]] <- data.frame(comparison = id, state = d$state[1], period = d$period[1],
      estimate = unname(coef(fit)["x"]), standard_error = unname(se(fit)["x"]),
      n = nrow(d), clusters = n_distinct(d[[cluster]]), control_mean = mean(d$y[d$x == 0]),
      correction = correction)
  }
  list(estimates = bind_rows(rows), scores = scores, covariance = crossprod(scores))
}

pool_weights <- function(estimates, keep = rep(TRUE, nrow(estimates))) {
  w <- numeric(nrow(estimates))
  states <- unique(estimates$state[keep])
  for (state in states) {
    selected <- keep & estimates$state == state
    w[selected] <- 1 / length(states) / sum(selected)
  }
  w
}

pool_summary <- function(joint, weights, specification) {
  stopifnot(all(weights >= 0), abs(sum(weights) - 1) < 1e-12)
  estimate <- sum(weights * joint$estimates$estimate)
  standard_error <- sqrt(drop(crossprod(weights, joint$covariance %*% weights)))
  df <- min(joint$estimates$clusters[weights > 0]) - 1L
  data.frame(specification, estimate, standard_error,
    conf_low = estimate - qt(.975, df) * standard_error,
    conf_high = estimate + qt(.975, df) * standard_error,
    df, control_mean = sum(weights * joint$estimates$control_mean))
}

build_short_term_pooling <- function() {
  loaded <- load_pool_panels()
  joint <- pool_joint_scores(loaded$panels)
  district <- pool_joint_scores(loaded$panels, "district")
  estimates <- joint$estimates
  existing <- read_csv("tabs/model_inference.csv", show_col_types = FALSE) |>
    filter(sample == "full", fixed_effects, variance == "cluster")
  existing <- existing[match(estimates$comparison, paste(existing$state, existing$period, sep = "_")), ]
  stopifnot(max(abs(existing$estimate - estimates$estimate)) < 1e-10,
    max(abs(existing$standard_error - estimates$standard_error)) < 1e-10,
    all(existing$n == estimates$n), all(existing$clusters == estimates$clusters))
  w <- pool_weights(estimates)
  summaries <- list(pool_summary(joint, w, "Equal states and transitions"),
    pool_summary(joint, estimates$n / sum(estimates$n), "Analysis-sample weights"),
    pool_summary(district, w, "District clustering"))
  weights <- list(w, estimates$n / sum(estimates$n), w)
  for (state in unique(estimates$state)) {
    sw <- pool_weights(estimates, estimates$state != state)
    summaries[[length(summaries) + 1L]] <- pool_summary(joint, sw, paste("Omit", if (state == "raj") "Rajasthan" else "Uttar Pradesh"))
    weights[[length(weights) + 1L]] <- sw
  }
  for (id in estimates$comparison) {
    sw <- pool_weights(estimates, estimates$comparison != id)
    summaries[[length(summaries) + 1L]] <- pool_summary(joint, sw, paste("Omit", id))
    weights[[length(weights) + 1L]] <- sw
  }
  summary <- bind_rows(summaries)
  weight_table <- bind_rows(lapply(seq_along(weights), function(j) {
    data.frame(specification = summary$specification[j], comparison = estimates$comparison, weight = weights[[j]])
  }))
  set.seed(5102026)
  boot <- matrix(NA_real_, 9999L, length(w))
  for (b in seq_len(nrow(boot))) {
    multiplier <- sample(c(-1, 1), nrow(joint$scores), replace = TRUE)
    boot[b, ] <- drop(crossprod(multiplier, joint$scores))
  }
  boot_average <- drop(boot %*% w)
  primary <- summary[1, ]
  bootstrap <- data.frame(specification = "Joint wild multiplier bootstrap", estimate = primary$estimate,
    standard_error = sd(boot_average),
    conf_low = primary$estimate - unname(quantile(boot_average, .975)),
    conf_high = primary$estimate - unname(quantile(boot_average, .025)),
    df = NA_real_, control_mean = primary$control_mean)
  summary <- bind_rows(summary, bootstrap)
  covariance <- expand.grid(first = estimates$comparison, second = estimates$comparison, stringsAsFactors = FALSE)
  covariance$covariance <- as.vector(joint$covariance)
  covariance$correlation <- as.vector(cov2cor(joint$covariance))
  covariance$district_covariance <- as.vector(district$covariance)
  write_csv(summary, "tabs/short_term_pooled.csv")
  write_csv(weight_table, "tabs/short_term_pooled_weights.csv")
  write_csv(covariance, "tabs/short_term_joint_covariance.csv")
  write_csv(estimates, "tabs/short_term_pool_components.csv")
  write_csv(loaded$geography, "tabs/short_term_pool_geography.csv")
  write_csv(loaded$overlap, "tabs/short_term_pool_overlap.csv")
  fmt <- function(x) format_number(100 * x)
  labels <- summary$specification
  for (j in seq_len(nrow(estimates))) {
    labels <- sub(estimates$comparison[j], paste(if (estimates$state[j] == "raj") "Rajasthan" else "UP",
      paste(2000L + as.integer(strsplit(estimates$period[j], "_", fixed = TRUE)[[1]]), collapse = "--")), labels, fixed = TRUE)
  }
  writeLines(c("\\begin{tabular}{lrr}", "\\toprule",
    r"(Average or inference method & Estimate & 95\% interval \\)", "\\midrule",
    paste0(labels, " & ", fmt(summary$estimate), " & [", fmt(summary$conf_low), ", ", fmt(summary$conf_high), r"(] \\)"),
    "\\bottomrule", "\\end{tabular}"), "tabs/short_term_pooled.tex")
  plot_data <- estimates |>
    mutate(conf_low = estimate - qt(.975, clusters - 1) * standard_error,
      conf_high = estimate + qt(.975, clusters - 1) * standard_error,
      label = paste(if_else(state == "raj", "Rajasthan", "Uttar Pradesh"),
        vapply(strsplit(period, "_", fixed = TRUE), function(x) paste(2000L + as.integer(x), collapse = " to "), character(1))), pooled = FALSE)
  plot_data <- bind_rows(primary |> mutate(label = "Average across states and elections", pooled = TRUE), plot_data) |>
    mutate(position = rev(seq_len(n())),
      display = paste0(fmt(estimate), " [", fmt(conf_low), ", ", fmt(conf_high), "]"))
  plot <- ggplot(plot_data, aes(x = 100 * estimate, y = position)) +
    geom_vline(xintercept = 0, color = FIG_STYLE$reference, linewidth = FIG_STYLE$reference_width, linetype = "dashed") +
    geom_segment(aes(x = 100 * conf_low, xend = 100 * conf_high, yend = position),
      linewidth = FIG_STYLE$interval_width, color = FIG_STYLE$interval) +
    geom_point(aes(shape = pooled), size = FIG_STYLE$point_size, color = FIG_STYLE$estimate) +
    geom_text(aes(label = display), x = 6.2, hjust = 0, size = FIG_STYLE$label_size, family = FIG_STYLE$family) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
    scale_y_continuous(breaks = plot_data$position, labels = plot_data$label, expand = expansion(add = .6)) +
    scale_x_continuous(breaks = seq(-6, 6, 2), limits = c(-6, 10.7)) +
    labs(x = "Effect on women winning the next open election (percentage points)", y = NULL) +
    theme_pub() +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(hjust = 0), plot.margin = margin(5, 5, 5, 5))
  ggsave("figs/short_term_pooled.pdf", plot, width = FIG_WIDTH_FULL, height = 3.3, device = cairo_pdf)
  write_csv(plot_data, "tabs/short_term_pooled_figure.csv")
  message("Pooled next-election estimates and joint covariance generated.")
  invisible(list(loaded = loaded, joint = joint, district = district, summary = summary))
}

if (sys.nframe() == 0L) build_short_term_pooling()
