# 04e_balance_electoral.R
# Balance tests on electoral variables (full sample, no SHRUG filtering)
# Tests whether treatment assignment is balanced on prior electoral characteristics
# Output: tabs/balance_electoral.tex (Table B.5)

library(dplyr)
library(arrow)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Electoral Balance Tests ===")

# =============================================================================
# Helper Functions
# =============================================================================

fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
}

fmt_int <- function(x) format(as.integer(x), big.mark = ",")

compute_balance_row <- function(data, var, treat_var) {
  if (!var %in% names(data)) {
    return(list(quota = NA, open = NA, p = NA, n = 0))
  }

  data_clean <- data %>% filter(!is.na(!!sym(var)) & !is.na(!!sym(treat_var)))

  if (nrow(data_clean) < 10) {
    return(list(quota = NA, open = NA, p = NA, n = 0))
  }

  means <- tapply(data_clean[[var]], data_clean[[treat_var]], mean, na.rm = TRUE)
  t_result <- tryCatch(
    {
      t.test(data_clean[[var]] ~ data_clean[[treat_var]])
    },
    error = function(e) list(p.value = NA)
  )

  list(
    quota = unname(means["1"]),
    open = unname(means["0"]),
    p = t_result$p.value,
    n = nrow(data_clean)
  )
}

# =============================================================================
# Load Rajasthan Panels
# =============================================================================
message("\n--- Loading Rajasthan Panels ---")

raj_05_10 <- read_raj(raj_product_path("raj_05_10.parquet"))
raj_10_15 <- read_raj(raj_product_path("raj_10_15.parquet"))
raj_15_20 <- read_raj(raj_product_path("raj_15_20.parquet"))

message("Raj 05-10 N: ", nrow(raj_05_10))
message("Raj 10-15 N: ", nrow(raj_10_15))
message("Raj 15-20 N: ", nrow(raj_15_20))

# =============================================================================
# Load UP Panels
# =============================================================================
message("\n--- Loading UP Panels ---")

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))
up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

message("UP 05-10 N: ", nrow(up_05_10))
message("UP 10-15 N: ", nrow(up_10_15))
message("UP 15-21 N: ", nrow(up_15_21))

# =============================================================================
# Compute Rajasthan Balance
# =============================================================================
message("\n--- Computing Rajasthan Balance ---")

raj_bal <- list()

raj_bal[["05_10"]] <- list(
  female_winner = compute_balance_row(raj_05_10, "female_winner_2005", "treat_2010"),
  obc = compute_balance_row(raj_05_10, "obc_2005", "treat_2010"),
  sc_st = compute_balance_row(raj_05_10 %>% mutate(sc_st_2005 = sc_2005 | st_2005), "sc_st_2005", "treat_2010")
)

raj_bal[["10_15"]] <- list(
  female_winner = compute_balance_row(raj_10_15, "female_winner_2010", "treat_2015"),
  obc = compute_balance_row(raj_10_15, "obc_2010", "treat_2015"),
  sc_st = compute_balance_row(raj_10_15 %>% mutate(sc_st_2010 = sc_2010 | st_2010), "sc_st_2010", "treat_2015")
)

raj_bal[["15_20"]] <- list(
  female_winner = compute_balance_row(raj_15_20, "female_winner_2015", "treat_2020"),
  obc = compute_balance_row(raj_15_20, "obc_2015", "treat_2020"),
  sc_st = compute_balance_row(raj_15_20 %>% mutate(sc_st_2015 = sc_2015 | st_2015), "sc_st_2015", "treat_2020")
)

# =============================================================================
# Compute UP Balance
# =============================================================================
message("\n--- Computing UP Balance ---")

up_bal <- list()

up_bal[["05_10"]] <- list(
  female_winner = compute_balance_row(up_05_10, "female_winner_2005", "treat_2010"),
  obc = compute_balance_row(up_05_10, "obc_2005", "treat_2010"),
  sc_st = compute_balance_row(up_05_10, "dalit_2005", "treat_2010")
)

up_bal[["10_15"]] <- list(
  female_winner = compute_balance_row(up_10_15, "female_winner_2010", "treat_2015"),
  obc = compute_balance_row(up_10_15, "obc_2010", "treat_2015"),
  sc_st = compute_balance_row(up_10_15, "dalit_2010", "treat_2015")
)

up_bal[["15_21"]] <- list(
  female_winner = compute_balance_row(up_15_21, "female_winner_2015", "treat_2021"),
  obc = compute_balance_row(up_15_21, "obc_2015", "treat_2021"),
  sc_st = compute_balance_row(up_15_21, "dalit_2015", "treat_2021")
)

# =============================================================================
# Generate LaTeX Table - Rajasthan
# =============================================================================
message("\n--- Creating Rajasthan Electoral Balance Table ---")

make_row <- function(label, bal_list, periods) {
  parts <- sapply(periods, function(p) {
    b <- bal_list[[p]]
    paste0(fmt(b$quota), " & ", fmt(b$open), " & ", fmt(b$p, 3))
  })
  paste0(label, " & ", paste(parts, collapse = " & "), " \\\\")
}

raj_tex <- c(
  "\\begin{table}[htbp]",
  "\\caption{Rajasthan: Electoral Balance Tests for Quota Assignment}",
  "\\label{tab:balance_electoral_raj}",
  "{\\centering\\scriptsize",
  "\\begin{tabular}{@{}lrrr rrr rrr@{}}",
  "\\toprule",
  "& \\multicolumn{3}{c}{2005$\\rightarrow$2010} & \\multicolumn{3}{c}{2010$\\rightarrow$2015} & \\multicolumn{3}{c}{2015$\\rightarrow$2020} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
  "Variable & Quota & Open & p & Quota & Open & p & Quota & Open & p \\\\",
  "\\midrule"
)

periods_raj <- c("05_10", "10_15", "15_20")

raj_tex <- c(
  raj_tex,
  make_row("Prior female winner", lapply(periods_raj, function(p) raj_bal[[p]]$female_winner), seq_along(periods_raj)),
  make_row("OBC reservation", lapply(periods_raj, function(p) raj_bal[[p]]$obc), seq_along(periods_raj)),
  make_row("SC/ST reservation", lapply(periods_raj, function(p) raj_bal[[p]]$sc_st), seq_along(periods_raj))
)

row_female <- paste0(
  "Prior female winner & ",
  fmt(raj_bal[["05_10"]]$female_winner$quota), " & ", fmt(raj_bal[["05_10"]]$female_winner$open), " & ", fmt(raj_bal[["05_10"]]$female_winner$p, 3), " & ",
  fmt(raj_bal[["10_15"]]$female_winner$quota), " & ", fmt(raj_bal[["10_15"]]$female_winner$open), " & ", fmt(raj_bal[["10_15"]]$female_winner$p, 3), " & ",
  fmt(raj_bal[["15_20"]]$female_winner$quota), " & ", fmt(raj_bal[["15_20"]]$female_winner$open), " & ", fmt(raj_bal[["15_20"]]$female_winner$p, 3), " \\\\"
)

row_obc <- paste0(
  "OBC reservation & ",
  fmt(raj_bal[["05_10"]]$obc$quota), " & ", fmt(raj_bal[["05_10"]]$obc$open), " & ", fmt(raj_bal[["05_10"]]$obc$p, 3), " & ",
  fmt(raj_bal[["10_15"]]$obc$quota), " & ", fmt(raj_bal[["10_15"]]$obc$open), " & ", fmt(raj_bal[["10_15"]]$obc$p, 3), " & ",
  fmt(raj_bal[["15_20"]]$obc$quota), " & ", fmt(raj_bal[["15_20"]]$obc$open), " & ", fmt(raj_bal[["15_20"]]$obc$p, 3), " \\\\"
)

row_sc_st <- paste0(
  "SC/ST reservation & ",
  fmt(raj_bal[["05_10"]]$sc_st$quota), " & ", fmt(raj_bal[["05_10"]]$sc_st$open), " & ", fmt(raj_bal[["05_10"]]$sc_st$p, 3), " & ",
  fmt(raj_bal[["10_15"]]$sc_st$quota), " & ", fmt(raj_bal[["10_15"]]$sc_st$open), " & ", fmt(raj_bal[["10_15"]]$sc_st$p, 3), " & ",
  fmt(raj_bal[["15_20"]]$sc_st$quota), " & ", fmt(raj_bal[["15_20"]]$sc_st$open), " & ", fmt(raj_bal[["15_20"]]$sc_st$p, 3), " \\\\"
)

raj_tex <- c(
  "\\begin{table}[htbp]",
  "\\caption{Rajasthan: Electoral Balance Tests for Quota Assignment}",
  "\\label{tab:balance_electoral_raj}",
  "{\\centering\\scriptsize",
  "\\begin{tabular}{@{}lrrr rrr rrr@{}}",
  "\\toprule",
  "& \\multicolumn{3}{c}{2005$\\rightarrow$2010} & \\multicolumn{3}{c}{2010$\\rightarrow$2015} & \\multicolumn{3}{c}{2015$\\rightarrow$2020} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
  "Variable & Quota & Open & p & Quota & Open & p & Quota & Open & p \\\\",
  "\\midrule",
  row_female,
  row_obc,
  row_sc_st,
  "\\midrule",
  paste0(
    "N & \\multicolumn{3}{c}{", fmt_int(nrow(raj_05_10)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(raj_10_15)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(raj_15_20)), "} \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par}",
  "",
  "\\vspace{0.5ex}",
  "\\parbox{\\linewidth}{\\scriptsize \\emph{Notes:} Balance tests for quota assignment on prior electoral characteristics in Rajasthan. ``Quota'' = GPs reserved for women in the later year; ``Open'' = GPs not reserved. Prior female winner = proportion of GPs with a female sarpanch in the prior election. OBC/SC/ST reservation = proportion of GPs with caste-based reservations. p-values from t-tests. This table uses the full electoral panel (not SHRUG-filtered).}",
  "\\end{table}"
)

writeLines(raj_tex, here("tabs/balance_electoral_raj.tex"))
message("Created: tabs/balance_electoral_raj.tex")

# =============================================================================
# Generate LaTeX Table - UP
# =============================================================================
message("\n--- Creating UP Electoral Balance Table ---")

row_female_up <- paste0(
  "Prior female winner & ",
  fmt(up_bal[["05_10"]]$female_winner$quota), " & ", fmt(up_bal[["05_10"]]$female_winner$open), " & ", fmt(up_bal[["05_10"]]$female_winner$p, 3), " & ",
  fmt(up_bal[["10_15"]]$female_winner$quota), " & ", fmt(up_bal[["10_15"]]$female_winner$open), " & ", fmt(up_bal[["10_15"]]$female_winner$p, 3), " & ",
  fmt(up_bal[["15_21"]]$female_winner$quota), " & ", fmt(up_bal[["15_21"]]$female_winner$open), " & ", fmt(up_bal[["15_21"]]$female_winner$p, 3), " \\\\"
)

row_obc_up <- paste0(
  "OBC reservation & ",
  fmt(up_bal[["05_10"]]$obc$quota), " & ", fmt(up_bal[["05_10"]]$obc$open), " & ", fmt(up_bal[["05_10"]]$obc$p, 3), " & ",
  fmt(up_bal[["10_15"]]$obc$quota), " & ", fmt(up_bal[["10_15"]]$obc$open), " & ", fmt(up_bal[["10_15"]]$obc$p, 3), " & ",
  fmt(up_bal[["15_21"]]$obc$quota), " & ", fmt(up_bal[["15_21"]]$obc$open), " & ", fmt(up_bal[["15_21"]]$obc$p, 3), " \\\\"
)

row_sc_st_up <- paste0(
  "SC/ST reservation & ",
  fmt(up_bal[["05_10"]]$sc_st$quota), " & ", fmt(up_bal[["05_10"]]$sc_st$open), " & ", fmt(up_bal[["05_10"]]$sc_st$p, 3), " & ",
  fmt(up_bal[["10_15"]]$sc_st$quota), " & ", fmt(up_bal[["10_15"]]$sc_st$open), " & ", fmt(up_bal[["10_15"]]$sc_st$p, 3), " & ",
  fmt(up_bal[["15_21"]]$sc_st$quota), " & ", fmt(up_bal[["15_21"]]$sc_st$open), " & ", fmt(up_bal[["15_21"]]$sc_st$p, 3), " \\\\"
)

up_tex <- c(
  "\\begin{table}[htbp]",
  "\\caption{Uttar Pradesh: Electoral Balance Tests for Quota Assignment}",
  "\\label{tab:balance_electoral_up}",
  "{\\centering\\scriptsize",
  "\\begin{tabular}{@{}lrrr rrr rrr@{}}",
  "\\toprule",
  "& \\multicolumn{3}{c}{2005$\\rightarrow$2010} & \\multicolumn{3}{c}{2010$\\rightarrow$2015} & \\multicolumn{3}{c}{2015$\\rightarrow$2021} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
  "Variable & Quota & Open & p & Quota & Open & p & Quota & Open & p \\\\",
  "\\midrule",
  row_female_up,
  row_obc_up,
  row_sc_st_up,
  "\\midrule",
  paste0(
    "N & \\multicolumn{3}{c}{", fmt_int(nrow(up_05_10)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(up_10_15)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(up_15_21)), "} \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par}",
  "",
  "\\vspace{0.5ex}",
  "\\parbox{\\linewidth}{\\scriptsize \\emph{Notes:} Balance tests for quota assignment on prior electoral characteristics in Uttar Pradesh. ``Quota'' = GPs reserved for women in the later year; ``Open'' = GPs not reserved. Prior female winner = proportion of GPs with a female pradhan in the prior election. OBC/SC/ST reservation = proportion of GPs with caste-based reservations. p-values from t-tests. This table uses the full electoral panel (not SHRUG-filtered).}",
  "\\end{table}"
)

writeLines(up_tex, here("tabs/balance_electoral_up.tex"))
message("Created: tabs/balance_electoral_up.tex")

# =============================================================================
# Generate Combined Table with All Years (Table B.5)
# =============================================================================
message("\n--- Creating Combined Electoral Balance Table (All Years) ---")

make_cell <- function(bal) {
  paste0(fmt(bal$quota), " & ", fmt(bal$open), " & ", fmt(bal$p, 2))
}

combined_tex <- c(
  "{\\centering\\tiny",
  "\\begin{tabular}{@{}l rrr rrr rrr c rrr rrr rrr@{}}",
  "\\toprule",
  "& \\multicolumn{9}{c}{Rajasthan} & & \\multicolumn{9}{c}{Uttar Pradesh} \\\\",
  "\\cmidrule(lr){2-10} \\cmidrule(lr){12-20}",
  "& \\multicolumn{3}{c}{05$\\rightarrow$10} & \\multicolumn{3}{c}{10$\\rightarrow$15} & \\multicolumn{3}{c}{15$\\rightarrow$20} & & \\multicolumn{3}{c}{05$\\rightarrow$10} & \\multicolumn{3}{c}{10$\\rightarrow$15} & \\multicolumn{3}{c}{15$\\rightarrow$21} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10} \\cmidrule(lr){12-14} \\cmidrule(lr){15-17} \\cmidrule(lr){18-20}",
  "Variable & Q & O & p & Q & O & p & Q & O & p & & Q & O & p & Q & O & p & Q & O & p \\\\",
  "\\midrule",
  paste0(
    "Prior female winner & ",
    make_cell(raj_bal[["05_10"]]$female_winner), " & ",
    make_cell(raj_bal[["10_15"]]$female_winner), " & ",
    make_cell(raj_bal[["15_20"]]$female_winner), " & & ",
    make_cell(up_bal[["05_10"]]$female_winner), " & ",
    make_cell(up_bal[["10_15"]]$female_winner), " & ",
    make_cell(up_bal[["15_21"]]$female_winner), " \\\\"
  ),
  paste0(
    "OBC reservation & ",
    make_cell(raj_bal[["05_10"]]$obc), " & ",
    make_cell(raj_bal[["10_15"]]$obc), " & ",
    make_cell(raj_bal[["15_20"]]$obc), " & & ",
    make_cell(up_bal[["05_10"]]$obc), " & ",
    make_cell(up_bal[["10_15"]]$obc), " & ",
    make_cell(up_bal[["15_21"]]$obc), " \\\\"
  ),
  paste0(
    "SC/ST reservation & ",
    make_cell(raj_bal[["05_10"]]$sc_st), " & ",
    make_cell(raj_bal[["10_15"]]$sc_st), " & ",
    make_cell(raj_bal[["15_20"]]$sc_st), " & & ",
    make_cell(up_bal[["05_10"]]$sc_st), " & ",
    make_cell(up_bal[["10_15"]]$sc_st), " & ",
    make_cell(up_bal[["15_21"]]$sc_st), " \\\\"
  ),
  "\\midrule",
  paste0(
    "N & \\multicolumn{3}{c}{", fmt_int(nrow(raj_05_10)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(raj_10_15)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(raj_15_20)), "} & & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(up_05_10)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(up_10_15)), "} & ",
    "\\multicolumn{3}{c}{", fmt_int(nrow(up_15_21)), "} \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par}",
  "",
  "\\vspace{0.5ex}",
  paste0(
    "\\parbox{\\linewidth}{\\scriptsize \\emph{Notes:} Balance tests for quota assignment ",
    "on prior electoral characteristics across all election transitions. ",
    "Q = Quota (GPs reserved for women in the later year); O = Open (GPs not reserved). ",
    "Prior female winner = proportion with female sarpanch/pradhan in prior election. ",
    "p-values from t-tests. Full electoral panel (not SHRUG-filtered).}"
  )
)

writeLines(combined_tex, here("tabs/balance_electoral.tex"))
message("Created: tabs/balance_electoral.tex")

message("\n=== Electoral Balance Tests Complete ===")
