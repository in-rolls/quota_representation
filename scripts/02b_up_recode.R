library(arrow)
library(dplyr)
library(here)
library(stringr)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# Linkage and source recodes are shared upstream. These are study-specific variables.
recode_up_panel <- function(data, years) {
  data <- data |> filter(if_all(all_of(paste0("women_reserved_", years)), ~ !is.na(.x)))
  for (year in years) {
    data <- data |>
      mutate(
        !!paste0("treat_", year) := as.integer(.data[[paste0("women_reserved_", year)]]),
        !!paste0("female_winner_", year) := as.integer(.data[[paste0("winner_woman_", year)]]),
        !!paste0("obc_", year) := as.integer(.data[[paste0("reservation_class_", year)]] == "obc"),
        !!paste0("dalit_", year) := as.integer(.data[[paste0("reservation_class_", year)]] %in% c("sc", "st")),
        !!paste0("dist_block_", year) := str_c(.data[[paste0("district_name_eng_", year)]],
          .data[[paste0("block_name_eng_", year)]],
          sep = "_"
        )
      )
  }
  if (2010L %in% years) {
    data <- data |> mutate(
      district_name_eng_2010 = recode(district_name_eng_2010, "Ramabai Nagar" = "Kanpur Dehat"),
      district_name_2010 = recode(district_name_2010, "रमाबाई नगर" = "कानपुर देहात"),
      dist_block_2010 = str_c(district_name_eng_2010, block_name_eng_2010, sep = "_")
    )
  }
  anchor <- if (2010L %in% years) 2010L else 2015L
  data <- data |>
    mutate(
      gp_id = .data[[paste0("key_", anchor)]],
      match_key = make_match_key(
        .data[[paste0("district_name_eng_", anchor)]],
        .data[[paste0("block_name_eng_", anchor)]],
        .data[[paste0("gp_name_eng_", anchor)]]
      )
    ) |>
    add_count(match_key, name = "english_key_records") |>
    mutate(english_key_ambiguous = is.na(match_key) | english_key_records > 1L)
  if (length(years) == 2L) {
    data <- data |> mutate(case = str_c(
      .data[[paste0("treat_", years[1])]],
      .data[[paste0("treat_", years[2])]]
    ))
  }
  stopifnot(!anyNA(data$gp_id), !anyDuplicated(data$gp_id))
  data
}

panels <- list(
  `05_10` = c(2005L, 2010L), `10_15` = c(2010L, 2015L),
  `15_21` = c(2015L, 2021L), `05_21` = c(2005L, 2010L, 2015L, 2021L)
)
for (name in names(panels)) {
  years <- panels[[name]]
  file <- paste0("panels/gp_panel_", paste(years, collapse = "_"), ".parquet")
  panel <- recode_up_panel(read_parquet(up_path(file)), years)
  if (name == "05_21") {
    panel <- panel |> mutate(
      twice_treated = as.integer((treat_2005 + treat_2010 + treat_2015) == 2),
      never_treated = as.integer(treat_2005 == 0 & treat_2010 == 0 & treat_2015 == 0),
      always_treated = as.integer(treat_2005 == 1 & treat_2010 == 1 & treat_2015 == 1),
      sometimes_treated = as.integer((treat_2005 + treat_2010 + treat_2015) > 0),
      count_treated = treat_2005 + treat_2010 + treat_2015,
      once = as.integer((treat_2005 + treat_2010 + treat_2015) == 1),
      thrice_treated = as.integer((treat_2005 + treat_2010 + treat_2015) == 3),
      inter_always_treated_05_10 = as.integer((treat_2010 == 1) & (treat_2005 == 1)),
      inter_sometimes_treated_05_10 = as.integer((treat_2010 == 1) | (treat_2005 == 1)),
      inter_never_treated_05_10 = as.integer(treat_2005 + treat_2010 == 0),
      treat_all = paste(treat_2005, treat_2010, sep = "_")
    )
  }

  write_parquet(panel, here("data/up", paste0("up_", name, ".parquet")))
  message("up_", name, ": ", nrow(panel), " linked GPs before model exclusions")
}
