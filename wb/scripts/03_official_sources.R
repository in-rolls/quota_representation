# Official reservation descriptions; no election-persistence estimation.
wb_official_relocations <- function(root) {
  ledger <- file.path(root, "data", "relocations.csv")
  if (!file.exists(ledger)) return(character())
  d <- utils::read.csv(ledger, colClasses = "character")
  d <- d[d$source_repo == "local_elections", ]
  stopifnot(!anyNA(d$historical_path), !anyNA(d$current_path), !anyDuplicated(d$historical_path))
  stats::setNames(d$current_path, d$historical_path)
}

wb_official_path <- function(root, relative, relocations = wb_official_relocations(root)) {
  root <- normalizePath(root, mustWork = TRUE)
  if (relative %in% names(relocations)) relative <- unname(relocations[relative])
  path <- normalizePath(file.path(root, relative), mustWork = TRUE)
  if (!startsWith(path, paste0(root, "/"))) stop("Source path escapes root: ", relative)
  path
}

wb_official_normalize <- function(x) {
  toupper(gsub("[[:space:]]+", " ", trimws(x)))
}

wb_official_nadia_pairs <- function(d) {
  if (
    anyNA(d$source_gp_key) || anyNA(d$source_gp_serial) ||
      any(d$district != "Nadia") || any(d$year != "2013") ||
      length(unique(d$source_sha256)) != 1L ||
      any(!d$tier %in% c("gp_head", "gp_deputy_head"))
  ) {
    stop("Nadia source identity or office scope differs")
  }
  expected_key <- paste(d$source_sha256, d$source_gp_serial, sep = ":")
  if (!identical(d$source_gp_key, expected_key)) stop("Nadia GP source key differs")
  codes <- c("UR", "W", "SC", "SCW", "ST", "STW", "OBC", "OBCW")
  if (any(!is.na(d$reservation) & !d$reservation %in% codes)) {
    stop("Unexpected Nadia reservation code")
  }
  woman <- ifelse(is.na(d$reservation), NA_character_,
    ifelse(grepl("W$", d$reservation), "1", "0")
  )
  caste <- ifelse(is.na(d$reservation), NA_character_,
    ifelse(d$reservation == "W", "UR", sub("W$", "", d$reservation))
  )
  if (!identical(woman, d$woman_reserved) || !identical(caste, d$caste_reservation)) {
    stop("Nadia code and parsed axes disagree")
  }
  fields <- c(
    "source_gp_key", "source_gp_serial", "gram_panchayat", "row_id", "source_path",
    "source_sha256", "source_page", "reservation_code_raw", "reservation",
    "caste_reservation", "woman_reserved", "review_status"
  )
  head <- d[d$tier == "gp_head", fields]
  deputy <- d[d$tier == "gp_deputy_head", fields]
  for (part in list(head, deputy)) {
    if (
      nrow(part) != 187L || anyDuplicated(part$source_gp_key) ||
        !identical(sort(as.integer(part$source_gp_serial)), seq_len(187L))
    ) {
      stop("Nadia office must contain each source serial once")
    }
  }
  if (!setequal(head$source_gp_key, deputy$source_gp_key)) stop("Nadia office keys differ")
  pairs <- merge(head, deputy, by = "source_gp_key", suffixes = c("_head", "_deputy"))
  if (
    nrow(pairs) != 187L ||
      any(pairs$source_gp_serial_head != pairs$source_gp_serial_deputy)
  ) {
    stop("Nadia office pair conservation failed")
  }
  pairs <- pairs[order(as.integer(pairs$source_gp_serial_head)), ]
  pairs$name_exact_agreement <- pairs$gram_panchayat_head == pairs$gram_panchayat_deputy
  pairs$join_basis <- "same_document_source_gp_serial; no name harmonization"
  rownames(pairs) <- NULL
  pairs
}

wb_official_south24 <- function(d) {
  selected <- d$district == "South 24 Parganas" &
    d$election_year == "2018" & d$tier %in% c("gp_head", "gp_deputy") &
    d$stage %in% c("draft", "final")
  x <- d[which(selected), ]
  if (
    !nrow(x) || anyNA(x$block) || anyNA(x$gram_panchayat_reading) ||
      anyNA(x$category) || any(!x$category %in% c("SC", "ST", "BC", "women"))
  ) {
    stop("South24 comparison requires named, block-identified positive mentions")
  }
  x$block_key <- wb_official_normalize(x$block)
  x$name_key <- wb_official_normalize(x$gram_panchayat_reading)
  if (any(!nzchar(x$block_key)) || any(!nzchar(x$name_key))) {
    stop("South24 comparison key is empty")
  }
  keys <- c("tier", "block_key", "name_key", "category")
  if (anyDuplicated(x[c("stage", keys)])) stop("Duplicate South24 positive mention")
  evidence <- c("row_id", "source_path", "source_sha256", "source_page", "source_bbox")
  draft <- x[x$stage == "draft", c(keys, evidence)]
  final <- x[x$stage == "final", c(keys, evidence)]
  compared <- merge(draft, final, by = keys, all = TRUE, suffixes = c("_draft", "_final"))
  compared$comparison <- ifelse(
    is.na(compared$row_id_draft), "final_only_positive_mention",
    ifelse(is.na(compared$row_id_final), "draft_only_positive_mention", "same_positive_mention")
  )
  compared$absence_policy <- "Nonlisting does not establish UR or a negative reservation axis"
  compared
}

wb_official_sources <- function(source_root, out) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Install R package digest")
  manifest_path <- file.path(out, "official_source_manifest.csv")
  manifest <- utils::read.csv(manifest_path, colClasses = "character", na.strings = "")
  required <- c(
    "nadia_2013", "office_native", "office_scans",
    "alipurduar_2018_offices", "alipurduar_2018_results"
  )
  if (anyDuplicated(manifest$input_id) || !setequal(manifest$input_id, required)) {
    stop("Official source manifest inputs differ")
  }
  expected_units <- c(
    nadia_2013 = "source_GP_office_row",
    office_native = "positive_office_axis_mention",
    office_scans = "positive_office_axis_mention",
    alipurduar_2018_offices = "source_office_positive_union_row",
    alipurduar_2018_results = "GP_member_seat_result"
  )
  if (!identical(unname(expected_units[manifest$input_id]), manifest$unit)) {
    stop("Official source row units differ")
  }
  sources <- list()
  ledger <- list()
  checked <- new.env(parent = emptyenv())
  relocations <- wb_official_relocations(source_root)
  for (i in seq_len(nrow(manifest))) {
    spec <- manifest[i, ]
    path <- wb_official_path(source_root, spec$path, relocations)
    actual <- digest::digest(file = path, algo = "sha256")
    if (!identical(actual, spec$sha256)) stop("Official input checksum mismatch: ", spec$path)
    d <- utils::read.csv(path, colClasses = "character", na.strings = "", check.names = FALSE)
    expected_columns <- strsplit(spec$columns, "|", fixed = TRUE)[[1]]
    if (!identical(names(d), expected_columns) || nrow(d) != as.integer(spec$rows)) {
      stop("Official input schema or row count differs: ", spec$input_id)
    }
    source_paths <- paste0(
      ifelse(is.na(spec$source_path_prefix), "", spec$source_path_prefix),
      d[[spec$source_path_column]]
    )
    source_hashes <- d[[spec$source_hash_column]]
    if (anyNA(source_hashes) || anyNA(d[[spec$source_path_column]])) {
      stop("Original source provenance is missing: ", spec$input_id)
    }
    for (j in seq_along(source_paths)) {
      original <- wb_official_path(source_root, source_paths[j], relocations)
      if (!exists(original, envir = checked, inherits = FALSE)) {
        assign(original, digest::digest(file = original, algo = "sha256"), envir = checked)
      }
      if (!identical(get(original, envir = checked), source_hashes[j])) {
        stop("Original source checksum mismatch: ", source_paths[j])
      }
    }
    groups <- data.frame(row.names = seq_len(nrow(d)))
    for (field in c("district", "year", "tier", "stage")) {
      column <- spec[[paste0(field, "_column")]]
      values <- if (is.na(column)) rep(spec$stage_default, nrow(d)) else d[[column]]
      values[is.na(values)] <- "<missing>"
      if (!identical(paste(sort(unique(values)), collapse = "|"), spec[[paste0(field, "_values")]])) {
        stop("Official input scope differs: ", spec$input_id, "/", field)
      }
      groups[[field]] <- values
    }
    scopes <- unique(groups)
    for (j in seq_len(nrow(scopes))) {
      keep <- rep(TRUE, nrow(d))
      for (field in names(groups)) keep <- keep & groups[[field]] == scopes[[field]][j]
      sex_column <- spec$winner_gender_column
      named <- d[[spec$name_column]][keep]
      ledger[[length(ledger) + 1L]] <- data.frame(
        input_id = spec$input_id, path = spec$path, sha256 = actual, unit = spec$unit,
        scopes[j, ], records = sum(keep), distinct_nonempty_name_readings = length(unique(named[!is.na(named)])),
        names_are_cross_year_ids = FALSE,
        women_axis_observed = sum(!is.na(d[[spec$women_column]][keep])),
        caste_axis_observed = sum(!is.na(d[[spec$caste_column]][keep])),
        winner_sex_column_present = !is.na(sex_column),
        winner_sex_nonempty = if (is.na(sex_column)) 0L else sum(!is.na(d[[sex_column]][keep])),
        source_pdfs = length(unique(source_hashes[keep])), source_hashes_verified = TRUE,
        stringsAsFactors = FALSE
      )
    }
    sources[[spec$input_id]] <- d
  }
  ledger <- do.call(rbind, ledger)
  if (any(ledger$winner_sex_nonempty != 0L)) {
    stop("New observed sex data require outcome and office-unit review")
  }
  pairs <- wb_official_nadia_pairs(sources$nadia_2013)
  south24 <- wb_official_south24(sources$office_native)
  label_blank <- function(x) ifelse(is.na(x), "<source blank>", x)
  category_pairs <- as.data.frame(table(
    head = label_blank(pairs$reservation_head), deputy = label_blank(pairs$reservation_deputy)
  ))
  women_pairs <- as.data.frame(table(
    head = factor(label_blank(pairs$woman_reserved_head), levels = c("0", "1", "<source blank>")),
    deputy = factor(label_blank(pairs$woman_reserved_deputy), levels = c("0", "1", "<source blank>"))
  ))
  south_summary <- as.data.frame(table(
    tier = south24$tier,
    comparison = factor(south24$comparison, levels = c(
      "same_positive_mention", "draft_only_positive_mention", "final_only_positive_mention"
    ))
  ))
  dir.create(file.path(out, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out, "tabs"), recursive = TRUE, showWarnings = FALSE)
  write <- function(data, directory, name) {
    utils::write.csv(data, file.path(out, directory, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  write(ledger, "data", "official_source_availability")
  write(pairs, "data", "official_nadia_2013_pairs")
  write(south24, "data", "official_south24_2018_positive_comparison")
  write(category_pairs, "tabs", "official_nadia_2013_category_pairs")
  write(women_pairs, "tabs", "official_nadia_2013_women_pairs")
  write(south_summary, "tabs", "official_south24_2018_positive_comparison")
  complete <- sum(!is.na(pairs$woman_reserved_head) & !is.na(pairs$woman_reserved_deputy))
  report <- c(
    "# Official West Bengal reservation sources", "",
    "The pinned official-source exports add reservation descriptions, but no observed later GP-head winner sex.",
    "No persistence model is estimated. Reservation assignments and elected GP-member seats have different units.", "",
    sprintf(
      "The module checks %d parsed exports and %d original source files by SHA-256 before writing outputs.",
      nrow(manifest), length(ls(checked))
    ),
    paste(
      "The availability ledger records district, year, stage, tier, physical rows and",
      "observed axes. Mention counts are not GP counts."
    ),
    paste(
      "Positive-list nonlisting and a blank opposite reservation axis remain unknown;",
      "neither becomes UR or woman_reserved=0."
    ),
    "No candidate or winner sex is inferred from names. Alipurduar 2018 results concern GP members, not Pradhans.", "",
    "## Nadia 2013", "",
    sprintf(
      paste(
        "The complete synopsis yields %d head/deputy pairs, with %d pairs having both",
        "women-reservation statuses observed."
      ),
      nrow(pairs), complete
    ),
    paste(
      "Pairs use the source document's GP serial key, not a fuzzy name join. Both original",
      "names and codes remain in the pair export."
    ),
    "Repeated GP names remain distinct. Head serial 123 has a source-blank code and stays missing.",
    paste(
      "Explicit UR and full category codes in this complete synopsis support its negative",
      "axes; that rule is not applied to positive lists."
    ),
    "The source audit was performed by one assistant, was non-blinded, and is not human double entry.",
    paste(
      "The category and women-axis cross-tabs describe contemporaneous office allocation,",
      "not subsequent political persistence."
    ), "",
    "## South 24 Parganas 2018", "",
    "Draft/final positive sets match only on tier, block, GP name and category after case/whitespace normalization.",
    paste(
      "No punctuation removal, fuzzy matching or cross-year linking is used. A",
      "draft-only/final-only mention is not a verified change to UR."
    ),
    sprintf(
      "Shared positive mentions: %d; draft-only: %d; final-only: %d.",
      sum(south24$comparison == "same_positive_mention"),
      sum(south24$comparison == "draft_only_positive_mention"),
      sum(south24$comparison == "final_only_positive_mention")
    ),
    paste(
      "The deputy source's printed totals disagree with its visually enumerated named cells.",
      "This comparison preserves named evidence and does not force it to totals."
    ), "",
    "## Run and verify", "", "From the wb directory, with the source checkout supplied explicitly:", "",
    "```r", "source('scripts/03_official_sources.R')",
    "wb_official_sources(Sys.getenv('WB_LOCAL_ELECTIONS'), '.')", "```", "",
    paste(
      "Run `WB_LOCAL_ELECTIONS=/path/to/local_elections Rscript",
      "tests/test_official_sources.R` for checksum, join and absence-policy checks."
    ),
    "Inputs are pinned in `official_source_manifest.csv`; outputs are `data/official_*.csv` and `tabs/official_*.csv`.",
    paste(
      "The module performs no downloads and no updates to observed source values. Updating a",
      "checksum requires an explicit source review."
    )
  )
  writeLines(report, file.path(out, "OFFICIAL_SOURCES.md"))
  invisible(list(
    availability = ledger, nadia_pairs = pairs, nadia_women_pairs = women_pairs,
    south24 = south24, south24_summary = south_summary
  ))
}
