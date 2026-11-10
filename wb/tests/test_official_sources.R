# Run without the persistence models or their shared preparation pipeline.
wb_test_official_sources <- function(root, source_root) {
  source(file.path(root, "scripts", "03_official_sources.R"), local = TRUE)
  expect_error <- function(expression, pattern) {
    message <- tryCatch(
      {
        force(expression)
        NULL
      },
      error = conditionMessage
    )
    if (is.null(message) || !grepl(pattern, message, fixed = TRUE)) {
      stop("Expected error containing: ", pattern)
    }
  }
  scratch <- tempfile("wb-official-tests-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  manifest <- file.path(root, "official_source_manifest.csv")
  file.copy(manifest, scratch)
  result <- wb_official_sources(source_root, scratch)
  stopifnot(
    nrow(result$nadia_pairs) == 187L,
    !anyDuplicated(result$nadia_pairs$source_gp_key),
    sum(result$availability$winner_sex_nonempty) == 0L,
    all(result$availability$source_hashes_verified),
    sum(result$nadia_women_pairs$Freq) == 187L
  )
  pairs <- result$nadia_pairs
  blank <- pairs$source_gp_serial_head == "123"
  stopifnot(
    sum(blank) == 1L, is.na(pairs$reservation_head[blank]),
    is.na(pairs$woman_reserved_head[blank]),
    pairs$reservation_deputy[blank] == "SC",
    sum(pairs$woman_reserved_head == "1", na.rm = TRUE) == 93L,
    sum(pairs$woman_reserved_deputy == "1", na.rm = TRUE) == 21L,
    sum(pairs$woman_reserved_head == "1" & pairs$woman_reserved_deputy == "1", na.rm = TRUE) == 0L
  )
  exported <- utils::read.csv(file.path(scratch, "data", "official_nadia_2013_pairs.csv"),
    colClasses = "character", na.strings = ""
  )
  stopifnot(
    identical(exported$source_gp_key, pairs$source_gp_key),
    identical(exported$reservation_head, pairs$reservation_head)
  )
  stopifnot(
    all(result$south24$comparison == "same_positive_mention"),
    sum(result$south24$tier == "gp_head") == 310L,
    sum(result$south24$tier == "gp_deputy") == 115L
  )
  nadia <- utils::read.csv(
    file.path(
      source_root,
      "data/west_bengal/release/nadia_2013_handbook/office_reservations.csv"
    ),
    colClasses = "character", na.strings = ""
  )
  duplicated <- rbind(nadia, nadia[1L, ])
  expect_error(wb_official_nadia_pairs(duplicated), "each source serial once")
  wrong_axis <- nadia
  wrong_axis$woman_reserved[1L] <- "0"
  expect_error(wb_official_nadia_pairs(wrong_axis), "parsed axes disagree")
  wrong_key <- nadia
  wrong_key$source_gp_key[1L] <- "not-a-source-key"
  expect_error(wb_official_nadia_pairs(wrong_key), "source key differs")
  native <- utils::read.csv(
    file.path(
      source_root,
      "data/west_bengal/release/office_native/mentions.csv"
    ),
    colClasses = "character", na.strings = ""
  )
  south <- native[which(native$district == "South 24 Parganas"), ]
  row <- which(south$stage == "final")[1L]
  withheld <- wb_official_south24(south[-row, ])
  stopifnot(
    sum(withheld$comparison == "draft_only_positive_mention") == 1L,
    !any(c("woman_reserved", "caste_reservation") %in% names(withheld))
  )
  expect_error(wb_official_south24(rbind(south, south[1L, ])), "Duplicate South24")
  bad_out <- file.path(scratch, "tampered")
  dir.create(bad_out)
  bad_manifest <- utils::read.csv(manifest, colClasses = "character")
  bad_manifest$sha256[1L] <- paste(rep("0", 64L), collapse = "")
  utils::write.csv(bad_manifest, file.path(bad_out, "official_source_manifest.csv"), row.names = FALSE)
  expect_error(wb_official_sources(source_root, bad_out), "input checksum mismatch")
  stopifnot(!file.exists(file.path(bad_out, "OFFICIAL_SOURCES.md")))
  bad_manifest <- utils::read.csv(manifest, colClasses = "character")
  bad_manifest$unit[1L] <- "elected_GP_head"
  utils::write.csv(bad_manifest, file.path(bad_out, "official_source_manifest.csv"), row.names = FALSE)
  expect_error(wb_official_sources(source_root, bad_out), "row units differ")
  bad_manifest <- utils::read.csv(manifest, colClasses = "character")
  bad_manifest$rows[1L] <- "373"
  utils::write.csv(bad_manifest, file.path(bad_out, "official_source_manifest.csv"), row.names = FALSE)
  expect_error(wb_official_sources(source_root, bad_out), "schema or row count differs")
  expect_error(wb_official_path(file.path(source_root, "data"), "../README.md"), "escapes root")
  cat("Official-source checksum, scope, pair, blank-axis and positive-set tests passed.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  invocation <- grep("^--file=", commandArgs(), value = TRUE)
  root <- dirname(dirname(normalizePath(sub("^--file=", "", invocation[1L]))))
  source_root <- Sys.getenv("WB_LOCAL_ELECTIONS", unset = file.path(root, "../../local_elections"))
  wb_test_official_sources(root, normalizePath(source_root, mustWork = TRUE))
}
