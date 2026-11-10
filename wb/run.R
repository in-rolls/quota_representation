# Run the WB preparation, checks and analysis from any working directory.
build_wb <- function(root = here::here("wb"), prepare_only = FALSE) {
  root <- normalizePath(root, mustWork = TRUE)
  source_root <- Sys.getenv("WB_LOCAL_ELECTIONS", unset = file.path(root, "../../local_elections"))
  shared_root <- Sys.getenv("WB_QUOTA", unset = file.path(root, "../../quota_spending/wb"))
  packages <- c("haven", "arrow", "dplyr", "digest", "jsonlite", "estimatr", "broom", "knitr", "ggplot2")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install R packages: ", paste(missing, collapse = ", "))
  manifest <- utils::read.csv(file.path(root, "shared_code_manifest.csv"),
    colClasses = "character"
  )
  for (i in seq_len(nrow(manifest))) {
    path <- file.path(shared_root, manifest$path[i])
    stopifnot(file.exists(path))
    actual <- digest::digest(file = path, algo = "sha256")
    if (!identical(actual, manifest$sha256[i])) {
      stop("Shared-code checksum mismatch: ", path)
    }
  }
  dictionary_hash <- digest::digest(file = file.path(root, "dictionary.csv"), algo = "sha256")
  expected_dictionary <- manifest$sha256[manifest$path == "dictionary.csv"]
  stopifnot(identical(dictionary_hash, expected_dictionary))
  dir.create(file.path(root, "logs"), showWarnings = FALSE)
  sink(file.path(root, "logs", "run.log"), split = TRUE)
  on.exit(sink(), add = TRUE)
  cat("Started:", format(Sys.time(), tz = "UTC"), "UTC\n")
  source(file.path(shared_root, "scripts", "00_common.R"), local = TRUE)
  d <- wb_prepare(normalizePath(source_root), root, shared_root)
  d <- wb_outcomes(d, root)
  source(file.path(shared_root, "tests", "contracts.R"), local = TRUE)
  wb_test_contracts(d)
  if (!prepare_only) {
    source(file.path(root, "scripts", "01_officeholding.R"), local = TRUE)
    wb_officeholding(d, root)
    source(file.path(root, "scripts", "02_selection_and_coverage.R"), local = TRUE)
    wb_selection_extension(d, root)
    source(file.path(root, "scripts", "03_official_sources.R"), local = TRUE)
    wb_official_sources(normalizePath(source_root), root)
  }
  writeLines(trimws(capture.output(sessionInfo()), which = "right"), file.path(root, "logs", "sessionInfo.txt"))
  cat("Completed:", format(Sys.time(), tz = "UTC"), "UTC\n")
}
if (sys.nframe() == 0L) {
  invocation <- grep("^--file=", commandArgs(), value = TRUE)
  root <- dirname(normalizePath(sub("^--file=", "", invocation[1])))
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(all(args %in% "--prepare-only"))
  build_wb(root, prepare_only = "--prepare-only" %in% args)
}
