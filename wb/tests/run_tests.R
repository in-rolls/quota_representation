run_wb_tests <- function(root = here::here("wb")) {
  root <- normalizePath(root, mustWork = TRUE)
  shared_root <- Sys.getenv("WB_QUOTA", unset = file.path(root, "../../quota_spending/wb"))
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

  source(file.path(shared_root, "scripts", "00_common.R"), local = TRUE)
  source(file.path(root, "scripts", "01_officeholding.R"), local = TRUE)
  source(file.path(root, "scripts", "02_selection_and_coverage.R"), local = TRUE)

  toy <- data.frame(
    gp = LETTERS[1:4], q03 = c(0, 0, 1, 0), explicit03 = TRUE,
    explicit98 = c(TRUE, TRUE, TRUE, FALSE), cross_source_conflict98 = FALSE,
    survey_observed = c(TRUE, FALSE, TRUE, TRUE),
    id_consistent = c(TRUE, TRUE, TRUE, TRUE), survey_year_valid = TRUE,
    female_followup = c(1, 0, 1, 0)
  )
  flow <- wb_officeholding_sample_flow(toy)
  stopifnot(
    nrow(flow) == 9L, flow$remaining[1] == 4L,
    flow$remaining[2] == 3L, flow$remaining[4] == 2L,
    flow$remaining[6] == 1L, flow$remaining[9] == 1L
  )
  cat("Shared-code checksum and officeholding sample-flow tests passed.\n")

  source(file.path(root, "tests", "test_selection.R"), local = TRUE)
  wb_test_selection()
  wb_test_selection_rederivation(root)

  source(file.path(root, "tests", "test_official_sources.R"), local = TRUE)
  source_root <- Sys.getenv("WB_LOCAL_ELECTIONS", unset = file.path(root, "../../local_elections"))
  wb_test_official_sources(root, normalizePath(source_root, mustWork = TRUE))
}

if (sys.nframe() == 0L) {
  invocation <- grep("^--file=", commandArgs(), value = TRUE)
  root <- dirname(dirname(normalizePath(sub("^--file=", "", invocation[1]))))
  run_wb_tests(root)
}
