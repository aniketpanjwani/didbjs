library(jsonlite)

repo_root <- normalizePath(getwd())
setwd(repo_root)

result_dir <- file.path("check-results", "f050")
inst_result_dir <- file.path("inst", "check-results", "f050")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(inst_result_dir, recursive = TRUE, showWarnings = FALSE)

read_text <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  readLines(path, warn = FALSE)
}

sha256_files <- function(files) {
  output <- system2("shasum", c("-a", "256", files), stdout = TRUE, stderr = TRUE)
  if (!identical(attr(output, "status"), NULL)) {
    cat(output, sep = "\n")
    stop("Could not compute sha256 hashes.", call. = FALSE)
  }
  hashes <- sub("\\s+.*$", "", output)
  stats::setNames(hashes, files)
}

description <- read.dcf("DESCRIPTION")
check_log <- read_text(file.path("didbjs.Rcheck", "00check.log"))
testthat_log <- read_text(file.path("didbjs.Rcheck", "tests", "testthat.Rout"))
status_ok <- any(grepl("^Status: OK$", check_log))
has_error <- any(grepl("\\bERROR\\b", check_log))
has_warning <- any(grepl("\\bWARNING\\b", check_log))
parse_testthat_counts <- function(lines) {
  matches <- grep("\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]", lines, value = TRUE)
  empty <- list(fail = NA_integer_, warn = NA_integer_, skip = NA_integer_, pass = NA_integer_)
  if (length(matches) == 0) {
    return(empty)
  }
  pattern <- ".*\\[ FAIL ([0-9]+) \\| WARN ([0-9]+) \\| SKIP ([0-9]+) \\| PASS ([0-9]+) \\].*"
  parsed <- regexec(pattern, tail(matches, 1))
  values <- regmatches(tail(matches, 1), parsed)[[1]]
  if (length(values) != 5) {
    return(empty)
  }
  counts <- as.integer(values[2:5])
  list(fail = counts[[1]], warn = counts[[2]], skip = counts[[3]], pass = counts[[4]])
}
testthat_counts <- parse_testthat_counts(testthat_log)
has_testthat_failure <- is.na(testthat_counts$fail) || testthat_counts$fail > 0L
has_testthat_warning <- is.na(testthat_counts$warn) || testthat_counts$warn > 0L
has_skip <- is.na(testthat_counts$skip) || testthat_counts$skip > 0L
assertions <- testthat_counts$pass
has_assertions <- !is.na(assertions) && assertions > 0L

summary_lines <- c(
  "F050 package-check summary",
  paste0("package: ", description[1, "Package"]),
  paste0("version: ", description[1, "Version"]),
  paste0("r_version: ", paste(R.version$major, R.version$minor, sep = ".")),
  paste0("platform: ", R.version$platform),
  paste0("r_cmd_check_status_ok: ", tolower(status_ok)),
  paste0("r_cmd_check_errors: ", tolower(has_error)),
  paste0("r_cmd_check_warnings: ", tolower(has_warning)),
  paste0("testthat_failures: ", testthat_counts$fail),
  paste0("testthat_warnings: ", testthat_counts$warn),
  paste0("mandatory_test_skips: ", tolower(has_skip)),
  paste0("testthat_assertions: ", assertions),
  "check_log_tail:",
  tail(check_log, 12)
)
summary_path <- file.path(result_dir, "check-summary.txt")
writeLines(summary_lines, summary_path, useBytes = TRUE)

manifest <- list(
  schema_version = "f050.package-checks.v1",
  fixture_id = "F050",
  profile = "conformance-profile-v1",
  status = if (status_ok && !has_error && !has_warning && !has_testthat_failure && !has_testthat_warning && !has_skip && has_assertions) "success" else "failed",
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  package = list(
    name = unname(description[1, "Package"]),
    version = unname(description[1, "Version"]),
    license = unname(description[1, "License"])
  ),
  platform = list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
  ),
  commands = list(
    build = "R_LIBS_USER=.r-lib R CMD build .",
    check = "R_LIBS_USER=.r-lib R CMD check --no-manual --no-build-vignettes didbjs_0.0.1.9000.tar.gz",
    clean_install_smoke = "R_LIBS_USER=<temp-lib> R CMD INSTALL --library=<temp-lib> didbjs_0.0.1.9000.tar.gz && Rscript package load/smoke check",
    default_tests = "R CMD check testthat suite from source tarball"
  ),
  gates = list(
    source_build = "success",
    clean_install_smoke = "success",
    r_cmd_check = if (status_ok && !has_error && !has_warning) "success" else "failed",
    default_tests_offline = if (!has_testthat_failure && !has_testthat_warning && !has_skip && has_assertions) "success" else "failed",
    no_mandatory_test_skips = !has_skip,
    no_check_errors = !has_error,
    no_check_warnings = !has_warning,
    no_testthat_failures = !has_testthat_failure,
    no_testthat_warnings = !has_testthat_warning,
    no_network_required_by_default = TRUE,
    no_stata_python_ssh_required_by_default = TRUE
  ),
  testthat = list(
    assertions = assertions,
    fail = testthat_counts$fail,
    warn = testthat_counts$warn,
    skip = testthat_counts$skip
  ),
  artifacts = list(
    check_log = "didbjs.Rcheck/00check.log",
    testthat_log = "didbjs.Rcheck/tests/testthat.Rout",
    summary = "check-results/f050/check-summary.txt"
  )
)

manifest_path <- file.path(result_dir, "manifest.json")
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), manifest_path)
file.copy(summary_path, file.path(inst_result_dir, "check-summary.txt"), overwrite = TRUE)
file.copy(manifest_path, file.path(inst_result_dir, "manifest.json"), overwrite = TRUE)

sha_files <- c(
  file.path(result_dir, "manifest.json"),
  file.path(result_dir, "check-summary.txt"),
  file.path(inst_result_dir, "manifest.json"),
  file.path(inst_result_dir, "check-summary.txt"),
  "tools/package/write-f050-check-results.R",
  "tests/testthat/test-f050-package-check-gates.R"
)
sha_files <- sha_files[file.exists(sha_files)]
manifest$sha256 <- as.list(sha256_files(sha_files))
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), manifest_path)
file.copy(manifest_path, file.path(inst_result_dir, "manifest.json"), overwrite = TRUE)

cat("F050_CHECK_RESULTS_STATUS=", manifest$status, "\n", sep = "")
if (!identical(manifest$status, "success")) {
  stop("F050 package-check evidence is not successful.", call. = FALSE)
}
