#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("usage: kyle-export.R <input_dir> <output_dir>", call. = FALSE)
}

input_dir <- args[[1]]
output_dir <- args[[2]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
generator_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "tools/parity/generators/f046-differential/kyle-export.R"
}

sha256 <- function(path) {
  tool <- Sys.which("shasum")
  if (nzchar(tool)) {
    out <- system2(tool, c("-a", "256", path), stdout = TRUE)
  } else {
    tool <- Sys.which("sha256sum")
    if (!nzchar(tool)) {
      stop("Neither shasum nor sha256sum is available", call. = FALSE)
    }
    out <- system2(tool, path, stdout = TRUE)
  }
  strsplit(out[[1]], "[[:space:]]+")[[1]][[1]]
}

log_path <- file.path(output_dir, "run.log")
log_con <- file(log_path, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("F046_KYLE_INPUT_DIR=", input_dir, "\n", sep = "")
cat("F046_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

scenarios <- read.csv(file.path(input_dir, "scenarios.csv"), na.strings = c("", "NA"), check.names = FALSE)
panel <- read.csv(file.path(input_dir, "panels.csv"), na.strings = c("", "NA"), check.names = FALSE)

estimate_rows <- list()
failure_rows <- list()

for (idx in seq_len(nrow(scenarios))) {
  spec <- scenarios[idx, , drop = FALSE]
  scenario <- spec$scenario[[1]]
  estimand <- spec$estimand[[1]]
  scenario_panel <- panel[panel$scenario == scenario, , drop = FALSE]
  horizon_arg <- if (identical(estimand, "dynamic")) 0:2 else NULL
  wname_arg <- if (identical(as.integer(spec$weighted[[1]]), 1L)) "w" else NULL
  result <- tryCatch(
    did_imputation(
      data = scenario_panel,
      yname = "Y",
      gname = "Ei",
      tname = "t",
      idname = "unit",
      wname = wname_arg,
      horizon = horizon_arg,
      cluster_var = "unit"
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    failure_rows[[length(failure_rows) + 1L]] <- data.frame(
      scenario = scenario,
      reference = "kyle",
      failure_class = class(result)[[1]],
      failure_message = conditionMessage(result),
      retained_fixture_path = "tests/fixtures/parity/f046-differential/inputs/panels.csv",
      stringsAsFactors = FALSE
    )
    cat("F046_KYLE_FAILURE ", scenario, " ", class(result)[[1]], ": ", conditionMessage(result), "\n", sep = "")
    next
  }
  estimate_rows[[length(estimate_rows) + 1L]] <- data.frame(
    scenario = scenario,
    estimand = estimand,
    result,
    check.names = FALSE
  )
}

estimates <- if (length(estimate_rows)) {
  data.table::rbindlist(estimate_rows, use.names = TRUE, fill = TRUE)
} else {
  data.frame(
    scenario = character(),
    estimand = character(),
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    stringsAsFactors = FALSE
  )
}
failures <- if (length(failure_rows)) {
  data.table::rbindlist(failure_rows, use.names = TRUE, fill = TRUE)
} else {
  data.frame(
    scenario = character(),
    reference = character(),
    failure_class = character(),
    failure_message = character(),
    retained_fixture_path = character(),
    stringsAsFactors = FALSE
  )
}

write.csv(estimates, file.path(output_dir, "estimates.csv"), row.names = FALSE)
write.csv(failures, file.path(output_dir, "failures.csv"), row.names = FALSE)

diagnostics <- list(
  status = if (nrow(failures) == 0) "success" else "reference_failures",
  command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = <scenario>, horizon = <scenario>, cluster_var = "unit")',
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  scenario_count = nrow(scenarios),
  static_count = sum(scenarios$estimand == "static"),
  dynamic_count = sum(scenarios$estimand == "dynamic"),
  weighted_count = sum(scenarios$weighted == 1),
  estimate_rows = nrow(estimates),
  failure_count = nrow(failures),
  input_sha256 = list(
    panels = sha256(file.path(input_dir, "panels.csv")),
    scenarios = sha256(file.path(input_dir, "scenarios.csv"))
  ),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE))
)

jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
cat("F046_KYLE_EXPORT_OK=1\n")
quit(status = if (nrow(failures) == 0) 0 else 1)
