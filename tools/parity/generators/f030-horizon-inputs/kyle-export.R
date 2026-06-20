#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("usage: kyle-export.R <input_csv> <output_dir>", call. = FALSE)
}

input_csv <- args[[1]]
output_dir <- args[[2]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
generator_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "tools/parity/generators/f030-horizon-inputs/kyle-export.R"
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

json_safe <- function(value) {
  if (is.numeric(value)) {
    value[!is.finite(value)] <- NA_real_
  }
  value
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

cat("F030_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F030_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel <- read.csv(input_csv, na.strings = c("", "NA"))

scenarios <- list(
  unsorted = list(horizon = c(2, 0), command = "horizon = c(2, 0)"),
  sparse = list(horizon = c(0, 2), command = "horizon = c(0, 2)"),
  absent = list(horizon = c(0, 3), command = "horizon = c(0, 3)"),
  duplicate = list(horizon = c(0, 0), command = "horizon = c(0, 0)"),
  negative = list(horizon = -1, command = "horizon = -1"),
  empty = list(horizon = integer(0), command = "horizon = integer(0)"),
  all_horizons_true = list(horizon = TRUE, command = "horizon = TRUE")
)

estimate_rows <- data.frame(
  scenario = character(),
  term = character(),
  estimate = numeric(),
  std.error = numeric(),
  conf.low = numeric(),
  conf.high = numeric()
)
schemas <- list()
probes <- list()

for (scenario_name in names(scenarios)) {
  scenario <- scenarios[[scenario_name]]
  cat("SCENARIO=", scenario_name, "\n", sep = "")
  result <- tryCatch(
    did_imputation(
      data = panel,
      yname = "Y",
      gname = "Ei",
      tname = "t",
      idname = "unit",
      wname = "w",
      horizon = scenario$horizon,
      cluster_var = "unit"
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    probes[[scenario_name]] <- list(
      status = "reference_error",
      command = paste0(
        'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", ',
        'idname = "unit", wname = "w", ',
        scenario$command,
        ', cluster_var = "unit")'
      ),
      error_class = class(result),
      error_message = conditionMessage(result)
    )
    schemas[[scenario_name]] <- list(
      class = "error",
      public_call_status = "reference_error",
      error_class = class(result),
      error_message = conditionMessage(result)
    )
  } else {
    result_out <- as.data.frame(result)
    result_out$scenario <- scenario_name
    result_out <- result_out[, c("scenario", "term", "estimate", "std.error", "conf.low", "conf.high")]
    estimate_rows <- rbind(estimate_rows, result_out)
    probes[[scenario_name]] <- list(
      status = "reference_success",
      command = paste0(
        'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", ',
        'idname = "unit", wname = "w", ',
        scenario$command,
        ', cluster_var = "unit")'
      ),
      terms = as.character(result$term),
      estimates = as.list(json_safe(as.numeric(result$estimate))),
      std_errors = as.list(json_safe(as.numeric(result$std.error)))
    )
    names(probes[[scenario_name]]$estimates) <- as.character(result$term)
    names(probes[[scenario_name]]$std_errors) <- as.character(result$term)
    schemas[[scenario_name]] <- list(
      class = class(result),
      names = names(result),
      nrow = nrow(result),
      public_call_status = "success"
    )
  }
}

utils::write.csv(estimate_rows, file.path(output_dir, "estimates.csv"), row.names = FALSE)
jsonlite::write_json(schemas, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(probes, file.path(output_dir, "probes.json"), auto_unbox = TRUE, pretty = TRUE)

diagnostics <- list(
  status = "success",
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  scenario_status = lapply(probes, `[[`, "status"),
  source = "pinned Kyle didimputation horizon boundary probes"
)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)

cat("F030_KYLE_EXPORT_OK=1\n")
