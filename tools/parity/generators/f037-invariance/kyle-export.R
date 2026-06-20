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
  "tools/parity/generators/f037-invariance/kyle-export.R"
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

cat("F037_KYLE_INPUT_DIR=", input_dir, "\n", sep = "")
cat("F037_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

scenarios <- c("base", "row_permuted", "unit_relabel", "time_shift", "outcome_scaled", "constant_shift", "weight_scaled")

run_reference <- function(scenario) {
  panel <- read.csv(file.path(input_dir, paste0(scenario, ".csv")), na.strings = c("", "NA"), check.names = FALSE)
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  )
}

outputs <- lapply(scenarios, run_reference)
names(outputs) <- scenarios
estimates <- data.table::rbindlist(lapply(scenarios, function(scenario) {
  cbind(scenario = scenario, outputs[[scenario]])
}), use.names = TRUE)
write.csv(estimates, file.path(output_dir, "estimates.csv"), row.names = FALSE)

base <- outputs$base
base_estimate <- base$estimate[base$term == "treat"][[1]]
base_se <- base$std.error[base$term == "treat"][[1]]
scaled <- outputs$outcome_scaled
scaled_estimate <- scaled$estimate[scaled$term == "treat"][[1]]
scaled_se <- scaled$std.error[scaled$term == "treat"][[1]]

diagnostics <- list(
  status = "success",
  command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_hashes = stats::setNames(vapply(scenarios, function(scenario) sha256(file.path(input_dir, paste0(scenario, ".csv"))), character(1)), scenarios),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  base_estimate = base_estimate,
  row_permutation_abs_diff = abs(outputs$row_permuted$estimate[[1]] - base_estimate),
  unit_relabel_abs_diff = abs(outputs$unit_relabel$estimate[[1]] - base_estimate),
  time_shift_abs_diff = abs(outputs$time_shift$estimate[[1]] - base_estimate),
  constant_shift_abs_diff = abs(outputs$constant_shift$estimate[[1]] - base_estimate),
  weight_scale_abs_diff = abs(outputs$weight_scaled$estimate[[1]] - base_estimate),
  outcome_scale = 3.5,
  outcome_scaled_estimate_ratio = scaled_estimate / base_estimate,
  outcome_scaled_se_ratio = scaled_se / base_se
)

jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)

cat("F037_KYLE_EXPORT_OK=1\n")
