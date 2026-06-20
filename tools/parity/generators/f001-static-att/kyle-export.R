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
  "tools/parity/generators/f001-static-att/kyle-export.R"
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

cat("F001_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F001_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel <- read.csv(input_csv, na.strings = c("", "NA"))

run_kyle <- function(data, idname, cluster_var) {
  tryCatch(
    did_imputation(
      data = data,
      yname = "Y",
      gname = "Ei",
      tname = "t",
      idname = idname,
      wname = "w",
      cluster_var = cluster_var
    ),
    error = function(e) e
  )
}

write_empty_estimates <- function(path) {
  write.csv(
    data.frame(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      conf.low = numeric(),
      conf.high = numeric()
    ),
    path,
    row.names = FALSE
  )
}

public_call <- tryCatch(
  run_kyle(panel, idname = "i", cluster_var = "i"),
  error = function(e) e
)

alias_panel <- panel
alias_panel$unit_id <- alias_panel$i
alias_panel$i <- NULL
alias_call <- run_kyle(alias_panel, idname = "unit_id", cluster_var = "unit_id")

diagnostics <- list(
  status = if (inherits(public_call, "error")) "reference_error" else "success",
  command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "i", wname = "w", cluster_var = "i")',
  alias_probe_command = 'did_imputation(data = within(transform(panel, unit_id = i), rm(i)), yname = "Y", gname = "Ei", tname = "t", idname = "unit_id", wname = "w", cluster_var = "unit_id")',
  alias_probe_status = if (inherits(alias_call, "error")) "reference_error" else "success",
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE))
)

if (inherits(public_call, "error")) {
  diagnostics$error_class <- class(public_call)
  diagnostics$error_message <- conditionMessage(public_call)
  diagnostics$root_cause <- "Kyle didimputation se_inner() uses for-loop variable i inside data.table NSE; F001 also has a data column named i, so v_star[, i] resolves i as the unit-id column rather than the loop counter."
  diagnostics$root_cause_probe <- "Renaming the Kyle id/cluster column from i to unit_id and removing the original i column removes the NSE name collision while preserving the same rows, outcome, treatment timing, time, and weights."
  write_empty_estimates(file.path(output_dir, "estimates.csv"))
  schema <- list(
    class = "error",
    public_call_status = "reference_error",
    error_class = class(public_call),
    error_message = conditionMessage(public_call),
    root_cause = diagnostics$root_cause
  )
} else {
  estimate <- public_call$estimate[public_call$term == "treat"][1]
  if (is.na(estimate) || abs(estimate - 2) > 1e-10) {
    stop("F001 Kyle static ATT assertion failed: ", estimate, call. = FALSE)
  }
  write.csv(public_call, file.path(output_dir, "estimates.csv"), row.names = FALSE)
  schema <- list(
    class = class(public_call),
    names = names(public_call),
    nrow = nrow(public_call),
    public_call_status = "success"
  )
  cat("F001_KYLE_EXPORT_OK=1\n")
}

if (inherits(alias_call, "error")) {
  diagnostics$alias_probe_error_class <- class(alias_call)
  diagnostics$alias_probe_error_message <- conditionMessage(alias_call)
  write_empty_estimates(file.path(output_dir, "alias-estimates.csv"))
  alias_schema <- list(
    class = "error",
    public_call_status = "reference_error",
    error_class = class(alias_call),
    error_message = conditionMessage(alias_call)
  )
} else {
  alias_estimate <- alias_call$estimate[alias_call$term == "treat"][1]
  if (is.na(alias_estimate) || abs(alias_estimate - 2) > 1e-10) {
    stop("F001 Kyle alias-probe static ATT assertion failed: ", alias_estimate, call. = FALSE)
  }
  write.csv(alias_call, file.path(output_dir, "alias-estimates.csv"), row.names = FALSE)
  alias_schema <- list(
    class = class(alias_call),
    names = names(alias_call),
    nrow = nrow(alias_call),
    public_call_status = "success"
  )
  cat("F001_KYLE_ALIAS_EXPORT_OK=1\n")
}

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(alias_schema, file.path(output_dir, "alias-output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)
