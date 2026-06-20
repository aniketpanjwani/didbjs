#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("usage: kyle-export.R <input_csv> <duplicate_csv> <output_dir>", call. = FALSE)
}

input_csv <- args[[1]]
duplicate_csv <- args[[2]]
output_dir <- args[[3]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
generator_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "tools/parity/generators/f027-irregular-unbalanced/kyle-export.R"
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

cat("F027_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F027_KYLE_DUPLICATE_INPUT=", duplicate_csv, "\n", sep = "")
cat("F027_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

run_reference <- function(data) {
  did_imputation(
    data = data,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    cluster_var = "unit"
  )
}

panel <- read.csv(input_csv, na.strings = c("", "NA"), check.names = FALSE)
output <- run_reference(panel)
write.csv(output, file.path(output_dir, "estimates.csv"), row.names = FALSE, na = "NA")

duplicate_panel <- read.csv(duplicate_csv, na.strings = c("", "NA"), check.names = FALSE)
duplicate_rows <- sum(duplicated(duplicate_panel[c("unit", "t")]) | duplicated(duplicate_panel[c("unit", "t")], fromLast = TRUE))
duplicate_call <- tryCatch(run_reference(duplicate_panel), error = function(e) e)
if (inherits(duplicate_call, "error")) {
  duplicate_probe <- list(
    status = "reference_error",
    duplicate_unit_time_rows = duplicate_rows,
    error_class = class(duplicate_call),
    error_message = conditionMessage(duplicate_call)
  )
} else {
  duplicate_probe <- list(
    status = "reference_success_with_duplicates",
    duplicate_unit_time_rows = duplicate_rows,
    estimate = duplicate_call$estimate[[1]],
    nrow = nrow(duplicate_call)
  )
}

schema <- list(
  class = class(output),
  names = names(output),
  nrow = nrow(output),
  term = as.character(output$term)
)
diagnostics <- list(
  status = "success",
  command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", cluster_var = "unit")',
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  duplicate_input_sha256 = sha256(duplicate_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  rows_input = nrow(panel),
  terms = as.character(output$term),
  algebraic_att = 2
)

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(duplicate_probe, file.path(output_dir, "duplicate-probe.json"), auto_unbox = TRUE, pretty = TRUE)

cat("F027_KYLE_EXPORT_OK=1\n")
