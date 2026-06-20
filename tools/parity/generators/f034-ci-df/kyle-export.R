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
  "tools/parity/generators/f034-ci-df/kyle-export.R"
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

cat("F034_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F034_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel <- read.csv(input_csv, na.strings = c("", "NA"), check.names = FALSE)

out <- did_imputation(
  data = panel,
  yname = "Y",
  gname = "Ei",
  tname = "t",
  idname = "unit",
  wname = "w",
  cluster_var = "unit"
)

stopifnot(inherits(out, "data.table"))
stopifnot(identical(names(out), c("term", "estimate", "std.error", "conf.low", "conf.high")))
stopifnot(identical(as.character(out$term), "treat"))
if (abs(out$conf.low - (out$estimate - 1.96 * out$std.error)) > 1e-12 ||
    abs(out$conf.high - (out$estimate + 1.96 * out$std.error)) > 1e-12) {
  stop("F034 Kyle CI schema is not using estimate +/- 1.96 * std.error", call. = FALSE)
}

write.csv(out, file.path(output_dir, "estimates.csv"), row.names = FALSE)

schema <- list(
  class = class(out),
  names = names(out),
  nrow = nrow(out),
  term = as.character(out$term)
)
diagnostics <- list(
  status = "success",
  command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  ci_formula = "estimate +/- 1.96 * std.error",
  critical_value = 1.96,
  exact_normal_critical_95 = unname(stats::qnorm(0.975)),
  exact_normal_critical_gap = abs(1.96 - unname(stats::qnorm(0.975)))
)

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)

cat("F034_KYLE_EXPORT_OK=1\n")
