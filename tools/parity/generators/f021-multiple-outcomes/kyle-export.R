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
  "tools/parity/generators/f021-multiple-outcomes/kyle-export.R"
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

cat("F021_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F021_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel <- read.csv(input_csv, na.strings = c("", "NA"), check.names = FALSE)

run_kyle <- function(yname) {
  did_imputation(
    data = panel,
    yname = yname,
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  )
}

multi <- run_kyle("c(Y, Y2)")
single_y <- run_kyle("Y")
single_y2 <- run_kyle("Y2")

stopifnot(inherits(multi, "data.table"))
stopifnot(identical(names(multi), c("lhs", "term", "estimate", "std.error", "conf.low", "conf.high")))
stopifnot(identical(as.character(multi$lhs), c("Y", "Y2")))
stopifnot(identical(as.character(multi$term), c("treat", "treat")))

multi_y <- multi[multi$lhs == "Y", ]
multi_y2 <- multi[multi$lhs == "Y2", ]
if (abs(multi_y$estimate - single_y$estimate) > 1e-12 ||
    abs(multi_y$std.error - single_y$std.error) > 1e-10 ||
    abs(multi_y2$estimate - single_y2$estimate) > 1e-12 ||
    abs(multi_y2$std.error - single_y2$std.error) > 1e-10) {
  stop("F021 multi-LHS output does not match single-outcome probes", call. = FALSE)
}
if (abs(multi_y$estimate - multi_y2$estimate) < 1) {
  stop("F021 fixture outcomes are not separated enough to test contamination", call. = FALSE)
}

write.csv(multi, file.path(output_dir, "multi-estimates.csv"), row.names = FALSE)
write.csv(single_y, file.path(output_dir, "single-Y-estimates.csv"), row.names = FALSE)
write.csv(single_y2, file.path(output_dir, "single-Y2-estimates.csv"), row.names = FALSE)

schema <- list(
  class = class(multi),
  names = names(multi),
  nrow = nrow(multi),
  lhs = as.character(multi$lhs),
  term = as.character(multi$term)
)
diagnostics <- list(
  status = "success",
  command = 'did_imputation(data = panel, yname = "c(Y, Y2)", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  single_y_command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  single_y2_command = 'did_imputation(data = panel, yname = "Y2", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  single_probe_match = TRUE,
  estimate_gap = abs(multi_y2$estimate - multi_y$estimate)
)

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)

cat("F021_KYLE_EXPORT_OK=1\n")
