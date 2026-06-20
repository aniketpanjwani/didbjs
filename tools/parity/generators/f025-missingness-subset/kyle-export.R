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
  "tools/parity/generators/f025-missingness-subset/kyle-export.R"
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

cat("F025_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F025_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel <- read.csv(input_csv, na.strings = c("", "NA"), check.names = FALSE)
subset_panel <- panel[panel$keep == 1, , drop = FALSE]

output <- did_imputation(
  data = subset_panel,
  yname = "Y",
  gname = "Ei",
  tname = "t",
  idname = "unit",
  wname = "w",
  cluster_var = "clust"
)

write.csv(output, file.path(output_dir, "estimates.csv"), row.names = FALSE, na = "NA")

schema <- list(
  class = class(output),
  names = names(output),
  nrow = nrow(output),
  term = as.character(output$term)
)
diagnostics <- list(
  status = "success",
  command = 'did_imputation(data = panel[panel$keep == 1, ], yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "clust")',
  subset_source = "prefilter keep == 1 because pinned Kyle has no subset argument",
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  rows_input = nrow(panel),
  rows_after_subset = nrow(subset_panel),
  rows_dropped_by_subset = nrow(panel) - nrow(subset_panel),
  terms = as.character(output$term),
  std_error_is_na = is.na(output$std.error)
)

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)

cat("F025_KYLE_EXPORT_OK=1\n")
