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
  "tools/parity/generators/f007-analytic-weights/kyle-export.R"
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

cat("F007_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F007_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel <- read.csv(input_csv, na.strings = c("", "NA"))

weighted <- tryCatch(
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  ),
  error = function(e) e
)
unweighted <- tryCatch(
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    cluster_var = "unit"
  ),
  error = function(e) e
)

diagnostics <- list(
  status = if (inherits(weighted, "error")) "reference_error" else "success",
  command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  unweighted_probe_status = if (inherits(unweighted, "error")) "reference_error" else "success",
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE))
)

if (inherits(weighted, "error")) {
  diagnostics$error_class <- class(weighted)
  diagnostics$error_message <- conditionMessage(weighted)
  write.csv(
    data.frame(term = character(), estimate = numeric(), std.error = numeric(), conf.low = numeric(), conf.high = numeric()),
    file.path(output_dir, "estimates.csv"),
    row.names = FALSE
  )
  schema <- list(
    class = "error",
    public_call_status = "reference_error",
    error_class = class(weighted),
    error_message = conditionMessage(weighted)
  )
} else {
  write.csv(weighted, file.path(output_dir, "estimates.csv"), row.names = FALSE)
  schema <- list(
    class = class(weighted),
    names = names(weighted),
    nrow = nrow(weighted),
    public_call_status = "success"
  )
  diagnostics$estimate <- weighted$estimate[weighted$term == "treat"][1]
  diagnostics$std_error <- weighted$std.error[weighted$term == "treat"][1]
  cat("F007_KYLE_EXPORT_OK=1\n")
}

if (inherits(unweighted, "error")) {
  diagnostics$unweighted_error_class <- class(unweighted)
  diagnostics$unweighted_error_message <- conditionMessage(unweighted)
} else if (!inherits(weighted, "error")) {
  diagnostics$unweighted_estimate <- unweighted$estimate[unweighted$term == "treat"][1]
  diagnostics$weighted_unweighted_abs_diff <- abs(diagnostics$estimate - diagnostics$unweighted_estimate)
  if (diagnostics$weighted_unweighted_abs_diff < 1e-6) {
    stop("F007 Kyle analytic-weight probe did not change the estimate enough", call. = FALSE)
  }
}

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)
