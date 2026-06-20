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
  "tools/parity/generators/f028-weight-scaling/kyle-export.R"
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

cat("F028_KYLE_INPUT=", input_csv, "\n", sep = "")
cat("F028_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

run_reference <- function(data, wname = "w") {
  did_imputation(
    data = data,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = wname,
    cluster_var = "unit"
  )
}

probe <- function(panel, mutate) {
  candidate <- panel
  candidate <- mutate(candidate)
  out <- tryCatch(run_reference(candidate), error = function(e) e)
  if (inherits(out, "error")) {
    list(
      status = "reference_error",
      error_class = class(out),
      error_message = conditionMessage(out)
    )
  } else {
    list(
      status = "reference_success",
      estimate = out$estimate[out$term == "treat"][[1]],
      std_error = out$std.error[out$term == "treat"][[1]],
      nrow = nrow(out)
    )
  }
}

panel <- read.csv(input_csv, na.strings = c("", "NA"), check.names = FALSE)
base <- run_reference(panel, "w")
scaled <- run_reference(panel, "w_scaled")

estimates <- rbind(
  cbind(scenario = "base", base),
  cbind(scenario = "scaled", scaled)
)
write.csv(estimates, file.path(output_dir, "estimates.csv"), row.names = FALSE, na = "NA")

invalid <- list(
  missing_weight = probe(panel, function(x) {
    x$w[x$row_id == "2_3"] <- NA_real_
    x
  }),
  zero_weight = probe(panel, function(x) {
    x$w[x$row_id == "1_3"] <- 0
    x
  }),
  negative_weight = probe(panel, function(x) {
    x$w[x$row_id == "1_3"] <- -1
    x
  }),
  infinite_weight = probe(panel, function(x) {
    x$w[x$row_id == "1_3"] <- Inf
    x
  }),
  all_zero_weight = probe(panel, function(x) {
    x$w <- 0
    x
  })
)

base_estimate <- base$estimate[base$term == "treat"][[1]]
scaled_estimate <- scaled$estimate[scaled$term == "treat"][[1]]
base_se <- base$std.error[base$term == "treat"][[1]]
scaled_se <- scaled$std.error[scaled$term == "treat"][[1]]

schema <- list(
  class = class(base),
  names = names(base),
  nrow = nrow(base),
  public_call_status = "success"
)
diagnostics <- list(
  status = "success",
  base_command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")',
  scaled_command = 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w_scaled", cluster_var = "unit")',
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_sha256 = sha256(input_csv),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  base_estimate = base_estimate,
  scaled_estimate = scaled_estimate,
  base_std_error = base_se,
  scaled_std_error = scaled_se,
  estimate_scale_abs_diff = abs(base_estimate - scaled_estimate),
  std_error_scale_abs_diff = abs(base_se - scaled_se)
)

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(invalid, file.path(output_dir, "invalid-probes.json"), auto_unbox = TRUE, pretty = TRUE)

cat("F028_KYLE_EXPORT_OK=1\n")
