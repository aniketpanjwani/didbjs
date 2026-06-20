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
  "tools/parity/generators/f040-kyle-compatibility/kyle-export.R"
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

write_empty_estimates <- function(path, lhs = FALSE) {
  cols <- if (isTRUE(lhs)) {
    data.frame(
      lhs = character(),
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      conf.low = numeric(),
      conf.high = numeric()
    )
  } else {
    data.frame(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      conf.low = numeric(),
      conf.high = numeric()
    )
  }
  write.csv(cols, path, row.names = FALSE)
}

schema_for <- function(x) {
  if (inherits(x, "error")) {
    return(list(
      status = "reference_error",
      class = class(x),
      error_message = conditionMessage(x)
    ))
  }
  list(
    status = "success",
    class = class(x),
    names = names(x),
    nrow = nrow(x),
    terms = as.character(x$term),
    lhs = if ("lhs" %in% names(x)) as.character(x$lhs) else NULL
  )
}

run_probe <- function(label, command, path, lhs = FALSE) {
  result <- tryCatch(force(command), error = function(e) e)
  if (inherits(result, "error")) {
    write_empty_estimates(path, lhs = lhs)
  } else {
    write.csv(result, path, row.names = FALSE)
  }
  result
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

cat("F040_KYLE_INPUT_DIR=", input_dir, "\n", sep = "")
cat("F040_KYLE_OUTPUT=", output_dir, "\n", sep = "")
cat("R_VERSION=", as.character(getRversion()), "\n", sep = "")

suppressPackageStartupMessages(library(didimputation))

panel_path <- file.path(input_dir, "panel.csv")
collision_path <- file.path(input_dir, "id-collision.csv")
panel <- read.csv(panel_path, na.strings = c("", "NA"), check.names = FALSE)
collision <- read.csv(collision_path, na.strings = c("", "NA"), check.names = FALSE)

probes <- list()
commands <- list()

commands$named_default <- 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")'
probes$named_default <- run_probe(
  "named_default",
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  ),
  file.path(output_dir, "named-default-estimates.csv")
)

commands$positional_dynamic_pretrends <- 'did_imputation(panel, "Y", "Ei", "t", "unit", NULL, "w", NULL, 0:1, c(-2, -1), "unit")'
probes$positional_dynamic_pretrends <- run_probe(
  "positional_dynamic_pretrends",
  did_imputation(panel, "Y", "Ei", "t", "unit", NULL, "w", NULL, 0:1, c(-2, -1), "unit"),
  file.path(output_dir, "pos-dyn-pre-estimates.csv")
)

commands$first_stage_formula <- 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", first_stage = ~ x1 + x2 | unit + t, wname = "w", cluster_var = "unit")'
probes$first_stage_formula <- run_probe(
  "first_stage_formula",
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    first_stage = ~ x1 + x2 | unit + t,
    wname = "w",
    cluster_var = "unit"
  ),
  file.path(output_dir, "first-stage-formula-estimates.csv")
)

commands$first_stage_string <- 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", first_stage = "x1 + x2 | unit + t", wname = "w", cluster_var = "unit")'
probes$first_stage_string <- run_probe(
  "first_stage_string",
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    first_stage = "x1 + x2 | unit + t",
    wname = "w",
    cluster_var = "unit"
  ),
  file.path(output_dir, "first-stage-string-estimates.csv")
)

commands$wtr <- 'did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", wtr = c("wtr_early", "wtr_late"), cluster_var = "unit")'
probes$wtr <- run_probe(
  "wtr",
  did_imputation(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    wtr = c("wtr_early", "wtr_late"),
    cluster_var = "unit"
  ),
  file.path(output_dir, "wtr-estimates.csv")
)

commands$multi <- 'did_imputation(data = panel, yname = "c(Y, Y2)", gname = "Ei", tname = "t", idname = "unit", first_stage = ~ x1 + x2 | unit + t, wname = "w", cluster_var = "unit")'
probes$multi <- run_probe(
  "multi",
  did_imputation(
    data = panel,
    yname = "c(Y, Y2)",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    first_stage = ~ x1 + x2 | unit + t,
    wname = "w",
    cluster_var = "unit"
  ),
  file.path(output_dir, "multi-estimates.csv"),
  lhs = TRUE
)

commands$id_collision <- 'did_imputation(data = collision, yname = "Y", gname = "Ei", tname = "t", idname = "i", wname = "w", cluster_var = "i")'
id_collision <- tryCatch(
  did_imputation(
    data = collision,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "i",
    wname = "w",
    cluster_var = "i"
  ),
  error = function(e) e
)
probes$id_collision <- id_collision
if (inherits(id_collision, "error")) {
  write_empty_estimates(file.path(output_dir, "id-collision-estimates.csv"))
} else {
  write.csv(id_collision, file.path(output_dir, "id-collision-estimates.csv"), row.names = FALSE)
}

alias_panel <- collision
alias_panel$unit_id <- alias_panel$i
alias_panel$i <- NULL
commands$id_collision_alias <- 'did_imputation(data = within(transform(collision, unit_id = i), rm(i)), yname = "Y", gname = "Ei", tname = "t", idname = "unit_id", wname = "w", cluster_var = "unit_id")'
probes$id_collision_alias <- run_probe(
  "id_collision_alias",
  did_imputation(
    data = alias_panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit_id",
    wname = "w",
    cluster_var = "unit_id"
  ),
  file.path(output_dir, "id-collision-alias-estimates.csv")
)

required_success <- setdiff(names(probes), "id_collision")
failed <- required_success[vapply(probes[required_success], inherits, logical(1), what = "error")]
if (length(failed) > 0) {
  stop("F040 Kyle required probes failed: ", paste(failed, collapse = ", "), call. = FALSE)
}
if (!inherits(id_collision, "error")) {
  stop("F040 expected the pinned Kyle idname = i public call to fail for D016 evidence.", call. = FALSE)
}

schema <- lapply(probes, schema_for)
diagnostics <- list(
  status = "success",
  commands = commands,
  r_version = as.character(getRversion()),
  didimputation_version = as.character(utils::packageVersion("didimputation")),
  data_table_version = as.character(utils::packageVersion("data.table")),
  matrix_version = as.character(utils::packageVersion("Matrix")),
  fixest_version = as.character(utils::packageVersion("fixest")),
  input_hashes = list(
    panel = sha256(panel_path),
    id_collision = sha256(collision_path)
  ),
  generator_sha256 = sha256(normalizePath(generator_path, mustWork = FALSE)),
  id_collision_status = schema$id_collision$status,
  id_collision_error_message = conditionMessage(id_collision),
  id_collision_alias_status = schema$id_collision_alias$status,
  first_stage_formula_string_match = isTRUE(all.equal(
    probes$first_stage_formula[, c("term", "estimate", "std.error", "conf.low", "conf.high")],
    probes$first_stage_string[, c("term", "estimate", "std.error", "conf.low", "conf.high")],
    tolerance = 1e-12,
    check.attributes = FALSE
  ))
)

jsonlite::write_json(schema, file.path(output_dir, "output-schema.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
jsonlite::write_json(diagnostics, file.path(output_dir, "diagnostics.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)

cat("F040_KYLE_EXPORT_OK=1\n")
