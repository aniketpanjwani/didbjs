#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("usage: kyle-export.R <input_dir> <output_dir>", call. = FALSE)
}

input_dir <- args[[1]]
output_dir <- args[[2]]
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(didimputation)
})

sha256 <- function(path) {
  line <- system2("shasum", c("-a", "256", path), stdout = TRUE)
  sub(" .*", "", line)
}

write_json <- function(path, x) {
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  cat("\n", file = path, append = TRUE)
}

generator_path <- "tools/parity/generators/f045-examples/kyle-export.R"

as_dt <- function(x, example) {
  out <- as.data.table(x)
  out[, example := example]
  setcolorder(out, c("example", setdiff(names(out), "example")))
  out[]
}

data("df_het", package = "didimputation")
setDT(df_het)
input_path <- file.path(input_dir, "kyle-df-het.csv")
fwrite(df_het, input_path)

static <- did_imputation(
  data = df_het,
  yname = "dep_var",
  gname = "g",
  tname = "year",
  idname = "unit"
)
event_explicit <- did_imputation(
  data = df_het,
  yname = "dep_var",
  gname = "g",
  tname = "year",
  idname = "unit",
  horizon = 0:7,
  pretrends = -5:-1
)
horizon_true <- tryCatch(
  {
    value <- did_imputation(
      data = df_het,
      yname = "dep_var",
      gname = "g",
      tname = "year",
      idname = "unit",
      horizon = TRUE,
      pretrends = -5:-1
    )
    list(status = "success", rows = nrow(as.data.table(value)), terms = as.character(as.data.table(value)$term))
  },
  error = function(err) {
    list(status = "error", error_class = class(err)[[1]], message = conditionMessage(err))
  }
)

estimates <- rbindlist(
  list(
    as_dt(static, "readme_static"),
    as_dt(event_explicit, "readme_event_explicit_0_7_pre5")
  ),
  use.names = TRUE,
  fill = TRUE
)
fwrite(estimates, file.path(output_dir, "estimates.csv"))

schema <- list(
  readme_static = list(
    class = class(static),
    columns = names(as.data.table(static)),
    rows = nrow(as.data.table(static)),
    terms = as.character(as.data.table(static)$term)
  ),
  readme_event_explicit_0_7_pre5 = list(
    class = class(event_explicit),
    columns = names(as.data.table(event_explicit)),
    rows = nrow(as.data.table(event_explicit)),
    terms = as.character(as.data.table(event_explicit)$term)
  ),
  readme_event_horizon_true = horizon_true
)
write_json(file.path(output_dir, "output-schema.json"), schema)
write_json(
  file.path(output_dir, "diagnostics.json"),
  list(
    status = "success",
    source = "pinned Kyle README/package examples",
    package_version = as.character(utils::packageVersion("didimputation")),
    input_sha256 = sha256(input_path),
    generator_sha256 = sha256(generator_path),
    examples = c("readme_static", "readme_event_explicit_0_7_pre5", "readme_event_horizon_true"),
    d023_horizon_true_policy = "didbjs rejects horizon = TRUE for RC-v1 while retaining explicit numeric horizon parity"
  )
)

cat("F045_KYLE_EXPORT_OK=1\n")
