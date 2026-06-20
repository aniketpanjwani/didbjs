skip_if_not(fixtures_present())

f046_fixture <- function(...) {
  fixture_path("parity", "f046-differential", ...)
}

f046_panel <- function(scenario) {
  panel <- read_fixture_csv("parity", "f046-differential", "inputs", "panels.csv")
  panel[panel$scenario == scenario, c("row_id", "unit", "t", "Ei", "Y", "w"), drop = FALSE]
}

f046_run_native <- function(panel, estimand) {
  args <- list(data = panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0)
  if (identical(estimand, "dynamic")) {
    args$horizons <- 0:2
  }
  do.call(did_imputation, args)
}

f046_run_python <- function(panel, estimand) {
  args <- list(df = panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = c("unit", "t"), aw = "w", minn = 0)
  if (identical(estimand, "dynamic")) {
    args$horizons <- 0:2
  }
  do.call(did_imputation_python, args)
}

f046_run_kyle <- function(panel, estimand) {
  args <- list(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")
  if (identical(estimand, "dynamic")) {
    args$horizon <- 0:2
  }
  did_imputation_kyle(
    data = args$data,
    yname = args$yname,
    gname = args$gname,
    tname = args$tname,
    idname = args$idname,
    wname = args$wname,
    horizon = args$horizon,
    cluster_var = args$cluster_var
  )
}

f046_expected_rows <- function(rows, scenario) {
  rows[rows$scenario == scenario, , drop = FALSE]
}

f046_covariance_for <- function(rows, scenario) {
  covariance <- rows[rows$scenario == scenario, , drop = FALSE]
  terms <- unique(c(covariance$row_term, covariance$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(covariance))) {
    mat[covariance$row_term[[idx]], covariance$col_term[[idx]]] <- covariance$value[[idx]]
  }
  mat
}

f046_python_rows <- function(output, scenario, estimand) {
  terms <- names(output$estimates)
  data.frame(
    scenario = scenario,
    estimand = estimand,
    term = terms,
    estimate = as.numeric(unlist(output$estimates, use.names = FALSE)),
    std_error = as.numeric(unlist(output$std_errors, use.names = FALSE)),
    n_obs = output$n_obs,
    stringsAsFactors = FALSE
  )
}

test_that("F046 fixture metadata covers hundreds of seeded panels with zero retained failures", {
  scenarios <- read_fixture_csv("parity", "f046-differential", "inputs", "scenarios.csv")
  stata_diag <- jsonlite::fromJSON(f046_fixture("expected", "stata", "diagnostics.json"))
  python_diag <- jsonlite::fromJSON(f046_fixture("expected", "python", "diagnostics.json"))
  kyle_diag <- jsonlite::fromJSON(f046_fixture("expected", "kyle", "diagnostics.json"))
  manifest <- jsonlite::fromJSON(f046_fixture("metadata", "manifest.json"))
  source_inventory <- jsonlite::fromJSON(f046_fixture("metadata", "source-inventory.json"))
  minimal_failures <- read_fixture_csv("parity", "f046-differential", "metadata", "minimal-failing-cases.csv")
  stata_failures <- read_fixture_csv("parity", "f046-differential", "expected", "stata", "failures.csv")
  python_failures <- read_fixture_csv("parity", "f046-differential", "expected", "python", "failures.csv")
  kyle_failures <- read_fixture_csv("parity", "f046-differential", "expected", "kyle", "failures.csv")

  expect_equal(nrow(scenarios), 200)
  expect_equal(sum(scenarios$estimand == "static"), 100)
  expect_equal(sum(scenarios$estimand == "dynamic"), 100)
  expect_true(all(scenarios$weighted == 1))
  expect_equal(stata_diag$status, "success")
  expect_equal(python_diag$status, "success")
  expect_equal(kyle_diag$status, "success")
  expect_equal(stata_diag$scenario_count, 200)
  expect_equal(python_diag$scenario_count, 200)
  expect_equal(kyle_diag$scenario_count, 200)
  expect_equal(stata_diag$failure_count, 0)
  expect_equal(python_diag$failure_count, 0)
  expect_equal(kyle_diag$failure_count, 0)
  expect_equal(nrow(minimal_failures), 0)
  expect_equal(nrow(stata_failures), 0)
  expect_equal(nrow(python_failures), 0)
  expect_equal(nrow(kyle_failures), 0)
  expect_equal(manifest$scenario_count, 200)
  expect_true("D026" %in% manifest$decision_record_ids)
  expect_equal(source_inventory$reference_commits$stata, "767c8d6670a751170910d419bbafd323df92ef08")
  expect_equal(source_inventory$reference_commits$python, "c7765a9fb2dcc48dc745b356784b4e9ce8b1d376")
  expect_equal(source_inventory$reference_commits$kyle, "69b4f8dfe16b007474721fc5610859b56a80cdc6")
})

test_that("F046 R-native randomized differential panels match Stata batch artifacts", {
  scenarios <- read_fixture_csv("parity", "f046-differential", "inputs", "scenarios.csv")
  stata_estimates <- read_fixture_csv("parity", "f046-differential", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f046-differential", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f046-differential", "expected", "stata", "sample-mask.csv")

  for (idx in seq_len(nrow(scenarios))) {
    spec <- scenarios[idx, , drop = FALSE]
    panel <- f046_panel(spec$scenario)
    result <- f046_run_native(panel, spec$estimand)
    expected <- f046_expected_rows(stata_estimates, spec$scenario)
    expected_covariance <- f046_covariance_for(stata_covariance, spec$scenario)
    expected_sample <- f046_expected_rows(stata_sample, spec$scenario)

    expect_equal(result$estimates$term, expected$term, info = spec$scenario)
    expect_equal(result$estimates$estimate, expected$estimate, tolerance = 1e-7, info = spec$scenario)
    expect_equal(result$estimates$std.error, expected$std_error, tolerance = 1e-8, info = spec$scenario)
    expect_equal(result$estimates$n_obs, expected$n_obs, info = spec$scenario)
    expect_equal(result$estimates$n_control, expected$n_control, info = spec$scenario)
    expect_equal(result$estimates$n_treated, expected$n_treated, info = spec$scenario)
    expect_equal(result$covariance[rownames(expected_covariance), colnames(expected_covariance), drop = FALSE], expected_covariance, tolerance = 1e-8, info = spec$scenario)
    expect_equal(result$sample_mask$row_id, as.character(expected_sample$row_id), info = spec$scenario)
    expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = spec$scenario)
  }
})

test_that("F046 Python-compatible wrapper matches pinned Python reference shape under D017", {
  scenarios <- read_fixture_csv("parity", "f046-differential", "inputs", "scenarios.csv")
  expected <- read_fixture_csv("parity", "f046-differential", "expected", "python", "estimates.csv")

  for (idx in seq_len(nrow(scenarios))) {
    spec <- scenarios[idx, , drop = FALSE]
    panel <- f046_panel(spec$scenario)
    output <- f046_run_python(panel, spec$estimand)
    actual <- f046_python_rows(output, spec$scenario, spec$estimand)
    expected_rows <- f046_expected_rows(expected, spec$scenario)

    expect_s3_class(output, "DIDImputationOutput")
    expect_equal(actual$term, expected_rows$term, info = spec$scenario)
    expect_equal(actual$estimate, expected_rows$estimate, tolerance = 5e-7, info = spec$scenario)
    expect_equal(actual$std_error, expected_rows$std_error, tolerance = 1e-8, info = spec$scenario)
    expect_equal(actual$n_obs, expected_rows$n_obs, info = spec$scenario)
  }
})

test_that("F046 Kyle-compatible wrapper matches pinned Kyle public output", {
  scenarios <- read_fixture_csv("parity", "f046-differential", "inputs", "scenarios.csv")
  expected <- read_fixture_csv("parity", "f046-differential", "expected", "kyle", "estimates.csv")

  for (idx in seq_len(nrow(scenarios))) {
    spec <- scenarios[idx, , drop = FALSE]
    panel <- f046_panel(spec$scenario)
    result <- f046_run_kyle(panel, spec$estimand)
    expected_rows <- f046_expected_rows(expected, spec$scenario)

    expect_s3_class(result, "data.table")
    expect_equal(result$term, as.character(expected_rows$term), info = spec$scenario)
    expect_equal(result$estimate, expected_rows$estimate, tolerance = 1e-7, info = spec$scenario)
    expect_equal(result$std.error, expected_rows$std.error, tolerance = 1e-8, info = spec$scenario)
    expect_equal(result$conf.low, expected_rows$conf.low, tolerance = 1e-8, info = spec$scenario)
    expect_equal(result$conf.high, expected_rows$conf.high, tolerance = 1e-8, info = spec$scenario)
  }
})
