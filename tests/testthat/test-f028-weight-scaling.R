skip_if_not(fixtures_present())

test_that("F028 analytic-weight scaling is invariant against Stata oracle", {
  panel <- read_fixture_csv("parity", "f028-weight-scaling", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f028-weight-scaling", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f028-weight-scaling", "expected", "stata", "covariance.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "stata", "diagnostics.json"))

  base <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0)
  scaled <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w_scaled", cluster = "unit", minn = 0)
  expected_base <- stata_estimates[stata_estimates$scenario == "base", , drop = FALSE]
  expected_scaled <- stata_estimates[stata_estimates$scenario == "scaled", , drop = FALSE]
  expected_base_v <- stata_covariance$value[stata_covariance$scenario == "base"]
  expected_scaled_v <- stata_covariance$value[stata_covariance$scenario == "scaled"]

  expect_equal(stata_diag$status, "success")
  expect_lt(stata_diag$estimate_scale_abs_diff, 1e-12)
  expect_lt(stata_diag$variance_scale_abs_diff, 1e-12)
  expect_equal(base$estimates$estimate, expected_base$estimate, tolerance = 1e-10)
  expect_equal(base$estimates$std.error, expected_base$std_error, tolerance = 1e-8)
  expect_equal(scaled$estimates$estimate, expected_scaled$estimate, tolerance = 1e-10)
  expect_equal(scaled$estimates$std.error, expected_scaled$std_error, tolerance = 1e-8)
  expect_equal(base$estimates$estimate, scaled$estimates$estimate, tolerance = 1e-12)
  expect_equal(base$estimates$std.error, scaled$estimates$std.error, tolerance = 1e-12)
  expect_equal(base$covariance["tau", "tau"], expected_base_v, tolerance = 1e-8)
  expect_equal(scaled$covariance["tau", "tau"], expected_scaled_v, tolerance = 1e-8)
})

test_that("F028 missing analytic weights are excluded with original-row masks", {
  panel <- read_fixture_csv("parity", "f028-weight-scaling", "inputs", "panel.csv")
  stata_sample <- read_fixture_csv("parity", "f028-weight-scaling", "expected", "stata", "sample-mask-missing.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "stata", "diagnostics.json"))
  panel$w[panel$row_id == "2_3"] <- NA_real_

  result <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0)

  expect_equal(stata_diag$missing_weight_row_id, "2_3")
  expect_equal(stata_diag$missing_weight_row_excluded, 1)
  expect_equal(result$estimates$estimate, stata_diag$missing_weight_estimate, tolerance = 1e-10)
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_false(result$sample_mask$sample[result$sample_mask$row_id == "2_3"])
  expect_true(result$sample_mask$missing_required[result$sample_mask$row_id == "2_3"])
})

test_that("F028 Python and Kyle references record scaling invariance and wrapper shape", {
  panel <- read_fixture_csv("parity", "f028-weight-scaling", "inputs", "panel.csv")
  python_schema <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "python", "diagnostics.json"))
  kyle_estimates <- read_fixture_csv("parity", "f028-weight-scaling", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "kyle", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f028-weight-scaling", "expected", "stata", "estimates.csv")
  expected_base <- stata_estimates[stata_estimates$scenario == "base", , drop = FALSE]
  expected_scaled <- stata_estimates[stata_estimates$scenario == "scaled", , drop = FALSE]

  py_base <- did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = c("unit", "t"), aw = "w", minn = 0)
  py_scaled <- did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = c("unit", "t"), aw = "w_scaled", minn = 0)
  kyle_base <- did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")
  kyle_scaled <- did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w_scaled", cluster_var = "unit")

  expect_s3_class(py_base, "DIDImputationOutput")
  expect_named(py_base, names(python_schema$fields))
  expect_equal(py_base$estimates$tau_ate, expected_base$estimate, tolerance = 1e-10)
  expect_equal(py_scaled$estimates$tau_ate, expected_scaled$estimate, tolerance = 1e-10)
  expect_equal(py_base$estimates$tau_ate, py_scaled$estimates$tau_ate, tolerance = 1e-12)
  expect_lt(python_diag$estimate_scale_abs_diff, 1e-12)
  expect_lt(python_diag$variance_scale_abs_diff, 1e-12)

  expect_s3_class(kyle_base, "data.table")
  expect_equal(kyle_base$term, "treat")
  expect_equal(kyle_base$estimate, kyle_estimates$estimate[kyle_estimates$scenario == "base"], tolerance = 1e-10)
  expect_equal(kyle_scaled$estimate, kyle_estimates$estimate[kyle_estimates$scenario == "scaled"], tolerance = 1e-10)
  expect_equal(kyle_base$estimate, kyle_scaled$estimate, tolerance = 1e-12)
  expect_lt(kyle_diag$estimate_scale_abs_diff, 1e-12)
})

test_that("F028 invalid and non-analytic weight classes fail closed in didbjs", {
  panel <- read_fixture_csv("parity", "f028-weight-scaling", "inputs", "panel.csv")
  stata_invalid <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "stata", "invalid-probes.json"))
  python_invalid <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "python", "invalid-probes.json"))
  kyle_invalid <- jsonlite::fromJSON(fixture_path("parity", "f028-weight-scaling", "expected", "kyle", "invalid-probes.json"))

  expect_equal(stata_invalid$iweight$status, "reference_error")
  expect_equal(stata_invalid$fweight$status, "reference_error")
  expect_equal(stata_invalid$zero_weight$status, "reference_success")
  expect_equal(python_invalid$zero_weight$status, "reference_success")
  expect_equal(python_invalid$negative_weight$status, "reference_success")
  expect_equal(kyle_invalid$zero_weight$status, "reference_success")
  expect_equal(kyle_invalid$negative_weight$status, "reference_success")
  expect_equal(kyle_invalid$infinite_weight$status, "reference_success")

  invalid_cases <- list(
    zero = function(x) {
      x$w[x$row_id == "1_3"] <- 0
      x
    },
    negative = function(x) {
      x$w[x$row_id == "1_3"] <- -1
      x
    },
    infinite = function(x) {
      x$w[x$row_id == "1_3"] <- Inf
      x
    },
    all_zero = function(x) {
      x$w <- 0
      x
    }
  )

  for (case_name in names(invalid_cases)) {
    bad <- invalid_cases[[case_name]](panel)
    expect_error(
      did_imputation(bad, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0),
      regexp = "Analytic weights must be positive and finite.",
      class = "didbjs_contract_error",
      info = case_name
    )
    expect_error(
      did_imputation_python(bad, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0),
      regexp = "Analytic weights must be positive and finite.",
      class = "didbjs_contract_error",
      info = case_name
    )
    expect_error(
      did_imputation_kyle(bad, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit"),
      regexp = "Analytic weights must be positive and finite.",
      class = "didbjs_contract_error",
      info = case_name
    )
  }

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", iw = "w", minn = 0),
    regexp = "Unsupported arguments: iw",
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", fw = "w", minn = 0),
    regexp = "Unsupported arguments: fw",
    class = "didbjs_unsupported_error"
  )
})
