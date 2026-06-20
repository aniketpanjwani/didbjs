skip_if_not(fixtures_present())

f037_scenarios <- c(
  "base",
  "row_permuted",
  "unit_relabel",
  "time_shift",
  "outcome_scaled",
  "constant_shift",
  "weight_scaled"
)

f037_invariant_scenarios <- c(
  "row_permuted",
  "unit_relabel",
  "time_shift",
  "constant_shift",
  "weight_scaled"
)

f037_run <- function(scenario) {
  panel <- read_fixture_csv("parity", "f037-invariance", "inputs", paste0(scenario, ".csv"))
  did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0)
}

f037_python_run <- function(scenario) {
  panel <- read_fixture_csv("parity", "f037-invariance", "inputs", paste0(scenario, ".csv"))
  did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = c("unit", "t"), aw = "w", minn = 0)
}

f037_kyle_run <- function(scenario) {
  panel <- read_fixture_csv("parity", "f037-invariance", "inputs", paste0(scenario, ".csv"))
  did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wname = "w", cluster_var = "unit")
}

f037_expected_row <- function(estimates, scenario) {
  estimates[estimates$scenario == scenario, , drop = FALSE]
}

f037_expect_diag_invariance <- function(diag, tolerance) {
  expect_lt(diag$row_permutation_abs_diff, tolerance)
  expect_lt(diag$unit_relabel_abs_diff, tolerance)
  expect_lt(diag$time_shift_abs_diff, tolerance)
  expect_lt(diag$constant_shift_abs_diff, tolerance)
  expect_lt(diag$weight_scale_abs_diff, tolerance)
  expect_equal(diag$outcome_scaled_estimate_ratio, diag$outcome_scale, tolerance = tolerance)
  expect_equal(diag$outcome_scaled_se_ratio, diag$outcome_scale, tolerance = tolerance)
}

test_that("F037 R-native estimates covariance and sample masks are invariant against Stata oracle", {
  stata_estimates <- read_fixture_csv("parity", "f037-invariance", "expected", "stata", "estimates.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f037-invariance", "expected", "stata", "diagnostics.json"))
  results <- stats::setNames(lapply(f037_scenarios, f037_run), f037_scenarios)

  expect_equal(stata_diag$status, "success")
  f037_expect_diag_invariance(stata_diag, 1e-8)
  expect_equal(stata_diag$outcome_scaled_variance_ratio, stata_diag$outcome_scale^2, tolerance = 1e-8)

  for (scenario in f037_scenarios) {
    expected <- f037_expected_row(stata_estimates, scenario)
    expected_sample <- read_fixture_csv("parity", "f037-invariance", "expected", "stata", paste0("sample-", scenario, ".csv"))
    result <- results[[scenario]]

    expect_equal(result$estimates$term, expected$term, info = scenario)
    expect_lt(abs(result$estimates$estimate - expected$estimate), 1e-8)
    expect_lt(abs(result$estimates$std.error - expected$std_error), 1e-8)
    expect_lt(abs(result$covariance[expected$term, expected$term] - expected$variance), 1e-8)
    expect_equal(result$estimates$n_obs, expected$n_obs, info = scenario)
    expect_equal(result$estimates$n_control, expected$n_control, info = scenario)
    expect_equal(result$estimates$n_treated, expected$n_treated, info = scenario)
    expect_equal(result$sample_mask$row_id, as.character(expected_sample$row_id), info = scenario)
    expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = scenario)
    expect_true(all(result$sample_mask$sample), info = scenario)
  }

  base <- results$base
  for (scenario in f037_invariant_scenarios) {
    result <- results[[scenario]]
    expect_equal(result$estimates$estimate, base$estimates$estimate, tolerance = 1e-12, info = scenario)
    expect_equal(result$estimates$std.error, base$estimates$std.error, tolerance = 1e-12, info = scenario)
    expect_equal(result$covariance["tau", "tau"], base$covariance["tau", "tau"], tolerance = 1e-12, info = scenario)
  }

  scaled <- results$outcome_scaled
  expect_equal(scaled$estimates$estimate / base$estimates$estimate, 3.5, tolerance = 1e-12)
  expect_equal(scaled$estimates$std.error / base$estimates$std.error, 3.5, tolerance = 1e-12)
  expect_equal(scaled$covariance["tau", "tau"] / base$covariance["tau", "tau"], 3.5^2, tolerance = 1e-12)
})

test_that("F037 Python-compatible wrapper and pinned artifact preserve invariance contract", {
  python_estimates <- read_fixture_csv("parity", "f037-invariance", "expected", "python", "estimates.csv")
  python_covariance <- read_fixture_csv("parity", "f037-invariance", "expected", "python", "covariance.csv")
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f037-invariance", "expected", "python", "diagnostics.json"))
  results <- stats::setNames(lapply(f037_scenarios, f037_python_run), f037_scenarios)

  expect_equal(python_diag$status, "success")
  f037_expect_diag_invariance(python_diag, 1e-6)
  expect_equal(python_diag$outcome_scaled_variance_ratio, python_diag$outcome_scale^2, tolerance = 1e-6)
  expect_equal(python_estimates$scenario, f037_scenarios)
  expect_equal(python_covariance$scenario, f037_scenarios)
  expect_true(all(python_estimates$n_obs == 60))

  base <- results$base
  expect_s3_class(base, "DIDImputationOutput")
  expect_equal(names(base$estimates), "tau_ate")
  for (scenario in f037_invariant_scenarios) {
    result <- results[[scenario]]
    expect_equal(result$estimates$tau_ate, base$estimates$tau_ate, tolerance = 1e-12, info = scenario)
    expect_equal(result$std_errors$tau_ate, base$std_errors$tau_ate, tolerance = 1e-12, info = scenario)
    expect_equal(result$V, base$V, tolerance = 1e-12, info = scenario)
  }
  expect_equal(results$outcome_scaled$estimates$tau_ate / base$estimates$tau_ate, 3.5, tolerance = 1e-12)
  expect_equal(results$outcome_scaled$std_errors$tau_ate / base$std_errors$tau_ate, 3.5, tolerance = 1e-12)
  expect_equal(results$outcome_scaled$V / base$V, 3.5^2, tolerance = 1e-12)
})

test_that("F037 Kyle-compatible wrapper matches Kyle invariance artifact", {
  kyle_estimates <- read_fixture_csv("parity", "f037-invariance", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f037-invariance", "expected", "kyle", "diagnostics.json"))
  results <- stats::setNames(lapply(f037_scenarios, f037_kyle_run), f037_scenarios)

  expect_equal(kyle_diag$status, "success")
  f037_expect_diag_invariance(kyle_diag, 1e-12)
  expect_equal(kyle_estimates$scenario, f037_scenarios)

  for (scenario in f037_scenarios) {
    result <- results[[scenario]]
    expected <- f037_expected_row(kyle_estimates, scenario)

    expect_s3_class(result, "data.table")
    expect_equal(result$term, expected$term, info = scenario)
    expect_equal(result$estimate, expected$estimate, tolerance = 1e-10, info = scenario)
    expect_equal(result$std.error, expected$std.error, tolerance = 1e-8, info = scenario)
    expect_equal(result$conf.low, expected$conf.low, tolerance = 1e-8, info = scenario)
    expect_equal(result$conf.high, expected$conf.high, tolerance = 1e-8, info = scenario)
  }

  base <- results$base
  for (scenario in f037_invariant_scenarios) {
    result <- results[[scenario]]
    expect_equal(result$estimate, base$estimate, tolerance = 1e-12, info = scenario)
    expect_equal(result$std.error, base$std.error, tolerance = 1e-12, info = scenario)
  }
  expect_equal(results$outcome_scaled$estimate / base$estimate, 3.5, tolerance = 1e-12)
  expect_equal(results$outcome_scaled$std.error / base$std.error, 3.5, tolerance = 1e-12)
})
