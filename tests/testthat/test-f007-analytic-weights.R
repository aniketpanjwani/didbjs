skip_if_not(fixtures_present())

test_that("F007 analytic weights match Stata estimates covariance sample mask and normalization", {
  panel <- read_fixture_csv("parity", "f007-analytic-weights", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f007-analytic-weights", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f007-analytic-weights", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f007-analytic-weights", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f007-analytic-weights", "expected", "stata", "diagnostics.json"))
  normalized_weights <- read_fixture_csv("parity", "f007-analytic-weights", "expected", "stata", "normalized-weights.csv")

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0
  )

  expect_equal(result$estimates$term, "tau")
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE], stata_covariance, tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(stata_diag$treated_weight_sum, 13.8)
  expect_equal(stata_diag$normalized_weight_sum, 1)
  expect_gt(stata_diag$weighted_unweighted_abs_diff, 0.21)
  expect_equal(sum(normalized_weights$normalized_weight), 1, tolerance = 1e-12)
})

test_that("F007 Python-compatible wrapper preserves object shape with approved weighted drift evidence", {
  panel <- read_fixture_csv("parity", "f007-analytic-weights", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f007-analytic-weights", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f007-analytic-weights", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f007-analytic-weights", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f007-analytic-weights", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    aw = "w",
    minn = 0
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_named(out$estimates, "tau_ate")
  expect_equal(out$estimates$tau_ate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std_errors$tau_ate, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(out$V, stata_covariance["tau", "tau"], tolerance = 1e-8)
  expect_lt(abs(abs(python_diag$estimate - stata_estimates$estimate) - 3.86305328170522e-7), 1e-14)
  expect_lt(abs(abs(python_diag$std_error - stata_estimates$std_error) - 6.628577742251451e-9), 1e-14)
  expect_gt(python_diag$weighted_unweighted_abs_diff, 0.21)
})

test_that("F007 Kyle-compatible wrapper keeps Kyle shape while Stata governs weighted covariance", {
  panel <- read_fixture_csv("parity", "f007-analytic-weights", "inputs", "panel.csv")
  kyle_estimates <- read_fixture_csv("parity", "f007-analytic-weights", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f007-analytic-weights", "expected", "kyle", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f007-analytic-weights", "expected", "stata", "estimates.csv")

  out <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  )

  expect_s3_class(out, "data.table")
  expect_named(out, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(out$term, "treat")
  expect_equal(out$estimate, kyle_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(kyle_diag$status, "success")
  expect_gt(abs(kyle_estimates$std.error - stata_estimates$std_error), 0.002)
  expect_gt(kyle_diag$weighted_unweighted_abs_diff, 0.21)
})
