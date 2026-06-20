skip_if_not(fixtures_present())

test_that("F005 custom positive weights match Stata estimates covariance and normalization", {
  panel <- read_fixture_csv("parity", "f005-custom-positive-weights", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f005-custom-positive-weights", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f005-custom-positive-weights", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f005-custom-positive-weights", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f005-custom-positive-weights", "expected", "stata", "diagnostics.json"))
  normalized_weights <- read_fixture_csv("parity", "f005-custom-positive-weights", "expected", "stata", "normalized-weights.csv")

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    wtr = c("wtr_uniform", "wtr_late")
  )

  expect_equal(result$diagnostics$wtr, c("wtr_uniform", "wtr_late"))
  expect_false(result$diagnostics$sum)
  expect_equal(result$estimates$term, c("tau_wtr_uniform", "tau_wtr_late"))
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(result$covariance[rownames(stata_covariance), colnames(stata_covariance)], stata_covariance, tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(stata_diag$normalized_sum_wtr_uniform, 1)
  expect_equal(stata_diag$normalized_sum_wtr_late, 1)
  expect_equal(stata_diag$algebraic_wtr_uniform, result$estimates$estimate[[1]], tolerance = 1e-10)
  expect_equal(stata_diag$algebraic_wtr_late, result$estimates$estimate[[2]], tolerance = 1e-10)
  expect_equal(
    stats::aggregate(normalized_weight ~ term, normalized_weights, sum)$normalized_weight,
    c(1, 1),
    tolerance = 1e-12
  )
})

test_that("F005 Python-compatible custom wtr preserves raw names with approved drift evidence", {
  panel <- read_fixture_csv("parity", "f005-custom-positive-weights", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f005-custom-positive-weights", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f005-custom-positive-weights", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f005-custom-positive-weights", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f005-custom-positive-weights", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    aw = "w",
    minn = 0,
    wtr = c("wtr_uniform", "wtr_late")
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_named(out$estimates, c("wtr_uniform", "wtr_late"))
  expect_named(out$std_errors, c("wtr_uniform", "wtr_late"))
  expect_equal(unname(unlist(out$estimates)), stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(unname(unlist(out$std_errors)), stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(out$V, sum(diag(stata_covariance)), tolerance = 1e-8)
  expect_false(python_diag$tol001_pass)
  expect_equal(unname(unlist(python_diag$algebraic_abs_diff)), rep(8.286954762404264e-07, 2), tolerance = 1e-15)
  expect_equal(names(python_diag$estimates), c("wtr_uniform", "wtr_late"))
})

test_that("F005 custom positive weight validation is structured", {
  panel <- read_fixture_csv("parity", "f005-custom-positive-weights", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "missing_wtr"),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = c("wtr_uniform", "wtr_uniform")),
    class = "didbjs_contract_error"
  )

  panel_negative <- panel
  panel_negative$wtr_uniform[panel_negative$row_id == "1_4"] <- -1
  expect_error(
    did_imputation(panel_negative, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_uniform"),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_uniform", horizons = 0:1),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_uniform", horizons = 0:1),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_uniform", sum = TRUE),
    class = "didbjs_unsupported_error"
  )
})
