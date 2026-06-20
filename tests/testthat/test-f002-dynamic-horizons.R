skip_if_not(fixtures_present())

test_that("F002 dynamic horizons match Stata estimates covariance and sample mask", {
  panel <- read_fixture_csv("parity", "f002-dynamic-horizons", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f002-dynamic-horizons", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f002-dynamic-horizons", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f002-dynamic-horizons", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f002-dynamic-horizons", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    horizons = 0:2
  )

  expect_equal(result$estimates$term, c("tau0", "tau1", "tau2"))
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(result$covariance[rownames(stata_covariance), colnames(stata_covariance)], stata_covariance, tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$n_treated_tau0, 5)
  expect_equal(stata_diag$n_treated_tau1, 5)
  expect_equal(stata_diag$n_treated_tau2, 5)
})

test_that("F002 Python-compatible wrapper preserves dynamic object shape with approved drift evidence", {
  panel <- read_fixture_csv("parity", "f002-dynamic-horizons", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f002-dynamic-horizons", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f002-dynamic-horizons", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f002-dynamic-horizons", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f002-dynamic-horizons", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    aw = "w",
    minn = 0,
    horizons = 0:2
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_named(out$estimates, stata_estimates$term)
  expect_named(out$std_errors, stata_estimates$term)
  expect_equal(unname(unlist(out$estimates)), stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(unname(unlist(out$std_errors)), stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(out$V, sum(diag(stata_covariance)), tolerance = 1e-8)
  expect_false(python_diag$tol001_pass)
  expect_true(all(!unlist(python_diag$tol001_pass_by_term)))
  expect_equal(unname(unlist(python_diag$algebraic_abs_diff)), rep(3.632157987709661e-7, 3), tolerance = 1e-15)
})

test_that("F002 Kyle-compatible wrapper matches Kyle dynamic public output", {
  panel <- read_fixture_csv("parity", "f002-dynamic-horizons", "inputs", "panel.csv")
  kyle_estimates <- read_fixture_csv("parity", "f002-dynamic-horizons", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f002-dynamic-horizons", "expected", "kyle", "diagnostics.json"))

  out <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    horizon = 0:2,
    cluster_var = "unit"
  )

  expect_s3_class(out, "data.table")
  expect_named(out, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(out$term, as.character(kyle_estimates$term))
  expect_equal(out$estimate, kyle_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, kyle_estimates$std.error, tolerance = 1e-8)
  expect_equal(out$conf.low, kyle_estimates$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, kyle_estimates$conf.high, tolerance = 1e-8)
  expect_equal(kyle_diag$status, "success")
})

test_that("F002 horizon validation is structured", {
  panel <- read_fixture_csv("parity", "f002-dynamic-horizons", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = c(0, 0)),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = -1),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", horizon = TRUE),
    class = "didbjs_unsupported_error"
  )
})
