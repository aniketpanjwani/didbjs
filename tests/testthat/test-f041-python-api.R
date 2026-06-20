test_that("Python-compatible F001 wrapper preserves object schema", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "i",
    t = "t",
    Ei = "Ei",
    fe = c("i", "t"),
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
  expect_named(out$std_errors, "tau_ate")
  expect_equal(out$estimates$tau_ate, stata_estimates$estimate[1], tolerance = 1e-10)
  expect_equal(out$std_errors$tau_ate, stata_estimates$std_error[1], tolerance = 1e-8)
  expect_equal(out$V, stata_covariance$value[1], tolerance = 1e-8)
  expect_equal(out$n_obs, 60)
  expect_false(python_diag$tol001_pass)
})

test_that("Python-compatible wrapper keeps placeholder arguments explicit", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  expect_error(
    did_imputation_python(panel, y = "Y", i = "i", t = "t", Ei = "Ei", timecontrols = "x"),
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "i", t = "t", Ei = "Ei", leaveoneout = TRUE),
    class = "didbjs_unsupported_error"
  )
})
