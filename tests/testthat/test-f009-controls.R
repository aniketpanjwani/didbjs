skip_if_not(fixtures_present())

test_that("F009 continuous controls match Stata estimates covariance and sample mask", {
  panel <- read_fixture_csv("parity", "f009-controls", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f009-controls", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f009-controls", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f009-controls", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f009-controls", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    controls = c("x1", "x2"),
    aw = "w",
    cluster = "unit",
    minn = 0
  )

  expected_tau <- stata_estimates[stata_estimates$term == "tau", , drop = FALSE]
  expected_controls <- stata_estimates[stata_estimates$term %in% c("x1", "x2"), , drop = FALSE]

  expect_equal(result$diagnostics$controls, c("x1", "x2"))
  expect_equal(result$estimates$term, expected_tau$term)
  expect_equal(result$estimates$estimate, expected_tau$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, expected_tau$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, expected_tau$n_obs)
  expect_equal(result$estimates$n_control, expected_tau$n_control)
  expect_equal(result$estimates$n_treated, expected_tau$n_treated)
  expect_equal(result$controls$term, expected_controls$term)
  expect_equal(result$controls$estimate, expected_controls$estimate, tolerance = 1e-10)
  expect_equal(result$controls$std.error, expected_controls$std_error, tolerance = 1e-8)
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-8
  )
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$terms, c("tau", "x1", "x2"))
})

test_that("F009 Python-compatible controls preserve object shape", {
  panel <- read_fixture_csv("parity", "f009-controls", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f009-controls", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f009-controls", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f009-controls", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f009-controls", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    controls = c("x1", "x2"),
    aw = "w",
    minn = 0
  )

  schema_fields <- names(schema$fields)
  expect_true(inherits(out, "DIDImputationOutput"))
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null, info = field)
  }

  expected_tau <- stata_estimates[stata_estimates$term == "tau", , drop = FALSE]
  expected_controls <- stata_estimates[stata_estimates$term %in% c("x1", "x2"), , drop = FALSE]
  expect_named(out$estimates, "tau_ate")
  expect_equal(out$estimates$tau_ate, expected_tau$estimate, tolerance = 1e-10)
  expect_equal(out$std_errors$tau_ate, expected_tau$std_error, tolerance = 1e-8)
  expect_equal(names(out$controls_estimates), expected_controls$term)
  expect_equal(unname(unlist(out$controls_estimates)), expected_controls$estimate, tolerance = 1e-10)
  expect_equal(names(out$controls_std_errors), expected_controls$term)
  expect_equal(unname(unlist(out$controls_std_errors)), expected_controls$std_error, tolerance = 1e-8)
  expect_equal(out$V, sum(diag(stata_covariance)), tolerance = 1e-8)
  expect_gt(abs(python_diag$estimates$tau_ate - expected_tau$estimate), 1e-8)
  expect_named(python_diag$controls_estimates, expected_controls$term)
})

test_that("F009 control validation and collinearity handling are structured", {
  panel <- read_fixture_csv("parity", "f009-controls", "inputs", "panel.csv")
  stata_collinearity <- jsonlite::fromJSON(fixture_path("parity", "f009-controls", "expected", "stata", "collinearity.json"))
  python_collinearity <- jsonlite::fromJSON(fixture_path("parity", "f009-controls", "expected", "python", "collinearity.json"))

  expect_equal(stata_collinearity$status, "error")
  expect_equal(stata_collinearity$return_code, 481)
  expect_equal(python_collinearity$status, "error")

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", controls = "missing_control"),
    class = "didbjs_contract_error"
  )
  panel$bad_text <- as.character(panel$x1)
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", controls = "bad_text"),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", controls = c("x1", "x1")),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", controls = "x_bad", aw = "w", cluster = "unit", minn = 0),
    class = "didbjs_contract_error"
  )
})
