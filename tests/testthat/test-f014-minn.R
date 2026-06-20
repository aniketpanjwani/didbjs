skip_if_not(fixtures_present())

test_that("F014 minn suppression keeps explicit native rows with diagnostics", {
  panel <- read_fixture_csv("parity", "f014-minn", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f014-minn", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f014-minn", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f014-minn", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f014-minn", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    cluster = "unit"
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$minn, 30)
  expect_equal(stata_diag$droplist, "tau1")
  expect_equal(result$diagnostics$minn, 30)
  expect_equal(result$diagnostics$droplist, "tau1")
  expect_equal(result$diagnostics$suppressed_terms, "tau1")
  expect_equal(unname(result$diagnostics$effective_n), c(31, 2), tolerance = 1e-10)
  expect_equal(result$diagnostics$suppression_adjusted_n_obs, 71)

  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate[1], stata_estimates$estimate[1], tolerance = 1e-10)
  expect_equal(result$estimates$std.error[1], stata_estimates$std_error[1], tolerance = 1e-8)
  expect_true(is.na(result$estimates$estimate[2]))
  expect_true(is.na(result$estimates$std.error[2]))
  expect_equal(stata_estimates$estimate[2], 0)
  expect_equal(stata_estimates$std_error[2], 0)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-8
  )
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
})

test_that("F014 minn = 0 reports the low-support term", {
  panel <- read_fixture_csv("parity", "f014-minn", "inputs", "panel.csv")

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    minn = 0,
    cluster = "unit"
  )

  expect_equal(result$diagnostics$droplist, character())
  expect_equal(result$estimates$estimate, c(1, 2), tolerance = 1e-10)
  expect_equal(result$estimates$n_treated, c(31, 2))

  integer_result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    minn = 0L,
    cluster = "unit"
  )

  expect_equal(integer_result$estimates$estimate, result$estimates$estimate, tolerance = 1e-10)
  expect_equal(integer_result$diagnostics$minn, 0L)
})

test_that("F014 integer minn = 30 follows default suppression", {
  panel <- read_fixture_csv("parity", "f014-minn", "inputs", "panel.csv")

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    minn = 30L,
    cluster = "unit"
  )

  expect_equal(result$diagnostics$minn, 30L)
  expect_equal(result$diagnostics$droplist, "tau1")
  expect_true(is.na(result$estimates$estimate[[2]]))
})

test_that("F014 Python-compatible wrapper follows Python suppression surface", {
  panel <- read_fixture_csv("parity", "f014-minn", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f014-minn", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f014-minn", "expected", "python", "diagnostics.json"))
  python_estimates <- read_fixture_csv("parity", "f014-minn", "expected", "python", "estimates.csv")
  stata_estimates <- read_fixture_csv("parity", "f014-minn", "expected", "stata", "estimates.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    horizons = 0:1,
    minn = 30,
    cluster = "unit"
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_match(python_diag$warning_text, "WARNING: suppressing wtr1", fixed = TRUE)
  expect_equal(out$n_obs, python_diag$n_obs)
  expect_named(out$estimates, python_estimates$term)
  expect_equal(out$estimates$tau0, stata_estimates$estimate[1], tolerance = 1e-10)
  expect_equal(out$estimates$tau1, python_estimates$estimate[2], tolerance = 1e-10)
  expect_equal(out$std_errors$tau1, python_estimates$std_error[2], tolerance = 1e-10)

  wrapper_diag <- attr(out, "diagnostics")
  expect_equal(wrapper_diag$droplist, "tau1")
  expect_equal(wrapper_diag$suppressed_terms, "tau1")
  expect_equal(wrapper_diag$suppression_adjusted_n_obs, python_diag$n_obs)
})
