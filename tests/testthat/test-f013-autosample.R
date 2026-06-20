skip_if_not(fixtures_present())

test_that("F013 default fail-closed behavior matches Stata cannot-impute error", {
  panel <- read_fixture_csv("parity", "f013-autosample", "inputs", "panel.csv")
  default_error <- jsonlite::fromJSON(fixture_path("parity", "f013-autosample", "expected", "stata", "default-error.json"))
  cannot_impute <- read_fixture_csv("parity", "f013-autosample", "expected", "stata", "cannot-impute-default.csv")

  expect_equal(default_error$status, "error")
  expect_equal(default_error$return_code, 198)
  expect_equal(default_error$cannot_impute_count, 3)
  expect_equal(cannot_impute$row_id[cannot_impute$cannot_impute == 1], c("u2_t2", "u2_t3", "u2_t4"))

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:2, minn = 0, cluster = "unit"),
    regexp = "Could not impute treated observations: u2_t2, u2_t3, u2_t4",
    class = "didbjs_contract_error"
  )
})

test_that("F013 autosample trims and drops terms with Stata diagnostics", {
  panel <- read_fixture_csv("parity", "f013-autosample", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f013-autosample", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f013-autosample", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f013-autosample", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f013-autosample", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:2,
    minn = 0,
    cluster = "unit",
    autosample = TRUE
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$autosample_drop, "tau2")
  expect_equal(stata_diag$autosample_trim, "tau0 tau1")
  expect_equal(stata_diag$droplist, "tau2")
  expect_equal(stata_diag$cannot_impute_count, 3)
  expect_equal(result$diagnostics$autosample, TRUE)
  expect_equal(result$diagnostics$cannot_impute_row_ids, c("u2_t2", "u2_t3", "u2_t4"))
  expect_equal(result$diagnostics$autosample_dropped_row_ids, c("u2_t2", "u2_t3", "u2_t4"))
  expect_equal(result$diagnostics$autosample_drop, "tau2")
  expect_equal(result$diagnostics$autosample_trim, c("tau0", "tau1"))

  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
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
  expect_true(all(result$sample_mask$cannot_impute == as.logical(stata_sample$cannot_impute)))
})

test_that("F013 Python-compatible wrapper autosamples and preserves object shape", {
  panel <- read_fixture_csv("parity", "f013-autosample", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f013-autosample", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f013-autosample", "expected", "python", "diagnostics.json"))
  python_estimates <- read_fixture_csv("parity", "f013-autosample", "expected", "python", "estimates.csv")
  stata_estimates <- read_fixture_csv("parity", "f013-autosample", "expected", "stata", "estimates.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    horizons = 0:2,
    minn = 0,
    cluster = "unit"
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_match(python_diag$autosample_message, "Cannot impute for 3 observations. Autosample used", fixed = TRUE)
  expect_equal(python_diag$n_obs, 19)
  expect_equal(python_diag$terms, c("tau0", "tau1", "tau2"))
  expect_named(out$estimates, python_estimates$term)
  expect_equal(unname(unlist(out$estimates)), stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(unname(unlist(out$std_errors)), stata_estimates$std_error, tolerance = 1e-8)

  wrapper_diag <- attr(out, "diagnostics")
  expect_equal(wrapper_diag$autosample, TRUE)
  expect_equal(wrapper_diag$cannot_impute_row_ids, c("u2_t2", "u2_t3", "u2_t4"))
  expect_equal(wrapper_diag$autosample_drop, "tau2")
  expect_equal(wrapper_diag$autosample_trim, c("tau0", "tau1"))
})
