skip_if_not(fixtures_present())

f008_covariance_for_spec <- function(covariance, spec) {
  rows <- covariance[covariance$spec == spec, , drop = FALSE]
  terms <- unique(c(rows$row_term, rows$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(rows))) {
    mat[rows$row_term[[idx]], rows$col_term[[idx]]] <- rows$value[[idx]]
  }
  mat
}

test_that("F008 alternative FE sets match Stata estimates covariance and sample masks", {
  panel <- read_fixture_csv("parity", "f008-fe-sets", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f008-fe-sets", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f008-fe-sets", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f008-fe-sets", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f008-fe-sets", "expected", "stata", "diagnostics.json"))
  specs <- list(
    constant_only = character(),
    unit_only = "unit",
    time_only = "t",
    arbitrary = c("group", "t")
  )

  for (spec in names(specs)) {
    fe_arg <- if (spec == "constant_only") NULL else specs[[spec]]
    result <- did_imputation(
      data = panel,
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      fe = fe_arg,
      cluster = "unit",
      minn = 0
    )
    expected_estimate <- stata_estimates[stata_estimates$spec == spec, , drop = FALSE]
    expected_covariance <- f008_covariance_for_spec(stata_covariance, spec)
    expected_sample <- stata_sample[stata_sample$spec == spec, , drop = FALSE]

    expect_equal(result$diagnostics$fe, specs[[spec]], info = spec)
    expect_equal(result$estimates$term, expected_estimate$term, info = spec)
    expect_equal(result$estimates$estimate, expected_estimate$estimate, tolerance = 1e-10, info = spec)
    expect_equal(result$estimates$std.error, expected_estimate$std_error, tolerance = 1e-8, info = spec)
    expect_equal(result$estimates$n_obs, expected_estimate$n_obs, info = spec)
    expect_equal(result$estimates$n_control, expected_estimate$n_control, info = spec)
    expect_equal(result$estimates$n_treated, expected_estimate$n_treated, info = spec)
    expect_equal(result$covariance[rownames(expected_covariance), colnames(expected_covariance), drop = FALSE], expected_covariance, tolerance = 1e-8, info = spec)
    expect_equal(result$sample_mask$row_id, expected_sample$row_id, info = spec)
    expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = spec)
  }

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$specs, names(specs))
  expect_equal(length(unique(unlist(stata_diag$estimates))), 3)
  expect_gt(abs(stata_diag$std_errors$arbitrary - stata_diag$std_errors$time_only), 0.2)
})

test_that("F008 Python-compatible FE arguments preserve object shape with approved arbitrary-FE drift", {
  panel <- read_fixture_csv("parity", "f008-fe-sets", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f008-fe-sets", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f008-fe-sets", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f008-fe-sets", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f008-fe-sets", "expected", "stata", "covariance.csv")
  specs <- list(
    constant_only = NULL,
    unit_only = "unit",
    time_only = "t",
    arbitrary = c("group", "t")
  )

  for (spec in names(specs)) {
    out <- did_imputation_python(
      df = panel,
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      fe = specs[[spec]],
      minn = 0
    )
    schema_fields <- names(schema$fields)
    expect_true(inherits(out, "DIDImputationOutput"), info = spec)
    expect_named(out, schema_fields, info = spec)
    for (field in schema_fields) {
      expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null, info = paste(spec, field))
    }
    expected_estimate <- stata_estimates[stata_estimates$spec == spec, , drop = FALSE]
    expected_covariance <- f008_covariance_for_spec(stata_covariance, spec)
    expect_named(out$estimates, "tau_ate", info = spec)
    expect_equal(out$estimates$tau_ate, expected_estimate$estimate, tolerance = 1e-10, info = spec)
    expect_equal(out$std_errors$tau_ate, expected_estimate$std_error, tolerance = 1e-8, info = spec)
    expect_equal(out$V, expected_covariance["tau", "tau"], tolerance = 1e-8, info = spec)
  }

  drift <- abs(unlist(python_diag$estimates) - stats::setNames(stata_estimates$estimate, stata_estimates$spec))
  expect_lt(drift[["constant_only"]], 1e-12)
  expect_lt(drift[["unit_only"]], 1e-12)
  expect_lt(drift[["time_only"]], 1e-12)
  expect_lt(drift[["arbitrary"]], 1e-12)
})

test_that("F008 FE-set validation is structured", {
  panel <- read_fixture_csv("parity", "f008-fe-sets", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = character()),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = "missing_fe"),
    class = "didbjs_contract_error"
  )
  default_out <- did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = character(), minn = 0)
  native_default <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", minn = 0)
  expect_equal(default_out$estimates$tau_ate, native_default$estimates$estimate, tolerance = 1e-10)
})
