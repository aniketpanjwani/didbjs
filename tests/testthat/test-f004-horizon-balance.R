skip_if_not(fixtures_present())

test_that("F004 hbalance matches Stata estimates sample mask and unit diagnostics", {
  panel <- read_fixture_csv("parity", "f004-horizon-balance", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f004-horizon-balance", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f004-horizon-balance", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f004-horizon-balance", "expected", "stata", "sample-mask.csv")
  stata_units <- read_fixture_csv("parity", "f004-horizon-balance", "expected", "stata", "hbalance-units.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f004-horizon-balance", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    horizons = 0:2,
    hbalance = TRUE
  )

  expect_true(result$diagnostics$hbalance)
  expect_equal(result$diagnostics$horizons, c(0L, 1L, 2L))
  expect_equal(result$diagnostics$hbalance_included_units, stata_diag$hbalance_included_units)
  expect_equal(result$diagnostics$hbalance_excluded_units, stata_diag$hbalance_excluded_units)
  expect_equal(stata_units$unit[stata_units$hbalance_included == 1], result$diagnostics$hbalance_included_units)
  expect_equal(stata_units$unit[stata_units$treated_unit == 1 & stata_units$hbalance_included == 0], result$diagnostics$hbalance_excluded_units)
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(result$covariance[rownames(stata_covariance), colnames(stata_covariance)], stata_covariance, tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(result$sample_mask$row_id[result$sample_mask$sample == FALSE], c("4_4", "4_5"))
})

test_that("F004 Python-compatible hbalance preserves object shape with approved drift evidence", {
  panel <- read_fixture_csv("parity", "f004-horizon-balance", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f004-horizon-balance", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f004-horizon-balance", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f004-horizon-balance", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f004-horizon-balance", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    aw = "w",
    minn = 0,
    horizons = 0:2,
    hbalance = TRUE
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
  expect_equal(python_diag$hbalance_included_units, c(1, 2, 3))
  expect_equal(python_diag$hbalance_excluded_units, 4)
  expect_false(python_diag$tol001_pass)
})

test_that("F004 hbalance validates unsupported combinations and non-constant weights", {
  panel <- read_fixture_csv("parity", "f004-horizon-balance", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", hbalance = TRUE),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:2, hbalance = TRUE, autosample = TRUE),
    class = "didbjs_contract_error"
  )

  panel_bad_weights <- panel
  panel_bad_weights$w[panel_bad_weights$unit == 1 & panel_bad_weights$t == 5] <- 2
  expect_error(
    did_imputation(panel_bad_weights, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", horizons = 0:2, hbalance = TRUE),
    class = "didbjs_contract_error"
  )
})
