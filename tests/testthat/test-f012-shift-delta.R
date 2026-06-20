skip_if_not(fixtures_present())

test_that("F012 shift and delta match Stata horizon estimates covariance and sample mask", {
  panel <- read_fixture_csv("parity", "f012-shift-delta", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f012-shift-delta", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f012-shift-delta", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f012-shift-delta", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f012-shift-delta", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:2,
    shift = 2,
    delta = 2,
    cluster = "unit",
    minn = 0
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$shift, 2)
  expect_equal(stata_diag$delta, 2)
  expect_equal(stata_diag$terms, c("tau0", "tau1", "tau2"))
  expect_equal(result$diagnostics$shift, 2)
  expect_equal(result$diagnostics$delta, 2)
  expect_equal(result$estimates$term, c("tau0", "tau1", "tau2"))
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$estimate, stata_estimates$algebraic_target, tolerance = 1e-10)
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
})

test_that("F012 invalid shift and delta cases are structured", {
  panel <- read_fixture_csv("parity", "f012-shift-delta", "inputs", "panel.csv")
  invalid_delta <- jsonlite::fromJSON(fixture_path("parity", "f012-shift-delta", "expected", "stata", "invalid-delta.json"))

  expect_equal(invalid_delta$status, "error")
  expect_equal(invalid_delta$return_code, 198)
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:2, shift = 2, delta = 3, minn = 0),
    regexp = "non-integer values",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", shift = 0.5, minn = 0),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", shift = "2", minn = 0),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", delta = -2, minn = 0),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", delta = "2", minn = 0),
    class = "didbjs_contract_error"
  )
})
