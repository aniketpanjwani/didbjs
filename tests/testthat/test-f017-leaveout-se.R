skip_if_not(fixtures_present())

test_that("F017 leaveout SE matches Stata with coarser avgeffectsby", {
  panel <- read_fixture_csv("parity", "f017-leaveout-se", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f017-leaveout-se", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f017-leaveout-se", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f017-leaveout-se", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f017-leaveout-se", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    avgeffectsby = "D",
    leaveout = TRUE
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$leaveout, TRUE)
  expect_equal(result$diagnostics$leaveout, TRUE)
  expect_equal(result$diagnostics$avgeffectsby, "D")
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-8)
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

test_that("F017 default leaveout singleton failure matches Stata guidance", {
  panel <- read_fixture_csv("parity", "f017-leaveout-se", "inputs", "panel.csv")
  default_error <- jsonlite::fromJSON(fixture_path("parity", "f017-leaveout-se", "expected", "stata", "default-error.json"))

  expect_equal(default_error$status, "error")
  expect_equal(default_error$return_code, 498)
  expect_equal(
    default_error$error_message,
    "Cannot compute leave-out standard errors because of 10 observations for coefficient \"tau\""
  )

  expect_error(
    did_imputation(
      data = panel,
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      minn = 0,
      cluster = "unit",
      leaveout = TRUE
    ),
    regexp = default_error$error_message,
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(
      data = panel,
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      minn = 0,
      cluster = "unit",
      leaveout = TRUE
    ),
    regexp = default_error$guidance_2,
    class = "didbjs_contract_error"
  )
})
