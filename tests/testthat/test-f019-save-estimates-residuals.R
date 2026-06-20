skip_if_not(fixtures_present())

test_that("F019 saved estimates and residuals match Stata artifacts", {
  panel <- read_fixture_csv("parity", "f019-save-estimates-residuals", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f019-save-estimates-residuals", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f019-save-estimates-residuals", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f019-save-estimates-residuals", "expected", "stata", "sample-mask.csv")
  saved_estimates <- utils::read.csv(
    fixture_path("parity", "f019-save-estimates-residuals", "expected", "stata", "saved-estimates.csv"),
    na.strings = c("", "NA", "."),
    check.names = FALSE,
    strip.white = TRUE
  )
  saved_residuals <- utils::read.csv(
    fixture_path("parity", "f019-save-estimates-residuals", "expected", "stata", "saved-residuals.csv"),
    check.names = FALSE,
    strip.white = TRUE
  )
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f019-save-estimates-residuals", "expected", "stata", "diagnostics.json"))

  original_names <- names(panel)
  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    saveestimates = TRUE,
    saveresid = TRUE
  )

  expect_equal(names(panel), original_names)
  expect_false(any(names(panel) %in% c("tau_hat", "eps_tau")))
  expect_s3_class(result$artifacts$estimates, "didbjs_estimates")
  expect_s3_class(result$artifacts$residuals, "didbjs_residuals")
  expect_equal(result$artifacts$estimates$schema_version, "didbjs.estimates.v1")
  expect_equal(result$artifacts$residuals$schema_version, "didbjs.residuals.v1")
  expect_true(result$diagnostics$saveestimates)
  expect_true(result$diagnostics$saveresid)
  expect_equal(stata_diag$status, "success")

  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-8
  )
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))

  r_saved_estimates <- result$artifacts$estimates$estimates
  expect_equal(r_saved_estimates$row_id, as.character(saved_estimates$row_id))
  expect_equal(r_saved_estimates$estimate, saved_estimates$estimate, tolerance = 1e-10)
  expect_true(all(is.na(r_saved_estimates$estimate[panel$D == 0])))
  expect_equal(r_saved_estimates$estimate[panel$D == 1], panel$tau[panel$D == 1], tolerance = 1e-10)

  r_saved_residuals <- result$artifacts$residuals$residuals
  expect_equal(r_saved_residuals$row_id, as.character(saved_residuals$row_id))
  expect_equal(r_saved_residuals$term, saved_residuals$term)
  expect_equal(r_saved_residuals$residual, saved_residuals$residual, tolerance = 1e-8)
  treated_residuals <- r_saved_residuals$residual[panel$D == 1]
  expect_equal(mean(treated_residuals), 0, tolerance = 1e-12)
})
