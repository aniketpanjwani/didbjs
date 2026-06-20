skip_if_not(fixtures_present())

test_that("F006 sum difference estimand matches Stata estimates covariance and sample mask", {
  panel <- read_fixture_csv("parity", "f006-difference-estimand", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f006-difference-estimand", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f006-difference-estimand", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f006-difference-estimand", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f006-difference-estimand", "expected", "stata", "diagnostics.json"))
  difference_weights <- read_fixture_csv("parity", "f006-difference-estimand", "expected", "stata", "difference-weights.csv")

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    wtr = "wtr_diff",
    sum = TRUE
  )

  expect_true(result$diagnostics$sum)
  expect_equal(result$diagnostics$wtr, "wtr_diff")
  expect_equal(result$estimates$term, "tau")
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE], stata_covariance, tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(result$sample_mask$row_id[result$sample_mask$sample == FALSE], paste0(1:5, "_5"))
  expect_equal(stata_diag$raw_weight_sum, 0)
  expect_equal(stata_diag$raw_abs_weight_sum, 2)
  expect_equal(stata_diag$raw_negative_weight_sum, -1)
  expect_equal(stata_diag$raw_positive_weight_sum, 1)
  expect_equal(stata_diag$algebraic_difference, result$estimates$estimate[[1]], tolerance = 1e-10)
  expect_equal(sum(difference_weights$raw_weight), 0, tolerance = 1e-12)
  expect_equal(sum(abs(difference_weights$raw_weight)), 2, tolerance = 1e-12)
})

test_that("F006 negative custom weight support is gated by sum", {
  panel <- read_fixture_csv("parity", "f006-difference-estimand", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", wtr = "wtr_diff"),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", sum = TRUE),
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_diff", sum = TRUE, horizons = 0:1),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_diff"),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "wtr_diff", sum = TRUE),
    class = "didbjs_unsupported_error"
  )
})
