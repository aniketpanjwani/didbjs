skip_if_not(fixtures_present())

test_that("F010 repeated cross sections match Stata group-FE cluster semantics", {
  panel <- read_fixture_csv("parity", "f010-repeated-cross-section", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f010-repeated-cross-section", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f010-repeated-cross-section", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f010-repeated-cross-section", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f010-repeated-cross-section", "expected", "stata", "diagnostics.json"))

  algebraic_att <- mean(panel$tau[panel$D == 1])
  expect_equal(nrow(panel), length(unique(panel$person)))
  expect_equal(stata_diag$total_rows, nrow(panel))
  expect_equal(stata_diag$unique_persons, nrow(panel))
  expect_equal(stata_diag$unique_regions, length(unique(panel$region)))
  expect_equal(stata_diag$algebraic_att, algebraic_att, tolerance = 1e-12)
  expect_lt(abs(stata_diag$algebraic_gap), 1e-8)

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "person",
    t = "t",
    Ei = "Ei",
    fe = c("region", "t"),
    cluster = "region",
    minn = 0
  )

  expected_tau <- stata_estimates[stata_estimates$term == "tau", , drop = FALSE]

  expect_equal(result$diagnostics$fe, c("region", "t"))
  expect_equal(result$diagnostics$cluster, "region")
  expect_equal(result$estimates$term, expected_tau$term)
  expect_equal(result$estimates$estimate, expected_tau$estimate, tolerance = 1e-8)
  expect_equal(result$estimates$estimate, algebraic_att, tolerance = 1e-8)
  expect_equal(result$estimates$std.error, expected_tau$std_error, tolerance = 1e-10)
  expect_equal(result$estimates$n_obs, expected_tau$n_obs)
  expect_equal(result$estimates$n_control, expected_tau$n_control)
  expect_equal(result$estimates$n_treated, expected_tau$n_treated)
  expect_gt(result$estimates$std.error, 0)
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-10
  )
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
})

test_that("F010 repeated cross sections require explicit stable group FE", {
  panel <- read_fixture_csv("parity", "f010-repeated-cross-section", "inputs", "panel.csv")

  expect_error(
    did_imputation(panel, y = "Y", i = "person", t = "t", Ei = "Ei", minn = 0),
    regexp = "Could not impute treated observations",
    class = "didbjs_contract_error"
  )
})
