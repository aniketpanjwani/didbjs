skip_if_not(fixtures_present())

test_that("F011 triple difference matches Stata composite-unit interacted-FE semantics", {
  panel <- read_fixture_csv("parity", "f011-triple-difference", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f011-triple-difference", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f011-triple-difference", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f011-triple-difference", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f011-triple-difference", "expected", "stata", "diagnostics.json"))

  algebraic_att <- mean(panel$tau[panel$D == 1])
  expect_equal(stata_diag$total_rows, nrow(panel))
  expect_equal(stata_diag$unique_composite_units, length(unique(panel$ig)))
  expect_equal(stata_diag$unique_counties, length(unique(panel$county)))
  expect_equal(stata_diag$unique_groups, length(unique(panel$g)))
  expect_equal(stata_diag$algebraic_att, algebraic_att, tolerance = 1e-12)
  expect_equal(stata_diag$algebraic_gap, 0, tolerance = 1e-12)

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "ig",
    t = "t",
    Ei = "Eig",
    fe = c("ig", "county#t", "g#t"),
    cluster = "county",
    minn = 0
  )

  expected_tau <- stata_estimates[stata_estimates$term == "tau", , drop = FALSE]

  expect_equal(result$diagnostics$fe, c("ig", "county#t", "g#t"))
  expect_equal(result$diagnostics$cluster, "county")
  expect_equal(result$estimates$term, expected_tau$term)
  expect_equal(result$estimates$estimate, expected_tau$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$estimate, algebraic_att, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, expected_tau$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, expected_tau$n_obs)
  expect_equal(result$estimates$n_control, expected_tau$n_control)
  expect_equal(result$estimates$n_treated, expected_tau$n_treated)
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-8
  )
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_false(any(grepl("^\\.didbjs_fe_", names(panel))))
})

test_that("F011 interacted FE validation is structured", {
  panel <- read_fixture_csv("parity", "f011-triple-difference", "inputs", "panel.csv")

  expect_error(
    did_imputation(panel, y = "Y", i = "ig", t = "t", Ei = "Eig", fe = "county#missing", minn = 0),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "ig", t = "t", Ei = "Eig", fe = "county#", minn = 0),
    class = "didbjs_contract_error"
  )
})
