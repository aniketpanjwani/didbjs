skip_if_not(fixtures_present())

test_that("F020 hetby matches Stata split-weight oracle with recorded direct divergence", {
  panel <- read_fixture_csv("parity", "f020-heterogeneity-projection", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f020-heterogeneity-projection", "expected", "stata", "hetby-estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f020-heterogeneity-projection", "expected", "stata", "hetby-covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f020-heterogeneity-projection", "expected", "stata", "hetby-sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f020-heterogeneity-projection", "expected", "stata", "diagnostics.json"))
  direct_error <- jsonlite::fromJSON(fixture_path("parity", "f020-heterogeneity-projection", "expected", "stata", "direct-hetby-error.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    hetby = "group"
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$hetby_terms, c("tau_0", "tau_1"))
  expect_equal(stata_diag$direct_hetby_return_code, 149)
  expect_equal(direct_error$status, "expected_divergence")
  expect_equal(direct_error$return_code, 149)
  expect_match(direct_error$root_cause, "levelsof returns r\\(levels\\)", perl = TRUE)
  expect_equal(result$diagnostics$hetby, "group")
  expect_equal(result$diagnostics$project, character())

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
})

test_that("F020 project matches seeded Stata projection oracle", {
  panel <- read_fixture_csv("parity", "f020-heterogeneity-projection", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f020-heterogeneity-projection", "expected", "stata", "project-estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f020-heterogeneity-projection", "expected", "stata", "project-covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f020-heterogeneity-projection", "expected", "stata", "project-sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f020-heterogeneity-projection", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    project = "x"
  )

  expect_equal(stata_diag$project_terms, c("tau_cons", "tau_x"))
  expect_null(result$diagnostics$hetby)
  expect_equal(result$diagnostics$project, "x")

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
})

test_that("F020 invalid hetby/project combinations follow Stata diagnostics", {
  panel <- read_fixture_csv("parity", "f020-heterogeneity-projection", "inputs", "panel.csv")
  autosample_error <- jsonlite::fromJSON(fixture_path("parity", "f020-heterogeneity-projection", "expected", "stata", "autosample-proj-error.json"))
  hetby_project_error <- jsonlite::fromJSON(fixture_path("parity", "f020-heterogeneity-projection", "expected", "stata", "hetby-project-error.json"))
  bad_hetby_error <- jsonlite::fromJSON(fixture_path("parity", "f020-heterogeneity-projection", "expected", "stata", "bad-hetby-error.json"))

  expect_equal(autosample_error$return_code, 184)
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", minn = 0, cluster = "unit", autosample = TRUE, project = "x"),
    regexp = autosample_error$error_message,
    class = "didbjs_contract_error"
  )

  expect_equal(hetby_project_error$return_code, 184)
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", minn = 0, cluster = "unit", hetby = "group", project = "x"),
    regexp = hetby_project_error$error_message,
    class = "didbjs_contract_error"
  )

  expect_equal(bad_hetby_error$return_code, 411)
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", minn = 0, cluster = "unit", hetby = "bad_group"),
    regexp = bad_hetby_error$error_message,
    class = "didbjs_contract_error"
  )

  panel$g0 <- as.numeric(panel$D == 1 & panel$group == 0)
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", minn = 0, cluster = "unit", wtr = "g0", project = "x"),
    regexp = "The option project can be combined with horizons/allhorizons but not with wtr.",
    class = "didbjs_contract_error"
  )
})
