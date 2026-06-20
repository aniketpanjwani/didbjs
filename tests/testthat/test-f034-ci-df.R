skip_if_not(fixtures_present())

f034_run <- function(panel, significance_level = 0.05) {
  did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    cluster = "cluster_two",
    minn = 0,
    significance_level = significance_level
  )
}

f034_expected_covariance <- function(covariance, scenario) {
  selected <- covariance[covariance$scenario == scenario, , drop = FALSE]
  terms <- unique(c(selected$row_term, selected$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(selected))) {
    mat[selected$row_term[[idx]], selected$col_term[[idx]]] <- selected$value[[idx]]
  }
  mat
}

test_that("F034 R-native CI coordinates use Stata normal critical-value semantics", {
  panel <- read_fixture_csv("parity", "f034-ci-df", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f034-ci-df", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f034-ci-df", "expected", "stata", "covariance.csv")
  ci_grid <- read_fixture_csv("parity", "f034-ci-df", "expected", "stata", "ci-grid.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f034-ci-df", "expected", "stata", "diagnostics.json"))
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f034-ci-df", "expected", "stata", "probes.json"))

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$ci_distribution, "normal")
  expect_equal(stata_probes$default$status, "reference_success")
  expect_equal(stata_probes$alpha10$status, "reference_success")

  scenarios <- data.frame(
    scenario = c("default", "alpha10"),
    significance_level = c(0.05, 0.10),
    stringsAsFactors = FALSE
  )
  for (idx in seq_len(nrow(scenarios))) {
    scenario <- scenarios$scenario[[idx]]
    significance_level <- scenarios$significance_level[[idx]]
    expected <- stata_estimates[stata_estimates$scenario == scenario, , drop = FALSE]
    expected_covariance <- f034_expected_covariance(stata_covariance, scenario)
    normal_row <- ci_grid[
      ci_grid$scenario == scenario &
        ci_grid$term == expected$term[[1]] &
        ci_grid$critical_type == "normal",
      ,
      drop = FALSE
    ]
    t_row <- ci_grid[
      ci_grid$scenario == scenario &
        ci_grid$term == expected$term[[1]] &
        ci_grid$critical_type == "t_n_clusters_minus_1",
      ,
      drop = FALSE
    ]

    result <- f034_run(panel, significance_level = significance_level)

    expect_equal(result$diagnostics$significance_level, expected$alpha, tolerance = 1e-15)
    expect_equal(result$diagnostics$ci_distribution, "normal")
    expect_equal(result$diagnostics$ci_degrees_of_freedom, Inf)
    expect_equal(result$diagnostics$ci_critical_value, expected$critical_value, tolerance = 1e-15)
    expect_equal(result$estimates$term, expected$term)
    expect_lt(abs(result$estimates$estimate - expected$estimate), 1e-8)
    expect_lt(abs(result$estimates$std.error - expected$std_error), 1e-8)
    expect_equal(
      result$estimates$conf.low,
      result$estimates$estimate - expected$critical_value * result$estimates$std.error,
      tolerance = 1e-15,
      info = scenario
    )
    expect_equal(
      result$estimates$conf.high,
      result$estimates$estimate + expected$critical_value * result$estimates$std.error,
      tolerance = 1e-15,
      info = scenario
    )
    expect_lt(abs(result$estimates$conf.low - expected$conf_low), 2e-8)
    expect_lt(abs(result$estimates$conf.high - expected$conf_high), 2e-8)
    expect_lt(abs(result$estimates$conf.low - normal_row$conf_low), 2e-8)
    expect_lt(abs(result$estimates$conf.high - normal_row$conf_high), 2e-8)
    expect_gt(abs(result$estimates$conf.low - t_row$conf_low), 0.5)
    expect_gt(abs(result$estimates$conf.high - t_row$conf_high), 0.5)
    expect_true(
      all(abs(result$covariance[rownames(expected_covariance), colnames(expected_covariance), drop = FALSE] - expected_covariance) < 1e-8)
    )
    expect_lt(abs(unname(result$covariance[expected$term, expected$term]) - expected$variance), 1e-8)
    expect_equal(result$estimates$n_obs, expected$n_obs)
    expect_equal(result$estimates$n_control, expected$n_control)
    expect_equal(result$estimates$n_treated, expected$n_treated)
  }
})

test_that("F034 R-native significance_level rejects alpha boundary values", {
  panel <- read_fixture_csv("parity", "f034-ci-df", "inputs", "panel.csv")
  invalid_values <- list(0, 1, -0.01, Inf, NA_real_, c(0.05, 0.10), "0.05")

  for (value in invalid_values) {
    expect_error(
      f034_run(panel, significance_level = value),
      regexp = "significance_level must be a finite number between 0 and 1",
      class = "didbjs_contract_error"
    )
  }
})

test_that("F034 Kyle-compatible wrapper preserves Kyle CI schema", {
  panel <- read_fixture_csv("parity", "f034-ci-df", "inputs", "panel.csv")
  kyle_estimates <- read_fixture_csv("parity", "f034-ci-df", "expected", "kyle", "estimates.csv")
  kyle_schema <- jsonlite::fromJSON(fixture_path("parity", "f034-ci-df", "expected", "kyle", "output-schema.json"))
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f034-ci-df", "expected", "kyle", "diagnostics.json"))

  out <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  )

  expect_s3_class(out, "data.table")
  expect_equal(kyle_diag$status, "success")
  expect_equal(kyle_diag$critical_value, 1.96)
  expect_lt(abs(kyle_diag$exact_normal_critical_gap - abs(1.96 - stats::qnorm(0.975))), 1e-12)
  expect_equal(names(out), kyle_schema$names)
  expect_equal(out$term, as.character(kyle_estimates$term))
  expect_equal(out$estimate, kyle_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, kyle_estimates$std.error, tolerance = 1e-8)
  expect_equal(out$conf.low, kyle_estimates$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, kyle_estimates$conf.high, tolerance = 1e-8)
  expect_equal(out$conf.low, out$estimate - 1.96 * out$std.error, tolerance = 1e-12)
  expect_equal(out$conf.high, out$estimate + 1.96 * out$std.error, tolerance = 1e-12)
})
