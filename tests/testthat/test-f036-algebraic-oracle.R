test_that("F036 algebraic oracle is independently derived from the F001 data-generating process", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  oracle <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "metadata", "f036-algebraic-oracle.json"))
  horizon_effects <- read_fixture_csv("smoke", "f001-static-att", "metadata", "f036-horizon-effects.csv")
  treated_weights <- read_fixture_csv("smoke", "f001-static-att", "metadata", "f036-treated-weights.csv")

  expect_equal(oracle$status, "success")
  expect_match(oracle$source, "no R package code is used", fixed = TRUE)
  expect_equal(nrow(panel), oracle$counts$n_obs)
  expect_equal(sum(panel$D == 0), oracle$counts$n_control)
  expect_equal(sum(panel$D == 1), oracle$counts$n_treated)
  expect_equal(panel$Y0, 10 * panel$i + panel$t)
  expected_tau <- ifelse(panel$D == 1, 1 + panel$event_time + (panel$i - 3) / 10, 0)
  expect_equal(panel$tau, expected_tau)
  expect_equal(panel$Y, panel$Y0 + panel$tau)
  expect_true(all(panel$w == 1))

  treated <- panel[panel$D == 1, ]
  expect_equal(mean(treated$tau), oracle$oracle$static_att, tolerance = 1e-12)
  expect_equal(sum(treated$tau), oracle$oracle$treated_tau_sum, tolerance = 1e-12)
  expect_equal(
    stats::setNames(horizon_effects$effect, horizon_effects$term),
    unlist(oracle$oracle$horizon_effects),
    tolerance = 1e-12
  )
  for (term in horizon_effects$term) {
    horizon <- horizon_effects$event_time[horizon_effects$term == term]
    expected <- mean(treated$tau[treated$event_time == horizon])
    expect_equal(horizon_effects$effect[horizon_effects$term == term], expected, tolerance = 1e-12)
  }
  expect_equal(sum(treated_weights$static_weight), 1, tolerance = 1e-12)
  expect_equal(sum(treated_weights$static_contribution), oracle$oracle$static_att, tolerance = 1e-12)
  by_horizon_weight <- stats::aggregate(horizon_weight ~ horizon_term, treated_weights, sum)
  expect_true(all(abs(by_horizon_weight$horizon_weight - 1) < 1e-12))
  by_horizon_contribution <- stats::aggregate(horizon_contribution ~ horizon_term, treated_weights, sum)
  expect_equal(
    stats::setNames(by_horizon_contribution$horizon_contribution, by_horizon_contribution$horizon_term),
    unlist(oracle$oracle$horizon_effects),
    tolerance = 1e-12
  )
})

test_that("F036 R-native estimator matches the frozen algebraic oracle", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  oracle <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "metadata", "f036-algebraic-oracle.json"))
  horizon_effects <- read_fixture_csv("smoke", "f001-static-att", "metadata", "f036-horizon-effects.csv")

  static <- did_imputation(panel, y = "Y", i = "i", t = "t", Ei = "Ei", aw = "w", cluster = "i", minn = 0)
  dynamic <- did_imputation(
    panel,
    y = "Y",
    i = "i",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "i",
    minn = 0,
    horizons = horizon_effects$event_time
  )

  expect_equal(static$estimates$estimate, oracle$oracle$static_att, tolerance = 1e-12)
  expect_equal(static$estimates$n_obs, oracle$counts$n_obs)
  expect_equal(static$estimates$n_control, oracle$counts$n_control)
  expect_equal(static$estimates$n_treated, oracle$counts$n_treated)
  expect_equal(dynamic$estimates$term, horizon_effects$term)
  expect_equal(dynamic$estimates$estimate, horizon_effects$effect, tolerance = 1e-12)
  expect_equal(dynamic$estimates$n_treated, horizon_effects$treated_count)
})

test_that("F036 external references are compared only after the algebraic oracle is fixed", {
  oracle <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "metadata", "f036-algebraic-oracle.json"))
  stata_estimates <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "estimates.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "stata", "diagnostics.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "python", "diagnostics.json"))
  kyle_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "kyle", "diagnostics.json"))
  kyle_alias <- read_fixture_csv("smoke", "f001-static-att", "expected", "kyle", "alias-estimates.csv")

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$n_obs, oracle$counts$n_obs)
  expect_equal(stata_diag$n_control, oracle$counts$n_control)
  expect_equal(stata_diag$n_treated, oracle$counts$n_treated)
  expect_equal(stata_estimates$estimate[stata_estimates$term == "tau"], oracle$oracle$static_att, tolerance = 1e-10)
  expect_equal(kyle_diag$status, "reference_error")
  expect_equal(kyle_diag$alias_probe_status, "success")
  expect_equal(kyle_alias$estimate[kyle_alias$term == "treat"], oracle$oracle$static_att, tolerance = 1e-10)
  expect_equal(python_diag$status, "success")
  expect_equal(python_diag$algebraic_static_att, oracle$oracle$static_att, tolerance = 1e-12)
  expect_false(python_diag$tol001_pass)
  expect_equal(python_diag$algebraic_abs_diff, abs(python_diag$estimate - oracle$oracle$static_att), tolerance = 1e-15)
})
