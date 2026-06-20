test_that("benchmark budgets remain numeric and fail-capable", {
  budget_path <- system.file("spec", "bench-budgets.yml", package = "didbjs")
  if (!nzchar(budget_path)) {
    budget_path <- testthat::test_path("..", "..", "bench", "budgets.yml")
  }
  budgets <- yaml::read_yaml(budget_path)
  expect_equal(budgets$status, "frozen")
  expect_true(budgets$global_rules$fail_on_missing_baseline)
  expect_true(budgets$global_rules$fail_on_missing_budget)
  for (budget in budgets$budgets) {
    expect_true(any(grepl("^max_", names(budget))))
  }
})
