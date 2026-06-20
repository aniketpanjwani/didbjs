test_that("F001 static ATT matches Stata and algebraic oracle", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "sample-mask.csv")

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "i",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "i",
    minn = 0
  )

  expect_s3_class(result, "didbjs")
  expect_equal(result$estimates$term, "tau")
  expect_equal(result$estimates$estimate, 2, tolerance = 1e-10)
  expect_equal(result$estimates$estimate, stata_estimates$estimate[1], tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error[1], tolerance = 1e-8)
  expect_equal(result$covariance[1, 1], stata_covariance$value[1], tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
})

test_that("F001 committed reference artifacts are present and structured", {
  python_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "python", "diagnostics.json"))
  kyle_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "kyle", "diagnostics.json"))
  kyle_alias <- read_fixture_csv("smoke", "f001-static-att", "expected", "kyle", "alias-estimates.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "stata", "diagnostics.json"))

  expect_equal(stata_diag$status, "success")
  expect_equal(python_diag$status, "success")
  expect_false(python_diag$tol001_pass)
  expect_equal(python_diag$estimate, 2.0000003632157988, tolerance = 1e-12)
  expect_equal(python_diag$algebraic_abs_diff, 3.632157987709661e-7, tolerance = 1e-15)
  expect_match(python_diag$root_cause_probe, "recover_fixed_effects_iterative", fixed = TRUE)
  expect_equal(kyle_diag$status, "reference_error")
  expect_match(kyle_diag$error_message, "subscript out of bounds")
  expect_match(kyle_diag$root_cause, "data column named i", fixed = TRUE)
  expect_equal(kyle_diag$alias_probe_status, "success")
  expect_equal(kyle_alias$term, "treat")
  expect_equal(kyle_alias$estimate, 2, tolerance = 1e-10)
  expect_equal(kyle_alias$std.error, 0.063245553203368, tolerance = 1e-8)
})
