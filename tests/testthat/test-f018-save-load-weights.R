skip_if_not(fixtures_present())

test_that("F018 saveweights artifact matches Stata and reproduces the estimator", {
  panel <- read_fixture_csv("parity", "f018-save-load-weights", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f018-save-load-weights", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f018-save-load-weights", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f018-save-load-weights", "expected", "stata", "sample-mask.csv")
  stata_weights <- read_fixture_csv("parity", "f018-save-load-weights", "expected", "stata", "imputation-weights.csv")
  stata_checks <- read_fixture_csv("parity", "f018-save-load-weights", "expected", "stata", "weight-checks.csv")
  checks <- stats::setNames(stata_checks$value, stata_checks$check)

  original_names <- names(panel)
  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    saveweights = TRUE
  )

  expect_equal(names(panel), original_names)
  expect_false(any(grepl("^__w_", names(panel))))
  expect_s3_class(result$artifacts$weights, "didbjs_weights")
  expect_equal(result$artifacts$weights$schema_version, "didbjs.weights.v1")
  expect_true(nzchar(result$artifacts$weights$metadata$spec_hash))
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

  weights <- result$artifacts$weights$weights
  expect_equal(weights$row_id, as.character(stata_weights$row_id))
  expect_equal(weights$term, stata_weights$term)
  expect_equal(weights$weight, stata_weights$weight, tolerance = 1e-8)

  panel_idx <- match(weights$row_id, panel$row_id)
  expect_false(anyNA(panel_idx))
  expect_equal(sum(weights$weight * panel$Y[panel_idx]), unname(checks["weighted_y_estimate"]), tolerance = 1e-8)
  expect_equal(sum(weights$weight[panel$D[panel_idx] == 1]), unname(checks["treated_weight_sum"]), tolerance = 1e-12)
  expect_equal(max(abs(tapply(weights$weight, panel$unit[panel_idx], sum))), unname(checks["max_abs_unit_sum"]), tolerance = 1e-8)
  expect_equal(max(abs(tapply(weights$weight, panel$t[panel_idx], sum))), unname(checks["max_abs_time_sum"]), tolerance = 1e-12)
})

test_that("F018 loadweights reuses saved weights for a second outcome", {
  panel <- read_fixture_csv("parity", "f018-save-load-weights", "inputs", "panel.csv")
  stata_load_estimates <- read_fixture_csv("parity", "f018-save-load-weights", "expected", "stata", "load-estimates.csv")
  stata_load_covariance <- read_covariance_matrix("parity", "f018-save-load-weights", "expected", "stata", "load-covariance.csv")

  saved <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    saveweights = TRUE
  )
  loaded <- did_imputation(
    data = panel,
    y = "Y2",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit",
    loadweights = saved$artifacts$weights
  )
  full <- did_imputation(
    data = panel,
    y = "Y2",
    i = "unit",
    t = "t",
    Ei = "Ei",
    minn = 0,
    cluster = "unit"
  )

  expect_true(loaded$diagnostics$loadweights)
  expect_equal(loaded$estimates$term, stata_load_estimates$term)
  expect_equal(loaded$estimates$estimate, stata_load_estimates$estimate, tolerance = 1e-8)
  expect_equal(loaded$estimates$std.error, stata_load_estimates$std_error, tolerance = 1e-8)
  expect_equal(
    loaded$covariance[rownames(stata_load_covariance), colnames(stata_load_covariance), drop = FALSE],
    stata_load_covariance,
    tolerance = 1e-8
  )
  expect_equal(loaded$estimates$estimate, full$estimates$estimate, tolerance = 1e-10)
  expect_equal(loaded$estimates$std.error, full$estimates$std.error, tolerance = 1e-8)

  incompatible <- saved$artifacts$weights
  incompatible$metadata$spec_hash <- "not-the-current-spec"
  expect_error(
    did_imputation(panel, "Y2", "unit", "t", "Ei", minn = 0, cluster = "unit", loadweights = incompatible),
    regexp = "incompatible sample or specification",
    class = "didbjs_contract_error"
  )
})

test_that("F018 Python-compatible saveweights field matches pinned Python shape", {
  panel <- read_fixture_csv("parity", "f018-save-load-weights", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f018-save-load-weights", "expected", "python", "object-schema.json"))
  python_weights <- read_fixture_csv("parity", "f018-save-load-weights", "expected", "python", "weights.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = character(),
    minn = 0,
    saveweights = TRUE
  )

  expect_s3_class(out, "DIDImputationOutput")
  expect_false(is.null(out$weights))
  expect_equal(names(out$weights), schema$weights_columns)
  expect_equal(out$weights$copywtr, python_weights$weight, tolerance = 1e-8)
})
