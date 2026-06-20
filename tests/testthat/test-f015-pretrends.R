skip_if_not(fixtures_present())

test_that("F015 pretrends match Stata estimates covariance diagnostics and sample mask", {
  panel <- read_fixture_csv("parity", "f015-pretrends", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f015-pretrends", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f015-pretrends", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f015-pretrends", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f015-pretrends", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    pretrends = 2,
    minn = 0,
    cluster = "unit"
  )
  no_pretrends <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    pretrends = 0,
    minn = 0,
    cluster = "unit"
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(result$estimates$term, c("tau0", "tau1", "pre1", "pre2"))
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, stata_estimates$n_obs)
  expect_equal(result$estimates$n_control, stata_estimates$n_control)
  stata_n_treated <- suppressWarnings(as.numeric(trimws(stata_estimates$n_treated)))
  expect_equal(result$estimates$n_treated[1:2], stata_n_treated[1:2])
  expect_true(all(is.na(result$estimates$n_treated[3:4])))
  expect_true(all(is.na(stata_n_treated[3:4])))
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-8
  )
  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_equal(result$diagnostics$pretrends, stata_diag$pretrends)
  expect_equal(result$diagnostics$pre_F, stata_diag$pre_F, tolerance = 1e-8)
  expect_lt(abs(result$diagnostics$pre_p - stata_diag$pre_p), 1e-18)
  expect_equal(result$diagnostics$pre_df, stata_diag$pre_df)
  expect_equal(result$estimates$estimate[1:2], no_pretrends$estimates$estimate, tolerance = 1e-10)
  expect_true(glance(result)$has_pretrends)
  expect_false(glance(no_pretrends)$has_pretrends)

  alpha10 <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    horizons = 0:1,
    pretrends = 2,
    minn = 0,
    cluster = "unit",
    significance_level = 0.10
  )
  pre_rows <- alpha10$estimates$term %in% c("pre1", "pre2")
  critical_value <- stats::qnorm(0.95)
  expect_equal(
    alpha10$estimates$conf.low[pre_rows],
    alpha10$estimates$estimate[pre_rows] - critical_value * alpha10$estimates$std.error[pre_rows],
    tolerance = 1e-15
  )
  expect_equal(
    alpha10$estimates$conf.high[pre_rows],
    alpha10$estimates$estimate[pre_rows] + critical_value * alpha10$estimates$std.error[pre_rows],
    tolerance = 1e-15
  )
  expect_gt(
    max(abs(alpha10$estimates$conf.low[pre_rows] -
      (alpha10$estimates$estimate[pre_rows] - 1.96 * alpha10$estimates$std.error[pre_rows]))),
    0.01
  )
})

test_that("F015 Python-compatible wrapper exposes pretrend aliases separately", {
  panel <- read_fixture_csv("parity", "f015-pretrends", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f015-pretrends", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f015-pretrends", "expected", "python", "diagnostics.json"))
  python_estimates <- read_fixture_csv("parity", "f015-pretrends", "expected", "python", "estimates.csv")
  python_pretrends <- read_fixture_csv("parity", "f015-pretrends", "expected", "python", "pretrends.csv")
  python_covariance <- read_fixture_csv("parity", "f015-pretrends", "expected", "python", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    horizons = 0:1,
    pretrends = 2,
    minn = 0,
    cluster = "unit"
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_equal(python_diag$terms, c("tau0", "tau1", "pre1", "pre2"))
  expect_named(out$estimates, python_estimates$term)
  expect_named(out$pretrends_estimates, python_pretrends$term)
  expect_named(out$pretrends_std_errors, python_pretrends$term)
  expect_equal(unname(unlist(out$estimates)), python_estimates$estimate, tolerance = 1e-6)
  expect_equal(unname(unlist(out$std_errors)), python_estimates$std_error, tolerance = 1e-8)
  expect_equal(unname(unlist(out$pretrends_estimates)), python_pretrends$estimate, tolerance = 1e-8)
  expect_equal(unname(unlist(out$pretrends_std_errors)), python_pretrends$std_error, tolerance = 1e-8)
  expect_equal(out$V, python_covariance$value[1], tolerance = 1e-8)
})

test_that("F015 Kyle-compatible wrapper matches Kyle pretrend row shape", {
  panel <- read_fixture_csv("parity", "f015-pretrends", "inputs", "panel.csv")
  kyle_estimates <- read_fixture_csv("parity", "f015-pretrends", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f015-pretrends", "expected", "kyle", "diagnostics.json"))

  out <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    horizon = 0:1,
    pretrends = c(-2, -1),
    cluster_var = "unit"
  )

  expect_s3_class(out, "data.table")
  expect_named(out, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(kyle_diag$status, "success")
  expect_equal(out$term, as.character(kyle_estimates$term))
  expect_equal(out$estimate, kyle_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, kyle_estimates$std.error, tolerance = 1e-8)
  expect_equal(out$conf.low, kyle_estimates$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, kyle_estimates$conf.high, tolerance = 1e-8)
  expect_equal(out$conf.low, out$estimate - 1.96 * out$std.error, tolerance = 1e-12)
  expect_equal(out$conf.high, out$estimate + 1.96 * out$std.error, tolerance = 1e-12)
})
