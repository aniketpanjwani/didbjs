skip_if_not(fixtures_present())

test_that("F026 NA Inf after-sample and negative timing encodings match Stata", {
  panel <- read_fixture_csv("parity", "f026-treatment-timing", "inputs", "encodings.csv")
  stata_estimates <- read_fixture_csv("parity", "f026-treatment-timing", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f026-treatment-timing", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f026-treatment-timing", "expected", "stata", "sample-mask.csv")
  stata_timing <- read_fixture_csv("parity", "f026-treatment-timing", "expected", "stata", "timing-classification.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f026-treatment-timing", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = NULL,
    aw = "w",
    cluster = "unit",
    minn = 0
  )

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$n_obs, 30)
  expect_equal(stata_diag$n_control, 20)
  expect_equal(stata_diag$n_treated, 10)
  expect_equal(stata_diag$missing_never_rows, 10)
  expect_equal(stata_diag$after_sample_rows, 5)
  expect_equal(stata_diag$before_sample_negative_rows, 5)
  expect_equal(table(stata_timing$timing_class)[["missing_never"]], 10)
  expect_equal(table(stata_timing$timing_class)[["after_sample"]], 5)
  expect_equal(table(stata_timing$timing_class)[["before_sample_negative"]], 5)

  expect_equal(result$diagnostics$fe, character())
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

test_that("F026 Python-compatible wrapper preserves pre-zero timing shape", {
  panel <- read_fixture_csv("parity", "f026-treatment-timing", "inputs", "encodings.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f026-treatment-timing", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f026-treatment-timing", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f026-treatment-timing", "expected", "stata", "estimates.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = NULL,
    aw = "w",
    cluster = "unit",
    minn = 0
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null, info = field)
  }
  expect_equal(python_diag$status, "success")
  expect_equal(python_diag$rows_input, 30)
  expect_equal(python_diag$n_obs, 30)
  expect_equal(out$estimates$tau_ate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std_errors$tau_ate, stata_estimates$std_error, tolerance = 1e-8)
  expect_lt(abs(python_diag$estimates$tau_ate - stata_estimates$estimate), 1e-8)
})

test_that("F026 R-native timing validation rejects zero and inconsistent units", {
  panel <- read_fixture_csv("parity", "f026-treatment-timing", "inputs", "encodings.csv")
  zero_panel <- read_fixture_csv("parity", "f026-treatment-timing", "inputs", "kyle-zero.csv")

  expect_error(
    did_imputation(zero_panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0),
    regexp = "rejects zero treatment timing",
    class = "didbjs_contract_error"
  )

  inconsistent <- panel
  inconsistent$Ei[inconsistent$row_id == "1_2"] <- 4
  expect_error(
    did_imputation(inconsistent, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = NULL, aw = "w", cluster = "unit", minn = 0),
    regexp = "constant within unit",
    class = "didbjs_contract_error"
  )

  finite_and_missing <- panel
  finite_and_missing$Ei[finite_and_missing$row_id == "1_1"] <- NA_real_
  expect_error(
    did_imputation(finite_and_missing, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = NULL, aw = "w", cluster = "unit", minn = 0),
    regexp = "constant within unit",
    class = "didbjs_contract_error"
  )

  bad_type <- panel
  bad_type$Ei <- as.character(bad_type$Ei)
  expect_error(
    did_imputation(bad_type, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = NULL, aw = "w", cluster = "unit", minn = 0),
    regexp = "Treatment timing column must be numeric",
    class = "didbjs_contract_error"
  )
})

test_that("F026 Stata-style tagged missing values are never-treated", {
  panel <- read_fixture_csv("parity", "f026-treatment-timing", "inputs", "encodings.csv")
  base <- did_imputation(
    panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = NULL,
    aw = "w",
    cluster = "unit",
    minn = 0
  )

  tagged <- panel
  tagged$Ei[tagged$unit == 4] <- haven::tagged_na("a")
  tagged_result <- did_imputation(
    tagged,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = NULL,
    aw = "w",
    cluster = "unit",
    minn = 0
  )

  expect_equal(tagged_result$estimates$estimate, base$estimates$estimate, tolerance = 1e-12)
  expect_equal(tagged_result$estimates$std.error, base$estimates$std.error, tolerance = 1e-12)
  expect_equal(tagged_result$sample_mask$sample, base$sample_mask$sample)
})

test_that("F026 Kyle-compatible wrapper treats zero timing as never-treated", {
  panel <- read_fixture_csv("parity", "f026-treatment-timing", "inputs", "kyle-zero.csv")
  kyle_estimates <- read_fixture_csv("parity", "f026-treatment-timing", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f026-treatment-timing", "expected", "kyle", "diagnostics.json"))

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
  expect_equal(kyle_diag$zero_timing_rows, 6)
  expect_equal(out$term, kyle_estimates$term)
  expect_equal(out$estimate, kyle_estimates$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, kyle_estimates$std.error, tolerance = 1e-8)
})
