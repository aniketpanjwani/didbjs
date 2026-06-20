skip_if_not(fixtures_present())

test_that("F027 irregular and unbalanced panel matches Stata and algebraic ATT", {
  panel <- read_fixture_csv("parity", "f027-irregular-unbalanced", "inputs", "irregular.csv")
  stata_estimates <- read_fixture_csv("parity", "f027-irregular-unbalanced", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f027-irregular-unbalanced", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f027-irregular-unbalanced", "expected", "stata", "sample-mask.csv")
  panel_structure <- read_fixture_csv("parity", "f027-irregular-unbalanced", "expected", "stata", "panel-structure.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    cluster = "unit",
    minn = 0
  )

  algebraic_att <- 2
  expect_equal(sort(unique(panel$t)), c(1, 2, 3, 5))
  expect_true(any(panel_structure$has_gap == 1))
  expect_gt(length(unique(panel_structure$n_rows)), 1)
  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$n_obs, 26)
  expect_equal(stata_diag$n_control, 19)
  expect_equal(stata_diag$n_treated, 7)
  expect_equal(stata_diag$algebraic_att, algebraic_att, tolerance = 1e-12)
  expect_equal(stata_diag$estimate, algebraic_att, tolerance = 1e-8)
  expect_equal(result$estimates$estimate, algebraic_att, tolerance = 1e-12)

  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-8)
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

test_that("F027 Python-compatible irregular panel preserves object shape with recorded drift", {
  panel <- read_fixture_csv("parity", "f027-irregular-unbalanced", "inputs", "irregular.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f027-irregular-unbalanced", "expected", "stata", "estimates.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
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
  expect_equal(python_diag$rows_input, 26)
  expect_equal(python_diag$n_obs, 26)
  expect_gt(abs(python_diag$estimates$tau_ate - stata_estimates$estimate), 1e-8)
  expect_lt(abs(python_diag$estimates$tau_ate - stata_estimates$estimate), 1e-6)
  expect_equal(out$estimates$tau_ate, stata_estimates$estimate, tolerance = 1e-8)
  expect_equal(out$std_errors$tau_ate, stata_estimates$std_error, tolerance = 1e-8)
})

test_that("F027 Kyle-compatible irregular panel preserves shape with Stata-governed core", {
  panel <- read_fixture_csv("parity", "f027-irregular-unbalanced", "inputs", "irregular.csv")
  kyle_estimates <- read_fixture_csv("parity", "f027-irregular-unbalanced", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "kyle", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f027-irregular-unbalanced", "expected", "stata", "estimates.csv")

  out <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    cluster_var = "unit"
  )

  expect_s3_class(out, "data.table")
  expect_named(out, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(kyle_diag$status, "success")
  expect_equal(kyle_diag$rows_input, 26)
  expect_equal(out$term, "treat")
  expect_equal(out$estimate, stata_estimates$estimate, tolerance = 1e-8)
  expect_equal(out$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(kyle_estimates$estimate, 2, tolerance = 1e-12)
})

test_that("F027 duplicate unit-time rows fail closed despite permissive references", {
  duplicate_panel <- read_fixture_csv("parity", "f027-irregular-unbalanced", "inputs", "duplicates.csv")
  stata_probe <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "stata", "duplicate-probe.json"))
  python_probe <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "python", "duplicate-probe.json"))
  kyle_probe <- jsonlite::fromJSON(fixture_path("parity", "f027-irregular-unbalanced", "expected", "kyle", "duplicate-probe.json"))

  expect_equal(stata_probe$status, "reference_success_with_duplicates")
  expect_equal(python_probe$status, "reference_success_with_duplicates")
  expect_equal(kyle_probe$status, "reference_success_with_duplicates")
  expect_equal(stata_probe$duplicate_unit_time_rows, 2)
  expect_equal(python_probe$duplicate_unit_time_rows, 2)
  expect_equal(kyle_probe$duplicate_unit_time_rows, 2)

  expect_error(
    did_imputation(duplicate_panel, y = "Y", i = "unit", t = "t", Ei = "Ei", cluster = "unit", minn = 0),
    regexp = "duplicate row ids: 1_1, dup_1_1",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(duplicate_panel, y = "Y", i = "unit", t = "t", Ei = "Ei", cluster = "unit", minn = 0),
    regexp = "duplicate row ids: 1_1, dup_1_1",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(duplicate_panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", cluster_var = "unit"),
    regexp = "duplicate row ids: 1_1, dup_1_1",
    class = "didbjs_contract_error"
  )
})
