skip_if_not(fixtures_present())

test_that("F025 missingness and subset masks match Stata", {
  panel <- read_fixture_csv("parity", "f025-missingness-subset", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f025-missingness-subset", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f025-missingness-subset", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f025-missingness-subset", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f025-missingness-subset", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    controls = c("x1", "x2"),
    fe = c("group", "t"),
    aw = "w",
    cluster = "clust",
    subset = "keep",
    minn = 0
  )

  expected_tau <- stata_estimates[stata_estimates$term == "tau", , drop = FALSE]
  expected_controls <- stata_estimates[stata_estimates$term %in% c("x1", "x2"), , drop = FALSE]

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$n_obs, 34)
  expect_equal(stata_diag$n_control, 24)
  expect_equal(stata_diag$n_treated, 10)
  expect_equal(stata_diag$subset_excluded, 1)
  expect_equal(stata_diag$missing_excluded, 5)

  expect_true(result$diagnostics$subset)
  expect_equal(result$diagnostics$subset_excluded_row_ids, "1")
  expect_equal(result$diagnostics$missing_excluded_row_ids, c("2", "6", "11", "12", "13"))
  expect_equal(result$estimates$term, expected_tau$term)
  expect_equal(result$estimates$estimate, expected_tau$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, expected_tau$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_obs, expected_tau$n_obs)
  expect_equal(result$estimates$n_control, expected_tau$n_control)
  expect_equal(result$estimates$n_treated, as.integer(expected_tau$n_treated))
  expect_equal(result$controls$term, expected_controls$term)
  expect_equal(result$controls$estimate, expected_controls$estimate, tolerance = 1e-10)
  expect_equal(result$controls$std.error, expected_controls$std_error, tolerance = 1e-8)
  expect_equal(
    result$covariance[rownames(stata_covariance), colnames(stata_covariance), drop = FALSE],
    stata_covariance,
    tolerance = 1e-8
  )

  expect_equal(result$sample_mask$row_id, as.character(stata_sample$row_id))
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
  expect_true(all(result$sample_mask$subset == as.logical(stata_sample$keep)))
  expect_true(all(result$sample_mask$missing_required == as.logical(stata_sample$missing_required)))

  included_missing_timing <- is.na(panel$Ei) &
    result$sample_mask$subset &
    !result$sample_mask$missing_required
  expect_true(any(included_missing_timing))
  expect_true(all(result$sample_mask$sample[included_missing_timing]))
})

test_that("F025 subset validation is fail-closed and structured", {
  panel <- read_fixture_csv("parity", "f025-missingness-subset", "inputs", "panel.csv")

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", subset = "missing_keep"),
    regexp = "subset column not found",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", subset = rep(TRUE, nrow(panel) - 1)),
    regexp = "one value per row",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", subset = c(rep(1, nrow(panel) - 1), 2)),
    regexp = "0/1 indicators",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", subset = rep(FALSE, nrow(panel))),
    regexp = "No observations remain",
    class = "didbjs_contract_error"
  )
})

test_that("F025 Python-compatible subset equals the prefiltered Python reference shape", {
  panel <- read_fixture_csv("parity", "f025-missingness-subset", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f025-missingness-subset", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f025-missingness-subset", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f025-missingness-subset", "expected", "stata", "estimates.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    controls = c("x1", "x2"),
    fe = c("group", "t"),
    aw = "w",
    cluster = "clust",
    subset = "keep",
    minn = 0
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null, info = field)
  }

  expected_tau <- stata_estimates[stata_estimates$term == "tau", , drop = FALSE]
  expect_equal(python_diag$status, "success")
  expect_equal(python_diag$rows_input, 40)
  expect_equal(python_diag$rows_after_subset, 39)
  expect_equal(python_diag$rows_dropped_by_subset, 1)
  expect_equal(python_diag$n_obs, 34)
  expect_lt(abs(python_diag$estimates$tau_ate - expected_tau$estimate), 1e-8)
  expect_equal(out$estimates$tau_ate, expected_tau$estimate, tolerance = 1e-10)
  expect_equal(out$std_errors$tau_ate, expected_tau$std_error, tolerance = 1e-8)

  wrapper_diag <- attr(out, "diagnostics")
  expect_true(wrapper_diag$subset)
  expect_equal(wrapper_diag$subset_excluded_row_ids, "1")
  expect_equal(wrapper_diag$missing_excluded_row_ids, c("2", "6", "11", "12", "13"))
})

test_that("F025 Kyle-compatible subset equals the prefiltered Kyle reference shape", {
  panel <- read_fixture_csv("parity", "f025-missingness-subset", "inputs", "panel.csv")
  kyle_estimates <- read_fixture_csv("parity", "f025-missingness-subset", "expected", "kyle", "estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("parity", "f025-missingness-subset", "expected", "kyle", "diagnostics.json"))

  out <- NULL
  expect_warning(
    out <- did_imputation_kyle(
      data = panel,
      yname = "Y",
      gname = "Ei",
      tname = "t",
      idname = "unit",
      wname = "w",
      cluster_var = "clust",
      subset = "keep"
    ),
    regexp = "NA standard errors",
    class = "didbjs_kyle_missingness_warning"
  )

  expect_s3_class(out, "data.table")
  expect_named(out, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(kyle_diag$status, "success")
  expect_equal(kyle_diag$rows_input, 40)
  expect_equal(kyle_diag$rows_after_subset, 39)
  expect_equal(kyle_diag$rows_dropped_by_subset, 1)
  expect_true(kyle_diag$std_error_is_na)
  expect_equal(out$term, kyle_estimates$term)
  expect_equal(out$estimate, kyle_estimates$estimate, tolerance = 1e-10)
  expect_true(all(is.na(out$std.error)))
  expect_true(all(is.na(kyle_estimates$std.error)))
})

test_that("F025 Kyle missingness fallback validates weights and wraps failures", {
  panel <- read_fixture_csv("parity", "f025-missingness-subset", "inputs", "panel.csv")
  kept <- which(panel$keep == 1)

  negative_weight <- panel
  negative_weight$w[[kept[[1]]]] <- -1
  expect_error(
    kyle_static_missingness_fallback(
      dt = data.table::as.data.table(negative_weight[negative_weight$keep == 1, , drop = FALSE]),
      yvar = "Y",
      gname = "Ei",
      tname = "t",
      idname = "unit",
      wname = "w"
    ),
    regexp = "positive and finite",
    class = "didbjs_contract_error"
  )

  nonfinite_weight <- panel
  nonfinite_weight$w[[kept[[1]]]] <- Inf
  expect_error(
    kyle_static_missingness_fallback(
      dt = data.table::as.data.table(nonfinite_weight[nonfinite_weight$keep == 1, , drop = FALSE]),
      yvar = "Y",
      gname = "Ei",
      tname = "t",
      idname = "unit",
      wname = "w"
    ),
    regexp = "positive and finite",
    class = "didbjs_contract_error"
  )

  expect_error(
    kyle_static_missingness_fallback(
      dt = data.table::as.data.table(panel[panel$keep == 1, , drop = FALSE]),
      yvar = "missing_outcome",
      gname = "Ei",
      tname = "t",
      idname = "unit",
      wname = "w"
    ),
    regexp = "Kyle-compatible missingness fallback failed",
    class = "didbjs_contract_error"
  )
})
