skip_if_not(fixtures_present())

f029_covariance <- function(covariance, scenario) {
  selected <- covariance[covariance$scenario == scenario, , drop = FALSE]
  terms <- unique(c(selected$row_term, selected$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(selected))) {
    mat[selected$row_term[[idx]], selected$col_term[[idx]]] <- selected$value[[idx]]
  }
  mat
}

test_that("F029 custom wtr scaling and untreated support match Stata", {
  panel <- read_fixture_csv("parity", "f029-wtr-validation", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "covariance.csv")
  stata_base_sample <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "sample-mask-base.csv")
  stata_untreated_sample <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "sample-mask-untreated-support.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f029-wtr-validation", "expected", "stata", "diagnostics.json"))

  base <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, wtr = "wtr_base")
  scaled <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, wtr = "wtr_scaled")
  untreated_support <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, wtr = "wtr_untreated")

  for (scenario in c("base", "scaled", "untreated_support")) {
    expected <- stata_estimates[stata_estimates$scenario == scenario, , drop = FALSE]
    result <- get(scenario)
    expect_equal(result$estimates$term, expected$term, info = scenario)
    expect_equal(result$estimates$estimate, expected$estimate, tolerance = 1e-10, info = scenario)
    expect_equal(result$estimates$std.error, expected$std_error, tolerance = 1e-8, info = scenario)
    expect_equal(result$estimates$n_obs, expected$n_obs, info = scenario)
    expect_equal(result$estimates$n_control, expected$n_control, info = scenario)
    expect_equal(result$estimates$n_treated, expected$n_treated, info = scenario)
    expect_equal(
      result$covariance[expected$term, expected$term, drop = FALSE],
      f029_covariance(stata_covariance, scenario),
      tolerance = 1e-8,
      info = scenario
    )
  }

  expect_lt(stata_diag$estimate_scale_abs_diff, 1e-12)
  expect_lt(stata_diag$untreated_support_abs_diff, 1e-12)
  expect_equal(base$estimates$estimate, scaled$estimates$estimate, tolerance = 1e-12)
  expect_equal(base$estimates$estimate, untreated_support$estimates$estimate, tolerance = 1e-12)
  expect_equal(base$sample_mask$row_id, as.character(stata_base_sample$row_id))
  expect_true(all(base$sample_mask$sample == as.logical(stata_base_sample$sample)))
  expect_equal(untreated_support$sample_mask$row_id, as.character(stata_untreated_sample$row_id))
  expect_true(all(untreated_support$sample_mask$sample == as.logical(stata_untreated_sample$sample)))
})

test_that("F029 multiple custom wtr and signed zero-sum estimands match Stata", {
  panel <- read_fixture_csv("parity", "f029-wtr-validation", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "covariance.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f029-wtr-validation", "expected", "stata", "diagnostics.json"))

  multiple <- did_imputation(
    panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    wtr = c("wtr_base", "wtr_alt")
  )
  sum_zero <- did_imputation(
    panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    wtr = "wtr_sum_zero",
    sum = TRUE
  )

  expected_multiple <- stata_estimates[stata_estimates$scenario == "multiple", , drop = FALSE]
  expected_sum_zero <- stata_estimates[stata_estimates$scenario == "sum_zero", , drop = FALSE]
  expect_equal(multiple$estimates$term, expected_multiple$term)
  expect_equal(multiple$estimates$estimate, expected_multiple$estimate, tolerance = 1e-8)
  expect_equal(multiple$estimates$std.error, expected_multiple$std_error, tolerance = 1e-8)
  expect_equal(
    multiple$covariance[expected_multiple$term, expected_multiple$term, drop = FALSE],
    f029_covariance(stata_covariance, "multiple"),
    tolerance = 1e-8
  )
  expect_equal(sum_zero$estimates$term, expected_sum_zero$term)
  expect_equal(sum_zero$estimates$estimate, expected_sum_zero$estimate, tolerance = 1e-10)
  expect_equal(sum_zero$estimates$std.error, expected_sum_zero$std_error, tolerance = 1e-8)
  expect_equal(sum_zero$covariance["tau", "tau"], f029_covariance(stata_covariance, "sum_zero")["tau", "tau"], tolerance = 1e-8)
  expect_equal(stata_diag$sum_zero_raw_weight_sum, 0, tolerance = 1e-12)
  expect_gt(stata_diag$sum_zero_raw_abs_weight_sum, 0.9)
})

test_that("F029 missing and zero custom wtr support follows Stata sample masks", {
  panel <- read_fixture_csv("parity", "f029-wtr-validation", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "estimates.csv")
  stata_missing_sample <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "sample-mask-missing.csv")
  stata_zero_sample <- read_fixture_csv("parity", "f029-wtr-validation", "expected", "stata", "sample-mask-zero.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f029-wtr-validation", "expected", "stata", "diagnostics.json"))

  missing <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, wtr = "wtr_missing")
  zero <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, wtr = "wtr_zero")
  expected_missing <- stata_estimates[stata_estimates$scenario == "missing", , drop = FALSE]
  expected_zero <- stata_estimates[stata_estimates$scenario == "zero", , drop = FALSE]

  expect_equal(stata_diag$missing_row_id, "2_3")
  expect_equal(stata_diag$missing_row_excluded, 1)
  expect_equal(missing$estimates$estimate, expected_missing$estimate, tolerance = 1e-8)
  expect_equal(missing$estimates$std.error, expected_missing$std_error, tolerance = 1e-8)
  expect_equal(missing$sample_mask$row_id, as.character(stata_missing_sample$row_id))
  expect_true(all(missing$sample_mask$sample == as.logical(stata_missing_sample$sample)))
  expect_false(missing$sample_mask$sample[missing$sample_mask$row_id == "2_3"])
  expect_true(missing$sample_mask$missing_required[missing$sample_mask$row_id == "2_3"])

  expect_equal(stata_diag$zero_estimate, 0)
  expect_equal(zero$estimates$estimate, expected_zero$estimate, tolerance = 1e-12)
  expect_equal(zero$estimates$std.error, expected_zero$std_error, tolerance = 1e-12)
  expect_equal(zero$estimates$n_obs, expected_zero$n_obs)
  expect_equal(zero$estimates$n_treated, expected_zero$n_treated)
  expect_equal(zero$sample_mask$row_id, as.character(stata_zero_sample$row_id))
  expect_true(all(zero$sample_mask$sample == as.logical(stata_zero_sample$sample)))
})

test_that("F029 custom wtr invalid combinations match Stata failure classes", {
  panel <- read_fixture_csv("parity", "f029-wtr-validation", "inputs", "panel.csv")
  stata_invalid <- jsonlite::fromJSON(fixture_path("parity", "f029-wtr-validation", "expected", "stata", "invalid-probes.json"))

  expect_equal(stata_invalid$duplicate_names$status, "reference_error")
  expect_equal(stata_invalid$negative_without_sum$status, "reference_error")
  expect_equal(stata_invalid$zero_treated_weight$status, "reference_success")

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = c("wtr_base", "wtr_base")),
    regexp = "cannot contain duplicates",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", wtr = "missing_wtr"),
    regexp = "Missing custom wtr columns",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", wtr = "wtr_negative"),
    regexp = "Negative custom wtr values require sum = TRUE",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", wtr = "wtr_negative"),
    regexp = "Negative custom wtr values require sum = TRUE",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", wtr = "wtr_base", sum = TRUE),
    class = "didbjs_unsupported_error"
  )
})
