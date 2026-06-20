skip_if_not(fixtures_present())

f016_covariance_for_spec <- function(covariance, spec) {
  rows <- covariance[covariance$spec == spec, , drop = FALSE]
  terms <- unique(c(rows$row_term, rows$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(rows))) {
    mat[rows$row_term[[idx]], rows$col_term[[idx]]] <- rows$value[[idx]]
  }
  mat
}

test_that("F016 small-cohort avgeffectsby matches Stata covariance behavior", {
  panel <- read_fixture_csv("parity", "f016-small-cohorts", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f016-small-cohorts", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f016-small-cohorts", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f016-small-cohorts", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f016-small-cohorts", "expected", "stata", "diagnostics.json"))
  specs <- list(
    default = NULL,
    avgeffectsby_D = "D"
  )

  results <- list()
  for (spec in names(specs)) {
    expect_warning(
      results[[spec]] <- did_imputation(
        data = panel,
        y = "Y",
        i = "unit",
        t = "t",
        Ei = "Ei",
        minn = 0,
        cluster = "unit",
        avgeffectsby = specs[[spec]]
      ),
      NA,
      info = spec
    )
    result <- results[[spec]]
    expected_estimate <- stata_estimates[stata_estimates$spec == spec, , drop = FALSE]
    expected_covariance <- f016_covariance_for_spec(stata_covariance, spec)
    expected_sample <- stata_sample[stata_sample$spec == spec, , drop = FALSE]

    expect_equal(result$diagnostics$avgeffectsby, stata_diag$avgeffectsby[[spec]], info = spec)
    expect_equal(result$estimates$term, expected_estimate$term, info = spec)
    expect_equal(result$estimates$estimate, expected_estimate$estimate, tolerance = 1e-8, info = spec)
    expect_equal(result$estimates$std.error, expected_estimate$std_error, tolerance = 1e-8, info = spec)
    expect_equal(result$estimates$n_obs, expected_estimate$n_obs, info = spec)
    expect_equal(result$estimates$n_control, expected_estimate$n_control, info = spec)
    expect_equal(result$estimates$n_treated, expected_estimate$n_treated, info = spec)
    expect_equal(
      result$covariance[rownames(expected_covariance), colnames(expected_covariance), drop = FALSE],
      expected_covariance,
      tolerance = 1e-8,
      info = spec
    )
    expect_equal(result$sample_mask$row_id, as.character(expected_sample$row_id), info = spec)
    expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = spec)
  }

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$small_cohort_warning_text, "")
  expect_equal(stata_diag$specs, names(specs))
  expect_equal(
    results$default$estimates$estimate,
    results$avgeffectsby_D$estimates$estimate,
    tolerance = 1e-8
  )
  expect_lt(results$default$estimates$std.error, 1e-6)
  expect_gt(results$avgeffectsby_D$estimates$std.error, 0.3)
})

test_that("F016 avgeffectsby validation is structured", {
  panel <- read_fixture_csv("parity", "f016-small-cohorts", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", avgeffectsby = "missing_group"),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", avgeffectsby = c("D", "D")),
    class = "didbjs_contract_error"
  )
})
