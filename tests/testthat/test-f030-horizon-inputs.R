skip_if_not(fixtures_present())

f030_covariance <- function(covariance, scenario) {
  selected <- covariance[covariance$scenario == scenario, , drop = FALSE]
  terms <- unique(c(selected$row_term, selected$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(selected))) {
    mat[selected$row_term[[idx]], selected$col_term[[idx]]] <- selected$value[[idx]]
  }
  mat
}

test_that("F030 unsorted and sparse horizons match Stata order and covariance", {
  panel <- read_fixture_csv("parity", "f030-horizon-inputs", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f030-horizon-inputs", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f030-horizon-inputs", "expected", "stata", "covariance.csv")
  stata_unsorted_sample <- read_fixture_csv("parity", "f030-horizon-inputs", "expected", "stata", "sample-mask-unsorted.csv")
  stata_sparse_sample <- read_fixture_csv("parity", "f030-horizon-inputs", "expected", "stata", "sample-mask-sparse.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "python", "probes.json"))

  unsorted <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, horizons = c(2, 0))
  sparse <- did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", aw = "w", cluster = "unit", minn = 0, horizons = c(0, 2))

  for (scenario in c("unsorted", "sparse")) {
    expected <- stata_estimates[stata_estimates$scenario == scenario, , drop = FALSE]
    result <- get(scenario)
    expect_equal(stata_probes[[scenario]]$status, "reference_success", info = scenario)
    expect_equal(result$estimates$term, expected$term, info = scenario)
    expect_equal(result$estimates$estimate, expected$estimate, tolerance = 1e-10, info = scenario)
    expect_equal(result$estimates$std.error, expected$std_error, tolerance = 1e-8, info = scenario)
    expect_equal(result$estimates$n_obs, expected$n_obs, info = scenario)
    expect_equal(result$estimates$n_control, expected$n_control, info = scenario)
    expect_equal(result$estimates$n_treated, expected$n_treated, info = scenario)
    expect_equal(
      result$covariance[expected$term, expected$term, drop = FALSE],
      f030_covariance(stata_covariance, scenario),
      tolerance = 1e-8,
      info = scenario
    )
    expect_equal(python_probes[[scenario]]$terms, expected$term, info = scenario)
  }

  expect_equal(unsorted$sample_mask$row_id, as.character(stata_unsorted_sample$row_id))
  expect_true(all(unsorted$sample_mask$sample == as.logical(stata_unsorted_sample$sample)))
  expect_equal(sparse$sample_mask$row_id, as.character(stata_sparse_sample$row_id))
  expect_true(all(sparse$sample_mask$sample == as.logical(stata_sparse_sample$sample)))
})

test_that("F030 absent and empty horizons fail closed with reference divergence evidence", {
  panel <- read_fixture_csv("parity", "f030-horizon-inputs", "inputs", "panel.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "python", "probes.json"))
  kyle_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "kyle", "probes.json"))

  expect_equal(stata_probes$absent$status, "reference_success")
  expect_equal(python_probes$absent$status, "reference_success")
  expect_equal(kyle_probes$absent$status, "reference_success")
  expect_equal(stata_probes$empty$status, "reference_success")
  expect_equal(python_probes$empty$status, "reference_success")
  expect_equal(kyle_probes$empty$status, "reference_success")
  expect_equal(python_probes$absent$terms, c("tau0", "tau3"))
  expect_equal(kyle_probes$absent$terms, c("0", "3"))
  expect_equal(kyle_probes$empty$terms, c("0", "1", "2"))

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = c(0, 3)),
    regexp = "Horizon 3 has zero treated weight",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = integer(0)),
    regexp = "Empty horizons are invalid",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", horizon = integer(0)),
    regexp = "Empty horizons are invalid",
    class = "didbjs_contract_error"
  )

  py_empty <- did_imputation_python(df = panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = c("unit", "t"), aw = "w", minn = 0, horizons = integer(0))
  expect_s3_class(py_empty, "DIDImputationOutput")
  expect_named(py_empty$estimates, "tau_ate")
})

test_that("F030 invalid horizon inputs raise structured errors", {
  panel <- read_fixture_csv("parity", "f030-horizon-inputs", "inputs", "panel.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "python", "probes.json"))
  kyle_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "kyle", "probes.json"))

  expect_equal(stata_probes$duplicate$status, "reference_error")
  expect_equal(python_probes$duplicate$status, "reference_error")
  expect_equal(kyle_probes$duplicate$status, "reference_error")
  expect_equal(stata_probes$negative$status, "reference_error")
  expect_equal(python_probes$negative$status, "reference_success")
  expect_equal(kyle_probes$negative$status, "reference_error")
  expect_equal(stata_probes$horizons_allhorizons$status, "reference_error")
  expect_equal(python_probes$horizons_allhorizons$status, "reference_error")

  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = c(0, 0)),
    regexp = "Horizons cannot contain duplicates",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = -1),
    regexp = "Horizons must be non-negative",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = -1),
    regexp = "Horizons must be non-negative",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:1, allhorizons = TRUE),
    regexp = "Options horizons and allhorizons cannot be combined",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:1, allhorizons = TRUE),
    regexp = "Options horizons and allhorizons cannot be combined",
    class = "didbjs_contract_error"
  )
})

test_that("F030 compatibility wrappers preserve wrapper-specific horizon order and guards", {
  panel <- read_fixture_csv("parity", "f030-horizon-inputs", "inputs", "panel.csv")
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "python", "probes.json"))
  kyle_estimates <- read_fixture_csv("parity", "f030-horizon-inputs", "expected", "kyle", "estimates.csv")
  kyle_probes <- jsonlite::fromJSON(fixture_path("parity", "f030-horizon-inputs", "expected", "kyle", "probes.json"))

  py_unsorted <- did_imputation_python(df = panel, y = "Y", i = "unit", t = "t", Ei = "Ei", fe = c("unit", "t"), aw = "w", minn = 0, horizons = c(2, 0))
  expect_named(py_unsorted$estimates, python_probes$unsorted$terms)
  expect_equal(unname(unlist(py_unsorted$estimates)), c(3, 1), tolerance = 1e-10)

  kyle_unsorted <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    horizon = c(2, 0),
    cluster_var = "unit"
  )
  expected_kyle_unsorted <- kyle_estimates[kyle_estimates$scenario == "unsorted", , drop = FALSE]
  expect_equal(kyle_probes$unsorted$terms, c("0", "2"))
  expect_equal(kyle_unsorted$term, as.character(expected_kyle_unsorted$term))
  expect_equal(kyle_unsorted$estimate, expected_kyle_unsorted$estimate, tolerance = 1e-10)
  expect_equal(kyle_unsorted$std.error, expected_kyle_unsorted$std.error, tolerance = 1e-8)

  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", horizon = c(0, 3)),
    regexp = "Horizon 3 has zero treated weight",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", horizon = TRUE),
    regexp = "Kyle horizon = TRUE all-horizon output is not implemented yet",
    class = "didbjs_unsupported_error"
  )
})
