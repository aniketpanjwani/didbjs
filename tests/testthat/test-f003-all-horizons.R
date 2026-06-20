skip_if_not(fixtures_present())

test_that("F003 allhorizons discovers the Stata horizon set and ordering", {
  panel <- read_fixture_csv("parity", "f003-all-horizons", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f003-all-horizons", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f003-all-horizons", "expected", "stata", "covariance.csv")
  stata_sample <- read_fixture_csv("parity", "f003-all-horizons", "expected", "stata", "sample-mask.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f003-all-horizons", "expected", "stata", "diagnostics.json"))

  result <- did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    allhorizons = TRUE
  )

  expect_true(result$diagnostics$allhorizons)
  expect_equal(result$diagnostics$horizons, c(0L, 1L, 2L))
  expect_equal(result$estimates$term, stata_diag$discovered_terms)
  expect_equal(result$estimates$term, stata_estimates$term)
  expect_equal(result$estimates$estimate, stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(result$estimates$std.error, stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(result$estimates$n_treated, stata_estimates$n_treated)
  expect_equal(result$covariance[rownames(stata_covariance), colnames(stata_covariance)], stata_covariance, tolerance = 1e-8)
  expect_equal(result$sample_mask$row_id, stata_sample$row_id)
  expect_true(all(result$sample_mask$sample == as.logical(stata_sample$sample)))
})

test_that("F003 Python-compatible allhorizons preserves object shape with approved drift evidence", {
  panel <- read_fixture_csv("parity", "f003-all-horizons", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f003-all-horizons", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f003-all-horizons", "expected", "python", "diagnostics.json"))
  stata_estimates <- read_fixture_csv("parity", "f003-all-horizons", "expected", "stata", "estimates.csv")
  stata_covariance <- read_covariance_matrix("parity", "f003-all-horizons", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    aw = "w",
    minn = 0,
    allhorizons = TRUE
  )

  expect_s3_class(out, "DIDImputationOutput")
  schema_fields <- names(schema$fields)
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$fields[[field]]$is_null)
  }
  expect_named(out$estimates, python_diag$discovered_terms)
  expect_named(out$std_errors, python_diag$discovered_terms)
  expect_equal(names(out$estimates), stata_estimates$term)
  expect_equal(unname(unlist(out$estimates)), stata_estimates$estimate, tolerance = 1e-10)
  expect_equal(unname(unlist(out$std_errors)), stata_estimates$std_error, tolerance = 1e-8)
  expect_equal(out$V, sum(diag(stata_covariance)), tolerance = 1e-8)
  expect_false(python_diag$tol001_pass)
  expect_true(all(!unlist(python_diag$tol001_pass_by_term)))
})

test_that("F003 rejects ambiguous allhorizons inputs", {
  panel <- read_fixture_csv("parity", "f003-all-horizons", "inputs", "panel.csv")
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:2, allhorizons = TRUE),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", horizons = 0:2, allhorizons = TRUE),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation(panel, y = "Y", i = "unit", t = "t", Ei = "Ei", allhorizons = NA),
    class = "didbjs_contract_error"
  )
})
