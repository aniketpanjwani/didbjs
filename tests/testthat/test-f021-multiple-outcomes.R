skip_if_not(fixtures_present())

test_that("F021 Kyle multi-outcome wrapper matches Kyle lhs output", {
  panel <- read_fixture_csv("parity", "f021-multiple-outcomes", "inputs", "panel.csv")
  kyle_multi <- read_fixture_csv("parity", "f021-multiple-outcomes", "expected", "kyle", "multi-estimates.csv")
  kyle_single_y <- read_fixture_csv("parity", "f021-multiple-outcomes", "expected", "kyle", "single-Y-estimates.csv")
  kyle_single_y2 <- read_fixture_csv("parity", "f021-multiple-outcomes", "expected", "kyle", "single-Y2-estimates.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f021-multiple-outcomes", "expected", "kyle", "output-schema.json"))
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f021-multiple-outcomes", "expected", "kyle", "diagnostics.json"))

  out <- did_imputation_kyle(
    data = panel,
    yname = "c(Y, Y2)",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  )

  expect_s3_class(out, "data.table")
  expect_equal(diagnostics$status, "success")
  expect_true(diagnostics$single_probe_match)
  expect_gt(diagnostics$estimate_gap, 2.9)
  expect_equal(schema$names, c("lhs", "term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_named(out, schema$names)
  expect_equal(out$lhs, kyle_multi$lhs)
  expect_equal(out$term, kyle_multi$term)
  expect_equal(out$estimate, kyle_multi$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, kyle_multi$std.error, tolerance = 1e-8)
  expect_equal(out$conf.low, kyle_multi$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, kyle_multi$conf.high, tolerance = 1e-8)

  expect_equal(out$estimate[out$lhs == "Y"], kyle_single_y$estimate, tolerance = 1e-10)
  expect_equal(out$std.error[out$lhs == "Y"], kyle_single_y$std.error, tolerance = 1e-8)
  expect_equal(out$estimate[out$lhs == "Y2"], kyle_single_y2$estimate, tolerance = 1e-10)
  expect_equal(out$std.error[out$lhs == "Y2"], kyle_single_y2$std.error, tolerance = 1e-8)
  expect_equal(out$estimate[out$lhs == "Y2"] - out$estimate[out$lhs == "Y"], 3, tolerance = 1e-10)
  expect_equal(out$std.error[out$lhs == "Y2"] / out$std.error[out$lhs == "Y"], 2, tolerance = 1e-8)
})

test_that("F021 Kyle multi-outcome validation is structured", {
  panel <- read_fixture_csv("parity", "f021-multiple-outcomes", "inputs", "panel.csv")

  expect_error(
    did_imputation_kyle(panel, yname = "c(Y, missing_y)", gname = "Ei", tname = "t", idname = "unit"),
    regexp = "Missing required Kyle columns: missing_y",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "c(Y, Y)", gname = "Ei", tname = "t", idname = "unit"),
    regexp = "Kyle multi-outcome yname cannot contain duplicate outcomes.",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "c(Y, )", gname = "Ei", tname = "t", idname = "unit"),
    regexp = "Kyle multi-outcome yname must contain non-empty outcome names.",
    class = "didbjs_contract_error"
  )
})
