test_that("Kyle-compatible F001 static call accepts idname i", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  kyle_alias <- read_fixture_csv("smoke", "f001-static-att", "expected", "kyle", "alias-estimates.csv")
  kyle_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "kyle", "diagnostics.json"))

  out <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "i",
    wname = "w",
    cluster_var = "i"
  )

  expect_s3_class(out, "data.table")
  expect_named(out, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_equal(out$term, kyle_alias$term)
  expect_equal(out$estimate, kyle_alias$estimate, tolerance = 1e-10)
  expect_equal(out$std.error, kyle_alias$std.error, tolerance = 1e-8)
  expect_equal(out$conf.low, kyle_alias$conf.low, tolerance = 1e-8)
  expect_equal(out$conf.high, kyle_alias$conf.high, tolerance = 1e-8)
  expect_equal(kyle_diag$status, "reference_error")
  expect_equal(kyle_diag$alias_probe_status, "success")
})

test_that("Kyle-compatible wrapper keeps unsupported Kyle surfaces explicit", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "i", horizon = TRUE),
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "i", first_stage = ~ log(t) | i + t),
    class = "didbjs_unsupported_error"
  )
})
