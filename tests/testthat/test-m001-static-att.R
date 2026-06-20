test_that("M001 static ATT surfaces use the F036 algebraic oracle as the point-estimate anchor", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  oracle <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "metadata", "f036-algebraic-oracle.json"))
  stata_estimates <- read_fixture_csv("smoke", "f001-static-att", "expected", "stata", "estimates.csv")
  kyle_alias <- read_fixture_csv("smoke", "f001-static-att", "expected", "kyle", "alias-estimates.csv")
  python_diag <- jsonlite::fromJSON(fixture_path("smoke", "f001-static-att", "expected", "python", "diagnostics.json"))

  native <- did_imputation(panel, y = "Y", i = "i", t = "t", Ei = "Ei", aw = "w", cluster = "i", minn = 0)
  python_compatible <- did_imputation_python(
    panel,
    y = "Y",
    i = "i",
    t = "t",
    Ei = "Ei",
    fe = c("i", "t"),
    aw = "w",
    minn = 0
  )
  kyle_compatible <- did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "i", wname = "w", cluster_var = "i")

  expect_equal(native$estimates$term, "tau")
  expect_equal(native$estimates$estimate, oracle$oracle$static_att, tolerance = 1e-12)
  expect_named(python_compatible$estimates, "tau_ate")
  expect_equal(unname(unlist(python_compatible$estimates)), oracle$oracle$static_att, tolerance = 1e-12)
  expect_equal(kyle_compatible$term, "treat")
  expect_equal(kyle_compatible$estimate, oracle$oracle$static_att, tolerance = 1e-12)
  expect_equal(stata_estimates$estimate[stata_estimates$term == "tau"], oracle$oracle$static_att, tolerance = 1e-10)
  expect_equal(kyle_alias$estimate[kyle_alias$term == "treat"], oracle$oracle$static_att, tolerance = 1e-10)
  expect_equal(python_diag$algebraic_static_att, oracle$oracle$static_att, tolerance = 1e-12)
  expect_false(python_diag$tol001_pass)
  expect_match(python_diag$root_cause_probe, "recover_fixed_effects_iterative", fixed = TRUE)
})
