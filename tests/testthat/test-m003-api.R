test_that("R-native API returns stable S3 shape", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  result <- did_imputation(panel, y = "Y", i = "i", t = "t", Ei = "Ei", aw = "w", cluster = "i", minn = 0)

  expect_named(result, c("estimates", "controls", "covariance", "sample_mask", "artifacts", "diagnostics", "call"))
  expect_type(result$artifacts, "list")
  expect_s3_class(tidy(result), "data.frame")
  expect_named(
    tidy(result),
    c("term", "estimate", "std.error", "conf.low", "conf.high", "n_obs", "n_control", "n_treated")
  )
  expect_named(result$controls, c("term", "estimate", "std.error", "conf.low", "conf.high"))
})
