f049_contract <- function() {
  jsonlite::fromJSON(
    testthat::test_path("..", "snapshots", "f049-object-methods", "method-contract.json"),
    simplifyVector = TRUE
  )
}

f049_result <- function(save_artifacts = FALSE) {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  did_imputation(
    panel,
    y = "Y",
    i = "i",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "i",
    minn = 0,
    saveweights = save_artifacts,
    saveestimates = save_artifacts,
    saveresid = save_artifacts
  )
}

test_that("F049 didbjs object contract and S3 methods are stable", {
  contract <- f049_contract()
  result <- f049_result(save_artifacts = TRUE)

  expect_s3_class(result, "didbjs")
  expect_equal(attr(result, "didbjs_object_version"), contract$object_version)
  expect_named(result, contract$result_names)
  expect_named(result$estimates, contract$estimate_columns)
  expect_named(result$controls, contract$control_columns)
  expect_named(result$sample_mask, contract$sample_mask_columns)
  expect_s3_class(result$artifacts$weights, "didbjs_weights")
  expect_s3_class(result$artifacts$estimates, "didbjs_estimates")
  expect_s3_class(result$artifacts$residuals, "didbjs_residuals")

  printed <- capture.output(print(result))
  expect_equal(printed[[1]], contract$print_header)
  expect_true(any(grepl("term", printed)))
  expect_true(any(grepl("tau", printed)))

  summarized <- summary(result)
  expect_s3_class(summarized, "summary.didbjs")
  expect_named(summarized, contract$summary_names)
  summary_printed <- capture.output(print(summarized))
  expect_equal(summary_printed[[1]], contract$summary_header)
  expect_identical(summarized$estimates, result$estimates)

  expect_identical(tidy(result), result$estimates)
  expect_identical(as.data.frame(result), result$estimates)
  expect_equal(names(coef(result)), contract$coef_names)
  expect_equal(unname(coef(result)), result$estimates$estimate)
  expect_identical(vcov(result), result$covariance)

  glance_out <- glance(result)
  expect_named(glance_out, contract$glance_columns)
  expect_equal(glance_out$n_terms, nrow(result$estimates))
  expect_equal(glance_out$n_obs, sum(result$sample_mask$sample))
  expect_equal(glance_out$n_control, result$estimates$n_control[[1]])
  expect_equal(glance_out$n_treated, sum(result$estimates$n_treated))
  expect_equal(glance_out$n_controls, nrow(result$controls))
  expect_true(glance_out$has_artifacts)
  expect_false(glance_out$has_pretrends)
  expect_equal(glance_out$object_version, contract$object_version)
})

test_that("F049 serialization and versioned upgrades preserve method outputs", {
  contract <- f049_contract()
  result <- f049_result(save_artifacts = TRUE)
  serialized_path <- tempfile(fileext = ".rds")
  saveRDS(result, serialized_path)
  restored <- readRDS(serialized_path)

  expect_s3_class(restored, "didbjs")
  expect_equal(attr(restored, "didbjs_object_version"), contract$object_version)
  expect_identical(coef(restored), coef(result))
  expect_identical(vcov(restored), vcov(result))
  expect_identical(tidy(restored), tidy(result))
  expect_identical(glance(restored), glance(result))

  old <- result
  old$artifacts <- NULL
  attr(old, "didbjs_object_version") <- NULL
  upgraded <- upgrade_didbjs_object(old)

  expect_s3_class(upgraded, "didbjs")
  expect_equal(attr(upgraded, "didbjs_object_version"), contract$object_version)
  expect_type(upgraded$artifacts, "list")
  expect_equal(length(upgraded$artifacts), 0L)
  expect_identical(coef(upgraded), coef(result))
  expect_identical(vcov(upgraded), vcov(result))
  expect_identical(tidy(upgraded), result$estimates)

  malformed <- old
  malformed$covariance <- NULL
  expect_error(
    upgrade_didbjs_object(malformed),
    regexp = "missing fields: covariance",
    class = "didbjs_contract_error"
  )
})
