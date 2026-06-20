skip_if_not(fixtures_present())

f045_rows <- function(result, example, component = "effect") {
  estimates <- result$estimates
  data.frame(
    example = example,
    component = ifelse(grepl("^pre[0-9]+$", estimates$term), "pretrend", component),
    term = estimates$term,
    estimate = estimates$estimate,
    std_error = estimates$std.error,
    stringsAsFactors = FALSE
  )
}

f045_python_rows <- function(output, example) {
  effect_terms <- names(output$estimates)
  effect_rows <- data.frame(
    example = example,
    component = "effect",
    term = effect_terms,
    estimate = as.numeric(unlist(output$estimates, use.names = FALSE)),
    std_error = as.numeric(unlist(output$std_errors, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  pre_rows <- data.frame(
    example = character(),
    component = character(),
    term = character(),
    estimate = numeric(),
    std_error = numeric(),
    stringsAsFactors = FALSE
  )
  if (length(output$pretrends_estimates) > 0) {
    pre_terms <- names(output$pretrends_estimates)
    pre_rows <- data.frame(
      example = example,
      component = "pretrend",
      term = pre_terms,
      estimate = as.numeric(unlist(output$pretrends_estimates, use.names = FALSE)),
      std_error = as.numeric(unlist(output$pretrends_std_errors, use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  }
  rbind(effect_rows, pre_rows)
}

f045_compare_estimates <- function(actual, expected, estimate_tol = 1e-10, se_tol = 1e-8) {
  row.names(actual) <- NULL
  row.names(expected) <- NULL
  expect_equal(actual$term, expected$term)
  expect_equal(actual$estimate, expected$estimate, tolerance = estimate_tol)
  expect_equal(actual$std_error, expected$std_error, tolerance = se_tol)
}

test_that("F045 Stata five_estimators BJS excerpt replicates estimates and plot data", {
  source_inventory <- jsonlite::fromJSON(fixture_path("parity", "f045-examples", "metadata", "source-inventory.json"))
  panel <- read_fixture_csv("parity", "f045-examples", "inputs", "stata-five-bjs-panel.csv")
  stata <- read_fixture_csv("parity", "f045-examples", "expected", "stata", "estimates.csv")
  stata_cov <- read_covariance_matrix("parity", "f045-examples", "expected", "stata", "covariance.csv")
  stata_plot <- read_fixture_csv("parity", "f045-examples", "expected", "stata", "plot-data.csv")
  true_effects <- read_fixture_csv("parity", "f045-examples", "expected", "stata", "true-effects.csv")
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f045-examples", "expected", "stata", "diagnostics.json"))

  out <- did_imputation(panel, "Y", "i", "t", "Ei", allhorizons = TRUE, pretrends = 5)
  r_rows <- f045_rows(out, "stata_five_bjs")
  r_rows <- r_rows[match(stata$term, r_rows$term), , drop = FALSE]

  expect_equal(source_inventory$reference_commits$stata, "767c8d6670a751170910d419bbafd323df92ef08")
  expect_match(source_inventory$source_boundary$stata, "third-party estimators")
  expect_true(all(nzchar(unlist(source_inventory$source_sha256))))
  expect_equal(diagnostics$status, "success")
  expect_equal(diagnostics$source_example, "five_estimators_example.do BJS did_imputation/event_plot excerpt")
  expect_equal(diagnostics$terms, 11)
  expect_equal(nrow(panel), diagnostics$observations)
  expect_equal(true_effects$term, paste0("tau", 0:5))
  expect_true(all(is.finite(true_effects$true_effect)))
  expect_true(all(diff(true_effects$true_effect) > 0))
  expect_equal(true_effects$true_effect[true_effects$term == "tau5"], 2.5, tolerance = 1e-12)

  f045_compare_estimates(r_rows, data.frame(
    term = stata$term,
    estimate = stata$estimate,
    std_error = stata$std_error
  ), estimate_tol = 1e-8)
  expect_equal(out$covariance[rownames(stata_cov), colnames(stata_cov)], stata_cov, tolerance = 1e-8)

  plot_out <- event_plot(results_obj = out, plot_type = "rcap", noplot = TRUE)
  plot_rows <- plot_out$plot_data[match(stata_plot$event_time, plot_out$plot_data$event_time), , drop = FALSE]
  expect_equal(plot_rows$event_time, stata_plot$event_time)
  expect_equal(plot_rows$position, stata_plot$position, tolerance = 1e-8)
  expect_equal(plot_rows$estimate, stata_plot$estimate, tolerance = 1e-8)
  expect_equal(plot_rows$ci_low, stata_plot$ci_low, tolerance = 1e-8)
  expect_equal(plot_rows$ci_high, stata_plot$ci_high, tolerance = 1e-8)
})

test_that("F045 Python README examples run through the Python-compatible wrapper", {
  panel <- read_fixture_csv("parity", "f045-examples", "inputs", "stata-five-bjs-panel.csv")
  expected <- read_fixture_csv("parity", "f045-examples", "expected", "python", "estimates.csv")
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f045-examples", "expected", "python", "diagnostics.json"))
  schema <- jsonlite::fromJSON(fixture_path("parity", "f045-examples", "expected", "python", "output-schema.json"))

  static <- did_imputation_python(panel, "Y", "i", "t", "Ei")
  allhorizons <- did_imputation_python(panel, "Y", "i", "t", "Ei", allhorizons = TRUE)
  horizons_0_5 <- did_imputation_python(panel, "Y", "i", "t", "Ei", horizons = 0:4)
  sparse <- did_imputation_python(panel, "Y", "i", "t", "Ei", horizons = c(0, 1, 2, 5))
  pretrends <- did_imputation_python(panel, "Y", "i", "t", "Ei", allhorizons = TRUE, pretrends = 3)

  actual <- do.call(rbind, list(
    f045_python_rows(static, "readme_static"),
    f045_python_rows(allhorizons, "readme_allhorizons"),
    f045_python_rows(horizons_0_5, "readme_horizons_0_5"),
    f045_python_rows(sparse, "readme_sparse_horizons"),
    f045_python_rows(pretrends, "readme_pretrends_3")
  ))
  actual <- actual[match(paste(expected$example, expected$component, expected$term), paste(actual$example, actual$component, actual$term)), , drop = FALSE]

  expect_equal(diagnostics$status, "success")
  expect_true(all(unlist(diagnostics$readme_examples_inventory) != "missing"))
  expect_equal(schema$readme_static$estimate_terms, "tau_ate")
  expect_equal(schema$readme_allhorizons$estimate_terms, paste0("tau", 0:5))
  expect_equal(schema$readme_horizons_0_5$estimate_terms, paste0("tau", 0:4))
  expect_equal(schema$readme_sparse_horizons$estimate_terms, c("tau0", "tau1", "tau2", "tau5"))
  expect_equal(schema$readme_pretrends_3$pretrend_terms, paste0("pre", 1:3))
  expect_true(schema$readme_event_plot$saved)

  expect_equal(actual$example, expected$example)
  expect_equal(actual$component, expected$component)
  expect_equal(actual$term, expected$term)
  expect_equal(actual$estimate, expected$estimate, tolerance = 5e-7)
  expect_equal(actual$std_error, expected$std_error, tolerance = 1e-8)
})

test_that("F045 Kyle README/package examples match explicit supported calls", {
  panel <- read_fixture_csv("parity", "f045-examples", "inputs", "kyle-df-het.csv")
  expected <- read_fixture_csv("parity", "f045-examples", "expected", "kyle", "estimates.csv")
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f045-examples", "expected", "kyle", "diagnostics.json"))
  schema <- jsonlite::fromJSON(fixture_path("parity", "f045-examples", "expected", "kyle", "output-schema.json"))

  static <- did_imputation_kyle(
    data = panel,
    yname = "dep_var",
    gname = "g",
    tname = "year",
    idname = "unit"
  )
  event <- did_imputation_kyle(
    data = panel,
    yname = "dep_var",
    gname = "g",
    tname = "year",
    idname = "unit",
    horizon = 0:7,
    pretrends = -5:-1
  )
  actual <- rbind(
    data.frame(example = "readme_static", static, check.names = FALSE),
    data.frame(example = "readme_event_explicit_0_7_pre5", event, check.names = FALSE)
  )
  actual <- actual[match(paste(expected$example, expected$term), paste(actual$example, actual$term)), , drop = FALSE]

  expect_equal(diagnostics$status, "success")
  expect_equal(schema$readme_static$terms, "treat")
  expect_equal(schema$readme_event_explicit_0_7_pre5$terms, as.character(c(-5:-1, 0:7)))
  expect_equal(schema$readme_event_horizon_true$status, "success")
  expect_error(
    did_imputation_kyle(
      data = panel,
      yname = "dep_var",
      gname = "g",
      tname = "year",
      idname = "unit",
      horizon = TRUE,
      pretrends = -5:-1
    ),
    regexp = "Kyle horizon = TRUE all-horizon output is not implemented yet.",
    class = "didbjs_unsupported_error"
  )

  expect_equal(actual$term, expected$term)
  expect_equal(actual$estimate, expected$estimate, tolerance = 1e-8)
  expect_equal(actual$std.error, expected$std.error, tolerance = 1e-8)
  expect_equal(actual$conf.low, expected$conf.low, tolerance = 1e-8)
  expect_equal(actual$conf.high, expected$conf.high, tolerance = 1e-8)
})
