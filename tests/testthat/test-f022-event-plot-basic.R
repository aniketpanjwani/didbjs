skip_if_not(fixtures_present())

compare_plot_data <- function(actual, expected, tolerance = 1e-8) {
  row.names(actual) <- NULL
  row.names(expected) <- NULL
  expect_equal(actual$source, expected$source)
  expect_equal(actual$plot_type, expected$plot_type)
  expect_equal(actual$together, expected$together)
  expect_equal(actual$series, expected$series)
  expect_equal(actual$term, expected$term)
  expect_equal(actual$event_time, expected$event_time)
  expect_equal(actual$estimate, expected$estimate, tolerance = tolerance)
  expect_equal(actual$std_error, expected$std_error, tolerance = tolerance)
  expect_equal(actual$critical_value, expected$critical_value, tolerance = tolerance)
  expect_equal(actual$ci_low, expected$ci_low, tolerance = tolerance)
  expect_equal(actual$ci_high, expected$ci_high, tolerance = tolerance)
  expect_equal(actual$has_ci, expected$has_ci)
}

test_that("F022 event_plot manual inputs match Python plot data and save output", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f022-event-plot-basic", "inputs", "manual.json"))
  python <- read_fixture_csv("parity", "f022-event-plot-basic", "expected", "python", "plot-data.csv")
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f022-event-plot-basic", "expected", "python", "diagnostics.json"))
  python_schema <- jsonlite::fromJSON(fixture_path("parity", "f022-event-plot-basic", "expected", "python", "output-schema.json"))

  save_path <- tempfile(fileext = ".png")
  rcap <- event_plot(
    pretrends = payload$pretrends,
    pretrends_std = payload$pretrends_std,
    effects = payload$effects,
    effects_std = payload$effects_std,
    significance_level = payload$significance_level,
    plot_type = "rcap",
    save_path = save_path,
    dpi = 72
  )
  rarea <- event_plot(
    pretrends = payload$pretrends,
    pretrends_std = payload$pretrends_std,
    effects = payload$effects,
    effects_std = payload$effects_std,
    significance_level = payload$significance_level,
    plot_type = "rarea"
  )

  expect_equal(python_diag$status, "success")
  expect_true(python_diag$manual_rcap_saved)
  expect_true(python_diag$manual_rarea_saved)
  expect_equal(python_schema$figure_class, "Figure")
  expect_s3_class(rcap, "didbjs_event_plot")
  expect_s3_class(rcap$plot, "ggplot")
  expect_s3_class(rarea$plot, "ggplot")
  expect_true(file.exists(save_path))
  expect_gt(file.info(save_path)$size, 0)

  compare_plot_data(
    rcap$plot_data,
    python[python$source == "manual" & python$plot_type == "rcap", , drop = FALSE]
  )
  compare_plot_data(
    rarea$plot_data,
    python[python$source == "manual" & python$plot_type == "rarea", , drop = FALSE]
  )
})

test_that("F022 event_plot object input and together mode match Python plot data", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f022-event-plot-basic", "inputs", "manual.json"))
  python <- read_fixture_csv("parity", "f022-event-plot-basic", "expected", "python", "plot-data.csv")
  object_payload <- structure(
    list(
      pretrends_estimates = payload$pretrends,
      pretrends_std_errors = payload$pretrends_std,
      estimates = payload$effects,
      std_errors = payload$effects_std
    ),
    class = c("DIDImputationOutput", "didbjs_python")
  )

  separate <- event_plot(results_obj = object_payload, significance_level = payload$significance_level, plot_type = "rcap")
  together <- event_plot(results_obj = object_payload, significance_level = payload$significance_level, plot_type = "rcap", together = TRUE)

  expect_s3_class(separate$plot, "ggplot")
  expect_s3_class(together$plot, "ggplot")
  compare_plot_data(
    separate$plot_data,
    python[python$source == "object" & python$plot_type == "rcap" & python$together == FALSE, , drop = FALSE]
  )
  compare_plot_data(
    together$plot_data,
    python[python$source == "object" & python$plot_type == "rcap" & python$together == TRUE, , drop = FALSE]
  )
})

test_that("F022 Stata savecoef coordinates match the shared CI oracle", {
  python <- read_fixture_csv("parity", "f022-event-plot-basic", "expected", "python", "plot-data.csv")
  stata <- read_fixture_csv("parity", "f022-event-plot-basic", "expected", "stata", "plot-data.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f022-event-plot-basic", "expected", "stata", "diagnostics.json"))
  manual_rcap <- python[python$source == "manual" & python$plot_type == "rcap", , drop = FALSE]

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$stata_version, "14.2")
  expect_true(stata_diag$savecoef)
  expect_true(stata_diag$noplot)
  expect_true(stata_diag$coef_sentinel)
  expect_equal(stata_diag$eclass_seed, "regress coef")
  expect_equal(stata$model, rep(1, nrow(manual_rcap)))
  expect_equal(stata$event_time, manual_rcap$event_time)
  expect_equal(stata$position, manual_rcap$event_time)
  expect_equal(stata$estimate, manual_rcap$estimate, tolerance = 1e-8)
  expect_equal(stata$ci_low, manual_rcap$ci_low, tolerance = 1e-8)
  expect_equal(stata$ci_high, manual_rcap$ci_high, tolerance = 1e-8)
})

test_that("F022 event_plot basic validation is structured", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f022-event-plot-basic", "inputs", "manual.json"))

  expect_error(
    event_plot(pretrends = payload$pretrends, effects = payload$effects, plot_type = "line"),
    regexp = "plot_type must be 'rcap' or 'rarea'.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(pretrends = payload$pretrends, pretrends_std = c(pre1 = 0.1), effects = payload$effects),
    regexp = "pretrends_std is missing terms: pre2",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(pretrends = c(pre1 = 0.1, pre1 = 0.2), effects = payload$effects),
    regexp = "pretrends cannot contain duplicate terms.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(pretrends = payload$pretrends, effects = payload$effects, save_path = tempfile(), noplot = TRUE),
    regexp = "save_path cannot be combined with noplot = TRUE.",
    class = "didbjs_contract_error"
  )
})
