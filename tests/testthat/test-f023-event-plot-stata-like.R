skip_if_not(fixtures_present())

test_that("F023 Stata-like event_plot extraction matches savecoef coordinates", {
  payload <- jsonlite::fromJSON(
    fixture_path("parity", "f023-event-plot-stata-like", "inputs", "models.json"),
    simplifyVector = FALSE
  )
  stata <- read_fixture_csv("parity", "f023-event-plot-stata-like", "expected", "stata", "plot-data.csv")
  diag <- jsonlite::fromJSON(fixture_path("parity", "f023-event-plot-stata-like", "expected", "stata", "diagnostics.json"))
  model_names <- vapply(payload$models, `[[`, character(1), "name")

  out <- event_plot(
    models = payload$models,
    stub_lag = unlist(payload$stub_lag),
    stub_lead = unlist(payload$stub_lead),
    trimlag = as.numeric(unlist(payload$trimlag)),
    trimlead = as.numeric(unlist(payload$trimlead)),
    shift = as.numeric(unlist(payload$shift)),
    perturb = as.numeric(unlist(payload$perturb)),
    model_names = model_names,
    significance_level = payload$significance_level,
    plot_type = "rcap"
  )

  expect_equal(diag$status, "success")
  expect_equal(diag$models, 2)
  expect_equal(diag$saved_rows, 8)
  expect_true(diag$savecoef)
  expect_true(diag$noplot)
  expect_true(diag$coef_sentinel)
  expect_equal(diag$eclass_seed, "regress coef")
  expect_s3_class(out, "didbjs_event_plot")
  expect_s3_class(out$plot, "ggplot")

  expect_equal(out$plot_data$model, stata$model)
  expect_equal(out$plot_data$model_label, rep(model_names, each = 4))
  expect_equal(out$plot_data$event_time, stata$event_time)
  expect_equal(out$plot_data$position, stata$position, tolerance = 1e-8)
  expect_equal(out$plot_data$estimate, stata$estimate, tolerance = 1e-8)
  expect_equal(out$plot_data$ci_low, stata$ci_low, tolerance = 1e-8)
  expect_equal(out$plot_data$ci_high, stata$ci_high, tolerance = 1e-8)
  expect_equal(out$plot_data$series, rep(c("Pre-trends", "Pre-trends", "Effects", "Effects"), 2))
  expect_false(any(out$plot_data$term %in% c("m1pre3", "m1tau2", "lead_3", "lag_2")))
  expect_equal(out$plot_data$term, c("m1pre2", "m1pre1", "m1tau0", "m1tau1", "lead_2", "lead_1", "lag_0", "lag_1"))
  expect_equal(out$plot_data$position - out$plot_data$event_time, c(rep(0, 4), rep(-0.75, 4)), tolerance = 1e-10)
})

test_that("F023 Stata-like event_plot supports rarea and structured stub validation", {
  payload <- jsonlite::fromJSON(
    fixture_path("parity", "f023-event-plot-stata-like", "inputs", "models.json"),
    simplifyVector = FALSE
  )
  model_names <- vapply(payload$models, `[[`, character(1), "name")

  rarea <- event_plot(
    models = payload$models,
    stub_lag = unlist(payload$stub_lag),
    stub_lead = unlist(payload$stub_lead),
    trimlag = as.numeric(unlist(payload$trimlag)),
    trimlead = as.numeric(unlist(payload$trimlead)),
    shift = as.numeric(unlist(payload$shift)),
    perturb = as.numeric(unlist(payload$perturb)),
    model_names = model_names,
    significance_level = payload$significance_level,
    plot_type = "rarea"
  )
  expect_s3_class(rarea$plot, "ggplot")
  expect_equal(rarea$plot_data$plot_type, rep("rarea", 8))
  expect_equal(rarea$plot_data$position, c(-2, -1, 0, 1, -2.75, -1.75, -0.75, 0.25))

  expect_error(
    event_plot(models = payload$models, stub_lag = "tau", stub_lead = unlist(payload$stub_lead)),
    regexp = "stub_lag for model 1 must contain exactly one # placeholder.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(models = payload$models, stub_lag = c("m1tau#", "lag_#"), stub_lead = c("m1tau#", "lead_#")),
    regexp = "stub_lag and stub_lead have to be different for model 1.",
    class = "didbjs_contract_error"
  )
})
