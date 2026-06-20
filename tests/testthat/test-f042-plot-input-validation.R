skip_if_not(fixtures_present())

test_that("F042 records pinned Python event_plot validation behavior", {
  probes <- jsonlite::fromJSON(
    fixture_path("parity", "f042-plot-validation", "expected", "python", "probes.json"),
    simplifyVector = FALSE
  )
  diagnostics <- jsonlite::fromJSON(
    fixture_path("parity", "f042-plot-validation", "expected", "python", "diagnostics.json")
  )

  expect_equal(diagnostics$status, "success")
  expect_equal(diagnostics$probe_count, 10)
  expect_equal(probes$missing_pretrend_se$status, "error")
  expect_equal(probes$missing_pretrend_se$error_class, "KeyError")
  expect_equal(probes$bad_term_name$error_class, "ValueError")
  expect_equal(probes$unsupported_kwarg$error_class, "AttributeError")
  expect_equal(probes$object_plus_manual$error_class, "AttributeError")
  expect_equal(probes$extra_effect_se$status, "success")
  expect_equal(probes$std_without_estimates$status, "success")
  expect_equal(probes$nan_estimate$status, "success")
  expect_equal(probes$inf_std_error$status, "success")
  expect_equal(probes$alpha_zero$status, "success")
  expect_equal(probes$alpha_one$status, "success")
})

test_that("F042 manual event_plot inputs fail closed with structured errors", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f042-plot-validation", "inputs", "manual.json"))

  expect_error(
    event_plot(
      pretrends = payload$pretrends,
      pretrends_std = c(pre1 = payload$pretrends_std$pre1),
      effects = payload$effects,
      effects_std = payload$effects_std,
      noplot = TRUE
    ),
    regexp = "pretrends_std is missing terms: pre2",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(
      pretrends = payload$pretrends,
      pretrends_std = payload$pretrends_std,
      effects = payload$effects,
      effects_std = c(payload$effects_std, tau9 = 0.9),
      noplot = TRUE
    ),
    regexp = "effects_std contains unknown terms: tau9",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(
      pretrends_std = c(pre1 = payload$pretrends_std$pre1),
      effects = payload$effects,
      effects_std = payload$effects_std,
      noplot = TRUE
    ),
    regexp = "pretrends_std contains unknown terms: pre1",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(
      pretrends = payload$pretrends,
      pretrends_std = payload$pretrends_std,
      effects = c(beta0 = 1),
      effects_std = c(beta0 = 0.1),
      noplot = TRUE
    ),
    regexp = "effects terms must match tau#: beta0",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(pretrends = c(pre1 = -0.1, pre1 = 0.2), effects = payload$effects, noplot = TRUE),
    regexp = "pretrends cannot contain duplicate terms.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(effects = c(tau0 = NA_real_), effects_std = c(tau0 = 0.1), noplot = TRUE),
    regexp = "effects must contain finite values.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(effects = c(tau0 = 1), effects_std = c(tau0 = Inf), noplot = TRUE),
    regexp = "effects_std must contain finite values.",
    class = "didbjs_contract_error"
  )
})

test_that("F042 alpha bounds and unsupported manual arguments are structured", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f042-plot-validation", "inputs", "manual.json"))

  expect_error(
    event_plot(effects = payload$effects, significance_level = 0, noplot = TRUE),
    regexp = "significance_level must be a finite number between 0 and 1.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(effects = payload$effects, significance_level = 1, noplot = TRUE),
    regexp = "significance_level must be a finite number between 0 and 1.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(effects = payload$effects, significance_level = NA_real_, noplot = TRUE),
    regexp = "significance_level must be a finite number between 0 and 1.",
    class = "didbjs_contract_error"
  )
  expect_error(
    event_plot(effects = payload$effects, definitely_not_supported_by_matplotlib = 1, noplot = TRUE),
    regexp = "Unsupported event_plot arguments: definitely_not_supported_by_matplotlib",
    class = "didbjs_unsupported_error"
  )
  expect_error(
    event_plot(
      results_obj = list(
        pretrends_estimates = payload$pretrends,
        pretrends_std_errors = payload$pretrends_std,
        estimates = payload$effects,
        std_errors = payload$effects_std
      ),
      effects = payload$effects,
      noplot = TRUE
    ),
    regexp = "results_obj cannot be combined with manual pretrends/effects inputs.",
    class = "didbjs_contract_error"
  )
})
