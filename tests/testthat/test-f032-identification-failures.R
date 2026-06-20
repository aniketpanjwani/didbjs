skip_if_not(fixtures_present())

f032_panel <- function(scenario) {
  panel <- read_fixture_csv("parity", "f032-identification-failures", "inputs", "panel.csv")
  panel[panel$scenario == scenario, , drop = FALSE]
}

f032_run <- function(scenario, ...) {
  did_imputation(
    data = f032_panel(scenario),
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("unit", "t"),
    cluster = "unit",
    minn = 0,
    ...
  )
}

test_that("F032 no untreated and no treated observations fail closed", {
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f032-identification-failures", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f032-identification-failures", "expected", "python", "probes.json"))
  stata_estimates <- read_fixture_csv("parity", "f032-identification-failures", "expected", "stata", "estimates.csv")

  expect_equal(stata_probes$no_untreated$status, "reference_error")
  expect_equal(stata_probes$no_untreated$return_code, 459)
  expect_equal(python_probes$no_untreated$status, "reference_error")
  expect_error(
    f032_run("no_untreated"),
    regexp = "No untreated observations are available",
    class = "didbjs_contract_error"
  )

  expect_equal(stata_probes$no_treated$status, "reference_success")
  expect_equal(python_probes$no_treated$status, "reference_error")
  omitted <- stata_estimates[stata_estimates$scenario == "no_treated", , drop = FALSE]
  expect_equal(omitted$term, "tau")
  expect_equal(omitted$estimate, 0)
  expect_equal(omitted$std_error, 0)
  expect_equal(omitted$n_treated, 0)
  expect_error(
    f032_run("no_treated"),
    regexp = "No treated observations are available",
    class = "didbjs_contract_error"
  )
})

test_that("F032 non-imputable static designs fail with row-level diagnostics", {
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f032-identification-failures", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f032-identification-failures", "expected", "python", "probes.json"))
  cannot_files <- c(one_cohort = "cannot-one.csv", all_post_treated = "cannot-all-post.csv")

  for (scenario in c("one_cohort", "all_post_treated")) {
    cannot <- read_fixture_csv("parity", "f032-identification-failures", "expected", "stata", cannot_files[[scenario]])
    failed <- cannot$row_id[cannot$cannot_impute == 1]
    expect_equal(stata_probes[[scenario]]$status, "reference_error", info = scenario)
    expect_equal(stata_probes[[scenario]]$return_code, 198, info = scenario)
    expect_equal(stata_probes[[scenario]]$cannot_impute_count, 8, info = scenario)
    expect_equal(python_probes[[scenario]]$status, "reference_error", info = scenario)
    expect_length(failed, 8)
    expect_error(
      f032_run(scenario),
      regexp = paste(failed, collapse = ", "),
      class = "didbjs_contract_error",
      info = scenario
    )
  }
})

test_that("F032 unsupported requested horizon fails despite omitted-zero references", {
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f032-identification-failures", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f032-identification-failures", "expected", "python", "probes.json"))
  stata_estimates <- read_fixture_csv("parity", "f032-identification-failures", "expected", "stata", "estimates.csv")
  python_estimates <- read_fixture_csv("parity", "f032-identification-failures", "expected", "python", "estimates.csv")

  expect_equal(stata_probes$no_supported_horizon$status, "reference_success")
  expect_equal(python_probes$no_supported_horizon$status, "reference_success")
  stata_omitted <- stata_estimates[stata_estimates$scenario == "no_supported_horizon", , drop = FALSE]
  python_zero <- python_estimates[python_estimates$scenario == "no_supported_horizon", , drop = FALSE]
  expect_equal(stata_omitted$term, "tau1")
  expect_equal(stata_omitted$estimate, 0)
  expect_equal(stata_omitted$n_treated, 0)
  expect_equal(python_zero$term, "tau1")
  expect_equal(python_zero$estimate, 0)

  expect_error(
    f032_run("no_supported_horizon", horizons = 1),
    regexp = "Horizon 1 has zero treated weight",
    class = "didbjs_contract_error"
  )
})

test_that("F032 Python-compatible wrapper preserves fail-closed identification surface", {
  for (scenario in c("no_untreated", "no_treated", "one_cohort", "all_post_treated")) {
    expected_message <- switch(
      scenario,
      no_untreated = "No untreated observations are available",
      no_treated = "No treated observations are available",
      "Python-compatible static ATT cannot be identified after autosample"
    )
    expect_error(
      did_imputation_python(
        df = f032_panel(scenario),
        y = "Y",
        i = "unit",
        t = "t",
        Ei = "Ei",
        fe = c("unit", "t"),
        cluster = "unit",
        minn = 0
      ),
      regexp = expected_message,
      class = "didbjs_contract_error",
      info = scenario
    )
  }

  expect_error(
    did_imputation_python(
      df = f032_panel("no_supported_horizon"),
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      fe = c("unit", "t"),
      cluster = "unit",
      minn = 0,
      horizons = 1
    ),
    regexp = "Horizon 1 has zero treated weight",
    class = "didbjs_contract_error"
  )
})
