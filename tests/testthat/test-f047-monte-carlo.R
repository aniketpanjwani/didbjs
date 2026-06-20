skip_if_not(fixtures_present())

f047_fixture <- function(...) {
  fixture_path("parity", "f047-monte-carlo", ...)
}

f047_make_panel <- function(seed, truth, dgp) {
  set.seed(seed)
  n_units <- dgp$panel$n_units
  n_periods <- dgp$panel$n_periods
  treated_units <- dgp$panel$treated_units
  treat_time <- dgp$panel$treat_time
  unit <- rep(seq_len(n_units), each = n_periods)
  time <- rep(seq_len(n_periods), times = n_units)
  Ei_unit <- c(rep(treat_time, treated_units), rep(NA_integer_, n_units - treated_units))
  Ei <- Ei_unit[unit]
  unit_fe <- stats::rnorm(n_units, mean = 0, sd = dgp$parameters$unit_fe_sd)
  time_fe <- stats::rnorm(n_periods, mean = 0, sd = dgp$parameters$time_fe_sd)
  epsilon <- stats::rnorm(length(unit), mean = 0, sd = dgp$parameters$epsilon_sd)
  treated <- !is.na(Ei) & time >= Ei
  data.frame(
    row_id = seq_along(unit),
    unit = unit,
    t = time,
    Ei = Ei,
    Y = unit_fe[unit] + time_fe[time] + epsilon + truth * treated,
    w = dgp$parameters$analytic_weight,
    stringsAsFactors = FALSE
  )
}

f047_run_design <- function(replications, dgp, truth) {
  estimates <- numeric(nrow(replications))
  std_errors <- numeric(nrow(replications))
  for (idx in seq_len(nrow(replications))) {
    panel <- f047_make_panel(replications$seed[[idx]], truth = truth, dgp = dgp)
    out <- did_imputation(
      data = panel,
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      aw = "w",
      cluster = "unit",
      minn = 0
    )
    estimates[[idx]] <- out$estimates$estimate[[1]]
    std_errors[[idx]] <- out$estimates$std.error[[1]]
  }
  lower <- estimates - 1.96 * std_errors
  upper <- estimates + 1.96 * std_errors
  data.frame(
    replications = nrow(replications),
    truth = truth,
    mean_estimate = mean(estimates),
    bias = mean(estimates) - truth,
    coverage = mean(lower <= truth & upper >= truth),
    rejection_rate = mean(abs(estimates / std_errors) > 1.96),
    mean_std_error = mean(std_errors),
    estimate_sd = stats::sd(estimates),
    stringsAsFactors = FALSE
  )
}

test_that("F047 Monte Carlo fixture freezes seeds, DGP, bands, and execution plan", {
  replications <- read_fixture_csv("parity", "f047-monte-carlo", "metadata", "replications.csv")
  dgp <- jsonlite::fromJSON(f047_fixture("metadata", "dgp.json"))
  bands <- jsonlite::fromJSON(f047_fixture("metadata", "bands.json"))
  execution_plan <- jsonlite::fromJSON(f047_fixture("metadata", "execution-plan.json"))
  manifest <- jsonlite::fromJSON(f047_fixture("metadata", "manifest.json"))

  expect_equal(bands$tolerance_id, "TOL007")
  expect_equal(bands$replications_per_design, 1000)
  expect_equal(table(replications$design)[["known_effect"]], 1000)
  expect_equal(table(replications$design)[["zero_effect"]], 1000)
  expect_equal(sort(unique(replications$seed)), 47001:48000)
  expect_equal(dgp$panel$rows_per_replication, dgp$panel$n_units * dgp$panel$n_periods)
  expect_equal(dgp$parameters$known_effect_truth, bands$known_effect$truth)
  expect_equal(dgp$parameters$zero_effect_truth, bands$zero_effect$truth)
  expect_match(execution_plan$default_test, "offline", fixed = TRUE)
  expect_true("Stata" %in% execution_plan$no_external_dependencies)
  expect_true("Python" %in% execution_plan$no_external_dependencies)
  expect_true("internet" %in% execution_plan$no_external_dependencies)
  expect_equal(manifest$replications_per_design, 1000)
  expect_true("D015" %in% manifest$decision_record_ids)
})

test_that("F047 Monte Carlo recovers known effect within frozen bias and coverage bands", {
  replications <- read_fixture_csv("parity", "f047-monte-carlo", "metadata", "replications.csv")
  dgp <- jsonlite::fromJSON(f047_fixture("metadata", "dgp.json"))
  bands <- jsonlite::fromJSON(f047_fixture("metadata", "bands.json"))
  design <- replications[replications$design == "known_effect", , drop = FALSE]
  summary <- f047_run_design(design, dgp, truth = bands$known_effect$truth)

  expect_equal(summary$replications, bands$replications_per_design)
  expect_lt(abs(summary$bias), bands$known_effect$abs_bias_max)
  expect_gte(summary$coverage, bands$known_effect$coverage_min)
  expect_lte(summary$coverage, bands$known_effect$coverage_max)
  expect_gt(summary$mean_std_error, 0)
  expect_gt(summary$estimate_sd, 0)
})

test_that("F047 Monte Carlo zero-effect DGP controls false rejection rate", {
  replications <- read_fixture_csv("parity", "f047-monte-carlo", "metadata", "replications.csv")
  dgp <- jsonlite::fromJSON(f047_fixture("metadata", "dgp.json"))
  bands <- jsonlite::fromJSON(f047_fixture("metadata", "bands.json"))
  design <- replications[replications$design == "zero_effect", , drop = FALSE]
  summary <- f047_run_design(design, dgp, truth = bands$zero_effect$truth)

  expect_equal(summary$replications, bands$replications_per_design)
  expect_lt(abs(summary$bias), bands$zero_effect$abs_bias_max)
  expect_gte(summary$coverage, bands$zero_effect$coverage_min)
  expect_lte(summary$coverage, bands$zero_effect$coverage_max)
  expect_gte(summary$rejection_rate, bands$zero_effect$rejection_min)
  expect_lte(summary$rejection_rate, bands$zero_effect$rejection_max)
  expect_gt(summary$mean_std_error, 0)
  expect_gt(summary$estimate_sd, 0)
})
