skip_if_not(fixtures_present())

f031_covariance <- function(covariance, scenario) {
  selected <- covariance[covariance$scenario == scenario, , drop = FALSE]
  terms <- unique(c(selected$row_term, selected$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(selected))) {
    mat[selected$row_term[[idx]], selected$col_term[[idx]]] <- selected$value[[idx]]
  }
  mat
}

f031_run <- function(panel, scenario) {
  specs <- list(
    nested_fe = list(fe = c("group", "unit", "t"), controls = NULL, data = panel),
    duplicate_fe = list(fe = c("group", "group_dup", "t"), controls = NULL, data = panel),
    singleton_fe = list(fe = c("singleton_fe", "t"), controls = NULL, data = panel),
    disconnected_fe = list(fe = c("unit", "disc_time"), controls = NULL, data = panel[panel$disc_keep == 1, , drop = FALSE]),
    absorbed_control = list(fe = c("group", "t"), controls = "x_absorbed", data = panel),
    untreated_rank = list(fe = c("unit", "t"), controls = "x_treated_only", data = panel)
  )
  spec <- specs[[scenario]]
  did_imputation(
    data = spec$data,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = spec$fe,
    controls = spec$controls,
    aw = "w",
    cluster = "unit",
    minn = 0
  )
}

test_that("F031 rank fallback selects independent columns across dense and sparse paths", {
  rank_deficient <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 1, 2),
    j = c(1, 2, 3, 3, 4),
    x = 1,
    dims = c(4, 4)
  )

  expect_equal(independent_design_columns(rank_deficient), c(1L, 2L, 3L))

  duplicate_before_independent <- Matrix::sparseMatrix(
    i = c(1, 2, 1, 3),
    j = c(1, 2, 3, 4),
    x = 1,
    dims = c(4, 4)
  )
  dense_keep <- independent_dense_design_columns(duplicate_before_independent)
  sparse_keep <- independent_sparse_design_columns(duplicate_before_independent)

  expect_equal(independent_design_columns(duplicate_before_independent), dense_keep)
  expect_equal(length(sparse_keep), length(dense_keep))
  expect_equal(qr(as.matrix(duplicate_before_independent[, sparse_keep, drop = FALSE]))$rank, length(sparse_keep))
})

test_that("F031 FE rank pathologies match Stata for successful scenarios", {
  panel <- read_fixture_csv("parity", "f031-fe-rank", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", "covariance.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "python", "probes.json"))
  scenarios <- c("nested_fe", "duplicate_fe", "singleton_fe", "disconnected_fe")

  for (scenario in scenarios) {
    result <- f031_run(panel, scenario)
    expected <- stata_estimates[stata_estimates$scenario == scenario, , drop = FALSE]
    expected_covariance <- f031_covariance(stata_covariance, scenario)
    expected_sample <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", paste0("sample-mask-", scenario, ".csv"))

    expect_equal(stata_probes[[scenario]]$status, "reference_success", info = scenario)
    expect_equal(python_probes[[scenario]]$status, "reference_success", info = scenario)
    expect_equal(result$estimates$term, expected$term, info = scenario)
    expect_equal(result$estimates$estimate, expected$estimate, tolerance = 1e-7, info = scenario)
    expect_equal(result$estimates$std.error, expected$std_error, tolerance = 1e-6, info = scenario)
    expect_equal(result$estimates$n_obs, expected$n_obs, info = scenario)
    expect_equal(result$estimates$n_control, expected$n_control, info = scenario)
    expect_equal(result$estimates$n_treated, expected$n_treated, info = scenario)
    expect_equal(
      result$covariance[rownames(expected_covariance), colnames(expected_covariance), drop = FALSE],
      expected_covariance,
      tolerance = 1e-6,
      info = scenario
    )
    if (scenario == "disconnected_fe") {
      matched_sample <- expected_sample[expected_sample$row_id %in% result$sample_mask$row_id, , drop = FALSE]
      dropped_sample <- expected_sample[!(expected_sample$row_id %in% result$sample_mask$row_id), , drop = FALSE]
      expect_equal(result$sample_mask$row_id, as.character(matched_sample$row_id), info = scenario)
      expect_true(all(result$sample_mask$sample == as.logical(matched_sample$sample)), info = scenario)
      expect_true(all(dropped_sample$sample == 0), info = scenario)
    } else {
      expect_equal(result$sample_mask$row_id, as.character(expected_sample$row_id), info = scenario)
      expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = scenario)
    }
  }
})

test_that("F031 absorbed controls are an approved fail-closed divergence", {
  panel <- read_fixture_csv("parity", "f031-fe-rank", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", "covariance.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "python", "probes.json"))

  expect_equal(stata_probes$absorbed_control$status, "reference_success")
  expect_equal(python_probes$absorbed_control$status, "reference_error")
  omitted <- stata_estimates[stata_estimates$scenario == "absorbed_control" & stata_estimates$term == "x_absorbed", , drop = FALSE]
  expect_equal(omitted$estimate, 0)
  expect_equal(omitted$std_error, 0)
  absorbed_covariance <- f031_covariance(stata_covariance, "absorbed_control")
  expect_equal(absorbed_covariance["x_absorbed", "x_absorbed"], 0)
  expect_equal(absorbed_covariance["tau", "x_absorbed"], 0)
  expect_equal(absorbed_covariance["x_absorbed", "tau"], 0)

  expect_error(
    f031_run(panel, "absorbed_control"),
    regexp = "controls are collinear in the D==0 subsample",
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_python(
      df = panel,
      y = "Y",
      i = "unit",
      t = "t",
      Ei = "Ei",
      fe = c("group", "t"),
      controls = "x_absorbed",
      aw = "w",
      minn = 0
    ),
    regexp = "controls are collinear in the D==0 subsample",
    class = "didbjs_contract_error"
  )
})

test_that("F031 untreated-sample rank failure is structured across references", {
  panel <- read_fixture_csv("parity", "f031-fe-rank", "inputs", "panel.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "stata", "probes.json"))
  python_probes <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "python", "probes.json"))

  expect_equal(stata_probes$untreated_rank$status, "reference_error")
  expect_equal(python_probes$untreated_rank$status, "reference_error")
  expect_error(
    f031_run(panel, "untreated_rank"),
    regexp = "controls are collinear in the D==0 subsample",
    class = "didbjs_contract_error"
  )
})

test_that("F031 Python-compatible wrapper preserves rank-resolved object shape", {
  panel <- read_fixture_csv("parity", "f031-fe-rank", "inputs", "panel.csv")
  schema <- jsonlite::fromJSON(fixture_path("parity", "f031-fe-rank", "expected", "python", "object-schema.json"))
  stata_estimates <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f031-fe-rank", "expected", "stata", "covariance.csv")

  out <- did_imputation_python(
    df = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    fe = c("group", "group_dup", "t"),
    aw = "w",
    minn = 0
  )

  schema_fields <- names(schema$duplicate_fe$fields)
  expect_true(inherits(out, "DIDImputationOutput"))
  expect_named(out, schema_fields)
  for (field in schema_fields) {
    expect_identical(is.null(out[[field]]), schema$duplicate_fe$fields[[field]]$is_null, info = field)
  }
  expected <- stata_estimates[stata_estimates$scenario == "duplicate_fe", , drop = FALSE]
  expected_covariance <- f031_covariance(stata_covariance, "duplicate_fe")
  expect_named(out$estimates, "tau_ate")
  expect_equal(out$estimates$tau_ate, expected$estimate, tolerance = 1e-7)
  expect_equal(out$std_errors$tau_ate, expected$std_error, tolerance = 1e-6)
  expect_equal(out$V, expected_covariance["tau", "tau"], tolerance = 1e-8)
})
