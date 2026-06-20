skip_if_not(fixtures_present())

f033_covariance <- function(covariance, scenario) {
  selected <- covariance[covariance$scenario == scenario, , drop = FALSE]
  terms <- unique(c(selected$row_term, selected$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(selected))) {
    mat[selected$row_term[[idx]], selected$col_term[[idx]]] <- selected$value[[idx]]
  }
  mat
}

f033_run <- function(panel, cluster) {
  did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    cluster = cluster,
    minn = 0
  )
}

test_that("F033 clustered covariance matches Stata across supported cluster structures", {
  panel <- read_fixture_csv("parity", "f033-cluster-rules", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f033-cluster-rules", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f033-cluster-rules", "expected", "stata", "covariance.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f033-cluster-rules", "expected", "stata", "probes.json"))
  clusters <- c(
    two = "cluster_two",
    alt = "cluster_alt",
    singleton = "cluster_singleton",
    nested = "cluster_nested",
    missing = "cluster_missing"
  )

  for (scenario in names(clusters)) {
    expected <- stata_estimates[stata_estimates$scenario == scenario, , drop = FALSE]
    expected_covariance <- f033_covariance(stata_covariance, scenario)
    expected_sample <- read_fixture_csv("parity", "f033-cluster-rules", "expected", "stata", paste0("sample-", scenario, ".csv"))
    result <- f033_run(panel, clusters[[scenario]])

    expect_equal(stata_probes[[scenario]]$status, "reference_success", info = scenario)
    expect_equal(result$diagnostics$cluster, clusters[[scenario]], info = scenario)
    expect_equal(result$estimates$term, expected$term, info = scenario)
    expect_lt(abs(result$estimates$estimate - expected$estimate), 1e-8)
    expect_lt(abs(result$estimates$std.error - expected$std_error), 1e-8)
    expect_equal(result$estimates$n_obs, expected$n_obs, info = scenario)
    expect_equal(result$estimates$n_control, expected$n_control, info = scenario)
    expect_equal(result$estimates$n_treated, expected$n_treated, info = scenario)
    observed_covariance <- result$covariance[rownames(expected_covariance), colnames(expected_covariance), drop = FALSE]
    expect_true(
      all(abs(observed_covariance - expected_covariance) < 1e-8),
      info = scenario
    )
    expect_equal(result$sample_mask$row_id, as.character(expected_sample$row_id), info = scenario)
    expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = scenario)
  }
})

test_that("F033 one-cluster and missing-cluster edges are structured", {
  panel <- read_fixture_csv("parity", "f033-cluster-rules", "inputs", "panel.csv")
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f033-cluster-rules", "expected", "stata", "probes.json"))

  expect_equal(stata_probes$one$status, "reference_error")
  expect_equal(stata_probes$one$return_code, 2001)
  expect_error(
    f033_run(panel, "cluster_one"),
    regexp = "Clustered covariance requires at least two clusters",
    class = "didbjs_contract_error"
  )

  missing_out <- f033_run(panel, "cluster_missing")
  expect_equal(missing_out$diagnostics$missing_excluded_row_ids, c("u8_t3", "u8_t4"))
  expect_false(missing_out$sample_mask$sample[missing_out$sample_mask$row_id == "u8_t3"])
  expect_false(missing_out$sample_mask$sample[missing_out$sample_mask$row_id == "u8_t4"])
  expect_true(missing_out$sample_mask$missing_required[missing_out$sample_mask$row_id == "u8_t3"])
  expect_true(missing_out$sample_mask$missing_required[missing_out$sample_mask$row_id == "u8_t4"])
})

test_that("F033 alternative cluster label ordering preserves Stata-equivalent output", {
  panel <- read_fixture_csv("parity", "f033-cluster-rules", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f033-cluster-rules", "expected", "stata", "estimates.csv")
  two <- f033_run(panel, "cluster_two")
  alt <- f033_run(panel, "cluster_alt")
  singleton <- f033_run(panel, "cluster_singleton")
  nested <- f033_run(panel, "cluster_nested")

  expect_equal(two$estimates$estimate, alt$estimates$estimate, tolerance = 1e-8)
  expect_lt(abs(two$estimates$std.error - alt$estimates$std.error), 1e-12)
  expect_true(all(abs(two$covariance - alt$covariance) < 1e-12))
  expect_lt(abs(
    stata_estimates$std_error[stata_estimates$scenario == "two"] -
      stata_estimates$std_error[stata_estimates$scenario == "alt"]
  ), 1e-12)
  expect_gt(singleton$estimates$std.error, two$estimates$std.error)
  expect_gt(nested$estimates$std.error, singleton$estimates$std.error)
})
