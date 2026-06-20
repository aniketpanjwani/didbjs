skip_if_not(fixtures_present())

f039_run <- function(panel, y = "Y", saveweights = FALSE, loadweights = NULL, saveestimates = FALSE, saveresid = FALSE) {
  did_imputation(
    panel,
    y = y,
    i = "unit",
    t = "t",
    Ei = "Ei",
    aw = "w",
    cluster = "unit",
    minn = 0,
    horizons = 0:2,
    saveweights = saveweights,
    loadweights = loadweights,
    saveestimates = saveestimates,
    saveresid = saveresid
  )
}

f039_keyed <- function(x) {
  x[order(x$row_id, x$term), , drop = FALSE]
}

f039_expected_weights <- function(weights, scenario) {
  out <- weights[weights$scenario == scenario, c("row_id", "term", "weight"), drop = FALSE]
  out$row_id <- as.character(out$row_id)
  f039_keyed(out)
}

f039_expect_weight_match <- function(actual, expected, tolerance = 1e-8) {
  actual <- f039_keyed(actual[, c("row_id", "term", "weight"), drop = FALSE])
  expected <- f039_keyed(expected[, c("row_id", "term", "weight"), drop = FALSE])
  expect_equal(actual$row_id, expected$row_id)
  expect_equal(actual$term, expected$term)
  expect_equal(actual$weight, expected$weight, tolerance = tolerance)
}

test_that("F039 dense and sparse saved weights preserve original row ids against Stata", {
  base <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "base.csv")
  reordered <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "reordered.csv")
  stata_estimates <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "stata", "estimates.csv")
  stata_dense <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "stata", "weights-dense.csv")
  stata_sparse <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "stata", "weights-sparse.csv")
  stata_base_sample <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "stata", "sample-base.csv")
  stata_reordered_sample <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "stata", "sample-reordered.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f039-artifact-row-mapping", "expected", "stata", "diagnostics.json"))

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_diag$dense_row_count, 360)
  expect_equal(stata_diag$sparse_row_count, 240)

  base_result <- f039_run(base, saveweights = TRUE)
  reordered_result <- f039_run(reordered, saveweights = TRUE)
  results <- list(base = base_result, reordered = reordered_result)
  samples <- list(base = stata_base_sample, reordered = stata_reordered_sample)

  for (scenario in names(results)) {
    result <- results[[scenario]]
    expected <- stata_estimates[stata_estimates$scenario == scenario, , drop = FALSE]
    expected_sample <- samples[[scenario]]

    expect_s3_class(result$artifacts$weights, "didbjs_weights")
    expect_equal(result$artifacts$weights$schema_version, "didbjs.weights.v1")
    expect_equal(result$artifacts$weights$representations, c("dense", "sparse"))
    expect_equal(result$estimates$term, expected$term, info = scenario)
    expect_equal(result$estimates$estimate, expected$estimate, tolerance = 1e-8, info = scenario)
    expect_equal(result$estimates$std.error, expected$std_error, tolerance = 1e-8, info = scenario)
    expect_equal(result$sample_mask$row_id, as.character(expected_sample$row_id), info = scenario)
    expect_true(all(result$sample_mask$sample == as.logical(expected_sample$sample)), info = scenario)
    expect_equal(nrow(result$artifacts$weights$weights), 180, info = scenario)
    expect_equal(nrow(result$artifacts$weights$sparse_weights), 120, info = scenario)
    expect_false(any(result$artifacts$weights$sparse_weights$weight == 0), info = scenario)
    f039_expect_weight_match(result$artifacts$weights$weights, f039_expected_weights(stata_dense, scenario))
    f039_expect_weight_match(result$artifacts$weights$sparse_weights, f039_expected_weights(stata_sparse, scenario))
  }

  expect_equal(base_result$artifacts$weights$metadata$spec_hash, reordered_result$artifacts$weights$metadata$spec_hash)
  f039_expect_weight_match(base_result$artifacts$weights$weights, reordered_result$artifacts$weights$weights)
})

test_that("F039 explicit row_id is the canonical artifact key when unique", {
  base <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "base.csv")
  base$row_id <- paste0("explicit-", seq_len(nrow(base)))

  result <- f039_run(base, saveweights = TRUE)

  expect_equal(result$sample_mask$row_id, base$row_id)
  expect_true(all(result$artifacts$weights$weights$row_id %in% base$row_id))
  expect_true(all(result$artifacts$weights$sparse_weights$row_id %in% base$row_id))
  expect_false(any(result$artifacts$weights$weights$row_id %in% as.character(seq_len(nrow(base)))))
})

test_that("F039 serialized dense and sparse loadweights reuse original row ids on reordered data", {
  base <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "base.csv")
  reordered <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "reordered.csv")
  stata_load <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "stata", "load-estimates.csv")
  expected_reordered <- stata_load[stata_load$scenario == "reordered", , drop = FALSE]

  saved <- f039_run(base, saveweights = TRUE)
  serialized_path <- tempfile(fileext = ".rds")
  saveRDS(saved$artifacts$weights, serialized_path)
  dense_artifact <- readRDS(serialized_path)
  sparse_artifact <- dense_artifact
  sparse_artifact$weights <- NULL

  dense_loaded <- f039_run(reordered, y = "Y2", loadweights = dense_artifact)
  sparse_loaded <- f039_run(reordered, y = "Y2", loadweights = sparse_artifact)
  full <- f039_run(reordered, y = "Y2")

  expect_true(dense_loaded$diagnostics$loadweights)
  expect_true(sparse_loaded$diagnostics$loadweights)
  expect_equal(dense_loaded$estimates$term, expected_reordered$term)
  expect_equal(dense_loaded$estimates$estimate, expected_reordered$estimate, tolerance = 1e-8)
  expect_equal(dense_loaded$estimates$std.error, expected_reordered$std_error, tolerance = 1e-8)
  expect_equal(sparse_loaded$estimates$estimate, dense_loaded$estimates$estimate, tolerance = 1e-12)
  expect_equal(sparse_loaded$estimates$std.error, dense_loaded$estimates$std.error, tolerance = 1e-12)
  expect_equal(full$estimates$estimate, dense_loaded$estimates$estimate, tolerance = 1e-12)
  expect_equal(full$estimates$std.error, dense_loaded$estimates$std.error, tolerance = 1e-12)
})

test_that("F039 bare data-frame loadweights are a warned manual override", {
  base <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "base.csv")
  reordered <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "reordered.csv")

  saved <- f039_run(base, saveweights = TRUE)
  artifact_loaded <- f039_run(reordered, y = "Y2", loadweights = saved$artifacts$weights)
  manual_loaded <- NULL

  expect_warning(
    manual_loaded <- f039_run(reordered, y = "Y2", loadweights = saved$artifacts$weights$weights),
    regexp = "manual override",
    class = "didbjs_manual_loadweights_warning"
  )

  expect_true(manual_loaded$diagnostics$loadweights)
  expect_equal(manual_loaded$estimates$estimate, artifact_loaded$estimates$estimate, tolerance = 1e-12)
  expect_equal(manual_loaded$estimates$std.error, artifact_loaded$estimates$std.error, tolerance = 1e-12)

  warn2_loaded <- NULL
  local({
    old_warn <- getOption("warn")
    on.exit(options(warn = old_warn), add = TRUE)
    options(warn = 2)
    expect_warning(
      warn2_loaded <<- f039_run(reordered, y = "Y2", loadweights = saved$artifacts$weights$weights),
      regexp = "manual override",
      class = "didbjs_manual_loadweights_warning"
    )
  })
  expect_equal(warn2_loaded$estimates$estimate, artifact_loaded$estimates$estimate, tolerance = 1e-12)
  expect_equal(warn2_loaded$estimates$std.error, artifact_loaded$estimates$std.error, tolerance = 1e-12)

  missing_manual <- saved$artifacts$weights$weights[-1, , drop = FALSE]
  expect_warning(
    expect_error(
      f039_run(base, y = "Y2", loadweights = missing_manual),
      regexp = "do not contain every row",
      class = "didbjs_contract_error"
    ),
    regexp = "manual override",
    class = "didbjs_manual_loadweights_warning"
  )
})

test_that("F039 invalid loadweights reuse and ambiguous row ids fail closed", {
  base <- read_fixture_csv("parity", "f039-artifact-row-mapping", "inputs", "base.csv")
  modified <- base
  modified$Ei[modified$unit == 1] <- 5
  saved <- f039_run(base, saveweights = TRUE)
  modified_saved <- f039_run(modified, y = "Y2", saveweights = TRUE)

  expect_false(identical(
    saved$artifacts$weights$metadata$spec_hash,
    modified_saved$artifacts$weights$metadata$spec_hash
  ))

  expect_error(
    f039_run(modified, y = "Y2", loadweights = saved$artifacts$weights),
    regexp = "incompatible sample or specification",
    class = "didbjs_contract_error"
  )

  duplicate_row_id <- base
  duplicate_row_id$row_id[[2]] <- duplicate_row_id$row_id[[1]]
  expect_error(
    f039_run(duplicate_row_id, saveweights = TRUE),
    regexp = "row_id values must uniquely identify observations",
    class = "didbjs_contract_error"
  )

  missing_dense <- saved$artifacts$weights
  missing_dense$weights <- missing_dense$weights[-1, , drop = FALSE]
  expect_error(
    f039_run(base, y = "Y2", loadweights = missing_dense),
    regexp = "do not contain every row",
    class = "didbjs_contract_error"
  )

  unknown_sparse <- saved$artifacts$weights
  unknown_sparse$weights <- NULL
  unknown_sparse$sparse_weights$row_id[[1]] <- "not_in_sample"
  expect_error(
    f039_run(base, y = "Y2", loadweights = unknown_sparse),
    regexp = "outside the current sample",
    class = "didbjs_contract_error"
  )

  bad_schema <- saved$artifacts$weights
  bad_schema$schema_version <- "didbjs.weights.v0"
  expect_error(
    f039_run(base, y = "Y2", loadweights = bad_schema),
    regexp = "unsupported schema version",
    class = "didbjs_contract_error"
  )

  duplicate_weight <- saved$artifacts$weights
  duplicate_weight$weights <- rbind(duplicate_weight$weights, duplicate_weight$weights[1, , drop = FALSE])
  expect_error(
    f039_run(base, y = "Y2", loadweights = duplicate_weight),
    regexp = "duplicate row_id/term pairs",
    class = "didbjs_contract_error"
  )
})

test_that("F039 Python reference records saveweights schema and reordered upstream drift", {
  python_schema <- jsonlite::fromJSON(fixture_path("parity", "f039-artifact-row-mapping", "expected", "python", "object-schema.json"))
  python_diag <- jsonlite::fromJSON(fixture_path("parity", "f039-artifact-row-mapping", "expected", "python", "diagnostics.json"))
  python_estimates <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "python", "estimates.csv")
  python_dense <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "python", "weights-dense.csv")
  python_sparse <- read_fixture_csv("parity", "f039-artifact-row-mapping", "expected", "python", "weights-sparse.csv")

  expect_equal(python_diag$status, "success")
  expect_equal(python_schema$weights_columns, c("copywtr0", "copywtr1", "copywtr2"))
  expect_equal(nrow(python_dense), python_diag$dense_row_count)
  expect_equal(nrow(python_sparse), python_diag$sparse_row_count)
  expect_equal(sum(python_dense$scenario == "base"), 180)
  expect_equal(sum(python_dense$scenario == "reordered"), 180)
  expect_equal(sum(python_sparse$scenario == "base"), 120)
  expect_equal(sum(python_sparse$scenario == "reordered"), 80)
  expect_equal(python_estimates$n_obs[python_estimates$scenario == "base"], rep(60, 3))
  expect_equal(python_estimates$n_obs[python_estimates$scenario == "reordered"], rep(55, 3))
})
