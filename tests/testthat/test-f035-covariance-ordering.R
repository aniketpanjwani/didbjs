skip_if_not(fixtures_present())

f035_run <- function(panel, cluster) {
  did_imputation(
    data = panel,
    y = "Y",
    i = "unit",
    t = "t",
    Ei = "Ei",
    controls = c("x1", "x2"),
    horizons = 0:1,
    pretrends = 2,
    cluster = cluster,
    aw = "w",
    minn = 0
  )
}

f035_covariance <- function(covariance, scenario) {
  selected <- covariance[covariance$scenario == scenario, , drop = FALSE]
  terms <- unique(c(selected$row_term, selected$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(selected))) {
    mat[selected$row_term[[idx]], selected$col_term[[idx]]] <- selected$value[[idx]]
  }
  mat
}

f035_combined_estimates <- function(result) {
  estimate_cols <- c("term", "estimate", "std.error", "conf.low", "conf.high")
  rbind(
    result$estimates[, estimate_cols],
    result$controls[, estimate_cols]
  )
}

test_that("F035 covariance construction removes per-term scratch columns", {
  panel <- read_fixture_csv("smoke", "f001-static-att", "inputs", "panel.csv")
  dt <- data.table::as.data.table(panel)
  dt[, .didbjs_event_time := treatment_event_time(t, Ei, shift = 0, delta = 1)]
  dt[, .didbjs_untreated := is.na(.didbjs_event_time) | .didbjs_event_time < 0]
  dt[, .didbjs_treated := !.didbjs_untreated]
  dt[, .didbjs_weight := 1]

  first_stage <- fixest::feols(
    Y ~ 0 | i + t,
    data = dt[.didbjs_untreated == TRUE],
    weights = ~.didbjs_weight,
    warn = FALSE,
    notes = FALSE,
    fixef.rm = "none",
    fixef.tol = 1e-10,
    fixef.iter = 100000
  )
  dt[, .didbjs_y_hat := as.numeric(stats::predict(first_stage, newdata = dt))]
  weight_contract <- build_treatment_weights(
    dt = dt,
    horizons = NULL,
    hbalance = FALSE,
    unit_col = "i",
    custom_wtr = character(),
    sum_estimand = FALSE,
    hetby = NULL,
    project = character()
  )
  analysis_dt <- weight_contract$data
  sample_mask <- analysis_dt$.didbjs_untreated |
    treatment_rows_with_nonzero_weights(analysis_dt, weight_contract$columns)
  analysis_dt <- analysis_dt[sample_mask == TRUE]
  analysis_dt[, .didbjs_tau := Y - .didbjs_y_hat]
  before_names <- names(analysis_dt)

  static_cluster_covariance(
    dt = analysis_dt,
    first_stage = first_stage,
    cluster = "i",
    Ei = "Ei",
    terms = weight_contract$terms,
    wtr_cols = weight_contract$columns,
    controls = character(),
    fe_cols = c("i", "t"),
    avgeffectsby = c("Ei", "t")
  )

  scratch_pattern <- paste0("^\\.didbjs_(", paste(.didbjs_scratch_column_stems, collapse = "|"), ")_\\d+$")
  expect_equal(names(analysis_dt), before_names)
  expect_false(any(grepl(scratch_pattern, names(analysis_dt))))
})

test_that("F035 full covariance stripes, off-diagonals, and PSD diagnostics match Stata", {
  panel <- read_fixture_csv("parity", "f035-covariance-ordering", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f035-covariance-ordering", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f035-covariance-ordering", "expected", "stata", "covariance.csv")
  stata_order <- read_fixture_csv("parity", "f035-covariance-ordering", "expected", "stata", "matrix-order.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f035-covariance-ordering", "expected", "stata", "diagnostics.json"))
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f035-covariance-ordering", "expected", "stata", "probes.json"))

  expect_equal(stata_diag$status, "success")
  expect_equal(stata_probes$full_order$status, "reference_success")

  result <- f035_run(panel, "unit")
  expected_order <- stata_diag$expected_order
  expected_estimates <- stata_estimates[stata_estimates$scenario == "full_order", , drop = FALSE]
  expected_covariance <- f035_covariance(stata_covariance, "full_order")
  observed <- result$covariance[expected_order, expected_order, drop = FALSE]
  observed_estimates <- f035_combined_estimates(result)
  observed_estimates <- observed_estimates[match(expected_order, observed_estimates$term), ]
  order_rows <- stata_order[stata_order$scenario == "full_order", , drop = FALSE]

  expect_equal(order_rows$b_term, expected_order)
  expect_equal(order_rows$v_row_term, expected_order)
  expect_equal(order_rows$v_col_term, expected_order)
  expect_equal(rownames(result$covariance), expected_order)
  expect_equal(colnames(result$covariance), expected_order)
  expect_equal(observed_estimates$term, expected_order)
  expect_lt(max(abs(observed_estimates$estimate - expected_estimates$estimate)), 1e-8)
  expect_lt(max(abs(observed_estimates$std.error - expected_estimates$std_error)), 1e-8)
  expect_true(all(abs(observed - expected_covariance) < 1e-8))
  expect_lt(max(abs(observed - t(observed))), 1e-12)
  expect_gt(max(abs(observed[row(observed) != col(observed)])), 1e-3)
  expect_gt(min(eigen(observed, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_equal(result$diagnostics$pre_F, stata_diag$full_pre_F, tolerance = 1e-8)
  expect_equal(result$diagnostics$pre_p, stata_diag$full_pre_p, tolerance = 1e-8)
  expect_equal(result$diagnostics$pre_df, stata_diag$full_pre_df)
})

test_that("F035 singular pretrend covariance follows Stata rank-adjusted joint test", {
  panel <- read_fixture_csv("parity", "f035-covariance-ordering", "inputs", "panel.csv")
  stata_estimates <- read_fixture_csv("parity", "f035-covariance-ordering", "expected", "stata", "estimates.csv")
  stata_covariance <- read_fixture_csv("parity", "f035-covariance-ordering", "expected", "stata", "covariance.csv")
  stata_diag <- jsonlite::fromJSON(fixture_path("parity", "f035-covariance-ordering", "expected", "stata", "diagnostics.json"))
  stata_probes <- jsonlite::fromJSON(fixture_path("parity", "f035-covariance-ordering", "expected", "stata", "probes.json"))

  expect_equal(stata_probes$singular_pretrend$status, "reference_success")

  result <- f035_run(panel, "cluster_pair")
  expected_order <- stata_diag$expected_order
  expected_estimates <- stata_estimates[stata_estimates$scenario == "singular_pretrend", , drop = FALSE]
  expected_covariance <- f035_covariance(stata_covariance, "singular_pretrend")
  observed <- result$covariance[expected_order, expected_order, drop = FALSE]
  observed_estimates <- f035_combined_estimates(result)
  observed_estimates <- observed_estimates[match(expected_order, observed_estimates$term), ]
  pre_covariance <- observed[c("pre1", "pre2"), c("pre1", "pre2"), drop = FALSE]
  covariance_values <- eigen(observed, symmetric = TRUE, only.values = TRUE)$values

  expect_equal(rownames(result$covariance), expected_order)
  expect_equal(colnames(result$covariance), expected_order)
  expect_lt(max(abs(observed_estimates$estimate - expected_estimates$estimate)), 1e-8)
  expect_lt(max(abs(observed_estimates$std.error - expected_estimates$std_error)), 1e-8)
  expect_true(all(abs(observed - expected_covariance) < 1e-8))
  expect_lt(max(abs(observed - t(observed))), 1e-12)
  expect_equal(qr(pre_covariance)$rank, 1L)
  expect_lt(min(abs(eigen(pre_covariance, symmetric = TRUE, only.values = TRUE)$values)), 1e-12)
  expect_lt(min(covariance_values), 1e-12)
  expect_gt(max(covariance_values), 0)
  expect_lt(abs(result$diagnostics$pre_F - stata_diag$singular_pre_F), 1e-5)
  expect_equal(result$diagnostics$pre_p, stata_diag$singular_pre_p, tolerance = 1e-8)
  expect_equal(result$diagnostics$pre_df, stata_diag$singular_pre_df)
})
