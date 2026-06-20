f048_benchmark_result_path <- function(filename) {
  installed <- system.file("bench", "results", "f048", filename, package = "didbjs")
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "bench", "results", "f048", filename)
}

test_that("F048 committed performance pathology results satisfy frozen budgets", {
  budget_path <- system.file("spec", "bench-budgets.yml", package = "didbjs")
  if (!nzchar(budget_path)) {
    budget_path <- testthat::test_path("..", "..", "bench", "budgets.yml")
  }
  budgets <- yaml::read_yaml(budget_path)
  results <- utils::read.csv(f048_benchmark_result_path("results.csv"), check.names = FALSE)
  diagnostics <- jsonlite::fromJSON(f048_benchmark_result_path("diagnostics.json"))
  manifest <- jsonlite::fromJSON(f048_benchmark_result_path("manifest.json"))

  required <- c("saveweights_pathology", "plotting_overlay")
  expect_equal(diagnostics$status, "success")
  expect_equal(manifest$fixture_id, "F048")
  expect_equal(manifest$terminal_status$benchmark, "success")
  expect_equal(diagnostics$measured_iterations, budgets$benchmark_environment$measured_iterations)
  expect_equal(diagnostics$warmup_iterations, budgets$benchmark_environment$warmup_iterations)
  expect_match(diagnostics$rss_method, "maximum resident set size")
  expect_equal(sort(results$benchmark), sort(required))
  expect_equal(results$budget_status, rep("pass", length(required)))
  expect_equal(results$parity_status, rep("passed_before_benchmark", length(required)))

  saveweights <- results[results$benchmark == "saveweights_pathology", , drop = FALSE]
  save_budget <- budgets$budgets$saveweights_pathology
  expect_equal(nrow(saveweights), 1L)
  expect_equal(saveweights$rows, save_budget$rows)
  expect_equal(saveweights$units, save_budget$units)
  expect_equal(saveweights$periods, save_budget$periods)
  expect_equal(saveweights$controls, 5L)
  expect_equal(saveweights$horizons, 6L)
  expect_true(saveweights$saveweights)
  expect_equal(saveweights$treated_units, 2500L)
  expect_equal(saveweights$baseline_status, "kyle_controls_fe_measured_with_equivalent_rss_artifact")
  expect_lte(saveweights$didbjs_median_seconds, save_budget$max_median_seconds)
  expect_lte(saveweights$didbjs_p90_seconds, save_budget$max_p90_seconds)
  expect_lte(saveweights$didbjs_peak_rss_mb, save_budget$max_peak_rss_mb)
  expect_lte(saveweights$runtime_ratio_vs_kyle, save_budget$max_runtime_ratio_vs_kyle)
  expect_lte(saveweights$peak_rss_ratio_vs_kyle, save_budget$max_peak_rss_ratio_vs_kyle)
  expect_gt(saveweights$kyle_median_seconds, 0)
  expect_gt(saveweights$kyle_peak_rss_mb, 0)

  sparse_probe <- diagnostics$sparse_controls_probe
  expect_equal(sparse_probe$status, "success")
  expect_equal(sparse_probe$rows, save_budget$rows)
  expect_equal(sparse_probe$units, save_budget$units)
  expect_equal(sparse_probe$periods, save_budget$periods)
  expect_lte(sparse_probe$treated_unit_share, 0.2)
  expect_equal(sparse_probe$controls, 5L)
  expect_equal(sparse_probe$horizons, 6L)
  expect_true(sparse_probe$saveweights)
  expect_lte(sparse_probe$didbjs_elapsed_seconds, save_budget$max_median_seconds)
  expect_gt(sparse_probe$saved_sparse_rows, 0)
  expect_lte(sparse_probe$max_abs_estimate_error_from_one, 1e-3)

  plot <- results[results$benchmark == "plotting_overlay", , drop = FALSE]
  plot_budget <- budgets$budgets$plotting_overlay
  expect_equal(nrow(plot), 1L)
  expect_equal(plot$models, plot_budget$models)
  expect_equal(plot$terms_per_model, plot_budget$terms_per_model)
  expect_equal(plot$baseline_status, "not_applicable_plotting_budget_has_no_kyle_ratio")
  expect_lte(plot$didbjs_median_seconds, plot_budget$max_median_seconds)
  expect_lte(plot$didbjs_p90_seconds, plot_budget$max_p90_seconds)
  expect_lte(plot$didbjs_peak_rss_mb, plot_budget$max_peak_rss_mb)
  expect_true(is.na(plot$runtime_ratio_vs_kyle))
  expect_equal(diagnostics$plotting_overlay$plot_data_rows, 168L)
})
