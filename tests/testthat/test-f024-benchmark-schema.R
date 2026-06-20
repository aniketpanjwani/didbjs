benchmark_result_path <- function(filename) {
  installed <- system.file("bench", "results", "f024", filename, package = "didbjs")
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "bench", "results", "f024", filename)
}

test_that("F024 committed benchmark results satisfy frozen budgets", {
  budget_path <- system.file("spec", "bench-budgets.yml", package = "didbjs")
  if (!nzchar(budget_path)) {
    budget_path <- testthat::test_path("..", "..", "bench", "budgets.yml")
  }
  budgets <- yaml::read_yaml(budget_path)
  results <- utils::read.csv(benchmark_result_path("results.csv"), check.names = FALSE)
  diagnostics <- jsonlite::fromJSON(benchmark_result_path("diagnostics.json"))
  required <- c("small_static_overlap", "medium_dynamic_overlap", "large_many_fe")

  expect_equal(diagnostics$status, "success")
  expect_equal(diagnostics$measured_iterations, budgets$benchmark_environment$measured_iterations)
  expect_equal(diagnostics$warmup_iterations, budgets$benchmark_environment$warmup_iterations)
  expect_match(diagnostics$rss_method, "maximum resident set size")
  expect_equal(sort(results$benchmark), sort(required))
  expect_equal(results$budget_status, rep("pass", length(required)))
  expect_equal(results$parity_status, rep("passed_before_benchmark", length(required)))
  expect_true(all(nzchar(results$python_status)))

  for (benchmark in required) {
    row <- results[results$benchmark == benchmark, , drop = FALSE]
    budget <- budgets$budgets[[benchmark]]
    expect_equal(nrow(row), 1L)
    expect_equal(row$rows, budget$rows)
    expect_equal(row$units, budget$units)
    expect_equal(row$periods, budget$periods)
    expect_lte(row$didbjs_median_seconds, budget$max_median_seconds)
    expect_lte(row$didbjs_p90_seconds, budget$max_p90_seconds)
    expect_lte(row$didbjs_peak_rss_mb, budget$max_peak_rss_mb)
    expect_lte(row$runtime_ratio_vs_kyle, budget$max_runtime_ratio_vs_kyle)
    expect_lte(row$peak_rss_ratio_vs_kyle, budget$max_peak_rss_ratio_vs_kyle)
    expect_gt(row$kyle_median_seconds, 0)
    expect_gt(row$kyle_peak_rss_mb, 0)
  }
})
