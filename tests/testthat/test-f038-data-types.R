f038_base_panel <- function() {
  read_fixture_csv("api", "f038-data-types", "inputs", "base.csv")
}

f038_run <- function(panel, i = "i", t = "t", Ei = "Ei", y = "Y", aw = "w", cluster = i) {
  did_imputation(panel, y = y, i = i, t = t, Ei = Ei, aw = aw, cluster = cluster, minn = 0)
}

f038_expect_static_result <- function(result) {
  expect_s3_class(result, "didbjs")
  expect_named(result, c("estimates", "controls", "covariance", "sample_mask", "artifacts", "diagnostics", "call"))
  expect_named(result$estimates, c("term", "estimate", "std.error", "conf.low", "conf.high", "n_obs", "n_control", "n_treated"))
  expect_named(result$sample_mask, c("row_id", "sample", "cannot_impute", "subset", "missing_required"))
  expect_equal(result$estimates$term, "tau")
  expect_equal(result$estimates$estimate, 2, tolerance = 1e-12)
  expect_equal(result$estimates$std.error, 0.0632455532033676, tolerance = 1e-12)
  expect_equal(result$covariance["tau", "tau"], 0.004, tolerance = 1e-12)
  expect_equal(result$estimates$n_obs, 60)
  expect_equal(result$estimates$n_control, 45)
  expect_equal(result$estimates$n_treated, 15)
  expect_true(all(result$sample_mask$sample))
  expect_false(any(result$sample_mask$cannot_impute))
  expect_false(any(result$sample_mask$missing_required))
}

test_that("F038 R-native object shape is stable across unit-id storage types", {
  base <- f038_base_panel()
  id_variants <- list(
    integer = transform(base, i = as.integer(i)),
    double = transform(base, i = as.numeric(i) + 0.125),
    factor = transform(base, i = factor(i)),
    character = transform(base, i = paste0("unit-", i))
  )

  for (variant_name in names(id_variants)) {
    result <- f038_run(id_variants[[variant_name]])
    f038_expect_static_result(result)
    expect_equal(result$sample_mask$row_id, base$row_id, info = variant_name)
    expect_equal(result$diagnostics$cluster, "i", info = variant_name)
  }
})

test_that("F038 data.frame data.table and tibble-like inputs are copied and not mutated", {
  base <- f038_base_panel()
  containers <- list(
    data_frame = base,
    data_table = data.table::as.data.table(base),
    tibble_like = structure(base, class = c("tbl_df", "tbl", "data.frame"))
  )

  for (container_name in names(containers)) {
    panel <- containers[[container_name]]
    before_names <- names(panel)
    before_data <- as.data.frame(panel)

    result <- f038_run(panel)

    f038_expect_static_result(result)
    expect_equal(names(panel), before_names, info = container_name)
    expect_equal(as.data.frame(panel), before_data, info = container_name)
    expect_false(any(startsWith(names(panel), ".didbjs")), info = container_name)
  }
})

test_that("F038 nonsyntactic R-native column names are accepted without mutation", {
  panel <- read_fixture_csv("api", "f038-data-types", "inputs", "nonsyntactic.csv")
  before <- panel

  result <- f038_run(
    panel,
    y = "outcome value",
    i = "unit id",
    t = "time period",
    Ei = "treat time",
    aw = "case weight",
    cluster = "unit id"
  )

  f038_expect_static_result(result)
  expect_equal(result$sample_mask$row_id, panel$row_id)
  expect_equal(result$diagnostics$fe, c("unit id", "time period"))
  expect_equal(result$diagnostics$cluster, "unit id")
  expect_equal(panel, before)
})

test_that("F038 row names do not replace explicit or generated row identifiers", {
  base <- f038_base_panel()
  explicit <- base
  row.names(explicit) <- paste0("rn_", seq_len(nrow(explicit)))

  explicit_result <- f038_run(explicit)

  f038_expect_static_result(explicit_result)
  expect_equal(explicit_result$sample_mask$row_id, base$row_id)
  expect_false(any(explicit_result$sample_mask$row_id %in% row.names(explicit)))

  generated <- base[, setdiff(names(base), "row_id"), drop = FALSE]
  row.names(generated) <- paste0("rn_", seq_len(nrow(generated)))
  generated_before <- generated

  generated_result <- f038_run(generated)

  f038_expect_static_result(generated_result)
  expect_equal(generated_result$sample_mask$row_id, as.character(seq_len(nrow(generated))))
  expect_false(any(generated_result$sample_mask$row_id %in% row.names(generated)))
  expect_equal(generated, generated_before)
})
