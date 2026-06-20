skip_if_not(fixtures_present())

f040_expect_numeric_equal <- function(actual, expected, tolerance = 1e-8) {
  same_missing <- is.na(actual) & is.na(expected)
  diff <- abs(actual - expected)
  expect_true(all(same_missing | diff <= tolerance, na.rm = TRUE))
}

f040_expect_kyle_table <- function(out, expected, schema, tolerance = 1e-8) {
  expect_equal(class(out), schema$class)
  expect_named(out, schema$names)
  out_df <- as.data.frame(out)[, names(expected), drop = FALSE]
  if ("lhs" %in% names(expected)) {
    out_df$lhs <- as.character(out_df$lhs)
    expected$lhs <- as.character(expected$lhs)
    expect_equal(out_df$lhs, expected$lhs)
  }
  out_df$term <- as.character(out_df$term)
  expected$term <- as.character(expected$term)
  expect_equal(out_df$term, expected$term)
  for (column in c("estimate", "std.error", "conf.low", "conf.high")) {
    f040_expect_numeric_equal(out_df[[column]], expected[[column]], tolerance = tolerance)
  }
}

test_that("F040 Kyle public calls match pinned Kyle outputs", {
  panel <- read_fixture_csv("parity", "f040-kyle-compatibility", "inputs", "panel.csv")
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f040-kyle-compatibility", "expected", "kyle", "diagnostics.json"))
  schema <- jsonlite::fromJSON(fixture_path("parity", "f040-kyle-compatibility", "expected", "kyle", "output-schema.json"))

  expect_equal(diagnostics$status, "success")
  expect_equal(diagnostics$didimputation_version, "0.5.0")
  expect_true(diagnostics$first_stage_formula_string_match)

  named <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    cluster_var = "unit"
  )
  f040_expect_kyle_table(
    named,
    read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "named-default-estimates.csv"),
    schema$named_default
  )
  expect_equal(named$conf.low, named$estimate - 1.96 * named$std.error, tolerance = 1e-12)
  expect_equal(named$conf.high, named$estimate + 1.96 * named$std.error, tolerance = 1e-12)

  positional <- did_imputation_kyle(panel, "Y", "Ei", "t", "unit", NULL, "w", NULL, 0:1, c(-2, -1), "unit")
  f040_expect_kyle_table(
    positional,
    read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "pos-dyn-pre-estimates.csv"),
    schema$positional_dynamic_pretrends
  )
  expect_equal(positional$conf.low, positional$estimate - 1.96 * positional$std.error, tolerance = 1e-12)
  expect_equal(positional$conf.high, positional$estimate + 1.96 * positional$std.error, tolerance = 1e-12)

  first_stage_formula <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    first_stage = ~ x1 + x2 | unit + t,
    wname = "w",
    cluster_var = "unit"
  )
  f040_expect_kyle_table(
    first_stage_formula,
    read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "first-stage-formula-estimates.csv"),
    schema$first_stage_formula
  )

  first_stage_string <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    first_stage = "x1 + x2 | unit + t",
    wname = "w",
    cluster_var = "unit"
  )
  f040_expect_kyle_table(
    first_stage_string,
    read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "first-stage-string-estimates.csv"),
    schema$first_stage_string
  )

  wtr <- did_imputation_kyle(
    data = panel,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    wname = "w",
    wtr = c("wtr_early", "wtr_late"),
    cluster_var = "unit"
  )
  f040_expect_kyle_table(
    wtr,
    read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "wtr-estimates.csv"),
    schema$wtr
  )

  multi <- did_imputation_kyle(
    data = panel,
    yname = "c(Y, Y2)",
    gname = "Ei",
    tname = "t",
    idname = "unit",
    first_stage = ~ x1 + x2 | unit + t,
    wname = "w",
    cluster_var = "unit"
  )
  f040_expect_kyle_table(
    multi,
    read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "multi-estimates.csv"),
    schema$multi
  )
})

test_that("F040 Kyle idname i divergence follows D016 alias oracle", {
  collision <- read_fixture_csv("parity", "f040-kyle-compatibility", "inputs", "id-collision.csv")
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f040-kyle-compatibility", "expected", "kyle", "diagnostics.json"))
  schema <- jsonlite::fromJSON(fixture_path("parity", "f040-kyle-compatibility", "expected", "kyle", "output-schema.json"))
  alias <- read_fixture_csv("parity", "f040-kyle-compatibility", "expected", "kyle", "id-collision-alias-estimates.csv")

  expect_equal(diagnostics$id_collision_status, "reference_error")
  expect_match(diagnostics$id_collision_error_message, "subscript out of bounds", fixed = TRUE)
  expect_equal(diagnostics$id_collision_alias_status, "success")
  expect_equal(schema$id_collision$status, "reference_error")

  out <- did_imputation_kyle(
    data = collision,
    yname = "Y",
    gname = "Ei",
    tname = "t",
    idname = "i",
    wname = "w",
    cluster_var = "i"
  )
  f040_expect_kyle_table(out, alias, schema$id_collision_alias)
})

test_that("F040 Kyle compatibility validation remains structured", {
  panel <- read_fixture_csv("parity", "f040-kyle-compatibility", "inputs", "panel.csv")

  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", horizon = TRUE),
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", first_stage = ~ log(x1) | unit + t),
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", first_stage = Y ~ x1 | unit + t),
    class = "didbjs_unsupported_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", wtr = c("wtr_early", "wtr_early")),
    class = "didbjs_contract_error"
  )
  expect_error(
    did_imputation_kyle(panel, yname = "Y", gname = "Ei", tname = "t", idname = "unit", first_stage = "missing_x | unit + t"),
    regexp = "Missing required Kyle columns: missing_x",
    class = "didbjs_contract_error"
  )
})
