skip_if_not(fixtures_present())

compare_f043_plot_data <- function(actual, expected, tolerance = 1e-10) {
  row.names(actual) <- NULL
  row.names(expected) <- NULL
  actual$plot_group_label <- sub("\r", "|", actual$plot_group, fixed = TRUE)

  expect_equal(actual$source, expected$source)
  expect_equal(actual$plot_type, expected$plot_type)
  expect_equal(actual$together, expected$together)
  expect_equal(actual$series, expected$series)
  expect_equal(actual$term, expected$term)
  expect_equal(actual$event_time, expected$event_time)
  expect_equal(actual$model, expected$model)
  expect_equal(actual$model_label, expected$model_label)
  expect_equal(actual$plot_group_label, expected$plot_group_label)
  expect_equal(actual$estimate, expected$estimate, tolerance = tolerance)
  expect_equal(actual$std_error, expected$std_error, tolerance = tolerance)
  expect_equal(actual$critical_value, expected$critical_value, tolerance = tolerance)
  expect_equal(actual$ci_low, expected$ci_low, tolerance = tolerance)
  expect_equal(actual$ci_high, expected$ci_high, tolerance = tolerance)
  expect_equal(actual$position, expected$position, tolerance = tolerance)
  expect_equal(actual$has_ci, expected$has_ci)
}

load_f043_payload <- function() {
  jsonlite::fromJSON(
    fixture_path("parity", "f043-plot-multimodel", "inputs", "models.json"),
    simplifyVector = FALSE
  )
}

run_f043_plot <- function(payload, ...) {
  args <- utils::modifyList(list(
    models = payload$models,
    stub_lag = payload$stub_lag,
    stub_lead = payload$stub_lead,
    trimlag = payload$trimlag,
    trimlead = payload$trimlead,
    shift = as.numeric(unlist(payload$shift)),
    perturb = as.numeric(unlist(payload$perturb)),
    model_names = unlist(payload$model_names),
    significance_level = payload$significance_level,
    plot_type = payload$plot_type
  ), list(...))
  do.call(event_plot, args)
}

test_that("F043 eight-model event_plot overlay matches independent semantic rows", {
  payload <- load_f043_payload()
  expected <- read_fixture_csv("parity", "f043-plot-multimodel", "expected", "semantic", "plot-data.csv")
  diag <- jsonlite::fromJSON(fixture_path("parity", "f043-plot-multimodel", "expected", "semantic", "diagnostics.json"))

  out <- run_f043_plot(payload)

  expect_equal(diag$status, "success")
  expect_equal(diag$model_count, 8)
  expect_equal(diag$rows, 32)
  expect_equal(diag$max_supported_models, 8)
  expect_equal(diag$scalar_arguments_recycled, c("stub_lag", "stub_lead", "trimlag", "trimlead"))
  expect_s3_class(out, "didbjs_event_plot")
  expect_s3_class(out$plot, "ggplot")

  compare_f043_plot_data(out$plot_data, expected)
  expect_equal(unique(out$plot_data$model_label), unlist(payload$model_names))
  expect_equal(unique(out$plot_data$series), c("Pre-trends", "Effects"))
  expect_equal(out$plot_data$model, rep(seq_len(8), each = 4))
  expect_equal(out$plot_data$event_time, rep(c(-2L, -1L, 0L, 1L), 8))
  expect_false(any(out$plot_data$term %in% c("pre3", "tau2")))
  expect_equal(
    out$plot_data$position - out$plot_data$event_time,
    rep(as.numeric(unlist(payload$perturb)) - as.numeric(unlist(payload$shift)), each = 4),
    tolerance = 1e-10
  )
  expect_equal(
    unique(sub("\r", "|", out$plot_data$plot_group, fixed = TRUE)),
    as.vector(rbind(
      paste(unlist(payload$model_names), "Pre-trends", sep = "|"),
      paste(unlist(payload$model_names), "Effects", sep = "|")
    ))
  )
})

test_that("F043 multi-model together mode and style controls remain stable", {
  payload <- load_f043_payload()

  together <- run_f043_plot(payload, together = TRUE)
  expect_equal(unique(together$plot_data$series), "Effects")
  expect_true(all(grepl("\rEffects$", together$plot_data$plot_group)))

  styled <- run_f043_plot(
    payload,
    pretrends_color = "darkgreen",
    effects_color = "orange",
    pretrends_marker = 17,
    effects_marker = 19
  )
  built <- ggplot2::ggplot_build(styled$plot)
  point_layer <- built$data[[3]]
  expect_setequal(unique(point_layer$colour), c("darkgreen", "orange"))
  expect_setequal(unique(point_layer$shape), c(17, 19))
})

test_that("F043 multi-model validation covers over-eight, stubs, trims, and offsets", {
  payload <- load_f043_payload()
  nine_models <- c(payload$models, payload$models[1])

  expect_error(
    event_plot(models = nine_models),
    regexp = "Combining at most 8 event_plot models is supported.",
    class = "didbjs_contract_error"
  )
  expect_error(
    run_f043_plot(payload, stub_lag = "tau#", stub_lead = "tau#"),
    regexp = "stub_lag and stub_lead have to be different for model 1.",
    class = "didbjs_contract_error"
  )
  expect_error(
    run_f043_plot(payload, perturb = c(0, 0.1)),
    regexp = "perturb must have length 1 or one value per model.",
    class = "didbjs_contract_error"
  )
  expect_error(
    run_f043_plot(payload, shift = c(0, rep(Inf, 7))),
    regexp = "shift must contain finite numeric values.",
    class = "didbjs_contract_error"
  )
  expect_error(
    run_f043_plot(payload, model_names = c("only one")),
    regexp = "model_names must contain one non-empty label per model.",
    class = "didbjs_contract_error"
  )

  boundary <- event_plot(
    models = payload$models,
    stub_lag = payload$stub_lag,
    stub_lead = payload$stub_lead,
    trimlag = 0,
    trimlead = 1,
    shift = 0,
    perturb = 0,
    model_names = unlist(payload$model_names),
    significance_level = payload$significance_level
  )
  expect_equal(nrow(boundary$plot_data), 16)
  expect_equal(boundary$plot_data$event_time, rep(c(-1L, 0L), 8))
  expect_equal(boundary$plot_data$term, rep(c("pre1", "tau0"), 8))
  expect_equal(boundary$plot_data$position, boundary$plot_data$event_time)

  default_perturb <- event_plot(
    models = payload$models,
    stub_lag = payload$stub_lag,
    stub_lead = payload$stub_lead,
    trimlag = payload$trimlag,
    trimlead = payload$trimlead,
    shift = 0,
    model_names = unlist(payload$model_names),
    significance_level = payload$significance_level
  )
  expect_equal(
    sort(unique(round(default_perturb$plot_data$position - default_perturb$plot_data$event_time, 12))),
    c(0, 0.2 * seq_len(7) / 8),
    tolerance = 1e-10
  )
})
