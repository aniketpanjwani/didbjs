skip_if_not(fixtures_present())

f044_png_info <- function(path) {
  bytes <- readBin(path, what = "raw", n = 24)
  raw_to_int <- function(x) {
    sum(as.integer(x) * 256^(rev(seq_along(x)) - 1))
  }
  list(
    magic = paste(format(bytes[1:8]), collapse = ""),
    width = raw_to_int(bytes[17:20]),
    height = raw_to_int(bytes[21:24])
  )
}

f044_run_plot <- function(payload, ...) {
  args <- utils::modifyList(list(
    pretrends = payload$pretrends,
    pretrends_std = payload$pretrends_std,
    effects = payload$effects,
    effects_std = payload$effects_std,
    significance_level = payload$significance_level,
    plot_type = payload$plot_type,
    figsize = as.numeric(unlist(payload$figsize)),
    title = payload$title,
    xlabel = payload$xlabel,
    ylabel = payload$ylabel,
    dpi = payload$dpi
  ), list(...))
  do.call(event_plot, args)
}

test_that("F044 records Python save-path behavior and D013 filesystem policy", {
  diagnostics <- jsonlite::fromJSON(fixture_path("parity", "f044-plot-filesystem", "expected", "python", "diagnostics.json"))
  policy <- jsonlite::fromJSON(fixture_path("parity", "f044-plot-filesystem", "expected", "semantic", "policy.json"))
  payload <- jsonlite::fromJSON(fixture_path("parity", "f044-plot-filesystem", "inputs", "manual.json"))

  expect_equal(diagnostics$status, "success")
  expect_equal(diagnostics$figure_class, "Figure")
  expect_equal(diagnostics$matplotlib_backend, "Agg")
  expect_equal(diagnostics$figure_inches, payload$figsize)
  expect_true(diagnostics$png_exists)
  expect_equal(diagnostics$png_magic, "89504e470d0a1a0a")
  expect_equal(length(diagnostics$png_dimensions), 2)
  expect_true(all(diagnostics$png_dimensions > 0))
  expect_true(diagnostics$overwrite_existing_path)
  expect_gt(diagnostics$overwrite_size, 1000)
  expect_equal(diagnostics$invalid_parent_error$class, "FileNotFoundError")
  expect_length(diagnostics$open_figures_after_close, 0)

  expect_equal(policy$status, "success")
  expect_equal(policy$dimensions$expected_png_pixels, payload$expected_png_pixels)
  expect_match(policy$overwrite_policy$didbjs, "fails closed")
  expect_match(policy$device_policy, "unchanged")
  expect_match(policy$theme_policy, "theme_get")
})

test_that("F044 event_plot saves PNG files with configured dimensions without side effects", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f044-plot-filesystem", "inputs", "manual.json"))
  before_devices <- grDevices::dev.list()
  before_theme <- ggplot2::theme_get()
  save_path <- tempfile(fileext = ".png")

  out <- f044_run_plot(payload, save_path = save_path)
  info <- f044_png_info(save_path)

  expect_s3_class(out, "didbjs_event_plot")
  expect_s3_class(out$plot, "ggplot")
  expect_true(file.exists(save_path))
  expect_gt(file.info(save_path)$size, 1000)
  expect_equal(info$magic, "89504e470d0a1a0a")
  expect_equal(c(info$width, info$height), payload$expected_png_pixels)
  expect_identical(grDevices::dev.list(), before_devices)
  expect_identical(ggplot2::theme_get(), before_theme)
})

test_that("F044 overwrite, noplot, invalid paths, and plot method are structured", {
  payload <- jsonlite::fromJSON(fixture_path("parity", "f044-plot-filesystem", "inputs", "manual.json"))
  before_devices <- grDevices::dev.list()
  before_theme <- ggplot2::theme_get()

  existing <- tempfile(fileext = ".png")
  writeLines("sentinel", existing)
  expect_error(
    f044_run_plot(payload, save_path = existing, overwrite = FALSE),
    regexp = "save_path already exists and overwrite is FALSE.",
    class = "didbjs_contract_error"
  )
  expect_equal(readLines(existing), "sentinel")
  overwritten <- f044_run_plot(payload, save_path = existing, overwrite = TRUE)
  expect_s3_class(overwritten$plot, "ggplot")
  expect_equal(f044_png_info(existing)$magic, "89504e470d0a1a0a")

  missing_parent <- file.path(tempdir(), "f044-missing-parent", "plot.png")
  expect_error(
    f044_run_plot(payload, save_path = missing_parent),
    regexp = "save_path parent directory does not exist:",
    class = "didbjs_contract_error"
  )
  expect_error(
    f044_run_plot(payload, save_path = tempdir()),
    regexp = "save_path must be a file path, not an existing directory.",
    class = "didbjs_contract_error"
  )
  expect_error(
    f044_run_plot(payload, save_path = ""),
    regexp = "save_path must be a single non-empty string.",
    class = "didbjs_contract_error"
  )
  expect_error(
    f044_run_plot(payload, save_path = tempfile(fileext = ".png"), noplot = TRUE),
    regexp = "save_path cannot be combined with noplot = TRUE.",
    class = "didbjs_contract_error"
  )

  noplot <- f044_run_plot(payload, noplot = TRUE)
  expect_s3_class(noplot, "didbjs_event_plot")
  expect_null(noplot$plot)
  expect_gt(nrow(noplot$plot_data), 0)
  expect_error(
    plot(noplot),
    regexp = "This event plot was created with noplot = TRUE.",
    class = "didbjs_contract_error"
  )
  expect_identical(grDevices::dev.list(), before_devices)
  expect_identical(ggplot2::theme_get(), before_theme)
})
