event_plot <- function(results_obj = NULL,
                       pretrends = NULL,
                       pretrends_std = NULL,
                       effects = NULL,
                       effects_std = NULL,
                       models = NULL,
                       stub_lag = "tau#",
                       stub_lead = "pre#",
                       trimlag = NULL,
                       trimlead = NULL,
                       shift = 0,
                       perturb = NULL,
                       model_names = NULL,
                       plot_type = "rcap",
                       significance_level = 0.05,
                       pretrends_marker = 16,
                       effects_marker = 15,
                       pretrends_color = "blue",
                       effects_color = "red",
                       effects_linestyle = "solid",
                       pretrends_linestyle = "solid",
                       figsize = c(12, 8),
                       title = "",
                       xlabel = "Time Relative to Treatment",
                       ylabel = "Coefficient",
                       show_grid = TRUE,
                       show_legend = TRUE,
                       show_zero_line = TRUE,
                       zero_line_position = 0,
                       zero_line_style = "dashed",
                       zero_line_color = "grey",
                       zero_line_width = 1,
                       zero_line_alpha = 0.5,
                       show_event_line = TRUE,
                       event_line_position = 0,
                       event_line_style = "dashed",
                       event_line_color = "black",
                       event_line_width = 1,
                       event_line_alpha = 0.5,
                       pretrends_marker_size = 2.8,
                       effects_marker_size = 2.8,
                       pretrends_line_width = 0.7,
                       effects_line_width = 0.7,
                       effects_alpha = 0.7,
                       pretrends_alpha = 0.7,
                       pretrends_rarea_color = "blue",
                       effects_rarea_color = "red",
                       pretrends_rarea_alpha = 0.3,
                       effects_rarea_alpha = 0.3,
                       pretrends_label = "Pre-trends",
                       effects_label = "Effects",
                       font_size = 12,
                       legend_loc = "best",
                       save_path = NULL,
                       dpi = 300,
                       together = FALSE,
                       noplot = FALSE,
                       overwrite = TRUE,
                       ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop_unsupported("Unsupported event_plot arguments: ", paste(names(dots), collapse = ", "))
  }
  if (!identical(plot_type, "rcap") && !identical(plot_type, "rarea")) {
    stop_contract("plot_type must be 'rcap' or 'rarea'.")
  }
  together <- normalize_flag(together, "together")
  noplot <- normalize_flag(noplot, "noplot")
  overwrite <- normalize_flag(overwrite, "overwrite")
  show_grid <- normalize_flag(show_grid, "show_grid")
  show_legend <- normalize_flag(show_legend, "show_legend")
  show_zero_line <- normalize_flag(show_zero_line, "show_zero_line")
  show_event_line <- normalize_flag(show_event_line, "show_event_line")
  if (!is.numeric(significance_level) || length(significance_level) != 1 ||
      !is.finite(significance_level) || significance_level <= 0 || significance_level >= 1) {
    stop_contract("significance_level must be a finite number between 0 and 1.")
  }
  if (!is.numeric(dpi) || length(dpi) != 1 || !is.finite(dpi) || dpi <= 0) {
    stop_contract("dpi must be a positive finite number.")
  }
  figsize <- normalize_figsize(figsize)
  if (!is.null(save_path)) {
    validate_event_plot_save_path(save_path, overwrite)
    if (isTRUE(noplot)) {
      stop_contract("save_path cannot be combined with noplot = TRUE.")
    }
  }

  if (!is.null(models)) {
    if (!is.null(results_obj) ||
        any(vapply(list(pretrends, pretrends_std, effects, effects_std), Negate(is.null), logical(1)))) {
      stop_contract("models cannot be combined with results_obj or manual pretrends/effects inputs.")
    }
    plot_data <- build_stata_model_plot_data(
      models = models,
      stub_lag = stub_lag,
      stub_lead = stub_lead,
      trimlag = trimlag,
      trimlead = trimlead,
      shift = shift,
      perturb = perturb,
      model_names = model_names,
      plot_type = plot_type,
      significance_level = significance_level,
      together = together,
      pretrends_label = pretrends_label,
      effects_label = effects_label
    )
  } else {
    pieces <- event_plot_extract_inputs(
      results_obj = results_obj,
      pretrends = pretrends,
      pretrends_std = pretrends_std,
      effects = effects,
      effects_std = effects_std
    )
    plot_data <- build_event_plot_data(
      pretrends = pieces$pretrends,
      pretrends_std = pieces$pretrends_std,
      effects = pieces$effects,
      effects_std = pieces$effects_std,
      source = pieces$source,
      plot_type = plot_type,
      together = together,
      significance_level = significance_level,
      pretrends_label = pretrends_label,
      effects_label = effects_label
    )
  }
  if (nrow(plot_data) == 0) {
    stop_contract("event_plot requires at least one pretrend or effect estimate.")
  }

  plot <- NULL
  if (!isTRUE(noplot)) {
    plot <- render_event_plot(
      plot_data = plot_data,
      plot_type = plot_type,
      pretrends_marker = pretrends_marker,
      effects_marker = effects_marker,
      pretrends_color = pretrends_color,
      effects_color = effects_color,
      effects_linestyle = effects_linestyle,
      pretrends_linestyle = pretrends_linestyle,
      pretrends_line_width = pretrends_line_width,
      effects_line_width = effects_line_width,
      pretrends_marker_size = pretrends_marker_size,
      effects_marker_size = effects_marker_size,
      pretrends_alpha = pretrends_alpha,
      effects_alpha = effects_alpha,
      pretrends_rarea_color = pretrends_rarea_color,
      effects_rarea_color = effects_rarea_color,
      pretrends_rarea_alpha = pretrends_rarea_alpha,
      effects_rarea_alpha = effects_rarea_alpha,
      title = title,
      xlabel = xlabel,
      ylabel = ylabel,
      show_grid = show_grid,
      show_legend = show_legend,
      show_zero_line = show_zero_line,
      zero_line_position = zero_line_position,
      zero_line_style = zero_line_style,
      zero_line_color = zero_line_color,
      zero_line_width = zero_line_width,
      zero_line_alpha = zero_line_alpha,
      show_event_line = show_event_line,
      event_line_position = event_line_position,
      event_line_style = event_line_style,
      event_line_color = event_line_color,
      event_line_width = event_line_width,
      event_line_alpha = event_line_alpha,
      font_size = font_size,
      legend_loc = legend_loc
    )
    if (!is.null(save_path)) {
      ggplot2::ggsave(
        filename = save_path,
        plot = plot,
        width = figsize[[1]],
        height = figsize[[2]],
        dpi = dpi,
        units = "in"
      )
    }
  }

  structure(
    list(
      plot = plot,
      plot_data = plot_data,
      plot_type = plot_type,
      together = together,
      save_path = save_path,
      call = match.call()
    ),
    class = "didbjs_event_plot"
  )
}

event_plot_extract_inputs <- function(results_obj, pretrends, pretrends_std, effects, effects_std) {
  manual_supplied <- any(vapply(list(pretrends, pretrends_std, effects, effects_std), Negate(is.null), logical(1)))
  if (!is.null(results_obj) && isTRUE(manual_supplied)) {
    stop_contract("results_obj cannot be combined with manual pretrends/effects inputs.")
  }
  if (is.null(results_obj)) {
    return(list(
      pretrends = normalize_plot_terms(pretrends, "pretrends", "pre"),
      pretrends_std = normalize_plot_std(pretrends_std, "pretrends_std", pretrends, "pre"),
      effects = normalize_plot_terms(effects, "effects", "tau"),
      effects_std = normalize_plot_std(effects_std, "effects_std", effects, "tau"),
      source = "manual"
    ))
  }
  if (inherits(results_obj, "didbjs")) {
    extracted <- extract_didbjs_event_terms(results_obj)
    return(c(extracted, list(source = "object")))
  }
  list(
    pretrends = normalize_plot_terms(results_obj$pretrends_estimates, "results_obj$pretrends_estimates", "pre"),
    pretrends_std = normalize_plot_std(
      results_obj$pretrends_std_errors,
      "results_obj$pretrends_std_errors",
      results_obj$pretrends_estimates,
      "pre"
    ),
    effects = normalize_plot_terms(results_obj$estimates, "results_obj$estimates", "tau"),
    effects_std = normalize_plot_std(results_obj$std_errors, "results_obj$std_errors", results_obj$estimates, "tau"),
    source = "object"
  )
}

extract_didbjs_event_terms <- function(results_obj) {
  estimates <- results_obj$estimates
  if (!is.data.frame(estimates) || !"term" %in% names(estimates) || !"estimate" %in% names(estimates)) {
    stop_contract("didbjs results_obj must contain estimates with term and estimate columns.")
  }
  se_col <- if ("std.error" %in% names(estimates)) "std.error" else NULL
  pre_rows <- grepl("^pre[0-9]+$", estimates$term)
  effect_rows <- grepl("^tau[0-9]+$", estimates$term) | estimates$term == "tau"
  preterms <- stats::setNames(estimates$estimate[pre_rows], estimates$term[pre_rows])
  effect_terms <- estimates$term[effect_rows]
  effect_terms[effect_terms == "tau"] <- "tau0"
  effects <- stats::setNames(estimates$estimate[effect_rows], effect_terms)
  pre_std <- NULL
  effects_std <- NULL
  if (!is.null(se_col)) {
    pre_std <- stats::setNames(estimates[[se_col]][pre_rows], estimates$term[pre_rows])
    effects_std <- stats::setNames(estimates[[se_col]][effect_rows], effect_terms)
  }
  list(
    pretrends = normalize_plot_terms(preterms, "results_obj pretrend estimates", "pre"),
    pretrends_std = normalize_plot_std(pre_std, "results_obj pretrend std errors", preterms, "pre"),
    effects = normalize_plot_terms(effects, "results_obj effect estimates", "tau"),
    effects_std = normalize_plot_std(effects_std, "results_obj effect std errors", effects, "tau")
  )
}

normalize_plot_terms <- function(x, name, prefix) {
  if (is.null(x) || length(x) == 0) {
    return(stats::setNames(numeric(), character()))
  }
  out <- normalize_named_numeric(x, name)
  validate_event_terms(names(out), name, prefix)
  out
}

normalize_plot_std <- function(x, name, estimates, prefix) {
  estimate_terms <- names(normalize_plot_terms(estimates, paste0(name, " estimates"), prefix))
  if (is.null(x) || length(x) == 0) {
    return(stats::setNames(numeric(), character()))
  }
  out <- normalize_named_numeric(x, name)
  validate_event_terms(names(out), name, prefix)
  missing_terms <- setdiff(estimate_terms, names(out))
  extra_terms <- setdiff(names(out), estimate_terms)
  if (length(missing_terms) > 0) {
    stop_contract(name, " is missing terms: ", paste(missing_terms, collapse = ", "))
  }
  if (length(extra_terms) > 0) {
    stop_contract(name, " contains unknown terms: ", paste(extra_terms, collapse = ", "))
  }
  out[estimate_terms]
}

normalize_named_numeric <- function(x, name) {
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
  }
  if (!is.numeric(x)) {
    stop_contract(name, " must be a named numeric vector or named list.")
  }
  term_names <- names(x)
  if (is.null(term_names) || anyNA(term_names) || any(term_names == "")) {
    stop_contract(name, " must have non-empty term names.")
  }
  if (anyDuplicated(term_names)) {
    stop_contract(name, " cannot contain duplicate terms.")
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop_contract(name, " must contain finite values.")
  }
  stats::setNames(as.numeric(x), term_names)
}

validate_event_terms <- function(terms, name, prefix) {
  if (length(terms) == 0) {
    return(invisible(NULL))
  }
  pattern <- paste0("^", prefix, "[0-9]+$")
  bad <- terms[!grepl(pattern, terms)]
  if (length(bad) > 0) {
    stop_contract(name, " terms must match ", prefix, "#: ", paste(bad, collapse = ", "))
  }
  invisible(NULL)
}

build_event_plot_data <- function(pretrends,
                                  pretrends_std,
                                  effects,
                                  effects_std,
                                  source,
                                  plot_type,
                                  together,
                                  significance_level,
                                  pretrends_label,
                                  effects_label) {
  critical_value <- stats::qnorm(1 - significance_level / 2)
  pre <- event_plot_rows(
    values = pretrends,
    std_errors = pretrends_std,
    source = source,
    plot_type = plot_type,
    together = together,
    series = if (isTRUE(together)) effects_label else pretrends_label,
    prefix = "pre",
    critical_value = critical_value
  )
  eff <- event_plot_rows(
    values = effects,
    std_errors = effects_std,
    source = source,
    plot_type = plot_type,
    together = together,
    series = effects_label,
    prefix = "tau",
    critical_value = critical_value
  )
  out <- rbind(pre, eff)
  if (nrow(out) == 0) {
    return(out)
  }
  out$model <- 1L
  out$model_label <- "Model 1"
  out$position <- out$event_time
  out$plot_group <- paste(out$model_label, out$series, sep = "\r")
  out[order(out$event_time), , drop = FALSE]
}

event_plot_rows <- function(values, std_errors, source, plot_type, together, series, prefix, critical_value) {
  if (length(values) == 0) {
    return(data.frame(
      source = character(),
      plot_type = character(),
      together = logical(),
      series = character(),
      term = character(),
      event_time = integer(),
      estimate = numeric(),
      std_error = numeric(),
      critical_value = numeric(),
      ci_low = numeric(),
      ci_high = numeric(),
      has_ci = logical(),
      model = integer(),
      model_label = character(),
      position = numeric(),
      plot_group = character(),
      stringsAsFactors = FALSE
    ))
  }
  term <- names(values)
  event_time <- as.integer(sub(paste0("^", prefix), "", term))
  if (identical(prefix, "pre")) {
    event_time <- -event_time
  }
  has_ci <- term %in% names(std_errors)
  se <- rep(NA_real_, length(values))
  se[has_ci] <- as.numeric(std_errors[term[has_ci]])
  ci_low <- rep(NA_real_, length(values))
  ci_high <- rep(NA_real_, length(values))
  ci_low[has_ci] <- values[has_ci] - critical_value * se[has_ci]
  ci_high[has_ci] <- values[has_ci] + critical_value * se[has_ci]
  data.frame(
    source = source,
    plot_type = plot_type,
    together = together,
    series = series,
    term = term,
    event_time = event_time,
    estimate = as.numeric(values),
    std_error = se,
    critical_value = critical_value,
    ci_low = ci_low,
    ci_high = ci_high,
    has_ci = has_ci,
    stringsAsFactors = FALSE
  )
}

build_stata_model_plot_data <- function(models,
                                        stub_lag,
                                        stub_lead,
                                        trimlag,
                                        trimlead,
                                        shift,
                                        perturb,
                                        model_names,
                                        plot_type,
                                        significance_level,
                                        together,
                                        pretrends_label,
                                        effects_label) {
  if (!is.list(models) || length(models) == 0 || is.data.frame(models)) {
    stop_contract("models must be a non-empty list of Stata-like coefficient specifications.")
  }
  if (length(models) > 8) {
    stop_contract("Combining at most 8 event_plot models is supported.")
  }
  n_models <- length(models)
  model_names <- normalize_model_names(model_names, names(models), n_models)
  stub_lag <- recycle_model_arg(stub_lag, n_models, "stub_lag")
  stub_lead <- recycle_model_arg(stub_lead, n_models, "stub_lead")
  trimlag <- recycle_optional_numeric_arg(trimlag, n_models, "trimlag")
  trimlead <- recycle_optional_numeric_arg(trimlead, n_models, "trimlead")
  shift <- recycle_numeric_arg(shift, n_models, "shift")
  perturb <- if (is.null(perturb)) default_model_perturb(n_models) else recycle_numeric_arg(perturb, n_models, "perturb")
  critical_value <- stats::qnorm(1 - significance_level / 2)

  rows <- vector("list", n_models)
  for (idx in seq_along(models)) {
    model <- normalize_stata_model(models[[idx]], idx)
    lag_stub <- parse_event_stub(stub_lag[[idx]], "stub_lag", idx)
    lead_stub <- parse_event_stub(stub_lead[[idx]], "stub_lead", idx)
    if (identical(stub_lag[[idx]], stub_lead[[idx]])) {
      stop_contract("stub_lag and stub_lead have to be different for model ", idx, ".")
    }
    lag_rows <- rows_for_stub(
      values = model$estimates,
      std_errors = model$std_errors,
      stub = lag_stub,
      source = "stata",
      plot_type = plot_type,
      together = together,
      series = effects_label,
      sign = 1L,
      critical_value = critical_value
    )
    lead_rows <- rows_for_stub(
      values = model$estimates,
      std_errors = model$std_errors,
      stub = lead_stub,
      source = "stata",
      plot_type = plot_type,
      together = together,
      series = if (isTRUE(together)) effects_label else pretrends_label,
      sign = -1L,
      critical_value = critical_value
    )
    model_rows <- rbind(lead_rows, lag_rows)
    if (!is.na(trimlag[[idx]])) {
      model_rows <- model_rows[model_rows$event_time < 0 | model_rows$event_time <= trimlag[[idx]], , drop = FALSE]
    }
    if (!is.na(trimlead[[idx]])) {
      model_rows <- model_rows[model_rows$event_time >= 0 | abs(model_rows$event_time) <= trimlead[[idx]], , drop = FALSE]
    }
    if (nrow(model_rows) == 0) {
      stop_contract("No estimates found for model ", idx, ". Make sure stub_lag and stub_lead are specified correctly.")
    }
    if (anyDuplicated(model_rows$event_time)) {
      stop_contract("Model ", idx, " contains duplicate event times after stub extraction.")
    }
    model_rows$model <- idx
    model_rows$model_label <- model_names[[idx]]
    model_rows$position <- model_rows$event_time + perturb[[idx]] - shift[[idx]]
    model_rows$plot_group <- paste(model_rows$model_label, model_rows$series, sep = "\r")
    rows[[idx]] <- model_rows[order(model_rows$event_time), , drop = FALSE]
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

normalize_stata_model <- function(model, idx) {
  if (!is.list(model) || is.data.frame(model)) {
    stop_contract("models[[", idx, "]] must be a list.")
  }
  estimates <- model$estimates %||% model$coef %||% model$coefficients
  estimates <- normalize_named_numeric(estimates, paste0("models[[", idx, "]]$estimates"))
  std_errors <- model$std_errors %||% model$std_error %||% model$se
  if (is.null(std_errors) && !is.null(model$vcov)) {
    if (!is.matrix(model$vcov) || is.null(rownames(model$vcov)) || is.null(colnames(model$vcov))) {
      stop_contract("models[[", idx, "]]$vcov must be a named covariance matrix.")
    }
    std_errors <- sqrt(diag(model$vcov))
  }
  if (is.null(std_errors) || length(std_errors) == 0) {
    std_errors <- stats::setNames(numeric(), character())
  } else {
    std_errors <- normalize_named_numeric(std_errors, paste0("models[[", idx, "]]$std_errors"))
    missing_terms <- setdiff(names(estimates), names(std_errors))
    extra_terms <- setdiff(names(std_errors), names(estimates))
    if (length(missing_terms) > 0) {
      stop_contract("models[[", idx, "]]$std_errors is missing terms: ", paste(missing_terms, collapse = ", "))
    }
    if (length(extra_terms) > 0) {
      stop_contract("models[[", idx, "]]$std_errors contains unknown terms: ", paste(extra_terms, collapse = ", "))
    }
    std_errors <- std_errors[names(estimates)]
  }
  list(estimates = estimates, std_errors = std_errors)
}

rows_for_stub <- function(values, std_errors, stub, source, plot_type, together, series, sign, critical_value) {
  event_numbers <- extract_stub_numbers(names(values), stub)
  keep <- !is.na(event_numbers)
  if (!any(keep)) {
    return(event_plot_rows(stats::setNames(numeric(), character()), stats::setNames(numeric(), character()), source, plot_type, together, series, "tau", critical_value))
  }
  terms <- names(values)[keep]
  event_time <- sign * event_numbers[keep]
  has_ci <- terms %in% names(std_errors)
  se <- rep(NA_real_, length(terms))
  se[has_ci] <- as.numeric(std_errors[terms[has_ci]])
  estimates <- as.numeric(values[terms])
  ci_low <- rep(NA_real_, length(terms))
  ci_high <- rep(NA_real_, length(terms))
  ci_low[has_ci] <- estimates[has_ci] - critical_value * se[has_ci]
  ci_high[has_ci] <- estimates[has_ci] + critical_value * se[has_ci]
  data.frame(
    source = source,
    plot_type = plot_type,
    together = together,
    series = series,
    term = terms,
    event_time = event_time,
    estimate = estimates,
    std_error = se,
    critical_value = critical_value,
    ci_low = ci_low,
    ci_high = ci_high,
    has_ci = has_ci,
    model = integer(length(terms)),
    model_label = character(length(terms)),
    position = numeric(length(terms)),
    plot_group = character(length(terms)),
    stringsAsFactors = FALSE
  )
}

parse_event_stub <- function(stub, name, idx) {
  if (!is.character(stub) || length(stub) != 1 || is.na(stub) || identical(stub, "")) {
    stop_contract(name, " for model ", idx, " must be a single non-empty string.")
  }
  hash_positions <- gregexpr("#", stub, fixed = TRUE)[[1]]
  if ((length(hash_positions) == 1 && hash_positions[[1]] == -1L) || length(hash_positions) != 1) {
    stop_contract(name, " for model ", idx, " must contain exactly one # placeholder.")
  }
  list(
    prefix = substr(stub, 1, hash_positions[[1]] - 1),
    suffix = substr(stub, hash_positions[[1]] + 1, nchar(stub))
  )
}

extract_stub_numbers <- function(terms, stub) {
  starts <- if (identical(stub$prefix, "")) rep(TRUE, length(terms)) else startsWith(terms, stub$prefix)
  ends <- if (identical(stub$suffix, "")) rep(TRUE, length(terms)) else endsWith(terms, stub$suffix)
  middle_start <- nchar(stub$prefix) + 1
  middle_end <- nchar(terms) - nchar(stub$suffix)
  raw <- ifelse(starts & ends & middle_end >= middle_start, substr(terms, middle_start, middle_end), NA_character_)
  parsed <- suppressWarnings(as.integer(raw))
  bad <- is.na(raw) | is.na(parsed) | raw != as.character(parsed)
  parsed[bad] <- NA_integer_
  parsed
}

normalize_model_names <- function(model_names, list_names, n_models) {
  if (is.null(model_names)) {
    model_names <- list_names
  }
  if (is.null(model_names) || length(model_names) == 0 || any(model_names == "")) {
    model_names <- paste0("Model ", seq_len(n_models))
  }
  model_names <- as.character(model_names)
  if (length(model_names) != n_models || anyNA(model_names) || any(model_names == "")) {
    stop_contract("model_names must contain one non-empty label per model.")
  }
  model_names
}

recycle_model_arg <- function(value, n_models, name) {
  if (length(value) == 1) {
    return(rep(as.character(value), n_models))
  }
  if (length(value) != n_models) {
    stop_contract(name, " must have length 1 or one value per model.")
  }
  as.character(value)
}

recycle_numeric_arg <- function(value, n_models, name) {
  if (!is.numeric(value) || anyNA(value) || any(!is.finite(value))) {
    stop_contract(name, " must contain finite numeric values.")
  }
  if (length(value) == 1) {
    return(rep(as.numeric(value), n_models))
  }
  if (length(value) != n_models) {
    stop_contract(name, " must have length 1 or one value per model.")
  }
  as.numeric(value)
}

recycle_optional_numeric_arg <- function(value, n_models, name) {
  if (is.null(value)) {
    return(rep(NA_real_, n_models))
  }
  recycle_numeric_arg(value, n_models, name)
}

default_model_perturb <- function(n_models) {
  if (n_models == 1) {
    return(0)
  }
  c(0, 0.2 * seq_len(n_models - 1) / n_models)
}

render_event_plot <- function(plot_data,
                              plot_type,
                              pretrends_marker,
                              effects_marker,
                              pretrends_color,
                              effects_color,
                              effects_linestyle,
                              pretrends_linestyle,
                              pretrends_line_width,
                              effects_line_width,
                              pretrends_marker_size,
                              effects_marker_size,
                              pretrends_alpha,
                              effects_alpha,
                              pretrends_rarea_color,
                              effects_rarea_color,
                              pretrends_rarea_alpha,
                              effects_rarea_alpha,
                              title,
                              xlabel,
                              ylabel,
                              show_grid,
                              show_legend,
                              show_zero_line,
                              zero_line_position,
                              zero_line_style,
                              zero_line_color,
                              zero_line_width,
                              zero_line_alpha,
                              show_event_line,
                              event_line_position,
                              event_line_style,
                              event_line_color,
                              event_line_width,
                              event_line_alpha,
                              font_size,
                              legend_loc) {
  series_levels <- unique(plot_data$series)
  colors <- stats::setNames(rep(effects_color, length(series_levels)), series_levels)
  linetypes <- stats::setNames(rep(effects_linestyle, length(series_levels)), series_levels)
  linewidths <- stats::setNames(rep(effects_line_width, length(series_levels)), series_levels)
  markers <- stats::setNames(rep(effects_marker, length(series_levels)), series_levels)
  sizes <- stats::setNames(rep(effects_marker_size, length(series_levels)), series_levels)
  alphas <- stats::setNames(rep(effects_alpha, length(series_levels)), series_levels)
  fills <- stats::setNames(rep(effects_rarea_color, length(series_levels)), series_levels)
  fill_alphas <- stats::setNames(rep(effects_rarea_alpha, length(series_levels)), series_levels)
  pre_series <- unique(as.character(plot_data$series[grepl("^pre", plot_data$term) & !plot_data$together]))
  if (length(pre_series) > 0) {
    colors[pre_series] <- pretrends_color
    linetypes[pre_series] <- pretrends_linestyle
    linewidths[pre_series] <- pretrends_line_width
    markers[pre_series] <- pretrends_marker
    sizes[pre_series] <- pretrends_marker_size
    alphas[pre_series] <- pretrends_alpha
    fills[pre_series] <- pretrends_rarea_color
    fill_alphas[pre_series] <- pretrends_rarea_alpha
  }
  plot_data$series <- factor(plot_data$series, levels = series_levels)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = position, y = estimate, color = series, group = plot_group))
  if (identical(plot_type, "rarea")) {
    ci_data <- plot_data[plot_data$has_ci, , drop = FALSE]
    if (nrow(ci_data) > 0) {
      p <- p + ggplot2::geom_ribbon(
        data = ci_data,
        ggplot2::aes(ymin = ci_low, ymax = ci_high, fill = series),
        alpha = mean(fill_alphas[as.character(ci_data$series)]),
        color = NA,
        inherit.aes = TRUE
      )
    }
  } else {
    ci_data <- plot_data[plot_data$has_ci, , drop = FALSE]
    if (nrow(ci_data) > 0) {
      p <- p + ggplot2::geom_errorbar(
        data = ci_data,
        ggplot2::aes(ymin = ci_low, ymax = ci_high),
        width = 0.08,
        alpha = mean(alphas[as.character(ci_data$series)]),
        inherit.aes = TRUE
      )
    }
  }
  p <- p +
    ggplot2::geom_line(ggplot2::aes(linetype = series, linewidth = series), alpha = mean(alphas)) +
    ggplot2::geom_point(ggplot2::aes(shape = series, size = series), alpha = mean(alphas)) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_linetype_manual(values = linetypes) +
    ggplot2::scale_linewidth_manual(values = linewidths) +
    ggplot2::scale_shape_manual(values = markers) +
    ggplot2::scale_size_manual(values = sizes) +
    ggplot2::scale_x_continuous(breaks = sort(unique(plot_data$position))) +
    ggplot2::labs(title = title, x = xlabel, y = ylabel, color = NULL, fill = NULL, linetype = NULL, shape = NULL, size = NULL) +
    ggplot2::theme_minimal(base_size = font_size)
  if (identical(plot_type, "rarea")) {
    p <- p + ggplot2::scale_fill_manual(values = fills)
  }
  if (isTRUE(show_zero_line)) {
    p <- p + ggplot2::geom_hline(
      yintercept = zero_line_position,
      linetype = zero_line_style,
      color = zero_line_color,
      linewidth = zero_line_width,
      alpha = zero_line_alpha
    )
  }
  if (isTRUE(show_event_line)) {
    p <- p + ggplot2::geom_vline(
      xintercept = event_line_position,
      linetype = event_line_style,
      color = event_line_color,
      linewidth = event_line_width,
      alpha = event_line_alpha
    )
  }
  if (!isTRUE(show_grid)) {
    p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  if (!isTRUE(show_legend)) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else if (!identical(legend_loc, "best")) {
    p <- p + ggplot2::theme(legend.position = legend_loc)
  }
  p
}

normalize_figsize <- function(figsize) {
  if (!is.numeric(figsize) || length(figsize) != 2 || anyNA(figsize) ||
      any(!is.finite(figsize)) || any(figsize <= 0)) {
    stop_contract("figsize must be a positive numeric vector of length 2.")
  }
  as.numeric(figsize)
}

validate_event_plot_save_path <- function(save_path, overwrite) {
  if (!is.character(save_path) || length(save_path) != 1 || is.na(save_path) || identical(save_path, "")) {
    stop_contract("save_path must be a single non-empty string.")
  }
  if (dir.exists(save_path)) {
    stop_contract("save_path must be a file path, not an existing directory.")
  }
  parent <- dirname(save_path)
  if (!dir.exists(parent)) {
    stop_contract("save_path parent directory does not exist: ", parent)
  }
  if (file.exists(save_path) && !isTRUE(overwrite)) {
    stop_contract("save_path already exists and overwrite is FALSE.")
  }
  invisible(save_path)
}

print.didbjs_event_plot <- function(x, ...) {
  if (!is.null(x$plot)) {
    print(x$plot)
  }
  invisible(x)
}

plot.didbjs_event_plot <- function(x, ...) {
  if (is.null(x$plot)) {
    stop_contract("This event plot was created with noplot = TRUE.")
  }
  print(x$plot)
  invisible(x)
}
